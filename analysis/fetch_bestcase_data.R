# ==============================================================================
# Fetch and cache the country-level data underlying Figure 1.
#
# Two series, both from primary sources:
#   (1) Interpersonal trust — Integrated Values Surveys (EVS-WVS), Wave 7.
#   (2) Public expenditure on biodiversity and landscape protection — Eurostat.
#
# Run this once to (re)build analysis/bestcase_data.csv. bestcase_map.R reads
# the cached CSV so the figure can be rebuilt offline and so the exact values
# behind the published figure stay auditable if the upstream sources revise.
#
# Provenance note: an earlier version of bestcase_map.R carried hand-entered
# approximations for these series. They were materially wrong (see
# bestcase_data_verification.md) and are superseded by this script.
# ==============================================================================

library(tidyverse)
library(jsonlite)
library(here)

# ------------------------------------------------------------------------------
# 1. Interpersonal trust
#
# Question (WVS A165 / EVS equivalent): "Generally speaking, would you say that
# most people can be trusted or that you need to be very careful in dealing with
# people?" Value = % answering "most people can be trusted".
#
# Source: Integrated Values Surveys (2024), IVS v4 — the joint EVS-WVS file —
# with processing by Our World in Data. Wave 7 fieldwork ran 2017-2022; OWID
# stamps every Wave 7 row with year 2022.
#
# The Nordic countries appear in the *integrated* EVS-WVS file, not in the
# standalone WVS Wave 7 release. Belgium and South Africa have no Wave 7 survey
# at all and are therefore absent by design, not by omission.
# ------------------------------------------------------------------------------

trust_url <- "https://ourworldindata.org/grapher/self-reported-trust-attitudes.csv"

trust_raw <- read_csv(trust_url, show_col_types = FALSE)

trust <- trust_raw |>
  filter(Year >= 2017) |>                      # Wave 7 window
  group_by(Code) |>
  slice_max(Year, n = 1, with_ties = FALSE) |>
  ungroup() |>
  transmute(iso3 = Code, country = Entity, trust_interp = `Trust in others`) |>
  filter(!is.na(iso3), iso3 != "")

message("Interpersonal trust: ", nrow(trust), " countries with Wave 7 data")

# ------------------------------------------------------------------------------
# 2. Public expenditure on biodiversity and landscape protection
#
# Eurostat gov_10a_exp, COFOG group GF0504 ("Protection of biodiversity and
# landscape"), general government (S13), total expenditure (TE).
#
# We use GF0504 rather than the whole environmental-protection division (GF05)
# because GF05 is dominated by waste and waste-water management, which are not
# biodiversity financing in the sense this paper studies.
#
# Eurostat publishes no per-capita unit for this series, so we divide million
# euro by population (tps00001) ourselves. The published percentage-of-GDP unit
# is rounded to 0.1pp, which is too coarse to separate the low spenders, hence
# working from absolute euro.
# ------------------------------------------------------------------------------

REF_YEAR <- 2023      # latest year with near-complete country coverage
EUR_USD  <- 1.082     # 2023 average ECB reference rate; paper reports USD

eurostat_json <- function(dataset, ...) {
  q <- paste0(names(list(...)), "=", unlist(list(...)), collapse = "&")
  url <- paste0(
    "https://ec.europa.eu/eurostat/api/dissemination/statistics/1.0/data/",
    dataset, "?format=JSON&lang=EN&", q
  )
  fromJSON(url)
}

# Helper: turn a one-dimension-varying JSON-stat response into iso2 -> value
es_values <- function(js) {
  idx <- unlist(js$dimension$geo$category$index)
  tibble(
    pos  = as.integer(names(js$value)),
    value = unlist(js$value)
  ) |>
    mutate(iso2 = names(idx)[match(pos, idx)]) |>
    select(iso2, value)
}

spend_raw <- eurostat_json(
  "gov_10a_exp",
  cofog99 = "GF0504", na_item = "TE", sector = "S13",
  unit = "MIO_EUR", time = REF_YEAR
) |> es_values() |> rename(spend_meur = value)

pop_raw <- eurostat_json("tps00001", time = REF_YEAR) |>
  es_values() |> rename(pop = value)

# Eurostat uses 2-letter codes (and EL/UK rather than GR/GB); map to ISO3 so the
# two series can be joined and matched to map geometry.
iso2_to_iso3 <- c(
  AT="AUT", BE="BEL", BG="BGR", CH="CHE", CY="CYP", CZ="CZE", DE="DEU",
  DK="DNK", EE="EST", EL="GRC", ES="ESP", FI="FIN", FR="FRA", HR="HRV",
  HU="HUN", IE="IRL", IS="ISL", IT="ITA", LT="LTU", LU="LUX", LV="LVA",
  MT="MLT", NL="NLD", NO="NOR", PL="POL", PT="PRT", RO="ROU", SE="SWE",
  SI="SVN", SK="SVK", TR="TUR", RS="SRB", UK="GBR"
)

spending <- spend_raw |>
  inner_join(pop_raw, by = "iso2") |>
  filter(!grepl("^(EU|EA)", iso2)) |>          # drop aggregates
  mutate(
    iso3          = iso2_to_iso3[iso2],
    spend_pc_eur  = spend_meur * 1e6 / pop,
    spend_pc_usd  = spend_pc_eur * EUR_USD
  ) |>
  filter(!is.na(iso3)) |>
  select(iso3, spend_pc_eur, spend_pc_usd)

message("Biodiversity spending: ", nrow(spending), " countries (Eurostat, ",
        REF_YEAR, ")")

# ------------------------------------------------------------------------------
# 3. Merge and cache
# ------------------------------------------------------------------------------

bestcase <- trust |>
  full_join(spending, by = "iso3") |>
  arrange(desc(trust_interp))

write_csv(bestcase, here("analysis", "bestcase_data.csv"))

n_both <- sum(!is.na(bestcase$trust_interp) & !is.na(bestcase$spend_pc_usd))
message("Wrote analysis/bestcase_data.csv — ", nrow(bestcase), " rows, ",
        n_both, " with both series")

# Quick sanity report on the case country
swe <- bestcase |> filter(iso3 == "SWE")
message(
  "Sweden: trust = ", round(swe$trust_interp, 1), "% (rank ",
  sum(bestcase$trust_interp > swe$trust_interp, na.rm = TRUE) + 1, " of ",
  sum(!is.na(bestcase$trust_interp)), "); biodiversity spending = USD ",
  round(swe$spend_pc_usd), "/capita (rank ",
  sum(bestcase$spend_pc_usd > swe$spend_pc_usd, na.rm = TRUE) + 1, " of ",
  sum(!is.na(bestcase$spend_pc_usd)), ")"
)
