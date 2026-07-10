library(tidyverse)
library(here)
library(readxl)
library(logitr)

# ==============================================================================
# Pre-registered heterogeneity analysis (AsPredicted #287153, Q8)
# Separate CL models by gender, income, and environmental attitude.
# Reports instrument attribute coefficients and implicit WTP by subgroup.
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. Load and prepare data (mirrors instrument_pref_analysis.R)
# ------------------------------------------------------------------------------

full_data <- read_excel(
  here("data/Full launch database", "4178_excel_databas.xlsx"), sheet = 2
)

price_map  <- c("0" = 492, "1" = 2460, "2" = 4920, "3" = 7380)
design_raw <- read_excel(
  here("design", "Trial 3 - factorial grouped, svensk.xlsx"), sheet = 1
)
names(design_raw) <- c(
  "cs", "block", "a_source", "a_src_txt", "a_land", "a_land_txt",
  "a_monitor", "a_mon_txt", "a_price", "a_price_txt",
  "b_source", "b_src_txt", "b_land", "b_land_txt",
  "b_monitor", "b_mon_txt", "b_price", "b_price_txt"
)
design <- design_raw |>
  select(cs, block, a_source, a_land, a_monitor, a_price,
         b_source, b_land, b_monitor, b_price) |>
  group_by(block) |> mutate(task = row_number()) |> ungroup() |>
  mutate(a_price_sek = price_map[as.character(a_price)],
         b_price_sek = price_map[as.character(b_price)])

choices_ord1 <- full_data |> filter(ordning == 1) |>
  select(id, block, choice1:choice8) |>
  pivot_longer(choice1:choice8, names_to = "col", values_to = "choice") |>
  mutate(task = as.integer(substr(col, 7, 7)))

choices_ord2 <- full_data |> filter(ordning == 2) |>
  select(id, block, choice12, choice22, choice32, choice42,
         choice52, choice62, choice72, choice82) |>
  pivot_longer(-c(id, block), names_to = "col", values_to = "choice") |>
  mutate(task = as.integer(substr(col, 7, 7)))

resp <- bind_rows(choices_ord1, choices_ord2) |>
  filter(!is.na(choice)) |>
  left_join(design, by = c("block", "task")) |>
  mutate(choice_num = case_when(
    choice == "a" ~ 1L, choice == "b" ~ 2L, TRUE ~ 3L
  ))

# Add individual-level covariates
covars <- full_data |>
  transmute(
    id,
    gender     = q10,
    income     = q8,
    env_import = q11,
    trust_mkt  = q2_2
  )

resp_c <- resp |>
  left_join(covars, by = "id") |>
  filter(!is.na(gender), !is.na(income), !is.na(env_import)) |>
  mutate(
    female       = as.integer(gender == 2),
    income_cat   = case_when(income == 1 ~ "Low", income == 2 ~ "Mid", TRUE ~ "High"),
    env_high     = as.integer(env_import >= median(env_import, na.rm = TRUE)),
    trust_mkt_t  = ntile(trust_mkt, 3),
    trust_mkt_cat = case_when(
      trust_mkt_t == 1 ~ "Low mkt trust",
      trust_mkt_t == 2 ~ "Mid mkt trust",
      TRUE             ~ "High mkt trust"
    ),
    obsID = row_number()
  )

cat("Total choice observations for heterogeneity analysis:", nrow(resp_c), "\n")
cat("Respondents:", n_distinct(resp_c$id), "\n")

# ------------------------------------------------------------------------------
# 2. Build long-format choice data for logitr
#    Three alternatives per observation: A (altID=1), B (altID=2), Neither (altID=3)
# ------------------------------------------------------------------------------

alt_A <- resp_c |>
  transmute(
    obsID, altID = 1L, chosen = as.integer(choice_num == 1),
    asc   = 0L,
    don   = as.integer(a_source == 1),
    cert  = as.integer(a_source == 2),
    off   = as.integer(a_source == 3),
    ind   = as.integer(a_land == 1),
    pland = as.integer(a_land == 2),
    pmon  = as.integer(a_monitor == 1),
    price = a_price_sek / 1000,
    female, income_cat, env_high, trust_mkt_cat
  )

alt_B <- resp_c |>
  transmute(
    obsID, altID = 2L, chosen = as.integer(choice_num == 2),
    asc   = 0L,
    don   = as.integer(b_source == 1),
    cert  = as.integer(b_source == 2),
    off   = as.integer(b_source == 3),
    ind   = as.integer(b_land == 1),
    pland = as.integer(b_land == 2),
    pmon  = as.integer(b_monitor == 1),
    price = b_price_sek / 1000,
    female, income_cat, env_high, trust_mkt_cat
  )

alt_C <- resp_c |>
  transmute(
    obsID, altID = 3L, chosen = as.integer(choice_num == 3),
    asc = 1L,
    don = 0L, cert = 0L, off = 0L,
    ind = 0L, pland = 0L, pmon = 0L,
    price = 0,
    female, income_cat, env_high, trust_mkt_cat
  )

long_df <- bind_rows(alt_A, alt_B, alt_C) |>
  arrange(obsID, altID)

cat("Long format rows:", nrow(long_df), " (should be", nrow(resp_c) * 3, ")\n")

# ------------------------------------------------------------------------------
# 3. Helper: run CL model and extract coefficient + WTP table
# ------------------------------------------------------------------------------

run_cl <- function(data, label) {
  m <- logitr(
    data     = data,
    outcome  = "chosen",
    obsID    = "obsID",
    pars     = c("asc", "don", "cert", "off", "ind", "pland", "pmon", "price"),
    numMultiStarts = 5
  )
  coefs   <- coef(m)
  ses     <- se(m)
  z_vals  <- coefs / ses
  pvals   <- 2 * (1 - pnorm(abs(z_vals)))
  price_c <- coefs["price"]

  stars <- function(p) {
    ifelse(p < .001, "***", ifelse(p < .01, "**",
      ifelse(p < .05, "*", ifelse(p < .1, "†", ""))))
  }

  instruments <- c("don", "cert", "off")
  tab <- tibble(
    Label     = label,
    Attr      = instruments,
    N_obs     = nrow(data) / 3,
    Coef      = round(coefs[instruments], 3),
    SE        = round(ses[instruments], 3),
    Stars     = stars(pvals[instruments]),
    WTP_SEK   = round(coefs[instruments] / (-price_c) * 1000)
  )
  tab
}

# ------------------------------------------------------------------------------
# 4. Run models by subgroup
# ------------------------------------------------------------------------------

cat("\n=== Heterogeneity CL models ===\n")
cat("Note: instrument coefficients relative to tax baseline\n\n")

# Full sample (replication check)
res_full   <- run_cl(long_df, "Full sample")

# Gender subgroups
res_female <- run_cl(long_df |> filter(female == 1), "Female")
res_male   <- run_cl(long_df |> filter(female == 0), "Male")

# Income subgroups
res_inc_lo <- run_cl(long_df |> filter(income_cat == "Low"),  "Income: low")
res_inc_mi <- run_cl(long_df |> filter(income_cat == "Mid"),  "Income: mid")
res_inc_hi <- run_cl(long_df |> filter(income_cat == "High"), "Income: high")

# Environmental attitude subgroups
res_env_lo <- run_cl(long_df |> filter(env_high == 0), "Low env. att.")
res_env_hi <- run_cl(long_df |> filter(env_high == 1), "High env. att.")

# Trust-in-market subgroups (for R8: offsetting by market trust)
res_mkt_lo <- run_cl(long_df |> filter(trust_mkt_cat == "Low mkt trust"),  "Low mkt trust")
res_mkt_mi <- run_cl(long_df |> filter(trust_mkt_cat == "Mid mkt trust"),  "Mid mkt trust")
res_mkt_hi <- run_cl(long_df |> filter(trust_mkt_cat == "High mkt trust"), "High mkt trust")

all_results <- bind_rows(
  res_full,
  res_female, res_male,
  res_inc_lo, res_inc_mi, res_inc_hi,
  res_env_lo, res_env_hi,
  res_mkt_lo, res_mkt_mi, res_mkt_hi
)

# ------------------------------------------------------------------------------
# R8 focus: offsetting and certification by market trust
# ------------------------------------------------------------------------------
cat("\n=== R8: Offsetting and certification by trust-in-market (Q2_2 tercile) ===\n")
bind_rows(res_mkt_lo, res_mkt_mi, res_mkt_hi) |>
  filter(Attr %in% c("off", "cert")) |>
  mutate(Cell = paste0(round(Coef,3), Stars, " (", round(SE,3), ")  WTP=", WTP_SEK)) |>
  select(Label, Attr, N_obs, Cell) |>
  pivot_wider(names_from = Attr, values_from = Cell) |>
  print()

# ------------------------------------------------------------------------------
# 5. Print formatted tables
# ------------------------------------------------------------------------------

cat("=== Instrument coefficients by subgroup (vs. tax baseline) ===\n\n")

format_tab <- all_results |>
  mutate(
    Cell = paste0(round(Coef, 3), Stars, " (", round(SE, 3), ")"),
    WTP_f = paste0(WTP_SEK, Stars)
  ) |>
  select(Label, Attr, Cell, WTP_f, N_obs)

for (attr in c("don", "cert", "off")) {
  attr_lab <- c(don = "Donation", cert = "Certification", off = "Offsetting")[attr]
  cat(paste0("--- ", attr_lab, " ---\n"))
  sub <- format_tab |> filter(Attr == attr) |>
    select(Label, N_obs, Cell, WTP_f)
  print(sub, n = 20)
  cat("\n")
}

# ------------------------------------------------------------------------------
# 6. Summary: wide table suitable for manuscript
# ------------------------------------------------------------------------------

cat("\n=== Wide table: Coef (SE) by subgroup ===\n\n")
wide <- all_results |>
  mutate(Cell = paste0(round(Coef, 3), Stars, " (", round(SE, 3), ")")) |>
  select(Label, Attr, Cell, WTP_SEK) |>
  pivot_wider(names_from = Attr, values_from = c(Cell, WTP_SEK),
              names_glue = "{Attr}_{.value}") |>
  select(Label,
         don_Cell,  don_WTP_SEK,
         cert_Cell, cert_WTP_SEK,
         off_Cell,  off_WTP_SEK)

print(wide, n = 20)
