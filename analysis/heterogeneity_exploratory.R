library(tidyverse)
library(here)
library(readxl)
library(logitr)
library(patchwork)

# ==============================================================================
# EXPLORATORY heterogeneity analysis — NOT pre-registered
# Subgroups: party vote (q16), state trust (q2_1), policy consequentiality (q14)
# Results should be treated as exploratory. See manuscript methods note.
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. Load and prepare data (mirrors heterogeneity_analysis.R)
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

# Individual-level covariates for this script
covars <- full_data |>
  transmute(
    id,
    party          = q16,
    trust_state    = q2_1,
    consequential  = q14
  )

resp_c <- resp |>
  left_join(covars, by = "id") |>
  filter(!is.na(party), !is.na(trust_state), !is.na(consequential)) |>
  mutate(
    # Party blocs
    # Left: S(1), V(4), MP(7) — pro-environment, pro-state
    # Centre-right: M(3), C(5), KD(6), L(8)
    # SD(2): Sweden Democrats — far-right, most skeptical of env regulation
    # No pref / DK: 9, 10
    party_bloc = case_when(
      party %in% c(1, 4, 7) ~ "Left (S/V/MP)",
      party %in% c(3, 5, 6, 8) ~ "Centre-right (M/C/KD/L)",
      party == 2              ~ "SD",
      party %in% c(9, 10)    ~ "No pref / DK"
    ),
    # State trust tercile
    trust_state_t   = ntile(trust_state, 3),
    trust_state_cat = case_when(
      trust_state_t == 1 ~ "Low state trust",
      trust_state_t == 2 ~ "Mid state trust",
      TRUE               ~ "High state trust"
    ),
    # Policy consequentiality: median split
    conseq_med  = median(consequential, na.rm = TRUE),
    conseq_cat  = if_else(consequential >= conseq_med, "High consequentiality", "Low consequentiality"),
    obsID = row_number()
  )

cat("Respondents after joining:", n_distinct(resp_c$id), "\n")
cat("Party bloc distribution:\n")
print(table(resp_c$party_bloc[!duplicated(resp_c$id)]))
cat("\nState trust distribution (respondent-level):\n")
print(table(resp_c$trust_state_cat[!duplicated(resp_c$id)]))
cat("\nConsequentiality distribution (respondent-level):\n")
print(table(resp_c$conseq_cat[!duplicated(resp_c$id)]))

# ------------------------------------------------------------------------------
# 2. Build long-format choice data
# ------------------------------------------------------------------------------

make_long <- function(data) {
  alt_A <- data |> transmute(
    obsID, altID = 1L, chosen = as.integer(choice_num == 1),
    asc = 0L,
    don = as.integer(a_source == 1), cert = as.integer(a_source == 2),
    off = as.integer(a_source == 3),
    ind = as.integer(a_land == 1),   pland = as.integer(a_land == 2),
    pmon  = as.integer(a_monitor == 1),
    price = a_price_sek / 1000,
    party_bloc, trust_state_cat, conseq_cat
  )
  alt_B <- data |> transmute(
    obsID, altID = 2L, chosen = as.integer(choice_num == 2),
    asc = 0L,
    don = as.integer(b_source == 1), cert = as.integer(b_source == 2),
    off = as.integer(b_source == 3),
    ind = as.integer(b_land == 1),   pland = as.integer(b_land == 2),
    pmon  = as.integer(b_monitor == 1),
    price = b_price_sek / 1000,
    party_bloc, trust_state_cat, conseq_cat
  )
  alt_C <- data |> transmute(
    obsID, altID = 3L, chosen = as.integer(choice_num == 3),
    asc = 1L,
    don = 0L, cert = 0L, off = 0L, ind = 0L, pland = 0L, pmon = 0L,
    price = 0,
    party_bloc, trust_state_cat, conseq_cat
  )
  bind_rows(alt_A, alt_B, alt_C) |> arrange(obsID, altID)
}

long_df <- make_long(resp_c)

# ------------------------------------------------------------------------------
# 3. Helper: run CL and extract coefficients
# ------------------------------------------------------------------------------

run_cl <- function(data, label) {
  n_resp <- n_distinct(data$obsID)
  if (n_resp < 50) {
    warning(paste("Only", n_resp, "observations in subgroup:", label))
  }
  m <- logitr(
    data = data, outcome = "chosen", obsID = "obsID",
    pars = c("asc", "don", "cert", "off", "ind", "pland", "pmon", "price"),
    numMultiStarts = 5
  )
  coefs   <- coef(m)
  ses     <- se(m)
  pvals   <- 2 * (1 - pnorm(abs(coefs / ses)))
  stars   <- function(p) ifelse(p < .001, "***", ifelse(p < .01, "**",
                         ifelse(p < .05, "*", ifelse(p < .1, "†", ""))))
  price_c <- coefs["price"]
  instruments <- c("don", "cert", "off")
  tibble(
    Label   = label,
    N_resp  = n_resp / 8,
    Attr    = instruments,
    Coef    = round(coefs[instruments], 3),
    SE      = round(ses[instruments], 3),
    Stars   = stars(pvals[instruments]),
    WTP_SEK = round(coefs[instruments] / (-price_c) * 1000)
  )
}

# ------------------------------------------------------------------------------
# 4. Party bloc subgroups
# ------------------------------------------------------------------------------

cat("\n=== Party vote bloc subgroups (q16, exploratory) ===\n")
res_left  <- run_cl(long_df |> filter(party_bloc == "Left (S/V/MP)"),       "Left (S/V/MP)")
res_cr    <- run_cl(long_df |> filter(party_bloc == "Centre-right (M/C/KD/L)"), "Centre-right")
res_sd    <- run_cl(long_df |> filter(party_bloc == "SD"),                   "SD")
res_nopref <- run_cl(long_df |> filter(party_bloc == "No pref / DK"),        "No pref / DK")

party_results <- bind_rows(res_left, res_cr, res_sd, res_nopref)

cat("\nInstrument coefficients vs. tax baseline:\n")
party_results |>
  mutate(Cell = paste0(Coef, Stars, " (", SE, ")  WTP=", WTP_SEK)) |>
  select(Label, N_resp, Attr, Cell) |>
  pivot_wider(names_from = Attr, values_from = Cell) |>
  print(width = 200)

# ------------------------------------------------------------------------------
# 5. State trust subgroups (q2_1, exploratory)
# ------------------------------------------------------------------------------

cat("\n=== State trust subgroups (q2_1, exploratory) ===\n")
res_trust_lo <- run_cl(long_df |> filter(trust_state_cat == "Low state trust"),  "Low state trust")
res_trust_mi <- run_cl(long_df |> filter(trust_state_cat == "Mid state trust"),  "Mid state trust")
res_trust_hi <- run_cl(long_df |> filter(trust_state_cat == "High state trust"), "High state trust")

trust_results <- bind_rows(res_trust_lo, res_trust_mi, res_trust_hi)

cat("\nInstrument coefficients vs. tax baseline:\n")
trust_results |>
  mutate(Cell = paste0(Coef, Stars, " (", SE, ")  WTP=", WTP_SEK)) |>
  select(Label, N_resp, Attr, Cell) |>
  pivot_wider(names_from = Attr, values_from = Cell) |>
  print(width = 200)

# ------------------------------------------------------------------------------
# 6. Policy consequentiality subgroups (q14, exploratory)
# ------------------------------------------------------------------------------

cat("\n=== Policy consequentiality subgroups (q14, exploratory) ===\n")
res_conseq_lo <- run_cl(long_df |> filter(conseq_cat == "Low consequentiality"),  "Low consequentiality")
res_conseq_hi <- run_cl(long_df |> filter(conseq_cat == "High consequentiality"), "High consequentiality")

conseq_results <- bind_rows(res_conseq_lo, res_conseq_hi)

cat("\nInstrument coefficients vs. tax baseline:\n")
conseq_results |>
  mutate(Cell = paste0(Coef, Stars, " (", SE, ")  WTP=", WTP_SEK)) |>
  select(Label, N_resp, Attr, Cell) |>
  pivot_wider(names_from = Attr, values_from = Cell) |>
  print(width = 200)

# ------------------------------------------------------------------------------
# 7. Summary: all exploratory subgroups combined
# ------------------------------------------------------------------------------

cat("\n=== Combined summary: all exploratory subgroups ===\n")
all_exp <- bind_rows(party_results, trust_results, conseq_results)
all_exp |>
  mutate(Cell = paste0(Coef, Stars, " (", SE, ")")) |>
  select(Label, N_resp, Attr, Cell, WTP_SEK) |>
  pivot_wider(names_from = Attr, values_from = c(Cell, WTP_SEK),
              names_glue = "{Attr}_{.value}") |>
  select(Label, N_resp,
         don_Cell, don_WTP_SEK,
         cert_Cell, cert_WTP_SEK,
         off_Cell, off_WTP_SEK) |>
  print(width = 300)

# ------------------------------------------------------------------------------
# 8. Coefficient plot: all exploratory subgroups
# ------------------------------------------------------------------------------

subgroup_order <- rev(c(
  "Left (S/V/MP)", "Centre-right", "SD", "No pref / DK",
  "High state trust", "Mid state trust", "Low state trust",
  "High consequentiality", "Low consequentiality"
))

plot_df <- all_exp |>
  mutate(
    ci_lo      = Coef - 1.96 * SE,
    ci_hi      = Coef + 1.96 * SE,
    Instrument = recode(Attr,
      don  = "Donation",
      cert = "Certification",
      off  = "Offsetting"
    ),
    Instrument = factor(Instrument, levels = c("Donation", "Certification", "Offsetting")),
    Dimension  = case_when(
      Label %in% c("Left (S/V/MP)", "Centre-right", "SD", "No pref / DK")
        ~ "A.  Party vote (q16)",
      Label %in% c("Low state trust", "Mid state trust", "High state trust")
        ~ "B.  State trust (q2_1)",
      TRUE
        ~ "C.  Policy consequentiality (q14)"
    ),
    Label = factor(Label, levels = subgroup_order)
  )

instr_colors <- c(
  "Donation"      = "#D55E00",
  "Certification" = "#E69F00",
  "Offsetting"    = "#0072B2"
)

p_coef <- ggplot(plot_df,
                 aes(x = Coef, y = Label, color = Instrument,
                     xmin = ci_lo, xmax = ci_hi)) +
  geom_vline(xintercept = 0, linetype = "dashed",
             colour = "grey50", linewidth = 0.4) +
  geom_pointrange(position = position_dodge(width = 0.55),
                  linewidth = 0.5, size = 0.35) +
  scale_color_manual(values = instr_colors) +
  facet_wrap(~Dimension, ncol = 1, scales = "free_y") +
  labs(
    x     = "Coefficient relative to tax baseline (95% CI)",
    y     = NULL,
    color = NULL,
    caption = "Note: exploratory analyses, not pre-registered. Tax is the omitted baseline (coef = 0).\nSubgroups: q16 = party vote intention; q2_1 = trust in state for biodiversity finance;\nq14 = belief Sweden will follow through on biodiversity policy targets."
  ) +
  theme_bw(base_size = 10) +
  theme(
    legend.position   = "bottom",
    legend.key.size   = unit(0.4, "cm"),
    strip.background  = element_blank(),
    strip.text        = element_text(face = "bold", hjust = 0, size = 9),
    panel.grid.minor  = element_blank(),
    plot.caption      = element_text(size = 7, hjust = 0, colour = "grey40"),
    axis.text.y       = element_text(size = 8)
  )

ggsave(
  here("paper", "fig_heterogeneity_exploratory.pdf"),
  p_coef, width = 6.5, height = 7.5
)

cat("\nFigure saved to paper/fig_heterogeneity_exploratory.pdf\n")

# Save results for manuscript inline plotting
saveRDS(all_exp, here("analysis", "heterogeneity_exploratory.rds"))
cat("Results saved to analysis/heterogeneity_exploratory.rds\n")
