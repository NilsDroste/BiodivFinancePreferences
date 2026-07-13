library(tidyverse)
library(patchwork)
library(here)

full_data <- readxl::read_excel(here("data/Full launch database", "4178_excel_databas.xlsx"), sheet = 2)

# ==============================================================================
# Donation experiment analysis
# Dictator game: respondents allocate up to 1000 SEK lottery prize to
# Naturskyddsföreningen (Swedish Society for Nature Conservation)
# ==============================================================================

don <- full_data |>
  transmute(
    id,
    donation     = yourdonation,
    share        = yourdonation / 1000,
    gave_zero    = yourdonation == 0,
    gave_all     = yourdonation == 1000,
    # consequentiality
    eff_donation = q13_1,   # perceived effectiveness of donations (1-5)
    eff_cert     = q13_2,
    eff_tax      = q13_3,
    eff_offset   = q13_4,
    expand_don   = q15_1,   # likelihood donations will expand (1-5)
    expand_cert  = q15_2,
    expand_tax   = q13_3,
    expand_off   = q15_4,
    policy_cq    = q14,     # "Sweden will follow through on biodiversity policies" (1-5)
    # trust in handlers
    trust_state  = q2_1,
    trust_market = q2_2,
    trust_ngo    = q2_3,
    # demographics
    env_import   = q11,     # environmental importance (1-5)
    bio_knowl    = q12,     # biodiversity knowledge (1-5)
    income       = q8,      # household income (1-4 categories)
    gender       = q10,
    party        = q16
  )

# --- 1. Distribution ---
p_hist <- ggplot(don, aes(x = donation)) +
  geom_histogram(binwidth = 50, fill = "steelblue", colour = "white") +
  geom_vline(xintercept = mean(don$donation), linetype = "dashed", colour = "red") +
  annotate("text", x = mean(don$donation) + 30, y = Inf, label = paste0("mean = ", round(mean(don$donation))),
           vjust = 2, hjust = 0, colour = "red", size = 3.5) +
  scale_x_continuous(breaks = seq(0, 1000, 200)) +
  labs(title = "Distribution of donation amounts",
       subtitle = paste0("n = ", nrow(don), "  |  ",
                         round(mean(don$gave_zero) * 100, 1), "% give nothing  |  ",
                         round(mean(don$gave_all)  * 100, 1), "% give everything"),
       x = "Donation (SEK)", y = "Count") +
  theme_minimal()

p_share <- ggplot(don, aes(x = share)) +
  stat_ecdf(colour = "steelblue", linewidth = 1) +
  scale_x_continuous(labels = scales::percent, breaks = seq(0, 1, .25)) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "CDF of donation share (out of 1000 SEK)",
       x = "Share donated", y = "Cumulative %") +
  theme_minimal()

p_hist + p_share

# --- 2. Consequentiality beliefs ---

# Perceived effectiveness of each instrument
eff_long <- don |>
  select(starts_with("eff_")) |>
  pivot_longer(everything(), names_to = "instrument", values_to = "score") |>
  mutate(instrument = recode(instrument,
    eff_donation = "Donation", eff_cert = "Certification",
    eff_tax = "Tax", eff_offset = "Offsetting"))

p_eff <- ggplot(eff_long, aes(x = instrument, y = score, fill = instrument)) +
  geom_violin(alpha = .6, draw_quantiles = c(.25, .5, .75)) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Q13: Perceived effectiveness by instrument",
       subtitle = "1 = Not effective, 5 = Very effective",
       x = NULL, y = "Score") +
  theme_minimal() + theme(legend.position = "none")

# Expansion likelihood
exp_long <- don |>
  select(expand_don, expand_cert, expand_tax, expand_off) |>
  pivot_longer(everything(), names_to = "instrument", values_to = "score") |>
  mutate(instrument = recode(instrument,
    expand_don = "Donation", expand_cert = "Certification",
    expand_tax = "Tax", expand_off = "Offsetting"))

p_expand <- ggplot(exp_long, aes(x = instrument, y = score, fill = instrument)) +
  geom_violin(alpha = .6, draw_quantiles = c(.25, .5, .75)) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Q15: Expansion likelihood by instrument",
       subtitle = "1 = Not likely, 5 = Very likely",
       x = NULL, y = "Score") +
  theme_minimal() + theme(legend.position = "none")

p_eff + p_expand

# --- 3. Donation amount vs. consequentiality / trust ---

make_jitter_box <- function(df, xvar, yvar = "donation", xlab, title) {
  ggplot(df, aes(x = factor(.data[[xvar]]), y = .data[[yvar]])) +
    geom_jitter(width = .2, alpha = .15, size = .8, colour = "steelblue") +
    geom_boxplot(alpha = .5, outlier.shape = NA) +
    labs(title = title, x = xlab, y = "Donation (SEK)") +
    theme_minimal()
}

p_eff_don   <- make_jitter_box(don, "eff_donation",  xlab = "Effectiveness score", title = "Donation vs. perceived effectiveness of donations")
p_exp_don   <- make_jitter_box(don, "expand_don",    xlab = "Expansion likelihood", title = "Donation vs. expansion likelihood of donations")
p_polcq_don <- make_jitter_box(don, "policy_cq",     xlab = "Policy follow-through", title = "Donation vs. policy consequentiality")
p_ngo_don   <- make_jitter_box(don, "trust_ngo",     xlab = "Trust in NGOs", title = "Donation vs. trust in NGOs")

(p_eff_don + p_exp_don) / (p_polcq_don + p_ngo_don)

# --- 4. Demographic breakdown ---

p_income <- don |>
  filter(!is.na(income)) |>
  mutate(income_lab = recode(as.character(income),
    "1" = "0–20k", "2" = "20–100k", "3" = "100–300k", "4" = "300–500k")) |>
  ggplot(aes(x = income_lab, y = donation)) +
  geom_jitter(width = .2, alpha = .15, size = .8, colour = "steelblue") +
  geom_boxplot(alpha = .5, outlier.shape = NA) +
  labs(title = "Donation by income", x = "Monthly household income (SEK)", y = "Donation (SEK)") +
  theme_minimal()

p_gender <- don |>
  filter(gender %in% 1:2) |>
  mutate(gender_lab = recode(as.character(gender), "1" = "Male", "2" = "Female")) |>
  ggplot(aes(x = gender_lab, y = donation)) +
  geom_jitter(width = .2, alpha = .15, size = .8, colour = "steelblue") +
  geom_boxplot(alpha = .5, outlier.shape = NA) +
  labs(title = "Donation by gender", x = NULL, y = "Donation (SEK)") +
  theme_minimal()

p_env <- make_jitter_box(don, "env_import", xlab = "Environmental importance (1–5)", title = "Donation vs. env. importance")
p_know <- make_jitter_box(don, "bio_knowl", xlab = "Biodiversity knowledge (1–5)", title = "Donation vs. biodiversity knowledge")

(p_income + p_gender) / (p_env + p_know)

# --- 5. OLS regression (robust SEs via lm + sandwich) ---
# install.packages("sandwich"); install.packages("lmtest") if needed

if (!requireNamespace("sandwich", quietly = TRUE)) install.packages("sandwich")
if (!requireNamespace("lmtest",   quietly = TRUE)) install.packages("lmtest")

library(sandwich)
library(lmtest)

reg_data <- don |>
  filter(!is.na(income), !is.na(gender)) |>
  mutate(
    female       = as.integer(gender == 2),
    income_mid   = as.integer(income == 2),
    income_high  = as.integer(income >= 3)
  )

m1 <- lm(donation ~ eff_donation + expand_don + policy_cq, data = reg_data)
m2 <- lm(donation ~ eff_donation + expand_don + policy_cq +
            trust_state + trust_market + trust_ngo, data = reg_data)
m3 <- lm(donation ~ eff_donation + expand_don + policy_cq +
            trust_state + trust_market + trust_ngo +
            env_import + bio_knowl + female + income_mid + income_high,
         data = reg_data)

cat("\n=== OLS regressions (donation ~ consequentiality + trust + demographics) ===\n")
cat("\n--- Model 1: consequentiality only ---\n")
print(coeftest(m1, vcov = vcovHC(m1, type = "HC3")))
cat("\n--- Model 2: + trust in handlers ---\n")
print(coeftest(m2, vcov = vcovHC(m2, type = "HC3")))
cat("\n--- Model 3: full ---\n")
print(coeftest(m3, vcov = vcovHC(m3, type = "HC3")))

cat("\n=== R² ===\n")
cat("Model 1:", round(summary(m1)$r.squared, 4), "\n")
cat("Model 2:", round(summary(m2)$r.squared, 4), "\n")
cat("Model 3:", round(summary(m3)$r.squared, 4), "\n")

# --- 6. Summary table ---
cat("\n=== Donation summary ===\n")
don |> summarise(
  n          = n(),
  mean       = round(mean(donation), 1),
  median     = median(donation),
  sd         = round(sd(donation), 1),
  pct_zero   = round(mean(gave_zero) * 100, 1),
  pct_half   = round(mean(donation == 500) * 100, 1),
  pct_all    = round(mean(gave_all) * 100, 1)
) |> print()

# ==============================================================================
# Session info (for reproducibility)
# ==============================================================================
cat("\n=== Session information ===\n")
print(sessionInfo())
