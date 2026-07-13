library(tidyverse)
library(here)
library(readxl)
library(sandwich)
library(lmtest)
library(patchwork)

# ==============================================================================
# Exploratory analysis (not preregistered):
# Does actual donation behaviour explain stated preference for voluntary vs.
# mandatory biodiversity financing instruments in the DCE?
#
# Following Yohei Mitani's framework: the key prediction is that respondents
# with high environmental attitudes but low actual donation prefer voluntary
# instruments in the DCE — because voluntary schemes impose no personal cost.
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. Load data
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

cat("Total choice observations:", nrow(resp), "\n")
cat("Respondents:", n_distinct(resp$id), "\n")

# ------------------------------------------------------------------------------
# 2. Per-respondent voluntary preference score
#
# Instrument coding:  0 = tax (mandatory), 1 = donation (voluntary),
#                     2 = certification (voluntary), 3 = offsetting (mandatory)
# voluntary_pref = share of mixed-instrument tasks (one V, one M option) where
# the voluntary option was chosen. Opt-outs excluded from denominator.
# ------------------------------------------------------------------------------

voluntary_set <- c(1L, 2L)
mandatory_set <- c(0L, 3L)

resp_scored <- resp |>
  mutate(
    a_vol = a_source %in% voluntary_set,
    b_vol = b_source %in% voluntary_set,
    a_man = a_source %in% mandatory_set,
    b_man = b_source %in% mandatory_set,
    mixed_task = (a_vol & b_man) | (a_man & b_vol),
    chose_voluntary = case_when(
      !mixed_task         ~ NA_real_,
      choice == "3"       ~ NA_real_,   # opt-out coded as "3" in raw data, not "neither"
      (a_vol & choice == "a") | (b_vol & choice == "b") ~ 1,
      TRUE ~ 0
    )
  )

resp_level <- resp_scored |>
  filter(mixed_task, !is.na(chose_voluntary)) |>
  group_by(id) |>
  summarise(
    voluntary_pref = mean(chose_voluntary),
    n_mixed        = n(),
    .groups        = "drop"
  )

opt_out <- resp |>
  group_by(id) |>
  summarise(opt_out_rate = mean(choice == "neither"), .groups = "drop")

resp_level <- resp_level |>
  left_join(opt_out, by = "id")

cat("\nRespondents with ≥1 mixed task:", nrow(resp_level), "\n")
cat("Mean mixed tasks per respondent:", round(mean(resp_level$n_mixed), 1), "\n")
cat("Mean voluntary_pref:", round(mean(resp_level$voluntary_pref), 3), "\n")
cat("SD voluntary_pref:  ", round(sd(resp_level$voluntary_pref), 3), "\n")

cat("\n=== n_mixed distribution ===\n")
print(table(resp_level$n_mixed))
cat("% with < 3 mixed tasks:", round(mean(resp_level$n_mixed < 3) * 100, 1), "\n")
cat("% with < 3 mixed tasks (n):", sum(resp_level$n_mixed < 3), "\n")

# ------------------------------------------------------------------------------
# 3. Merge with covariates
# ------------------------------------------------------------------------------

covars <- full_data |>
  transmute(
    id,
    donation    = yourdonation,
    share_don   = yourdonation / 1000,
    gave        = as.integer(yourdonation > 0),
    env_import  = q11,
    bio_knowl   = q12,
    trust_ngo   = q2_3,
    trust_state = q2_1,
    trust_mkt   = q2_2,
    policy_cq   = q14,
    eff_don     = q13_1,
    income      = q8,
    gender      = q10,
    party       = q16
  )

anal_data <- resp_level |>
  left_join(covars, by = "id") |>
  filter(!is.na(donation), !is.na(env_import),
         !is.na(income), !is.na(gender)) |>
  mutate(
    donation_z  = scale(donation)[, 1],
    env_z       = scale(env_import)[, 1],
    female      = as.integer(gender == 2),
    income_mid  = as.integer(income == 2),
    income_high = as.integer(income >= 3),
    env_high    = as.integer(env_import >= median(env_import, na.rm = TRUE)),
    gave_binary = gave
  )

cat("\nAnalysis sample n =", nrow(anal_data), "\n")

# Sensitivity sample: exclude respondents with < 3 mixed tasks
# (VP score is near-binary with 1-2 tasks and carries high measurement noise)
anal_data_sens <- anal_data |> filter(n_mixed >= 3)
cat("Sensitivity sample (n_mixed >= 3) n =", nrow(anal_data_sens), "\n")
cat("Dropped:", nrow(anal_data) - nrow(anal_data_sens), "respondents\n")

# ------------------------------------------------------------------------------
# 4. Descriptive 2×2 table (Yohei's framework)
# ------------------------------------------------------------------------------

cat("\n=== 2×2: Mean voluntary_pref by env_import × donation ===\n")
cell_stats <- anal_data |>
  group_by(env_high, gave_binary) |>
  summarise(
    mean_vp = round(mean(voluntary_pref), 3),
    sd_vp   = round(sd(voluntary_pref), 3),
    n       = n(),
    .groups = "drop"
  ) |>
  mutate(
    env_lab  = ifelse(env_high,    "High env att", "Low env att"),
    gave_lab = ifelse(gave_binary, "Donated",      "Did not donate")
  )
print(cell_stats[, c("env_lab", "gave_lab", "mean_vp", "sd_vp", "n")])

# ------------------------------------------------------------------------------
# 5. OLS regressions
# ------------------------------------------------------------------------------

m1 <- lm(voluntary_pref ~ donation_z + env_z,
         data = anal_data)
m2 <- lm(voluntary_pref ~ donation_z * env_z,
         data = anal_data)
m3 <- lm(voluntary_pref ~ donation_z * env_z +
            trust_ngo + trust_state + policy_cq +
            bio_knowl + female + income_mid + income_high,
         data = anal_data)

# Robustness: replace donation_z with binary gave
m4 <- lm(voluntary_pref ~ gave_binary * env_z +
            trust_ngo + trust_state + policy_cq +
            bio_knowl + female + income_mid + income_high,
         data = anal_data)

cat("\n=== OLS: voluntary_pref ~ donation × env_import (HC3 robust SEs) ===\n")
cat("\n--- M1: main effects ---\n")
print(coeftest(m1, vcov = vcovHC(m1, "HC3")))
cat("\n--- M2: + interaction ---\n")
print(coeftest(m2, vcov = vcovHC(m2, "HC3")))
cat("\n--- M3: full covariates ---\n")
print(coeftest(m3, vcov = vcovHC(m3, "HC3")))
cat("\n--- M4: binary gave (robustness) ---\n")
print(coeftest(m4, vcov = vcovHC(m4, "HC3")))

cat("\n=== R² ===\n")
for (nm in c("M1","M2","M3","M4")) {
  m <- get(tolower(nm))
  cat(nm, ": R²=", round(summary(m)$r.squared, 4),
      "  adj.R²=", round(summary(m)$adj.r.squared, 4), "\n")
}

# ------------------------------------------------------------------------------
# 6. Formatted regression table (following _table.R style)
# ------------------------------------------------------------------------------

stars <- function(p) {
  ifelse(p < .001, "***", ifelse(p < .01, "**",
    ifelse(p < .05, "*", ifelse(p < .1, ".", ""))))
}

model_row <- function(m, var) {
  raw <- coeftest(m, vcov = vcovHC(m, "HC3"))
  ct  <- as.data.frame(unclass(raw))
  names(ct) <- c("est", "se", "t", "p")
  if (!var %in% rownames(ct)) return("")
  r <- ct[var, ]
  paste0(round(r$est, 3), stars(r$p), " (", round(r$se, 3), ")")
}

vars_tab <- c("(Intercept)", "donation_z", "env_z", "donation_z:env_z",
              "gave_binary", "env_z:gave_binary",
              "trust_ngo", "trust_state", "policy_cq",
              "bio_knowl", "female", "income_mid", "income_high")
labs_tab <- c("Intercept", "Donation (z-score)", "Env. importance (z-score)",
              "Donation × Env. importance",
              "Donated (binary)", "Donated × Env. importance",
              "Trust: NGOs (Q2_3)", "Trust: state (Q2_1)",
              "Policy consequentiality (Q14)",
              "Biodiversity knowledge (Q12)", "Female",
              "Income: mid", "Income: high")

models <- list(M1 = m1, M2 = m2, M3 = m3, M4 = m4)
w  <- 26; lw <- 34
sep <- paste(rep("-", lw + 4 * (w + 2)), collapse = "")

cat("\n=== Regression table: DV = voluntary_pref ===\n")
cat(sprintf("%-*s  %*s  %*s  %*s  %*s\n", lw, "", w,"M1",w,"M2",w,"M3",w,"M4"))
cat(sep, "\n")
for (i in seq_along(vars_tab)) {
  cells <- sapply(models, function(m) model_row(m, vars_tab[i]))
  cat(sprintf("%-*s  %*s  %*s  %*s  %*s\n",
              lw, labs_tab[i], w, cells[1], w, cells[2], w, cells[3], w, cells[4]))
}
cat(sep, "\n")
for (stat in c("r2","adj","n")) {
  vals <- sapply(models, function(m) {
    s <- summary(m)
    switch(stat,
      r2  = round(s$r.squared, 4),
      adj = round(s$adj.r.squared, 4),
      n   = nobs(m))
  })
  cat(sprintf("%-*s  %*s  %*s  %*s  %*s\n",
              lw, c(r2="R²", adj="Adj. R²", n="N")[stat],
              w, vals[1], w, vals[2], w, vals[3], w, vals[4]))
}
cat("\nNote: HC3 robust SEs in parentheses. . p<.1  * p<.05  ** p<.01  *** p<.001\n")
cat("M4 uses binary donated indicator instead of continuous donation (robustness).\n")

# ------------------------------------------------------------------------------
# 5b. Sensitivity check: rerun OLS on n_mixed >= 3 subsample
# ------------------------------------------------------------------------------

cat("\n=== OLS sensitivity (n_mixed >= 3): voluntary_pref ~ donation × env_import ===\n")
m3_sens <- lm(voluntary_pref ~ donation_z * env_z +
                trust_ngo + trust_state + policy_cq +
                bio_knowl + female + income_mid + income_high,
              data = anal_data_sens)
m4_sens <- lm(voluntary_pref ~ gave_binary * env_z +
                trust_ngo + trust_state + policy_cq +
                bio_knowl + female + income_mid + income_high,
              data = anal_data_sens)

cat("\n--- M3 (n_mixed >= 3) ---\n")
print(coeftest(m3_sens, vcov = vcovHC(m3_sens, "HC3")))
cat("\n--- M4 (n_mixed >= 3) ---\n")
print(coeftest(m4_sens, vcov = vcovHC(m4_sens, "HC3")))

# ------------------------------------------------------------------------------
# 5c. Fractional logit (Papke-Wooldridge) — primary specification per R2
#
# glm with quasibinomial(link="logit") implements the Papke-Wooldridge (1996)
# fractional logit estimator (logit link, not probit). HC3 robust SEs account
# for quasi-likelihood (not classical MLE) standard errors.
# ------------------------------------------------------------------------------

cat("\n=== Fractional logit (Papke-Wooldridge): primary specification ===\n")

fp1 <- glm(voluntary_pref ~ donation_z + env_z,
           data = anal_data, family = quasibinomial(link = "logit"))
fp2 <- glm(voluntary_pref ~ donation_z * env_z,
           data = anal_data, family = quasibinomial(link = "logit"))
fp3 <- glm(voluntary_pref ~ donation_z * env_z +
             trust_ngo + trust_state + policy_cq +
             bio_knowl + female + income_mid + income_high,
           data = anal_data, family = quasibinomial(link = "logit"))
fp4 <- glm(voluntary_pref ~ gave_binary * env_z +
             trust_ngo + trust_state + policy_cq +
             bio_knowl + female + income_mid + income_high,
           data = anal_data, family = quasibinomial(link = "logit"))

cat("\n--- FP3 (full model) ---\n")
print(coeftest(fp3, vcov = vcovHC(fp3, "HC3")))

# Average partial effects (APE) for FP3
# APE_k = coef_k * mean_i[ mu_i * (1 - mu_i) ]  where mu_i = fitted(fp3)
fp3_ape_scale <- mean(fitted(fp3) * (1 - fitted(fp3)))
fp3_coefs     <- coeftest(fp3, vcov = vcovHC(fp3, "HC3"))
fp3_ape       <- fp3_coefs[, "Estimate"] * fp3_ape_scale

cat("\nFP3 Average Partial Effects (multiply coefs by", round(fp3_ape_scale, 4), "):\n")
ape_df <- data.frame(
  Coef = fp3_coefs[, "Estimate"],
  APE  = fp3_ape,
  SE_APE = fp3_coefs[, "Std. Error"] * fp3_ape_scale
)
print(round(ape_df, 4))

# Deviance-based R² for quasi-likelihood models: 1 - deviance/null.deviance
dev_r2 <- function(m) 1 - m$deviance / m$null.deviance
cat("\nFractional logit deviance R²:\n")
for (nm in c("fp1","fp2","fp3","fp4")) {
  m <- get(nm)
  cat(nm, ": dev-R²=", round(dev_r2(m), 4), "  N=", nobs(m), "\n")
}

cat("\n--- FP4 (binary donated, robustness) ---\n")
print(coeftest(fp4, vcov = vcovHC(fp4, "HC3")))

# Fractional probit sensitivity (n_mixed >= 3)
fp3_sens <- glm(voluntary_pref ~ donation_z * env_z +
                  trust_ngo + trust_state + policy_cq +
                  bio_knowl + female + income_mid + income_high,
                data = anal_data_sens, family = quasibinomial(link = "logit"))
cat("\n--- FP3 sensitivity (n_mixed >= 3) ---\n")
print(coeftest(fp3_sens, vcov = vcovHC(fp3_sens, "HC3")))

# ------------------------------------------------------------------------------
# 7. Visualisations
# ------------------------------------------------------------------------------

# 7a. Distribution of voluntary_pref
p_dist <- ggplot(anal_data, aes(x = voluntary_pref)) +
  geom_histogram(binwidth = 0.1, fill = "#0072B2", colour = "white") +
  geom_vline(xintercept = mean(anal_data$voluntary_pref),
             linetype = "dashed", colour = "#D55E00") +
  annotate("text",
           x = mean(anal_data$voluntary_pref) + 0.03, y = Inf,
           label = paste0("mean = ", round(mean(anal_data$voluntary_pref), 2)),
           vjust = 2, hjust = 0, colour = "#D55E00", size = 3.5) +
  scale_x_continuous(labels = scales::percent, breaks = seq(0, 1, 0.2)) +
  labs(title = "Distribution of voluntary preference score",
       subtitle = "Share of mixed-instrument tasks where voluntary option chosen",
       x = "Voluntary preference", y = "Count") +
  theme_minimal()

# 7b. 2×2 cell means with 95% CIs
p_cell <- anal_data |>
  mutate(
    cell = paste0(
      ifelse(env_high, "High env. att.", "Low env. att."), "\n",
      ifelse(gave_binary, "Donated", "Did not donate")
    ),
    env_f = factor(env_high, labels = c("Low", "High"))
  ) |>
  group_by(cell, env_f, gave_binary) |>
  summarise(
    mn  = mean(voluntary_pref),
    se  = sd(voluntary_pref) / sqrt(n()),
    n   = n(),
    .groups = "drop"
  ) |>
  mutate(cell = fct_reorder(cell, mn)) |>
  ggplot(aes(x = cell, y = mn, fill = env_f)) +
  geom_col(alpha = 0.85) +
  geom_errorbar(aes(ymin = mn - 1.96*se, ymax = mn + 1.96*se), width = 0.25) +
  geom_text(aes(label = paste0("n=", n)), vjust = -0.6, size = 3) +
  scale_fill_manual(values = c("Low" = "#D55E00", "High" = "#0072B2"),
                    name = "Env. att.") +
  scale_y_continuous(labels = scales::percent, expand = expansion(mult = c(0, .15))) +
  labs(title = "Voluntary preference by environmental attitude × donation",
       subtitle = "Strategic hypothesis: High env att + Did not donate → elevated voluntary pref",
       x = NULL, y = "Mean voluntary preference (±95% CI)") +
  theme_minimal()

# 7c. Donation vs voluntary_pref by env. attitude tercile
p_interact <- anal_data |>
  mutate(
    env_t   = ntile(env_import, 3),
    env_lab = factor(env_t, labels = c("Low env. att.", "Medium", "High env. att."))
  ) |>
  ggplot(aes(x = donation, y = voluntary_pref, colour = env_lab)) +
  geom_jitter(alpha = 0.2, size = 0.8, width = 30, height = 0.02) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1) +
  scale_colour_manual(
    values = c("Low env. att." = "#D55E00", "Medium" = "#E69F00",
               "High env. att." = "#0072B2"),
    name = NULL) +
  scale_x_continuous(labels = scales::label_comma()) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Voluntary preference vs. donation by environmental attitude",
       subtitle = "Convergence of lines = no interaction; divergence = strategic behaviour",
       x = "Dictator game donation (SEK)", y = "Voluntary preference") +
  theme_minimal()

# 7d. Scatter: voluntary_pref vs env_import, by donated
p_env <- ggplot(anal_data,
                aes(x = env_import, y = voluntary_pref,
                    colour = factor(gave_binary),
                    fill   = factor(gave_binary))) +
  geom_jitter(alpha = 0.2, size = 0.8, width = 0.1, height = 0.02) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1) +
  scale_colour_manual(values = c("0" = "#D55E00", "1" = "#0072B2"),
                      labels = c("Did not donate", "Donated"), name = NULL) +
  scale_fill_manual(values  = c("0" = "#D55E00", "1" = "#0072B2"),
                    labels  = c("Did not donate", "Donated"), name = NULL) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Voluntary preference vs. environmental attitude by donation status",
       x = "Environmental importance (Q11, 1–5)", y = "Voluntary preference") +
  theme_minimal()

# Compose
(p_dist + p_cell) / (p_interact + p_env)

# ------------------------------------------------------------------------------
# 8. Summary statistics for paper
# ------------------------------------------------------------------------------

cat("\n=== Summary statistics (analysis sample) ===\n")
anal_data |>
  summarise(
    n               = n(),
    mean_vol_pref   = round(mean(voluntary_pref), 3),
    sd_vol_pref     = round(sd(voluntary_pref), 3),
    pct_all_vol     = round(mean(voluntary_pref == 1) * 100, 1),
    pct_all_man     = round(mean(voluntary_pref == 0) * 100, 1),
    mean_donation   = round(mean(donation), 1),
    sd_donation     = round(sd(donation), 1),
    pct_donated     = round(mean(gave) * 100, 1),
    mean_env        = round(mean(env_import), 2),
    mean_n_mixed    = round(mean(n_mixed), 1)
  ) |> print()
