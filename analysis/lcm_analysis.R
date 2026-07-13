library(tidyverse)
library(readxl)
library(here)
library(apollo)

# ==============================================================================
# 1. Load data and build database (same structure as analysis.R)
# ==============================================================================

full_data <- read_excel(here("data/Full launch database", "4178_excel_databas.xlsx"), sheet = 2)

price_map  <- c("0" = 492, "1" = 2460, "2" = 4920, "3" = 7380)
design_raw <- read_excel(here("design", "Trial 3 - factorial grouped, svensk.xlsx"), sheet = 1)
names(design_raw) <- c(
  "cs", "block", "a_source", "a_src_txt", "a_land", "a_land_txt",
  "a_monitor", "a_mon_txt", "a_price", "a_price_txt",
  "b_source", "b_src_txt", "b_land", "b_land_txt",
  "b_monitor", "b_mon_txt", "b_price", "b_price_txt"
)
design <- design_raw |>
  select(cs, block, a_source, a_land, a_monitor, a_price, b_source, b_land, b_monitor, b_price) |>
  group_by(block) |> mutate(task = row_number()) |> ungroup() |>
  mutate(a_price_sek = price_map[as.character(a_price)],
         b_price_sek = price_map[as.character(b_price)])

choices_ord1 <- full_data |>
  filter(ordning == 1) |>
  select(id, block, choice1:choice8) |>
  pivot_longer(choice1:choice8, names_to = "col", values_to = "choice") |>
  mutate(task = as.integer(substr(col, 7, 7)))

choices_ord2 <- full_data |>
  filter(ordning == 2) |>
  select(id, block, choice12, choice22, choice32, choice42, choice52, choice62, choice72, choice82) |>
  pivot_longer(-c(id, block), names_to = "col", values_to = "choice") |>
  mutate(task = as.integer(substr(col, 7, 7)))

resp <- bind_rows(choices_ord1, choices_ord2) |>
  filter(!is.na(choice)) |>
  left_join(design, by = c("block", "task")) |>
  mutate(choice_num = case_when(choice == "a" ~ 1L, choice == "b" ~ 2L, TRUE ~ 3L))

# Respondent-level covariates for post-hoc class characterisation
resp_covariates <- full_data |>
  transmute(
    id,
    env_z = as.numeric(scale(as.numeric(q11))),
    don_z = as.numeric(scale(as.numeric(yourdonation)))
  )

database <- as.data.frame(
  resp |>
    transmute(
      ID     = id, task = task, choice = choice_num,
      optA_don   = as.integer(a_source == 1), optA_cert  = as.integer(a_source == 2),
      optA_off   = as.integer(a_source == 3), optA_ind   = as.integer(a_land == 1),
      optA_pland = as.integer(a_land == 2),   optA_pmon  = as.integer(a_monitor == 1),
      optA_price = a_price_sek / 1000,
      optB_don   = as.integer(b_source == 1), optB_cert  = as.integer(b_source == 2),
      optB_off   = as.integer(b_source == 3), optB_ind   = as.integer(b_land == 1),
      optB_pland = as.integer(b_land == 2),   optB_pmon  = as.integer(b_monitor == 1),
      optB_price = b_price_sek / 1000
    ) |>
    left_join(resp_covariates, by = c("ID" = "id"))
)

cat("Database:", nrow(database), "rows,", n_distinct(database$ID), "respondents\n")

# ==============================================================================
# 2. Apollo 2-class Latent Class MNL
# ==============================================================================
# Specification:
#   Class 1 (reference): expected mandatory-preference pattern (negative don/cert)
#   Class 2: expected voluntary-preference pattern (positive don/cert)
#   Class membership: unconditional (only intercept delta_0); env_z and don_z
#   are used post-estimation for class characterisation (two-step approach)
#
# The two-step approach avoids the complication of respondent-level covariates
# entering the task-level probability function. After estimation, posterior class
# membership probabilities are computed and regressed on env_z and don_z.

apollo_initialise()

apollo_control <- list(
  modelName       = "LCM_2class",
  modelDescr      = "2-class LC-MNL: mandatory vs. voluntary preference classes",
  indivID         = "ID",
  outputDirectory = here("analysis"),
  mixing          = FALSE
)

apollo_beta <- c(
  delta_0    = 0,      # log-odds of class 2 (voluntary) relative to class 1 (mandatory)
  # Class 1 utilities (starting values informed by CL estimates)
  b1_asc    = -1.2,   b1_don  = -0.55, b1_cert = -0.15, b1_off  =  0.20,
  b1_ind    =  0.01,  b1_pland =  0.28, b1_pmon =  0.40, b1_price = -0.20,
  # Class 2 utilities (expect reversal on don/cert vs. class 1)
  b2_asc    = -0.8,   b2_don  =  0.20, b2_cert =  0.15, b2_off  = -0.10,
  b2_ind    =  0.01,  b2_pland =  0.15, b2_pmon =  0.20, b2_price = -0.20
)
apollo_fixed <- c()

apollo_inputs <- apollo_validateInputs()

apollo_probabilities <- function(apollo_beta, apollo_inputs, functionality = "estimate") {
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))

  P <- list()

  # Class 1: mandatory-preference class
  V1 <- list(
    a = b1_don * optA_don + b1_cert * optA_cert + b1_off * optA_off +
        b1_ind * optA_ind + b1_pland * optA_pland + b1_pmon * optA_pmon +
        b1_price * optA_price,
    b = b1_don * optB_don + b1_cert * optB_cert + b1_off * optB_off +
        b1_ind * optB_ind + b1_pland * optB_pland + b1_pmon * optB_pmon +
        b1_price * optB_price,
    neither = b1_asc + 0 * optA_price
  )
  P1 <- list(model = apollo_mnl(
    list(componentName = "MNL_class1",
         alternatives  = c(a = 1, b = 2, neither = 3),
         avail         = list(a = 1, b = 1, neither = 1),
         choiceVar     = choice,
         utilities     = V1),
    functionality
  ))
  P1 <- apollo_panelProd(P1, apollo_inputs, functionality)
  P[["class_1"]] <- P1[["model"]]

  # Class 2: voluntary-preference class
  V2 <- list(
    a = b2_don * optA_don + b2_cert * optA_cert + b2_off * optA_off +
        b2_ind * optA_ind + b2_pland * optA_pland + b2_pmon * optA_pmon +
        b2_price * optA_price,
    b = b2_don * optB_don + b2_cert * optB_cert + b2_off * optB_off +
        b2_ind * optB_ind + b2_pland * optB_pland + b2_pmon * optB_pmon +
        b2_price * optB_price,
    neither = b2_asc + 0 * optA_price
  )
  P2 <- list(model = apollo_mnl(
    list(componentName = "MNL_class2",
         alternatives  = c(a = 1, b = 2, neither = 3),
         avail         = list(a = 1, b = 1, neither = 1),
         choiceVar     = choice,
         utilities     = V2),
    functionality
  ))
  P2 <- apollo_panelProd(P2, apollo_inputs, functionality)
  P[["class_2"]] <- P2[["model"]]

  # Class membership probabilities (unconditional binary logit; class 1 = reference)
  pi1 <- 1 / (1 + exp(delta_0))
  pi2 <- 1 - pi1

  # Combined LC likelihood: weighted sum of per-class panel likelihoods
  P[["model"]] <- pi1 * P1[["model"]] + pi2 * P2[["model"]]

  P <- apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

lcm_model <- apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)
apollo_modelOutput(lcm_model, modelOutput_settings = list(printPVal = TRUE))

# ==============================================================================
# 3. Class shares and WTP by class
# ==============================================================================

coef <- lcm_model$estimate

class1_share <- 1 / (1 + exp(coef["delta_0"]))
class2_share <- 1 - class1_share
cat("\n--- Estimated class shares (unconditional) ---\n")
cat("Class 1 (mandatory-preference):", round(class1_share * 100, 1), "%\n")
cat("Class 2 (voluntary-preference):", round(class2_share * 100, 1), "%\n")

# WTP by class (SEK/year)
wtp_class <- function(prefix) {
  betas <- coef[paste0(prefix, c("don","cert","off","ind","pland","pmon"))]
  wtp   <- -betas / (coef[paste0(prefix, "price")] / 1000)
  names(wtp) <- c("Donation","Certification","Offsetting","Industrial land","Public land","Public monitoring")
  round(wtp)
}
cat("\n--- WTP Class 1 (SEK/year) ---\n"); print(wtp_class("b1_"))
cat("\n--- WTP Class 2 (SEK/year) ---\n"); print(wtp_class("b2_"))

# ==============================================================================
# 4. Post-hoc class characterisation via posterior membership probabilities
# ==============================================================================
# Posterior P(class=k | choices_i) via Bayes: requires per-class, per-individual
# likelihood. We extract these by running apollo_probabilities in "conditionals" mode
# using apollo_conditionals (if available in installed version), or approximate via
# the ratio approach below.

# Approximate posterior: weight unconditional pi_k by within-class panel likelihood
# The within-class panel likelihoods are stored in the model object under fitted values.
# Use apollo_conditionals for a clean extraction if the version supports it.

# ==============================================================================
# 4. Manual posterior class membership probabilities
# ==============================================================================
# Posterior P(class=k | choices_i) = pi_k * L_ki / (pi_1*L_1i + pi_2*L_2i)
# where L_ki = product of within-class choice probabilities across tasks for i

compute_panel_lik <- function(db, coef, pfx, asc_key) {
  VA <- coef[paste0(pfx,"don")]   * db$optA_don   +
        coef[paste0(pfx,"cert")]  * db$optA_cert  +
        coef[paste0(pfx,"off")]   * db$optA_off   +
        coef[paste0(pfx,"ind")]   * db$optA_ind   +
        coef[paste0(pfx,"pland")] * db$optA_pland +
        coef[paste0(pfx,"pmon")]  * db$optA_pmon  +
        coef[paste0(pfx,"price")] * db$optA_price

  VB <- coef[paste0(pfx,"don")]   * db$optB_don   +
        coef[paste0(pfx,"cert")]  * db$optB_cert  +
        coef[paste0(pfx,"off")]   * db$optB_off   +
        coef[paste0(pfx,"ind")]   * db$optB_ind   +
        coef[paste0(pfx,"pland")] * db$optB_pland +
        coef[paste0(pfx,"pmon")]  * db$optB_pmon  +
        coef[paste0(pfx,"price")] * db$optB_price

  VN <- rep(coef[asc_key], nrow(db))
  denom <- exp(VA) + exp(VB) + exp(VN)

  log_pr <- dplyr::case_when(
    db$choice == 1 ~ log(exp(VA) / denom),
    db$choice == 2 ~ log(exp(VB) / denom),
    TRUE           ~ log(exp(VN) / denom)
  )

  data.frame(ID = db$ID, log_pr = log_pr) |>
    dplyr::group_by(ID) |>
    dplyr::summarise(panel_lik = exp(sum(log_pr)), .groups = "drop")
}

coef <- lcm_model$estimate
pi1  <- 1 / (1 + exp(coef["delta_0"]))
pi2  <- 1 - pi1

L1 <- compute_panel_lik(database, coef, "b1_", "b1_asc")
L2 <- compute_panel_lik(database, coef, "b2_", "b2_asc")

posterior <- L1 |>
  dplyr::left_join(L2, by = "ID", suffix = c("_c1", "_c2")) |>
  dplyr::mutate(
    post_mix    = pi1 * panel_lik_c1 + pi2 * panel_lik_c2,
    post_class1 = pi1 * panel_lik_c1 / post_mix,
    post_class2 = pi2 * panel_lik_c2 / post_mix,
    modal_class = dplyr::if_else(post_class1 > 0.5,
                                 "Class 1 (strong mandatory, 30.5%)",
                                 "Class 2 (moderate mandatory, 69.5%)")
  ) |>
  dplyr::left_join(resp_covariates, by = c("ID" = "id"))

cat("\n--- Posterior class characterisation (mean env_z and don_z by modal class) ---\n")
posterior |>
  dplyr::group_by(modal_class) |>
  dplyr::summarise(
    n       = dplyr::n(),
    pct     = round(100 * dplyr::n() / nrow(posterior), 1),
    env_z   = round(mean(env_z, na.rm = TRUE), 3),
    don_z   = round(mean(don_z, na.rm = TRUE), 3),
    .groups = "drop"
  ) |>
  print()

# Save model
saveRDS(lcm_model, here("analysis", "LCM_2class_apollo.rds"))
cat("\nModel saved to analysis/LCM_2class_apollo.rds\n")


# ==============================================================================
# Session info (for reproducibility)
# ==============================================================================
cat("\n=== Session information ===\n")
print(sessionInfo())
