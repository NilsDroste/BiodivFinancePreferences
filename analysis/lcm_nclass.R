library(tidyverse)
library(readxl)
library(here)
library(apollo)

# ==============================================================================
# LCM class-number specification test: 3 and 4 classes
# Model selection via BIC / AIC alongside saved 2-class model.
# Results feed into Appendix H.
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. Data preparation (mirrors lcm_analysis.R)
# ------------------------------------------------------------------------------

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
  select(cs, block, a_source, a_land, a_monitor, a_price,
         b_source, b_land, b_monitor, b_price) |>
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
  select(id, block, choice12, choice22, choice32, choice42,
         choice52, choice62, choice72, choice82) |>
  pivot_longer(-c(id, block), names_to = "col", values_to = "choice") |>
  mutate(task = as.integer(substr(col, 7, 7)))

resp <- bind_rows(choices_ord1, choices_ord2) |>
  filter(!is.na(choice)) |>
  left_join(design, by = c("block", "task")) |>
  mutate(choice_num = case_when(choice == "a" ~ 1L, choice == "b" ~ 2L, TRUE ~ 3L))

database <- as.data.frame(
  resp |>
    transmute(
      ID = id, task = task, choice = choice_num,
      optA_don   = as.integer(a_source == 1), optA_cert  = as.integer(a_source == 2),
      optA_off   = as.integer(a_source == 3), optA_ind   = as.integer(a_land == 1),
      optA_pland = as.integer(a_land == 2),   optA_pmon  = as.integer(a_monitor == 1),
      optA_price = a_price_sek / 1000,
      optB_don   = as.integer(b_source == 1), optB_cert  = as.integer(b_source == 2),
      optB_off   = as.integer(b_source == 3), optB_ind   = as.integer(b_land == 1),
      optB_pland = as.integer(b_land == 2),   optB_pmon  = as.integer(b_monitor == 1),
      optB_price = b_price_sek / 1000
    )
)
cat("Database:", nrow(database), "rows,", n_distinct(database$ID), "respondents\n")

# ==============================================================================
# 2. THREE-CLASS LCM
# ==============================================================================

apollo_initialise()

apollo_control <- list(
  modelName       = "LCM_3class",
  modelDescr      = "3-class LC-MNL specification test",
  indivID         = "ID",
  outputDirectory = here("analysis"),
  mixing          = FALSE
)

apollo_beta <- c(
  delta_1 = 0.8,   delta_2 = -0.5,
  b1_asc = -9.8,  b1_don = -1.7,  b1_cert = -0.2, b1_off =  0.3,
  b1_ind = -0.5,  b1_pland =  0.7, b1_pmon =  1.0, b1_price = -1.0,
  b2_asc = -0.3,  b2_don = -0.2,  b2_cert = -0.1, b2_off =  0.1,
  b2_ind =  0.1,  b2_pland =  0.2, b2_pmon =  0.3, b2_price = -0.1,
  b3_asc = -0.8,  b3_don =  0.1,  b3_cert =  0.2, b3_off =  0.0,
  b3_ind =  0.1,  b3_pland =  0.1, b3_pmon =  0.2, b3_price = -0.2
)
apollo_fixed <- c()
apollo_inputs <- apollo_validateInputs()

apollo_probabilities <- function(apollo_beta, apollo_inputs, functionality = "estimate") {
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  P <- list()

  V1 <- list(
    a       = b1_don*optA_don + b1_cert*optA_cert + b1_off*optA_off +
              b1_ind*optA_ind + b1_pland*optA_pland + b1_pmon*optA_pmon + b1_price*optA_price,
    b       = b1_don*optB_don + b1_cert*optB_cert + b1_off*optB_off +
              b1_ind*optB_ind + b1_pland*optB_pland + b1_pmon*optB_pmon + b1_price*optB_price,
    neither = b1_asc + 0*optA_price
  )
  V2 <- list(
    a       = b2_don*optA_don + b2_cert*optA_cert + b2_off*optA_off +
              b2_ind*optA_ind + b2_pland*optA_pland + b2_pmon*optA_pmon + b2_price*optA_price,
    b       = b2_don*optB_don + b2_cert*optB_cert + b2_off*optB_off +
              b2_ind*optB_ind + b2_pland*optB_pland + b2_pmon*optB_pmon + b2_price*optB_price,
    neither = b2_asc + 0*optA_price
  )
  V3 <- list(
    a       = b3_don*optA_don + b3_cert*optA_cert + b3_off*optA_off +
              b3_ind*optA_ind + b3_pland*optA_pland + b3_pmon*optA_pmon + b3_price*optA_price,
    b       = b3_don*optB_don + b3_cert*optB_cert + b3_off*optB_off +
              b3_ind*optB_ind + b3_pland*optB_pland + b3_pmon*optB_pmon + b3_price*optB_price,
    neither = b3_asc + 0*optA_price
  )

  P1 <- list(model = apollo_mnl(list(componentName="MNL_c1",
    alternatives=c(a=1,b=2,neither=3), avail=list(a=1,b=1,neither=1),
    choiceVar=choice, utilities=V1), functionality))
  P2 <- list(model = apollo_mnl(list(componentName="MNL_c2",
    alternatives=c(a=1,b=2,neither=3), avail=list(a=1,b=1,neither=1),
    choiceVar=choice, utilities=V2), functionality))
  P3 <- list(model = apollo_mnl(list(componentName="MNL_c3",
    alternatives=c(a=1,b=2,neither=3), avail=list(a=1,b=1,neither=1),
    choiceVar=choice, utilities=V3), functionality))

  P1 <- apollo_panelProd(P1, apollo_inputs, functionality)
  P2 <- apollo_panelProd(P2, apollo_inputs, functionality)
  P3 <- apollo_panelProd(P3, apollo_inputs, functionality)

  P[["class_1"]] <- P1[["model"]]
  P[["class_2"]] <- P2[["model"]]
  P[["class_3"]] <- P3[["model"]]

  denom <- 1 + exp(delta_1) + exp(delta_2)
  pi1 <- 1 / denom
  pi2 <- exp(delta_1) / denom
  pi3 <- exp(delta_2) / denom

  P[["model"]] <- pi1*P[["class_1"]] + pi2*P[["class_2"]] + pi3*P[["class_3"]]
  P <- apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

lcm3 <- apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)
saveRDS(lcm3, here("analysis", "LCM_3class_apollo.rds"))
cat("3-class model saved.\n")
apollo_modelOutput(lcm3, modelOutput_settings = list(printPVal = TRUE))

# ==============================================================================
# 3. FOUR-CLASS LCM
# ==============================================================================

apollo_initialise()

apollo_control <- list(
  modelName       = "LCM_4class",
  modelDescr      = "4-class LC-MNL specification test",
  indivID         = "ID",
  outputDirectory = here("analysis"),
  mixing          = FALSE
)

apollo_beta <- c(
  delta_1 = 0.8, delta_2 = -0.5, delta_3 = -1.0,
  b1_asc = -9.8,  b1_don = -1.7,  b1_cert = -0.2, b1_off =  0.3,
  b1_ind = -0.5,  b1_pland =  0.7, b1_pmon =  1.0, b1_price = -1.0,
  b2_asc = -0.3,  b2_don = -0.2,  b2_cert = -0.1, b2_off =  0.1,
  b2_ind =  0.1,  b2_pland =  0.2, b2_pmon =  0.3, b2_price = -0.1,
  b3_asc = -0.8,  b3_don =  0.1,  b3_cert =  0.2, b3_off =  0.0,
  b3_ind =  0.1,  b3_pland =  0.1, b3_pmon =  0.2, b3_price = -0.2,
  b4_asc = -0.5,  b4_don = -0.1,  b4_cert =  0.1, b4_off =  0.2,
  b4_ind =  0.0,  b4_pland =  0.1, b4_pmon =  0.1, b4_price = -0.3
)
apollo_fixed <- c()
apollo_inputs <- apollo_validateInputs()

apollo_probabilities <- function(apollo_beta, apollo_inputs, functionality = "estimate") {
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  P <- list()

  V1 <- list(
    a       = b1_don*optA_don + b1_cert*optA_cert + b1_off*optA_off +
              b1_ind*optA_ind + b1_pland*optA_pland + b1_pmon*optA_pmon + b1_price*optA_price,
    b       = b1_don*optB_don + b1_cert*optB_cert + b1_off*optB_off +
              b1_ind*optB_ind + b1_pland*optB_pland + b1_pmon*optB_pmon + b1_price*optB_price,
    neither = b1_asc + 0*optA_price
  )
  V2 <- list(
    a       = b2_don*optA_don + b2_cert*optA_cert + b2_off*optA_off +
              b2_ind*optA_ind + b2_pland*optA_pland + b2_pmon*optA_pmon + b2_price*optA_price,
    b       = b2_don*optB_don + b2_cert*optB_cert + b2_off*optB_off +
              b2_ind*optB_ind + b2_pland*optB_pland + b2_pmon*optB_pmon + b2_price*optB_price,
    neither = b2_asc + 0*optA_price
  )
  V3 <- list(
    a       = b3_don*optA_don + b3_cert*optA_cert + b3_off*optA_off +
              b3_ind*optA_ind + b3_pland*optA_pland + b3_pmon*optA_pmon + b3_price*optA_price,
    b       = b3_don*optB_don + b3_cert*optB_cert + b3_off*optB_off +
              b3_ind*optB_ind + b3_pland*optB_pland + b3_pmon*optB_pmon + b3_price*optB_price,
    neither = b3_asc + 0*optA_price
  )
  V4 <- list(
    a       = b4_don*optA_don + b4_cert*optA_cert + b4_off*optA_off +
              b4_ind*optA_ind + b4_pland*optA_pland + b4_pmon*optA_pmon + b4_price*optA_price,
    b       = b4_don*optB_don + b4_cert*optB_cert + b4_off*optB_off +
              b4_ind*optB_ind + b4_pland*optB_pland + b4_pmon*optB_pmon + b4_price*optB_price,
    neither = b4_asc + 0*optA_price
  )

  P1 <- list(model = apollo_mnl(list(componentName="MNL_c1",
    alternatives=c(a=1,b=2,neither=3), avail=list(a=1,b=1,neither=1),
    choiceVar=choice, utilities=V1), functionality))
  P2 <- list(model = apollo_mnl(list(componentName="MNL_c2",
    alternatives=c(a=1,b=2,neither=3), avail=list(a=1,b=1,neither=1),
    choiceVar=choice, utilities=V2), functionality))
  P3 <- list(model = apollo_mnl(list(componentName="MNL_c3",
    alternatives=c(a=1,b=2,neither=3), avail=list(a=1,b=1,neither=1),
    choiceVar=choice, utilities=V3), functionality))
  P4 <- list(model = apollo_mnl(list(componentName="MNL_c4",
    alternatives=c(a=1,b=2,neither=3), avail=list(a=1,b=1,neither=1),
    choiceVar=choice, utilities=V4), functionality))

  P1 <- apollo_panelProd(P1, apollo_inputs, functionality)
  P2 <- apollo_panelProd(P2, apollo_inputs, functionality)
  P3 <- apollo_panelProd(P3, apollo_inputs, functionality)
  P4 <- apollo_panelProd(P4, apollo_inputs, functionality)

  P[["class_1"]] <- P1[["model"]]
  P[["class_2"]] <- P2[["model"]]
  P[["class_3"]] <- P3[["model"]]
  P[["class_4"]] <- P4[["model"]]

  denom <- 1 + exp(delta_1) + exp(delta_2) + exp(delta_3)
  pi1 <- 1 / denom
  pi2 <- exp(delta_1) / denom
  pi3 <- exp(delta_2) / denom
  pi4 <- exp(delta_3) / denom

  P[["model"]] <- pi1*P[["class_1"]] + pi2*P[["class_2"]] +
                  pi3*P[["class_3"]] + pi4*P[["class_4"]]
  P <- apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

lcm4 <- apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)
saveRDS(lcm4, here("analysis", "LCM_4class_apollo.rds"))
cat("4-class model saved.\n")
apollo_modelOutput(lcm4, modelOutput_settings = list(printPVal = TRUE))

# ==============================================================================
# 4. Model fit comparison table
# ==============================================================================

lcm2 <- readRDS(here("analysis", "LCM_2class_apollo.rds"))
n_resp <- n_distinct(database$ID)

model_fit <- tibble(
  Classes = c(2L, 3L, 4L),
  Params  = c(lcm2$nFreeParams, lcm3$nFreeParams, lcm4$nFreeParams),
  LL      = round(c(lcm2$finalLL, lcm3$finalLL, lcm4$finalLL), 1),
  AIC     = round(c(lcm2$AIC, lcm3$AIC, lcm4$AIC), 1),
  BIC     = round(-2 * c(lcm2$finalLL, lcm3$finalLL, lcm4$finalLL) +
                  log(n_resp) * c(lcm2$nFreeParams, lcm3$nFreeParams, lcm4$nFreeParams), 1)
)

cat("\n=== Model fit comparison ===\n")
print(model_fit)

saveRDS(model_fit, here("analysis", "LCM_fit_comparison.rds"))
cat("Fit comparison saved.\n")

# ==============================================================================
# Session info
# ==============================================================================
cat("\n=== Session information ===\n")
print(sessionInfo())
