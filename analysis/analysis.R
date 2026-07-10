library(tidyverse)
library(janitor)
library(readr)
library(here)
library(patchwork)
library(apollo)


full_data <- readxl::read_excel(here("data/Full launch database", "4178_excel_databas.xlsx"), sheet = 2)
full_data_annotated <- readxl::read_excel(here("data/Full launch database", "4178_excel_databas.xlsx"), sheet = 3)
codebook <- readxl::read_excel(here("data/Full launch database", "4178_excel_databas.xlsx"), sheet = 4)

# value label maps
instrument_labels <- c("1" = "Tax payment", "2" = "Voluntary donation", "3" = "Market certification", "4" = "Mandatory offsetting")
ownership_labels  <- c("1" = "Public land", "2" = "Industrial private", "3" = "Small-scale private")
audit_labels      <- c("1" = "Public administration", "2" = "Private audit")
trust_labels      <- c("1" = "1 - Do not trust", "2" = "2", "3" = "3", "4" = "4", "5" = "5 - Trust completely")
protection_labels <- c("1" = "1 - Not at all", "2" = "2", "3" = "3", "4" = "4", "5" = "5 - Completely")
monitoring_labels <- c("1" = "1 - Definitely not", "2" = "2", "3" = "3", "4" = "4", "5" = "5 - Definitely yes")

# Q1: instrument matching (correct answer = x-axis label)
p1 <- full_data |> select(contains("q1_")) |>
  rename(certification = 1, tax = 2, donation = 3, offsetting = 4) |>
  mutate(across(everything(), ~ factor(instrument_labels[as.character(.x)], levels = instrument_labels))) |>
  pivot_longer(everything(), names_to = "item", values_to = "answer") |>
  mutate(item = factor(item, levels = c("certification", "tax", "donation", "offsetting"))) |>
  count(item, answer) |>
  ggplot(aes(x = item, y = n, fill = answer)) +
  geom_bar(position = "fill", stat = "identity") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Q1: Instruments", subtitle = "Correct answer = x-axis label", x = NULL, y = NULL, fill = NULL) +
  theme_minimal() + theme(axis.text.x = element_text(angle = 20, hjust = 1), legend.position = "bottom")

# Q2: trust in funding handler (Likert 1-5)
p2 <- full_data |> select(contains("q2_")) |>
  rename(State = 1, "Private market" = 2, NGOs = 3) |>
  mutate(across(everything(), ~ factor(trust_labels[as.character(.x)], levels = trust_labels))) |>
  pivot_longer(everything(), names_to = "item", values_to = "answer") |>
  count(item, answer) |>
  ggplot(aes(x = item, y = n, fill = answer)) +
  geom_bar(position = "fill", stat = "identity") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "RdYlGn") +
  labs(title = "Q2: Trust in funding handler", x = NULL, y = NULL, fill = NULL) +
  theme_minimal() + theme(legend.position = "bottom")

# Q3: ownership matching (correct answer = x-axis label)
p3 <- full_data |> select(contains("q3_")) |>
  rename(industrial = 1, public = 2, "small-scale" = 3) |>
  mutate(across(everything(), ~ factor(ownership_labels[as.character(.x)], levels = ownership_labels))) |>
  pivot_longer(everything(), names_to = "item", values_to = "answer") |>
  mutate(item = factor(item, levels = c("industrial", "public", "small-scale"))) |>
  count(item, answer) |>
  ggplot(aes(x = item, y = n, fill = answer)) +
  geom_bar(position = "fill", stat = "identity") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Q3: Ownership", subtitle = "Correct answer = x-axis label", x = NULL, y = NULL, fill = NULL) +
  theme_minimal() + theme(axis.text.x = element_text(angle = 20, hjust = 1), legend.position = "bottom")

# Q4: desired protection level by land type (Likert 1-5)
p4 <- full_data |> select(contains("q4_")) |>
  rename(Public = 1, Industrial = 2, "Small-scale private" = 3) |>
  mutate(across(everything(), ~ factor(protection_labels[as.character(.x)], levels = protection_labels))) |>
  pivot_longer(everything(), names_to = "item", values_to = "answer") |>
  count(item, answer) |>
  ggplot(aes(x = item, y = n, fill = answer)) +
  geom_bar(position = "fill", stat = "identity") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "RdYlGn") +
  labs(title = "Q4: Desired protection by land type", x = NULL, y = NULL, fill = NULL) +
  theme_minimal() + theme(axis.text.x = element_text(angle = 20, hjust = 1), legend.position = "bottom")

# Q5: audit matching (correct answer = x-axis label)
p5 <- full_data |> select(contains("q5_")) |>
  rename(private = 1, public = 2) |>
  mutate(across(everything(), ~ factor(audit_labels[as.character(.x)], levels = audit_labels))) |>
  pivot_longer(everything(), names_to = "item", values_to = "answer") |>
  mutate(item = factor(item, levels = c("private", "public"))) |>
  count(item, answer) |>
  ggplot(aes(x = item, y = n, fill = answer)) +
  geom_bar(position = "fill", stat = "identity") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Q5: Audit matching", subtitle = "Correct answer = x-axis label", x = NULL, y = NULL, fill = NULL) +
  theme_minimal() + theme(legend.position = "bottom")

# Q6: preferred monitoring type (Likert 1-5)
p6 <- full_data |> select(contains("q6_")) |>
  rename("Public administration" = 1, "Private audit" = 2) |>
  mutate(across(everything(), ~ factor(monitoring_labels[as.character(.x)], levels = monitoring_labels))) |>
  pivot_longer(everything(), names_to = "item", values_to = "answer") |>
  count(item, answer) |>
  ggplot(aes(x = item, y = n, fill = answer)) +
  geom_bar(position = "fill", stat = "identity") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "RdYlGn") +
  labs(title = "Q6: Preferred monitoring type", x = NULL, y = NULL, fill = NULL) +
  theme_minimal() + theme(axis.text.x = element_text(angle = 20, hjust = 1), legend.position = "bottom")

(p1 + p2) / (p3 + p4) / (p5 + p6)


# ==============================================================================
# Choice experiment analysis (conditional logit via apollo)
# ==============================================================================

# --- design matrix ---
price_map <- c("0" = 492, "1" = 2460, "2" = 4920, "3" = 7380)

design_raw <- readxl::read_excel(here("design", "Trial 3 - factorial grouped, svensk.xlsx"), sheet = 1)
names(design_raw) <- c(
  "cs", "block", "a_source", "a_src_txt", "a_land", "a_land_txt",
  "a_monitor", "a_mon_txt", "a_price", "a_price_txt",
  "b_source", "b_src_txt", "b_land", "b_land_txt",
  "b_monitor", "b_mon_txt", "b_price", "b_price_txt"
)
design <- design_raw |>
  select(cs, block, a_source, a_land, a_monitor, a_price, b_source, b_land, b_monitor, b_price) |>
  group_by(block) |>
  mutate(task = row_number()) |>
  ungroup() |>
  mutate(
    a_price_sek = price_map[as.character(a_price)],
    b_price_sek = price_map[as.character(b_price)]
  )

# --- extract choices (ordning=1: choice1-8, ordning=2: choice12-82) ---
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

# --- apollo database: wide format, one row per respondent × task ---
# Attributes: source (0=tax,1=donation,2=cert,3=offset), land (0=small-scale,1=industrial,2=public),
#             monitor (0=private,1=public), price in kSEK
# Baseline (reference): tax payment + small-scale private + private monitoring
database <- resp |>
  transmute(
    ID    = id,
    task  = task,
    choice = choice_num,
    # Option A
    optA_don   = as.integer(a_source == 1), optA_cert  = as.integer(a_source == 2),
    optA_off   = as.integer(a_source == 3), optA_ind   = as.integer(a_land == 1),
    optA_pland = as.integer(a_land == 2),   optA_pmon  = as.integer(a_monitor == 1),
    optA_price = a_price_sek / 1000,
    # Option B
    optB_don   = as.integer(b_source == 1), optB_cert  = as.integer(b_source == 2),
    optB_off   = as.integer(b_source == 3), optB_ind   = as.integer(b_land == 1),
    optB_pland = as.integer(b_land == 2),   optB_pmon  = as.integer(b_monitor == 1),
    optB_price = b_price_sek / 1000
  )

# --- apollo setup ---
apollo_initialise()

apollo_control <- list(
  modelName   = "CL_full",
  modelDescr  = "Conditional logit - full data DCE",
  indivID     = "ID",
  outputDirectory = here("analysis")
)

apollo_beta <- c(
  asc_n      = 0,   # ASC for opt-out ("neither")
  beta_don   = 0,   # donation vs. tax (base)
  beta_cert  = 0,   # certification vs. tax
  beta_off   = 0,   # offsetting vs. tax
  beta_ind   = 0,   # industrial land vs. small-scale private (base)
  beta_pland = 0,   # public land vs. small-scale private
  beta_pmon  = 0,   # public monitoring vs. private audit (base)
  beta_price = 0    # price (kSEK/year), expected negative
)
apollo_fixed <- c()

apollo_inputs <- apollo_validateInputs()

apollo_probabilities <- function(apollo_beta, apollo_inputs, functionality = "estimate") {
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))

  P <- list()

  V <- list(
    a = beta_don * optA_don + beta_cert * optA_cert + beta_off * optA_off +
      beta_ind * optA_ind + beta_pland * optA_pland +
      beta_pmon * optA_pmon + beta_price * optA_price,
    b = beta_don * optB_don + beta_cert * optB_cert + beta_off * optB_off +
      beta_ind * optB_ind + beta_pland * optB_pland +
      beta_pmon * optB_pmon + beta_price * optB_price,
    neither = asc_n + 0 * optA_price
  )

  mnl_settings <- list(
    alternatives = c(a = 1, b = 2, neither = 3),
    avail        = list(a = 1, b = 1, neither = 1),
    choiceVar    = choice,
    utilities    = V
  )

  P[["model"]] <- apollo_mnl(mnl_settings, functionality)
  P <- apollo_panelProd(P, apollo_inputs, functionality)
  P <- apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

# --- estimation ---
cl_model <- apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)
apollo_modelOutput(cl_model, modelOutput_settings = list(printPVal = TRUE))

# --- implicit WTP (SEK/year) ---
coef <- cl_model$estimate
wtp_sek <- -coef[c("beta_don", "beta_cert", "beta_off", "beta_ind", "beta_pland", "beta_pmon")] /
  (coef["beta_price"] / 1000)
names(wtp_sek) <- c("Donation", "Certification", "Offsetting", "Industrial land", "Public land", "Public monitoring")
cat("\n=== Implicit WTP (SEK/year) vs baseline: Tax + Small-scale private + Private monitoring ===\n")
print(round(wtp_sek))