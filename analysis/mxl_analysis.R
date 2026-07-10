library(here)
library(readxl)
library(dplyr)
library(tidyr)

# ==============================================================================
# Step 1: Build apollo database
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

choices_ord1 <- full_data |> filter(ordning == 1) |>
  select(id, block, choice1:choice8) |>
  pivot_longer(choice1:choice8, names_to = "col", values_to = "choice") |>
  mutate(task = as.integer(substr(col, 7, 7)))

choices_ord2 <- full_data |> filter(ordning == 2) |>
  select(id, block, choice12, choice22, choice32, choice42, choice52, choice62, choice72, choice82) |>
  pivot_longer(-c(id, block), names_to = "col", values_to = "choice") |>
  mutate(task = as.integer(substr(col, 7, 7)))

resp <- bind_rows(choices_ord1, choices_ord2) |>
  filter(!is.na(choice)) |>
  left_join(design, by = c("block", "task")) |>
  mutate(choice_num = case_when(choice == "a" ~ 1L, choice == "b" ~ 2L, TRUE ~ 3L))

database <- as.data.frame(resp |>
  transmute(
    ID    = id, task = task, choice = choice_num,
    optA_don   = as.integer(a_source == 1), optA_cert  = as.integer(a_source == 2),
    optA_off   = as.integer(a_source == 3), optA_ind   = as.integer(a_land == 1),
    optA_pland = as.integer(a_land == 2),   optA_pmon  = as.integer(a_monitor == 1),
    optA_price = a_price_sek / 1000,
    optB_don   = as.integer(b_source == 1), optB_cert  = as.integer(b_source == 2),
    optB_off   = as.integer(b_source == 3), optB_ind   = as.integer(b_land == 1),
    optB_pland = as.integer(b_land == 2),   optB_pmon  = as.integer(b_monitor == 1),
    optB_price = b_price_sek / 1000
  ))

db_path <- here("analysis", "mxl_database.rds")
saveRDS(database, db_path)
cat("Database saved:", nrow(database), "rows\n")

# ==============================================================================
# Step 2: Run MXL in a clean Rscript subprocess (Apollo needs global env)
# ==============================================================================

runner  <- here("analysis", "_mxl_runner.R")
out_dir <- here("analysis")

ret <- system2("Rscript", args = c(runner, db_path, out_dir), stdout = "", stderr = "")
if (ret != 0) stop("MXL runner script failed — check output above.")
