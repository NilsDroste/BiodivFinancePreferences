library(logitr)

args     <- commandArgs(trailingOnly = TRUE)
db_path  <- args[1]
out_path <- args[2]

# logitr expects long format: one row per alternative per choice situation
# We need: obsID (unique per choice set), altID, chosen, + attribute columns
db <- readRDS(db_path)

# Reshape from wide (one row per task) to long (3 rows per task: A, B, neither)
make_long <- function(db) {
  n <- nrow(db)

  obs_int <- as.integer(factor(paste0(db$ID, "_", db$task)))

  # Option A rows
  df_a <- data.frame(
    obsID  = obs_int,
    respID = db$ID,
    altID  = "a",
    chosen = as.integer(db$choice == 1),
    don    = db$optA_don,
    cert   = db$optA_cert,
    off    = db$optA_off,
    ind    = db$optA_ind,
    pland  = db$optA_pland,
    pmon   = db$optA_pmon,
    price  = db$optA_price
  )
  # Option B rows
  df_b <- data.frame(
    obsID  = obs_int,
    respID = db$ID,
    altID  = "b",
    chosen = as.integer(db$choice == 2),
    don    = db$optB_don,
    cert   = db$optB_cert,
    off    = db$optB_off,
    ind    = db$optB_ind,
    pland  = db$optB_pland,
    pmon   = db$optB_pmon,
    price  = db$optB_price
  )
  # Neither rows (opt-out): all attributes 0, ASC captured via altID dummy
  df_n <- data.frame(
    obsID  = obs_int,
    respID = db$ID,
    altID  = "neither",
    chosen = as.integer(db$choice == 3),
    don    = 0, cert = 0, off = 0, ind = 0, pland = 0, pmon = 0, price = 0
  )

  long <- rbind(df_a, df_b, df_n)
  long <- long[order(long$respID, long$obsID, long$altID), ]
  return(long)
}

long_db <- make_long(db)
cat("Long format rows:", nrow(long_db), "\n")

# ==============================================================================
# Conditional logit (for comparison / starting values)
# ==============================================================================
cat("\n--- Conditional logit ---\n")
long_db$asc_neither <- as.integer(long_db$altID == "neither")

cl2 <- logitr(
  data    = long_db,
  outcome = "chosen",
  obsID   = "obsID",
  pars    = c("asc_neither", "don", "cert", "off", "ind", "pland", "pmon", "price")
)
summary(cl2)

# ==============================================================================
# Mixed logit: random normal coefficients on all attribute dummies
# Price and ASC fixed
# Seeds and multiple starts ensure reproducibility and global optimum.
# ==============================================================================
cat("\n--- Mixed logit ---\n")
set.seed(42)   # reproducibility: fix the (Sobol) draw sequence logitr generates
mxl <- logitr(
  data           = long_db,
  outcome        = "chosen",
  obsID          = "obsID",
  panelID        = "respID",
  pars           = c("asc_neither", "don", "cert", "off", "ind", "pland", "pmon", "price"),
  randPars       = c(don = "n", cert = "n", off = "n", ind = "n", pland = "n", pmon = "n"),
  numDraws       = 500,
  numMultiStarts = 10   # guard against local optima; report best LL across starts
)
summary(mxl)

# --- WTP (SEK/year) from mean parameters ---
coef  <- coef(mxl)
price_coef <- coef["price"]

attr_means <- coef[c("don","cert","off","ind","pland","pmon")]
wtp_sek <- -attr_means / (price_coef / 1000)
names(wtp_sek) <- c("Donation","Certification","Offsetting","Industrial land","Public land","Public monitoring")
cat("\n=== Mean WTP (SEK/year) vs baseline: Tax + Small-scale private + Private monitoring ===\n")
print(round(wtp_sek))

# --- Share preferring each attribute (normal CDF) ---
sd_coef <- coef[paste0("sd_", c("don","cert","off","ind","pland","pmon"))]
ppos    <- pnorm(attr_means / abs(sd_coef))
names(ppos) <- names(wtp_sek)
cat("\n=== Share of population preferring each attribute over baseline ===\n")
print(round(ppos, 3))

saveRDS(mxl, file.path(out_path, "MXL_full_logitr.rds"))
