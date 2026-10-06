# ==============================================================================
# Graphical abstract (draft) for the One Earth submission.
#
# Cell Press graphical abstracts are a single visual that carries the argument
# without the reader consulting the paper. This one is built as three stacked
# bands following the paper's logic:
#
#   1. the setup   - Sweden is where voluntary provision should work best
#   2. the result  - citizens choose mandatory instruments anyway
#   3. the twist   - distrust and underprovision pull in opposite directions
#
# Wording note: voluntary instruments are 'ranked last', not 'rejected'. Even at
# the highest cost presented, and in tasks offering only voluntary options, most
# respondents still chose a programme over the status quo. The finding is an
# ordering, not a refusal, and the abstract should not overstate it.
#
# Colours match the manuscript figures so the abstract and the paper read as one
# piece: donation #D55E00, certification #E69F00, mandatory #3b4994.
#
# Cell Press ask for at least 1200 px on the shortest side AND that the whole
# thing stay readable at 5 x 5 cm on screen. The second constraint binds, not
# the first: at that display size a 9 pt label on this canvas renders near
# 3 pt. Type below is therefore sized against the canvas, with nothing
# under ~11 pt. Shrink it back and the abstract fails the legibility rule.
# ==============================================================================

library(tidyverse)
library(patchwork)
library(readxl)
library(here)
library(logitr)
source(here("analysis", "cluster_se.R"))

# --- Recompute the numbers shown, rather than hard-coding them ---------------
# Typed-in values drift silently when the analysis is revised. Everything
# quantitative below is estimated here from the deposited data; the only
# hard-coded positions are the two gauge markers in band 1, which are schematic
# rather than statistics (see comment there).

full_data <- read_csv(here("deposit", "biodivfinance_microdata_anon.csv"),
                      show_col_types = FALSE)
price_map  <- c("0" = 492, "1" = 2460, "2" = 4920, "3" = 7380)
design_raw <- read_excel(here("design", "Trial 3 - factorial grouped, svensk.xlsx"), sheet = 1)
names(design_raw) <- c("cs","block","a_source","a_src_txt","a_land","a_land_txt",
                       "a_monitor","a_mon_txt","a_price","a_price_txt",
                       "b_source","b_src_txt","b_land","b_land_txt",
                       "b_monitor","b_mon_txt","b_price","b_price_txt")
design <- design_raw |>
  select(cs, block, a_source, a_land, a_monitor, a_price,
         b_source, b_land, b_monitor, b_price) |>
  group_by(block) |> mutate(task = row_number()) |> ungroup() |>
  mutate(a_price_sek = price_map[as.character(a_price)],
         b_price_sek = price_map[as.character(b_price)])

ord1 <- full_data |> filter(ordning == 1) |>
  select(id, block, q2_1, q14, choice1:choice8) |>
  pivot_longer(choice1:choice8, names_to = "col", values_to = "choice")
ord2 <- full_data |> filter(ordning == 2) |>
  select(id, block, q2_1, q14, choice12, choice22, choice32, choice42,
         choice52, choice62, choice72, choice82) |>
  pivot_longer(-c(id, block, q2_1, q14), names_to = "col", values_to = "choice")
resp <- bind_rows(ord1, ord2) |>
  filter(!is.na(choice)) |>
  mutate(task = as.integer(substr(col, 7, 7))) |>
  left_join(design, by = c("block", "task")) |>
  mutate(choice_num = case_when(choice == "a" ~ 1L, choice == "b" ~ 2L, TRUE ~ 3L))

fit_cl <- function(d) {
  o <- as.integer(factor(paste0(d$id, "_", d$task)))
  side <- function(px, k) data.frame(
    obsID = o, chosen = as.integer(d$choice_num == k), asc = 0L,
    don   = as.integer(d[[paste0(px, "_source")]] == 1),
    cert  = as.integer(d[[paste0(px, "_source")]] == 2),
    off   = as.integer(d[[paste0(px, "_source")]] == 3),
    ind   = as.integer(d[[paste0(px, "_land")]] == 1),
    pland = as.integer(d[[paste0(px, "_land")]] == 2),
    pmon  = as.integer(d[[paste0(px, "_monitor")]] == 1),
    price = d[[paste0(px, "_price_sek")]] / 1000)
  sq <- data.frame(obsID = o, chosen = as.integer(d$choice_num == 3), asc = 1L,
                   don = 0, cert = 0, off = 0, ind = 0, pland = 0, pmon = 0, price = 0)
  L <- rbind(side("a", 1), side("b", 2), sq)
  coef(logitr(data = L[order(L$obsID), ], outcome = "chosen", obsID = "obsID",
              pars = c("asc","don","cert","off","ind","pland","pmon","price"),
              numMultiStarts = 3))
}

cf_all      <- fit_cl(resp)
cf_distrust <- fit_cl(filter(resp, q2_1 <= 2))   # distrusts the state with money
cf_doubt    <- fit_cl(filter(resp, q14  <= 2))   # doubts the state will deliver

MAND <- "#3b4994"; VOL <- "#D55E00"; CERT <- "#E69F00"; GREY <- "grey45"

# --- Band 1: the two conditions that make Sweden a most-likely case ----------
setup <- tibble(
  cond  = factor(c("Interpersonal trust", "Public biodiversity spending"),
                 levels = c("Interpersonal trust", "Public biodiversity spending")),
  # Schematic gauge positions, not statistics: they convey 'near the top' and
  # 'near the bottom'. The underlying ranks are in the labels beside them.
  value = c(0.95, 0.12),
  lab   = c("5th of 91 worldwide", "among Europe's lowest")
)

p1 <- ggplot(setup, aes(x = value, y = fct_rev(cond))) +
  geom_segment(aes(x = 0, xend = 1, y = fct_rev(cond), yend = fct_rev(cond)),
               colour = "grey88", linewidth = 4, lineend = "round") +
  geom_point(size = 6, colour = MAND) +
  geom_text(aes(label = lab), hjust = ifelse(setup$value > 0.5, 1.15, -0.15),
            size = 3.9, colour = GREY) +
  scale_x_continuous(limits = c(-0.05, 1.05)) +
  labs(title = "Where voluntary biodiversity finance should work best",
       subtitle = "Sweden: others can be expected to contribute,\nand the state is not already paying") +
  theme_void(base_size = 12) +
  theme(axis.text.y = element_text(hjust = 1, size = 11, colour = "grey20"),
        plot.title = element_text(face = "bold", size = 12),
        plot.subtitle = element_text(size = 11, colour = GREY),
        plot.margin = margin(6, 10, 10, 6))

# --- Band 2: the preference ordering -----------------------------------------
pref <- tibble(
  instrument = c("Mandatory\noffsetting", "Tax\n(reference)", "Certification", "Voluntary\ndonation"),
  utility    = c(cf_all[["off"]], 0, cf_all[["cert"]], cf_all[["don"]]),
  type       = c("Mandatory", "Mandatory", "Voluntary", "Voluntary")
) |>
  mutate(instrument = fct_reorder(instrument, utility))

p2 <- ggplot(pref, aes(x = utility, y = instrument, fill = type)) +
  geom_vline(xintercept = 0, colour = "grey75", linetype = "dashed") +
  geom_col(width = 0.62) +
  scale_fill_manual(values = c(Mandatory = MAND, Voluntary = VOL), guide = "none") +
  annotate("text", x = 0.02, y = 4.42, label = "preferred to tax",
           colour = MAND, size = 3.9, fontface = "bold", hjust = 0) +
  annotate("text", x = -0.02, y = 1.50, label = "ranked last",
           colour = VOL, size = 3.9, fontface = "bold", hjust = 1) +
  scale_x_continuous(limits = c(-0.52, 0.36)) +  # headroom for the larger annotations
  labs(title = "Citizens choose mandatory instruments anyway",
       subtitle = "n = 2,101; preference relative to a tax-financed programme",
       x = NULL, y = NULL) +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 11, colour = "grey20"),
        plot.title = element_text(face = "bold", size = 12),
        plot.subtitle = element_text(size = 11, colour = GREY),
        plot.margin = margin(6, 10, 6, 6))

# --- Band 3: the two judgements about the state ------------------------------
mech <- tibble(
  judgement = c("Doubt the state\nwill deliver", "Distrust the state\nwith money"),
  effect    = c(cf_doubt[["don"]], cf_distrust[["don"]]),
  outcome   = c("more mandatory", "more voluntary")  # donation coefficient vs tax
)

p3 <- ggplot(mech, aes(x = effect, y = judgement, fill = effect > 0)) +
  geom_vline(xintercept = 0, colour = "grey55") +
  geom_col(width = 0.5) +
  geom_text(aes(label = outcome, hjust = ifelse(mech$effect > 0, -0.1, 1.1)),
            size = 3.9, colour = GREY) +
  scale_fill_manual(values = c(`TRUE` = VOL, `FALSE` = MAND), guide = "none") +
  scale_x_continuous(limits = c(-0.92, 0.72)) +  # headroom for the larger annotations
  labs(title = "Underprovision and distrust are not the same thing",
       subtitle = "They move preferences in opposite directions",
       x = NULL, y = NULL) +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 11, colour = "grey20"),
        plot.title = element_text(face = "bold", size = 12),
        plot.subtitle = element_text(size = 11, colour = GREY),
        plot.margin = margin(6, 10, 6, 6))

ga <- p1 / p2 / p3 + plot_layout(heights = c(0.85, 1.3, 0.9))

ggsave(here::here("paper", "graphical_abstract.png"), ga,
       width = 6.8, height = 6.6, dpi = 300, bg = "white")
ggsave(here::here("paper", "graphical_abstract.pdf"), ga,
       width = 6.8, height = 6.6, device = pdf, bg = "white")
cat("Saved paper/graphical_abstract.png and .pdf\n")
