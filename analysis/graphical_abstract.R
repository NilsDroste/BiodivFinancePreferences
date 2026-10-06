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
# This is a sketch for discussion, not final art. Cell Press ask for at least
# 1200 px on the shortest side; the PNG below is written at 300 dpi.
# ==============================================================================

library(tidyverse)
library(patchwork)

MAND <- "#3b4994"; VOL <- "#D55E00"; CERT <- "#E69F00"; GREY <- "grey45"

# --- Band 1: the two conditions that make Sweden a most-likely case ----------
setup <- tibble(
  cond  = factor(c("Interpersonal trust", "Public biodiversity spending"),
                 levels = c("Interpersonal trust", "Public biodiversity spending")),
  value = c(0.95, 0.12),                      # Sweden's position, 0-1 within Europe
  lab   = c("5th of 91 worldwide", "among Europe's lowest")
)

p1 <- ggplot(setup, aes(x = value, y = fct_rev(cond))) +
  geom_segment(aes(x = 0, xend = 1, y = fct_rev(cond), yend = fct_rev(cond)),
               colour = "grey88", linewidth = 4, lineend = "round") +
  geom_point(size = 6, colour = MAND) +
  geom_text(aes(label = lab), hjust = ifelse(setup$value > 0.5, 1.15, -0.15),
            size = 3.1, colour = GREY) +
  scale_x_continuous(limits = c(-0.05, 1.05)) +
  labs(title = "Where voluntary biodiversity finance should work best",
       subtitle = "Sweden: others can be expected to contribute,\nand the state is not already paying") +
  theme_void(base_size = 11) +
  theme(axis.text.y = element_text(hjust = 1, size = 9.5, colour = "grey20"),
        plot.title = element_text(face = "bold", size = 11.5),
        plot.subtitle = element_text(size = 9, colour = GREY),
        plot.margin = margin(6, 10, 10, 6))

# --- Band 2: the preference ordering -----------------------------------------
pref <- tibble(
  instrument = c("Mandatory\noffsetting", "Tax", "Certification", "Voluntary\ndonation"),
  utility    = c(0.146, 0, -0.082, -0.376),
  type       = c("Mandatory", "Mandatory", "Voluntary", "Voluntary")
) |>
  mutate(instrument = fct_reorder(instrument, utility))

p2 <- ggplot(pref, aes(x = utility, y = instrument, fill = type)) +
  geom_vline(xintercept = 0, colour = "grey75", linetype = "dashed") +
  geom_col(width = 0.62) +
  scale_fill_manual(values = c(Mandatory = MAND, Voluntary = VOL), guide = "none") +
  annotate("text", x = 0.02, y = 4.42, label = "preferred to tax",
           colour = MAND, size = 3, fontface = "bold", hjust = 0) +
  annotate("text", x = -0.02, y = 1.58, label = "ranked last",
           colour = VOL, size = 3, fontface = "bold", hjust = 1) +
  scale_x_continuous(limits = c(-0.46, 0.26)) +
  labs(title = "Citizens choose mandatory instruments anyway",
       subtitle = "n = 2,101; preference relative to a tax-financed programme",
       x = NULL, y = NULL) +
  theme_minimal(base_size = 11) +
  theme(panel.grid = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 9, colour = "grey20"),
        plot.title = element_text(face = "bold", size = 11.5),
        plot.subtitle = element_text(size = 9, colour = GREY),
        plot.margin = margin(6, 10, 6, 6))

# --- Band 3: the two judgements about the state ------------------------------
mech <- tibble(
  judgement = c("Doubt the state\nwill deliver", "Distrust the state\nwith money"),
  effect    = c(-0.13, 0.26),
  outcome   = c("more mandatory", "more voluntary")
)

p3 <- ggplot(mech, aes(x = effect, y = judgement, fill = effect > 0)) +
  geom_vline(xintercept = 0, colour = "grey55") +
  geom_col(width = 0.5) +
  geom_text(aes(label = outcome, hjust = ifelse(mech$effect > 0, -0.1, 1.1)),
            size = 3, colour = GREY) +
  scale_fill_manual(values = c(`TRUE` = VOL, `FALSE` = MAND), guide = "none") +
  scale_x_continuous(limits = c(-0.52, 0.62)) +
  labs(title = "Underprovision and distrust are not the same thing",
       subtitle = "They move preferences in opposite directions",
       x = NULL, y = NULL) +
  theme_minimal(base_size = 11) +
  theme(panel.grid = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 9, colour = "grey20"),
        plot.title = element_text(face = "bold", size = 11.5),
        plot.subtitle = element_text(size = 9, colour = GREY),
        plot.margin = margin(6, 10, 6, 6))

ga <- p1 / p2 / p3 + plot_layout(heights = c(0.85, 1.3, 0.9))

ggsave(here::here("paper", "graphical_abstract.png"), ga,
       width = 6.3, height = 6.4, dpi = 300, bg = "white")
ggsave(here::here("paper", "graphical_abstract.pdf"), ga,
       width = 6.3, height = 6.4, device = pdf, bg = "white")
cat("Saved paper/graphical_abstract.png and .pdf\n")
