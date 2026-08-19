# ==============================================================================
# Figure 1 — Sweden as a most-likely case for VOLUNTARY biodiversity financing.
#
# The most-likely-case logic needs two conditions to hold jointly:
#
#   (A) ENABLING condition — citizens must believe others will also contribute,
#       otherwise voluntary provision fails on free-riding grounds alone.
#       Measured by interpersonal trust.
#
#   (B) DEMAND condition — there must be an unmet need for voluntary money to
#       fill. If the state already funds biodiversity adequately, voluntary
#       instruments are redundant rather than unattractive.
#       Measured by public spending on biodiversity protection.
#
# Sweden is close to the only country in Europe that sits in the top tercile on
# (A) and the bottom tercile on (B). Voluntary instruments should therefore look
# maximally attractive here. They do not — which is the paper's result.
#
# Panel A: European bivariate choropleth, trust x biodiversity spending.
# Panel B: global interpersonal trust ranking, so the worldwide framing survives
#          the fact that comparable spending data exist only for Europe.
#
# Data are fetched and cached by analysis/fetch_bestcase_data.R. Do NOT hand-edit
# values here — an earlier version of this script carried hand-entered
# approximations that were materially wrong.
# ==============================================================================

library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(biscale)
library(cowplot)
library(magick)
library(here)

dat <- read_csv(here("analysis", "bestcase_data.csv"), show_col_types = FALSE)

# ------------------------------------------------------------------------------
# Bivariate classification (Europe only — spending exists only for EU/EEA)
# ------------------------------------------------------------------------------

bi_dat <- dat |>
  filter(!is.na(trust_interp), !is.na(spend_pc_usd))

message("Bivariate panel: ", nrow(bi_dat), " countries with both series")

# ------------------------------------------------------------------------------
# Map geometry, projected to LAEA Europe (EPSG:3035), the standard for EU maps
# ------------------------------------------------------------------------------

world <- ne_countries(scale = "medium", returnclass = "sf")

europe <- world |>
  left_join(bi_dat, by = c("iso_a3_eh" = "iso3")) |>
  st_transform(crs = 3035)

# bi_class() drops rows with NA on either axis, so classify the joined frame and
# let countries without data fall through to na.value in the fill scale.
europe <- bi_class(europe, x = trust_interp, y = spend_pc_usd,
                   style = "quantile", dim = 3)

# ------------------------------------------------------------------------------
# Palette — Joshua Stevens purple x cyan
#   x (rightward) = rising interpersonal trust      -> cyan
#   y (upward)    = rising public biodiversity spend -> purple
# Sweden's cell is "3-1": high trust, low spending — the strong cyan corner.
# ------------------------------------------------------------------------------

custom_pal <- c(
  "1-1" = "#c8c8c8", "2-1" = "#ace4e4", "3-1" = "#5ac8c8",
  "1-2" = "#dfb0d6", "2-2" = "#a5add3", "3-2" = "#5698b9",
  "1-3" = "#be64ac", "2-3" = "#8c62aa", "3-3" = "#3b4994"
)

swe_poly <- europe |> filter(iso_a3_eh == "SWE")
rest     <- europe |> filter(iso_a3_eh != "SWE" | is.na(iso_a3_eh))

p_map <- ggplot() +
  geom_sf(data = rest, aes(fill = bi_class),
          colour = "white", linewidth = 0.15, show.legend = FALSE) +
  geom_sf(data = swe_poly, aes(fill = bi_class),
          colour = "#cc0000", linewidth = 0.9, show.legend = FALSE) +
  bi_scale_fill(pal = custom_pal, dim = 3, na.value = "#eeeeee") +
  # Extent clipped to continental Europe + Nordics (metres, EPSG:3035)
  coord_sf(xlim = c(2400000, 6000000), ylim = c(1400000, 5400000),
           expand = FALSE) +
  theme_void(base_size = 10) +
  theme(plot.background = element_rect(fill = "white", colour = NA),
        plot.margin = margin(0, 0, 0, 0))

p_legend <- bi_legend(
  pal  = custom_pal, dim = 3,
  xlab = "Interpersonal trust",
  ylab = "Biodiversity spending",
  size = 9, arrows = FALSE
)

# ------------------------------------------------------------------------------
# Panel B — global interpersonal trust ranking
#
# Spending data are European-only, so the global reach of the most-likely-case
# claim rests on this panel: Sweden is 5th of 91 countries worldwide on the
# mechanism most directly relevant to voluntary collective action.
# ------------------------------------------------------------------------------

trust_global <- dat |> filter(!is.na(trust_interp))
swe_rank  <- sum(trust_global$trust_interp > trust_global$trust_interp[trust_global$iso3 == "SWE"]) + 1
n_global  <- nrow(trust_global)

top_n_show <- 20
bar_df <- trust_global |>
  slice_max(trust_interp, n = top_n_show) |>
  mutate(country = fct_reorder(country, trust_interp),
         is_swe  = iso3 == "SWE")

p_bar <- ggplot(bar_df, aes(x = trust_interp, y = country, fill = is_swe)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = sprintf("%.0f", trust_interp)),
            hjust = -0.25, size = 3.1, colour = "grey30") +
  scale_fill_manual(values = c(`TRUE` = "#cc0000", `FALSE` = "#5ac8c8"),
                    guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.13))) +
  labs(x = "\"Most people can be trusted\" (%)", y = NULL,
       subtitle = sprintf("Sweden ranks %dth of %d worldwide",
                          swe_rank, n_global)) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.subtitle      = element_text(size = 10, colour = "grey30"),
    axis.text.y        = element_text(
      face = ifelse(levels(bar_df$country) == "Sweden", "bold", "plain")),
    plot.background    = element_rect(fill = "white", colour = NA),
    plot.margin        = margin(26, 18, 4, 4)
  )

# ------------------------------------------------------------------------------
# Composite: rasterise the map so the bivariate legend can be overlaid, then
# place the two panels side by side.
# ------------------------------------------------------------------------------

tmp_map <- tempfile(fileext = ".png")
ggsave(tmp_map, p_map, width = 5.2, height = 6.4, dpi = 300, bg = "white")
map_img <- image_read(tmp_map) |> image_trim(fuzz = 0)
map_h   <- image_info(map_img)$height

tmp_leg <- tempfile(fileext = ".png")
ggsave(tmp_leg, p_legend, width = 1.7, height = 1.7, dpi = 300, bg = "white")
leg_img <- image_read(tmp_leg) |> image_trim(fuzz = 1)
leg_h   <- image_info(leg_img)$height

map_img <- image_composite(map_img, leg_img,
                           offset = paste0("+20+", map_h - leg_h - 20))

map_panel <- ggdraw(image_ggplot(map_img, interpolate = TRUE))

fig_bestcase <- plot_grid(
  map_panel, p_bar,
  ncol       = 2,
  rel_widths = c(1.15, 1),
  labels         = c("A  Trust and biodiversity spending, Europe",
                     "B  Global interpersonal trust"),
  label_size     = 11,
  label_fontface = "bold",
  label_x        = c(0.02, 0),
  label_y        = 1.0,
  hjust = 0, vjust = 1
)

ggsave(here("paper", "fig_bestcase.pdf"), fig_bestcase,
       width = 7.6, height = 5.4, device = pdf, bg = "white")
ggsave(here("paper", "fig_bestcase.png"), fig_bestcase,
       width = 7.6, height = 5.4, dpi = 300, bg = "white")

# ------------------------------------------------------------------------------
# Report the numbers quoted in the manuscript so they stay in sync with the data
# ------------------------------------------------------------------------------

swe <- bi_dat |> filter(iso3 == "SWE")
cat("\n--- Figure 1 key values ---\n")
cat(sprintf("Sweden interpersonal trust : %.1f%% (rank %d of %d worldwide)\n",
            swe$trust_interp, swe_rank, n_global))
cat(sprintf("Sweden biodiversity spend  : USD %.0f/capita (rank %d of %d in Europe)\n",
            swe$spend_pc_usd,
            sum(bi_dat$spend_pc_usd > swe$spend_pc_usd) + 1, nrow(bi_dat)))
cat(sprintf("Sweden bivariate cell      : %s (3-1 = high trust, low spending)\n",
            europe$bi_class[europe$iso_a3_eh == "SWE"][1]))
cat("\nOther countries in Sweden's 3-1 cell:\n")
print(europe |> st_drop_geometry() |>
        filter(bi_class == "3-1", iso_a3_eh != "SWE") |>
        transmute(country, trust = round(trust_interp, 1),
                  spend_usd = round(spend_pc_usd)) |> distinct())
cat("\nSaved paper/fig_bestcase.pdf and .png\n")
