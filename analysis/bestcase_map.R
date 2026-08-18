library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(biscale)
library(cowplot)
library(ggrepel)
library(magick)
library(here)

# ─── Data ────────────────────────────────────────────────────────────────────
# WVS Wave 7 (2017–2022) country-level aggregates
# Interpersonal trust: Q57 "Most people can be trusted" (% agree)
# Institutional trust: Q71 "Confidence in the government" (% a great deal + quite a lot)
# Source: WVS Wave 7 online data analysis tool (worldvaluessurvey.org)
# ASSUMPTION: values below are from WVS Wave 7 published country aggregates —
#             verify against the official WVS data tool before publication.

trust_data <- tribble(
  ~iso3,  ~country,              ~trust_interp, ~trust_inst,
  "SWE",  "Sweden",              60,            68,
  "NOR",  "Norway",              73,            74,
  "FIN",  "Finland",             66,            72,
  "DNK",  "Denmark",             74,            69,
  "NLD",  "Netherlands",         66,            57,
  "CHE",  "Switzerland",         56,            55,
  "AUS",  "Australia",           48,            54,
  "NZL",  "New Zealand",         54,            58,
  "CAN",  "Canada",              53,            48,
  "DEU",  "Germany",             45,            42,
  "JPN",  "Japan",               39,            38,
  "KOR",  "South Korea",         43,            44,
  "USA",  "United States",       38,            38,
  "GBR",  "United Kingdom",      43,            37,
  "ESP",  "Spain",               27,            25,
  "FRA",  "France",              24,            30,
  "ITA",  "Italy",               27,            28,
  "POL",  "Poland",              22,            44,
  "AUT",  "Austria",             38,            46,
  "BEL",  "Belgium",             35,            31,
  "PRT",  "Portugal",            19,            27,
  "GRC",  "Greece",               9,            23,
  "ROU",  "Romania",             16,            31,
  "RUS",  "Russia",              32,            32,
  "UKR",  "Ukraine",             22,            28,
  "TUR",  "Turkey",              12,            58,
  "CHN",  "China",               62,            95,
  "IDN",  "Indonesia",           14,            79,
  "IND",  "India",               20,            77,
  "BGD",  "Bangladesh",          25,            66,
  "PAK",  "Pakistan",            22,            76,
  "BRA",  "Brazil",               7,            31,
  "MEX",  "Mexico",              15,            39,
  "ARG",  "Argentina",           17,            24,
  "COL",  "Colombia",            12,            38,
  "CHL",  "Chile",               20,            25,
  "PER",  "Peru",                11,            27,
  "NGA",  "Nigeria",             14,            62,
  "ETH",  "Ethiopia",            22,            79,
  "ZWE",  "Zimbabwe",             8,            53,
  "KEN",  "Kenya",               10,            66,
  "ZAF",  "South Africa",        23,            35
)

# Conservation spending: per-capita public expenditure on environmental protection (USD/year)
# Source: OECD National Accounts / Eurostat Government Finance Statistics
# ASSUMPTION: values below are approximate; verify before publication.

spending_data <- tribble(
  ~iso3,  ~country,        ~spend_pc_usd,
  "SWE",  "Sweden",        520,
  "NOR",  "Norway",        680,
  "FIN",  "Finland",       350,
  "DNK",  "Denmark",       430,
  "NLD",  "Netherlands",   310,
  "CHE",  "Switzerland",   460,
  "AUS",  "Australia",     280,
  "CAN",  "Canada",        240
)

# ─── Merge with map ───────────────────────────────────────────────────────────
world <- ne_countries(scale = "medium", returnclass = "sf") |>
  st_transform(crs = "+proj=robin")

df <- trust_data |>
  left_join(spending_data |> select(iso3, spend_pc_usd), by = "iso3")

world_df <- world |>
  left_join(df, by = c("iso_a3_eh" = "iso3"))

# ─── Bivariate classification ─────────────────────────────────────────────────
world_df <- bi_class(world_df,
                     x = trust_interp,
                     y = trust_inst,
                     style = "quantile",
                     dim = 3)

# ─── Country label centroids ─────────────────────────────────────────────────
label_countries <- c("SWE", "NOR", "DNK", "FIN", "NLD", "CHE", "CAN", "AUS")

centroids <- world_df |>
  filter(iso_a3_eh %in% label_countries) |>
  group_by(iso_a3_eh) |>
  slice(1) |>
  ungroup() |>
  st_point_on_surface() |>
  mutate(
    lon = st_coordinates(geometry)[, 1],
    lat = st_coordinates(geometry)[, 2]
  ) |>
  st_drop_geometry() |>
  mutate(
    lon = case_when(iso_a3_eh == "AUS" ~ 12200000, TRUE ~ lon),
    lat = case_when(iso_a3_eh == "AUS" ~ -2800000, TRUE ~ lat)
  )

# ─── Bivariate palette ────────────────────────────────────────────────────────
# Joshua Stevens purple × cyan scheme
# Rows (y = institutional trust): bottom light → top purple
# Cols (x = interpersonal trust): left light → right cyan
# High-high corner: dark blue-purple (#3b4994)
# "1-1" = darker grey (countries with data, low on both dimensions)
# na.value = lighter grey (no WVS Wave 7 data)
custom_pal <- c(
  "1-1" = "#c8c8c8",
  "2-1" = "#ace4e4",
  "3-1" = "#5ac8c8",
  "1-2" = "#dfb0d6",
  "2-2" = "#a5add3",
  "3-2" = "#5698b9",
  "1-3" = "#be64ac",
  "2-3" = "#8c62aa",
  "3-3" = "#3b4994"
)

# ─── Map: Sweden highlighted with red border instead of circle ────────────────
# Split world_df into Sweden and rest for separate border rendering
world_swe  <- world_df |> filter(iso_a3_eh == "SWE")
world_rest <- world_df |> filter(iso_a3_eh != "SWE")

swe_centroid <- filter(centroids, iso_a3_eh == "SWE")

p_map <- ggplot() +
  geom_sf(data = world_rest,
          aes(fill = bi_class),
          colour = "white", linewidth = 0.12, show.legend = FALSE) +
  geom_sf(data = world_swe,
          aes(fill = bi_class),
          colour = "#cc0000", linewidth = 0.5, show.legend = FALSE) +
  bi_scale_fill(pal = custom_pal, dim = 3, na.value = "#e8e8e8") +
  annotate("text", x = swe_centroid$lon + 500000,
           y = swe_centroid$lat - 550000,
           label = "Sweden", fontface = "bold", size = 3, hjust = 0,
           colour = "grey10") +
  coord_sf(xlim = c(-12500000, 16500000),
           ylim = c(-6200000, 8400000), expand = FALSE) +
  theme_void(base_size = 10) +
  theme(
    plot.background = element_rect(fill = "white", colour = NA),
    plot.margin     = margin(0, 0, 0, 0)
  )

# ─── Bivariate legend ─────────────────────────────────────────────────────────
p_legend <- bi_legend(
  pal    = custom_pal,
  dim    = 3,
  xlab   = "Interpersonal trust",
  ylab   = "Institutional trust",
  size   = 9,
  arrows = FALSE
)

# ─── Bar chart: colour bars by bivariate class of each country ────────────────
# Look up each bar country's bi_class from world_df, map to palette colour
bar_biclass <- world_df |>
  st_drop_geometry() |>
  filter(iso_a3_eh %in% spending_data$iso3) |>
  select(iso_a3_eh, bi_class) |>
  distinct()

bar_df <- spending_data |>
  left_join(bar_biclass, by = c("iso3" = "iso_a3_eh")) |>
  arrange(spend_pc_usd) |>
  mutate(
    country     = factor(country, levels = country),
    bar_col     = custom_pal[bi_class],
    swe         = iso3 == "SWE",
    outline_col = if_else(swe, "#cc0000", NA_character_),
    outline_lwd = if_else(swe, 0.8, 0)
  )

p_bar <- ggplot(bar_df, aes(x = spend_pc_usd, y = country)) +
  geom_col(aes(fill = bar_col, colour = outline_col, linewidth = outline_lwd),
           width = 0.65) +
  geom_text(aes(label = spend_pc_usd), hjust = -0.2, size = 3, colour = "grey30") +
  scale_fill_identity() +
  scale_colour_identity() +
  scale_linewidth_identity() +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15)),
                     labels = scales::label_number()) +
  labs(x = "Public expenditure on environmental protection (USD per capita per year)", y = NULL,
       title = "B  Per-capita conservation spending") +
  theme_minimal(base_size = 10) +
  theme(
    panel.grid.minor    = element_blank(),
    panel.grid.major.y  = element_blank(),
    plot.title          = element_text(face = "bold", size = 12, hjust = 0),
    plot.title.position = "plot",
    axis.text.y         = element_text(
      face = ifelse(levels(bar_df$country) == "Sweden", "bold", "plain")
    ),
    plot.background     = element_rect(fill = "white", colour = NA),
    plot.margin         = margin(4, 10, 4, 45)
  )

# ─── Render map to raster, composite legend (trimmed), add title ──────────────
tmp_map <- tempfile(fileext = ".png")
ggsave(tmp_map, p_map, width = 9, height = 4.2, dpi = 300, bg = "white")
map_img  <- image_read(tmp_map) |> image_trim(fuzz = 0)
map_w    <- image_info(map_img)$width
map_h    <- image_info(map_img)$height

tmp_leg <- tempfile(fileext = ".png")
ggsave(tmp_leg, p_legend, width = 1.6, height = 1.6, dpi = 300, bg = "white")
leg_img <- image_read(tmp_leg) |> image_trim(fuzz = 1)
leg_h   <- image_info(leg_img)$height

map_img <- image_composite(map_img, leg_img,
                            offset = paste0("+40+", map_h - leg_h - 40))

map_raster <- image_ggplot(map_img, interpolate = TRUE)

map_panel <- ggdraw(map_raster)

# ─── Combine ──────────────────────────────────────────────────────────────────
# label_x = 257/2700 ≈ 0.095 is where panel B title starts (measured)
# panel A spans full width; label placed at that same fraction
fig_bestcase <- plot_grid(
  map_panel,
  p_bar,
  ncol        = 1,
  rel_heights = c(1.5, 1),
  align       = "v",
  axis        = "lr",
  labels         = c("A  Interpersonal × institutional trust", ""),
  label_size     = 12,
  label_fontface = "bold",
  label_x        = 190/2700,
  label_y        = 1.0,
  hjust          = 0,
  vjust          = 1
)

ggsave(here("paper", "fig_bestcase.pdf"),
       fig_bestcase, width = 9, height = 7.5, device = pdf,
       bg = "white")
ggsave(here("paper", "fig_bestcase.png"),
       fig_bestcase, width = 9, height = 7.5, dpi = 300,
       bg = "white")

cat("Saved fig_bestcase.pdf and .png\n")
