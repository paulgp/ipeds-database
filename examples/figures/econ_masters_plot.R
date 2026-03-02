## ---------------------------------------------------------------
## Terminal Master's in Economics, 2000–2024
##
## The big story: "Economics, General" is collapsing while
## "Econometrics & Quantitative Economics" explodes — partly
## reclassification, partly real growth (total doubles).
## ---------------------------------------------------------------

library(ggplot2)
library(duckdb)
library(scales)

base_family <- "Helvetica Neue"

# ── Data ───────────────────────────────────────────────────────
con <- dbConnect(duckdb(), dbdir = "ipeds.duckdb", read_only = TRUE)

query <- function(label, cips) {
  cip_sql <- paste0("'", cips, "'", collapse = ", ")
  sql <- sprintf("
    SELECT year,
           SUM(
             COALESCE(
               CAST(ctotalt  AS BIGINT),
               CAST(crace24  AS BIGINT),
               CAST(crace15  AS BIGINT) + CAST(crace16 AS BIGINT)
             )
           ) AS degrees
    FROM   c_a
    WHERE  CAST(cipcode AS VARCHAR) IN (%s)
      AND  award_level = 7
    GROUP  BY year
    ORDER  BY year
  ", cip_sql)
  d <- dbGetQuery(con, sql)
  d$program <- label
  d
}

df <- rbind(
  query("Economics, General",             "45.0601"),
  query("Econometrics & Quant Econ",      "45.0603"),
  query("Applied Economics",              "45.0602"),
  query("Development Economics",          "45.0604"),
  query("International Economics",        "45.0605"),
  query("Economics, Other",               "45.0699")
)

# Also compute total across all 45.06xx
total_df <- dbGetQuery(con, "
  SELECT year,
         SUM(
           COALESCE(
             CAST(ctotalt  AS BIGINT),
             CAST(crace24  AS BIGINT),
             CAST(crace15  AS BIGINT) + CAST(crace16 AS BIGINT)
           )
         ) AS degrees
  FROM   c_a
  WHERE  CAST(cipcode AS VARCHAR) LIKE '45.06%'
    AND  award_level = 7
  GROUP  BY year
  ORDER  BY year
")
total_df$program <- "All Economics (total)"

dbDisconnect(con, shutdown = TRUE)

# ── Separate the main series and the total ─────────────────────
# Plot individual CIPs as coloured lines, total as dashed grey

# Order by 2024 value
latest <- df[df$year == max(df$year), ]
latest <- latest[order(-latest$degrees), ]

# Focus on the two big movers + total; group the small ones
big <- c("Econometrics & Quant Econ", "Economics, General")
small_cips <- setdiff(unique(df$program), big)

small_agg <- aggregate(degrees ~ year, df[df$program %in% small_cips, ], sum)
small_agg$program <- "Other Econ specialities"

plot_df <- rbind(
  df[df$program %in% big, ],
  small_agg,
  total_df
)

plot_df$program <- factor(plot_df$program, levels = c(
  "All Economics (total)",
  "Econometrics & Quant Econ",
  "Economics, General",
  "Other Econ specialities"
))

# ── Colours ────────────────────────────────────────────────────
cols <- c(
  "All Economics (total)"       = "grey45",
  "Econometrics & Quant Econ"   = "#e15759",
  "Economics, General"          = "#4e79a7",
  "Other Econ specialities"     = "#76b7b2"
)

linetypes <- c(
  "All Economics (total)"       = "dashed",
  "Econometrics & Quant Econ"   = "solid",
  "Economics, General"          = "solid",
  "Other Econ specialities"     = "solid"
)

linewidths <- c(
  "All Economics (total)"       = 0.9,
  "Econometrics & Quant Econ"   = 0.9,
  "Economics, General"          = 0.9,
  "Other Econ specialities"     = 0.7
)

# ── Theme ──────────────────────────────────────────────────────
theme_healy <- function(base_size = 13, base_family = "Helvetica Neue") {
  theme_minimal(base_size = base_size, base_family = base_family) %+replace%
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      panel.grid.major.y = element_line(colour = "grey88", linewidth = 0.3),
      axis.line.x    = element_line(colour = "grey40", linewidth = 0.35),
      axis.ticks.x   = element_line(colour = "grey40", linewidth = 0.3),
      axis.ticks.length = unit(4, "pt"),
      axis.title     = element_text(size = rel(0.9), colour = "grey30"),
      axis.text      = element_text(size = rel(0.85), colour = "grey40"),
      plot.title     = element_text(size = rel(1.3), face = "bold",
                                    colour = "grey10", hjust = 0,
                                    margin = margin(b = 3)),
      plot.subtitle  = element_text(size = rel(0.9), colour = "grey40",
                                    hjust = 0, margin = margin(b = 14)),
      plot.caption   = element_text(size = rel(0.7), colour = "grey55",
                                    hjust = 1, margin = margin(t = 12)),
      plot.margin  = margin(18, 20, 12, 12),
      legend.position = "none"
    )
}

# ── Labels at right end ────────────────────────────────────────
end <- plot_df[plot_df$year == max(plot_df$year), ]
end$label <- paste0(end$program, "  ", comma(end$degrees))

# Nudge to avoid overlap — sort top to bottom, enforce gap
end <- end[order(-end$degrees), ]
min_gap <- 280
for (i in 2:nrow(end)) {
  if (end$degrees[i] > end$degrees[i - 1] - min_gap) {
    end$degrees[i] <- end$degrees[i - 1] - min_gap
  }
}
end$degrees <- pmax(end$degrees, 80)
end$y_lab <- end$degrees

# ── Annotation: crossover year ─────────────────────────────────
econ_gen <- plot_df[plot_df$program == "Economics, General", ]
econmtrc <- plot_df[plot_df$program == "Econometrics & Quant Econ", ]
# They cross around 2019
cross_year <- 2019

# ── Plot ───────────────────────────────────────────────────────
p <- ggplot(plot_df, aes(x = year, y = degrees, colour = program,
                          linetype = program, linewidth = program)) +
  geom_line() +
  geom_point(data = plot_df[plot_df$program != "All Economics (total)", ],
             size = 1.1) +

  # Crossover annotation
  annotate("segment", x = cross_year, xend = cross_year,
           y = 0, yend = 2300,
           colour = "grey70", linewidth = 0.3, linetype = "dotted") +
  annotate("text", x = cross_year, y = 2450,
           label = "Crossover\n2019", size = 2.8,
           colour = "grey50", family = base_family,
           fontface = "italic", lineheight = 0.85) +

  # Direct labels
  geom_text(
    data = end,
    aes(x = max(plot_df$year) + 0.5, y = y_lab, label = label,
        colour = program),
    size = 3.0, family = base_family, fontface = "bold",
    hjust = 0, show.legend = FALSE, inherit.aes = FALSE
  ) +

  scale_colour_manual(values = cols) +
  scale_linetype_manual(values = linetypes) +
  scale_linewidth_manual(values = linewidths) +

  scale_x_continuous(
    breaks = seq(2000, 2024, 4),
    expand = expansion(mult = c(0.02, 0.35))
  ) +
  scale_y_continuous(
    labels = comma,
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.08))
  ) +

  labs(
    title    = "The Reinvention of the Economics Master\u2019s",
    subtitle = "Master\u2019s degrees by CIP subfield, 2000\u20132024. Programs reclassified from\n\u201cEconomics, General\u201d to \u201cEconometrics & Quantitative Economics\u201d as curricula shifted.",
    x        = NULL,
    y        = "Degrees conferred",
    caption  = paste0(
      "Source: IPEDS Completions Survey (award level 7), NCES.  2024 data are provisional.\n",
      "Other specialities = Applied (45.0602), Development (45.0604), International (45.0605), Other (45.0699).\n",
      "178 institutions reported under 45.0601 in 2000; by 2024 only 47 remained, while 171 reported under 45.0603."
    )
  ) +

  theme_healy()

# ── Save ───────────────────────────────────────────────────────
ggsave("econ_masters.png", p, width = 10, height = 6.5, dpi = 300,
       bg = "white", device = ragg::agg_png)

message("Saved \u2192 econ_masters.png")
