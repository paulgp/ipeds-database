## ---------------------------------------------------------------
## Three figures: Bachelor's, MBA, and PhD degrees conferred
## in the United States, 2000–2024
##
## Kieran Healy style: minimal, high data-ink, clean sans-serif,
## direct annotation, y-axis at zero for counts.
## ---------------------------------------------------------------

library(ggplot2)
library(duckdb)
library(scales)

base_family <- "Helvetica Neue"

# ── Data ───────────────────────────────────────────────────────
con <- dbConnect(duckdb(), dbdir = "ipeds.duckdb", read_only = TRUE)

# 1) Bachelor's degrees (award_level = 5), all CIP codes
ba <- dbGetQuery(con, "
  SELECT year,
         COUNT(DISTINCT unitid) AS num_schools,
         SUM(
           COALESCE(
             CAST(ctotalt  AS BIGINT),
             CAST(crace24  AS BIGINT),
             CAST(crace15  AS BIGINT) + CAST(crace16 AS BIGINT)
           )
         ) AS degrees
  FROM   c_a
  WHERE  award_level = 5
  GROUP  BY year
  ORDER  BY year
")

# 2) MBA (CIP 52.0201, award_level = 7)
mba <- dbGetQuery(con, "
  SELECT year,
         COUNT(DISTINCT unitid) AS num_schools,
         SUM(
           COALESCE(
             CAST(ctotalt  AS BIGINT),
             CAST(crace24  AS BIGINT),
             CAST(crace15  AS BIGINT) + CAST(crace16 AS BIGINT)
           )
         ) AS degrees
  FROM   c_a
  WHERE  CAST(cipcode AS VARCHAR) = '52.0201'
    AND  award_level = 7
  GROUP  BY year
  ORDER  BY year
")

# 3) Research doctorates (PhD)
#    2000-2009: award_level 9 + 17 (transition; no institution overlap)
#    2010+:     award_level 17 only
phd <- dbGetQuery(con, "
  SELECT year,
         COUNT(DISTINCT unitid) AS num_schools,
         SUM(
           COALESCE(
             CAST(ctotalt  AS BIGINT),
             CAST(crace24  AS BIGINT),
             CAST(crace15  AS BIGINT) + CAST(crace16 AS BIGINT)
           )
         ) AS degrees
  FROM   c_a
  WHERE  (award_level = 9 OR award_level = 17)
  GROUP  BY year
  ORDER  BY year
")

dbDisconnect(con, shutdown = TRUE)

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
      plot.title     = element_text(size = rel(1.35), face = "bold",
                                    colour = "grey10", hjust = 0,
                                    margin = margin(b = 3)),
      plot.subtitle  = element_text(size = rel(0.9), colour = "grey40",
                                    hjust = 0, margin = margin(b = 14)),
      plot.caption   = element_text(size = rel(0.7), colour = "grey55",
                                    hjust = 1, margin = margin(t = 12)),
      plot.margin = margin(18, 20, 12, 12)
    )
}

# ── Helper: build one plot ─────────────────────────────────────
make_plot <- function(df, line_col, title, subtitle, y_div = 1000,
                      y_suffix = "K", caption_note = "") {

  df$y <- df$degrees / y_div
  peak   <- df[which.max(df$degrees), ]
  latest <- df[nrow(df), ]
  first  <- df[1, ]

  # Nudge annotations away from data
  y_range  <- max(df$y) - min(df$y)
  ann_up   <- y_range * 0.09
  seg_gap  <- y_range * 0.025

  accent_col <- "#c44e52"
  peak_is_latest <- (peak$year == latest$year)

  p <- ggplot(df, aes(x = year, y = y)) +
    geom_area(fill = line_col, alpha = 0.10) +
    geom_line(colour = line_col, linewidth = 0.9) +
    geom_point(colour = line_col, size = 1.6) +

    # Peak
    annotate("point", x = peak$year, y = peak$degrees / y_div,
             colour = accent_col, size = 3.2) +
    annotate("text",  x = peak$year,
             y = peak$degrees / y_div + ann_up,
             label = paste0(comma(peak$degrees), " (", peak$year, ")"),
             size = 3.3, colour = "grey25", family = base_family,
             fontface = "bold",
             hjust = if (peak$year >= max(df$year) - 1) 1 else 0.5) +
    annotate("segment", x = peak$year, xend = peak$year,
             y = peak$degrees / y_div + seg_gap,
             yend = peak$degrees / y_div + ann_up - seg_gap,
             colour = "grey50", linewidth = 0.3) +

    # Start label (nudge right so it doesn't clip)
    annotate("text", x = first$year + 0.3, y = first$degrees / y_div - ann_up * 0.8,
             label = comma(first$degrees),
             size = 3.1, colour = "grey40", family = base_family, hjust = 0)

  # End label — only if peak is not the latest year (avoid overlap)
  if (!peak_is_latest) {
    p <- p +
      annotate("text", x = latest$year - 0.3,
               y = latest$degrees / y_div - ann_up * 0.8,
               label = comma(latest$degrees),
               size = 3.1, colour = "grey40", family = base_family, hjust = 1)
  }

  p <- p +
    scale_x_continuous(breaks = seq(2000, 2024, 4),
                       expand = expansion(mult = c(0.02, 0.05))) +
    scale_y_continuous(
      labels = function(x) paste0(format(x, big.mark = ","), y_suffix),
      limits = c(0, NA),
      expand = expansion(mult = c(0, 0.12))
    ) +
    labs(
      title    = title,
      subtitle = subtitle,
      x        = NULL,
      y        = NULL,
      caption  = paste0("Source: IPEDS Completions Survey, National Center ",
                        "for Education Statistics.", caption_note)
    ) +
    theme_healy()

  p
}

# ── Figure 1: Bachelor's ──────────────────────────────────────
p1 <- make_plot(
  ba,
  line_col  = "#4e79a7",
  title     = "Bachelor\u2019s Degrees Conferred in the United States",
  subtitle  = "Total four-year bachelor\u2019s degrees across all fields (award level 5), by academic year",
  y_div     = 1000,
  y_suffix  = "K",
  caption_note = "  2024 data are provisional."
)

# ── Figure 2: MBA ─────────────────────────────────────────────
p2 <- make_plot(
  mba,
  line_col  = "#59a14f",
  title     = "MBA Degrees Conferred in the United States",
  subtitle  = "Master\u2019s degrees in Business Administration & Management (CIP 52.0201), by academic year",
  y_div     = 1000,
  y_suffix  = "K",
  caption_note = "  2024 data are provisional."
)

# ── Figure 3: PhD ─────────────────────────────────────────────
p3 <- make_plot(
  phd,
  line_col  = "#b07aa1",
  title     = "Research Doctorates Conferred in the United States",
  subtitle  = "Doctor\u2019s degrees \u2014 research/scholarship (award levels 9 & 17), by academic year",
  y_div     = 1000,
  y_suffix  = "K",
  caption_note = "\nAward-level coding changed in 2008\u201310; series spliced across transition.  2024 data are provisional."
)

# ── Save ───────────────────────────────────────────────────────
ggsave("ba_degrees.png",  p1, width = 8, height = 5, dpi = 300,
       bg = "white", device = ragg::agg_png)
ggsave("mba_degrees.png", p2, width = 8, height = 5, dpi = 300,
       bg = "white", device = ragg::agg_png)
ggsave("phd_degrees.png", p3, width = 8, height = 5, dpi = 300,
       bg = "white", device = ragg::agg_png)

message("Saved \u2192 ba_degrees.png, mba_degrees.png, phd_degrees.png")
