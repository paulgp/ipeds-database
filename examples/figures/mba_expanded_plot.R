## ---------------------------------------------------------------
## Business Master's Degrees: MBA and Competing Programs
## 2000–2024
##
## Kieran Healy style: clean, minimal, direct labels, no legend box
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
  query("MBA",                        "52.0201"),
  query("Mgmt Science &\nAnalytics",  c("52.1301", "52.1399", "52.1302")),
  query("Accounting",                  "52.0301"),
  query("Data Science &\nInfo Systems", c("52.1201", "11.0401", "27.0501", "30.7001")),
  query("Finance",                     "52.0801"),
  query("Marketing",                   "52.1401")
)

dbDisconnect(con, shutdown = TRUE)

df$degrees_k <- df$degrees / 1000

# Order factor by 2024 value (largest on top in legend-free direct labels)
latest <- df[df$year == max(df$year), ]
latest <- latest[order(-latest$degrees), ]
df$program <- factor(df$program, levels = latest$program)

# ── Colours: Tableau 10 muted ──────────────────────────────────
# One colour per program, MBA gets the strongest
cols <- c(
  "MBA"                         = "#4e79a7",
  "Mgmt Science &\nAnalytics"   = "#e15759",
  "Accounting"                   = "#59a14f",
  "Data Science &\nInfo Systems" = "#f28e2b",
  "Finance"                      = "#76b7b2",
  "Marketing"                    = "#b07aa1"
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
      plot.title     = element_text(size = rel(1.35), face = "bold",
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

# ── Direct labels at right end ─────────────────────────────────
# Position labels at the last year, with the program name
end <- df[df$year == max(df$year), ]
end <- end[order(-end$degrees_k), ]

# Manually nudge y positions to avoid overlap
# Start from actual values, then adjust
end$label <- paste0(end$program, "  ", comma(end$degrees))
end$y_lab <- end$degrees_k

# Simple repulsion: enforce minimum gap, but don't push below zero
min_gap <- 2.8  # in thousands
for (i in 2:nrow(end)) {
  if (end$y_lab[i] > end$y_lab[i - 1] - min_gap) {
    end$y_lab[i] <- end$y_lab[i - 1] - min_gap
  }
}
# Ensure nothing below 1K
end$y_lab <- pmax(end$y_lab, 1)

# ── Plot ───────────────────────────────────────────────────────
p <- ggplot(df, aes(x = year, y = degrees_k, colour = program)) +
  geom_line(linewidth = 0.85) +
  geom_point(size = 1.1) +

  # Direct labels at right margin
  geom_text(
    data = end,
    aes(x = max(df$year) + 0.5, y = y_lab, label = label, colour = program),
    hjust = 0, size = 3.0, family = base_family, fontface = "bold",
    lineheight = 0.85, show.legend = FALSE
  ) +

  scale_colour_manual(values = cols) +

  scale_x_continuous(
    breaks = seq(2000, 2024, 4),
    expand = expansion(mult = c(0.02, 0.28))   # generous right margin for labels
  ) +
  scale_y_continuous(
    labels = function(x) paste0(format(x, big.mark = ","), "K"),
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.06))
  ) +

  labs(
    title    = "The Changing Landscape of Business Master\u2019s Degrees",
    subtitle = "Annual degrees conferred by program type, 2000\u20132024",
    x        = NULL,
    y        = NULL,
    caption  = paste0(
      "Source: IPEDS Completions Survey, NCES.  2024 data are provisional.\n",
      "Mgmt Science & Analytics = CIP 52.1301/1399/1302.  ",
      "Data Science & Info Systems = CIP 52.1201, 11.0401, 27.0501, 30.7001."
    )
  ) +

  theme_healy()

# ── Save ───────────────────────────────────────────────────────
ggsave("mba_expanded.png", p, width = 10, height = 6, dpi = 300,
       bg = "white", device = ragg::agg_png)

message("Saved \u2192 mba_expanded.png")
