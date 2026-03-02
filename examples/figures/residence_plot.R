## ---------------------------------------------------------------
## In-State vs. Out-of-State First-Time Undergraduates, 2002–2022
##
## Two panels: raw counts and percentage share, by institution type
## IPEDS residence data (EF_C) with full coverage in even years only
## ---------------------------------------------------------------

library(ggplot2)
library(duckdb)
library(scales)

base_family <- "Helvetica Neue"

# ── Data ───────────────────────────────────────────────────────
con <- dbConnect(duckdb(), dbdir = "ipeds.duckdb", read_only = TRUE)

df <- dbGetQuery(con, "
  WITH inst_state AS (
    SELECT unitid, year, CAST(fips_state AS INT) AS fips, control
    FROM   hd
  )
  SELECT c.year,
         CASE WHEN h.control = 1 THEN 'Public'
              WHEN h.control = 2 THEN 'Private nonprofit'
              ELSE 'Private for-profit' END AS inst_type,
         SUM(CASE WHEN CAST(c.efcstate AS INT) = h.fips
                  THEN CAST(c.efres01 AS BIGINT) END) AS in_state,
         SUM(CASE WHEN CAST(c.efcstate AS INT) != h.fips
                   AND CAST(c.efcstate AS INT) NOT IN (90, 98, 99)
                   AND c.line != 999
                  THEN CAST(c.efres01 AS BIGINT) END) AS out_of_state,
         SUM(CASE WHEN CAST(c.efcstate AS INT) = 90
                  THEN CAST(c.efres01 AS BIGINT) END) AS foreign_students,
         SUM(CASE WHEN CAST(c.efcstate AS INT) = 99
                  THEN CAST(c.efres01 AS BIGINT) END) AS total
  FROM   ef_c c
  JOIN   inst_state h ON c.unitid = h.unitid AND c.year = h.year
  WHERE  c.year IN (2002,2004,2006,2008,2010,2012,2014,2016,2018,2020,2022)
  GROUP  BY c.year, inst_type
  ORDER  BY c.year, inst_type
")

dbDisconnect(con, shutdown = TRUE)

# Compute percentages
df$pct_in  <- df$in_state / df$total * 100
df$pct_oos <- df$out_of_state / df$total * 100
df$pct_fgn <- df$foreign_students / df$total * 100

# Reshape to long for plotting
library(tidyr)

long_pct <- df[, c("year", "inst_type", "pct_in", "pct_oos", "pct_fgn")]
long_pct <- pivot_longer(long_pct, cols = c("pct_in", "pct_oos", "pct_fgn"),
                          names_to = "residence", values_to = "pct")
long_pct$residence <- factor(long_pct$residence,
  levels = c("pct_in", "pct_oos", "pct_fgn"),
  labels = c("In-state", "Out-of-state", "Foreign")
)

long_count <- df[, c("year", "inst_type", "in_state", "out_of_state", "foreign_students")]
names(long_count)[3:5] <- c("In-state", "Out-of-state", "Foreign")
long_count <- pivot_longer(long_count, cols = c("In-state", "Out-of-state", "Foreign"),
                            names_to = "residence", values_to = "count")
long_count$count_k <- long_count$count / 1000

# Factor ordering
long_pct$inst_type <- factor(long_pct$inst_type,
  levels = c("Public", "Private nonprofit", "Private for-profit"))
long_count$inst_type <- factor(long_count$inst_type,
  levels = c("Public", "Private nonprofit", "Private for-profit"))

# ── Colours ────────────────────────────────────────────────────
res_cols <- c(
  "In-state"     = "#4e79a7",
  "Out-of-state" = "#e15759",
  "Foreign"      = "#f28e2b"
)

# ── Theme ──────────────────────────────────────────────────────
theme_healy <- function(base_size = 12, base_family = "Helvetica Neue") {
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
      plot.subtitle  = element_text(size = rel(0.85), colour = "grey40",
                                    hjust = 0, margin = margin(b = 10)),
      plot.caption   = element_text(size = rel(0.65), colour = "grey55",
                                    hjust = 1, margin = margin(t = 10)),
      plot.margin  = margin(14, 16, 10, 10),
      strip.text   = element_text(size = rel(1.0), face = "bold",
                                  colour = "grey25", hjust = 0),
      legend.position = "none"
    )
}

# ── Figure 1: Percentage share by sector ───────────────────────

# Compute end-labels
end_pct <- long_pct[long_pct$year == max(long_pct$year), ]
end_pct$label <- paste0(end_pct$residence, " ",
                         sprintf("%.0f%%", end_pct$pct))

p1 <- ggplot(long_pct, aes(x = year, y = pct, colour = residence)) +
  geom_line(linewidth = 0.85) +
  geom_point(size = 1.3) +
  facet_wrap(~ inst_type, nrow = 1) +

  # Direct labels at end — nudge overlapping labels apart
  geom_text(
    data = end_pct,
    aes(x = max(long_pct$year) + 0.4, y = pct +
          ifelse(residence == "In-state" & inst_type == "Private nonprofit", 2,
          ifelse(residence == "Out-of-state" & inst_type == "Private nonprofit", -2, 0)),
        label = label),
    size = 2.6, family = base_family, fontface = "bold",
    hjust = 0, show.legend = FALSE
  ) +

  scale_colour_manual(values = res_cols) +
  scale_x_continuous(
    breaks = seq(2002, 2022, 4),
    expand = expansion(mult = c(0.03, 0.30))
  ) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    limits = c(0, 100),
    breaks = seq(0, 100, 20)
  ) +

  labs(
    title    = "Where Do Undergraduates Come From?",
    subtitle = "Share of first-time entering students by state of residence, 2002\u20132022",
    x = NULL, y = NULL,
    caption  = paste0(
      "Source: IPEDS Fall Enrollment by Residence (EF_C), NCES.  Even years only (full institutional coverage).\n",
      "First-time degree/certificate-seeking undergraduates.  ",
      "In-state = student\u2019s home state matches institution\u2019s state."
    )
  ) +
  theme_healy()

ggsave("residence_pct.png", p1, width = 12, height = 5, dpi = 300,
       bg = "white", device = ragg::agg_png)
message("Saved \u2192 residence_pct.png")


# ── Figure 2: Counts (all sectors combined) ────────────────────

# Aggregate across sectors
agg <- aggregate(count ~ year + residence, long_count, sum, na.rm = TRUE)
agg$count_k <- agg$count / 1000
agg$residence <- factor(agg$residence,
  levels = c("In-state", "Out-of-state", "Foreign"))

end_agg <- agg[agg$year == max(agg$year), ]
end_agg$label <- paste0(end_agg$residence, "  ", comma(end_agg$count))

# Nudge labels
end_agg <- end_agg[order(-end_agg$count_k), ]
min_gap <- 80
for (i in 2:nrow(end_agg)) {
  if (end_agg$count_k[i] > end_agg$count_k[i-1] - min_gap) {
    end_agg$count_k[i] <- end_agg$count_k[i-1] - min_gap
  }
}
end_agg$y_lab <- pmax(end_agg$count_k, 30)

p2 <- ggplot(agg, aes(x = year, y = count_k, colour = residence)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.4) +

  geom_text(
    data = end_agg,
    aes(x = max(agg$year) + 0.4, y = y_lab, label = label),
    size = 3.1, family = base_family, fontface = "bold",
    hjust = 0, show.legend = FALSE
  ) +

  scale_colour_manual(values = res_cols) +
  scale_x_continuous(
    breaks = seq(2002, 2022, 4),
    expand = expansion(mult = c(0.03, 0.28))
  ) +
  scale_y_continuous(
    labels = function(x) paste0(format(x, big.mark = ","), "K"),
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.06))
  ) +

  labs(
    title    = "Out-of-State Enrollment Holds Steady as In-State Shrinks",
    subtitle = "First-time entering undergraduates by residence, all institutions, 2002\u20132022",
    x = NULL, y = NULL,
    caption  = paste0(
      "Source: IPEDS Fall Enrollment by Residence (EF_C), NCES.  Even years only (full institutional coverage).\n",
      "First-time degree/certificate-seeking undergraduates."
    )
  ) +
  theme_healy()

ggsave("residence_count.png", p2, width = 10, height = 6, dpi = 300,
       bg = "white", device = ragg::agg_png)
message("Saved \u2192 residence_count.png")
