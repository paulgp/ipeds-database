## ---------------------------------------------------------------
## Business Master's Degrees: Stacked Area
## Shows total growing while composition shifts away from MBA
##
## Kieran Healy style: clean type, direct labels, no legend box
## ---------------------------------------------------------------

library(ggplot2)
library(duckdb)
library(scales)
library(dplyr)

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
  query("MBA",                       "52.0201"),
  query("Mgmt Science & Analytics",  c("52.1301", "52.1399", "52.1302")),
  query("Data Science & Info Sys",   c("52.1201", "11.0401", "27.0501", "30.7001")),
  query("Accounting",                "52.0301"),
  query("Finance",                   "52.0801"),
  query("Marketing",                 "52.1401")
)

dbDisconnect(con, shutdown = TRUE)

df$degrees_k <- df$degrees / 1000

# Stack order: small/shrinking at bottom, growing at middle, MBA on top
stack_order <- c(
  "Marketing",
  "Finance",
  "Accounting",
  "Data Science & Info Sys",
  "Mgmt Science & Analytics",
  "MBA"
)
df$program <- factor(df$program, levels = stack_order)

# ── Colours ────────────────────────────────────────────────────
cols <- c(
  "MBA"                        = "#4e79a7",
  "Accounting"                 = "#59a14f",
  "Finance"                    = "#76b7b2",
  "Marketing"                  = "#b07aa1",
  "Data Science & Info Sys"    = "#f28e2b",
  "Mgmt Science & Analytics"   = "#e15759"
)

# Darker versions for label text (more readable than fill colours)
label_cols <- c(
  "MBA"                        = "#3a6390",
  "Accounting"                 = "#407a36",
  "Finance"                    = "#538a86",
  "Marketing"                  = "#8a5c82",
  "Data Science & Info Sys"    = "#c0700f",
  "Mgmt Science & Analytics"   = "#b83c3e"
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

# ── Compute label positions: band midpoints at final year ──────
label_df <- df %>%
  filter(year == max(year)) %>%
  arrange(factor(program, levels = stack_order)) %>%
  mutate(
    cum_top    = cumsum(degrees_k),
    cum_bottom = cum_top - degrees_k,
    y_mid      = (cum_top + cum_bottom) / 2,
    label      = paste0(program, "  ", comma(degrees))
  )

# Nudge y positions for right-margin labels to avoid overlap
# Work from top to bottom, enforce minimum gap
label_df <- label_df %>% arrange(desc(y_mid))
min_gap <- 5.5
for (i in 2:nrow(label_df)) {
  if (label_df$y_mid[i] > label_df$y_mid[i - 1] - min_gap) {
    label_df$y_mid[i] <- label_df$y_mid[i - 1] - min_gap
  }
}
label_df$y_mid <- pmax(label_df$y_mid, 2)

# ── Totals for annotation ─────────────────────────────────────
totals <- aggregate(degrees_k ~ year, df, sum)
total_2000 <- totals$degrees_k[totals$year == min(totals$year)]
total_last <- totals$degrees_k[totals$year == max(totals$year)]
year_last  <- max(totals$year)

# ── Plot ───────────────────────────────────────────────────────
p <- ggplot(df, aes(x = year, y = degrees_k, fill = program)) +
  geom_area(alpha = 0.85, colour = "white", linewidth = 0.3,
            position = position_stack(reverse = TRUE)) +

  # All labels on right margin, colour-coded to match bands
  geom_text(
    data = label_df,
    aes(x = max(df$year) + 0.6, y = y_mid, label = label, colour = program),
    size = 3.0, family = base_family, fontface = "bold",
    hjust = 0, inherit.aes = FALSE, show.legend = FALSE
  ) +

  # Total annotation at top
  annotate("text", x = year_last - 0.3,
           y = total_last + 5,
           label = paste0("Total: ", comma(round(total_last * 1000)), " (",
                          year_last, ")"),
           size = 3.5, colour = "grey25", family = base_family,
           fontface = "bold", hjust = 1) +
  annotate("text", x = min(df$year) + 0.3,
           y = total_2000 + 5,
           label = paste0("Total: ", comma(round(total_2000 * 1000)), " (",
                          min(df$year), ")"),
           size = 3.2, colour = "grey45", family = base_family, hjust = 0) +

  scale_fill_manual(values = cols) +
  scale_colour_manual(values = label_cols) +

  scale_x_continuous(
    breaks = seq(2000, 2024, 4),
    expand = expansion(mult = c(0.02, 0.30))
  ) +
  scale_y_continuous(
    labels = function(x) paste0(format(x, big.mark = ","), "K"),
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.08))
  ) +

  labs(
    title    = "Business Master\u2019s Degrees Are Still Growing \u2014 Just Not the MBA",
    subtitle = "Total degrees conferred across six major business master\u2019s programs, 2000\u20132024",
    x        = NULL,
    y        = NULL,
    caption  = paste0(
      "Source: IPEDS Completions Survey, NCES.  2024 data are provisional.\n",
      "Mgmt Science & Analytics = CIP 52.1301/1399/1302.  ",
      "Data Science & Info Sys = CIP 52.1201, 11.0401, 27.0501, 30.7001."
    )
  ) +

  theme_healy()

# ── Save ───────────────────────────────────────────────────────
ggsave("mba_stacked.png", p, width = 11, height = 7, dpi = 300,
       bg = "white", device = ragg::agg_png)

message("Saved \u2192 mba_stacked.png")
