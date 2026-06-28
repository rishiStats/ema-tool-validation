# ── 1. LIBRARIES ──────────────────────────────────────────────────────────────
library(readr)
library(dplyr)
library(ggplot2)
library(patchwork)
library(cowplot)
library(grid)

# ── 2. DATA PROCESSING ────────────────────────────────────────────────────────
data_1 <- read_csv("tropical-summer-ema/data/daily_data.csv")
data_2 <- read_csv("Desktop/daily_data_cleaned.csv")

s1_clean <- data_1 %>%
  mutate(
    Factor1 = Q3 + Q4 + Q5 + Q7,
    Factor2 = Q8 + Q9 + Q10 + Q11,
    Overall = 24 - Factor1 + Factor2,
    Study   = "Study 1"
  ) %>%
  rename(Day = day)

s2_clean <- data_2 %>%
  mutate(
    Factor1 = q3 + q4 + q5 + q7,
    Factor2 = q8 + q9 + q10 + q11,
    Overall = 16 - Factor1 + Factor2,
    Study   = "Study 2"
  )

# ── 3. PUBLICATION THEME ──────────────────────────────────────────────────────
pub_theme <- theme_classic(base_size = 10, base_family = "Helvetica") +
  theme(
    axis.title       = element_text(size = 9,  colour = "grey20"),
    axis.text        = element_text(size = 8,  colour = "grey30"),
    axis.line        = element_line(colour = "grey40", linewidth = 0.35),
    axis.ticks       = element_line(colour = "grey40", linewidth = 0.35),
    panel.grid.major = element_line(colour = "grey93", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    plot.background  = element_rect(fill = "transparent", colour = NA),
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.margin      = margin(4, 8, 4, 8)
  )

# ── 4. SPAGHETTI HELPER ───────────────────────────────────────────────────────
make_spaghetti <- function(df, y_var, id_var, line_col, mean_col, y_limits, y_label) {
  ggplot(df, aes(x = Day, y = .data[[y_var]], group = .data[[id_var]])) +
    geom_line(alpha = 0.12, color = line_col, linewidth = 0.35) +
    stat_summary(aes(group = 1), fun = mean, geom = "line",
                 color = mean_col, linewidth = 1.0) +
    scale_y_continuous(limits = y_limits, expand = expansion(mult = c(0.02, 0.05))) +
    scale_x_continuous(breaks = seq(0, 30, 5), expand = expansion(mult = c(0.02, 0.02))) +
    labs(x = "Day", y = y_label) +
    pub_theme
}

# ── 5. COLOUR PALETTE ─────────────────────────────────────────────────────────
f1_light <- "#7B9BB5";  f1_dark <- "#2C4A6E"
f2_light <- "#B58A8A";  f2_dark <- "#7A2E2E"
ov_light <- "#7AADA0";  ov_dark <- "#1D6A5E"

# ── 6. INDIVIDUAL PLOTS ───────────────────────────────────────────────────────
sp1 <- make_spaghetti(s1_clean, "Factor1", "Study ID", f1_light, f1_dark, c(0, 24), "Score (0–24)")
sp2 <- make_spaghetti(s2_clean, "Factor1", "ID",       f1_light, f1_dark, c(0, 16), "Score (0–16)")
sp3 <- make_spaghetti(s1_clean, "Factor2", "Study ID", f2_light, f2_dark, c(0, 24), "Score (0–24)")
sp4 <- make_spaghetti(s2_clean, "Factor2", "ID",       f2_light, f2_dark, c(0, 16), "Score (0–16)")
sp5 <- make_spaghetti(s1_clean, "Overall", "Study ID", ov_light, ov_dark, c(0, 48), "Score (0–48)")
sp6 <- make_spaghetti(s2_clean, "Overall", "ID",       ov_light, ov_dark, c(0, 32), "Score (0–32)")

# ── 7. HEADER HELPERS (OPTIMIZED) ─────────────────────────────────────────────
col_header <- function(label) {
  ggplot() +
    annotate("text", x = 0.5, y = 0.4, # Lowered slightly
             label = label, size = 4.2, fontface = "bold",
             colour = "grey10", family = "Helvetica") +
    annotate("segment",
             x = 0, xend = 1, y = 0.1, yend = 0.1,
             colour = "grey50", linewidth = 0.5) +
    scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
    theme_void()
}

row_header <- function(label) {
  ggplot() +
    # Moved x to 0.8 to put text closer to the vertical line/graphs
    annotate("text", x = 0.75, y = 0.5,
             label = label, angle = 90,
             size = 4.2, fontface = "bold",
             colour = "grey10", family = "Helvetica") +
    # Moved segment to 0.98 to act as a tight border for the plots
    annotate("segment",
             x = 0.98, xend = 0.98, y = 0.05, yend = 0.95,
             colour = "grey50", linewidth = 0.5) +
    scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
    coord_cartesian(clip = "off") +
    theme_void() +
    theme(plot.margin = margin(0, 0, 0, 0))
}

blank <- plot_spacer()

# ── 8. ASSEMBLE PATCHWORK (TIGHTER WIDTHS) ────────────────────────────────────
final_plot <-
  blank + col_header("Study 1") + col_header("Study 2") +
  row_header("Factor 1") + sp1 + sp2 +
  row_header("Factor 2") + sp3 + sp4 +
  row_header("Overall")  + sp5 + sp6 +
  plot_layout(
    ncol = 3,
    # Changed 0.5 to 0.15. This makes the label column much narrower.
    widths  = c(0.15, 1, 1), 
    heights = c(0.2, 1, 1, 1) # Shortened header height too
  )
final_plot

# ── 10. EXPORT ────────────────────────────────────────────────────────────────
#ggsave("spaghetti_figure.pdf", plot = final_with_bg,
       #width = 9, height = 8, device = cairo_pdf)