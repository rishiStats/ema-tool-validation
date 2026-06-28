# ── 1. LIBRARIES ──────────────────────────────────────────────────────────────
library(readr)
library(dplyr)
library(ggplot2)
library(patchwork)
library(grid)
library(cowplot)

# ── 2. DATA LOADING & UPDATED HELPER FUNCTION ─────────────────────────────────
data_1 <- read_csv("tropical-summer-ema/data/daily_data.csv")
data_2 <- read_csv("Desktop/daily_data_cleaned.csv")

# Helper function with Percentages and Internal Labels
make_bar_pct <- function(df, col_name, item_label, fill_color) {
  # Calculate percentages
  pct_df <- df %>%
    count(Response = .data[[col_name]]) %>%
    mutate(perc = n / sum(n) * 100)
  
  ggplot(pct_df, aes(x = factor(Response), y = perc)) +
    geom_bar(stat = "identity", fill = fill_color, color = "white") +
    # Add percentage text over bars
    geom_text(aes(label = sprintf("%.1f%%", perc)), 
              vjust = -0.5, size = 2.5, color = "grey20") +
    # Internal label in top right
    annotate("text", x = Inf, y = Inf, label = item_label, 
             hjust = 1.1, vjust = 1.5, size = 3.5, 
             fontface = "bold", color = "grey30") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.2))) + # Space for text
    labs(x = NULL, y = NULL) +
    theme_minimal(base_size = 8) +
    theme(panel.grid.minor = element_blank())
}

# ── 3. GENERATE 16 PLOTS (p1-p16) ─────────────────────────────────────────────
# Study 1: Factor 1 (3,4,5,7) then Factor 2 (8,9,10,11)
p1 <- make_bar_pct(data_1, "Q3", "Item 3", "#2C3E50")
p2 <- make_bar_pct(data_1, "Q4", "Item 4", "#2C3E50")
p5 <- make_bar_pct(data_1, "Q5", "Item 5", "#2C3E50")
p6 <- make_bar_pct(data_1, "Q7", "Item 7", "#2C3E50")

p9  <- make_bar_pct(data_1, "Q8",  "Item 8",  "#E74C3C")
p10 <- make_bar_pct(data_1, "Q9",  "Item 9",  "#E74C3C")
p13 <- make_bar_pct(data_1, "Q10", "Item 10", "#E74C3C")
p14 <- make_bar_pct(data_1, "Q11", "Item 11", "#E74C3C")

# Study 2: Factor 1 (3,4,5,7) then Factor 2 (8,9,10,11)
p3 <- make_bar_pct(data_2, "q3", "Item 3", "#34495E")
p4 <- make_bar_pct(data_2, "q4", "Item 4", "#34495E")
p7 <- make_bar_pct(data_2, "q5", "Item 5", "#34495E")
p8 <- make_bar_pct(data_2, "q7", "Item 7", "#34495E")

p11 <- make_bar_pct(data_2, "q8",  "Item 8",  "#C0392B")
p12 <- make_bar_pct(data_2, "q9",  "Item 9",  "#C0392B")
p15 <- make_bar_pct(data_2, "q10", "Item 10", "#C0392B")
p16 <- make_bar_pct(data_2, "q11", "Item 11", "#C0392B")

# ── 4. VISUAL STYLE & HEADERS (FIXED SPACING & UNDERLINES) ────────────────────
pub_theme <- theme_classic(base_size = 10, base_family = "Helvetica") +
  theme(
    axis.text        = element_text(size = 7,  colour = "grey30"),
    axis.line        = element_line(colour = "grey40", linewidth = 0.35),
    panel.grid.major = element_line(colour = "grey93", linewidth = 0.3),
    plot.margin      = margin(3, 5, 3, 5)
  )

# Updated Column Header with Underline
col_header <- function(label) {
  ggplot() +
    annotate("text", x = 0.5, y = 0.35, label = label, 
             size = 4, fontface = "bold", colour = "grey10") +
    annotate("segment", x = 0.02, xend = 0.98, y = 0.05, yend = 0.05, 
             colour = "grey50", linewidth = 0.5) +
    scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
    theme_void()
}

# Updated Row Header with Vertical Line and Right-Aligned text
row_header <- function(label) {
  ggplot() +
    annotate("text", x = 0.8, y = 0.5, label = label, angle = 90, 
             size = 4, fontface = "bold", colour = "grey10") +
    annotate("segment", x = 0.95, xend = 0.95, y = 0.05, yend = 0.95, 
             colour = "grey50", linewidth = 0.5) +
    scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
    theme_void()
}

# Generate specific headers
study1_title  <- col_header("Study 1 (n = 71)")
study2_title  <- col_header("Study 2 (n = 99)")
factor1_title <- row_header("Factor 1")
factor2_title <- row_header("Factor 2")
blank         <- ggplot() + theme_void()

# ── 5. FINAL ASSEMBLY (TIGHTER RATIOS) ────────────────────────────────────────
final_plot <- blank + study1_title + study2_title +
  factor1_title + 
  (p1 + pub_theme) + (p2 + pub_theme) + (p3 + pub_theme) + (p4 + pub_theme) +
  (p5 + pub_theme) + (p6 + pub_theme) + (p7 + pub_theme) + (p8 + pub_theme) +
  factor2_title + 
  (p9 + pub_theme) + (p10 + pub_theme) + (p11 + pub_theme) + (p12 + pub_theme) +
  (p13 + pub_theme) + (p14 + pub_theme) + (p15 + pub_theme) + (p16 + pub_theme) +
  plot_layout(
    design = c(
      area(1, 1), area(1, 2, 1, 3), area(1, 4, 1, 5),
      area(2, 1, 3, 1),
      area(2, 2), area(2, 3), area(2, 4), area(2, 5),
      area(3, 2), area(3, 3), area(3, 4), area(3, 5),
      area(4, 1, 5, 1),
      area(4, 2), area(4, 3), area(4, 4), area(4, 5),
      area(5, 2), area(5, 3), area(5, 4), area(5, 5)
    ),
    # Reduced first col from 0.4 to 0.15 and first row from 0.4 to 0.18
    widths  = c(0.25, 1, 1, 1, 1), 
    heights = c(0.25, 1, 1, 1, 1)
  )

# Adjust Background math to match new 0.15 width ratio
# Total width units = 4.15. Row label = 0.15/4.15 = 0.036
# Each study (2 columns) = 2/4.15 = 0.48
final_with_bg <- ggdraw() +
  draw_grob(rectGrob(gp = gpar(fill = "grey96", col = NA)), 
            x = 0.045, y = 0, width = 0.47, height = 0.95, hjust = 0, vjust = 0) +
  draw_grob(rectGrob(gp = gpar(fill = "grey96", col = NA)), 
            x = 0.525, y = 0, width = 0.47, height = 0.95, hjust = 0, vjust = 0) +
  draw_plot(final_plot)

final_with_bg