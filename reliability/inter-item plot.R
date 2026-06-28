library(tidyverse)
library(readr)
library(ggplot2)
library(patchwork)

# --- 1. Load + process Study 1 ---
data1 <- read_csv("tropical-summer-ema/data/daily_data.csv")

data_within1 <- data1 %>% 
  mutate(across(Q3:Q7, ~ 6 - .x)) %>% 
  group_by(`Study ID`) %>% 
  mutate(across(Q3:Q11, ~ .x - mean(.x, na.rm = TRUE))) %>% 
  ungroup()

# --- 2. Load + process Study 2 ---
data2 <- read_csv("Desktop/daily_data_cleaned.csv")

data_within2 <- data2 %>% 
  mutate(across(q3:q7, ~ 4 - .x)) %>% 
  group_by(ID) %>% 
  mutate(across(q3:q11, ~ .x - mean(.x, na.rm = TRUE))) %>% 
  ungroup()

# --- 3. Function to create triangular correlation plot ---
make_cor_plot <- function(data, vars, title) {
  
  cor_mat <- cor(data %>% select(all_of(vars)), use = "pairwise.complete.obs")
  n <- length(vars)
  
  cor_df <- as.data.frame(as.table(cor_mat))
  colnames(cor_df) <- c("Var1", "Var2", "Correlation")
  
  cor_df <- cor_df %>%
    mutate(
      x = as.numeric(factor(Var2, levels = vars)),
      y = as.numeric(factor(Var1, levels = vars))
    ) %>%
    filter(y >= x)   # lower triangle + diagonal
  
  ggplot(cor_df, aes(x, y, fill = Correlation)) +
    
    geom_tile(width = 0.95, height = 0.95) +
    
    geom_text(aes(label = sprintf("%.2f", Correlation)), size = 4) +
    
    scale_fill_gradient(low = "#c7c7ff", high = "#2f2be9") +
    
    scale_x_continuous(
      breaks = 1:n,
      labels = paste("Item", 3:11),
      expand = c(0, 0)
    ) +
    
    scale_y_reverse(
      breaks = 1:n,
      labels = paste("Item", 3:11),
      expand = c(0, 0)
    ) +
    
    # IMPORTANT: no coord_fixed → resizes normally
    coord_cartesian() +
    
    labs(title = title, x = NULL, y = NULL) +
    
    theme_classic(base_size = 12) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(hjust = 0.5, face = "bold"),
      legend.position = "right"
    )
}

# --- 4. Create plots ---
p1 <- make_cor_plot(data_within1, paste0("Q", 3:11), "A. Study 1 (n = 71)")
p2 <- make_cor_plot(data_within2, paste0("q", 3:11), "B. Study 2 (n = 99)")

# --- 5. Combine plots ---
final_plot <- p1 / p2 

# --- 6. Display ---
print(final_plot)

# --- 7. Optional: save with explicit size control ---
ggsave("correlation_plot.png", final_plot, width = 8, height = 10, dpi = 300)