library(tidyverse)

# -------------------------------------------------------------
# 1. Load data and create required variables
# -------------------------------------------------------------

# Load the CSV file (place the file in your current RStudio Project directory)
df_raw <- read_csv("~/Desktop/GMFD_TINV_clim_regsort.csv")

# Data preparation:
df <- df_raw %>%
  mutate(
    # Convert annual temperature sum into average daily temperature (approx. -5 to 35 °C)
    temp_c = temp1_GMFD / 365,
    
    # Create income deciles based on 15-year moving average income
    inc_decile = ntile(lgdppc_MA15, 10),
    inc_decile = factor(
      inc_decile,
      levels = 1:10,
      labels = c("Poorest decile",
                 paste("Decile", 2:9),
                 "Richest decile")
    )
  ) %>%
  # Keep a reasonable temperature range similar to the original Figure 1a
  filter(temp_c >= -5, temp_c <= 35)

# -------------------------------------------------------------
# 2. Plot a Figure-1a-style energy–temperature response curve
# -------------------------------------------------------------

# The logic:
# - For each income decile × fuel type, fit a weighted smooth curve
# - Weights = population, similar to the idea of population-weighted regressions
# - Facet by income decile to reproduce the style of Panel 1a

p_1a <- ggplot(
  df,
  aes(x = temp_c,
      y = load_pc,
      colour = product,
      group = product)
) +
  geom_smooth(
    aes(weight = pop),
    method = "loess",   # non-parametric local regression
    span = 0.7,         # controls smoothness
    se = FALSE          # no confidence band (matches original style)
  ) +
  facet_wrap(~ inc_decile, ncol = 5) +
  labs(
    x = "Daily temperature (°C)",
    y = "Energy use (GJ per capita)",
    colour = "Fuel type",
    title = "Figure 1a replication: Energy–temperature response by income decile"
  ) +
  theme_bw() +
  theme(
    strip.background = element_rect(fill = "grey95"),
    panel.grid.minor = element_blank()
  )

# Display the plot
p_1a


# save the plot to a PNG file
ggsave("figure1a_replication.png",
       p_1a,
       width = 8, height = 5, dpi = 300)
