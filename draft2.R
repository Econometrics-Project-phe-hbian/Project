# ---------------------------------------------------------------
# Load packages
# ---------------------------------------------------------------
library(tidyverse)

# ---------------------------------------------------------------
# Load data
# ---------------------------------------------------------------
df <- read_csv("~/Desktop/GMFD_TINV_clim_regsort.csv")

# Check energy product categories (should include electricity & other fuels)
count(df, product)

# ---------------------------------------------------------------
# Construct global income deciles (based on 15-year moving-average log GDP)
# ---------------------------------------------------------------
df <- df %>%
  mutate(
    # 1 = poorest, 10 = richest
    inc_decile = ntile(lgdppc_MA15, 10),
    inc_label  = case_when(
      inc_decile == 1  ~ "Poorest\ndecile",
      inc_decile == 10 ~ "Richest\ndecile",
      TRUE             ~ paste0("Decile ", inc_decile)
    )
  )

# ---------------------------------------------------------------
# Create temperature grid for plotting
# ---------------------------------------------------------------
T_grid <- seq(-5, 35, by = 0.25)

# ---------------------------------------------------------------
# Function to estimate temperature response curve for a subgroup
# ---------------------------------------------------------------
fit_temp_curve <- function(dat) {
  
  # Convert FE variables to factors
  dat <- dat %>%
    mutate(
      country = factor(country),
      year    = factor(year)
    )
  
  # Baseline regression used for simplified replication:
  # load_pc ~ polynomial temperature bins + country FE + year FE
  m <- lm(
    load_pc ~ temp1_GMFD + temp2_GMFD + temp3_GMFD + temp4_GMFD +
      country + year,
    data = dat
  )
  
  # Extract coefficients for the temperature polynomial terms
  cf <- coef(m)[c("temp1_GMFD", "temp2_GMFD", "temp3_GMFD", "temp4_GMFD")]
  cf[is.na(cf)] <- 0
  
  # Energy response function f(T)
  fT  <- cf["temp1_GMFD"] * T_grid +
    cf["temp2_GMFD"] * T_grid^2 +
    cf["temp3_GMFD"] * T_grid^3 +
    cf["temp4_GMFD"] * T_grid^4
  
  # Evaluate at 20°C to set a baseline
  f20 <- cf["temp1_GMFD"] * 20 +
    cf["temp2_GMFD"] * 20^2 +
    cf["temp3_GMFD"] * 20^3 +
    cf["temp4_GMFD"] * 20^4
  
  tibble(
    temp_C = T_grid,
    dE_GJ  = fT - f20     # Energy relative to 20°C
  )
}

# ---------------------------------------------------------------
# Estimate curves for each income decile × fuel category
# ---------------------------------------------------------------
curve_df <- df %>%
  filter(product %in% c("electricity", "other_energy")) %>%  # Select two fuels
  group_by(product, inc_decile, inc_label) %>%
  group_modify(~ fit_temp_curve(.x)) %>%
  ungroup() %>%
  mutate(
    # Relabel product names for plotting
    fuel = recode(product,
                  "electricity"   = "Electricity",
                  "other_energy"  = "Other fuels"),
    fuel = factor(fuel, levels = c("Electricity", "Other fuels")),
    
    inc_f = factor(
      inc_decile,
      levels = 1:10,
      labels = c("Poorest\ndecile",
                 paste0("Decile ", 2:9),
                 "Richest\ndecile")
    )
  )

# ---------------------------------------------------------------
# Plot Figure 1a (approximate reproduction)
# ---------------------------------------------------------------
library(ggplot2)

p_fig1a <- ggplot(curve_df,
                  aes(x = temp_C, y = dE_GJ, colour = fuel)) +
  
  # Horizontal zero line
  geom_hline(yintercept = 0, linewidth = 0.2) +
  
  # Vertical reference lines
  geom_vline(xintercept = c(-5, 5, 15, 25, 35),
             linewidth = 0.2, colour = "grey60") +
  
  # Energy response curves
  geom_line(linewidth = 0.4) +
  
  # Axis settings to match original figure
  scale_x_continuous(
    breaks  = c(-5, 5, 15, 25, 35),
    limits  = c(-5, 35),
    expand  = c(0, 0)
  ) +
  scale_y_continuous(
    breaks = c(-0.05, 0, 0.05, 0.10),
    limits = c(-0.05, 0.10)
  ) +
  
  # Colors similar to the Nature figure
  scale_colour_manual(values = c("Electricity" = "#4C72B0",
                                 "Other fuels" = "#DD8452")) +
  
  labs(
    x = "Daily temperature (ºC)",
    y = "Energy (GJ per capita)"
  ) +
  
  # Facet layout: rows = fuel type, columns = income deciles
  facet_grid(fuel ~ inc_f, switch = "y") +
  
  theme_bw(base_size = 8) +
  theme(
    panel.grid        = element_blank(),
    strip.background  = element_blank(),
    strip.placement   = "outside",
    strip.text.y.left = element_text(angle = 0),
    legend.position   = "none",
    axis.title.x      = element_text(margin = margin(t = 3)),
    axis.title.y      = element_text(margin = margin(r = 3))
  )

# ---------------------------------------------------------------
# Save the figure (you may adjust width/height for perfect replication)
# ---------------------------------------------------------------
ggsave("figure1a_replication.pdf",
       p_fig1a, width = 7.2, height = 2.4, units = "in")
print(p_fig1a)
