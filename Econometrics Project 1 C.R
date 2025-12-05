#package
library(data.table)
library(fixest)
library(ggplot2)
library(dplyr)
library(haven)
library(patchwork)
#data
break_data <- read.csv("break_data_TINV_clim.csv")
reg_data <- read.csv("GMFD_TINV_clim_regsort.csv")
#Setting up
model <- "TINV_clim"
var <- "electricity"                         # or "other_energy"
output_path <- "figures"                     # folder to save plots
outcome_var <- "FD_load_pc"                  # outcome variable
#Variables
# Sort and diff year
reg_data <- reg_data %>%
  arrange(gpid, year) %>%
  group_by(gpid) %>%
  mutate(D_year = year - lag(year)) %>%
  ungroup()

# Create normalized temperature interaction terms
reg_data <- reg_data %>%
  mutate(
    FD_yeartemp1_GMFD = D_year * (temp1_GMFD - 20),
    FD_yeartemp2_GMFD = D_year * (temp2_GMFD - 400)
  )

#Model
model_fit <- feols(
  FD_load_pc ~ indp1 + FD_yeartemp1_GMFD + FD_yeartemp2_GMFD + 
    indp1:FD_yeartemp1_GMFD + indp1:FD_yeartemp2_GMFD,
  data = reg_data
)
#Grid
plot_data <- data.frame(temp1 = seq(-5, 35, by = 1)) %>%
  mutate(temp2 = temp1^2)

# Get income breakpoint (knot)
gpid_var <- paste0("largegpid_", var)
maxInc_var <- paste0("maxInc_largegpid_", var)

ibar <- break_data %>%
  filter(.data[[gpid_var]] == 1) %>%
  summarise(maxInc = max(.data[[maxInc_var]], na.rm = TRUE)) %>%
  pull(maxInc)

# Get average income terciles
avg_incomes <- break_data %>%
  filter(!is.na(avgInc_tgpid)) %>%
  distinct(tpid, tgpid, .keep_all = TRUE) %>%
  arrange(avgInc_tgpid) %>%
  pull(avgInc_tgpid)

tercile_avgs <- quantile(avg_incomes, probs = c(1/6, 3/6, 5/6), na.rm = TRUE)

#Loop
plots <- list()

for (lg in 1:3) {
  subInc <- tercile_avgs[lg]
  deltacut_subInc <- subInc - ibar
  
  line <- rep(0, nrow(plot_data))
  se_vec <- rep(0, nrow(plot_data))
  
  for (k in 1:2) {
    term1 <- paste0("FD_yeartemp", k, "_GMFD")
    term2 <- paste0("indp1:FD_yeartemp", k, "_GMFD")
    
    tempk <- plot_data[[paste0("temp", k)]]
    centered_tempk <- if (k == 1) tempk - 20 else tempk - 400
    
    b1 <- if (term1 %in% names(coef(model_fit))) coef(model_fit)[term1] else 0
    b2 <- if (term2 %in% names(coef(model_fit))) coef(model_fit)[term2] else 0
    
    line <- line + b1 * centered_tempk + b2 * deltacut_subInc * centered_tempk
    
    vcov_mat <- vcov(model_fit)
    v1 <- if (term1 %in% rownames(vcov_mat)) vcov_mat[term1, term1] else 0
    v2 <- if (term2 %in% rownames(vcov_mat)) vcov_mat[term2, term2] else 0
    cov12 <- if (term1 %in% rownames(vcov_mat) && term2 %in% rownames(vcov_mat)) vcov_mat[term1, term2] else 0
    
    se_vec <- se_vec + (centered_tempk^2) * (v1 + (deltacut_subInc^2) * v2 + 2 * deltacut_subInc * cov12)
  }
  
  se <- sqrt(se_vec)
  lower <- line - 1.96 * se
  upper <- line + 1.96 * se
  
  # Prepare dataframe
  df_plot <- plot_data %>%
    mutate(
      yhat = line,
      lower = lower,
      upper = upper
    )
  
  # Create plot
  p <- ggplot(df_plot, aes(x = temp1, y = yhat)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), fill = "lightblue", alpha = 0.4) +
    geom_line(color = "darkblue", size = 1) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
    scale_x_continuous(breaks = seq(-5, 35, 10)) +
    labs(
      subtitle = paste("Income Tercile", lg),
      x = "Temperature (°C)",
      y = "Marginal Effect of Year"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.subtitle = element_text(color = "darkgreen", size = 10),
      axis.text = element_text(size = 9),
      axis.title = element_text(size = 10)
    )
  
  plots[[lg]] <- p
}
#Plot
# Create output folder if needed
if (!dir.exists(output_path)) {
  dir.create(output_path)
}

combined_plot <- wrap_plots(plots, nrow = 1) +
  plot_annotation(
    title = "Marginal Effect of Time on electricity Response Function",
    theme = theme(plot.title = element_text(size = 13, face = "bold"))
  )

ggsave(
  filename = file.path(output_path, "fig_1c_electricity_response.png"),
  plot = combined_plot,
  width = 10, height = 4
)

print("✅ Plot saved successfully.")
