library(data.table)
library(lfe)
library(ggplot2)
library(grid)
library(gridExtra)
library(readr)
df<- read_csv("Desktop/GMFD_TINV_clim_regsort.csv")

setDT(df)
df[, region_i_f := as.factor(region_i)]
df[, subregion_year := paste(subregionid, year, sep = "_")]


elec  <- df[product == "electricity"   & !is.na(FD_load_pc)]
other <- df[product == "other_energy"  & !is.na(FD_load_pc)]


temp_vars <- c()
for (j in 1:2) {
  for (i in 1:10) {
    temp_vars <- c(temp_vars, paste0("FD_I", i, "temp", j, "_GMFD"))
  }
}
formula_str <- paste(
  "FD_load_pc ~",
  paste(temp_vars, collapse = " + "),
  "+ FD_precip1_GMFD + FD_precip2_GMFD | region_i + subregion_year | 0 | region_i"
)
formula_a <- as.formula(formula_str)


reg_elec  <- felm(formula_a, data = elec)
reg_other <- felm(formula_a, data = other)

coef_elec  <- coef(reg_elec)
coef_other <- coef(reg_other)
vcov_elec  <- vcov(reg_elec)
vcov_other <- vcov(reg_other)


predict_response <- function(T_val, coefs, vcov_mat, decile) {
  dT1 <- T_val - 20
  dT2 <- T_val^2 - 400
  coef_names <- paste0("FD_I", decile, "temp", 1:2, "_GMFD")
  if (!all(coef_names %in% names(coefs))) return(list(response = NA, se = NA))
  beta <- coefs[coef_names]
  response <- sum(beta * c(dT1, dT2))
  gradient <- c(dT1, dT2)
  sub_vcov <- vcov_mat[coef_names, coef_names]
  se_response <- sqrt(t(gradient) %*% sub_vcov %*% gradient)
  return(list(response = response, se = as.numeric(se_response)))
}


temps <- seq(-5, 35, 0.5)
results_a <- data.table()

for (d in 1:10) {
  for (T_val in temps) {
    res_e <- predict_response(T_val, coef_elec, vcov_elec, d)
    res_o <- predict_response(T_val, coef_other, vcov_other, d)
    results_a <- rbind(
      results_a,
      data.table(
        decile = d,
        temp = T_val,
        response_elec  = res_e$response,
        se_elec        = res_e$se,
        response_other = res_o$response,
        se_other       = res_o$se
      )
    )
  }
}

results_a[, ci_lo_elec  := response_elec  - 1.96 * se_elec]
results_a[, ci_hi_elec  := response_elec  + 1.96 * se_elec]
results_a[, ci_lo_other := response_other - 1.96 * se_other]
results_a[, ci_hi_other := response_other + 1.96 * se_other]


theme_fig1 <- theme_bw() +
  theme(
    panel.grid    = element_blank(),
    axis.title    = element_text(size = 8),
    axis.text     = element_text(size = 7),
    plot.margin   = margin(2, 2, 2, 2),
    panel.border  = element_rect(linewidth = 0.5),
    axis.ticks    = element_line(linewidth = 0.3)
  )


elec_plots <- list()
for (d in 1:10) {
  df_d <- results_a[decile == d]
  p <- ggplot(df_d) +
    geom_hline(yintercept = 0, color = "gray50", linewidth = 0.3) +
    geom_ribbon(aes(temp, ymin = ci_lo_elec, ymax = ci_hi_elec),
                fill = "#4393C3", alpha = 0.3) +
    geom_line(aes(temp, response_elec), color = "#2166AC", linewidth = 0.5) +
    scale_x_continuous(breaks = c(-5, 15, 35), limits = c(-5, 35)) +
    scale_y_continuous(limits = c(-0.01, 0.02)) +
    labs(x = NULL, y = NULL) +
    theme_fig1
  elec_plots[[d]] <- p
}

row1_elec <- arrangeGrob(grobs = elec_plots, nrow = 1)


other_plots <- list()
for (d in 1:10) {
  df_d <- results_a[decile == d]
  p <- ggplot(df_d) +
    geom_hline(yintercept = 0, color = "gray50", linewidth = 0.3) +
    geom_ribbon(aes(temp, ymin = ci_lo_other, ymax = ci_hi_other),
                fill = "#F4A582", alpha = 0.3) +
    geom_line(aes(temp, response_other), color = "#D6604D", linewidth = 0.5) +
    scale_x_continuous(breaks = c(-5, 15, 35), limits = c(-5, 35)) +
    scale_y_continuous(limits = c(-0.05, 0.1)) +
    labs(x = NULL, y = NULL) +
    theme_fig1
  other_plots[[d]] <- p
}

row1_other <- arrangeGrob(grobs = other_plots, nrow = 1)


panel_a <- arrangeGrob(row1_elec, row1_other, nrow = 2)

grid.newpage()
grid.draw(panel_a)


p_elec_8  <- elec_plots[[8]]
p_elec_10 <- elec_plots[[10]]
p_other_5 <- other_plots[[5]]


library(gridExtra)
library(grid)

combined_1a <- arrangeGrob(
  p_elec_8, p_elec_10, p_other_5,
  nrow = 1
)


caption_text <- "Notes: Electricity (Deciles 8 and 10) and Other fuels (Decile 5) temperature response curves."

combined_1a_with_caption <- arrangeGrob(
  combined_1a,
  bottom = textGrob(
    caption_text,
    x = 0, hjust = 0,
    gp = gpar(fontsize = 9)
  )
)


output_path <- "~/Desktop/1a.png"

ggsave(
  filename = output_path,
  plot     = combined_1a_with_caption,
  width    = 12,
  height   = 4.5,
  dpi      = 300
)


grid.newpage()
grid.draw(combined_1a_with_caption)

