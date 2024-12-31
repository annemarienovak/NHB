library(ggplot2)

# Time points for the X axis
time_points <- c('Baseline', '1 month', '3 months', '6 months', '12 months')

# Updated Means and SEMs for each measure
data_list <- list(
  Burnout = list(
    control_means = c(37.97, 37.49, 37.03, 36.29, 36.11),
    intervention_means = c(38.27, 33.58, 33.98, 34.15, 33.56),
    control_sem = c(0.260, 0.267, 0.254, 0.220, 0.204),
    intervention_sem = c(0.300, 0.270, 0.256, 0.233, 0.247),
    y_min = 32,
    y_max = 40,
    ylabel = "Burnout total scores (s.e.m.)"
  ),
  Stress = list(
    control_means = c(17.35, 16.89, 16.21, 15.31, 15.74),
    intervention_means = c(18.79, 13.72, 15.02, 14.70, 14.25),
    control_sem = c(0.263, 0.251, 0.260, 0.252, 0.235),
    intervention_sem = c(0.252, 0.241, 0.242, 0.248, 0.253),
    y_min = 12,
    y_max = 20,
    ylabel = "Stress total scores (s.e.m.)"
  ),
  PERMA_Total = list(
    control_means = c(7.80, 7.89, 7.96, 8.08, 8.11),
    intervention_means = c(6.35, 7.39, 7.25, 7.29, 7.28),
    control_sem = c(0.062, 0.059, 0.058, 0.053, 0.054),
    intervention_sem = c(0.054, 0.050, 0.050, 0.051, 0.054),
    y_min = 5.8,
    y_max = 8.5,
    ylabel = "PERMA Total scores (s.e.m.)"
  ),
  PERMA_Health = list(
    control_means = c(7.36, 7.47, 7.48, 7.64, 7.71),
    intervention_means = c(6.28, 7.08, 7.02, 7.14, 7.08),
    control_sem = c(0.081, 0.076, 0.076, 0.073, 0.073),
    intervention_sem = c(0.082, 0.075, 0.074, 0.074, 0.073),
    y_min = 5.8,
    y_max = 8.5,
    ylabel = "PERMA Health total scores (s.e.m.)"
  ),
  Self_Efficacy = list(
    control_means = c(30.61, 30.85, 31.30, 31.33, 31.28),
    intervention_means = c(31.31, 34.24, 33.78, 33.82, 34.03),
    control_sem = c(0.188, 0.184, 0.180, 0.173, 0.176),
    intervention_sem = c(0.170, 0.162, 0.170, 0.169, 0.172),
    y_min = 29,
    y_max = 35,
    ylabel = "Self-efficacy total scores (s.e.m.)"
  ),
  Resilience = list(
    control_means = c(27.39, 27.60, 27.87, 28.29, 28.26),
    intervention_means = c(28.27, 32.41, 31.84, 31.94, 32.40),
    control_sem = c(0.238, 0.239, 0.219, 0.216, 0.214),
    intervention_sem = c(0.228, 0.199, 0.210, 0.208, 0.215),
    y_min = 26,
    y_max = 33,
    ylabel = "Resilience total scores (s.e.m.)"
  ),
  Gratitude = list(
    control_means = c(32.64, 32.95, 33.59, 33.92, 33.84),
    intervention_means = c(36.77, 38.53, 37.92, 38.38, 38.36),
    control_sem = c(0.249, 0.242, 0.237, 0.231, 0.224),
    intervention_sem = c(0.202, 0.179, 0.187, 0.188, 0.187),
    y_min = 32,
    y_max = 40,
    ylabel = "Gratitude total scores (s.e.m.)"
  )
)

# Plot function
plot_graph <- function(data, y_min, y_max, ylabel) {
  ggplot(data, aes(x = Time, y = Mean, group = Group, color = Group, shape = Group)) +
    geom_line(size = 1) +
    geom_point(size = 3) +
    geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM), width = 0.2, size = 0.5) +
    scale_color_manual(values = c("Control" = "#4f83bf", "Intervention" = "#bf0101")) +
    scale_shape_manual(values = c("Control" = 16, "Intervention" = 17)) +
    labs(y = ylabel, x = "Time Points") +
    theme_minimal() +
    theme(
      legend.position = "top",
      legend.title = element_blank()
    ) +
    coord_cartesian(ylim = c(y_min, y_max))
}

# Iterate through each measure
for (measure in names(data_list)) {
  measure_data <- data_list[[measure]]
  
  
  df <- data.frame(
    Time = factor(rep(time_points, 2), levels = time_points),
    Group = rep(c("Control", "Intervention"), each = length(time_points)),
    Mean = c(measure_data$control_means, measure_data$intervention_means),
    SEM = c(measure_data$control_sem, measure_data$intervention_sem)
  )
  
  # Plot the graph
  plot <- plot_graph(df, measure_data$y_min, measure_data$y_max, measure_data$ylabel)
  print(plot)
}


