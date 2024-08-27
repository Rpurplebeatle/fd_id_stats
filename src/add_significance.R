# add_significance.R

# Convert the models to tidy data frames and add significance levels
add_significance <- function(model) {
  model <- as.data.frame(model)
  model %>%
    mutate(significance = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      TRUE ~ ""
    ))
}
