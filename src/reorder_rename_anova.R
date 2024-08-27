# reorder_rename_anova.R

# Function to reorder and rename columns
reorder_rename_anova <- function(anova_result) {
  # Convert to data frame
  anova_df <- as.data.frame(anova_result)
  
  # Create a new data frame with reordered and renamed columns
  anova_df <- data.frame(
    term = rownames(anova_df),
    df = anova_df$Df,
    sumsq = anova_df$`Sum Sq`,
    meansq = anova_df$`Sum Sq` / anova_df$Df,
    statistic = anova_df$`F value`,
    p.value = anova_df$`Pr(>F)`
  )
  
  return(anova_df)
}