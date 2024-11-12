# A file for testing things I want to do

# Testing how to do the wilcoxon test
# get level 1 learners and put the totals for the construct in a vector
x <- normalized_students %>%
  filter(feature == 1, text_level == "A1") %>%
  pull(total)
y <- normalized_students %>%
  filter(feature == 1, text_level == "A2")  %>%
  pull(total)

median(outer(y, x, "-"))
wilcox.test(x, y, alternative = "less")

# The second level is higher if the difference in location is negative
temp_wilc_res <- normalized_constr_counts %>%
  filter(feature == 1, text_level %in% c("A1", "A2")) %>%
  wilcox.test(formula = total ~ text_level, data = ., alternative = "less")

temp_wilc_res$p.value
temp_wilc_res 



test_actual= c("A2", "A2", "A2", "B1", "B1", "B1", "B2", "B2", "B2", "C1", "C1", "C2", "C2")
test_pred  = c("A2", "B1", "C1", "C1", "B1", "A2", "A2", "B2", "B1", "C1", "C2", "A2", "B1")


getExactPrecisionAndRecallCefr(test_actual, test_pred)


t_temp_twoside <- normalized_constr_counts %>%
  filter(feature == constr, text_level %in% c("A1", "A2")) %>%
  t.test(total ~ text_level, data = .)
t_temp_twoside$statistic

## Get whether there is a significant difference at the column where the level is that the construct belongs to
temp_pval_df <- t_1side_pvals
temp_predict_df <- t_1side_level_predict
temp_pval_df <- add_feature_level(temp_pval_df)
#iterate through the columns, get the second level. If it's the same as feat_level, then TRUE in the prediction df
for (row_idx in 1:nrow(temp_pval_df)) {
  # get the feature of the row:
  constr_lvl <- temp_pval_df$feat_level[row_idx]
  
  # find the column where the second level is that of the construct
  for (col_idx in 2:6) {
    colname <- colnames(temp_pval_df)[col_idx]
    col_second_lvl <- str_split_i(colname, "_", 2)
    
    if (col_second_lvl == constr_lvl) {
      temp_predict_df[row_idx, "signif_at_EGP_level"] <- !is.na(temp_pval_df[row_idx, col_idx])
    }
    
  }
}


predict_df <- wcx_1side_level_predict
predict_df %>%
  filter(!is.na(signif_at_EGP_level)) %>%
  count()

predict_df %>% 
  filter(!is.na(signif_at_EGP_level) & signif_at_EGP_level == TRUE)


# Get those features that have NAs in t2 


cefr_levels_neighbors <- list("A2"=c("A2", "B1"), "B1" = c("A2", "B1", "B2"), "B2" = c("B1", "B2", "C1") , "C1" = c("B2", "C1", "C2"), "C2" = c("C1", "C2"))
("C1" %in% cefr_levels["A2"][[1]])

for (l in names(cefr_levels)){
  print(l)
}


predicted_repl <- c("B1", "B2", "A2", "B1", "C1")
for (p_idx in 1:length(predicted_repl)) {
  if (predicted_repl[p_idx] %in% cefr_levels_neighbors["A2"][[1]]) {
    predicted_repl[p_idx] <- "A2"
  }
}


# check whether something is one-off or less a given level
actual_level = "A2"
predicted_level = "B2"


normalized_counts_students_long %>%
  filter(feature == 110) %>%
  group_by(text_level) %>%
  summarise(median = median(total), mean=mean(total))




# Calculate summary statistics
min_value <- min(no)
max_value <- max(normalized_constr_counts$total)
q1 <- quantile(normalized_constr_counts$total, 0.25)
q3 <- quantile(normalized_constr_counts$total, 0.75)
iqr <- q3 - q1

# Calculate the range for the y-axis
lower_limit <- min(min_value, q1 - 1.5 * iqr)
upper_limit <- max(max_value, q3 + 1.5 * iqr)

# Create the boxplot
boxplot(normalized_constr_counts$total, ylim = c(lower_limit, upper_limit))

normalized_counts_students_long %>%
  get_boxplot_construct(173)
 
normalized_counts_students_long %>%
  get_lineplot(173)


df_filtered <- normalized_counts_students_long %>%
  filter(feature == 935)
q3s = c()

