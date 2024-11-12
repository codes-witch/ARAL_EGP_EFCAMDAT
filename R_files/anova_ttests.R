# For each student of each level, how many times they used each feature per thousand words
a1_normalized_counts_students_long$text_level = "A1"
a2_normalized_counts_students_long$text_level = "A2"
b1_normalized_counts_students_long$text_level = "B1"
b2_normalized_counts_students_long$text_level = "B2"
c1_normalized_counts_students_long$text_level = "C1"
c2_normalized_counts_students_long$text_level = "C2"

# put all dataframes of students of different levels together
# learnerID, feature, total, text level
normalized_counts_students_long <- bind_rows(a1_normalized_counts_students_long, a2_normalized_counts_students_long, b1_normalized_counts_students_long, b2_normalized_counts_students_long, c1_normalized_counts_students_long, c2_normalized_counts_students_long)

# general mean of each feature
normalized_counts_students_long %>%
  filter(text_level == "A1") %>%
  group_by(feature) %>%
  summarise(avg_freq = mean(total)) %>%
  pull(avg_freq) %>%
  max()

# get only feature, text_level and total variables (column)
normalized_constr_counts <- normalized_counts_students_long %>%
  select(feature, text_level, total)


anova_results <- list()

unique_feats <- unique(normalized_constr_counts$feature)

anova_non_sign_constructs <- c()
anova_sign_constructs <- c()
anova_nan_results_constructs <- c()
zero_feats_all_learners <- c()

# ANOVA. Check which features have significant differences between their frequencies in levels levels.
for (constr in unique_feats) {
  
  # df with only the current construct. Check whether there are no occurrences of the feature.
  feature_dataframe <- normalized_constr_counts %>%
    filter(feature == constr)
  # Keep track of constructs with no occurrences
  if (sum(feature_dataframe$total) == 0){
    print(paste0(constr, " was a 0 feature"))
    zero_feats_all_learners <- append(zero_feats_all_learners, constr)
    
  } else { # Fill in anova result list 
    anova_results[[constr]] <- normalized_constr_counts %>%
      filter(feature == constr) %>%
      aov(total ~ text_level, data = .) %>%
      summary()
    
    p_value = anova_results[[constr]][[1]][[5]][1]
  
    if (is.nan(p_value)){ # sanity check: anova_nan_results_constructs will remain empty because we are discarding all features with no occurrences
      anova_nan_results_constructs <- append(anova_nan_results_constructs, constr)
    } else if (p_value > 0.05) {
      anova_non_sign_constructs <- append(anova_non_sign_constructs, constr)
    } else {
      anova_sign_constructs <- append(anova_sign_constructs, constr)
    }
  }
  
}

normalized_constr_counts <- add_feature_level(normalized_constr_counts) 


# For all significant features,  do T-tests to check for significant differences between the feature level and the previous one. 
levels <- c("A1", "A2", "B1", "B2", "C1", "C2")

# store p-values from t-tests
#t_2side_pvals <- data.frame(matrix(ncol = 6, nrow = length(anova_sign_constructs)))
#names(t_2side_pvals) <- c("feature", "A1_A2", "A2_B1", "B1_B2", "B2_C1", "C1_C2")

t_1side_pvals <- data.frame(matrix(ncol = 6, nrow = length(anova_sign_constructs)))
names(t_1side_pvals) <- c("feature", "A1_A2", "A2_B1", "B1_B2", "B2_C1", "C1_C2")


# For all relevant constructs, do a t-test between all consecutive levels
for (f in 1:length(anova_sign_constructs)) {
    constr <- anova_sign_constructs[f]
    #t_2side_pvals_vec <- c()
    t_1side_pvals_vec <- c()
    for (i in 1:(length(levels) - 1)){
      level1 <- levels[i]
      level2 <- levels[i + 1]
      print(paste0("Construct: ", constr))
      
      appears_in_lev1 <- sum(filter(normalized_constr_counts, text_level == level1, feature == constr)$total) > 0
      appears_in_lev2 <- sum(filter(normalized_constr_counts, text_level == level2, feature == constr)$total) > 0
      
      # Check that the current feature appears in the level before and in the current level
      if (!appears_in_lev1 | !appears_in_lev2) { 
        if (!appears_in_lev1 & !appears_in_lev2){
          print(paste0("Contsruct ", constr, " does not appear in any texts written by ", level1, " or ", level2, " students"))
        } else if (!appears_in_lev1){
          print(paste0("Contsruct ", constr, " does not appear in any texts written by ", level1, " students"))
        } else if (!appears_in_lev2) {
          print(paste0("Contsruct ", constr, " does not appear in any texts written by ", level2, " students"))
        }
        
        #t_2side_pvals_vec <- append(t_2side_pvals_vec, NA) 
        t_1side_pvals_vec <- append(t_1side_pvals_vec, NA)
      } else { # It must appear in BOTH levels to be compared
        
        # do one- and two- sided test
        #t_2side_result <- normalized_constr_counts %>%
        #  filter(feature == constr, text_level %in% c(level1, level2)) %>%
        #  t.test(total ~ text_level, data = .)
        
        t_1side_result <- normalized_constr_counts %>%
          filter(feature == constr, text_level %in% c(level1, level2)) %>%
          t.test(total ~ text_level, data = ., alternative = "less")
        
        #if (t_2side_result$p.value <= 0.05 && t_2side_result$statistic < 0){ # For two-sided check that the direction is the way we want it (aka the greater level has greater frequency)
         # t_2side_pvals_vec <- append(t_2side_pvals_vec, t_2side_result$p.value)
          
          
        #} else {
        #  t_2side_pvals_vec <- append(t_2side_pvals_vec, NA)
        #}
        
        if (t_1side_result$p.value <= 0.05){
          t_1side_pvals_vec <- append(t_1side_pvals_vec, t_1side_result$p.value)
        } else {
          t_1side_pvals_vec <- append(t_1side_pvals_vec, NA)
        }
        
      }
     
    }
    
    # make a row we can append to the result dataframe with the construct in the first column
    #constr_2side_pval_row <- append(as.character(constr), t_2side_pvals_vec)
    constr_1side_pval_row <- append(as.character(constr), t_1side_pvals_vec)
    
    #t_2side_pvals[f,] <- constr_2side_pval_row
    t_1side_pvals[f, ] <- constr_1side_pval_row
    
}

# Make the predictions based on p-values

t_1side_level_predict <- data.frame(feature = anova_sign_constructs) %>%
  add_feature_level() %>%
  rename(EGP_level = feat_level) %>%
  rename(construct = feature) %>%
  mutate(min_p_pred = NA, first_sign_p_pred = NA, signif_at_EGP_level = NA)

#t_2side_level_predict <- data.frame(feature = anova_sign_constructs) %>%
#  add_feature_level() %>%
#  rename(EGP_level = feat_level) %>%
#  rename(construct = feature) %>%
#  mutate(min_p_pred = NA, first_sign_p_pred = NA, signif_at_EGP_level = NA)

# Find first significant diff
t_1side_level_predict <- fillFirstSignPrediction(t_1side_pvals, t_1side_level_predict)
#t_2side_level_predict <- fillFirstSignPrediction(t_2side_pvals, t_2side_level_predict)


# find lowest p-value
t_1side_level_predict <- fillLowestPvalPrediction(t_1side_pvals, t_1side_level_predict)
#t_2side_level_predict <- fillLowestPvalPrediction(t_2side_pvals, t_2side_level_predict)

# fill in TRUE FALSE, is there a significant difference at the level the EGP says?
t_1side_level_predict <- fillIsSigAtEgpLvl(t_1side_pvals, t_1side_level_predict)
#t_2side_level_predict <- fillIsSigAtEgpLvl(t_2side_pvals, t_2side_level_predict)

# Check differences:

model_for_tukey <- normalized_constr_counts %>%
  filter(feature == 1025) %>%
  aov(total ~ text_level, data = .)
TukeyHSD(model_for_tukey)

normalized_constr_counts %>%
  filter(feature == 1025, text_level %in% c("B1", "C2")) %>%
  t.test(total ~ text_level, data = ., alternative = "less")
