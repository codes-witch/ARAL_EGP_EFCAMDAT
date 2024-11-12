library(rstatix)
library(ggpubr)

# Kruskal-Wallis: some constructs might not show significant differences across test levels
kw_results <- list()
kw_non_sign_constructs <- c()
kw_sign_constructs <- c()
kw_nan_results_constructs <- c()


for (constr in unique_feats) {
  # df with only the current construct. Check whether there are no occurrences of the feature.
  feature_dataframe <- normalized_constr_counts %>%
    filter(feature == constr)
  # Keep track of constructs with no occurrences
  if (constr %in% zero_feats_all_learners ){
    print(paste0(constr, " was a 0 feature"))
    
  } else { # Fill in kw result list 
    kw_results[[constr]] <- normalized_constr_counts %>%
      filter(feature == constr) %>%
      kruskal.test(total ~ text_level, data = .)
    
    p_value = kw_results[[constr]]$p.value
    
    if (is.nan(p_value)){ # sanity check: kw_nan_results_constructs will remain empty because we are discarding all features with no occurrences
      kw_nan_results_constructs <- append(kw_nan_results_constructs, constr)
    } else if (p_value > 0.05) {
      kw_non_sign_constructs <- append(kw_non_sign_constructs, constr)
    } else {
      kw_sign_constructs <- append(kw_sign_constructs, constr)
    }
  }
  
}



# For all significant features,  do Wilcoxon Rank Sum tests to check for significant differences between the feature level and the previous one. 
levels <- c("A1", "A2", "B1", "B2", "C1", "C2")

# store p-values from wilcoxon tests
#wcx_2side_pvals <- data.frame(matrix(ncol = 6, nrow = length(kw_sign_constructs)))
#names(wcx_2side_pvals) <- c("feature", "A1_A2", "A2_B1", "B1_B2", "B2_C1", "C1_C2")

wcx_1side_pvals <-data.frame(matrix(ncol = 6, nrow = length(kw_sign_constructs)))
names(wcx_1side_pvals) <- c("feature", "A1_A2", "A2_B1", "B1_B2", "B2_C1", "C1_C2")

# For all relevant constructs, do a wilcoxon-test between all consecutive levels
for (f in 1:length(kw_sign_constructs)) {
  constr <- kw_sign_constructs[f]
  
  # build the rows for the p-values column by column
  #wcx_2side_pvals_vec <- c()
  wcx_1side_pvals_vec <- c()
  
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
      
      #wcx_2side_pvals_vec <- append(wcx_2side_pvals_vec, NA) 
      wcx_1side_pvals_vec <- append(wcx_1side_pvals_vec, NA)
    } else { # It must appear in BOTH levels to be compared
      
      # do one- and two-sided tests
      #wcx_2side_result <- normalized_constr_counts %>%
      #  filter(feature == constr, text_level %in% c(level1, level2)) %>%
      #  wilcox.test(total ~ text_level, data = ., conf.int = TRUE)
      
      wcx_1side_result <- normalized_constr_counts %>%
        filter(feature == constr, text_level %in% c(level1, level2)) %>%
        wilcox.test(total ~ text_level, data = ., alternative = "less")
      
      # For two-sided test check that the direction is the one we want:
      #if (wcx_2side_result$p.value <= 0.05 && wcx_2side_result$estimate < 0){
       # wcx_2side_pvals_vec <- append(wcx_2side_pvals_vec, wcx_2side_result$p.value)
      #} else {
      #  wcx_2side_pvals_vec <- append(wcx_2side_pvals_vec, NA)
      #}
    
      if (wcx_1side_result$p.value <= 0.05) {
        wcx_1side_pvals_vec <- append(wcx_1side_pvals_vec, wcx_1side_result$p.value)
      } else {
        wcx_1side_pvals_vec <- append(wcx_1side_pvals_vec, NA)
      }
    }
  }
  
  #constr_2side_pval_row <- append(as.character(constr), wcx_2side_pvals_vec)
  constr_1side_pval_row <- append(as.character(constr), wcx_1side_pvals_vec)
 
  #wcx_2side_pvals[f,] <- constr_2side_pval_row
  wcx_1side_pvals[f, ] <- constr_1side_pval_row
  
}


#wcx_2side_level_predict <- data.frame(feature = kw_sign_constructs) %>%
 # add_feature_level() %>%
  #rename(EGP_level = feat_level) %>%
#  rename(construct = feature) %>%
#  mutate(min_p_pred = NA, first_sign_p_pred = NA, signif_at_EGP_level = NA)

wcx_1side_level_predict <-  data.frame(feature = kw_sign_constructs) %>%
  add_feature_level() %>%
  rename(EGP_level = feat_level) %>%
  rename(construct = feature) %>%
  mutate(min_p_pred = NA, first_sign_p_pred = NA, signif_at_EGP_level = NA)
# Find first significant diff
wcx_1side_level_predict <- fillFirstSignPrediction(wcx_1side_pvals, wcx_1side_level_predict)
#wcx_2side_level_predict <- fillFirstSignPrediction(wcx_2side_pvals, wcx_2side_level_predict)


# find lowest p-value
wcx_1side_level_predict <- fillLowestPvalPrediction(wcx_1side_pvals, wcx_1side_level_predict)
#wcx_2side_level_predict <- fillLowestPvalPrediction(wcx_2side_pvals, wcx_2side_level_predict)

# fill in TRUE FALSE, is there a significant difference at the level the EGP says?
wcx_1side_level_predict <- fillIsSigAtEgpLvl(wcx_1side_pvals, wcx_1side_level_predict)
#wcx_2side_level_predict <- fillIsSigAtEgpLvl(wcx_2side_pvals, wcx_2side_level_predict)

