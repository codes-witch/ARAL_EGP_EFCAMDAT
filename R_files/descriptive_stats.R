# Get the levels of all constructs:
constr_lvl_df <- data.frame(feature = all_features)
constr_lvl_df <- add_feature_level(constr_lvl_df)

# Get percentage of construct levels: how many of the constructs belong to each level
get_percentage_of_constr_lvl <- function(level, construct_level_df = constr_lvl_df ){
  num_constr_of_level <- construct_level_df %>%
    filter(feat_level == level) %>%
    count()
  
  return(num_constr_of_level/nrow(constr_lvl_df) * 100)
}

get_percentage_of_constr_lvl("A1")
get_percentage_of_constr_lvl("A2")
get_percentage_of_constr_lvl("B1")
get_percentage_of_constr_lvl("B2")
get_percentage_of_constr_lvl("C1")
get_percentage_of_constr_lvl("C2")

total = 0
for (level in c("A1", "A2", "B1", "B2", "C1", "C2")) {
  total = total + get_percentage_of_constr_lvl(level)
}

zero_feats_level <- data.frame(feature = zero_feats_all_learners)
zero_feats_level <- add_feature_level(zero_feats_level)

egp_zero_feats <- egp_list %>%
  filter(EGP_ID %in% zero_feats_all_learners)

get_percent_constr_zero_freq <- function(construct_level_df=constr_lvl_df, zero_freq_constr = zero_feats_level) {
  cefr_levels = c("A1", "A2", "B1", "B2", "C1", "C2")
  ret_df <- data.frame(level = cefr_levels, percentage_zero_freq = rep(NA, 6))
  for (lvl in cefr_levels) {
    n_constr_level <- construct_level_df %>%
      filter(feat_level == lvl) %>%
      count()
    
    n_zero_feats_lvl <- zero_freq_constr %>%
      filter(feat_level == lvl) %>%
      count()
    
    ret_df[ret_df$level == lvl,]$percentage_zero_freq = n_zero_feats_lvl/n_constr_level
  }
  
  return(ret_df)
}

get_percent_constr_zero_freq()  

# Read the EGP list
egp_list <- read_delim("~/Documents/thesis/egp_list.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)

# Get the non-significant ANOVA constructs
df_non_sign_anova_constructs <- egp_list %>%
  filter(EGP_ID %in% anova_non_sign_constructs) %>%
  select(EGP_ID, Level, Guideword, `Can-do statement`)

kable(df_non_sign_anova_constructs, "latex")

# Number of constructs per EGP level that presented difference at the EGP level
num_per_level_sign <- function(predict_df){
  cefr_levels = c("A2", "B1", "B2", "C1")
  ret_df <- data.frame(EGP_lvl = cefr_levels, not_signific_at_EGP_lvl = rep(NA, 4))
  
  sign_at_level <- predict_df %>%
    filter(signif_at_EGP_level == TRUE) 
  
  for (lvl in cefr_levels){
    n <- sign_at_level %>%
      filter(EGP_level == lvl) %>%
      count()
    
    n_all <- predict_df %>%
      filter(EGP_level == lvl) %>%
      count()
    
    ret_df[ret_df$EGP_lvl == lvl,]$not_signific_at_EGP_lvl = n/n_all * 100
  }
  
  return(ret_df)
}


non_predictable_consecutive <- function(predict_df){
  cefr_levels = c("A2", "B1", "B2", "C1")
  ret_df <- data.frame(EGP_lvl = cefr_levels, non_predict = rep(NA, 4))
  
  
  for (lvl in cefr_levels){
    na <- predict_df %>%
      filter(EGP_level == lvl & is.na(first_sign_p_pred)) %>%
      count()
    
    n_all <- predict_df %>%
      filter(EGP_level == lvl) %>%
      count()
    
    ret_df[ret_df$EGP_lvl == lvl,]$non_predict = na/n_all * 100
  }
  
  return(ret_df)
}

non_predictable_consecutive(t_1side_level_predict)

kable(num_per_level_sign(t_1side_level_predict))

# percentage of constructs assigned to A2
assigned_a2 <- t_1side_level_predict %>%
  filter(first_sign_p_pred == "A2") %>%
  count()
total <- t_1side_level_predict %>%
  count()

assigned_a2 / total * 100

non_predict_consec <- t_1side_level_predict %>%
  filter(is.na(first_sign_p_pred)) %>%
  pull(construct)

df_non_predict_consec <- egp_list %>%
  filter(EGP_ID %in% non_predict_consec)

# Calculate the percentage of constructs of each level that were informative (ANOVA)
calculate_percentage_informative <- function(construct_level_df = constr_lvl_df, informative_constr = anova_sign_constructs, zero_constr = zero_feats_all_learners) {
  cefr_levels = c("A2", "B1", "B2", "C1")
  ret_df <- data.frame(EGP_lvl = cefr_levels, num_informative_constr = rep(NA, 4), percent_informative_level = rep(NA, 4))
  
  #Iterate over CEFR levels, 
  for (lvl in cefr_levels) {
    #count all constructs of the level that are not zero
    num_found_at_level <- construct_level_df %>%
      filter(feat_level == lvl & !(feature %in% zero_feats_all_learners)) %>%
      count()
    
    num_significant_at_level <- construct_level_df %>%
      filter(feat_level == lvl & feature %in% informative_constr) %>%
      count()
    
    ret_df[ret_df$EGP_lvl == lvl,]$num_informative_constr = num_significant_at_level
    ret_df[ret_df$EGP_lvl == lvl,]$percent_informative_level = num_significant_at_level/num_found_at_level * 100
  }
  
  return(ret_df)
  
  
}
kable(calculate_percentage_informative(), format = "latex")

#Get accuracy
n_correct_predictions <- t_1side_level_predict %>%
  filter(EGP_level == first_sign_p_pred)%>%
  count()

n_non_a1 <- t_1side_level_predict %>%
  filter(EGP_level != "A1")%>%
  count()
n_correct_predictions/n_non_a1


mismatch <- t_1side_level_predict %>%
  filter(EGP_level != "A1" & EGP_level != first_sign_p_pred ) %>%
  select(construct, EGP_level, first_sign_p_pred)