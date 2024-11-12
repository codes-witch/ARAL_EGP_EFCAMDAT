# For plotting constructs

#Typical A2 construct
normalized_constr_counts %>%
  get_lineplot(110)

line_plot <- normalized_counts_students_long %>%
  get_lineplot(110)

normalized_counts_students_long %>%
  get_boxplot_construct(110)

normalized_counts_students_long %>%
  filter(feature == 110) %>%
  group_by(text_level) %>%
  summarise(median = median(total), mean=mean(total))


# Save boxplots in a file 
for (construct in unique_non_zero) {
  
  
  bp <- normalized_counts_students_long %>%
    get_boxplot_construct(construct)
  
  ggsave(paste0("plots/", constr_lvl_df[constr_lvl_df$feature == construct, "feat_level"], "_", construct, "_boxplot.png"), width= 5.40, height = 4.20, plot = bp)
  
  lp <- normalized_constr_counts %>%
    get_lineplot(construct)
  
  ggsave(paste0("plots/", constr_lvl_df[constr_lvl_df$feature == construct, "feat_level"], "_", construct, "_lineplot.png"), width= 5.40, height = 4.20, plot = lp)
}


a1_texts_constr_means <- data.frame(feature = c(), text_level = c(), mean= c())
unique_non_zero <- setdiff(unique_feats, zero_feats_all_learners)
for (feat in unique_non_zero){

a1_mean <- normalized_constr_counts %>% 
  group_by(feature, text_level) %>%
  filter(feature == feat && text_level == "A1") %>%
  summarise(meadian = median(total))
  

a1_texts_constr_means <- rbind(a1_texts_constr_means, a1_mean)
}

a1_texts_constr_medians <- a1_texts_constr_means

a1_texts_constr_medians %>%
  filter(meadian > 0) %>%
  pull(meadian) %>%
  max()

a1_texts_constr_medians %>%
  filter(meadian >= 0.0001)

normalized_counts_students_long %>%
  get_boxplot_construct(935)

df_filtered <- normalized_counts_students_long %>%
  filter(feature == constr_id)

lp <- normalized_constr_counts %>%
  get_lineplot(323)

ggsave(paste0("plots/", t_1side_level_predict[t_1side_level_predict$construct == construct, "EGP_level"], "_", construct, "_lineplot.png"), width= 5.40, height = 4.20, plot = lp)

normalized_constr_counts %>% 
  group_by(text_level)  %>%
  summarise(median = median(total), mean = mean(total))
