setwd("/home/daniela/Documents/thesis/R_files/")
load("./.RData")
source("/home/daniela/Documents/thesis/R_files/functions.R")
library(ggplot2)
library(gridExtra)
library("dplyr")
library("stringr")
library(tidyr)
library(tools)
library(knitr)
load("/home/daniela/Documents/corpus_annotation_tp/R_files/ef2_short_lvl.RData")
all_features <- readLines("all_features.txt")

# Create a df for the avg at each level
feat_level_avg_students <- data.frame(matrix(ncol = length(all_features), nrow = 6))
colnames(feat_level_avg_students) <- all_features
rownames(feat_level_avg_students) <- c("a1", "a2", "b1", "b2", "c1", "c2")


a2_feat_count_per_student_short <- get_feat_count_per_student(all_features, 24, "../text_annotation/data/output_filtered_07032024/a2/csv/", learner_ids = learners_a2_full, FALSE)
a2_feat_count_per_student_long <- make_long_feats_df(cbind("learnerID" = rownames(a2_feat_count_per_student_short), a2_feat_count_per_student_short), "learnerID", "total")

a2_learner_wc <- get_learner_word_count_df(learners_a2_full, "../text_annotation/data/output_filtered_07032024/a2/text/")



# Get students that completed n A2 texts
a2_feat_count_per_student_short <- get_feat_count_per_student(all_features, 24, "../text_annotation/data/output_filtered_07032024/a2/csv/", learner_ids = learners_a2_full, FALSE)
a2_feat_count_per_student_long <- make_long_feats_df(cbind("learnerID" = rownames(a2_feat_count_per_student_short), a2_feat_count_per_student_short), "learnerID", "total")

a2_learner_wc <- get_learner_word_count_df(learners_a2_full, "../text_annotation/data/output_filtered_07032024/a2/text/")

# Getthe features with no occurrences
zero_feats_learner_a2 <- colnames(a2_feat_count_per_student_short[,colSums(a2_feat_count_per_student_short) == 0])

# normalize per 1k words
a2_normalized_counts_students_short <- a2_feat_count_per_student_short / a2_learner_wc$word_count * 1000 
a2_normalized_counts_students_long <- make_long_feats_df(cbind("learnerID" = rownames(a2_feat_count_per_student_short), a2_normalized_counts_students_short), "learnerID", "total")

# Fill in df with the mean of all features
feat_level_avg_students["a2",] <- colMeans(a2_normalized_counts_students_short)

# -----------------------------

# Get students that completed n B1 texts
b1_feat_count_per_student_short <- get_feat_count_per_student(all_features, 24, "../text_annotation/data/output_filtered_07032024/b1/csv/", learner_ids = learners_b1_full, FALSE)
b1_feat_count_per_student_long <- make_long_feats_df(cbind("learnerID" = rownames(b1_feat_count_per_student_short), b1_feat_count_per_student_short), "learnerID", "total")
b1_learner_wc <- get_learner_word_count_df(learners_b1_full, "../text_annotation/data/output_filtered_07032024/b1/text/")

# Getthe features with no occurrences
zero_feats_learner_b1 <- colnames(b1_feat_count_per_student_short[,colSums(b1_feat_count_per_student_short) == 0])

# normalize per 1k words
b1_normalized_counts_students_short <- b1_feat_count_per_student_short / b1_learner_wc$word_count * 1000 
b1_normalized_counts_students_long <- make_long_feats_df(cbind("learnerID" = rownames(b1_feat_count_per_student_short), b1_normalized_counts_students_short), "learnerID", "total")

# Fill in df with the mean of all features
feat_level_avg_students["b1",] <- colMeans(b1_normalized_counts_students_short)

# _______________________________

# Get students that completed n B2 texts
b2_feat_count_per_student_short <- get_feat_count_per_student(all_features, 24, "../text_annotation/data/output_filtered_07032024/b2/csv/", learner_ids = learners_b2_full, FALSE)
b2_feat_count_per_student_long <- make_long_feats_df(cbind("learnerID" = rownames(b2_feat_count_per_student_short), b2_feat_count_per_student_short), "learnerID", "total")
b2_learner_wc <- get_learner_word_count_df(learners_b2_full, "../text_annotation/data/output_filtered_07032024/b2/text/")

# Getthe features with no occurrences
zero_feats_learner_b2 <- colnames(b2_feat_count_per_student_short[,colSums(b2_feat_count_per_student_short) == 0])

# normalize per 1k words
b2_normalized_counts_students_short <- b2_feat_count_per_student_short / b2_learner_wc$word_count * 1000 
b2_normalized_counts_students_long <- make_long_feats_df(cbind("learnerID" = rownames(b2_feat_count_per_student_short), b2_normalized_counts_students_short), "learnerID", "total")

# Fill in df with the mean of all features
feat_level_avg_students["b2",] <- colMeans(b2_normalized_counts_students_short)

# ____________________________________
# Get students that completed n C1 texts
c1_feat_count_per_student_short <- get_feat_count_per_student(all_features, 24, "../text_annotation/data/output_filtered_07032024/c1/csv/", learner_ids = learners_c1_full, FALSE)
c1_feat_count_per_student_long <- make_long_feats_df(cbind("learnerID" = rownames(c1_feat_count_per_student_short), c1_feat_count_per_student_short), "learnerID", "total")
c1_learner_wc <- get_learner_word_count_df(learners_c1_full, "../text_annotation/data/output_filtered_07032024/c1/text/")

# Getthe features with no occurrences
zero_feats_learner_c1 <- colnames(c1_feat_count_per_student_short[,colSums(c1_feat_count_per_student_short) == 0])

# normalize per 1k words
c1_normalized_counts_students_short <- c1_feat_count_per_student_short / c1_learner_wc$word_count * 1000 
c1_normalized_counts_students_long <- make_long_feats_df(cbind("learnerID" = rownames(c1_feat_count_per_student_short), c1_normalized_counts_students_short), "learnerID", "total")

# Fill in df with the mean of all features
feat_level_avg_students["c1",] <- colMeans(c1_normalized_counts_students_short)
 
# _____________________________________
# Get students that completed n C2 texts
c2_feat_count_per_student_short <- get_feat_count_per_student(all_features, 8, "../text_annotation/data/output_filtered_07032024/c2/csv/", learner_ids = learners_c2_full, FALSE)
c2_feat_count_per_student_long <- make_long_feats_df(cbind("learnerID" = rownames(c2_feat_count_per_student_short), c2_feat_count_per_student_short), "learnerID", "total")
c2_learner_wc <- get_learner_word_count_df(learners_c2_full, "../text_annotation/data/output_filtered_07032024/c2/text/")

# Getthe features with no occurrences
zero_feats_learner_c2 <- colnames(c2_feat_count_per_student_short[,colSums(c2_feat_count_per_student_short) == 0])

# normalize per 1k words
c2_normalized_counts_students_short <- c2_feat_count_per_student_short / c2_learner_wc$word_count * 1000 
c2_normalized_counts_students_long <- make_long_feats_df(cbind("learnerID" = rownames(c2_feat_count_per_student_short), c2_normalized_counts_students_short), "learnerID", "total")

# Fill in df with the mean of all features
feat_level_avg_students["c2",] <- colMeans(c2_normalized_counts_students_short)

feat_level_avg_students_long <- make_long_feats_df(cbind("level" =  c("a1", "a2", "b1", "b2", "c1", "c2"), feat_level_avg_students), "level", "total")



# ######################################## FOR PLOTS #####################################
a1_for_plot <- get_lvl_feats("a1", feat_level_avg_students_long)
plot_percentages_level(a1_for_plot, "A1 per student")
plot_percentages_level(a1_for_plot[a1_for_plot$total < 200 & !(a1_for_plot$feature %in% zero_feats_learner_a1) ,], "A1 per student")

a2_for_plot <- get_lvl_feats("a2", feat_level_avg_students_long)
plot_percentages_level(a2_for_plot[a2_for_plot$total< 40 & !(a2_for_plot$feature %in% zero_feats_learner_a2),], "A2 per student")

b1_for_plot <- get_lvl_feats("b1", feat_level_avg_students_long)
View(b1_for_plot[b1_for_plot$total > 20,])
plot_percentages_level(b1_for_plot[!(b1_for_plot$feature %in% zero_feats_learner_b1),], "B1 per student")

b2_for_plot <- get_lvl_feats("b2", feat_level_avg_students_long)
plot_percentages_level(b2_for_plot[!(b2_for_plot$feature %in% zero_feats_learner_b2),], "B2 per student")

c1_for_plot <- get_lvl_feats("c1", feat_level_avg_students_long)
plot_percentages_level(c1_for_plot[!(c1_for_plot$feature %in% zero_feats_learner_c1) & c1_for_plot$total < 6,], "C1 per student")

c2_for_plot <- get_lvl_feats("c2", feat_level_avg_students_long)
plot_percentages_level(c2_for_plot[!(c2_for_plot$feature %in% zero_feats_learner_c2),], "C2 per student")

# ----------------------------- CLUSTERING -----------------------------

# ------------------------ a1 ------------------------
a1_for_cluster <- feat_level_avg_students[, names(feat_level_avg_students) %in% as.character(1:109)]
a1_for_cluster <- t(a1_for_cluster)
fviz_nbclust(a1_for_cluster, kmeans, method="wss") # I'll choose 4
kmeans_out_a1_student <- kmeans(a1_for_cluster, centers = 4, nstart = 100)
# Visualize cluster
fviz_cluster(list(data=a1_for_cluster, cluster= kmeans_out_a1_student$cluster), main = "A1 constructs per student")
# Get the means of the clusters
a1_cluster_means <- aggregate(a1_for_cluster, by=list(cluster=kmeans_out_a1_student$cluster), mean)
colnames(a1_cluster_means) <- c("cluster", "a1", "a2", "b1", "b2", "c1", "c2")

# Visualize them 
a1_cluster_means_long <- pivot_longer(a1_cluster_means, cols = !cluster, values_to = "value", names_to = "level")
a1_cluster_means_long$cluster <- as.factor(a1_cluster_means_long$cluster)
plot_cluster_means(a1_cluster_means_long, "Clustering of A1 features (no scaling)")


# ------------------------ a2 ------------------------
a2_for_cluster <- feat_level_avg_students[, names(feat_level_avg_students) %in% as.character(110:397) & names(feat_level_avg_students) != "275"]
a2_for_cluster <- t(a2_for_cluster)
fviz_nbclust(a2_for_cluster, kmeans, method="wss") # I'll choose 4
kmeans_out_a2_student <- kmeans(a2_for_cluster, centers = 4, nstart = 100)
# Visualize cluster
fviz_cluster(list(data=a2_for_cluster, cluster= kmeans_out_a2_student$cluster), main = "A2 constructs per student")
# Get the means of the clusters
a2_cluster_means <- aggregate(a2_for_cluster, by=list(cluster=kmeans_out_a2_student$cluster), mean)
colnames(a2_cluster_means) <- c("cluster", "a1", "a2", "b1", "b2", "c1", "c2")

# Visualize them 
a2_cluster_means_long <- pivot_longer(a2_cluster_means, cols = !cluster, values_to = "value", names_to = "level")
a2_cluster_means_long$cluster <- as.factor(a2_cluster_means_long$cluster)
plot_cluster_means(a2_cluster_means_long, "Centroids of A2 construct clustering)")

# ------------------------ b1 ------------------------
b1_for_cluster <- feat_level_avg_students[, names(feat_level_avg_students) %in% as.character(401:734)]
b1_for_cluster <- t(b1_for_cluster)
fviz_nbclust(b1_for_cluster, kmeans, method="wss") 
kmeans_out_b1_student <- kmeans(b1_for_cluster, centers = 3, nstart = 100)
# Visualize cluster
fviz_cluster(list(data=b1_for_cluster, cluster= kmeans_out_b1_student$cluster), main = "B1 constructs per student")
# Get the means of the clusters
b1_cluster_means <- aggregate(b1_for_cluster, by=list(cluster=kmeans_out_b1_student$cluster), mean)
colnames(b1_cluster_means) <- c("cluster", "a1", "a2", "b1", "b2", "c1", "c2")

# Visualize them 
b1_cluster_means_long <- pivot_longer(b1_cluster_means, cols = !cluster, values_to = "value", names_to = "level")
b1_cluster_means_long$cluster <- as.factor(b1_cluster_means_long$cluster)
plot_cluster_means(b1_cluster_means_long, "Centroids of B1 construct clustering)")

# ------------------------ b2 ------------------------
b2_for_cluster <- feat_level_avg_students[, names(feat_level_avg_students) %in% as.character(739:977)]
b2_for_cluster <- delete_column(b2_for_cluster, "785")
b2_for_cluster <- t(b2_for_cluster)
fviz_nbclust(b2_for_cluster, kmeans, method="wss") 
kmeans_out_b2_student <- kmeans(b2_for_cluster, centers = 4, nstart = 100)
# Visualize cluster
fviz_cluster(list(data=b2_for_cluster, cluster= kmeans_out_b2_student$cluster), main = "B2 constructs per student")
# Get the means of the clusters
b2_cluster_means <- aggregate(b2_for_cluster, by=list(cluster=kmeans_out_b2_student$cluster), mean)
colnames(b2_cluster_means) <- c("cluster", "a1", "a2", "b1", "b2", "c1", "c2")

# Visualize them 
b2_cluster_means_long <- pivot_longer(b2_cluster_means, cols = !cluster, values_to = "value", names_to = "level")
b2_cluster_means_long$cluster <- as.factor(b2_cluster_means_long$cluster)
plot_cluster_means(b2_cluster_means_long, "Centroids of B2 construct clustering)")

# ------------------------ c1 ------------------------
c1_for_cluster <- feat_level_avg_students[, names(feat_level_avg_students) %in% as.character(982:1105) & names(feat_level_avg_students) != "1057"]
c1_for_cluster <- t(c1_for_cluster)
fviz_nbclust(c1_for_cluster, kmeans, method="wss") 
kmeans_out_c1_student <- kmeans(c1_for_cluster, centers = 5, nstart = 100)
# Visualize cluster
fviz_cluster(list(data=c1_for_cluster, cluster= kmeans_out_c1_student$cluster), main = "C1 constructs per student")
# Get the means of the clusters
c1_cluster_means <- aggregate(c1_for_cluster, by=list(cluster=kmeans_out_c1_student$cluster), mean)
colnames(c1_cluster_means) <- c("cluster", "a1", "a2", "b1", "b2", "c1", "c2")

# Visualize them 
c1_cluster_means_long <- pivot_longer(c1_cluster_means, cols = !cluster, values_to = "value", names_to = "level")
c1_cluster_means_long$cluster <- as.factor(c1_cluster_means_long$cluster)
plot_cluster_means(c1_cluster_means_long, "Centroids of C1 construct clustering)")

# ------------------------ c2 ------------------------
c2_for_cluster <- feat_level_avg_students[, names(feat_level_avg_students) %in% as.character(1111:1203)]
c2_for_cluster <- t(c2_for_cluster)
fviz_nbclust(c2_for_cluster, kmeans, method="wss") 
kmeans_out_c2_student <- kmeans(c2_for_cluster, centers = 4, nstart = 100)
# Visualize cluster
fviz_cluster(list(data=c2_for_cluster, cluster= kmeans_out_c2_student$cluster), main = "C2 constructs per student")
# Get the means of the clusters
c2_cluster_means <- aggregate(c2_for_cluster, by=list(cluster=kmeans_out_c2_student$cluster), mean)
colnames(c2_cluster_means) <- c("cluster", "a1", "a2", "b1", "b2", "c1", "c2")

# Visualize them 
c2_cluster_means_long <- pivot_longer(c2_cluster_means, cols = !cluster, values_to = "value", names_to = "level")
c2_cluster_means_long$cluster <- as.factor(c2_cluster_means_long$cluster)
plot_cluster_means(c2_cluster_means_long, "Centroids of C2 construct clustering)")



# Getthe features with no occurrences
non_zero_constr_a1_txt <- colnames(a1_feat_count_per_student_short[,colSums(a1_feat_count_per_student_short) != 0])
non_zero_constr_a2_txt <- colnames(a2_feat_count_per_student_short[,colSums(a2_feat_count_per_student_short) != 0])
non_zero_constr_b1_txt <- colnames(b1_feat_count_per_student_short[,colSums(b1_feat_count_per_student_short) != 0])
non_zero_constr_b2_txt <- colnames(b2_feat_count_per_student_short[,colSums(b2_feat_count_per_student_short) != 0])
non_zero_constr_c1_txt <- colnames(c1_feat_count_per_student_short[,colSums(c1_feat_count_per_student_short) != 0])
non_zero_constr_c2_txt <- colnames(c2_feat_count_per_student_short[,colSums(c2_feat_count_per_student_short) != 0])
