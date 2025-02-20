# -----------------------------------------------------------------------
# ------------------------------ DATA PREP ------------------------------
# -----------------------------------------------------------------------

# Grabs the text from the ef dataframe and put
# it in .txt files in the provided directory
ef_text_2_txt <- function(dataframe, directory) {
  # Create the directory to store the text files (if it doesn't exist)
  dir.create(directory, showWarnings = FALSE)

  # Only get relevant rows (top 3 highest/lowest units of CEFR level)
  dataframe <- dataframe %>%
    filter(!is.na(combined_cefr))

  # Iterate over each row in the df
  for (i in 1:nrow(dataframe)) {
    cefr_level <- dataframe$cefr_level[i]
    learner_id <- dataframe$learnerID[i]
    ef_level <- dataframe$ef_level[i]
    unit <- dataframe$unit[i]
    combined_level <- dataframe$combined_cefr[i]

    # Create a subdir for each level (if it doesn't exist) and for start and end
    lvl_dir <- paste0(directory, combined_level)

    dir.create(lvl_dir, showWarnings = FALSE, recursive = TRUE)

    # filename for each row. Structure: ID_unit_EFlevel_learnerId.txt
    filename <- paste0(lvl_dir, "/", dataframe$writingID[i], "_unit", dataframe$unit[i], "_eflvl", ef_level, "_learner", learner_id, ".txt") # nolint: line_length_linter
    file_conn <- file(filename, open = "w")

    # Write the text content to the file
    writeLines(as.character(dataframe$original[i]), con = file_conn)

    close(file_conn)
  }
}



# To add cefr levels when we have EF levels
add_cefr_from_ef_levels <- function(dataframe) {
  dataframe <- dataframe %>%
    mutate(cefr_level = case_when(
      ef_level <= 3 ~ "A1",
      ef_level <= 6 ~ "A2",
      ef_level <= 9 ~ "B1",
      ef_level <= 12 ~ "B2",
      ef_level <= 15 ~ "C1",
      TRUE ~ "C2"
    ), .after = ef_level)
  # CEFR levels are factors
  dataframe$cefr_level <- as.factor(dataframe$cefr_level)
  return(dataframe)
}

# Adds information about the combined CEFR levels that a text belongs to.
# Specifically targets the first n units of a CEFR level.
# (e.g. A0_A1 for low-A1, B1_B2 for high B1 and low B2)
# n is the number of units of each level taken into account.
# In the case of A0_A1, all n units come from the start of A1
# In other cases, half the n units come from each of the CEFR levels in
# the combination. N should not be higher than 4
add_combined_level <- function(dataframe, n = 3) {
  dataframe <- dataframe %>%
    mutate(combined_cefr = case_when(
      cefr_level == "A1" & ef_level == 1 & unit <= 2 * n ~ "A0_A1",
      (cefr_level == "A1" & ef_level == 3 & unit > 8 - n) | (cefr_level == "A2" & ef_level == 4 & unit <= n) ~ "A1_A2", # nolint: line_length_linter.
      (cefr_level == "A2" & ef_level == 6 & unit > 8 - n) | (cefr_level == "B1" & ef_level == 7 & unit <= n) ~ "A2_B1", # nolint: line_length_linter.
      (cefr_level == "B1" & ef_level == 9 & unit > 8 - n) | (cefr_level == "B2" & ef_level == 10 & unit <= n) ~ "B1_B2", # nolint: line_length_linter.
      (cefr_level == "B2" & ef_level == 12 & unit > 8 - n) | (cefr_level == "C1" & ef_level == 13 & unit <= n) ~ "B2_C1", # nolint: line_length_linter.
      (cefr_level == "C1" & ef_level == 15 & unit > 8 - n) | (cefr_level == "C2" & ef_level == 16 & unit <= n) ~ "C1_C2", # nolint: line_length_linter.
      TRUE ~ NA_character_
    ))
}

# To add levels to a dataframe based on the construct as defined by the EGP
add_feature_level <- function(dataframe) {
  dataframe %>% mutate(feat_level = case_when(
    as.numeric(feature) < 110 ~ "A1",
    as.numeric(feature) < 401 ~ "A2",
    as.numeric(feature) < 739 ~ "B1",
    as.numeric(feature) < 982 ~ "B2",
    as.numeric(feature) < 1111 ~ "C1",
    TRUE ~ "C2" # default value if no conditions match
  ))
}


# -------------------------------------------------------------------
# ------------------------------ COUNTS------------------------------
# -------------------------------------------------------------------
# Function to count words in all text files in a directory
count_words_in_directory <- function(directory_path, pattern = NULL) {
  if (is.null(pattern)) {
    pattern <- "\\.txt$"
  }

  total_word_count <- 0
  file_list <- list.files(directory_path, pattern = pattern, full.names = TRUE, recursive = TRUE)

  for (file_path in file_list) {
    # Read the text file
    text <- readLines(file_path, warn = FALSE)
    # Combine lines into a single text (optional)
    text <- paste(text, collapse = " ")
    # Count words in the text
    word_count <- tokenizers::count_words(text)
    total_word_count <- total_word_count + word_count
  }
  return(total_word_count)
}

# to get a learner's word count at a given level
get_learner_word_count_at_level <- function(learner_id, directory_path) {
  return(count_words_in_directory(directory_path, pattern = paste0("learner", learner_id, "\\.txt$")))
}

# get a df of word counts per learner
get_learner_word_count_df <- function(learner_ids, directory_path) {
  learner_wc <- data.frame(matrix(ncol = 1, nrow = length(learner_ids)))

  rownames(learner_wc) <- learner_ids
  colnames(learner_wc) <- c("word_count")

  for (id in learner_ids) {
    learner_wc[id, "word_count"] <- get_learner_word_count_at_level(id, directory_path)
  }

  return(learner_wc)
}

# For getting the frequency per hundred words with which each student used a construct
get_feats_normalized_students <- function(feat_count_df, word_count_df, make_long = TRUE) {
  short_normalized <- feat_count_df / word_count_df$word_count * 100

  if (make_long) {
    make_long_feats_df(cbind("learnerID" = rownames(feat_count_df), short_normalized), exclude_col = "learnerID", "total")
  }
}


# For making a dataframe long.
make_long_feats_df <- function(dataframe, exclude_col, values_colname) {
  dataframe <- pivot_longer(dataframe, cols = -{{ exclude_col }}, names_to = "feature", values_to = values_colname)
  dataframe$feature <- as.numeric(dataframe$feature)

  return(dataframe)
}

# Each row in feat_count_per_student corresponds to a student and each column corresponds to a feature.
get_feat_count_per_student <- function(directory_path, make_long = TRUE, learner_ids) {
  # define dataframe:
  feat_count_per_student <- data.frame(matrix(ncol = length(all_features), nrow = length(learner_ids)))

  rownames(feat_count_per_student) <- learner_ids
  colnames(feat_count_per_student) <- all_features
  # make all NANs 0
  feat_count_per_student[] <- 0

  for (id in learner_ids) {
    # get all files by student at that level
    file_list <- list.files(directory_path, pattern = paste0("learner", id, "\\.csv$"), full.names = TRUE, recursive = TRUE)
    print(paste("File list length for learner", id, ": ", length(file_list)))
    for (file_path in file_list) {
      # Get the features in the file:
      csv_file <- read.csv(file_path)

      # Get the frequency of all features found in the text
      frequencies <- table(csv_file$constructID)

      for (feature in names(frequencies)) {
        feat_count_per_student[id, feature] <- feat_count_per_student[id, feature] + frequencies[[feature]]
      }
    }
  }


  if (make_long) {
    feat_count_per_student$learnerID <- rownames(feat_count_per_student)
    feat_count_per_student <- make_long_feats_df(feat_count_per_student, "learnerID", "total")
  }
  return(feat_count_per_student)
}

get_language_frequencies <- function(directory_path) {
  # Get the language from the output file names
  file_list <- list.files(directory_path, pattern = paste0("\\.csv$"), full.names = FALSE, recursive = TRUE)
  match_langs <- regexpr("_[a-z]{2}_", file_list)
  langs <- regmatches(file_list, match_langs)
  freqs <- as.data.frame(table(langs))
  return(freqs)
}



#--------------------------------------------------------
#-------------------------PLOTS--------------------------
#--------------------------------------------------------

get_boxplot_construct <- function(df, constr_id, title = NULL) {

  df_filtered <- df %>%
    filter(feature == constr_id)

  min_val <- df_filtered %>%
    pull(total) %>%
    min()

  max_val <- df_filtered %>%
    pull(total) %>%
    max()

  q1s = c()
  q3s = c()

  for (text_lvl in c("A0_A1","A1_A2", "A2_B1", "B1_B2", "B2_C1", "C1_C2")){
    q1 <- quantile(df_filtered$total[df_filtered$text_level == text_lvl], 0.25)
    print(paste("Q1 for", text_lvl, "=", q1, "Construct", constr_id))
    q1s = append(q1s, q1)

    q3 <- quantile(df_filtered$total[df_filtered$text_level == text_lvl], 0.75)
    print(paste("Q3 for", text_lvl, "=", q3, "Construct", constr_id))
    q3s = append(q3s, q3)
  }
  q1 <- min(q1s)
  q3 <- max(q3s)

  iqr <- q3 - q1

  lower_limit <- 0
  upper_limit <-q3 + 1.75 * iqr

  if(upper_limit == 0) {
    upper_limit = max_val
  }
  print(paste("Upper limit", upper_limit ))
  plot <- df_filtered %>%
    group_by(learnerID, text_level) %>%
    ggplot(aes(x = text_level, y = total, fill = text_level)) +
    geom_boxplot() +
    labs(x = "Text Level", y = "Normalized frequency", fill = "Text Level") +
    ggtitle(title) +
    coord_cartesian(ylim = c(lower_limit, upper_limit))


  return(plot)

}

get_lineplot <- function(df, constr_id, title = NULL, ylim_vector = NULL) {
  plt <- df %>%
    group_by(feature, text_level) %>%
    filter(feature == constr_id) %>%
    summarise(mean = mean(total)) %>%
    ggplot(aes(x = text_level, y = mean, color = as.factor(feature), group = feature)) +
    geom_line() +

    labs(x = "Text Level", y = "Mean frequency of construct", col = "Construct ID") +
    # xlab("Levels") +
    # ylab("Mean frequency of construct") +
    scale_x_discrete(
      labels = c("pre-A1", "A1", "A2", "B1", "B2", "C1"),
      breaks = c("A0_A1", "A1_A2", "A2_B1", "B1_B2", "B2_C1", "C1_C2")
    ) +
    ggtitle(title)

  return(plt)
}

# For getting a dataframe with features of only one level
get_level_feats <- function(level, dataframe_long) {
  level <- tolower(level)
  dataframe_long$feature <- as.numeric(dataframe_long$feature)
  if (level == "a1") {
    level_df <- dataframe_long[dataframe_long$feature <= 109, ]
  } else if (level == "a2") {
    level_df <- dataframe_long[dataframe_long$feature >= 110 & dataframe_long$feature <= 397, ]
  } else if (level == "b1") {
    level_df <- dataframe_long[dataframe_long$feature >= 401 & dataframe_long$feature <= 734, ]
  } else if (level == "b2") {
    level_df <- dataframe_long[dataframe_long$feature >= 739 & dataframe_long$feature <= 977, ]
  } else if (level == "c1") {
    level_df <- dataframe_long[dataframe_long$feature >= 982 & dataframe_long$feature <= 1105, ]
  } else if (level == "c2") {
    level_df <- dataframe_long[dataframe_long$feature >= 1111, ]
  } else {
    print("INVALID LEVEL!")
  }

  return(level_df)
}

# ----------------------------------------------------
# ------------ CONSTRUCT-LEVEL ASSIGNMENT ------------
# ----------------------------------------------------

# Fills in the first_sign_p_pred column of the prediction dataframe in with the level prediction.
fill_first_signif_pred <- function(pvals_df, predict_df) {
  for (r in 1:nrow(pvals_df)) {
    print(pvals_df[r, 1])
    for (c in 2:6) { # For each column in the p values for each level
      if (!is.na(pvals_df[r, c])) { # only if the comparison in the tests was successful (if )
        print(pvals_df[r, c])
        predict_df$first_sign_p_pred[r] <- colnames(pvals_df)[c] # Predicted the name of the level
        break
      }
    }
  }

  return(predict_df)
}


get_exact_precision_recall_f1 <- function(actual, predicted) {
  if (length(actual) != length(predicted)) {
    print("Actual and predicted must be same length")
  }

  cefr_levels <- c("A1", "A2", "B1", "B2", "C1")
  result <- data.frame(level = cefr_levels, precision = rep(NA, 5), recall = rep(NA, 5), f1 = rep(NA, 5))

  total_tp <- 0
  total_fp <- 0
  total_fn <- 0
  for (l in cefr_levels) {
    tp <- 0
    fp <- 0
    fn <- 0

    for (i in 1:length(actual)) {
      if (actual[i] == l && predicted[i] == l) {
        # "New TP"
        tp <- tp + 1
        total_tp <- total_tp + tp
      } else if (actual[i] == l) {
        # "New FN"
        fn <- fn + 1
        total_fn <- total_fn + fn
      } else if (predicted[i] == l) {
        # "New FP"
        fp <- fp + 1
        total_fp <- total_fp + fp
      }
    }
    precision <- NA
    recall <- NA

    if (fp + tp != 0) {
      precision <- tp / (fp + tp)
      result$precision[result$level == l] <- precision
    }

    if (tp + fn != 0) {
      recall <- tp / (fn + tp)
      result$recall[result$level == l] <- recall
    }


    if (!is.na(precision) && !is.na(recall)) {
      f1 <- 2 * (precision * recall) / (precision + recall)
      result$f1[result$level == l] <- f1
    }
  }

  macro_avg <- data.frame(precision = mean(result$precision, na.rm = TRUE), recall = mean(result$recall, na.rm = TRUE), f1 = mean(result$f1, na.rm = TRUE))
  micro_avg <- data.frame(precision = total_tp / (total_tp + total_fp), recall = total_tp / (total_tp + total_fn), f1 = total_tp / (total_tp + ((total_fn + total_fp) / 2)))
  return(list("result" = result, "macro_avg" = macro_avg, "micro_avg" = micro_avg))
}


get_neigh_prec_rec_f1 <- function(actual, predicted) {
  if (length(actual) != length(predicted)) {
    print("Actual and predicted must be same length")
  }

  cefr_levels <-  c("A1", "A2", "B1", "B2", "C1")
  result <- data.frame(level = cefr_levels, precision = rep(NA, 5), recall = rep(NA, 5), f1 = rep(NA, 5))

  total_tp <- 0
  total_fp <- 0
  total_fn <- 0
  for (l in cefr_levels) {
    tp <- 0
    fp <- 0
    fn <- 0

    for (i in 1:length(actual)) {
      if ((actual[i] == l | predicted[i] == l) && (is_neighbor_level(actual[i], predicted[i]))) {
        tp <- tp + 1
        total_tp <- total_tp + tp
      } else if (actual[i] == l) {
        # New FN
        fn <- fn + 1
        total_fn <- total_fn + fn
      } else if (predicted[i] == l) {
        # New FP
        fp <- fp + 1
        total_fp <- total_fp + fp
      }
    }
    precision <- NA
    recall <- NA

    if (fp + tp != 0) {
      precision <- tp / (fp + tp)
      result$precision[result$level == l] <- precision
    }

    if (tp + fn != 0) {
      recall <- tp / (fn + tp)
      result$recall[result$level == l] <- recall
    }


    if (!is.na(precision) && !is.na(recall)) {
      f1 <- 2 * (precision * recall) / (precision + recall)
      result$f1[result$level == l] <- f1
    }
  }
  macro_avg <- data.frame(precision = mean(result$precision, na.rm = TRUE), recall = mean(result$recall, na.rm = TRUE), f1 = mean(result$f1, na.rm = TRUE))
  micro_avg <- data.frame(precision = total_tp / (total_tp + total_fp), recall = total_tp / (total_tp + total_fn), f1 = total_tp / (total_tp + ((total_fn + total_fp) / 2)))
  return(list("result" = result, "macro_avg" = macro_avg, "micro_avg" = micro_avg))
}


# fill with TRUE or FALSE, whether the construct is significant at the level it belongs to in the EGP
fill_is_sig_at_lvl <- function(pval_df, predict_df) {
  pval_df <- add_feature_level(pval_df)
  # iterate through the columns, get the second level. If it's the same as feat_level, then TRUE in the prediction df
  for (row_idx in 1:nrow(pval_df)) {
    # get the feature of the row:
    constr_lvl <- pval_df$feat_level[row_idx]

    # find the column where the level is that of the construct
    for (col_idx in 2:6) {
      col_second_lvl <- colnames(pval_df)[col_idx]

      if (col_second_lvl == constr_lvl) {
        predict_df[row_idx, "signif_at_EGP_level"] <- !is.na(pval_df[row_idx, col_idx])
      }
    }
  }

  return(predict_df)
}

# Get the accuracy with regard to whether it is significant at the EGP level (ignoring operationalization)
accuracy_signif_at_level <- function(predict_df) {
  total_booleans <- predict_df %>%
    filter(!is.na(signif_at_EGP_level)) %>%
    count()

  total_trues <- predict_df %>%
    filter(!is.na(signif_at_EGP_level) & signif_at_EGP_level == TRUE) %>%
    count()

  return(total_trues / total_booleans)
}

is_neighbor_level <- function(actual_level, predicted_level) {
  cefr_levels_neighbors <- list("A1" = c("A2"), "A2" = c("A1", "A2", "B1"), "B1" = c("A2", "B1", "B2"), "B2" = c("B1", "B2", "C1"), "C1" = c("B2", "C1", "C2"), "C2" = c("C1", "C2"))
  return(predicted_level %in% cefr_levels_neighbors[actual_level][[1]])
}


# ----------------------------------------------------
# ----------------------- MISC -----------------------
# ----------------------------------------------------
get_text_author_df <- function(ef_dataframe, learner_ids, level) {
  return(ef_dataframe[ef_dataframe$cefr_level == level & ef_dataframe$learnerID %in% learner_ids, c("id", "learnerID")])
}
