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
    filename <- paste0(lvl_dir, "/", dataframe$writingID[i], "_unit", dataframe$unit[i], "_eflvl", ef_level, "_learner", learner_id,   ".txt") # nolint: line_length_linter
    file_conn <- file(filename, open = "w")

    # Write the text content to the file
    writeLines(as.character(dataframe$original[i]), con = file_conn)

    close(file_conn)
  }
}

# Get the EF level at which a cefr level starts
get_start_ef_level <- function(cefr_level) {
  start <- switch(cefr_level,
    "a1" <- 1,
    "a2" <- 4,
    "b1" <- 7,
    "b2" <- 10,
    "c1" <- 13,
    "c2" <- 16,
    "INVALID CEFR LEVEL"
  )

  return(start)
}

# Get the EF level at which a CEFR level ends
get_end_ef_level <- function(cefr_level) {
  if (cefr_level == "c2") {
    return(16)
  } else {
    return(get_start_ef_level(cefr_level) + 2)
  }
}

get_start_units <- function(cefr_level, n = 6) {
  if (n > 8) {
    print("Cannot use more than 8 units")
    return()
  }

  return(as.list(seq(1:n)))
}

get_end_units <- function(cefr_level, n = 6) {
  # start at 8 and iterate backwards
  ret_val <- as.list(seq(from = 8, by = -1, length.out = n))
  return(ret_val)
}


delete_column <- function(dataframe, colname) {
  dataframe <- dataframe[, !(colnames(dataframe) %in% colname)]
  return(dataframe)
}

# To add cefr levels when we have EF levels
add_cefr_from_ef_levels <- function(dataframe){
  dataframe <- dataframe %>%
    mutate(cefr_level = case_when(
      ef_level <= 3  ~ "a1",
      ef_level <= 6  ~ "a2",
      ef_level <= 9  ~ "b1",
      ef_level <= 12 ~ "b2",
      ef_level <= 15 ~ "c1",
      TRUE           ~ "c2"
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
      cefr_level == "a1" & ef_level == 1 & unit <= 2 * n ~ "A0_A1",
      (cefr_level == "a1" & ef_level == 3 & unit > 8 - n) | (cefr_level == "a2" & ef_level == 4 & unit <= n) ~ "A1_A2", #nolint: line_length_linter.
      (cefr_level == "a2" & ef_level == 6 & unit > 8 - n) | (cefr_level == "b1" & ef_level == 7 & unit <= n) ~ "A2_B1", # nolint: line_length_linter.
      (cefr_level == "b1" & ef_level == 9 & unit > 8 - n) | (cefr_level == "b2" & ef_level == 10 & unit <= n) ~ "B1_B2", # nolint: line_length_linter.
      (cefr_level == "b2" & ef_level == 12 & unit > 8 - n) | (cefr_level == "c1" & ef_level == 13 & unit <= n) ~ "B2_C1", # nolint: line_length_linter.
      (cefr_level == "c1" & ef_level == 15 & unit > 8 - n) | (cefr_level == "c2" & ef_level == 16 & unit <= n) ~ "C1_C2", # nolint: line_length_linter.
      TRUE ~ NA_character_
    ))
}

# To add levels to a dataframe based on the construct as defined by the EGP
add_feature_level <- function(dataframe){
  dataframe %>% mutate(feat_level = case_when(
         as.numeric(feature) < 110 ~ "A1",
         as.numeric(feature) < 401 ~ "A2",
         as.numeric(feature) < 739 ~ "B1",
         as.numeric(feature) < 982 ~ "B2",
         as.numeric(feature) < 1111 ~ "C1",
         TRUE ~ "C2" # default value if no conditions match
    ))
}

put_learner_id_in_filenames <- function(directory_path, ef_df) {
  # Get the list of files in the directory
  file_list <- list.files(directory_path, full.names = TRUE, recursive = TRUE)
  
  # Iterate through each file
  for (file_path in file_list) {
    # Extract the filename without extension
    file_name <- tools::file_path_sans_ext(basename(file_path))
    extension <- paste0(".", tools::file_ext())
    print(file_name)
    
    if (grepl("learner", file_name)) {
      next
    }
    
    # Get textID
    match <- regmatches(file_path, regexpr("\\d+_", file_path))
    
    # Delete everything including and after thefirst underscore
    textID <- sub("_.*", "", match[1])
    
    # Find the corresponding learnerId in ef2 dataset
    learner_id <- ef_df$learnerID[ef_df$id == textID]
    
    new_file_name <- paste0(file_name, "_learner", learner_id, extension)
    new_file_path <- file.path(directory_path, new_file_name)
    file.rename(file_path, new_file_path)
    print(paste0("New file path: ", new_file_path))
  }
  
  
}
  

# For renaming files to the pattern <file_id>_<unit>_<iso_code>.txt
put_units_and_lang_in_filenames <-function(directory_path, dataframe) {
    print("in function")
    # Get the list of files in the directory
    file_list <- list.files(directory_path, pattern = "\\.txt$", full.names = TRUE, recursive = TRUE)
    
    # Iterate through each file
    for (file_path in file_list) {
      # Extract the filename without extension
      file_name <- tools::file_path_sans_ext(basename(file_path))
      print(file_name)
      
      # Find the corresponding unit in ef2 dataset
      unit <- dataframe$unit[dataframe$id == file_name]
      lang_iso <- dataframe$lang_iso[dataframe$id == file_name]
      if (is.null(lang_iso) || lang_iso == "") {
        lang_iso <- "xx"
      } 
      print(paste0("Unit: ", unit))
      print(paste0("language ISO: ", lang_iso))
      
      # Rename the file
      new_file_name <- paste0(file_name, "_", unit, "_", lang_iso, ".txt")
      new_file_path <- file.path(directory_path, new_file_name)
      file.rename(file_path, new_file_path)
      print(paste0("New file path: ", new_file_path))
    }
    
    
}
  


# For renaming the filenames to always have access to unit and id
# Dataframe is the reference df where we get the units from
put_units_in_filenames <- function(directory_path, dataframe){
  
  # Get the list of files in the directory
  file_list <- list.files(directory_path, pattern = "\\.txt$", full.names = TRUE)
  
  # Iterate through each file
  for (file_path in file_list) {
    # Extract the filename without extension
    file_name <- tools::file_path_sans_ext(basename(file_path))
    print(file_name)
    
    # Don't add unit if it already has it
    if (grepl("_", file_name)){
      next
    }
    
    # Find the corresponding unit in ef2 dataset
    unit <- dataframe$unit[dataframe$id == file_name]
    print(unit)
    
    # Rename the file
    new_file_name <- paste0(unit, "_", file_name, ".txt")
    new_file_path <- file.path(directory_path, new_file_name)
    file.rename(file_path, new_file_path)
  }
  
  
}

add_newline_to_files <- function(directory_path) {
  # Get a list of all files in the directory and its subdirectories
  file_list <- list.files(directory_path, recursive = TRUE, full.names = TRUE)
  
  # Loop through each file
  for (file_path in file_list) {
    print(file_path)
    # Check if the file is a text file
    if (endsWith(file_path, ".txt")) {
      # Read the contents of the file
      file_contents <- readLines(file_path)
      
      # Add a new line at the end of the file contents
      file_contents <- c(file_contents)
      
      # Write the updated contents back to the file
      writeLines(file_contents, file_path)
      
      # Print a message for each file processed
      print("New line added")
    }
  }
}



# -------------------------------------------------------------------
# ------------------------------ COUNTS------------------------------
# -------------------------------------------------------------------

# Function to count words in a text
count_words <- function(text) {
  # Use regular expressions to split text into words at both whitespace and punctuation boundaries
  words <- str_split(text, "[\\s.,!?;:]+|(?<=[.,!?:;])(?=\\w)", simplify = TRUE)
  # Filter out empty strings
  words <- words[words != ""]
  # Print the words (optional)
  print(words)
  print(length(words))
  return(length(words))
}

# Function to count words in all text files in a directory
count_words_in_directory <- function(directory_path, pattern=NULL) {
  
  if (is.null(pattern)){
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
    word_count <- count_words(text)
    print(text)
    total_word_count <- total_word_count + word_count
  }
  return(total_word_count)
}

# to get a learner's word count at a given level
get_learner_word_count_at_level <- function(learner_id, directory_path) {
  return (count_words_in_directory(directory_path, pattern = paste0("learner", learner_id, "\\.txt$")))
}

get_learner_word_count_df <- function(learner_ids, directory_path){
  learner_wc <- data.frame(matrix(ncol = 1, nrow = length(learner_ids)))
  
  rownames(learner_wc) <- learner_ids
  colnames(learner_wc) <- c("word_count")
  
  for (id in learner_ids) {
    learner_wc[id, "word_count"] <- get_learner_word_count_at_level(id, directory_path)
  }
  
  return (learner_wc)
}

# Use this for calculating the percentages
count_texts_per_unit <- function(directory_path) {
  file_list <- list.files(directory_path, recursive = TRUE, pattern = "\\.txt$")
  unit_count_df = data.frame(matrix(nrow = 128, ncol = 1))
  unit_count_df[] <- 0
  colnames(unit_count_df) <- c("n_texts")
  for (file_path in file_list){
    print(file_path)
    # get the file unit and id
    file_unit <- as.integer(str_extract(file_path, "\\d+(?=_)"))
    file_id <- as.integer(str_extract(file_path, "(?<=_)\\d+"))
    
    # add one to the unit counts
    unit_count_df$n_texts[file_unit] <- unit_count_df$n_texts[file_unit] + 1 
    
  }
  return(unit_count_df)
}


# 6 levels x 2 cols (level, counts). Use for percentages
count_texts_per_level <- function(directory_path, lang=NULL){
  file_list <- list.files(directory_path, recursive = TRUE, pattern = "\\.csv$")
  level_count_df <- data.frame(level = c("a1", "a2", "b1", "b2", "c1", "c2"), n_texts = 0)
  
  for (file_path in file_list){
    print(file_path)
    # get the file level, unit, id and language. 
    # The file naming convention is <level>/<id>_<unit>_<lang_iso>.txt
    level <- str_extract(file_path, "[abc]\\d")
    unit <- str_extract(file_path, "(?<=_)\\d+") 
    file_id <- str_extract(file_path, "(?<=/)\\d+")
    lang_iso <- str_extract(file_path, "[a-z]{2}")
    print(paste0("level: ", level))
    print(paste0("file_id: ", file_id))
    print(paste0("unit: ", unit))
    print(paste0("lang_iso: ", lang_iso))
    
    # find the row index based on the level
    row_index <- which(level_count_df$level == level)
    if (is.null(lang)) {
      print("No lang param passed. Counting all texts.")
      level_count_df$n_texts[row_index] <- level_count_df$n_texts[row_index] +1
    } else if (tolower(lang) == lang_iso) {
      print("Languages are equal. Counting this file.")
      level_count_df$n_texts[row_index] <- level_count_df$n_texts[row_index] +1
    } else {
      print("Languages are not equal. Skipping this")
    }
    
  }
  return(level_count_df)
  
}

# File path is the output file
features_in_text_file <- function(file_path){
  return(unique(readLines(file_path)))
}

unique_features_in_csv <- function(file_path) {
  csv_file <- read.csv(file_path)
  unique_feats <- as.character(unique(csv_file$constructID)) 
  print(unique_feats)
  #remove empty character
  return(unique_feats)
}

# For making a dataframe long. 
make_long_feats_df <- function(dataframe, exclude_col, values_colname){
  dataframe <- pivot_longer(dataframe, cols = -{{exclude_col}}, names_to = "feature", values_to = values_colname)
  dataframe$feature = as.numeric(dataframe$feature)
  
  return(dataframe)
}

# unit x features in short form. Also possible to make long.
# Each row in feats_in_units corresponds to a unit and each column corresponds to a feature. 
# The values in the cells are how many texts contain feature C in unit R 
get_feats_in_units <- function(all_features, directory_path, make_long = TRUE) {
  file_list <- list.files(directory_path, pattern = "\\.txt$", full.names = TRUE, recursive = TRUE)
  
  feats_in_units <- data.frame(matrix(ncol = 659, nrow = 128))
  # make all NANs 0
  feats_in_units[is.na(feats_in_units)] <- 0
  colnames(feats_in_units) <- all_features
  
  for (file_path in file_list) {
    # Get the unique features in the text
    unique_feats = features_in_text_file(file_path)
    # get unit text belongs to
    unit <- as.integer(str_extract(file_path, "\\d+(?=_)"))
    
    print(file_path)
    print(unique_feats)
    
    # Per feat in text, add one to the counts
    for (feat in unique_feats) {
      feats_in_units[unit, feat] <- feats_in_units[unit, feat] + 1
    }
  }
  feats_in_units <- cbind(unit = c(1:nrow(feats_in_units)), feats_in_units)
  
  if (make_long){
    feats_in_units <- make_long_feats_df(feats_in_units, "unit", "total")
  }
  
  return(feats_in_units)
}

# Each row in feats_in_level corresponds to a level and each column corresponds to a feature. 
# Counts features presence per text (aka the first approach) 
# DEPRECATED. USE get_feat_presence_in_texts
get_feats_in_levels <- function(all_features, directory_path, make_long = TRUE, percentage = TRUE) {
  file_list <- list.files(directory_path, pattern = "\\.txt$", full.names = TRUE, recursive = TRUE)
  
  # define dataframe:
  feats_in_levels <- data.frame(matrix(ncol = length(all_features), nrow = 6))
  # make all NANs 0
  feats_in_levels[] <- 0
  
  colnames(feats_in_levels) <- all_features
  print(file_list)
  
  # get number of files per level
  level_counts <- count_texts_per_level(directory_path)
  
  for (file_path in file_list) {
    print(file_path)
    # Get the unique features in the text
    unique_feats = features_in_text_file(file_path)
    # Get the level
    level <- str_extract(file_path, "[abc]\\d")
    if (level == "a1"){
      level_num = 1
    } else if (level == "a2") {
      level_num = 2
    } else if (level == "b1") {
      level_num = 3
    } else if (level == "b2") {
      level_num = 4
    } else if (level == "c1") {
      level_num = 5
    } else if (level == "c2") {
      level_num = 6
    }
    
    for (feat in unique_feats) {
      feats_in_levels[level_num , feat] <- feats_in_levels[level_num , feat]  + 1
    }
    
    
  }
  
  if (percentage){
    feats_in_levels <- feats_in_levels / level_counts$n_texts
  }
  
  # add the level names
  feats_in_levels = cbind(level = c("a1", "a2", "b1", "b2", "c1", "c2"), feats_in_levels)
  
  if (make_long){
    feats_in_levels <- make_long_feats_df(feats_in_levels, "level", "total")
  }
  return(feats_in_levels)
  
}

# Each row in feats_in_level corresponds to a level and each column corresponds to a feature. 
# Counts features presence per text (aka the first approach) 
get_feat_presence_in_texts <- function(all_features, directory_path, make_long = TRUE, percentage = TRUE) {
  file_list <- list.files(directory_path, pattern = "\\.csv$", full.names = TRUE, recursive = TRUE)
  
  # define dataframe:
  feats_in_levels <- data.frame(matrix(ncol = length(all_features), nrow = 6))
  # make all NANs 0
  feats_in_levels[] <- 0
  
  colnames(feats_in_levels) <- all_features
  print(file_list)
  
  # get number of files per level
  level_counts <- count_texts_per_level(directory_path)
  
  for (file_path in file_list) {
    print(file_path)
    # Get the unique features in the text
    unique_feats = unique_features_in_csv(file_path)
    print(unique_feats)
    # Get the level
    level <- str_extract(file_path, "[abc]\\d")
    if (level == "a1"){
      level_num = 1
    } else if (level == "a2") {
      level_num = 2
    } else if (level == "b1") {
      level_num = 3
    } else if (level == "b2") {
      level_num = 4
    } else if (level == "c1") {
      level_num = 5
    } else if (level == "c2") {
      level_num = 6
    }
    
    for (feat in unique_feats) {
      print(paste0("level_num: ", level_num, "feat: ", feat))
      feats_in_levels[level_num, feat] <- feats_in_levels[level_num , feat]  + 1
    }
    
    
  }
  
  if (percentage){
    feats_in_levels <- feats_in_levels / level_counts$n_texts
  }
  
  # add the level names
  feats_in_levels = cbind(level = c("a1", "a2", "b1", "b2", "c1", "c2"), feats_in_levels)
  
  if (make_long){
    feats_in_levels <- make_long_feats_df(feats_in_levels, "level", "total")
  }
  return(feats_in_levels)
  
}


# Each row in feat_count_per_level corresponds to a level and each column corresponds to a feature. 
get_feat_count_per_level <- function(all_features, directory_path, make_long = TRUE) {
  file_list <- list.files(directory_path, pattern = "\\.csv$", full.names = TRUE, recursive = TRUE)
  
  # define dataframe:
  feat_count_per_level <- data.frame(matrix(ncol = length(all_features), nrow = 6))
  # make all NANs 0
  feat_count_per_level[] <- 0
  
  colnames(feat_count_per_level) <- all_features
  
  for (file_path in file_list) {
    print(file_path)
    # Get the features in the file:
    csv_file <- read.csv(file_path)
    
    # Get the frequency of all features found in the text
    frequencies <- table(csv_file$constructID)
    
    # Get the level
    level <- str_extract(file_path, "[abc]\\d")
    if (level == "a1"){
      level_num = 1
    } else if (level == "a2") {
      level_num = 2
    } else if (level == "b1") {
      level_num = 3
    } else if (level == "b2") {
      level_num = 4
    } else if (level == "c1") {
      level_num = 5
    } else if (level == "c2") {
      level_num = 6
    }
    
    for (feature in names(frequencies)) {
      feat_count_per_level[level_num , feature] <- feat_count_per_level[level_num , feature] + frequencies[[feature]]
    }
    
    
  }
  
  
  # add the level names
  feat_count_per_level = cbind(level = c("a1", "a2", "b1", "b2", "c1", "c2"), feat_count_per_level)
  
  if (make_long){
    feat_count_per_level <- make_long_feats_df(feat_count_per_level, "level", "total")
  }
  return(feat_count_per_level)
  
}

# Each row in feat_count_per_student corresponds to a student and each column corresponds to a feature. 
get_feat_count_per_student <- function(all_features, directory_path, make_long = TRUE, learner_ids) {

  # define dataframe:
  feat_count_per_student <- data.frame(matrix(ncol = length(all_features), nrow = length(learner_ids)))

  rownames(feat_count_per_student) <- learner_ids
  colnames(feat_count_per_student) <- all_features
  # make all NANs 0
  feat_count_per_student[] <- 0
  
  for (id in learner_ids) {
    # get all files by student at that level
    file_list <- list.files(directory_path, pattern = paste0("learner", id,"\\.csv$"), full.names = TRUE, recursive = TRUE)
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

  print("finished looping")

  if (make_long){
    feat_count_per_student$learnerID <- rownames(feat_count_per_student)
    feat_count_per_student <- make_long_feats_df(feat_count_per_student, "learnerID", "total")
  }
  return(feat_count_per_student)

} 

# get_learner_ids_by_ntexts_level_with_directory_path <- function(directory_path, min_num_texts) {
#   file_list <- list.files(directory_path, pattern = paste0("\\.csv$"), full.names = TRUE, recursive = TRUE)
#   # Get only the <learnerID> part
#   match_indices <- regexpr("learner\\d+", file_list)
#   learner_ids <- regmatches(file_list, match_indices) # at this point we have "learner<ID>"
#   # Extract number to get the learner IDs on their own
#   match_indices <- regexpr("\\d+", learner_ids)
#   learner_ids <- regmatches(learner_ids, match_indices)
  
#   freqs <- as.data.frame(table(learner_ids))
#   learner_ids <- freqs[freqs$Freq >= min_num_texts ,]$learner_ids
#   return(learner_ids)
# }

get_language_frequencies <- function(directory_path) {
  # Get the language from the output file names
  file_list <- list.files(directory_path, pattern = paste0("\\.csv$"), full.names = FALSE, recursive = TRUE)
  match_langs <- regexpr("_[a-z]{2}_", file_list)
  langs <- regmatches(file_list, match_langs)
  freqs <- as.data.frame(table(langs))
  return (freqs)
}



#--------------------------------------------------------
#-------------------------PLOTS--------------------------
#--------------------------------------------------------
# Make the boxplot by grouping different feature levels - optional vector for ylim
# make_boxpolot_group <- function(df, ylim_vector=NULL){
#   ggplot(df, aes(x=level, y=total, fill=feat_level))+
#     geom_boxplot(notch = TRUE)+
#     labs(fill="Feature level", x="Text level", y="Percentage of presence")+
#     coord_cartesian(ylim = ylim_vector)
# }


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
  
  for (text_lvl in c("A1", "A2", "B1", "B2", "C1", "C2")){
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

get_lineplot <- function(df, constr_id, title = NULL, ylim_vector=NULL) {
  plt <- df %>%
    group_by(feature, text_level) %>%
    filter(feature == constr_id) %>%
    summarise(mean = mean(total)) %>%
    ggplot(aes(x = text_level, y = mean, color = as.factor(feature), group = feature)) +
    geom_line() +
    #geom_errorbar(aes(ymin=mean-var, ymax=mean+var), width=.2, position=position_dodge(.9)) 
    labs(x = "Text Level", y = "Mean frequency of construct", col= "Construct ID") +
    #xlab("Levels") +
    #ylab("Mean frequency of construct") +
    scale_x_discrete(
      breaks = c("A1", "A2", "B1", "B2", "C1", "C2"),
      labels = c("A1", "A2", "B1", "B2", "C1", "C2")
    ) +
    ggtitle(title)
  
  return(plt)
}

# For getting a dataframe with features of only one level
get_level_feats <- function(level, dataframe_long){
  level = tolower(level)
  dataframe_long$feature <- as.numeric(dataframe_long$feature)
  if (level == "a1"){
    level_df <- dataframe_long[dataframe_long$feature <= 109,]
  } else if (level == "a2") {
    level_df <- dataframe_long[dataframe_long$feature >= 110 & dataframe_long$feature <=397,]
  } else if (level == "b1") {
    level_df <- dataframe_long[dataframe_long$feature >= 401 & dataframe_long$feature <=734,]
  } else if (level == "b2"){
    level_df <- dataframe_long[dataframe_long$feature >= 739 & dataframe_long$feature <=977,]
  } else if (level == "c1") {
    level_df <- dataframe_long[dataframe_long$feature >= 982 & dataframe_long$feature <=1105,]
  } else if (level == "c2") {
    level_df <- dataframe_long[dataframe_long$feature >= 1111,]
  } else {
    print("INVALID LEVEL!")
  }
  
  return(level_df)
}

# ----------------------------------------------------
# ------------ CONSTRUCT-LEVEL ASSIGNMENT ------------
# ----------------------------------------------------

# Fills in the first_sign_p_pred column of the prediction dataframe in with the level prediction. 
fillFirstSignPrediction <- function(pvals_df, predict_df) {
  for (r in 1:nrow(pvals_df)){
    print(pvals_df[r, 1])
    for (c in 2:6){
      if(!is.na(pvals_df[r, c])){
        print(pvals_df[r, c])
        predict_df$first_sign_p_pred[r] <- str_split_i(colnames(pvals_df)[c], "_", 2)
        break
      }
    }
  }
  
  return(predict_df)
}

# Fills in the min_p_pred column of the prediction dataframe in with the level prediction. 
# fillLowestPvalPrediction <- function(pvals_df, predict_df){
#   for (r in 1:nrow(pvals_df)){
#     if (any(!is.na(pvals_df[r, 2:6 ]))){
#       # get the name of the column with the minimum p-val
#       column_name <- names(pvals_df)[apply(pvals_df[r, 1:6], 1, which.min)]
#       predicted_level <- str_split_i(column_name, "_", 2)
#       print(predicted_level)
#       print(r)
#       print(predict_df)
#       predict_df[r, "min_p_pred"] <- predicted_level
#     }
    
#   }
  
#   return(predict_df)
  
# }

get_exact_precision_recall_f1 <- function(actual, predicted) {
  if (length(actual) != length(predicted)) {
    print("Actual and predicted must be same length")
  }
  
  cefr_levels <- c("A2", "B1", "B2", "C1", "C2")
  result <- data.frame(level = cefr_levels, precision = rep(NA, 5), recall = rep(NA, 5), f1 = rep(NA, 5))
  
  total_tp <- 0
  total_fp <- 0
  total_fn <- 0
  for (l in cefr_levels) {
    tp = 0
    fp = 0
    fn = 0
    
    for (i in 1:length(actual)) {
      if(actual[i] == l && predicted[i] == l) {
        # print("New TP")
        tp <- tp + 1
        total_tp <- total_tp + tp
      } else if (actual[i] == l) {
        # print("New FN")
        fn <- fn + 1
        total_fn <- total_fn + fn
      } else if (predicted[i] == l) {
        # print("New FP")
        fp <- fp +1
        total_fp <- total_fp + fp
      }
    }
    precision <- NA
    recall <- NA
    
    if (fp + tp != 0){
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
  
  macro_avg <- data.frame(precision = mean(result$precision, na.rm=TRUE), recall = mean(result$recall, na.rm=TRUE), f1 = mean(result$f1, na.rm=TRUE) )
  micro_avg <- data.frame(precision = total_tp/(total_tp + total_fp), recall = total_tp / (total_tp + total_fn), f1 = total_tp/(total_tp + ((total_fn + total_fp)/2)))
  return(list("result" = result, "macro_avg" = macro_avg, "micro_avg" = micro_avg))
}


get_neighbors_precision_recall_f1 <- function(actual, predicted) {
    if (length(actual) != length(predicted)) {
      print("Actual and predicted must be same length")
    }
    
    cefr_levels <- c("A2", "B1", "B2", "C1", "C2")
    result <- data.frame(level = cefr_levels, precision = rep(NA, 5), recall = rep(NA, 5), f1 = rep(NA, 5))
    
    total_tp <- 0
    total_fp <- 0
    total_fn <- 0
    for (l in cefr_levels) {
      tp = 0
      fp = 0
      fn = 0
      
      for (i in 1:length(actual)) {
        if((actual[i] == l | predicted[i]== l) && (is_neighbor_level(actual[i], predicted[i]))) {
          #print(paste("New TP", actual[i], predicted[i]))
          tp <- tp + 1
          total_tp <- total_tp + tp
        } else if (actual[i] == l) {
          # print("New FN")
          fn <- fn + 1
          total_fn <- total_fn + fn
        } else if (predicted[i] == l) {
          # print("New FP")
          fp <- fp +1
          total_fp <- total_fp + fp
        }
      }
      precision <- NA
      recall <- NA
      
      if (fp + tp != 0){
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
    macro_avg <- data.frame(precision = mean(result$precision, na.rm=TRUE), recall = mean(result$recall, na.rm=TRUE), f1 = mean(result$f1, na.rm=TRUE) )
    micro_avg <- data.frame(precision = total_tp/(total_tp + total_fp), recall = total_tp / (total_tp + total_fn), f1 = total_tp/(total_tp + ((total_fn + total_fp)/2)))
    return(list("result" = result, "macro_avg" = macro_avg, "micro_avg" = micro_avg))
  }


# fill with TRUE or FALSE, whether the construct is significant at the level it belongs to in the EGP
fillIsSigAtEgpLvl <- function(pval_df, predict_df) {
  pval_df <- add_feature_level(pval_df)
  #iterate through the columns, get the second level. If it's the same as feat_level, then TRUE in the prediction df
  for (row_idx in 1:nrow(pval_df)) {
    # get the feature of the row:
    constr_lvl <- pval_df$feat_level[row_idx]
    
    # find the column where the second level is that of the construct
    for (col_idx in 2:6) {
      colname <- colnames(pval_df)[col_idx]
      col_second_lvl <- str_split_i(colname, "_", 2)
      
      if (col_second_lvl == constr_lvl) {
        predict_df[row_idx, "signif_at_EGP_level"] <- !is.na(pval_df[row_idx, col_idx])
      }
      
    }
  }
  
  return(predict_df)
  
}

# Get the accuracy with regard to whether it is significant at the EGP level (ignoring operationalization)
accuracyIsSignificantEGPLevel <- function(predict_df) {
  total_booleans <-predict_df %>%
    filter(!is.na(signif_at_EGP_level)) %>%
    count()
  
  total_trues <- predict_df %>% 
    filter(!is.na(signif_at_EGP_level) & signif_at_EGP_level == TRUE) %>%
    count()
  
  return(total_trues/total_booleans)
}

is_neighbor_level <- function(actual_level, predicted_level){
  cefr_levels_neighbors <- list("A2"=c("A2", "B1"), "B1" = c("A2", "B1", "B2"), "B2" = c("B1", "B2", "C1") , "C1" = c("B2", "C1", "C2"), "C2" = c("C1", "C2"))
  return(predicted_level %in% cefr_levels_neighbors[actual_level][[1]])
}


# ----------------------------------------------------
# ----------------------- MISC -----------------------
# ----------------------------------------------------
get_text_author_df <- function(ef_dataframe, learner_ids, level) {
  return (ef_dataframe[ef_dataframe$cefr_level == level & ef_dataframe$learnerID %in% learner_ids, c("id", "learnerID")])
}