
################ A1 ################ 

learners_a1_full <- ef2_short %>%
  filter(cefr_level == "a1") %>%
  count(learnerID) %>%
  filter(n >= 24) %>%
  pull(learnerID)

ef2_short %>%
  filter(cefr_level == "a1", learnerID %in% learners_a1_full) %>%
  ef_text_2_txt("../text_annotation/data/input_filtered_07032024/")

################ A2 ################ 

learners_a2_full <- ef2_short %>%
  filter(cefr_level == "a2") %>%
  count(learnerID) %>%
  filter(n >= 24) %>%
  pull(learnerID)

ef2_short %>%
  filter(cefr_level == "a2", learnerID %in% learners_a2_full) %>%
  ef_text_2_txt("../text_annotation/data/input_filtered_07032024/")

################ B1 ################ 
learners_b1_full <- ef2_short %>%
  filter(cefr_level == "b1") %>%
  count(learnerID) %>%
  filter(n >= 24) %>%
  pull(learnerID)

ef2_short %>%
  filter(cefr_level == "b1", learnerID %in% learners_b1_full) %>%
  ef_text_2_txt("../text_annotation/data/input_filtered_07032024/")

################ B2 ################ 
learners_b2_full <- ef2_short %>%
  filter(cefr_level == "b2") %>%
  count(learnerID) %>%
  filter(n >= 24) %>%
  pull(learnerID)

ef2_short %>%
  filter(cefr_level == "b2", learnerID %in% learners_b2_full) %>%
  ef_text_2_txt("../text_annotation/data/input_filtered_07032024/")

################ C1 ################ 
learners_c1_full <- ef2_short %>%
  filter(cefr_level == "c1") %>%
  count(learnerID) %>%
  filter(n >= 24) %>%
  pull(learnerID)

ef2_short %>%
  filter(cefr_level == "c1", learnerID %in% learners_c1_full) %>%
  ef_text_2_txt("../text_annotation/data/input_filtered_07032024/")


################ C2 ################ 
learners_c2_full <- ef2_short %>%
  filter(cefr_level == "c2") %>%
  count(learnerID) %>%
  filter(n >= 8) %>%
  pull(learnerID)

ef2_short %>%
  filter(cefr_level == "c2", learnerID %in% learners_c2_full) %>%
  ef_text_2_txt("../text_annotation/data/input_filtered_07032024/")
