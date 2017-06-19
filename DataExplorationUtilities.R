Get_Free_Text_Measures <- function(data_set, minimum_unique_threshold=0.9, features_to_ignore=c()) {
  # look for text entries that are mostly unique
  text_features <- c(names(data_set[sapply(data_set, is.character)]), names(data_set[sapply(data_set, is.factor)]))
  for (f_name in setdiff(text_features, features_to_ignore)) {
    f_vector <- as.character(data_set[,f_name])
    # treat as raw text if data over minimum_precent_unique unique
    if (length(unique(as.character(f_vector))) > (nrow(data_set) * minimum_unique_threshold)) {
      data_set[,paste0(f_name, '_word_count')] <- sapply(strsplit(f_vector, " "), length)
      data_set[,paste0(f_name, '_character_count')] <- nchar(as.character(f_vector))
      data_set[,paste0(f_name, '_first_word')] <- sapply(strsplit(as.character(f_vector), " "), `[`, 1)
      data_set[,paste0(f_name, '_second_word')] <- sapply(strsplit(as.character(f_vector), " "), `[`, 2)
      # remove orginal field
      data_set[,f_name] <- NULL
    }
  }
  return(data_set)
}
