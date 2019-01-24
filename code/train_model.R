library(wordVectors)
library(tidyverse)

x <- read_csv("data/guidestar_full.csv")
full_text <- paste(x$Mission, collapse = "\n")

writeLines(full_text, "training_temp.txt")

prep_word2vec("training_temp.txt", destination = "training_complete.txt", lowercase = TRUE, bundle_ngrams = 2)
word2vec_model <- train_word2vec("training_complete.txt", "training_vectors.bin", 
                                 vectors = 200, threads = 4,
                                 window = 12, iter = 5,
                                 negative_samples = 0)

save(word2vec_model, file = "data/word2vec_model.RData")

file.remove("training_temp.txt")
file.remove("training_complete.txt")
file.remove("training_vectors.bin")
