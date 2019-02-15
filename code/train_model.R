library(wordVectors)
library(tidyverse)
library(fastrtext)

x <- read_csv("../data/guidestar_full.csv")
full_text <- tolower(paste(x$Mission, collapse = "\n"))

writeLines(full_text, "training_temp.txt")
## fasttext word2vec

tmp_file_txt <- "training_temp.txt"
tmp_file_model <- "training_vectors"
execute(commands = c("skipgram", "-input", tmp_file_txt, "-output", tmp_file_model, 
                     "-verbose", 1, "-dim", 20,
                     "-lr", 0.08, "-epoch", 1))
fasttext_word2vec_model <- load_model(tmp_file_model)
dict <- get_dictionary(fasttext_word2vec_model)
fasttext_word_vectors <- get_word_vectors(fasttext_word2vec_model, dict)


## word2vec using wordVector package
prep_word2vec("training_temp.txt", destination = "training_complete.txt", lowercase = TRUE, bundle_ngrams = 2)
word2vec_model <- train_word2vec("training_complete.txt", "training_vectors.bin", 
                                 vectors = 200, threads = 4,
                                 window = 12, iter = 5,
                                 negative_samples = 0)

save(word2vec_model, file = "../data/word2vec_model.RData")
save(fasttext_word2vec_model, file = "../data/fasttext_word2vec_model.RData")

file.remove("training_temp.txt")
file.remove("training_complete.txt")
file.remove("training_vectors.bin")
