library(tidyverse)
library(tidytext)
library(wordVectors)
library(udpipe)

query <- "Celebrating Animals, Confronting Cruelty"

#udmodel <- udpipe_download_model(language = "english")

load("data/udmodel.RData")

guidestar <- read_csv("data/guidestar_full.csv")

# writeLines(paste(guidestar$Mission, collapse = "\n"), "mission_temp.txt")
# word2phrase_temp <- word2phrase("mission_temp.txt", "mission_phrases.txt", min_count = 4, force = TRUE)
# word2phrase_model <- train_word2vec("mission_phrases.txt", "mission_vectors.bin")
# save(word2phrase_model, file = "data/word2phrase_model.RData")

load("data/word2phrase_model.RData")
load("data/word2vec_model.RData")

stopwords <- get_stopwords() %>%
    rbind(c("inspire", "snowball"),
          c("empower", "snowball"),
          c("throughout", "snowball"),
          c("united", "snowball"),
          c("states", "snowball"),
          c("create", "snowball"),
          c("people", "snowball"),
          c("improve", "snowball"),
          c("affected", "snowball"),
          c("quality", "snowball"),
          c("life", "snowball"),
          c("network", "snowball"),
          c("disease", "snowball"),
          c("families", "snowball"),
          c("lasting", "snowball"))

udpipe_query <- udpipe(query, object = udmodel)
#udpipe_guidestar <- udpipe(paste(guidestar$Mission, collapse = " "), object = udmodel)
load("data/udpipe_guidestar.RData")

query_words <- query %>%
    as.data.frame %>%
    select(Query = 1) %>%
    mutate(Query = as.character(Query)) %>%
    unnest_tokens(word, Query) %>%
    anti_join(stopwords) %>%
    inner_join(
        udpipe_query %>%
            select(token, upos) %>%
            mutate(token = tolower(token)) %>%
            filter(upos == "NOUN")
    , by = c("word" = "token")) %>%
    select(1)

query_twograms <- query %>%
    as.data.frame %>%
    select(Query = 1) %>%
    mutate(Query = as.character(Query)) %>%
    unnest_tokens(word, Query, token = "ngrams", n = 2, n_min = 2)

query_words <- query_words %>% rbind(query_twograms)

guidestar_words <- guidestar %>%
    mutate(Mission = paste(Organization, Mission)) %>%
    unnest_tokens(word, Mission, drop = FALSE) %>%
    anti_join(stopwords) %>%
    inner_join(
        udpipe_guidestar %>%
            select(token, upos) %>%
            mutate(token = tolower(token)) %>%
            filter(upos == "NOUN")
    , by = c("word" = "token")) %>%
    add_count(Organization, word, sort = TRUE) %>%
    bind_tf_idf(word, Organization, n)

guidestar_twograms <- guidestar %>%
    mutate(Mission = paste(Organization, Mission)) %>%
    unnest_tokens(word, Mission, token = "ngrams", n = 2, n_min = 2)

guidestar_words <- guidestar_words %>%
    bind_rows(guidestar_twograms)

vec1 <- word2phrase_model[[query_words$word, average = FALSE]]
vec2 <- word2phrase_model[[guidestar_words$word, average = FALSE]]
#vec1 <- word2vec_model[[query_words$word, average = FALSE]]
#vec2 <- word2vec_model[[guidestar_words$word, average = FALSE]]
similarities <- cosineSimilarity(vec1, vec2)

#similarities <-  cosineSimilarity(get_word_vectors(fasttext_word2vec_model, unique(query_words$word)),
#                                  get_word_vectors(fasttext_word2vec_model, unique(guidestar_words$word)))

highest_matching_words <- colSums(similarities)
matching_df <- data.frame(word = names(highest_matching_words), sim = as.numeric(highest_matching_words), stringsAsFactors = FALSE)

test <- guidestar_words %>%
    select(EIN, Organization, Mission, word) %>%
    left_join(matching_df) %>%
    replace_na(list(Score = 0)) %>%
    group_by(EIN, word) %>%
    slice(1) %>%
    group_by(EIN) %>%
    summarise(Organization = Organization[1],
              Mission = Mission[!is.na(Mission)][1],
              Score = mean(sim, na.rm = TRUE)) %>%
    arrange(desc(Score))
test
