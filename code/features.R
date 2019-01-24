library(tidyverse)
library(stringdist)
library(tidytext)

guidestar <- read_csv("guidestar_full.csv") %>%
    mutate(Mission = gsub("Cuncer", "Cancer", Mission))

query <- "Cure leukemia, lymphoma, Hodgkin’s disease and myeloma, and improve the quality of life of patients and their families."

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

guidestar_words <- guidestar %>%
    mutate(Mission = paste(Organization, Mission)) %>%
    unnest_tokens(word, Mission, drop = FALSE) %>%
    anti_join(stopwords) %>%
    add_count(Organization, word, sort = TRUE) %>%
    bind_tf_idf(word, Organization, n)

guidestar_twograms <- guidestar_words %>%
    select(EIN, Organization, word) %>%
    group_by(Organization) %>%
    summarise(Mission = paste(unique(word), collapse = " ")) %>%
    unnest_tokens(twogram, Mission, token = "ngrams", n = 2)

query_words <- query %>%
    as.data.frame %>%
    select(Query = 1) %>%
    mutate(Query = as.character(Query)) %>%
    unnest_tokens(word, Query) %>%
    anti_join(stopwords)

query_twograms <- query_words %>%
    summarise(Mission = paste(unique(word), collapse = " ")) %>%
    unnest_tokens(twogram, Mission, token = "ngrams", n = 2)

full_list <- query_twograms %>%
    inner_join(guidestar_twograms) %>%
    group_by(Organization, twogram) %>%
    summarise(MatchingTwoGrams = n()) %>%
    group_by(Organization) %>%
    summarise(MatchingTwoGrams = sum(MatchingTwoGrams),
              UniqueTwoGrams = length(unique(twogram))) %>%
    full_join(
        query_words %>%
            inner_join(guidestar_words) %>%
            group_by(Organization, word) %>%
            summarise(MatchingWords = n(),
                      tf_idf = tf_idf[1],
                      Mission = Mission[1]) %>%
            group_by(Organization) %>%
            summarise(MatchingWords = sum(MatchingWords),
                      UniqueWords = length(unique(word)),
                      Mission = Mission[1],
                      LengthDiff = abs(nchar(query) - nchar(Mission[1])),
                      AverageTFIDF = mean(tf_idf),
                      MaxTFIDF = max(tf_idf))
    )

full_ranks <- full_list %>%
    mutate_if(is.numeric, function(.) (. - min(., na.rm = TRUE)) / (max(., na.rm = TRUE) - min(., na.rm = TRUE))) %>%
    mutate(MatchingTwoGrams = ifelse(is.na(MatchingTwoGrams), 0, MatchingTwoGrams)) %>%
    mutate(UniqueTwoGrams = ifelse(is.na(UniqueTwoGrams), 0, UniqueTwoGrams)) %>%
    mutate(MatchingWords = ifelse(is.na(MatchingWords), 0, MatchingWords)) %>%
    mutate(UniqueWords = ifelse(is.na(UniqueWords), 0, UniqueWords)) %>%
    mutate(Score = MatchingWords + UniqueWords + MatchingTwoGrams - LengthDiff + AverageTFIDF + MaxTFIDF) %>%
    arrange(desc(Score)) %>%
    slice(1)

full_ranks %>%
    select(Organization, Mission) %>%
    as.data.frame
