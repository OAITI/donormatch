library(shiny)
library(shinythemes)
library(tidyverse)
library(stringdist)
library(tidytext)
library(wordVectors)

## Set images resource path
addResourcePath("images", "images")

guidestar <- read_csv("data/guidestar_full.csv") %>%
    mutate(Mission = gsub("Cuncer", "Cancer", Mission))

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
#    mutate(Mission = paste(Organization, Mission)) %>%
    unnest_tokens(word, Mission, drop = FALSE) %>%
    anti_join(stopwords) %>%
    add_count(Organization, word, sort = TRUE) %>%
    bind_tf_idf(word, Organization, n)

guidestar_twograms <- guidestar_words %>%
    select(EIN, Organization, word) %>%
    group_by(Organization) %>%
    summarise(Mission = paste(unique(word), collapse = " ")) %>%
    unnest_tokens(twogram, Mission, token = "ngrams", n = 2)

load("data/word2phrase_model.RData")
# load("data/fasttext_word2vec_model.RData")

ui <- fluidPage(theme = shinytheme("cerulean"),
                
    includeCSS("css/styles.css"),
    
    titlePanel("Donor Matching"),
    
    sidebarLayout(
        sidebarPanel(
            a(href = "https://oaiti.org", target = "_blank", img(src = "images/oaiti_transparent.png", width = "135")),
            h4("About"),
            HTML("This application allows non-profits to type in a mission statement and receive a best possible donor match among IRS Private Foundations."),
            
            h4("Configuration"),
            
            textInput("query", "Mission Statement", placeholder = "To create lasting solutions to poverty, hunger, and social injustice."),
            actionButton("submit", "Find Donor Match"),
            
            hr(),
            
            HTML("Powered by <a href='https://guidestar.org' target='_blank'>GuideStar</a> data")
        ),
        
        mainPanel(
            tabsetPanel(
                tabPanel("Match",
                         uiOutput("result")
                ),
                tabPanel("Raw",
                         DT::dataTableOutput("top5_missions"),
                         hr(),
                         DT::dataTableOutput("top5")
                )
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    values <- reactiveValues(query = NULL)
    
    observeEvent(input$submit, {
        values$query <- input$query
    })
    
    query_words <- reactive({
        if (is.null(values$query)) return(NULL)
        
        values$query %>%
            as.data.frame %>%
            select(Query = 1) %>%
            mutate(Query = as.character(Query)) %>%
            unnest_tokens(word, Query) %>%
            anti_join(stopwords)
    })
    
    query_twograms <- reactive({
        if (is.null(values$query)) return(NULL)
        
        query_words() %>%
            summarise(Mission = paste(unique(word), collapse = " ")) %>%
            unnest_tokens(twogram, Mission, token = "ngrams", n = 2)
    })
    
    vec1 <- reactive({ word2phrase_model[[query_words()$word, average = FALSE]] })
    vec2 <- reactive({ word2phrase_model[[guidestar_words$word, average = FALSE]] })
    # vec1 <- reactive({ get_word_vectors(fasttext_word2vec_model, unique(query_words()$word)) })
    # vec2 <- reactive({ get_word_vectors(fasttext_word2vec_model, unique(guidestar_words$word)) })
    
    cosine_sim <- reactive({
        similarities <- cosineSimilarity(vec1(), vec2())
        highest_matching_words <- colSums(similarities)
        matching_df <- data.frame(word = names(highest_matching_words), sim = as.numeric(highest_matching_words), stringsAsFactors = FALSE)
        
        guidestar_words %>%
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
    })
    
    full_list <- reactive({
        if (is.null(values$query)) return(NULL)
        
        query_twograms() %>%
            inner_join(guidestar_twograms) %>%
            group_by(Organization, twogram) %>%
            summarise(MatchingTwoGrams = n()) %>%
            group_by(Organization) %>%
            summarise(MatchingTwoGrams = sum(MatchingTwoGrams),
                      UniqueTwoGrams = length(unique(twogram))) %>%
            full_join(
                query_words() %>%
                    inner_join(guidestar_words) %>%
                    group_by(Organization, word) %>%
                    summarise(MatchingWords = n(),
                              tf_idf = tf_idf[1],
                              Mission = Mission[1]) %>%
                    group_by(Organization) %>%
                    summarise(MatchingWords = sum(MatchingWords),
                              UniqueWords = length(unique(word)),
                              Mission = Mission[1],
                              LengthDiff = abs(nchar(values$query) - nchar(Mission[1])),
                              AverageTFIDF = mean(tf_idf),
                              MaxTFIDF = max(tf_idf)) %>%
                    left_join(select(cosine_sim(), Organization, VecScore = Score))
            )
    })
    
    full_ranks <- reactive({
        if (is.null(values$query)) return(NULL)
        
        full_list() %>%
            mutate_if(is.numeric, function(.) (. - min(., na.rm = TRUE)) / (max(., na.rm = TRUE) - min(., na.rm = TRUE))) %>%
            mutate(MatchingTwoGrams = ifelse(is.na(MatchingTwoGrams), 0, MatchingTwoGrams)) %>%
            mutate(UniqueTwoGrams = ifelse(is.na(UniqueTwoGrams), 0, UniqueTwoGrams)) %>%
            mutate(MatchingWords = ifelse(is.na(MatchingWords), 0, MatchingWords)) %>%
            mutate(UniqueWords = ifelse(is.na(UniqueWords), 0, UniqueWords)) %>%
            mutate(Score = 10 * MatchingWords + 7.5 * UniqueWords + 10 * MatchingTwoGrams - 3.33 * LengthDiff + 3.33 * AverageTFIDF + 3.33 * MaxTFIDF + 10 * VecScore) %>%
            arrange(desc(Score))
    })
    
    output$result <- renderUI({
        if (is.null(values$query)) return(NULL)
        if (nrow(full_ranks()) == 0) return(HTML("We are sorry, but we didn't find a suitable organization."))
        
        myrnk <- full_ranks() %>% slice(1)
        
        return(HTML(paste0("Your best donor match is: <b>", myrnk$Organization, 
                           "</b><br><br>Their mission statement is: ", myrnk$Mission,
                           "<br><br>Their website is ", ifelse(is.na(guidestar$URL[guidestar$Organization == myrnk$Organization]), "not available", paste0(": <a target='_blank' href='https://", guidestar$URL[guidestar$Organization == myrnk$Organization], "'>", guidestar$URL[guidestar$Organization == myrnk$Organization], "</a>")),
                           "<br><br>Their guidestar profile is: <a target='_blank', href='", guidestar$Guidestar[guidestar$Organization == myrnk$Organization], "'>", guidestar$Guidestar[guidestar$Organization == myrnk$Organization], "</a>")))
    })
    
    output$top5 <- DT::renderDataTable({
        if (is.null(values$query)) return(NULL)
        if (nrow(full_ranks()) == 0) return(NULL)
        
        full_ranks() %>%
            slice(1:5) %>%
	    select(-Mission)
    })
    
    output$top5_missions <- DT::renderDataTable({
        if (is.null(values$query)) return(NULL)
        if (nrow(full_ranks()) == 0) return(NULL)
        
        full_ranks() %>%
            slice(1:5) %>%
            select(Organization, Mission)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
