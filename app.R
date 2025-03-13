library(shiny)
library(bs4Dash)
library(tidyverse)
library(DT)
library(wordcloud2)
library(treemap)
library(d3treeR)
library(viridis)
library(waiter)

bgC3_flat <- read_csv("bgC3_flat.csv")
wfn <- read_csv("wfn.csv")
wfp <- read_csv("wfp.csv")
dataReviews <- read_csv("reviews.csv") %>%
      mutate(score = as.numeric(as.character(score))) %>%
      relocate(score, .before = review)

min_score_bgC3 <- min(as.numeric(as.character(bgC3_flat$score)), na.rm = TRUE)
max_score_bgC3 <- max(as.numeric(as.character(bgC3_flat$score)), na.rm = TRUE)

unique_scores <- sort(unique(dataReviews$score))

ui <- bs4DashPage(
      title = "Uber Analysis Dashboard",
      
      header = bs4DashNavbar(
            title = bs4DashBrand(
                  title = "Uber Review Sentiment Analysis",
                  color = "primary"
            ),
            skin = "dark"
      ),
      
      sidebar = dashboardSidebar(disable = TRUE),
      
      body = bs4DashBody(
            tabsetPanel(
                  id = "mainTabs",
                  
                  tabPanel("Treemap & Reviews",
                              fluidRow(
                                    bs4Card(
                                          selectInput("tm_sentiment", "Select Sentiment:",
                                                   choices = c("All", "Positive", "Negative"),
                                                   selected = "All"),
                                          hr(),
                                          selectInput("tableScore", "Select Uber Score for Reviews:",
                                                   choices = unique_scores,
                                                   selected = unique_scores[1])
                                          ),
                                    bs4Card(
                                          title = "Interactive Treemap",
                                          status = "primary",
                                          solidHeader = TRUE,
                                          collapsible = TRUE,
                                          width = 12,
                                          d3tree2Output("treemapOutput")
                                       ),
                                    bs4Card(
                                         title = "Reviews Table",
                                         status = "info",
                                         solidHeader = TRUE,
                                         collapsible = TRUE,
                                         width = 12,
                                         DTOutput("reviewsTable")
                                             )
                                       )
                                 ),
                  
                  tabPanel("Wordcloud",
                           fluidRow(
                                 bs4Card(
                                       title = "Wordcloud",
                                       status = "success",
                                       solidHeader = TRUE,
                                       collapsible = TRUE,
                                       width = 12,
                                       fluidRow(
                                             column(6,
                                                    radioButtons("wc_sentiment", "Select Sentiment:",
                                                                 choices = c("Positive", "Negative"),
                                                                 selected = "Positive")
                                             ),
                                             column(6,
                                                    sliderInput("minFreq", "Minimum Frequency:",
                                                                min = 1, max = 30, value = 1)
                                             )
                                       ),
                                       wordcloud2Output("wordcloud")
                                 )
                           )
                  ),
                  
                  tabPanel("References",
                           fluidRow(
                                 bs4Card(
                                       title = "References & Citations",
                                       status = "warning",
                                       solidHeader = TRUE,
                                       width = 12,
                                       HTML("
                     <ul>
                       <li><strong>Uber Dataset:</strong> Kanchana Karunarathna. (2024). Uber Customer Reviews Dataset (2024) [Data set]. Kaggle. <a href='https://doi.org/10.34740/KAGGLE/DSV/10248932' target='_blank'>https://doi.org/10.34740/KAGGLE/DSV/10248932</a></li>
                       <li><strong>R:</strong> R Core Team. (2023). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. <a href='https://www.R-project.org/' target='_blank'>https://www.R-project.org/</a></li>
                       <li><strong>VADER Lexicon:</strong> Hutto, C. J., & Gilbert, E. (2014). VADER: A Parsimonious Rule-based Model for Sentiment Analysis of Social Media Text. In Proceedings of the Eighth International Conference on Weblogs and Social Media (ICWSM-14).</li>
                       <li><strong>Bing Lexicon:</strong> Hu, M., & Liu, B. (2004). Mining and Summarizing Customer Reviews. In Proceedings of the Tenth ACM SIGKDD International Conference on Knowledge Discovery and Data Mining (KDD-2004), 168–177.</li>
                       <li><strong>NRC Emotion Lexicon:</strong> Mohammad, S. M., & Turney, P. D. (2013). Crowdsourcing a Word-Emotion Association Lexicon. <em>Computational Intelligence, 29</em>(3), 436–465.</li>
                       <li><strong>AFINN Lexicon:</strong> Nielsen, F. Å. (2011). A new ANEW: Evaluation of a word list for sentiment analysis in microblogs. In Proceedings of the 15th Conference on World Wide Web (WWW-2011), 631–638.</li>
                       <li><strong>bs4Dash:</strong> Granjon, D. (2024). bs4Dash: A 'Bootstrap 4' Version of 'shinydashboard'. R package version 2.3.4. <a href='https://github.com/RinteRface/bs4Dash' target='_blank'>https://github.com/RinteRface/bs4Dash</a></li>
                       <li><strong>shinydashboard:</strong> Chang, W., & Borges Ribeiro, B. (2018). shinydashboard: Create dashboards with Shiny. R package version 0.7.1. <a href='https://cran.r-project.org/package=shinydashboard' target='_blank'>https://cran.r-project.org/package=shinydashboard</a></li>
                       <li><strong>tidyverse:</strong> Wickham, H., et al. (2019). Welcome to the tidyverse. <em>Journal of Open Source Software, 4</em>(43), 1686. <a href='https://www.tidyverse.org/' target='_blank'>https://www.tidyverse.org/</a></li>
                       <li><strong>d3treeR:</strong> Lê, S., Josse, J., Husson, F., & Pagès, J. (2020). d3treeR: An htmlwidgets interface for d3.js tree visualizations. R package version 0.2.0.</li>
                       <li><strong>DT:</strong> Xie, Y. (2021). DT: A Wrapper of the JavaScript Library DataTables. R package version 0.20.</li>
                       <li><strong>wordcloud2:</strong> Zhao, J., & Huang, Z. (2020). wordcloud2: A word cloud generator. R package version 0.0.2.</li>
                       <li><strong>viridis:</strong> Garnier, S. (2021). viridis: Color Maps from matplotlib. R package version 0.6.2.</li>
                       <li><strong>tidytext:</strong> Silge, J., & Robinson, D. (2016). tidytext: Text Mining and Analysis using Tidy Data Principles. R package version 0.2.4.</li>
                     </ul>")
                                 )
                           )
                  )
            )
      ),
      
      footer = bs4DashFooter(
            left = "Uber Review Analysis © 2024",
            right = "Powered by R, bs4Dash & Shiny"
      )
)

server <- function(input, output, session) {
      
      filteredTreemapData <- reactive({
            dt <- bgC3_flat
            if (input$tm_sentiment != "All") {
                  dt <- dt %>% filter(sentiment == input$tm_sentiment)
            }
            dt
      })
      
      output$treemapOutput <- renderD3tree2({
            tm <- treemap(
                  filteredTreemapData(),
                  index = c("sentiment", "score", "bigram"),
                  vSize = "n",
                  type = "index",
                  title = "",
                  palette = viridis(200, alpha = 1, option = "D"),
                  border.col = c("cornsilk", "#fff", "#fff"),
                  border.lwds = c(1, 0.5, 0.1),
                  fontsize.labels = c(0.7, 0.4, 0.3),
                  fontcolor.labels = c("#fff", "#fff", "firebrick"),
                  fontface.labels = 1,
                  bg.labels = "transparent",
                  align.labels = list(c("center", "center"),
                                      c("left", "top"),
                                      c("right", "bottom")),
                  overlap.labels = 0.5
            )
            d3tree2(tm, rootname = "Uber Sentiment Bigrams")
      })
      
      filteredReviews <- reactive({
            dataReviews %>% filter(score == input$tableScore)
      })
      
      output$reviewsTable <- renderDT({
            datatable(filteredReviews()[, c("id", "review", "score", "time_of_day", "sentiment")],
                      options = list(pageLength = 20))
      })
      
      filteredWordcloud <- reactive({
            df <- if (input$wc_sentiment == "Positive") wfp else wfn
            df <- df %>% filter(freq >= input$minFreq)
            df
      })
      
      output$wordcloud <- renderWordcloud2({
            wordcloud2(filteredWordcloud(), size = 1, color = "random-dark")
      })
}

shinyApp(ui, server)