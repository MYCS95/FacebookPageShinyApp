#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tm)
library(memoise)
library(wordcloud2)
library(httr)
library(Rfacebook)
library(RCurl)
library(syuzhet)
library(ggplot2)

getPageData <- memoise(function(app_id, app_secret, page_id, nb_posts) {
    # Demande de connexion à facebook via les accès de l'app
    fb_oauth <- fb_auth(app_id, app_secret)
    
    # Extraction des posts
    page <- getPage(page_id, token=fb_oauth, feed=FALSE,n=nb_posts)
    
    # Extraction des messages des posts
    lmessage <- list()
    post <-  for(i in 1:length(page$from_id)){
        
        msg <- getPost(post=page$id[i],token=fb_oauth)
        
        comments <- msg$comments
        messages <- comments$message
        
        lmessage[paste(messages, sep = "")] <- messages
        options(max.print = 999999)
    }
    
    msgClean <- gsub("\n"," ",lmessage)
    #retrait des URL
    msgClean <- gsub('https\\S+\\s*',"",msgClean)
    #retrait des espaces en trop
    msgClean <- gsub("\\s+"," ",msgClean)
    #retrait des "\"
    msgClean <- gsub("[\\]","",msgClean)
    #retrait des espaces en fin de texte
    msgClean <- gsub("\\s*$","",msgClean)
    #harmonisation de la casse - tout mettre en minuscule
    msgClean <- tolower(msgClean)
    #retrait des accents
    msgClean <- gsub("[éèê]","e",msgClean)
    msgClean <- gsub("[àâ]","a",msgClean)
    
    msgClean <- gsub("rt ","",msgClean)
    msgClean <- msgClean[!duplicated(msgClean)]
    
    text <- iconv(msgClean, 'UTF-8', 'ASCII')
    
    myCorpus = Corpus(VectorSource(text))
    myCorpus = tm_map(myCorpus, content_transformer(tolower))
    myCorpus = tm_map(myCorpus, removePunctuation)
    myCorpus = tm_map(myCorpus, removeNumbers)
    myCorpus = tm_map(myCorpus, removeWords, c("quand","tout","pour","cabine","des","par", "pas","une","qui","vous","dans","sur","les","que","jai","qui","vos","tous","mon","cela","elle","elle","el","car"))
    myCorpus = tm_map(myCorpus, removeWords, c("mais","avec","avoir","mes","ceux","suis", "d'un","c'est", "depuis","votre","svp","bonjour","est","cas","comme","fcfa","ses","vers","qu'il","bonsoir","trop","rien","cette","dans","mais","moi","moi","apres","tant","dis","fai","quil","autre","jai","suis","nous","tres","lui","cest","sil","parce","davance","donc","encore","ici","heure","soit","certain"))
    myCorpus = tm_map(myCorpus, removeWords, c("dans","tes","fait","mais","plus","andrea","tchoutchou","joseph","aboubakar","non","gar","jordan","change","soro","aziz","kone","noir","marc","mdr","serge","serge","ouattara","michel","encore","ibrahim","kouadio","bamba","yann","audrey","konan","yao","person","junior","vivian","yao","grace","kouabenan","cedric","ang","gadeau","kalvin","emmanuel","nguessan","chantal","kadjo","kouakou","christian","cedric","roland","doumbia","celui"))
    myCorpus = tm_map(myCorpus, removeWords, c("je", "me", "moi",  "tu", "te", "toi", "t'", "il", "elle", "on", "se", "soi",  "lui", "le", "la", "l'", "y", "en","nous", "vous", "ils", "elles", "les","leur", "eux"))
    myCorpus = tm_map(myCorpus, removeWords, c("ci","celui", "celle", "ceux", "celles", "ce", "ceci", "cela", "ça", "celui-ci", "celle-ci", "ceux-ci", "celles-ci"))
    myCorpus = tm_map(myCorpus, removeWords, c("qui", "que", "quoi", "lequel", "laquelle", "lesquels", "lesquelles","où","dont","personne", "certains", "rien", "quelqu'un","quelque chose","n'importe qui"))
    
    myDTM = TermDocumentMatrix(myCorpus, control = list(minWordLength = 1))
    
    m = as.matrix(myDTM)
    
    sort(rowSums(m), decreasing = TRUE)
})


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Titre de l'application
   titlePanel("Wordcloud from Facebook page"),
   
   sidebarLayout(
      sidebarPanel(
        textInput("appID",label = "Facebook App ID", value = "", placeholder = "Enter your app ID"),
        textInput("appSecret", label = "Facebook App Secret", 
                  value = "", placeholder = "Enter your app secret"),
        textInput("pageID", label = "Facebook Page ID", value = "", placeholder = "Enter page ID"),
        numericInput("nbPosts", label = "Number of posts", value = 10),
        actionButton("update", "Update")
      ),
      
      # Dessine une zone pour générer le wordcloud
      mainPanel(
         plotOutput("wordcloudPlot")
      )
   )
)

# Définition de la logique serveur pour géréer le wordcloud
server <- function(input, output) {
  # Définition d'une fonction reactive afin de tenir compte des changements
  pageData <- reactive({
    
    # Mise à jour après clique sur le bouton
    input$update
    
    isolate({
      withProgress({
        setProgress(message = "Traitement en cours...")
        getPageData(input$appID, input$appSecret, input$pageID, input$nbPosts)
      })
    })
  })
  
  # Make the wordcloud drawing predictable during a session
  wordcloud_rep <- repeatable(wordcloud)
  
  output$wordcloudPlot <- renderPlot({
    if(input$update == 0)
      return()
    v <- pageData()
    wordcloud_rep(names(v), v, scale=c(4,0.5), min.freq = 10, max.words = 100, colors = brewer.pal(8, "Dark2"))
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

