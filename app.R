#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(formattable)

data<-lapply(list.files("data",full.names = T,pattern="wordemb"),
             function (x) readRDS(x))
names(data)<-str_remove_all(list.files("data",pattern="wordemb"),".Rds")

ui <- fluidPage(
  tags$style(type = "text/css", "
              .js-irs-0 .irs-bar {border-top-color: #80ba24;border-bottom-color: #80ba24;}
             .js-irs-0 .irs-bar-edge {border-color: #80ba24;}
             .js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar, .js-irs-0 .irs-from, .js-irs-0 .irs-to {background: #80ba24;}"),
    sidebarLayout(
        sidebarPanel(textInput("searchword", "Word", value = "inflation", width = NULL, placeholder = NULL),
                     #selectInput("model", "Model:",names(data)),
                     sliderInput("top_n","Number of Words:",min = 1,max = 20,value = 5),
                     "The table shows the most similar terms to the target word according to the cosine distance of the underlying word embeddings"),
        mainPanel(formattableOutput("similarity_table"))
        )
    )

server <- function(input, output) {
    
    most_similar_to_word<-function(model,word,n_output){
        word<-tolower(word)
        rownumber<-which(rownames(model)==word)
        if(length(rownumber)==0){
            message("Word does not exist in Corpus")
            return(NA)
        }else{
            input<-as.matrix(t(model[rownumber,]))
            return(top_embeddings(model,input,n_output))
        }
    }
    top_embeddings<-function(model,input,n_output){
        as_tibble(text2vec::sim2(model,input),rownames="Word") %>%
            arrange(desc(V1)) %>%
            slice(-1) %>%
            top_n(n_output,V1) %>%
            mutate(Rank=1:n_output) %>% 
            rename(Similarity=V1) %>% 
            relocate(Rank)
    }
    similarity_table <- reactive({
        dat<-data[[names(data)]]
        a<-most_similar_to_word(dat,input$searchword,input$top_n)%>%
            formattable(align = c("c","c","r"),
                        list(`Rank` = formatter("span", style = ~ formattable::style(font.weight = "bold")),
                             `Word` = formatter("span", style = ~ formattable::style(font.weight = "bold")),
                             `Similarity` = color_bar(#"#dc3545"
                               "#80ba24")))
        
        return(a)
    })
    output$similarity_table <- renderFormattable({similarity_table()})
}

# Run the application 
shinyApp(ui = ui, server = server)
