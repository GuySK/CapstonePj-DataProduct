#
# server.R
#

library(shiny)
library(tm)
library(hash)
library(grDevices)

source("./Code/eda1_auxFunctions.R", local = T)
source("./Code/trnsFuncts.R", local = T)
source("./Code/ngram2.R", local = T)
source("./Code/nCodeNgram.R", local = T)
source("./Code/predictFuncts_3.R", local = T)

DATA_STR = './Data/dp_objects.RData';          
load(DATA_STR, envir = environment())

shinyServer(function(input, output, session) {
    
    my_rainbow <- function(n){
        rainbow(n, start = 0.666, end = 1)    
    }
            
    pred <- reactiveValues(words = NULL)
    
    observeEvent(input$predButton, {
        sent <- cleanSent(input$Sentence)
        pred$words <- nextWord(sent, maxwords = input$n_words, heart = F)
    })

    observeEvent(input$autoButton, {
        sent <- paste(input$Sentence, pred$words[[1]][1], sep = ' ')
        updateTextInput(session, inputId = 'Sentence', value = sent)
        sent <- cleanSent(sent)
        pred$words <- nextWord(sent, maxwords = input$n_words, heart = F)
    })
    
    observeEvent(input$clearButton, {
        updateTextInput(session, inputId = 'Sentence', value = '')
    })
    
    output$predWords <- renderText({
        paste0(paste0(pred$words[[1]],' (', pred$words[[3]], ')')
               , collapse = ' - ')
    })
    
    output$predProbs <- renderText({
        if (input$want_probs){
            probs <- as.character(round(pred$words[[2]] * 100, 5))
            paste0(probs, collapse = ' - ')            
        } else {
            probs <- NULL
        }
    })

    output$userInput <- renderText({
        input$pred
        input$Sentence                            
    })
    
    output$probsPlot <- renderPlot({
        if (input$want_probs){
            x <- pred$words[[2]]
            y <- pred$words[[1]]
            names(x) <- NULL
            # x <- rev(x)
            # insert barplot below this line
            barplot(height = x, 
                    # names.arg = names(x),
                    names.arg = NULL,
                    horiz = TRUE, 
                    col = my_rainbow(length(x)),
                    main = 'Probabilities of Predicted Words (log Scale)', 
                    axes = TRUE,
                    border = TRUE,
                    log = 'x')
            
            legend("topright", 
                   legend = y, 
                   cex = 1,
                   bty = 'n',
                   pch = NA,
                   y.intersp = 1,
                   text.col = 'darkblue',
                   fill = my_rainbow(length(x)))
            # insert barplot above this line
            } else {
                NULL
        }
    })
})