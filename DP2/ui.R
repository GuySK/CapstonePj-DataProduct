#
# ui.R
#
shinyUI(fluidPage(title = 'WordPredictor',
    fluidRow(        
        column(width = 4,
               br(),
               h3('Input Parameters & Data'),
               br(),
               h4('Number of predictions you want to get'),
               sliderInput('n_words', label = NULL, 1, 10, 3, step = 1, round = FALSE, 
                           ticks = FALSE, animate = FALSE, 
                           width = NULL, sep = ",", pre = NULL, post = NULL),
               br(),
               h4('Want probabilities?'),
               radioButtons(inputId = 'want_probs', label = NULL, 
                            choices = c(Yep = TRUE, Nop = FALSE),
                            selected = FALSE, inline = TRUE),
               br(),
               textInput('Sentence', 'Type / Paste text here', ''),
               actionButton('predButton', 'Predict', icon = NULL),
               actionButton('autoButton', 'Auto', icon = NULL),
               actionButton('clearButton', 'Clear', icon = NULL),
               br(),
               br(),
               helpText(HTML('Use the <b>PREDICT</b> button to get the next word.')),
               helpText(HTML('Use the <b>AUTO</b> button to use the last predicted word 
                             as continuation.')),
               helpText(HTML('Use the <b>CLEAR</b> button to remove the text.')),
               helpText(HTML('<i><b>Note.</b> You can get a statistically generated 
                             text by pressing </i><b>AUTO</b><i> repeatedly.</i>')), 
               offset = 1),
        
        column(width = 6, 
               h1('Word Predictor'),
               h4("Input Text:"),
               h5(textOutput('userInput')),
               br(),
               h4("Predicted Words / Probabilities (%)"),
               h5(textOutput('predWords')),
               h5(textOutput('predProbs')),
               br(),
               plotOutput('probsPlot'),
               offset = 0)
    )
))
