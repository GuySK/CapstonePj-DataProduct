#
# Predict Functions. 
# Functions related to model prediction and accuracy
#

#
# nextWord - Retrieves the next most probable words in a sentence
#
nextWord <- function(sentence, 
                     n = 3, 
                     maxwords = 5, 
                     talkative = FALSE, 
                     heartbeat = FALSE){
    #
    # Retrieves the most probable words using N-grams
    # Returns words and their discounted probability.
    #
    
    # debugging mode    
    if (talkative)                                         
        cat('n=', n, '\n')
        
    # shows progress in batch
    if (heartbeat) {                                       
        cat('.')
    }
    
    # Base case. If we're here, there are no ngrams left to search. just return words.
    if (n == 1) {                                          
        odf <- data.frame(Word = n1gn_df$W1, 
                          Prob = n1gn_df$ProbWB, 
                          Ngram = n, 
                          stringsAsFactors = F, row.names = NULL)[1:maxwords,]
        return(odf[order(odf$Ngram, odf$Prob, decreasing = T),]) 
    }
    
    # General case. Look for ngrams of level 'n'.
    df <- paste0('n', n, 'gn_df')                          # ngram data frame name
    
    srchNgrams <- ngram2(sentence, n = n-1, encode = T)    # convert sentence to n-grams
    srch <- srchNgrams[length(srchNgrams)]                 # get the last one
    candidates <- subset(get(df), Skey == srch)            # search the n-gram data frame
    nr_words   <- min(nrow(candidates), maxwords)          # number of words in my list
    pend_words <- maxwords - nr_words                      # number of words pending    
    
    # create output data frame.
    if (nr_words > 0) {                                    
        col <- paste0('W', n)                              # set appropriate column name    
        odf <- data.frame(Word = candidates[,col],      
                          Prob = candidates$ProbWB, 
                          Ngram = n, 
                          stringsAsFactors = F, row.names = NULL)
    } else {odf <- data.frame()}                           # no results. create a void df  
    
    # check if back-off is necessary.
    if (pend_words > 0)  {                                 # if not enough words
        odf_new <- nextWord(sentence, n-1,                 # back off to n-1 n-gram
                            maxwords = pend_words,         # and get the rest of the words     
                            talkative = talkative,
                            heartbeat = heartbeat)
        odf <- rbind(odf, odf_new)                         # append lower ngram results
        }
    
    # return results sorted by ngram level and probability.
    odf[order(odf$Ngram, odf$Prob, decreasing = T),][1: maxwords,] 
}

#
# predict_word - Predicts next word in a sentence
#
predict_word <- function(sentence, silent = FALSE, ...){
    #
    
    if (!silent){
        cat('\n --->', sentence, '<--- \n')
    }
    
    words <- ngram2(sentence, n = 1, encode = F, encoded = F)
    predicted <- nextWord(words, ...)
    
    return(predicted)
}
#
# chooseSent - Cuts a sentence at random for later prediction
#
chooseSent <- function(words, stopMark = 'xxstopxx'){
    # words is a vector of words
    # returns a list with a sentence and its next word
    
    if (length(words) == 0)                         # there's nothing to choose from 
        stop('ERROR. Void sentence. \n')
    
    idx <- sample(1:length(words), 1)               # choose one word at random
    
    if (idx == 1) {                                 # create a stop if first word chosen
        words <- c(stopMark, words)
        idx <- idx + 1
    }                                         
    sent <- words[1:(idx - 1)]                      # get sentence to predict  
                       
    return(list(sent, words[idx]))                  # return sentence and target word
}
#
# predictSentences -  Finds most probable word continuations in a vector of asentences
#
predictSentences <- function(sentences, 
                             MaxNgram = 3, 
                             maxwords = 5,
                             talkative = FALSE,
                             heartbeat = TRUE){
    #
    # predictSentences - Predicts sentences 
    #
    
    # clean sentences with standard transformations
    sents <- cleanSent(sentences)
    
    # tokenize sentences
    tokenSents <- lapply(sents, FUN= ngram2, USE.NAMES = F, n=1, encode=F)

    # get rid of zero length sentences
    sent_lens <- sapply(tokenSents, length)
    tokenSents <- tokenSents[!(sent_lens == 0)]
    
    # choose random subsentence and target word 
    sentList <- lapply(tokenSents, chooseSent)
    
    # group all sentences to be predicted together as a char vector
    toPredSents <- sapply(sentList, "[[", 1)
    
    # group all target words
    targets <- sapply(sentList, "[[", 2)
    
    # predict next word for each sentence
    if (talkative){
        cat('   ', length(sentences), 'sentences to predict. \n')
    }
    predicts <- lapply(toPredSents, nextWord, 
                       n = MaxNgram, 
                       maxwords = maxwords,
                       talkative = talkative,
                       heartbeat = heartbeat)
    if (heartbeat) cat('\n')

    # rebuild predicted sentence
    predSents <- sapply(toPredSents, paste, collapse = ' ')
    
    # return a data frame with target and prediction
    lens <- sapply(X = predicts, FUN = length)
    if (!all.equal(max(lens), min(lens, maxwords))) {
        stop('ERROR. MAXWORDS or List lengths do not agree. \n')
    }

    df_cols <- c()
    for (i in 1:maxwords){
        df_cols <- c(df_cols, paste0('W', i))
    }
    
    df1 <- data.frame(matrix(data = unlist(predicts), 
                             nrow = length(predicts), 
                             ncol = length(df_cols), 
                             byrow = T), 
                      row.names = NULL, stringsAsFactors = F)
    colnames(df1) <- df_cols
    
    df2 <- data.frame(Target = targets, df1, Sentence = predSents, stringsAsFactors = FALSE)
    
    return(df2)
}
#
# get_accuracy -  Computes accuracy on word prediction
#
get_accuracy <- function(prd_df){
    #
    acc <- rep(0, df_degree(prd_df))
    for (i in 1:df_degree(prd_df)){
        acc[i] <- sum(with(prd_df, Target == get(paste0('W',i))), na.rm = T) / nrow(prd_df)
    }
  return(round(acc,4))
}
#
# getWord - Retrieves a word from 1-gram dataframe.
#
getWord <- function(code, df = n1gn_df){
    #
    # retrieves word(s) by code.
    #
    
    if (missing(code))
        return(NULL)
    
    if (length(code) > 1)
        return(sapply(code, getWord, df))
    
    return(df[df$Key == code, 'W1'])    
}
#
# getCount - Retrieves a word's count from 1-gram dataframe.
#
getCount <- function(code, df = n1gn_df){
    #
    # retrieves word(s) by code.
    #
    
    if (missing(code))
        return(NULL)
    
    if (length(code) > 1)
        return(sapply(code, getCount, df))
    
    return(df[df$Key == code, 'Count'])    
}

#
# letsee - predicts a sentence in the prediction data base for analysis
#
letsee <- function(df, sent_nr){
    #
    # takes a sentence by its number from the prediction data base 
    # specified and re-predicts it. Show results in a convenient way.
    #
    words <- predict_word(df$Sentence[sent_nr], 
                          silent = T, 
                          maxwords = 3, 
                          heartbeat = F)
    
    cat('---> ', df$Sentence[sent_nr], ' ... ',  
        ' (', paste0(words, collapse=' / '), ') ',
        df$Target[sent_nr],
        '.\n', sep = '')
}
