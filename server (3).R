 # Define server logic required to draw a histogram
    shinyServer(function(input, output) {
        
        
        library(Matrix)
        library(dplyr)
        library(tidytext)
        library(textreadr)
        library(textdata)
        library(tidyverse)
        library(tidytext)
        library(stringr)
        library(dplyr)
        library(ggplot2)
        library(reshape2)
        library(wordcloud)
        library(igraph)
        library(ggraph)
        library(gutenbergr)
        library(quanteda)
        library(shinydashboard)
        library(shiny)
        library(shinyWidgets)
        # Stopwords Data
        setwd("C:/Users/bhavya garg/Desktop/semester 1/R class 1/text_analyt_app")
        survey_data <- read_document(file="C:/Users/bhavya garg/Desktop/semester 1/R class 1/text_analyt_app/full version survey data.txt")
        
        
        
        #creating empty data frame
        a<-40
        b<-5
        my_df = as.data.frame(matrix(ncol = b, nrow = a))
        
        #putting values in empty data frame
        for(z in 1:b){
            for(i in 1:a){
                my_df[i,z]<-survey_data[i*b+z-b]
            }
        }
        
        # question 1 datframe
        my_txt1<-my_df$V1
        q1_df<-data_frame(line=1:a,text=my_txt1)
        print(q1_df)
        
        # question 2 datframe
        my_txt2<-my_df$V2
        q2_df<-data_frame(line=1:a,text=my_txt2)
        print(q2_df)
        
        # question 3 datframe
        my_txt3<-my_df$V3
        q3_df<-data_frame(line=1:a,text=my_txt3)
        print(q3_df)
        
        # question 4 datframe
        my_txt4<-my_df$V4
        q4_df<-data_frame(line=1:a,text=my_txt4)
        print(q4_df)
        
        # question 5 datframe
        my_txt5<-my_df$V5
        q5_df<-data_frame(line=1:a,text=my_txt5)
        print(q5_df)
        
        #Tokenization of questions
        token_q1 <- q1_df %>%
            unnest_tokens(word, text)%>%
            anti_join(stop_words) %>% 
            count(word, sort=T)
        token_q1
        
        token_q2 <- q2_df %>%
            unnest_tokens(word, text)%>%
            anti_join(stop_words) %>% 
            count(word, sort=T)
        token_q2
        
        token_q3 <- q3_df %>%
            unnest_tokens(word, text)%>%
            anti_join(stop_words) %>% 
            count(word, sort=T)
        token_q3
        
        token_q4 <- q4_df %>%
            unnest_tokens(word, text)%>%
            anti_join(stop_words) %>% 
            count(word, sort=T)
        token_q4
        # Not removing stop words for 
        token_q5 <- q5_df %>%
            unnest_tokens(word, text)%>%
            count(word, sort=T)
        token_q5
        #combining all of the dataframe into one dataframe
        survey_df <- bind_rows(mutate(token_q1, question="Question1"),
                               mutate(token_q2, question="Question2"),
                               mutate(token_q3, question="Question3"),
                               mutate(token_q4, question="Question4"),
                               mutate(token_q5, question="Question5")
        )
        
        #Sentiment Analysis
        #Getting code of each sentiment metrics
        afinn <-get_sentiments("afinn") #(-5 is most negative 5 is positive)
        nrc <-get_sentiments("nrc") #all emotions
        bing <-get_sentiments("bing")
        
        ### Mutating the 3 sentiment metric ###
        sentiments <- bind_rows(
            mutate(afinn, lexicon ="afinn"),
            mutate(nrc, lexicon = "nrc"),
            mutate(bing, lexicon = "bing"))
        
        
        #Afinn For question 1
        afinn_q1<-token_q1 %>%
            inner_join(get_sentiments("afinn"))
        #Afinn For question 2
        afinn_q2<-token_q2 %>%
            inner_join(get_sentiments("afinn"))
        #Afinn For question 3
        afinn_q3<-token_q3 %>%
            inner_join(get_sentiments("afinn"))
        #Afinn For question 4
        afinn_q4<-token_q4 %>%
            inner_join(get_sentiments("afinn"))
        #Afinn For question 5
        afinn_q5<-token_q5 %>%
            inner_join(get_sentiments("afinn"))
        
        #bing For question 1
        bing_q1<-token_q1 %>%
            inner_join(get_sentiments("bing"))
        #bing For question 2
        bing_q2<-token_q2 %>%
            inner_join(get_sentiments("bing"))
        #bing For question 3
        bing_q3<-token_q3 %>%
            inner_join(get_sentiments("bing"))
        #bing For question 4
        bing_q4<-token_q4 %>%
            inner_join(get_sentiments("bing"))
        #bing For question 5
        bing_q5<-token_q5 %>%
            inner_join(get_sentiments("bing"))
        
        #nrc For question 1
        nrc_q1<-token_q1 %>%
            inner_join(get_sentiments("nrc"))
        #nrc For question 2
        nrc_q2<-token_q2 %>%
            inner_join(get_sentiments("nrc"))
        #nrc For question 3
        nrc_q3<-token_q3 %>%
            inner_join(get_sentiments("nrc"))
        #nrc For question 4
        nrc_q4<-token_q4 %>%
            inner_join(get_sentiments("nrc"))
        #nrc For question 5
        nrc_q5<-token_q5 %>%
            inner_join(get_sentiments("nrc"))
        ### Creating the bi-grams tibble for each candidate ###
        
        # Question 1 
        q1_bigrams <- q1_df %>%
            unnest_tokens(bigram, text, token = "ngrams", n=2)
        
        q1_bigrams %>%
            count(bigram, sort = TRUE) #this has many stop words, need to remove them 
        
        ### Deleting the stop words from the bi-gram ###
        
        q1_bigrams_separated <- q1_bigrams %>%
            separate(bigram, c("word1", "word2"), sep = " ")
        
        q1_bigrams_filtered <- q1_bigrams_separated %>%
            filter(!word1 %in% stop_words$word) %>%
            filter(!word2 %in% stop_words$word)
        
        ### creating the new bigram, "no-stop-words" ###
        
        q1_bigram_counts <- q1_bigrams_filtered %>%
            count(word1, word2, sort = TRUE)
        
        ### 4 most common phrases #
        
        
        q1_bigram_united <- q1_bigrams_filtered %>%
            unite(bigram, word1, word2, sep=" ") #we need to unite what we split in the previous section
        
        q1_bigram_tf_idf <- q1_bigram_united %>%
            count(bigram, line) %>%
            bind_tf_idf(bigram, line, n) %>%
            arrange(desc(tf_idf))
        
        
        
        ### Plotting the bi-grams ###
        
        q1_bigram_graph <- q1_bigram_counts %>%
            filter(n>3) %>%
            graph_from_data_frame()
        
        
      
        
        
        # Question 2
        q2_bigrams <- q2_df %>%
            unnest_tokens(bigram, text, token = "ngrams", n=2)
        
        q2_bigrams %>%
            count(bigram, sort = TRUE) #this has many stop words, need to remove them 
        
        ### Deleting the stop words from the bi-gram ###
        
        q2_bigrams_separated <- q2_bigrams %>%
            separate(bigram, c("word1", "word2"), sep = " ")
        
        q2_bigrams_filtered <- q2_bigrams_separated %>%
            filter(!word1 %in% stop_words$word) %>%
            filter(!word2 %in% stop_words$word)
        
        ### creating the new bigram, "no-stop-words" ###
        
        q2_bigram_counts <- q2_bigrams_filtered %>%
            count(word1, word2, sort = TRUE)
        
        
        
        q2_bigram_united <- q2_bigrams_filtered %>%
            unite(bigram, word1, word2, sep=" ") #we need to unite what we split in the previous section
        
        q2_bigram_tf_idf <- q2_bigram_united %>%
            count(bigram, line) %>%
            bind_tf_idf(bigram, line, n) %>%
            arrange(desc(tf_idf))
        
     
        
        ### Plotting the bi-grams ###
      
        q2_bigram_graph <- q2_bigram_counts %>%
            filter(n>1) %>%
            graph_from_data_frame()
        
       
        
        
        # Question 3
        q3_bigrams <- q3_df %>%
            unnest_tokens(bigram, text, token = "ngrams", n=2)
        
        q3_bigrams %>%
            count(bigram, sort = TRUE) #this has many stop words, need to remove them 
        
        ### Deleting the stop words from the bi-gram ###
        
        q3_bigrams_separated <- q3_bigrams %>%
            separate(bigram, c("word1", "word2"), sep = " ")
        
        q3_bigrams_filtered <- q3_bigrams_separated %>%
            filter(!word1 %in% stop_words$word) %>%
            filter(!word2 %in% stop_words$word)
        
        ### creating the new bigram, "no-stop-words" ###
        
        q3_bigram_counts <- q3_bigrams_filtered %>%
            count(word1, word2, sort = TRUE)
        
        
        
        q3_bigram_united <- q3_bigrams_filtered %>%
            unite(bigram, word1, word2, sep=" ") #we need to unite what we split in the previous section
        
        q3_bigram_tf_idf <- q3_bigram_united %>%
            count(bigram, line) %>%
            bind_tf_idf(bigram, line, n) %>%
            arrange(desc(tf_idf))
        
       
        
        ### Plotting the bi-grams ###
       
        q3_bigram_graph <- q3_bigram_counts %>%
            filter(n>1) %>%
            graph_from_data_frame()
        
        
        
        # Question 4
        q4_bigrams <- q4_df %>%
            unnest_tokens(bigram, text, token = "ngrams", n=2)
        
        q4_bigrams %>%
            count(bigram, sort = TRUE) #this has many stop words, need to remove them 
        
        ### Deleting the stop words from the bi-gram ###
        
        q4_bigrams_separated <- q4_bigrams %>%
            separate(bigram, c("word1", "word2"), sep = " ")
        
        q4_bigrams_filtered <- q4_bigrams_separated %>%
            filter(!word1 %in% stop_words$word) %>%
            filter(!word2 %in% stop_words$word)
        
        ### creating the new bigram, "no-stop-words" ###
        
        q4_bigram_counts <- q4_bigrams_filtered %>%
            count(word1, word2, sort = TRUE)
        
        ### 4 most common phrases #
        
        
        q4_bigram_united <- q4_bigrams_filtered %>%
            unite(bigram, word1, word2, sep=" ") #we need to unite what we split in the previous section
        
        q4_bigram_tf_idf <- q4_bigram_united %>%
            count(bigram, line) %>%
            bind_tf_idf(bigram, line, n) %>%
            arrange(desc(tf_idf))
       
        
        ### Plotting the bi-grams ###
       
        q4_bigram_graph <- q4_bigram_counts %>%
            filter(n>1) %>%
            graph_from_data_frame()
        
       
        
        # Question 5
        q5_bigrams <- q5_df %>%
            unnest_tokens(bigram, text, token = "ngrams", n=2)
        
        q5_bigrams %>%
            count(bigram, sort = TRUE) #this has many stop words, need to remove them 
        
        ### Deleting the stop words from the bi-gram ###
        
        q5_bigrams_separated <- q5_bigrams %>%
            separate(bigram, c("word1", "word2"), sep = " ")
        
        q5_bigrams_filtered <- q5_bigrams_separated %>%
            filter(!word1 %in% stop_words$word) %>%
            filter(!word2 %in% stop_words$word)
        
        ### creating the new bigram, "no-stop-words" ###
        
        q5_bigram_counts <- q5_bigrams_filtered %>%
            count(word1, word2, sort = TRUE)
        
        ### 4 most common phrases #
        
        
        
        q5_bigram_united <- q5_bigrams_filtered %>%
            unite(bigram, word1, word2, sep=" ") #we need to unite what we split in the previous section
        
        q5_bigram_tf_idf <- q5_bigram_united %>%
            count(bigram, line) %>%
            bind_tf_idf(bigram, line, n) %>%
            arrange(desc(tf_idf))
       
        
        ### Plotting the bi-grams ###
       
        q5_bigram_graph <- q5_bigram_counts %>%
            filter(n>1) %>%
            graph_from_data_frame()
      
     
      
       
        
        
         ################################################################
        
        
         #Frequency graph for bing 
        
      
        output$bingQ1 <- renderPlot({
            
            if (input$Question=="Question1"){
                if (input$Graph=="Bing Sentiment"){  
                bing_q1 %>%
                    group_by(sentiment) %>%
                    top_n(5) %>%
                    ungroup() %>%
                    mutate(word=reorder(word, n)) %>%
                    ggplot(aes(word, n, fill=sentiment)) +
                    geom_col(show.legend = FALSE) +
                    facet_wrap(~sentiment, scales = "free_y")+
                    labs(y="Contribution to sentiment", x=NULL)+
                    coord_flip()
              } } 
                else if (input$Question=="Question2"){
                    if (input$Graph=="Bing Sentiment"){
                    bing_q2 %>%
                        group_by(sentiment) %>%
                        top_n(5) %>%
                        ungroup() %>%
                        mutate(word=reorder(word, n)) %>%
                        ggplot(aes(word, n, fill=sentiment)) +
                        geom_col(show.legend = FALSE) +
                        facet_wrap(~sentiment, scales = "free_y")+
                        labs(y="Contribution to sentiment", x=NULL)+
                        coord_flip()}}
                    else if (input$Question=="Question3"){
                        if (input$Graph=="Bing Sentiment"){
                        bing_q3 %>%
                            group_by(sentiment) %>%
                            top_n(5) %>%
                            ungroup() %>%
                            mutate(word=reorder(word, n)) %>%
                            ggplot(aes(word, n, fill=sentiment)) +
                            geom_col(show.legend = FALSE) +
                            facet_wrap(~sentiment, scales = "free_y")+
                            labs(y="Contribution to sentiment", x=NULL)+
                           coord_flip()
                    }}
            else if (input$Question=="Question4"){
                if (input$Graph=="Bing Sentiment"){
                bing_q4 %>%
                    group_by(sentiment) %>%
                    top_n(5) %>%
                    ungroup() %>%
                    mutate(word=reorder(word, n)) %>%
                    ggplot(aes(word, n, fill=sentiment)) +
                    geom_col(show.legend = FALSE) +
                    facet_wrap(~sentiment, scales = "free_y")+
                    labs(y="Contribution to sentiment", x=NULL)+
                    coord_flip()
            }}
            else if (input$Question=="Question5"){ 
                if (input$Graph=="Bing Sentiment"){
                bing_q5 %>%
                    group_by(sentiment) %>%
                    top_n(5) %>%
                    ungroup() %>%
                    mutate(word=reorder(word, n)) %>%
                    ggplot(aes(word, n, fill=sentiment)) +
                    geom_col(show.legend = FALSE) +
                    facet_wrap(~sentiment, scales = "free_y")+
                    labs(y="Contribution to sentiment", x=NULL)+
                    coord_flip()
                }}
                
                } )
       #######NRC GRAPH 
        output$nrcQ <- renderPlot({
            
            if (input$Question=="Question1"){
                if (input$Graph=="NRC Sentiment"){
                nrc_q1 %>%
                    group_by(sentiment) %>%
                    top_n(5) %>%
                    ungroup() %>%
                    mutate(word=reorder(word, n)) %>%
                    ggplot(aes(word, n, fill=sentiment)) +
                    geom_col(show.legend = FALSE) +
                    facet_wrap(~sentiment, scales = "free_y")+
                    labs(y="Contribution to sentiment", x=NULL)+
                    coord_flip()}}
            else if (input$Question=="Question2"){
                if (input$Graph=="NRC Sentiment"){
                nrc_q2 %>%
                    group_by(sentiment) %>%
                    top_n(5) %>%
                    ungroup() %>%
                    mutate(word=reorder(word, n)) %>%
                    ggplot(aes(word, n, fill=sentiment)) +
                    geom_col(show.legend = FALSE) +
                    facet_wrap(~sentiment, scales = "free_y")+
                    labs(y="Contribution to sentiment", x=NULL)+
                    coord_flip()}}
            else if (input$Question=="Question3"){
                if (input$Graph=="NRC Sentiment"){
                nrc_q3 %>%
                    group_by(sentiment) %>%
                    top_n(5) %>%
                    ungroup() %>%
                    mutate(word=reorder(word, n)) %>%
                    ggplot(aes(word, n, fill=sentiment)) +
                    geom_col(show.legend = FALSE) +
                    facet_wrap(~sentiment, scales = "free_y")+
                    labs(y="Contribution to sentiment", x=NULL)+
                    coord_flip()
            }}
            else if (input$Question=="Question4"){
                if (input$Graph=="NRC Sentiment"){
                nrc_q4 %>%
                    group_by(sentiment) %>%
                    top_n(5) %>%
                    ungroup() %>%
                    mutate(word=reorder(word, n)) %>%
                    ggplot(aes(word, n, fill=sentiment)) +
                    geom_col(show.legend = FALSE) +
                    facet_wrap(~sentiment, scales = "free_y")+
                    labs(y="Contribution to sentiment", x=NULL)+
                    coord_flip()
            }}
            else if (input$Question=="Question5"){ 
                if (input$Graph=="NRC Sentiment"){
                nrc_q5 %>%
                    group_by(sentiment) %>%
                    top_n(5) %>%
                    ungroup() %>%
                    mutate(word=reorder(word, n)) %>%
                    ggplot(aes(word, n, fill=sentiment)) +
                    geom_col(show.legend = FALSE) +
                    facet_wrap(~sentiment, scales = "free_y")+
                    labs(y="Contribution to sentiment", x=NULL)+
                    coord_flip()
                
            } }})
######## NB
        
        
        survey_data <- read_document(file="C:/Users/bhavya garg/Desktop/semester 1/R class 1/full version survey data.txt")
        
        
        
        #creating empty data frame
        a<-40
        b<-5
        my_df = as.data.frame(matrix(ncol = b, nrow = a))
        
        #putting values in empty data frame
        for(z in 1:b){
            for(i in 1:a){
                my_df[i,z]<-survey_data[i*b+z-b]
            }
        }
        
        # Transposing dataframe #
        transpose <- t(my_df)
        
        # Changing the transpose into a dataframe #
        t_df <- as.data.frame(transpose)
        
        # Changing it into a string #
        my_txt_1 <- t_df$V1
        my_txt_2 <- t_df$V2
        my_txt_3 <- t_df$V3
        my_txt_4 <- t_df$V4
        my_txt_5 <- t_df$V5
        my_txt_6 <- t_df$V6
        my_txt_7 <- t_df$V7
        my_txt_8 <- t_df$V8
        my_txt_9 <- t_df$V9
        my_txt_10 <- t_df$V10
        my_txt_11 <- t_df$V11
        my_txt_12 <- t_df$V12
        my_txt_13 <- t_df$V13
        my_txt_14 <- t_df$V14
        my_txt_15 <- t_df$V15
        my_txt_16 <- t_df$V16
        my_txt_17 <- t_df$V17
        my_txt_18 <- t_df$V18
        my_txt_19 <- t_df$V19
        my_txt_20 <- t_df$V20
        my_txt_21 <- t_df$V21
        my_txt_22 <- t_df$V22
        my_txt_23 <- t_df$V23
        my_txt_24 <- t_df$V24
        my_txt_25 <- t_df$V25
        my_txt_26 <- t_df$V26
        my_txt_27 <- t_df$V27
        my_txt_28 <- t_df$V28
        my_txt_29 <- t_df$V29
        my_txt_30 <- t_df$V30
        my_txt_31 <- t_df$V31
        my_txt_32 <- t_df$V32
        my_txt_33 <- t_df$V33
        my_txt_34 <- t_df$V34
        my_txt_35 <- t_df$V35
        my_txt_36 <- t_df$V36
        my_txt_37 <- t_df$V37
        my_txt_38 <- t_df$V38
        my_txt_39 <- t_df$V39
        my_txt_40 <- t_df$V40
        
        a = 5
        
        # Making each text into a dataframe #
        mydf_1 <- data_frame(line=1:a, text=my_txt_1)
        mydf_2 <- data_frame(line=1:a, text=my_txt_2)
        mydf_3 <- data_frame(line=1:a, text=my_txt_3)
        mydf_4 <- data_frame(line=1:a, text=my_txt_4)
        mydf_5 <- data_frame(line=1:a, text=my_txt_5)
        mydf_6 <- data_frame(line=1:a, text=my_txt_6)
        mydf_7 <- data_frame(line=1:a, text=my_txt_7)
        mydf_8 <- data_frame(line=1:a, text=my_txt_8)
        mydf_9 <- data_frame(line=1:a, text=my_txt_9)
        mydf_10 <- data_frame(line=1:a, text=my_txt_10)
        mydf_11 <- data_frame(line=1:a, text=my_txt_11)
        mydf_12 <- data_frame(line=1:a, text=my_txt_12)
        mydf_13 <- data_frame(line=1:a, text=my_txt_13)
        mydf_14 <- data_frame(line=1:a, text=my_txt_14)
        mydf_15 <- data_frame(line=1:a, text=my_txt_15)
        mydf_16 <- data_frame(line=1:a, text=my_txt_16)
        mydf_17 <- data_frame(line=1:a, text=my_txt_17)
        mydf_18 <- data_frame(line=1:a, text=my_txt_18)
        mydf_19 <- data_frame(line=1:a, text=my_txt_19)
        mydf_20 <- data_frame(line=1:a, text=my_txt_20)
        mydf_21 <- data_frame(line=1:a, text=my_txt_21)
        mydf_22 <- data_frame(line=1:a, text=my_txt_22)
        mydf_23 <- data_frame(line=1:a, text=my_txt_23)
        mydf_24 <- data_frame(line=1:a, text=my_txt_24)
        mydf_25 <- data_frame(line=1:a, text=my_txt_25)
        mydf_26 <- data_frame(line=1:a, text=my_txt_26)
        mydf_27 <- data_frame(line=1:a, text=my_txt_27)
        mydf_28 <- data_frame(line=1:a, text=my_txt_28)
        mydf_29 <- data_frame(line=1:a, text=my_txt_29)
        mydf_30 <- data_frame(line=1:a, text=my_txt_30)
        mydf_31 <- data_frame(line=1:a, text=my_txt_31)
        mydf_32 <- data_frame(line=1:a, text=my_txt_32)
        mydf_33 <- data_frame(line=1:a, text=my_txt_33)
        mydf_34 <- data_frame(line=1:a, text=my_txt_34)
        mydf_35 <- data_frame(line=1:a, text=my_txt_35)
        mydf_36 <- data_frame(line=1:a, text=my_txt_36)
        mydf_37 <- data_frame(line=1:a, text=my_txt_37)
        mydf_38 <- data_frame(line=1:a, text=my_txt_38)
        mydf_39 <- data_frame(line=1:a, text=my_txt_39)
        mydf_40 <- data_frame(line=1:a, text=my_txt_40)
        
        
        frequency_person <- bind_rows(mutate(mydf_1, person = 'person 01'),
                                      mutate(mydf_2, person = 'person 02'),
                                      mutate(mydf_3, person = 'person 03'),
                                      mutate(mydf_4, person = 'person 04'),
                                      mutate(mydf_5, person = 'person 05'),
                                      mutate(mydf_6, person = 'person 06'),
                                      mutate(mydf_7, person = 'person 07'),
                                      mutate(mydf_8, person = 'person 08'),
                                      mutate(mydf_9, person = 'person 09'),
                                      mutate(mydf_10, person = 'person 10'),
                                      mutate(mydf_11, person = 'person 11'),
                                      mutate(mydf_12, person = 'person 12'),
                                      mutate(mydf_13, person = 'person 13'),
                                      mutate(mydf_14, person = 'person 14'),
                                      mutate(mydf_15, person = 'person 15'),
                                      mutate(mydf_16, person = 'person 16'),
                                      mutate(mydf_17, person = 'person 17'),
                                      mutate(mydf_18, person = 'person 18'),
                                      mutate(mydf_19, person = 'person 19'),
                                      mutate(mydf_20, person = 'person 20'),
                                      mutate(mydf_21, person = 'person 21'),
                                      mutate(mydf_22, person = 'person 22'),
                                      mutate(mydf_23, person = 'person 23'),
                                      mutate(mydf_24, person = 'person 24'),
                                      mutate(mydf_25, person = 'person 25'),
                                      mutate(mydf_26, person = 'person 26'),
                                      mutate(mydf_27, person = 'person 27'),
                                      mutate(mydf_28, person = 'person 28'),
                                      mutate(mydf_29, person = 'person 29'),
                                      mutate(mydf_30, person = 'person 30'),
                                      mutate(mydf_31, person = 'person 31'),
                                      mutate(mydf_32, person = 'person 32'),
                                      mutate(mydf_33, person = 'person 33'),
                                      mutate(mydf_34, person = 'person 34'),
                                      mutate(mydf_35, person = 'person 35'),
                                      mutate(mydf_36, person = 'person 36'),
                                      mutate(mydf_37, person = 'person 37'),
                                      mutate(mydf_38, person = 'person 38'),
                                      mutate(mydf_39, person = 'person 39'),
                                      mutate(mydf_40, person = 'person 40')
        )
        
        survey_dfm <- frequency_person %>%
            group_by(person) %>%
            unnest_tokens(word, text) %>% 
            anti_join(stop_words) %>% 
            count(word) %>%
            cast_dfm(person, word, n) 
        
   
        
        # Setting the training and testing data #
        survey_dfm.train <-survey_dfm[1:30,]
        survey_dfm.test <-survey_dfm[31:40,]
        
        # Creating a vector for the busines success #
       # business_success <- c(0,1,0,0,1,1,1,1,1,0,0,1,1,1,1,1,1,0,1,1,0,1,1,0,0,0,1,0,1,0,1,0,0,0,1,0,0,0,0,0)
        
        #building the Naive Bayes model:
        NB_classifier <- textmodel_nb(survey_dfm.train, c(0,1,0,0,1,1,1,1,1,0,0,1,1,1,1,1,1,0,1,1,0,1,1,0,0,0,1,0,1,0))
        NB_classifier
       
        
        # predicting the testing data
        pred <- predict(NB_classifier, survey_dfm.test)
        pred
        
        
###################Afinn
        output$afinn <- renderPlot({
            
            if (input$Question=="Question1"){
                if (input$Graph=="Afinn Sentiment"){
                    afinn_q1 %>%
                        group_by(value) %>%
                        top_n(5) %>%
                        ungroup() %>%
                        mutate(word=reorder(word, n)) %>%
                        ggplot(aes(word, n, fill=value)) +
                        geom_col(show.legend = FALSE) +
                        facet_wrap(~value, scales = "free_y")+
                        labs(y="Contribution to sentiment", x=NULL)+
                        coord_flip()}}
            else if (input$Question=="Question2"){
                if (input$Graph=="Afinn Sentiment"){
                    afinn_q2 %>%
                        group_by(value) %>%
                        top_n(5) %>%
                        ungroup() %>%
                        mutate(word=reorder(word, n)) %>%
                        ggplot(aes(word, n, fill=value)) +
                        geom_col(show.legend = FALSE) +
                        facet_wrap(~value, scales = "free_y")+
                        labs(y="Contribution to sentiment", x=NULL)+
                        coord_flip()}}
            else if (input$Question=="Question3"){
                if (input$Graph=="Afinn Sentiment"){
                    afinn_q3 %>%
                        group_by(value) %>%
                        top_n(5) %>%
                        ungroup() %>%
                        mutate(word=reorder(word, n)) %>%
                        ggplot(aes(word, n, fill=value)) +
                        geom_col(show.legend = FALSE) +
                        facet_wrap(~value, scales = "free_y")+
                        labs(y="Contribution to sentiment", x=NULL)+
                        coord_flip()
                }}
            else if (input$Question=="Question4"){
                if (input$Graph=="Afinn Sentiment"){
                    afinn_q4 %>%
                        group_by(value) %>%
                        top_n(5) %>%
                        ungroup() %>%
                        mutate(word=reorder(word, n)) %>%
                        ggplot(aes(word, n, fill=value)) +
                        geom_col(show.legend = FALSE) +
                        facet_wrap(~value, scales = "free_y")+
                        labs(y="Contribution to sentiment", x=NULL)+
                        coord_flip()
                }}
            else if (input$Question=="Question5"){ 
                if (input$Graph=="Afinn Sentiment"){
                    afinn_q5 %>%
                        group_by(value) %>%
                        top_n(5) %>%
                        ungroup() %>%
                        mutate(word=reorder(word, n)) %>%
                        ggplot(aes(word, n, fill=value)) +
                        geom_col(show.legend = FALSE) +
                        facet_wrap(~value, scales = "free_y")+
                        labs(y="Contribution to sentiment", x=NULL)+
                        coord_flip()
                    
                } }})
        #Frequency graph for bigram
        output$ngram <- renderPlot({
            
            if (input$Question=="Question1"){
                if (input$Graph=="N-gram"){  
                    ggraph(q1_bigram_graph, layout = "fr") +
                        geom_edge_link()+
                        geom_node_point()+
                        geom_node_text(aes(label=name), vjust =1, hjust=1)
                } } 
            else if (input$Question=="Question2"){
                if (input$Graph=="N-gram"){
                    
                    ggraph(q2_bigram_graph, layout = "fr") +
                        geom_edge_link()+
                        geom_node_point()+
                        geom_node_text(aes(label=name), vjust =1, hjust=1)}}
            else if (input$Question=="Question3"){
                if (input$Graph=="N-gram"){
                    
                    ggraph(q3_bigram_graph, layout = "fr") +
                        geom_edge_link()+
                        geom_node_point()+
                        geom_node_text(aes(label=name), vjust =1, hjust=1)
                    
                }}
            else if (input$Question=="Question4"){
                if (input$Graph=="N-gram"){
                    
                    ggraph(q4_bigram_graph, layout = "fr") +
                        geom_edge_link()+
                        geom_node_point()+
                        geom_node_text(aes(label=name), vjust =1, hjust=1)
                    
                }}
            else if (input$Question=="Question5"){ 
                if (input$Graph=="N-gram"){
                    ggraph(q5_bigram_graph, layout = "fr") +
                        geom_edge_link()+
                        geom_node_point()+
                        geom_node_text(aes(label=name), vjust =1, hjust=1) 
                    
                }}
            
        } )
        output$naive <- renderPlot({
           
            plot(pred, color="BLUE")
        })
        output$introduction  <- renderImage({
            
            filename<-normalizePath(file.path('C:/Users/bhavya garg/Desktop/semester 1/R class /text_analyt_app/image',paste('as.png',sep='')))
            list(src=filename)},
            deleteFile=FALSE)
        
        output$counts <- renderPlot({
          if (input$Question=="Question1"){
            token_q1 %>%
              mutate(word=reorder(word,n)) %>%
              top_n(7) %>%
              ggplot(aes(word, n))+
              geom_col()+
              xlab(NULL)+
              coord_flip()}
          else if (input$Question=="Question2"){
            token_q2 %>%
              mutate(word=reorder(word,n)) %>%
              top_n(7) %>%
              ggplot(aes(word, n))+
              geom_col()+
              xlab(NULL)+
              coord_flip()}
          else if (input$Question=="Question3"){
            token_q3 %>%
              mutate(word=reorder(word,n)) %>%
              top_n(7) %>%
              ggplot(aes(word, n))+
              geom_col()+
              xlab(NULL)+
              coord_flip()}
          else if (input$Question=="Question4"){
            token_q4 %>%
              mutate(word=reorder(word,n)) %>%
              top_n(7) %>%
              ggplot(aes(word, n))+
              geom_col()+
              xlab(NULL)+
              coord_flip()}
          else if (input$Question=="Question5"){
            token_q5 %>%
              mutate(word=reorder(word,n)) %>%
              top_n(7) %>%
              ggplot(aes(word, n))+
              geom_col()+
              xlab(NULL)+
              coord_flip()}}
        )
       
    })
