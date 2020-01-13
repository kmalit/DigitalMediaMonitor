##########################################################################################################################
# This APP performs Analysis on twitter stories as well as doing the story's network analysis to provide you with
# intelligence on: "what is being said', 'Is it good/bad",'what exactly is being said', 'How is the story spreading'.
##########################################################################################################################
#
# Define server logic required perform the analysis:

shinyServer(function(session, input, output) {
  
  # HOME
  {
    
  }
  
  # OVERVIEW
  {
    
  }
  
  # TWITTER
  {
    # TRENDS 
    {
      # GETTING THE TREND LOCATIONS
      tlocaInput <- reactive({
        string <- as.String(input$country)
        tlocOutput <- trendloc (string)
        colnames(tlocOutput) <- c("Place", "Country", "Search_ID")
        tlocOutput})
      output$trendlocationTable = renderDataTable ({tlocaInput()},options = list(lengthMenu = c(10, 15, 20), pageLength = 10))
      
      # UPDATING THE SELECTION LIST FOR THE TRENDS
      observeEvent(input$country,
                   updateSelectInput(session,"LocT", 'select Location ID for trend',choices = {
                     string <- as.String(input$country)
                     tid <- tid %>% filter (tid$country == string) 
                     sort(tid$name)}))
      
      # GETTING THE TREND FOR THE LOCATION OF CHOICE
      trendsInput <- reactive({
        t<- tid %>% filter (tid$name==input$LocT)
        t<-t$woeid
        tOutput <- trends (t)
        tOutput <- tOutput[,c(1,2)]
        colnames(tOutput) <- c("Trend Topic", "URL")
        tOutput})
      output$trendTable = renderDataTable ({trendsInput()}, options = list(lengthMenu = c(10, 15, 20), pageLength = 10))
      
      # JUMPING TO THE TWEET TABLE UPON SELECTION OF DATA PARAMETERS
      observeEvent(input$goButton, {
        updateTabsetPanel(session, "Tgetdata",selected = "TWEETS")
      })  }
    
    # GETTING THE DATA
    data1 = eventReactive(input$goButton, {
      
      if (input$typeInput == "hashtag") {
        df <- searchTwitter(input$hashtagInput,n=input$numberInput,since=NULL, until=NULL)
        searchterm  <- input$hashtagInput
      }
      
      else if (input$typeInput == "username") {
        df <- userTimeline(input$usernameInput,n = input$numberInput)
        searchterm  <- input$usernameInput
      }
      
      else {}
      
      #Cleaning the tweets
      df <- prepDataFile (df,searchterm)
      df
    })
    
    # GETTING THE TABLE WITH THE TWEETS
    
    output$trendTable = renderDataTable ({trendsInput()},
                                         options = list(lengthMenu = c(10, 15, 20), 
                                                        pageLength = 10))
    output$tweetTable = DT::renderDataTable({
      df <- data1()
      df <- df[,c(1,4,7,2,8)]
      #colnames(df) <- c("Tweets", "Date", "Username", "Fav Count", "RT Count", "Location")
      colnames(df) <- c("Tweets", "Date", "Username", "Fav Count", "RT Count")
      df}, 
      options = list(lengthMenu = c(10, 15, 20), pageLength = 5,searching = FALSE, lengthChange = FALSE))
    
    observeEvent(input$analyzeButton , { updateTabItems  (session, "tabs", selected = "Ttanalytics")})
    
    # CLEANING THE TWEETS TEXT
    data2 = reactive({
      df <- data1()
      df$text <- cleanTweetText (df$text)
      #df$text <- cleanTweetText (df$Tweets)
      df
    })
    
    # GETTING THE TERM DOCUMENT MATRIX
    data3 = reactive({
      data <- data2()
      data <- data$text
      data <- tdm (data)
      data
    })
    
    # GETTING THE MELTED TERM DOCUMENT MATRIX
    data4 = reactive({
      tdm <- data3()
      tdm <- melttdm (tdm)
      tdm
    })
    
    # DATA ANALYTICS
    {
      
      output$Hashtagbeinganalysed <- renderValueBox({
        valueBox("Hashtag analyzed:", h2(input$hashtagInput) , icon = icon("hashtag"))
      })
      
      output$Tweetscount <- renderValueBox({
        valueBox("Tweet Count analyzed:", h2(nrow(data1())) , icon = icon("twitter"))
      })
      
      output$Tweetsentiment <- renderValueBox({
        valueBox("Avg. Sentiment Score:", icon = icon("twitter"), h2( round(mean(Tsentimentdf()$syuzhet),2)))
      })
      
      # TWEETS CONTENT ANALYTICS
      {
        #PLOT BY TIME OF DAY
        output$Thistogram1 <- renderPlotly({
          df <- data1()
          g <- ggplot(data = df, aes(x = df$day_n_hour, fill = factor(isRetweet)))+
            geom_bar(stat = "count", colour="black") + 
            scale_fill_manual(values = c("green","tomato"))+
            theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1),legend.position="none",panel.grid.major = element_blank(),
                               panel.grid.minor = element_blank(),panel.border = element_blank(),panel.background = element_blank()) +
            labs(x="Day & Hour", y="frequency_of_tweets", title="Count of Tweet(green) & Rewteet (Red) by day & hour")
          (gg <- ggplotly(g))
        })
        
        # WORD COUNT BAR PLOT
        output$wordbar1 = renderPlot({
          tdm <- data3()
          minfreq <- input$minfreq2
          wordplot <- wordbarplot(tdm, minfreq)
          wordplot
        })
        
        # GETTING THE WORD CLOUD OUTPUT
        output$wordCloud1 = renderPlot ({
          tdm <- data3()
          minfreq <- input$minfreq1
          maxnum  <- input$maxword1
          wcld1 <- wordcloudgen (tdm,minfreq, maxnum)
          wcld1
        })
        
        # STORY WORD NETWORK OUTPUT
        filter1 <- eventReactive(input$AnalyzewordNetwork, {filter <- input$filter},ignoreNULL = F )
        correlation1 <- eventReactive(input$AnalyzewordNetwork, {correlation <- input$correltn},ignoreNULL = F )
        
        output$wordCloud2 = renderPlot({
          tdm <-data4()
          filter <- filter1()
          correlation <- correlation1()
          wcld2 <- story(tdm,filter,correlation)
          wcld2
        })
        
        # WORD CORRELATION BAR PLOT
        
        topic1 <- eventReactive(input$Analyzecorr, {topic <- input$topic},ignoreNULL = F )
        correlation2 <- eventReactive(input$Analyzecorr, {correlation2 <- input$correltn2},ignoreNULL = F )
        
        output$corrplot1 = renderPlotly({
          meltedtdm <-data4()
          topic <- topic1()
          correlation <- correlation2()
          corrplot1 <-wordcorr (meltedtdm, topic, correlation)
          corrplot1
          
        })
      }
      
      # SENTIMENT ANALYTICS
      {
        Tsentimentdf <- reactive({
          df <- data2()
          sent <- scoreSentiment(df)
          sent
        })
        
        output$Tsentplot1 = renderPlot({
          
          df   <- Tsentimentdf()
          df   <- df[,c("anger",'anticipation','disgust','fear','joy','sadness','surprise','trust','negative','positive')]
          Tall <- df[, order(colSums(-df))]
          barplot(colSums(Tall),
                  las = 2,
                  col = rainbow(10),
                  ylab = "Count",
                  main = "OVERALL SENTIMENT SCORE")
        })
        
        output$Tsentplot2 = renderPlot({
          df <- Tsentimentdf() 
          threshold <- 0
          plot <- emotionanalysis(df,threshold)
          plot
          
        })
        
        Tsentimentdf2 <- reactive({
          
          users = ddply(Tsentimentdf(), ~ screenName, summarize, num_tweets = length(positive), ave_sentiment = mean(syuzhet),
                        ave_negative = mean(negative), ave_positive = mean(positive), ave_anger = mean(anger))
          users
          
        })
        
        output$Tsentplot3 = renderPlotly({
          
          g <- ggplot(Tsentimentdf2(), aes(x= as.factor(num_tweets), y=ave_sentiment)) + 
            geom_boxplot(colour = "#3366FF", outlier.colour = "red", outlier.shape = 1, fill = "tomato")+
            theme_bw()+ theme(axis.text.x = element_text(angle = 90, hjust = 1),legend.position="right",panel.grid.major = element_blank(),
                              panel.grid.minor = element_blank(),panel.border = element_blank(),panel.background = element_blank())+
            labs(x = "Frequency of tweeting", y = "Sentiment", title = "Ditsribution of Sentiment versus Frequency of Tweeting")
          (gg <- ggplotly(g))
        })
        
      }  
      
      # INFLUENCER NETWORK ANALYTICS
      {
        # TOP TWEETS INITIALIZERS
        output$Thistogram2 <- renderPlotly({
          df1 <- data1()
          topn1 <- input$Toptweeps
          
          df1 %>% filter(isRetweet=="FALSE") %>% group_by(screenName) %>% summarise(nr = length(favoriteCount)) %>%  
            top_n(n=topn1) %>% arrange (nr) %>% ungroup() ->da
          
          g <- ggplot(data = da, aes(x = reorder(factor(screenName),-nr), y = nr))+
            geom_bar( stat = "identity", fill="green", colour="black") +  
            theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1),legend.position="right",panel.grid.major = element_blank(),
                               panel.grid.minor = element_blank(),panel.border = element_blank(),panel.background = element_blank()) +
            labs(x="Users", y="Number of tweets initialized", title="Top Users initializing tweets on the topic")
          (gg <- ggplotly(g))
        })
        
        # TOP RETWEETERS  
        output$Thistogram3 <- renderPlotly({
          df2 <- data1()
          topn2 <- input$TopRetweeps
          
          df2 %>% filter(isRetweet=="TRUE") %>% group_by(screenName) %>% summarise(nr = length(favoriteCount)) %>%  
            top_n(n=topn2) %>% arrange (nr) %>% ungroup() ->db
          
          g <- ggplot(data = db, aes(x = reorder(factor(screenName),-nr), y = nr))+
            geom_bar( stat = "identity", fill="tomato", colour="black") +  
            theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1),legend.position="right",panel.grid.major = element_blank(),
                               panel.grid.minor = element_blank(),panel.border = element_blank(),panel.background = element_blank()) +
            labs(x="Users", y="Number of times retweeted", title="Top Retweeters on the topic")
          (gg <- ggplotly(g))
        })
        
        # TOP RETWEETED USERS 
        output$Thistogram4 <- renderPlotly({
          df3 <- data1()
          topn3 <- input$TopRetweeted
          
          df3 %>% group_by(screenName) %>% summarise(nr = sum(retweetCount)) %>% top_n(n=topn3) %>% arrange (nr) %>% ungroup() ->dc
          
          g <- ggplot(data = dc, aes(x = reorder(factor(screenName),-nr), y = nr))+
            geom_bar( stat = "identity", fill="tomato", colour="black") +  
            theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1),legend.position="right",panel.grid.major = element_blank(),
                               panel.grid.minor = element_blank(),panel.border = element_blank(),panel.background = element_blank()) +
            labs(x="Users", y="Number of times retweeted", title="Top Retweeted Users")
          (gg <- ggplotly(g))
        })
        
        # TWEET RESPONSE COUNT 
        output$Thistogram5 <- renderPlotly({
          df4 <- data1()
          
          df4 %>% filter(!is.na(replyToSN))-> dd
          
          g <- ggplot(data = dd, aes(x = fct_infreq(replyToSN)))+
            geom_bar(stat = "count", fill="gold", colour="black") +  theme_bw() + 
            theme(axis.text.x = element_text(angle = 90, hjust = 1),legend.position="right",panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),panel.border = element_blank(),panel.background = element_blank()) +
            labs(x="Entity being responded to :", y="frequency", title="Tweet Response Count")
          (gg <- ggplotly(g))
          
        })
        
        # TWEET RETWEET NETWORK
        output$network1 = renderPlot({
          data <- data1()
          
          df <- data.frame(data$screenName,data$pretweeted)
          net <- graph.data.frame(df,directed = T)
          
          V(net)$label <- V(net)$name
          V(net)$degree <- degree(net)
          V(net)$degreein <- degree(net, mode = 'in')
          V(net)$degreeout <- degree(net, mode = 'out')
          V(net)$degreeall <- degree(net, mode = 'all')
          
          type <- if (input$tinfluencetype=="BOTH") {V(net)$degreeall} 
          else{ if (input$tinfluencetype == "Receivers") {V(net)$degreein} 
            else {V(net)$degreeout}}
          
          
          net.copy <- delete.vertices(net, which(type <= input$tinfluence))
          
          
          glay = layout.fruchterman.reingold(net.copy, niter=10000, area=30*vcount(net.copy)^2)
          plot(net.copy, layout= glay,
               vertex.color = 'green',
               vertex.size = 0.1 ,
               edge.size = 0.001,
               edge.arrow.size = 0.1,
               edge.color = 'lightblue',
               vertex.label.cex = 1* log( V(net.copy)$degree), 
               vertex.label.font=3,
               vertex.label.color = brewer.pal(8, 'Dark2')
          )
          
          
          
          
        })
        
      }
    }
    
  } 
  
  # FACEBOOK
  {
    
  }  
  
  # INSTAGRAM
  {
    
  }
  
  # PRINT MEDIA
  {
    
  }
  
  # RADIO
  {
    
  }
  
  # BLOGS
  {
    
  } 
  
})