##########################################################################################################################################
# This APP performs Analysis on twitter stories as well as doing the story's network analysis to provide you with
# intelligence on: "what is being said', 'Is it good/bad",'what exactly is being said', 'How is the story spreading'.
##########################################################################################################################################
# 
#   
##########################################################################################################################################
##########################################################################################################################################

# HEADER
{
  header <- dashboardHeader(title = "SOCIAL MEDIA MONITOR: Information, sentiment & Influencers' network Analysis", disable = FALSE,titleWidth = 650,
                            dropdownMenuOutput("messageMenu")
                            
  )
}

# SIDEBAR
{
  sidebar <-  dashboardSidebar(disable = FALSE,width =180,
                               sidebarMenu(id = "tabs",
                                           menuItem("HOME", tabName = "home", icon = icon("home")),
                                           
                                           menuItem("TWITTER", tabName = "twitter", icon = icon("twitter"),startExpanded = TRUE, #style="color: #fff; background-color: #337ab7",
                                                    
                                                    menuSubItem("Get Data", tabName = "Tgetdata", selected = TRUE),
                                                    menuSubItem("Tweets Analytics", tabName = "Ttanalytics"),
                                                    menuSubItem("User Analytics", tabName = "Tuseranalytics")
                                                    
                                           ),
                                           
                                           menuItem("FACEBOOK", tabName = "facebook", icon = icon("facebook"),  badgeLabel = "new", badgeColor = "green"),
                                           
                                           menuItem("INSTAGRAM" , tabName = "instagram", icon = icon("instagram"),badgeLabel = "new", badgeColor = "green"),
                                           
                                           menuItem("PRINT MEDIA", tabName = "print_media", icon = icon("wpforms"), badgeLabel = "new", badgeColor = "green"),
                                           
                                           menuItem("BLOGS", tabName = "blogs", icon = icon("rss"), badgeLabel = "new", badgeColor = "green")
                               )
  )    
}

# BODY
{
  body <- dashboardBody(
    tabItems(
      # HOME
      {
        tabItem(tabName = "home",
                h2("Home content"))},
      
      # TWEETER
      
      # Tweeter get data            
      {                
        tabItem(tabName= "Tgetdata",
                {
                  column(3,
                         
                         fluidRow( h5("GET THE TRENDS"),
                                   wellPanel(
                                     selectInput('country',"Select Country for available trend locations",choices = unique(sort(tid$country))),
                                     selectInput("LocT", 'select Location for trend',choices = "", selected = "")
                                   )
                         ),
                         
                         fluidRow(h5("EXTRACT DATA"),
                                  wellPanel(
                                    radioButtons("typeInput", "Select tweets by: ",
                                                 list("Hashtag/Trends" = "hashtag","Twitter Username"= "username", 'My Twitter Handle' = 'twitterhandle')
                                    ),
                                    sliderInput("numberInput", "Select number of tweets",
                                                min = 0, max = 3000, value = 100),
                                    
                                    conditionalPanel(
                                      condition = "input.typeInput == 'username'",
                                      textInput("usernameInput", "Enter Username", placeholder = "input username")
                                    ),    
                                    
                                    conditionalPanel(
                                      condition = "input.typeInput == 'hashtag'",
                                      textInput("hashtagInput", "Enter Trend/Hashtag string","", placeholder = "hashtag/trend")
                                    ),
                                    
                                    actionButton("goButton", "Search", icon("twitter"),
                                                 style="color: #fff; background-color: #337ab7")
                                  )
                         )
                  )
                },
                {
                  column(9,fluidRow(
                    tabBox(title = "GET DATA", id = "Tgetdata", width = 600, side = "right",
                           tabPanel("TRENDING", dataTableOutput("trendTable")),
                           tabPanel("TWEETS", dataTableOutput("tweetTable"), 
                                    actionButton("analyzeButton", "Perform Analysis", icon("calculator"), style="color: #fff; background-color: #337ab7"))
                    )))
                }
        )},
      # Tweets analytics
      {
        tabItem(tabName= "Ttanalytics",
                fluidRow(
                  # A static valueBox
                  valueBoxOutput("Hashtagbeinganalysed"),
                  valueBoxOutput("Tweetscount"),
                  valueBoxOutput("Tweetsentiment")
                ),
                fluidRow(
                  tabsetPanel(id = 'Tanalyticspanel', type = "tabs", selected = 'toverview', position = NULL,
                              tabPanel("Tweet Content Analytics", value = 'toverview', 
                                       fluidRow(
                                         box(title = "Tweets & Retweets by Day & Hour",background = "aqua", width = 12,height = 600,
                                             status = "warning", solidHeader = TRUE, collapsible = TRUE,
                                             plotlyOutput("Thistogram1", height = 500)
                                         )
                                       ),
                                       
                                       fluidRow(
                                         box(title = "What is being said on the topic?",background = "aqua",width = 6, height = 900, 
                                             status = "warning", solidHeader = TRUE, collapsible = TRUE,
                                             sliderInput('minfreq2', 'Select minimum frequency of words to be plotted',4,min = 1,max = 300,1),
                                             plotOutput("wordbar1", height = 600)),
                                         
                                         box(title = "What is being said on the topic?",background = "aqua",width = 6, height = 900,
                                             status = "warning", solidHeader = TRUE, collapsible = TRUE,
                                             sliderInput('minfreq1', 'Select minimum frequency of words to be plotted',4,min = 1,max = 300,1),
                                             plotOutput("wordCloud1", height = 600),
                                             sliderInput('maxword1', 'Select max number of words to be plotted',100,min = 10,max = 300,5)
                                         )
                                       ),
                                       
                                       fluidRow(
                                         
                                         box(title = "Words' Network",background = "blue",width = 12, height = 1000,
                                             status = "warning", solidHeader = TRUE, collapsible = TRUE,
                                             numericInput('correltn', 'Input Correlation lower limit',0.3,min = -1,max = 1,step = 0.05),
                                             numericInput('filter', 'Input word frequency filter',5,min = 5,max = 200,step = 1),
                                             actionButton("AnalyzewordNetwork","Analyze" ,icon("calculator"), style="color: #fff; background-color: #337ab7"),
                                             plotOutput("wordCloud2", height = 700))
                                         
                                       ),
                                       
                                       fluidRow(
                                         
                                         box(title = "Word Correlation",background = "blue",width = 12, height = 1000,
                                             status = "warning", solidHeader = TRUE, collapsible = TRUE,
                                             textInput("topic", "Input Word", placeholder = "input word"),
                                             numericInput('correltn2', 'Input Correlation lower limit',0.2,min = -1,max = 1,step = 0.05),
                                             actionButton("Analyzecorr","Analyze" ,icon("calculator"), style="color: #fff; background-color: #337ab7"),
                                             plotlyOutput("corrplot1", height = 700)
                                         )
                                         
                                       ),
                                       fluidRow(downloadButton("downloadpage1","Download Page as pdf")) 
                              ),
                              tabPanel("Sentiment Analytics", value = 'tsentiment', 
                                       fluidRow(
                                         box(title = "Overall Sentiment",background = "aqua",width = 12, height = 700,
                                             status = "warning", solidHeader = TRUE, collapsible = TRUE,
                                             plotOutput("Tsentplot1", height = 600))
                                       ),
                                       
                                       fluidRow(
                                         box(title = "Emotion Tagged Word Cloud",background = "aqua",width = 6, height = 700,
                                             status = "warning", solidHeader = TRUE, collapsible = TRUE,
                                             plotOutput("Tsentplot2", height = 600)),
                                         
                                         box(title = "Average Sentiment vs Frequency of tweeting",background = "aqua",width = 6, height = 700,
                                             status = "warning", solidHeader = TRUE, collapsible = TRUE,
                                             plotlyOutput("Tsentplot3", height = 600))
                                       ),
                                       fluidRow(downloadButton("downloadpage2","Download Page as pdf"))),
                              tabPanel("Influencer Analytics", value = 'tinfluencer', 
                                       fluidRow(
                                         box(title = "Topic Initializers",background = "aqua", width = 6,
                                             status = "warning", solidHeader = TRUE, collapsible = TRUE,
                                             sliderInput("Toptweeps", "Number of observations", min = 1, max = 20, value = 5),
                                             plotlyOutput("Thistogram2", height = 350)),
                                         
                                         box(title = "Topic Broadcasters",background = "aqua",width = 6,
                                             status = "warning", solidHeader = TRUE, collapsible = TRUE,
                                             sliderInput("TopRetweeps", "Number of observations", min = 1, max = 20, value = 5),
                                             plotlyOutput("Thistogram3", height = 350)
                                         )),
                                       fluidRow(
                                         box(title = "Topic Receivers",background = "aqua", width = 6,
                                             status = "warning", solidHeader = TRUE, collapsible = TRUE,
                                             sliderInput("TopRetweeted", "Number of observations", min = 1, max = 20, value = 5),
                                             plotlyOutput("Thistogram4", height = 350)),
                                         
                                         box(title = "Top Users responded to",background = "aqua",width = 6,
                                             status = "warning", solidHeader = TRUE, collapsible = TRUE,
                                             plotlyOutput("Thistogram5", height = 450)
                                         )
                                       ),
                                       fluidRow(
                                         box(title = "Broadcast and Receiver Networks",background = "blue", width = 12, height = 1100,
                                             status = "warning", solidHeader = TRUE, collapsible = TRUE,
                                             selectInput('tinfluencetype',"Select type of influence",choices = c("Both",
                                                                                                                 "Broadcasters","Receivers"), selected = "Both"),
                                             sliderInput("tinfluence", "Choose degree of influence", min = 0, max = 1000, value = 5),
                                             plotOutput("network1", height = 750))),
                                       fluidRow(downloadButton("downloadpage3","Download Page as pdf")))
                  )
                )
        )
      },
      # User analytics
      {
        tabItem(tabName= "Tuseranalytics","PROFILE ANALYTICS HERE")},
      
      # FACEBOOK
      {
        tabItem(tabName = "facebook",
                h2("Facebook Analytics"))},
      
      # INSTAGRAM
      {
        tabItem(tabName = "instagram",
                h2("instagram Analytics"))},
      
      # PRINT MEDIA
      {
        tabItem(tabName = "print_media",
                h2("print Media Analytics"))},
      
      # BLOGS
      {
        tabItem(tabName = "blogs",
                h2("Blog content Analytics"))}
    )
  )
}
dashboardPage(header, sidebar,body, skin = "green")