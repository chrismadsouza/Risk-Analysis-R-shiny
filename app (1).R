 


ui <- dashboardPage(dashboardHeader(title=strong("STOCKER"),titleWidth = 1500),
                    dashboardSidebar(width=350,
                                      sidebarMenu(
                                        menuItem(selectInput("company","Choose a Company of your Choice",list('NSE-National Stock Exchange of India'=c("Bajaj Auto Ltd (BAJAJ_AUTO)"="1","BASF India Ltd (BASF)"="2","ACC Ltd (ACC)"="3","Biocon (BIOCON)"="4","Bharat Heavy Electricals (BHEL)"="5"),
                                                                                      'NASDAQ-National Association of Securities Dealers Automated Quotations'=c("Apple (AAPL)"="6","Atlantic American Corp (AAL)"="7","American Airlines Group Inc (AAME)"="8","ACI Worldwide Inc (ACI)"="9","Abaxis Inc (ABAXIS)"="10"),
                                                                                      'NYSE-New York Stock Exchange'=c("ABB Ltd (ABB)"="11","Alliance Bernstein Holding (AB)"="12","Abbott Laboratories (ABT)"="13","Archer Daniels Midland (ADM)"="14","American Assets Trust(AAT)"="15")))),
                                        menuItem(selectInput("Variable", "Select a Variable",choices=c("Returns"="7", "High"="3", "Low"="4","Open"="2","Close"="5","Volume"="6"))))
                                      
                     ),
                     dashboardBody(theme_blue_gradient ,
                                   tags$head(tags$style(HTML('
      .main-header .logo {
        font-family: "Copper Black", Times, "Times New Roman", serif;
        font-weight: bold;
        font-size: 30px;
      }
    '))),
                       
        (box(width=20,height=1500,
            tabBox(width=20,height=1500, id="tabBox_next_previous",
              tabPanel("Home",p(""),img(src='tenor.gif',height = 550, width = 1100, align = "center")),
              tabPanel("App Description",p(""),
                 mainPanel(h2(strong("This App Aims To", span("STALK", style="color:red"),"Your Stocks!"),align="center"),
                        h4( strong("It Will Help You Visualize Your Stock's Daily Movement And Also How News Articles Affect Stock Prices "),align="center"),
                            h4("Steps:"),
                          h4("1. Select the Company and Variable to be plottled from the Sidebar Menu"),
                          h4("2. View the same in the Visualizations Tab"),
                          h4("3. For the chosen company, CVAR value is obtained in the ES/CVaR Tab"),
                          h4("4. Sentiment Analysis on Biocon's Stock is obtained in the Sentiment Analysis Tab"),
                          h4("5. A classification is obtained for all companies in the Conclusion Tab"),
                          img(src='man.gif',height = 300, width = 250, align = "left"))),
             tabPanel("Visualization",p(""),
                    plotlyOutput("distPlot"),
                    verbatimTextOutput("info")),
              tabPanel("ES/CVaR Value",p(""),
                    mainPanel(h3(span(strong("ES/CVaR is a Statistic used to quantify the Risk of a Portfolio",style="color:blue"))),h2(""),align="center",
                              plotlyOutput("hist"),
                              h4(textOutput("Cvar"),align='center'),
                              h4(textOutput("Var"),align='center'))),
               tabPanel("Sentiment Analysis",p(""),
                    mainPanel(h3(span(strong("Sentiment Analysis on News Articles Related to Biocon",style="color:blue"))),
                              span("Disclaimer:",style="color:red"),span(" News is just one of the many factors affecting Stock Prices",style="color:red"),
                              h2(""),align="center",
                              plotlyOutput("Biocon"),
                              h2(""),
                              dataTableOutput('links'))),
               tabPanel("Conclusion",p(""),
                    mainPanel(h3(span(strong("The Companies are classified on the basis of ES/CVaR value",style="color:blue"))),align="center",h3(""),
                              plotlyOutput('bar'),
                              plotOutput('legend')
                                             ))))
                                        )))






server <- function(input, output,session) {
##Objective 1:plotting Line chart
  
  #Making dataset to be chosen reactive  
  dat <- reactive({
    temp <- datafiles[[as.numeric(input$company)]]
  })
  #In chosen dataset, making variable to be chosen reactive  
  var <- reactive({
    temp <- datafiles[[as.numeric(input$company)]]
    temp <- temp[,c(as.numeric(input$Variable))]
    
  })
  
    #Making dataset reactive for date column
  date <- reactive({
    temp <- datafiles[[as.numeric(input$company)]]
    temp <- temp[,c(1)]
  })

   #Plotting Line chart for chosen dataset and variable  
  output$distPlot <- renderPlotly({
    
    plot_ly(dat(),y=var(),x=date(),type="scatter",mode="lines") %>%
      #add_trace(dat(),y=~var())%>%
      layout(title="LINE CHART", xaxis=list(title='Time Period'),yaxis=list(title="Variable Selected"))
  
    })
##Objective 2: Calculating cvar plotting histogram of returns   
  #Making dataset reactive for return column
   day<-reactive({
     temp<-datafiles[[as.numeric(input$company)]]
     temp<-temp[,c(7)]
   })
  #Finding density of return column for x
   dayx<-reactive({
     temp<-datafiles[[as.numeric(input$company)]]
     temp<-temp[,c(7)]
     temp<-density(temp)$x
   })
  #Finding density of return column for y 
   dayy<-reactive({
     temp<-datafiles[[as.numeric(input$company)]]
     temp<-temp[,c(7)]
     temp<-density(temp)$y
   })
  #Plotting histogram and density curve   
   output$hist<-renderPlotly({
     plot_ly(dat(),x = day() , type = "histogram", name = "Histogram")%>%
       add_trace(x = dayx() , y = dayy() , type = "scatter" , mode = "lines" , fill ="tozeroy", yaxis = "y2" , name = "Density")%>%
       layout(title="Histogram and Density Plot for Returns",xaxis=list(title="Return Value"),yaxis=list(title="Number of Days"),yaxis2=list(overlaying="y",side="right"))
  })
  #calculating cvar for chosen company's returns
   Cvar<-reactive({
     temp<-datafiles[[as.numeric(input$company)]]
     temp<-temp[,c(7)]
     temp <-(cvar::ES(temp))
    
   })
   #Returnig CVAR value as Text   
   output$Cvar<-renderText({
    paste( "ES/CVaR of the stock is :", Cvar())
   })
   
   #calculating var for chosen company's returns
   Var<-reactive({
     temp<-datafiles[[as.numeric(input$company)]]
     temp<-temp[,c(7)]
     temp<-(VaR(c(temp)))
   })
   #Returning VAR value as Text
   output$Var<-renderText({
     paste( "VaR of the stock is :", Var())
   })
   
##Objective 3:Sentiment ANalysis on Biocon   
   #Plotting Line chart for High Value of Biocon   
   output$Biocon<-renderPlotly({
     
     plot_ly(Biocon,y=Biocon$High,x=Biocon$Date,type="scatter",mode="lines",color=I("red")) %>%
       layout(title="Line chart for Biocon's Daily Highest Value", xaxis=list(title='Time Period'),yaxis=list(title="High value"))
   })
   
  #Creating datatable for sentiment analysis   
   Number<-c('1','2','3','4','5','6')
   Article_Date<-c("23th Mar 2016",'26th May 2016','30th Nov 2017','18th Jan 2018','11th July 2018','21st Sept 2018')
   Headline<-c("Biocon Signs Rh-insulin Co-development Agreement",'Biocon gets Karnataka govt nod for Rs1060-crore plant','Biocon, Mylan joint product Fulphila gets approval in EU',
               'Sandoz announces collaboration with biocon','Biocon puts arthritis drug on backburner','Biocon closer to EU approval of biosimilar pegfilgrastim')
   Sentiment_Value<-c("0.2105672",'0.07406787','0.08808731','0.3599401','-0.05303301','0.2432904')
   Link<-c('https://www.businessworld.in/article/Biocon-Signs-Rh-insulin-Co-development-Agreement-With-Mexican-Co/17-03-2016-92068/',
           'https://www.thehindubusinessline.com/companies/biocon-gets-karnataka-govt-nod-for-1060crore-plant/article8651006.ece',
           'https://www.moneycontrol.com/news/business/biocon-mylan-joint-product-fulphila-gets-approval-in-eu-3239571.html',
           'https://www.sandoz.com/news/media-releases/sandoz-announces-exclusive-global-collaboration-biocon-next-generation',
           'https://economictimes.indiatimes.com/industry/healthcare/biotech/biocon-puts-arthritis-drug-on-backburner/articleshow/64942706.cms',
           'https://www.moneycontrol.com/news/business/companies/biocon-closer-to-europe-approval-of-biosimilar-pegfilgrastim-2974631.html')
   link123 <- data.frame(Number,Article_Date, Headline,Sentiment_Value,Link)
   
   #Making datatable
   output$links <- renderDataTable({
     data <- link123[,c("Number","Article_Date","Headline","Sentiment_Value","Link")] %>% 
       mutate(Link = paste0("<a href='", Link,"' target='_blank'>", Link,"</a>"))
     data
   
     },
   escape = FALSE)

##Objective4:Conclusion Tab (Barplot)
   #Making dataframe of cvar values   
   x <- c('BAJAJ', 'BASF', 'ACC', 'BIOCON', 'BHEL','AAPL','AAL','AAME','ACIW','ABAX','ABB','AB','ABT','ADM','AAT')
   y <- c(4.2,5.13,4.26,5.08,5.11,3.84,9.011,4.5,4.77,4.84,1.79,4.95,2.5,3.65,2.63)
   data <- data.frame(x, y, stringsAsFactors = FALSE)
   data$x <- factor(data$x, levels = unique(data$x)[order(data$y, decreasing = FALSE)])
   #Creating a barplot of cvar values  
   output$bar <- renderPlotly({
     
     plot_ly(data, x = ~x, y = ~y, type = 'bar',
             marker = list(color = c('rgba(255,0,129,1)', 'rgba(255,0,129,1)',
                                     'rgba(255,0,129,1)', 'rgba(255,0,129,1)',
                                     'rgba(255,0,129,1)','rgba(255,0,129,1)','rgba(167, 55, 250,1)','rgba(255,0,129,1)','rgba(255,0,129,1)','rgba(255,0,129,1)',
                                     'rgba(0,167,0,1)','rgba(255,0,129,1)',
                                     'rgba(0,167,0,1)',
                                     'rgba(255,0,129,1)','rgba(0, 167, 0,1)'))) %>%

       layout(title = "Bar Plot of ES/CVaR values of Different Companies",
              xaxis = list(title = "Companies (Ticker Symbol)"),
              yaxis = list(title = "CVAR/ES Value"))
   })
   #Creating legend of above plot   
   output$legend <- renderPlot({
     plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
     legend("topleft", legend =c('High Risk', 'Medium Risk', 'Low Risk'), pch=16, pt.cex=3, cex=1.5, bty='n',
            col = c('darkorchid1', 'deeppink', 'springgreen3'))
     mtext("Labels", at=0.2, cex=2)
     
   })
   
   
   
  session$onSessionEnded(stopApp)  
}



shinyApp(ui, server)

