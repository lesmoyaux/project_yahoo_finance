library(shiny)
library(shinyjs)
library(shinythemes)
library(shinyWidgets)
library(VGAM)
library(lubridate)
library(quantmod)
library(curl)
library(plotly)
library(tidyverse)
library(tidyquant)
library(ggplot2)
library(plyr)
library(dplyr)
library(shinybusy)
library(tidyr)
library(stringr)
library(readr)
library(TTR)
library(quantr)

useShinyjs()

urlfile="https://raw.githubusercontent.com/MastThesis/R/master/TICKERS.csv"
database<-read_csv(url(urlfile),show_col_types = FALSE)
print("")
database<-data.frame(database)
database<-database[,1:2]

tickers<-vector(mode="list",length=length(database[,1]))
for(i in 1:length(tickers)){
 tickers[[i]]<-i
}
names(tickers)<-database[,1]
css <- "
 #name {
 color: white;
 font-size: 120px;
 }
 #table {
     background: grey;
     font-size: 120px;
     border: 1px solid black !important ;
 }
"

tickersTopTen_tab_4 <- c("AAPL","MSFT", "INTC","AMD","GOOGL", "AMZN","META","TSLA","JNJ","XOM","^NDX","^GSPC" )
prices_tab_4 <- tq_get(tickersTopTen_tab_4,
                       get  = "stock.prices",
                       from = today()-months(12),
                       to   = today(),
                       complete_cases = F) %>%
 select(symbol,date,close)

##### Create the shiny UI
ui <- fluidPage (
 #theme = shinytheme("cerulean"),
 # theme = shinytheme("superhero"),
 # theme = shinytheme("united"),
 theme = shinytheme("cyborg"),
 tags$style("checkboxGroup label{font-size:40px}"),        
 navbarPage(#theme=shinytheme("cerulean"),
  "Stock Radioscopie",
  #########################################################################################                  
  tabPanel("Stock yahoo financial data",
           sidebarPanel(
            # first Apple i
            selectizeInput(
             "textEndo", label = h3("Symbol"), choices = database[,1],selected="AAPL", multiple = F),
            br(),
            br(),
            dateRangeInput("dateEndo", label = h3("Date range"),start = Sys.Date()-182, end = Sys.Date()),
           ),
           mainPanel(
            tags$style(css),
            tags$h3(strong("Summary:")),
            #div(tableOutput("table1"), style = "font-size:200%"),
            #span(style = "color:green;font-weight:bold;", textOutput("table2"),inline=T),
            textOutput("wrong"),
            textOutput("skipped"),
            textOutput("togo"),
            splitLayout(cellWidths = rep("16.5%", 6),
                        # htmlOutput("table0"),
                        span(style = "font-size:30px;",textOutput("table1Endo")),
                        span(style = "font-size:30px;",textOutput("table2Endo")),
                        span(style = "font-size:30px;",textOutput("table3Endo")),
                        span(style = "font-size:30px;",htmlOutput("table4Endo")),
                        #span(style = "color:green;font-weight:bold;font-size:30px;", textOutput("table4")),
                        span(style = "font-size:30px;",textOutput("table5Endo")),
                        #span(style = "color:red;font-weight:bold;font-size:30px;", textOutput("table6")),
                        span(style = "font-size:30px;",htmlOutput("table6Endo")),
            ),
            # DTOutput("table1")
            splitLayout(cellWidths = c("53%", "47%"),
                        tableOutput("outputIdEndo"),
                        plotOutput("PlotEndo",height=660,width=560))
           )
           
  ), # Navbar 1, tabPanel
  #########################################################################################  
  
  tabPanel("Analysis one Stock",
           sidebarPanel(
            # wellPanel
            helpText("Select a stock to examine.
        Information will be collected from yahoo finance.
        Please make sure your internet connection is OK
             "),
            textInput("symb1", "Symbol", "MSFT"),
            dateRangeInput("dates1", "Compare to historic returns",start ="2021-01-01", end = "2023-01-05",separator="to"),
            actionButton("get1", "Get Stock"),
            uiOutput("newBox1")
           ),
           mainPanel(
            tabsetPanel(
             tabPanel("Charts", plotOutput("chart1",height=800)),
             tabPanel("Model", div(h3(textOutput("ks"))),div(h3(textOutput("ksp"))),plotOutput("hist",height=700)),
             # tabPanel("VaR", h3(textOutput("text3"))),
             tabPanel("About", h3(textOutput("text4"))),
             id = "tab"
            )
           )
  ),
  #########################################################################################  
  tabPanel ("Comparison Multiple Stocks",
            sidebarPanel(
             numericInput("m0", label = "choose a starting month", value = 3),
             numericInput("d0", label = "choose a starting day", value = 4),
             numericInput("y0", label = "choose a starting year", value = 2022),
             selectInput("frequency", "choose frequency:",choices = c("daily","weekly","monthly") ),
             selectInput("price", "choose data type:",choices = c("Adjusted","Open","High","Low","Close","Volume")),
             textInput("v", label = "choose tickers separated by spaces", value = c("MSFT INTC AMZN GOOG")  ),
             fileInput("file1", "choose text file for tickers instead", accept=c('text-separated-values'))),
            
            mainPanel(
             tabsetPanel(
              tabPanel("Curves", plotlyOutput('plot1',height=750)),
              tabPanel("Synopsis", tableOutput("table.ret")),
              tabPanel("Download",downloadButton('downloadData', 'Download')),
              id = "tab1"
             )
            )
             
  ), # Navbar 1, tabPanel
  
  ##################################################################################        
  
  tabPanel("Top Ten Caps.",
           sidebarPanel(width = 3,
                        
                        # Let user pick stocks
                        pickerInput(
                         inputId = "stocks",
                         label = h4("Stocks"),
                         choices = list(
                          "AAPL"      = 1,
                          "MSFT"      = 2,
                          "INTC"      = 3,
                          "AMD"       = 4,
                          "GOOGL"     = 5,
                          "AMZN"      = 6,
                          "META"      = 7,
                          "TSLA"      = 8,
                          "JNJ"       = 9,
                          "XOM"       = 10),
                         selected = c(1:10),  
                         options = list(`actions-box` = TRUE),
                         multiple = T
                        ),
                        
                        # Pick time period
                        radioButtons("period", label = h4("Period"),
                                     choices = list("1 month" = 1,"2 months"= 2, "3 months" = 3,
                                                    "4 months" = 4, "5 months" =5, "6 months" = 6,
                                                    "7 months" = 7, "8 months" = 8, "9 months" = 9,
                                                    "10 months" = 10, "11 months" = 11,"12 months"= 12,
                                                    "YTD" = 13),
                                     selected = 3
                        ),
                        
                        # Pick benchmark
                        radioButtons("benchmark", label = h4("Benchmark"),
                                     choices = list("SP500" = 11, "Nasdaq100" = 12,"None" = 13),
                                     selected = 12)
           ),
           
           # Plot results
           mainPanel(
            plotlyOutput("plot",height=800)
           ),
           
  ), # Navbar 1, tabPanel
  
  ##################################################################################        
  
 )    # navbarPage
)# fluidPage

# some usefull functions
ret.f <- function(x) c(NA,x[2:length(x)]/x[1:(length(x)-1)] - 1)
lag.f <- function(x) c(NA,x[1:(length(x)-1)])

# get the prices function
# tickersTopTen <- c("AAPL","MSFT", "INTC","AMD","GOOGL", "AMZN","META","TSLA","JNJ","XOM" )
# benchmarks <- c("^NDX","^GSPC")
# bench <- tq_get(benchmarks,get="stock.prices",from=today()-months(12),to=today()) %>%
 # select(symbol,date,close)
# rm(list = ls())
# prices <- tq_get(tickersTopTen, get="stock.prices",from = today()-months(12),to=today(),complete_cases=F) %>%
#  select(symbol,date,close)

multion_converter<-function(x){
 if(x>1000000000 & x<1000000000000){
  x<-x/1000000000
  x<-format(x,scienticic=F)
  x<-as.numeric(x)
  x<-round(x,digits=2)
  x<- paste0(x," Bln.")
 }else if(x<1000000000){
  x<-x/1000000
  x<-format(x,scienticic=F)
  x<-as.numeric(x)
  x<-round(x,digits=2)
  x<- paste0(x," Mio.")
 }else if(x>1000000000000){
  x<-x/1000000000000
  x<-format(x,scienticic=F)
  x<-as.numeric(x)
  x<-round(x,digits=2)
  x<- paste0(x," Tri.")
 }
 return(x)
}

get.prices <- function(v,p,t1,price) {
 ds.list <- lapply(v, function(tic) get(getSymbols(tic, from=t1)) )
 names(ds.list) <- v
 # aggregate data respectively
 if(p == "w") {
  ds.list <- lapply(ds.list,function(x) apply.weekly(x, function(y) y[nrow(y),])   )
 }
 if(p == "m") {
  ds.list <- lapply(ds.list,function(x) apply.monthly(x, function(y) y[nrow(y),])   )
 }
 # do some adjustment to the data list before merging all together, normalizing
 ds.list <- lapply(ds.list, function(x) data.frame(x[,grep(price,names(x))]) )
 ds.list <- lapply(ds.list, function(x) data.frame(Date = rownames(x), Price = x[,1]) )
 ds.list <- lapply(v, function(v.i) { names(ds.list[[v.i]])[2] <- v.i ; return(ds.list[[v.i]])}  )
 ds <- Reduce(function(...) merge(...,by = "Date", all = T), ds.list)
 ds$Date <- as.Date(ds$Date)
 # if( !is.null(t2) ) {
 #   ds <- ds[ds$Date <= t2 ,]
 # }
 return(ds)
}
# Define server logic for random distribution application

server <- function(input, output,session) {
 
 ##################################################################################################
 # shinyServer(function(input, output) {
 firstTime <- TRUE
 # o <-observe({
 if (firstTime) {
  shinyjs::click("get1", asis=FALSE)
  firstTime <- FALSE
  # o$destroy()
 }
 ########################################################### TAB ZERO
 output$table1Endo <-renderText({
  symbol<-input$textEndo
  index<-which(database[,1]==symbol)
  symbol=database[index,1]
  data <- getQuote(symbol)
  frame<-matrix(ncol=2,nrow = 3)
  frame[,1]<-c("Last Price:", "Change:", "% Change:")
  frame[,2]<-c(data$Last,data$Change,data$`% Change`)
  table=c(t(frame))
  table<-matrix(table,nrow = 1)
  table<-table[1]
 })
 
 output$table2Endo<-renderText({
  symbol<-input$textEndo
  index<-which(database[,1]==symbol)
  symbol=database[index,1]
  data <- getQuote(symbol)
  frame<-matrix(ncol=2,nrow = 3)
  frame[,1]<-c("<strong>Last Price<strong>", "<strong>Change<strong>", "<strong>% Change<strong>")
  frame[,2]<-c(data$Last,data$Change,data$`% Change`)
  table=c(t(frame))
  table<-matrix(table,nrow = 1)
  table<-table[2]
 })
 
 output$table3Endo <-renderText({
  symbol<-input$textEndo
  index<-which(database[,1]==symbol)
  symbol=database[index,1]
  data <- getQuote(symbol)
  frame<-matrix(ncol=2,nrow = 3)
  frame[,1]<-c("Last Price:", "Change:", "% Change:")
  frame[,2]<-c(data$Last,data$Change,data$`% Change`)
  table=c(t(frame))
  table<-matrix(table,nrow = 1)
  table<-table[3]
 })
 
 output$table4Endo <-renderText({
  symbol<-input$textEndo
  index<-which(database[,1]==symbol)
  symbol=database[index,1]
  data <- getQuote(symbol)
  frame<-matrix(ncol=2,nrow = 3)
  frame[,1]<-c("Last Price:", "Change:", "% Change:")
  frame[,2]<-c(data$Last,data$Change,data$`% Change`)
  table=c(t(frame))
  table<-matrix(table,nrow = 1)
  table<-round(as.numeric(table[4]),digits = 2)
  if(table>=0){
   paste("<font color=\"#47F23A\"><b>", table, "</b></font>")
  }
  else
  {
   paste("<font color=\"#FF0000\"><b>", table, "</b></font>")
  }
 })
 
 output$table5Endo <-renderText({
  symbol<-input$textEndo
  index<-which(database[,1]==symbol)
  symbol=database[index,1]
  data <- getQuote(symbol)
  frame<-matrix(ncol=2,nrow = 3)
  frame[,1]<-c("Last Price:", "Change:", "% Change:")
  frame[,2]<-c(data$Last,data$Change,data$`% Change`)
  table=c(t(frame))
  table<-matrix(table,nrow = 1)
  table<-table[5]
 })
 
 output$table6Endo <-renderText({
  symbol<-input$textEndo
  index<-which(database[,1]==symbol)
  symbol=database[index,1]
  data <- getQuote(symbol)
  frame<-matrix(ncol=2,nrow = 3)
  frame[,1]<-c("Last Price:", "Change:", "% Change:")
  frame[,2]<-c(data$Last,data$Change,data$`% Change`)
  table=c(t(frame))
  table<-matrix(table,nrow = 1)
  table<-paste0(round(as.numeric(table[6]),digits = 2)," %")
  if(table>=0){
   paste("<font color=\"#47F23A\"><b>", table, "</b></font>")
  }else{
   paste("<font color=\"#FF0000\"><b>", table, "</b></font>")
  }
 })
 
 #,rownames = F,align = 'lrlrlr',
 #colnames = F,bordered = T,spacing = 'm',striped = F,sanitize.text.function=function(x){x})  
 
 output$outputIdEndo <-renderTable( {
  symbol<-input$textEndo
  index<-which(database[,1]==symbol)
  symbol=database[index,1]
  data<-yahoo_summary(c(symbol))
  header1<-c("Enterprise Value ($)","Previous Close","Open","Bid/Bid Size",
             "Ask/Ask Size","Day's Range")
  values1<-c(multion_converter( data$enterpriseValue),data$previousClose,data$open,paste0(data$bid,"/",data$bidSize),
             paste0(data$ask,"/",data$askSize), paste0(data$dayLow,"-",data$dayHigh) )
  header2<-c("52 Weeks Range","Volume","Avg. Volume", "Market Cap", "Beta (5Y Monthly)","Pay Out Ratio")
  values2<-c(paste0(data$fiftyTwoWeekLow,"-",data$fiftyTwoWeekHigh), data$volume,data$averageVolume,multion_converter( data$marketCap),data$beta,data$payoutRatio)
  table1<-matrix(c(header1,values1,header2,values2),ncol=4)
  for(i in 1:length(table1[,1])){
   table1[i,1]<-paste0("<strong>",table1[i,1],"<strong>")
   table1[i,3]<-paste0("<strong>",table1[i,3],"<strong>")
  }
  table1<-rbind(rep("",4),rep("",4),table1) # four empty rows above just for spacing purpose can also be done using br()
  # table1
 },rownames = F,align = 'lrlr',
 colnames = F,bordered = T,spacing = 'l',striped = F,sanitize.text.function=function(x){x})
 
 ##################################################################################################
 
 output$PlotEndo<-renderPlot({
  symbol<-input$textEndo
  index<-which(database[,1]==symbol)
  symbol=database[index,1]
  data=getSymbols(symbol,src="yahoo",from=Sys.Date()-365,to=Sys.Date(), auto.assign = FALSE)
  dataframe=data.frame(data)
  colnames(dataframe)<-c("Open","High","Low","Close","Volume","Adjusted")
  par(bg = "#2B3E50")
  par(mar=c(8,6,2,2))
  len<-nrow(dataframe)
  plot(c(1:len),dataframe[,4],type="l",
       col="green",lwd=2,
       xlab = "",ylab = "",xaxt="n", #,yaxt="n"
       ylim=c(0,1.2*max(dataframe[,4])),
       las=2,
       cex.axis=1.2,
       col.axis="white"
  )
  box(col="white")
  # yaxis<-axis(2,labels = F,tick=F)
  # yaxis<-format(yaxis,scientific=T)
  # axis(2,at=as.numeric(yaxis),labels = yaxis,las=2,col.axis = "white",col.ticks="white")
  mtext(2,text = "Close",col = "white",line = 3.8,cex=1.3 )
  mtext(3,text = paste0(database[index,2],": ","1 Year Close"),col = "white",line = 0.2,cex=1.2 )
  axis(1,at=c(1,51,101,151,201,251,301,351),
       labels=rownames(dataframe)[c(1,51,101,151,201,251,301,351)],
       las=2,
       col.axis = "white",
       col.ticks = "white")
  rownames(data)
  x<-c(1:len)
  y<-data[,4]
  y[1]<-0
  y[len]<-0
  den=matrix(c(x,y) ,ncol = 2)
  polygon(den, col = "brown1")
  box(col="white")
 },height = 400)
 
 ###############################################################################
 # first TAB
 
 # acquiring data
 dataInput <- reactive({
  ret <- workFlow()
  if (ret) {
   data=try(
    getSymbols(input$symb1,
               src='yahoo',
               auto.assign = F,
               from=input$dates1[1], #"2022-08-09",
               to=input$dates1[2]))  # "2022-09-28"))
   if(isTRUE(class(data)=="try-error")) {return(FALSE)}
   else {return (data)}
   # valid_ticker<-append(valid_ticker,validity) with integrate looping
  }
 })
 
 datesInput <- reactive({
  if (input$get1 == 0)
   return(NULL)
  return(isolate({paste0(input$dates1[1], "::", input$dates1[2])
  }))
 })
 
 returns <- reactive({
  if (input$get1 == 0)
   return(NULL)
  dailyReturn(dataInput())
 })
 
 xs <- reactive({
  ret <- workFlow()
  if (ret) {
   span <- range(returns())
   seq(span[1], span[2], by = diff(span) / 100)
  }
 })
 
 # tab based controls
 output$newBox1 <- renderUI({
  switch(input$tab,
         "Charts" = chartControls,
         "Model" = modelControls,
         # "VaR" = helpText("VaR"),
         "future"= helpText("developemnt by moyal unlimited")
  )
 })
 
 # Charts tab : Pseudo UI usuallly in the ui segment
 chartControls <- div(
  wellPanel(
   selectInput("chart_type",
               label = "Chart type",
               choices = c("Candlestick" = "candlesticks", "Matchstick" = "matchsticks","Bar" = "bars","Line" = "line"),
               selected = "Line"),
   checkboxInput(inputId = "log_y", label = "log y axis", value = FALSE),
   checkboxInput(inputId="bckgrnd_black", label ="backgroud black", value=FALSE)
  ),
  wellPanel(
   p(strong("Technical Analysis")),
   checkboxInput("ta_vol", label = "Volume", value = FALSE),
   checkboxInput("ta_sma", label = "Simple Moving Average", value = FALSE),
   checkboxInput("ta_ema", label = "Exponential Moving Average",value = FALSE),
   checkboxInput("ta_wma", label = "Weighted Moving Average",value = FALSE),
   checkboxInput("ta_bb", label = "Bolinger Bands", value = FALSE),
   checkboxInput("ta_momentum", label = "Momentum", value = FALSE),
   actionButton("chart_act", "Add Technical Analysis")
  )
 )
 
 TAInput <- reactive({
  ret <- workFlow()
  if (ret) {
   # if (input$chart_act == 0) return("NULL")
   tas <- {c(input$ta_vol,
             input$ta_sma,
             input$ta_ema,
             input$ta_wma,
             input$ta_bb,
             input$ta_momentum)}
   funcs <- c(addVo(),
              addSMA(),
              addEMA(),
              addWMA(),
              addBBands(),
              # addMACD(),
              addMomentum())
   if (any(tas)) funcs[tas]
   else "NULL"
  }
 })
 
 output$chart1 <- renderPlot({
  ret <- workFlow()
  if (ret) {
   screencolor <- "white"
   if (input$bckgrnd_black== TRUE)
    screencolor<- "black"
   chartSeries(dataInput(),
               name = input$symb1,
               type = input$chart_type,
               subset = datesInput(),
               log.scale = input$log_y,
               theme =chartTheme(screencolor),
               TA = TAInput())
  }
 })
 
 # Model tab
 modelControls <- div(
  wellPanel(
   sliderInput("n", "Number of bins in histogram",min = 1, max = 250, value = 30)
  ),
  wellPanel(
   selectInput("family", "Model returns as",choices = c("normal", "double exponential", "t"),selected = "normal"),
   sliderInput("mu", "Mean",min = -1, max = 1, value = 0, step = 0.01),
   sliderInput("sigma", "Standard Deviation",min = 0, max = 0.1, value = 0.05, step = 0.001),
   conditionalPanel(condition = "input.family == 't'",sliderInput("df", "Degrees of freedom",min = 2, max = 1000, value = 10))
  )
 )
 
 workFlow <- function (){
  # showModal(modalDialog(   es ist nicht gut, erscheint ganz am Anfang die ganze Zeit
  #  title = "Dear User",
  #  "Please enter a valid ticker, ensure your internet connection and press the get stock button!",
  #  easyClose = TRUE
  # ))
  validate(
   need(input$symb1 != "", "Please select a valid ticker"),
   need(input$symb1 != 0, "Pls select a ticker"),
   need(input$get1 != 0, "Pls press the GET button")
  )
  if (is.null(input$symb1)) return (FALSE)
  return (TRUE)
 }
 
 ys <- reactive({
  # ret <- workFlow()
  # if (ret) {
  switch(input$family,
         "double exponential" = dlaplace(xs(), location = input$mu,  scale = input$sigma),
         "normal" = dnorm(xs(), mean = input$mu, sd = input$sigma),
         "t" = dt((xs() - input$mu) / input$sigma, df = input$df) * sqrt(2 * length(returns()))
  )
  #}
 })
 
 ks <- reactive({
  # ret <- workFlow()
  # if (ret) {
  switch(input$family,
         "double exponential" = ks.test(returns(), "plaplace",input$mu, input$sigma),
         "normal" = ks.test(returns(), "pnorm", input$mu, input$sigma),
         "t" = ks.test((returns() - input$mu) / input$sigma, "pt", input$df)
  )
  # }
 })
 
 output$hist <- renderPlot({
  ret <- workFlow()
  if (ret) {
   hist(returns(),
        xlab = "returns",
        freq = FALSE,
        main = paste(input$symb1, "Daily Returns:", input$dates1[1], "-", input$dates1[2], sep = " "),
        breaks = input$n)
   lines(xs(), ys(), col = "red")}
 })
 
 output$ks <- renderText({
  ret <- workFlow()
  if (ret) {
   paste0("Kolmogorv-Smirnoff statistic: ", ks()$statistic)
  }
 })
 
 output$ksp1 <- renderText({
  ret <- workFlow()
  if (ret) {paste0("P-value for model: ", ks()$p.value)}
 })
 
 # VaR tab
 output$text3 <- renderText({
  ret <- workFlow()
  if (ret) {paste0(input$symb1, " 3: ", input$tab1)}
 })
 
 # about tab
 output$text4 <- renderText({
  ret <- workFlow()
  if (ret) {paste0(input$symb1, " 3: ", "Development of Tech Stocks software management")}
 })
 
 ########################################################### TAB One
 
 ###TAB 2 ##########################################################
 
 f1 <- function(v,p,y0,m0,d0,price) {
  v <- as.character(v)
  v <- unlist(strsplit(v, " "))
  p <- substr(p,1,1)
  if(nchar(d0) < 2) {
   d0 <- paste(0,d0,sep = "")
  }
  if(nchar(m0) < 2) {
   m0 <- paste(0,m0,sep = "")
  }
  # month day year
  t1 <- paste(c(y0,m0,d0),collapse = "-")
  # today's date
  t2 <- as.character(today())
  P <- get.prices(v,p,t1,price)
  P$Date <- as.character(P$Date)
  return(P)
 }
 
 DataInput <- reactive({
  if(is.null(input$file1[[1]]) )
   return(f1(input$v,input$frequency,input$y0,input$m0,input$d0,input$price))
  else {
   req(input$file1)
   tics <- scan(input$file1$datapath,what = character())
   if(length(tics) == 0) {
    cat("empty text file. I am returning data for the above tickers")
    return(f1(input$v,input$frequency,input$y0,input$m0,input$d0,input$price))
   }
   else
    return(f1(tics,input$frequency,input$y0,input$m0,input$d0,input$price))
  }
 })
 
 output$table.ret <- renderTable( {
  # tail(DataInput())
  DataInput()
 } )
 
 output$downloadData <- downloadHandler(
  filename = "yahoo_returns.csv",
  content = function(file) {
   write.csv(DataInput(), file,row.names = F)
  }
 )
 
 output$plot1 <- renderPlotly( {
  ds <- na.omit(DataInput())
  ds.list <- lapply(2:ncol(ds),function(i) data.frame(Date = ds[,1], Price = ds[,i], Tic = as.factor(names(ds)[i])  ) )
  ds.list <- lapply(ds.list, function(ds.i) { ds.i$Price <- ds.i$Price/ds.i$Price[1]; return(ds.i) }  )
  ds <- Reduce(rbind,ds.list)
  my.plot <- ggplot(ds, aes(x = Date, y = Price, group = Tic , colour = Tic) ) + geom_line(alpha = 0.5, size=0.7) + theme(legend.text=element_text(size=16)  )  + scale_x_discrete(breaks = ds$Date[(1:7)*floor(nrow(ds)/7)])
  my.plot <- ggplotly(my.plot)
  return(my.plot)
 } )
 
 ########################################################### TAB two
 
 # TAB 3 ##########################################################
 
 
 data_filter <- reactive({
   # Filtering parameter what stock symbol to plot
  if(input$benchmark!="13" ){
   index<- c(as.numeric(input$benchmark),as.numeric(input$stocks))
   name<-tickersTopTen_tab_4[index]
  }else{
   index<- c(as.numeric(input$stocks))
   name<-tickersTopTen_tab_4[index]
  }
  
  # Filtering parameter what period to plot
  if(input$period=="13"){
   date_min <- paste0(year(today()),"-01-01")
  }else{
   date_min <-  today() - months(as.numeric(input$period))
  }
  ##### Filtering in one step
  prices_filter <- prices_tab_4 %>%
   dplyr::filter(symbol %in% name) %>%
   dplyr::filter(date >= date_min)
  prices_filter
  
 })
 
 # Create plot
 output$plot <- renderPlotly({
  
  prices_filter<- data_filter()
  data<-prices_filter %>%
   group_by(symbol) %>%
   mutate(init_close = if_else(date == min(date),close,NA_real_)) 
  vector<-rep(NA,nrow(data))
  
  for(i in unique(data$symbol)){
   index_symbol <- which(data$symbol==i)
   vector[index_symbol] <- data$close[index_symbol]/ data$init_close[index_symbol[1]] *100
  }
  data<-data %>% mutate(value=vector)
  
  if(as.numeric(input$benchmark)== 13){
   bench_add<-data.frame(symbol = data$symbol[nrow(data):nrow(data)+1],
                         date=c(today(),today()-1 ),
                         close= c(1,1),
                         init_close =c(2,2),
                         value=c(mean(data$value),mean(data$value)+1 ))
   
   coul<- gray(level = 0,alpha = 0)
  }else{
   index<-as.numeric(input$benchmark)
   ref<-tickersTopTen_tab_4[index]
   bench_add <- data %>%
    dplyr::filter(symbol == ref )
   coul<-"red"
  }
  
  data <- data %>% filter(symbol %in% tickersTopTen_tab_4[1:10])
  
  print(
   ggplotly(data %>%
             ggplot(aes(date, value,colour = symbol)) +
             geom_line(linewidth = 1)+
             # uncomment the line below to show area under curves
             #geom_area(aes(fill=symbol),position="identity",alpha=.2) +
             theme_minimal(base_size=16) +
             theme(axis.title=element_blank(),
                   axis.text=element_text(colour="violet"),
                   plot.background = element_rect(fill = "black"),
                   panel.background = element_rect(fill="black"),
                   # panel.border = element_rect(fill="white"),
                   panel.grid = element_blank(),
                   legend.text = element_text(colour="white"))+
             geom_line(data=bench_add,aes(y=value,x= date) ,colour=coul,size=2)
   )
  )
 })
} # server

shinyApp(ui = ui, server = server)