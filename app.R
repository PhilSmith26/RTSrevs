# Shiny app to calculate monthly retail trade growth rate 
# revisions and analyze them - July 24, 2021

library(shiny)
library(tidyverse)
library(stringr)
library(lubridate)
library(simplecolors)

r0 <- readRDS("r0.rds")

#================================================================================
Rev_table <- function(indLG,r0) {
  #browser()
  r1 <- dplyr::filter(r0,NAICS==indLG)
  indST <- sub(".*?\\[","[",indLG)
  r1 <- dplyr::rename(r1,"REF"="REF_DATE","VAL"="VALUE") 
  # convert REL to a date
  r1 <- dplyr::mutate(r1,REL=as.Date(Release,format="%B %d, %Y"))
  r1 <- dplyr::mutate(r1,DAY=lubridate::day(REL))
  r1 <- dplyr::mutate(r1,MONTH=lubridate::month(REL))
  r1 <- dplyr::mutate(r1,YEAR=lubridate::year(REL))
  # r1[,.N,DAY] # Display the frequency for all days of the week
  # Result is that 1, 2 and 3 occur occasionally, 21, 22 and 23 
  # sometimes (Xmas) and 28, 29, 30 and 31 most of the time
  # select only the variables needed
  r1 <- dplyr::select(r1,REF,REL,VAL,DAY,MONTH,YEAR)
  # Convert release dates to months (make DAY=1 and shift MONTH 
  # back if needed). For example, release date of February 2, 
  # 2020 becomes January 1, 2020
  r1 <- dplyr::mutate(r1,MONTH=ifelse(DAY==1 | DAY==2 | DAY==3 | 
    DAY==4 | DAY==5, MONTH-1,MONTH))
  # Now reconstruct the release date as the first of the month 
  # in that possibly modified month
  r1 <- dplyr::mutate(r1,REL=as.Date(paste0(YEAR,"-",MONTH,"-01")))
  r1 <- dplyr::select(r1,REF,REL,VAL)
  # Nesting creates a list-column of data frames; unnesting flattens 
  # it back out into regular columns. Nesting is implicitly a 
  # summarising operation: you get one row for each group defined 
  # by the non-nested columns. This is useful in conjunction with 
  # other summaries that work with whole datasets, most notably models.
  # In this case, df1 becomes a list-column with one row for each 
  # release date (REL). The first column is REL and the second 
  # column has two columns with all the reference dates and all 
  # the corresponding values.
  df1 <- tidyr::nest(r1, data = -REL ) # r1 has 5852 rows, df1 has 77
  df1 <- dplyr::rename(df1, REL2 = REL )
  df1 <- dplyr::rowwise(df1) # seems to have no effect
  # The following is the only use of the do_per_REL() function
  # It converts the values to percentage changes
  df1 <- dplyr::mutate(df1, data = list( do_per_REL( data ) ) )
  df1 <- dplyr::ungroup(df1)
  df1 <- tidyr::unnest(df1, cols = "data" )
  df2 <- dplyr::select(df1, REF2, REL2, VAL2 )
  df2 <- dplyr::arrange(df2, REF2, desc( REL2 ), VAL2 )
  df4 <- df2[!is.na(df2$VAL2),]
  df4 <- dplyr::filter(df4,REF2>=as.Date("2015-01-01"))
  if (indST=="[44-45]") {
    REF2 <- c(as.Date("2020-04-01"),
      as.Date("2020-05-01"),as.Date("2020-06-01"),as.Date("2020-07-01"),
      as.Date("2020-08-01"),as.Date("2020-09-01"))
    REL2 <- c(as.Date("2020-05-01"),
      as.Date("2020-06-01"),as.Date("2020-07-01"),as.Date("2020-08-01"),
      as.Date("2020-09-01"),as.Date("2020-10-01"))
    VAL2 <- c(-15.6,19.1,24.5,0.7,1.1,0.0)
    newdf4 <- data.frame(REF2,REL2,VAL2)
    df4 <- rbind(df4,newdf4)
    df4 <- dplyr::arrange(df4,REF2,REL2)
  }
  df5 <- df4
  df5 <- dplyr::group_by(df5,REF2)
  df5 <- dplyr::mutate(df5,range=max(VAL2)-min(VAL2))
  df5 <- dplyr::ungroup(df5)
  df5 <- dplyr::mutate(df5,REF2_pure=REF2)
  df5 <- dplyr::mutate(df5,REF2=paste0(str_sub(as.character(REF2),1,7),"\n Range = ",
    as.character(round(range,2))))
  return(df5)
}
#================================================================================
Rev_charts <- function(df5,indLG) {  
  # This prints the "revisions paths" facet chart
  c1 <- ggplot2::ggplot(dplyr::filter(df5,REF2_pure<as.Date("2021-04-01")),
      aes(x=REL2,y=VAL2,group=REF2))+
    geom_line()+
    labs(title=as.character(indLG),
         caption=paste0("Source: Statistics Canada table 36-10-0491-01."),
         x="",y="Monthly % change") +
    theme(strip.text.x=element_text(size=10,colour="black",face="bold"),
          strip.background=element_rect(colour="black",fill=sc("violet1")),
          panel.background=element_rect(fill=sc("brightgreen1"),colour="black")) +
    theme(panel.grid.minor=element_line(colour="lightgrey",size=0.5)) +
    theme(panel.grid.major=element_line(colour="grey",size=0.5)) +
    theme(plot.title = element_text(size=16,face="bold")) +
    theme(plot.subtitle = element_text(size=12,face="bold")) +
    theme(axis.text.x = element_text(angle=0,hjust=1,size=9)) +
    theme(axis.text.y = element_text(size=9))+
    facet_wrap(~REF2,scales="free_y",ncol=6)
  return(c1)
}  
#================================================================================
Rev_stats <- function(df5) { # df5 is same as r2
  Stats=data.frame()
  rng <- range(str_sub(df5$REF2,1,10))
  r3 <- data.frame(REF=seq.Date(as.Date(paste0(str_sub(rng[1],
    1,7),"-01")),as.Date(paste0(str_sub(rng[2],1,7),"-01")),by="month"))
  r3 <- dplyr::filter(r3,REF<=as.Date("2020-12-01"))
  r3 <- dplyr::mutate(r3,VAL00=round(LUPV(REF,0,df5),2),
    VAL01=round(LUPV(REF,1,df5),2),
    VAL02=round(LUPV(REF,2,df5),2),
    VAL03=round(LUPV(REF,3,df5),2),
    VAL04=round(LUPV(REF,4,df5),2),
    REV01=round(VAL01-VAL00,2),
    REV02=round(VAL02-VAL01,2),
    REV03=round(VAL03-VAL02,2),
    REV04=round(VAL04-VAL03,2))
  (MEAN01 <- mean(r3$REV01))
  (MEAN02 <- mean(r3$REV02))
  (MEAN03 <- mean(r3$REV03))
  (MEAN04 <- mean(r3$REV04))
  (MABS01 <- mean(abs(r3$REV01)))
  (MABS02 <- mean(abs(r3$REV02)))
  (MABS03 <- mean(abs(r3$REV03)))
  (MABS04 <- mean(abs(r3$REV04)))
  (RMS01 <- RMS(r3$REV01))
  (RMS02 <- RMS(r3$REV02))
  (RMS03 <- RMS(r3$REV03))
  (RMS04 <- RMS(r3$REV04))
  Stats[1,1] <- round(MEAN01,3)
  Stats[1,2] <- round(MEAN02,3)
  Stats[1,3] <- round(MEAN03,3)
  Stats[1,4] <- round(MEAN04,3)
  Stats[2,1] <- round(MABS01,3)
  Stats[2,2] <- round(MABS02,3)
  Stats[2,3] <- round(MABS03,3)
  Stats[2,4] <- round(MABS04,3)
  Stats[3,1] <- round(RMS01,3)
  Stats[3,2] <- round(RMS02,3)
  Stats[3,3] <- round(RMS03,3)
  Stats[3,4] <- round(RMS04,3)
  colnames(Stats) <- c("1st revision","2nd revision",
    "3rd revision","4th revision")
  rownames(Stats) <- c("Arithmetic mean","Mean absolute value",
    "Root mean squared")
  dat <- list(Stats,r3)
  return(dat)
}
#================================================================================
Rev_scatter <- function(r3,LAG,indLG) {
  # Scatter chart for 1-, 2-, 3- and 4- month later revs
  if (LAG==1) {titl <- paste0("\nScatter diagram for one-month-later",
    "\nretail trade growth rate revisions")}
  else if (LAG==2) {titl <- paste0("\nScatter diagram for two-months-later\n",
    "retail trade growth rate revisions")}
  else if (LAG==3) {titl <- paste0("\nScatter diagram for three-months-later\n",
    "retail trade growth rate revisions")}
  else {titl <- paste0("\nScatter diagram for four-months-later\n",
    "retail trade growth rate revisions")}
  p0 <- ggplot(r3)+
    labs(title=paste0(indLG,titl),
         x="Reference months, February 2015 to December 2020",
         y="Percentage point revisions to growth rates",
         caption=paste0("Source: Statistics Canada table 36-10-0491-01."))+
    theme(plot.title = element_text(size=20,face="bold")) +
    theme(plot.subtitle = element_text(size=16,face="bold")) +
    theme(axis.text.x = element_text(angle=0,hjust=1,size=16)) +
    theme(axis.text.y = element_text(size=16))+
    theme(panel.background = element_rect(fill="aliceblue",colour="black")) +
    theme(panel.border = element_rect(fill=NA,colour="black"))+
    theme(plot.background = element_rect(fill="#FFFACD",colour="black"))+
    geom_hline(yintercept=0,colour="black",size=0.8)+
    if (LAG==1) {geom_point(aes(x=REF,y=REV01),colour="black")}
    else if (LAG==2) {geom_point(aes(x=REF,y=REV02),colour="black")}
    else if (LAG==3) {geom_point(aes(x=REF,y=REV03),colour="black")}
    else if (LAG==4) {geom_point(aes(x=REF,y=REV04),colour="black")}
    else {}
    return(p0)
}
#================================================================================
Rev_autocor <- function(r3,LAG,indLG) {
  if (LAG==1) {AUT0 <- acf(r3$REV01,plot=FALSE)} 
  else if (LAG==2) AUT0 <- acf(r3$REV02,plot=FALSE) 
  else if (LAG==3) AUT0 <- acf(r3$REV03,plot=FALSE) 
  else if (LAG==4) AUT0 <- acf(r3$REV04,plot=FALSE) 
  else {}
  tmp <- AUT0[[1]]
  df <- data.frame(lag1=1:length(tmp),auto1=tmp)
  # Autocorrelation chart for 1-, 2-, 3- and 4- month later revs
  if (LAG==1) {titl <- paste0("\nAutocorrelation diagram for one-month-later",
    "\nretail trade growth rate revisions")}
  else if (LAG==2) {titl <- paste0("\nAutocorrelation diagram for two-months-later\n",
    "retail trade growth rate revisions")}
  else if (LAG==3) {titl <- paste0("\nAutocorrelation diagram for three-months-later\n",
    "retail trade growth rate revisions")}
  else {titl <- paste0("\nAutocorrelation diagram for four-months-later\n",
    "retail trade growth rate revisions")}
  p0 <- ggplot(df)+
    labs(title=paste0(indLG,titl),
         x="Autocorrelation lag",
         y="Correlation",
         caption=paste0("Source: Statistics Canada table 36-10-0491-01."))+
    theme(plot.title = element_text(size=20,face="bold")) +
    theme(plot.subtitle = element_text(size=16,face="bold")) +
    theme(axis.text.x = element_text(angle=0,hjust=1,size=16)) +
    theme(axis.text.y = element_text(size=16))+
    theme(panel.background = element_rect(fill="aliceblue",colour="black")) +
    theme(panel.border = element_rect(fill=NA,colour="black"))+
    theme(plot.background = element_rect(fill="#FFFACD",colour="black"))+
    geom_hline(yintercept=0,colour="black",size=0.8)+
    geom_col(aes(x=lag1,y=auto1),fill="blue",colour="black")
  return(p0)
}
#================================================================================
Rev_density <- function(r3,LAG,indLG) {
  # Probability densities for 1-, 2-, 3- and 4- months later revs
  if (LAG==1) {titl <- paste0("\nEstimated density for one-month-later",
    "\nretail trade growth rate revisions")}
  else if (LAG==2) {titl <- paste0("\nEstimated density for two-months-later\n",
    "retail trade growth rate revisions")}
  else if (LAG==3) {titl <- paste0("\nEstimated density for three-months-later\n",
    "retail trade growth rate revisions")}
  else {titl <- paste0("\nEstimated density for four-months-later\n",
    "retail trade growth rate revisions")}  
  p1 <- ggplot2::ggplot(r3)+
         geom_vline(xintercept=0,linetype="dotted")+  
         labs(title=paste0(indLG,titl),
         x="Monthly percentage revision in percentage points",
         y="Probability density",
         caption=paste0("Source: Statistics Canada table 36-10-0491-01."))+
         theme(plot.title = element_text(size=20,face="bold")) +
         theme(plot.subtitle = element_text(size=16,face="bold")) +
         theme(axis.text.x = element_text(angle=0,hjust=1,size=16)) +
         theme(axis.text.y = element_text(size=16))+
         theme(panel.border = element_rect(fill=NA,colour="black"))+
         theme(plot.background = element_rect(fill="#FFFACD",colour="black"))+
         geom_hline(yintercept=0,colour="black",size=0.8)+
         if (LAG==1)      {geom_density(aes(x=REV01),fill="hotpink2",
           colour="black",alpha=1.0)}
         else if (LAG==2) {geom_density(aes(x=REV02),fill="darkolivegreen2",
           colour="black",alpha=1.0)}
         else if (LAG==3) {geom_density(aes(x=REV03),fill="cadetblue2",
           colour="black",alpha=1.0)}
         else if (LAG==4) {geom_density(aes(x=REV04),fill="bisque",
           colour="black",alpha=1.0)}
         else {}
  p1 <- p1+geom_vline(xintercept=0,linetype="dotted",size=1)
  return(p1)
}
#================================================================================
IndustriesL <- c(
"Retail trade [44-45]",                                              
"Motor vehicle and parts dealers [441]",                             
"Automobile dealers [4411]",                                         
"New car dealers [44111]",                                           
"Used car dealers [44112]",                                          
"Other motor vehicle dealers [4412]",                                
"Automotive parts, accessories and tire stores [4413]",              
"Furniture and home furnishings stores [442]",                       
"Furniture stores [4421]",                                           
"Home furnishings stores [4422]",                                    
"Electronics and appliance stores [443]",                            
"Building material and garden equipment and supplies dealers [444]", 
"Food and beverage stores [445]",                                    
"Grocery stores [4451]",                                             
"Supermarkets and other grocery (except convenience) stores [44511]",
"Convenience stores [44512]",                                        
"Specialty food stores [4452]",                                      
"Beer, wine and liquor stores [4453]",                               
"Health and personal care stores [446]",                             
"Gasoline stations [447]",                                           
"Clothing and clothing accessories stores [448]",                    
"Clothing stores [4481]",                                            
"Shoe stores [4482]",                                                
"Jewellery, luggage and leather goods stores [4483]",                
"Sporting goods, hobby, book and music stores [451]",                
"General merchandise stores [452]",                                  
"Department stores [4521]",                                          
"Other general merchandise stores [4529]",                           
"Miscellaneous store retailers [453]",                               
"Cannabis stores [453993]")
#================================================================================
# The LUP function retrieves values from the r2 data frame, for a given
# reference date and a given LEAD (LEAD=0 for the first estimate)
LUP <- function(REF_value,LEAD,r2) { # lookup single value function
  # First, if the reference date that is passed to the function
  # is a character value, convert it to a date value
  if (class(REF_value)=="character") REF_value <- as.Date(REF_value)
  # The %m+% function adds a number of months (or years) to a
  # given date without rollover, so as.Date("2021-02-01") %m+% 
  # months(3) gives as.Date("2021-05-01").
  # So the following statement calculates a particular release 
  # value for a given reference date, either the first one (LEAD=0)
  # or some future revised one
  REL_value <- as.Date(REF_value %m+% months(2+LEAD))
  # This filter shrinks the data frame so that the revision
  # value VAL2 can be isolated and returned to the calling
  # function
  df <- dplyr::filter(r2,REF2_pure==REF_value & REL2==REL_value)
  return(df$VAL2)
}
#================================================================================
# The LUPV function is similar to the LUP function, but it returns a
# vector of values rather than a single value. It makes calls to
# the LUP function to build up the vector to return
LUPV <- function(REF_vector,LEAD,r2) { # lookup vector function
  if (class(REF_vector)=="character") REF_vector <- as.Date(REF_vector)
  REL_vector <- as.Date(REF_vector %m+% months(2+LEAD))
  VAL_vector <- numeric()
  for (i in 1:length(REF_vector)) {
    VAL_vector[i] <- LUP(REF_vector[i],LEAD,r2)
  }
  return(VAL_vector)
}
#================================================================================
RMS <- function(x) {
  rms <- sqrt(mean((x-mean(x))^2))
}
#================================================================================
do_per_REL <- function(DF) {
    # range() returns a vector containing the minimum and maximum of 
    # all the given arguments.
    rng <- range(DF$REF) # watch out for missing months?
    DF <- (data.frame(REF=seq(rng[1],rng[2],by="month"))
      # left_join() returns all rows from x, and all columns from x and y. 
      # Rows in x with no match in y will have NA values in the new columns. 
      # If there are multiple matches between x and y, all combinations of 
      # the matches are returned.
      # The next statement compares the input DF to the sequential one
      # just created and adds NAs if any are missing in DF
      %>% dplyr::left_join(DF,by="REF")
          %>% dplyr::arrange(REF) # arrange REF from first to last ref period
          )
    # with() evaluates an R expression in an environment constructed from data, 
    # possibly modifying (a copy of) the original data.
    # REF2=REF[-1] just drops the first item in the vector, which is
    # necessary because the % change calculation shortens the vector
    # with() example: z = with(mtcars, mean(cyl + disp + wt))
    with(DF,data.frame(REF2=REF[-1],VAL2=100*diff(VAL)/VAL[-length(VAL)]))
  }
#===============================================================================

ui <- fluidPage(
  theme=bslib::bs_theme(bg="#FFE4C4",fg="black",base_font="Source Sans Pro",
    primary="#0000FF",secondary="#63B8FF",success="#54FF9F",info="#40E0D0",
    warning="#FFFF00",danger="#FF0000",heading_font="Helvetica",font_scale=1.1),
  # #0b3d91 - blue
  #tags$head(tags$style(HTML('* {font-family: "Helvetica"};'))),
  title = tags$b(tags$span(style="color:red", 
  "Monthly retail trade survey revisions analysis")),
  windowTitle = "Canadian monthly retail trade analysis",
  fluidRow(column(3,HTML("<h2><b>Retail trade revisions analysis</b></h2>")),
    tags$style(HTML(".selectize-input, .option {
        color:blue; 
        font-size:26px;
        font-family:'Source Sans Pro'
      }")),
    column(9,selectInput("Industry",tags$b(tags$span(style="color:blue;
      font-weight:900;font-size:20px;font-family:'Source Sans Pro'", 
      "Choose an industry:")),    # "Choose an industry:",
      choices=IndustriesL,width="50%"))),
  navlistPanel(widths=c(3,9),
    tabPanel("Description",
      htmlOutput("textInfo"),
      HTML("<br><br><br><br><br><br><br><br>")),
    tabPanel("Scatter diagrams",
      htmlOutput("scatInfo"),
      fluidRow(
        column(6,plotOutput("scatter1")),
        column(6,plotOutput("scatter2"))),
      HTML("<br>"),
      fluidRow(
        column(6,plotOutput("scatter3")),
        column(6,plotOutput("scatter4")))),
    tabPanel("Measures of central tendency and dispersion",
      htmlOutput("centralTendencyInfo"),
      htmlOutput("industry"),
      tableOutput("stats")),
    tabPanel("Autocorrelations",
      htmlOutput("autocInfo"),
      fluidRow(
        column(6,plotOutput("autocor1")),
        column(6,plotOutput("autocor2"))),
      HTML("<br>"),
      fluidRow(
        column(6,plotOutput("autocor3")),
        column(6,plotOutput("autocor4")))),      
    tabPanel("Estimated probability densities",
      htmlOutput("densityInfo"),
      fluidRow(
        column(6,plotOutput("density1")),
        column(6,plotOutput("density2"))),
      HTML("<br>"),
      fluidRow(
        column(6,plotOutput("density3")),
        column(6,plotOutput("density4")))),
    tabPanel("Revision time paths",
      htmlOutput("pathsInfo"),
      plotOutput("RevChrt",width="100%",height="2500px")),
    tabPanel("About",
      htmlOutput("aboutInfo"))
  )
)
server <- function(input,output,session) {
  info <- "textInfo.html"
  output$textInfo <- renderUI(includeHTML(info))
  ctinfo <- "Central_tendency_measures.html"
  output$centralTendencyInfo <- renderUI(includeHTML(ctinfo))
  scat <- "scatterInfo.html"
  output$scatInfo <- renderUI(includeHTML(scat))
  autoc <- "autocorInfo.html"
  output$autocInfo <- renderUI(includeHTML(autoc))
  dens <- "densityInfo.html"
  output$densityInfo <- renderUI(includeHTML(dens))
  paths <- "pathsInfo.html"
  output$pathsInfo <- renderUI(includeHTML(paths))
  about <- "aboutInfo.html"
  output$aboutInfo <- renderUI(includeHTML(about))
  output$industry <- renderText(ind())
  ind <- reactive(input$Industry)
  revs <- reactive(Rev_table(ind(),r0))
  chrt <- reactive(Rev_charts(revs(),ind()))
  dat <- reactive({Rev_stats(revs())})
  output$RevChrt <- renderPlot({chrt()})
  output$stats <- renderTable(dat()[[1]],rownames=TRUE,bordered=TRUE)
  output$scatter1 <- renderPlot({Rev_scatter(dat()[[2]],1,ind())})
  output$scatter2 <- renderPlot({Rev_scatter(dat()[[2]],2,ind())})
  output$scatter3 <- renderPlot({Rev_scatter(dat()[[2]],3,ind())})
  output$scatter4 <- renderPlot({Rev_scatter(dat()[[2]],4,ind())})
  output$autocor1 <- renderPlot({Rev_autocor(dat()[[2]],1,ind())})
  output$autocor2 <- renderPlot({Rev_autocor(dat()[[2]],2,ind())})
  output$autocor3 <- renderPlot({Rev_autocor(dat()[[2]],3,ind())})
  output$autocor4 <- renderPlot({Rev_autocor(dat()[[2]],4,ind())})
  output$density1 <- renderPlot({Rev_density(dat()[[2]],1,ind())})
  output$density2 <- renderPlot({Rev_density(dat()[[2]],2,ind())})
  output$density3 <- renderPlot({Rev_density(dat()[[2]],3,ind())})
  output$density4 <- renderPlot({Rev_density(dat()[[2]],4,ind())})
}
shinyApp(ui,server)
