#
# Paul Harmon
# README: The Shiny App created here is written by Paul Harmon. Its intent is to assess the sensitivity of the 
# Carnegie Classifications to small changes in the underlying variables used to calculate each index. 
#
# Note that the functions and data must be read in prior to running this code. The easiest way to do this right now is 
# probably is to just run all of the code in the R-Code for Carnegie Stuff.R file. We might source it in later .
#

#reads in the dataset needed

#projectWD <- "Z:/Carnegie Classification/Paul Harmon 2018 Carnegie Update Info/Carnegie18"
#setwd(projectWD)

library(DT)
library(dplyr);library(ggplot2);library(ggthemes);library(mclust);library(ggforce);library(shinyjs);library(plotly)
cc2015 <- filter(read.csv("CC2015data.csv",header = TRUE),BASIC2015 %in%c(15,16,17))
cc2015 <- cc2015[order(cc2015$NAME),]
#X:/PH_Desktop/Carnegie2018/Carnegie18/2018PublicData_Jan31.csv
cc18 <- filter(read.csv("2018PublicData_Jan31.csv"), BASIC2018 %in% c(15,16,17))

cc2021 <- filter(read.csv("CC2021data_Feb04.csv"),basic2021 %in% c(15,16))
cc2021 <- cc2021[order(cc2021$name),]

names <- unique(cc2015$NAME)
#gets the right package going
library(shiny); library(shinythemes)

#other functions needed
# instead treat as 0's
replace_0 <- function(vector){
  x <- ifelse(is.na(vector), 0, vector)
  x <- as.numeric(as.character(x))
  return(x)}


###UI #######################################
# Define UI for application that allows you to pick a college and see how it would change
ui <- fluidPage(
  #shiny theme
  theme = shinytheme("cosmo"),
  
  # Application title
  titlePanel("Carnegie Classifications: Montana State Internal"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
      selectInput(inputId = "school", "School", choices = names, selected = "Montana State University", multiple = FALSE,
                  selectize = TRUE), 
      useShinyjs(),
      hr(),
      actionButton("resetAll", "Reset all",style = "background-color: gray;"),
      hr(),
      div( id = "form",
           sliderInput("sosc",
                       "Number of Additional Social Science PhDs:",
                       min = 0,
                       max = 60,
                       value = 0)
           ,
           
           sliderInput("other",
                       "Number of Additional Other PhDs:",
                       min = -9,
                       max = 50,
                       value = 0)
           ,
           sliderInput("stem",
                       "Number of Additional STEM PhDs:",
                       min = -45,
                       max = 100,
                       step = 5,
                       value = 0)
           ,
           sliderInput("hum",
                       "Number of Additional Humanities PhDs:",
                       min = -2,
                       max = 50,
                       value = 0)
           ,
           sliderInput("staff",
                       "Additional Research Staff:",
                       min = -150,
                       max = 1000,
                       value = 0,
                       step = 10)
           ,
           sliderInput("serd",
                       "Additional Stem Research Exp. (Thousands of Dollars):",
                       min = -10500,
                       max = 23000,
                       value = 0,
                       step = 1000)
           ,
           sliderInput("nonserd",
                       "Additional Non-Stem Research Exp. (Thousands of Dollars):",
                       min = -10000,
                       max = 10000,
                       value = 0,
                       step = 500)
      )#div
    ), #sidebar panel
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(type = "pills",
                  tabPanel("2021 Update",list(plotlyOutput("classPlot21"),tableOutput("table.out21"))),
                  tabPanel("2018 Update",list(plotOutput("classPlot"), tableOutput("table.out"))),
                  tabPanel("2015 Update", plotOutput("ccPlot"))
                  
                  
      )
    )
  )
)



# GLOBAL CODE #######################################################
cc2015 <- read.csv("CC2015data.csv",header = TRUE)
cc18 <- read.csv('2018PublicData_Jan31.csv', header = TRUE)
CC2021 <- read.csv("CC2021data_Feb04.csv", header = TRUE)

#function for ranking the data
minrank <- function(x){rank(x, ties.method = "min")}
avrank <- function(x){rank(x, ties.method = 'average')}

#dataset that we want to use
cc2015Ps<-
  na.omit(cc2015[,c("NAME","BASIC2010","BASIC2015","FACNUM","HUM_RSD","OTHER_RSD","SOCSC_RSD","STEM_RSD","PDNFRSTAFF","S.ER.D","NONS.ER.D")])
## do the same for 2018:
cc18Ps <-
  cc18[,c("NAME","BASIC2010","BASIC2015","BASIC2018","FACNUM","HUM_RSD","OTHER_RSD","SOCSC_RSD","STEM_RSD","PDNFRSTAFF","S.ER.D","NONS.ER.D")]
cc18Ps.rm <- filter(cc18Ps, BASIC2018 %in% c(15,16)) #removes those pesky R3 schools
cc18Ps <- na.omit(as_tibble(cbind(cc18Ps.rm[,c(1:4)],lapply(cc18Ps.rm[,5:12],replace_0)))) #replaces with 0's
## do the same for 2021:
cc21Ps <-
  CC2021[,c("name","basic2021","facnum","hum_rsd","oth_rsd","socsc_rsd","stem_rsd","pdnfrstaff","serd","nonserd")]
cc21Ps.rm <- filter(cc21Ps, basic2021 %in% c(15,16)) 
cc21Ps <- na.omit(as_tibble(cbind(cc21Ps.rm[,c(1:2)],lapply(cc21Ps.rm[,3:10],replace_0)))) #replaces with 0's
dim(cc21Ps)  


### Calculate Ranks: 
#calculate the ranked data
cc2015.r <- data.frame(cc2015Ps[,1:3],sapply(cc2015Ps[,-c(1:3)],minrank)) 

## for 2018
cc18.r <- data.frame(cc18Ps[,1:4],sapply(cc18Ps[,-c(1:4)],avrank)) 
cc18percap <- cc18Ps[,c("PDNFRSTAFF","S.ER.D","NONS.ER.D")]/cc18Ps$FACNUM
colnames(cc18percap) <- c("PDNRSTAFF_PC", "S.ER.D_PC", "NONS.ER.D_PC")
cc18percap.r<-data.frame(sapply(cc18percap,avrank))

## For 2021
cc21.r <- data.frame(cc21Ps[,1:2],sapply(cc21Ps[,-c(1:2)],avrank)) 
cc21percap <- cc21Ps[,c("pdnfrstaff","serd","nonserd")]/cc21Ps$facnum
colnames(cc21percap) <- c("pdnfrstaff","serd","nonserd")
cc21percap.r<-data.frame(sapply(cc21percap,avrank))



server <- function(input, output,session) {
  # cc2015.full <- read.csv("Updated2015.csv", header = TRUE)
  
  #Defines a Reactive Expression for each school selected
  new_school <- reactive({
    # Change when the "update" button is pressed...
    input$school
  })
  
  
  #reset sliders: 
  observeEvent(input$resetAll,{
    reset("form")
  })
  
  output$classPlot21 <- renderPlotly({
    
    inst_name <- new_school()
    new_dat <- cc21Ps
    
    
    ##References School of Interest  
    a <- which(as.character(new_dat$name) == as.character(inst_name))
    #current_school <- as.character(input$school)
    
    ##To Move Left or Right
    # adds one phd to the number of social science phds
    new_dat[a,"socsc_rsd"] <- new_dat[a,"socsc_rsd"] + input$sosc
    #adds one phd to the number of other phds
    new_dat[a,"oth_rsd"] <- new_dat[a,"oth_rsd"] + input$other
    #adds one phd to the number of stem phds
    new_dat[a,"stem_rsd"] <- new_dat[a,"stem_rsd"] + input$stem
    #adds one phd to the number of humanities phds
    new_dat[a,"hum_rsd"] <- new_dat[a,"hum_rsd"] + input$hum
    
    
    #adds research staff by 1 ; also adds to the FACNUM
    new_dat[a,"pdnfrstaff"] <- new_dat[a,"pdnfrstaff"] + input$staff
    
    #adds to NonSERD expenditures
    new_dat[a,"nonserd"] <- new_dat[a,"nonserd"] + input$nonserd
    #adds to SERD expenditures
    new_dat[a,"serd"] <- new_dat[a,"serd"] + input$serd
    
    ## Calculates the Per-Capita version of the data frame
    #creates the newdat pc object 
    new_dat_pc <- new_dat[,c("pdnfrstaff","serd","nonserd")]/new_dat$facnum
    
    ### Calculates the 2021 Version of the Carnegie Classifications: 
    
    AGcc <- function(x){
      #rank the data (everything but name (column 1) and basic2021 (column 2))
      ranked <- data.frame(x[,1:2],sapply(x[,-c(1:2)],avrank)) 
      #get pc's (Principal Components Analysis) 
      pca.ranked <- prcomp(ranked[,-c(1:3)], scale = TRUE)
      summary <- summary(pca.ranked)
      standard.score <- pca.ranked$x[,1] - min(pca.ranked$x[,1])
      #needs to return the standardized scores
      return(list(scorez = standard.score, sum =summary))
    }
    #function for percap
    PCcc <- function(x){
      #rank the data
      ranked.dat <- data.frame(sapply(new_dat_pc,avrank))
      #get pc's
      pc.ranked <- prcomp(ranked.dat, scale = TRUE)
      summary <- summary(pc.ranked)
      standard.score <- -pc.ranked$x[,1] - min(-pc.ranked$x[,1])
      return(list(scorez = standard.score, sum = summary))
    }
    
    
    percap <- PCcc(new_dat_pc)
    ag <- AGcc(new_dat)
    
    scores21 <- tibble(cc21.r$name,ag$scorez, percap$scorez, cc21.r$basic2021)
    names(scores21) <- c("Name","Ag","PC", "Status")
    scores21$Symbols <- rep(0, nrow(scores21))
    scores21$Symbols[a] <- 1
    scores21$Alpha <- rep(0, nrow(scores21))
    scores21$Alpha[a] <- 1
    
    
    #creates a plot and colors by Carnegie Classification Colors  
    # ggplot(scores21, aes(Ag, PC)) + geom_point(aes(color = factor(Status), shape = factor(Symbols), size = factor(Symbols)))  + 
    #  ggtitle("2021 Classifications") + theme_classic() + coord_fixed(ratio = 1) + guides(shape = FALSE, size = FALSE) + 
    #  theme(plot.title = element_text(hjust = 0.5)) + scale_color_discrete(name = "Classification") + 
    #  scale_alpha_manual(aes(Alpha)) + xlab("Aggregate") + ylab("Per Capita")
    
    # Plotly code 
    scores21 %>% 
      plot_ly(x= ~Ag, y= ~PC, type="scatter", mode="markers",
              color = ~as.factor(Status), colors = "Set2",
              symbol = ~as.factor(Symbols),
              name = ~as.factor(Status),
              text = ~as.factor(Name),
              hoverinfo = 'text') %>% 
      layout( 
        title = list(title="2021 Classifications", titlefont = list(size=30)),
        xaxis = list(title = "Aggregate", showgrid = FALSE, titlefont = list(size=20)),
        yaxis = list(title = "Per Capita", showgrid = FALSE, titlefont = list(size=20)))
    
    
  })
  
  ## table of current values
  output$table.out21 <- renderTable({
    
    inst_name <- new_school()
    
    a <- which(as.character(cc2015Ps$NAME) == as.character(inst_name))
    
    #new_table <- rbind(cc2021Ps[a,],topten)
    inst_name <- new_school()
    new_dat <- cc21Ps
    
    
    ##References School of Interest  
    a <- which(as.character(new_dat$name) == as.character(inst_name))
    #current_school <- as.character(input$school)
    print(new_dat[a,])
    
  })
  
  
  output$classPlot <- renderPlot({
    
    inst_name <- new_school()
    new_dat <- cc18Ps
    
    
    ##References School of Interest  
    a <- which(as.character(new_dat$NAME) == as.character(inst_name))
    #current_school <- as.character(input$school)
    
    ##To Move Left or Right
    # adds one phd to the number of social science phds
    new_dat[a,"SOCSC_RSD"] <- new_dat[a,"SOCSC_RSD"] + input$sosc
    #adds one phd to the number of other phds
    new_dat[a,"OTHER_RSD"] <- new_dat[a,"OTHER_RSD"] + input$other
    #adds one phd to the number of stem phds
    new_dat[a,"STEM_RSD"] <- new_dat[a,"STEM_RSD"] + input$stem
    #adds one phd to the number of humanities phds
    new_dat[a,"HUM_RSD"] <- new_dat[a,"HUM_RSD"] + input$hum
    
    
    #adds research staff by 1 ; also adds to the FACNUM
    new_dat[a,"PDNFRSTAFF"] <- new_dat[a,"PDNFRSTAFF"] + input$staff
    
    #adds to NonSERD expenditures
    new_dat[a,"NONS.ER.D"] <- new_dat[a,"NONS.ER.D"] + input$nonserd
    #adds to SERD expenditures
    new_dat[a,"S.ER.D"] <- new_dat[a,"S.ER.D"] + input$serd
    
    ## Calculates the Per-Capita version of the data frame
    #creates the newdat pc object 
    new_dat_pc <- new_dat[,c("PDNFRSTAFF","S.ER.D","NONS.ER.D")]/new_dat$FACNUM
  
    ### Calculates the 2018 Version of the Carnegie Classifications: 
    
    
    AGcc <- function(x){
      #rank the data
      ranked <- data.frame(x[,1:4],sapply(x[,-c(1:4)],avrank)) 
      #get pc's
      pca.ranked <- prcomp(ranked[,-c(1:5)], scale = TRUE)
      summary <- summary(pca.ranked)
      standard.score <- pca.ranked$x[,1] - min(pca.ranked$x[,1])
      #needs to return the standardized scores
      return(list(scorez = standard.score, sum =summary))
    }
    #function for percap
    PCcc <- function(x){
      #rank the data
      ranked.dat <- data.frame(sapply(new_dat_pc,avrank))
      #get pc's
      pc.ranked <- prcomp(ranked.dat, scale = TRUE)
      summary <- summary(pc.ranked)
      standard.score <- -pc.ranked$x[,1] - min(-pc.ranked$x[,1])
      return(list(scorez = standard.score, sum = summary))
    }
    
    
    percap <- PCcc(new_dat_pc)
    ag <- AGcc(new_dat)
   
    scores18 <- tibble(cc18.r$NAME,ag$scorez, percap$scorez, cc18.r$BASIC2018)
    names(scores18) <- c("Name","Ag","PC", "Status")
    scores18$Symbols <- rep(0, nrow(scores18))
    scores18$Symbols[a] <- 1
    scores18$Alpha <- rep(0, nrow(scores18))
    scores18$Alpha[a] <- 1
    
    
    #creates a plot and colors by Carnegie Classification Colors  
    ggplot(scores18, aes(Ag, PC)) + geom_point(aes(color = factor(Status), shape = factor(Symbols), size = factor(Symbols)))  + 
      ggtitle("2018 Classifications") + theme_classic() + coord_fixed(ratio = 1) + guides(shape = FALSE, size = FALSE) + 
      theme(plot.title = element_text(hjust = 0.5)) + scale_color_discrete(name = "Classification") + 
        scale_alpha_manual(aes(Alpha)) + xlab("Aggregate") + ylab("Per Capita")
     
    
    
  })
  
  ## table of current values
  output$table.out <- renderTable({
    
    inst_name <- new_school()
    
    a <- which(as.character(cc2015Ps$NAME) == as.character(inst_name))
    
    #new_table <- rbind(cc2015Ps[a,],topten)
    inst_name <- new_school()
    new_dat <- cc18Ps
    
    
    ##References School of Interest  
    a <- which(as.character(new_dat$NAME) == as.character(inst_name))
    #current_school <- as.character(input$school)
    print(new_dat[a,])
    
  })
  
  #Carnegie Classifications Version of this
  output$ccPlot <- renderPlot({
    
    inst_name <- new_school()
    new_dat <- cc2015Ps
    
    
    ##References School of Interest  
    a <- which(as.character(new_dat$NAME) == as.character(inst_name))
    #current_school <- as.character(input$school)
    
    ##To Move Left or Right
    # adds one phd to the number of social science phds
    new_dat[a,"SOCSC_RSD"] <- new_dat[a,"SOCSC_RSD"] + input$sosc
    #adds one phd to the number of other phds
    new_dat[a,"OTHER_RSD"] <- new_dat[a,"OTHER_RSD"] + input$other
    #adds one phd to the number of stem phds
    new_dat[a,"STEM_RSD"] <- new_dat[a,"STEM_RSD"] + input$stem
    #adds one phd to the number of humanities phds
    new_dat[a,"HUM_RSD"] <- new_dat[a,"HUM_RSD"] + input$hum
    
    
    #adds research staff by 1 ; also adds to the FACNUM
    new_dat[a,"PDNFRSTAFF"] <- new_dat[a,"PDNFRSTAFF"] + input$staff
    
    #adds to NonSERD expenditures
    new_dat[a,"NONS.ER.D"] <- new_dat[a,"NONS.ER.D"] + input$nonserd
    #adds to SERD expenditures
    new_dat[a,"S.ER.D"] <- new_dat[a,"S.ER.D"] + input$serd
    
    #creates the newdat pc object 
    new_dat_pc <- new_dat[,c("PDNFRSTAFF","S.ER.D","NONS.ER.D")]/new_dat$FACNUM
    
    
    AGcc <- function(x){
      #rank the data
      ranked <- data.frame(x[,1:3],sapply(x[,-c(1:3)],minrank)) 
      #get pc's
      pca.ranked <- prcomp(ranked[,-c(1:4)], scale = TRUE)
      summary <- summary(pca.ranked)
      standard.score <- scale(pca.ranked$x[,1], scale = TRUE, center = TRUE)
      #needs to return the standardized scores
      return(list(scorez = standard.score, sum =summary))
    }
    #function for percap
    PCcc <- function(x){
      #rank the data
      ranked.dat <- data.frame(sapply(x,minrank)) 
      #get pc's
      pc.ranked <- prcomp(ranked.dat, scale = TRUE)
      summary <- summary(pc.ranked)
      standard.score <- scale(pc.ranked$x[,1], scale = TRUE, center = TRUE)
      return(list(scorez = standard.score, sum = summary))
    }
    
    
    percap <- PCcc(new_dat_pc)
    ag <- AGcc(new_dat)
    
    mean.percap <- 340.52
    sd.percap <- 170.51
    mean.ag <- 780.74
    sd.ag <- 413.10
    
    rawscores.percap <- sd.percap * -percap$scorez + mean.percap
    rawscores.ag <- sd.ag * ag$scorez + mean.ag
    
    #X1 <- seq(500,984,by =1)
    R1 <- 984.007
    #X2 <- seq(0,409,by =1)
    R2<- 409.461
    
    #establish symbols
    cdat <- data.frame(cbind(rawscores.ag,rawscores.percap,factor(cc2015Ps$BASIC2015))); names(cdat) <- c("AG","PC","Basic")
    cdat$Symbols <- rep(0, nrow(cdat))
    cdat$Symbols[a] <- 1
    #creates a plot and colors by Carnegie Classification Colors  
    ggplot(cdat) + geom_point(aes(x =AG ,y = PC, color = factor(Basic),shape = factor(Symbols),size = factor(Symbols))) + theme_classic() + coord_fixed(ratio = 1) +
      xlab("Aggregate Index") + ylab("Per-Capita Index") + ggtitle("2015 Carnegie Classifications") + 
      scale_color_discrete(name = "Classification",labels = c("R1","R2","R3")) + guides(shape = FALSE, size = FALSE)+
      stat_arc(aes(x0 = 0, y0 = 0, r = R1, start = 0,end = 1.58)) + 
      stat_arc(aes(x0 = 0,y0 = 0, r = R2, start = 0, end = 1.555 ))
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

