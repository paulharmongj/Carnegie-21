

projectWD <- "Z:/Carnegie Classification/Paul Harmon 2018 Carnegie Update Info/Carnegie18"
setwd(projectWD)

library(DT)
library(dplyr);library(ggplot2);library(ggthemes);library(mclust);library(ggforce);
library(shiny); library(shinythemes);

library(readr)
CC2021 <- read.csv("CC2021data_Feb03.csv", header = TRUE)
dim(CC2021)

#other functions needed
# instead treat as 0's
replace_0 <- function(vector){
  x <- ifelse(is.na(vector), 0, vector)
  x <- as.numeric(as.character(x))
  return(x)}

## Dataset that we want to use for 2021:

# Paul --- Do we want to used this kind of data set or the one that you filter (carn_filter)?
cc21Ps <-
  CC2021[,c("name","basic2021","facnum","hum_rsd","oth_rsd","socsc_rsd","stem_rsd","pdnfrstaff","serd","nonserd")]
cc21Ps.rm <- filter(cc21Ps, basic2021 %in% c(15,16)) 
cc21Ps <- na.omit(as_tibble(cbind(cc21Ps.rm[,c(1:2)],lapply(cc21Ps.rm[,3:10],replace_0)))) #replaces with 0's
dim(cc21Ps)  

# I ordered the columns in the same order, so all the functions work for both ways
# I did not used carn_filter. I used cc21ps
#carn_filter <- CC2021 %>% dplyr::filter(basic2021 %in% c(15,16)) %>% 
 # select(name, basic2021, facnum, serd, hum_rsd, oth_rsd, socsc_rsd, stem_rsd, pdnfrstaff, serd, nonserd)

# cc21Ps %>% head(3)
# carn_filter %>% head(3)

## work with carn_filter
## follow the steps outlined in document ()

## step through functions and compare resulting plot to Carnegie documentation

# Rank function using average method where ties all receive the average value
# of the group followed by the next available number. 
avrank <- function(x){rank(x, ties.method = 'average')}


### Calculate Ranks: 
cc21.r <- data.frame(cc21Ps[,1:2],sapply(cc21Ps[,-c(1:2)],avrank)) 
cc21percap <- cc21Ps[,c("pdnfrstaff","serd","nonserd")]/cc21Ps$facnum
colnames(cc21percap) <- c("pdnfrstaff","serd","nonserd")
cc21percap.r<-data.frame(sapply(cc21percap,avrank))

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

new_dat <- cc21Ps
new_dat_pc <- new_dat[,c("pdnfrstaff","serd","nonserd")]/new_dat$facnum

percap <- PCcc(new_dat_pc)
ag <- AGcc(new_dat)


scores21 <- tibble(cc21.r$name,ag$scorez, percap$scorez, cc21.r$basic2021)
names(scores21) <- c("Name","Ag","PC", "Status")
scores21$Symbols <- rep(0, nrow(scores21))
# scores21$Symbols[a] <- 1
scores21$Alpha <- rep(0, nrow(scores21))
# scores21$Alpha[a] <- 1

#creates a plot and colors by Carnegie Classification Colors  
ggplot(scores21, aes(Ag, PC)) + geom_point(aes(color = factor(Status), shape = factor(Symbols), size = factor(Symbols)))  + 
  ggtitle("2021 Classifications") + theme_classic() + coord_fixed(ratio = 1) + guides(shape = FALSE, size = FALSE) + 
  theme(plot.title = element_text(hjust = 0.5)) + scale_color_discrete(name = "Classification") + 
  scale_alpha_manual(aes(Alpha)) + xlab("Aggregate") + ylab("Per Capita")

