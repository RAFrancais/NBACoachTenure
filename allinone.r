
install.packages("XML")
install.packages("RCurl")
install.packages("plyr")
install.packages("ggplot2")
library(XML)
library(RCurl)
library(plyr)
library(ggplot2)
url <- "https://www.basketball-reference.com/leagues/NBA_2019_per_game.html"
# These produce the same results: url <- "https://stats.nba.com/leaders/"
urldata <-  getURL(url)
data <- readHTMLTable(urldata, stringsAsFactors = FALSE)
dat <- structure(data, row.names = c(NA, -734), .Names = seq_along(data), class = "data.frame")
#This one does not work
dat3 <- as.data.frame(data, col.names = names(data), headers = TRUE)
#This one works - But all variables are characteristic
stats18 <- ldply(data, data.frame)
# Adjusting variable names
colnames(stats18)[12] <- "FGpct"
colnames(stats18)[13] <- "3P"
colnames(stats18)[14] <- "3PA"
colnames(stats18)[15] <- "3Ppct"
colnames(stats18)[16] <- "2P"
colnames(stats18)[17] <- "2PA"
colnames(stats18)[18] <- "2Ppct"
colnames(stats18)[19] <- "eFG"
colnames(stats18)[22] <- "FTpct"
#Removing irrelevant variables, NAs, and converting to numeric
statsbind2$.id <- NULL
statsbind2$Rk <- NULL
statsbind2 <- as.data.frame(sapply(statsbind2[1:31], as.numeric))
statsbind2[is.na(statsbind2)] <- 0
#Isolating all traded players that show up as double in the data set 
TOTplayer <- subset(stats18, Tm == "TOT")
#Cleaning traded duplicates and adding in traded players totals
statsTraded <- subset(stats18, Player %in% c("Ryan Anderson", "Trevor Ariza", "Ron Baker", "Harrison Barnes", "Avery Bradley", "Corey Brewer", "Reggie Bullock", "Trey Burke", "Alec Burks", "Jimmy Butler", "Isiah Canaan", "Michael Carter-Williams", "Tyson Chandler", "Wilson Chandler", "Marquese Chriss", "Robert Covington", "Mitch Creek", "Sam Dekker", "Matthew Dellavedova", "Tyler Dorsey", "Henry Ellenson", "Wayne Ellington", "James Ennis", "Jawun Evans", "Kenneth Faried", "Tim Frazier", "Marc Gasol", "Pau Gasol", "JaMychal Green", "Tim Hardaway", "Tobias Harris", "Andrew Harrison", "George Hill", "Justin Holiday", "Rodney Hood", "Justin Jackson", "John Jenkins", "B.J. Johnson", "Stanley Johnson", "Tyler Johnson", "Wesley Johnson", "DeAndre Jordan","Enes Kanter", "Brandon Knight", "Kyle Korver", "Skal Labissiere", "Courtney Lee", "Jeremy Lin", "Timothe Luwawu-Cabarrot", "Shelvin Mack", "Thon Maker", "Boban Marjanovic", "Wesley Matthews", "Patrick McCaw", "C.J. Miles", "Nikola Mirotic", "Greg Monroe", "Eric Moreland", "Markieff Morris", "Mike Muscala", "Sviatoslav Mykhailiuk", "James Nunnally", "Kelly Oubre", "Jabari Parker", "Cameron Payne", "Otto Porter", "Bobby Portis", "Austin Rivers", "Dario Saric", "Mike Scott", "Wayme Selden", "Landry Shamet", "Iman Shumpert", "Jonathon Simmons", "Dennis Smith", "Jason Smith", "Ray Spalding", "Nik Stauskas", "Caleb Swanigan", "Garrett Temple", "Emanuel Terry", "Jonas Valanciunas", "Christian Wood", "Delon Wright", "Tyler Zeller", "Ivica Zubac"))
"%ni%" <- Negate("%in%")
statsnotTraded <- subset(stats18, Player %ni% c("Ryan Anderson", "Trevor Ariza", "Ron Baker", "Harrison Barnes", "Avery Bradley", "Corey Brewer", "Reggie Bullock", "Trey Burke", "Alec Burks", "Jimmy Butler", "Isiah Canaan", "Michael Carter-Williams", "Tyson Chandler", "Wilson Chandler", "Marquese Chriss", "Robert Covington", "Mitch Creek", "Sam Dekker", "Matthew Dellavedova", "Tyler Dorsey", "Henry Ellenson", "Wayne Ellington", "James Ennis", "Jawun Evans", "Kenneth Faried", "Tim Frazier", "Marc Gasol", "Pau Gasol", "JaMychal Green", "Tim Hardaway", "Tobias Harris", "Andrew Harrison", "George Hill", "Justin Holiday", "Rodney Hood", "Justin Jackson", "John Jenkins", "B.J. Johnson", "Stanley Johnson", "Tyler Johnson", "Wesley Johnson", "DeAndre Jordan","Enes Kanter", "Brandon Knight", "Kyle Korver", "Skal Labissiere", "Courtney Lee", "Jeremy Lin", "Timothe Luwawu-Cabarrot", "Shelvin Mack", "Thon Maker", "Boban Marjanovic", "Wesley Matthews", "Patrick McCaw", "C.J. Miles", "Nikola Mirotic", "Greg Monroe", "Eric Moreland", "Markieff Morris", "Mike Muscala", "Sviatoslav Mykhailiuk", "James Nunnally", "Kelly Oubre", "Jabari Parker", "Cameron Payne", "Otto Porter", "Bobby Portis", "Austin Rivers", "Dario Saric", "Mike Scott", "Wayme Selden", "Landry Shamet", "Iman Shumpert", "Jonathon Simmons", "Dennis Smith", "Jason Smith", "Ray Spalding", "Nik Stauskas", "Caleb Swanigan", "Garrett Temple", "Emanuel Terry", "Jonas Valanciunas", "Christian Wood", "Delon Wright", "Tyler Zeller", "Ivica Zubac"))
statsbind <- rbind(statsnotTraded, TOTplayer)
#Eliminating the extraneous "player" headers from the HTML Table
statsbind2 <- subset(statsbind, Player %ni% c("Player"))
#Calculating Z Scores
statsbind2$PTSZ <- (statsbind2$PTS - mean(statsbind2$PTS))/sd(statsbind2$PTS)
statsbindScale <- scale(statsbind2[,5:29])
statscbindScale <- cbind(statsbind2[c(1,2,4)], statsbindScale)
statscbindScale$NegTOV <- statscbindScale$TOV * -1
statscbindScale$NegPF <- statscbindScale$PF * -1
##Creating the all-in-one value
statscbindScale[,31] <- rowSums(statscbindScale[c(10,13,16,17,20,21,23,24,25,28,29,30)])
statscbindScale$combined_Z <- statscbindScale[,31]/12
##Data visualization
statscbindScale$abovebelow_Z <- ifelse(statscbindScale$combined_Z < 0, "below", "above")
ggplot(subset(statscbindScale), aes(x=Player, y=combined_Z, label="Combined Z Score")) + geom_bar(stat='identity', aes(fill=abovebelow_Z), width=.5) + scale_fill_manual(name="Mileage", labels=c("Above Average", "Below Average"), values=c("above"="#FF2E00", "below"="#0078FF")) +labs(title ="Normalized Production for the 2018-19 NBA Season") + coord_flip()