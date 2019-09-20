setwd("D:/RLearning/NBACoachingTenure")
##Importing the years
  nbawin2015 <- read.csv("D:/Rlearning/NBACoachingTenure/NBAWins2015v1.csv", header = TRUE, stringsAsFactors=FALSE, row.names = NULL, sep=",")
  nbawin2016 <- read.csv("D:/Rlearning/NBACoachingTenure/NBAWins2016v1.csv", header = TRUE, stringsAsFactors=FALSE, row.names = NULL, sep=",")
  nbawin2017 <- read.csv("D:/Rlearning/NBACoachingTenure/NBAWins2017v1.csv", header = TRUE, stringsAsFactors=FALSE, row.names = NULL, sep=",")
  nbawin2018 <- read.csv("D:/Rlearning/NBACoachingTenure/NBAWins2018v1.csv", header = TRUE, stringsAsFactors=FALSE, row.names = NULL, sep=",")
  nbawin2019 <- read.csv("D:/Rlearning/NBACoachingTenure/NBAWins2019v1.csv", header = TRUE, stringsAsFactors=FALSE, row.names = NULL, sep=",")
##Fixing column names
 colnames(nbawin2016)[10] <- "Coach.Tenure"
 colnames(nbawin2018)[10] <- "Coach.Tenure"
 colnames(nbawin2019)[10] <- "Coach.Tenure"
 colnames(nbawin2017)[10] <- "Coach.Tenure"
 colnames(nbawin2017)[15] <- "NBAYear"
 colnames(nbawin2018)[15] <- "NBAYear"
 colnames(nbawin2019)[15] <- "NBAYear"
##Binding Data
 nbawin5.9 <- rbind(nbawin2015,nbawin2016,nbawin2017,nbawin2018,nbawin2019)
 nbawin5.9$TOVpercent <- nbawin5.9$TOV/100
##Installing packages (WIP)
 install.packages("plm")
 install.packages("Formula")
 install.packages("xlsx")
 install.packages("ggplot2")
 install.packages("ggrepel")
 install.packages("car")
 install.packages("carData")
 install.packages("")
 library("plm")
 library("Formula")
 library("xlsx")
 library("ggplot2")
 library("ggrepel")
 library(carData)
 library(car)
##Generating the ID Variable 
 names(nbawin5.9) [1] <-"Team"
 nbawin5.9$ID <- as.numeric(as.factor(nbawin5.9$Team))
 ##Reimporting the data set because I was having a problem with the regression commands having an error and i believe the problem was linked to Microsoft Excel doing something to the CSV
 nbadat6 <- nbawin5.9
 nbadat6temp <- read.delim("D:/Rlearning/nbadata6.txt", header = TRUE, sep = "\t")
 nbadat6$logCAP <- log(nbadat6$Total.Cap)
##Regression Analysis 
 pdata6 <- pdata.frame(nbadat6, index=c("ID","NBAYear"))
 summary(pdata6)
 poolingRTG <- plm(Wins ~ logCAP + Avg.Age + Coach.Tenure +OFFRTG + DEFRTG + TOV, data=pdata6, model= "pooling")
 vif(poolingRTG)
 randomlogRTG <- plm(Wins ~ logCAP + Avg.Age + Coach.Tenure +OFFRTG + DEFRTG +TOV, data=pdata6, model= "random")
 fixedlogRTG <- plm(Wins ~ logCAP + Avg.Age + Coach.Tenure +OFFRTG + DEFRTG + TOV, data=pdata6, model= "within")
 fixedlog <- plm(Wins ~ logCAP + Avg.Age + Coach.Tenure + TOV, data=pdata6, model= "within")
 randomlog <- plm(Wins ~ logCAP + Avg.Age + Coach.Tenure + TOV, data=pdata6, model= "random")
 plmtest(poolingRTG)
 pFtest(fixedlogRTG, poolingRTG)
 pFtest(fixedlogRTG, poolingRTG)
 phtest(randomlogRTG, fixedlogRTG)
 pdata6$predicted <- predict(fixedlogRTG)
 pdata6$residuals <- residuals(fixedlogRTG)
##Data visualizations
 ggplot(nbawin2019, aes(x=Coach.Tenure))  + geom_histogram(bins=15, color="darkblue", fill="lightblue") + geom_vline(aes(xintercept=mean(Coach.Tenure)), color="blue", linetype="dashed", size=1)
 ggplot(nbadat6, aes(x=Coach.Tenure, y=OFFRTG, colour=NBAYear, shape=NBAYear, label = paste("(",Team,",",NBAYear,")"))) + geom_point(size=4, shape=18)  + geom_label_repel(data = subset(nbadat6,Coach.Tenure>10), size=3) + scale_shape_identity()  + geom_label_repel(data = subset(nbadat6, OFFRTG<100), size = 3) + geom_vline(aes(yintercept=mean(OFFRTG)), color="blue", linetype="dashed", size=1)
 nbawin5.9sub <- subset(nbawin5.9, Coach.Tenure < 16)
 ggplot(nbawin5.9sub, aes(x=Coach.Tenure, y=OFFRTG, colour=NBAYear, shape=NBAYear)) + geom_point(size=4, shape=18)
 ## Animated scatterplot
 install.packages("gganimate")
 library(gganimate)
 scatteranim <- ggplot(nbadat6, aes(x=Coach.Tenure, y=OFFRTG, colour=NBAYear, shape=NBAYear, label = paste("(",Team,",",NBAYear,")"))) + geom_point(size=4, shape=21)  + geom_label_repel(data = subset(nbadat6,Coach.Tenure>10), size=3) + scale_shape_identity()  + geom_label_repel(data = subset(nbadat6, OFFRTG<100), size = 3) + geom_hline(aes(yintercept=mean(OFFRTG)), color="blue", linetype="dashed", size=1) + xlab("Coach Tenure (Years)") + ylab("Offensive Rating")
 hitchanim <- scatteranim + transition_time(NBAYear) + labs(title = "Year: {frame_time}") + ease_aes('cubic-in-out')
 animate(hitchanim, nframes = 125, end_pause = 30, height = 500, width = 600)
 scatteranim <- ggplot(nbadat6, aes(x=Coach.Tenure, y=OFFRTG, colour=NBAYear, shape=NBAYear, label = paste("(",Team,",",NBAYear,")"))) + geom_point(size=4, shape=21)  + geom_label_repel(data = subset(nbadat6,Coach.Tenure>10), size=3) + scale_shape_identity()  + geom_label_repel(data = subset(nbadat6, OFFRTG<100), size = 3) + xlab("Coach Tenure (Years)") + ylab("Offensive Rating") + scale_fill_teams(name = "Team") + scale_color_teams(name = "Team") + scale_x_continuous("Coach Tenure", limits = c(0,23)) + scale_y_continuous("Offensive Rating", limits = c(95, 115))
 ##Correlation Matrix
 nbadat6$Total.Cap <- as.numeric(nbadat6$Total.Cap)
 cor(nbadat6[c(3,6,11,12,14)])
 corrmat <- cor(nbadat6[c(3,6,11,12,14)])
 corrmat[,1] <- round(corrmat[,1], digits = 2)
 melted_corrmat <- melt(corrmat)
 upper_tri <- corrmat
 upper_tri[lower.tri(upper_tri)] <- NA
 melted_cormat <- melt(upper_tri, na.rm = TRUE)
 melted_cormat[,3] <- round(melted_cormat[,3], digits = 2)
 ggplot(data = melted_cormat, aes(Var2, Var1, fill=value)) + geom_tile(color = "white") + scale_fill_gradient2(low="blue", high="red", mid="white", midpoint = 0, limit = c(-1,1), space = "Lab", name="Dependent Variable\nCorrelation") + theme_minimal() + geom_text(aes(Var2,Var1, label = value), color = "black", size = 4)
##Output Regression tables 
 install.packages("stargazer")
 library(stargazer)
 stargazer(fixedlog,fixedlogRTG,randomlog,randomlogRTG, type="html", title="Regression Results", align=TRUE,covariate.labels=c("Log of Total Cap", "Average Age", "Coach Tenure", "Offensive Rating", "Defensive Rating", "Turnovers per 100"), style="aer", column.labels=c("Fixed", "FixedRTG", "Random", "RandomRTG"))
 
 