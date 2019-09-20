##Importing the years
  nbawin2015 <- read.csv("D:/Rlearning/NBAWins2015CSV.csv")
  nbawin2018 <- read.csv("D:/Rlearning/NBAWins2018v1.csv")
  nbawin2019 <- read.csv("D:/Rlearning/NBAWins2019.csv")
  nbawin2017 <- read.csv("D:/Rlearning/NBAWins2017.csv")
  nbawin2016 <- read.csv("D:/Rlearning/NBAWins2016.csv")
##Fixing column names
 colnames(nbawin2016)[10] <- "Coach.Tenure"
 colnames(nbawin2018)[10] <- "Coach.Tenure"
 colnames(nbawin2019)[10] <- "Coach.Tenure"
 colnames(nbawin2017)[10] <- "Coach.Tenure"
##Binding Data
 nbawin5.9 <- rbind(nbawin2015,nbawin2016,nbawin2017,nbawin2018,nbawin2019)
##Installing packages (WIP)
 install.packages("plm")
 install.packages("Formula")
 install.packages("xlsx")
 install.packages("broom")
 install.packages("huxtable")
 install.packages("ggplot2")
 library("plm")
 library("Formula")
 library("xlsx")
 library("broom")
 library("huxtable")
 library("ggplot2")
##This is me reimporting the dataset with the IDs and East/West Dummies i generated in excel because I couldn't figure it out in R
 write.xlsx(nbawin5.9, "D:/Rlearning/nbawin5.9.xlsx")
 nbawin5.9 <- read.csv("D:/Rlearning/nbawin5.9.csv")
 colnames(nbawin5.9)[16] <- "ID"
 names(nbawin5.9) [1] <-"Team"
##Panel Data Analysis 
 pdata <- pdata.frame(nbawin5.9, index=c("ID","Year"))
 summary(pdata)
 poolingRTG <- plm(Wins ~ Total.Cap + Avg.Age + Coach.Tenure +OFFRTG + DEFRTG + TOV%, data=pdata, model= "pooling")
 fixedRTG <- plm(Wins ~ Total.Cap + Avg.Age + Coach.Tenure +OFFRTG + DEFRTG, data=pdata, model= "within")
 randomRTG <- plm(Wins ~ Total.Cap + Avg.Age + Coach.Tenure +OFFRTG + DEFRTG, +TOV., data=pdata, model= "random")
 random <- plm(Wins ~ Total.Cap + Avg.Age + Coach.Tenure, data=pdata, model= "random")
 plmtest(poolingRTG)
 pFtest(fixedRTG, poolingRTG)
 pFtest(fixedRTG, poolingRTG)
 phtest(randomRTG, fixedRTG)
 pdata$predicted <- predict(randomRTG)
 pdata$residuals <- residuals(randomRTG)
##Data visualizations
 ggplot(nbawin2019, aes(x=Coach.Tenure)) + geom_histogram(bins=15, color="darkblue", fill="lightblue") + geom_vline(aes(xintercept=mean(Coach.Tenure)), color="blue", linetype="dashed", size=1)
 ggplot(nbawin5.9, aes(x=Coach.Tenure, y=OFFRTG, colour=Year, shape=Year)) + geom_point(size=4, shape=18)
 nbawin5.9sub <- subset(nbawin5.9, Coach.Tenure < 16)
 ggplot(nbawin5.9sub, aes(x=Coach.Tenure, y=OFFRTG, colour=Year, shape=Year)) + geom_point(size=4, shape=18)
 
 ##Animated histogram?
 
 