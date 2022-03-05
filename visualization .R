library(tidyverse)
df <-read_csv("IPL_trophy_over_years.csv")
pdf("visualization.pdf")

#boxplot

boxplot(df$`Win percentage` ~ df$`Number of trophies won`,data=df,varwidth = TRUE, main = "BOXPLOT  OF IPL OVER THE YEARS",xlab="Win percentage",ylab = "Number of trophies won",col="azure")
grid(nx=NULL,ny=NULL)
legend("topleft", c("Win percentage"), fill=c("black"))

#SCATTER PLOT

plot(df$`Win percentage`,df$`Number of trophies won`, main = "SCATTER PLOT OF IPL OVER YEARS", xlab="WIN PERCENTAGE",ylab="NUMBER OF TROPHIES WON",pch = 20)
x <- df$`Win percentage`
y <- df$`Number of trophies won`
model <- lm(y ~ x, data = df)
abline(model, col = "blue")
legend("topleft", c("Number of trophies won","straight line"), fill=c("black","blue"))

#HISTOGRAM

d<-read.csv("IPL_trophy_over_years.csv")
y<- d$Win.percentage
h<- hist(y,10,breaks = 5,main = "HISTOGRAM OF IPL OVER YEARS",xlab ="WIN PERCENTAGE",ylab ="FREQUENCY",col = "azure" )
legend("topleft", c("Frequency","normal curve overlay"), fill=c("azure","blue"))
mn<- mean(y)
stDt<- sd(y)
x<- seq(20,60,1)
y1<- dnorm(x,mean = mn,sd=stDt)
y1<- y1*diff(h$mids[1:2])*length(y)
lines(x,y1,col="blue",lwd=2)

dev.off()

