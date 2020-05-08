
(library(ggplot2))
(library(dplyr)) 
(library(plyr)) 
(library(forcats))
(library(viridis))
(library(wesanderson))

#####--------Visualization Part--------#####
attach(data)
str(data)
head(data, n=5)

#Run some descriptive stats

#Port of Entry
PORT<-rev(sort(table(Port.Name)))
par(mar = c(11, 5, 2, 2))  
barplot(PORT[1:10], las=2, col="navy", main="Port Name (Top 10)", ylab="Count")


#State
detach("package:plyr") #Otherwise the group_by funtion doesn't work properly
data$Value<-as.numeric(data$Value)
data %>% 
  group_by(State) %>%
  summarise(totalvalue=sum(Value))->TOTALSTATE
TOTALSTATE<-as.data.frame(TOTALSTATE)
TOTALSTATE
suppressPackageStartupMessages(library(plyr)) #Reload plyr

#State plot
ggplot(TOTALSTATE, aes(x=reorder(State, totalvalue), y=totalvalue)) + geom_col(fill="navy") + coord_flip() +
  ggtitle("States with Most Crossings") + xlab("State") + ylab("Count")

#Reattach main dataset
attach(data)

#Border
BORDER<-table(Border)
pie(BORDER, main="Border", col=c("blue", "green"))

#Extract Year and Month
data$YEAR<-format(as.Date(data$Date, format="%m/%d/%Y"), "%Y")
data$MONTH<-format(as.Date(data$Date, format="%m/%d/%Y"), "%m")
#Check
head(data, n=5)

#Measure- Conveyances, containers, passengers, pedestrians
MEASURE<-rev(sort(table(Measure)))
par(mar = c(10, 5, 2, 2))
barplot(MEASURE, las=2, col="navy", main="Measure", ylim=c(0, 35000), ylab=("Count"))

#Value-Count
summary(Value)
hist(Value, col="navy")
#Create buckets for Value: small, medium, and large
data$ValueGr<-cut(x=Value, breaks=c(-Inf, 90, 2483, Inf), labels=c("Small", "Medium", "Large"))
#Check
head(data,n=5)

#Condense the measure categories
data$MEASURE<-as.character(data$Measure)

data$MEASURE[data$MEASURE=="Bus Passengers"]<-"Bus Passengers"
data$MEASURE[data$MEASURE=="Buses"]<-"Bus Passengers"

data$MEASURE[data$MEASURE=="Personal Vehicles"]<-"Personal Vehicle Passengers"

data$MEASURE[data$MEASURE=="Rail Containers Empty"]<-"Train Passengers"
data$MEASURE[data$MEASURE=="Rail Containers Full"]<-"Train Passengers"
data$MEASURE[data$MEASURE=="Trains"]<-"Train Passengers"

data$MEASURE[data$MEASURE=="Truck Containers Empty"]<-"Truck Passengers"
data$MEASURE[data$MEASURE=="Truck Containers Full"]<-"Truck Passengers"
data$MEASURE[data$MEASURE=="Trucks"]<-"Truck Passengers"

data$MEASURE<-as.factor(data$MEASURE)

#Check
head(data, n=10)

#Border and measure
ggplot(data, aes(x=Border, fill=MEASURE)) + geom_bar(position="fill") +
  ggtitle("Border and Measure") + xlab("Border") + ylab("Proportion") 


#Those percentages look very evenly spread; let's see the numbers exactly
BM<-table(data$MEASURE, data$Border)
BM<-prop.table(BM, margin=2)*100
BM

#Value by year
ggplot(data, aes(x=YEAR, y=Value)) + stat_summary(fun="mean", geom="line", aes(group=Border, color=Border)) + 
  ggtitle("Border Crossings by Year") + ylab("Count") + xlab("Year") +
  theme(axis.text.x = element_text(angle = 60)) + 
  theme(legend.position="right") +
  scale_color_manual(values= c("blue", "green"))


#Value by month
ggplot(data, aes(x=MONTH, y=Value)) + stat_summary(fun="mean", geom="line", aes(group=Border, color=Border)) + 
  ggtitle("Border Crossings by Month") + ylab("Count") + xlab("Month") +
  theme(legend.position="right") + labs(colour="Border Crossing") +
  scale_color_manual(values= c("blue", "green"))


#Value Groups by year
ggplot(data, aes(x=YEAR, y=ValueGr)) + stat_summary(fun="sum", geom="line", aes(group=ValueGr, color=ValueGr)) + 
  ggtitle("Crossings by Year") + ylab("Count") + xlab("Year") +
  theme(axis.text.x = element_text(angle = 60)) + 
  theme(legend.position="right") + labs(colour="Value Groups") +
  scale_color_manual(values = wes_palette("Darjeeling1", type=c("discrete"), n=3))


#Value Groups by month
ggplot(data, aes(x=MONTH, y=ValueGr)) + stat_summary(fun="sum", geom="line", aes(group=ValueGr, color=ValueGr)) + 
  ggtitle("Crossings by Year") + ylab("Count") + xlab("Month") +
  theme(axis.text.x = element_text(angle = 60)) + 
  theme(legend.position="right") + labs(colour="Value Groups") +
  scale_color_manual(values = wes_palette("Darjeeling1", type=c("discrete"), n=3))

#Measure by year
ggplot(data, aes(x=YEAR, y=Value)) + stat_summary(fun="mean", geom="line", aes(group=MEASURE, color=MEASURE)) + 
  ggtitle("Measure by Year") + ylab("Count") + xlab("Month") +
  theme(axis.text.x = element_text(angle = 60)) + 
  theme(legend.position="right") 


#Measure by month
ggplot(data, aes(x=MONTH, y=Value)) + stat_summary(fun="mean", geom="line", aes(group=MEASURE, color=MEASURE)) + 
  ggtitle("Measure by Month") + ylab("Count") + xlab("Month") + 
  theme(legend.position="right")







#####--------Linear Regression Part--------#####

library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)
library(dplyr)
library(tidyverse)

brand <- read.csv("/MasterProgram/Stat Method/Project/Border_Crossing_Entry_Data 2.csv", header = T,stringsAsFactors = F)

brand <- na.omit(brand)
brand$Date <- as.POSIXlt(as.character(brand$Date), format="%m/%d/%Y %H:%M")
brand$Year <- year(brand$Date)
brand$Month <- month(brand$Date)

brand$Year <- as.factor(brand$Year)
brand$Month <- as.factor(brand$Month)

state_count <- brand %>%
  group_by(State,Year,Month) %>%
  summarize(total_value = sum(as.numeric(Value)))

state_separate <- brand%>%
  group_by(State)%>%
  summarize(total_value = sum(as.numeric(Value)))


avg_year_state <- state_count%>%
  group_by(State,Year)%>%
  summarize(year_value = mean(as.numeric(total_value)))

##overview
ggplot(data=avg_year_state, aes(x=Year, y=year_value, group=State,col = State)) +
  geom_line()+
  geom_point()


#####Build Model
#Alaska part
Alaska <- avg_year_state[avg_year_state$State=="Alaska",]
plot(lm(Alaska$year_value~Alaska$Year,))
##point24 should be droped  by cook distance
Alaska <- avg_year_state[1:23,]
#fit final lm model
ModAlaska <- lm(Alaska$year_value~Alaska$Year,)


#Arizona part
Arizona <- avg_year_state[avg_year_state$State=="Arizona",]
plot(lm(Arizona$year_value~Arizona$Year,))
##point1 should be droped by cook distance
Arizona <- Arizona[-1,]
#fit final lm model
ModArizona <- lm(Arizona$year_value~Arizona$Year,)



#California part
California <- avg_year_state[avg_year_state$State=="California",]
plot(lm(California$year_value~California$Year,))
#point should be droped by cook distance
California <- California[-1,]
#fit final lm model
ModCalifornia <- lm(California$year_value~California$Year,)



#Idaho part
Idaho <- avg_year_state[avg_year_state$State=="Idaho",]
plot(lm(Idaho$year_value~Idaho$Year,))
#point24 should be droped by cook distance
Idaho <- Idaho[-24,]
#fit final lm model
ModIdaho <- lm(Idaho$year_value~Idaho$Year,)



#Maine part
Maine <- avg_year_state[avg_year_state$State=="Maine",]
plot(lm(Maine$year_value~Maine$Year,))
#no point should be droped by cook distance
#fit final lm model
ModMaine <- lm(Maine$year_value~Maine$Year,)



#Michigan part
Michigan <- avg_year_state[avg_year_state$State=="Michigan",]
plot(lm(Michigan$year_value~Michigan$Year,))
#point5 should be droped by cook distance
Michigan <- Michigan[-5,]
#fit final lm model
ModMichigan <- lm(Michigan$year_value~Michigan$Year,)



#Minnesota part
Minnesota <- avg_year_state[avg_year_state$State=="Minnesota",]
plot(lm(Minnesota$year_value~Minnesota$Year,))
#point24 should be droped by cook distance
Minnesota <- Minnesota[-24,]
#fit final lm model
ModMinnesota <- lm(Minnesota$year_value~Minnesota$Year,)


#################################################################################
#Montana part
Montana <- avg_year_state[avg_year_state$State=="Montana",]
plot(lm(Montana$year_value~Montana$Year,))
summary(lm(Montana$year_value~Montana$Year,))
#point24 should be droped by cook distance
Montana <- Montana[-24,]
#fit final lm model
ModMontana <- lm(Montana$year_value~Montana$Year,)


#New Mexico part
NewMexico <- avg_year_state[avg_year_state$State=="New Mexico",]
plot(lm(NewMexico$year_value~NewMexico$Year,))
#point12 should be droped by cook distance
NewMexico <- NewMexico[-12,]
#fit final lm model
ModNewMexico <- lm(NewMexico$year_value~NewMexico$Year,)



#New York part
NewYork <- avg_year_state[avg_year_state$State=="New York",]
plot(lm(NewYork$year_value~NewYork$Year,))
#point24 should be droped by cook distance
NewYork <- NewYork[-24,]
#fit final lm model
ModNewYork <- lm(NewYork$year_value~NewYork$Year,)



#North Dakota part
NorthDakota <- avg_year_state[avg_year_state$State=="North Dakota",]
plot(lm(NorthDakota$year_value~NorthDakota$Year,))
#point24 should be droped by cook distance
NorthDakota <- NorthDakota[-24,]
#fit final lm model
ModNorthDakota <- lm(NorthDakota$year_value~NorthDakota$Year,)



#Ohio part
Ohio <- avg_year_state[avg_year_state$State=="Ohio",]
#Only one point in Ohio



#Texas part
Texas <- avg_year_state[avg_year_state$State=="Texas",]
plot(lm(Texas$year_value~Texas$Year,))
#point16 should be droped by cook distance
Texas <- Texas[-16,]
#fit final lm model
ModTexas <- lm(Texas$year_value~Texas$Year,)



#Vermont part
Vermont <- avg_year_state[avg_year_state$State=="Vermont",]
plot(lm(Vermont$year_value~Vermont$Year,))
#point24 should be droped by cook distance
Vermont <- Vermont[-24,]
#fit final lm model
ModVermont <- lm(Vermont$year_value~Vermont$Year,)



#Washington part
Washington <- avg_year_state[avg_year_state$State=="Washington",]
plot(lm(Washington$year_value~Washington$Year,))
#no point should be droped by cook distance
#fit final lm model
ModWashington <- lm(Washington$year_value~Washington$Year,)


##### Plot chaptor#######
par(mfrow=c(3,5))
plot(Alaska$Year,Alaska$year_value, main="Alaska model")
abline(ModAlaska, col = "#800080")
lines(lowess(Alaska$Year,Alaska$year_value),col ='red', lwd = 1, lty = 1)

plot(Arizona$Year,Arizona$year_value, main="Arizona model")
abline(ModArizona, col = "#0abab5")
lines(lowess(Arizona$Year, Arizona$year_value),col ='red', lwd = 1, lty = 1)

plot(California$Year,California$year_value, main="California model")
abline(ModCalifornia, col = "#0abab5")
lines(lowess(California$Year, California$year_value),col ='red', lwd = 1, lty = 1)

plot(Idaho$Year,Idaho$year_value, main="Idaho model")
abline(ModIdaho, col = "#800080")
lines(lowess(Idaho$Year, Idaho$year_value),col ='red', lwd = 1, lty = 1)

plot(Maine$Year,Maine$year_value, main="Maine model")
abline(ModMaine, col = "#0abab5")
lines(lowess(Maine$Year, Maine$year_value),col ='red', lwd = 1, lty = 1)

plot(Michigan$Year,Michigan$year_value, main="Michigan model")
abline(ModMichigan, col = "#0abab5")
lines(lowess(Michigan$Year, Michigan$year_value),col ='red', lwd = 1, lty = 1)

plot(Minnesota$Year,Minnesota$year_value, main="Minnesota model")
abline(ModMinnesota, col = "#0abab5")
lines(lowess(Minnesota$Year, Minnesota$year_value),col ='red', lwd = 1, lty = 1)

plot(Montana$Year,Montana$year_value, main="Montana model")
abline(ModMontana, col = "#800080")
lines(lowess(Montana$Year, Montana$year_value),col ='red', lwd = 1, lty = 1)

plot(NewMexico$Year,NewMexico$year_value, main="New Mexico model")
abline(ModNewMexico, col = "#800080")
lines(lowess(NewMexico$Year, NewMexico$year_value),col ='red', lwd = 1, lty = 1)

plot(NewYork$Year,NewYork$year_value, main="New York model")
abline(ModNewYork, col = "#0abab5")
lines(lowess(NewYork$Year, NewYork$year_value),col ='red', lwd = 1, lty = 1)

plot(NorthDakota$Year,NorthDakota$year_value, main="North Dakota model")
abline(ModNorthDakota, col = "#800080")
lines(lowess(NorthDakota$Year, NorthDakota$year_value),col ='red', lwd = 1, lty = 1)

plot(Ohio$Year,Ohio$year_value, main="Ohio model")

plot(Texas$Year,Texas$year_value, main="Texas model")
abline(ModTexas, col = "#0abab5")
lines(lowess(Texas$Year, Texas$year_value),col ='red', lwd = 1, lty = 1)

plot(Vermont$Year,Vermont$year_value, main="Vermont model")
abline(ModVermont, col = "#0abab5")
lines(lowess(Vermont$Year, Vermont$year_value),col ='red', lwd = 1, lty = 1)

plot(Washington$Year,Washington$year_value, main="Washington model")
abline(ModWashington, col = "red")
lines(lowess(Washington$Year, Washington$year_value),col ='red', lwd = 1, lty = 1)


##### Predict part

print("Estimated number of entry in Alaska 2020")
predict(ModAlaska, type = "response")[23]
print("Estimated number of entry in Arizona 2020")
predict(ModArizona, type = "response")[23]
print("Estimated number of entry in California 2020")
predict(ModCalifornia, type = "response")[23]
print("Estimated number of entry in Idaho 2020")
predict(ModIdaho, type = "response")[23]
print("Estimated number of entry in Maine 2020")
predict(ModMaine, type = "response")[24]
print("Estimated number of entry in Michigan 2020")
predict(ModMichigan, type = "response")[23]
print("Estimated number of entry in Minnesota 2020")
predict(ModMinnesota, type = "response")[23]
print("Estimated number of entry in Montana 2020")
predict(ModMontana, type = "response")[23]
print("Estimated number of entry in New Mexico 2020")
predict(ModNewMexico, type = "response")[23]
print("Estimated number of entry in New York 2020")
predict(ModNewYork, type = "response")[23]
print("Estimated number of entry in North Dakota 2020")
predict(ModNorthDakota, type = "response")[23]
print("We can not predict future value in Ohio")
print("Estimated number of entry in Texas 2020")
predict(ModTexas, type = "response")[23]
print("Estimated number of entry in Vermont 2020")
predict(ModVermont, type = "response")[23]
print("Estimated number of entry in Washington 2020")
predict(ModWashington, type = "response")[23]

### Analysis Part
a <- lm(Alaska$year_value~Alaska$Year,)
b <- lm(Alaska$year_value~Alaska$Year,)
summary(a)
summary(b)
Alaska <- avg_year_state[avg_year_state$State=="Alaska",]
plot(lm(Alaska$year_value~Alaska$Year,))
##point24 should be droped  by cook distance
Alaska1 <- avg_year_state[1:23,]
plot(x=Alaska$Year, Alaska$year_value)
plot(x=Alaska1$Year, Alaska1$year_value)



#####--------Categorical Regression Part--------#####

library(lubridate)
library(tidyverse)
library(ggplot2)
library(car)
library(effects)
library(stargazer)

border <- read.csv("/MasterProgram/Stat Method/Project/Border_Crossing_Entry_Data 2.csv",header = T)

#categorical regression data subset
cf <- border[,c(2,4,5,6,7)]

#Factory the data to be format
cf$Date <- as.POSIXlt(as.character(cf$Date), format="%m/%d/%Y %H:%M")
cf$Year <- year(cf$Date)
cf$Month <- month(cf$Date)

cf_copy <- na.omit(cf)
str(cf_copy)

#Build factors
cf_copy$Month= as.factor(cf_copy$Month)
cf_copy$Measure= as.factor(cf_copy$Measure)
cf_copy$Border= as.factor(cf_copy$Border)


###Find the influence of measure and border on value
##Build the main model and interaction model
#Create dummy codes
res <- model.matrix(~Measure,data = cf_copy)
head(res[,-1])

#Y= b0+b1*Border+b2*Measure
model1 <- lm(Value~ Border+Measure, data = cf_copy)
model1
anova(model1)
summary(model1)

#Y= b0+b1*Border*Month
model2 <- lm(Value~ Border*Measure, data = cf_copy)
model2
anova(model2)
summary(model2)

#Y=b0+b1*Border+b2*Measure+b3*Border*Measure
model3 <- lm(Value~ Border+Measure+Border*Measure, data = cf_copy)
model3
anova(model3)
summary(model3)

anova(model1,model2)

#Run interaction in the two categorical variables
Effect1 <- effect('Border*Measure', model2,se=TRUE)

Effect1 <- as.data.frame(Effect1)
head(Effect1)


#Plot the interaction plot
ggplot(Effect1,aes(x= Measure,y= fit, group= Border))+
  geom_line(size=1,aes(color=Border))+
  xlab("Month")+
  ylab("Fitted Value")+
  theme(axis.text.x = element_text(angle=55, hjust=1, vjust=1))


###Find the influence of meonth and border on value
#Create dummy codes
res2 <- model.matrix(~Month,data = cf_copy)
head(res2[,-1])

#Y= b0+b1*Border+b2*Measure
model4 <- lm(Value~Border+Month,data = cf_copy)
model4
anova(model4)
summary(model4)

#Y= b0+b1*Border*Month
model5 <- lm(Value~Month*Border,data = cf_copy)
model5
anova(model5)
summary(model5)

#Y= b0+b1*Border+b2*Month+b3*Month*Border
model6 <- lm(Value~Border+Month+Month*Border,data = cf_copy)
model6
anova(model6)
summary(model6)

anova(model4,model5)

#Run interaction
Effect2 <- effect('Month*Border',model5,se=TRUE)
Effect2 <- as.data.frame(Effect2)

#Plot the interaction
ggplot(Effect2,aes(x= Month,y= fit, group= Border))+
  geom_line(size=1,aes(color=Border))+
  xlab("Month")+
  ylab("Fitted Value")+
  theme(axis.text.x = element_text(angle=0, hjust=1, vjust=1))







