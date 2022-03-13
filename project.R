library("dplyr")
library("tidyr")
library("corrplot")
library("ggplot2")
library("GGally")

data1 = read.csv("output_file.csv")

data4 <- data1[sample(1:nrow(data1),12000,replace=FALSE),]
data4 = data4[-1]

data4$loan_status[data4$loan_status=="Fully Paid" | data4$loan_status=="Current"]<-1
data4$loan_status[data4$loan_status!=1]<-0

#data4$home_ownership[data4$home_ownership=="MORTGAGE"] <- -1
#data4$home_ownership[data4$home_ownership=="OWN"] <- 1
#data4$home_ownership[data4$home_ownership=="RENT"] <- 0

data4$emp_length[data4$emp_length=="10+ years"] <- 11
data4$emp_length[data4$emp_length=="9 years"] <- 9
data4$emp_length[data4$emp_length=="8 years"] <- 8
data4$emp_length[data4$emp_length=="7 years"] <- 7
data4$emp_length[data4$emp_length=="6 years"] <- 6
data4$emp_length[data4$emp_length=="5 years"] <- 5
data4$emp_length[data4$emp_length=="4 years"] <- 4
data4$emp_length[data4$emp_length=="3 years"] <- 3
data4$emp_length[data4$emp_length=="2 years"] <- 2
data4$emp_length[data4$emp_length=="1 year"] <- 1
data4$emp_length[data4$emp_length=="< 1 year"] <- 0

data4[data4==""]<- NA

sum(is.na(data4))

data4 = drop_na(data4)

sum(is.na(data4))
#data4 = drop_na(data6)
str(data4)
data_cleaned = data4


# E D A
#data sturucture
str(data_cleaned)



data_cleaned$loan_status = as.factor(data_cleaned$loan_status)
data_cleaned$loan_status = as.numeric(data_cleaned$loan_status)

data_cleaned$loan_status[data_cleaned$loan_status == 1]<-0
data_cleaned$loan_status[data_cleaned$loan_status == 2]<-1
#data_cleaned$loan_status = as.integer(data_cleaned$loan_status)
#data_cleaned$home_ownership = as.factor(data_cleaned$home_ownership)
#data_cleaned$home_ownership = as.numeric(data_cleaned$home_ownership)
data_cleaned$emp_length = as.factor(data_cleaned$emp_length)
data_cleaned$emp_length = as.numeric(data_cleaned$emp_length)

#assigning the priority to emp_length
data_cleaned$emp_length[data_cleaned$emp_length == 3] <- 11
data_cleaned$emp_length[data_cleaned$emp_length == 8] <- 6
data_cleaned$emp_length[data_cleaned$emp_length == 5] <- 3
data_cleaned$emp_length[data_cleaned$emp_length == 2] <- 1
data_cleaned$emp_length[data_cleaned$emp_length == 7] <- 5
data_cleaned$emp_length[data_cleaned$emp_length == 4] <- 2
data_cleaned$emp_length[data_cleaned$emp_length == 1] <- 0
data_cleaned$emp_length[data_cleaned$emp_length == 6] <- 4
data_cleaned$emp_length[data_cleaned$emp_length == 10] <- 8
data_cleaned$emp_length[data_cleaned$emp_length == 11] <- 9
data_cleaned$emp_length[data_cleaned$emp_length == 9] <- 7


data_cleaned$home_ownership[data_cleaned$home_ownership == "NONE" | data_cleaned$home_ownership == "ANY"]<-"OTHER"
# droping unwanted x column
#data_cleaned = data_cleaned[-1]

# Bharat Chhodo
# Correlation

cor(data_cleaned$loan_amnt, data_cleaned$fico_range_low, method = "pearson")
cor(data_cleaned$loan_amnt, data_cleaned$annual_inc, method = "pearson")

cor(data_cleaned$loan_status, data_cleaned$annual_inc, method = "pearson")
#cor(data_cleaned$loan_status, data_cleaned$fico_range_low, method = "pearson")
cor(data_cleaned$loan_status, data_cleaned$fico_range_high, method = "pearson")
cor(data_cleaned$loan_status, data_cleaned$loan_amnt)
#cor(data_cleaned$loan_status, data_cleaned$dti)
cor(data_cleaned$loan_status, data_cleaned$emp_length)

cor(data_cleaned$loan_status,data_cleaned$loan_amnt+data_cleaned$annual_inc+data_cleaned$fico_range_high+data_cleaned$emp_length)

data4$loan_status = as.factor(data4$loan_status)

chisq.test(data4$loan_status,data_cleaned$home_ownership)
chisq.test(data4$loan_status, data_cleaned$emp_title)
chisq.test(data4$loan_status, data_cleaned$purpose)

#str(data4)



#cor(data_cleaned$loan_status, data_cleaned$home_ownership, method = "pearson")
#cor(data_cleaned$loan_status, data_cleaned$dti, method = "pearson")


ggcorr(data_cleaned)

#Visualization

#print(ggplot(data_cleaned, aes(x=loan_status))+geom_bar()+facet_grid(.~dti)
#      +ggtitle("Loan Status by Gender of Applicant"))


plot(data4$loan_status, data_cleaned$loan_amnt)

ggplot(data_cleaned, aes(home_ownership))+ geom_bar(fill = "blue")+
        theme_bw()+ labs(title = "Bar Chart") + theme_gray()

ggplot(data_cleaned,aes(x = annual_inc, y = loan_amnt)) +
  geom_point(color= "steelblue") + geom_smooth(method = "lm")

ggplot(data_cleaned, aes(x = annual_inc, fill = home_ownership)) +
  geom_density(alpha = 0.4) +
  labs(title = "annual_inc by home_ownership")

#model

#logistic regression

set.seed(1234)

ind<-sample(2,nrow(data_cleaned),replace=T,prob=c(0.8,0.2))
train<-data_cleaned[ind == 1,]
test<-data_cleaned[ind == 2,]

s <- train$loan_status
la <- train$loan_amnt
a <- train$annual_inc
h <- train$home_ownership
f <- train$fico_range_high
#r <- train$purpose
e <- train$emp_title
el <- train$emp_length


model <- glm(s~la+a+h+f+el, data=train)
p1 <- predict(model,data.frame("la"=175000,"a"=160000,"h"=	
               'RENT',"f"=500,"el"=0,"e"='teacher'))
print(p1)

#accuracy

library(caret)
library(stringi)
library(e1071)
confusionMatrix(data4$loan_status, sample(data4$loan_status))






