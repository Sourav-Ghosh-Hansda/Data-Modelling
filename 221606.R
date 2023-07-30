library(ggplot2)
library(psych)
myproject<-read.csv("C:/Users/Admin/OneDrive - University of Essex/MA335/Final project-20230509 2023-05-09 09_39_38/project data.csv",header = TRUE, sep=",")
colnames(myproject)
head(myproject)
tail(myproject)
nrow(myproject)
table(myproject$Group)
table(myproject$M.F)
myproject$M.F <- ifelse( myproject$M.F == "F", 0, 1) #Converted Male/Female into numerical value
myproject$M.F
num_na_values <- sum(is.na(myproject))
num_na_values
data_clean <- na.omit(myproject)  # Remove missing date
nrow(data_clean)
new_data_clean <- subset(data_clean, !(Group == "Converted"))  #Remove rows wit Group="Converted"
nrow(new_data_clean) #317
summary(new_data_clean)
table(new_data_clean$Group)
unique(new_data_clean$Group)

###########################Summary Statistics############

head(new_data_clean)
nrow(new_data_clean) #317
colnames(new_data_clean)
ncol(new_data_clean)
describe(new_data_clean[,2:10])


new_data_clean_D <- subset(new_data_clean, (Group == "Demented"))
head(new_data_clean_D)
nrow(new_data_clean_D) #127
colnames(new_data_clean_D)
ncol(new_data_clean_D)
describe(new_data_clean_D[,2:10])


new_data_clean_ND <- subset(new_data_clean, (Group == "Nondemented"))
head(new_data_clean_ND)
nrow(new_data_clean_ND) #190
colnames(new_data_clean_ND)
ncol(new_data_clean_ND)
describe(new_data_clean_ND[,2:10])



nrow(new_data_clean_D)+nrow(new_data_clean_ND)#317

ggplot(new_data_clean, aes(x = factor(Group), y = Age, fill = Group)) +
  geom_boxplot() +
  geom_jitter(aes(color = factor(M.F)), width = 0.2, alpha = 0.5) +
  labs(x = "Group", y = "Age", fill = "Group", color = "Gender") +
  scale_color_manual(values = c("blue", "red"), labels = c("Female", "Male")) +
  ggtitle("Box Plot: Age by Group and Gender")


#################Each independent variable relation with dependent variable###############

################Gender vs Alzeimer#######################

M<-subset(new_data_clean, M.F == 1)
nrow(M) #137
F<-subset(new_data_clean, M.F == 0)
nrow(F) #180
nrow(M)+nrow(F)#317

MD<- subset(new_data_clean, M.F == 1 & Group == 'Demented')
nrow(MD) #76
FD<- subset(new_data_clean, M.F == 0 & Group == 'Demented')
nrow(FD) #51
MND<- subset(new_data_clean, M.F == 1 & Group == 'Nondemented')
nrow(MND) #61
FND<- subset(new_data_clean, M.F == 0 & Group == 'Nondemented')
nrow(FND) #129

nrow(MD)+nrow(FD)+nrow(MND)+nrow(FND)#317

proportionMales<-round(76/137,2) #55% Males
proportionMales

proportionfemales<-round(51/180,2) #28% Females
proportionfemales


gender <- c("Male", "Female", "Male", "Female")
demented <- c("Non-Demented", "Non-Demented", "Demented", "Demented")
count <- c(61,129,76,51)

df <- data.frame(gender, demented, count)

ggplot(df, aes(x = gender, y = count, fill = demented)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Gender", y = "Count", fill = "Alzheimer") +
  scale_fill_manual(values = c("red", "springgreen2"))  

##########################Age vs Alzheimer#######################
sex <- ifelse(new_data_clean$M.F == 1, "Male", "Female")

library(plyr)
mu <- ddply(new_data_clean, "Group", summarise, grp.mean=round(mean(Age),2))
mu #Demented Mean Age is 76.20
   #NonDemented Mean Age is 77.06

p<-ggplot(new_data_clean, aes(x=Age, color=sex))+
  geom_histogram(fill="white")+
  facet_grid(Group ~ .)
p

# Add mean lines
p+geom_vline(data=mu, aes(xintercept=grp.mean),linetype="dashed")


########################Education vs Alzemier#################

nu <- ddply(new_data_clean, "Group", summarise, grp.mean=round(mean(EDUC),2))
nu #Demented Mean EDUC is 13.83
#NonDemented Mean EDUC is 15.14

q<-ggplot(new_data_clean, aes(x=EDUC))+
  geom_histogram(color="blue", fill="white")+
  facet_grid(Group ~ .)
q

# Add mean lines
q+geom_vline(data=nu, aes(xintercept=grp.mean),linetype="dashed")


########################Socio Economic Status vs Alzeimer###############

SEconomicStatus<-as.factor(new_data_clean$SES)
ggplot(new_data_clean) + 
  geom_bar(mapping = aes(x = SES,fill=SEconomicStatus)) +
  labs(x = "Socio Economic Status")


ggplot(new_data_clean, aes(x = factor(SES), fill = factor(Group))) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("red", "lightskyblue")) +
  labs(x = "Socioeconomic Status", y = "Proportion", fill = "Alzheimer") +
  theme_bw()

#######################Mini Mental State Examination vs Alzeimer################

new_data_clean$MMSE
count(new_data_clean$MMSE)

wu <- ddply(new_data_clean, "Group", summarise, grp.mean=round(mean(MMSE),2))
wu #Demented Mean MMSE is 24.32
#NonDemented Mean MMSE is 29.23

g<-ggplot(new_data_clean, aes(x=MMSE))+
  geom_histogram(color="green", fill="white")+
  facet_grid(Group ~ .)
g

# Add mean lines
g+geom_vline(data=wu, aes(xintercept=grp.mean),linetype="dashed")

######################Clinical Dimentia Rating vs Alzeimer################

new_data_clean$CDR

yu <- ddply(new_data_clean, "Group", summarise, grp.mean=round(mean(CDR),2))
yu #Demented Mean CDR is 0.67
#NonDemented Mean CDR is 0.01
count(new_data_clean$CDR)

f<-ggplot(new_data_clean, aes(x=CDR))+
  geom_histogram(color="orange", fill="white")+
  facet_grid(Group ~ .)
f

####################Estimated total intracranial volume vs Alzeimer#################

new_data_clean$eTIV
plot(new_data_clean$eTIV)


ru <- ddply(new_data_clean, "Group", summarise, grp.mean=round(mean(eTIV),2))
ru #Demented Mean eTIV is 1490.7
#NonDemented Mean eTIV is 1495.5
count(new_data_clean$CDR)

s<-ggplot(new_data_clean, aes(x=eTIV))+
  geom_histogram(color="pink4", fill="white")+
  facet_grid(Group ~ .)
s

s+geom_vline(data=ru, aes(xintercept=grp.mean),linetype="dashed")

################################Normalize whole brain volume vs Alzeimer#################

new_data_clean$nWBV
plot(new_data_clean$nWBV)


pu <- ddply(new_data_clean, "Group", summarise, grp.mean=round(mean(nWBV),2))
pu #Demented Mean nWBV is 0.72
#NonDemented Mean nWBV is 0.74

r<-ggplot(new_data_clean, aes(x=nWBV))+
  geom_histogram(color="yellowgreen", fill="white")+
  facet_grid(Group ~ .)
r

r+geom_vline(data=pu, aes(xintercept=grp.mean),linetype="dashed")


#############################Atlas scaling factor VS Alzeimer#################


new_data_clean$ASF
plot(new_data_clean$ASF)


du <- ddply(new_data_clean, "Group", summarise, grp.mean=round(mean(ASF),2))
du #Demented Mean ASF is 1.19
#NonDemented Mean ASF is 1.19

t<-ggplot(new_data_clean, aes(x=ASF))+
  geom_histogram(color="sienna3", fill="white")+
  facet_grid(Group ~ .)
t

t+geom_vline(data=du, aes(xintercept=grp.mean),linetype="dashed")


############Correlation Matrix######################
library(ggcorrplot)

corr <- round(cor(new_data_clean[,2:10]), 1)
head(corr)

ggcorrplot(corr, hc.order = TRUE, type = "lower",
           outline.col = "white")

#####################Clustering Algorithm###################

##################K mean Clustering##################

z <-new_data_clean[,-c(1,1)]
head(z)
m<-apply(z,2,mean)
m
s<-apply(z,2,sd)
s
nor<-scale(z,center=m,scale =s)
head(nor)

# Calculate distance matrix  
distance <- dist(nor)
print(distance, digits =2)


# Scree Plot
wss <- (nrow(nor)-1)*sum(apply(nor,2,var))
for (i in 2:20) wss[i] <- sum(kmeans(nor, centers=i)$withinss)
plot(1:20, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares") 

# K-means clustering
set.seed(1234)
kc1<-kmeans(nor,2)
kc1

kc2<-kmeans(nor,3)
kc2

kc3<-kmeans(nor,4)
kc3

library("cluster")
clusplot(new_data_clean,kc1$cluster,
         color=T,shade= T,
         labels=2, lines=0)

clusplot(new_data_clean,kc2$cluster,
         color=T,shade= T,
         labels=2, lines=0)

#######################Logistic Regression####################


str(new_data_clean)
head(new_data_clean)
head(new_data_clean$Group)
tail(new_data_clean$Group)
group <- ifelse(new_data_clean$Group== "Nondemented", 0, 1)
head(group)
tail(group)

logisticdata<-data.frame(group,new_data_clean[,-1])
head(logisticdata)
logisticdata$SES<-as.factor(logisticdata$SES)
head(logisticdata$SES)
logisticdata$Age<-log(logisticdata$Age)
head(logisticdata$Age)
logisticdata$EDUC<-log(logisticdata$EDUC)
head(logisticdata$EDUC)
logisticdata$MMSE<-log(logisticdata$MMSE)
head(logisticdata$MMSE)
logisticdata$eTIV<-log(logisticdata$eTIV)
head(logisticdata$eTIV)
logisticdata$M.F<-as.factor(logisticdata$M.F)
head(logisticdata$M.F)

head(logisticdata)
str(logisticdata)


# Partition data - train (80%) & test (20%)
set.seed(1234)
ind <- sample(2, nrow(logisticdata), replace = T, prob = c(0.8, 0.2))
head(ind)
train <- logisticdata[ind==1,]
head(train)
test <- logisticdata[ind==2,]
head(test)

# Logistic regression model
mymodel <- glm(group ~ Age + EDUC + MMSE + nWBV + SES , data = train, family = "binomial")
summary(mymodel)

# Prediction
p1 <- predict(mymodel, train, type = 'response')
head(p1)
tail(p1)
head(train)
tail(train)

# Misclassification error - train data
pred1 <- ifelse(p1>0.5, 1, 0)
tab1 <- table(Predicted = pred1, Actual = train$group)
tab1
1 - sum(diag(tab1))/sum(tab1)

# Misclassification error - test data
p2 <- predict(mymodel, test, type = 'response')
pred2 <- ifelse(p2>0.5, 1, 0)
tab2 <- table(Predicted = pred2, Actual = test$group)
tab2
1 - sum(diag(tab2))/sum(tab2)

#############################Stepwise Forward Selection#################

#define intercept-only model
intercept_only <- lm(group ~ 1, data=logisticdata)

#define model with all predictors
all <- lm(group ~ ., data=logisticdata)

#perform forward stepwise regression
forward <- step(intercept_only, direction='forward', scope=formula(all), trace=1)
forward

#view results of forward stepwise regression
forward$anova

#view final model
round(forward$coefficients,2)

