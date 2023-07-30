library(ggplot2)
myproject<-read.csv("C:\\Users\\Asus_owner\\Desktop\\Project MA335\\project data.csv",header = TRUE, sep=",")
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

