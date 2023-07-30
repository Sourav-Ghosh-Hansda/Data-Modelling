library(ggplot2)
library(psych)
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

