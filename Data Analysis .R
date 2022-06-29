# Βιβλιοθήκες
library(ggplot2)
library(tidyverse)
library(caret) 
library(neuralnet)
library(nnet)
library(lubridate)
library(dplyr)
library(readr)
library(e1071)
library(mlbench)
library(gginference)
library(foreign)
library(gginference)
library(DescTools)
library(usethis)
library(devtools)
library(DescriptiveStats.OBeu)
library(FSA)
library(foreign)
library(nortest)
library(ggpubr)
library(randtests)

# Ανάγνωση των δεδομένων
Churn <- read_csv("C:/Users/User/Downloads/Churn_Modelling.csv")
Churn = data.frame(Churn)
View(Churn)

#Καθαρισμός των δεδομένων
Churn<-na.omit(Churn)
Churn

# Πρώτες 6 εγγραφές και τις τελευταίες 6 των δεδομένων
head(Churn)
tail(Churn)


######## Περιγραφική στατιστική ################################################
######## Δημιουργία καινούργιου Dataset για την περιγραφική στατιστική

Churn2 <- Churn

######## Αλλάζουμε τιςμεταβλητές
str(Churn2)


Churn2$Exited = as.factor(Churn2$Exited)
Churn2$Geography = as.factor(Churn2$Geography)
Churn2$Gender = as.factor(Churn2$Gender)
Churn2$HasCrCard = as.factor(Churn2$HasCrCard)
Churn2$IsActiveMember = as.factor(Churn2$IsActiveMember)

###### Εισάγουμε ετικέτες στις ποιοτικές μεταβλητές
levels(Churn2$Gender) <- c( "male", "female")
levels(Churn2$IsActiveMember) <- c( "Not Active", "Active")
levels(Churn2$HasCrCard) <- c( "Without Credit Card", "With Credit Card")
levels(Churn2$Exited) <- c( "Stayed", "Left")


#########################Πίνακες Συχνοτήτων και διαγραμματα#####################
#### Has credit card
freq.Crc<-table(Churn2$HasCrCard)
freqcrc <-as.data.frame(freq.Crc)
colnames(freqcrc)<-c("Card","Frequency")
freqcrc

# Barplot

bar.freq.Crc<-barplot(freq.Crc,main="Country distribution", 
                      
                      xlab="Gender",ylab="Frequency",
                      
                      horiz=FALSE ,cex.names=0.8)

#### Gender
freq.gen<-table(Churn2$Gender)
freqgen <-as.data.frame(freq.gen)
colnames(freqgen)<-c("Gender","Frequency")
freqgen

# Barplot

bar.freq.gen<-barplot(freq.gen,main="Country distribution", 
                      
                      xlab="Gender",ylab="Frequency",
                      
                      horiz=FALSE ,cex.names=0.8)


#### Geography
freq.geo<-table(Churn2$Geography)
freqgeo <-as.data.frame(freq.geo)
colnames(freqgeo)<-c("Country","Frequency")
freqgeo

# Barplot

bar.freq.geo<-barplot(freq.geo,main="Country distribution", 
                      
                      xlab="Country",ylab="Frequency",
                      
                      horiz=FALSE ,cex.names=0.8)

#### Is Active
freq.act<-table(Churn2$IsActiveMember)
freqact <-as.data.frame(freq.act)
colnames(freqact)<-c("Activity","Frequency")
freqact

# Barplot
bar.freq.act<-barplot(freq.act,main="Activity distribution", 
                      
                      xlab="Active",ylab="Frequency",
                      
                      horiz=FALSE ,cex.names=0.8)
#### Exited


freq.exi<-table(Churn2$Exited)
freqexi<-as.data.frame(freq.exi)
colnames(freqexi)<-c("Exited","Frequency")
freqexi

## Barplot
bar.freq.exi<-barplot(freq.exi,main="Exit distribution", 
                      
                      xlab="Exit",ylab="Frequency",
                      
                      horiz=FALSE ,cex.names=0.8)



################ Χ τετράγωνο τεστ για την εξάρτηση των μεταβλητών###############
chi_test = chisq.test(Churn2$Exited,Churn2$IsActiveMember)
chi_test
# p-value μικρότερο της σ.σ. αρα εξαρτημένες

chi_test = chisq.test(Churn2$Exited,Churn2$HasCrCard)
chi_test
ggchisqtest(chi_test)


chi_test = chisq.test(Churn2$Exited,Churn2$NumOfProducts)
chi_test
ggchisqtest(chi_test)


chi_test = chisq.test(Churn2$Exited,Churn2$Gender)
chi_test
ggchisqtest(chi_test)


chi_test = chisq.test(Churn2$Exited,Churn2$Geography)
chi_test
ggchisqtest(chi_test)

######################### Ανάλυση ποσοτικών μεταβλητών #########################
Left <- subset(Churn2, Churn2$Exit =="Left")
Left
Stayed <- subset(Churn2, Churn2$Exit =="Stayed")
Stayed

France <- subset(Churn2, Churn2$Geography =="France")
Germany <- subset(Churn2, Churn2$Geography =="Germany")
Spain <- subset(Churn2, Churn2$Geography =="Spain")


# Μεση τιμή, διακύμανση και τυπική απόκλιση για την μεταβλητή Exit
# Μέση τιμή
mean0 <- mean(Left$Age)
mean2 <- mean(Left$Balance)
mean4 <- mean(Left$CreditScore)
mean6 <- mean(Left$EstimatedSalary)

# Διακύμανση/διασπορά
var0 <- var(Left$Age)
var2 <- var(Left$Balance)
var4 <- var(Left$CreditScore)
var6 <- var(Left$EstimatedSalary)

# Τυπική απόκλιση
sd0 <- sd(Left$Age)
sd2 <- sd(Left$Balance)
sd4 <- sd(Left$CreditScore)
sd6 <- sd(Left$EstimatedSalary)

# Μέση τιμή
mean1 <- mean(Stayed$Age)
mean3 <- mean(Stayed$Balance)
mean5 <- mean(Stayed$CreditScore)
mean7 <- mean(Stayed$EstimatedSalary)

# Διακύμανση/διασπορά
var1 <- var(Stayed$Age)
var3 <- var(Stayed$Balance)
var5 <- var(Stayed$CreditScore)
var7 <- var(Stayed$EstimatedSalary)


# Τυπική απόκλιση
sd1 <- sd(Stayed$Age)
sd3 <- sd(Stayed$Balance)
sd5 <- sd(Stayed$CreditScore)
sd7 <- sd(Stayed$EstimatedSalary)


Descriptives <- c("mean","var","sd")
Descriptives.Left <- c(mean0,var0,sd0)
Descriptives.Stayed <- c(mean1,var1,sd1)
Descriptives.Exit <- cbind(Descriptives,Descriptives.Left,Descriptives.Stayed)
Descriptives.Exit

Descriptives1 <- c("mean","var","sd")
Descriptives.Left1 <- c(mean2,var2,sd2)
Descriptives.Stayed1 <- c(mean3,var3,sd3)
Descriptives.Exit1 <- cbind(Descriptives1,Descriptives.Left1,Descriptives.Stayed1)
Descriptives.Exit1


Descriptives <- c("mean","var","sd")
Descriptives.Left <- c(mean4,var4,sd4)
Descriptives.Stayed <- c(mean5,var5,sd5)
Descriptives.Exit <- cbind(Descriptives,Descriptives.Left,Descriptives.Stayed)
Descriptives.Exit

Descriptives <- c("mean","var","sd")
Descriptives.Left <- c(mean6,var6,sd6)
Descriptives.Stayed <- c(mean7,var7,sd7)
Descriptives.Exit <- cbind(Descriptives,Descriptives.Left,Descriptives.Stayed)
Descriptives.Exit


ds.skewness(Left$Age)
ds.skewness(Stayed$Age)

#Age
Churn2$zscore_Age <- scale(Churn2$Age)
Churn2$zscore_Age

hist( 
  Churn2$Age,
  breaks = "scott", 
  main = "Histogram of Age", 
  xlab = "Age",
  col = "blue",
  freq = TRUE,
  labels = TRUE)

#Credit Score
Churn2$zscore_CreditScore <- scale(Churn2$CreditScore)
Churn2$zscore_CreditScore

hist( 
  Churn2$CreditScore,
  breaks = "scott", 
  main = "Histogram of Credit Score", 
  xlab = "Credit Score",
  col = "blue",
  freq = TRUE,
  labels = TRUE)

# z-scores και histogramm της μεταβλητής Credit Score
Churn2$zscore_EstimatedSalary <- scale(Churn2$EstimatedSalary)
Churn2$zscore_EstimatedSalary

hist( 
  Churn2$EstimatedSalary,
  breaks = "scott", 
  main = "Histogram of Estimated Salary", 
  xlab = "Estimated Salary",
  col = "blue",
  freq = TRUE,
  labels = TRUE)

# z-scores και histogramm της μεταβλητής Balance
Churn2$zscore_Balance <- scale(Churn2$Balance)
Churn2$zscore_Balance

hist( 
  Churn2$Balance,
  breaks = "scott", 
  main = "Histogram of Balance", 
  xlab = "Balance",
  col = "blue",
  freq = TRUE,
  labels = TRUE)


# Πίνακας συνάφειας συχνοτήτων
Country_gender.freq = table(Churn2$Geography,Churn2$Gender)
Country_gender.freq

Country_exited.freq = table(Churn2$Geography,Churn2$Exited)
Country_exited.freq

Active_exited.freq = table(Churn2$Exited,Churn2$IsActiveMember)
Active_exited.freq

CreditCr_exited.freq = table(Churn2$Exited,Churn2$HasCrCard)
CreditCr_exited.freq

Age_exited.freq = table(Churn2$Exited,Churn2$Age)
Age_exited.freq

Country_gender.freq2 = addmargins(Country_gender.freq)
Country_gender.freq2
Country_exited.freq2 = addmargins(Country_exited.freq)
Country_exited.freq2
CreditCr_exited.freq2 = addmargins(CreditCr_exited.freq)
CreditCr_exited.freq2
Active_exited.freq2 = addmargins(Active_exited.freq)
Active_exited.freq2
Age_exited.freq2 = addmargins(Age_exited.freq)
Age_exited.freq2

#Πίνακας συνάφειας σχετικών συχνοτήτων

CreditCr_exited.relfreq = prop.table(CreditCr_exited.freq)
CreditCr_exited.relfreq
CreditCr_exited.relfreq = round(CreditCr_exited.relfreq, 4)
CreditCr_exited.relfreq

Active_exited.relfreq = prop.table(Active_exited.freq)
Active_exited.relfreq
Active_exited.relfreq = round(Active_exited.relfreq, 4)
Active_exited.relfreq

Country_exited.relfreq = prop.table(Country_exited.freq)
Country_exited.relfreq
Country_exited.relfreq = round(Country_exited.relfreq, 4)
Country_exited.relfreq

# Ραβδόγραμμα συχνοτήτων

# Exited - Country
bar1 <- barplot(
  height = t(Country_exited.freq), 
  beside = TRUE,
  horiz = FALSE, 
  col = rainbow(length(levels(Churn2$Exited))),
  main = "Country - Exited", 
  xlab = "Country", 
  ylab = "Frequency", 
  ylim = c(0,5000),
  cex.names = 0.8,
  legend.text = levels(Churn2$Exited))
  
text( 
  x = bar1, 
  y = t(Country_exited.freq), 
  labels = t(Country_exited.freq), 
  pos = 3)


# Exited - Activity
bar2 <- barplot(
  height = t(Active_exited.freq), 
  beside = TRUE,
  horiz = FALSE, 
  col = rainbow(length(levels(Churn2$Exited))),
  main = "Activity - Exited", 
  xlab = "Activity", 
  ylab = "Frequency", 
  ylim = c(0,5000),
  cex.names = 0.8,
  legend.text = levels(Churn2$Exited))

text( 
  x = bar2 , 
  y = t(Active_exited.freq), 
  labels = t(Active_exited.freq), 
  pos = 3)

# Exited - Credit Card
bar3 <- barplot(
  height = t(CreditCr_exited.freq), 
  beside = TRUE,
  horiz = FALSE, 
  col = rainbow(length(levels(Churn2$Exited))),
  main = "Credit Card - Exited", 
  xlab = "Credit Card", 
  ylab = "Frequency", 
  ylim = c(0,6000),
  cex.names = 0.8,
  legend.text = levels(Churn2$Exited))

text( 
  x = bar2 , 
  y = t(CreditCr_exited.freq), 
  labels = t(CreditCr_exited.freq), 
  pos = 3)




# Σχεδιαγράμματα ποσοτικών - ποιοτικών μεταβλητών

boxplot(
  formula = Balance~Exited,
  data = Churn2,
  main = "Balance by Exit", 
  ylab = "Balance", 
  xlab = "Exit",
  col = rainbow(length(levels(Churn2$Balance))))

boxplot(
  formula = CreditScore~Exited,
  data = Churn2,
  main = "Credit Score by Exit", 
  ylab = "Credit Score", 
  xlab = "Exit",
  col = rainbow(length(levels(Churn2$CreditScore))))

boxplot(
  formula = Age~Exited,
  data = Churn2,
  main = "Age by Exit", 
  ylab = "Age", 
  xlab = "Exit",
  col = rainbow(length(levels(Churn2$Age))))

boxplot(
  formula = EstimatedSalary~Exited,
  data = Churn2,
  main = "Estimated Salary by Exit", 
  ylab = "Estimated Salary", 
  xlab = "Exit",
  col = rainbow(length(levels(Churn2$EstimatedSalary))))

# Θα κατασκευάσουμε το συγκριτικό ιστόγραμμα 

hist(
  formula = Balance~Exited,
  data=Churn2,
  breaks = "scott",
  xlab="Balance",
  col = "lightblue")

hist(
  formula = CreditScore~Exited,
  data=Churn2,
  breaks = "scott",
  xlab="Credit Score",
  col = "lightblue")

hist(
  formula = Age~Exited,
  data=Churn2,
  breaks = "scott",
  xlab="Age",
  col = "lightblue")

hist(
  formula = EstimatedSalary~Exited,
  data=Churn2,
  breaks = "scott",
  xlab="Estimated Salary",
  col = "lightblue")







################ 'Ελεγχοι υποθέσεων ############################################

#Ελεγχοι κανονικότητας Kolmogorov-Smirnov 
#Δεν μπορούμε να χρησιμοποίησουμε Shapiro Wilk διότι 
#ο πληθυσμός ειναι μεγάλος
normtest_CreditScore = lillie.test(Churn2$CreditScore)
normtest_CreditScore

normtest_Balance = lillie.test(Churn2$Balance)
normtest_Balance

normtest_EstimatedSalary = lillie.test(Churn2$EstimatedSalary)
normtest_EstimatedSalary

normtest_Age = lillie.test(Churn2$Age)
normtest_Age

# Οπτικοί έλεγχοι
ggqqplot(
  data = Churn2$EstimatedSalary,
  conf.int = TRUE, 
  conf.int.level = 0.95, 
  title = "Q-Q plot for Estimated Salary")

ggqqplot(
  data = Churn2$Balance,
  conf.int = TRUE, 
  conf.int.level = 0.95, 
  title = "Q-Q plot for Account Balance")

ggqqplot(
  data = Churn2$Age,
  conf.int = TRUE, 
  conf.int.level = 0.95, 
  title = "Q-Q plot for Age")

ggqqplot(
  data = Churn2$CreditScore,
  conf.int = TRUE, 
  conf.int.level = 0.95, 
  title = "Q-Q plot for CreditScore")

# test για την τυχαιότητα του δείγματος - κριτήριο ροών

Balance_randtest = runs.test(
  x = Churn2$Balance,
  alternative = "two.sided",
  threshold = median(Churn2$Balance))
Balance_randtest #Τυχαίο

CreditScore_randtest = runs.test(
  x = Churn2$CreditScore,
  alternative = "two.sided",
  threshold = median(Churn2$CreditScore))
CreditScore_randtest #Μη-Τυχαίο

# Διωνυμικό τεστ 
Gender_binomtest = binom.test(
  x = table(Churn2$Gender),
  n = length(Churn2$Gender), 
  p = 0.50)
Gender_binomtest

CreditC_binomtest = binom.test(
  x = table(Churn2$HasCrCard),
  n = length(Churn2$HasCrCard), 
  p = 0.50)
CreditC_binomtest

Activity_binomtest = binom.test(
  x = table(Churn2$IsActiveMember),
  n = length(Churn2$IsActiveMember), 
  p = 0.50)
Activity_binomtest

Exited_binomtest = binom.test(
  x = table(Churn2$Exited),
  n = length(Churn2$Exited), 
  p = 0.50)
Exited_binomtest
#p-value > a επομένως αποδέχομαι την μηδενική υπόθεση να είναι 50% οι left

# Ανεξαρτησία 2 μεταβλητών

chitest = chisq.test(Churn2$Exited, Churn2$Gender)
chitest

chitest = chisq.test(Churn2$Exited, Churn2$Geography)
chitest

chitest = chisq.test(Churn2$Exited, Churn2$NumOfProducts)
chitest

chitest = chisq.test(Churn2$Exited, Churn2$HasCrCard)
chitest

chitest = chisq.test(Churn2$Exited, Churn2$IsActiveMember)
chitest

# Για ανεξαρτητα δειγματα
# Κριτήριο Wilcoxon - Mann - Whitney μη παραμετρικός έλεγχος

Age_Exited_wilcoxtest = wilcox.test(
  x = Churn2[Churn2$Exited == "Left", "Age"],
  y = Churn2[Churn2$Exited == "Stayed", "Age"],
  alternative = "two.sided",
  paired = FALSE,
  conf.level = 0.95)

Age_Exited_wilcoxtest

Balance_Exited_wilcoxtest = wilcox.test(
  x = Churn2[Churn2$Exited == "Left", "Balance"],
  y = Churn2[Churn2$Exited == "Stayed", "Balance"],
  alternative = "two.sided",
  paired = FALSE,
  conf.level = 0.95)

Balance_Exited_wilcoxtest

Credit_Exited_wilcoxtest = wilcox.test(
  x = Churn2[Churn2$Exited == "Left", "CreditScore"],
  y = Churn2[Churn2$Exited == "Stayed", "CreditScore"],
  alternative = "two.sided",
  paired = FALSE,
  conf.level = 0.95)

Credit_Exited_wilcoxtest

Salary_Exited_wilcoxtest = wilcox.test(
  x = Churn2[Churn2$Exited == "Left", "EstimatedSalary"],
  y = Churn2[Churn2$Exited == "Stayed", "EstimatedSalary"],
  alternative = "two.sided",
  paired = FALSE,
  conf.level = 0.95)

Salary_Exited_wilcoxtest


# Kruskal - Wallis test για τον έλεγχο κατα πόσο τα δείγματα προέρχονται απο την ίδια κατανομή
kruskal.test(Age ~ Exited, data = Churn2)
kruskal.test(Balance~ Exited, data = Churn2)
kruskal.test(CreditScore ~ Exited, data = Churn2)
kruskal.test(EstimatedSalary ~ Exited, data = Churn2)

################ Γραμμική Παλινδρόμηση - Διάγραμμα Διασποράς ###################
#
Salary_Balance_lm = lm(
  formula = EstimatedSalary ~ Balance,
  data = Churn2)
Salary_Balance_lm

plot(
  x = Churn2$EstimatedSalary, 
  y = Churn2$Balance,
  xlab = "Estimated Salary",
  ylab = "Balance") 
abline( 
  a = Salary_Balance_lm , 
  col = "red")

cor(Churn2$EstimatedSalary, Churn2$Balance)


#
Credit_Salary_lm = lm(
  formula = EstimatedSalary ~ CreditScore,
  data = Churn2)
Credit_Salary_lm

plot( 
  x = Churn2$EstimatedSalary, 
  y = Churn2$CreditScore,
  xlab = "Estimated Salary",
  ylab = "Credit Score") 
abline( 
  a = Credit_Salary_lm , 
  col = "red")
cor(Churn2$EstimatedSalary, Churn2$CreditScore)

#
Age_Salary_lm = lm(
  formula = EstimatedSalary ~ Age,
  data = Churn2)
Age_Salary_lm

plot( 
  x = Churn2$EstimatedSalary, 
  y = Churn2$Age,
  xlab = "Estimated Salary",
  ylab = "Age") 
abline( 
  a = Age_Salary_lm , 
  col = "red")
cor(Churn2$EstimatedSalary, Churn2$Age)


#
Credit_Salary_lm = lm(
  formula = CreditScore ~ Age,
  data = Churn2)
Credit_Salary_lm

plot( 
  x = Churn2$CreditScore, 
  y = Churn2$Age,
  xlab = "Credit Score",
  ylab = "Age") 
abline( 
  a = Credit_Salary_lm , 
  col = "red")
cor(Churn2$CreditScore, Churn2$Age)

#
Credit_Balance_lm = lm(
  formula = CreditScore ~ Balance,
  data = Churn2)
Credit_Balance_lm

plot( 
  x = Churn2$CreditScore, 
  y = Churn2$Balance,
  xlab = "Credit Score",
  ylab = "Balance") 
abline( 
  a = Credit_Balance_lm , 
  col = "red")
cor(Churn2$CreditScore, Churn2$Balance)




### Οπτικοποιήσεις με ggplot

ggplot( Churn2, aes(
  x = Balance, 
  y = CreditScore,
  col = Exited)) + 
  geom_jitter(shape = 1)


ggplot( Churn2, aes(
  x = EstimatedSalary, 
  y = Balance,
  col = Exited)) + 
  geom_jitter(shape = 1)

ggplot( Churn2, aes(
  x = EstimatedSalary, 
  y = CreditScore,
  col = Exited)) + 
  geom_jitter(shape = 1) +
  scale_x_continuous("Estimated Salary", limits = c(0, 200000))



#Ραβδογραμμάτα

ggplot(Churn2, aes(Geography)) +
  geom_bar()

ggplot(Churn2, aes(Geography)) +
  geom_bar()








######### Δημιουργεία καινούργιου dataset για την μηχανική μάθηση ##############
# Αφαιρώ τις 3 πρώτες στήλες

Churn <- Churn[, c(4, 5, 6, 7, 9, 10, 11, 12, 13, 14)]
view(Churn)

Churn$Exited = as.factor(Churn$Exited)
Churn$Geography = as.factor(Churn$Geography)
Churn$Gender = as.factor(Churn$Gender)


Churn1 <- Churn[, c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)]
str(Churn1)

view(Churn1)
Churn1
Churn1 = as.data.frame(Churn1)

##############Διαχωρισμός dataset σε train και test#############

#######Το 80% δείγματός χρησιμοποιούμε για εκπαίδευση smp_size 

smp_size <- floor(0.8 * nrow(Churn1)) #h entolh floor takes a single numeric argument x and returns a numeric vector containing the largest integers not greater than the corresponding elements of x.
set.seed(1) #idio algori8mo tuxaias deigmatolhpsias
train_index <- sample(1:nrow(Churn1), size = smp_size)

#######Ορίστε το train data frame από το quakesD μόνο με τις 
####### γραμμές του διανύσματος train_index   
train <- Churn1[train_index, ]
train  

#######Ορίστε το test data frame από το quakesD εκτός από τις 
####### γραμμές του διανύσματος train_index   
test <- Churn1[-train_index, ]
test

##########################Δημιουργία SVM ###########


####### Ορίζω την μεταβλητή svm.model_1 ως την λίστα των αποτελεσμάτων 
####### του SVM, το όποιο προβλέπει την μεταβλητή Exited 
####### από όλες τις μεταβλητές του συνόλου δεδομένων train, χρησιμοποιώντας κόστος 10
####### και scale FALSE και γραμμικό πυρήνα

#Linear
svm.model_1<-svm(train$Exited~. , data=train, kernel="linear",cost=10,scale=FALSE) #metavlhth, dataset, kernel, cost, scale

#RBF 
svm.model_2 = svm(train$Exited~., data = train, kernel="radial", gamma = 1, cost=1)

#######Δείτε τα πρώτα στατιστικά του SVM που κάνατε
#Linear
summary(svm.model_1)

#RBF
summary(svm.model_2)

#######Οπτοικοποιήστε το αποτέλεσμα με την 
#######plot, μονο για τις μεταβλητες CreditScore (y) και την Age (x)
#Linear
plot(svm.model_1 , train, Age~CreditScore)

#RBF
plot(svm.model_2 , train, Age~CreditScore)


####### Βρείτε τις προβλέψεις της μεταβλητής Exited
## στο test data frame
#Linear
svm.pred_1<-predict(svm.model_1, test) 

#RBF
svm.pred_3 <- predict(svm.model_2, test)

####### Κατασκευή του confution matrix
#Linear
xtab<-table(predict=svm.pred_1 ,truth=test$Exited)
xtab

#RBF
xtab3<-table(predict=svm.pred_3 ,truth=test$Exited)
xtab3

####### Αξιολογώ το Μοντέλο χρησιμοποιώντας την βιβλιοθήκη caret

library(caret) 
#Linear
confusionMatrix(xtab)

#Radial
confusionMatrix(xtab3)

###### Βέλτιστο μοντέλο SVM, με 10 fold cross validation, 
###### το όποιο προβλέπει την μεταβλητή Exited από όλες τις
####### μεταβλητές του συνόλου δεδομένων train, εξετάζοντας το κόστος 
####### για τις τιμές 0.1 ,1 ,10 ,100 ,1000 και γραμμικό πυρήνα.

#Linear
tune.out=tune(svm , Exited~., data=train, kernel ="linear", ranges =list(cost=c(0.1 ,1 ,10 ,100 ,1000)))

#Radial
tune.out2=tune(svm , Exited~., data=train, kernel ="radial", ranges =list(cost=c(0.1 ,1 ,10 ,100 ,1000), gamma=c(0.5, 1, 2, 3, 4)))

###### Περισσότερες πληροφορίες για το αποτέλεσμα της προηγούμενης διαδικασίας
#Linear
summary(tune.out)

#RBF
summary(tune.out2)
###### Αποθήκευση του καλυτερου μοντέλου στην μεταβλητή bestmode
#Linear
bestmode <- tune.out$best.model
summary(bestmode)

#RBF
bestmodel2 <- tune.out2$best.model
summary(bestmodel2)


####### Βρίσκω τις προβλέψεις της μεταβλητής Exited στο test data frame χρησιμοποιώντας το καλύτερο μοντέλο
#Predictions for Linear Kernel
svm.pred_2<-predict(bestmode,test)

#Predictions for RBF Kernel
svm.pred_4<-predict(bestmodel2,test)

#######Κατασκευή του confusion matrix
#Confusion matrix for Linear Kernel
xtab2<-table(predict=svm.pred_2,truth=test$Exited)

#Confusion matrix for the RBF Kernel
xtab4<-table(predict=svm.pred_4,truth=test$Exited)

#######Αξιολόγηση του Μοντέλου χρησιμοποιώντας την βιβλιοθήκη caret
#Αξιολόγηση για το Linear Kernel
confusionMatrix(xtab2)

#Αξιολόγηση για το RBF Kernel
confusionMatrix(xtab4)


########################## Neural Network ##############################################
library(neuralnet)
library(nnet)


##Προσθέτω τις δύο μεταβλητές left και stayed
train$Left = train$Exited == 1
train$Stayed = train$Exited == 0

##Εκπαιδεύω το νευρωνικό δίκτυο με 3 επίπεδα

network= neuralnet(Left+Stayed~
                     CreditScore+Balance,
                   data=train, hidden=3)


## Παρατηρούμε το summary μέσω του result.matrix 

network$result.matrix

##Τα βάρη
head(network$generalized.weights)

##Οπτικοποίηση του εκπαιδευμένου νευρωνικού
plot(network)


## Πρόβλεψη των ετικετών βασισμένη στο νευρωνικό που υλοποιήσαμε
## και δημιουργούμε τον πίνακα πιθανοτήτων
##dataset, testset

net.predict=compute(network,test[-10])$net.result
net.predict

## Βρίσκω τις πιθανές ετικέτες με την μεγαλύτερη πιθανότηταα

net.prediction=c("CreditScore","Balance")[apply(net.predict,1,which.max)]
net.prediction
str(net.prediction)

## Δημιουργία ενός πίνακα ταξινόμησης 

predict.table=table(test$Exited,net.prediction)
predict.table

## Αλλάζω την μεταβλητή net.prediction σε factor
net.prediction = as.factor(net.prediction)
net.prediction

## Παρατηρούμε τα αποτελέσματα απο το confusionMatrix
library(caret)
confusionMatrix(predict.table)


