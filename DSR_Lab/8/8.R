library(e1071) 

sample <- read.table("C:/Users/Downloads/DSR_Lab/sample1.csv",header=TRUE,sep=",")
# define the data frames for the NB classifier
traindata <- as.data.frame(sample[1:14,])
testdata <- as.data.frame(sample[15,])
traindata
testdata

tprior <- table(traindata$Enrolls)
tprior
tprior <- tprior/sum(tprior)
tprior

ageCounts <- table(traindata[,c("Enrolls", "Age")])
ageCounts

ageCounts <- ageCounts/rowSums(ageCounts)
ageCounts

incomeCounts <- table(traindata[,c("Enrolls", "Income")])
incomeCounts <- incomeCounts/rowSums(incomeCounts)
incomeCounts

jsCounts <- table(traindata[,c("Enrolls", "JobSatisfaction")])
jsCounts <- jsCounts/rowSums(jsCounts)
jsCounts

desireCounts <- table(traindata[,c("Enrolls", "Desire")])
desireCounts <- desireCounts/rowSums(desireCounts)
desireCounts

prob_yes <-
  ageCounts["Yes",testdata[,c("Age")]]*
  incomeCounts["Yes",testdata[,c("Income")]]*
  jsCounts["Yes",testdata[,c("JobSatisfaction")]]*
  desireCounts["Yes",testdata[,c("Desire")]]*
  tprior["Yes"]

prob_no <-
  ageCounts["No",testdata[,c("Age")]]*
  incomeCounts["No",testdata[,c("Income")]]*
  jsCounts["No",testdata[,c("JobSatisfaction")]]*
  desireCounts["No",testdata[,c("Desire")]]*
  tprior["No"]

prob_yes
prob_no
max(prob_yes,prob_no)

model <- naiveBayes(Enrolls ~ Age+Income+JobSatisfaction+Desire,
                    traindata)

model

results <- predict (model,testdata)
results

# use the NB classifier with Laplace smoothing
model1 = naiveBayes(Enrolls ~., traindata, laplace=.01)
model1

# predict with testdata
results1 <- predict (model1,testdata)
results1

