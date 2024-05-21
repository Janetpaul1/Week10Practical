salary_data <- read.csv("salary.csv", na = "")
str(salary_data)
View(salary_data)
summary(salary_data)
data1<-salary_data[c(2,3,4,5)]
data1
colnames(data1)[colnames(data1)=="number,of.subjects"]<-"Num_subjects"
windows(20,10)
pairs(salary_data)
#install.packages("psych")
library(psych)
pairs.panels(data1,
             smooth = FALSE,      # If TRUE, draws loess smooths
             scale = FALSE,      # If TRUE, scales the correlation text font
             density = TRUE,     # If TRUE, adds density plots and histograms
             ellipses = FALSE,    # If TRUE, draws ellipses
             method = "pearson",# Correlation method (also "pearson" or "kendall")
             pch = 21,           # pch symbol
             lm = FALSE,         # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE,         # If TRUE, reports correlations
             jiggle = FALSE,     # If TRUE, data points are jittered
             factor = 2,         # Jittering factor
             hist.col = 4,       # Histograms color
             stars = TRUE,       # If TRUE, adds significance level with stars
             ci = TRUE) 

windows(20,10)
par(mfrow=c(4,2))


scatter.smooth(x=salary_data$CaseNum,
               y=salary_data$Salary,
               main = "Scatter Plot of CaseNum vs. Salary",
               xlab = "CaseNum",
               ylab = "Salary")
scatter.smooth(x=salary_data$Years,
               y=salary_data$Salary,
               main = "Scatter Plot of Years vs. Salary",
               xlab = "Years",
               ylab = "Salary")
scatter.smooth(x=salary_data$Rating,
               y=salary_data$Salary,
               main = "Scatter Plot of Rating vs. Salary",
               xlab = "Rating",
               ylab = "Salary")               
scatter.smooth(x=salary_data$Number.of.Subjects,
               y=salary_data$Salary,
               main = "Scatter Plot of No of sub vs. Salary",
               xlab = "no of sub",
               ylab = "Salary")
correlation_matrix<-cor(data1)
windows(20,16)
corPlot(correlation_matrix)
attach(salary_data)
model <- lm(Salary ~ Years + 
              Rating + 
              Number.of.Subjects+
              CaseNum,
            data = salary_data)
model
summary(model)
#ttest
shapiro.test(residuals(model))
t.test(residuals(model),mu=0)
#multicoliniarity
install.packages("faraway")
library(faraway)
v1<-vif(model)
v1


#regression model2
model2<-lm (Salary~
             Years+
             Rating)
model2
summary(model2)
AIC(model)
BIC(model)
AIC(model2)
BIC(model2)
