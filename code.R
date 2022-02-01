#Prashant Kumar
#____________________________________________________________________________________

library(readxl) #Using the library
Data_cholestrol <- read_excel("dataset.xlsx")
N <- length(Data_cholestrol$Subject) #total number of data
LDL <- c(Data_cholestrol$PE,Data_cholestrol$AE,Data_cholestrol$BE,Data_cholestrol$PD,Data_cholestrol$AD,Data_cholestrol$BD)#Assigning all the group data to vector LDL
Factor_lifestyle <- c(rep("Exercise",3*N), rep("Diet",3*N))

Drugs <- c(rep("Placebo",N),rep("DrugA",N),rep("DrugB",N),rep("Placebo",N),rep("DrugA",N),rep("DrugB",N))#Group
Data <- data.frame(Drugs,Factor_lifestyle,LDL) #Using data.frame to adjust and form Nx3 data set
colnames(Data) <- c("Drugs","Factors(lifestyle)","LDL[mg/L]") #Renaming the columns
print(Data)
#________________________________________________________________________________________
#TWO WAY ANOVA
#Anova test
Anova <- aov(LDL~Drugs + Factor_lifestyle,data = Data)
print(summary(Anova))
#________________________________________________________________________________________
#Visualization
library(ggpubr)
library(ggplot2)
#Box plot
boxplt <-ggboxplot(Data, x = "Drugs", y = "LDL[mg/L]", color = "Factors(lifestyle)",
                   palette = c("#00AFBB", "#E7B800"))
boxplt
#Interaction plot
plot_inter <- ggline(Data, x = "Drugs", y = "LDL[mg/L]", color = "Factors(lifestyle)",
                     add = c("mean_se", "dotplot"),
                     palette = c("#00AFBB", "#E7B800"))
plot_inter
#______________________________________________________________________________________
#post hoc test
#conducting pairwise t test
test_pairwise <- pairwise.t.test(Data$`LDL[mg/L]`, Data$Drugs,
                                 p.adjust.method = "BH")
test_pairwise
#__________________________________________________________________________________________
#____________________________________________________________________________________________
#____________________________________________________________________________________________
#Linear Regression
library(readxl)
Data_reg <- read_excel("dataset.xlsx", 
                       sheet = "Linear regression")
#Mean
cat("Mean of Explanatory variable is",mean(Data_reg$x))

cat("Mean of Dependent variable is",mean(Data_reg$y))

#Variance
cat("Variance of Explanatory variable is",var(Data_reg$x))

cat("Variance of Dependent variable is",var(Data_reg$y))
#Scatterplot
library(ggplot2)
plt1 <- ggplot() +
  geom_point(aes(x = Data_reg$x, y = Data_reg$y))+
  theme_bw()+
  labs( x = "Weight[mg]",y = "LDL[mg/L]")+
  ggtitle("Scatter plot b/w LDL[mg/L] and Weight[mg]")+
  theme( axis.text = element_text( size =12) ,
         axis.title = element_text( size =12) ,
         plot.title = element_text( hjust = 0.5))
plt1
#__________________________________________________________________________________________
#Using glm command to analyse the standard linear model
model <- glm(Data_reg$y ~ Data_reg$x, data = Data_reg)
summary(model)

#_________________________________________________________________________________________
#Calculating the slope m and intercept b
b = model$coefficients[1]
m = model$coefficients[2]
m
b
#For calculating the confidence interval of b and m we have to first compute standord errors
#Computation of Standard error of Intercept i.e. SEb
SEb <- summary(model)$coefficients[,2][1]
#Computation of standard error of slope(m) i.e. SEm
SEm <- summary(model)$coefficients[,2][2]
SEb
SEm
#Assigning predicted values as regression
regression <- (m*Data_reg$x) + b

#Computation of Confidence interval
#For slope
CIm_max <- m + qt(0.985,(length(Data_reg$`Subject ID`)-2))*SEm
CIm_min <- m - qt(0.985,(length(Data_reg$`Subject ID`)-2))*SEm
#For Intercept
CIb_max <- b + qt(0.985,(length(Data_reg$`Subject ID`)-2))*SEb
CIb_min <- b - qt(0.985,(length(Data_reg$`Subject ID`)-2))*SEb
#printing the confidence interval for slope and Intercept
CIm_max
CIm_min
CIb_max
CIb_min
#___________________________________________________________________________________________
library(ggplot2)
library(ggpubr)

#Regression maximum and minimum line
Reg_max <- (CIm_max * Data_reg$x) + CIb_min
Reg_min <- (CIm_min * Data_reg$x) + CIb_max
#plotting the scatterplot
plt2 <- ggplot() +
  geom_point( aes(x = Data_reg$x , y= Data_reg$y ))+
  geom_line( aes(x= Data_reg$x , y= regression ) , color = " blue ")+ # Optimal regression line
  geom_line( aes(x= Data_reg$x , y= Reg_max ) , color = " red ")+ # regression with upper -limit slope
  geom_line( aes(x= Data_reg$x, y= Reg_min ) , color = " red ")+ # Regression with lower -limit slope
  geom_smooth( aes( x= Data_reg$x , y= Data_reg$y ) , method = lm)+ #Auto - generated slope
  stat_regline_equation( aes(x= Data_reg$x, y= Data_reg$y) )+ # Inserting regression equation
  theme_bw() +
  labs (x =" Weight[mg]",
        y = " LDL[mg/L]")+
  ggtitle(" LDL[mg/L] VS Weight[mg]")+
  theme( axis.text = element_text ( size =12) ,
          axis.title = element_text ( size =12) ,
          plot.title = element_text ( hjust = 0.5) )

plot(plt2)
#_______________________________________________________________________________________________
library(ggplot2)
e <-regression - Data_reg$y #computing residual
#Scatterplot of the residual
library(ggplot2)
plt3 <- ggplot() +
  geom_point(aes(x = Data_reg$y, y = e))+
  geom_line(aes(x = Data_reg$y, y = 0), color = "Red")+
  theme_bw()+
  labs( x = "LDL[mg/L]",y = "Residual")+
  ggtitle("Scatter plot b/w LDL[mg/L] and Residual")+
  theme( axis.text = element_text( size =12) ,
         axis.title = element_text( size =12) ,
         plot.title = element_text( hjust = 0.5))
plt3
#_______________________________________________________________________________________________________

