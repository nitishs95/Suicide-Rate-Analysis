library(tidyverse) 
library(knitr) 
library(kableExtra)
library(treemap) 
library(ggthemes) 
library(highcharter)
library(summarytools)
library(corrplot)
library(formattable)

# Importing dataset
data <-read.csv("C:\Users\nitis\OneDrive\Desktop\Subject Semester 2\MVA\Project") 

# Data summary
# We are using str() & head() function to inspect and have a brief overwiew of the dataset.
str(data)
summary(data)
head(data)

# No of Columns in the dataset. 
length(data[,-1])

# Cleaning Data
# droppig NA values from suicide nos collumn.
clean_data <- data %>% 
filter(suicides_no != "NA" & suicides_no!=0) 

# Checking for the missing values in each collumns. 
colSums(is.na(clean_data))

# Cleaning HDI collumn. 
clean_data$HDI.for.year <- NULL

# Changing collumn name. 
colnames(data)[colnames(data) == "ï..country"] <- "country"

# Data exploration

#Nearly 70% of the data is missing. 
sum(is.na(data$HDI.for.year))/length(data$HDI.for.year) * 100

# Qualitative Variable frequencies 

# No of occurences of each generation in the dataset.
data %>% group_by(generation) %>% 
summarize(nb = n()) %>% kable () %>%
kable_styling(bootstrap_options = "striped", full_width = F)

# X generation and silent are the most popular.
# Generation Z is the smallest group.
hcbar(x = data$generation, name = "Génération") %>% 
hc_add_theme(hc_theme_economist())

# By Age Groups 
# Age groups are equally distributed.
hcbar(x = data$age, name = " ge") %>% 
hc_add_theme(hc_theme_economist())

# By Sex
# Both are equally distributed
hcbar(x = data$sex, name = "Sexe") %>% 
hc_add_theme(hc_theme_economist())

# By year
hcbar(x = as.character(data$year), name = "Years") %>% 
hc_add_theme(hc_theme_economist())

# Suicide rates by Sex and Age group
# For all age groups suicide rate is higher for men than women.
# This means 'sex' variable differentiates the population of dataset. 

data %>% group_by(year,sex, age) %>% 
summarize(moy_suicide = mean(suicides.100k.pop)) %>% 
ggplot(aes(x= year, y= moy_suicide)) + 
geom_line(aes(color = sex), size=1.1) + facet_wrap(~age, scale = "free_y") + 
ylab("Suicides (mean)") + ggtitle("Evolution of suicide rate per sex and age categories")

#Visualization
plot(data[,1], data[,5],main = "suicide/country", xlab="", ylab="suicide-no")
plot(data[,12], data[,5],main = "suicide/generation", xlab="", ylab="suicide-no")

#transforming age from character to numeric
data_1<-transform(data, age = as.numeric(age))
ggplot(data_1, aes(x=age))+geom_bar()
plot(data_1$suicides_no~data_1$sex, xlab="sex", ylab = "suicide_no")
qqnorm(data_1[,"age"], main = "age")
qqnorm(data_1[,"population"], main = "population")
qqnorm(data_1[,"suicides_no"], main = "suicides_no")
sd(data_1$suicides_no)
mean(data_1$suicides_no)

#removing suicides outliers
data_1<- subset(data_1, suicides_no< 2948.12)

# T-Test on dataset columns Age and suicides_no. Also, Suicides_no vs gdp
t.test(data_1$age,data_1$sucides_no, var.equal = TRUE, paired=FALSE)
t.test(data_1$gdp_per_capita,data_1$sucides_no, var.equal = TRUE, paired=FALSE)
