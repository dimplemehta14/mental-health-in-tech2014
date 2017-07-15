# mental-health-in-tech2014

#This project aims to understand the attitude of people towards their mental health issue.

install_load <- function (packages)  {   
  
  # Start loop to determine if each package is installed
  for(package in packages){
    
    # If package is installed locally, load
    if(package %in% rownames(installed.packages()))
      do.call('library', list(package))
    
    # If package is not installed locally, download, then load
    else {
      install.packages(package, dependencies = TRUE)
      do.call("library", list(package))
    }
  } 
}

# Generic libraries loading
libs <- c("ggplot2", "maps", "plotly", "plyr", "dplyr", "rworldmap","stringr","lubridate", "plotly", "reshape2", "magrittr", "ggthemes", "tidyr", "DT", "lubridate","RColorBrewer")
install_load(libs)

# Specific methods libraries loading
libs.methods <- c("C50", "lattice", "nnet", "e1071","Matrix", "foreach","glmnet","C50","randomForest","ipred","rpart")
install_load(libs.methods)
#Load the file
survey=read.csv("survey.csv")
#To see it in dataframe
str(survey)
summary(survey)
# Get the numerical summary (minimum, first quartile, median, mean, third quartile, and maximum) of Age in data 
summary(survey$Age)
# Print the statistics of each factor
barplot(table(survey$Age))
# Print out the outliers
outlier_age <- subset(survey[2:4], Age < 16 | Age > 75 )
nrow(outlier_age)
# Drop the outliers
age_clean <- subset(survey, Age > 16 & Age <60)
dim(age_clean)
# Print the statistics of each factor
boxplot(age_clean$Age)
summary(age_clean$Age)

# Age categorization
age_clean$Age<-cut(age_clean$Age, breaks = c(16, 24, 34, 60), labels = c('Fresh', 'Junior', 'Senior'))

# Create the frequency table of age group
table(age_clean$Age)


# Create the relative frequency table of age
table(age_clean$Age)/length(age_clean$Age)


# Group by Age Group and count each group
age_group <- age_clean %>%
  group_by(Age) %>%
  dplyr::summarize(count = n())
age_group

# Visualize the number of subjects in each Age Group  
ggplot(age_group, aes(x = Age, y = count, fill = Age)) +  
  geom_bar(stat = "identity", alpha = 0.5) +
  xlab("Age Group") + 
  ylab("No of People") + 
  ggtitle("Comparing Age Group in the 2014 Mental Health in Tech Survey")

# To create a dataset similar to age_clean
data <- age_clean
# To delete no important elements
data <- data[ , !(names(data) %in% "state")]
data <- data[ , !(names(data) %in% "Timestamp")]
data <- data[ , !(names(data) %in% "comments")]
data <- data[ , !(names(data) %in% "self_employed")]
# NA values detection and deleting the row.
sapply(data, function(x) sum(is.na(x)))
data <- data[!is.na(data$work_interfere),]

# Gender unification.

data$Gender %<>% str_to_lower()


male_str <- c("male", "m", "male-ish", "maile", "mal", "male (cis)", "make", "male ", "man","msle", "mail", "malr","cis man", "cis male")
trans_str <- c("trans-female", "something kinda male?", "queer/she/they", "non-binary","nah", "all", "enby", "fluid", "genderqueer", "androgyne", "agender", "male leaning androgynous", "guy (-ish) ^_^", "trans woman", "neuter", "female (trans)", "queer", "ostensibly male, unsure what that really means" )
female_str <- c("cis female", "f", "female", "woman",  "femake", "female ","cis-female/femme", "female (cis)", "femail")

data$Gender <- sapply(as.vector(data$Gender), function(x) if(x %in% male_str) "male" else x )
data$Gender <- sapply(as.vector(data$Gender), function(x) if(x %in% female_str) "female" else x )
data$Gender <- sapply(as.vector(data$Gender), function(x) if(x %in% trans_str) "trans" else x )
data %<>% filter(Gender != "a little about you")
data %<>% filter(Gender != "guy (-ish) ^_^")
data %<>% filter(Gender != "p")


gender_group <- data %>%
  group_by(Gender) %>%
  dplyr::summarize(count = n())
gender_group


ggplot(gender_group, aes(x = Gender, y = count, fill = Gender)) +  
  geom_bar(stat = "identity", alpha = 0.5) +
  xlab("Gender Group") + 
  ylab("No of People") + 
  ggtitle("Comparing Gender in the 2014 Mental Health in Tech Survey")

for(i in 1:length(data)){
  aux <- prop.table(table(data$treatment, data[,i]), 1)*100 
  percent <- round(max(abs(aux[1,]-aux[2,])), digits = 2)

  if(percent > 10 & percent < 99){
    
    # Data preparing to visualization
    aux <- prop.table(table(data$treatment, data[,i]), 1)*100 
    nom <- colnames(aux)
    type <- c(rep("Yes",ncol(aux)),rep("No",ncol(aux)))
    val <- append(aux[1,], aux[2,])
    data.aux<-data.frame(nom=nom,type=type ,val=val)
    
    # Use of the library ggplot2 to data visualization 
    g <- ggplot() + geom_bar(data=data.aux,aes(x=nom, y=val,fill=type),stat='identity',position='dodge')+
      coord_flip() +
      labs(
        x = "Importance",
        y = "",
        title = paste("Mental Health comparation about ", names(data[i]), sep=""),
        subtitle = paste("The most different is ", percent, "%", sep=""),
        caption = "\nDetermined by matrix of covariances"
        ) %>% suppressWarnings()
    print(g)
  }

}

data <- data.frame(family_history= data$family_history,
                   work_interfere= data$work_interfere,
                   care_options= data$care_options,
                   treatment=data$treatment)

# Preparing regression function for the use in other methods
regresion <- treatment~
                family_history+
                work_interfere+
                care_options
              
# Saving prediction percentage of each method
percent <- data.frame(methods=c("Trees Classifier","Random Forest"), value=c(0,0))

# Data training and testing
set.seed(101)
n <- nrow(data)
data.index <- sample(1:n , size=round(n*0.7))
train <- data[data.index,]
test <- data[-data.index,]

# Executing model C5.0
treemodel <- C5.0( treatment ~ . , data = train)

# Prediction
prediction <- predict(treemodel,newdata=test)

# Confussion matrix
( mc <- table(prediction, test$treatment) )


# Succesful percentage of clasification
( percent$value[1] <- sum(diag(mc)) / sum(mc) * 100 )


# Random Forest model
model <- randomForest(treatment ~ .,  data= train)

# Prediction. Creating a dataframe with the probabilities
predict <- predict(model, test)

# Confussion matrix
( mc <- with(test, table(predict, treatment)) )

# Succesful percentage of clasification
( percent$value[2] <- sum(diag(mc)) / sum(mc) * 100 )

#importance of each variable
mental_all_var <- treatment~
  family_history+
  work_interfere+
  care_options

mental_var_tree <- treatment~family_history+work_interfere+care_options
treeRF_mental <- randomForest(mental_var_tree, data = train, ntree=25, proximity = T)
dataimp_spam <- varImpPlot(treeRF_mental, main = "Importance of each variable")

# Executing model C5.0
model <- C5.0( treatment ~ . , data = train)
plot(model)

percent$methods <- paste(percent$methods, " - " , round(percent$value,digits = 2) , "%" , sep = "")
visualize <- data.frame(valor=percent$value, group= percent$methods)
visualize2 <- rbind(visualize,data.frame(valor=50, group= visualize$group))

ggplot() +
  geom_point(data = visualize, aes(x = valor, y = group, color = group), size = 4) +
  geom_path(data = visualize2, aes(x = valor, y = group, color = group, group = group), size = 2) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5)) +
  labs(
    x = "Percentage of success",
    y = "Methods",
    title = "Percentage of success of the methods"
  ) 
