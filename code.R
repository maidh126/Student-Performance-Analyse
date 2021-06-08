# Read the dataset
data <- read.csv("/Users/maido/Desktop/Dataset/Statistical Programming/StudentsPerformance.csv", header = TRUE)

attach(data)
# Average score in 3 subjects
avg.score <- rowSums(data[ , 6:8])/3
avg.score
data <- cbind(data, avg.score)
data
# Take a look at dataset
head(data)
summary(data)

## Visualisation
# Comparison of Gender attributes to the Marks
par(mfrow=c(2,2))
boxplot(data$math.score~data$gender, col=rainbow(7), main="Math", xlab="Gender", ylab="Math Score")
boxplot(data$reading.score~data$gender, col=rainbow(2), main="Reading", xlab="Gender", ylab="Reading Score")
boxplot(data$writing.score~data$gender, col=rainbow(3), main="Writing", xlab="Gender", ylab="Writing Score")
boxplot(data$avg.score~data$gender, col=rainbow(4), main="Average", xlab="Gender", ylab="Average Score")

## Hypothesis Testing
# Comparing the means of Math by Gender – the unpaired samples t-test
var.test(data$math.score~data$gender)
# The p-value is greater than 0.05, so do not reject the null hypothesis 
# that the variances are equal. 

# Perform an unpaired samples t-test using
t.test(data$math.score~data$gender, var.equal=TRUE)
# Conclude: cannot reject the null hypothesis H0.
# There is some difference in the math score of male and female


# Comparison of Race Ethnicity attributes to the Marks
library(dplyr)
library(ggplot2)
# Mean of Group
math_score <- data %>% group_by(race.ethnicity) %>% summarise(math.avg = mean(math.score), .groups = 'drop')
reading_score <- data %>% group_by(race.ethnicity) %>% summarise(reading.avg = mean(reading.score), .groups = 'drop')
writing_score <- data %>% group_by(race.ethnicity) %>% summarise(writing.avg = mean(writing.score), .groups = 'drop')
avg_score <- data %>% group_by(race.ethnicity) %>% summarise(avg = mean(avg.score), .groups = 'drop')

# Plot to compare 
par(mfrow=c(2,2))
barplot(math_score$math.avg~math_score$race.ethnicity,
        main="Average of Math Score",
        xlab="Race", ylab="Average Math Scores")
barplot(reading_score$reading.avg~reading_score$race.ethnicity,
        main="Average of Reading Score",
        xlab="Race", ylab="Average Reading Scores")
barplot(writing_score$writing.avg~writing_score$race.ethnicity,
        main="Average of Writing Score",
        xlab="Race", ylab="Average Writing Scores")
barplot(avg_score$avg~avg_score$race.ethnicity,
        main="Average Score",
        xlab="Race", ylab="Average Scores")


# Comparison of Test Preparation attributes to the Marks
library(gridExtra)
g1 <- ggplot(subset(data), aes(x=math.score, colour=test.preparation.course)) +
  geom_density() +
  ggtitle("Math Score")
g2 <- ggplot(subset(data), aes(x=reading.score, colour=test.preparation.course)) +
  geom_density() +
  ggtitle("Reading Score")
g3 <- ggplot(subset(data), aes(x=writing.score, colour=test.preparation.course)) +
  geom_density() +
  ggtitle("Writing Score")
g4 <- ggplot(subset(data), aes(x=avg.score, colour=test.preparation.course)) +
  geom_density() +
  ggtitle("Average Score")
grid.arrange(g1, g2, g3, g4, nrow = 2, ncol  = 2)

## Hypothesis Testing
# Comparing the means of Reading by Pre-Test Course
var.test(data$reading.score~data$test.preparation.course)
# The p-value is greater than 0.05, so do not reject the null hypothesis 
# that the variances are equal. 

# Perform an unpaired samples t-test using
t.test(data$reading.score~data$test.preparation.course, var.equal=TRUE)
# Conclude: Cannot reject the null hypothesis H0.
# There is some difference in the reading score of preparing course


# Comparison of Parent's Education attributes to the Average Marks
ggplot(data=data, mapping=aes(x=parental.level.of.education, y = avg.score, 
                              col=parental.level.of.education))+
  theme_bw() +
  geom_boxplot()+
  scale_y_continuous(limits=c(0,110),breaks = seq(0,110,10))+
  labs(title="Parent's Education effects the Average Marks", 
       x="Parent's Education Status", y="Average Score")

# ANOVA test to compare parents education and average marks
ANOVA<- aov(data$avg.score ~ data$parental.level.of.education)
summary(ANOVA)



## Decsriptive Statistics
# Summary on Pre-Test Course determining the average score
data %>% group_by(test.preparation.course) %>% summarise(
  Min = min(avg.score,na.rm = TRUE),
  Q1 = quantile(avg.score,probs= .25,na.rm = TRUE),
  Median = median(avg.score, na.rm = TRUE),
  Q3 = quantile(avg.score,probs = .75,na.rm = TRUE),
  Max = max(avg.score,na.rm = TRUE),
  Mean = mean(avg.score, na.rm = TRUE),
  SD = sd(avg.score, na.rm = TRUE),
  n = n(),
  Missing = sum(is.na(avg.score)), .groups = 'drop')


# Summary on gender determining the average score
data %>% group_by(gender) %>% summarise(
  Min = min(avg.score,na.rm = TRUE),
  Q1 = quantile(avg.score,probs= .25,na.rm = TRUE),
  Median = median(avg.score, na.rm = TRUE),
  Q3 = quantile(avg.score,probs = .75,na.rm = TRUE),
  Max = max(avg.score,na.rm = TRUE),
  Mean = mean(avg.score, na.rm = TRUE),
  SD = sd(avg.score, na.rm = TRUE),
  n = n(),
  Missing = sum(is.na(avg.score)), .groups = 'drop')


# Summary on parents determining the average score
data %>% group_by(parental.level.of.education) %>% summarise(
  Min = min(avg.score,na.rm = TRUE),
  Q1 = quantile(avg.score,probs= .25,na.rm = TRUE),
  Median = median(avg.score, na.rm = TRUE),
  Q3 = quantile(avg.score,probs = .75,na.rm = TRUE),
  Max = max(avg.score,na.rm = TRUE),
  Mean = mean(avg.score, na.rm = TRUE),
  SD = sd(avg.score, na.rm = TRUE),
  n = n(),
  Missing = sum(is.na(avg.score)), .groups = 'drop')


# Summary on Race Ethnicity determining the average score
data %>% group_by(race.ethnicity) %>% summarise(
  Min = min(avg.score,na.rm = TRUE),
  Q1 = quantile(avg.score,probs= .25,na.rm = TRUE),
  Median = median(avg.score, na.rm = TRUE),
  Q3 = quantile(avg.score,probs = .75,na.rm = TRUE),
  Max = max(avg.score,na.rm = TRUE),
  Mean = mean(avg.score, na.rm = TRUE),
  SD = sd(avg.score, na.rm = TRUE),
  n = n(),
  Missing = sum(is.na(avg.score)), .groups = 'drop')



