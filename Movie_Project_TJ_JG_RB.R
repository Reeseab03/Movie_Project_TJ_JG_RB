library(tidyverse)
library(Metrics)
library(caTools)
library(dplyr) 
dataset=read.csv('Movie_project.csv')

# explore
head(dataset)
glimpse(dataset)
names(dataset)
summary(dataset)
unique(dataset$rating)
unique(dataset$year)
str(dataset)
summary(dataset[sapply(data,is.numeric)])

# missing data 
colSums(is.na(dataset))
# inpute missing data

# score
png("density_score.png")
ggplot(data=dataset, aes(score))+
  geom_density()
dev.off()
score_mean=mean(dataset$score, na.rm=TRUE)
dataset$score=ifelse(is.na(dataset$score), score_mean, dataset$score)

ggplot(data = dataset, aes(budget))+
  geom_density()

# votes
png("density_votes")
ggplot(data=dataset, aes(votes))+
  geom_density()
dev.off()
votes_mean=mean(dataset$votes, na.rm=TRUE)
dataset$votes=ifelse(is.na(dataset$votes), votes_mean, dataset$votes)

# budget
png("density_budget")
ggplot(data=dataset, aes(budget))+
  geom_density()
dev.off()
budget_mean=mean(dataset$budget, na.rm=TRUE)
dataset$budget=ifelse(is.na(dataset$budget),budget_mean, dataset$budget)

# gross
png("density_gross")
ggplot(data=dataset, aes(gross))+
  geom_density()
dev.off()
gross_median=median(dataset$gross, na.rm=TRUE)
dataset$gross=ifelse(is.na(dataset$gross), gross_median, dataset$gross)

# runtime
png("density_runtime")
ggplot(data=dataset, aes(runtime))+
  geom_density()
dev.off()
runtime_mean=mean(dataset$runtime, na.rm=TRUE)
dataset$runtime=ifelse(is.na(dataset$runtime), runtime_mean, dataset$runtime)

colSums(is.na(dataset))

#Grouping into stars and showing the top 10
stars_gross <- dataset %>%
  group_by(star) %>%
  summarise(total_gross = sum(gross, na.rm = TRUE)) %>%
  arrange(desc(total_gross)) %>%
  top_n(n = 10, wt = total_gross)

#Graphing the top 10 stars by total gross of their movies
ggplot(stars_gross, aes(x = reorder(star, total_gross), y = total_gross)) +
  geom_bar(stat = "identity", fill = "blue") +
  coord_flip() + 
  labs(x = "Star", y = "Total Gross", title = "Top 10 Stars by Total Gross") +
  theme_minimal()


#grouping into genre and getting a count
genre_counts <- dataset %>%
  group_by(genre) %>%
  summarise(count = n()) %>%
  arrange(desc(count))
#plotting the number of movies in each genre with a barchart
ggplot(genre_counts, aes(x = genre, y = count)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Genre", y = "Number of Movies", title = "Number of Movies in Each Genre")

#grouping into genres and averaging the budget
average_budget_by_genre <- dataset %>%
  group_by(genre) %>%
  summarise(average_budget = mean(budget, na.rm = TRUE)) %>%
  arrange(desc(average_budget))
#plotting the average budget for each genre
ggplot(average_budget_by_genre,
       aes(x = genre,
           y = average_budget))+
  geom_bar(stat = 'identity', fill = 'red') +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Genre", y = "Average Budget", title = "Average Budget of Movies by Genre")+
  scale_y_continuous(labels = scales::dollar_format())

#sorting into top 10 movies by gross
top_movies <- dataset %>%
  arrange(desc(gross)) %>%
  head(10)
#plotting top 10 movies by gross in bar chart
ggplot(top_movies, aes(x = reorder(name, gross), y = gross)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  coord_flip() +  
  labs(x = "Movie", y = "Gross Earnings", title = "Top 10 Highest-Grossing Movies") +
  theme_minimal()

#grouping average budget by year
average_budget_per_year <- dataset %>%
  group_by(year) %>%
  summarise(average_budget = mean(budget, na.rm = TRUE)) %>%
  arrange(year)
#plotting average budget year by year in line plot
ggplot(average_budget_per_year, aes(x = year, y = average_budget)) +
  geom_line() +
  geom_point() +  
  theme_minimal() +
  labs(x = "Year", y = "Average Budget", title = "Average Movie Budget Over Years") +
  scale_y_continuous(labels = scales::dollar_format())

#grouping average gross by year
yearly_avg_gross <- dataset %>% 
  group_by(year) %>% 
  summarise(avg_gross = median(gross, na.rm = TRUE)) %>% 
  arrange(year)
#plotting average gross year by year in line plot
ggplot(yearly_avg_gross, 
       aes(x = year,
           y = avg_gross))+
  geom_line()+
  geom_point()+
  theme_minimal()+
  labs(x = 'year', y = 'average gross', title = 'Average Movie Gross Over Years')+
  scale_y_continuous(labels = scales::dollar_format())

#filtering data to exclude suspectd outliers
filtered_dataset <- dataset %>% 
  filter(runtime <= 200)
#plotting distribution of runtime in a histogram
ggplot(filtered_dataset, aes(x = runtime)) +
  geom_histogram(binwidth = 8, fill = "blue", color = "black") + 
  labs(x = "Runtime (minutes)", y = "Number of Movies", title = "Distribution of Movie Runtimes") +
  theme_minimal() 

#establishing new dataset with only variables used in regression model
data_new = dataset[c(12,13)]
#setting seed and splitting data into test and training sets
set.seed(123)
split = sample.split(data_new$gross, SplitRatio = 2/3)
training_set = subset(data_new, split == TRUE)
test_set = subset(data_new, split == FALSE)

#creating the regressor variable
regressor = lm(formula = gross~budget, training_set)
summary(regressor)

y_pred = predict(regressor, newdata = test_set)
result = data.frame(test_set$gross, y_pred)
head(result)

#plotting the prediction relative to the actual data
ggplot()+geom_point(aes(x=test_set$budget,
                        y=test_set$gross),
                    color = 'blue')+
  geom_line(aes(x=test_set$budget,
                y = y_pred),
            color='red')+
  xlab('Budget')+ylab('Gross')+
  scale_y_continuous(labels = scales::dollar_format())+
  scale_x_continuous(labels = scales::dollar_format())

#accuracy checks
mae(test_set$gross, y_pred)
mse(test_set$gross, y_pred)
rmse(test_set$gross, y_pred)
#predict
new=data.frame(budget=1000000)
predict(regressor,newdata=new)

