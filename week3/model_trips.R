library(tidyverse)
library(scales)
library(modelr)
library(lubridate)

theme_set(theme_bw())
options(repr.plot.width=4, repr.plot.height=3)

trips_per_day <- read_tsv('trips_per_day.tsv')
head(trips_per_day)
holidays <- read.delim2('holidays',header=F,sep = ',',
                        col.names = c('i','ymd','holiday'),
                        colClasses = c("integer","Date","character")) %>% select(ymd,holiday)
holidays <- holidays %>% filter(ymd >= "2014-01-01" & ymd < "2015-01-01")

trips_per_day <- trips_per_day %>% left_join(holidays,by='ymd')

trips_per_day <- trips_per_day %>% mutate(if_holiday = if_else(is.na(holiday),0,1)) %>%
  mutate(week_day = if_else(wday(ymd)>1 & wday(ymd)<7,1,0)) %>%
  mutate(rain = if_else(prcp>0.5,1,0)) %>%
  mutate(deep_snow = if_else(snwd>=0.5, snwd, 0))

frac_train = 0.8
frac_test = 0.1

set.seed(38)

num_days <- nrow(trips_per_day)
frac_train <- 0.8
num_train <- floor(num_days * frac_train)

# randomly sample rows for the training set 
train <- sample(1:num_days, num_train, replace=F)

# training set used to fit the model
trips_per_day_train <- trips_per_day[train, ]

# validate and test sets used to evaluate the fit
test_validate_set <- trips_per_day[-train, ]
num_test_validate <- floor((num_days - num_train)/2)
validate <- sample(nrow(test_validate_set),num_test_validate,replace = F)
trips_per_day_validate <- test_validate_set[validate,]
trips_per_day_test <- test_validate_set[-validate,]

K <- 1:8
train_err <- c()
validate_err <- c()
for (k in K) {
  
  # fit on the training data
  model <- lm(num_trips ~ poly(tmax,k,raw = T) + snwd + week_day + tmin*rain , data = trips_per_day_train)
  
  # evaluate on the training data
  train_err[k] <- sqrt(mean((predict(model, trips_per_day_train) - trips_per_day_train$num_trips)^2))
  
  # evaluate on the validate data
  validate_err[k] <- sqrt(mean((predict(model, trips_per_day_validate) - trips_per_day_validate$num_trips)^2))
}
plot_data <- data.frame(K, train_err, validate_err) %>%
  gather("split", "error", -K)

ggplot(plot_data, aes(x=K, y=error, color=split)) +
  geom_line() +
  scale_x_continuous(breaks=K) +
  xlab('Polynomial Degree') +
  ylab('RMSE')

train_err
validate_err

final_model <- lm(num_trips ~ poly(tmax,4,raw = T) + snwd + week_day + tmin*rain, data = trips_per_day_train)

trips_per_day<- trips_per_day %>% add_predictions(final_model)

trips_per_day %>% ggplot(aes(x=ymd,y=num_trips)) + geom_point() + geom_line(aes(y=pred))


save(final_model, file = 'citibike_model.RData')



