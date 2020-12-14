###################                           ######################
###################      GROUP 3 project      ######################
###################                           ######################
###################                           ######################

#Zhenpeng 2020.11.4  18:00 Beijing time

#data
data <- read.csv(file = 'usa_states_data.csv', header = TRUE)
head(data)

#info: lockdown states and timelines
info <- list(state = c("MI", "CA", "NY","IL","CT"), start_date = as.Date(c("2020-03-24", "2020-03-19", "2020-03-22","2020-03-21","2020-03-23"), "%Y-%m-%d"), end_date = as.Date(c("2020-04-23", "2020-05-08", "2020-6-13","2020-05-30","2020-04-22"), "%Y-%m-%d"))


total_cases_increase_rate <- list(state=c(), during_lock=c(), after_lock=c())
new_cases_increase_rate <- list(state=c(), slope_t=c(), slope_c=c())
#1:length(info$state)
for (i in 1:length(info$state)) {
  
  # select state
  data.state <- data[data$state == info$state[i], ]
  data.state$date <- as.Date(data.state$date, "%Y-%m-%d")
  
  # select time: during lock and after lock
  end_date_after <- as.Date(info$end_date[i], "%Y-%m-%d") + (info$end_date[i]-info$start_date[i])
  
  data.lock <- data.state[data.state$date > info$start_date[i] & data.state$date < end_date_after, ]
  data.lock <- data.lock[order(data.lock$date), ] # sort the data by dates
  
  # select explanatory variable: t(continues: time) and c(categories: lockdown |not lockdown)
  c = numeric(length(data.lock$date))
  c[data.lock$date<info$end_date[i]]=1
  
  data.l <- list(t = 1:length(data.lock$date), c=c, new_cases = data.lock$positiveIncrease)
  
  
  
  #regression
  lm.reg.l <- lm(data.l$new_cases ~ data.l$t+data.l$c)
  
  
  
  #plot
  #plot(x=data.l$t, y=data.l$new_cases)
  #print(summary(lm.reg.l))

  #result
  new_cases_increase_rate$state <- append(new_cases_increase_rate$state, info$state[i])# info$state[i]
  new_cases_increase_rate$slope_t <- append(new_cases_increase_rate$slope_t, lm.reg.l$coefficients[2])# lm.reg$coefficients[2]
  new_cases_increase_rate$slope_c <- append(new_cases_increase_rate$slope_c, lm.reg.l$coefficients[3])
  
}

print(new_cases_increase_rate)
