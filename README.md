# homegrown-dsc-week3
## 5.5.2 Question 1
library(nycflights13)
library(tidyverse)
mutate(flights,
       dep_time = (dep_time %/% 100) * 60 + (dep_time %% 100),
       sched_dep_time = (sched_dep_time %/% 100) * 60 + (sched_dep_time %% 100))
## Question 2
## Numbers are in different formats per question
## number formats. To fix it we'd need to convert arr_time in the same way. Below does this. 
flights %>% 
  mutate(dep_time = (dep_time %/% 100) * 60 + (dep_time %% 100),
         sched_dep_time = (sched_dep_time %/% 100) * 60 + (sched_dep_time %% 100),
         arr_time = (arr_time %/% 100) * 60 + (arr_time %% 100),
         sched_arr_time = (sched_arr_time %/% 100) * 60 + (sched_arr_time %% 100)) %>%
  transmute((arr_time - dep_time) %% (60*24) - air_time))

## Question 3
## You'd expect sched_dep_time + dep_delay to = dep_time

## Question 4
filter(flights, min_rank(desc(dep_delay))<=10)

## Question 5
1:3 + 1:10
## Creates a vector but since they are different lengths it provides a warning message 

## 5.6.7 Question 2
not_cancelled <- filter(flights, !is.na(dep_delay), !is.na(arr_delay))
not_cancelled %>%
  group_by(dest) %>%
  tally()

not_cancelled %>%
  group_by(tailnum) %>%
  summarise(n = sum(distance))

## Question 3 There are no flights which arrived but did not depart, so we can just use `!is.na(dep_delay)

## Question 4 As delays increase cancelations increase. Its a low slope so not an intense correlation. 
canceled_delayed <-
  flights %>%
  mutate(canceled = (is.na(arr_delay) | is.na(dep_delay))) %>%
  group_by(year, month, day) %>%
  summarise(prop_canceled = mean(canceled),
            avg_dep_delay = mean(dep_delay, na.rm = TRUE))
ggplot(canceled_delayed, aes(x = avg_dep_delay, prop_canceled)) +
  geom_point() +
  geom_smooth()

## Question 5 This sorts the carriers by avg. delays
flights %>%
  filter(arr_delay > 0) %>%
  group_by(carrier) %>%
  summarise(average_arr_delay = mean(arr_delay, na.rm=TRUE)) %>%
  arrange(desc(average_arr_delay))

## Question 6
## Sorts by descending order of n

## 5.7.1 Question 1 
## Operate within each group rather than over all of the data frame 

## Question 2 N844MH
flights %>%
  group_by(tailnum) %>%
  summarise(arr_delay = mean(arr_delay)) %>%
  filter(min_rank(desc(arr_delay)) <= 1)

## Question 3 Earlier in the morning has the least amount of delays. Makes sense since less time to get off schedule.
flights %>%
  group_by(hour) %>%
  summarise(arr_delay = mean(arr_delay, na.rm = TRUE)) %>%
  arrange(arr_delay)

## Question 5
lagged_delays <- flights %>%
  arrange(origin, year, month, day, dep_time) %>%
  group_by(origin) %>%
  mutate(dep_delay_lag = lag(dep_delay)) %>%
  filter(!is.na(dep_delay), !is.na(dep_delay_lag))
lagged_delays %>%
  summarise(delay_diff = mean(dep_delay - dep_delay_lag), na.rm = TRUE)

## Question 6 Used zscore to determine the most suspicious flights 
flights_with_zscore <- flights %>%
  filter(!is.na(air_time)) %>%
  group_by(dest, origin) %>%
  mutate(air_time_mean = mean(air_time),
         air_time_sd = sd(air_time),
         n = n()) %>%
  ungroup() %>%
  mutate(z_score = (air_time - air_time_mean) / air_time_sd)

flights_with_zscore %>%
  arrange(desc(abs(z_score))) %>%
  select(z_score,flight) %>%
  print(n = 15)

## Question 7
flights %>%
  group_by(dest) %>%
  filter(n_distinct(carrier)>=2) %>%
  group_by(carrier) %>%
  summarise(multipleflights = n_distinct(dest)) %>%
  arrange(desc(multipleflights))
## Question 8
flights %>%
  arrange(tailnum, year, month, day) %>%
  group_by(tailnum) %>%
  mutate(delay1hr = dep_delay > 60) %>%
  mutate(before_delay = cumsum(delay1hr)) %>%
  filter(before_delay < 1) %>%
  count(sort = TRUE)

## 19.3.1 Question 1
## f1 checks to see if the string starts with prefix contains_prefix; f2 if the length is less than or equal to 1 return null 

## Question 2 One of the ones written in datacamp where we squared values val_square

## question 3 the first pulls from a univariate and the mass pulls from a multivariate

## quesiton 4
## norm groups by distrubution rnorm groups by the function both could be useful depending on how you are used to manipulating the data 

## 19.4.4 Question 1 
## if and ifelse differ in that ifelse provides a second option so instead of just providing the value for the first if statement it also provides it for all the other values 

## Quesiton 2
library(lubridate)
greet <- function(time = lubridate::now()) {
  hr <- lubridate::hour(time)
  if (hr < 12) {
    print("good morning")
  } else if (hr < 17) {
    print("good afternoon")
  } else {
    print("good evening")
  }
}

## Quesiton 3
fizzbuzz <- function(x) {
  if (!(x %% 3) && !(x %% 5)) {
    "fizzbuzz"
  } else if (!(x %% 3)) {
    "fizz"
  } else if (!(x %% 5)) {
    "buzz"
  } else {
    x
  }
}
fizzbuzz(30)
fizzbuzz(10)
fizzbuzz(18)
fizzbuzz(9)

##Question 4
temp <- seq(0, 50, by = 10)
cut(temp, c(0, 10, 20, 30,Inf), right = TRUE,
    labels = c("freezing", "cold", "cool", "warm", "hot"))
##Question 5
## Switch will return the value in the sequence n=3 will return 3rd set 

##Question 6
x<-"e"
switch(x, 
       a = ,
       b = "ab",
       c = ,
       d = "cd"
)
## nothing appears to happen 
