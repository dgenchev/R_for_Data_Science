### Data transformation
### Source: http://r4ds.had.co.nz/transform.html

library(nycflights13)
library(tidyverse)


# Dplyr Basics ------------------------------------------------------------

# Pick observations by their values (filter()).
# Reorder the rows (arrange()).
# Pick variables by their names (select()).
# Create new variables with functions of existing variables (mutate()).
# Collapse many values down to a single summary (summarise()).
# These can all be used in conjunction with group_by() which changes the scope of each 
# function from operating on the entire dataset to operating on it group-by-group.

jan1 <- filter(flights, month == 1, day == 1)

# arrange() works similarly to filter() except that instead of selecting rows, 
# it changes their order. It takes a data frame and a set of column names 
# (or more complicated expressions) to order by. If you provide more than one column name, 
# each additional column will be used to break ties in the values of preceding columns:

arrange(flights, year, month, day)
arrange(flights, desc(dep_delay))

select(flights, year, month, day)

# Select all columns between year and day (inclusive)
select(flights, year:day)

# Select all columns except those from year to day (inclusive)
select(flights, -(year:day))

# starts_with("abc"): matches names that begin with “abc”.
# ends_with("xyz"): matches names that end with “xyz”.
# contains("ijk"): matches names that contain “ijk”.
# matches("(.)\\1"): selects variables that match a regular expression. 
# num_range("x", 1:3): matches x1, x2 and x3.

rename(flights, tail_num = tailnum)

# mutate() always adds new columns at the end of your dataset so we’ll start by creating 
# a narrower dataset so we can see the new variables
flights_sml <- select(flights, 
                      year:day, 
                      ends_with("delay"), 
                      distance, 
                      air_time
)
mutate(flights_sml,
       gain = dep_delay - arr_delay,
       speed = distance / air_time * 60
)

# Note that you can refer to columns that you’ve just created:
  
mutate(flights_sml,
       gain = dep_delay - arr_delay,
       hours = air_time / 60,
       gain_per_hour = gain / hours
)

# If you only want to keep the new variables, use transmute():
  
transmute(flights,
          gain = dep_delay - arr_delay,
          hours = air_time / 60,
          gain_per_hour = gain / hours
)

# summarise(). It collapses a data frame to a single row:
summarise(flights, delay = mean(dep_delay, na.rm = TRUE))

by_day <- group_by(flights, year, month, day)
summarise(by_day, delay = mean(dep_delay, na.rm = TRUE))


# The Pipe ----------------------------------------------------------------

delays <- flights %>% 
  group_by(dest) %>% 
  summarise(
    count = n(),
    dist = mean(distance, na.rm = TRUE),
    delay = mean(arr_delay, na.rm = TRUE)
  ) %>% 
  filter(count > 20, dest != "HNL")

not_cancelled <- flights %>% 
  filter(!is.na(dep_delay), !is.na(arr_delay))

not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(mean = mean(dep_delay))

# Whenever you do any aggregation, it’s always a good idea to include either a count (n()), 
# or a count of non-missing values (sum(!is.na(x))). That way you can check that you’re not 
# drawing conclusions based on very small amounts of data.
delays <- not_cancelled %>% 
  group_by(tailnum) %>% 
  summarise(
    delay = mean(arr_delay)
  )

ggplot(data = delays, mapping = aes(x = delay)) + 
  geom_freqpoly(binwidth = 10)

delays <- not_cancelled %>% 
  group_by(tailnum) %>% 
  summarise(
    delay = mean(arr_delay, na.rm = TRUE),
    n = n()
  )

ggplot(data = delays, mapping = aes(x = n, y = delay)) + 
  geom_point(alpha = 1/10)

# it’s often useful to filter out the groups with the smallest numbers of observations, 
# so you can see more of the pattern and less of the extreme variation in the smallest groups
delays %>% 
  filter(n > 25) %>% 
  ggplot(mapping = aes(x = n, y = delay)) + 
  geom_point(alpha = 1/10)

