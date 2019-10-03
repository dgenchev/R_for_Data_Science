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

# plot the skill of the batter (measured by the batting average, ba) 
# against the number of opportunities to hit the ball (measured by at bat, ab)
# Convert to a tibble so it prints nicely
library(Lahman)
batting <- as_tibble(Lahman::Batting)

batters <- batting %>% 
  group_by(playerID) %>% 
  summarise(
    ba = sum(H, na.rm = TRUE) / sum(AB, na.rm = TRUE),
    ab = sum(AB, na.rm = TRUE)
  )

batters %>% 
  filter(ab > 100) %>% 
  ggplot(mapping = aes(x = ab, y = ba)) +
  geom_point() + 
  geom_smooth(se = FALSE)
#> `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'


# Useful summary functions ------------------------------------------------

# Measures of location: mean(x) and median(x)
not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(
    avg_delay1 = mean(arr_delay),
    avg_delay2 = mean(arr_delay[arr_delay > 0]) # the average positive delay
  )

# Measures of spread: sd(x), IQR(x), mad(x)
# IQR() and mad(x) are robust equivalents to sd(), useful if you have outliers
# Why is distance to some destinations more variable than to others?
not_cancelled %>% 
  group_by(dest) %>% 
  summarise(distance_sd = sd(distance)) %>% 
  arrange(desc(distance_sd))

# Measures of rank: min(x), quantile(x, 0.25), max(x)
# When do the first and last flights leave each day?
not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(
    first = min(dep_time),
    last = max(dep_time)
  )

# Measures of position: first(x), nth(x, 2), last(x)
not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(
    first_dep = first(dep_time), 
    last_dep = last(dep_time)
  )

not_cancelled %>% 
  group_by(year, month, day) %>% 
  mutate(r = min_rank(desc(dep_time))) %>% 
  filter(r %in% range(r))

# Counts: n(), sum(!is.na(x)), n_distinct(x)
# Which destinations have the most carriers?
not_cancelled %>% 
  group_by(dest) %>% 
  summarise(carriers = n_distinct(carrier)) %>% 
  arrange(desc(carriers))

not_cancelled %>% 
  count(dest)
# Can provide a weight variable
not_cancelled %>% 
  count(tailnum, wt = distance)

# Counts and proportions of logical values: sum(x > 10), mean(y == 0)
# hen used with numeric functions, TRUE is converted to 1 and FALSE to 0. 
# This makes sum() and mean() very useful: sum(x) gives the number of TRUEs in x, 
# and mean(x) gives the proportion.
# How many flights left before 5am? (these usually indicate delayed
# flights from the previous day)
not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(n_early = sum(dep_time < 500))

# What proportion of flights are delayed by more than an hour?
not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(hour_perc = mean(arr_delay > 60))


# Grouping by multiple vars -----------------------------------------------

daily <- group_by(flights, year, month, day)
(per_day   <- summarise(daily, flights = n()))
(per_month <- summarise(per_day, flights = sum(flights)))
(per_year  <- summarise(per_month, flights = sum(flights)))

# Be careful when progressively rolling up summaries: it’s OK for sums and counts,
# but you need to think about weighting means and variances, and it’s not possible 
# to do it exactly for rank-based statistics like the median. 
# In other words, the sum of groupwise sums is the overall sum, 
# but the median of groupwise medians is not the overall median.


# Ungrouping --------------------------------------------------------------

daily %>% 
  ungroup() %>%             # no longer grouped by date
  summarise(flights = n())  # all flights


# Grouped mutates ---------------------------------------------------------

# Find the worst members of each group:
flights_sml %>% 
  group_by(year, month, day) %>%
  filter(rank(desc(arr_delay)) < 10)

# Find all groups bigger than a threshold:
popular_dests <- flights %>% 
  group_by(dest) %>% 
  filter(n() > 365)
popular_dests

# Standardise to compute per group metrics:
popular_dests %>% 
  filter(arr_delay > 0) %>% 
  mutate(prop_delay = arr_delay / sum(arr_delay)) %>% 
  select(year:day, dest, arr_delay, prop_delay)
