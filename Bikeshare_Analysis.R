library(ggplot2)

# =============================================================
# Bikeshare Data Exploration: New York City, Chicago, Washington
# Data: randomly selected trips from January-June 2017.
# Each row in the data is one trip, not one unique person.
#
# Questions of interest:
#   1. What is the average age of riders in each city?
#   2. How does bikeshare use differ between genders?
#   3. What are the mean and median trip times, and how much do
#      outliers (very long trips) affect the difference?
#
# Note: Washington has no Gender or Birth.Year columns, so
# questions 1 and 2 only apply to New York City and Chicago.
# =============================================================

# Store all cities in one named list so functions can be applied
# to every city at once with sapply()/lapply()
df_chicago <- read.csv("chicago.csv")
df_new_york <- read.csv("new-york-city.csv")
df_washington <- read.csv("washington.csv")

cities <- list(
    "New York" = df_new_york,
    "Chicago" = df_chicago,
    "Washington" = df_washington
)


## -- Question 1: What is the average age of riders in each city? -- ##

# avg_age: mean rider age for one city.
#   df      - a city data frame
#   max_age - ages above this are treated as entry errors and dropped
#             (Chicago contains a birth year of 1899)
# Returns one number (mean age in years), or NA if the city has no
# Birth.Year column (Washington). Rows with a missing birth year are ignored.
avg_age <- function(df, max_age = 100) {
  if(!"Birth.Year" %in% names(df)) return (NA_real_) # If no birth year
  age <- 2017 - df$Birth.Year # Age is from 2017 since that's when data is from
  mean(age[!is.na(age) & age <= max_age])
}

age_df <- data.frame(
  City = names(cities),
  Avg_Age = round(sapply(cities, avg_age), 1),
  row.names = NULL
)
age_df

## Plot: average rider age by city. Washington is dropped because it has
## no age data, so only cities with real ages are compared.
ggplot(subset(age_df, !is.na(Avg_Age)), aes(x = City, y = Avg_Age)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = Avg_Age), vjust = -0.4) +
  labs(title = "Average User Age", x = "City", y = "Age (years)") +
  theme_minimal()

## Numeric summary of the average ages across cities with age data
summary(transform(age_df,       City = factor(City)))


# FINDING (Question 1):
# The average rider age is 38.7 years in New York and 36.1 years in Chicago,
# a difference of only 2.6 years, so riders in the two cities are very
# similar in age. Ages come from trips where a birth year was recorded
# (about 61,000 Chicago trips have none), and ages over 100 were removed.
# Because each row is a trip, frequent riders count more than once.
# Possible reason: the bikeshare systems are used mostly by working-age
# commuters, which keeps the average in the 30s in both cities.
# Washington has no birth year data, so no age can be reported for it.



## -- Question 2: How does bikeshare use differ between genders? -- ##

# gender_totals: counts trips by gender for one city.
#   df - a city data frame
# Returns a table of counts, or NULL if the city has no Gender column
# (Washington). Blank and NA entries are excluded, so only trips with a
# reported gender are counted.
gender_totals <- function(df) {
  if (!"Gender" %in% names(df)) return(NULL) # If no gender column
  gender <- df$Gender
  gender <- gender[!is.na(gender) & gender != ""] # Drop NA and blank strings
  table(Gender = gender)
}

# Apply to every city, then keep only cities that returned results
gender_list <- Filter(Negate(is.null), lapply(cities, gender_totals))


# Combine the per-city tables into one data_frame for plotting
gender_df <- do.call(rbind, lapply(names(gender_list), function(nm) {
  data.frame(City = nm, as.data.frame(gender_list[[nm]], responseName = "Count"))
}))
gender_df

# Plot: number of trips by gender, one panel per city, to show the
# difference in trip counts between genders.
ggplot(gender_df, aes(x = Gender, y = Count, fill = Gender)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ City) +
  labs(title = "Trips by Gender", x = "Gender", y = "Number of trips") +
  theme_minimal()

# Numeric summary of trip counts per city. This is run on gender_df, which
# already excludes blank/NA genders, so missing values don't distort it.
tapply(gender_df$Count, gender_df$City, summary)

# OPTIONAL: prints how many trips were left out for missing gender
sapply(cities[c("New York", "Chicago")], function(df) sum(is.na(df$Gender) | df$Gender == ""))


# FINDING (Question 2):
# In both cities, male riders take far more trips than female riders:
# 204,008 male vs. 66,783 female trips in New York (about 3.1 to 1) and
# 181,190 male vs. 57,758 female trips in Chicago (about 3.1 to 1).
# Missing gender values are stored as blank entries. In Chicago, 61,052 of
# 300,000 trips (about 20%) have no gender and are excluded from these
# counts; in New York, 29,209 trips (about 10%) are excluded. These results
# therefore describe only trips with a reported gender.
# Possible reason: gender is entered at sign-up, and blank entries appear
# mostly for casual customers who never registered, so the counts reflect
# subscribers more than casual riders.




## -- Question 3: What are the mean and median trip times, and how do
##    outliers affect the difference? -- ##

# median_trip_duration: median trip length for one city.
#   df    - a city data frame (Trip.Duration is in seconds)
#   units - "minutes" (default) or "seconds"
# Returns one number.
median_trip_duration <- function(df, units = c("minutes", "seconds")) {
  units <- match.arg(units)
  secs <- median(df$Trip.Duration, na.rm = TRUE)
  if (units == "minutes") secs / 60 else secs
}

# mean_trip_duration: same arguments and return value, but the mean.
mean_trip_duration <- function(df, units = c("minutes", "seconds")) {
  units <- match.arg(units)
  secs  <- mean(df$Trip.Duration, na.rm = TRUE)
  if (units == "minutes") secs / 60 else secs
}

# Data frame containing both mean and median (in minutes) for comparison
trip_summary <- data.frame(
  City           = names(cities),
  Mean_Minutes   = round(sapply(cities, mean_trip_duration), 2),
  Median_Minutes = round(sapply(cities, median_trip_duration), 2),
  row.names      = NULL
)

# Difference = how many minutes the mean sits above the median.
# Mean_vs_Median_Pct = that difference as a percent of the median.
trip_summary$Difference <- round(trip_summary$Mean_Minutes - trip_summary$Median_Minutes, 2)
trip_summary$Mean_vs_Median_Pct <- round(100 * trip_summary$Difference / trip_summary$Median_Minutes, 1)
trip_summary

# Reshape to long format (one row per city/statistic pair) for plotting
trip_long <- rbind(
  data.frame(City = trip_summary$City, Statistic = "Mean",   Minutes = trip_summary$Mean_Minutes),
  data.frame(City = trip_summary$City, Statistic = "Median", Minutes = trip_summary$Median_Minutes)
)

# Plot: mean vs. median trip duration side by side for each city, to show
# how much long trips pull the mean above the median.
ggplot(trip_long, aes(x = City, y = Minutes, fill = Statistic)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = Minutes), position = position_dodge(width = 0.9), vjust = -0.4) +
  scale_fill_manual(values = c(Mean = "steelblue", Median = "darkorange")) +
  labs(title = "Mean vs. Median Trip Duration", x = "City", y = "Minutes") +
  theme_minimal()

# Numeric summaries of the mean and median values, grouped by statistic
tapply(trip_long$Minutes, trip_long$Statistic, summary)
summary(transform(trip_long,    City = factor(City), Statistic = factor(Statistic)))


# FINDING (Question 3):
# In every city the mean trip is longer than the median trip. The mean is
# 14.99 minutes vs. a median of 10.15 in New York (about 48% higher),
# 15.60 vs. 11.17 in Chicago (about 40% higher), and 20.62 vs. 11.78 in
# Washington (about 75% higher). The longest typical trip (highest median)
# is in Washington, at 11.78 minutes.
# Possible reason: a small number of extremely long trips (Chicago's longest
# is 86,224 seconds, over 23 hours, likely bikes not returned promptly)
# pull the mean upward, while the median is not affected by them. The median
# is therefore the better measure of a typical trip, and the gap between the
# two shows how strongly outliers influence each city's average. Washington
# has the largest gap, so its average is the most affected by long trips.