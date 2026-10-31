library(ggplot2)


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


## -- What is the average age of each user? -- ##

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

## Plotting the average ages and dropping NA value (Washington) to reduce/compare only real/expected ages
ggplot(subset(age_df, !is.na(Avg_Age)), aes(x = City, y = Avg_Age)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = Avg_Age), vjust = -0.4) +
  labs(title = "Average User Age", x = "Age", y = "Age (years)") +
  theme_minimal()



## -- What are the differences in use between genders? -- ##

# Function to accurately return the total amounts of each gender
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

# Plot the data collected from the gender data_frame to highlight the difference in user count
ggplot(gender_df, aes(x = Gender, y = Count, fill = Gender)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ City) +
  labs(title = "Users by Gender", x = "Gender", y = "Number of users") +
  theme_minimal()

 

## -- What are the mean, and median trip times and what would be the difference involving outliers? -- ##

median_trip_duration <- function(df, units = c("minutes", "seconds")) {
  units <- match.arg(units)
  secs <- median(df$Trip.Duration, na.rm = TRUE)
  if (units == "minutes") secs / 60 else secs
}

mean_trip_duration <- function(df, units = c("minutes", "seconds")) {
  units <- match.arg(units)
  secs  <- mean(df$Trip.Duration, na.rm = TRUE)
  if (units == "minutes") secs / 60 else secs
}

# Data frame containing both mean and median for comparison
trip_summary <- data.frame(
  City           = names(cities),
  Mean_Minutes   = round(sapply(cities, mean_trip_duration), 2),
  Median_Minutes = round(sapply(cities, median_trip_duration), 2),
  row.names      = NULL
)

# How much the mean is pulled above the median by long trips
trip_summary$Difference <- round(trip_summary$Mean_Minutes - trip_summary$Median_Minutes, 2)
trip_summary$Mean_vs_Median_Pct <- round(100 * trip_summary$Difference / trip_summary$Median_Minutes, 1)
trip_summary

# Charting the mean vs median trip durations
trip_long <- rbind(
  data.frame(City = trip_summary$City, Statistic = "Mean",   Minutes = trip_summary$Mean_Minutes),
  data.frame(City = trip_summary$City, Statistic = "Median", Minutes = trip_summary$Median_Minutes)
)

# Plotting the difference between the mean and median to highlight why median is better
ggplot(trip_long, aes(x = City, y = Minutes, fill = Statistic)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = Minutes), position = position_dodge(width = 0.9), vjust = -0.4) +
  scale_fill_manual(values = c(Mean = "steelblue", Median = "darkorange")) +
  labs(title = "Mean vs. Median Trip Duration", x = "City", y = "Minutes") +
  theme_minimal()



## Proper creation of summary statistics for each group/plot presented
summary(transform(age_df,       City = factor(City)))
summary(transform(trip_long,    City = factor(City), Statistic = factor(Statistic)))

# Age/gender larger summary is excluded due to NA vals being present below

# Only get the gender from the relevant ones
tapply(gender_df$Count, gender_df$City, summary)

# Concatenate the means/medians
tapply(trip_long$Minutes, trip_long$Statistic, summary)