# %%
df_chicago <- read.csv("chicago.csv")
df_new_york <- read.csv("new-york-city.csv")
df_washington <- read.csv("washington.csv")

cities <- list(
    "New York" = df_new_york,
    "Chicago" = df_chicago,
    "Washington" = df_washington
)

## Pretty sure I can just run l/sapply on cities to easily iterate over them


# %%
## Calculate the average age of users

avg_age <- function(df) {
  age <- 2017 - df$Birth.Year # Age is from 2017 since that's when data is from
  mean(age, na.rm = TRUE)
}

avg_age(df_chicago)
avg_age(df_new_york)


#%%
## Calculate the number of each gender

gender_totals <- function(df) {
  gender <- df$Gender
  gender[gender == ""] <- NA # Missing values stored as empty strings
  table(gender, useNA = "ifany")
}

gender_totals(df_chicago)
gender_totals(df_new_york)


#%% 
# Median trip duration in seconds 
median_trip_duration <- function(df, units = c("minutes", "seconds")) {
  units <- match.arg(units)
  secs <- median(df$Trip.Duration, na.rm = TRUE)
  if (units == "minutes") secs / 60 else secs
}

median_trip_duration(df_chicago)
median_trip_duration(df_new_york)
median_trip_duration(df_washington)
