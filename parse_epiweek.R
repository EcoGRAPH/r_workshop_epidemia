
# Documentation for the user-defined function parse_epiweek()
#
# This function parses epdemiological weeks into dates. In other words, it
# calculate Gregorian calendar dates based on epidemiological years, week
# numbers, and (optionally) weekday numbers.
#
# Arguments:
# 
#   year    Year. Four-digit epidemiological years, as numeric or character 
#           values.
#   week    Week number. Epidemiological week numbers with numeric values of
#           1—53.
#   weekday Weekday number. Epidemiological weekday numbers with numeric values
#           of 1—7. Defaults to `1`, the first day of the epidemiological week, 
#           which is a Monday if system = "who" or a Sunday if system = "cdc".
#   system  Either "who" or "cdc". WHO epidemiological weeks start on Monday. 
#           CDC epidemiological weeks (MMWR weeks) start on Sunday. The default 
#           is "who".
#
# Arguments year, week, and weekday are recycled as necessary. You can, for 
# example, give the function a single year and multiple week numbers.
#
# The function returns a vector of class `Date`.
#
# The inverse, calculating epidemiological week number and year from a date, can
# be done using two functions in the lubridate package, isoweek() and isoyear().
# Note that these functions only returns values based on the WHO implementation 
# of epidemiological weeks, but not the CDC implementation.
#


parse_epiweek <- function(year, week, weekday = 1L, system = "who") {
  
  # check input values
  stopifnot(week %in% 1:53)
  stopifnot(weekday %in% 1:7)
  match.arg(system, c("who", "cdc"))
  
  # recycle arguments
  max_length <- max(length(year), length(week), length(weekday))
  y <- rep(year, length.out = max_length)
  w <- rep(week, length.out = max_length)
  d <- rep(weekday, length.out = max_length)
  
  epiwday <- function(x, system = "who") {
    match.arg(system, c("who", "cdc"))
    if (system == "who") {
      as.integer(lubridate::wday(x - 1))
    } else if (system == "cdc") {
      as.integer(lubridate::wday(x))
    }
  }
  
  # calculate output
  jan1 <- as.Date(paste0(y, "-01-01"))
  wday <- epiwday(jan1, system)
  to_add <- ifelse(wday <= 4, 1 - wday, 7 - wday + 1)
  wk1 <- jan1 + lubridate::duration(to_add, "days")
  day1 <- wk1 + (w - 1) * 7
  day1 + lubridate::duration(d - 1, "days")
  
}
