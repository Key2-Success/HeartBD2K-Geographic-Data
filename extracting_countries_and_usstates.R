# load packages
library(stringr)
library(readr)
library(reshape)
library(stringi)

# load all case reports
allCCR <- read.csv("AllCaseReports.csv", sep="\t", stringsAsFactors = FALSE)

# load supplementary data found online
most_populous_US_cities <- read_csv("most populous US cities.csv", col_names = FALSE)
countries <- read.csv("countries.csv", stringsAsFactors = FALSE)
usstates <- read.csv("US_States.csv", stringsAsFactors = FALSE, header = FALSE)
major_global_cities <- read_csv("major global cities.csv", col_names = TRUE)
country_capitals <- read_csv("country-capitals.csv", col_names = TRUE)

# ----- cleaning data ----- #

# only use relevant variables
data <- allCCR[ , c(2, 10:13, 19, 41:58, 63:64)]

# if institution doesn't contain enough info aka impossible to extract location (usually nan, 0, 1)
data$unavailable <- ifelse(nchar(allCCR$Institution) < 4, 1, 0)

# reorder variables for ease
data <- data[ , c(1, 28:29, 27, 3:4, 2, 6:8, 5, 9:26)]

# clean most populous US cities dataframe
US_cities <- most_populous_US_cities
US_cities$X1 <- str_replace_all(US_cities$X1, "<.*>", "")
US_cities$X1 <- substring(US_cities$X1, 2, nchar(US_cities$X1))
US_cities$X7 <- sub(pattern = " *\\(.*?\\) *", replacement = "", x = US_cities$X7)
US_cities$X7 <- substring(US_cities$X7, 1, nchar(US_cities$X7) - 2)
US_cities$X1[7] <- substr(US_cities$X1[7], 2, nchar(US_cities$X1[7])) # weird North Dakota exception
US_cities <- t(US_cities) # transpose
colnames(US_cities) <- US_cities[1, ] # rename headers
US_cities <- US_cities[-1, ]
US_cities <- melt(data = US_cities) # reshape from wide to long format
US_cities <- US_cities[ , -1]
names(US_cities) <- c("State", "City")
US_cities <- US_cities[complete.cases(US_cities), c(2, 1)]
US_cities$City <- as.character(US_cities$City)
US_cities$State <- as.character(US_cities$State)
remove(most_populous_US_cities)

# clean major global cities dataframe
major_global_cities <- major_global_cities[-1, c(2:3)]
major_global_cities$City <- stri_trans_tolower(major_global_cities$City)
major_global_cities$Country <- tolower(major_global_cities$Country)

# replace usa and uk with full form 
major_global_cities$Country <- stri_replace_all_regex(str = major_global_cities$Country, pattern = "\\busa\\b", replacement = "united states")
major_global_cities$Country <- stri_replace_all_regex(str = major_global_cities$Country, pattern = "\\buk\\b", replacement = "united kingdom")

# remove those with ( in the dataframe
a <- which(grepl("\\(", major_global_cities$City))
major_global_cities <- major_global_cities[-a, ]

# add in those with ( from dataframe
paranthesis <- data.frame("City" = c("pretoria", "bombay", "jiangxi", "halab", "tshwane", "mumbai", "fuzhou", "aleppo"), 
                          "Country" = c("south africa", "india", "china", "syria", "south africa", "india", "china", "syria"), 
                          stringsAsFactors = FALSE)
major_global_cities <- rbind(major_global_cities, paranthesis)


# clean country capitals dataframe
country_capitals <- country_capitals[, 1:2]

# start to string match location by making all strings lowercase to be consistent
data$Institution <- tolower(data$Institution)
countries$Name <- tolower(countries$Name)
countries$Code <- tolower(countries$Code)
usstates$V1 <- tolower(usstates$V1)
usstates$V2 <- tolower(usstates$V2)
US_cities$City <- tolower(US_cities$City)
US_cities$State <- tolower(US_cities$State)
country_capitals$CountryName <- tolower(country_capitals$CountryName)
country_capitals$CapitalName <- tolower(country_capitals$CapitalName)


# ----- string matching ----- #

# add in countries and us states variable in dataset...this is where the output will be
data$country <- ""
data$us_state <- ""

# string match the countries
for(i in 1:nrow(data))
{
  for (j in 1:nrow(countries))
  {
    data$country[i] <- ifelse(str_detect(string = data$Institution[i], pattern = paste0("\\b", countries$Name[j], "\\b")), countries$Name[j], data$country[i])
  }
}

# string match the US states
for(i in 1:nrow(data))
{
  for (j in 1:nrow(usstates))
  {
    data$us_state[i] <- ifelse(str_detect(string = data$Institution[i], pattern = paste0("\\b", usstates$V1[j], "\\b")), usstates$V1[j], data$us_state[i])
  }
}

# string match the top US cities
for(i in 1:nrow(data))
{
  for (j in 1:nrow(US_cities))
  {
    if (data$us_state[i] == "")
    {
      data$us_state[i] <- ifelse(str_detect(string = data$Institution[i], pattern = paste0("\\b", US_cities$City[j], "\\b")), US_cities$State[j], data$us_state[i])
    }
  }
}

# string match country capitals
for(i in 1:nrow(data))
{
  for (j in 1:nrow(country_capitals))
  {
    if (data$country[i] == "")
    {
      data$country[i] <- ifelse(str_detect(string = data$Institution[i], pattern = paste0("\\b", country_capitals$CapitalName[j], "\\b")), country_capitals$CountryName[j], data$country[i])
    }
  }
}

# string match the major global cities
for(i in 1:nrow(data))
{
  for (j in 1:nrow(major_global_cities))
  {
    if (data$country[i] == "")
    {
      data$country[i] <- ifelse(str_detect(string = data$Institution[i], pattern = paste0("\\b", major_global_cities$City[j], "\\b")), major_global_cities$Country[j], data$country[i])
    }
  }
}

# hardcode uk and usa abbreviations as well as mapping peking to China and others
data$country <- ifelse(str_detect(string = data$Institution, pattern = "\\buk\\b"), "United Kingdom", data$country)
data$country <- ifelse(grepl("\\busa\\b", data$Institution), "United States", data$country)
data$country <- ifelse(grepl("peking", data$Institution), "China", data$country)
data$us_state <- ifelse(grepl("stanford", data$Institution), "california", data$us_state)
data$us_state <- ifelse(grepl("palo alto", data$Institution), "california", data$us_state)
data$country <- ifelse(grepl("england", data$Institution), "United Kingdom", data$country)

# vector of all US states
states <- usstates$V1[1:50]

# if state is in the USA, change country name to USA
data$country <- ifelse(data$us_state %in% states, "United States", data$country)

# make all countries and states title case
data$country <- str_to_title(data$country)
data$us_state <- str_to_title(data$us_state)

# countries, us states, top us cities, top global cities, country capitals
