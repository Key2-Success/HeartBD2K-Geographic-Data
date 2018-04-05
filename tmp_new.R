# load packages
library(tidyr)
library(dplyr)
library(stringr)
library(rebus)
library(stringi)
library(reshape2)
library(readr)
library(stringi)
library(tidyverse)
library(xlsx)
library(leaflet)
library(ggmap)
library(rgdal)
library(ggplot2)
library(maps)
library(RColorBrewer)
library(plotly)
library(grDevices)

# load data
allCCR <- read.csv("AllCaseReports.csv", sep="\t", stringsAsFactors = FALSE)
usstates <- read.csv("US_States.csv", stringsAsFactors = FALSE)
countries <- read.csv("countries.csv", stringsAsFactors = FALSE)

######## Clem's code ########

# regex on usstates
tmp1 <- lapply(allInstitution[ , 2], function(y) names(which(sapply(usstates[ , 1], function(x) grepl(paste("\\<", x, "\\>", sep = ""), y, ignore.case = TRUE)))))
tmp1[sapply(tmp1, function(x) length(x) == 0)] <- NA
allInstitution$usstates <- sapply(tmp1, function(x) unlist(x)[1])

# regex on countries
tmp2 <- lapply(allInstitution[ , 2],
               function(y) names(which(
                 sapply(countries[ , 1], 
                        function(x) grepl(paste("\\<", x, "\\>", sep = ""),
                                          y, ignore.case = TRUE)))))

# NA for length of 0
tmp2[sapply(tmp2, function(x) length(x) == 0)] <- NA

# unlist
allInstitution$countries <- sapply(tmp2, function(x) unlist(x)[1])

# if both are NA
cond <- is.na(allInstitution$usstates) & is.na(allInstitution$countries)

# more USA regex
tmp3 <- lapply(allInstitution$Institution[cond & grepl("usa", allInstitution$Institution)], function(y) names(which(sapply(usstates[ , 2], function(x) grepl(paste("\\<", x, "\\>", sep = ""), y, ignore.case = TRUE)))))

# NA for length of 0
tmp3[sapply(tmp3, function(x) length(x) == 0)] <- NA

# more USA regex
allInstitution$usstates[cond & grepl("usa", allInstitution$Institution)] <- sapply(tmp3, function(x) unlist(x)[1])


######## Kitu's code ########

# work with just instititution dataframe
allCCR_clean <- as.data.frame(allCCR[ , 11])
names(allCCR_clean) <- "Institution"
allCCR_clean <- separate(allCCR_clean, 'Institution', paste("Institution", 1:2, sep = "_"), sep = ";", extra = "drop") # creates two variables; one for hospital and one for university 

# work with merged dataframe
merged <- cbind(allCCR_clean, allCCR)

# if 0, 1, or nan present in both institutions, declare it impossible to extract info from
merged$unavailable <- ifelse(nchar(merged$Institution_1) > 4 & nchar(merged$Institution_2) < 4, 1, 0)

# starting to string match
final2 <- merged
final2$countries <- ""
final2$usstates <- ""
final2$Institution_1 <- tolower(final2$Institution_1)
final2$Institution_2 <- tolower(final2$Institution_2)

# lower case the datasets so they matches with the case reports (which are all in lower case)
countries$Name <- tolower(countries$Name)
usstates$X1 <- tolower(usstates$X1)
usstates$X2 <- tolower(usstates$X2)

# add in all UKs
final2$countries <- ifelse(str_detect(string = final2$Institution_1, pattern = "\\buk\\b"), "United Kingdom", final2$countries)

# read in most_populous_cities dataframe
most_populous_US_cities <- read_csv("~/Kitu/College/Senior Year/Geographic/most populous US cities.csv", 
                                    col_names = FALSE)

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

# vector of all US states
states <- unique(US_cities$State)
states <- tolower(states)

# one for just institutions
final2$countries2 <- ""

# string match the countries
for(i in 1:nrow(final2))
{
  for (j in 1:nrow(countries))
  {
    final2$countries2[i] <- ifelse(str_detect(string = final2$Institution[i], pattern = paste0("\\b", countries$Name[j], "\\b")), countries$Name[j], final2$countries2[i])
  }
}

# one for just institutions
final2$usstates2 <- ""

# string match the US states
for(i in 1:nrow(final2))
{
  for (j in 1:nrow(usstates))
  {
    final2$usstates2[i] <- ifelse(str_detect(string = final2$Institution[i], pattern = paste0("\\b", usstates$X1[j], "\\b")), usstates$X1[j], final2$usstates2[i])
  }
}

# string match the top US cities
for(i in 1:nrow(final2))
{
  for (j in 1:nrow(US_cities))
  {
    final2$usstates2[i] <- ifelse(str_detect(string = final2$Institution[i], pattern = paste0("\\b", US_cities$City[j], "\\b")), US_cities$State[j], final2$usstates2[i])
  }
}

# string match country's capitals
country_capitals$CountryName <- tolower(country_capitals$CountryName)
country_capitals$CapitalName <- tolower(country_capitals$CapitalName)

for(i in 1:nrow(final2))
{
  for (j in 1:nrow(country_capitals))
  {
    final2$countries2[i] <- ifelse(str_detect(string = final2$Institution[i], pattern = paste0("\\b", country_capitals$CapitalName[j], "\\b")), country_capitals$CountryName[j], final2$countries2[i])
  }
}

# clean major global cities dataframe
major_global_cities <- major_global_cities[-1, c(2:3)]
major_global_cities$City <- stri_trans_tolower(major_global_cities$City)
major_global_cities$Country <- tolower(major_global_cities$Country)

# replace usa and uk with full form 
major_global_cities$Country <- stri_replace_all_regex(str = major_global_cities$Country, pattern = "usa", replacement = "united states")
major_global_cities$Country <- stri_replace_all_regex(str = major_global_cities$Country, pattern = "uk", replacement = "united kingdom")

# remove those with ( in the dataframe
a <- which(grepl("\\(", major_global_cities$City))
major_global_cities <- major_global_cities[-a, ]

# add in those with ( from dataframe
paranthesis <- data.frame("City" = c("pretoria", "bombay", "jiangxi", "halab", "tshwane", "mumbai", "fuzhou", "aleppo"), 
                          "Country" = c("south africa", "india", "china", "syria", "south africa", "india", "china", "syria"), 
                          stringsAsFactors = FALSE)
major_global_cities <- rbind(major_global_cities, paranthesis)

# string match the major global cities
for(i in 1:nrow(final2))
{
  for (j in 1:nrow(major_global_cities))
  {
      final2$countries2[i] <- ifelse(str_detect(string = final2$Institution[i], pattern = paste0("\\b", major_global_cities$City[j], "\\b")), major_global_cities$Country[j], final2$countries2[i])
  }
}

# read in state abbreviations file
state_abbreviations <- read_csv("~/Kitu/College/Senior Year/Geographic/state_abbreviations.csv")

# if state is abbreviated, make it full state
final2$usstates2 <- ifelse(final2$usstates2 %in% state_abbreviations$Abbreviation, state_abbreviations$State, final2$usstates2)

# if state is in the USA, change country name to USA
states <- tolower(states)
final2$countries2 <- ifelse(final2$usstates2 %in% states, "United States", final2$countries2)

# hardcode uk, usa, peking
final2$countries2 <- ifelse(grepl("uk", final2$Institution), "United Kingdom", final2$countries2)
final2$countries2 <- ifelse(grepl("usa", final2$Institution), "United States", final2$countries2)
final2$countries2 <- ifelse(grepl("peking", final2$Institution), "China", final2$countries2)

# update both_NA column
final2$both_NA <- ifelse((is.na(final2$usstates) & is.na(final2$countries)), 1, 0)

# how many case reports are impossible to extract from?
length(which(nchar(final2$Institution) < 4)) # how many institutions have less than 4 chars (0, 1, nan)?

# make all title case
final2$countries2 <- str_to_title(final2$countries2)
final2$usstates2 <- str_to_title(final2$usstates2)

# if in the US, add that as country
final2$countries2 <- ifelse(final2$usstates %in% states, "United States", final2$countries2)

# make dataframe of count occurence of all US states and of country counts
US_count <- as.data.frame(table(final2$usstates))
countries_count <- as.data.frame(table(final2$countries))

# merge country and US states dataframe
all_count <- rbind(countries_count, US_count)
all_count <- all_count[-which(all_count$Var1 == "United States"), ] # remove the USA row


######## Joel's code ########

# world and state map data
world <- map_data(map = "world")
state <- map_data(map = "state")

# add alaska and hawaii
alaska <- world[which(world$subregion == "Alaska"), ]
alaska[ , 6] <- NA
alaska$region <- "alaska"
state <- rbind(alaska, state)
alaska$group <- 100

hawaii <- world[which(world$subregion == "Hawaii"), ]
hawaii[ , 6] <- NA
hawaii$region <- "hawaii"
hawaii$group <- 99

state <- rbind(hawaii, state)
restWorld <- world[which(world$region != "USA"),]


######## Kitu's code ########

# save state and world dataframes
save(state, file = "state.Rdata")
save(world, file = "world.Rdata")

# merge state with US data
US_count$Var1 <- tolower(US_count$Var1) # lowercase just like in the state df
names(US_count)[1] <- "region" # rename just like in the state df
state <- left_join(state, US_count)

# merge world with countries data
names(countries_count)[1] <- "region" # rename just like in the world df
world <- left_join(world, countries_count)

# clean up
names(state)[5] <- "Region"
names(state)[7] <- "Frequency"
names(world)[5] <- "Region"
names(world)[7] <- "Frequency"
state$Region <- str_to_title(state$Region)

# make map of world
p1 <- ggplot(data = world, aes(x = long, y = lat, fill = Frequency, Region = Region, frequency = Frequency, group = group)) + geom_polygon(color = "white", show.legend = FALSE)

# combine map of world with US states map
p2 <- p1 + geom_polygon(data = state, color = "white" , aes(fill = Frequency)) + guides(fill = guide_legend()) + 
  ggtitle(label = "Frequency of Case Reports by Region") + theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "", y = "") +
  scale_fill_continuous(low = "lightblue", high = "darkblue", 
                        guide = "colorbar", na.value = "grey") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill = "white", colour = "white"), 
        axis.line = element_line(colour = "white"),
        axis.ticks = element_blank(), axis.text.x = element_blank(),
        axis.text.y = element_blank())

# plot it!
ggplotly(p2, tooltip = c("Region", "frequency"))


#scale_fill_distiller(palette = "Blues", trans = "reverse")

#scale_fill_continuous(low = "#b3ccfff", high = "#001a4d", 
                      #guide = "colorbar", na.value = "#cccccc")

# to do:
# add in institution_2 comparison
# capitals of countries
# major cities of countries
# country name in their language
# state abbreviations
