# DryScrape
Required packages: rvest, dplyr, RCurl

scrape.RData contains two functions. 

The first, todays.games(), returns a vector of nhl game IDs for a given season and date. 

The second, scrape(), returns two data frames (one play-by-play, one roster) for a given range of IDs, season and according to the
specification provided by the names boolean.

The season argument must be a string in the "yyyyyyyy" format, the date argument a string in the "m/d/yyyy" format, the IDs numeric, and names one of TRUE or FALSE.

names = TRUE will provoke all player values to take the form "FIRSTNAME.LASTNAME", names = FALSE will cause player values to exist as a "TEAM##" code.

DryScrape.R contains the complete code for both functions, as well as an example of scrape() usage.

scrape() example
scrape(season = "20152016", start = 20001, end = 20012, names = TRUE) # scrape games 20001-20012 from the 20152016 season, with names

todays.games() example
todays.games("20152016", "1/16/2016") # Provide IDs for games played on 1/16/2016 of the 20152016 season
