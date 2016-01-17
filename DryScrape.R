# Dry Scrape
# Last edited 1-16-2015
# Manny

# Load libraries
library(rvest)
library(dplyr)
library(RCurl)

########################################################################################################################################################################################################

todays.games <- function(season, date) {
  url <- paste("http://www.nhl.com/ice/schedulebyday.htm?date=", date, "&season=", season, sep = "")
  glist <- getURL(url, header = FALSE,
                  .opts = curlOptions(
                    referer = 'nhl.com',
                    verbose = TRUE,
                    header = TRUE,
                    followLocation = TRUE,
                    useragent = 'Mozilla/5.0 (Windows NT 6.3; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/43.0.2357.130 Safari/537.36'))
  
  # href=\"http://www.nhl.com/gamecenter/en/preview?id=2015020415\">
  gameids <- gsub("http://www.nhl.com/gamecenter/en/(preview|recap)[?]id=", "", unique(unlist(regmatches(glist, gregexpr("http://www.nhl.com/gamecenter/en/(preview|recap)[?]id=[0-9]+", glist)))))
  ids <- sort(substr(gameids, start = 6, stop = 10))
  return(ids)
}

scrape <- function(season, start, end, names) {
  
  pbp.list <- NULL
  roster.list <- NULL
  
  for (j in 1:((end - start) + 1)) {
  
    ########################################################################################################################################################################################################
    ########################## SCRAPE PLAY-BY-PLAY #########################################################################################################################################################
    ########################################################################################################################################################################################################
    
    # Define URL
    ID <- as.character((start - 1) + j)
    print(ID)
    url <- paste("http://www.nhl.com/scores/htmlreports/", season, "/PL0", ID, ".HTM", sep = "")
    
    url.text <- getURL(url, header = FALSE,
                       .opts = curlOptions(
                         referer = 'nhl.com',
                         verbose = TRUE,
                         header = TRUE,
                         followLocation = TRUE,
                         useragent = 'Mozilla/5.0 (Windows NT 6.3; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/43.0.2357.130 Safari/537.36'))
    
    # Create HTML object
    html <- read_html(url.text)
    
    # Scrape text
    all <- html_nodes(html, "td")
    body <- html_nodes(html, ".bborder")
    full.text <- html_text(all)
    body.text <- html_text(body)
    
    pbp.raw <- matrix(body.text, byrow = TRUE, ncol = 8) %>% data.frame() %>% filter(X2 != "Per")
    
    # Team list
    teamlist <- c("ANA", "ARI", "BOS", "BUF", "CAR", "CBJ",
                  "CGY", "CHI", "COL", "DAL", "DET", "EDM",
                  "FLA", "L.A", "MIN", "MTL", "N.J", "NSH",
                  "NYI", "NYR", "OTT", "PHI", "PIT", "S.J",
                  "STL", "T.B", "TOR", "VAN", "WPG", "WSH",
                  "PHX")
    
    # Get teams
    hometeam <- gsub(" On Ice", "", body.text[8])
    awayteam <- gsub(" On Ice", "", body.text[7])
    teams <- c(awayteam, hometeam)
    
    # Date, game and etc. data
    date <- gsub("^[a-zA-Z]*, ", "", full.text[grep("^[a-zA-Z]*, ", full.text)]) %>% as.Date(format = "%B %d, %Y") %>% first() %>% as.character()
    Date <- rep(date, time = length(pbp.raw$X1))
    
    Game.ID <- rep(ID, times = length(pbp.raw$X1))
    
    Home.Team <- rep(hometeam, times = length(pbp.raw$X1))
    Away.Team <- rep(awayteam, times = length(pbp.raw$X1))
    
    Duration <- rep(NA, times = length(pbp.raw$X1))
    
    # Parse time
    timemat <- data.frame(matrix(as.numeric(unlist(strsplit(as.character(pbp.raw$X4), ":"))), byrow = TRUE, ncol = 3))
    
    Seconds <- 1200*(as.numeric(pbp.raw$X2) - 1) + timemat$X1*60 + (timemat$X3 > 0)*(60 - timemat$X3)
    
    ## Parse On-ice
    stretch <- function(x) {
      t <- as.character(unlist(x))
      t2 <- list(c(t, rep(c(0, NA), times = (12 - (length(t)/2)))))
      return(t2)
    }
    
    # Away
    a.match <- regmatches(as.character(pbp.raw$X7), gregexpr("[0-9|A-Z]+", as.character(pbp.raw$X7)))
    a.new <- lapply(a.match, stretch)
    Away.On <- data.frame(matrix(unlist(a.new), byrow = TRUE, ncol = 24))
    colnames(Away.On) <- c("a1.num", "a1.pos", "a2.num", "a2.pos", "a3.num", "a3.pos", "a4.num", "a4.pos", "a5.num", "a5.pos", "a6.num", "a6.pos",
                           "a7.num", "a7.pos", "a8.num", "a8.pos", "a9.num", "a9.pos", "a10.num", "a10.pos", "a11.num", "a11.pos", "a12.num", "a12.pos")
    
    # Home
    h.match <- regmatches(as.character(pbp.raw$X8), gregexpr("[0-9|A-Z]+", as.character(pbp.raw$X8)))
    h.new <- lapply(h.match, stretch)
    Home.On <- data.frame(matrix(unlist(h.new), byrow = TRUE, ncol = 24))
    colnames(Home.On) <- c("h1.num", "h1.pos", "h2.num", "h2.pos", "h3.num", "h3.pos", "h4.num", "h4.pos", "h5.num", "h5.pos", "h6.num", "h6.pos", 
                           "h7.num", "h7.pos", "h8.num", "h8.pos", "h9.num", "h9.pos", "h10.num", "h10.pos","h11.num", "h11.pos", "h12.num", "h12.pos")
    
    ## Parse Description
    clean.nums <- function(x) {
      t <- gsub("#", "", as.character(unlist(x)))
      t2 <- list(c(t, rep(NA, times = (3 - length(t)))))
      return(t2)
    }
    
    dummy.team <- function(x) {
      if (length(unlist(x)) > 0) {
        t <- x
      } else {
        t <- NA
      }
      return(t)
    }
    
    dummy.zone <- function(x) {
      if (length(unlist(x)) > 0) {
        t <- x
      } else {
        t <- NA
      }
      return(t)
    }
    
    dummy.detail <- function(x) {
      if (length(unlist(x)) > 0) {
        t <- paste(unlist(x), collapse = "")
      } else {
        t <- NA
      }
      return(t)
    }
    
    # Event team
    t.match <- regmatches(as.character(pbp.raw$X6), gregexpr(paste("(^", paste(teamlist, collapse = "|^"), ")", sep = ""), as.character(pbp.raw$X6)))
    t.new <- lapply(t.match, dummy.team)
    ev.team <- gsub(" ", "", as.character(unlist(t.new)))
    
    # Event players
    d.match <- regmatches(as.character(pbp.raw$X6), gregexpr("#[0-9]+", as.character(pbp.raw$X6)))
    d.new <- lapply(d.match, clean.nums)
    ev.players <- data.frame(matrix(unlist(d.new), byrow = TRUE, ncol = 3))
    colnames(ev.players) <- c("p1", "p2", "p3")
    
    # Event zone
    z.match <- regmatches(as.character(pbp.raw$X6), gregexpr("[a-zA-Z]{3}. [zZ]one", as.character(pbp.raw$X6)))
    z.new <- lapply(z.match, dummy.zone)
    ev.zone <- gsub(". [zZ]one", "", as.character(unlist(z.new)))
    
    # Event details
    e.match <- regmatches(as.character(pbp.raw$X6), gregexpr(", [a-zA-Z|-]+,|[A-Z] .+[(].{4,}[)],|[A-Z] .+[(][a-zA-Z]{3,}[)],", as.character(pbp.raw$X6)))
    e.new <- lapply(e.match, dummy.detail)
    Detail <- gsub(",|, |[A-Z]+ |#[0-9]+ |[A-Z]{2,}.", "", as.character(unlist(e.new)))
    
    # On-ice goalies
    Home.Goalie <- (Home.On$h12.pos == "G" & !is.na(Home.On$h12.pos))*as.numeric(as.character(Home.On$h12.num)) + (Home.On$h11.pos == "G" & !is.na(Home.On$h11.pos))*as.numeric(as.character(Home.On$h11.num)) +
      (Home.On$h10.pos == "G" & !is.na(Home.On$h10.pos))*as.numeric(as.character(Home.On$h10.num)) + (Home.On$h9.pos == "G" & !is.na(Home.On$h9.pos))*as.numeric(as.character(Home.On$h9.num)) +
      (Home.On$h8.pos == "G" & !is.na(Home.On$h8.pos))*as.numeric(as.character(Home.On$h8.num)) + (Home.On$h7.pos == "G" & !is.na(Home.On$h7.pos))*as.numeric(as.character(Home.On$h7.num)) +
      (Home.On$h6.pos == "G" & !is.na(Home.On$h6.pos))*as.numeric(as.character(Home.On$h6.num)) + (Home.On$h5.pos == "G" & !is.na(Home.On$h5.pos))*as.numeric(as.character(Home.On$h5.num)) +
      (Home.On$h4.pos == "G" & !is.na(Home.On$h4.pos))*as.numeric(as.character(Home.On$h4.num)) + (Home.On$h3.pos == "G" & !is.na(Home.On$h3.pos))*as.numeric(as.character(Home.On$h3.num)) +
      (Home.On$h2.pos == "G" & !is.na(Home.On$h2.pos))*as.numeric(as.character(Home.On$h2.num)) + (Home.On$h1.pos == "G" & !is.na(Home.On$h1.pos))*as.numeric(as.character(Home.On$h1.num))
    
    Away.Goalie <- (Away.On$a12.pos == "G" & !is.na(Away.On$a12.pos))*as.numeric(as.character(Away.On$a12.num)) + (Away.On$a11.pos == "G" & !is.na(Away.On$a11.pos))*as.numeric(as.character(Away.On$a11.num)) +
      (Away.On$a10.pos == "G" & !is.na(Away.On$a10.pos))*as.numeric(as.character(Away.On$a10.num)) + (Away.On$a9.pos == "G" & !is.na(Away.On$a9.pos))*as.numeric(as.character(Away.On$a9.num)) +
      (Away.On$a8.pos == "G" & !is.na(Away.On$a8.pos))*as.numeric(as.character(Away.On$a8.num)) + (Away.On$a7.pos == "G" & !is.na(Away.On$a7.pos))*as.numeric(as.character(Away.On$a7.num)) +
      (Away.On$a6.pos == "G" & !is.na(Away.On$a6.pos))*as.numeric(as.character(Away.On$a6.num)) + (Away.On$a5.pos == "G" & !is.na(Away.On$a5.pos))*as.numeric(as.character(Away.On$a5.num)) +
      (Away.On$a4.pos == "G" & !is.na(Away.On$a4.pos))*as.numeric(as.character(Away.On$a4.num)) + (Away.On$a3.pos == "G" & !is.na(Away.On$a3.pos))*as.numeric(as.character(Away.On$a3.num)) +
      (Away.On$a2.pos == "G" & !is.na(Away.On$a2.pos))*as.numeric(as.character(Away.On$a2.num)) + (Away.On$a1.pos == "G" & !is.na(Away.On$a1.pos))*as.numeric(as.character(Away.On$a1.num))
    
    # Create PBP
    pbp.new <- pbp.raw %>% select(-c(X1, X3, X4, X7, X8)) %>% cbind(Duration, Date, Game.ID, ev.team, ev.players, ev.zone, Detail, Seconds, Away.On[, 1:12], Home.On[, 1:12], Away.Team, Home.Team, Away.Goalie, Home.Goalie)
    
    ## Replace with teamnum ID
    pbp.new <- rbind_list(
      filter(pbp.new, X5 == "FAC") %>% 
        mutate(p1 = paste(awayteam, p1, sep = ""), p2 = paste(hometeam, p2, sep = "")) %>% data.frame(),
      filter(pbp.new, X5 == "HIT") %>% group_by(ev.team) %>%
        mutate(p1 = paste(first(ev.team), p1, sep = ""), p2 = paste(teams[which(teams != first(ev.team))], p2, sep = "")) %>% data.frame(),
      filter(pbp.new, X5 == "SHOT") %>%
        mutate(p1 = paste(ev.team, p1, sep = "")) %>% data.frame(),
      filter(pbp.new, X5 == "GIVE") %>%
        mutate(p1 = paste(ev.team, p1, sep = "")) %>% data.frame(),
      filter(pbp.new, X5 == "MISS") %>%
        mutate(p1 = paste(ev.team, p1, sep = "")) %>% data.frame(),
      filter(pbp.new, X5 == "GOAL") %>% 
        mutate(p1 = paste(ev.team, p1, sep = ""), 
               p2 = gsub(paste(paste(teamlist, collapse = "NA|"), "NA", sep = ""), NA, paste(ev.team, p2, sep = "")),
               p3 = gsub(paste(paste(teamlist, collapse = "NA|"), "NA", sep = ""), NA, paste(ev.team, p3, sep = ""))) %>% data.frame(),
      filter(pbp.new, X5 == "BLOCK") %>% group_by(ev.team) %>%
        mutate(p1 = paste(first(ev.team), p1, sep = ""), p2 = paste(teams[which(teams != first(ev.team))], p2, sep = "")) %>% data.frame(),
      filter(pbp.new, X5 == "PENL") %>% group_by(ev.team) %>%
        mutate(p1 = paste(first(ev.team), p1, sep = ""), 
               p2 = gsub(paste(paste(teamlist, collapse = "NA|"), "NA", sep = ""), NA, paste(teams[which(teams != first(ev.team))], p2, sep = ""))) %>% data.frame(),
      filter(pbp.new, X5 == "TAKE") %>% 
        mutate(p1 = paste(ev.team, p1, sep = "")) %>% data.frame(),
      filter(pbp.new, X5 %in% c("FAC", "HIT", "SHOT", "GIVE", "MISS", "GOAL", "BLOCK", "PENL", "TAKE") == FALSE) %>% data.frame()
    ) %>% data.frame() %>% 
      mutate(a1.num = gsub(paste(paste(teamlist, collapse = "0|"), "0", sep = ""), NA, paste(awayteam, a1.num, sep = "")),
             a2.num = gsub(paste(paste(teamlist, collapse = "0|"), "0", sep = ""), NA, paste(awayteam, a2.num, sep = "")),
             a3.num = gsub(paste(paste(teamlist, collapse = "0|"), "0", sep = ""), NA, paste(awayteam, a3.num, sep = "")),
             a4.num = gsub(paste(paste(teamlist, collapse = "0|"), "0", sep = ""), NA, paste(awayteam, a4.num, sep = "")),
             a5.num = gsub(paste(paste(teamlist, collapse = "0|"), "0", sep = ""), NA, paste(awayteam, a5.num, sep = "")),
             a6.num = gsub(paste(paste(teamlist, collapse = "0|"), "0", sep = ""), NA, paste(awayteam, a6.num, sep = "")),
             h1.num = gsub(paste(paste(teamlist, collapse = "0|"), "0", sep = ""), NA, paste(hometeam, h1.num, sep = "")),
             h2.num = gsub(paste(paste(teamlist, collapse = "0|"), "0", sep = ""), NA, paste(hometeam, h2.num, sep = "")),
             h3.num = gsub(paste(paste(teamlist, collapse = "0|"), "0", sep = ""), NA, paste(hometeam, h3.num, sep = "")),
             h4.num = gsub(paste(paste(teamlist, collapse = "0|"), "0", sep = ""), NA, paste(hometeam, h4.num, sep = "")),
             h5.num = gsub(paste(paste(teamlist, collapse = "0|"), "0", sep = ""), NA, paste(hometeam, h5.num, sep = "")),
             h6.num = gsub(paste(paste(teamlist, collapse = "0|"), "0", sep = ""), NA, paste(hometeam, h6.num, sep = "")),
             Home.Goalie = gsub(paste(paste(teamlist, collapse = "0|"), "0", sep = ""), NA, paste(hometeam, Home.Goalie, sep = "")),
             Away.Goalie = gsub(paste(paste(teamlist, collapse = "0|"), "0", sep = ""), NA, paste(awayteam, Away.Goalie, sep = "")),
             Home.Skaters = 6 - (is.na(h1.num)) - (is.na(h2.num)) - (is.na(h3.num)) - (is.na(h4.num)) - (is.na(h5.num)) - (is.na(h6.num)) - (!is.na(Home.Goalie)),
             Away.Skaters = 6 - (is.na(a1.num)) - (is.na(a2.num)) - (is.na(a3.num)) - (is.na(a4.num)) - (is.na(a5.num)) - (is.na(a6.num)) - (!is.na(Away.Goalie)),
             Seconds = Seconds - 0.01*(X5 %in% c("STOP", "PENL", "GOAL", "PEND")) + 0.01*(X5 == "FAC")) %>%
      rename(Period = X2, Event = X5, Description = X6) %>% arrange(Seconds) %>%
      mutate(Home.Score = cumsum(as.character(Event) == "GOAL" & as.character(ev.team) == as.character(Home.Team)) - 1*(as.character(Event) == "GOAL" & as.character(ev.team) == as.character(Home.Team)),
             Away.Score = cumsum(as.character(Event) == "GOAL" & as.character(ev.team) == as.character(Away.Team)) - 1*(as.character(Event) == "GOAL" & as.character(ev.team) == as.character(Away.Team))) %>%
      data.frame()
    
    # Re-assign event zone for blocked shots to perspective of shooting team
    pbp.new$ev.zone[which(pbp.new$Event == "BLOCK" & pbp.new$ev.zone == "Def")] <- "Off"
    
    # Append strength and score states
    pbp.new$Strength.State <- paste(pbp.new$Home.Skaters, pbp.new$Away.Skaters, sep = "v"); pbp.new$Score.State <- paste(pbp.new$Home.Score, pbp.new$Away.Score, sep = "-")
    pbp.new$Score.Cat <- 1*(pbp.new$Home.Score - pbp.new$Away.Score == 1) + 2*(pbp.new$Home.Score - pbp.new$Away.Score == 2) + 3*(pbp.new$Home.Score - pbp.new$Away.Score >= 3) -
                         1*(pbp.new$Home.Score - pbp.new$Away.Score == -1) - 2*(pbp.new$Home.Score - pbp.new$Away.Score == -2) - 3*(pbp.new$Home.Score - pbp.new$Away.Score <= -3)
    
    ########################################################################################################################################################################################################
    ########################## SCRAPE SHIFT REPORTS ########################################################################################################################################################
    ########################################################################################################################################################################################################
    
    # Define URLs
    url1 <- paste("http://www.nhl.com/scores/htmlreports/", season, "/TH0", ID, ".HTM", sep = "") # Home
    url2 <- paste("http://www.nhl.com/scores/htmlreports/", season, "/TV0", ID, ".HTM", sep = "") # Away
    
    url1.text <- getURL(url1, header = FALSE,
                       .opts = curlOptions(
                         referer = 'nhl.com',
                         verbose = TRUE,
                         header = TRUE,
                         followLocation = TRUE,
                         useragent = 'Mozilla/5.0 (Windows NT 6.3; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/43.0.2357.130 Safari/537.36'))
    
    url2.text <- getURL(url2, header = FALSE,
                       .opts = curlOptions(
                         referer = 'nhl.com',
                         verbose = TRUE,
                         header = TRUE,
                         followLocation = TRUE,
                         useragent = 'Mozilla/5.0 (Windows NT 6.3; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/43.0.2357.130 Safari/537.36'))
    
    # Create HTML objects
    html1 <- read_html(url1.text) # Home
    html2 <- read_html(url2.text) # Away
    
    # Scrape tables
    home.text.1 <- html_nodes(html1, ".border")
    away.text.1 <- html_nodes(html2, ".border")
    home.text.2 <- html_nodes(html1, ".bborder")
    away.text.2 <- html_nodes(html2, ".bborder")
    
    home.outer <- html_text(home.text.1)
    away.outer <- html_text(away.text.1)
    home.inner <- html_text(home.text.2)
    away.inner <- html_text(away.text.2)
    
    hometeam.full <- home.outer[1]
    home.players <- home.outer[-1]
    home.players <- home.players[which(grepl("^[0-9]+", home.players) == TRUE)] # FIX FOR 20132014-20934
    awayteam.full <- away.outer[1]
    away.players <- away.outer[-1]
    away.players <- away.players[which(grepl("^[0-9]+", away.players) == TRUE)] # FIX FOR 20132014-20934
    
    # Create roster table
    roster <- rbind_list(cbind(rep(hometeam, times = length(home.players)), home.players) %>% data.frame() %>% rename(Num.Last.First = home.players),
                         cbind(rep(awayteam, times = length(away.players)), away.players) %>% data.frame() %>% rename(Num.Last.First = away.players)) %>%
      data.frame()
    
    namemat <- data.frame(matrix(as.character(unlist(strsplit(gsub("^[0-9]+ ", "", roster$Num.Last.First), ","))), byrow = T, ncol = 2))
    
    roster$Game.ID <- rep(ID, times = length(roster$Num.Last.First))
    roster$Date <- rep(date, times = length(roster$Num.Last.First))
    roster$Number <- unlist(regmatches(as.character(roster$Num.Last.First), gregexpr("^[0-9]+", as.character(roster$Num.Last.First))))
    roster$Last.Name <- namemat$X1
    roster$First.Name <- namemat$X2
    
    posmatch <- rbind_list(group_by(pbp.new, a1.num) %>% rename(player = a1.num) %>% 
                             summarise(C = sum(a1.pos == "C"), L = sum(a1.pos == "L"), R = sum(a1.pos == "R"), D = sum(a1.pos == "D"), G = sum(a1.pos == "G"), N = sum(a1.pos %in% c("C", "L", "R", "D", "G") == F)),
                           group_by(pbp.new, a2.num) %>% rename(player = a2.num) %>% 
                             summarise(C = sum(a2.pos == "C"), L = sum(a2.pos == "L"), R = sum(a2.pos == "R"), D = sum(a2.pos == "D"), G = sum(a2.pos == "G"), N = sum(a2.pos %in% c("C", "L", "R", "D", "G") == F)),
                           group_by(pbp.new, a3.num) %>% rename(player = a3.num) %>% 
                             summarise(C = sum(a3.pos == "C"), L = sum(a3.pos == "L"), R = sum(a3.pos == "R"), D = sum(a3.pos == "D"), G = sum(a3.pos == "G"), N = sum(a3.pos %in% c("C", "L", "R", "D", "G") == F)),
                           group_by(pbp.new, a4.num) %>% rename(player = a4.num) %>% 
                             summarise(C = sum(a4.pos == "C"), L = sum(a4.pos == "L"), R = sum(a4.pos == "R"), D = sum(a4.pos == "D"), G = sum(a4.pos == "G"), N = sum(a4.pos %in% c("C", "L", "R", "D", "G") == F)),
                           group_by(pbp.new, a5.num) %>% rename(player = a5.num) %>% 
                             summarise(C = sum(a5.pos == "C"), L = sum(a5.pos == "L"), R = sum(a5.pos == "R"), D = sum(a5.pos == "D"), G = sum(a5.pos == "G"), N = sum(a5.pos %in% c("C", "L", "R", "D", "G") == F)),
                           group_by(pbp.new, a6.num) %>% rename(player = a6.num) %>% 
                             summarise(C = sum(a6.pos == "C"), L = sum(a6.pos == "L"), R = sum(a6.pos == "R"), D = sum(a6.pos == "D"), G = sum(a6.pos == "G"), N = sum(a6.pos %in% c("C", "L", "R", "D", "G") == F)),
                           group_by(pbp.new, h1.num) %>% rename(player = h1.num) %>% 
                             summarise(C = sum(h1.pos == "C"), L = sum(h1.pos == "L"), R = sum(h1.pos == "R"), D = sum(h1.pos == "D"), G = sum(h1.pos == "G"), N = sum(h1.pos %in% c("C", "L", "R", "D", "G") == F)),
                           group_by(pbp.new, h2.num) %>% rename(player = h2.num) %>% 
                             summarise(C = sum(h2.pos == "C"), L = sum(h2.pos == "L"), R = sum(h2.pos == "R"), D = sum(h2.pos == "D"), G = sum(h2.pos == "G"), N = sum(h2.pos %in% c("C", "L", "R", "D", "G") == F)),
                           group_by(pbp.new, h3.num) %>% rename(player = h3.num) %>% 
                             summarise(C = sum(h3.pos == "C"), L = sum(h3.pos == "L"), R = sum(h3.pos == "R"), D = sum(h3.pos == "D"), G = sum(h3.pos == "G"), N = sum(h3.pos %in% c("C", "L", "R", "D", "G") == F)),
                           group_by(pbp.new, h4.num) %>% rename(player = h4.num) %>% 
                             summarise(C = sum(h4.pos == "C"), L = sum(h4.pos == "L"), R = sum(h4.pos == "R"), D = sum(h4.pos == "D"), G = sum(h4.pos == "G"), N = sum(h4.pos %in% c("C", "L", "R", "D", "G") == F)),
                           group_by(pbp.new, h5.num) %>% rename(player = h5.num) %>% 
                             summarise(C = sum(h5.pos == "C"), L = sum(h5.pos == "L"), R = sum(h5.pos == "R"), D = sum(h5.pos == "D"), G = sum(h5.pos == "G"), N = sum(h5.pos %in% c("C", "L", "R", "D", "G") == F)),
                           group_by(pbp.new, h6.num) %>% rename(player = h6.num) %>% 
                             summarise(C = sum(h6.pos == "C"), L = sum(h6.pos == "L"), R = sum(h6.pos == "R"), D = sum(h6.pos == "D"), G = sum(h6.pos == "G"), N = sum(h6.pos %in% c("C", "L", "R", "D", "G") == F))) %>%
      data.frame() %>% group_by(player) %>%
      summarise(C = sum(C), L = sum(L), R = sum(R), D = sum(D), G = sum(G), N = sum(N)) %>% 
      mutate(Pos.Num = 1*(C > L & C > R & C > D & C > G & C > N) +
               2*(L > C & L > R & L > D & L > G & L > N) +
               3*(R > L & R > C & R > D & R > G & R > N) +
               4*(D > L & D > R & D > C & D > G & D > N) +
               5*(G > C & G > L & G > R & G > D & G > N) +
               6*(N > C & N > L & N > R & N > D & N > G)) %>%
      data.frame()
    
    posmatch$Pos <- colnames(posmatch)[-1][posmatch$Pos.Num[1:nrow(posmatch)]]
    
    roster <- roster %>% mutate(Team.Num = paste(V1, Number, sep = ""),
                                Full.Name = paste(First.Name, Last.Name, sep = "."),
                                Position = posmatch$Pos[match(Team.Num, posmatch$player)]) %>%
      rename(Team = V1) %>% data.frame()
    
    # Create shift tables
    shiftlist.home <- NULL
    shiftlist.away <- NULL
    
    for (i in 1:(length(home.outer)-1)) {
      shiftlist.home[[i]] <- home.inner[which(home.inner == "Shift #" | home.inner == "Présence #Shift #")[i]:(which(home.inner == "SHF" | home.inner == "PR/SHF")[i]-3)]
    }
    
    for (i in 1:(length(away.outer)-1)) {
      shiftlist.away[[i]] <- away.inner[which(away.inner == "Shift #" | away.inner == "Présence #Shift #")[i]:(which(away.inner == "SHF" | away.inner == "PR/SHF")[i]-3)]
    }
    
    htoi.raw <- matrix(unlist(shiftlist.home), byrow = TRUE, ncol = 6) %>% data.frame()
    atoi.raw <- matrix(unlist(shiftlist.away), byrow = TRUE, ncol = 6) %>% data.frame()
    
    htoi.raw$p.match <- cumsum(htoi.raw$X2 == "Per")
    htoi.raw$Player <- home.players[htoi.raw$p.match[1:nrow(htoi.raw)]]
    htoi.raw <- filter(htoi.raw, X2 != "Per")
    
    atoi.raw$p.match <- cumsum(atoi.raw$X2 == "Per")
    atoi.raw$Player <- away.players[atoi.raw$p.match[1:nrow(atoi.raw)]]
    atoi.raw <- filter(atoi.raw, X2 != "Per")
    
    startmat.home <- data.frame(matrix(as.numeric(unlist(strsplit(unlist(strsplit(as.character(htoi.raw$X3), " ")), ":"))), byrow = TRUE, ncol = 5))
    endmat.home <- data.frame(matrix(as.numeric(unlist(strsplit(unlist(strsplit(as.character(htoi.raw$X4), " ")), ":"))), byrow = TRUE, ncol = 5))
    startmat.away <- data.frame(matrix(as.numeric(unlist(strsplit(unlist(strsplit(as.character(atoi.raw$X3), " ")), ":"))), byrow = TRUE, ncol = 5))
    endmat.away <- data.frame(matrix(as.numeric(unlist(strsplit(unlist(strsplit(as.character(atoi.raw$X4), " ")), ":"))), byrow = TRUE, ncol = 5))
    
    startsec.home <- 1200*(as.numeric(htoi.raw$X2) - 1) + startmat.home$X1*60 + startmat.home$X2
    endsec.home <- 1200*(as.numeric(htoi.raw$X2) - 1) + endmat.home$X1*60 + endmat.home$X2
    startsec.away <- 1200*(as.numeric(atoi.raw$X2) - 1) + startmat.away$X1*60 + startmat.away$X2
    endsec.away <- 1200*(as.numeric(atoi.raw$X2) - 1) + endmat.away$X1*60 + endmat.away$X2
    
    htoi.new <- htoi.raw %>% select(-c(X1, X3:X6, p.match)) %>% cbind(roster[match(htoi.raw$Player, roster$Num.Last.First), c(3,4,1,5,8,9)], startsec.home, endsec.home) %>%
      mutate(Duration = endsec.home - startsec.home) %>% data.frame()
    atoi.new <- atoi.raw %>% select(-c(X1, X3:X6, p.match)) %>% cbind(roster[match(atoi.raw$Player, roster$Num.Last.First), c(3,4,1,5,8,9)], startsec.away, endsec.away) %>% 
      mutate(Duration = endsec.away - startsec.away) %>% data.frame()
    
    colnames(htoi.new) <- c("Period", "Num.Last.First", "Game.ID", "Date", "Team", "Num", "Team.Num", "Full.Name", "Start.Seconds", "End.Seconds", "Duration")
    colnames(atoi.new) <- c("Period", "Num.Last.First", "Game.ID", "Date", "Team", "Num", "Team.Num", "Full.Name", "Start.Seconds", "End.Seconds", "Duration")
    
    shift.on <- rbind_list(htoi.new %>% select(Period, Start.Seconds, Game.ID, Date, Team.Num, Duration, Team) %>% rename(Seconds = Start.Seconds, p1 = Team.Num, ev.team = Team) %>% 
                             mutate(Event = "ON", Description = NA, p2 = NA, p3 = NA, ev.zone = NA, Detail = NA, Away.Team = NA, Home.Team = NA,
                                    Away.Goalie = NA, Home.Goalie = NA, Strength.State = NA, Score.State = NA),
                           atoi.new %>% select(Period, Start.Seconds, Game.ID, Date, Team.Num, Duration, Team) %>% rename(Seconds = Start.Seconds, p1 = Team.Num, ev.team = Team) %>% 
                             mutate(Event = "ON", Description = NA, p2 = NA, p3 = NA, ev.zone = NA, Detail = NA, Away.Team = NA, Home.Team = NA,
                                    Away.Goalie = NA, Home.Goalie = NA, Strength.State = NA, Score.State = NA)) %>% data.frame()
    
    shift.off <- rbind_list(htoi.new %>% select(Period, End.Seconds, Game.ID, Date, Team.Num, Duration, Team) %>% rename(Seconds = End.Seconds, p1 = Team.Num, ev.team = Team) %>% 
                              mutate(Event = "OFF", Description = NA, p2 = NA, p3 = NA, ev.zone = NA, Detail = NA, Away.Team = NA, Home.Team = NA,
                                     Away.Goalie = NA, Home.Goalie = NA, Strength.State = NA, Score.State = NA),
                            atoi.new %>% select(Period, End.Seconds, Game.ID, Date, Team.Num, Duration, Team) %>% rename(Seconds = End.Seconds, p1 = Team.Num, ev.team = Team) %>% 
                              mutate(Event = "OFF", Description = NA, p2 = NA, p3 = NA, ev.zone = NA, Detail = NA, Away.Team = NA, Home.Team = NA,
                                     Away.Goalie = NA, Home.Goalie = NA, Strength.State = NA, Score.State = NA)) %>% data.frame()
    
    who.on.1 <- function(x) {
      n <- htoi.new$Team.Num[which(as.numeric(htoi.new$Start.Seconds) <= as.numeric(x) & as.numeric(htoi.new$End.Seconds) > as.numeric(x))]
      n2 <- c(n, rep(NA, times = (12 - length(n))))
      p <- roster$Position[match(n2, roster$Team.Num)]
      on.home <- c(n2, p)
      return(on.home)
    }
    
    who.off.1 <- function(x) {
      n <- htoi.new$Team.Num[which(as.numeric(htoi.new$Start.Seconds) < as.numeric(x) & as.numeric(htoi.new$End.Seconds) > as.numeric(x))]
      n2 <- c(n, rep(NA, times = (12 - length(n))))
      p <- roster$Position[match(n2, roster$Team.Num)]
      off.home <- c(n2, p)
      return(off.home)
    }
    
    who.on.2 <- function(x) {
      n <- atoi.new$Team.Num[which(as.numeric(atoi.new$Start.Seconds) <= as.numeric(x) & as.numeric(atoi.new$End.Seconds) > as.numeric(x))]
      n2 <- c(n, rep(NA, times = (12 - length(n))))
      p <- roster$Position[match(n2, roster$Team.Num)]
      on.away <- c(n2, p)
      return(on.away)
    }
    
    who.off.2 <- function(x) {
      n <- atoi.new$Team.Num[which(as.numeric(atoi.new$Start.Seconds) < as.numeric(x) & as.numeric(atoi.new$End.Seconds) > as.numeric(x))]
      n2 <- c(n, rep(NA, times = (12 - length(n))))
      p <- roster$Position[match(n2, roster$Team.Num)]
      off.away <- c(n2, p)
      return(off.away)
    }
    
    on.home <- lapply(shift.on$Seconds, who.on.1) %>% unlist() %>% matrix(byrow = TRUE, ncol = 24) %>% data.frame() %>% 
      rename(h1.num = X1, h2.num = X2, h3.num = X3, h4.num = X4, h5.num = X5, h6.num = X6, h1.pos = X13, h2.pos = X14, h3.pos = X15, h4.pos = X16, h5.pos = X17, h6.pos = X18)
    off.home <- lapply(shift.off$Seconds, who.off.1) %>% unlist() %>% matrix(byrow = TRUE, ncol = 24) %>% data.frame() %>% 
      rename(h1.num = X1, h2.num = X2, h3.num = X3, h4.num = X4, h5.num = X5, h6.num = X6, h1.pos = X13, h2.pos = X14, h3.pos = X15, h4.pos = X16, h5.pos = X17, h6.pos = X18)
    on.away <- lapply(shift.on$Seconds, who.on.2) %>% unlist() %>% matrix(byrow = TRUE, ncol = 24) %>% data.frame() %>% 
      rename(a1.num = X1, a2.num = X2, a3.num = X3, a4.num = X4, a5.num = X5, a6.num = X6, a1.pos = X13, a2.pos = X14, a3.pos = X15, a4.pos = X16, a5.pos = X17, a6.pos = X18)
    off.away <- lapply(shift.off$Seconds, who.off.2) %>% unlist() %>% matrix(byrow = TRUE, ncol = 24) %>% data.frame() %>% 
      rename(a1.num = X1, a2.num = X2, a3.num = X3, a4.num = X4, a5.num = X5, a6.num = X6, a1.pos = X13, a2.pos = X14, a3.pos = X15, a4.pos = X16, a5.pos = X17, a6.pos = X18)
    
    shift.on <- cbind(shift.on, on.home[,c(1:6, 13:18)], on.away[,c(1:6, 13:18)]) %>% data.frame()
    shift.off <- cbind(shift.off, off.home[,c(1:6, 13:18)], off.away[,c(1:6, 13:18)]) %>% data.frame()
    
    check <- pbp.new %>% filter(Event == "FAC") %>% mutate(Event = "CHECK", Seconds = Seconds - 0.011, Description = "Checkpoint") %>% data.frame()
    
    pbp.new <- rbind_list(pbp.new, shift.on, shift.off, check) %>% arrange(Seconds) %>% 
      mutate(event.ref = cumsum(Event %in% c("ON", "OFF") == F)) %>% group_by(event.ref) %>%
      mutate(Away.Team = first(Away.Team), Home.Team = first(Home.Team), Away.Goalie = first(Away.Goalie), Home.Goalie = first(Home.Goalie),
             Home.Skaters = first(Home.Skaters), Away.Skaters = first(Away.Skaters), Strength.State = first(Strength.State), 
             Home.Score = first(Home.Score), Away.Score = first(Away.Score), Score.State = first(Score.State), Score.Cat = first(Score.Cat),
             Seconds = Seconds - 0.001*(Event == "OFF") + 0.001*(Event == "ON")) %>% 
      filter(Event != "CHECK") %>% arrange(Seconds) %>% data.frame() %>% select(-c(event.ref)) %>% data.frame() 
    
    pbp.new$Event.Length <- c((pbp.new$Seconds[2:nrow(pbp.new)] - pbp.new$Seconds[1:(nrow(pbp.new) - 1)]), 0)
    
    ########################################################################################################################################################################################################
    ########################## SCRAPE HIGHLIGHTS JSON ######################################################################################################################################################
    ########################################################################################################################################################################################################
    
    # Define URL
    year <- substr(season, start = 1, stop = 4)
    url3 <- paste("http://live.nhle.com/GameData/", season, "/", year, "0", ID, "/gc/gcgm.jsonp", sep = "")
    
    full.text.3 <- getURL(url3, header = FALSE,
                       .opts = curlOptions(
                         referer = 'nhl.com',
                         verbose = TRUE,
                         header = TRUE,
                         followLocation = TRUE,
                         useragent = 'Mozilla/5.0 (Windows NT 6.3; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/43.0.2357.130 Safari/537.36'))
    
    text.3 <- unlist(strsplit(full.text.3, ","))
    
    hl.presecs <- gsub("sip[\":]*", "", unlist(regmatches(text.3, gregexpr("sip[\":]*[0-9]*", text.3))))
    hl.period <- gsub("[^a-z]+p[\":]+", "", unlist(regmatches(text.3, gregexpr("[^a-z]+p[\":]+[0-9]*", text.3))))
    hl.Team1 <- gsub("[^a-z]+t1[\":]+", "", unlist(regmatches(text.3, gregexpr("[^a-z]+t1[\":]+[A-Z]*", text.3))))
    hl.Team2 <- gsub("[^a-z]+t2[\":]+", "", unlist(regmatches(text.3, gregexpr("[^a-z]+t2[\":]+[A-Z]*", text.3))))
    hl.seconds <- 1200*(as.numeric(hl.period) - 1) + as.numeric(hl.presecs)
    
    pbp.new$Highlight <- 1*({!is.na(match(pbp.new$Seconds, hl.seconds)) & pbp.new$Event %in% c("GOAL", "SHOT")} | pbp.new$Event == "GOAL")
  
    ########################################################################################################################################################################################################
    ########################## SCRAPE SPORTSNET ############################################################################################################################################################
    ########################################################################################################################################################################################################
    
    if (as.numeric(season) >= 20152016) {
      
      # Provide date
      day <- date
      
      # Scrape main page
      url <- paste0("http://www.sportsnet.ca/hockey/nhl/scores/?datepicker-date=", day)
      glist <- getURL(url, header = FALSE,
                      .opts = curlOptions(
                        referer = 'sportsnet.ca',
                        verbose = TRUE,
                        header = TRUE,
                        followLocation = TRUE,
                        useragent = 'Mozilla/5.0 (Windows NT 6.3; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/43.0.2357.130 Safari/537.36'))
      
      gameids <- gsub("window.open[(][']", "", unique(unlist(regmatches (glist, gregexpr("window.open[(][']http://www.sportsnet.ca/hockey/nhl/livetracker/game/[0-9]+", glist)))))
      teamcity <- gsub("<span class=\"scores-team-city\">|</span>", "", unlist(regmatches (glist, gregexpr("<span class=\"scores-team-city\">([a-zA-Z]|[.]|[-]|[ ])+</span>", glist))))
      teamname <- gsub("<span class=\"scores-team-name\">|</span>", "", unlist(regmatches (glist, gregexpr("<span class=\"scores-team-name\">([a-zA-Z]|[.]|[-]|[ ])+</span>", glist))))
      teams <- paste(teamcity, teamname)

      # Standardize team names
      teamlist <- c("ANA", "ARI", "BOS", "BUF", "CAR", "CBJ",
                    "CGY", "CHI", "COL", "DAL", "DET", "EDM",
                    "FLA", "L.A", "MIN", "MTL", "N.J", "NSH",
                    "NYI", "NYR", "OTT", "PHI", "PIT", "S.J",
                    "STL", "T.B", "TOR", "VAN", "WPG", "WSH",
                    "PHX")
      
      fullnames <- c("Anaheim Ducks", "Arizona Coyotes", "Boston Bruins", "Buffalo Sabres", "Carolina Hurricanes", "Columbus Blue Jackets",
                     "Calgary Flames", "Chicago Blackhawks", "Colorado Avalanche", "Dallas Stars", "Detroit Red Wings", "Edmonton Oilers",
                     "Florida Panthers", "Los Angeles Kings", "Minnesota Wild", "Montreal Canadiens", "New Jersey Devils", "Nashville Predators",
                     "New York Islanders", "New York Rangers", "Ottawa Senators", "Philadelphia Flyers", "Pittsburgh Penguins", "San Jose Sharks",
                     "St. Louis Blues", "Tampa Bay Lightning", "Toronto Maple Leafs", "Vancouver Canucks", "Winnipeg Jets", "Washington Capitals",
                     "Phoenix Coyotes")
      
      team.match <- cbind(teamlist, fullnames) %>% data.frame()
      
      teams <- team.match$teamlist[match(teams, team.match$fullnames)]
      
      teammat <- matrix(teams, byrow = TRUE, ncol = 2) %>% data.frame()

      # Create URL directory
      url.match <- cbind(gameids, teammat) %>% data.frame() %>% rename(awayteam = X1, hometeam = X2)
      
      # Match URL
      urlt <- first(url.match$gameids[which(url.match$awayteam == awayteam | url.match$hometeam == awayteam)])
      
      ########################################################################################################################################################################################################
      ########################################################################################################################################################################################################
      
      # Scrape game page
      gamepage <- getURL(urlt, header = FALSE,
                         .opts = curlOptions(
                           referer = 'sportsnet.ca',
                           verbose = TRUE,
                           header = TRUE,
                           followLocation = TRUE,
                           useragent = 'Mozilla/5.0 (Windows NT 6.3; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/43.0.2357.130 Safari/537.36'))
      
      events <- unlist(regmatches (gamepage, gregexpr("[{]\\\"id\\\":[0-9]+,\\\"loc.*?momentum", gamepage)))
      time <- gsub("elapsed\\\":\\\"", "", unlist(regmatches (events, gregexpr("elapsed\\\":\\\"[0-9:]+", events))))
      period <- gsub("period\\\":|,", "", unlist(regmatches (events, gregexpr("period\\\":[0-9],", events))))
      type <- gsub("event\\\":\\\"", "", unlist(regmatches (events, gregexpr("event\\\":\\\"[a-zA-Z -]+", events))))
      location <- gsub("location\\\":|[[]|[]]|", "", unlist(regmatches (events, gregexpr("location\\\":[[][0-9a-zA-Z,-]+[]]", events))))
      
      # Rename event types
      type[type == "hit"] <- "HIT"; type[type == "score"] <- "GOAL"; type[type == "penalty"] <- "PENL"
      type[type == "shot-on-goal"] <- "SHOT"; type[type == "shot-missed"] <- "MISS"; type[type == "shot-blocked"] <- "BLOCK"
      
      # Parse time
      timemat <- data.frame(matrix(as.numeric(unlist(strsplit(as.character(time), ":"))), byrow = TRUE, ncol = 2))
      seconds <- 1200*(as.numeric(period) - 1) + timemat$X1*60 + timemat$X2
      
      # Parse coordinates
      locmat <- data.frame(matrix(as.numeric(unlist(strsplit(as.character(location), ","))), byrow = TRUE, ncol = 2))
      
      sn.table <- cbind(seconds, type, locmat) %>% data.frame() %>% rename(xc = X1, yc = X2)
      
      # Match with PBP
      pbp.new <- group_by(pbp.new, Seconds, Event) %>% 
        mutate(XC = first(sn.table$xc[which(sn.table$seconds == round(as.numeric(as.character(Seconds)), 0) & as.character(sn.table$type) == as.character(Event))]),
               YC = first(sn.table$yc[which(sn.table$seconds == round(as.numeric(as.character(Seconds)), 0) & as.character(sn.table$type) == as.character(Event))]))
      
    } else {
      
      # Provide date
      day <- gsub("-", "", as.character(date))
      
      # Scrape main page
      url <- paste("http://scores.espn.go.com/nhl/scoreboard?date=", day, sep = "")
      glist <- getURL(url, header = FALSE,
                      .opts = curlOptions(
                      referer = 'sports.espn.go.com',
                      verbose = TRUE,
                      header = TRUE,
                      followLocation = TRUE,
                      useragent = 'Mozilla/5.0 (Windows NT 6.3; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/43.0.2357.130 Safari/537.36'))
      
      gameids <- unique(unlist(regmatches(glist, gregexpr("gameId=[0-9]+", glist))))
      teams <- toupper(gsub("team/_/name/", "", unique(unlist(regmatches(glist, gregexpr("team/_/name/[a-zA-Z]+", glist))))))
      
      # Format team names
      teams[which(teams == "PHX")] <- "ARI"
      teams[which(teams == "TB")] <- "T.B"
      teams[which(teams == "NJ")] <- "N.J"
      teams[which(teams == "SJ")] <- "S.J"
      teams[which(teams == "LA")] <- "L.A"
      
      teammat <- matrix(teams, byrow = TRUE, ncol = 2) %>% data.frame()
      
      # Create URL directory
      url.match <- cbind(gameids, teammat) %>% data.frame() %>% rename(awayteam = X1, hometeam = X2)
      
      # Match URL
      urlt <- first(as.character(url.match$gameids[which(as.character(url.match$awayteam) == as.character(awayteam) | as.character(url.match$hometeam) == as.character(awayteam))]))
      
      ####################################################################################################################################################################################
      ####################################################################################################################################################################################
      
      # Scrape game page
      url2 <- paste("http://sports.espn.go.com/nhl/gamecast/data/masterFeed?lang=en&isAll=true&rand=0&", urlt, sep = "")
      gamepage <- getURL(url2, header = FALSE,
                         .opts = curlOptions(
                         referer = 'sports.espn.go.com',
                         verbose = TRUE,
                         header = TRUE,
                         followLocation = TRUE,
                         useragent = 'Mozilla/5.0 (Windows NT 6.3; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/43.0.2357.130 Safari/537.36'))
      
      enames = c("FAC", "HIT", "GvTk", "GOAL", "SHOT", "MISS", "BLOCK", "PENL",
                 "STOP", "PRDY", "PSTR", "PEND", "PERD", "SOC", "GEnd", "SOut",
                 "error", "TAKE", "GIVE", "early intermission", "nothing", "nothing")
      ecodes = as.character(c(502, 503, 504, 505, 506, 507, 508, 509,
                              516, 517, 518, 519, 520, 521, 522, 0, 
                              9999, 1401, 1402, -2147483648, 1, 5))
      
      etext <- unlist(regmatches(gamepage, gregexpr("<Play.*?/Play>", gamepage))) # MISSING GAMECASTS
      
      if (length(etext) > 1) {
        esplit <- t(do.call(cbind, strsplit (etext, "[\\[~]")))
        esplit <- esplit[,c(5,3,4,6,7,11)]  #
        colnames(esplit) <- c("etype","xc","yc","time","period","event.description")
        esplit <- esplit[,1:5] %>% as.data.frame(stringsAsFactors = FALSE)
        
        esplit$etype <- enames[match(esplit$etype, ecodes)]
        
        timesplits <- do.call(rbind, strsplit(esplit$time, ":"))
        seconds <- 1200*(as.numeric(esplit$period) - 1) + as.numeric(timesplits[,1])*60 + as.numeric(timesplits[,2])
        esplit$seconds <- seconds
        
        # Match with PBP
        pbp.new <- group_by(pbp.new, Seconds, Event) %>% 
          mutate(XC = first(esplit$xc[which(esplit$seconds == round(as.numeric(as.character(Seconds)), 0) & as.character(esplit$etype) == as.character(Event))]),
                 YC = first(esplit$yc[which(esplit$seconds == round(as.numeric(as.character(Seconds)), 0) & as.character(esplit$etype) == as.character(Event))]))
      } else {
        
        pbp.new$XC <- NA; pbp.new$YC <- NA
        
      }
    }
    
    ########################################################################################################################################################################################################
    ########################################################################################################################################################################################################
    ########################################################################################################################################################################################################
    
    # Fix duplicate names
    roster$Full.Name[which(roster$Full.Name == "ERIK.KARLSSON" & roster$Team == "CAR")] <- "ERIK.KARLSSON.2"
    
    # Replace teamnum
    if(names == TRUE) {
      pbp.new$p1 <- roster$Full.Name[match(pbp.new$p1, roster$Team.Num)]; pbp.new$p2 <- roster$Full.Name[match(pbp.new$p2, roster$Team.Num)]; pbp.new$p3 <- roster$Full.Name[match(pbp.new$p3, roster$Team.Num)]
      pbp.new$a1.num <- roster$Full.Name[match(pbp.new$a1.num, roster$Team.Num)]; pbp.new$a2.num <- roster$Full.Name[match(pbp.new$a2.num, roster$Team.Num)]; pbp.new$a3.num <- roster$Full.Name[match(pbp.new$a3.num, roster$Team.Num)]
      pbp.new$a4.num <- roster$Full.Name[match(pbp.new$a4.num, roster$Team.Num)]; pbp.new$a5.num <- roster$Full.Name[match(pbp.new$a5.num, roster$Team.Num)]; pbp.new$a6.num <- roster$Full.Name[match(pbp.new$a6.num, roster$Team.Num)]
      pbp.new$h1.num <- roster$Full.Name[match(pbp.new$h1.num, roster$Team.Num)]; pbp.new$h2.num <- roster$Full.Name[match(pbp.new$h2.num, roster$Team.Num)]; pbp.new$h3.num <- roster$Full.Name[match(pbp.new$h3.num, roster$Team.Num)]
      pbp.new$h4.num <- roster$Full.Name[match(pbp.new$h4.num, roster$Team.Num)]; pbp.new$h5.num <- roster$Full.Name[match(pbp.new$h5.num, roster$Team.Num)]; pbp.new$h6.num <- roster$Full.Name[match(pbp.new$h6.num, roster$Team.Num)]
      pbp.new$Away.Goalie <- roster$Full.Name[match(pbp.new$Away.Goalie, roster$Team.Num)]; pbp.new$Home.Goalie <- roster$Full.Name[match(pbp.new$Home.Goalie, roster$Team.Num)]
    }
    
    # Populate lists
    pbp.list[[j]] <- c(t(pbp.new))
    cnames1 <- colnames(pbp.new)
    
    roster.list[[j]] <- c(t(roster))
    cnames2 <- colnames(roster)
    
  }
  
  # Unlist tables
  pbp.full <- matrix(unlist(pbp.list), byrow = T, ncol = 52) %>% as.data.frame(stringsAsFactors = FALSE)
  colnames(pbp.full) <- cnames1
  pbp.full$Season <- season
  pbp.full$Season.Type <- "Regular"
  pbp.full$Season.Type[which(as.numeric(as.character(pbp.full$Game.ID)) >= 30000)] <- "Playoffs"
  
  roster.full <- matrix(unlist(roster.list), byrow = T, ncol = 10) %>% data.frame()
  colnames(roster.full) <- cnames2
  roster.full$Season <- season
  roster.full$Season.Type <- "Regular"
  roster.full$Season.Type[which(as.numeric(as.character(roster.full$Game.ID)) >= 30000)] <- "Playoffs"
  
  pbp.full <<- pbp.full
  roster.full <<- roster.full
  
}

########################################################################################################################################################################################################

# Test function
start = Sys.time()

scrape(season = "20132014", start = 20934, end = 20934, names = TRUE)

end = Sys.time()
print(end - start)

########################################################################################################################################################################################################

# FIX DETAILS
# SCRAPE SHIFT JSON
# ADD GRANULARITY TO SCORE CATEGORY
# ADD REBOUNDS AND RUSHES (NOT SEPARATED BY FACEOFFS)
# CALCULATE XG
# ASSIGN COLUMN CLASSES
# CHRONOLOGY STILL IFFY
