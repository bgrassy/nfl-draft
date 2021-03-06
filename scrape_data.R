# Collects the 2018 player data.
library("rvest")
source("impute.R")
source("fix_conference.R")

# Flag to check whether the pick and round data should be scraped.
current = FALSE

years = list()

START_YEAR = 2008
END_YEAR = 2018

# Given html table data and a stat, returns the value of the stat.
get_stat <- function(html_data, stat) {
  nodes <- html_nodes(html_data, paste("#", stat, sep="")) 
  if (length(nodes) > 0) {
    html_table(nodes, header=TRUE)[[1]]
  } else {
    nodes <- html_nodes(html_data, paste("#all_", stat, sep=""))  
    nodes <- html_nodes(nodes, xpath = 'comment()')
    if (length(nodes) > 0) {
      nodes %>% html_text() %>%   
        read_html() %>%             
        html_node('table') %>%    
        html_table(header=TRUE)  
    } else {
      NULL
    }
  }
}

# Cleans the data from get_stat.
header <- function(final_table) {
  if (!is.null(final_table)) {
    colnames(final_table) = final_table[1, ]
    final_table[-1, ]
  } else {
    NULL
  }
}

# Loop over all years in the dataset.
for (year in START_YEAR:END_YEAR) {
  print(year)
  # Read draft data
  url <- paste(c("https://www.pro-football-reference.com/draft/", as.character(year), "-combine.htm"), collapse="")
  webpage <- read_html(url)
  
  # Get the names of the players
  names_html <- html_nodes(webpage, "tbody .left:nth-child(1)")
  all_names <- html_text(names_html)
  names <- all_names[all_names != "Player"]
  num_players = length(names)
  
  # Get the position of the players
  pos_html <- html_nodes(webpage, "th+ td")
  pos <- html_text(pos_html)
  
  pick <- rep(0, num_players)
  round <- rep(0, num_players)
  # Get draft data if this is not the current year.
  if (!current) {
    draft_html <- html_nodes(webpage , ".right+ .left")
    draft_info <- html_text(draft_html)
    draft_info[draft_info == ""] = "Undrafted / 0th / 0th / 0"
    draft_spots <- matrix(unlist(strsplit(draft_info, " / ")), ncol = 4, byrow = T);
    round <- as.numeric(substr(draft_spots[,2], 0, 1))
    pick <- as.numeric(gsub("[^0-9.]", "", draft_spots[,3]))
  }
  
  # Get college Data
  college_html <- html_nodes(webpage, "td.left+ .left")
  college <- html_text(college_html)
  
  # Define names of columns
  cols = c("Row", "Name", "Position", "Round", "Pick", "College", "Conference", "Games", "Seasons")
  combine = c("Height", "Weight", "40 Yard", "Bench", "Broad Jump", "Shuttle", "3 Cone", 
              "Vertical")
  defense = c("Solo Tackles", "Ast Tackles", "Total Tackles", "Tackles for Loss", 
              "Sacks", "Int", "Int Return Yards", "Int TD", 
              "Pass Deflection", "Fumble Recovery", "Fumble Return Yards", "Fumble TD", 
              "Forced Fumbles")
  offense = c("Pass Att", "Pass Completions", "Pass Yds",
              "Pass TD", "Pass Int", "Passer Rating", "Rush Att", "Rush Yds",
              "Rush TD", "Rec", "Rec Yds", "Rec TD")
  special = c("PRs", "PR Yds", "PR TD", "KRs", "KR Yds", "KR TD", "XPA", "XP%",
              "FGA", "FG%", "Punts", "Punt Avg")
  cols = c(cols, combine, offense, defense, special)
  
  # List names of positions
  def_pos = c("CB", "DB", "DE", "DT", "FS", "ILB", "LB", "NT", "OLB", "S", "SS", "EDGE")
  off_pos = c("C", "FB", "G", "QB", "RB", "T", "TE", "WR")
  other_pos = c("K", "P", "LS")
  
  # Set up dataframe that holds the data.
  info_df = data.frame(1:num_players, names, pos, round, pick, college)
  other_df = data.frame(matrix(0, nrow = num_players, ncol=49))
  other_df[1] = 1:num_players
  names(other_df)[1] = "Row"
  names(info_df)[1] = "Row"
  df = merge(info_df, other_df)
  names(df) = cols
  rownames(df) <- NULL
  df["Row"] <- NULL
  
  # Get list of stat pages for all players.
  stat_urls = html_nodes(webpage, ".left+ .right")
  stats = html_table(webpage)[[1]]
  stats = stats[stats$Player != "Player",]
  rownames(stats) <- NULL
  
  # Initialize the list of rows that do not have any good information.
  bad_rows = list()
  
  # Loops through all players and collects college stats
  for (row in 1:nrow(df)) {
    # Skip header rows
    if (df[row, "Name"] == "Player") {
      bad_rows <- c(bad_rows, row)
      next
    }
    
    stat_url = html_attr(html_nodes(stat_urls[row], "a"), "href")
    
    # Read vital data
    pos <- df[row, "Position"]
    height <- stats[row, "Ht"]
    weight <- stats[row, "Wt"]
    height <- as.numeric(strsplit(height, "-")[[1]])
    df[row, "Height"] = 12 * height[1] + height[2]
    df[row, "Weight"] = as.numeric(substr(weight, 1, 3))
    
    # Read combine data
    df[row, "40 Yard"] = as.numeric(stats[row, "40yd"])
    df[row, "Vertical"] = as.numeric(stats[row, "Vertical"])
    df[row, "Bench"] = as.numeric(stats[row, "Bench"])
    df[row, "Broad Jump"] = as.numeric(stats[row, "Broad Jump"])
    df[row, "3 Cone"] = as.numeric(stats[row, "3Cone"])
    df[row, "Shuttle"] = as.numeric(stats[row, "Shuttle"])
    
    # Offensive linemen have no stats, so we continue if this is the case.
    if ((pos == "T") || (pos == "LS") || (pos == "G") || (pos == "C")) {
      next
    }
    
    # If there are no other stats and this is not an offensive lineman, we want
    # to ignore this player also.
    if (length(stat_url) == 0) {
      bad_rows <- c(bad_rows, row)
      next
    }
    # special case of broken links
    if (stat_url == "https://www.sports-reference.com/cfb/players/walter-thurmond-1.html") {
      stat_url = "https://www.sports-reference.com/cfb/players/walter-thurmond-iii-1.html"
    }
    if (stat_url == "https://www.sports-reference.com/cfb/players/jj-watt-2.html") {
      stat_url = "https://www.sports-reference.com/cfb/players/jj-watt-1.html"
    }
    if (stat_url == "https://www.sports-reference.com/cfb/players/donta-hightower-2.html") {
      stat_url = "https://www.sports-reference.com/cfb/players/donta-hightower-1.html"
    }
    if (stat_url == "https://www.sports-reference.com/cfb/players/jr-sweezy-2.html") {
      stat_url = "https://www.sports-reference.com/cfb/players/jr-sweezy-1.html"
    }
    if (stat_url == "https://www.sports-reference.com/cfb/players/louis-nix-iii.html") {
      stat_url = "https://www.sports-reference.com/cfb/players/louis-nix-iii-1.html"
    }
    
    
    stat_page <- read_html(stat_url)
    conf <- html_text(html_nodes(stat_page, "tbody .left+ .left"))
    conf <- conf[conf != ""]
    if (length(conf) > 0) {
      df[row, "Conference"] = conf[length(conf)]
    }
    
    # Get all other stats
    pass_table = NULL
    punt_table = NULL
    kick_table = NULL
    rush_table = NULL
    def_table = NULL
    games = {}
    RB = TRUE
    # Get table of statistics
    if (pos == "QB") {
      pass_table <- header(get_stat(stat_page, "passing"))
    }
    if (pos %in% off_pos) {
      rush_table <- header(get_stat(stat_page, "rushing"))
      if (length(rush_table) == 0) {
        rush_table <- header(get_stat(stat_page, "receiving"))
        RB = FALSE
      }
      games <- rush_table$G
    } else if (pos %in% def_pos) {
      def_table <- header(get_stat(stat_page, "defense"))
      games <- def_table$G
    } else {
      kick_table <- header(get_stat(stat_page, "kicking"))
      if (length(kick_table) == 0) {
        kick_table <- header(get_stat(stat_page, "punting"))
      }
      games <- kick_table$G
    }
    ret_table <- header(get_stat(stat_page, "punt_ret"))
    if (length(ret_table) == 0) {
      ret_table <- header(get_stat(stat_page, "kick_ret"))
    }
    
    games <- na.omit(as.numeric(games[-length(games)]))
    df[row, "Games"] = sum(games)
    df[row, "Seasons"] = length(games)
    if (length(pass_table) > 0) {
      df[row, "Pass Att"] = as.numeric(pass_table$Att[nrow(pass_table)])
      df[row, "Pass Completions"] = as.numeric(pass_table$Cmp[nrow(pass_table)])
      df[row, "Pass Yds"] = as.numeric(pass_table$Yds[nrow(pass_table)])
      df[row, "Pass TD"] = as.numeric(pass_table$TD[nrow(pass_table)])
      df[row, "Pass Int"] = as.numeric(pass_table$Int[nrow(pass_table)])
      df[row, "Passer Rating"] = as.numeric(pass_table$Rate[nrow(pass_table)])
    }
    if (length(rush_table) > 0) {
      if (RB) { # Running back
        df[row, "Rush Att"] = as.numeric(rush_table[nrow(rush_table), 7])
        df[row, "Rush Yds"] = as.numeric(rush_table[nrow(rush_table), 8])
        df[row, "Rush TD"] = as.numeric(rush_table[nrow(rush_table), 10])
        df[row, "Rec"] = as.numeric(rush_table[nrow(rush_table), 11])
        df[row, "Rec Yds"] = as.numeric(rush_table[nrow(rush_table), 12])
        df[row, "Rec TD"] = as.numeric(rush_table[nrow(rush_table), 14])
      } else {
        df[row, "Rush Att"] = as.numeric(rush_table[nrow(rush_table), 11])
        df[row, "Rush Yds"] = as.numeric(rush_table[nrow(rush_table), 12])
        df[row, "Rush TD"] = as.numeric(rush_table[nrow(rush_table), 14])
        df[row, "Rec"] = as.numeric(rush_table[nrow(rush_table), 7])
        df[row, "Rec Yds"] = as.numeric(rush_table[nrow(rush_table), 8])
        df[row, "Rec TD"] = as.numeric(rush_table[nrow(rush_table), 10])
      }
    }
    if (length(def_table) > 0) {
      df[row, "Solo Tackles"] = as.numeric(def_table$Solo[nrow(def_table)])
      df[row, "Ast Tackles"] = as.numeric(def_table$Ast[nrow(def_table)])
      df[row, "Total Tackles"] = as.numeric(def_table$Tot[nrow(def_table)])
      df[row, "Tackles for Loss"] = as.numeric(def_table$Loss[nrow(def_table)])
      df[row, "Sacks"] = as.numeric(def_table$Sk[nrow(def_table)])
      df[row, "Int"] = as.numeric(def_table$Int[nrow(def_table)])
      df[row, "Int Return Yards"] = as.numeric(def_table[nrow(def_table), 13])
      df[row, "Int TD"] = as.numeric(def_table[nrow(def_table), 15])
      df[row, "Pass Deflection"] = as.numeric(def_table$PD[nrow(def_table)])
      df[row, "Fumble Recovery"] = as.numeric(def_table$FR[nrow(def_table)])
      df[row, "Fumble Return Yards"] = as.numeric(def_table[nrow(def_table), 18])
      df[row, "Fumble TD"] = as.numeric(def_table[nrow(def_table), 19])
      df[row, "Forced Fumbles"] = as.numeric(def_table$FF[nrow(def_table)])
    }
    if (length(ret_table) > 0) {
      df[row, "KRs"] = as.numeric(ret_table[nrow(ret_table), 7])
      df[row, "KR Yds"] = as.numeric(ret_table[nrow(ret_table), 8])
      df[row, "KR TD"] = as.numeric(ret_table[nrow(ret_table), 10])
      df[row, "PRs"] = as.numeric(ret_table[nrow(ret_table), 11])
      df[row, "PR Yds"] = as.numeric(ret_table[nrow(ret_table), 12])
      df[row, "PR TD"] = as.numeric(ret_table[nrow(ret_table), 14])
    }
    if (length(kick_table) > 0) {
      if (pos == "K") {
        df[row, "XPA"] = as.numeric(kick_table[nrow(kick_table), 8])
        df[row, "XP%"] = as.numeric(kick_table[nrow(kick_table), 9])
        df[row, "FGA"] = as.numeric(kick_table[nrow(kick_table), 11])
        df[row, "FG%"] = as.numeric(kick_table[nrow(kick_table), 12])
        df[row, "Punts"] = as.numeric(kick_table[nrow(kick_table), 14])
        df[row, "Punt Avg"] = as.numeric(kick_table[nrow(kick_table), 16])
      } else if (pos == "P") {
        df[row, "XPA"] = as.numeric(kick_table[nrow(kick_table), 11])
        df[row, "XP%"] = as.numeric(kick_table[nrow(kick_table), 12])
        df[row, "FGA"] = as.numeric(kick_table[nrow(kick_table), 14])
        df[row, "FG%"] = as.numeric(kick_table[nrow(kick_table), 15])
        df[row, "Punts"] = as.numeric(kick_table[nrow(kick_table), 7])
        df[row, "Punt Avg"] = as.numeric(kick_table[nrow(kick_table), 9])
      }
    }
  }
  # Set NA values/stats to zero
  df[is.na(df)] = 0
  # clean bad rows
  df <- df[!(rownames(df) %in% bad_rows), ]
  new_names <- c("Year", names(df))
  other <- data.frame(matrix(year, nrow = nrow(df), ncol=1))
  df <- cbind(other, df)
  names(df) <- new_names
  write.csv(df, paste(c("data/", year, ".csv"), collapse=""), row.names=FALSE)
  
  years[[year - START_YEAR + 1]] = df
}
full <- do.call(rbind, years)
full_imputed <- impute(full)
full_fixed <- fix_conferences(full_imputed)
write.csv(full_fixed, "data/full.csv", row.names=FALSE)
