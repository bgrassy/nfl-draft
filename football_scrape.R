library("rvest")

for (year in 2008:2017) { 
  print(year)
  # Read draft data
  url <- paste("https://www.pro-football-reference.com/years/", year, "/draft.htm", sep="")
  webpage <- read_html(url)
  
  names_html <- html_nodes(webpage, "#drafts td:nth-child(4)")
  names <- html_text(names_html)
  filter_names <- function(names) {
    grepl("HOF", names)
  }
  filter_names <- Vectorize(filter_names)
  names[filter_names(names)] = substr(names[filter_names(names)], 1, nchar(names[filter_names(names)]) - 4)
  
  rnd_html <- html_nodes(webpage, "#drafts th.right")
  rnd <- as.numeric(html_text(rnd_html))
  
  pick_html <- html_nodes(webpage, "#drafts th+ .right")
  pick <- as.numeric(html_text(pick_html))
  
  pos_html <- html_nodes(webpage, "#drafts .left~ .left+ td.left")
  pos <- html_text(pos_html)
  
  age_html <- html_nodes(webpage, "#drafts .right:nth-child(6)")
  age <- as.numeric(html_text(age_html))
  
  college_html <- html_nodes(webpage, "#drafts td~ .right+ .left")
  college <- html_text(college_html)
  
  cols = c("Row", "Name", "Position", "Age", "Round", "Pick", "College", "Conference", "Games", "Seasons")
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
  
  def_pos = c("CB", "DB", "DE", "DT", "FS", "ILB", "LB", "NT", "OLB", "S", "SS")
  off_pos = c("C", "FB", "G", "QB", "RB", "T", "TE", "WR")
  other_pos = c("K", "P", "LS")
  
  info_df = data.frame(pick, names, pos, age, rnd, pick, college)
  other_df = data.frame(matrix(0, nrow = length(names), ncol=49))
  other_df[1] = 1:length(names)
  names(other_df)[1] = "Row"
  names(info_df)[1] = "Row"
  df = merge(info_df, other_df)
  names(df) = cols
  df = df[-1]
  df = df[order(df$Pick),]
  rownames(df) <- NULL
  
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
  
  header <- function(final_table) {
    if (!is.null(final_table)) {
      colnames(final_table) = final_table[1, ]
      final_table[-1, ]
    } else {
      NULL
    }
  }
  
  old_rnd = 0
  bad_rows = list()
  
  # We now have all of our info data. Let's now collect college stats.
  for (row in 1:nrow(df)) {
    curr_rnd = df[row, "Round"]
    if (curr_rnd != old_rnd) {
      print(paste("Round", curr_rnd))
      old_rnd <- curr_rnd
    }
    stat_url = html_attr(html_nodes(webpage, paste("#drafts tr:nth-child(", row+curr_rnd-1, ") .right a")), "href")
    nfl_url = html_attr(html_nodes(webpage, paste("#drafts tr:nth-child(", row+curr_rnd-1, ") .left+ .left a")), "href")
    nfl_url = paste("https://www.pro-football-reference.com", nfl_url, sep="")
    
    pos = df[row, "Position"]
    
    if ((length(stat_url) == 0) || (length(nfl_url) == 0)) {
      if ((pos == "T") || (pos == "LS") || (pos == "G") || (pos == "C")) {
        if (length(nfl_url) == 0) {
          bad_rows <- c(bad_rows, row)
          next
        }
        nfl_page <- read_html(nfl_url)
        combine <- get_stat(nfl_page, "combine")
        if (length(combine) > 0) {
          df[row, "Height"] = as.numeric(combine[1, "Ht"])
          df[row, "Weight"] = as.numeric(combine[1, "Wt"])
          df[row, "40 Yard"] = as.numeric(combine[1, "40yd"])
          df[row, "Bench"] = as.numeric(combine[1, "Bench"])
          df[row, "Broad Jump"] = as.numeric(combine[1, "Broad Jump"])
          df[row, "Shuttle"] = as.numeric(combine[1, "Shuttle"])
          df[row, "3 Cone"] = as.numeric(combine[1, "3Cone"])
          df[row, "Vertical"] = as.numeric(combine[1, "Vertical"])
        } else {
          if (length(html_nodes(nfl_page, "#meta p+ p span:nth-child(1)")) > 0) {
            height <- html_text(html_nodes(nfl_page, "#meta p+ p span:nth-child(1)"))
            weight <- html_text(html_nodes(nfl_page, "#meta span+ span:nth-child(2)"))
            if ((length(height) == 0) || (length(weight) == 0)) {
              bad_rows <- c(bad_rows, row)
              next
            }
            height <- as.numeric(strsplit(height, "-")[[1]])
            df[row, "Height"] = 12 * height[1] + height[2]
            df[row, "Weight"] = as.numeric(substr(weight, 1, 3))
          }
        }
      } else {
        bad_rows <- c(bad_rows, row)
      }
      next
    }
    if (stat_url == "http://www.sports-reference.com/cfb/players/brian-calhoun-2.html") {
      stat_url = "http://www.sports-reference.com/cfb/players/brian-calhoun-1.html"
    }
    if (stat_url == "http://www.sports-reference.com/cfb/players/walter-thurmond-1.html") {
      stat_url = "http://www.sports-reference.com/cfb/players/walter-thurmond-iii-1.html"
    }
    stat_page <- read_html(stat_url)
    nfl_page <- read_html(nfl_url)
    
    conf <- html_text(html_nodes(stat_page, "tbody .left+ .left"))
    conf <- conf[conf != ""]
    if (length(conf) > 0) {
      df[row, "Conference"] = conf[length(conf)]
    }
    
    if ((pos == "T") || (pos == "LS") || (pos == "G") || (pos == "C")) {
      next
    }
  
    # Get combine statistics
    phys_html <- html_nodes(stat_page, "#meta span")
    combine <- get_stat(nfl_page, "combine")
    if (length(combine) > 0) {
      df[row, "Height"] = as.numeric(combine[1, "Ht"])
      df[row, "Weight"] = as.numeric(combine[1, "Wt"])
      df[row, "40 Yard"] = as.numeric(combine[1, "40yd"])
      df[row, "Bench"] = as.numeric(combine[1, "Bench"])
      df[row, "Broad Jump"] = as.numeric(combine[1, "Broad Jump"])
      df[row, "Shuttle"] = as.numeric(combine[1, "Shuttle"])
      df[row, "3 Cone"] = as.numeric(combine[1, "3Cone"])
      df[row, "Vertical"] = as.numeric(combine[1, "Vertical"])
    } else {
      if (length(html_nodes(nfl_page, "#meta p+ p span:nth-child(1)")) > 0) {
        height <- html_text(html_nodes(nfl_page, "#meta p+ p span:nth-child(1)"))
        weight <- html_text(html_nodes(nfl_page, "#meta span+ span:nth-child(2)"))
        if ((length(height) == 0) || (length(weight) == 0)) {
          bad_rows <- c(bad_rows, row)
          next
        }
        height <- as.numeric(strsplit(height, "-")[[1]])
        df[row, "Height"] = 12 * height[1] + height[2]
        df[row, "Weight"] = as.numeric(substr(weight, 1, 3))
      } else {
        bad_rows <- c(bad_rows, row)
      }
    }
    
    # Get all other stats
    pass_table = NULL
    punt_table = NULL
    kick_table = NULL
    rush_table = NULL
    def_table = NULL
    games = {}
    RB = TRUE
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
  df[is.na(df)] = 0
  df <- df[!(rownames(df) %in% bad_rows), ]
  new_names <- c("Year", names(df))
  other <- data.frame(matrix(year, nrow = nrow(df), ncol=1))
  df <- cbind(other, df)
  names(df) <- new_names
  #write.csv(df, paste("data/", year, ".csv", sep=""), row.names=FALSE)
}
