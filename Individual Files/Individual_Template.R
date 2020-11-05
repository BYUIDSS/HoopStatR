
### HoopStats Dynamic Analytics
###
### <YOUR NAME>





library(RSQLite)      # Can used to create a connection to a SQLite3 Database
                      # Also downloads DBI, which is used to interact with the database
library(tidyverse)

library(dplyr)

library(ggplot2)

library(grid)         # For rastering images to plot

library(jpeg)         # For reading JPEGS

library(RCurl)        # For downloading a URI





######## =============================== DEFINE FUNCTIONS =============================== ########



create_sqlite_connection <- function(db_path) {

  ### Returns a connection to a SQLite Database file on computer
  ###
  ### Found working solution here:
  ### https://stackoverflow.com/questions/32545777/r-connection-to-sqlite

  require(RSQLite)

  # Create the connection to the SQLite Database using RSQLite
  conn <- dbConnect(dbDriver("SQLite"), db_path)

  # Return the connection
  return(conn)
}



# Doesn't actually use court_img, but YOU can!
test_analytics <- function(conn, court_img) {

  ### An example of simple summary statistics
  ### You can do better than this, though!


  # Get the needed data
  hp_tables <- DBI::dbListTables(conn)
  # print(hp_tables)


  ### Get the tables
  zevent <- DBI::dbReadTable(conn = conn, name = hp_tables[1])
  zteams <- DBI::dbReadTable(conn = conn, name = hp_tables[6])
  zplayers <- DBI::dbReadTable(conn = conn, name = hp_tables[4])



  ### Tidy the tables
  ###   - Filter rows to keep only the teams specified
  tidy_zevent <- zevent %>%
    filter(ZTEAM == 168 | ZTEAM == 169)

  tidy_zteams <- zteams %>%
    filter(Z_PK == 168 | Z_PK == 169) %>%
    select(-c("Z_OPT","Z_ENT")) # Keep all columns except these
  # Change column name to match ZEVENT's for joining
  colnames(tidy_zteams)[colnames(tidy_zteams) == "Z_PK"] <- "ZTEAM"

  tidy_zplayers <- zplayers %>%
    select(-c("Z_OPT","Z_ENT")) # Keep all columns except these
  # Change column name to match ZEVENT's for joining
  colnames(tidy_zplayers)[colnames(tidy_zplayers) == "Z_PK"] <- "ZPLAYER"



  ### Replace Z_EVENT numbers with team and player names via Join
  tidy_zevent <- left_join(tidy_zevent, tidy_zteams, "ZTEAM")
  # Need to change ZNAME column name before the next join
  colnames(tidy_zevent)[colnames(tidy_zevent) == "ZNAME"] <- "Z_TEAM_NAME"

  tidy_zevent <- left_join(tidy_zevent, tidy_zplayers, "ZPLAYER")
  colnames(tidy_zevent)[colnames(tidy_zevent) == "ZNAME"] <- "Z_PLAYER_NAME"



  ### Consolidate to just the columns/rows needed for the graphic
  gg_data <- tidy_zevent %>%
    select(c("ZTYPE","Z_TEAM_NAME","Z_PLAYER_NAME")) %>%
    filter(ZTYPE == 0 | ZTYPE == 1 | ZTYPE == 2 |
             ZTYPE == 3 | ZTYPE == 4 | ZTYPE == 5) %>%
    mutate(Z_SHOT_TYPE = ZTYPE,   # Add a column for graph grouping
           Z_FILL = ZTYPE)        # Add a column for graph coloring
  # Replace ZTYPE integer with matching string
  gg_data$ZTYPE[gg_data$ZTYPE == 0] <- "MADE_2"
  gg_data$ZTYPE[gg_data$ZTYPE == 1] <- "MADE_3"
  gg_data$ZTYPE[gg_data$ZTYPE == 2] <- "MADE_FT"
  gg_data$ZTYPE[gg_data$ZTYPE == 3] <- "MISSED_2"
  gg_data$ZTYPE[gg_data$ZTYPE == 4] <- "MISSED_3"
  gg_data$ZTYPE[gg_data$ZTYPE == 5] <- "MISSED_FT"
  # Replace ZTYPE integer with matching string
  gg_data$Z_SHOT_TYPE[gg_data$Z_SHOT_TYPE == 0] <- "2 Ptr"
  gg_data$Z_SHOT_TYPE[gg_data$Z_SHOT_TYPE == 3] <- "2 Ptr"
  gg_data$Z_SHOT_TYPE[gg_data$Z_SHOT_TYPE == 1] <- "3 Ptr"
  gg_data$Z_SHOT_TYPE[gg_data$Z_SHOT_TYPE == 4] <- "3 Ptr"
  gg_data$Z_SHOT_TYPE[gg_data$Z_SHOT_TYPE == 2] <- "FT"
  gg_data$Z_SHOT_TYPE[gg_data$Z_SHOT_TYPE == 5] <- "FT"
  # Replace ZTYPE integer with matching string
  gg_data$Z_FILL[gg_data$Z_FILL == 0] <- "Shot Made"
  gg_data$Z_FILL[gg_data$Z_FILL == 1] <- "Shot Made"
  gg_data$Z_FILL[gg_data$Z_FILL == 2] <- "Shot Made"
  gg_data$Z_FILL[gg_data$Z_FILL == 3] <- "Shot Missed"
  gg_data$Z_FILL[gg_data$Z_FILL == 4] <- "Shot Missed"
  gg_data$Z_FILL[gg_data$Z_FILL == 5] <- "Shot Missed"


  # print(gg_data)


  ### Graph the data
  plot <- ggplot(gg_data, aes(x = Z_SHOT_TYPE, group = ZTYPE, fill = Z_FILL)) +
    geom_bar(position = position_dodge(), width = .5) +
    scale_fill_manual(values = c("green3","orangered3")) +
    facet_grid(.~Z_TEAM_NAME) +
    labs(x = "Shot Type", y = "", title = "Hoop Stats - Team Shot Summary") +
    theme_minimal() +
    theme(panel.grid = element_blank(),
          panel.grid.major.y = element_line(color = "gray90"),
          legend.title = element_blank())

  print(plot)
}





######## =============================== CALL FUNCTIONS =============================== ########



## Test with the HoopStats SQLite Database

## FILL IN THE PATH TO THE "HoopStats.sqlite" FILE
conn <- create_sqlite_connection(db_path = "C:/Users/mneff/Documents/Git/HoopStatR/Hoop Stats Data/HoopStats.sqlite")



# To test the connection, list all tables in the database
hp_tables <- DBI::dbListTables(conn)
print(hp_tables)



# Create a plottable image of a basketball court from URL
court_url <- "https://thedatagame.files.wordpress.com/2016/03/nba_court.jpg"
court_plot_image <- rasterGrob(readJPEG(getURLContent(court_url)))



# Call test function
test_analytics(conn, court_plot_image) # Doesn't actually use court_plot_image, but YOU can!



# Call My Functions







