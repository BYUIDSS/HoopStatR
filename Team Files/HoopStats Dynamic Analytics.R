###  HOOPSTATS DYNAMIC ANALYTICS
###         
###   
###   This file contains functions that allow for dynamic analytics using data
###   from the HoopStats SQLite3 Server
###   
###   The goal is to use this file as the beginnings of an R Shiny app AND to unify
###   each teammate's individual contributions





#### Calls all analytical and pre-definedfunctions

main <- function(sql_db_path) {
  
  # Make sure we've got the right tools
  require(tidyverse)
  require(dplyr)
  require(stringr)
  require(grid)
  require(jpeg)
  require(png)
  require(RCurl)
  require(ggplot2)
  require(DBI)
  require(ggpubr)
  require(ggthemes)
  require(expss)
  require(scales)
  require(expss)
  require(scales)
  
  # Create the SQLite3 database connection to be used in all functions
  conn <- create_sqlite_connection(sql_db_path)
  
  
  # Court images
  court_url <- "https://thedatagame.files.wordpress.com/2016/03/nba_court.jpg"
  half_court_url <- "https://26955tnyu1c2aeak614rqou1-wpengine.netdna-ssl.com/wp-content/uploads/half-court.jpg"
  
  
  # Create a basketball court plot background with court images
  court_plot_image <- rasterGrob(readJPEG(getURLContent(court_url)))
  half_court_plot_image <- readJPEG(getURLContent(half_court_url))
  
  
  ######### ======================== < TEAM PUT FUNCTION CALLS HERE > ======================== #########
  
  # A simple summary example
  test_analytics(conn, court_plot_image)
  
  ### Michael's Functions
  show_opposing_team_highest_shooting_position(conn, half_court_plot_image, "Highland Varsity 19-20")
  
  ### Matt's Functions
  team_three(conn)
  
  
  
  
  
  # Close the connection
  dbDisconnect(conn)
}





######### ======================== PRE-DEFINED FUNCTIONS ======================== #########



# Create connection function
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



test_analytics <- function(conn, court_img) {
  
  ### An example of simple summary statistics
  
  
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





######### ======================== < TEAM DEFINE ANALYTICAL FUNCTIONS BELOW > ======================== #########

# NOTE: Each function must include a "connection" argument





# ================= Player Analytics ================= #



# When Jimmy and John are playing wings, what’s our 3-Pt percentage on the base 
# line (narrow shot)? Note: I still need to make this player specific...
### ========== < MATT put code here > ========== ###



# When Jimmy’s playing against Idaho Falls, how good is his mid-range shot around 
# the top of the key?



# When Jason is playing low-post, what’s his shooting percentage in the 1st half?



# When Steven is defending at center-position, what’s the shooting percentage of 
# the opposing team?



# When Bob is playing point-guard, what is our turnover rate after he’s been in the 
# game for more than 10 minutes?





# ================= Team Analytics ================= #



### MATT
# What’s our shooting percentage when shooting 3-Pointers at the top of the key? 
team_three <- function(conn){
  # Late in the game (last 5-minutes)? Here's the first part---Matt
  
  # Loading in background images
  half_court_url <- "https://26955tnyu1c2aeak614rqou1-wpengine.netdna-ssl.com/wp-content/uploads/half-court.jpg"
  
  img2 <- readJPEG(getURLContent(half_court_url))
  
  # logo <- png::readPNG('/Users/mattlewis/Documents/GitHub/madisonBB_data/logo.png')
  
  # Cleaning up the data
  hp_tables <- DBI::dbListTables(conn)
  raw_dat <- DBI::dbReadTable(conn = conn, name = hp_tables[1])
  
  # Creating ztype reference df
  ztype_ref <- tibble(ZTYPE = c(0,1,2,3,4,5), TYPE_DESC = c('Made 2pt', 'Made 3pt', 'Made ft',
                                                            'Miss 2pt', 'Miss 3pt', 'Miss ft'),
                      TYPE = c('made', 'made', 'made', 'miss', 'miss', 'miss'))
  dat <- raw_dat %>%
    filter(ZTYPE %in% c(0,1,2,3,4,5)) %>%
    left_join(ztype_ref)
  
  # Calculating fieldgoal percentage for 3 point regions
  dat3pts <- dat %>%
    filter(ZTEAM == 168 | ZTEAM == 169,ZTYPE == 1 | ZTYPE == 4)
  
  # Total 3pt%
  ThreeCent <- round(count_if(c('1'), dat3pts$ZTYPE)/count_if(c('1','4'), dat3pts$ZTYPE),digits = 2)
  
  # Right Corner 3pt%
  RightCorner <- filter(dat3pts, ZLOCATIONX >150, ZLOCATIONY<50)
  RCThreeCent <- round(count_if(c('1'), RightCorner$ZTYPE)/count_if(c('1','4'), RightCorner$ZTYPE),digits = 2)
  
  # Left Corner 3pt%
  LeftCorner <- filter(dat3pts, ZLOCATIONX <50, ZLOCATIONY<50)
  LCThreeCent <- round(count_if(c('1'), LeftCorner$ZTYPE)/count_if(c('1','4'), LeftCorner$ZTYPE),digits = 2)
  
  # Top of Key 3pt%
  TopKey <- filter(dat3pts, ZLOCATIONX <150, ZLOCATIONX<75)
  TKThreeCent <- round(count_if(c('1'), TopKey$ZTYPE)/count_if(c('1','4'), TopKey$ZTYPE),digits = 2)
  
  # Wing Right
  WingRight <- filter(dat3pts, ZLOCATIONX >150, ZLOCATIONX < min(RightCorner$ZLOCATIONX))
  WRThreeCent <- round(count_if(c('1'), WingRight$ZTYPE)/count_if(c('1','4'), WingRight$ZTYPE),digits = 2)
  
  # Wing Left
  WingLeft <- filter(dat3pts, ZLOCATIONX >max(LeftCorner$ZLOCATIONX), ZLOCATIONX < 75)
  WLThreeCent <- round(count_if(c('1'), WingLeft$ZTYPE)/count_if(c('1','4'), WingLeft$ZTYPE),digits = 2)
  
  # Plotting the data after filtering for 3 pointers only
  ThreePlot<-ggplot(filter(dat3pts,ZTEAM == 168 | ZTEAM == 169,ZTYPE == 1 | ZTYPE == 4),aes(ZLOCATIONX, ZLOCATIONY, color = TYPE)) + 
    background_image(img2) +
    #annotation_custom(rasterGrob(logo), xmin = 153, xmax = 220, ymin = 153, ymax = 170) +
    #geom_segment(aes(x=104.5, xend=ZLOCATIONX, y=20, yend=ZLOCATIONY), size = .25) +
    geom_point(size=5) +
    #geom_point(size=1, color = 'white') +
    scale_color_manual(values = c('#6df96b', '#f94a56')) +
    theme_minimal() +
    annotate("text", x = c(15,25, 100,175,200), y = c(20,100,160,100,20), 
             label = c(percent(LCThreeCent), percent(WLThreeCent),percent(TKThreeCent),percent(WRThreeCent),percent(RCThreeCent)) , color="black", 
             size=15 , angle=45, fontface="bold")+
    theme(axis.ticks = element_line(linetype = "blank"),
          panel.grid.major = element_line(linetype = "blank"),
          panel.grid.minor = element_line(linetype = "blank"),
          axis.title = element_text(colour = NA),
          axis.text = element_text(colour = NA))
  
  print(ThreePlot)
}



# When these 5 players are on the floor, what’s our turn-over rate?



# What 5 players produce the highest turn-over rate?



# Who are the top three shooters from the right side of the court for the upcoming 
# opposing team?



### MICHAEL
# What side of the court has the highest shooting percentage from the upcoming opposing team?
show_opposing_team_highest_shooting_position <- function(conn, court, team_name) {
  
  ## Plots and highlights the area of the court where the team has the highest shooting percentage
  ##  conn - Connection to Database
  ##  court - bbcourt image that has been rastered
  
  
  # Get all table names
  hp_tables <- DBI::dbListTables(conn)
  # print(hp_tables)
  
  # Get all needed table data
  zevent <- dbReadTable(conn, hp_tables[1])
  zteam <- dbReadTable(conn, hp_tables[6])
  
  # Get the primary key (pk) for the team
  team_data <- zteam %>% 
    filter(ZNAME == team_name) 
  pk_team <- team_data[1,1]
  # print(pk_team)
  
  # Get all shots performed by the specified team
  team_shots <- zevent %>% 
    filter(ZTEAM == pk_team,
           ZTYPE %in% c(0,1,3,4)) %>% 
    mutate(ZSHOT = ifelse(ZTYPE %in% c(0,1), "Shot Made", "Shot Missed")) %>% # Make up for image offset
    select(c("ZTYPE", "ZLOCATIONX", "ZLOCATIONY", "ZSHOT")) # We only need these columns
  print(team_shots)
  
  # Divide the shots different datasets based on court section:
  #   - Three Pointer
  #   - Two Pointer
  df_3ptrs <- team_shots %>% 
    filter(ZTYPE %in% c(1, 4))
  df_2ptrs <- team_shots %>% 
    filter(ZTYPE %in% c(0, 3))
  
  # Calculate the shooting percentage in each court section
  # sp = shots made / shots attempted
  sp_3ptr <- df_3ptrs %>% 
    filter(ZTYPE == 1) %>% 
    nrow() / nrow(df_3ptrs)
  
  sp_2ptr <- df_2ptrs %>% 
    filter(ZTYPE == 0) %>% 
    nrow() / nrow(df_2ptrs)
  
  # Find out which shooting percentage is the highest
  
  
  # Combine the shooting percentages into a dataframe with an x and y LOCATION
  sp_df <- as.data.frame(matrix(nrow = 3, ncol = 3))
  sp_df[1,] <- c(paste0(round(sp_3ptr, digits = 3) * 100, "%"), 60, 125)
  sp_df[2,] <- c(paste0(round(sp_2ptr, digits = 3) * 100, "%"), 165, 40)
  
  # Graph the results
  plot <- ggplot(team_shots, group = as.factor(ZSHOT)) + 
    background_image(court) + 
    geom_point(aes(x = ZLOCATIONX, y = ZLOCATIONY, alpha = as.factor(ZSHOT), fill = as.factor(ZSHOT)),
               shape = 21, size = 15, show.legend = FALSE) + 
    geom_text(data = sp_df, aes(x = as.numeric(V2), y = as.numeric(V3), label = V1, size = 5),
              show.legend = FALSE) + 
    scale_fill_manual(values = c("green","red")) + 
    scale_alpha_manual(values = c(0.7, 0.7)) + 
    labs(subtitle = "Area with Highest Shooting Percentage", title = team_name) + 
    # geom_vline(xintercept = 103) + 
    # geom_hline(yintercept = 45) + 
    theme_fivethirtyeight() + 
    theme(legend.title = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank())
  
  
  print(plot)
}



# How does the upcoming opposing team shooting percentage performance shift during 
# the game positionally?





######### ======================== Call Main Function ======================== ######### 



# Pass the location of the SQLite3 HoopStats Database
main(sql_db_path = "C:/Users/mneff/Documents/Git/madisonBB_data/Hoop Stats Data/HoopStats.sqlite")







