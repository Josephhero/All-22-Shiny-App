#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(nflreadr)
library(DT)

teams_data <- load_teams() |> filter(!team_abbr %in% c("LAR", "SD", "STL", "OAK"))
max_year <- get_current_season()
max_week <- get_current_week(use_date = TRUE)
players_data <- load_rosters(2016:max_year)

# Define UI for application
ui <- fluidPage(
  
  # Application title
  splitLayout(cellWidths = c(350, 140, 170),
              cellArgs = list(style = "padding: 10px"),
              h2(strong("All-22 Information")), 
              actionButton(inputId='twitter_id', label="@JosephJefe",
                           icon = icon("twitter"),
                           onclick ="window.open('https://twitter.com/josephjefe')",
                           style="color: white; background-color: #26a7de; border-color: #2e6da4"),
              actionButton(inputId='coffee_id', label="Buy Me a Coffee",
                           icon = icon("mug-hot"),
                           onclick ="window.open('https://www.buymeacoffee.com/JosephJefe')",
                           style="background-color: #ecb02e; border-color: #2e6da4")),
  
  # Sidebar with selector inputs
  sidebarLayout(
    sidebarPanel(
      width = 3, 
      p(strong("Data: ", em(a(href="https://nflverse.nflverse.com/", target = "_blank", "nflverse")))), 
      br(), 
      selectInput("year_var",
                  "Season:",
                  (2016:max_year),
                  selected = max_year
      ), 
      selectInput("team_var",
                  "Team:",
                  unique(teams_data$team_abbr) 
      ), 
      sliderInput("week_var",
                  "Week:",
                  (1:22), 
                  value = c(1, 22), 
                  min = 1, 
                  max = 22
      ), 
      h5("Update Season and Team before selecting player"), 
      uiOutput("playerInput"
      ), 
      submitButton("Update", 
                   icon("refresh"))
    ),  
    
    # Create panels for your data to be shown
    mainPanel(
      width = 9, 
      tabsetPanel(type = "tabs", 
                  tabPanel("All-22 Data", DTOutput("table")), 
                  tabPanel("Data Dictionary", DTOutput("table2")))
      
    )
  )
)

# Define server logic required for output
server <- function(input, output, session) {
  
  output$playerInput <- renderUI({
    
    players_data <- load_rosters(seasons = as.numeric(input$year_var)) |> 
      filter(season == as.numeric(input$year_var), team == input$team_var) |> 
      mutate(name_pos = paste0(full_name, ", ", position))
    
    selectInput("player_var",
                "Player (Optional):",
                c("", unique(sort(players_data$name_pos)))
    )
  })
  
  # Data ------
  my_table <- reactive({
    YEAR <- as.numeric(input$year_var)
    WEEK_START <- input$week_var[1]
    WEEK_END <- input$week_var[2]
    SELECTED_TEAM <- input$team_var
    USE_PLAYER <- input$use_player
    PLAYER <- input$player_var
    

    plays1 <- load_participation(seasons = YEAR, include_pbp = TRUE) |>
      filter(season == YEAR, posteam == SELECTED_TEAM | defteam == SELECTED_TEAM, week %in% WEEK_START:WEEK_END) |>
      mutate(play_num = row_number()) |>
      mutate(score = paste0(home_team, " ", total_home_score, "-", total_away_score, " ", away_team))
    
    if(length(PLAYER)) {
      player_info <- load_rosters(seasons = YEAR) |>
        mutate(name_pos = paste0(full_name, ", ", position)) |> 
        filter(team == SELECTED_TEAM, name_pos == PLAYER) 
      
      PLAYER_ID <- player_info$gsis_id[1]
      
      plays <- plays1 |> 
        filter(str_detect(offense_players, PLAYER_ID) | str_detect(defense_players, PLAYER_ID)) |> 
        mutate(player_name = PLAYER) |> 
        select(nflverse_game_id, week, play_num, score, player_name, qtr, time, posteam, defteam, 
               yrdln, yardline_100, down, ydstogo, play_type, yards_gained, epa, wpa, cpoe, success,  
               offense_formation, offense_personnel, defense_personnel, number_of_pass_rushers, 
               defenders_in_box, complete_pass, interception, passer_player_name, 
               receiver_player_name, interception_player_name, rusher_player_name, 
               qb_hit, qb_hit_1_player_name, qb_hit_2_player_name, sack, sack_player_name, 
               half_sack_1_player_name, half_sack_2_player_name, solo_tackle_1_player_name, 
               assist_tackle_1_player_name, assist_tackle_2_player_name, 
               assist_tackle_3_player_name, tackled_for_loss, tackle_for_loss_1_player_name, 
               tackle_for_loss_2_player_name, special_teams_play)
    } else {
      plays <- plays1 |> 
        select(nflverse_game_id, week, play_num, score, qtr, time, posteam, defteam,  
               yrdln, yardline_100, down, ydstogo, play_type, yards_gained, epa, wpa, cpoe, success,
               offense_formation, offense_personnel, defense_personnel, number_of_pass_rushers, 
               defenders_in_box, complete_pass, interception, passer_player_name, 
               receiver_player_name, interception_player_name, rusher_player_name, 
               qb_hit, qb_hit_1_player_name, qb_hit_2_player_name, sack, sack_player_name, 
               half_sack_1_player_name, half_sack_2_player_name, solo_tackle_1_player_name, 
               assist_tackle_1_player_name, assist_tackle_2_player_name, 
               assist_tackle_3_player_name, tackled_for_loss, tackle_for_loss_1_player_name, 
               tackle_for_loss_2_player_name, special_teams_play)
    }
    
    int_cols <- c("week", "qtr", "yardline_100", "down", "ydstogo", "yards_gained",
      "number_of_pass_rushers", "defenders_in_box", "complete_pass", "interception", 
      "qb_hit", "sack", "tackled_for_loss", "special_teams_play")
    
    plays <- as.data.frame(plays) |> 
      mutate(across(where(is.character), as.factor)) |> 
      mutate(across(all_of(int_cols), as.integer)) |> 
      mutate(across(c("epa", "wpa", "cpoe"), \(x) round(x, 3)))

  })
  
  data_dic <- reactive({
  
    cols_filter_raw <- colnames(my_table())
    
    cols_filter <- append(cols_filter_raw, "player_name", after = 4)
    
    dic_other <- 
      tibble(Field = c("play_num", "score", "player_name"),
             Description = c("Sequence of plays in game, starting from 1", 
                             "Current score in game, formatted as 'away_team away_score home_score home_team'", 
                             "Name of player in player filter, if player filter is used"), 
             Type = c("numeric", "character", "character"))
    
    dic_all <- bind_rows(dictionary_pbp, dictionary_participation) |> 
      filter(Field %in% cols_filter) |> 
      bind_rows(dic_other)
    
    cols_df <- tibble(Field = cols_filter) |> 
      left_join(dic_all, by = "Field") |> 
      distinct()
    
    cols_df <- as.data.frame(cols_df)
  
  })
  
  output$table <- renderDT(server = FALSE, {
    datatable(
      my_table(), 
      filter = list(position = 'top', clear = TRUE),
      selection = "none", #this is to avoid select rows if you click on the rows
      rownames = FALSE,
      extensions = "Buttons", 
      
      options = list(
        scrollX = TRUE,
        scrollY = "78vh", 
        dom = 'Blrtip',
        buttons =
          list(I('colvis'), 
            list(
              extend = 'colvisGroup', 
              text = "Show all Columns",
              show = ":hidden"
            ), 
            list(
            extend = 'collection',
            text = 'Download',
            buttons = list(
              list(
                extend = "csv",
                text = "csv", 
                filename = paste0("all_22"), 
                title=NULL, 
                exportOptions = list(
                  columns = ":visible")
              ),
              list(
                extend = "excel", 
                text = "excel", 
                filename = paste0("all_22"), 
                title=NULL, 
                exportOptions = list(
                  columns = ":visible")
              )
            )
          )),
        lengthMenu = list(c(25, 50, -1),
                          c("25", "50", "All"))
      ),
      class = "display"
    )
  })
  
  output$table2 <- renderDT({
    datatable(
      data_dic(), 
      options = list(
        scrollY = "78vh", 
        lengthMenu = list(c(50, -1),
                          c("50", "All"))
      )
      )
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
