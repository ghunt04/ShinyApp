# Load required libraries
library(shiny)
library(shinythemes)
library(shinyjs)
library(ggplot2)
library(ggforce)
library(dplyr)
library(grid)
library(gridExtra)
library(scales)
library(png)
library(corrplot)
library(plotly)
library(htmltools)
library(tidyr)
library(Metrics)
library(RColorBrewer)

# Define the user interface (UI) for the Shiny app
ui <- fluidPage(
  navbarPage(
    theme = shinytheme("flatly"),  # Apply the Flatly theme
    
    # Custom CSS styles
    tags$head(
      tags$style(
        HTML(
          "
          .banner {
            background-color: #337ab7;
            color: white;
            padding: 10px;
            text-align: center;
            font-size: 24px;
            font-weight: bold;
          }
          .footer {
            background-color: #f8f8f8;
            padding: 10px;
            text-align: center;
            font-size: 14px;
            color: #777;
          }
          .title {
            font-size: 36px;
            color: #000000;
            text-align: center;
            margin-top: 30px;
            text-decoration: underline;
          }
          .image-container {
            display: block;
            margin: 0 auto;
            width: 200px;
            border-radius: 50%;
            box-shadow: 0px 0px 10px rgba(0, 0, 0, 0.3);
          }
          .intro-text,
          .experience-text,
          .blast-motion-text,
          .hockey-text,
          .football-analyst-text,
          .leadership-text,
          .softball-text,
          .webdev-text,
          .passion-text,
          .excitement-text,
          .shinyapp-text {
            font-size: 18px;
            color: #000000;
            text-align: justify;
            margin-top: 20px;
            margin-bottom: 20px;
            line-height: 1.5;
          }
          .report-info {
            font-size: 18px;
          }
          .content-container {
            position: relative;
            background-color: rgba(255, 255, 255, 0.9);
            padding: 30px;
          }
          .projects-background {
            background-image: url('field.jpg');
            background-repeat: no-repeat;
            background-size: cover;
            background-position: center;
          }
          .projects-background-hockey {
            background-image: url('msuhockey.jpeg');
            background-repeat: no-repeat;
            background-size: cover;
            background-position: center;
          }
          .projects-background-football {
            background-image: url('football.jpeg');
            background-repeat: no-repeat;
            background-size: cover;
            background-position: center;
          }
          .spartan-background {
            background-image: url('spartan.jpg');
            background-repeat: no-repeat;
            background-size: cover;
            background-position: center;
          }
          "
        )
      )
    ),
    
    # Banner section
    tabPanel(
      title = "Home",
      class = "spartan-background",
      tags$div(
        class = "banner",
        "George F. Hunt IV Shiny App"
      ),
      tags$div(
        class = "content-container",
        tags$h1(class = "title", "About Myself"),
        tags$div(
          img(src = "profile.png", class = "image-container"),
          tags$p(class = "intro-text",
                 "Hello, I'm George F. Hunt IV, a hard-working and passionate 
                 college student currently pursuing a degree in Computer Science at 
                 Michigan State University. As a member of the Honors College, I 
                 have consistently demonstrated exceptional dedication and commitment 
                 to my studies, earning recognition on the Dean's List. With a strong 
                 passion for the sports industry, I have actively sought opportunities 
                 to merge my love for sports with my skills in data analysis."),
          tags$p(class = "experience-text",
                 "My experience as a manager and bullpen catcher for the Michigan 
                 State baseball team allowed me to thrive in a competitive 
                 environment, balancing academics with gameplay. Leveraging advanced 
                 technologies like Trackman, Rapsodo, Spiideo, and Blast Motion, I 
                 gained invaluable insights through data analysis and interpretation. 
                 These experiences reinforced the importance of teamwork, effective 
                 communication, and unwavering dedication in achieving success."),
          tags$p(class = "blast-motion-text",
                 "While being a manager for the Spartans, I was given the opportunity 
                 to participate in an internship experience with Blast Motion, which 
                 provided me with many valuable experiences. During my time at Blast 
                 Motion, I earned my",
                 tags$a(
                   href = "blast.pdf",
                   target = "_blank",
                   download = "blast.pdf",
                   "Blast Motion Certification"
                 ), ", which 
                 demonstrated my proficiency in Blast metrics and knowledge. I developed
                 advanced skills in analytics using Blast Motion sensors, a tool 
                 favored by 28 MLB organizations. Applying Blast metrics to players, 
                 I identified areas for improvement and shared the insights with the
                 coaching staff at Michigan State. This internship allowed me to deepen 
                 my understanding of the impact of data-driven decision-making in sports 
                 and how it can be leveraged to help athletes perform at their best."
          ),
          tags$p(class = "hockey-text",
                 "Building on this foundation, my role as a Data Analyst for the MSU 
                 Hockey team deepened my understanding of data-driven decision-making 
                 in sports. By analyzing game data using tools like Excel, Tableau, and 
                 R, I contributed to the team's strategic decision-making process, 
                 identifying areas for improvement and leveraging data for a 
                 competitive edge."),
          tags$p(class = "football-analyst-text",
                 "During the summer of 2023, I was fortunate to be a Football Analyst Intern
                 at", tags$a(href = "https://www.instagram.com/draftcapitol/", target = "_blank", "Draft Capitol"), 
                 ", an Instagram page with 20k followers that covers NFL News and Fantasy 
                 Football. While interning here, I delved into player analysis, utilizing 
                 metrics and advanced statistics to objectively evaluate performance and 
                 identify trends. Working in a fast-paced environment, I delivered timely 
                 and accurate information to our followers, engaging with them through 
                 captivating content on social media platforms. This transformative 
                 experience equipped me with essential skills to excel in the field of 
                 football analysis."),
          tags$p(
            class = "leadership-text",
            HTML("Beyond my involvement with sports teams, I have assumed leadership 
                 positions in organizations related to sports analytics and management. 
                 As the Vice President of the Sports Analytics Club ("),
            tags$a(href = "https://www.linkedin.com/company/msu-sports-analytics-club", target = "_blank", "SAC"),
            HTML("), I fostered teamwork 
                 and communication through leading discussions and guiding team projects. 
                 Additionally, as the Professional Development Chair of the Future Leaders 
                 in Sports and Entertainment ("),
            tags$a(href = "https://msuflise.org/", target = "_blank", "FLISE"),
            HTML(") club, I organized networking events, guest speaker presentations, 
                 and career development workshops, providing valuable resources for members 
                 to excel in the sports management field.")
          ),
          tags$p(
            class = "softball-text",
            "One of my notable achievements was collaborating with Dr. Cohen to utilize 
                 machine learning techniques in Python, predicting the probability of the 
                 Michigan State Softball team receiving a playoff bid. Achieving an impressive 
                 accuracy rating of 90%, I contributed to the team's performance evaluation 
                 and strategic planning, communicating complex findings through visual 
                 representations and providing valuable insights for data-driven 
                 decision-making. Download a PDF file of my research ",
            tags$a(
              href = "sball_report.pdf",
              target = "_blank",
              download = "sball_report.pdf",
              "HERE"
            ), "."
          ),
          tags$p(class = "webdev-text",
                 "Furthermore, my proficiency in web development enabled me to create 
                 dynamic websites with engaging features, such as a piano interface, 
                 an NFT marketplace, and a chat feature. These projects showcased my skills 
                 in HTML, JavaScript, SQL, Python, and the ability to craft captivating 
                 digital experiences. Take a look at my website ",
                 tags$a(href = "https://exam-eqdcpymbha-uc.a.run.app/home", 
                        target = "_blank", "HERE"), "."),
          tags$p(class = "passion-text",
                 "Throughout my academic journey and professional experiences, I have 
                 consistently applied my analytical skills and knowledge to contribute 
                 to the success of sports teams and organizations. Fueled by my passion 
                 for data-driven decision-making, I aspire to make a meaningful impact 
                 in the sports industry, leveraging my expertise in computer science, 
                 econometrics, and data analysis."),
          tags$p(class = "excitement-text",
                 "With a strong work ethic, excellent communication skills, and a 
                 proven ability to lead and collaborate effectively, I am thrilled to 
                 continue my growth and learning journey in the field of sports 
                 analytics and management. I eagerly anticipate applying my expertise 
                 and insights to future endeavors, making valuable contributions to 
                 the success of sports teams and organizations at a professional level."),
          tags$p(class = "shinyapp-text",
                 "Welcome to my ShinyApp, where I have the opportunity to showcase
                 some of the data skills I mentioned earlier. Through this interactive 
                 platform, I aim to demonstrate the immense power of data analysis and 
                 visualization in the sports industry. Explore captivating visualizations, 
                 insightful statistical analysis, and innovative applications of 
                 data-driven decision-making. Whether you're interested in diving 
                 into player performance metrics, game statistics, or exploring the 
                 latest trends in sports analytics, this ShinyApp offers a glimpse 
                 into the exciting possibilities that emerge when data and sports intersect. 
                 Enjoy your exploration and gain a deeper appreciation for the transformative 
                 role of data in shaping the future of sports."),
          tags$p(class = "contact-text",
                 HTML(paste0("Connect with me on <a href='https://www.linkedin.com/in/ghunt04/' target='_blank'>LinkedIn</a><br>")),
                 HTML(paste0("<a href='resume.pdf' download>Download Resume</a>"))
          )
        )
      ),
      tags$div(
        class = "footer",
        HTML("Contact me at: <a href='mailto:ghunt1713@yahoo.com'>ghunt1713@yahoo.com</a>")
      )
    ),
    
    ##### Projects section #####
    
    # Pitching Reports
    tabPanel(
      title = "Pitching Reports",
      id = "projects-tab",
      tags$div(
        class = "projects-background",
        sidebarLayout(
          sidebarPanel(
            # Sidebar inputs
            selectInput("Team", "Select a team:", c("Team 1", "MSU Spartans Fall 22"), selected = "Team 1"),
            selectInput("Player", "Select a player:", choices = NULL),
            selectInput("Game", "Select a game:", choices = NULL),
            tags$img(src = "SpartyBackground.png", class = "image-container")
          ),
          mainPanel(
            tags$div(
              style = "text-decoration: underline",
              h3("Pitcher Report Dashboard")
            ),
            tags$div(
              class = "report-info",
              textOutput("team_info"),
              plotOutput("team_plot", height = "1000px")
            )
          )
        )
      ),
      tags$div(
        class = "footer",
        HTML("Contact me at: <a href='mailto:ghunt1713@yahoo.com'>ghunt1713@yahoo.com</a>")
      )
    ),
    
    # Hitting Reports
    tabPanel(
      title = "Hitting Reports",
      id = "projects-tab",
      tags$div(
        class = "projects-background",
        sidebarLayout(
          sidebarPanel(
            # Sidebar inputs
            selectInput("Batter", "Select a Batter:",
                        choices = c("All")),
            selectInput("BatterDate", "Select a Game:",
                        choices = c("All")),
            selectInput("BatterPitchType", "Select Pitch Type:",
                        choices = c("All")),
            selectInput("PitcherHand", "Select Pitcher Hand:",
                        choices = c("Right", "Left", "All")),
            textInput("ExitVeloAbove", "Enter Minimum Exit Velo: "),  # Text box input
            tags$img(src = "spartanline.jpg", class = "image-container")
          ),
          mainPanel(
            tags$div(
              style = "text-decoration: underline;",
              h3("Hitting Report Dashboard")
            ),
            tags$div(
              class = "report-info",
              textOutput("hitter_info"),
              plotOutput("hitter_plot", height = "1000px")
            )
          )
        )
      ),
      tags$div(
        class = "footer",
        HTML("Contact me at: <a href='mailto:ghunt1713@yahoo.com'>ghunt1713@yahoo.com</a>")
      )
    ),
    
    # Umpire Reports
    tabPanel(
      title = "Umpire Reports",
      id = "projects-tab",
      tags$div(
        class = "projects-background",
        sidebarLayout(
          sidebarPanel(
            # Sidebar inputs
            selectInput("UmpireGame", "Select a Game:",
                        choices = c("All")),
            selectInput("UmpirePitchType", "Select a Pitch Type:",
                        choices = c("All")),
            selectInput("PitcherHandUmpire", "Select Pitcher Hand:",
                        choices = c("Right", "Left", "All")),
            tags$img(src = "shinyspartan.jpg", class = "image-container")
          ),
          mainPanel(
            tags$div(
              style = "text-decoration: underline;",
              h3("Umpire Report Dashboard")
            ),
            tags$div(
              class = "report-info",
              textOutput("umpire_info"),
              plotOutput("umpire_plot", height = "1000px")
            )
          )
        )
      ),
      tags$div(
        class = "footer",
        HTML("Contact me at: <a href='mailto:ghunt1713@yahoo.com'>ghunt1713@yahoo.com</a>")
      )
    ),
    
    # Blast Motion Reports
    tabPanel(
      title = "Blast Reports",
      id = "projects-tab",
      tags$div(
        class = "projects-background",
        sidebarLayout(
          sidebarPanel(
            # Sidebar inputs
            selectInput("BlastPlayer", "Select a Player:",
                        choices = c("All")),
            selectInput("GraphType", "Select a Graph:",
                        choices = c("Blast Factor 3D", "Bat Speed vs. Power / Hand Speed",
                                    "Correlation Matrix")),
            tags$img(src = "baseball.jpg", class = "image-container")
          ),
          mainPanel(
            tags$div(
              style = "text-decoration: underline;",
              h3("Blast Motion Dashboard")
            ),
            tags$div(
              class = "report-info",
              textOutput("blast_info"),
              conditionalPanel(
                condition = "input.GraphType == 'Blast Factor 3D'",
                plotly::plotlyOutput("blast_plot2", height = "750px")
              ),
              conditionalPanel(
                condition = "input.GraphType != 'Blast Factor 3D'",
                plotOutput("blast_plot1", height = "750px")
              )
            )
          )
        )
      ),
      tags$div(
        class = "footer",
        HTML("Contact me at: <a href='mailto:ghunt1713@yahoo.com'>ghunt1713@yahoo.com</a>")
      )
    ),
    
    # Hockey Reports
    tabPanel(
      title = "Hockey Reports",
      id = "projects-tab",
      tags$div(
        class = "projects-background-hockey",
        sidebarLayout(
          sidebarPanel(
            # Sidebar inputs
            selectInput("HockeyGame", "Select a Game:",
                        choices = c("All")),
            selectInput("HockeyPlayer", "Select Player:",
                        choices = c("All")),
            selectInput("HockeyPeriod", "Select Period:",
                        choices = c("1","2","3","All")),
            tags$img(src = "spartanpuck.jpg", class = "image-container")
          ),
          mainPanel(
            tags$div(
              style = "text-decoration: underline;",
              h3("Hockey Report Dashboard")
            ),
            tags$div(
              class = "report-info",
              textOutput("hockey_info"),
              plotOutput("hockey_plot", height = "1000px")
            )
          )
        )
      ),
      tags$div(
        class = "footer",
        HTML("Contact me at: <a href='mailto:ghunt1713@yahoo.com'>ghunt1713@yahoo.com</a>")
      )
    ),
    
    # Fantasy Football Reports
    tabPanel(
      title = "Fantasy Football",
      id = "projects-tab",
      tags$div(
        class = "projects-background-football",
        
        # Main panel for Predicting Fantasy Football Player Title and Plot
        tags$div(
          style = "text-decoration: underline;",
          h3("Fantasy Football Linear Regression Prediction")
        ),
        tags$div(
          class = "report-info",
          plotOutput("football_plot", height = "850px")
        ),
        
        # FluidRow for options (previously in sidebarPanel)
        fluidRow(
          style = "font-weight: bold; font-size: 18px;", # Making the text even bigger
          # For each input or element, adjust the width argument and make font color red and background white using div wrapper
          column(3, tags$div(selectInput("FFBPosition", "Position:", choices = c("QB", "RB", "WR", "TE", "K", "DST")), style = "color: red; background-color: white; padding: 10px;")),   # Make font color red with white background here
          column(3, tags$div(textInput("OneAgo", "Total Fantasy Points Last Year:"), style = "color: red; background-color: white; padding: 10px;")),   # Make font color red with white background here
          column(3, tags$div(textInput("TwoAgo", "Total Fantasy Points Two Years Ago:"), style = "color: red; background-color: white; padding: 10px;")),   # Make font color red with white background here
          column(3, tags$div(textInput("Years", "Total Years Playing:"), style = "color: red; background-color: white; padding: 10px;"))   # Make font color red with white background here
          #column(3, tags$img(src = "ffb.jpg", class = "image-container")),
        ),
        tags$div(
          style = "color: red; font-size: 22px; background-color: white; padding: 10px; font-weight: bold;",
          class = "report-info",
          textOutput("football_info")
        )),
      tags$div(
        class = "footer",
        HTML("Contact me at: <a href='mailto:ghunt1713@yahoo.com'>ghunt1713@yahoo.com</a>")
      )
    )
  )
)

# Define the server logic for the shiny app
server <- function(input, output, session) {
  # Create a reactive value to track whether the code has been executed
  codeExecutedBlast <- reactiveVal(FALSE)
  codeExecutedUmp <- reactiveVal(FALSE)
  codeExecutedBatter <- reactiveVal(FALSE)
  codeExecutedHockey <- reactiveVal(FALSE)
  
  # Observe the input$HockeyPlayer and execute the code only if it hasn't been executed before
  observeEvent(input$HockeyPlayer, {
    data <- read.csv("HockeyData.csv")
    if (!codeExecutedHockey()) {
      players <- unique(data$Name)
      players <- players[players != ""]
      updateSelectInput(session, "HockeyPlayer", choices = c(players, "All"))  # Update player selection choices
      codeExecutedHockey(TRUE)  # Set the codeExecuted reactive value to TRUE to indicate that the code has been executed
    }
    if (input$HockeyPlayer != "All"){
      data <- data[data$Name == input$HockeyPlayer, ]
      periods = unique(data$Period)
      updateSelectInput(session, "HockeyPeriod", choices = c(periods, "All"))  # Update period selection choices
    }
    else{
      updateSelectInput(session, "HockeyPeriod", choices = c("1","2","3", "All"))  # Update period selection choices
    }
  })
  
  # Observe the input$Batter and execute the code only if it hasn't been executed before
  observeEvent(input$Batter, {
    data <- read.csv("Master_Fall_2022.csv")
    if (!codeExecutedBatter()) {
      batters <- unique(data$Batter)
      updateSelectInput(session, "Batter", choices = c(batters, "All"))  # Update player selection choices
      codeExecutedBatter(TRUE)  # Set the codeExecuted reactive value to TRUE to indicate that the code has been executed
    }
    if (input$Batter != "All"){
      data <- data[data$Batter == input$Batter,]
    }
    
    # Select the data of balls in play
    data <- data[data$PlayResult == "Single" | data$PlayResult == "Double" | data$PlayResult == "Triple" | data$PlayResult == "HomeRun" | data$PlayResult == "Out",]
    dates <- unique(data$Date)    # Select the unique Dates that the play has played
    updateSelectInput(session, "BatterDate", choices = c(dates, "All"))  # Update game selection choices
  })
  
  # Observe the input$BatterDate and change the pitch selection
  observeEvent(input$BatterDate, {
    data <- read.csv("Master_Fall_2022.csv")
    data <- data[data$PlayResult == "Single" | data$PlayResult == "Double" | data$PlayResult == "Triple" | data$PlayResult == "HomeRun" | data$PlayResult == "Out",]
    # Sort the data by the pitches the batter has seen
    if (input$Batter != "All"){
      data <- data[data$Batter == input$Batter,]
    }
    if (input$BatterDate != "All")
    {
      data <- data[data$Date == input$BatterDate,]
    }
    pitches <- unique(data$TaggedPitchType)
    pitches <- pitches[pitches != "Undefined"]   # Get rid of the undefined pitches
    updateSelectInput(session, "BatterPitchType", choices = c(pitches, "All"))  # Update pitchtype selection choices
    })  
  
  # Observe the input$BatterPitchType and change the pitcher hand
  observeEvent(input$BatterPitchType, {
    data <- read.csv("Master_Fall_2022.csv")
    data <- data[data$PlayResult == "Single" | data$PlayResult == "Double" | data$PlayResult == "Triple" | data$PlayResult == "HomeRun" | data$PlayResult == "Out",]
    # Sort the data by the pitches the batter has seen
    if (input$Batter != "All"){
      data <- data[data$Batter == input$Batter,]
    }
    if (input$BatterDate != "All")
    {
      data <- data[data$Date == input$BatterDate,]
    }
    if (input$BatterPitchType != "All")
    {
      data <- data[data$TaggedPitchType == input$BatterPitchType, ]
    }
    
    hands <- unique(data$PitcherThrows)
    updateSelectInput(session, "PitcherHand", choices = c(hands, "All"))  # Update PitchHand selection choices
  })
  
  
  # Observe the input$BlastPlayer and execute the code only if it hasn't been executed before
  observeEvent(input$BlastPlayer, {
    if (!codeExecutedBlast()) {
      data <- read.csv("BlastData.csv")
      data <- unique(data$Player)
      updateSelectInput(session, "BlastPlayer", choices = c(data, "All"))  # Update player selection choices
      codeExecutedBlast(TRUE)  # Set the codeExecuted reactive value to TRUE to indicate that the code has been executed
    }
  })
  
  # Observe the input$UmpireGame and execute the code only if it hasn't been executed before
  observeEvent(input$UmpireGame, {
    data <- read.csv("Master_Fall_2022.csv")
    if (!codeExecutedUmp()) {
      dates <- unique(data$Date)
      updateSelectInput(session, "UmpireGame", choices = c(dates, "All"))  # Update game selection choices
      codeExecutedUmp(TRUE)  # Set the codeExecuted reactive value to TRUE to indicate that the code has been executed
    }
    if (input$UmpireGame != "All")
    {
      data <- data[data$Date == input$UmpireGame,]
    }
    pitches <- unique(data$TaggedPitchType)
    pitches <- pitches[pitches != "Undefined"]   # Get rid of the undefined pitches
    updateSelectInput(session, "UmpirePitchType", choices = c(pitches, "All"))  # Update pitchtype selection choices
  })
  
  # Observe the input$BatterPitchType and change the pitcher hand
  observeEvent(input$UmpirePitchType, {
    data <- read.csv("Master_Fall_2022.csv")
    # Sort the data by the pitches the batter has seen
    if (input$UmpireGame != "All")
    {
      data <- data[data$Date == input$UmpireGame,]
    }
    if (input$UmpirePitchType != "All")
    {
      data <- data[data$TaggedPitchType == input$UmpirePitchType,]
    }
    
    hands <- unique(data$PitcherThrows)
    updateSelectInput(session, "PitcherHandUmpire", choices = c(hands, "All"))  # Update PitchHand selection choices
  })
  
  observeEvent(input$BlastPlayer, {
    if (input$BlastPlayer == "All") {
      updateSelectInput(session, "GraphType", choices = c("Blast Factor 3D", "Bat Speed vs. Power / Hand Speed",
                                                          "Correlation Matrix", "Blast Factor vs. WOBA"))  # Update player selection choices
    } else {
      updateSelectInput(session, "GraphType", choices = c("Blast Factor 3D", "Bat Speed vs. Power / Hand Speed",
                                                          "Correlation Matrix"))  # Update player selection choices
    }
  })
  
  observeEvent(input$Team, {
    # When the team selection changes, update the choices for player selection
    if (input$Team == "Team 1") {
      data <- read.csv("AnalyticsQuestionnairePitchData.csv")
      game_pitchers <- sort(unique(na.omit(data$PitcherId)))  # Remove rows with missing values and sort in ascending order
      updateSelectInput(session, "Player", choices = c(game_pitchers, "All"))  # Update player selection choices
    } else if (input$Team == "MSU Spartans Fall 22") {
      data <- read.csv("Master_Fall_2022.csv")
      game_pitchers <- sort(unique(na.omit(data$Pitcher)))  # Remove rows with missing values and sort in ascending order
      updateSelectInput(session, "Player", choices = c(game_pitchers, "All"))  # Update player selection choices
    }
  })
  
  observeEvent(input$Player, {
    selected_pitcher <- input$Player
    if (input$Team == "MSU Spartans Fall 22") {
      data <- read.csv("Master_Fall_2022.csv")
      if (selected_pitcher != "All") {
        # Filter the game dates based on the selected pitcher's ID
        game_dates <- unique(data[data$Pitcher == selected_pitcher, "Date"])
      } else {
        game_dates <- unique(data$Date)
      }
      game_dates <- unique(na.omit(game_dates))  # Remove rows with missing values and sort in reverse order
      updateSelectInput(session, "Game", choices = c(game_dates, "All"))  # Update game selection choices
    } else {
      data <- read.csv("AnalyticsQuestionnairePitchData.csv")
      if (selected_pitcher != "All") {
        # Filter the game dates based on the selected pitcher's ID
        game_dates <- unique(data[data$PitcherId == selected_pitcher, "GamePk"])
      } else {
        game_dates <- unique(data$GamePk)
      }
      game_dates <- unique(na.omit(game_dates))  # Remove rows with missing values and sort in reverse order
      if (length(game_dates) > 1) {
        game_dates <- c(game_dates, "Both")
      }
      updateSelectInput(session, "Game", choices = c(game_dates))  # Update game selection choices
    }
  })
  
  # Define a reactive expression that returns the team information based on the user's selection
  team_info <- reactive({
    switch(
      input$Player,
      if (input$Player != "All") {
        if (input$Game != "Both" && input$Game != "All") {
          paste("Pitching Analysis for pitcher", input$Player, "in Game", input$Game)
        } else if (input$Game == "Both") {
          paste("Pitching Analysis for pitcher", input$Player, "in both games")
        } else {
          paste("Pitching Analysis for pitcher", input$Player, "all season")
        }
      } else {
        if (input$Game != "Both" && input$Game != "All") {
          paste("Pitching Analysis for all pitchers in Game", input$Game)
        } else if (input$Game == "Both") {
          paste("Pitching Analysis for all pitchers in both games")
        } else {
          paste("Pitching Analysis for all pitchers all season")
        }
      }
    )
  })
  
  # Define a reactive expression that returns the team data based on the user's selection
  team_data <- reactive({
    if (input$Team == "Team 1") {
      if (input$Game != "Both") {
        data <- read.csv("AnalyticsQuestionnairePitchData.csv")
        return(data[data$GamePk == input$Game, ])  # Return data for the selected game
      } else {
        return(read.csv("AnalyticsQuestionnairePitchData.csv"))  # Return all data
      }
    } else if (input$Team == "MSU Spartans Fall 22") {
      if (input$Game != "All") {
        data <- read.csv("Master_Fall_2022.csv")
        return(data[data$Date == input$Game, ])  # Return data for the selected game
      } else {
        return(read.csv("Master_Fall_2022.csv"))  # Return all data
      }
    }
  })
  # Define a reactive expression that returns the team plot based on the user's selection
  team_plot <- reactive({
    pitch_data <- team_data()  # Load the team data
    
    if (input$Team == "Team 1") {
      pitch_colors <- c("FF" = "red", "SL" = "blue", "CU" = "green", "CH" = "orange",
                        "FC" = "purple", "SI" = "pink", "KC" = "yellow")  # Define colors for each pitch type
      
      pitch_data = pitch_data[pitch_data$PitchType != "NULL", ]  # Remove rows with missing values in PitchType column
      pitch_type_col <- "PitchType"
      break_induced_col <- "TrajectoryVerticalBreakInduced"
      break_col <- "TrajectoryHorizontalBreak"
      release_x_col <- "ReleasePositionX"
      release_y_col <- "ReleaseAngle"
      release_speed_col <- "ReleaseSpeed"
      release_extension_col <- "ReleaseExtension"
      release_spinrate_col <- "ReleaseSpinRate"
      loc_x_col <- "TrajectoryLocationX"
      loc_y_col <- "TrajectoryLocationZ"
      
    } else if (input$Team == "MSU Spartans Fall 22") {
      pitch_colors <- c("Fastball" = "red", "Slider" = "blue", "Curveball" = "green", "ChangeUp" = "orange",
                        "Cutter" = "purple", "Sinker" = "pink", "Knuckle-Curve" = "yellow")  # Define colors for each pitch type
      
      pitch_data = pitch_data[pitch_data$TaggedPitchType != "NULL", ]  # Remove rows with missing values in TaggedPitchType column
      pitch_type_col <- "TaggedPitchType"
      break_induced_col <- "InducedVertBreak"
      break_col <- "HorzBreak"
      release_x_col <- "RelSide"
      release_y_col <- "RelHeight"
      release_speed_col <- "RelSpeed"
      release_extension_col <- "Extension"
      release_spinrate_col <- "SpinRate"
      loc_x_col <- "PlateLocSide"
      loc_y_col <- "PlateLocHeight"
    }
    
    if (input$Team == "MSU Spartans Fall 22" && input$Player != "All") {
      pid_data <- pitch_data[pitch_data$Pitcher == input$Player, ]  # Filter data for the selected player
    } else if (input$Team == "Team 1" && input$Player != "All") {
      pid_data <- pitch_data[pitch_data$PitcherId == input$Player, ]  # Filter data for the selected player
    } else {
      pid_data <- pitch_data  # Use all data for "All" players
    }
    
    pid_data <- subset(pid_data, !is.na(get(pitch_type_col)))  # Remove rows with missing values in pitch type column
    dot_size <- 2.5
    
    # Plot a histogram of pitch speeds
    velo_plot <- ggplot(pid_data, aes(x = get(release_speed_col), fill = get(pitch_type_col))) +
      geom_histogram(binwidth = 1) +
      scale_fill_manual(values = pitch_colors, name = "Pitch Type") +
      labs(
        title = "Distribution of Pitch Speeds by Pitch Type",
        x = "Pitch Speed (mph)",
        y = "Frequency"
      ) +
      theme(plot.title = element_text(hjust = 0.5))
    
    # Plot a graph of spin rates
    spinrate_plot <- ggplot(pid_data, aes(x = get(release_extension_col), y = get(release_spinrate_col), color = get(pitch_type_col))) +
      geom_point(size = dot_size) +  # Modify size of dots
      scale_color_manual(values = pitch_colors) +
      labs(
        title = "Extension vs. Spin Rate",
        x = "Extension (ft)",
        y = "Spin Rate (rpm)",
        color = "Pitch Type"
      ) +
      theme(plot.title = element_text(hjust = 0.5))
    
    # Define a variable for break values
    if (input$Team == "Team 1") {
      vertical_break <- pid_data[[break_induced_col]] * 12
      horizontal_break <- pid_data[[break_col]] * 12
    } else {
      vertical_break <- pid_data[[break_induced_col]]
      horizontal_break <- pid_data[[break_col]]
    }
    
    # Plot a graph of vertical/horizontal break
    vertical_break_plot <- ggplot(pid_data, aes(x = get(pitch_type_col), y = vertical_break, color = get(pitch_type_col))) +
      geom_boxplot() +
      scale_color_manual(values = pitch_colors) +
      labs(
        title = "Vertical Break by Pitch Type",
        x = "Pitch Type",
        y = "Vertical Break (in)",
        color = "Pitch Type"
      ) +
      theme(plot.title = element_text(hjust = 0.5))
    
    horizontal_break_plot <- ggplot(pid_data, aes(x = get(pitch_type_col), y = horizontal_break, color = get(pitch_type_col))) +
      geom_boxplot() +
      scale_color_manual(values = pitch_colors) +
      labs(
        title = "Horizontal Break by Pitch Type",
        x = "Pitch Type",
        y = "Horizontal Break (in)",
        color = "Pitch Type"
      ) +
      theme(plot.title = element_text(hjust = 0.5))
    
    # Plot a graph of release points
    release_plot <- ggplot(pid_data, aes(x = get(release_x_col), y = get(release_y_col), color = get(pitch_type_col))) +
      geom_point(size = dot_size) +  # Modify size of dots
      scale_color_manual(values = pitch_colors) +
      labs(
        title = "Release Position vs. Release Angle",
        x = "Horizontal Location (ft)",
        y = "Release Angle",
        color = "Pitch Type"
      ) +
      theme(plot.title = element_text(hjust = 0.5))
    
    # Plot a graph of the pitch location
    strike_zone_plot <- ggplot(pid_data, aes(x = get(loc_x_col), y = get(loc_y_col), color = get(pitch_type_col))) +
      geom_point(size = dot_size) +  # Modify size of dots
      scale_color_manual(values = pitch_colors) +
      geom_rect(aes(xmin = -0.7083, xmax = 0.7083, ymin = 1.5, ymax = 3.5), fill = NA, color = "black") +
      labs(
        title = "Strike Zone Plot",
        x = "X Location",
        y = "Y Location",
        color = "Pitch Type"
      ) +
      theme(plot.title = element_text(hjust = 0.5)) + xlim(-2.75, 2.75) + ylim(-0.25, 4.75)
    
    # Plot a pitch location heat map
    total_obs <- nrow(pid_data)
    pitch_heat_map <- ggplot(pid_data, aes(x = get(loc_x_col), y = get(loc_y_col))) +
      geom_bin2d(binwidth = c(0.5, 0.5), aes(fill = round(after_stat(count) / total_obs * 100))) +
      geom_rect(aes(xmin = -0.7083, xmax = 0.7083, ymin = 1.5, ymax = 3.5), fill = NA, color = "black") +
      scale_fill_gradient(low = "white", high = "red", name = "Count (%)") +
      labs(
        title = "Pitch Location Heat Map",
        x = "X Location",
        y = "Y Location",
        fill = "Count"
      ) +
      theme(plot.title = element_text(hjust = 0.5)) + xlim(-2.75, 2.75) + ylim(-0.25, 4.75)
    
    # Plot a graph of pitch type usage
    pitch_usage <- ggplot(pid_data, aes(x = get(pitch_type_col), fill = get(pitch_type_col))) +
      geom_bar() +
      scale_fill_manual(values = pitch_colors, name = "Pitch Type") +
      labs(
        title = "Pitch Type Usage",
        x = "Pitch Type",
        y = "Count"
      ) +
      theme(plot.title = element_text(hjust = 0.5))
    
    
    velo_plot <- velo_plot +
      theme(plot.title = element_text(size = 16),
            axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14))
    
    spinrate_plot <- spinrate_plot +
      theme(plot.title = element_text(size = 16),
            axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14))
    
    vertical_break_plot <- vertical_break_plot +
      theme(plot.title = element_text(size = 16),
            axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14))
    
    horizontal_break_plot <- horizontal_break_plot +
      theme(plot.title = element_text(size = 16),
            axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14))
    
    release_plot <- release_plot +
      theme(plot.title = element_text(size = 16),
            axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14))
    
    strike_zone_plot <- strike_zone_plot +
      theme(plot.title = element_text(size = 16),
            axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14))
    
    pitch_heat_map <- pitch_heat_map +
      theme(plot.title = element_text(size = 16),
            axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14))
    
    pitch_usage <- pitch_usage +
      theme(plot.title = element_text(size = 16),
            axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14))
    
    # Determine the data source based on the selected team
    data_source <- if (input$Team == "Team 1") {
      "Hawk-Eye Data"
    } else if (input$Team == "MSU Spartans Fall 22") {
      "Trackman Data"
    } else {
      "Unknown Data Source"
    }
    
    # Combine the plots using grid.arrange()
    grid.arrange(
      grobs = list(velo_plot, spinrate_plot, vertical_break_plot, horizontal_break_plot,
                   release_plot, strike_zone_plot, pitch_heat_map, pitch_usage),
      ncol = 1,
      layout_matrix = rbind(c(1, 2), c(3, 4), c(5, 6), c(7, 8)),
      bottom = NULL,
      top = textGrob(paste("Made from", data_source), gp = gpar(fontsize = 20)),
      left = NULL,
      right = NULL
    )
  })
  
  # Display the team information in the text output area
  output$team_info <- renderText({
    team_info()
  })
  
  # Display the team plot in the plot output area
  output$team_plot <- renderPlot({
    team_plot()
  })
  
  # Define a reactive expression that returns the umpire information based on the user's selection
  umpire_info <- reactive({
    switch(
      input$Player,
      if (input$UmpireGame != "All") {
        paste("Umpire Report for", input$UmpireGame)  # Generate umpire report for a specific game
      } else {
        paste("Umpire Report for All Games")  # Generate umpire report for all games
      }
    )
  })
  
  # Define a reactive expression that generates the umpire information and plot based on the user's selection
  
  # Reactive expression to generate umpire information
  umpire_info <- reactive({
    switch(
      input$UmpireGame,
      if (input$UmpireGame != "All") {
        paste("Umpire Report for", input$UmpireGame)  # Generate umpire report for a specific game
      } else {
        paste("Umpire Report for All Games")  # Generate umpire report for all games
      }
    )
  })
  
  # Reactive expression to generate the umpire plot
  umpire_plot <- reactive({
    
    # Function to calculate accuracy
    calculate_accuracy <- function(df) {
      # Calculate the number of correct called balls and strikes
      df <- df[df$PitchCall == "BallCalled" | df$PitchCall == "StrikeCalled", ]
      correct_called_balls <- sum(df$PitchCall == "BallCalled" & (df$PlateLocSide < -0.7083 | df$PlateLocSide > 0.7083 | df$PlateLocHeight < 1.5 | df$PlateLocHeight > 3.5))
      correct_called_strikes <- sum(df$PitchCall == "StrikeCalled" & df$PlateLocSide >= -0.7083 & df$PlateLocSide <= 0.7083 & df$PlateLocHeight >= 1.5 & df$PlateLocHeight <= 3.5)
      
      # Calculate the total number of balls and strikes
      total_balls <- sum(df$PlateLocSide < -0.7083 | df$PlateLocSide > 0.7083 | df$PlateLocHeight < 1.5 | df$PlateLocHeight > 3.5)
      total_strikes <- sum(df$PlateLocSide >= -0.7083 & df$PlateLocSide <= 0.7083 & df$PlateLocHeight >= 1.5 & df$PlateLocHeight <= 3.5)
      
      # Calculate ball and strike accuracy
      ball_accuracy <- correct_called_balls / total_balls
      strike_accuracy <- correct_called_strikes / total_strikes
      
      # Return accuracy metrics
      return(list(ball_accuracy = ball_accuracy, strike_accuracy = strike_accuracy,
                  correct_balls = correct_called_balls, total_balls = total_balls,
                  correct_strikes = correct_called_strikes, total_strikes = total_strikes))
    }
    
    # Function to generate a strike zone plot for incorrect called balls
    plot_incorrect_balls <- function(df) {
      incorrect_calls <- df %>%
        filter(PitchCall == "BallCalled" & (PlateLocSide >= -0.7083 & PlateLocSide <= 0.7083 & PlateLocHeight >= 1.5 & PlateLocHeight <= 3.5))
      
      p <- ggplot(incorrect_calls, aes(x = PlateLocSide, y = PlateLocHeight)) +
        geom_point(color = "green", size = 3) +
        annotate("rect", xmin = -0.7083, xmax = 0.7083, ymin = 1.5, ymax = 3.5,
                 fill = NA, color = "black", size = 1) +
        annotate("rect", xmin = -0.8305, xmax = 0.8305, ymin = 1.3775, ymax = 3.6225,
                 fill = NA, color = "#555555", size = 1, linetype = "dashed") +
        coord_cartesian(xlim = c(-3, 3), ylim = c(0, 5)) +
        labs(x = "Horizontal Position", y = "Vertical Position") +
        ggtitle("Incorrect Called Balls") +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5))
      
      return(p)
    }
    
    # Function to generate a strike zone plot for incorrect called strikes
    plot_incorrect_strikes <- function(df) {
      incorrect_calls <- df %>%
        filter(PitchCall == "StrikeCalled" & (PlateLocSide < -0.7083 | PlateLocSide > 0.7083 | PlateLocHeight < 1.5 | PlateLocHeight > 3.5))
      
      p <- ggplot(incorrect_calls, aes(x = PlateLocSide, y = PlateLocHeight)) +
        geom_point(color = "red", size = 3) +
        annotate("rect", xmin = -0.7083, xmax = 0.7083, ymin = 1.5, ymax = 3.5,
                 fill = NA, color = "black", size = 1) +
        annotate("rect", xmin = -0.8305, xmax = 0.8305, ymin = 1.3775, ymax = 3.6225,
                 fill = NA, color = "#555555", size = 1, linetype = "dashed") +
        coord_cartesian(xlim = c(-3, 3), ylim = c(0, 5)) +
        labs(x = "Horizontal Position", y = "Vertical Position") +
        ggtitle("Incorrect Called Strikes") +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5))
      
      return(p)
    }
    
    # Function to generate a pie chart
    plot_accuracy_pie <- function(accuracy, title, pitch) {
      df <- data.frame(
        variable = c("Correct", "Incorrect"),
        accuracy = c(accuracy, 1 - accuracy)
      )
      
      color <- ifelse(pitch == "strike", "green", ifelse(pitch == "both", "gold", "red"))
      p <- ggplot(df, aes(x = "", y = accuracy, fill = variable)) +
        geom_bar(stat = "identity", width = 1, color = NA) +
        coord_polar(theta = "y") +
        labs(fill = NULL) +
        scale_fill_manual(values = c("Correct" = color, "Incorrect" = "white"), guide = FALSE) +
        theme_void() +
        ggtitle(title) +
        theme(plot.title = element_text(hjust = 0.5)) +
        annotate("text", x = -0.75, y = 0, label = paste0(scales::percent(accuracy)),
                 vjust = 0.4, hjust = 0.45, size = 8)
      
      return(p)
    }
    
    # Load Trackman data
    trackman_data <- read.csv("Master_Fall_2022.csv")
    
    # Filter by Pitch Type
    if (input$UmpirePitchType != "All")
    {
      trackman_data <- trackman_data[trackman_data$TaggedPitchType == input$UmpirePitchType,]
    }
    
    # Filter Trackman data based on selected game
    if (input$UmpireGame != "All") {
      trackman_data <- trackman_data[trackman_data$Date == input$UmpireGame, ]
    }
    
    # Filter Trackman data based on selected pitch hand
    if (input$PitcherHandUmpire != "All")
    {
      trackman_data <- trackman_data[trackman_data$PitcherThrows == input$PitcherHandUmpire,]
    }
    
    # Calculate accuracy
    accuracy <- calculate_accuracy(trackman_data)
    
    # Generate plots and pie charts
    incorrect_balls_plot <- plot_incorrect_balls(trackman_data)
    incorrect_strikes_plot <- plot_incorrect_strikes(trackman_data)
    strike_accuracy_pie <- plot_accuracy_pie(accuracy$strike_accuracy, "In-Zone Accuracy", "strike")
    ball_accuracy_pie <- plot_accuracy_pie(accuracy$ball_accuracy, "Out-Zone Accuracy", "ball")
    overall_accuracy <- (accuracy$correct_balls + accuracy$correct_strikes) / (accuracy$total_balls + accuracy$total_strikes)
    overall_accuracy_pie <- plot_accuracy_pie(overall_accuracy, "Overall Accuracy", "both")
    
    # Modify titles and axis titles
    incorrect_balls_plot <- incorrect_balls_plot +
      theme(plot.title = element_text(size = 14),
            axis.title.x = element_text(size = 12),
            axis.title.y = element_text(size = 12))
    
    incorrect_strikes_plot <- incorrect_strikes_plot +
      theme(plot.title = element_text(size = 14),
            axis.title.x = element_text(size = 12),
            axis.title.y = element_text(size = 12))
    
    strike_accuracy_pie <- strike_accuracy_pie +
      theme(plot.title = element_text(size = 14),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text = element_blank(),
            legend.title = element_text(size = 12),
            legend.text = element_text(size = 12))
    
    ball_accuracy_pie <- ball_accuracy_pie +
      theme(plot.title = element_text(size = 14),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text = element_blank(),
            legend.title = element_text(size = 12),
            legend.text = element_text(size = 12))
    
    overall_accuracy_pie <- overall_accuracy_pie +
      theme(plot.title = element_text(size = 14),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text = element_blank(),
            legend.title = element_text(size = 12),
            legend.text = element_text(size = 12))
    
    # Arrange plots and pie charts in a grid
    grid.arrange(arrangeGrob(incorrect_balls_plot, incorrect_strikes_plot, ncol = 2),
                 arrangeGrob(strike_accuracy_pie, overall_accuracy_pie, ball_accuracy_pie, ncol = 3),
                 ncol = 1, top = textGrob("Accuracy Visualization", gp = gpar(fontsize = 20)),
                 heights = c(1, 1))
  })
  
  # Display the umpire information in the text output area
  output$umpire_info <- renderText({
    umpire_info()
  })
  
  # Display the umpire plot in the plot output area
  output$umpire_plot <- renderPlot({
    umpire_plot()
  })
  
  # Define a reactive expression that returns the umpire information based on the user's selection
  blast_info <- reactive({
    switch(
      input$BlastPlayer,
      if (input$BlastPlayer != "All") {
        paste("Blast Motion Report for", input$BlastPlayer)  # Generate Blast Motion report for a specific player
      } else {
        paste("Blast Motion Report for All Players")  # Generate Blast Motion report for all players
      }
    )
  })
  
  # Reactive expression to generate the bat speed histogram plot
  blast_plot_bat_speed_hist <- reactive({
    
    # Read Blast Motion data
    data <- read.csv("BlastData.csv")
    
    # Filter data based on selected player
    if (input$BlastPlayer != "All") {
      data <- data[data$Player == input$BlastPlayer, ]
    }
    
    # Convert columns to appropriate data types
    data$Player <- as.factor(data$Player)
    data$Swing <- as.integer(data$Swing)
    data$Bat_Speed <- as.numeric(data$Bat.Speed..mph.)
    data$Attack_Angle <- as.numeric(data$Attack.Angle..deg.)
    data$Time_to_Contact <- as.numeric(data$Time.to.Contact..sec.)
    data$Peak_Hand_Speed <- as.numeric(data$Peak.Hand.Speed..mph.)
    data$Blast_Factor <- as.numeric(data$Blast.Factor)
    data$On_Plane <- as.numeric(data$On.Plane....)
    data$Power <- as.numeric(data$Power..kW.)
    data$Peak_Bat_Speed <- as.numeric(data$Peak.Bat.Speed..mph.)
    data$Body_Rotation <- as.numeric(data$Body.Rotation....)
    data$Vertical_Bat_Angle <- as.numeric(data$Vertical.Bat.Angle..deg.)
    data$Woba <- as.numeric(data$Season.Woba)
    
    # Filter data for bat speeds greater than 50 mph
    data <- data[data$Bat_Speed > 50, ]
    
    # Function to calculate and format correlation coefficient
    get_corr_text <- function(x, y) {
      corr_value <- cor(x, y)
      corr_text <- paste("Correlation:", round(corr_value, 2))
      return(corr_text)
    }
    
    # Create a plot object for Bat_Speed vs. Power and Peak Hand Speed
    plot1 <- plot(data$Bat_Speed, data$Power, pch = 16, cex = 1.5, axes = FALSE, ylim = c(ceiling((min(data$Power)-1)), ceiling((max(data$Power)+1))),
                  xlab = "Bat Speed (mph)", ylab = "Power (kW)", main = "Bat Speed vs. Power and Peak Hand Speed")
    points(data$Bat_Speed, data$Power, pch = 16, cex = 1.5, col = "black")
    abline(lm(data$Power ~ data$Bat_Speed), lty = 2)  # Add dashed trendline
    axis(2, ylim = c(ceiling((min(data$Power)-1)), ceiling((max(data$Power)+1))), col = "black", las = 1)
    box()
    
    # Add background lines
    grid(col = "gray", lty = "dashed")
    
    # Add a second plot on the same graph for Bat_Speed vs. Peak_Hand_Speed
    par(new = TRUE)
    plot(data$Bat_Speed, data$Peak_Hand_Speed, pch = 16, cex = 1.5, xlab = "", ylab = "", ylim = c((min(data$Peak_Hand_Speed)-(0.5*(min(data$Peak_Hand_Speed)))), (max(data$Peak_Hand_Speed)+1)),
         col = "red", yaxt = "n")
    points(data$Bat_Speed, data$Peak_Hand_Speed, pch = 16, col = "red", cex = 1.5)
    abline(lm(data$Peak_Hand_Speed ~ data$Bat_Speed), lty = 2, col = "red")  # Add dashed trendline
    axis(4, ylim = c((min(data$Peak_Hand_Speed)-(0.5*(min(data$Peak_Hand_Speed)))), (max(data$Peak_Hand_Speed)+1)), col = "red", col.axis = "red", las = 1)
    mtext("Peak Hand Speed (mph)", side = 4, col = "red", line = 2.5)
    
    # Draw the Bat Speed axis
    axis(1, pretty(range(data$Bat_Speed), 10))
    
    # Add legend
    legend("topleft", legend = c("Power", "Peak Hand Speed"),
           text.col = c("black", "red"), pch = 16, col = c("black", "red"))
    
    # Add correlations
    text(
      min(data$Bat_Speed) + (0.8 * (max(data$Bat_Speed) - min(data$Bat_Speed))),
      (0.7 * min(data$Peak_Hand_Speed)),
      get_corr_text(data$Bat_Speed, data$Power),
      cex = 1.5  # Adjust the value to increase or decrease the font size
    )
    text(
      min(data$Bat_Speed) + (0.7 * (max(data$Bat_Speed) - min(data$Bat_Speed))),
      max(data$Peak_Hand_Speed) + 0.5,
      get_corr_text(data$Bat_Speed, data$Peak_Hand_Speed),
      cex = 1.5  # Adjust the value to increase or decrease the font size
    )
    
    # Assign the plot to a variable
    bat_speed_peak_hand_speed_power <- plot1
    
    if (input$GraphType == "Bat Speed vs. Power / Hand Speed") {
      return(bat_speed_peak_hand_speed_power)  # Return the plot for Bat Speed vs. Power and Peak Hand Speed
    } else {
      # Create correlation matrix
      cor_matrix <- cor(data[, c("Bat_Speed", "Attack_Angle", "Time_to_Contact", "Peak_Hand_Speed", "Blast_Factor", "On_Plane", "Power", "Peak_Bat_Speed", "Body_Rotation", "Vertical_Bat_Angle")])
      
      # Remove underscores from labels
      colnames(cor_matrix) <- gsub("_", " ", colnames(cor_matrix))
      rownames(cor_matrix) <- gsub("_", " ", rownames(cor_matrix))
      
      # Create correlation matrix plot
      matrix <- corrplot(cor_matrix, method = "color", type = "full", tl.col = "black", tl.srt = 45,
                         pch.col = "black", addCoef.col = "black", number.cex = 0.8, number.digits = 2,
                         title = "Correlation Matrix", mar = c(0, 0, 1, 0))
      
      if (input$GraphType == "Correlation Matrix") {
        return(matrix)  # Return the correlation matrix plot
      } else if (input$GraphType == "Blast Factor vs. WOBA") {
        # Calculate average Blast Factor and Woba for each player
        averages <- aggregate(cbind(Blast_Factor, Woba) ~ Player, data, function(x) mean(x, na.rm = TRUE))
        
        averages <- averages[averages$Blast_Factor >= 75,]
        
        # Fit a quadratic trendline
        model <- glm(Woba ~ poly(Blast_Factor, 2, raw = TRUE), data = averages)
        
        # Create histogram plot with colors by Player and trendline
        histogram <- ggplot(averages, aes(x = Blast_Factor, y = Woba)) +
          geom_bar(stat = "identity", width = 1.5, aes(fill = Player)) +
          labs(x = "Average Blast Factor", y = "Player Woba", title = "Average Blast Factor vs. Woba") +
          theme(
            plot.title = element_text(size = 20, hjust = 0.5),  # Increase the size parameter to make the title bigger
            axis.title = element_text(size = 18),  # Increase the size parameter to make the axis titles bigger
            axis.text = element_text(size = 16),  # Increase the size parameter to make the axis tick labels bigger
            legend.title = element_text(size = 18)  # Increase the size parameter to make the legend title bigger
          )
        
        # Add quadratic trendline
        histogram <- histogram + geom_smooth(method = "glm", formula = y ~ poly(x, 2, raw = TRUE), se = FALSE, color = "black", linetype = "dashed") +
          annotate(
            "text",
            x = min(averages$Blast_Factor) + 0.175 * diff(range(averages$Blast_Factor)),
            y = (max(model$fitted.values)),
            label = paste("y =", round(coef(model)[1], 2), "-", abs(round(coef(model)[2], 2)), "x +", round(coef(model)[3], 4), "x^2"),
            size = 8  # Increase the size parameter to make the text on the plot bigger
          )
        
        return(histogram)  # Return the histogram plot for Blast Factor vs. WOBA
      }
    }
  })
  
  # Reactive expression to generate the 3D scatter plot
  blast_plot_3d <- reactive({
    # Read Blast Motion data
    data <- read.csv("BlastData.csv")
    
    # Filter data based on selected player
    if (input$BlastPlayer != "All") {
      data <- data[data$Player == input$BlastPlayer, ]
    }
    
    # Convert columns to appropriate data types
    data$Player <- as.factor(data$Player)
    data$Swing <- as.integer(data$Swing)
    data$Bat_Speed <- as.numeric(data$Bat.Speed..mph.)
    data$Attack_Angle <- as.numeric(data$Attack.Angle..deg.)
    data$Time_to_Contact <- as.numeric(data$Time.to.Contact..sec.)
    data$Peak_Hand_Speed <- as.numeric(data$Peak.Hand.Speed..mph.)
    data$Blast_Factor <- as.numeric(data$Blast.Factor)
    data$On_Plane <- as.numeric(data$On.Plane....)
    data$Power <- as.numeric(data$Power..kW.)
    data$Peak_Bat_Speed <- as.numeric(data$Peak.Bat.Speed..mph.)
    data$Body_Rotation <- as.numeric(data$Body.Rotation....)
    data$Vertical_Bat_Angle <- as.numeric(data$Vertical.Bat.Angle..deg.)
    data$Woba <- as.numeric(data$Season.Woba)
    
    # Filter data for bat speeds greater than 50 mph
    data <- data[data$Bat_Speed > 50, ]
    
    # Create 3D scatter plot
    scatter_3d <- plot_ly(
      data,
      x = ~Bat_Speed,
      y = ~Power,
      z = ~Blast_Factor,
      type = "scatter3d",
      mode = "markers",
      marker = list(
        color = ~Blast_Factor,
        colorscale = list(c(0, 1), c("blue", "red")),
        colorbar = list(title = list(text = "Blast Factor", font = list(size = 20)))
      )
    ) %>%
      layout(
        scene = list(
          xaxis = list(title = list(text = "Bat Speed (mph)", font = list(size = 18, color = "black"))),
          yaxis = list(title = list(text = "Power (kW)", font = list(size = 18, color = "black"))),
          zaxis = list(title = list(text = "Blast Factor", font = list(size = 18, color = "black")))
        ),
        title = list(
          text = "Blast Factor 3D Scatter Plot",
          y = 0.925,  # Adjust the value to position the title vertically
          x = 0.5,  # Adjust the value to position the title horizontally
          font = list(size = 24, color = "black")
        )
      )
    
    return(scatter_3d)  # Return the 3D scatter plot
  })
  
  # Display the blast information in the text output area
  output$blast_info <- renderText({
    blast_info()
  })
  
  # Display the blast plot in the plot output area
  output$blast_plot1 <- renderPlot({
    blast_plot_bat_speed_hist()
  })
  
  # Display the blast plot in the plot output area
  output$blast_plot2 <- plotly::renderPlotly({
    blast_plot_3d()
  })
  
  # Define a reactive expression that returns the hitting information based on the user's selection
  hitter_info <- reactive({
    switch(
      input$Batter,
      if (input$Batter != "All") {
        paste("Hitting Report for", input$Batter, ifelse(input$BatterDate != "All", paste0("(", input$BatterDate, ")"), "(All Games)"))  # Generate Hitting report for a specific player
      } else {
        paste("Hitting Report for All Players", ifelse(input$BatterDate != "All", paste0("(", input$BatterDate, ")"), "(All Games)"))  # Generate Hitting report for all players
      }
    )
  })
  
  # Reactive expression to generate the Hitter reports
  hitter_plot <- reactive({
    # Load Trackman data
    trackman_data <- read.csv("Master_Fall_2022.csv")
    if (input$Batter != "All") {
      trackman_data <- trackman_data[trackman_data$Batter == input$Batter,]
    }
    if (input$BatterDate != "All") {
      trackman_data <- trackman_data[trackman_data$Date == input$BatterDate,]
    }
    if (input$BatterPitchType != "All") {
      trackman_data <- trackman_data[trackman_data$TaggedPitchType == input$BatterPitchType,]
    }
    if (input$PitcherHand != "All")
    {
      trackman_data <- trackman_data[trackman_data$PitcherThrows == input$PitcherHand,]
    }
    if (!(is.null(input$ExitVeloAbove) || input$ExitVeloAbove == "" || input$ExitVeloAbove == "All"))
    {
      trackman_data <- subset(trackman_data, ExitSpeed > as.numeric(input$ExitVeloAbove))
    }
    
    pitch_colors <- c("Fastball" = "red", "Slider" = "blue", "Curveball" = "green", "ChangeUp" = "orange",
                      "Cutter" = "purple", "Sinker" = "pink", "Knuckle-Curve" = "yellow")  # Define colors for each pitch type
    
    trackman_data <- trackman_data[trackman_data$PlayResult %in% c("Single", "Double", "Triple", "HomeRun", "Out"), ]
    trackman_data <- trackman_data[!is.na(trackman_data$ExitSpeed), ]
    
    # Define the order of the PlayResult variable
    trackman_data$PlayResult <- factor(trackman_data$PlayResult, levels = c("Single", "Double", "Triple", "HomeRun", "Out"))
    
    # Plot a bar graph of the average exit velocities based on hit type / out
    exit_velo_plot <- ggplot(trackman_data, aes(x = PlayResult, y = ExitSpeed, fill = PlayResult)) +
      stat_summary(fun.y = "mean", geom = "bar") +
      labs(
        title = "Average Exit Velocities by Hit Type/Out",
        x = "Hit Type/Out",
        y = "Exit Velocity",
        fill = "Hit Type/Out"
      ) +
      theme(plot.title = element_text(hjust = 0.5))
    
    # Combine "Single," "Double," "Triple," and "HomeRun" into "Hit" category
    trackman_data$PlayResult <- ifelse(trackman_data$PlayResult %in% c("Single", "Double", "Triple", "HomeRun"), "Hit", "Out")
    
    # Create a new dataset for hits only
    hits_data <- trackman_data[trackman_data$PlayResult == "Hit", ]
    
    # Create a new dataset for outs only
    outs_data <- trackman_data[trackman_data$PlayResult == "Out", ]
    
    # Calculate the total number of observations for hits and outs
    total_hits <- nrow(hits_data)
    total_outs <- nrow(outs_data)
    dot_size <- 4
    
    # Plot a pitch location heat map for hits
    pitch_heat_map_hits <- ggplot(hits_data, aes(x = PlateLocSide, y = PlateLocHeight)) +
      geom_bin2d(binwidth = c(0.5, 0.5), aes(fill = after_stat(count)/(total_outs+total_hits)*100)) +
      geom_rect(aes(xmin = -0.7083, xmax = 0.7083, ymin = 1.5, ymax = 3.5), fill = NA, color = "black") +
      geom_rect(aes(xmin = -0.83, xmax = 0.83, ymin = 1.3775, ymax = 3.6225), fill = NA, color = "black", linetype = "dashed") +
      scale_fill_gradient(low = "white", high = "green", name = "Hit %") +
      labs(title = "Hit % for All Balls In Play",
           x = "X Location",
           y = "Y Location",
           fill = "Hit %") +
      theme(plot.title = element_text(hjust = 0.3)) +
      xlim(-2.75, 2.75) +
      ylim(-0.25, 4.75)
    
    # Plot a pitch location heat map for outs
    pitch_heat_map_outs <- ggplot(outs_data, aes(x = PlateLocSide, y = PlateLocHeight)) +
      geom_bin2d(binwidth = c(0.5, 0.5), aes(fill = after_stat(count)/(total_outs+total_hits)*100)) +
      geom_rect(aes(xmin = -0.7083, xmax = 0.7083, ymin = 1.5, ymax = 3.5), fill = NA, color = "black") +
      geom_rect(aes(xmin = -0.83, xmax = 0.83, ymin = 1.3775, ymax = 3.6225), fill = NA, color = "black", linetype = "dashed") +
      scale_fill_gradient(low = "white", high = "red", name = "Out %") +
      labs(title = "Out % for All Balls In Play",
           x = "X Location",
           y = "Y Location",
           fill = "Out %") +
      theme(plot.title = element_text(hjust = 0.3)) +
      xlim(-2.75, 2.75) +
      ylim(-0.25, 4.75)
    
    # Plot a graph of the pitch location hits
    strike_zone_plot_hits <- ggplot(hits_data, aes(x = PlateLocSide, y = PlateLocHeight, color = TaggedPitchType)) +
      geom_point(size = dot_size) +  # Modify size of dots
      scale_color_manual(values = pitch_colors) +
      geom_rect(aes(xmin = -0.7083, xmax = 0.7083, ymin = 1.5, ymax = 3.5), fill = NA, color = "black") +
      labs(
        title = "Hit Location Plot",
        x = "X Location",
        y = "Y Location",
        color = "Pitch Type"
      ) +
      theme(plot.title = element_text(hjust = 0.5)) + xlim(-2.75, 2.75) + ylim(-0.25, 4.75)
    
    # Plot a graph of the pitch location outs
    strike_zone_plot_outs <- ggplot(outs_data, aes(x = PlateLocSide, y = PlateLocHeight, color = TaggedPitchType)) +
      geom_point(size = dot_size) +  # Modify size of dots
      scale_color_manual(values = pitch_colors) +
      geom_rect(aes(xmin = -0.7083, xmax = 0.7083, ymin = 1.5, ymax = 3.5), fill = NA, color = "black") +
      labs(
        title = "Out Location Plot",
        x = "X Location",
        y = "Y Location",
        color = "Pitch Type"
      ) +
      theme(plot.title = element_text(hjust = 0.5)) + xlim(-2.75, 2.75) + ylim(-0.25, 4.75)
    
    # Combine the heatmaps, location plots, and exit velocity plot
    combined_plot <- grid.arrange(arrangeGrob(pitch_heat_map_hits, pitch_heat_map_outs, ncol = 2),
                                  arrangeGrob(strike_zone_plot_hits, strike_zone_plot_outs, ncol = 2),
                                  exit_velo_plot,
                                  ncol = 1, nrow = 3,
                                  top = textGrob("Hitting Report from Trackman Data", gp = gpar(fontsize = 20)))
    return (combined_plot)
  })
  
  # Display the hitting information in the text output area
  output$hitter_info <- renderText({
    hitter_info()
  })
  
  # Display the hitting info in the plot output area
  output$hitter_plot <- renderPlot({
    hitter_plot()
  })
  
  # Define a reactive expression that returns the hockey information based on the user's selection
  hockey_info <- reactive({
    switch(
      input$HockeyPlayer,
      if (input$HockeyPlayer != "All") {
        paste("Hockey Report for", input$HockeyPlayer)  # Generate Hockey report for a specific player
      } else {
        paste("Hockey Report for All Players")  # Generate Hockey report for all players
      }
    )
  })
  
  # Define a reactive expression that returns the hitting information based on the user's selection
  hockey_plot <- reactive({
    
    data <- read.csv("HockeyData.csv")
    
    #if (input$HockeyGame != "All")
    #{
      #data <- data[data$Date == input$HockeyGame, ]
    #}
    
    if (input$HockeyPlayer != "All")
    {
      data <- data[data$Name == input$HockeyPlayer, ]
    }
    if (input$HockeyPeriod != "All")
    {
      data <- data[data$Period == input$HockeyPeriod, ]
    }
    if (nrow(data) == 0)
    {
      return ()
    }
    
    # Remove rows with NA values in the 'Quadrant' column
    data <- data[!is.na(data$Quadrant), ]
    
    # Setting up colour values
    hockey_red <- "#FF0000" 
      hockey_blue <- "#0000FF" 
        hockey_light_blue <- "#CCF5FF"
          
        # Function to calculate the color based on the number (ranging from 0 to 1)
        get_dynamic_color <- function(number) {
          # Convert the number to a transparency level (alpha value) between 00 and FF in hexadecimal
          alpha_value <- sprintf("%02X", round(number * 255))
          # Set the color as a darker green and apply the transparency
          color <- paste0("#006400", alpha_value)  # Dark green ("#006400") with the calculated transparency
          return(color)
        }
        
        nhl_full_rink_plot <- function () {
          quad_1 <- sum(data$Quadrant == 1) / nrow(data)  # Top quadrant
          quad_2 <- sum(data$Quadrant == 2) / nrow(data)  # Top-Middle quadrant
          quad_3 <- sum(data$Quadrant == 3) / nrow(data)  # Bottom-Middle quadrant
          quad_4 <- sum(data$Quadrant == 4) / nrow(data)  # Bottom quadrant
          ggplot() +
            
            # Adding code to highlight quadrants
            geom_rect(aes(xmin = -100, xmax = 100, ymin = 21.25, ymax = 42.5), fill = get_dynamic_color(quad_1)) +      # Bottom-Right quadrant
            geom_rect(aes(xmin = -100, xmax = 100, ymin = 0, ymax = 21.25), fill = get_dynamic_color(quad_2)) +     # Bottom-Left quadrant
            geom_rect(aes(xmin = -100, xmax = 100, ymin = -21.25, ymax = 0), fill = get_dynamic_color(quad_3)) +   # Top-Right quadrant
            geom_rect(aes(xmin = -100, xmax = 100, ymin = -42.5, ymax = -21.25), fill = get_dynamic_color(quad_4)) +  # Top-Left quadrant
            
            # Adding the color scale on the right side of the graph
            #scale_fill_gradient(name = "Quadrant Proportion", low = "#00640000", high = "#006400FF", 
            #                    breaks = seq(0, 1, by = 0.2), labels = scales::percent_format(scale = 1)) +
            
            # Hash marks in T-R/B-R/T-L/B-R order, groups of four
            geom_tile(aes(x = 66.125, y = 37.77, width = 2 / 12, height = 2), fill = hockey_red) +
            geom_tile(aes(x = 66.125, y = 6.23, width = 2 / 12, height = 2), fill = hockey_red) +
            geom_tile(aes(x = 71.875, y = 37.77, width = 2 / 12, height = 2), fill = hockey_red) +
            geom_tile(aes(x = 71.875, y = 6.23, width = 2 / 12, height = 2), fill = hockey_red) +
            geom_tile(aes(x = 66.125, y = -37.77, width = 2 / 12, height = 2), fill = hockey_red) +
            geom_tile(aes(x = 66.125, y = -6.23, width = 2 / 12, height = 2), fill = hockey_red) +
            geom_tile(aes(x = 71.875, y = -37.77, width = 2 / 12, height = 2), fill = hockey_red) +
            geom_tile(aes(x = 71.875, y = -6.23, width = 2 / 12, height = 2), fill = hockey_red) +
            geom_tile(aes(x = -66.125, y = 37.77, width = 2 / 12, height = 2), fill = hockey_red) +
            geom_tile(aes(x = -66.125, y = 6.23, width = 2 / 12, height = 2), fill = hockey_red) +
            geom_tile(aes(x = -71.875, y = 37.77, width = 2 / 12, height = 2), fill = hockey_red) +
            geom_tile(aes(x = -71.875, y = 6.23, width = 2 / 12, height = 2), fill = hockey_red) +
            geom_tile(aes(x = -66.125, y = -37.77, width = 2 / 12, height = 2), fill = hockey_red) +
            geom_tile(aes(x = -66.125, y = -6.23, width = 2 / 12, height = 2), fill = hockey_red) +
            geom_tile(aes(x = -71.875, y = -37.77, width = 2 / 12, height = 2), fill = hockey_red) +
            geom_tile(aes(x = -71.875, y = -6.23, width = 2 / 12, height = 2), fill = hockey_red) +
            
            # Centre line
            geom_tile(aes(x = 0, y = 0, width = 1, height = 85), fill = hockey_red) + # Centre line
            
            # Faceoff dots - Plot AFTER centre lines for centre ice circle to show up above
            geom_circle(aes(x0 = 0, y0 = 0, r = 6 / 12), colour = "#FF99B4", fill = "#FF99B4", size = 0) + # Centre dot with unique red
            geom_circle(aes(x0 = 69, y0 = 22, r = 1), colour = hockey_red, fill = hockey_red, size = 0) + # Top-Right
            geom_circle(aes(x0 = 69, y0 = -22, r = 1), colour = hockey_red, fill = hockey_red, size = 0) + # Bottom-Right
            geom_circle(aes(x0 = -69, y0 = 22, r = 1), colour = hockey_red, fill = hockey_red, size = 0) + # Top-Left
            geom_circle(aes(x0 = -69, y0 = -22, r = 1), colour = hockey_red, fill = hockey_red, size = 0) + # Bottom-Left
            
            geom_circle(aes(x0 = 20.5, y0 = 22, r = 1), colour = hockey_red, fill = hockey_red, size = 0) + # Neutral Top-Right
            geom_circle(aes(x0 = 20.5, y0 = -22, r = 1), colour = hockey_red, fill = hockey_red, size = 0) + # Neutral Bottom-Right
            geom_circle(aes(x0 = -20.5, y0 = 22, r = 1), colour = hockey_red, fill = hockey_red, size = 0) + # Neutral Top-Left
            geom_circle(aes(x0 = -20.5, y0 = -22, r = 1), colour = hockey_red, fill = hockey_red, size = 0) + # Neutral Bottom-Left
            
            # Ells surrounding faceoff dots
            geom_tile(aes(x = 65, y = 22.83, width = 4, height = 2 / 12), fill = hockey_red) + # Top-Right
            geom_tile(aes(x = 73, y = 22.83, width = 4, height = 2 / 12), fill = hockey_red) +
            geom_tile(aes(x = 65, y = 21.17, width = 4, height = 2 / 12), fill = hockey_red) +
            geom_tile(aes(x = 73, y = 21.17, width = 4, height = 2 / 12), fill = hockey_red) +
            geom_tile(aes(x = 66.92, y = 24.25, width = 2 / 12, height = 3), fill = hockey_red) +
            geom_tile(aes(x = 71.08, y = 24.25, width = 2 / 12, height = 3), fill = hockey_red) +
            geom_tile(aes(x = 66.92, y = 19.75, width = 2 / 12, height = 3), fill = hockey_red) +
            geom_tile(aes(x = 71.08, y = 19.75, width = 2 / 12, height = 3), fill = hockey_red) +
            
            geom_tile(aes(x = 65, y = -22.83, width = 4, height = 2 / 12), fill = hockey_red) + # Bottom-Right
            geom_tile(aes(x = 73, y = -22.83, width = 4, height = 2 / 12), fill = hockey_red) +
            geom_tile(aes(x = 65, y = -21.17, width = 4, height = 2 / 12), fill = hockey_red) +
            geom_tile(aes(x = 73, y = -21.17, width = 4, height = 2 / 12), fill = hockey_red) +
            geom_tile(aes(x = 66.92, y = -24.25, width = 2 / 12, height = 3), fill = hockey_red) +
            geom_tile(aes(x = 71.08, y = -24.25, width = 2 / 12, height = 3), fill = hockey_red) +
            geom_tile(aes(x = 66.92, y = -19.75, width = 2 / 12, height = 3), fill = hockey_red) +
            geom_tile(aes(x = 71.08, y = -19.75, width = 2 / 12, height = 3), fill = hockey_red) +
            
            geom_tile(aes(x = -65, y = 22.83, width = 4, height = 2 / 12), fill = hockey_red) + # Top-Left
            geom_tile(aes(x = -73, y = 22.83, width = 4, height = 2 / 12), fill = hockey_red) +
            geom_tile(aes(x = -65, y = 21.17, width = 4, height = 2 / 12), fill = hockey_red) +
            geom_tile(aes(x = -73, y = 21.17, width = 4, height = 2 / 12), fill = hockey_red) +
            geom_tile(aes(x = -66.92, y = 24.25, width = 2 / 12, height = 3), fill = hockey_red) +
            geom_tile(aes(x = -71.08, y = 24.25, width = 2 / 12, height = 3), fill = hockey_red) +
            geom_tile(aes(x = -66.92, y = 19.75, width = 2 / 12, height = 3), fill = hockey_red) +
            geom_tile(aes(x = -71.08, y = 19.75, width = 2 / 12, height = 3), fill = hockey_red) +
            
            geom_tile(aes(x = -65, y = -22.83, width = 4, height = 2 / 12), fill = hockey_red) + # Bottom-Left
            geom_tile(aes(x = -73, y = -22.83, width = 4, height = 2 / 12), fill = hockey_red) +
            geom_tile(aes(x = -65, y = -21.17, width = 4, height = 2 / 12), fill = hockey_red) +
            geom_tile(aes(x = -73, y = -21.17, width = 4, height = 2 / 12), fill = hockey_red) +
            geom_tile(aes(x = -66.92, y = -24.25, width = 2 / 12, height = 3), fill = hockey_red) +
            geom_tile(aes(x = -71.08, y = -24.25, width = 2 / 12, height = 3), fill = hockey_red) +
            geom_tile(aes(x = -66.92, y = -19.75, width = 2 / 12, height = 3), fill = hockey_red) +
            geom_tile(aes(x = -71.08, y = -19.75, width = 2 / 12, height = 3), fill = hockey_red) +
            
            # Referee crease
            geom_arc(aes(x0 = 0, y0 = -42.5, start = -pi / 2, end = pi / 2, r = 10), colour = hockey_red) +
            
            # Left goalie crease
            geom_tile(aes(x = -86.75, y = 0, width = 4.5, height = 8), fill = hockey_light_blue) +
            geom_arc_bar(aes(x0 = -89, y0 = 0, start = atan(4.5/4) - 0.01, end = pi - atan(4.5 / 4) + 0.01, r0 = 4, r = 6), fill = hockey_light_blue, colour = hockey_light_blue, size = 1 / 12) + # manually adjusted arc
            geom_tile(aes(x = -86.75, y = -4.1, width = 4.5, height = 2 / 12), fill = hockey_red) +
            geom_tile(aes(x = -86.75, y = 4.1, width = 4.5, height = 2 / 12), fill = hockey_red) +
            geom_arc(aes(x0 = -89, y0 = 0, start = atan(4.5/4) - 0.01, end = pi - atan(4.5 / 4) + 0.01, r = 6), colour = hockey_red, size = 2 / 12) + # manually adjusted arc
            geom_tile(aes(x = -85, y = 3.75, width = 2 / 12, height = 0.42), fill = hockey_red) +
            geom_tile(aes(x = -85, y = -3.75, width = 2 / 12, height = 0.42), fill = hockey_red) +
            
            # Right goalie crease
            geom_tile(aes(x = 86.75, y = 0, width = 4.5, height = 8), fill = hockey_light_blue) +
            geom_arc_bar(aes(x0 = 89, y0 = 0, start = -atan(4.5/4) + 0.01, end = -pi + atan(4.5 / 4) - 0.01, r0 = 4, r = 6), fill = hockey_light_blue, colour = hockey_light_blue, size = 1 / 12) + # manually adjusted arc
            geom_tile(aes(x = 86.75, y = -4.1, width = 4.5, height = 2 / 12), fill = hockey_red) +
            geom_tile(aes(x = 86.75, y = 4.1, width = 4.5, height = 2 / 12), fill = hockey_red) +
            geom_arc(aes(x0 = 89, y0 = 0, start = -atan(4.5/4) + 0.01, end = -pi + atan(4.5 / 4) - 0.01, r = 6), colour = hockey_red, size = 2 / 12) + # manually adjusted arc
            geom_tile(aes(x = 85, y = 3.75, width = 2 / 12, height = 0.42), fill = hockey_red) +
            geom_tile(aes(x = 85, y = -3.75, width = 2 / 12, height = 0.42), fill = hockey_red) +
            
            # Goalie nets placed as rectangles
            geom_tile(aes(x = -90.67, y = 0, width = 3.33, height = 6), fill = "#E5E5E3") + # Left # with grey fills
            geom_tile(aes(x = 90.67, y = 0, width = 3.33, height = 6), fill = "#E5E5E3") + # Right
            
            # Trapezoids
            geom_polygon(aes(x = c(-100, -100, -89, -89), y = c(10.92, 11.08, 7.08, 6.92)), fill = hockey_red) + # Left
            geom_polygon(aes(x = c(-100, -100, -89, -89), y = c(-10.92, -11.08, -7.08, -6.92)), fill = hockey_red) + # Left 
            geom_polygon(aes(x = c(100, 100, 89, 89), y = c(10.92, 11.08, 7.08, 6.92)), fill = hockey_red) + # Right
            geom_polygon(aes(x = c(100, 100, 89, 89), y = c(-10.92, -11.08, -7.08, -6.92)), fill = hockey_red) + # Right
            
            # Lines
            geom_tile(aes(x = -25.5, y = 0, width = 1, height = 85), fill = hockey_blue) + # Left Blue line
            geom_tile(aes(x = 25.5, y = 0, width = 1, height = 85),  fill = hockey_blue) + # Right Blue line
            geom_tile(aes(x = -89, y = 0, width = 2 / 12, height = 73.50), fill = hockey_red) + # Left goal line (73.5 value is rounded from finding intersect of goal line and board radius)
            geom_tile(aes(x = 89, y = 0, width = 2 / 12, height = 73.50), fill = hockey_red) + # Right goal line
            
            # Borders as line segments - plotted last to cover up line ends, etc.
            geom_line(aes(x = c(-72, 72), y = c(42.5, 42.5))) + # Top
            geom_line(aes(x = c(-72, 72), y = c(-42.5, -42.5))) + # Bottom
            geom_line(aes(x = c(-100, -100), y = c(-14.5, 14.5))) + # Left
            geom_line(aes(x = c(100, 100), y = c(-14.5, 14.5))) + # Right
            geom_arc(aes(x0 = 72, y0 = 14.5, start = pi / 2, end = 0, r = 28)) + # Top-Right
            geom_arc(aes(x0 = 72, y0 = -14.5, start = pi, end =  pi / 2, r = 28)) + # Bottom-Right
            geom_arc(aes(x0 = -72, y0 = 14.5, start = - pi / 2, end = 0, r = 28)) + # Top-Left
            geom_arc(aes(x0 = -72, y0 = -14.5, start = pi, end =  3 * pi / 2, r = 28)) + # Bottom-Left
            
            # Zone description
            annotate("text", -1.5, 31.875, label = paste("ZONE 1  ", round(quad_1*100,1), "%"), size = 6, fontface = "bold") +
            annotate("text", -1.5, 10.625, label = paste("ZONE 2  ", round(quad_2*100,1), "%"), size = 6, fontface = "bold") +
            annotate("text", -1.5, -10.625, label = paste("ZONE 3  ", round(quad_3*100,1), "%"), size = 6, fontface = "bold") +
            annotate("text", -1.5, -31.875, label = paste("ZONE 4  ", round(quad_4*100,1), "%"), size = 6, fontface = "bold") +
            
            labs(title = "Zone Entry Position") +
            theme_minimal() +
            theme(
              plot.title = element_text(size = 18, hjust = 0.5)  # Set title size to 18 and center it horizontally (hjust = 0.5)
            ) +
            
            # Fixed scale for the coordinate system  
            coord_fixed()
        }
        
        # Calculate the count of each entry type
        entry_counts <- table(data$EntryType)
        
        # Calculate the percentage of each entry type relative to the total entries
        total_entries <- length(data$EntryType)
        entry_percentages <- (entry_counts / total_entries) * 100
        
        # Convert entry_percentages data to a data frame for plotting
        entry_type_data <- data.frame(EntryType = names(entry_percentages), EntryPercentage = entry_percentages)
        
        #### ENTRIES LEADING TO SHOTS #####
        # Filter the data and only keep rows where FinalPlay is SGF, SGR, or G
        shots_data <- data[data$FinalPlay %in% c("SGF", "SGR", "G"), ]
        
        # Calculate the count of each entry type in the filtered data
        shot_counts <- table(shots_data$EntryType)
        
        # Calculate the percentage of each entry type relative to the total entries
        shots_percentages <- (shot_counts / total_entries) * 100
        
        # Convert the table to a data frame
        shot_percentages_df <- as.data.frame(shots_percentages)
        
        # Check if 'chip' is present in the data frame
        if (!'chip' %in% shot_percentages_df$Var1) {
          # If not present, add it with a frequency of 0
          shot_percentages_df <- rbind(shot_percentages_df, data.frame(Var1 = "chip", Freq = 0))
        }
        # Check if 'dump' is present in the data frame
        if (!'dump' %in% shot_percentages_df$Var1) {
          # If not present, add it with a frequency of 0
          shot_percentages_df <- rbind(shot_percentages_df, data.frame(Var1 = "dump", Freq = 0))
        }
        # Check if 'carry' is present in the data frame
        if (!'carry' %in% shot_percentages_df$Var1) {
          # If not present, add it with a frequency of 0
          shot_percentages_df <- rbind(shot_percentages_df, data.frame(Var1 = "carry", Freq = 0))
        }
        # Check if 'pass' is present in the data frame
        if (!'pass' %in% shot_percentages_df$Var1) {
          # If not present, add it with a frequency of 0
          shot_percentages_df <- rbind(shot_percentages_df, data.frame(Var1 = "pass", Freq = 0))
        }
        
        # Convert entry_percentages data to a data frame for plotting
        shot_type_data <- data.frame(EntryType = shot_percentages_df$Var1, ShotPercentage = shot_percentages_df$Freq)
        
        ##### ENTRIES LEADING TO GOALS #####
        # Filter the data and only keep rows where FinalPlay is SGF, SGR, or G
        goal_data <- data[data$FinalPlay %in% c("G"), ]
        
        # Calculate the count of each entry type in the filtered data
        goal_counts <- table(goal_data$EntryType)
        
        # Calculate the percentage of each entry type relative to the total entries
        goal_percentages <- (goal_counts / total_entries) * 100
        
        # Convert the table to a data frame
        goal_percentages_df <- as.data.frame(goal_percentages)
        
        # Check if 'chip' is present in the data frame
        if (!'chip' %in% goal_percentages_df$Var1) {
          # If not present, add it with a frequency of 0
          goal_percentages_df <- rbind(goal_percentages_df, data.frame(Var1 = "chip", Freq = 0))
        }
        # Check if 'dump' is present in the data frame
        if (!'dump' %in% goal_percentages_df$Var1) {
          # If not present, add it with a frequency of 0
          goal_percentages_df <- rbind(goal_percentages_df, data.frame(Var1 = "dump", Freq = 0))
        }
        # Check if 'carry' is present in the data frame
        if (!'carry' %in% goal_percentages_df$Var1) {
          # If not present, add it with a frequency of 0
          goal_percentages_df <- rbind(goal_percentages_df, data.frame(Var1 = "carry", Freq = 0))
        }
        # Check if 'pass' is present in the data frame
        if (!'pass' %in% goal_percentages_df$Var1) {
          # If not present, add it with a frequency of 0
          goal_percentages_df <- rbind(goal_percentages_df, data.frame(Var1 = "pass", Freq = 0))
        }
        
        # Convert entry_percentages data to a data frame for plotting
        goal_type_data <- data.frame(EntryType = goal_percentages_df$Var1, ZGoalPercentage = goal_percentages_df$Freq)
        
        
        # Merge the three data frames based on 'EntryType'
        total_data <- merge(entry_type_data, shot_type_data, by = "EntryType")
        total_data <- merge(total_data, goal_type_data, by = "EntryType")
        
        total_data_long <- tidyr::pivot_longer(total_data, cols = c("EntryPercentage.Freq", "ShotPercentage", "ZGoalPercentage"),
                                               names_to = "Category", values_to = "Percentage")
        
        # Custom colors
        all_entries_color <- "#1f77b4"   # Replace with the desired color code for "All entries"
          shots_on_goal_color <- "#ff7f0e" # Replace with the desired color code for "Shots on Goal"
            goal_color <- "#2ca02c"   # Replace with the desired color code for "Goals"
              
            # Plot the bar graph
            entry_type_bar_plot <- ggplot(total_data_long, aes(x = EntryType, y = Percentage, fill = Category)) +
              geom_bar(stat = "identity", position = "dodge", color = "black") +
              labs(title = "Entry Outcomes",
                   x = "Entry Type",
                   y = "Percentage",
                   fill = "Category") +
              scale_fill_manual(values = c("EntryPercentage.Freq" = all_entries_color, "ShotPercentage" = shots_on_goal_color, "ZGoalPercentage" = goal_color),
                                name = "Category",
                                labels = c("All entries", "Shots on Goal", "Goal")) +
              theme_minimal() + 
              theme(      
                plot.title = element_text(size = 18, hjust = 0.5)  # Set title size to 18 and center it horizontally (hjust = 0.5)
              )
            
            hockey_rink_plot <- nhl_full_rink_plot()
            
            #####  Entry Type By Period   #####
            # Calculate the count of each entry type for each period
            entry_counts_per_period <- table(data$EntryType, data$Period)
            
            # Calculate the percentage of each entry type for each period relative to the total entries in that period
            entry_percentages_per_period <- prop.table(entry_counts_per_period, margin = 2) * 100
            
            # Convert entry_percentages_per_period data to a data frame for plotting
            entry_type_period_data <- as.data.frame(entry_percentages_per_period)
            
            # Rename the columns for better visualization
            colnames(entry_type_period_data) <- c("EntryType", "Period", "Percentage")
            
            entry_type_period_data$EntryType <- gsub("chip", "zchip", entry_type_period_data$EntryType)
            
            # Plot the bar graph for entry outcomes with periods on the x-axis
            entry_type_period_bar_plot <- ggplot(entry_type_period_data, aes(x = Period, y = Percentage, fill = EntryType)) +
              geom_bar(stat = "identity", position = "dodge", color = "black") +
              labs(title = "Entry Type by Period",
                   x = "Period",
                   y = "Percentage",
                   fill = "Entry Type") +
              scale_fill_manual(values = c("zchip" = "#FF5733", "dump" = "#33FFC1", "carry" = "#FFD933", "pass" = "#3366FF"),
                                labels = c("zchip" = "chip"),
                                name = "Entry Type") +
              theme_minimal() +
              theme(
                plot.title = element_text(size = 18, hjust = 0.5)  # Set title size to 18 and center it horizontally (hjust = 0.5)
              )
            
            #####  Entries By Period  #####
            # Calculate the count of each entry type for each outcome (G, SGF, SGR)
            entry_counts_per_outcome <- table(data$Period, data$FinalPlay)
            
            # If the data doesn't contain some outcome in any period, add a column of zeros
            if (!("G" %in% colnames(entry_counts_per_outcome))) {
              entry_counts_per_outcome <- cbind(entry_counts_per_outcome, G = 0)
            }
            if (!("SGF" %in% colnames(entry_counts_per_outcome))) {
              entry_counts_per_outcome <- cbind(entry_counts_per_outcome, SGF = 0)
            }
            if (!("SGR" %in% colnames(entry_counts_per_outcome))) {
              entry_counts_per_outcome <- cbind(entry_counts_per_outcome, SGR = 0)
            }
            
            # Sum of all entry counts for each period
            all_entries_totals <- rowSums(entry_counts_per_outcome)
            
            # Calculate the total sum of all entries
            total_sum <- sum(all_entries_totals)
            
            # Calculate the percentage of each entry type for each period
            all_entries_totals <- all_entries_totals / total_sum * 100
            
            # Filter the data to include only "G", "SGF", and "SGR" outcomes
            
            new_entry_counts_per_outcome <- entry_counts_per_outcome[, c("G", "SGF", "SGR")]
            
            if (!is.null(nrow(new_entry_counts_per_outcome))){
              shots_on_goal_totals <- rowSums(new_entry_counts_per_outcome)
            } else{
              shots_on_goal_totals <- sum(new_entry_counts_per_outcome)
            }
            shots_on_goal_totals <- shots_on_goal_totals / total_sum *100
            
            # Filter the data to include only "G" outcomes
            
            if (!is.null(nrow(new_entry_counts_per_outcome))){
              goal_totals <- new_entry_counts_per_outcome[, c("G")]
            } else{
              goal_totals <- new_entry_counts_per_outcome["G"]
            }
            goal_totals <- goal_totals / total_sum *100
            
            if (length(unique(data$Period)) == 1)
            {
              per = unique(data$Period)
            } else {
              per = c(unique(data$Period))
            }
            # Create a data frame with the desired format
            combined_totals <- data.frame(Period = per, ALL = all_entries_totals, SOG = shots_on_goal_totals, ZG = goal_totals)
            reshaped_combined_totals <- gather(combined_totals, EntryType, EntryCount, -Period)
            
            # Plot the bar graph for entry outcomes with outcomes on the x-axis
            entry_type_outcome_bar_plot <- ggplot(reshaped_combined_totals, aes(x = Period, y = EntryCount, fill = EntryType)) +
              geom_bar(stat = "identity", position = "dodge", color = "black") +          
              labs(title = "Entries by Period",
                   x = "Period",
                   y = "Count",
                   fill = "Entry Type") +
              scale_fill_manual(values = c("ALL" = "#FF5733", "SOG" = "#3366FF", "ZG" = "#33CC33"),
                                name = "Entry Type",
                                labels = c("All Entries", "Shots on Goal", "Goals")) +
              theme_minimal() +
              theme(
                plot.title = element_text(size = 18, hjust = 0.5)  # Set title size to 18 and center it horizontally (hjust = 0.5)
              ) +
              scale_x_continuous(breaks = per)
            
            
            # Combine the two bar charts in a grid layout
            row2_plot <- grid.arrange(entry_type_bar_plot, entry_type_outcome_bar_plot, ncol = 2)
            
            # Then, arrange the first plot (hockey_rink_plot) alone in the first row along with the second row of plots
            end_plot <- grid.arrange(hockey_rink_plot, row2_plot, entry_type_period_bar_plot, ncol = 1, heights = c(1.35, 1, 1), 
                                     top = textGrob(paste("MSU Spartans Hockey Report"), gp = gpar(fontsize = 22, fontface = "bold")))
            return (end_plot)
  })
  
  # Display the hitting information in the text output area
  output$hockey_info <- renderText({
    hockey_info()
  })
  
  # Display the hitting info in the plot output area
  output$hockey_plot <- renderPlot({
    hockey_plot()
  })















# Define a reactive expression that returns the ffb information based on the user's selection
football_plot <- reactive({
  
  ### DATA CLEANING ####
  data <- read.csv("MasterFantasy.csv")
  data <- data %>%
    filter(TTL >= 2) %>%
    arrange(Player, Year) %>%
    group_by(Player)
  
  ### GRAPHING AND PLOTTING ###
  player_data <- data %>%
    mutate(Player_Year = paste(Player, Year, sep = " - "))
  
  # Aggregate by player, position, and year across all seasons
  player_aggregated <- player_data %>%
    group_by(Player_Year, Pos) %>%
    summarise(Total_Points = sum(TTL))
  
  # Then find the top seasons by position
  top_players <- player_aggregated %>%
    arrange(Pos, desc(Total_Points)) %>%
    group_by(Pos) %>%
    slice_head(n = 5)  # Take top 5 seasons for each position
  
  # Plot
  player_plot_year <- ggplot(top_players, aes(x = reorder(Player_Year, Total_Points), y = Total_Points)) +
    geom_segment(aes(xend = Player_Year, yend = 0), color = "skyblue") +
    geom_point(size = 3, color = "red") +
    coord_flip() +
    labs(title = "Top 5 Seasons by Position Across All Years", x = "Player and Year", y = "Total Points") +
    facet_wrap(~ Pos, scales = "free")
  
  # Aggregate by player and position across all years to sum their total points
  player_total_aggregated <- data %>%
    group_by(Player, Pos) %>%
    summarise(Total_Points = sum(TTL))
  
  # Then find the top players by position based on their total points
  top_total_players <- player_total_aggregated %>%
    arrange(Pos, desc(Total_Points)) %>%
    group_by(Pos) %>%
    slice_head(n = 5)  # Take top 5 players for each position based on their total points
  
  # Plot
  top_players_total <- ggplot(top_total_players, aes(x = reorder(Player, Total_Points), y = Total_Points)) +
    geom_segment(aes(xend = Player, yend = 0), color = "skyblue") +
    geom_point(size = 3, color = "red") +
    coord_flip() +
    labs(title = "Top 5 Players by Position for Total Fantasy Points", x = "Player", y = "Total Points") +
    facet_wrap(~ Pos, scales = "free")
  
  # Boxplot showing distribution of points by position for all years
  box_plot_all_years <- ggplot(data, aes(x=Pos, y=TTL, fill=Pos)) +
    geom_boxplot() +
    labs(title="Distribution of Points by Position across All Years",
         x="Position",
         y="Points") +
    theme_minimal() +
    scale_fill_brewer(palette="Set2")
  
  # Bar chart showing total points by year
  total_year <- data %>%
    group_by(Year) %>%
    summarise(TotalPoints = sum(TTL))
  
  bar_plot <- ggplot(total_year, aes(x=as.factor(Year), y=TotalPoints)) +
    geom_bar(stat="identity", fill="steelblue", alpha = 0.7) +
    labs(title="Total Points by Year",
         x="Year",
         y="Total Points") +
    theme_minimal()
  
  ### MORE DATA CLEANING ###

  data <- data %>% mutate(PrevYear1_TTL = lag(TTL, 1), PrevYear2_TTL = lag(TTL, 2)) %>%
    filter(!is.na(PrevYear1_TTL) & !is.na(PrevYear2_TTL))
  
  ### LINEAR REGRESSION MODEL ###
  
  set.seed(123)  # This ensures reproducibility of the random split
  split <- sample(1:nrow(data), nrow(data)*0.8)
  train <- data[split,]
  test <- data[-split,]
  
  # Ensure 'Pos' factor levels in train and test are the same
  train$Pos <- factor(train$Pos, levels = unique(data$Pos))  # Re-level 'Pos' factor in training data
  test$Pos <- factor(test$Pos, levels = unique(data$Pos))    # Re-level 'Pos' factor in test data
  
  # Fit a linear regression model to predict the total fantasy points based on the past two years and the position
  model <- lm(TTL ~ PrevYear1_TTL + PrevYear2_TTL + Pos, data=train)
  
  # Use the model to predict on the test set
  predictions <- predict(model, newdata=test)
  
  ### GRAPH THE DATA ###
  
  # Check the unique values in the Pos column
  unique_positions <- unique(data$Pos)
  color_values <- rainbow(length(unique_positions))  # Generate enough distinct colors
  
  # Calculate the average of the previous two seasons' fantasy points for each player
  data <- data %>%
    mutate(Average_Prev2Years = (PrevYear1_TTL + PrevYear2_TTL) / 2)
  
  
  # Create and visualize the scatterplot
  linear_model <- ggplot(data, aes(x = Average_Prev2Years, y = TTL)) +
    geom_point(aes(color = Pos), alpha = 0.6, size = 2.5) +  # scatterplot points colored by position
    geom_point(aes(color = Pos, size = YearsPro), alpha = 0.6) +
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "darkred") +  # linear regression line with specified formula
    labs(title = "Predicted Fantasy Points vs Past Two Seasons Average",
         x = "Average Fantasy Points of Past Two Seasons",
         y = "Predicted Fantasy Points") +
    theme_minimal() +  # minimalistic theme for cleaner visuals
    scale_color_manual(values = color_values)  # custom colors for different player positions
  
  theme_clean <- theme(
    plot.title = element_text(hjust = 0.5, face="bold", size=14), 
    strip.text = element_text(face="bold"),   # For facet titles
    axis.title.x = element_text(face="bold", size=12, color="black"),
    axis.title.y = element_text(face="bold", size=12, color="black"),
    axis.text.x = element_text(size=10, color="darkgray"),
    axis.text.y = element_text(size=10, color="darkgray")
  )
  
  
  player_plot_year <- player_plot_year + theme_clean
  top_players_total <- top_players_total + theme_clean
  box_plot_all_years <- box_plot_all_years + theme_clean
  bar_plot <- bar_plot + theme_clean
  linear_model <- linear_model + theme_clean
  
  end_plot <- grid.arrange(
    # First row with player_plot_year and top_players_total side by side
    player_plot_year, top_players_total, 
    # Second row with box_plot_all_years and bar_plot side by side
    box_plot_all_years, bar_plot, 
    # Third row with linear_model taking up both columns
    linear_model, 
    # Define the layout: 2 plots in the first 2 rows, and 1 plot in the third row 
    layout_matrix = rbind(
      c(1, 2),
      c(3, 4),
      c(5, 5)
    ),
    top = textGrob("Fantasy Football Data (Since 2013)", gp = gpar(fontsize = 20, fontface = "bold"))
  )
  
  return(end_plot)
})


# Define a reactive expression that returns the ffb information based on the user's selection
football_info <- reactive({
  data <- read.csv("MasterFantasy.csv")
  data <- data %>%
    filter(TTL >= 2) %>%
    arrange(Player, Year) %>%
    group_by(Player)
  
  player_data <- data %>%
    mutate(Player_Year = paste(Player, Year, sep = " - "))
  
  data <- data %>% mutate(PrevYear1_TTL = lag(TTL, 1), PrevYear2_TTL = lag(TTL, 2)) %>%
    filter(!is.na(PrevYear1_TTL) & !is.na(PrevYear2_TTL))
  
  set.seed(123)  # This ensures reproducibility of the random split
  split <- sample(1:nrow(data), nrow(data)*0.8)
  train <- data[split,]
  test <- data[-split,]
  
  # Ensure 'Pos' factor levels in train and test are the same
  train$Pos <- factor(train$Pos, levels = unique(data$Pos))  # Re-level 'Pos' factor in training data
  test$Pos <- factor(test$Pos, levels = unique(data$Pos))    # Re-level 'Pos' factor in test data
  
  # Fit a linear regression model to predict the total fantasy points based on the past two years and the position
  model <- lm(TTL ~ PrevYear1_TTL + PrevYear2_TTL + Pos + YearsPro, data=train)
  
  # Use the model to predict on the test set
  predictions <- predict(model, newdata=test)
  
  ### CALCULATE ACCURACY SCORES / ERRORS ###
  
  # Calculate the Mean Squared Error (MSE) - a measure of how close the model's 
  # predictions are to the actual values
  
  mse <- mean((predictions - test$TTL)^2)
  #print(paste("Mean Squared Error:", mse))
  
  # Check how many predictions are within 50 points of the actual values. 
  # Assign 1 if within range, otherwise 0
  accuracy_vector <- ifelse(abs(predictions - test$TTL) <= 50, 1, 0)
  
  # Calculate the overall percentage of predictions that met the accuracy criteria
  accuracy_percentage <- mean(accuracy_vector) * 100
  #print(paste("Personal Accuracy Score:", round(accuracy_percentage, 2), "%"))
  
  # Filter out rows where actual fantasy points are zero to avoid division by zero in MAPE
  test_non_zero <- test[test$TTL != 0,]
  predictions_non_zero <- predictions[test$TTL != 0]
  
  # Calculate the Mean Absolute Percentage Error (MAPE) - represents the average 
  # error between predicted and actual values as a percentage
  
  #mape_value <- mape(test_non_zero$TTL, predictions_non_zero)
  #print(paste("Mean Absolute Percentage Error (MAPE):", round(mape_value, 2), "%"))
  
  ### PREDICTIONS ###
  # Check if input$OneAgo and input$TwoAgo are numeric values and are not NA
  if (!(is.null(input$OneAgo) || input$OneAgo == "" || input$TwoAgo == "" || is.null(input$TwoAgo) || is.null(input$Years) || input$Years == "" ))
  {
  # Predict fantasy points for a hypothetical running back
  new_player <- data.frame(
    PrevYear1_TTL = as.integer(input$OneAgo),
    PrevYear2_TTL = as.integer(input$TwoAgo),
    YearsPro = as.integer(input$Years),
    Pos = factor(input$FFBPosition, levels = unique(levels(train$Pos)))
  )
  predicted_points <- predict(model, newdata = new_player)
  return(paste("Predicted fantasy points for the ", input$FFBPosition, ": ", round(predicted_points, 2)))
  }
  else{
    return()
  }
})

# Display the ffb information in the text output area
output$football_info <- renderText({
  football_info()
})

# Display the ffb info in the plot output area
output$football_plot <- renderPlot({
  football_plot()
})


}


# Modify the HTML head
ui <- tagList(
  # Custom Icon
  tags$script(
    HTML('$(document).ready(function() {
               $("head").append(\'<link rel="icon" type="image/png" href="computer.png">\');
             });')
  ),
  #Custom Title
  tags$head(
    tags$script(HTML('document.title = "George F. Hunt IV\'s Shiny App";'))
  ),
  ui
)

# Run the Shiny app
shinyApp(ui, server = server)
