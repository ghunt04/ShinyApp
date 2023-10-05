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
      #updateSelectInput(session, "HockeyPlayer", choices = c(players, "All"))  # Update player selection choices
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
