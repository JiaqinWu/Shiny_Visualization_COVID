# Load necessary packages
library(shiny)
library(tidyverse)
library(shinythemes)
library(tools)
library(sf)
library(countrycode)
library(tigris)
library(tidycensus) 
library(rnaturalearth)
library(DT)
library(shinyWidgets)

# Load necessary data/do necessary data manipulations 
df <- read.csv('COVID_report.csv')
df1 <- read.csv('COVID_report_age.csv')
df2 <- read.csv('Beds.csv')
df3 <- read.csv('Death.csv')

# Rename some columns in the df1
df1$Age <- ifelse(df1$Age == '4', '0-4',
                  ifelse(df1$Age == '11', '5-11',
                         ifelse(df1$Age == '17', '12-17', df1$Age)))

# Input a state level shapefiles
us_states <- states(cb = TRUE, resolution = "20m") %>%
  shift_geometry()

# Define UI
ui <- fluidPage(
  
  # Change app theme
  theme = shinytheme("sandstone"),
  
  tags$head(
    tags$style(HTML("
      body {
        font-family: 'Times New Roman', serif;
        font-size: 14px; /* Adjust the font size as needed */
      }
    "))
  ),
  
  # Application title with styling
  tags$div(
    style = "background-color: #f8f9fa; padding: 15px; border-bottom: 2px solid #dee2e6;",
    titlePanel("COVID-19 Data Insights Dashboard")
  ),
  
  # Subtitle with student name and styling
  tags$div(
    style = "background-color: #f8f9fa; padding: 10px; border-bottom: 1px solid #dee2e6;",
    h3("Jiaqin Wu")
  ), 
  
  # Introductory information
  fluidRow(
    column(12, 
           tags$div(
             style = "background-color: #ffffff; padding: 15px; border: 1px solid #dee2e6; margin-top: 15px;",
             h4("Welcome to the", strong("COVID-19 Data Insights Dashboard!")),
             p("This dashboard offers visualizations depicting COVID-19 data trends, encompassing case numbers, inpatient bed utilization, and death cases. It utilizes", strong("Washington, DC"), "as a baseline to elucidate the information presented in our dashboard."),
             p("Use the tabs below to navigate through different plots and gain insights into the data."),
             p("Data Source: ", a("U.S. Department of Health & Human Services", href = "https://healthdata.gov/Hospital/COVID-19-Reported-Patient-Impact-and-Hospital-Capa/g62h-syeh", target = "_blank")),
             p("Last Updated: November 10, 2023"), 
             p("Pipeline: ", a("GitHub",href="https://github.com/JiaqinWu/Shiny_Visualization_COVID", target = "_blank")),
             p("Note: Due to incomplete data for November 2023, the dashboard utilizes data from March 2020 to October 2023."),
             p("The data dictionary is as follows:",
               tags$ul(
                 tags$li(tags$b("State"), " - the state where the data were collected"),
                 tags$li(tags$b("Year"), " - the year when the data were collected"),
                 tags$li(tags$b("Month"), " - the month when the data were collected"),
                 tags$li(tags$b("COVID Scenario"), " - the COVID scenario of patients (Confirmed/Suspected)"),
                 tags$li(tags$b("Demographic Group"), " - the demographic group for whom the data were collected (adult/pediatric)"),
                 tags$li(tags$b("Age Band"), " - the age band for whom the data were collected (0-4, 5-11, 12-17, etc.)"),
                 tags$li(tags$b("Reported Cases"), " - the number of cases (Confirmed/Suspected/Death) reported for the selected COVID scenario, state, and demographic group in the chosen year and month"),
                 tags$li(tags$b("Reported Beds"), " - the number of beds for the selected state in the chosen year and month"),
                 tags$li(tags$b("Reported Hospitalizations"), " - the number of hospitalizations reporting the data for the selected COVID scenario, state, and demographic group in the chosen year and month")
               )
             )
           )
    )
  ),
  
  
  # Create a tabset with four tabs
  navbarPage(
    "My Application",
    
    # First tab
    navbarMenu("Case Number",
               tabPanel("Plot: Case Number",
                        sidebarPanel(
                          img(src = "healthdata.png", height = 80, width = 300),
                          br(),
                          br(),
                          # Input function 1
                          selectInput("state_1_1", "Select A State:",
                                      choices = unique(df$state),selected='DC'),
                          # New dropdown input
                          dateRangeInput(
                            "date_range_1_1",
                            "Select Time Period:",
                            start = "2020-03-01",
                            end = "2023-10-31",
                            format = "mm/yyyy", 
                            startview = "year",  # Set the initial view to year
                          ),
                          # New dropdown input
                          selectInput("covid_1_1", "Select the COVID Scenario(s):",
                                      choices = c("All Types", unique(df$covid))),
                          # New dropdown input
                          selectInput("group_1_1",
                                      HTML("Select A Demographic Group:<br><small>Note: Adult includes people 18 years and older, Pediatric includes people under 18.</small>"),
                                      choices = unique(df$group),
                                      helpText("Note: Adult includes people aged 18 and above. Pediatric includes people below 18.")),
                          # New checkbox input
                          checkboxInput("show_labels_1_1", "Show labels on lines", value = FALSE),
                          # Horizon Line
                          hr(),
                          # Download Button
                          downloadButton(
                            outputId = "download_epicurve_1",
                            label = "Download plot"),
                        ),
                        
                        # Show plot 1 and appropriate titles
                        mainPanel(
                          # show additional elements (if applicable)
                          
                          # title text for plot 1 (h3)
                          h3(textOutput("title1_1")),
                          # subtitle text for plot 1 (bold text) 
                          tags$b("Data is from March 2020 to October 2023"),
                          hr(),
                          p("In Washington, D.C., COVID-19 data reporting by hospitals commenced in July 2020. The peak of COVID cases occurred between August 2020 and February 2023, with a notable decline after June 2023, indicating the waning impact of the COVID. The number of pediatric cases remain substantially lower compared to the adult population, emphasizing the heightened susceptibility of the adult group to the impact of COVID. But their trend of case number remains similar."),
                          p("Across other states, the initiation of case data collection varied, primarily starting in July 2020. However, the peak periods differed, with many experiencing peaks in 2021 and 2022. Notably, a majority of U.S. states began to witness a decline in case numbers from late 2022 or early 2023. A similar pattern appears in the pediatric as well. We can thus infer that the enduring impact of COVID has weakened after 2023."),
                          hr(),
                          # show plot 1
                          plotOutput('plot1_1'),
                          br(),
                          hr(),
                          p("To utilize this app, manipulate the widgets on the side to explore the trends in case numbers for other states according to your requirements! Additionally, you can download a high-quality image of your plot using the download button.", style = "font-weight: bold;"),
                          hr(),
                        )
               ),
               tabPanel("Table: Case Number",
                        sidebarPanel(
                          img(src = "healthdata.png", height = 80, width = 300),
                          br(),
                          br(),
                          # Input function 1
                          selectizeInput("state_1_2", "Select State(s):",
                                         choices = unique(df$state),
                                         multiple = TRUE),
                          # New dropdown input
                          selectInput("year_1_2", "Select A Year:",
                                      choices = unique(df$Year)),
                          # New dropdown input
                          selectizeInput("month_1_2", "Select the Month(s):",
                                         choices = month.abb,
                                         multiple = TRUE,
                                         selected = month.abb),
                          # New dropdown input
                          selectInput("covid_1_2", "Select the COVID Scenario(s):",
                                      choices = c("All Types", unique(df$covid))),
                          # New dropdown input
                          selectInput("group_1_2", 
                                      HTML("Select the Demographic Group(s):<br><small>Note: Adult includes people 18 years and older, Pediatric includes people under 18.</small>"),
                                      choices = c("All Groups", unique(df$group)),
                                      helpText("Note: Adult includes people aged 18 and above. Pediatric includes people below 18.")),
                          # Horizon Line
                          hr(),
                          # Download Button
                          downloadButton(
                            outputId = "download_table_1",
                            label = "Download table"),
                        ),
                        
                        
                        # Show table 1 and appropriate titles
                        mainPanel(
                          # show additional elements (if applicable)
                          
                          # title text for table 1 (h3)
                          h3(textOutput("title_table1")),
                          # subtitle text for table 1 (bold text) 
                          tags$b("Data is from March 2020 to October 2023"),
                          hr(),
                          # show table 1
                          tableOutput('table_1'),
                          hr(),
                          tags$b("This table only shows the first 10 rows"),
                          br(),
                          hr(),
                          p("To utilize this app, manipulate the widgets on the side to customize the table according to your requirements! Additionally, you can download the selected dataset using the download button.", style = "font-weight: bold;"),
                          p("Some specific data dictionary is as follows:"),
                          tags$ul(
                            tags$li(tags$b("group"), " - the demographic group for whom the data were collected (adult/pediatric)"),
                            tags$li(tags$b("covid"), " - the COVID scenario of patients (Confirmed/Suspected)"),
                            tags$li(tags$b("coverage"), " - the type of data, indicating whether it represents hospitalizations reporting the case number(hospital) or patient cases(patients)"),
                            tags$li(tags$b("value"), " - the number of cases/hospitalizations reporting the case number for the selected COVID scenario, state, and demographic group in the chosen year and month")), 
                          hr(),
                          hr(),
                          # GitHub Link
                          h4("To review the methodology and access the completed dataset, ", a("click here!",href="https://github.com/JiaqinWu/Shiny_Visualization_COVID", target = "_blank")),
                          hr(),
                        )
               )
    ), 
    
    # Second tab
    navbarMenu("Case Number by Age Band",
               tabPanel("Plot: Case Number by Age Band",
                        sidebarPanel(
                          img(src = "healthdata.png", height = 80, width = 300),
                          br(),
                          br(),
                          # Input function 1
                          selectInput("state_2_1", "Select A State:",
                                      choices = unique(df1$state),selected='DC'),
                          # New dropdown input
                          dateRangeInput("date_range_2_1", "Select Time Period:",
                                         start = "2020-03-01", end = "2023-10-31", format = "mm/yyyy",startview='year'),
                          # New dropdown input
                          selectInput("covid_2_1", "Select the COVID Scenario(s):",
                                      choices = c("All Types", unique(df1$covid))),
                          # New dropdown input
                          selectInput("group_2_1", 
                                      HTML("Select A Demographic Group:<br><small>Note: Adult includes people 18 years and older, Pediatric includes people under 18.</small>"),
                                      choices = unique(df1$group),
                                      helpText("Note: Adult includes people aged 18 and above. Pediatric includes people below 18.")),
                          # New dropdown input
                          selectInput("age_2_1", "Select An Age Band:",
                                      choices = unique(df1$Age)),
                          # New checkbox input
                          checkboxInput("show_labels_2_1", "Show labels on lines", value = FALSE),
                          # Horizon Line
                          hr(),
                          # Download Button
                          downloadButton(
                            outputId = "download_epicurve_2",
                            label = "Download plot"),
                        ),
                        
                        # Show plot 2 and appropriate titles
                        mainPanel(
                          # show additional elements (if applicable)
                          
                          # title text for plot 2 (h3)
                          h3(textOutput("title2_1")),
                          # subtitle text for plot 2 (bold text) 
                          tags$b("Data is from March 2020 to October 2023"),
                          hr(),
                          p("In Washington D.C., COVID data reporting by hospitals commenced in July 2020, with the peak occurring between August 2020 and February 2023. Subsequently, after June 2023, there was a significant decline in COVID cases, indicating the waning impact of the COVID. Notably, individuals aged 30-39 and 50-79 represented the highest proportions of COVID cases within various age bands, highlighting their prominent impact during the pandemic."),
                          p("For other states, data collection mostly started in July 2020, with peak periods varying, but commonly observed in 2021 and 2022. Across the USA, states began to witness a decline in case numbers from late 2022 or early 2023. Among these states, a recurring pattern emerges, with the 30-39 and 50+ age bands showing the highest proportions of COVID patients, and some states indicating a notable presence in the unknown age group. The number of pediatric cases remain substantially lower compared to the adult population in every state, emphasizing the heightened susceptibility of the adult group to the impact of COVID."),
                          hr(),
                          # show plot 2
                          plotOutput('plot2_1'),
                          br(),
                          hr(),
                          p("To utilize this app, manipulate the widgets on the side to explore the trends in case numbers in people groups with different age bands for other states according to your requirements! Additionally, you can download a high-quality image of your plot using the download button.", style = "font-weight: bold;"),
                          hr(),
                        )
               ),
               tabPanel("Table: Case Number by Age Band",
                        sidebarPanel(
                          img(src = "healthdata.png", height = 80, width = 300),
                          br(),
                          br(),
                          # Input function 1
                          selectizeInput("state_2_2", "Select State(s):",
                                         choices = unique(df1$state),
                                         multiple = TRUE),
                          # New dropdown input
                          selectInput("year_2_2", "Select A Year:",
                                      choices = unique(df1$Year)),
                          # New dropdown input
                          selectizeInput("month_2_2", "Select the Month(s):",
                                         choices = month.abb,
                                         multiple = TRUE,
                                         selected = month.abb),
                          # New dropdown input
                          selectInput("covid_2_2", "Select the COVID Scenario:",
                                      choices = c("All Types", unique(df1$covid))),
                          # New dropdown input
                          selectInput("group_2_2", 
                                      HTML("Select the Demographic Group(s):<br><small>Note: Adult includes people 18 years and older, Pediatric includes people under 18.</small>"),
                                      choices = c("All Groups", unique(df1$group)),
                                      helpText("Note: Adult includes people aged 18 and above. Pediatric includes people below 18.")),
                          # New dropdown input
                          selectizeInput("age_2_2", "Select Age Band(s):",
                                         choices = unique(df1$Age),
                                         multiple = TRUE),
                          # Horizon Line
                          hr(),
                          # Download Button
                          downloadButton(
                            outputId = "download_table_2",
                            label = "Download table"),
                        ),
                        
                        # Show table 2 and appropriate titles
                        mainPanel(
                          # show additional elements (if applicable)
                          
                          # title text for table 2 (h3)
                          h3(textOutput("title_table2")),
                          # subtitle text for table 2 (bold text) 
                          tags$b("Data is from March 2020 to October 2023"),
                          hr(),
                          # show table 2
                          tableOutput('table_2'),
                          hr(),
                          tags$b("This table only shows the first 10 rows"),
                          br(),
                          hr(),
                          p("To utilize this app, manipulate the widgets on the side to customize the table according to your requirements! Additionally, you can download the selected dataset using the download button.", style = "font-weight: bold;"),
                          p("Some specific data dictionary is as follows:"),
                          tags$ul(
                            tags$li(tags$b("group"), " - the demographic group for whom the data were collected (adult/pediatric)"),
                            tags$li(tags$b("covid"), " - the COVID scenario of patients (Confirmed/Suspected)"),
                            tags$li(tags$b("Age"), " - the age band for whom the data were collected (0-4, 5-11, 12-17, etc.)"),
                            tags$li(tags$b("coverage"), " - the type of data, indicating whether it represents hospitalizations reporting the case number(hospital) or patient cases(patients)"),
                            tags$li(tags$b("value"), " - the number of cases/hospitalizations reporting the case number for the selected COVID scenario, state, and demographic group in the chosen year and month")),
                          hr(),
                          hr(),
                          # GitHub Link
                          h4("To review the methodology and access the completed dataset, ", a("click here!",href="https://github.com/JiaqinWu/Shiny_Visualization_COVID", target = "_blank")),
                          br(),
                        )
               )
    ), 
    
    # Third tab
    navbarMenu("Inpatient Bed Number",
               tabPanel("Plot: Inpatient Bed Number",
                        sidebarPanel(
                          img(src = "healthdata.png", height = 80, width = 300),
                          br(),
                          br(),
                          # Input function 1
                          selectInput("state_3_1", "Select A State:",
                                      choices = unique(df2$state),selected='DC'),
                          # New dropdown input
                          selectInput("year_3_1", "Select A Year:",
                                      choices = unique(df2$Year)),
                          # New dropdown input
                          selectInput("month_3_1", "Select A Month:",
                                      choices = month.abb),  # Use month.abb for month dropdown
                          # New checkbox input
                          checkboxInput("show_labels_3_1", "Show labels on bars", value = TRUE),
                          # Group of checkboxes under the label "Choose the Variables"
                          checkboxGroupInput("choose_variables_1", "Choose the Variable(s):",
                                             choices = c("Inpatient beds", "Used inpatient beds", "Used inpatient beds for COVID"),
                                             selected = c("Inpatient beds", "Used inpatient beds", "Used inpatient beds for COVID")),
                          # Horizon Line
                          hr(),
                          # Download Button
                          downloadButton(
                            outputId = "download_epicurve_3",
                            label = "Download plot"),
                        ),
                        
                        # Show plot 3 and appropriate titles
                        mainPanel(
                          # show additional elements (if applicable)
                          
                          # title text for plot 3 (h3)
                          h3(textOutput("title3_1")),
                          # subtitle text for plot 3 (bold text)
                          tags$b("Data is from March 2020 to October 2023"),
                          hr(),
                          p("In Washington D.C., the reporting of inpatient bed numbers by hospitals has been consistent since May 2020. Notably, around 80% of these beds were consistently occupied, with 5% to 10% specifically allocated for COVID cases during the pandemic. The proportion of beds used for COVID dropped to below 5% after February 2023. Remarkably, January 2022 recorded the highest proportion of beds used for COVID, reaching around 25%, signaling a potential surge. However, since June 2023, the usage dropped to less than 1%, indicating the diminishing impact of COVID as a public health challenge."),
                          p("Across other states, the utilization of inpatient beds during the pandemic varies, typically falling between 50% and 90%. Beds allocated for COVID cases during this period generally remained below 20%. This suggests the importance of maintaining a reserve of 20% of inpatient beds to effectively address potential future public health challenges."),
                          hr(),
                          # show plot 3
                          plotOutput('plot3_1'),
                          br(),
                          hr(),
                          p("To utilize this app, manipulate the widgets on the side to explore the trends in inpatient beds and utilization rates during pandemic for other states according to your requirements! Additionally, you can download a high-quality image of your plot using the download button.", style = "font-weight: bold;"),
                          hr(),
                          p("Some specific data dictionary is as follows:"),
                          tags$ul(
                            tags$li(tags$b("Inpatient beds"), " - reported total number of staffed inpatient beds including all overflow and surge/expansion beds used for inpatients (includes all ICU beds) for the selected state in the chosen year and month"),
                            tags$li(tags$b("Used inpatient beds"), " - reported total number of staffed inpatient beds that are occupied for the selected state in the chosen year and month"),
                            tags$li(tags$b("Used inpatient beds for COVID"), " - reported patients currently hospitalized in an inpatient bed who have suspected or confirmed COVID-19 for the selected state in the chosen year and month"),
                          )
                        )
               ),
               tabPanel("Table: Inpatient Bed Number",
                        sidebarPanel(
                          img(src = "healthdata.png", height = 80, width = 300),
                          br(),
                          br(),
                          # Input function 1
                          selectizeInput("state_3_2", "Select State(s):",
                                         choices = unique(df2$state),
                                         multiple = TRUE),
                          # New dropdown input
                          selectInput("year_3_2", "Select A Year:",
                                      choices = unique(df2$Year)),
                          # New dropdown input
                          selectizeInput("month_3_2", "Select the Month(s):",
                                         choices = month.abb,
                                         multiple = TRUE,
                                         selected = month.abb),
                          # Group of checkboxes under the label "Choose the Variables"
                          checkboxGroupInput("choose_variables_2", "Choose the Variable(s):",
                                             choices = c("Inpatient beds", "Used inpatient beds", "Used inpatient beds for COVID"),
                                             selected = c("Inpatient beds", "Used inpatient beds", "Used inpatient beds for COVID")),
                          # Horizon Line
                          hr(),
                          # Download Button
                          downloadButton(
                            outputId = "download_table_3",
                            label = "Download table"),
                        ),
                        
                        # Show table 3 and appropriate titles
                        mainPanel(
                          # show additional elements (if applicable)
                          
                          # title text for table 3 (h3)
                          h3(textOutput("title_table3")),
                          # subtitle text for table 1 (bold text) 
                          tags$b("Data is from March 2020 to October 2023"),
                          hr(),
                          # show table 3
                          tableOutput('table_3'),
                          tags$b("This table only shows the first 10 rows"),
                          br(),
                          hr(),
                          p("To utilize this app, manipulate the widgets on the side to customize the table according to your requirements! Additionally, you can download the selected dataset using the download button.", style = "font-weight: bold;"),
                          p("Some specific data dictionary is as follows:"),
                          tags$ul(
                            tags$li(tags$b("coverage"), " - the type of data, indicating whether it represents hospitalizations reporting the case number(hospital) or patient cases(patients)"),
                            tags$li(tags$b("Variable"), " - the selected variable(s) (Inpatient beds/Used inpatient beds/Used inpatient beds for COVID)"),
                            tags$li(tags$b("value"), " - the number of beds reported for the selected COVID scenario, state, and demographic group in the chosen year and month")),
                          hr(),
                          hr(),
                          # GitHub Link
                          h4("To review the methodology and access the completed dataset, ", a("click here!",href="https://github.com/JiaqinWu/Shiny_Visualization_COVID", target = "_blank")),
                          br(),
                        )
               )
    ), 
    
    # Fourth tab
    navbarMenu("Distribution of Death Cases by State",
               tabPanel("Plot: Distribution of Death Cases by State",
                        sidebarPanel(
                          img(src = "healthdata.png", height = 80, width = 300),
                          br(),
                          br(),
                          # Input function 4
                          selectInput("year_4_1", "Select A Year:",
                                      choices = unique(df3$Year)),
                          # New dropdown input
                          selectInput("month_4_1", "Select A Month:",
                                      choices = month.abb),  # Use month.abb for month dropdown
                          # New dropdown input
                          selectInput("variable_4_1", "Select A Variable:",
                                      choices = c("Reported Cases", "Reported Hospitalizations")),
                          # Horizon Line
                          hr(),
                          # Download Button
                          downloadButton(
                            outputId = "download_epicurve_4",
                            label = "Download plot"),
                        ),
                        
                        # Show plot 4 and appropriate titles
                        mainPanel(
                          # show additional elements (if applicable)
                          
                          # title text for plot 4 (h3)
                          h3(textOutput("title4_1")),
                          # subtitle text for plot 3 (bold text)
                          tags$b("Data is from March 2020 to October 2023"),
                          hr(),
                          p("From the death case distribution map, noteworthy observations emerge. Examining the death case numbers, April 2020 stands out as the most severe month, recording the highest number of fatalities. The data also reveals heightened severity from April 2020 to March 2021 and August 2021 to February 2022, indicating periods of increased mortality."),
                          p("At the state level, FL, NY, TX, CA, OR, and MI consistently exhibit higher severity over most timeframes. However, specific regions within states experience distinct public health challenges. For instance, AZ emerges as the most severe state only from October 2021 to January 2022, while IA takes the lead from December 2022 to March 2023. These isolated periods suggest unique health challenges requiring targeted interventions."),
                          p("The elevated death case numbers in these states may be attributed to factors such as population density or limitations in healthcare infrastructure and technology. Allocating additional resources, such as increased funding for hospital construction and hiring skilled medical professionals, could aid these states in addressing future public health challenges effectively."),
                          hr(),
                          # show plot 4
                          plotOutput('plot4_1'),
                          br(),
                          hr(),
                          p("To utilize this app, manipulate the widgets on the side to explore the trends in the distribution of the number of death cases for COVID across states during pandemic for different time periods according to your requirements! Additionally, you can download a high-quality image of your plot using the download button.", style = "font-weight: bold;"),
                          hr()
                        )
               ),
               tabPanel("Table: Distribution of Death Cases by State",
                        sidebarPanel(
                          img(src = "healthdata.png", height = 80, width = 300),
                          br(),
                          br(),
                          # Input function 4
                          selectInput("year_4_2", "Select A Year:",
                                      choices = unique(df3$Year)),
                          # New dropdown input
                          selectizeInput("month_4_2", "Select the Month(s):",
                                         choices = month.abb,
                                         multiple = TRUE,
                                         selected = month.abb),
                          # Use month.abb for month dropdown
                          # Horizon Line
                          hr(),
                          # Download Button
                          downloadButton(
                            outputId = "download_table_4",
                            label = "Download table"),
                        ),
                        
                        # Show table 4 and appropriate titles
                        mainPanel(
                          # show additional elements (if applicable)
                          
                          # title text for table 4 (h3)
                          h3(textOutput("title_table4")),
                          # subtitle text for table 1 (bold text) 
                          tags$b("Data is from March 2020 to October 2023"),
                          hr(),
                          # show table 4
                          dataTableOutput("table_4"),
                          br(),
                          hr(),
                          p("To utilize this app, manipulate the widgets on the side to customize the table according to your requirements! Additionally, you can download the selected dataset using the download button.", style = "font-weight: bold;"),
                          p("Some specific data dictionary is as follows:"),
                          tags$ul(
                            tags$li(tags$b("deaths_covid"), " - the number of patients with suspected or confirmed COVID-19 who died in the hospital, ED, or overflow location for the selected state in the chosen year and month"),
                            tags$li(tags$b("deaths_covid_coverage"), " - the number of hospitalizations reporting the patient number with suspected or confirmed COVID-19 who died in the hospital, ED, or overflow location for the selected state in the chosen year and month"),),
                          hr(),
                          hr(),
                          # GitHub Link
                          h4("To review the methodology and access the completed dataset, ", a("click here!",href="https://github.com/JiaqinWu/Shiny_Visualization_COVID", target = "_blank")),
                          br(),
                        )
               )   
    )
  )
)




# Define server logic
server <- function(input, output, session) {
  
  # Define function to get available months
  get_available_months <- function(selected_year) {
    if (selected_year == 2020) {
      return(month.abb[3:12])
    } else if (selected_year == 2023) {
      return(month.abb[1:10])
    } else {
      return(month.abb)
    }
  }
  
  
  # Function to filter data based on inputs
  filtered_data <- reactive({
    df %>%
      filter(state == input$state_1_1 &
               between(as.Date(paste(df$Year, df$Month, "01", sep = "-")), input$date_range_1_1[1], input$date_range_1_1[2]) &
               group == input$group_1_1)
  })
  
  # Define the title dynamically for plot 1
  output$title1_1 <- renderText({
    paste("Total Number of Reported Cases and Hospitalizations in",
          toTitleCase(input$group_1_1), "Group in",
          input$state_1_1, "from",
          format(input$date_range_1_1[1], "%b %Y"),
          "to",
          format(input$date_range_1_1[2], "%b %Y"))
  })
  
  
  # Function to generate plot 1
  plot1_1 <- reactive({
    filtered_data_plot1 <- filtered_data()
    
    if (input$covid_1_1 == "All Types") {
      # Aggregate data for "All Types" by summing counts for "Confirmed" and "Suspected"
      filtered_data_plot1 <- filtered_data_plot1 %>%
        group_by(across(c(Year, Month, coverage))) %>%
        summarise(value = sum(value), .groups = 'drop')
    } else {
      # Filter data based on selected COVID scenario
      filtered_data_plot1 <- filtered_data_plot1 %>%
        filter(covid == input$covid_1_1)
    }
    
    p <- ggplot(filtered_data_plot1, aes(x = as.Date(paste(Year, Month, "01", sep = "-")), y = value, color = coverage)) +
      geom_line() +
      geom_point()
    
    if(input$show_labels_1_1) {
      p <- p + geom_text(aes(label = value), vjust = -0.5, size = 3)
    }
    
    p +
      labs(x = "Time Period",
           y = "Number",
           caption = "Data Source: U.S. Department of Health & Human Services") +
      theme_minimal() +
      scale_color_discrete(name = "", labels = c("Reported Hospitalizations", "Reported Cases")) +
      theme(text = element_text(size = 14, family = "Times New Roman"),
            axis.text.x = element_text(size = 8)) +
      scale_x_date(labels = scales::date_format("%b %Y"), date_breaks = "3 months")
  })
  
  
  # Render plot 1
  output$plot1_1 <- renderPlot({
    plot1_1()
  })
  
  # Add a download button for plot 1
  output$download_epicurve_1 <- downloadHandler(
    
    filename = function() {
      stringr::str_glue("Plot_COVID_{input$group_1_1}_{input$state_1_1}_{format(input$date_range_1_1[1], '%b%Y')}_{format(input$date_range_1_1[2], '%b%Y')}.png")
    },
    
    content = function(file) {
      ggsave(file, plot1_1(), width = 8, height = 5, dpi = 300)
    })
  
  
  # Update reactiveValues when data changes
  observe({
    data_values$filtered_data_table1 <- if (input$covid_1_2 != "All Types") {
      filtered_data11() %>% filter(covid == input$covid_1_2)
    } else {
      filtered_data11()
    }
  })
  
  # Update month dropdown based on selected year
  observe({
    updateSelectInput(session, "month_1_2", choices = get_available_months(input$year_1_2))
  })
  
  # Create reactiveValues to store filtered data
  data_values <- reactiveValues(filtered_data_table1 = NULL)
  
  # Function to filter data based on inputs
  filtered_data11 <- reactive({
    df %>%
      filter(state %in% input$state_1_2 & 
               Year == input$year_1_2 & 
               Month %in% match(input$month_1_2, month.abb))
  })
  
  
  # Update reactiveValues when data changes
  observe({
    data_values$filtered_data_table1 <- if (input$covid_1_2 != "All Types") {
      filtered_data11() %>% filter(covid == input$covid_1_2)
    } else {
      filtered_data11()
    }
  })
  
  observe({
    data_values$filtered_data_table1 <- if (input$group_1_2 != "All Groups") {
      filtered_data11() %>% filter(group == input$group_1_2)
    } else {
      filtered_data11()
    }
  })
  
  
  # Define the title dynamically for table 1
  output$title_table1 <- renderText({
    paste("Total Number of Reported Cases and Hospitalizations in", 
          "Selected Group(s) in Selected State(s) in Selected Month(s) in", 
          input$year_1_2)
  })
  
  
  # Function to generate table 1
  output$table_1 <- renderTable({
    head(data_values$filtered_data_table1, 10)
  }, rownames = FALSE)
  
  
  # Add a download button for table 1
  output$download_table_1 <- downloadHandler(
    filename = function() {
      selected_months <- input$month_1_2
      selected_states <- input$state_1_2
      stringr::str_glue("Table_COVID_{paste(selected_states, collapse = '_')}_{paste(selected_months, collapse = '_')}_{input$year_1_2}.csv")
    },
    content = function(file) {
      write.csv(data_values$filtered_data_table1, file)
    }
  )
  
  
  
  
  # Function to filter data based on inputs
  filtered_data2 <- reactive({
    df1 %>%
      filter(state == input$state_2_1 &
               between(as.Date(paste(Year, Month, "01", sep = "-")), input$date_range_2_1[1], input$date_range_2_1[2]) &
               (group == input$group_2_1 | (input$group_2_1 == "Pediatric" & Age %in% c('0-4', '5-11', '12-17'))))
  })
  
  # Update COVID dropdown based on selected Demographic group
  observe({
    if (input$group_2_1 == "Pediatric") {
      updateSelectInput(session, "covid_2_1", choices = "Confirmed")
    } else {
      updateSelectInput(session, "covid_2_1", choices = c("All Types", unique(df1$covid)))
    }
  })
  
  # Update age band dropdown based on selected Demographic group
  observe({
    unique_age_values <- unique(filtered_data2()$Age)
    if (input$group_2_1 == "Pediatric") {
      # Define custom order for pediatric group
      custom_order <- c('0-4', '5-11', '12-17', 'unknown')
      unique_age_values <- custom_order[custom_order %in% unique_age_values]
    }
    updateSelectInput(session, "age_2_1", choices = unique_age_values)
  })
  
  # Define the title dynamically for plot 2
  output$title2_1 <- renderText({
    paste("Total Number of Reported Cases and Hospitalizations in",
          input$age_2_1, " ",
          toTitleCase(input$group_2_1), "Group in",
          input$state_2_1, "from",
          format(input$date_range_2_1[1], "%b %Y"),
          "to",
          format(input$date_range_2_1[2], "%b %Y"))
  })
  
  # Function to generate plot 2
  plot2_1 <- reactive({
    filtered_data_plot2 <- filtered_data2()
    
    if (input$covid_2_1 == "All Types") {
      # Aggregate data for "All Types" by summing counts for "Confirmed" and "Suspected"
      filtered_data_plot2 <- filtered_data_plot2 %>%
        group_by(across(c(Year, Month, coverage, Age))) %>%
        summarise(value = sum(value), .groups = 'drop') %>% 
        filter(Age == input$age_2_1)
    } else {
      # Filter data based on selected COVID scenario
      filtered_data_plot2 <- filtered_data_plot2 %>%
        filter(covid == input$covid_2_1, Age == input$age_2_1)
    }
    
    
    p <- ggplot(filtered_data_plot2, aes(x = as.Date(paste(Year, Month, "01", sep = "-")), y = value, color = coverage)) +
      geom_line() +
      geom_point()
    
    if(input$show_labels_2_1) {
      p <- p + geom_text(aes(label = value), vjust = -0.5, size = 3)
    }
    
    p +
      labs(x = "Time Period",
           y = "Number",
           caption = "Data Source: U.S. Department of Health & Human Services") +
      theme_minimal() +
      scale_color_discrete(name = "", labels = c("Reported Hospitalizations", "Reported Cases")) +
      theme(text=element_text(size=14, family = "Times New Roman"),
            axis.text.x = element_text(size = 8)) +
      scale_x_date(labels = scales::date_format("%b %Y"), date_breaks = "3 months")
  })
  
  # Render plot 2
  output$plot2_1 <- renderPlot({
    plot2_1()
  })
  
  # Add a download button for plot 2
  output$download_epicurve_2 <- downloadHandler(
    
    filename = function() {
      stringr::str_glue("Plot_COVID_{input$age_2_1}_{input$group_2_1}_{input$state_2_1}_{format(input$date_range_2_1[1], '%b%Y')}_{format(input$date_range_2_1[2], '%b%Y')}.png")
    },
    
    content = function(file) {
      ggsave(file, plot2_1(), width = 8, height = 5, dpi = 300)
    }
  )
  
  # Update month dropdown based on selected year
  observe({
    updateSelectInput(session, "month_2_2", choices = get_available_months(input$year_2_2))
  })
  
  # Update COVID dropdown based on selected Demographic group
  observe({
    if (input$group_2_2 == "Pediatric") {
      updateSelectInput(session, "covid_2_2", choices = "Confirmed")
    } else {
      updateSelectInput(session, "covid_2_2", choices = c("All Types", unique(df1$covid)))
    }
  })
  
  # Update age band dropdown based on selected Demographic group
  observe({
    unique_age_values <- unique(df1$Age)
    if (input$group_2_2 == "Pediatric") {
      # If Pediatric is selected, only allow age_2_2 < 18
      custom_order <- c('0-4', '5-11', '12-17', 'unknown')
      unique_age_values <- custom_order[custom_order %in% unique_age_values]
    } else if (input$group_2_2 == "Adult") {
      # If Adult is selected, only allow age_2_2 >= 18
      unique_age_values <- unique_age_values[unique_age_values %in% c('18-19', '20-29', '30-39', '40-49', '50-59', '60-69', '70-79', '80+', 'unknown')]
    }
    updateSelectInput(session, "age_2_2", choices = unique_age_values)
  })
  
  
  # Create reactiveValues to store filtered data
  data_values1 <- reactiveValues(filtered_data_table2 = NULL)
  
  # Function to filter data based on inputs
  filtered_data21 <- reactive({
    filtered_data <- df1 %>%
      filter(
        state %in% input$state_2_2 & 
          Year == input$year_2_2 & 
          Month %in% match(input$month_2_2, month.abb)
      )
    
    if (input$group_2_2 == 'All Groups' & input$covid_2_2 == 'All Types') {
      filtered_data <- filtered_data %>%
        filter(Age %in% input$age_2_2)
    } else if (input$group_2_2 == 'All Groups' & input$covid_2_2 != 'All Types') {
      filtered_data <- filtered_data %>%
        filter(Age %in% input$age_2_2, covid == input$covid_2_2)
    } else if (input$group_2_2 != 'All Groups' & input$covid_2_2 != 'All Types') {
      filtered_data <- filtered_data %>%
        filter(group == input$group_2_2, Age %in% input$age_2_2, covid == input$covid_2_2)
    } else if (input$group_2_2 != 'All Groups' & input$covid_2_2 == 'All Types') {
      filtered_data <- filtered_data %>%
        filter(group == input$group_2_2, Age %in% input$age_2_2)
    }})
  
  
  
  # Update reactiveValues when data changes
  observe({
    data_values1$filtered_data_table2 <- filtered_data21()
  })
  
  # Define the title dynamically for table 2
  output$title_table2 <- renderText({
    paste("Total Number of Reported Cases and Hospitalizations in",
          "Selected Age Group(s) in Selected State(s) in Selected Month(s) in", 
          input$year_2_2)
  })
  
  # Function to generate table 2
  output$table_2 <- renderTable({
    head(data_values1$filtered_data_table2, 10)
  }, rownames = FALSE)
  
  # Add a download button for table 2
  output$download_table_2 <- downloadHandler(
    filename = function() {
      selected_months <- input$month_2_2
      selected_states <- input$state_2_2
      selected_ages <- input$age_2_2
      stringr::str_glue("Table_COVID_{paste(selected_ages, collapse = '_')}_{paste(selected_states, collapse = '_')}_{paste(selected_months, collapse = '_')}_{input$year_3_2}.csv")
    },
    content = function(file) {
      write.csv(data_values1$filtered_data_table2, file)
    }
  )
  
  
  
  
  
  # Update month dropdown based on selected year
  observe({
    updateSelectInput(session, "month_3_1", choices = get_available_months(input$year_3_1))
    updateSelectInput(session, "month_3_2", choices = get_available_months(input$year_3_2))
  })
  
  # code to render plot 3
  filtered_data3 <- reactive({
    df2 %>%
      filter(state == input$state_3_1 & 
               Year == input$year_3_1 & 
               Month == match(input$month_3_1, month.abb))
  })
  
  
  
  # Define the title dynamically for plot 3
  output$title3_1 <- renderText({
    paste("Total Number of Inpatient Bed in", 
          input$state_3_1, "in", 
          input$month_3_1, 
          input$year_3_1)
  })
  
  # Function to generate plot 3
  plot3_1 <- reactive({
    # Map checkbox values to dataset variable names
    selected_variables_1 <- lapply(input$choose_variables_1, function(variable) {
      switch(variable,
             "Inpatient beds" = "inpatient_beds",
             "Used inpatient beds" = "inpatient_beds_used",
             "Used inpatient beds for COVID" = "inpatient_beds_used_covid")
    })
    
    
    # Filter data based on the selected variables
    filtered_data_plot3 <- filtered_data3() %>%
      filter(Variable %in% selected_variables_1)
    
    p <- ggplot(filtered_data_plot3, aes(x = Variable, y = value, fill = coverage)) +
      geom_bar(stat = 'identity', position = 'dodge') 
    
    if (input$show_labels_3_1) {
      p <- p + geom_text(aes(label = value), position = position_dodge(width = 0.9), vjust = -0.5,size=5)
    }
    
    
    # Dynamically set y-axis limits
    if (all(filtered_data_plot3$value == 0)) {
      p <- p + ylim(0, 100) + 
        theme_minimal() +
        labs(x = "Variable", y = "Number") +
        labs(caption = "Data Source: U.S. Department of Health & Human Services") +
        scale_fill_discrete(name = "", labels = c("Reported Hospitalizations", "Reported Beds")) +
        scale_x_discrete(labels = c("inpatient_beds" = "Inpatient beds",
                                    "inpatient_beds_used" = "Used inpatient Beds",
                                    "inpatient_beds_used_covid" = "Used inpatient beds for COVID"))+
        theme(text=element_text(size=10, family = "Times New Roman")) 
    } else {
      p <- p + ylim(0, 1.2*max(filtered_data_plot3$value)) + 
        theme_minimal() + 
        labs(x = "Variable", y = "Number") +
        labs(caption = "Data Source: U.S. Department of Health & Human Services") +
        scale_fill_discrete(name = "", labels = c("Reported Hospitalizations", "Reported Beds")) +
        scale_x_discrete(labels = c("inpatient_beds" = "Inpatient beds",
                                    "inpatient_beds_used" = "Used inpatient Beds",
                                    "inpatient_beds_used_covid" = "Used inpatient beds for COVID"))+
        theme(text=element_text(size=14, family = "Times New Roman")) 
    }
    
    p
  })
  
  # Render plot 3
  output$plot3_1 <- renderPlot({
    plot3_1()
  })
  
  # Add a download button for plot 3
  output$download_epicurve_3 <- downloadHandler(
    
    filename = function() {
      stringr::str_glue("Plot_Bed_{input$state_3_1}_{input$month_3_1}_{input$year_3_1}.png")
    },
    
    content = function(file) {
      ggsave(file, plot3_1(), width = 8, height = 5, dpi = 300)
    }
  )
  
  # Create reactiveValues to store filtered data
  data_values2 <- reactiveValues(filtered_data_table3 = NULL)
  
  # Function to filter data based on inputs
  filtered_data31 <- reactive({
    df2 %>%
      filter(state %in% input$state_3_2 & 
               Year == input$year_3_2 & 
               Month %in% match(input$month_3_2, month.abb))
  })
  
  # Reactive expression to update filtered data when inputs change
  observe({
    selected_variables_2 <- lapply(input$choose_variables_2, function(variable) {
      switch(variable,
             "Inpatient beds" = "inpatient_beds",
             "Used inpatient beds" = "inpatient_beds_used",
             "Used inpatient beds for COVID" = "inpatient_beds_used_covid")
    })
    
    # Update reactiveValues when data changes
    data_values2$filtered_data_table3 <- filtered_data31() %>%
      filter(Variable %in% selected_variables_2) %>%
      select(state, Year, Month, coverage, Variable, value)
  })
  
  # Function to generate table 3
  output$table_3 <- renderTable({
    head(data_values2$filtered_data_table3, 10)
  }, rownames = FALSE)
  
  
  
  # Add a download button for table 3
  output$download_table_3 <- downloadHandler(
    filename = function() {
      selected_months <- input$month_3_2
      selected_states <- input$state_3_2
      stringr::str_glue("Table_Bed_{paste(selected_states, collapse = '_')}_{paste(selected_months, collapse = '_')}_{input$year_3_2}.csv")
    },
    content = function(file) {
      write.csv(data_values2$filtered_data_table3, file)
    }
  )
  
  
  
  # Update month dropdown based on selected year
  observe({
    updateSelectInput(session, "month_4_1", choices = get_available_months(input$year_4_1))
    updateSelectInput(session, "month_4_2", choices = get_available_months(input$year_4_2))
  })
  
  # code to render plot 4
  filtered_data4 <- reactive({
    df3 %>%
      filter(Year == input$year_4_1 & 
               Month == match(input$month_4_1, month.abb))
  })
  
  
  
  # Define the title dynamically for plot 4
  output$title4_1 <- renderText({
    if (input$variable_4_1 == 'Reported Cases') {
      paste("Distribution of Deaths of patients who were confirmed or suspected COVID-19 dead in",
            input$month_4_1, input$year_4_1)
    } else {
      paste("Distribution of Hospitalizations which Reported Death Cases in",
            input$month_4_1, input$year_4_1)
    }
  })
  
  # Function to generate plot 4
  plot4_1 <- reactive({
    # Merge df3 with us_states based on the state variable
    merged_data <- left_join(us_states, filtered_data4(), by = c("STUSPS" = "state"))
    
    # Filter data based on input$variable_4
    if (input$variable_4_1 == 'Reported Cases') {
      fill_variable <- merged_data$deaths_covid
      fill_name <- "Death Case Number"
    } else {
      fill_variable <- merged_data$deaths_covid_coverage
      fill_name <- "Reported Hospitalization Number"
    }
    
    # Plotting using geom_sf
    ggplot() +
      geom_sf(data = merged_data, aes(fill = fill_variable), color = NA) +
      geom_sf(data = us_states, fill = NA, color = "black", size = 0.1) +
      geom_sf_text(data = merged_data,
                   aes(label = STUSPS),
                   colour = "black",
                   size = ifelse(merged_data$ALAND >= 3e10, 2.5, 0)) + 
      scale_fill_gradient(low = "white", high = "red", name = fill_name) +
      labs(caption = "Data Source: U.S. Department of Health & Human Services") +
      theme_void() +
      theme(axis.text = element_blank(),
            axis.title = element_blank(),
            axis.ticks = element_blank(),
            text=element_text(size=14, family = "Times New Roman"))
  })
  
  # Render plot 4
  output$plot4_1 <- renderPlot({
    plot4_1()
  })
  
  # Add a download button for plot 3
  output$download_epicurve_4 <- downloadHandler(
    
    filename = function() {
      stringr::str_glue("Plot_Distribution_{input$month_4_1}_{input$year_4_1}.png")
    },
    
    content = function(file) {
      ggsave(file, plot4_1(), width = 8, height = 5, dpi = 300)
    }
  )
  
  # Create reactiveValues to store filtered data
  data_values3 <- reactiveValues(filtered_data_table4 = NULL)
  
  # Function to filter data based on inputs
  filtered_data41 <- reactive({
    df3 %>%
      filter(Year == input$year_4_2 & 
               Month %in% match(input$month_4_2, month.abb))
  })
  
  # Update reactiveValues when data changes
  observe({
    selected_months <- input$month_4_2
    data_values3$filtered_data_table4 <- filtered_data41() %>%
      select(state, Year, Month, deaths_covid, deaths_covid_coverage)
  })
  
  
  # Define the title dynamically for table 4
  output$title_table4 <- renderText({
    selected_months <- input$month_4_2
    paste("Distribution of Death Cases and Hospitalizations which Reported Death Cases in Selected Month(s) in", input$year_4_2)
  })
  
  
  # Function to generate table 4 with DataTable
  output$table_4 <- renderDT({
    datatable(data_values3$filtered_data_table4, 
              options = list(aLengthMenu = c(5, 10, 20), 
                             iDisplayLength = 10))
  })
  
  # Add a download button for table 4
  output$download_table_4 <- downloadHandler(
    filename = function() {
      selected_months <- input$month_4_2
      stringr::str_glue("Table_Distribution_{paste(selected_months, collapse = '_')}_{input$year_4_2}.csv")
    },
    content = function(file) {
      write.csv(data_values3$filtered_data_table4, file)
    }
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)









