# Define UI for application that draws a histogram
shinyUI(fluidPage(theme = shinytheme("journal"),
  
  tags$head(
    tags$style(HTML("
          *{
            font-family: 'Arial';
           }
      "))),
  tags$head(
    tags$style(HTML("
          h1{
            font-family: 'Arial';
            font-size: 22pt;
           }
      "))),               
  
  # Application title
  h1("Interactive Tool for Visualizing LA Homelessness Data"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    #Subset the data
    sidebarPanel(
       tags$i(tags$b("Use these selectors to choose a particular subpopulation of interest.")), br(), br(),
       selectInput("gender", label = "Gender", 
                    choices = c("All", "Female", "Male", "Transgender", "Unknown")),
       selectInput("ethnicity", label = "Ethnicity",
                    choices = c("All","African American", "European American", "Latino",
                                "Other Ethnicity")),
       sliderInput("age", label = "Age",
                   min = min(multi_year$Age, na.rm = TRUE),
                   max = max(multi_year %>% filter(Age <= 100) %>% .$Age,
                             na.rm = TRUE),
                   value = c(min(multi_year$Age, na.rm = TRUE),
                             max(multi_year %>% filter(Age <= 100) %>% .$Age,
                                 na.rm = TRUE))),
       #Subset by SPA
       selectInput("SPA", label = "Service Planning Area",
                   choices = list("All" = "All","Antelope Valley"=1,"San Fernando Valley"=2,"San Gabriel Valley" = 3,
                                  "Metro Los Angeles" = 4, "West Los Angeles" = 5,"South Los Angeles" = 6,
                                  "East Los Angeles" = 7,"South Bay" = 8)),
       #First Characteristic to subset data by
       selectInput("var", label = "Population Characteristic 1",
                   choices = list("All"="All","Veteran" = "Veteran", "Persistenly Homeless" = "Chronic_Time",
                                  "Chronic Health Condition" = "Chronic_Condition","Chronically Homeless" = "Chronic",
                                  "First Time Homeless"="New_to_Homelessness",
                                  "Living With Child" = "Adult_With_Child","Ex-Foster Care" = "Foster_Care_Involvement",
                                  "Justice System-Past Year" = "Justice_System_Past_Year",
                                  "ER-Past Year" = "ER_Past_Year",
                                  "Physical/Sexual Abuse" = "Physical_Sexual_Abuse","Physically Disabled" = "Physical_Disability",
                                  "Mentally Ill" = "Mental_Illness","Current Alcohol Problem" = "Alcohol_Abuse",
                                  "Current Drug Problem"="Drug_Abuse","Drug or Alcohol History"="Drug_Alcohol_History",
                                  "HIV Positive" = "HIV_Positive","Part Time Employment" = "Part_Time",
                                  "Full Time Employment"= "Full_Time",
                                  "Unemployed - Looking" = "Unemployed_Looking","Unemployed - Not Looking" ="Unemployed_Not_Looking")),
       #Second Characteristic to subset data by
       selectInput("var2", label = "Population Characteristic 2",
                   choices = list("All"="All","Veteran" = "Veteran", "Persistenly Homeless" = "Chronic_Time",
                               "Chronic Health Condition" = "Chronic_Condition","Chronically Homeless" = "Chronic",
                               "First Time Homeless"="New_to_Homelessness",
                               "Living With Child" = "Adult_With_Child","Ex-Foster Care" = "Foster_Care_Involvement",
                               "Justice System-Past Year" = "Justice_System_Past_Year",
                               "ER-Past Year" = "ER_Past_Year",
                               "Physical/Sexual Abuse" = "Physical_Sexual_Abuse","Physically Disabled" = "Physical_Disability",
                               "Mentally Ill" = "Mental_Illness","Current Alcohol Problem" = "Alcohol_Abuse",
                               "Current Drug Problem"="Drug_Abuse","Drug or Alcohol History"="Drug_Alcohol_History",
                               "HIV Positive" = "HIV_Positive","Part Time Employment" = "Part_Time",
                               "Full Time Employment"= "Full_Time",
                              "Unemployed - Looking" = "Unemployed_Looking","Unemployed - Not Looking" ="Unemployed_Not_Looking")),
       #Choose Variable to stack bar plots
       br(), 
       tags$i(tags$b("Divide the bars on the plot by the characteristic below.")),
       br(), br(),
       radioButtons("stack", label = "Division Characteristic", choiceNames = c("No Division", "Gender", "Ethnicity", "Age"), 
                    choiceValues =  c("No Division", "Gender", "Ethnicity", "Age_Categorical"))
       
       ),
    
    
 # Main Panel
    mainPanel(
      tabsetPanel(
        tabPanel("Compare Year to Year Homeless Populations", 
          br(),br(),
            fluidPage(
              textOutput("warning"),
              br(),
              plotOutput("compare_years"),
                br(),br(), br(),
                selectInput("data", label = "Data Set 1",
                             choices = c("Unsheltered 2016",
                                            "Sheltered 2016",
                                            "Unsheltered 2017",
                                            "Sheltered 2017")),
                 selectInput("data2", label = "Data Set 2",
                             choices = c("Unsheltered 2016",
                                         "Sheltered 2016",
                                         "Unsheltered 2017",
                                         "Sheltered 2017"), selected = "Unsheltered 2017"), 
                 selectInput("response", label = "Characteristic to Plot (x-axis)", 
                             choices = list("Age" = "Age_Categorical",
                                            "Ethnicity" =  "Ethnicity",
                                            "Gender"    =  "Gender",
                                            "Length of Current Episode" ="Current_Stint_Duration",
                                            "Times Homeless-Past 3 Years" = "Times_Homeless_3yrs",
                                            "Location Type" = "Outside_Majority_Last_Month")))),
                                                       
        tabPanel("Reasons Given for Homelessness", br(),br(),
                 textOutput("warning2"),
                 br(),
                 plotOutput("reasons"),
                 br(),br(),br(),
                 selectInput("year", label = "Data Set",
                             choices = c("2016 Unsheltered","2017 Unsheltered"))),
        tabPanel("Employment Status", br(),br(),
                 textOutput("warning3"),
                 br(),
                 plotOutput("employment"),
                 br(),br(),br(),
                 selectInput("year2", label = "Data Set",
                             choices = c("2016 Unsheltered","2017 Unsheltered")))
        
      )
    )
  )
))

