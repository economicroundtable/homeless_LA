shinyServer(function(input, output) {
   
   #Text Warning
    output$warning <- renderText({
      if(input$gender != "All" | input$ethnicity != "All" | input$SPA != "All" | 
               input$var != "All" | input$var2 != "All" | 
               min(input$age) !=0 | max(input$age) != 100){ 
                  print("NOTE: You are currently plotting a subset of the data. To see the full population, select \"All\" for each subset option and set the age sliders to 0 and 100.", quote = FALSE)
      }
                
    })
   # Year to year comparisons
    output$compare_years <- renderPlot({
      #Choose Data 1
      if (input$data == "Unsheltered 2016") {
        tmp <- unsheltered_2016
      }
      if (input$data == "Unsheltered 2017") {
        tmp <- unsheltered_2017
      }
      if (input$data == "Sheltered 2016") {
        tmp <- sheltered_2016
      }
      if (input$data == "Sheltered 2017") {
        tmp <- sheltered_2017
      }
      #Choose Data 2
      
      if (input$data2 == "Unsheltered 2016") {
        tmp2 <- unsheltered_2016
      }
      if (input$data2 == "Unsheltered 2017") {
        tmp2 <- unsheltered_2017
      }
      if (input$data2 == "Sheltered 2016") {
        tmp2 <- sheltered_2016
      }
      if (input$data2 == "Sheltered 2017") {
        tmp2 <- sheltered_2017
      }
      
      #Filter based on the left panel
    
      # If gender != all, need to filter
      if (input$gender != "All") {
        tmp <- tmp %>%
          filter(Gender == input$gender)
      }
      if (input$gender != "All") {
        tmp2 <- tmp2 %>%
          filter(Gender == input$gender)
      }
      #If ethnicity != all, need to filter
      if(input$ethnicity != "All"){
        tmp <- tmp %>% filter(Ethnicity == input$ethnicity)
      }
      if(input$ethnicity != "All"){
        tmp2 <- tmp2 %>% filter(Ethnicity == input$ethnicity)
      }
      #If SPA != all, need to filter
      if(input$SPA != "All"){
        tmp <- tmp %>% filter(SPA == as.numeric(input$SPA)) 
      }
      if(input$SPA != "All"){
        tmp2 <- tmp2 %>% filter(SPA == as.numeric(input$SPA)) 
      }
      #If First characteristic != all, need to filter
      if(input$var != "All"){
        tmp <- tmp[tmp[,grepl(paste0("^",input$var,"$"), colnames(tmp))] == 1, ]
      }
      if(input$var != "All"){
        tmp2 <- tmp2[tmp2[,grepl(paste0("^",input$var,"$"), colnames(tmp2))] == 1, ]
      }
      #If second characteristic != all, need to filter
      if(input$var2 != "All"){
        tmp <- tmp[tmp[,grepl(paste0("^",input$var2,"$"), colnames(tmp))] == 1, ]
      }
      if(input$var2 != "All"){
        tmp2 <- tmp2[tmp2[,grepl(paste0("^",input$var2,"$"), colnames(tmp2))] == 1, ]
      }
      #Age filter
      tmp <- tmp %>% filter(Age >= input$age[1] & Age <= input$age[2])
      tmp2 <- tmp2 %>% filter(Age >= input$age[1] & Age <= input$age[2])
      
      #Bind based on the characteristic to plot selection
      
      tmp_bind <- cbind(rbind(tmp[, input$response], tmp2[,input$response]), c(tmp$Data, tmp2$Data), 
                        c(tmp$Weights, tmp2$Weights), c(tmp$Gender, tmp2$Gender), 
                        c(tmp$Ethnicity, tmp2$Ethnicity), c(as.character(tmp$Age_Categorical), as.character(tmp2$Age_Categorical))) 
      tmp_bind <- as.data.frame(tmp_bind)
      names(tmp_bind) <- c("input_data", "Data", "Weights","Gender", "Ethnicity", "Age_Categorical")
      ####################################
      #X axis title needs to be hard coded
      x_label <- ifelse(input$response=="Age_Categorical", "Age", 
                        ifelse(input$response=="Current_Stint_Duration","Length of Current Episode",
                               ifelse(input$response=="Times_Homeless_3yrs", "Times Homeless-Past 3 Years",
                                      ifelse(input$response=="Outside_Majority_Last_Month", "Location Type", 
                                             input$response))))
      
      #Plot output
      if(input$stack != "No Division"){
        #legend key needs to be hard coded
        legend_key <- ifelse(input$stack == "Gender", "Gender", 
                             ifelse(input$stack == "Ethnicity", "Ethnicity",
                                    ifelse(input$stack == "Age_Categorical", "Age")))

          ggplot(data = tmp_bind, aes(x= input_data)) +
          geom_bar(aes(fill = eval(parse(text = input$stack)), weight = Weights)) +
          facet_wrap(~Data) +
          labs(x = x_label, y = "Count") +
          scale_fill_manual(values=ERT_palette_2) +
          #guides(fill=guide_legend(nrow=2,byrow=TRUE)) + 
          theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 14, family = "Arial"), 
                axis.text.y = element_text(hjust = 1, size = 14, family = "Arial"),
                axis.title.x = element_text(size = 16, family = "Arial", face = "bold"),
                axis.title.y = element_text(size = 16, family = "Arial", face = "bold"),
                plot.title = element_text(hjust = 0.5, size = 18, family = "Arial",face = "bold"),
                legend.position="bottom",
                legend.title = element_blank(),
                legend.text = element_text(size = 14, family = "Arial"),
                strip.text.x = element_text(size = 16, family = "Arial", face = "bold"),
                panel.grid.major.x = element_blank(),
                panel.grid.major.y = element_line(size=.2, color="black"),
                panel.grid.minor.y = element_line(size=.1, color="black"),
                panel.background = element_rect(fill = NA))} else{
                  #Plot with no stacking
                  
                    ggplot(data = tmp_bind, aes(x=input_data, fill = Data)) +
                    geom_bar(alpha =.75, aes(weight = Weights)) +
                    facet_wrap(~Data) +
                    labs(x = x_label, y = "Count") +
                    scale_fill_manual(values=c("orange2","darkseagreen4")) +
                    theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 14, family = "Arial"), 
                          axis.text.y = element_text(hjust = 1, size = 14, family = "Arial"),
                          axis.title.x = element_text(size = 16, family = "Arial", face = "bold"),
                          axis.title.y = element_text(size = 16, family = "Arial", face = "bold"),
                          legend.position="none",
                          plot.title = element_text(hjust = 0.5, size = 18, family = "Arial",face = "bold"),
                          strip.text.x = element_text(size = 16, family = "Arial", face = "bold"),
                          panel.grid.major.x = element_blank(),
                          panel.grid.major.y = element_line(size=.2, color="black"),
                          panel.grid.minor.y = element_line(size=.1, color="black"),
                          panel.background = element_rect(fill = NA)) 
                }
        
        
        
    })
    
   
  
#Reasons for homelessness
    
#Text Warning
  output$warning2 <- renderText({
      if(input$gender != "All" | input$ethnicity != "All" | input$SPA != "All" | 
         input$var != "All" | input$var2 != "All" | 
         min(input$age) !=0 | max(input$age) != 100){ 
        print("NOTE: You are currently plotting a subset of the data. To see the full population, select \"All\" for each subset option and set the age sliders to 0 and 100.", quote = FALSE)
      }
      
    })
  output$reasons <- 
      renderPlot({
    #User picks dataset 
    #If year == 2016, use 2016 data
    if(input$year == "2016 Unsheltered"){
      tmp_unsheltered <- unsheltered_2016
    }
    #If year == 2017, use 2017 data
    if(input$year == "2017 Unsheltered"){
      tmp_unsheltered <- unsheltered_2017 
    }
    # If gender != all, need to filter
    if (input$gender != "All") {
      tmp_unsheltered <- tmp_unsheltered %>%
        filter(Gender == input$gender)
    }
    #If ethnicity != all, need to filter
    if(input$ethnicity != "All"){
      tmp_unsheltered <- tmp_unsheltered %>% 
        filter(Ethnicity == input$ethnicity)
    }
    #If SPA != all, need to filter
    if(input$SPA != "All"){
      tmp_unsheltered <- tmp_unsheltered %>% filter(SPA == as.numeric(input$SPA)) 
    }
    #If First characteristic != all, need to filter
    if(input$var != "All"){
      tmp_unsheltered <- tmp_unsheltered[tmp_unsheltered[,grepl(input$var, colnames(tmp_unsheltered))] == 1, ]
    }
    #If second characteristic != all, need to filter
    if(input$var2 != "All"){
      tmp_unsheltered <- tmp_unsheltered[tmp_unsheltered[,grepl(input$var2, colnames(tmp_unsheltered))] == 1, ]
    }
   
   tmp_unsheltered <- 
     tmp_unsheltered %>% 
      select(starts_with("Reason_"), Gender, Age, Ethnicity, Age_Categorical, Weights) %>% 
      gather(key = "Reason", value = "count", -Gender, -Age, -Ethnicity, -Age_Categorical, -Weights) %>% 
      mutate(Reason = recode(Reason, "Reason_Homeless_DV" = "Domestic violence", "Reason_Homeless_Mental" = "Mental illness",
                             "Reason_Homeless_Job" = "Loss of job", "Reason_Homeless_Child_Support" = "Child support",
                             "Reason_Homeless_Death_Illness_Fam" = "Death or illness in family",
                             "Reason_Homeless_Sexual_Orientation" = "Sexual orientation",
                             "Reason_Homeless_No_Fam" ="No family support", "Reason_Homeless_Released_Treatment"="Released from treatment center",
                             "Reason_Homeless_Safety"= "Safety concerns", 
                             "Reason_Previous_Uninhabitable_Housing" = "Previous housing was uninhabitable",
                             "Reason_Homeless_Subs_Abuse" = "Substance abuse", "Reason_Homeless_Medical" = "Medical expenses",
                             "Reason_Homeless_Breakup" = "Breakup", "Reason_Homeless_HH_Conflict" = "Household conflict",
                             "Reason_Homeless_Eviction" = "Eviction", "Reason_Homeless_Foster_Care" = "Left or aged out of foster care",
                             "Reason_Homeless_Immigration" = "Immigration", "Reason_Homeless_Jail_Prison" = "Released from jail or prison",
                             "Reason_Homeless_Previous_Housing_Program"="Timed out or left housing program",
                             "Reason_Homeless_Family_Hmls" = "Family is homeless")) %>%
      filter(count == 1) %>% 
      filter(Age >= input$age[1] & Age <= input$age[2]) %>% 
      select(-count) %>% 
      mutate(Reason = reorder(Reason, Weights, sum)) 
      
      #Plot output
   
      if(input$stack != "No Division"){
      #legend key
      legend_key <- ifelse(input$stack == "Gender", "Gender", 
                             ifelse(input$stack == "Ethnicity", "Ethnicity",
                                    ifelse(input$stack == "Age_Categorical", "Age")))
      #Plot 
      ggplot(data = tmp_unsheltered, aes(x = Reason)) +
      geom_bar(width=0.8, aes(fill = eval(parse(text = input$stack)), weight = Weights)) +
      coord_flip() +
      labs(x = "Reason", y = "Count", title =  paste0(input$year,":"," Reasons for Homelessness")) + 
      scale_fill_manual(values=ERT_palette) +
      theme(axis.text.x = element_text(hjust = 1, size = 14, family = "Arial"), 
            axis.text.y = element_text(hjust = 1, size = 14, family = "Arial"),
            axis.title.x = element_text(size = 16, family = "Arial", face = "bold"),
            axis.title.y = element_text(size = 16, family = "Arial", face = "bold"),
            plot.title = element_text(hjust = 0.5, vjust = 2, size = 18, family = "Arial", face = "bold"),
            legend.position="bottom",
            legend.title = element_blank(),
            legend.text = element_text(size = 14, family = "Arial"),
            panel.grid.major.y = element_blank(),
            panel.grid.major.x = element_line(size=.3, color="black"),
            panel.grid.minor.x = element_line(size=.2, color="black"),
            panel.background = element_rect(fill = NA)) +
     scale_y_continuous(expand = c(.01, 0)) +
     geom_blank(aes(y=1.1*..count..), stat="count") 
     } else{
       #Plot without stacking
       ggplot(data = tmp_unsheltered, aes(x = Reason)) +
         geom_bar(width=0.8, alpha = .75, fill = "indianred4", aes(weight = Weights)) +
         coord_flip() +
         labs(x = "Reason", y = "Count", title =  paste0(input$year,":"," Reasons for Homelessness")) +
         theme(axis.text.x = element_text(hjust = 1, size = 14, family = "Arial"), 
               axis.text.y = element_text(hjust = 1, size = 14, family = "Arial"),
               axis.title.x = element_text(size = 16, family = "Arial", face = "bold"),
               axis.title.y = element_text(size = 16, family = "Arial", face = "bold"),
               plot.title = element_text(hjust = 0.5, vjust = 2, size = 18, family = "Arial", face = "bold"),
               panel.grid.major.y = element_blank(),
               panel.grid.major.x = element_line(size=.3, color="black"),
               panel.grid.minor.x = element_line(size=.2, color="black"),
               panel.background = element_rect(fill = NA)) +
         scale_y_continuous(expand = c(.01, 0)) +
         geom_blank(aes(y=1.1*..count..), stat="count")
     } 
    
  })
  
  
#Employment Status
  
  #Text Warning
  output$warning3 <- renderText({
    if(input$gender != "All" | input$ethnicity != "All" | input$SPA != "All" | 
       input$var != "All" | input$var2 != "All" | 
       min(input$age) !=0 | max(input$age) != 100){ 
      print("NOTE: You are currently plotting a subset of the data. To see the full population, select \"All\" for each subset option and set the age sliders to 0 and 100.", quote = FALSE)
    }
    
  })
  
  output$employment <- 
    renderPlot({
      #User picks dataset 
      #If year == 2016, use 2016 data
      if(input$year2 == "2016 Unsheltered"){
        tmp_unsheltered <- unsheltered_2016
      }
      #If year == 2017, use 2017 data
      if(input$year2 == "2017 Unsheltered"){
        tmp_unsheltered <- unsheltered_2017 
      }
      # If gender != all, need to filter
      if (input$gender != "All") {
        tmp_unsheltered <- tmp_unsheltered %>%
          filter(Gender == input$gender)
      }
      #If ethnicity != all, need to filter
      if(input$ethnicity != "All"){
        tmp_unsheltered <- tmp_unsheltered %>% 
          filter(Ethnicity == input$ethnicity)
      }
      #If SPA != all, need to filter
      if(input$SPA != "All"){
        tmp_unsheltered <- tmp_unsheltered %>% filter(SPA == as.numeric(input$SPA)) 
      }
      #If First characteristic != all, need to filter
      if(input$var != "All"){
        tmp_unsheltered <- tmp_unsheltered[tmp_unsheltered[,grepl(input$var, colnames(tmp_unsheltered))] == 1, ]
      }
      #If second characteristic != all, need to filter
      if(input$var2 != "All"){
        tmp_unsheltered <- tmp_unsheltered[tmp_unsheltered[,grepl(input$var2, colnames(tmp_unsheltered))] == 1, ]
      }
      
      tmp_unsheltered <- 
        tmp_unsheltered %>% 
        select(starts_with("Job_"), Gender, Age, Ethnicity, Age_Categorical, Weights) %>% 
        gather(key = "Job", value = "count", -Gender, -Age, -Ethnicity, -Age_Categorical, -Weights) %>% 
        mutate(Job = recode(Job, "Job_Unemployed_Looking" = "Looking for work",
                                 "Job_Unemployed_Not_Looking" = "Not looking for work",
                                 "Job_Full_Time" = "Full time employment",
                                 "Job_Part_Time" = "Part time employment",
                                 "Job_Seasonal" = "Seasonal employment",
                                 "Job_Unable_To_Work_Disability" = "Disabled",
                                 "Job_Temp_Work" = "Temporary work",
                                 "Job_Retired" = "Retired",
                                 "Job_Student" = "Student",
                                 "Job_Panhandling" = "Panhandling",
                                 "Job_Recycling" = "Recycling",
                                 "Job_Day_Labor" = "Day labor",
                                 "Job_Performance" = "Performance",
                                 "Job_Sex" = "Sex work")) %>%
        filter(count == 1) %>% 
        filter(Age >= input$age[1] & Age <= input$age[2]) %>% 
        select(-count) %>% 
        mutate(Job = reorder(Job, Weights, sum)) 
      
      #Plot output
      
      if(input$stack != "No Division"){
        #legend key
        legend_key <- ifelse(input$stack == "Gender", "Gender", 
                             ifelse(input$stack == "Ethnicity", "Ethnicity",
                                    ifelse(input$stack == "Age_Categorical", "Age")))
        #Plot 
        ggplot(data = tmp_unsheltered, aes(x = Job)) +
          geom_bar(width=0.8, aes(fill = eval(parse(text = input$stack)), weight = Weights)) +
          coord_flip() +
          labs(x = "Employment Status", y = "Count", title =  paste0(input$year2,":"," Employment Status")) + 
          scale_fill_manual(values=ERT_palette) +
          theme(axis.text.x = element_text(hjust = 1, size = 14, family = "Arial"), 
                axis.text.y = element_text(hjust = 1, size = 14, family = "Arial"),
                axis.title.x = element_text(size = 16, family = "Arial", face = "bold"),
                axis.title.y = element_text(size = 16, family = "Arial", face = "bold"),
                plot.title = element_text(hjust = 0.5, vjust = 2, size = 18, family = "Arial", face = "bold"),
                legend.position="bottom",
                legend.title = element_blank(),
                legend.text = element_text(size = 14, family = "Arial"),
                panel.grid.major.y = element_blank(),
                panel.grid.major.x = element_line(size=.3, color="black"),
                panel.grid.minor.x = element_line(size=.2, color="black"),
                panel.background = element_rect(fill = NA)) +
          scale_y_continuous(expand = c(.01, 0)) +
          geom_blank(aes(y=1.1*..count..), stat="count") 
      } else{
        #Plot without stacking
        ggplot(data = tmp_unsheltered, aes(x = Job)) +
          geom_bar(width=0.8, alpha = .75, fill = "lightsteelblue3", aes(weight = Weights)) +
          coord_flip() +
          labs(x = "Employment Status", y = "Count", title =  paste0(input$year2,":"," Employment Status")) +
          theme(axis.text.x = element_text(hjust = 1, size = 14, family = "Arial"), 
                axis.text.y = element_text(hjust = 1, size = 14, family = "Arial"),
                axis.title.x = element_text(size = 16, family = "Arial", face = "bold"),
                axis.title.y = element_text(size = 16, family = "Arial", face = "bold"),
                plot.title = element_text(hjust = 0.5, vjust = 2, size = 18, family = "Arial", face = "bold"),
                panel.grid.major.y = element_blank(),
                panel.grid.major.x = element_line(size=.3, color="black"),
                panel.grid.minor.x = element_line(size=.2, color="black"),
                panel.background = element_rect(fill = NA)) +
          scale_y_continuous(expand = c(.01, 0)) +
          geom_blank(aes(y=1.1*..count..), stat="count")
      } 
      
    })
  

  
  
})
