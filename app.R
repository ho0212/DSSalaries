#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(tidyverse)
library(ggplot2)
library(GGally)
library(visdat)
library(naniar)
library(stats)
library(corrplot)
library(caret)
library(rpart)
library(rpart.plot)
library(reshape2)
library(DescTools)
library(nnet)
library(gganimate)
library(gifski)
library(networkD3)
library(ggalluvial)
library(ggridges)
library(rpart)
library(rpart.plot)
library(igraph)
library(magick)
library(circlize)
library(leaflet)
library(leaflet.extras2)
library(sf)
library(dplyr)
library(rnaturalearth)
library(htmlwidgets)
library(RColorBrewer)
library(geosphere)
library(plotly)
library(DT)


line_csv <- read_csv("line_chart.csv")
sankey_links_csv <- read_csv("sankey_links.csv")
sankey_nodes_csv <- read_csv("sankey_nodes.csv")
chord_csv <- read_csv("chord_diagram.csv")
choropleth_csv <- read_csv("choropleth_map.csv")
flow_map_csv <- read_csv("flow_map.csv")
flows <- gcIntermediate(flow_map_csv[, c("long_employee", "lat_employee")], 
                        flow_map_csv[, c("long_company", "lat_company")], 
                        sp = TRUE, addStartEnd = TRUE)
flows$counts <- flow_map_csv$count
flows$origins <- flow_map_csv$employee_residence
flows$destinations <- flow_map_csv$company_location
hover <- paste0(flows$origins, " to ", 
                flows$destinations, ': ', 
                as.character(flows$counts))
pal <- colorFactor(brewer.pal(4, 'Set2'), flows$origins)



countries <- ne_countries(scale = "medium", returnclass = "sf")
countries <- countries %>%
  mutate(company_location = tolower(name))
ridge_csv <- read_csv("ridge_chart.csv")
decision_csv <- read_csv("decision_tree.csv") %>% select(-...1)



# Define UI for application that draws a histogram
ui <- fixedPage(
  setBackgroundColor(
    color = c("#F0F4F8", "#D9E2EC"), # Light gradient colors
    gradient = "linear",
    direction = "bottom"
  ),
  tags$style(HTML("
    hr {
      border: 1px solid #000;
    }
  ")),
  h1(tags$b("Comprehensive Analysis of Data Science Salaries: 2020 ~ 2024"), align="center", style="font-family: 'Helvetica Neue'; font-size: 40px"),
  p("By Malone Ho, 10/10/2024", style="font-family: 'Helvetica'; color: grey; margin-left: 65px; font-size: 12px"),
  p("Recently, data science has become one of the most sought-after careers, with competitive salaries reflecting its high demand (Singh, 2024). 
  This website presents a detailed exploration of salary trends in the data science field, analysing various factors that influence earnings over the years. 
  Leveraging datasets from", tags$a(href = "https://www.kaggle.com/", "Kaggle"), "covering the period from 2020 to 2024, 
    This website is going to explore key factors such as experience level, employment type, location of company to understand their impact on salaries. 
    The goal of this website is to provide the answers for the three questions below.", 
    style="font-family: 'Helvetica'; font-size: 15px; text-align: center"),
  tags$ul(
    tags$li(tags$b("What is the major factor that affects the amount of salary most?")),
    tags$li(tags$b("How does the company location affect the amount of salary?")),
    tags$li(tags$b("How does the employee residence affect job location and salary?")),
    style="font-family: 'Helvetica'; font-size: 17px; list-style-position: inside; text-align: center"
  ),
  tags$hr(),
  p(HTML("According to the graph for the different levels of experience, the salary for the executive level is undoubtedly higher than other levels during the period of 2020 to 2024.
    The executive level has the highest salary, followed by the senior level, the middle level and the entry level, which has the lowest salary. Interestingly, the amount of salary for all levels increases steadily from 2020 to 2024, except for the executive level in 2020 to 2021. During 2020 to 2021, 
    the amount of the executive level experienced a significant drop from 180k to 160k."), style="font-family: 'Helvetica'; font-size: 15px; text-align: center"),
  
  p("According to the graph for the different remote types, the amount of salary for in-person type increases steadily from 2020 to 2024 and 
    the amount of salary for remote started increasing from 2021. However, the amount of salary for hybrid fluctuates over these years. The average salary for hybrid shows a sligh increase in 2024, following a drop to its lowest point in 2023.", style="font-family: 'Helvetica'; font-size: 15px; text-align: center"),
  
  p("According to the graph related to different types of employment, most of the employment types, with the exception of full-time, experience some fluctuation between 2020 and 2024. 
    Essentially, the average salary for full-time employees consistently grows during these 5 years. Despite those fluctuations, the average salaries of contractors and part-time employees raise slightly from 2020 to 2024.
    Only the average salary for freelancers experiences a steady decline in these 5 years.", style="font-family: 'Helvetica'; font-size: 15px; text-align: center"),
  p(tags$b("USER GUIDE"), style="font-family: 'Helvetica'; font-size: 15px; text-align: center"),
  tags$ul(
    tags$li("Use filter to show the variation of salary in different key factors"),
    tags$li("Hover on the line chart to reveal the detailed information for each time period"),
    style="font-family: 'Helvetica'; font-size: 15px; list-style-position: inside; text-align: center"
  ),
  fixedRow(
    column(2, selectInput("filter_line_chart", "Types", choices = c("Experiecne Level" = "experience_level",
                                                                    "Remote Type" = "remote_ratio",
                                                                    "Employment Type" = "employment_type"), selected = "Experiecne Level")),
    column(12, plotlyOutput("line_chart"))

  ),
  h3(tags$b("transparent"), style="font-family: 'Helvetica Neue'; font-size: 25px; text-align: center; color: rgba(0,0,0,0)"),
  fixedRow(
    column(6, h3(tags$b("Experience Level V.S. Salary"), style="font-family: 'Helvetica Neue'; font-size: 25px; text-align: center")),
    column(6, h3(tags$b("Experience Level + Employment Type V.S. Salary"), style="font-family: 'Helvetica Neue'; font-size: 25px; text-align: center"))
  ),
  fixedRow(
    column(6, p("The animation below illustrates the variation in salary at different levels of experience between 2020 to 2024. It is clear to see that the median for each box plot grows steadily. 
                Additionally, the gap between the highest and lowest point in each experience level except executive level become wider over these 5 years.
                The gap for executive level firstly narrows in 2021 and extends same as other levels between 2022 and 2024.", style="font-family: 'Helvetica'; font-size: 15px; text-align: center"),
           p("In terms of the number of employees at the same salary level, the majority part of entry-level employees are paid less than the median salary of all entry-level employees in 2020 and 2021.
             The salary for these employees has grown to around or above the median. The majority of employees at other experience levels are paid around the median as the widest areas in the violin plot are thinner than in the box plot.", style="font-family: 'Helvetica'; font-size: 15px; text-align: center"),
           h3(tags$b("123"), style="font-family: 'Helvetica Neue'; font-size: 25px; text-align: center; color: rgba(0,0,0,0)")),
    
    column(6, p("The sankey diagram shows that most of employees are full-time at various levels of experience, and more than half of full-time employees are paid between 100k and 200k. 
                Only a few people are paid more than 300k. Moreover, for those employees who do not work full-time, the salary is generally less than 100k. 
                Therefore, it is plain to see that those people who working as full-time can earn more money than others.", style="font-family: 'Helvetica'; font-size: 15px; text-align: center"),
           
           p("In terms of level of experience, more than half of the employees belong to the senior level and they work as full-time employees. 
                Furthermore, despite the majority of entry-level employees work as full-time, some of employees also work in different types of employment such as contractors or part-time.", style="font-family: 'Helvetica'; font-size: 15px; text-align: center"),
           
           p(tags$b("USER GUIDE")), style="font-family: 'Helvetica'; font-size: 15px; text-align: center"),
    tags$ul(
      tags$li("Hover on flows and nodes to reveal more information"),
      tags$li("Feel free to move the nodes to desired position"),
      style="font-family: 'Helvetica'; font-size: 15px; list-style-position: inside; text-align: center"
    )
  ),
  fixedRow(
    column(6, plotOutput("box_plot")),
    column(6, sankeyNetworkOutput("sankey_diagram"))
  ),
  h3(tags$b("transparent"), style="font-family: 'Helvetica Neue'; font-size: 25px; text-align: center; color: rgba(0,0,0,0)"),
  fixedRow(
    h3(tags$b("Company Location V.S. Employee Residence"), style="font-family: 'Helvetica Neue'; font-size: 25px; text-align: center")
  ),
  fixedRow(
    p("In the chord diagram, the flows show how employees move from home to work.The different colours on each country represent the destination of each flow. For instance, there is a green bar on the top of India, which represents that the destination of that flow is the United States.
      Overall, basically, the destination of each flow is the United Stated, which means that most employees move to the United States for their careers. However, there are some Americans move to Canada to work.", style="font-family: 'Helvetica'; font-size: 15px; text-align: center"),
    
    p("In the flow map, we can see that the thickest flow is the one from India to the United States, with 20 people. 
      With the exception of people moving to the United States and Canada, there are serval flows whose destination is Australia, which means that Australia is also a country that can offer a higher salary.", style="font-family: 'Helvetica'; font-size: 15px; text-align: center"),
    
    p("From the view point of different company location and employee residence, apparently working in the United Stated or Canada can earn more money that other countries.", style="font-family: 'Helvetica'; font-size: 15px; text-align: center"),
    
    p(tags$b("USER GUIDE"), style="font-family: 'Helvetica'; font-size: 15px; text-align: center"),
    tags$ul(
      tags$li("Please use the filter to select different ways to represent the flows"),
      tags$li("Chord diagram shows the clearer direction of each flow, whereas flow map illustrates more detailed number of each flow while hovering on it"),
      tags$li("The filter in the flow map can filter out some flows originating from some specific countries"),
      style="font-family: 'Helvetica'; font-size: 15px; list-style-position: inside; text-align: center"
    )
  ),
  fixedRow(
    column(2, selectInput("filter_diagram_map", "Display in", choices = c("Chord Diagram", "Flow Map"), selected = "Chord Diagram")),
    column(uiOutput("diagram_map"), width=10, offset=1.1)
  ),
  h3(tags$b("transparent"), style="font-family: 'Helvetica Neue'; font-size: 25px; text-align: center; color: rgba(0,0,0,0)"),
  fixedRow(
    h3(tags$b("Company Location V.S. Salary"), style="font-family: 'Helvetica Neue'; font-size: 25px; text-align: center")
  ),
  fixedRow(
    p("The choropleth map shows the average salary in different countries. The deeper the colour of the country, the higher average salary in that country. 
      The default ridgeline plot next to the map reveals the global salary distribution in various levels of experience.", style="font-family: 'Helvetica'; font-size: 15px; text-align: center"),
    p("A glance at the choropleth map illustrates that ", tags$b("the United States, Canada, Australia, Japan and some coutries in Europe")," have the highest salaries in the world. The average salary in ",tags$b("the United States")," is around 150k which is the highest average salary globally.
      In contrast, the lowest average salary in the dataset is ",tags$b("Turkey"),", which average salary is less than 35k.", style="font-family: 'Helvetica'; font-size: 15px; text-align: center"),
    p("The default ridgeline plot illustrates the global average salary distribution at different levels of experience. It is clear to see that the shape for each experience level is slightly right skewed, representing that more than half of employees in each experience level are paid less than the average.
      The higher the experience level, the greater the right skewed. Unsurprisingly, the salary distribution in the United States and Canada is similar to the global salary distribution as most of the employees in this dataset work in these two countries.", style="font-family: 'Helvetica'; font-size: 15px; text-align: center"),
    p(tags$b("USER GUIDE"), style="font-family: 'Helvetica'; font-size: 15px; text-align: center"),
    tags$ul(
      tags$li("Please use the filter to select the number of countries shown in the map"),
      tags$li("Hover on coutries to see the exact average salary in each country"),
      tags$li("Click on any country to see the detailed salary distribution for different levels of experience in the specific country"),
      style="font-family: 'Helvetica'; font-size: 15px; list-style-position: inside; text-align: center"
    )
  ),
  fixedRow(
    column(4, selectInput("filter_linked_map", "The Number of Countries", choices = c("All", "Top 10", "Top 5"), selected = "All"))
  ),
  fixedRow(
    column(6, leafletOutput("choropleth")),
    column(6, plotOutput("ridge"))
  ),
  h3(tags$b("transparent"), style="font-family: 'Helvetica Neue'; font-size: 25px; text-align: center; color: rgba(0,0,0,0)"),
  fixedRow(
    h3(tags$b("Comprehensive Analysis"), style="font-family: 'Helvetica Neue'; font-size: 25px; text-align: center")
  ),
  fixedRow(
    p("The correlation matrix presents the relationship between each pair of variables in the dataset. 
      As the salary is the most vital factor in this analysis, we only need to focus on the highlighted row, which is the row showing the relationship between salary and other variables.
      The higher the number, the more it relates to the amount of salary. 
      According to the findings in the previous sections, experience level and company location are the most significant factors which could affect the amount of salary. 
      And according to the correlation matrix, these two factors do have the highest correlation coefficients, which are 0.3 and 0.17 respectively. 
      The reason why that the correlation coefficient of the employee residence is also higher than others is that only around 300 people live in the country other than the company location.", style="font-family: 'Helvetica'; font-size: 15px; text-align: center"),
    p("For the final section of this analysis, the decision tree examinaes how the model uses multiple factors to identify the salary interval. This decision tree only uses company location, experience level and remote type to identify the salary interval.
      And these three factors are the most related factors to the salary based on the correlation matrix.", style="font-family: 'Helvetica'; font-size: 15px; text-align: center")
  ),
  fixedRow(
    column(6, plotOutput("correlation")),
    column(6, plotOutput("decision_tree"))
  ),
  fixedRow(
    h3(tags$b("transparent"), style="font-family: 'Helvetica Neue'; font-size: 25px; text-align: center; color: rgba(0,0,0,0)")
  ),
  h3(tags$b("transparent"), style="font-family: 'Helvetica Neue'; font-size: 25px; text-align: center; color: rgba(0,0,0,0)"),
  fixedRow(
    h3(tags$b("Conclusion"), style="font-family: 'Helvetica Neue'; font-size: 25px; text-align: center")
  ),
  fixedRow(
    p("Based on the above analysis, the major factors that affect the amount of salary are level of experience and the location of the company. For those employees who work as full-time can also earn more than employees work as other types of employment.
      In terms of company location, ",tags$b("the United States "),"and",tags$b("Canada"),"offer a higher amount of salary than other countries in the world.
      As only approximately 300 people work in overseas, essentially the employee residence and company location are identical.", style="font-family: 'Helvetica'; font-size: 15px; text-align: center"),
    p("In conclusion, ",tags$b("company location and level of experience")," are the most important factors that can affect the amount of salary, 
      even though other factors besides company location and level of experience can still affect it.", style="font-family: 'Helvetica'; font-size: 15px; text-align: center")
  ),
  fixedRow(
    h3(tags$b("Data Source"), style="font-family: 'Helvetica Neue'; font-size: 25px; text-align: center")
  ),
  fixedRow(
    tags$ul(
      tags$li(tags$b(tags$a("Jobs and Salaries in Data Science", href = "https://www.kaggle.com/datasets/hummaamqaasim/jobs-in-data/data")),
              tags$p("This tabular data contains 9355 entries with 12 columns. This dataset has various sorts of text data that might affect the amount of salary. 
                     These 12 columns include both text and spatial data (the location of company and employee residence).")),
      tags$li(tags$b(tags$a("Data Jobs Salaries", href = "https://www.kaggle.com/datasets/lorenzovzquez/data-jobs-salaries?resource=download")),
              tags$p("This tabular data contains 29.6k entries with 11 columns. Basically, all of columns are same as the previous dataset which are all potentially factor affecting the amount of salary.")),
      tags$li(tags$b(tags$a("Global AI, ML, Data Science Salary 2023", href = "https://www.kaggle.com/datasets/dparas01/global-ai-ml-data-science-salary")),
              tags$p("This tabular data contains 8805 entries with 11 columns. Similarly, all columns are quite identical to the first dataset. 
                     This dataset provides more data which locations are not United States.")),
      style="font-family: 'Helvetica'; font-size: 17px; list-style-position: inside; text-align: center"
    )
  )
  
  
  
  

  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  # line chart
  line_chart <- eventReactive({
    input$filter_line_chart
  },{
    if (input$filter_line_chart == "experience_level"){
      line_df <- line_csv %>% select(experience_level, work_year, salary_in_usd) %>%
        group_by(experience_level, work_year) %>% summarise(avg=mean(salary_in_usd), .groups = "drop") %>%
        mutate(tooltip_text = paste("Year:", work_year, "<br>Avg Salary: $", round(avg, 2), "<br>Experience Level:", experience_level))
      
      p <- ggplot(line_df, aes(work_year, avg, group = experience_level, col=experience_level, text = tooltip_text)) + geom_line() +
        labs( x="Year", y= "Average Salary in USD") + ggtitle("Average Salary in Different Experience Levels (2020 ~ 2024)") + 
        guides(col=guide_legend(title = "Experience Levels")) +
        theme(plot.title = element_text(face = "bold", hjust=.5, size = 17), 
              axis.title.x = element_text(face="bold", size =13), 
              axis.title.y = element_text(face="bold", size =13),
              legend.title = element_text(face="bold", size =13),
              legend.text = element_text(size =10))
      p_plotly <- ggplotly(p, tooltip = "text") %>%
        layout(hoverlabel = list(bgcolor = "white", font = list(color = "black", size = 12)),
               hovermode = "closest",
               hovertemplate = "<b>Year:</b> %{x}<br><b>Avg Salary:</b> $%{y:.2f}<br><b>Experience Level:</b> %{text}<extra></extra>")
      
      
    } else if (input$filter_line_chart == "remote_ratio"){
      line_df <- line_csv %>% select(remote_ratio, work_year, salary_in_usd) %>%
        group_by(remote_ratio, work_year) %>% summarise(avg=mean(salary_in_usd), .groups = "drop") %>%
        mutate(tooltip_text = paste("Year:", work_year, "<br>Avg Salary: $", round(avg, 2), "<br>Remote Type:", remote_ratio))
      
      p <- ggplot(line_df, aes(work_year, avg, group = remote_ratio, col=remote_ratio, text = tooltip_text)) + geom_line() +
        labs( x="Year", y= "Average Salary in USD") + ggtitle("Average Salary in Different Remote Types (2020 ~ 2024)") + 
        guides(col=guide_legend(title = "Remote Types")) +
        theme(plot.title = element_text(face = "bold", hjust=.5, size = 17), 
              axis.title.x = element_text(face="bold", size =13), 
              axis.title.y = element_text(face="bold", size =13),
              legend.title = element_text(face="bold", size =13),
              legend.text = element_text(size =10))
      
      p_plotly <- ggplotly(p, tooltip = "text") %>%
        layout(hoverlabel = list(bgcolor = "white", font = list(color = "black", size = 12)),
               hovermode = "closest",
               hovertemplate = "<b>Year:</b> %{x}<br><b>Avg Salary:</b> $%{y:.2f}<br><b>Remote Type:</b> %{text}<extra></extra>")
    } else {
      line_df <- line_csv %>% select(employment_type, work_year, salary_in_usd) %>%
        group_by(employment_type, work_year) %>% summarise(avg=mean(salary_in_usd), .groups = "drop") %>%
        mutate(tooltip_text = paste("Year:", work_year, "<br>Avg Salary: $", round(avg, 2), "<br>Employment Types:", employment_type))
      
      p <- ggplot(line_df, aes(work_year, avg, group = employment_type, col=employment_type, text = tooltip_text)) + geom_line() +
        labs( x="Year", y= "Average Salary in USD") + ggtitle("Average Salary in Different Employment Types (2020 ~ 2024)") + 
        guides(col=guide_legend(title = "Employment Types")) +
        theme(plot.title = element_text(face = "bold", hjust=.5, size = 17), 
              axis.title.x = element_text(face="bold", size =13), 
              axis.title.y = element_text(face="bold", size =13),
              legend.title = element_text(face="bold", size =13),
              legend.text = element_text(size =10)) 
      
      p_plotly <- ggplotly(p, tooltip = "text") %>%
        layout(hoverlabel = list(bgcolor = "white", font = list(color = "black", size = 12)),
               hovermode = "closest",
               hovertemplate = "<b>Year:</b> %{x}<br><b>Avg Salary:</b> $%{y:.2f}<br><b>Employment Types:</b> %{text}<extra></extra>")
    }
    p_plotly
  })
  
  output$line_chart <- renderPlotly({
    line_chart()
  })
  
  # box & violin plot
  # the code below is the way to render the box & violin plot to animation
    # data <- read_csv("Full_data.csv")
    # p <- data %>% ggplot(aes(x=experience_level, y=salary_in_usd, fill=experience_level)) + geom_violin() + geom_boxplot(outlier.shape = NA)
    # 
    # ani <- p + transition_time(work_year) + labs(x="Experience Level", y="Salary in USD") + 
    #   ggtitle("{round(frame_time)} Salary Distribution Across Different Experience Levels") + 
    #   guides(fill = guide_legend(title = "Experience Levels")) + 
    #   theme(plot.title = element_text(face = "bold", hjust=.5, size = 17),
    #         axis.title.x = element_text(face="bold", size =15),
    #         axis.title.y = element_text(face="bold", size =15),
    #         legend.title = element_text(face="bold", size =13),
    #         legend.text = element_text(size =12))
    # 
    # gif <- animate(ani, nframes = 50, width = 600, height = 400, renderer = gifski_renderer())
  output$box_plot <- renderImage({
    list(src = "box&violin_chart.gif", contentType = "image/gif")
  }, deleteFile = F)
  
  # Sankey Diagram
  # the code below is how the data render for this sankey diagram
  #   data <- read_csv("Full_data.csv")
  #   experience_levels <- unique(data$experience_level)
  #   employment_types <- unique(data$employment_type)
  #   salary_intervals <- unique(data$salary_interval)
  #   
  #   # Combine all nodes
  #   all_nodes <- c(experience_levels, employment_types, salary_intervals)
  #   
  #   # Create mappings from node names to indices
  #   node_map <- data.frame(name = all_nodes, id = 0:(length(all_nodes) - 1))
  #   
  #   # Create links (edges) between nodes
  #   # First from experience_level to employment_type
  #   exp_to_emp <- data %>%
  #     group_by(experience_level, employment_type) %>%
  #     summarize(value = n()) %>%
  #     ungroup()
  #   
  #   # Then from employment_type to salary_interval
  #   emp_to_salary <- data %>%
  #     group_by(employment_type, salary_interval) %>%
  #     summarize(value = n()) %>%
  #     ungroup()
  #   
  #   # Prepare links for sankey
  #   links <- rbind(
  #     data.frame(
  #       source = match(exp_to_emp$experience_level, all_nodes) - 1,
  #       target = match(exp_to_emp$employment_type, all_nodes) - 1,
  #       value = exp_to_emp$value
  #     ),
  #     data.frame(
  #       source = match(emp_to_salary$employment_type, all_nodes) - 1,
  #       target = match(emp_to_salary$salary_interval, all_nodes) - 1,
  #       value = emp_to_salary$value
  #     )
  #   )
  #   write.csv(links, "sankey_links.csv")
  output$sankey_diagram <- renderSankeyNetwork({
    sankey <- sankeyNetwork(Links = sankey_links_csv, Nodes = sankey_nodes_csv,
                            Source = "source", Target = "target", Value = "value",
                            NodeID = "name", units = "Peoples",
                            fontSize = 12, nodeWidth = 30)
    
    onRender(sankey, "
      function(el, x) {
        d3.select(el).select('svg')
          .append('text')
          .attr('x', el.offsetWidth / 2)
          .attr('y', 15)
          .attr('text-anchor', 'middle')
          .style('font-size', '15px')
          .style('font-weight', 'bold')
          .text('Distribution of Employment Type, Experience, and Salary');
      }
                      ")
  })
  
  # Chord Diagram or Flow Map
  output$diagram_map <- renderUI({
    if (input$filter_diagram_map == "Chord Diagram"){
      plotOutput("chord", height = "600px", width = "800px")
    } else {
      renderLeaflet({
        leaflet() %>% setView(lng=0, lat=50, zoom=1.5) %>%
          addProviderTiles('CartoDB.Positron') %>%
          addPolylines(data = flows, weight = ~counts, label = hover, 
                       group = ~origins, color = ~pal(origins)) %>% 
          addArrowhead(data = flows, weight = ~counts, label = hover, 
                       group = ~origins, color = ~pal(origins)) %>%
          addLayersControl(overlayGroups = unique(flows$origins), 
                           options = layersControlOptions(collapsed = FALSE))
      })
    }
  })
  
  output$chord <- renderPlot({
    set.seed(2)
    adjacencyData <- with(chord_csv, table(employee_residence, company_location))
    adjacencyData[adjacencyData <= 5] <- 0
    
    chordDiagram(adjacencyData, transparency = 0.5, directional = 1)
    title(main = "Relation Between Company Location and Employee Residence", cex.main = 1.25)
  })
  
  
  # Choropleth Map
  cho_df <- eventReactive({
    input$filter_linked_map
  },{
    if (input$filter_linked_map == "All"){
      choropleth_df <- choropleth_csv
    } else if (input$filter_linked_map == "Top 10"){
      choropleth_df <- choropleth_csv %>% arrange(desc(avg)) %>% head(10)
    } else if (input$filter_linked_map == "Top 5"){
      choropleth_df <- choropleth_csv %>% arrange(desc(avg)) %>% head(5)
    }
    countries <- ne_countries(scale = "medium", returnclass = "sf")
    countries <- countries %>%
      mutate(company_location = tolower(name))
    countries <- countries %>%
      right_join(choropleth_df, by = "company_location")
    
  })
  
  output$choropleth <- renderLeaflet({
    leaflet(cho_df()) %>% setView(lng=-10, lat=30, zoom=1.5) %>%
      addProviderTiles("CartoDB.VoyagerLabelsUnder") %>%
      addPolygons(
        fillColor = ~colorQuantile("YlOrRd", avg)(avg),
        weight = 1,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          weight = 2,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        label = ~sprintf("<div style='font-size: 11px;'>Country: %s<br>Avg. Salary: $%s", name, avg) %>% lapply(HTML),
        layerId = ~name  # Ensure the layerId is set to the country name
      ) %>%
      addLegend(pal = colorQuantile("YlOrRd", countries$avg), values = ~avg, opacity = 0.7, title = "Average Salary", position = "bottomright")
  })
  
  output$ridge <- renderPlot({
    ggplot(ridge_csv, aes(x = salary_in_usd, y = experience_level, fill = experience_level)) +
      geom_density_ridges() + ggtitle("Global Average Salary by Experience Level (2020–2024)") +
      labs(x = "Salary in USD", y = "Experience Level") + guides(col=guide_legend(title = "Experience Level")) +
      theme(plot.title = element_text(face = "bold", hjust=.5, size = 15),
            axis.title.x = element_text(face="bold", size =15),
            axis.title.y = element_text(face="bold", size =15),
            legend.title = element_text(face="bold", size =15),
            legend.text = element_text(size =12))
  })

  observeEvent(input$choropleth_shape_click, {
    country <- input$choropleth_shape_click$id
    select_country <- ridge_csv %>% filter(company_location == country)

    output$ridge <- renderPlot({
      ggplot(select_country, aes(x = salary_in_usd, y = experience_level, fill = experience_level)) +
        geom_density_ridges() +
        labs(x = "Salary in USD", y = "Experience Level") + 
        ggtitle(paste("Salary Distribution in", country, "(2020–2024)")) + guides(col=guide_legend(title = "Experience Level")) +
        theme(plot.title = element_text(face = "bold", hjust=.5, size = 15),
              axis.title.x = element_text(face="bold", size =15),
              axis.title.y = element_text(face="bold", size =15),
              legend.title = element_text(face="bold", size =15),
              legend.text = element_text(size =12))
    })
  })
  
  # Correlation Matrix
  # the code below is the way to render the correlation to animation
  # data <- read_csv("Full_data.csv")
  # data <- select(data, -salary_interval)
  # data$experience_level <- as.numeric(as.factor(data$experience_level))
  # data$employment_type <- as.numeric(as.factor(data$employment_type))
  # data$employee_residence <- as.numeric(as.factor(data$employee_residence))
  # data$remote_ratio <- as.numeric(as.factor(data$remote_ratio))
  # data$company_location <- as.numeric(as.factor(data$company_location))
  # data$company_size <- as.numeric(as.factor(data$company_size))
  # colnames(data) <- c("Year", "Experience Level", "Employment Type", "Salary", "Employee Residence", "Remote Type", "Company Location", "Company Size")
  # corr <- cor(data)
  # corr <- as.data.frame(as.table(corr))
  # 
  # p <- ggplot(corr, aes(Var1, Var2, fill=Freq)) + geom_tile(col="white") + geom_text(aes(label=round(Freq,2)), col="black") + scale_fill_gradient(low="white", high="blue") + 
  #   labs(x="Attributes", y="Attributes") + ggtitle("Correlation Matrix") + theme_minimal() +
  #   geom_tile(data = filter(corr, Var2=="Salary"), color = "red", size = 1.5, fill = NA) +
  #   theme(plot.title = element_text(face = "bold", hjust=.5, size = 15),
  #         axis.title.x = element_text(face="bold", size =15),
  #         axis.title.y = element_text(face="bold", size =15)) + guides(fill=guide_legend(title = "Frequency"))
  # 
  # # Create an animation
  # p_anim <- p + transition_states(Var2, transition_length = 2, state_length = 1) +
  #   enter_fade() + exit_fade()
  # 
  # # Render the animation
  # gif <- animate(p_anim, nframes = 50, fps = 5)
  # gif
  # anim_save("correlation_matrix.gif")
  # 
  # img <- image_read("correlation_matrix.gif")
  # print(img)
  
  output$correlation <- renderImage({
    list(src = "correlation_matrix.gif", contentType = "image/gif")
  }, deleteFile = F)
  
  output$decision_tree <- renderPlot({
    tree <- rpart(salary_interval ~ ., data =decision_csv, method="class", cp=0.001)
    rpart.plot(tree, main="Decision Tree for Comprehensive Analysis")
  })




  

}

# Run the application 
shinyApp(ui = ui, server = server)
