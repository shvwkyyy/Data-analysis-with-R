library("dplyr")
library("arules")
library("shiny")
library("shinythemes")
library("ggplot2")

ui <- fluidPage(
  theme = shinytheme("darkly"), 
  titlePanel("Team 27"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file","Dataset Path",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      sliderInput("slider", "Choose the number of rows:", 1, 10000, 100),
      sliderInput("clusters", "Number of clusters:", 2, 4, 1),
      sliderInput("min_support", "Minimum Support", min = 0.001, max = 1, value = 0.001, step = 0.001),
      sliderInput("min_confidence", "Minimum Confidence", min = 0.001, max = 1, value = 0.001, step = 0.001)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Table",
                 tableOutput("contents"),
                 verbatimTextOutput("data_structure")
        ),
        tabPanel("Visualization" ,
                 plotOutput("pie_chart"),
                 plotOutput("scatter_plot"),
                 plotOutput("bar_plot"),
                 plotOutput("box_plot")),
        tabPanel("Kmeans",tableOutput("kmeans_table"),plotOutput("kmeans_plot")),
        tabPanel("Association Rule",
                 verbatimTextOutput("association_output"),
                 plotOutput("item_frequency_plot")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  
  clean_data <- reactive({
    req(input$file) 
    dataa <- read.csv(input$file$datapath)
    dataa <- distinct(dataa)
    dataa <- na.omit(dataa)
    return(dataa)
  })
  
  
  output$contents <- renderTable({
    finalData <- clean_data()
    head(finalData, input$slider)
  })
  
  
  output$data_structure <- renderPrint({
    finalData <- clean_data()
    print("The Data is cleaned Successfully")
    print("there is no duplicates")
    print("There is no missing values")
      
    str(finalData)
  })
  

  output$pie_chart <- renderPlot({
    finalData <- clean_data()
    x <- table(finalData$paymentType)
    percentage <-sprintf("%.2f%%",round(100 * x / sum(x)))
    payment_data <- data.frame(payment_type = names(x), count = x)
    piep <- ggplot(payment_data, aes(x = "", y = x, fill = payment_type)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar(theta = "y") +
      labs(title = "Compare Payment Types") +
      theme_minimal() +
      scale_fill_manual(values = c("purple", "pink")) +
      geom_text(aes(label = paste0(percentage, "%")), position = position_stack(vjust = 0.5)) +
      guides(fill = guide_legend(title = "Payment Type", reverse = TRUE))
    print(piep)
  })
  
  output$scatter_plot <- renderPlot({
    finalData <- clean_data()
    new_data <- finalData %>% 
      group_by(age) %>%
      summarise(TotalSpending = sum(total))
    scatterp <- ggplot(new_data, aes(x = age, y = TotalSpending)) +
      geom_point(color = "blue") +
      labs(title = "Age vs Total Spending", x = "Age", y = "Total Spending") +
      xlim(22, 65)
    print(scatterp)
  })
  
  
  output$bar_plot <- renderPlot({
    finalData <- clean_data()
    new_data1 <- finalData %>% 
      group_by(city) %>%
      summarise(TotalSpending1 = sum(total))  
    ordered_plot <- ggplot(new_data1, aes(x = reorder(city, -TotalSpending1), y = TotalSpending1)) +
      geom_bar(stat = "identity", fill = "pink") +
      geom_text(aes(label = TotalSpending1), vjust = -0.5, color = "black", size = 3) + 
      labs(title = "Compare Cities Total Spending", x = "City", y = "Total Spending") +
      scale_y_continuous(labels = scales::label_number()) + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    print(ordered_plot)
  })
  

  output$box_plot <- renderPlot({
    finalData <- clean_data()
    boxb <- ggplot(finalData, aes(x = "", y = total)) +
      geom_boxplot(fill = "lightgreen") +
      labs(title = "Distribution of Total Spending", x = NULL, y = "Total Spending")
    print(boxb)
  })
  

  output$kmeans_table <- renderTable({
    finalData <- clean_data()
    Grouping <- finalData %>%
      group_by(customer, age) %>%
      summarise(TotalSpending = sum(total))
    kmeans_result <- kmeans(Grouping[, c("age", "TotalSpending")], centers = input$clusters)
    Grouping$cluster <- kmeans_result$cluster
    return(Grouping)
  })
  
 
  output$kmeans_plot <- renderPlot({
    finalData <- clean_data()
    Grouping <- finalData %>%
      group_by(customer, age) %>%
      summarise(TotalSpending = sum(total))
    kmeans_result <- kmeans(Grouping[, c("age", "TotalSpending")], centers = input$clusters)
    Grouping$cluster <- kmeans_result$cluster
    plot(Grouping$age, Grouping$TotalSpending, col = Grouping$cluster,
         main = "K-means Clustering", xlab = "Age", ylab = "Total Spending")
    points(kmeans_result$centers, col = 1:input$clusters, pch = 8, cex = 2)
  })
  

  output$association_output <- renderPrint({
    finalData <- clean_data()
    x <- finalData$items
    y <- read.transactions(textConnection(x), sep = ",")
    apriori_rules <- apriori(y, parameter = list(supp = input$min_support, conf = input$min_confidence, minlen = 2))
    inspect(apriori_rules)
  })
  
  
  output$item_frequency_plot <- renderPlot({
    finalData <- clean_data()
    x <- finalData$items
    y <- read.transactions(textConnection(x), sep = ",")
    itemFrequencyPlot(y, topN = 5, type = "absolute")
  })
}

shinyApp(ui = ui, server = server)