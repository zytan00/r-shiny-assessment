library(shiny)
library(readxl)
library(DT)
library(ggplot2)
library(tidyr)


ui <- fluidPage(
  tabsetPanel(
    tabPanel("Data Input",
             sidebarLayout(
               sidebarPanel(
                 fileInput("file", "Data File", buttonLabel = "Upload"),
                 numericInput("tail_factor","Tail Factor",value = 1.1, min = 0.1, max = 10, step = 0.1)
               ),
               mainPanel(
                 DTOutput("uploaded_data")
               )
             )
    ),
    tabPanel("Results",
             mainPanel(
               DTOutput("cumulative_paid_claims_table"),
               plotOutput("cumulative_paid_claims_plot")
             )
    )
  )
)

server <- function(input, output, session) {
  
  claims <- reactive({                                      
    req(input$file)
    read_xlsx(input$file$datapath)
  })
  
  output$uploaded_data <- renderDT({                        
    req(claims())
    datatable(claims(), rownames = TRUE, options = list(
      dom = 't',
      paging = FALSE,
      rownames = TRUE,
      columnDefs = list(list(className = 'dt-center', targets = 0))
    )) %>%
      formatRound(names(claims())[3], 0) %>%  
      formatStyle(names(claims()), background = 'white') 
  })
  
  cumulative_data <- reactive({
    req(claims())
    claims <- as.matrix(claims())
    
    num_lossyr <- length(unique(claims[,1]))
    num_devyr <- length(unique(claims[,2]))
    
    cumulative <- matrix(0, nrow = num_lossyr, ncol = num_devyr+1,
                         dimnames = list(unique(claims[, 1]), c(1,2,3,4)))
    
    for (i in 1:num_lossyr){
      for (j in 1:(num_devyr+1)){
        cumulative[i,j] <- sum(claims[,3][claims[,1]==rownames(cumulative)[i] & claims[,2]<=colnames(cumulative)[j]])
        if (j == 2){
          cumulative[3,j] <- sum(cumulative[1,j], cumulative[2,j])/sum(cumulative[1,j-1],cumulative[2,j-1])*cumulative[3,j-1]
        }
        if (j == 3){
          cumulative[2,j] <- cumulative[2,j-1]*cumulative[1,j]/cumulative[1,j-1]
          cumulative[3,j] <- cumulative[3,j-1]*cumulative[1,j]/cumulative[1,j-1]
        }
        if (j == 4){
          cumulative[i,j] <- cumulative[i,j-1]*input$tail_factor
        }
      }
    }
    
    cumulative
  })
  
  output$cumulative_paid_claims_table <- renderDT({             
    req(claims())
    req(cumulative_data())
    
    cumulative_df <- as.data.frame(cumulative_data())
    
    sketch = htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          th(colspan = 1, ''),
          th(colspan = 4, 'Development Year',class = "dt-center")
        ),
        tr(
          lapply(c("Loss Year",1:4), th)
        )
      )
    ))
    
    datatable(cumulative_df, container = sketch, 
              caption = htmltools::tags$caption(
                style = 'caption-side: top; color: black; font-weight: bold; font-size: 15px;',
                "Cumulative Paid Claims ($) - Table"), rownames = TRUE, options = list(
                  dom = 't',
                  paging = FALSE,
                  rownames = TRUE,
                  columnDefs = list(list(className = 'dt-center', targets = 0))
                )) %>%
      formatRound(names(cumulative_df), 0) %>%
      formatStyle(names(cumulative_df), background = 'white')
  })
  
  output$cumulative_paid_claims_plot <- renderPlot({
    req(cumulative_data())
    
    data <- cumulative_data() 
    
    rownames_numeric <- as.numeric(rownames(data))
    colnames_numeric <- as.numeric(colnames(data))
    
    data_long <- reshape2::melt(data, varnames = c("Loss_Year", "Development_Year"))
    
    p <- ggplot(data = data_long, aes(x = Development_Year, y = value, color = as.factor(Loss_Year))) +  
      geom_line(linewidth = 1.5) +
      geom_point(size = 3, shape = 16) + 
      labs(x = "Development Year", y = "", 
           title = "Cumulative Paid Claims ($) - Graph", color = "Loss Year") +
      scale_color_manual(values = c("lightgreen", "orange", "lightblue"),
                         labels = c("2017", "2018", "2019"), name = NULL) +
      theme_minimal() + 
      theme(
        legend.position = "bottom",
        legend.text = element_text(size = 12),
        plot.title = element_text(hjust = 0, size = 15, face = "bold"),
        axis.text.y = element_text(size = 12, vjust = 0.5, hjust = 0.5),
        axis.text.x = element_text(size = 12, vjust = 0.5, hjust = 0.5),
        axis.title.x = element_text(size = 12),
        plot.margin = margin(50, 10, 20, 0)
      ) + 
      scale_y_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE), limits = c(500000, 1500000))
    
    p + 
      annotate(geom = "text", x = c(4, 3, 4, 2, 3, 4), y = c(data[1, 4], data[2, 3], data[2, 4], data[3, 2], data[3, 3], data[3, 4]),
               label = c(format(round(data[1, 4]), big.mark = ",", scientific = FALSE), 
                         format(round(data[2, 3]), big.mark = ",", scientific = FALSE), 
                         format(round(data[2, 4]), big.mark = ",", scientific = FALSE), 
                         format(round(data[3, 2]), big.mark = ",", scientific = FALSE), 
                         format(round(data[3, 3]), big.mark = ",", scientific = FALSE), 
                         format(round(data[3, 4]), big.mark = ",", scientific = FALSE)),
               vjust = -1, size = 4)
  })
}

shinyApp(ui, server)