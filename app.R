library(shiny)
library (plotly)
library (data.table)
set.seed (13920)

###### UI
ui <- fluidPage(
    
    # Title
    titlePanel("Iris clustering"),
    
    # The page will have a sidebar layout
    sidebarLayout(
        
        # In the sidebar the user can select the number of clusters and the axis from the
        # name of the features of iris
        sidebarPanel(
            numericInput (inputId = "clusters",
                          label = "Number of clusters:",
                          min = 2,
                          max = 10,
                          value = 3,
                          step = 1),
            selectInput ("xAxis", "X Axis: ", names (iris)[-5], selected = names (iris)[1]),
            selectInput ("yAxis", "Y Axis: ", names (iris)[-5], selected = names (iris)[3])
            # submitButton(text = "Apply values")
        ),
        
        # The main pain will show the plot, the title of the table, and the table of the selected
        # points.
        # Within the plotOutput will show the clustPlot plot and points will be selected.
        mainPanel(
            plotOutput ("clustPlot", brush = brushOpts (id = "brush")),
            h4 (textOutput ("Selected")),
            tableOutput ("brushedpoints")
        )
    )
)

# Create the function for plotting
plotting <- function (k, xax, yax) {
    # Plotting function using the iris dataset
    # It divides the cases into k groups and plot them in xax and yax axis
    #
    # Args:
    # k    Number of clusters
    # xax  X axis to plot
    # yax  Y axis to plot
    #
    clusteringResult <- kmeans (iris [, -5], centers = k, nstart = 3)
    
    clustIris <- cbind (iris, clus = clusteringResult$cluster)
    
    centroids <- as.data.frame (clusteringResult$centers)
    
    for (i in 1:k) {
        centroids$cluster[i] <- i 
    }
    
    gp <- ggplot (data = clustIris, 
                  aes (x = clustIris [, xax], y = clustIris [, yax])) +
        geom_point (aes (color = as.factor (clus)) ) +
        geom_point (data = centroids,
                    aes (x = centroids [, xax], y = centroids [, yax], color = factor (cluster)),
                    shape = 4, size = 20) +
        theme (plot.title = element_text (size=22)) +
        labs (title = "Clustering of the Iris data set",
              x = xax,
              y = yax,
              color = "Clusters")
    
    return (gp)
}

###### SERVER
server <- function (input, output) {
    
    # Make the function reactive
    reactPlotting <- reactive ({ plotting (input$clusters, input$xAxis, input$yAxis) })
    
    # The clustering is calculated and show where the "clusPlot" is place in the UI
    output$clustPlot <- renderPlot({
        reactPlotting ()
    })
    
    # Here the table from the brushed point is created and it will appear where the UI says.
    output$brushedpoints <- renderTable ({
        x <- input$xAxis
        y <- input$yAxis
        tab <- brushedPoints (iris, input$brush, x, y)
        if (nrow (tab) == 0)
            return()
        tab
    })
    
    # The text of the table is created and assigned to the variable "Selected".
    output$Selected <- renderText ("Selected Points")
}

###### RUN
shinyApp(ui = ui, server = server)

