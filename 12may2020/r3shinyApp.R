library("shiny")
library("threejs")

data("LeMis")
#----------------------------------------------------------------------------------------------------
ui <- shinyUI(fluidPage(

  titlePanel("Shiny three.js graph example"),

  sidebarLayout(
    sidebarPanel(
      selectInput("select", label = h3("Vertex shape"),
                  choices = list("Spheres" = "o", "Circles" = "@", "Labels" = "Labels"), selected = 1),
      p("Use the mouse zoom to rotate, zoom, and pan.")
    ),
    mainPanel(
        scatterplotThreeOutput("graph")
    )
  )
))

#----------------------------------------------------------------------------------------------------
server <- function(input, output, session)
{
  output$graph <- renderScatterplotThree({
    v <- input$select
    if (v == "Labels") v <- V(LeMis)$label
    graphjs(LeMis, vertex.shape=v)
  })
}
#----------------------------------------------------------------------------------------------------
browseURL("http://localhost:6799")
runApp(shinyApp(ui=ui,server=server), port=6799)
