library(shiny)
library(cyjShiny)
library(threejs)
library(later)
#----------------------------------------------------------------------------------------------------
printf <- function(...) print (noquote (sprintf (...)))
#----------------------------------------------------------------------------------------------------
# one way to create a graph is via the Bioconductor graphNEL class.
# here we use the data.frame strategy.
#----------------------------------------------------------------------------------------------------
load("nodesAndEdges.RData")
dim(tbl.nodes)
dim(tbl.edges)
tbl.customNodePositions <- get(load("layout.RData"))
g.json.string <- cyjShiny::dataFramesToJSON(tbl.edges, tbl.nodes)
ig <- graph_from_data_frame(tbl.edges[, c("source", "target", "interaction")], directed = TRUE,
                            vertices=tbl.nodes)
#----------------------------------------------------------------------------------------------------
button.style <- "padding:4px; font-size:90%"
defaultLayoutStrategy <- "cola"

#----------------------------------------------------------------------------------------------------
ui = shinyUI(fluidPage(

  tags$head(
     tags$style("#cyjShiny{height:90vh !important;}"),
     tags$style("#graph3d{height:90vh !important;}")
     ),
  sidebarLayout(
     sidebarPanel(
        #selectInput("selectCohort", "Select Cohort", c("", cohort.names), selectize=FALSE),
        #actionButton("nextCohortButton", "Next Cohort", style=button.style),
        #actionButton("previousCohortButton", "previous Cohort", style=button.style),
        actionButton("fitButton", "Fit", style=button.style),
        actionButton("fitSelectionButton", "Fit Selected", style=button.style),
        actionButton("sfn", "Select First Neighbor", style=button.style),
        actionButton("hideUnselected", "Hide Unselected", style=button.style),
        actionButton("showAll", "Show All", style=button.style),
        actionButton("clearSelection", "Deselect Nodes", style=button.style),
        #actionButton("getSelectedNodes", "Get Selected Nodes", style=button.style),
        #htmlOutput("selectedNodesDisplay"),
        selectInput("layoutSelector", "Layout",
                    c("custom", "breadthfirst", "circle", "cola", "concentric", "cose", "cose-bilkent",
                      "dagre", "grid", "random"), selected=defaultLayoutStrategy),
        width=2,
        style="margin-top: 20px; margin-right:0px; padding-right:10px;"
        ),
     mainPanel(
         tabsetPanel(type="tabs", id="lcGenesTabs",
            tabPanel(title="cytoscape", value="cytoscapeTab", cyjShinyOutput('cyjShiny')),
            tabPanel(title="3D (experimental)", value="threejsTab", scatterplotThreeOutput("graph3d"))
            ),
        width=10,
        style="margin-left:0px; padding-left:0px; padding-right:30px;padding-top: 10px;"
        )
     ) # sidebarLayout
))
#----------------------------------------------------------------------------------------------------
server = function(input, output, session) {

   observeEvent(input$fitButton, ignoreInit=TRUE, {
      fit(session)
      Sys.sleep(0.5)
      selectNodes(session, tbl.nodes$id[sample(1:3, 1)])
      })

   observeEvent(input$fitSelectionButton, ignoreInit=TRUE, {
      fitSelected(session)
      Sys.sleep(0.5)
      selectNodes(session, tbl.nodes$id[sample(1:3, 1)])
      })

   observeEvent(input$sfn, ignoreInit=TRUE, {
      selectFirstNeighbors(session)
      })

   observeEvent(input$hideUnselected, ignoreInit=TRUE, {
      selectedNodes <- getSelectedNodes(session)
      invertSelection(session)
      hideSelection(session)
      selectNodes(session, selectedNodes)
      })


   observeEvent(input$showAll, ignoreInit=TRUE, {
      showAll(session)
      })

   observeEvent(input$clearSelection, ignoreInit=TRUE, {
      clearSelection(session)
      })


   observeEvent(input$layoutSelector, ignoreInit=FALSE, {
      layoutName <- input$layoutSelector
      if(layoutName == "custom")
         setNodePositions(session, tbl.customNodePositions)
      else
         doLayout(session, layoutName)
      fit(session)
      })

   observeEvent(input$selectCohort, ignoreInit=TRUE, {
      cohort.name <- input$selectCohort;
      nodeNames <- tbl.nodes$id
      if(nchar(cohort.name) == 0)
        newValues <- rep(0, nrow(tbl.nodes))
      else
        newValues <- tbl.nodes[, cohort.name]
      setNodeAttributes(session, attributeName="assay", nodes=nodeNames, newValues)
      #newValues <- runif(n=3, min=-3, max=3)
      #setNodeAttributes(session, attributeName="lfc", nodes=nodeNames, newValues)
      })

   output$value <- renderPrint({input$action})

   output$cyjShiny <- renderCyjShiny({
     printf("renderCyjShiny")
     #print(graph.json)
     #print(class(graph.json))
     cyjShiny(graph=g.json.string, layoutName=defaultLayoutStrategy, styleFile="basicStyle.js")
     # later(function(){setNodePositions(session, tbl.pos)}, 2)
     })

    output$graph3d <- renderScatterplotThree({
       # graphjs(ig)
       l1 = light_directional(color = "red", position = c(0, -0.8, 0.5))
       l2 = light_directional(color = "yellow", position = c(0, 0.8, -0.5))
       l3 = light_ambient(color = "#555555")
       node.colors <- rainbow(n=11)[as.factor(tbl.nodes$Type.of.node)]
       graphjs(ig,
               vertex.shape="sphere",
               main="LASER",
               vertex.color=node.colors,
               vertex.label=tbl.nodes$id,
               lights=list(l1, l2, l3),
               brush=TRUE
               )
       })

   observeEvent(input$savePNGbutton, ignoreInit=TRUE, {
     file.name <- tempfile(fileext=".png")
     savePNGtoFile(session, file.name)
     })

   observeEvent(input$pngData, ignoreInit=TRUE, {
     printf("received pngData")
     png.parsed <- fromJSON(input$pngData)
     substr(png.parsed, 1, 30) # [1] "data:image/png;base64,iVBORw0K"
     nchar(png.parsed)  # [1] 768714
     png.parsed.headless <- substr(png.parsed, 23, nchar(png.parsed))  # chop off the uri header
     png.parsed.binary <- base64decode(png.parsed.headless)
     printf("writing png to foo.png")
     conn <- file("foo.png", "wb")
     writeBin(png.parsed.binary, conn)
     close(conn)

     })


} # server
#----------------------------------------------------------------------------------------------------
#runApp(shinyApp(ui=ui,server=server), port=6769)
shinyApp(ui=ui, server=server)


