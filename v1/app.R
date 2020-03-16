library(cyjShiny)
library(later)
#----------------------------------------------------------------------------------------------------
styles <- c("",
            "generic style"="basicStyle.js",
            "style 01" = "style01.js")
#----------------------------------------------------------------------------------------------------
# create  data.frames for nodes, edges, and two simulated experimental variables, in 3 conditions
#----------------------------------------------------------------------------------------------------
#tbl.edges <- read.table("tab3.tsv", sep="\t", as.is=TRUE)
#tbl.edges <- read.table("laser.tsv", sep="\t", as.is=TRUE, skip=1)
tbl.edges <- read.table("04mar2020.tsv", sep="\t", header=FALSE, as.is=TRUE, fill=TRUE)
colnames(tbl.edges) <- c("source", "interaction", "target")
dim(tbl.edges)
tbl.edges <- subset(tbl.edges, nchar(target) > 0)
dim(tbl.edges)
graph.json <- dataFramesToJSON(tbl.edges)

#----------------------------------------------------------------------------------------------------
ui = shinyUI(fluidPage(

  tags$head(tags$style("#cyjShiny{height:95vh !important;}")),
  sidebarLayout(
      sidebarPanel(
          selectInput("loadStyleFile", "Select Style: ", choices=styles),
          selectInput("doLayout", "Select Layout:",
                      choices=c("",
                                "cose",
                                "cola",
                                "circle",
                                "concentric",
                                "breadthfirst",
                                "grid",
                                "preset",
                                "random",
                                "dagre",
                                "cose-bilkent")),


          actionButton("sfn", "Select First Neighbor"),
          actionButton("fit", "Fit Graph"),
          actionButton("fitSelected", "Fit Selected"),
          actionButton("clearSelection", "Clear Selection"), HTML("<br>"),
          #actionButton("loopConditions", "Loop Conditions"), HTML("<br>"),
          actionButton("removeGraphButton", "Remove Graph"), HTML("<br>"),
          actionButton("addRandomGraphFromDataFramesButton", "Add Random Graph"), HTML("<br>"),
          actionButton("getSelectedNodes", "Get Selected Nodes"), HTML("<br><br>"),
          htmlOutput("selectedNodesDisplay"),
          width=2
      ),
      mainPanel(cyjShinyOutput('cyjShiny', height=400),width=10)
  ) # sidebarLayout
))
#----------------------------------------------------------------------------------------------------
server = function(input, output, session)
{
    observeEvent(input$fit, ignoreInit=TRUE, {
       fit(session, 80)
       })

    observeEvent(input$loadStyleFile,  ignoreInit=TRUE, {
       if(input$loadStyleFile != ""){
          tryCatch({
             loadStyleFile(input$loadStyleFile)
             }, error=function(e) {
                msg <- sprintf("ERROR in stylesheet file '%s': %s", input$loadStyleFile, e$message)
                showNotification(msg, duration=NULL, type="error")
                })
           later(function() {updateSelectInput(session, "loadStyleFile", selected=character(0))}, 0.5)
          }
       })

    observeEvent(input$doLayout,  ignoreInit=TRUE,{
       if(input$doLayout != ""){
          strategy <- input$doLayout
          doLayout(session, strategy)
          later(function() {updateSelectInput(session, "doLayout", selected=character(0))}, 1)
          }
       })

    observeEvent(input$selectName,  ignoreInit=TRUE,{
       selectNodes(session, input$selectName)
       })

    observeEvent(input$sfn,  ignoreInit=TRUE,{
       selectFirstNeighbors(session)
       })

    observeEvent(input$fitSelected,  ignoreInit=TRUE,{
       fitSelected(session, 100)
       })

    observeEvent(input$getSelectedNodes, ignoreInit=TRUE, {
       output$selectedNodesDisplay <- renderText({" "})
       getSelectedNodes(session)
       })

    observeEvent(input$clearSelection,  ignoreInit=TRUE, {
       clearSelection(session)
       })

    observeEvent(input$loopConditions, ignoreInit=TRUE, {
        condition.names <- rownames(tbl.lfc)
        for(condition.name in condition.names[-1]){
           #browser()
           lfc.vector <- as.numeric(tbl.lfc[condition.name,])
           node.names <- rownames(tbl.lfc)
           setNodeAttributes(session, attributeName="lfc", nodes=node.names, values=lfc.vector)
           #updateSelectInput(session, "setNodeAttributes", selected=condition.name)
           Sys.sleep(1)
           } # for condition.name
        updateSelectInput(session, "setNodeAttributes", selected="baseline")
        })

    observeEvent(input$removeGraphButton, ignoreInit=TRUE, {
        removeGraph(session)
        })

    observeEvent(input$addRandomGraphFromDataFramesButton, ignoreInit=TRUE, {
        source.nodes <-  LETTERS[sample(1:5, 5)]
        target.nodes <-  LETTERS[sample(1:5, 5)]
        tbl.edges <- data.frame(source=source.nodes,
                                target=target.nodes,
                                interaction=rep("generic", length(source.nodes)),
                                stringsAsFactors=FALSE)
        all.nodes <- sort(unique(c(source.nodes, target.nodes, "orphan")))
        tbl.nodes <- data.frame(id=all.nodes,
                                type=rep("unspecified", length(all.nodes)),
                                stringsAsFactors=FALSE)
        addGraphFromDataFrame(session, tbl.edges, tbl.nodes)
        })

    observeEvent(input$selectedNodes, {
          #  communicated here via assignement in cyjShiny.js
          #     Shiny.setInputValue("selectedNodes", value, {priority: "event"});
        newNodes <- input$selectedNodes;
        output$selectedNodesDisplay <- renderText({
           paste(newNodes)
           })
        })

    # output$value <- renderPrint({ input$action })

    output$cyjShiny <- renderCyjShiny({
       printf(" renderCyjShiny invoked")
       printf("graph.json:")
       print(fromJSON(graph.json))
       cyjShiny(graph=graph.json, layoutName="cose")
       })

} # server
#----------------------------------------------------------------------------------------------------
app <- shinyApp(ui = ui, server = server)
runApp(app, port=9999)

