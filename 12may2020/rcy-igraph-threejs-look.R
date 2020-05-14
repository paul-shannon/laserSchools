library(igraph)
library(RCyjs)
library(threejs)

load("shiny/nodesAndEdges.RData")
dim(tbl.nodes) # 178 12
dim(tbl.edges) # 286  3
# tbl.nodes <- subset(tbl.nodes, id %in% c(tbl.edges$source, tbl.edges$target))
# dim(tbl.nodes) #  182  12
# deleters <- which(duplicated(tbl.nodes$id))
# if(length(deleters) > 0)
#     tbl.nodes <- tbl.nodes[-deleters,]
#dim(tbl.nodes)

g.json.string <- RCyjs::dataFramesToJSON(tbl.edges, tbl.nodes)
nchar(g.json.string)

rcy <- RCyjs()
setGraph(rcy, g.json.string)
layout(rcy, "cola")
getSupportedNodeShapes(rcy)

unique(tbl.nodes$Type.of.node)
# "CUR", "OPS", "CAS", "PTW", "advocacy", NA, "partner", "SD", "ESD", "LASER Alliance", "government",
unique(tbl.edges$interaction)
# "given to"    "implemented" "developed"


# loadStyleFile(rcy, "kengo-style.js")



ig <- graph_from_data_frame(tbl.edges[, c("source", "target", "interaction")], directed = TRUE,
                            vertices=tbl.nodes)
plot(ig)

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
