# from https://docs.google.com/spreadsheets/d/1m5LXEFUEcSUnoA_LD_VW-f_Y13IY5TOp66rTgMYs4Yo/edit#gid=1444711158
tbl.nodes <- read.table("nodes.tsv", sep="\t", as.is=TRUE, header=TRUE)
dim(tbl.nodes)   # 360 12
colnames(tbl.nodes)
colnames(tbl.nodes)[2] <- "id"
#  [1] "Long.name"
#  [2] "ID..label.in.the.relationships.sheet."
#  [3] "Type.of.node"
#  [4] "LASER.Alliance"
#  [5] "Notes"
#  [6] "X6.LASER.PILLARS"
#  [7] "OPERATIONS..OPS."
#  [8] "PATHWAYS..PTW."
#  [9] "COMMUNITY.AND.ADMINISTRATOR.SUPPORT..CAS."
# [10] "ASSESSMENT..AST."
# [11] "CURRICULUM..CUR."
# [12] "INSTRUCTIONAL.MATERIALS.SUPPORT..IMS."

tbl.edges <- read.table("edges.tsv", sep="\t",  header=FALSE, nrow=-1, as.is=TRUE)
dim(tbl.edges)   # 285 3
head(tbl.edges)
colnames(tbl.edges) <- c("source", "edgeType", "target")
tbl.edges$source <-  sub(" *$", "", tbl.edges$source)
tbl.edges$target <-  sub(" *$", "", tbl.edges$target)

tbl.edges$source <- sub("1-189-????-CA", "1-189-???-CA", tbl.edges$source, fixed=TRUE)
tbl.edges$target <- sub("1-189-????-CA", "1-189-???-CA", tbl.edges$target, fixed=TRUE)

length(tbl.nodes$id) # 360
length(setdiff(tbl.nodes$id, c(tbl.edges$target, tbl.edges$source))) # 178

nodes.in.edges <- sort(unique(c(tbl.edges$source, tbl.edges$target)))
length(nodes.in.edges)  # 178
nodes <- sort(unique(tbl.nodes$id))
length(nodes)  # 356
all(nodes.in.edges %in% nodes)  # TRUE
length(intersect(nodes.in.edges, nodes)) # 178
length(setdiff(nodes.in.edges, nodes))   # 0
x <- setdiff(nodes.in.edges, nodes)

coi <- c("id", "Long.name", "Type.of.node", "LASER.Alliance", "Notes",
         "X6.LASER.PILLARS", "OPERATIONS..OPS.", "PATHWAYS..PTW.",
         "COMMUNITY.AND.ADMINISTRATOR.SUPPORT..CAS.", "ASSESSMENT..AST.",
         "CURRICULUM..CUR.", "INSTRUCTIONAL.MATERIALS.SUPPORT..IMS.")

tbl.nodes <- tbl.nodes[, coi]
head(tbl.nodes)
head(tbl.edges)
tbl.edges$edgeType <- sub(" $", "", tbl.edges$edgeType)
colnames(tbl.edges) <- c("source", "interaction", "target")
table(tbl.edges$interaction)

#  developed    given to implemented
#         57         179          50

dim(tbl.nodes)  # 360 12
dim(tbl.edges)  # 286  3

tbl.nodes <- subset(tbl.nodes, id %in% c(tbl.edges$source, tbl.edges$target))
dim(tbl.nodes) #  182  12
deleters <- which(duplicated(tbl.nodes$id))
if(length(deleters) > 0)
    tbl.nodes <- tbl.nodes[-deleters,]

dim(tbl.nodes) # 178 12

coi <- c("id", "Long.name", "Type.of.node", "LASER.Alliance", "Notes", "X6.LASER.PILLARS",
         "OPERATIONS..OPS.", "PATHWAYS..PTW.", "COMMUNITY.AND.ADMINISTRATOR.SUPPORT..CAS.",
         "ASSESSMENT..AST.", "CURRICULUM..CUR.", "INSTRUCTIONAL.MATERIALS.SUPPORT..IMS.")
tbl.nodes <- tbl.nodes[, coi]

unknown.node.types <- which(is.na(tbl.nodes$Type.of.node))
if(length(unknown.node.types) > 0)
    tbl.nodes$Type.of.node[unknown.node.types] <- "unknown"

save(tbl.nodes, tbl.edges, file="shiny/nodesAndEdges.RData")


