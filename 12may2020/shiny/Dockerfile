FROM rocker/shiny
WORKDIR /home/shiny
COPY installPrerequisites.R .
RUN R -f ./installPrerequisites.R .
COPY packages/cyjShiny_1.0.17.tar.gz .
RUN R CMD INSTALL cyjShiny_1.0.17.tar.gz
COPY packages/threejs_0.3.3.tar.gz .
RUN R CMD INSTALL threejs_0.3.3.tar.gz
COPY laserShiny.R .
COPY nodesAndEdges.RData .
COPY layout.RData .
COPY basicStyle.js .
RUN R -f ./installPrerequisites.R .
CMD ["R", "-e", "shiny::runApp('/home/shiny/laserShiny.R', port=3838, host='0.0.0.0')"]
