ID = 14may2020
IMAGE_NAME = laser_schools_$(ID)

default:
	@echo targets:  shiny buildDocker

shiny:
	R -f laserShiny.R


build:
	docker build -t $(IMAGE_NAME) -f Dockerfile .


bash:
	docker run -it \
           -p 3838:9012 \
           --entrypoint /bin/bash \
           $(IMAGE_NAME)

dockerRun:
	docker run -p $(TESTPORT):3838 $(IMAGE_NAME)

