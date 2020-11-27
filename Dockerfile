FROM ptagliolato/r-spatial-base:1.0.2
# the r-spatial-base:1.0.2 is preconfigured with all needed system and R libraries.
# refer to it for any detail

# copy shiny app files
RUN mkdir /root/TELLme-mapComposer
RUN mkdir /root/TELLme-mapComposer/offline_files
RUN mkdir /root/TELLme-mapComposer/cityGeojson
RUN mkdir /root/TELLme-mapComposer/www
RUN mkdir /root/TELLme-mapComposer/www/icons
RUN mkdir /root/TELLme-mapComposer/www/css

COPY README.md root/TELLme-mapComposer/README.md
COPY server.R root/TELLme-mapComposer/server.R
COPY global.R root/TELLme-mapComposer/global.R
COPY ui.R root/TELLme-mapComposer/ui.R
COPY lazyLoadGlossary.R root/TELLme-mapComposer/lazyLoadGlossary.R
COPY TELLmeHub.R root/TELLme-mapComposer/TELLmeHub.R
COPY www/css/style.css root/TELLme-mapComposer/www/css/style.css
COPY www/icons/logo-tellme-1.jpg root/TELLme-mapComposer/www/icons/logo-tellme-1.jpg
COPY cityGeojson/dt_Metropolis.rds root/TELLme-mapComposer/cityGeojson/dt_Metropolis.rds
COPY cityGeojson/dt_Metropolis.gpkg root/TELLme-mapComposer/cityGeojson/dt_Metropolis.gpkg
COPY cityGeojson/citiesGPS.gpkg root/TELLme-mapComposer/cityGeojson/citiesGPS.gpkg
COPY cityGeojson/metropolis2.gpkg root/TELLme-mapComposer/cityGeojson/metropolis2.gpkg

# TODO: Substitute with env file
COPY accounts_private.R root/TELLme-mapComposer/accounts_private.R

# set shiny listen on port 3838 with Rprofile.site provided
COPY Rprofile.site /usr/lib/R/etc/
EXPOSE 3838

LABEL maintainer="ptagliolato <tagliolato.p@irea.cnr.it>" \
   org.opencontainers.image.authors="Tagliolato (tagliolato.p@irea.cnr.it) and Oggioni" \
   org.opencontainers.image.version="1.0.1" \
   org.opencontainers.image.licences="GPL3" \
   edu.science.data.group.project="Erasmus Plus TELLme Project" \
   edu.science.data.group.name="CNR-IREA-milano-LabSDI-GETIT" \
   author.orcid="0000-0002-0261-313X" 

##### USAGE:
# build the image:
#   docker build -t tellme-vlab-mapcomposer:1.0.0 .
# run the container:
#   docker run -it --rm --env-file=env -p 3838:3838 tellme-vlab-mapcomposer:1.0.0
# open your browser (if you are runnung the container in your machine) at the url:
#   http://127.0.0.1:3838/
#####

CMD ["R", "-e", "shiny::runApp('/root/TELLme-mapComposer')"]
