FROM rocker/shiny:latest

# System dependencies for sf, GEOS, GDAL, curl, Excel files
RUN apt-get update && apt-get install -y \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libssl-dev \
    libcurl4-openssl-dev \
    libxml2-dev \
    libudunits2-dev \
    && rm -rf /var/lib/apt/lists/*

# Install all R packages
RUN R -e "install.packages(c( \
    'shiny', \
    'shinydashboard', \
    'shinyjs', \
    'plotly', \
    'dplyr', \
    'DT', \
    'sf', \
    'readr', \
    'leaflet', \
    'htmltools', \
    'yyjsonr', \
    'readxl', \
    'lattice', \
    'grid', \
    'ggplot2', \
    'lubridate', \
    'bslib', \
    'bsicons' \
), repos='https://cran.rstudio.com/', dependencies=TRUE)"

# Copy app files into the Shiny server app directory
COPY app.R /srv/shiny-server/app/app.R
COPY kaudulla_elephants_clean.csv /srv/shiny-server/app/
COPY daily_climate.xlsx /srv/shiny-server/app/

# Expose Shiny default port
EXPOSE 3838
