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

# Copy app files
COPY app.R /srv/shiny-server/app/app.R
COPY kaudulla_elephants_clean.csv /srv/shiny-server/app/
COPY daily_climate.xlsx /srv/shiny-server/app/

# Configure Shiny Server to run on port 7860 (required by Hugging Face)
RUN echo 'run_as shiny;\n\
server {\n\
  listen 7860;\n\
  location / {\n\
    site_dir /srv/shiny-server/app;\n\
    log_dir /var/log/shiny-server;\n\
    directory_index on;\n\
  }\n\
}' > /etc/shiny-server/shiny-server.conf

EXPOSE 7860

CMD ["/usr/bin/shiny-server"]
