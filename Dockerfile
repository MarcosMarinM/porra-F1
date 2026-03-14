FROM rocker/r-ver:4.3.2

# Sistema base para paquetes de R que requieren compilar
RUN apt-get update && apt-get install -y --no-install-recommends \
    libssl-dev libcurl4-openssl-dev libxml2-dev libfontconfig1-dev libharfbuzz-dev libfribidi-dev \
  && rm -rf /var/lib/apt/lists/*

WORKDIR /app
COPY . /app

# Instala las dependencias de R necesarias para la API
RUN R -e "options(repos='https://cloud.r-project.org'); install.packages(c('plumber','shiny','shinymanager','googlesheets4','dplyr','httr2','jsonlite','memoise','cachem','bslib','ggplot2','tidyr','plotly','uuid','RSQLite','base64enc'))"

EXPOSE 8000
CMD ["R", "-e", "pr <- plumber::plumb('plumber.R'); pr$run(host='0.0.0.0', port=8000)"]
