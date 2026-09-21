# Base R Shiny image
FROM rocker/r-ver:4.6.1

# 1. Installeer systeemvereisten (libuv1-dev is nodig voor het 'fs' pakket in tidyverse)
RUN apt-get update && apt-get install -y --no-install-recommends \
    libuv1-dev \
    libssl-dev \
    libcurl4-openssl-dev \
    libxml2-dev \
    libwebp-dev \
    && rm -rf /var/lib/apt/lists/*

# 2. Installeer R dependencies
RUN R -e "install.packages(c('shiny','tidyverse','DT','bslib'))"
 
# Copy the Shiny app code
COPY app.R /app/
COPY data/productiedata.dat /app/data/
# COPY data.Rda /app

WORKDIR /app

# Expose the application port
EXPOSE 8080

# Run the R Shiny app
CMD ["R", "-e", "shiny::runApp('./app.R', host='0.0.0.0', port=8080)"]
