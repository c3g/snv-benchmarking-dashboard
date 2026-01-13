# R Shiny base
FROM rocker/shiny-verse:4.5.0

# Install system dependencies
RUN apt-get update && apt-get install -y \
    python3 \
    python3-pip \
    python3-venv \
    curl \
    libcairo2-dev \
    libxt-dev \
    libpango1.0-dev \
    && rm -rf /var/lib/apt/lists/*

# Setup Python virtual environment
RUN python3 -m venv /opt/venv
ENV PATH="/opt/venv/bin:$PATH"

# Install Python packages
COPY backend/requirements.txt /tmp/requirements.txt
RUN pip install --no-cache-dir -r /tmp/requirements.txt

# Install R packages
RUN install2.r --error \
    reticulate \
    DT \
    plotly \
    ggsci \
    ggrepel \
    patchwork \
    geomtextpath \
    jsonlite \
    htmltools \
    htmlwidgets \
    tidyr \
    base64enc \
    ggforce \
    dplyr \
    shinyBS \
    scales \
    httr \
    httr2 \
    logger \
    jose \
    shinyjs

# Install app in /app folder
COPY backend/ /app/backend/
COPY frontend/ /app/frontend/

# Environment variables
ENV PYTHONPATH="/app/backend:/app"
ENV BENCHMARKING_DATA_LOCATION="/data"
ENV RETICULATE_PYTHON="/opt/venv/bin/python"

# Expose port 3838
EXPOSE 3838

# Volume
VOLUME ["/data"]

WORKDIR /app/frontend

# Start command
CMD ["sh", "-c", "cd /app/backend && python3 create_database.py && cd /app/frontend && R -e \"library(reticulate); use_python('/opt/venv/bin/python'); options(shiny.host='0.0.0.0', shiny.port=3838); shiny::runApp('.', launch.browser=FALSE)\""]