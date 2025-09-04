# snv-benchmarking-dashboard
Interactive web application for variant calling performance analysis and visualization.
# Overview
The SNV Benchmarking Dashboard provides systematic evaluation and comparison of variant calling performance across multiple sequencing technologies (Illumina, PacBio, ONT, MGI) and computational pipelines. It processes hap.py benchmarking results and provides interactive visualizations for performance analysis across genomic regions.
# Architecture
<img width="716" height="417" alt="image" src="https://github.com/user-attachments/assets/19a7e2e8-26ee-482c-b18f-bc5c06ce4cf1" />

<br>The dashboard is built on a hybrid R–Python setup that combines R’s strengths in visualization with Python’s data processing:

**R Shiny Frontend** handles user interactions, reactive visualizations, and web interface management. The frontend is modularized across multiple files for maintainability and includes interactive plots, data tables, and export functionality.

**Python Backend** manages data management, file processing, and complex query operations using SQLAlchemy ORM. It processes hap.py CSV files, handles uploads/deletions, and provides an API for the frontend.

**SQLite Database** stores normalized experiment metadata and benchmarking results, ready for PostgreSQL scaling.
Communication between layers occurs through the **reticulate** interface, allowing R to directly call Python functions and access the database.


# <br> Code Structure

**Frontend (R Shiny)**

* **frontend/**
<br> - **app.R** - Main application file
<br> - **constants.R** - UI configuration and styling
<br> - **data_processing**.R - Reactive data management
<br> - **plot_functions.R** - Visualization generation
<br> - **table_functions.R** - Data table creation
<br> - **observers.R** - Event handling and interactions
<br> - **ui_components.R** - Dynamic UI elements (upload/delete modal, etc)
<br> - **utils.R** - Color schemes and helper functions
<br> - **html_export.R** - Report generation functionality

 **Backend (Python)**

* **backend/**
<br> - **__init__.py**
<br> - **config.py** - Environment and path configuration
<br> - **database.py** - Database connection and session management
<br> - **models.py** - SQLAlchemy ORM models and enums
<br> - **db_interface.py** - Query functions for R integration
<br> - **populate_metadata.py** - CSV metadata processing
<br> - **happy_parser.py** - Hap.py result file parsing
<br> - **upload_handler.py** - File upload processing
<br> - **delete_handler.py** - Dataset deletion management
<br> - **create_database.py** - Database initialization script
<br> - **utils.py** - Shared utility functions
<br> - **requirements.txt** - Python dependencies


 **Data (External Volume) & Configuration**

* **data/**
<br> - **happy_files/** - Hap.py CSV files and experiment metadata (mounted at /data in container)
<br> - **benchmarking.db** - SQLite database


* **Containerfile** - Container build with R and Python environments
* **github/workflows/** - Automated container builds (create GitHub release to trigger deployment)

# <br>Features
**1. Multi-Technology Benchmarking**
<br> * Sequencing Technologies: Illumina, PacBio, ONT, MGI
<br> * Variant Callers: DeepVariant, GATK, Clair3
<br> * Truth Sets: GIAB, CMRG, T2T reference standards
<br> * Variant Types: SNP and INDEL performance analysis

<br> **2. Interactive Analysis**
<br> * Comparison Modes: Technology-vs-technology, caller-vs-caller, or custom experiment selection
<br> * Data Tables: Sortable, filterable datasets with expandable metadata and performance metrics 
<br> * Performance Visualization: Precision vs Recall scatter plots with F1-score contours
<br> * Stratified Analysis: Performance across genomic regions (easy/difficult, GC content, functional annotations)

<br> **3. Data Management**
<br> * Upload Interface: Add new hap.py results with metadata validation to database
<br> * Export Functionality: Generate comprehensive HTML reports
<br> * Database Integration: Normalized storage with metadata relationships
<br> * File Management: Automated filename generation organization

# <br> Local Deployment 
<br>**Prerequisites**

* **Docker**
* **Data directory containing:**
<br>* Metadata CSV (000_benchmark_dashboard_default_metadata.csv) 
<br>* Hap.py CSV files (e.g., 001_HG002_Illumina_DeepVariant_GIAB.csv)

<br>**1. Create deployment directory**
```bash
mkdir -p ~/snv-dashboard
cd ~/snv-dashboard
```
**2.Deploy with Docker**
```bash
# Pull the latest container image
docker pull ghcr.io/c3g/snv-dashboard:latest

# Start the dashboard (replace with your data path)
docker run -d \
  --name snv-benchmarking \
  -p 3838:3838 \
  -v "/path/to/your/benchmarking/data:/data" \
  -e BENCHMARKING_DATA_LOCATION=/data \
  ghcr.io/c3g/snv-dashboard:latest
```
**3. Access Local Dashboard**
<br>Open http://localhost:3838 on your browser
