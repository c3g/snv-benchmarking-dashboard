# snv-benchmarking-dashboard
Interactive web application for variant calling performance analysis and visualization.
# Overview
The SNV Benchmarking Dashboard provides systematic evaluation and comparison of variant calling performance across multiple sequencing technologies (Illumina, PacBio, ONT, MGI) and computational pipelines. It processes hap.py benchmarking results and provides interactive visualizations for performance analysis across genomic regions.
# Architecture
<img width="716" height="417" alt="image" src="https://github.com/user-attachments/assets/19a7e2e8-26ee-482c-b18f-bc5c06ce4cf1" />

The dashboard uses a hybrid R-Python architecture that combines R's visualization strengths with Python's data processing capabilities:

**R Shiny Frontend** handles user interactions, reactive visualizations, and web interface management. The frontend is modularized across multiple files for maintainability and includes interactive plots, data tables, and export functionality.

**Python Backend** manages data management, file processing, and complex query operations using SQLAlchemy ORM. It processes hap.py CSV files, handles uploads/deletions, and provides an API for the frontend.

**SQLite Database** stores normalized experiment metadata and benchmarking results, ready for PostgreSQL scaling.
Communication between layers occurs through the **reticulate** interface, allowing R to directly call Python functions and access the database.
