# snv-benchmarking-dashboard

Interactive web application for variant calling performance analysis and visualization with authentication.

# Overview

The SNV Benchmarking Dashboard provides systematic evaluation and comparison of variant calling performance across multiple sequencing technologies (Illumina, PacBio, ONT, MGI) and computational pipelines. It processes hap.py benchmarking results and provides interactive visualizations for performance analysis across genomic regions.

# Architecture

<img width="716" height="417" alt="image" src="https://github.com/user-attachments/assets/19a7e2e8-26ee-482c-b18f-bc5c06ce4cf1" />

<br>The dashboard is built on a hybrid R–Python setup that combines R's strengths in visualization with Python's data processing:

**R Shiny Frontend** handles user interactions, reactive visualizations, and web interface management. The frontend is modularized across multiple files for maintainability and includes interactive plots, data tables, and export functionality.

**Python Backend** manages data management, file processing, and complex query operations using SQLAlchemy ORM. It processes hap.py CSV files, handles uploads/deletions, and provides an API for the frontend.

**SQLite Database** stores normalized experiment metadata and benchmarking results, ready for PostgreSQL scaling.

Communication between layers occurs through the **reticulate** interface, allowing R to directly call Python functions and access the database.

# Authentication & Authorization

## OpenID Connect (OIDC) Integration

The dashboard implements authentication using OpenID Connect protocol with COManage integration:

**Authentication Flow:**
- Users authenticate via OIDC provider (COManage)
- OAuth 2.0 authorization code flow
- JWT token validation and claims extraction
- Session persistence using browser localStorage
- Automatic session restoration on page reload

** Current Authorization Roles:** 
- **Admin**: Full access including upload and delete operations
- **Member**: Standard view-only access (includes a beta message)
- **anonymous**: Standard view-only access

**Group-Based Access Control:**
The system maps COManage groups to application roles:
- `snv-benchmarking-dashboard-admins` → Admin role
- `snv-benchmarking-dashboard-members` → Member role

**Authorization:**
- Upload and delete operations are currently restricted to admin role only
- Role verification performed in backend via `@require_admin` decorator
- Frontend UI elements conditionally rendered based on user role

**Configuration:**
Authentication is configured through the following environment variables (outside the app):
```bash
OIDC_ISSUER=https://comanage-domain.com/realms/realm
OIDC_CLIENT_ID=client-id
OIDC_CLIENT_SECRET=client-secret
OIDC_REDIRECT_URI=https://dashboard-domain.com
```


# <br> Code Structure

**Frontend (R Shiny)**

* **frontend/**
<br> - **app.R** - Main application file with UI definition and server logic
<br> - **auth.R** - OIDC authentication flow, token exchange, and session management
<br> - **constants.R** - UI configuration, styling, and JavaScript handlers
<br> - **data_processing.R** - Reactive data management and state handling
<br> - **plot_functions.R** - Visualization generation (precision-recall plots, F1 contours)
<br> - **table_functions.R** - Data table creation and formatting
<br> - **observers.R** - Event handling and user interactions
<br> - **ui_components.R** - Dynamic UI elements (upload/delete modals)
<br> - **utils.R** - Color schemes, helper functions, and backend interface
<br> - **html_export.R** - Report generation functionality
<br> - **dynamic_options.R** - Dynamic dropdown population based on current technologies/callers in the databse

 **Backend (Python)**

* **backend/**
<br> - **__init__.py** - Package initialization
<br> - **config.py** - Environment detection (container vs local), path configuration
<br> - **database.py** - Database connection and session management
<br> - **models.py** - SQLAlchemy ORM models with enums for all categorical data
<br> - **db_interface.py** - Query functions for R integration via reticulate
<br> - **authorization.py** - Admin permission decorator for sensitive operations
<br> - **populate_metadata.py** - CSV metadata processing and database population
<br> - **happy_parser.py** - Hap.py result file parsing and validation
<br> - **upload_handler.py** - File upload processing with admin authorization
<br> - **delete_handler.py** - Dataset deletion with admin authorization
<br> - **create_database.py** - Database initialization and schema creation
<br> - **utils.py** - Shared utility functions
<br> - **requirements.txt** - Python dependencies

 **Data (External Volume) & Configuration**

* **data/**
<br> - **happy_files/** - Hap.py CSV files and experiment metadata (mounted at /data in container)
<br> - **benchmarking.db** - SQLite database
<br> - **deleted/** - Archive folder for deleted experiments and metadata backups

* **Containerfile** - Container build with R and Python environments, OIDC dependencies
* **.github/workflows/** - Automated container builds (create GitHub release to trigger deployment)

# <br>Features

**1. Authentication & Authorization**
<br> * OpenID Connect integration with COManage
<br> * Role-based access control (Admin, Approver, Member)
<br> * Session management with automatic restoration
<br> * Secure OAuth 2.0 authorization flow

<br> **2. Multi-Technology Benchmarking**
<br> * Sequencing Technologies: Illumina, PacBio, ONT, MGI
<br> * Variant Callers: DeepVariant, GATK, Clair3
<br> * Truth Sets: GIAB, CMRG, T2T reference standards
<br> * Variant Types: SNP and INDEL performance analysis

<br> **3. Interactive Analysis**
<br> * Comparison Modes: Technology-vs-technology, caller-vs-caller, or custom experiment selection
<br> * Data Tables: Sortable, filterable datasets with expandable metadata and performance metrics 
<br> * Performance Visualization: Precision vs Recall scatter plots with F1-score contours
<br> * Stratified Analysis: Performance across genomic regions (easy/difficult, GC content, functional annotations)

<br> **4. Data Management (Admin Only - for now)**
<br> * Upload Interface: Add new hap.py results with metadata validation
<br> * Delete Functionality: Remove experiments with automatic database rebuilding
<br> * Archive System: Automatic backup of deleted experiments and metadata
<br> * File Management: Automated filename generation and organization
<br> * Export Functionality: Generate comprehensive HTML reports (available to all authenticated users)

# <br> Deployment

## Prerequisites

* **Docker** or **Podman**
* **OIDC Provider** (ie. COManage) with configured client
* **Data directory containing:**
<br>  - Metadata CSV (000_benchmark_dashboard_default_metadata.csv) including hap.py filenames linked to each record
<br>  - Hap.py CSV files ( All file names must match those included in the metadata CSV)

## Environment Configuration

Create a `.env` file with your OIDC configuration:

```bash
OIDC_ISSUER=https://your-keycloak-domain.com/realms/your-realm
OIDC_CLIENT_ID=your-client-id
OIDC_CLIENT_SECRET=your-client-secret
OIDC_REDIRECT_URI=https://your-dashboard-domain.com
BENCHMARKING_DATA_LOCATION=/data
```

## Container Deployment

**1. Create deployment directory**
```bash
mkdir -p ~/snv-dashboard
cd ~/snv-dashboard
```

**2. Pull container image**
```bash
docker pull ghcr.io/c3g/snv-dashboard:latest
```

**3. Start the dashboard**
```bash
docker run -d \
  --name snv-benchmarking \
  -p 3838:3838 \
  -v "/path/to/your/benchmarking/data:/data" \
  -e BENCHMARKING_DATA_LOCATION=/data \
  -e OIDC_ISSUER="https://your-keycloak-domain.com/realms/your-realm" \
  -e OIDC_CLIENT_ID="your-client-id" \
  -e OIDC_CLIENT_SECRET="your-client-secret" \
  -e OIDC_REDIRECT_URI="http://localhost:3838" \
  ghcr.io/c3g/snv-dashboard:latest
```

**4. Access the dashboard**
<br>Open http://localhost:3838 in your browser and sign in with your OIDC credentials

## Local Development

**1. Install dependencies**
```bash
# Python dependencies
cd backend
pip install -r requirements.txt

# R dependencies (in R console)
install.packages(c("shiny", "reticulate", "DT", "plotly", "ggsci", 
                   "patchwork", "geomtextpath", "jsonlite", "htmltools",
                   "htmlwidgets", "tidyr", "shinyBS", "httr2", "jose", 
                   "logger", "shinyjs"))
```

**2. Configure environment**
```bash
# Set environment variables
export OIDC_ISSUER="https://your-keycloak-domain.com/realms/your-realm"
export OIDC_CLIENT_ID="your-client-id"
export OIDC_CLIENT_SECRET="your-client-secret"
export OIDC_REDIRECT_URI="http://localhost:3838"
export BENCHMARKING_DATA_LOCATION="./data/happy_files"
```

**3. Initialize database**
```bash
cd backend
python create_database.py
```

**4. Start application**
```bash
cd frontend
R -e "shiny::runApp('.', port=3838)"
```

# Database Schema

The application uses a normalized SQLite database with the following structure:

**Core Tables:**
- `experiments` - Main experiment metadata linking all components
- `sequencing_technologies` - Sequencing platform details (Illumina, PacBio, ONT, MGI)
- `variant_callers` - Caller information (DeepVariant, GATK, Clair3)
- `aligners` - Alignment tool details
- `truth_sets` - Validation/reference sets (GIAB, CMRG, T2T)
- `benchmark_tools` - Benchmarking tool versions (hap.py)
- `variants` - Variant type specifications (SNP, INDEL)
- `quality_control_metrics` - Sequencing QC metrics
- `chemistries` - Chemistry and kit information

**Results Tables:**
- `overall_results` - Fast access table for overall (*) region performance metrics
- `benchmark_results` - Complete stratified results across all genomic regions

**Enums:**
All categorical data uses Python enums for type safety, including SeqTechName, CallerName, TruthSetName, VariantType, RegionType, and more.

# File Naming Convention

Uploaded hap.py files are automatically renamed following this format:

```
[ID]_[Sample]_[Technology]_[Platform]_[Caller]_[TruthSet].csv
```

Example: `001_HG002_Illumina_NovaseqX_DeepVariant_GIAB.csv`

# Data Flow

**Upload Process:**
1. Admin authenticates via OIDC
2. Upload hap.py CSV with metadata form
3. Backend validates file format and metadata
4. Generate unique filename and ID
5. Update metadata CSV (000_benchmark_dashboard_default_metadata.csv)
6. Parse and insert data into database
7. File becomes available in dashboard

**Delete Process:**
1. Admin selects experiment for deletion
2. Backend creates backup of metadata CSV
3. Remove experiment from metadata CSV
4. Move hap.py file to deleted/ archive folder
5. Drop and rebuild entire database from updated CSV
6. Changes reflected immediately in dashboard

# Troubleshooting

**Authentication Issues:**
- Verify OIDC environment variables are correctly set
- Check that redirect URI and client secret matches exactly in both OIDC provider and application
- Ensure COManage groups are properly configured for your users

**Database Issues:**
- Ensure data directory is mounted to /data
- Run `python backend/create_database.py` to reinitialize database
- Check that metadata CSV (000_benchmark_dashboard_default_metadata.csv) exists and is valid
- Check hap.py files all exist under happy_files folder


**Upload/Delete Not Working:**
- Verify user has admin role in COManage groups
- Check backend logs for authorization failures
- Ensure metadata CSV is writable by application

# Contact
soofiagarmeh@gmail.com
