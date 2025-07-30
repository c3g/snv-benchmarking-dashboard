# upload_ui.R
# Upload-related UI components for SNV Benchmarking Dashboard

# Upload Modal UI
upload_modal_ui <- function() {
  bsModal(
    "upload_modal", 
    "Add New Experiment", 
    "show_upload_modal", 
    size = "large",
    
    # Step 1: File Upload
    wellPanel(
      style = "background-color: #f8f9fa; margin-bottom: 20px;",
      fluidRow(
        column(8,
               h4("1. Upload hap.py CSV File"),
               fileInput("upload_file", 
                         "Select File",
                         accept = ".csv",
                         placeholder = "Choose hap.py CSV file...")
        ),
        column(4,
               br(),
               div(id = "file_status", style = "margin-top: 10px;")
        )
      )
    ),
    
    # Step 2: Metadata Form
    wellPanel(
      h4("2. Experiment Metadata"),
      
      # Row 1: Experiment Info
      fluidRow(
        column(4,
               textInput("exp_name", "Experiment Name*", 
                         placeholder = "e.g., HG002_Illumina_NovaSeq")
        ),
        column(4,
               textInput("description", "Description", 
                         placeholder = "Optional description")
        ),
        column(4,
               selectInput("target", "Target",
                           choices = c("WGS" = "wgs", "WES" = "wes"),
                           selected = "wgs")
        )
      ),
      
      # Row 2: Sequencing Technology
      h5("Sequencing Technology", style = "color: #007bff; margin-top: 20px;"),
      fluidRow(
        column(3,
               selectInput("technology", "Technology*",
                           choices = c("", "ILLUMINA", "PACBIO", "ONT", "MGI"))
        ),
        column(3,
               textInput("platform_name", "Platform*")
        ),
        column(3,
               selectInput("platform_type", "Platform Type",
                           choices = c("","SRS" = "srs", "LRS" = "lrs"))
        ),
        column(3,
               textInput("platform_version", "Platform Version",
                         placeholder = "Optional")
        )
      ),
      
      # Row 3: Chemistry
      fluidRow(
        column(6,
               textInput("chemistry_name", "Chemistry",
                         placeholder = "Optional")
        ),
        column(6,
               textInput("chemistry_version", "Chemistry Version",
                         placeholder = "Optional")
        )
      ),
      
      # Row 4: Variant Caller
      h5("Variant Caller", style = "color: #007bff; margin-top: 20px;"),
      fluidRow(
        column(3,
               selectInput("caller_name", "Caller*",
                           choices = c("", "DEEPVARIANT", "GATK", "CLAIR3"))
        ),
        column(3,
               selectInput("caller_type", "Caller Type",
                           choices = c("","ML" = "ml", "Traditional" = "traditional"))
        ),
        column(3,
               textInput("caller_version", "Caller Version",
                         placeholder = "Optional")
        ),
        column(3,
               textInput("caller_model", "Caller Model",
                         placeholder = "Optional")
        )
      ),
      
      # Row 5: Aligner
      h5("Aligner", style = "color: #007bff; margin-top: 20px;"),
      fluidRow(
        column(6,
               textInput("aligner_name", "Aligner",
                         placeholder = "Optional")
        ),
        column(6,
               textInput("aligner_version", "Aligner Version",
                         placeholder = "Optional")
        )
      ),
      
      # Row 6: Truth Set
      h5("Truth Set", style = "color: #007bff; margin-top: 20px;"),
      fluidRow(
        column(3,
               selectInput("truth_set_name", "Truth Set",
                           choices = c("GIAB" = "giab", "CMRG" = "cmrg", "T2T" = "t2t"),
                           selected = "giab")
        ),
        column(3,
               selectInput("truth_set_sample", "Sample",
                           choices = c("HG001" = "hg001", "HG002" = "hg002", "HG003" = "hg003", "HG004" = "hg004"),
                           selected = "hg002")
        ),
        column(3,
               textInput("truth_set_version", "Truth Set Version",
                         value = "4.2.1")
        ),
        column(3,
               selectInput("truth_set_reference", "Reference",
                           choices = c("GRCh37" = "grch37", "GRCh38" = "grch38"),
                           selected = "grch38")
        )
      ),
      
      # Row 7: Variant Info
      h5("Variant Information", style = "color: #007bff; margin-top: 20px;"),
      fluidRow(
        column(3,
               selectInput("variant_type", "Variant Type",
                           choices = c("SNP+INDEL" = "snp+indel", "SNP" = "snp", "INDEL" = "indel"),
                           selected = "snp+indel")
        ),
        column(3,
               selectInput("variant_size", "Variant Size",
                           choices = c("Small" = "small", "Large" = "large"),
                           selected = "small")
        ),
        column(3,
               selectInput("variant_origin", "Variant Origin",
                           choices = c("Germline" = "germline", "Somatic" = "somatic"),
                           selected = "germline")
        ),
        column(3,
               selectInput("is_phased", "Phased",
                           choices = c("No" = "false", "Yes" = "true"),
                           selected = "false")
        )
      ),
      
      # Row 8: Benchmark Tool
      h5("Benchmark Tool", style = "color: #007bff; margin-top: 20px;"),
      fluidRow(
        column(6,
               selectInput("benchmark_tool_name", "Benchmark Tool",
                           choices = c("hap.py" = "hap.py", "vcfdist" = "vcfdist", "truvari" = "truvari"),
                           selected = "hap.py")
        ),
        column(6,
               textInput("benchmark_tool_version", "Tool Version",
                         value = "0.3.12")
        )
      ),
      
      # Row 9: Quality Metrics
      h5("Quality Metrics", style = "color: #007bff; margin-top: 20px;"),
      fluidRow(
        column(2,
               numericInput("mean_coverage", "Mean Coverage", 
                            value = NA, min = 1, max = 200, step = 0.1)
        ),
        column(3,
               numericInput("read_length", "Read Length (bp) _ SRS", 
                            value = NA, min = 50, max = 500, step = 0.1)
        ),
        column(3,
               numericInput("mean_insert_size", "Mean Insert Size (bp) _ SRS", 
                            value = NA, min = 100, max = 1000, step = 1)
        ),
        column(4,
               numericInput("mean_read_length", "Mean Read Length (bp) _ LRS", 
                            value = NA, min = 100, max = 100000, step = 0.1)
        )
      )
    ),
    
    # Step 3: Preview & Submit
    wellPanel(
      h4("3. Review & Submit"),
      fluidRow(
        column(8,
               div(
                 h5("Generated Filename:"),
                 verbatimTextOutput("filename_preview", placeholder = TRUE)
               )
        ),
        column(4,
               div(style = "text-align: center; padding-top: 20px;",
                   actionButton("submit_upload", "Add to Database", 
                                class = "btn-success btn-lg", 
                                style = "min-width: 150px;")
               )
        )
      ),
      
      br(),
      div(id = "upload_status", style = "margin-top: 15px;")
    )
  )
}

# Upload button for main UI
upload_button_ui <- function() {
  actionButton(
    "show_upload_modal", 
    label = tagList(icon("upload"), "Upload Dataset"),
    class = "btn-success btn-sm",
    style = "font-size: 14px; padding: 6px 12px; white-space: nowrap;"
  )
}