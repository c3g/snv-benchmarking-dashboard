# upload_server.R
# Upload-related server logic for SNV Benchmarking Dashboard

# Initialize upload server components
upload_server <- function(input, output, session) {
  
  # Import the Python upload handler
  upload_handler <- import("upload_handler")
  
  # Helper functions for safe input handling
  safe_input <- function(input_value, default = "") {
    if (is.null(input_value) || is.na(input_value) || input_value == "") {
      return(default)
    }
    return(as.character(input_value))
  }
  
  safe_numeric <- function(input_value) {
    if (is.null(input_value) || is.na(input_value)) {
      return("")
    }
    return(as.character(input_value))
  }
  
  # File validation with real-time feedback
  observe({
    if (!is.null(input$upload_file)) {
      file_info <- input$upload_file
      
      # Basic file validation
      if (!grepl("\\.csv$", file_info$name, ignore.case = TRUE)) {
        output$file_status <- renderUI({
          div(class = "alert alert-danger", style = "padding: 8px; margin: 5px 0;",
              "Please upload a CSV file")
        })
      } else if (file_info$size == 0) {
        output$file_status <- renderUI({
          div(class = "alert alert-danger", style = "padding: 8px; margin: 5px 0;",
              "File appears to be empty")
        })
      } else if (file_info$size > 100 * 1024 * 1024) {  # 100MB limit
        output$file_status <- renderUI({
          div(class = "alert alert-warning", style = "padding: 8px; margin: 5px 0;",
              "⚠️ Large file detected. Upload may take some time.")
        })
      } else {
        output$file_status <- renderUI({
          div(class = "alert alert-success", style = "padding: 8px; margin: 5px 0;",
              paste("File ready:", file_info$name, 
                    paste0("(", round(file_info$size / 1024, 1), " KB)")))
        })
      }
    } else {
      output$file_status <- renderUI({ NULL })
    }
  })
  
  # Generate filename preview
  # Generate filename preview
  output$filename_preview <- renderText({
    if (!is.null(input$exp_name) && input$exp_name != "" &&
        !is.null(input$technology) && input$technology != "" &&
        !is.null(input$platform_name) && input$platform_name != "" &&
        !is.null(input$caller_name) && input$caller_name != "" &&
        !is.null(input$truth_set_name) && input$truth_set_name != "") {
      
      # Helper function to clean values
      strip_value <- function(value) {
        if (is.null(value) || is.na(value) || value == "") {
          return("")
        }
        return(trimws(as.character(value)))
      }
      
      tryCatch({
        # Get next experiment ID 
        if (file.exists(system.file("", package = "base"))) {  
          preview_id <- "XXX"  # Placeholder ----------------------------------------------------------------------------------to be fixed
        }
        
        # Extract sample name
        sample <- strsplit(strip_value(input$exp_name), "_")[[1]][1]
        if (is.na(sample) || sample == "") sample <- "UNKNOWN"
        
        # Clean all metadata components
        technology <- tolower(strip_value(input$technology))
        platform <- tolower(strip_value(input$platform_name))
        caller <- tolower(strip_value(input$caller_name))
        truthset <- tolower(strip_value(input$truth_set_name))
        
        # Build filename matching Python format: {id:03d}_{sample}_{technology}_{platform}_{caller}_{truthset}.csv
        filename <- paste0(preview_id, "_", sample, "_", technology, "_", platform, "_", caller, "_", truthset, ".csv")
        
        return(paste("Preview:", filename))
        
      }, error = function(e) {
        return("Error generating preview")
      })
      
    } else {
      return("Fill required fields (*) to see filename preview")
    }
  })
  
  # Main upload submission observer
  observeEvent(input$submit_upload, {
    
    # Basic validation checks
    if (is.null(input$upload_file)) {
      showNotification("Please select a file to upload", type = "error", duration = 5)
      return()
    }
    
    # Check required fields
    required_fields <- list(
      exp_name = input$exp_name,
      technology = input$technology,
      platform_name = input$platform_name,
      caller_name = input$caller_name
    )
    
    missing_fields <- names(required_fields)[sapply(required_fields, function(x) is.null(x) || x == "")]
    
    if (length(missing_fields) > 0) {
      showNotification(
        paste("Please fill required fields:", paste(missing_fields, collapse = ", ")), 
        type = "error", 
        duration = 6
      )
      return()
    }
    
    # Show loading notification
    loading_id <- showNotification(
      " Processing upload... Please wait", 
      type = "message", 
      duration = NULL,
      closeButton = FALSE
    )
    
    # Prepare complete metadata JSON
    tryCatch({
      
      metadata_json <- jsonlite::toJSON(list(
        # REQUIRED FIELDS
        exp_name = safe_input(input$exp_name),
        technology = safe_input(input$technology),
        platform_name = safe_input(input$platform_name),
        caller_name = safe_input(input$caller_name),
        
        # BASIC INFO
        description = safe_input(input$description),
        target = safe_input(input$target, "wgs"),
        
        # SEQUENCING PLATFORM
        platform_type = safe_input(input$platform_type),
        platform_version = safe_input(input$platform_version),
        
        # CHEMISTRY
        chemistry_name = safe_input(input$chemistry_name),
        chemistry_version = safe_input(input$chemistry_version),
        
        # VARIANT CALLER
        caller_type = safe_input(input$caller_type),
        caller_version = safe_input(input$caller_version),
        caller_model = safe_input(input$caller_model),
        
        # ALIGNER
        aligner_name = safe_input(input$aligner_name),
        aligner_version = safe_input(input$aligner_version),
        
        # TRUTH SET
        truth_set_name = safe_input(input$truth_set_name, "giab"),
        truth_set_sample = safe_input(input$truth_set_sample, "hg002"),
        truth_set_version = safe_input(input$truth_set_version, "4.2.1"),
        truth_set_reference = safe_input(input$truth_set_reference, "grch38"),
        
        # VARIANT INFO
        variant_type = safe_input(input$variant_type, "snp+indel"),
        variant_size = safe_input(input$variant_size, "small"),
        variant_origin = safe_input(input$variant_origin, "germline"),
        is_phased = safe_input(input$is_phased, "false"),
        
        # BENCHMARK TOOL
        benchmark_tool_name = safe_input(input$benchmark_tool_name, "hap.py"),
        benchmark_tool_version = safe_input(input$benchmark_tool_version, "0.3.12"),
        
        # QUALITY METRICS
        mean_coverage = safe_numeric(input$mean_coverage),
        read_length = safe_numeric(input$read_length),
        mean_insert_size = safe_numeric(input$mean_insert_size),
        mean_read_length = safe_numeric(input$mean_read_length)
        
      ), auto_unbox = TRUE)
      
      # Print metadata 
      cat(" Metadata JSON:\n", metadata_json, "\n")
      
      # Call Python upload handler
      result <- upload_handler$upload_experiment(
        file_path = input$upload_file$datapath,
        metadata_json = metadata_json
      )
      
      # Remove loading notification
      removeNotification(loading_id)
      
      # Handle result
      if (result$success) {
        # Success notification
        showNotification(
          HTML(paste(" Upload Successful!<br>", result$message)), 
          type = "message", 
          duration = 8
        )
        
        # Close modal
        toggleModal(session, "upload_modal", toggle = "close")
        
        # Refresh data tables
        return(TRUE)
        
      } else {
        # Error notification
        showNotification(
          HTML(paste("Upload Failed<br>", result$message)), 
          type = "error", 
          duration = 12
        )
        
        return(FALSE)
      }
      
    }, error = function(e) {
      # Remove loading notification
      removeNotification(loading_id)
      
      # Show error
      showNotification(
        paste("Upload Error:", e$message), 
        type = "error", 
        duration = 10
      )
      
      #  error details
      cat("Upload Error Details:\n")
      print(e)
      
      return(FALSE)
    })
  })
  
  return(list(
    upload_success = reactive({ input$submit_upload })  # For triggering refreshes
  ))
}