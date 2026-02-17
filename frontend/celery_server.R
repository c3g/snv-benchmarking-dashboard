# ============================================================================
# celery_server.R
# ============================================================================
# Server logic for Flask/Celery pipeline integration.
# job submission, status polling, and result retrieval.

# ============================================================================
# CONFIG
# ============================================================================

# Folder to save received CSVs
CELERY_RESULTS_FOLDER <- "celery_results"

# ============================================================================
# PYTHON CLIENT SETUP
# ============================================================================

setup_celery_client <- function() {
  # Create results folder if needed
  if (!dir.exists(CELERY_RESULTS_FOLDER)) {
    dir.create(CELERY_RESULTS_FOLDER, recursive = TRUE)
  }
  
  tryCatch({
    py_run_string("import sys; sys.path.insert(0, '.')")
    job_client <<- import("job_client")
    message("Celery job client loaded")
  }, error = function(e) {
    message("Failed to load job_client: ", e$message)
    job_client <<- NULL
  })
}

# ============================================================================
# SERVER MODULE
# ============================================================================

setup_celery_observers <- function(input, output, session) {
  
  # State for tracking job progress
  celery_state <- reactiveValues(
    job_id = NULL,
    status = "IDLE",
    result = NULL,
    saved_path = NULL,
    polling = FALSE
  )
  
  # ==========================================================================
  # SUBMIT BUTTON HANDLER
  # ==========================================================================
  
  observeEvent(input$celery_submit, {
    req(input$celery_file)
    
    celery_state$status <- "SUBMITTING..."
    celery_state$result <- NULL
    celery_state$saved_path <- NULL
    
    result <- tryCatch({
      py_to_r(job_client$submit_file(input$celery_file$datapath))
    }, error = function(e) {
      list(error = e$message)
    })
    
    if (!is.null(result$job_id)) {
      celery_state$job_id <- result$job_id
      celery_state$status <- "QUEUED"
      celery_state$polling <- TRUE
      showNotification(paste("Submitted:", result$job_id), type = "message")
    } else {
      celery_state$status <- "ERROR"
      celery_state$result <- result$error
      showNotification(paste("Failed:", result$error), type = "error")
    }
  })
  
  # ==========================================================================
  # POLLING LOOP
  # ==========================================================================
    # Runs every 2 seconds while polling=TRUE.
  # Checks job status and fetches result when complete.
  observe({
    req(celery_state$polling, celery_state$job_id)
    
    status <- tryCatch({ # check current status
      py_to_r(job_client$check_status(celery_state$job_id))
    }, error = function(e) {
      list(status = "ERROR", error = e$message)
    })
    
    celery_state$status <- status$status
    
    if (status$status == "SUCCESS") {
      celery_state$polling <- FALSE
      result <- py_to_r(job_client$get_result(celery_state$job_id))
      
      if (result$status == "SUCCESS") {
        # Save CSV to file
        filename <- paste0("result_", celery_state$job_id, ".csv")
        filepath <- file.path(CELERY_RESULTS_FOLDER, filename)
        
        tryCatch({
          writeLines(result$csv_content, filepath)
          celery_state$saved_path <- filepath
          
          celery_state$result <- paste(
            "Rows:", result$rows_processed,
            "\nSaved to:", filepath,
            "\n\nPreview:\n", substr(result$csv_content, 1, 200)
          )
          showNotification(paste("Saved:", filepath), type = "message")
        }, error = function(e) {
          celery_state$result <- paste("Got result but failed to save:", e$message)
        })
        
      } else {
        celery_state$result <- paste("Error:", result$error)
      }
      
    } else if (status$status == "FAILURE") {
      celery_state$polling <- FALSE
      celery_state$result <- status$error
      
    } else {
      invalidateLater(2000, session)
    }
  })
  
  # ==========================================================================
  # UI OUTPUTS
  # ==========================================================================
  
  output$celery_job_id <- renderText({
    celery_state$job_id %||% "None"
  })
  
  output$celery_status <- renderText({
    celery_state$status
  })
  
  output$celery_result <- renderText({
    celery_state$result %||% ""
  })
  
  # Return saved path for other modules to use
  return(list(
    saved_path = reactive({ celery_state$saved_path })
  ))
}