# ============================================================================
# celery_server.R
# ============================================================================
# Server logic for Flask/Celery pipeline integration.
# job submission, status polling, and result retrieval.

# ============================================================================
# PYTHON CLIENT SETUP
# ============================================================================

# Import job_client module (call this in app.R after other py imports)
setup_celery_client <- function() {
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
    polling = FALSE
  )
  
  # ==========================================================================
  # SUBMIT BUTTON HANDLER
  # ==========================================================================
  # Triggered when user clicks "Test Celery" button.
  # Sends file to Flask API and starts polling loop.
  
  observeEvent(input$celery_submit, {
    req(input$celery_file)
    
    # Update UI state
    celery_state$status <- "SUBMITTING..."
    celery_state$result <- NULL
    
    # Call Python client to POST file to Flask
    result <- tryCatch({
      py_to_r(job_client$submit_file(input$celery_file$datapath))
    }, error = function(e) {
      list(error = e$message)
    })
    
    # Handle response
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
    
    # Check current status
    status <- tryCatch({
      py_to_r(job_client$check_status(celery_state$job_id))
    }, error = function(e) {
      list(status = "ERROR", error = e$message)
    })
    
    celery_state$status <- status$status
    
    if (status$status == "SUCCESS") {
      # Job complete - fetch result
      celery_state$polling <- FALSE
      result <- py_to_r(job_client$get_result(celery_state$job_id))
      
      celery_state$result <- if (result$status == "SUCCESS") {
        paste("Rows:", result$rows_processed, "\n", 
              substr(result$csv_content, 1, 300))
      } else {
        paste("Error:", result$error)
      }
      showNotification("Done!", type = "message")
      
    } else if (status$status == "FAILURE") {
      # Job failed
      celery_state$polling <- FALSE
      celery_state$result <- status$error
      
    } else {
      # Still running - poll again in 2 seconds
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
}