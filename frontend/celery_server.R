# ============================================================================
# celery_server.R
# ============================================================================
# Server logic for Flask/Celery pipeline integration.
# Job submission, status polling, and result retrieval.

# ============================================================================
# CONFIG
# ============================================================================

CELERY_RESULTS_FOLDER <- "celery_results"

# ============================================================================
# PYTHON CLIENT SETUP
# ============================================================================

setup_celery_client <- function() {
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
  
  celery_state <- reactiveValues(
    job_id     = NULL,
    status     = "IDLE",
    result     = NULL,
    saved_path = NULL,
    polling    = FALSE
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
  # POLLING LOOP — every 2s while polling=TRUE
  # ==========================================================================
  
  observe({
    req(celery_state$polling, celery_state$job_id)
    
    status <- tryCatch({ 
      py_to_r(job_client$check_status(celery_state$job_id))
    }, error = function(e) {
      list(status = "ERROR", error = e$message)
    })
    
    celery_state$status <- status$status
    
    if (status$status == "SUCCESS") {
      celery_state$polling <- FALSE
      result <- py_to_r(job_client$get_result(celery_state$job_id))
      
      if (result$status == "SUCCESS") {
        # Copy downloaded VCF from /tmp to results folder
        src_path  <- result$result_local_path
        base_name <- result$metadata$original_filename %||% celery_state$job_id
        filename  <- paste0(base_name, "_result.csv")
        dest_path <- file.path(CELERY_RESULTS_FOLDER, filename)
        
        tryCatch({
          file.copy(src_path, dest_path, overwrite = TRUE)
          celery_state$saved_path <- dest_path
          celery_state$result <- paste("Saved to:", dest_path)
          showNotification(paste("Result saved:", dest_path), type = "message")
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
  
  return(list(
    saved_path = reactive({ celery_state$saved_path })
  ))
}