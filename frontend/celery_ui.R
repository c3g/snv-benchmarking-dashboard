# ============================================================================
# celery_ui.R
# ============================================================================
# UI components for Flask/Celery pipeline integration.
# Source this in app.R and add celery_test_panel_ui() to the sidebar.

# ============================================================================
# UI PANEL
# ============================================================================

celery_test_panel_ui <- function() {
  tagList(
    hr(),
    h4("Pipeline Test:"),
    fileInput("celery_file", "CSV File:", accept = ".csv", width = "100%"),
    actionButton("celery_submit", "Test Celery", 
                 class = "btn-warning", style = "width: 100%;"),
    
    div(
      style = "margin-top: 10px; padding: 10px; background: #f8f9fa; 
               border-radius: 4px; font-size: 12px;",
      div(tags$b("Job: "), textOutput("celery_job_id", inline = TRUE)),
      div(tags$b("Status: "), textOutput("celery_status", inline = TRUE)),
      verbatimTextOutput("celery_result")
    )
  )
}