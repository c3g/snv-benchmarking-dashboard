# ============================================================================
# auth.R
# ============================================================================

library(reticulate)

# Import Python modules
tryCatch({
  py_run_string("import sys")
  py_run_string("sys.path.append('../backend')") 
  auth <<- import("auth")
  user_interface <<- import("user_data_interface")
}, error = function(e) {
  stop("Cannot connect to Python backend: ", e$message)
})

#' Authenticate user credentials
authenticate_user <- function(username, password) {
  tryCatch({
    if (is.null(username) || is.null(password) || username == "" || password == "") {
      return(list(success = FALSE, message = "Please enter username and password"))
    }
    
    result <- auth$authenticate_user(username, password)
    return(result)
    
  }, error = function(e) {
    return(list(success = FALSE, message = paste("Authentication error:", e$message)))
  })
}

#' Create new user
create_user <- function(username, email, password) {
  tryCatch({
    if (is.null(username) || is.null(email) || is.null(password) || 
        username == "" || email == "" || password == "") {
      return(list(success = FALSE, message = "Please fill in all fields"))
    }
    
    result <- auth$create_user(username, email, password, "user")
    return(result)
  }, error = function(e) {
    return(list(success = FALSE, message = paste("Error:", e$message)))
  })
}

#' Upload user experiment
upload_user_experiment <- function(user_id, file_path, experiment_name) {
  tryCatch({
    result <- user_interface$upload_user_experiment(
      user_id = as.integer(user_id),
      file_path = file_path,
      experiment_name = experiment_name,
      metadata = list(),
      description = ""
    )
    return(result)
  }, error = function(e) {
    return(list(success = FALSE, message = paste("Upload error:", e$message)))
  })
}

#' Get user experiments
get_user_experiments <- function(user_id) {
  tryCatch({
    result <- user_interface$get_user_experiments(as.integer(user_id))
    if (result$success) {
      return(result$data)
    } else {
      return(data.frame())
    }
  }, error = function(e) {
    warning(paste("Error getting user experiments:", e$message))
    return(data.frame())
  })
}