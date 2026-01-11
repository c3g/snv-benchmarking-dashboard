# ============================================================================
# auth.R - OpenID Connect (OIDC) Authentication - COManage
# ============================================================================
# Handles user authentication via OIDC provider and syncs users to database.
#
# Key changes for private uploads:
# - Syncs user to database on login via user_management.py
# - Stores user_id in session for ownership tracking
# - Passes user context to data queries for visibility filtering

library(httr2)
library(jose)
library(jsonlite)
library(logger)
library(reticulate)

log_threshold(INFO)

# ============================================================================
# CONFIGURATION
# ============================================================================

OIDC_ISSUER <- trimws(Sys.getenv("OIDC_ISSUER", ""))
OIDC_CLIENT_ID <- trimws(Sys.getenv("OIDC_CLIENT_ID", ""))
OIDC_CLIENT_SECRET <- trimws(Sys.getenv("OIDC_CLIENT_SECRET", ""))
OIDC_REDIRECT_URI <- trimws(Sys.getenv("OIDC_REDIRECT_URI", ""))

OIDC_ENABLED <- (OIDC_ISSUER != "" && OIDC_CLIENT_ID != "" && 
                 OIDC_CLIENT_SECRET != "" && OIDC_REDIRECT_URI != "")

log_info("OIDC config loaded - enabled: {OIDC_ENABLED}")
log_info("OIDC redirect URI: {OIDC_REDIRECT_URI}")
log_info("OIDC issuer: {OIDC_ISSUER}")
if (OIDC_CLIENT_ID != "") {
  log_info("Client ID configured: {substr(OIDC_CLIENT_ID, 1, 8)}...")
}

# ============================================================================
# PYTHON USER MANAGEMENT INTERFACE
# ============================================================================

# Import user management module (initialized lazily)
user_mgmt <- NULL

get_user_management <- function() {
  if (is.null(user_mgmt)) {
    tryCatch({
      user_mgmt <<- import("user_management")
      log_info("User management module loaded")
    }, error = function(e) {
      log_error("Failed to load user_management module: {e$message}")
      return(NULL)
    })
  }
  return(user_mgmt)
}

sync_user_to_database <- function(username, email, full_name, is_admin) {
  #'
  #' Sync user to database on login.
  #' Creates new user or updates existing user's last_login and admin status.
  #'
  #' @param username OIDC username (eppn or sub)
  #' @param email User email
  #' @param full_name Display name

  #' @param is_admin Admin status from OIDC groups
  #' @return list with user_id, username, is_new, success
  #'
  
  mgmt <- get_user_management()
  
  if (is.null(mgmt)) {
    log_error("Cannot sync user - user_management module not available")
    return(list(
      user_id = NULL,
      username = username,
      is_new = FALSE,
      success = FALSE,
      error = "User management module not available"
    ))
  }
  
  tryCatch({
    result <- mgmt$get_or_create_user(
      username = username,
      email = email,
      full_name = full_name,
      is_admin = is_admin
    )
    
    if (result$success) {
      log_info("User synced to database: {username} (ID: {result$user_id}, new: {result$is_new})")
    } else {
      log_error("User sync failed: {result$error}")
    }
    
    return(result)
    
  }, error = function(e) {
    log_error("Error syncing user to database: {e$message}")
    return(list(
      user_id = NULL,
      username = username,
      is_new = FALSE,
      success = FALSE,
      error = e$message
    ))
  })
}

# ============================================================================
# OIDC CONFIGURATION FETCH
# ============================================================================

get_oidc_config <- function() {
  if (!OIDC_ENABLED) {
    log_warn("OIDC disabled, skipping config fetch")
    return(NULL)
  }
  
  tryCatch({
    url <- paste0(OIDC_ISSUER, "/.well-known/openid-configuration")
    log_info("Fetching OIDC discovery from: {url}")
    
    config <- request(url) %>% req_perform() %>% resp_body_json()
    
    log_success("OIDC config retrieved")
    log_debug("Auth endpoint: {config$authorization_endpoint}")
    log_debug("Token endpoint: {config$token_endpoint}")
    
    return(config)
  }, error = function(e) {
    log_error("Failed to fetch OIDC config")
    log_error("URL: {paste0(OIDC_ISSUER, '/.well-known/openid-configuration')}")
    log_error("Error: {e$message}")
    return(NULL)
  })
}

OIDC_CONFIG <- get_oidc_config()

if (!is.null(OIDC_CONFIG)) {
  log_success("OIDC provider ready")
} else {
  log_warn("OIDC provider config unavailable")
}

# ============================================================================
# ADMIN CONFIGURATION
# ============================================================================

is_admin <- function(user_group) {
  if (is.null(user_group)) return(FALSE)
  if (is.na(user_group)) return(FALSE)
  if (length(user_group) == 0) return(FALSE)
  if (nchar(as.character(user_group)) == 0) return(FALSE)
  
  tolower(as.character(user_group)) == "admin"
}

# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

random_string <- function() {
  paste0(sample(c(letters, LETTERS, 0:9), 32, replace = TRUE), collapse = "")
}

is_authenticated <- function(session) {
  !is.null(session$userData$user_email)
}

get_user_info <- function(session) {
  #'
  #' Get current user information from session.
  #' Includes database user_id for ownership tracking.
  #'
  #' @param session Shiny session object
  #' @return list with email, name, username, group, is_admin, user_id
  #'
  
  if (!is_authenticated(session)) return(NULL)
  
  user_group <- session$userData$user_group
  
  if (is.null(user_group) || is.na(user_group) || length(user_group) == 0) {
    user_group <- NULL
  } else if (nchar(as.character(user_group)) == 0) {
    user_group <- NULL
  }
  
  admin_status <- is_admin(user_group)
  
  list(
    email = session$userData$user_email, 
    name = session$userData$user_name, 
    username = session$userData$user_username,
    group = user_group,
    is_admin = admin_status,
    user_id = session$userData$user_id  # Database user ID for ownership
  )
}

create_login_url <- function(state) {
  if (is.null(OIDC_CONFIG)) {
    log_error("Cannot build login URL - OIDC_CONFIG is NULL")
    return(NULL)
  }
  
  params <- paste0(
    "client_id=", OIDC_CLIENT_ID,
    "&redirect_uri=", URLencode(OIDC_REDIRECT_URI, reserved = TRUE),
    "&response_type=code",
    "&scope=openid+profile+email+org.cilogon.userinfo",
    "&state=", state
  )
  
  login_url <- paste0(OIDC_CONFIG$authorization_endpoint, "?", params)
  
  return(login_url)
}

# ============================================================================
# TOKEN EXCHANGE
# ============================================================================

get_user_from_code <- function(code) {
  #'
  #' Exchange authorization code for user info.
  #' Decodes JWT token to extract user claims.
  #'
  
  if (is.null(OIDC_CONFIG)) {
    log_error("OIDC_CONFIG is NULL, cannot exchange code")
    return(NULL)
  }

  log_info("Token exchange started")

  tryCatch({
    token_response <- request(OIDC_CONFIG$token_endpoint) %>%
      req_body_form(
        grant_type = "authorization_code",
        code = code,
        redirect_uri = OIDC_REDIRECT_URI,
        client_id = OIDC_CLIENT_ID,
        client_secret = OIDC_CLIENT_SECRET
      ) %>%
      req_perform() %>%
      resp_body_json()
    
    log_success("Token response received")
    log_info("Response keys: {paste(names(token_response), collapse=', ')}")

    
    if (is.null(token_response$id_token)) {
      log_error("No id_token in response")
      log_error("Response keys: {paste(names(token_response), collapse=', ')}")

      return(list(success = FALSE, error = "No id_token in token response"))
    }
    log_info("ID token present - length: {nchar(token_response$id_token)} chars")

    
    token_parts <- strsplit(token_response$id_token, "\\.")[[1]]
    
    if (length(token_parts) < 2) {
      log_error("Invalid JWT format")
      return(list(success = FALSE, error = "Invalid JWT token format"))
    }
    
    claims_json <- rawToChar(jose::base64url_decode(token_parts[2]))
    claims <- jsonlite::fromJSON(claims_json)
    
    log_success("Claims decoded")
    log_info("All claims:\n{paste(names(claims), '=', claims, collapse='\n')}")
    log_info("User email: {claims$email %||% 'NULL'}")
    log_info("User name: {claims$name %||% 'NULL'}")
    log_info("User eppn: {claims$eppn %||% 'NULL'}")
    log_info("User sub: {claims$sub %||% 'NULL'}")
    
    if(is.null(claims$groups)) {
      log_info("No groups claim found")
    } else {
      log_info("Groups found: {length(claims$groups)}")
      log_info("Groups : {(claims$groups)}")
      for(i in seq_along(claims$groups)) {
        log_debug("  Group {i}: {claims$groups[i]}")
      }
    }
    
    # Filter for dashboard groups
    dashboard_group <- NULL
    if (!is.null(claims$groups) && length(claims$groups) > 0) {
      matching_groups <- claims$groups[grepl("snv-benchmarking-dashboard", claims$groups)]
      
      if (length(matching_groups) > 0) {
        if (any(grepl("admins", matching_groups))) {
          dashboard_group <- "admin"
        } else if (any(grepl("approvers", matching_groups))) {
          dashboard_group <- "approver"
        } else if (any(grepl("members", matching_groups))) {
          dashboard_group <- "member"
        }
      }
    }
    
    log_info("Dashboard role assigned: {dashboard_group %||% 'none'}")

    result <- list(
      email = claims$email,
      name = claims$name %||% claims$given_name %||% claims$email,
      username = claims$eppn %||% claims$sub,
      group = dashboard_group,
      success = TRUE
    )
    
    log_success("Token exchange complete - user: {result$username}")
    return(result)
    
  }, error = function(e) {
    log_error("TOKEN EXCHANGE FAILED")
    log_error("Error class: {paste(class(e), collapse=', ')}")
    log_error("Error message: {e$message}")
    
    if (inherits(e, "httr2_http")) {
      tryCatch({
        log_error("HTTP status: {e$status}")
      if (!is.null(e$body)) {
          log_error("Response body:")
          log_error(paste(capture.output(print(e$body)), collapse = "\n"))
        }
      }, error = function(e2) {
        log_warn("Couldn't extract HTTP details: {e2$message}")
      })
    }
    
    return(list(success = FALSE, error = e$message))
  })
}

# ============================================================================
# AUTHENTICATION COMPLETION
# ============================================================================

complete_authentication <- function(session, result, authenticated) {
  #'
  #' Complete authentication after successful OIDC flow.
  #' 
  #' Key steps:
  #' 1. Store user info in session
  #' 2. Sync user to database (creates or updates user record)
  #' 3. Store database user_id in session for ownership tracking
  #' 4. Update localStorage for session persistence
  #'
  
  # Store basic user info in session
  session$userData$user_email <- result$email
  session$userData$user_name <- result$name
  session$userData$user_username <- result$username %||% result$email
  session$userData$user_group <- result$group
  
  # Determine admin status
  admin_status <- is_admin(result$group)
  
  # SYNC USER TO DATABASE
  db_result <- sync_user_to_database(
    username = result$username %||% result$email,
    email = result$email,
    full_name = result$name,
    is_admin = admin_status
  )
  
  # Store database user_id in session
  if (db_result$success && !is.null(db_result$user_id)) {
    session$userData$user_id <- db_result$user_id
    log_info("User ID stored in session: {db_result$user_id}")
  } else {
    session$userData$user_id <- NULL
    log_warn("Could not store user_id - database sync failed")
  }
  
  # Set authenticated state
  authenticated(TRUE)
  
  # Prepare localStorage data
  user_data_list <- list(
    email = result$email,
    name = result$name,
    username = result$username %||% result$email,
    user_id = db_result$user_id  # Include user_id for session restoration
  )
  
  if (!is.null(result$group) && !is.na(result$group)) {
    user_data_list$group <- result$group
  }
  
  user_json <- jsonlite::toJSON(user_data_list, auto_unbox = TRUE, null = "null")
  
  runjs(sprintf("localStorage.setItem('user_session', '%s');", 
                gsub("'", "\\\\'", user_json)))
  
  runjs("sessionStorage.removeItem('auth_state');")
  updateQueryString("?", mode = "replace")
  showNotification(paste("Welcome", result$name), type = "message")
  
  log_success("Authentication completed - user: {result$name} (DB ID: {db_result$user_id})")
}

clear_session_data <- function(session, authenticated) {
  session$userData$user_email <- NULL
  session$userData$user_name <- NULL
  session$userData$user_username <- NULL
  session$userData$user_group <- NULL
  session$userData$user_id <- NULL
  session$userData$auth_state <- NULL
  authenticated(FALSE)
  
  runjs("localStorage.removeItem('user_session');")
  runjs("sessionStorage.clear();")
  
  log_info("Session cleared")
}

# ============================================================================
# UI COMPONENT
# ============================================================================

auth_ui <- function() {
  tagList(
    tags$script(HTML("
      // Handle /callback route
      if (window.location.pathname === '/callback' || window.location.pathname === '/callback/') {
        const newUrl = window.location.origin + '/' + window.location.search;
        window.history.replaceState({}, '', newUrl);
      }
    ")),
    uiOutput("auth_status")
  )
}

# ============================================================================
# SERVER LOGIC
# ============================================================================

auth_server <- function(input, output, session) {
  
  log_info("Auth server initialized - session: {session$token}")
  
  authenticated <- reactiveVal(FALSE)
  
  observeEvent(session$clientData$url_hostname, {
    log_info("Client connected: {session$clientData$url_hostname}")
  }, once = TRUE)
  
  observe({
    invalidateLater(10000)
    if (authenticated()) {
      log_debug("Session alive - ID: {substr(session$token, 1, 12)}, authenticated: TRUE")
    }
  })
  
  observe(priority = 1000, {
    runjs("Shiny.setInputValue('stored_user_session', localStorage.getItem('user_session'), {priority: 'event'});")
  })
  
  # Handle retrieved localStorage value - restore session
  observeEvent(input$stored_user_session, {
    user_json <- input$stored_user_session
        
    if (!is.null(user_json) && user_json != "" && user_json != "null") {
      tryCatch({
        user_data <- jsonlite::fromJSON(user_json)
        log_info("Parsed localStorage data - email: {user_data$email %||% 'NULL'}")
                
        if (is.null(user_data$email) || is.null(user_data$name)) {
          log_warn("Invalid user data in localStorage")
          runjs("localStorage.removeItem('user_session');")
          return()
        }
        
        session$userData$user_email <- user_data$email
        session$userData$user_name <- user_data$name
        session$userData$user_username <- user_data$username %||% user_data$email
        session$userData$user_group <- user_data$group
        
        # Restore user_id from localStorage or re-sync from database
        if (!is.null(user_data$user_id) && !is.na(user_data$user_id)) {
          session$userData$user_id <- user_data$user_id
          log_info("User ID restored from localStorage: {user_data$user_id}")
        } else {
          # Re-sync to get user_id
          admin_status <- is_admin(user_data$group)
          db_result <- sync_user_to_database(
            username = user_data$username %||% user_data$email,
            email = user_data$email,
            full_name = user_data$name,
            is_admin = admin_status
          )
          if (db_result$success) {
            session$userData$user_id <- db_result$user_id
            log_info("User ID re-synced from database: {db_result$user_id}")
          }
        }
        
        authenticated(TRUE)
        
        log_info("Session restored for: {user_data$name} (ID: {session$userData$user_id})")
      }, error = function(e) {
        log_error("Failed to restore session: {e$message}")
        runjs("localStorage.removeItem('user_session');")
      })
    } else {
      log_info("No saved session in localStorage")
    }
  }, ignoreNULL = FALSE, ignoreInit = FALSE)
  
  session$onSessionEnded(function() {
    session$userData$user_email <- NULL
    session$userData$user_name <- NULL
    session$userData$user_username <- NULL
    session$userData$user_group <- NULL
    session$userData$user_id <- NULL
    session$userData$auth_state <- NULL
    authenticated(FALSE)
  })
  
  # Render login/logout button
  output$auth_status <- renderUI({
    auth_state <- authenticated()
    
    tryCatch({
      if (is_authenticated(session)) {
        user <- get_user_info(session)
        
        if (is.null(user)) {
          log_warn("User authenticated but get_user_info returned NULL")
          return(actionButton(
            "login_btn", 
            "Sign In", 
            class = "btn-primary btn-sm", 
            icon = icon("sign-in-alt"), 
            style = "font-size: 13px; padding: 6px 16px;"
          ))
        }
        
        div(
          style = "display: flex; gap: 8px; align-items: center; background-color: #f8f9fa; padding: 6px 22px; border-radius: 4px; border: 1px solid #e4e7ea;",
          span(
            style = "font-size: 13px; color: #4472ca; font-weight: 500; display: flex; align-items: center; gap: 5px; white-space: nowrap;",
            icon("user"),
            span(user$name %||% "User")
          ),
          actionButton(
            "logout_btn", 
            "Sign Out",
            class = "btn-sm",
            style = "font-size: 13px; padding: 6px 12px; background-color: #ffffff; border: 1px solid #d1d5db; color: #556b78; font-weight: 500; min-width: 80px;"
          )
        )
      } else {
        actionButton(
          "login_btn", 
          "Sign In", 
          class = "btn-primary btn-sm", 
          icon = icon("sign-in-alt"), 
          style = "font-size: 13px; padding: 6px 16px;"
        )
      }
    }, error = function(e) {
      log_error("Error in auth_status renderUI: {e$message}")
      actionButton(
        "login_btn", 
        "Sign In", 
        class = "btn-primary btn-sm", 
        icon = icon("sign-in-alt"), 
        style = "font-size: 13px; padding: 6px 16px;"
      )
    })
  })
  
  # Handle login button click
  observeEvent(input$login_btn, {
    log_info("=== LOGIN INITIATED ===")
    if (!OIDC_ENABLED) {
      log_error("Login attempted but OIDC not configured")
      showNotification("Authentication not configured", type = "warning", duration = 5)
      return()
    }
    
    # Generate state token
    state <- random_string()
    session$userData$auth_state <- state
    
    runjs(sprintf("sessionStorage.setItem('auth_state', '%s');", state))
    log_info("State stored in browser sessionStorage")

    # Build and redirect
    login_url <- create_login_url(state)
    if (!is.null(login_url)) {
      log_info("Redirecting to OIDC provider")
      runjs(sprintf("window.location.href = '%s';", login_url))
    } else {
      log_error("Failed to create login URL")
      showNotification("Authentication service unavailable", type = "error")
    }
  })
  
  # Handle OIDC callback
  observe({
    query <- parseQueryString(session$clientData$url_search)
    
    if (!is.null(query$code) && !is.null(query$state)) {
      log_info("=== OAUTH CALLBACK RECEIVED ===")
      log_info("New session ID: {session$token}")
      log_info("Full callback URL: {session$clientData$url_search}")
      log_info("Callback query string length: {nchar(session$clientData$url_search)}")
      log_info("Authorization code (full): {query$code}")
      log_info("Authorization code length: {nchar(query$code)}")
      log_info("State from callback (full): {query$state}")
      log_info("State from callback length: {nchar(query$state)}")
      
      stored_state <- session$userData$auth_state
    
      if (is.null(stored_state)) {
        log_warn("State not found, attempting sessionStorage recovery")
        runjs("Shiny.setInputValue('recovered_state', sessionStorage.getItem('auth_state'), {priority: 'event'});")
        return()
      }
      
      if (query$state != stored_state) {
        log_error("State mismatch - security check failed")
        updateQueryString("?", mode = "replace")
        showNotification("Login failed - security check failed", type = "error")
        return()
      }
      
      log_info("State validated")
      
      # Exchange code
      result <- get_user_from_code(query$code)
      
      if (!is.null(result) && result$success) {
        log_success("Token exchange succeeded, completing auth")

        complete_authentication(session, result, authenticated)
      } else {
        log_error("Token exchange failed")
        log_error("Result: {paste(capture.output(str(result)), collapse=' ')}")

        showNotification("Login failed", type = "error")
        authenticated(FALSE)
        updateQueryString("?", mode = "replace")
      }
    }
  })
  
  # Handle recovered state from sessionStorage
  observe({
    req(input$recovered_state)
    
    recovered_state <- input$recovered_state
    query <- parseQueryString(session$clientData$url_search)
    
    if (!is.null(query$code) && !is.null(query$state)) {
      if (query$state == recovered_state) {
        log_info("State recovered from sessionStorage")
        
        result <- get_user_from_code(query$code)
        
        if (!is.null(result) && result$success) {
          complete_authentication(session, result, authenticated)
        } else {
          log_error("Token exchange failed with recovered state")
          showNotification("Login failed", type = "error")
          authenticated(FALSE)
          updateQueryString("?", mode = "replace")
        }
      } else {
        log_error("Recovered state mismatch")
        updateQueryString("?", mode = "replace")
      }
    }
  })
  
  # Handle logout
  observeEvent(input$logout_btn, {
    clear_session_data(session, authenticated)
    showNotification("Signed out", type = "message")
  })
  
  # ====================================================================
  # AUTHORIZATION OUTPUTS
  # ====================================================================
  
  output$user_authenticated <- reactive({
    authenticated()
  })
  outputOptions(output, "user_authenticated", suspendWhenHidden = FALSE)
  
  output$user_is_admin <- reactive({
    tryCatch({
      auth_state <- authenticated()
      
      if (!auth_state) return(FALSE)
      
      user <- get_user_info(session)
      
      if (is.null(user)) return(FALSE)
      
      if (is.null(user$is_admin) || is.na(user$is_admin)) return(FALSE)
      
      return(user$is_admin)
      
    }, error = function(e) {
      log_error("Error in user_is_admin reactive: {e$message}")
      return(FALSE)
    })
  })
  outputOptions(output, "user_is_admin", suspendWhenHidden = FALSE)
  
  return(authenticated)
}