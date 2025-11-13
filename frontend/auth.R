# ============================================================================
# auth.R - OpenID Connect (OIDC) Authentication - COManage
# ============================================================================
# Handles user authentication via OIDC provider

library(httr2)
library(jose)
library(jsonlite)
library(logger)

log_threshold(INFO)

# ============================================================================
# CONFIGURATION
# ============================================================================

OIDC_ISSUER <- Sys.getenv("OIDC_ISSUER", "")
OIDC_CLIENT_ID <- Sys.getenv("OIDC_CLIENT_ID", "")
OIDC_CLIENT_SECRET <- Sys.getenv("OIDC_CLIENT_SECRET", "")
OIDC_REDIRECT_URI <- Sys.getenv("OIDC_REDIRECT_URI", "")

OIDC_ENABLED <- (OIDC_ISSUER != "" && OIDC_CLIENT_ID != "" && 
                 OIDC_CLIENT_SECRET != "" && OIDC_REDIRECT_URI != "")

log_info("OIDC config loaded - enabled: {OIDC_ENABLED}")
log_info("OIDC redirect URI: {OIDC_REDIRECT_URI}")
log_info("OIDC issuer: {OIDC_ISSUER}")
if (OIDC_CLIENT_ID != "") {
  log_info("Client ID configured: {substr(OIDC_CLIENT_ID, 1, 8)}...")
}


# get OIDC provider config
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
    is_admin = admin_status
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
  log_info("Built login URL with redirect_uri: {OIDC_REDIRECT_URI}")
  log_debug("Login URL length: {nchar(login_url)} chars")
  
  return(login_url)
}

# Exchange authorization code for user info
get_user_from_code <- function(code) {
  if (is.null(OIDC_CONFIG)) {
    log_error("OIDC_CONFIG is NULL, cannot exchange code")
    return(NULL)
  }

  log_info("=== TOKEN EXCHANGE START ===")
  log_info("Token endpoint: {OIDC_CONFIG$token_endpoint}")
  log_info("Redirect URI in env var: {OIDC_REDIRECT_URI}")
  log_info("Redirect URI length: {nchar(OIDC_REDIRECT_URI)}")
  log_info("Client ID: {OIDC_CLIENT_ID}")
  log_info("Auth code: {code}")
  log_info("Auth code length: {nchar(code)}")

  tryCatch({
    log_info("Building token request...")
    log_info("Request parameters:")
    log_info("  grant_type: authorization_code")
    log_info("  code: {substr(code, 1, 20)}...")
    log_info("  redirect_uri: {OIDC_REDIRECT_URI}")
    log_info("  client_id: {OIDC_CLIENT_ID}")
    
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
      log_error("Invalid JWT format - parts: {length(token_parts)}")
      return(list(success = FALSE, error = "Invalid JWT token format"))
    }
    
    claims_json <- rawToChar(jose::base64url_decode(token_parts[2]))
    claims <- jsonlite::fromJSON(claims_json)
    
    log_success("Claims decoded")
    log_info("User email: {claims$email %||% 'NULL'}")
    log_info("User name: {claims$name %||% 'NULL'}")
    log_info("User eppn: {claims$eppn %||% 'NULL'}")
    log_info("User sub: {claims$sub %||% 'NULL'}")
    
    if(is.null(claims$groups)) {
      log_info("No groups claim found")
    } else {
      log_info("Groups found: {length(claims$groups)}")
      for(i in seq_along(claims$groups)) {
        log_debug("  Group {i}: {claims$groups[i]}")
      }
    }

    # Filter for dashboard groups
    dashboard_group <- NULL
    if (!is.null(claims$groups) && length(claims$groups) > 0) {
      matching_groups <- claims$groups[grepl("snv-benchmarking-dashboard", claims$groups)]
      
      log_info("Dashboard-specific groups found: {length(matching_groups)}")
      if (length(matching_groups) > 0) {
        for(g in matching_groups) {
          log_info("  Matched: {g}")
        }
      }
      
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
    
    log_info("Final dashboard role assigned: {dashboard_group %||% 'none'}")

    result <- list(
      email = claims$email,
      name = claims$name %||% claims$given_name %||% claims$email,
      username = claims$eppn %||% claims$sub,
      group = dashboard_group,
      success = TRUE
    )
    
    log_success("User extracted: {result$username}")
    log_success("Token exchange complete")
    return(result)
    
  }, error = function(e) {
    log_error("TOKEN EXCHANGE FAILED")
    log_error("Error class: {paste(class(e), collapse=', ')}")
    log_error("Error message: {e$message}")
    
    if (inherits(e, "httr2_http")) {
      tryCatch({
        log_error("HTTP status code: {e$status}")
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

# Store user info after successful auth
complete_authentication <- function(session, result, authenticated) {
  log_info("Completing authentication for user: {result$name}")
  
  session$userData$user_email <- result$email
  session$userData$user_name <- result$name
  session$userData$user_username <- result$username %||% result$email
  session$userData$user_group <- result$group
  authenticated(TRUE)
  
  log_info("Session data set - email: {result$email}, group: {result$group %||% 'none'}")
  
  # Store in localStorage
  user_data_list <- list(
    email = result$email,
    name = result$name,
    username = result$username %||% result$email
  )
  
  if (!is.null(result$group) && !is.na(result$group)) {
    user_data_list$group <- result$group
  }
  
  user_json <- jsonlite::toJSON(user_data_list, auto_unbox = TRUE, null = "null")
  
  runjs(sprintf("localStorage.setItem('user_session', '%s');", 
                gsub("'", "\\\\'", user_json)))
  
  log_info("User data persisted to localStorage")
  
  runjs("sessionStorage.removeItem('auth_state');")
  updateQueryString("?", mode = "replace")
  showNotification(paste("Welcome", result$name), type = "message")
  
  log_success("Authentication flow completed")
}

clear_session_data <- function(session, authenticated) {
  log_info("Clearing session data")
  
  session$userData$user_email <- NULL
  session$userData$user_name <- NULL
  session$userData$user_username <- NULL
  session$userData$user_group <- NULL
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
  
  log_info("=== AUTH SERVER INITIALIZED ===")
  log_info("Session ID: {session$token}")
  
  authenticated <- reactiveVal(FALSE)
  
  # Log client data once available
  observeEvent(session$clientData$url_hostname, {
    log_info("Client URL hostname: {session$clientData$url_hostname}")
    log_info("Client URL pathname: {session$clientData$url_pathname}")
  }, once = TRUE)
  
  # Session lifecycle observer
  observe({
    invalidateLater(10000)
    if (authenticated()) {
      log_debug("Session alive - ID: {substr(session$token, 1, 12)}, authenticated: TRUE")
    }
  })
  
  # Restore session from localStorage on load
  observe(priority = 1000, {
    log_info("Attempting to restore session from localStorage")
    runjs("Shiny.setInputValue('stored_user_session', localStorage.getItem('user_session'), {priority: 'event'});")
  })
  
  # Handle retrieved localStorage value
  observeEvent(input$stored_user_session, {
    user_json <- input$stored_user_session
    
    log_info("localStorage retrieval triggered - value present: {!is.null(user_json) && user_json != '' && user_json != 'null'}")
        
    if (!is.null(user_json) && user_json != "" && user_json != "null") {
      tryCatch({
        user_data <- jsonlite::fromJSON(user_json)
        
        log_info("Parsed localStorage data - email: {user_data$email %||% 'NULL'}")
                
        if (is.null(user_data$email) || is.null(user_data$name)) {
          log_warn("Invalid user data structure in localStorage, clearing")
          runjs("localStorage.removeItem('user_session');")
          return()
        }
        
        session$userData$user_email <- user_data$email
        session$userData$user_name <- user_data$name
        session$userData$user_username <- user_data$username %||% user_data$email
        session$userData$user_group <- user_data$group
        authenticated(TRUE)
        
        log_success("Session restored from localStorage for: {user_data$name}")
      }, error = function(e) {
        log_error("Failed to restore session from localStorage: {e$message}")
        runjs("localStorage.removeItem('user_session');")
      })
    } else {
      log_info("No saved session in localStorage")
    }
  
  }, ignoreNULL = FALSE, ignoreInit = FALSE)
  
  # Clear on session end
  session$onSessionEnded(function() {
    log_info("Shiny session ended, clearing auth data")
    session$userData$user_email <- NULL
    session$userData$user_name <- NULL
    session$userData$user_username <- NULL
    session$userData$user_group <- NULL
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
          style = "display: flex; gap: 8px; align-items: center; background-color: #f8f9fa; padding: 6px 12px; border-radius: 4px; border: 1px solid #e4e7ea;",
          span(
            style = "font-size: 13px; color: #4472ca; font-weight: 500; display: flex; align-items: center; gap: 5px;",
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
    log_info("Session ID: {session$token}")
    log_info("Client URL: {session$clientData$url_hostname}")

    if (!OIDC_ENABLED) {
      log_error("Login attempted but OIDC not configured")
      showNotification("Authentication not configured", type = "warning", duration = 5)
      return()
    }
    
    # Generate state token
    state <- random_string()
    session$userData$auth_state <- state
    
    log_info("Generated state token (full): {state}")
    log_info("State length: {nchar(state)} chars")
    log_info("State stored in session$userData$auth_state")
    
    runjs(sprintf("sessionStorage.setItem('auth_state', '%s');", state))
    log_info("State stored in browser sessionStorage")
    
    # Verify storage immediately
    log_info("Verifying session storage: session$userData$auth_state = {session$userData$auth_state}")
    
    # Build and redirect
    login_url <- create_login_url(state)
    if (!is.null(login_url)) {
      log_info("Redirecting to OIDC provider...")
      log_info("Full login URL: {login_url}")
      runjs(sprintf("window.location.href = '%s';", login_url))
    } else {
      log_error("Failed to create login URL")
      showNotification("Authentication service unavailable", type = "error")
    }
  })
  
  # Handle OAuth callback
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
      
      log_info("Checking session$userData$auth_state...")
      if (is.null(stored_state)) {
        log_warn("session$userData$auth_state is NULL")
      } else {
        log_info("session$userData$auth_state (full): {stored_state}")
        log_info("session$userData$auth_state length: {nchar(stored_state)}")
      }
    
      if (is.null(stored_state)) {
        log_warn("State not found in session$userData, attempting sessionStorage recovery")
        log_info("Triggering JavaScript to read sessionStorage...")
        runjs("Shiny.setInputValue('recovered_state', sessionStorage.getItem('auth_state'), {priority: 'event'});")
        return()
      }
      
      # Verify state
      log_info("=== STATE VALIDATION ===")
      log_info("Comparing states...")
      log_info("  Stored:  {stored_state}")
      log_info("  Received: {query$state}")
      log_info("  Match: {query$state == stored_state}")
      
      if (query$state != stored_state) {
        log_error("STATE MISMATCH DETECTED!")
        log_error("Expected (full): {stored_state}")
        log_error("Received (full): {query$state}")
        updateQueryString("?", mode = "replace")
        showNotification("Login failed - security check failed", type = "error")
        return()
      }
      
      log_success("State validated successfully")
      
      # Exchange code
      result <- get_user_from_code(query$code)
      
      if (!is.null(result) && result$success) {
        log_success("Token exchange succeeded, completing auth")
        complete_authentication(session, result, authenticated)
      } else {
        log_error("Token exchange failed or returned error")
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
    
    log_info("=== SESSIONSTORAGE RECOVERY TRIGGERED ===")
    recovered_state <- input$recovered_state
    
    log_info("Recovered state value: {recovered_state}")
    log_info("Recovered state type: {class(recovered_state)}")
    log_info("Recovered state length: {nchar(recovered_state)} chars")
    
    query <- parseQueryString(session$clientData$url_search)
    
    if (!is.null(query$code) && !is.null(query$state)) {
      log_info("=== VALIDATING RECOVERED STATE ===")
      log_info("Callback state (full): {query$state}")
      log_info("Recovered state (full): {recovered_state}")
      log_info("Do they match? {query$state == recovered_state}")
      
      if (query$state == recovered_state) {
        log_success("Recovered state matches callback state")
        
        result <- get_user_from_code(query$code)
        
        if (!is.null(result) && result$success) {
          log_success("Token exchange succeeded with recovered state")
          complete_authentication(session, result, authenticated)
        } else {
          log_error("Token exchange failed with recovered state")
          log_error("Result details: {paste(capture.output(str(result)), collapse=' ')}")
          showNotification("Login failed", type = "error")
          authenticated(FALSE)
          updateQueryString("?", mode = "replace")
        }
      } else {
        log_error("RECOVERED STATE MISMATCH!")
        log_error("Expected from callback: {query$state}")
        log_error("Got from sessionStorage: {recovered_state}")
        log_error("Lengths - callback: {nchar(query$state)}, recovered: {nchar(recovered_state)}")
        updateQueryString("?", mode = "replace")
      }
    } else {
      log_warn("State recovered but no code/state in URL query")
      log_info("query$code present: {!is.null(query$code)}")
      log_info("query$state present: {!is.null(query$state)}")
    }
  })
  
  # Handle logout
  observeEvent(input$logout_btn, {
    log_info("Logout button clicked")
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