# ============================================================================
# auth.R - OpenID Connect (OIDC) Authentication - COManage
# ============================================================================
# Handles user authentication via OIDC provider

library(httr2)
library(jose)
library(jsonlite)

# ============================================================================
# CONFIGURATION
# ============================================================================

OIDC_ISSUER <- Sys.getenv("OIDC_ISSUER", "")
OIDC_CLIENT_ID <- Sys.getenv("OIDC_CLIENT_ID", "")
OIDC_CLIENT_SECRET <- Sys.getenv("OIDC_CLIENT_SECRET", "")
OIDC_REDIRECT_URI <- Sys.getenv("OIDC_REDIRECT_URI", "")


OIDC_ENABLED <- (OIDC_ISSUER != "" && OIDC_CLIENT_ID != "" && 
                 OIDC_CLIENT_SECRET != "" && OIDC_REDIRECT_URI != "")

cat("=== OIDC Configuration ===\n")
cat("OIDC_ENABLED:", OIDC_ENABLED, "\n")
cat("OIDC_REDIRECT_URI:", OIDC_REDIRECT_URI, "\n\n")

# get OIDC provider config as JSON (mainly for authorization endpoint) 
get_oidc_config <- function() {
  if (!OIDC_ENABLED) {
    cat("Skipping OIDC config fetch (OIDC disabled)\n")
    return(NULL)
  }
  
  tryCatch({
    url <- paste0(OIDC_ISSUER, "/.well-known/openid-configuration")
    cat("Fetching OIDC config from:", url, "\n")
    
    config <- request(url) %>% req_perform() %>% resp_body_json()
    
    cat("✓ OIDC config retrieved successfully\n")
    cat("  Authorization endpoint:", substr(config$authorization_endpoint, 1, 50), "...\n")
    cat("  Token endpoint:", substr(config$token_endpoint, 1, 50), "...\n")
    
    return(config)
  }, error = function(e) {
    cat("❌ ERROR: Can't reach OIDC provider\n")
    cat("URL:", paste0(OIDC_ISSUER, "/.well-known/openid-configuration"), "\n")
    cat("Error:", e$message, "\n")
    return(NULL)
  })
}

OIDC_CONFIG <- get_oidc_config()

if (!is.null(OIDC_CONFIG)) {
  cat("OIDC config loaded successfully\n\n")
}

# ============================================================================
# ADMIN CONFIGURATION
# ============================================================================

# check admin privileges based on group membership
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

# Generate random state token for CSRF protection
random_string <- function() {
  paste0(sample(c(letters, LETTERS, 0:9), 32, replace = TRUE), collapse = "")
}

# Check if user is authenticated
is_authenticated <- function(session) {
  !is.null(session$userData$user_email)
}

# Get user information from session
get_user_info <- function(session) {
  if (!is_authenticated(session)) return(NULL)
  
  user_group <- session$userData$user_group
  
  # Handle NULL, NA, or missing group
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

# Build OIDC log-in URL 
# by combiniing parameters (scope, client_id, etc) with Authorization endpoint
create_login_url <- function(state) {
  if (is.null(OIDC_CONFIG)) return(NULL)
  params <- paste0(
    "client_id=", OIDC_CLIENT_ID,
    "&redirect_uri=", URLencode(OIDC_REDIRECT_URI, reserved = TRUE),
    "&response_type=code",
    "&scope=openid+profile+email+org.cilogon.userinfo",
    "&state=", state
  )
  paste0(OIDC_CONFIG$authorization_endpoint, "?", params)
}

# Exchange authorization code for user info
get_user_from_code <- function(code) {
  if (is.null(OIDC_CONFIG)) {
    cat("ERROR: OIDC_CONFIG is NULL\n")
    return(NULL)
  }

  cat("\n=== TOKEN EXCHANGE START ===\n")
  cat("Token endpoint:", OIDC_CONFIG$token_endpoint, "\n")
  cat("Redirect URI:", OIDC_REDIRECT_URI, "\n")
  cat("Client ID:", OIDC_CLIENT_ID, "\n")
  cat("===========================\n\n")

  tryCatch({
    cat("Sending token request...\n")
    
    # Request JWT token
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
    
    cat("✓ Token received successfully\n")
    
    # Check if id_token exists
    if (is.null(token_response$id_token)) {
      cat("ERROR: No id_token in response\n")
      cat("Response keys:", paste(names(token_response), collapse=", "), "\n")
      return(list(success = FALSE, error = "No id_token in token response"))
    }
    
    cat("✓ ID token present (length:", nchar(token_response$id_token), ")\n")
    
    # Extract user claims
    token_parts <- strsplit(token_response$id_token, "\\.")[[1]]
    
    if (length(token_parts) < 2) {
      cat("ERROR: Invalid JWT format\n")
      return(list(success = FALSE, error = "Invalid JWT token format"))
    }
    
    claims_json <- rawToChar(jose::base64url_decode(token_parts[2]))
    claims <- jsonlite::fromJSON(claims_json)
    
    cat("✓ Claims decoded successfully\n")

    cat("\n=== RECEIVED CLAIMS ===\n")
    cat("email:", claims$email %||% "NULL", "\n")
    cat("name:", claims$name %||% "NULL", "\n")
    cat("eppn:", claims$eppn %||% "NULL", "\n")
    cat("sub:", claims$sub %||% "NULL", "\n")
    if(is.null(claims$groups)) {
      cat("groups: NULL\n")
    } else {
      cat("groups (", length(claims$groups), "):\n", sep="")
      for(i in seq_along(claims$groups)) {
        cat("  - ", claims$groups[i], "\n", sep="")
      }
    }
    cat("========================\n\n")

    # Filter for dashboard groups
    dashboard_group <- NULL
    if (!is.null(claims$groups) && length(claims$groups) > 0) {
      matching_groups <- claims$groups[grepl("snv-benchmarking-dashboard", claims$groups)]
      
      cat("Dashboard groups found:", length(matching_groups), "\n")
      if (length(matching_groups) > 0) {
        cat("Matching groups:\n")
        for(g in matching_groups) {
          cat("  - ", g, "\n", sep="")
        }
      }
      
      # Determine highest role
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
    cat("→ Final dashboard role:", dashboard_group %||% "none", "\n\n")

    result <- list(
      email = claims$email,
      name = claims$name %||% claims$given_name %||% claims$email,
      username = claims$eppn %||% claims$sub,
      group = dashboard_group,
      success = TRUE
    )
    
    cat("✓ User extracted successfully:", result$username, "\n")
    cat("=== TOKEN EXCHANGE COMPLETE ===\n\n")
    return(result)
    
  }, error = function(e) {
    cat("\n=== TOKEN EXCHANGE ERROR ===\n")
    cat("Error type:", class(e)[1], "\n")
    cat("Error message:", e$message, "\n")
    
    # Try to get HTTP error details
    if (inherits(e, "httr2_http")) {
      tryCatch({
        cat("HTTP status:", e$status, "\n")
        if (!is.null(e$body)) {
          cat("Response body:\n")
          print(e$body)
        }
      }, error = function(e2) {
        cat("Could not extract HTTP details\n")
      })
    }
    
    cat("============================\n\n")
    return(list(success = FALSE, error = e$message))
  })
}

# store user info in session after successful authentication
complete_authentication <- function(session, result, authenticated) {
  cat("Completing authentication for:", result$name, "\n")
  
  # Store in session with safe defaults
  session$userData$user_email <- result$email
  session$userData$user_name <- result$name
  session$userData$user_username <- result$username %||% result$email
  session$userData$user_group <- result$group  # Can be NULL for non-admin users
  authenticated(TRUE)
  
  # Store in localStorage for persistence across reloads
  user_data_list <- list(
    email = result$email,
    name = result$name,
    username = result$username %||% result$email
  )
  
  # add group if not NULL
  if (!is.null(result$group) && !is.na(result$group)) {
    user_data_list$group <- result$group
  }
  
  user_json <- jsonlite::toJSON(user_data_list, auto_unbox = TRUE, null = "null")
  
  runjs(sprintf("localStorage.setItem('user_session', '%s');", 
                gsub("'", "\\\\'", user_json)))
  
  runjs("sessionStorage.removeItem('auth_state');")
  updateQueryString("?", mode = "replace")
  showNotification(paste("Welcome", result$name), type = "message")
  cat("Authentication completed\n\n")
}

# Clear all
clear_session_data <- function(session, authenticated) {
  session$userData$user_email <- NULL
  session$userData$user_name <- NULL
  session$userData$user_username <- NULL
  session$userData$user_group <- NULL
  session$userData$auth_state <- NULL
  authenticated(FALSE)
  
  # Clear localStorage
  runjs("localStorage.removeItem('user_session');")
  runjs("sessionStorage.clear();")
}

# ============================================================================
# UI COMPONENT
# ============================================================================

auth_ui <- function() {
  tagList(
    tags$script(HTML("
      // Handle /callback route by redirecting to root with query params
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

# Main authentication server logic - returns reactive authentication state
auth_server <- function(input, output, session) {
  
  # Track authentication state
  authenticated <- reactiveVal(FALSE)
  
  # Restore session from localStorage on page load
  observe(priority = 1000, {
    
    # Trigger localStorage retrieval
    runjs("Shiny.setInputValue('stored_user_session', localStorage.getItem('user_session'), {priority: 'event'});")
  })
  
  # Handle retrieved localStorage value
  observeEvent(input$stored_user_session, {
    user_json <- input$stored_user_session
        
    if (!is.null(user_json) && user_json != "" && user_json != "null") {
      tryCatch({
        user_data <- jsonlite::fromJSON(user_json)
                
        # Validate user_data structure
        if (is.null(user_data$email) || is.null(user_data$name)) {
          cat("Invalid user data structure, clearing localStorage\n")
          runjs("localStorage.removeItem('user_session');")
          return()
        }
        
        # Restore to session
        session$userData$user_email <- user_data$email
        session$userData$user_name <- user_data$name
        session$userData$user_username <- user_data$username %||% user_data$email
        session$userData$user_group <- user_data$group  # Can be NULL
        authenticated(TRUE)
        # cat("==================================\n\n")
       # cat("Session restored from localStorage for:", user_data$name, "\n")
      }, error = function(e) {
       # cat("Failed to restore session:", e$message, "\n")
        runjs("localStorage.removeItem('user_session');")
      })
    } else {
      #cat("No saved session found\n")
    }
  
  }, ignoreNULL = FALSE, ignoreInit = FALSE)
  
  # Clear session data when user disconnects
  session$onSessionEnded(function() {
    session$userData$user_email <- NULL
    session$userData$user_name <- NULL
    session$userData$user_username <- NULL
    session$userData$user_group <- NULL
    session$userData$auth_state <- NULL
    authenticated(FALSE)
  })
  
  # Render login/logout button based on auth state
  output$auth_status <- renderUI({
    # trigger re-render when authenticated() changes
    auth_state <- authenticated()
    
    tryCatch({
      if (is_authenticated(session)) {
        user <- get_user_info(session)
        
        if (is.null(user)) {
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
      cat("ERROR in auth_status renderUI:", e$message, "\n")
      # Fallback to login button on error
      actionButton(
        "login_btn", 
        "Sign In", 
        class = "btn-primary btn-sm", 
        icon = icon("sign-in-alt"), 
        style = "font-size: 13px; padding: 6px 16px;"
      )
    })
  })
  
  # Handle login button click - redirect to OIDC provider
  observeEvent(input$login_btn, {
    
    cat("\n=== Login Button Clicked ===\n")

    if (!OIDC_ENABLED) { #make sure OIDC is configured
    showNotification("Authentication not configured", type = "warning", duration = 5)
    return()
  }
    
    # 1. Generate and store state token and store in sessionStorage
    state <- random_string()
    session$userData$auth_state <- state 
    runjs(sprintf("sessionStorage.setItem('auth_state', '%s');", state))
    
    # 2. Build login URL and Redirect to OIDC login page
    login_url <- create_login_url(state)
    if (!is.null(login_url)) {
      cat("Redirecting to OIDC provider\n")
      runjs(sprintf("window.location.href = '%s';", login_url)) # redirect 
    } else {
      showNotification("Authentication service unavailable", type = "error")
    }
  })
  
  # 3. Handle OAuth callback - process authorization code
  observe({
    query <- parseQueryString(session$clientData$url_search)
    
    if (!is.null(query$code) && !is.null(query$state)) {
      cat("\n=== OAuth Callback Received ===\n")
      
      stored_state <- session$userData$auth_state
      
      full_callback_url <- session$clientData$url_search
      cat("Callback URL length:", nchar(full_callback_url), "\n")
      cat("Authorization code length:", nchar(query$code), "\n")
    
      if (is.null(stored_state)) {
        cat("No stored state, attempting recovery\n")
        runjs("Shiny.setInputValue('recovered_state', sessionStorage.getItem('auth_state'));")
        return()
      }
      
      # 3.1 Verify state token matches
      if (query$state != stored_state) {
        cat("ERROR: State mismatch\n")
        updateQueryString("?", mode = "replace")
        return()
      }
      
      cat("State validated\n")
      
      # 3.2 Exchange code for user info
      result <- get_user_from_code(query$code)
      
      # 3.3 Complete authentication and get user info if successful
      if (!is.null(result) && result$success) {
        complete_authentication(session, result, authenticated)
      } else {
        showNotification("Login failed", type = "error")
        authenticated(FALSE)
        updateQueryString("?", mode = "replace")
      }
    }
  })
  
  # Handle recovered state from sessionStorage -------------------------------------------------------------------------------
  observe({
    req(input$recovered_state)
    cat("\n=== Using Recovered State ===\n")
    stored_state <- input$recovered_state
    query <- parseQueryString(session$clientData$url_search)
    
    if (!is.null(query$code) && !is.null(query$state) && query$state == stored_state) {
      result <- get_user_from_code(query$code)
      
      # Complete authentication and get user info if successful
      if (!is.null(result) && result$success) {
        complete_authentication(session, result, authenticated)
      } else {
        showNotification("Login failed", type = "error")
        authenticated(FALSE)
        updateQueryString("?", mode = "replace")
      }
    }
  })
  
  # Handle logout button click
  observeEvent(input$logout_btn, {
    clear_session_data(session, authenticated)
    showNotification("Signed out", type = "message")
  })
  
  # ====================================================================
  # AUTHORIZATION OUTPUTS
  # ====================================================================
  
  # Expose authentication status to UI
  output$user_authenticated <- reactive({
    authenticated()
  })
  outputOptions(output, "user_authenticated", suspendWhenHidden = FALSE)
  
  # Expose admin status to UI
  output$user_is_admin <- reactive({
    tryCatch({
      auth_state <- authenticated()
      
      if (!auth_state) return(FALSE)
      
      user <- get_user_info(session)
      
      if (is.null(user)) return(FALSE)
      
      if (is.null(user$is_admin) || is.na(user$is_admin)) return(FALSE)
      
      return(user$is_admin)
      
    }, error = function(e) {
      cat("ERROR in user_is_admin reactive:", e$message, "\n")
      return(FALSE)
    })
  })
  outputOptions(output, "user_is_admin", suspendWhenHidden = FALSE)
  
  # Return reactive authentication state
  return(authenticated)
}