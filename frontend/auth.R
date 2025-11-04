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
cat("OIDC_REDIRECT_URI:", OIDC_REDIRECT_URI, "\n")
if (OIDC_ENABLED && !grepl("/callback$", OIDC_REDIRECT_URI)) {
  cat("WARNING: OIDC_REDIRECT_URI should end with /callback\n")
}
cat("\n")

# get OIDC provider config as JSON (mainly for authorization endpoint) 
get_oidc_config <- function() {
  if (!OIDC_ENABLED) return(NULL)
  tryCatch({
    url <- paste0(OIDC_ISSUER, "/.well-known/openid-configuration")
    request(url) %>% req_perform() %>% resp_body_json()
  }, error = function(e) {
    cat("ERROR: Can't reach OIDC provider:", e$message, "\n")
    return(NULL)
  })
}

OIDC_CONFIG <- get_oidc_config()

if (!is.null(OIDC_CONFIG)) {
  cat("OIDC config loaded successfully\n\n")
}

# ============================================================================
# ADMIN CONFIGURATION - TO BE CHANGED ACCORDING TO GROUPS +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ============================================================================

# Load admin usernames from env
admin_env <- Sys.getenv("ADMIN_USERNAMES", "")
if (admin_env != "") {
  ADMIN_USERNAMES <- trimws(strsplit(admin_env, ",")[[1]])
} else {
  ADMIN_USERNAMES <- c()
}

# check admin privileges 
is_admin <- function(username) {
  if (is.null(username) || username == "") return(FALSE)
  tolower(username) %in% tolower(ADMIN_USERNAMES)
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
  list(
    email = session$userData$user_email, 
    name = session$userData$user_name, 
    username = session$userData$user_username
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
  if (is.null(OIDC_CONFIG)) return(NULL)
  
  cat("\n=== Token Exchange Started ===\n")
  
  tryCatch({
    # Request JWT token
    token_response <- request(OIDC_CONFIG$token_endpoint) %>%
      req_body_form(
        grant_type = "authorization_code",
        code = code,
        redirect_uri = OIDC_REDIRECT_URI,
        client_id = OIDC_CLIENT_ID,
        client_secret = OIDC_CLIENT_SECRET
      ) %>%
      req_perform() %>% #send POST req to IODC provider
      resp_body_json() #parse JSON into R list
    
    cat("Token received\n")
    
    #  extract user claims
    token_parts <- strsplit(token_response$id_token, "\\.")[[1]]
    claims_json <- rawToChar(jose::base64url_decode(token_parts[2]))
    claims <- jsonlite::fromJSON(claims_json)
    
    # Log all claims received
    cat("\n=== RECEIVED CLAIMS ===\n")
    cat("email:", claims$email %||% "NULL", "\n")
    cat("name:", claims$name %||% "NULL", "\n")
    cat("eppn:", claims$eppn %||% "NULL", "\n")
    cat("sub:", claims$sub %||% "NULL", "\n")
    cat("========================\n\n")

    # TO BE UPDATED WITH COMANAGE
    result <- list(
      email = claims$email,
      name = claims$name %||% claims$given_name %||% claims$email,
      username = claims$eppn %||% claims$sub, # eppn or fall back to sub
      # GROUP TO BE ADDED HERE
      success = TRUE
    )
    
    cat("User extracted:", result$username, "\n")
    return(result)
    
  }, error = function(e) {
    cat("ERROR getting user info:", e$message, "\n")
    return(list(success = FALSE, error = e$message))
  })
}

# store user info in session after successful authentication
complete_authentication <- function(session, result, authenticated) {
  cat("Completing authentication for:", result$name, "\n")
  session$userData$user_email <- result$email
  session$userData$user_name <- result$name
  session$userData$user_username <- result$username
  authenticated(TRUE) # set auth state to TRUE
  runjs("sessionStorage.removeItem('auth_state');") # cleanup
  updateQueryString("?", mode = "replace")
  showNotification(paste("Welcome", result$name), type = "message")
  cat("Authentication completed\n\n")
}

# Clear all
clear_session_data <- function(session, authenticated) {
  session$userData$user_email <- NULL
  session$userData$user_name <- NULL
  session$userData$user_username <- NULL
  session$userData$auth_state <- NULL
  authenticated(FALSE)
  runjs("sessionStorage.clear();")
}

# ============================================================================
# UI COMPONENT
# ============================================================================

auth_ui <- function() {
  uiOutput("auth_status")
}

# ============================================================================
# SERVER LOGIC
# ============================================================================

# Main authentication server logic - returns reactive authentication state
auth_server <- function(input, output, session) {
  
  # Track authentication state
  authenticated <- reactiveVal(FALSE)
  
  # Clear session data when user disconnects
  session$onSessionEnded(function() {
    session$userData$user_email <- NULL
    session$userData$user_name <- NULL
    session$userData$user_username <- NULL
    session$userData$auth_state <- NULL
    authenticated(FALSE)
  })
  
  # Render login/logout button based on auth state
  output$auth_status <- renderUI({
    if (is_authenticated(session)) {
      user <- get_user_info(session)
      div(
        style = "display: flex; gap: 8px; align-items: center; background-color: #f8f9fa; padding: 6px 12px; border-radius: 4px; border: 1px solid #e4e7ea;",
        span(
          style = "font-size: 13px; color: #4472ca; font-weight: 500; display: flex; align-items: center; gap: 5px;",
          icon("user"),
          span(user$name)
        ),
        actionButton(
          "logout_btn", 
          "Sign Out",
          class = "btn-sm",
          style = "font-size: 13px; padding: 6px 12px; background-color: #ffffff; border: 1px solid #d1d5db; color: #556b78; font-weight: 500;"
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
      
      if (is.null(stored_state)) {
        cat("No stored state, attempting recovery\n")
        runjs("Shiny.setInputValue('recovered_state', sessionStorage.getItem('auth_state'));") # try to recover state from sessionStorage
        return()
      }
      
      # 3.1 Verify state token matches
      if (query$state != stored_state) {
        cat("ERROR: State mismatch\n")
        updateQueryString("?", mode = "replace") # clean URL
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
    user <- get_user_info(session)
    if (!is.null(user)) {
      return(is_admin(user$username))
    }
    return(FALSE)
  })
  outputOptions(output, "user_is_admin", suspendWhenHidden = FALSE)
  
  # Return reactive authentication state
  return(authenticated)
}