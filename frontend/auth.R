# ============================================================================
# auth.R - OpenID Connect (OIDC) Authentication
# ============================================================================
# Handles user authentication via OIDC provider
# Manages session state and authorization

library(httr2)
library(jose)
library(jsonlite)

source("admin_config.R")

# ============================================================================
# CONFIGURATION
# ============================================================================

# load OIDC credentials from environment variables
OIDC_ISSUER <- Sys.getenv("OIDC_ISSUER", "")
OIDC_CLIENT_ID <- Sys.getenv("OIDC_CLIENT_ID", "")
OIDC_CLIENT_SECRET <- Sys.getenv("OIDC_CLIENT_SECRET", "")
OIDC_REDIRECT_URI <- Sys.getenv("OIDC_REDIRECT_URI", "")

# Check if authentication is properly configured
OIDC_ENABLED <- (OIDC_ISSUER != "" && OIDC_CLIENT_ID != "" && 
                 OIDC_CLIENT_SECRET != "" && OIDC_REDIRECT_URI != "")

# Fetch OIDC provider configuration
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

# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

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
  list(email = session$userData$user_email, name = session$userData$user_name, username = session$userData$user_username)
}

# Build OIDC authorization URL
create_login_url <- function(state) {
  if (is.null(OIDC_CONFIG)) return(NULL)
  params <- paste0(
    "client_id=", OIDC_CLIENT_ID,
    "&redirect_uri=", URLencode(OIDC_REDIRECT_URI, reserved = TRUE),
    "&response_type=code",
    "&scope=openid+profile+email",
    "&state=", state
  )
  paste0(OIDC_CONFIG$authorization_endpoint, "?", params)
}

# Exchange authorization code for user info
get_user_from_code <- function(code) {
  if (is.null(OIDC_CONFIG)) return(NULL)
  tryCatch({
    # Request JWT token from OIDC provider (keycloak)
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
    
    # Decode JWT payload to extract user claims
    token_parts <- strsplit(token_response$id_token, "\\.")[[1]]
    claims_json <- rawToChar(jose::base64url_decode(token_parts[2]))
    claims <- jsonlite::fromJSON(claims_json)
    
    return(list(
      email = claims$email,
      name = claims$name %||% claims$preferred_username %||% claims$email,
      username = claims$preferred_username %||% claims$sub,
      success = TRUE
    ))
  }, error = function(e) {
    cat("ERROR getting user info:", e$message, "\n")
    return(list(success = FALSE, error = e$message))
  })
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
    auth_state <- authenticated()
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
      actionButton("login_btn", "Sign In", class = "btn-primary btn-sm", icon = icon("sign-in-alt"), style = "font-size: 13px; padding: 6px 16px;")
    }
  })
  
  # Handle login button click - redirect to OIDC provider
  observeEvent(input$login_btn, {
    # Clear any existing session
    session$userData$user_email <- NULL
    session$userData$user_name <- NULL
    session$userData$user_username <- NULL
    session$userData$auth_state <- NULL
    authenticated(FALSE)
    runjs("sessionStorage.clear();")
    
    if (!OIDC_ENABLED) {
      showNotification("Authentication not configured", type = "warning", duration = 5)
      return()
    }
    
    # Generate and store state token
    state <- random_string()
    session$userData$auth_state <- state
    runjs(sprintf("sessionStorage.setItem('auth_state', '%s');", state))
    
    # Redirect to OIDC login page
    login_url <- create_login_url(state)
    if (!is.null(login_url)) {
      runjs(sprintf("window.location.href = '%s';", login_url))
    } else {
      showNotification("Authentication service unavailable", type = "error")
    }
  })
  
  # Handle OAuth callback - process authorization code
  observe({
    query <- parseQueryString(session$clientData$url_search)
    
    if (!is.null(query$code) && !is.null(query$state)) {
      stored_state <- session$userData$auth_state
      
      # Recover state from sessionStorage if server session lost it
      if (is.null(stored_state)) {
        runjs("Shiny.setInputValue('recovered_state', sessionStorage.getItem('auth_state'));")
        Sys.sleep(0.1)
        stored_state <- input$recovered_state
      }
      
      # Verify state token matches
      if (is.null(stored_state) || query$state != stored_state) {
        updateQueryString("?", mode = "replace")
        return()
      }
      
      # Exchange code for user info
      result <- get_user_from_code(query$code)
      
      if (result$success) {
        session$userData$user_email <- result$email
        session$userData$user_name <- result$name
        session$userData$user_username <- result$username
        authenticated(TRUE) 
        runjs("sessionStorage.removeItem('auth_state');")
        updateQueryString("?", mode = "replace")
        showNotification(paste("Welcome", result$name), type = "message")
      } else {
        showNotification("Login failed", type = "error")
        authenticated(FALSE)
        updateQueryString("?", mode = "replace")
      }
    }
  })
  
  # Handle logout button click
  observeEvent(input$logout_btn, {
    session$userData$user_email <- NULL
    session$userData$user_name <- NULL
    session$userData$user_username <- NULL
    session$userData$auth_state <- NULL
    authenticated(FALSE) 
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
  authenticated()
  user <- get_user_info(session)
  if (!is.null(user)) {
    result <- is_admin(user$username)
  } else {
    result <- FALSE
  }
  return(result)
})
  outputOptions(output, "user_is_admin", suspendWhenHidden = FALSE)
  
  # Return reactive authentication state
  return(authenticated)
}