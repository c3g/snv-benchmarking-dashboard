library(httr2)
library(jose)
library(jsonlite)

Sys.setenv(
  OIDC_ISSUER = "http://localhost:8080/realms/test", #URL of the identity provider
  OIDC_CLIENT_ID = "snv-dashboard",
  OIDC_CLIENT_SECRET = "2yosJOz5mAOCkpoGzTMWtWkjOKh6bCkd",
  OIDC_REDIRECT_URI = "http://127.0.0.1:3838"
)

OIDC_ISSUER <- Sys.getenv("OIDC_ISSUER", "")
OIDC_CLIENT_ID <- Sys.getenv("OIDC_CLIENT_ID", "")
OIDC_CLIENT_SECRET <- Sys.getenv("OIDC_CLIENT_SECRET", "")
OIDC_REDIRECT_URI <- Sys.getenv("OIDC_REDIRECT_URI", "")

OIDC_ENABLED <- (OIDC_ISSUER != "" && OIDC_CLIENT_ID != "" && 
                 OIDC_CLIENT_SECRET != "" && OIDC_REDIRECT_URI != "")

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

random_string <- function() {
  paste0(sample(c(letters, LETTERS, 0:9), 32, replace = TRUE), collapse = "")
}

is_authenticated <- function(session) {
  !is.null(session$userData$user_email)
}

get_user_info <- function(session) {
  if (!is_authenticated(session)) return(NULL)
  list(email = session$userData$user_email, name = session$userData$user_name)
}

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

# requests JWT from OICD and decode
get_user_from_code <- function(code) {
  if (is.null(OIDC_CONFIG)) return(NULL)
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
    
    # Decode token
    token_parts <- strsplit(token_response$id_token, "\\.")[[1]]
    claims_json <- rawToChar(jose::base64url_decode(token_parts[2]))
    claims <- jsonlite::fromJSON(claims_json)
    
    return(list(
      email = claims$email,
      name = claims$name %||% claims$preferred_username %||% claims$email,
      success = TRUE
    ))
  }, error = function(e) {
    cat("ERROR getting user info:", e$message, "\n")
    return(list(success = FALSE, error = e$message))
  })
}

auth_ui <- function() {
  uiOutput("auth_status")
}

auth_server <- function(input, output, session) {
  
  # Create a reactive value to track authentication state
  authenticated <- reactiveVal(FALSE)
  
  # Clear on session end
  session$onSessionEnded(function() {
    session$userData$user_email <- NULL
    session$userData$user_name <- NULL
    session$userData$auth_state <- NULL
    authenticated(FALSE)
  })
  
  output$auth_status <- renderUI({
    if (is_authenticated(session)) {
      user <- get_user_info(session)
      div(
        style = "display: flex; gap: 10px; align-items: center;",
        span(style = "font-size: 14px; color: #4472ca;", icon("user-circle"), paste("Welcome,", user$name)),
        actionButton("logout_btn", "Sign Out", class = "btn-secondary btn-sm", style = "font-size: 12px; padding: 4px 12px;")
      )
    } else {
      actionButton("login_btn", "Sign In", class = "btn-primary btn-sm", icon = icon("sign-in-alt"), style = "font-size: 13px; padding: 6px 16px;")
    }
  })
  
  observeEvent(input$login_btn, {
    # FORCE LOGOUT FIRST (clears any existing session)
    session$userData$user_email <- NULL
    session$userData$user_name <- NULL
    session$userData$auth_state <- NULL
    authenticated(FALSE)
    runjs("sessionStorage.clear();")
    
    if (!OIDC_ENABLED) {
      showNotification("Authentication not configured", type = "warning", duration = 5)
      return()
    }
    
    state <- random_string()
    session$userData$auth_state <- state
    runjs(sprintf("sessionStorage.setItem('auth_state', '%s');", state))
    
    login_url <- create_login_url(state)
    if (!is.null(login_url)) {
      runjs(sprintf("window.location.href = '%s';", login_url))
    } else {
      showNotification("Authentication service unavailable", type = "error")
    }
  })
  
  observe({
    query <- parseQueryString(session$clientData$url_search)
    
    if (!is.null(query$code) && !is.null(query$state)) {
      stored_state <- session$userData$auth_state
      
      if (is.null(stored_state)) {
        runjs("Shiny.setInputValue('recovered_state', sessionStorage.getItem('auth_state'));")
        Sys.sleep(0.1)
        stored_state <- input$recovered_state
      }
      
      if (is.null(stored_state) || query$state != stored_state) {
        updateQueryString("?", mode = "replace")
        return()
      }
      
      result <- get_user_from_code(query$code)
      
      # Store email and name from the JWT token 
      if (result$success) {
        session$userData$user_email <- result$email
        session$userData$user_name <- result$name
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
  
  observeEvent(input$logout_btn, {
    session$userData$user_email <- NULL
    session$userData$user_name <- NULL
    session$userData$auth_state <- NULL
    authenticated(FALSE) 
    showNotification("Signed out", type = "message")
  })
  
  # Return the reactive value
  return(authenticated)
}