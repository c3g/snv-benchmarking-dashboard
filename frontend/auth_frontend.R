# ============================================================================
# auth_frontend.R - Authentication Frontend Components
# ============================================================================
# Complete authentication system for SNV Benchmarking Dashboard
# Includes sign-in button, modal with role selection, forms, and session management

# ============================================================================
# 1. AUTHENTICATION CSS STYLES
# ============================================================================

AUTH_CSS_STYLES <- "
/* Authentication Button Styling */
.auth-button-container {
  position: absolute;
  top: 16px;
  right: 20px;
  z-index: 1003;
}

.auth-button {
  background: rgba(255, 255, 255, 0.15) !important;
  border: 1px solid rgba(255, 255, 255, 0.3) !important;
  color: white !important;
  padding: 8px 16px !important;
  border-radius: 5px !important;
  font-size: 13px !important;
  font-weight: 500 !important;
  transition: all 0.2s ease !important;
}

.auth-button:hover {
  background: rgba(255, 255, 255, 0.25) !important;
  border-color: rgba(255, 255, 255, 0.5) !important;
  color: white !important;
  transform: translateY(-1px) !important;
}

.auth-button:focus {
  outline: none !important;
  box-shadow: 0 0 0 2px rgba(255, 255, 255, 0.3) !important;
}

/* User Info Display */
.user-info-container {
  position: absolute;
  top: 16px;
  right: 20px;
  z-index: 1003;
  display: flex;
  align-items: center;
  gap: 12px;
}

.user-info-display {
  background: rgba(255, 255, 255, 0.15);
  border: 1px solid rgba(255, 255, 255, 0.3);
  border-radius: 5px;
  padding: 8px 12px;
  color: white;
  font-size: 13px;
  display: flex;
  align-items: center;
  gap: 8px;
}

.user-info-display .username {
  font-weight: 500;
}

.admin-badge {
  background: #ff4444 !important;
  color: white !important;
  padding: 2px 6px !important;
  border-radius: 3px !important;
  font-size: 10px !important;
  font-weight: 600 !important;
  letter-spacing: 0.5px !important;
}

.logout-btn {
  background: rgba(255, 255, 255, 0.15) !important;
  border: 1px solid rgba(255, 255, 255, 0.3) !important;
  color: white !important;
  padding: 4px 8px !important;
  border-radius: 3px !important;
  font-size: 11px !important;
  cursor: pointer;
  transition: all 0.2s ease !important;
}

.logout-btn:hover {
  background: rgba(255, 255, 255, 0.25) !important;
  border-color: rgba(255, 255, 255, 0.5) !important;
}

/* Authentication Modal Styling */
.auth-modal {
  position: fixed !important;
  top: 0 !important;
  left: 0 !important;
  width: 100vw !important;
  height: 100vh !important;
  background: rgba(0, 0, 0, 0.6) !important;
  z-index: 2000 !important;
  display: flex !important;
  align-items: center !important;
  justify-content: center !important;
}

.auth-modal-content {
  background: white;
  border-radius: 8px;
  box-shadow: 0 10px 40px rgba(0, 0, 0, 0.3);
  width: 90%;
  max-width: 450px;
  max-height: 90vh;
  overflow-y: auto;
  animation: slideIn 0.3s ease-out;
}

@keyframes slideIn {
  from {
    opacity: 0;
    transform: translateY(-30px);
  }
  to {
    opacity: 1;
    transform: translateY(0);
  }
}

.auth-modal-header {
  background: linear-gradient(135deg, #4472ca 0%, #365a96 100%);
  color: white;
  padding: 20px 24px;
  border-radius: 8px 8px 0 0;
  display: flex;
  justify-content: between;
  align-items: center;
}

.auth-modal-header h4 {
  margin: 0;
  font-size: 1.3em;
  font-weight: 600;
}

.auth-modal-close {
  background: none;
  border: none;
  color: white;
  font-size: 24px;
  cursor: pointer;
  padding: 0;
  margin-left: auto;
  opacity: 0.8;
  transition: opacity 0.2s ease;
}

.auth-modal-close:hover {
  opacity: 1;
}

.auth-modal-body {
  padding: 24px;
}

/* Role Selection Buttons */
.role-selection {
  display: grid;
  grid-template-columns: 1fr 1fr;
  gap: 16px;
  margin-bottom: 24px;
}

.role-button {
  background: #f8f9fa;
  border: 2px solid #e9ecef;
  border-radius: 8px;
  padding: 20px 16px;
  text-align: center;
  cursor: pointer;
  transition: all 0.2s ease;
  text-decoration: none;
  color: #495057;
}

.role-button:hover {
  border-color: #4472ca;
  background: #f0f4ff;
  color: #4472ca;
  text-decoration: none;
}

.role-button.active {
  border-color: #4472ca;
  background: #4472ca;
  color: white;
}

.role-button .role-title {
  font-weight: 600;
  font-size: 1.2em;
  margin-bottom: 8px;
}

.role-button .role-description {
  font-size: 0.9em;
  opacity: 0.8;
  line-height: 1.3;
}

/* Form Styling */
.auth-form {
  margin-top: 20px;
}

.auth-form .form-group {
  margin-bottom: 16px;
}

.auth-form label {
  font-weight: 500;
  color: #495057;
  margin-bottom: 6px;
  display: block;
}

.auth-form .form-control {
  border: 2px solid #e9ecef;
  border-radius: 6px;
  padding: 10px 12px;
  font-size: 14px;
  transition: all 0.2s ease;
  width: 100%;
  box-sizing: border-box;
}

.auth-form .form-control:focus {
  outline: none;
  border-color: #4472ca;
  box-shadow: 0 0 0 3px rgba(68, 114, 202, 0.1);
}

.auth-form .form-control.is-invalid {
  border-color: #dc3545;
}

.auth-form .invalid-feedback {
  color: #dc3545;
  font-size: 12px;
  margin-top: 4px;
}

/* Form Toggle */
.form-toggle {
  text-align: center;
  margin-top: 16px;
  padding-top: 16px;
  border-top: 1px solid #e9ecef;
}

.form-toggle a {
  color: #4472ca;
  text-decoration: none;
  font-size: 14px;
}

.form-toggle a:hover {
  text-decoration: underline;
}

/* Action Buttons */
.auth-actions {
  margin-top: 20px;
  display: flex;
  gap: 12px;
}

.auth-btn {
  padding: 10px 20px;
  border: none;
  border-radius: 6px;
  font-size: 14px;
  font-weight: 500;
  cursor: pointer;
  transition: all 0.2s ease;
  flex: 1;
}

.auth-btn-primary {
  background: #4472ca;
  color: white;
}

.auth-btn-primary:hover {
  background: #365a96;
  transform: translateY(-1px);
}

.auth-btn-primary:disabled {
  background: #6c757d;
  cursor: not-allowed;
  transform: none;
}

.auth-btn-secondary {
  background: #6c757d;
  color: white;
}

.auth-btn-secondary:hover {
  background: #5a6268;
}

/* Messages */
.auth-message {
  padding: 12px 16px;
  border-radius: 6px;
  margin-bottom: 16px;
  font-size: 14px;
}

.auth-message.success {
  background: #d4edda;
  border: 1px solid #c3e6cb;
  color: #155724;
}

.auth-message.error {
  background: #f8d7da;
  border: 1px solid #f5c6cb;
  color: #721c24;
}

.auth-message.info {
  background: #d1ecf1;
  border: 1px solid #bee5eb;
  color: #0c5460;
}

/* Loading Spinner */
.auth-loading {
  display: inline-block;
  width: 16px;
  height: 16px;
  border: 2px solid #ffffff;
  border-radius: 50%;
  border-top-color: transparent;
  animation: spin 1s ease-in-out infinite;
  margin-right: 8px;
}

@keyframes spin {
  to { transform: rotate(360deg); }
}
"

# ============================================================================
# 2. AUTHENTICATION JAVASCRIPT
# ============================================================================

AUTH_JS <- '
// Authentication Modal Management
$(document).ready(function() {
  
  // Show authentication modal
  $(document).on("click", "#show_auth_modal", function(e) {
    e.preventDefault();
    $("#auth_modal").show();
    $("body").css("overflow", "hidden");
  });
  
  // Hide authentication modal
  function hideAuthModal() {
    $("#auth_modal").hide();
    $("body").css("overflow", "auto");
    // Reset modal state
    $(".role-button").removeClass("active");
    $(".auth-form").hide();
    $("#auth_message").html("");
  }
  
  // Close modal handlers
  $(document).on("click", "#close_auth_modal, .auth-modal-close", function(e) {
    e.preventDefault();
    hideAuthModal();
  });
  
  // Close modal when clicking outside
  $(document).on("click", "#auth_modal", function(e) {
    if (e.target === this) {
      hideAuthModal();
    }
  });
  
  // Role selection
  $(document).on("click", ".role-button", function(e) {
    e.preventDefault();
    const role = $(this).data("role");
    
    // Update active state
    $(".role-button").removeClass("active");
    $(this).addClass("active");
    
    // Show appropriate form
    $(".auth-form").hide();
    if (role === "user") {
      $("#user_auth_form").show();
    } else if (role === "admin") {
      $("#admin_auth_form").show();
    }
  });
  
  // Toggle between login and signup for users
  $(document).on("click", "#toggle_signup", function(e) {
    e.preventDefault();
    $("#user_login_form").hide();
    $("#user_signup_form").show();
  });
  
  $(document).on("click", "#toggle_login", function(e) {
    e.preventDefault();
    $("#user_signup_form").hide();
    $("#user_login_form").show();
  });
  
  // Form validation helpers
  function validateEmail(email) {
    // Simple email validation
    return email.includes("@") && email.includes(".") && email.length > 5;
  }
  
  function showValidationError(inputId, message) {
    const input = $("#" + inputId);
    input.addClass("is-invalid");
    input.siblings(".invalid-feedback").remove();
    input.after("<div class=\\"invalid-feedback\\">" + message + "</div>");
  }
  
  function clearValidationErrors() {
    $(".form-control").removeClass("is-invalid");
    $(".invalid-feedback").remove();
  }
  
  // Client-side validation
  function validateUserSignup() {
    clearValidationErrors();
    let isValid = true;
    
    const username = $("#signup_username").val();
    const email = $("#signup_email").val();
    const password = $("#signup_password").val();
    const confirmPassword = $("#signup_confirm_password").val();
    
    if (!username || username.length < 3) {
      showValidationError("signup_username", "Username must be at least 3 characters");
      isValid = false;
    }
    
    if (!email || !validateEmail(email)) {
      showValidationError("signup_email", "Please enter a valid email address");
      isValid = false;
    }
    
    if (!password || password.length < 6) {
      showValidationError("signup_password", "Password must be at least 6 characters");
      isValid = false;
    }
    
    if (password !== confirmPassword) {
      showValidationError("signup_confirm_password", "Passwords do not match");
      isValid = false;
    }
    
    return isValid;
  }
  
  function validateUserLogin() {
    clearValidationErrors();
    let isValid = true;
    
    const username = $("#login_username").val();
    const password = $("#login_password").val();
    
    if (!username) {
      showValidationError("login_username", "Username or email is required");
      isValid = false;
    }
    
    if (!password) {
      showValidationError("login_password", "Password is required");
      isValid = false;
    }
    
    return isValid;
  }
  
  function validateAdminLogin() {
    clearValidationErrors();
    let isValid = true;
    
    const username = $("#admin_username").val();
    const password = $("#admin_password").val();
    
    if (!username) {
      showValidationError("admin_username", "Username is required");
      isValid = false;
    }
    
    if (!password) {
      showValidationError("admin_password", "Password is required");
      isValid = false;
    }
    
    return isValid;
  }
  
  // Show loading state
  function setButtonLoading(buttonId, isLoading) {
    const button = $("#" + buttonId);
    if (isLoading) {
      button.prop("disabled", true);
      button.html("<span class=\\"auth-loading\\"></span>Processing...");
    } else {
      button.prop("disabled", false);
      // Restore original text based on button
      if (buttonId === "submit_signup") {
        button.html("Create Account");
      } else if (buttonId === "submit_login") {
        button.html("Sign In");
      } else if (buttonId === "submit_admin_login") {
        button.html("Admin Login");
      }
    }
  }
  
  // Form submissions - trigger Shiny inputs
  $(document).on("click", "#submit_signup", function(e) {
    e.preventDefault();
    if (validateUserSignup()) {
      setButtonLoading("submit_signup", true);
      // Trigger Shiny event
      Shiny.setInputValue("submit_signup_click", Math.random());
    }
  });
  
  $(document).on("click", "#submit_login", function(e) {
    e.preventDefault();
    if (validateUserLogin()) {
      setButtonLoading("submit_login", true);
      // Trigger Shiny event
      Shiny.setInputValue("submit_login_click", Math.random());
    }
  });
  
  $(document).on("click", "#submit_admin_login", function(e) {
    e.preventDefault();
    if (validateAdminLogin()) {
      setButtonLoading("submit_admin_login", true);
      // Trigger Shiny event
      Shiny.setInputValue("submit_admin_login_click", Math.random());
    }
  });
  
  // Reset loading states when needed
  function resetFormLoadingStates() {
    setButtonLoading("submit_signup", false);
    setButtonLoading("submit_login", false);
    setButtonLoading("submit_admin_login", false);
  }
  
  // Global function to show messages from Shiny
  window.showAuthMessage = function(message, type) {
    const alertClass = type === "success" ? "success" : (type === "error" ? "error" : "info");
    $("#auth_message").html("<div class=\\"auth-message " + alertClass + "\\">" + message + "</div>");
    resetFormLoadingStates();
    
    // Auto-hide success messages and close modal after delay
    if (type === "success") {
      setTimeout(function() {
        hideAuthModal();
      }, 2000);
    }
  };
});
'

# ============================================================================
# 3. AUTHENTICATION UI COMPONENTS
# ============================================================================

# Sign In Button (placed in sidebar header)
auth_button_ui <- function() {
  div(class = "auth-button-container",
    # Show sign in button when not authenticated
    conditionalPanel(
      condition = "!output.is_authenticated",
      actionButton("show_auth_modal", "Sign In", 
                  class = "auth-button",
                  icon = icon("sign-in-alt"))
    ),
    
    # Show user info when authenticated
    conditionalPanel(
      condition = "output.is_authenticated",
      div(class = "user-info-display",
        div(class = "username",
          "Welcome, ",
          textOutput("current_username", inline = TRUE)
        ),
        
        # Admin badge
        conditionalPanel(
          condition = "output.is_admin",
          span("ADMIN", class = "admin-badge")
        ),
        
        # Logout button
        actionButton("logout", "Logout", 
                    class = "logout-btn",
                    icon = icon("sign-out-alt"))
      )
    )
  )
}

# Authentication Modal
auth_modal_ui <- function() {
  div(id = "auth_modal", class = "auth-modal", style = "display: none;",
    div(class = "auth-modal-content",
      # Modal Header
      div(class = "auth-modal-header",
        h4("Sign In to SNV Benchmarking Dashboard"),
        tags$button(type = "button", class = "auth-modal-close", 
                   id = "close_auth_modal", "×")
      ),
      
      # Modal Body
      div(class = "auth-modal-body",
        # Message area
        div(id = "auth_message"),
        
        # Role Selection
        div(class = "role-selection",
          div(class = "role-button", `data-role` = "user",
            div(class = "role-title", "User"),
            div(class = "role-description", "Upload and analyze your experiments")
          ),
          div(class = "role-button", `data-role` = "admin", 
            div(class = "role-title", "Admin"),
            div(class = "role-description", "Manage all experiments and users")
          )
        ),
        
        # User Authentication Form
        div(id = "user_auth_form", class = "auth-form", style = "display: none;",
          # User Login Form
          div(id = "user_login_form",
            h5("User Login"),
            div(class = "form-group",
              tags$label("Username or Email"),
              textInput("login_username", NULL, placeholder = "Enter username or email")
            ),
            div(class = "form-group",
              tags$label("Password"),
              passwordInput("login_password", NULL, placeholder = "Enter password")
            ),
            div(class = "auth-actions",
              tags$button(type = "button", id = "submit_login", 
                         class = "auth-btn auth-btn-primary", "Sign In")
            ),
            div(class = "form-toggle",
              "Don't have an account? ",
              a(href = "#", id = "toggle_signup", "Create one")
            )
          ),
          
          # User Signup Form
          div(id = "user_signup_form", style = "display: none;",
            h5("Create User Account"),
            div(class = "form-group",
              tags$label("Username"),
              textInput("signup_username", NULL, placeholder = "Choose a username")
            ),
            div(class = "form-group",
              tags$label("Email"),
              textInput("signup_email", NULL, placeholder = "Enter your email")
            ),
            div(class = "form-group",
              tags$label("Password"),
              passwordInput("signup_password", NULL, placeholder = "Create a password")
            ),
            div(class = "form-group",
              tags$label("Confirm Password"),
              passwordInput("signup_confirm_password", NULL, placeholder = "Confirm your password")
            ),
            div(class = "auth-actions",
              tags$button(type = "button", id = "submit_signup", 
                         class = "auth-btn auth-btn-primary", "Create Account")
            ),
            div(class = "form-toggle",
              "Already have an account? ",
              a(href = "#", id = "toggle_login", "Sign in")
            )
          )
        ),
        
        # Admin Authentication Form
        div(id = "admin_auth_form", class = "auth-form", style = "display: none;",
          h5("Admin Login"),
          div(class = "form-group",
            tags$label("Admin Username"),
            textInput("admin_username", NULL, placeholder = "Enter admin username")
          ),
          div(class = "form-group",
            tags$label("Admin Password"), 
            passwordInput("admin_password", NULL, placeholder = "Enter admin password")
          ),
          div(class = "auth-actions",
            tags$button(type = "button", id = "submit_admin_login", 
                       class = "auth-btn auth-btn-primary", "Admin Login")
          )
        )
      )
    )
  )
}

# ============================================================================
# 4. AUTHENTICATION OBSERVERS (Server-side logic)
# ============================================================================

setup_auth_observers <- function(input, output, session) {
  
  # Authentication state reactive values
  auth_state <- reactiveValues(
    authenticated = FALSE,
    user_info = NULL,
    is_admin = FALSE
  )
  
  # Authentication status outputs
  output$is_authenticated <- reactive({
    auth_state$authenticated
  })
  outputOptions(output, "is_authenticated", suspendWhenHidden = FALSE)
  
  output$is_admin <- reactive({
    auth_state$is_admin
  })
  outputOptions(output, "is_admin", suspendWhenHidden = FALSE)
  
  output$current_username <- renderText({
    if (auth_state$authenticated && !is.null(auth_state$user_info)) {
      auth_state$user_info$username
    } else {
      ""
    }
  })
  
  # User Signup Observer
  observeEvent(input$submit_signup_click, {
    req(input$signup_username, input$signup_email, 
        input$signup_password, input$signup_confirm_password)
    
    # Client-side validation should have passed, but double-check
    if (input$signup_password != input$signup_confirm_password) {
      session$sendCustomMessage("showAuthMessage", 
        list(message = "Passwords do not match", type = "error"))
      return()
    }
    
    tryCatch({
      # Call Python backend to create user
      result <- auth$create_user(
        username = input$signup_username,
        email = input$signup_email, 
        password = input$signup_password,
        role = "user"
      )
      
      if (result$success) {
        session$sendCustomMessage("showAuthMessage", 
          list(message = paste("Account created successfully! Welcome,", input$signup_username), 
               type = "success"))
        
        # Auto-login the new user
        auth_state$authenticated <- TRUE
        auth_state$user_info <- list(
          id = result$user$id,
          username = result$user$username,
          email = result$user$email,
          role = result$user$role
        )
        auth_state$is_admin <- (result$user$role == "admin")
        
      } else {
        session$sendCustomMessage("showAuthMessage", 
          list(message = result$message, type = "error"))
      }
      
    }, error = function(e) {
      session$sendCustomMessage("showAuthMessage", 
        list(message = paste("Registration failed:", e$message), type = "error"))
    })
  })
  
  # User Login Observer
  observeEvent(input$submit_login_click, {
    req(input$login_username, input$login_password)
    
    tryCatch({
      # Call Python backend to authenticate
      result <- auth$authenticate_user(
        username = input$login_username,
        password = input$login_password
      )
      
      if (result$success) {
        session$sendCustomMessage("showAuthMessage", 
          list(message = paste("Welcome back,", result$user$username, "!"), 
               type = "success"))
        
        # Update authentication state
        auth_state$authenticated <- TRUE
        auth_state$user_info <- list(
          id = result$user$id,
          username = result$user$username,
          email = result$user$email,
          role = result$user$role
        )
        auth_state$is_admin <- (result$user$role == "admin")
        
      } else {
        session$sendCustomMessage("showAuthMessage", 
          list(message = result$message, type = "error"))
      }
      
    }, error = function(e) {
      session$sendCustomMessage("showAuthMessage", 
        list(message = paste("Login failed:", e$message), type = "error"))
    })
  })
  
  # Admin Login Observer
  observeEvent(input$submit_admin_login_click, {
    req(input$admin_username, input$admin_password)
    
    tryCatch({
      # Call Python backend to authenticate admin
      result <- auth$authenticate_user(
        username = input$admin_username,
        password = input$admin_password
      )
      
      if (result$success && result$user$role == "admin") {
        session$sendCustomMessage("showAuthMessage", 
          list(message = "Admin access granted!", type = "success"))
        
        # Update authentication state
        auth_state$authenticated <- TRUE
        auth_state$user_info <- list(
          id = result$user$id,
          username = result$user$username,
          email = result$user$email,
          role = result$user$role
        )
        auth_state$is_admin <- TRUE
        
      } else if (result$success && result$user$role != "admin") {
        session$sendCustomMessage("showAuthMessage", 
          list(message = "Admin access required", type = "error"))
      } else {
        session$sendCustomMessage("showAuthMessage", 
          list(message = result$message, type = "error"))
      }
      
    }, error = function(e) {
      session$sendCustomMessage("showAuthMessage", 
        list(message = paste("Admin login failed:", e$message), type = "error"))
    })
  })
  
  # Logout Observer
  observeEvent(input$logout, {
    auth_state$authenticated <- FALSE
    auth_state$user_info <- NULL
    auth_state$is_admin <- FALSE
    
    # Optionally show a message or refresh the page
    showNotification("Logged out successfully", type = "message")
  })
  
  # Return authentication state for use in other parts of the app
  return(auth_state)
}