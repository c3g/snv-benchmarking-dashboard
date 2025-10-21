ADMIN_USERNAMES <- c("snv_admin")

is_admin <- function(username) {
  if (is.null(username) || username == "") return(FALSE)
  tolower(username) %in% tolower(ADMIN_USERNAMES)
}