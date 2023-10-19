isLocalEnvironment <- function() {
  # Check if running in RStudio
  identical(Sys.getenv("RSTUDIO", unset = "not_running"), "running")
}

getPictureURL <- function(jpg_path) {
  if (isLocalEnvironment()) {
    # Use the local URL when running locally
    paste0("http://127.0.0.1:3437", jpg_path)
  } else {
    # Use the public URL when running on a public server
    paste0("https://thiagosilveira.shinyapps.io/dash_monitoring/", jpg_path)
  }
}
