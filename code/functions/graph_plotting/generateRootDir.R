# Generates standardised directory for graph plotting which updates based on the date

generateRootDir <- function(directory) {
  rootDir <- file.path("./Graphs",Sys.Date())
  dir.create(rootDir, showWarnings = FALSE, recursive = TRUE)
  rootDir <- file.path("./Graphs",Sys.Date(),directory)
  dir.create(rootDir, showWarnings = FALSE, recursive = TRUE)
  return(rootDir)
}