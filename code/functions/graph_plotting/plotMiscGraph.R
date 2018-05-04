# set up to plot a generic graph

plotMiscGraph <- function(dirName, graphName, width = 6000, height = 3000, ptSize = 14) {
  rootDir <- generateRootDir(directory = dirName)
  fileName <- paste(rootDir,
                    "/",graphName,
                    "_",
                    Sys.Date(),
                    ".png",
                    sep = "")
  
  png(file = fileName,
      width = width,
      height = height,
      res = 300,
      pointsize = ptSize)
}
