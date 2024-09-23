library(gmdp)
library(dsra)
library(buildPlot)
library(buildTable)
library(data.table)
library(grid)
library(png)
library(knitr)


.buildFigure <- function(PLOT,DELAY=0.3,imagesFolder="_var",params=list(background="white")){
  FILE <- tempfile(tmpdir = imagesFolder,pattern="fig",fileext=".png")
  XDELAY <- max(DELAY,round(nrow(DT)/1500*DELAY,digits = 2))
  AUX <- tempfile(pattern="fig",fileext=".html")
  htmlwidgets::saveWidget(widget = PLOT,background=params$background, file = AUX,selfcontained = TRUE)
  webshot2::webshot(delay=XDELAY,url = AUX, file = FILE)
  FIG <- knitr::include_graphics(path=FILE)
  return(FIG)
}
