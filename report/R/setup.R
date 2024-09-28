library(gmdp)
library(dsra)
library(buildPlot)
library(buildTable)
library(data.table)
library(grid)
library(png)
library(knitr)
library(DT)
library(shinyWidgets)
library(markdown)
library(shinyjs)
library(htmltools)

library(data.table)
library(readxl)
library(highcharter)
library(stringr)
library(lubridate)
library(zoo)


.buildFigure <- function(PLOT,DELAY=0.3,imagesFolder="_var",params=list(background="white")){
  FILE <- tempfile(tmpdir = imagesFolder,pattern="fig",fileext=".png")
  XDELAY <- max(DELAY,round(nrow(DT)/1500*DELAY,digits = 2))
  AUX <- tempfile(pattern="fig",fileext=".html")
  htmlwidgets::saveWidget(widget = PLOT,background=params$background, file = AUX,selfcontained = TRUE)
  webshot2::webshot(delay=XDELAY,url = AUX, file = FILE)
  FIG <- knitr::include_graphics(path=FILE)
  return(FIG)
}

get_season <- function(x) {
  m <- month(x)
  seasons <- c("Summer", "Summer", "Autumn", "Autumn", "Autumn", 
               "Winter", "Winter", "Winter", "Spring", "Spring", 
               "Spring", "Summer")
  seasons[m]
}

get_season_year <- function(x) {
  m <- month(x)
  y <- year(x)   # get the last two digits of the year
  seasons <- c("Summer", "Summer", "Autumn", "Autumn", "Autumn", 
               "Winter", "Winter", "Winter", "Spring", "Spring", 
               "Spring", "Summer")
  paste(seasons[m], y, sep = "/")
}
get_month_year <- function(x) {
  
  m <- month(x)
  y <- year(x)   # get the last two digits of the year
  
  paste(y,m,"15", sep = "-") |> as.Date()
}

# Outlier detection routine
tagOutliers <- function(x, COL) {
  # IQR
  DT <- copy(x)
  # Tag seasons
  DT[, DATE_MD := format(DATE, "%m%d")]
  DT[, SEASON := ""]
  DT[,LO:=FALSE]
  DT[DATE_MD >= "0101" & DATE_MD < "0321", SEASON := "Summer"]
  DT[DATE_MD >= "0321" & DATE_MD < "0621", SEASON := "Autumn"]
  DT[DATE_MD >= "0621" & DATE_MD < "0921", SEASON := "Winter"]
  DT[DATE_MD >= "0921" & DATE_MD < "1221", SEASON := "Spring"]
  DT[DATE_MD >= "1221" & DATE_MD <= "1231", SEASON := "Summer"]
  DT[, DATE_MD := NULL]  # Cleanup
  
  
  
  # MAD local
  DT[, Median := median(get(COL)), by=.(SID, SEASON)]
  DT[, MAD := mad(get(COL)), by=.(SID, SEASON)]
  DT[, Z := 0.6745 * (get(COL) - Median) / MAD, by=.(SID, SEASON)]
  DT[LO==FALSE, LO := abs(Z) > 3.5, by=.(SID, SEASON)]  # Outlier flag
  DT[, `:=`(Median = NULL, MAD = NULL, Z = NULL)]  # Cleanup
  
  # Local outliers (by SID and SEASON)
  DT[, LowerBound := quantile(get(COL), 0.25) - 1.5 * IQR(get(COL)), by=.(SID, SEASON)]
  DT[, UpperBound := quantile(get(COL), 0.75) + 1.5 * IQR(get(COL)), by=.(SID, SEASON)]
  DT[LO==FALSE, LO := (get(COL) < LowerBound | get(COL) > UpperBound), by=.(SID, SEASON)]
  
  DT[, `:=`(LowerBound = NULL, UpperBound = NULL, SEASON = NULL)]  # Cleanup
  
  return(DT)
}

# Main function
buildDataset <- function(data=data.table::fread("../app/data/WL.csv"),chemical_name, fraction=NULL, matrix_code, mass, year_min=NULL, year_max=NULL, sys_loc_code=NULL,FLOW_ID="Flujo") {
  # 
  DATA <- data
  if(is.null(year_min)){
    year_min <- min(year(DATA$SAMPLE_DATE))
  }
  if(is.null(year_max)){
    year_max <- max(year(DATA$SAMPLE_DATE))
  }
  if(is.null(sys_loc_code)){
    sys_loc_code <- unique(DATA$SYS_LOC_CODE)
  }
  if(is.null(fraction)){
    fraction <- unique(DATA$FRACTION)
  }
  
  
  # Filtering concentration data (DTc)
  AUX <- DATA[DETECT_FLAG == TRUE &
                CHEMICAL_NAME == chemical_name &
                SYS_LOC_CODE %in% sys_loc_code &
                MATRIX_CODE == matrix_code &
                FRACTION %in% fraction & # CAUTION. mixing SW with GW can lead wrong values
                year(SAMPLE_DATE) >= year_min & 
                year(SAMPLE_DATE) <= year_max,
              .(ID = toupper(CHEMICAL_NAME), SID = SYS_LOC_CODE, DATE = SAMPLE_DATE, C = RESULT_NUMERIC, UN = RESULT_UNIT)]
  # DTc <- AUX[, .(C = mean(C)), by = .(ID, SID, DATE, UN)]
  DTc <- AUX[, .(C), by = .(ID, SID, DATE, UN)]
  
  # Outlier detection
  DTc <- tagOutliers(DTc, "C")
  if (mass == "c" && chemical_name != FLOW_ID) {
    return(DTc[, .(ID, SID, DATE, UN, VALUE = C,LO)])}
  
  
  # Filtering flow data (DTq)
  AUX <- DATA[DETECT_FLAG == TRUE &
                CHEMICAL_NAME == FLOW_ID &
                SYS_LOC_CODE %in% sys_loc_code &
                MATRIX_CODE == matrix_code &
                FRACTION == fraction &
                year(SAMPLE_DATE) >= year_min & 
                year(SAMPLE_DATE) <= year_max,
              .(ID = toupper(CHEMICAL_NAME), SID = SYS_LOC_CODE, DATE = SAMPLE_DATE, Q = RESULT_NUMERIC, UN = RESULT_UNIT)]
  # DTq <- AUX[, .(Q = mean(Q)), by = .(ID, SID, DATE, UN)]
  DTq <- AUX[, .(Q), by = .(ID, SID, DATE, UN)]
  
  # Outlier detection
  DTq <- tagOutliers(DTq, "Q")
  if (tolower(mass) == "q" || chemical_name == FLOW_ID) {
    return(DTq[, .(ID, SID, DATE, UN, VALUE = Q,LO)])}
  
  rm(AUX)
  # Mass calculation (DTm = DTc * DTq)
  DTm <- DTq[, .(SID, DATE,  Q)][DTc[, .(ID,SID, DATE,  C)], on = .(SID, DATE)] |> na.omit()
  DTm[, M := (Q * C * 3600/1000000)]
  # Outlier detection
  DTm <- tagOutliers(DTm, "M")
  UNIT <- unique(DTc$UN)[1] |> gsub(pattern="/L", replacement="/hr") |> gsub(pattern="mg/", replacement="kg/")
  # Replace pattern "/L" for "/hr" in UN
  if (tolower(mass) == "m") {
    return(DTm[, .(ID, SID, DATE, UN=UNIT, VALUE = M,LO)])}
  
  stop("Invalid mass flag!")
}

buildPlot.categorical <- function(
    data,
    plot.subtitle = "",
    plot.title ="",
    yAxis.label ="Y",
    xAxis.label ="X",
    group.label ="ID",
    tip.formatter = NULL,
    plot.bands = FALSE,
    band1.label = "B1=TRUE",
    band2.label = "B2=TRUE",
    plot.palette = hcl.pals()[4],
    plot.theme = hc_theme_grid(),# or hc_theme_ffx()
    legend.layout = "vertical",
    legend.align = "right", # c("center", "left", "right")
    legend.valign = "top", # c("top", "middle", "bottom")
    legend.show = TRUE,
    plot.save = FALSE,
    group.legend.fontsize = "12px"
) {
  # Initialize plot bands if necessary
  plot_band <- list()
  
  if (plot.bands == TRUE & "B1" %in% colnames(data)) {
    start_date <- min(data$X[data$B1])
    end_date <- max(data$X[data$B1])
    plot_band_1 <- list(
      from = as.numeric(as.POSIXct(start_date)) * 1000,  # convert to milliseconds
      to = as.numeric(as.POSIXct(end_date)) * 1000,      # convert to milliseconds
      color = "rgba(230,255,230,0.5)",  # Light green for B1
      label = list(text = band1.label)
    )
    plot_band <- c(plot_band, list(plot_band_1))  # Add to the plot_band list
  }
  
  if (plot.bands == TRUE & "B2" %in% colnames(data)) {
    start_date <- min(data$X[data$B2])
    end_date <- max(data$X[data$B2])
    plot_band_2 <- list(
      from = as.numeric(as.POSIXct(start_date)) * 1000,  # convert to milliseconds
      to = as.numeric(as.POSIXct(end_date)) * 1000,      # convert to milliseconds
      color = "rgba(230,247,255,0.5)",  # Light blue for B2
      label = list(text = band2.label)
    )
    plot_band <- c(plot_band, list(plot_band_2))  # Add to the plot_band list
  }
  HC <- highchart()
  HC <- HC |>
    hc_chart(style = list(fontFamily = "Helvetica"))|> 
    hc_legend(
      enabled = legend.show,
      align = legend.align,
      verticalAlign = legend.valign,
      layout = legend.layout,
      itemStyle = list(fontSize = group.legend.fontsize)
    ) 
  # Update the hc_xAxis function to include the plot band
  HC <-  HC |>
    # Add stacked columns for each group (ID)
    hc_add_series(
      data = data, 
      type = "column", 
      hcaes(x = X, y = Y, group = ID),
      stacking = "normal"  # Enable stacking for the column chart
    ) |> 
    
    hc_xAxis(
      type = "category",
      categories = unique(data$X),
      crosshair = TRUE,
      title = list(text = xAxis.label)
    ) |> 
    
    # Customize the y-axis and crosshairs
    hc_yAxis(
      type = 'linear', 
      crosshair = TRUE,
      title = list(text = yAxis.label )
    ) |> 
    hc_tooltip(
      shared = FALSE,  # Split tooltips depending on the stack portion
      crosshairs = TRUE,
      formatter = tip.formatter) |> 
    
    hc_title(text = plot.title) |> 
    hc_subtitle(text = plot.subtitle) |> 
    hc_add_theme(plot.theme) |> 
    hc_exporting(
      enabled = plot.save,
      type = "application/pdf",
      width = 800,
      scale = 3
    )
  
  HC
}


buildPlot.timeseries<- function(
    data,
    plot.type=c("point","average"), #scatter, spline, combined
    time.width=30,
    plot.subtitle = "",
    plot.title = "",
    yAxis.label = "Y",
    xAxis.label = "X",
    group.label = "ID",
    tip.formatter = NULL,
    plot.bands = TRUE,
    band1.label = "B1=TRUE",
    band2.label = "B2=TRUE",
    band3.label = "B3=TRUE",
    band4.label = "B4=TRUE",
    plot.palette = hcl.pals()[4],
    plot.theme = hc_theme_grid(),# or hc_theme_ffx()
    legend.layout = "vertical",
    legend.align = "right", # c("center", "left", "right")
    legend.valign = "top", # c("top", "middle", "bottom")
    legend.show = TRUE,
    plot.save = FALSE,
    group.legend.fontsize = "12px",
    Y1.max = NULL,
    Y2.max = NULL,
    Y1.label = NULL,
    Y2.label = NULL,
    Y1.color = "red",
    Y1.dashStyle = "Dash",
    Y2.color = "blue",
    Y2.dashStyle = "DashDot"
){
  # Ensure the dataset is ordered by ID and X (date) to calculate moving averages correctly
  data <- data[order(ID, X)]
  
  # Calculate n-day moving averages for Y
  data[, maY := rollapply(Y, width = time.width, FUN = mean, fill = NA, align = "right", partial = TRUE), by = ID]
  
  NID <- length(unique(data$ID))
  COLORS <- hcl.colors(n=NID, palette = plot.palette)
  # COLORS <- RColorBrewer::brewer.pal(n = NID, name = "Set1")
  data[, IDC := factor(ID, levels = unique(data$ID), labels = COLORS)]
  
  # Initialize plot bands if necessary
  plot_band <- NULL
  
  if (plot.bands == TRUE & "B1" %in% colnames(data)) {
    start_date <- min(data$X[data$B1])
    end_date <- max(data$X[data$B1])
    AUX <- list(
      from = as.numeric(as.POSIXct(start_date)) * 1000,  # convert to milliseconds
      to = as.numeric(as.POSIXct(end_date)) * 1000,      # convert to milliseconds
      color = "rgba(230,255,230,0.5)",  # Light green for B1
      label = list(text = band1.label)
    )
    plot_band <- c(plot_band, list(AUX))  # Add to the plot_band list
  }
  
  if (plot.bands == TRUE & "B2" %in% colnames(data)) {
    start_date <- min(data$X[data$B2])
    end_date <- max(data$X[data$B2])
    AUX <- list(
      from = as.numeric(as.POSIXct(start_date)) * 1000,  # convert to milliseconds
      to = as.numeric(as.POSIXct(end_date)) * 1000,      # convert to milliseconds
      color = "rgba(230,247,255,0.5)",  # Light blue for B2
      label = list(text = band2.label)
    )
    plot_band <- c(plot_band, list(AUX))  # Add to the plot_band list
  }
  
  if (plot.bands == TRUE & "B3" %in% colnames(data)) {
    start_date <- min(data$X[data$B3])
    end_date <- max(data$X[data$B3])
    AUX <- list(
      from = as.numeric(as.POSIXct(start_date)) * 1000,  # convert to milliseconds
      to = as.numeric(as.POSIXct(end_date)) * 1000,      # convert to milliseconds
      color = "rgba(230,247,255,0.5)",  # Light blue for B2
      label = list(text = band3.label)
    )
    plot_band <- c(plot_band, list(AUX))  # Add to the plot_band list
  }
  
  if (plot.bands == TRUE & "B4" %in% colnames(data)) {
    start_date <- min(data$X[data$B4])
    end_date <- max(data$X[data$B4])
    AUX <- list(
      from = as.numeric(as.POSIXct(start_date)) * 1000,  # convert to milliseconds
      to = as.numeric(as.POSIXct(end_date)) * 1000,      # convert to milliseconds
      color = "rgba(230,247,255,0.5)",  # Light blue for B2
      label = list(text = band4.label)
    )
    plot_band <- c(plot_band, list(AUX))  # Add to the plot_band list
  }
  
  # **Define Plot Lines for Y1.max and Y2.max**
  plot_lines <- NULL
  
  if (!is.null(Y1.max)) {
    plot_lines <- c(plot_lines, list(
      list(
        color = Y1.color,                # **Color for Y1.max**
        dashStyle = Y1.dashStyle,           # **Dashed Line Style**
        value = Y1.max,               # **Y-coordinate for the Line**
        width = 2,                    # **Line Width**
        label = list(
          text = paste0( Y1.label," = ", Y1.max),
          style = list(color = Y1.color)
        )
      )
    ))
  }
  
  if (!is.null(Y2.max)) {
    plot_lines <- c(plot_lines, list(
      list(
        color = Y2.color,               # **Color for Y2.max**
        dashStyle = Y2.dashStyle,        # **Dash-Dot Line Style**
        value = Y2.max,               # **Y-coordinate for the Line**
        width = 2,                    # **Line Width**
        label = list(
          text =  paste0( Y2.label," = ", Y2.max),
          style = list(color = Y2.color)
        )
      )
    ))
  }
  # Determine Y-axis min and max
  y_min <- min(c(data$Y, data$maY), na.rm = TRUE)
  y_max <- max(c(data$Y, data$maY), na.rm = TRUE)
  
  # Expand Y-axis range to include Y1.max and Y2.max if necessary
  if (!is.null(Y1.max)) y_max <- max(y_max, Y1.max, na.rm = TRUE)
  if (!is.null(Y2.max)) y_max <- max(y_max, Y2.max, na.rm = TRUE)
  
  
  # Create the highchart object with the original data
  HC <- highchart()
  HC <- HC |>
    hc_chart(style = list(fontFamily = "Helvetica"))|> 
    hc_legend(
      enabled = legend.show,
      align = legend.align,
      verticalAlign = legend.valign,
      layout = legend.layout,
      itemStyle = list(fontSize = group.legend.fontsize)
    ) 
  if("point" %in% plot.type){
    HC <- HC |> 
      hc_add_series(
        data = data, 
        type = "scatter", 
        hcaes(x = X, y = Y, group = ID,color = IDC),
      )
  }
  if("average" %in% plot.type){
    HC <- HC |> 
      hc_add_series(
        data = data,#data[!is.na(maY)], 
        type = "spline", 
        hcaes(x = X, y = maY, group = ID,color = IDC), 
      )
  }
  
  HC <- HC |> 
    hc_colors(COLORS) |>   
    hc_xAxis(
      type = "datetime",
      crosshair = TRUE,
      title = list(text = xAxis.label),
      plotBands = plot_band  # Add plot, bands
      
    ) |>
    hc_yAxis(
      type = 'linear', 
      crosshair = TRUE,
      title = list(text = yAxis.label),
      plotLines =plot_lines,  # Add plot lines here
      min = y_min,
      max = y_max
    ) |>
    hc_tooltip(
      shared = FALSE,  
      crosshairs = TRUE,
      formatter = tip.formatter
    ) |>
    hc_title(text = plot.title) |>
    hc_subtitle(text = plot.subtitle) |>
    hc_add_theme(plot.theme) |> 
    hc_exporting(
      enabled = plot.save,
      type = "application/pdf",
      width = 800,
      scale = 3
    )
  
  return(HC)
}


buildPlot.timeseries.combined<- function(
    data,
    id1,
    id2,
    
    plot.subtitle = "",
    plot.title = "",
    y1Axis.label = "Y1",
    y2Axis.label = "Y2",
    xAxis.label = "X",
    group.label = "SID",
    tip.formatter = NULL,
    plot.bands = TRUE,
    band1.label = "B1=TRUE",
    band2.label = "B2=TRUE",
    plot.palette = hcl.pals()[4],
    plot.theme = hc_theme_grid(),# or hc_theme_ffx()
    legend.layout = "vertical",
    legend.align = "right", # c("center", "left", "right")
    legend.valign = "top", # c("top", "middle", "bottom")
    legend.show = TRUE,
    plot.save = TRUE,
    group.legend.fontsize = "12px"
    
){
  
  # NID <- length(unique(data$SID))
  # COLORS <- hcl.colors(n=NID, palette = plot.palette)
  # # COLORS <- RColorBrewer::brewer.pal(n = NID, name = "Set1")
  # data[, IDC := factor(SID, levels = unique(data$SID), labels = COLORS)]
  
  # Initialize plot bands if necessary
  plot_band <- NULL
  
  if (plot.bands == TRUE & "B1" %in% colnames(data)) {
    start_date <- min(data$X[data$B1])
    end_date <- max(data$X[data$B1])
    AUX <- list(
      from = as.numeric(as.POSIXct(start_date)) * 1000,  # convert to milliseconds
      to = as.numeric(as.POSIXct(end_date)) * 1000,      # convert to milliseconds
      color = "rgba(230,255,230,0.5)",  # Light green for B1
      label = list(text = band1.label)
    )
    plot_band <- c(plot_band, list(AUX))  # Add to the plot_band list
  }
  
  if (plot.bands == TRUE & "B2" %in% colnames(data)) {
    start_date <- min(data$X[data$B2])
    end_date <- max(data$X[data$B2])
    AUX <- list(
      from = as.numeric(as.POSIXct(start_date)) * 1000,  # convert to milliseconds
      to = as.numeric(as.POSIXct(end_date)) * 1000,      # convert to milliseconds
      color = "rgba(230,247,255,0.5)",  # Light blue for B2
      label = list(text = band2.label)
    )
    plot_band <- c(plot_band, list(AUX))  # Add to the plot_band list
  }
  
  
  
  # Create the highchart object with the original data
  HC <- highchart()
  HC <- HC |>
    hc_chart(style = list(fontFamily = "Helvetica"))|> 
    hc_legend(
      enabled = legend.show,
      align = legend.align,
      verticalAlign = legend.valign,
      layout = legend.layout,
      itemStyle = list(fontSize = group.legend.fontsize)
    ) 
  
  HC <- HC |> 
    # hc_colors(COLORS) |>   
    hc_xAxis(
      # type = "datetime",
      type = "datetime",
      crosshair = TRUE,
      title = list(text = xAxis.label),
      plotBands = plot_band  # Add plot, bands
      
    ) |>
    hc_yAxis_multiples(
      list(
        # First yAxis for Y1
        title = list(text = y1Axis.label),
        labels = list(style = list(color = "#7cb5ec")),
        opposite = FALSE  # Positions the axis on the left
      ),
      list(
        # Second yAxis for Y2
        title = list(text = y2Axis.label),
        labels = list(style = list(color = "#434348")),
        opposite = TRUE  # Positions the axis on the right
      )
    ) %>%
    
    # Add the first series and assign to first yAxis (index 0)
    hc_add_series(
      data = data[ID==id1],
      type = "column",
      # hcaes(x = X, y = Y,group=SID),
      hcaes(x = X, y = Y),
      
      name = y1Axis.label,
      color = "red",
      yAxis = 0
    ) %>%
    
    # Add the second series and assign to second yAxis (index 1)
    hc_add_series(
      data =  data[ID==id2],
      type = "column",
      # hcaes(x = X, y = Y,group=SID),
      hcaes(x = X, y = Y),
      
      name = y2Axis.label,
      color ="blue",
      yAxis = 1
    ) %>%
    hc_tooltip(
      shared = FALSE,  
      crosshairs = TRUE,
      formatter = tip.formatter
    ) |>
    hc_title(text = plot.title) |>
    hc_subtitle(text = plot.subtitle) |>
    hc_add_theme(plot.theme) |> 
    hc_exporting(
      enabled = plot.save,
      type = "application/pdf",
      width = 800,
      scale = 3
    )
  
  return(HC)
}


