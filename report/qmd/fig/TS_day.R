# build FLOW timeseries ----

DT <- buildDataset(
  chemical_name = chemical_name,
  year_min=year_min,
  year_max=year_max,
  matrix_code = matrix_code,
  fraction =fraction,
  mass=mass,
  sys_loc_code=sys_loc_code)
# Tag periods
DT[,B1:=FALSE]
DT[,B2:=FALSE]
DT[,B1:=DATE>=as.Date("2011-01-01") & DATE <= as.Date("2018-01-01")]
DT[,B2:=DATE>=as.Date("2018-01-01")]
# outliers
DT <- DT[LO==FALSE]


chemical_unit <- DT$UN[1]

DATA <- DT[order(DATE),.(X=DATE,Y=VALUE,B1=B1,B2=B2,ID=SID)][, .(Y = mean(Y),B1,B2), by = .(ID, X)] 

yAxis.label <-  paste0(chemical_name, " [", chemical_unit, "]")
xAxis.label <- "Date"
group.label <-  "Station"
tip.formatter <- JS(paste0("function () { return '<b>", xAxis.label, ":</b> ' + Highcharts.dateFormat('%Y-%m-%d', this.x) + '<br>' + '<b>", yAxis.label, ":</b> ' + this.y + '<br>' + '<b>", group.label, ":</b> ' + this.series.name; }"))

PLOT <- buildPlot.timeseries(
  data=DATA,
  plot.type=plot.type,
  time.width = time_width,
  plot.subtitle = "",
  plot.title = yAxis.label,
  yAxis.label = yAxis.label,
  xAxis.label =xAxis.label,
  group.label = group.label,
  band1.label="Tunnel Open",
  band2.label="Tunnel Plugged",
  tip.formatter = tip.formatter,
  plot.bands = TRUE,
  plot.theme=hc_theme_smpl())
