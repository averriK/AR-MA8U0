# build FLOW timeseries ----


DT <- PDT
DT <- DT[year(DATE)>=year_min & year(DATE)<=year_max & SID %in% piezo_code]
# # Tag periods
DT[,B1:=FALSE]
DT[,B2:=FALSE]
# outliers
DT <- DT[LO==FALSE]

chemical_name <- "Hw"
chemical_unit <- "m"
DATA <- DT[order(DATE),.(X=DATE,Y=VALUE,B1=B1,B2=B2,ID=SID)][, .(Y = mean(Y),B1,B2), by = .(ID, X)] 
yAxis.label <-  paste0(chemical_name, " [", chemical_unit, "]")
xAxis.label <- "Date"
group.label <-  "Station"
tip.formatter <- JS(paste0("function () { return '<b>", xAxis.label, ":</b> ' + Highcharts.dateFormat('%Y-%m-%d', this.x) + '<br>' + '<b>", yAxis.label, ":</b> ' + this.y + '<br>' + '<b>", group.label, ":</b> ' + this.series.name; }"))

PLOT <- buildPlot.timeseries(
  data=DATA,
  Y2.max=Y1.max,
  Y1.max=Y2.max,
  Y1.label=Y1.label,
  Y2.label=Y2.label,
  plot.type=plot.type,
  time.width = time_width,
  plot.subtitle = "",
  plot.title = yAxis.label,
  yAxis.label = yAxis.label,
  xAxis.label =xAxis.label,
  group.label = group.label,
  tip.formatter = tip.formatter,
  plot.bands = TRUE,
  plot.theme=hc_theme_smpl())

