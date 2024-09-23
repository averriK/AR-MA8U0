rm(DT)
DT <- buildDataset(chemical_name = "Calcio",year_min=2010,year_max=2024,matrix_code = "SW",fraction = "T",mass="m")




# Promedios estacionales  ---------------------------------------------

DATA <- DT[LO==FALSE,.(X=DATE,Y=VALUE,ID=SID)][,.(Y=mean(Y) ),by=.(ID,X=get_season_year(X))][order(X)]

plot.subtitle <- ""
plot.title <-  ""
yAxis.label <-  ""
xAxis.label <- "Season"
group.label <- "Station"
tip.formatter <- JS(paste0("function (){return '<b>", xAxis.label, ":</b> ' + this.x + '<br>' + '<b>", yAxis.label, ":</b> ' + this.y + '<br>' + '<b>",group.label,":</b> ' + this.series.name; }"))



buildPlot.categorical(
  data=DATA,
  plot.subtitle = plot.subtitle,
  plot.title = plot.title,
  yAxis.label = yAxis.label,
  xAxis.label =xAxis.label,
  group.label =group.label,
  tip.formatter = tip.formatter,
  plot.theme=hc_theme_smpl())


# Promedios estacionales historicos ---------------------------------------------

# DT <- WL[LO_FLAG==FALSE & CHEMICAL_NAME %in% CHEMICAL_ID & SYS_LOC_CODE %in% GROUP_ID & year(SAMPLE_DATE)>=YEAR_MIN & year(SAMPLE_DATE)<=YEAR_MAX,.(X=SAMPLE_DATE,Y=RESULT_NUMERIC,ID=paste0(SYS_LOC_CODE,"[",MATRIX_CODE,"]"))]
# DATA <- DT[order(X),.(Y=mean(Y) |> round(digits=DIGITS)),by=.(ID,X=get_season(X))]

DATA <- DT[LO==FALSE,.(X=DATE,Y=VALUE,ID=SID)][, .(Y = mean(Y)), by=.(ID,X=get_season(X))][order(X)]

plot.subtitle <- ""
plot.title <-  ""
yAxis.label <-  ""
xAxis.label <- "Season (average)"
group.label <- "Station"
tip.formatter <- JS(paste0("function (){return '<b>", xAxis.label, ":</b> ' + this.x + '<br>' + '<b>", yAxis.label, ":</b> ' + this.y + '<br>' + '<b>",group.label,":</b> ' + this.series.name; }"))


buildPlot.categorical(
  data=DATA,
  plot.subtitle = plot.subtitle,
  plot.title = plot.title,
  yAxis.label = yAxis.label,
  xAxis.label =xAxis.label,
  group.label =group.label,
  tip.formatter = tip.formatter,
  plot.theme=hc_theme_smpl())



# Promedios semanales por ubicacion ---------------------------------------------
# DT <- WL[LO_FLAG==FALSE & CHEMICAL_NAME %in% CHEMICAL_ID & SYS_LOC_CODE %in% GROUP_ID & year(SAMPLE_DATE)>=YEAR_MIN & year(SAMPLE_DATE)<=YEAR_MAX,.(X=SAMPLE_DATE,Y=RESULT_NUMERIC,ID=paste0(SYS_LOC_CODE,"[",MATRIX_CODE,"]"))]
# DATA <- DT[,.(Y=mean(Y) |> round(digits=DIGITS)),by=.(ID,X=week(X))][order(X)]


DATA <- DT[LO==FALSE,.(X=DATE,Y=VALUE,ID=SID)][, .(Y = mean(Y)), by=.(ID,X=week(X))][order(X)]


plot.subtitle <- ""
plot.title <-  ""
yAxis.label <-  ""
xAxis.label <- "Week"
group.label <- "Station"
tip.formatter <- JS(paste0("function (){return '<b>", xAxis.label, ":</b> ' + this.x + '<br>' + '<b>", yAxis.label, ":</b> ' + this.y + '<br>' + '<b>",group.label,":</b> ' + this.series.name; }"))


buildPlot.categorical(
  data=DATA,
  plot.subtitle = plot.subtitle,
  plot.title = plot.title,
  yAxis.label = yAxis.label,
  xAxis.label =xAxis.label,
  group.label =group.label,
  tip.formatter = tip.formatter,
  plot.theme=hc_theme_smpl())

# 
# Promedios mensuales por ubicacion ---------------------------------------------
# DT <- WL[LO_FLAG==FALSE & CHEMICAL_NAME %in% CHEMICAL_ID & SYS_LOC_CODE %in% GROUP_ID & year(SAMPLE_DATE)>=YEAR_MIN & year(SAMPLE_DATE)<=YEAR_MAX,.(X=SAMPLE_DATE,Y=RESULT_NUMERIC,ID=paste0(SYS_LOC_CODE,"[",MATRIX_CODE,"]"))]
# DATA <- DT[,.(Y=mean(Y)|> round(digits=DIGITS)),by=.(ID,X=month.abb[month(X)])][order(match(X, month.abb)) ]


DATA <- DT[LO==FALSE,.(X=DATE,Y=VALUE,ID=SID)][, .(Y = mean(Y)), by=.(ID,X=month.abb[month(X)])][order(match(X, month.abb)) ]


plot.subtitle <- ""
plot.title <-  ""
yAxis.label <-  ""
xAxis.label <- "Month"
group.label <- "Station"
tip.formatter <- JS(paste0("function (){return '<b>", xAxis.label, ":</b> ' + Highcharts.dateFormat('%b', this.x) + '<br>' + '<b>", yAxis.label, ":</b> ' + this.y + '<br>' + '<b>",group.label,":</b> ' + this.series.name; }"))


buildPlot.categorical(
  data=DATA,
  plot.subtitle = plot.subtitle,
  plot.title = plot.title,
  yAxis.label = yAxis.label,
  xAxis.label =xAxis.label,
  group.label =group.label,
  tip.formatter = tip.formatter,
  plot.theme=hc_theme_smpl())

# *
# Promedios de cada locacion por año ---------------------------------------------
# XXX <- WL[LO_FLAG==FALSE & CHEMICAL_NAME %in% CHEMICAL_ID & SYS_LOC_CODE %in% GROUP_ID & year(SAMPLE_DATE)>=YEAR_MIN & year(SAMPLE_DATE)<=YEAR_MAX,.(ID=year(SAMPLE_DATE),Y=RESULT_NUMERIC,X=paste0(SYS_LOC_CODE,"[",MATRIX_CODE,"]"))]
# DATA <- XXX[,.(Y=sum(Y)),by=.(ID,X)]
#*
DATA <- DT[LO==FALSE,.(X=SID,Y=VALUE,ID=year(DATE))][, .(Y = mean(Y)), by=.(ID,X)]

plot.subtitle <- ""
plot.title <-  ""
yAxis.label <-  ""
xAxis.label <- "Station"
group.label <- "Year"
tip.formatter <- JS(paste0("function (){return '<b>", xAxis.label, ":</b> ' + this.x + '<br>' + '<b>", yAxis.label, ":</b> ' + this.y + '<br>' + '<b>",group.label,":</b> ' + this.series.name; }"))


buildPlot.categorical(
  data=DATA,
  plot.subtitle = plot.subtitle,
  plot.title = plot.title,
  yAxis.label = yAxis.label,
  xAxis.label =xAxis.label,
  group.label =group.label,
  tip.formatter = tip.formatter,
  plot.theme=hc_theme_smpl())
