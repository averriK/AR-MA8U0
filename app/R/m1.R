m1.ui <- function(id, height = "650px") {
  ns <- NS(id)
  
  nav_panel(
    title = "Water Quality",
    
    tabsetPanel(
      # Flows tab with sub-tabs for Daily and Seasonal
      tabPanel(
        title = "Qo",
        tabsetPanel(
          tabPanel("Q (day)", highchartOutput(ns("TS_flow_day"), height = height)),
          tabPanel("Q (season)", highchartOutput(ns("TS_flow_season"), height = height)),
          tabPanel("Summary (month)", highchartOutput(ns("CP_flow_month"), height = height)),
          tabPanel("Summary (location)", highchartOutput(ns("CP_flow_station"), height = height))
        )
      ),
      
      # Chemicals tab with sub-tabs for Daily and Seasonal
      tabPanel(
        title = "C",
        tabsetPanel(
          tabPanel("C (daily)", highchartOutput(ns("TS_chemical_day"), height = height)),
          tabPanel("C (season)", highchartOutput(ns("TS_chemical_season"), height = height)),
          tabPanel("Q&C (month)", highchartOutput(ns("TS_chemical_flow_month"), height = height)), #tabPanel
          tabPanel("Summary (month)", highchartOutput(ns("CP_chemical_month"), height = height)),
          tabPanel("Summary (location)", highchartOutput(ns("CP_chemical_station"), height = height))
          
        )
      ),
      
      # pH tab with sub-tabs for Daily and Seasonal
      tabPanel(
        title = "pH",
        tabsetPanel(
          tabPanel("pH (day)", highchartOutput(ns("TS_pH_day"), height = height)),
          tabPanel("pH (season)", highchartOutput(ns("TS_pH_season"), height = height)),
          #tabPanel("Q&pH (month)", highchartOutput(ns("TS_pH_flow_month"), height = height)), #tabPanel
          tabPanel("Summary (month)", highchartOutput(ns("CP_pH_month"), height = height)),
          tabPanel("Summary (location)", highchartOutput(ns("CP_pH_station"), height = height))
        )
      ),# tabPanel
      
      tabPanel(
        title = "Hw",
        tabsetPanel(
          tabPanel("Hw (day)", highchartOutput(ns("TS_Hw_day"), height = height)),
          tabPanel("Hw (season)", highchartOutput(ns("TS_Hw_season"), height = height)),
          tabPanel("Summary (month)", highchartOutput(ns("CP_Hw_month"), height = height)),
          tabPanel("Summary (location)", highchartOutput(ns("CP_Hw_station"), height = height))
        )
      ),# tabPanel
      
      
      tabPanel(
        title = "Hs",
        tabsetPanel(
          tabPanel("Hs (day)", highchartOutput(ns("TS_Hs_day"), height = height)),
          tabPanel("Hs (season)", highchartOutput(ns("TS_Hs_season"), height = height)),
          tabPanel("Summary (month)", highchartOutput(ns("CP_Hs_month"), height = height)),
          tabPanel("Summary (location)", highchartOutput(ns("CP_Hs_station"), height = height))
        )
      ),# tabPanel
      
      
    ) # tabsetPanel
    
    
    
    
  ) # nav_panel
}

m1.server <- function(id) {
  moduleServer(
    id, 
    function(input, output, session) {
      ns <- NS(id)
      # build pH/Q timeseries ----
      ts7 <- reactive({
        sys_loc_code <- req(input$sys_loc_code)
        matrix_code = req(input$matrix_code)
        year_min <- min(req(input$years))#2010
        year_max <- max(req(input$years))#2024
        fraction <- req(input$fraction)
        qc_average <- req(input$qc_average)
        DT1 <- buildDataset(
          chemical_name = "pH de Campo",
          year_min=year_min,
          year_max=year_max,
          matrix_code = matrix_code,
          fraction =fraction,
          mass="c",
          sys_loc_code=sys_loc_code)
        # outliers
        if(!is.null(input$outliers) & input$outliers==TRUE ){
          DT1 <- DT1[LO==FALSE]
        }
        DT2 <- buildDataset(
          chemical_name = "Flujo",
          year_min=year_min,
          year_max=year_max,
          matrix_code = matrix_code,
          fraction =fraction,
          mass="q",
          sys_loc_code=sys_loc_code)
        if(!is.null(input$outliers) & input$outliers==TRUE ){
          DT2 <- DT2[LO==FALSE]
        }
        if(input$qc_average=="mean"){
          # browser()
          DT1 <- DT1[order(DATE),.(VALUE=mean(VALUE)),by=.(ID,UN,DATE=get_month_year(DATE))]
          DT2 <- DT2[order(DATE),.(VALUE=mean(VALUE)),by=.(ID,UN,DATE=get_month_year(DATE))]
        }
        if(input$qc_average=="max"){
          # browser()
          DT1 <- DT1[order(DATE),.(VALUE=max(VALUE)),by=.(ID,UN,DATE=get_month_year(DATE))]
          DT2 <- DT2[order(DATE),.(VALUE=max(VALUE)),by=.(ID,UN,DATE=get_month_year(DATE))]
        }
        
        if(input$qc_average=="sum"){
          # browser()
          DT1 <- DT1[order(DATE),.(VALUE=sum(VALUE)),by=.(ID,UN,DATE=get_month_year(DATE))]
          DT2 <- DT2[order(DATE),.(VALUE=sum(VALUE)),by=.(ID,UN,DATE=get_month_year(DATE))]
        }
        
        DT <- rbindlist(list(DT1,DT2),use.names = TRUE)
        # Tag periods
        DT[,B1:=FALSE]
        DT[,B2:=FALSE]
        DT[,B1:=DATE>=as.Date("2011-01-01") & DATE < as.Date("2018-01-01")]
        DT[,B2:=DATE>=as.Date("2018-01-01")]
        
        return(DT)
      })
      
      
      
      # build C/Q timeseries ----
      ts6 <- reactive({
        chemical_name <- req(input$chemical_name)
        sys_loc_code <- req(input$sys_loc_code)
        matrix_code = req(input$matrix_code)
        year_min <- min(req(input$years))#2010
        year_max <- max(req(input$years))#2024
        fraction <- req(input$fraction)
        qc_average <- req(input$qc_average)
        mass <- "c"
        if(!is.null(input$mass_default) & input$mass_default==TRUE){
          mass <- "m"
        } 
        DT1 <- buildDataset(
          chemical_name = chemical_name,
          year_min=year_min,
          year_max=year_max,
          matrix_code = matrix_code,
          fraction =fraction,
          mass=mass,
          sys_loc_code=sys_loc_code)
        # outliers
        if(!is.null(input$outliers) & input$outliers==TRUE ){
          DT1 <- DT1[LO==FALSE]
        }
        DT2 <- buildDataset(
          chemical_name = "Flujo",
          year_min=year_min,
          year_max=year_max,
          matrix_code = matrix_code,
          fraction =fraction,
          mass="q",
          sys_loc_code=sys_loc_code)
        if(!is.null(input$outliers) & input$outliers==TRUE ){
          DT2 <- DT2[LO==FALSE]
        }
        if(input$qc_average=="mean"){
          DT1 <- DT1[order(DATE),.(VALUE=mean(VALUE)),by=.(ID,UN,DATE=get_month_year(DATE))]
          DT2 <- DT2[order(DATE),.(VALUE=mean(VALUE)),by=.(ID,UN,DATE=get_month_year(DATE))]
        }
        if(input$qc_average=="max"){
          DT1 <- DT1[order(DATE),.(VALUE=max(VALUE)),by=.(ID,UN,DATE=get_month_year(DATE))]
          DT2 <- DT2[order(DATE),.(VALUE=max(VALUE)),by=.(ID,UN,DATE=get_month_year(DATE))]
        }
        
        if(input$qc_average=="sum"){
          DT1 <- DT1[order(DATE),.(VALUE=sum(VALUE)),by=.(ID,UN,DATE=get_month_year(DATE))]
          DT2 <- DT2[order(DATE),.(VALUE=sum(VALUE)),by=.(ID,UN,DATE=get_month_year(DATE))]
        }
        
        DT <- rbindlist(list(DT1,DT2),use.names = TRUE)
        # Tag periods
        DT[,B1:=FALSE]
        DT[,B2:=FALSE]
        DT[,B1:=DATE>=as.Date("2011-01-01") & DATE < as.Date("2018-01-01")]
        DT[,B2:=DATE>=as.Date("2018-01-01")]
        
        return(DT)
      })
      
      
      
      # build SNW timeseries ----
      ts5 <- reactive({
        
        year_min <- min(req(input$years))#2010
        year_max <- max(req(input$years))#2024
        DT <- fread("data/SNW.csv")
        DT <- DT[year(DATE)>=year_min & year(DATE)<=year_max]
        
        # Tag periods
        DT[,B1:=FALSE]
        DT[,B2:=FALSE]
        DT[,B1:=DATE>=as.Date("2011-01-01") & DATE < as.Date("2018-01-01")]
        DT[,B2:=DATE>=as.Date("2018-01-01")]
        # outliers
        if(!is.null(input$outliers) & input$outliers==TRUE ){
          DT <- DT[LO==FALSE]
        }
        return(DT)
      })
      
      # build PRESSURE timeseries ----
      ts4 <- reactive({
        piezo_code <- req(input$piezo_code)
        year_min <- min(req(input$years))#2010
        year_max <- max(req(input$years))#2024
        DT <- fread("data/PDT.csv")
        DT <- DT[year(DATE)>=year_min & year(DATE)<=year_max & SID %in% piezo_code]
        
        # Tag periods
        DT[,B1:=FALSE]
        DT[,B2:=FALSE]
        DT[,B1:=DATE>=as.Date("2011-01-01") & DATE < as.Date("2018-01-01")]
        DT[,B2:=DATE>=as.Date("2018-01-01")]
        # outliers
        if(!is.null(input$outliers) & input$outliers==TRUE ){
          DT <- DT[LO==FALSE]
        }
        return(DT)
      })
      
      # build CHEMICAL timeseries ----
      ts1 <- reactive({
        
        chemical_name <- req(input$chemical_name)
        sys_loc_code <- req(input$sys_loc_code)
        matrix_code = req(input$matrix_code)
        year_min <- min(req(input$years))#2010
        year_max <- max(req(input$years))#2024
        fraction <- req(input$fraction)
        
        mass <- "c"
        if(!is.null(input$mass_default) & input$mass_default==TRUE){
          mass <- "m"
        } 
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
        DT[,B1:=DATE>=as.Date("2011-01-01") & DATE < as.Date("2018-01-01")]
        DT[,B2:=DATE>=as.Date("2018-01-01")]
        # outliers
        if(!is.null(input$outliers) & input$outliers==TRUE ){
          DT <- DT[LO==FALSE]
        }
        return(DT)
        
      })
      
      # build FLOW timeseries ----
      ts2 <- reactive({
        
        chemical_name <- "Flujo"
        sys_loc_code <- req(input$sys_loc_code)
        matrix_code = req(input$matrix_code)
        year_min <- min(req(input$years))#2010
        year_max <- max(req(input$years))#2024
        fraction <- "T"
        
        mass <- "q"
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
        DT[,B1:=DATE>=as.Date("2011-01-01") & DATE < as.Date("2018-01-01")]
        DT[,B2:=DATE>=as.Date("2018-01-01")]
        # outliers
        if(!is.null(input$outliers) & input$outliers==TRUE ){
          DT <- DT[LO==FALSE]
        }
        return(DT)
        
      })
      
      # build pH timeseries ----
      ts3 <- reactive({
        
        # chemical_name <- "pH de Laboratorio"
        chemical_name <- "pH de Campo"
        sys_loc_code <- req(input$sys_loc_code)
        matrix_code = req(input$matrix_code)
        year_min <- min(req(input$years))#2010
        year_max <- max(req(input$years))#2024
        fraction <- "T"
        
        mass <- "c"
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
        DT[,B1:=DATE>=as.Date("2011-01-01") & DATE < as.Date("2018-01-01")]
        DT[,B2:=DATE>=as.Date("2018-01-01")]
        # outliers
        if(!is.null(input$outliers) & "local" %in% input$outliers){
          DT <- DT[LO==FALSE]
        }
        
        return(DT)
        
      })
      
      
      # output chemical----
      output[["TS_chemical_day"]] <- renderHighchart({
        req(ts1())
        chemical_name <- ts1()$ID[1]
        chemical_unit <- ts1()$UN[1]
        # matrix_code = req(input$matrix_code)
        time_width <- req(as.numeric(input$time_width))
        plot.type <- c("point")
        if(!is.null(input$mav_default) & input$mav_default==TRUE){
          plot.type <- c("point","average")
        } 
        DATA <- ts1()[order(DATE),.(X=DATE,Y=VALUE,B1=B1,B2=B2,ID=SID)][, .(Y = mean(Y),B1,B2), by = .(ID, X)] 
        yAxis.label <-  paste0(chemical_name, " [", chemical_unit, "]")
        xAxis.label <- "Date"
        group.label <-  "Station"
        tip.formatter <- JS(paste0("function () { return '<b>", xAxis.label, ":</b> ' + Highcharts.dateFormat('%Y-%m-%d', this.x) + '<br>' + '<b>", "Value:</b> ' + this.y + '<br>' + '<b>", group.label, ":</b> ' + this.series.name; }"))
        
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
        PLOT
      })
      output[["TS_chemical_flow_month"]] <- renderHighchart({
        req(ts6())
        chemical_name <- ts6()[ID!="FLUJO"]$ID[1]
        chemical_unit <- ts6()[ID!="FLUJO"]$UN[1]
        ID1 <- chemical_name
       
        y1Axis.label <-  paste0(chemical_name, " [", chemical_unit, "]")
        y2Axis.label <-  "FLOW [L/s]"
        ID2 <- "FLUJO"
        xAxis.label <- "Month"
        band1.label <- "Tunnel Open"
        band2.label <- "Tunnel Plugged"
        group.label <- "Group"
        

        tip.formatter <- JS(paste0("function (){return '<b>", xAxis.label, ":</b> ' + Highcharts.dateFormat('%Y-%m', this.x) + '<br>'  + this.series.name + this.y  ; }"))
        
        
        # DATA<- ts6()[order(DATE),.(Y=VALUE,B1=B1,B2=B2,ID=ID,X=DATE,SID=SID)]
        DATA<- ts6()[order(DATE),.(Y=VALUE,B1=B1,B2=B2,ID=ID,X=DATE)]
        

        PLOT <- buildPlot.timeseries.combined(
          data=DATA,
          id1=ID1,
          id2=ID2,
          plot.subtitle = "",
          plot.title = y1Axis.label,
          y1Axis.label = y1Axis.label,
          y2Axis.label = y2Axis.label,
          xAxis.label =xAxis.label,
          group.label = group.label,
          band1.label="Tunnel Open",
          band2.label="Tunnel Plugged",
          tip.formatter = tip.formatter,
          plot.bands = TRUE,
          plot.theme=hc_theme_smpl())
        PLOT
      })
      output[["TS_chemical_season"]] <- renderHighchart({
        req(ts1())
        chemical_name <- ts1()$ID[1]
        chemical_unit <- ts1()$UN[1]
        time_width <- req(as.numeric(input$time_width))
        
        DATA <- ts1()[order(DATE),.(X=DATE,Y=VALUE,B1=B1,B2=B2,ID=SID)][,.(Y=mean(Y) ),by=.(ID,X=get_season_year(X))]
        #
        yAxis.label <-  paste0(chemical_name, " [", chemical_unit, "]")
        xAxis.label <- "Season"
        group.label <-  "Station"
        tip.formatter <- JS(paste0("function (){return '<b>", xAxis.label, ":</b> ' + this.x + '<br>' + '<b>", yAxis.label, ":</b> ' + this.y + '<br>' + '<b>",group.label,":</b> ' + this.series.name; }"))
        
        PLOT <- buildPlot.categorical(
          data=DATA,
          plot.subtitle = "",
          plot.title = yAxis.label,
          yAxis.label = yAxis.label,
          xAxis.label =xAxis.label,
          group.label =group.label,
          tip.formatter = tip.formatter,
          plot.theme=hc_theme_smpl())
        PLOT
      })
      output[["CP_chemical_month"]] <- renderHighchart({
        req(ts1())
        chemical_name <- ts1()$ID[1]
        chemical_unit <- ts1()$UN[1]
        DATA <- ts1()[order(DATE),.(X=DATE,Y=VALUE,B1=B1,B2=B2,ID=SID)][, .(Y = mean(Y)), by=.(ID,X=month.abb[month(X)])][order(match(X, month.abb)) ]
        
        yAxis.label <-  paste0(chemical_name, " [", chemical_unit, "]")
        xAxis.label <- "Month (average)"
        group.label <-  "Station"
        tip.formatter <- JS(paste0("function (){return '<b>", xAxis.label, ":</b> ' + Highcharts.dateFormat('%b', this.x) + '<br>' + '<b>", yAxis.label, ":</b> ' + this.y + '<br>' + '<b>",group.label,":</b> ' + this.series.name; }"))
        
        PLOT <- buildPlot.categorical(
          data=DATA,
          plot.subtitle = "",
          plot.title = yAxis.label,
          yAxis.label = yAxis.label,
          xAxis.label =xAxis.label,
          group.label =group.label,
          tip.formatter = tip.formatter,
          plot.theme=hc_theme_smpl())
        PLOT
      })
      output[["CP_chemical_station"]] <- renderHighchart({
        req(ts1())
        chemical_name <- ts1()$ID[1]
        chemical_unit <- ts1()$UN[1]
        
        DATA <- ts1()[order(DATE),.(X=SID,Y=VALUE,ID=year(DATE))][, .(Y = mean(Y)), by=.(ID,X)]
        yAxis.label <-  paste0(chemical_name, " [", chemical_unit, "]")
        xAxis.label <- "Station"
        group.label <-  "Year"
        tip.formatter <- JS(paste0("function (){return '<b>", xAxis.label, ":</b> ' + this.x + '<br>' + '<b>", yAxis.label, ":</b> ' + this.y + '<br>' + '<b>",group.label,":</b> ' + this.series.name; }"))
        
        PLOT <- buildPlot.categorical(
          data=DATA,
          plot.subtitle = "",
          plot.title = yAxis.label,
          yAxis.label = yAxis.label,
          xAxis.label =xAxis.label,
          group.label =group.label,
          tip.formatter = tip.formatter,
          plot.theme=hc_theme_smpl())
        PLOT
      })
      
      # output Flow ----
      output[["TS_flow_day"]] <- renderHighchart({
        req(ts2())
        chemical_name <- "Flow rate"
        chemical_unit <- ts2()$UN[1]
        # matrix_code = req(input$matrix_code)
        time_width <- req(as.numeric(input$time_width))
        plot.type <- c("point")
        if(!is.null(input$mav_default) & input$mav_default==TRUE){
          plot.type <- c("point","average")
        } 
        DATA <- ts2()[order(DATE),.(X=DATE,Y=VALUE,B1=B1,B2=B2,ID=SID)][, .(Y = mean(Y),B1,B2), by = .(ID, X)] # Keep X as datetime
        
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
        PLOT
      })
      output[["TS_flow_season"]] <- renderHighchart({
        req(ts2())
        chemical_name <- "Flow rate"
        chemical_unit <- ts2()$UN[1]
        
        # ID=paste0(SID,"[",matrix_code,"]")
        DATA <- ts2()[order(DATE),.(X=DATE,Y=VALUE,B1=B1,B2=B2,ID=SID)][,.(Y=mean(Y) ),by=.(ID,X=get_season_year(X))]
        
        yAxis.label <-  paste0(chemical_name, " [", chemical_unit, "]")
        xAxis.label <- "Season"
        group.label <-  "Station"
        tip.formatter <- JS(paste0("function (){return '<b>", xAxis.label, ":</b> ' + this.x + '<br>' + '<b>", yAxis.label, ":</b> ' + this.y + '<br>' + '<b>",group.label,":</b> ' + this.series.name; }"))
        
        PLOT <- buildPlot.categorical(
          data=DATA,
          plot.subtitle = "",
          plot.title = yAxis.label,
          yAxis.label = yAxis.label,
          xAxis.label =xAxis.label,
          group.label =group.label,
          tip.formatter = tip.formatter,
          plot.theme=hc_theme_smpl())
        PLOT
      })
      output[["CP_flow_month"]] <- renderHighchart({
        req(ts2())
        chemical_name <- "Flow rate"
        chemical_unit <- ts2()$UN[1]
        DATA <- ts2()[order(DATE),.(X=DATE,Y=VALUE,B1=B1,B2=B2,ID=SID)][, .(Y = mean(Y)), by=.(ID,X=month.abb[month(X)])][order(match(X, month.abb)) ]
        
        yAxis.label <-  paste0(chemical_name, " [", chemical_unit, "]")
        xAxis.label <- "Month (average)"
        group.label <-  "Station"
        tip.formatter <- JS(paste0("function (){return '<b>", xAxis.label, ":</b> ' + Highcharts.dateFormat('%b', this.x) + '<br>' + '<b>", yAxis.label, ":</b> ' + this.y + '<br>' + '<b>",group.label,":</b> ' + this.series.name; }"))
        
        PLOT <- buildPlot.categorical(
          data=DATA,
          plot.subtitle = "",
          plot.title = yAxis.label,
          yAxis.label = yAxis.label,
          xAxis.label =xAxis.label,
          group.label =group.label,
          tip.formatter = tip.formatter,
          plot.theme=hc_theme_smpl())
        PLOT
      })
      output[["CP_flow_station"]] <- renderHighchart({
        req(ts2())
        chemical_name <- "Flow rate"
        chemical_unit <- ts2()$UN[1]
        
        DATA <- ts2()[order(DATE),.(X=SID,Y=VALUE,ID=year(DATE))][, .(Y = mean(Y)), by=.(ID,X)]
        yAxis.label <-  paste0(chemical_name, " [", chemical_unit, "]")
        xAxis.label <- "Station"
        group.label <-  "Year"
        tip.formatter <- JS(paste0("function (){return '<b>", xAxis.label, ":</b> ' + this.x + '<br>' + '<b>", yAxis.label, ":</b> ' + this.y + '<br>' + '<b>",group.label,":</b> ' + this.series.name; }"))
        
        PLOT <- buildPlot.categorical(
          data=DATA,
          plot.subtitle = "",
          plot.title = yAxis.label,
          yAxis.label = yAxis.label,
          xAxis.label =xAxis.label,
          group.label =group.label,
          tip.formatter = tip.formatter,
          plot.theme=hc_theme_smpl())
        PLOT
      })
      # output pH ----
      output[["TS_pH_day"]] <- renderHighchart({
        req(ts3())
        chemical_name <- "pH"
        chemical_unit <- "pH units"
        time_width <- req(as.numeric(input$time_width))
        plot.type <- c("point")
        if(!is.null(input$mav_default) & input$mav_default==TRUE){
          plot.type <- c("point","average")
        } 
        DATA <- ts3()[order(DATE),.(X=DATE,Y=VALUE,B1=B1,B2=B2,ID=SID)][, .(Y = mean(Y),B1,B2), by = .(ID, X)] # Keep X as datetime
        
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
        PLOT
      })
      output[["TS_pH_flow_month"]] <- renderHighchart({
        req(ts7())
        chemical_name <- "pH"
        chemical_unit <- "pH units"
        ID1 <- chemical_name
        
        y1Axis.label <-  paste0(chemical_name, " [", chemical_unit, "]")
        y2Axis.label <-  "FLOW [L/s]"
        ID2 <- "FLUJO"
        xAxis.label <- "Month"
        band1.label <- "Tunnel Open"
        band2.label <- "Tunnel Plugged"
        group.label <- "Group"
        
        
        tip.formatter <- JS(paste0("function (){return '<b>", xAxis.label, ":</b> ' + Highcharts.dateFormat('%Y-%m', this.x) + '<br>'  + this.series.name + this.y  ; }"))
        
        
        # DATA<- ts6()[order(DATE),.(Y=VALUE,B1=B1,B2=B2,ID=ID,X=DATE,SID=SID)]
        DATA<- ts7()[order(DATE),.(Y=VALUE,B1=B1,B2=B2,ID=ID,X=DATE)]
        
        
        PLOT <- buildPlot.timeseries.combined(
          data=DATA,
          id1=ID1,
          id2=ID2,
          plot.subtitle = "",
          plot.title = y1Axis.label,
          y1Axis.label = y1Axis.label,
          y2Axis.label = y2Axis.label,
          xAxis.label =xAxis.label,
          group.label = group.label,
          band1.label="Tunnel Open",
          band2.label="Tunnel Plugged",
          tip.formatter = tip.formatter,
          plot.bands = TRUE,
          plot.theme=hc_theme_smpl())
        PLOT
      })
      output[["TS_pH_season"]] <- renderHighchart({
        req(ts3())
        chemical_name <- "pH"
        chemical_unit <- "pH units"
        DATA <- ts3()[order(DATE),.(X=DATE,Y=VALUE,B1=B1,B2=B2,ID=SID)][,.(Y=mean(Y) ),by=.(ID,X=get_season_year(X))]
        
        yAxis.label <-  paste0(chemical_name, " [", chemical_unit, "]")
        xAxis.label <- "Season"
        group.label <-  "Station"
        tip.formatter <- JS(paste0("function (){return '<b>", xAxis.label, ":</b> ' + this.x + '<br>' + '<b>", yAxis.label, ":</b> ' + this.y + '<br>' + '<b>",group.label,":</b> ' + this.series.name; }"))
        
        PLOT <- buildPlot.categorical(
          data=DATA,
          plot.subtitle = "",
          plot.title = yAxis.label,
          yAxis.label = yAxis.label,
          xAxis.label =xAxis.label,
          group.label =group.label,
          tip.formatter = tip.formatter,
          plot.theme=hc_theme_smpl())
        PLOT
      })
      output[["CP_pH_month"]] <- renderHighchart({
        req(ts3())
        chemical_name <- "pH"
        chemical_unit <- "pH units"
        DATA <- ts3()[order(DATE),.(X=DATE,Y=VALUE,B1=B1,B2=B2,ID=SID)][, .(Y = mean(Y)), by=.(ID,X=month.abb[month(X)])][order(match(X, month.abb)) ]
        
        yAxis.label <-  paste0(chemical_name, " [", chemical_unit, "]")
        xAxis.label <- "Month (average)"
        group.label <-  "Station"
        tip.formatter <- JS(paste0("function (){return '<b>", xAxis.label, ":</b> ' + Highcharts.dateFormat('%b', this.x) + '<br>' + '<b>", yAxis.label, ":</b> ' + this.y + '<br>' + '<b>",group.label,":</b> ' + this.series.name; }"))
        
        PLOT <- buildPlot.categorical(
          data=DATA,
          plot.subtitle = "",
          plot.title = yAxis.label,
          yAxis.label = yAxis.label,
          xAxis.label =xAxis.label,
          group.label =group.label,
          tip.formatter = tip.formatter,
          plot.theme=hc_theme_smpl())
        PLOT
      })
      output[["CP_pH_station"]] <- renderHighchart({
        req(ts3())
        chemical_name <- "pH"
        chemical_unit <- "pH units"
        
        DATA <- ts3()[order(DATE),.(X=SID,Y=VALUE,ID=year(DATE))][, .(Y = mean(Y)), by=.(ID,X)]
        yAxis.label <-  paste0(chemical_name, " [", chemical_unit, "]")
        xAxis.label <- "Station"
        group.label <-  "Year"
        tip.formatter <- JS(paste0("function (){return '<b>", xAxis.label, ":</b> ' + this.x + '<br>' + '<b>", yAxis.label, ":</b> ' + this.y + '<br>' + '<b>",group.label,":</b> ' + this.series.name; }"))
        
        PLOT <- buildPlot.categorical(
          data=DATA,
          plot.subtitle = "",
          plot.title = yAxis.label,
          yAxis.label = yAxis.label,
          xAxis.label =xAxis.label,
          group.label =group.label,
          tip.formatter = tip.formatter,
          plot.theme=hc_theme_smpl())
        PLOT
      })
      
      # output pressure ----
      output[["TS_Hw_day"]] <- renderHighchart({
        req(ts4())
        chemical_name <- "Hw"
        chemical_unit <- "m"
        time_width <- req(as.numeric(input$time_width))
        plot.type <- c("point")
        if(!is.null(input$mav_default) & input$mav_default==TRUE){
          plot.type <- c("point","average")
        } 
        DATA <- ts4()[order(DATE),.(X=DATE,Y=VALUE,B1=B1,B2=B2,ID=SID)][, .(Y = mean(Y),B1,B2), by = .(ID, X)] # Keep X as datetime
        
        yAxis.label <-  paste0(chemical_name, " [", chemical_unit, "]")
        xAxis.label <- "Date"
        group.label <-  "Station"
        tip.formatter <- JS(paste0("function () { return '<b>", xAxis.label, ":</b> ' + Highcharts.dateFormat('%Y-%m-%d', this.x) + '<br>' + '<b>", yAxis.label, ":</b> ' + this.y + '<br>' + '<b>", group.label, ":</b> ' + this.series.name; }"))
        Y1.max <- NULL
        Y2.max <- NULL
        if(!is.null(input$max_default) & input$max_default==TRUE){
          Y1.max <- 240
          Y2.max <- 80
        } 
        PLOT <- buildPlot.timeseries(
          data=DATA,
          Y2.max=Y1.max,
          Y1.max=Y2.max,
          Y1.label="Service Load [m]",
          Y2.label="Ultimate Load [m]",
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
        PLOT
      })
      output[["TS_Hw_season"]] <- renderHighchart({
        req(ts4())
        chemical_name <- "Hw"
        chemical_unit <- "m"
        DATA <- ts4()[order(DATE),.(X=DATE,Y=VALUE,B1=B1,B2=B2,ID=SID)][,.(Y=mean(Y) ),by=.(ID,X=get_season_year(X))]
        
        yAxis.label <-  paste0(chemical_name, " [", chemical_unit, "]")
        xAxis.label <- "Season"
        group.label <-  "Station"
        tip.formatter <- JS(paste0("function (){return '<b>", xAxis.label, ":</b> ' + this.x + '<br>' + '<b>", yAxis.label, ":</b> ' + this.y + '<br>' + '<b>",group.label,":</b> ' + this.series.name; }"))
        
        PLOT <- buildPlot.categorical(
          data=DATA,
          plot.subtitle = "",
          plot.title = yAxis.label,
          yAxis.label = yAxis.label,
          xAxis.label =xAxis.label,
          group.label =group.label,
          tip.formatter = tip.formatter,
          plot.theme=hc_theme_smpl())
        PLOT
      })
      output[["CP_Hw_month"]] <- renderHighchart({
        req(ts4())
        chemical_name <- "Hw"
        chemical_unit <- "m"
        DATA <- ts4()[order(DATE),.(X=DATE,Y=VALUE,B1=B1,B2=B2,ID=SID)][, .(Y = mean(Y)), by=.(ID,X=month.abb[month(X)])][order(match(X, month.abb)) ]
        
        yAxis.label <-  paste0(chemical_name, " [", chemical_unit, "]")
        xAxis.label <- "Month (average)"
        group.label <-  "Station"
        tip.formatter <- JS(paste0("function (){return '<b>", xAxis.label, ":</b> ' + Highcharts.dateFormat('%b', this.x) + '<br>' + '<b>", yAxis.label, ":</b> ' + this.y + '<br>' + '<b>",group.label,":</b> ' + this.series.name; }"))
        
        PLOT <- buildPlot.categorical(
          data=DATA,
          plot.subtitle = "",
          plot.title = yAxis.label,
          yAxis.label = yAxis.label,
          xAxis.label =xAxis.label,
          group.label =group.label,
          tip.formatter = tip.formatter,
          plot.theme=hc_theme_smpl())
        PLOT
      })
      output[["CP_Hw_station"]] <- renderHighchart({
        req(ts4())
        chemical_name <- "Hw"
        chemical_unit <- "m"
        
        DATA <- ts4()[order(DATE),.(X=SID,Y=VALUE,ID=year(DATE))][, .(Y = mean(Y)), by=.(ID,X)]
        yAxis.label <-  paste0(chemical_name, " [", chemical_unit, "]")
        xAxis.label <- "Station"
        group.label <-  "Year"
        tip.formatter <- JS(paste0("function (){return '<b>", xAxis.label, ":</b> ' + this.x + '<br>' + '<b>", yAxis.label, ":</b> ' + this.y + '<br>' + '<b>",group.label,":</b> ' + this.series.name; }"))
        
        PLOT <- buildPlot.categorical(
          data=DATA,
          plot.subtitle = "",
          plot.title = yAxis.label,
          yAxis.label = yAxis.label,
          xAxis.label =xAxis.label,
          group.label =group.label,
          tip.formatter = tip.formatter,
          plot.theme=hc_theme_smpl())
        PLOT
      })
      
      
      # output snow ----
      output[["TS_Hs_day"]] <- renderHighchart({
        req(ts5())
        chemical_name <- "Hs"
        chemical_unit <- "cm"
        time_width <- req(as.numeric(input$time_width))
        plot.type <- c("point")
        if(!is.null(input$mav_default) & input$mav_default==TRUE){
          plot.type <- c("point","average")
        } 
        DATA <- ts5()[order(DATE),.(X=DATE,Y=VALUE,B1=B1,B2=B2,ID=SID)][, .(Y = mean(Y),B1,B2), by = .(ID, X)] # Keep X as datetime
        
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
          band1.label=band1.label,
          band2.label=band2.label,
          tip.formatter = tip.formatter,
          plot.bands = TRUE,
          plot.theme=hc_theme_smpl())
        PLOT
      })
      output[["TS_Hs_season"]] <- renderHighchart({
        req(ts5())
        chemical_name <- "Hs"
        chemical_unit <- "cm"
        DATA <- ts5()[order(DATE),.(X=DATE,Y=VALUE,B1=B1,B2=B2,ID=SID)][,.(Y=mean(Y) ),by=.(ID,X=get_season_year(X))]
        
        yAxis.label <-  paste0(chemical_name, " [", chemical_unit, "]")
        xAxis.label <- "Season"
        group.label <-  "Station"
        tip.formatter <- JS(paste0("function (){return '<b>", xAxis.label, ":</b> ' + this.x + '<br>' + '<b>", yAxis.label, ":</b> ' + this.y + '<br>' + '<b>",group.label,":</b> ' + this.series.name; }"))
        
        PLOT <- buildPlot.categorical(
          data=DATA,
          plot.subtitle = "",
          plot.title = yAxis.label,
          yAxis.label = yAxis.label,
          xAxis.label =xAxis.label,
          group.label =group.label,
          tip.formatter = tip.formatter,
          plot.theme=hc_theme_smpl())
        PLOT
      })
      output[["CP_Hs_month"]] <- renderHighchart({
        req(ts5())
        chemical_name <- "Hs"
        chemical_unit <- "cm"
        DATA <- ts5()[order(DATE),.(X=DATE,Y=VALUE,B1=B1,B2=B2,ID=SID)][, .(Y = mean(Y)), by=.(ID,X=month.abb[month(X)])][order(match(X, month.abb)) ]
        
        yAxis.label <-  paste0(chemical_name, " [", chemical_unit, "]")
        xAxis.label <- "Month (average)"
        group.label <-  "Station"
        tip.formatter <- JS(paste0("function (){return '<b>", xAxis.label, ":</b> ' + Highcharts.dateFormat('%b', this.x) + '<br>' + '<b>", yAxis.label, ":</b> ' + this.y + '<br>' + '<b>",group.label,":</b> ' + this.series.name; }"))
        
        PLOT <- buildPlot.categorical(
          data=DATA,
          plot.subtitle = "",
          plot.title = yAxis.label,
          yAxis.label = yAxis.label,
          xAxis.label =xAxis.label,
          group.label =group.label,
          tip.formatter = tip.formatter,
          plot.theme=hc_theme_smpl())
        PLOT
      })
      output[["CP_Hs_station"]] <- renderHighchart({
        req(ts5())
        chemical_name <- "Hs"
        chemical_unit <- "cm"
        
        DATA <- ts5()[order(DATE),.(X=SID,Y=VALUE,ID=year(DATE))][, .(Y = mean(Y)), by=.(ID,X)]
        yAxis.label <-  paste0(chemical_name, " [", chemical_unit, "]")
        xAxis.label <- "Station"
        group.label <-  "Year"
        tip.formatter <- JS(paste0("function (){return '<b>", xAxis.label, ":</b> ' + this.x + '<br>' + '<b>", yAxis.label, ":</b> ' + this.y + '<br>' + '<b>",group.label,":</b> ' + this.series.name; }"))
        
        PLOT <- buildPlot.categorical(
          data=DATA,
          plot.subtitle = "",
          plot.title = yAxis.label,
          yAxis.label = yAxis.label,
          xAxis.label =xAxis.label,
          group.label =group.label,
          tip.formatter = tip.formatter,
          plot.theme=hc_theme_smpl())
        PLOT
      })
      
    }
  )
}


m1.sidebar <- function(id) {
  ns <- NS(id)
  tagList(
    # time ----
    
    sliderTextInput(
      inputId = ns("years"),
      label = "Interval (yr):",
      choices = seq(2010,2024,1),
      selected = c(2010,2024),
      grid = TRUE
    ),
    
    # chemical ----
    
    virtualSelectInput(
      inputId = ns("chemical_name"),
      label = "Chemical:   ",  
      choices = CHEMICAL_CHOICES ,
      autoSelectFirstOption=FALSE,
      multiple = FALSE,
      disableSelectAll=TRUE,
      dropboxWrapper = "body",
      search = TRUE,
      width = "50%",
      inline = FALSE
    ),
    
    # mass ----
    
    awesomeCheckbox(
      inputId = ns("mass_default"),
      label = "Normalize by flowrate:",
      value = FALSE
    ),
    
    # outliers ----
    
    awesomeCheckbox(
      inputId = ns("outliers"),
      label = "Remove outliers:",
      value = TRUE
    ),
    
    
    # mav ----
    awesomeCheckbox(
      inputId = ns("mav_default"),
      label = "Moving average:",
      value = FALSE
    ),    
    conditionalPanel(
      condition = sprintf("input['%s'] == true", ns("mav_default")),
      sliderTextInput(
        inputId = ns("time_width"),
        label = NULL,
        choices = seq(3,30,3),
        selected = 15,
        grid = TRUE
      ),
    ),
    
    # sys_loc_code ----
    virtualSelectInput(
      inputId = ns("sys_loc_code"),
      label = "Location:   ",  
      choices = SYS_LOC_CHOICES,
      selected = SYS_LOC_SELECTED,
      autoSelectFirstOption=FALSE,
      multiple = TRUE,
      disableSelectAll=FALSE,
      dropboxWrapper = "body",
      search = TRUE,
      inline = FALSE,
      width = "50%"
    ),
    # matrix_code ----
    awesomeRadio(
      inputId = ns("matrix_code"),
      label = "Matrix:",
      choices = c("GW","SW"),
      selected = "SW",
      inline = TRUE,
      checkbox = TRUE,
      status = "primary",
      width = NULL
    ),
    
    # fraction ----
    awesomeRadio(
      inputId = ns("fraction"),
      label = "Fraction:",
      choices = c("T","D" ),
      selected = "T",
      inline = TRUE,
      checkbox = TRUE,
      status = "primary",
      width = NULL
    ),
    
    # piezo_code ----
    virtualSelectInput(
      inputId = ns("piezo_code"),
      label = "Piezometer:",  
      choices = PIEZO_CHOICES ,
      selected = PIEZO_SELECTED,
      autoSelectFirstOption=TRUE,
      multiple = TRUE,
      disableSelectAll=FALSE,
      dropboxWrapper = "body",
      search = TRUE,
      width = "50%",
      inline = FALSE
    ),
    # MAX PRESSURE ----
    awesomeCheckbox(
      inputId = ns("max_default"),
      label = "Limit States:",
      value = FALSE
    ),
    
    # matrix_code ----
    awesomeRadio(
      inputId = ns("qc_average"),
      label = "Summary Q&C:",
      choices = c("max","mean","sum"),
      selected = "mean",
      inline = TRUE,
      checkbox = TRUE,
      status = "primary",
      width = NULL
    ),
    
  ) # tagList
  
  
}


