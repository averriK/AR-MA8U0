
WL <- fread("data/WL.csv")
CHEMICAL_SELECTED <- "pH de Campo"
CHEMICAL_CHOICES <- c("Aluminio","Sulfatos","Cloruros","Hierro","Calcio","Cobre","Zinc","Arsénico","Niquel","Potasio","Niquel","Manganeso","Magnesio")|> unique() |> sort()
SYS_LOC_CHOICES <- WL$SYS_LOC_CODE |> unique() |> sort()

SYS_LOC_SELECTED <- c("LA-1A","MAR-01","TUN-1","LA-8","SW-9","LA-16") |> unique() |> sort()

# ********************************* ----
# Pressures and Flowrates in Tunnel ----

DT <- read_xlsx("data-raw/P&Q.xlsx") |> as.data.table()
# build long table
ivars <- c("month","day")
mvars <- DT[, -c("month","day")] |>  colnames()
DT <- data.table::melt(DT, id.vars = ivars, measure.vars = mvars) 
# Split identifiers
DT[, c("V1", "V2", "V3", "year") := tstrsplit(variable , "-", fixed = TRUE)]
DT[,variable:=NULL]
# Build Dates
DT[day=="d1",day:=5]
DT[day=="d2",day:=15]
DT[day=="d3",day:=25]
DT[,date:=as.Date(paste0(year,"-",month,"-",day),format="%Y-%m-%d")]

# split pressures
PDT <- DT[V1=="pressure",-c("V1","day","month","year")]
# cosmetic
setnames(PDT,c("value","date","V2","V3"),c("VALUE","DATE","streamID","SID"))

# Remove sufixes
PDT[streamID=="upstream",streamID:="U"]
PDT[streamID=="downstream",streamID:="D"]

PDT[,SID:=gsub(pattern="^ ", replacement="", SID)]
PDT[,SID:=gsub(pattern=" ", replacement="-", SID)]
PDT[,SID:=gsub(pattern="-.*$", replacement="", SID)]
PDT[,SID:=paste0(SID,"(",streamID,")")]
PDT[,streamID:=NULL]
# Remove dates without lectures
PDT <- PDT |> na.omit()

PDT[,ID:="Pressure"]
PDT[,UN:="mH2O"]
PDT <- tagOutliers(PDT,"VALUE")



fwrite(PDT,"data/PDT.csv")
PIEZO_CHOICES <- PDT$SID |> unique() |> sort()
PIEZO_SELECTED <- PIEZO_CHOICES
# ********************************* ----
# Flowrate from Rizotti ----

# Split Flowrates
QDT1 <- DT[V1=="flowrate",-c("V1","V2","day","month","year")]
setnames(QDT1, c("V3","date","value"), c("ID","DATE","VALUE"))
QDT1 <- QDT1 |> na.omit()
QDT1[,ID:="Flow"]
QDT1[,UN:="L/s"]
QDT1[,SID:="Rizotti"]
QDT2 <-  WL[SYS_LOC_CODE=="MAR-1" & CHEMICAL_NAME=="Flujo",.(DATE=SAMPLE_DATE,SID=SYS_LOC_CODE,ID="Flow",VALUE=RESULT_NUMERIC,UN=RESULT_UNIT)]
QDT2[,DATE:=as.Date(DATE)]
QDT <- rbindlist(list(QDT1,QDT2),use.names = TRUE)
QDT <- tagOutliers(QDT,"VALUE")

fwrite(QDT,"data/QDT.csv")
# ********************************* ----
# Flowrate from database ----


# ********************************* ----
# Snow data from Barreal ----
AUX1 <- readxl::read_xlsx("data-raw/SNW-W.xlsx") |> as.data.table()
AUX1 <- melt(AUX1, id.vars = "YEAR", variable.name = "MONTH", value.name = "VALUE")
AUX1 <- AUX1[MONTH!="Total"]
mmap <- c(
  "Ene" = 1, "Feb" = 2, "Mar" = 3, "Abr" = 4, 
  "May" = 5, "Jun" = 6, "Jul" = 7, "Ago" = 8, 
  "Sep" = 9, "Oct" = 10, "Nov" = 11, "Dic" = 12)
AUX1[, MONTH := mmap[MONTH]]
AUX1[,ID:="Snow"]
AUX1[,UN:="cm"]
AUX1[,SID:="Barreal"]
# Snow data from Veladero ----

AUX2 <- readxl::read_xlsx("data-raw/SNW-L.xlsx") |> as.data.table()
AUX2[,DATE:=as.Date(DATE)]
AUX2[,ID:="Snow"]
AUX2[,SID:="Veladero"]
# Add MONTH YEAR columns from DATE
AUX2[,`:=`(YEAR=year(DATE),MONTH=month(DATE))]
# Agregate data by month
AUX2 <- AUX2[,.(VALUE=max(VALUE)),by=.(YEAR,MONTH,SID,ID,UN)]
SNW <- rbindlist(list(AUX1,AUX2),use.names = TRUE)


SNW[, DATE := as.Date(paste0(YEAR,"-",MONTH,"-25"),format="%Y-%m-%d")]

SNW[,`:=`(YEAR=NULL,MONTH=NULL)]
SNW <- tagOutliers(SNW,"VALUE")
fwrite(SNW,"data/SNW.csv")

# ********************************* ----
# Combine data ----

