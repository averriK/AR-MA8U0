source("setup.R")
source("global.R")
WL <- fread("data-raw/RAW.csv")

# keep columns in RAW that exists in SW 
COLS <-  c("SYS_LOC_CODE","SAMPLE_ID", "SAMPLE_DATE","SAMPLE_TYPE_CODE", "MATRIX_CODE","FRACTION", "CAS_RN","CHEMICAL_NAME", "ORGANIC_YN","REPORTABLE_RESULT", "DETECT_FLAG","METHOD_DETECTION_LIMIT", "DETECTION_LIMIT_UNIT","RESULT_NUMERIC", "RESULT_UNIT","LONGITUDE", "LATITUDE")
WL <- WL[, names(WL) %in% COLS, with = FALSE]
# Remove OR
# Clean Matrix Codes
WL$MATRIX_CODE |> unique()
WL[MATRIX_CODE=="sw",MATRIX_CODE:="SW"]
WL <- WL[MATRIX_CODE!="N/D" & MATRIX_CODE!="W"]
# Fix organic flag
WL[ORGANIC_YN=="",ORGANIC_YN:="N"]
WL[ORGANIC_YN=="Y",ORGANIC_YN:=TRUE]
WL[ORGANIC_YN=="N",ORGANIC_YN:=FALSE]
WL[,ORGANIC_FLAG:=as.logical(ORGANIC_YN)]
WL[,ORGANIC_YN:=NULL]

WL$ORGANIC_YN |> unique()
# Clean Fraction Codes
WL$FRACTION |> unique()
# 
WL[SAMPLE_TYPE_CODE=="dc",SAMPLE_TYPE_CODE:="DC"]
# Set logical flags
WL$REPORTABLE_RESULT |> unique()
WL[REPORTABLE_RESULT=="YNo", REPORTABLE_RESULT := FALSE]
WL[, REPORTABLE_RESULT := gsub("\\bno\\b", FALSE, REPORTABLE_RESULT, ignore.case = TRUE)]
WL[, REPORTABLE_RESULT := gsub("\\byes\\b", TRUE, REPORTABLE_RESULT, ignore.case = TRUE)]
WL[,REPORTABLE_RESULT:=as.logical(REPORTABLE_RESULT)]

WL$REPORTABLE_RESULT |> unique()
names(WL)

# clean DETECT_FLAG
WL$DETECT_FLAG |> unique()
WL[, DETECT_FLAG := gsub("\\bn\\b", FALSE, DETECT_FLAG, ignore.case = TRUE)]
WL[, DETECT_FLAG := gsub("\\by\\b", TRUE, DETECT_FLAG, ignore.case = TRUE)]
WL[,DETECT_FLAG:=as.logical(DETECT_FLAG)]


#
WL[, SAMPLE_DATE := as.Date(SAMPLE_DATE, format = "%d/%m/%Y")] 
WL[,SYS_LOC_CODE:=SYS_LOC_CODE |> toupper() |> gsub(pattern=" ",replacement = ".")]
WL[,MATRIX_CODE:=MATRIX_CODE |> toupper() |> gsub(pattern=" ",replacement = ".")]
WL[,FRACTION:=FRACTION |> toupper() |> gsub(pattern=" ",replacement = ".")]

COLS <- c("SYS_LOC_CODE","CHEMICAL_NAME","MATRIX_CODE","CAS_RN","SAMPLE_TYPE_CODE","SAMPLE_ID")  
WL[, (COLS) := lapply(.SD, function(x) {
  x <- stringr::str_trim(x)  # Remove leading and trailing spaces
  #toupper(x)  # Convert to uppercase (or use tolower(x) to convert to lowercase)
}), .SDcols = COLS]


# Clean RESULT_NUMERIC
WL[is.na(as.numeric(RESULT_NUMERIC))]$RESULT_NUMERIC
WL <- WL[!is.na(RESULT_NUMERIC)]
WL[is.na(as.numeric(RESULT_NUMERIC))]$RESULT_NUMERIC
WL[,RESULT_NUMERIC:=as.numeric(RESULT_NUMERIC)]

# Clean METHOD_DETECTION_LIMIT
# Replace ","
WL[,METHOD_DETECTION_LIMIT:=gsub(pattern=",", replacement=".", METHOD_DETECTION_LIMIT)]

# Replace ""
WL[METHOD_DETECTION_LIMIT=="",METHOD_DETECTION_LIMIT:=0]
# Check
WL[is.na(as.numeric(METHOD_DETECTION_LIMIT))]$METHOD_DETECTION_LIMIT
WL[,METHOD_DETECTION_LIMIT:=as.numeric(METHOD_DETECTION_LIMIT)]

# fix detect flag
WL[RESULT_NUMERIC==0 & DETECT_FLAG==TRUE,DETECT_FLAG:=FALSE]


# FIX NA columns
names(WL)[(colSums(is.na(WL)) > 0)]
# LATITUDE LONGITUDE has NA values. Fill (impute) with mean
FIX_LAT <- WL[!is.na(LATITUDE)]$LATITUDE |> unique() |> mean()
FIX_LON <- WL[!is.na(LONGITUDE)]$LONGITUDE |> unique() |> mean()  
WL[is.na(LATITUDE),LATITUDE:=FIX_LAT]
WL[is.na(LONGITUDE),LONGITUDE:=FIX_LON]
#remove duplicates
WL <- unique(WL)

# Write
fwrite(WL,"data/WL.csv")



# combined data
