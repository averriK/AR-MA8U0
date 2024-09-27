Caption <- CAP |> sprintf(p_TARGET)
AUX <- SHT[level==p_TARGET,.(ID,USCS,level,b,s,Hs,Go,VSo,mo,lo,an,Ts)]

TBL <- AUX |> buildTable(library=LIB,font.size.body = FONT.SIZE.BODY,font.size.header = FONT.SIZE.HEADER,font.bold.header=TRUE,caption=Caption)
