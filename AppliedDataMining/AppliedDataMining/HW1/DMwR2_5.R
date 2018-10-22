data(algae)
algae <- algae[-manyNAs(algae), ]
fillPO4 <- function(oP){
  if (is.na(oP))
    return(NA)
  else return (42.897 + 1.293 * oP)
}

algae[is.na(algae$PO4), "PO4"] <- sapply(algae[is.na(algae$PO4), "oPO4"], fillPO4)
histogram(~mxPH| season, data = algae)
