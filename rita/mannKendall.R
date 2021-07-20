
runmk = function(r){
  mkcalc = function(x){
    if(all(is.na(x))) return(as.numeric(NA))
    hold = capture.output(a <- Kendall::MannKendall(x))
    if(length(hold)>0 && grepl('WARNING', hold)) return(as.numeric(NA))
    return(as.numeric(a[[2]][1]))
  }
  #make things data.tabley
  blah =setDT(as.data.frame(r))
  blah[, id:= .I]
  blah = melt(blah, id.vars = 'id', variable.factor = F)
  #res = blah[, as.integer(mkcalc(value)<.05), by = 'id']
  res = blah[, mkcalc(value), by = 'id']
  rr = r[[1]]
  rr[] = res[, V1]
}
