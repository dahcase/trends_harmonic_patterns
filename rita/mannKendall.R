
runmk = function(r){
  mkcalc = function(x){
    if(all(is.na(x))) return(as.numeric(NA))
    hold = capture.output(a <- Kendall::MannKendall(x))
    if(length(hold)>0 && grepl('WARNING', hold)) return(as.numeric(NA))
    return(as.numeric(a[[2]][1]))
  }
  #make things data.tabley
  blah =setDT(as.data.frame(r, cells = TRUE, na.rm = FALSE)) #terra instructions
  blah = melt(blah, id.vars = 'cell', variable.factor = F)
  #res = blah[, as.integer(mkcalc(value)<.05), by = 'id']
  res = blah[, mkcalc(value), by = 'cell']
  rr = r[[1]]
  setorder(res, cell)
  rr[] = res[, V1]
  
  rr
}
