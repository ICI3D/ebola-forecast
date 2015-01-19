## import data
require(data.table)
importer <- function(target, date.format = "%d-%b-%Y") 
  data.table(within(read.csv(target,header=TRUE,as.is=TRUE), {
    Date <- as.Date(Date, format=date.format)
  }, key = "Date"))

melter <- function(target) {
  res <- melt(target[, grep("(Date|_)", colnames(target), value = T), with=FALSE], id.vars = c("Date"), value.name = "cases")
  res[,Location := factor(sub("_.*$", "", as.character(variable)))]
  res[,Status   := factor(sub("^.*_", "", as.character(variable)))]
  res$variable <- NULL
  res
}
