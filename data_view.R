## data display plot

plot_base <- function(src, forecast_date = Sys.Date(), censor_interval = 7, include_interval = 60, fit_ref) {
  end_date <- max(src$Date) - censor_interval
  start_date <- end_date - include_interval
  src$fit <- expcurve(fit_ref, date_zero = as.numeric(start_date))(as.numeric(src$Date))
  # + ylim(0, max(src$cases))
  res <- ggplot(src) + aes(x=Date, y=cases) + ylim(0, max(src$cases)) + xlim(min(src$Date), Sys.Date()) + ylab("confirmed cases") + theme_bw() + geom_bar(stat="identity", fill="darkgrey") +
    annotate("rect", xmin = start_date, xmax=end_date, ymin=0, ymax=max(src$cases), alpha=0.1, fill="green") +
    annotate("text", x = start_date + as.numeric(end_date - start_date)/2, y=max(src$cases)-2, label="Fitted Interval") +
    geom_vline(x = as.numeric(forecast_date), color="red", size=1) + annotate("text", x=forecast_date-1, y=max(src$cases)/2, label="Forecast Start", angle=90)
  relrows <- (start_date <= src$Date) & (src$Date <= end_date) 
  res <- res + geom_line(aes(y=fit)) + annotate("line", y=src$fit[relrows], x=src$Date[relrows], color="green", alpha=0.5, size=4)
  res
}