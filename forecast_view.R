## forecast plot
require(data.table); require(ggplot2); require(reshape2)
forecast <- function(forecast_date = Sys.Date(), clear_days = 42, ref) with(ref, {
  forecast_ts <- as.numeric(seq(from=forecast_date, by=1, length.out = clear_days) - t0)
  prod(dpois(0, par["rate_0"]*exp(-par["decay_rate"]*forecast_ts)))
})

p_starting <- function(ps, days = 42) {
  c(sapply(0:(length(ps)-42), function(i) prod(ps[1:42+i])), rep(NA, 41))
}

p_ending <- function(ps, days = 42, prior_zeros = 0) {
  c(rep(NA, 41-prior_zeros), sapply(0:(length(ps)-42), function(i) prod(ps[1:42+i])))
}

forecast_plot <- function(forecast_date, total_days, ref, clear_days = 42) with(ref, {
  src <- within(data.table(Date = seq(from=forecast_date, by=1, length.out=total_days+clear_days)), {
    t <- as.numeric(Date - t0)
    p_0_est <- dpois(0, par["rate_0"]*exp(-b["est"]*t))
    p_0_low <- dpois(0, par["rate_0"]*exp(-b["hi"]*t))
    p_0_hi <- dpois(0, par["rate_0"]*exp(-b["low"]*t))
    p_start_est <- p_starting(p_0_est, clear_days)
    p_start_low <- p_starting(p_0_low, clear_days)
    p_start_hi <- p_starting(p_0_hi, clear_days)
    ## n.b. - end p's same as start ps, just need to shift dates
    p_end_est <- p_ending(p_0_est, clear_days)
    p_end_low <- p_ending(p_0_low, clear_days)
    p_end_hi <- p_ending(p_0_hi, clear_days)
  })
  
  lvls <- c("est","low","hi")
  plot.src <- melt(src, id.vars = "Date",
                   measure.vars = c(paste0("p_0_",lvls), paste0("p_start_",lvls), paste0("p_end_",lvls)),
                   na.rm = TRUE)
  plot.src$bound <- as.factor(sub("^.*_", "", as.character(plot.src$variable)))
  plot.src$measure <- as.factor(sub("_[^_]*$", "", as.character(plot.src$variable)))
  ggplot(plot.src) + theme_bw() + aes(x=Date, y=value, color=measure, linetype=bound) + geom_line() + ylim(0,1) + xlim(forecast_date, forecast_date+total_days-1)
})