## analysis functions
llgenerator <- function(ratefun, logprobfun) return(
  function(params, cases, times, param.xform = function(x) x) {
    ps <- param.xform(params)
    return(sum(logprobfun(ps, ratefun(ps, times), cases)))
  }
)
exp_decay <- function(params, times) params["rate_0"]*exp(-params["decay_rate"]*times)
pois_cases <- function(params, rates, cases) dpois(cases, lambda=rates, log=TRUE)
exp_pois_ll <- llgenerator(exp_decay, pois_cases)
mod_exp_decay <- function(params, times) params["rate_0"]*exp(- (params["decay_rate"]*times)^params["shape_parameter"])
mod_exp_pois_ll <- llgenerator(mod_exp_decay, pois_cases)
flat_model <- function(params, times) {
  rep(params["rate_0"],length(times))
}
flat_pois_ll <- llgenerator(flat_model, pois_cases)

expcurve <- function(optimresults, date_zero, params.xform = function(x) x) with(optimresults, {
  ps <- params.xform(par)
  #print(class(date_zero))
  return(function(d) {
    t <- d - date_zero
    ps["rate_0"]*exp(-ps["decay_rate"]*t)
  })
})