
# load libraries ---------------------------------------------------------------
library(tidyverse)


# load data --------------------------------------------------------------------
fib <- read.csv("data/fib.csv", header = TRUE)


# divvy data -------------------------------------------------------------------
fib_serum <- fib %>% filter(variable == "fib_serum")
fib_plasma <- fib %>% filter(variable == "fib_plasma")
fga <- fib %>% filter(variable == "fga")
fgb <- fib %>% filter(variable == "fgb")
fgg <- fib %>% filter(variable == "fgg")


# fit models -------------------------------------------------------------------
# mod_fib_serum ----------------------------------------------------------------
mod_fib_serum <- lm(
  log(value) ~ 
    treatment,
  fib_serum
)
summary(mod_fib_serum)
plot(resid(mod_fib_serum))


# mod_fib_plasma ---------------------------------------------------------------
mod_fib_plasma <- lm(
  log(value) ~ 
    treatment,
  fib_plasma
)
summary(mod_fib_plasma)
plot(resid(mod_fib_plasma))


# mod_fga ----------------------------------------------------------------------
mod_fga <- lm(
  log(value) ~ 
    treatment,
  fga
)
summary(mod_fga)
plot(resid(mod_fga))


# mod_fgb ----------------------------------------------------------------------
mod_fgb <- lm(
  log(value) ~ 
    treatment,
  fgb
)
summary(mod_fgb)
plot(resid(mod_fgb))


# mod_fgg ----------------------------------------------------------------------
mod_fgg <- lm(
  log(value) ~ 
    treatment,
  fgg
)
summary(mod_fgg)
plot(resid(mod_fgg))
