install.packages("mkin")
library(mkin)
# {{{ Source new versions of mkin functions
source("mkin/R/ilr.R")
source("mkin/R/mkinpredict.R")
source("mkin/R/transform_odeparms.R")
source("mkin/R/mkinmod.R")
source("mkin/R/mkinfit.R")
# }}}
# {{{ Delete new versions of mkin functions
rm(mkinmod, mkinfit, summary.mkinfit, print.summary.mkinfit)
rm(ilr, invilr, transform_odeparms, backtransform_odeparms)
# }}}
detach(package:mkin)
detach(package:kinfit)

data <- mkin_wide_to_long(schaefer07_complex_case, time = "time")#{{{
model <- mkinmod(
    parent = list(type = "SFO", to = c("A1", "B1", "C1"), sink = FALSE),
    A1 = list(type = "SFO", to = "A2"),
    B1 = list(type = "SFO"),
    C1 = list(type = "SFO"),
    A2 = list(type = "SFO"))

fit <- mkinfit(model, data, plot=TRUE)
summary(fit)#}}}
model2 <- mkinmod(#{{{
    parent = list(type = "FOMC", to = c("A1", "B1", "C1"), sink = FALSE),
    A1 = list(type = "SFO", to = "A2"),
    B1 = list(type = "SFO"),
    C1 = list(type = "SFO"),
    A2 = list(type = "SFO"))
fit2 <- mkinfit(model2, data, plot=TRUE, parms.ini = c(f_A1_to_A2 = 0.3))
summary(fit2)#}}}
model3 <- mkinmod(#{{{
    parent = list(type = "SFORB", to = c("A1", "B1", "C1"), sink = FALSE),
    A1 = list(type = "SFO", to = "A2"),
    B1 = list(type = "SFO"),
    C1 = list(type = "SFO"),
    A2 = list(type = "SFO"))
fit3 <- mkinfit(model3, data, plot=FALSE)
summary(fit3)#}}}

SFO_SFO <- mkinmod(parent = list(type = "SFO", to = "m1"),
	           m1 = list(type = "SFO"))
FOMC_SFO <- mkinmod(parent = list(type = "FOMC", to = "m1"),
	           m1 = list(type = "SFO"))
SFORB_SFO <- mkinmod(parent = list(type = "SFORB", to = "m1"),
	           m1 = list(type = "SFO"))
DFOP_SFO <- mkinmod(parent = list(type = "DFOP", to = "m1"),
	           m1 = list(type = "SFO"))

debug(transform_odeparms)
undebug(transform_odeparms)
d2 <- rbind(FOCUS_2006_C, 
    data.frame(
       name = "m1", 
       time = FOCUS_2006_C$time,
       value = c(0, 20, 40, 45, 48, 49, 48, 46, 45)))
system.time(fit.SFORB <- mkinfit(SFORB_SFO, d2, 
 parms.ini = c(
   f_parent_to_m1 = 0.67, k_m1 = 0.002,
   k_parent_free_bound = 0.05, k_parent_bound_free = 0.03),
 plot=TRUE))
summary(fit.SFORB, data=FALSE)
system.time(fit.SFORB <- mkinfit(SFORB_SFO, d2, plot=TRUE))
system.time(fit.FOMC <- mkinfit(FOMC_SFO, d2, plot=TRUE))
system.time(fit.DFOP <- mkinfit(DFOP_SFO, d2, 
  parms.ini = c(k1 = 0.5, k2 = 0.02, g = 0.8, 
		k_m1 = 0.002, f_parent_to_m1 = 0.66),
  plot=TRUE))
summary(fit.DFOP, data=FALSE)

system.time(fit.SFO <- mkinfit(SFO_SFO, FOCUS_2006_D, plot=TRUE))
system.time(fit.SFORB <- mkinfit(SFORB_SFO, FOCUS_2006_D, plot=TRUE))
summary(fit.SFO, data=FALSE)

# vim: set foldmethod=marker:
