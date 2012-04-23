library(colorout)
library(mkin)
# {{{ Source new versions of mkin functions
source("mkin/R/ilr.R")
source("mkin/R/mkinpredict.R")
source("mkin/R/transform_odeparms.R")
source("mkin/R/mkinmod.R")
source("mkin/R/mkinfit.R")
source("mkin/R/mkinplot.R")
# }}}
# {{{ Delete new versions of mkin functions
rm(mkinmod, mkinfit, summary.mkinfit, print.summary.mkinfit, mkinplot)
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
model <- mkinmod(
    parent = list(type = "SFO", to = c("A1", "B1", "C1"), sink = FALSE),
    A1 = list(type = "SFO", to = "A2"),
    B1 = list(type = "SFO"),
    C1 = list(type = "SFO"),
    A2 = list(type = "SFO"), use_of_ff = "max")
model

fit <- mkinfit(model, data, plot=TRUE)
fit <- mkinfit(model, data, eigen=TRUE, plot=TRUE)
summary(fit, data=FALSE)#}}}
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

debug(mkinmod)

test <- mkinmod(parent = list(type = "SFO", 

SFO <- mkinmod(parent = list(type = "SFO"))
DFOP <- mkinmod(parent = list(type = "DFOP"))
SFORB <- mkinmod(parent = list(type = "SFORB"))
# Test different types of model specification and solution
# SFO_SFO {{{
SFO_SFO.1 <- mkinmod(parent = list(type = "SFO", to = "m1"),
		   m1 = list(type = "SFO"), use_of_ff = "min")
SFO_SFO.2 <- mkinmod(parent = list(type = "SFO", to = "m1"),
		   m1 = list(type = "SFO"), use_of_ff = "max")
ot = seq(0, 100, by = 10)
print(mkinpredict(SFO_SFO.1, c(k_parent_m1 = 0.1, k_parent_sink = 0.1, k_m1_sink = 0.1), 
	    c(parent = 100, m1 = 0), ot, solution_type = "deSolve"))
print(mkinpredict(SFO_SFO.1, c(k_parent_m1 = 0.1, k_parent_sink = 0.1, k_m1_sink = 0.1), 
	    c(parent = 100, m1 = 0), ot, solution_type = "eigen"))
print(mkinpredict(SFO_SFO.2, c(k_parent = 0.2, f_parent_to_m1 = 0.5, k_m1 = 0.1), 
	    c(parent = 100, m1 = 0), ot, solution_type = "deSolve"))
print(mkinpredict(SFO_SFO.2, c(k_parent = 0.2, f_parent_to_m1 = 0.5, k_m1 = 0.1), 
	    c(parent = 100, m1 = 0), ot, solution_type = "eigen"))
# }}}
# SFO_SFO2_SFO {{{
SFO_SFO2_SFO.1 <- mkinmod(parent = list(type = "SFO", to = c("m1", "m2")),
		   m1 = list(type = "SFO", to = "m3"),
		   m2 = list(type = "SFO"),
		   m3 = list(type = "SFO"), 
		   use_of_ff = "min")
SFO_SFO2_SFO.2 <- mkinmod(parent = list(type = "SFO", to = c("m1", "m2")),
		   m1 = list(type = "SFO", to = "m3"),
		   m2 = list(type = "SFO"),
		   m3 = list(type = "SFO"), 
		   use_of_ff = "max")
SFO_SFO2_SFO.2$diffs
ot = seq(0, 100, by = 1)
print(mkinpredict(SFO, c(k_parent_sink = 0.3),
	    c(parent = 100), ot, solution_type = "deSolve"))
print(mkinpredict(SFO, c(k_parent = 0.3),
	    c(parent = 100), ot, solution_type = "analytical"))
print(tail(mkinpredict(SFO_SFO2_SFO.1, c(k_parent_m1 = 0.1, k_parent_m2 = 0.1, 
	    k_parent_sink = 0.1, k_m1_m3 = 0.1, k_m1_sink = 0.1, 
	    k_m2_sink = 0.1, k_m3_sink = 0.1),
	    c(parent = 100, m1 = 0, m2 = 0, m3 = 0), ot, solution_type = "deSolve")))
print(tail(mkinpredict(SFO_SFO2_SFO.1, c(k_parent_m1 = 0.1, k_parent_m2 = 0.1, 
	    k_parent_sink = 0.1, k_m1_m3 = 0.1, k_m1_sink = 0.1, 
	    k_m2_sink = 0.1, k_m3_sink = 0.1),
	    c(parent = 100, m1 = 0, m2 = 0, m3 = 0), ot, solution_type = "eigen")))
print(tail(mkinpredict(SFO_SFO2_SFO.2, 
            c(k_parent = 0.3, f_parent_to_m1 = 1/3, f_parent_to_m2 = 1/3, 
              k_m1 = 0.2, f_m1_to_m3 = 0.5,
	      k_m2 = 0.1,
	      k_m3 = 0.1),
	    c(parent = 100, m1 = 0, m2 = 0, m3 = 0), ot, solution_type = "deSolve")))
print(tail(mkinpredict(SFO_SFO2_SFO.2, 
            c(k_parent = 0.3, f_parent_to_m1 = 1/3, f_parent_to_m2 = 1/3, 
              k_m1 = 0.2, f_m1_to_m3 = 0.5,
	      k_m2 = 0.1,
	      k_m3 = 0.1),
	    c(parent = 100, m1 = 0, m2 = 0, m3 = 0), ot, solution_type = "eigen")))
# }}}
# FOMC_SFO {{{
FOMC_SFO.1 <- mkinmod(parent = list(type = "FOMC", to = "m1"),
	           m1 = list(type = "SFO"))
FOMC_SFO.2 <- mkinmod(parent = list(type = "FOMC", to = "m1"),
	           m1 = list(type = "SFO"), use_of_ff = "max")
print(tail(mkinpredict(FOMC_SFO.1,
  c(alpha = 1, beta = 10, f_parent_to_m1 = 0.5, k_m1_sink = 0.1),
  c(parent = 100, m1 = 0), ot, solution_type = "deSolve")))
print(tail(mkinpredict(FOMC_SFO.1,
  c(alpha = 1, beta = 10, f_parent_to_m1 = 0.5, k_m1_sink = 0.1),
  c(parent = 100, m1 = 0), ot, solution_type = "deSolve")))
# }}}
# SFORB_SFO {{{
SFORB_SFO.1 <- mkinmod(parent = list(type = "SFORB", to = "m1"),
	           m1 = list(type = "SFO"))
SFORB_SFO.2 <- mkinmod(parent = list(type = "SFORB", to = "m1"),
	           m1 = list(type = "SFO"), use_of_ff = "max")
# }}}
system.time(fit.SFORB <- mkinfit(SFORB_SFO, d2, 
  parms.ini = f.SFORB$parms.all,
  plot=TRUE))
summary(fit.SFORB, data=FALSE)

debug(mkinfit)
debug(cost)
undebug(mkinfit)
d2 <- rbind(FOCUS_2006_C, 
    data.frame(
       name = "m1", 
       time = FOCUS_2006_C$time,
       value = c(0, 20, 40, 45, 48, 49, 48, 46, 45)))
# Compare eigenvalue and deSolve based fitting for SFO_SFO {{{
# for both model specification variants
system.time(fit.SFO.1.eigen <- mkinfit(SFO_SFO.1, d2, plot=TRUE))
system.time(fit.SFO.1.lsoda <- mkinfit(SFO_SFO.1, d2, solution_type = "deSolve", plot=TRUE))
summary(fit.SFO.1.eigen, data=FALSE)
summary(fit.SFO.1.lsoda, data=FALSE)
system.time(fit.SFO.2.eigen <- mkinfit(SFO_SFO.2, d2, plot=TRUE))
system.time(fit.SFO.2.lsoda <- mkinfit(SFO_SFO.2, d2, solution_type = "deSolve", plot=TRUE))
summary(fit.SFO.2.eigen, data=FALSE)
summary(fit.SFO.2.lsoda, data=FALSE)
# }}}
system.time(fit.FOMC <- mkinfit(FOMC_SFO.1, d2, plot=TRUE))
f.DFOP <- mkinfit(DFOP, d2)
system.time(fit.DFOP <- mkinfit(DFOP_SFO, d2, 
  parms.ini = f.DFOP$parms.all,
  plot=TRUE))
f.SFORB.1.eigen <- mkinfit(SFORB_SFO.1, d2, plot=TRUE)
f.SFORB.1.lsoda <- mkinfit(SFORB_SFO.1, d2, solution_type = "deSolve", plot=TRUE)
summary(f.SFORB.1.eigen, data = FALSE)
summary(f.SFORB.1.lsoda, data = FALSE)
system.time(fit.SFORB <- mkinfit(SFORB_SFO, d2, 
  parms.ini = f.SFORB$parms.all,
  plot=TRUE))
summary(fit.SFORB, data=FALSE)
system.time(fit.SFORB <- mkinfit(SFORB_SFO, d2, 
  parms.ini = f.SFORB$parms.all,
  eigen=TRUE,
  plot=TRUE))
summary(fit.SFORB, data=FALSE)

system.time(fit.SFO <- mkinfit(SFO_SFO, FOCUS_2006_D, plot=TRUE))
system.time(fit.SFORB <- mkinfit(SFORB_SFO, FOCUS_2006_D, plot=TRUE))
summary(fit.SFO, data=FALSE)

# vim: set foldmethod=marker:
