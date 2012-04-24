# This file contains fold markers for the vim editor, but can be edited with
# any other editor
library(colorout) # Not available on Windows
library(mkin)
# {{{ Source new versions of mkin functions
source("mkin/R/ilr.R")
source("mkin/R/mkinpredict.R")
source("mkin/R/transform_odeparms.R")
source("mkin/R/mkinmod.R")
source("mkin/R/mkinfit.R")
source("mkin/R/mkinplot.R")
# }}}

# Dataset d2 {{{
d2 <- rbind(FOCUS_2006_C, 
    data.frame(
       name = "m1", 
       time = FOCUS_2006_C$time,
       value = c(0, 20, 40, 45, 48, 49, 48, 46, 45)))
# }}}

# Check deSolve solution versus analytical for parent only case {{{
ot = seq(0, 100, by = 1)
print(mkinpredict(SFO, c(k_parent_sink = 0.1),
	    c(parent = 100), ot, solution_type = "deSolve")[101,])
print(mkinpredict(SFO, c(k_parent = 0.1),
	    c(parent = 100), ot, solution_type = "analytical")[101,])
# }}}

# Some code useful for debugging {{{
as.list(body(mkinfit))
trace("mkinfit", quote(browser(skipCalls=4)), at = 55)
untrace("mkinfit")
# }}}

# Test different types of model specification {{{
# SFO_SFO {{{
SFO_SFO.1 <- mkinmod(parent = list(type = "SFO", to = "m1"),
		   m1 = list(type = "SFO"), use_of_ff = "min")
SFO_SFO.2 <- mkinmod(parent = list(type = "SFO", to = "m1"),
		   m1 = list(type = "SFO"), use_of_ff = "max")
ot = seq(0, 100, by = 1)
print(mkinpredict(SFO_SFO.1, c(k_parent_m1 = 0.1, k_parent_sink = 0.1, k_m1_sink = 0.1), 
	    c(parent = 100, m1 = 0), ot, solution_type = "deSolve")[101,])
print(mkinpredict(SFO_SFO.1, c(k_parent_m1 = 0.1, k_parent_sink = 0.1, k_m1_sink = 0.1), 
	    c(parent = 100, m1 = 0), ot, solution_type = "eigen")[101,])
print(mkinpredict(SFO_SFO.2, c(k_parent = 0.2, f_parent_to_m1 = 0.5, k_m1 = 0.1), 
	    c(parent = 100, m1 = 0), ot, solution_type = "deSolve")[101,])
print(mkinpredict(SFO_SFO.2, c(k_parent = 0.2, f_parent_to_m1 = 0.5, k_m1 = 0.1), 
	    c(parent = 100, m1 = 0), ot, solution_type = "eigen")[101,])
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

ot = seq(0, 100, by = 1)
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
# FOMC_SFO (actually two times the same model {{{
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
# }}}

# Compare eigenvalue and deSolve based fitting {{{
testdata = d2
# SFO_SFO {{{
# for both model specification variants
system.time(fit.SFO.1.eigen <- mkinfit(SFO_SFO.1, testdata, plot=TRUE))
system.time(fit.SFO.1.lsoda <- mkinfit(SFO_SFO.1, testdata, solution_type = "deSolve", plot=TRUE))
system.time(fit.SFO.2.eigen <- mkinfit(SFO_SFO.2, testdata, plot=TRUE))
f.SFO <- mkinfit(SFO.2, testdata)
system.time(fit.SFO.2.eigen <- mkinfit(SFO_SFO.2, parms.ini = f.SFO$odeparms.final, 
  testdata, plot=TRUE))
system.time(fit.SFO.2.lsoda <- mkinfit(SFO_SFO.2, testdata, solution_type = "deSolve", plot=TRUE))
summary(fit.SFO.1.eigen, data=FALSE)
summary(fit.SFO.1.lsoda, data=FALSE)
summary(fit.SFO.2.eigen, data=FALSE)
summary(fit.SFO.2.lsoda, data=FALSE)
# }}}
# SFORB_SFO {{{
testdata = FOCUS_2006_D
f.SFORB.1.eigen <- mkinfit(SFORB_SFO.1, testdata, plot=TRUE)
f.SFORB.1.lsoda <- mkinfit(SFORB_SFO.1, testdata, solution_type = "deSolve", plot=TRUE)
# SFORB_SFO.2 is not there because combining maximum use of ff with SFORB is not supported
summary(f.SFORB.1.eigen, data = FALSE)
summary(f.SFORB.1.lsoda, data = FALSE)
# }}}
# }}}

# Check coupled FOMC models {{{
fit.FOMC <- mkinfit(FOMC, d2)
system.time(fit.FOMC.1 <- mkinfit(FOMC_SFO.1, d2, plot=TRUE))
system.time(fit.FOMC.1 <- mkinfit(FOMC_SFO.1, d2, parms.ini = fit.FOMC$odeparms.final, plot=TRUE))
system.time(fit.FOMC.2 <- mkinfit(FOMC_SFO.2, d2, parms.ini = fit.FOMC$odeparms.final, plot=TRUE))
summary(fit.FOMC, data=FALSE)
summary(fit.FOMC.1, data=FALSE)
summary(fit.FOMC.2, data=FALSE)
fit.FOMC$distimes
fit.FOMC.1$distimes
fit.FOMC.2$distimes
# }}}

# Check DFOP_SFO {{{
DFOP_SFO <- mkinmod(parent = list(type = "DFOP", to = "m1"),
	           m1 = list(type = "SFO"))
f.DFOP <- mkinfit(DFOP, d2)
summary(f.DFOP)
system.time(fit.DFOP <- mkinfit(DFOP_SFO, d2, 
  plot=TRUE))
system.time(fit.DFOP <- mkinfit(DFOP_SFO, FOCUS_2006_D, 
  plot=TRUE))
summary(fit.DFOP, data=FALSE)
# }}}

# {{{ KinGUI test data from 2007
data <- mkin_wide_to_long(schaefer07_complex_case, time = "time")
debug(mkinmod)
model.1 <- mkinmod(
    parent = list(type = "SFO", to = c("A1", "B1", "C1"), sink = FALSE),
    A1 = list(type = "SFO", to = "A2"),
    B1 = list(type = "SFO"),
    C1 = list(type = "SFO"),
    A2 = list(type = "SFO"))
model.1.sink <- mkinmod(
    parent = list(type = "SFO", to = c("A1", "B1", "C1"), sink = TRUE),
    A1 = list(type = "SFO", to = "A2"),
    B1 = list(type = "SFO"),
    C1 = list(type = "SFO"),
    A2 = list(type = "SFO"))
model.2 <- mkinmod(
    parent = list(type = "SFO", to = c("A1", "B1", "C1"), sink = FALSE),
    A1 = list(type = "SFO", to = "A2"),
    B1 = list(type = "SFO"),
    C1 = list(type = "SFO"),
    A2 = list(type = "SFO"), use_of_ff = "max")

# Compare eigenvalue and deSolve based solutions {{{
undebug(mkinpredict)
print(mkinpredict(model.1, 
  fit.1.eigen$odeparms.final, 
  c(parent = 100, A1 = 0, B1 = 0, C1 = 0, A2 = 0),
  outtimes = 0:100, solution_type = "eigen")[101,])
print(mkinpredict(model.1, 
  fit.1.eigen$odeparms.final, 
  c(parent = 100, A1 = 0, B1 = 0, C1 = 0, A2 = 0),
  outtimes = 0:100, solution_type = "deSolve")[101,])
# }}}

fit.1.eigen <- mkinfit(model.1, data, plot=TRUE)
fit.1.lsoda <- mkinfit(model.1, data, solution_type = "deSolve", plot=TRUE)
fit.1.lsoda <- mkinfit(model.1, data, solution_type = "deSolve", plot=TRUE)
fit.1.lsoda.1000 <- mkinfit(model.1, data, solution_type = "deSolve", n.outtimes = 1000, plot=TRUE)
fit.1.eigen.sink <- mkinfit(model.1.sink, data, plot=TRUE)
#fit.2.eigen <- mkinfit(model.2, data, plot=TRUE) # Does not work, singular matrix...
fit.2.lsoda <- mkinfit(model.2, data, solution_type = "deSolve", plot=TRUE)
fit.2.lsoda.1000 <- mkinfit(model.2, data, solution_type = "deSolve", n.outtimes = 1000, plot=TRUE)
summary(fit.1.eigen, data=FALSE) # Fast and fits very nicely with Schaefer 2007
summary(fit.1.lsoda, data=FALSE) # Slower, not precise, presumably because the model does not fit the data well (no sink term...), but maybe also a different ode solver would do...
summary(fit.1.lsoda.1000, data=FALSE) # Gives solution much closer to eigen solution, but slow!
summary(fit.1.eigen.sink, data=FALSE)
#summary(fit.2.eigen, data=FALSE) # See above, did not work
summary(fit.2.lsoda, data=FALSE)
summary(fit.2.lsoda.1000, data=FALSE) # Not a lot different from n.outtimes = 100

# Compare the models with sink - the solution method does not make a significant difference
fit.1.eigen.sink$distimes
fit.2.lsoda$distimes
# }}}

# vim: set foldmethod=marker ts=2 sw=2 expandtab:
