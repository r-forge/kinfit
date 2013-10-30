# Some test code used in the development of mkin

# This file contains fold markers for the vim editor, but can be edited with
# any other editor

library(mkin)
# {{{1 Source new versions of mkin functions
source("mkin/R/ilr.R")
source("mkin/R/mkinpredict.R")
source("mkin/R/mkinerrmin.R")
source("mkin/R/transform_odeparms.R")
source("mkin/R/mkinmod.R")
source("mkin/R/mkinfit.R")
source("mkin/R/mkin_wide_to_long.R")
source("mkin/R/endpoints.R")
source("mkin/R/mkinplot.R")
# Compare eigenvalue and deSolve based fitting {{{1
testdata = FOCUS_2006_D
# SFO_SFO {{{2
SFO_SFO.1 <- mkinmod(parent = list(type = "SFO", to = "m1"),
       m1 = list(type = "SFO"), use_of_ff = "min")
SFO_SFO.2 <- mkinmod(parent = list(type = "SFO", to = "m1"),
       m1 = list(type = "SFO"), use_of_ff = "max")

# for both model specification variants
system.time(fit.SFO.1.eigen <- mkinfit(SFO_SFO.1, testdata, plot=TRUE))
system.time(fit.SFO.1.lsoda <- mkinfit(SFO_SFO.1, testdata, solution_type = "deSolve", plot=TRUE))
system.time(fit.SFO.2.eigen <- mkinfit(SFO_SFO.2, testdata, plot=TRUE))
SFO.2 <- mkinmod(parent = list(type = "SFO"))
f.SFO <- mkinfit(SFO.2, testdata, plot=TRUE)
#system.time(fit.SFO.2.eigen <- mkinfit(SFO_SFO.2, parms.ini = f.SFO$odeparms.final, 
#  testdata, plot=TRUE))
system.time(fit.SFO.2.eigen <- mkinfit(SFO_SFO.2, parms.ini = c(k_m1 = 0.001),
  testdata, plot=TRUE))
system.time(fit.SFO.2.lsoda <- mkinfit(SFO_SFO.2, testdata, solution_type = "deSolve", plot=TRUE))
summary(fit.SFO.1.eigen, data=FALSE)
summary(fit.SFO.1.lsoda, data=FALSE)
summary(fit.SFO.2.eigen, data=FALSE)
summary(fit.SFO.2.lsoda, data=FALSE)
# }}}
# SFORB_SFO {{{2
f.SFORB.1.eigen <- mkinfit(SFORB_SFO.1, testdata, plot=TRUE)
f.SFORB.1.lsoda <- mkinfit(SFORB_SFO.1, testdata, solution_type = "deSolve", plot=TRUE)
# SFORB_SFO.2 is not there because combining maximum use of ff with SFORB is not supported
summary(f.SFORB.1.eigen, data = FALSE)
summary(f.SFORB.1.lsoda, data = FALSE)
# }}}
# {{{1 KinGUI test data from 2007
data <- mkin_wide_to_long(schaefer07_complex_case, time = "time")
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
    parent = list(type = "SFO", to = c("A1", "B1", "C1")),
    A1 = list(type = "SFO", to = "A2"),
    B1 = list(type = "SFO"),
    C1 = list(type = "SFO"),
    A2 = list(type = "SFO"), use_of_ff = "max")

# Compare eigenvalue and deSolve based solutions {{{2
print(mkinpredict(model.1, 
  fit.1.eigen$odeparms.final, 
  c(parent = 100, A1 = 0, B1 = 0, C1 = 0, A2 = 0),
  outtimes = 0:100, solution_type = "eigen")[101,])
print(mkinpredict(model.1, 
  fit.1.eigen$odeparms.final, 
  c(parent = 100, A1 = 0, B1 = 0, C1 = 0, A2 = 0),
  outtimes = 0:100, solution_type = "deSolve")[101,])

fit.1.eigen <- mkinfit(model.1, data, plot=TRUE)
# 40 seconds on the workstation, 134 solutions
system.time(fit.1.lsoda <- mkinfit(model.1, data, solution_type = "deSolve",
                                   plot=TRUE)) 
# The following is much closer to the eigenvalue based solution, but very slow
fit.1.lsoda.1000 <- mkinfit(model.1, data, solution_type = "deSolve",
                            n.outtimes = 1000, plot=TRUE)
# Lowering atol and rtol helps as well with less impact on the computing time
# 114 seconds on the workstation, 332 solutions:
print(system.time(fit.1.lsoda.atol8 <- mkinfit(model.1, data, 
                                               solution_type = "deSolve",
                                               atol=1e-8, plot=TRUE)))
# 73 seconds on the workstation, 185 solutions:
print(system.time(fit.1.lsoda.artol10 <- mkinfit(model.1, data, 
                                           solution_type = "deSolve",
                                           atol=1e-10, rtol=1e-10, plot=TRUE)))
# 57 seconds on the workstation, 186 solutions:
print(system.time(fit.1.lsoda.rtol10 <- mkinfit(model.1, data, 
                                           solution_type = "deSolve",
                                           rtol=1e-10, plot=TRUE)))
# The following fails after 34 steps, obviously 10 output times are not enough
print(system.time(fit.1.lsoda.10.artol10 <- mkinfit(model.1, data, 
                                  solution_type = "deSolve", atol=1e-10,
                                  rtol=1e-10, n.outtimes = 10, plot=TRUE)))

summary(fit.1.eigen, data=FALSE) # Fast and fits very nicely with Schaefer 2007
summary(fit.1.lsoda, data=FALSE) # Slower, not as precise, presumably because
# this model without sink term does not fit the data well.
#summary(fit.1.lsoda.1000, data=FALSE) # Much closer to eigen solution, but slow!
summary(fit.1.lsoda)$errmin
summary(fit.1.lsoda.1000)$errmin
endpoints(fit.1.eigen)
endpoints(fit.1.lsoda)
endpoints(fit.1.lsoda.atol8)
endpoints(fit.1.lsoda.artol10)
endpoints(fit.1.lsoda.rtol10)
endpoints(fit.1.lsoda.1000)
summary(fit.1.lsoda.atol8)$errmin
summary(fit.1.lsoda.artol10)$errmin
summary(fit.1.lsoda.rtol10)$errmin

fit.1.eigen.sink <- mkinfit(model.1.sink, data, plot=TRUE)
#fit.2.eigen <- mkinfit(model.2, data, plot=TRUE) # Does not work, singular matrix...
fit.2.lsoda <- mkinfit(model.2, data, solution_type = "deSolve", plot=TRUE)
# Again, the following is very slow
#fit.2.lsoda.1000 <- mkinfit(model.2, data, solution_type = "deSolve", n.outtimes = 1000, plot=TRUE)
# This works with very good starting parameters...
fit.2.eigen <- mkinfit(model.2, data, parms.ini = fit.2.lsoda$bparms.ode, plot=TRUE) 
summary(fit.2.eigen, data=FALSE)


summary(fit.1.eigen.sink, data=FALSE)
#summary(fit.2.eigen, data=FALSE) # See above, did not work
summary(fit.2.lsoda, data=FALSE)
summary(fit.2.lsoda.1000, data=FALSE) # Not a lot different from n.outtimes = 100

# Compare the models with sink - the solution method does not make a significant difference
endpoints(fit.1.eigen.sink)
endpoints(fit.2.lsoda)
# Different optimisation methods {{{1
fit.Marq <- mkinfit(SFO_SFO, FOCUS_2006_D, plot = TRUE)
fit.Port <- mkinfit(SFO_SFO, FOCUS_2006_D, method.modFit = "Port", plot = TRUE)
fit.Pseudo <- mkinfit(SFO_SFO, FOCUS_2006_D, method.modFit = "Pseudo", lower = c(10, rep(-10, 3)), 
                                                                       upper = c(200, rep(5, 3)), plot = TRUE)
# Stepwise approach to schaefer data with ffs and eigenvalue solution {{{2
m.SFO <- mkinmod(parent = list(type = "SFO", to = c("A1", "C1")),
  A1 = list(type = "SFO"),
  C1 = list(type = "SFO"), use_of_ff = "max")
f.SFO <- mkinfit(m.SFO, data, plot=TRUE)
m.final <- mkinmod(parent = list(type = "SFO", to = c("A1", "B1", "C1")),
  A1 = list(type = "SFO", to = "A2"),
  B1 = list(type = "SFO"),
  C1 = list(type = "SFO"),
  A2 = list(type = "SFO"), use_of_ff = "max")
m.1 <- mkinfit(m.final, data, parms.ini = f.SFO$bparms.ode, plot=TRUE)
m.2 <- mkinfit(m.final, data, parms.ini = f.SFO$bparms.ode, plot=TRUE, solution_type = "deSolve")
m.3 <- mkinfit(m.final, data, plot=TRUE, solution_type = "deSolve")
summary(m.1)
summary(m.2)
summary(m.3)
# The covarianc matrix is only returned in the last of the three cases :(

# {{{1 Water sediment models
ws <- mkinmod(water = list(type = "SFO", to = "sediment", sink = TRUE),
  sediment = list(type = "SFO"))
ws_back <- mkinmod(water = list(type = "SFO", to = "sediment", sink = TRUE),
  sediment = list(type = "SFO", to = "water"))


# Show how to use nesting of models for getting suitable starting parameters {{{
# Add synthetic data to two metabolites to FOCUS 2006 C  
d <- FOCUS_2006_C
d2 <- FOCUS_2006_C
d2$name <- "m1"
d2$value <- cumsum(c(0, 9.5, 12, 6.8, 2, 1.2, 0.2, -0.1, -0.5))
d3 <- FOCUS_2006_C
d3$name <- "m2"
d3$value <- cumsum(c(0, 6.3, 7.2, 2.0, 1.0, -2, -3.5, -4, -4.5))
observed <- rbind(d, d2, d3)

FOMC <- mkinmod(parent = list(type="FOMC"))
FOMC_SFO <- mkinmod(parent = list(type="FOMC", to = "m1"), m1 = list(type="SFO"))
FOMC_SFO2 <- mkinmod(parent = list(type="FOMC", to = c("m1", "m2")),
  m1 = list(type = "SFO"), m2 = list(type="SFO"))

fit.FOMC = mkinfit(FOMC, observed)
fit <- mkinfit(FOMC_SFO, observed, 
               parms.ini = fit.FOMC$bparms.ode, plot=TRUE)
fit <- mkinfit(FOMC_SFO2, observed, 
               parms.ini = fit.FOMC$bparms.ode, plot=TRUE)
summary(fit)

# Weighted fits {{{1
SFO_SFO.ff <- mkinmod(parent = list(type = "SFO", to = "m1"),
                      m1 = list(type = "SFO"), use_of_ff = "max")
f.noweight <- mkinfit(SFO_SFO.ff, FOCUS_2006_D)
summary(f.noweight)
f.irls <- mkinfit(SFO_SFO.ff, FOCUS_2006_D, reweight.method = "obs")
summary(f.irls)
f.w.mean <- mkinfit(SFO_SFO.ff, FOCUS_2006_D, weight = "mean")
summary(f.w.mean)
f.w.mean.irls <- mkinfit(SFO_SFO.ff, FOCUS_2006_D, weight = "mean",
                         reweight.method = "obs")
summary(f.w.mean.irls)

dw <- FOCUS_2006_D
errors <- c(parent = 2, m1 = 1)
dw$err.man <- errors[FOCUS_2006_D$name]
f.w.man <- mkinfit(SFO_SFO.ff, dw, err = "err.man")
summary(f.w.man)
f.w.man.irls <- mkinfit(SFO_SFO.ff, dw, err = "err.man",
                       reweight.method = "obs")
summary(f.w.man.irls)

# Test the new kinerrmin implementation {{{1
LOD = 0.5
FOCUS_2006_Z = data.frame(
  t = c(0, 0.04, 0.125, 0.29, 0.54, 1, 2, 3, 4, 7, 10, 14, 21, 
        42, 61, 96, 124),
  Z0 = c(100, 81.7, 70.4, 51.1, 41.2, 6.6, 4.6, 3.9, 4.6, 4.3, 6.8, 
         2.9, 3.5, 5.3, 4.4, 1.2, 0.7),
  Z1 = c(0, 18.3, 29.6, 46.3, 55.1, 65.7, 39.1, 36, 15.3, 5.6, 1.1, 
         1.6, 0.6, 0.5 * LOD, NA, NA, NA),
  Z2 = c(0, NA, 0.5 * LOD, 2.6, 3.8, 15.3, 37.2, 31.7, 35.6, 14.5, 
         0.8, 2.1, 1.9, 0.5 * LOD, NA, NA, NA),
  Z3 = c(0, NA, NA, NA, NA, 0.5 * LOD, 9.2, 13.1, 22.3, 28.4, 32.5, 
         25.2, 17.2, 4.8, 4.5, 2.8, 4.4))
FOCUS_2006_Z_mkin <- mkin_wide_to_long(FOCUS_2006_Z)
Z.FOCUS <- mkinmod(Z0 = list(type = "SFO", to = "Z1", sink = FALSE), 
                   Z1 = list(type = "SFO", to = "Z2", sink = FALSE), 
                   Z2 = list(type = "SFO", to = "Z3"),
                   Z3 = list(type = "SFO"))

m.Z.FOCUS <- mkinfit(Z.FOCUS, FOCUS_2006_Z_mkin, 
                     parms.ini = c(k_Z0_Z1 = 0.5, k_Z1_Z2 = 0.2, k_Z2_Z3 = 0.3), 
                     plot = TRUE)
#capture.output(summary(m.Z.FOCUS), file = "summary_m.Z.FOCUS_0.9.20.txt")
#capture.output(summary(m.Z.FOCUS), file = "summary_m.Z.FOCUS_0.9.22.txt")
capture.output(summary(m.Z.FOCUS), file = "summary_m.Z.FOCUS_0.9.22a.txt")
summary(m.Z.FOCUS, data = FALSE)


# Test the GUI experiments {{{1
library(gWidgetsWWW2)
load_app("mkin/inst/GUI/mkinGUI.R")

cur <- length(app$session_manager$sessions)

app$session_manager$sessions[[cur]]$e$f.df
str(app$session_manager$sessions[[cur]]$e$f)

load_app("gimage.R")

demo(gWidgetsWWW2)
# {{{1
# vim: set foldmethod=marker ts=2 sw=2 expandtab:
