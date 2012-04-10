library(mkin)
source("mkin/R/ilr.R")
source("mkin/R/mkinpredict.R")
source("mkin/R/transform_odeparms.R")
source("mkin/R/mkinmod.R")
source("mkin/R/mkinfit.R")

data <- mkin_wide_to_long(schaefer07_complex_case, time = "time")
model <- mkinmod(
    parent = list(type = "SFO", to = c("A1", "B1", "C1"), sink = FALSE),
    A1 = list(type = "SFO", to = "A2"),
    B1 = list(type = "SFO"),
    C1 = list(type = "SFO"),
    A2 = list(type = "SFO"))

debug(mkinfit)
undebug(mkinfit)
debug(transform_odeparms)
fit <- mkinfit(model, data, plot=TRUE)
summary(fit)

model2 <- mkinmod(
    parent = list(type = "FOMC", to = c("A1", "B1", "C1"), sink = FALSE),
    A1 = list(type = "SFO", to = "A2"),
    B1 = list(type = "SFO"),
    C1 = list(type = "SFO"),
    A2 = list(type = "SFO"))
fit2 <- mkinfit(model2, data, plot=TRUE, parms.ini = c(f_A1_to_A2 = 0.3))
summary(fit2)

model3 <- mkinmod(
    parent = list(type = "SFORB", to = c("A1", "B1", "C1"), sink = FALSE),
    A1 = list(type = "SFO", to = "A2"),
    B1 = list(type = "SFO"),
    C1 = list(type = "SFO"),
    A2 = list(type = "SFO"))
fit3 <- mkinfit(model3, data, plot=FALSE)
summary(fit3)

SFO_SFO <- mkinmod(parent = list(type = "SFO", to = "m1"),
	           m1 = list(type = "SFO"))
SFORB_SFO <- mkinmod(parent = list(type = "SFORB", to = "m1"),
	           m1 = list(type = "SFO"))
DFOP_SFO <- mkinmod(parent = list(type = "DFOP", to = "m1"),
	           m1 = list(type = "SFO"))

fit <- mkinfit(SFO_SFO, FOCUS_2006_D, plot=TRUE)
fit <- mkinfit(SFORB_SFO, FOCUS_2006_D, plot=TRUE)
fit <- mkinfit(DFOP_SFO, FOCUS_2006_D, plot=TRUE, parms.ini = c(f_parent_to_m1 = 0.5))
fit <- mkinfit(SFO_SFO, FOCUS_2006_D, parms.ini = fit$parms.all, plot=TRUE, eigen=TRUE)
summary(fit)

SFO <- mkinmod(parent = list(type = "SFO"))
fit <- mkinfit(SFO, FOCUS_2006_B, plot=TRUE)
FOMC <- mkinmod(parent = list(type = "FOMC"))
fit <- mkinfit(FOMC, FOCUS_2006_C, plot=TRUE, parms.ini = c(alpha = 3, beta = 10))
DFOP <- mkinmod(parent = list(type = "DFOP"))
fit <- mkinfit(DFOP, FOCUS_2006_B, plot=TRUE)
HS <- mkinmod(parent = list(type = "HS"))
fit <- mkinfit(HS, FOCUS_2006_C, plot=TRUE)
summary(fit)
