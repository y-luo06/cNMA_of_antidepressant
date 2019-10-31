library(netmeta)

#preparing full dataset: all the studies compared with fluoxetine; files can be found in the Data folder
##efficacy network
##full (long-arm-based format)
fef16 <- read.csv("fluo-efficacy.csv", header = TRUE, stringsAsFactors = FALSE)
##full (contrast-based format)
fefpw16 <- pairwise(treat = t, n, event = r, data = fef16, studlab = id, sm = "OR")

##acceptability network
##full (long-arm-based format)
fac16 <- read.csv("fluo-acceptability.csv", header = TRUE, stringsAsFactors = FALSE)
##full (contrast-based format)
facpw16 <- pairwise(treat = t, n, event = r, data = fac16, studlab = id, sm = "OR")


#2016 efficacy
netfef16 <- netmeta(fefpw16, comb.fixed = FALSE, comb.random = TRUE)
##every drug is compared with fluoxetine, so as long as the first drug is fluoxetine, it would be fine
ord16 <- c("fluo", "agom", "amit", "bupr", "cita", "clom", "esci", "fluv", "miln", "mirt", "nefa", "paro", 
             "rebo", "sert", "traz", "venl")
colors16 <- c(agom = "thistle", amit = "lightgreen", bupr = "coral3", cita = "cadetblue4", clom = "orange3",
                esci = "pink", fluv = "dodgerblue4", miln = "goldenrod4", mirt = "yellow3", nefa = "darkgrey",
                paro = "gray9", rebo = "lightblue3", sert = "lightslateblue", traz = "red4", venl= "mediumvioletred", fluo = "red")
comparison16 <- netfef16$trts[netfef16$trts != "fluoxetine"]
##generate the png
png("funnel_plot_ef_16.png", width = 6, height = 6,
    units = 'in', res = 300)
funnel(netfef16, order = ord16, pch = rep(19), col = colors16, legend = FALSE)
legend("topleft", legend = comparison16, pch = rep(19),col = colors16, cex = 0.75) 
dev.off()
##see the result of tests
funnel(netfef16, order = ord16, pch = rep(19), col = colors16, legend = FALSE, linreg = TRUE, rank = TRUE, mm = TRUE, digits.pval = 2)

#2016 acceptability
netfac16 <- netmeta(facpw16, comb.fixed = FALSE, comb.random = TRUE)
##generate the png
png("funnel_plot_ac_16.png", width = 6, height = 6,
    units = 'in', res = 300)
funnel(netfac16, order = ord16, pch = rep(19), col = colors16, legend = FALSE)
legend("topleft", legend = comparison16, pch = rep(19),col = colors16, cex = 0.75) 
dev.off()
##see the result of tests
funnel(netfac16, order = ord16, pch = rep(19), col = colors16, legend = FALSE, linreg = TRUE, rank = TRUE, mm = TRUE, digits.pval = 2)


#2010 efficacy
##dataset preparation
fef10 <- subset(fef16, study_year < 2010)
fefpw10 <- pairwise(treat = t, n, event = r, data = fef10, studlab = id, sm = "OR")
fac10 <- subset(fac16, study_year < 2010)
facpw10 <- pairwise(treat = t, n, event = r, data = fac10, studlab = id, sm = "OR")

netfef10 <- netmeta(fefpw10, comb.fixed = FALSE, comb.random = TRUE)
##every drug is compared with fluoxetine, so as long as the first drug is fluoxetine, it would be fine
ord10 <- c("fluo", "agom", "amit", "bupr", "cita", "clom", "esci", "fluv", "miln", "mirt", "nefa", "paro", 
           "rebo", "sert", "traz", "venl")
colors10 <- c(agom = "thistle", amit = "lightgreen", bupr = "coral3", cita = "cadetblue4", clom = "orange3",
              esci = "pink", fluv = "dodgerblue4", miln = "goldenrod4", mirt = "yellow3", nefa = "darkgrey",
              paro = "gray9", rebo = "lightblue3", sert = "lightslateblue", traz = "red4", venl= "mediumvioletred", fluo = "red")
comparison10 <- netfef10$trts[netfef10$trts != "fluoxetine"]
##generate the png
png("funnel_plot_ef_10.png", width = 6, height = 6,
    units = 'in', res = 300)
funnel(netfef10, order = ord10, pch = rep(19), col = colors10, legend = FALSE)
legend("topleft", legend = comparison10, pch = rep(19),col = colors10, cex = 0.75) 
dev.off()
##see the result of tests
funnel(netfef10, order = ord10, pch = rep(19), col = colors10, legend = FALSE, linreg = TRUE, rank = TRUE, mm = TRUE, digits.pval = 2)

#2010 acceptability
netfac10 <- netmeta(facpw10, comb.fixed = FALSE, comb.random = TRUE)
##generate the png
png("funnel_plot_ac_10.png", width = 6, height = 6,
    units = 'in', res = 300)
funnel(netfac10, order = ord10, pch = rep(19), col = colors10, legend = FALSE)
legend("topleft", legend = comparison10, pch = rep(19),col = colors10, cex = 0.75) 
dev.off()
##see the result of tests
funnel(netfac10, order = ord10, pch = rep(19), col = colors10, legend = FALSE, linreg = TRUE, rank = TRUE, mm = TRUE, digits.pval = 2)


#2005 efficacy
##dataset preparation
fef05 <- subset(fef16, study_year < 2005)
fefpw05 <- pairwise(treat = t, n, event = r, data = fef05, studlab = id, sm = "OR")
fac05 <- subset(fac16, study_year < 2005)
facpw05 <- pairwise(treat = t, n, event = r, data = fac05, studlab = id, sm = "OR")

netfef05 <- netmeta(fefpw05, comb.fixed = FALSE, comb.random = TRUE)
##every drug is compared with fluoxetine, so as long as the first drug is fluoxetine, it would be fine
ord05 <- c("fluo", "amit", "bupr", "cita", "clom", "esci", "fluv", "miln", "mirt", "nefa", "paro", 
           "rebo", "sert", "traz", "venl")
colors05 <- c(amit = "lightgreen", bupr = "coral3", cita = "cadetblue4", clom = "orange3",
              esci = "pink", fluv = "dodgerblue4", miln = "goldenrod4", mirt = "yellow3", nefa = "darkgrey",
              paro = "gray9", rebo = "lightblue3", sert = "lightslateblue", traz = "red4", venl= "mediumvioletred", fluo = "red")
comparison05 <- netfef05$trts[netfef05$trts != "fluoxetine"]
##generate the png
png("funnel_plot_ef_05.png", width = 6, height = 6,
    units = 'in', res = 300)
funnel(netfef05, order = ord05, pch = rep(19), col = colors05, legend = FALSE)
legend("topleft", legend = comparison05, pch = rep(19), col = colors05, cex = 0.75) 
dev.off()
##see the result of tests
funnel(netfef05, order = ord05, pch = rep(19), col = colors05, legend = FALSE, linreg = TRUE, rank = TRUE, mm = TRUE, digits.pval = 2)

#2005 acceptability
netfac05 <- netmeta(facpw05, comb.fixed = FALSE, comb.random = TRUE)
##generate the png
png("funnel_plot_ac_05.png", width = 6, height = 6,
    units = 'in', res = 300)
funnel(netfac05, order = ord05, pch = rep(19), col = colors05, legend = FALSE)
legend("topleft", legend = comparison05, pch = rep(19), col = colors05, cex = 0.75) 
dev.off()
##see the result of tests
funnel(netfac05, order = ord05, pch = rep(19), col = colors05, legend = FALSE, linreg = TRUE, rank = TRUE, mm = TRUE, digits.pval = 2)


#2000 efficacy
##dataset preparation
fef00 <- subset(fef16, study_year < 2000)
fefpw00 <- pairwise(treat = t, n, event = r, data = fef00, studlab = id, sm = "OR")
fac00 <- subset(fac16, study_year < 2000)
facpw00 <- pairwise(treat = t, n, event = r, data = fac00, studlab = id, sm = "OR")

netfef00 <- netmeta(fefpw00, comb.fixed = FALSE, comb.random = TRUE)
##every drug is compared with fluoxetine, so as long as the first drug is fluoxetine, it would be fine
ord00 <- c("fluo", "amit", "bupr", "cita", "clom", "fluv", "miln", "mirt", "nefa", "paro", 
           "rebo", "sert", "traz", "venl")
colors00 <- c(amit = "lightgreen", bupr = "coral3", cita = "cadetblue4", clom = "orange3",
              fluv = "dodgerblue4", miln = "goldenrod4", mirt = "yellow3", nefa = "darkgrey",
              paro = "gray9", rebo = "lightblue3", sert = "lightslateblue", traz = "red4", venl= "mediumvioletred")
comparison00 <- netfef00$trts[netfef00$trts != "fluoxetine"]
##generate the png
png("funnel_plot_ef_00.png", width = 6, height = 6,
    units = 'in', res = 300)
funnel(netfef00, order = ord00, pch = rep(19), col = colors00, legend = FALSE)
legend("topleft", legend = comparison00, pch = rep(19), col = colors00, cex = 0.75) 
dev.off()
##see the result of tests
funnel(netfef00, order = ord00, pch = rep(19), col = colors00, legend = FALSE, linreg = TRUE, rank = TRUE, mm = TRUE, digits.pval = 2)

#2000 acceptability
netfac00 <- netmeta(facpw00, comb.fixed = FALSE, comb.random = TRUE)
##generate the png
png("funnel_plot_ac_00.png", width = 6, height = 6,
    units = 'in', res = 300)
funnel(netfac00, order = ord00, pch = rep(19), col = colors00, legend = FALSE)
legend("topleft", legend = comparison00, pch = rep(19), col = colors00, cex = 0.75) 
dev.off()
##see the result of tests
funnel(netfac00, order = ord00, pch = rep(19), col = colors00, legend = FALSE, linreg = TRUE, rank = TRUE, mm = TRUE, digits.pval = 2)


#1995 efficacy
##dataset preparation
fef95 <- subset(fef16, study_year < 1995)
fefpw95 <- pairwise(treat = t, n, event = r, data = fef95, studlab = id, sm = "OR")
fac95 <- subset(fac16, study_year < 1995)
facpw95 <- pairwise(treat = t, n, event = r, data = fac95, studlab = id, sm = "OR")

netfef95 <- netmeta(fefpw95, comb.fixed = FALSE, comb.random = TRUE)
##every drug is compared with fluoxetine, so as long as the first drug is fluoxetine, it would be fine
ord95 <- c("fluo", "amit", "bupr", "clom", "miln", "paro", 
           "rebo", "sert", "traz", "venl")
colors95 <- c(amit = "lightgreen", bupr = "coral3", clom = "orange3",
              miln = "goldenrod4", paro = "gray9", rebo = "lightblue3", sert = "lightslateblue", traz = "red4", venl= "mediumvioletred")
comparison95 <- netfef95$trts[netfef95$trts != "fluoxetine"]
##generate the png
png("funnel_plot_ef_95.png", width = 6, height = 6,
    units = 'in', res = 300)
funnel(netfef95, order = ord95, pch = rep(19), col = colors95, legend = FALSE)
legend("topleft", legend = comparison95, pch = rep(19), col = colors95, cex = 0.75) 
dev.off()
##see the result of tests
funnel(netfef95, order = ord95, pch = rep(19), col = colors95, legend = FALSE, linreg = TRUE, rank = TRUE, mm = TRUE, digits.pval = 2)

#1995 acceptability
netfac95 <- netmeta(facpw95, comb.fixed = FALSE, comb.random = TRUE)
##generate the png
png("funnel_plot_ac_95.png", width = 6, height = 6,
    units = 'in', res = 300)
funnel(netfac95, order = ord95, pch = rep(19), col = colors95, legend = FALSE)
legend("topleft", legend = comparison95, pch = rep(19), col = colors95, cex = 0.75) 
dev.off()
##see the result of tests
funnel(netfac95, order = ord95, pch = rep(19), col = colors95, legend = FALSE, linreg = TRUE, rank = TRUE, mm = TRUE, digits.pval = 2)


#1990 efficacy
##dataset preparation
fef90 <- subset(fef16, study_year < 1990)
fefpw90 <- pairwise(treat = t, n, event = r, data = fef90, studlab = id, sm = "OR")
fac90 <- subset(fac16, study_year < 1990)
facpw90 <- pairwise(treat = t, n, event = r, data = fac90, studlab = id, sm = "OR")

netfef90 <- netmeta(fefpw90, comb.fixed = FALSE, comb.random = TRUE)
##every drug is compared with fluoxetine, so as long as the first drug is fluoxetine, it would be fine
ord90 <- c("fluo", "amit", "traz", "clom")
colors90 <- c(amit = "lightgreen", clom = "orange3", traz = "red4")
comparison90 <- netfef90$trts[netfef90$trts != "fluoxetine"]
##generate the png
png("funnel_plot_ef_90.png", width = 6, height = 6,
    units = 'in', res = 300)
funnel(netfef90, order = ord90, pch = rep(19), col = colors90, legend = FALSE)
legend("topleft", legend = comparison90, pch = rep(19), col = colors90, cex = 0.75) 
dev.off()
##see the result of tests
funnel(netfef90, order = ord90, pch = rep(19), col = colors90, legend = FALSE, linreg = TRUE, rank = TRUE, mm = TRUE, digits.pval = 2)

#1990 acceptability
netfac90 <- netmeta(facpw90, comb.fixed = FALSE, comb.random = TRUE)
##generate the png
png("funnel_plot_ac_90.png", width = 6, height = 6,
    units = 'in', res = 300)
funnel(netfac90, order = ord90, pch = rep(19), col = colors90, legend = FALSE)
legend("topleft", legend = comparison90, pch = rep(19), col = colors90, cex = 0.75) 
dev.off()
##see the result of tests
funnel(netfac90, order = ord90, pch = rep(19), col = colors90, legend = FALSE, linreg = TRUE, rank = TRUE, mm = TRUE, digits.pval = 2)
