library(netmeta)

#preparing full dataset: all the studies compared with fluoxetine
##efficacy network
##full (long-arm-based format)
fef <- read.csv("fluo-ef.csv", header = TRUE, stringsAsFactors = FALSE)

##acceptability network
##full (long-arm-based format)
fac <- read.csv("fluo-ac.csv", header = TRUE, stringsAsFactors = FALSE)

colors <- c(agom = "thistle", amit = "lightgreen", bupr = "coral3", cita = "cadetblue4", clom = "orange3",
            esci = "pink", fluv = "dodgerblue4", miln = "goldenrod4", mirt = "yellow3", nefa = "darkgrey",
            paro = "gray9", rebo = "lightblue3", sert = "lightslateblue", traz = "red4", venl= "mediumvioletred")
years = c(1990, 1995, 2000, 2005, 2010, 2016)

netmeta_ef = list()
netmeta_ac = list()
comparison_ef = list()
comparison_ac = list()
ord_ef = list()
ord_ac = list()
for (year in years) {
    dfef <- subset(fef, study_year < year)
    dfefpw <- pairwise(treat = t, n, event = r, data = dfef, studlab = id, sm = "OR")
    dfac <- subset(fac, study_year < year)
    dfacpw <- pairwise(treat = t, n, event = r, data = dfac, studlab = id, sm = "OR")
    netmeta_ef[[year]] <- netmeta(dfefpw, comb.fixed = FALSE, comb.random = TRUE)
    netmeta_ac[[year]] <- netmeta(dfacpw, comb.fixed = FALSE, comb.random = TRUE)
    trts_ef <- substr(netmeta_ef[[year]]$trts, 0, 4)
    trts_ac <- substr(netmeta_ac[[year]]$trts, 0, 4)
    comparison_ef[[year]] <- trts_ef[trts_ef != "fluo"]
    comparison_ac[[year]] <- trts_ac[trts_ac != "fluo"]
    ord_ef[[year]] <- c(comparison_ef[[year]], "fluo")
    ord_ac[[year]] <- c(comparison_ac[[year]], "fluo")
}


#2016 efficacy
##generate the png, which will be saved in your computer directly (not in the "Plots" of R)
png("funnel_plot_ef_16.png", width = 6, height = 6,
    units = 'in', res = 300)
funnel(netmeta_ef[[2016]], order = ord_ef[[2016]], pch = rep(19), col = colors[comparison_ef[[2016]]], legend = FALSE, linreg = TRUE, text.linreg = "(Egger's test)", pos.tests = "topright")
legend("topleft", legend = comparison_ef[[2016]], pch = rep(19),col = colors[comparison_ef[[2016]]], cex = 0.75) 
dev.off()
#2016 acceptability
png("funnel_plot_ac_16.png", width = 6, height = 6,
    units = 'in', res = 300)
funnel(netmeta_ac[[2016]], order = ord_ac[[2016]], pch = rep(19), col = colors[comparison_ac[[2016]]], legend = FALSE, linreg = TRUE, text.linreg = "(Egger's test)", pos.tests = "topright")
legend("topleft", legend = comparison_ac[[2016]], pch = rep(19),col = colors[comparison_ac[[2016]]], cex = 0.75) 
dev.off()

#2010 efficacy
png("funnel_plot_ef_10.png", width = 6, height = 6,
    units = 'in', res = 300)
funnel(netmeta_ef[[2010]], order = ord_ef[[2010]], pch = rep(19), col = colors[comparison_ef[[2010]]], legend = FALSE, linreg = TRUE, text.linreg = "(Egger's test)", pos.tests = "topright")
legend("topleft", legend = comparison_ef[[2010]], pch = rep(19),col = colors[comparison_ef[[2010]]], cex = 0.75) 
dev.off()
#2010 acceptability
png("funnel_plot_ac_10.png", width = 6, height = 6,
    units = 'in', res = 300)
funnel(netmeta_ac[[2010]], order = ord_ac[[2010]], pch = rep(19), col = colors[comparison_ac[[2010]]], legend = FALSE, linreg = TRUE, text.linreg = "(Egger's test)", pos.tests = "topright")
legend("topleft", legend = comparison_ac[[2010]], pch = rep(19),col = colors[comparison_ac[[2010]]], cex = 0.75) 
dev.off()

#2005 efficacy
png("funnel_plot_ef_05.png", width = 6, height = 6,
    units = 'in', res = 300)
funnel(netmeta_ef[[2005]], order = ord_ef[[2005]], pch = rep(19), col = colors[comparison_ef[[2005]]], legend = FALSE, linreg = TRUE, text.linreg = "(Egger's test)", pos.tests = "topright")
legend("topleft", legend = comparison_ef[[2005]], pch = rep(19),col = colors[comparison_ef[[2005]]], cex = 0.75) 
dev.off()
#2005 acceptability
png("funnel_plot_ac_05.png", width = 6, height = 6,
    units = 'in', res = 300)
funnel(netmeta_ac[[2005]], order = ord_ac[[2005]], pch = rep(19), col = colors[comparison_ac[[2005]]], legend = FALSE, linreg = TRUE, text.linreg = "(Egger's test)", pos.tests = "topright")
legend("topleft", legend = comparison_ac[[2005]], pch = rep(19),col = colors[comparison_ac[[2005]]], cex = 0.75) 
dev.off()

#2000 efficacy
png("funnel_plot_ef_00.png", width = 6, height = 6,
    units = 'in', res = 300)
funnel(netmeta_ef[[2000]], order = ord_ef[[2000]], pch = rep(19), col = colors[comparison_ef[[2000]]], legend = FALSE, linreg = TRUE, text.linreg = "(Egger's test)", pos.tests = "topright")
legend("topleft", legend = comparison_ef[[2000]], pch = rep(19),col = colors[comparison_ef[[2000]]], cex = 0.75) 
dev.off()
#2000 acceptability
png("funnel_plot_ac_00.png", width = 6, height = 6,
    units = 'in', res = 300)
funnel(netmeta_ac[[2000]], order = ord_ac[[2000]], pch = rep(19), col = colors[comparison_ac[[2000]]], legend = FALSE, linreg = TRUE, text.linreg = "(Egger's test)", pos.tests = "topright")
legend("topleft", legend = comparison_ac[[2000]], pch = rep(19),col = colors[comparison_ac[[2000]]], cex = 0.75) 
dev.off()

#1995 efficacy
png("funnel_plot_ef_95.png", width = 6, height = 6,
    units = 'in', res = 300)
funnel(netmeta_ef[[1995]], order = ord_ef[[1995]], pch = rep(19), col = colors[comparison_ef[[1995]]], legend = FALSE, linreg = TRUE, text.linreg = "(Egger's test)", pos.tests = "topright")
legend("topleft", legend = comparison_ef[[1995]], pch = rep(19),col = colors[comparison_ef[[1995]]], cex = 0.75) 
dev.off()
#1995 acceptability
png("funnel_plot_ac_95.png", width = 6, height = 6,
    units = 'in', res = 300)
funnel(netmeta_ac[[1995]], order = ord_ac[[1995]], pch = rep(19), col = colors[comparison_ac[[1995]]], legend = FALSE, linreg = TRUE, text.linreg = "(Egger's test)", pos.tests = "topright")
legend("topleft", legend = comparison_ac[[1995]], pch = rep(19),col = colors[comparison_ac[[1995]]], cex = 0.75) 
dev.off()

#1990 efficacy
png("funnel_plot_ef_90.png", width = 6, height = 6,
    units = 'in', res = 300)
funnel(netmeta_ef[[1990]], order = ord_ef[[1990]], pch = rep(19), col = colors[comparison_ef[[1990]]], legend = FALSE, linreg = TRUE, text.linreg = "(Egger's test)", pos.tests = "topright")
legend("topleft", legend = comparison_ef[[1990]], pch = rep(19),col = colors[comparison_ef[[1990]]], cex = 0.75) 
dev.off()
#1990 acceptability
png("funnel_plot_ac_90.png", width = 6, height = 6,
    units = 'in', res = 300)
funnel(netmeta_ac[[1990]], order = ord_ac[[1990]], pch = rep(19), col = colors[comparison_ac[[1990]]], legend = FALSE, linreg = TRUE, text.linreg = "(Egger's test)", pos.tests = "topright")
legend("topleft", legend = comparison_ac[[1990]], pch = rep(19),col = colors[comparison_ac[[1990]]], cex = 0.75) 
dev.off()

