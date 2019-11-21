library(netmeta)
library(ggplot2)
library(grid)
library(dplyr)
library(reshape2)

def <- read.csv("active-efficacy.csv", header = TRUE, stringsAsFactors = FALSE)
defpw <- pairwise(treat = t, n, event = r, data = def, studlab = id, sm = "OR")

def90 <- subset(def, study_year < 1990)
defpw90 <- pairwise(treat = t, n, event = r, data = def90, studlab = id, sm = "OR")
netef90 <- netmeta(defpw90, comb.fixed = FALSE, comb.random = TRUE, reference.group = "citalopram")
d1990 <- data.frame(trts = netef90$trts, OR90 = netef90$TE.random[, "citalopram"],
                    lower90 = netef90$lower.random[, "citalopram"],
                    upper90 = netef90$upper.random[, "citalopram"])
def95 <- subset(def, study_year < 1995)
defpw95 <- pairwise(treat = t, n, event = r, data = def95, studlab = id, sm = "OR")
netef95 <- netmeta(defpw95, comb.fixed = FALSE, comb.random = TRUE, reference.group = "citalopram")
d1995 <- data.frame(trts = netef95$trts, OR95 = netef95$TE.random[, "citalopram"],
                    lower95 = netef95$lower.random[, "citalopram"],
                    upper95 = netef95$upper.random[, "citalopram"])
def00 <- subset(def, study_year < 2000)
defpw00 <- pairwise(treat = t, n, event = r, data = def00, studlab = id, sm = "OR")
netef00 <- netmeta(defpw00, comb.fixed = FALSE, comb.random = TRUE, reference.group = "citalopram")
d2000 <- data.frame(trts = netef00$trts, OR00 = netef00$TE.random[, "citalopram"],
                    lower00 = netef00$lower.random[, "citalopram"],
                    upper00 = netef00$upper.random[, "citalopram"])
def05 <- subset(def, study_year < 2005)
defpw05 <- pairwise(treat = t, n, event = r, data = def05, studlab = id, sm = "OR")
netef05 <- netmeta(defpw05, comb.fixed = FALSE, comb.random = TRUE, reference.group = "citalopram")
d2005 <- data.frame(trts = netef05$trts, OR05 = netef05$TE.random[, "citalopram"],
                    lower05 = netef05$lower.random[, "citalopram"],
                    upper05 = netef05$upper.random[, "citalopram"])
def10 <- subset(def, study_year < 2010)
defpw10 <- pairwise(treat = t, n, event = r, data = def10, studlab = id, sm = "OR")
netef10 <- netmeta(defpw10, comb.fixed = FALSE, comb.random = TRUE, reference.group = "citalopram")
d2010 <- data.frame(trts = netef10$trts, OR10 = netef10$TE.random[, "citalopram"],
                    lower10 = netef10$lower.random[, "citalopram"],
                    upper10 = netef10$upper.random[, "citalopram"])
netef16 <- netmeta(defpw, comb.fixed = FALSE, comb.random = TRUE, reference.group = "citalopram")
d2016 <- data.frame(trts = netef16$trts, OR16 = netef16$TE.random[, "citalopram"],
                    lower16 = netef16$lower.random[, "citalopram"],
                    upper16 = netef16$upper.random[, "citalopram"])
#combine many data frames together
##attention: all the ORs and CIs are logORs!
df <- Reduce(full_join, list(d1990, d1995, d2000, d2005, d2010, d2016))
arrange(df, trts)

newdata <- round(df[, -c(1)], digits = 2)
df <- cbind(df$trts, newdata)
#change the wide data into long data (*reshape2 package)
df <- melt(df, id.vars = "df$trts")
#change the column name
names(df)[names(df) == 'df$trts'] <- 'trts'
#reorder: put the data of the same drug together
df <- df[order(df$trts),]
df

year <- c(1990, 1995, 2000, 2005, 2010, 2016)
OR_list <- c("OR90", "OR95", "OR00", "OR05", "OR10", "OR16")
lower_list <- c("lower90", "lower95", "lower00", "lower05", "lower10", "lower16")
upper_list <- c("upper90", "upper95", "upper00", "upper05", "upper10", "upper16")
drug_list <- c("agomelatine", "amitriptyline", "bupropion", "citalopram", "clomipramine", 
          "duloxetine", "escitalopram", "fluoxetine", "fluvoxamine",
          "milnacipran", "mirtazapine", "nefazodone", "paroxetine",
          "reboxetine", "sertraline", "trazodone", "venlafaxine", "vortioxetine")


#draw forest plots
datalist = list()
ORlist = list()
lowerlist = list()
upperlist = list()
annolist = list ()
dfplotlist = list()
plist = list()
for (i in drug_list) {
  dat <- subset(df, trts == i)
  datalist[[i]] <- dat
  OR <- datalist[[i]][datalist[[i]]$variable %in% OR_list, ]$value
  ORlist[[i]] <- OR
  lower <- datalist[[i]][datalist[[i]]$variable %in% lower_list, ]$value
  lowerlist[[i]] <- lower
  upper <- datalist[[i]][datalist[[i]]$variable %in% upper_list, ]$value
  upperlist[[i]] <- upper
  anno <- paste(round(exp(ORlist[[i]]), digits = 2), "(", round(exp(lowerlist[[i]]), digits = 2), "to", round(exp(upperlist[[i]]), digits = 2), ")")
  annolist[[i]] <- anno
  dfplot <- na.omit(data.frame(year, ORlist[[i]], lowerlist[[i]], upperlist[[i]], annolist[[i]]))
  row.names(dfplot) <- (1:nrow(dfplot))
  names(dfplot)[names(dfplot) == 'ORlist..i..'] <- 'OR'
  names(dfplot)[names(dfplot) == 'lowerlist..i..'] <- 'lower'
  names(dfplot)[names(dfplot) == 'upperlist..i..'] <- 'upper'
  names(dfplot)[names(dfplot) == 'annolist..i..'] <- 'anno'
  dfplotlist[[i]] <- dfplot
  dfplotlist[[i]]$year <- factor(dfplotlist[[i]]$year, levels=rev(dfplotlist[[i]]$year))
  p <- ggplot(data = dfplotlist[[i]], aes(x = year, y = OR, ymin = lower, ymax = upper, label = anno)) +
    geom_hline(yintercept = 0, lty = 2) +
    geom_pointrange(shape = 15) +
    coord_flip() +
    theme(plot.margin = unit(c(1,8,1,1), "lines")) 
  plist[[i]] <- p
}


#add labels and adjust for each drug
##agom
plist[["agomelatine"]]
for (i in 1:3) {
  plist[["agomelatine"]] <- plist[["agomelatine"]] + 
    scale_y_continuous(breaks=c(log(0.75), log(1.0), log(1.25), log(1.5), log(1.75), log(2.0)), labels=c(0.75, 1.0, 1.25, 1.5, 1.75, 2.0)) +
    annotation_custom(
      grob = textGrob(label = dfplotlist[["agomelatine"]]$anno[i], hjust = 0, gp = gpar(cex = 0.9)),
      ymin = 0.77,
      ymax = 0.77,
      xmin = dfplotlist[["agomelatine"]]$year[i],
      xmax = dfplotlist[["agomelatine"]]$year[i]
  )
}
gt.agom <- ggplot_gtable(ggplot_build(plist[["agomelatine"]]))
gt.agom$layout$clip[gt.agom$layout$name == "panel"] <- "off"
grid.draw(gt.agom)

##amit
plist[["amitriptyline"]]
for (i in 1:6) {
  plist[["amitriptyline"]] <- plist[["amitriptyline"]] + 
    scale_y_continuous(breaks=c(log(0.5), log(1), log(2.5), log(5), log(7.5)), labels=c(0.5, 1, 2.5, 5, 7.5)) +
    annotation_custom(
      grob = textGrob(label = dfplotlist[["amitriptyline"]]$anno[i], hjust = 0, gp = gpar(cex = 0.9)),
      ymin = 2.5,
      ymax = 2.5,
      xmin = dfplotlist[["amitriptyline"]]$year[i],
      xmax = dfplotlist[["amitriptyline"]]$year[i]
  )
}
gt.amit <- ggplot_gtable(ggplot_build(plist[["amitriptyline"]]))
gt.amit$layout$clip[gt.amit$layout$name == "panel"] <- "off"
grid.draw(gt.amit)

##bupr
plist[["bupropion"]]
for (i in 1:5) {
  plist[["bupropion"]] <- plist[["bupropion"]] + 
    scale_y_continuous(breaks=c(log(0.5), log(1), log(3), log(5)), labels=c(0.5, 1, 3, 5)) +
    annotation_custom(
      grob = textGrob(label = dfplotlist[["bupropion"]]$anno[i], hjust = 0, gp = gpar(cex = 0.9)),
      ymin = 2.1,
      ymax = 2.1,
      xmin = dfplotlist[["bupropion"]]$year[i],
      xmax = dfplotlist[["bupropion"]]$year[i]
    )
}
gt.bupr <- ggplot_gtable(ggplot_build(plist[["bupropion"]]))
gt.bupr$layout$clip[gt.bupr$layout$name == "panel"] <- "off"
grid.draw(gt.bupr)

##clom
plist[["clomipramine"]]
for (i in 1:6) {
  plist[["clomipramine"]] <- plist[["clomipramine"]] + 
    scale_y_continuous(breaks=c(log(0.5), log(1), log(2), log(4)), labels=c(0.5, 1, 2, 4)) +
    annotation_custom(
      grob = textGrob(label = dfplotlist[["clomipramine"]]$anno[i], hjust = 0, gp = gpar(cex = 0.9)),
      ymin = 1.6,
      ymax = 1.6,
      xmin = dfplotlist[["clomipramine"]]$year[i],
      xmax = dfplotlist[["clomipramine"]]$year[i]
    )
}
gt.clom <- ggplot_gtable(ggplot_build(plist[["clomipramine"]]))
gt.clom$layout$clip[gt.clom$layout$name == "panel"] <- "off"
grid.draw(gt.clom)

##dulo
plist[["duloxetine"]]
for (i in 1:3) {
  plist[["duloxetine"]] <- plist[["duloxetine"]] + 
    scale_y_continuous(breaks=c(log(0.5), log(0.75), log(1), log(1.25), log(1.5), log(1.75)), labels=c(0.5, 0.75, 1, 1.25, 1.5, 1.75)) +
    annotation_custom(
      grob = textGrob(label = dfplotlist[["duloxetine"]]$anno[i], hjust = 0, gp = gpar(cex = 0.9)),
      ymin = 0.62,
      ymax = 0.62,
      xmin = dfplotlist[["duloxetine"]]$year[i],
      xmax = dfplotlist[["duloxetine"]]$year[i]
    )
}
gt.dulo <- ggplot_gtable(ggplot_build(plist[["duloxetine"]]))
gt.dulo$layout$clip[gt.dulo$layout$name == "panel"] <- "off"
grid.draw(gt.dulo)

##esci
plist[["escitalopram"]]
for (i in 1:3) {
  plist[["escitalopram"]] <- plist[["escitalopram"]] + 
    scale_y_continuous(breaks=c(log(0.5), log(0.75), log(1), log(1.25), log(1.5), log(1.75)), labels=c(0.5, 0.75, 1, 1.25, 1.5, 1.75)) +
    annotation_custom(
      grob = textGrob(label = dfplotlist[["escitalopram"]]$anno[i], hjust = 0, gp = gpar(cex = 0.9)),
      ymin = 0.64,
      ymax = 0.64,
      xmin = dfplotlist[["escitalopram"]]$year[i],
      xmax = dfplotlist[["escitalopram"]]$year[i]
    )
}
gt.esci <- ggplot_gtable(ggplot_build(plist[["escitalopram"]]))
gt.esci$layout$clip[gt.esci$layout$name == "panel"] <- "off"
grid.draw(gt.esci)

##fluo
plist[["fluoxetine"]]
for (i in 1:6) {
  plist[["fluoxetine"]] <- plist[["fluoxetine"]] + 
    scale_y_continuous(breaks=c(log(0.5), log(1), log(2.5), log(5), log(7.5)), labels=c(0.5, 1, 2.5, 5, 7.5)) +
    annotation_custom(
      grob = textGrob(label = dfplotlist[["fluoxetine"]]$anno[i], hjust = 0, gp = gpar(cex = 0.9)),
      ymin = 2.4,
      ymax = 2.4,
      xmin = dfplotlist[["fluoxetine"]]$year[i],
      xmax = dfplotlist[["fluoxetine"]]$year[i]
    )
}
gt.fluo <- ggplot_gtable(ggplot_build(plist[["fluoxetine"]]))
gt.fluo$layout$clip[gt.fluo$layout$name == "panel"] <- "off"
grid.draw(gt.fluo)

##fluv
plist[["fluvoxamine"]]
for (i in 1:6) {
  plist[["fluvoxamine"]] <- plist[["fluvoxamine"]] + 
    scale_y_continuous(breaks=c(log(0.5), log(1), log(2), log(4), log(6)), labels=c(0.5, 1, 2, 4, 6)) +
    annotation_custom(
      grob = textGrob(label = dfplotlist[["fluvoxamine"]]$anno[i], hjust = 0, gp = gpar(cex = 0.9)),
      ymin = 2.15,
      ymax = 2.15,
      xmin = dfplotlist[["fluvoxamine"]]$year[i],
      xmax = dfplotlist[["fluvoxamine"]]$year[i]
    )
}
gt.fluv <- ggplot_gtable(ggplot_build(plist[["fluvoxamine"]]))
gt.fluv$layout$clip[gt.fluv$layout$name == "panel"] <- "off"
grid.draw(gt.fluv)

##miln
plist[["milnacipran"]]
for (i in 1:6) {
  plist[["milnacipran"]] <- plist[["milnacipran"]] + 
    scale_y_continuous(breaks=c(log(0.5), log(1), log(2.5), log(5), log(7.5)), labels=c(0.5, 1, 2.5, 5, 7.5)) +
    annotation_custom(
      grob = textGrob(label = dfplotlist[["milnacipran"]]$anno[i], hjust = 0, gp = gpar(cex = 0.9)),
      ymin = 2.5,
      ymax = 2.5,
      xmin = dfplotlist[["milnacipran"]]$year[i],
      xmax = dfplotlist[["milnacipran"]]$year[i]
    )
}
gt.miln <- ggplot_gtable(ggplot_build(plist[["milnacipran"]]))
gt.miln$layout$clip[gt.miln$layout$name == "panel"] <- "off"
grid.draw(gt.miln)

##mirt
plist[["mirtazapine"]]
for (i in 1:4) {
  plist[["mirtazapine"]] <- plist[["mirtazapine"]] + 
    scale_y_continuous(breaks=c(log(0.5), log(0.75), log(1), log(1.25), log(1.5), log(1.75)), labels=c(0.5, 0.75, 1, 1.25, 1.5, 1.75)) +
    annotation_custom(
      grob = textGrob(label = dfplotlist[["mirtazapine"]]$anno[i], hjust = 0, gp = gpar(cex = 0.9)),
      ymin = 0.62,
      ymax = 0.62,
      xmin = dfplotlist[["mirtazapine"]]$year[i],
      xmax = dfplotlist[["mirtazapine"]]$year[i]
    )
}
gt.mirt <- ggplot_gtable(ggplot_build(plist[["mirtazapine"]]))
gt.mirt$layout$clip[gt.mirt$layout$name == "panel"] <- "off"
grid.draw(gt.mirt)

##nefa
plist[["nefazodone"]]
for (i in 1:4) {
  plist[["nefazodone"]] <- plist[["nefazodone"]] + 
    scale_y_continuous(breaks=c(log(0.5), log(0.75), log(1), log(1.25), log(1.5), log(1.75)), labels=c(0.5, 0.75, 1, 1.25, 1.5, 1.75)) +
    annotation_custom(
      grob = textGrob(label = dfplotlist[["nefazodone"]]$anno[i], hjust = 0, gp = gpar(cex = 0.9)),
      ymin = 0.68,
      ymax = 0.68,
      xmin = dfplotlist[["nefazodone"]]$year[i],
      xmax = dfplotlist[["nefazodone"]]$year[i]
    )
}
gt.nefa <- ggplot_gtable(ggplot_build(plist[["nefazodone"]]))
gt.nefa$layout$clip[gt.nefa$layout$name == "panel"] <- "off"
grid.draw(gt.nefa)

##paro
plist[["paroxetine"]]
for (i in 1:6) {
  plist[["paroxetine"]] <- plist[["paroxetine"]] + 
    scale_y_continuous(breaks=c(log(0.5), log (0.75), log(1), log(2), log(4), log(6)), labels=c(0.5, 0.75, 1, 2, 4, 6)) +
    annotation_custom(
      grob = textGrob(label = dfplotlist[["paroxetine"]]$anno[i], hjust = 0, gp = gpar(cex = 0.9)),
      ymin = 2.2,
      ymax = 2.2,
      xmin = dfplotlist[["paroxetine"]]$year[i],
      xmax = dfplotlist[["paroxetine"]]$year[i]
    )
}
gt.paro <- ggplot_gtable(ggplot_build(plist[["paroxetine"]]))
gt.paro$layout$clip[gt.paro$layout$name == "panel"] <- "off"
grid.draw(gt.paro)

##rebo
plist[["reboxetine"]]
for (i in 1:5) {
  plist[["reboxetine"]] <- plist[["reboxetine"]] + 
    scale_y_continuous(breaks=c(log(0.25), log (0.5), log(1), log(3), log(5)), labels=c(0.25, 0.5, 1, 3, 5)) +
    annotation_custom(
      grob = textGrob(label = dfplotlist[["reboxetine"]]$anno[i], hjust = 0, gp = gpar(cex = 0.9)),
      ymin = 2,
      ymax = 2,
      xmin = dfplotlist[["reboxetine"]]$year[i],
      xmax = dfplotlist[["reboxetine"]]$year[i]
    )
}
gt.rebo <- ggplot_gtable(ggplot_build(plist[["reboxetine"]]))
gt.rebo$layout$clip[gt.rebo$layout$name == "panel"] <- "off"
grid.draw(gt.rebo)

##sert
plist[["sertraline"]]
for (i in 1:5) {
  plist[["sertraline"]] <- plist[["sertraline"]] + 
    scale_y_continuous(breaks=c(log(0.5), log (0.75), log(1), log(2), log(4)), labels=c(0.5, 0.75, 1, 2, 4)) +
    annotation_custom(
      grob = textGrob(label = dfplotlist[["sertraline"]]$anno[i], hjust = 0, gp = gpar(cex = 0.9)),
      ymin = 1.8,
      ymax = 1.8,
      xmin = dfplotlist[["sertraline"]]$year[i],
      xmax = dfplotlist[["sertraline"]]$year[i]
    )
}
gt.sert <- ggplot_gtable(ggplot_build(plist[["sertraline"]]))
gt.sert$layout$clip[gt.sert$layout$name == "panel"] <- "off"
grid.draw(gt.sert)

##traz
plist[["trazodone"]]
for (i in 1:6) {
  plist[["trazodone"]] <- plist[["trazodone"]] + 
    scale_y_continuous(breaks=c(log(0.25), log (0.5), log(1), log(2), log(4), log(6)), labels=c(0.25, 0.5, 1, 2, 4, 6)) +
    annotation_custom(
      grob = textGrob(label = dfplotlist[["trazodone"]]$anno[i], hjust = 0, gp = gpar(cex = 0.9)),
      ymin = 2.2,
      ymax = 2.2,
      xmin = dfplotlist[["trazodone"]]$year[i],
      xmax = dfplotlist[["trazodone"]]$year[i]
    )
}
gt.traz <- ggplot_gtable(ggplot_build(plist[["trazodone"]]))
gt.traz$layout$clip[gt.traz$layout$name == "panel"] <- "off"
grid.draw(gt.traz)

##venl
plist[["venlafaxine"]]
for (i in 1:5) {
  plist[["venlafaxine"]] <- plist[["venlafaxine"]] + 
    scale_y_continuous(breaks=c(log(0.5), log (0.75), log(1), log(2), log(4), log(6)), labels=c(0.5, 0.75, 1, 2, 4, 6)) +
    annotation_custom(
      grob = textGrob(label = dfplotlist[["venlafaxine"]]$anno[i], hjust = 0, gp = gpar(cex = 0.9)),
      ymin = 2.2,
      ymax = 2.2,
      xmin = dfplotlist[["venlafaxine"]]$year[i],
      xmax = dfplotlist[["venlafaxine"]]$year[i]
    )
}
gt.venl <- ggplot_gtable(ggplot_build(plist[["venlafaxine"]]))
gt.venl$layout$clip[gt.venl$layout$name == "panel"] <- "off"
grid.draw(gt.venl)

##vort
plist[["vortioxetine"]]
plist[["vortioxetine"]] <- plist[["vortioxetine"]] + 
    scale_y_continuous(breaks=c(log(0.5), log (0.75), log(1), log(2)), labels=c(0.5, 0.75, 1, 2)) +
    annotation_custom(
      grob = textGrob(label = dfplotlist[["vortioxetine"]]$anno, hjust = 0, gp = gpar(cex = 0.9)),
      ymin = 1.2,
      ymax = 1.2,
      xmin = dfplotlist[["vortioxetine"]]$year,
      xmax = dfplotlist[["vortioxetine"]]$year
    )
gt.vort <- ggplot_gtable(ggplot_build(plist[["vortioxetine"]]))
gt.vort$layout$clip[gt.vort$layout$name == "panel"] <- "off"
grid.draw(gt.vort)
