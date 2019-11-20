library(netmeta)
library(ggplot2)
library(scatterpie)
library(scales)
library(ggrepel)

#preparing full dataset
##efficacy network
##full (long-arm-based format)
def <- read.csv("active-efficacy.csv", header = TRUE, stringsAsFactors = FALSE)
##full (contrast-based format)
defpw <- pairwise(treat = t, n, event = r, data = def, studlab = id, sm = "OR")

##acceptability network
##full (long-arm-based format)
dac <- read.csv("active-acceptability.csv", header = TRUE, stringsAsFactors = FALSE)
##full (contrast-based format)
dacpw <- pairwise(treat = t, n, event = r, data = dac, studlab = id, sm = "OR")

#prepare the function for log transformation
##it is not possible to reverse + log for an axis at the same time
reverselog_trans <- function(base = exp(1)) {
  trans <- function(x) -log(x, base)
  inv <- function(x) base^(-x)
  trans_new(paste0("reverselog-", format(base)), trans, inv, 
            log_breaks(base = base), 
            domain = c(1e-100, Inf))
}


#NMA in 2016
##nma for efficacy: random-effect model
netef16 <- netmeta(defpw, comb.fixed = FALSE, comb.random = TRUE, reference.group = "citalopram")
##nma for acceptability: random-effect model
netac16 <- netmeta(dacpw, comb.fixed = FALSE, comb.random = TRUE, reference.group = "citalopram")

##prepare the data frame for the 2D plot
defor16 <- data.frame(trts = netef16$trts,
                      ORef = exp(netef16$TE.random[, netef16$reference.group]),
                      loweref = exp(netef16$lower.random[, netef16$reference.group]),
                      upperef = exp(netef16$upper.random[, netef16$reference.group]),
                      sample_ef = netef16$n.trts)
dacor16 <- data.frame(trts = netac16$trts,
                    ORac = exp(netac16$TE.random[, netac16$reference.group]),
                    lowerac = exp(netac16$lower.random[, netac16$reference.group]),
                    upperac = exp(netac16$upper.random[, netac16$reference.group]),
                    sample_ac = netac16$n.trts)
##merge dataframe
dator16 <- merge(defor16, dacor16, by = "trts", all = TRUE)
dator16$trts <- substr(dator16$trts, 0, 4)

##add confidence information (results from CINeMA, please refer to the article/appendix)
dcon16 <- data.frame(trts = c("agom", "amit", "bupr", "cita", "clom", "dulo", "esci", "fluo", "fluv", 
                            "miln", "mirt", "nefa", "paro", "rebo", "sert", "traz", "venl", "vort"),
                   vlow = c(0, 0.1765, 0.1471, 0, 0.1765, 0.1176, 0, 0.0294, 0.2059, 0.0882, 0, 0.3529,
                            0.0295, 0.1471, 0.0588, 0.2353, 0.0588, 0),
                   low = c(0.2647, 0.7647, 0.6176, 0.4706, 0.7059, 0.3529, 0.3235, 0.4706, 0.6176, 0.9118,
                           0.3235, 0.4706, 0.4412, 0.7353, 0.5, 0.6765, 0.4412, 0.2059), 
                   mod = c(0.6765, 0.0588, 0.2353, 0.5, 0.1176, 0.4706, 0.4706, 0.2647, 0.1764, 0, 0.5588, 0.1765,
                           0.4118, 0.1176, 0.4118, 0.0882, 0.4412, 0.7353),
                   high = c(0.0588, 0, 0, 0.0294, 0, 0.0588, 0.2059, 0.2353, 0, 0, 0.0882, 0, 0.1176, 0, 0.0294, 0,
                            0.0588, 0.0588),
                   radius = 1/(5*netef16$seTE.random[, netef16$reference.group]))
dcon16["citalopram", "radius"] = 1
dator16 <- merge(dator16, dcon16, by = "trts", all = TRUE)

##draw plot
##dots -> pie charts, size reflects SE, log scale
plt16 <- ggplot(dator16, aes(x = ORef, y = ORac, label = trts)) + labs(x = "OR of efficacy", y = "OR of acceptability") +
  geom_hline(yintercept = 1, linetype = "dotted") + geom_vline(xintercept = 1, linetype = "dotted")
  scale_y_continuous(trans = reverselog_trans(10), limits = c(2.2, 0.2), breaks = seq(2.2, 0.2, -0.2)) + 
  scale_x_continuous(limits = c(0.6, 3.6), breaks = seq(0.6, 3.6, 0.2), trans = "log10") +
  annotation_logticks() +
  geom_text_repel(data = subset(dator16, ORac < 1.1), aes(fontface = ifelse(subset(dator16, ORac < 1.1)$trts == "vort", "bold", "plain")),
                  col = ifelse(subset(dator16, ORac < 1.1)$trts == "vort", "hotpink3", "gray29"), 
                  cex = 3, point.padding = 0.3, nudge_y = 0.05, force = 3, segment.size = 0.2, segment.color = "grey50") +
  geom_text_repel(data = subset(dator16, ORac > 1.1), col = "gray29", cex = 3, point.padding = 0.3, 
                  nudge_y = -0.05, force = 3, segment.size = 0.2, segment.color = "grey50") +
  geom_hline(yintercept = 1, linetype = "dotted", col = "lightblue4") + geom_vline(xintercept = 1, linetype = "dotted", col = "lightblue4") +
  geom_scatterpie(data = dator16, aes(x = ORef, y = ORac, r = 0.012*radius), cols = colnames(dator16[, 10:13]), alpha = 0.9, color = NA, show.legend = FALSE) +
  scale_fill_manual(values = c(vlow = "red", low = "yellow", mod = "steelblue3", high = "limegreen")) + theme_bw()
plt16


#NMA in 2010
##dataset preparation
def10 <- subset(def, study_year < 2010)
defpw10 <- pairwise(treat = t, n, event = r, data = def10, studlab = id, sm = "OR")
dac10 <- subset(dac, study_year < 2010)
dacpw10 <- pairwise(treat = t, n, event = r, data = dac10, studlab = id, sm = "OR")
##nma for efficacy: random-effect model
netef10 <- netmeta(defpw10, comb.fixed = FALSE, comb.random = TRUE, reference.group = "citalopram")
##nma for acceptability: random-effect model
netac10 <- netmeta(dacpw10, comb.fixed = FALSE, comb.random = TRUE, reference.group = "citalopram")

##prepare the data frame for the 2D plot
defor10 <- data.frame(trts = netef10$trts,
                      ORef = exp(netef10$TE.random[, netef10$reference.group]),
                      loweref = exp(netef10$lower.random[, netef10$reference.group]),
                      upperef = exp(netef10$upper.random[, netef10$reference.group]),
                      sample_ef = netef10$n.trts)
dacor10 <- data.frame(trts = netac10$trts,
                      ORac = exp(netac10$TE.random[, netac10$reference.group]),
                      lowerac = exp(netac10$lower.random[, netac10$reference.group]),
                      upperac = exp(netac10$upper.random[, netac10$reference.group]),
                      sample_ac = netac10$n.trts)
##merge dataframe
dator10 <- merge(defor10, dacor10, by = "trts", all = TRUE)
dator10$trts <- substr(dator10$trts, 0, 4)
##add confidence information
dcon10 <- data.frame(trts = c("agom", "amit", "bupr", "cita", "clom", "dulo", "esci", "fluo", "fluv", 
                              "miln", "mirt", "nefa", "paro", "rebo", "sert", "traz", "venl"),
                     vlow = c(0, 0.15625, 0.125, 0.03125, 0.1875, 0.1875, 0, 0, 0.1875, 0.21875, 0, 0.28125,
                              0.0625, 0.125, 0.0625, 0.15625, 0.0625),
                     low = c(0.09375, 0.65625, 0.5625, 0.375, 0.71875, 0.25, 0.1875, 0.5, 0.5625, 0.6875,
                             0.375, 0.53125, 0.40625, 0.8125, 0.46875, 0.65625, 0.375), 
                     mod = c(0.53125, 0.1875, 0.3125, 0.3125, 0.09375, 0.3125, 0.46875, 0.25, 0.25, 0.09375, 0.3125, 0.15625,
                             0.3125, 0.0625, 0.34375, 0.1875, 0.375),
                     high = c(0.375, 0, 0, 0.28125, 0, 0.25, 0.34375, 0.25, 0, 0, 0.3125, 0.03125, 0.21875, 0, 0.125, 0,
                              0.1875),
                     radius = 1/(5*netef10$seTE.random[, netef10$reference.group]))
dcon10["citalopram", "radius"] = 1
dator10 <- merge(dator10, dcon10, by = "trts", all = TRUE)

##draw plot
##dots -> pie charts, size reflects SE, log scale
plt10 <- ggplot(dator10, aes(x = ORef, y = ORac, label = trts)) + labs(x = "OR of efficacy", y = "OR of acceptability") +
  scale_y_continuous(trans = reverselog_trans(10), limits = c(2.2, 0.2), breaks = seq(2.2, 0.2, -0.2)) + 
  scale_x_continuous(limits = c(0.6, 3.6), breaks = seq(0.6, 3.6, 0.2), trans = "log10") +
  annotation_logticks() +
  geom_text_repel(data = subset(dator10, ORac < 1.1), col = "gray29", 
                  cex = 3, point.padding = 0.3, nudge_y = 0.05, force = 3, segment.size = 0.2, segment.color = "grey50") +
  geom_text_repel(data = subset(dator10, ORac > 1.1), col = "gray29",
                  cex = 3, point.padding = 0.3, nudge_y = -0.05, force = 3, segment.size = 0.2, segment.color = "grey50") +
  geom_hline(yintercept = 1, linetype = "dotted", col = "lightblue4") + geom_vline(xintercept = 1, linetype = "dotted", col = "lightblue4") +
  geom_scatterpie(data = dator10, aes(x = ORef, y = ORac, r = 0.012*radius), cols = colnames(dator10[, 10:13]), alpha = 0.9, color = NA, show.legend = FALSE) +
  scale_fill_manual(values = c(vlow = "red", low = "yellow", mod = "steelblue3", high = "limegreen")) + theme_bw()
plt10


#NMA in 2005
##dataset preparation
def05 <- subset(def, study_year < 2005)
defpw05 <- pairwise(treat = t, n, event = r, data = def05, studlab = id, sm = "OR")
dac05 <- subset(dac, study_year < 2005)
dacpw05 <- pairwise(treat = t, n, event = r, data = dac05, studlab = id, sm = "OR")
##nma for efficacy: random-effect model
netef05 <- netmeta(defpw05, comb.fixed = FALSE, comb.random = TRUE, reference.group = "citalopram")
##nma for acceptability: random-effect model
netac05 <- netmeta(dacpw05, comb.fixed = FALSE, comb.random = TRUE, reference.group = "citalopram")

##prepare the data frame for the 2D plot
defor05 <- data.frame(trts = netef05$trts,
                      ORef = exp(netef05$TE.random[, netef05$reference.group]),
                      loweref = exp(netef05$lower.random[, netef05$reference.group]),
                      upperef = exp(netef05$upper.random[, netef05$reference.group]),
                      sample_ef = netef05$n.trts)
dacor05 <- data.frame(trts = netac05$trts,
                      ORac = exp(netac05$TE.random[, netac05$reference.group]),
                      lowerac = exp(netac05$lower.random[, netac05$reference.group]),
                      upperac = exp(netac05$upper.random[, netac05$reference.group]),
                      sample_ac = netac05$n.trts)
##merge dataframe
dator05 <- merge(defor05, dacor05, by = "trts", all = TRUE)
dator05$trts <- substr(dator05$trts, 0, 4)
##add confidence information
dcon05 <- data.frame(trts = c("agom", "amit", "bupr", "cita", "clom", "dulo", "esci", "fluo", "fluv", 
                              "miln", "mirt", "nefa", "paro", "rebo", "sert", "traz", "venl"),
                     vlow = c(0.03125, 0.21875, 0.21875, 0.0625, 0.25, 0.1875, 0.09375, 0.0625, 0.28125, 0.28125, 0, 0.28125,
                              0.03125, 0.34375, 0.0625, 0.25, 0.09375),
                     low = c(0.40625, 0.6875, 0.6875, 0.40625, 0.75, 0.46875, 0.4375, 0.4375, 0.5625, 0.65625,
                             0.4375, 0.625, 0.59375, 0.65625, 0.46875, 0.71875, 0.46875), 
                     mod = c(0.4375, 0.09375, 0.09375, 0.40625, 0, 0.28125, 0.34375, 0.46875, 0.15625, 0.0625, 0.46875, 0.09375,
                             0.34375, 0, 0.40625, 0.03125, 0.34375),
                     high = c(0.125, 0, 0, 0.125, 0, 0.0625, 0.125, 0.03125, 0, 0, 0.09375, 0, 0.03125, 0, 0.0625, 0,
                              0.09375),
                     radius = 1/(5*netef05$seTE.random[, netef05$reference.group]))
dcon05["citalopram", "radius"] = 1
dator05 <- merge(dator05, dcon05, by = "trts", all = TRUE)

##draw plot
##dots -> pie charts, size reflects SE, log scale
plt05 <- ggplot(dator05, aes(x = ORef, y = ORac, label = trts)) + labs(x = "OR of efficacy", y = "OR of acceptability") +
  scale_y_continuous(trans = reverselog_trans(10), limits = c(2.2, 0.2), breaks = seq(2.2, 0.2, -0.2)) + 
  scale_x_continuous(limits = c(0.6, 3.6), breaks = seq(0.6, 3.6, 0.2), trans = "log10") +
  annotation_logticks() +
  geom_text_repel(data = subset(dator05, ORac <= 1.2), aes(fontface = ifelse(subset(dator05, ORac <= 1.2)$trts %in% c("agom", "esci", "dulo"), "bold", "plain")),
                  col = ifelse(subset(dator05, ORac <= 1.2)$trts %in% c("agom", "esci", "dulo"), "hotpink3", "gray29"), 
                  cex = 3, point.padding = 0.3, nudge_y = 0.05, force = 3, segment.size = 0.2, segment.color = "grey50") +
  geom_text_repel(data = subset(dator05, ORac > 1.2), aes(fontface = ifelse(subset(dator05, ORac > 1.2)$trts %in% c("agom", "esci", "dulo"), "bold", "plain")),
                  col = ifelse(subset(dator05, ORac > 1.2)$trts %in% c("agom", "esci", "dulo"), "hotpink3", "gray29"),
                  cex = 3, point.padding = 0.3, nudge_y = -0.05, force = 3, segment.size = 0.2, segment.color = "grey50") +
  geom_hline(yintercept = 1, linetype = "dotted", col = "lightblue4") + geom_vline(xintercept = 1, linetype = "dotted", col = "lightblue4") +
  geom_scatterpie(data = dator05, aes(x = ORef, y = ORac, r = 0.012*radius), cols = colnames(dator05[, 10:13]), alpha = 0.9, color = NA, show.legend = FALSE) +
  scale_fill_manual(values = c(vlow = "red", low = "yellow", mod = "steelblue3", high = "limegreen")) + theme_bw()
plt05


#NMA in 2000
##dataset preparation
def00 <- subset(def, study_year < 2000)
defpw00 <- pairwise(treat = t, n, event = r, data = def00, studlab = id, sm = "OR")
dac00 <- subset(dac, study_year < 2000)
dacpw00 <- pairwise(treat = t, n, event = r, data = dac00, studlab = id, sm = "OR")
##nma for efficacy: random-effect model
netef00 <- netmeta(defpw00, comb.fixed = FALSE, comb.random = TRUE, reference.group = "citalopram")
##nma for acceptability: random-effect model
netac00 <- netmeta(dacpw00, comb.fixed = FALSE, comb.random = TRUE, reference.group = "citalopram")

##prepare the data frame for the 2D plot
defor00 <- data.frame(trts = netef00$trts,
                      ORef = exp(netef00$TE.random[, netef00$reference.group]),
                      loweref = exp(netef00$lower.random[, netef00$reference.group]),
                      upperef = exp(netef00$upper.random[, netef00$reference.group]),
                      sample_ef = netef00$n.trts)
dacor00 <- data.frame(trts = netac00$trts,
                      ORac = exp(netac00$TE.random[, netac00$reference.group]),
                      lowerac = exp(netac00$lower.random[, netac00$reference.group]),
                      upperac = exp(netac00$upper.random[, netac00$reference.group]),
                      sample_ac = netac00$n.trts)
##merge dataframe
dator00 <- merge(defor00, dacor00, by = "trts", all = TRUE)
dator00$trts <- substr(dator00$trts, 0, 4)
##add confidence information
dcon00 <- data.frame(trts = c("amit", "bupr", "cita", "clom", "fluo", "fluv", 
                              "miln", "mirt", "nefa", "paro", "rebo", "sert", "traz", "venl"),
                     vlow = c(0.4231, 0.3846, 0.1154, 0.4231, 0.1154, 0.3846, 0.3846, 0.1923, 0.3846, 0.1154, 1, 0.1154,
                              0.5, 0.1538),
                     low = c(0.5385, 0.6154, 0.4615, 0.5769, 0.5384, 0.5769, 0.6154, 0.4615, 0.5769, 0.5769, 0, 0.6154,
                             0.5, 0.5385), 
                     mod = c(0.0385, 0.0385, 0.3846, 0, 0.3077, 0.0385, 0, 0.1923, 0.0385, 0.2692, 0, 0.2308,
                             0, 0.3077),
                     high = c(0, 0, 0.0385, 0, 0.0385, 0, 0, 0.1538, 0, 0.0385, 0, 0.0385,
                              0, 0),
                     radius = 1/(5*netef00$seTE.random[, netef00$reference.group]))
dcon00["citalopram", "radius"] = 1
dator00 <- merge(dator00, dcon00, by = "trts", all = TRUE)

##draw plot
##dots -> pie charts, size reflects SE, log scale
plt00 <- ggplot(dator00, aes(x = ORef, y = ORac, label = trts)) + labs(x = "OR of efficacy", y = "OR of acceptability") +
  scale_y_continuous(trans = reverselog_trans(10), limits = c(2.2, 0.2), breaks = seq(2.2, 0.2, -0.2)) + 
  scale_x_continuous(limits = c(0.6, 3.6), breaks = seq(0.6, 3.6, 0.2), trans = "log10") +
  annotation_logticks() +
  geom_text_repel(data = subset(dator00, ORac <= 1.2), aes(fontface = ifelse(subset(dator00, ORac <= 1.2)$trts %in% c("nefa", "mirt"), "bold", "plain")),
                  col = ifelse(subset(dator00, ORac <= 1.2)$trts %in% c("nefa", "mirt"), "hotpink3", "gray29"), 
                  cex = 3, point.padding = 0.3, nudge_y = 0.05, force = 3, segment.size = 0.2, segment.color = "grey50") +
  geom_text_repel(data = subset(dator00, ORac > 1.2), aes(fontface = ifelse(subset(dator00, ORac > 1.2)$trts %in% c("nefa", "mirt"), "bold", "plain")),
                  col = ifelse(subset(dator00, ORac > 1.2)$trts %in% c("nefa", "mirt"), "hotpink3", "gray29"),
                  cex = 3, point.padding = 0.3, nudge_y = -0.05, force = 3, segment.size = 0.2, segment.color = "grey50") +
  geom_hline(yintercept = 1, linetype = "dotted", col = "lightblue4") + geom_vline(xintercept = 1, linetype = "dotted", col = "lightblue4") +
  geom_scatterpie(data = dator00, aes(x = ORef, y = ORac, r = 0.012*radius), cols = colnames(dator00[, 10:13]), alpha = 0.9, color = NA, show.legend = FALSE) +
  scale_fill_manual(values = c(vlow = "red", low = "yellow", mod = "steelblue3", high = "limegreen")) + theme_bw()
plt00



#NMA in 1995
##dataset preparation
def95 <- subset(def, study_year < 1995)
defpw95 <- pairwise(treat = t, n, event = r, data = def95, studlab = id, sm = "OR")
dac95 <- subset(dac, study_year < 1995)
dacpw95 <- pairwise(treat = t, n, event = r, data = dac95, studlab = id, sm = "OR")
##nma for efficacy: random-effect model
netef95 <- netmeta(defpw95, comb.fixed = FALSE, comb.random = TRUE, reference.group = "citalopram")
##nma for acceptability: random-effect model
netac95 <- netmeta(dacpw95, comb.fixed = FALSE, comb.random = TRUE, reference.group = "citalopram")

##prepare the data frame for the 2D plot
defor95 <- data.frame(trts = netef95$trts,
                      ORef = exp(netef95$TE.random[, netef95$reference.group]),
                      loweref = exp(netef95$lower.random[, netef95$reference.group]),
                      upperef = exp(netef95$upper.random[, netef95$reference.group]),
                      sample_ef = netef95$n.trts)
dacor95 <- data.frame(trts = netac95$trts,
                      ORac = exp(netac95$TE.random[, netac95$reference.group]),
                      lowerac = exp(netac95$lower.random[, netac95$reference.group]),
                      upperac = exp(netac95$upper.random[, netac95$reference.group]),
                      sample_ac = netac95$n.trts)
##merge dataframe
dator95 <- merge(defor95, dacor95, by = "trts", all = TRUE)
dator95$trts <- substr(dator95$trts, 0, 4)
##add confidence information
dcon95 <- data.frame(trts = c("amit", "bupr", "cita", "clom", "fluo", "fluv", 
                              "miln", "paro", "rebo", "sert", "traz", "venl"),
                     vlow = c(0.5909, 0.5455, 0.2273, 0.4545, 0.2273, 0.5, 0.5909, 0.3636, 1, 0.1818, 0.7273, 0.3182),
                     low = c(0.4091, 0.4091, 0.4545, 0.5455, 0.4545, 0.5, 0.4091, 0.4091, 0, 0.5, 0.2727, 0.5), 
                     mod = c(0, 0.0455, 0.3182, 0, 0.3182, 0, 0, 0.2273, 0, 0.3182, 0, 0.1818),
                     high = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                     radius = 1/(5*netef95$seTE.random[, netef95$reference.group]))
dcon95["citalopram", "radius"] = 1
dator95 <- merge(dator95, dcon95, by = "trts", all = TRUE)

##draw plot
##dots -> pie charts, size reflects SE, log scale
plt95 <- ggplot(dator95, aes(x = ORef, y = ORac, label = trts)) + labs(x = "OR of efficacy", y = "OR of acceptability") +
  scale_y_continuous(trans = reverselog_trans(10), limits = c(2.2, 0.2), breaks = seq(2.2, 0.2, -0.2)) + 
  scale_x_continuous(limits = c(0.6, 3.6), breaks = seq(0.6, 3.6, 0.2), trans = "log10") +
  annotation_logticks() +
  geom_text_repel(data = subset(dator95, ORac <= 0.9), aes(fontface = ifelse(subset(dator95, ORac <= 0.9)$trts %in% c("fluv", "rebo", "sert", "bupr", "venl"), "bold", "plain")),
                  col = ifelse(subset(dator95, ORac <= 0.9)$trts %in% c("fluv", "rebo", "sert", "bupr", "venl"), "hotpink3", "gray29"), 
                  cex = 3, point.padding = 0.3, nudge_y = 0.05, force = 3, segment.size = 0.2, segment.color = "grey50") +
  geom_text_repel(data = subset(dator95, ORac > 0.9), aes(fontface = ifelse(subset(dator95, ORac > 0.9)$trts %in% c("fluv", "rebo", "sert", "bupr", "venl"), "bold", "plain")),
                  col = ifelse(subset(dator95, ORac > 0.9)$trts %in% c("fluv", "rebo", "sert", "bupr", "venl"), "hotpink3", "gray29"),
                  cex = 3, point.padding = 0.3, nudge_y = -0.05, force = 3, segment.size = 0.2, segment.color = "grey50") +
  geom_hline(yintercept = 1, linetype = "dotted", col = "lightblue4") + geom_vline(xintercept = 1, linetype = "dotted", col = "lightblue4") +
  geom_scatterpie(data = dator95, aes(x = ORef, y = ORac, r = 0.012*radius), cols = colnames(dator95[, 10:13]), alpha = 0.9, color = NA, show.legend = FALSE) +
  scale_fill_manual(values = c(vlow = "red", low = "yellow", mod = "steelblue3", high = "limegreen")) + theme_bw()
plt95



#NMA in 1990
##dataset preparation
def90 <- subset(def, study_year < 1990)
defpw90 <- pairwise(treat = t, n, event = r, data = def90, studlab = id, sm = "OR")
dac90 <- subset(dac, study_year < 1990)
dacpw90 <- pairwise(treat = t, n, event = r, data = dac90, studlab = id, sm = "OR")
##nma for efficacy: random-effect model
netef90 <- netmeta(defpw90, comb.fixed = FALSE, comb.random = TRUE, reference.group = "citalopram")
##nma for acceptability: random-effect model
netac90 <- netmeta(dacpw90, comb.fixed = FALSE, comb.random = TRUE, reference.group = "citalopram")

##prepare the data frame for the 2D plot
defor90 <- data.frame(trts = netef90$trts,
                      ORef = exp(netef90$TE.random[, netef90$reference.group]),
                      loweref = exp(netef90$lower.random[, netef90$reference.group]),
                      upperef = exp(netef90$upper.random[, netef90$reference.group]),
                      sample_ef = netef90$n.trts)
dacor90 <- data.frame(trts = netac90$trts,
                      ORac = exp(netac90$TE.random[, netac90$reference.group]),
                      lowerac = exp(netac90$lower.random[, netac90$reference.group]),
                      upperac = exp(netac90$upper.random[, netac90$reference.group]),
                      sample_ac = netac90$n.trts)
##merge dataframe
dator90 <- merge(defor90, dacor90, by = "trts", all = TRUE)
dator90$trts <- substr(dator90$trts, 0, 4)
dator90 <- na.omit(dator90)

##add confidence information
dcon90 <- data.frame(trts = c("amit", "cita", "clom", "fluo", "fluv", "miln", "paro", "traz"),
                     vlow = c(0.5714, 0.2143, 0.4286, 0.3572, 0.7143, 0.9286, 0.5, 0.7857),
                     low = c(0.4286, 0.5714, 0.5714, 0.4286, 0.2857, 0.0714, 0.3571, 0.2143), 
                     mod = c(0, 0.2143, 0, 0.2143, 0, 0, 0.1429, 0),
                     high = c(0, 0, 0, 0, 0, 0, 0, 0),
                     radius = 1/(5*netef90$seTE.random[, netef90$reference.group]))
dcon90["citalopram", "radius"] = 1
dator90 <- merge(dator90, dcon90, by = "trts", all = TRUE)
dator90 <- na.omit(dator90)

##draw plot
##dots -> pie charts, size reflects SE, log scale
plt90 <- ggplot(dator90, aes(x = ORef, y = ORac, label = trts)) + labs(x = "OR of efficacy", y = "OR of acceptability") +
  scale_y_continuous(trans = reverselog_trans(10), limits = c(2.2, 0.2), breaks = seq(2.2, 0.2, -0.2)) + 
  scale_x_continuous(limits = c(0.6, 3.6), breaks = seq(0.6, 3.6, 0.2), trans = "log10") +
  annotation_logticks() +
  geom_text_repel(data = subset(dator90, ORac <= 0.9), 
                  col = "gray29", 
                  cex = 3, point.padding = 0.3, nudge_y = 0.05, force = 3, segment.size = 0.2, segment.color = "grey50") +
  geom_text_repel(data = subset(dator90, ORac > 0.9), 
                  col = "gray29",
                  cex = 3, point.padding = 0.3, nudge_y = -0.05, force = 3, segment.size = 0.2, segment.color = "grey50") +
  geom_hline(yintercept = 1, linetype = "dotted", col = "lightblue4") + geom_vline(xintercept = 1, linetype = "dotted", col = "lightblue4") +
  geom_scatterpie(data = dator90, aes(x = ORef, y = ORac, r = 0.012*radius), cols = colnames(dator90[, 10:13]), alpha = 0.9, color = NA, show.legend = FALSE) +
  scale_fill_manual(values = c(vlow = "red", low = "yellow", mod = "steelblue3", high = "limegreen")) + theme_bw()
plt90
