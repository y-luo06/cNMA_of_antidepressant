library(netmeta)

#preparing full dataset: files can be found in the Data folder
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


#NMA in 2016
##nma for efficacy: random-effect model
netef16 <- netmeta(defpw, comb.fixed = FALSE, comb.random = TRUE)

##network plot for efficacy: bar thickness proportionate to number of studies, node size proportionate to sample size
netgraph(netef16, start ="circle", cex = 0.7, col = "black", plastic = FALSE, 
         points = TRUE, col.points = "darkgreen", cex.points =15*sqrt(n.trts/max(n.trts)), 
         thickness = "number.of.studies", lwd.max = 12, lwd.min = 1, multiarm = FALSE)

##forestplot for efficacy sort by OR compared with citalopram
forest(netef16, ref = "citalopram", pooled = "random", digits = 2, smlab = "Random effects model", 
       label.left = "Favors citalopram", label.right = "Favors the other", leftlabs = "Efficacy", 
       drop = TRUE, sortvar = -TE)

##nma for acceptability: random-effect model
netac16 <- netmeta(dacpw, comb.fixed = FALSE, comb.random = TRUE)

##network plot for acceptability:
netgraph(netac16, start ="circle", cex = 0.7, col = "black", plastic = FALSE, 
         points = TRUE, col.points = "darkred", cex.points =15*sqrt(n.trts/max(n.trts)), 
         thickness = "number.of.studies", lwd.max = 12, lwd.min = 1, multiarm = FALSE)

##forestplot for accetability
forest(netac16, ref = "citalopram", pooled = "random", digits = 2, smlab = "Random effects model", 
       label.left = "Favors the other", label.right = "Favors citalopram", leftlabs = "Acceptability", 
       drop = TRUE, sortvar = TE)

##league table combining efficacy and acceptability
league16 <- netleague(netef16, netac16, backtransf = TRUE, direct = FALSE, comb.fixed = FALSE, digits =2, bracket = "(", separator = " to ")
write.table(league16$random, file = "league table-2016.csv", row.names = FALSE, col.names = FALSE, sep = ",")


#NMA in 2010
##dataset preparation
def10 <- subset(def, study_year < 2010)
defpw10 <- pairwise(treat = t, n, event = r, data = def10, studlab = id, sm = "OR")
dac10 <- subset(dac, study_year < 2010)
dacpw10 <- pairwise(treat = t, n, event = r, data = dac10, studlab = id, sm = "OR")

##nma for efficacy: random-effect model
netef10 <- netmeta(defpw10, comb.fixed = FALSE, comb.random = TRUE)

##network plot for efficacy: bar thickness proportionate to number of studies, node size proportionate to sample size
netgraph(netef10, start ="circle", cex = 0.7, col = "black", plastic = FALSE, 
         points = TRUE, col.points = "darkgreen", cex.points =15*sqrt(n.trts/max(n.trts)), 
         thickness = "number.of.studies", lwd.max = 12, lwd.min = 1, multiarm = FALSE)

##forestplot for efficacy sort by OR compared with citalopram
forest(netef10, ref = "citalopram", pooled = "random", digits = 2, smlab = "Random effects model", 
       label.left = "Favors citalopram", label.right = "Favors the other", leftlabs = "Efficacy", 
       drop = TRUE, sortvar = -TE)

##nma for acceptability: random-effect model
netac10 <- netmeta(dacpw10, comb.fixed = FALSE, comb.random = TRUE)

##network plot for acceptability:
netgraph(netac10, start ="circle", cex = 0.7, col = "black", plastic = FALSE, 
         points = TRUE, col.points = "darkred", cex.points =15*sqrt(n.trts/max(n.trts)), 
         thickness = "number.of.studies", lwd.max = 12, lwd.min = 1, multiarm = FALSE)

##forestplot for accetability
forest(netac10, ref = "citalopram", pooled = "random", digits = 2, smlab = "Random effects model", 
       label.left = "Favors the other", label.right = "Favors citalopram", leftlabs = "Acceptability", 
       drop = TRUE, sortvar = TE)

##league table combining efficacy and acceptability
league10 <- netleague(netef10, netac10, backtransf = TRUE, direct = FALSE, comb.fixed = FALSE, digits =2, bracket = "(", separator = " to ")
write.table(league10$random, file = "league table-2010.csv", row.names = FALSE, col.names = FALSE, sep = ",")


#NMA in 2005
##dataset preparation
def05 <- subset(def, study_year < 2005)
defpw05 <- pairwise(treat = t, n, event = r, data = def05, studlab = id, sm = "OR")
dac05 <- subset(dac, study_year < 2005)
dacpw05 <- pairwise(treat = t, n, event = r, data = dac05, studlab = id, sm = "OR")

##nma for efficacy: random-effect model
netef05 <- netmeta(defpw05, comb.fixed = FALSE, comb.random = TRUE)

##network plot for efficacy: bar thickness proportionate to number of studies, node size proportionate to sample size
netgraph(netef05, start ="circle", cex = 0.7, col = "black", plastic = FALSE, 
         points = TRUE, col.points = "darkgreen", cex.points =15*sqrt(n.trts/max(n.trts)), 
         thickness = "number.of.studies", lwd.max = 12, lwd.min = 1, multiarm = FALSE)

##forestplot for efficacy sort by OR compared with citalopram
forest(netef05, ref = "citalopram", pooled = "random", digits = 2, smlab = "Random effects model", 
       label.left = "Favors citalopram", label.right = "Favors the other", leftlabs = "Efficacy", 
       drop = TRUE, sortvar = -TE)

##nma for acceptability: random-effect model
netac05 <- netmeta(dacpw05, comb.fixed = FALSE, comb.random = TRUE)

##network plot for acceptability:
netgraph(netac05, start ="circle", cex = 0.7, col = "black", plastic = FALSE, 
         points = TRUE, col.points = "darkred", cex.points =15*sqrt(n.trts/max(n.trts)), 
         thickness = "number.of.studies", lwd.max = 12, lwd.min = 1, multiarm = FALSE)

##forestplot for accetability
forest(netac05, ref = "citalopram", pooled = "random", digits = 2, smlab = "Random effects model", 
       label.left = "Favors the other", label.right = "Favors citalopram", leftlabs = "Acceptability", 
       drop = TRUE, sortvar = TE)

##league table combining efficacy and acceptability
league05 <- netleague(netef05, netac05, backtransf = TRUE, direct = FALSE, comb.fixed = FALSE, digits =2, bracket = "(", separator = " to ")
write.table(league05$random, file = "league table-2010.csv", row.names = FALSE, col.names = FALSE, sep = ",")


#NMA in 2000
##dataset preparation
def00 <- subset(def, study_year < 2000)
defpw00 <- pairwise(treat = t, n, event = r, data = def00, studlab = id, sm = "OR")
dac00 <- subset(dac, study_year < 2000)
dacpw00 <- pairwise(treat = t, n, event = r, data = dac00, studlab = id, sm = "OR")

##nma for efficacy: random-effect model
netef00 <- netmeta(defpw00, comb.fixed = FALSE, comb.random = TRUE)

##network plot for efficacy: bar thickness proportionate to number of studies, node size proportionate to sample size
netgraph(netef00, start ="circle", cex = 0.7, col = "black", plastic = FALSE, 
         points = TRUE, col.points = "darkgreen", cex.points =15*sqrt(n.trts/max(n.trts)), 
         thickness = "number.of.studies", lwd.max = 12, lwd.min = 1, multiarm = FALSE)

##forestplot for efficacy sort by OR compared with citalopram
forest(netef00, ref = "citalopram", pooled = "random", digits = 2, smlab = "Random effects model", 
       label.left = "Favors citalopram", label.right = "Favors the other", leftlabs = "Efficacy", 
       drop = TRUE, sortvar = -TE)

##nma for acceptability: random-effect model
netac00 <- netmeta(dacpw00, comb.fixed = FALSE, comb.random = TRUE)

##network plot for acceptability:
netgraph(netac00, start ="circle", cex = 0.7, col = "black", plastic = FALSE, 
         points = TRUE, col.points = "darkred", cex.points =15*sqrt(n.trts/max(n.trts)), 
         thickness = "number.of.studies", lwd.max = 12, lwd.min = 1, multiarm = FALSE)

##forestplot for accetability
forest(netac00, ref = "citalopram", pooled = "random", digits = 2, smlab = "Random effects model", 
       label.left = "Favors the other", label.right = "Favors citalopram", leftlabs = "Acceptability", 
       drop = TRUE, sortvar = TE)

##league table combining efficacy and acceptability
league00 <- netleague(netef00, netac00, backtransf = TRUE, direct = FALSE, comb.fixed = FALSE, digits =2, bracket = "(", separator = " to ")
write.table(league00$random, file = "league table-2010.csv", row.names = FALSE, col.names = FALSE, sep = ",")


#NMA in 1995
##dataset preparation
def95 <- subset(def, study_year < 1995)
defpw95 <- pairwise(treat = t, n, event = r, data = def95, studlab = id, sm = "OR")
dac95 <- subset(dac, study_year < 1995)
dacpw95 <- pairwise(treat = t, n, event = r, data = dac95, studlab = id, sm = "OR")

##nma for efficacy: random-effect model
netef95 <- netmeta(defpw95, comb.fixed = FALSE, comb.random = TRUE)

##network plot for efficacy: bar thickness proportionate to number of studies, node size proportionate to sample size
netgraph(netef95, start ="circle", cex = 0.7, col = "black", plastic = FALSE, 
         points = TRUE, col.points = "darkgreen", cex.points =15*sqrt(n.trts/max(n.trts)), 
         thickness = "number.of.studies", lwd.max = 12, lwd.min = 1, multiarm = FALSE)

##forestplot for efficacy sort by OR compared with citalopram
forest(netef95, ref = "citalopram", pooled = "random", digits = 2, smlab = "Random effects model", 
       label.left = "Favors citalopram", label.right = "Favors the other", leftlabs = "Efficacy", 
       drop = TRUE, sortvar = -TE)

##nma for acceptability: random-effect model
netac95 <- netmeta(dacpw95, comb.fixed = FALSE, comb.random = TRUE)

##network plot for acceptability:
netgraph(netac95, start ="circle", cex = 0.7, col = "black", plastic = FALSE, 
         points = TRUE, col.points = "darkred", cex.points =15*sqrt(n.trts/max(n.trts)), 
         thickness = "number.of.studies", lwd.max = 12, lwd.min = 1, multiarm = FALSE)

##forestplot for accetability
forest(netac95, ref = "citalopram", pooled = "random", digits = 2, smlab = "Random effects model", 
       label.left = "Favors the other", label.right = "Favors citalopram", leftlabs = "Acceptability", 
       drop = TRUE, sortvar = TE)

##league table combining efficacy and acceptability
league95 <- netleague(netef95, netac95, backtransf = TRUE, direct = FALSE, comb.fixed = FALSE, digits =2, bracket = "(", separator = " to ")
write.table(league95$random, file = "league table-2010.csv", row.names = FALSE, col.names = FALSE, sep = ",")


#NMA in 1990
##dataset preparation
def90 <- subset(def, study_year < 1990)
defpw90 <- pairwise(treat = t, n, event = r, data = def90, studlab = id, sm = "OR")
dac90 <- subset(dac, study_year < 1990)
dacpw90 <- pairwise(treat = t, n, event = r, data = dac90, studlab = id, sm = "OR")

##nma for efficacy: random-effect model
netef90 <- netmeta(defpw90, comb.fixed = FALSE, comb.random = TRUE)

##network plot for efficacy: bar thickness proportionate to number of studies, node size proportionate to sample size
netgraph(netef90, start ="circle", cex = 0.7, col = "black", plastic = FALSE, 
         points = TRUE, col.points = "darkgreen", cex.points =15*sqrt(n.trts/max(n.trts)), 
         thickness = "number.of.studies", lwd.max = 12, lwd.min = 1, multiarm = FALSE)

##forestplot for efficacy sort by OR compared with citalopram
forest(netef90, ref = "citalopram", pooled = "random", digits = 2, smlab = "Random effects model", 
       label.left = "Favors citalopram", label.right = "Favors the other", leftlabs = "Efficacy", 
       drop = TRUE, sortvar = -TE)

##nma for acceptability: random-effect model
netac90 <- netmeta(dacpw90, comb.fixed = FALSE, comb.random = TRUE)

##network plot for acceptability:
netgraph(netac90, start ="circle", cex = 0.7, col = "black", plastic = FALSE, 
         points = TRUE, col.points = "darkred", cex.points =15*sqrt(n.trts/max(n.trts)), 
         thickness = "number.of.studies", lwd.max = 12, lwd.min = 1, multiarm = FALSE)

##forestplot for accetability
forest(netac90, ref = "citalopram", pooled = "random", digits = 2, smlab = "Random effects model", 
       label.left = "Favors the other", label.right = "Favors citalopram", leftlabs = "Acceptability", 
       drop = TRUE, sortvar = TE)

##league table combining efficacy and acceptability: not able to be generated as the number of arms are different, 
##but we can generate the league table for efficacy and acceptability respectively
##league table for efficacy
leagueef90 <- netleague(netef90, backtransf = TRUE, direct = FALSE, comb.fixed = FALSE, digits =2, bracket = "(", separator = " to ")
write.table(leagueef90$random, file = "league table-present.csv", row.names = FALSE, col.names = FALSE, sep = ",")
##league table for acceptability
leagueac90 <- netleague(netac90, backtransf = TRUE, direct = FALSE, comb.fixed = FALSE, digits =2, bracket = "(", separator = " to ")
write.table(leagueac90$random, file = "league table-present.csv", row.names = FALSE, col.names = FALSE, sep = ",")

