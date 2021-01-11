# Moss Phylogeny Project Script - Phylogenetic Signal Analysis
# IR Moodie 2021

# ---- SETUP ENV ----

rm(list=ls()) # clear environment

library(treeio) # importing tree data and handling
library(ggtree) # plotting trees using ggplot2
library(phylobase)
library(phylosignal)
library(tidyverse) # data handling and plotting

# ---- DATA ----

mossdata <- read_csv("mossdataset.csv") %>%
  mutate(species = str_replace(species, "Tortula truncata", "Pottia truncata")) %>%
  select(# match species names in GBIF data with tree species names
mosstreeBI <- read.nexus("MrBayesTree.tre")
mosstreeML <- read.nexus("MLTree2.nexus")

labels <- mosstreeBI$tip.label
labels <- str_replace(labels, "_", " ")
mosstreeBI$tip.label <- labels # removes underscores from names

# ---- ANALYSIS ----


order <- data.frame(mosstreeBI$tip.label)
order <- order %>%
  rename(label = mosstreeBI.tip.label)

mossdata <- left_join(order, mossdata, by = "label") %>%
  select(spore_size)

p4d <- phylo4d(mosstreeBI, mossdata)

barplot.phylo4d(p4d, tree.type = "phylo", tree.ladderize = TRUE)

phyloSignal(p4d = p4d, method = "all")

phylosim <- phyloSim(tree = mosstreeBI, method = "all", nsim = 100, reps = 99)

plot(phylosim, stacked.methods = FALSE, quantiles = c(0.05, 0.95))

plot.phylosim(phylosim, what = "pval", stacked.methods = TRUE)

spore.crlg <- phyloCorrelogram(p4d, trait = "spore_size")

lipaMoran(p4d)

