# Moss Phylogeny Project Script - Plotting
# IR Moodie 2021

# ---- SETUP ENV ----

rm(list=ls()) # clear environment

library(treeio) # importing tree data and handling
library(ggtree) # plotting trees using ggplot2
library(ggstance) # plotting horizontal ggplot2
library(gridExtra) # multiplotting
library(ggforce) # multiplotting
library(gtable) # multiplotting
library(tidyverse) # data handling and plotting

# ---- DATA ----

mossdata <- read_csv("mossdataset.csv") %>%
  mutate(species = str_replace(species, "Tortula truncata", "Pottia truncata")) # match species names in GBIF data with tree species names
mosstreeBI <- read.nexus("MrBayesTree.tre")
mosstreeML <- read.nexus("MLTree2.nexus")

# ---- SPORE SIZE ~ RANGE SIZE ----

sporeRangePlot <- ggplot(data = mossdata, aes(x = range/1000000, y = spore_size)) + # create ggplot with range in millions of km2 against spore size
  geom_smooth(method = "lm", colour = "black") + # plot linear model
  geom_point(size = 4, alpha = 0.7) + # plot points
  xlab(label = c(expression("Range size in millions of" ~km^2)))+ # set axis name to format correctly
  ylab(expression(paste(
    "Spore diameter (",
    mu, m, ")", sep=""))) +
  theme(legend.position = "none") + # remove legend
  theme_classic() # set theme to classic#

sporeRangePlot

# ---- TREE (BI) ----

labels <- mosstreeBI$tip.label
labels <- str_replace(labels, "_", " ")
mosstreeBI$tip.label <- labels # removes underscores from names

treeBI <- ggtree(mosstreeBI, ladderize = TRUE, right = FALSE) # create tree

d <- treeBI$data # for rounding and cut off of values
d <- d[!d$isTip,]
d$label <- as.numeric(d$label)
d <- d[d$label > .5,] # cut off value
d$label <- round(d$label, digits = 2) # round to 2 digits eg 0.95

treeBI <- treeBI + 
  geom_text(data=d, aes(label=label), size = 6) + # add PP values
  geom_tiplab(linetype = "dotted", align = FALSE, fontface = "italic", size = 8) + # add taxa names
  geom_treescale(y = -2, width = 0.02) + # add scale bar
  xlim(0, 0.27) # set xlims so species names are not cut off

treeBI # needs editing elsewhere

ggsave(filename = "treeBI.svg", # export as svg for editing (boxy.svg online works well)
       plot = treeBI,
       dpi = "retina",
       height = 12,
       width = 16)

ggsave(filename = "treeBI.png", # export as png if desired
       plot = treeBI,
       dpi = "retina",
       height = 12,
       width = 16)

# ---- TREE (ML) ----

orderoftaxa <- ape::rotateConstr(mosstreeML, rev(get_taxa_name(treeBI))) # gets order of taxa from BI tree to try and plot them in a comparable way
# this step I don't think has helped much

mllabels <- orderoftaxa$tip.label
mllabels <- str_replace(mllabels, "_", " ")
orderoftaxa$tip.label <- mllabels

treeML <- ggtree(orderoftaxa, ladderize = FALSE)

d <- treeML$data
d <- d[!d$isTip,]
d$label <- as.numeric(d$label)
#d <- d[d$label > .5,]
d$label <- round(d$label, digits = 2)

treeML <- treeML + 
  geom_text(data=d, aes(label=label), size = 6) +
  geom_tiplab(linetype = "dotted", align = FALSE, fontface = "italic", size = 8) +
  geom_treescale(y = -2, width = 0.02) +
  # geom_text(aes(label=node), size = 6) +
  xlim(0, 0.27)

treeML

ggsave(filename = "MLTree.svg",
       plot = treeML,
       dpi = "retina",
       height = 12,
       width = 16)

ggsave(filename = "MLTree.png",
       plot = treeML,
       dpi = "retina",
       height = 12,
       width = 16)

# ---- TREE CHARACTER MAPPING

ggmosstree <- ggtree(mosstreeBI, branch.length = "none") 

ggmosstree <- ggmosstree %<+% mossdata #ggtree object + attach spore and range dataset

ggmosstree <- ggmosstree + geom_tippoint(aes(size = spore_size)) + # put spore size on tips as circles
  #geom_tiplab(offset = 0.6) + # use for species names
  geom_tiplab(aes(label = have_data), hjust = 0.5) + # mark tips with no data with an X
  theme(legend.position = "bottom") + # legend at bottom
  labs(size = expression(paste(
    "Spore diameter (",
    mu, m, ")", sep="")))

order<-c("Sphagnum girgensohnii",
         "Sphagnum rubellum",
         "Sphagnum fallax",
         "Sphagnum palustre",
         "Encalypta streptocarpa",
         "Encalypta longicollis",
         "Encalypta alpina",
         "Encalypta rhaptocarpa",
         "Pleurophascum grandiglobum",
         "Serpotortella chenagonii",
         "Mittenia plumula",
         "Leucoloma serrulatum",
         "Syrrhopodon gardneri",
         "Calymperes erosum",
         "Calymperes afzelii",
         "Chorisodontium aciphyllum",
         "Dicranum flagellare",
         "Orthodicranum montanum",
         "Paraleucobryum longifolium",
         "Dicranum scoparium",
         "Dicranella cerviculata",
         "Dicranella heteromalla",
         "Cnestrum schisti",
         "Schistostega pennata",
         "Syntrichia ruralis",
         "Pleurochaete squarrosa",
         "Trichostomum tenuirostre",
         "Weissia controversa",
         "Tortula muralis",
         "Pottia truncata",
         "Cinclidotus aquaticus",
         "Cinclidotus riparius")

order <- data.frame(order)

order <- rename(order, species = order)

order <- left_join(order, mossdata, by = "species")

ggmosstreerange <- facet_plot(ggmosstree, panel = "Geographic range", data = order, # plot range data
                              geom = geom_barh, # horizontal geom_bar object from ggstance (integrated well with ggtree)
                              mapping = aes(x = order$range), # set aes (was having weird issue hence the $)
                              stat="identity") # stat identity so not a histogram 
#  + xlim_tree(20) # use if want to see species names in full

ggmosstreerange

#ggsave(filename = "TreeRange.png",
#       plot = ggmosstreerange,
#       dpi = "retina",
#       height = 6,
#       width = 8)

treesporerange <- grid.arrange(ggmosstreerange,
                               sporeRangePlot,
                               ncol = 2,
                               nrow = 1)

ggsave(filename = "TreeRangeSporePlotFull.png",
       plot = treesporerange,
       dpi = "retina",
       height = 6,
       width = 12)
