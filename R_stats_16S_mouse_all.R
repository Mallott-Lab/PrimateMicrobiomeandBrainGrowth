#Set up environment----

setwd("/Users/elizabethmallott/Dropbox/Projects/Gut_microbiome/mouse_inoculation/16S/All")

#Import data----
unweighted = as.dist(read.table("unweighted-distance-matrix-noinfant.tsv", header = T))
weighted = as.dist(read.table("weighted-distance-matrix-noinfant.tsv", header = T))
metadata = read.csv("mapping_all_r_update072722_noinfant.csv", header=T)

#Permanovas----
library(vegan)
library(pairwiseAdonis)

adonis2(unweighted ~ Stock + Timepoint, data=metadata, 
        by = "margin", permutations = 5000)

pairwise.adonis(unweighted, factors = metadata$Timepoint, perm = 5000, 
                p.adjust.m='holm')
pairwise.adonis(unweighted, factors = metadata$Stock, perm = 5000, 
                p.adjust.m='holm')

adonis2(weighted ~ Stock + Timepoint, data=metadata, 
        by = "margin", permutations = 5000)

pairwise.adonis(weighted, factors = metadata$Timepoint, perm = 5000, 
                p.adjust.m='holm')
pairwise.adonis(weighted, factors = metadata$Stock, perm = 5000, 
                p.adjust.m='holm')

#NMDS plots----
library(ggplot2)
library(vegan)

mds_otus_weighted<-metaMDS(weighted, k=2, trymax=500)
mds_otus_weighted_points<-mds_otus_weighted$points
mds_otus_weighted_points2<-merge(x=mds_otus_weighted_points, y = metadata, 
                                 by.x = "row.names", by.y = "SampleID")
tiff(file="nmds_plot_weighted_noinfant.tif", res=300, width=8, height=6, units="in")
nmds1v2 <- ggplot(mds_otus_weighted_points2, aes(x = MDS1, y = MDS2, 
                                                 color = Stock, shape = Timepoint)) +
  geom_point(size=3) + scale_color_brewer(palette = 'Set1') +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'), 
        legend.key=element_blank()) + 
  theme(axis.title.x=element_text(size=rel(2)), 
        axis.title.y=element_text(size=rel(2)),
        plot.title = element_text(size=rel(3)),
        legend.title = element_text(size=rel(2)),
        legend.text = element_text(size = rel(1.8))) + 
  ggtitle("Weighted UniFrac") 
nmds1v2
dev.off()

mds_otus_unweighted<-metaMDS(unweighted, k=2, trymax=500)
mds_otus_unweighted_points<-mds_otus_unweighted$points
mds_otus_unweighted_points2<-merge(x=mds_otus_unweighted_points, y = metadata, 
                                   by.x = "row.names", by.y = "SampleID")
tiff(file="nmds_plot_unweighted_noinfant.tif", res=300, width=8, height=6, units="in")
nmds1v2 <- ggplot(mds_otus_unweighted_points2, aes(x = MDS1, y = MDS2, 
                                                      color = Stock, shape = Timepoint)) +
  geom_point(size=3) + scale_color_brewer(palette = 'Set1') +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'), 
        legend.key=element_blank()) + 
  theme(axis.title.x=element_text(size=rel(2)), 
        axis.title.y=element_text(size=rel(2)),
        plot.title = element_text(size=rel(3)),
        legend.title = element_text(size=rel(2)),
        legend.text = element_text(size = rel(1.8))) + 
  ggtitle("Unweighted UniFrac") 
nmds1v2
dev.off()

#Alpha diversity ----

faith = read.table("pd-diversity.tsv", header=T)
breakaway = read.table("breakaway-richness.tsv", header = T)
shannon = read.table("shannon-diversity.tsv", header = T)
metadata = read.csv("mapping_all_r_update072722_noinfant.csv", header=T)

library(tidyverse)
library(nlme)
library(multcomp)
library(car)

alpha = inner_join(metadata, faith, by = "SampleID") %>% 
  inner_join(breakaway, by = "SampleID") %>% 
  inner_join(shannon, by = "SampleID")
alpha$Stock = as.factor(alpha$Stock)
alpha$Timepoint = as.factor(alpha$Timepoint)

f <- lme(fixed = faith_pd ~ Stock + Timepoint, data = alpha, 
         random = ~1|Individual)
summary(f)
Anova(f)
summary(glht(f,linfct=mcp(Stock="Tukey")))
summary(glht(f,linfct=mcp(Timepoint="Tukey")))

ft = lme(fixed = faith_pd ~ Timepoint, data = alpha, random = ~1|Stock)
summary(ft)
anova(ft)
summary(glht(ft,linfct=mcp(Timepoint="Tukey")))

b <- lme(fixed = estimate ~ Stock + Timepoint, data = alpha, 
         random = ~1|Individual)
summary(b)
Anova(b)
summary(glht(b,linfct=mcp(Stock="Tukey")))
summary(glht(b,linfct=mcp(Timepoint="Tukey")))

bt = lme(fixed = estimate ~ Timepoint, data = alpha, random = ~1|Stock)
summary(bt)
anova(bt)
summary(glht(bt,linfct=mcp(Timepoint="Tukey")))

s <- lme(fixed = shannon ~ Stock + Timepoint, data = alpha, 
         random = ~1|Individual)
summary(s)
Anova(s)
summary(glht(s,linfct=mcp(Stock="Tukey")))
summary(glht(s,linfct=mcp(Timepoint="Tukey")))

st = lme(fixed = shannon ~ Timepoint, data = alpha, random = ~1|Stock)
summary(st)
anova(st)
summary(glht(st,linfct=mcp(Timepoint="Tukey")))


library(ggpubr)

first = filter(alpha, Timepoint == "first")
last = filter(alpha, Timepoint == "last")
stock = filter(alpha, Timepoint == "stock")

plot0 = ggboxplot(stock, x = "Stock", 
                  y = "shannon", color = "Stock", 
                  palette = "Set1", add = "jitter", 
                  add.params = list(fill = "white"), ylab = "Shannon Diversity") 
plot0 = ggpar(plot0, legend = "right", title = "Stock") + 
  rremove("xlab") + rremove("x.text") + 
  rremove("legend.title")

plot1 = ggboxplot(first, x = "Stock", 
                  y = "shannon", color = "Stock", 
                  palette = "Set1", add = "jitter", 
                  add.params = list(fill = "white"), ylab = "Shannon Diversity") 
plot1 = ggpar(plot1, legend = "right", title = "First time point") + 
  rremove("xlab") + rremove("x.text") + 
  rremove("legend.title")

plot2 = ggboxplot(last, x = "Stock", 
                  y = "shannon", color = "Stock", 
                  palette = "Set1", add = "jitter", 
                  add.params = list(fill = "white"), ylab = "Shannon Diversity") 
plot2 = ggpar(plot2, legend = "right", title = "Last time point") + 
  rremove("xlab") + rremove("x.text") + 
  rremove("legend.title")

tiff(file="shannon_all_noinfant.tif", res=150, width=20, height=4, units="in")
ggarrange(plot0, plot1, plot2, ncol = 3, common.legend = TRUE, legend = "right",
          nrow = 1, align = "h")
dev.off()

plot3a = ggboxplot(stock, x = "Stock", 
                  y = "estimate", color = "Stock", 
                  palette = "Set1", add = "jitter", 
                  add.params = list(fill = "white"), ylab = "Breakaway Richness") 
plot3a = ggpar(plot3a, legend = "right", title = "Stock") + 
  rremove("xlab") + rremove("x.text") + 
  rremove("legend.title")

plot3 = ggboxplot(first, x = "Stock", 
                  y = "estimate", color = "Stock", 
                  palette = "Set1", add = "jitter", 
                  add.params = list(fill = "white"), ylab = "Breakaway Richness") 
plot3 = ggpar(plot3, legend = "right", title = "First time point") + 
  rremove("xlab") + rremove("x.text") + 
  rremove("legend.title")

plot4 = ggboxplot(last, x = "Stock", 
                  y = "estimate", color = "Stock", 
                  palette = "Set1", add = "jitter", 
                  add.params = list(fill = "white"), ylab = "Breakaway Richness") 
plot4 = ggpar(plot4, legend = "right", title = "Last time point") + 
  rremove("xlab") + rremove("x.text") + 
  rremove("legend.title")

tiff(file="breakaway_all_noinfant.tif", res=150, width=20, height=4, units="in")
ggarrange(plot3a, plot3, plot4, ncol = 3, common.legend = TRUE, legend = "right",
          nrow = 1, align = "h")
dev.off()

plot5a = ggboxplot(stock, x = "Stock", 
                  y = "faith_pd", color = "Stock", 
                  palette = "Set1", add = "jitter", 
                  add.params = list(fill = "white"), ylab = "Faith's PD") 
plot5a = ggpar(plot5a, legend = "right", title = "Stock") + 
  rremove("xlab") + rremove("x.text") + 
  rremove("legend.title")

plot5 = ggboxplot(first, x = "Stock", 
                  y = "faith_pd", color = "Stock", 
                  palette = "Set1", add = "jitter", 
                  add.params = list(fill = "white"), ylab = "Faith's PD") 
plot5 = ggpar(plot5, legend = "right", title = "First time point") + 
  rremove("xlab") + rremove("x.text") + 
  rremove("legend.title")

plot6 = ggboxplot(last, x = "Stock", 
                  y = "faith_pd", color = "Stock", 
                  palette = "Set1", add = "jitter", 
                  add.params = list(fill = "white"), ylab = "Faith's PD") 
plot6 = ggpar(plot6, legend = "right", title = "Last time point") + 
  rremove("xlab") + rremove("x.text") + 
  rremove("legend.title")

tiff(file="faith_all_noinfant.tif", res=150, width=20, height=4, units="in")
ggarrange(plot5a, plot5, plot6, ncol = 3, common.legend = TRUE, legend = "right",
          nrow = 1, align = "h")
dev.off()

#Phyla relative abundance ----

library(glmmTMB)
library(multcomp)
library(car)
library(tidyverse)

metadata_phyla = read.csv("mapping_all_r_update072722_noinfant.csv", header = T)
seqs = read.csv("phyla_noinfant.csv", header = T)

phyla = inner_join(seqs, metadata_phyla, by = "SampleID")
phyla$Stock = as.factor(phyla$Stock)
phyla$Timepoint = as.factor(phyla$Timepoint)

acti = glmmTMB(Actinobacteria ~ Stock + Timepoint + (1|Individual), 
               data = phyla, family = nbinom2)
summary(acti)
Anova(acti)
summary(glht(acti,linfct=mcp(Stock="Tukey")))

actit = glmmTMB(Actinobacteria ~ Timepoint + (1|Stock), 
               data = phyla, family = nbinom2)
summary(actit)
Anova(actit)
summary(glht(actit,linfct=mcp(Timepoint="Tukey")))

bact = glmmTMB(Bacteroidetes ~ Stock + Timepoint + (1|Individual), 
               data = phyla, family = nbinom2)
summary(bact)
Anova(bact)
summary(glht(bact,linfct=mcp(Stock="Tukey")))

bactt = glmmTMB(Bacteroidetes ~ Timepoint + (1|Stock), 
               data = phyla, family = nbinom2)
summary(bactt)
Anova(bactt)
summary(glht(bactt,linfct=mcp(Timepoint="Tukey")))

firm = glmmTMB(Firmicutes ~ Stock + Timepoint + (1|Individual), 
               data = phyla, family = nbinom2)
summary(firm)
Anova(firm)
summary(glht(firm,linfct=mcp(Stock="Tukey")))

firmt = glmmTMB(Firmicutes ~ Timepoint + (1|Stock), 
               data = phyla, family = nbinom2)
summary(firmt)
Anova(firmt)
summary(glht(firmt,linfct=mcp(Timepoint="Tukey")))

fuso = glmmTMB(Fusobacteria ~ Stock + Timepoint + (1|Individual), 
               data = phyla, family = nbinom2)
summary(fuso)
Anova(fuso)
summary(glht(fuso,linfct=mcp(Stock="Tukey")))

fusot = glmmTMB(Fusobacteria ~ Timepoint + (1|Stock), 
               data = phyla, family = nbinom2)
summary(fusot)
Anova(fusot)
summary(glht(fusot,linfct=mcp(Timepoint="Tukey")))

prot = glmmTMB(Proteobacteria ~ Stock + Timepoint + (1|Individual), 
               data = phyla, family = nbinom2)
summary(prot)
Anova(prot)
summary(glht(prot,linfct=mcp(Stock="Tukey")))

prott = glmmTMB(Proteobacteria ~ Timepoint + (1|Stock), 
               data = phyla, family = nbinom2)
summary(prott)
Anova(prott)
summary(glht(prott,linfct=mcp(Timepoint="Tukey")))

tene = glmmTMB(Tenericutes ~ Stock + Timepoint + (1|Individual), 
               data = phyla, family = nbinom2)
summary(tene)
Anova(tene)
summary(glht(tene,linfct=mcp(Stock="Tukey")))

tenet = glmmTMB(Tenericutes ~ Timepoint + (1|Stock), 
               data = phyla, family = nbinom2)
summary(tenet)
Anova(tenet)
summary(glht(tenet,linfct=mcp(Timepoint="Tukey")))

verr = glmmTMB(Verrucomicrobia ~ Stock + Timepoint + (1|Individual), 
               data = phyla, family = nbinom2)
summary(verr)
Anova(verr)
summary(glht(verr,linfct=mcp(Stock="Tukey")))

verrt = glmmTMB(Verrucomicrobia ~ Timepoint + (1|Stock), 
               data = phyla, family = nbinom2)
summary(verrt)
Anova(verrt)
summary(glht(verrt,linfct=mcp(Timepoint="Tukey")))

#Phyla plots ----

library(ggpubr)

first.phyla = filter(phyla, Timepoint == "first")
last.phyla = filter(phyla, Timepoint == "last")
stock.phyla = filter(phyla, Timepoint == "stock")

protplot0 = ggboxplot(stock.phyla, x = "Stock", 
                      y = "Proteobacteria", color = "Stock", 
                      palette = "Set1", add = "jitter", 
                      add.params = list(fill = "white"), ylab = "Relative abundance") 
protplot0 = ggpar(protplot0, legend = "right", 
                  title = "Proteobacteria: stock") +
  rremove("xlab") + rremove("x.text") + 
  rremove("legend.title")

protplot1 = ggboxplot(first.phyla, x = "Stock", 
                      y = "Proteobacteria", color = "Stock", 
                      palette = "Set1", add = "jitter", 
                      add.params = list(fill = "white"), ylab = "Relative abundance") 
protplot1 = ggpar(protplot1, legend = "right", 
                  title = "Proteobacteria: first time point") + 
  rremove("xlab") + rremove("x.text") + 
  rremove("legend.title")

protplot2 = ggboxplot(last.phyla, x = "Stock", 
                      y = "Proteobacteria", color = "Stock", 
                      palette = "Set1", add = "jitter", 
                      add.params = list(fill = "white"), ylab = "Relative abundance") 
protplot2 = ggpar(protplot2, legend = "right", 
                  title = "Proteobacteria: last time point") + 
  rremove("xlab") + rremove("x.text") + 
  rremove("legend.title")

tiff(file="proteobacteria_all_noinfants.tif", res=150, width=15, height=4, units="in")
ggarrange(protplot0, protplot1, protplot2, ncol = 3, common.legend = TRUE, legend = "right",
          nrow = 1, align = "h")
dev.off()

#Family GLMMs----
library(glmmTMB)
library(multcomp)
library(car)
library(tidyverse)
library(fdrtool)

family_relab = read.csv("families_noinfant.csv", header = T)
metadata = read.csv("mapping_all_r_update072722_noinfant.csv.csv", header = T)

family = metadata %>% inner_join(family_relab, by = "SampleID") 

stock_estimatematrix = mat.or.vec(124,2)
stock_pvaluematrix = mat.or.vec(124,2)
timepoint_estimatematrix = mat.or.vec(124,2)
timepoint_pvaluematrix = mat.or.vec(124,2)

for(i in 12:135) {
  variable = family[,i]
  b = glmmTMB(variable ~ Stock + Timepoint + (1|Individual), 
                  data = family, family = nbinom2)
  anova = Anova(b)
  stock_estimatematrix[i-11,2] = anova[1,1]
  stock_pvaluematrix[i-11,2] = anova[1,3]
  timepoint_estimatematrix[i-11,2] = anova[2,1]
  timepoint_pvaluematrix[i-11,2] = anova[2,3]
  stock_estimatematrix[i-11,1] = names(family)[i]
  stock_pvaluematrix[i-11,1] = names(family)[i]
  timepoint_estimatematrix[i-11,1] = names(family)[i]
  timepoint_pvaluematrix[i-11,1] = names(family)[i]
}

family_stock = bind_cols(as.data.frame(stock_estimatematrix[,1:2]), 
                         as.data.frame(stock_pvaluematrix[,2]))
write.csv(family_stock, "family_stock_noinfants.csv")
family_stock_nona = filter(family_stock, family_stock[,3] != "NaN")
family_stock_nona[,3] = as.numeric(as.character(family_stock_nona[,3]))
family_stock_corrected = bind_cols(family_stock_nona, 
                                   as.data.frame(fdrtool(family_stock_nona[,3], 
                                                         statistic = "pvalue", plot = F)))
write.csv(family_stock_corrected, "family_stock_corrected_noinfants.csv")

family_timepoint = bind_cols(as.data.frame(timepoint_estimatematrix[,1:2]), 
                             as.data.frame(timepoint_pvaluematrix[,2]))
write.csv(family_timepoint, "family_timepoint_noinfants.csv")
family_timepoint_nona = filter(family_timepoint, family_timepoint[,3] != "NaN")
family_timepoint_nona[,3] = as.numeric(as.character(family_timepoint_nona[,3]))
family_timepoint_corrected = bind_cols(family_timepoint_nona, 
                                       as.data.frame(fdrtool(family_timepoint_nona[,3], 
                                                             statistic = "pvalue", plot = F)))
write.csv(family_timepoint_corrected, "family_timepoint_corrected_noinfants.csv")

#Stocks as random to assess timepoint

timepoint_estimatematrix = mat.or.vec(124,2)
timepoint_pvaluematrix = mat.or.vec(124,2)

for(i in 12:135) {
  variable = family[,i]
  b = try(glmmTMB(variable ~ Timepoint + (1|Stock), 
                  data = family, family = nbinom2))
  anova = Anova(b)
  timepoint_estimatematrix[i-11,2] = anova[1,1]
  timepoint_pvaluematrix[i-11,2] = anova[1,3]
  timepoint_estimatematrix[i-11,1] = names(family)[i]
  timepoint_pvaluematrix[i-11,1] = names(family)[i]
}

family_timepoint = bind_cols(as.data.frame(timepoint_estimatematrix[,1:2]), 
                             as.data.frame(timepoint_pvaluematrix[,2]))
write.csv(family_timepoint, "family_timepoint_blockstock_noinfants.csv")
family_timepoint_nona = filter(family_timepoint, family_timepoint[,3] != "NaN")
family_timepoint_nona[,3] = as.numeric(as.character(family_timepoint_nona[,3]))
family_timepoint_corrected = bind_cols(family_timepoint_nona, 
                                       as.data.frame(fdrtool(family_timepoint_nona[,3], 
                                                             statistic = "pvalue", plot = F)))
write.csv(family_timepoint_corrected, "family_timepoint_blockstock_corrected_noinfants.csv")

#Genus GLMMs----
library(glmmTMB)
library(multcomp)
library(car)
library(tidyverse)
library(fdrtool)

genus_relab = read.csv("genera_noinfant.csv", header = T)
metadata = read.csv("mapping_all_r_update072722_noinfant.csv", header = T)

genus = metadata %>% inner_join(genus_relab, by = "SampleID") 

stock_estimatematrix = mat.or.vec(264,2)
stock_pvaluematrix = mat.or.vec(264,2)
timepoint_estimatematrix = mat.or.vec(264,2)
timepoint_pvaluematrix = mat.or.vec(264,2)

for(i in 212:276) {
  variable = genus[,i]
  b = try(glmmTMB(variable ~ Stock + Timepoint + (1|Individual), 
                  data = genus, family = nbinom2))
  anova = Anova(b)
  stock_estimatematrix[i-11,2] = anova[1,1]
  stock_pvaluematrix[i-11,2] = anova[1,3]
  timepoint_estimatematrix[i-11,2] = anova[2,1]
  timepoint_pvaluematrix[i-11,2] = anova[2,3]
  stock_estimatematrix[i-11,1] = names(genus)[i]
  stock_pvaluematrix[i-11,1] = names(genus)[i]
  timepoint_estimatematrix[i-11,1] = names(genus)[i]
  timepoint_pvaluematrix[i-11,1] = names(genus)[i]
}

genus_stock = bind_cols(as.data.frame(stock_estimatematrix[,1:2]), 
                         as.data.frame(stock_pvaluematrix[,2]))
write.csv(genus_stock, "genus_stock_noinfants.csv")
genus_stock_nona = filter(genus_stock, genus_stock[,3] != "NaN")
genus_stock_nona[,3] = as.numeric(as.character(genus_stock_nona[,3]))
genus_stock_corrected = bind_cols(genus_stock_nona, 
                                   as.data.frame(fdrtool(genus_stock_nona[,3], 
                                                         statistic = "pvalue", plot = F)))
write.csv(genus_stock_corrected, "genus_stock_corrected_noinfants.csv")

genus_timepoint = bind_cols(as.data.frame(timepoint_estimatematrix[,1:2]), 
                             as.data.frame(timepoint_pvaluematrix[,2]))
write.csv(genus_timepoint, "genus_timepoint_noinfants.csv")
genus_timepoint_nona = filter(genus_timepoint, genus_timepoint[,3] != "NaN")
genus_timepoint_nona[,3] = as.numeric(as.character(genus_timepoint_nona[,3]))
genus_timepoint_corrected = bind_cols(genus_timepoint_nona, 
                                       as.data.frame(fdrtool(genus_timepoint_nona[,3], 
                                                             statistic = "pvalue", plot = F)))
write.csv(genus_timepoint_corrected, "genus_timepoint_corrected_noinfants.csv")

#Stocks as random to assess timepoint

timepoint_estimatematrix = mat.or.vec(264,2)
timepoint_pvaluematrix = mat.or.vec(264,2)

for(i in 12:276) {
  variable = genus[,i]
  b = try(glmmTMB(variable ~ Timepoint + (1|Stock), 
                  data = genus, family = nbinom2))
  anova = Anova(b)
  timepoint_estimatematrix[i-11,2] = anova[1,1]
  timepoint_pvaluematrix[i-11,2] = anova[1,3]
  timepoint_estimatematrix[i-11,1] = names(genus)[i]
  timepoint_pvaluematrix[i-11,1] = names(genus)[i]
}

genus_timepoint = bind_cols(as.data.frame(timepoint_estimatematrix[,1:2]), 
                             as.data.frame(timepoint_pvaluematrix[,2]))
write.csv(genus_timepoint, "genus_timepoint_blockstock_noinfants.csv")
genus_timepoint_nona = filter(genus_timepoint, genus_timepoint[,3] != "NaN")
genus_timepoint_nona[,3] = as.numeric(as.character(genus_timepoint_nona[,3]))
genus_timepoint_corrected = bind_cols(genus_timepoint_nona, 
                                       as.data.frame(fdrtool(genus_timepoint_nona[,3], 
                                                             statistic = "pvalue", plot = F)))
write.csv(genus_timepoint_corrected, "genus_timepoint_blockstock_corrected_noinfants.csv")

#Average distances unweighted ----
library(tidyverse)

metadata = read.csv("mapping_all_r_update072722_noinfant.csv", header=T)
unweighted = read.table("unweighted-distance-matrix-noinfant-a.tsv", header = T, fill = T)

unweighted_long = unweighted %>% 
  pivot_longer(2:83, names_to = "pair", values_to = "distance")

unweighted_long_filtered = unweighted_long %>% 
  filter(distance != 0) %>% filter(is.na(distance) == FALSE)

unweighted_long_filtered_meta = unweighted_long_filtered %>% 
  left_join(metadata, by = c("sampleid" = "SampleID")) 

write.csv(unweighted_long_filtered_meta, "unweighted_long_filtered_meta_noinfant.csv")

ulfm = read.csv("unweighted_long_filtered_meta_noinfant.csv", header = T)

ulfm = ulfm %>% left_join(metadata, by = c("pair" = "SampleID"))

ulfm = ulfm %>% mutate(SamplePairMatch = ifelse(Sample_treatment_time == 
                                                  Treatment_time, "Match", 
                                                "NonMatch"))

ulfm_added = ulfm %>% 
  mutate(SamplePair = paste(Sample_stock, Sample_age, Sample_timepoint, 
                            Stock, Age, Timepoint, sep = "_"))

ulfm_added$SamplePair = as.factor(ulfm_added$SamplePair)

ulfm_added = ulfm_added %>% 
  mutate(SamplePairNew = ifelse(SamplePair == "human_adult_last_human_adult_first", 
                                "human_adult_first_human_adult_last", ifelse(
                                      SamplePair == "SQM_adult_first_human_adult_first",
                                      "human_adult_first_SQM_adult_first", ifelse(
                                        SamplePair == "SQM_adult_last_human_adult_first",
                                        "human_adult_first_SQM_adult_last", ifelse(
                                          SamplePair == "SQM_adult_first_human_adult_last",
                                          "human_adult_last_SQM_adult_first", ifelse(
                                            SamplePair == "SQM_adult_last_human_adult_last",
                                            "human_adult_last_SQM_adult_last", ifelse(
                                              SamplePair == "macaque_adult_last_macaque_adult_first",
                                              "macaque_adult_first_macaque_adult_last", ifelse(
                                                SamplePair == "SQM_adult_first_macaque_adult_first",
                                                "macaque_adult_first_SQM_adult_first", ifelse(
                                                  SamplePair == "SQM_adult_last_macaque_adult_first",
                                                  "macaque_adult_first_SQM_adult_last", ifelse(
                                                    SamplePair == "SQM_adult_first_macaque_adult_last",
                                                    "macaque_adult_last_SQM_adult_first", ifelse(
                                                      SamplePair == "SQM_adult_last_macaque_adult_last",
                                                      "macaque_adult_last_SQM_adult_last", as.character(SamplePair)
                                                      )))))))))))
                                                                                         
ulfm_added = ulfm_added %>% 
  mutate(SamplePairNew2 = ifelse(SamplePairNew == "SQM_infant_last_macaque_adult_last",
 "macaque_adult_last_SQM_infant_last", ifelse(
   SamplePair == "macaque_infant_stock_macaque_adult_stock",
   "macaque_adult_stock_macaque_infant_stock", ifelse(
     SamplePair == "macaque_infant_last_macaque_infant_first",
     "macaque_infant_first_macaque_infant_last", ifelse(
       SamplePair == "SQM_adult_first_macaque_infant_first",
       "macaque_infant_first_SQM_adult_first", ifelse(
         SamplePair == "SQM_adult_last_macaque_infant_first",
         "macaque_infant_first_SQM_adult_last", ifelse(
           SamplePair == "SQM_infant_first_macaque_infant_first",
           "macaque_infant_first_SQM_infant_first", ifelse(
             SamplePair == "SQM_infant_last_macaque_infant_first",
             "macaque_infant_first_SQM_infant_last", ifelse(
               SamplePair == "SQM_adult_first_macaque_infant_last",
               "macaque_infant_last_SQM_adult_first", ifelse(
                 SamplePair == "SQM_adult_last_macaque_infant_last",
                 "macaque_infant_last_SQM_adult_last", ifelse(
                   SamplePair == "SQM_infant_first_macaque_infant_last",
                   "macaque_infant_last_SQM_infant_first", ifelse(
                     SamplePair == "SQM_infant_last_macaque_infant_last",
                     "macaque_infant_last_SQM_infant_last", ifelse(
                       SamplePair == "SQM_adult_last_SQM_adult_first",
                       "SQM_adult_first_SQM_adult_last", ifelse(
                         SamplePair == "SQM_infant_first_SQM_adult_first",
                         "SQM_adult_first_SQM_infant_first", ifelse(
                           SamplePair == "SQM_infant_last_SQM_adult_first",
                           "SQM_adult_first_SQM_infant_last", ifelse(
                             SamplePair == "SQM_infant_first_SQM_adult_last",
                             "SQM_adult_last_SQM_infant_first", ifelse(
                               SamplePair == "SQM_infant_last_SQM_adult_last",
                               "SQM_adult_last_SQM_infant_last", ifelse(
                                 SamplePair == "SQM_infant_stock_SQM_adult_stock",
                                 "SQM_adult_stock_SQM_infant_stock", ifelse(
                                   SamplePair == "SQM_infant_last_SQM_infant_first",
                                    "SQM_infant_first_SQM_infant_last", as.character(SamplePairNew)
                                     )))))))))))))))))))

ulfm_added$SamplePairNew2 = as.factor(ulfm_added$SamplePairNew2)

write.csv(ulfm_added, "unweighted_distance_noinfant.csv")

unweighted_grouped_distances = ulfm_added %>% 
  group_by(SamplePairNew2) %>% 
  summarize(mean_distance = mean(distance), sd_distance = sd(distance))

write.csv(unweighted_grouped_distances, "unweighted_distance_comp_noinfant.csv")

library(ggpubr)

tiff(file="unweighted_pairwise_distance_noinfant.tif", res=150, width=10, height=30, units="in")
ggboxplot(ulfm_added, x = "SamplePairNew2", y = "distance", 
          ylab = "Pairwise Unweighted UniFrac Distance", rotate = TRUE)
dev.off()

#Average distances weighted ----
library(tidyverse)

metadata = read.csv("mapping_all_r_update072722_noinfant.csv", header=T)
weighted = read.table("weighted-distance-matrix-noinfant-a.tsv", header = T, fill = T)

weighted_long = weighted %>% 
  pivot_longer(2:83, names_to = "pair", values_to = "distance")

weighted_long_filtered = weighted_long %>% 
  filter(distance != 0) %>% filter(is.na(distance) == FALSE)

weighted_long_filtered_meta = weighted_long_filtered %>% 
  left_join(metadata, by = c("sampleid" = "SampleID")) 

write.csv(weighted_long_filtered_meta, "weighted_long_filtered_meta_noinfant.csv")

wlfm = read.csv("weighted_long_filtered_meta2_noinfant.csv", header = T)

wlfm = wlfm %>% left_join(metadata, by = c("pair" = "SampleID"))

wlfm = wlfm %>% mutate(SamplePairMatch = ifelse(Sample_treatment_time == 
                                                  Treatment_time, "Match", 
                                                "NonMatch"))

wlfm_added = wlfm %>% 
  mutate(SamplePair = paste(Sample_stock, Sample_age, Sample_timepoint, 
                            Stock, Age, Timepoint, sep = "_"))

wlfm_added$SamplePair = as.factor(wlfm_added$SamplePair)

wlfm_added = wlfm_added %>% 
  mutate(SamplePairNew = ifelse(SamplePair == "human_adult_last_human_adult_first", 
                                "human_adult_first_human_adult_last", ifelse(
                                  SamplePair == "human_infant_first_human_adult_first",
                                  "human_adult_first_human_infant_first", ifelse(
                                    SamplePair == "human_infant_last_human_adult_first",
                                    "human_adult_first_human_infant_last", ifelse(
                                      SamplePair == "SQM_adult_first_human_adult_first",
                                      "human_adult_first_SQM_adult_first", ifelse(
                                        SamplePair == "SQM_adult_last_human_adult_first",
                                        "human_adult_first_SQM_adult_last", ifelse(
                                          SamplePair == "SQM_infant_first_human_adult_first",
                                          "human_adult_first_SQM_infant_first", ifelse(
                                            SamplePair == "SQM_infant_last_human_adult_first",
                                            "human_adult_first_SQM_infant_last", ifelse(
                                              SamplePair == "human_infant_first_human_adult_last",
                                              "human_adult_last_human_infant_first", ifelse(
                                                SamplePair == "human_infant_last_human_adult_last",
                                                "human_adult_last_human_infant_last", ifelse(
                                                  SamplePair == "SQM_adult_first_human_adult_last",
                                                  "human_adult_last_SQM_adult_first", ifelse(
                                                    SamplePair == "SQM_adult_last_human_adult_last",
                                                    "human_adult_last_SQM_adult_last", ifelse(
                                                      SamplePair == "SQM_infant_first_human_adult_last",
                                                      "human_adult_last_SQM_infant_first", ifelse(
                                                        SamplePair == "SQM_infant_last_human_adult_last",
                                                        "human_adult_last_SQM_infant_last", ifelse(
                                                          SamplePair == "human_infant_last_human_infant_first",
                                                          "human_infant_first_human_infant_last", ifelse(
                                                            SamplePair == "macaque_adult_first_human_infant_first",
                                                            "human_infant_first_macaque_adult_first", ifelse(
                                                              SamplePair == "macaque_adult_last_human_infant_first",
                                                              "human_infant_first_macaque_adult_last", ifelse(
                                                                SamplePair == "macaque_infant_first_human_infant_first",
                                                                "human_infant_first_macaque_infant_first", ifelse(
                                                                  SamplePair == "macaque_infant_last_human_infant_first",
                                                                  "human_infant_first_macaque_infant_last", ifelse(
                                                                    SamplePair == "SQM_adult_first_human_infant_first",
                                                                    "human_infant_first_SQM_adult_first", ifelse(
                                                                      SamplePair == "SQM_adult_last_human_infant_first", 
                                                                      "human_infant_first_SQM_adult_last", ifelse(
                                                                        SamplePair == "SQM_infant_first_human_infant_first",
                                                                        "human_infant_first_SQM_infant_first", ifelse(
                                                                          SamplePair == "SQM_infant_last_human_infant_first",
                                                                          "human_infant_first_SQM_infant_last", ifelse(
                                                                            SamplePair == "SQM_adult_first_human_infant_last",
                                                                            "human_infant_last_SQM_adult_first", ifelse(
                                                                              SamplePair == "SQM_adult_last_human_infant_last",
                                                                              "human_infant_last_SQM_adult_last", ifelse(
                                                                                SamplePair == "SQM_infant_first_human_infant_last",
                                                                                "human_infant_last_SQM_infant_first", ifelse(
                                                                                  SamplePair == "SQM_infant_last_human_infant_last",
                                                                                  "human_infant_last_SQM_infant_last", ifelse(
                                                                                    SamplePair == "macaque_adult_last_macaque_adult_first",
                                                                                    "macaque_adult_first_macaque_adult_last", ifelse(
                                                                                      SamplePair == "macaque_infant_first_macaque_adult_first",
                                                                                      "macaque_adult_first_macaque_infant_first", ifelse(
                                                                                        SamplePair == "macaque_infant_last_macaque_adult_first",
                                                                                        "macaque_adult_first_macaque_infant_last", ifelse(
                                                                                          SamplePair == "SQM_adult_first_macaque_adult_first", 
                                                                                          "macaque_adult_first_SQM_adult_first", ifelse(
                                                                                            SamplePair == "SQM_adult_last_macaque_adult_first", 
                                                                                            "macaque_adult_first_SQM_adult_last", ifelse(
                                                                                              SamplePair == "SQM_infant_first_macaque_adult_first", 
                                                                                              "macaque_adult_first_SQM_infant_first", ifelse(
                                                                                                SamplePair == "SQM_infant_last_macaque_adult_first",
                                                                                                "macaque_adult_first_SQM_infant_last", ifelse(
                                                                                                  SamplePair == "macaque_infant_first_macaque_adult_last",
                                                                                                  "macaque_adult_last_macaque_infant_first", ifelse(
                                                                                                    SamplePair == "macaque_infant_last_macaque_adult_last",
                                                                                                    "macaque_adult_last_macaque_infant_last", ifelse(
                                                                                                      SamplePair == "SQM_adult_first_macaque_adult_last",
                                                                                                      "macaque_adult_last_SQM_adult_first", ifelse(
                                                                                                        SamplePair == "SQM_adult_last_macaque_adult_last",
                                                                                                        "macaque_adult_last_SQM_adult_last", ifelse(
                                                                                                          SamplePair == "SQM_infant_first_macaque_adult_last",
                                                                                                          "macaque_adult_last_SQM_infant_first", as.character(SamplePair)
                                                                                                        )))))))))))))))))))))))))))))))))))))))

wlfm_added = wlfm_added %>% 
  mutate(SamplePairNew2 = ifelse(SamplePairNew == "SQM_infant_last_macaque_adult_last",
                                 "macaque_adult_last_SQM_infant_last", ifelse(
                                   SamplePair == "macaque_infant_stock_macaque_adult_stock",
                                   "macaque_adult_stock_macaque_infant_stock", ifelse(
                                     SamplePair == "macaque_infant_last_macaque_infant_first",
                                     "macaque_infant_first_macaque_infant_last", ifelse(
                                       SamplePair == "SQM_adult_first_macaque_infant_first",
                                       "macaque_infant_first_SQM_adult_first", ifelse(
                                         SamplePair == "SQM_adult_last_macaque_infant_first",
                                         "macaque_infant_first_SQM_adult_last", ifelse(
                                           SamplePair == "SQM_infant_first_macaque_infant_first",
                                           "macaque_infant_first_SQM_infant_first", ifelse(
                                             SamplePair == "SQM_infant_last_macaque_infant_first",
                                             "macaque_infant_first_SQM_infant_last", ifelse(
                                               SamplePair == "SQM_adult_first_macaque_infant_last",
                                               "macaque_infant_last_SQM_adult_first", ifelse(
                                                 SamplePair == "SQM_adult_last_macaque_infant_last",
                                                 "macaque_infant_last_SQM_adult_last", ifelse(
                                                   SamplePair == "SQM_infant_first_macaque_infant_last",
                                                   "macaque_infant_last_SQM_infant_first", ifelse(
                                                     SamplePair == "SQM_infant_last_macaque_infant_last",
                                                     "macaque_infant_last_SQM_infant_last", ifelse(
                                                       SamplePair == "SQM_adult_last_SQM_adult_first",
                                                       "SQM_adult_first_SQM_adult_last", ifelse(
                                                         SamplePair == "SQM_infant_first_SQM_adult_first",
                                                         "SQM_adult_first_SQM_infant_first", ifelse(
                                                           SamplePair == "SQM_infant_last_SQM_adult_first",
                                                           "SQM_adult_first_SQM_infant_last", ifelse(
                                                             SamplePair == "SQM_infant_first_SQM_adult_last",
                                                             "SQM_adult_last_SQM_infant_first", ifelse(
                                                               SamplePair == "SQM_infant_last_SQM_adult_last",
                                                               "SQM_adult_last_SQM_infant_last", ifelse(
                                                                 SamplePair == "SQM_infant_stock_SQM_adult_stock",
                                                                 "SQM_adult_stock_SQM_infant_stock", ifelse(
                                                                   SamplePair == "SQM_infant_last_SQM_infant_first",
                                                                   "SQM_infant_first_SQM_infant_last", as.character(SamplePairNew)
                                                                 )))))))))))))))))))

wlfm_added$SamplePairNew2 = as.factor(wlfm_added$SamplePairNew2)

write.csv(wlfm_added, "weighted_distance_noinfant.csv")

weighted_grouped_distances = wlfm_added %>% 
  group_by(SamplePairNew2) %>% 
  summarize(mean_distance = mean(distance), sd_distance = sd(distance))

write.csv(weighted_grouped_distances, "weighted_distance_comp_noinfant.csv")

library(ggpubr)

tiff(file="weighted_pairwise_distance_noinfant.tif", res=150, width=10, height=30, units="in")
ggboxplot(wlfm_added, x = "SamplePairNew2", y = "distance", 
          ylab = "Pairwise weighted UniFrac Distance", rotate = TRUE)
dev.off()

