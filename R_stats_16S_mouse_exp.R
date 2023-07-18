#Set up environment----

setwd("/Users/elizabethmallott/Dropbox/Projects/Gut_microbiome/mouse_inoculation/16S/Experimental_only")

#Import data----
unweighted = as.dist(read.table("unweighted-distance-matrix-noinfant.tsv", header = T))
weighted = as.dist(read.table("weighted-distance-matrix-noinfant.tsv", header = T))
metadata = read.csv("mapping_exp_r_updated_072722_noinfant.csv", header=T)

#Permanovas----
library(vegan)
library(pairwiseAdonis)

adonis2(unweighted ~ Stock + Timepoint, data=metadata, 
        by = "margin", permutations = 5000)

pairwise.adonis(unweighted, factors = metadata$Stock, perm = 5000, 
                p.adjust.m='holm')

adonis2(weighted ~ Stock + Timepoint, data=metadata, 
        by = "margin", permutations = 5000)

pairwise.adonis(weighted, factors = metadata$Stock, perm = 5000, 
                p.adjust.m='holm')

#NMDS plots----
library(ggplot2)
library(vegan)
library(tidyverse)

metadatanew = metadata %>% 
  mutate(Treatment = case_when(Treatment == "human_adult_mouse" ~ "Human Adult",
                              Treatment == "macaque_adult_mouse" ~ "Macaque Adult",
                              Treatment == "SQM_adult_mouse" ~ "Squirrel Monkey Adult"))

metadatanew$Treatment<-factor(metadatanew$Treatment,levels=
                            c("Human Adult",
                              "Squirrel Monkey Adult", 
                              "Macaque Adult"))

mds_otus_weighted<-metaMDS(weighted, k=2, trymax=500)
mds_otus_weighted_points<-mds_otus_weighted$points
mds_otus_weighted_points2<-merge(x=mds_otus_weighted_points, y = metadatanew, 
                                 by.x = "row.names", by.y = "SampleID")
tiff(file="nmds_plot_weighted_exp_noinfant.tif", res=300, width=8, height=6, units="in")
nmds1v2w <- ggplot(mds_otus_weighted_points2, aes(x = MDS1, y = MDS2, 
                                                 color = Treatment, shape = Timepoint)) +
  geom_point(size=3) + scale_color_brewer(palette = 'Set1') +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'), 
        legend.key=element_blank()) + 
  theme(axis.title.x=element_text(size=rel(2)), 
        axis.title.y=element_text(size=rel(2)),
        plot.title = element_text(size=rel(3)),
        legend.title = element_text(size=rel(2)),
        legend.text = element_text(size = rel(1.5))) + 
  ggtitle("Weighted UniFrac") 
nmds1v2w
dev.off()

mds_otus_unweighted<-metaMDS(unweighted, k=2, trymax=500)
mds_otus_unweighted_points<-mds_otus_unweighted$points
mds_otus_unweighted_points2<-merge(x=mds_otus_unweighted_points, y = metadatanew, 
                                   by.x = "row.names", by.y = "SampleID")
tiff(file="nmds_plot_unweighted_exp_noinfant.tif", res=300, width=8, height=6, units="in")
nmds1v2u <- ggplot(mds_otus_unweighted_points2, aes(x = MDS1, y = MDS2, 
                                                   color = Treatment, shape = Timepoint)) +
  geom_point(size=3) + scale_color_brewer(palette = 'Set1') +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'), 
        legend.key=element_blank()) + 
  theme(axis.title.x=element_text(size=rel(2)), 
        axis.title.y=element_text(size=rel(2)),
        plot.title = element_text(size=rel(3)),
        legend.title = element_text(size=rel(2)),
        legend.text = element_text(size = rel(1.5))) + 
  ggtitle("Unweighted UniFrac") 
nmds1v2u
dev.off()

library(cowplot)

tiff(file = "nmds_plot_combined_exp_noinfant.tif", res = 300, width = 16, height = 6, units="in")
legend1 = get_legend(nmds1v2w + theme(legend.box.margin = margin(0, 0, 0, 12)))
col1 = plot_grid(nmds1v2u + theme(legend.position = "none"),
                 nrow = 1, ncol = 1, labels = c('A'), 
                 label_size = 20)
col2 = plot_grid(nmds1v2w + theme(legend.position = "none"), 
                 nrow = 1, ncol = 1, labels = c('B'),
                 label_size = 20)
plot_grid(col1, col2, legend1, 
          nrow = 1, ncol = 3, rel_widths = c(1.5, 1.5, 0.75), align = "h", axis = "t")
dev.off()

#Alpha diversity ----

faith = read.table("pd-diversity.tsv", header=T)
breakaway = read.table("breakaway-richness.tsv", header = T)
shannon = read.table("shannon-diversity.tsv", header = T)
metadata = read.csv("mapping_exp_r_updated_072722_noinfant.csv", header=T)

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
summary(glht(f,linfct=mcp(Timepoint="Tukey")))
summary(glht(f,linfct=mcp(Stock="Tukey")))

b <- lme(fixed = estimate ~ Stock + Timepoint, data = alpha, 
         random = ~1|Individual)
summary(b)
Anova(b)
summary(glht(b,linfct=mcp(Timepoint="Tukey")))
summary(glht(b,linfct=mcp(Stock="Tukey")))

s <- lme(fixed = shannon ~ Stock + Timepoint, data = alpha, 
         random = ~1|Individual)
summary(s)
Anova(s)
summary(glht(s,linfct=mcp(Timepoint="Tukey")))
summary(glht(s,linfct=mcp(Stock="Tukey")))

library(ggpubr)

alphanew = alpha %>% 
  mutate(Treatment = case_when(Treatment == "human_adult_mouse" ~ "Human Adult",
                               Treatment == "macaque_adult_mouse" ~ "Macaque Adult",
                               Treatment == "SQM_adult_mouse" ~ "Squirrel Monkey Adult"))

first = filter(alphanew, Timepoint == "first")
last = filter(alphanew, Timepoint == "last")

first$Treatment<-factor(first$Treatment,levels=
                          c("Human Adult",
                            "Squirrel Monkey Adult",
                            "Macaque Adult"))
last$Treatment<-factor(last$Treatment,levels=
                         c("Human Adult",
                           "Squirrel Monkey Adult",
                           "Macaque Adult"))

plot1 = ggboxplot(first, x = "Treatment", 
                  y = "shannon", color = "Treatment", 
                  palette = "Set1", add = "jitter", 
                  add.params = list(fill = "white"), ylab = "Shannon Diversity") 
plot1 = ggpar(plot1, legend = "right", title = "First time point") + 
  rremove("xlab") + rremove("x.text") + 
  rremove("legend.title")

plot2 = ggboxplot(last, x = "Treatment", 
                  y = "shannon", color = "Treatment", 
                  palette = "Set1", add = "jitter", 
                  add.params = list(fill = "white"), ylab = "Shannon Diversity") 
plot2 = ggpar(plot2, legend = "right", title = "Last time point") + 
  rremove("xlab") + rremove("x.text") + 
  rremove("legend.title")

tiff(file="shannon_exp_noinfant.tif", res=150, width=15, height=4, units="in")
ggarrange(plot1, plot2, ncol = 2, common.legend = TRUE, legend = "right",
          nrow = 1, align = "h")
dev.off()

plot3 = ggboxplot(first, x = "Treatment", 
                  y = "estimate", color = "Treatment", 
                  palette = "Set1", add = "jitter", 
                  add.params = list(fill = "white"), ylab = "Breakaway Richness") 
plot3 = ggpar(plot3, legend = "right") + 
  rremove("xlab") + rremove("x.text") + 
  rremove("legend.title")

plot4 = ggboxplot(last, x = "Treatment", 
                  y = "estimate", color = "Treatment", 
                  palette = "Set1", add = "jitter", 
                  add.params = list(fill = "white"), ylab = "Breakaway Richness") 
plot4 = ggpar(plot4, legend = "right") + 
  rremove("xlab") + rremove("x.text") + 
  rremove("legend.title")

tiff(file="breakaway_exp_noinfant.tif", res=150, width=15, height=4, units="in")
ggarrange(plot3, plot4, ncol = 2, common.legend = TRUE, legend = "right",
          nrow = 1, align = "h")
dev.off()

plot5 = ggboxplot(first, x = "Treatment", 
                  y = "faith_pd", color = "Treatment", 
                  palette = "Set1", add = "jitter", 
                  add.params = list(fill = "white"), ylab = "Faith's PD") 
plot5 = ggpar(plot5, legend = "right") + 
  rremove("xlab") + rremove("x.text") + 
  rremove("legend.title")

plot6 = ggboxplot(last, x = "Treatment", 
                  y = "faith_pd", color = "Treatment", 
                  palette = "Set1", add = "jitter", 
                  add.params = list(fill = "white"), ylab = "Faith's PD") 
plot6 = ggpar(plot6, legend = "right") + 
  rremove("xlab") + rremove("x.text") + 
  rremove("legend.title")

tiff(file="faith_exp_noinfant.tif", res=150, width=15, height=4, units="in")
ggarrange(plot5, plot6, ncol = 2, common.legend = TRUE, legend = "right",
          nrow = 1, align = "h")
dev.off()

tiff(file = "combined_alpha_exp_noinfant.tif", res=300, width=12, height=8, units="in")
combined = ggarrange(plot1, plot2 + rremove("ylab"), plot3, 
          plot4 + rremove("ylab"), plot5, 
          plot6 + rremove("ylab"), ncol = 2, nrow = 3,
          common.legend = TRUE, legend = "right", align = "hv")
combined
dev.off()

#Phyla relative abundance ----

library(glmmTMB)
library(multcomp)
library(car)
library(tidyverse)

metadata_phyla = read.csv("mapping_exp_r_updated_072722_noinfant.csv", header = T)
seqs = read.csv("phyla.csv", header = T)

phyla = inner_join(seqs, metadata_phyla, by = "SampleID") 
phyla$Stock = as.factor(phyla$Stock)
phyla$Timepoint = as.factor(phyla$Timepoint)

acti = glmmTMB(Actinobacteria ~ Stock + Timepoint + (1|Individual), 
               data = phyla, family = nbinom2)
summary(acti)
Anova(acti)
summary(glht(acti,linfct=mcp(Stock="Tukey")))

bact = glmmTMB(Bacteroidetes ~ Stock + Timepoint + (1|Individual), 
               data = phyla, family = nbinom2)
summary(bact)
Anova(bact)
summary(glht(bact,linfct=mcp(Stock="Tukey")))

firm = glmmTMB(Firmicutes ~ Stock + Timepoint + (1|Individual), 
               data = phyla, family = nbinom2)
summary(firm)
Anova(firm)
summary(glht(firm,linfct=mcp(Stock="Tukey")))

fuso = glmmTMB(Fusobacteria ~ Stock + Timepoint + (1|Individual), 
               data = phyla, family = nbinom2)
summary(fuso)
Anova(fuso)
summary(glht(fuso,linfct=mcp(Stock="Tukey")))

prot = glmmTMB(Proteobacteria ~ Stock + Timepoint + (1|Individual), 
               data = phyla, family = nbinom2)
summary(prot)
Anova(prot)
summary(glht(prot,linfct=mcp(Stock="Tukey")))
summary(glht(prot, linfct = mcp(Timepoint = "Tukey")))

tene = glmmTMB(Tenericutes ~ Stock + Timepoint + (1|Individual), 
               data = phyla, family = nbinom2)
summary(tene)
Anova(tene)
summary(glht(tene,linfct=mcp(Stock="Tukey")))

verr = glmmTMB(Verrucomicrobia ~ Stock + Timepoint + (1|Individual), 
               data = phyla, family = nbinom2)
summary(verr)
Anova(verr)
summary(glht(verr,linfct=mcp(Stock="Tukey")))

#Phyla plots ----

library(ggpubr)

phylanew = phyla %>% 
  mutate(Treatment = case_when(Treatment == "human_adult_mouse" ~ "Human Adult",
                               Treatment == "human_infant_mouse" ~ "Human Infant",
                               Treatment == "macaque_adult_mouse" ~ "Macaque Adult",
                               Treatment == "macaque_infant_mouse" ~ "Macaque Infant",
                               Treatment == "SQM_adult_mouse" ~ "Squirrel Monkey Adult",
                               Treatment == "SQM_infant_mouse" ~ "Squirrel Monkey Infant"))

phylanew$Treatment<-factor(phylanew$Treatment,levels=
                                c("Human Adult","Human Infant",
                                  "Squirrel Monkey Adult", "Squirrel Monkey Infant",
                                  "Macaque Adult", "Macaque Infant"))

first.phyla = filter(phylanew, Timepoint == "first")
last.phyla = filter(phylanew, Timepoint == "last")

protplot1 = ggboxplot(first.phyla, x = "Treatment", 
                  y = "Proteobacteria", color = "Treatment", 
                  palette = "Paired", add = "jitter", 
                  add.params = list(fill = "white"), ylab = "Relative abundance") 
protplot1 = ggpar(protplot1, legend = "right", 
                  title = "Proteobacteria: first time point") + 
  rremove("xlab") + rremove("x.text") + 
  rremove("legend.title")

protplot2 = ggboxplot(last.phyla, x = "Treatment", 
                  y = "Proteobacteria", color = "Treatment", 
                  palette = "Paired", add = "jitter", 
                  add.params = list(fill = "white"), ylab = "Relative abundance") 
protplot2 = ggpar(protplot2, legend = "right", 
              title = "Proteobacteria: last time point") + 
  rremove("xlab") + rremove("x.text") + 
  rremove("legend.title")

tiff(file="proteobacteria_exp.tif", res=150, width=15, height=4, units="in")
ggarrange(protplot1, protplot2, ncol = 2, common.legend = TRUE, legend = "right",
          nrow = 1, align = "h")
dev.off()

#Family GLMMs----
library(glmmTMB)
library(multcomp)
library(car)
library(tidyverse)
library(fdrtool)


family_relab = read.csv("families-noinfant.csv", header = T)
metadata = read.csv("mapping_exp_r_updated_072722_noinfant.csv", header = T)

family = metadata %>% inner_join(family_relab, by = "SampleID") 

stock_estimatematrix = mat.or.vec(59,2)
stock_pvaluematrix = mat.or.vec(59,2)
timepoint_estimatematrix = mat.or.vec(59,2)
timepoint_pvaluematrix = mat.or.vec(59,2)

for(i in 10:68) {
  variable = family[,i]
  b = try(glmmTMB(variable ~ Stock + Timepoint + (1|Individual), 
                  data = family, family = nbinom2))
  anova = Anova(b)
  stock_estimatematrix[i-9,2] = anova[1,1]
  stock_pvaluematrix[i-9,2] = anova[1,3]
  timepoint_estimatematrix[i-9,2] = anova[2,1]
  timepoint_pvaluematrix[i-9,2] = anova[2,3]
  stock_estimatematrix[i-9,1] = names(family)[i]
  stock_pvaluematrix[i-9,1] = names(family)[i]
  timepoint_estimatematrix[i-9,1] = names(family)[i]
  timepoint_pvaluematrix[i-9,1] = names(family)[i]
}

family_stock = bind_cols(as.data.frame(stock_estimatematrix[,1:2]), 
                                  as.data.frame(stock_pvaluematrix[,2]))
write.csv(family_stock, "family_stock_noinfant.csv")
family_stock_nona = filter(family_stock, family_stock[,3] != "NaN")
family_stock_nona[,3] = as.numeric(as.character(family_stock_nona[,3]))
family_stock_corrected = bind_cols(family_stock_nona, 
                                            as.data.frame(fdrtool(family_stock_nona[,3], 
                                                                  statistic = "pvalue", plot = F)))
write.csv(family_stock_corrected, "family_stock_corrected_noinfant.csv")

family_timepoint = bind_cols(as.data.frame(timepoint_estimatematrix[,1:2]), 
                                 as.data.frame(timepoint_pvaluematrix[,2]))
write.csv(family_timepoint, "family_timepoint_noinfant.csv")
family_timepoint_nona = filter(family_timepoint, family_timepoint[,3] != "NaN")
family_timepoint_nona[,3] = as.numeric(as.character(family_timepoint_nona[,3]))
family_timepoint_corrected = bind_cols(family_timepoint_nona, 
                                           as.data.frame(fdrtool(family_timepoint_nona[,3], 
                                                                 statistic = "pvalue", plot = F)))
write.csv(family_timepoint_corrected, "family_timepoint_corrected_noinfant.csv")

#Genus GLMMs----
library(glmmTMB)
library(multcomp)
library(car)
library(tidyverse)
library(fdrtool)


genus_relab = read.csv("genera-noinfant.csv", header = T)
metadata = read.csv("mapping_exp_r_updated_072722_noinfant.csv", header = T)

genus = metadata %>% inner_join(genus_relab, by = "SampleID") 

stock_estimatematrix = mat.or.vec(111,2)
stock_pvaluematrix = mat.or.vec(111,2)
timepoint_estimatematrix = mat.or.vec(111,2)
timepoint_pvaluematrix = mat.or.vec(111,2)

for(i in 10:120) {
  variable = genus[,i]
  b = try(glmmTMB(variable ~ Stock + Timepoint + (1|Individual), 
                  data = genus, family = nbinom2))
  anova = Anova(b)
  stock_estimatematrix[i-9,2] = anova[1,1]
  stock_pvaluematrix[i-9,2] = anova[1,3]
  timepoint_estimatematrix[i-9,2] = anova[2,1]
  timepoint_pvaluematrix[i-9,2] = anova[2,3]
  stock_estimatematrix[i-9,1] = names(genus)[i]
  stock_pvaluematrix[i-9,1] = names(genus)[i]
  timepoint_estimatematrix[i-9,1] = names(genus)[i]
  timepoint_pvaluematrix[i-9,1] = names(genus)[i]
}

genus_stock = bind_cols(as.data.frame(stock_estimatematrix[,1:2]), 
                         as.data.frame(stock_pvaluematrix[,2]))
write.csv(genus_stock, "genus_stock_noinfant.csv")
genus_stock_nona = filter(genus_stock, genus_stock[,3] != "NaN")
genus_stock_nona[,3] = as.numeric(as.character(genus_stock_nona[,3]))
genus_stock_corrected = bind_cols(genus_stock_nona, 
                                   as.data.frame(fdrtool(genus_stock_nona[,3], 
                                                         statistic = "pvalue", plot = F)))
write.csv(genus_stock_corrected, "genus_stock_corrected_noinfant.csv")

genus_timepoint = bind_cols(as.data.frame(timepoint_estimatematrix[,1:2]), 
                             as.data.frame(timepoint_pvaluematrix[,2]))
write.csv(genus_timepoint, "genus_timepoint_noinfant.csv")
genus_timepoint_nona = filter(genus_timepoint, genus_timepoint[,3] != "NaN")
genus_timepoint_nona[,3] = as.numeric(as.character(genus_timepoint_nona[,3]))
genus_timepoint_corrected = bind_cols(genus_timepoint_nona, 
                                       as.data.frame(fdrtool(genus_timepoint_nona[,3], 
                                                             statistic = "pvalue", plot = F)))
write.csv(genus_timepoint_corrected, "genus_timepoint_corrected_noinfant.csv")


#Absolute abundance ----
library(tidyverse)
library(vegan)

feature_table = read_tsv("mouse-exp-paired-table-noinfant.txt")
feature_table_a = t(feature_table[-1])
colnames(feature_table_a) = feature_table[1][[1]]
feature_table_b = decostand(feature_table_a, method = "total")
feature_table_b = as_tibble(rownames_to_column(as.data.frame(feature_table_b))) %>% rename(sampleid = rowname)
qpcr = read_csv("qPCR.csv", col_types = cols(qpcr = col_number()))
merged = inner_join(feature_table_b, qpcr, by = "sampleid")
samples = merged %>% select(sampleid)
transformed = merged %>% select(-sampleid) %>% transmute_all(funs(.*qpcr)) %>% select(-qpcr)
absolute = bind_cols(samples, transformed)
write_tsv(absolute, "inferred_abundance_table_noinfant.txt")

#Phyla inferred glmms ----

library(glmmTMB)
library(multcomp)
library(car)
library(tidyverse)


metadata_phyla = read.csv("mapping_exp_r_updated_072722_noinfant.csv", header = T)
seqs = read.csv("inferred-phyla-noinfant.csv", header = T)

phyla = inner_join(seqs, metadata_phyla, by = "SampleID") %>% 
  filter(Timepoint == "last")
phyla$Stock = as.factor(phyla$Stock)

acti = glmmTMB(Actinobacteria ~ Stock, 
               data = phyla, family = nbinom2)
summary(acti)
Anova(acti)
summary(glht(acti,linfct=mcp(Stock="Tukey")))

bact = glmmTMB(Bacteroidetes ~ Stock, 
               data = phyla, family = nbinom2)
summary(bact)
Anova(bact)
summary(glht(bact,linfct=mcp(Stock="Tukey")))

firm = glmmTMB(Firmicutes ~ Stock, 
               data = phyla, family = nbinom2)
summary(firm)
Anova(firm)
summary(glht(firm,linfct=mcp(Stock="Tukey")))

fuso = glmmTMB(Fusobacteria ~ Stock, 
               data = phyla, family = nbinom2)
summary(fuso)
Anova(fuso)
summary(glht(fuso,linfct=mcp(Stock="Tukey")))

prot = glmmTMB(Proteobacteria ~ Stock, 
               data = phyla, family = nbinom2)
summary(prot)
Anova(prot)
summary(glht(prot,linfct=mcp(Stock="Tukey")))

tene = glmmTMB(Tenericutes ~ Stock, 
               data = phyla, family = nbinom2)
summary(tene)
Anova(tene)
summary(glht(tene,linfct=mcp(Stock="Tukey")))

verr = glmmTMB(Verrucomicrobia ~ Stock, 
               data = phyla, family = nbinom2)
summary(verr)
Anova(verr)
summary(glht(verr,linfct=mcp(Stock="Tukey")))

#Inferred phyla plots -----

library(ggpubr)

phylanew = phyla %>% 
  mutate(Treatment = case_when(Treatment == "human_adult_mouse" ~ "Human Adult",
                               Treatment == "macaque_adult_mouse" ~ "Macaque Adult",
                               Treatment == "SQM_adult_mouse" ~ "Squirrel Monkey Adult"))

phylanew$Treatment<-factor(phylanew$Treatment,levels=
                             c("Human Adult",
                               "Squirrel Monkey Adult",
                               "Macaque Adult"))

bactplot = ggboxplot(phylanew, x = "Treatment", 
                      y = "Bacteroidetes", color = "Treatment", 
                      palette = "Set1", add = "jitter", 
                      add.params = list(fill = "white"), ylab = "Absolute abundance") 
bactplot = ggpar(bactplot, legend = "right", 
                  title = "Bacteroidetes") + 
  rremove("xlab") + rremove("x.text") + 
  rremove("legend.title")

firmplot = ggboxplot(phylanew, x = "Treatment", 
                      y = "Firmicutes", color = "Treatment", 
                      palette = "Set1", add = "jitter", 
                      add.params = list(fill = "white"), ylab = "Absolute abundance") 
firmplot = ggpar(firmplot, legend = "right", 
                  title = "Firmicutes") + 
  rremove("xlab") + rremove("x.text") + 
  rremove("legend.title")

protplot = ggboxplot(phylanew, x = "Treatment", 
                     y = "Proteobacteria", color = "Treatment", 
                     palette = "Set1", add = "jitter", 
                     add.params = list(fill = "white"), ylab = "Absolute abundance") 
protplot = ggpar(protplot, legend = "right", 
                 title = "Proteobacteria") + 
  rremove("xlab") + rremove("x.text") + 
  rremove("legend.title")

verrplot = ggboxplot(phylanew, x = "Treatment", 
                     y = "Verrucomicrobia", color = "Treatment", 
                     palette = "Set1", add = "jitter", 
                     add.params = list(fill = "white"), ylab = "Absolute abundance") 
verrplot = ggpar(verrplot, legend = "right", 
                 title = "Verrucomicrobia") + 
  rremove("xlab") + rremove("x.text") + 
  rremove("legend.title")

tiff(file="abs_abun_phyla_noinfant.tif", res=150, width=15, height=10, units="in")
ggarrange(bactplot, firmplot, protplot, verrplot, nrow = 2, ncol = 2, 
          common.legend = TRUE, legend = "right", align = "h")
dev.off()

#Family inferred glmms ----

library(glmmTMB)
library(multcomp)
library(car)
library(tidyverse)
library(fdrtool)

family_relab = read.csv("inferred-families-noinfant.csv", header = T)
metadata = read.csv("mapping_exp_r_updated_072722_noinfant.csv", header = T)

family = metadata %>% inner_join(family_relab, by = "SampleID") %>% 
  filter(Timepoint == "last")

stock_estimatematrix = mat.or.vec(59,2)
stock_pvaluematrix = mat.or.vec(59,2)

for(i in 10:68) {
  variable = family[,i]
  b = try(glmmTMB(variable ~ Stock, 
                  data = family, family = nbinom2))
  anova = Anova(b)
  stock_estimatematrix[i-9,2] = anova[1,1]
  stock_pvaluematrix[i-9,2] = anova[1,3]
  stock_estimatematrix[i-9,1] = names(family)[i]
  stock_pvaluematrix[i-9,1] = names(family)[i]
}

family_stock = bind_cols(as.data.frame(stock_estimatematrix[,1:2]), 
                         as.data.frame(stock_pvaluematrix[,2]))
write.csv(family_stock, "inferred_family_stock_noinfant.csv")
family_stock_nona = filter(family_stock, family_stock[,3] != "NaN")
family_stock_nona[,3] = as.numeric(as.character(family_stock_nona[,3]))
family_stock_corrected = bind_cols(family_stock_nona, 
                                   as.data.frame(fdrtool(family_stock_nona[,3], 
                                                         statistic = "pvalue", plot = F)))
write.csv(family_stock_corrected, "inferred_family_stock_corrected_noinfant.csv")

#Inferred family contrasts----

family$Stock = as.factor(family$Stock)

a = glmmTMB(k__Bacteria.p__Bacteroidetes.c__Bacteroidia.o__Bacteroidales.__ ~ Stock, 
               data = family, family = nbinom2)
summary(a)
Anova(a)
summary(glht(a,linfct=mcp(Stock="Tukey")))

b = glmmTMB(k__Bacteria.p__Bacteroidetes.c__Bacteroidia.o__Bacteroidales.f__.Paraprevotellaceae. ~ Stock, 
             data = family, family = nbinom2)
summary(b)
Anova(b)
summary(glht(b,linfct=mcp(Stock="Tukey")))

c = glmmTMB(k__Bacteria.p__Bacteroidetes.c__Bacteroidia.o__Bacteroidales.f__Bacteroidaceae ~ Stock, 
            data = family, family = nbinom2)
summary(c)
Anova(c)
summary(glht(c,linfct=mcp(Stock="Tukey")))

d = glmmTMB(k__Bacteria.p__Bacteroidetes.c__Bacteroidia.o__Bacteroidales.f__Porphyromonadaceae ~ Stock, 
            data = family, family = nbinom2)
summary(d)
Anova(d)
summary(glht(d,linfct=mcp(Stock="Tukey")))

e = glmmTMB(k__Bacteria.p__Firmicutes.c__Clostridia.o__Clostridiales.f__ ~ Stock, 
             data = family, family = nbinom2)
summary(e)
Anova(e)
summary(glht(e,linfct=mcp(Stock="Tukey")))

f = glmmTMB(k__Bacteria.p__Firmicutes.c__Clostridia.o__Clostridiales.f__Lachnospiraceae ~ Stock, 
             data = family, family = nbinom2)
summary(f)
Anova(f)
summary(glht(f,linfct=mcp(Stock="Tukey")))

g = glmmTMB(k__Bacteria.p__Firmicutes.c__Clostridia.o__Clostridiales.f__Ruminococcaceae ~ Stock, 
             data = family, family = nbinom2)
summary(g)
Anova(g)
summary(glht(g,linfct=mcp(Stock="Tukey")))

h = glmmTMB(k__Bacteria.p__Firmicutes.c__Clostridia.o__Clostridiales.f__Veillonellaceae ~ Stock, 
             data = family, family = nbinom2)
summary(h)
Anova(h)
summary(glht(h,linfct=mcp(Stock="Tukey")))

i = glmmTMB(k__Bacteria.p__Verrucomicrobia.c__Verrucomicrobiae.o__Verrucomicrobiales.f__Verrucomicrobiaceae ~ Stock, 
             data = family, family = nbinom2)
summary(i)
Anova(i)
summary(glht(i,linfct=mcp(Stock="Tukey")))

#Inferred family plots----

library(ggpubr)

familynew = family %>% 
  mutate(Treatment = case_when(Treatment == "human_adult_mouse" ~ "Human Adult",
                               Treatment == "human_infant_mouse" ~ "Human Infant",
                               Treatment == "macaque_adult_mouse" ~ "Macaque Adult",
                               Treatment == "macaque_infant_mouse" ~ "Macaque Infant",
                               Treatment == "SQM_adult_mouse" ~ "Squirrel Monkey Adult",
                               Treatment == "SQM_infant_mouse" ~ "Squirrel Monkey Infant"))

familynew$Treatment<-factor(familynew$Treatment,levels=
                             c("Human Adult","Human Infant",
                               "Squirrel Monkey Adult", "Squirrel Monkey Infant",
                               "Macaque Adult", "Macaque Infant"))

aa = ggboxplot(familynew, x = "Treatment", 
                     y = "k__Bacteria.p__Bacteroidetes.c__Bacteroidia.o__Bacteroidales.__", 
               color = "Treatment", 
                     palette = "Paired", add = "jitter", 
                     add.params = list(fill = "white"), ylab = "Absolute abundance") 
aa = ggpar(aa, legend = "right", 
                 title = "Bacteroidales, unclassified family") + 
  rremove("xlab") + rremove("x.text") + 
  rremove("legend.title")

ab = ggboxplot(familynew, x = "Treatment", 
                     y = "k__Bacteria.p__Bacteroidetes.c__Bacteroidia.o__Bacteroidales.f__Bacteroidaceae", 
               color = "Treatment", 
                     palette = "Paired", add = "jitter", 
                     add.params = list(fill = "white"), ylab = "Absolute abundance") 
ab = ggpar(ab, legend = "right", 
                 title = "Bacteroidaceae") + 
  rremove("xlab") + rremove("x.text") + 
  rremove("legend.title")

ac = ggboxplot(familynew, x = "Treatment", 
                     y = "k__Bacteria.p__Bacteroidetes.c__Bacteroidia.o__Bacteroidales.f__.Paraprevotellaceae.", 
               color = "Treatment", 
                     palette = "Paired", add = "jitter", 
                     add.params = list(fill = "white"), ylab = "Absolute abundance") 
ac = ggpar(ac, legend = "right", 
                 title = "Paraprevotellaceae") + 
  rremove("xlab") + rremove("x.text") + 
  rremove("legend.title")

ad = ggboxplot(familynew, x = "Treatment", 
                     y = "k__Bacteria.p__Firmicutes.c__Clostridia.o__Clostridiales.f__", 
               color = "Treatment", 
                     palette = "Paired", add = "jitter", 
                     add.params = list(fill = "white"), ylab = "Absolute abundance") 
ad = ggpar(ad, legend = "right", 
                 title = "Clostridiales, unclassified family") + 
  rremove("xlab") + rremove("x.text") + 
  rremove("legend.title")

ae = ggboxplot(familynew, x = "Treatment", 
               y = "k__Bacteria.p__Firmicutes.c__Clostridia.o__Clostridiales.f__Lachnospiraceae", 
               color = "Treatment", 
               palette = "Paired", add = "jitter", 
               add.params = list(fill = "white"), ylab = "Absolute abundance") 
ae = ggpar(ae, legend = "right", 
           title = "Lachnospiraceae") + 
  rremove("xlab") + rremove("x.text") + 
  rremove("legend.title")

af = ggboxplot(familynew, x = "Treatment", 
               y = "k__Bacteria.p__Firmicutes.c__Clostridia.o__Clostridiales.f__Ruminococcaceae", 
               color = "Treatment", 
               palette = "Paired", add = "jitter", 
               add.params = list(fill = "white"), ylab = "Absolute abundance") 
af = ggpar(af, legend = "right", 
           title = "Ruminococcaceae") + 
  rremove("xlab") + rremove("x.text") + 
  rremove("legend.title")

ag = ggboxplot(familynew, x = "Treatment", 
               y = "k__Bacteria.p__Firmicutes.c__Clostridia.o__Clostridiales.f__Veillonellaceae", 
               color = "Treatment", 
               palette = "Paired", add = "jitter", 
               add.params = list(fill = "white"), ylab = "Absolute abundance") 
ag = ggpar(ag, legend = "right", 
           title = "Veillonellaceae") + 
  rremove("xlab") + rremove("x.text") + 
  rremove("legend.title")

ah = ggboxplot(familynew, x = "Treatment", 
               y = "k__Bacteria.p__Firmicutes.c__Erysipelotrichi.o__Erysipelotrichales.f__Erysipelotrichaceae", 
               color = "Treatment", 
               palette = "Paired", add = "jitter", 
               add.params = list(fill = "white"), ylab = "Absolute abundance") 
ah = ggpar(ah, legend = "right", 
           title = "Erysipelotrichaceae") + 
  rremove("xlab") + rremove("x.text") + 
  rremove("legend.title")

ai = ggboxplot(familynew, x = "Treatment", 
               y = "k__Bacteria.p__Proteobacteria.c__Gammaproteobacteria.o__Enterobacteriales.f__Enterobacteriaceae", 
               color = "Treatment", 
               palette = "Paired", add = "jitter", 
               add.params = list(fill = "white"), ylab = "Absolute abundance") 
ai = ggpar(ai, legend = "right", 
           title = "Enterobacteriaceae") + 
  rremove("xlab") + rremove("x.text") + 
  rremove("legend.title")

tiff(file="abs_abun_family.tif", res=150, width=15, height=10, units="in")
ggarrange(aa, ab, ac, ad, ae, af, ag, ah, ai, nrow = 3, ncol = 3, 
          common.legend = TRUE, legend = "right", align = "h")
dev.off()

#Genus inferred glmms ----
library(glmmTMB)
library(multcomp)
library(car)
library(tidyverse)
library(fdrtool)

genus_relab = read.csv("inferred-genera-noinfant.csv", header = T)
metadata = read.csv("mapping_exp_r_updated_072722_noinfant.csv", header = T)

genus = metadata %>% inner_join(genus_relab, by = "SampleID") %>% 
  filter(Timepoint == "last")

stock_estimatematrix = mat.or.vec(111,2)
stock_pvaluematrix = mat.or.vec(111,2)

for(i in 10:120) {
  variable = genus[,i]
  b = try(glmmTMB(variable ~ Stock, 
                  data = genus, family = nbinom2))
  anova = Anova(b)
  stock_estimatematrix[i-9,2] = anova[1,1]
  stock_pvaluematrix[i-9,2] = anova[1,3]
  stock_estimatematrix[i-9,1] = names(genus)[i]
  stock_pvaluematrix[i-9,1] = names(genus)[i]
}

genus_stock = bind_cols(as.data.frame(stock_estimatematrix[,1:2]), 
                        as.data.frame(stock_pvaluematrix[,2]))
write.csv(genus_stock, "inferred_genus_stock_noinfant.csv")
genus_stock_nona = filter(genus_stock, genus_stock[,3] != "NaN")
genus_stock_nona[,3] = as.numeric(as.character(genus_stock_nona[,3]))
genus_stock_corrected = bind_cols(genus_stock_nona, 
                                  as.data.frame(fdrtool(genus_stock_nona[,3], 
                                                        statistic = "pvalue", plot = F)))
write.csv(genus_stock_corrected, "inferred_genus_stock_corrected_noinfant.csv")

#Inferred genus contrasts----

a = glmmTMB(k__Bacteria.p__Bacteroidetes.c__Bacteroidia.o__Bacteroidales.__.__ ~ Stock + Age, 
            data = genus, family = nbinom2)
summary(a)
Anova(a)
summary(glht(a,linfct=mcp(Stock="Tukey")))

b = glmmTMB(k__Bacteria.p__Firmicutes.c__Clostridia.o__Clostridiales.f__Lachnospiraceae.__ ~ Stock + Age, 
            data = genus, family = nbinom2)
summary(b)
Anova(b)
summary(glht(b,linfct=mcp(Stock="Tukey")))

c = glmmTMB(k__Bacteria.p__Firmicutes.c__Clostridia.o__Clostridiales.f__Ruminococcaceae.g__Butyricicoccus ~ Stock + Age, 
            data = genus, family = nbinom2)
summary(c)
Anova(c)
summary(glht(c,linfct=mcp(Stock="Tukey")))

d = glmmTMB(k__Bacteria.p__Firmicutes.c__Clostridia.o__Clostridiales.f__Ruminococcaceae.g__Oscillospira ~ Stock + Age, 
            data = genus, family = nbinom2)
summary(d)
Anova(d)
summary(glht(d,linfct=mcp(Stock="Tukey")))

e = glmmTMB(k__Bacteria.p__Firmicutes.c__Clostridia.o__Clostridiales.f__Ruminococcaceae.g__Ruminococcus ~ Stock + Age, 
            data = genus, family = nbinom2)
summary(e)
Anova(e)
summary(glht(e,linfct=mcp(Stock="Tukey")))

f = glmmTMB(k__Bacteria.p__Proteobacteria.c__Deltaproteobacteria.o__Desulfovibrionales.f__Desulfovibrionaceae.g__Bilophila ~ Stock + Age, 
            data = genus, family = nbinom2)
summary(f)
Anova(f)
summary(glht(f,linfct=mcp(Stock="Tukey")))

g = glmmTMB(k__Bacteria.p__Proteobacteria.c__Gammaproteobacteria.o__Enterobacteriales.f__Enterobacteriaceae.g__Proteus ~ Stock + Age, 
            data = genus, family = nbinom2)
summary(g)
Anova(g)
summary(glht(g,linfct=mcp(Stock="Tukey")))

#Inferred family plots----

library(ggpubr)

genusnew = genus %>% 
  mutate(Treatment = case_when(Treatment == "human_adult_mouse" ~ "Human Adult",
                               Treatment == "human_infant_mouse" ~ "Human Infant",
                               Treatment == "macaque_adult_mouse" ~ "Macaque Adult",
                               Treatment == "macaque_infant_mouse" ~ "Macaque Infant",
                               Treatment == "SQM_adult_mouse" ~ "Squirrel Monkey Adult",
                               Treatment == "SQM_infant_mouse" ~ "Squirrel Monkey Infant"))

genusnew$Treatment<-factor(genusnew$Treatment,levels=
                              c("Human Adult","Human Infant",
                                "Squirrel Monkey Adult", "Squirrel Monkey Infant",
                                "Macaque Adult", "Macaque Infant"))

aa = ggboxplot(genusnew, x = "Treatment", 
               y = "k__Bacteria.p__Bacteroidetes.c__Bacteroidia.o__Bacteroidales.__.__", 
               color = "Treatment", 
               palette = "Paired", add = "jitter", 
               add.params = list(fill = "white"), ylab = "Absolute abundance") 
aa = ggpar(aa, legend = "right", 
           title = "Bacteroidales, unclassified genus") + 
  rremove("xlab") + rremove("x.text") + 
  rremove("legend.title")

ab = ggboxplot(genusnew, x = "Treatment", 
               y = "k__Bacteria.p__Firmicutes.c__Clostridia.o__Clostridiales.f__Lachnospiraceae.__", 
               color = "Treatment", 
               palette = "Paired", add = "jitter", 
               add.params = list(fill = "white"), ylab = "Absolute abundance") 
ab = ggpar(ab, legend = "right", 
           title = "Lachnospiraceae, unclassified genus") + 
  rremove("xlab") + rremove("x.text") + 
  rremove("legend.title")

ac = ggboxplot(genusnew, x = "Treatment", 
               y = "k__Bacteria.p__Firmicutes.c__Clostridia.o__Clostridiales.f__Ruminococcaceae.g__Butyricicoccus", 
               color = "Treatment", 
               palette = "Paired", add = "jitter", 
               add.params = list(fill = "white"), ylab = "Absolute abundance") 
ac = ggpar(ac, legend = "right", 
           title = "Butyricicoccus") + 
  rremove("xlab") + rremove("x.text") + 
  rremove("legend.title")

ad = ggboxplot(genusnew, x = "Treatment", 
               y = "k__Bacteria.p__Firmicutes.c__Clostridia.o__Clostridiales.f__Ruminococcaceae.g__Oscillospira", 
               color = "Treatment", 
               palette = "Paired", add = "jitter", 
               add.params = list(fill = "white"), ylab = "Absolute abundance") 
ad = ggpar(ad, legend = "right", 
           title = "Oscillospira") + 
  rremove("xlab") + rremove("x.text") + 
  rremove("legend.title")

ae = ggboxplot(genusnew, x = "Treatment", 
               y = "k__Bacteria.p__Firmicutes.c__Clostridia.o__Clostridiales.f__Ruminococcaceae.g__Ruminococcus", 
               color = "Treatment", 
               palette = "Paired", add = "jitter", 
               add.params = list(fill = "white"), ylab = "Absolute abundance") 
ae = ggpar(ae, legend = "right", 
           title = "Ruminococcus") + 
  rremove("xlab") + rremove("x.text") + 
  rremove("legend.title")

af = ggboxplot(genusnew, x = "Treatment", 
               y = "k__Bacteria.p__Proteobacteria.c__Deltaproteobacteria.o__Desulfovibrionales.f__Desulfovibrionaceae.g__Bilophila", 
               color = "Treatment", 
               palette = "Paired", add = "jitter", 
               add.params = list(fill = "white"), ylab = "Absolute abundance") 
af = ggpar(af, legend = "right", 
           title = "Bilophila") + 
  rremove("xlab") + rremove("x.text") + 
  rremove("legend.title")

ag = ggboxplot(genusnew, x = "Treatment", 
               y = "k__Bacteria.p__Proteobacteria.c__Gammaproteobacteria.o__Enterobacteriales.f__Enterobacteriaceae.g__Proteus", 
               color = "Treatment", 
               palette = "Paired", add = "jitter", 
               add.params = list(fill = "white"), ylab = "Absolute abundance") 
ag = ggpar(ag, legend = "right", 
           title = "Proteus") + 
  rremove("xlab") + rremove("x.text") + 
  rremove("legend.title")

tiff(file="abs_abun_genus.tif", res=150, width=18, height=7, units="in")
ggarrange(aa, ab, ac, ad, ae, af, ag, nrow = 2, ncol = 4, 
          common.legend = TRUE, legend = "right", align = "hv")
dev.off()

