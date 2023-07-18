#Set up environment----

setwd("/Users/elizabethmallott/Dropbox/Projects/Gut_microbiome/mouse_inoculation/shotgun/humann2_output")

#Import data----
bray_gene = as.dist(read.table("bray-genefamilies-unstrat-noinfant.tsv", header = T))
jaccard_gene = as.dist(read.table("jaccard_genefamilies_unstrat_noinfant.tsv", header = T))
bray_pa = as.dist(read.table("bray_pathabund_unstrat_noinfant.tsv", header = T))
jaccard_pa = as.dist(read.table("jaccard_pathabund_unstrat_noinfant.tsv", header = T))
metadata_gene = read.csv("mapping_genefam_update_072722_noinfant.csv", header = T)
metadata_pa = read.csv("mapping_update_072722_noinfant.csv", header = T)

#Permanovas----
library(vegan)
library(pairwiseAdonis)

adonis2(bray_gene ~ Stock, data=metadata_gene, 
        by = "margin", permutations = 5000)

pairwise.adonis(bray_gene, factors = metadata_gene$Stock, perm = 5000, 
                p.adjust.m='holm')

adonis2(jaccard_gene ~ Stock, data=metadata_gene, 
        by = "margin", permutations = 5000)

pairwise.adonis(jaccard_gene, factors = metadata_gene$Stock, perm = 5000, 
                p.adjust.m='holm')

adonis2(bray_pa ~ Stock, data=metadata_pa, 
        by = "margin", permutations = 5000)

pairwise.adonis(bray_pa, factors = metadata_pa$Stock, perm = 5000, 
                p.adjust.m='holm')

adonis2(jaccard_pa ~ Stock, data=metadata_pa, 
        by = "margin", permutations = 5000)

pairwise.adonis(jaccard_pa, factors = metadata_pa$Stock, perm = 5000, 
                p.adjust.m='holm')

#NMDS plots----
library(ggplot2)
library(vegan)

metadata_gene_new = metadata_gene %>% 
  mutate(Treatment = case_when(Treatment == "human_adult" ~ "Human",
                               Treatment == "macaque_adult" ~ "Macaque",
                               Treatment == "SQM_adult" ~ "Squirrel Monkey",))

metadata_pa_new = metadata_pa %>% 
  mutate(Treatment = case_when(Treatment == "human_adult" ~ "Human",
                               Treatment == "macaque_adult" ~ "Macaque",
                               Treatment == "SQM_adult" ~ "Squirrel Monkey"))

metadata_gene_new$Treatment<-factor(metadata_gene_new$Treatment,levels=
                             c("Human",
                               "Squirrel Monkey", 
                               "Macaque"))

metadata_pa_new$Treatment<-factor(metadata_pa_new$Treatment,levels=
                             c("Human",
                               "Squirrel Monkey", 
                               "Macaque"))

mds_otus_bray_gene<-metaMDS(bray_gene, k=2, trymax=500)
mds_otus_bray_gene_points<-mds_otus_bray_gene$points
mds_otus_bray_gene_points2<-merge(x=mds_otus_bray_gene_points, y = metadata_gene_new, 
                                 by.x = "row.names", by.y = "sampleid")
tiff(file="nmds_plot_bray_genefam_unstrat.tif", res=300, width=8, height=6, units="in")
braygf <- ggplot(mds_otus_bray_gene_points2, aes(x = MDS1, y = MDS2, 
                                                 color = Treatment)) +
  geom_point(size=3, shape = 17) + scale_color_brewer(palette = 'Paired') +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'), 
        legend.key=element_blank()) + 
  theme(axis.title.x=element_text(size=rel(2)), 
        axis.title.y=element_text(size=rel(2)),
        plot.title = element_text(size=rel(3)),
        legend.title = element_text(size=rel(2)),
        legend.text = element_text(size = rel(1.8))) + 
  ggtitle("Bray-Curtis:\nGene families") 
braygf
dev.off()

mds_otus_jaccard_gene<-metaMDS(jaccard_gene, k=2, trymax=500)
mds_otus_jaccard_gene_points<-mds_otus_jaccard_gene$points
mds_otus_jaccard_gene_points2<-merge(x=mds_otus_jaccard_gene_points, y = metadata_gene_new, 
                                  by.x = "row.names", by.y = "sampleid")
tiff(file="nmds_plot_jaccard_genefam_unstrat.tif", res=300, width=8, height=6, units="in")
jaccgf <- ggplot(mds_otus_jaccard_gene_points2, aes(x = MDS1, y = MDS2, 
                                                  color = Treatment)) +
  geom_point(size=3, shape=17) + scale_color_brewer(palette = 'Paired') +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'), 
        legend.key=element_blank()) + 
  theme(axis.title.x=element_text(size=rel(2)), 
        axis.title.y=element_text(size=rel(2)),
        plot.title = element_text(size=rel(3)),
        legend.title = element_text(size=rel(2)),
        legend.text = element_text(size = rel(1.8))) + 
  ggtitle("Jaccard:\nGene families") 
jaccgf
dev.off()

mds_otus_bray_pa<-metaMDS(bray_pa, k=2, trymax=500)
mds_otus_bray_pa_points<-mds_otus_bray_pa$points
mds_otus_bray_pa_points2<-merge(x=mds_otus_bray_pa_points, y = metadata_pa_new, 
                                  by.x = "row.names", by.y = "sampleid")
tiff(file="nmds_plot_bray_pathabund_unstrat.tif", res=300, width=8, height=6, units="in")
braypa <- ggplot(mds_otus_bray_pa_points2, aes(x = MDS1, y = MDS2, 
                                                  color = Treatment)) +
  geom_point(size=3, shape=17) + scale_color_brewer(palette = 'Paired') +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'), 
        legend.key=element_blank()) + 
  theme(axis.title.x=element_text(size=rel(2)), 
        axis.title.y=element_text(size=rel(2)),
        plot.title = element_text(size=rel(3)),
        legend.title = element_text(size=rel(2)),
        legend.text = element_text(size = rel(1.8))) + 
  ggtitle("Bray-Curtis:\nPathway abundance") 
braypa
dev.off()

mds_otus_jaccard_pa<-metaMDS(jaccard_pa, k=2, trymax=500)
mds_otus_jaccard_pa_points<-mds_otus_jaccard_pa$points
mds_otus_jaccard_pa_points2<-merge(x=mds_otus_jaccard_pa_points, y = metadata_pa_new, 
                                     by.x = "row.names", by.y = "sampleid")
tiff(file="nmds_plot_jaccard_pathabund_unstrat.tif", res=300, width=8, height=6, units="in")
jaccpa <- ggplot(mds_otus_jaccard_pa_points2, aes(x = MDS1, y = MDS2, 
                                                     color = Treatment)) +
  geom_point(size=3, shape=17) + scale_color_brewer(palette = 'Paired') +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'), 
        legend.key=element_blank()) + 
  theme(axis.title.x=element_text(size=rel(2)), 
        axis.title.y=element_text(size=rel(2)),
        plot.title = element_text(size=rel(3)),
        legend.title = element_text(size=rel(2)),
        legend.text = element_text(size = rel(1.8))) + 
  ggtitle("Jaccard:\nPathway Abundance") 
jaccpa
dev.off()

library(cowplot)

tiff(file = "nmds_plot_combined_shotgun.tif", res = 300, width = 18, height = 14, units="in")
legend1 = get_legend(braygf + theme(legend.box.margin = margin(0, 0, 0, 12)))
col1 = plot_grid(braygf + theme(legend.position = "none"),
                 braypa + theme(legend.position = "none"),
                 nrow = 2, ncol = 1, labels = c('A', 'C'), 
                 label_size = 20)
col2 = plot_grid(jaccgf + theme(legend.position = "none"),
                 jaccpa + theme(legend.position = "none"),
                 nrow = 2, ncol = 1, labels = c('B', 'D'),
                 label_size = 20)
plot_grid(col1, col2, legend1, 
          nrow = 1, ncol = 3, rel_widths = c(1.5, 1.5, 0.75), align = "hv", 
          axis = "t")
dev.off()

#Alpha diversity models ----

library(tidyverse)
library(nlme)
library(multcomp)
library(car)

shannon_gene = read.table("shannon_genefam_unstrat.tsv", header = T)
evenness_gene = read.table("evenness_genefam_unstrat.tsv", header = T)
obs_otus_gene = read.table("observed_otus_genefam_unstrat.tsv", header = T)

shannon_pa = read.table("shannon_pathabund_unstrat.tsv", header = T)
evenness_pa = read.table("evenness_pathabund_unstrat.tsv", header = T)
obs_otus_pa = read.table("observed_otus_pathabund_unstrat.tsv", header = T)

alpha_gene = inner_join(metadata_gene_new, shannon_gene, by = "sampleid") %>% 
  inner_join(evenness_gene, by = "sampleid") %>% 
  inner_join(obs_otus_gene, by = "sampleid")
alpha_gene$Stock = as.factor(alpha_gene$Stock)

alpha_pa = inner_join(metadata_pa_new, shannon_pa, by = "sampleid") %>% 
  inner_join(evenness_pa, by = "sampleid") %>% 
  inner_join(obs_otus_pa, by = "sampleid")
alpha_pa$Stock = as.factor(alpha_pa$Stock)

sg <- lm(shannon ~ Stock, data = alpha_gene)
summary(sg)
Anova(sg)
summary(glht(sg,linfct=mcp(Stock="Tukey")))

eg <- lm(pielou_e ~ Stock, data = alpha_gene)
summary(eg)
Anova(eg)
summary(glht(eg,linfct=mcp(Stock="Tukey")))

og <- lm(observed_otus ~ Stock, data = alpha_gene)
summary(og)
Anova(og)
summary(glht(og,linfct=mcp(Stock="Tukey")))

sp <- lm(shannon ~ Stock, data = alpha_pa)
summary(sp)
Anova(sp)
summary(glht(sp,linfct=mcp(Stock="Tukey")))

ep <- lm(pielou_e ~ Stock, data = alpha_pa)
summary(ep)
Anova(ep)
summary(glht(ep,linfct=mcp(Stock="Tukey")))

op <- lm(observed_otus ~ Stock, data = alpha_pa)
summary(op)
Anova(op)
summary(glht(op,linfct=mcp(Stock="Tukey")))

#Alpha diversity plots ---- 

library(ggpubr)

plot1 = ggboxplot(alpha_gene, x = "Treatment", 
                  y = "shannon", color = "Treatment", 
                  palette = "Paired", add = "jitter", 
                  add.params = list(fill = "white"), ylab = "Shannon Diversity") 
plot1 = ggpar(plot1, legend = "right") + rremove("xlab") + rremove("x.text") + 
  rremove("legend.title")

plot2 = ggboxplot(alpha_gene, x = "Treatment", 
                  y = "pielou_e", color = "Treatment", 
                  palette = "Paired", add = "jitter", 
                  add.params = list(fill = "white"), ylab = "Pielou's Evenness") 
plot2 = ggpar(plot2, legend = "right") + rremove("xlab") + rremove("x.text") + 
  rremove("legend.title")


plot3 = ggboxplot(alpha_gene, x = "Treatment", 
                  y = "observed_otus", color = "Treatment", 
                  palette = "Paired", add = "jitter", 
                  add.params = list(fill = "white"), ylab = "Observed OTUs") 
plot3 = ggpar(plot3, legend = "right") + rremove("xlab") + rremove("x.text") + 
  rremove("legend.title")

tiff(file="alpha_diversity_genefam.tif", res=150, width=16, height=4, units="in")
ggarrange(plot1, plot2, plot3, ncol = 3, common.legend = TRUE, legend = "right",
          nrow = 1, align = "h")
dev.off()

plot4 = ggboxplot(alpha_pa, x = "Treatment", 
                  y = "shannon", color = "Treatment", 
                  palette = "Paired", add = "jitter", 
                  add.params = list(fill = "white"), ylab = "Shannon Diversity") 
plot4 = ggpar(plot4, legend = "right") + rremove("xlab") + rremove("x.text") + 
  rremove("legend.title")

plot5 = ggboxplot(alpha_pa, x = "Treatment", 
                  y = "pielou_e", color = "Treatment", 
                  palette = "Paired", add = "jitter", 
                  add.params = list(fill = "white"), ylab = "Pielou's Evenness") 
plot5 = ggpar(plot5, legend = "right") + rremove("xlab") + rremove("x.text") + 
  rremove("legend.title")


plot6 = ggboxplot(alpha_pa, x = "Treatment", 
                  y = "observed_otus", color = "Treatment", 
                  palette = "Paired", add = "jitter", 
                  add.params = list(fill = "white"), ylab = "Observed OTUs") 
plot6 = ggpar(plot6, legend = "right") + rremove("xlab") + rremove("x.text") + 
  rremove("legend.title")

tiff(file="alpha_diversity_pathabund.tif", res=150, width=16, height=4, units="in")
ggarrange(plot4, plot5, plot6, ncol = 3, common.legend = TRUE, legend = "right",
          nrow = 1, align = "h")
dev.off()

#Pathway abundance GLMMs----

library(glmmTMB)
library(multcomp)
library(car)
library(tidyverse)
library(fdrtool)


path_abund = read.csv("shotgun_mouse_pathabundance_relab_unstratified.csv", 
                      header = T)
metadata_pa = read.csv("mapping_update_072722_noinfant.csv", header = T)

pa = metadata_pa %>% inner_join(path_abund, by = "sampleid")

Anova(glmmTMB(`FUCCAT.PWY..fucose.degradation` ~ Stock, data = pa, family  = nbinom2))

#Interaction
stock_estimatematrix = mat.or.vec(448,2)
stock_pvaluematrix = mat.or.vec(448,2)

for(i in 7:454) {
  variable = pa[,i]
  b = try(glmmTMB(variable ~ Stock, 
                  data = pa, family = nbinom2))
  anova = Anova(b)
  stock_estimatematrix[i-6,2] = anova[1,1]
  stock_pvaluematrix[i-6,2] = anova[1,3]
  stock_estimatematrix[i-6,1] = names(pa)[i]
  stock_pvaluematrix[i-6,1] = names(pa)[i]
}

pa_stock = bind_cols(as.data.frame(stock_estimatematrix[,1:2]), 
                         as.data.frame(stock_pvaluematrix[,2]))
write.csv(pa_stock, "pa_stock_noinfant.csv")
pa_stock_nona = filter(pa_stock, pa_stock[,3] != "NaN") 
pa_stock_nona[,3] = as.numeric(as.character(pa_stock_nona[,3]))
pa_stock_corrected = bind_cols(pa_stock_nona, 
                                   as.data.frame(fdrtool(pa_stock_nona[,3], 
                                                         statistic = "pvalue", plot = F)))
write.csv(pa_stock_corrected, "pa_stock_corrected_noinfant.csv")


#No Interaction
stock_estimatematrix = mat.or.vec(448,2)
stock_pvaluematrix = mat.or.vec(448,2)
age_estimatematrix = mat.or.vec(448,2)
age_pvaluematrix = mat.or.vec(448,2)

for(i in 7:454) {
  variable = pa[,i]
  b = try(glmmTMB(variable ~ Stock + Age, 
                  data = pa, family = nbinom2))
  anova = Anova(b)
  stock_estimatematrix[i-6,2] = anova[1,1]
  stock_pvaluematrix[i-6,2] = anova[1,3]
  age_estimatematrix[i-6,2] = anova[2,1]
  age_pvaluematrix[i-6,2] = anova[2,3]
  stock_estimatematrix[i-6,1] = names(pa)[i]
  stock_pvaluematrix[i-6,1] = names(pa)[i]
  age_estimatematrix[i-6,1] = names(pa)[i]
  age_pvaluematrix[i-6,1] = names(pa)[i]
}

pa_stock = bind_cols(as.data.frame(stock_estimatematrix[,1:2]), 
                     as.data.frame(stock_pvaluematrix[,2]))
write.csv(pa_stock, "pa_stock_noint.csv")
pa_stock_nona = filter(pa_stock, pa_stock[,3] != "NaN") 
pa_stock_nona[,3] = as.numeric(as.character(pa_stock_nona[,3]))
pa_stock_corrected = bind_cols(pa_stock_nona, 
                               as.data.frame(fdrtool(pa_stock_nona[,3], 
                                                     statistic = "pvalue", plot = F)))
write.csv(pa_stock_corrected, "pa_stock_noint_corrected.csv")

pa_age = bind_cols(as.data.frame(age_estimatematrix[,1:2]), 
                   as.data.frame(age_pvaluematrix[,2]))
write.csv(pa_age, "pa_age_noint.csv")
pa_age_nona = filter(pa_age, pa_age[,3] != "NaN")
pa_age_nona[,3] = as.numeric(as.character(pa_age_nona[,3]))
pa_age_corrected = bind_cols(pa_age_nona, 
                             as.data.frame(fdrtool(pa_age_nona[,3], 
                                                   statistic = "pvalue", plot = F)))
write.csv(pa_age_corrected, "pa_age_noint_corrected.csv")

a = glmmTMB(X1CMET2.PWY..N10.formyl.tetrahydrofolate.biosynthesis ~ Stock + Age + Stock*Age, 
            data = pa, family = nbinom2)
summary(a)
Anova(a)
summary(glht(a,linfct=mcp(Stock="Tukey")))

#Pathway abundance LMs----
library(fdrtool)
library(car)
library(multcomp)

Anova(lm(`FUCCAT.PWY..fucose.degradation` ~ Stock, data = pa))

#No interaction
stock_estimatematrix = mat.or.vec(448,2)
stock_pvaluematrix = mat.or.vec(448,2)

for(i in 7:454) {
  variable = pa[,i]
  b<-lm(variable ~ Stock, data=pa)
  anova = anova(b)
  stock_estimatematrix[i-6,1] = names(pa)[i]
  stock_estimatematrix[i-6,2] = anova[1,4]
  stock_pvaluematrix[i-6] = anova[1,5]
}

pa_stock = bind_cols(as.data.frame(stock_estimatematrix[,1:2]), 
                         as.data.frame(stock_pvaluematrix[,1]))
write.csv(pa_stock, "pa_stock_lm_noinfant.csv")
pa_stock_nona = filter(pa_stock, pa_stock[,3] != "NaN") 
pa_stock_nona[,3] = as.numeric(as.character(pa_stock_nona[,3]))
pa_stock_corrected = bind_cols(pa_stock_nona, 
                               as.data.frame(fdrtool(pa_stock_nona[,3], 
                                                     statistic = "pvalue", plot = F)))
write.csv(pa_stock_corrected, "pa_stock_lm_corrected_noinfant.csv")


a = lm(X1CMET2.PWY..N10.formyl.tetrahydrofolate.biosynthesis ~ Stock + Age, 
       data = pa)
summary(a)
anova(a)
summary(glht(a,linfct=mcp(Stock="Tukey")))
summary(glht(a,linfct=mcp(Age="Tukey")))

#Interaction
stock_estimatematrix = mat.or.vec(448,2)
stock_pvaluematrix = mat.or.vec(448,2)
age_estimatematrix = mat.or.vec(448,2)
age_pvaluematrix = mat.or.vec(448,2)
stockage_estimatematrix = mat.or.vec(448,2)
stockage_pvaluematrix = mat.or.vec(448,2)

for(i in 7:454) {
  variable = pa[,i]
  b<-lm(variable ~ Stock + Age + Stock*Age, data=pa)
  anova = anova(b)
  stock_estimatematrix[i-6,1] = names(pa)[i]
  stock_estimatematrix[i-6,2] = anova[1,4]
  stock_pvaluematrix[i-6] = anova[1,5]
  age_estimatematrix[i-6,1] = names(pa)[i]
  age_estimatematrix[i-6,2] = anova[2,4]
  age_pvaluematrix[i-6] = anova[2,5]
  stockage_estimatematrix[i-6,1] = names(pa)[i]
  stockage_estimatematrix[i-6,2] = anova[3,4]
  stockage_pvaluematrix[i-6] = anova[3,5]
}

pa_stock = bind_cols(as.data.frame(stock_estimatematrix[,1:2]), 
                     as.data.frame(stock_pvaluematrix[,1]))
write.csv(pa_stock, "pa_stock_int_lm.csv")
pa_stock_corrected = bind_cols(pa_stock, 
                               as.data.frame(fdrtool(pa_stock[,3], 
                                                     statistic = "pvalue", 
                                                     plot = F)))
write.csv(pa_stock_corrected, "pa_stock_int_lm_corrected.csv")

pa_age = bind_cols(as.data.frame(age_estimatematrix[,1:2]), 
                   as.data.frame(age_pvaluematrix[,1]))
write.csv(pa_age, "pa_age_int_lm.csv")
pa_age_corrected = bind_cols(pa_age, 
                             as.data.frame(fdrtool(pa_age[,3], 
                                                   statistic = "pvalue", plot = F)))
write.csv(pa_age_corrected, "pa_age_int_lm_corrected.csv")

pa_stockage = bind_cols(as.data.frame(stockage_estimatematrix[,1:2]), 
                   as.data.frame(stockage_pvaluematrix[,1]))
write.csv(pa_age, "pa_stockage_int_lm.csv")
pa_stockage_corrected = bind_cols(pa_stockage, 
                             as.data.frame(fdrtool(pa_stockage[,3], 
                                                   statistic = "pvalue", plot = F)))
write.csv(pa_stockage_corrected, "pa_stockage_int_lm_corrected.csv")

#Tukey pathway loops----
library(multcomp)

pa$Stock = as.factor(pa$Stock)

a = lm(X1CMET2.PWY..N10.formyl.tetrahydrofolate.biosynthesis ~ Stock, 
       data = pa)
summary(a)
anova(a)
summary(glht(a,linfct=mcp(Stock="Tukey")))

hvs_estimatematrix = mat.or.vec(448,2)
hvs_pvaluematrix = mat.or.vec(448,2)
hvm_estimatematrix = mat.or.vec(448,2)
hvm_pvaluematrix = mat.or.vec(448,2)
mvs_estimatematrix = mat.or.vec(448,2)
mvs_pvaluematrix = mat.or.vec(448,2)

for(i in 7:454) {
  variable = pa[,i]
  b<-lm(variable ~ Stock, data=pa)
  tukey = summary(glht(b,linfct=mcp(Stock="Tukey")))
  hvs_estimatematrix[i-6,1] = names(pa)[i]
  hvs_estimatematrix[i-6,2] = tukey[["test"]][["tstat"]][2]
  hvs_pvaluematrix[i-6] = tukey[["test"]][["pvalues"]][2]
  hvm_estimatematrix[i-6,1] = names(pa)[i]
  hvm_estimatematrix[i-6,2] = tukey[["test"]][["tstat"]][1]
  hvm_pvaluematrix[i-6] = tukey[["test"]][["pvalues"]][1]
  mvs_estimatematrix[i-6,1] = names(pa)[i]
  mvs_estimatematrix[i-6,2] = tukey[["test"]][["tstat"]][3]
  mvs_pvaluematrix[i-6] = tukey[["test"]][["pvalues"]][3]
}

pa_hvs_tukey = bind_cols(as.data.frame(hvs_estimatematrix[,1:2]), 
                     as.data.frame(hvs_pvaluematrix[,1]))
write.csv(pa_hvs_tukey, "pa_hvs_tukey_lm_noinfant.csv")

pa_hvm_tukey = bind_cols(as.data.frame(hvm_estimatematrix[,1:2]), 
                         as.data.frame(hvm_pvaluematrix[,1]))
write.csv(pa_hvm_tukey, "pa_hvm_tukey_lm_noinfant.csv")

pa_mvs_tukey = bind_cols(as.data.frame(mvs_estimatematrix[,1:2]), 
                         as.data.frame(mvs_pvaluematrix[,1]))
write.csv(pa_mvs_tukey, "pa_mvs_tukey_lm_noinfant.csv")

