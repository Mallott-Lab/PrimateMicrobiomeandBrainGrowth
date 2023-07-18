##Figure 1 (Physiology) ----

setwd("/Users/mallott/Dropbox/Projects/Gut_microbiome/mouse_inoculation/phys_data")

library(tidyverse)
library(ggpubr)
library(ggnewscale)

phys = read_csv("Phys_combined_update_072722_noinfant.csv")

physnew = phys %>% 
  mutate(Treatment = case_when(Treatment == "human_Adult" ~ "Human",
                               Treatment == "macaque_Adult" ~ "Macaque",
                               Treatment == "squirrel_monkey_Adult" ~ "Squirrel Monkey"))

physnew = physnew %>% 
  mutate(BG = case_when(BG == "SPBG" ~ "low-EQ",
                        BG == "RPBG" ~ "high-EQ"))

physnew$Treatment<-factor(physnew$Treatment,levels=
                            c("Human",
                              "Squirrel Monkey", 
                              "Macaque"))

food = read_csv("adult_food_intake.csv")

foodnew = food %>% 
  mutate(Treatment = case_when(Species == "Human" ~ "Human",
                               Species == "Macaque" ~ "Macaque",
                               Species == "SQM" ~ "Squirrel Monkey"))

foodnew = foodnew %>% 
  mutate(BG = case_when(BG == "SPBG" ~ "low-EQ",
                        BG == "RPBG" ~ "high-EQ"))

foodnew$Treatment<-factor(foodnew$Treatment,levels=
                            c("Human",
                              "Squirrel Monkey", 
                              "Macaque"))

liver = ggboxplot(physnew, x = "Treatment", 
                  y = "Liver_norm", color = "Treatment", fill = "BG",
                  add = "jitter", ylab = "Liver (% of total body weight)") +
  scale_color_manual(values = c("Human" = "#1f78b4", 
                                "Macaque" = "#e31a1c", 
                                "Squirrel Monkey" = "#33a02c")) +
  scale_fill_manual(values = c("high-EQ" = "lightgrey", "low-EQ" = "white"))
liver = ggpar(liver, legend = "right") + 
  rremove("xlab") + rremove("x.text") + 
  rremove("legend.title") +
  annotate(geom = "text", label = "p = 0.379", x = Inf, y = Inf,
           hjust = 1, vjust = 1)
  

pancreas = ggboxplot(physnew, x = "Treatment", 
                     y = "Pancreas_norm", color = "Treatment", fill = "BG", 
                     add = "jitter", ylab = "Pancreas (% of total body weight)") +
  scale_color_manual(values = c("Human" = "#1f78b4", 
                                "Macaque" = "#e31a1c", 
                                "Squirrel Monkey" = "#33a02c")) +
  scale_fill_manual(values = c("high-EQ" = "lightgrey", "low-EQ" = "white"))
pancreas = ggpar(pancreas, legend = "right") + 
  rremove("xlab") + rremove("x.text") + 
  rremove("legend.title") +
  annotate(geom = "text", label = "p = 0.080", x = Inf, y = Inf,
           hjust = 1, vjust = 1)

brain = ggboxplot(physnew, x = "Treatment", 
                  y = "Brain_norm", color = "Treatment", fill = "BG",
                  add = "jitter", ylab = "Brain (% of total body weight)") +
  scale_color_manual(values = c("Human" = "#1f78b4", 
                                "Macaque" = "#e31a1c", 
                                "Squirrel Monkey" = "#33a02c")) +
  scale_fill_manual(values = c("high-EQ" = "lightgrey", "low-EQ" = "white"))
brain = ggpar(brain, legend = "right") + 
  rremove("xlab") + rremove("x.text") + 
  rremove("legend.title") +
  annotate(geom = "text", label = "p = 0.797", x = Inf, y = Inf,
           hjust = 1, vjust = 1)

alt = ggboxplot(physnew, x = "Treatment", 
                y = "logALT", color = "Treatment", fill = "BG",
                add = "jitter", ylab = "ALT (log)") +
  scale_color_manual(values = c("Human" = "#1f78b4", 
                                "Macaque" = "#e31a1c", 
                                "Squirrel Monkey" = "#33a02c")) +
  scale_fill_manual(values = c("high-EQ" = "lightgrey", "low-EQ" = "white"))
alt = ggpar(alt, legend = "right") + 
  rremove("xlab") + rremove("x.text") + 
  rremove("legend.title") +
  annotate(geom = "text", label = "p = 0.003", x = Inf, y = Inf,
           hjust = 1, vjust = 1)

ast = ggboxplot(physnew, x = "Treatment", 
                y = "logAST", color = "Treatment", fill = "BG",
                add = "jitter", ylab = "AST (log)") +
  scale_color_manual(values = c("Human" = "#1f78b4", 
                                "Macaque" = "#e31a1c", 
                                "Squirrel Monkey" = "#33a02c")) +
  scale_fill_manual(values = c("high-EQ" = "lightgrey", "low-EQ" = "white"))
ast = ggpar(ast, legend = "right") + 
  rremove("xlab") + rremove("x.text") + 
  rremove("legend.title") +
  annotate(geom = "text", label = "p = 0.131", x = Inf, y = Inf,
           hjust = 1, vjust = 1)

alp = ggboxplot(physnew, x = "Treatment", 
                y = "Logalp", color = "Treatment", fill = "BG",
                add = "jitter", ylab = "ALP (log)") +
  scale_color_manual(values = c("Human" = "#1f78b4", 
                                "Macaque" = "#e31a1c", 
                                "Squirrel Monkey" = "#33a02c")) +
  scale_fill_manual(values = c("high-EQ" = "lightgrey", "low-EQ" = "white"))
alp = ggpar(alp, legend = "right") + 
  rremove("xlab") + rremove("x.text") + 
  rremove("legend.title") +
  annotate(geom = "text", label = "p = 0.008", x = Inf, y = Inf,
           hjust = 1, vjust = 1)

chol = ggboxplot(physnew, x = "Treatment", 
                 y = "Cholesterol", color = "Treatment", fill = "BG",
                 add = "jitter", ylab = "Cholesterol") +
  scale_color_manual(values = c("Human" = "#1f78b4", 
                                "Macaque" = "#e31a1c", 
                                "Squirrel Monkey" = "#33a02c")) +
  scale_fill_manual(values = c("high-EQ" = "lightgrey", "low-EQ" = "white"))
chol = ggpar(chol, legend = "right") + 
  rremove("xlab") + rremove("x.text") + 
  rremove("legend.title") +
  annotate(geom = "text", label = "p = 0.020", x = Inf, y = Inf,
           hjust = 1, vjust = 1)

trig = ggboxplot(physnew, x = "Treatment", 
                 y = "logTriglycerides", color = "Treatment", fill = "BG",
                 add = "jitter", ylab = "Triglycerides (log)") +
  scale_color_manual(values = c("Human" = "#1f78b4", 
                                "Macaque" = "#e31a1c", 
                                "Squirrel Monkey" = "#33a02c")) +
  scale_fill_manual(values = c("high-EQ" = "lightgrey", "low-EQ" = "white"))
trig = ggpar(trig, legend = "right") + 
  rremove("xlab") + rremove("x.text") + 
  rremove("legend.title") +
  annotate(geom = "text", label = "p < 0.001", x = Inf, y = Inf,
           hjust = 1, vjust = 1)

hdl = ggboxplot(physnew, x = "Treatment", 
                y = "HDL", color = "Treatment", fill = "BG",
                add = "jitter", ylab = "HDL Cholesterol") +
  scale_color_manual(values = c("Human" = "#1f78b4", 
                                "Macaque" = "#e31a1c", 
                                "Squirrel Monkey" = "#33a02c")) +
  scale_fill_manual(values = c("high-EQ" = "lightgrey", "low-EQ" = "white"))
hdl = ggpar(hdl, legend = "right") + 
  rremove("xlab") + rremove("x.text") + 
  rremove("legend.title") +
  annotate(geom = "text", label = "p = 0.096", x = Inf, y = Inf,
           hjust = 1, vjust = 1)

gluc = ggboxplot(physnew, x = "Treatment", 
                 y = "Glucose", color = "Treatment", fill = "BG",
                 add = "jitter", ylab = "Glucose") +
  scale_color_manual(values = c("Human" = "#1f78b4", 
                                "Macaque" = "#e31a1c", 
                                "Squirrel Monkey" = "#33a02c")) +
  scale_fill_manual(values = c("high-EQ" = "lightgrey", "low-EQ" = "white"))
gluc = ggpar(gluc, legend = "right") + 
  rremove("xlab") + rremove("x.text") + 
  rremove("legend.title") +
  annotate(geom = "text", label = "p = 0.003", x = Inf, y = Inf,
           hjust = 1, vjust = 1)

auc = ggboxplot(physnew, x = "Treatment", 
                y = "AUC", color = "Treatment", fill = "BG",
                add = "jitter", ylab = "AUC") +
  scale_color_manual(values = c("Human" = "#1f78b4", 
                                "Macaque" = "#e31a1c", 
                                "Squirrel Monkey" = "#33a02c")) +
  scale_fill_manual(values = c("high-EQ" = "lightgrey", "low-EQ" = "white"))
auc = ggpar(auc, legend = "right") + 
  rremove("xlab") + rremove("x.text") + 
  rremove("legend.title") +
  annotate(geom = "text", label = "p < 0.001", x = Inf, y = Inf,
           hjust = 1, vjust = 1)

weight = ggboxplot(physnew, x = "Treatment", 
                    y = "Body_Weight", color = "Treatment", fill = "BG",
                    add = "jitter", ylab = "Body weight (gm)") +
  scale_color_manual(values = c("Human" = "#1f78b4", 
                                "Macaque" = "#e31a1c", 
                                "Squirrel Monkey" = "#33a02c")) +
  scale_fill_manual(values = c("high-EQ" = "lightgrey", "low-EQ" = "white"))
weight = ggpar(weight, legend = "right") + 
  rremove("xlab") + rremove("x.text") + 
  rremove("legend.title")

weightg = ggboxplot(physnew, x = "Treatment", 
                    y = "percent_weight_gain", color = "Treatment", fill = "BG",
                    add = "jitter", ylab = "Percent weight gained") +
  scale_color_manual(values = c("Human" = "#1f78b4", 
                                "Macaque" = "#e31a1c", 
                                "Squirrel Monkey" = "#33a02c")) +
  scale_fill_manual(values = c("high-EQ" = "lightgrey", "low-EQ" = "white"))
weightg = ggpar(weightg, legend = "right") + 
  rremove("xlab") + rremove("x.text") + 
  rremove("legend.title") +
  annotate(geom = "text", label = "p = 0.012", x = Inf, y = Inf,
           hjust = 1, vjust = 1)

fat = ggboxplot(physnew, x = "Treatment", 
                y = "Fat", color = "Treatment", fill = "BG",
                add = "jitter", ylab = "% Fat (relative to fat free mass)") +
  scale_color_manual(values = c("Human" = "#1f78b4", 
                                "Macaque" = "#e31a1c", 
                                "Squirrel Monkey" = "#33a02c")) +
  scale_fill_manual(values = c("high-EQ" = "lightgrey", "low-EQ" = "white"))
fat = ggpar(fat, legend = "right") + 
  rremove("xlab") + rremove("x.text") + 
  rremove("legend.title") +
  annotate(geom = "text", label = "p = 0.034", x = Inf, y = Inf,
           hjust = 1, vjust = 1)

glyc = ggboxplot(physnew, x = "Treatment", 
                 y = "glycogen_conc", color = "Treatment", fill = "BG", 
                 add = "jitter", ylab = "Liver glycogen concentration") +
  scale_color_manual(values = c("Human" = "#1f78b4", 
                                "Macaque" = "#e31a1c", 
                                "Squirrel Monkey" = "#33a02c")) +
  scale_fill_manual(values = c("high-EQ" = "lightgrey", "low-EQ" = "white"))
glyc = ggpar(glyc, legend = "right") + 
  rremove("xlab") + rremove("x.text") + 
  rremove("legend.title") +
  annotate(geom = "text", label = "p = 0.133", x = Inf, y = Inf,
           hjust = 1, vjust = 1)

food = ggboxplot(foodnew, x = "Treatment", 
                 y = "Food_per_body", color = "Treatment", fill = "BG", 
                 add = "jitter", ylab = "Relative food intake") +
  scale_color_manual(values = c("Human" = "#1f78b4", 
                                "Macaque" = "#e31a1c", 
                                "Squirrel Monkey" = "#33a02c")) +
  scale_fill_manual(values = c("high-EQ" = "lightgrey", "low-EQ" = "white"))
food = ggpar(food, legend = "right") + 
  rremove("xlab") + rremove("x.text") + 
  rremove("legend.title") +
  annotate(geom = "text", label = "p < 0.001", x = Inf, y = Inf,
           hjust = 1, vjust = 1)

setwd("/Users/mallott/Dropbox/Projects/Gut_microbiome/mouse_inoculation/")

pdf(file="Fig1b-2.pdf", width=12, height=6, paper="special")
ggarrange(liver, pancreas, brain, weightg, fat, food, ncol = 3, nrow=2, 
          common.legend = TRUE, legend = "right", align = "hv", labels = c(""))
dev.off()

pdf(file="Fig1c-2.pdf", width=12, height=9, paper = "special")
ggarrange(gluc, auc, glyc, alp, ast, alt, chol, trig, hdl, ncol = 3, nrow=3, 
          common.legend = TRUE, legend = "right", align = "hv", labels = c(""))
dev.off()
#to be combined with PCA from Hongmei and pics of fat mice
#also might need to add in liver glycogen results as one of the bar charts

##Figure 2 (Microbiome) ----

setwd("/Users/mallott/Dropbox/Projects/Gut_microbiome/mouse_inoculation/shotgun/humann2_output")

library(tidyverse)
library(ComplexHeatmap)
library(circlize)

path_abund_sig = read.csv("shotgun_mouse_pathabundance_relab_unstratified_sig_contrasts_noinfant.csv",
                          header=T)
metadata_pa = read.csv("mapping_update_072722_noinfant.csv", header = T)

meta = metadata_pa %>% 
  mutate(Treatment = case_when(Treatment == "human_adult" ~ "Human",
                               Treatment == "macaque_adult" ~ "Macaque",
                               Treatment == "SQM_adult" ~ "Squirrel Monkey"))


pa_sig_mat = as.matrix(path_abund_sig[,-1])
rownames(pa_sig_mat) = path_abund_sig[,1]

color_func = colorRamp2(c(0, 0.00006, 0.0008), c("#e0ecf4", "#9ebcda", "#8856a7"))

col = list(Treatment = c("Human" = "dodgerblue3",
                   "Squirrel Monkey" = "forestgreen",
                   "Macaque" = "firebrick2"))

ha = HeatmapAnnotation(Treatment = meta$Treatment, col = col,
                       annotation_legend_param = list(Treatment = list(
                         at = c("Human","Squirrel Monkey","Macaque"))))

pa_ht = Heatmap(pa_sig_mat, name = "Pathway Abundance", col = color_func, border = T,
                clustering_distance_columns = "spearman", show_column_names = F,
                show_row_dend = F, cluster_rows = F, bottom_annotation = ha)

htmap = grid.grabExpr(draw(pa_ht, heatmap_legend_side = "left", annotation_legend_side = "left",
                   merge_legend = T, padding = unit(c(2, 2, 2, 100), "mm")))

setwd("/Users/mallott/Dropbox/Projects/Gut_microbiome/mouse_inoculation/")

tiff(file="Fig2a.tif", res = 300, width = 14, height = 8, units="in")
draw(pa_ht, heatmap_legend_side = "left", annotation_legend_side = "left",
     merge_legend = T, padding = unit(c(2, 2, 2, 100), "mm"))
dev.off()

setwd("/Users/mallott/Dropbox/Projects/Gut_microbiome/mouse_inoculation/phys_data")

library(tidyverse)
library(ggpubr)

phys = read_csv("Phys_combined_corrected_adults.csv")

physnew = phys %>% 
  mutate(Treatment = case_when(Species == "human" ~ "Human",
                               Species == "macaque" ~ "Macaque",
                               Species == "squirrel_monkey" ~ "Squirrel Monkey"))

physnew$Treatment<-factor(physnew$Treatment,levels=
                            c("Human",
                              "Squirrel Monkey", 
                              "Macaque"))

acetate = ggboxplot(physnew, x = "Treatment", 
                    y = "AA", color = "Treatment", fill = "BG",
                    add = "jitter", ylab = "Fecal acetate concentration") +
  scale_color_manual(values = c("Human" = "#1f78b4", 
                                "Macaque" = "#e31a1c", 
                                "Squirrel Monkey" = "#33a02c")) +
  scale_fill_manual(values = c("high-EQ" = "lightgrey", "low-EQ" = "white"))
acetate = ggpar(acetate, legend = "right") + 
  rremove("xlab") + rremove("x.text") + 
  rremove("legend.title") +
  annotate(geom = "text", label = "LME: p < 0.001", x = Inf, y = Inf,
           hjust = 1, vjust = 1)

prop = ggboxplot(physnew, x = "Treatment", 
                 y = "PA", color = "Treatment", fill = "BG",
                 add = "jitter", ylab = "Fecal propionate concentration") +
  scale_color_manual(values = c("Human" = "#1f78b4", 
                                "Macaque" = "#e31a1c", 
                                "Squirrel Monkey" = "#33a02c")) +
  scale_fill_manual(values = c("high-EQ" = "lightgrey", "low-EQ" = "white"))
prop = ggpar(prop, legend = "right") + 
  rremove("xlab") + rremove("x.text") + 
  rremove("legend.title") +
  annotate(geom = "text", label = "LME: p < 0.001", x = Inf, y = Inf,
           hjust = 1, vjust = 1)

but = ggboxplot(physnew, x = "Treatment", 
                y = "BA", color = "Treatment", fill = "BG", 
                add = "jitter", ylab = "Fecal butyrate concentration") +
  scale_color_manual(values = c("Human" = "#1f78b4", 
                                "Macaque" = "#e31a1c", 
                                "Squirrel Monkey" = "#33a02c")) +
  scale_fill_manual(values = c("high-EQ" = "lightgrey", "low-EQ" = "white"))
but = ggpar(but, legend = "right") + 
  rremove("xlab") + rremove("x.text") + 
  rremove("legend.title") +
  annotate(geom = "text", label = "LME: p < 0.001", x = Inf, y = Inf,
           hjust = 1, vjust = 1)

val = ggboxplot(physnew, x = "Treatment", 
                y = "VA", color = "Treatment", fill = "BG",
                add = "jitter", ylab = "Fecal valerate concentration") +
  scale_color_manual(values = c("Human" = "#1f78b4", 
                                "Macaque" = "#e31a1c", 
                                "Squirrel Monkey" = "#33a02c")) +
  scale_fill_manual(values = c("high-EQ" = "lightgrey", "low-EQ" = "white"))
val = ggpar(val, legend = "right") + 
  rremove("xlab") + rremove("x.text") + 
  rremove("legend.title") +
  annotate(geom = "text", Inf, Inf, label = "LME: p < 0.001", 
           hjust = 1, vjust = 1)

setwd("/Users/mallott/Dropbox/Projects/Gut_microbiome/mouse_inoculation/")

tiff(file="Fig2.tif", res=300, width=24, height=7, units="in")
ggarrange(htmap, ggarrange(acetate, prop, 
                                               but, val, 
                                               ncol = 2, nrow=2, 
                                               common.legend = TRUE, 
                                               legend = "right", align = "hv"),
          labels = c("A", "B"), widths = c(1, 1))
dev.off()

setEPS()
postscript(file="Fig2.eps", width=24, height=7, paper = "special")
ggarrange(htmap, ggarrange(acetate, prop, 
                           but, val, 
                           ncol = 2, nrow=2, 
                           common.legend = TRUE, 
                           legend = "right", align = "hv"),
          labels = c("A", "B"), widths = c(1, 1))
dev.off()

pdf(file = "Fig2b.pdf", width = 8, height = 6, paper = "letter")
ggarrange(acetate, prop, 
          but, val, 
          ncol = 2, nrow=2, 
          common.legend = TRUE, 
          legend = "right", align = "hv")
dev.off()

##Figure 3 (Gene expression) ----

setwd("/Users/mallott/Dropbox/Projects/Gut_microbiome/mouse_inoculation/RNAseq")

library(tidyverse)
library(ggpubr)

genes = read_csv("transformed_data_to_plot2.csv")
metadata = read_csv("Gene_expression_metadata_update_072722_noinfant.csv", col_types = "ccffff")

genes_long = genes %>% pivot_longer(-ensgene, names_to = "sampleid") 

bg_wmeta = genes_long %>% left_join(metadata)

bg_wmeta = bg_wmeta %>% filter(Group != "NA")

bg_wmeta = bg_wmeta %>% 
  mutate(Group = case_when(Group == "HumanAdult" ~ "Human",
                           Group == "MacaqueAdult" ~ "Macaque",
                           Group == "Squirrel_monkeyAdult" ~ "Squirrel Monkey"))

bg_wmeta$Group<-factor(bg_wmeta$Group,levels=
                            c("Human",
                              "Squirrel Monkey",
                              "Macaque"))

bg2_wmeta_filter1 = bg_wmeta %>% filter(ensgene == "ENSMUSG00000024029")
bg2_wmeta_filter2 = bg_wmeta %>% filter(ensgene == "ENSMUSG00000039533") 
bg2_wmeta_filter3 = bg_wmeta %>% filter(ensgene == "ENSMUSG00000095061")
bg2_wmeta_filter4 = bg_wmeta %>% filter(ensgene == "ENSMUSG00000023044")
bg2_wmeta_filter5 = bg_wmeta %>% filter(ensgene == "ENSMUSG00000027533")
bg2_wmeta_filter6 = bg_wmeta %>% filter(ensgene == "ENSMUSG00000032418")
bg2_wmeta_filter7 = bg_wmeta %>% filter(ensgene == "ENSMUSG00000025153")
bg2_wmeta_filter8 = bg_wmeta %>% filter(ensgene == "ENSMUSG00000041220")
bg2_wmeta_filter9 = bg_wmeta %>% filter(ensgene == "ENSMUSG00000028051") 
bg2_wmeta_filter10 = bg_wmeta %>% filter(ensgene == "ENSMUSG00000094793")

bg1a = ggboxplot(bg2_wmeta_filter1, x = "Group", y = "value",             
                  add = "jitter", color = "Group", fill = "BG",
                  ylab = "Tff3\nExpression") +
  scale_color_manual(values = c("Human" = "#1f78b4", 
                                "Macaque" = "#e31a1c", 
                                "Squirrel Monkey" = "#33a02c")) +
  scale_fill_manual(values = c("RPBG" = "lightgrey", "SPBG" = "white"))
bg1a = ggpar(bg1a, legend = "right") + 
  rremove("xlab") + rremove("x.text") + rremove("legend.title") +
  annotate(geom = "text", label = "p = 0.023", x = Inf, y = Inf,
           hjust = 1, vjust = 1)

bg2a = ggboxplot(bg2_wmeta_filter2, x = "Group", y = "value",             
                 add = "jitter", color = "Group", fill = "BG",
                 ylab = "Mmd2\nExpression") +
  scale_color_manual(values = c("Human" = "#1f78b4", 
                                "Macaque" = "#e31a1c", 
                                "Squirrel Monkey" = "#33a02c")) +
  scale_fill_manual(values = c("RPBG" = "lightgrey", "SPBG" = "white"))
bg2a = ggpar(bg2a, legend = "right") + 
  rremove("xlab") + rremove("x.text") + rremove("legend.title") +
  annotate(geom = "text", label = "p = 0.013", x = Inf, y = Inf,
           hjust = 1, vjust = 1)

bg3a = ggboxplot(bg2_wmeta_filter3, x = "Group", y = "value",             
                 add = "jitter", color = "Group", fill = "BG",
                 ylab = "E030018B13Rik\nExpression") +
  scale_color_manual(values = c("Human" = "#1f78b4", 
                                "Macaque" = "#e31a1c", 
                                "Squirrel Monkey" = "#33a02c")) +
  scale_fill_manual(values = c("RPBG" = "lightgrey", "SPBG" = "white"))
bg3a = ggpar(bg3a, legend = "right") + 
  rremove("xlab") + rremove("x.text") + rremove("legend.title") +
  annotate(geom = "text", label = "p = 0.004", x = Inf, y = Inf,
           hjust = 1, vjust = 1)

bg4a = ggboxplot(bg2_wmeta_filter4, x = "Group", y = "value",             
                 add = "jitter", color = "Group", fill = "BG",
                 ylab = "Csad\nExpression") +
  scale_color_manual(values = c("Human" = "#1f78b4", 
                                "Macaque" = "#e31a1c", 
                                "Squirrel Monkey" = "#33a02c")) +
  scale_fill_manual(values = c("RPBG" = "lightgrey", "SPBG" = "white"))
bg4a = ggpar(bg4a, legend = "right") + 
  rremove("xlab") + rremove("x.text") + rremove("legend.title") +
  annotate(geom = "text", label = "p < 0.001", x = Inf, y = Inf,
           hjust = 1, vjust = 1)

bg5a = ggboxplot(bg2_wmeta_filter5, x = "Group", y = "value",             
                 add = "jitter", color = "Group", fill = "BG",
                 ylab = "Fabp5\nExpression") +
  scale_color_manual(values = c("Human" = "#1f78b4", 
                                "Macaque" = "#e31a1c", 
                                "Squirrel Monkey" = "#33a02c")) +
  scale_fill_manual(values = c("RPBG" = "lightgrey", "SPBG" = "white"))
bg5a = ggpar(bg5a, legend = "right") + 
  rremove("xlab") + rremove("x.text") + rremove("legend.title") +
  annotate(geom = "text", label = "p < 0.001", x = Inf, y = Inf,
           hjust = 1, vjust = 1)

bg6a = ggboxplot(bg2_wmeta_filter6, x = "Group", y = "value",             
                 add = "jitter", color = "Group", fill = "BG",
                 ylab = "Me1\nExpression") +
  scale_color_manual(values = c("Human" = "#1f78b4", 
                                "Macaque" = "#e31a1c", 
                                "Squirrel Monkey" = "#33a02c")) +
  scale_fill_manual(values = c("RPBG" = "lightgrey", "SPBG" = "white"))
bg6a = ggpar(bg6a, legend = "right") + 
  rremove("xlab") + rremove("x.text") + rremove("legend.title") +
  annotate(geom = "text", label = "p = 0.003", x = Inf, y = Inf,
           hjust = 1, vjust = 1)

bg7a = ggboxplot(bg2_wmeta_filter7, x = "Group", y = "value",             
                 add = "jitter", color = "Group", fill = "BG",
                 ylab = "Fasn\nExpression") +
  scale_color_manual(values = c("Human" = "#1f78b4", 
                                "Macaque" = "#e31a1c", 
                                "Squirrel Monkey" = "#33a02c")) +
  scale_fill_manual(values = c("RPBG" = "lightgrey", "SPBG" = "white"))
bg7a = ggpar(bg7a, legend = "right") + 
  rremove("xlab") + rremove("x.text") + rremove("legend.title") +
  annotate(geom = "text", label = "p < 0.001", x = Inf, y = Inf,
           hjust = 1, vjust = 1)

bg8a = ggboxplot(bg2_wmeta_filter8, x = "Group", y = "value",             
                 add = "jitter", color = "Group", fill = "BG",
                 ylab = "Elovl6\nExpression") +
  scale_color_manual(values = c("Human" = "#1f78b4", 
                                "Macaque" = "#e31a1c", 
                                "Squirrel Monkey" = "#33a02c")) +
  scale_fill_manual(values = c("RPBG" = "lightgrey", "SPBG" = "white"))
bg8a = ggpar(bg8a, legend = "right") + 
  rremove("xlab") + rremove("x.text") + rremove("legend.title") +
  annotate(geom = "text", label = "p = 0.011", x = Inf, y = Inf,
           hjust = 1, vjust = 1)

bg9a = ggboxplot(bg2_wmeta_filter9, x = "Group", y = "value",             
                 add = "jitter", color = "Group", fill = "BG",
                 ylab = "Hcn3\nExpression") +
  scale_color_manual(values = c("Human" = "#1f78b4", 
                                "Macaque" = "#e31a1c", 
                                "Squirrel Monkey" = "#33a02c")) +
  scale_fill_manual(values = c("RPBG" = "lightgrey", "SPBG" = "white"))
bg9a = ggpar(bg9a, legend = "right") + 
  rremove("xlab") + rremove("x.text") + rremove("legend.title") +
  annotate(geom = "text", label = "p = 0.011", x = Inf, y = Inf,
           hjust = 1, vjust = 1)

bg10a = ggboxplot(bg2_wmeta_filter10, x = "Group", y = "value",             
                 add = "jitter", color = "Group", fill = "BG",
                 ylab = "Mup12\nExpression") +
  scale_color_manual(values = c("Human" = "#1f78b4", 
                                "Macaque" = "#e31a1c", 
                                "Squirrel Monkey" = "#33a02c")) +
  scale_fill_manual(values = c("RPBG" = "lightgrey", "SPBG" = "white"))
bg10a = ggpar(bg10a, legend = "right") + 
  rremove("xlab") + rremove("x.text") + rremove("legend.title") +
  annotate(geom = "text", label = "p < 0.001", x = Inf, y = Inf,
           hjust = 1, vjust = 1)

setwd("/Users/mallott/Dropbox/Projects/Gut_microbiome/mouse_inoculation/")

tiff(file="Fig3.tif", res=300, width=18, height=6, units="in")
ggarrange(bg1a, bg2a, bg3a, bg4a, bg5a, bg6a, bg7a, 
          bg8a, bg9a, bg10a, 
          ncol = 5, nrow=2, common.legend = TRUE, legend = "right", align = "hv")
dev.off()

setEPS()
postscript(file="Fig3.eps", width=18, height=6, paper = "special")
ggarrange(bg1a, bg2a, bg3a, bg4a, bg5a, bg6a, bg7a, 
          bg8a, bg9a, bg10a, 
          ncol = 5, nrow=2, common.legend = TRUE, legend = "right", align = "hv")
dev.off()

#Supplemental figure 1 ----

##Experimental design

#Supplemental figure 2 ----

##Inoculation is real (distances)

setwd("/Users/elizabethmallott/Dropbox/Projects/Gut_microbiome/mouse_inoculation/16S/All")

library(tidyverse)
library(ggpubr)

und = read_csv("unweighted_distance_noinfant.csv", col_types = "cccdcffffffffcffffffffffff")
wed = read_csv("weighted_distance_noinfant.csv", col_types = "cccdcffffffffcffffffffffff")

und2 = und %>% mutate(SamplePairNew3 = 
                        case_when(SamplePairNew2 == "SQM_adult_stock_SQM_adult_first" ~ 
                          "SQM adult first vs. SQM adult stock",
                          SamplePairNew2 == "SQM_adult_stock_macaque_adult_first" ~ 
                            "Macaque adult first vs. SQM adult stock",
                          SamplePairNew2 == "SQM_adult_stock_human_adult_first" ~ 
                            "Human adult first vs. SQM adult stock",
                          SamplePairNew2 == "human_adult_stock_human_adult_first" ~ 
                            "Human adult first vs. human adult stock",
                          SamplePairNew2 == "human_adult_stock_macaque_adult_first" ~ 
                            "Macaque adult first vs. human adult stock",
                          SamplePairNew2 == "human_adult_stock_SQM_adult_first" ~ 
                            "SQM adult first vs. human adult stock",
                          SamplePairNew2 == "macaque_adult_stock_macaque_adult_first" ~ 
                            "Macaque adult first vs. macaque adult stock",
                          SamplePairNew2 == "macaque_adult_stock_human_adult_first" ~ 
                            "Human adult first vs. macaque adult stock",
                          SamplePairNew2 == "macaque_adult_stock_SQM_adult_first" ~ 
                            "SQM adult first vs. macaque adult stock",
                          SamplePairNew2 == "SQM_infant_stock_SQM_infant_first" ~ 
                            "SQM infant first vs. SQM infant stock",
                          SamplePairNew2 == "SQM_infant_stock_macaque_infant_first" ~ 
                            "Macaque infant first vs. SQM infant stock",
                          SamplePairNew2 == "SQM_infant_stock_human_infant_first" ~ 
                            "Human infant first vs. SQM infant stock",
                          SamplePairNew2 == "human_infant_stock_human_infant_first" ~ 
                            "Human infant first vs. human infant stock",
                          SamplePairNew2 == "human_infant_stock_macaque_infant_first" ~ 
                            "Macaque infant first vs. human infant stock",
                          SamplePairNew2 == "human_infant_stock_SQM_infant_first" ~ 
                            "SQM infant first vs. human infant stock",
                          SamplePairNew2 == "macaque_infant_stock_macaque_infant_first" ~ 
                            "Macaque infant first vs. macaque infant stock",
                          SamplePairNew2 == "macaque_infant_stock_human_infant_first" ~ 
                            "Human infant first vs. macaque infant stock",
                          SamplePairNew2 == "macaque_infant_stock_SQM_infant_first" ~ 
                            "SQM infant first vs. macaque infant stock",
                          SamplePairNew2 == "SQM_adult_stock_SQM_adult_last" ~ 
                            "SQM adult last vs. SQM adult stock",
                          SamplePairNew2 == "SQM_adult_stock_macaque_adult_last" ~ 
                            "Macaque adult last vs. SQM adult stock",
                          SamplePairNew2 == "SQM_adult_stock_human_adult_last" ~ 
                            "Human adult last vs. SQM adult stock",
                          SamplePairNew2 == "human_adult_stock_human_adult_last" ~ 
                            "Human adult last vs. human adult stock",
                          SamplePairNew2 == "human_adult_stock_macaque_adult_last" ~ 
                            "Macaque adult last vs. human adult stock",
                          SamplePairNew2 == "human_adult_stock_SQM_adult_last" ~ 
                            "SQM adult last vs. human adult stock",
                          SamplePairNew2 == "macaque_adult_stock_macaque_adult_last" ~ 
                            "Macaque adult last vs. macaque adult stock",
                          SamplePairNew2 == "macaque_adult_stock_human_adult_last" ~ 
                            "Human adult last vs. macaque adult stock",
                          SamplePairNew2 == "macaque_adult_stock_SQM_adult_last" ~ 
                            "SQM adult last vs. macaque adult stock",
                          SamplePairNew2 == "SQM_infant_stock_SQM_infant_last" ~ 
                            "SQM infant last vs. SQM infant stock",
                          SamplePairNew2 == "SQM_infant_stock_macaque_infant_last" ~ 
                            "Macaque infant last vs. SQM infant stock",
                          SamplePairNew2 == "SQM_infant_stock_human_infant_last" ~ 
                            "Human infant last vs. SQM infant stock",
                          SamplePairNew2 == "human_infant_stock_human_infant_last" ~ 
                            "Human infant last vs. human infant stock",
                          SamplePairNew2 == "human_infant_stock_macaque_infant_last" ~ 
                            "Macaque infant last vs. human infant stock",
                          SamplePairNew2 == "human_infant_stock_SQM_infant_last" ~ 
                            "SQM infant last vs. human infant stock",
                          SamplePairNew2 == "macaque_infant_stock_macaque_infant_last" ~ 
                            "Macaque infant last vs. macaque infant stock",
                          SamplePairNew2 == "macaque_infant_stock_human_infant_last" ~ 
                            "Human infant last vs. macaque infant stock",
                          SamplePairNew2 == "macaque_infant_stock_SQM_infant_last" ~ 
                            "SQM infant last vs. macaque infant stock"))

wed2 = wed %>% mutate(SamplePairNew3 = 
                        case_when(SamplePairNew2 == "SQM_adult_stock_SQM_adult_first" ~ 
                                    "SQM adult first vs. SQM adult stock",
                                  SamplePairNew2 == "SQM_adult_stock_macaque_adult_first" ~ 
                                    "Macaque adult first vs. SQM adult stock",
                                  SamplePairNew2 == "SQM_adult_stock_human_adult_first" ~ 
                                    "Human adult first vs. SQM adult stock",
                                  SamplePairNew2 == "human_adult_stock_human_adult_first" ~ 
                                    "Human adult first vs. human adult stock",
                                  SamplePairNew2 == "human_adult_stock_macaque_adult_first" ~ 
                                    "Macaque adult first vs. human adult stock",
                                  SamplePairNew2 == "human_adult_stock_SQM_adult_first" ~ 
                                    "SQM adult first vs. human adult stock",
                                  SamplePairNew2 == "macaque_adult_stock_macaque_adult_first" ~ 
                                    "Macaque adult first vs. macaque adult stock",
                                  SamplePairNew2 == "macaque_adult_stock_human_adult_first" ~ 
                                    "Human adult first vs. macaque adult stock",
                                  SamplePairNew2 == "macaque_adult_stock_SQM_adult_first" ~ 
                                    "SQM adult first vs. macaque adult stock",
                                  SamplePairNew2 == "SQM_infant_stock_SQM_infant_first" ~ 
                                    "SQM infant first vs. SQM infant stock",
                                  SamplePairNew2 == "SQM_infant_stock_macaque_infant_first" ~ 
                                    "Macaque infant first vs. SQM infant stock",
                                  SamplePairNew2 == "SQM_infant_stock_human_infant_first" ~ 
                                    "Human infant first vs. SQM infant stock",
                                  SamplePairNew2 == "human_infant_stock_human_infant_first" ~ 
                                    "Human infant first vs. human infant stock",
                                  SamplePairNew2 == "human_infant_stock_macaque_infant_first" ~ 
                                    "Macaque infant first vs. human infant stock",
                                  SamplePairNew2 == "human_infant_stock_SQM_infant_first" ~ 
                                    "SQM infant first vs. human infant stock",
                                  SamplePairNew2 == "macaque_infant_stock_macaque_infant_first" ~ 
                                    "Macaque infant first vs. macaque infant stock",
                                  SamplePairNew2 == "macaque_infant_stock_human_infant_first" ~ 
                                    "Human infant first vs. macaque infant stock",
                                  SamplePairNew2 == "macaque_infant_stock_SQM_infant_first" ~ 
                                    "SQM infant first vs. macaque infant stock",
                                  SamplePairNew2 == "SQM_adult_stock_SQM_adult_last" ~ 
                                    "SQM adult last vs. SQM adult stock",
                                  SamplePairNew2 == "SQM_adult_stock_macaque_adult_last" ~ 
                                    "Macaque adult last vs. SQM adult stock",
                                  SamplePairNew2 == "SQM_adult_stock_human_adult_last" ~ 
                                    "Human adult last vs. SQM adult stock",
                                  SamplePairNew2 == "human_adult_stock_human_adult_last" ~ 
                                    "Human adult last vs. human adult stock",
                                  SamplePairNew2 == "human_adult_stock_macaque_adult_last" ~ 
                                    "Macaque adult last vs. human adult stock",
                                  SamplePairNew2 == "human_adult_stock_SQM_adult_last" ~ 
                                    "SQM adult last vs. human adult stock",
                                  SamplePairNew2 == "macaque_adult_stock_macaque_adult_last" ~ 
                                    "Macaque adult last vs. macaque adult stock",
                                  SamplePairNew2 == "macaque_adult_stock_human_adult_last" ~ 
                                    "Human adult last vs. macaque adult stock",
                                  SamplePairNew2 == "macaque_adult_stock_SQM_adult_last" ~ 
                                    "SQM adult last vs. macaque adult stock",
                                  SamplePairNew2 == "SQM_infant_stock_SQM_infant_last" ~ 
                                    "SQM infant last vs. SQM infant stock",
                                  SamplePairNew2 == "SQM_infant_stock_macaque_infant_last" ~ 
                                    "Macaque infant last vs. SQM infant stock",
                                  SamplePairNew2 == "SQM_infant_stock_human_infant_last" ~ 
                                    "Human infant last vs. SQM infant stock",
                                  SamplePairNew2 == "human_infant_stock_human_infant_last" ~ 
                                    "Human infant last vs. human infant stock",
                                  SamplePairNew2 == "human_infant_stock_macaque_infant_last" ~ 
                                    "Macaque infant last vs. human infant stock",
                                  SamplePairNew2 == "human_infant_stock_SQM_infant_last" ~ 
                                    "SQM infant last vs. human infant stock",
                                  SamplePairNew2 == "macaque_infant_stock_macaque_infant_last" ~ 
                                    "Macaque infant last vs. macaque infant stock",
                                  SamplePairNew2 == "macaque_infant_stock_human_infant_last" ~ 
                                    "Human infant last vs. macaque infant stock",
                                  SamplePairNew2 == "macaque_infant_stock_SQM_infant_last" ~ 
                                    "SQM infant last vs. macaque infant stock"))

und_stf = und2 %>% filter(SamplePairNew3 == "SQM adult first vs. SQM adult stock" |
                           SamplePairNew3 == "Macaque adult first vs. SQM adult stock" |
                           SamplePairNew3 == "Human adult first vs. SQM adult stock" | 
                           SamplePairNew3 == "Human adult first vs. human adult stock" |
                           SamplePairNew3 == "Macaque adult first vs. human adult stock" |
                           SamplePairNew3 == "SQM adult first vs. human adult stock" |
                           SamplePairNew3 == "Human adult first vs. macaque adult stock" |
                           SamplePairNew3 == "SQM adult first vs. macaque adult stock" |
                           SamplePairNew3 == "Macaque adult first vs. macaque adult stock" |
                           SamplePairNew3 == "SQM infant first vs. SQM infant stock" |
                           SamplePairNew3 == "Macaque infant first vs. SQM infant stock" |
                           SamplePairNew3 == "Human infant first vs. SQM infant stock" |
                           SamplePairNew3 == "Human infant first vs. human infant stock" |
                           SamplePairNew3 == "Macaque infant first vs. human infant stock" |
                           SamplePairNew3 == "SQM infant first vs. human infant stock" |
                           SamplePairNew3 == "Macaque infant first vs. macaque infant stock" |
                           SamplePairNew3 == "Human infant first vs. macaque infant stock" |
                           SamplePairNew3 == "SQM infant first vs. macaque infant stock")

und_sts  = und2 %>% filter(SamplePairNew3 == "SQM adult last vs. SQM adult stock" |
                             SamplePairNew3 == "Macaque adult last vs. SQM adult stock" |
                             SamplePairNew3 == "Human adult last vs. SQM adult stock" | 
                             SamplePairNew3 == "Human adult last vs. human adult stock" |
                             SamplePairNew3 == "Macaque adult last vs. human adult stock" |
                             SamplePairNew3 == "SQM adult last vs. human adult stock" |
                             SamplePairNew3 == "Human adult last vs. macaque adult stock" |
                             SamplePairNew3 == "SQM adult last vs. macaque adult stock" |
                             SamplePairNew3 == "Macaque adult last vs. macaque adult stock" |
                             SamplePairNew3 == "SQM infant last vs. SQM infant stock" |
                             SamplePairNew3 == "Macaque infant last vs. SQM infant stock" |
                             SamplePairNew3 == "Human infant last vs. SQM infant stock" |
                             SamplePairNew3 == "Human infant last vs. human infant stock" |
                             SamplePairNew3 == "Macaque infant last vs. human infant stock" |
                             SamplePairNew3 == "SQM infant last vs. human infant stock" |
                             SamplePairNew3 == "Macaque infant last vs. macaque infant stock" |
                             SamplePairNew3 == "Human infant last vs. macaque infant stock" |
                             SamplePairNew3 == "SQM infant last vs. macaque infant stock")

wed_stf = wed2 %>% filter(SamplePairNew3 == "SQM adult first vs. SQM adult stock" |
                            SamplePairNew3 == "Macaque adult first vs. SQM adult stock" |
                            SamplePairNew3 == "Human adult first vs. SQM adult stock" | 
                            SamplePairNew3 == "Human adult first vs. human adult stock" |
                            SamplePairNew3 == "Macaque adult first vs. human adult stock" |
                            SamplePairNew3 == "SQM adult first vs. human adult stock" |
                            SamplePairNew3 == "Human adult first vs. macaque adult stock" |
                            SamplePairNew3 == "SQM adult first vs. macaque adult stock" |
                            SamplePairNew3 == "Macaque adult first vs. macaque adult stock" |
                            SamplePairNew3 == "SQM infant first vs. SQM infant stock" |
                            SamplePairNew3 == "Macaque infant first vs. SQM infant stock" |
                            SamplePairNew3 == "Human infant first vs. SQM infant stock" |
                            SamplePairNew3 == "Human infant first vs. human infant stock" |
                            SamplePairNew3 == "Macaque infant first vs. human infant stock" |
                            SamplePairNew3 == "SQM infant first vs. human infant stock" |
                            SamplePairNew3 == "Macaque infant first vs. macaque infant stock" |
                            SamplePairNew3 == "Human infant first vs. macaque infant stock" |
                            SamplePairNew3 == "SQM infant first vs. macaque infant stock")

wed_sts  = wed2 %>% filter(SamplePairNew3 == "SQM adult last vs. SQM adult stock" |
                             SamplePairNew3 == "Macaque adult last vs. SQM adult stock" |
                             SamplePairNew3 == "Human adult last vs. SQM adult stock" | 
                             SamplePairNew3 == "Human adult last vs. human adult stock" |
                             SamplePairNew3 == "Macaque adult last vs. human adult stock" |
                             SamplePairNew3 == "SQM adult last vs. human adult stock" |
                             SamplePairNew3 == "Human adult last vs. macaque adult stock" |
                             SamplePairNew3 == "SQM adult last vs. macaque adult stock" |
                             SamplePairNew3 == "Macaque adult last vs. macaque adult stock" |
                             SamplePairNew3 == "SQM infant last vs. SQM infant stock" |
                             SamplePairNew3 == "Macaque infant last vs. SQM infant stock" |
                             SamplePairNew3 == "Human infant last vs. SQM infant stock" |
                             SamplePairNew3 == "Human infant last vs. human infant stock" |
                             SamplePairNew3 == "Macaque infant last vs. human infant stock" |
                             SamplePairNew3 == "SQM infant last vs. human infant stock" |
                             SamplePairNew3 == "Macaque infant last vs. macaque infant stock" |
                             SamplePairNew3 == "Human infant last vs. macaque infant stock" |
                             SamplePairNew3 == "SQM infant last vs. macaque infant stock")

und_stf$SamplePairNew3 = factor(und_stf$SamplePairNew3, levels = c("Human adult first vs. human adult stock",
                                                                   "Human adult first vs. macaque adult stock",
                                                                   "Human adult first vs. SQM adult stock",
                                                                   "SQM adult first vs. SQM adult stock",
                                                                   "SQM adult first vs. human adult stock",
                                                                   "SQM adult first vs. macaque adult stock",
                                                                   "Macaque adult first vs. macaque adult stock",
                                                                   "Macaque adult first vs. human adult stock",
                                                                   "Macaque adult first vs. SQM adult stock",
                                                                   "SQM infant first vs. SQM infant stock",
                                                                   "SQM infant first vs. human infant stock",
                                                                   "SQM infant first vs. macaque infant stock",
                                                                   "Human infant first vs. human infant stock",
                                                                   "Human infant first vs. macaque infant stock",
                                                                   "Human infant first vs. SQM infant stock",
                                                                   "Macaque infant first vs. macaque infant stock",
                                                                   "Macaque infant first vs. human infant stock",
                                                                   "Macaque infant first vs. SQM infant stock"))

und_sts$SamplePairNew3 = factor(und_sts$SamplePairNew3, levels = c("Human adult last vs. human adult stock",
                                                                   "Human adult last vs. macaque adult stock",
                                                                   "Human adult last vs. SQM adult stock",
                                                                   "SQM adult last vs. SQM adult stock",
                                                                   "SQM adult last vs. human adult stock",
                                                                   "SQM adult last vs. macaque adult stock",
                                                                   "Macaque adult last vs. macaque adult stock",
                                                                   "Macaque adult last vs. human adult stock",
                                                                   "Macaque adult last vs. SQM adult stock",
                                                                   "SQM infant last vs. SQM infant stock",
                                                                   "SQM infant last vs. human infant stock",
                                                                   "SQM infant last vs. macaque infant stock",
                                                                   "Human infant last vs. human infant stock",
                                                                   "Human infant last vs. macaque infant stock",
                                                                   "Human infant last vs. SQM infant stock",
                                                                   "Macaque infant last vs. macaque infant stock",
                                                                   "Macaque infant last vs. human infant stock",
                                                                   "Macaque infant last vs. SQM infant stock"))

wed_stf$SamplePairNew3 = factor(wed_stf$SamplePairNew3, levels = c("Human adult first vs. human adult stock",
                                                                   "Human adult first vs. macaque adult stock",
                                                                   "Human adult first vs. SQM adult stock",
                                                                   "SQM adult first vs. SQM adult stock",
                                                                   "SQM adult first vs. human adult stock",
                                                                   "SQM adult first vs. macaque adult stock",
                                                                   "Macaque adult first vs. macaque adult stock",
                                                                   "Macaque adult first vs. human adult stock",
                                                                   "Macaque adult first vs. SQM adult stock",
                                                                   "SQM infant first vs. SQM infant stock",
                                                                   "SQM infant first vs. human infant stock",
                                                                   "SQM infant first vs. macaque infant stock",
                                                                   "Human infant first vs. human infant stock",
                                                                   "Human infant first vs. macaque infant stock",
                                                                   "Human infant first vs. SQM infant stock",
                                                                   "Macaque infant first vs. macaque infant stock",
                                                                   "Macaque infant first vs. human infant stock",
                                                                   "Macaque infant first vs. SQM infant stock"))

wed_sts$SamplePairNew3 = factor(wed_sts$SamplePairNew3, levels = c("Human adult last vs. human adult stock",
                                                                   "Human adult last vs. macaque adult stock",
                                                                   "Human adult last vs. SQM adult stock",
                                                                   "SQM adult last vs. SQM adult stock",
                                                                   "SQM adult last vs. human adult stock",
                                                                   "SQM adult last vs. macaque adult stock",
                                                                   "Macaque adult last vs. macaque adult stock",
                                                                   "Macaque adult last vs. human adult stock",
                                                                   "Macaque adult last vs. SQM adult stock",
                                                                   "SQM infant last vs. SQM infant stock",
                                                                   "SQM infant last vs. human infant stock",
                                                                   "SQM infant last vs. macaque infant stock",
                                                                   "Human infant last vs. human infant stock",
                                                                   "Human infant last vs. macaque infant stock",
                                                                   "Human infant last vs. SQM infant stock",
                                                                   "Macaque infant last vs. macaque infant stock",
                                                                   "Macaque infant last vs. human infant stock",
                                                                   "Macaque infant last vs. SQM infant stock"))


und_stf_plot = ggerrorplot(und_stf, x = "SamplePairNew3", y = "distance", 
                           xlab = "Treatment pair", rotate = TRUE, desc_stat = "median_iqr",
                         ylab = "Pairwise Unweighted UniFrac Distances", add = "jitter",
                         add.params = list(color = "grey35"), 
                         title = "Stock vs. First Timepoint") + 
  font("title", size = 18, face = "bold") + font("xlab", size = 14) + font("ylab", size = 14)

und_sts_plot = ggerrorplot(und_sts, x = "SamplePairNew3", y = "distance", 
                           xlab = "Treatment pair", rotate = TRUE, desc_stat = "median_iqr",
                           ylab = "Pairwise Unweighted UniFrac Distances", add = "jitter",
                           add.params = list(color = "grey35"), 
                           title = "Stock vs. Last Timepoint") + 
  font("title", size = 18, face = "bold") + font("xlab", size = 14) + font("ylab", size = 14)

wed_stf_plot = ggerrorplot(wed_stf, x = "SamplePairNew3", y = "distance", 
                           xlab = "Treatment pair", rotate = TRUE, desc_stat = "median_iqr",
                           ylab = "Pairwise Weighted UniFrac Distances", add = "jitter",
                           add.params = list(color = "grey35")) + 
  font("xlab", size = 14) + font("ylab", size = 14)

wed_sts_plot = ggerrorplot(wed_sts, x = "SamplePairNew3", y = "distance", 
                           xlab = "Treatment pair", rotate = TRUE, desc_stat = "median_iqr",
                           ylab = "Pairwise Weighted UniFrac Distances", add = "jitter",
                           add.params = list(color = "grey35")) + 
  font("xlab", size = 14) + font("ylab", size = 14)

setwd("/Users/elizabethmallott/Dropbox/Projects/Gut_microbiome/mouse_inoculation/")

tiff(file="Supplementary_Fig2_noinfant.tif", res=300, width=16, height=14, units="in")
ggarrange(und_stf_plot, und_sts_plot, wed_stf_plot, wed_sts_plot, 
          ncol = 2, nrow = 2, align = "hv", labels = c("A", "B", "C", "D"))
dev.off()

#Supplemental figure 3 ----
##Unweighted and weighted UniFrac distances for stocks, first treatment timepoint, and last treatment timepoint

#Supplemental figure 5 ----
##Unweighted and weighted UniFrac distances for last treatment timepoint only

#Supplemental figure 6 ----
##NMDS plots for metagenomic data

#Supplemental figure 3 ----

##Mouse physiology (everything not in figure 1)

setwd("/Users/elizabethmallott/Dropbox/Projects/Gut_microbiome/mouse_inoculation/phys_data")

library(tidyverse)
library(ggpubr)

phys = read_csv("Phys_combined_update_072722_noinfant.csv")

physnew = phys %>% 
  mutate(Treatment = case_when(Treatment == "human_Adult" ~ "Human",
                               Treatment == "macaque_Adult" ~ "Macaque",
                               Treatment == "squirrel_monkey_Adult" ~ "Squirrel Monkey"))

physnew$Treatment<-factor(physnew$Treatment,levels=
                            c("Human",
                              "Squirrel Monkey",
                              "Macaque"))

weight = ggboxplot(physnew, x = "Treatment", 
                   y = "Body_Weight", color = "Treatment", fill = "BG",
                   add = "jitter", ylab = "Body weight (gm)") +
  scale_color_manual(values = c("Human" = "#1f78b4",
                                "Macaque" = "#e31a1c", 
                                "Squirrel Monkey" = "#33a02c")) +
  scale_fill_manual(values = c("RPBG" = "lightgrey", "SPBG" = "white"))
weight = ggpar(weight, legend = "right") + 
  rremove("xlab") + rremove("x.text") + 
  rremove("legend.title") +
  annotate(geom = "text", label = "LME: p > 0.05", x = Inf, y = Inf,
           hjust = 1, vjust = 1)

setwd("/Users/elizabethmallott/Dropbox/Projects/Gut_microbiome/mouse_inoculation/")

tiff(file="Supplementary_Fig3_noinfant.tif", res=300, width=5, height=4, units="in")
ggarrange(weight, ncol = 1, nrow=1, 
          common.legend = TRUE, legend = "right")
dev.off()

#Supplemental figure 4-11 ----

##16S NMDS plots for unweighted/weighted

setwd("/Users/elizabethmallott/Dropbox/Projects/Gut_microbiome/mouse_inoculation/16S/All")

library(ggplot2)
library(vegan)
library(cowplot)

unweighted = as.dist(read.table("unweighted-distance-matrix-noinfant.tsv", header = T))
weighted = as.dist(read.table("weighted-distance-matrix-noinfant.tsv", header = T))
metadata = read.csv("mapping_all_r_update072722_noinfant.csv", header=T)

metadatanew = metadata %>% 
  mutate(StockAge = case_when(StockAge == "human_adult" ~ "Human",
                              StockAge == "macaque_adult" ~ "Macaque",
                              StockAge == "SQM_adult" ~ "Squirrel Monkey"))

metadatanew$StockAge<-factor(metadatanew$StockAge,levels=
                                c("Macaque", "Human",
                                  "Squirrel Monkey"))

mds_otus_weighted<-metaMDS(weighted, k=2, trymax=500)
mds_otus_weighted_points<-mds_otus_weighted$points
mds_otus_weighted_points2<-merge(x=mds_otus_weighted_points, y = metadatanew, 
                                 by.x = "row.names", by.y = "SampleID")
nmds1v2w <- ggplot(mds_otus_weighted_points2, aes(x = MDS1, y = MDS2, 
                                                 color = StockAge, shape = Timepoint)) +
  geom_point(size=3) + scale_color_brewer(palette = 'Set1') +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'), 
        legend.key=element_blank()) + 
  theme(axis.title.x=element_text(size=rel(2)), 
        axis.title.y=element_text(size=rel(2)),
        plot.title = element_text(size=rel(3)),
        legend.title = element_text(size=rel(2)),
        legend.text = element_text(size = rel(1.8))) + 
  ggtitle("Weighted UniFrac") 

mds_otus_unweighted<-metaMDS(unweighted, k=2, trymax=500)
mds_otus_unweighted_points<-mds_otus_unweighted$points
mds_otus_unweighted_points2<-merge(x=mds_otus_unweighted_points, y = metadatanew, 
                                   by.x = "row.names", by.y = "SampleID")
nmds1v2u <- ggplot(mds_otus_unweighted_points2, aes(x = MDS1, y = MDS2, 
                                                   color = StockAge, shape = Timepoint)) +
  geom_point(size=3) + scale_color_brewer(palette = 'Set1') +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'), 
        legend.key=element_blank()) + 
  theme(axis.title.x=element_text(size=rel(2)), 
        axis.title.y=element_text(size=rel(2)),
        plot.title = element_text(size=rel(3)),
        legend.title = element_text(size=rel(2)),
        legend.text = element_text(size = rel(1.8))) + 
  ggtitle("Unweighted UniFrac") 

setwd("/Users/elizabethmallott/Dropbox/Projects/Gut_microbiome/mouse_inoculation/")

tiff(file = "Supplementary_Fig4_noinfant.tif", res = 300, width = 16, height = 6, units="in")
legend1 = get_legend(nmds1v2w + theme(legend.box.margin = margin(0, 0, 0, 12)))
col1 = plot_grid(nmds1v2u + theme(legend.position = "none"),
                 nrow = 1, ncol = 1, labels = c('A'), 
                 label_size = 20)
col2 = plot_grid(nmds1v2w + theme(legend.position = "none"), 
                 nrow = 1, ncol = 1, labels = c('B'),
                 label_size = 20)
plot_grid(col1, col2, legend1, 
          nrow = 1, ncol = 3, rel_widths = c(1.5, 1.5, 0.85), align = "h", axis = "t")
dev.off()

#Phyla relative abundance (three timepoints, significant)
##Reanalysis there are none
setwd("/Users/mallott/Dropbox/Projects/Gut_microbiome/mouse_inoculation/16S/All")

library(tidyverse)
library(ggpubr)

metadata_phyla = read.csv("mapping_all_r.csv", header = T)
seqs = read.csv("phyla.csv", header = T)

metadatanew = metadata_phyla %>% 
  mutate(StockAge = case_when(StockAge == "human_adult" ~ "Human Adult",
                              StockAge == "human_infant" ~ "Human Infant",
                              StockAge == "macaque_adult" ~ "Macaque Adult",
                              StockAge == "macaque_infant" ~ "Macaque Infant",
                              StockAge == "SQM_adult" ~ "Squirrel Monkey Adult",
                              StockAge == "SQM_infant" ~ "Squirrel Monkey Infant"))

metadatanew$StockAge<-factor(metadatanew$StockAge,levels=
                               c("Human Adult","Human Infant",
                                 "Squirrel Monkey Adult", "Squirrel Monkey Infant",
                                 "Macaque Adult", "Macaque Infant"))

phyla = inner_join(seqs, metadatanew, by = "SampleID") 

first.phyla = filter(phyla, Timepoint == "first")
last.phyla = filter(phyla, Timepoint == "last")
stock.phyla = filter(phyla, Timepoint == "stock")

phyla = phyla %>% 
  mutate(BG = case_when(BG == "SPBG" ~ "low-EQ",
                        BG == "RPBG" ~ "high-EQ"))

protplot0 = ggboxplot(stock.phyla, x = "StockAge", 
                      y = "Proteobacteria", color = "StockAge", fill = "BG",
                      add = "jitter", ylab = "Relative abundance") +
  scale_color_manual(values = c("Human Adult" = "#a6cee3",
                                "Human Infant" = "#1f78b4", 
                                "Macaque Adult" = "#fb9a99",
                                "Macaque Infant" = "#e31a1c", 
                                "Squirrel Monkey Adult" = "#b2df8a", 
                                "Squirrel Monkey Infant" = "#33a02c")) +
  scale_fill_manual(values = c("RPBG" = "lightgrey", "SPBG" = "white"))
protplot0 = ggpar(protplot0, legend = "right", 
                  title = "Proteobacteria: stock") +
  rremove("xlab") + rremove("x.text") + 
  rremove("legend.title") +
  annotate(geom = "text", label = "LME, stock species: p = 0.01", x = Inf, y = Inf,
           hjust = 1, vjust = 1)

protplot1 = ggboxplot(first.phyla, x = "StockAge", 
                      y = "Proteobacteria", color = "StockAge", fill = "BG", 
                      add = "jitter", ylab = "Relative abundance") +
  scale_color_manual(values = c("Human Adult" = "#a6cee3",
                                "Human Infant" = "#1f78b4", 
                                "Macaque Adult" = "#fb9a99",
                                "Macaque Infant" = "#e31a1c", 
                                "Squirrel Monkey Adult" = "#b2df8a", 
                                "Squirrel Monkey Infant" = "#33a02c")) +
  scale_fill_manual(values = c("RPBG" = "lightgrey", "SPBG" = "white"))
protplot1 = ggpar(protplot1, legend = "right", 
                  title = "Proteobacteria: first time point") + 
  rremove("xlab") + rremove("x.text") + 
  rremove("legend.title") +
  annotate(geom = "text", label = "LME, stock species: p = 0.01", x = Inf, y = Inf,
           hjust = 1, vjust = 1)

protplot2 = ggboxplot(last.phyla, x = "StockAge", 
                      y = "Proteobacteria", color = "StockAge", fill = "BG", 
                      add = "jitter", ylab = "Relative abundance") +
  scale_color_manual(values = c("Human Adult" = "#a6cee3",
                                "Human Infant" = "#1f78b4", 
                                "Macaque Adult" = "#fb9a99",
                                "Macaque Infant" = "#e31a1c", 
                                "Squirrel Monkey Adult" = "#b2df8a", 
                                "Squirrel Monkey Infant" = "#33a02c")) +
  scale_fill_manual(values = c("RPBG" = "lightgrey", "SPBG" = "white"))
protplot2 = ggpar(protplot2, legend = "right", 
                  title = "Proteobacteria: last time point") + 
  rremove("xlab") + rremove("x.text") + 
  rremove("legend.title") +
  annotate(geom = "text", label = "LME, stock species: p = 0.01", x = Inf, y = Inf,
           hjust = 1, vjust = 1)

setwd("/Users/mallott/Dropbox/Projects/Gut_microbiome/mouse_inoculation/")

tiff(file="Supplementary_Fig5.tif", res=300, width=15, height=4, units="in")
ggarrange(protplot0, protplot1, protplot2, ncol = 3, common.legend = TRUE, legend = "right",
          nrow = 1, align = "h")
dev.off()

#Family relative abundance (three timepoints, significant p but not q)

setwd("/Users/elizabethmallott/Dropbox/Projects/Gut_microbiome/mouse_inoculation/16S/All")

library(tidyverse)
library(ggpubr)

family_relab = read.csv("families_noinfant.csv", header = T)
metadata = read.csv("mapping_all_r_update072722_noinfant.csv", header = T)

metadatanew = metadata %>% 
  mutate(StockAge = case_when(StockAge == "human_adult" ~ "Human",
                              StockAge == "macaque_adult" ~ "Macaque",
                              StockAge == "SQM_adult" ~ "Squirrel Monkey"))

metadatanew$StockAge<-factor(metadatanew$StockAge,levels=
                               c("Macaque", "Human",
                                 "Squirrel Monkey"))

family = metadatanew %>% inner_join(family_relab, by = "SampleID") 

first.family = filter(family, Timepoint == "first")
last.family = filter(family, Timepoint == "last")
stock.family = filter(family, Timepoint == "stock")

methanoplot0 = ggboxplot(stock.family, x = "StockAge", 
                      y = "k__Archaea.p__Euryarchaeota.c__Methanobacteria.o__Methanobacteriales.f__Methanobacteriaceae", 
                      color = "StockAge", fill = "BG", 
                      add = "jitter", ylab = "Relative abundance") +
  scale_color_manual(values = c("Human" = "#1f78b4",
                                "Macaque" = "#e31a1c", 
                                "Squirrel Monkey" = "#33a02c")) +
  scale_fill_manual(values = c("RPBG" = "lightgrey", "SPBG" = "white"))
methanoplot0 = ggpar(methanoplot0, legend = "right", 
                  title = "Methanobacteriaceae: stock") +
  rremove("xlab") + rremove("x.text") + 
  rremove("legend.title")

methanoplot1 = ggboxplot(first.family, x = "StockAge", 
                      y = "k__Archaea.p__Euryarchaeota.c__Methanobacteria.o__Methanobacteriales.f__Methanobacteriaceae", 
                      color = "StockAge", fill = "BG", 
                      add = "jitter", ylab = "Relative abundance") +
  scale_color_manual(values = c("Human" = "#1f78b4",
                                "Macaque" = "#e31a1c", 
                                "Squirrel Monkey" = "#33a02c")) +
  scale_fill_manual(values = c("RPBG" = "lightgrey", "SPBG" = "white"))
methanoplot1 = ggpar(methanoplot1, ylim = c(0,0.000125), 
                     legend = "right", 
                  title = "Methanobacteriaceae: first time point") + 
  rremove("xlab") + rremove("x.text") + 
  rremove("legend.title")

methanoplot2 = ggboxplot(last.family, x = "StockAge", 
                      y = "k__Archaea.p__Euryarchaeota.c__Methanobacteria.o__Methanobacteriales.f__Methanobacteriaceae", 
                      color = "StockAge", fill = "BG", 
                      add = "jitter", ylab = "Relative abundance") +
  scale_color_manual(values = c("Human" = "#1f78b4",
                                "Macaque" = "#e31a1c", 
                                "Squirrel Monkey" = "#33a02c")) +
  scale_fill_manual(values = c("RPBG" = "lightgrey", "SPBG" = "white"))
methanoplot2 = ggpar(methanoplot2, ylim = c(0,0.0012),
                     legend = "right", 
                  title = "Methanobacteriaceae: last time point") + 
  rremove("xlab") + rremove("x.text") + 
  rremove("legend.title")

victivaplot0 = ggboxplot(stock.family, x = "StockAge", 
                         y = "k__Bacteria.p__Lentisphaerae.c__.Lentisphaeria..o__Victivallales.f__Victivallaceae", 
                         color = "StockAge", fill = "BG", 
                         add = "jitter", ylab = "Relative abundance") +
  scale_color_manual(values = c("Human" = "#1f78b4",
                                "Macaque" = "#e31a1c", 
                                "Squirrel Monkey" = "#33a02c")) +
  scale_fill_manual(values = c("RPBG" = "lightgrey", "SPBG" = "white"))
victivaplot0 = ggpar(victivaplot0, legend = "right", 
                     title = "Victivallaceae: stock") +
  rremove("xlab") + rremove("x.text") + 
  rremove("legend.title")

victivaplot1 = ggboxplot(first.family, x = "StockAge", 
                         y = "k__Bacteria.p__Lentisphaerae.c__.Lentisphaeria..o__Victivallales.f__Victivallaceae", 
                         color = "StockAge", fill = "BG", 
                         add = "jitter", ylab = "Relative abundance") +
  scale_color_manual(values = c("Human" = "#1f78b4",
                                "Macaque" = "#e31a1c", 
                                "Squirrel Monkey" = "#33a02c")) +
  scale_fill_manual(values = c("RPBG" = "lightgrey", "SPBG" = "white"))
victivaplot1 = ggpar(victivaplot1, ylim = c(0,0.00007),
                     legend = "right", 
                     title = "Victivallaceae: first time point") + 
  rremove("xlab") + rremove("x.text") + 
  rremove("legend.title")

victivaplot2 = ggboxplot(last.family, x = "StockAge", 
                         y = "k__Bacteria.p__Lentisphaerae.c__.Lentisphaeria..o__Victivallales.f__Victivallaceae", 
                         color = "StockAge", fill = "BG", 
                         add = "jitter", ylab = "Relative abundance") +
  scale_color_manual(values = c("Human" = "#1f78b4",
                                "Macaque" = "#e31a1c", 
                                "Squirrel Monkey" = "#33a02c")) +
  scale_fill_manual(values = c("RPBG" = "lightgrey", "SPBG" = "white"))
victivaplot2 = ggpar(victivaplot2, ylim = c(0,0.0001),
                     legend = "right", 
                     title = "Victivallaceae: last time point") + 
  rremove("xlab") + rremove("x.text") + 
  rremove("legend.title")

setwd("/Users/elizabethmallott/Dropbox/Projects/Gut_microbiome/mouse_inoculation/")

tiff(file="Supplementary_Fig6_noinfant.tif", res=300, width=15, height=8, units="in")
ggarrange(methanoplot0, methanoplot1, methanoplot2,
          victivaplot0, victivaplot1, victivaplot2,
          ncol = 3, common.legend = TRUE, legend = "right",
          nrow = 2, align = "hv")
dev.off()

#Genus relative abundance (three timepoints, significant p but not q)

setwd("/Users/elizabethmallott/Dropbox/Projects/Gut_microbiome/mouse_inoculation/16S/All")

library(tidyverse)
library(ggpubr)

genus_relab = read.csv("genera_noinfant.csv", header = T)
metadata = read.csv("mapping_all_r_update072722_noinfant.csv", header = T)

metadatanew = metadata %>% 
  mutate(StockAge = case_when(StockAge == "human_adult" ~ "Human",
                              StockAge == "macaque_adult" ~ "Macaque",
                              StockAge == "SQM_adult" ~ "Squirrel Monkey"))

metadatanew$StockAge<-factor(metadatanew$StockAge,levels=
                               c("Macaque", "Human",
                                 "Squirrel Monkey"))

genus = metadatanew %>% inner_join(genus_relab, by = "SampleID") 

first.genus = filter(genus, Timepoint == "first")
last.genus = filter(genus, Timepoint == "last")
stock.genus = filter(genus, Timepoint == "stock")

bifidoplot0 = ggboxplot(stock.genus, x = "StockAge", 
                        y = "k__Bacteria.p__Actinobacteria.c__Actinobacteria.o__Bifidobacteriales.f__Bifidobacteriaceae.g__", 
                        color = "StockAge", fill = "BG",
                        add = "jitter", ylab = "Relative abundance") +
  scale_color_manual(values = c("Human" = "#1f78b4",
                                "Macaque" = "#e31a1c", 
                                "Squirrel Monkey" = "#33a02c")) +
  scale_fill_manual(values = c("RPBG" = "lightgrey", "SPBG" = "white"))
bifidoplot0 = ggpar(bifidoplot0, ylim = c(0,0.0001),
                    legend = "right", 
                    title = "Bifidobacteriaceae,\nunclassified genus: stock") +
  rremove("xlab") + rremove("x.text") + 
  rremove("legend.title")

bifidoplot1 = ggboxplot(first.genus, x = "StockAge", 
                        y = "k__Bacteria.p__Actinobacteria.c__Actinobacteria.o__Bifidobacteriales.f__Bifidobacteriaceae.g__", 
                        color = "StockAge", fill = "BG",
                        add = "jitter", ylab = "Relative abundance") +
  scale_color_manual(values = c("Human" = "#1f78b4",
                                "Macaque" = "#e31a1c", 
                                "Squirrel Monkey" = "#33a02c")) +
  scale_fill_manual(values = c("RPBG" = "lightgrey", "SPBG" = "white"))
bifidoplot1 = ggpar(bifidoplot1, ylim = c(0,0.0001),
                    legend = "right", 
                    title = "Bifidobacteriaceae,\nunclassified genus: first time point") + 
  rremove("xlab") + rremove("x.text") + 
  rremove("legend.title")

bifidoplot2 = ggboxplot(last.genus, x = "StockAge", 
                        y = "k__Bacteria.p__Actinobacteria.c__Actinobacteria.o__Bifidobacteriales.f__Bifidobacteriaceae.g__", 
                        color = "StockAge", fill = "BG",
                        add = "jitter", ylab = "Relative abundance") +
  scale_color_manual(values = c("Human" = "#1f78b4",
                                "Macaque" = "#e31a1c", 
                                "Squirrel Monkey" = "#33a02c")) +
  scale_fill_manual(values = c("RPBG" = "lightgrey", "SPBG" = "white"))
bifidoplot2 = ggpar(bifidoplot2, legend = "right", 
                    title = "Bifidobacteriaceae,\nunclassified genus: last time point") + 
  rremove("xlab") + rremove("x.text") + 
  rremove("legend.title")

peptoplot0 = ggboxplot(stock.genus, x = "StockAge", 
                        y = "k__Bacteria.p__Firmicutes.c__Clostridia.o__Clostridiales.f__.Tissierellaceae..g__Peptoniphilus", 
                        color = "StockAge", fill = "BG",
                        add = "jitter", ylab = "Relative abundance") +
  scale_color_manual(values = c("Human" = "#1f78b4",
                                "Macaque" = "#e31a1c", 
                                "Squirrel Monkey" = "#33a02c")) +
  scale_fill_manual(values = c("RPBG" = "lightgrey", "SPBG" = "white"))
peptoplot0 = ggpar(peptoplot0, legend = "right", 
                    title = "Peptoniphilus: stock") +
  rremove("xlab") + rremove("x.text") + 
  rremove("legend.title")

peptoplot1 = ggboxplot(first.genus, x = "StockAge", 
                        y = "k__Bacteria.p__Firmicutes.c__Clostridia.o__Clostridiales.f__.Tissierellaceae..g__Peptoniphilus", 
                        color = "StockAge", fill = "BG",
                        add = "jitter", ylab = "Relative abundance") +
  scale_color_manual(values = c("Human" = "#1f78b4",
                                "Macaque" = "#e31a1c", 
                                "Squirrel Monkey" = "#33a02c")) +
  scale_fill_manual(values = c("RPBG" = "lightgrey", "SPBG" = "white"))
peptoplot1 = ggpar(peptoplot1, ylim = c(0,0.0001),
                   legend = "right", 
                    title = "Peptoniphilus: first time point") + 
  rremove("xlab") + rremove("x.text") + 
  rremove("legend.title")

peptoplot2 = ggboxplot(last.genus, x = "StockAge", 
                        y = "k__Bacteria.p__Firmicutes.c__Clostridia.o__Clostridiales.f__.Tissierellaceae..g__Peptoniphilus", 
                        color = "StockAge", fill = "BG", 
                        add = "jitter", ylab = "Relative abundance") +
  scale_color_manual(values = c("Human" = "#1f78b4",
                                "Macaque" = "#e31a1c", 
                                "Squirrel Monkey" = "#33a02c")) +
  scale_fill_manual(values = c("RPBG" = "lightgrey", "SPBG" = "white"))
peptoplot2 = ggpar(peptoplot2, ylim = c(0,0.0001),
                   legend = "right", 
                    title = "Peptoniphilus: last time point") + 
  rremove("xlab") + rremove("x.text") + 
  rremove("legend.title")

butyrplot0 = ggboxplot(stock.genus, x = "StockAge", 
                       y = "k__Bacteria.p__Firmicutes.c__Clostridia.o__Clostridiales.f__Lachnospiraceae.g__Butyrivibrio", 
                       color = "StockAge", fill = "BG",
                       add = "jitter", ylab = "Relative abundance") +
  scale_color_manual(values = c("Human" = "#1f78b4",
                                "Macaque" = "#e31a1c", 
                                "Squirrel Monkey" = "#33a02c")) +
  scale_fill_manual(values = c("RPBG" = "lightgrey", "SPBG" = "white"))
butyrplot0 = ggpar(butyrplot0, legend = "right", 
                   title = "Butyrivibrio: stock") +
  rremove("xlab") + rremove("x.text") + 
  rremove("legend.title")

butyrplot1 = ggboxplot(first.genus, x = "StockAge", 
                       y = "k__Bacteria.p__Firmicutes.c__Clostridia.o__Clostridiales.f__Lachnospiraceae.g__Butyrivibrio", 
                       color = "StockAge", fill = "BG",
                       add = "jitter", ylab = "Relative abundance") +
  scale_color_manual(values = c("Human" = "#1f78b4",
                                "Macaque" = "#e31a1c", 
                                "Squirrel Monkey" = "#33a02c")) +
  scale_fill_manual(values = c("RPBG" = "lightgrey", "SPBG" = "white"))
butyrplot1 = ggpar(butyrplot1, legend = "right", 
                   title = "Butyrivibrio: first time point") + 
  rremove("xlab") + rremove("x.text") + 
  rremove("legend.title")

butyrplot2 = ggboxplot(last.genus, x = "StockAge", 
                       y = "k__Bacteria.p__Firmicutes.c__Clostridia.o__Clostridiales.f__Lachnospiraceae.g__Butyrivibrio", 
                       color = "StockAge", fill = "BG", 
                       add = "jitter", ylab = "Relative abundance") +
  scale_color_manual(values = c("Human" = "#1f78b4",
                                "Macaque" = "#e31a1c", 
                                "Squirrel Monkey" = "#33a02c")) +
  scale_fill_manual(values = c("RPBG" = "lightgrey", "SPBG" = "white"))
butyrplot2 = ggpar(butyrplot2, legend = "right", 
                   title = "Butyrivibrio: last time point") + 
  rremove("xlab") + rremove("x.text") + 
  rremove("legend.title")

setwd("/Users/elizabethmallott/Dropbox/Projects/Gut_microbiome/mouse_inoculation/")

tiff(file="Supplementary_Fig7_noinfant.tif", res=300, width=15, height=12, units="in")
ggarrange(bifidoplot0, bifidoplot1, bifidoplot2, 
          peptoplot0, peptoplot1, peptoplot2,
          butyrplot0, butyrplot1, butyrplot1,
          ncol = 3, common.legend = TRUE, legend = "right",
          nrow = 3, align = "hv")
dev.off()

#Phyla absolute abundance (significant)

setwd("/Users/mallott/Dropbox/Projects/Gut_microbiome/mouse_inoculation/16S/Experimental_only")

library(tidyverse)
library(ggpubr)

metadata_phyla = read.csv("mapping_exp_r_updated_072722_noinfant.csv", header = T)
seqs = read.csv("inferred-phyla-noinfant.csv", header = T)

phyla = inner_join(seqs, metadata_phyla, by = "SampleID") %>% 
  filter(Timepoint == "last")

phylanew = phyla %>% 
  mutate(Treatment = case_when(Treatment == "human_adult_mouse" ~ "Human",
                               Treatment == "macaque_adult_mouse" ~ "Macaque",
                               Treatment == "SQM_adult_mouse" ~ "Squirrel Monkey"))

phylanew$Treatment<-factor(phylanew$Treatment,levels=
                             c("Human",
                               "Squirrel Monkey", 
                               "Macaque"))

phylanew = phylanew %>% 
  mutate(BG = case_when(BG == "SPBG" ~ "low-EQ",
                       BG == "RPBG" ~ "high-EQ"))

bactplot1 = ggboxplot(phylanew, x = "Treatment", 
                      y = "Bacteroidetes", color = "Treatment", fill = "BG",
                      add = "jitter", ylab = "Absolute abundance") +
  scale_color_manual(values = c("Human" = "#1f78b4",
                                "Macaque" = "#e31a1c", 
                                "Squirrel Monkey" = "#33a02c")) +
  scale_fill_manual(values = c("high-EQ" = "lightgrey", "low-EQ" = "white"))
bactplot1 = ggpar(bactplot1, legend = "right", 
                  title = "Bacteroidetes") + 
  rremove("xlab") + rremove("x.text") + 
  rremove("legend.title")

firmplot1 = ggboxplot(phylanew, x = "Treatment", 
                      y = "Firmicutes", color = "Treatment", fill = "BG",
                      add = "jitter", ylab = "Absolute abundance") +
  scale_color_manual(values = c("Human" = "#1f78b4",
                                "Macaque" = "#e31a1c", 
                                "Squirrel Monkey" = "#33a02c")) +
  scale_fill_manual(values = c("high-EQ" = "lightgrey", "low-EQ" = "white"))
firmplot1 = ggpar(firmplot1, legend = "right", 
                  title = "Firmicutes") + 
  rremove("xlab") + rremove("x.text") + 
  rremove("legend.title")

verrplot1 = ggboxplot(phylanew, x = "Treatment", 
                      y = "Verrucomicrobia", color = "Treatment", fill = "BG",
                      add = "jitter", ylab = "Absolute abundance") +
  scale_color_manual(values = c("Human" = "#1f78b4",
                                "Macaque" = "#e31a1c", 
                                "Squirrel Monkey" = "#33a02c")) +
  scale_fill_manual(values = c("high-EQ" = "lightgrey", "low-EQ" = "white"))
verrplot1 = ggpar(verrplot1, legend = "right", 
                  title = "Verrucomicrobia") + 
  rremove("xlab") + rremove("x.text") + 
  rremove("legend.title")

setwd("/Users/mallott/Dropbox/Projects/Gut_microbiome/mouse_inoculation/")

tiff(file="Supplementary_Fig8_noinfant.tif", res=300, width=18, height=4, units="in")
ggarrange(bactplot1, firmplot1, verrplot1, ncol = 3, common.legend = TRUE,
          legend = "right", align = "h")
dev.off()

#Family absolute abundance (significant)

setwd("/Users/mallott/Dropbox/Projects/Gut_microbiome/mouse_inoculation/16S/Experimental_only")

library(tidyverse)
library(ggpubr)

family_relab = read.csv("inferred-families-noinfant.csv", header = T)
metadata = read.csv("mapping_exp_r_updated_072722_noinfant.csv", header = T)

family = metadata %>% inner_join(family_relab, by = "SampleID") %>% 
  filter(Timepoint == "last")

familynew = family %>% 
  mutate(Treatment = case_when(Treatment == "human_adult_mouse" ~ "Human",
                               Treatment == "macaque_adult_mouse" ~ "Macaque",
                               Treatment == "SQM_adult_mouse" ~ "Squirrel Monkey"))

familynew$Treatment<-factor(familynew$Treatment,levels=
                              c("Human",
                                "Squirrel Monkey", 
                                "Macaque"))

familynew = familynew %>% 
  mutate(BG = case_when(BG == "SPBG" ~ "low-EQ",
                        BG == "RPBG" ~ "high-EQ"))

aa = ggboxplot(familynew, x = "Treatment", 
               y = "k__Bacteria.p__Bacteroidetes.c__Bacteroidia.o__Bacteroidales.__", 
               color = "Treatment", fill = "BG",
               add = "jitter", ylab = "Absolute abundance") +
  scale_color_manual(values = c("Human" = "#1f78b4",
                                "Macaque" = "#e31a1c", 
                                "Squirrel Monkey" = "#33a02c")) +
  scale_fill_manual(values = c("high-EQ" = "lightgrey", "low-EQ" = "white"))
aa = ggpar(aa, legend = "right", 
           title = "Bacteroidales, unclassified family") + 
  rremove("xlab") + rremove("x.text") + 
  rremove("legend.title")

ab = ggboxplot(familynew, x = "Treatment", 
               y = "k__Bacteria.p__Bacteroidetes.c__Bacteroidia.o__Bacteroidales.f__.Paraprevotellaceae.", 
               color = "Treatment", fill = "BG",
               add = "jitter", ylab = "Absolute abundance") +
  scale_color_manual(values = c("Human" = "#1f78b4",
                                "Macaque" = "#e31a1c", 
                                "Squirrel Monkey" = "#33a02c")) +
  scale_fill_manual(values = c("high-EQ" = "lightgrey", "low-EQ" = "white"))
ab = ggpar(ab, legend = "right", 
           title = "Paraprevotellaceae") + 
  rremove("xlab") + rremove("x.text") + 
  rremove("legend.title")

ac = ggboxplot(familynew, x = "Treatment", 
               y = "k__Bacteria.p__Bacteroidetes.c__Bacteroidia.o__Bacteroidales.f__Bacteroidaceae", 
               color = "Treatment", fill = "BG",
               add = "jitter", ylab = "Absolute abundance") +
  scale_color_manual(values = c("Human" = "#1f78b4",
                                "Macaque" = "#e31a1c", 
                                "Squirrel Monkey" = "#33a02c")) +
  scale_fill_manual(values = c("high-EQ" = "lightgrey", "low-EQ" = "white"))
ac = ggpar(ac, legend = "right", 
           title = "Bacteroidaceae") + 
  rremove("xlab") + rremove("x.text") + 
  rremove("legend.title")

ad = ggboxplot(familynew, x = "Treatment", 
               y = "k__Bacteria.p__Bacteroidetes.c__Bacteroidia.o__Bacteroidales.f__Porphyromonadaceae", 
               color = "Treatment", fill = "BG",
               add = "jitter", ylab = "Absolute abundance") +
  scale_color_manual(values = c("Human" = "#1f78b4",
                                "Macaque" = "#e31a1c", 
                                "Squirrel Monkey" = "#33a02c")) +
  scale_fill_manual(values = c("high-EQ" = "lightgrey", "low-EQ" = "white"))
ad = ggpar(ad, legend = "right", 
           title = "Porphyromonadaceae") + 
  rremove("xlab") + rremove("x.text") + 
  rremove("legend.title")

ae = ggboxplot(familynew, x = "Treatment", 
               y = "k__Bacteria.p__Firmicutes.c__Clostridia.o__Clostridiales.f__", 
               color = "Treatment", fill = "BG",
               add = "jitter", ylab = "Absolute abundance") +
  scale_color_manual(values = c("Human" = "#1f78b4",
                                "Macaque" = "#e31a1c", 
                                "Squirrel Monkey" = "#33a02c")) +
  scale_fill_manual(values = c("high-EQ" = "lightgrey", "low-EQ" = "white"))
ae = ggpar(ae, legend = "right", 
           title = "Clostridiales, unclassified family") + 
  rremove("xlab") + rremove("x.text") + 
  rremove("legend.title")

af = ggboxplot(familynew, x = "Treatment", 
               y = "k__Bacteria.p__Firmicutes.c__Clostridia.o__Clostridiales.f__Lachnospiraceae", 
               color = "Treatment", fill = "BG",
               add = "jitter", ylab = "Absolute abundance") +
  scale_color_manual(values = c("Human" = "#1f78b4",
                                "Macaque" = "#e31a1c", 
                                "Squirrel Monkey" = "#33a02c")) +
  scale_fill_manual(values = c("high-EQ" = "lightgrey", "low-EQ" = "white"))
af = ggpar(af, legend = "right", 
           title = "Lachnospiraceae") + 
  rremove("xlab") + rremove("x.text") + 
  rremove("legend.title")

ag = ggboxplot(familynew, x = "Treatment", 
               y = "k__Bacteria.p__Firmicutes.c__Clostridia.o__Clostridiales.f__Ruminococcaceae", 
               color = "Treatment", fill = "BG",
               add = "jitter", ylab = "Absolute abundance") +
  scale_color_manual(values = c("Human" = "#1f78b4",
                                "Macaque" = "#e31a1c", 
                                "Squirrel Monkey" = "#33a02c")) +
  scale_fill_manual(values = c("high-EQ" = "lightgrey", "low-EQ" = "white"))
ag = ggpar(ag, legend = "right", 
           title = "Ruminococcaceae") + 
  rremove("xlab") + rremove("x.text") + 
  rremove("legend.title")

ah = ggboxplot(familynew, x = "Treatment", 
               y = "k__Bacteria.p__Firmicutes.c__Clostridia.o__Clostridiales.f__Veillonellaceae", 
               color = "Treatment", fill = "BG",
               add = "jitter", ylab = "Absolute abundance") +
  scale_color_manual(values = c("Human" = "#1f78b4",
                                "Macaque" = "#e31a1c", 
                                "Squirrel Monkey" = "#33a02c")) +
  scale_fill_manual(values = c("high-EQ" = "lightgrey", "low-EQ" = "white"))
ah = ggpar(ah, legend = "right", 
           title = "Veillonellaceae") + 
  rremove("xlab") + rremove("x.text") + 
  rremove("legend.title")

ai = ggboxplot(familynew, x = "Treatment", 
               y = "k__Bacteria.p__Verrucomicrobia.c__Verrucomicrobiae.o__Verrucomicrobiales.f__Verrucomicrobiaceae", 
               color = "Treatment", fill = "BG",
               add = "jitter", ylab = "Absolute abundance") +
  scale_color_manual(values = c("Human" = "#1f78b4",
                                "Macaque" = "#e31a1c", 
                                "Squirrel Monkey" = "#33a02c")) +
  scale_fill_manual(values = c("high-EQ" = "lightgrey", "low-EQ" = "white"))
ai = ggpar(ai, legend = "right", 
           title = "Verrucomicrobiaceae") + 
  rremove("xlab") + rremove("x.text") + 
  rremove("legend.title")

setwd("/Users/mallott/Dropbox/Projects/Gut_microbiome/mouse_inoculation/")

tiff(file="Supplementary_Fig9_noinfant.tif", res=300, width=15, height=10, units="in")
ggarrange(aa, ab, ac, ad, ae, af, ag, ah, ai, nrow = 3, ncol = 3, 
          common.legend = TRUE, legend = "right", align = "hv")
dev.off()

#Genus absolute abundance (significant with p but not q)

setwd("/Users/mallott/Dropbox/Projects/Gut_microbiome/mouse_inoculation/16S/Experimental_only")

library(tidyverse)
library(ggpubr)

genus_relab = read.csv("inferred-genera-noinfant.csv", header = T)
metadata = read.csv("mapping_exp_r_updated_072722_noinfant.csv", header = T)

genus = metadata %>% inner_join(genus_relab, by = "SampleID") %>% 
  filter(Timepoint == "last")

genusnew = genus %>% 
  mutate(Treatment = case_when(Treatment == "human_adult_mouse" ~ "Human",
                               Treatment == "macaque_adult_mouse" ~ "Macaque",
                               Treatment == "SQM_adult_mouse" ~ "Squirrel Monkey"))

genusnew$Treatment<-factor(genusnew$Treatment,levels=
                              c("Human",
                                "Squirrel Monkey", 
                                "Macaque"))

genusnew = genusnew %>% 
  mutate(BG = case_when(BG == "SPBG" ~ "low-EQ",
                        BG == "RPBG" ~ "high-EQ"))

aa = ggboxplot(genusnew, x = "Treatment", 
               y = "k__Bacteria.p__Bacteroidetes.c__Bacteroidia.o__Bacteroidales.__.__", 
               color = "Treatment", fill = "BG",
               add = "jitter", ylab = "Absolute abundance") +
  scale_color_manual(values = c("Human" = "#1f78b4",
                                "Macaque" = "#e31a1c", 
                                "Squirrel Monkey" = "#33a02c")) +
  scale_fill_manual(values = c("high-EQ" = "lightgrey", "low-EQ" = "white"))
aa = ggpar(aa, legend = "right", 
           title = "Bacteroidales, unclassified genus") + 
  rremove("xlab") + rremove("x.text") + 
  rremove("legend.title")

ab = ggboxplot(genusnew, x = "Treatment", 
               y = "k__Bacteria.p__Bacteroidetes.c__Bacteroidia.o__Bacteroidales.f__Bacteroidaceae.g__Bacteroides", 
               color = "Treatment", fill = "BG",
               add = "jitter", ylab = "Absolute abundance") +
  scale_color_manual(values = c("Human" = "#1f78b4",
                                "Macaque" = "#e31a1c", 
                                "Squirrel Monkey" = "#33a02c")) +
  scale_fill_manual(values = c("high-EQ" = "lightgrey", "low-EQ" = "white"))
ab = ggpar(ab, legend = "right", 
           title = "Bacteroides") + 
  rremove("xlab") + rremove("x.text") + 
  rremove("legend.title")

ac = ggboxplot(genusnew, x = "Treatment", 
               y = "k__Bacteria.p__Firmicutes.c__Clostridia.o__Clostridiales.f__.g__", 
               color = "Treatment", fill = "BG",
               add = "jitter", ylab = "Absolute abundance") +
  scale_color_manual(values = c("Human" = "#1f78b4",
                                "Macaque" = "#e31a1c", 
                                "Squirrel Monkey" = "#33a02c")) +
  scale_fill_manual(values = c("high-EQ" = "lightgrey", "low-EQ" = "white"))
ac = ggpar(ac, legend = "right", 
           title = "Clostridiales, unclassified genus") + 
  rremove("xlab") + rremove("x.text") + 
  rremove("legend.title")

ad = ggboxplot(genusnew, x = "Treatment", 
               y = "k__Bacteria.p__Firmicutes.c__Clostridia.o__Clostridiales.f__Christensenellaceae.g__", 
               color = "Treatment", fill = "BG",
               add = "jitter", ylab = "Absolute abundance") +
  scale_color_manual(values = c("Human" = "#1f78b4",
                                "Macaque" = "#e31a1c", 
                                "Squirrel Monkey" = "#33a02c")) +
  scale_fill_manual(values = c("high-EQ" = "lightgrey", "low-EQ" = "white"))
ad = ggpar(ad, legend = "right", 
           title = "Christensenellaceae, unclassified genus") + 
  rremove("xlab") + rremove("x.text") + 
  rremove("legend.title")

ae = ggboxplot(genusnew, x = "Treatment", 
               y = "k__Bacteria.p__Firmicutes.c__Clostridia.o__Clostridiales.f__Lachnospiraceae.__", 
               color = "Treatment", fill = "BG",
               add = "jitter", ylab = "Absolute abundance") +
  scale_color_manual(values = c("Human" = "#1f78b4",
                                "Macaque" = "#e31a1c", 
                                "Squirrel Monkey" = "#33a02c")) +
  scale_fill_manual(values = c("high-EQ" = "lightgrey", "low-EQ" = "white"))
ae = ggpar(ae, legend = "right", 
           title = "Lachnospiraceae, unclassified genus") + 
  rremove("xlab") + rremove("x.text") + 
  rremove("legend.title")

af = ggboxplot(genusnew, x = "Treatment", 
               y = "k__Bacteria.p__Firmicutes.c__Clostridia.o__Clostridiales.f__Lachnospiraceae.g__Coprococcus", 
               color = "Treatment", fill = "BG",
               add = "jitter", ylab = "Absolute abundance") +
  scale_color_manual(values = c("Human" = "#1f78b4",
                                "Macaque" = "#e31a1c", 
                                "Squirrel Monkey" = "#33a02c")) +
  scale_fill_manual(values = c("high-EQ" = "lightgrey", "low-EQ" = "white"))
af = ggpar(af, legend = "right", 
           title = "Coprococcus") + 
  rremove("xlab") + rremove("x.text") + 
  rremove("legend.title")

ag = ggboxplot(genusnew, x = "Treatment", 
               y = "k__Bacteria.p__Firmicutes.c__Clostridia.o__Clostridiales.f__Ruminococcaceae.g__Oscillospira", 
               color = "Treatment", fill = "BG",
               add = "jitter", ylab = "Absolute abundance") +
  scale_color_manual(values = c("Human" = "#1f78b4",
                                "Macaque" = "#e31a1c", 
                                "Squirrel Monkey" = "#33a02c")) +
  scale_fill_manual(values = c("high-EQ" = "lightgrey", "low-EQ" = "white"))
ag = ggpar(ag, legend = "right", 
           title = "Oscillospira") + 
  rremove("xlab") + rremove("x.text") + 
  rremove("legend.title")

ah = ggboxplot(genusnew, x = "Treatment", 
               y = "k__Bacteria.p__Firmicutes.c__Clostridia.o__Clostridiales.f__Ruminococcaceae.g__Ruminococcus", 
               color = "Treatment", fill = "BG",
               add = "jitter", ylab = "Absolute abundance") +
  scale_color_manual(values = c("Human" = "#1f78b4",
                                "Macaque" = "#e31a1c", 
                                "Squirrel Monkey" = "#33a02c")) +
  scale_fill_manual(values = c("high-EQ" = "lightgrey", "low-EQ" = "white"))
ah = ggpar(ah, legend = "right", 
           title = "Ruminococcus") + 
  rremove("xlab") + rremove("x.text") + 
  rremove("legend.title")

ai = ggboxplot(genusnew, x = "Treatment", 
               y = "k__Bacteria.p__Verrucomicrobia.c__Verrucomicrobiae.o__Verrucomicrobiales.f__Verrucomicrobiaceae.g__Akkermansia", 
               color = "Treatment", fill = "BG",
               add = "jitter", ylab = "Absolute abundance") +
  scale_color_manual(values = c("Human" = "#1f78b4",
                                "Macaque" = "#e31a1c", 
                                "Squirrel Monkey" = "#33a02c")) +
  scale_fill_manual(values = c("high-EQ" = "lightgrey", "low-EQ" = "white"))
ai = ggpar(ai, legend = "right", 
           title = "Akkermansia") + 
  rremove("xlab") + rremove("x.text") + 
  rremove("legend.title")

setwd("/Users/mallott/Dropbox/Projects/Gut_microbiome/mouse_inoculation/")

tiff(file="Supplementary_Fig10_noinfant.tif", res=300, width=15, height=10, units="in")
ggarrange(aa, ab, ac, ad, ae, af, ag, ah, ai, nrow = 3, ncol = 3, 
          common.legend = TRUE, legend = "right", align = "hv")
dev.off()

#Shotgun NMDS plots for bray-curtis/jaccard

setwd("/Users/elizabethmallott/Dropbox/Projects/Gut_microbiome/mouse_inoculation/shotgun/humann2_output")

library(vegan)
library(ggplot2)
library(tidyverse)
library(cowplot)

bray_gene = as.dist(read.table("bray-genefamilies-unstrat-noinfant.tsv", header = T))
jaccard_gene = as.dist(read.table("jaccard_genefamilies_unstrat_noinfant.tsv", header = T))
bray_pa = as.dist(read.table("bray_pathabund_unstrat_noinfant.tsv", header = T))
jaccard_pa = as.dist(read.table("jaccard_pathabund_unstrat_noinfant.tsv", header = T))
metadata_gene = read.csv("mapping_genefam_update_072722_noinfant.csv", header = T)
metadata_pa = read.csv("mapping_update_072722_noinfant.csv", header = T)

metadata_gene_new = metadata_gene %>% 
  mutate(Treatment = case_when(Treatment == "human_adult" ~ "Human",
                               Treatment == "macaque_adult" ~ "Macaque",
                               Treatment == "SQM_adult" ~ "Squirrel Monkey"))

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

mds_otus_bray_gene<-metaMDS(bray_gene, k=2, trymax=499)
mds_otus_bray_gene_points<-mds_otus_bray_gene$points
mds_otus_bray_gene_points2<-merge(x=mds_otus_bray_gene_points, y = metadata_gene_new, 
                                  by.x = "row.names", by.y = "sampleid")

braygf <- ggplot(mds_otus_bray_gene_points2, aes(x = MDS1, y = MDS2, 
                                                 color = Treatment)) +
  geom_point(size=3, shape = 17) +
  scale_color_manual(values = c("Human" = "#1f78b4",
                                "Macaque" = "#e31a1c", 
                                "Squirrel Monkey" = "#33a02c")) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'), 
        legend.key=element_blank()) + 
  theme(axis.title.x=element_text(size=rel(2)), 
        axis.title.y=element_text(size=rel(2)),
        plot.title = element_text(size=rel(3)),
        legend.title = element_text(size=rel(2)),
        legend.text = element_text(size = rel(1.8))) + 
  ggtitle("Bray-Curtis:\nGene families") 

mds_otus_jaccard_gene<-metaMDS(jaccard_gene, k=2, trymax=500)
mds_otus_jaccard_gene_points<-mds_otus_jaccard_gene$points
mds_otus_jaccard_gene_points2<-merge(x=mds_otus_jaccard_gene_points, y = metadata_gene_new, 
                                     by.x = "row.names", by.y = "sampleid")

jaccgf <- ggplot(mds_otus_jaccard_gene_points2, aes(x = MDS1, y = MDS2, 
                                                    color = Treatment)) +
  geom_point(size=3, shape=17) +
  scale_color_manual(values = c("Human" = "#1f78b4",
                                "Macaque" = "#e31a1c", 
                                "Squirrel Monkey" = "#33a02c")) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'), 
        legend.key=element_blank()) + 
  theme(axis.title.x=element_text(size=rel(2)), 
        axis.title.y=element_text(size=rel(2)),
        plot.title = element_text(size=rel(3)),
        legend.title = element_text(size=rel(2)),
        legend.text = element_text(size = rel(1.8))) + 
  ggtitle("Jaccard:\nGene families") 

mds_otus_bray_pa<-metaMDS(bray_pa, k=2, trymax=500)
mds_otus_bray_pa_points<-mds_otus_bray_pa$points
mds_otus_bray_pa_points2<-merge(x=mds_otus_bray_pa_points, y = metadata_pa_new, 
                                by.x = "row.names", by.y = "sampleid")
braypa <- ggplot(mds_otus_bray_pa_points2, aes(x = MDS1, y = MDS2, 
                                               color = Treatment)) +
  geom_point(size=3, shape=17) +
  scale_color_manual(values = c("Human" = "#1f78b4",
                                "Macaque" = "#e31a1c", 
                                "Squirrel Monkey" = "#33a02c")) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'), 
        legend.key=element_blank()) + 
  theme(axis.title.x=element_text(size=rel(2)), 
        axis.title.y=element_text(size=rel(2)),
        plot.title = element_text(size=rel(3)),
        legend.title = element_text(size=rel(2)),
        legend.text = element_text(size = rel(1.8))) + 
  ggtitle("Bray-Curtis:\nPathway abundance")

mds_otus_jaccard_pa<-metaMDS(jaccard_pa, k=2, trymax=500)
mds_otus_jaccard_pa_points<-mds_otus_jaccard_pa$points
mds_otus_jaccard_pa_points2<-merge(x=mds_otus_jaccard_pa_points, y = metadata_pa_new, 
                                   by.x = "row.names", by.y = "sampleid")
jaccpa <- ggplot(mds_otus_jaccard_pa_points2, aes(x = MDS1, y = MDS2, 
                                                  color = Treatment)) +
  geom_point(size=3, shape=17) +
  scale_color_manual(values = c("Human" = "#1f78b4",
                                "Macaque" = "#e31a1c", 
                                "Squirrel Monkey" = "#33a02c")) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'), 
        legend.key=element_blank()) + 
  theme(axis.title.x=element_text(size=rel(2)), 
        axis.title.y=element_text(size=rel(2)),
        plot.title = element_text(size=rel(3)),
        legend.title = element_text(size=rel(2)),
        legend.text = element_text(size = rel(1.8))) + 
  ggtitle("Jaccard:\nPathway Abundance") 

setwd("/Users/elizabethmallott/Dropbox/Projects/Gut_microbiome/mouse_inoculation/")

tiff(file = "Supplementary_Fig11_noinfant.tif", res = 300, width = 18, height = 14, units="in")
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

##Supplemental figure 12 and 13 ----

#Differential gene expression for infants 

setwd("/Users/elizabethmallott/Dropbox/Projects/Gut_microbiome/mouse_inoculation/RNAseq")

library(tidyverse)
library(ggpubr)

genes = read_csv("transformed_data_to_plot2.csv")
metadata = read_csv("Gene_expression_metadata.csv", col_types = "ccffff")

genes_long = genes %>% pivot_longer(-ensgene, names_to = "sampleid") 

genes_wmeta = genes_long %>% left_join(metadata)

genes_wmeta = genes_wmeta %>% 
  mutate(Group = case_when(Group == "HumanAdult" ~ "Human Adult",
                           Group == "HumanInfant" ~ "Human Infant",
                           Group == "MacaqueAdult" ~ "Macaque Adult",
                           Group == "MacaqueInfant" ~ "Macaque Infant",
                           Group == "Squirrel_monkeyAdult" ~ "Squirrel Monkey Adult",
                           Group == "Squirrel_monkeyInfant" ~ "Squirrel Monkey Infant"))

genes_wmeta$Group<-factor(genes_wmeta$Group,levels=
                            c("Human Adult","Human Infant",
                              "Squirrel Monkey Adult", "Squirrel Monkey Infant",
                              "Macaque Adult", "Macaque Infant"))

genes_wmeta_filter1 = genes_wmeta %>% filter(ensgene == "ENSMUSG00000006362")
genes_wmeta_filter2 = genes_wmeta %>% filter(ensgene == "ENSMUSG00000010025") 
genes_wmeta_filter3 = genes_wmeta %>% filter(ensgene == "ENSMUSG00000037440")
genes_wmeta_filter4 = genes_wmeta %>% filter(ensgene == "ENSMUSG00000041220")
genes_wmeta_filter5 = genes_wmeta %>% filter(ensgene == "ENSMUSG00000022853")
genes_wmeta_filter6 = genes_wmeta %>% filter(ensgene == "ENSMUSG00000021226")
genes_wmeta_filter7 = genes_wmeta %>% filter(ensgene == "ENSMUSG00000055782")
genes_wmeta_filter8 = genes_wmeta %>% filter(ensgene == "ENSMUSG00000008153")
genes_wmeta_filter9 = genes_wmeta %>% filter(ensgene == "ENSMUSG00000062545")
genes_wmeta_filter10 = genes_wmeta %>% filter(ensgene == "ENSMUSG00000060548")
genes_wmeta_filter11 = genes_wmeta %>% filter(ensgene == "ENSMUSG00000019935")
genes_wmeta_filter12 = genes_wmeta %>% filter(ensgene == "ENSMUSG00000056035")
genes_wmeta_filter13 = genes_wmeta %>% filter(ensgene == "ENSMUSG00000032418")
genes_wmeta_filter14 = genes_wmeta %>% filter(ensgene == "ENSMUSG00000056666")
genes_wmeta_filter15 = genes_wmeta %>% filter(ensgene == "ENSMUSG00000010651")
genes_wmeta_filter16 = genes_wmeta %>% filter(ensgene == "ENSMUSG00000035560")
genes_wmeta_filter17 = genes_wmeta %>% filter(ensgene == "ENSMUSG00000072949")
genes_wmeta_filter18 = genes_wmeta %>% filter(ensgene == "ENSMUSG00000023044")
genes_wmeta_filter19 = genes_wmeta %>% filter(ensgene == "ENSMUSG00000073758")
genes_wmeta_filter20 = genes_wmeta %>% filter(ensgene == "ENSMUSG00000002980")
genes_wmeta_filter21 = genes_wmeta %>% filter(ensgene == "ENSMUSG00000042812")
genes_wmeta_filter22 = genes_wmeta %>% filter(ensgene == "ENSMUSG00000026853")
genes_wmeta_filter23 = genes_wmeta %>% filter(ensgene == "ENSMUSG00000066072")
genes_wmeta_filter24 = genes_wmeta %>% filter(ensgene == "ENSMUSG00000021228")
genes_wmeta_filter25 = genes_wmeta %>% filter(ensgene == "ENSMUSG00000028715")
genes_wmeta_filter26 = genes_wmeta %>% filter(ensgene == "ENSMUSG00000024989")
genes_wmeta_filter27 = genes_wmeta %>% filter(ensgene == "ENSMUSG00000029272")
genes_wmeta_filter28 = genes_wmeta %>% filter(ensgene == "ENSMUSG00000057465")
genes_wmeta_filter29 = genes_wmeta %>% filter(ensgene == "ENSMUSG00000090555")
genes_wmeta_filter30 = genes_wmeta %>% filter(ensgene == "ENSMUSG00000026390")
genes_wmeta_filter31 = genes_wmeta %>% filter(ensgene == "ENSMUSG00000066361")
genes_wmeta_filter32 = genes_wmeta %>% filter(ensgene == "ENSMUSG00000074115")
genes_wmeta_filter33 = genes_wmeta %>% filter(ensgene == "ENSMUSG00000075176")
genes_wmeta_filter34 = genes_wmeta %>% filter(ensgene == "ENSMUSG00000062061")
genes_wmeta_filter35 = genes_wmeta %>% filter(ensgene == "ENSMUSG00000062209")
genes_wmeta_filter36 = genes_wmeta %>% filter(ensgene == "ENSMUSG00000047462")
genes_wmeta_filter37 = genes_wmeta %>% filter(ensgene == "ENSMUSG00000028359")
genes_wmeta_filter38 = genes_wmeta %>% filter(ensgene == "ENSMUSG00000092021")
genes_wmeta_filter39 = genes_wmeta %>% filter(ensgene == "ENSMUSG00000031762")
genes_wmeta_filter40 = genes_wmeta %>% filter(ensgene == "ENSMUSG00000039533")
genes_wmeta_filter41 = genes_wmeta %>% filter(ensgene == "ENSMUSG00000024029")
genes_wmeta_filter42 = genes_wmeta %>% filter(ensgene == "ENSMUSG00000029546")
genes_wmeta_filter43 = genes_wmeta %>% filter(ensgene == "ENSMUSG00000019139")
genes_wmeta_filter44 = genes_wmeta %>% filter(ensgene == "ENSMUSG00000061540")
genes_wmeta_filter45 = genes_wmeta %>% filter(ensgene == "ENSMUSG00000005553")
genes_wmeta_filter46 = genes_wmeta %>% filter(ensgene == "ENSMUSG00000091407")
genes_wmeta_filter47 = genes_wmeta %>% filter(ensgene == "ENSMUSG00000034116")
genes_wmeta_filter48 = genes_wmeta %>% filter(ensgene == "ENSMUSG00000023067")
genes_wmeta_filter49 = genes_wmeta %>% filter(ensgene == "ENSMUSG00000031765")
genes_wmeta_filter50 = genes_wmeta %>% filter(ensgene == "ENSMUSG00000000320")
genes_wmeta_filter51 = genes_wmeta %>% filter(ensgene == "ENSMUSG00000066372")
genes_wmeta_filter52 = genes_wmeta %>% filter(ensgene == "ENSMUSG00000026072")
genes_wmeta_filter53 = genes_wmeta %>% filter(ensgene == "ENSMUSG00000068877")
genes_wmeta_filter54 = genes_wmeta %>% filter(ensgene == "ENSMUSG00000030131")
genes_wmeta_filter55 = genes_wmeta %>% filter(ensgene == "ENSMUSG00000111585")
genes_wmeta_filter56 = genes_wmeta %>% filter(ensgene == "ENSMUSG00000111163")
genes_wmeta_filter57 = genes_wmeta %>% filter(ensgene == "ENSMUSG00000057203")
genes_wmeta_filter58 = genes_wmeta %>% filter(ensgene == "ENSMUSG00000071204")
genes_wmeta_filter59 = genes_wmeta %>% filter(ensgene == "ENSMUSG00000052058")
genes_wmeta_filter60 = genes_wmeta %>% filter(ensgene == "ENSMUSG00000030364")
genes_wmeta_filter61 = genes_wmeta %>% filter(ensgene == "ENSMUSG00000029304")
genes_wmeta_filter62 = genes_wmeta %>% filter(ensgene == "ENSMUSG00000040026")
genes_wmeta_filter63 = genes_wmeta %>% filter(ensgene == "ENSMUSG00000039519")
genes_wmeta_filter64 = genes_wmeta %>% filter(ensgene == "ENSMUSG00000001128")

gene1 = ggboxplot(genes_wmeta_filter1, x = "Group", y = "value",  
                  add = "jitter", color = "Group", fill = "BG", 
                  ylab = "Cbfa2t3 Expression") +
  scale_color_manual(values = c("Human Adult" = "#a6cee3",
                                "Human Infant" = "#1f78b4", 
                                "Macaque Adult" = "#fb9a99",
                                "Macaque Infant" = "#e31a1c", 
                                "Squirrel Monkey Adult" = "#b2df8a", 
                                "Squirrel Monkey Infant" = "#33a02c")) +
  scale_fill_manual(values = c("RPBG" = "lightgrey", "SPBG" = "white"))
gene1 = ggpar(gene1, legend = "right") + 
  rremove("xlab") + rremove("x.text") + rremove("legend.title")

gene2 = ggboxplot(genes_wmeta_filter2, x = "Group", y = "value",  
                  add = "jitter", color = "Group", fill = "BG", 
                  ylab = "Aldh3a2 Expression") +
  scale_color_manual(values = c("Human Adult" = "#a6cee3",
                                "Human Infant" = "#1f78b4", 
                                "Macaque Adult" = "#fb9a99",
                                "Macaque Infant" = "#e31a1c", 
                                "Squirrel Monkey Adult" = "#b2df8a", 
                                "Squirrel Monkey Infant" = "#33a02c")) +
  scale_fill_manual(values = c("RPBG" = "lightgrey", "SPBG" = "white"))
gene2 = ggpar(gene2, legend = "right") + 
  rremove("xlab") + rremove("x.text") + rremove("legend.title")

gene3 = ggboxplot(genes_wmeta_filter3, x = "Group", y = "value", 
                  add = "jitter", color = "Group", fill = "BG", 
                  ylab = "Vnn1 Expression") +
  scale_color_manual(values = c("Human Adult" = "#a6cee3",
                                "Human Infant" = "#1f78b4", 
                                "Macaque Adult" = "#fb9a99",
                                "Macaque Infant" = "#e31a1c", 
                                "Squirrel Monkey Adult" = "#b2df8a", 
                                "Squirrel Monkey Infant" = "#33a02c")) +
  scale_fill_manual(values = c("RPBG" = "lightgrey", "SPBG" = "white"))
gene3 = ggpar(gene3, legend = "right") + 
  rremove("xlab") + rremove("x.text") + rremove("legend.title")

gene4 = ggboxplot(genes_wmeta_filter4, x = "Group", y = "value",  
                  add = "jitter", color = "Group", fill = "BG", 
                  ylab = "Elovl6 Expression") +
  scale_color_manual(values = c("Human Adult" = "#a6cee3",
                                "Human Infant" = "#1f78b4", 
                                "Macaque Adult" = "#fb9a99",
                                "Macaque Infant" = "#e31a1c", 
                                "Squirrel Monkey Adult" = "#b2df8a", 
                                "Squirrel Monkey Infant" = "#33a02c")) +
  scale_fill_manual(values = c("RPBG" = "lightgrey", "SPBG" = "white"))
gene4 = ggpar(gene4, legend = "right") + 
  rremove("xlab") + rremove("x.text") + rremove("legend.title")

gene5 = ggboxplot(genes_wmeta_filter5, x = "Group", y = "value",  
                  add = "jitter", color = "Group", fill = "BG", 
                  ylab = "Ehhadh Expression") +
  scale_color_manual(values = c("Human Adult" = "#a6cee3",
                                "Human Infant" = "#1f78b4", 
                                "Macaque Adult" = "#fb9a99",
                                "Macaque Infant" = "#e31a1c", 
                                "Squirrel Monkey Adult" = "#b2df8a", 
                                "Squirrel Monkey Infant" = "#33a02c")) +
  scale_fill_manual(values = c("RPBG" = "lightgrey", "SPBG" = "white"))
gene5 = ggpar(gene5, legend = "right") + 
  rremove("xlab") + rremove("x.text") + rremove("legend.title")

gene6 = ggboxplot(genes_wmeta_filter6, x = "Group", y = "value",  
                  add = "jitter", color = "Group", fill = "BG", 
                  ylab = "Acot2 Expression") +
  scale_color_manual(values = c("Human Adult" = "#a6cee3",
                                "Human Infant" = "#1f78b4", 
                                "Macaque Adult" = "#fb9a99",
                                "Macaque Infant" = "#e31a1c", 
                                "Squirrel Monkey Adult" = "#b2df8a", 
                                "Squirrel Monkey Infant" = "#33a02c")) +
  scale_fill_manual(values = c("RPBG" = "lightgrey", "SPBG" = "white"))
gene6 = ggpar(gene6, legend = "right") + 
  rremove("xlab") + rremove("x.text") + rremove("legend.title")

gene7 = ggboxplot(genes_wmeta_filter7, x = "Group", y = "value", 
                  add = "jitter", color = "Group", fill = "BG", 
                  ylab = "Abcd2 Expression") +
  scale_color_manual(values = c("Human Adult" = "#a6cee3",
                                "Human Infant" = "#1f78b4", 
                                "Macaque Adult" = "#fb9a99",
                                "Macaque Infant" = "#e31a1c", 
                                "Squirrel Monkey Adult" = "#b2df8a", 
                                "Squirrel Monkey Infant" = "#33a02c")) +
  scale_fill_manual(values = c("RPBG" = "lightgrey", "SPBG" = "white"))
gene7 = ggpar(gene7, legend = "right") + 
  rremove("xlab") + rremove("x.text") + rremove("legend.title")

gene8 = ggboxplot(genes_wmeta_filter8, x = "Group", y = "value", 
                  add = "jitter", color = "Group", fill = "BG", 
                  ylab = "Clstn3 Expression") +
  scale_color_manual(values = c("Human Adult" = "#a6cee3",
                                "Human Infant" = "#1f78b4", 
                                "Macaque Adult" = "#fb9a99",
                                "Macaque Infant" = "#e31a1c", 
                                "Squirrel Monkey Adult" = "#b2df8a", 
                                "Squirrel Monkey Infant" = "#33a02c")) +
  scale_fill_manual(values = c("RPBG" = "lightgrey", "SPBG" = "white"))
gene8 = ggpar(gene8, legend = "right") + 
  rremove("xlab") + rremove("x.text") + rremove("legend.title")

gene9 = ggboxplot(genes_wmeta_filter9, x = "Group", y = "value", 
                  add = "jitter", color = "Group", fill = "BG", 
                  ylab = "Tlr12 Expression") +
  scale_color_manual(values = c("Human Adult" = "#a6cee3",
                                "Human Infant" = "#1f78b4", 
                                "Macaque Adult" = "#fb9a99",
                                "Macaque Infant" = "#e31a1c", 
                                "Squirrel Monkey Adult" = "#b2df8a", 
                                "Squirrel Monkey Infant" = "#33a02c")) +
  scale_fill_manual(values = c("RPBG" = "lightgrey", "SPBG" = "white"))
gene9 = ggpar(gene9, legend = "right") + 
  rremove("xlab") + rremove("x.text") + rremove("legend.title")

gene10 = ggboxplot(genes_wmeta_filter10, x = "Group", y = "value", 
                   add = "jitter", color = "Group", fill = "BG", 
                   ylab = "ENSMUSG00000060548\nExpression") +
  scale_color_manual(values = c("Human Adult" = "#a6cee3",
                                "Human Infant" = "#1f78b4", 
                                "Macaque Adult" = "#fb9a99",
                                "Macaque Infant" = "#e31a1c", 
                                "Squirrel Monkey Adult" = "#b2df8a", 
                                "Squirrel Monkey Infant" = "#33a02c")) +
  scale_fill_manual(values = c("RPBG" = "lightgrey", "SPBG" = "white"))
gene10 = ggpar(gene10, legend = "right") + 
  rremove("xlab") + rremove("x.text") + rremove("legend.title")

gene11 = ggboxplot(genes_wmeta_filter11, x = "Group", y = "value", 
                   add = "jitter", color = "Group", fill = "BG", 
                   ylab = "Slc17a8 Expression") +
  scale_color_manual(values = c("Human Adult" = "#a6cee3",
                                "Human Infant" = "#1f78b4", 
                                "Macaque Adult" = "#fb9a99",
                                "Macaque Infant" = "#e31a1c", 
                                "Squirrel Monkey Adult" = "#b2df8a", 
                                "Squirrel Monkey Infant" = "#33a02c")) +
  scale_fill_manual(values = c("RPBG" = "lightgrey", "SPBG" = "white"))
gene11 = ggpar(gene11, legend = "right") + 
  rremove("xlab") + rremove("x.text") + rremove("legend.title")

gene12 = ggboxplot(genes_wmeta_filter12, x = "Group", y = "value",  
                   add = "jitter", color = "Group", fill = "BG", 
                   ylab = "Cyp3a11 Expression") +
  scale_color_manual(values = c("Human Adult" = "#a6cee3",
                                "Human Infant" = "#1f78b4", 
                                "Macaque Adult" = "#fb9a99",
                                "Macaque Infant" = "#e31a1c", 
                                "Squirrel Monkey Adult" = "#b2df8a", 
                                "Squirrel Monkey Infant" = "#33a02c")) +
  scale_fill_manual(values = c("RPBG" = "lightgrey", "SPBG" = "white"))
gene12 = ggpar(gene12, legend = "right") + 
  rremove("xlab") + rremove("x.text") + rremove("legend.title")

gene13 = ggboxplot(genes_wmeta_filter13, x = "Group", y = "value",  
                   add = "jitter", color = "Group", fill = "BG", 
                   ylab = "Me1 Expression") +
  scale_color_manual(values = c("Human Adult" = "#a6cee3",
                                "Human Infant" = "#1f78b4", 
                                "Macaque Adult" = "#fb9a99",
                                "Macaque Infant" = "#e31a1c", 
                                "Squirrel Monkey Adult" = "#b2df8a", 
                                "Squirrel Monkey Infant" = "#33a02c")) +
  scale_fill_manual(values = c("RPBG" = "lightgrey", "SPBG" = "white"))
gene13 = ggpar(gene13, legend = "right") + 
  rremove("xlab") + rremove("x.text") + rremove("legend.title")

gene14 = ggboxplot(genes_wmeta_filter14, x = "Group", y = "value",  
                   add = "jitter", color = "Group", fill = "BG", 
                   ylab = "Retsat Expression") +
  scale_color_manual(values = c("Human Adult" = "#a6cee3",
                                "Human Infant" = "#1f78b4", 
                                "Macaque Adult" = "#fb9a99",
                                "Macaque Infant" = "#e31a1c", 
                                "Squirrel Monkey Adult" = "#b2df8a", 
                                "Squirrel Monkey Infant" = "#33a02c")) +
  scale_fill_manual(values = c("RPBG" = "lightgrey", "SPBG" = "white"))
gene14 = ggpar(gene14, legend = "right") + 
  rremove("xlab") + rremove("x.text") + rremove("legend.title")

gene15 = ggboxplot(genes_wmeta_filter15, x = "Group", y = "value", 
                   add = "jitter", color = "Group", fill = "BG", 
                   ylab = "Acaa1b Expression") +
  scale_color_manual(values = c("Human Adult" = "#a6cee3",
                                "Human Infant" = "#1f78b4", 
                                "Macaque Adult" = "#fb9a99",
                                "Macaque Infant" = "#e31a1c", 
                                "Squirrel Monkey Adult" = "#b2df8a", 
                                "Squirrel Monkey Infant" = "#33a02c")) +
  scale_fill_manual(values = c("RPBG" = "lightgrey", "SPBG" = "white"))
gene15 = ggpar(gene15, legend = "right") + 
  rremove("xlab") + rremove("x.text") + rremove("legend.title")

gene16 = ggboxplot(genes_wmeta_filter16, x = "Group", y = "value", 
                   add = "jitter", color = "Group", fill = "BG", 
                   ylab = "ENSMUSG00000035560\nExpression") +
  scale_color_manual(values = c("Human Adult" = "#a6cee3",
                                "Human Infant" = "#1f78b4", 
                                "Macaque Adult" = "#fb9a99",
                                "Macaque Infant" = "#e31a1c", 
                                "Squirrel Monkey Adult" = "#b2df8a", 
                                "Squirrel Monkey Infant" = "#33a02c")) +
  scale_fill_manual(values = c("RPBG" = "lightgrey", "SPBG" = "white"))
gene16 = ggpar(gene16, legend = "right") + 
  rremove("xlab") + rremove("x.text") + rremove("legend.title")

gene17 = ggboxplot(genes_wmeta_filter17, x = "Group", y = "value",  
                   add = "jitter", color = "Group", fill = "BG", 
                   ylab = "Acot1 Expression") +
  scale_color_manual(values = c("Human Adult" = "#a6cee3",
                                "Human Infant" = "#1f78b4", 
                                "Macaque Adult" = "#fb9a99",
                                "Macaque Infant" = "#e31a1c", 
                                "Squirrel Monkey Adult" = "#b2df8a", 
                                "Squirrel Monkey Infant" = "#33a02c")) +
  scale_fill_manual(values = c("RPBG" = "lightgrey", "SPBG" = "white"))
gene17 = ggpar(gene17, legend = "right") + 
  rremove("xlab") + rremove("x.text") + rremove("legend.title")

gene18 = ggboxplot(genes_wmeta_filter18, x = "Group", y = "value", 
                   add = "jitter", color = "Group", fill = "BG", 
                   ylab = "Csad Expression") +
  scale_color_manual(values = c("Human Adult" = "#a6cee3",
                                "Human Infant" = "#1f78b4", 
                                "Macaque Adult" = "#fb9a99",
                                "Macaque Infant" = "#e31a1c", 
                                "Squirrel Monkey Adult" = "#b2df8a", 
                                "Squirrel Monkey Infant" = "#33a02c")) +
  scale_fill_manual(values = c("RPBG" = "lightgrey", "SPBG" = "white"))
gene18 = ggpar(gene18, legend = "right") + 
  rremove("xlab") + rremove("x.text") + rremove("legend.title")

gene19 = ggboxplot(genes_wmeta_filter19, x = "Group", y = "value", 
                   add = "jitter", color = "Group", fill = "BG", 
                   ylab = "ENSMUSG00000073758\nExpression") +
  scale_color_manual(values = c("Human Adult" = "#a6cee3",
                                "Human Infant" = "#1f78b4", 
                                "Macaque Adult" = "#fb9a99",
                                "Macaque Infant" = "#e31a1c", 
                                "Squirrel Monkey Adult" = "#b2df8a", 
                                "Squirrel Monkey Infant" = "#33a02c")) +
  scale_fill_manual(values = c("RPBG" = "lightgrey", "SPBG" = "white"))
gene19 = ggpar(gene19, legend = "right") + 
  rremove("xlab") + rremove("x.text") + rremove("legend.title")

gene20 = ggboxplot(genes_wmeta_filter20, x = "Group", y = "value", 
                   add = "jitter", color = "Group", fill = "BG", 
                   ylab = "ENSMUSG00000002980\nExpression") +
  scale_color_manual(values = c("Human Adult" = "#a6cee3",
                                "Human Infant" = "#1f78b4", 
                                "Macaque Adult" = "#fb9a99",
                                "Macaque Infant" = "#e31a1c", 
                                "Squirrel Monkey Adult" = "#b2df8a", 
                                "Squirrel Monkey Infant" = "#33a02c")) +
  scale_fill_manual(values = c("RPBG" = "lightgrey", "SPBG" = "white"))
gene20 = ggpar(gene20, legend = "right") + 
  rremove("xlab") + rremove("x.text") + rremove("legend.title")

gene21 = ggboxplot(genes_wmeta_filter21, x = "Group", y = "value", 
                   add = "jitter", color = "Group", fill = "BG", 
                   ylab = "ENSMUSG00000042812\nExpression") +
  scale_color_manual(values = c("Human Adult" = "#a6cee3",
                                "Human Infant" = "#1f78b4", 
                                "Macaque Adult" = "#fb9a99",
                                "Macaque Infant" = "#e31a1c", 
                                "Squirrel Monkey Adult" = "#b2df8a", 
                                "Squirrel Monkey Infant" = "#33a02c")) +
  scale_fill_manual(values = c("RPBG" = "lightgrey", "SPBG" = "white"))
gene21 = ggpar(gene21, legend = "right") + 
  rremove("xlab") + rremove("x.text") + rremove("legend.title")

gene22 = ggboxplot(genes_wmeta_filter22, x = "Group", y = "value",
                   add = "jitter", color = "Group", fill = "BG", 
                   ylab = "Crat Expression") +
  scale_color_manual(values = c("Human Adult" = "#a6cee3",
                                "Human Infant" = "#1f78b4", 
                                "Macaque Adult" = "#fb9a99",
                                "Macaque Infant" = "#e31a1c", 
                                "Squirrel Monkey Adult" = "#b2df8a", 
                                "Squirrel Monkey Infant" = "#33a02c")) +
  scale_fill_manual(values = c("RPBG" = "lightgrey", "SPBG" = "white"))
gene22 = ggpar(gene22, legend = "right") + 
  rremove("xlab") + rremove("x.text") + rremove("legend.title")

gene23 = ggboxplot(genes_wmeta_filter23, x = "Group", y = "value", 
                   add = "jitter", color = "Group", fill = "BG", 
                   ylab = "Cyp4a10 Expression") +
  scale_color_manual(values = c("Human Adult" = "#a6cee3",
                                "Human Infant" = "#1f78b4", 
                                "Macaque Adult" = "#fb9a99",
                                "Macaque Infant" = "#e31a1c", 
                                "Squirrel Monkey Adult" = "#b2df8a", 
                                "Squirrel Monkey Infant" = "#33a02c")) +
  scale_fill_manual(values = c("RPBG" = "lightgrey", "SPBG" = "white"))
gene23 = ggpar(gene23, legend = "right") + 
  rremove("xlab") + rremove("x.text") + rremove("legend.title")

gene24 = ggboxplot(genes_wmeta_filter24, x = "Group", y = "value",  
                   add = "jitter", color = "Group", fill = "BG", 
                   ylab = "Acot3 Expression") +
  scale_color_manual(values = c("Human Adult" = "#a6cee3",
                                "Human Infant" = "#1f78b4", 
                                "Macaque Adult" = "#fb9a99",
                                "Macaque Infant" = "#e31a1c", 
                                "Squirrel Monkey Adult" = "#b2df8a", 
                                "Squirrel Monkey Infant" = "#33a02c")) +
  scale_fill_manual(values = c("RPBG" = "lightgrey", "SPBG" = "white"))
gene24 = ggpar(gene24, legend = "right") + 
  rremove("xlab") + rremove("x.text") + rremove("legend.title")

gene25 = ggboxplot(genes_wmeta_filter25, x = "Group", y = "value", 
                   add = "jitter", color = "Group", fill = "BG", 
                   ylab = "Cyp4a14 Expression") +
  scale_color_manual(values = c("Human Adult" = "#a6cee3",
                                "Human Infant" = "#1f78b4", 
                                "Macaque Adult" = "#fb9a99",
                                "Macaque Infant" = "#e31a1c", 
                                "Squirrel Monkey Adult" = "#b2df8a", 
                                "Squirrel Monkey Infant" = "#33a02c")) +
  scale_fill_manual(values = c("RPBG" = "lightgrey", "SPBG" = "white"))
gene25 = ggpar(gene25, legend = "right") + 
  rremove("xlab") + rremove("x.text") + rremove("legend.title")

gene26 = ggboxplot(genes_wmeta_filter26, x = "Group", y = "value",
                   add = "jitter", color = "Group", fill = "BG", 
                   ylab = "ENSMUSG00000024989\nExpression") +
  scale_color_manual(values = c("Human Adult" = "#a6cee3",
                                "Human Infant" = "#1f78b4", 
                                "Macaque Adult" = "#fb9a99",
                                "Macaque Infant" = "#e31a1c", 
                                "Squirrel Monkey Adult" = "#b2df8a", 
                                "Squirrel Monkey Infant" = "#33a02c")) +
  scale_fill_manual(values = c("RPBG" = "lightgrey", "SPBG" = "white"))
gene26 = ggpar(gene26, legend = "right") + 
  rremove("xlab") + rremove("x.text") + rremove("legend.title")

gene27 = ggboxplot(genes_wmeta_filter27, x = "Group", y = "value",  
                   add = "jitter", color = "Group", fill = "BG", 
                   ylab = "ENSMUSG00000029272\nExpression") +
  scale_color_manual(values = c("Human Adult" = "#a6cee3",
                                "Human Infant" = "#1f78b4", 
                                "Macaque Adult" = "#fb9a99",
                                "Macaque Infant" = "#e31a1c", 
                                "Squirrel Monkey Adult" = "#b2df8a", 
                                "Squirrel Monkey Infant" = "#33a02c")) +
  scale_fill_manual(values = c("RPBG" = "lightgrey", "SPBG" = "white"))
gene27 = ggpar(gene27, legend = "right") + 
  rremove("xlab") + rremove("x.text") + rremove("legend.title")

gene28 = ggboxplot(genes_wmeta_filter28, x = "Group", y = "value", 
                   add = "jitter", color = "Group", fill = "BG", 
                   ylab = "Saa2 Expression") +
  scale_color_manual(values = c("Human Adult" = "#a6cee3",
                                "Human Infant" = "#1f78b4", 
                                "Macaque Adult" = "#fb9a99",
                                "Macaque Infant" = "#e31a1c", 
                                "Squirrel Monkey Adult" = "#b2df8a", 
                                "Squirrel Monkey Infant" = "#33a02c")) +
  scale_fill_manual(values = c("RPBG" = "lightgrey", "SPBG" = "white"))
gene28 = ggpar(gene28, legend = "right") + 
  rremove("xlab") + rremove("x.text") + rremove("legend.title")

gene29 = ggboxplot(genes_wmeta_filter29, x = "Group", y = "value",  
                   add = "jitter", color = "Group", fill = "BG", 
                   ylab = "Gm8893 Expression") +
  scale_color_manual(values = c("Human Adult" = "#a6cee3",
                                "Human Infant" = "#1f78b4", 
                                "Macaque Adult" = "#fb9a99",
                                "Macaque Infant" = "#e31a1c", 
                                "Squirrel Monkey Adult" = "#b2df8a", 
                                "Squirrel Monkey Infant" = "#33a02c")) +
  scale_fill_manual(values = c("RPBG" = "lightgrey", "SPBG" = "white"))
gene29 = ggpar(gene29, legend = "right") + 
  rremove("xlab") + rremove("x.text") + rremove("legend.title")

gene30 = ggboxplot(genes_wmeta_filter30, x = "Group", y = "value",  
                   add = "jitter", color = "Group", fill = "BG", 
                   ylab = "Marco Expression") +
  scale_color_manual(values = c("Human Adult" = "#a6cee3",
                                "Human Infant" = "#1f78b4", 
                                "Macaque Adult" = "#fb9a99",
                                "Macaque Infant" = "#e31a1c", 
                                "Squirrel Monkey Adult" = "#b2df8a", 
                                "Squirrel Monkey Infant" = "#33a02c")) +
  scale_fill_manual(values = c("RPBG" = "lightgrey", "SPBG" = "white"))
gene30 = ggpar(gene30, legend = "right") + 
  rremove("xlab") + rremove("x.text") + rremove("legend.title")

gene31 = ggboxplot(genes_wmeta_filter31, x = "Group", y = "value",  
                   add = "jitter", color = "Group", fill = "BG", 
                   ylab = "Serpina3c Expression") +
  scale_color_manual(values = c("Human Adult" = "#a6cee3",
                                "Human Infant" = "#1f78b4", 
                                "Macaque Adult" = "#fb9a99",
                                "Macaque Infant" = "#e31a1c", 
                                "Squirrel Monkey Adult" = "#b2df8a", 
                                "Squirrel Monkey Infant" = "#33a02c")) +
  scale_fill_manual(values = c("RPBG" = "lightgrey", "SPBG" = "white"))
gene31 = ggpar(gene31, legend = "right") + 
  rremove("xlab") + rremove("x.text") + rremove("legend.title")

gene32 = ggboxplot(genes_wmeta_filter32, x = "Group", y = "value",  
                   add = "jitter", color = "Group", fill = "BG", 
                   ylab = "Saa1 Expression") +
  scale_color_manual(values = c("Human Adult" = "#a6cee3",
                                "Human Infant" = "#1f78b4", 
                                "Macaque Adult" = "#fb9a99",
                                "Macaque Infant" = "#e31a1c", 
                                "Squirrel Monkey Adult" = "#b2df8a", 
                                "Squirrel Monkey Infant" = "#33a02c")) +
  scale_fill_manual(values = c("RPBG" = "lightgrey", "SPBG" = "white"))
gene32 = ggpar(gene32, legend = "right") + 
  rremove("xlab") + rremove("x.text") + rremove("legend.title")

gene33 = ggboxplot(genes_wmeta_filter33, x = "Group", y = "value", 
                   add = "jitter", color = "Group", fill = "BG", 
                   ylab = "Olfr1085 Expression") +
  scale_color_manual(values = c("Human Adult" = "#a6cee3",
                                "Human Infant" = "#1f78b4", 
                                "Macaque Adult" = "#fb9a99",
                                "Macaque Infant" = "#e31a1c", 
                                "Squirrel Monkey Adult" = "#b2df8a", 
                                "Squirrel Monkey Infant" = "#33a02c")) +
  scale_fill_manual(values = c("RPBG" = "lightgrey", "SPBG" = "white"))
gene33 = ggpar(gene33, legend = "right") + 
  rremove("xlab") + rremove("x.text") + rremove("legend.title")

gene34 = ggboxplot(genes_wmeta_filter34, x = "Group", y = "value", 
                   add = "jitter", color = "Group", fill = "BG", 
                   ylab = "Obp2a Expression") +
  scale_color_manual(values = c("Human Adult" = "#a6cee3",
                                "Human Infant" = "#1f78b4", 
                                "Macaque Adult" = "#fb9a99",
                                "Macaque Infant" = "#e31a1c", 
                                "Squirrel Monkey Adult" = "#b2df8a", 
                                "Squirrel Monkey Infant" = "#33a02c")) +
  scale_fill_manual(values = c("RPBG" = "lightgrey", "SPBG" = "white"))
gene34 = ggpar(gene34, legend = "right") + 
  rremove("xlab") + rremove("x.text") + rremove("legend.title")

gene35 = ggboxplot(genes_wmeta_filter35, x = "Group", y = "value",  
                   add = "jitter", color = "Group", fill = "BG", 
                   ylab = "ENSMUSG00000062209\nExpression") +
  scale_color_manual(values = c("Human Adult" = "#a6cee3",
                                "Human Infant" = "#1f78b4", 
                                "Macaque Adult" = "#fb9a99",
                                "Macaque Infant" = "#e31a1c", 
                                "Squirrel Monkey Adult" = "#b2df8a", 
                                "Squirrel Monkey Infant" = "#33a02c")) +
  scale_fill_manual(values = c("RPBG" = "lightgrey", "SPBG" = "white"))
gene35 = ggpar(gene35, legend = "right") + 
  rremove("xlab") + rremove("x.text") + rremove("legend.title")

gene36 = ggboxplot(genes_wmeta_filter36, x = "Group", y = "value", 
                   add = "jitter", color = "Group", fill = "BG", 
                   ylab = "Gpr141b Expression") +
  scale_color_manual(values = c("Human Adult" = "#a6cee3",
                                "Human Infant" = "#1f78b4", 
                                "Macaque Adult" = "#fb9a99",
                                "Macaque Infant" = "#e31a1c", 
                                "Squirrel Monkey Adult" = "#b2df8a", 
                                "Squirrel Monkey Infant" = "#33a02c")) +
  scale_fill_manual(values = c("RPBG" = "lightgrey", "SPBG" = "white"))
gene36 = ggpar(gene36, legend = "right") + 
  rremove("xlab") + rremove("x.text") + rremove("legend.title")

gene37 = ggboxplot(genes_wmeta_filter37, x = "Group", y = "value",  
                   add = "jitter", color = "Group", fill = "BG", 
                   ylab = "Orm3 Expression") +
  scale_color_manual(values = c("Human Adult" = "#a6cee3",
                                "Human Infant" = "#1f78b4", 
                                "Macaque Adult" = "#fb9a99",
                                "Macaque Infant" = "#e31a1c", 
                                "Squirrel Monkey Adult" = "#b2df8a", 
                                "Squirrel Monkey Infant" = "#33a02c")) +
  scale_fill_manual(values = c("RPBG" = "lightgrey", "SPBG" = "white"))
gene37 = ggpar(gene37, legend = "right") + 
  rremove("xlab") + rremove("x.text") + rremove("legend.title")

gene38 = ggboxplot(genes_wmeta_filter38, x = "Group", y = "value",
                   add = "jitter", color = "Group", fill = "BG", 
                   ylab = "Gbp11 Expression") +
  scale_color_manual(values = c("Human Adult" = "#a6cee3",
                                "Human Infant" = "#1f78b4", 
                                "Macaque Adult" = "#fb9a99",
                                "Macaque Infant" = "#e31a1c", 
                                "Squirrel Monkey Adult" = "#b2df8a", 
                                "Squirrel Monkey Infant" = "#33a02c")) +
  scale_fill_manual(values = c("RPBG" = "lightgrey", "SPBG" = "white"))
gene38 = ggpar(gene38, legend = "right") + 
  rremove("xlab") + rremove("x.text") + rremove("legend.title")

gene39 = ggboxplot(genes_wmeta_filter39, x = "Group", y = "value",  
                   add = "jitter", color = "Group", fill = "BG", 
                   ylab = "Mt2 Expression") +
  scale_color_manual(values = c("Human Adult" = "#a6cee3",
                                "Human Infant" = "#1f78b4", 
                                "Macaque Adult" = "#fb9a99",
                                "Macaque Infant" = "#e31a1c", 
                                "Squirrel Monkey Adult" = "#b2df8a", 
                                "Squirrel Monkey Infant" = "#33a02c")) +
  scale_fill_manual(values = c("RPBG" = "lightgrey", "SPBG" = "white"))
gene39 = ggpar(gene39, legend = "right") + 
  rremove("xlab") + rremove("x.text") + rremove("legend.title")

gene40 = ggboxplot(genes_wmeta_filter40, x = "Group", y = "value",  
                   add = "jitter", color = "Group", fill = "BG", 
                   ylab = "Mmd2 Expression") +
  scale_color_manual(values = c("Human Adult" = "#a6cee3",
                                "Human Infant" = "#1f78b4", 
                                "Macaque Adult" = "#fb9a99",
                                "Macaque Infant" = "#e31a1c", 
                                "Squirrel Monkey Adult" = "#b2df8a", 
                                "Squirrel Monkey Infant" = "#33a02c")) +
  scale_fill_manual(values = c("RPBG" = "lightgrey", "SPBG" = "white"))
gene40 = ggpar(gene40, legend = "right") + 
  rremove("xlab") + rremove("x.text") + rremove("legend.title")

gene41 = ggboxplot(genes_wmeta_filter41, x = "Group", y = "value", 
                   add = "jitter", color = "Group", fill = "BG", 
                   ylab = "Tff3 Expression") +
  scale_color_manual(values = c("Human Adult" = "#a6cee3",
                                "Human Infant" = "#1f78b4", 
                                "Macaque Adult" = "#fb9a99",
                                "Macaque Infant" = "#e31a1c", 
                                "Squirrel Monkey Adult" = "#b2df8a", 
                                "Squirrel Monkey Infant" = "#33a02c")) +
  scale_fill_manual(values = c("RPBG" = "lightgrey", "SPBG" = "white"))
gene41 = ggpar(gene41, legend = "right") + 
  rremove("xlab") + rremove("x.text") + rremove("legend.title")

gene42 = ggboxplot(genes_wmeta_filter42, x = "Group", y = "value",  
                   add = "jitter", color = "Group", fill = "BG", 
                   ylab = "Uncx Expression") +
  scale_color_manual(values = c("Human Adult" = "#a6cee3",
                                "Human Infant" = "#1f78b4", 
                                "Macaque Adult" = "#fb9a99",
                                "Macaque Infant" = "#e31a1c", 
                                "Squirrel Monkey Adult" = "#b2df8a", 
                                "Squirrel Monkey Infant" = "#33a02c")) +
  scale_fill_manual(values = c("RPBG" = "lightgrey", "SPBG" = "white"))
gene42 = ggpar(gene42, legend = "right") + 
  rremove("xlab") + rremove("x.text") + rremove("legend.title")

gene43 = ggboxplot(genes_wmeta_filter43, x = "Group", y = "value",  
                   add = "jitter", color = "Group", fill = "BG", 
                   ylab = "Isyna1 Expression") +
  scale_color_manual(values = c("Human Adult" = "#a6cee3",
                                "Human Infant" = "#1f78b4", 
                                "Macaque Adult" = "#fb9a99",
                                "Macaque Infant" = "#e31a1c", 
                                "Squirrel Monkey Adult" = "#b2df8a", 
                                "Squirrel Monkey Infant" = "#33a02c")) +
  scale_fill_manual(values = c("RPBG" = "lightgrey", "SPBG" = "white"))
gene43 = ggpar(gene43, legend = "right") + 
  rremove("xlab") + rremove("x.text") + rremove("legend.title")

gene44 = ggboxplot(genes_wmeta_filter44, x = "Group", y = "value",  
                   add = "jitter", color = "Group", fill = "BG", 
                   ylab = "Orm2 Expression") +
  scale_color_manual(values = c("Human Adult" = "#a6cee3",
                                "Human Infant" = "#1f78b4", 
                                "Macaque Adult" = "#fb9a99",
                                "Macaque Infant" = "#e31a1c", 
                                "Squirrel Monkey Adult" = "#b2df8a", 
                                "Squirrel Monkey Infant" = "#33a02c")) +
  scale_fill_manual(values = c("RPBG" = "lightgrey", "SPBG" = "white"))
gene44 = ggpar(gene44, legend = "right") + 
  rremove("xlab") + rremove("x.text") + rremove("legend.title")

gene45 = ggboxplot(genes_wmeta_filter45, x = "Group", y = "value",  
                   add = "jitter", color = "Group", fill = "BG", 
                   ylab = "ENSMUSG00000005553\nExpression") +
  scale_color_manual(values = c("Human Adult" = "#a6cee3",
                                "Human Infant" = "#1f78b4", 
                                "Macaque Adult" = "#fb9a99",
                                "Macaque Infant" = "#e31a1c", 
                                "Squirrel Monkey Adult" = "#b2df8a", 
                                "Squirrel Monkey Infant" = "#33a02c")) +
  scale_fill_manual(values = c("RPBG" = "lightgrey", "SPBG" = "white"))
gene45 = ggpar(gene45, legend = "right") + 
  rremove("xlab") + rremove("x.text") + rremove("legend.title")

gene46 = ggboxplot(genes_wmeta_filter46, x = "Group", y = "value", 
                   add = "jitter", color = "Group", fill = "BG", 
                   ylab = "Vmn2r117 Expression") +
  scale_color_manual(values = c("Human Adult" = "#a6cee3",
                                "Human Infant" = "#1f78b4", 
                                "Macaque Adult" = "#fb9a99",
                                "Macaque Infant" = "#e31a1c", 
                                "Squirrel Monkey Adult" = "#b2df8a", 
                                "Squirrel Monkey Infant" = "#33a02c")) +
  scale_fill_manual(values = c("RPBG" = "lightgrey", "SPBG" = "white"))
gene46 = ggpar(gene46, legend = "right") + 
  rremove("xlab") + rremove("x.text") + rremove("legend.title")

gene47 = ggboxplot(genes_wmeta_filter47, x = "Group", y = "value",  
                   add = "jitter", color = "Group", fill = "BG", 
                   ylab = "ENSMUSG00000034116\nExpression") +
  scale_color_manual(values = c("Human Adult" = "#a6cee3",
                                "Human Infant" = "#1f78b4", 
                                "Macaque Adult" = "#fb9a99",
                                "Macaque Infant" = "#e31a1c", 
                                "Squirrel Monkey Adult" = "#b2df8a", 
                                "Squirrel Monkey Infant" = "#33a02c")) +
  scale_fill_manual(values = c("RPBG" = "lightgrey", "SPBG" = "white"))
gene47 = ggpar(gene47, legend = "right") + 
  rremove("xlab") + rremove("x.text") + rremove("legend.title")

gene48 = ggboxplot(genes_wmeta_filter48, x = "Group", y = "value",  
                   add = "jitter", color = "Group", fill = "BG", 
                   ylab = "Cdkn1a Expression") +
  scale_color_manual(values = c("Human Adult" = "#a6cee3",
                                "Human Infant" = "#1f78b4", 
                                "Macaque Adult" = "#fb9a99",
                                "Macaque Infant" = "#e31a1c", 
                                "Squirrel Monkey Adult" = "#b2df8a", 
                                "Squirrel Monkey Infant" = "#33a02c")) +
  scale_fill_manual(values = c("RPBG" = "lightgrey", "SPBG" = "white"))
gene48 = ggpar(gene48, legend = "right") + 
  rremove("xlab") + rremove("x.text") + rremove("legend.title")

gene49 = ggboxplot(genes_wmeta_filter49, x = "Group", y = "value",  
                   add = "jitter", color = "Group", fill = "BG", 
                   ylab = "Mt1 Expression") +
  scale_color_manual(values = c("Human Adult" = "#a6cee3",
                                "Human Infant" = "#1f78b4", 
                                "Macaque Adult" = "#fb9a99",
                                "Macaque Infant" = "#e31a1c", 
                                "Squirrel Monkey Adult" = "#b2df8a", 
                                "Squirrel Monkey Infant" = "#33a02c")) +
  scale_fill_manual(values = c("RPBG" = "lightgrey", "SPBG" = "white"))
gene49 = ggpar(gene49, legend = "right") + 
  rremove("xlab") + rremove("x.text") + rremove("legend.title")

gene50 = ggboxplot(genes_wmeta_filter50, x = "Group", y = "value",  
                   add = "jitter", color = "Group", fill = "BG", 
                   ylab = "Alox12 Expression") +
  scale_color_manual(values = c("Human Adult" = "#a6cee3",
                                "Human Infant" = "#1f78b4", 
                                "Macaque Adult" = "#fb9a99",
                                "Macaque Infant" = "#e31a1c", 
                                "Squirrel Monkey Adult" = "#b2df8a", 
                                "Squirrel Monkey Infant" = "#33a02c")) +
  scale_fill_manual(values = c("RPBG" = "lightgrey", "SPBG" = "white"))
gene50 = ggpar(gene50, legend = "right") + 
  rremove("xlab") + rremove("x.text") + rremove("legend.title")

gene51 = ggboxplot(genes_wmeta_filter51, x = "Group", y = "value",  
                   add = "jitter", color = "Group", fill = "BG", 
                   ylab = "Vmn2r65 Expression") +
  scale_color_manual(values = c("Human Adult" = "#a6cee3",
                                "Human Infant" = "#1f78b4", 
                                "Macaque Adult" = "#fb9a99",
                                "Macaque Infant" = "#e31a1c", 
                                "Squirrel Monkey Adult" = "#b2df8a", 
                                "Squirrel Monkey Infant" = "#33a02c")) +
  scale_fill_manual(values = c("RPBG" = "lightgrey", "SPBG" = "white"))
gene51 = ggpar(gene51, legend = "right") + 
  rremove("xlab") + rremove("x.text") + rremove("legend.title")

gene52 = ggboxplot(genes_wmeta_filter52, x = "Group", y = "value",  
                   add = "jitter", color = "Group", fill = "BG", 
                   ylab = "Il1r1 Expression") +
  scale_color_manual(values = c("Human Adult" = "#a6cee3",
                                "Human Infant" = "#1f78b4", 
                                "Macaque Adult" = "#fb9a99",
                                "Macaque Infant" = "#e31a1c", 
                                "Squirrel Monkey Adult" = "#b2df8a", 
                                "Squirrel Monkey Infant" = "#33a02c")) +
  scale_fill_manual(values = c("RPBG" = "lightgrey", "SPBG" = "white"))
gene52 = ggpar(gene52, legend = "right") + 
  rremove("xlab") + rremove("x.text") + rremove("legend.title")

gene53 = ggboxplot(genes_wmeta_filter53, x = "Group", y = "value",  
                   add = "jitter", color = "Group", fill = "BG", 
                   ylab = "Selenbp2 Expression") +
  scale_color_manual(values = c("Human Adult" = "#a6cee3",
                                "Human Infant" = "#1f78b4", 
                                "Macaque Adult" = "#fb9a99",
                                "Macaque Infant" = "#e31a1c", 
                                "Squirrel Monkey Adult" = "#b2df8a", 
                                "Squirrel Monkey Infant" = "#33a02c")) +
  scale_fill_manual(values = c("RPBG" = "lightgrey", "SPBG" = "white"))
gene53 = ggpar(gene53, legend = "right") + 
  rremove("xlab") + rremove("x.text") + rremove("legend.title")

gene54 = ggboxplot(genes_wmeta_filter54, x = "Group", y = "value", 
                   add = "jitter", color = "Group", fill = "BG", 
                   ylab = "Mug2 Expression") +
  scale_color_manual(values = c("Human Adult" = "#a6cee3",
                                "Human Infant" = "#1f78b4", 
                                "Macaque Adult" = "#fb9a99",
                                "Macaque Infant" = "#e31a1c", 
                                "Squirrel Monkey Adult" = "#b2df8a", 
                                "Squirrel Monkey Infant" = "#33a02c")) +
  scale_fill_manual(values = c("RPBG" = "lightgrey", "SPBG" = "white"))
gene54 = ggpar(gene54, legend = "right") + 
  rremove("xlab") + rremove("x.text") + rremove("legend.title")

gene55 = ggboxplot(genes_wmeta_filter55, x = "Group", y = "value",  
                   add = "jitter", color = "Group", fill = "BG", 
                   ylab = "Gm47355 Expression") +
  scale_color_manual(values = c("Human Adult" = "#a6cee3",
                                "Human Infant" = "#1f78b4", 
                                "Macaque Adult" = "#fb9a99",
                                "Macaque Infant" = "#e31a1c", 
                                "Squirrel Monkey Adult" = "#b2df8a", 
                                "Squirrel Monkey Infant" = "#33a02c")) +
  scale_fill_manual(values = c("RPBG" = "lightgrey", "SPBG" = "white"))
gene55 = ggpar(gene55, legend = "right") + 
  rremove("xlab") + rremove("x.text") + rremove("legend.title")

gene56 = ggboxplot(genes_wmeta_filter56, x = "Group", y = "value",  
                   add = "jitter", color = "Group", fill = "BG", 
                   ylab = "Gm46136 Expression") +
  scale_color_manual(values = c("Human Adult" = "#a6cee3",
                                "Human Infant" = "#1f78b4", 
                                "Macaque Adult" = "#fb9a99",
                                "Macaque Infant" = "#e31a1c", 
                                "Squirrel Monkey Adult" = "#b2df8a", 
                                "Squirrel Monkey Infant" = "#33a02c")) +
  scale_fill_manual(values = c("RPBG" = "lightgrey", "SPBG" = "white"))
gene56 = ggpar(gene56, legend = "right") + 
  rremove("xlab") + rremove("x.text") + rremove("legend.title")

gene57 = ggboxplot(genes_wmeta_filter57, x = "Group", y = "value", 
                   add = "jitter", color = "Group", fill = "BG", 
                   ylab = "Vmn1r234 Expression") +
  scale_color_manual(values = c("Human Adult" = "#a6cee3",
                                "Human Infant" = "#1f78b4", 
                                "Macaque Adult" = "#fb9a99",
                                "Macaque Infant" = "#e31a1c", 
                                "Squirrel Monkey Adult" = "#b2df8a", 
                                "Squirrel Monkey Infant" = "#33a02c")) +
  scale_fill_manual(values = c("RPBG" = "lightgrey", "SPBG" = "white"))
gene57 = ggpar(gene57, legend = "right") + 
  rremove("xlab") + rremove("x.text") + rremove("legend.title")

gene58 = ggboxplot(genes_wmeta_filter58, x = "Group", y = "value",  
                   add = "jitter", color = "Group", fill = "BG", 
                   ylab = "Gm10319 Expression") +
  scale_color_manual(values = c("Human Adult" = "#a6cee3",
                                "Human Infant" = "#1f78b4", 
                                "Macaque Adult" = "#fb9a99",
                                "Macaque Infant" = "#e31a1c", 
                                "Squirrel Monkey Adult" = "#b2df8a", 
                                "Squirrel Monkey Infant" = "#33a02c")) +
  scale_fill_manual(values = c("RPBG" = "lightgrey", "SPBG" = "white"))
gene58 = ggpar(gene58, legend = "right") + 
  rremove("xlab") + rremove("x.text") + rremove("legend.title")

gene59 = ggboxplot(genes_wmeta_filter59, x = "Group", y = "value", 
                   add = "jitter", color = "Group", fill = "BG", 
                   ylab = "Olfr901 Expression") +
  scale_color_manual(values = c("Human Adult" = "#a6cee3",
                                "Human Infant" = "#1f78b4", 
                                "Macaque Adult" = "#fb9a99",
                                "Macaque Infant" = "#e31a1c", 
                                "Squirrel Monkey Adult" = "#b2df8a", 
                                "Squirrel Monkey Infant" = "#33a02c")) +
  scale_fill_manual(values = c("RPBG" = "lightgrey", "SPBG" = "white"))
gene59 = ggpar(gene59, legend = "right") + 
  rremove("xlab") + rremove("x.text") + rremove("legend.title")

gene60 = ggboxplot(genes_wmeta_filter60, x = "Group", y = "value", 
                   add = "jitter", color = "Group", fill = "BG", 
                   ylab = "Clec2h Expression") +
  scale_color_manual(values = c("Human Adult" = "#a6cee3",
                                "Human Infant" = "#1f78b4", 
                                "Macaque Adult" = "#fb9a99",
                                "Macaque Infant" = "#e31a1c", 
                                "Squirrel Monkey Adult" = "#b2df8a", 
                                "Squirrel Monkey Infant" = "#33a02c")) +
  scale_fill_manual(values = c("RPBG" = "lightgrey", "SPBG" = "white"))
gene60 = ggpar(gene60, legend = "right") + 
  rremove("xlab") + rremove("x.text") + rremove("legend.title")

gene61 = ggboxplot(genes_wmeta_filter61, x = "Group", y = "value", 
                   add = "jitter", color = "Group", fill = "BG", 
                   ylab = "Spp1 Expression") +
  scale_color_manual(values = c("Human Adult" = "#a6cee3",
                                "Human Infant" = "#1f78b4", 
                                "Macaque Adult" = "#fb9a99",
                                "Macaque Infant" = "#e31a1c", 
                                "Squirrel Monkey Adult" = "#b2df8a", 
                                "Squirrel Monkey Infant" = "#33a02c")) +
  scale_fill_manual(values = c("RPBG" = "lightgrey", "SPBG" = "white"))
gene61 = ggpar(gene61, legend = "right") + 
  rremove("xlab") + rremove("x.text") + rremove("legend.title")

gene62 = ggboxplot(genes_wmeta_filter62, x = "Group", y = "value", 
                   add = "jitter", color = "Group", fill = "BG", 
                   ylab = "Saa3 Expression") +
  scale_color_manual(values = c("Human Adult" = "#a6cee3",
                                "Human Infant" = "#1f78b4", 
                                "Macaque Adult" = "#fb9a99",
                                "Macaque Infant" = "#e31a1c", 
                                "Squirrel Monkey Adult" = "#b2df8a", 
                                "Squirrel Monkey Infant" = "#33a02c")) +
  scale_fill_manual(values = c("RPBG" = "lightgrey", "SPBG" = "white"))
gene62 = ggpar(gene62, legend = "right") + 
  rremove("xlab") + rremove("x.text") + rremove("legend.title")

gene63 = ggboxplot(genes_wmeta_filter63, x = "Group", y = "value",  
                   add = "jitter", color = "Group", fill = "BG", 
                   ylab = "Cyp7b1 Expression") +
  scale_color_manual(values = c("Human Adult" = "#a6cee3",
                                "Human Infant" = "#1f78b4", 
                                "Macaque Adult" = "#fb9a99",
                                "Macaque Infant" = "#e31a1c", 
                                "Squirrel Monkey Adult" = "#b2df8a", 
                                "Squirrel Monkey Infant" = "#33a02c")) +
  scale_fill_manual(values = c("RPBG" = "lightgrey", "SPBG" = "white"))
gene63 = ggpar(gene63, legend = "right") + 
  rremove("xlab") + rremove("x.text") + rremove("legend.title")

gene64 = ggboxplot(genes_wmeta_filter64, x = "Group", y = "value",  
                   add = "jitter", color = "Group", fill = "BG", 
                   ylab = "Cfp Expression") +
  scale_color_manual(values = c("Human Adult" = "#a6cee3",
                                "Human Infant" = "#1f78b4", 
                                "Macaque Adult" = "#fb9a99",
                                "Macaque Infant" = "#e31a1c", 
                                "Squirrel Monkey Adult" = "#b2df8a", 
                                "Squirrel Monkey Infant" = "#33a02c")) +
  scale_fill_manual(values = c("RPBG" = "lightgrey", "SPBG" = "white"))
gene64 = ggpar(gene64, legend = "right") + 
  rremove("xlab") + rremove("x.text") + rremove("legend.title")

setwd("/Users/elizabethmallott/Dropbox/Projects/Gut_microbiome/mouse_inoculation/")

tiff(file="Supplementary_Fig12.tif", res=150, width=50, height=25, units="in")
ggarrange(gene1, gene2, gene3, gene4, gene5, gene6, gene7, gene8, gene9, gene10, gene11,
          gene12, gene13, gene14, gene15, gene16, gene17, gene18, gene19, gene20, gene21,
          gene22, gene23, gene24, gene25, gene26, gene27, gene28, gene29, gene30, gene31,
          gene32, gene33, gene34, gene35, gene36, gene37, gene38, gene39, gene40, gene41,
          gene42, gene43, gene44, gene45, gene46, gene47, gene48, gene49, gene50, gene51,
          gene52, gene53, gene54, gene55, gene56, gene57, gene58, gene59, gene60, gene61, 
          gene62, gene63, gene64, ncol = 11, nrow=6, 
          common.legend = TRUE, legend = "right", align = "hv")
dev.off()

#Differential gene expression for adults

setwd("/Users/elizabethmallott/Dropbox/Projects/Gut_microbiome/mouse_inoculation/RNAseq")

library(tidyverse)
library(ggpubr)

genes = read_csv("transformed_data_to_plot2.csv")
metadata = read_csv("Gene_expression_metadata.csv", col_types = "ccffff")

genes_long = genes %>% pivot_longer(-ensgene, names_to = "sampleid") 

genes_wmeta = genes_long %>% left_join(metadata)

genes_wmeta = genes_wmeta %>% 
  mutate(Group = case_when(Group == "HumanAdult" ~ "Human Adult",
                           Group == "HumanInfant" ~ "Human Infant",
                           Group == "MacaqueAdult" ~ "Macaque Adult",
                           Group == "MacaqueInfant" ~ "Macaque Infant",
                           Group == "Squirrel_monkeyAdult" ~ "Squirrel Monkey Adult",
                           Group == "Squirrel_monkeyInfant" ~ "Squirrel Monkey Infant"))

genes_wmeta$Group<-factor(genes_wmeta$Group,levels=
                            c("Human Adult","Human Infant",
                              "Squirrel Monkey Adult", "Squirrel Monkey Infant",
                              "Macaque Adult", "Macaque Infant"))

genes_wmeta_filter1a = genes_wmeta %>% filter(ensgene == "ENSMUSG00000023044")
genes_wmeta_filter2a = genes_wmeta %>% filter(ensgene == "ENSMUSG00000028051")
genes_wmeta_filter3a = genes_wmeta %>% filter(ensgene == "ENSMUSG00000027533")
genes_wmeta_filter4a = genes_wmeta %>% filter(ensgene == "ENSMUSG00000041237")
genes_wmeta_filter5a = genes_wmeta %>% filter(ensgene == "ENSMUSG00000041801")
genes_wmeta_filter6a = genes_wmeta %>% filter(ensgene == "ENSMUSG00000008035")
genes_wmeta_filter7a = genes_wmeta %>% filter(ensgene == "ENSMUSG00000055137")
genes_wmeta_filter8a = genes_wmeta %>% filter(ensgene == "ENSMUSG00000094793")
genes_wmeta_filter9a = genes_wmeta %>% filter(ensgene == "ENSMUSG00000090389")

gene1a = ggboxplot(genes_wmeta_filter1a, x = "Group", y = "value",  
                   add = "jitter", color = "Group", fill = "BG", 
                   ylab = "Csad Expression") +
  scale_color_manual(values = c("Human Adult" = "#a6cee3",
                                "Human Infant" = "#1f78b4", 
                                "Macaque Adult" = "#fb9a99",
                                "Macaque Infant" = "#e31a1c", 
                                "Squirrel Monkey Adult" = "#b2df8a", 
                                "Squirrel Monkey Infant" = "#33a02c")) +
  scale_fill_manual(values = c("RPBG" = "lightgrey", "SPBG" = "white"))
gene1a = ggpar(gene1a, legend = "right") + 
  rremove("xlab") + rremove("x.text") + rremove("legend.title")

gene2a = ggboxplot(genes_wmeta_filter2a, x = "Group", y = "value", 
                   add = "jitter", color = "Group", fill = "BG", 
                   ylab = "Hcn3 Expression") +
  scale_color_manual(values = c("Human Adult" = "#a6cee3",
                                "Human Infant" = "#1f78b4", 
                                "Macaque Adult" = "#fb9a99",
                                "Macaque Infant" = "#e31a1c", 
                                "Squirrel Monkey Adult" = "#b2df8a", 
                                "Squirrel Monkey Infant" = "#33a02c")) +
  scale_fill_manual(values = c("RPBG" = "lightgrey", "SPBG" = "white"))
gene2a = ggpar(gene2a, legend = "right") + 
  rremove("xlab") + rremove("x.text") + rremove("legend.title")

gene3a = ggboxplot(genes_wmeta_filter3a, x = "Group", y = "value", 
                   add = "jitter", color = "Group", fill = "BG", 
                   ylab = "Fabp5 Expression") +
  scale_color_manual(values = c("Human Adult" = "#a6cee3",
                                "Human Infant" = "#1f78b4", 
                                "Macaque Adult" = "#fb9a99",
                                "Macaque Infant" = "#e31a1c", 
                                "Squirrel Monkey Adult" = "#b2df8a", 
                                "Squirrel Monkey Infant" = "#33a02c")) +
  scale_fill_manual(values = c("RPBG" = "lightgrey", "SPBG" = "white"))
gene3a = ggpar(gene3a, legend = "right") + 
  rremove("xlab") + rremove("x.text") + rremove("legend.title")

gene4a = ggboxplot(genes_wmeta_filter4a, x = "Group", y = "value",  
                   add = "jitter", color = "Group", fill = "BG", 
                   ylab = "ENSMUSG00000041237\nExpression") +
  scale_color_manual(values = c("Human Adult" = "#a6cee3",
                                "Human Infant" = "#1f78b4", 
                                "Macaque Adult" = "#fb9a99",
                                "Macaque Infant" = "#e31a1c", 
                                "Squirrel Monkey Adult" = "#b2df8a", 
                                "Squirrel Monkey Infant" = "#33a02c")) +
  scale_fill_manual(values = c("RPBG" = "lightgrey", "SPBG" = "white"))
gene4a = ggpar(gene4a, legend = "right") + 
  rremove("xlab") + rremove("x.text") + rremove("legend.title")

gene5a = ggboxplot(genes_wmeta_filter5a, x = "Group", y = "value", 
                   add = "jitter", color = "Group", fill = "BG", 
                   ylab = "ENSMUSG00000041801\nExpression") +
  scale_color_manual(values = c("Human Adult" = "#a6cee3",
                                "Human Infant" = "#1f78b4", 
                                "Macaque Adult" = "#fb9a99",
                                "Macaque Infant" = "#e31a1c", 
                                "Squirrel Monkey Adult" = "#b2df8a", 
                                "Squirrel Monkey Infant" = "#33a02c")) +
  scale_fill_manual(values = c("RPBG" = "lightgrey", "SPBG" = "white"))
gene5a = ggpar(gene5a, legend = "right") + 
  rremove("xlab") + rremove("x.text") + rremove("legend.title")

gene6a = ggboxplot(genes_wmeta_filter6a, x = "Group", y = "value", 
                   add = "jitter", color = "Group", fill = "BG", 
                   ylab = "ENSMUSG00000008035\nExpression") +
  scale_color_manual(values = c("Human Adult" = "#a6cee3",
                                "Human Infant" = "#1f78b4", 
                                "Macaque Adult" = "#fb9a99",
                                "Macaque Infant" = "#e31a1c", 
                                "Squirrel Monkey Adult" = "#b2df8a", 
                                "Squirrel Monkey Infant" = "#33a02c")) +
  scale_fill_manual(values = c("RPBG" = "lightgrey", "SPBG" = "white"))
gene6a = ggpar(gene6a, legend = "right") + 
  rremove("xlab") + rremove("x.text") + rremove("legend.title")

gene7a = ggboxplot(genes_wmeta_filter7a, x = "Group", y = "value",  
                   add = "jitter", color = "Group", fill = "BG", 
                   ylab = "Sugct Expression") +
  scale_color_manual(values = c("Human Adult" = "#a6cee3",
                                "Human Infant" = "#1f78b4", 
                                "Macaque Adult" = "#fb9a99",
                                "Macaque Infant" = "#e31a1c", 
                                "Squirrel Monkey Adult" = "#b2df8a", 
                                "Squirrel Monkey Infant" = "#33a02c")) +
  scale_fill_manual(values = c("RPBG" = "lightgrey", "SPBG" = "white"))
gene7a = ggpar(gene7a, legend = "right") + 
  rremove("xlab") + rremove("x.text") + rremove("legend.title")

gene8a = ggboxplot(genes_wmeta_filter8a, x = "Group", y = "value", 
                   add = "jitter", color = "Group", fill = "BG", 
                   ylab = "Mup12 Expression") +
  scale_color_manual(values = c("Human Adult" = "#a6cee3",
                                "Human Infant" = "#1f78b4", 
                                "Macaque Adult" = "#fb9a99",
                                "Macaque Infant" = "#e31a1c", 
                                "Squirrel Monkey Adult" = "#b2df8a", 
                                "Squirrel Monkey Infant" = "#33a02c")) +
  scale_fill_manual(values = c("RPBG" = "lightgrey", "SPBG" = "white"))
gene8a = ggpar(gene8a, legend = "right") + 
  rremove("xlab") + rremove("x.text") + rremove("legend.title")

gene9a = ggboxplot(genes_wmeta_filter9a, x = "Group", y = "value",
                   add = "jitter", color = "Group", fill = "BG", 
                   ylab = "Cdv3-ps Expression") +
  scale_color_manual(values = c("Human Adult" = "#a6cee3",
                                "Human Infant" = "#1f78b4", 
                                "Macaque Adult" = "#fb9a99",
                                "Macaque Infant" = "#e31a1c", 
                                "Squirrel Monkey Adult" = "#b2df8a", 
                                "Squirrel Monkey Infant" = "#33a02c")) +
  scale_fill_manual(values = c("RPBG" = "lightgrey", "SPBG" = "white"))
gene9a = ggpar(gene9a, legend = "right") + 
  rremove("xlab") + rremove("x.text") + rremove("legend.title")

setwd("/Users/elizabethmallott/Dropbox/Projects/Gut_microbiome/mouse_inoculation/")

tiff(file="Supplementary_Fig13.tif", res=150, width=15, height=10, units="in")
ggarrange(gene1a, gene2a, gene3a, gene4a, gene5a, gene6a, gene7a, gene8a, gene9a,
          ncol = 3, nrow=3, 
          common.legend = TRUE, legend = "right", align = "hv")
dev.off()



