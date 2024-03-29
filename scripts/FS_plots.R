source("scripts/FS_cleaning.R")
## Load and install packages, control options ####
## First specify the packages of interest
packages = c("patchwork","plyr","svglite")

## Now load or install&load all
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

## Data ####
data_FS_plots <- data_FS %>% 
  droplevels() %>% 
  mutate(juniorEd = recode(juniorEd, "primary" = "state")) %>%
  filter(style %in% c("interview","minimalpair","wordlist")) %>% 
  droplevels()

data_FST_plots <- data_FST %>% 
  droplevels() %>% 
  mutate(juniorEd = recode(juniorEd, "primary" = "state")) %>%
  filter(style %in% c("interview","minimalpair","wordlist")) %>% 
  droplevels()
  

data_G_plots <- data_G %>% 
  droplevels() %>% 
  mutate(juniorEd = recode(juniorEd, "primary" = "state")) %>%
  filter(style %in% c("interview","minimalpair","wordlist")) %>% 
  droplevels()

data_FSG_plots <- rbind(data_FS_plots,data_G_plots)

corpus_order <- c("CoRP-SE","DECTE-NE","CoRP-NE")
## theme ------------------------------------------
theme_Caitlin_present <- function() {theme_bw(base_size = 22) %+replace%
    theme(plot.background  = element_rect(fill = "transparent", colour = NA),
          panel.background = element_rect(fill="gray90", colour=NA),
          legend.background = element_rect(fill="transparent", colour=NA),
          legend.key = element_rect(fill="transparent", colour=NA),
          panel.grid.major = element_line(colour = "white", size = 0.2), 
          panel.grid.minor = element_line(colour = "grey98", size = 0.5))}

theme_Caitlin <- function() {theme_bw(base_size = 12) %+replace%
    theme(plot.background  = element_rect(fill = "transparent", colour = NA),
          panel.background = element_rect(fill="transparent", colour=NA),
          legend.background = element_rect(fill="transparent", colour=NA),
          legend.key = element_rect(fill="transparent", colour=NA),
          panel.grid.major = element_line(colour = "grey80", size = 0.2), 
          panel.grid.minor = element_line(colour = "grey90", size = 0.5)
          )}

# colours ####
# lexSet colours
FSColours <- c("#C70E7B", "#A6E000")
names(FSColours) = levels(data_FS_plots$lexSet)
FSFillScale <- scale_fill_manual(name = "lexSet", values = FSColours)
FSColScale <- scale_colour_manual(name= "lexSet", values = FSColours)

# region colours
regionColours <- c("#77b144","#524c95","#95524C")
names(regionColours) = levels(data_FS_plots$corpus)
regionFillScale <- scale_fill_manual(name = "corpus",values = regionColours)
regionColScale <- scale_colour_manual(name = "corpus",values=regionColours)

# Predictor eyeball check ####
## strut ####
STRUT_predcheck_F1.plot <- ggplot(data_FS_plots %>% filter(lexSet == "STRUT") %>% filter(corpus == "CoRP-NE"), aes(x = lexSet, y = norm_F1, fill = lexSet)) +
  geom_boxplot() +
  theme_Caitlin() +
  theme(legend.position = "none") +
  xlab("lexical set") +
  ylab("F1 (Hz)") +
  scale_y_continuous(limits = c(200,1000), breaks = seq(0,1000,100))+
  FSFillScale +
  # facet_wrap(~folMan) + #lose
  # facet_wrap(~folPlace) + # lose
  # facet_wrap(~folVc) + #lose
  facet_wrap(~preSeg) + #lose
  # facet_wrap(~folSeq_small) + # lose
  NULL
STRUT_predcheck_F1.plot

STRUT_predcheck_F2.plot <- ggplot(data_FS_plots %>% filter(lexSet == "STRUT"), aes(x = lexSet, y = norm_F2, fill = lexSet)) +
  geom_boxplot() +
  theme_Caitlin() +
  theme(legend.position = "none") +
  xlab("lexical set") +
  ylab("F2 (Hz)") +
  scale_y_continuous(limits = c(600,2200), breaks = seq(0,3000,200))+
  FSFillScale +
  # facet_wrap(~folMan) + #lose
  # facet_wrap(~folPlace) + #lose
  # facet_wrap(~folVc) + #lose
  # facet_wrap(~preSeg) + #lose
  # facet_wrap(~folSeq_small) + #lose
  NULL
STRUT_predcheck_F2.plot

## FOOT ####
FOOT_predcheck_F1.plot <- ggplot(data_FS_plots %>% filter(lexSet == "FOOT"), aes(x = lexSet, y = norm_F1, fill = lexSet)) +
  geom_boxplot() +
  theme_Caitlin() +
  theme(legend.position = "none") +
  xlab("lexical set") +
  ylab("F1 (Hz)") +
  scale_y_continuous(limits = c(200,1000), breaks = seq(0,1000,100))+
  FSFillScale +
  # facet_wrap(~folMan) + #lose
  # facet_wrap(~folPlace) + #lose
  # facet_wrap(~folVc) + #lose
  # facet_wrap(~preSeg) + #lose
  # facet_wrap(~folSeq_small) + #lose
  NULL
FOOT_predcheck_F1.plot

FOOT_predcheck_F2.plot <- ggplot(data_FS_plots %>% filter(lexSet == "FOOT"), aes(x = lexSet, y = norm_F2, fill = lexSet)) +
  geom_boxplot() +
  theme_Caitlin() +
  theme(legend.position = "none") +
  xlab("lexical set") +
  ylab("F2 (Hz)") +
  scale_y_continuous(limits = c(600,2200), breaks = seq(0,3000,200))+
  FSFillScale +
  # facet_wrap(~folMan) + #keep
  # facet_wrap(~folPlace) + #lose
  # facet_wrap(~folVc) +  #lose
  # facet_wrap(~preSeg) +  #keep
  # facet_wrap(~folSeq_small) + #lose
  NULL
FOOT_predcheck_F2.plot

# SE speakers ----------------------
data_FS_plots_SE.means = as_tibble(ddply(data_FS_plots%>% filter(corpus == "CoRP-SE"),.(id,sex,lexSet),summarise,
                                         mean_F2 = mean(norm_F2),
                                         mean_F1 = mean(norm_F1)))
data_FS_plots_SE.means


FS_SE_F1.plot = ggplot(data_FS_plots %>% filter(corpus == "CoRP-SE"), aes(x=lexSet,y=norm_F1,fill=lexSet)) +
  geom_boxplot() +
  # geom_point(data=data_FS_plots_SE.means, aes(y=mean_F1,lexSet))+
  # geom_line(data=data_FS_plots_SE.means, aes(y=mean_F1,x=lexSet,group=id))+
  theme_Caitlin() +
  theme(legend.position = "none") +
  xlab("lexical set") +
  ylab("F1 (Hz)") +
  scale_y_continuous(limits = c(300,1000), breaks = seq(0,1000,100))+
  FSFillScale +
  # ggtitle("\\scs{foot}-STRUT (F1) CoRP-SE") +
  # facet_wrap(~sex) +
  NULL
FS_SE_F1.plot
ggsave("figures/FS-SE-F1.svg",FS_SE_F1.plot,height=4,width=6,units="in")

# by agegroup
FS_SE_F1_sex.plot = FS_SE_F1.plot +
  facet_wrap(~sex) +
  NULL
FS_SE_F1_sex.plot
ggsave("figures/FS-SE-F1-sex.svg",FS_SE_F1.plot,height=4,width=6,units="in")


FS_SE_F2.plot = ggplot(data_FS_plots %>% filter(corpus == "CoRP-SE"), aes(x=lexSet,y=norm_F2,fill=lexSet)) +
  geom_boxplot() +
  geom_point(data=data_FS_plots_SE.means, aes(y=mean_F2))+
  geom_line(data=data_FS_plots_SE.means, aes(y=mean_F2,group=id))+
  xlab("lexical set") +
  ylab("F2 (Hz)") +
  theme_Caitlin() +
  theme(legend.position = "none") +
  # facet_wrap(~sex)+
  FSFillScale +
  scale_y_continuous(limits = c(800,2200), breaks = seq(0,3000,200))+
  # ggtitle("\\scs{foot}-STRUT (F2) CoRP-SE") +
  NULL
FS_SE_F2.plot
ggsave("figures/FS-SE-F2.svg",FS_SE_F2.plot,height=4,width=6,units="in")

# by style
FS_SE_F2_style.plot = FS_SE_F2.plot +
  facet_wrap(~style)+
  NULL
FS_SE_F2_style.plot
ggsave("figures/FS-SE-F2-style.svg",FS_SE_F2.plot,height=4,width=6,units="in")

# by age
FS_SE_F2_age.plot = FS_SE_F2.plot +
  facet_wrap(~ageGroup)+
  NULL
FS_SE_F2_age.plot
ggsave("figures/FS-SE-F2-style.svg",FS_SE_F2.plot,height=4,width=6,units="in")

#incl THOUGHT
ST_SE_F2.plot = ggplot(data_FST_plots %>% filter(corpus == "CoRP-SE") %>% filter(lexSet !="FOOT"), aes(x=lexSet,y=norm_F2,fill=lexSet)) +
  geom_boxplot() +
  xlab("lexical set") +
  ylab("F2 (Hz)") +
  theme_Caitlin() +
  theme(legend.position = "none") +
  FSFillScale +
  scale_y_continuous(limits = c(500,2300), breaks = seq(100,3000,200))+
  # ggtitle("\\scs{foot}-STRUT (F2) CoRP-SE") +
  # facet_wrap(~style)+
  NULL
ST_SE_F2.plot
ggsave("figures/ST-SE-F2.svg",ST_SE_F2.plot,height=4,width=6,units="in")

FS_SE.plot <- FS_SE_F1.plot + FS_SE_F2.plot + theme_Caitlin() + theme(plot.background  = element_rect(fill = "transparent", colour = NA)) +theme(legend.position = "none")
ggsave("figures/FS-SE.svg",FS_SE.plot, height=4,width=6,units="in")


data_FS_plots_SE.means = as_tibble(ddply(data_FS_plots%>% filter(corpus == "CoRP-SE"),.(lexSet),summarise,
                                         mean_F2 = mean(norm_F2),
                                         mean_F1 = mean(norm_F1)))

data_FS_plots_SE.means

FS_SE.vplot <- ggplot(data_FS %>% filter(corpus=="CoRP-SE"), aes(x=norm_F2, y = norm_F1, color = lexSet, label = lexSet)) +
  geom_text(aes(label=word), size=1.5, alpha=0.75) +
  stat_ellipse(level = 0.67, geom = "polygon", alpha = 0.5, aes(fill = lexSet)) +
  geom_label(data = data_FS_plots_SE.means, aes(x = mean_F2, y = mean_F1), size = 1.5,color="black") + 
  theme_Caitlin() +
  theme(legend.position = "none") +
  scale_x_reverse(position = "top", breaks=seq(0, 2300, 100), limits=c(2200,600)) + 
  scale_y_reverse(position = "right",breaks=seq(300, 1000, 100), limits=c(950, 350)) +
  FSColScale +
  FSFillScale +
  xlab("F2 (Hz)")+
  ylab("F1 (Hz)") +
  NULL
FS_SE.vplot

ggsave("figures/FS-SE-vplot.png", FS_SE.vplot,height = 4, width = 6,units = "in")

# NE speakers ------------------------
data_FS_plots_NE.means = as_tibble(ddply(data_FS_plots%>% filter(corpus == "CoRP-NE"),.(id,sex,lexSet,ageGroup),summarise,
                                         mean_F2 = mean(norm_F2),
                                         mean_F1 = mean(norm_F1)))
data_FS_plots_NE.means

FS_NE_F1.plot <- ggplot(data_FS_plots %>% filter(corpus == "CoRP-NE"), aes(x=lexSet,y=norm_F1,fill=lexSet,colour=ageGroup)) +
  geom_boxplot() +
  # geom_point(data=data_FS_plots_NE.means, aes(y=mean_F1,lexSet,colour=ageGroup))+
  # geom_line(data=data_FS_plots_NE.means, aes(y=mean_F1,x=lexSet,group=id,colour=ageGroup))+
  xlab("lexical set") +
  ylab("F1 (Hz)") +
  theme_Caitlin() +
  theme(legend.position = "right") +
  # facet_grid(rows=vars(sex),cols=vars(ageGroup)) +
  facet_wrap(~sex) +
  scale_color_manual(values=c("black","grey90"))+
  FSFillScale +
  scale_y_continuous(limits = c(200,1000), breaks = seq(0,1000,100))+
  # ggtitle("\\scs{foot}-\\scs{strutt} (F1) CoRP-NE") +
  ylab("F1 (Hz)") +
  xlab("lexical set")+
  NULL
FS_NE_F1.plot
ggsave("figures/FS-NE-F1.svg", FS_NE_F1.plot, width=12, height = 8, units = "in")

FS_F1.plot = FS_SE_F1.plot + FS_NE_F1.plot
ggsave("figures/FS-F1.svg", FS_F1.plot, width=12, height = 8, units = "in")


FS_NE_F2.plot = ggplot(data_FS_plots %>% filter(corpus == "CoRP-NE"), aes(x=lexSet,y=norm_F2,fill=lexSet)) +
  geom_boxplot() +
  geom_point(data=data_FS_plots_NE.means, aes(y=mean_F2,lexSet,colour=NULL))+
  geom_line(data=data_FS_plots_NE.means, aes(y=mean_F2,x=lexSet,group=id,colour=NULL))+
  xlab("lexical set") +
  ylab("F2 (Hz)") +
  theme_Caitlin() +
  theme(legend.position = "none") +
  facet_wrap(~sex) +
  # facet_wrap(~id) +
  scale_color_manual(values=c("black","grey90"))+
  # ggtitle("\\scs{foot}-STRUT (F2) CoRP-NE") +
  FSFillScale +
  scale_y_continuous(limits = c(600,2200), breaks = seq(0,3000,200))+
  NULL
FS_NE_F2.plot
ggsave("figures/FS-NE-F2.svg",FS_NE_F2.plot,height=4,width=6,units="in")


data_FS_plots_NE.means = as_tibble(ddply(data_FS_plots%>% filter(corpus == "CoRP-NE"),.(lexSet),summarise,
                                         mean_F2 = mean(norm_F2),
                                         mean_F1 = mean(norm_F1)))


data_FS_plots_NE.means

FS_NE.vplot = ggplot(data_FS_plots %>% filter(corpus=="CoRP-NE"), aes(x=norm_F2, y = norm_F1, color = lexSet, label = lexSet)) +
  # geom_text(aes(label=word), size=2.5, alpha=1) +
  stat_ellipse(level = 0.67, geom = "polygon", alpha = 0.2, aes(fill = lexSet)) +
  # geom_label(data = data_FS_plots_NE.means, aes(x = mean_F2, y = mean_F1), size = 3,alpha=1.5) +
  # stat_density2d() 
  theme_Caitlin() +
  theme(legend.position = "bottom") +
  scale_x_reverse(position = "top", breaks=seq(800, 2200, 200), limits=c(2250,750)) + 
  scale_y_reverse(position = "right",breaks=seq(300, 1000, 100), limits=c(900, 300)) +
  ylab("F1 (Hz)") +
  xlab("F2 (Hz)") +
  FSColScale +
  FSFillScale +
  # facet_wrap(~SecondEd) +
  # facet_grid(rows=vars(ageGroup),cols=vars(sex)) +
  # ggtitle("FS-CorP-NE")+
  NULL
FS_NE.vplot
ggsave("figures/FS-NE-vplot.png",FS_NE.vplot, height=4,width=6,units="in")

data_FS_plots_NE.means = as_tibble(ddply(data_FS_plots%>% filter(corpus == "CoRP-NE"),.(lexSet),summarise,
                                         mean_F2 = mean(norm_F2),
                                         mean_F1 = mean(norm_F1)))

data_FS_plots_NE.means

FS_NE.vplot <- ggplot(data_FS %>% filter(corpus=="CoRP-NE"), aes(x=norm_F2, y = norm_F1, color = lexSet, label = lexSet)) +
  geom_text(aes(label=word), size=1.5, alpha=0.75) +
  stat_ellipse(level = 0.67, geom = "polygon", alpha = 0.5, aes(fill = lexSet)) +
  geom_label(data = data_FS_plots_NE.means, aes(x = mean_F2, y = mean_F1), size = 1.5,color="black") + 
  theme_Caitlin() +
  theme(legend.position = "none") +
  scale_x_reverse(position = "top", breaks=seq(0, 2300, 100), limits=c(2200,600)) + 
  scale_y_reverse(position = "right",breaks=seq(300, 1000, 100), limits=c(950, 350)) +
  FSColScale +
  FSFillScale +
  xlab("F2 (Hz)")+
  ylab("F1 (Hz)") +
  NULL
FS_NE.vplot




Ss_NE_female.vplot <-  ggplot(data_FST_plots %>% filter(corpus=="CoRP-NE") %>% filter(lexSet %in% c("schwa","STRUT","FOOT")), aes(x=norm_F2, y = norm_F1, color = lexSet, label = lexSet)) +
  # geom_text(aes(label=word), size=2.5, alpha=1) +
  stat_ellipse(level = 0.67, geom = "polygon", alpha = 0.2, aes(fill = lexSet)) +
  # geom_label(data = data_FS_plots_NE.means, aes(x = mean_F2, y = mean_F1), size = 3,alpha=1.5) +
  # stat_density2d() 
  theme_Caitlin() +
  theme(legend.position = "bottom") +
  scale_x_reverse(position = "top", breaks=seq(800, 2200, 200), limits=c(2250,750)) + 
  scale_y_reverse(position = "right",breaks=seq(300, 1000, 100), limits=c(900, 300)) +
  ylab("F1 (Hz)") +
  xlab("F2 (Hz)") +
  FSColScale +
  FSFillScale +
  facet_wrap(~sex) +
  # facet_grid(rows=vars(ageGroup),cols=vars(sex)) +
  # ggtitle("FS-CorP-NE")+
  NULL

# DECTE ####
data_FS_plots_DE.means = as_tibble(ddply(data_FS_plots%>% filter(corpus == "DECTE-NE"),.(id,sex,lexSet),summarise,
                                         mean_F2 = mean(norm_F2),
                                         mean_F1 = mean(norm_F1)))
data_FS_plots_DE.means


FS_DE_F1.plot <- ggplot(data_FS_plots %>% filter(corpus == "DECTE-NE"), aes(x=lexSet,y=norm_F1,fill=lexSet)) +
  geom_boxplot() +
  geom_point(data=data_FS_plots_DE.means, aes(y=mean_F1,lexSet,colour=NULL))+
  geom_line(data=data_FS_plots_DE.means, aes(y=mean_F1,x=lexSet,group=id,colour=NULL))+
  xlab("lexical set") +
  ylab("F1 (Hz)") +
  theme_Caitlin() +
  theme(legend.position = "none") +
  # facet_wrap(~id)+
  # facet_grid(rows=vars(sex),cols=vars(id)) +
  FSFillScale +
  scale_y_continuous(limits = c(200,1000), breaks = seq(0,1000,100))+
  # ggtitle("\\scs{foot}-STRUT (F1) DECTE") +
  ylab("F1 (Hz)") +
  xlab("lexical set") +
NULL
FS_DE_F1.plot
ggsave("figures/FS-DE-F1.svg", FS_DE_F1.plot, width=12, height = 8, units = "in")

FS_DE_F1_id.plot <- FS_DE_F1.plot +
  facet_wrap(~id) +
  NULL
FS_DE_F1_id.plot
ggsave("figures/FS-DE-F1-id.svg", FS_DE_F1_id.plot, width=12, height = 8, units = "in")

FSG_DE_F1_age.plot = ggplot(data_FSG_plots %>% filter(corpus == "DECTE-NE"), aes(x=lexSet,y=norm_F1,fill=lexSet)) +
  xlab("lexical set") +
  ylab("F1 (Hz)") +
  # ggtitle("\\scs{foot}, \\scs{strut}, and \\scs{goose} (F2) DECTE, by age group") +
  geom_boxplot() +
  theme_Caitlin() +
  theme(legend.position = "none") +
  FSFillScale +
  facet_wrap(~ageGroup) +
  scale_y_continuous(limits = c(200,1000), breaks = seq(0,1000,250))+
  NULL
FSG_DE_F1_age.plot
ggsave("figures/FSG-DE-F1.svg",FSG_DE_F1_age.plot,height=4,width=6,units="in")

FS_DE_F2.plot = ggplot(data_FS_plots %>% filter(corpus == "DECTE-NE"), aes(x=lexSet,y=norm_F2,fill=lexSet)) +
  geom_boxplot() +
  geom_point(data=data_FS_plots_NE.means, aes(y=mean_F2,lexSet,colour=NULL))+
  geom_line(data=data_FS_plots_NE.means, aes(y=mean_F2,x=lexSet,group=id,colour=NULL))+
  xlab("lexical set") +
  ylab("F2 (Hz)") +
  # ggtitle("\\scs{foot}-\\scs{strut} (F2) DECTE") +
  theme_Caitlin() +
  theme(legend.position = "none") +
  FSFillScale +
  # facet_wrap(~id) +
  scale_y_continuous(limits = c(600,2200), breaks = seq(0,3000,200))+
  NULL
FS_DE_F2.plot
ggsave("figures/FS-DE-F2.svg",FS_DE_F2.plot,height=4,width=6,units="in")

FS_DE_F2_age.plot <- FS_DE_F2.plot +
  facet_wrap(~ageGroup)
FS_DE_F2_age.plot
ggsave("figures/FS-DE-F2-age.svg",FS_DE_F2_age.plot,height=4,width=6,units="in")

data_FSG_plots_DE.means = as_tibble(ddply(data_FSG_plots%>% filter(corpus == "DECTE-NE"),.(id,sex,lexSet),summarise,
                                         mean_F2 = mean(norm_F2),
                                         mean_F1 = mean(norm_F1)))
data_FSG_plots_DE.means


FSG_DE_F2_age.plot = ggplot(data_FSG_plots %>% filter(corpus == "DECTE-NE"), aes(x=lexSet,y=norm_F2,fill=lexSet)) +
  xlab("lexical set") +
  ylab("F2 (Hz)") +
  # ggtitle("\\scs{foot}, \\scs{strut}, and \\scs{goose} (F2) DECTE, by age group") +
  geom_boxplot() +
  geom_point(data=data_FS_plots_DE.means, aes(y=mean_F2,lexSet,colour=NULL))+
  geom_line(data=data_FS_plots_DE.means, aes(y=mean_F2,x=lexSet,group=id,colour=NULL))+
  theme_Caitlin() +
  theme(legend.position = "none") +
  FSFillScale +
  facet_wrap(~ageGroup) +
  scale_y_continuous(limits = c(600,2600), breaks = seq(0,3000,200))+
  NULL
FSG_DE_F2_age.plot
ggsave("figures/FSG-DE-F2-age.svg",FSG_DE_F2_age.plot,height=4,width=6,units="in")

data_FS_plots_DE.means = as_tibble(ddply(data_FS_plots%>% filter(corpus == "DECTE-NE"),.(lexSet),summarise,
                                         mean_F2 = mean(norm_F2),
                                         mean_F1 = mean(norm_F1)))

data_FS_plots_DE.means

FS_DE.vplot <- ggplot(data_FS %>% filter(corpus=="DECTE-NE"), aes(x=norm_F2, y = norm_F1, color = lexSet, label = lexSet)) +
  geom_text(aes(label=word), size=1.5, alpha=0.75) +
  stat_ellipse(level = 0.67, geom = "polygon", alpha = 0.5, aes(fill = lexSet)) +
  geom_label(data = data_FS_plots_DE.means, aes(x = mean_F2, y = mean_F1), size = 1.5,color="black") +
  theme_Caitlin() +
  theme(legend.position = "none") +
  scale_x_reverse(position = "top", breaks=seq(0, 2300, 100), limits=c(2200,600)) + 
  scale_y_reverse(position = "right",breaks=seq(300, 1000, 100), limits=c(950, 350)) +
  FSColScale +
  FSFillScale +
  xlab("F2 (Hz)")+
  ylab("F1 (Hz)") +
  facet_wrap(~ageGroup) +
  # facet_grid(rows=vars(juniorEd),cols=vars(SecondEd)) +
  # ggtitle("FST-CorP-SE")+
  NULL
FS_DE.vplot

ggsave("figures/FS-DE-vplot.png", FS_DE.vplot,height = 4, width = 6,units = "in")

FSG_DE.vplot <- ggplot(data_FSG_plots %>% filter(corpus=="DECTE-NE"), aes(x=norm_F2, y = norm_F1, color = lexSet, label = lexSet)) +
  geom_text(aes(label=word), size=1.5, alpha=0.75) +
  stat_ellipse(level = 0.67, geom = "polygon", alpha = 0.5, aes(fill = lexSet)) +
  # geom_label(data = data_FS_plots_DE.means, aes(x = mean_F2, y = mean_F1), size = 1.5,color="black") +
  theme_Caitlin() +
  theme(legend.position = "none") +
  scale_x_reverse(position = "top", breaks=seq(0, 2300, 100), limits=c(2200,600)) + 
  scale_y_reverse(position = "right",breaks=seq(300, 1000, 100), limits=c(950, 350)) +
  FSColScale +
  FSFillScale +
  xlab("F2 (Hz)")+
  ylab("F1 (Hz)") +
  facet_wrap(~ageGroup) +
  # facet_grid(rows=vars(juniorEd),cols=vars(SecondEd)) +
  # ggtitle("FST-CorP-SE")+
  NULL
FSG_DE.vplot

# All ####
data_FS_plots.means = as_tibble(ddply(data_FS_plots,.(lexSet,corpus,sex),summarise,
                                         mean_F2 = mean(norm_F2),
                                         mean_F1 = mean(norm_F1)))

data_FS_plots.means


FS.vplot <- ggplot(data_FS, aes(x=norm_F2, y = norm_F1, color = lexSet, label = lexSet)) +
  # geom_text(aes(label=word), size=1.5, alpha=0.75) +
  stat_ellipse(level = 0.67, geom = "polygon", alpha = 0.5, aes(fill = lexSet)) +
  geom_label(data = data_FS_plots.means, aes(x = mean_F2, y = mean_F1), size = 1.5,color="black") +
  theme_Caitlin() +
  theme(legend.position = "none") +
  scale_x_reverse(position = "top", breaks=seq(0, 2300, 200), limits=c(2200,600)) + 
  scale_y_reverse(position = "right",breaks=seq(300, 1000, 100), limits=c(950, 350)) +
  FSColScale +
  FSFillScale +
  xlab("F2 (Hz)")+
  ylab("F1 (Hz)") +
  # facet_wrap(~ageGroup) +
  facet_grid(rows=vars(corpus),cols=vars(sex)) +
  # ggtitle("FST-CorP-SE")+
  NULL
FS.vplot


ggsave("figures/FS-vplot.png", FS.vplot,height = 4, width = 6,units = "in")

# STRUT ####
## F1 ####
strut_F1.plot <- ggplot(data_FS_plots %>% filter(lexSet == "STRUT"), aes(x=corpus, y=norm_F1,fill=corpus))+
  geom_boxplot() +
  xlab("corpus") +
  ylab("F1 (Hz)") +
  theme_Caitlin() +
  theme(legend.position = "none") +
  # facet_wrap(~id)+
  # facet_grid(rows=vars(sex),cols=vars(occClass)) +
  regionFillScale +
  # FSColScale +
  scale_y_continuous(limits = c(400,1000), breaks = seq(0,1000,100))+
  ggtitle("\\scs{strut} (F1)") +
  ylab("F1 (Hz)") +
  xlab("lexical set") +
  NULL
strut_F1.plot
ggsave("figures/STRUT_F1.svg", STRUT_F1.plot, width=6, height = 4, units = "in")

## F2 ####
strut_F2.plot <- ggplot(data_FS_plots %>% filter(lexSet == "STRUT"), aes(x=corpus, y=norm_F2,fill=corpus))+
  geom_boxplot() +
  xlab("corpus") +
  ylab("F2 (Hz)") +
  theme_Caitlin() +
  theme(legend.position = "none") +
  # facet_wrap(~id)+
  # facet_grid(rows=vars(sex),cols=vars(occClass)) +
  regionFillScale +
  # FSColScale +
  scale_y_continuous(limits = c(600,2200), breaks = seq(0,3000,200))+
  ggtitle("\\scs{strut} (F2)") +
  ylab("F2 (Hz)") +
  xlab("lexical set") +
  NULL
strut_F2.plot
ggsave("figures/STRUT_F2.svg", STRUT_F2.plot, width=6, height = 4, units = "in")

# FOOT-STRUT together ####
FSF1.plot <- ggplot(data_FS_plots, aes(x=lexSet, y=norm_F1,fill=lexSet))+
  geom_boxplot() +
  theme_Caitlin_present() +
  theme(legend.position = "none") +
  facet_wrap(~corpus)+
  FSFillScale +
  scale_y_continuous(limits = c(350,1000), breaks = seq(0,1000,100))+
  # ggtitle("\\scs{strut} (F2)") +
  ylab("F1 (Hz)") +
  xlab("lexical set") +
  ylab("F1 (Hz)") +
  NULL
FSF1.plot
ggsave("figures/FS-F1-corpus.svg", FSF1.plot,height = 8, width = 12,units = "in")

# vowel space plots ####
## FOOT-STRUT ####
data_FSG_DE.means <- as_tibble(ddply(data_FSTG %>% filter(corpus == "DECTE-NE") %>% filter(lexSet %in% c("FOOT","STRUT","GOOSE")),.(lexSet),summarise,
                  mean_F2 = mean(norm_F2),
                  mean_F1 = mean(norm_F1)))

FSG_DE.vplot <- ggplot(data_FSTG %>% filter(corpus=="CoRP-SE") %>% filter(lexSet %in% c("FOOT","STRUT","GOOSE")), aes(x=norm_F2, y = norm_F1, color = lexSet, label = lexSet)) +
  geom_text(aes(label=word), size=1.5, alpha=0.75) +
  stat_ellipse(level = 0.67, geom = "polygon", alpha = 0.2, aes(fill = lexSet)) +
  # geom_label(data = data_FSG_DE.means, aes(x = mean_F2, y = mean_F1), size = 1.5,color="black") + 
  theme_Caitlin() +
  theme(legend.position = "top") +
  scale_x_reverse(position = "top", breaks=seq(0,3000,500), limits=c(3000,500)) + 
  scale_y_reverse(position = "right",breaks=seq(0,1000,100), limits=c(1000,300)) +
  # FSColScale +
  # FSFillScale +
  facet_wrap(~ageGroup) +
  # facet_grid(rows=vars(juniorEd),cols=vars(SecondEd)) +
  NULL
FSG_DE.vplot


# STRUT ####
## F1 ####
strut_F1.plot = ggplot(data_FS_plots %>% filter(lexSet == "STRUT"), aes(x=factor(corpus, level = corpus_order),y=norm_F1,fill=corpus)) +
  geom_boxplot() +
  theme_Caitlin() +
  theme(legend.position = "none") +
  xlab("speaker group") +
  ylab("F1 (Hz)") +
  scale_y_continuous(limits = c(300,1000), breaks = seq(0,1000,100))+
  regionFillScale+
  # ggtitle("\\scs{foot}-STRUT (F1) CoRP-SE") +
  # facet_wrap(~sex) +
  NULL
strut_F1.plot
ggsave("figures/strut-F1.svg",strut_F1.plot,height=4,width=6,units="in")

# by sex
strut_F1_sex.plot = strut_F1.plot +
  facet_wrap(~sex) +
  NULL
strut_F1_sex.plot
ggsave("figures/strut-F1-sex.svg",strut_F1_sex.plot,height=4,width=6,units="in")

#by id
strut_F1_id.plot <- 
  strut_F1.plot +
  facet_wrap(~id)
strut_F1_id.plot

## F2 ####

strut_F2.plot = ggplot(data_FS_plots %>% filter(lexSet == "STRUT"), aes(x=factor(corpus, level = corpus_order),y=norm_F2,fill=corpus)) +
  geom_boxplot() +
  xlab("speaker group") +
  ylab("F2 (Hz)") +
  theme_Caitlin() +
  theme(legend.position = "none") +
  regionFillScale +
  scale_y_continuous(limits = c(700,2000), breaks = seq(0,3000,200))+
  # ggtitle("\\scs{foot}-STRUT (F2) CoRP-SE") +
  NULL
strut_F2.plot
ggsave("figures/strut-F2.svg",strut_F2.plot,height=4,width=6,units="in")


strut_F2_flip.plot = ggplot(data_FS_plots %>% filter(lexSet == "STRUT"), aes(x=sex,y=norm_F2,fill=corpus)) +
  geom_boxplot() +
  xlab("speaker group") +
  ylab("F2 (Hz)") +
  theme_Caitlin() +
  theme(legend.position = "none") +
  regionFillScale +
  scale_y_continuous(limits = c(700,2000), breaks = seq(0,3000,200))+
  # ggtitle("\\scs{foot}-STRUT (F2) CoRP-SE") +
  facet_wrap(~corpus) +
  NULL
strut_F2_flip.plot
ggsave("figures/strut-F2.svg",strut_F2.plot,height=4,width=6,units="in")


strut_F2_sex.plot <- strut_F2.plot+
  facet_wrap(~sex)
strut_F2_sex.plot
ggsave("figures/strut-F2-sex.svg",strut_F2_sex.plot,height=4,width=6,units="in")

ST_SE_F2.plot <- ggplot(data_FST_plots %>% filter(lexSet %in% c("STRUT","THOUGHT")) %>% filter(corpus == "CoRP-SE"), aes(x=lexSet,y=norm_F2,fill=lexSet)) +
  geom_boxplot() +
  xlab("speaker group") +
  ylab("F2 (Hz)") +
  theme_Caitlin() +
  theme(legend.position = "none") +
  # regionFillScale +
  FSFillScale+
  # scale_y_continuous(limits = c(700,2000), breaks = seq(0,3000,200))+
    facet_wrap(~sex) +
  # ggtitle("\\scs{foot}-STRUT (F2) CoRP-SE") +
  NULL
ST_SE_F2.plot

##vowel space ####

data_strut_plots.means = as_tibble(ddply(data_FS_plots%>% filter(lexSet == "STRUT"),.(corpus),summarise,
                                         mean_F2 = mean(norm_F2),
                                         mean_F1 = mean(norm_F1)))

data_strut_plots.means

strut.vplot = ggplot(data_FS_plots %>% filter(lexSet=="STRUT"), aes(x=norm_F2, y = norm_F1, color = corpus, label = corpus)) +
  # geom_text(aes(label=word), size=1.5, alpha=0.75) +
  stat_ellipse(level = 0.67, geom = "polygon", alpha = 0.2, aes(fill = corpus)) +
  geom_label(data = data_strut_plots.means, aes(x = mean_F2, y = mean_F1), size = 1.5) + 
  theme_Caitlin() +
  theme(legend.position = "none") +
  scale_x_reverse(position = "top", breaks=seq(0, 2300, 100), limits=c(2200,600)) + 
  scale_y_reverse(position = "right",breaks=seq(300, 1000, 100), limits=c(950, 350)) +
  regionColScale +
  regionFillScale +
  # facet_wrap(~SecondEd) +
  # facet_grid(rows=vars(juniorEd),cols=vars(SecondEd)) +
  NULL
strut.vplot
ggsave("figures/strut-corpus.png",strut.vplot,height=4,width=6,units="in")

# Teaching Pod ####
FS_all <- ggplot(data_FS_plots,aes(x=lexSet,y=norm_F1, fill=lexSet))+ 
  geom_boxplot() +
  theme(legend.position="none") +
  facet_wrap(~factor(corpus, level = corpus_order)) +
  xlab("Lexical Set") +
  ylab("F1 (Hz)") +
  scale_y_continuous(limits = c(300,1000), breaks = seq(0,1000,100))+
  FSFillScale +
  ggtitle("F1 of the FOOT-STRUT split in state (DECTE) and privately (CoRP) educated speakers in the North East and South East.") +
  NULL
FS_all  
ggsave("figures/FS_all_pod.png",FS_all,height=8,width=12,units="in")

data_FS_plots.means = as_tibble(ddply(data_FS_plots %>% group_by(corpus,lexSet),.(lexSet,corpus),summarise,
                                         mean_F2 = mean(norm_F2),
                                         mean_F1 = mean(norm_F1)))

data_FS_plots.means

FS.vplot_pod1 <- ggplot(data_FS_plots, aes(x=norm_F2, y = norm_F1, color = lexSet, label = lexSet)) +
  # geom_text(aes(label=word), size=1.5, alpha=0.75) +
  stat_ellipse(level = 0.67, geom = "polygon", alpha = 0.5, aes(fill = lexSet)) +
  geom_label(data = data_FS_plots.means, aes(x = mean_F2, y = mean_F1), size = 2.5) +
  facet_wrap(~factor(corpus, level = corpus_order)) +
  theme_Caitlin() +
  theme(legend.position = "none") +
  scale_x_reverse(position = "top", breaks=seq(0, 2300, 500), limits=c(2200,600)) + 
  scale_y_reverse(position = "right",breaks=seq(300, 1000, 100), limits=c(950, 350)) +
  FSColScale +
  FSFillScale +
  xlab("F2 (Hz)")+
  ylab("F1 (Hz)") +
  ggtitle("The FOOT-STRUT split in state (DECTE) and privately (CoRP) educated speakers in the North East and South East.") +
  NULL
FS.vplot_pod1

ggsave("figures/FS-vplot_pod1.png", FS.vplot_pod1,height = 8, width = 12,units = "in")

FS.vplot_pod2 <- ggplot(data_FS_plots, aes(x=norm_F2, y = norm_F1, color = lexSet, label = lexSet)) +
  geom_text(aes(label=word), size=1.5, alpha=0.75) +
  stat_ellipse(level = 0.67, geom = "polygon", alpha = 0.3, aes(fill = lexSet)) +
  geom_label(data = data_FS_plots.means, aes(x = mean_F2, y = mean_F1), size = 2.5) +
  facet_wrap(~factor(corpus, level = corpus_order)) +
  theme_Caitlin() +
  theme(legend.position = "none") +
  scale_x_reverse(position = "top", breaks=seq(0, 2300, 500), limits=c(2200,600)) + 
  scale_y_reverse(position = "right",breaks=seq(300, 1000, 100), limits=c(950, 350)) +
  FSColScale +
  FSFillScale +
  xlab("F2 (Hz)")+
  ylab("F1 (Hz)") +
  ggtitle("The FOOT-STRUT split in state (DECTE) and privately (CoRP) educated speakers in the North East and South East.") +
  NULL
FS.vplot_pod2

ggsave("figures/FS-vplot_pod2.png", FS.vplot_pod2,height = 8, width = 12,units = "in")

FS.vplot_pod3 <- ggplot(data_FS_plots, aes(x=norm_F2, y = norm_F1, color = lexSet, label = lexSet)) +
  geom_point(size=1, alpha=0.5) +
  stat_ellipse(level = 0.67, geom = "polygon", alpha = 0.3, aes(fill = lexSet)) +
  geom_label(data = data_FS_plots.means, aes(x = mean_F2, y = mean_F1), size = 2.5) +
  facet_wrap(~factor(corpus, level = corpus_order)) +
  theme_Caitlin() +
  theme(legend.position = "none") +
  scale_x_reverse(position = "top", breaks=seq(0, 2300, 500), limits=c(2200,600)) + 
  scale_y_reverse(position = "right",breaks=seq(300, 1000, 100), limits=c(950, 350)) +
  FSColScale +
  FSFillScale +
  xlab("F2 (Hz)")+
  ylab("F1 (Hz)") +
  ggtitle("The FOOT-STRUT split in state (DECTE) and privately (CoRP) educated speakers in the North East and South East.") +
  NULL
FS.vplot_pod3

ggsave("figures/FS-vplot_pod3.png", FS.vplot_pod3,height = 8, width = 12,units = "in")
