source("scripts/FS_cleaning.R")

options(digits = 6)

packages = c("lme4","stats","cAIC4","stargazer","tidymodels","broom.mixed","xtable","MASS","dplyr")



package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

data_FS_stats <- data_FS %>% 
  dplyr::select(rowNumber_all, rowid, word, style, time, norm_F1, norm_F2, dur, rowNumber_all_fac, lastName, sex, YOB, ageGroup, corpus, id, juniorEd, SecondEd, sixthFEd, privateTotal, occupation, occClass, edLevel, fatherRegion, motherRegion, fatherEd, motherEd, motherEdLevel, fatherEdLevel, parentEdLevelHigh, motherOccupation, fatherOccupation, parentOccClass, lexicalSet_broad, lexicalSet_narrow, folMan, preSeg_small,lexSet, FreqCount,BNC_freq, LogFreq.Zipf., LogFreqBNC.Zipf.) %>% 
  droplevels() %>% 
  mutate(time_z = scale(time)) %>%  
  mutate(privateTotal_z=scale(privateTotal)) %>% 
  mutate(corpus=factor(corpus, levels=rev(levels(corpus)))) %>% 
  mutate(F1_z = scale(norm_F1)) %>% 
  mutate(F2_z = scale(norm_F2)) %>% 
  mutate(freq.zipf_z = scale(LogFreq.Zipf.)) %>% #Turton&Baranowski2021
  mutate(motherRegion_FS = recode(motherRegion, "Birmingham"="north", "Lancashire"="north", "Lincoln"="north","London"="south","North-East"="north", "Nottingham"="north", "Sheffield"="north","South-East"="south")) %>% 
  mutate(fatherRegion_FS = recode(fatherRegion, "IsleofMan"="north","North-East"="north","South-East"="south")) %>% 
  mutate(lexSetSum = factor(lexSet)) %>% 
  mutate(sexSum = factor(sex)) %>% 
  mutate(ageGroupSum = factor(ageGroup)) %>% 
  mutate(corpusSum = factor(corpus)) %>% 
  mutate(motherRegionSum = factor(motherRegion_FS)) %>% 
  mutate(fatherRegionSum = factor(fatherRegion_FS)) %>% 
  mutate(styleSum = factor (style)) %>% 
  mutate(folManSum = factor(folMan)) %>% 
  mutate(preSeg_smallSum = factor(preSeg_small)) %>% 
  mutate(occClassSum = factor(occClass)) %>% 
  filter(style %in% c("interview","minimalpair","wordlist")) %>%
  droplevels()

# strut/thought
data_ST_stats <- data_ST %>% 
  dplyr::select(rowNumber_all, rowid, word, style, time, norm_F1, norm_F2, dur, rowNumber_all_fac, lastName, sex, YOB, ageGroup, corpus, id, juniorEd, SecondEd, sixthFEd, privateTotal, occupation, occClass, edLevel, fatherRegion, motherRegion, fatherEd, motherEd, motherEdLevel, fatherEdLevel, parentEdLevelHigh, motherOccupation, fatherOccupation, parentOccClass, lexicalSet_broad, lexicalSet_narrow, folMan, preSeg_small,lexSet, FreqCount,BNC_freq, LogFreq.Zipf., LogFreqBNC.Zipf.) %>% 
  droplevels() %>% 
  mutate(time_z = scale(time)) %>%  
  mutate(privateTotal_z=scale(privateTotal)) %>% 
  mutate(corpus=factor(corpus, levels=rev(levels(corpus)))) %>% 
  mutate(F1_z = scale(norm_F1)) %>% 
  mutate(F2_z = scale(norm_F2)) %>% 
  mutate(freq.zipf_z = scale(LogFreq.Zipf.)) %>% #Turton&Baranowski2021
  mutate(motherRegion_FS = recode(motherRegion, "Birmingham"="north", "Lancashire"="north", "Lincoln"="north","London"="south","North-East"="north", "Nottingham"="north", "Sheffield"="north","South-East"="south")) %>% 
  mutate(fatherRegion_FS = recode(fatherRegion, "IsleofMan"="north","North-East"="north","South-East"="south")) %>% 
  mutate(lexSetSum = factor(lexSet)) %>% 
  mutate(sexSum = factor(sex)) %>% 
  mutate(ageGroupSum = factor(ageGroup)) %>% 
  mutate(corpusSum = factor(corpus)) %>% 
  mutate(motherRegionSum = factor(motherRegion_FS)) %>% 
  mutate(fatherRegionSum = factor(fatherRegion_FS)) %>% 
  mutate(styleSum = factor (style)) %>% 
  mutate(folManSum = factor(folMan)) %>% 
  mutate(occClassSum = factor(occClass)) %>% 
  mutate(preSeg_smallSum = factor(preSeg_small)) %>% 
  filter(style %in% c("interview","minimalpair","wordlist")) %>%
  droplevels()

# FOOT-STRUT ####
## South East ####
data_FS_statsSE <- data_FS_stats %>%
  filter(corpus == "CoRP-SE")
data_strut_statsSE <- data_FS_stats %>%
  filter(corpus == "CoRP-SE") %>% 
  filter(lexSet == "STRUT")
contrasts(data_FS_statsSE$lexSetSum) <- contr.sum(2)
contrasts(data_FS_statsSE$sexSum) <- contr.sum(2)
contrasts(data_FS_statsSE$ageGroupSum) <- contr.sum(2)
contrasts(data_FS_statsSE$styleSum) <- contr.sum(3)
contrasts(data_FS_statsSE$folManSum) <- contr.sum(4)
contrasts(data_FS_statsSE$preSeg_smallSum) <- contr.sum(5)
#
contrasts(data_strut_statsSE$lexSetSum) <- contr.sum(2)
contrasts(data_strut_statsSE$corpusSum) <- contr.sum(2)
contrasts(data_strut_statsSE$sexSum) <- contr.sum(2)
contrasts(data_strut_statsSE$ageGroupSum) <- contr.sum(2)
contrasts(data_strut_statsSE$styleSum) <- contr.sum(3)
contrasts(data_strut_statsSE$folManSum) <- contr.sum(4)
contrasts(data_strut_statsSE$preSeg_smallSum) <- contr.sum(5)
#STRUT/THOUGHT/schwa
data_ST_statsSE <- data_ST_stats %>%
  filter(corpus == "CoRP-SE")
contrasts(data_ST_statsSE$lexSetSum) <- contr.sum(3)
contrasts(data_ST_statsSE$sexSum) <- contr.sum(2)
contrasts(data_ST_statsSE$ageGroupSum) <- contr.sum(2)
contrasts(data_ST_statsSE$styleSum) <- contr.sum(3)
contrasts(data_ST_statsSE$folManSum) <- contr.sum(4)
contrasts(data_ST_statsSE$preSeg_smallSum) <- contr.sum(5)


### F1 ----
#fullest model
FS_SE_F1.full.mod <- lmer(norm_F1 ~ lexSet +
                            sexSum +
                            ageGroupSum +
                            folManSum +
                            preSeg_smallSum +
                            freq.zipf_z +
                            styleSum +
                            time_z+
                            # (1 + style | id) +
                            # (1+time_z|id) +
                            (1|id) +
                            (1|word)
                          , data = data_FS_statsSE)

tidy(FS_SE_F1.full.mod) %>% dplyr::select(term,estimate,statistic)
#stepwise
cAIC4::cAIC(FS_SE_F1.full.mod)
FS_SE_F1.step <- cAIC4::stepcAIC(FS_SE_F1.full.mod, calcNonOptimMod=TRUE,numberOfSavedModels = 2)
FS_SE_F1.step
FS_SE_F1.step.og <- lmer(norm_F1 ~ lexSet +
                           sexSum +
                           ageGroupSum +
                           folManSum +
                           preSeg_smallSum +
                           freq.zipf_z +
                           styleSum +
                           time_z+
                           # (1 + style | id) +
                           # (1+time_z|id) +
                           (1|id) +
                           (1|word)
                         , data = data_FS_statsSE)
FS_SE_F1.step.mod <- FS_SE_F1.step.og %>%
  tidy()
print(FS_SE_F1.step.mod, n=nrow(FS_SE_F1.step.mod))


print(
  xtable(
  (FS_SE_F1.step.mod %>%
    filter(effect=="fixed") %>%
    dplyr::select(term, estimate, statistic) %>%
    dplyr::rename(tvalue = statistic) %>% 
    dplyr::rename(fixedeffect = term)),
  caption = "Linear Mixed Effects Model of F1 of \\textsc{foot} and \\textsc{strut} in the South East \\label{tbl:FSF1SE}"),
  label = "tbl:FSF1SE",
  include.rownames=FALSE,
  file="models/FS-SE-F1-mod.tex"
)



### F2----
FS_SE_F2.full.mod <- lmer(norm_F2  ~
                            lexSet +
                            sexSum +
                            ageGroupSum +
                            folManSum +
                            preSeg_smallSum +
                            freq.zipf_z +
                            styleSum +
                            time_z+
                            # (1 + style | id) +
                            # (1+time_z|id) +
                            (1|id) +
                            (1|word)
                          , data_FS_statsSE)
FS_SE_F2.full.mod %>% tidy()
cAIC4::cAIC(FS_SE_F2.full.mod)
FS_SE_F2.step <- cAIC4::stepcAIC(FS_SE_F2.full.mod,calcNonOptimMod=TRUE,numberOfSavedModels = 1)
FS_SE_F2.step
FS_SE_F2.step.og <- lmer(norm_F2  ~
                            lexSet +
                            sexSum +
                            ageGroupSum +
                            folManSum +
                            preSeg_smallSum +
                            freq.zipf_z +
                            styleSum +
                            time_z+
                            # (1 + style | id) +
                            # (1+time_z|id) +
                            (1|id) +
                            (1|word)
                          , data_FS_statsSE)
FS_SE_F2.step.mod <- FS_SE_F2.step.og %>% 
  tidy()
print(FS_SE_F2.step.mod, n = nrow(FS_SE_F2.step.mod))


print(
  xtable(
    (FS_SE_F2.step.mod %>%
       filter(effect=="fixed") %>%
       dplyr::select(term, estimate, statistic) %>%
       dplyr::rename(tvalue = statistic) %>% 
       dplyr::rename(fixedeffect = term)),
    caption = "Linear Mixed Effects Model of F2 of \\textsc{foot} and \\textsc{strut} in the South East \\label{tbl:FSF2SE}"),
  label = "tbl:FSF2SE",
  include.rownames=FALSE,
  file="models/FS-SE-F2-mod.tex"
)

ST_SE_F2.full.mod <- lmer(norm_F2  ~
                            lexSet +
                            sexSum +
                            ageGroupSum +
                            folManSum +
                            preSeg_smallSum +
                            freq.zipf_z +
                            styleSum +
                            time_z+
                            # (1 + style | id) +
                            # (1+time_z|id) +
                            (1|id) +
                            (1|word)
                          , data_ST_statsSE)
ST_SE_F2.full.mod %>% tidy()
cAIC4::cAIC(ST_SE_F2.full.mod)
ST_SE_F2.step <- cAIC4::stepcAIC(ST_SE_F2.full.mod,calcNonOptimMod=TRUE,numberOfSavedModels = 1)
ST_SE_F2.step
ST_SE_F2.step.og <- lmer(norm_F2  ~
                            lexSet +
                            sexSum +
                            ageGroupSum +
                            folManSum +
                            preSeg_smallSum +
                            freq.zipf_z +
                            styleSum +
                            # time_z+
                            # (1 + style | id) +
                            # (1+time_z|id) +
                            (1|id) +
                            (1|word)
                          , data_ST_statsSE)
ST_SE_F2.step.mod <- ST_SE_F2.step.og %>% 
  tidy()
print(ST_SE_F2.step.mod, n = nrow(ST_SE_F2.step.mod))


print(
  xtable(
    (ST_SE_F2.step.mod %>%
       filter(effect=="fixed") %>%
       dplyr::select(term, estimate, statistic) %>%
       dplyr::rename(tvalue = statistic) %>% 
       dplyr::rename(fixedeffect = term)),
    caption = "Linear Mixed Effects Model of F2 of \\textsc{strut}, \\textsc{thought}, and schwa in the South East \\label{tbl:FSF2SE}"),
  label = "tbl:FSF2SE",
  include.rownames=FALSE,
  file="models/ST-SE-F2-mod.tex"
)




## North East ------------
data_FS_statsNE <- data_FS_stats %>% filter(corpus == "CoRP-NE")
data_strut_statsNE <- data_FS_stats %>%
  filter(corpus == "CoRP-NE") %>% 
  filter(lexSet == "STRUT")

contrasts(data_FS_statsNE$lexSetSum) <- contr.sum(2)
contrasts(data_FS_statsNE$sexSum) <- contr.sum(2)
contrasts(data_FS_statsNE$ageGroupSum) <- contr.sum(2)
contrasts(data_FS_statsNE$styleSum) <- contr.sum(3)
contrasts(data_FS_statsNE$folManSum) <- contr.sum(4)
contrasts(data_FS_statsNE$preSeg_smallSum) <- contr.sum(5)
contrasts(data_FS_statsNE$styleSum) <- contr.sum(3)


### F1 ----

FS_NE_F1.full.mod <- lmer(norm_F1 ~ lexSet*
                            sex*
                            ageGroup +
                            folManSum +
                            preSeg_smallSum +
                            freq.zipf_z +
                            styleSum +
                            # time_z+
                            # (1 + styleSum | id) +
                            (1 + time_z|id) +
                            # (1|id) +
                            (1|word), data_FS_statsNE)
FS_NE_F1.full.mod %>% tidy()
cAIC4::cAIC(FS_NE_F1.full.mod)
FS_NE_F1.step <- cAIC4::stepcAIC(FS_NE_F1.full.mod,calcNonOptimMod=TRUE,numberOfSavedModels = 2)
FS_NE_F1.step 
FS_NE_F1.step.og <- lmer(norm_F1 ~ lexSet*
                           sex*
                           ageGroup +
                           folManSum +
                           preSeg_smallSum +
                           freq.zipf_z +
                           styleSum +
                           # time_z+
                           # (1 + styleSum | id) +
                           (1 + time_z|id) +
                           # (1|id) +
                           (1|word), data_FS_statsNE)
FS_NE_F1.step.mod <- FS_NE_F1.step.og %>%
  tidy()
print(FS_NE_F1.step.mod, n=nrow(FS_NE_F1.step.mod))


print(
  xtable(
    (FS_NE_F1.step.mod %>%
       filter(effect=="fixed") %>%
       dplyr::select(term, estimate, statistic) %>%
       dplyr::rename(tvalue = statistic) %>% 
       dplyr::rename(fixedeffect = term)),
    caption = "Linear Mixed Effects Model of F1 of \\textsc{foot} and \\textsc{strut} in the North East \\label{tbl:FSF1NE}"),
  label = "tbl:FSF1NE",
  include.rownames=FALSE,
  file="models/FS-NE-F1-mod.tex"
)



### F2 ----
FS_NE_F2.full.mod <- lmer(norm_F2 ~ lexSet*
                            sex*
                            ageGroup +
                            folManSum +
                            preSeg_smallSum +
                            freq.zipf_z +
                            styleSum +
                            time_z+
                            # (1+time_z|id) +
                            # (1+styleSum|id) +
                            (1|id) +
                            (1|word), data_FS_statsNE)
cAIC4::cAIC(FS_NE_F2.full.mod)
FS_NE_F2.full.mod %>% tidy()


FS_NE_F2.step <- cAIC4::stepcAIC(FS_NE_F2.full.mod,calcNonOptimMod=TRUE,numberOfSavedModels = 2)
FS_NE_F2.step
FS_NE_F2.step.og <- lmer(norm_F2 ~ lexSet*
                           sex*
                           ageGroup +
                           folManSum +
                           preSeg_smallSum +
                           freq.zipf_z +
                           styleSum +
                           time_z+
                           # (1+time_z|id) +
                           # (1+styleSum|id) +
                           (1|id) +
                           (1|word), data_FS_statsNE)
FS_NE_F2.step.mod <- FS_NE_F2.step.og %>%
  tidy()
print(FS_NE_F2.step.mod, n=nrow(FS_NE_F2.step.mod))


print(
  xtable(
    (FS_NE_F2.step.mod %>%
       filter(effect=="fixed") %>%
       dplyr::select(term, estimate, statistic) %>%
       dplyr::rename(tvalue = statistic) %>% 
       dplyr::rename(fixedeffect = term)),
    caption = "Linear Mixed Effects Model of F2 of \\textsc{foot} and \\textsc{strut} in the North East \\label{tbl:FSF2NE}"),
  label = "tbl:FSF2NE",
  include.rownames=FALSE,
  file="models/FS-NE-F2-mod.tex"
)

    ## DECTE -----------------
data_FS_statsDE <- data_FS_stats %>% filter(corpus == "DECTE-NE") %>% 
  droplevels()
data_strut_statsDE <- data_FS_stats %>%
  filter(corpus == "DECTE-NE") %>% 
  filter(lexSet == "STRUT")

contrasts(data_FS_statsDE$lexSetSum) <- contr.sum(2)
contrasts(data_FS_statsDE$sexSum) <- contr.sum(2)
contrasts(data_FS_statsDE$ageGroupSum) <- contr.sum(2)
contrasts(data_FS_statsDE$folManSum) <- contr.sum(4)
contrasts(data_FS_statsDE$preSeg_smallSum) <- contr.sum(5)
contrasts(data_FS_statsDE$occClassSum) <- contr.sum(5)
#
contrasts(data_strut_statsDE$sexSum) <- contr.sum(2)
contrasts(data_strut_statsDE$ageGroupSum) <- contr.sum(2)
contrasts(data_strut_statsDE$folManSum) <- contr.sum(4)
contrasts(data_strut_statsDE$preSeg_smallSum) <- contr.sum(5)

### F1 ----

FS_DE_F1.full.mod <- lmer(norm_F1 ~ lexSet +
                            sexSum +
                            # occClassSum +
                            ageGroupSum +
                            folManSum +
                            preSeg_smallSum +
                            freq.zipf_z +
                            time_z+
                            # (1+time_z|id) +
                            (1|id) +
                            (1|word),
                          data_FS_statsDE)
FS_DE_F1.full.mod %>% tidy()
cAIC4::cAIC(FS_DE_F1.full.mod)
FS_DE_F1.step <- cAIC4::stepcAIC(FS_DE_F1.full.mod,calcNonOptimMod=TRUE,numberOfSavedModels = 1)
FS_DE_F1.step
FS_DE_F1.step.og <- lmer(norm_F1 ~ lexSet + 
                            sexSum +
                            ageGroupSum +
                            folManSum +
                            preSeg_smallSum +
                            freq.zipf_z +
                            time_z +
                            (1 | word),
                          data_FS_statsDE)
FS_DE_F1.step.mod <- FS_DE_F1.step.og %>%
  tidy()
print(FS_DE_F1.step.mod, n=nrow(FS_DE_F1.step.mod))


print(
  xtable(
    (FS_DE_F1.step.mod %>%
       filter(effect=="fixed") %>%
       dplyr::select(term, estimate, statistic) %>%
       dplyr::rename(tvalue = statistic) %>% 
       dplyr::rename(fixedeffect = term)),
    caption = "Linear Mixed Effects Model of F1 of \\textsc{foot} and \\textsc{strut} in DECTE speakers \\label{tbl:FSF1DE}"),
  include.rownames=FALSE,
  file="models/FS-DE-F1-mod.tex"
)

### F2 ---------------
FS_DE_F2.full.mod <- lmer(norm_F2 ~ lexSet*ageGroup +
                            sexSum+
                            ageGroupSum +
                            folManSum +
                            preSeg_smallSum +
                            freq.zipf_z +
                            time_z+
                            # (1+time_z|id) +
                            (1|id)+
                            (1|word),
                          data_FS_statsDE)
FS_DE_F2.full.mod %>% tidy()
cAIC4::cAIC(FS_DE_F2.full.mod)
FS_DE_F2.step <- cAIC4::stepcAIC(FS_DE_F2.full.mod,calcNonOptimMod=TRUE,numberOfSavedModels = 1)
FS_DE_F2.step 
FS_DE_F2.step.og <- lmer(norm_F2 ~ lexSet*ageGroup +
                           sexSum+
                           # ageGroupSum +
                           folManSum +
                           preSeg_smallSum +
                           freq.zipf_z +
                           time_z+
                           # (1+time_z|id) +
                           (1|id)+
                           (1|word),
                         data_FS_statsDE)
FS_DE_F2.step.mod <- FS_DE_F2.step.og %>%
  tidy()
print(FS_DE_F2.step.mod, n=nrow(FS_DE_F2.step.mod))


print(
  xtable(
    (FS_DE_F2.step.mod %>%
       filter(effect=="fixed") %>%
       dplyr::select(term, estimate, statistic) %>%
       dplyr::rename(tvalue = statistic) %>% 
       dplyr::rename(fixedeffect = term)),
    caption = "Linear Mixed Effects Model of F2 of \\textsc{foot} and \\textsc{strut} in DECTE speakers \\label{tbl:FSF2DE}"),
  include.rownames=FALSE,
  file="models/FS-DE-F2-mod.tex"
)
# FOOT -------------
data_foot_stats <- data_FS_stats %>% filter(lexSet == "FOOT")

contrasts(data_foot_stats$lexSetSum) <- contr.sum(2)
contrasts(data_foot_stats$corpusSum) <- contr.sum(3)
contrasts(data_foot_stats$sexSum) <- contr.sum(2)
contrasts(data_foot_stats$ageGroupSum) <- contr.sum(2)
contrasts(data_foot_stats$styleSum) <- contr.sum(3)
contrasts(data_foot_stats$folManSum) <- contr.sum(4)
contrasts(data_foot_stats$preSeg_smallSum) <- contr.sum(5)

## F1 ####

FOOT_F1.full.mod <- lmer(norm_F1 ~ relevel(corpus, "CoRP-NE")+
                            sex+
                            ageGroup +
                            folMan +
                            preSeg_small +
                            freq.zipf_z +
                            time_z+
                           style +
                            # (1+time_z|id) +
                            (1|id)+
                            (1|word),
                          data_foot_stats)
FOOT_F1.full.mod %>% tidy()



# STRUT ------------
data_strut_stats <- data_FS_stats %>% filter(lexSet == "STRUT")

contrasts(data_strut_stats$lexSetSum) <- contr.sum(2)
contrasts(data_strut_stats$corpusSum) <- contr.sum(3)
contrasts(data_strut_stats$sexSum) <- contr.sum(2)
contrasts(data_strut_stats$ageGroupSum) <- contr.sum(2)
contrasts(data_strut_stats$styleSum) <- contr.sum(3)
contrasts(data_strut_stats$folManSum) <- contr.sum(4)
contrasts(data_strut_stats$preSeg_smallSum) <- contr.sum(5)


## F1 ####
strut_F1.full.mod <- lmer(norm_F1 ~ relevel(corpus, "CoRP-NE") + 
                            sex+
                            ageGroup +
                            folMan +
                            preSeg_small +
                            freq.zipf_z +
                            style +
                            # time_z+
                            (1+time_z|id) +
                            # (1|id) +
                            (1|word),
                          data_strut_stats)
cAIC(strut_F1.full.mod)
strut_F1.step <- cAIC4::stepcAIC(strut_F1.full.mod,calcNonOptimMod=TRUE,numberOfSavedModels = 2)
strut_F1.step

strut_F1.step.mod <- lmer(norm_F1 ~ relevel(corpus, "CoRP-NE") + 
                            sex+
                            ageGroup +
                            folMan +
                            preSeg_small +
                            freq.zipf_z +
                            style +
                            # time_z+
                            # (1+time_z|id) +
                            (1|id) +
                            (1|word),
                          data_strut_stats) %>%
  tidy()
print(strut_F1.step.mod, n=nrow(strut_F1.step.mod))


print(
  xtable(
    (strut_F1.step.mod %>%
       filter(effect=="fixed") %>%
       dplyr::select(term, estimate, statistic) %>%
       dplyr::rename(tvalue = statistic) %>% 
       dplyr::rename(fixedeffect = term)),
    caption = "Linear Mixed Effects Model of F1 of \\textsc{strut} \\label{tbl:SF1}"),
  include.rownames=FALSE,
  file="models/S-F1-mod.tex"
)
## F2 ####
strut_F2.full.mod <- lmer(norm_F2 ~ relevel(corpus, "CoRP-NE") + 
                            sex+
                            ageGroup +
                            folMan +
                            preSeg_small +
                            freq.zipf_z +
                            style +
                            time_z+
                            # (1+time_z|id) +
                            (1|id) +
                            (1|word),
                          data_strut_stats)
cAIC(strut_F2.full.mod)
strut_F2.step <- cAIC4::stepcAIC(strut_F2.full.mod,calcNonOptimMod=TRUE,numberOfSavedModels = 2)
strut_F2.step

strut_F2.step.mod <- lmer(norm_F2 ~ relevel(corpus, "CoRP-NE") + 
                            sex+
                            ageGroup +
                            folMan +
                            preSeg_small +
                            freq.zipf_z +
                            style +
                            time_z+
                            # (1+time_z|id) +
                            (1|id) +
                            (1|word),
                          data_strut_stats) %>%
  tidy()
print(strut_F2.step.mod, n=nrow(strut_F2.step.mod))


print(
  xtable(
    (strut_F2.step.mod %>%
       filter(effect=="fixed") %>%
       dplyr::select(term, estimate, statistic) %>%
       dplyr::rename(tvalue = statistic) %>% 
       dplyr::rename(fixedeffect = term)),
    caption = "Linear Mixed Effects Model of F2 of \\textsc{strut} \\label{tbl:SF2}"),
  include.rownames=FALSE,
  file="models/S-F2-mod.tex"
)
