lexSet <- rep(c("FOOT","STRUT"), each = 2)
ageGroup <- rep(c("Old","Young"), times = 2)
sexSum <- c(1,1,1,)
folManSum <- c(0,0,0,0)
preSeg_smallSum <- c(0,0,0,0)
freq.zipf_z <- c(0,0,0,0)
time_z <- c(0,0,0,0)
(1|id)
  (1|word)

newdata <- tibble(lexSet,ageGroup, sexSum,folManSum,preSeg_smallSum,freq.zipf_z,time_z) %>% 
  mutate(lexSet =factor(lexSet)) %>% 
  mutate(ageGroup =factor(ageGroup)) %>% 
  mutate(sexSum =factor(sexSum)) %>%
  mutate(folManSum =factor(folManSum)) %>%
  mutate(preSeg_smallSum =factor(preSeg_smallSum)) %>%
  mutate(freq.zipf_z =factor(freq.zipf_z)) %>%
  mutate(time_z =factor(time_z))



newdata$fit <- predict(FS_DE_F2.step.og, newdata)
newdata
 #########################
I <- 1468.01
S <- -27.34
M <- -261.86
Y <- -77.10
SM <- 203.82
SY <- 52.55
MY <- 162.76
SMY <- -240.35