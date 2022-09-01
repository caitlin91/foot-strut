FS_DE_F1.plot <- ggplot(data_FS_plots %>% filter(corpus == "DECTE-NE"), aes(x=lexSet,y=norm_F1,fill=lexSet)) +
  geom_boxplot() +
  geom_point(data=data_FS_plots_DE.means, aes(y=mean_F1,lexSet,colour=NULL))+
  geom_line(data=data_FS_plots_DE.means, aes(y=mean_F1,x=lexSet,group=id,colour=NULL))+
  theme_Caitlin_present() +
  theme(legend.position = "none") +
  xlab("lexical set") +
  ylab("F1 (Hz)") +
  # facet_wrap(~id)+
  # facet_grid(rows=vars(sex),cols=vars(id)) +
  FSFillScale +
  scale_y_continuous(limits = c(200,1000), breaks = seq(0,1000,100))+
  # ggtitle("\\scs{foot}-STRUT (F1) DECTE") +
  ylab("F1 (Hz)") +
  xlab("lexical set") +
  NULL
FS_DE_F1.plot
ggsave("figures/FS-DE-F1_present.png", FS_DE_F1.plot, width=6, height = 4, units = "in")

#------------------------------------------
FS_SE_F1.plot = ggplot(data_FS_plots %>% filter(corpus == "CoRP-SE"), aes(x=lexSet,y=norm_F1,fill=lexSet)) +
  geom_boxplot() +
  # geom_point(data=data_FS_plots_SE.means, aes(y=mean_F1,lexSet))+
  # geom_line(data=data_FS_plots_SE.means, aes(y=mean_F1,x=lexSet,group=id))+
  xlab("lexical set") +
  ylab("F1 (Hz)") +
  scale_y_continuous(limits = c(300,1000), breaks = seq(0,1000,100))+
  FSFillScale +
  # ggtitle("\\scs{foot}-STRUT (F1) CoRP-SE") +
  # facet_wrap(~sex) +
  NULL
FS_SE_F1.plot
ggsave("figures/FS-SE-F1_present.png",FS_SE_F1.plot,height=4,width=6,units="in")

#----------------------------------------
FS_NE_F1.plot <- ggplot(data_FS_plots %>% filter(corpus == "CoRP-NE"), aes(x=lexSet,y=norm_F1,fill=lexSet,colour=ageGroup)) +
  geom_boxplot() +
  # geom_point(data=data_FS_plots_NE.means, aes(y=mean_F1,lexSet,colour=ageGroup))+
  # geom_line(data=data_FS_plots_NE.means, aes(y=mean_F1,x=lexSet,group=id,colour=ageGroup))+
  xlab("lexical set") +
  ylab("F1 (Hz)") +
  theme_Caitlin_present() +
  theme(legend.position = "top") +
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
ggsave("figures/FS-NE-F1_present.png", FS_NE_F1.plot, width=6, height = 4, units = "in")
