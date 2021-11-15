## Themes -----------
theme_Caitlin_present <- function() {theme_bw(base_size = 22) %+replace%
    theme(plot.background  = element_rect(fill = "transparent", colour = NA),
          panel.background = element_rect(fill="gray90", colour=NA),
          legend.background = element_rect(fill="transparent", colour=NA),
          legend.key = element_rect(fill="transparent", colour=NA),
          panel.grid.major = element_line(colour = "white", size = 0.2), 
          panel.grid.minor = element_line(colour = "grey98", size = 0.5))}

theme_Caitlin <- function() {theme_bw(base_size = 12) %+replace%
    theme(plot.background  = element_rect(fill = "transparent", colour = NA),
          panel.background = element_rect(fill="gray90", colour=NA),
          legend.background = element_rect(fill="transparent", colour=NA),
          legend.key = element_rect(fill="transparent", colour=NA),
          panel.grid.major = element_line(colour = "white", size = 0.2), 
          panel.grid.minor = element_line(colour = "grey98", size = 0.5))}

### Colours --------------------
# region colours -----------
regionColours <- c("#77b144","#524c95")
names(regionColours) = levels(data_goat$region_ord)
regionFill <- scale_fill_manual(name = "region_ord",values = regionColours)
regionCol <- scale_colour_manual(name = "region_ord",values=regionColours)


# goatcolours ---------------
goatColours <- c("#172869", "#1BB6AF","#de8c62","#0076BB", "#FF3200")
names(goatColours) = levels(data_goat$lexicalSet_narrow)
goatFill <- scale_fill_manual(name = "lexicalSet_narrow",values = goatColours)
goatCol <- scale_colour_manual(name = "lexicalSet_narrow",values=goatColours)

# FS colours --------------
FSColours <- c("#C70E7B", "#A6E000")
names(FSColours) = levels(data_FS_plots$lexSet)
FSFillScale <- scale_fill_manual(name = "lexSet", values = FSColours)
FSColScale <- scale_colour_manual(name= "lexSet", values = FSColours)
