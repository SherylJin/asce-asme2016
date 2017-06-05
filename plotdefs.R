# --------------------------------------------------------------------------- #
# ASCE-ASME paper - plot definitions
# --------------------------------------------------------------------------- #

bottomlegend <- theme(legend.position = 'bottom', legend.direction = 'horizontal', legend.title = element_blank())
rightlegend <- theme(legend.title = element_blank())
nolegend <- guides(linetype="none", fill="none", group="none", colour="none")
ijarcols <- c("#b2df8a", "#1f78b4")
ijarcol <- scale_colour_manual(values = ijarcols)
ijarfill <- scale_fill_manual(values = ijarcols)

#library(extrafont) # has problems with transparency in figures
#font_import()
#loadfonts()

#install.packages("showtext")
#library(showtext)
#old = setwd(tempdir())
#download.file("http://downloads.sourceforge.net/project/cm-unicode/cm-unicode/0.7.0/cm-unicode-0.7.0-ttf.tar.xz",
#              "cm-unicode-0.7.0-ttf.tar.xz")
#untar("cm-unicode-0.7.0-ttf.tar.xz", compressed = "xz")
#setwd("cm-unicode-0.7.0")
#font.add("CM Roman",
#         regular = "cmunrm.ttf",
#         italic = "cmunti.ttf",
#         symbol = "cmunti.ttf")
#setwd(old)

#