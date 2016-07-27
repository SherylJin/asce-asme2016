# --------------------------------------------------------------------------- #
# ASCE-ASME paper - plot definitions
# --------------------------------------------------------------------------- #

bottomlegend <- theme(legend.position = 'bottom', legend.direction = 'horizontal', legend.title = element_blank())
rightlegend <- theme(legend.title = element_blank())
nolegend <- guides(linetype="none", fill="none", group="none", colour="none")
ijarcols <- c("#b2df8a", "#1f78b4")
ijarcol <- scale_colour_manual(values = ijarcols)
ijarfill <- scale_fill_manual(values = ijarcols)

#