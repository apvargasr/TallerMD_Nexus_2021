#############################################
# SASA PLOTS BY DOMAIN
# A. Paula Vargas Ruiz
# Github: apvargasr
# Zimic-Sheen lab. UPCH, Lima, Peru.
#
# Adaptado para:
# Taller Nexus UNALM. 26 jun 21.
#############################################


# Load libraries
library(patchwork)
library(ggplot2)

# Check working directory
setwd("~/CursoMD_NEXUS/Pics/")



#############################################
# LOAD PROTEIN DATA BY DOMAIN
#############################################

sasas <- read.csv(file = "SasaByDomain/sasas_by_domain.csv", header = T, quote = "", stringsAsFactors = F)
sasas$ns <- seq(1,21000)
sasas$ns <- (as.numeric(as.numeric(sasas$ns)) / 100)*2



#############################################
# PLOTS
#############################################

p_sasa <- ggplot() +
  geom_smooth(data = sasas, aes(x=ns, y=ep1, colour="D1")) +
  geom_smooth(data = sasas, aes(x=ns, y=ep2, colour="D2")) +
  geom_smooth(data = sasas, aes(x=ns, y=ep3, colour="D3")) +
  geom_smooth(data = sasas, aes(x=ns, y=ep4, colour="D4")) +
  geom_smooth(data = sasas, aes(x=ns, y=ep5, colour="D5")) +
  geom_smooth(data = sasas, aes(x=ns, y=ep6, colour="D6")) +
  geom_smooth(data = sasas, aes(x=ns, y=ep7, colour="D7")) +
  geom_smooth(data = sasas, aes(x=ns, y=ep8, colour="D8")) +
  scale_color_manual(name = "", breaks = paste0("D", 1:8),
                     values = c(D1 = "blue", D2 = "red", D3 = "black", D4 = "orange",
                                D5 = "yellow", D6 = "green", D7 = "pink", D8 = "cyan")) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length=unit(-0.25, "cm"),
        axis.text.x = element_text(margin=margin(0.5,0,0,0, "cm"), size = 10),
        axis.text.y = element_text(margin=margin(0,0.5,0,0, "cm"), size = 10)) +
  scale_x_continuous(breaks = seq(0,420,by=50), expand = c(0, 0),  limits = c(0, 420),
                     sec.axis = dup_axis(labels = rep("",9), name = element_blank())) +
  scale_y_continuous(breaks = seq(500,1500,by=200), expand = c(0, 0), limits = c(500, 1500),
                     sec.axis = dup_axis(labels=rep("",6), name = element_blank())) +
  labs(y=expression(paste("SASA (", ring(A^2),")")), x="Time (ns)")

p_sasa


#############################################
# SAVE PLOT
#############################################

ggsave(plot = p_sasa, "sasaplot.png", dpi = 300, width = 6, height = 5)





