#############################################
# HBOND PLOTS
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
# LOAD PROTEIN DATA
#############################################

data <- read.table("hbonds.dat", sep = "", col.names = "hb")
data$ns <- seq(1,21000)
data$ns <- (as.numeric(as.numeric(data$ns)) / 100)*2



#############################################
# PLOTS
#############################################

p_hbonds <- ggplot() +
  geom_line(data = data, aes(x = ns, y = hb),
            color="black", size=0.4)+
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size = 10),
        axis.title.x.top = element_blank(),
        axis.title.y.right = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length=unit(-0.1, "cm"),
        axis.text.x = element_text(margin=margin(0.5,0,0,0,"cm")),
        axis.text.y = element_text(margin=margin(0,0.5,0,0, "cm"), size = 7)) +
  scale_x_continuous(breaks = seq(0,420, by=50), sec.axis = dup_axis(labels = rep("",9))) +
  scale_y_continuous(breaks = seq(0,20, by=4), expand = c(0, 0), limits = c(0, 20),
                     sec.axis = dup_axis(labels=rep("",6), name = element_blank())) +
  labs(y = "Hydrogen bonds", x = "Time (ns)")

p_hbonds



#############################################
# SAVE PLOT
#############################################

ggsave(plot = p_hbonds, "hbondplot.png", dpi = 300, width = 6, height = 5)


