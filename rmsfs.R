#############################################
# RMSF PLOTS
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

data <- read.table("rmsf.dat", sep = "", col.names = c("resn", "rmsf"))


#############################################
# PLOTS
#############################################

# General
p_rmsf  <- ggplot() +
  geom_line(data=data, aes(x=resn, y=rmsf), color="black", size=0.5)+
  theme_minimal() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size = 10),
        axis.title.x.top = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(-0.1, "cm"),
        axis.text.x = element_text(margin=margin(0.5,0,0,0,"cm")),
        axis.text.y = element_text(margin=margin(0,0.5,0,0, "cm"), size = 7)) + 
  #plot.margin = unit(c(0, 0, 0, 0), "cm")) +
  scale_x_continuous(breaks = seq(0,104, by=20), sec.axis = dup_axis(labels = rep("",6))) +
  scale_y_continuous(breaks = seq(0,10,by=2), expand = c(0, 0), limits = c(0, 10),
                     sec.axis = dup_axis(labels=rep("",6), name = element_blank())) +
  labs(y = expression(paste("RMSF (", ring(A),")")), x = "Residue number")

p_rmsf


# By domain
p_rmsf_d  <- ggplot() +
  geom_line(data=data[1:9,], aes(x=resn, y=rmsf), color="blue", size=0.5)+
  geom_line(data=data[9:18,], aes(x=resn, y=rmsf), color="red", size=0.5)+
  geom_line(data=data[18:33,], aes(x=resn, y=rmsf), color="black", size=0.5)+
  geom_line(data=data[33:48,], aes(x=resn, y=rmsf), color="orange", size=0.5)+
  geom_line(data=data[48:64,], aes(x=resn, y=rmsf), color="yellow", size=0.5)+
  geom_line(data=data[64:73,], aes(x=resn, y=rmsf), color="green", size=0.5)+
  geom_line(data=data[73:88,], aes(x=resn, y=rmsf), color="pink", size=0.5)+
  geom_line(data=data[88:104,], aes(x=resn, y=rmsf), color="cyan", size=0.5)+
  theme_minimal() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size = 10),
        axis.title.x.top = element_blank(),
        axis.title.y = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(-0.1, "cm"),
        axis.text.x = element_text(margin=margin(0.5,0,0,0,"cm")),
        axis.text.y = element_text(margin=margin(0,0.5,0,0, "cm"), size = 7)) + 
  #plot.margin = unit(c(0, 0, 0, 0), "cm")) +
  scale_x_continuous(breaks = seq(0,104, by=20), sec.axis = dup_axis(labels = rep("",6))) +
  scale_y_continuous(breaks = seq(0,10,by=2), expand = c(0, 0), limits = c(0, 10),
                     sec.axis = dup_axis(labels=rep("",6), name = element_blank())) +
  labs(y = expression(paste("RMSF (", ring(A),")")), x = "Residue number")

p_rmsf_d



#############################################
# ASSEMBLE AND SAVE
#############################################

A <- p_rmsf + p_rmsf_d

ggsave(plot = A, "rmsf_plot.png", dpi = 300, width = 9.5, height = 5)








