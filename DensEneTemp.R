#############################################
# DENSITY, ENERGY & TEMPERATURE PLOTS
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

# Intuitive way
#Dens <- read.table("density.dat", sep = "", col.names = "dens")
#Dens$ns <- seq(1,21000)
#Dens$ns <- (as.numeric(as.numeric(Dens$ns)) / 100)*2


# R way
txt_files <- c("density.dat", "energy.dat", "temperature.dat")
data_list <- lapply(txt_files, read.table, sep = "", col.names = c("value"))
data_names <-  c("Dens", "Ene", "Temp")
names(data_list)[1:3] <- data_names

for (i in data_names) {
  data_list[[i]]["ns"] <- seq(1,21000)
  data_list[[i]]["ns"] <- (as.numeric(as.numeric(data_list[[i]]$ns)) / 100)*2
}


#############################################
# PLOTS
#############################################

# Density
p_dens <- ggplot() +
  geom_line(data = data_list$Dens, aes(x=ns, y=value), color="black", size=0.4)+
  theme_minimal() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.title.x.top = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(-0.1, "cm"),
        axis.text.x.bottom = element_text(margin=margin(0.2,0,0,0, "cm")),
        axis.text.y.left = element_text(margin=margin(0,0.5,0,0, "cm"))) +
  scale_x_continuous(breaks = seq(0,420,by=50), sec.axis = dup_axis(labels = rep("",9))) +
  scale_y_continuous(breaks = seq(1.022,1.044,by=0.004), expand = c(0, 0), limits = c(1.022,1.044),
                     sec.axis = dup_axis(labels=rep("",6), name = element_blank())) +
  labs(y = bquote("Density"~(g/cm^2)), x = "Time (ns)") +
  annotate("text", x=25, y=1.042, label="(a)", size = 3)

p_dens


# Potential energy
p_ene  <- ggplot() +
  geom_line(data = data_list$Ene, aes(x=ns, y=value), color="forestgreen", size=0.4)

p_ene  <- ggplot() +
  geom_line(data = data_list$Ene, aes(x=ns, y=value), color="forestgreen", size=0.4)+
  theme_minimal() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.title.x.top = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(-0.1, "cm"),
        axis.text.x.bottom = element_text(margin=margin(0.2,0,0,0, "cm")),
        axis.text.y = element_text(margin=margin(0,0.5,0,0, "cm"))) +
  scale_x_continuous(breaks = seq(0,420,by=50), sec.axis = dup_axis(labels = rep("",9))) +
  scale_y_continuous(breaks = seq(-105400,-103800,by=400), expand = c(0, 0), limits = c(-105400,-103800),
                     sec.axis = dup_axis(labels=rep("",5), name = element_blank())) +
  labs(y = "Potential energy (kcal/mol)", x = "Time (ns)") +
  annotate("text", x=25, y=-105300, label="(b)", size = 3)

p_ene


# Temperature
p_temp <- ggplot() +
  geom_line(data = data_list$Temp, aes(x=ns, y=value), color="darkblue", size=0.4)+
  theme_minimal() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.title.x.top = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(-0.1, "cm"),
        axis.text.x.bottom = element_text(margin=margin(0.2,0,0,0, "cm")),
        axis.text.y = element_text(margin=margin(0,0.5,0,0, "cm"))) +
  scale_x_continuous(breaks = seq(0,420,by=50), sec.axis = dup_axis(labels = rep("",9))) +
  scale_y_continuous(breaks = seq(295,300,by=0.5), expand = c(0, 0), limits = c(295,300),
                     labels = c("295", "", "296", "", "297", "", "298", "", "299", "", "300"),
                     sec.axis = dup_axis(labels=rep("",11), name = element_blank())) +
  labs(y = "Temperature (K)", x = "Time (ns)") +
  annotate("text", x=25, y=299.5, label="(c)", size = 3)

p_temp



#############################################
# ASSEMBLE AND SAVE
#############################################

A <- p_dens + p_ene + p_temp

ggsave(plot = A, "DensEneTemp.png", dpi = 300, width = 19, height = 6)












