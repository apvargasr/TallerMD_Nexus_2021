#############################################
# RMSD PLOTS
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

data <- read.table("rmsd_allprot.dat", sep = "", col.names = "rmsd")
data$ns <- seq(1,21000)
data$ns <- (as.numeric(as.numeric(data$ns)) / 100)*2



#############################################
# PLOTS
#############################################

p_rmsd  <- ggplot() +
  #geom_vline(xintercept = 156, color = "green3", size = 0.5) +
  geom_line(data=data, aes(x=ns, y=rmsd), color="black",
            size=0.4) +
  theme_minimal() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.title.x.top = element_blank(),
        axis.title.y.right = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(-0.1, "cm"),
        axis.text.x = element_text(margin=margin(0.5,0,0,0,"cm")),
        axis.text.y = element_text(margin=margin(0,0.5,0,0, "cm"))) +
  scale_x_continuous(breaks = seq(0,420,by=50),limits = c(0, 420), sec.axis = dup_axis(labels = rep("",9))) +
  scale_y_continuous(breaks = seq(0,10,by=1), expand = c(0, 0), limits = c(0, 10),
                     sec.axis = dup_axis(labels=rep("",11), name = element_blank())) +
  labs(y=expression(paste("RMSD (", ring(A),")")), x = "Time (ns)") 

p_rmsd


# Add a trendline 
p_rmsd_trend  <- ggplot(data, aes(x=ns, y=rmsd)) +
  #geom_vline(xintercept = 156, color = "green3", size = 0.5) +
  geom_line(data=data, aes(x=ns, y=rmsd), color="black",
            size=0.4) +
  theme_minimal() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.title.x.top = element_blank(),
        axis.title.y.right = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(-0.1, "cm"),
        axis.text.x = element_text(margin=margin(0.5,0,0,0,"cm")),
        axis.text.y = element_text(margin=margin(0,0.5,0,0, "cm"))) +
  scale_x_continuous(breaks = seq(0,420,by=50),limits = c(0, 420), sec.axis = dup_axis(labels = rep("",9))) +
  scale_y_continuous(breaks = seq(0,10,by=1), expand = c(0, 0), limits = c(0, 10),
                     sec.axis = dup_axis(labels=rep("",11), name = element_blank())) +
  labs(y=expression(paste("RMSD (", ring(A),")")), x = "Time (ns)") +
  stat_smooth(method = 'lm', aes(colour = 'linear'), se = FALSE) +
  stat_smooth(method = 'lm', formula = y ~ poly(x,2), aes(colour = 'polynomial'), se= FALSE) +
  stat_smooth(method = 'nls', formula = y ~ a * log(x) + b, aes(colour = 'logarithmic'), se = FALSE, method.args = list(start = list(a = 1, b = 1)))

p_rmsd_trend




#############################################
# ASSEMBLE AND SAVE
#############################################

A <- p_rmsd + p_rmsd_trend

ggsave(plot = A, "rmsd_plot.png", dpi = 300, width = 9.5, height = 5)
















