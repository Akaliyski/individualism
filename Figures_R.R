################################################################################
# Project: Individualism‐Collectivism: Reconstructing Hofstede’s Cultural Index
# Purpose:  Generate all data visualizations for Studies 1 and 2 in R
# Author:   Plamen Akaliyski (Lingnan University)
# Contact:  akaliyski.plamen@gmail.com
# Date:     2025‐06‐01
# Last Updated: 2025‐06‐01

# This script uses the processed country‐level data (exported from Stata) to produce the figures

# All outputs are saved to “…/Replication_Files/Output/Figures from R.”
################################################################################

options(rlib_downstream_check = FALSE)

library(gapminder)     # Example datasets
library(here)          # Simplify file paths
library(socviz)        # Social science visualization utilities
library(tidyverse)     # Core tidy data packages: ggplot2, dplyr, tidyr, etc.
library(haven)         # Read Stata (.dta) and SPSS/SAS files
library(foreign)       # Legacy data import (older .dta, .dbf, etc.)
library(readxl)        # Read Excel workbooks
library(cowplot)       # Arrange multiple ggplots into grids
library(ggrepel)       # Prevent overlapping text labels in ggplot2
library(ggalt)         # Extra geoms for “geographic” plots
library(ggfortify)     # Auto‐plotting for PCA and other stats objects
library(ggforce)       # Additional ggplot2 extensions (e.g. facet_zoom)
library(concaveman)    # Compute concave hulls around scatter points
library(magrittr)      # Pipe operators (`%>%`)—already in tidyverse but explicit here
library(dplyr)         # Data manipulation verbs (select, mutate, filter, etc.)
library(ggpubr)        # Publication‐ready ggplot2 functions and themes
library(pheatmap)      # Pretty heatmap visualization
library(reshape)       # Reshape data frames (melt/cast) – older package, but used in parts
library(countrycode)   # Convert between country code schemes (iso3c, iso3n, etc.)
library(patchwork)     # Compose multiple ggplots with simple syntax
library(mcr)           # Method comparison regression (Deming, Bland‐Altman)
library(maps)          # Map data (world, US states, etc.)
library(ggnewscale)    # Support for multiple fill/color scales in a single ggplot
library(fillpattern)   # Fill polygon geoms with stripes, dots, etc.
library(ggthemes)      # Extra ggplot2 themes (e.g., theme_fivethirtyeight)
library(ggtext)        # Render markdown/HTML in ggplot2 text elements

# Load custom fonts (e.g., “Segoe UI”) to ensure consistent styling
extrafont::loadfonts(quiet = TRUE)

# ------------------------------------------------------------------------------
# Define the folder containing the data
data_dir <- "C:/Users/akali/OneDrive - Lingnan University/Desktop/Replication_Files/Data/R_data"

# Set working directory to where figures will be saved
setwd("C:/Users/akali/OneDrive - Lingnan University/Desktop/Replication_Files/Output/Figures from R")




################################################################################
#################   Study 1a: Convergent Validity  #############################
################################################################################

#-------------------------------------------------------------------------------
# Figure 2 – Difference between Hofstede’s I-C and a combined I-C index
#-------------------------------------------------------------------------------

# Read Stata file from data_dir
mydata <- read.dta(file.path(data_dir, "Difference_from_Hofstede.dta"))

# ---- Left panel: combined index vs. Hofstede ----
H_C_plot <- ggplot(data=mydata, aes(x=IDV_combined, y=idv_Hofs, label=country, color=EN_EA)) +
  geom_point(position=position_jitter(width=0.2, height=0), aes(color=EN_EA), size=3) + 
  geom_text_repel(aes(label=country), size=3.5, box.padding = 0.5, point.padding = 0.5) + 
  geom_abline(intercept = 0, slope = 1, color = "black", size = 0.5) +  # Isoline
  labs(x="Collectivism-Individualism Average of Recent Measures", y="Collectivism-Individualism Hofstede", color="Region") +
  theme_classic() +
  theme(legend.position = c(1, 0.1),  # Place the legend at the bottom right
        legend.justification = c(1, 0),  # Anchor the legend at its bottom right corner
        text = element_text(size=15)) +
annotate("text", 
         x = Inf, y = Inf, 
         label = "Isoline", 
         hjust = 1.2, vjust = 2, 
         size = 5, fontface = "italic", color = "black") 

ggsave("Scatter_plot_with_Hofstede_and_combined_I-C.png", plot = H_C_plot, height = 7, width = 8, dpi = 300)



# ---- Right panel: horizontal deviations ----
start_value <- -20  
end_value <- 50     
right_side_position <- max(mydata$IDV_difference, na.rm = TRUE) + 5 

H_C <- ggplot(data=subset(mydata, !is.na(IDV_difference )), aes(x=IDV_difference , y=reorder(country, IDV_difference), color = EN_EA)) +
  geom_point(size = 2) +
  geom_segment(aes(x = 0, 
                   xend = IDV_difference, 
                   y = reorder(country, IDV_difference), 
                   yend = reorder(country, IDV_difference)), size = 0.2) + 
  geom_vline(xintercept = 0, linewidth = 0.5, color = "grey80") +
  geom_vline(xintercept = seq(from = start_value, to = end_value, by = 10), color = "grey90", size = 0.2, alpha = 0.8) + 
  labs (x = "Understimated Scores << >> Overestimated Scores                ",
        y = "",
        color = NULL,
        title = "               Deviations") +
  theme_classic() + 
  theme(axis.title.x = element_text(size = 12, color = "Grey30")) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  theme(legend.position = "none") + 
  geom_text(aes(label=sprintf("%.1f", IDV_difference), x = right_side_position, y = reorder(country, IDV_difference)),
            size=2.7, hjust=0.3, vjust=0.5)  + 
  coord_cartesian(clip = "off", xlim = c(NA, right_side_position + 6)) 

# ---- Combine and save both panels ----
ggsave("Differences.png", plot = H_C, height = 7, width = 3.2, dpi = 300)

combined_plot <- H_C_plot +  H_C + plot_layout(widths = c(2, 1))

ggsave("Figure 2.png", combined_plot, width = 12, height = 6)





#-------------------------------------------------------------------------------
# Figure S1 – Difference between Hofstede’s I-C and the combined WVS-based index
#-------------------------------------------------------------------------------

# (Assumes mydata is already loaded from “Difference_from_Hofstede.dta”)

# ---- Left panel: WVS-based combined I-C index vs. Hofstede ----
H_C_plot2 <- ggplot(data=mydata, aes(x=IDV_combined2, y=idv_Hofs, label=country, color=EN_EA)) +
  geom_point(position=position_jitter(width=0.2, height=0), aes(color=EN_EA), size=3) + 
  geom_text_repel(aes(label=country), size=3.5, box.padding = 0.5, point.padding = 0.5) + 
  geom_abline(intercept = 0, slope = 1, color = "black", size = 0.5) +  # Isoline
  labs(x="Collectivism-Individualism Average of Recent Measures", y="Collectivism-Individualism Hofstede", color="Region") +
  theme_classic() +
  theme(legend.position = c(1, 0.1),  # Place the legend at the bottom right
        legend.justification = c(1, 0),  # Anchor the legend at its bottom right corner
        text = element_text(size=15)) +
  annotate("text", 
           x = Inf, y = Inf, 
           label = "Isoline", 
           hjust = 1.2, vjust = 2, 
           size = 5, fontface = "italic", color = "black") 

ggsave("Scatter_plot_with_Hofstede_and_combined_IDV2.png", plot = H_C_plot2, height = 7, width = 8, dpi = 300)


# ---- Right panel: horizontal deviations (WVS-based) ----
start_value <- -20
end_value <- 50   
right_side_position2 <- max(mydata$IDV_difference2, na.rm = TRUE) + 5 

H_C2 <- ggplot(data=subset(mydata, !is.na(IDV_difference2)), aes(x=IDV_difference2 , y=reorder(country, IDV_difference2), color = EN_EA)) +
  geom_point(size = 2) +
  geom_segment(aes(x = 0, 
                   xend = IDV_difference2, 
                   y = reorder(country, IDV_difference2), 
                   yend = reorder(country, IDV_difference2)), size = 0.2) + 
  geom_vline(xintercept = 0, linewidth = 0.5, color = "grey80") +
  geom_vline(xintercept = seq(from = start_value, to = end_value, by = 10), color = "grey90", size = 0.2, alpha = 0.8) + 
  labs (x = "Understimated Scores << >> Overestimated Scores                ",
        y = "",
        color = NULL,
        title = "               Deviations") +
  theme_classic() + 
  theme(axis.title.x = element_text(size = 12, color = "Grey30")) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  theme(legend.position = "none") + 
  geom_text(aes(label=sprintf("%.1f", IDV_difference2), x = right_side_position2, y = reorder(country, IDV_difference2)),
            size=2.7, hjust=0.3, vjust=0.5)  + 
  coord_cartesian(clip = "off", xlim = c(NA, right_side_position + 6)) 

# ---- Combine and save both panels ----
ggsave("Differences.png", plot = H_C, height = 7, width = 3.2, dpi = 300)

combined_plot2 <- H_C_plot2 +  H_C2 + plot_layout(widths = c(2, 1))

ggsave("SOM Figure S1.png", combined_plot2, width = 12, height = 6)






#-------------------------------------------------------------------------------
## Cleveland dot plots created for each dimension separately (for SOM)
#-------------------------------------------------------------------------------

# Read Stata file from data_dir
mydata <- read.dta(file.path(data_dir, "All individualism measures.dta"))

#-------------------------------------------------------------------------------
# Figure S2 – Difference from Hofstede’s I-C (Beugelsdijk & Welzel)
#-------------------------------------------------------------------------------
H_BW <- ggplot(data=subset(mydata, !is.na(diff_H_BW)), aes(x=diff_H_BW, y=reorder(country_IDV_dim, diff_H_BW), color = EN_EA)) +
  geom_point(size = 2) +
  geom_segment(aes(x = 0, 
                   xend = diff_H_BW, 
                   y = reorder(country_IDV_dim, diff_H_BW), 
                   yend = reorder(country_IDV_dim, diff_H_BW)), size = 0.2) + 
  geom_vline(xintercept = 0, size = 0.5, color = "grey80") +
  labs (x = "Understimated scores << Same scores >> Overstimated scores                                      ",
        y = "",
        color = NULL,
        title = "Difference from Hofstede's Individualism-Collectivism Scores") +
  theme_minimal() + 
  theme(axis.title.x = element_text(size = 10, color = "Grey30")) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  theme(legend.position = c(.85, .40))

ggsave("Figure S2 Beulegsdijk and Welzel.pdf", plot = H_BW, height = 8.5, width = 8)


#-------------------------------------------------------------------------------
# Figure S3 – Difference from Hofstede’s I-C (Minkov)
#-------------------------------------------------------------------------------
H_M <- ggplot(data=subset(mydata, !is.na(diff_H_M)), aes(x=diff_H_M, y=reorder(country_IDV_dim, diff_H_M), color = EN_EA)) +
  geom_point(size = 2) +
  geom_segment(aes(x = 0, 
                   xend = diff_H_M, 
                   y = reorder(country_IDV_dim, diff_H_M), 
                   yend = reorder(country_IDV_dim, diff_H_M)), size = 0.2) + 
  geom_vline(xintercept = 0, size = 0.5, color = "grey80") +
  labs (x = "      Understimated scores << Same scores >> Overstimated scores",
        y = "",
        color = NULL,
        title = "Difference from Hofstede's Individualism-Collectivism Scores") +
  theme_minimal() + 
  theme(axis.title.x = element_text(size = 9, color = "Grey30")) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  theme(legend.position = c(.85, .40))

ggsave("Figure S3 IDV Minkov.pdf", plot = H_M, height = 7.5, width = 8)


#-------------------------------------------------------------------------------
# Figure S4 – Difference from Hofstede’s I-C (GLOBE’s I-C)
#-------------------------------------------------------------------------------
H_GL <- ggplot(data=subset(mydata, !is.na(diff_H_GL)), aes(x=diff_H_GL, y=reorder(country_IDV_dim, diff_H_GL), color = EN_EA)) +
  geom_point(size = 2) +
  geom_segment(aes(x = 0, 
                   xend = diff_H_GL, 
                   y = reorder(country_IDV_dim, diff_H_GL), 
                   yend = reorder(country_IDV_dim, diff_H_GL)), size = 0.2) + 
  geom_vline(xintercept = 0, size = 0.5, color = "grey80") + 
  labs (x = "Understimated scores << Same scores >> Overstimated scores                                                      ",
        y = "",
        color = NULL,
        title = "Difference from Hofstede's Individualism-Collectivism Scores") +
  theme_minimal() + 
  theme(axis.title.x = element_text(size = 9, color = "Grey30"))  +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  theme(legend.position = c(.85, .40))

ggsave("Figure S4 GLOBE's I-C.pdf", plot = H_GL, height = 7, width = 8)


#-------------------------------------------------------------------------------
# Figure S5 – Difference from Hofstede’s I-C (Inglehart’s I-C)
#-------------------------------------------------------------------------------
H_INGL <- ggplot(data=subset(mydata, !is.na(diff_H_INGL)), aes(x=diff_H_INGL, y=reorder(country_IDV_dim, diff_H_INGL), color = EN_EA)) +
  geom_point(size = 2) +
  geom_segment(aes(x = 0, 
                   xend = diff_H_INGL, 
                   y = reorder(country_IDV_dim, diff_H_INGL), 
                   yend = reorder(country_IDV_dim, diff_H_INGL)), size = 0.2) + 
  geom_vline(xintercept = 0, size = 0.5, color = "grey80") + 
  labs (x = "                               Understimated scores << Same scores >> Overstimated scores                                                      ",
        y = "",
        color = NULL,
        title = "Difference from Hofstede's Individualism-Collectivism Scores") +
  theme_minimal() + 
  theme(axis.title.x = element_text(size = 9, color = "Grey30"))  +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  theme(legend.position = c(.85, .40))

ggsave("Figure S5 Inglehart's I-C.pdf", plot = H_INGL, height = 9, width = 8)


#-------------------------------------------------------------------------------
# Figure S6 – Difference from Hofstede’s I-C (Emancipative Values)
#-------------------------------------------------------------------------------
H_EVI <- ggplot(data=subset(mydata, !is.na(diff_H_EVI)), aes(x=diff_H_EVI, y=reorder(country_IDV_dim, diff_H_EVI), color = EN_EA)) +
  geom_point(size = 2) +
  geom_segment(aes(x = 0, 
                   xend = diff_H_EVI, 
                   y = reorder(country_IDV_dim, diff_H_EVI), 
                   yend = reorder(country_IDV_dim, diff_H_EVI)), size = 0.2) + 
  geom_vline(xintercept = 0, size = 0.5, color = "grey80") +
  labs (x = "Understimated scores << Same scores >> Overstimated scores               ",
        y = "",
        color = NULL,
        title = "Difference from Hofstede's Individualism-Collectivism Scores") +
  theme_minimal() + 
  theme(axis.title.x = element_text(size = 9, color = "Grey30"))  +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  theme(legend.position = c(.85, .40))

ggsave("Figure S6 EVI.pdf", plot = H_EVI, height = 9, width = 8)


#-------------------------------------------------------------------------------
# Figure S7 – Difference from Hofstede’s I-C (Autonomy‐Embeddedness)
#-------------------------------------------------------------------------------
H_A <- ggplot(data=subset(mydata, !is.na(diff_H_A)), aes(x=diff_H_A, y=reorder(country_IDV_dim, diff_H_A), color = EN_EA)) +
  geom_point(size = 2) +
  geom_segment(aes(x = 0, 
                   xend = diff_H_A, 
                   y = reorder(country_IDV_dim, diff_H_A), 
                   yend = reorder(country_IDV_dim, diff_H_A)), size = 0.2) + 
  geom_vline(xintercept = 0, size = 0.5, color = "grey80") + 
  labs (x = "Understimated scores << Same scores >> Overstimated scores           ",
        y = "",
        color = NULL,
        title = "Difference from Hofstede's Individualism-Collectivism Scores") +
  theme_minimal() + 
  theme(axis.title.x = element_text(size = 9, color = "Grey30"))  +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  theme(legend.position = c(.85, .40))

ggsave("Figure S7 Embeddedness-Autonomy.pdf", plot = H_A, height = 7.5, width = 8)











################################################################################
#################   Study 1b: Nomological Validity ###############################
################################################################################

#-------------------------------------------------------------------------------
# Figure 3 – Nomological network of Hofstede’s I-C and six alternative measures
#-------------------------------------------------------------------------------

# Read Excel file from data_dir
df <- read_excel(file.path(data_dir, "Study2b_Nomological_Correlations.xlsx"))

CZ <- df %>% 
  pivot_longer(!indicator, names_to = "Individualism", values_to = "Value") %>% 
  ggplot() + 
  geom_tile(aes(x = factor(Individualism, ordered = TRUE, levels = c("I-C (Hofstede)",	
                                                                     "I-C (B&W)",	
                                                                     "I-C (Minkov)",
                                                                     "I-C (GLOBE)",
                                                                     "I-C (Inglehart)",
                                                                     "EVI (Welzel)",	
                                                                     "Aut-Emb (Schw)")), y = factor(indicator, ordered = TRUE, levels = c( "Liberal Democracy Index", 
                                                                                                                                     "Political Corrption Index (r)",
                                                                                                                                     "Corruption Perception Index",
                                                                                                                                     "Human Rights and Rule of Law",
                                                                                                                                     "Social Progress Index",
                                                                                                                                     "GDP per capita (log)",
                                                                                                                                     "Human Development Index")), fill = Value), color = "black") +
  scale_fill_gradient(low = "white", high = "blue") + 
  geom_text(aes(x = Individualism, y = indicator, label = format(round(Value,2),nsmall=1)), size = 5.5) +
  labs(y = "Societal Indicator", x = "I-C Measure", fill = "Correlation") +
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=15, face="bold")) + 
  theme(legend.position = "bottom")+ 
  theme(legend.title = element_text(size=14, face="bold")) + 
  theme(legend.text = element_text(size=11)) +
  scale_fill_gradient(low = "white", high = "blue",
    breaks = c(0.6, 0.7, 0.8), 
    labels = c(".60", ".70", ".80"))  

ggsave("Figure 3 Nomological network of IDV dimensions.PNG", plot = CZ, height = 5, width = 12)













################################################################################
#################   Study 1c: Implications for Predictive Utility   ############
################################################################################

#-------------------------------------------------------------------------------
# Figure 4 – Comparison of individualism indices as predictors of obesity
#-------------------------------------------------------------------------------

# Read Excel file from data_dir
mydata <- read_excel(file.path(data_dir, "Study2c_individualism_and_obesity.xlsx"))

mydata$dimension <- factor(mydata$dimension, c("Aut-Emb (Schwartz)", "EVI (Welzel)", "I-C (Inglehart)", "I-C (GLOBE)", "I-C (Minkov)", "I-C (Beug. & Welzel)","I-C (Hofstede)"))

# Left Panel: Obesity among women
p <- ggplot(data = mydata, mapping = aes(x = dimension, y = obesity_f, color = color))

women <- p + geom_hline(yintercept = 0, size = 1, color = "grey80") + 
  geom_point(position = position_dodge(0.3), size=3.5) + 
  geom_errorbar(mapping = aes(ymin = obesity_f-1.96*SE_f, ymax = obesity_f+1.96*SE_f), width = 0.05) + 
  labs(x = "Individualism-collectivism dimension", 
       y = "Standardized beta coefficients", color = NULL, title = "Obesity among women") +
  coord_flip()  +
  theme_classic2() +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("Predictors of obesity women.png", plot = women, height = 4, width = 8, dpi = 300)


# Right Panel: Obesity among men
p <- ggplot(data = mydata, mapping = aes(x = dimension, y = obesity_m, color = color))

men <- p + geom_hline(yintercept = 0, size = 1, color = "grey80") +
  geom_point(position = position_dodge(0.3), size=3.5) + 
  geom_errorbar(mapping = aes(ymin = obesity_m-1.96*SE_m, ymax = obesity_m+1.96*SE_m), width = 0.05) + 
  labs(x = NULL, 
       y = "Standardized beta coefficients", color = NULL, title = "Obesity among men") +
  coord_flip() +
  theme_classic2() + 
  theme(legend.position = "none") +
  theme(axis.text.y = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("Predictors of obesity men.png", plot = men, height = 4, width = 8, dpi = 300)

# Combine both panels and save as Figure 4
combined_obesity <- cowplot::plot_grid(women, men, nrow = 1, rel_widths = c(0.66, 0.34), scale = 1)

ggsave("Figure 4 Individualism and obesity.png", plot = combined_obesity, height = 4, width = 10, dpi = 300)






#-------------------------------------------------------------------------------
# Figure S8 – Comparison of individualism indices as predictors of obesity (fixed sample)
#-------------------------------------------------------------------------------
# Read Excel file from data_dir
mydata2 <- read_excel(file.path(data_dir, "individualism and obesity - fixed sample.xlsx"))

mydata2$dimension <- factor(mydata2$dimension, c("Aut-Emb (Schwartz)", "EVI (Welzel)", "I-C (Inglehart)", "I-C (GLOBE)", "I-C (Minkov)", "I-C (Beug. & Welzel)","I-C (Hofstede)"))

#For females 
p2 <- ggplot(data = mydata2, mapping = aes(x = dimension, y = obesity_f, color = color))

women2 <- p2 + geom_hline(yintercept = 0, size = 1, color = "grey80") + 
  geom_point(position = position_dodge(0.3), size=3.5) + 
  geom_errorbar(mapping = aes(ymin = obesity_f-1.96*SE_f, ymax = obesity_f+1.96*SE_f), width = 0.05) + 
  labs(x = "I-C dimension", 
       y = "Standardized beta coefficients", color = NULL, title = "Obesity among women") +
  coord_flip()  +
  theme_classic2() +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("Predictors of obesity women2.png", plot = women2, height = 4, width = 8, dpi = 300)


#For males 
p2 <- ggplot(data = mydata2, mapping = aes(x = dimension, y = obesity_m, color = color))

men2 <- p2 + geom_hline(yintercept = 0, size = 1, color = "grey80") +
  geom_point(position = position_dodge(0.3), size=3.5) + 
  geom_errorbar(mapping = aes(ymin = obesity_m-1.96*SE_m, ymax = obesity_m+1.96*SE_m), width = 0.05) + 
  labs(x = NULL, 
       y = "Standardized beta coefficients", color = NULL, title = "Obesity among men") +
  coord_flip() +
  theme_classic2() + 
  theme(legend.position = "none") +
  theme(axis.text.y = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("Predictors of obesity men2.png", plot = men2, height = 4, width = 8, dpi = 300)

# Combine both panels and save as Figure S8
combined_obesity2 <- cowplot::plot_grid(women2, men2, nrow = 1, rel_widths = c(0.66, 0.34), scale = 1)

ggsave("Figure S8 Individualism and obesity2.png", plot = combined_obesity2, height = 4, width = 10, dpi = 300)















################################################################################
#################   Study 2c: Mapping Global Variation           ############
################################################################################

#-------------------------------------------------------------------------------
# Figure 7 – Map and ranking of new I-C country scores
#-------------------------------------------------------------------------------

# plot settings
fam <- "Segoe UI"   # adjust font (must be installed on system)
digs <- 1           # number of decimals for country scores

# import data from data_dir
df <- openxlsx::read.xlsx(file.path(data_dir, "Data_IC_Country_Ranking.xlsx"))

# load world map data and fix mismatched country names
world <- as_tibble(map_data("world")) %>% 
  mutate(region = case_when(
    # replace some country names by hand which are stored differently
    subregion == "Macao" ~ "Macao",
    subregion == "Hong Kong" ~ "Hong Kong",
    subregion == "Northern Ireland" ~ "Northern Ireland",
    subregion == "Great Britain" ~ "Great Britain",
    region    == "Micronesia" ~ "Federated States of Micronesia",
    TRUE ~ region
  )) %>% 
  mutate(countryname = countrycode(region, "country.name", "country.name",
                                   custom_match = c("Northern Ireland" = "Northern Ireland",
                                                    "Great Britain" = "Great Britain")))

df <- df %>% mutate(countryname = countrycode(countryn, "iso3n", "country.name",
                                              custom_match = c("909" = "Northern Ireland",
                                                               "826" = "Great Britain")))
dfmap <- df %>% 
  full_join(world, by = join_by(countryname)) %>% 
  filter(region !="Antarctica") # not shown on map


# get sample quantiles and median for centering of color palette
indiv_med <- median(df$individualism) # 45.64928
lims <- indiv_med + c(-0.725, 0.725) * indiv_med # choose a range symmetric around median that still includes min and max
breaks_quantiles <- quantile(df$individualism, c(0.05, 0.25,0.5,0.75, 0.95)) # quantiles for breaks, with median as center
names(breaks_quantiles) <- paste0(
  names(breaks_quantiles),
  " (", format(round(breaks_quantiles, 1), nsmall = 1), ")"
)

# color scales
fills <- paletteer::scale_fill_paletteer_c("grDevices::Geyser", # <- adjust color palette, 
                                           # choose a suitable one from: https://pmassicotte.github.io/paletteer_gallery/
                                           na.value="white", direction = -1,
                                           breaks = breaks_quantiles,
                                           #breaks = seq(30,70,10),
                                           limits = lims,
                                           # limits = c(head(qs, 1),
                                           #            tail(qs, 1))
) 


# plot map
midpart <- ggplot(dfmap,
                  aes(x = long, y = lat, group = group)) + 
  geom_polygon(aes(fill = individualism), col = "black", linewidth = 0.2) + 
  coord_fixed(1.2) + 
  theme_void() + 
  theme(text = element_text(family = fam),
        legend.position = "inside",
        legend.position.inside = c(0.180,0.3),
        legend.key.size = unit(0.175, "in"),
        #panel.border = element_rect(colour = "black", fill=NA, linewidth = 0.5),
        #legend.title = element_text(size = 6),
        legend.text = element_text(size = 7, family = fam),
        legend.title = element_text(size = 8, family = fam), 
        plot.margin = margin(0, 0, 0, 0)
  ) +
  guides(fill = guide_legend(override.aes = list(col = "white"), ncol = 1,
                             reverse = TRUE) ) +
  labs(fill= "Percentile:") + 
  fills
midpart

# country score lists
dflist <- df %>% 
  select(countryname, individualism) %>% 
  mutate(countryname = case_when(
    countryname == "Turkey" ~ "Türkiye",
    countryname == "Palestinian Territories" ~ "Palestine",
    countryname == "Myanmar (Burma)" ~ "Myanmar",
    TRUE ~ countryname
  )) %>% 
  arrange(desc(individualism))

# Define splitting rules 
nper <- ceiling(102 / 6)  

splitting <- c(
  rep(1, nper * 2.2),  # Proportion for group 1
  rep(2, nper * 0.425),  # Proportion for group 2
  rep(3, nper * 0.425),  # Proportion for group 3
  rep(4, nper * 0.425),  # Proportion for group 4
  rep(5, nper * 0.425),  # Proportion for group 5
  rep(6, nper * 2.2)   # Proportion for group 6
)

length(splitting)>= 102


# add rank to country name
dflist <- dflist %>% 
  mutate(countryname = paste0(rank(-individualism), ": ", countryname))

# create plots with names and scores
sideplots <- purrr::map2(
  split(dflist, splitting), 
  list(FALSE, TRUE, TRUE, TRUE, TRUE, FALSE),
  function(dat, space){
    
    pname <- dat %>%
      ggplot() +
      geom_text(
        aes(
          x = 0,
          y = rank(individualism),
          label = countryname
        ),
        family = fam,
        size = 2.25,
        hjust = 0
      ) +
      theme_void() +
      scale_y_continuous(expand = if(isTRUE(space)) c(0.2, 0.2) else c(3e-2, 3e-2)) +
      scale_x_continuous(limits = c(0,1)) +
      theme(text = element_text(family = fam),
            plot.margin = margin(0,0 , 0, 0))
    
    pscore <- dat %>%
      ggplot() +
      geom_text(
        aes(
          x = 0,
          #fill = individualism,
          y = rank(individualism),
          label = format(round(individualism, digs), nsmall = digs)
        ),
        # show.legend = FALSE,
        # label.padding = unit(0.5, "lines"),
        # label.r = unit(0, "lines"),
        # label.size = 0,
        family = fam,
        size = 2.25,
        hjust = 0
      ) +
      theme_void() +
      scale_y_continuous(expand = if(isTRUE(space)) c(0.2, 0.2) else c(3e-2, 3e-2)) +
      theme(text = element_text(family = fam),
            plot.margin = margin(0, 1, 0, 0)) +
      fills
    
    wrap_plots(pname, pscore, widths = c(3,1))
  })

# build layout
layout <- '
AAAAA##BBBBB##EEEEE##GGGGG
AAAAADDDDDDDDDDDDDDDDGGGGG
AAAAADDDDDDDDDDDDDDDDGGGGG
AAAAADDDDDDDDDDDDDDDDGGGGG
AAAAA##CCCCC##FFFFF##GGGGG
'
pp <- wrap_plots(
  A = sideplots[[1]],
  B = sideplots[[2]],
  C = sideplots[[3]],
  D = midpart,
  E = sideplots[[4]],
  F = sideplots[[5]],
  G = sideplots[[6]],
  design = layout
)

# export
png("Figure 7.png", width = 5000, height = 2600, res = 580)
print(pp)
dev.off()











#-------------------------------------------------------------------------------
# Figure 8 – Mean I-C Scores by Domain and Culture Zone
#-------------------------------------------------------------------------------

# Read the Excel file
df <- read_excel(file.path(data_dir, "IDV by culture zone.xlsx"))

# Rename culture_zones column explicitly and recode its values
df <- df %>%
  dplyr::rename(Region = culture_zones) %>%
  mutate(
    Region = case_when(
      Region == "East Asia"            ~ "East Asia",
      Region == "Islamic East"         ~ "Islamic",
      Region == "Latin America"        ~ "Lat. Amer.",
      Region == "New West"             ~ "New West",
      Region == "Old West"             ~ "Old West",
      Region == "Orthodox East"        ~ "Orthodox",
      Region == "Reformed West"        ~ "Ref. West",
      Region == "Returned West"        ~ "Ret. West",
      Region == "South and SE Asia"    ~ "S/SE Asia",
      Region == "Sub-Saharan Africa"   ~ "SS Africa",
      TRUE ~ Region
    )
  )

# 3) Pivot longer so that each row is (Region, Domain, Value)
pivoted_data <- df %>%
  pivot_longer(
    cols      = -Region,
    names_to  = "Domain",
    values_to = "Value"
  ) %>%
  # If you want nicer domain names, replace these manually:
  mutate(
    Domain = case_when(
      Domain == "individualism_mean"    ~ "Individualism (vs. Collectivism)",
      Domain == "permissivness_mean"    ~ "Childrearing for self-direction",
      Domain == "neighbors_mean"        ~ "Equal trust",
      Domain == "trustSD_mean"          ~ "Accepting diverse others",
      Domain == "equality_mean"         ~ "Egalitarian beliefs",
      Domain == "obligations_mean"      ~ "Distancing from traditional duties",
      Domain == "childrearing_mean"     ~ "Accepting diverse life choices",
      TRUE ~ Domain
    )
  ) %>%
  # Now rank within each Domain for coloring
  group_by(Domain) %>%
  mutate(Rank = rank(Value, ties.method = "first")) %>%
  ungroup()

# Plot with text labels for the mean values, and color boxes by reversed rank
CZ <- ggplot(pivoted_data) + 
  geom_tile(aes(x = factor(Region, ordered = TRUE, levels = c("Ref. West", "New West", "Old West", "Ret. West", "East Asia", "Lat. Amer.", "Orthodox", "S/SE Asia", "SS Africa", "Islamic")), 
                y = factor(Domain, ordered = TRUE, levels = c("Individualism (vs. Collectivism)",
                                                              "Accepting diverse life choices",
                                                              "Accepting diverse others",  
                                                              "Equal trust",
                                                              "Egalitarian beliefs",
                                                              "Distancing from traditional duties", 
                                                              "Childrearing for self-direction")), 
                fill = Rank), 
            color = "black") +
  scale_fill_gradient(low = "white", high = "blue", labels = scales::label_number(accuracy = 1)) +
  geom_text(aes(x = Region, y = Domain, label = format(round(Value, 1), nsmall = 1)), size = 5.5) +
  labs(y = "Individualism-Collectivism Subindex", x = "Culture zone", fill = "Rank") +
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=15, face="bold")) + 
  theme(legend.position = "bottom")+ 
  theme(legend.title = element_text(size=14, face="bold")) + 
  theme(legend.text = element_text(size=11))

ggsave("Figure 8.png", plot = CZ, height = 5, width = 12, dpi = 300)












################################################################################
#################   Study 2d: Validating the New I-C Index   ##################
################################################################################

#-------------------------------------------------------------------------------
# Figure 9 – Nomological analysis
#-------------------------------------------------------------------------------

# Read Excel file from data_dir
df <- read_excel(file.path(data_dir, "Indicators for Individualism.xlsx"))

custom_colors <- c("#006400", "#4B0082")

p <- ggplot(data = df, mapping = aes(y = Individualism, x = Individualism_Hofstede, size = n_cases))

corr <- p +  
  geom_point(aes()) + 
  geom_text_repel(aes(label=Indicator), 
                  size = 3.33, 
                  box.padding = 0.32, 
                  force = 8) +  
  labs(y = "Correlation with Novel Individualism-Collectivism Index", 
       x = "Correlation with Hofstede Individualism-Collectivism Index", 
       fill=NULL, 
       size = "Sample size", 
       shape = NULL) +
  guides(size = guide_legend("Sample size")) +
  guides(fill = "none") + 
  theme_minimal_grid()   +  
  theme(legend.title = element_text(size = 8),
        legend.text = element_text(size = 6)) + 
  scale_y_continuous(limits = c(.40,.972), breaks = seq(0.40, 1, by = 0.10), expand = expansion(mult = c(0, 0.05))) +
  scale_x_continuous(limits = c(.40,.972), breaks = seq(0.40, 1, by = 0.10), expand = expansion(mult = c(0, 0.06))) +
  scale_size_continuous(range = c(2, 2.7), breaks = c(56,57,58), labels = scales::number_format(accuracy = 1)) + 
  scale_color_manual(values = custom_colors) + 
  theme(legend.title = element_text(size = 13), 
        legend.text = element_text(size = 11),
        legend.position = c(0.90, 0.10),     
        legend.justification = c("right", "bottom"),
        legend.box.background = element_rect(fill = "white", color = "grey"), 
        legend.margin = margin(10, 10, 10, 10)) +  
  geom_abline(
    mapping = NULL,
    data = NULL,
    na.rm = FALSE,
    show.legend = NA
  ) + 
  annotate("richtext", x = 0.42, y = 0.96, label = "<b>Correlations higher with New I-C index</b>", 
           hjust = 0, size = 4.5, fill = NA, label.color = NA) +
  annotate("richtext", x = 0.96, y = 0.58, label = "<b>Correlations higher with Hofstede's I-C index</b>", 
           hjust = 1, size = 4.5, fill = NA, label.color = NA)


ggsave("Figure 9 Nomological validity.PDF", plot = corr, height = 10, width = 10)











################################################################################
#################   Discussion Section   #######################################
################################################################################

#-------------------------------------------------------------------------------
# Figure 10 – Map showing differences between Hofstede’s scores and new I-C scores
#-------------------------------------------------------------------------------

# plot settings
fam <- "Segoe UI"  # adjust font (must be installed on system)
digs <- 1          # number of decimals for labels
ggpattern::geom_polygon_pattern()
# import data
df <- read_excel("C:/Users/akali/OneDrive - Lingnan University/Desktop/Data viz with R/Hofstede individualism/Data/New I-C vs. Hofstede country scores.xlsx")

df <- df %>% 
  mutate(across(c(individualism , idv_Hofs ), as.numeric)) %>% 
  mutate(diff = individualism - idv_Hofs) %>% 
  mutate(DataLegend = case_when(
    DataLegend == "Countries only in Hofstede data" ~ "Only in Hofstede's data",
    DataLegend == "New countries" ~ "New country scores",
    TRUE ~ DataLegend  # Preserve other values
  )) 


# color scales
fills <- paletteer::scale_fill_paletteer_c("pals::coolwarm", # <- adjust color palette for the score difference
                                           # choose a suitable one from: https://pmassicotte.github.io/paletteer_gallery/
                                           na.value="white",
                                           direction = -1,
                                           breaks = seq(-40,40,20),
                                           limits = c(-55, 55),
                                           labels = ifelse(seq(-40,40,20) > 1, paste0("+", seq(-40,40,20)), seq(-40,40,20))) 

# load world
world <- as_tibble(map_data("world")) %>% 
  mutate(region = case_when(
    # replace some country names by hand which are stored differently
    subregion == "Macao" ~ "Macao",
    subregion == "Hong Kong" ~ "Hong Kong",
    subregion == "Northern Ireland" ~ "Northern Ireland",
    subregion == "Great Britain" ~ "Great Britain",
    region    == "Micronesia" ~ "Federated States of Micronesia",
    TRUE ~ region
  )) %>% 
  mutate(countryname = countrycode(region, "country.name", "country.name",
                                   custom_match = c("Northern Ireland" = "Northern Ireland",
                                                    "Great Britain" = "Great Britain")))

df <- df %>% mutate(countryname = countrycode(country, "country.name", "country.name",
                                              custom_match = c("Northern Ireland" = "Northern Ireland",
                                                               "Great Britain" = "Great Britain")))

df %>% filter(countryname %in% c("Kenya", "South Africa", "Lesotho"))


dfmap <- df %>% 
  full_join(world, by = join_by(countryname)) %>% 
  filter(region !="Antarctica") %>%  # not shown on map
  mutate(DataLegend = ifelse(is.na(DataLegend), "No data", DataLegend))

dfmap1 <- dfmap %>% filter(DataLegend == "Overlapping countries")
dfmap2 <- dfmap %>% filter(DataLegend != "Overlapping countries")

dfmap2 %>% 
  filter(countryname == "Lesotho")

# plot map
midpart <- ggplot() + 
  geom_polygon(data = dfmap1,
               aes(x = long, y = lat, group = region, subgroup = group, fill = diff),
               col = "black", linewidth = 0.2) +
  fills +
  coord_fixed(1.2) + 
  theme_void() +
  theme(text = element_text(family = fam),
        legend.position = "inside",
        legend.position.inside = c(0.19, 0.315),
        legend.key.size = unit(0.175, "in"),
        legend.title = element_text(size = 9, family = fam),
        legend.text = element_text(size = 7, family = fam),
        plot.margin = margin(0, 0, 0, 0)
  ) +
  guides(fill = guide_legend(override.aes = list(col = "white"), ncol = 1,reverse = TRUE)) +
  labs(fill= "Difference from\nHofstede") +
  new_scale_fill() +
  geom_polygon(data = dfmap2, 
               aes(x = long, y = lat, group = region, subgroup = group, fill = DataLegend), 
               col = "black", 
               linewidth = 0.2) +
  scale_fill_pattern(
    patterns = c("stripe", "circle", "stripe"),
    fg = c("#1b9e77", "#F5F5F5", "#7570b3"), # <- colors for the stripes
    bg = rep("white", 3),
    angle = c(360-45, NA, 0),
    width = 1,
    min_size = unit(1e-5, "mm"),
    lwd = 0.9
  ) +
  guides(fill = guide_legend(override.aes = list(col = "white"), ncol = 1)) +
  labs(fill = "")

# country score lists
dflist <- df %>% 
  select(countryname, diff) %>% 
  mutate(countryname = case_when(
    countryname == "Turkey" ~ "Türkiye",
    countryname == "Hong Kong SAR China" ~ "Hong Kong SAR",
    TRUE ~ countryname
  )) %>% 
  arrange(desc(diff)) %>% 
  filter(!is.na(diff))


# create plots with names and scores
sideplots <- purrr::map2(
  list(head(dflist, 15),
       tail(dflist, 15)), 
  list(TRUE, FALSE),
  function(dat, dir){
    
    pname <- dat %>%
      ggplot() +
      geom_text(
        aes(
          x = if(!dir) 1 else 0,
          y = rank(diff),
          label = countryname
        ),
        family = fam,
        size = 2.5,
        hjust = if(!dir) 1 else 0
      ) +
      theme_void() +
      scale_x_continuous(limits = c(0,1)) +
      theme(text = element_text(family = fam),
            plot.margin = margin(0,0 , 0, 0),
            axis.title.y = element_text(angle = 90)) + 
      labs(
        y = if(isTRUE(dir)) expression("Largest positive difference"~symbol('\256')) else (
          expression(symbol('\254')~"Largest negative difference")
        )
      )
    if(!dir){
      pname <- pname + scale_y_continuous(position = "right")
    }
    wrap_plots(pname)
  })

combined_plot <- midpart +
  inset_element(sideplots[[1]], left = 0, bottom = 0.2, right = 0.2, top = 0.7) +
  inset_element(sideplots[[2]], left = 0.8, bottom = 0.2, right = 1, top = 0.7)

combined_plot


ggsave(combined_plot, filename = "Figure 10.png",
       device = ragg::agg_png,
       width = 4000, height = 1600, dpi = 340,
       units = "px")


# layout <- paste(
#   "ABBBBBBBBBBBBC",
#   "ABBBBBBBBBBBBC",
#   "ABBBBBBBBBBBBC",
#   "ABBBBBBBBBBBBC",
#   "ABBBBBBBBBBBBC",
#   sep = "\n"
# )

# pp <- wrap_plots(
#   A = sideplots[[1]],
#   B = midpart,
#   C = sideplots[[2]],
#   design = layout
# )












#-------------------------------------------------------------------------------
# Figure S12 – Scatterplot showing differences between Hofstede scores and new I-C scores
#-------------------------------------------------------------------------------

# Read Excel file from data_dir
mydata <- read_excel(file.path(data_dir, "Comparison_Hofstede_NewIC.xlsx"))

# Perform PCA on the two variables to get orthogonal slope
x1 <- mydata$idv_Hofs 
y1 <- mydata$individualism

pca1 <- prcomp(cbind(x1, y1))$rotation

# Extract the slope from the first principal component
pca_slope1 <- pca1[2, 1] / pca1[1, 1]

# Calculate the intercept using the means of x and y
pca_intercept1 <- mean(y1) - pca_slope1 * mean(x1)


H_C_plot1 <- ggplot(data=mydata, aes(x=idv_Hofs, y=individualism, label=country, color=culture_zones, shape=culture_zones)) +
  geom_point(position=position_jitter(width=0.2, height=0), aes(color=culture_zones), size=3) + 
  geom_text_repel(aes(label=country), size=3.5, box.padding = 0.5, point.padding = 0.5) + 
  geom_abline(intercept = pca_intercept1, slope = pca_slope1, color = "black", size = 0.5) +
  scale_shape_manual(values = c(16, 17, 15, 3, 7, 8, 10, 12, 13)) +  # Specify unique shapes
  labs(x="Collectivism-Individualism Hofstede" , y="New Collectivism-Individualism Scores", color="Culture Zone", shape="Culture Zone") +
  theme_classic() +
  theme(legend.position = c(1, 0.05),  # Place the legend at the bottom right
        legend.justification = c(1, 0),  # Anchor the legend at its bottom right corner
        text = element_text(size=15))

ggsave("Figure S12.png", plot = H_C_plot1, height = 7, width = 8, dpi = 300)


