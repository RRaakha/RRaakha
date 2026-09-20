
# THIS CODE This report uses Bayesian statistical methods to study Body Mass Index (BMI) among Beninese women aged 15–49 years, 
#based on data from the 2017–18 Demographic and Health Surveys in Benin. 
#The main aim is to understand how factors such as place of residence, social and economic conditions, and 
#reproductive characteristics influence women’s BMI across different geographic areas. 
#The study applied a Bayesian geo-additive semi-parametric regression model, which is a flexible approach that can capture 
#both linear and non-linear relationships while also accounting for geographic differences 

library(haven)
library(purrr)

library(dplyr)
library(ggplot2)
library(leaflet)
library(tmap)
library(tmaptools)
library(RColorBrewer)
library(sf)

# Point to the FOLDER, not a specific file
files <- list.files(
  path = "C:\\Users\\Alamukii\\Documents\\MSc\\BayesX_assessment\\", 
  pattern = "\\.dta$", 
  full.names = TRUE
)

# Check it found the files
print(files)

# Now check dimensions of each
map_dfr(files, ~{
  d <- read_dta(.x)
  data.frame(file = basename(.x), rows = nrow(d), cols = ncol(d))
})





bmi <- read_dta("C:\\Users\\Alamukii\\Documents\\MSc\\BayesX_assessment\\BayesX_group_3.dta")

library(haven)
library(dplyr)

bmi <- read_dta("C:\\Users\\Alamukii\\Documents\\MSc\\BayesX_assessment\\BayesX_group_3.dta")

# Basic overview
dim(bmi)
names(bmi)
str(bmi)
summary(bmi)

# Check for missing values
colSums(is.na(bmi))





############################################
#CLEANING##################################
bmi$BMI <- bmi$v445 / 100

bmi_clean <- bmi %>%
  filter(!is.na(v445)) %>%
  mutate(
    BMI        = v445 / 100,
    age        = v012,
    region     = as.factor(v101),
    residence  = as.factor(v102),
    religion   = as.factor(v130),
    education  = as.factor(v149),
    birthorder = bord,
    childsex   = as.factor(b4),
    childalive = as.factor(b5)
  )

dim(bmi_clean)  # Should be ~991 rows
summary(bmi_clean$BMI)




library(ggplot2)
library(dplyr)

#################################################################
#OBSERVING THE DATA GRAPHICALLY ################################


# --- Plot 1: BMI Distribution ---
p1 <- ggplot(bmi_clean, aes(x = BMI)) +
  geom_histogram(aes(y = after_stat(density)), bins = 40,
                 fill = "darkgreen", colour = "white", alpha = 0.8) +
  geom_density(colour = "red", linewidth = 1) +
  geom_vline(xintercept = mean(bmi_clean$BMI), linetype = "dashed",
             colour = "red", linewidth = 0.8) +
  annotate("text", x = mean(bmi_clean$BMI) + 1.5, y = 0.12,
           label = paste0("Mean = ", round(mean(bmi_clean$BMI), 2)),
           colour = "darkblue", size = 3.5) +
  labs(title = "Fig1: BMI Distribution among Beninese women aged 15–49",
              x = "BMI (kg/m²)", y = "Density") +
  theme_minimal()+
  theme(panel.grid = element_blank())

print(p1)

# --- Plot 2: BMI by Region ---
region_labels <- c("1" = "Alibori", "2" = "Atacora", "3" = "Atlantique",
                   "4" = "Borgou", "5" = "Collines", "6" = "Couffo",
                   "7" = "Donga", "8" = "Littoral", "9" = "Mono",
                   "10" = "Oueme", "11" = "Plateau", "12" = "Zou")
p2 <- ggplot(bmi_clean, aes(x = region, y = BMI, fill = region)) +
  geom_boxplot(alpha = 0.7, outlier.colour = "darkred", outlier.size = 2) +
  scale_fill_manual(values = c("darkgreen", "darkblue")) +
  scale_x_discrete(labels = region_labels) +
  labs(title = "Fig 2: BMI distribution by region",
       x = "Group 3 Regions", y = "BMI (kg/m²)") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1),panel.grid.minor = element_blank())
print(p2)



# --- Plot 3: BMI vs Age (nonlinear trend) ---

p3 <- ggplot(bmi_clean, aes(x = age, y = BMI)) +
  geom_point(alpha = 0.2, colour = "darkorange", size = 1) +
  geom_smooth(method = "loess", colour = "darkgreen",
              linewidth = 1.2, linetype = "solid", se = TRUE) +
  geom_smooth(method = "lm", colour = "darkblue",
              linetype = "dotdash", linewidth = 0.9, se = FALSE) +
  labs(title = "Fig3: BMI by Age Plot with LOESS(non-parametric) & Linear Fit",
       subtitle = "Green = LOESS, Blue dashes = Linear",
       x = "Age (years)", y = "BMI (kg/m²)") +
  theme_classic() +
  theme(panel.grid = element_blank())

print(p3)

p4 <- ggplot(bmi_clean, aes(x = education, y = BMI, fill = education)) +
  geom_boxplot(alpha = 0.7, outlier.colour = "darkred", outlier.size = 2) +
  scale_fill_manual(values = c("purple4", "darkblue", "darkorange",
                               "darkgreen", "steelblue", "sienna")) +
  scale_x_discrete(labels = edu_labels) +
  labs(title = "Fig 4: BMI by Educational Level",
       x = "Education level", y = "BMI (kg/m²)") +
  theme_classic() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 30, hjust = 1),
        panel.grid = element_blank())

print(p4)

p5 <- ggplot(bmi_clean, aes(x = residence, y = BMI, fill = residence)) +
  geom_boxplot(alpha = 0.7, outlier.colour = "darkred", outlier.size = 2) +
  scale_fill_manual(values = c("darkgreen", "darkblue")) +
  scale_x_discrete(labels = c("1" = "Urban", "2" = "Rural")) +
  labs(title = "Fig 5: BMI by Type of Residence",
       x = "Residence", y = "BMI (kg/m²)") +
  theme_classic() +
  theme(legend.position = "none",
        panel.grid = element_blank())

print(p5)

p6 <- ggplot(bmi_clean, aes(x = birthorder, y = BMI)) +
  geom_point(alpha = 0.2, colour = "darkorange", size = 1) +
  geom_smooth(method = "loess", colour = "darkgreen",
              linewidth = 1.2, linetype = "solid", se = TRUE) +
  labs(title = "Fig 6: BMI vs Birth Order Number",
       x = "Birth order", y = "BMI (kg/m²)") +
  theme_classic() +
  theme(panel.grid = element_blank())

print(p6)




library(patchwork)
(p3 | p4)  # arrange as needed
combined_plot = wrap_plots(list(p3,p4), ncol = 2)


# Save as high-resolution image instead
ggsave("bmi_plots.png", 
       plot = combined_plot,   # your patchwork object
       width = 20, 
       height = 16, 
       dpi = 300,
       units = "in")
#######################################################
############# LETS MOVE TO REGION GRAPH AND ALL########

library(R2BayesX)
library(BayesX)

# Check available maps in R2BayesX
data(package = "R2BayesX")

# Also try
library(BayesXsrc)
table(bmi_clean$region)
levels(bmi_clean$region)
region_labels


# Loading Benin Map in the shap file 
Benin<-st_read("gadm41_BEN_shp\\gadm41_BEN_0.shp") # Read the Benin shapefile
Benin
### Loading department shapefile as sf file. 

benin<-st_read("gadm41_BEN_shp\\gadm41_BEN_1.shp") 
plot(benin)

# Check department names in shapefile
print(benin[, c("NAME_1", "GID_1")])

# Check which codes correspond to Atlantic and Borgou
# From DHS: 3 = Atlantic, 4 = Borgou
# Verify counts
table(bmi_clean$region)
prop.table(table(bmi_clean$region)) * 100
# Quick comparison
bmi_clean %>%
  group_by(region) %>%
  summarise(
    n       = n(),
    mean_BMI = round(mean(BMI), 2),
    sd_BMI   = round(sd(BMI), 2),
    median_BMI = round(median(BMI), 2)
  )

# BayesX needs a plain data.frame, not a tibble
# Also needs region as numeric integer for spatial term
bmi_bayesx <- as.data.frame(bmi_clean) %>%
  mutate(
    region_num = as.integer(as.character(region)),  # 3 or 4
    region_fac = as.factor(region),
    education  = as.factor(education),
    residence  = as.factor(residence),
    religion   = as.factor(religion),
    childsex   = as.factor(childsex),
    childalive = as.factor(childalive)
  )

# Verify
str(bmi_bayesx[, c("BMI","age","region_fac","residence",
                   "education","religion","birthorder",
                   "childsex","childalive")])
# Spatial/geographic main effect only
# With 2 regions, region enters as a fixed factor
m1 <- bayesx(BMI ~ region_fac,
             data   = bmi_bayesx,
             family = "gaussian",
             method = "MCMC",
             iterations = 12000,
             burnin     = 2000,
             step       = 10)

summary(m1)
plot(m1)

# Full geo-additive semi-parametric model
# - age and birthorder enter as nonparametric smooth terms (P-splines)
# - all categorical variables as fixed effects
# - region as fixed spatial factor

m2 <- bayesx(BMI ~ sx(age, bs = "ps") +
               sx(birthorder, bs = "ps") +
               region_fac +
               residence +
               education +
               religion +
               childsex +
               childalive,
             data   = bmi_bayesx,
             family = "gaussian",
             method = "MCMC",
             iterations = 12000,
             burnin     = 2000,
             step       = 10)

summary(m2)
plot(m2)










#############################################################

#Plot the maps##############################################

############################################################

library(ggplot2)


# Step 1: Calculate mean BMI per region from your data
region_bmi <- bmi_bayesx %>%
  group_by(region_num) %>%
  summarise(
    mean_BMI = mean(BMI, na.rm = TRUE),
    n = n()
  )

print(region_bmi)

# Step 2: Add a numeric region ID to the shapefile to match your data
# Your shapefile has 12 departments in order
# Atlantique = row 3 (GID BEN.3_1), Borgou = row 4 (GID BEN.4_1)
benin$region_num <- 1:12  # assigns 1-12 matching DHS codes

# Step 3: Join BMI data to shapefile
benin_bmi <- benin %>%
  left_join(region_bmi, by = "region_num")

# Step 4: Plot the map
ggplot(benin_bmi) +
  geom_sf(aes(fill = mean_BMI), colour = "white", linewidth = 0.5) +
  scale_fill_gradient(
    low  = "lightyellow",
    high = "darkred",
    na.value = "grey85",
    name = "Mean BMI\n(kg/m²)"
  ) +
  geom_sf_text(
    data = benin_bmi %>% filter(!is.na(mean_BMI)),
    aes(label = paste0(NAME_1, "\n", round(mean_BMI, 1))),
    size = 3.5, colour = "black"
  ) +
  labs(
    title    = "Figure 7: Mean BMI by region, Benin DHS 2017–18",
    subtitle = "Grey = regions not represented in Group 3 sample",
    caption  = "Atlantique (n=121) and Borgou (n=870)"
  ) +
  theme_void() +
  theme(
    plot.title    = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10),
    legend.position = "right"
  )

# Check shapefile row order matches DHS codes
print(benin[, c("NAME_1", "GID_1", "region_num")])


# Improved version - fixes label overlap and adds sample size to labels
ggplot(benin_bmi) +
  geom_sf(aes(fill = mean_BMI), colour = "white", linewidth = 0.5) +
  scale_fill_gradient(
    low      = "lightyellow",
    high     = "darkred",
    na.value = "grey85",
    name     = "Mean BMI\n(kg/m²)",
    limits   = c(21.9, 22.3)
  ) +
  geom_sf_label(
    data = benin_bmi %>% filter(!is.na(mean_BMI)),
    aes(label = paste0(NAME_1, "\n",
                       "BMI: ", round(mean_BMI, 2), "\n",
                       "n = ", n)),
    size = 2, fill = "white", alpha = 0.7
  ) +
  labs(
    title    = "Posterior mean BMI by region, Benin DHS 2017–18",
    subtitle = "Grey regions not represented in this project",
    caption  = "Source: Benin DHS 2017–18; Bayesian geo-additive model"
  ) +
  theme_void() +
  theme(
    plot.title    = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10, colour = "grey40"),
    plot.caption  = element_text(size = 8, colour = "grey50"),
    legend.position = "right"
  )