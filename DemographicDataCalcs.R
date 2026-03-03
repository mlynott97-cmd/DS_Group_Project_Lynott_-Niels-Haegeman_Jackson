library(readxl)
library(tidyverse)

district_mapping <- read_excel("C:/Users/sajac/OneDrive/Documents/Columbia/DS FOR POAN/2020_equiv.xlsx")

district_mapping <- district_mapping %>% 
  distinct(TIGER20VTD, COUNTY, .keep_all = TRUE)

district_mapping %>% 
  count(TIGER20VTD, COUNTY) %>% 
  filter(n > 1)


# unique(data2$`MCD/AD`)
# max(data2$TIGER20VTD)

county_demographics <- read_excel("C:/Users/sajac/OneDrive/Documents/Columbia/DS FOR POAN/Population Data Files/PL_ADJUSTED_COUNTY.xlsx")

colnames(county_demographics) <- county_demographics[2,]
county_demographics <- county_demographics[-c(1,2),]

county_demographics <- county_demographics %>% 
  filter(CNTY_NAME %in% c("Bronx", "Kings", "New York", "Queens", "Richmond"))


demographics <- read_excel("C:/Users/sajac/OneDrive/Documents/Columbia/DS FOR POAN/Population Data Files/PL_ADJUSTED_VTD.xlsx", 
                           col_names = FALSE)

colnames(demographics) <- demographics[3,]
demographics <- demographics[-c(1,2,3),]


demographics <- demographics %>% 
  filter(CNTY_NAME %in% c("Bronx", "Kings", "New York", "Queens", "Richmond")) %>% 
  mutate(VTD = as.numeric(VTD)) 


  
AD_demographics <- inner_join(demographics, district_mapping, 
                              by = c("VTD" = "TIGER20VTD", "COUNTY"))


AD_grouped <- AD_demographics %>% 
  mutate(TOTAL_ADJ = as.numeric(TOTAL_ADJ),
         WHITE_ADJ = as.numeric(WHITE_ADJ)) %>% 
  group_by(`MCD/AD`) %>%
  summarize(total = sum(TOTAL_ADJ), 
            white = sum(WHITE_ADJ))


staten_island <- AD_demographics %>% 
  filter(CNTY_NAME == "Richmond") %>% 
  mutate(TOTAL_ADJ = as.numeric(TOTAL_ADJ),
         WHITE_ADJ = as.numeric(WHITE_ADJ)) %>% 
  group_by(`MCD/AD`) %>%
  summarize(total = sum(TOTAL_ADJ), 
            white = sum(WHITE_ADJ))

new_york <- AD_demographics %>% 
  filter(CNTY_NAME == "New York") %>% 
  mutate(TOTAL_ADJ = as.numeric(TOTAL_ADJ),
         WHITE_ADJ = as.numeric(WHITE_ADJ)) %>% 
  group_by(`MCD/AD`)%>%
  summarize(total = sum(TOTAL_ADJ), 
            white = sum(WHITE_ADJ))

queens <- AD_demographics %>% 
  filter(CNTY_NAME == "Queens") %>% 
  mutate(TOTAL_ADJ = as.numeric(TOTAL_ADJ),
         WHITE_ADJ = as.numeric(WHITE_ADJ)) %>% 
  group_by(`MCD/AD`)%>%
  summarize(total = sum(TOTAL_ADJ), 
            white = sum(WHITE_ADJ))

brooklyn <- AD_demographics %>% 
  filter(CNTY_NAME == "Kings") %>% 
  mutate(TOTAL_ADJ = as.numeric(TOTAL_ADJ),
         WHITE_ADJ = as.numeric(WHITE_ADJ)) %>% 
  group_by(`MCD/AD`)%>%
  summarize(total = sum(TOTAL_ADJ), 
            white = sum(WHITE_ADJ))

bronx <- AD_demographics %>% 
  filter(CNTY_NAME == "Bronx") %>% 
  mutate(TOTAL_ADJ = as.numeric(TOTAL_ADJ),
         WHITE_ADJ = as.numeric(WHITE_ADJ)) %>% 
  group_by(`MCD/AD`)%>%
  summarize(total = sum(TOTAL_ADJ), 
            white = sum(WHITE_ADJ))


