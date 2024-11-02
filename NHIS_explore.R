library(tidyverse)

adult21 <- read.csv("C:/Users/phiggins/RCode/adult21.csv", header = T)

names(adult21)

length(unique(adult21$HHX))

# explanations of varnames in codebook
# found as pdf in C:/Users/phiggins/RCode/
adult21$WTFA_A[1:10] #weights
adult21$URBRRL[1:10] # urban-rural (1-4)
adult21$REGION[1:10] # region (1-4)
adult21$PSTRAT[1:10] # psuedostratification
adult21$PPSU[1:10] # pseudo-psu for variance estimation
adult21$SEX_A[1:10] # sex
adult21$EDUCP_A[1:10] # educ level (0-10)
adult21$HISP_A[1:10] # hispanic (1-9)
adult21$RACEALLP_A[1:10] # race (1-9)
adult21$PHSTAT_A[1:10] # overall health (1-9)
adult21$LSATIS4R_A %>% head() #life satisfaction (1-4+)
adult21$CANEV_A %>% head(20) # cancer
adult21$COLONCAN_A %>% head(15) # colon cancer
adult21$COLRCCAN_A %>% head(10) #colorectal cancer
adult21$LIVERCAN_A %>% head(15) # liver cancer
adult21$ARTHEV_A %>% head(15) # arthritis
adult21$ANXEV_A %>% head(15) #anxiety
adult21$DEPEV_A %>% head(15) # depression p.138/629
adult21$HEPEV_A %>% head(15) # hepatitis
adult21$LIVEREV_A %>% head(15) # cirrhosis
adult21$MEDRXTRT_A %>% head(15) # immune suppression from meds
adult21$HLTHCOND_A %>% head(15) # immune suppression from health condition
adult21$HEIGHTTC_A %>% head(15) #height wihtout shoes in inches
adult21$WEIGHTLBTC_A %>% head(15) # weight in lb
adult21$BMICAT_A %>% head(15) # BMI in categories 1-4 p161/629



ggplot(adult21, aes(x=PHSTAT_A)) + xlim(0.5,5.5) + geom_histogram() +
  coord_flip()

adult21 %>% filter(LSATIS4R_A <5) %>% 
  select(LSATIS4R_A) %>% summary()     
       
# creating a survey object

library(tidyverse)
library(labelled)
library(janitor)
library(survey)
library(srvyr)
library(gtsummary)
library(broom)

# Read in data
adult23 <- read_csv('adult23.csv', guess_max = 5000) |>   clean_names()


svy_adult23 <- adult23 |> 
  as_survey_design(
          ids = ppsu, # cluster ids, rather than hhx = household
          strata = pstrat, 
          weights = wtfa_a, 
          nest = TRUE) 


class(svy_adult23)

svy_adult23

srvyr::svy_adult23 %>%
  summarize(
    age_mean = survey_mean(agep_a),
    age_med = survey_median(agep_a)
  )

# repeat with survey package
survey::svymean(~agep_a, svy_adult23)

svy_adult23 |> 
  group_by(crohnsev_a) |> 
  summarize(
    age_mean = survey_mean(agep_a)
  )

print("1=Yes, 2=No, 7=Refused, 9=Don't know")

# Producing a Weighted Table
svy_adult23 |> 
  filter(crohnsev_a %in% c(1, 2)) |>
  filter(paiamnt_a %in% c(1, 2, 3)) |>
  filter(paifrq3m_a <5) |>
  filter(paiapg3m_a <5) |>
  select(crohnsev_a, paifrq3m_a, paiamnt_a, paiapg3m_a) |>
  tbl_svysummary(
    by = crohnsev_a,
    include = c(paifrq3m_a, paiamnt_a, paiapg3m_a),
    label = list(
      paifrq3m_a = "Frequency of Pain",
      paiamnt_a = "Amount of Pain",
      paiapg3m_a = "Abdominal & Pelvic Pain"
    )
  ) |> 
  add_p() |> 
  modify_header(label = "**Pain Symptoms**", 
                stat_1 = "**Crohn's Disease**",
                stat_2 = "**No Crohn's Disease**",
                p.value= "**p**")

# Basic Modeling
# note higher phstat is WORSE general health
model <- svy_adult23 |> 
  filter(phstat_a %in% c(1, 2, 3, 4, 5)) |>
  filter(ulccolev_a %in% c(1, 2)) |>
  mutate(ulc_col = 2 - ulccolev_a) |> #now 0=no uc, 1 = uc
  filter(medrxtrt_a %in% c(1, 2)) |>
  mutate(immsupp = 2-medrxtrt_a) |> # now 0=no immune suppression, 1=immune suppression
  filter(paiapg3m_a <5) |>
  svyglm(
    formula = phstat_a ~ paiapg3m_a + ulc_col + immsupp) 

# baseline (intercept) general health between 2 very good and 3 good
model |>
  broom::tidy() 
# uc plus abdpain plus immuunosupp pushes from 2.3 to 3.6 (between good and fair)

model |> 
  tbl_regression(
    label = list(
      paiapg3m_a = "Abdominal & Pelvic Pain",
      ulc_col = "Ulcerative Colitis",
      immsupp = "Immune Suppression"
    ),
    pvalue_fun = label_style_pvalue(digits = 3),
    exponentiate = TRUE
  ) |> 
  modify_header(label = "**Health State (higher # is worse)**",
                estimate = "**Adjusted OR**",)

