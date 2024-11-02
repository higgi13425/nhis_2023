# ----------------------------------------------------
#   
#   The National Health Interview Survey (NHIS) 2023;
# 
# Sample Adult data file
# 
# This program relabels the NHIS sample adult csv data file downloaded from the
# CDC website (https://www.cdc.gov/nchs/nhis/index.htm) 
# to provide you with nice, readable variable labels and value labels
# 
# The script below creates descriptive variable labels and and value labels. 
# you will want to convert some of your categorical variables to factors, or even ordered factors
# 
# BEFORE RUNNING THIS SCRIPT, PLEASE:
#   Create a project named nhis_2023 
#  Download the sample adult nhis 2023 csv file from the CDC website to your project folder
# `read_csv` this file to an object named 'data'
# 
# Running the script below will change the dataset named 'data' in memory. 
# You will have to save it to something like `data_labelled`


# labelling NHIS data imported from a downloaded csv
# use read_csv to read in the file
# then we will label the variables and the values
# with the labels provided in the codebook
# from the Stata Do file provided by the CDC for 2023
# take the do file, download it to a text file
# Skip over the locations section at the beginning 
# read_csv will take care of the reading.
# Starting with variable labels
# then paste chunks of code into Claude LLM
# (you get a limited number of lines per day)
# *and a limited number of lines per prompt*
# with the prompt
# "rewrite this Stata code in R using the labelled package:"
# to get the following code:
# then you will have to define value labels (part 2)
# then you will have to assign value labels (part 3)
# then write the result to 'data_labelled'
# ----------------------------------------------------

# NHIS 2023 Sample Adult data file
# Setup
library(tidyverse)
library(labelled)
library(janitor)

# Read in data
data <- read_csv('adult23.csv', guess_max = 29522) |> 
  clean_names()




# start with the variable labels



# Identification labels
var_label(data$rectype) <- "Record type"
var_label(data$srvy_yr) <- "Year of the National Health Interview Survey"
var_label(data$hhx) <- "Randomly assigned household number unique to a household"
var_label(data$wtfa_a) <- "Weight - Final Annual"

# Urban-Rural Classification labels
var_label(data$urbrrl) <- "2013 NCHS Urban-Rural Classification Scheme for Counties"
var_label(data$region) <- "Household region"
var_label(data$pstrat) <- "Pseudo-stratum for public-use file variance estimation"
var_label(data$ppsu) <- "Pseudo-PSU for public-use file variance estimation"

# Flag labels
var_label(data$hhrespsa_flg) <- "Sample Adult is the household respondent or the proxy who lives in the household"

# General information labels
var_label(data$intv_qrt) <- "Interview quarter"
var_label(data$intv_mon) <- "Interview month"
var_label(data$hhstat_a) <- "Indicates person is the Sample Adult"
var_label(data$astatnew) <- "Recoded ASTAT"
var_label(data$avail_a) <- "Sample Adult Available"
var_label(data$proxy_a) <- "Proxy Available"
var_label(data$proxyrel_a) <- "Proxy's Relationship to Sample Adult"

# Household composition labels
var_label(data$sex_a) <- "Sex of Sample Adult"
var_label(data$agep_a) <- "Age of SA (top coded)"
var_label(data$age65) <- "Age 65 or older"
var_label(data$hisp_a) <- "Recode: Hispanic ethnicity of SA"
var_label(data$hispallp_a) <- "Single and multiple race groups with Hispanic origin"
var_label(data$raceallp_a) <- "Single and multiple race groups"
var_label(data$hisdetp_a) <- "Hispanic origin detail"
var_label(data$afnow) <- "Full-time active duty screener"
var_label(data$educp_a) <- "Educational level of sample adult"
var_label(data$pcnt18uptc) <- "Top-coded count of persons 18 or older in the household"
var_label(data$pcntlt18tc) <- "Top-coded count of persons under 18 in the household"

# Family composition labels
var_label(data$pcntadlt_a) <- "Number of adults in Sample Adult family, top-coded 3+"
var_label(data$pcntkids_a) <- "Number of children in Sample Adult family, top-coded 3+"
var_label(data$over65flg_a) <- "Indicator for at least 1 person aged 65+ in SA family"
var_label(data$mltfamflg_a) <- "Indicator for multifamily households"
var_label(data$maxeducp_a) <- "Highest level of education of all the adults in the SA's family"

# Health status labels
var_label(data$phstat_a) <- "General health status"



# Life satisfaction labels
var_label(data$lsatis4_a) <- "Life satisfaction/dissatisfaction"

# Hypertension labels
var_label(data$hypev_a) <- "Ever been told you had hypertension"
var_label(data$hypdif_a) <- "Told had hypertension on two or more visits"
var_label(data$hyp12m_a) <- "Hypertension, past 12 months"
var_label(data$hypmed_a) <- "Now taking high blood pressure medication"

# Cholesterol labels
var_label(data$chlev_a) <- "Ever told you had high cholesterol"
var_label(data$chl12m_a) <- "High cholesterol, past 12 months"
var_label(data$chlmed_a) <- "Now taking cholesterol medication"

# Cardiovascular labels
var_label(data$chdev_a) <- "Ever been told you had coronary heart disease"
var_label(data$angev_a) <- "Ever been told you had angina"
var_label(data$miev_a) <- "Ever been told you had a heart attack"
var_label(data$strev_a) <- "Ever been told you had a stroke"

# Aspirin labels
var_label(data$aspmedev_a) <- "Told to take low-dose aspirin"
var_label(data$aspmednown_a) <- "Now following aspirin advice"
var_label(data$aspmedstp_a) <- "Advise to stop taking aspirin"
var_label(data$asponown_a) <- "Taking low dose-aspirin on own"

# Asthma labels
var_label(data$asev_a) <- "Ever had asthma"
var_label(data$astill_a) <- "Still have asthma"
var_label(data$asat12m_a) <- "Asthma episode"
var_label(data$aser12m_a) <- "Asthma ER visit"

# Cancer labels
var_label(data$canev_a) <- "Ever been told you had cancer"
var_label(data$bladdcan_a) <- "Bladder cancer mentioned"
var_label(data$bladdagetc_a) <- "Age when first told had bladder cancer"
var_label(data$bloodcan_a) <- "Blood cancer mentioned"
var_label(data$bloodagetc_a) <- "Age when first told had blood cancer"
var_label(data$bonecan_a) <- "Bone cancer mentioned"
var_label(data$boneagetc_a) <- "Age when first told had bone cancer"
var_label(data$braincan_a) <- "Brain cancer mentioned"
var_label(data$brainagetc_a) <- "Age when first told had brain cancer"
var_label(data$breascan_a) <- "Breast cancer mentioned"
var_label(data$breasagetc_a) <- "Age when first told had breast cancer"
var_label(data$cervican_a) <- "Cervical cancer mentioned"
var_label(data$cerviagetc_a) <- "Age when first told had cervical cancer"
var_label(data$coloncan_a) <- "Colon cancer mentioned"
var_label(data$colonagetc_a) <- "Age when first told had colon cancer"
var_label(data$esophcan_a) <- "Esophageal cancer mentioned"
var_label(data$esophagetc_a) <- "Age when first told had esophageal cancer"
var_label(data$gallbcan_a) <- "Gallbladder cancer mentioned"
var_label(data$gallbagetc_a) <- "Age when first told had gallbladder cancer"
var_label(data$laryncan_a) <- "Larynx-trachea cancer mentioned"
var_label(data$larynagetc_a) <- "Age when first told had larynx-trachea cancer"
var_label(data$leukecan_a) <- "Leukemia mentioned"
var_label(data$leukeagetc_a) <- "Age when first told had leukemia"
var_label(data$livercan_a) <- "Liver cancer mentioned"
var_label(data$liveragetc_a) <- "Age when first told had liver cancer"
var_label(data$lungcan_a) <- "Lung cancer mentioned"
var_label(data$lungagetc_a) <- "Age when first told had lung cancer"
var_label(data$lymphcan_a) <- "Lymphoma cancer mentioned"
var_label(data$lymphagetc_a) <- "Age when first told had lymphoma"
var_label(data$melancan_a) <- "Melanoma cancer mentioned"
var_label(data$melanagetc_a) <- "Age when first told had melanoma cancer"
var_label(data$mouthcan_a) <- "Mouth, tongue or lip cancer mentioned"
var_label(data$mouthagetc_a) <- "Age when first told had mouth, tongue or lip cancer"
var_label(data$ovarycan_a) <- "Ovarian cancer mentioned"
var_label(data$ovaryagetc_a) <- "Age when first told had ovarian cancer"
var_label(data$pancrcan_a) <- "Pancreatic cancer mentioned"
var_label(data$pancragetc_a) <- "Age when first told had pancreatic cancer"
var_label(data$prostcan_a) <- "Prostate cancer mentioned"
var_label(data$prostagetc_a) <- "Age when first told had prostate cancer"
var_label(data$rectucan_a) <- "Rectal cancer mentioned"
var_label(data$rectuagetc_a) <- "Age when first told had rectal cancer"
var_label(data$sknmcan_a) <- "Skin melanoma cancer mentioned"
var_label(data$sknmagetc_a) <- "Age when first told had skin melanoma cancer"
var_label(data$sknnmcan_a) <- "Skin non-melanoma cancer mentioned"
var_label(data$sknnmagetc_a) <- "Age when first told skin non-melanoma cancer"
var_label(data$skndkcan_a) <- "Skin cancer (don't know what kind) mentioned"
var_label(data$skndkagetc_a) <- "Age when first told had skin cancer (don't know what kind)"
var_label(data$stomacan_a) <- "Stomach cancer mentioned"
var_label(data$stomaagetc_a) <- "Age when first told had stomach cancer"
var_label(data$throacan_a) <- "Throat - pharynx cancer mentioned"
var_label(data$throaagetc_a) <- "Age when first told had throat-pharynx cancer"
var_label(data$thyrocan_a) <- "Thyroid cancer mentioned"
var_label(data$thyroagetc_a) <- "Age when first told had thyroid cancer"
var_label(data$uterucan_a) <- "Uterine cancer mentioned"
var_label(data$uteruagetc_a) <- "Age when first told had uterine cancer"
var_label(data$hdnckcan_a) <- "Head and neck cancers mentioned"
var_label(data$hdnckagetc_a) <- "Age when first told had head or neck cancer"
var_label(data$colrccan_a) <- "Colorectal cancer mentioned"
var_label(data$colrcagetc_a) <- "Age when first told had colon or rectal cancer"
var_label(data$othercanp_a) <- "Other cancer mentioned"
var_label(data$otheragetc_a) <- "Age when first told had other cancer"
var_label(data$numcan_a) <- "Number of reported types of cancers"




# Diabetes labels
var_label(data$predib_a) <- "Ever had prediabetes"
var_label(data$gesdib_a) <- "Gestational diabetes"
var_label(data$dibev_a) <- "Ever had diabetes"
var_label(data$dibagetc_a) <- "Age first diagnosed w/diabetes"
var_label(data$difyrstc1_a) <- "Years since first diagnosed w/diabetes"
var_label(data$dibpill_a) <- "Taking diabetic pills"
var_label(data$dibins_a) <- "Taking insulin"
var_label(data$dibinstime_a) <- "Time from diabetes to insulin"
var_label(data$dibinsstop_a) <- "Ever stop using insulin"
var_label(data$dibinsstyr_a) <- "Only stop insulin in first year"
var_label(data$dibtype_a) <- "Diabetes type"

# Chronic conditions labels
var_label(data$copdev_a) <- "Ever been told you had COPD, emphysema, or chronic bronchitis?"
var_label(data$arthev_a) <- "Ever had arthritis"
var_label(data$demenev_a) <- "Ever had dementia"
var_label(data$anxev_a) <- "Ever had anxiety disorder"
var_label(data$depev_a) <- "Ever had depression"

# Specific conditions labels
var_label(data$hepev_a) <- "Ever had hepatitis"
var_label(data$crohnsev_a) <- "Ever had Crohn's disease"
var_label(data$ulccolev_a) <- "Ever had ulcerative colitis"
var_label(data$psorev_a) <- "Ever had psoriasis"

# Chronic Fatigue Syndrome labels
var_label(data$cfsev_a) <- "Ever had Chronic Fatigue Syndrome"
var_label(data$cfsnow_a) <- "Still have Chronic Fatigue Syndrome"

# Immune system labels
var_label(data$medrxtrt_a) <- "Weakened immune system due to prescriptions"
var_label(data$hlthcond_a) <- "Weakened immune system due to health condition"

# Epilepsy labels
var_label(data$epiev_a) <- "Ever had epilepsy"
var_label(data$epimed_a) <- "Take medication for epilepsy"
var_label(data$epinumsezp_a) <- "How many seizures past 12 months"
var_label(data$epidr_a) <- "See a doctor for epilepsy"

# Vision conditions labels
var_label(data$vimdrev_a) <- "Ever had diabetic retinopathy"
var_label(data$vimlsdr_a) <- "Lost vision due to diabetic retinopathy"
var_label(data$vimglev_a) <- "Ever had glaucoma"
var_label(data$vimlsgl_a) <- "Lost vision due to glaucoma"
var_label(data$vimmdev_a) <- "Ever had macular degeneration"
var_label(data$vimlsmd_a) <- "Lost vision due to macular degeneration"
var_label(data$vimcsurg_a) <- "Ever had cataract surgery"
var_label(data$vimcaev_a) <- "Ever had cataracts"
var_label(data$vimlsca_a) <- "Lost vision due to cataracts"

# BMI labels
var_label(data$pregnow_a) <- "Pregnant now"
var_label(data$heighttc_a) <- "Height without shoes (inches), Public Use"
var_label(data$weightlbtc_a) <- "Weight without shoes (pounds), Public Use"
var_label(data$bmicat_a) <- "Categorical Body Mass Index, Public Use"

# Vision labels
var_label(data$wearglss_a) <- "Wear glasses/contact lenses"
var_label(data$visiondf_a) <- "Difficulty seeing"

# Hearing labels
var_label(data$hearaid_a) <- "Use hearing aid"
var_label(data$hearaidfr_a) <- "How often use hearing aid"
var_label(data$hearingdf_a) <- "Difficulty hearing"




# Mobility labels
var_label(data$diff_a) <- "Difficulty walking or climbing steps"
var_label(data$equip_a) <- "Use equipment to get around"
var_label(data$wlk100_a) <- "Difficulty walking 100 yards"
var_label(data$wlk13m_a) <- "Difficulty walking 1/3 mile"
var_label(data$steps_a) <- "Difficulty walking steps"
var_label(data$canewlkr_a) <- "Use cane or walker"
var_label(data$wchair_a) <- "Use wheelchair or scooter"
var_label(data$perasst_a) <- "Use someone's assistance"
var_label(data$noeqwlk100_a) <- "Difficulty walking 100 yards without aid"
var_label(data$noeqwlk13m_a) <- "Difficulty walking 1/3 mile without aid"
var_label(data$noeqsteps_a) <- "Difficulty walking steps without equipment"
var_label(data$eqwlk100_a) <- "Difficulty walking 100 yards with aid"
var_label(data$eqwlk13m_a) <- "Difficulty walking 1/3 mile with aid"
var_label(data$eqsteps_a) <- "Difficulty walking steps with equipment"

# Communication label
var_label(data$comdiff_a) <- "Difficulty communicating"

# Cognitive function labels
var_label(data$cogmemdff_a) <- "Difficulty remembering/concentrating"
var_label(data$cogtypedff_a) <- "Remembering/concentrating or both"
var_label(data$cogfrqdff_a) <- "Difficulty remembering-how often"
var_label(data$cogamtdff_a) <- "Difficulty remembering-how much"

# Upper body function labels
var_label(data$uppslfcr_a) <- "Difficulty with self care"
var_label(data$uppraise_a) <- "Difficulty raising soda bottle"
var_label(data$uppobjct_a) <- "Difficulty using hands and fingers"
var_label(data$disab3_a) <- "The Washington Group Short Set Composite Disability Indicator"

# Social function labels
var_label(data$socerrnds_a) <- "Difficulty doing errands alone"
var_label(data$socsclpar_a) <- "Difficulty participating in social activities"
var_label(data$socwrklim_a) <- "Work limited due to health problem"

# Age of onset label
var_label(data$devdonset_a) <- "Difficulty doing activities before age 22"

#INS variable labels
