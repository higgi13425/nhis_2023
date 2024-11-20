# Introduction------------------------------------------
#   
#   The National Health Interview Survey (NHIS) 2023 
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
# then paste chunks of Stata code from the do file
# from the label variable section
# then use find/replace in the code window to
# 1. replace "variable label" with "var_label(data$"
# 2. then replace '_a    "' with ') <- "' - vary the spaces
# 3. then replace "))" with " "
# 
# then paste in the value label creation code
# 1. replace "label define" with "val_labels("
# 2. replace "X$" (select regex box in find bar) with "X <- c("
# 3. replace numbers - (regex on) - "1\s{3,}" with "1_
# 4. repeat for all numbers 0 thorugh 9
# 5. put commas and end of each line (regex on) - replace '"$' with '",' (but only for this section, not all)
# 6. replace ",$^))" with "))" and end of each value label
# 
# then you will have to assign value labels (part 3)
# then write the result to 'data_labelled'
# Start Code----------------------------------------

# NHIS 2023 Sample Adult data file
# Setup ---------------------------------------------
library(tidyverse)
library(labelled)
library(janitor)

# Function ------------------------------------------
# build function to parse string in combo factor values like 01_Yes
parse_string <- function(string) {
  str_extract(string = string, 
    pattern = "(?<=\\d\\_).+")
}
  


# Read in data -------------------------------------
data <- read_csv('adult23.csv', guess_max = 29522) |> 
  clean_names()




# Start variable labels --------------------------------



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
var_label(data$pcnt18uptc) <- "Top-coded count of persons 18_or older in the household"
var_label(data$pcntlt18tc) <- "Top-coded count of persons under 18_in the household"

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


# INS  VARIABLE LABELS 
var_label(data$notcov_a) <- "Coverage status as used in Health United States"
var_label(data$cover_a) <- "Health insurance hierarchy under 65"
var_label(data$cover65_a) <- "Health Insurance hierarchy 65+"
var_label(data$sincovde_a) <- "Single service plan - dental"
var_label(data$sincovvs_a) <- "Single service plan - vision"
var_label(data$sincovrx_a) <- "Single service plan - prescription" 
var_label(data$medicare_a) <- "Medicare recode" 
var_label(data$mcpart_a) <- "Type of Medicare coverage" 
var_label(data$mcchoice_a) <- "Enrolled in Medicare Advantage Plan" 
var_label(data$mchmo_a) <- "Medicare HMO" 
var_label(data$mcadvr_a) <- "Medicare Advantage Plan" 
var_label(data$mcdncov_a) <- "Medicare Advantage Plan pays for any dental care costs" 
var_label(data$mcvscov_a) <- "Medicare Advantage Plan pays for any vision care costs" 
var_label(data$mcpartd_a) <- "Medicare Part D" 
var_label(data$medicaid_a) <- "Medicaid recode" 
var_label(data$maxchng_a) <- "Medicaid through Marketplace" 
var_label(data$maprem_a) <- "Medicaid premium" 
var_label(data$madeduc_a) <- "Medicaid deductible" 
var_label(data$mahdhp_a) <- "Medicaid HDHP" 
var_label(data$maflg_a) <- "Medicaid reassignment flag" 
var_label(data$private_a) <- "Private health insurance recode" 
var_label(data$exchange_a) <- "Plan through Health Insurance Exchange, NCHS algorithm" 
var_label(data$polhld1_a) <- "Policyholder for private plan 1" 
var_label(data$prplcov1_a) <- "Plan coverage for others - plan 1" 
var_label(data$prpolh1_a) <- "Relation to policyholder - plan 1" 
var_label(data$prplcov1_c_a) <- "Response to PRPLCOV1_C or PRPLCOV2_C  from child - plan 1" 
var_label(data$plnwrkr1_a) <- "How plan was originally obtained - plan 1" 
var_label(data$plnexchg1_a) <- "Health plan obtained through Marketplace - plan 1" 
var_label(data$pln1pay1_a) <- "Paid for by self or family - plan 1" 
var_label(data$pln1pay2_a) <- "Paid for by employer or union - plan 1" 
var_label(data$pln1pay3_a) <- "Paid for by someone outside the household - plan 1" 
var_label(data$pln1pay4_a) <- "Paid for by Medicare - plan 1" 
var_label(data$pln1pay5_a) <- "Paid for by Medicaid - plan 1" 
var_label(data$pln1pay6_a) <- "Paid for by other government program - plan 1" 
var_label(data$hicostr1_a) <- "Out-of-pocket premium cost - plan 1" 
var_label(data$prdeduc1_a) <- "Deductible - plan 1" 
var_label(data$prhdhp1_a) <- "High deductible health plan - plan 1" 
var_label(data$hsahra1_a) <- "Health Savings Accounts / Health Reimbursement Accounts - plan 1" 
var_label(data$prrxcov1_a) <- "Plan pays for prescription drug - plan 1" 
var_label(data$prdncov1_a) <- "Plan pays for dental care - plan 1" 
var_label(data$prvscov1_a) <- "Plan pays for vision care - plan 1" 
var_label(data$exchpr1_a) <- "Exchange company coding NCHS - plan 1" 
var_label(data$prflg_a) <- "Private reassignment flag" 
var_label(data$pxchng1_a) <- "Marketplace or state exchange, reassigned from public to private" 
var_label(data$prprem1_a) <- "Premium on plan reassigned from public to private" 
var_label(data$plexchpr1_a) <- "Exchange company coding, NCHS, reassigned from public to private" 
var_label(data$polhld2_a) <- "Policyholder for private plan 2" 
var_label(data$prplcov2_a) <- "Plan coverage for others - plan 2" 
var_label(data$prpolh2_a) <- "Relation to policyholder - plan 2" 
var_label(data$prplcov2_c_a) <- "Response to PRPLCOV1_C or PRPLCOV2_C from child - plan 2" 
var_label(data$plnwrkr2_a) <- "How plan was originally obtained - plan 2" 
var_label(data$plnexchg2_a) <- "Health plan obtained through Marketplace - plan 2" 
var_label(data$pln2pay1_a) <- "Paid for by self or family - plan 2" 
var_label(data$pln2pay2_a) <- "Paid for by employer or union - plan 2" 
var_label(data$pln2pay3_a) <- "Paid for by someone outside the household - plan 2" 
var_label(data$pln2pay4_a) <- "Paid for by Medicare - plan 2" 
var_label(data$pln2pay5_a) <- "Paid for by Medicaid - plan 2" 
var_label(data$pln2pay6_a) <- "Paid for by other government program - plan 2" 
var_label(data$hicostr2_a) <- "Out-of-pocket premium cost - plan 2" 
var_label(data$prdeduc2_a) <- "Deductible - plan 2" 
var_label(data$prhdhp2_a) <- "High deductible health plan - plan 2" 
var_label(data$hsahra2_a) <- "Health Savings Accounts / Health Reimbursement Accounts - plan 2" 
var_label(data$prrxcov2_a) <- "Plan pays for prescription drug - plan 2" 
var_label(data$prdncov2_a) <- "Plan pays for dental care - plan 2" 
var_label(data$prvscov2_a) <- "Plan pays for vision care - plan 2" 
var_label(data$exchpr2_a) <- "Exchange company coding NCHS - plan 2" 
var_label(data$chip_a) <- "Children's Health Insurance Program (CHIP) recode" 
var_label(data$chxchng_a) <- "CHIP through Marketplace" 
var_label(data$chprem_a) <- "Pay CHIP premium" 
var_label(data$chdeduc_a) <- "CHIP deductible" 
var_label(data$chhdhp_a) <- "CHIP HDHP" 
var_label(data$chflg_a) <- "CHIP reassignment flag" 
var_label(data$othpub_a) <- "State-sponsored health plan recode" 
var_label(data$opxchng_a) <- "State-sponsored plan through Marketplace" 
var_label(data$opprem_a) <- "Pay premium for state-sponsored plan" 
var_label(data$opdeduc_a) <- "State-sponsored plan deductible" 
var_label(data$ophdhp_a) <- "State-sponsored plan HDHP" 
var_label(data$plexchop_a) <- "Exchange company coding, NCHS (state-sponsored plan)" 
var_label(data$opflg_a) <- "State-sponsored reassignment flag" 
var_label(data$othgov_a) <- "Other government program recode" 
var_label(data$ogxchng_a) <- "Other government program through Marketplace" 
var_label(data$ogprem_a) <- "Pay premium for other government program" 
var_label(data$ogdeduc_a) <- "Other government program deductible" 
var_label(data$oghdhp_a) <- "Other government program HDHP" 
var_label(data$plexchog_a) <- "Exchange company coding, NCHS (other government program)" 
var_label(data$ogflg_a) <- "Other government reassignment flag" 
var_label(data$military_a) <- "Military health care coverage recode" 
var_label(data$milspc1r_a) <- "Types of military health care - VA health care" 
var_label(data$milspc2_a) <- "Types of military health care - TRICARE" 
var_label(data$milspc3_a) <- "Types of military health care - CHAMP-VA" 
var_label(data$ihs_a) <- "Indian Health Service recode" 
var_label(data$hilast_a) <- "How long since last health coverage" 
var_label(data$hilastmy_a) <- "Number of months without coverage" 
var_label(data$histopjob_a) <- "Lost job or changed employers" 
var_label(data$histopmiss_a) <- "Missed deadline" 
var_label(data$histopage_a) <- "Ineligible because of age/left school" 
var_label(data$histopcost_a) <- "Cost increase" 
var_label(data$histopelig_a) <- "Not eligible for Medicaid, CHIP, or other public coverage" 
var_label(data$rsnhicost_a) <- "Not affordable" 
var_label(data$rsnhiwant_a) <- "Do not need or want coverage" 
var_label(data$rsnhielig_a) <- "Not eligible for coverage" 
var_label(data$rsnhiconf_a) <- "Signing up too difficult or confusing" 
var_label(data$rsnhimeet_a) <- "Cannot find a plan that meets the needs" 
var_label(data$rsnhiwait_a) <- "Coverage has not started yet" 
var_label(data$rsnhioth_a) <- "Another reason" 
var_label(data$rsnhijob_a) <- "Lost job" 
var_label(data$rsnhimiss_a) <- "Missed deadline" 
var_label(data$hinotyr_a) <- "No health coverage during the past 12 months" 
var_label(data$hinotmyr_a) <- "Months without coverage in the past 12 months" 
var_label(data$milspc1_a) <- "Types of military health care - VA health care original response" 
var_label(data$hicov_a) <- "Have health insurance" 
var_label(data$hikind01_a) <- "Kind(s) of health insurance - private health insurance" 
var_label(data$hikind02_a) <- "Kind(s) of health insurance - Medicare" 
var_label(data$hikind03_a) <- "Kind(s) of health insurance - Medicare supplement (Medigap)" 
var_label(data$hikind04_a) <- "Kind(s) of health insurance - Medicaid" 
var_label(data$hikind05_a) <- "Kind(s) of health insurance - CHIP" 
var_label(data$hikind06_a) <- "Kind(s) of health insurance - military related health care" 
var_label(data$hikind07_a) <- "Kind(s) of health insurance - Indian Health Service" 
var_label(data$hikind08_a) <- "Kind(s) of health insurance - State-sponsored health plan" 
var_label(data$hikind09_a) <- "Kind(s) of health insurance - Other government program" 
var_label(data$hikind10_a) <- "Kind(s) of health insurance - No coverage of any type" 
var_label(data$mcareprb_a) <- "Medicare coverage probe" 
var_label(data$mcaidprb_a) <- "Medicaid coverage probe" 


# PAY  VARIABLE LABELS 
var_label(data$paybll12m_a) <- "Problems paying medical bills, past 12m" 
var_label(data$paynobllnw_a) <- "Unable to pay medical bills" 
var_label(data$payworry_a) <- "Get sick or have accident, worry about paying medical bills" 

# CVL  VARIABLE LABELS 
var_label(data$evercovd_a) <- "Ever had COVID-19" 
var_label(data$longcovd1_a) <- "Had COVID-19 symptoms for 3 or more months" 
var_label(data$sympnow1_a) <- "Currently has COVID-19 symptoms" 
var_label(data$lcvdact_a) <- "COVID-19 impacts activities" 

# DNC  VARIABLE LABELS 
var_label(data$denprev_a) <- "Time since last dental exam or cleaning" 
var_label(data$dendl12m_a) <- "Delayed dental care due to cost, past 12 months" 
var_label(data$denng12m_a) <- "Needed dental care but did not get it due to cost, past 12 months" 

# UTZ  VARIABLE LABELS 
var_label(data$lastdr_a) <- "Time since last saw doctor" 
var_label(data$wellness_a) <- "Was last visit a wellness visit" 
var_label(data$wellvis_a) <- "Time since last wellness visit" 
var_label(data$usualpl_a) <- "Have a usual place for care" 
var_label(data$usplkind_a) <- "Type of place for usual care" 
var_label(data$urgnt12mtc_a) <- "Number of times visited urgent care, past 12 months, top-coded" 
var_label(data$emerg12mtc_a) <- "Number of times visited hospital emergency room, past 12 months, top-coded" 
var_label(data$hospongt_a) <- "Hospitalized overnight, past 12 months" 
var_label(data$meddl12m_a) <- "Delayed medical care due to cost, past 12 months" 
var_label(data$medng12m_a) <- "Needed medical care but did not get it due to cost, past 12 months" 

# TLH  VARIABLE LABELS 
var_label(data$virapp12m_a) <- "Virtual medical appointment, past 12m" 

# HIT  VARIABLE LABELS 
var_label(data$accssint_a) <- "Internet access" 
var_label(data$accsshom_a) <- "Internet access at home" 
var_label(data$hitlook_a) <- "Used internet for health information" 
var_label(data$hitcomm_a) <- "Communicated with doctor's office" 
var_label(data$hittest_a) <- "Used internet for test results" 

# PMD  VARIABLE LABELS 
var_label(data$rx12m_a) <- "Took prescription medication, past 12 months" 
var_label(data$rxsk12m_a) <- "Skipped medication doses to save money, past 12m" 
var_label(data$rxls12m_a) <- "Took less medication to save money, past 12 months" 
var_label(data$rxdl12m_a) <- "Delayed filling prescription to save money, past 12 months" 
var_label(data$rxdg12m_a) <- "Needed prescription medication but did not get it due to cost, past 12 months" 

# PRV  VARIABLE LABELS 
var_label(data$bplast_a) <- "Last time blood pressure checked" 
var_label(data$chollast_a) <- "Last time cholesterol checked" 
var_label(data$diblast1_a) <- "Last time blood sugar test, if never told had diabetes" 
var_label(data$diba1clast_a) <- "Last time A1C test, if ever told had diabetes" 
var_label(data$diba1cnmt_a) <- "Number of A1C tests past 12 months - topcoded" 
var_label(data$colorectev_a) <- "Ever had a colonoscopy or sigmoidoscopy" 
var_label(data$colorectyp_a) <- "Had a colonoscopy or sigmoidoscopy or both" 
var_label(data$colwhen_a) <- "Most recent colonoscopy" 
var_label(data$colreason1_a) <- "Main reason for recent colonoscopy" 
var_label(data$colsigwhen_a) <- "Most recent colonoscopy or sigmoidoscopy" 
var_label(data$sigwhen_a) <- "Most recent sigmoidoscopy" 
var_label(data$ctcolev1_a) <- "Ever had a colonography or virtual colonoscopy" 
var_label(data$ctcolwhen1_a) <- "Most recent colonography or virtual colonoscopy" 
var_label(data$fithev1_a) <- "Ever had home blood stool test or FIT" 
var_label(data$fithwhen1_a) <- "Most recent home blood stool test or FIT" 
var_label(data$cologuard1_a) <- "Ever had Cologuard" 
var_label(data$fitcolg1_a) <- "Blood stool or FIT was part of Cologuard test" 
var_label(data$cguardwhe1_a) <- "Most recent Cologuard" 
var_label(data$colprob1_a) <- "Doctor recommended testing to look for problems in colon or rectum" 
var_label(data$coltest1_a) <- "Doctor recommended stool blood test, fecal occult blood, or FIT test" 
var_label(data$coltest2_a) <- "Doctor recommended Cologuard or FIT DNA test" 
var_label(data$coltest3_a) <- "Doctor recommended sigmoidoscopy" 
var_label(data$coltest4_a) <- "Doctor recommended colonoscopy" 
var_label(data$coltest5_a) <- "Doctor recommended CT colonography or virtual colonoscopy" 
var_label(data$coltest6_a) <- "Doctor recommended other test for colon cancer" 
var_label(data$psatest_a) <- "Ever had a PSA test" 
var_label(data$psawhen_a) <- "Most recent PSA test" 
var_label(data$psareason_a) <- "Main reason had a PSA test" 
var_label(data$psa5yr1_a) <- "Number of PSA tests-last 5 years" 
var_label(data$cervicev1_a) <- "Ever had cervical cancer screening test" 
var_label(data$cervicwhen_a) <- "Most recent cervical cancer test" 
var_label(data$hystev2_a) <- "Had hysterectomy" 
var_label(data$mamev_a) <- "Ever had mammogram" 
var_label(data$mamwhen_a) <- "Most recent mammogram" 
var_label(data$mamreason_a) <- "Main reason for mammogram" 
var_label(data$mamnot1_a) <- "Most important reason for no mammogram" 
var_label(data$mrihad_a) <- "Ever had breast MRI" 
var_label(data$mriwhen_a) <- "Most recent breast MRI" 
var_label(data$mrirea_a) <- "Main reason for breast MRI" 

# GCT  VARIABLE LABELS 
var_label(data$gtposs1_a) <- "Ever discussed genetic cancer risk test with doctor" 
var_label(data$gtgrisk_a) <- "Ever had a genetic test" 

# FHC  VARIABLE LABELS 
var_label(data$fhcanev_a) <- "Family ever had cancer" 
var_label(data$fhbcanev_a) <- "Family ever had breast cancer" 
var_label(data$fhbcannum_a) <- "Number of family members with breast cancer" 
var_label(data$fhbcan50_a) <- "Number of family members with breast cancer before age 50" 
var_label(data$fhovcanev_a) <- "Family ever had ovarian cancer" 
var_label(data$fhovcannum_a) <- "Number of family members with ovarian cancer" 
var_label(data$fhcanrisk_a) <- "Talked to doctor about family risk of cancer" 

# IMS  VARIABLE LABELS 
var_label(data$pregfluyr_a) <- "Was sample adult pregnant last flu season" 
var_label(data$livebirth_a) <- "Any live births" 
var_label(data$shtflu12m_a) <- "Flu vaccine, past 12 months" 
var_label(data$shtflum_a) <- "Month of last flu vaccine" 
var_label(data$shtfluy_a) <- "Year of last flu vaccine" 
var_label(data$flupreg_a) <- "Was flu shot before or during pregnancy" 
var_label(data$flupreg2_a) <- "Earlier pregnancy and flu vaccine" 
var_label(data$shtcvd191_a) <- "COVID-19 vaccination" 
var_label(data$shtcvd19nm1_a) <- "Number of COVID-19 vaccinations" 
var_label(data$cvdvac1m1_a) <- "Month of most recent COVID-19 vaccination" 
var_label(data$cvdvac1y1_a) <- "Year of most recent COVID-19 vaccination" 
var_label(data$shottype2_a) <- "Brand of first COVID-19 shot" 
var_label(data$shtpnuev_a) <- "Ever had pneumonia shot" 
var_label(data$shtpneunb_a) <- "Number of pneumonia shots" 
var_label(data$shtshingl1_a) <- "Ever had a shingles vaccination" 
var_label(data$shingyearp_a) <- "Year of most recent Shingles vaccine" 
var_label(data$shingwhen1_a) <- "Was last shingles shot before 2017" 
var_label(data$shingrix3_a) <- "Ever had Shingrix vaccination" 
var_label(data$shingrixn3_a) <- "How many Shingrix shots" 
var_label(data$shingrixfs1_a) <- "First or second Shingrix shot" 
var_label(data$tdappreg_a) <- "Have a Tdap booster shot" 
var_label(data$shthepb1_a) <- "Hepatitis B vaccine" 
var_label(data$livehep_a) <- "Live with someone with hepatitis" 
var_label(data$workhealth_a) <- "Currently provide medical care to patients" 
var_label(data$wrkhlthfc_a) <- "Currently volunteer or work in health care" 
var_label(data$travel_a) <- "Travel to other countries since 1995" 

# PTC  VARIABLE LABELS 
var_label(data$eyeex12m_a) <- "Had eye exam, past 12 months" 
var_label(data$thera12m_a) <- "Received physical/speech/rehabilitative/occupational therapy, past 12 months" 
var_label(data$homehc12m_a) <- "Received care at home, past 12 months" 

# ANX  VARIABLE LABELS 
var_label(data$anxfreq_a) <- "How often feel worried, nervous, or anxious" 
var_label(data$anxmed_a) <- "Take medication for worried/nervous/anxious feelings" 
var_label(data$anxlevel_a) <- "Level of feelings when last felt worried/nervous/anxious" 

# DEP  VARIABLE LABELS 
var_label(data$depfreq_a) <- "How often depressed" 
var_label(data$depmed_a) <- "Take medication for depression" 
var_label(data$deplevel_a) <- "Level of how depressed" 

# MHC  VARIABLE LABELS 
var_label(data$mhrx_a) <- "Took medication for other emotions/concentration/behavior/mental health, past 12" 
var_label(data$mhthrpy_a) <- "Received counseling/therapy from mental health professional, past 12 months" 
var_label(data$mhtpynow_a) <- "Currently receiving counseling/therapy from mental health professional" 
var_label(data$mhthdly_a) <- "Delayed counseling, therapy due to cost, past 12 months" 
var_label(data$mhthnd_a) <- "Needed counseling, therapy but did not get it due to cost, past 12 months" 

# MHA  VARIABLE LABELS 
var_label(data$phq41_a) <- "How often little interest in things, past 2 weeks" 
var_label(data$phq42_a) <- "How often feeling down, past 2 weeks" 
var_label(data$phq2screen_a) <- "PHQ-2 screener result" 
var_label(data$phq43_a) <- "How often felt nervous/anxious/on edge, past 2 weeks" 
var_label(data$phq44_a) <- "How often can't stop/control worrying, past 2 weeks" 
var_label(data$gad2screen_a) <- "GAD-2 screener result" 

# EDS  VARIABLE LABELS 
var_label(data$discrim1_a) <- "Treated with less courtesy or respect" 
var_label(data$discrim2_a) <- "Receive poor service at restaurant or store" 
var_label(data$discrim3_a) <- "Treated as not smart" 
var_label(data$discrim4_a) <- "People act afraid of you" 
var_label(data$discrim5_a) <- "You are threatened or harassed" 

# HVS  VARIABLE LABELS 
var_label(data$vigil1_a) <- "Prepare for possible insults before leaving home" 
var_label(data$vigil2_a) <- "Careful about your appearance in order to get good service or avoid harassment" 
var_label(data$vigil3_a) <- "Watch what you say and how you say it" 
var_label(data$vigil4_a) <- "Avoid certain situations and places" 

# PAI  VARIABLE LABELS 
var_label(data$paifrq3m_a) <- "How often had pain, past 3 months" 
var_label(data$paiamnt_a) <- "How much pain last time" 
var_label(data$paiwklm3m_a) <- "How often pain limits life or work" 
var_label(data$paiaffm3m_a) <- "How often pain impacts family" 
var_label(data$paiback3m_a) <- "Back pain" 
var_label(data$paiulmb3m_a) <- "Pain in hands" 
var_label(data$paillmb3m_a) <- "Pain in hips" 
var_label(data$paihdfc3m_a) <- "Migraine" 
var_label(data$paiapg3m_a) <- "Abdominal pain" 
var_label(data$paitooth3m_a) <- "Toothache or jaw pain" 

# REP  VARIABLE LABELS 
var_label(data$repstrain_a) <- "Repetitive strain injuries in the past 3 months" 
var_label(data$replimit_a) <- "Limited by repetitive strain injury in the past 3 months" 
var_label(data$repsawdoc_a) <- "Saw a doctor about repetitive strain injury" 
var_label(data$repwrkdytc_a) <- "Number of days missed due to repetitive strain injury" 
var_label(data$repfutwrk_a) <- "Expected workdays missed due to repetitive strain injury" 
var_label(data$repstopchg_a) <- "Stopped working or changed job due to repetitive strain injury" 
var_label(data$repreduce_a) <- "Reduced work or changed tasks due to repetitive strain injury" 
var_label(data$repwrkcaus_a) <- "Did repetitive strain injury occur while working" 

# INJ  VARIABLE LABELS 
var_label(data$anyinjury_a) <- "Any injury in the past 3 months (not including repetitive strain injuries)" 
var_label(data$injlimit_a) <- "Limited by injury in the past 3 months" 
var_label(data$numinjtc_a) <- "Number of injuries in the past 3 months" 
var_label(data$injhome_a) <- "Did injury occur at home" 
var_label(data$injwork_a) <- "Did injury occur at work" 
var_label(data$injsports_a) <- "Did injury occur while playing sports or exercising" 
var_label(data$injfall_a) <- "Did injury occur because of a fall" 
var_label(data$injfallhom_a) <- "Did fall occur at home" 
var_label(data$injfallwrk_a) <- "Did fall occur at work" 
var_label(data$injmotor_a) <- "Injury caused by a motor vehicle crash or collision" 
var_label(data$injmvtype1_a) <- "Motor vehicle accident - driver" 
var_label(data$injmvtype2_a) <- "Motor vehicle accident - passenger" 
var_label(data$injmvtype3_a) <- "Motor vehicle accident - bicyclist" 
var_label(data$injmvtype4_a) <- "Motor vehicle accident - pedestrian" 
var_label(data$injmvtype5_a) <- "Motor vehicle accident - something else" 
var_label(data$injchores_a) <- "Did injury occur while doing household activities" 
var_label(data$injsawdoc_a) <- "Saw a doctor about injury" 
var_label(data$injer_a) <- "Visited ER for injury" 
var_label(data$injhosp_a) <- "Hospitalized for injury" 
var_label(data$injbones_a) <- "Did injury cause broken bones" 
var_label(data$injstitch_a) <- "Did injury require stitches or staples" 
var_label(data$injwrkdytc_a) <- "Number of workdays missed due to injury in the past 3 months" 
var_label(data$injfutwrk_a) <- "Expected workdays missed due to injury" 
var_label(data$injstopchg_a) <- "Stopped working or changed jobs due to injury" 
var_label(data$injreduce_a) <- "Reduced work or changed tasks due to injury" 

# TBI  VARIABLE LABELS 
var_label(data$tbilcdcmg_a) <- "Lost consciousness, dazed or confused, or had gap in memory, past 12 months" 
var_label(data$tbihlsbmc_a) <- "Headache, sensitivities, balance problems or mood change, past 12 months" 
var_label(data$tbisport_a) <- "Blow or jolt to head while playing sports or rec activity, past 12 months" 
var_label(data$tbileague_a) <- "Blow or jolt to head while playing organized sports, past 12 months" 
var_label(data$tbieval_a) <- "Evaluated for concussion, past 12 months" 

# ART  VARIABLE LABELS 
var_label(data$jntsymp_a) <- "Arthritis symptoms, past 30 days" 
var_label(data$jntpn_a) <- "Arthritis pain, past 30 days" 
var_label(data$arthlmt_a) <- "Arthritis activity limitations" 
var_label(data$arthwrk_a) <- "Arthritis work limitations" 
var_label(data$arthph_a) <- "Physical activity to help with arthritis" 

# CIG  VARIABLE LABELS 
var_label(data$smkev_a) <- "Ever smoked 100 cigarettes" 
var_label(data$smknow_a) <- "Now smoke cigarettes" 
var_label(data$smkcigst_a) <- "Cigarette smoking status" 
var_label(data$cignow_a) <- "Number of cigarettes a day" 
var_label(data$smk30d_a) <- "Number of days smoked past month" 
var_label(data$cig30d_a) <- "Number of cigarettes on days smoked past month" 
var_label(data$mentholc_a) <- "Smoke menthol or non-menthol cigarettes" 
var_label(data$ecigev_a) <- "Ever used electronic cigarettes" 
var_label(data$ecignow_a) <- "Now use electronic cigarettes" 
var_label(data$smkecigst_a) <- "Electronic cigarette use status" 

# OTB  VARIABLE LABELS 
var_label(data$cigarev_a) <- "Ever smoked a cigar" 
var_label(data$cigarcur_a) <- "Now smoke cigars" 
var_label(data$cigar30d_a) <- "How many days smoked a cigar, past 30 days" 
var_label(data$pipeev_a) <- "Ever smoked a pipe filled with tobacco" 
var_label(data$pipecur_a) <- "Now smoked pipe filled with tobacco" 
var_label(data$smokelsev_a) <- "Ever used smokeless tobacco" 
var_label(data$smokelscur_a) <- "Now use smokeless tobacco" 

# SVI  VARIABLE LABELS 
var_label(data$avisexam_a) <- "Last time had eye exam" 
var_label(data$avisreh_a) <- "Use vision rehabilitation services" 
var_label(data$avisdev_a) <- "Use vision assistive devices" 
var_label(data$avissadv_a) <- "Health professional recommend services" 
var_label(data$vimread_a) <- "Need eyeglasses or contacts to read up close" 
var_label(data$vimdrive_a) <- "Need eyeglasses or contacts to see in distance" 

# SHE  VARIABLE LABELS 
var_label(data$ahearst1_a) <- "Hearing ability" 
var_label(data$hrwhisp_a) <- "Hear whispers" 
var_label(data$earinfect_a) <- "Ear infection past 12 months" 
var_label(data$earinfect3_a) <- "3or more ear infections" 
var_label(data$cbalhdinj_a) <- "Lifetime significant head injury" 
var_label(data$cbalhdno_a) <- "Number of lifetime head injuries" 
var_label(data$hrtest_a) <- "Ever had hearing test" 
var_label(data$hrtestlast_a) <- "How long since hearing test" 
var_label(data$hraidaqr_a) <- "Hearing aid fit or purchased" 
var_label(data$baldizz_a) <- "Balance or dizziness problem past 12 months" 
var_label(data$baldprob_a) <- "How big of a balance or dizziness problem" 
var_label(data$baldhp_a) <- "Health provider for balance or dizziness problem" 
var_label(data$bfall12m_a) <- "Fallen past 12 months" 
var_label(data$bfalltimes_a) <- "Number of falls past 12 months" 
var_label(data$hrtinnitus_a) <- "Tinnitus past 12 months" 
var_label(data$hrtinlng_a) <- "Tinnitus how long" 
var_label(data$hrtinprob_a) <- "Tinnitus how big a problem" 
var_label(data$hrtinmedsp_a) <- "Medical specialist for tinnitus" 
var_label(data$hrloudjob_a) <- "Ever exposed to loud noise at job" 
var_label(data$hrloudjbyr_a) <- "Years exposed to loud sounds at job" 
var_label(data$hrloudjb12m_a) <- "Exposed to loud sounds past 12 months" 
var_label(data$hrjobprot_a) <- "Job exposure hearing protection" 
var_label(data$hrfireev_a) <- "Ever used a firearm" 
var_label(data$hrfiretotr_a) <- "Firearm total rounds" 
var_label(data$hrfire12m_a) <- "Firearm rounds past 12 months" 
var_label(data$hrfireprot_a) <- "Firearm hearing protection" 
var_label(data$hrvloud12m_a) <- "Exposed to very loud sounds past 12 months" 
var_label(data$hrvldprot_a) <- "Very loud sounds hearing protection" 

# SWE  VARIABLE LABELS 
var_label(data$hrjbexp12m_a) <- "Job exposure to chemicals, past 12 months" 
var_label(data$hrjbexp4hr_a) <- "Job exposure to chemicals 4 or more hours" 
var_label(data$hrjbexptb_a) <- "Job exposure to tobacco smoke 4 or more hours, past 12 months" 

# ORN  VARIABLE LABELS 
var_label(data$orient_a) <- "Sexual orientation" 

# MAR  VARIABLE LABELS 
var_label(data$marital_a) <- "Sample adult's current marital status" 
var_label(data$spousliv_a) <- "Sample adult's spouse lives here" 
var_label(data$spousep_a) <- "Sample adult's spouse does not reside here due to legal separation" 
var_label(data$evrmarried_a) <- "Sample adult has ever been married" 
var_label(data$marstat_a) <- "Current marital status of sample adult" 
var_label(data$legmstat_a) <- "Legal marital status of sample adult" 
var_label(data$spousesex_a) <- "Sex of sample adult's spouse" 
var_label(data$saspprace_a) <- "Race of sample adult and spouse or partner are the same" 
var_label(data$saspphisp_a) <- "Hispanic ethnicity of sample adult and spouse or partner are the same" 
var_label(data$spousagetc_a) <- "Age of sample adult's spouse, top-coded" 
var_label(data$spouseducp_a) <- "Education level of sample adult's spouse" 
var_label(data$spouswrk_a) <- "Working status of sample adult's spouse" 
var_label(data$spouswkft_a) <- "Sample adult's spouse is working full-time" 
var_label(data$prtnrsex_a) <- "Sex of the sample adult's partner" 
var_label(data$prtnragetc_a) <- "Age of sample adult's partner, top-coded" 
var_label(data$prtnreducp_a) <- "Education level of sample adult's partner" 
var_label(data$prtnrwrk_a) <- "Working status of sample adult's partner" 
var_label(data$prtnrwkft_a) <- "Sample adult's partner is working full-time" 
var_label(data$saparentsc_a) <- "Sample adult relationship to sample child" 
var_label(data$parstat_a) <- "Parental Status of sample adult" 

# VET  VARIABLE LABELS 
var_label(data$afvet_a) <- "Ever serve active duty military" 
var_label(data$afvettrn_a) <- "Reserves or National Guard" 
var_label(data$combat_a) <- "Ever served abroad during armed conflict" 
var_label(data$vadisb_a) <- "Have VA disability rating" 
var_label(data$vahosp_a) <- "Receive care at VA facility" 
var_label(data$vacareev_a) <- "Ever use VA health care" 

# NAT  VARIABLE LABELS 
var_label(data$natusborn_a) <- "Born in U.S. or U.S. territory" 
var_label(data$yrsinus_a) <- "Years that sample adult has been in the United States" 
var_label(data$citznstp_a) <- "Citizenship status" 

# LNG  VARIABLE LABELS 
var_label(data$langhm_a) <- "Other language spoken at home" 
var_label(data$langspecr_a) <- "Language at home, public use" 
var_label(data$langmed_a) <- "Language for TV, news, radio" 
var_label(data$langdoc_a) <- "Language at doctor" 
var_label(data$langsoc_a) <- "Language socially" 

# SCH  VARIABLE LABELS 
var_label(data$schcurenr_a) <- "Currently in school" 
var_label(data$schdymsstc_a) <- "Days of school missed due to illness or injury past 12 months, top-coded" 

# EMP  VARIABLE LABELS 
var_label(data$emplastwk_a) <- "Worked for pay last week" 
var_label(data$empnowrk_a) <- "Temporarily absent from work last week" 
var_label(data$empwhynot_a) <- "Main reason not working" 
var_label(data$empwhenwrk_a) <- "Last time worked" 
var_label(data$empwrklsw1_a) <- "Worked last week" 
var_label(data$emplstwor1_a) <- "Last time worked for pay" 
var_label(data$empwkhrs3_a) <- "Hours worked per week (topcoded for Public Use)" 
var_label(data$empwrkft1_a) <- "Usually work 35+ hours per week" 
var_label(data$empsicklv_a) <- "Paid sick leave" 
var_label(data$emphealins_a) <- "Health insurance offered" 
var_label(data$empdysmss3_a) <- "Days missed work, past 12 months (top-coded)" 

# EMD  VARIABLE LABELS 
var_label(data$emdindstn1_a) <- "Detailed 2-digit recode for sample adult's industry" 
var_label(data$emdindstn2_a) <- "Simple 2-digit recode for sample adult's industry" 
var_label(data$emdoccupn1_a) <- "Detailed 2-digit recode for sample adult's occupation" 
var_label(data$emdoccupn2_a) <- "Simple 2-digit recode for sample adult's occupation" 
var_label(data$emdsuper_a) <- "Supervise other employees" 
var_label(data$emdwrkcat1_a) <- "Type of main job" 

# VOL  VARIABLE LABELS 
var_label(data$cevolun1_a) <- "Volunteer for organization or association" 
var_label(data$cevolun2_a) <- "Other volunteer activities" 

# FEM  VARIABLE LABELS 
var_label(data$pcntadwkp1_a) <- "Number of adults in sample adult's family who are working (top-coded)" 
var_label(data$pcntadwfp1_a) <- "Number of adults in sample adult's family who are working full-time (top-coded)" 

# INC  VARIABLE LABELS 
var_label(data$incwrko_a) <- "Income from wages" 
var_label(data$incinter_a) <- "Income from accounts" 
var_label(data$incssrr_a) <- "Income from SS/Railroad Retirement" 
var_label(data$incssissdi_a) <- "Family income from SSDI" 
var_label(data$ssissdibth_a) <- "Which family income SSI/SSDI" 
var_label(data$ssissdidsb_a) <- "SSI/SSDI due to disability" 
var_label(data$incwelf_a) <- "Income from public assistance" 
var_label(data$incretire_a) <- "Income from retirement" 
var_label(data$incothr_a) <- "Income from other sources" 
var_label(data$impnum_a) <- "Imputation num" 
var_label(data$povrattc_a) <- "SA family poverty ratio (top-coded)" 
var_label(data$ratcat_a) <- "Ratio of family income to poverty threshold for SA's family" 
var_label(data$impincflg_a) <- "Imputed SA family income imputation flag" 
var_label(data$inctcflg_a) <- "Sample adult family income top-code flag" 

# FOO  VARIABLE LABELS 
var_label(data$fsnap12m_a) <- "Receive food stamps, past 12m" 
var_label(data$fsnap30d_a) <- "Receive food stamps, past 30d" 
var_label(data$fwic12m_a) <- "Receive WIC benefits, past 12m" 
var_label(data$flunch12m1_a) <- "Receive free/reduced meals at school" 

# FDS  VARIABLE LABELS 
var_label(data$fdsrunout_a) <- "Worry food would run out" 
var_label(data$fdslast_a) <- "Food didn't last" 
var_label(data$fdsbalance_a) <- "Couldn't afford to eat balanced meals" 
var_label(data$fdsskip_a) <- "Cut the size of meals or skip meals" 
var_label(data$fdsskipdys_a) <- "How many days did you/adults in the family cut the size of meals or skip meals" 
var_label(data$fdsless_a) <- "Eat less than should" 
var_label(data$fdshungry_a) <- "Ever hungry because not enough money for food" 
var_label(data$fdsweight_a) <- "Lose weight because not enough money for food" 
var_label(data$fdsnoteat_a) <- "Not eat for a whole day" 
var_label(data$fdsnedays_a) <- "How many days not eat" 
var_label(data$fdscat3_a) <- "Adult 3 category food security recode" 
var_label(data$fdscat4_a) <- "Adult  4 category food security recode" 

# HOU  VARIABLE LABELS 
var_label(data$houyrsliv_a) <- "Length of time in house or apartment" 
var_label(data$houtenure_a) <- "Residence owned or rented" 
var_label(data$hougvasst_a) <- "Paying lower rent" 

# SDH  VARIABLE LABELS 
var_label(data$housecost_a) <- "Had trouble paying for housing" 

# TBH  VARIABLE LABELS 
var_label(data$transpor_a) <- "Delay care in the past 12 months because no reliable transportation" 

# CIV  VARIABLE LABELS 
var_label(data$cemmetng_a) <- "Attend public meeting" 
var_label(data$cevotelc_a) <- "Vote in last local elections"

# first 881 lines work
# Start Value Labels------------------------------------------------------------
# val_labels(SECTION
# 
# 1. replace "label define " with "val_labels("
# 2. replace "X$" (select regex box in find bar) with "X <- c("
# 3. replace numbers - (regex on) - "1\s{3,}" with "1_
# 4. repeat for all numbers 0 thorugh 9
# 5. put commas and end of each line (regex on) - replace '"$' with '",' (but only for this section, not all)
# 6. replace "))" with "))" and end of each value label
# 7. Replace '"Don't Know",' with '"Don't Know"' to remove final comma
# 8. any trailing commas on the last value label by hand
# 
# DEFINE VALUE LABELS FOR REPORTS))

data |> 
  mutate(bmicat_a = ordered(bmicat_a,
                           levels = c(1,2,3,4,5),
                           labels = c("1_Underweight", "2_Healthy weight", "3_Overweight", "4_Obese", "9_Unknown")))  


# SA001X_labels <- c("Underweight", "Healthy weight", "Overweight", "Obese", "Unknown")
# 
# label define SA001X
# 1    "Underweight"
# 2    "Healthy weight"
# 3    "Overweight"
# 4    "Obese"
# 9    "Unknown"
# ;

data |> 
  mutate(chflg_a = ordered(chflg_a,
                          levels = c(1),
                          labels = c("1_Reassigned_to_CHIP_from_private"))) 


# label define SA002X
# 1    "Reassigned to CHIP from private"
# ;

data |> 
  mutate(citznstp_a = ordered(citznstp_a,
      levels = c(1,2,3,4,5),
      labels = c("1_Yes, a citizen of the United States",
                  "2_No, not a citizen of the United States",
                  "7_Refused",
                  "8_Not Ascertained",
                  "9_Don't Know")))

# val_labels(SA003X <- c(
# 1_Yes, a citizen of the United States",
# 2_No, not a citizen of the United States",
# 7_Refused",
# 8_Not Ascertained",
# 9_Don't Know"
# ))

data |> 
  mutate(cover_a = ordered(cover_a,
        levels = c(1,2,3,4,5),
        labels = c("1_Private", "2_Medicaid and other public", "3_Other coverage", "4_Uninsured", "5_Don't Know")))

# val_labels(SA004X <- c(
# 1_Private",
# 2_Medicaid and other public",
# 3_Other coverage",
# 4_Uninsured",
# 5_Don't Know"
# ))

data |> 
  mutate(cover65_a = ordered(cover65_a,
        levels = c(1,2,3,4,5,6,7),
        labels = c("1_Private", "2_Dual eligible", "3_Medicare Advantage", "4_Medicare only excluding Medicare Advantage", "5_Other coverage", "6_Uninsured", "7_Don't Know")))

# val_labels(SA005X <- c(
# 1_Private",
# 2_Dual eligible",
# 3_Medicare Advantage",
# 4_Medicare only excluding Medicare Advantage",
# 5_Other coverage",
# 6_Uninsured",
# 7_Don't Know"
# ))

data |> 
  mutate(diba1cnmt_a = ordered(diba1cnmt_a,
        levels = c(1,2,3,4,5,6,7,8),
        labels = c("1", "2", "3", "4", "5 or more", "7_Refused", "8_Not Ascertained", "9_Don't Know")))

        
# val_labels(SA006X <- c(
# 1_1",
# 2_2",
# 3_3",
# 4_4",
# 5_5 or more",
# 7_Refused",
# 8_Not Ascertained",
# 9_Don't Know"
# ))

# write a mutate function to replace the values in the data frame
# for the variable smkecigst_a with the value labels for SA007X found below
# by making it an ordered factor with the levels and labels below

data |> 
  mutate(smkecigst_a = ordered(smkecigst_a,
        levels = c(1,2,3,4,5),
        labels = c("1_Current e-cigarette user", "2_Used e-cigarette, not current user", "3_Never e-cigarette user", "4_E-cigarette user, current status unknown", "5_Unknown if ever used e-cigarette")))

# val_labels(SA007X <- c(
# 1_Current e-cigarette user",
# 2_Used e-cigarette, not current user",
# 3_Never e-cigarette user",
# 4_E-cigarette user, current status unknown",
# 9_Unknown if ever used e-cigarette"
# ))

# write a mutate function to replace the values in the data frame
# for the variable spouseducp_a with the value labels for SA008X found below
# by making it an ordered factor with the levels and labels below

data |> 
  mutate(spouseducp_a = ordered(spouseducp_a,
        levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13),
        labels = c("01_Grade 0-11", "02_12th grade, no diploma", "03_GED or equivalent", "04_High School Graduate", "05_Some college, no degree", "06_Associate degree: occupational, technical, or vocational program", "07_Associate degree: academic program", "08_Bachelor's degree (Example: BA, AB, BS, BBA)", "09_Master's degree (Example: MA, MS, MEng, MEd, MBA)", "10_Professional School or Doctoral degree (Example: MD, DDS, DVM, JD, PhD, EdD)", "97_Refused", "98_Not Ascertained", "99_Don't Know")))

data |> 
  mutate(prtnreducp_a = ordered(prtnreducp_a,
        levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13),
        labels = c("01_Grade 0-11", "02_12th grade, no diploma", "03_GED or equivalent", "04_High School Graduate", "05_Some college, no degree", "06_Associate degree: occupational, technical, or vocational program", "07_Associate degree: academic program", "08_Bachelor's degree (Example: BA, AB, BS, BBA)", "09_Master's degree (Example: MA, MS, MEng, MEd, MBA)", "10_Professional School or Doctoral degree (Example: MD, DDS, DVM, JD, PhD, EdD)", "97_Refused", "98_Not Ascertained", "99_Don't Know")))

# write a mutate function to replace the values in the data frame
# for the variable smkecigst_a with the value labels for SA008X found below
# by making it an ordered factor with the levels and labels below

# val_labels(SA008X <- c(
# 01_Grade 0-11",
# 02_12th grade, no diploma",
# 03_GED or equivalent",
# 04_High School Graduate",
# 05_Some college, no degree",
# 06_Associate degree: occupational, technical, or vocational program",
# 07_Associate degree: academic program",
# 08_Bachelor's degree (Example: BA, AB, BS, BBA)",
# 09_Master's degree (Example: MA, MS, MEng, MEd, MBA)",
# 10_Professional School or Doctoral degree (Example: MD, DDS, DVM, JD, PhD, EdD)",
# 97_Refused",
# 98_Not Ascertained",
# 99_Don't Know"
# ))


# write a mutate function to replace the values in the data frame
# for the variable emerg12mtc_a with the value labels for SA009X found below
# by making it an ordered factor with the levels and labels below

data |> 
  mutate(emerg12mtc_a = ordered(emerg12mtc_a,
        levels = c(1,2,3,4,5,6,7,8),
        labels = c("0 times", "1 time", "2 times", "3 times", "4+ times", "7_Refused", "8_Not Ascertained", "9_Don't Know")))


# val_labels(SA009X <- c(
# 0_0 times",
# 1_1 time",
# 2_2 times",
# 3_3 times",
# 4_4+ times",
# 7_Refused",
# 8_Not Ascertained",
# 9_Don't Know"
# ))


# write a mutate function to replace the values in the data frame
# for the variable exchange_a with the value labels for SA010X found below
# by making it an ordered factor with the levels and labels below

data |> 
  mutate(exchange_a = ordered(exchange_a,
        levels = c(1,2,3),
        labels = c("1_Exchange plan", "2_Not exchange plan", "3_Not Ascertained")))

# val_labels(SA010X <- c(
# 1_Exchange plan",
# 2_Not exchange plan",
# 8_Not Ascertained"
# ))


# write a mutate function to replace the values in the data frame
# for the variable exchpr1_a with the value labels for SA011X found below
# by making it an ordered factor with the levels and labels below

data |> 
  mutate(exchpr1_a = ordered(exchpr1_a,
        levels = c(1,2,3,4),
        labels = c("1_Company provides exchange plans", "2_Not an exchange company", "3_Exchange Portal or exact exchange plan name", "8_Not Ascertained"))) |> 
  mutate(exchpr1_a = ordered(exchpr2_a,
        levels = c(1,2,3,4),
        labels = c("1_Company provides exchange plans", "2_Not an exchange company", "3_Exchange Portal or exact exchange plan name", "8_Not Ascertained"))) |> 
  mutate(exchpr1_a = ordered(plexchpr1_a,
        levels = c(1,2,3,4),
        labels = c("1_Company provides exchange plans", "2_Not an exchange company", "3_Exchange Portal or exact exchange plan name", "8_Not Ascertained"))) |> 
  mutate(exchpr1_a = ordered(plexchop_a,
        levels = c(1,2,3,4),
        labels = c("1_Company provides exchange plans", "2_Not an exchange company", "3_Exchange Portal or exact exchange plan name", "8_Not Ascertained"))) |> 
  mutate(exchpr1_a = ordered(plexchog_a,
         levels = c(1,2,3,4),
         labels = c("1_Company provides exchange plans", "2_Not an exchange company", "3_Exchange Portal or exact exchange plan name", "8_Not Ascertained"))) 

# val_labels(SA011X <- c(
# 1_Company provides exchange plans",
# 2_Not an exchange company",
# 3_Exchange Portal or exact exchange plan name",
# 8_Not Ascertained"
# ))

# write a mutate function to replace the values in the data frame
# for the variable fdscat3_a with the numeric and value labels for SA012X found below, linked by an underscore, i.e. "1_Food secure"
# by making it an ordered factor with the levels and labels below

data |> 
  mutate(fdscat3_a = ordered(fdscat3_a,
        levels = c(1,2,3,4),
        labels = c("1_Food secure", "2_Low food security", "3_Very low food security", "8_Not Ascertained")))

# val_labels(SA012X <- c(
# 1_Food secure",
# 2_Low food security",
# 3_Very low food security",
# 8_Not Ascertained"
# ))

# write a mutate function to replace the values in the data frame
# for the variable fdscat4_a with the numeric and value labels for SA013X found below, linked by an underscore, i.e. "1_Food secure"
# by making it an ordered factor with the levels and labels below

data |> 
  mutate(fdscat4_a = ordered(fdscat4_a,
        levels = c(1,2,3,4,5),
        labels = c("1_High food security", "2_Marginal food security", "3_Low food security", "4_Very low food security", "8_Not Ascertained")))

# val_labels(SA013X <- c(
# 1_High food security",
# 2_Marginal food security",
# 3_Low food security",
# 4_Very low food security",
# 8_Not Ascertained"
# ))

# write a mutate function to replace the values in the data frame
# for the variables phq2screen_a  and gad2screen_a with the numeric and value labels for SA014X found below, linked by an underscore, i.e. "1_Food secure"
# by making it an ordered factor with the levels and labels below

data |> 
  mutate(phq2screen_a = ordered(phq2screen_a,
        levels = c(1,2,3),
        labels = c("1_Positive", "2_Negative", "8_Not Ascertained"))) |> 
  mutate(gad2screen_a = ordered(gad2screen_a,
        levels = c(1,2,3),
        labels = c("1_Positive", "2_Negative", "8_Not Ascertained")))

# val_labels(SA014X <- c(
# 1_Positive",
# 2_Negative",
# 8_Not Ascertained"
# ))

# write a mutate function to replace the values in the data frame
# for the variable hhstat_a with the numeric and value labels for SA015X found below, linked by an underscore, i.e. "1_Food secure"
# by making it an ordered factor with the levels and labels below

data |> 
  mutate(hhstat_a = ordered(hhstat_a,
        levels = c(1,2),
        labels = c("1_Person is sample adult", "8_Not Ascertained")))

# val_labels(SA015X <- c(
# 1_Person is sample adult"
# ))

# write a mutate function to replace the values in the data frame
# for the variable hisdetp_a with the numeric and value labels for SA016X found below, linked by an underscore, i.e. "1_Food secure"
# by making it an ordered factor with the levels and labels below

data |> 
  mutate(hisdetp_a = ordered(hisdetp_a,
        levels = c(1,2,3,4,5,6),
        labels = c("1_Hispanic (Mexican/Mexican American)", "2_Hispanic (all other groups)", "3_Not Hispanic", "7_Refused", "8_Not Ascertained", "9_Don't Know")))

# val_labels(SA016X <- c(
# 1_Hispanic (Mexican/Mexican American)",
# 2_Hispanic (all other groups)",
# 3_Not Hispanic",
# 7_Refused",
# 8_Not Ascertained",
# 9_Don't Know"
# ))

# write a mutate function to replace the values in the data frame
# for the variable hispallp_a with the numeric and value labels for SA017X found below, linked by an underscore, i.e. "1_Food secure"
# by making it an ordered factor with the levels and labels below

data |> 
  mutate(hispallp_a = ordered(hispallp_a,
        levels = c(1,2,3,4,5,6,7,8,9,10),
        labels = c("01_Hispanic", "02_Non-Hispanic White only", "03_Non-Hispanic Black/African American only", "04_Non-Hispanic Asian only", "05_Non-Hispanic AIAN only", "06_Non-Hispanic AIAN and any other group", "07_Other single and multiple races", "97_Refused", "98_Not Ascertained", "99_Don't Know")))

# val_labels(SA017X <- c(
# 01_Hispanic",
# 02_Non-Hispanic White only",
# 03_Non-Hispanic Black/African American only",
# 04_Non-Hispanic Asian only",
# 05_Non-Hispanic AIAN only",
# 06_Non-Hispanic AIAN and any other group",
# 07_Other single and multiple races",
# 97_Refused",
# 98_Not Ascertained",
# 99_Don't Know"
# ))

# write a mutate function to replace the values in the data frame
# for the variable emdindstn1_a with the 99 numeric and 99 value labels for SA018X found below, linked by an underscore, i.e. "01_Crop production"
# by making it an ordered factor with the levels and labels below

data |> 
  mutate(emdindstn1_a = ordered(emdindstn1_a,
       levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82),
        labels = c("01_Crop production", "02_Animal production and aquaculture", "03_Forestry and logging", "04_Fishing, hunting, and trapping", "05_Support activities for agriculture and forestry", "06_Oil and gas extraction", "07_Mining (except oil and gas)", "08_Support activities for mining", "09_Utilities industries", "10_Construction industries", "11_Food manufacturing", "12_Beverage and tobacco product manufacturing", "13_Textile mills", "14_Textile product mills", "15_Apparel manufacturing", "16_Leather and allied product manufacturing", "17_Wood product manufacturing", "18_Paper manufacturing", "19_Printing and related support activities", "20_Petroleum and coal products manufacturing", "21_Chemical manufacturing", "22_Plastics and rubber products manufacturing", "23_Nonmetallic mineral product manufacturing", "24_Primary metal manufacturing", "25_Fabricated metal product manufacturing", "26_Machinery manufacturing", "27_Computer and electronic product manufacturing", "28_Electrical equipment, appliance, and component manufacturing", "29_Transportation equipment manufacturing", "30_Furniture and related product manufacturing", "31_Miscellaneous manufacturing", "32_Wholesale trade", "33_Retail trade", "34_Non-specified wholesale trade",
"35_Motor vehicle and parts dealers",
"36_Furniture and home furnishings stores",
"37_Electronics and appliance stores",
"38_Building material and garden equipment and supplies dealers",
"39_Food and beverage stores",
"40_Health and personal care stores",
"41_Gasoline stations",
"42_Clothing, shoe, jewelry, luggage, and leather goods stores",
"43_Sporting goods, camera, hobby, book and music stores",
"44_General merchandise stores",
"45_Miscellaneous store retailers",
"46_Nonstore retailers and non-specified retail trade",
"47_Transportation (including support activities for transportation)",
"48_Postal service, couriers, and messengers",
"49_Warehousing and storage",
"50_Newspaper, periodical, book, and software publishing industries",
"51_Motion picture, video, and sound recording industries",
"52_Broadcasting and telecommunications",
"53_Libraries and archives, internet publishing, web search portals, data processing and hosting services, and other information services",
"54_Monetary authorities -- central bank",
"55_Credit intermediation and related activities",
"56_Securities, commodity contracts, and other financial investments and related activities",
"57_Insurance carriers and related activities",
"58_Real estate",
"59_Automotive and other consumer goods rental and leasing services",
"60_Commercial, industrial, and other intangible assets (except copyrighted works)",
"61_Professional, scientific, and technical services industries",
"62_Management of companies and enterprises industries",
"63_Administrative and support and waste management and remediation services industries",
"64_Education services industries",
"65_Ambulatory health care services",
"66_Hospitals",
"67_Nursing and residential care facilities",
"68_Social assistance",
"69_Performing arts, spectator sports, promoters, agents, artists, writers and related industries",
"70_Museums, historical sites, and similar institutions",
"71_Amusement, gambling, and recreation industries",
"72_Accommodation",
"73_Food services and drinking places",
"74_Repair and maintenance",
"75_Personal services (barber shops, beauty salons, nail salons, laundry, funeral homes and cemetaries)",
"76_Religious, grantmaking, civic, labor, professional, and similar organizations",
"77_Private households",
"78_Public administration industries",
"79_Armed forces",
"97_Refused, classified",
"98_Not ascertained",
"99_Don't_know"
)))


# val_labels(SA018X <- c(
# "01_Crop production",
# "02_Animal production and aquaculture",
# "03_Forestry and logging",
# "04_Fishing, hunting, and trapping",
# "05_Support activities for agriculture and forestry",
# "06_Oil and gas extraction",
# "07_Mining (except oil and gas)",
# "08_Support activities for mining",
# "09_Utilities industries",
# "10_Construction industries",
# "11_Food manufacturing",
# "12_Beverage and tobacco product manufacturing",
# "13_Textile mills",
# "14_Textile product mills",
# "15_Apparel manufacturing",
# "16_Leather and allied product manufacturing",
# "17_Wood product manufacturing",
# "18_Paper manufacturing",
# "19_Printing and related support activities",
# "20_Petroleum and coal products manufacturing",
# "21_Chemical manufacturing",
# "22_Plastics and rubber products manufacturing",
# "23_Nonmetallic mineral product manufacturing",
# "24_Primary metal manufacturing",
# "25_Fabricated metal product manufacturing",
# "26_Machinery manufacturing",
# "27_Computer and electronic product manufacturing",
# "28_Electrical equipment, appliance, and component manufacturing",
# "29_Transportation equipment manufacturing",
# "30_Furniture and related product manufacturing",
# "31_Miscellaneous manufacturing",
# "32_Merchant wholesalers, durable goods",
# "33_Merchant wholesalers, nondurable goods",
# "34_Non-specified wholesale trade",
# "35_Motor vehicle and parts dealers",
# "36_Furniture and home furnishings stores",
# "37_Electronics and appliance stores",
# "38_Building material and garden equipment and supplies dealers",
# "39_Food and beverage stores",
# "40_Health and personal care stores",
# "41_Gasoline stations",
# "42_Clothing, shoe, jewelry, luggage, and leather goods stores",
# "43_Sporting goods, camera, hobby, book and music stores",
# "44_General merchandise stores",
# "45_Miscellaneous store retailers",
# "46_Nonstore retailers and non-specified retail trade",
# "47_Transportation (including support activities for transportation)",
# "48_Postal service, couriers, and messengers",
# "49_Warehousing and storage",
# "50_Newspaper, periodical, book, and software publishing industries",
# "51_Motion picture, video, and sound recording industries",
# "52_Broadcasting and telecommunications",
# "53_Libraries and archives, internet publishing, web search portals, data processing and hosting services, and other information services",
# "54_Monetary authorities -- central bank",
# "55_Credit intermediation and related activities",
# "56_Securities, commodity contracts, and other financial investments and related activities",
# "57_Insurance carriers and related activities",
# "58_Real estate",
# "59_Automotive and other consumer goods rental and leasing services",
# "60_Commercial, industrial, and other intangible assets (except copyrighted works)",
# "61_Professional, scientific, and technical services industries",
# "62_Management of companies and enterprises industries",
# "63_Administrative and support and waste management and remediation services industries",
# "64_Education services industries",
# "65_Ambulatory health care services",
# "66_Hospitals",
# "67_Nursing and residential care facilities",
# "68_Social assistance",
# "69_Performing arts, spectator sports, promoters, agents, artists, writers and related industries",
# "70_Museums, historical sites, and similar institutions",
# "71_Amusement, gambling, and recreation industries",
# "72_Accommodation",
# "73_Food services and drinking places",
# "74_Repair and maintenance",
# "75_Personal services (barber shops, beauty salons, nail salons, laundry, funeral homes and cemetaries)",
# "76_Religious, grantmaking, civic, labor, professional, and similar organizations",
# "77_Private households",
# "78_Public administration industries",
# "79_Armed forces",
# "97_Refused, classified",
# "98_Not ascertained",
# "99_Don't know"
# ))

# write a mutate function to replace the values in the data frame
# for the variable emdindstn2_a with the 99 numeric and 99 value labels for SA019X found below, linked by an underscore, i.e. "01_Crop production"
# by making it an ordered factor with the levels and labels below

data |> 
  mutate(emdindstn2_a = ordered(emdindstn2_a, 
     levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24),
      labels = c(
"01_Agriculture, Forestry, Fishing, and Hunting Industries",
"02_Mining Industries",
"03_Utilities Industries",
"04_Construction Industries",
"05_Manufacturing Industries",
"06_Wholesale Trade Industries",
"07_Retail Trade Industries",
"08_Transportation and Warehousing Industries",
"09_Information Industries",
"10_Finance and Insurance Industries",
"11_Real Estate and Rental and Leasing Industries",
"12_Professional, Scientific, and Technical Services Industries",
"13_Management of Companies and Enterprises Industries",
"14_Administrative and Support and Waste Management and Remediation Services Industries",
"15_Education Services Industries",
"16_Health Care and Social Assistance Industries",
"17_Arts, Entertainment, and Recreation Industries",
"18_Accommodation and Food Services Industries",
"19_Other Services (except Public Administration) Industries",
"20_Public Administration Industries",
"21_Armed Forces",
"97_Refused, classified",
"98_Not ascertained",
"99_Don't know" )))


# write a mutate function to replace the values in the data frame
# for the variables military_a, othpub_a, medicare_a, medicaid_a, private_a, and chip_a with the numeric and value labels for SA020X found below, linked by an underscore, i.e. "01_Crop production"
# by making it an ordered factor with the levels and labels below

data |> 
  mutate(military_a = ordered(military_a, 
     levels = c(1,2,3,4,5,6),
      labels = c(
        "1_Yes, information",
        "2_Yes, but no information",
        "3_No",
        "7_Refused",
        "8_Not Ascertained",
        "9_Don't Know"
))) |> 
  mutate(othpub_a = ordered(othpub_a, 
     levels = c(1,2,3,4,5,6),
      labels = c(
        "1_Yes, information",
        "2_Yes, but no information",
        "3_No",
        "7_Refused",
        "8_Not Ascertained",
        "9_Don't Know"
))) |> 
  mutate(medicare_a = ordered(medicare_a, 
     levels = c(1,2,3,4,5,6),
      labels = c(
        "1_Yes, information",
        "2_Yes, but no information",
        "3_No",
        "7_Refused",
        "8_Not Ascertained",
        "9_Don't Know"
))) |> 
  mutate(medicaid_a = ordered(medicaid_a, 
     levels = c(1,2,3,4,5,6),
      labels = c(
        "1_Yes, information",
        "2_Yes, but no information",
        "3_No",
        "7_Refused",
        "8_Not Ascertained",
        "9_Don't Know"
)))  |> 
  mutate(private_a = ordered(private_a, 
     levels = c(1,2,3,4,5,6),
      labels = c(
        "1_Yes, information",
        "2_Yes, but no information",
        "3_No",
        "7_Refused",
        "8_Not Ascertained",
        "9_Don't Know"
))) |> 
  mutate(chip_a = ordered(chip_a, 
     levels = c(1,2,3,4,5,6),
      labels = c(
        "1_Yes, information",
        "2_Yes, but no information",
        "3_No",
        "7_Refused",
        "8_Not Ascertained",
        "9_Don't Know"
))) 


# write a mutate function to replace the values in the data frame
# for the variable intv_mon with the numeric and value labels for SA021X found below, linked by an underscore, i.e. "01_January",
# by making it an ordered factor with the levels and labels below

data |> 
  mutate(intv_mon = ordered(intv_mon, 
     levels = c(1,2,3,4,5,6,7,8,9,10,11,12),
      labels = c(
        "01_January",
        "02_February",
        "03_March",
        "04_April",
        "05_May",
        "06_June",
        "07_July",
        "08_August",
        "09_September",
        "10_October",
        "11_November",
        "12_December")))

# val_labels(SA021X <- c(
# "01_January",
# "02_February",
# "03_March",
# "04_April",
# "05_May",
# "06_June",
# "07_July",
# "08_August",
# "09_September",
# "10_October",
# "11_November",
# "12_December"
# ))

# write a mutate function to replace the values in the data frame
# for the variable intv_qrt with the numeric and value labels for SA022X found below, linked by an underscore, i.e. "01_January",
# by making it an ordered factor with the levels and labels below

data |> 
  mutate(intv_qrt = ordered(intv_qrt, 
     levels = c(1,2,3,4),
      labels = c(
        "1_Quarter 1",
        "2_Quarter 2",
        "3_Quarter 3",
        "4_Quarter 4")))

# val_labels(SA022X <- c(
# 1_Quarter 1",
# 2_Quarter 2",
# 3_Quarter 3",
# 4_Quarter 4"
# ))

# write a mutate function to replace the values in the data frame
# for the variable langspecr_a with the numeric and value labels for SA022X found below, linked by an underscore, i.e. "1_Spanish"",
# by making it an ordered factor with the levels and labels below

data |> 
  mutate(langspecr_a = ordered(langspecr_a, 
     levels = c(1,2,3,4,5),
      labels = c(
        "1_Spanish",
        "2_Other Language",
        "7_Refused",
        "8_Not Ascertained",
        "9_Don't Know")))

# val_labels(SA023X <- c(
# 1_Spanish",
# 2_Other Language",
# 7_Refused",
# 8_Not Ascertained",
# 9_Don't Know"
# ))

# write a mutate function to replace the values in the data frame
# for the variable legmstat_a with the numeric and value labels for SA024X found below, linked by an underscore, i.e. "1_Spanish"",
# by making it an ordered factor with the levels and labels below

data |> 
  mutate(legmstat_a = ordered(legmstat_a, 
     levels = c(1,2,3,4,5,6),
      labels = c(
        "1_Separated",
        "2_Divorced",
        "3_Married",
        "4_Single/never married",
        "5_Widowed",
        "9_Unknown legal marital status")))

# val_labels(SA024X <- c(
# 1_Separated",
# 2_Divorced",
# 3_Married",
# 4_Single/never married",
# 5_Widowed",
# 9_Unknown legal marital status"
# ))

# write a mutate function to replace the values in the data frame
# for the variable maflg_a with the numeric and value labels for SA025X found below, linked by an underscore, i.e. "1_Spanish"",
# by making it an ordered factor with the levels and labels below

data |> 
  mutate(maflg_a = ordered(maflg_a, 
     levels = c(1),
      labels = 
        "1_Reassigned to Medicaid from private"))

# val_labels(SA025X <- c(
# 1_Reassigned to Medicaid from private"
# ))

# write a mutate function to replace the values in the data frame
# for the variable marstat_a with the numeric and value labels for SA025X found below, linked by an underscore, i.e. "1_Married, spouse is present",
# by making it an ordered factor with the levels and labels below

data |> 
  mutate(marstat_a = ordered(marstat_a, 
     levels = c(1,2,3,4,5,6,7,8,9),
      labels = c(
        "1_Married, spouse is present",
        "2_Married, spouse is not present",
        "3_Married, spouse presence unknown",
        "4_Widowed",
        "5_Divorced",
        "6_Separated",
        "7_Never married",
        "8_Living with a partner",
        "9_Unknown marital status")))

# val_labels(SA026X <- c(
# 1_Married, spouse is present",
# 2_Married, spouse is not present",
# 3_Married, spouse presence unknown",
# 4_Widowed",
# 5_Divorced",
# 6_Separated",
# 7_Never married",
# 8_Living with a partner",
# 9_Unknown marital status"
# ))

# write a mutate function to replace the values in the data frame
# for the variable mcadvr_a with the numeric and value labels for SA027X found below, linked by an underscore, i.e. "1_Medicare Advantage",
# by making it an ordered factor with the levels and labels below

data |> 
  mutate(mcadvr_a = ordered(mcadvr_a, 
     levels = c(1,2,3,4,5),
      labels = c(
        "1_Medicare Advantage",
        "2_Private plan not Medicare Advantage",
        "7_Refused",
        "8_Not Ascertained",
        "9_Don't Know")))

# val_labels(SA027X <- c(
# 1_Medicare Advantage",
# 2_Private plan not Medicare Advantage",
# 7_Refused",
# 8_Not Ascertained",
# 9_Don't Know"
# ))

# write a mutate function to replace the values in the data frame
# for the cancer variables below with the numeric and value labels for SA028X found below, linked by an underscore, i.e. "1_Mentioned",
# by making it an ordered factor with the levels and labels below

data |> 
  mutate(bladdcan_a = ordered(bladdcan_a, 
     levels = c(1,2,3,4,5),
      labels = c(
        "1_Mentioned",
        "2_Not Mentioned",
        "7_Refused",
        "8_Not Ascertained",
        "9_Don't Know"))) |> 
  mutate(bloodcan_a = ordered(bloodcan_a, 
     levels = c(1,2,3,4,5),
      labels = c(
        "1_Mentioned",
        "2_Not Mentioned",
        "7_Refused",
        "8_Not Ascertained",
        "9_Don't Know"))) |> 
  mutate(bonecan_a = ordered(bonecan_a, 
     levels = c(1,2,3,4,5),
      labels = c(
        "1_Mentioned",
        "2_Not Mentioned",
        "7_Refused",
        "8_Not Ascertained",
        "9_Don't Know"))) |> 
  mutate(braincan_a = ordered(braincan_a, 
     levels = c(1,2,3,4,5),
      labels = c(
        "1_Mentioned",
        "2_Not Mentioned",
        "7_Refused",
        "8_Not Ascertained",
        "9_Don't Know"))) |> 
  mutate(breascan_a = ordered(breascan_a, 
     levels = c(1,2,3,4,5),
      labels = c(
        "1_Mentioned",
        "2_Not Mentioned",
        "7_Refused",
        "8_Not Ascertained",
        "9_Don't Know"))) |> 
  mutate(cervican_a = ordered(cervican_a, 
     levels = c(1,2,3,4,5),
      labels = c(
        "1_Mentioned",
        "2_Not Mentioned",
        "7_Refused",
        "8_Not Ascertained",
        "9_Don't Know"))) |> 
  mutate(coloncan_a = ordered(coloncan_a, 
        levels = c(1,2,3,4,5),
        labels = c(
        "1_Mentioned",
        "2_Not Mentioned",
        "7_Refused",
        "8_Not Ascertained",
        "9_Don't Know"))) |> 
  mutate(esophcan_a = ordered(esophcan_a, 
        levels = c(1,2,3,4,5),
        labels = c(
        "1_Mentioned",
        "2_Not Mentioned",
        "7_Refused",
        "8_Not Ascertained",
        "9_Don't Know"))) |>
  mutate(gallbcan_a = ordered(gallbcan_a,
        levels = c(1,2,3,4,5),
        labels = c(
        "1_Mentioned",
        "2_Not Mentioned",
        "7_Refused",
        "8_Not Ascertained",
        "9_Don't Know"))) |>
  mutate(laryncan_a = ordered(laryncan_a,
        levels = c(1,2,3,4,5),
        labels = c(
        "1_Mentioned",
        "2_Not Mentioned",
        "7_Refused",
        "8_Not Ascertained",
        "9_Don't Know"))) |>
  mutate(leukecan_a = ordered(leukecan_a,
        levels = c(1,2,3,4,5),
        labels = c(
        "1_Mentioned",
        "2_Not Mentioned",
        "7_Refused",
        "8_Not Ascertained",
        "9_Don't Know"))) |>
  mutate(livercan_a = ordered(livercan_a,
        levels = c(1,2,3,4,5),
        labels = c(
        "1_Mentioned",
        "2_Not Mentioned",
        "7_Refused",
        "8_Not Ascertained",
        "9_Don't Know"))) |>
  mutate(lungcan_a = ordered(lungcan_a,
        levels = c(1,2,3,4,5),
        labels = c(
        "1_Mentioned",
        "2_Not Mentioned",
        "7_Refused",
        "8_Not Ascertained",
        "9_Don't Know"))) |>
  mutate(lymphcan_a = ordered(lymphcan_a,
        levels = c(1,2,3,4,5),
        labels = c(
        "1_Mentioned",
        "2_Not Mentioned",
        "7_Refused",
        "8_Not Ascertained",
        "9_Don't Know"))) |>
  mutate(melancan_a = ordered(melancan_a,
        levels = c(1,2,3,4,5),
        labels = c(
        "1_Mentioned",
        "2_Not Mentioned",
        "7_Refused",
        "8_Not Ascertained",
        "9_Don't Know"))) |>
  mutate(mouthcan_a = ordered(mouthcan_a,
        levels = c(1,2,3,4,5),
        labels = c(
        "1_Mentioned",
        "2_Not Mentioned",
        "7_Refused",
        "8_Not Ascertained",
        "9_Don't Know"))) |>
  mutate(ovarycan_a = ordered(ovarycan_a,
        levels = c(1,2,3,4,5),
        labels = c(
        "1_Mentioned",
        "2_Not Mentioned",
        "7_Refused",
        "8_Not Ascertained",
        "9_Don't Know"))) |>
  mutate(pancrecan_a = ordered(pancrcan_a,
        levels = c(1,2,3,4,5),
        labels = c(
        "1_Mentioned",
        "2_Not Mentioned",
        "7_Refused",
        "8_Not Ascertained",
        "9_Don't Know"))) |>
  mutate(prostcan_a = ordered(prostcan_a,
        levels = c(1,2,3,4,5),
        labels = c(
        "1_Mentioned",
        "2_Not Mentioned",
        "7_Refused",
        "8_Not Ascertained",
        "9_Don't Know"))) |>
  mutate(rectucan_a = ordered(rectucan_a,
        levels = c(1,2,3,4,5),
        labels = c(
        "1_Mentioned",
        "2_Not Mentioned",
        "7_Refused",
        "8_Not Ascertained",
        "9_Don't Know"))) |>
  mutate(sknmcan_a = ordered(sknmcan_a,
        levels = c(1,2,3,4,5),
        labels = c(
        "1_Mentioned",
        "2_Not Mentioned",
        "7_Refused",
        "8_Not Ascertained",
        "9_Don't Know"))) |>
  mutate(sknnmcan_a = ordered(sknnmcan_a,
        levels = c(1,2,3,4,5),
        labels = c(
        "1_Mentioned",
        "2_Not Mentioned",
        "7_Refused",
        "8_Not Ascertained",
        "9_Don't Know"))) |>
  mutate(stomacan_a = ordered(stomacan_a,
        levels = c(1,2,3,4,5),
        labels = c(
        "1_Mentioned",
        "2_Not Mentioned",
        "7_Refused",
        "8_Not Ascertained",
        "9_Don't Know"))) |>
  mutate(throacan_a = ordered(throacan_a,
        levels = c(1,2,3,4,5),
        labels = c(
        "1_Mentioned",
        "2_Not Mentioned",
        "7_Refused",
        "8_Not Ascertained",
        "9_Don't Know"))) |>
  mutate(thyrocan_a = ordered(thyrocan_a,
        levels = c(1,2,3,4,5),
        labels = c(
        "1_Mentioned",
        "2_Not Mentioned",
        "7_Refused",
        "8_Not Ascertained",
        "9_Don't Know"))) |>
  mutate(uterucan_a = ordered(uterucan_a,
        levels = c(1,2,3,4,5),
        labels = c(
        "1_Mentioned",
        "2_Not Mentioned",
        "7_Refused",
        "8_Not Ascertained",
        "9_Don't Know"))) |>
  mutate(hdnckcan_a = ordered(hdnckcan_a,
        levels = c(1,2,3,4,5),
        labels = c(
        "1_Mentioned",
        "2_Not Mentioned",
        "7_Refused",
        "8_Not Ascertained",
        "9_Don't Know"))) |>
  mutate(colrccan_a = ordered(colrccan_a,
        levels = c(1,2,3,4,5),
        labels = c(
        "1_Mentioned",
        "2_Not Mentioned",
        "7_Refused",
        "8_Not Ascertained",
        "9_Don't Know"))) |>
  mutate(othercanp_a = ordered(othercanp_a,
        levels = c(1,2,3,4,5),
        labels = c(
        "1_Mentioned",
        "2_Not Mentioned",
        "7_Refused",
        "8_Not Ascertained",
        "9_Don't Know")))

# 
# val_labels(SA028X <- c(
# 1_Mentioned",
# 2_Not mentioned",
# 7_Refused",
# 8_Not Ascertained",
# 9_Don't Know"
# ))

# write a mutate function to replace the values in the data frame
# for the  variable mltfamflg_a below with the numeric and value labels for SA029X found below, linked by an underscore, i.e. "1_Mentioned",
# by making it an ordered factor with the levels and labels below
  
data |> 
  mutate(mltfamflg_a = ordered(mltfamflg_a,
        levels = c(1,2,3),
        labels = c(
        "1_Multiple families in household",
        "2_Only one family in household",
        "9_Unknown"))) 

  
# val_labels(SA029X <- c(
# 1_Multiple families in household",
# 2_Only one family in household",
# 9_Unknown"
# ))

# write a mutate function to replace the values in the data frame
# for the  variable over65flg_a below with the numeric and value labels for SA030X found below, linked by an underscore, i.e. "1_Mentioned",
# by making it an ordered factor with the levels and labels below

data |> 
  mutate(over65flg_a = ordered(over65flg_a,
        levels = c(1,2),
        labels = c(
        "0_None",
        "1_1 or more")))

# val_labels(SA030X <- c(
# 0_None",
# 1_1 or more",
# ))

# write a mutate function to replace the values in the data frame
# for the  variable notcov_a below with the numeric and value labels for SA031X found below, linked by an underscore, i.e. "1_Mentioned",
# by making it an ordered factor with the levels and labels below

data |> 
  mutate(notcov_a = ordered(notcov_a,
        levels = c(1,2,3,4,5),
        labels = c(
        "1_Not covered",
        "2_Covered",
        "7_Refused",
        "8_Not Ascertained",
        "9_Don't Know")))

# val_labels(SA031X <- c(
# 1_Not covered",
# 2_Covered",
# 7_Refused",
# 8_Not Ascertained",
# 9_Don't Know"
# ))

# write a mutate function to replace the values in the data frame
# for the  variable emdoccupn1_a below with the numeric and value labels for SA032X found below, linked by an underscore, i.e. "1_Mentioned",
# by making it an ordered factor with the levels and labels below

data |> 
  mutate(emdoccupn1_a = ordered(emdoccupn1_a,
        levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97),
        labels = c( 
"01_Chief executives ((general and operations managers)) legislators",
"02_Advertising, marketing, promotions, public relations, and sales managers",
"03_Administrative services, compensation/benefits, human resources, training, production, purchasing, and transportion/distribution, and other operations managers",
"04_All other management occupations",
"05_Business operations specialists",
"06_Financial specialists",
"07_Computer specialists",
"08_Mathematical science occupations",
"09_Architects, surveyors, and cartographers",
"10_Engineers",
"11_Drafters, engineering, and mapping technicians",
"12_Life scientists",
"13_Physical scientists",
"14_Social scientists and related workers",
"15_Life, physical, and social science technicians",
"16_Counselors, social workers, and other community and social service specialists",
"17_Religious workers",
"18_Lawyers, judges, and related workers",
"19_Legal support workers",
"20_Postsecondary teachers",
"21_Primary, secondary, and special education school teachers",
"22_Other teachers and instructors",
"23_Librarians, curators, and archivists",
"24_Other educational instruction and library occupations",
"25_Art and design workers",
"26_Entertainers and performers, sports and related workers",
"27_Media and communication workers",
"28_Media and communication equipment workers",
"29_Health diagnosing and treating practitioners",
"30_Health technologists and technicians",
"31_Other healthcare practitioners and technical occupations",
"32_Nursing, psychiatric, and home health aides",
"33_Occupational and physical therapist assistants and aides",
"34_Other healthcare support occupations",
"35_First-line supervisors/managers, protective service workers",
"36_Fire fighting and prevention workers",
"37_Law enforcement workers",
"38_Other protective service workers",
"39_Supervisors, food preparation and serving workers",
"40_Cooks and food preparation workers",
"41_Food and beverage serving workers",
"42_Other food preparation and serving related workers",
"43_Supervisors, building and grounds cleaning and maintenance workers",
"44_Building cleaning and pest control workers",
"45_Grounds maintenance workers",
"46_Supervisors, personal care and service workers",
"47_Animal care and service workers",
"48_Entertainment attendants and related workers",
"49_Funeral service workers",
"50_Personal appearance workers",
"51_Transportation, tourism, and lodging attendants",
"52_Other personal care and service workers",
"53_Supervisors, sales workers",
"54_Retail sales workers",
"55_Sales representatives, services",
"56_Sales representatives, wholesale and manufacturing",
"57_Other sales and related workers",
"58_Supervisors, office and administrative support workers",
"59_Communications equipment operators",
"60_Financial clerks",
"61_Information and record clerks",
"62_Material recording, scheduling, dispatching, and distributing workers",
"63_Secretaries and administrative assistants",
"64_Other office and administrative support workers",
"65_Supervisors, farming, fishing, and forestry workers",
"66_Agricultural workers",
"67_Fishing and hunting workers",
"68_Forest, conservation, and logging workers",
"69_Supervisors, construction and extraction workers",
"70_Construction trades workers",
"71_Helpers, construction trades",
"72_Other construction and related workers",
"73_Extraction workers",
"74_Supervisors of installation, maintenance, and repair workers",
"75_Electrical and electronic equipment mechanics, installers, and repairers",
"76_Vehicle and mobile equipment mechanics, installers, and repairers",
"77_Other installation, maintenance, and repair occupations",
"78_Supervisors, production workers",
"79_Assemblers and fabricators",
"80_Food processing workers",
"81_Metal workers and plastic workers",
"82_Printing workers",
"83_Textile, apparel, and furnishings workers",
"84_Woodworkers",
"85_Plant and system operators",
"86_Other production occupations",
"87_Supervisors, transportation and material moving workers",
"88_Air transportation workers",
"89_Motor vehicle operators",
"90_Rail transportation workers",
"91_Water transportation workers",
"92_Other transportation workers",
"93_Material moving workers",
"94_Military specific occupations",
"97_Refused, classified",
"98_Not ascertained",
"99_Don't know"
)))

# val_labels(SA032X <- c(
# "01_Chief executives)) general and operations managers)) legislators",
# "02_Advertising, marketing, promotions, public relations, and sales managers",
# "03_Administrative services, compensation/benefits, human resources, training, production, purchasing, and transportion/distribution, and other operations managers",
# "04_All other management occupations",
# "05_Business operations specialists",
# "06_Financial specialists",
# "07_Computer specialists",
# "08_Mathematical science occupations",
# "09_Architects, surveyors, and cartographers",
# "10_Engineers",
# "11_Drafters, engineering, and mapping technicians",
# "12_Life scientists",
# "13_Physical scientists",
# "14_Social scientists and related workers",
# "15_Life, physical, and social science technicians",
# "16_Counselors, social workers, and other community and social service specialists",
# "17_Religious workers",
# "18_Lawyers, judges, and related workers",
# "19_Legal support workers",
# "20_Postsecondary teachers",
# "21_Primary, secondary, and special education school teachers",
# "22_Other teachers and instructors",
# "23_Librarians, curators, and archivists",
# "24_Other educational instruction and library occupations",
# "25_Art and design workers",
# "26_Entertainers and performers, sports and related workers",
# "27_Media and communication workers",
# "28_Media and communication equipment workers",
# "29_Health diagnosing and treating practitioners",
# "30_Health technologists and technicians",
# "31_Other healthcare practitioners and technical occupations",
# "32_Nursing, psychiatric, and home health aides",
# "33_Occupational and physical therapist assistants and aides",
# "34_Other healthcare support occupations",
# "35_First-line supervisors/managers, protective service workers",
# "36_Fire fighting and prevention workers",
# "37_Law enforcement workers",
# "38_Other protective service workers",
# "39_Supervisors, food preparation and serving workers",
# "40_Cooks and food preparation workers",
# "41_Food and beverage serving workers",
# "42_Other food preparation and serving related workers",
# "43_Supervisors, building and grounds cleaning and maintenance workers",
# "44_Building cleaning and pest control workers",
# "45_Grounds maintenance workers",
# "46_Supervisors, personal care and service workers",
# "47_Animal care and service workers",
# "48_Entertainment attendants and related workers",
# "49_Funeral service workers",
# "50_Personal appearance workers",
# "51_Transportation, tourism, and lodging attendants",
# "52_Other personal care and service workers",
# "53_Supervisors, sales workers",
# "54_Retail sales workers",
# "55_Sales representatives, services",
# "56_Sales representatives, wholesale and manufacturing",
# "57_Other sales and related workers",
# "58_Supervisors, office and administrative support workers",
# "59_Communications equipment operators",
# "60_Financial clerks",
# "61_Information and record clerks",
# "62_Material recording, scheduling, dispatching, and distributing workers",
# "63_Secretaries and administrative assistants",
# "64_Other office and administrative support workers",
# "65_Supervisors, farming, fishing, and forestry workers",
# "66_Agricultural workers",
# "67_Fishing and hunting workers",
# "68_Forest, conservation, and logging workers",
# "69_Supervisors, construction and extraction workers",
# "70_Construction trades workers",
# "71_Helpers, construction trades",
# "72_Other construction and related workers",
# "73_Extraction workers",
# "74_Supervisors of installation, maintenance, and repair workers",
# "75_Electrical and electronic equipment mechanics, installers, and repairers",
# "76_Vehicle and mobile equipment mechanics, installers, and repairers",
# "77_Other installation, maintenance, and repair occupations",
# "78_Supervisors, production workers",
# "79_Assemblers and fabricators",
# "80_Food processing workers",
# "81_Metal workers and plastic workers",
# "82_Printing workers",
# "83_Textile, apparel, and furnishings workers",
# "84_Woodworkers",
# "85_Plant and system operators",
# "86_Other production occupations",
# "87_Supervisors, transportation and material moving workers",
# "88_Air transportation workers",
# "89_Motor vehicle operators",
# "90_Rail transportation workers",
# "91_Water transportation workers",
# "92_Other transportation workers",
# "93_Material moving workers",
# "94_Military specific occupations",
# "97_Refused, classified",
# "98_Not ascertained",
# "99_Don't know"
# ))

# write a mutate function to replace the values in the data frame
# for the  variable emdoccupn2_a below with the numeric and value labels for SA033X found below, linked by an underscore, i.e. "1_Mentioned",
# by making it an ordered factor with the levels and labels below

data |> 
  mutate(emdoccupn2_a = factor(emdoccupn2_a, 
        levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26),
        labels = c("01_Management Occupations",
"02_Business and Financial Operations Occupations",
"03_Computer and Mathematical Occupations",
"04_Architecture and Engineering Occupations",
"05_Life, Physical, and Social Science Occupations",
"06_Community and Social Services Occupations",
"07_Legal Occupations",
"08_Educational Instruction and Library Occupations",
"09_Arts, Design, Entertainment, Sports and Media Occupations",
"10_Healthcare Practitioners and Technical Occupations",
"11_Healthcare Support Occupations",
"12_Protective Service Occupations",
"13_Food Preparation and Serving Related Occupations",
"14_Building and Grounds Cleaning and Maintenance Occupations",
"15_Personal Care and Service Occupations",
"16_Sales and Related Occupations",
"17_Office and Administrative Support Occupations",
"18_Farming, Fishing, and Forestry Occupations",
"19_Construction and Extraction Occupations",
"20_Installation, Maintenance, and Repair Occupations",
"21_Production Occupations",
"22_Transportation and Material Moving Occupations",
"23_Military Specific Occupations",
"97_Refused, classified",
"98_Not ascertained",
"99_Don't know")))


# val_labels(SA033X <- c(
# "01_Management Occupations",
# "02_Business and Financial Operations Occupations",
# "03_Computer and Mathematical Occupations",
# "04_Architecture and Engineering Occupations",
# "05_Life, Physical, and Social Science Occupations",
# "06_Community and Social Services Occupations",
# "07_Legal Occupations",
# "08_Educational Instruction and Library Occupations",
# "09_Arts, Design, Entertainment, Sports and Media Occupations",
# "10_Healthcare Practitioners and Technical Occupations",
# "11_Healthcare Support Occupations",
# "12_Protective Service Occupations",
# "13_Food Preparation and Serving Related Occupations",
# "14_Building and Grounds Cleaning and Maintenance Occupations",
# "15_Personal Care and Service Occupations",
# "16_Sales and Related Occupations",
# "17_Office and Administrative Support Occupations",
# "18_Farming, Fishing, and Forestry Occupations",
# "19_Construction and Extraction Occupations",
# "20_Installation, Maintenance, and Repair Occupations",
# "21_Production Occupations",
# "22_Transportation and Material Moving Occupations",
# "23_Military Specific Occupations",
# "97_Refused, classified",
# "98_Not ascertained",
# "99_Don't know"
# ))

# write a mutate function to replace the values in the data frame
# for the  variable ogflg_a below with the numeric and value labels for SA034X found below, linked by an underscore, i.e. "1_Mentioned",
# by making it an ordered factor with the levels and labels below

data |> 
  mutate(ogflg_a = factor(ogflg_a, 
        levels = c(1),
        labels = c("1_Reassigned to other government from private")))


# val_labels(SA034X <- c(
# 1_Reassigned to other government from private"
# ))

# write a mutate function to replace the values in the data frame
# for the  variable orient_a below with the numeric and value labels for SA035X found below, linked by an underscore, i.e. "1_Mentioned",
# by making it an ordered factor with the levels and labels below

data |> 
  mutate(orient_a = factor(orient_a, 
        levels = c(1,2,3,4,5,6,7),
        labels = c("1_GayLesbian",
                  "2_Straight, that is, not ^gaylesbian",
                  "3_Bisexual",
                  "4_Something else",
                  "5_I don't know the answer",
                  "7_Refused",
                  "8_Not Ascertained")))

# val_labels(SA035X <- c(
# 1_^GayLesbian",
# 2_Straight, that is, not ^gaylesbian",
# 3_Bisexual",
# 4_Something else",
# 5_I don't know the answer",
# 7_Refused",
# 8_Not Ascertained"
# ))

# write a mutate function to replace the values in the data frame
# for the  variable opflg_a below with the numeric and value labels for SA036X found below, linked by an underscore, i.e. "1_Mentioned",
# by making it an ordered factor with the levels and labels below

data |> 
  mutate(opflg_a = factor(opflg_a, 
        levels = c(1),
        labels = c("1_Reassigned to other public from private")))

# val_labels(SA036X <- c(
# 1_Reassigned to other public from private"
# ))

# write a mutate function to replace the values in the data frame
# for the  variable parstat_a below with the numeric and value labels for SA037X found below, linked by an underscore, i.e. "1_Mentioned",
# by making it an ordered factor with the levels and labels below

data |> 
  mutate(parstat_a = factor(parstat_a, 
        levels = c(1,2,3,4),
        labels = c(
        "1_Yes, the sample adult is a parent of a child residing in the family",
        "2_There are minor children residing in family but sample adult is not their parent",
        "3_There are no minor children residing in the family",
        "9_Unknown")))

# val_labels(SA037X <- c(
# 1_Yes, the sample adult is a parent of a child residing in the family",
# 2_There are minor children residing in family but sample adult is not their parent",
# 3_There are no minor children residing in the family",
# 9_Unknown"
# ))

# write a mutate function to replace the values in the data frame
# for the  variable pcnt18uptc below with the numeric and value labels for SA038X found below, linked by an underscore, i.e. "1_Mentioned",
# by making it an ordered factor with the levels and labels below

data |> 
  mutate(pcnt18uptc = factor(pcnt18uptc, 
        levels = c(1,2,3,4,5),
        labels = c(
        "0_0 adults",
        "1_1 adult",
        "2_2 adults",
        "3_3+ adults",
        "8_Not Ascertained")))

# val_labels(SA038X <- c(
# 0_0 adults",
# 1_1 adult",
# 2_2 adults",
# 3_3+ adults",
# 8_Not Ascertained"
# ))

# write a mutate function to replace the values in the data frame
# for the  variable pcntadlt_a below with the numeric and value labels for SA039X found below, linked by an underscore, i.e. "1_Mentioned",
# by making it an ordered factor with the levels and labels below

data |> 
  mutate(pcntadlt_a = factor(pcntadlt_a, 
        levels = c(1,2,3,4),
        labels = c(
        "1_1 adult",
        "2_2 adults",
        "3_3+ adults",
        "8_Not Ascertained")))

# val_labels(SA039X <- c(
# 1_1 adult",
# 2_2 adults",
# 3_3+ adults",
# 8_Not Ascertained"
# ))

# write a mutate function to replace the values in the data frame
# for the  variable pcntadwkp1_a and pcntadwfp1_a below with the numeric and value labels for SA040X found below, linked by an underscore, i.e. "1_Mentioned",
# by making it an ordered factor with the levels and labels below

data |> 
  mutate(pcntadwkp1_a = factor(pcntadwkp1_a, 
        levels = c(1,2,3,4,5,6,7,8),
        labels = c(
        "0_0 adults",
        "1_1 adult",
        "2_2 adults",
        "3_3+ adults (at least 3, may have unknown)",
        "5_at least 2 adults, has unknown",
        "6_at least 1 adult, has unknown",
        "7_0 adults, has unknown",
        "8_Not Ascertained"))) |> 
  mutate(pcntadwfp1_a = factor(pcntadwfp1_a,
        levels = c(1,2,3,4,5,6,7,8),
        labels = c(
        "0_0 adults",
        "1_1 adult",
        "2_2 adults",
        "3_3+ adults (at least 3, may have unknown)",
        "5_at least 2 adults, has unknown",
        "6_at least 1 adult, has unknown",
        "7_0 adults, has unknown",
        "8_Not Ascertained")))

# val_labels(SA040X <- c(
# 0_0 adults",
# 1_1 adult",
# 2_2 adults",
# 3_3+ adults (at least 3, may have unknown)",
# 5_at least 2 adults, has unknown",
# 6_at least 1 adult, has unknown",
# 7_0 adults, has unknown",
# 8_not ascertained (all unknown)"
# ))

# write a mutate function to replace the values in the data frame
# for the  variable pcntlt18tc and pcntkids_a below with the numeric and value labels for SA041X found below, linked by an underscore, i.e. "1_Mentioned",
# by making it an ordered factor with the levels and labels below

data |> 
  mutate(pcntlt18tc = factor(pcntlt18tc, 
        levels = c(1,2,3,4,5),
        labels = c(
        "0_0 children",
        "1_1 child",
        "2_2 children",
        "3_3+ children",
        "8_Not Ascertained"))) |> 
  mutate(pcntkids_a = factor(pcntkids_a,
        levels = c(1,2,3,4,5),
        labels = c(
        "0_0 children",
        "1_1 child",
        "2_2 children",
        "3_3+ children",
        "8_Not Ascertained")))
                        
# val_labels(SA041X <- c(
# 0_0 children",
# 1_1 child",
# 2_2 children",
# 3_3+ children",
# 8_Not Ascertained"
# ))

# write a mutate function to replace the values in the data frame
# for the  variable plnwrkr1_a and plnwrkr2_a below with the numeric and value labels for SA042X found below, linked by an underscore, i.e. "1_Mentioned",
# by making it an ordered factor with the levels and labels below

data |> 
  mutate(plnwrkr1_a = factor(plnwrkr1_a, 
        levels = c(1,2,3,4,5,6,7,8,9,10,11),
        labels = c(
        "1_Through an employer, union, or professional organization",
        "2_Purchased directly",
        "3_Through Healthcare.gov or the Affordable Care Act, also known as Obamacare",
        "4_Through a state or local government or community program",
        "5_Other",
        "6_Through school",
        "7_Through parents",
        "8_Through other relative",
        "97_Refused",
        "98_Not Ascertained",
        "99_Don't Know"))) |> 
  mutate(plnwrkr2_a = factor(plnwrkr2_a,
        levels = c(1,2,3,4,5,6,7,8,9,10,11),
        labels = c(
        "1_Through an employer, union, or professional organization",
        "2_Purchased directly",
        "3_Through Healthcare.gov or the Affordable Care Act, also known as Obamacare",
        "4_Through a state or local government or community program",
        "5_Other",
        "6_Through school",
        "7_Through parents",
        "8_Through other relative",
        "97_Refused",
        "98_Not Ascertained",
        "99_Don't Know")))

# val_labels(SA042X <- c(
# "01_Through an employer, union, or professional organization",
# "02_Purchased directly",
# "03_Through Healthcare.gov or the Affordable Care Act, also known as Obamacare",
# "04_Through a state or local government or community program",
# "05_Other",
# "06_Through school",
# "07_Through parents",
# "08_Through other relative",
# "97_Refused",
# "98_Not Ascertained",
# "99_Don't Know"
# ))

# write a mutate function to replace the values in the data frame
# for the  variable prflg_a below with the numeric and value labels for SA043X found below, linked by an underscore, i.e. "1_Mentioned",
# by making it an ordered factor with the levels and labels below

data |> 
  mutate(prflg_a = factor(prflg_a, 
        levels = c(1),
        labels = c(
        "1_Reassigned to private from public")))

# val_labels(SA043X <- c(
# 1_Reassigned to private from public"
# ))

# write a mutate function to replace the values in the data frame
# for the  variable prflg_a below with the numeric and value labels for SA043X found below, linked by an underscore, i.e. "1_Mentioned",
# by making it an ordered factor with the levels and labels below

data |> 
  mutate(mahdhp_a = factor(mahdhp_a, 
        levels = c(1,2,3,4,5),
        labels = c(
        "1_Deductible is less than $1,500",
        "2_Deductible is $1,500 or more",
        "7_Refused",
        "8_Not Ascertained",
        "9_Don't Know"))) |> 
  mutate(chhdhp_a = factor(chhdhp_a,
        levels = c(1,2,3,4,5),
        labels = c(
        "1_Deductible is less than $1,500",
        "2_Deductible is $1,500 or more",
        "7_Refused",
        "8_Not Ascertained",
        "9_Don't Know"))) |> 
  mutate(oghdhp_a = factor(oghdhp_a,
        levels = c(1,2,3,4,5),
        labels = c(
        "1_Deductible is less than $1,500",
        "2_Deductible is $1,500 or more",
        "7_Refused",
        "8_Not Ascertained",
        "9_Don't Know"))) |>
  mutate(ophdhp_a = factor(ophdhp_a,
      levels = c(1,2,3,4,5),
      labels = c(
      "1_Deductible is less than $1,500",
      "2_Deductible is $1,500 or more",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know")))

# val_labels(SA044X <- c(
# 1_Deductible is less than $1,500",
# 2_Deductible is $1,500 or more",
# 7_Refused",
# 8_Not Ascertained",
# 9_Don't Know"
# ))

data |> 
  mutate(raceallp_a = ordered(raceallp_a, 
        levels = c(1,2,3,4,5,6,7,8,9),
        labels = c(
        "1_White only",
        "2_Black/African American only",
        "3_Asian only",
        "4_AIAN only",
        "5_AIAN and any other group",
        "6_Other single and multiple",
        "7_Refused",
        "8_Not Ascertained",
        "9_Don't know"))) 
        
# val_labels(SA045X <- c(
# 1_White only",
# 2_Black/African American only",
# 3_Asian only",
# 4_AIAN only",
# 5_AIAN and any other group",
# 6_Other single and multiple races",
# 7_Refused",
# 8_Not Ascertained",
# 9_Don't know"
# ))

data |> 
  mutate(ratcat_a = ordered(ratcat_a, 
        levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15),
        labels = c(
          "01_0.00 - 0.49",
          "02_0.50 - 0.74",
          "03_0.75 - 0.99",
          "04_1.00 - 1.24",
          "05_1.25 - 1.49",
          "06_1.50 - 1.74",
          "07_1.75 - 1.99",
          "08_2.00 - 2.49",
          "09_2.50 - 2.99",
          "10_3.00 - 3.49",
          "11_3.50 - 3.99",
          "12_4.00 - 4.49",
          "13_4.50 - 4.99",
          "14_5.00 or greater",
          "98_Not Ascertained")))

# val_labels(SA046X <- c(
# "01_0.00 - 0.49",
# "02_0.50 - 0.74",
# "03_0.75 - 0.99",
# "04_1.00 - 1.24",
# "05_1.25 - 1.49",
# "06_1.50 - 1.74",
# "07_1.75 - 1.99",
# "08_2.00 - 2.49",
# "09_2.50 - 2.99",
# "10_3.00 - 3.49",
# "11_3.50 - 3.99",
# "12_4.00 - 4.49",
# "13_4.50 - 4.99",
# "14_5.00 or greater",
# "98_Not Ascertained"
# ))

# write a mutate function to replace the values in the data frame
# for the  variable rectype below with the numeric and value labels for SA047X found below, linked by an underscore, i.e. "1_Mentioned",
# by making it an ordered factor with the levels and labels below

data |> 
  mutate(rectype = ordered(rectype, 
        levels = c(1,2,3,4,5),
        labels = c(
          "10_Sample Adult",
          "20_Sample Child",
          "30_Sample Adult Income",
          "40_Sample Child Income",
          "50_Paradata")))

# val_labels(SA047X <- c(
# "10_Sample Adult",
# "20_Sample Child",
# "30_Sample Adult Income",
# "40_Sample Child Income",
# "50_Paradata"
# ))

# write a mutate function to replace the values in the data frame
# for the  variable region below with the numeric and value labels for SA048X found below, linked by an underscore, i.e. "1_Mentioned",
# by making it an ordered factor with the levels and labels below

data |> 
  mutate(region = ordered(region, 
        levels = c(1,2,3,4),
        labels = c(
          "1_Northeast",
          "2_Midwest",
          "3_South",
          "4_West")))

# val_labels(SA048X <- c(
# 1_Northeast",
# 2_Midwest",
# 3_South",
# 4_West"
# ))

# write a mutate function to replace the values in the data frame
# for the  variable saparentsc_a below with the numeric and value labels for SA049X found below, linked by an underscore, i.e. "1_Mentioned",
# by making it an ordered factor with the levels and labels below

data |> 
  mutate(saparentsc_a = ordered(saparentsc_a, 
        levels = c(1,2,3,4),
        labels = c(
          "1_Sample adult is parent of sample child",
          "2_Sample adult is not parent of  sample child",
          "3_No sample child in sample adult's family",
          "9_Unknown")))

# val_labels(SA049X <- c(
# 1_Sample adult is parent of sample child",
# 2_Sample adult is not parent of  sample child",
# 3_No sample child in sample adult's family",
# 9_Unknown"
# ))

# write a mutate function to replace the values in the data frame
# for the  variable smkcigst_a below with the numeric and value labels for SA050X found below, linked by an underscore, i.e. "1_Mentioned",
# by making it an ordered factor with the levels and labels below

data |> 
  mutate(smkcigst_a = ordered(smkcigst_a, 
        levels = c(1,2,3,4,5,6),
        labels = c(
          "1_Current every day smoker",
          "2_Current some day smoker",
          "3_Former smoker",
          "4_Never smoker",
          "5_Smoker, current status unknown",
          "9_Unknown if ever smoked")))

# val_labels(SA050X <- c(
# 1_Current every day smoker",
# 2_Current some day smoker",
# 3_Former smoker",
# 4_Never smoker",
# 5_Smoker, current status unknown",
# 9_Unknown if ever smoked"
# ))

# write a mutate function to replace the values in the data frame
# for the  variable urgnt12mtc_a below with the numeric and value labels for SA051X found below, linked by an underscore, i.e. "1_Mentioned",
# by making it an ordered factor with the levels and labels below

data |> 
  mutate(urgnt12mtc_a = ordered(urgnt12mtc_a, 
        levels = c(1,2,3,4,5,7,8,9,10),
        labels = c(
          "0_0 times",
          "1_1 time",
          "2_2 times",
          "3_3 times",
          "4_4 times",
          "5_5+ times",
          "7_Refused",
          "8_Not Ascertained",
          "9_Don't Know")))

# val_labels(SA051X <- c(
# 0_0 times",
# 1_1 time",
# 2_2 times",
# 3_3 times",
# 4_4 times",
# 5_5+ times",
# 7_Refused",
# 8_Not Ascertained",
# 9_Don't Know"
# ))

# write a mutate function to replace the values in the data frame
# for the  variable urbrrl below with the numeric and value labels for SA052X found below, linked by an underscore, i.e. "1_Mentioned",
# by making it an ordered factor with the levels and labels below

data |> 
  mutate(urbrrl = ordered(urbrrl, 
        levels = c(1,2,3,4),
        labels = c(
          "1_Large central metro",
          "2_Large fringe metro",
          "3_Medium and small metro",
          "4_Nonmetropolitan")))

# val_labels(SA052X <- c(
# 1_Large central metro",
# 2_Large fringe metro",
# 3_Medium and small metro",
# 4_Nonmetropolitan"
# ))

# write a mutate function to replace the values in the data frame
# for the  variable disab3_a below with the numeric and value labels for SA053X found below, linked by an underscore, i.e. "1_Mentioned",
# by making it an ordered factor with the levels and labels below

data |> 
  mutate(disab3_a = ordered(disab3_a, 
        levels = c(1,2,3),
        labels = c(
          "1_Yes",
          "2_No",
          "9_Don't Know")))

# val_labels(SA053X <- c(
# 1_Yes",
# 2_No",
# 9_Don't Know"
# ))

# write a mutate function to replace the values in the data frame
# for the  variable inctcflg_a below with the numeric and value labels for SA054X found below, linked by an underscore, i.e. "1_Mentioned",
# by making it an ordered factor with the levels and labels below

data |> 
  mutate(inctcflg_a = ordered(inctcflg_a, 
        levels = c(1,2),
        labels = c(
          "0_No",
          "1_Yes")))

# val_labels(SA054X <- c(
# 0_No",
# 1_Yes"
# ))

# write a mutate function to replace the values in the data frame
# for the  variable saspprace_a and saspphisp_a below with the numeric and value labels for SA054X found below, linked by an underscore, i.e. "1_Mentioned",
# by making it an ordered factor with the levels and labels below

data |> 
  mutate(saspprace_a = ordered(saspprace_a, 
        levels = c(1,2,3),
        labels = c(
          "1_Yes",
          "2_No",
          "3_Unknown"))) |> 
  mutate(saspphisp_a = ordered(saspphisp_a,
        levels = c(1,2,3),
        labels = c(
          "1_Yes",
          "2_No",
          "3_Unknown")))

# val_labels(SA055X <- c(
# 1_Yes",
# 2_No",
# 3_Unknown"
# ))

# write a mutate function to replace the values in the data frame
# for the  variable hhrespsa_flg below with the numeric and value labels for SA056X found below, linked by an underscore, i.e. "1_Mentioned",
# by making it an ordered factor with the levels and labels below

data |> 
  mutate(hhrespsa_flg = ordered(hhrespsa_flg, 
        levels = c(1),
        labels = c(
          "1_Yes")))

# val_labels(SA056X <- c(
# 1_Yes"
# ))

# write a mutate function to replace the values in the data frame
# for the  variable yrsinus_a below with the numeric and value labels for SA057X found below, linked by an underscore, i.e. "1_Mentioned",
# by making it an ordered factor with the levels and labels below

data |> 
  mutate(yrsinus_a = ordered(yrsinus_a, 
        levels = c(1,2,3,4,5,6),
        labels = c(
          "1_Less than 1 year",
          "2_1 to less than 5 years",
          "3_5 to less than 10 years",
          "4_10 to less than 15 years",
          "5_15 years or more",
          "9_Unknown")))

# val_labels(SA057X <- c(
# 1_Less than 1 year",
# 2_1 to less than 5 years",
# 3_5 to less than 10 years",
# 4_10 to less than 15 years",
# 5_15 years or more",
# 9_Unknown"
# ))

# write a mutate function to replace the values in the data frame
# for the  variable age65 below with the numeric and value labels for SA058X found below, linked by an underscore, i.e. "1_Mentioned",
# by making it an ordered factor with the levels and labels below

data |> 
  mutate(age65 = ordered(age65, 
        levels = c(1,2,3,4,5),
        labels = c(
          "1_Less than 65",
          "2_65 or older",
          "7_Refused",
          "8_Not Ascertained",
          "9_Don't Know")))

# val_labels(SA058X <- c(
# 1_Less than 65",
# 2_65 or older",
# 7_Refused",
# 8_Not Ascertained",
# 9_Don't Know"
# ))

# write a mutate function to replace the values in the data frame
# for the  variable payworry_a below with the numeric and value labels for SA058X found below, linked by an underscore, i.e. "1_Mentioned",
# by making it an ordered factor with the levels and labels below

data |> 
  mutate(payworry_a = ordered(payworry_a, 
        levels = c(1,2,3,4,5,6),
        labels = c(
          "1_Very worried",
          "2_Somewhat worried",
          "3_Not at all worried",
          "7_Refused",
          "8_Not Ascertained",
          "9_Don't Know")))

# val_labels(SA059X <- c(
# 1_Very worried",
# 2_Somewhat worried",
# 3_Not at all worried",
# 7_Refused",
# 8_Not Ascertained",
# 9_Don't Know"
# ))

# write a mutate function to replace the values in the data frame
# for the  variable baldprob_a and hrtinprob_a below with the numeric and value labels for SA060X found below, linked by an underscore, i.e. "1_Mentioned",
# by making it an ordered factor with the levels and labels below

data |> 
  mutate(baldprob_a = ordered(baldprob_a, 
        levels = c(1,2,3,4,5,6,7,8),
        labels = c(
          "1_No problem",
          "2_A small problem",
          "3_A moderate problem",
          "4_A big problem",
          "5_A very big problem",
          "7_Refused",
          "8_Not Ascertained",
          "9_Don't Know"))) |> 
  mutate(hrtinprob_a = ordered(hrtinprob_a,
        levels = c(1,2,3,4,5,6,7,8),
        labels = c(
          "1_No problem",
          "2_A small problem",
          "3_A moderate problem",
          "4_A big problem",
          "5_A very big problem",
          "7_Refused",
          "8_Not Ascertained",
          "9_Don't Know")))

# val_labels(SA060X <- c(
# 1_No problem",
# 2_A small problem",
# 3_A moderate problem",
# 4_A big problem",
# 5_A very big problem",
# 7_Refused",
# 8_Not Ascertained",
# 9_Don't Know"
# ))

# write a mutate function to replace the values in the data frame
# for the  variable cogamtdff_a  below with the numeric and value labels for SA061X found below, linked by an underscore, i.e. "1_Mentioned",
# by making it an ordered factor with the levels and labels below

data |> 
  mutate(cogamtdff_a = ordered(cogamtdff_a, 
        levels = c(1,2,3,4,5,6),
        labels = c(
          "1_A few things",
          "2_A lot of things",
          "3_Almost everything",
          "7_Refused",
          "8_Not Ascertained",
          "9_Don't Know")))

# val_labels(SA061X <- c(
# 1_A few things",
# 2_A lot of things",
# 3_Almost everything",
# 7_Refused",
# 8_Not Ascertained",
# 9_Don't Know"
# ))

# write a mutate function to replace the values in the data frame
# for the  variable cogfrqdff_a  below with the numeric and value labels for SA062X found below, linked by an underscore, i.e. "1_Mentioned",
# by making it an ordered factor with the levels and labels below

data |> 
  mutate(cogfrqdff_a = ordered(cogfrqdff_a, 
        levels = c(1,2,3,4,5,6),
        labels = c(
          "1_Sometimes",
          "2_Often",
          "3_All of the time",
          "7_Refused",
          "8_Not Ascertained",
          "9_Don't Know")))

# val_labels(SA062X <- c(
# 1_Sometimes",
# 2_Often",
# 3_All of the time",
# 7_Refused",
# 8_Not Ascertained",
# 9_Don't Know"
# ))

# write a mutate function to replace the values in the data frame
# for the  variable cogtypedff_a  below with the numeric and value labels for SA063X found below, linked by an underscore, i.e. "1_Mentioned",
# by making it an ordered factor with the levels and labels below

data |> 
  mutate(cogtypedff_a = ordered(cogtypedff_a, 
        levels = c(1,2,3,4,5,6),
        labels = c(
          "1_Difficulty remembering only",
          "2_Difficulty concentrating only",
          "3_Difficulty with both remembering and concentrating",
          "7_Refused",
          "8_Not Ascertained",
          "9_Don't Know")))

# val_labels(SA063X <- c(
# 1_Difficulty remembering only",
# 2_Difficulty concentrating only",
# 3_Difficulty with both remembering and concentrating",
# 7_Refused",
# 8_Not Ascertained",
# 9_Don't Know"
# ))

# write a mutate function to replace the values in the data frame
# for the  variable colorectyp_a  below with the numeric and value labels for SA064X found below, linked by an underscore, i.e. "1_Mentioned",
# by making it an ordered factor with the levels and labels below

data |> 
  mutate(colorectyp_a = ordered(colorectyp_a, 
        levels = c(1,2,3,4,5,6),
        labels = c(
          "1_Colonoscopy",
          "2_Sigmoidoscopy",
          "3_Both",
          "7_Refused",
          "8_Not Ascertained",
          "9_Don't Know")))

# val_labels(SA064X <- c(
# 1_Colonoscopy",
# 2_Sigmoidoscopy",
# 3_Both",
# 7_Refused",
# 8_Not Ascertained",
# 9_Don't Know"
# ))

# write a mutate function to replace the values in the data frame
# for the  variable colreason1_a  below with the numeric and value labels for SA065X found below, linked by an underscore, i.e. "1_Mentioned",
# by making it an ordered factor with the levels and labels below

data |> 
  mutate(colreason1_a = ordered(colreason1_a, 
        levels = c(1,2,3,4,5,6,7),
        labels = c(
          "1_Part of a routine exam",
          "2_Because of a problem",
          "3_Follow-up test of an earlier test or screening exam",
          "4_Other reason",
          "7_Refused",
          "8_Not Ascertained",
          "9_Don't Know")))

# val_labels(SA065X <- c(
# 1_Part of a routine exam",
# 2_Because of a problem",
# 3_Follow-up test of an earlier test or screening exam",
# 4_Other reason",
# 7_Refused",
# 8_Not Ascertained",
# 9_Don't Know"
# ))

# write a mutate function to replace the values in the data frame
# for the  variable shtcvd19nm1_a  below with the numeric and value labels for SA066X found below, linked by an underscore, i.e. "1_Mentioned",
# by making it an ordered factor with the levels and labels below

data |> 
  mutate(shtcvd19nm1_a = ordered(shtcvd19nm1_a, 
        levels = c(1,2,3,4,5,6,7,8,9),
        labels = c(
          "1_1 vaccination",
          "2_2 vaccinations",
          "3_3 vaccinations",
          "4_4 vaccinations",
          "5_5 vaccinations",
          "6_6 or more vaccinations",
          "7_Refused",
          "8_Not Ascertained",
          "9_Don't Know")))

# val_labels(SA066X <- c(
# 1_1 vaccination",
# 2_2 vaccinations",
# 3_3 vaccinations",
# 4_4 vaccinations",
# 5_5 vaccinations",
# 6_6 or more vaccinations",
# 7_Refused",
# 8_Not Ascertained",
# 9_Don't Know"
# ))

# write a mutate function to replace the values in the data frame
# for the  variable shottype2_a  below with the numeric and value labels for SA067X found below, linked by an underscore, i.e. "1_Mentioned",
# by making it an ordered factor with the levels and labels below

data |> 
  mutate(shottype2_a = ordered(shottype2_a, 
        levels = c(1,2,3,4,5,6,7,8,9),
        labels = c(
          "1_Pfizer-BioNTech Comirnaty? shot",
          "2_Moderna Spikevax? shot",
          "3_Johnson and Johnson (Janssen) shot",
          "4_Novavax shot",
          "5_One of the brands that requires two initial shots, but not sure which brand",
          "6_None of these brands",
          "7_Refused",
          "8_Not Ascertained",
          "9_Don't Know")))

# val_labels(SA067X <- c(
# 1_Pfizer-BioNTech Comirnaty? shot",
# 2_Moderna Spikevax? shot",
# 3_Johnson and Johnson (Janssen) shot",
# 4_Novavax shot",
# 5_One of the brands that requires two initial shots, but not sure which brand",
# 6_None of these brands",
# 7_Refused",
# 8_Not Ascertained",
# 9_Don't Know"
# ))

# write a mutate function to replace the values in the data frame
# for the  variable smk30d_a and cigar30d_a below with the numeric and value labels for SA068X found below, linked by an underscore, i.e. "1_Mentioned",
# by making it an ordered factor with the levels and labels below

data |> 
  mutate(smk30d_a = ordered(smk30d_a, 
        levels = c(1,2,3),
        labels = c("97_Refused",
        "98_Not Ascertained",
        "99_Don't Know"))) |> 
  mutate(cigar30d_a = ordered(cigar30d_a,
        levels = c(1,2,3),
        labels = c("97_Refused",
                    "98_Not Ascertained",
                    "99_Don't Know")))

# val_labels(SA068X <- c(
# "97_Refused",
# "98_Not Ascertained",
# "99_Don't Know"
# ))

# write a mutate function to replace the values in the data frame
# for the  variable fdsskipdys_a and fdsnedays_a below with the numeric and value labels for SA069X found below, linked by an underscore, i.e. "1_Mentioned",
# by making it an ordered factor with the levels and labels below

data |> 
  mutate(fdsskipdys_a = ordered(fdsskipdys_a, 
        levels = c(1,2,3),
        labels = c("97_Refused",
        "98_Not Ascertained",
        "99_Don't Know"))) |> 
  mutate(fdsnedays_a = ordered(fdsnedays_a,
        levels = c(1,2,3),
        labels = c("97_Refused",
                    "98_Not Ascertained",
                    "99_Don't Know")))

# val_labels(SA069X <- c(
# 97_Refused",
# 98_Not Ascertained",
# 99_Don't Know"
# ))

# write a mutate function to replace the values in the data frame
# for the  variable cignow_a and cig30d_a below with the numeric and value labels for SA070X found below, linked by an underscore, i.e. "1_Mentioned",
# by making it an ordered factor with the levels and labels below

data |> 
  mutate(cignow_a = ordered(cignow_a, 
        levels = c(1,2,3),
        labels = c("97_Refused",
        "98_Not Ascertained",
        "99_Don't Know"))) |> 
  mutate(cig30d_a = ordered(cig30d_a,
        levels = c(1,2,3),
        labels = c("97_Refused",
                    "98_Not Ascertained",
                    "99_Don't Know")))

# val_labels(SA070X <- c(
# 97_Refused",
# 98_Not Ascertained",
# 99_Don't Know"
# ))

# write a mutate function to replace the values in the data frame
# for the  variable dibinstime_a  below with the numeric and value labels for SA071X found below, linked by an underscore, i.e. "1_Mentioned",
# by making it an ordered factor with the levels and labels below

data |> 
  mutate(dibinstime_a = ordered(dibinstime_a, 
        levels = c(1,2,3,4,5,6,7),
        labels = c(
          "1_Less than 1 month",
          "2_1 month to less than 6 months",
          "3_6 months to less than 1 year",
          "4_1 year or more",
          "7_Refused",
          "8_Not Ascertained",
          "9_Don't Know")))

# val_labels(SA071X <- c(
# 1_Less than 1 month",
# 2_1 month to less than 6 months",
# 3_6 months to less than 1 year",
# 4_1 year or more",
# 7_Refused",
# 8_Not Ascertained",
# 9_Don't Know"
# ))

# write a mutate function to replace the values in the data frame
# for the  variable dibtype_a  below with the numeric and value labels for SA072X found below, linked by an underscore, i.e. "1_Mentioned",
# by making it an ordered factor with the levels and labels below

data |> 
  mutate(dibtype_a = ordered(dibtype_a, 
        levels = c(1,2,3,4,5,6),
        labels = c(
          "1_Type 1",
          "2_Type 2",
          "3_Other type of diabetes",
          "7_Refused",
          "8_Not Ascertained",
          "9_Don't Know")))

# val_labels(SA072X <- c(
# 1_Type 1",
# 2_Type 2",
# 3_Other type of diabetes",
# 7_Refused",
# 8_Not Ascertained",
# 9_Don't Know"
# ))

# write a mutate function to replace the values in the data frame
# for the  variable visiondf_a and hearingdf_a  below with the numeric and value labels for SA073X found below, linked by an underscore, i.e. "1_Mentioned",
# by making it an ordered factor with the levels and labels below

data |> 
  mutate(visiondf_a = ordered(visiondf_a, 
        levels = c(1,2,3,4,5,6,7),
        labels = c(
          "1_No difficulty",
          "2_Some difficulty",
          "3_A lot of difficulty",
          "4_Cannot do at all",
          "7_Refused",
          "8_Not Ascertained",
          "9_Don't Know"))) |> 
  mutate(hearingdf_a = ordered(hearingdf_a,
        levels = c(1,2,3,4,5,6,7),
        labels = c(
          "1_No difficulty",
          "2_Some difficulty",
          "3_A lot of difficulty",
          "4_Cannot do at all",
          "7_Refused",
          "8_Not Ascertained",
          "9_Don't Know"))) |> 
  mutate(diff_a = ordered(diff_a,
        levels = c(1,2,3,4,5,6,7),
        labels = c(
          "1_No difficulty",
          "2_Some difficulty",
          "3_A lot of difficulty",
          "4_Cannot do at all",
          "7_Refused",
          "8_Not Ascertained",
          "9_Don't Know"))) |>
  mutate(wlk100_a = ordered(wlk100_a,
        levels = c(1,2,3,4,5,6,7),
        labels = c(
          "1_No difficulty",
          "2_Some difficulty",
          "3_A lot of difficulty",
          "4_Cannot do at all",
          "7_Refused",
          "8_Not Ascertained",
          "9_Don't Know"))) |>
  mutate(wlk13m_a = ordered(wlk13m_a,
        levels = c(1,2,3,4,5,6,7),
        labels = c(
          "1_No difficulty",
          "2_Some difficulty",
          "3_A lot of difficulty",
          "4_Cannot do at all",
          "7_Refused",
          "8_Not Ascertained",
          "9_Don't Know"))) |>
  mutate(steps_a = ordered(steps_a,
        levels = c(1,2,3,4,5,6,7),
        labels = c(
          "1_No difficulty",
          "2_Some difficulty",
          "3_A lot of difficulty",
          "4_Cannot do at all",
          "7_Refused",
          "8_Not Ascertained",
          "9_Don't Know"))) |>
  mutate(noeqwlk100_a = ordered(noeqwlk100_a,
        levels = c(1,2,3,4,5,6,7),
        labels = c(
          "1_No difficulty",
          "2_Some difficulty",
          "3_A lot of difficulty",
          "4_Cannot do at all",
          "7_Refused",
          "8_Not Ascertained",
          "9_Don't Know"))) |>
  mutate(noeqwlk13m_a = ordered(noeqwlk13m_a,
        levels = c(1,2,3,4,5,6,7),
        labels = c(
          "1_No difficulty",
          "2_Some difficulty",
          "3_A lot of difficulty",
          "4_Cannot do at all",
          "7_Refused",
          "8_Not Ascertained",
          "9_Don't Know"))) |>
  mutate(noeqsteps_a = ordered(noeqsteps_a,
        levels = c(1,2,3,4,5,6,7),
        labels = c(
          "1_No difficulty",
          "2_Some difficulty",
          "3_A lot of difficulty",
          "4_Cannot do at all",
          "7_Refused",
          "8_Not Ascertained",
          "9_Don't Know"))) |>
  mutate(eqwlk100_a = ordered(eqwlk100_a,
        levels = c(1,2,3,4,5,6,7),
        labels = c(
          "1_No difficulty",
          "2_Some difficulty",
          "3_A lot of difficulty",
          "4_Cannot do at all",
          "7_Refused",
          "8_Not Ascertained",
          "9_Don't Know"))) |>
  mutate(eqwlk13m_a = ordered(eqwlk13m_a,
        levels = c(1,2,3,4,5,6,7),
        labels = c(
          "1_No difficulty",
          "2_Some difficulty",
          "3_A lot of difficulty",
          "4_Cannot do at all",
          "7_Refused",
          "8_Not Ascertained",
          "9_Don't Know"))) |>
  mutate(eqsteps_a = ordered(eqsteps_a,
        levels = c(1,2,3,4,5,6,7),
        labels = c(
          "1_No difficulty",
          "2_Some difficulty",
          "3_A lot of difficulty",
          "4_Cannot do at all",
          "7_Refused",
          "8_Not Ascertained",
          "9_Don't Know"))) |>
  mutate(comdiff_a = ordered(comdiff_a,
        levels = c(1,2,3,4,5,6,7),
        labels = c(
          "1_No difficulty",
          "2_Some difficulty",
          "3_A lot of difficulty",
          "4_Cannot do at all",
          "7_Refused",
          "8_Not Ascertained",
          "9_Don't Know"))) |>
  mutate(cogmemdff_a = ordered(cogmemdff_a,
        levels = c(1,2,3,4,5,6,7),
        labels = c(
          "1_No difficulty",
          "2_Some difficulty",
          "3_A lot of difficulty",
          "4_Cannot do at all",
          "7_Refused",
          "8_Not Ascertained",
          "9_Don't Know"))) |>
  mutate(uppslfcr_a = ordered(uppslfcr_a,
        levels = c(1,2,3,4,5,6,7),
        labels = c(
          "1_No difficulty",
          "2_Some difficulty",
          "3_A lot of difficulty",
          "4_Cannot do at all",
          "7_Refused",
          "8_Not Ascertained",
          "9_Don't Know"))) |>
  mutate(uppraise_a = ordered(uppraise_a,
        levels = c(1,2,3,4,5,6,7),
        labels = c(
          "1_No difficulty",
          "2_Some difficulty",
          "3_A lot of difficulty",
          "4_Cannot do at all",
          "7_Refused",
          "8_Not Ascertained",
          "9_Don't Know"))) |>
  mutate(uppobjct_a = ordered(uppobjct_a,
        levels = c(1,2,3,4,5,6,7),
        labels = c(
          "1_No difficulty",
          "2_Some difficulty",
          "3_A lot of difficulty",
          "4_Cannot do at all",
          "7_Refused",
          "8_Not Ascertained",
          "9_Don't Know"))) |>
  mutate(socerrnds_a = ordered(socerrnds_a,
        levels = c(1,2,3,4,5,6,7),
        labels = c(
          "1_No difficulty",
          "2_Some difficulty",
          "3_A lot of difficulty",
          "4_Cannot do at all",
          "7_Refused",
          "8_Not Ascertained",
          "9_Don't Know"))) |>
  mutate(socsclpar_a = ordered(socsclpar_a,
        levels = c(1,2,3,4,5,6,7),
        labels = c(
          "1_No difficulty",
          "2_Some difficulty",
          "3_A lot of difficulty",
          "4_Cannot do at all",
          "7_Refused",
          "8_Not Ascertained",
          "9_Don't Know")))

# val_labels(SA073X <- c(
# 1_No difficulty",
# 2_Some difficulty",
# 3_A lot of difficulty",
# 4_Cannot do at all",
# 7_Refused",
# 8_Not Ascertained",
# 9_Don't Know"
# ))

# write a mutate function to replace the values in the data frame
# for the  variable educp_a and maxeducp_a  below with the numeric and value labels for SA074X found below, linked by an underscore, i.e. "1_Mentioned",
# by making it an ordered factor with the levels and labels below

mutate(educp_a = ordered(educp_a,
  levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14),
  labels = c(
    "00_Never attended/kindergarten only",
    "01_Grade 1-11",
    "02_12th grade, no diploma",
    "03_GED or equivalent",
    "04_High School Graduate",
    "05_Some college, no degree",
    "06_Associate degree: occupational, technical, or vocational program",
    "07_Associate degree: academic program",
    "08_Bachelor's degree (Example: BA, AB, BS, BBA)",
    "09_Master's degree (Example: MA, MS, MEng, MEd, MBA)",
    "10_Professional School or Doctoral degree (Example: MD, DDS, DVM, JD, PhD, EdD)",
    "97_Refused",
    "98_Not Ascertained",
    "99_Don't Know"))) |>
mutate(maxeducp_a = ordered(maxeducp_a,
  levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14),
  labels = c(
    "00_Never attended/kindergarten only",
    "01_Grade 1-11",
    "02_12th grade, no diploma",
    "03_GED or equivalent",
    "04_High School Graduate",
    "05_Some college, no degree",
    "06_Associate degree: occupational, technical, or vocational",
    "07_Associate degree: academic program",
    "08_Bachelor's degree (Example: BA, AB, BS, BBA)",
    "09_Master's degree (Example: MA, MS, MEng, MEd, MBA)",
    "10_Professional School or Doctoral degree (Example: MD, DDS, DVM, JD, PhD, EdD)",
    "97_Refused",
    "98_Not Ascertained",
    "99_Don't Know")))

# val_labels(SA074X <- c(
# 00_Never attended/kindergarten only",
# 01_Grade 1-11",
# 02_12th grade, no diploma",
# 03_GED or equivalent",
# 04_High School Graduate",
# 05_Some college, no degree",
# 06_Associate degree: occupational, technical, or vocational program",
# 07_Associate degree: academic program",
# 08_Bachelor's degree (Example: BA, AB, BS, BBA)",
# 09_Master's degree (Example: MA, MS, MEng, MEd, MBA)",
# 10_Professional School or Doctoral degree (Example: MD, DDS, DVM, JD, PhD, EdD)",
# 97_Refused",
# 98_Not Ascertained",
# 99_Don't Know"
# ))

# write a mutate function to replace the values in the data frame
# for the  variable emdwrkcat1_a   below with the numeric and value labels for SA075X found below, linked by an underscore, i.e. "1_Mentioned",
# by making it an ordered factor with the levels and labels below

mutate(emdwrkcat1_a = ordered(emdwrkcat1_a,
  levels = c(1,2,3,4,5,6,7,8,9),
  labels = c(
    "1_Employee of a PRIVATE company for wages",
    "2_A FEDERAL government employee",
    "3_A STATE government employee",
    "4_A LOCAL government employee",
    "5_Self-employed in OWN business, professional practice or farm",
    "6_Working WITHOUT PAY in a family-owned business or farm",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know")))

# val_labels(SA075X <- c(
# 1_Employee of a PRIVATE company for wages",
# 2_A FEDERAL government employee",
# 3_A STATE government employee",
# 4_A LOCAL government employee",
# 5_Self-employed in OWN business, professional practice or farm",
# 6_Working WITHOUT PAY in a family-owned business or farm",
# 7_Refused",
# 8_Not Ascertained",
# 9_Don't Know"
# ))

# write a mutate function to replace the values in the data frame
# for the  variable emplstwor1_a   below with the numeric and value labels for SA075X found below, linked by an underscore, i.e. "1_Mentioned",
# by making it an ordered factor with the levels and labels below

mutate(emplstwor1_a = ordered(emplstwor1_a,
  levels = c(1,2,3,4,5,6,7,8,9),
  labels = c(
    "1_Employee of a PRIVATE company for wages",
    "2_A FEDERAL government employee",
    "3_A STATE government employee",
    "4_A LOCAL government employee",
    "5_Self-employed in OWN business, professional practice or farm",
    "6_Working WITHOUT PAY in a family-owned business or farm",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know")))

# val_labels(SA076X <- c(
# 1_Within the past 12 months",
# 2_1-5 years ago",
# 3_Over 5 years ago",
# 4_Never worked",
# 7_Refused",
# 8_Not Ascertained",
# 9_Don't Know"
# ))

# write a mutate function to replace the values in the data frame
# for the  variable empwhenwrk_a   below with the numeric and value labels for SA077X found below, linked by an underscore, i.e. "1_Mentioned",
# by making it an ordered factor with the levels and labels below

mutate(empwhenwrk_a = ordered(empwhenwrk_a,
  levels = c(1,2,3,4,5,6,7),
  labels = c(
    "1_Within the past 12 months",
    "2_1-5 years ago",
    "3_Over 5 years ago",
    "4_Never worked",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know")))

# val_labels(SA077X <- c(
# 1_Within the past 12 months",
# 2_1-5 years ago",
# 3_Over 5 years ago",
# 4_Never worked",
# 7_Refused",
# 8_Not Ascertained",
# 9_Don't Know"
# ))

# write a mutate function to replace the values in the data frame
# for the  variable empwhynot_a   below with the numeric and value labels for SA078X found below, linked by an underscore, i.e. "1_Mentioned",
# by making it an ordered factor with the levels and labels below

mutate(empwhynot_a = ordered(empwhynot_a,
  levels = c(1,2,3,4,5,6,7,8,9,10,11),
  labels = c(
    "1_Unemployed, laid off, looking for work",
    "2_Seasonal/contract work",
    "3_Retired",
    "4_Unable to work for health reasons/disabled",
    "5_Taking care of house or family",
    "6_Going to school",
    "7_Working at a family-owned job or business not for pay",
    "8_Other",
    "97_Refused",
    "98_Not Ascertained",
    "99_Don't Know")))

# val_labels(SA078X <- c(
# 01_Unemployed, laid off, looking for work",
# 02_Seasonal/contract work",
# 03_Retired",
# 04_Unable to work for health reasons/disabled",
# 05_Taking care of house or family",
# 06_Going to school",
# 07_Working at a family-owned job or business not for pay",
# 08_Other",
# 97_Refused",
# 98_Not Ascertained",
# 99_Don't Know"
# ))

# write a mutate function to replace the values in the data frame
# for the  variable epinumsezp_a   below with the numeric and value labels for SA079X found below, linked by an underscore, i.e. "1_Mentioned",
# by making it an ordered factor with the levels and labels below

mutate(epinumsezp_a = ordered(epinumsezp_a,
  levels = c(1,2,3,4,5,6,7),
  labels = c(
    "1_0",
    "2_1",
    "3_2 or 3",
    "4_4 or more",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know")))

# val_labels(SA079X <- c(
# 0_0",
# 1_1",
# 2_2 or 3",
# 3_4 or more",
# 7_Refused",
# 8_Not Ascertained",
# 9_Don't Know"
# ))

# write a mutate function to replace the values in the data frame
# for the  variable psareason_a and mamreason_a   below with the numeric and value labels for SA080X found below, linked by an underscore, i.e. "1_Mentioned",
# by making it an ordered factor with the levels and labels below

mutate(psareason_a = ordered(psareason_a,
  levels = c(1,2,3,4,5,6),
  labels = c(
    "1_Part of a routine exam",
    "2_Because of a problem",
    "3_Other reason",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know"))) |> 
mutate(mamreason_a = ordered(mamreason_a,
  levels = c(1,2,3,4,5,6),
  labels = c(
    "1_Part of a routine exam",
    "2_Because of a problem",
    "3_Other reason",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know")))

# val_labels(SA080X <- c(
# 1_Part of a routine exam",
# 2_Because of a problem",
# 3_Other reason",
# 7_Refused",
# 8_Not Ascertained",
# 9_Don't Know"
# ))ß

# write a mutate function to replace the values in the data frame
# for the  variable smknow_a and ecignow_aand cigarcur_a and pipecur_a and smokelscur_a below with the numeric and value labels for SA081X found below, linked by an underscore, i.e. "1_Mentioned",
# by making it an ordered factor with the levels and labels below

mutate(smknow_a = ordered(smknow_a,
  levels = c(1,2,3,4,5,6),
  labels = c(
    "1_Every day",
    "2_Some days",
    "3_Not at all",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know")))|>
mutate(ecignow_a = ordered(ecignow_a,
  levels = c(1,2,3,4,5,6),
  labels = c(
    "1_Every day",
    "2_Some days",
    "3_Not at all",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know")))|>
mutate(cigarcur_a = ordered(cigarcur_a,
  levels = c(1,2,3,4,5,6),
  labels = c(
    "1_Every day",
    "2_Some days",
    "3_Not at all",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know")))|>
mutate(pipecur_a = ordered(pipecur_a,
  levels = c(1,2,3,4,5,6),
  labels = c(
    "1_Every day",
    "2_Some days",
    "3_Not at all",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know")))|>
mutate(smokelscur_a = ordered(smokelscur_a,
  levels = c(1,2,3,4,5,6),
  labels = c(
    "1_Every day",
    "2_Some days",
    "3_Not at all",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know")))

# val_labels(SA081X <- c(
# 1_Every day",
# 2_Some days",
# 3_Not at all",
# 7_Refused",
# 8_Not Ascertained",
# 9_Don't Know"
# ))

# write a mutate function to replace the values in the data frame
# for the  variable hraidaqr_a below with the numeric and value labels for SA082X found below, linked by an underscore, i.e. "1_Mentioned",
# by making it an ordered factor with the levels and labels below

mutate(hraidaqr_a = ordered(hraidaqr_a,
  levels = c(1,2,3,4,5),
  labels = c(
    "1_Hearing aid fit by a health care professional",
    "2_Hearing aid purchased online or over the counter without assistance from a health care professional",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know")))

# val_labels(SA082X <- c(
# 1_Hearing aid fit by a health care professional",
# 2_Hearing aid purchased online or over the counter without assistance from a health care professional",
# 7_Refused",
# 8_Not Ascertained",
# 9_Don't Know"
# ))

# write a mutate function to replace the values in the data frame
# for the  variable hearaidfr_a below with the numeric and value labels for SA083X found below, linked by an underscore, i.e. "1_Mentioned",
# by making it an ordered factor with the levels and labels below

mutate(hearaidfr_a = ordered(hearaidfr_a,
  levels = c(1,2,3,4,5,6,7),
  labels = c(
    "1_All of the time",
    "2_Some of the time",
    "3_Rarely",
    "4_Never",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know")))

# val_labels(SA083X <- c(
# 1_All of the time",
# 2_Some of the time",
# 3_Rarely",
# 4_Never",
# 7_Refused",
# 8_Not Ascertained",
# 9_Don't Know"
# ))

# write a mutate function to replace the values in the data frame
# for the  variable ahearst1_a below with the numeric and value labels for SA084X found below, linked by an underscore, i.e. "1_Mentioned",
# by making it an ordered factor with the levels and labels below

mutate(ahearst1_a = ordered(ahearst1_a,
  levels = c(1,2,3,4,5,6,7,8,9),
  labels = c(
    "1_Excellent",
    "2_Good",
    "3_A little trouble hearing",
    "4_Moderate trouble",
    "5_A lot of trouble",
    "6_Deaf",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know")))

# val_labels(SA084X <- c(
# 1_Excellent",
# 2_Good",
# 3_A little trouble hearing",
# 4_Moderate trouble",
# 5_A lot of trouble",
# 6_Deaf",
# 7_Refused",
# 8_Not Ascertained",
# 9_Don't Know"
# ))

# write a mutate function to replace the values in the data frame
# for the  variable houtenure_a below with the numeric and value labels for SA085X found below, linked by an underscore, i.e. "1_Mentioned",
# by making it an ordered factor with the levels and labels below

mutate(houtenure_a = ordered(houtenure_a,
  levels = c(1,2,3,4,5,6),
  labels = c(
    "1_Owned or being bought",
    "2_Rented",
    "3_Other arrangement",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know")))

# val_labels(SA085X <- c(
# 1_Owned or being bought",
# 2_Rented",
# 3_Other arrangement",
# 7_Refused",
# 8_Not Ascertained",
# 9_Don't Know"
# ))

# write a mutate function to replace the values in the data frame
# for the  variable hrtestlast_a below with the numeric and value labels for SA086X found below, linked by an underscore, i.e. "1_Mentioned",
# by making it an ordered factor with the levels and labels below

mutate(hrtestlast_a = ordered(hrtestlast_a,
  levels = c(1,2,3,4,5,6,7,8),
  labels = c(
    "1_Less than a year ago",
    "2_1 to 2 years ago",
    "3_3 to 4 years ago",
    "4_5 to 9 years ago",
    "5_10 or more years ago",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know")))

# val_labels(SA086X <- c(
# 1_Less than a year ago",
# 2_1 to 2 years ago",
# 3_3 to 4 years ago",
# 4_5 to 9 years ago",
# 5_10 or more years ago",
# 7_Refused",
# 8_Not Ascertained",
# 9_Don't Know"
# ))

# write a mutate function to replace the values in the data frame
# for the  variable hrtinlng_a and hrloudjbyr_a below with the numeric and value labels for SA087X found below, linked by an underscore, i.e. "1_Mentioned",
# by making it an ordered factor with the levels and labels below

mutate(hrtinlng_a = ordered(hrtinlng_a,
  levels = c(1,2,3,4,5,6,7,8),
  labels = c(
    "1_Less than 1 year",
    "2_1 to 2 years",
    "3_3 to 4 years",
    "4_5 to 9 years",
    "5_10 years or more",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know"))) |> 
mutate(hrloudjbyr_a = ordered(hrloudjbyr_a,
  levels = c(1,2,3,4,5,6,7),
  labels = c(
    "1_Less than 1 year",
    "2_1 to 2 years",
    "3_3 to 4 years",
    "4_5 to 9 years",
    "5_10 years or more",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know")))
    

# val_labels(SA087X <- c(
# 1_Less than 1 year",
# 2_1 to 2 years",
# 3_3 to 4 years",
# 4_5 to 9 years",
# 5_10 years or more",
# 7_Refused",
# 8_Not Ascertained",
# 9_Don't Know"
# ))

# write a mutate function to replace the values in the data frame
# for the  variable paifrq3m_a and paiwklm3m_a and paiaffm3m_a below with the numeric and value labels for SA088X found below, linked by an underscore, i.e. "1_Mentioned",
# by making it an ordered factor with the levels and labels below

mutate(paifrq3m_a = ordered(paifrq3m_a,
  levels = c(1,2,3,4,5,6,7),
  labels = c(
    "1_Never",
    "2_Some days",
    "3_Most days",
    "4_Every day",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know"))) |>
mutate(paiwklm3m_a = ordered(paiwklm3m_a,
  levels = c(1,2,3,4,5,6,7),
  labels = c(
    "1_Never",
    "2_Some days",
    "3_Most days",
    "4_Every day",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know"))) |>
mutate(paiaffm3m_a = ordered(paiaffm3m_a,
  levels = c(1,2,3,4,5,6,7),
  labels = c(
    "1_Never",
    "2_Some days",
    "3_Most days",
    "4_Every day",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know")))

# val_labels(SA088X <- c(
# 1_Never",
# 2_Some days",
# 3_Most days",
# 4_Every day",
# 7_Refused",
# 8_Not Ascertained",
# 9_Don't Know"
# ))

# write a mutate function to replace the values in the data frame
# for the  variable anxfreq_a and depfreq_a below with the numeric and value labels for SA089X found below, linked by an underscore, i.e. "1_Mentioned",
# by making it an ordered factor with the levels and labels below

mutate(anxfreq_a = ordered(anxfreq_a,
  levels = c(1,2,3,4,5,6,7,8),
  labels = c(
    "1_Daily",
    "2_Weekly",
    "3_Monthly",
    "4_A few times a year",
    "5_Never",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know"))) |>
mutate(depfreq_a = ordered(depfreq_a,
  levels = c(1,2,3,4,5,6,7,8),
  labels = c(
    "1_Daily",
    "2_Weekly",
    "3_Monthly",
    "4_A few times a year",
    "5_Never",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know")))

# val_labels(SA089X <- c(
# 1_Daily",
# 2_Weekly",
# 3_Monthly",
# 4_A few times a year",
# 5_Never",
# 7_Refused",
# 8_Not Ascertained",
# 9_Don't Know"
# ))

# write a mutate function to replace the values in the data frame
# for the  variable discrim1_a, discrim2_a, discrim3_a, discrim4_a, discrim5_a and vigil1_a, vigil2_a, vigil3_a, vigil4_a below with the numeric and value labels for SA090X found below, linked by an underscore, i.e. "1_Mentioned",
# by making it an ordered factor with the levels and labels below

mutate(discrim1_a = ordered(discrim1_a,
  levels = c(1,2,3,4,5,6,7,8),
  labels = c(
    "1_At least once a week",
    "2_A few times a month",
    "3_A few times a year",
    "4_Less than once a year",
    "5_Never",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know"))) |>
mutate(discrim2_a = ordered(discrim2_a,
  levels = c(1,2,3,4,5,6,7,8),
  labels = c(
    "1_At least once a week",
    "2_A few times a month",
    "3_A few times a year",
    "4_Less than once a year",
    "5_Never",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know"))) |>
mutate(discrim3_a = ordered(discrim3_a,
  levels = c(1,2,3,4,5,6,7,8),
  labels = c(
    "1_At least once a week",
    "2_A few times a month",
    "3_A few times a year",
    "4_Less than once a year",
    "5_Never",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know"))) |>
mutate(discrim4_a = ordered(discrim4_a,
  levels = c(1,2,3,4,5,6,7,8),
  labels = c(
    "1_At least once a week",
    "2_A few times a month",
    "3_A few times a year",
    "4_Less than once a year",
    "5_Never",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know"))) |>
mutate(discrim5_a = ordered(discrim5_a,
  levels = c(1,2,3,4,5,6,7,8),
  labels = c(
    "1_At least once a week",
    "2_A few times a month",
    "3_A few times a year",
    "4_Less than once a year",
    "5_Never",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know"))) |>
mutate(vigil1_a = ordered(vigil1_a,
  levels = c(1,2,3,4,5,6,7,8),
  labels = c(
    "1_At least once a week",
    "2_A few times a month",
    "3_A few times a year",
    "4_Less than once a year",
    "5_Never",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know"))) |>
mutate(vigil2_a = ordered(vigil2_a,
  levels = c(1,2,3,4,5,6,7,8),
  labels = c(
    "1_At least once a week",
    "2_A few times a month",
    "3_A few times a year",
    "4_Less than once a year",
    "5_Never",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know"))) |>
mutate(vigil3_a = ordered(vigil3_a,
  levels = c(1,2,3,4,5,6,7,8),
  labels = c(
    "1_At least once a week",
    "2_A few times a month",
    "3_A few times a year",
    "4_Less than once a year",
    "5_Never",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know"))) |>
mutate(vigil4_a = ordered(vigil4_a,
  levels = c(1,2,3,4,5,6,7,8),
  labels = c(
    "1_At least once a week",
    "2_A few times a month",
    "3_A few times a year",
    "4_Less than once a year",
    "5_Never",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know")))

# val_labels(SA090X <- c(
# 1_At least once a week",
# 2_A few times a month",
# 3_A few times a year",
# 4_Less than once a year",
# 5_Never",
# 7_Refused",
# 8_Not Ascertained",
# 9_Don't Know"
# ))

# write a mutate function to replace the values in the data frame
# for the  variable langmed_a, langdoc_a, langsoc_a,  below with the numeric and value labels for SA091X found below, linked by an underscore, i.e. "1_Mentioned",
# by making it an ordered factor with the levels and labels below

mutate(langmed_a = ordered(langmed_a,
  levels = c(1,2,3,4,5),
  labels = c(
    "1_English",
    "2_Other language",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know"))) |>
mutate(langdoc_a = ordered(langdoc_a,
  levels = c(1,2,3,4,5),
  labels = c(
    "1_English",
    "2_Other language",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know"))) |>
mutate(langsoc_a = ordered(langsoc_a,
  levels = c(1,2,3,4,5),
  labels = c(
    "1_English",
    "2_Other language",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know")))

# val_labels(SA091X <- c(
# 1_English",
# 2_Other language",
# 7_Refused",
# 8_Not Ascertained",
# 9_Don't Know"
# ))

# write a mutate function to replace the values in the data frame
# for the  variable anxlevel_a, deplevel_a, paiamnt_a,  below with the numeric and value labels for SA092X found below, linked by an underscore, i.e. "1_Mentioned",
# by making it an ordered factor with the levels and labels below

mutate(anxlevel_a = ordered(anxlevel_a,
  levels = c(1,2,3,4,5,6),
  labels = c(
    "1_A little",
    "2_A lot",
    "3_Somewhere in between a little and a lot",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know"))) |>
mutate(deplevel_a = ordered(deplevel_a,
  levels = c(1,2,3,4,5,6),
  labels = c(
    "1_A little",
    "2_A lot",
    "3_Somewhere in between a little and a lot",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know"))) |>
mutate(paiamnt_a = ordered(paiamnt_a,
  levels = c(1,2,3,4,5,6),
  labels = c(
    "1_A little",
    "2_A lot",
    "3_Somewhere in between a little and a lot",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know")))

# val_labels(SA092X <- c(
# 1_A little",
# 2_A lot",
# 3_Somewhere in between a little and a lot",
# 7_Refused",
# 8_Not Ascertained",
# 9_Don't Know"
# ))

# write a mutate function to replace the values in the data frame
# for the  variable lcvdact_a below with the numeric and value labels for SA093X found below, linked by an underscore, i.e. "1_Mentioned",
# by making it an ordered factor with the levels and labels below

mutate(lcvdact_a = ordered(lcvdact_a,
  levels = c(1,2,3,4,5,6),
  labels = c(
    "1_Not at all",
    "2_A little",
    "3_A lot",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know")))
 
# val_labels(SA093X <- c(
# 1_Not at all",
# 2_A little",
# 3_A lot",
# 7_Refused",
# 8_Not Ascertained",
# 9_Don't Know"
# ))

# write a mutate function to replace the values in the data frame
# for the  variable mamnot1_a below with the numeric and value labels for SA094X found below, linked by an underscore, i.e. "1_Mentioned",
# by making it an ordered factor with the levels and labels below

mutate(mamnot1_a = ordered(mamnot1_a,
  levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14),
  labels = c(
    "01_No reason/never thought about it",
    "02_Didn't need it/didn't know I needed this type of test",
    "03_Doctor didn't order it/didn't say I needed it",
    "04_Haven't had any problems",
    "05_Put it off/didn't get around to it",
    "06_Too expensive/no insurance/cost",
    "07_Too painful, unpleasant, or embarrassing",
    "08_Don't have a doctor",
    "09_I am too old",
    "10_I am too young",
    "11_Other",
    "97_Refused",
    "98_Not Ascertained",
    "99_Don't Know")))

# val_labels(SA094X <- c(
# 01_No reason/never thought about it",
# 02_Didn't need it/didn't know I needed this type of test",
# 03_Doctor didn't order it/didn't say I needed it",
# 04_Haven't had any problems",
# 05_Put it off/didn't get around to it",
# 06_Too expensive/no insurance/cost",
# 07_Too painful, unpleasant, or embarrassing",
# 08_Don't have a doctor",
# 09 _ I am too old",
# 10 _ I am too young",
# 11 _ Other",
# 97_Refused",
# 98_Not Ascertained",
# 99_Don't Know"
# ))

# write a mutate function to replace the values in the data frame
# for the  variable marital_a below with the numeric and value labels for SA095X found below, linked by an underscore, i.e. "1_Mentioned",
# by making it an ordered factor with the levels and labels below

mutate(marital_a = ordered(marital_a,
  levels = c(1,2,3,4,5,6),
  labels = c(
    "1_Married",
    "2_Living with a partner together as an unmarried couple",
    "3_Neither",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know")))

# val_labels(SA095X <- c(
# 1_Married",
# 2_Living with a partner together as an unmarried couple",
# 3_Neither",
# 7_Refused",
# 8_Not Ascertained",
# 9_Don't Know"
# ))

# for the  variable mcpart_a below with the numeric and value labels for SA096X found below, linked by an underscore, i.e. "1_Mentioned",
# by making it an ordered factor with the levels and labels below

mutate(mcpart_a = ordered(mcpart_a,
  levels = c(1,2,3,4,5,6),
  labels = c(
    "1_Part A- hospital only",
    "2_Part B- medical only",
    "3_Both Part A and Part B",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know")))

# val_labels(SA096X <- c(
# 1_Part A- hospital only",
# 2_Part B- medical only",
# 3_Both Part A and Part B",
# 7_Refused",
# 8_Not Ascertained",
# 9_Don't Know"
# ))

# write a mutate function to replace the values in the data frame
# for the  variable mentholc_a below with the numeric and value labels for SA097X found below, linked by an underscore, i.e. "1_Mentioned",
# by making it an ordered factor with the levels and labels below

mutate(mentholc_a = ordered(mentholc_a,
  levels = c(1,2,3,4,5,6),
  labels = c(
    "1_Menthol",
    "2_Non-menthol",
    "3_No usual type",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know")))

# val_labels(SA097X <- c(
# 1_Menthol",
# 2_Non-menthol",
# 3_No usual type",
# 7_Refused",
# 8_Not ascertained",
# 9_Don't Know"
# ))

# write a mutate function to replace the values in the data frame
# for the  variable pln1pay1_a, pln1pay2_a, pln1pay3_a, pln1pay4_a, pln1pay5_a, pln1pay6_a, pln2pay1_a, pln2pay2_a, pln2pay3_a, pln2pay4_a, pln2pay5_a, pln2pay6_a, milspc1r_a, milspc2_a, milspc3_a, milspc1_a, hikind01_a, hikind02_a, hikind03_a, hikind04_a, hikind05_a, hikind06_a, hikind07_a, hikind08_a, hikind09_a, hikind10_a, coltest1_a, coltest2_a, coltest3_a, coltest4_a, coltest5_a, coltest6_a,  injmvtype1_a, injmvtype2_a, injmvtype3_a, injmvtype4_a, injmvtype5_a, below with the numeric and value labels for SA098X found below, linked by an underscore, i.e. "1_Mentioned",
# by making it an ordered factor with the levels and labels below

mutate(pln1pay1_a = ordered(pln1pay1_a,
  levels = c(1,2,3,4,5),
  labels = c(
    "1_Mentioned",
    "2_Not mentioned",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know"))) |> 
mutate(pln1pay2_a = ordered(pln1pay2_a,
  levels = c(1,2,3,4,5),
  labels = c(
    "1_Mentioned",
    "2_Not mentioned",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know"))) |>
mutate(pln1pay3_a = ordered(pln1pay3_a,
  levels = c(1,2,3,4,5),
  labels = c(
    "1_Mentioned",
    "2_Not mentioned",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know"))) |>
mutate(pln1pay4_a = ordered(pln1pay4_a,
  levels = c(1,2,3,4,5),
  labels = c(
    "1_Mentioned",
    "2_Not mentioned",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know"))) |>
mutate(pln1pay5_a = ordered(pln1pay5_a,
  levels = c(1,2,3,4,5),
  labels = c(
    "1_Mentioned",
    "2_Not mentioned",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know"))) |>
mutate(pln1pay6_a = ordered(pln1pay6_a,
  levels = c(1,2,3,4,5),
  labels = c(
    "1_Mentioned",
    "2_Not mentioned",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know"))) |>
mutate(pln2pay1_a = ordered(pln2pay1_a,
  levels = c(1,2,3,4,5),
  labels = c(
    "1_Mentioned",
    "2_Not mentioned",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know"))) |>
mutate(pln2pay2_a = ordered(pln2pay2_a,
  levels = c(1,2,3,4,5),
  labels = c(
    "1_Mentioned",
    "2_Not mentioned",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know"))) |>
mutate(pln2pay3_a = ordered(pln2pay3_a,
  levels = c(1,2,3,4,5),
  labels = c(
    "1_Mentioned",
    "2_Not mentioned",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know"))) |>
mutate(pln2pay4_a = ordered(pln2pay4_a,
  levels = c(1,2,3,4,5),
  labels = c(
    "1_Mentioned",
    "2_Not mentioned",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know"))) |>
mutate(pln2pay5_a = ordered(pln2pay5_a,
  levels = c(1,2,3,4,5),
  labels = c(
    "1_Mentioned",
    "2_Not mentioned",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know"))) |>
mutate(pln2pay6_a = ordered(pln2pay6_a,
  levels = c(1,2,3,4,5),
  labels = c(
    "1_Mentioned",
    "2_Not mentioned",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know"))) |>
mutate(milspc1r_a = ordered(milspc1r_a,
  levels = c(1,2,3,4,5),
  labels = c(
    "1_Mentioned",
    "2_Not mentioned",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know"))) |>
mutate(milspc2_a = ordered(milspc2_a,
  levels = c(1,2,3,4,5),
  labels = c(
    "1_Mentioned",
    "2_Not mentioned",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know"))) |>
mutate(milspc3_a = ordered(milspc3_a,
  levels = c(1,2,3,4,5),
  labels = c(
    "1_Mentioned",
    "2_Not mentioned",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know"))) |>
mutate(milspc1_a = ordered(milspc1_a,
  levels = c(1,2,3,4,5),
  labels = c(
    "1_Mentioned",
    "2_Not mentioned",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know"))) |>
mutate(hikind01_a = ordered(hikind01_a,
  levels = c(1,2,3,4,5),
  labels = c(
    "1_Mentioned",
    "2_Not mentioned",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know"))) |>
mutate(hikind02_a = ordered(hikind02_a,
  levels = c(1,2,3,4,5),
  labels = c(
    "1_Mentioned",
    "2_Not mentioned",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know"))) |>
mutate(hikind03_a = ordered(hikind03_a,
  levels = c(1,2,3,4,5),
  labels = c(
    "1_Mentioned",
    "2_Not mentioned",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know"))) |>
mutate(hikind04_a = ordered(hikind04_a,
  levels = c(1,2,3,4,5),
  labels = c(
    "1_Mentioned",
    "2_Not mentioned",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know"))) |>
mutate(hikind05_a = ordered(hikind05_a,
  levels = c(1,2,3,4,5),
  labels = c(
    "1_Mentioned",
    "2_Not mentioned",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know"))) |>
mutate(hikind06_a = ordered(hikind06_a,
  levels = c(1,2,3,4,5),
  labels = c(
    "1_Mentioned",
    "2_Not mentioned",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know"))) |>
mutate(hikind07_a = ordered(hikind07_a,
  levels = c(1,2,3,4,5),
  labels = c(
    "1_Mentioned",
    "2_Not mentioned",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know"))) |>
mutate(hikind08_a = ordered(hikind08_a,
  levels = c(1,2,3,4,5),
  labels = c(
    "1_Mentioned",
    "2_Not mentioned",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know"))) |>
mutate(hikind09_a = ordered(hikind09_a,
  levels = c(1,2,3,4,5),
  labels = c(
    "1_Mentioned",
    "2_Not mentioned",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know"))) |>
mutate(hikind10_a = ordered(hikind10_a,
  levels = c(1,2,3,4,5),
  labels = c(
    "1_Mentioned",
    "2_Not mentioned",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know"))) |>
mutate(coltest1_a = ordered(coltest1_a,
  levels = c(1,2,3,4,5),
  labels = c(
    "1_Mentioned",
    "2_Not mentioned",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know"))) |>
mutate(coltest2_a = ordered(coltest2_a,
  levels = c(1,2,3,4,5),
  labels = c(
    "1_Mentioned",
    "2_Not mentioned",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know"))) |>
mutate(coltest3_a = ordered(coltest3_a,
  levels = c(1,2,3,4,5),
  labels = c(
    "1_Mentioned",
    "2_Not mentioned",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know"))) |>
mutate(coltest4_a = ordered(coltest4_a,
  levels = c(1,2,3,4,5),
  labels = c(
    "1_Mentioned",
    "2_Not mentioned",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know"))) |>
mutate(coltest5_a = ordered(coltest5_a,
  levels = c(1,2,3,4,5),
  labels = c(
    "1_Mentioned",
    "2_Not mentioned",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know"))) |>
mutate(coltest6_a = ordered(coltest6_a,
  levels = c(1,2,3,4,5),
  labels = c(
    "1_Mentioned",
    "2_Not mentioned",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know"))) |>
mutate(injmvtype1_a = ordered(injmvtype1_a,
  levels = c(1,2,3,4,5),
  labels = c(
    "1_Mentioned",
    "2_Not mentioned",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know"))) |>
mutate(injmvtype2_a = ordered(injmvtype2_a,
  levels = c(1,2,3,4,5),
  labels = c(
    "1_Mentioned",
    "2_Not mentioned",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know"))) |>
mutate(injmvtype3_a = ordered(injmvtype3_a,
  levels = c(1,2,3,4,5),
  labels = c(
    "1_Mentioned",
    "2_Not mentioned",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know"))) |>
mutate(injmvtype4_a = ordered(injmvtype4_a,
  levels = c(1,2,3,4,5),
  labels = c(
    "1_Mentioned",
    "2_Not mentioned",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know"))) |>
mutate(injmvtype5_a = ordered(injmvtype5_a,
  levels = c(1,2,3,4,5),
  labels = c(
    "1_Mentioned",
    "2_Not mentioned",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know")))
  
# val_labels(SA098X <- c(
# 1_Mentioned",
# 2_Not mentioned",
# 7_Refused",
# 8_Not Ascertained",
# 9_Don't Know"
# ))

# write a mutate function to replace the values in the data frame
# for the  variable shtflum_a, cvdvac1m1_a below with the numeric and value labels for SA099X found below, linked by an underscore, i.e. "1_Mentioned",
# by making it an ordered factor with the levels and labels below

mutate(shtflum_a = ordered(shtflum_a,
  levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15),
  labels = c(
    "01_January",
    "02_February",
    "03_March",
    "04_April",
    "05_May",
    "06_June",
    "07_July",
    "08_August",
    "09_September",
    "10_October",
    "11_November",
    "12_December",
    "97_Refused",
    "98_Not Ascertained",
    "99_Don't Know"))) |>
mutate(cvdvac1m1_a = ordered(cvdvac1m1_a,
  levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15),
  labels = c(
    "01_January",
    "02_February",
    "03_March",
    "04_April",
    "05_May",
    "06_June",
    "07_July",
    "08_August",
    "09_September",
    "10_October",
    "11_November",
    "12_December",
    "97_Refused",
    "98_Not Ascertained",
    "99_Don't Know")))

# val_labels(SA099X <- c(
# 01_January",
# 02_February",
# 03_March",
# 04_April",
# 05_May",
# 06_June",
# 07_July",
# 08_August",
# 09_September",
# 10_October",
# 11_November",
# 12_December",
# 97_Refused",
# 98_Not Ascertained",
# 99_Don't Know"
# ))

# write a mutate function to replace the values in the data frame
# for the  variable hilastmy_a, hinotmyr_a below with the numeric and value labels for SA100X found below, linked by an underscore, i.e. "1_Mentioned",
# by making it an ordered factor with the levels and labels below

mutate(hilastmy_a = ordered(hilastmy_a,
  levels = c(1,2,3),
  labels = c(
    "97_Refused",
    "98_Not Ascertained",
    "99_Don't Know"))) |>
mutate(hinotmyr_a = ordered(hinotmyr_a,
  levels = c(1,2,3),
  labels = c(
    "97_Refused",
    "98_Not Ascertained",
    "99_Don't Know")))

# val_labels(SA100X <- c(
# 97_Refused",
# 98_Not Ascertained",
# 99_Don't Know"
# ))

# write a mutate function to replace the values in the data frame
# for the  variable mrirea_a below with the numeric and value labels for SA101X found below, linked by an underscore, i.e. "1_Mentioned",
# by making it an ordered factor with the levels and labels below

mutate(mrirea_a = ordered(mrirea_a,
  levels = c(1,2,3,4,5,6,7,8),
  labels = c(
    "1_A follow up of an abnormal mammogram",
    "2_Because of a breast problem",
    "3_I am high risk due to family history, genetic test, or another reason",
    "4_Part of a routine exam",
    "5_Other",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know")))

# val_labels(SA101X <- c(
# 1_A follow up of an abnormal mammogram",
# 2_Because of a breast problem",
# 3_I am high risk due to family history, genetic test, or another reason",
# 4_Part of a routine exam",
# 5_Other",
# 7_Refused",
# 8_Not Ascertained",
# 9_Don't Know"
# ))

# write a mutate function to replace the values in the data frame
# for the  variable paiback3m_a, paiulmb3m_a, paillmb3m_a, paihdfc3m_a, paiapg3m_a, paitooth3m_a below with the numeric and value labels for SA102X found below, linked by an underscore, i.e. "1_Mentioned",
# by making it an ordered factor with the levels and labels below

mutate(paiback3m_a = ordered(paiback3m_a,
  levels = c(1,2,3,4,5,6,7),
  labels = c(
    "1_Not at all",
    "2_A little",
    "3_A lot",
    "4_Somewhere in between a little and a lot",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know")))|>
mutate(paiulmb3m_a = ordered(paiulmb3m_a,
  levels = c(1,2,3,4,5,6,7),
  labels = c(
    "1_Not at all",
    "2_A little",
    "3_A lot",
    "4_Somewhere in between a little and a lot",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know")))|>
mutate(paillmb3m_a = ordered(paillmb3m_a,
  levels = c(1,2,3,4,5,6,7),
  labels = c(
    "1_Not at all",
    "2_A little",
    "3_A lot",
    "4_Somewhere in between a little and a lot",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know")))|>
mutate(paihdfc3m_a = ordered(paihdfc3m_a,
  levels = c(1,2,3,4,5,6,7),
  labels = c(
    "1_Not at all",
    "2_A little",
    "3_A lot",
    "4_Somewhere in between a little and a lot",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know")))|>
mutate(paiapg3m_a = ordered(paiapg3m_a,
  levels = c(1,2,3,4,5,6,7),
  labels = c(
    "1_Not at all",
    "2_A little",
    "3_A lot",
    "4_Somewhere in between a little and a lot",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know")))|>
mutate(paitooth3m_a = ordered(paitooth3m_a,
  levels = c(1,2,3,4,5,6,7),
  labels = c(
    "1_Not at all",
    "2_A little",
    "3_A lot",
    "4_Somewhere in between a little and a lot",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know")))

# val_labels(SA102X <- c(
# 1_Not at all",
# 2_A little",
# 3_A lot",
# 4_Somewhere in between a little and a lot",
# 7_Refused",
# 8_Not Ascertained",
# 9_Don't Know"
# ))

# write a mutate function to replace the values in the data frame
# for the  variable bfalltimes_a below with the numeric and value labels for SA103X found below, linked by an underscore, i.e. "1_Mentioned",
# by making it an ordered factor with the levels and labels below

mutate(bfalltimes_a = ordered(bfalltimes_a,
  levels = c(1,2,3,4,5,6,7,8),
  labels = c(
    "1_1 time",
    "2_2 times",
    "3_3 to 4 times",
    "4_5 to 7 times",
    "5_8_or more times",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know")))

# val_labels(SA103X <- c(
# 1_1 time",
# 2_2 times",
# 3_3 to 4 times",
# 4_5 to 7 times",
# 5_8_or more times",
# 7_Refused",
# 8_Not Ascertained",
# 9_Don't Know"
# ))

# write a mutate function to replace the values in the data frame
# for the  variable fhbcannum_a, fhovcannum_a below with the numeric and value labels for SA104X found below, linked by an underscore, i.e. "1_Mentioned",
# by making it an ordered factor with the levels and labels below

mutate(fhbcannum_a = ordered(fhbcannum_a,
  levels = c(1,2,3,4,5,6),
  labels = c(
    "1_1 relative",
    "2_2 relatives",
    "3_3 or more relatives",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know")))|>
mutate(fhovcannum_a = ordered(fhovcannum_a,
  levels = c(1,2,3,4,5,6),
  labels = c(
    "1_1 relative",
    "2_2 relatives",
    "3_3 or more relatives",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know")))

# val_labels(SA104X <- c(
# 1_1 relative",
# 2_2 relatives",
# 3_3 or more relatives",
# 7_Refused",
# 8_Not Ascertained",
# 9_Don't Know"
# ))

# write a mutate function to replace the values in the data frame
# for the  variable fhbcan50_a below with the numeric and value labels for SA105X found below, linked by an underscore, i.e. "1_Mentioned",
# by making it an ordered factor with the levels and labels below

mutate(fhbcan50_a = ordered(fhbcan50_a,
  levels = c(1,2,3,4,5,6),
  labels = c(
    "1_1 relative",
    "2_2 relatives",
    "3_3 or more relatives",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know")))

# val_labels(SA105X <- c(
# 0_0 relatives",
# 1_1 relative",
# 2_2 relatives",
# 3_3 or more relatives",
# 7_Refused",
# 8_Not Ascertained",
# 9_Don't Know"
# ))

# write a mutate function to replace the values in the data frame
# for the  variable phq41_a, phq42_a, phq43_a, phq44_a below with the numeric and value labels for SA106X found below, linked by an underscore, i.e. "1_Mentioned",
# by making it an ordered factor with the levels and labels below
# use consecutive integers starting at 1 for the levels

mutate(phq41_a = ordered(phq41_a,
  levels = c(1,2,3,4,5,6,7),
  labels = c(
    "1_Not at all",
    "2_Several days",
    "3_More than half the days",
    "4_Nearly every day",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know")))|>
mutate(phq42_a = ordered(phq42_a,
  levels = c(1,2,3,4,5,6,7),
  labels = c(
    "1_Not at all",
    "2_Several days",
    "3_More than half the days",
    "4_Nearly every day",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know")))|>
mutate(phq43_a = ordered(phq43_a,
  levels = c(1,2,3,4,5,6,7),
  labels = c(
    "1_Not at all",
    "2_Several days",
    "3_More than half the days",
    "4_Nearly every day",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know")))|>
mutate(phq44_a = ordered(phq44_a,
  levels = c(1,2,3,4,5,6,7),
  labels = c(
    "1_Not at all",
    "2_Several days",
    "3_More than half the days",
    "4_Nearly every day",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know")))

# val_labels(SA106X <- c(
# 1_Not at all",
# 2_Several days",
# 3_More than half the days",
# 4_Nearly every day",
# 7_Refused",
# 8_Not Ascertained",
# 9_Don't Know"
# ))

# write a mutate function to replace the values in the data frame
# for the  variable phstat_a below with the numeric and value labels for SA107X found below, linked by an underscore, i.e. "1_Mentioned",
# by making it an ordered factor with the levels and labels below
# use consecutive integers starting at 1 for the levels

mutate(phstat_a = ordered(phstat_a,
  levels = c(1,2,3,4,5,6,7,8),
  labels = c(
    "1_Excellent",
    "2_Very Good",
    "3_Good",
    "4_Fair",
    "5_Poor",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know")))

# val_labels(SA107X <- c(
# 1_Excellent",
# 2_Very Good",
# 3_Good",
# 4_Fair",
# 5_Poor",
# 7_Refused",
# 8_Not Ascertained",
# 9_Don't Know"
# ))

# write a mutate function to replace the values in the data frame
# for the  variable usplkind_a below with the numeric and value labels for SA108X found below, linked by an underscore, i.e. "1_Mentioned",
# by making it an ordered factor with the levels and labels below
# use consecutive integers starting at 1 for the levels with one level for each label

mutate(usplkind_a = ordered(usplkind_a,
  levels = c(1,2,3,4,5,6,7,8,9),
  labels = c(
    "1_A doctor's office or health center",
    "2_Urgent care center or clinic in a drug store or grocery store",
    "3_Hospital emergency room",
    "4_A VA Medical Center or VA outpatient clinic",
    "5_Some other place",
    "6_Does not go to one place most often",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know")))

# val_labels(SA108X <- c(
# 1_A doctor's office or health center",
# 2_Urgent care center or clinic in a drug store or grocery store",
# 3_Hospital emergency room",
# 4_A VA Medical Center or VA outpatient clinic",
# 5_Some other place",
# 6_Does not go to one place most often",
# 7_Refused",
# 8_Not Ascertained",
# 9_Don't Know"
# ))

# write a mutate function to replace the values in the data frame
# for the  variable prhdhp1_a, prhdhp2_a below with the numeric and value labels for SA109X found below, linked by an underscore, i.e. "1_Mentioned",
# by making it an ordered factor with the levels and labels below
# use consecutive integers starting at 1 for the levels with one level for each label

mutate(prhdhp1_a = ordered(prhdhp1_a,
  levels = c(1,2,3,4,5),
  labels = c(
    "1_Less than ^HDHPAMT_A",
    "2_^HDHPAMT_A or more",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know")))|>
mutate(prhdhp2_a = ordered(prhdhp2_a,
  levels = c(1,2,3,4,5),
  labels = c(
    "1_Less than ^HDHPAMT_A",
    "2_^HDHPAMT_A or more",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know")))

# val_labels(SA109X <- c(
# 1_Less than ^HDHPAMT_A",
# 2_^HDHPAMT_A or more",
# 7_Refused",
# 8_Not Ascertained",
# 9_Don't Know"
# ))

# write a mutate function to replace the values in the data frame
# for the  variable proxyrel_a below with the numeric and value labels for SA110X found below, linked by an underscore, i.e. "1_Mentioned",
# by making it an ordered factor with the levels and labels below
# use consecutive integers starting at 1 for the levels with one level for each label

mutate(proxyrel_a = ordered(proxyrel_a,
  levels = c(1,2,3,4,5,6,7),
  labels = c(
    "1_Relative who lives in household",
    "2_Relative who doesn't live in household",
    "3_Nonrelative who lives in household",
    "4_Nonrelative who does not live in household",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know")))

# val_labels(SA110X <- c(
# 1_Relative who lives in household",
# 2_Relative who doesn't live in household",
# 3_Nonrelative who lives in household",
# 4_Nonrelative who does not live in household",
# 7_Refused",
# 8_Not Ascertained",
# 9_Don't Know"
# ))

# write a mutate function to replace the values in the data frame
# for the  variable prpolh1_a, prpolh2_a below with the numeric and value labels for SA111X found below, linked by an underscore, i.e. "1_Mentioned",
# by making it an ordered factor with the levels and labels below
# use consecutive integers starting at 1 for the levels with one level for each label

mutate(prpolh1_a = ordered(prpolh1_a,
  levels = c(1,2,3,4,5,6,7),
  labels = c(
    "1_Child",
    "2_Spouse",
    "3_Former spouse",
    "4_Some other relationship",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know")))|>
mutate(prpolh2_a = ordered(prpolh2_a,
  levels = c(1,2,3,4,5,6,7),
  labels = c(
    "1_Child",
    "2_Spouse",
    "3_Former spouse",
    "4_Some other relationship",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know")))

# val_labels(SA111X <- c(
# 1_Child",
# 2_Spouse",
# 3_Former spouse",
# 4_Some other relationship",
# 7_Refused",
# 8_Not Ascertained",
# 9_Don't Know"
# ))

# write a mutate function to replace the values in the data frame
# for the  variable psa5yr1_a below with the numeric and value labels for SA112X found below, linked by an underscore, i.e. "1_Mentioned",
# by making it an ordered factor with the levels and labels below
# use consecutive integers starting at 1 for the levels with one level for each label

mutate(psa5yr1_a = ordered(psa5yr1_a,
  levels = c(1,2,3,4,5,6,7,8,9,10),
  labels = c(
    "0_0 tests",
    "1_1 test",
    "2_2 tests",
    "3_3 tests",
    "4_4 tests",
    "5_5 tests",
    "6_6 or more tests",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know")))

# val_labels(SA112X <- c(
# 0_0 tests",
# 1_1 test",
# 2_2 tests",
# 3_3 tests",
# 4_4 tests",
# 5_5 tests",
# 6_6 or more tests",
# 7_Refused",
# 8_Not Ascertained",
# 9_Don't Know"
# ))

# write a mutate function to replace the values in the data frame
# for the  variable shtpneunb_a below with the numeric and value labels for SA113X found below, linked by an underscore, i.e. "1_Mentioned",
# by making it an ordered factor with the levels and labels below
# use consecutive integers starting at 1 for the levels with one level for each label

mutate(shtpneunb_a = ordered(shtpneunb_a,
  levels = c(1,2,3,4,5,6),
  labels = c(
    "1_One pneumonia shot",
    "2_Two pneumonia shots",
    "3_More than two pneumonia shots",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know")))

# val_labels(SA113X <- c(
# 1_One pneumonia shot",
# 2_Two pneumonia shots",
# 3_More than two pneumonia shots",
# 7_Refused",
# 8_Not Ascertained",
# 9_Don't Know"
# ))

# write a mutate function to replace the values in the data frame
# for the  variable avail_a below with the numeric and value labels for SA114X found below, linked by an underscore, i.e. "1_Mentioned",
# by making it an ordered factor with the levels and labels below
# use consecutive integers starting at 1 for the levels with one level for each label

mutate(avail_a = ordered(avail_a,
  levels = c(1,2,3,4,5),
  labels = c(
    "1_Available",
    "2_Not Available or not able to answer right now",
    "3_Physical or mental condition prohibits responding",
    "7_Refused",
    "8_Not Ascertained")))

# val_labels(SA114X <- c(
# 1_Available",
# 2_Not Available or not able to answer right now",
# 3_Physical or mental condition prohibits responding",
# 7_Refused",
# 8_Not Ascertained"
# ))

# write a mutate function to replace the values in the data frame
# for the  variable lsatis4_a below with the numeric and value labels for SA115X found below, linked by an underscore, i.e. "1_Mentioned",
# by making it an ordered factor with the levels and labels below
# use consecutive integers starting at 1 for the levels with one level for each label

mutate(lsatis4_a = ordered(lsatis4_a,
  levels = c(1,2,3,4,5,6,7),
  labels = c(
    "1_Very satisfied",
    "2_Satisfied",
    "3_Dissatisfied",
    "4_Very dissatisfied",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know")))

# val_labels(SA115X <- c(
# 1_Very satisfied",
# 2_Satisfied",
# 3_Dissatisfied",
# 4_Very dissatisfied",
# 7_Refused",
# 8_Not Ascertained",
# 9_Don't Know"
# ))

# write a mutate function to replace the values in the data frame
# for the  variable jntpn_a below with the numeric and value labels for SA116X found below, linked by an underscore, i.e. "1_Mentioned",
# by making it an ordered factor with the levels and labels below
# use consecutive integers starting at 1 for the levels with one level for each label

mutate(jntpn_a = ordered(jntpn_a,
  levels = c(1,2,3),
  labels = c(
    "97_Refused",
    "98_Not Ascertained",
    "99_Don't Know")))

# val_labels(SA116X <- c(
# 97_Refused",
# 98_Not Ascertained",
# 99_Don't Know"
# ))

# write a mutate function to replace the values in the data frame
# for the  variable hrjobprot_a, hrfireprot_a, hrvldprot_a below with the numeric and value labels for SA117X found below, linked by an underscore, i.e. "1_Mentioned",
# by making it an ordered factor with the levels and labels below
# use consecutive integers starting at 1 for the levels with one level for each label

mutate(hrjobprot_a = ordered(hrjobprot_a,
  levels = c(1,2,3,4,5,6,7,8),
  labels = c(
    "1_Always",
    "2_Usually",
    "3_About half the time",
    "4_Seldom",
    "5_Never",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know"))) |> 
mutate(hrfireprot_a = ordered(hrfireprot_a,
  levels = c(1,2,3,4,5,6,7,8),
  labels = c(
    "1_Always",
    "2_Usually",
    "3_About half the time",
    "4_Seldom",
    "5_Never",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know"))) |>
mutate(hrvldprot_a = ordered(hrvldprot_a,
  levels = c(1,2,3,4,5,6,7,8),
  labels = c(
    "1_Always",
    "2_Usually",
    "3_About half the time",
    "4_Seldom",
    "5_Never",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know")))

# val_labels(SA117X <- c(
# 1_Always",
# 2_Usually",
# 3_About half the time",
# 4_Seldom",
# 5_Never",
# 7_Refused",
# 8_Not Ascertained",
# 9_Don't Know"
# ))

# write a mutate function to replace the values in the data frame
# for the  variable sex_a, spousesex_a, prtnrsex_a below with the numeric and value labels for SA118X found below, linked by an underscore, i.e. "1_Mentioned",
# by making it an ordered factor with the levels and labels below
# use consecutive integers starting at 1 for the levels with one level for each label

mutate(sex_a = ordered(sex_a,
  levels = c(1,2,3,4,5),
  labels = c(
    "1_Male",
    "2_Female",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know"))) |>
mutate(spousesex_a = ordered(spousesex_a,
  levels = c(1,2,3,4,5),
  labels = c(
    "1_Male",
    "2_Female",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know"))) |>
mutate(prtnrsex_a = ordered(prtnrsex_a,
  levels = c(1,2,3,4,5),
  labels = c(
    "1_Male",
    "2_Female",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know"))) 

# val_labels(SA118X <- c(
# 1_Male",
# 2_Female",
# 7_Refused",
# 8_Not Ascertained",
# 9_Don't Know"
# ))

# write a mutate function to replace the values in the data frame
# for the  variable shingrixfs1_a below with the numeric and value labels for SA119X found below, linked by an underscore, i.e. "1_Mentioned",
# by making it an ordered factor with the levels and labels below
# use consecutive integers starting at 1 for the levels with one level for each label

mutate(shingrixfs1_a = ordered(shingrixfs1_a,
  levels = c(1,2,3,4,5),
  labels = c(
    "1_First shot",
    "2_Second shot",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know")))

# val_labels(SA119X <- c(
# 1_First shot",
# 2_Second shot",
# 7_Refused",
# 8_Not Ascertained",
# 9_Don't Know"
# ))

# write a mutate function to replace the values in the data frame
# for the  variable shingrixn3_a below with the numeric and value labels for SA120X found below, linked by an underscore, i.e. "1_Mentioned",
# by making it an ordered factor with the levels and labels below
# use consecutive integers starting at 1 for the levels with one level for each label

mutate(shingrixn3_a = ordered(shingrixn3_a,
  levels = c(1,2,3,4,5),
  labels = c(
    "1_One Shingrix shot",
    "2_Two Shingrix shots",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know")))

# val_labels(SA120X <- c(
# 1_One Shingrix shot",
# 2_Two Shingrix shots",
# 7_Refused",
# 8_Not Ascertained",
# 9_Don't Know"
# ))

# write a mutate function to replace the values in the data frame
# for the  variable ssissdibth_a below with the numeric and value labels for SA121X found below, linked by an underscore, i.e. "1_Mentioned",
# by making it an ordered factor with the levels and labels below
# use consecutive integers starting at 1 for the levels with one level for each label

mutate(ssissdibth_a = ordered(ssissdibth_a,
  levels = c(1,2,3,4,5,6),
  labels = c(
    "1_SSI",
    "2_SSDI",
    "3_Both SSI and SSDI",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know")))

# val_labels(SA121 <-c(
# 1_SSI",
# 2_SSDI",
# 3_Both SSI and SSDI",
# 7_Refused",
# 8_Not Ascertained",
# 9_Don't Know"
# ) )

# write a mutate function to replace the values in the data frame
# for the  variable astatnew below with the numeric and value labels for SA122X found below, linked by an underscore, i.e. "1_Mentioned",
# by making it an ordered factor with the levels and labels below
# use consecutive integers starting at 1 for the levels with one level for each label

mutate(astatnew = ordered(astatnew,
  levels = c(1,2,3,4,5,6,7),
  labels = c(
    "1_Completed",
    "2_HH member selected",
    "3_Reached Sample Adult",
    "4_Started HIS section",
    "5_Sufficient Partial",
    "6_HH member selected and no longer eligible",
    "7_Refused")))

# val_labels(SA122X <- c(
# 0_Not applicable/None eligible",
# 1_Completed",
# 2_HH member selected",
# 3_Reached Sample Adult",
# 4_Started HIS section",
# 5_Sufficient Partial",
# 6_HH member selected and no longer eligible",
# 7_Refused"
# ))

# write a mutate function to replace the values in the data frame
# for the  variable avisexam_a below with the numeric and value labels for SA123X found below, linked by an underscore, i.e. "1_Mentioned",
# by making it an ordered factor with the levels and labels below
# use consecutive integers starting at 1 for the levels with one level for each label

mutate(avisexam_a = ordered(avisexam_a,
  levels = c(1,2,3,4,5,6,7,8,9,10),
  labels = c(
    "0_Never",
    "1_Within the past year (anytime less than 12 months ago)",
    "2_Within the last 2 years (1 year but less than 2 years ago)",
    "3_Within the last 3 years (2 years but less than 3 years ago)",
    "4_Within the last 5 years (3 years but less than 5 years ago)",
    "5_5 years ago or more",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know")))

# val_labels(SA123X <- c(
# 0_Never",
# 1_Within the past year (anytime less than 12 months ago)",
# 2_Within the last 2 years (1 year but less than 2 years ago)",
# 3_Within the last 3 years (2 years but less than 3 years ago)",
# 4_Within the last 5 years (3 years but less than 5 years ago)",
# 5_5 years ago or more",
# 7_Refused",
# 8_Not Ascertained",
# 9_Don't Know"
# ))

# write a mutate function to replace the values in the data frame
# for the  variable colwhen_a, colsigwhen_a, sigwhen_a, ctcolwhen1_a, 
# fithwhen1_a, cguardwhe1_a, psawhen_a, cervicwhen_a, mamwhen_a, mriwhen_a below with the numeric and value labels for SA124X found below, linked by an underscore, i.e. "1_Mentioned",
# by making it an ordered factor with the levels and labels below
# use consecutive integers starting at 1 for the levels with one level for each label

mutate(colwhen_a = ordered(colwhen_a,
  levels = c(1,2,3,4,5,6,7,8,9),
  labels = c(
    "1_Within the past year (anytime less than 12 months ago)",
    "2_Within the past 2 years (1 year but less than 2 years ago)",
    "3_Within the past 3 years (2 years but less than 3 years ago)",
    "4_Within the past 5 years (3 years but less than 5 years ago)",
    "5_Within the past 10 years (5 years but less than 10 year ago)",
    "6_10 years ago or more",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know"))) |> 
mutate(colsigwhen_a = ordered(colsigwhen_a,
  levels = c(1,2,3,4,5,6,7,8,9),
  labels = c(
    "1_Within the past year (anytime less than 12 months ago)",
    "2_Within the past 2 years (1 year but less than 2 years ago)",
    "3_Within the past 3 years (2 years but less than 3 years ago)",
    "4_Within the past 5 years (3 years but less than 5 years ago)",
    "5_Within the past 10 years (5 years but less than 10 year ago)",
    "6_10 years ago or more",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know"))) |>
mutate(sigwhen_a = ordered(sigwhen_a,
  levels = c(1,2,3,4,5,6,7,8,9),
  labels = c(
    "1_Within the past year (anytime less than 12 months ago)",
    "2_Within the past 2 years (1 year but less than 2 years ago)",
    "3_Within the past 3 years (2 years but less than 3 years ago)",
    "4_Within the past 5 years (3 years but less than 5 years ago)",
    "5_Within the past 10 years (5 years but less than 10 year ago)",
    "6_10 years ago or more",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know"))) |>
mutate(ctcolwhen1_a = ordered(ctcolwhen1_a,
  levels = c(1,2,3,4,5,6,7,8,9),
  labels = c(
    "1_Within the past year (anytime less than 12 months ago)",
    "2_Within the past 2 years (1 year but less than 2 years ago)",
    "3_Within the past 3 years (2 years but less than 3 years ago)",
    "4_Within the past 5 years (3 years but less than 5 years ago)",
    "5_Within the past 10 years (5 years but less than 10 year ago)",
    "6_10 years ago or more",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know"))) |>
mutate(fithwhen1_a = ordered(fithwhen1_a,
  levels = c(1,2,3,4,5,6,7,8,9),
  labels = c(
    "1_Within the past year (anytime less than 12 months ago)",
    "2_Within the past 2 years (1 year but less than 2 years ago)",
    "3_Within the past 3 years (2 years but less than 3 years ago)",
    "4_Within the past 5 years (3 years but less than 5 years ago)",
    "5_Within the past 10 years (5 years but less than 10 year ago)",
    "6_10 years ago or more",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know"))) |>
mutate(cguardwhe1_a = ordered(cguardwhe1_a,
  levels = c(1,2,3,4,5,6,7,8,9),
  labels = c(
    "1_Within the past year (anytime less than 12 months ago)",
    "2_Within the past 2 years (1 year but less than 2 years ago)",
    "3_Within the past 3 years (2 years but less than 3 years ago)",
    "4_Within the past 5 years (3 years but less than 5 years ago)",
    "5_Within the past 10 years (5 years but less than 10 year ago)",
    "6_10 years ago or more",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know"))) |>
mutate(psawhen_a = ordered(psawhen_a,
  levels = c(1,2,3,4,5,6,7,8,9),
  labels = c(
    "1_Within the past year (anytime less than 12 months ago)",
    "2_Within the past 2 years (1 year but less than 2 years ago)",
    "3_Within the past 3 years (2 years but less than 3 years ago)",
    "4_Within the past 5 years (3 years but less than 5 years ago)",
    "5_Within the past 10 years (5 years but less than 10 year ago)",
    "6_10 years ago or more",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know"))) |>
mutate(cervicwhen_a = ordered(cervicwhen_a,
  levels = c(1,2,3,4,5,6,7,8,9),
  labels = c(
    "1_Within the past year (anytime less than 12 months ago)",
    "2_Within the past 2 years (1 year but less than 2 years ago)",
    "3_Within the past 3 years (2 years but less than 3 years ago)",
    "4_Within the past 5 years (3 years but less than 5 years ago)",
    "5_Within the past 10 years (5 years but less than 10 year ago)",
    "6_10 years ago or more",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know"))) |>
mutate(mamwhen_a = ordered(mamwhen_a,
  levels = c(1,2,3,4,5,6,7,8,9),
  labels = c(
    "1_Within the past year (anytime less than 12 months ago)",
    "2_Within the past 2 years (1 year but less than 2 years ago)",
    "3_Within the past 3 years (2 years but less than 3 years ago)",
    "4_Within the past 5 years (3 years but less than 5 years ago)",
    "5_Within the past 10 years (5 years but less than 10 year ago)",
    "6_10 years ago or more",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know"))) |>
mutate(mriwhen_a = ordered(mriwhen_a,
  levels = c(1,2,3,4,5,6,7,8,9),
  labels = c(
    "1_Within the past year (anytime less than 12 months ago)",
    "2_Within the past 2 years (1 year but less than 2 years ago)",
    "3_Within the past 3 years (2 years but less than 3 years ago)",
    "4_Within the past 5 years (3 years but less than 5 years ago)",
    "5_Within the past 10 years (5 years but less than 10 year ago)",
    "6_10 years ago or more",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know")))

# val_labels(SA124X <- c(
# 1_Within the past year (anytime less than 12 months ago)",
# 2_Within the past 2 years (1 year but less than 2 years ago)",
# 3_Within the past 3 years (2 years but less than 3 years ago)",
# 4_Within the past 5 years (3 years but less than 5 years ago)",
# 5_Within the past 10 years (5 years but less than 10 year ago)",
# 6_10 years ago or more",
# 7_Refused",
# 8_Not Ascertained",
# 9_Don't Know"
# ))

# write a mutate function to replace the values in the data frame
# for the  variable hilast_a, denprev_a, lastdr_a, wellvis_a, 
# bplast_a, chollast_a, diblast1_a, diba1clast_a below with the numeric and value labels for SA125X found below, linked by an underscore, i.e. "1_Mentioned",
# by making it an ordered factor with the levels and labels below
# use consecutive integers starting at 1 for the levels with one level for each label

mutate(hilast_a = ordered(hilast_a,
  levels = c(1,2,3,4,5,6,7,8,9,10),
  labels = c(
    "0_Never",
    "1_Within the past year (anytime less than 12 months ago)",
    "2_Within the past 2 years (1 year but less than 2 years ago)",
    "3_Within the past 3 years (2 years but less than 3 years ago)",
    "4_Within the past 5 years (3 years but less than 5 years ago)",
    "5_Within the past 10 years (5 years but less than 10 year ago)",
    "6_10 years ago or more",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know"))) |>
mutate(denprev_a = ordered(denprev_a,
  levels = c(1,2,3,4,5,6,7,8,9,10),
  labels = c(
    "0_Never",
    "1_Within the past year (anytime less than 12 months ago)",
    "2_Within the past 2 years (1 year but less than 2 years ago)",
    "3_Within the past 3 years (2 years but less than 3 years ago)",
    "4_Within the past 5 years (3 years but less than 5 years ago)",
    "5_Within the past 10 years (5 years but less than 10 year ago)",
    "6_10 years ago or more",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know"))) |>
mutate(lastdr_a = ordered(lastdr_a,
  levels = c(1,2,3,4,5,6,7,8,9,10),
  labels = c(
    "0_Never",
    "1_Within the past year (anytime less than 12 months ago)",
    "2_Within the past 2 years (1 year but less than 2 years ago)",
    "3_Within the past 3 years (2 years but less than 3 years ago)",
    "4_Within the past 5 years (3 years but less than 5 years ago)",
    "5_Within the past 10 years (5 years but less than 10 year ago)",
    "6_10 years ago or more",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know"))) |>
mutate(wellvis_a = ordered(wellvis_a,
  levels = c(1,2,3,4,5,6,7,8,9,10),
  labels = c(
    "0_Never",
    "1_Within the past year (anytime less than 12 months ago)",
    "2_Within the past 2 years (1 year but less than 2 years ago)",
    "3_Within the past 3 years (2 years but less than 3 years ago)",
    "4_Within the past 5 years (3 years but less than 5 years ago)",
    "5_Within the past 10 years (5 years but less than 10 year ago)",
    "6_10 years ago or more",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know"))) |>
mutate(bplast_a = ordered(bplast_a,
  levels = c(1,2,3,4,5,6,7,8,9,10),
  labels = c(
    "0_Never",
    "1_Within the past year (anytime less than 12 months ago)",
    "2_Within the past 2 years (1 year but less than 2 years ago)",
    "3_Within the past 3 years (2 years but less than 3 years ago)",
    "4_Within the past 5 years (3 years but less than 5 years ago)",
    "5_Within the past 10 years (5 years but less than 10 year ago)",
    "6_10 years ago or more",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know"))) |>
mutate(chollast_a = ordered(chollast_a,
  levels = c(1,2,3,4,5,6,7,8,9,10),
  labels = c(
    "0_Never",
    "1_Within the past year (anytime less than 12 months ago)",
    "2_Within the past 2 years (1 year but less than 2 years ago)",
    "3_Within the past 3 years (2 years but less than 3 years ago)",
    "4_Within the past 5 years (3 years but less than 5 years ago)",
    "5_Within the past 10 years (5 years but less than 10 year ago)",
    "6_10 years ago or more",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know"))) |>
mutate(diblast1_a = ordered(diblast1_a,
  levels = c(1,2,3,4,5,6,7,8,9,10),
  labels = c(
    "0_Never",
    "1_Within the past year (anytime less than 12 months ago)",
    "2_Within the past 2 years (1 year but less than 2 years ago)",
    "3_Within the past 3 years (2 years but less than 3 years ago)",
    "4_Within the past 5 years (3 years but less than 5 years ago)",
    "5_Within the past 10 years (5 years but less than 10 year ago)",
    "6_10 years ago or more",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know"))) |>
mutate(diba1clast_a = ordered(diba1clast_a,
  levels = c(1,2,3,4,5,6,7,8,9,10),
  labels = c(
    "0_Never",
    "1_Within the past year (anytime less than 12 months ago)",
    "2_Within the past 2 years (1 year but less than 2 years ago)",
    "3_Within the past 3 years (2 years but less than 3 years ago)",
    "4_Within the past 5 years (3 years but less than 5 years ago)",
    "5_Within the past 10 years (5 years but less than 10 year ago)",
    "6_10 years ago or more",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know")))
                            
# val_labels(SA125X <- c(
# 0_Never",
# 1_Within the past year (anytime less than 12 months ago)",
# 2_Within the last 2 years (1 year but less than 2 years ago)",
# 3_Within the last 3 years (2 years but less than 3 years ago)",
# 4_Within the last 5 years (3 years but less than 5 years ago)",
# 5_Within the last 10 years (5 years but less than 10 years ago)",
# 6_10 years ago or more",
# 7_Refused",
# 8_Not Ascertained",
# 9_Don't Know"
# ))

# write a mutate function to replace the values in the data frame
# for the  variable houyrsliv_a below with the numeric and value labels for SA126X found below, linked by an underscore, i.e. "1_Mentioned",
# by making it an ordered factor with the levels and labels below
# use consecutive integers starting at 1 for the levels with one level for each label

mutate(houyrsliv_a = ordered(houyrsliv_a,
  levels = c(1,2,3,4,5,6,7,8,9),
  labels = c(
    "1_Less than 1 year",
    "2_1 to 3 years",
    "3_4 to 10 years",
    "4_11 to 20 years",
    "5_More than 20 years",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know")))

# val_labels(SA126X <- c(
# 1_Less than 1 year",
# 2_1 to 3 years",
# 3_4 to 10 years",
# 4_11 to 20 years",
# 5_More than 20 years",
# 7_Refused",
# 8_Not Ascertained",
# 9_Don't Know"
# ))

# write a mutate function to replace the values in the data frame
# for the  variable hrfiretotr_a below with the numeric and value labels for SA127X found below, linked by an underscore, i.e. "1_Mentioned",
# by making it an ordered factor with the levels and labels below
# use consecutive integers starting at 1 for the levels with one level for each label

mutate(hrfiretotr_a = ordered(hrfiretotr_a,
  levels = c(1,2,3,4,5,6,7),
  labels = c(
    "1_Less than 100 rounds",
    "2_100 to less than 1,000 rounds",
    "3_1,000 to less than 10,000 rounds",
    "4_10,000 rounds or more",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know")))

# val_labels(SA127X <- c(
# 1_Less than 100 rounds",
# 2_100 to less than 1,000 rounds",
# 3_1,000 to less than 10,000 rounds",
# 4_10,000 rounds or more",
# 7_Refused",
# 8_Not Ascertained",
# 9_Don't Know"
# ))

val_labels(SA128X <- c(
1_Often true",
2_Sometimes true",
3_Never true",
7_Refused",
8_Not Ascertained",
9_Don't Know"
))

# write a mutate function to replace the values in the data frame
# for the  variable usualpl_a below with the numeric and value labels for SA129X found below, linked by an underscore, i.e. "1_Mentioned",
# by making it an ordered factor with the levels and labels below
# use consecutive integers starting at 1 for the levels with one level for each label

mutate(usualpl_a = ordered(usualpl_a,
  levels = c(1,2,3,4,5,6),
  labels = c(
    "1_Yes",
    "2_There is NO place",
    "3_There is MORE THAN ONE place",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know")))


# val_labels(SA129X <- c(
# 1_Yes",
# 2_There is NO place",
# 3_There is MORE THAN ONE place",
# 7_Refused",
# 8_Not Ascertained",
# 9_Don't Know"
# ))

# write a mutate function to replace the values in the data frame
# for the variables proxy_a, hisp_a, afnow, hypev_a, hypdif_a, hyp12m_a, hypmed_a,
# chlev_a, chl12m_a, chlmed_a, chdev_a, 
# angev_a, miev_a, strev_a, aspmedev_a, aspmednown_a, aspmedstp_a, asponown_a, asev_a, astill_a, asat12m_a, aser12m_a,
# canev_a, predib_a, gesdib_a, dibev_a. dibpill_a, dibins_a, dibinstime_a, 
# dibinsstop_a, dibinsstyr_a, dibtype_a, copdev_a, arthev_a, demenev_a,
# anxev_a, depev_a, hepev_a, crohnsev_a, ulccolev_a, psorev_a. cfsev_a,
# cfsnow_a, medrxtrt_a, hlthcond_a, epiev_a, epimed_a, epinumsezp_a, epidr_a,
# vimdrev_a, vimlsdr_a, vimglev_a,vimlsgl_a, vimlsmd_a, vimcaev_a, pregnow_a,
# bmicat_a, wearglss_a, hearaid_a, hearaidfr_a, hearingdf_a, canewlkr_a, wchair_a,
# perasst_a, devdonset_a, sincovde_a, sincovvs_a, sincovrx_a, mcchoice_a, mchmo_a,
# mcdncov_a, mcvscov_a, mcpartd_a, maxchng_a, maprem_a, madeduc_a, polhld1_a,
# prplcov1_a, prplcov1_c_a, plnexchg1_a, prdeduc1_a, hsahra1_a, prrxcov1_a,
# prdncov1_a, prvscov1_a, pxchng1_a, prprem1_a, polhld2_a, prplcov2_a,
# prplcov2_c_a, plnexchg2_a, prdeduc2_a, hsahra2_a, prrxcov2_a, prdncov2_a,
# prvscov2_a, chxchng_a, chprem_a, chdeduc_a, opxchng_a, opprem_a, opdeduc_a,
# ogxchng_a, ogprem_a, ogdeduc_a, ihs_a, histopjob_a, histopmiss_a, histopage_a,
# histopcost_a, histopelig_a, rsnhicost_a, rsnhiwant_a, rsnhielig_a, rsnhiconf_a.
# rsnhimeet_a, rsnhiwait_a, rsnhijob_a, rsnhimiss_a, rsnhioth_a, hinotyr_a,
# hicov_a, mcareprb_a, mcaidprb_a,paybll12m_a, paynobllnw_a, evercovd_a,
# longcovd1_a, sympnow1_a, dendl12m_a, denng12m_a, wellness_a, hospongt_a,
# meddl12m_a, medng12m_a, virapp12m_a, accssint_a, accsshom_a, hitlook_a,
# hitcomm_a, hittest_a, rx12m_a, rxsk12m_a, rxls12m_a, rxdl12m_a, rxdg12m_a,
# colorectev_a, ctcolev1_a, fithev1_a. cologuard1_a, fitcolg1_a, colprob1_a,
# psatest_a, cervicev1_a, hystev2_a, mamev_a, mrihad_a, gtposs1_a, gtrisk_a, 
# fhcanev_a, fhbcanev_a, fhcanrisk_a, fhovcanev_a, pregfluyr_a, livebirth_a, 
# shtflu12m_a, shtcvd191_a, shtpnuev_a, shtshingl1_a, shingwhen1_a, shingrix3_a,
# tdappreg_a, shthepb1_a, livehep_a, workhealth_a, wrkhlthfc_a, travel_a, 
# eyeex12m_a, thera12m_a, homehc12m_a, anxmed_a, depmed_a, mhrx_a, mhthrpy_a,
# mhtpynow_a, mhthdly_a, mhthnd_a, repstrain_a, replimit_a, repsawdoc_a, 
# repfutwrk_a, repstopchg_a, repreduce_a, repwrkcaus_a, anyinjury_a, injlimit_a,
# injhome_a, injwork_a, injsports_a, injfall_a, injfallhom_a, injfallwrk_a,
# injmotor_a, injchores_a, injsawdoc_a, injer_a, injhosp_a, injbones_a,
# injstitch_a, injfutwrk_a, injstopchg_a, injreduce_a, tbilcdcmg_a, tbihlsbmc_a,
# tbisport_a, tbileague_a, tbieval_a, jntsymp_a, arthlmt_a, arthwrk_a, arthph_a,
# smkev_a, ecigev_a, cigarev_a, pipeev_a, smokelsev_a, avisreh_a, avisdev_a,
# avisadv_a, vimread_a, vimdrive_a, hrwhisp_a, earinfect_a, earinfect3_a,
#
#
# 
# 
# cbalhdinj_a, hrtest_a, baldizz_a, baldhp_a, bfall12m_a, hrtinnitus_a,
# hrtinmedsp_a, hrloudjob_a, hrloudjb12m_a, hrfireev_a, hrfire12m_a, hrvloud12m_a,
#  hrjbexp12m_a , hrjbexp4hr_a, hrjbexptb_a, spousliv_a, spousep_a, evrmarried_a,
#  spouswrk_a, spouswkft_a, prtnrwrk_a, prtnrwkft_a, afvet_a, afvettrn_a, 
#  combat_a, vadisb_a, vahosp_a, vacareev_a, natusborn_a, langhm_a, schcurenr_a,
#  emplastwk_a, empnowrk_a, empwrklsw1_a, empwrkft1_a, empsicklv_a, emphealins_a,
#  cevolun1_a, cevolun2_a, incwrko_a, incinter_a, incssrr_a, incssissdi_a,
#  ssissdidsb_a, incwelf_a, incretire_a, incothr_a, fsnap12m_a, fsnap30d_a,
#  fwic12m_a, flunch12m1_a, fdsskip_a,fdsless_a, fdshungry_a, fdsweight_a,
#  fdsnoteat_a, hougvasst_a, housecost_a, transpor_a, cemmetng_a, cevotelc_a#
#  
#  
# using SA130X below with the numeric and value labels for SA130X found below, linked by an underscore, i.e. "1_Mentioned",
# by making it an ordered factor with the levels and labels below
# use consecutive integers starting at 1 for the levels with one level for each label

mutate(proxy_a = ordered(proxy_a,
  levels = c(1,2,3,4,5),
  labels = c(
    "1_Yes",
    "2_No",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know"
  ))) |> 
    mutate(hisp_a = ordered(hisp_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
    mutate(afnow = ordered(afnow,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(hypev_a = ordered(hypev_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
mutate(hypdif_a = ordered(hypdif_a,
  levels = c(1,2,3,4,5),
  labels = c(
    "1_Yes",
    "2_No",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know"
  ))) |>
  mute(hyp12m_a = ordered(hyp12m_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(hypmed_a = ordered(hypmed_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(chlev_a = ordered(chlev_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(chl12m_a = ordered(chl12m_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(chlmed_a = ordered(chlmed_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(chdev_a = ordered(chdev_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |> # now add these variables canev_a, predib_a, gesdib_a, dibev_a. dibpill_a, dibins_a, dibinstime_a
  mutate(canev_a = ordered(canev_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(predib_a = ordered(predib_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(gesdib_a = ordered(gesdib_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(dibev_a = ordered(dibev_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(dibpill_a = ordered(dibpill_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(dibins_a = ordered(dibins_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |> # now add these variables dibinsstop_a, dibinsstyr_a, dibtype_a, copdev_a, arthev_a, demenev_a,
  mutate(dibinsstop_a = ordered(dibinsstop_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(dibinsstyr_a = ordered(dibinsstyr_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(dibtype_a = ordered(dibtype_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(copdev_a = ordered(copdev_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(arthev_a = ordered(arthev_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(demenev_a = ordered(demenev_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |> # now add these variables anxev_a, depev_a, hepev_a, crohnsev_a, ulccolev_a, psorev_a. cfsev_a,
  mutate(anxev_a = ordered(anxev_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(depev_a = ordered(depev_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(hepev_a = ordered(hepev_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(crohnsev_a = ordered(crohnsev_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(ulccolev_a = ordered(ulccolev_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(psorev_a = ordered(psorev_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(cfsev_a = ordered(cfsev_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |> # now add these variables cfsnow_a, medrxtrt_a, hlthcond_a, epiev_a, epimed_a, epinumsezp_a, epidr_a,
  mutate(cfsnow_a = ordered(cfsnow_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(medrxtrt_a = ordered(medrxtrt_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(hlthcond_a = ordered(hlthcond_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(epiev_a = ordered(epiev_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(epimed_a = ordered(epimed_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(epinumsezp_a = ordered(epinumsezp_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(epidr_a = ordered(epidr_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |> # now add these variables vimdrev_a, vimlsdr_a, vimglev_a,vimlsgl_a, vimlsmd_a, vimcaev_a, pregnow_a,
  mutate(vimdrev_a = ordered(vimdrev_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(vimlsdr_a = ordered(vimlsdr_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(vimglev_a = ordered(vimglev_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(vimlsgl_a = ordered(vimlsgl_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(vimlsmd_a = ordered(vimlsmd_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(vimcaev_a = ordered(vimcaev_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(pregnow_a = ordered(pregnow_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |> # now add these variables bmicat_a, wearglss_a, hearaid_a, hearaidfr_a, hearingdf_a, canewlkr_a, wchair_a,
  mutate(bmicat_a = ordered(bmicat_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(wearglss_a = ordered(wearglss_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(hearaid_a = ordered(hearaid_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(hearaidfr_a = ordered(hearaidfr_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(hearingdf_a = ordered(hearingdf_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(canewlkr_a = ordered(canewlkr_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(wchair_a = ordered(wchair_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |> # now add these variables erasst_a, devdonset_a, sincovde_a, sincovvs_a, sincovrx_a, mcchoice_a, mchmo_a,
  mutate(erasst_a = ordered(erasst_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |> 
  mutate(devdonset_a = ordered(devdonset_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(sincovde_a = ordered(sincovde_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(sincovvs_a = ordered(sincovvs_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(sincovrx_a = ordered(sincovrx_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(mcchoice_a = ordered(mcchoice_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(mchmo_a = ordered(mchmo_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |> # now add these variables mcdncov_a, mcvscov_a, mcpartd_a, maxchng_a, maprem_a, madeduc_a, polhld1_a,
  mutate(mcdncov_a = ordered(mcdncov_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(mcvscov_a = ordered(mcvscov_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(mcpartd_a = ordered(mcpartd_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(maxchng_a = ordered(maxchng_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(maprem_a = ordered(maprem_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(madeduc_a = ordered(madeduc_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(polhld1_a = ordered(polhld1_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |> # now add these variables prplcov1_a, prplcov1_c_a, plnexchg1_a, prdeduc1_a, hsahra1_a, prrxcov1_a,
  mutate(prplcov1_a = ordered(prplcov1_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(prplcov1_c_a = ordered(prplcov1_c_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(plnexchg1_a = ordered(plnexchg1_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(prdeduc1_a = ordered(prdeduc1_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(hsahra1_a = ordered(hsahra1_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(prrxcov1_a = ordered(prrxcov1_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |> # now add these variables prdncov1_a, prvscov1_a, pxchng1_a, prprem1_a, polhld2_a, prplcov2_a,
  mutate(prdncov1_a = ordered(prdncov1_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(prvscov1_a = ordered(prvscov1_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(pxchng1_a = ordered(pxchng1_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(prprem1_a = ordered(prprem1_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(polhld2_a = ordered(polhld2_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(prplcov2_a = ordered(prplcov2_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |> # now add these variables: prplcov2_c_a, plnexchg2_a, prdeduc2_a, hsahra2_a, prrxcov2_a, prdncov2_a,
  mutate(prplcov2_c_a = ordered(prplcov2_c_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(plnexchg2_a = ordered(plnexchg2_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(prdeduc2_a = ordered(prdeduc2_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(hsahra2_a = ordered(hsahra2_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(prrxcov2_a = ordered(prrxcov2_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(prdncov2_a = ordered(prdncov2_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |> # now add these variables: prvscov2_a, chxchng_a, chprem_a, chdeduc_a, opxchng_a, opprem_a, opdeduc_a,
  mutate(prvscov2_a = ordered(prvscov2_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(chxchng_a = ordered(chxchng_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(chprem_a = ordered(chprem_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(chdeduc_a = ordered(chdeduc_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(opxchng_a = ordered(opxchng_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(opprem_a = ordered(opprem_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(opdeduc_a = ordered(opdeduc_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |> # now add these variables: ogxchng_a, ogprem_a, ogdeduc_a, ihs_a, histopjob_a, histopmiss_a, histopage_a,
  mutate(ogxchng_a = ordered(ogxchng_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(ogprem_a = ordered(ogprem_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(ogdeduc_a = ordered(ogdeduc_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(ihs_a = ordered(ihs_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(histopjob_a = ordered(histopjob_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(histopmiss_a = ordered(histopmiss_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(histopage_a = ordered(histopage_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |> # now add these variables: histopcost_a, histopelig_a, rsnhicost_a, rsnhiwant_a, rsnhielig_a, rsnhiconf_a.
  mutate(histopcost_a = ordered(histopcost_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(histopelig_a = ordered(histopelig_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(rsnhicost_a = ordered(rsnhicost_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(rsnhiwant_a = ordered(rsnhiwant_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(rsnhielig_a = ordered(rsnhielig_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(rsnhiconf_a = ordered(rsnhiconf_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |> # now add these variables: rsnhimeet_a, rsnhiwait_a, rsnhijob_a, rsnhimiss_a, rsnhioth_a, hinotyr_a,
  mutate(rsnhimeet_a = ordered(rsnhimeet_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(rsnhiwait_a = ordered(rsnhiwait_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(rsnhijob_a = ordered(rsnhijob_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(rsnhimiss_a = ordered(rsnhimiss_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(rsnhioth_a = ordered(rsnhioth_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(hinotyr_a = ordered(hinotyr_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |> # now add these variables: hicov_a, mcareprb_a, mcaidprb_a,paybll12m_a, paynobllnw_a, evercovd_a,
  mutate(hicov_a = ordered(hicov_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(mcareprb_a = ordered(mcareprb_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(mcaidprb_a = ordered(mcaidprb_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(paybll12m_a = ordered(paybll12m_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(paynobllnw_a = ordered(paynobllnw_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(evercovd_a = ordered(evercovd_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |> # now add these variables: longcovd1_a, sympnow1_a, dendl12m_a, denng12m_a, wellness_a, hospongt_a,
  mutate(longcovd1_a = ordered(longcovd1_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(sympnow1_a = ordered(sympnow1_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(dendl12m_a = ordered(dendl12m_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(denng12m_a = ordered(denng12m_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(wellness_a = ordered(wellness_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(hospongt_a = ordered(hospongt_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |> # now add these variables: meddl12m_a, medng12m_a, virapp12m_a, accssint_a, accsshom_a, hitlook_a,
  mutate(meddl12m_a = ordered(meddl12m_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(medng12m_a = ordered(medng12m_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(virapp12m_a = ordered(virapp12m_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(accssint_a = ordered(accssint_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(accsshom_a = ordered(accsshom_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(hitlook_a = ordered(hitlook_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |> # now add these variables: hitcomm_a, hittest_a, rx12m_a, rxsk12m_a, rxls12m_a, rxdl12m_a, rxdg12m_a, colorectev_a, ctcolev1_a, fithev1_a. cologuard1_a, fitcolg1_a, colprob1_a, psatest_a, c
  mutate(hitcomm_a = ordered(hitcomm_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(hittest_a = ordered(hittest_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(rx12m_a = ordered(rx12m_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(rxsk12m_a = ordered(rxsk12m_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(rxls12m_a = ordered(rxls12m_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(rxdl12m_a = ordered(rxdl12m_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(rxdg12m_a = ordered(rxdg12m_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(colorectev_a = ordered(colorectev_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(ctcolev1_a = ordered(ctcolev1_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(fithev1_a = ordered(fithev1_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(cologuard1_a = ordered(cologuard1_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(fitcolg1_a = ordered(fitcolg1_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(colprob1_a = ordered(colprob1_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(psatest_a = ordered(psatest_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |> # now add these variables: cervicev1_a, hystev2_a, mamev_a, mrihad_a, gtposs1_a, gtrisk_a, 
  mutate(cervicev1_a = ordered(cervicev1_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(hystev2_a = ordered(hystev2_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(mamev_a = ordered(mamev_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(mrihad_a = ordered(mrihad_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(gtposs1_a = ordered(gtposs1_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(gtrisk_a = ordered(gtrisk_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |> # now add these variables: fhcanev_a, fhbcanev_a, fhcanrisk_a, fhovcanev_a, pregfluyr_a, livebirth_a, shtflu12m_a, shtcvd191_a, shtpnuev_a, shtshingl1_a, shingwhen1_a, shingrix3_a,
  mutate(fhcanev_a = ordered(fhcanev_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(fhbcanev_a = ordered(fhbcanev_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(fhcanrisk_a = ordered(fhcanrisk_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(fhovcanev_a = ordered(fhovcanev_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(pregfluyr_a = ordered(pregfluyr_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(livebirth_a = ordered(livebirth_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(shtflu12m_a = ordered(shtflu12m_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(shtcvd191_a = ordered(shtcvd191_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(shtpnuev_a = ordered(shtpnuev_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(shtshingl1_a = ordered(shtshingl1_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(shingwhen1_a = ordered(shingwhen1_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(shingrix3_a = ordered(shingrix3_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |> # now add these variables: tdappreg_a, shthepb1_a, livehep_a, workhealth_a, wrkhlthfc_a, travel_a, eyeex12m_a, thera12m_a, homehc12m_a, anxmed_a, depmed_a, mhrx_a, mhthrpy_a,
  mutate(tdappreg_a = ordered(tdappreg_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(shthepb1_a = ordered(shthepb1_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(livehep_a = ordered(livehep_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(workhealth_a = ordered(workhealth_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(wrkhlthfc_a = ordered(wrkhlthfc_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(travel_a = ordered(travel_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(eyeex12m_a = ordered(eyeex12m_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(thera12m_a = ordered(thera12m_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(homehc12m_a = ordered(homehc12m_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(anxmed_a = ordered(anxmed_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(depmed_a = ordered(depmed_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(mhrx_a = ordered(mhrx_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(mhthrpy_a = ordered(mhthrpy_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |> # now add these variables: mhtpynow_a, mhthdly_a, mhthnd_a, repstrain_a, replimit_a, repsawdoc_a, repfutwrk_a, repstopchg_a, repreduce_a, repwrkcaus_a, anyinjury_a, injlimit_a,
  mutate(mhtpynow_a = ordered(mhtpynow_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(mhthdly_a = ordered(mhthdly_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(mhthnd_a = ordered(mhthnd_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(repstrain_a = ordered(repstrain_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(replimit_a = ordered(replimit_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(repsawdoc_a = ordered(repsawdoc_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(repfutwrk_a = ordered(repfutwrk_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(repstopchg_a = ordered(repstopchg_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(repreduce_a = ordered(repreduce_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(repwrkcaus_a = ordered(repwrkcaus_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(anyinjury_a = ordered(anyinjury_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(injlimit_a = ordered(injlimit_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |> # now add these variables: injhome_a, injwork_a, injsports_a, injfall_a, injfallhom_a, injfallwrk_a, injmotor_a, injchores_a, injsawdoc_a, injer_a, injhosp_a, injbones_a,
  mutate(injhome_a = ordered(injhome_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(injwork_a = ordered(injwork_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(injsports_a = ordered(injsports_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(injfall_a = ordered(injfall_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(injfallhom_a = ordered(injfallhom_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(injfallwrk_a = ordered(injfallwrk_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(injmotor_a = ordered(injmotor_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(injchores_a = ordered(injchores_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(injsawdoc_a = ordered(injsawdoc_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(injer_a = ordered(injer_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(injhosp_a = ordered(injhosp_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(injbones_a = ordered(injbones_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |> # now add these variables: injstitch_a, injfutwrk_a, injstopchg_a, injreduce_a, tbilcdcmg_a, tbihlsbmc_a, tbisport_a, tbileague_a, tbieval_a, jntsymp_a, arthlmt_a, arthwrk_a, arthph_a,
  mutate(injstitch_a = ordered(injstitch_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(injfutwrk_a = ordered(injfutwrk_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(injstopchg_a = ordered(injstopchg_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(injreduce_a = ordered(injreduce_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(tbilcdcmg_a = ordered(tbilcdcmg_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(tbihlsbmc_a = ordered(tbihlsbmc_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(tbisport_a = ordered(tbisport_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(tbileague_a = ordered(tbileague_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(tbieval_a = ordered(tbieval_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(jntsymp_a = ordered(jntsymp_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(arthlmt_a = ordered(arthlmt_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(arthwrk_a = ordered(arthwrk_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(arthph_a = ordered(arthph_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |> # now add these variables: smkev_a, ecigev_a, cigarev_a, pipeev_a, smokelsev_a, avisreh_a, avisdev_a, avisadv_a, vimread_a, vimdrive_a, hrwhisp_a, earinfect_a, earinfect3_a,
  mutate(smkev_a = ordered(smkev_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(ecigev_a = ordered(ecigev_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(cigarev_a = ordered(cigarev_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(pipeev_a = ordered(pipeev_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(smokelsev_a = ordered(smokelsev_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(avisreh_a = ordered(avisreh_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(avisdev_a = ordered(avisdev_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(avisadv_a = ordered(avisadv_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(vimread_a = ordered(vimread_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(vimdrive_a = ordered(vimdrive_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(hrwhisp_a = ordered(hrwhisp_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(earinfect_a = ordered(earinfect_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |>
  mutate(earinfect3_a = ordered(earinfect3_a,
    levels = c(1,2,3,4,5),
    labels = c(
      "1_Yes",
      "2_No",
      "7_Refused",
      "8_Not Ascertained",
      "9_Don't Know"
    ))) |> # now add these variables: 
  
  
  
  
  
  
  
  
val_labels(SA130X <- c(
1_Yes",
2_No",
7_Refused",
8_Not Ascertained",
9_Don't Know"
))

# write a mutate function to replace the values in the data frame
# for the  variables cbalhdno_a with the numeric and value labels for SA131X found below, linked by an underscore, i.e. "1_Mentioned",
# by making it an ordered factor with the levels and labels below
# use consecutive integers starting at 1 for the levels with one level for each label

mutate(cbalhdno_a = ordered(cbalhdno_a,
  levels = c(1,2,3,4,5,6,7,8,9),
  labels = c(
    "0_0",
    "1_1",
    "2_2",
    "3_3",
    "4_4",
    "5_5 or more",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know")))

# val_labels(SA131X <- c(
# 0_0",
# 1_1",
# 2_2",
# 3_3",
# 4_4",
# 5_5 or more",
# 7_Refused",
# 8_Not Ascertained",
# 9_Don't Know"
# ))

# write a mutate function to replace the values in the data frame
# for the  variables flupreg_a with the numeric and value labels for SA132X found below, linked by an underscore, i.e. "1_Mentioned",
# by making it an ordered factor with the levels and labels below
# use consecutive integers starting at 1 for the levels with one level for each label

mutate(flupreg_a = ordered(flupreg_a,
  levels = c(1,2,3,4,5),
  labels = c(
    "1_Before pregnancy",
    "2_During pregnancy",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know")))

# val_labels(SA132X <- c(
# 1_Before pregnancy",
# 2_During pregnancy",
# 7_Refused",
# 8_Not Ascertained",
# 9_Don't Know"
# ))

# write a mutate function to replace the values in the data frame
# for the  variables flupreg2_a with the numeric and value labels for SA133X found below, linked by an underscore, i.e. "1_Mentioned",
# by making it an ordered factor with the levels and labels below
# use consecutive integers starting at 1 for the levels with one level for each label

mutate(flupreg2_a = ordered(flupreg2_a,
  levels = c(1,2,3,4,5,6),
  labels = c(
    "1_Before pregnancy",
    "2_During pregnancy",
    "3_After pregnancy",
    "7_Refused",
    "8_Not Ascertained",
    "9_Don't Know")))  

# val_labels(SA133X <- c(
# 1_Before pregnancy",
# 2_During pregnancy",
# 3_After pregnancy",
# 7_Refused",
# 8_Not Ascertained",
# 9_Don't Know"
# ))

#  associate variables with value label definitions------


# ASSOCIATE VARIABLES WITH VALUE LABEL DEFINITION  

# write a mutate statement for the 'data' dataset for each variable in the
# IDN VALUE LABEL ASSOCIATIONS below, changing each of these variables
# to an ordered factor, and assigning the labels defined above to the levels


# write a mutate statement for the 'data' dataset for the rectype variable in the
# IDN VALUE LABEL ASSOCIATIONS below, changing each of these variables
# to an ordered factor, and assigning the labels defined above to the levels

data |>
  mutate(rectype_ordered(rectype, levels_c(10,20,30,40,50), 
                           labels_SA047X_labels))
      
* IDN  VALUE LABEL ASSOCIATIONS;
label values  rectype          SA047X; 

* UCF  VALUE LABEL ASSOCIATIONS;
label values  urbrrl           SA052X; 
label values   region           SA048X; 

* FLG  VALUE LABEL ASSOCIATIONS;
label values  hhrespsa_flg     SA056X; 

* GEN  VALUE LABEL ASSOCIATIONS;
label values  intv_qrt         SA022X;
label values   intv_mon         SA021X;
label values  hhstat_a         SA015X; 
label values   astatnew         SA122X;
label values  avail_a          SA114X; 
label values   proxy_a          SA130X;
label values  proxyrel_a       SA110X; 

* HHC  VALUE LABEL ASSOCIATIONS;
label values  sex_a            SA118X; 
label values   age65            SA058X;
label values  hisp_a           SA130X; 
label values   hispallp_a       SA017X;
label values  raceallp_a       SA045X; 
label values   hisdetp_a        SA016X;
label values  afnow            SA130X; 
label values   educp_a          SA074X;
label values  pcnt18uptc       SA038X; 
label values   pcntlt18tc       SA041X; 

* FAM  VALUE LABEL ASSOCIATIONS;
label values  pcntadlt_a       SA039X; 
label values   pcntkids_a       SA041X;
label values  over65flg_a      SA030X; 
label values   mltfamflg_a      SA029X;
label values  maxeducp_a       SA074X; 

* HIS  VALUE LABEL ASSOCIATIONS;
label values  phstat_a         SA107X; 

* LSF  VALUE LABEL ASSOCIATIONS;
label values  lsatis4_a        SA115X; 

* HYP  VALUE LABEL ASSOCIATIONS;
label values  hypev_a          SA130X; 
label values   hypdif_a         SA130X;
label values  hyp12m_a         SA130X; 
label values   hypmed_a         SA130X; 

* CHL  VALUE LABEL ASSOCIATIONS;
label values  chlev_a          SA130X; 
label values   chl12m_a         SA130X;
label values  chlmed_a         SA130X; 

* CVC  VALUE LABEL ASSOCIATIONS;
label values  chdev_a          SA130X; 
label values   angev_a          SA130X;
label values  miev_a           SA130X; 
label values   strev_a          SA130X; 

* ASP  VALUE LABEL ASSOCIATIONS;
label values  aspmedev_a       SA130X; l
abel values   aspmednown_a     SA130X;
label values  aspmedstp_a      SA130X; 
label values   asponown_a       SA130X; 

* AST  VALUE LABEL ASSOCIATIONS;
label values  asev_a           SA130X; 
label values   astill_a         SA130X;
label values  asat12m_a        SA130X; 
label values   aser12m_a        SA130X; 

* CAN  VALUE LABEL ASSOCIATIONS;
label values  canev_a          SA130X; 
label values   bladdcan_a       SA028X;
label values  bloodcan_a       SA028X; 
label values   bonecan_a        SA028X;
label values  braincan_a       SA028X; 
label values   breascan_a       SA028X;
label values  cervican_a       SA028X; 
label values   coloncan_a       SA028X;
label values  esophcan_a       SA028X; 
label values   gallbcan_a       SA028X;
label values  laryncan_a       SA028X; 
label values   leukecan_a       SA028X;
label values  livercan_a       SA028X; 
label values   lungcan_a        SA028X;
label values  lymphcan_a       SA028X; 
label values   melancan_a       SA028X;
label values  mouthcan_a       SA028X; 
label values   ovarycan_a       SA028X;
label values  pancrcan_a       SA028X; 
label values   prostcan_a       SA028X;
label values  rectucan_a       SA028X; 
label values   sknmcan_a        SA028X;
label values  sknnmcan_a       SA028X; 
label values   skndkcan_a       SA028X;
label values  stomacan_a       SA028X; 
label values   throacan_a       SA028X;
label values  thyrocan_a       SA028X; 
label values   uterucan_a       SA028X;
label values  hdnckcan_a       SA028X; 
label values   colrccan_a       SA028X;
label values  othercanp_a      SA028X; 

* DIB  VALUE LABEL ASSOCIATIONS;
label values  predib_a         SA130X; 
label values   gesdib_a         SA130X;
label values  dibev_a          SA130X; 
label values   dibpill_a        SA130X;
label values  dibins_a         SA130X; 
label values   dibinstime_a     SA071X;
label values  dibinsstop_a     SA130X; 
label values   dibinsstyr_a     SA130X;
label values  dibtype_a        SA072X; 

* CON  VALUE LABEL ASSOCIATIONS;
label values  copdev_a         SA130X; 
label values   arthev_a         SA130X;
label values  demenev_a        SA130X; 
label values   anxev_a          SA130X;
label values  depev_a          SA130X; 

* SCE  VALUE LABEL ASSOCIATIONS;
label values  hepev_a          SA130X; 
label values   crohnsev_a       SA130X;
label values  ulccolev_a       SA130X; 
label values   psorev_a         SA130X; 

* CFS  VALUE LABEL ASSOCIATIONS;
label values  cfsev_a          SA130X; 
label values   cfsnow_a         SA130X; 

* ISN  VALUE LABEL ASSOCIATIONS;
label values  medrxtrt_a       SA130X; 
label values   hlthcond_a       SA130X; 

* EPI  VALUE LABEL ASSOCIATIONS;
label values  epiev_a          SA130X; 
label values   epimed_a         SA130X;
label values  epinumsezp_a     SA079X; 
label values   epidr_a          SA130X; 

* SVC  VALUE LABEL ASSOCIATIONS;
label values  vimdrev_a        SA130X; 
label values   vimlsdr_a        SA130X;
label values  vimglev_a        SA130X; 
label values   vimlsgl_a        SA130X;
label values  vimmdev_a        SA130X; 
label values   vimlsmd_a        SA130X;
label values  vimcsurg_a       SA130X; 
label values   vimcaev_a        SA130X;
label values  vimlsca_a        SA130X; 

* BMI  VALUE LABEL ASSOCIATIONS;
label values  pregnow_a        SA130X; 
label values   bmicat_a         SA001X; 

* VIS  VALUE LABEL ASSOCIATIONS;
label values  wearglss_a       SA130X; 
label values   visiondf_a       SA073X; 

* HEA  VALUE LABEL ASSOCIATIONS;
label values  hearaid_a        SA130X; 
label values   hearaidfr_a      SA083X;
label values  hearingdf_a      SA073X; 

* MOB  VALUE LABEL ASSOCIATIONS;
label values  diff_a           SA073X; 
label values   equip_a          SA130X;
label values  wlk100_a         SA073X; 
label values   wlk13m_a         SA073X;
label values  steps_a          SA073X; 
label values   canewlkr_a       SA130X;
label values  wchair_a         SA130X; 
label values   perasst_a        SA130X;
label values  noeqwlk100_a     SA073X; 
label values   noeqwlk13m_a     SA073X;
label values  noeqsteps_a      SA073X; 
label values   eqwlk100_a       SA073X;
label values  eqwlk13m_a       SA073X; 
label values   eqsteps_a        SA073X; 

* COM  VALUE LABEL ASSOCIATIONS;
label values  comdiff_a        SA073X; 

* COG  VALUE LABEL ASSOCIATIONS;
label values  cogmemdff_a      SA073X; 
label values   cogtypedff_a     SA063X;
label values  cogfrqdff_a      SA062X; 
label values   cogamtdff_a      SA061X; 

* UPP  VALUE LABEL ASSOCIATIONS;
label values  uppslfcr_a       SA073X; 
label values   uppraise_a       SA073X;
label values  uppobjct_a       SA073X; 
label values   disab3_a         SA053X; 

* SOC  VALUE LABEL ASSOCIATIONS;
label values  socerrnds_a      SA073X; 
label values   socsclpar_a      SA073X;
label values  socwrklim_a      SA130X; 

* ADO  VALUE LABEL ASSOCIATIONS;
label values  devdonset_a      SA130X; 

* INS  VALUE LABEL ASSOCIATIONS;
label values  notcov_a         SA031X; 
label values   cover_a          SA004X;
label values  cover65_a        SA005X; 
label values   sincovde_a       SA130X;
label values  sincovvs_a       SA130X; 
label values   sincovrx_a       SA130X;
label values  medicare_a       SA020X; 
label values   mcpart_a         SA096X;
label values  mcchoice_a       SA130X; 
label values   mchmo_a          SA130X;
label values  mcadvr_a         SA027X; 
label values   mcdncov_a        SA130X;
label values  mcvscov_a        SA130X; 
label values   mcpartd_a        SA130X;
label values  medicaid_a       SA020X; 
label values   maxchng_a        SA130X;
label values  maprem_a         SA130X; 
label values   madeduc_a        SA130X;
label values  mahdhp_a         SA044X; 
label values   maflg_a          SA025X;
label values  private_a        SA020X; 
label values   exchange_a       SA010X;
label values  polhld1_a        SA130X; 
label values   prplcov1_a       SA130X;
label values  prpolh1_a        SA111X; 
label values   prplcov1_c_a     SA130X;
label values  plnwrkr1_a       SA042X; 
label values   plnexchg1_a      SA130X;
label values  pln1pay1_a       SA098X; 
label values   pln1pay2_a       SA098X;
label values  pln1pay3_a       SA098X; 
label values   pln1pay4_a       SA098X;
label values  pln1pay5_a       SA098X; 
label values   pln1pay6_a       SA098X;
label values  prdeduc1_a       SA130X; 
label values   prhdhp1_a        SA109X;
label values  hsahra1_a        SA130X; 
label values   prrxcov1_a       SA130X;
label values  prdncov1_a       SA130X; 
label values   prvscov1_a       SA130X;
label values  exchpr1_a        SA011X; 
label values   prflg_a          SA043X;
label values  pxchng1_a        SA130X; 
label values   prprem1_a        SA130X;
label values  plexchpr1_a      SA011X; 
label values   polhld2_a        SA130X;
label values  prplcov2_a       SA130X; 
label values   prpolh2_a        SA111X;
label values  prplcov2_c_a     SA130X; 
label values   plnwrkr2_a       SA042X;
label values  plnexchg2_a      SA130X; 
label values   pln2pay1_a       SA098X;
label values  pln2pay2_a       SA098X; 
label values   pln2pay3_a       SA098X;
label values  pln2pay4_a       SA098X; 
label values   pln2pay5_a       SA098X;
label values  pln2pay6_a       SA098X; 
label values   prdeduc2_a       SA130X;
label values  prhdhp2_a        SA109X; 
label values   hsahra2_a        SA130X;
label values  prrxcov2_a       SA130X; 
label values   prdncov2_a       SA130X;
label values  prvscov2_a       SA130X; 
label values   exchpr2_a        SA011X;
label values  chip_a           SA020X; 
label values   chxchng_a        SA130X;
label values  chprem_a         SA130X; 
label values   chdeduc_a        SA130X;
label values  chhdhp_a         SA044X; 
label values   chflg_a          SA002X;
label values  othpub_a         SA020X; 
label values   opxchng_a        SA130X;
label values  opprem_a         SA130X; 
label values   opdeduc_a        SA130X;
label values  ophdhp_a         SA044X; 
label values   plexchop_a       SA011X;
label values  opflg_a          SA036X; 
label values   othgov_a         SA020X;
label values  ogxchng_a        SA130X; 
label values   ogprem_a         SA130X;
label values  ogdeduc_a        SA130X; 
label values   oghdhp_a         SA044X;
label values  plexchog_a       SA011X; 
label values   ogflg_a          SA034X;
label values  military_a       SA020X; 
label values   milspc1r_a       SA098X;
label values  milspc2_a        SA098X; 
label values   milspc3_a        SA098X;
label values  ihs_a            SA130X; 
label values   hilast_a         SA125X;
label values  hilastmy_a       SA100X; 
label values   histopjob_a      SA130X;
label values  histopmiss_a     SA130X; 
label values   histopage_a      SA130X;
label values  histopcost_a     SA130X; 
label values   histopelig_a     SA130X;
label values  rsnhicost_a      SA130X; 
label values   rsnhiwant_a      SA130X;
label values  rsnhielig_a      SA130X; 
label values   rsnhiconf_a      SA130X;
label values  rsnhimeet_a      SA130X; 
label values   rsnhiwait_a      SA130X;
label values  rsnhioth_a       SA130X; 
label values   rsnhijob_a       SA130X;
label values  rsnhimiss_a      SA130X; 
label values   hinotyr_a        SA130X;
label values  hinotmyr_a       SA100X; 
label values   milspc1_a        SA098X;
label values  hicov_a          SA130X; 
label values   hikind01_a       SA098X;
label values  hikind02_a       SA098X; 
label values   hikind03_a       SA098X;
label values  hikind04_a       SA098X; 
label values   hikind05_a       SA098X;
label values  hikind06_a       SA098X; 
label values   hikind07_a       SA098X;
label values  hikind08_a       SA098X; 
label values   hikind09_a       SA098X;
label values  hikind10_a       SA098X; 
label values   mcareprb_a       SA130X;
label values  mcaidprb_a       SA130X; 

* PAY  VALUE LABEL ASSOCIATIONS;
label values  paybll12m_a      SA130X; 
label values   paynobllnw_a     SA130X;
label values  payworry_a       SA059X; 

* CVL  VALUE LABEL ASSOCIATIONS;
label values  evercovd_a       SA130X; 
label values   longcovd1_a      SA130X;
label values  sympnow1_a       SA130X; 
label values   lcvdact_a        SA093X; 

* DNC  VALUE LABEL ASSOCIATIONS;
label values  denprev_a        SA125X; 
label values   dendl12m_a       SA130X;
label values  denng12m_a       SA130X; 

* UTZ  VALUE LABEL ASSOCIATIONS;
label values  lastdr_a         SA125X; 
label values   wellness_a       SA130X;
label values  wellvis_a        SA125X; 
label values   usualpl_a        SA129X;
label values  usplkind_a       SA108X; 
label values   urgnt12mtc_a     SA051X;
label values  emerg12mtc_a     SA009X; 
label values   hospongt_a       SA130X;
label values  meddl12m_a       SA130X; 
label values   medng12m_a       SA130X; 

* TLH  VALUE LABEL ASSOCIATIONS;
label values  virapp12m_a      SA130X; 

* HIT  VALUE LABEL ASSOCIATIONS;
label values  accssint_a       SA130X; 
label values   accsshom_a       SA130X;
label values  hitlook_a        SA130X; 
label values   hitcomm_a        SA130X;
label values  hittest_a        SA130X; 

* PMD  VALUE LABEL ASSOCIATIONS;
label values  rx12m_a          SA130X; 
label values   rxsk12m_a        SA130X;
label values  rxls12m_a        SA130X; 
label values   rxdl12m_a        SA130X;
label values  rxdg12m_a        SA130X; 

* PRV  VALUE LABEL ASSOCIATIONS;
label values  bplast_a         SA125X; 
label values   chollast_a       SA125X;
label values  diblast1_a       SA125X; 
label values   diba1clast_a     SA125X;
label values  diba1cnmt_a      SA006X; 
label values   colorectev_a     SA130X;
label values  colorectyp_a     SA064X; 
label values   colwhen_a        SA124X;
label values  colreason1_a     SA065X; 
label values   colsigwhen_a     SA124X;
label values  sigwhen_a        SA124X; 
label values   ctcolev1_a       SA130X;
label values  ctcolwhen1_a     SA124X; 
label values   fithev1_a        SA130X;
label values  fithwhen1_a      SA124X; 
label values   cologuard1_a     SA130X;
label values  fitcolg1_a       SA130X; 
label values   cguardwhe1_a     SA124X;
label values  colprob1_a       SA130X; 
label values   coltest1_a       SA098X;
label values  coltest2_a       SA098X; 
label values   coltest3_a       SA098X;
label values  coltest4_a       SA098X; 
label values   coltest5_a       SA098X;
label values  coltest6_a       SA098X; 
label values   psatest_a        SA130X;
label values  psawhen_a        SA124X; 
label values   psareason_a      SA080X;
label values  psa5yr1_a        SA112X; 
label values   cervicev1_a      SA130X;
label values  cervicwhen_a     SA124X; 
label values   hystev2_a        SA130X;
label values  mamev_a          SA130X; 
label values   mamwhen_a        SA124X;
label values  mamreason_a      SA080X; 
label values   mamnot1_a        SA094X;
label values  mrihad_a         SA130X; 
label values   mriwhen_a        SA124X;
label values  mrirea_a         SA101X; 

* GCT  VALUE LABEL ASSOCIATIONS;
label values  gtposs1_a        SA130X; 
label values   gtgrisk_a        SA130X; 

* FHC  VALUE LABEL ASSOCIATIONS;
label values  fhcanev_a        SA130X; 
label values   fhbcanev_a       SA130X;
label values  fhbcannum_a      SA104X; 
label values   fhbcan50_a       SA105X;
label values  fhovcanev_a      SA130X; 
label values   fhovcannum_a     SA104X;
label values  fhcanrisk_a      SA130X; 

* IMS  VALUE LABEL ASSOCIATIONS;
label values  pregfluyr_a      SA130X; 
label values   livebirth_a      SA130X;
label values  shtflu12m_a      SA130X; 
label values   shtflum_a        SA099X;
label values  flupreg_a        SA132X; 
label values   flupreg2_a       SA133X;
label values  shtcvd191_a      SA130X; 
label values   shtcvd19nm1_a    SA066X;
label values  cvdvac1m1_a      SA099X; 
label values   shottype2_a      SA067X;
label values  shtpnuev_a       SA130X; 
label values   shtpneunb_a      SA113X;
label values  shtshingl1_a     SA130X; 
label values   shingwhen1_a     SA130X;
label values  shingrix3_a      SA130X; 
label values   shingrixn3_a     SA120X;
label values  shingrixfs1_a    SA119X; 
label values   tdappreg_a       SA130X;
label values  shthepb1_a       SA130X; 
label values   livehep_a        SA130X;
label values  workhealth_a     SA130X; 
label values   wrkhlthfc_a      SA130X;
label values  travel_a         SA130X; 

* PTC  VALUE LABEL ASSOCIATIONS;
label values  eyeex12m_a       SA130X; 
label values   thera12m_a       SA130X;
label values  homehc12m_a      SA130X; 

* ANX  VALUE LABEL ASSOCIATIONS;
label values  anxfreq_a        SA089X; 
label values   anxmed_a         SA130X;
label values  anxlevel_a       SA092X; 

* DEP  VALUE LABEL ASSOCIATIONS;
label values  depfreq_a        SA089X; 
label values   depmed_a         SA130X;
label values  deplevel_a       SA092X; 

* MHC  VALUE LABEL ASSOCIATIONS;
label values  mhrx_a           SA130X; 
label values   mhthrpy_a        SA130X;
label values  mhtpynow_a       SA130X; 
label values   mhthdly_a        SA130X;
label values  mhthnd_a         SA130X; 

* MHA  VALUE LABEL ASSOCIATIONS;
label values  phq41_a          SA106X; 
label values   phq42_a          SA106X;
label values  phq2screen_a     SA014X; 
label values   phq43_a          SA106X;
label values  phq44_a          SA106X; 
label values   gad2screen_a     SA014X; 

* EDS  VALUE LABEL ASSOCIATIONS;
label values  discrim1_a       SA090X; 
label values   discrim2_a       SA090X;
label values  discrim3_a       SA090X; 
label values   discrim4_a       SA090X;
label values  discrim5_a       SA090X; 

* HVS  VALUE LABEL ASSOCIATIONS;
label values  vigil1_a         SA090X; 
label values   vigil2_a         SA090X;
label values  vigil3_a         SA090X; 
label values   vigil4_a         SA090X; 

* PAI  VALUE LABEL ASSOCIATIONS;
label values  paifrq3m_a       SA088X; 
label values   paiamnt_a        SA092X;
label values  paiwklm3m_a      SA088X; 
label values   paiaffm3m_a      SA088X;
label values  paiback3m_a      SA102X; 
label values   paiulmb3m_a      SA102X;
label values  paillmb3m_a      SA102X; 
label values   paihdfc3m_a      SA102X;
label values  paiapg3m_a       SA102X; 
label values   paitooth3m_a     SA102X; 

* REP  VALUE LABEL ASSOCIATIONS;
label values  repstrain_a      SA130X; 
label values   replimit_a       SA130X;
label values  repsawdoc_a      SA130X; 
label values   repfutwrk_a      SA130X;
label values  repstopchg_a     SA130X; 
label values   repreduce_a      SA130X;
label values  repwrkcaus_a     SA130X; 

* INJ  VALUE LABEL ASSOCIATIONS;
label values  anyinjury_a      SA130X; 
label values   injlimit_a       SA130X;
label values  injhome_a        SA130X; 
label values   injwork_a        SA130X;
label values  injsports_a      SA130X; 
label values   injfall_a        SA130X;
label values  injfallhom_a     SA130X; 
label values   injfallwrk_a     SA130X;
label values  injmotor_a       SA130X; 
label values   injmvtype1_a     SA098X;
label values  injmvtype2_a     SA098X; 
label values   injmvtype3_a     SA098X;
label values  injmvtype4_a     SA098X; 
label values   injmvtype5_a     SA098X;
label values  injchores_a      SA130X; 
label values   injsawdoc_a      SA130X;
label values  injer_a          SA130X; 
label values   injhosp_a        SA130X;
label values  injbones_a       SA130X; 
label values   injstitch_a      SA130X;
label values  injfutwrk_a      SA130X; 
label values   injstopchg_a     SA130X;
label values  injreduce_a      SA130X; 

* TBI  VALUE LABEL ASSOCIATIONS;
label values  tbilcdcmg_a      SA130X; 
label values   tbihlsbmc_a      SA130X;
label values  tbisport_a       SA130X; 
label values   tbileague_a      SA130X;
label values  tbieval_a        SA130X; 

* ART  VALUE LABEL ASSOCIATIONS;
label values  jntsymp_a        SA130X; 
label values   jntpn_a          SA116X;
label values  arthlmt_a        SA130X; 
label values   arthwrk_a        SA130X;
label values  arthph_a         SA130X; 

* CIG  VALUE LABEL ASSOCIATIONS;
label values  smkev_a          SA130X; 
label values   smknow_a         SA081X;
label values  smkcigst_a       SA050X; 
label values   cignow_a         SA070X;
label values  smk30d_a         SA068X; 
label values   cig30d_a         SA070X;
label values  mentholc_a       SA097X; 
label values   ecigev_a         SA130X;
label values  ecignow_a        SA081X; 
label values   smkecigst_a      SA007X; 

* OTB  VALUE LABEL ASSOCIATIONS;
label values  cigarev_a        SA130X; 
label values   cigarcur_a       SA081X;
label values  cigar30d_a       SA068X; 
label values   pipeev_a         SA130X;
label values  pipecur_a        SA081X; 
label values   smokelsev_a      SA130X;
label values  smokelscur_a     SA081X; 

* SVI  VALUE LABEL ASSOCIATIONS;
label values  avisexam_a       SA123X; 
label values   avisreh_a        SA130X;
label values  avisdev_a        SA130X; 
label values   avissadv_a       SA130X;
label values  vimread_a        SA130X; 
label values   vimdrive_a       SA130X; 

* SHE  VALUE LABEL ASSOCIATIONS;
label values  ahearst1_a       SA084X; 
label values   hrwhisp_a        SA130X;
label values  earinfect_a      SA130X; 
label values   earinfect3_a     SA130X;
label values  cbalhdinj_a      SA130X; 
label values   cbalhdno_a       SA131X;
label values  hrtest_a         SA130X; 
label values   hrtestlast_a     SA086X;
label values  hraidaqr_a       SA082X; 
label values   baldizz_a        SA130X;
label values  baldprob_a       SA060X; 
label values   baldhp_a         SA130X;
label values  bfall12m_a       SA130X; 
label values   bfalltimes_a     SA103X;
label values  hrtinnitus_a     SA130X; 
label values   hrtinlng_a       SA087X;
label values  hrtinprob_a      SA060X; 
label values   hrtinmedsp_a     SA130X;
label values  hrloudjob_a      SA130X; 
label values   hrloudjbyr_a     SA087X;
label values  hrloudjb12m_a    SA130X; 
label values   hrjobprot_a      SA117X;
label values  hrfireev_a       SA130X; 
label values   hrfiretotr_a     SA127X;
label values  hrfire12m_a      SA130X; 
label values   hrfireprot_a     SA117X;
label values  hrvloud12m_a     SA130X; 
label values   hrvldprot_a      SA117X; 

* SWE  VALUE LABEL ASSOCIATIONS;
label values  hrjbexp12m_a     SA130X; 
label values   hrjbexp4hr_a     SA130X;
label values  hrjbexptb_a      SA130X; 

* ORN  VALUE LABEL ASSOCIATIONS;
label values  orient_a         SA035X; 

* MAR  VALUE LABEL ASSOCIATIONS;
label values  marital_a        SA095X; 
label values   spousliv_a       SA130X;
label values  spousep_a        SA130X; 
label values   evrmarried_a     SA130X;
label values  marstat_a        SA026X; 
label values   legmstat_a       SA024X;
label values  spousesex_a      SA118X; 
label values   saspprace_a      SA055X;
label values  saspphisp_a      SA055X; 
label values   spouseducp_a     SA008X;
label values  spouswrk_a       SA130X; 
label values   spouswkft_a      SA130X;
label values  prtnrsex_a       SA118X; 
label values   prtnreducp_a     SA008X;
label values  prtnrwrk_a       SA130X; 
label values   prtnrwkft_a      SA130X;
label values  saparentsc_a     SA049X; 
label values   parstat_a        SA037X; 

* VET  VALUE LABEL ASSOCIATIONS;
label values  afvet_a          SA130X; 
label values   afvettrn_a       SA130X;
label values  combat_a         SA130X; 
label values   vadisb_a         SA130X;
label values  vahosp_a         SA130X; 
label values   vacareev_a       SA130X; 

* NAT  VALUE LABEL ASSOCIATIONS;
label values  natusborn_a      SA130X; 
label values   yrsinus_a        SA057X;
label values  citznstp_a       SA003X; 

* LNG  VALUE LABEL ASSOCIATIONS;
label values  langhm_a         SA130X; 
label values   langspecr_a      SA023X;
label values  langmed_a        SA091X; 
label values   langdoc_a        SA091X;
label values  langsoc_a        SA091X; 

* SCH  VALUE LABEL ASSOCIATIONS;
label values  schcurenr_a      SA130X; 

* EMP  VALUE LABEL ASSOCIATIONS;
label values  emplastwk_a      SA130X; 
label values   empnowrk_a       SA130X;
label values  empwhynot_a      SA078X; 
label values   empwhenwrk_a     SA077X;
label values  empwrklsw1_a     SA130X; 
label values   emplstwor1_a     SA076X;
label values  empwrkft1_a      SA130X; 
label values   empsicklv_a      SA130X;
label values  emphealins_a     SA130X; 

* EMD  VALUE LABEL ASSOCIATIONS;
label values  emdindstn1_a     SA018X; 
label values   emdindstn2_a     SA019X;
label values  emdoccupn1_a     SA032X; 
label values   emdoccupn2_a     SA033X;
label values  emdsuper_a       SA130X; 
label values   emdwrkcat1_a     SA075X; 

* VOL  VALUE LABEL ASSOCIATIONS;
label values  cevolun1_a       SA130X; 
label values   cevolun2_a       SA130X; 

* FEM  VALUE LABEL ASSOCIATIONS;
label values  pcntadwkp1_a     SA040X; 
label values   pcntadwfp1_a     SA040X; 

* INC  VALUE LABEL ASSOCIATIONS;
label values  incwrko_a        SA130X; 
label values   incinter_a       SA130X;
label values  incssrr_a        SA130X; 
label values   incssissdi_a     SA130X;
label values  ssissdibth_a     SA121X; 
label values   ssissdidsb_a     SA130X;
label values  incwelf_a        SA130X; 
label values   incretire_a      SA130X;
label values  incothr_a        SA130X; 
label values   ratcat_a         SA046X;
label values  inctcflg_a       SA054X; 

* FOO  VALUE LABEL ASSOCIATIONS;
label values  fsnap12m_a       SA130X; 
label values   fsnap30d_a       SA130X;
label values  fwic12m_a        SA130X; 
label values   flunch12m1_a     SA130X; 

* FDS  VALUE LABEL ASSOCIATIONS;
label values  fdsrunout_a      SA128X; 
label values   fdslast_a        SA128X;
label values  fdsbalance_a     SA128X; 
label values   fdsskip_a        SA130X;
label values  fdsskipdys_a     SA069X; 
label values   fdsless_a        SA130X;
label values  fdshungry_a      SA130X; 
label values   fdsweight_a      SA130X;
label values  fdsnoteat_a      SA130X; 
label values   fdsnedays_a      SA069X;
label values  fdscat3_a        SA012X; 
label values   fdscat4_a        SA013X; 

* HOU  VALUE LABEL ASSOCIATIONS;
label values  houyrsliv_a      SA126X; 
label values   houtenure_a      SA085X;
label values  hougvasst_a      SA130X; 

* SDH  VALUE LABEL ASSOCIATIONS;
label values  housecost_a      SA130X; 

* TBH  VALUE LABEL ASSOCIATIONS;
label values  transpor_a       SA130X; 

* CIV  VALUE LABEL ASSOCIATIONS;
label values  cemmetng_a       SA130X; 
label values   cevotelc_a       SA130X; 
