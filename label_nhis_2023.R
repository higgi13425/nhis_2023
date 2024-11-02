# ----------------------------------------------------
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
# 3. replace numbers - (regex on) - "1\s{3,}" with "1 = "
# 4. repeat for all numbers 0 thorugh 9
# 5. put commas and end of each line (regex on) - replace '"$' with '",' (but only for this section, not all)
# 6. replace ",$^))" with "))" and end of each value label
# 
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
var_label(data$pcnt18uptc) <- "Top-coded count of persons 18 = or older in the household"
var_label(data$pcntlt18tc) <- "Top-coded count of persons under 18 = in the household"

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
var_label(data$paybll12m) <- "Problems paying medical bills, past 12m" 
var_label(data$paynobllnw) <- "Unable to pay medical bills" 
var_label(data$payworry) <- "Get sick or have accident, worry about paying medical bills" 

# CVL  VARIABLE LABELS 
var_label(data$evercovd) <- "Ever had COVID-19" 
var_label(data$longcovd1) <- "Had COVID-19 symptoms for 3 or more months" 
var_label(data$sympnow1) <- "Currently has COVID-19 symptoms" 
var_label(data$lcvdact) <- "COVID-19 impacts activities" 

# DNC  VARIABLE LABELS 
var_label(data$denprev) <- "Time since last dental exam or cleaning" 
var_label(data$dendl12m) <- "Delayed dental care due to cost, past 12 months" 
var_label(data$denng12m) <- "Needed dental care but did not get it due to cost, past 12 months" 

# UTZ  VARIABLE LABELS 
var_label(data$lastdr) <- "Time since last saw doctor" 
var_label(data$wellness) <- "Was last visit a wellness visit" 
var_label(data$wellvis) <- "Time since last wellness visit" 
var_label(data$usualpl) <- "Have a usual place for care" 
var_label(data$usplkind) <- "Type of place for usual care" 
var_label(data$urgnt12mtc) <- "Number of times visited urgent care, past 12 months, top-coded" 
var_label(data$emerg12mtc) <- "Number of times visited hospital emergency room, past 12 months, top-coded" 
var_label(data$hospongt) <- "Hospitalized overnight, past 12 months" 
var_label(data$meddl12m) <- "Delayed medical care due to cost, past 12 months" 
var_label(data$medng12m) <- "Needed medical care but did not get it due to cost, past 12 months" 

# TLH  VARIABLE LABELS 
var_label(data$virapp12m) <- "Virtual medical appointment, past 12m" 

# HIT  VARIABLE LABELS 
var_label(data$accssint) <- "Internet access" 
var_label(data$accsshom) <- "Internet access at home" 
var_label(data$hitlook) <- "Used internet for health information" 
var_label(data$hitcomm) <- "Communicated with doctor's office" 
var_label(data$hittest) <- "Used internet for test results" 

# PMD  VARIABLE LABELS 
var_label(data$rx12m) <- "Took prescription medication, past 12 months" 
var_label(data$rxsk12m) <- "Skipped medication doses to save money, past 12m" 
var_label(data$rxls12m) <- "Took less medication to save money, past 12 months" 
var_label(data$rxdl12m) <- "Delayed filling prescription to save money, past 12 months" 
var_label(data$rxdg12m) <- "Needed prescription medication but did not get it due to cost, past 12 months" 

# PRV  VARIABLE LABELS 
var_label(data$bplast) <- "Last time blood pressure checked" 
var_label(data$chollast) <- "Last time cholesterol checked" 
var_label(data$diblast1) <- "Last time blood sugar test, if never told had diabetes" 
var_label(data$diba1clast) <- "Last time A1C test, if ever told had diabetes" 
var_label(data$diba1cnmt) <- "Number of A1C tests past 12 months - topcoded" 
var_label(data$colorectev) <- "Ever had a colonoscopy or sigmoidoscopy" 
var_label(data$colorectyp) <- "Had a colonoscopy or sigmoidoscopy or both" 
var_label(data$colwhen) <- "Most recent colonoscopy" 
var_label(data$colreason1) <- "Main reason for recent colonoscopy" 
var_label(data$colsigwhen) <- "Most recent colonoscopy or sigmoidoscopy" 
var_label(data$sigwhen) <- "Most recent sigmoidoscopy" 
var_label(data$ctcolev1) <- "Ever had a colonography or virtual colonoscopy" 
var_label(data$ctcolwhen1) <- "Most recent colonography or virtual colonoscopy" 
var_label(data$fithev1) <- "Ever had home blood stool test or FIT" 
var_label(data$fithwhen1) <- "Most recent home blood stool test or FIT" 
var_label(data$cologuard1) <- "Ever had Cologuard" 
var_label(data$fitcolg1) <- "Blood stool or FIT was part of Cologuard test" 
var_label(data$cguardwhe1) <- "Most recent Cologuard" 
var_label(data$colprob1) <- "Doctor recommended testing to look for problems in colon or rectum" 
var_label(data$coltest1) <- "Doctor recommended stool blood test, fecal occult blood, or FIT test" 
var_label(data$coltest2) <- "Doctor recommended Cologuard or FIT DNA test" 
var_label(data$coltest3) <- "Doctor recommended sigmoidoscopy" 
var_label(data$coltest4) <- "Doctor recommended colonoscopy" 
var_label(data$coltest5) <- "Doctor recommended CT colonography or virtual colonoscopy" 
var_label(data$coltest6) <- "Doctor recommended other test for colon cancer" 
var_label(data$psatest) <- "Ever had a PSA test" 
var_label(data$psawhen) <- "Most recent PSA test" 
var_label(data$psareason) <- "Main reason had a PSA test" 
var_label(data$psa5yr1) <- "Number of PSA tests-last 5 years" 
var_label(data$cervicev1) <- "Ever had cervical cancer screening test" 
var_label(data$cervicwhen) <- "Most recent cervical cancer test" 
var_label(data$hystev2) <- "Had hysterectomy" 
var_label(data$mamev) <- "Ever had mammogram" 
var_label(data$mamwhen) <- "Most recent mammogram" 
var_label(data$mamreason) <- "Main reason for mammogram" 
var_label(data$mamnot1) <- "Most important reason for no mammogram" 
var_label(data$mrihad) <- "Ever had breast MRI" 
var_label(data$mriwhen) <- "Most recent breast MRI" 
var_label(data$mrirea) <- "Main reason for breast MRI" 

# GCT  VARIABLE LABELS 
var_label(data$gtposs1) <- "Ever discussed genetic cancer risk test with doctor" 
var_label(data$gtgrisk) <- "Ever had a genetic test" 

# FHC  VARIABLE LABELS 
var_label(data$fhcanev) <- "Family ever had cancer" 
var_label(data$fhbcanev) <- "Family ever had breast cancer" 
var_label(data$fhbcannum) <- "Number of family members with breast cancer" 
var_label(data$fhbcan50) <- "Number of family members with breast cancer before age 50" 
var_label(data$fhovcanev) <- "Family ever had ovarian cancer" 
var_label(data$fhovcannum) <- "Number of family members with ovarian cancer" 
var_label(data$fhcanrisk) <- "Talked to doctor about family risk of cancer" 

# IMS  VARIABLE LABELS 
var_label(data$pregfluyr) <- "Was sample adult pregnant last flu season" 
var_label(data$livebirth) <- "Any live births" 
var_label(data$shtflu12m) <- "Flu vaccine, past 12 months" 
var_label(data$shtflum) <- "Month of last flu vaccine" 
var_label(data$shtfluy) <- "Year of last flu vaccine" 
var_label(data$flupreg) <- "Was flu shot before or during pregnancy" 
var_label(data$flupreg2) <- "Earlier pregnancy and flu vaccine" 
var_label(data$shtcvd191) <- "COVID-19 vaccination" 
var_label(data$shtcvd19nm1) <- "Number of COVID-19 vaccinations" 
var_label(data$cvdvac1m1) <- "Month of most recent COVID-19 vaccination" 
var_label(data$cvdvac1y1) <- "Year of most recent COVID-19 vaccination" 
var_label(data$shottype2) <- "Brand of first COVID-19 shot" 
var_label(data$shtpnuev) <- "Ever had pneumonia shot" 
var_label(data$shtpneunb) <- "Number of pneumonia shots" 
var_label(data$shtshingl1) <- "Ever had a shingles vaccination" 
var_label(data$shingyearp) <- "Year of most recent Shingles vaccine" 
var_label(data$shingwhen1) <- "Was last shingles shot before 2017" 
var_label(data$shingrix3) <- "Ever had Shingrix vaccination" 
var_label(data$shingrixn3) <- "How many Shingrix shots" 
var_label(data$shingrixfs1) <- "First or second Shingrix shot" 
var_label(data$tdappreg) <- "Have a Tdap booster shot" 
var_label(data$shthepb1) <- "Hepatitis B vaccine" 
var_label(data$livehep) <- "Live with someone with hepatitis" 
var_label(data$workhealth) <- "Currently provide medical care to patients" 
var_label(data$wrkhlthfc) <- "Currently volunteer or work in health care" 
var_label(data$travel) <- "Travel to other countries since 1995" 

# PTC  VARIABLE LABELS 
var_label(data$eyeex12m) <- "Had eye exam, past 12 months" 
var_label(data$thera12m) <- "Received physical/speech/rehabilitative/occupational therapy, past 12 months" 
var_label(data$homehc12m) <- "Received care at home, past 12 months" 

# ANX  VARIABLE LABELS 
var_label(data$anxfreq) <- "How often feel worried, nervous, or anxious" 
var_label(data$anxmed) <- "Take medication for worried/nervous/anxious feelings" 
var_label(data$anxlevel) <- "Level of feelings when last felt worried/nervous/anxious" 

# DEP  VARIABLE LABELS 
var_label(data$depfreq) <- "How often depressed" 
var_label(data$depmed) <- "Take medication for depression" 
var_label(data$deplevel) <- "Level of how depressed" 

# MHC  VARIABLE LABELS 
var_label(data$mhrx) <- "Took medication for other emotions/concentration/behavior/mental health, past 12" 
var_label(data$mhthrpy) <- "Received counseling/therapy from mental health professional, past 12 months" 
var_label(data$mhtpynow) <- "Currently receiving counseling/therapy from mental health professional" 
var_label(data$mhthdly) <- "Delayed counseling, therapy due to cost, past 12 months" 
var_label(data$mhthnd) <- "Needed counseling, therapy but did not get it due to cost, past 12 months" 

# MHA  VARIABLE LABELS 
var_label(data$phq41) <- "How often little interest in things, past 2 weeks" 
var_label(data$phq42) <- "How often feeling down, past 2 weeks" 
var_label(data$phq2screen) <- "PHQ-2 screener result" 
var_label(data$phq43) <- "How often felt nervous/anxious/on edge, past 2 weeks" 
var_label(data$phq44) <- "How often can't stop/control worrying, past 2 weeks" 
var_label(data$gad2screen) <- "GAD-2 screener result" 

# EDS  VARIABLE LABELS 
var_label(data$discrim1) <- "Treated with less courtesy or respect" 
var_label(data$discrim2) <- "Receive poor service at restaurant or store" 
var_label(data$discrim3) <- "Treated as not smart" 
var_label(data$discrim4) <- "People act afraid of you" 
var_label(data$discrim5) <- "You are threatened or harassed" 

# HVS  VARIABLE LABELS 
var_label(data$vigil1) <- "Prepare for possible insults before leaving home" 
var_label(data$vigil2) <- "Careful about your appearance in order to get good service or avoid harassment" 
var_label(data$vigil3) <- "Watch what you say and how you say it" 
var_label(data$vigil4) <- "Avoid certain situations and places" 

# PAI  VARIABLE LABELS 
var_label(data$paifrq3m) <- "How often had pain, past 3 months" 
var_label(data$paiamnt) <- "How much pain last time" 
var_label(data$paiwklm3m) <- "How often pain limits life or work" 
var_label(data$paiaffm3m) <- "How often pain impacts family" 
var_label(data$paiback3m) <- "Back pain" 
var_label(data$paiulmb3m) <- "Pain in hands" 
var_label(data$paillmb3m) <- "Pain in hips" 
var_label(data$paihdfc3m) <- "Migraine" 
var_label(data$paiapg3m) <- "Abdominal pain" 
var_label(data$paitooth3m) <- "Toothache or jaw pain" 

# REP  VARIABLE LABELS 
var_label(data$repstrain) <- "Repetitive strain injuries in the past 3 months" 
var_label(data$replimit) <- "Limited by repetitive strain injury in the past 3 months" 
var_label(data$repsawdoc) <- "Saw a doctor about repetitive strain injury" 
var_label(data$repwrkdytc) <- "Number of days missed due to repetitive strain injury" 
var_label(data$repfutwrk) <- "Expected workdays missed due to repetitive strain injury" 
var_label(data$repstopchg) <- "Stopped working or changed job due to repetitive strain injury" 
var_label(data$repreduce) <- "Reduced work or changed tasks due to repetitive strain injury" 
var_label(data$repwrkcaus) <- "Did repetitive strain injury occur while working" 

# INJ  VARIABLE LABELS 
var_label(data$anyinjury) <- "Any injury in the past 3 months (not including repetitive strain injuries)" 
var_label(data$injlimit) <- "Limited by injury in the past 3 months" 
var_label(data$numinjtc) <- "Number of injuries in the past 3 months" 
var_label(data$injhome) <- "Did injury occur at home" 
var_label(data$injwork) <- "Did injury occur at work" 
var_label(data$injsports) <- "Did injury occur while playing sports or exercising" 
var_label(data$injfall) <- "Did injury occur because of a fall" 
var_label(data$injfallhom) <- "Did fall occur at home" 
var_label(data$injfallwrk) <- "Did fall occur at work" 
var_label(data$injmotor) <- "Injury caused by a motor vehicle crash or collision" 
var_label(data$injmvtype1) <- "Motor vehicle accident - driver" 
var_label(data$injmvtype2) <- "Motor vehicle accident - passenger" 
var_label(data$injmvtype3) <- "Motor vehicle accident - bicyclist" 
var_label(data$injmvtype4) <- "Motor vehicle accident - pedestrian" 
var_label(data$injmvtype5) <- "Motor vehicle accident - something else" 
var_label(data$injchores) <- "Did injury occur while doing household activities" 
var_label(data$injsawdoc) <- "Saw a doctor about injury" 
var_label(data$injer) <- "Visited ER for injury" 
var_label(data$injhosp) <- "Hospitalized for injury" 
var_label(data$injbones) <- "Did injury cause broken bones" 
var_label(data$injstitch) <- "Did injury require stitches or staples" 
var_label(data$injwrkdytc) <- "Number of workdays missed due to injury in the past 3 months" 
var_label(data$injfutwrk) <- "Expected workdays missed due to injury" 
var_label(data$injstopchg) <- "Stopped working or changed jobs due to injury" 
var_label(data$injreduce) <- "Reduced work or changed tasks due to injury" 

# TBI  VARIABLE LABELS 
var_label(data$tbilcdcmg) <- "Lost consciousness, dazed or confused, or had gap in memory, past 12 months" 
var_label(data$tbihlsbmc) <- "Headache, sensitivities, balance problems or mood change, past 12 months" 
var_label(data$tbisport) <- "Blow or jolt to head while playing sports or rec activity, past 12 months" 
var_label(data$tbileague) <- "Blow or jolt to head while playing organized sports, past 12 months" 
var_label(data$tbieval) <- "Evaluated for concussion, past 12 months" 

# ART  VARIABLE LABELS 
var_label(data$jntsymp) <- "Arthritis symptoms, past 30 days" 
var_label(data$jntpn) <- "Arthritis pain, past 30 days" 
var_label(data$arthlmt) <- "Arthritis activity limitations" 
var_label(data$arthwrk) <- "Arthritis work limitations" 
var_label(data$arthph) <- "Physical activity to help with arthritis" 

# CIG  VARIABLE LABELS 
var_label(data$smkev) <- "Ever smoked 100 cigarettes" 
var_label(data$smknow) <- "Now smoke cigarettes" 
var_label(data$smkcigst) <- "Cigarette smoking status" 
var_label(data$cignow) <- "Number of cigarettes a day" 
var_label(data$smk30d) <- "Number of days smoked past month" 
var_label(data$cig30d) <- "Number of cigarettes on days smoked past month" 
var_label(data$mentholc) <- "Smoke menthol or non-menthol cigarettes" 
var_label(data$ecigev) <- "Ever used electronic cigarettes" 
var_label(data$ecignow) <- "Now use electronic cigarettes" 
var_label(data$smkecigst) <- "Electronic cigarette use status" 

# OTB  VARIABLE LABELS 
var_label(data$cigarev) <- "Ever smoked a cigar" 
var_label(data$cigarcur) <- "Now smoke cigars" 
var_label(data$cigar30d) <- "How many days smoked a cigar, past 30 days" 
var_label(data$pipeev) <- "Ever smoked a pipe filled with tobacco" 
var_label(data$pipecur) <- "Now smoked pipe filled with tobacco" 
var_label(data$smokelsev) <- "Ever used smokeless tobacco" 
var_label(data$smokelscur) <- "Now use smokeless tobacco" 

# SVI  VARIABLE LABELS 
var_label(data$avisexam) <- "Last time had eye exam" 
var_label(data$avisreh) <- "Use vision rehabilitation services" 
var_label(data$avisdev) <- "Use vision assistive devices" 
var_label(data$avissadv) <- "Health professional recommend services" 
var_label(data$vimread) <- "Need eyeglasses or contacts to read up close" 
var_label(data$vimdrive) <- "Need eyeglasses or contacts to see in distance" 

# SHE  VARIABLE LABELS 
var_label(data$ahearst1) <- "Hearing ability" 
var_label(data$hrwhisp) <- "Hear whispers" 
var_label(data$earinfect) <- "Ear infection past 12 months" 
var_label(data$earinfect3) <- "3or more ear infections" 
var_label(data$cbalhdinj) <- "Lifetime significant head injury" 
var_label(data$cbalhdno) <- "Number of lifetime head injuries" 
var_label(data$hrtest) <- "Ever had hearing test" 
var_label(data$hrtestlast) <- "How long since hearing test" 
var_label(data$hraidaqr) <- "Hearing aid fit or purchased" 
var_label(data$baldizz) <- "Balance or dizziness problem past 12 months" 
var_label(data$baldprob) <- "How big of a balance or dizziness problem" 
var_label(data$baldhp) <- "Health provider for balance or dizziness problem" 
var_label(data$bfall12m) <- "Fallen past 12 months" 
var_label(data$bfalltimes) <- "Number of falls past 12 months" 
var_label(data$hrtinnitus) <- "Tinnitus past 12 months" 
var_label(data$hrtinlng) <- "Tinnitus how long" 
var_label(data$hrtinprob) <- "Tinnitus how big a problem" 
var_label(data$hrtinmedsp) <- "Medical specialist for tinnitus" 
var_label(data$hrloudjob) <- "Ever exposed to loud noise at job" 
var_label(data$hrloudjbyr) <- "Years exposed to loud sounds at job" 
var_label(data$hrloudjb12m) <- "Exposed to loud sounds past 12 months" 
var_label(data$hrjobprot) <- "Job exposure hearing protection" 
var_label(data$hrfireev) <- "Ever used a firearm" 
var_label(data$hrfiretotr) <- "Firearm total rounds" 
var_label(data$hrfire12m) <- "Firearm rounds past 12 months" 
var_label(data$hrfireprot) <- "Firearm hearing protection" 
var_label(data$hrvloud12m) <- "Exposed to very loud sounds past 12 months" 
var_label(data$hrvldprot) <- "Very loud sounds hearing protection" 

# SWE  VARIABLE LABELS 
var_label(data$hrjbexp12m) <- "Job exposure to chemicals, past 12 months" 
var_label(data$hrjbexp4hr) <- "Job exposure to chemicals 4 or more hours" 
var_label(data$hrjbexptb) <- "Job exposure to tobacco smoke 4 or more hours, past 12 months" 

# ORN  VARIABLE LABELS 
var_label(data$orient) <- "Sexual orientation" 

# MAR  VARIABLE LABELS 
var_label(data$marital) <- "Sample adult's current marital status" 
var_label(data$spousliv) <- "Sample adult's spouse lives here" 
var_label(data$spousep) <- "Sample adult's spouse does not reside here due to legal separation" 
var_label(data$evrmarried) <- "Sample adult has ever been married" 
var_label(data$marstat) <- "Current marital status of sample adult" 
var_label(data$legmstat) <- "Legal marital status of sample adult" 
var_label(data$spousesex) <- "Sex of sample adult's spouse" 
var_label(data$saspprace) <- "Race of sample adult and spouse or partner are the same" 
var_label(data$saspphisp) <- "Hispanic ethnicity of sample adult and spouse or partner are the same" 
var_label(data$spousagetc) <- "Age of sample adult's spouse, top-coded" 
var_label(data$spouseducp) <- "Education level of sample adult's spouse" 
var_label(data$spouswrk) <- "Working status of sample adult's spouse" 
var_label(data$spouswkft) <- "Sample adult's spouse is working full-time" 
var_label(data$prtnrsex) <- "Sex of the sample adult's partner" 
var_label(data$prtnragetc) <- "Age of sample adult's partner, top-coded" 
var_label(data$prtnreducp) <- "Education level of sample adult's partner" 
var_label(data$prtnrwrk) <- "Working status of sample adult's partner" 
var_label(data$prtnrwkft) <- "Sample adult's partner is working full-time" 
var_label(data$saparentsc) <- "Sample adult relationship to sample child" 
var_label(data$parstat) <- "Parental Status of sample adult" 

# VET  VARIABLE LABELS 
var_label(data$afvet) <- "Ever serve active duty military" 
var_label(data$afvettrn) <- "Reserves or National Guard" 
var_label(data$combat) <- "Ever served abroad during armed conflict" 
var_label(data$vadisb) <- "Have VA disability rating" 
var_label(data$vahosp) <- "Receive care at VA facility" 
var_label(data$vacareev) <- "Ever use VA health care" 

# NAT  VARIABLE LABELS 
var_label(data$natusborn) <- "Born in U.S. or U.S. territory" 
var_label(data$yrsinus) <- "Years that sample adult has been in the United States" 
var_label(data$citznstp) <- "Citizenship status" 

# LNG  VARIABLE LABELS 
var_label(data$langhm) <- "Other language spoken at home" 
var_label(data$langspecr) <- "Language at home, public use" 
var_label(data$langmed) <- "Language for TV, news, radio" 
var_label(data$langdoc) <- "Language at doctor" 
var_label(data$langsoc) <- "Language socially" 

# SCH  VARIABLE LABELS 
var_label(data$schcurenr) <- "Currently in school" 
var_label(data$schdymsstc) <- "Days of school missed due to illness or injury past 12 months, top-coded" 

# EMP  VARIABLE LABELS 
var_label(data$emplastwk) <- "Worked for pay last week" 
var_label(data$empnowrk) <- "Temporarily absent from work last week" 
var_label(data$empwhynot) <- "Main reason not working" 
var_label(data$empwhenwrk) <- "Last time worked" 
var_label(data$empwrklsw1) <- "Worked last week" 
var_label(data$emplstwor1) <- "Last time worked for pay" 
var_label(data$empwkhrs3) <- "Hours worked per week (topcoded for Public Use)" 
var_label(data$empwrkft1) <- "Usually work 35+ hours per week" 
var_label(data$empsicklv) <- "Paid sick leave" 
var_label(data$emphealins) <- "Health insurance offered" 
var_label(data$empdysmss3) <- "Days missed work, past 12 months (top-coded)" 

# EMD  VARIABLE LABELS 
var_label(data$emdindstn1) <- "Detailed 2-digit recode for sample adult's industry" 
var_label(data$emdindstn2) <- "Simple 2-digit recode for sample adult's industry" 
var_label(data$emdoccupn1) <- "Detailed 2-digit recode for sample adult's occupation" 
var_label(data$emdoccupn2) <- "Simple 2-digit recode for sample adult's occupation" 
var_label(data$emdsuper) <- "Supervise other employees" 
var_label(data$emdwrkcat1) <- "Type of main job" 

# VOL  VARIABLE LABELS 
var_label(data$cevolun1) <- "Volunteer for organization or association" 
var_label(data$cevolun2) <- "Other volunteer activities" 

# FEM  VARIABLE LABELS 
var_label(data$pcntadwkp1) <- "Number of adults in sample adult's family who are working (top-coded)" 
var_label(data$pcntadwfp1) <- "Number of adults in sample adult's family who are working full-time (top-coded)" 

# INC  VARIABLE LABELS 
var_label(data$incwrko) <- "Income from wages" 
var_label(data$incinter) <- "Income from accounts" 
var_label(data$incssrr) <- "Income from SS/Railroad Retirement" 
var_label(data$incssissdi) <- "Family income from SSDI" 
var_label(data$ssissdibth) <- "Which family income SSI/SSDI" 
var_label(data$ssissdidsb) <- "SSI/SSDI due to disability" 
var_label(data$incwelf) <- "Income from public assistance" 
var_label(data$incretire) <- "Income from retirement" 
var_label(data$incothr) <- "Income from other sources" 
var_label(data$impnum) <- "Imputation num" 
var_label(data$povrattc) <- "SA family poverty ratio (top-coded)" 
var_label(data$ratcat) <- "Ratio of family income to poverty threshold for SA's family" 
var_label(data$impincflg) <- "Imputed SA family income imputation flag" 
var_label(data$inctcflg) <- "Sample adult family income top-code flag" 

# FOO  VARIABLE LABELS 
var_label(data$fsnap12m) <- "Receive food stamps, past 12m" 
var_label(data$fsnap30d) <- "Receive food stamps, past 30d" 
var_label(data$fwic12m) <- "Receive WIC benefits, past 12m" 
var_label(data$flunch12m1) <- "Receive free/reduced meals at school" 

# FDS  VARIABLE LABELS 
var_label(data$fdsrunout) <- "Worry food would run out" 
var_label(data$fdslast) <- "Food didn't last" 
var_label(data$fdsbalance) <- "Couldn't afford to eat balanced meals" 
var_label(data$fdsskip) <- "Cut the size of meals or skip meals" 
var_label(data$fdsskipdys) <- "How many days did you/adults in the family cut the size of meals or skip meals" 
var_label(data$fdsless) <- "Eat less than should" 
var_label(data$fdshungry) <- "Ever hungry because not enough money for food" 
var_label(data$fdsweight) <- "Lose weight because not enough money for food" 
var_label(data$fdsnoteat) <- "Not eat for a whole day" 
var_label(data$fdsnedays) <- "How many days not eat" 
var_label(data$fdscat3) <- "Adult 3 category food security recode" 
var_label(data$fdscat4) <- "Adult  4 category food security recode" 

# HOU  VARIABLE LABELS 
var_label(data$houyrsliv) <- "Length of time in house or apartment" 
var_label(data$houtenure) <- "Residence owned or rented" 
var_label(data$hougvasst) <- "Paying lower rent" 

# SDH  VARIABLE LABELS 
var_label(data$housecost) <- "Had trouble paying for housing" 

# TBH  VARIABLE LABELS 
var_label(data$transpor) <- "Delay care in the past 12 months because no reliable transportation" 

# CIV  VARIABLE LABELS 
var_label(data$cemmetng) <- "Attend public meeting" 
var_label(data$cevotelc) <- "Vote in last local elections"


#---------------------------------#
# val_labels(SECTION
# 
# 1. replace "label define " with "val_labels("
# 2. replace "X$" (select regex box in find bar) with "X <- c("
# 3. replace numbers - (regex on) - "1\s{3,}" with "1 = "
# 4. repeat for all numbers 0 thorugh 9
# 5. put commas and end of each line (regex on) - replace '"$' with '",' (but only for this section, not all)
# 6. replace "))" with "))" and end of each value label
# 7. Replace '"Don't Know",' with '"Don't Know"' to remove final comma
# 8. any trailing commas on the last value label by hand
# 
# DEFINE VALUE LABELS FOR REPORTS))

val_labels(SA001X <- c(
1 = "Underweight",
2 = "Healthy weight",
3 = "Overweight",
4 = "Obese",
9 = "Unknown"
))

val_labels(SA002X <- c(
1 = "Reassigned to CHIP from private"
))

val_labels(SA003X <- c(
1 = "Yes, a citizen of the United States",
2 = "No, not a citizen of the United States",
7 = "Refused",
8 = "Not Ascertained",
9 = "Don't Know"
))

val_labels(SA004X <- c(
1 = "Private",
2 = "Medicaid and other public",
3 = "Other coverage",
4 = "Uninsured",
5 = "Don't Know"
))

val_labels(SA005X <- c(
1 = "Private",
2 = "Dual eligible",
3 = "Medicare Advantage",
4 = "Medicare only excluding Medicare Advantage",
5 = "Other coverage",
6 = "Uninsured",
7 = "Don't Know"
))

val_labels(SA006X <- c(
1 = "1",
2 = "2",
3 = "3",
4 = "4",
5 = "5 or more",
7 = "Refused",
8 = "Not Ascertained",
9 = "Don't Know"
))

val_labels(SA007X <- c(
1 = "Current e-cigarette user",
2 = "Used e-cigarette, not current user",
3 = "Never e-cigarette user",
4 = "E-cigarette user, current status unknown",
9 = "Unknown if ever used e-cigarette"
))

val_labels(SA008X <- c(
01 = "Grade 0-11",
02 = "12th grade, no diploma",
03 = "GED or equivalent",
04 = "High School Graduate",
05 = "Some college, no degree",
06 = "Associate degree: occupational, technical, or vocational program",
07 = "Associate degree: academic program",
08 = "Bachelor's degree (Example: BA, AB, BS, BBA)",
09 = "Master's degree (Example: MA, MS, MEng, MEd, MBA)",
10 = "Professional School or Doctoral degree (Example: MD, DDS, DVM, JD, PhD, EdD)",
97 = "Refused",
98 = "Not Ascertained",
99 = "Don't Know"
))

val_labels(SA009X <- c(
0 = "0 times",
1 = "1 time",
2 = "2 times",
3 = "3 times",
4 = "4+ times",
7 = "Refused",
8 = "Not Ascertained",
9 = "Don't Know"
))

val_labels(SA010X <- c(
1 = "Exchange plan",
2 = "Not exchange plan",
8 = "Not Ascertained"
))

val_labels(SA011X <- c(
1 = "Company provides exchange plans",
2 = "Not an exchange company",
3 = "Exchange Portal or exact exchange plan name",
8 = "Not Ascertained"
))

val_labels(SA012X <- c(
1 = "Food secure",
2 = "Low food security",
3 = "Very low food security",
8 = "Not Ascertained"
))

val_labels(SA013X <- c(
1 = "High food security",
2 = "Marginal food security",
3 = "Low food security",
4 = "Very low food security",
8 = "Not Ascertained"
))

val_labels(SA014X <- c(
1 = "Positive",
2 = "Negative",
8 = "Not Ascertained"
))

val_labels(SA015X <- c(
1 = "Person is sample adult"
))

val_labels(SA016X <- c(
1 = "Hispanic (Mexican/Mexican American)",
2 = "Hispanic (all other groups)",
3 = "Not Hispanic",
7 = "Refused",
8 = "Not Ascertained",
9 = "Don't Know"
))

val_labels(SA017X <- c(
01 = "Hispanic",
02 = "Non-Hispanic White only",
03 = "Non-Hispanic Black/African American only",
04 = "Non-Hispanic Asian only",
05 = "Non-Hispanic AIAN only",
06 = "Non-Hispanic AIAN and any other group",
07 = "Other single and multiple races",
97 = "Refused",
98 = "Not Ascertained",
99 = "Don't Know"
))

val_labels(SA018X <- c(
01 = "Crop production",
02 = "Animal production and aquaculture",
03 = "Forestry and logging",
04 = "Fishing, hunting, and trapping",
05 = "Support activities for agriculture and forestry",
06 = "Oil and gas extraction",
07 = "Mining (except oil and gas)",
08 = "Support activities for mining",
09 = "Utilities industries",
10 = "Construction industries",
11 = "Food manufacturing",
12 = "Beverage and tobacco product manufacturing",
13 = "Textile mills",
14 = "Textile product mills",
15 = "Apparel manufacturing",
16 = "Leather and allied product manufacturing",
17 = "Wood product manufacturing",
18 = "Paper manufacturing",
19 = "Printing and related support activities",
20 = "Petroleum and coal products manufacturing",
21 = "Chemical manufacturing",
22 = "Plastics and rubber products manufacturing",
23 = "Nonmetallic mineral product manufacturing",
24 = "Primary metal manufacturing",
25 = "Fabricated metal product manufacturing",
26 = "Machinery manufacturing",
27 = "Computer and electronic product manufacturing",
28 = "Electrical equipment, appliance, and component manufacturing",
29 = "Transportation equipment manufacturing",
30 = "Furniture and related product manufacturing",
31 = "Miscellaneous manufacturing",
32 = "Merchant wholesalers, durable goods",
33 = "Merchant wholesalers, nondurable goods",
34 = "Non-specified wholesale trade",
35 = "Motor vehicle and parts dealers",
36 = "Furniture and home furnishings stores",
37 = "Electronics and appliance stores",
38 = "Building material and garden equipment and supplies dealers",
39 = "Food and beverage stores",
40 = "Health and personal care stores",
41 = "Gasoline stations",
42 = "Clothing, shoe, jewelry, luggage, and leather goods stores",
43 = "Sporting goods, camera, hobby, book and music stores",
44 = "General merchandise stores",
45 = "Miscellaneous store retailers",
46 = "Nonstore retailers and non-specified retail trade",
47 = "Transportation (including support activities for transportation)",
48 = "Postal service, couriers, and messengers",
49 = "Warehousing and storage",
50 = "Newspaper, periodical, book, and software publishing industries",
51 = "Motion picture, video, and sound recording industries",
52 = "Broadcasting and telecommunications",
53 = "Libraries and archives, internet publishing, web search portals, data processing and hosting services, and other information services",
54 = "Monetary authorities -- central bank",
55 = "Credit intermediation and related activities",
56 = "Securities, commodity contracts, and other financial investments and related activities",
57 = "Insurance carriers and related activities",
58 = "Real estate",
59 = "Automotive and other consumer goods rental and leasing services",
60 = "Commercial, industrial, and other intangible assets (except copyrighted works)",
61 = "Professional, scientific, and technical services industries",
62 = "Management of companies and enterprises industries",
63 = "Administrative and support and waste management and remediation services industries",
64 = "Education services industries",
65 = "Ambulatory health care services",
66 = "Hospitals",
67 = "Nursing and residential care facilities",
68 = "Social assistance",
69 = "Performing arts, spectator sports, promoters, agents, artists, writers and related industries",
70 = "Museums, historical sites, and similar institutions",
71 = "Amusement, gambling, and recreation industries",
72 = "Accommodation",
73 = "Food services and drinking places",
74 = "Repair and maintenance",
75 = "Personal services (barber shops, beauty salons, nail salons, laundry, funeral homes and cemetaries)",
76 = "Religious, grantmaking, civic, labor, professional, and similar organizations",
77 = "Private households",
78 = "Public administration industries",
79 = "Armed forces",
97 = "Refused, classified",
98 = "Not ascertained",
99 = "Don't know"
))

val_labels(SA019X <- c(
01 = "Agriculture, Forestry, Fishing, and Hunting Industries",
02 = "Mining Industries",
03 = "Utilities Industries",
04 = "Construction Industries",
05 = "Manufacturing Industries",
06 = "Wholesale Trade Industries",
07 = "Retail Trade Industries",
08 = "Transportation and Warehousing Industries",
09 = "Information Industries",
10 = "Finance and Insurance Industries",
11 = "Real Estate and Rental and Leasing Industries",
12 = "Professional, Scientific, and Technical Services Industries",
13 = "Management of Companies and Enterprises Industries",
14 = "Administrative and Support and Waste Management and Remediation Services Industries",
15 = "Education Services Industries",
16 = "Health Care and Social Assistance Industries",
17 = "Arts, Entertainment, and Recreation Industries",
18 = "Accommodation and Food Services Industries",
19 = "Other Services (except Public Administration) Industries",
20 = "Public Administration Industries",
21 = "Armed Forces",
97 = "Refused, classified",
98 = "Not ascertained",
99 = "Don't know"
))

val_labels(SA020X <- c(
1 = "Yes, information",
2 = "Yes, but no information",
3 = "No",
7 = "Refused",
8 = "Not Ascertained",
9 = "Don't Know"
))

val_labels(SA021X <- c(
01 = "January",
02 = "February",
03 = "March",
04 = "April",
05 = "May",
06 = "June",
07 = "July",
08 = "August",
09 = "September",
10 = "October",
11 = "November",
12 = "December"
))

val_labels(SA022X <- c(
1 = "Quarter 1",
2 = "Quarter 2",
3 = "Quarter 3",
4 = "Quarter 4"
))

val_labels(SA023X <- c(
1 = "Spanish",
2 = "Other Language",
7 = "Refused",
8 = "Not Ascertained",
9 = "Don't Know"
))

val_labels(SA024X <- c(
1 = "Separated",
2 = "Divorced",
3 = "Married",
4 = "Single/never married",
5 = "Widowed",
9 = "Unknown legal marital status"
))

val_labels(SA025X <- c(
1 = "Reassigned to Medicaid from private"
))

val_labels(SA026X <- c(
1 = "Married, spouse is present",
2 = "Married, spouse is not present",
3 = "Married, spouse presence unknown",
4 = "Widowed",
5 = "Divorced",
6 = "Separated",
7 = "Never married",
8 = "Living with a partner",
9 = "Unknown marital status"
))

val_labels(SA027X <- c(
1 = "Medicare Advantage",
2 = "Private plan not Medicare Advantage",
7 = "Refused",
8 = "Not Ascertained",
9 = "Don't Know"
))

val_labels(SA028X <- c(
1 = "Mentioned",
2 = "Not mentioned",
7 = "Refused",
8 = "Not Ascertained",
9 = "Don't Know"
))

val_labels(SA029X <- c(
1 = "Multiple families in household",
2 = "Only one family in household",
9 = "Unknown"
))

val_labels(SA030X <- c(
0 = "None",
1 = "1 or more",
))

val_labels(SA031X <- c(
1 = "Not covered",
2 = "Covered",
7 = "Refused",
8 = "Not Ascertained",
9 = "Don't Know"
))

val_labels(SA032X <- c(
01 = "Chief executives)) general and operations managers)) legislators",
02 = "Advertising, marketing, promotions, public relations, and sales managers",
03 = "Administrative services, compensation/benefits, human resources, training, production, purchasing, and transportion/distribution, and other operations managers",
04 = "All other management occupations",
05 = "Business operations specialists",
06 = "Financial specialists",
07 = "Computer specialists",
08 = "Mathematical science occupations",
09 = "Architects, surveyors, and cartographers",
10 = "Engineers",
11 = "Drafters, engineering, and mapping technicians",
12 = "Life scientists",
13 = "Physical scientists",
14 = "Social scientists and related workers",
15 = "Life, physical, and social science technicians",
16 = "Counselors, social workers, and other community and social service specialists",
17 = "Religious workers",
18 = "Lawyers, judges, and related workers",
19 = "Legal support workers",
20 = "Postsecondary teachers",
21 = "Primary, secondary, and special education school teachers",
22 = "Other teachers and instructors",
23 = "Librarians, curators, and archivists",
24 = "Other educational instruction and library occupations",
25 = "Art and design workers",
26 = "Entertainers and performers, sports and related workers",
27 = "Media and communication workers",
28 = "Media and communication equipment workers",
29 = "Health diagnosing and treating practitioners",
30 = "Health technologists and technicians",
31 = "Other healthcare practitioners and technical occupations",
32 = "Nursing, psychiatric, and home health aides",
33 = "Occupational and physical therapist assistants and aides",
34 = "Other healthcare support occupations",
35 = "First-line supervisors/managers, protective service workers",
36 = "Fire fighting and prevention workers",
37 = "Law enforcement workers",
38 = "Other protective service workers",
39 = "Supervisors, food preparation and serving workers",
40 = "Cooks and food preparation workers",
41 = "Food and beverage serving workers",
42 = "Other food preparation and serving related workers",
43 = "Supervisors, building and grounds cleaning and maintenance workers",
44 = "Building cleaning and pest control workers",
45 = "Grounds maintenance workers",
46 = "Supervisors, personal care and service workers",
47 = "Animal care and service workers",
48 = "Entertainment attendants and related workers",
49 = "Funeral service workers",
50 = "Personal appearance workers",
51 = "Transportation, tourism, and lodging attendants",
52 = "Other personal care and service workers",
53 = "Supervisors, sales workers",
54 = "Retail sales workers",
55 = "Sales representatives, services",
56 = "Sales representatives, wholesale and manufacturing",
57 = "Other sales and related workers",
58 = "Supervisors, office and administrative support workers",
59 = "Communications equipment operators",
60 = "Financial clerks",
61 = "Information and record clerks",
62 = "Material recording, scheduling, dispatching, and distributing workers",
63 = "Secretaries and administrative assistants",
64 = "Other office and administrative support workers",
65 = "Supervisors, farming, fishing, and forestry workers",
66 = "Agricultural workers",
67 = "Fishing and hunting workers",
68 = "Forest, conservation, and logging workers",
69 = "Supervisors, construction and extraction workers",
70 = "Construction trades workers",
71 = "Helpers, construction trades",
72 = "Other construction and related workers",
73 = "Extraction workers",
74 = "Supervisors of installation, maintenance, and repair workers",
75 = "Electrical and electronic equipment mechanics, installers, and repairers",
76 = "Vehicle and mobile equipment mechanics, installers, and repairers",
77 = "Other installation, maintenance, and repair occupations",
78 = "Supervisors, production workers",
79 = "Assemblers and fabricators",
80 = "Food processing workers",
81 = "Metal workers and plastic workers",
82 = "Printing workers",
83 = "Textile, apparel, and furnishings workers",
84 = "Woodworkers",
85 = "Plant and system operators",
86 = "Other production occupations",
87 = "Supervisors, transportation and material moving workers",
88 = "Air transportation workers",
89 = "Motor vehicle operators",
90 = "Rail transportation workers",
91 = "Water transportation workers",
92 = "Other transportation workers",
93 = "Material moving workers",
94 = "Military specific occupations",
97 = "Refused, classified",
98 = "Not ascertained",
99 = "Don't know"
))

val_labels(SA033X <- c(
01 = "Management Occupations",
02 = "Business and Financial Operations Occupations",
03 = "Computer and Mathematical Occupations",
04 = "Architecture and Engineering Occupations",
05 = "Life, Physical, and Social Science Occupations",
06 = "Community and Social Services Occupations",
07 = "Legal Occupations",
08 = "Educational Instruction and Library Occupations",
09 = "Arts, Design, Entertainment, Sports and Media Occupations",
10 = "Healthcare Practitioners and Technical Occupations",
11 = "Healthcare Support Occupations",
12 = "Protective Service Occupations",
13 = "Food Preparation and Serving Related Occupations",
14 = "Building and Grounds Cleaning and Maintenance Occupations",
15 = "Personal Care and Service Occupations",
16 = "Sales and Related Occupations",
17 = "Office and Administrative Support Occupations",
18 = "Farming, Fishing, and Forestry Occupations",
19 = "Construction and Extraction Occupations",
20 = "Installation, Maintenance, and Repair Occupations",
21 = "Production Occupations",
22 = "Transportation and Material Moving Occupations",
23 = "Military Specific Occupations",
97 = "Refused, classified",
98 = "Not ascertained",
99 = "Don't know"
))

val_labels(SA034X <- c(
1 = "Reassigned to other government from private"
))

val_labels(SA035X <- c(
1 = "^GayLesbian",
2 = "Straight, that is, not ^gaylesbian",
3 = "Bisexual",
4 = "Something else",
5 = "I don't know the answer",
7 = "Refused",
8 = "Not Ascertained"
))

val_labels(SA036X <- c(
1 = "Reassigned to other public from private"
))

val_labels(SA037X <- c(
1 = "Yes, the sample adult is a parent of a child residing in the family",
2 = "There are minor children residing in family but sample adult is not their parent",
3 = "There are no minor children residing in the family",
9 = "Unknown"
))

val_labels(SA038X <- c(
0 = "0 adults",
1 = "1 adult",
2 = "2 adults",
3 = "3+ adults",
8 = "Not Ascertained"
))

val_labels(SA039X <- c(
1 = "1 adult",
2 = "2 adults",
3 = "3+ adults",
8 = "Not Ascertained"
))

val_labels(SA040X <- c(
0 = "0 adults",
1 = "1 adult",
2 = "2 adults",
3 = "3+ adults (at least 3, may have unknown)",
5 = "at least 2 adults, has unknown",
6 = "at least 1 adult, has unknown",
7 = "0 adults, has unknown",
8 = "not ascertained (all unknown)"
))

val_labels(SA041X <- c(
0 = "0 children",
1 = "1 child",
2 = "2 children",
3 = "3+ children",
8 = "Not Ascertained"
))

val_labels(SA042X <- c(
01 = "Through an employer, union, or professional organization",
02 = "Purchased directly",
03 = "Through Healthcare.gov or the Affordable Care Act, also known as Obamacare",
04 = "Through a state or local government or community program",
05 = "Other",
06 = "Through school",
07 = "Through parents",
08 = "Through other relative",
97 = "Refused",
98 = "Not Ascertained",
99 = "Don't Know"
))

val_labels(SA043X <- c(
1 = "Reassigned to private from public"
))

val_labels(SA044X <- c(
1 = "Deductible is less than $1,500",
2 = "Deductible is $1,500 or more",
7 = "Refused",
8 = "Not Ascertained",
9 = "Don't Know"
))

val_labels(SA045X <- c(
1 = "White only",
2 = "Black/African American only",
3 = "Asian only",
4 = "AIAN only",
5 = "AIAN and any other group",
6 = "Other single and multiple races",
7 = "Refused",
8 = "Not Ascertained",
9 = "Don't know"
))

val_labels(SA046X <- c(
01 = "0.00 - 0.49",
02 = "0.50 - 0.74",
03 = "0.75 - 0.99",
04 = "1.00 - 1.24",
05 = "1.25 - 1.49",
06 = "1.50 - 1.74",
07 = "1.75 - 1.99",
08 = "2.00 - 2.49",
09 = "2.50 - 2.99",
10 = "3.00 - 3.49",
11 = "3.50 - 3.99",
12 = "4.00 - 4.49",
13 = "4.50 - 4.99",
14 = "5.00 or greater",
98 = "Not Ascertained"
))

val_labels(SA047X <- c(
10 = "Sample Adult",
20 = "Sample Child",
30 = "Sample Adult Income",
40 = "Sample Child Income",
50 = "Paradata"
))

val_labels(SA048X <- c(
1 = "Northeast",
2 = "Midwest",
3 = "South",
4 = "West"
))

val_labels(SA049X <- c(
1 = "Sample adult is parent of sample child",
2 = "Sample adult is not parent of  sample child",
3 = "No sample child in sample adult's family",
9 = "Unknown"
))

val_labels(SA050X <- c(
1 = "Current every day smoker",
2 = "Current some day smoker",
3 = "Former smoker",
4 = "Never smoker",
5 = "Smoker, current status unknown",
9 = "Unknown if ever smoked"
))

val_labels(SA051X <- c(
0 = "0 times",
1 = "1 time",
2 = "2 times",
3 = "3 times",
4 = "4 times",
5 = "5+ times",
7 = "Refused",
8 = "Not Ascertained",
9 = "Don't Know"
))

val_labels(SA052X <- c(
1 = "Large central metro",
2 = "Large fringe metro",
3 = "Medium and small metro",
4 = "Nonmetropolitan"
))

val_labels(SA053X <- c(
1 = "Yes",
2 = "No",
9 = "Don't Know"
))

val_labels(SA054X <- c(
0 = "No",
1 = "Yes"
))

val_labels(SA055X <- c(
1 = "Yes",
2 = "No",
3 = "Unknown"
))

val_labels(SA056X <- c(
1 = "Yes"
))

val_labels(SA057X <- c(
1 = "Less than 1 year",
2 = "1 to less than 5 years",
3 = "5 to less than 10 years",
4 = "10 to less than 15 years",
5 = "15 years or more",
9 = "Unknown"
))

val_labels(SA058X <- c(
1 = "Less than 65",
2 = "65 or older",
7 = "Refused",
8 = "Not Ascertained",
9 = "Don't Know"
))

val_labels(SA059X <- c(
1 = "Very worried",
2 = "Somewhat worried",
3 = "Not at all worried",
7 = "Refused",
8 = "Not Ascertained",
9 = "Don't Know"
))

val_labels(SA060X <- c(
1 = "No problem",
2 = "A small problem",
3 = "A moderate problem",
4 = "A big problem",
5 = "A very big problem",
7 = "Refused",
8 = "Not Ascertained",
9 = "Don't Know"
))

val_labels(SA061X <- c(
1 = "A few things",
2 = "A lot of things",
3 = "Almost everything",
7 = "Refused",
8 = "Not Ascertained",
9 = "Don't Know"
))

val_labels(SA062X <- c(
1 = "Sometimes",
2 = "Often",
3 = "All of the time",
7 = "Refused",
8 = "Not Ascertained",
9 = "Don't Know"
))

val_labels(SA063X <- c(
1 = "Difficulty remembering only",
2 = "Difficulty concentrating only",
3 = "Difficulty with both remembering and concentrating",
7 = "Refused",
8 = "Not Ascertained",
9 = "Don't Know"
))

val_labels(SA064X <- c(
1 = "Colonoscopy",
2 = "Sigmoidoscopy",
3 = "Both",
7 = "Refused",
8 = "Not Ascertained",
9 = "Don't Know"
))

val_labels(SA065X <- c(
1 = "Part of a routine exam",
2 = "Because of a problem",
3 = "Follow-up test of an earlier test or screening exam",
4 = "Other reason",
7 = "Refused",
8 = "Not Ascertained",
9 = "Don't Know"
))

val_labels(SA066X <- c(
1 = "1 vaccination",
2 = "2 vaccinations",
3 = "3 vaccinations",
4 = "4 vaccinations",
5 = "5 vaccinations",
6 = "6 or more vaccinations",
7 = "Refused",
8 = "Not Ascertained",
9 = "Don't Know"
))

val_labels(SA067X <- c(
1 = "Pfizer-BioNTech Comirnaty? shot",
2 = "Moderna Spikevax? shot",
3 = "Johnson and Johnson (Janssen) shot",
4 = "Novavax shot",
5 = "One of the brands that requires two initial shots, but not sure which brand",
6 = "None of these brands",
7 = "Refused",
8 = "Not Ascertained",
9 = "Don't Know"
))

val_labels(SA068X <- c(
97 = "Refused",
98 = "Not Ascertained",
99 = "Don't Know"
))

val_labels(SA069X <- c(
97 = "Refused",
98 = "Not Ascertained",
99 = "Don't Know"
))

val_labels(SA070X <- c(
97 = "Refused",
98 = "Not Ascertained",
99 = "Don't Know"
))

val_labels(SA071X <- c(
1 = "Less than 1 month",
2 = "1 month to less than 6 months",
3 = "6 months to less than 1 year",
4 = "1 year or more",
7 = "Refused",
8 = "Not Ascertained",
9 = "Don't Know"
))

val_labels(SA072X <- c(
1 = "Type 1",
2 = "Type 2",
3 = "Other type of diabetes",
7 = "Refused",
8 = "Not Ascertained",
9 = "Don't Know"
))

val_labels(SA073X <- c(
1 = "No difficulty",
2 = "Some difficulty",
3 = "A lot of difficulty",
4 = "Cannot do at all",
7 = "Refused",
8 = "Not Ascertained",
9 = "Don't Know"
))

val_labels(SA074X <- c(
00 = "Never attended/kindergarten only",
01 = "Grade 1-11",
02 = "12th grade, no diploma",
03 = "GED or equivalent",
04 = "High School Graduate",
05 = "Some college, no degree",
06 = "Associate degree: occupational, technical, or vocational program",
07 = "Associate degree: academic program",
08 = "Bachelor's degree (Example: BA, AB, BS, BBA)",
09 = "Master's degree (Example: MA, MS, MEng, MEd, MBA)",
10 = "Professional School or Doctoral degree (Example: MD, DDS, DVM, JD, PhD, EdD)",
97 = "Refused",
98 = "Not Ascertained",
99 = "Don't Know"
))

val_labels(SA075X <- c(
1 = "Employee of a PRIVATE company for wages",
2 = "A FEDERAL government employee",
3 = "A STATE government employee",
4 = "A LOCAL government employee",
5 = "Self-employed in OWN business, professional practice or farm",
6 = "Working WITHOUT PAY in a family-owned business or farm",
7 = "Refused",
8 = "Not Ascertained",
9 = "Don't Know"
))

val_labels(SA076X <- c(
1 = "Within the past 12 months",
2 = "1-5 years ago",
3 = "Over 5 years ago",
4 = "Never worked",
7 = "Refused",
8 = "Not Ascertained",
9 = "Don't Know"
))

val_labels(SA077X <- c(
1 = "Within the past 12 months",
2 = "1-5 years ago",
3 = "Over 5 years ago",
4 = "Never worked",
7 = "Refused",
8 = "Not Ascertained",
9 = "Don't Know"
))

val_labels(SA078X <- c(
01 = "Unemployed, laid off, looking for work",
02 = "Seasonal/contract work",
03 = "Retired",
04 = "Unable to work for health reasons/disabled",
05 = "Taking care of house or family",
06 = "Going to school",
07 = "Working at a family-owned job or business not for pay",
08 = "Other",
97 = "Refused",
98 = "Not Ascertained",
99 = "Don't Know"
))

val_labels(SA079X <- c(
0 = "0",
1 = "1",
2 = "2 or 3",
3 = "4 or more",
7 = "Refused",
8 = "Not Ascertained",
9 = "Don't Know"
))

val_labels(SA080X <- c(
1 = "Part of a routine exam",
2 = "Because of a problem",
3 = "Other reason",
7 = "Refused",
8 = "Not Ascertained",
9 = "Don't Know"
))

val_labels(SA081X <- c(
1 = "Every day",
2 = "Some days",
3 = "Not at all",
7 = "Refused",
8 = "Not Ascertained",
9 = "Don't Know"
))

val_labels(SA082X <- c(
1 = "Hearing aid fit by a health care professional",
2 = "Hearing aid purchased online or over the counter without assistance from a health care professional",
7 = "Refused",
8 = "Not Ascertained",
9 = "Don't Know"
))

val_labels(SA083X <- c(
1 = "All of the time",
2 = "Some of the time",
3 = "Rarely",
4 = "Never",
7 = "Refused",
8 = "Not Ascertained",
9 = "Don't Know"
))

val_labels(SA084X <- c(
1 = "Excellent",
2 = "Good",
3 = "A little trouble hearing",
4 = "Moderate trouble",
5 = "A lot of trouble",
6 = "Deaf",
7 = "Refused",
8 = "Not Ascertained",
9 = "Don't Know"
))

val_labels(SA085X <- c(
1 = "Owned or being bought",
2 = "Rented",
3 = "Other arrangement",
7 = "Refused",
8 = "Not Ascertained",
9 = "Don't Know"
))

val_labels(SA086X <- c(
1 = "Less than a year ago",
2 = "1 to 2 years ago",
3 = "3 to 4 years ago",
4 = "5 to 9 years ago",
5 = "10 or more years ago",
7 = "Refused",
8 = "Not Ascertained",
9 = "Don't Know"
))

val_labels(SA087X <- c(
1 = "Less than 1 year",
2 = "1 to 2 years",
3 = "3 to 4 years",
4 = "5 to 9 years",
5 = "10 years or more",
7 = "Refused",
8 = "Not Ascertained",
9 = "Don't Know"
))

val_labels(SA088X <- c(
1 = "Never",
2 = "Some days",
3 = "Most days",
4 = "Every day",
7 = "Refused",
8 = "Not Ascertained",
9 = "Don't Know"
))

val_labels(SA089X <- c(
1 = "Daily",
2 = "Weekly",
3 = "Monthly",
4 = "A few times a year",
5 = "Never",
7 = "Refused",
8 = "Not Ascertained",
9 = "Don't Know"
))

val_labels(SA090X <- c(
1 = "At least once a week",
2 = "A few times a month",
3 = "A few times a year",
4 = "Less than once a year",
5 = "Never",
7 = "Refused",
8 = "Not Ascertained",
9 = "Don't Know"
))

val_labels(SA091X <- c(
1 = "English",
2 = "Other language",
7 = "Refused",
8 = "Not Ascertained",
9 = "Don't Know"
))

val_labels(SA092X <- c(
1 = "A little",
2 = "A lot",
3 = "Somewhere in between a little and a lot",
7 = "Refused",
8 = "Not Ascertained",
9 = "Don't Know"
))

val_labels(SA093X <- c(
1 = "Not at all",
2 = "A little",
3 = "A lot",
7 = "Refused",
8 = "Not Ascertained",
9 = "Don't Know"
))

val_labels(SA094X <- c(
01 = "No reason/never thought about it",
02 = "Didn't need it/didn't know I needed this type of test",
03 = "Doctor didn't order it/didn't say I needed it",
04 = "Haven't had any problems",
05 = "Put it off/didn't get around to it",
06 = "Too expensive/no insurance/cost",
07 = "Too painful, unpleasant, or embarrassing",
08 = "Don't have a doctor",
09 = "I am too old",
10 = "I am too young",
11 = "Other",
97 = "Refused",
98 = "Not Ascertained",
99 = "Don't Know"
))

val_labels(SA095X <- c(
1 = "Married",
2 = "Living with a partner together as an unmarried couple",
3 = "Neither",
7 = "Refused",
8 = "Not Ascertained",
9 = "Don't Know"
))

val_labels(SA096X <- c(
1 = "Part A- hospital only",
2 = "Part B- medical only",
3 = "Both Part A and Part B",
7 = "Refused",
8 = "Not Ascertained",
9 = "Don't Know"
))

val_labels(SA097X <- c(
1 = "Menthol",
2 = "Non-menthol",
3 = "No usual type",
7 = "Refused",
8 = "Not ascertained",
9 = "Don't Know"
))

val_labels(SA098X <- c(
1 = "Mentioned",
2 = "Not mentioned",
7 = "Refused",
8 = "Not Ascertained",
9 = "Don't Know"
))

val_labels(SA099X <- c(
01 = "January",
02 = "February",
03 = "March",
04 = "April",
05 = "May",
06 = "June",
07 = "July",
08 = "August",
09 = "September",
10 = "October",
11 = "November",
12 = "December",
97 = "Refused",
98 = "Not Ascertained",
99 = "Don't Know"
))

val_labels(SA100X <- c(
97 = "Refused",
98 = "Not Ascertained",
99 = "Don't Know"
))

val_labels(SA101X <- c(
1 = "A follow up of an abnormal mammogram",
2 = "Because of a breast problem",
3 = "I am high risk due to family history, genetic test, or another reason",
4 = "Part of a routine exam",
5 = "Other",
7 = "Refused",
8 = "Not Ascertained",
9 = "Don't Know"
))

val_labels(SA102X <- c(
1 = "Not at all",
2 = "A little",
3 = "A lot",
4 = "Somewhere in between a little and a lot",
7 = "Refused",
8 = "Not Ascertained",
9 = "Don't Know"
))

val_labels(SA103X <- c(
1 = "1 time",
2 = "2 times",
3 = "3 to 4 times",
4 = "5 to 7 times",
5 = "8 = or more times",
7 = "Refused",
8 = "Not Ascertained",
9 = "Don't Know"
))

val_labels(SA104X <- c(
1 = "1 relative",
2 = "2 relatives",
3 = "3 or more relatives",
7 = "Refused",
8 = "Not Ascertained",
9 = "Don't Know"
))

val_labels(SA105X <- c(
0 = "0 relatives",
1 = "1 relative",
2 = "2 relatives",
3 = "3 or more relatives",
7 = "Refused",
8 = "Not Ascertained",
9 = "Don't Know"
))

val_labels(SA106X <- c(
1 = "Not at all",
2 = "Several days",
3 = "More than half the days",
4 = "Nearly every day",
7 = "Refused",
8 = "Not Ascertained",
9 = "Don't Know"
))

val_labels(SA107X <- c(
1 = "Excellent",
2 = "Very Good",
3 = "Good",
4 = "Fair",
5 = "Poor",
7 = "Refused",
8 = "Not Ascertained",
9 = "Don't Know"
))

val_labels(SA108X <- c(
1 = "A doctor's office or health center",
2 = "Urgent care center or clinic in a drug store or grocery store",
3 = "Hospital emergency room",
4 = "A VA Medical Center or VA outpatient clinic",
5 = "Some other place",
6 = "Does not go to one place most often",
7 = "Refused",
8 = "Not Ascertained",
9 = "Don't Know"
))

val_labels(SA109X <- c(
1 = "Less than ^HDHPAMT_A",
2 = "^HDHPAMT_A or more",
7 = "Refused",
8 = "Not Ascertained",
9 = "Don't Know"
))

val_labels(SA110X <- c(
1 = "Relative who lives in household",
2 = "Relative who doesn't live in household",
3 = "Nonrelative who lives in household",
4 = "Nonrelative who does not live in household",
7 = "Refused",
8 = "Not Ascertained",
9 = "Don't Know"
))

val_labels(SA111X <- c(
1 = "Child",
2 = "Spouse",
3 = "Former spouse",
4 = "Some other relationship",
7 = "Refused",
8 = "Not Ascertained",
9 = "Don't Know"
))

val_labels(SA112X <- c(
0 = "0 tests",
1 = "1 test",
2 = "2 tests",
3 = "3 tests",
4 = "4 tests",
5 = "5 tests",
6 = "6 or more tests",
7 = "Refused",
8 = "Not Ascertained",
9 = "Don't Know"
))

val_labels(SA113X <- c(
1 = "One pneumonia shot",
2 = "Two pneumonia shots",
3 = "More than two pneumonia shots",
7 = "Refused",
8 = "Not Ascertained",
9 = "Don't Know"
))

val_labels(SA114X <- c(
1 = "Available",
2 = "Not Available or not able to answer right now",
3 = "Physical or mental condition prohibits responding",
7 = "Refused",
8 = "Not Ascertained"
))

val_labels(SA115X <- c(
1 = "Very satisfied",
2 = "Satisfied",
3 = "Dissatisfied",
4 = "Very dissatisfied",
7 = "Refused",
8 = "Not Ascertained",
9 = "Don't Know"
))

val_labels(SA116X <- c(
97 = "Refused",
98 = "Not Ascertained",
99 = "Don't Know"
))

val_labels(SA117X <- c(
1 = "Always",
2 = "Usually",
3 = "About half the time",
4 = "Seldom",
5 = "Never",
7 = "Refused",
8 = "Not Ascertained",
9 = "Don't Know"
))

val_labels(SA118X <- c(
1 = "Male",
2 = "Female",
7 = "Refused",
8 = "Not Ascertained",
9 = "Don't Know"
))

val_labels(SA119X <- c(
1 = "First shot",
2 = "Second shot",
7 = "Refused",
8 = "Not Ascertained",
9 = "Don't Know"
))

val_labels(SA120X <- c(
1 = "One Shingrix shot",
2 = "Two Shingrix shots",
7 = "Refused",
8 = "Not Ascertained",
9 = "Don't Know"
))

val_labels(SA121 <-c(
1 = "SSI",
2 = "SSDI",
3 = "Both SSI and SSDI",
7 = "Refused",
8 = "Not Ascertained",
9 = "Don't Know"
) )

val_labels(SA122X <- c(
0 = "Not applicable/None eligible",
1 = "Completed",
2 = "HH member selected",
3 = "Reached Sample Adult",
4 = "Started HIS section",
5 = "Sufficient Partial",
6 = "HH member selected and no longer eligible",
7 = "Refused"
))

val_labels(SA123X <- c(
0 = "Never",
1 = "Within the past year (anytime less than 12 months ago)",
2 = "Within the last 2 years (1 year but less than 2 years ago)",
3 = "Within the last 3 years (2 years but less than 3 years ago)",
4 = "Within the last 5 years (3 years but less than 5 years ago)",
5 = "5 years ago or more",
7 = "Refused",
8 = "Not Ascertained",
9 = "Don't Know"
))

val_labels(SA124X <- c(
1 = "Within the past year (anytime less than 12 months ago)",
2 = "Within the past 2 years (1 year but less than 2 years ago)",
3 = "Within the past 3 years (2 years but less than 3 years ago)",
4 = "Within the past 5 years (3 years but less than 5 years ago)",
5 = "Within the past 10 years (5 years but less than 10 year ago)",
6 = "10 years ago or more",
7 = "Refused",
8 = "Not Ascertained",
9 = "Don't Know"
))

val_labels(SA125X <- c(
0 = "Never",
1 = "Within the past year (anytime less than 12 months ago)",
2 = "Within the last 2 years (1 year but less than 2 years ago)",
3 = "Within the last 3 years (2 years but less than 3 years ago)",
4 = "Within the last 5 years (3 years but less than 5 years ago)",
5 = "Within the last 10 years (5 years but less than 10 years ago)",
6 = "10 years ago or more",
7 = "Refused",
8 = "Not Ascertained",
9 = "Don't Know"
))

val_labels(SA126X <- c(
1 = "Less than 1 year",
2 = "1 to 3 years",
3 = "4 to 10 years",
4 = "11 to 20 years",
5 = "More than 20 years",
7 = "Refused",
8 = "Not Ascertained",
9 = "Don't Know"
))

val_labels(SA127X <- c(
1 = "Less than 100 rounds",
2 = "100 to less than 1,000 rounds",
3 = "1,000 to less than 10,000 rounds",
4 = "10,000 rounds or more",
7 = "Refused",
8 = "Not Ascertained",
9 = "Don't Know"
))

val_labels(SA128X <- c(
1 = "Often true",
2 = "Sometimes true",
3 = "Never true",
7 = "Refused",
8 = "Not Ascertained",
9 = "Don't Know"
))

val_labels(SA129X <- c(
1 = "Yes",
2 = "There is NO place",
3 = "There is MORE THAN ONE place",
7 = "Refused",
8 = "Not Ascertained",
9 = "Don't Know"
))

val_labels(SA130X <- c(
1 = "Yes",
2 = "No",
7 = "Refused",
8 = "Not Ascertained",
9 = "Don't Know"
))

val_labels(SA131X <- c(
0 = "0",
1 = "1",
2 = "2",
3 = "3",
4 = "4",
5 = "5 or more",
7 = "Refused",
8 = "Not Ascertained",
9 = "Don't Know"
))

val_labels(SA132X <- c(
1 = "Before pregnancy",
2 = "During pregnancy",
7 = "Refused",
8 = "Not Ascertained",
9 = "Don't Know"
))

val_labels(SA133X <- c(
1 = "Before pregnancy",
2 = "During pregnancy",
3 = "After pregnancy",
7 = "Refused",
8 = "Not Ascertained",
9 = "Don't Know"
))