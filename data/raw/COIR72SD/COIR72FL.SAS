libname user 'C:\COUNTRIES\CO72';
Proc format;
  value F00001_
     0 = "13-14"
     1 = "15-19"
     2 = "20-24"
     3 = "25-29"
     4 = "30-34"
     5 = "35-39"
     6 = "40-44"
     7 = "45-49"
     ;
  value F00002_
     1 = "Month and year - information complete"
     2 = "Month and age - year imputed"
     3 = "Year and age - month imputed"
     4 = "Year and age - year ignored"
     5 = "Year - age/month imputed"
     6 = "Age - year/month imputed"
     7 = "Month - age/year imputed"
     8 = "None - all imputed"
     ;
  value F00003_
     1 = "Completed"
     2 = "Not at home"
     3 = "Postponed"
     4 = "Refused"
     5 = "Partially completed"
     6 = "Respondent incapacitated"
     7 = "Other"
     ;
  value F00004_
     0 = "No calendar"
     ;
  value F00005_
     0 = "No calendar"
     ;
  value F00006_
     0 = "No calendar"
     ;
  value F00007_
     0 = "All woman sample"
     1 = "Ever married sample"
     ;
  value F00008_
     1 = "Atlantica"
     2 = "Oriental"
     3 = "Central"
     4 = "Pacifica"
     5 = "Bogota"
     6 = "Orinoquia/Amazonia"
     ;
  value F00009_
     1 = "Urban"
     2 = "Rural"
     ;
  value F00010_
     0 = "Capital, large city"
     1 = "Small city"
     2 = "Town"
     3 = "Countryside"
     ;
  value F00011_
     0 = "Husband not in household"
     ;
  value F00012_
     0 = "Not selected"
     1 = "Selected"
     ;
  value F00013_
     0 = "Woman not selected"
     1 = "Woman selected and interviewed"
     2 = "Woman selected, but privacy not possible"
     3 = "Woman selected, but not interviewed"
     ;
  value F00014_
     1 = "English"
     2 = "Language 2"
     3 = "Language 3"
     4 = "Language 4"
     5 = "Language 5"
     6 = "Language 6"
     ;
  value F00015_
     1 = "English"
     2 = "Language 2"
     3 = "Language 3"
     4 = "Language 4"
     5 = "Language 5"
     6 = "Language 6"
     ;
  value F00016_
     1 = "English"
     2 = "Language 2"
     3 = "Language 3"
     4 = "Language 4"
     5 = "Language 5"
     6 = "Language 6"
     ;
  value F00017_
     0 = "No"
     1 = "Yes"
     ;
  value F00018_
     1 = "Atlantica"
     2 = "Oriental"
     3 = "Central"
     4 = "Pacifica"
     5 = "Bogota"
     6 = "Orinoquia/Amazonia"
     ;
  value F00019_
     1 = "Urban"
     2 = "Rural"
     ;
  value F00020_
     0 = "Capital, large city"
     1 = "City"
     2 = "Town"
     3 = "Countryside"
     4 = "Abroad"
     ;
  value F00021_
    95 = "Always"
    96 = "Visitor"
    97 = "Inconsistent"
    98 = "Don't know"
     ;
  value F00022_
     0 = "Capital, large city"
     1 = "City"
     2 = "Town"
     3 = "Countryside"
     4 = "Abroad"
     ;
  value F00023_
     1 = "Region 1"
     2 = "Region 2"
     3 = "Region 3"
     4 = "Region 4"
    96 = "Abroad"
     ;
  value F00024_
     0 = "No education"
     1 = "Primary"
     2 = "Secondary"
     3 = "Higher"
     ;
  value F00025_
     0 = "No years completed at level V106"
     ;
  value F00026_
    11 = "Piped water from utility company"
    12 = "Piped water from rural system"
    13 = "Public tap"
    21 = "Open well with sump pump"
    22 = "Open well without sump pump"
    42 = "River/stream/spring"
    51 = "Rain water"
    61 = "Tanker truck"
    62 = "Cart with small tank"
    71 = "Bottled water"
    96 = "Other"
    97 = "Not a dejure resident"
     ;
  value F00027_
   996 = "On premises"
   997 = "Not a dejure resident"
   998 = "Don't know"
     ;
  value F00028_
    11 = "Toilet connected to sewer"
    12 = "Toilet connected to septic well"
    13 = "Toilet connected to plot/yard"
    21 = "Traditional pit toilet"
    22 = "Traditional toilet to sea/river (low tide)"
    31 = "No toilet facility"
    96 = "Other"
    97 = "Not a dejure resident"
     ;
  value F00029_
     0 = "No"
     1 = "Yes"
     7 = "Not a dejure resident"
     ;
  value F00030_
     0 = "No"
     1 = "Yes"
     7 = "Not a dejure resident"
     ;
  value F00031_
     0 = "No"
     1 = "Yes"
     7 = "Not a dejure resident"
     ;
  value F00032_
     0 = "No"
     1 = "Yes"
     7 = "Not a dejure resident"
     ;
  value F00033_
     0 = "No"
     1 = "Yes"
     7 = "Not a dejure resident"
     ;
  value F00034_
     0 = "No"
     1 = "Yes"
     7 = "Not a dejure resident"
     ;
  value F00035_
     0 = "No"
     1 = "Yes"
     7 = "Not a dejure resident"
     ;
  value F00036_
    10 = "NATURAL"
    11 = "Earth/Sand"
    20 = "RUDIMENTARY"
    21 = "Wood planks"
    22 = "Cement/gravel"
    30 = "FINISHED"
    31 = "Rug, carpet"
    32 = "Polished wood, parquet"
    33 = "Marble"
    34 = "Ceramic tiles, vinyl, bricks"
    96 = "OTHER"
    97 = "Not a dejure resident"
     ;
  value F00037_
    11 = "Bamboo(guadua), straw, other plants"
    12 = "Bamboo without plaster"
    21 = "Adobe (mud bricks)"
    22 = "Bamboo with mud plaster"
    23 = "Planks"
    24 = "Zinc, canvas, plastics"
    31 = "Bricks/polished wood/pre-manufactured material"
    32 = "Prefabricated material"
    95 = "Without walls"
    96 = "Other"
    97 = "Not a dejure resident"
     ;
  value F00038_
    10 = "NATURAL"
    20 = "RUDIMENTARY"
    30 = "FINISHED"
    96 = "Other"
    97 = "Not a dejure resident"
     ;
  value F00039_
    96 = "Other"
     ;
  value F00040_
     1 = "Indigenous"
     2 = "Gypsy (ROM)"
     3 = "Raizal from archipelago (San Andres)"
     4 = "Palanquero from San Basilio"
     5 = "Black/Mulato/Afro-Colombian/Afro-descendant"
     6 = "None of the above"
     ;
  value F00041_
    97 = "Inconsistent"
     ;
  value F00042_
     0 = "Capital, large city"
     1 = "Small city"
     2 = "Town"
     3 = "Countryside"
     ;
  value F00043_
     1 = "Usual resident"
     2 = "Visitor"
     ;
  value F00044_
     0 = "Abroad"
     1 = "Atlantica"
     2 = "Oriental"
     3 = "Central"
     4 = "Pacifica"
     5 = "Bogota"
     6 = "Territorios Nacionales"
    97 = "Not dejure resident"
     ;
  value F00045_
     0 = "Abroad"
     1 = "Urban"
     2 = "Rural"
     7 = "Not a dejure resident"
     ;
  value F00046_
     0 = "Capital, large city"
     1 = "City"
     2 = "Town"
     3 = "Countryside"
     4 = "Abroad"
     7 = "Not a dejure resident"
     ;
  value F00047_
     0 = "No education"
     1 = "Incomplete primary"
     2 = "Complete primary"
     3 = "Incomplete secondary"
     4 = "Complete secondary"
     5 = "Higher"
     ;
  value F00048_
     1 = "Head"
     2 = "Wife or husband"
     3 = "Son/daughter"
     4 = "Son/daughter-in-law"
     5 = "Grandchild"
     6 = "Parent"
     7 = "Parent-in-law"
     8 = "Brother/sister"
     9 = "Co-spouse"
    10 = "Other relative"
    11 = "Adopted/foster child"
    12 = "Not related"
    13 = "Niece/nephew by blood"
    14 = "Niece/nephew by marriage"
    15 = "Sister brother-in-law"
    16 = "In-house maid"
    17 = "Related to house maid"
    18 = "Tenant"
    98 = "DK"
     ;
  value F00049_
     1 = "Male"
     2 = "Female"
     3 = "Male transgender"
     ;
  value F00050_
    97 = "97+"
    98 = "Don't know"
     ;
  value F00051_
     0 = "No"
     1 = "Yes"
     7 = "Not a dejure resident"
     ;
  value F00052_
     0 = "Cannot read at all"
     1 = "Able to read only parts of sentence"
     2 = "Able to read whole sentence"
     3 = "No card with required language"
     4 = "Blind/visually impaired"
     ;
  value F00053_
     0 = "No"
     1 = "Yes"
     ;
  value F00054_
     0 = "Not at all"
     1 = "Less than once a week"
     2 = "At least once a week"
     3 = "Almost every day"
     ;
  value F00055_
     0 = "Not at all"
     1 = "Less than once a week"
     2 = "At least once a week"
     3 = "Almost every day"
     ;
  value F00056_
     0 = "Not at all"
     1 = "Less than once a week"
     2 = "At least once a week"
     3 = "Almost every day"
     ;
  value F00057_
     0 = "No"
     1 = "Yes"
     7 = "Not a dejure resident"
     ;
  value F00058_
     1 = "Natural gas"
     2 = "Propane gas"
     3 = "Kerosene, oil, cocinol, diesel, gasoline, alcohol"
     4 = "Electricity"
     5 = "Firewood, charcoal"
     6 = "Mineral coal"
     7 = "Disposable material"
    95 = "Do not cook"
    96 = "Other"
    97 = "Not a dejure resident"
     ;
  value F00059_
     0 = "0 ppm (no iodine)"
     7 = "7 ppm"
    15 = "15 ppm"
    30 = "30 ppm"
   994 = "Salt not tested"
   995 = "No salt"
   997 = "Not a dejure resident"
     ;
  value F00060_
     0 = "None"
    90 = "90+"
     ;
  value F00061_
     0 = "No"
     1 = "Yes"
     ;
  value F00062_
     0 = "No"
     1 = "Yes"
     ;
  value F00063_
     0 = "No"
     1 = "Yes"
     ;
  value F00064_
     0 = "No"
     1 = "Yes"
     ;
  value F00065_
     0 = "Never"
     1 = "Yes, last 12 months"
     2 = "Yes, before last 12 months"
     3 = "Yes, can't establish when"
     ;
  value F00066_
     0 = "Not at all"
     1 = "Less than once a week"
     2 = "At least once a week"
     3 = "Almost every day"
     ;
  value F00067_
     1 = "Poorest"
     2 = "Poorer"
     3 = "Middle"
     4 = "Richer"
     5 = "Richest"
     ;
  value F00068_
     1 = "Poorest"
     2 = "Poorer"
     3 = "Middle"
     4 = "Richer"
     5 = "Richest"
     ;
  value F00069_
     0 = "No net"
     1 = "Only treated nets"
     2 = "Both treated and untreated nets"
     3 = "Only untreated nets"
     ;
  value F00070_
     0 = "Single birth"
     1 = "1st of multiple"
     2 = "2nd of multiple"
     3 = "3rd of multiple"
     4 = "4th of multiple"
     5 = "5th of multiple"
     ;
  value F00071_
     1 = "Male"
     2 = "Female"
     ;
  value F00072_
     0 = "No"
     1 = "Yes"
     ;
  value F00073_
   100 = "Died on day of birth"
   101 = "Days: 1"
   199 = "Days: number missing"
   201 = "Months: 1"
   299 = "Months: number missing"
   301 = "Years: 1"
   399 = "Years: number missing"
   997 = "Inconsistent"
   998 = "Don't know"
     ;
  value F00074_
     0 = "Respondent"
     1 = "Father"
     2 = "Other relative"
     3 = "Someone else"
     4 = "Lives elsewhere"
     ;
  value F00075_
     1 = "Month and year - information complete"
     2 = "Month and age - year imputed"
     3 = "Year and age - month imputed"
     4 = "Year and age - year ignored"
     5 = "Year - age/month imputed"
     6 = "Age - year/month imputed"
     7 = "Month - age/year imputed"
     8 = "None - all imputed"
     ;
  value F00076_
     0 = "No flag"
     1 = "> interview"
     2 = "< breastfeeding"
     3 = "< age supplemented"
     4 = "< first breastfed"
     5 = "< last vaccination"
     6 = "Outside range"
     7 = "Imputed, units given"
     8 = "Imputed, no units"
     ;
  value F00077_
     0 = "No"
     1 = "Yes"
     ;
  value F00078_
     0 = "Not listed in household"
     ;
  value F00079_
     0 = "No births"
     ;
  value F00080_
     0 = "No births"
     ;
  value F00081_
     0 = "No or unsure"
     1 = "Yes"
     ;
  value F00082_
   100 = "Day of interview"
   101 = "Days: 1"
   199 = "Days: number missing"
   201 = "Weeks: 1"
   299 = "Weeks: number missing"
   301 = "Months: 1"
   399 = "Months: number missing"
   401 = "Years: 1"
   499 = "Years: number missing"
   993 = "Hysterectomy"
   994 = "In menopause"
   995 = "Before last birth"
   996 = "Never menstruated"
   997 = "Inconsistent"
   998 = "Don't know"
     ;
  value F00083_
     0 = "No"
     1 = "Yes"
     ;
  value F00084_
     1 = "During her period"
     2 = "After period ended"
     3 = "Middle of the cycle"
     4 = "Before period begins"
     5 = "At any time"
     6 = "Other"
     8 = "Don't know"
     ;
  value F00085_
     6 = "6+"
     ;
  value F00086_
   996 = "Negative interval"
     ;
  value F00087_
     7 = "Month - exact date"
     8 = "None - date placed"
     ;
  value F00088_
     1 = "Then"
     2 = "Later"
     3 = "Not at all"
     ;
  value F00089_
   993 = "Hysterectomy"
   994 = "In menopause"
   995 = "Before last birth"
   996 = "Never menstruated"
   997 = "Inconsistent"
   998 = "Don't know"
     ;
  value F00090_
     0 = "No flag"
     1 = "N > interval since birth"
     2 = "N + amen. > interval"
     3 = "N, but period not returned"
     4 = "95, but no birth"
     5 = "95, but period returned"
     6 = "96, but period returned"
     7 = "N, last period during pregnancy"
     8 = "96, had children"
     9 = "N, period before last birth"
     ;
  value F00091_
     0 = "No"
     1 = "Yes"
     ;
  value F00092_
    97 = "Inconsistent"
    98 = "Don't know"
     ;
  value F00093_
  9997 = "Inconsistent"
  9998 = "Don't know"
     ;
  value F00094_
  9997 = "Inconsistent"
  9998 = "Don't know"
     ;
  value F00095_
     1 = "Month and year"
     2 = "From calendar"
     5 = "Year"
     7 = "Month"
     8 = "None"
     ;
  value F00096_
    97 = "Inconsistent"
    98 = "Don't know"
     ;
  value F00097_
     0 = "No"
     1 = "Yes"
     ;
  value F00098_
     0 = "No prior child"
     ;
  value F00099_
     0 = "No"
     1 = "Yes"
     ;
  value F00100_
     0 = "No"
     1 = "Yes"
     ;
  value F00101_
    97 = "Inconsistent"
    98 = "Don't know"
     ;
  value F00102_
  9997 = "Inconsistent"
  9998 = "Don't know"
     ;
  value F00103_
  9997 = "Inconsistent"
  9998 = "Don't know"
     ;
  value F00104_
     1 = "Month and year"
     2 = "From calendar"
     5 = "Year"
     7 = "Month"
     8 = "None"
     ;
  value F00105_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00106_
     0 = "Knows no method"
     1 = "Knows only folkloric method"
     2 = "Knows only traditional method"
     3 = "Knows modern method"
     ;
  value F00107_
     0 = "Never used"
     1 = "Used only folkloric method"
     2 = "Used only traditional method"
     3 = "Used modern method"
     ;
  value F00108_
     0 = "No"
     1 = "Yes, used outside calendar"
     2 = "Yes, used in calendar"
     ;
  value F00109_
     1 = "Modern method"
     2 = "Traditional method"
     3 = "Folkloric method"
     ;
  value F00110_
     0 = "No"
     1 = "Yes"
     7 = "Not in contraception table, but as a current method"
     8 = "Not asked"
     ;
  value F00111_
     0 = "No"
     1 = "Yes"
     ;
  value F00112_
     0 = "No"
     1 = "Yes"
     ;
  value F00113_
     4 = "4+"
     5 = "Never used"
     ;
  value F00114_
     0 = "Not using"
     1 = "Pill"
     2 = "IUD"
     3 = "Monthy Injection"
     4 = "Diaphragm"
     5 = "Male condom"
     6 = "Female sterilization"
     7 = "Male sterilization"
     8 = "Periodic abstinence"
     9 = "Withdrawal"
    10 = "Other traditional"
    11 = "Implants/Norplant"
    12 = "Prolonged abstinence"
    13 = "Lactational amenorrhea (LAM)"
    14 = "Female condom"
    15 = "Foam or jelly"
    16 = "Emergency contraception"
    17 = "Patch"
    18 = "Standard days method (SDM)"
    19 = "Injections every three months"
    20 = "Vaginal ring"
     ;
  value F00115_
     0 = "No method"
     1 = "Folkloric method"
     2 = "Traditional method"
     3 = "Modern method"
     ;
  value F00116_
     1 = "Month and year - information complete"
     5 = "Year - month imputed, age calculated"
     7 = "Month - date placed"
     8 = "None - date placed"
     9 = "From calendar"
     ;
  value F00117_
     1 = "<2"
     2 = "2-3"
     3 = "4-5"
     4 = "6-7"
     5 = "8-9"
     6 = "10+"
     ;
  value F00118_
     1 = "<25"
     2 = "25-29"
     3 = "30-34"
     4 = "35-39"
     5 = "40-44"
     6 = "45-49"
     ;
  value F00119_
     0 = "Single, before marriage"
     1 = "0-4"
     2 = "5-9"
     3 = "10-14"
     4 = "15-19"
     5 = "20-24"
     6 = "25+"
     ;
  value F00120_
     5 = "5+"
     ;
  value F00121_
    98 = "Don't know"
     ;
  value F00122_
    98 = "Don't know"
     ;
  value F00123_
9999995 = "Free   "
9999998 = "Don't know"
     ;
  value F00124_
    10 = "PUBLIC"
    11 = "Government hospital/health center/health post"
    20 = "PRIVATE MEDICAL"
    21 = "Private hospital/clinic"
    22 = "EPS health center"
    23 = "Private doctor"
    24 = "PROFAMILIA"
    25 = "Drugstore/pharmacy"
    30 = "OTHER PRIVATE"
    31 = "Supermarket/shop"
    96 = "OTHER"
    98 = "DK"
     ;
  value F00125_
     1 = "Government clinic/pharmacy"
     2 = "Government home/community delivery"
     3 = "NGO"
     4 = "Private clinic/delivery"
     5 = "Pharmacy"
     6 = "Shop, church, friend"
     7 = "Other"
     8 = "Don't know"
     ;
  value F00126_
    95 = "All calendar {A core}"
    96 = "96+ {B core}"
   997 = "Inconsistent"
   998 = "Don't know"
     ;
  value F00127_
     1 = "Pill"
     2 = "IUD"
     3 = "Monthly injection"
     4 = "Diaphragm"
     5 = "Male condom"
     6 = "Female sterilization"
     7 = "Male sterilization"
     8 = "Periodic abstinence"
     9 = "Withdrawal"
    10 = "Other traditional"
    11 = "Implants/Norplant"
    12 = "Prolonged abstinence"
    13 = "Lactational amenorrhea (LAM)"
    14 = "Female condom"
    15 = "Foam or jelly"
    16 = "Emergency contraception"
    17 = "Patch"
    18 = "Standard days method (SDM)"
    19 = "Injection every three months"
    20 = "Vaginal ring"
     ;
  value F00128_
     1 = "Became pregnant"
     2 = "Wanted to become pregnant"
     3 = "Husband disapproved"
     4 = "Side effects"
     5 = "Health concerns"
     6 = "Access, availability"
     7 = "Wanted more effective method"
     8 = "Inconvenient to use"
     9 = "Infrequent sex, husband away"
    10 = "Cost"
    11 = "Fatalistic"
    12 = "Difficult pregnancy, menopause"
    13 = "Marital dissolution"
    14 = "Other"
    98 = "Don't know"
     ;
  value F00129_
     1 = "Currently using"
     2 = "Used since last birth"
     3 = "Used before last birth"
     4 = "Never used"
     ;
  value F00130_
     1 = "In next 12 months"
     2 = "Use later"
     3 = "Unsure about timing"
     4 = "Unsure about use"
     5 = "Does not intend"
     6 = "Never had sex"
     ;
  value F00131_
     1 = "Pill"
     2 = "IUD"
     3 = "Injections (monthly)"
     4 = "Diaphragm"
     5 = "Condom"
     6 = "Female Sterilization"
     7 = "Male Sterilization"
     8 = "Periodic Abstinence"
     9 = "Withdrawal"
    10 = "Other"
    11 = "Norplant"
    12 = "Abstinence"
    13 = "Lactational amenorrhea"
    14 = "Female condom"
    15 = "Foam or jelly"
    17 = "Other modern method (patch)"
    19 = "Injection every three months"
    20 = "Vaginal ring"
    98 = "Don't know"
     ;
  value F00132_
     1 = "Using modern method"
     2 = "Using traditional method"
     3 = "Non-user - intends to use later"
     4 = "Does not intend to use"
     5 = "Never had sex"
     ;
  value F00133_
     1 = "Wanted then"
     2 = "Wanted later"
     3 = "Wanted no more"
     ;
  value F00134_
     0 = "Package not seen"
     1 = "Package seen"
     ;
  value F00135_
     0 = "Package not seen"
     1 = "Package seen"
     ;
  value F00136_
    11 = "Not married"
    20 = "FERTILITY-RELATED"
    21 = "Infrequent sex"
    22 = "Menopausal"
    23 = "Hysterectomy"
    24 = "Subfecund, infecund"
    25 = "Postpartum, breastfeeding"
    26 = "Wants more children"
    27 = "Pregnant"
    28 = "Not having sex with men"
    29 = "Has sex only with women"
    30 = "OPPOSITION TO USE"
    31 = "Respondent opposed"
    32 = "Husband opposed"
    33 = "Others opposed/religious prohibition"
    35 = "Parents opposed"
    40 = "LACK OF KNOWLEDGE"
    41 = "Knows no method"
    42 = "Knows no source"
    50 = "METHOD-RELATED"
    51 = "Health concerns"
    54 = "Side effects/inconvenient to use/Interfere with body"
    60 = "ACCESS/COST REASONS"
    61 = "Personal reasons"
    62 = "Service too far"
    63 = "Economic reasons"
    68 = "Too much paper work"
    71 = "Not affiliated to health insurance"
    73 = "Didn't know that could use service (RC/RS)"
    96 = "Other"
    98 = "DK"
     ;
  value F00137_
    11 = "Not married"
    20 = "FERTILITY-RELATED"
    21 = "Infrequent sex"
    22 = "Menopausal"
    23 = "Hysterectomy"
    24 = "Subfecund, infecund"
    25 = "Postpartum, breastfeeding"
    26 = "Wants more children"
    27 = "Pregnant"
    28 = "Not having sex with men"
    29 = "Has sex only with women"
    30 = "OPPOSITION TO USE"
    31 = "Respondent opposed"
    32 = "Husband opposed"
    33 = "Others opposed/religious prohibition"
    35 = "Parents opposed"
    40 = "LACK OF KNOWLEDGE"
    41 = "Knows no method"
    42 = "Knows no source"
    50 = "METHOD-RELATED"
    51 = "Health concerns"
    54 = "Side effects/inconvenient to use/Interfere with body"
    60 = "ACCESS/COST REASONS"
    61 = "Don't have time"
    62 = "Service too far"
    63 = "Transport too  expensive"
    68 = "Too much paper work"
    71 = "Not affiliated to health insurance"
    73 = "Didn't know that could use service (RC/RS)"
    96 = "Other"
    98 = "DK"
     ;
  value F00138_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00139_
    10 = "PUBLIC SECTOR"
    20 = "PRIVATE SECTOR"
    30 = "OTHER SOURCE"
    96 = "Other"
    98 = "Don't know"
     ;
  value F00140_
     1 = "Government clinic/pharmacy"
     2 = "Government home/community delivery"
     3 = "NGO"
     4 = "Private clinic/delivery"
     5 = "Pharmacy"
     6 = "Shop, church, friend"
     7 = "Other"
     8 = "Don't know"
     ;
  value F00141_
     0 = "No"
     1 = "Yes"
     ;
  value F00142_
     0 = "No"
     1 = "Yes"
     ;
  value F00143_
     0 = "No"
     1 = "Yes"
     ;
  value F00144_
     0 = "No"
     1 = "Yes"
     ;
  value F00145_
     0 = "No"
     1 = "Yes"
     ;
  value F00146_
     0 = "No"
     1 = "Yes"
     ;
  value F00147_
     0 = "No"
     1 = "Yes"
     ;
  value F00148_
     0 = "No"
     1 = "Yes"
     ;
  value F00149_
     0 = "No"
     1 = "Yes"
     ;
  value F00150_
     0 = "No"
     1 = "Yes"
     ;
  value F00151_
     0 = "No"
     1 = "Yes"
     ;
  value F00152_
     0 = "No"
     1 = "Yes"
     ;
  value F00153_
     0 = "No"
     1 = "Yes"
     ;
  value F00154_
     0 = "No"
     1 = "Yes"
     ;
  value F00155_
     0 = "No"
     1 = "Yes"
     ;
  value F00156_
     0 = "No"
     1 = "Yes"
     ;
  value F00157_
     0 = "No"
     1 = "Yes"
     ;
  value F00158_
     0 = "No"
     1 = "Yes"
     ;
  value F00159_
     0 = "No"
     1 = "Yes"
     ;
  value F00160_
     0 = "No"
     1 = "Yes"
     ;
  value F00161_
     0 = "No"
     1 = "Yes"
     ;
  value F00162_
     0 = "No"
     1 = "Yes"
     ;
  value F00163_
     0 = "No"
     1 = "Yes"
     ;
  value F00164_
     0 = "No"
     1 = "Yes"
     ;
  value F00165_
     0 = "No"
     1 = "Yes"
     ;
  value F00166_
     0 = "No"
     1 = "Yes"
     ;
  value F00167_
     0 = "No"
     1 = "Yes"
     ;
  value F00168_
     0 = "No"
     1 = "Yes"
     ;
  value F00169_
     0 = "No"
     1 = "Yes"
     ;
  value F00170_
     0 = "No"
     1 = "Yes"
     ;
  value F00171_
     0 = "No"
     1 = "Yes"
     ;
  value F00172_
     0 = "No"
     1 = "Yes"
     ;
  value F00173_
     0 = "No: knows a source"
     1 = "Yes: knows no source"
     ;
  value F00174_
     0 = "No"
     1 = "Yes"
     ;
  value F00175_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00176_
     0 = "No"
     1 = "Yes"
     ;
  value F00177_
     0 = "No"
     1 = "Yes"
     ;
  value F00178_
     0 = "No"
     1 = "Yes"
     ;
  value F00179_
     0 = "No"
     1 = "Yes"
     ;
  value F00180_
     0 = "No"
     1 = "Yes"
     ;
  value F00181_
    10 = "PUBLIC"
    11 = "Government hospital/health center/health post"
    20 = "PRIVATE MEDICAL"
    21 = "Private hospital/clinic"
    22 = "EPS health center"
    23 = "Private doctor"
    24 = "PROFAMILIA"
    25 = "Drugstore/pharmacy"
    30 = "OTHER PRIVATE"
    31 = "Supermarket/shop"
    96 = "OTHER"
    98 = "DK"
     ;
  value F00182_
     0 = "No"
     1 = "Yes"
     ;
  value F00183_
     0 = "No"
     1 = "Yes"
     ;
  value F00184_
     0 = "No"
     1 = "Yes"
     ;
  value F00185_
     0 = "No"
     1 = "Yes"
     ;
  value F00186_
     0 = "No"
     1 = "Yes"
     ;
  value F00187_
     0 = "No"
     1 = "Yes"
     ;
  value F00188_
     0 = "No"
     1 = "Yes"
     ;
  value F00189_
     0 = "No"
     1 = "Yes"
     ;
  value F00190_
     0 = "No"
     1 = "Yes"
     ;
  value F00191_
     0 = "No"
     1 = "Yes"
     ;
  value F00192_
     0 = "No"
     1 = "Yes"
     ;
  value F00193_
     0 = "No"
     1 = "Yes"
     ;
  value F00194_
     0 = "No"
     1 = "Yes"
     ;
  value F00195_
     0 = "No"
     1 = "Yes"
     ;
  value F00196_
     0 = "No"
     1 = "Yes"
     ;
  value F00197_
     0 = "No"
     1 = "Yes"
     ;
  value F00198_
     0 = "No"
     1 = "Yes"
     ;
  value F00199_
     0 = "No"
     1 = "Yes"
     ;
  value F00200_
     0 = "No"
     1 = "Yes"
     ;
  value F00201_
     0 = "No"
     1 = "Yes"
     ;
  value F00202_
     0 = "No"
     1 = "Yes"
     ;
  value F00203_
     0 = "No"
     1 = "Yes"
     ;
  value F00204_
     0 = "No"
     1 = "Yes"
     ;
  value F00205_
     0 = "No"
     1 = "Yes"
     ;
  value F00206_
     0 = "No"
     1 = "Yes"
     ;
  value F00207_
     0 = "No"
     1 = "Yes"
     ;
  value F00208_
     0 = "No"
     1 = "Yes"
     ;
  value F00209_
     0 = "No"
     1 = "Yes"
     ;
  value F00210_
     0 = "No"
     1 = "Yes: don't know"
     ;
  value F00211_
   998 = "Don't know"
     ;
  value F00212_
   998 = "Don't know"
     ;
  value F00213_
     0 = "Received no injection"
     7 = "7+"
     8 = "Don't know"
     ;
  value F00214_
     0 = "Received no injection"
     7 = "7+"
     8 = "Don't know"
     ;
  value F00215_
    97 = "Inconsistent"
    98 = "Don't know month"
     ;
  value F00216_
  9997 = "Inconsistent"
  9998 = "Don't know year"
     ;
  value F00217_
  9996 = "Can't be determined"
     ;
  value F00218_
     0 = "No"
     1 = "Yes"
     ;
  value F00219_
     0 = "No"
     1 = "Yes"
     ;
  value F00220_
     0 = "No"
     1 = "Yes"
     ;
  value F00221_
     0 = "No"
     1 = "Yes"
     ;
  value F00222_
     0 = "No"
     1 = "Yes"
     ;
  value F00223_
     0 = "No"
     1 = "Yes"
     ;
  value F00224_
     0 = "No"
     1 = "Yes"
     ;
  value F00225_
     0 = "No"
     1 = "Yes"
     ;
  value F00226_
     0 = "No"
     1 = "Yes"
     ;
  value F00227_
     0 = "No"
     1 = "Yes"
     ;
  value F00228_
     0 = "No"
     1 = "Yes"
     ;
  value F00229_
     0 = "No"
     1 = "Yes"
     ;
  value F00230_
     0 = "No"
     1 = "Yes"
     ;
  value F00231_
     0 = "No: some care"
     1 = "Yes: no care"
     ;
  value F00232_
     0 = "No"
     1 = "Yes"
     ;
  value F00233_
     0 = "No"
     1 = "Yes"
     ;
  value F00234_
     0 = "No"
     1 = "Yes"
     ;
  value F00235_
     0 = "No"
     1 = "Yes"
     ;
  value F00236_
     0 = "No"
     1 = "Yes"
     ;
  value F00237_
     0 = "No"
     1 = "Yes"
     ;
  value F00238_
     0 = "No"
     1 = "Yes"
     ;
  value F00239_
     0 = "No"
     1 = "Yes"
     ;
  value F00240_
     0 = "No"
     1 = "Yes"
     ;
  value F00241_
     0 = "No"
     1 = "Yes"
     ;
  value F00242_
     0 = "No"
     1 = "Yes"
     ;
  value F00243_
     0 = "No"
     1 = "Yes"
     ;
  value F00244_
     0 = "No"
     1 = "Yes"
     ;
  value F00245_
     0 = "No: some assistance"
     1 = "Yes: no assistance"
     ;
  value F00246_
    93 = "Ever breastfed, not currently breastfeeding"
    94 = "Never breastfed"
    95 = "Still breastfeeding"
    96 = "Breastfed until died"
    97 = "Inconsistent"
    98 = "Don't know"
     ;
  value F00247_
    93 = "Ever breastfed, not currently breastfeeding"
    94 = "Never breastfed"
    97 = "Inconsistent"
    98 = "Don't know"
     ;
  value F00248_
    96 = "Period not returned"
    97 = "Inconsistent"
    98 = "Don't know"
     ;
  value F00249_
    97 = "Inconsistent"
    98 = "Don't know"
     ;
  value F00250_
    96 = "Still abstaining"
    97 = "Inconsistent"
    98 = "Don't know"
     ;
  value F00251_
    97 = "Inconsistent"
    98 = "Don't know"
     ;
  value F00252_
     1 = "Then"
     2 = "Later"
     3 = "No more"
     ;
  value F00253_
   101 = "Months: 1"
   199 = "Months: number missing"
   201 = "Years: 1"
   299 = "Years: number missing"
   998 = "Don't know"
     ;
  value F00254_
    98 = "Don't know"
     ;
  value F00255_
     0 = "No antenatal visits"
    98 = "Don't know"
     ;
  value F00256_
    10 = "HOME"
    11 = "Respondent/other's home"
    20 = "PUBLIC SECTOR"
    21 = "Government hospital/health center/health post"
    30 = "PRIVATE SECTOR"
    31 = "Private hospital/clinic"
    32 = "EPS health center"
    96 = "Other"
     ;
  value F00257_
     0 = "No"
     1 = "Yes"
     ;
  value F00258_
     1 = "Before labor started"
     2 = "After labor started"
     ;
  value F00259_
     1 = "Very large"
     2 = "Larger than average"
     3 = "Average"
     4 = "Smaller than average"
     5 = "Very small"
     8 = "Don't know"
     ;
  value F00260_
  9996 = "Not weighed at birth"
  9998 = "Don't know"
     ;
  value F00261_
     0 = "Not weighed"
     1 = "From written card"
     2 = "From mother's recall"
     8 = "Don't know"
     9 = "Special answers"
     ;
  value F00262_
     0 = "No flag"
     1 = "> interval"
     2 = "> interval by 1 month"
     3 = "> age at death"
     4 = "During pregnancy in calendar"
     ;
  value F00263_
     0 = "No flag"
     1 = "> interval"
     2 = "> interval by 1 month"
     4 = "During pregnancy in calendar"
     ;
  value F00264_
     0 = "No flag"
     1 = "> interval"
     2 = "> interval by 1 month"
     4 = "During pregnancy in calendar"
     ;
  value F00265_
     0 = "Immediately"
   100 = "Within first hour"
   101 = "Hours: 1"
   199 = "Hours: number missing"
   201 = "Days: 1"
   299 = "Days: number missing"
     ;
  value F00266_
    96 = "On demand"
     ;
  value F00267_
    96 = "On demand"
     ;
  value F00268_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00269_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00270_
     0 = "None"
     7 = "7+"
     8 = "Don't know"
     ;
  value F00271_
     0 = "No"
     1 = "Yes"
     ;
  value F00272_
     0 = "No"
     1 = "Yes"
     ;
  value F00273_
     0 = "No"
     1 = "Yes"
     ;
  value F00274_
     0 = "No"
     1 = "Yes"
     ;
  value F00275_
     0 = "No"
     1 = "Yes"
     ;
  value F00276_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00277_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00278_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00279_
     0 = "Were not taken"
   998 = "Don't know"
     ;
  value F00280_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00281_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00282_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00283_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00284_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00285_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00286_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00287_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00288_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00289_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00290_
     0 = "No"
     1 = "Yes: don't know which drug taken"
     8 = "Don't know"
     ;
  value F00291_
     0 = "No"
     1 = "Yes, took no drug"
     8 = "Don't know"
     ;
  value F00292_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00293_
     0 = "No"
     1 = "Yes"
     ;
  value F00294_
     0 = "No"
     1 = "Yes"
     ;
  value F00295_
     0 = "No"
     1 = "Yes"
     ;
  value F00296_
     0 = "No"
     1 = "Yes"
     ;
  value F00297_
     0 = "No"
     1 = "Yes"
     ;
  value F00298_
     0 = "No"
     1 = "Yes"
     ;
  value F00299_
     0 = "No"
     1 = "Yes"
     ;
  value F00300_
     0 = "No"
     1 = "Yes"
     ;
  value F00301_
     0 = "No"
     1 = "Yes"
     ;
  value F00302_
     0 = "No"
     1 = "Yes"
     ;
  value F00303_
     0 = "No"
     1 = "Yes"
     ;
  value F00304_
     0 = "No"
     1 = "Yes"
     ;
  value F00305_
     0 = "No"
     1 = "Yes"
     ;
  value F00306_
     0 = "No"
     1 = "Yes"
     ;
  value F00307_
     0 = "No"
     1 = "Yes"
     ;
  value F00308_
     0 = "No"
     1 = "Yes"
     ;
  value F00309_
     0 = "No"
     1 = "Yes"
     ;
  value F00310_
     0 = "No: given something"
     1 = "Yes: given nothing"
     ;
  value F00311_
     0 = "No"
     1 = "Yes"
     ;
  value F00312_
     0 = "No"
     1 = "Yes"
     ;
  value F00313_
     0 = "No"
     1 = "Yes"
     ;
  value F00314_
     0 = "No"
     1 = "Yes"
     ;
  value F00315_
     0 = "No"
     1 = "Yes"
     ;
  value F00316_
     0 = "No"
     1 = "Yes"
     ;
  value F00317_
     0 = "No"
     1 = "Yes"
     ;
  value F00318_
     0 = "No"
     1 = "Yes"
     ;
  value F00319_
     0 = "No"
     1 = "Yes"
     ;
  value F00320_
     0 = "No"
     1 = "Yes"
     ;
  value F00321_
     0 = "No"
     1 = "Yes"
     ;
  value F00322_
     0 = "No"
     1 = "Yes"
     ;
  value F00323_
     0 = "No"
     1 = "Yes"
     ;
  value F00324_
     0 = "No"
     1 = "Yes"
     ;
  value F00325_
     0 = "No"
     1 = "Yes"
     ;
  value F00326_
     0 = "No"
     1 = "Yes"
     ;
  value F00327_
     0 = "No"
     1 = "Yes"
     ;
  value F00328_
     0 = "No"
     1 = "Yes"
     ;
  value F00329_
     0 = "No"
     1 = "Yes"
     ;
  value F00330_
     0 = "No"
     1 = "Yes"
     ;
  value F00331_
     0 = "No"
     1 = "Yes"
     ;
  value F00332_
     0 = "No"
     1 = "Yes"
     ;
  value F00333_
     0 = "No"
     1 = "Yes"
     ;
  value F00334_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00335_
   100 = "Hours: 0"
   101 = "Hours: 1"
   199 = "Hours: number missing"
   201 = "Days: 1"
   299 = "Days: number missing"
   301 = "Weeks: 1"
   399 = "Weeks: number missing"
   998 = "Don't know"
     ;
  value F00336_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00337_
   100 = "Zero hours"
   201 = "One day"
   301 = "One week"
   998 = "DK "
     ;
  value F00338_
    10 = "HEALTH PERSONNEL"
    11 = "Doctor"
    12 = "Nurse, midwife"
    13 = "Auxiliary midwife"
    20 = "OTHER PERSON"
    21 = "Traditional birth attendant"
    22 = "Community/village health worker"
    96 = "Other"
     ;
  value F00339_
     0 = "No"
     1 = "Yes"
     ;
  value F00340_
     0 = "No"
     1 = "Yes"
     ;
  value F00341_
     0 = "No"
     1 = "Yes"
     ;
  value F00342_
     0 = "No"
     1 = "Yes"
     ;
  value F00343_
     0 = "No"
     1 = "Yes"
     ;
  value F00344_
     0 = "No"
     1 = "Yes"
     ;
  value F00345_
     0 = "No"
     1 = "Yes"
     ;
  value F00346_
     0 = "No"
     1 = "Yes"
     ;
  value F00347_
     0 = "No"
     1 = "Yes"
     ;
  value F00348_
     0 = "No"
     1 = "Yes"
     ;
  value F00349_
     0 = "No"
     1 = "Yes"
     ;
  value F00350_
     0 = "No"
     1 = "Yes"
     ;
  value F00351_
     0 = "No"
     1 = "Yes"
     ;
  value F00352_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00353_
   100 = "Zero hours"
   200 = "Zero days"
   201 = "One day"
   301 = "One week"
   998 = "DK "
     ;
  value F00354_
    10 = "HEALTH PERSONNEL"
    11 = "Doctor"
    12 = "Nurse, midwife"
    13 = "Auxiliary midwife"
    20 = "OTHER PERSON"
    21 = "Traditional birth attendant"
    22 = "Community/village health worker"
    96 = "Other"
     ;
  value F00355_
    10 = "HOME"
    11 = "Respondent's home"
    12 = "Other home"
    20 = "PUBLIC SECTOR"
    21 = "Government hospital"
    30 = "PRIVATE MEDICAL SECTOR"
    31 = "Private hospital, clinic"
    32 = "EPS health center"
    33 = "Private doctor"
    34 = "PROFAMILIA"
    96 = "Other"
     ;
  value F00356_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00357_
   100 = "Immediately/in first hour"
   101 = "Hours: 1"
   199 = "Hours: number missing"
   200 = "Zero days"
   201 = "Days: 1"
   299 = "Days: number missing"
   301 = "Weeks: 1"
   399 = "Weeks: number missing"
   998 = "Don't know"
     ;
  value F00358_
    10 = "MEDICAL PERSONNEL"
    11 = "Doctor"
    12 = "Nurse, midwife"
    13 = "Auxiliary midwife"
    20 = "OTHER PERSON"
    21 = "Traditional birth attendant"
    22 = "Community/village health worker"
    96 = "Other"
     ;
  value F00359_
    10 = "HOME"
    11 = "Respondent's home"
    12 = "Other home"
    20 = "PUBLIC SECTOR"
    21 = "Government hospital"
    22 = "Government health center"
    23 = "Government health post"
    26 = "Other public sector"
    30 = "PRIVATE SECTOR"
    31 = "Private hospital, clinic"
    36 = "Other private sector"
    96 = "Other"
     ;
  value F00360_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00361_
   100 = "Zero hours"
   201 = "One day"
   301 = "One week"
   998 = "DK "
     ;
  value F00362_
    10 = "HEALTH PERSONNEL"
    11 = "Doctor"
    12 = "Nurse, midwife"
    13 = "Auxiliary midwife"
    20 = "OTHER PERSON"
    21 = "Traditional birth attendant"
    22 = "Community/village health worker"
    96 = "Other"
     ;
  value F00363_
     0 = "No"
     1 = "Put on chest, touching bare skin"
     2 = "Put on chest, no touching of bare skin"
     3 = "Put on chest, DK/Missing on touching of bare skin"
     8 = "Don't know"
     ;
  value F00364_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00365_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00366_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00367_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00368_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00369_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00370_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00371_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00372_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00373_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00374_
     0 = "No"
     1 = "Yes"
     ;
  value F00375_
     0 = "No"
     1 = "Yes"
     ;
  value F00376_
     0 = "No"
     1 = "Yes"
     ;
  value F00377_
     0 = "No"
     1 = "Yes"
     ;
  value F00378_
    96 = "On demand"
     ;
  value F00379_
    96 = "On demand"
     ;
  value F00380_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00381_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00382_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00383_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00384_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00385_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00386_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00387_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00388_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00389_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00390_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00391_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00392_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00393_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00394_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00395_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00396_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00397_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00398_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00399_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00400_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00401_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00402_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00403_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00404_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00405_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00406_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00407_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00408_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00409_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00410_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00411_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00412_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00413_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00414_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00415_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00416_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00417_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00418_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00419_
     0 = "Never heard of"
     1 = "Used ORS"
     2 = "Heard of ORS"
     ;
  value F00420_
     0 = "Immediately"
   100 = "Within first hour"
   101 = "Hours: 1"
   199 = "Hours: number missing"
   201 = "Days: 1"
   299 = "Days: number missing"
     ;
  value F00421_
  9994 = "Not present"
  9995 = "Refused"
  9996 = "Other"
     ;
  value F00422_
  9994 = "Not present"
  9995 = "Refused"
  9996 = "Other"
     ;
  value F00423_
  9998 = "Flagged cases"
     ;
  value F00424_
  9998 = "Flagged cases"
     ;
  value F00425_
 99998 = "Flagged cases"
     ;
  value F00426_
 99998 = "Flagged cases"
     ;
  value F00427_
 99998 = "Flagged cases"
     ;
  value F00428_
 99998 = "Flagged cases"
     ;
  value F00429_
  9998 = "Flagged cases"
     ;
  value F00430_
  9998 = "Flagged cases"
     ;
  value F00431_
  9998 = "Flagged cases"
     ;
  value F00432_
     0 = "Measured"
     3 = "Not present"
     4 = "Refused"
     6 = "Other"
     7 = "No measurement found in household"
     ;
  value F00433_
     1 = "Under age 18"
     2 = "Age 18 or older"
     ;
  value F00434_
     0 = "Not in household"
     ;
  value F00435_
     1 = "Granted"
     2 = "Parent/other refused"
     3 = "Respondent refused"
     ;
  value F00436_
     0 = "No/don't know"
     1 = "Yes"
     ;
  value F00437_
     0 = "Measured"
     3 = "Not present"
     4 = "Refused"
     6 = "Other"
     7 = "No measurement found in household"
     ;
  value F00438_
     1 = "Severe"
     2 = "Moderate"
     3 = "Mild"
     4 = "Not anemic"
     ;
  value F00439_
     0 = "No"
     1 = "Yes"
     ;
  value F00440_
     0 = "No"
     1 = "Yes"
     ;
  value F00441_
     0 = "No"
     1 = "All children"
     2 = "Some children"
     3 = "No net in household"
     ;
  value F00442_
     0 = "No"
     1 = "Yes"
     ;
  value F00443_
     0 = "No"
     1 = "Yes"
     2 = "Never prepared meal"
     ;
  value F00444_
     0 = "No"
     1 = "Yes"
     ;
  value F00445_
     0 = "No"
     1 = "Yes"
     ;
  value F00446_
     0 = "No"
     1 = "Yes"
     ;
  value F00447_
     0 = "No"
     1 = "Yes"
     ;
  value F00448_
     0 = "No"
     1 = "Yes"
     ;
  value F00449_
     0 = "No"
     1 = "Yes"
     ;
  value F00450_
     0 = "No"
     1 = "Yes"
     ;
  value F00451_
     0 = "No"
     1 = "Yes"
     ;
  value F00452_
     0 = "No"
     1 = "Yes"
     ;
  value F00453_
     0 = "No"
     1 = "Yes"
     ;
  value F00454_
     0 = "No"
     1 = "Yes"
     ;
  value F00455_
     0 = "No"
     1 = "Yes"
     ;
  value F00456_
     0 = "No"
     1 = "Yes"
     ;
  value F00457_
     0 = "No"
     1 = "Yes, smokes nothing"
     ;
  value F00458_
     0 = "Do not smokes"
     1 = "Every day"
     2 = "Some days"
     ;
  value F00459_
     0 = "Do not smokes"
     1 = "Every day"
     2 = "Some days"
     ;
  value F00460_
    80 = "80+"
    98 = "Don't know"
     ;
  value F00461_
     1 = "Used toilet/latrine"
     2 = "Put/rinsed in toilet/latrine"
     3 = "Put/rinsed into drain or ditch"
     4 = "Throw into garbage"
     5 = "Buried"
     6 = "Rinse away"
     7 = "Use disposable diapers"
     8 = "Use washable diapers"
     9 = "Left in the open/not disposed of"
    96 = "Other"
     ;
  value F00462_
     0 = "No"
     1 = "Yes"
     2 = "Depends"
     ;
  value F00463_
     0 = "No problem"
     1 = "Big problem"
     2 = "Not a big problem"
     ;
  value F00464_
     0 = "No problem"
     1 = "Big problem"
     2 = "Not a big problem"
     ;
  value F00465_
     0 = "No problem"
     1 = "Big problem"
     2 = "Not a big problem"
     ;
  value F00466_
     0 = "No problem"
     1 = "Big problem"
     2 = "Not a big problem"
     ;
  value F00467_
     0 = "No problem"
     1 = "Big problem"
     2 = "Not a big problem"
     ;
  value F00468_
     0 = "No problem"
     1 = "Big problem"
     2 = "Not a big problem"
     ;
  value F00469_
     0 = "No problem"
     1 = "Big problem"
     2 = "Not a big problem"
     ;
  value F00470_
     0 = "No problem"
     1 = "Big problem"
     2 = "Not a big problem"
     ;
  value F00471_
     0 = "No problem"
     1 = "Big problem"
     2 = "Not a big problem"
     ;
  value F00472_
     0 = "No problem"
     1 = "Big problem"
     2 = "Not a big problem"
     ;
  value F00473_
     0 = "No problem"
     1 = "Big problem"
     2 = "Not a big problem"
     ;
  value F00474_
     0 = "No problem"
     1 = "Big problem"
     2 = "Not a big problem"
     ;
  value F00475_
     0 = "No problem"
     1 = "Big problem"
     2 = "Not a big problem"
     ;
  value F00476_
     0 = "See specific questions"
     1 = "Last birth only"
     2 = "All births in last 5 years"
     ;
  value F00477_
     0 = "No"
     7 = "7+"
     8 = "DK"
     ;
  value F00478_
     0 = "No"
     7 = "7+"
     8 = "DK"
     ;
  value F00479_
     0 = "No"
     7 = "7+"
     8 = "DK"
     ;
  value F00480_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00481_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00482_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00483_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00484_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00485_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00486_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00487_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00488_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00489_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00490_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00491_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00492_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00493_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00494_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00495_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00496_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00497_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00498_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00499_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00500_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00501_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00502_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00503_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00504_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00505_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00506_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00507_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00508_
     1 = "Granted"
     2 = "Parent/other responsible refused"
     3 = "Respondent refused"
     4 = "Sample not tested/lost/not enough DBS"
     ;
  value F00509_
     1 = "Blood taken"
     2 = "Not present"
     3 = "Refused"
     4 = "Sample not tested/lost/damaged/insufficient"
     5 = "Not enough DBS to complete protocol"
     6 = "Other"
     ;
  value F00510_
     0 = "No"
     1 = "Yes"
     ;
  value F00511_
     0 = "No"
     1 = "Yes"
     ;
  value F00512_
     0 = "No"
     1 = "Yes"
     ;
  value F00513_
     0 = "No"
     1 = "Yes"
     ;
  value F00514_
     0 = "No"
     1 = "Yes"
     ;
  value F00515_
     0 = "No"
     1 = "Yes"
     ;
  value F00516_
     0 = "No"
     1 = "Yes"
     ;
  value F00517_
     0 = "No"
     1 = "Yes"
     ;
  value F00518_
     0 = "No"
     1 = "Yes"
     ;
  value F00519_
     0 = "No"
     1 = "Yes"
     ;
  value F00520_
     0 = "No"
     1 = "Yes"
     ;
  value F00521_
     0 = "No"
     1 = "Yes"
     ;
  value F00522_
     0 = "No"
     1 = "Yes"
     ;
  value F00523_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00524_
     0 = "No"
     1 = "Yes, remain a secret"
     8 = "Don't know/not sure/depends"
     ;
  value F00525_
     0 = "None"
    90 = "90+"
    98 = "Don't know"
     ;
  value F00526_
     0 = "None"
    90 = "90+"
    98 = "Don't know"
     ;
  value F00527_
    10 = "PUBLIC SECTOR"
    11 = "Government hospital"
    12 = "Government health center"
    16 = "Other public sector"
    20 = "PRIVATE SECTOR"
    21 = "Private hospital/clinic/doctor"
    22 = "Dental clinic/office"
    23 = "Pharmacy"
    24 = "Office or home of nurse/health worker"
    26 = "Other private sector"
    30 = "OTHER NON-MEDICAL"
    31 = "Respondent's home"
    96 = "Other"
     ;
  value F00528_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00529_
     0 = "No"
     1 = "Yes"
     ;
  value F00530_
     0 = "No"
     1 = "Yes"
     ;
  value F00531_
     0 = "No"
     1 = "Yes"
     ;
  value F00532_
     0 = "No"
     1 = "Yes"
     ;
  value F00533_
     0 = "No"
     1 = "Yes"
     ;
  value F00534_
     0 = "No"
     1 = "Yes"
     ;
  value F00535_
     0 = "No"
     1 = "Yes"
     ;
  value F00536_
     0 = "No"
     1 = "Yes"
     ;
  value F00537_
     0 = "No"
     1 = "Yes"
     ;
  value F00538_
     0 = "No"
     1 = "Yes"
     ;
  value F00539_
     0 = "No"
     1 = "Yes"
     8 = "Unsure"
     ;
  value F00540_
     0 = "No"
     1 = "Yes"
     ;
  value F00541_
     0 = "No"
     1 = "Yes"
     8 = "Unsure"
     ;
  value F00542_
     0 = "No card"
     1 = "Yes, seen"
     2 = "Yes, not seen"
     3 = "No longer has card"
     ;
  value F00543_
     0 = "Does not have either card or other document"
     1 = "Has only health card and was seen"
     2 = "Has only health card and wasn't seen"
     3 = "Has only other document and was seen"
     4 = "Has only other document and wasn't seen"
     5 = "Has card/other document but only card was seen"
     6 = "Has card/other document but only other document was seen"
     7 = "Has card/other document and both were seen"
     8 = "Has card/other document and none were seen"
     ;
  value F00544_
     0 = "No"
     1 = "Vaccination date on card"
     2 = "Reported by mother"
     3 = "Vaccination marked on card"
     8 = "Don't know"
     ;
  value F00545_
    97 = "Inconsistent"
    98 = "Don't know"
     ;
  value F00546_
    97 = "Inconsistent"
    98 = "Don't know"
     ;
  value F00547_
  9997 = "Inconsistent"
  9998 = "Don't know"
     ;
  value F00548_
     0 = "No"
     1 = "Vaccination date on card"
     2 = "Reported by mother"
     3 = "Vaccination marked on card"
     8 = "Don't know"
     ;
  value F00549_
    97 = "Inconsistent"
    98 = "Don't know"
     ;
  value F00550_
    97 = "Inconsistent"
    98 = "Don't know"
     ;
  value F00551_
  9997 = "Inconsistent"
  9998 = "Don't know"
     ;
  value F00552_
     0 = "No"
     1 = "Vaccination date on card"
     2 = "Reported by mother"
     3 = "Vaccination marked on card"
     8 = "Don't know"
     ;
  value F00553_
    97 = "Inconsistent"
    98 = "Don't know"
     ;
  value F00554_
    97 = "Inconsistent"
    98 = "Don't know"
     ;
  value F00555_
  9997 = "Inconsistent"
  9998 = "Don't know"
     ;
  value F00556_
     0 = "No"
     1 = "Vaccination date on card"
     2 = "Reported by mother"
     3 = "Vaccination marked on card"
     8 = "Don't know"
     ;
  value F00557_
    97 = "Inconsistent"
    98 = "Don't know"
     ;
  value F00558_
    97 = "Inconsistent"
    98 = "Don't know"
     ;
  value F00559_
  9997 = "Inconsistent"
  9998 = "Don't know"
     ;
  value F00560_
     0 = "No"
     1 = "Vaccination date on card"
     2 = "Reported by mother"
     3 = "Vaccination marked on card"
     8 = "Don't know"
     ;
  value F00561_
    97 = "Inconsistent"
    98 = "Don't know"
     ;
  value F00562_
    97 = "Inconsistent"
    98 = "Don't know"
     ;
  value F00563_
  9997 = "Inconsistent"
  9998 = "Don't know"
     ;
  value F00564_
     0 = "No"
     1 = "Vaccination date on card"
     2 = "Reported by mother"
     3 = "Vaccination marked on card"
     8 = "Don't know"
     ;
  value F00565_
    97 = "Inconsistent"
    98 = "Don't know"
     ;
  value F00566_
    97 = "Inconsistent"
    98 = "Don't know"
     ;
  value F00567_
  9997 = "Inconsistent"
  9998 = "Don't know"
     ;
  value F00568_
     0 = "No"
     1 = "Vaccination date on card"
     2 = "Reported by mother"
     3 = "Vaccination marked on card"
     8 = "Don't know"
     ;
  value F00569_
    97 = "Inconsistent"
    98 = "Don't know"
     ;
  value F00570_
    97 = "Inconsistent"
    98 = "Don't know"
     ;
  value F00571_
  9997 = "Inconsistent"
  9998 = "Don't know"
     ;
  value F00572_
     0 = "No"
     1 = "Vaccination date on card"
     2 = "Reported by mother"
     3 = "Vaccination marked on card"
     8 = "Don't know"
     ;
  value F00573_
    97 = "Inconsistent"
    98 = "Don't know"
     ;
  value F00574_
    97 = "Inconsistent"
    98 = "Don't know"
     ;
  value F00575_
  9997 = "Inconsistent"
  9998 = "Don't know"
     ;
  value F00576_
     0 = "No"
     1 = "Vaccination date on card"
     2 = "Reported by mother"
     3 = "Vaccination marked on card"
     8 = "Don't know"
     ;
  value F00577_
    97 = "Inconsistent"
    98 = "Don't know"
     ;
  value F00578_
    97 = "Inconsistent"
    98 = "Don't know"
     ;
  value F00579_
  9997 = "Inconsistent"
  9998 = "Don't know"
     ;
  value F00580_
     0 = "No"
     1 = "Vaccination date on card"
     2 = "Reported by mother"
     3 = "Vaccination marked on card"
     8 = "Don't know"
     ;
  value F00581_
    97 = "Inconsistent"
    98 = "Don't know"
     ;
  value F00582_
    97 = "Inconsistent"
    98 = "Don't know"
     ;
  value F00583_
  9997 = "Inconsistent"
  9998 = "Don't know"
     ;
  value F00584_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00585_
     0 = "No"
     1 = "Vaccination date on card"
     2 = "Reported by mother"
     3 = "Vaccination marked on card"
     8 = "Don't know"
     ;
  value F00586_
    97 = "Inconsistent"
    98 = "Don't know"
     ;
  value F00587_
    97 = "Inconsistent"
    98 = "Don't know"
     ;
  value F00588_
  9997 = "Inconsistent"
  9998 = "Don't know"
     ;
  value F00589_
     0 = "No"
     1 = "Yes"
     2 = "No vaccination in last 2 years"
     8 = "Don't know"
     ;
  value F00590_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00591_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00592_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00593_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00594_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00595_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00596_
     0 = "No"
     1 = "Vaccination date on card"
     2 = "Reported by mother"
     3 = "Vaccination marked on card"
     8 = "Don't know"
     ;
  value F00597_
    97 = "Inconsistent"
    98 = "Don't know"
     ;
  value F00598_
    97 = "Inconsistent"
    98 = "Don't know"
     ;
  value F00599_
  9997 = "Inconsistent"
  9998 = "Don't know"
     ;
  value F00600_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00601_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00602_
     0 = "No"
     1 = "Vaccination date on card"
     2 = "Reported by mother"
     3 = "Vaccination marked on card"
     8 = "Don't know"
     ;
  value F00603_
    97 = "Inconsistent"
    98 = "Don't know"
     ;
  value F00604_
    97 = "Inconsistent"
    98 = "Don't know"
     ;
  value F00605_
  9997 = "Inconsistent"
  9998 = "Don't know"
     ;
  value F00606_
     0 = "No"
     1 = "Vaccination date on card"
     2 = "Reported by mother"
     3 = "Vaccination marked on card"
     8 = "Don't know"
     ;
  value F00607_
    97 = "Inconsistent"
    98 = "Don't know"
     ;
  value F00608_
    97 = "Inconsistent"
    98 = "Don't know"
     ;
  value F00609_
  9997 = "Inconsistent"
  9998 = "Don't know"
     ;
  value F00610_
     0 = "No"
     1 = "Vaccination date on card"
     2 = "Reported by mother"
     3 = "Vaccination marked on card"
     8 = "Don't know"
     ;
  value F00611_
    97 = "Inconsistent"
    98 = "Don't know"
     ;
  value F00612_
    97 = "Inconsistent"
    98 = "Don't know"
     ;
  value F00613_
  9997 = "Inconsistent"
  9998 = "Don't know"
     ;
  value F00614_
     0 = "No"
     1 = "Vaccination date on card"
     2 = "Reported by mother"
     3 = "Vaccination marked on card"
     8 = "Don't know"
     ;
  value F00615_
    97 = "Inconsistent"
    98 = "Don't know"
     ;
  value F00616_
    97 = "Inconsistent"
    98 = "Don't know"
     ;
  value F00617_
  9997 = "Inconsistent"
  9998 = "Don't know"
     ;
  value F00618_
     0 = "No"
     1 = "Vaccination date on card"
     2 = "Reported by mother"
     3 = "Vaccination marked on card"
     8 = "Don't know"
     ;
  value F00619_
    97 = "Inconsistent"
    98 = "Don't know"
     ;
  value F00620_
    97 = "Inconsistent"
    98 = "Don't know"
     ;
  value F00621_
  9997 = "Inconsistent"
  9998 = "Don't know"
     ;
  value F00622_
     0 = "No"
     1 = "Vaccination date on card"
     2 = "Reported by mother"
     3 = "Vaccination marked on card"
     8 = "Don't know"
     ;
  value F00623_
    97 = "Inconsistent"
    98 = "Don't know"
     ;
  value F00624_
    97 = "Inconsistent"
    98 = "Don't know"
     ;
  value F00625_
  9997 = "Inconsistent"
  9998 = "Don't know"
     ;
  value F00626_
     0 = "No"
     1 = "Vaccination date on card"
     2 = "Reported by mother"
     3 = "Vaccination marked on card"
     8 = "Don't know"
     ;
  value F00627_
    97 = "Inconsistent"
    98 = "Don't know"
     ;
  value F00628_
    97 = "Inconsistent"
    98 = "Don't know"
     ;
  value F00629_
  9997 = "Inconsistent"
  9998 = "Don't know"
     ;
  value F00630_
     0 = "No"
     1 = "Vaccination date on card"
     2 = "Reported by mother"
     3 = "Vaccination marked on card"
     8 = "Don't know"
     ;
  value F00631_
    97 = "Inconsistent"
    98 = "Don't know"
     ;
  value F00632_
    97 = "Inconsistent"
    98 = "Don't know"
     ;
  value F00633_
  9997 = "Inconsistent"
  9998 = "Don't know"
     ;
  value F00634_
     0 = "No"
     1 = "Vaccination date on card"
     2 = "Reported by mother"
     3 = "Vaccination marked on card"
     8 = "Don't know"
     ;
  value F00635_
    97 = "Inconsistent"
    98 = "Don't know"
     ;
  value F00636_
    97 = "Inconsistent"
    98 = "Don't know"
     ;
  value F00637_
  9997 = "Inconsistent"
  9998 = "Don't know"
     ;
  value F00638_
     0 = "No"
     1 = "Vaccination date on card"
     2 = "Reported by mother"
     3 = "Vaccination marked on card"
     8 = "Don't know"
     ;
  value F00639_
    97 = "Inconsistent"
    98 = "Don't know"
     ;
  value F00640_
    97 = "Inconsistent"
    98 = "Don't know"
     ;
  value F00641_
  9997 = "Inconsistent"
  9998 = "Don't know"
     ;
  value F00642_
     0 = "No"
     1 = "Vaccination date on card"
     2 = "Reported by mother"
     3 = "Vaccination marked on card"
     8 = "Don't know"
     ;
  value F00643_
    97 = "Inconsistent"
    98 = "Don't know"
     ;
  value F00644_
    97 = "Inconsistent"
    98 = "Don't know"
     ;
  value F00645_
  9997 = "Inconsistent"
  9998 = "Don't know"
     ;
  value F00646_
     0 = "No"
     1 = "Vaccination date on card"
     2 = "Reported by mother"
     3 = "Vaccination marked on card"
     8 = "Don't know"
     ;
  value F00647_
    97 = "Inconsistent"
    98 = "Don't know"
     ;
  value F00648_
    97 = "Inconsistent"
    98 = "Don't know"
     ;
  value F00649_
  9997 = "Inconsistent"
  9998 = "Don't know"
     ;
  value F00650_
     0 = "No"
     1 = "Vaccination date on card"
     2 = "Reported by mother"
     3 = "Vaccination marked on card"
     8 = "Don't know"
     ;
  value F00651_
    97 = "Inconsistent"
    98 = "Don't know"
     ;
  value F00652_
    97 = "Inconsistent"
    98 = "Don't know"
     ;
  value F00653_
  9997 = "Inconsistent"
  9998 = "Don't know"
     ;
  value F00654_
     0 = "No"
     1 = "Vaccination date on card"
     2 = "Reported by mother"
     3 = "Vaccination marked on card"
     8 = "Don't know"
     ;
  value F00655_
    97 = "Inconsistent"
    98 = "Don't know"
     ;
  value F00656_
    97 = "Inconsistent"
    98 = "Don't know"
     ;
  value F00657_
  9997 = "Inconsistent"
  9998 = "Don't know"
     ;
  value F00658_
     0 = "No"
     1 = "Vaccination date on card"
     2 = "Reported by mother"
     3 = "Vaccination marked on card"
     8 = "Don't know"
     ;
  value F00659_
    97 = "Inconsistent"
    98 = "Don't know"
     ;
  value F00660_
    97 = "Inconsistent"
    98 = "Don't know"
     ;
  value F00661_
  9997 = "Inconsistent"
  9998 = "Don't know"
     ;
  value F00662_
     0 = "No"
     1 = "Vaccination date on card"
     2 = "Reported by mother"
     3 = "Vaccination marked on card"
     8 = "Don't know"
     ;
  value F00663_
    97 = "Inconsistent"
    98 = "Don't know"
     ;
  value F00664_
    97 = "Inconsistent"
    98 = "Don't know"
     ;
  value F00665_
  9997 = "Inconsistent"
  9998 = "Don't know"
     ;
  value F00666_
     0 = "No"
     1 = "Vaccination date on card"
     2 = "Reported by mother"
     3 = "Vaccination marked on card"
     8 = "Don't know"
     ;
  value F00667_
    97 = "Inconsistent"
    98 = "Don't know"
     ;
  value F00668_
    97 = "Inconsistent"
    98 = "Don't know"
     ;
  value F00669_
  9997 = "Inconsistent"
  9998 = "Don't know"
     ;
  value F00670_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00671_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00672_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00673_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00674_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00675_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00676_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00677_
     0 = "No"
     1 = "Yes, last 24 hours"
     2 = "Yes, last two weeks"
     8 = "Don't know"
     ;
  value F00678_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00679_
     0 = "No"
     1 = "Yes"
     ;
  value F00680_
     0 = "No"
     1 = "Yes"
     ;
  value F00681_
     0 = "No"
     1 = "Yes"
     ;
  value F00682_
     0 = "No"
     1 = "Yes"
     ;
  value F00683_
     0 = "No"
     1 = "Yes"
     ;
  value F00684_
     0 = "No"
     1 = "Yes"
     ;
  value F00685_
     0 = "No"
     1 = "Yes"
     ;
  value F00686_
     0 = "No"
     1 = "Yes"
     ;
  value F00687_
     0 = "No"
     1 = "Yes"
     ;
  value F00688_
     0 = "No"
     1 = "Yes"
     ;
  value F00689_
     0 = "No"
     1 = "Yes"
     ;
  value F00690_
     0 = "No"
     1 = "Yes"
     ;
  value F00691_
     0 = "No"
     1 = "Yes"
     ;
  value F00692_
     0 = "No"
     1 = "Yes"
     ;
  value F00693_
     0 = "No"
     1 = "Yes"
     ;
  value F00694_
     0 = "No"
     1 = "Yes"
     ;
  value F00695_
     0 = "No"
     1 = "Yes"
     ;
  value F00696_
     0 = "No"
     1 = "Yes"
     ;
  value F00697_
     0 = "No"
     1 = "Yes"
     ;
  value F00698_
     0 = "No"
     1 = "Yes"
     ;
  value F00699_
     0 = "No"
     1 = "Yes"
     ;
  value F00700_
     0 = "No"
     1 = "Yes"
     ;
  value F00701_
     0 = "No"
     1 = "Yes"
     ;
  value F00702_
     0 = "No"
     1 = "Yes"
     ;
  value F00703_
     0 = "No: received treatment"
     1 = "Yes: no treatment"
     ;
  value F00704_
     0 = "No"
     1 = "Yes"
     ;
  value F00705_
     0 = "No"
     1 = "Yes: ORS - spontaneous"
     2 = "Yes: ORS - probed"
     8 = "Don't know"
     ;
  value F00706_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00707_
     0 = "No"
     1 = "Yes: RHS - spontaneous"
     2 = "Yes: RHS - probed"
     8 = "Don't know"
     ;
  value F00708_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00709_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00710_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00711_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00712_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00713_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00714_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00715_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00716_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00717_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00718_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00719_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00720_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00721_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00722_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00723_
     0 = "No: received treatment"
     1 = "Yes: no treatment"
     8 = "Don't know"
     ;
  value F00724_
     0 = "No"
     1 = "Yes"
     ;
  value F00725_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00726_
     0 = "No"
     1 = "Yes, last 24 hours"
     2 = "Yes, last two weeks"
     8 = "Don't know"
     ;
  value F00727_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00728_
     1 = "Chest only"
     2 = "Nose only"
     3 = "Both"
     6 = "Other"
     8 = "Don't know"
     ;
  value F00729_
     0 = "Nothing to drink"
     2 = "Much less"
     3 = "Somewhat less"
     4 = "About the same"
     5 = "More"
     8 = "Don't know"
     ;
  value F00730_
     0 = "Stopped food"
     1 = "Never gave food"
     2 = "Much less"
     3 = "Somewhat less"
     4 = "About the same"
     5 = "More"
     8 = "Don't know"
     ;
  value F00731_
     0 = "No"
     1 = "Yes"
     ;
  value F00732_
     0 = "No"
     1 = "Yes"
     ;
  value F00733_
     0 = "No"
     1 = "Yes"
     ;
  value F00734_
     0 = "No"
     1 = "Yes"
     ;
  value F00735_
     0 = "No"
     1 = "Yes"
     ;
  value F00736_
     0 = "No"
     1 = "Yes"
     ;
  value F00737_
     0 = "No"
     1 = "Yes"
     ;
  value F00738_
     0 = "No"
     1 = "Yes"
     ;
  value F00739_
     0 = "No"
     1 = "Yes"
     ;
  value F00740_
     0 = "No"
     1 = "Yes"
     ;
  value F00741_
     0 = "No"
     1 = "Yes"
     ;
  value F00742_
     0 = "No"
     1 = "Yes"
     ;
  value F00743_
     0 = "No"
     1 = "Yes"
     ;
  value F00744_
     0 = "No"
     1 = "Yes"
     ;
  value F00745_
     0 = "No"
     1 = "Yes"
     ;
  value F00746_
     0 = "No"
     1 = "Yes"
     ;
  value F00747_
     0 = "No"
     1 = "Yes"
     ;
  value F00748_
     0 = "No"
     1 = "Yes"
     ;
  value F00749_
     0 = "No"
     1 = "Yes"
     ;
  value F00750_
     0 = "No"
     1 = "Yes"
     ;
  value F00751_
     0 = "No"
     1 = "Yes"
     ;
  value F00752_
     0 = "No"
     1 = "Yes"
     ;
  value F00753_
     0 = "No"
     1 = "Yes"
     ;
  value F00754_
     0 = "No"
     1 = "Yes"
     ;
  value F00755_
     0 = "No: received treatment"
     1 = "Yes: no treatment"
     ;
  value F00756_
     0 = "No"
     1 = "Yes"
     ;
  value F00757_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00758_
     0 = "No"
     1 = "Yes"
     ;
  value F00759_
     0 = "No"
     1 = "Yes"
     ;
  value F00760_
     0 = "No"
     1 = "Yes"
     ;
  value F00761_
     0 = "No"
     1 = "Yes"
     ;
  value F00762_
     0 = "No"
     1 = "Yes"
     ;
  value F00763_
     0 = "No"
     1 = "Yes"
     ;
  value F00764_
     0 = "No"
     1 = "Yes"
     ;
  value F00765_
     0 = "No"
     1 = "Yes"
     ;
  value F00766_
     0 = "No"
     1 = "Yes"
     ;
  value F00767_
     0 = "No"
     1 = "Yes"
     ;
  value F00768_
     0 = "No"
     1 = "Yes"
     ;
  value F00769_
     0 = "No"
     1 = "Yes"
     ;
  value F00770_
     0 = "No"
     1 = "Yes"
     ;
  value F00771_
     0 = "No"
     1 = "Yes"
     ;
  value F00772_
     0 = "No"
     1 = "Yes"
     ;
  value F00773_
     0 = "No"
     1 = "Yes"
     ;
  value F00774_
     0 = "No"
     1 = "Yes"
     ;
  value F00775_
     0 = "No"
     1 = "Yes"
     ;
  value F00776_
     0 = "No"
     1 = "Yes"
     ;
  value F00777_
     0 = "No"
     1 = "Yes"
     ;
  value F00778_
     0 = "No"
     1 = "Yes, nothing taken"
     ;
  value F00779_
     0 = "No"
     1 = "Yes, don't know"
     ;
  value F00780_
     0 = "Nothing to drink"
     2 = "Much less"
     3 = "Somewhat less"
     4 = "About the same"
     5 = "More"
     8 = "Don't know"
     ;
  value F00781_
     0 = "Stopped food"
     1 = "Never gave food"
     2 = "Much less"
     3 = "Somewhat less"
     4 = "About the same"
     5 = "More"
     8 = "Don't know"
     ;
  value F00782_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00783_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00784_
    10 = "PUBLIC SECTOR"
    11 = "Government hospital"
    12 = "CS public"
    13 = "CS public"
    14 = "CS public"
    15 = "CS public"
    16 = "CS public"
    20 = "PRIVATE SECTOR"
    21 = "Private hospital, clinic"
    22 = "Pharmacy"
    23 = "Private doctor"
    24 = "CS private"
    25 = "CS private"
    26 = "CS private"
    30 = "OTHER"
    31 = "CS other"
    32 = "CS other"
    96 = "Other"
     ;
  value F00785_
     0 = "Same day"
    98 = "Don't know"
     ;
  value F00786_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00787_
    98 = "Don't know"
     ;
  value F00788_
    10 = "PUBLIC SECTOR"
    11 = "Government hospital"
    12 = "CS public"
    13 = "CS public"
    14 = "CS public"
    15 = "CS public"
    16 = "CS public"
    20 = "PRIVATE SECTOR"
    21 = "Private hospital, clinic"
    22 = "Pharmacy"
    23 = "Private doctor"
    24 = "CS private"
    25 = "CS private"
    26 = "CS private"
    30 = "OTHER"
    31 = "CS other"
    32 = "CS other"
    33 = "CS other"
    96 = "Other"
     ;
  value F00789_
     0 = "Same day"
    98 = "Don't know"
     ;
  value F00790_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00791_
  9994 = "Not present"
  9995 = "Refused"
  9996 = "Other"
     ;
  value F00792_
  9994 = "Not present"
  9995 = "Refused"
  9996 = "Other"
     ;
  value F00793_
  9998 = "Flagged cases"
     ;
  value F00794_
  9998 = "Flagged cases"
     ;
  value F00795_
 99998 = "Flagged cases"
     ;
  value F00796_
  9998 = "Flagged cases"
     ;
  value F00797_
  9998 = "Flagged cases"
     ;
  value F00798_
 99998 = "Flagged cases"
     ;
  value F00799_
  9998 = "Flagged cases"
     ;
  value F00800_
  9998 = "Flagged cases"
     ;
  value F00801_
 99998 = "Flagged cases"
     ;
  value F00802_
     0 = "Measured"
     1 = "Dead"
     2 = "Sick"
     3 = "Not present"
     4 = "Refused"
     5 = "Mother refused"
     6 = "Other"
     7 = "No measurement found in household"
     ;
  value F00803_
     0 = "Not measured"
     1 = "Lying"
     2 = "Standing"
     ;
  value F00804_
    97 = "Inconsistent"
    98 = "Don't know"
     ;
  value F00805_
     0 = "Not in household"
     ;
  value F00806_
     1 = "Granted"
     2 = "Refused"
     ;
  value F00807_
   994 = "Not present"
   995 = "Refused"
   996 = "Other"
     ;
  value F00808_
     0 = "Measured"
     1 = "Dead"
     3 = "Not present"
     4 = "Refused"
     6 = "Other"
     7 = "No measurement found in household"
     ;
  value F00809_
   997 = "Inconsistent"
     ;
  value F00810_
     1 = "Severe"
     2 = "Moderate"
     3 = "Mild"
     4 = "Not anemic"
     ;
  value F00811_
     0 = "No"
     1 = "Yes"
     ;
  value F00812_
  9996 = "Height out of plausible limits"
  9997 = "Age in days out of plausible limits"
  9998 = "Flagged cases"
     ;
  value F00813_
  9996 = "Height out of plausible limits"
  9997 = "Age in days out of plausible limits"
  9998 = "Flagged cases"
     ;
  value F00814_
  9996 = "Height out of plausible limits"
  9997 = "Age in days out of plausible limits"
  9998 = "Flagged cases"
     ;
  value F00815_
  9996 = "Height out of plausible limits"
  9997 = "Age in days out of plausible limits"
  9998 = "Flagged cases"
     ;
  value F00816_
     0 = "Never in union"
     1 = "Married"
     2 = "Living with partner"
     3 = "Widowed"
     4 = "Divorced"
     5 = "No longer living together/separated"
     ;
  value F00817_
     0 = "Never in union"
     1 = "Currently in union/living with a man"
     2 = "Formerly in union/living with a man"
     ;
  value F00818_
     1 = "Once"
     2 = "More than once"
     ;
  value F00819_
     1 = "Living with her"
     2 = "Staying elsewhere"
     ;
  value F00820_
     0 = "No other wives"
    98 = "Don't know"
     ;
  value F00821_
    98 = "Don't know"
     ;
  value F00822_
     1 = "Month and year - information complete"
     2 = "Month and age - year imputed"
     3 = "Year and age - month imputed"
     5 = "Year - age/month imputed"
     6 = "Age - year/month imputed"
     7 = "Month - age/year imputed"
     8 = "None - all imputed"
     ;
  value F00823_
     0 = "Never married"
     1 = "0-4"
     2 = "5-9"
     3 = "10-14"
     4 = "15-19"
     5 = "20-24"
     6 = "25-29"
     7 = "30+"
     ;
  value F00824_
     0 = "Not had sex"
    96 = "At first union"
    97 = "Inconsistent"
    98 = "Don't know"
     ;
  value F00825_
   100 = "<1 day ago"
   101 = "Days: 1"
   199 = "Days: number missing"
   201 = "Weeks: 1"
   299 = "Weeks: number missing"
   301 = "Months: 1"
   399 = "Months: number missing"
   401 = "Years: 1"
   499 = "Years: number missing"
   995 = "Within last 4 weeks"
   996 = "Before last birth"
   997 = "Refuse to answer"
   998 = "Don't know"
     ;
  value F00826_
    31 = "31+ days"
    95 = "Within last 4 weeks"
    96 = "Before last birth"
    97 = "Inconsistent"
    98 = "Don't know"
     ;
  value F00827_
   995 = "Within last 4 weeks"
   996 = "Before last birth"
   997 = "Inconsistent"
   998 = "Don't know"
     ;
  value F00828_
     0 = "No flag"
     1 = "N > interval since birth"
     2 = "N + abstinence > interval"
     3 = "N, but sex not res."
     4 = "96, but no birth"
     5 = "96, but currently pregnant"
     6 = "96, but resumed sex"
     7 = "N, last sex before pregnancy"
     8 = "N, inconsistent 4 weeks"
     9 = "N, sex before last birth"
     ;
  value F00829_
     0 = "Not had sex"
    97 = "Inconsistent"
    98 = "Don't know"
     ;
  value F00830_
     0 = "No flag"
     1 = "After interview"
     2 = "After conception >= 1 year"
     3 = "After conception < 1 year"
     4 = "At marriage, but never married"
     5 = "At marriage, but after conception"
     6 = "After marriage"
     ;
  value F00831_
     0 = "No"
     1 = "Formerly married"
     2 = "Lived with a man"
     ;
  value F00832_
     0 = "Never had sex"
     1 = "Active in last 4 weeks"
     2 = "Not active in last 4 weeks - postpartum abstinence"
     3 = "Not active in last 4 weeks - not postpartum abstinence"
     ;
  value F00833_
    60 = "60+"
    96 = "Before last birth"
    97 = "Inconsistent"
    98 = "Don't know"
     ;
  value F00834_
     1 = "Death/widowhood"
     2 = "Divorce"
     3 = "Separation"
     ;
  value F00835_
     1 = "Respondent"
     2 = "Other wife"
     3 = "Spouse's children"
     4 = "Spouse's family"
     6 = "Other"
     7 = "No property"
     ;
  value F00836_
     0 = "No"
     1 = "Yes"
     ;
  value F00837_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00838_
     1 = "Have another"
     2 = "Undecided"
     3 = "No more"
     4 = "Sterilized (respondent or partner)"
     5 = "Declared infecund"
     6 = "Never had sex"
     ;
  value F00839_
   100 = "Wanted now"
   101 = "Months: 1"
   199 = "Months: number missing"
   201 = "Years: 1"
   299 = "Years: number missing"
   993 = "After marriage"
   994 = "Soon, now"
   996 = "Other, non-numeric"
   997 = "Inconsistent"
   998 = "Don't know"
     ;
  value F00840_
     0 = "<12 months"
     1 = "1 year"
     2 = "2 years"
     3 = "3 years"
     4 = "4 years"
     5 = "5 years"
     6 = "6+ years"
     7 = "Non-numeric"
     8 = "Don't know"
     ;
  value F00841_
     1 = "Wants within 2 years"
     2 = "Wants after 2+ years"
     3 = "Wants, unsure timing"
     4 = "Undecided"
     5 = "Wants no more"
     6 = "Sterilized (respondent or partner)"
     7 = "Declared infecund"
     8 = "Never had sex"
     ;
  value F00842_
    96 = "Non-numeric response"
    98 = "Don't know"
     ;
  value F00843_
     6 = "6+"
     7 = "Non-numeric response"
     ;
  value F00844_
   100 = "Wanted now"
   101 = "Months: 1"
   199 = "Months: number missing"
   201 = "Years: 1"
   299 = "Years: number missing"
   993 = "After marriage"
   994 = "Soon/now"
   995 = "Can't get pregnant"
   996 = "Other"
   998 = "Don't know"
     ;
  value F00845_
     1 = "Both want same"
     2 = "Husband wants more"
     3 = "Husband wants fewer"
     8 = "Don't know"
     ;
  value F00846_
     0 = "Fecund"
     1 = "Pregnant"
     2 = "Postpartum amenorrheic"
     3 = "Infecund, menopausal"
     ;
  value F00847_
     0 = "Never had sex"
     1 = "Unmet need for spacing"
     2 = "Unmet need for limiting"
     3 = "Using for spacing"
     4 = "Using for limiting"
     5 = "Spacing failure"
     6 = "Limiting failure"
     7 = "No unmet need"
     8 = "Not married and no sex in last 30 days"
     9 = "Infecund, menopausal"
     ;
  value F00848_
     0 = "Fecund"
     1 = "Pregnant"
     2 = "Postpartum amenorrheic"
     3 = "Infecund, menopausal"
     ;
  value F00849_
     0 = "Never had sex"
     1 = "Unmet need for spacing"
     2 = "Unmet need for limiting"
     3 = "Using for spacing"
     4 = "Using for limiting"
     5 = "Spacing failure"
     6 = "Limiting failure"
     7 = "No unmet need"
     8 = "Not married and no sex in last 30 days"
     9 = "Infecund, menopausal"
     ;
  value F00850_
     0 = "Fecund"
     1 = "Pregnant"
     2 = "Postpartum amenorrheic"
     3 = "Infecund, menopausal"
     ;
  value F00851_
     0 = "Never had sex"
     1 = "Unmet need for spacing"
     2 = "Unmet need for limiting"
     3 = "Using for spacing"
     4 = "Using for limiting"
     7 = "No unmet need"
     8 = "Not married and no sex in last 30 days"
     9 = "Infecund, menopausal"
     ;
  value F00852_
    96 = "Other"
    98 = "Don't know"
     ;
  value F00853_
    96 = "Other"
    98 = "Don't know"
     ;
  value F00854_
    96 = "Other"
    98 = "Don't know"
     ;
  value F00855_
     1 = "Big problem"
     2 = "Small problem"
     3 = "No problem"
     4 = "Says she can't get pregnant/not having sex"
     ;
  value F00856_
     1 = "Mainly respondent"
     2 = "Mainly husband, partner"
     3 = "Joint decision"
     6 = "Other"
     ;
  value F00857_
     1 = "Mainly respondent"
     2 = "Mainly husband, partner"
     3 = "Joint decision"
     6 = "Other"
     ;
  value F00858_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00859_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00860_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00861_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00862_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00863_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00864_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00865_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00866_
     0 = "No education"
     1 = "Primary"
     2 = "Secondary"
     3 = "Higher"
     8 = "Don't know"
     ;
  value F00867_
     0 = "No years completed at level V702"
    98 = "Don't know"
     ;
  value F00868_
     0 = "Not working and didn't work in last 12 months"
 99998 = "Don't know"
     ;
  value F00869_
     0 = "Didn't work last 12 months"
     1 = "Worked last 7 days"
     2 = "Worked last 12 months"
     8 = "Don't know"
     ;
  value F00870_
     0 = "Did not work"
     1 = "Professional/technical/managerial"
     2 = "Clerical"
     3 = "Sales"
     4 = "Agricultural - self employed"
     5 = "Agricultural - employee"
     6 = "Household and domestic"
     7 = "Services"
     8 = "Skilled manual"
     9 = "Unskilled manual"
    98 = "Don't know"
     ;
  value F00871_
     0 = "No"
     1 = "Yes"
     ;
  value F00872_
     0 = "No"
     1 = "Yes"
     ;
  value F00873_
    97 = "Inconsistent"
    98 = "Don't know"
     ;
  value F00874_
     0 = "Not working and have never worked"
     1 = "Professional, technical (similar employment)"
     2 = "Director, public employee (high rank positions)"
     3 = "Administrative personnel (similar employment)"
     4 = "Merchant, vendors"
     5 = "Service workers"
     6 = "Agricultural work (forester,  fisher, animal hunter)"
     7 = "Machinery operator, transportation (similar employment)"
     8 = "Other including army"
 99998 = "Don't know"
     ;
  value F00875_
     0 = "Not working"
     1 = "Professional/technical/managerial"
     2 = "Clerical"
     3 = "Sales"
     4 = "Agricultural - self employed"
     5 = "Agricultural - employee"
     6 = "Household and domestic"
     7 = "Services"
     8 = "Skilled manual"
     9 = "Unskilled manual"
    98 = "Don't know"
     ;
  value F00876_
     1 = "For family member"
     2 = "For someone else"
     3 = "Self-employed"
     ;
  value F00877_
     1 = "At home"
     2 = "Away"
     ;
  value F00878_
     0 = "No education"
     1 = "Incomplete primary"
     2 = "Complete primary"
     3 = "Incomplete secondary"
     4 = "Complete secondary"
     5 = "Higher"
     8 = "Don't know"
     ;
  value F00879_
    96 = "96+"
    98 = "Don't know"
     ;
  value F00880_
     0 = "No"
     1 = "In the past year"
     2 = "Currently working"
     3 = "Have a job, but on leave last 7 days"
     ;
  value F00881_
     1 = "All year"
     2 = "Seasonal"
     3 = "Occasional"
     ;
  value F00882_
     1 = "Respondent alone"
     2 = "Respondent and husband/partner"
     3 = "Respondent and other person"
     4 = "Husband/partner alone"
     5 = "Someone else"
     ;
  value F00883_
     0 = "Own land"
     1 = "Family land"
     2 = "Someone else's land"
     3 = "Rented land"
     8 = "Don't know"
     ;
  value F00884_
     0 = "Not paid"
     1 = "Cash only"
     2 = "Cash and in-kind"
     3 = "In-kind only"
     ;
  value F00885_
     0 = "No one"
     1 = "Respondent alone"
     2 = "Respondent and husband/partner"
     3 = "Respondent and other person"
     4 = "Husband/partner alone"
     5 = "Someone else"
     6 = "Other"
     ;
  value F00886_
     0 = "No one"
     1 = "Respondent alone"
     2 = "Respondent and husband/partner"
     3 = "Respondent and other person"
     4 = "Husband/partner alone"
     5 = "Someone else"
     6 = "Other"
     ;
  value F00887_
     0 = "No one"
     1 = "Respondent alone"
     2 = "Respondent and husband/partner"
     3 = "Respondent and other person"
     4 = "Husband/partner alone"
     5 = "Someone else"
     6 = "Other"
     ;
  value F00888_
     0 = "No one"
     1 = "Respondent alone"
     2 = "Respondent and husband/partner"
     3 = "Respondent and other person"
     4 = "Husband/partner alone"
     5 = "Someone else"
     6 = "Other"
     ;
  value F00889_
     0 = "No one"
     1 = "Respondent alone"
     2 = "Respondent and husband/partner"
     3 = "Respondent and other person"
     4 = "Husband/partner alone"
     5 = "Someone else"
     6 = "Other"
     ;
  value F00890_
     0 = "No one"
     1 = "Respondent alone"
     2 = "Respondent and husband/partner"
     3 = "Respondent and other person"
     4 = "Husband/partner alone"
     5 = "Someone else"
     6 = "Other"
     7 = "Husband/partner has no earnings"
     ;
  value F00891_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00892_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00893_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00894_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00895_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00896_
     0 = "Does not own"
     1 = "Alone only"
     2 = "Jointly only"
     3 = "Both alone and jointly"
     ;
  value F00897_
     0 = "Does not own"
     1 = "Alone only"
     2 = "Jointly only"
     3 = "Both alone and jointly"
     ;
  value F00898_
     0 = "Does not have a title deed"
     1 = "Has a title deed and respondent on title"
     2 = "Has a title deed but respondent not on title"
     3 = "Has a title deed DK if respondent on title"
     8 = "DK if has a title deed"
     ;
  value F00899_
     0 = "Does not have a title deed"
     1 = "Has a title deed and respondent on title"
     2 = "Has a title deed but respondent not on title"
     3 = "Has a title deed DK if respondent on title"
     8 = "DK if has a title deed"
     ;
  value F00900_
     1 = "More than him"
     2 = "Less than him"
     3 = "About the same"
     4 = "Husband/partner doesn't bring in money"
     5 = "Only worked before marriage"
     8 = "Don't know"
     ;
  value F00901_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00902_
     0 = "No"
     1 = "Yes"
     ;
  value F00903_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00904_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00905_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00906_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00907_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00908_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00909_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00910_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00911_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00912_
    10 = "PUBLIC"
    20 = "PRIVATE MEDICAL"
    30 = "OTHER PRIVATE"
    96 = "OTHER"
    98 = "DK"
     ;
  value F00913_
    98 = "Don't know"
     ;
  value F00914_
     0 = "No"
     1 = "Yes"
     ;
  value F00915_
     0 = "No"
     1 = "Yes"
     ;
  value F00916_
     0 = "No"
     1 = "Yes"
     ;
  value F00917_
     0 = "No"
     1 = "Yes"
     ;
  value F00918_
     0 = "No"
     1 = "Yes"
     ;
  value F00919_
     0 = "No"
     1 = "Yes"
     ;
  value F00920_
     0 = "No"
     1 = "Yes"
     ;
  value F00921_
     0 = "No"
     1 = "Yes"
     ;
  value F00922_
     0 = "No"
     1 = "Yes"
     ;
  value F00923_
     0 = "No"
     1 = "Yes"
     ;
  value F00924_
     0 = "No"
     1 = "Yes"
     ;
  value F00925_
     0 = "No"
     1 = "Yes"
     ;
  value F00926_
     0 = "No"
     1 = "Yes"
     ;
  value F00927_
     0 = "No"
     1 = "Yes"
     ;
  value F00928_
     0 = "No"
     1 = "Yes"
     ;
  value F00929_
     0 = "No"
     1 = "Yes"
     ;
  value F00930_
     0 = "No"
     1 = "Yes"
     ;
  value F00931_
     0 = "No"
     1 = "Yes"
     ;
  value F00932_
     0 = "No"
     1 = "Yes"
     ;
  value F00933_
     0 = "No"
     1 = "Yes"
     ;
  value F00934_
     0 = "No"
     1 = "Yes"
     ;
  value F00935_
     0 = "No"
     1 = "Yes"
     ;
  value F00936_
     0 = "No"
     1 = "Yes"
     ;
  value F00937_
     0 = "No"
     1 = "Yes"
     ;
  value F00938_
     0 = "No: source known"
     1 = "Yes: no source known"
     ;
  value F00939_
     0 = "No"
     1 = "Yes"
     ;
  value F00940_
     0 = "No"
     1 = "Yes"
     ;
  value F00941_
     0 = "No"
     1 = "Yes"
     ;
  value F00942_
     0 = "No"
     1 = "Yes"
     ;
  value F00943_
     0 = "No"
     1 = "Yes"
     ;
  value F00944_
     0 = "No"
     1 = "Yes"
     ;
  value F00945_
     0 = "No"
     1 = "Yes"
     ;
  value F00946_
     0 = "No"
     1 = "Yes"
     ;
  value F00947_
     0 = "No"
     1 = "Yes"
     ;
  value F00948_
     0 = "No"
     1 = "Yes"
     ;
  value F00949_
     0 = "No"
     1 = "Yes"
     ;
  value F00950_
     0 = "No"
     1 = "Yes"
     ;
  value F00951_
     0 = "No"
     1 = "Yes"
     ;
  value F00952_
     0 = "No"
     1 = "Yes"
     ;
  value F00953_
     0 = "No"
     1 = "Yes"
     ;
  value F00954_
     0 = "No"
     1 = "Yes"
     ;
  value F00955_
     0 = "No"
     1 = "Yes"
     ;
  value F00956_
     0 = "No"
     1 = "Yes"
     ;
  value F00957_
     0 = "No"
     1 = "Yes"
     ;
  value F00958_
     0 = "No"
     1 = "Yes"
     ;
  value F00959_
     0 = "No"
     1 = "Yes"
     ;
  value F00960_
     0 = "No"
     1 = "Yes"
     ;
  value F00961_
     0 = "No"
     1 = "Yes"
     ;
  value F00962_
     0 = "No"
     1 = "Yes"
     ;
  value F00963_
     0 = "No: source known"
     1 = "Yes: no source known"
     ;
  value F00964_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00965_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00966_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00967_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00968_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00969_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00970_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00971_
    98 = "Don't know"
     ;
  value F00972_
    98 = "Don't know"
     ;
  value F00973_
     1 = "Spouse"
     2 = "Boyfriend not living with respondent"
     3 = "Other friend"
     4 = "Casual acquaintance"
     5 = "Relative"
     6 = "Commercial sex worker"
     7 = "Live-in partner"
     8 = "Ex-husband/partner"
    96 = "Other"
     ;
  value F00974_
     1 = "Spouse"
     2 = "Boyfriend not living with respondent"
     3 = "Other friend"
     4 = "Casual acquaintance"
     5 = "Relative"
     6 = "Commercial sex worker"
     7 = "Live-in partner"
     8 = "Ex-husband/partner"
    96 = "Other"
     ;
  value F00975_
     1 = "Spouse"
     2 = "Boyfriend not living with respondent"
     3 = "Other friend"
     4 = "Casual acquaintance"
     5 = "Relative"
     6 = "Commercial sex worker"
     7 = "Live-in partner"
     8 = "Ex-husband/partner"
    96 = "Other"
     ;
  value F00976_
   101 = "One day"
   201 = "One month"
   301 = "One year"
     ;
  value F00977_
   101 = "One day"
   201 = "One month"
   301 = "One year"
     ;
  value F00978_
   101 = "One day"
   201 = "One month"
   301 = "One year"
     ;
  value F00979_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00980_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00981_
     0 = "No"
     1 = "Yes"
     ;
  value F00982_
     0 = "No"
     1 = "Yes"
     ;
  value F00983_
     0 = "No"
     1 = "Yes"
     ;
  value F00984_
     0 = "No"
     1 = "Yes"
     ;
  value F00985_
     0 = "No"
     1 = "Yes"
     ;
  value F00986_
     0 = "No"
     1 = "Yes"
     ;
  value F00987_
     0 = "No"
     1 = "Yes"
     ;
  value F00988_
     0 = "No"
     1 = "Yes"
     ;
  value F00989_
     0 = "No"
     1 = "Yes"
     ;
  value F00990_
     0 = "No"
     1 = "Yes"
     ;
  value F00991_
     0 = "No"
     1 = "Yes"
     ;
  value F00992_
     0 = "No"
     1 = "Yes"
     ;
  value F00993_
     0 = "No"
     1 = "Yes"
     ;
  value F00994_
     0 = "No"
     1 = "Yes"
     ;
  value F00995_
     0 = "No"
     1 = "Yes"
     ;
  value F00996_
     0 = "No"
     1 = "Yes"
     ;
  value F00997_
     0 = "No"
     1 = "Yes"
     ;
  value F00998_
     0 = "No"
     1 = "Yes"
     ;
  value F00999_
     0 = "No"
     1 = "Yes"
     ;
  value F01000_
     0 = "No"
     1 = "Yes"
     ;
  value F01001_
     0 = "No"
     1 = "Yes"
     ;
  value F01002_
     0 = "No"
     1 = "Yes"
     ;
  value F01003_
     0 = "No"
     1 = "Yes"
     ;
  value F01004_
     0 = "No"
     1 = "Yes"
     ;
  value F01005_
     0 = "No"
     1 = "Yes"
     ;
  value F01006_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F01007_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F01008_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F01009_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F01010_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F01011_
     0 = "Disagree"
     1 = "Agree"
     8 = "Don't know/not sure/depends"
     ;
  value F01012_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F01013_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F01014_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F01015_
     0 = "No"
     1 = "Yes"
     ;
  value F01016_
     0 = "No"
     1 = "Yes"
     ;
  value F01017_
     0 = "No"
     1 = "Yes"
     ;
  value F01018_
     0 = "No"
     1 = "Yes"
     ;
  value F01019_
     0 = "No"
     1 = "Yes"
     ;
  value F01020_
     0 = "No"
     1 = "Yes"
     ;
  value F01021_
     0 = "No"
     1 = "Yes"
     ;
  value F01022_
     0 = "No"
     1 = "Yes"
     ;
  value F01023_
     0 = "No"
     1 = "Yes"
     ;
  value F01024_
     0 = "No"
     1 = "Yes"
     ;
  value F01025_
     0 = "No"
     1 = "Yes"
     ;
  value F01026_
     0 = "No"
     1 = "Yes"
     ;
  value F01027_
     0 = "No"
     1 = "Yes"
     ;
  value F01028_
     0 = "No"
     1 = "Yes"
     ;
  value F01029_
     0 = "No"
     1 = "Yes"
     ;
  value F01030_
     0 = "No"
     1 = "Yes"
     ;
  value F01031_
     0 = "No"
     1 = "Yes"
     ;
  value F01032_
     0 = "No"
     1 = "Yes"
     ;
  value F01033_
     0 = "No"
     1 = "Yes"
     ;
  value F01034_
     0 = "No"
     1 = "Yes"
     ;
  value F01035_
     0 = "No"
     1 = "Yes"
     ;
  value F01036_
     0 = "No"
     1 = "Yes"
     ;
  value F01037_
     0 = "No"
     1 = "Yes"
     ;
  value F01038_
     0 = "No"
     1 = "Yes"
     ;
  value F01039_
     0 = "No"
     1 = "Yes"
     ;
  value F01040_
     0 = "No"
     1 = "Yes"
     ;
  value F01041_
     0 = "No"
     1 = "Yes"
     ;
  value F01042_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F01043_
     1 = "Younger"
     2 = "About the same age"
     3 = "Less than 10 years older"
     4 = "10 or more years older"
     5 = "Older, don't know difference"
     8 = "Don't know"
     ;
  value F01044_
     1 = "Younger"
     2 = "About the same age"
     3 = "Less than 10 years older"
     4 = "10 or more years older"
     5 = "Older, don't know difference"
     8 = "Don't know"
     ;
  value F01045_
     1 = "Younger"
     2 = "About the same age"
     3 = "Less than 10 years older"
     4 = "10 or more years older"
     5 = "Older, don't know difference"
     8 = "Don't know"
     ;
  value F01046_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F01047_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F01048_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F01049_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F01050_
     1 = "Less than 12 months"
     2 = "12-23 months"
     3 = "2 years or more"
     ;
  value F01051_
    95 = "Two or more years"
    98 = "Don't know"
     ;
  value F01052_
     1 = "Asked for the test"
     2 = "Offered and accepted"
     3 = "Required"
     ;
  value F01053_
     0 = "No"
     1 = "Yes"
     ;
  value F01054_
    10 = "PUBLIC"
    11 = "Hospital/Clinic"
    20 = "PRIVATE MEDICAL"
    21 = "Private hospital/clinic"
    22 = "EPS health center"
    23 = "Private doctor"
    24 = "PROFAMILIA"
    96 = "Other"
    98 = "Don't know"
     ;
  value F01055_
    95 = "95+"
    98 = "Don't know"
     ;
  value F01056_
     1 = "Younger"
     2 = "About the same age"
     3 = "Less than 10 years older"
     4 = "10 or more years older"
     5 = "Older, don't know difference"
     8 = "Don't know"
     ;
  value F01057_
   100 = "<1 day ago"
   101 = "Days: 1"
   199 = "Days: number missing"
   201 = "Weeks: 1"
   299 = "Weeks: number missing"
   301 = "Months: 1"
   399 = "Months: number missing"
   401 = "Years: 1"
   995 = "Within last 4 weeks"
   996 = "Before last birth"
   997 = "Inconsistent"
   998 = "Don't know"
     ;
  value F01058_
   100 = "<1 day ago"
   101 = "Days: 1"
   199 = "Days: number missing"
   201 = "Weeks: 1"
   299 = "Weeks: number missing"
   301 = "Months: 1"
   399 = "Months: number missing"
   401 = "Years: 1"
   995 = "Within last 4 weeks"
   996 = "Before last birth"
   997 = "Inconsistent"
   998 = "Don't know"
     ;
  value F01059_
     0 = "No"
     1 = "Yes"
     ;
  value F01060_
     0 = "No"
     1 = "Yes"
     ;
  value F01061_
     0 = "No"
     1 = "Yes"
     ;
  value F01062_
    95 = "95+"
    98 = "Don't know"
     ;
  value F01063_
    95 = "95+"
    98 = "Don't know"
     ;
  value F01064_
    95 = "95+"
    98 = "Don't know"
     ;
  value F01065_
     0 = "No"
     1 = "Respondent drunk only"
     2 = "Partner drunk only"
     3 = "Both drunk"
     4 = "Neither drunk but consumed alcohol"
     ;
  value F01066_
     0 = "No"
     1 = "Respondent drunk only"
     2 = "Partner drunk only"
     3 = "Both drunk"
     4 = "Neither drunk but consumed alcohol"
     ;
  value F01067_
     0 = "No"
     1 = "Respondent drunk only"
     2 = "Partner drunk only"
     3 = "Both drunk"
     4 = "Neither drunk but consumed alcohol"
     ;
  value F01068_
    95 = "95+"
    98 = "Don't know"
     ;
  value F01069_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F01070_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F01071_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F01072_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F01073_
     0 = "No"
     1 = "Yes"
     ;
  value F01074_
     0 = "No"
     1 = "Yes"
     ;
  value F01075_
     0 = "No"
     1 = "Yes"
     ;
  value F01076_
     0 = "No"
     1 = "Yes"
     ;
  value F01077_
     0 = "No"
     1 = "Yes"
     ;
  value F01078_
     0 = "No"
     1 = "Yes"
     ;
  value F01079_
    10 = "PUBLIC"
    11 = "Hospital/Clinic"
    20 = "PRIVATE MEDICAL"
    21 = "Private hospital/clinic"
    22 = "EPS health center"
    23 = "Private doctor"
    24 = "PROFAMILIA"
    96 = "Other"
    98 = "Don't know"
     ;
  value F01080_
     0 = "No"
     1 = "Yes"
     ;
  value F01081_
     0 = "No"
     1 = "Yes"
     8 = "Don't know anyone with HIV"
     ;
  value F01082_
     0 = "No"
     1 = "Yes"
     ;
  value F01083_
     0 = "No"
     1 = "Yes"
     ;
  value F01084_
     0 = "Disagree"
     1 = "Agree"
     8 = "Don't know/no opinion"
     ;
  value F01085_
     0 = "Disagree"
     1 = "Agree"
     8 = "Don't know/no opinion"
     ;
  value F01086_
     0 = "No"
     1 = "Yes"
     8 = "Don't know/depends"
     ;
  value F01087_
     0 = "No"
     1 = "Yes"
     8 = "Don't know/not sure/depends"
     ;
  value F01088_
     0 = "No"
     1 = "Yes"
     8 = "Don't know/not sure/depends"
     ;
  value F01089_
     0 = "No"
     1 = "Yes"
     8 = "Don't know/not sure/depends"
     ;
  value F01090_
     0 = "No"
     1 = "Yes"
     8 = "Don't know/not sure/depends"
     ;
  value F01091_
     0 = "No"
     1 = "Yes"
     8 = "Don't know/not sure/depends"
     ;
  value F01092_
     0 = "No"
     1 = "Yes"
     8 = "Don't know/not sure/depends"
     ;
  value F01093_
     0 = "No"
     1 = "Yes"
     8 = "Don't know/not sure/depends"
     ;
  value F01094_
     0 = "No"
     1 = "Yes"
     8 = "Don't know/not sure/depends"
     ;
  value F01095_
     0 = "No"
     1 = "Yes"
     8 = "Don't know/not sure/depends"
     ;
  value F01096_
     0 = "No"
     1 = "Yes"
     8 = "Don't know/not sure/depends"
     ;
  value F01097_
     0 = "No"
     1 = "Yes"
     8 = "Don't know/not sure/depends"
     ;
  value F01098_
     0 = "No"
     1 = "Yes"
     8 = "Don't know/not sure/depends"
     ;
  value F01099_
     0 = "No"
     1 = "Yes"
     8 = "Don't know/not sure/depends"
     ;
  value F01100_
     0 = "No"
     1 = "Yes"
     8 = "Don't know/not sure/depends"
     ;
  value F01101_
   101 = "Days: 1"
   199 = "Days: number missing"
   201 = "Weeks: 1"
   299 = "Weeks: number missing"
   301 = "Months: 1"
   399 = "Months: number missing"
   401 = "Years: 1"
   499 = "Years: number missing"
   995 = "Had sex with partner only once"
     ;
  value F01102_
   101 = "Days: 1"
   199 = "Days: number missing"
   201 = "Weeks: 1"
   299 = "Weeks: number missing"
   301 = "Months: 1"
   399 = "Months: number missing"
   401 = "Years: 1"
   499 = "Years: number missing"
   995 = "Had sex with partner only once"
     ;
  value F01103_
   101 = "Days: 1"
   199 = "Days: number missing"
   201 = "Weeks: 1"
   299 = "Weeks: number missing"
   301 = "Months: 1"
   399 = "Months: number missing"
   401 = "Years: 1"
   499 = "Years: number missing"
   995 = "Had sex with partner only once"
     ;
  value F01104_
    95 = "95+"
    97 = "Refuse to answer"
    98 = "Don't know"
     ;
  value F01105_
    95 = "95+"
    97 = "Refuse to answer"
    98 = "Don't know"
     ;
  value F01106_
    95 = "95+"
    97 = "Refuse to answer"
    98 = "Don't know"
     ;
  value F01107_
     0 = "No"
     1 = "Yes"
     ;
  value F01108_
     0 = "No"
     1 = "Yes"
     ;
  value F01109_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F01110_
     0 = "Never heard of HIV test kits"
     1 = "Has tested with HIV test kits"
     2 = "Knows test kits but never tested with them"
     3 = "Knows test kits un-known if tested"
     ;
  value F01111_
     0 = "No"
     1 = "Yes"
     8 = "Don't know/not sure/depends"
     ;
  value F01112_
     0 = "No"
     1 = "Yes"
     8 = "Don't know/not sure/depends"
     ;
  value F01113_
     0 = "No"
     1 = "Yes"
     8 = "Don't know/not sure/depends"
     ;
  value F01114_
     0 = "No"
     1 = "Yes"
     8 = "Don't know/not sure/depends"
     ;
  value F01115_
     0 = "No"
     1 = "Yes"
     2 = "Says she has HIV"
     8 = "Don't know/not sure/depends"
     ;
  value F01116_
    95 = "95+"
    96 = "2+ visits"
    97 = "Inconsistent"
     ;
  value F01117_
     0 = "No"
     1 = "Yes - listening"
     2 = "Yes - not listening"
     ;
  value F01118_
     0 = "No"
     1 = "Yes - listening"
     2 = "Yes - not listening"
     ;
  value F01119_
     0 = "No"
     1 = "Yes - listening"
     2 = "Yes - not listening"
     ;
  value F01120_
     0 = "No"
     1 = "Yes - listening"
     2 = "Yes - not listening"
     ;
  value F01121_
     0 = "No"
     1 = "Yes"
     ;
  value F01122_
     0 = "No"
     1 = "Yes"
     ;
  value F01123_
     0 = "No"
     1 = "Yes"
     ;
  value F01124_
     1 = "Pregnancies and contraception"
     2 = "Reason for discontinuation"
     3 = "Marriage and unions"
     4 = "Moves and communities"
     5 = "Source of method"
     6 = "Country specific"
     7 = "Country specific"
     8 = "Country specific"
     9 = "Country specific"
     ;
  value $F01125_
'B                                                                               ' = "Birth                                                                           "
'P                                                                               ' = "Pregnancy                                                                       "
'T                                                                               ' = "Termination                                                                     "
'0                                                                               ' = "Not using                                                                       "
'1                                                                               ' = "Pill                                                                            "
'2                                                                               ' = "IUD                                                                             "
'3                                                                               ' = "Injections                                                                      "
'5                                                                               ' = "Condom                                                                          "
'6                                                                               ' = "Female sterilization                                                            "
'7                                                                               ' = "Male sterilization                                                              "
'8                                                                               ' = "Periodic abstinence                                                             "
'9                                                                               ' = "Withdrawal                                                                      "
'W                                                                               ' = "Other traditional                                                               "
'N                                                                               ' = "Implants/Norplant                                                               "
'L                                                                               ' = "Lactational amenorrhea (LAM)                                                    "
'F                                                                               ' = "Foam or jelly                                                                   "
'I                                                                               ' = "Injection every three months                                                    "
'R                                                                               ' = "Vaginal ring                                                                    "
'M                                                                               ' = "Other modern method (patch)                                                     "
'?                                                                               ' = "Missing                                                                         "
     ;
  value F01126_
     1 = "Male"
     2 = "Female"
     3 = "Male transgender"
     4 = "Female transgender"
     8 = "Don't know"
     ;
  value F01127_
     0 = "Dead"
     1 = "Alive"
     8 = "Don't know"
     ;
  value F01128_
    97 = "97+"
    98 = "Don't know"
     ;
  value F01129_
     0 = "Never married"
     1 = "Ever married"
     8 = "Don't know"
     ;
  value F01130_
    97 = "97+"
    98 = "Don't know"
     ;
  value F01131_
    97 = "97+"
    98 = "Don't know"
     ;
  value F01132_
     0 = "Never pregnant"
     1 = "Death not related"
     2 = "Died while pregnant"
     3 = "Died during delivery"
     4 = "Since delivery"
     5 = "6 weeks after delivery"
     6 = "2 months after delivery"
    98 = "Don't know"
     ;
  value F01133_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F01134_
     0 = "Country specific"
     ;
  value F01135_
   100 = "Same day"
   101 = "Days: 1"
   199 = "Days: number missing"
   201 = "Months: 1"
   299 = "Months: number missing"
   301 = "Years: 1"
   399 = "Years: number missing"
   997 = "Inconsistent"
   998 = "Don't know"
     ;
  value F01136_
     0 = "Country specific"
     ;
  value F01137_
  9998 = "Don't know"
     ;
  value F01138_
     0 = "No"
     1 = "Violence"
     2 = "Accident"
     ;
  value F01139_
    98 = "Don't know"
     ;
  value F01140_
     0 = "Respondent reported"
     ;
  value F01141_
    10 = "10 years"
    12 = "12 years"
    13 = "13 years"
    15 = "15 years"
     ;
  value F01142_
     0 = "No"
     1 = "Yes, last year"
     2 = "Yes, before last year"
     ;
  value F01143_
     0 = "No"
     1 = "Yes, last year"
     2 = "Yes, before last year"
     ;
  value F01144_
     0 = "No"
     1 = "Yes, last year"
     2 = "Yes, before last year"
     ;
  value F01145_
     0 = "No"
     1 = "Yes, last year"
     2 = "Yes, before last year"
     ;
  value F01146_
     0 = "No"
     1 = "Yes, last year"
     2 = "Yes, before last year"
     ;
  value F01147_
     0 = "No"
     1 = "Yes, last year"
     2 = "Yes, before last year"
     ;
  value F01148_
     0 = "No"
     1 = "Yes, last year"
     2 = "Yes, before last year"
     ;
  value F01149_
     0 = "No"
     1 = "Yes, last year"
     2 = "Yes, before last year"
     ;
  value F01150_
     0 = "No"
     1 = "Yes, last year"
     2 = "Yes, before last year"
     ;
  value F01151_
     0 = "No"
     1 = "Yes, last year"
     2 = "Yes, before last year"
     ;
  value F01152_
     0 = "No"
     1 = "Yes, last year"
     2 = "Yes, before last year"
     ;
  value F01153_
     0 = "No"
     1 = "Yes, last year"
     2 = "Yes, before last year"
     ;
  value F01154_
     0 = "No"
     1 = "Yes, last year"
     2 = "Yes, before last year"
     ;
  value F01155_
     0 = "Never"
     1 = "Yes, last year"
     2 = "Yes, before last year"
     ;
  value F01156_
     0 = "No"
     1 = "Yes, last year"
     2 = "Yes, before last year"
     ;
  value F01157_
     0 = "No"
     1 = "Yes, last year"
     2 = "Yes, before last year"
     ;
  value F01158_
     0 = "No"
     1 = "Yes"
     ;
  value F01159_
     0 = "Never"
     1 = "Yes, last year"
     2 = "Yes, before last year"
     ;
  value F01160_
     0 = "Never"
     1 = "Yes, last year"
     2 = "Yes, before last year"
     ;
  value F01161_
     0 = "Never"
     1 = "Yes, last year"
     2 = "Yes, before last year"
     ;
  value F01162_
     0 = "Never"
     1 = "Yes, last year"
     2 = "Yes, before last year"
     ;
  value F01163_
     0 = "Never"
     1 = "Yes, last year"
     2 = "Yes, before last year"
     ;
  value F01164_
     0 = "Never"
     1 = "Yes, last year"
     2 = "Yes, before last year"
     ;
  value F01165_
     0 = "Never"
     1 = "Yes, last year"
     2 = "Yes, before last year"
     ;
  value F01166_
     0 = "Never"
     1 = "Yes, last year"
     2 = "Yes, before last year"
     ;
  value F01167_
     0 = "Never"
     1 = "Often"
     2 = "Sometimes"
     3 = "Yes, but not in the last 12 months"
     4 = "Yes, but frequency in last 12 months missing"
     ;
  value F01168_
     0 = "Never"
     1 = "Often"
     2 = "Sometimes"
     3 = "Yes, but not in the last 12 months"
     4 = "Yes, but frequency in last 12 months missing"
     ;
  value F01169_
     0 = "Never"
     1 = "Often"
     2 = "Sometimes"
     3 = "Yes, but not in the last 12 months"
     4 = "Yes, but frequency in last 12 months missing"
     ;
  value F01170_
     0 = "Never"
     1 = "Often"
     2 = "Sometimes"
     3 = "Yes, but not in the last 12 months"
     4 = "Yes, but frequency in last 12 months missing"
     ;
  value F01171_
     0 = "Never"
     1 = "Often"
     2 = "Sometimes"
     3 = "Yes, but not in the last 12 months"
     4 = "Yes, but frequency in last 12 months missing"
     ;
  value F01172_
     0 = "Never"
     1 = "Often"
     2 = "Sometimes"
     3 = "Yes, but not in the last 12 months"
     4 = "Yes, but frequency in last 12 months missing"
     ;
  value F01173_
     0 = "No"
     1 = "Yes"
     ;
  value F01174_
     0 = "No"
     1 = "Yes"
     ;
  value F01175_
     0 = "No"
     1 = "Yes"
     ;
  value F01176_
    95 = "Before union"
    96 = "After separation/divorce"
    98 = "Don't know"
     ;
  value F01177_
     0 = "No"
     1 = "Yes"
     8 = "Refuse to answer"
     ;
  value F01178_
     0 = "No"
     1 = "Yes"
     8 = "Refuse to answer"
     ;
  value F01179_
     0 = "No"
     1 = "Yes"
     8 = "Refuse to answer"
     ;
  value F01180_
     0 = "No"
     1 = "Yes"
     8 = "Refuse to answer"
     ;
  value F01181_
     0 = "No"
     1 = "Yes"
     8 = "Refuse to answer"
     ;
  value F01182_
     0 = "No"
     1 = "Yes"
     8 = "Refuse to answer"
     ;
  value F01183_
     0 = "No"
     1 = "Yes"
     8 = "Refuse to answer"
     ;
  value F01184_
     0 = "No"
     1 = "Yes"
     8 = "Refuse to answer"
     ;
  value F01185_
     0 = "No"
     1 = "Yes"
     ;
  value F01186_
     0 = "No"
     1 = "Yes"
     ;
  value F01187_
     0 = "Not at all in last 12 months"
     1 = "Often"
     2 = "Sometimes"
     8 = "Don't know"
     ;
  value F01188_
     0 = "No"
     1 = "Yes"
     ;
  value F01189_
     0 = "Never"
     1 = "Often"
     2 = "Sometimes"
     ;
  value F01190_
     0 = "No"
     1 = "Yes"
     8 = "Refuse to answer"
     ;
  value F01191_
     0 = "No"
     1 = "Yes"
     ;
  value F01192_
     0 = "No"
     1 = "Yes"
     8 = "Refuse to answer"
     ;
  value F01193_
     0 = "No"
     1 = "Yes"
     8 = "Refuse to answer"
     ;
  value F01194_
     0 = "No"
     1 = "Yes"
     8 = "Refuse to answer"
     ;
  value F01195_
     0 = "No"
     1 = "Yes"
     8 = "Refuse to answer"
     ;
  value F01196_
     0 = "No"
     1 = "Yes"
     8 = "Refuse to answer"
     ;
  value F01197_
     0 = "No"
     1 = "Yes"
     8 = "Refuse to answer"
     ;
  value F01198_
     0 = "No"
     1 = "Yes"
     8 = "Refuse to answer"
     ;
  value F01199_
     0 = "No"
     1 = "Yes"
     8 = "Refuse to answer"
     ;
  value F01200_
     0 = "No"
     1 = "Yes"
     8 = "Refuse to answer"
     ;
  value F01201_
     0 = "No"
     1 = "Yes"
     8 = "Refuse to answer"
     ;
  value F01202_
     0 = "No"
     1 = "Yes"
     8 = "Refuse to answer"
     ;
  value F01203_
     0 = "No"
     1 = "Yes"
     8 = "Refuse to answer"
     ;
  value F01204_
     0 = "No"
     1 = "Yes"
     8 = "Refuse to answer"
     ;
  value F01205_
     0 = "No"
     1 = "Yes"
     8 = "Refuse to answer"
     ;
  value F01206_
     0 = "No"
     1 = "Yes"
     8 = "Refuse to answer"
     ;
  value F01207_
     0 = "No"
     1 = "Yes"
     8 = "Refuse to answer"
     ;
  value F01208_
     0 = "No"
     1 = "Yes"
     8 = "Refuse to answer"
     ;
  value F01209_
     0 = "No"
     1 = "Yes"
     8 = "Refuse to answer"
     ;
  value F01210_
     0 = "No"
     1 = "Yes"
     8 = "Refuse to answer"
     ;
  value F01211_
     0 = "No"
     1 = "Yes"
     8 = "Refuse to answer"
     ;
  value F01212_
     0 = "No"
     1 = "Yes"
     8 = "Refuse to answer"
     ;
  value F01213_
     0 = "Someone physically hurt respondent"
     1 = "None of these people ever physically hurt respondent"
     6 = "No response to question"
     ;
  value F01214_
     0 = "No"
     1 = "Yes"
     ;
  value F01215_
     0 = "No"
     1 = "Yes"
     ;
  value F01216_
     0 = "No"
     1 = "Yes"
     ;
  value F01217_
     0 = "No"
     1 = "Yes"
     ;
  value F01218_
     0 = "No"
     1 = "Yes"
     ;
  value F01219_
     0 = "No"
     1 = "Yes"
     ;
  value F01220_
     0 = "No"
     1 = "Yes"
     ;
  value F01221_
     0 = "No"
     1 = "Yes"
     ;
  value F01222_
     0 = "No"
     1 = "Yes"
     8 = "Refuse to answer"
     ;
  value F01223_
     0 = "No"
     1 = "Yes"
     8 = "Refuse to answer"
     ;
  value F01224_
     0 = "No"
     1 = "Yes"
     ;
  value F01225_
     0 = "No one else ever physically hurt respondent"
     1 = "BLANK (Husband/partner skipped for this question)"
     2 = "Mother"
     3 = "Father"
     4 = "Daughter"
     5 = "Son"
     6 = "Sister"
     7 = "Brother"
     8 = "Other female relatives"
     9 = "Other male relatives"
    10 = "Former partner"
    11 = "Current boyfriend"
    12 = "Former boyfriend"
    13 = "Step-mother"
    14 = "Step-father"
    15 = "Mother-in-law"
    16 = "Father-in-law"
    17 = "Other female in-laws"
    18 = "Other male in-laws"
    19 = "Female friend, acquaintance"
    20 = "Male friend, acquaintance"
    21 = "Neighbor"
    22 = "Teacher"
    23 = "Employer"
    27 = "Stranger"
    28 = "Other female relative/in-law"
    29 = "Other male relative/in-law"
    30 = "Friend"
    31 = "Police"
    95 = "No response (D115Y=6)"
    96 = "Other"
     ;
  value F01226_
     0 = "Not at all"
     1 = "Often"
     2 = "Sometimes"
     ;
  value F01227_
     0 = "No"
     1 = "Yes"
     ;
  value F01228_
     0 = "No"
     1 = "Yes"
     ;
  value F01229_
     0 = "No"
     1 = "Yes"
     ;
  value F01230_
     0 = "No"
     1 = "Yes"
     ;
  value F01231_
     0 = "No"
     1 = "Yes"
     ;
  value F01232_
     0 = "No"
     1 = "Yes"
     ;
  value F01233_
     0 = "No"
     1 = "Yes"
     ;
  value F01234_
     0 = "No"
     1 = "Yes"
     ;
  value F01235_
     0 = "No"
     1 = "Yes"
     ;
  value F01236_
     0 = "No"
     1 = "Yes"
     ;
  value F01237_
     0 = "No"
     1 = "Yes"
     ;
  value F01238_
     0 = "No"
     1 = "Yes"
     ;
  value F01239_
     0 = "No"
     1 = "Yes"
     ;
  value F01240_
     0 = "No"
     1 = "Yes"
     ;
  value F01241_
     0 = "No"
     1 = "Yes"
     ;
  value F01242_
     0 = "No"
     1 = "Yes"
     ;
  value F01243_
     0 = "No"
     1 = "Yes"
     ;
  value F01244_
     0 = "No"
     1 = "Yes"
     ;
  value F01245_
     0 = "No"
     1 = "Yes"
     ;
  value F01246_
     0 = "No"
     1 = "Yes"
     ;
  value F01247_
     0 = "No"
     1 = "Yes"
     ;
  value F01248_
     0 = "No"
     1 = "Yes"
     ;
  value F01249_
     0 = "No"
     1 = "Yes"
     ;
  value F01250_
     0 = "No"
     1 = "Yes"
     ;
  value F01251_
     0 = "Someone hurt respondent during pregnancy"
     1 = "No one hurt respondent during pregnancy"
     ;
  value F01252_
     0 = "No"
     1 = "Yes"
     ;
  value F01253_
     0 = "No"
     1 = "Yes"
     ;
  value F01254_
     0 = "No"
     1 = "Yes"
     ;
  value F01255_
     0 = "No"
     1 = "Yes"
     ;
  value F01256_
     0 = "No"
     1 = "Yes"
     ;
  value F01257_
     0 = "No"
     1 = "Yes"
     ;
  value F01258_
     0 = "No"
     1 = "Yes"
     ;
  value F01259_
     0 = "No"
     1 = "Yes"
     ;
  value F01260_
     0 = "No"
     1 = "Yes"
     ;
  value F01261_
     0 = "No"
     1 = "Yes"
     ;
  value F01262_
     0 = "No"
     1 = "Yes"
     ;
  value F01263_
     0 = "No"
     1 = "Yes"
     ;
  value F01264_
     0 = "No"
     1 = "Yes"
     ;
  value F01265_
     0 = "No"
     1 = "Yes"
     ;
  value F01266_
     0 = "No"
     1 = "Yes"
     ;
  value F01267_
     0 = "No"
     1 = "Yes"
     ;
  value F01268_
     0 = "No"
     1 = "Yes"
     ;
  value F01269_
     0 = "No"
     1 = "Yes"
     ;
  value F01270_
     0 = "No"
     1 = "Yes"
     ;
  value F01271_
     0 = "No"
     1 = "Yes"
     ;
  value F01272_
     0 = "No"
     1 = "Yes"
     ;
  value F01273_
     0 = "No"
     1 = "Yes"
     ;
  value F01274_
     0 = "No"
     1 = "Yes"
     ;
  value F01275_
     0 = "No"
     1 = "Yes"
     ;
  value F01276_
     0 = "No"
     1 = "Yes"
     ;
  value F01277_
     0 = "No"
     1 = "Yes"
     ;
  value F01278_
     0 = "No"
     1 = "Yes"
     ;
  value F01279_
     0 = "No"
     1 = "Yes"
     ;
  value F01280_
     0 = "No"
     1 = "Yes"
     ;
  value F01281_
     0 = "No"
     1 = "Yes"
     ;
  value F01282_
     0 = "No"
     1 = "Yes"
     ;
  value F01283_
     0 = "No"
     1 = "Yes"
     ;
  value F01284_
     0 = "No"
     1 = "Yes"
     ;
  value F01285_
     0 = "No"
     1 = "Yes"
     ;
  value F01286_
     0 = "No"
     1 = "Yes"
     ;
  value F01287_
     0 = "Sought help from someone"
     1 = "No help was sought"
     ;
  value F01288_
     0 = "No"
     1 = "Yes"
     ;
  value F01289_
     0 = "No"
     1 = "Yes"
     ;
  value F01290_
     0 = "No"
     1 = "Yes"
     ;
  value F01291_
     0 = "No"
     1 = "Yes"
     ;
  value F01292_
     0 = "No"
     1 = "Yes"
     ;
  value F01293_
     0 = "No"
     1 = "Yes"
     ;
  value F01294_
     0 = "No"
     1 = "Yes"
     ;
  value F01295_
     0 = "No"
     1 = "Yes"
     ;
  value F01296_
     0 = "No"
     1 = "Yes"
     ;
  value F01297_
     0 = "No"
     1 = "Yes"
     ;
  value F01298_
     0 = "No"
     1 = "Yes"
     ;
  value F01299_
     1 = "Don't know where to go to"
     2 = "No use"
     3 = "Part of life"
     4 = "Afraid of divorce"
     5 = "Afraid of further beatings"
     6 = "Afraid of getting the person in trouble"
     7 = "Embarrassed"
     8 = "Don't want to disgrace family"
     9 = "No money"
    96 = "Other"
     ;
  value F01300_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F01301_
     0 = "No"
     1 = "Yes, once"
     2 = "Yes, more than once"
     ;
  value F01302_
     0 = "No"
     1 = "Yes, once"
     2 = "Yes, more than once"
     ;
  value F01303_
     0 = "No"
     1 = "Yes, once"
     2 = "Yes, more than once"
     ;
  value F01304_
     1 = "Wanted"
     2 = "Forced"
     6 = "Refused to answer/no response"
     ;
  value F01305_
     0 = "No"
     1 = "Yes"
     8 = "Refused to answer/no response"
     ;
  value F01306_
     0 = "No"
     1 = "Yes"
     6 = "Refused to answer/no response"
     ;
  value F01307_
    98 = "Don't know"
     ;
  value F01308_
     1 = "Current husband/partner"
     2 = "Former husband/partner"
     3 = "Current/former boyfriend"
     4 = "Father/step father"
     5 = "Brother/step brother"
     6 = "Other relative"
     7 = "In-law"
     8 = "Own friend/acquaintance"
     9 = "Family friend"
    10 = "Teacher"
    11 = "Employer/someone at work"
    12 = "Police/soldier"
    13 = "Priest/religious leader"
    14 = "Stranger"
    96 = "Other"
     ;
  value F01309_
     0 = "No"
     1 = "Yes"
     ;
  value F01310_
     0 = "Never afraid"
     1 = "Most of the time afraid"
     2 = "Sometimes afraid"
     ;
  value F01311_
     0 = "Never"
     1 = "0-11 months ago"
     2 = "12+ months ago"
     3 = "Yes, but don't remember when"
     4 = "Yes, but frequency in last 12 months missing"
     ;
  value F01312_
     0 = "Never"
     1 = "0-11 months ago"
     2 = "12+ months ago"
     3 = "Yes, but don't remember when"
     4 = "Yes, but frequency in last 12 months missing"
     ;
  value F01313_
     0 = "Never"
     1 = "0-11 months ago"
     2 = "12+ months ago"
     3 = "Yes, but don't remember when"
     4 = "Yes, but frequency in last 12 months missing"
     ;
  value F01314_
    91 = "Amazonas"
     5 = "Antioquia"
    81 = "Arauca"
     8 = "Atl?ntico"
    11 = "Bogot?"
    13 = "Bol?var"
    15 = "Boyac?"
    17 = "Caldas"
    18 = "Caquet?"
    85 = "Casanare"
    19 = "Cauca"
    20 = "Cesar"
    27 = "Choc?"
    23 = "C?rdoba"
    25 = "Cundinamarca"
    94 = "Guain?a"
    95 = "Guaviare"
    41 = "Huila"
    44 = "La Guajira"
    47 = "Magdalena"
    50 = "Meta"
    52 = "Nari?o"
    54 = "Norte de Santander"
    86 = "Putumayo"
    63 = "Quind?o"
    66 = "Risaralda"
    88 = "San Andr?s y Providencia"
    68 = "Santander"
    70 = "Sucre"
    73 = "Tolima"
    76 = "Valle"
    97 = "Vaup?s"
    99 = "Vichada"
     ;
  value F01315_
     1 = "City urban area"
     2 = "Small village"
     3 = "Disperse population"
     ;
  value F01316_
     1 = "Guajira, Cesar, Magdalena"
     2 = "Barranquilla A. M."
     3 = "Atlantico, San Andres, Bolivar Norte"
     4 = "Bolivar Sur, Sucre, Cordoba"
     5 = "Santanderes"
     6 = "Boyaca, Cmarca, Meta"
     7 = "Medellin A.M."
     8 = "Antioquia sin Medellin"
     9 = "Caldas, Risaralda, Quindio"
    10 = "Tolima, Huila, Caqueta"
    11 = "Cali A.M."
    12 = "Valle sin Cali ni Litoral"
    13 = "Cauca y Nari?o sin Litoral"
    14 = "Litoral Pacifico"
    15 = "Bogota"
    16 = "Orinoquia y Amazonia"
     ;
  value F01317_
     1 = "Male"
     2 = "Female"
     3 = "Male transgender"
     4 = "Female transgender"
     ;
  value F01318_
   100 = "0 Months"
   101 = "1 Month"
   200 = "0 Years"
   201 = "1 Years"
   995 = "Always"
     ;
  value F01319_
     0 = "No education/None"
     1 = "Preschool"
     2 = "Primary"
     3 = "Secondary (high school)"
     4 = "Middle academy or classic (basic high school)"
     5 = "Middle technical (technical high school)"
     6 = "Technical professional (after high school)"
     7 = "Technological"
     8 = "Professional (towards bachelor degree)"
     9 = "Post-graduate training"
    10 = "Master's degree studies"
    11 = "PHD"
    98 = "Don't know"
     ;
  value F01320_
    98 = "Don't know"
     ;
  value F01321_
     0 = "No"
     1 = "Yes"
     ;
  value F01322_
     0 = "No"
     1 = "Yes"
     ;
  value F01323_
     0 = "Less than one year"
     ;
  value F01324_
     1 = "Got pregnant"
     2 = "Got married"
     3 = "Had to take care of the children"
     4 = "Family needed help"
     5 = "Couldn't pay tuition"
     6 = "Had to earn money"
     7 = "Graduated/enough studies"
     8 = "Didn't want to study"
     9 = "Too many requirements/paper work"
    96 = "Other"
    98 = "Don't know"
     ;
  value F01325_
     1 = "Married"
     2 = "Living with partner"
     3 = "Separated"
     4 = "Divorced"
     5 = "Widowed"
     6 = "Never in union"
     7 = "Deceased"
     8 = "Don't know"
     ;
  value F01326_
     1 = "Married"
     2 = "Living with partner"
     3 = "Separated"
     4 = "Divorced"
     5 = "Widowed"
     6 = "Never in union"
     7 = "Deceased"
     8 = "Don't know"
     ;
  value F01327_
     1 = "Married"
     2 = "Living with partner"
     3 = "Separated"
     4 = "Divorced"
     5 = "Widowed"
     6 = "Never in union"
     7 = "Deceased"
     8 = "Don't know"
     ;
  value F01328_
     1 = "Married"
     2 = "Living with partner"
     3 = "Separated"
     4 = "Divorced"
     5 = "Widowed"
     6 = "Never in union"
     7 = "Deceased"
     8 = "Don't know"
     ;
  value F01329_
     1 = "Biological father and mother"
     2 = "Only with biological mother"
     3 = "Only with biological father"
     4 = "Biological mother and stepfather"
     5 = "Biological father and stepmother"
     6 = "Others different from father and mother"
     7 = "Alone"
     ;
  value F01330_
     1 = "Biological father and mother"
     2 = "Only with biological mother"
     3 = "Only with biological father"
     4 = "Biological mother and stepfather"
     5 = "Biological father and stepmother"
     6 = "Others different from father and mother"
     7 = "Alone"
     ;
  value F01331_
    98 = "Don't know"
     ;
  value F01332_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F01333_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F01334_
    98 = "Don't know"
     ;
  value F01335_
    98 = "Don't know"
     ;
  value F01336_
     0 = "No"
     1 = "Yes"
     3 = "Don't have older brothers/sisters"
     8 = "Don't know"
     ;
  value F01337_
     1 = "Always"
     2 = "Almost always"
     3 = "Few times"
     4 = "Never"
     7 = "Not applicable (NA)"
     ;
  value F01338_
     1 = "Always"
     2 = "Almost always"
     3 = "Few times"
     4 = "Never"
     7 = "Not applicable (NA)"
     ;
  value F01339_
     1 = "Always"
     2 = "Almost always"
     3 = "Few times"
     4 = "Never"
     7 = "Not applicable (NA)"
     ;
  value F01340_
     1 = "Always"
     2 = "Almost always"
     3 = "Few times"
     4 = "Never"
     7 = "Not applicable (NA)"
     ;
  value F01341_
     1 = "None of them"
     2 = "Some of them"
     3 = "A lot of them"
     4 = "All of them"
     8 = "Don't know"
     ;
  value F01342_
     1 = "None of them"
     2 = "Some of them"
     3 = "A lot of them"
     4 = "All of them"
     8 = "Don't know"
     ;
  value F01343_
     1 = "None of them"
     2 = "Some of them"
     3 = "A lot of them"
     4 = "All of them"
     8 = "Don't know"
     ;
  value F01344_
    98 = "Don't know"
     ;
  value F01345_
    98 = "Don't know"
     ;
  value F01346_
    98 = "Don't know"
     ;
  value F01347_
    98 = "Don't know"
     ;
  value F01348_
     0 = "No"
     1 = "Yes"
     ;
  value F01349_
    98 = "Don't know"
     ;
  value F01350_
    98 = "Don't know"
     ;
  value F01351_
     0 = "No"
     1 = "Yes"
     ;
  value F01352_
     0 = "No"
     1 = "Yes"
     ;
  value F01353_
     0 = "No prenatal control"
    10 = "PUBLIC"
    11 = "Government hospital/health center/health post"
    20 = "PRIVATE MEDICAL"
    21 = "Private hospital/clinic"
    22 = "EPS health center"
    23 = "Private doctor"
    24 = "PROFAMILIA"
    96 = "OTHER"
    98 = "DK"
     ;
  value F01354_
     0 = "No"
     1 = "Yes"
     ;
  value F01355_
     0 = "No"
     1 = "Yes"
     ;
  value F01356_
     0 = "No"
     1 = "Yes"
     ;
  value F01357_
     0 = "No"
     1 = "Yes"
     ;
  value F01358_
     0 = "No"
     1 = "Yes"
     ;
  value F01359_
     0 = "No"
     1 = "Yes"
     ;
  value F01360_
     0 = "No"
     1 = "Yes"
     ;
  value F01361_
     0 = "No"
     1 = "Yes"
     ;
  value F01362_
     0 = "No"
     1 = "Yes"
     ;
  value F01363_
     0 = "Less than 1 day"
   995 = "Wasn't hospitalized"
     ;
  value F01364_
     0 = "No"
     1 = "Yes"
     ;
  value F01365_
     0 = "No"
     1 = "Yes"
     ;
  value F01366_
     0 = "No"
     1 = "Yes"
     ;
  value F01367_
     0 = "No"
     1 = "Yes"
     ;
  value F01368_
     0 = "No"
     1 = "Yes"
     ;
  value F01369_
     0 = "No"
     1 = "Yes"
     ;
  value F01370_
     0 = "No"
     1 = "Yes"
     ;
  value F01371_
     0 = "No"
     1 = "Yes"
     ;
  value F01372_
    98 = "Don't know/don't remember"
     ;
  value F01373_
    98 = "Don't know/don't remember"
     ;
  value F01374_
     1 = "Termination/spontaneous abortion"
     2 = "Interruption/induced abortion"
     3 = "Ectopic pregnancy"
     4 = "Fetal death/born dead"
     ;
  value F01375_
     0 = "No prenatal control"
    10 = "PUBLIC"
    11 = "Government hospital/health center/health post"
    20 = "PRIVATE MEDICAL"
    21 = "Private hospital/clinic"
    22 = "EPS health center"
    23 = "Private doctor"
    24 = "PROFAMILIA"
    96 = "OTHER"
    98 = "DK"
     ;
  value F01376_
    96 = "Never menstruated"
     ;
  value F01377_
     1 = "Family"
     2 = "School public sector"
     3 = "School private sector"
     4 = "Public hospital/health center "
     5 = "Private clínica/IPS"
     6 = "Acquaintance/friend"
     7 = "Community"
     8 = "Media"
     9 = "Drugstore/pharmacy"
    96 = "Other"
     ;
  value F01378_
     1 = "Based on calendar"
     2 = "Based on body temperature"
     3 = "Based on cervical mucous"
     4 = "Based on body temperature and cervical mucous"
     5 = "No specific system"
     6 = "Other"
     ;
  value F01379_
     0 = "No"
     1 = "Yes"
     ;
  value F01380_
     1 = "Wanted another child"
     2 = "Partner wanted another child"
     3 = "Side effects"
     4 = "Child died"
     6 = "Other"
     ;
  value F01381_
     0 = "No"
     1 = "Yes"
     ;
  value F01382_
     0 = "No"
     1 = "Yes"
     ;
  value F01383_
     0 = "No"
     1 = "Yes"
     ;
  value F01384_
     0 = "No"
     1 = "Yes"
     ;
  value F01385_
     0 = "No"
     1 = "Yes"
     ;
  value F01386_
     0 = "No"
     1 = "Yes"
     ;
  value F01387_
     0 = "No"
     1 = "Yes"
     ;
  value F01388_
     0 = "No"
     1 = "Yes"
     ;
  value F01389_
     0 = "No"
     1 = "Yes"
     ;
  value F01390_
     1 = "Heterosexual"
     2 = "Homosexual"
     3 = "Bisexual"
     ;
  value F01391_
    98 = "Don't know"
     ;
  value F01392_
  9998 = "Don't know"
     ;
  value F01393_
     1 = "Male"
     2 = "Female"
     3 = "Male transexual"
     4 = "Female transexual"
     ;
  value F01394_
    95 = "95+"
    98 = "Don't know"
     ;
  value F01395_
     1 = "Married"
     2 = "Living together"
     ;
  value F01396_
     0 = "No"
     1 = "Yes"
     ;
  value F01397_
    98 = "Don't know"
     ;
  value F01398_
  9998 = "Don't know"
     ;
  value F01399_
     0 = "No"
     1 = "Yes"
     ;
  value F01400_
    98 = "Don't know"
     ;
  value F01401_
  9998 = "Don't know"
     ;
  value F01402_
     1 = "Became widow"
     2 = "Separated"
     3 = "Got divorced"
     ;
  value F01403_
     0 = "No"
     1 = "Yes"
     ;
  value F01404_
     0 = "No"
     1 = "Yes"
     8 = "Refuse to answer"
     ;
  value F01405_
     0 = "No"
     1 = "Yes"
     ;
  value F01406_
     0 = "No"
     1 = "Yes"
     ;
  value F01407_
     1 = "No pressure"
     2 = "Pressure from half of female friends"
     3 = "Pressure from more than half of female friends"
     4 = "A lot of pressure (from all friends)"
     ;
  value F01408_
     1 = "No pressure"
     2 = "Pressure from half of female friends"
     3 = "Pressure from more than half of female friends"
     4 = "A lot of pressure (from all friends)"
     ;
  value F01409_
     1 = "Male"
     2 = "Female"
     3 = "Transgender"
     ;
  value F01410_
     1 = "10 or more years older"
     2 = "Less than 10 years older"
     8 = "Older, don't know difference"
     ;
  value F01411_
     1 = "10 or more years younger"
     2 = "Less than 10 years younger"
     8 = "Younger, don't know difference"
     ;
  value F01412_
     1 = "Stable couple"
     2 = "It is uncomfortable"
     3 = "Use other method"
     4 = "Partner don't like to use it"
     5 = "Difficult to obtain"
     6 = "Believe is not safe"
     7 = "Goes against religion"
     8 = "Trust partner"
     9 = "Wants children"
    10 = "Infecund/pregnant"
    96 = "Other"
     ;
  value F01413_
     1 = "Stable couple"
     2 = "It is uncomfortable"
     3 = "Use other method"
     4 = "Partner don't like to use it"
     5 = "Difficult to obtain"
     6 = "Believe is not safe"
     7 = "Goes against religion"
     8 = "Trust partner"
     9 = "Wants children"
    10 = "Infecund/pregnant"
    96 = "Other"
     ;
  value F01414_
     1 = "Stable couple"
     2 = "It is uncomfortable"
     3 = "Use other method"
     4 = "Partner don't like to use it"
     5 = "Difficult to obtain"
     6 = "Believe is not safe"
     7 = "Goes against religion"
     8 = "Trust partner"
     9 = "Wants children"
    10 = "Infecund/pregnant"
    96 = "Other"
     ;
  value F01415_
     1 = "Always"
     2 = "In some cases"
     3 = "Never"
     7 = "Refuse to answer"
     8 = "Don't know"
     ;
  value F01416_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F01417_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F01418_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F01419_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F01420_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F01421_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F01422_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F01423_
     0 = "No, shouldn't be permitted"
     1 = "Yes, No, should be permitted"
     8 = "Don't know"
     ;
  value F01424_
     0 = "No, shouldn't be permitted"
     1 = "Yes, No, should be permitted"
     8 = "Don't know"
     ;
  value F01425_
     0 = "No, shouldn't be permitted"
     1 = "Yes, No, should be permitted"
     8 = "Don't know"
     ;
  value F01426_
     0 = "No, shouldn't be permitted"
     1 = "Yes, No, should be permitted"
     8 = "Don't know"
     ;
  value F01427_
     0 = "No, shouldn't be permitted"
     1 = "Yes, No, should be permitted"
     8 = "Don't know"
     ;
  value F01428_
     0 = "No, shouldn't be permitted"
     1 = "Yes, No, should be permitted"
     8 = "Don't know"
     ;
  value F01429_
     0 = "No, shouldn't be permitted"
     1 = "Yes, No, should be permitted"
     8 = "Don't know"
     ;
  value F01430_
     0 = "No, shouldn't be permitted"
     1 = "Yes, No, should be permitted"
     8 = "Don't know"
     ;
  value F01431_
     0 = "No, shouldn't be permitted"
     1 = "Yes, No, should be permitted"
     8 = "Don't know"
     ;
  value F01432_
     0 = "No, shouldn't be permitted"
     1 = "Yes, No, should be permitted"
     8 = "Don't know"
     ;
  value F01433_
     0 = "No, shouldn't be permitted"
     1 = "Yes, No, should be permitted"
     8 = "Don't know"
     ;
  value F01434_
     0 = "No, shouldn't be permitted"
     1 = "Yes, No, should be permitted"
     8 = "Don't know"
     ;
  value F01435_
     0 = "No"
     1 = "Yes"
     3 = "Never pregnant/never had sex"
     ;
  value F01436_
     0 = "No"
     1 = "Yes"
     ;
  value F01437_
     1 = "Life threatening"
     2 = "Respondent's physical health was at risk"
     3 = "Respondent's mental health was at risk"
     4 = "Respondent was raped"
     5 = "Fetus malformation"
     6 = "Other"
     ;
  value F01438_
     0 = "No"
     1 = "Yes"
     ;
  value F01439_
     0 = "No looking for pregnancy anymore"
     1 = "Before November 2010"
     2 = "Since November 2010"
     ;
  value F01440_
     0 = "No"
     1 = "Yes"
     ;
  value F01441_
     1 = "Couple's problem"
     2 = "Partner problem"
     3 = "Respondent problem"
     4 = "Illness"
     6 = "Other"
     8 = "Don't know"
     ;
  value F01442_
     1 = "Yes, in treatment"
     2 = "No, too expensive"
     3 = "No, religion prohibits"
     4 = "No, partner opposes"
     6 = "Other"
     ;
  value F01443_
     1 = "Personal reasons"
     2 = "Health services related reasons"
     3 = "Economic reasons"
     4 = "Authorization problems"
     5 = "Lack of knowledge"
     6 = "Opposition"
     7 = "Wasn't affiliated to health insurance"
     8 = "Didn't know that could use service"
    96 = "Other"
     ;
  value F01444_
     1 = "Approve"
     2 = "Disapprove"
     3 = "No opinion"
     ;
  value F01445_
     0 = "No"
     1 = "Yes"
     ;
  value F01446_
     0 = "No"
     1 = "Yes"
     ;
  value F01447_
     0 = "Nothing, saves everything"
     1 = "Almost nothing"
     2 = "Less than half"
     3 = "Half"
     4 = "More than half"
     5 = "All income"
     ;
  value F01448_
     0 = "No one"
     1 = "Respondent alone"
     2 = "Respondent and husband/partner"
     3 = "Respondent and other person"
     4 = "Husband/partner alone"
     5 = "Someone else"
     6 = "Other"
     ;
  value F01449_
     0 = "No one"
     1 = "Respondent alone"
     2 = "Respondent and husband/partner"
     3 = "Respondent and other person"
     4 = "Husband/partner alone"
     5 = "Someone else"
     6 = "Other"
     ;
  value F01450_
     0 = "Does not own"
     1 = "Alone only"
     2 = "Jointly only"
     3 = "Both alone and jointly"
     ;
  value F01451_
     0 = "Does not own"
     1 = "Alone only"
     2 = "Jointly only"
     3 = "Both alone and jointly"
     ;
  value F01452_
     0 = "Does not own"
     1 = "Alone only"
     2 = "Jointly only"
     3 = "Both alone and jointly"
     ;
  value F01453_
     0 = "Does not own"
     1 = "Alone only"
     2 = "Jointly only"
     3 = "Both alone and jointly"
     ;
  value F01454_
     1 = "Between 5 to 9 years"
     2 = "Between 10 to 20 years"
     3 = "More than 20 years"
     4 = "Like any other person"
     8 = "Don't know"
     ;
  value F01455_
     1 = "homosexual man that always use condoms"
     2 = "Women that don't tell their partners to use condoms"
     3 = "Female sexual workers that protect themselves"
     4 = "Drug users that don't share injection tools"
     8 = "Don't know"
     ;
  value F01456_
     1 = "Disagrees"
     2 = "Neither agrees nor disagrees"
     3 = "Agrees"
     ;
  value F01457_
     1 = "Disagrees"
     2 = "Neither agrees nor disagrees"
     3 = "Agrees"
     ;
  value F01458_
     1 = "Disagrees"
     2 = "Neither agrees nor disagrees"
     3 = "Agrees"
     ;
  value F01459_
     1 = "Disagrees"
     2 = "Neither agrees nor disagrees"
     3 = "Agrees"
     ;
  value F01460_
     0 = "No"
     1 = "Yes"
     8 = "Don't know/not sure"
     ;
  value F01461_
     0 = "No"
     1 = "Yes"
     8 = "Don't know/not sure"
     ;
  value F01462_
     0 = "No"
     1 = "Yes"
     8 = "Don't know/not sure"
     ;
  value F01463_
     1 = "Respondent requested test"
     2 = "Test was offered and respondent accepted"
     3 = "Was ordered to take the test and had to do it"
     ;
  value F01464_
     0 = "Never"
     1 = "Every 3 months"
     2 = "Every 6 months"
     3 = "Every year"
     4 = "Every 5 years"
     5 = "Never"
     6 = "Other"
     8 = "Don't know"
     ;
  value F01465_
     0 = "No"
     1 = "Yes"
     8 = "Don't know/not sure"
     ;
  value F01466_
     0 = "No"
     1 = "Yes"
     ;
  value F01467_
     0 = "No"
     1 = "Yes"
     ;
  value F01468_
     0 = "No"
     1 = "Yes"
     ;
  value F01469_
     0 = "No"
     1 = "Yes"
     ;
  value F01470_
     0 = "No"
     1 = "Yes"
     ;
  value F01471_
     0 = "No"
     1 = "Yes"
     ;
  value F01472_
     0 = "No"
     1 = "Yes"
     ;
  value F01473_
     0 = "No"
     1 = "Yes"
     ;
  value F01474_
     0 = "No"
     1 = "Yes"
     ;
  value F01475_
     0 = "No"
     1 = "Yes"
     ;
  value F01476_
     0 = "No"
     1 = "Yes"
     ;
  value F01477_
     0 = "No"
     1 = "Yes"
     ;
  value F01478_
     0 = "No"
     1 = "Yes"
     ;
  value F01479_
     0 = "No"
     1 = "Yes"
     ;
  value F01480_
     0 = "No"
     1 = "Yes"
     ;
  value F01481_
     0 = "No"
     1 = "Yes"
     ;
  value F01482_
     0 = "No"
     1 = "Yes"
     ;
  value F01483_
     0 = "No"
     1 = "Yes"
     ;
  value F01484_
     0 = "No"
     1 = "Yes"
     ;
  value F01485_
     0 = "No"
     1 = "Yes"
     ;
  value F01486_
     0 = "No"
     1 = "Yes"
     ;
  value F01487_
     0 = "No"
     1 = "Yes"
     ;
  value F01488_
     0 = "No"
     1 = "Yes"
     ;
  value F01489_
     0 = "No"
     1 = "Yes"
     ;
  value F01490_
     0 = "No"
     1 = "Yes"
     ;
  value F01491_
     0 = "No"
     1 = "Yes"
     ;
  value F01492_
     0 = "No"
     1 = "Yes"
     ;
  value F01493_
     0 = "No"
     1 = "Yes"
     ;
  value F01494_
     0 = "No"
     1 = "Yes"
     2 = "Some/not all"
     3 = "No partner"
     ;
  value F01495_
     0 = "No"
     1 = "Yes"
     2 = "Partner already infect"
     ;
  value F01496_
     0 = "No"
     1 = "Yes"
     ;
  value F01497_
     0 = "No"
     1 = "Yes"
     ;
  value F01498_
     0 = "No"
     1 = "Yes"
     ;
  value F01499_
     0 = "No"
     1 = "Yes"
     2 = "Hasn't consulted with a doctor"
     3 = "No partner"
     ;
  value F01500_
     1 = "Public"
     2 = "Private"
     3 = "Both public/private"
     ;
  value F01501_
     1 = "Public"
     2 = "Private"
     3 = "Both public/private"
     ;
  value F01502_
     1 = "Public"
     2 = "Private"
     3 = "Both public/private"
     ;
  value F01503_
     1 = "Public"
     2 = "Private"
     3 = "Both public/private"
     ;
  value F01504_
     1 = "Public"
     2 = "Private"
     3 = "Both public/private"
     ;
  value F01505_
     1 = "Public"
     2 = "Private"
     3 = "Both public/private"
     ;
  value F01506_
     1 = "Public"
     2 = "Private"
     3 = "Both public/private"
     ;
  value F01507_
     1 = "Public"
     2 = "Private"
     3 = "Both public/private"
     ;
  value F01508_
     1 = "Public"
     2 = "Private"
     3 = "Both public/private"
     ;
  value F01509_
     1 = "Public"
     2 = "Private"
     3 = "Both public/private"
     ;
  value F01510_
     1 = "Public"
     2 = "Private"
     3 = "Both public/private"
     ;
  value F01511_
     1 = "Public"
     2 = "Private"
     3 = "Both public/private"
     ;
  value F01512_
     1 = "Public"
     2 = "Private"
     3 = "Both public/private"
     ;
  value F01513_
     1 = "Public"
     2 = "Private"
     3 = "Both public/private"
     ;
  value F01514_
     1 = "Public"
     2 = "Private"
     3 = "Both public/private"
     ;
  value F01515_
     1 = "Public"
     2 = "Private"
     3 = "Both public/private"
     ;
  value F01516_
     0 = "No"
     1 = "Yes, last year"
     2 = "Yes, before last year"
     ;
  value F01517_
     0 = "No"
     1 = "Yes, last year"
     2 = "Yes, before last year"
     ;
  value F01518_
     0 = "No"
     1 = "Yes, last year"
     2 = "Yes, before last year"
     ;
  value F01519_
     0 = "No"
     1 = "Yes, last year"
     2 = "Yes, before last year"
     ;
  value F01520_
     1 = "Public"
     2 = "Private"
     3 = "Both public/private"
     ;
  value F01521_
     1 = "Public"
     2 = "Private"
     3 = "Both public/private"
     ;
  value F01522_
     1 = "Public"
     2 = "Private"
     3 = "Both public/private"
     ;
  value F01523_
     1 = "Public"
     2 = "Private"
     3 = "Both public/private"
     ;
  value F01524_
     0 = "No"
     1 = "Yes"
     ;
  value F01525_
     0 = "No"
     1 = "Yes"
     2 = "Never pregnant"
     ;
  value F01526_
     0 = "No"
     1 = "Yes"
     ;
  value F01527_
     0 = "No"
     1 = "Yes"
     ;
  value F01528_
     0 = "No"
     1 = "Yes"
     2 = "Never pregnant"
     ;
  value F01529_
     0 = "No"
     1 = "Yes"
     ;
  value F01530_
     0 = "No"
     1 = "Yes"
     8 = "Refuse to answer"
     ;
  value F01531_
     0 = "No"
     1 = "Yes"
     8 = "Refuse to answer"
     ;
  value F01532_
     0 = "No"
     1 = "Yes"
     8 = "Refuse to answer"
     ;
  value F01533_
     0 = "No"
     1 = "Yes"
     8 = "Refuse to answer"
     ;
  value F01534_
     0 = "No"
     1 = "Yes"
     8 = "Refuse to answer"
     ;
  value F01535_
     0 = "No"
     1 = "Yes"
     8 = "Refuse to answer"
     ;
  value F01536_
     0 = "No"
     1 = "Yes"
     8 = "Refuse to answer"
     ;
  value F01537_
     0 = "No"
     1 = "Yes"
     8 = "Refuse to answer"
     ;
  value F01538_
     0 = "No"
     1 = "Yes"
     ;
  value F01539_
     1 = "Domestic violence"
     2 = "Husband unfaithful"
     3 = "Doesn't comply with his duties as father/husband"
     4 = "Regularly drunk"
     5 = "Use narcotic drugs"
     6 = "Mutual agreement"
     7 = "Different sexuality needs"
    96 = "Other"
     ;
  value F01540_
     0 = "Never"
     1 = "Yes, some time"
     2 = "Yes, last year"
     ;
  value F01541_
     0 = "No"
     1 = "Yes"
     ;
  value F01542_
     0 = "No"
     1 = "Yes"
     ;
  value F01543_
     0 = "No"
     1 = "Yes"
     ;
  value F01544_
     0 = "No"
     1 = "Yes"
     ;
  value F01545_
     0 = "No"
     1 = "Yes"
     ;
  value F01546_
     0 = "No"
     1 = "Yes, never done anything"
     ;
  value F01547_
     0 = "Not at all in last 12 months"
     1 = "Often"
     2 = "Sometimes"
     8 = "Don't know"
     ;
  value F01548_
     0 = "No"
     1 = "Yes"
     ;
  value F01549_
     0 = "No"
     1 = "Yes"
     ;
  value F01550_
     0 = "No"
     1 = "Yes"
     ;
  value F01551_
     0 = "No"
     1 = "Yes"
     ;
  value F01552_
     0 = "No"
     1 = "Yes"
     ;
  value F01553_
     0 = "No"
     1 = "Yes"
     ;
  value F01554_
     0 = "No"
     1 = "Yes"
     ;
  value F01555_
     0 = "No"
     1 = "Yes"
     ;
  value F01556_
     0 = "No"
     1 = "Yes"
     ;
  value F01557_
     0 = "No"
     1 = "Yes"
     ;
  value F01558_
     0 = "No"
     1 = "Yes"
     ;
  value F01559_
     0 = "No"
     1 = "Yes"
     ;
  value F01560_
     0 = "No"
     1 = "Yes"
     ;
  value F01561_
     0 = "No"
     1 = "Yes"
     ;
  value F01562_
     0 = "No"
     1 = "Yes"
     ;
  value F01563_
     0 = "No"
     1 = "Yes, parents didn't punish respondent"
     ;
  value F01564_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F01565_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F01566_
     0 = "No"
     1 = "Yes"
     ;
  value F01567_
     0 = "No"
     1 = "Yes"
     ;
  value F01568_
     0 = "No"
     1 = "Yes"
     ;
  value F01569_
     0 = "No"
     1 = "Yes"
     ;
  value F01570_
     0 = "No"
     1 = "Yes, nobody/never punish children"
     ;
  value F01571_
     0 = "No"
     1 = "Yes"
     ;
  value F01572_
     0 = "No"
     1 = "Yes"
     ;
  value F01573_
     0 = "No"
     1 = "Yes"
     ;
  value F01574_
     0 = "No"
     1 = "Yes"
     ;
  value F01575_
     0 = "No"
     1 = "Yes"
     ;
  value F01576_
     0 = "No"
     1 = "Yes"
     ;
  value F01577_
     0 = "No"
     1 = "Yes"
     ;
  value F01578_
     0 = "No"
     1 = "Yes"
     ;
  value F01579_
     0 = "No"
     1 = "Yes"
     ;
  value F01580_
     0 = "No"
     1 = "Yes"
     ;
  value F01581_
     0 = "No"
     1 = "Yes"
     ;
  value F01582_
     0 = "No"
     1 = "Yes"
     ;
  value F01583_
     0 = "No"
     1 = "Yes"
     ;
  value F01584_
     0 = "No"
     1 = "Yes"
     ;
  value F01585_
     0 = "No"
     1 = "Yes"
     ;
  value F01586_
     0 = "No"
     1 = "Yes"
     ;
  value F01587_
     0 = "No"
     1 = "Yes"
     ;
  value F01588_
     0 = "No"
     1 = "Yes"
     ;
  value F01589_
     0 = "No"
     1 = "Yes"
     ;
  value F01590_
     0 = "No"
     1 = "Yes"
     ;
  value F01591_
     0 = "No"
     1 = "Yes"
     ;
  value F01592_
     0 = "No"
     1 = "Yes"
     ;
  value F01593_
     0 = "No"
     1 = "Yes"
     ;
  value F01594_
     0 = "No"
     1 = "Yes"
     ;
  value F01595_
     0 = "No"
     1 = "Yes"
     ;
  value F01596_
     0 = "No"
     1 = "Yes"
     ;
  value F01597_
     0 = "No"
     1 = "Yes"
     ;
  value F01598_
     0 = "No"
     1 = "Yes"
     ;
  value F01599_
     0 = "No"
     1 = "Yes"
     ;
  value F01600_
     0 = "No"
     1 = "Yes"
     ;
  value F01601_
     0 = "No"
     1 = "Yes"
     ;
  value F01602_
     0 = "No"
     1 = "Yes"
     ;
  value F01603_
     0 = "No"
     1 = "Yes"
     ;
  value F01604_
     0 = "No"
     1 = "Yes"
     ;
  value F01605_
     0 = "No"
     1 = "Yes"
     ;
  value F01606_
     0 = "No"
     1 = "Yes"
     ;
  value F01607_
     0 = "No"
     1 = "Yes"
     ;
  value F01608_
     0 = "No"
     1 = "Yes"
     ;
  value F01609_
     0 = "No"
     1 = "Yes"
     ;
  value F01610_
     0 = "No"
     1 = "Yes"
     ;
  value F01611_
     0 = "No"
     1 = "Yes"
     ;
  value F01612_
     0 = "No"
     1 = "Yes"
     ;
  value F01613_
     0 = "No"
     1 = "Yes"
     ;
  value F01614_
     0 = "No"
     1 = "Yes"
     ;
  value F01615_
     0 = "No"
     1 = "Yes"
     ;
  value F01616_
     0 = "No"
     1 = "Yes"
     ;
  value F01617_
     0 = "No"
     1 = "Yes"
     8 = "Refuse to answer"
     ;
  value F01618_
     0 = "No"
     1 = "Yes"
     ;
  value F01619_
     0 = "No"
     1 = "Yes"
     ;
  value F01620_
     0 = "No"
     1 = "Yes"
     ;
  value F01621_
     0 = "No"
     1 = "Yes"
     ;
  value F01622_
     0 = "No"
     1 = "Yes"
     ;
  value F01623_
     0 = "No"
     1 = "Yes"
     ;
  value F01624_
     0 = "No"
     1 = "Yes"
     ;
  value F01625_
     0 = "No"
     1 = "Yes"
     ;
  value F01626_
     0 = "No"
     1 = "Yes"
     8 = "Refuse to answer"
     ;
  value F01627_
     0 = "No"
     1 = "Yes"
     ;
  value F01628_
     0 = "No"
     1 = "Yes"
     ;
  value F01629_
     0 = "No"
     1 = "Yes"
     ;
  value F01630_
     0 = "No"
     1 = "Yes"
     ;
  value F01631_
     0 = "No"
     1 = "Yes"
     ;
  value F01632_
     0 = "No"
     1 = "Yes"
     ;
  value F01633_
     0 = "No"
     1 = "Yes"
     ;
  value F01634_
     0 = "No"
     1 = "Yes"
     ;
  value F01635_
     0 = "No"
     1 = "Yes"
     8 = "Refuse to answer"
     ;
  value F01636_
     0 = "No"
     1 = "Yes"
     ;
  value F01637_
     0 = "No"
     1 = "Yes"
     ;
  value F01638_
     0 = "No"
     1 = "Yes"
     ;
  value F01639_
     0 = "No"
     1 = "Yes"
     ;
  value F01640_
     0 = "No"
     1 = "Yes"
     ;
  value F01641_
     0 = "No"
     1 = "Yes"
     ;
  value F01642_
     0 = "No"
     1 = "Yes"
     ;
  value F01643_
     0 = "No"
     1 = "Yes"
     ;
  value F01644_
     0 = "No"
     1 = "Yes"
     ;
  value F01645_
     0 = "No"
     1 = "Yes"
     8 = "Refuse to answer"
     ;
  value F01646_
     0 = "No"
     1 = "Yes"
     ;
  value F01647_
     0 = "No"
     1 = "Yes"
     ;
  value F01648_
     0 = "No"
     1 = "Yes"
     ;
  value F01649_
     0 = "No"
     1 = "Yes"
     ;
  value F01650_
     0 = "No"
     1 = "Yes"
     ;
  value F01651_
     0 = "No"
     1 = "Yes"
     ;
  value F01652_
     0 = "No"
     1 = "Yes"
     ;
  value F01653_
     0 = "No"
     1 = "Yes"
     ;
  value F01654_
     0 = "No"
     1 = "Yes"
     ;
  value F01655_
     0 = "No"
     1 = "Yes"
     ;
  value F01656_
     0 = "No"
     1 = "Yes"
     ;
  value F01657_
     0 = "No"
     1 = "Yes"
     ;
  value F01658_
     0 = "No"
     1 = "Yes"
     ;
  value F01659_
     0 = "No"
     1 = "Yes"
     ;
  value F01660_
     0 = "No"
     1 = "Yes"
     ;
  value F01661_
     0 = "No"
     1 = "Yes"
     ;
  value F01662_
     0 = "No"
     1 = "Yes"
     ;
  value F01663_
     0 = "No"
     1 = "Yes"
     ;
  value F01664_
     0 = "No"
     1 = "Yes"
     ;
  value F01665_
     0 = "No"
     1 = "Yes"
     ;
  value F01666_
     0 = "No"
     1 = "Yes"
     ;
  value F01667_
     0 = "No"
     1 = "Yes"
     ;
  value F01668_
     0 = "No"
     1 = "Yes"
     ;
  value F01669_
     0 = "No"
     1 = "Yes"
     ;
  value F01670_
     0 = "No"
     1 = "Yes"
     ;
  value F01671_
     0 = "No"
     1 = "Yes"
     ;
  value F01672_
     0 = "No"
     1 = "Yes"
     ;
  value F01673_
     0 = "No"
     1 = "Yes"
     ;
  value F01674_
     0 = "No"
     1 = "Yes"
     ;
  value F01675_
     0 = "No"
     1 = "Yes"
     ;
  value F01676_
     0 = "No"
     1 = "Yes"
     ;
  value F01677_
     0 = "No"
     1 = "Yes"
     ;
  value F01678_
    98 = "Don't remember"
     ;
  value F01679_
     0 = "No"
     1 = "Yes"
     ;
  value F01680_
     0 = "No"
     1 = "Yes"
     ;
  value F01681_
     0 = "No"
     1 = "Yes"
     ;
  value F01682_
     0 = "No"
     1 = "Yes"
     ;
  value F01683_
     0 = "No"
     1 = "Yes"
     ;
  value F01684_
     0 = "No"
     1 = "Yes"
     ;
  value F01685_
     0 = "No"
     1 = "Yes"
     ;
  value F01686_
     0 = "No"
     1 = "Yes"
     ;
  value F01687_
     0 = "No"
     1 = "Yes"
     ;
  value F01688_
     0 = "No"
     1 = "Yes"
     ;
  value F01689_
     0 = "No"
     1 = "Yes"
     ;
  value F01690_
     0 = "No"
     1 = "Yes"
     2 = "never have sought work"
     ;
  value F01691_
     0 = "No"
     1 = "Yes"
     2 = "never have sought work"
     ;
  value F01692_
     0 = "No"
     1 = "Yes"
     2 = "never have sought work"
     ;
  value F01693_
     0 = "No"
     1 = "Yes"
     2 = "Never pregnant/never worked while pregnant"
     ;
  value F01694_
     0 = "No"
     1 = "Yes"
     ;
  value F01695_
     0 = "No"
     1 = "Yes"
     ;
  value F01696_
     0 = "No"
     1 = "Yes"
     ;
  value F01697_
     0 = "No"
     1 = "Yes"
     ;
  value F01698_
     0 = "No"
     1 = "Yes"
     ;
  value F01699_
     0 = "No"
     1 = "Yes"
     ;
  value F01700_
     0 = "No"
     1 = "Yes"
     ;
  value F01701_
     0 = "No"
     1 = "Yes"
     ;
  value F01702_
     0 = "No"
     1 = "Yes"
     ;
  value F01703_
     0 = "No"
     1 = "Yes"
     ;
  value F01704_
     0 = "No"
     1 = "Yes"
     ;
  value F01705_
     0 = "No"
     1 = "Yes, never denounced"
     ;
  value F01706_
     0 = "No"
     1 = "Yes"
     ;
  value F01707_
     0 = "No"
     1 = "Yes"
     ;
  value F01708_
     0 = "No"
     1 = "Yes"
     ;
  value F01709_
     0 = "No"
     1 = "Yes"
     ;
  value F01710_
     0 = "No"
     1 = "Yes"
     ;
  value F01711_
     0 = "No"
     1 = "Yes"
     ;
  value F01712_
     0 = "No"
     1 = "Yes"
     ;
  value F01713_
     0 = "No"
     1 = "Yes"
     ;
  value F01714_
     0 = "No"
     1 = "Yes"
     ;
  value F01715_
     0 = "No"
     1 = "Yes"
     ;
  value F01716_
     0 = "No"
     1 = "Yes"
     ;
  value F01717_
     0 = "No"
     1 = "Yes"
     ;
  value F01718_
     0 = "No"
     1 = "Yes"
     ;
  value F01719_
     0 = "No"
     1 = "Yes"
     ;
  value F01720_
     0 = "No"
     1 = "Yes"
     ;
  value F01721_
     0 = "No"
     1 = "Yes"
     ;
  value F01722_
     0 = "No"
     1 = "Yes"
     ;
  value F01723_
     0 = "No"
     1 = "Yes"
     ;
  value F01724_
     0 = "No"
     1 = "Yes"
     ;
  value F01725_
     0 = "No"
     1 = "Yes"
     ;
  value F01726_
     0 = "No"
     1 = "Yes"
     ;
  value F01727_
     0 = "No"
     1 = "Yes"
     ;
  value F01728_
     0 = "No"
     1 = "Yes"
     ;
  value F01729_
     0 = "No"
     1 = "Yes"
     ;
  value F01730_
     0 = "No"
     1 = "Yes"
     ;
  value F01731_
     0 = "No"
     1 = "Yes"
     ;
  value F01732_
     0 = "No"
     1 = "Yes"
     ;
  value F01733_
     0 = "No"
     1 = "Yes"
     ;
  value F01734_
     0 = "No"
     1 = "Yes"
     ;
  value F01735_
     0 = "No"
     1 = "Yes"
     ;
  value F01736_
     1 = "Disagrees"
     2 = "Neither agrees nor disagrees"
     3 = "Agrees"
     ;
  value F01737_
     1 = "Disagrees"
     2 = "Neither agrees nor disagrees"
     3 = "Agrees"
     ;
  value F01738_
     1 = "Disagrees"
     2 = "Neither agrees nor disagrees"
     3 = "Agrees"
     ;
  value F01739_
     1 = "Disagrees"
     2 = "Neither agrees nor disagrees"
     3 = "Agrees"
     ;
  value F01740_
     1 = "Disagrees"
     2 = "Neither agrees nor disagrees"
     3 = "Agrees"
     ;
  value F01741_
     1 = "Disagrees"
     2 = "Neither agrees nor disagrees"
     3 = "Agrees"
     ;
  value F01742_
     1 = "Disagrees"
     2 = "Neither agrees nor disagrees"
     3 = "Agrees"
     ;
  value F01743_
     1 = "Disagrees"
     2 = "Neither agrees nor disagrees"
     3 = "Agrees"
     ;
  value F01744_
     1 = "Disagrees"
     2 = "Neither agrees nor disagrees"
     3 = "Agrees"
     ;
  value F01745_
     0 = "No"
     1 = "Yes"
     8 = "Refuse to answer"
     ;
  value F01746_
     1 = "Has already done it"
     2 = "Would do it"
     3 = "Wouldn't do it"
     8 = "Refuse to answer"
     ;
  value F01747_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F01748_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F01749_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F01750_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F01751_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F01752_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F01753_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F01754_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F01755_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F01756_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F01757_
    98 = "Don't know"
     ;
  value F01758_
     0 = "Not"
     1 = "Yes"
     ;
  value F01759_
     0 = "Not"
     1 = "Yes"
     ;
  value F01760_
     0 = "No"
     1 = "Yes"
     8 = "Doesn't remember"
     ;
  value F01761_
     0 = "No"
     1 = "Yes"
     8 = "Don't remember"
     ;
  value F01762_
     0 = "No"
     1 = "Yes"
     ;
  value F01763_
     0 = "No"
     1 = "Yes"
     ;
  value F01764_
     0 = "No"
     1 = "Yes"
     ;
  value F01765_
     0 = "No"
     1 = "Yes"
     ;
  value F01766_
     0 = "No"
     1 = "Yes"
     ;
  value F01767_
     0 = "No"
     1 = "Yes"
     ;
  value F01768_
     0 = "No"
     1 = "Yes"
     ;
  value F01769_
     0 = "No"
     1 = "Yes"
     ;
  value F01770_
     0 = "No"
     1 = "Yes"
     ;
  value F01771_
     0 = "No"
     1 = "Yes"
     3 = "Not a member of health insurance"
     ;
  value F01772_
    98 = "Don't know"
     ;
  value F01773_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F01774_
     0 = "No"
     1 = "Yes"
     3 = "Not a member of health insurance"
     ;
  value F01775_
    98 = "Don't know"
     ;
  value F01776_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F01777_
     0 = "No"
     1 = "Yes"
     3 = "Not a member of health insurance"
     ;
  value F01778_
    98 = "Don't know"
     ;
  value F01779_
     0 = "No"
     1 = "Yes"
     ;
  value F01780_
     0 = "Once in awhile"
    51 = "51+"
    98 = "Don't know"
     ;
  value F01781_
     0 = "No"
     1 = "Yes"
     ;
  value F01782_
     0 = "Once in awhile"
    98 = "Don't know"
     ;
  value F01783_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F01784_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F01785_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F01786_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F01787_
     1 = "Daily"
     2 = "Weekly"
     3 = "Every two weeks"
     4 = "Monthly"
     5 = "Once in a while"
     6 = "Other"
     8 = "Refused"
     ;
  value F01788_
     0 = "Less than one day"
   995 = "Was not hospitalized"
     ;
  value F01789_
     0 = "No"
     1 = "Yes"
     ;
  value F01790_
     0 = "No"
     1 = "Yes"
     ;
  value F01791_
     0 = "No"
     1 = "Yes"
     ;
  value F01792_
     0 = "No"
     1 = "Yes"
     ;
  value F01793_
     0 = "No"
     1 = "Yes"
     ;
  value F01794_
     0 = "No"
     1 = "Yes"
     ;
  value F01795_
     0 = "No"
     1 = "Yes"
     ;
  value F01796_
     0 = "No"
     1 = "Yes"
     ;
  value F01797_
     0 = "No"
     1 = "Yes"
     ;
  value F01798_
     0 = "No"
     1 = "Yes"
     ;
  value F01799_
     0 = "No"
     1 = "Yes"
     ;
  value F01800_
     1 = "Urgency"
     2 = "Programmed"
     ;
  value F01801_
     0 = "No"
     1 = "Yes"
     ;
  value F01802_
     0 = "No"
     1 = "Yes"
     ;
  value F01803_
     0 = "No"
     1 = "Yes"
     ;
  value F01804_
     0 = "No"
     1 = "Yes"
     ;
  value F01805_
     0 = "No"
     1 = "Yes"
     ;
  value F01806_
     0 = "No"
     1 = "Yes"
     ;
  value F01807_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F01808_
     0 = "Child not measured"
    98 = "Don't know/don't remember"
     ;
  value F01809_
     0 = "No"
     1 = "Yes"
     ;
  value F01810_
     0 = "No"
     1 = "Yes"
     ;
  value F01811_
     0 = "No"
     1 = "Yes"
     ;
  value F01812_
     0 = "No"
     1 = "Yes"
     ;
  value F01813_
     0 = "No"
     1 = "Yes"
     ;
  value F01814_
     0 = "No"
     1 = "Yes"
     ;
  value F01815_
     0 = "No"
     1 = "Yes"
     ;
  value F01816_
     0 = "No"
     1 = "Yes"
     ;
  value F01817_
     0 = "No"
     1 = "Yes"
     ;
  value F01818_
     0 = "No"
     1 = "Yes"
     ;
  value F01819_
     0 = "No"
     1 = "Yes"
     ;
  value F01820_
     0 = "No"
     1 = "Yes"
     ;
  value F01821_
     0 = "No"
     1 = "Yes"
     ;
  value F01822_
     0 = "No"
     1 = "Yes"
     ;
  value F01823_
     0 = "No"
     1 = "Yes"
     ;
  value F01824_
     1 = "Before delivery"
     2 = "At time of delivery"
     3 = "In prenatal check-up"
     ;
  value F01825_
     0 = "No"
     1 = "Yes"
     ;
  value F01826_
     0 = "No"
     1 = "Yes"
     ;
  value F01827_
     0 = "No"
     1 = "Yes"
     ;
  value F01828_
     0 = "No"
     1 = "Yes"
     ;
  value F01829_
     0 = "No"
     1 = "Yes"
     ;
  value F01830_
     0 = "No"
     1 = "Yes"
     ;
  value F01831_
     0 = "No"
     1 = "Yes"
     ;
  value F01832_
     0 = "No"
     1 = "Yes"
     ;
  value F01833_
     0 = "No"
     1 = "Yes"
     ;
  value F01834_
     0 = "No"
     1 = "Yes"
     ;
  value F01835_
   100 = "0 days"
   101 = "1 day"
   200 = "0 months"
   201 = "1 month"
   998 = "Doesn't remember"
     ;
  value F01836_
     1 = "Have sexual intercourse"
     2 = "Make love"
     3 = "How you see yourself, feel and behave"
     4 = "Have sex to have children"
     6 = "Other"
     8 = "Don't know"
     ;
  value F01837_
     0 = "No"
     1 = "Yes"
     ;
  value F01838_
     1 = "How sex organs work"
     2 = "Changes as a result of puberty"
     3 = "Changes as a result of aging"
     4 = "Emotional relationships"
     5 = "Emotional communication and conflict resolution"
     6 = "Gender equity"
     7 = "Pleasure and eroticism"
     8 = "Sexual orientation"
     9 = "Right to request use of condoms"
    10 = "To have children"
    11 = "Use of contraception"
    12 = "Right to interrupt a pregnancy - IVE"
    13 = "Safe sexual practices"
    14 = "Violence based on gender - VBG? (i.e., sex abuse, harassment, discrimination)"
    15 = "Where to get information about sexuality"
    16 = "Services and resources available for SSR"
    17 = "How to denounce violation of sexual and reproductive rights"
     ;
  value F01839_
     0 = "No"
     1 = "Yes"
     ;
  value F01840_
    98 = "Don't know"
     ;
  value F01841_
     1 = "Family"
     2 = "Education institutions"
     3 = "Health institutions"
     4 = "Acquaintances, friends"
     5 = "Community"
     6 = "Media"
    96 = "Other"
     ;
  value F01842_
     1 = "Radio"
     2 = "Newspapers"
     3 = "Television"
     4 = "Brochures/pamphlets"
     5 = "Internet"
     6 = "Social media"
     7 = "Mobile phones"
    96 = "Other"
     ;
  value F01843_
     1 = "Never"
     2 = "1 to 3 times a year"
     3 = "4 to 6 times a year"
     4 = "7 to 9 times a year"
     5 = "10+ times a year"
     8 = "Don't remember"
     ;
  value F01844_
     1 = "Never"
     2 = "1 to 3 times a year"
     3 = "4 to 6 times a year"
     4 = "7 to 9 times a year"
     5 = "10+ times a year"
     8 = "Don't remember"
     ;
  value F01845_
     1 = "Nothing"
     2 = "Little"
     3 = "A lot"
     ;
  value F01846_
     1 = "Nothing"
     2 = "Little"
     3 = "A lot"
     ;
  value F01847_
     1 = "Nothing"
     2 = "Little"
     3 = "A lot"
     ;
  value F01848_
     1 = "Nothing"
     2 = "Little"
     3 = "A lot"
     ;
  value F01849_
     1 = "Nothing"
     2 = "Little"
     3 = "A lot"
     ;
  value F01850_
     1 = "Nothing"
     2 = "Little"
     3 = "A lot"
     ;
  value F01851_
     1 = "Nothing"
     2 = "Little"
     3 = "A lot"
     ;
  value F01852_
     1 = "Nothing"
     2 = "Little"
     3 = "A lot"
     ;
  value F01853_
     1 = "Disagree"
     2 = "Neither agree or disagree"
     3 = "Agree"
     ;
  value F01854_
     1 = "Disagree"
     2 = "Neither agree or disagree"
     3 = "Agree"
     ;
  value F01855_
     1 = "Disagree"
     2 = "Neither agree or disagree"
     3 = "Agree"
     ;
  value F01856_
     1 = "Disagree"
     2 = "Neither agree or disagree"
     3 = "Agree"
     ;
  value F01857_
     1 = "Disagree"
     2 = "Neither agree or disagree"
     3 = "Agree"
     ;
  value F01858_
     1 = "Disagree"
     2 = "Neither agree or disagree"
     3 = "Agree"
     ;
  value F01859_
     1 = "Disagree"
     2 = "Neither agree or disagree"
     3 = "Agree"
     ;
  value F01860_
     1 = "Disagree"
     2 = "Neither agree or disagree"
     3 = "Agree"
     ;
  value F01861_
     1 = "Disagree"
     2 = "Neither agree or disagree"
     3 = "Agree"
     ;
  value F01862_
     1 = "Disagree"
     2 = "Neither agree or disagree"
     3 = "Agree"
     ;
  value F01863_
     0 = "No"
     1 = "Yes, spontaneous"
     2 = "Yes, probed"
     ;
  value F01864_
     0 = "No"
     1 = "Yes"
     ;
  value F01865_
     0 = "No"
     1 = "Yes"
     ;
  value F01866_
     0 = "No"
     1 = "Yes"
     ;
  value F01867_
     0 = "No"
     1 = "Yes"
     ;
  value F01868_
     0 = "No"
     1 = "Yes"
     ;
  value F01869_
     0 = "No"
     1 = "Yes"
     ;
  value F01870_
     0 = "No"
     1 = "Yes"
     ;
  value F01871_
     0 = "No"
     1 = "Yes"
     ;
  value F01872_
    98 = "Don't know"
    99 = "Missing"
     ;
  value F01873_
  9998 = "Don't know"
     ;
  value F01874_
     0 = "No"
     1 = "Yes"
     8 = "Don't know/don't remember"
     ;
  value F01875_
     1 = "More than once a year"
     2 = "Every year"
     3 = "Every three years"
     4 = "Has done only once"
     6 = "Other"
     ;
  value F01876_
     0 = "No"
     1 = "Yes"
     2 = "Haven't received yet"
     ;
  value F01877_
     1 = "Normal"
     2 = "Abnormal"
     8 = "Don't know/don't remember"
     ;
  value F01878_
     1 = "Woman has cancer"
     2 = "Woman suspect to have cancer"
     3 = "Woman has early stages of cancer"
     4 = "Woman suspect to have early stages of cancer"
     5 = "Test needs to be repeated"
     6 = "Other"
     8 = "Don't know"
     ;
  value F01879_
     1 = "Treatment for cancer"
     2 = "Treatment for early stages of cancer"
     3 = "Additional studies to know if treatment is needed"
     6 = "Other"
     8 = "Don't know"
     ;
  value F01880_
     1 = "Afraid to have cancer"
     2 = "Felt mistreated/offended when took test"
     3 = "Not interested in result"
     4 = "Institution where test was taken didn't return it"
     5 = "Careless/don't have time"
     6 = "Other"
     ;
  value F01881_
     1 = "Personal reasons"
     2 = "Health services related reasons"
     3 = "Economic reasons"
     4 = "Authorization problems"
     5 = "Lack of knowledge"
     6 = "Opposition"
     7 = "Wasn't affiliated to health insurance"
     8 = "Didn't know that could use service"
    96 = "Other"
     ;
  value F01882_
     0 = "No"
     1 = "Yes"
     ;
  value F01883_
     1 = "Personal reasons"
     2 = "Health services related reasons"
     3 = "Economic reasons"
     4 = "Authorization problems"
     5 = "Lack of knowledge"
     6 = "Opposition"
     7 = "Wasn't affiliated to health insurance"
     8 = "Didn't know that could use service"
    96 = "Other"
     ;
  value F01884_
     0 = "No"
     1 = "Yes, spontaneous"
     2 = "Yes, probed"
     ;
  value F01885_
     0 = "No"
     1 = "Yes, spontaneous"
     2 = "Yes, probed"
     ;
  value F01886_
     0 = "No"
     1 = "Yes, spontaneous"
     2 = "Yes, probed"
     ;
  value F01887_
     0 = "No"
     1 = "Yes"
     ;
  value F01888_
     0 = "No"
     1 = "Yes"
     ;
  value F01889_
     0 = "No"
     1 = "Yes"
     ;
  value F01890_
     0 = "No"
     1 = "Yes"
     ;
  value F01891_
     0 = "No"
     1 = "Yes"
     ;
  value F01892_
     0 = "No"
     1 = "Yes"
     ;
  value F01893_
     0 = "No"
     1 = "Yes"
     ;
  value F01894_
     1 = "Every month"
     2 = "Every six months"
     3 = "Has made it only once"
     6 = "Other"
     ;
  value F01895_
     1 = "After menstruation"
     2 = "Before menstruation"
     3 = "During menstruation"
     4 = "Any time"
     5 = "No menstruation anymore, any time"
     6 = "Other"
     ;
  value F01896_
     0 = "No"
     1 = "Yes"
     8 = "Don't know/don't remember"
     ;
  value F01897_
     1 = "During last year"
     2 = "From 1 to 5 years"
     3 = "More than 5 years"
     8 = "Don't remember"
     ;
  value F01898_
     1 = "Had symptoms in breasts"
     2 = "Regular check-up"
     8 = "Don't know/don't remember"
     ;
  value F01899_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F01900_
     0 = "No"
     1 = "Yes, spontaneous"
     2 = "Yes, probed"
     ;
  value F01901_
     0 = "No"
     1 = "Yes"
     ;
  value F01902_
     0 = "No"
     1 = "Yes"
     ;
  value F01903_
     0 = "No"
     1 = "Yes"
     ;
  value F01904_
     0 = "No"
     1 = "Yes"
     ;
  value F01905_
     0 = "No"
     1 = "Yes"
     ;
  value F01906_
     0 = "No"
     1 = "Yes"
     ;
  value F01907_
     0 = "No"
     1 = "Yes"
     ;
  value F01908_
     1 = "Had symptoms in breasts"
     2 = "Regular check-up"
     8 = "Don't know/don't remember"
     ;
  value F01909_
    98 = "Don't remember"
     ;
  value F01910_
  9998 = "Don't remember"
     ;
  value F01911_
     1 = "Once a year"
     2 = "Twice a year"
     3 = "Every other year"
     4 = "First time that was done"
     6 = "Other"
     ;
  value F01912_
    10 = "PUBLIC"
    11 = "Hospital/Clinic"
    20 = "PRIVATE MEDICAL"
    21 = "Private hospital/clinic"
    22 = "EPS health center"
    23 = "Private doctor"
    24 = "PROFAMILIA"
    25 = "League against cancer"
    96 = "Other"
    98 = "Don't know"
     ;
  value F01913_
     0 = "No"
     1 = "Yes"
     2 = "Haven't received it yet"
     ;
  value F01914_
     1 = "Normal"
     2 = "Abnormal"
     8 = "Don't know/don't remember"
     ;
  value F01915_
     1 = "Afraid to have cancer"
     2 = "Not interested in result"
     3 = "Institution where test was taken didn't return it"
     6 = "Other"
     ;
  value F01916_
     0 = "No"
     1 = "Yes"
     8 = "Don't remember"
     ;
  value F01917_
     1 = "Personal reasons"
     2 = "Health services related reasons"
     3 = "Economic reasons"
     4 = "Authorization problems"
     5 = "Lack of knowledge"
     6 = "Opposition"
     7 = "Wasn't affiliated to health insurance"
     8 = "Didn't know that could use service"
    96 = "Other"
     ;
  value F01918_
     0 = "No"
     1 = "Yes"
     ;
  value F01919_
     1 = "Personal reasons"
     2 = "Health services related reasons"
     3 = "Economic reasons"
     4 = "Authorization problems"
     5 = "Lack of knowledge"
     6 = "Opposition"
     7 = "Wasn't affiliated to health insurance"
     8 = "Didn't know that could use service"
    96 = "Other"
     ;
  value F01920_
     1 = "Personal reasons"
     2 = "Health services related reasons"
     3 = "Economic reasons"
     4 = "Authorization problems"
     5 = "Lack of knowledge"
     6 = "Opposition"
     7 = "Wasn't affiliated to health insurance"
     8 = "Didn't know that could use service"
    96 = "Other"
     ;
  value F01921_
     1 = "Disagrees"
     2 = "Neither agrees nor disagrees"
     3 = "Agrees"
     ;
  value F01922_
     1 = "Disagrees"
     2 = "Neither agrees nor disagrees"
     3 = "Agrees"
     ;
  value F01923_
     1 = "Disagrees"
     2 = "Neither agrees nor disagrees"
     3 = "Agrees"
     ;
  value F01924_
     1 = "Disagrees"
     2 = "Neither agrees nor disagrees"
     3 = "Agrees"
     ;
  value F01925_
     1 = "Disagrees"
     2 = "Neither agrees nor disagrees"
     3 = "Agrees"
     ;
  value F01926_
     1 = "Disagrees"
     2 = "Neither agrees nor disagrees"
     3 = "Agrees"
     ;
  value F01927_
     1 = "Disagrees"
     2 = "Neither agrees nor disagrees"
     3 = "Agrees"
     ;
  value F01928_
     1 = "Disagrees"
     2 = "Neither agrees nor disagrees"
     3 = "Agrees"
     ;
  value F01929_
     1 = "Disagrees"
     2 = "Neither agrees nor disagrees"
     3 = "Agrees"
     ;
  value F01930_
     1 = "Disagrees"
     2 = "Neither agrees nor disagrees"
     3 = "Agrees"
     ;
  value F01931_
     1 = "Disagrees"
     2 = "Neither agrees nor disagrees"
     3 = "Agrees"
     ;
  value F01932_
     1 = "Disagrees"
     2 = "Neither agrees nor disagrees"
     3 = "Agrees"
     ;
  value F01933_
     1 = "Disagrees"
     2 = "Neither agrees nor disagrees"
     3 = "Agrees"
     ;
  value F01934_
     1 = "Disagrees"
     2 = "Neither agrees nor disagrees"
     3 = "Agrees"
     ;
  value F01935_
     1 = "Disagrees"
     2 = "Neither agrees nor disagrees"
     3 = "Agrees"
     ;
  value F01936_
     1 = "Disagrees"
     2 = "Neither agrees nor disagrees"
     3 = "Agrees"
     ;
  value F01937_
     1 = "Disagrees"
     2 = "Neither agrees nor disagrees"
     3 = "Agrees"
     ;
  value F01938_
     1 = "Disagrees"
     2 = "Neither agrees nor disagrees"
     3 = "Agrees"
     ;
  value F01939_
     1 = "Disagrees"
     2 = "Neither agrees nor disagrees"
     3 = "Agrees"
     ;
  value F01940_
     1 = "Disagrees"
     2 = "Neither agrees nor disagrees"
     3 = "Agrees"
     ;
  value F01941_
     1 = "Respondent alone"
     2 = "More respondent than spouse"
     3 = "Both equally"
     4 = "Spouse more than respondent"
     5 = "Spouse alone"
     7 = "Neither of them"
     ;
  value F01942_
     1 = "Respondent alone"
     2 = "More respondent than spouse"
     3 = "Both equally"
     4 = "Spouse more than respondent"
     5 = "Spouse alone"
     7 = "Neither of them"
     ;
  value F01943_
     1 = "Respondent alone"
     2 = "More respondent than spouse"
     3 = "Both equally"
     4 = "Spouse more than respondent"
     5 = "Spouse alone"
     7 = "Neither of them"
     ;
  value F01944_
     1 = "Respondent alone"
     2 = "More respondent than spouse"
     3 = "Both equally"
     4 = "Spouse more than respondent"
     5 = "Spouse alone"
     7 = "Neither of them"
     ;
  value F01945_
     1 = "Respondent alone"
     2 = "More respondent than spouse"
     3 = "Both equally"
     4 = "Spouse more than respondent"
     5 = "Spouse alone"
     7 = "Neither of them"
     ;
  value F01946_
     1 = "Respondent alone"
     2 = "More respondent than spouse"
     3 = "Both equally"
     4 = "Spouse more than respondent"
     5 = "Spouse alone"
     7 = "Neither of them"
     ;
  value F01947_
     1 = "Respondent alone"
     2 = "More respondent than spouse"
     3 = "Both equally"
     4 = "Spouse more than respondent"
     5 = "Spouse alone"
     7 = "Neither of them"
     ;
  value F01948_
     1 = "Respondent alone"
     2 = "More respondent than spouse"
     3 = "Both equally"
     4 = "Spouse more than respondent"
     5 = "Spouse alone"
     7 = "Neither of them"
     ;
  value F01949_
     1 = "Respondent alone"
     2 = "More respondent than spouse"
     3 = "Both equally"
     4 = "Spouse more than respondent"
     5 = "Spouse alone"
     7 = "Neither of them"
     ;
  value F01950_
     1 = "Respondent alone"
     2 = "More respondent than spouse"
     3 = "Both equally"
     4 = "Spouse more than respondent"
     5 = "Spouse alone"
     7 = "Neither of them"
     ;
  value F01951_
     1 = "Respondent alone"
     2 = "More respondent than spouse"
     3 = "Both equally"
     4 = "Spouse more than respondent"
     5 = "Spouse alone"
     7 = "Neither of them"
     ;
  value F01952_
     1 = "Respondent alone"
     2 = "More respondent than spouse"
     3 = "Both equally"
     4 = "Spouse more than respondent"
     5 = "Spouse alone"
     7 = "Neither of them"
     ;
  value F01953_
     1 = "Respondent alone"
     2 = "More respondent than spouse"
     3 = "Both equally"
     4 = "Spouse more than respondent"
     5 = "Spouse alone"
     7 = "Neither of them"
     ;
  value F01954_
     1 = "Respondent take child with her"
     2 = "Both"
     3 = "Older female child"
     4 = "Older male child"
     5 = "Grand parents"
     6 = "Other relatives"
     7 = "Neighbors"
     8 = "Friends"
     9 = "Domestic maid"
    10 = "Child is at school"
    11 = "Colombia welfare institute (ICBF)"
    12 = "Other institution"
    13 = "Nobody"
    96 = "Other"
     ;
  value F01955_
     0 = "No"
     1 = "Yes"
     ;
  value F01956_
     0 = "No"
     1 = "Yes"
     ;
  value F01957_
     0 = "No"
     1 = "Yes"
     ;
  value F01958_
     0 = "No"
     1 = "Yes"
     ;
  value F01959_
     0 = "No"
     1 = "Yes"
     ;
  value F01960_
     0 = "No"
     1 = "Yes"
     ;
  value F01961_
     0 = "No"
     1 = "Yes"
     ;
  value F01962_
     0 = "No"
     1 = "Yes"
     ;
  value F01963_
     0 = "No"
     1 = "Yes"
     ;
  value F01964_
     0 = "No"
     1 = "Yes"
     ;
  value F01965_
     0 = "No"
     1 = "Yes"
     ;
  value F01966_
     1 = "Do nothing for fear"
     2 = "Would be indifferent"
     3 = "Wouldn't do nothing because agrees with affront"
     4 = "Would defend the person"
     5 = "Would call the police"
     6 = "Other"
     8 = "Don't know"
     ;
  value F01967_
     0 = "No"
     1 = "Yes"
     ;
  value F01968_
     1 = "Agrees"
     2 = "Disagrees"
     ;
  value F01969_
     0 = "No"
     1 = "Yes"
     ;
  value F01970_
     1 = "Disagrees"
     2 = "Neither agrees nor disagrees"
     3 = "Agrees"
     ;
  value F01971_
     1 = "Disagrees"
     2 = "Neither agrees nor disagrees"
     3 = "Agrees"
     ;
  value F01972_
     1 = "Disagrees"
     2 = "Neither agrees nor disagrees"
     3 = "Agrees"
     ;
  value F01973_
     1 = "Disagrees"
     2 = "Neither agrees nor disagrees"
     3 = "Agrees"
     ;
  value F01974_
     1 = "Disagrees"
     2 = "Neither agrees nor disagrees"
     3 = "Agrees"
     ;
  value F01975_
     1 = "Disagrees"
     2 = "Neither agrees nor disagrees"
     3 = "Agrees"
     ;
  value F01976_
     1 = "Disagrees"
     2 = "Neither agrees nor disagrees"
     3 = "Agrees"
     ;
  value F01977_
     1 = "Disagrees"
     2 = "Neither agrees nor disagrees"
     3 = "Agrees"
     ;
  value F01978_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F01979_
     1 = "Would throw out of the house"
     2 = "Would withdraw economic support"
     3 = "Would seek mental treatment"
     4 = "Would seek medical help"
     5 = "Would support him"
     6 = "Would advise/get in the right direction"
     7 = "Would not know what to do"
     8 = "Would accept him/her"
    96 = "Other"
     ;
  value F01980_
     1 = "Did throw out of the house"
     2 = "Did withdraw economic support"
     3 = "Did seek mental treatment"
     4 = "Did seek medical help"
     5 = "Did support him"
     6 = "Did advise/got in the right direction"
     7 = "Did not know what to do"
     8 = "Did accept him/her"
    96 = "Other"
     ;
Data user.COIR72FL;
  attrib CASEID   length=$15 label="Case Identification";
  attrib V000     length=$3 label="Country code and phase";
  attrib V001     label="Cluster number";
  attrib V002     label="Household number";
  attrib V003     label="Respondent's line number";
  attrib V004     label="Ultimate area unit";
  attrib V005     label="Women's individual sample weight (6 decimals)";
  attrib V006     label="Month of interview";
  attrib V007     label="Year of interview";
  attrib V008     label="Date of interview (CMC)";
  attrib V008A    label="Date of interview Century Day Code (CDC)";
  attrib V009     label="Respondent's month of birth";
  attrib V010     label="Respondent's year of birth";
  attrib V011     label="Date of birth (CMC)";
  attrib V012     label="Respondent's current age";
  attrib V013     format=F00001_. label="Age in 5-year groups";
  attrib V014     format=F00002_. label="Completeness of age information";
  attrib V015     format=F00003_. label="Result of individual interview";
  attrib V016     label="Day of interview";
  attrib V017     label="CMC start of calendar";
  attrib V018     format=F00004_. label="Row of month of interview";
  attrib V019     format=F00005_. label="Length of calendar";
  attrib V019A    format=F00006_. label="Number of calendar columns";
  attrib V020     format=F00007_. label="Ever-married sample";
  attrib V021     label="Primary sampling unit";
  attrib V022     label="Sample strata for sampling errors";
  attrib V023     label="Stratification used in sample design";
  attrib V024     format=F00008_. label="Region";
  attrib V025     format=F00009_. label="Type of place of residence";
  attrib V026     format=F00010_. label="NA - De facto place of residence";
  attrib V027     label="Number of visits";
  attrib V028     label="Interviewer identification";
  attrib V029     label="NA - Keyer identification";
  attrib V030     label="Field supervisor";
  attrib V031     label="NA - Field editor";
  attrib V032     label="NA - Office editor";
  attrib V034     format=F00011_. label="Line number of husband";
  attrib V040     label="NA - Cluster altitude in meters";
  attrib V042     format=F00012_. label="NA - Household selected for hemoglobin";
  attrib V044     format=F00013_. label="Selected for Domestic Violence module";
  attrib V045A    format=F00014_. label="NA - Language of questionnaire";
  attrib V045B    format=F00015_. label="NA - Language of interview";
  attrib V045C    format=F00016_. label="NA - Native language of respondent";
  attrib V046     format=F00017_. label="NA - Translator used";
  attrib V101     format=F00018_. label="Region";
  attrib V102     format=F00019_. label="Type of place of residence";
  attrib V103     format=F00020_. label="NA - Childhood place of residence";
  attrib V104     format=F00021_. label="Years lived in place of residence";
  attrib V105     format=F00022_. label="NA - Type of place of previous residence";
  attrib V105A    format=F00023_. label="NA - Region of previous residence";
  attrib V106     format=F00024_. label="Highest educational level";
  attrib V107     format=F00025_. label="Highest year of education";
  attrib V113     format=F00026_. label="Source of drinking water";
  attrib V115     format=F00027_. label="Time to get to water source";
  attrib V116     format=F00028_. label="Type of toilet facility";
  attrib V119     format=F00029_. label="Household has: electricity";
  attrib V120     format=F00030_. label="Household has: radio";
  attrib V121     format=F00031_. label="Household has: television";
  attrib V122     format=F00032_. label="Household has: refrigerator";
  attrib V123     format=F00033_. label="Household has: bicycle";
  attrib V124     format=F00034_. label="Household has: motorcycle/scooter";
  attrib V125     format=F00035_. label="Household has: car/truck";
  attrib V127     format=F00036_. label="Main floor material";
  attrib V128     format=F00037_. label="Main wall material";
  attrib V129     format=F00038_. label="NA - Main roof material";
  attrib V130     format=F00039_. label="NA - Religion";
  attrib V131     format=F00040_. label="Ethnicity";
  attrib V133     format=F00041_. label="Education in single years";
  attrib V134     format=F00042_. label="NA - De facto place of residence";
  attrib V135     format=F00043_. label="Usual resident or visitor";
  attrib V136     label="Number of household members (listed)";
  attrib V137     label="Number of children 5 and under in household (de jure)";
  attrib V138     label="Number of eligible women in household (de facto)";
  attrib V139     format=F00044_. label="De jure region of residence";
  attrib V140     format=F00045_. label="De jure type of place of residence";
  attrib V141     format=F00046_. label="NA - De jure place of residence";
  attrib V149     format=F00047_. label="Educational attainment";
  attrib V150     format=F00048_. label="Relationship to household head";
  attrib V151     format=F00049_. label="Sex of household head";
  attrib V152     format=F00050_. label="Age of household head";
  attrib V153     format=F00051_. label="Household has: telephone (land-line)";
  attrib AWFACTT  label="All woman factor - total";
  attrib AWFACTU  label="All woman factor - urban/rural";
  attrib AWFACTR  label="All woman factor - regional";
  attrib AWFACTE  label="All woman factor - educational";
  attrib AWFACTW  label="All woman factor - wealth index";
  attrib V155     format=F00052_. label="NA - Literacy";
  attrib V156     format=F00053_. label="NA - Ever participated in a literacy program outside of primary";
  attrib V157     format=F00054_. label="NA - Frequency of reading newspaper or magazine";
  attrib V158     format=F00055_. label="NA - Frequency of listening to radio";
  attrib V159     format=F00056_. label="NA - Frequency of watching television";
  attrib V160     format=F00057_. label="Toilet facilities shared with other households";
  attrib V161     format=F00058_. label="Type of cooking fuel";
  attrib V166     format=F00059_. label="NA - Result of salt test for iodine";
  attrib V167     format=F00060_. label="NA - Number of trips in last 12 months";
  attrib V168     format=F00061_. label="NA - Away for more than one month in last 12 months";
  attrib V169A    format=F00062_. label="NA - Owns a mobile telephone";
  attrib V169B    format=F00063_. label="NA - Use mobile telephone for financial transactions";
  attrib V170     format=F00064_. label="NA - Has an account in a bank or other financial institution";
  attrib V171A    format=F00065_. label="NA - Use of internet";
  attrib V171B    format=F00066_. label="NA - Frequency of using internet last month";
  attrib V190     format=F00067_. label="Wealth index";
  attrib V191     label="Wealth index factor score (5 decimals)";
  attrib V190A    format=F00068_. label="Wealth index for urban/rural";
  attrib V191A    label="Wealth index factor score for urban/rural (5 decimals)";
  attrib ML101    format=F00069_. label="NA - Type of mosquito bed net(s) slept under last night";
  attrib BIDX_01  label="Birth column number(1)";
  attrib BIDX_02  label="Birth column number(2)";
  attrib BIDX_03  label="Birth column number(3)";
  attrib BIDX_04  label="Birth column number(4)";
  attrib BIDX_05  label="Birth column number(5)";
  attrib BIDX_06  label="Birth column number(6)";
  attrib BIDX_07  label="Birth column number(7)";
  attrib BIDX_08  label="Birth column number(8)";
  attrib BIDX_09  label="Birth column number(9)";
  attrib BIDX_10  label="Birth column number(10)";
  attrib BIDX_11  label="Birth column number(11)";
  attrib BIDX_12  label="Birth column number(12)";
  attrib BIDX_13  label="Birth column number(13)";
  attrib BIDX_14  label="Birth column number(14)";
  attrib BIDX_15  label="Birth column number(15)";
  attrib BIDX_16  label="Birth column number(16)";
  attrib BIDX_17  label="Birth column number(17)";
  attrib BIDX_18  label="Birth column number(18)";
  attrib BIDX_19  label="Birth column number(19)";
  attrib BIDX_20  label="Birth column number(20)";
  attrib BORD_01  label="Birth order number(1)";
  attrib BORD_02  label="Birth order number(2)";
  attrib BORD_03  label="Birth order number(3)";
  attrib BORD_04  label="Birth order number(4)";
  attrib BORD_05  label="Birth order number(5)";
  attrib BORD_06  label="Birth order number(6)";
  attrib BORD_07  label="Birth order number(7)";
  attrib BORD_08  label="Birth order number(8)";
  attrib BORD_09  label="Birth order number(9)";
  attrib BORD_10  label="Birth order number(10)";
  attrib BORD_11  label="Birth order number(11)";
  attrib BORD_12  label="Birth order number(12)";
  attrib BORD_13  label="Birth order number(13)";
  attrib BORD_14  label="Birth order number(14)";
  attrib BORD_15  label="Birth order number(15)";
  attrib BORD_16  label="Birth order number(16)";
  attrib BORD_17  label="Birth order number(17)";
  attrib BORD_18  label="Birth order number(18)";
  attrib BORD_19  label="Birth order number(19)";
  attrib BORD_20  label="Birth order number(20)";
  attrib B0_01    format=F00070_. label="Child is twin(1)";
  attrib B0_02    format=F00070_. label="Child is twin(2)";
  attrib B0_03    format=F00070_. label="Child is twin(3)";
  attrib B0_04    format=F00070_. label="Child is twin(4)";
  attrib B0_05    format=F00070_. label="Child is twin(5)";
  attrib B0_06    format=F00070_. label="Child is twin(6)";
  attrib B0_07    format=F00070_. label="Child is twin(7)";
  attrib B0_08    format=F00070_. label="Child is twin(8)";
  attrib B0_09    format=F00070_. label="Child is twin(9)";
  attrib B0_10    format=F00070_. label="Child is twin(10)";
  attrib B0_11    format=F00070_. label="Child is twin(11)";
  attrib B0_12    format=F00070_. label="Child is twin(12)";
  attrib B0_13    format=F00070_. label="Child is twin(13)";
  attrib B0_14    format=F00070_. label="Child is twin(14)";
  attrib B0_15    format=F00070_. label="Child is twin(15)";
  attrib B0_16    format=F00070_. label="Child is twin(16)";
  attrib B0_17    format=F00070_. label="Child is twin(17)";
  attrib B0_18    format=F00070_. label="Child is twin(18)";
  attrib B0_19    format=F00070_. label="Child is twin(19)";
  attrib B0_20    format=F00070_. label="Child is twin(20)";
  attrib B1_01    label="Month of birth(1)";
  attrib B1_02    label="Month of birth(2)";
  attrib B1_03    label="Month of birth(3)";
  attrib B1_04    label="Month of birth(4)";
  attrib B1_05    label="Month of birth(5)";
  attrib B1_06    label="Month of birth(6)";
  attrib B1_07    label="Month of birth(7)";
  attrib B1_08    label="Month of birth(8)";
  attrib B1_09    label="Month of birth(9)";
  attrib B1_10    label="Month of birth(10)";
  attrib B1_11    label="Month of birth(11)";
  attrib B1_12    label="Month of birth(12)";
  attrib B1_13    label="Month of birth(13)";
  attrib B1_14    label="Month of birth(14)";
  attrib B1_15    label="Month of birth(15)";
  attrib B1_16    label="Month of birth(16)";
  attrib B1_17    label="Month of birth(17)";
  attrib B1_18    label="Month of birth(18)";
  attrib B1_19    label="Month of birth(19)";
  attrib B1_20    label="Month of birth(20)";
  attrib B2_01    label="Year of birth(1)";
  attrib B2_02    label="Year of birth(2)";
  attrib B2_03    label="Year of birth(3)";
  attrib B2_04    label="Year of birth(4)";
  attrib B2_05    label="Year of birth(5)";
  attrib B2_06    label="Year of birth(6)";
  attrib B2_07    label="Year of birth(7)";
  attrib B2_08    label="Year of birth(8)";
  attrib B2_09    label="Year of birth(9)";
  attrib B2_10    label="Year of birth(10)";
  attrib B2_11    label="Year of birth(11)";
  attrib B2_12    label="Year of birth(12)";
  attrib B2_13    label="Year of birth(13)";
  attrib B2_14    label="Year of birth(14)";
  attrib B2_15    label="Year of birth(15)";
  attrib B2_16    label="Year of birth(16)";
  attrib B2_17    label="Year of birth(17)";
  attrib B2_18    label="Year of birth(18)";
  attrib B2_19    label="Year of birth(19)";
  attrib B2_20    label="Year of birth(20)";
  attrib B3_01    label="Date of birth (CMC)(1)";
  attrib B3_02    label="Date of birth (CMC)(2)";
  attrib B3_03    label="Date of birth (CMC)(3)";
  attrib B3_04    label="Date of birth (CMC)(4)";
  attrib B3_05    label="Date of birth (CMC)(5)";
  attrib B3_06    label="Date of birth (CMC)(6)";
  attrib B3_07    label="Date of birth (CMC)(7)";
  attrib B3_08    label="Date of birth (CMC)(8)";
  attrib B3_09    label="Date of birth (CMC)(9)";
  attrib B3_10    label="Date of birth (CMC)(10)";
  attrib B3_11    label="Date of birth (CMC)(11)";
  attrib B3_12    label="Date of birth (CMC)(12)";
  attrib B3_13    label="Date of birth (CMC)(13)";
  attrib B3_14    label="Date of birth (CMC)(14)";
  attrib B3_15    label="Date of birth (CMC)(15)";
  attrib B3_16    label="Date of birth (CMC)(16)";
  attrib B3_17    label="Date of birth (CMC)(17)";
  attrib B3_18    label="Date of birth (CMC)(18)";
  attrib B3_19    label="Date of birth (CMC)(19)";
  attrib B3_20    label="Date of birth (CMC)(20)";
  attrib B4_01    format=F00071_. label="Sex of child(1)";
  attrib B4_02    format=F00071_. label="Sex of child(2)";
  attrib B4_03    format=F00071_. label="Sex of child(3)";
  attrib B4_04    format=F00071_. label="Sex of child(4)";
  attrib B4_05    format=F00071_. label="Sex of child(5)";
  attrib B4_06    format=F00071_. label="Sex of child(6)";
  attrib B4_07    format=F00071_. label="Sex of child(7)";
  attrib B4_08    format=F00071_. label="Sex of child(8)";
  attrib B4_09    format=F00071_. label="Sex of child(9)";
  attrib B4_10    format=F00071_. label="Sex of child(10)";
  attrib B4_11    format=F00071_. label="Sex of child(11)";
  attrib B4_12    format=F00071_. label="Sex of child(12)";
  attrib B4_13    format=F00071_. label="Sex of child(13)";
  attrib B4_14    format=F00071_. label="Sex of child(14)";
  attrib B4_15    format=F00071_. label="Sex of child(15)";
  attrib B4_16    format=F00071_. label="Sex of child(16)";
  attrib B4_17    format=F00071_. label="Sex of child(17)";
  attrib B4_18    format=F00071_. label="Sex of child(18)";
  attrib B4_19    format=F00071_. label="Sex of child(19)";
  attrib B4_20    format=F00071_. label="Sex of child(20)";
  attrib B5_01    format=F00072_. label="Child is alive(1)";
  attrib B5_02    format=F00072_. label="Child is alive(2)";
  attrib B5_03    format=F00072_. label="Child is alive(3)";
  attrib B5_04    format=F00072_. label="Child is alive(4)";
  attrib B5_05    format=F00072_. label="Child is alive(5)";
  attrib B5_06    format=F00072_. label="Child is alive(6)";
  attrib B5_07    format=F00072_. label="Child is alive(7)";
  attrib B5_08    format=F00072_. label="Child is alive(8)";
  attrib B5_09    format=F00072_. label="Child is alive(9)";
  attrib B5_10    format=F00072_. label="Child is alive(10)";
  attrib B5_11    format=F00072_. label="Child is alive(11)";
  attrib B5_12    format=F00072_. label="Child is alive(12)";
  attrib B5_13    format=F00072_. label="Child is alive(13)";
  attrib B5_14    format=F00072_. label="Child is alive(14)";
  attrib B5_15    format=F00072_. label="Child is alive(15)";
  attrib B5_16    format=F00072_. label="Child is alive(16)";
  attrib B5_17    format=F00072_. label="Child is alive(17)";
  attrib B5_18    format=F00072_. label="Child is alive(18)";
  attrib B5_19    format=F00072_. label="Child is alive(19)";
  attrib B5_20    format=F00072_. label="Child is alive(20)";
  attrib B6_01    format=F00073_. label="Age at death(1)";
  attrib B6_02    format=F00073_. label="Age at death(2)";
  attrib B6_03    format=F00073_. label="Age at death(3)";
  attrib B6_04    format=F00073_. label="Age at death(4)";
  attrib B6_05    format=F00073_. label="Age at death(5)";
  attrib B6_06    format=F00073_. label="Age at death(6)";
  attrib B6_07    format=F00073_. label="Age at death(7)";
  attrib B6_08    format=F00073_. label="Age at death(8)";
  attrib B6_09    format=F00073_. label="Age at death(9)";
  attrib B6_10    format=F00073_. label="Age at death(10)";
  attrib B6_11    format=F00073_. label="Age at death(11)";
  attrib B6_12    format=F00073_. label="Age at death(12)";
  attrib B6_13    format=F00073_. label="Age at death(13)";
  attrib B6_14    format=F00073_. label="Age at death(14)";
  attrib B6_15    format=F00073_. label="Age at death(15)";
  attrib B6_16    format=F00073_. label="Age at death(16)";
  attrib B6_17    format=F00073_. label="Age at death(17)";
  attrib B6_18    format=F00073_. label="Age at death(18)";
  attrib B6_19    format=F00073_. label="Age at death(19)";
  attrib B6_20    format=F00073_. label="Age at death(20)";
  attrib B7_01    label="Age at death (months, imputed)(1)";
  attrib B7_02    label="Age at death (months, imputed)(2)";
  attrib B7_03    label="Age at death (months, imputed)(3)";
  attrib B7_04    label="Age at death (months, imputed)(4)";
  attrib B7_05    label="Age at death (months, imputed)(5)";
  attrib B7_06    label="Age at death (months, imputed)(6)";
  attrib B7_07    label="Age at death (months, imputed)(7)";
  attrib B7_08    label="Age at death (months, imputed)(8)";
  attrib B7_09    label="Age at death (months, imputed)(9)";
  attrib B7_10    label="Age at death (months, imputed)(10)";
  attrib B7_11    label="Age at death (months, imputed)(11)";
  attrib B7_12    label="Age at death (months, imputed)(12)";
  attrib B7_13    label="Age at death (months, imputed)(13)";
  attrib B7_14    label="Age at death (months, imputed)(14)";
  attrib B7_15    label="Age at death (months, imputed)(15)";
  attrib B7_16    label="Age at death (months, imputed)(16)";
  attrib B7_17    label="Age at death (months, imputed)(17)";
  attrib B7_18    label="Age at death (months, imputed)(18)";
  attrib B7_19    label="Age at death (months, imputed)(19)";
  attrib B7_20    label="Age at death (months, imputed)(20)";
  attrib B8_01    label="Current age of child(1)";
  attrib B8_02    label="Current age of child(2)";
  attrib B8_03    label="Current age of child(3)";
  attrib B8_04    label="Current age of child(4)";
  attrib B8_05    label="Current age of child(5)";
  attrib B8_06    label="Current age of child(6)";
  attrib B8_07    label="Current age of child(7)";
  attrib B8_08    label="Current age of child(8)";
  attrib B8_09    label="Current age of child(9)";
  attrib B8_10    label="Current age of child(10)";
  attrib B8_11    label="Current age of child(11)";
  attrib B8_12    label="Current age of child(12)";
  attrib B8_13    label="Current age of child(13)";
  attrib B8_14    label="Current age of child(14)";
  attrib B8_15    label="Current age of child(15)";
  attrib B8_16    label="Current age of child(16)";
  attrib B8_17    label="Current age of child(17)";
  attrib B8_18    label="Current age of child(18)";
  attrib B8_19    label="Current age of child(19)";
  attrib B8_20    label="Current age of child(20)";
  attrib B9_01    format=F00074_. label="Child lives with whom(1)";
  attrib B9_02    format=F00074_. label="Child lives with whom(2)";
  attrib B9_03    format=F00074_. label="Child lives with whom(3)";
  attrib B9_04    format=F00074_. label="Child lives with whom(4)";
  attrib B9_05    format=F00074_. label="Child lives with whom(5)";
  attrib B9_06    format=F00074_. label="Child lives with whom(6)";
  attrib B9_07    format=F00074_. label="Child lives with whom(7)";
  attrib B9_08    format=F00074_. label="Child lives with whom(8)";
  attrib B9_09    format=F00074_. label="Child lives with whom(9)";
  attrib B9_10    format=F00074_. label="Child lives with whom(10)";
  attrib B9_11    format=F00074_. label="Child lives with whom(11)";
  attrib B9_12    format=F00074_. label="Child lives with whom(12)";
  attrib B9_13    format=F00074_. label="Child lives with whom(13)";
  attrib B9_14    format=F00074_. label="Child lives with whom(14)";
  attrib B9_15    format=F00074_. label="Child lives with whom(15)";
  attrib B9_16    format=F00074_. label="Child lives with whom(16)";
  attrib B9_17    format=F00074_. label="Child lives with whom(17)";
  attrib B9_18    format=F00074_. label="Child lives with whom(18)";
  attrib B9_19    format=F00074_. label="Child lives with whom(19)";
  attrib B9_20    format=F00074_. label="Child lives with whom(20)";
  attrib B10_01   format=F00075_. label="Completeness of information(1)";
  attrib B10_02   format=F00075_. label="Completeness of information(2)";
  attrib B10_03   format=F00075_. label="Completeness of information(3)";
  attrib B10_04   format=F00075_. label="Completeness of information(4)";
  attrib B10_05   format=F00075_. label="Completeness of information(5)";
  attrib B10_06   format=F00075_. label="Completeness of information(6)";
  attrib B10_07   format=F00075_. label="Completeness of information(7)";
  attrib B10_08   format=F00075_. label="Completeness of information(8)";
  attrib B10_09   format=F00075_. label="Completeness of information(9)";
  attrib B10_10   format=F00075_. label="Completeness of information(10)";
  attrib B10_11   format=F00075_. label="Completeness of information(11)";
  attrib B10_12   format=F00075_. label="Completeness of information(12)";
  attrib B10_13   format=F00075_. label="Completeness of information(13)";
  attrib B10_14   format=F00075_. label="Completeness of information(14)";
  attrib B10_15   format=F00075_. label="Completeness of information(15)";
  attrib B10_16   format=F00075_. label="Completeness of information(16)";
  attrib B10_17   format=F00075_. label="Completeness of information(17)";
  attrib B10_18   format=F00075_. label="Completeness of information(18)";
  attrib B10_19   format=F00075_. label="Completeness of information(19)";
  attrib B10_20   format=F00075_. label="Completeness of information(20)";
  attrib B11_01   label="Preceding birth interval (months)(1)";
  attrib B11_02   label="Preceding birth interval (months)(2)";
  attrib B11_03   label="Preceding birth interval (months)(3)";
  attrib B11_04   label="Preceding birth interval (months)(4)";
  attrib B11_05   label="Preceding birth interval (months)(5)";
  attrib B11_06   label="Preceding birth interval (months)(6)";
  attrib B11_07   label="Preceding birth interval (months)(7)";
  attrib B11_08   label="Preceding birth interval (months)(8)";
  attrib B11_09   label="Preceding birth interval (months)(9)";
  attrib B11_10   label="Preceding birth interval (months)(10)";
  attrib B11_11   label="Preceding birth interval (months)(11)";
  attrib B11_12   label="Preceding birth interval (months)(12)";
  attrib B11_13   label="Preceding birth interval (months)(13)";
  attrib B11_14   label="Preceding birth interval (months)(14)";
  attrib B11_15   label="Preceding birth interval (months)(15)";
  attrib B11_16   label="Preceding birth interval (months)(16)";
  attrib B11_17   label="Preceding birth interval (months)(17)";
  attrib B11_18   label="Preceding birth interval (months)(18)";
  attrib B11_19   label="Preceding birth interval (months)(19)";
  attrib B11_20   label="Preceding birth interval (months)(20)";
  attrib B12_01   label="Succeeding birth interval (months)(1)";
  attrib B12_02   label="Succeeding birth interval (months)(2)";
  attrib B12_03   label="Succeeding birth interval (months)(3)";
  attrib B12_04   label="Succeeding birth interval (months)(4)";
  attrib B12_05   label="Succeeding birth interval (months)(5)";
  attrib B12_06   label="Succeeding birth interval (months)(6)";
  attrib B12_07   label="Succeeding birth interval (months)(7)";
  attrib B12_08   label="Succeeding birth interval (months)(8)";
  attrib B12_09   label="Succeeding birth interval (months)(9)";
  attrib B12_10   label="Succeeding birth interval (months)(10)";
  attrib B12_11   label="Succeeding birth interval (months)(11)";
  attrib B12_12   label="Succeeding birth interval (months)(12)";
  attrib B12_13   label="Succeeding birth interval (months)(13)";
  attrib B12_14   label="Succeeding birth interval (months)(14)";
  attrib B12_15   label="Succeeding birth interval (months)(15)";
  attrib B12_16   label="Succeeding birth interval (months)(16)";
  attrib B12_17   label="Succeeding birth interval (months)(17)";
  attrib B12_18   label="Succeeding birth interval (months)(18)";
  attrib B12_19   label="Succeeding birth interval (months)(19)";
  attrib B12_20   label="Succeeding birth interval (months)(20)";
  attrib B13_01   format=F00076_. label="Flag for age at death(1)";
  attrib B13_02   format=F00076_. label="Flag for age at death(2)";
  attrib B13_03   format=F00076_. label="Flag for age at death(3)";
  attrib B13_04   format=F00076_. label="Flag for age at death(4)";
  attrib B13_05   format=F00076_. label="Flag for age at death(5)";
  attrib B13_06   format=F00076_. label="Flag for age at death(6)";
  attrib B13_07   format=F00076_. label="Flag for age at death(7)";
  attrib B13_08   format=F00076_. label="Flag for age at death(8)";
  attrib B13_09   format=F00076_. label="Flag for age at death(9)";
  attrib B13_10   format=F00076_. label="Flag for age at death(10)";
  attrib B13_11   format=F00076_. label="Flag for age at death(11)";
  attrib B13_12   format=F00076_. label="Flag for age at death(12)";
  attrib B13_13   format=F00076_. label="Flag for age at death(13)";
  attrib B13_14   format=F00076_. label="Flag for age at death(14)";
  attrib B13_15   format=F00076_. label="Flag for age at death(15)";
  attrib B13_16   format=F00076_. label="Flag for age at death(16)";
  attrib B13_17   format=F00076_. label="Flag for age at death(17)";
  attrib B13_18   format=F00076_. label="Flag for age at death(18)";
  attrib B13_19   format=F00076_. label="Flag for age at death(19)";
  attrib B13_20   format=F00076_. label="Flag for age at death(20)";
  attrib B15_01   format=F00077_. label="Live birth between births(1)";
  attrib B15_02   format=F00077_. label="Live birth between births(2)";
  attrib B15_03   format=F00077_. label="Live birth between births(3)";
  attrib B15_04   format=F00077_. label="Live birth between births(4)";
  attrib B15_05   format=F00077_. label="Live birth between births(5)";
  attrib B15_06   format=F00077_. label="Live birth between births(6)";
  attrib B15_07   format=F00077_. label="Live birth between births(7)";
  attrib B15_08   format=F00077_. label="Live birth between births(8)";
  attrib B15_09   format=F00077_. label="Live birth between births(9)";
  attrib B15_10   format=F00077_. label="Live birth between births(10)";
  attrib B15_11   format=F00077_. label="Live birth between births(11)";
  attrib B15_12   format=F00077_. label="Live birth between births(12)";
  attrib B15_13   format=F00077_. label="Live birth between births(13)";
  attrib B15_14   format=F00077_. label="Live birth between births(14)";
  attrib B15_15   format=F00077_. label="Live birth between births(15)";
  attrib B15_16   format=F00077_. label="Live birth between births(16)";
  attrib B15_17   format=F00077_. label="Live birth between births(17)";
  attrib B15_18   format=F00077_. label="Live birth between births(18)";
  attrib B15_19   format=F00077_. label="Live birth between births(19)";
  attrib B15_20   format=F00077_. label="Live birth between births(20)";
  attrib B16_01   format=F00078_. label="Child's line number in household(1)";
  attrib B16_02   format=F00078_. label="Child's line number in household(2)";
  attrib B16_03   format=F00078_. label="Child's line number in household(3)";
  attrib B16_04   format=F00078_. label="Child's line number in household(4)";
  attrib B16_05   format=F00078_. label="Child's line number in household(5)";
  attrib B16_06   format=F00078_. label="Child's line number in household(6)";
  attrib B16_07   format=F00078_. label="Child's line number in household(7)";
  attrib B16_08   format=F00078_. label="Child's line number in household(8)";
  attrib B16_09   format=F00078_. label="Child's line number in household(9)";
  attrib B16_10   format=F00078_. label="Child's line number in household(10)";
  attrib B16_11   format=F00078_. label="Child's line number in household(11)";
  attrib B16_12   format=F00078_. label="Child's line number in household(12)";
  attrib B16_13   format=F00078_. label="Child's line number in household(13)";
  attrib B16_14   format=F00078_. label="Child's line number in household(14)";
  attrib B16_15   format=F00078_. label="Child's line number in household(15)";
  attrib B16_16   format=F00078_. label="Child's line number in household(16)";
  attrib B16_17   format=F00078_. label="Child's line number in household(17)";
  attrib B16_18   format=F00078_. label="Child's line number in household(18)";
  attrib B16_19   format=F00078_. label="Child's line number in household(19)";
  attrib B16_20   format=F00078_. label="Child's line number in household(20)";
  attrib B17_01   label="Day of birth(1)";
  attrib B17_02   label="Day of birth(2)";
  attrib B17_03   label="Day of birth(3)";
  attrib B17_04   label="Day of birth(4)";
  attrib B17_05   label="Day of birth(5)";
  attrib B17_06   label="Day of birth(6)";
  attrib B17_07   label="Day of birth(7)";
  attrib B17_08   label="Day of birth(8)";
  attrib B17_09   label="Day of birth(9)";
  attrib B17_10   label="Day of birth(10)";
  attrib B17_11   label="Day of birth(11)";
  attrib B17_12   label="Day of birth(12)";
  attrib B17_13   label="Day of birth(13)";
  attrib B17_14   label="Day of birth(14)";
  attrib B17_15   label="Day of birth(15)";
  attrib B17_16   label="Day of birth(16)";
  attrib B17_17   label="Day of birth(17)";
  attrib B17_18   label="Day of birth(18)";
  attrib B17_19   label="Day of birth(19)";
  attrib B17_20   label="Day of birth(20)";
  attrib B18_01   label="Century Day Code of birth (CDC)(1)";
  attrib B18_02   label="Century Day Code of birth (CDC)(2)";
  attrib B18_03   label="Century Day Code of birth (CDC)(3)";
  attrib B18_04   label="Century Day Code of birth (CDC)(4)";
  attrib B18_05   label="Century Day Code of birth (CDC)(5)";
  attrib B18_06   label="Century Day Code of birth (CDC)(6)";
  attrib B18_07   label="Century Day Code of birth (CDC)(7)";
  attrib B18_08   label="Century Day Code of birth (CDC)(8)";
  attrib B18_09   label="Century Day Code of birth (CDC)(9)";
  attrib B18_10   label="Century Day Code of birth (CDC)(10)";
  attrib B18_11   label="Century Day Code of birth (CDC)(11)";
  attrib B18_12   label="Century Day Code of birth (CDC)(12)";
  attrib B18_13   label="Century Day Code of birth (CDC)(13)";
  attrib B18_14   label="Century Day Code of birth (CDC)(14)";
  attrib B18_15   label="Century Day Code of birth (CDC)(15)";
  attrib B18_16   label="Century Day Code of birth (CDC)(16)";
  attrib B18_17   label="Century Day Code of birth (CDC)(17)";
  attrib B18_18   label="Century Day Code of birth (CDC)(18)";
  attrib B18_19   label="Century Day Code of birth (CDC)(19)";
  attrib B18_20   label="Century Day Code of birth (CDC)(20)";
  attrib B19_01   label="Current age of child in months(1)";
  attrib B19_02   label="Current age of child in months(2)";
  attrib B19_03   label="Current age of child in months(3)";
  attrib B19_04   label="Current age of child in months(4)";
  attrib B19_05   label="Current age of child in months(5)";
  attrib B19_06   label="Current age of child in months(6)";
  attrib B19_07   label="Current age of child in months(7)";
  attrib B19_08   label="Current age of child in months(8)";
  attrib B19_09   label="Current age of child in months(9)";
  attrib B19_10   label="Current age of child in months(10)";
  attrib B19_11   label="Current age of child in months(11)";
  attrib B19_12   label="Current age of child in months(12)";
  attrib B19_13   label="Current age of child in months(13)";
  attrib B19_14   label="Current age of child in months(14)";
  attrib B19_15   label="Current age of child in months(15)";
  attrib B19_16   label="Current age of child in months(16)";
  attrib B19_17   label="Current age of child in months(17)";
  attrib B19_18   label="Current age of child in months(18)";
  attrib B19_19   label="Current age of child in months(19)";
  attrib B19_20   label="Current age of child in months(20)";
  attrib B20_01   label="Duration of pregnancy(1)";
  attrib B20_02   label="Duration of pregnancy(2)";
  attrib B20_03   label="Duration of pregnancy(3)";
  attrib B20_04   label="Duration of pregnancy(4)";
  attrib B20_05   label="Duration of pregnancy(5)";
  attrib B20_06   label="Duration of pregnancy(6)";
  attrib B20_07   label="Duration of pregnancy(7)";
  attrib B20_08   label="Duration of pregnancy(8)";
  attrib B20_09   label="Duration of pregnancy(9)";
  attrib B20_10   label="Duration of pregnancy(10)";
  attrib B20_11   label="Duration of pregnancy(11)";
  attrib B20_12   label="Duration of pregnancy(12)";
  attrib B20_13   label="Duration of pregnancy(13)";
  attrib B20_14   label="Duration of pregnancy(14)";
  attrib B20_15   label="Duration of pregnancy(15)";
  attrib B20_16   label="Duration of pregnancy(16)";
  attrib B20_17   label="Duration of pregnancy(17)";
  attrib B20_18   label="Duration of pregnancy(18)";
  attrib B20_19   label="Duration of pregnancy(19)";
  attrib B20_20   label="Duration of pregnancy(20)";
  attrib V201     label="Total children ever born";
  attrib V202     label="Sons at home";
  attrib V203     label="Daughters at home";
  attrib V204     label="Sons elsewhere";
  attrib V205     label="Daughters elsewhere";
  attrib V206     label="Sons who have died";
  attrib V207     label="Daughters who have died";
  attrib V208     format=F00079_. label="Births in last five years";
  attrib V209     format=F00080_. label="Births in past year";
  attrib V210     label="Births in month of interview";
  attrib V211     label="Date of first birth (CMC)";
  attrib V212     label="Age of respondent at 1st birth";
  attrib V213     format=F00081_. label="Currently pregnant";
  attrib V214     label="Duration of current pregnancy";
  attrib V215     format=F00082_. label="Time since last menstrual period";
  attrib V216     format=F00083_. label="Menstruated in last six weeks";
  attrib V217     format=F00084_. label="Knowledge of ovulatory cycle";
  attrib V218     label="Number of living children";
  attrib V219     label="Living children + current pregnancy";
  attrib V220     format=F00085_. label="Living children + current pregnancy (grouped)";
  attrib V221     format=F00086_. label="Marriage to first birth interval (months)";
  attrib V222     label="Last birth to interview (months)";
  attrib V223     format=F00087_. label="Completeness of current pregnancy information";
  attrib V224     label="Entries in birth history";
  attrib V225     format=F00088_. label="Current pregnancy wanted";
  attrib V226     format=F00089_. label="Time since last period (comp) (months)";
  attrib V227     format=F00090_. label="Flag for last period";
  attrib V228     format=F00091_. label="Ever had a terminated pregnancy";
  attrib V229     format=F00092_. label="Month pregnancy ended";
  attrib V230     format=F00093_. label="Year pregnancy ended";
  attrib V231     format=F00094_. label="CMC pregnancy ended";
  attrib V232     format=F00095_. label="Completeness of last termination information";
  attrib V233     format=F00096_. label="Months when pregnancy ended";
  attrib V234     format=F00097_. label="Other such pregnancies";
  attrib V235     format=F00098_. label="Index last child prior to maternity-health (calendar)";
  attrib V237     format=F00099_. label="Birth between last and interview";
  attrib V238     label="Births in last three years";
  attrib V239     format=F00100_. label="Pregnancies terminated before calendar beginning";
  attrib V240     format=F00101_. label="Month of last termination prior to calendar";
  attrib V241     format=F00102_. label="Year of last termination prior to calendar";
  attrib V242     format=F00103_. label="CMC termination ended prior to calendar";
  attrib V243     format=F00104_. label="Completeness of last termination date prior to calendar";
  attrib V244     format=F00105_. label="NA - Can women get pregnant after birth and before period";
  attrib V301     format=F00106_. label="Knowledge of any method";
  attrib V302     format=F00107_. label="Ever use of any method";
  attrib V302A    format=F00108_. label="NA - Ever used anything or tried to delay or avoid getting pregnant";
  attrib V304A_01 format=F00109_. label="Type of method(1)";
  attrib V304A_02 format=F00109_. label="Type of method(2)";
  attrib V304A_03 format=F00109_. label="Type of method(3)";
  attrib V304A_04 format=F00109_. label="Type of method(4)";
  attrib V304A_05 format=F00109_. label="Type of method(5)";
  attrib V304A_06 format=F00109_. label="Type of method(6)";
  attrib V304A_07 format=F00109_. label="Type of method(7)";
  attrib V304A_08 format=F00109_. label="Type of method(8)";
  attrib V304A_09 format=F00109_. label="Type of method(9)";
  attrib V304A_10 format=F00109_. label="Type of method(10)";
  attrib V304A_11 format=F00109_. label="Type of method(11)";
  attrib V304A_12 format=F00109_. label="Type of method(12)";
  attrib V304A_13 format=F00109_. label="Type of method(13)";
  attrib V304A_14 format=F00109_. label="Type of method(14)";
  attrib V304A_15 format=F00109_. label="Type of method(15)";
  attrib V304A_16 format=F00109_. label="Type of method(16)";
  attrib V304A_17 format=F00109_. label="Type of method(17)";
  attrib V304A_18 format=F00109_. label="Type of method(18)";
  attrib V304A_19 format=F00109_. label="Type of method(19)";
  attrib V304A_20 format=F00109_. label="Type of method(20)";
  attrib V304_01  format=F00110_. label="Knows method(1)";
  attrib V304_02  format=F00110_. label="Knows method(2)";
  attrib V304_03  format=F00110_. label="Knows method(3)";
  attrib V304_04  format=F00110_. label="Knows method(4)";
  attrib V304_05  format=F00110_. label="Knows method(5)";
  attrib V304_06  format=F00110_. label="Knows method(6)";
  attrib V304_07  format=F00110_. label="Knows method(7)";
  attrib V304_08  format=F00110_. label="Knows method(8)";
  attrib V304_09  format=F00110_. label="Knows method(9)";
  attrib V304_10  format=F00110_. label="Knows method(10)";
  attrib V304_11  format=F00110_. label="Knows method(11)";
  attrib V304_12  format=F00110_. label="Knows method(12)";
  attrib V304_13  format=F00110_. label="Knows method(13)";
  attrib V304_14  format=F00110_. label="Knows method(14)";
  attrib V304_15  format=F00110_. label="Knows method(15)";
  attrib V304_16  format=F00110_. label="Knows method(16)";
  attrib V304_17  format=F00110_. label="Knows method(17)";
  attrib V304_18  format=F00110_. label="Knows method(18)";
  attrib V304_19  format=F00110_. label="Knows method(19)";
  attrib V304_20  format=F00110_. label="Knows method(20)";
  attrib V305_01  format=F00111_. label="Ever used method(1)";
  attrib V305_02  format=F00111_. label="Ever used method(2)";
  attrib V305_03  format=F00111_. label="Ever used method(3)";
  attrib V305_04  format=F00111_. label="Ever used method(4)";
  attrib V305_05  format=F00111_. label="Ever used method(5)";
  attrib V305_06  format=F00111_. label="Ever used method(6)";
  attrib V305_07  format=F00111_. label="Ever used method(7)";
  attrib V305_08  format=F00111_. label="Ever used method(8)";
  attrib V305_09  format=F00111_. label="Ever used method(9)";
  attrib V305_10  format=F00111_. label="Ever used method(10)";
  attrib V305_11  format=F00111_. label="Ever used method(11)";
  attrib V305_12  format=F00111_. label="Ever used method(12)";
  attrib V305_13  format=F00111_. label="Ever used method(13)";
  attrib V305_14  format=F00111_. label="Ever used method(14)";
  attrib V305_15  format=F00111_. label="Ever used method(15)";
  attrib V305_16  format=F00111_. label="Ever used method(16)";
  attrib V305_17  format=F00111_. label="Ever used method(17)";
  attrib V305_18  format=F00111_. label="Ever used method(18)";
  attrib V305_19  format=F00111_. label="Ever used method(19)";
  attrib V305_20  format=F00111_. label="Ever used method(20)";
  attrib V307_01  format=F00112_. label="Method currently used(1)";
  attrib V307_02  format=F00112_. label="Method currently used(2)";
  attrib V307_03  format=F00112_. label="Method currently used(3)";
  attrib V307_04  format=F00112_. label="Method currently used(4)";
  attrib V307_05  format=F00112_. label="Method currently used(5)";
  attrib V307_06  format=F00112_. label="Method currently used(6)";
  attrib V307_07  format=F00112_. label="Method currently used(7)";
  attrib V307_08  format=F00112_. label="Method currently used(8)";
  attrib V307_09  format=F00112_. label="Method currently used(9)";
  attrib V307_10  format=F00112_. label="Method currently used(10)";
  attrib V307_11  format=F00112_. label="Method currently used(11)";
  attrib V307_12  format=F00112_. label="Method currently used(12)";
  attrib V307_13  format=F00112_. label="Method currently used(13)";
  attrib V307_14  format=F00112_. label="Method currently used(14)";
  attrib V307_15  format=F00112_. label="Method currently used(15)";
  attrib V307_16  format=F00112_. label="Method currently used(16)";
  attrib V307_17  format=F00112_. label="Method currently used(17)";
  attrib V307_18  format=F00112_. label="Method currently used(18)";
  attrib V307_19  format=F00112_. label="Method currently used(19)";
  attrib V307_20  format=F00112_. label="Method currently used(20)";
  attrib V310     label="Living children at first use";
  attrib V311     format=F00113_. label="Children at first use (grouped)";
  attrib V312     format=F00114_. label="Current contraceptive method";
  attrib V313     format=F00115_. label="Current use by method type";
  attrib V315     label="Month of start of use of method";
  attrib V316     label="Year of start of use of method";
  attrib V317     label="Date of start of use of method (CMC)";
  attrib V318     format=F00116_. label="Completeness of information";
  attrib V319     format=F00117_. label="Years since sterilization";
  attrib V320     format=F00118_. label="Age at sterilization";
  attrib V321     format=F00119_. label="Marital duration at sterilization";
  attrib V322     format=F00120_. label="Parity at sterilization";
  attrib V323     format=F00121_. label="NA - Brand of pill used";
  attrib V323A    format=F00122_. label="NA - Brand of condom used";
  attrib V325A    format=F00123_. label="NA - Cost of current method (in CURRENCY)";
  attrib V326     format=F00124_. label="Last source for current users";
  attrib V327     format=F00125_. label="Last source for users by type";
  attrib V337     format=F00126_. label="Months of use of current method";
  attrib V359     format=F00127_. label="Last method discontinued in last 5 years";
  attrib V360     format=F00128_. label="Reason of last discontinuation";
  attrib V361     format=F00129_. label="Pattern of use";
  attrib V362     format=F00130_. label="Intention to use";
  attrib V363     format=F00131_. label="Preferred future method";
  attrib V364     format=F00132_. label="Contraceptive use and intention";
  attrib V367     format=F00133_. label="Wanted last child";
  attrib V372     format=F00134_. label="NA - Shown pill package";
  attrib V372A    format=F00135_. label="NA - Shown condom package";
  attrib V375A    format=F00136_. label="Main reason not using a method";
  attrib V376     format=F00137_. label="Main reason not to use a method";
  attrib V376A    format=F00138_. label="Would ever use method if married";
  attrib V379     format=F00139_. label="NA - Source known for any method";
  attrib V380     format=F00140_. label="NA - Source known for any method";
  attrib V384A    format=F00141_. label="Heard family planning on radio last few months";
  attrib V384B    format=F00142_. label="Heard family planning on TV last few months";
  attrib V384C    format=F00143_. label="Heard family planning in newspaper/magazine last few months";
  attrib V384D    format=F00144_. label="NA - Heard family planning by text messages on mobile phone";
  attrib V393     format=F00145_. label="Visited by fieldworker in last 12 months";
  attrib V393A    format=F00146_. label="NA - Did fieldworker talk about family planning";
  attrib V394     format=F00147_. label="Visited health facility last 12 months";
  attrib V395     format=F00148_. label="At health facility, told of family planning";
  attrib V3A00A   format=F00149_. label="Source of family planning for non-users: government hospital/health center/health post";
  attrib V3A00B   format=F00150_. label="NA - Source of family planning for non-users: CS public sector";
  attrib V3A00C   format=F00151_. label="NA - Source of family planning for non-users: CS public sector";
  attrib V3A00D   format=F00152_. label="NA - Source of family planning for non-users: CS public sector";
  attrib V3A00E   format=F00153_. label="NA - Source of family planning for non-users: CS public sector";
  attrib V3A00F   format=F00154_. label="NA - Source of family planning for non-users: CS public sector";
  attrib V3A00G   format=F00155_. label="NA - Source of family planning for non-users: CS public sector";
  attrib V3A00H   format=F00156_. label="NA - Source of family planning for non-users: CS public sector";
  attrib V3A00I   format=F00157_. label="NA - Source of family planning for non-users: CS public sector";
  attrib V3A00J   format=F00158_. label="Source of family planning for non-users: private hospital/clinic";
  attrib V3A00K   format=F00159_. label="Source of family planning for non-users: pharmacy";
  attrib V3A00L   format=F00160_. label="Source of family planning for non-users: private doctor";
  attrib V3A00M   format=F00161_. label="Source of family planning for non-users: EPS health center";
  attrib V3A00N   format=F00162_. label="Source of family planning for non-users: PROFAMILIA";
  attrib V3A00O   format=F00163_. label="NA - Source of family planning for non-users: CS private sector";
  attrib V3A00P   format=F00164_. label="NA - Source of family planning for non-users: CS private sector";
  attrib V3A00Q   format=F00165_. label="NA - Source of family planning for non-users: CS private sector";
  attrib V3A00R   format=F00166_. label="NA - Source of family planning for non-users: CS private sector";
  attrib V3A00S   format=F00167_. label="Source of family planning for non-users: shop";
  attrib V3A00T   format=F00168_. label="NA - Source of family planning for non-users: CS other";
  attrib V3A00U   format=F00169_. label="NA - Source of family planning for non-users: CS other";
  attrib V3A00V   format=F00170_. label="NA - Source of family planning for non-users: CS other";
  attrib V3A00W   format=F00171_. label="NA - Source of family planning for non-users: CS other";
  attrib V3A00X   format=F00172_. label="Source of family planning for non-users: other";
  attrib V3A00Y   format=F00173_. label="Source of family planning for non-users: no source";
  attrib V3A00Z   format=F00174_. label="Source of family planning for non-users: any source";
  attrib V3A01    format=F00175_. label="Told sterilization would mean no more children";
  attrib V3A02    format=F00176_. label="Told about side effects";
  attrib V3A03    format=F00177_. label="NA - Told about side effects by health or family planning worker";
  attrib V3A04    format=F00178_. label="Told how to deal with side effects";
  attrib V3A05    format=F00179_. label="Told about other family planning methods";
  attrib V3A06    format=F00180_. label="NA - Told about other family planning methods by health or family planning worker";
  attrib V3A07    format=F00181_. label="First source for current method";
  attrib V3A08A   format=F00182_. label="Reason not using: not married";
  attrib V3A08B   format=F00183_. label="Reason not using: not having sex with men";
  attrib V3A08C   format=F00184_. label="Reason not using: infrequent sex";
  attrib V3A08D   format=F00185_. label="Reason not using: menopausal";
  attrib V3A08E   format=F00186_. label="Reason not using: subfecund/infecund";
  attrib V3A08F   format=F00187_. label="Reason not using: postpartum amenorrheic";
  attrib V3A08G   format=F00188_. label="Reason not using: hysterectomy";
  attrib V3A08H   format=F00189_. label="NA - Reason not using: fatalistic";
  attrib V3A08I   format=F00190_. label="Reason not using: respondent opposed";
  attrib V3A08J   format=F00191_. label="Reason not using: husband/partner opposed";
  attrib V3A08K   format=F00192_. label="Reason not using: others opposed";
  attrib V3A08L   format=F00193_. label="Reason not using: parents opposed";
  attrib V3A08M   format=F00194_. label="Reason not using: knows no method";
  attrib V3A08N   format=F00195_. label="Reason not using: knows no source";
  attrib V3A08O   format=F00196_. label="Reason not using: health concerns";
  attrib V3A08P   format=F00197_. label="Reason not using: fear of side effects/health concerns";
  attrib V3A08Q   format=F00198_. label="Reason not using: lack of access/too far";
  attrib V3A08R   format=F00199_. label="Reason not using: costs too much";
  attrib V3A08S   format=F00200_. label="NA - Reason not using: inconvenient to use";
  attrib V3A08T   format=F00201_. label="NA - Reason not using: interferes with body’s processes";
  attrib V3A08U   format=F00202_. label="NA - Reason not using: preferred method not available";
  attrib V3A08V   format=F00203_. label="NA - Reason not using: no method available";
  attrib V3A08W   format=F00204_. label="Reason not using: have sex with other women";
  attrib V3A08AA  format=F00205_. label="Reason not using: personal reasons";
  attrib V3A08AB  format=F00206_. label="Reason not using: too much paper work";
  attrib V3A08AC  format=F00207_. label="Reason not using: not affiliated to health insurance";
  attrib V3A08AD  format=F00208_. label="Reason not using: didn't know could use service (RC/RS) for FP";
  attrib V3A08X   format=F00209_. label="Reason not using: other";
  attrib V3A08Z   format=F00210_. label="Reason not using: don't know";
  attrib V3A09A   format=F00211_. label="NA - How many pill cycles did get last time";
  attrib V3A09B   format=F00212_. label="NA - How many condoms did get last time";
  attrib MIDX_1   label="Index to Birth History(1)";
  attrib MIDX_2   label="Index to Birth History(2)";
  attrib MIDX_3   label="Index to Birth History(3)";
  attrib MIDX_4   label="Index to Birth History(4)";
  attrib MIDX_5   label="Index to Birth History(5)";
  attrib MIDX_6   label="Index to Birth History(6)";
  attrib M1_1     format=F00213_. label="Number of tetanus injections before birth(1)";
  attrib M1_2     format=F00213_. label="Number of tetanus injections before birth(2)";
  attrib M1_3     format=F00213_. label="Number of tetanus injections before birth(3)";
  attrib M1_4     format=F00213_. label="Number of tetanus injections before birth(4)";
  attrib M1_5     format=F00213_. label="Number of tetanus injections before birth(5)";
  attrib M1_6     format=F00213_. label="Number of tetanus injections before birth(6)";
  attrib M1A_1    format=F00214_. label="NA - Number of tetanus injections before pregnancy(1)";
  attrib M1A_2    format=F00214_. label="NA - Number of tetanus injections before pregnancy(2)";
  attrib M1A_3    format=F00214_. label="NA - Number of tetanus injections before pregnancy(3)";
  attrib M1A_4    format=F00214_. label="NA - Number of tetanus injections before pregnancy(4)";
  attrib M1A_5    format=F00214_. label="NA - Number of tetanus injections before pregnancy(5)";
  attrib M1A_6    format=F00214_. label="NA - Number of tetanus injections before pregnancy(6)";
  attrib M1B_1    format=F00215_. label="NA - Month of last tetanus injection before pregnancy(1)";
  attrib M1B_2    format=F00215_. label="NA - Month of last tetanus injection before pregnancy(2)";
  attrib M1B_3    format=F00215_. label="NA - Month of last tetanus injection before pregnancy(3)";
  attrib M1B_4    format=F00215_. label="NA - Month of last tetanus injection before pregnancy(4)";
  attrib M1B_5    format=F00215_. label="NA - Month of last tetanus injection before pregnancy(5)";
  attrib M1B_6    format=F00215_. label="NA - Month of last tetanus injection before pregnancy(6)";
  attrib M1C_1    format=F00216_. label="NA - Year of last tetanus injection before pregnancy(1)";
  attrib M1C_2    format=F00216_. label="NA - Year of last tetanus injection before pregnancy(2)";
  attrib M1C_3    format=F00216_. label="NA - Year of last tetanus injection before pregnancy(3)";
  attrib M1C_4    format=F00216_. label="NA - Year of last tetanus injection before pregnancy(4)";
  attrib M1C_5    format=F00216_. label="NA - Year of last tetanus injection before pregnancy(5)";
  attrib M1C_6    format=F00216_. label="NA - Year of last tetanus injection before pregnancy(6)";
  attrib M1D_1    label="NA - Years ago received last tetanus injection before pregnancy(1)";
  attrib M1D_2    label="NA - Years ago received last tetanus injection before pregnancy(2)";
  attrib M1D_3    label="NA - Years ago received last tetanus injection before pregnancy(3)";
  attrib M1D_4    label="NA - Years ago received last tetanus injection before pregnancy(4)";
  attrib M1D_5    label="NA - Years ago received last tetanus injection before pregnancy(5)";
  attrib M1D_6    label="NA - Years ago received last tetanus injection before pregnancy(6)";
  attrib M1E_1    format=F00217_. label="NA - Last tetanus injection before pregnancy (CMC)(1)";
  attrib M1E_2    format=F00217_. label="NA - Last tetanus injection before pregnancy (CMC)(2)";
  attrib M1E_3    format=F00217_. label="NA - Last tetanus injection before pregnancy (CMC)(3)";
  attrib M1E_4    format=F00217_. label="NA - Last tetanus injection before pregnancy (CMC)(4)";
  attrib M1E_5    format=F00217_. label="NA - Last tetanus injection before pregnancy (CMC)(5)";
  attrib M1E_6    format=F00217_. label="NA - Last tetanus injection before pregnancy (CMC)(6)";
  attrib M2A_1    format=F00218_. label="Prenatal: doctor(1)";
  attrib M2A_2    format=F00218_. label="Prenatal: doctor(2)";
  attrib M2A_3    format=F00218_. label="Prenatal: doctor(3)";
  attrib M2A_4    format=F00218_. label="Prenatal: doctor(4)";
  attrib M2A_5    format=F00218_. label="Prenatal: doctor(5)";
  attrib M2A_6    format=F00218_. label="Prenatal: doctor(6)";
  attrib M2B_1    format=F00219_. label="Prenatal: nurse(1)";
  attrib M2B_2    format=F00219_. label="Prenatal: nurse(2)";
  attrib M2B_3    format=F00219_. label="Prenatal: nurse(3)";
  attrib M2B_4    format=F00219_. label="Prenatal: nurse(4)";
  attrib M2B_5    format=F00219_. label="Prenatal: nurse(5)";
  attrib M2B_6    format=F00219_. label="Prenatal: nurse(6)";
  attrib M2C_1    format=F00220_. label="Prenatal: auxiliary nurse(1)";
  attrib M2C_2    format=F00220_. label="Prenatal: auxiliary nurse(2)";
  attrib M2C_3    format=F00220_. label="Prenatal: auxiliary nurse(3)";
  attrib M2C_4    format=F00220_. label="Prenatal: auxiliary nurse(4)";
  attrib M2C_5    format=F00220_. label="Prenatal: auxiliary nurse(5)";
  attrib M2C_6    format=F00220_. label="Prenatal: auxiliary nurse(6)";
  attrib M2D_1    format=F00221_. label="NA - Prenatal: CS health professional(1)";
  attrib M2D_2    format=F00221_. label="NA - Prenatal: CS health professional(2)";
  attrib M2D_3    format=F00221_. label="NA - Prenatal: CS health professional(3)";
  attrib M2D_4    format=F00221_. label="NA - Prenatal: CS health professional(4)";
  attrib M2D_5    format=F00221_. label="NA - Prenatal: CS health professional(5)";
  attrib M2D_6    format=F00221_. label="NA - Prenatal: CS health professional(6)";
  attrib M2E_1    format=F00222_. label="NA - Prenatal: CS health professional(1)";
  attrib M2E_2    format=F00222_. label="NA - Prenatal: CS health professional(2)";
  attrib M2E_3    format=F00222_. label="NA - Prenatal: CS health professional(3)";
  attrib M2E_4    format=F00222_. label="NA - Prenatal: CS health professional(4)";
  attrib M2E_5    format=F00222_. label="NA - Prenatal: CS health professional(5)";
  attrib M2E_6    format=F00222_. label="NA - Prenatal: CS health professional(6)";
  attrib M2F_1    format=F00223_. label="NA - Prenatal: CS health professional(1)";
  attrib M2F_2    format=F00223_. label="NA - Prenatal: CS health professional(2)";
  attrib M2F_3    format=F00223_. label="NA - Prenatal: CS health professional(3)";
  attrib M2F_4    format=F00223_. label="NA - Prenatal: CS health professional(4)";
  attrib M2F_5    format=F00223_. label="NA - Prenatal: CS health professional(5)";
  attrib M2F_6    format=F00223_. label="NA - Prenatal: CS health professional(6)";
  attrib M2G_1    format=F00224_. label="Prenatal: traditional birth attendant(1)";
  attrib M2G_2    format=F00224_. label="Prenatal: traditional birth attendant(2)";
  attrib M2G_3    format=F00224_. label="Prenatal: traditional birth attendant(3)";
  attrib M2G_4    format=F00224_. label="Prenatal: traditional birth attendant(4)";
  attrib M2G_5    format=F00224_. label="Prenatal: traditional birth attendant(5)";
  attrib M2G_6    format=F00224_. label="Prenatal: traditional birth attendant(6)";
  attrib M2H_1    format=F00225_. label="NA - Prenatal: CS other person(1)";
  attrib M2H_2    format=F00225_. label="NA - Prenatal: CS other person(2)";
  attrib M2H_3    format=F00225_. label="NA - Prenatal: CS other person(3)";
  attrib M2H_4    format=F00225_. label="NA - Prenatal: CS other person(4)";
  attrib M2H_5    format=F00225_. label="NA - Prenatal: CS other person(5)";
  attrib M2H_6    format=F00225_. label="NA - Prenatal: CS other person(6)";
  attrib M2I_1    format=F00226_. label="NA - Prenatal: CS other person(1)";
  attrib M2I_2    format=F00226_. label="NA - Prenatal: CS other person(2)";
  attrib M2I_3    format=F00226_. label="NA - Prenatal: CS other person(3)";
  attrib M2I_4    format=F00226_. label="NA - Prenatal: CS other person(4)";
  attrib M2I_5    format=F00226_. label="NA - Prenatal: CS other person(5)";
  attrib M2I_6    format=F00226_. label="NA - Prenatal: CS other person(6)";
  attrib M2J_1    format=F00227_. label="NA - Prenatal: CS other person(1)";
  attrib M2J_2    format=F00227_. label="NA - Prenatal: CS other person(2)";
  attrib M2J_3    format=F00227_. label="NA - Prenatal: CS other person(3)";
  attrib M2J_4    format=F00227_. label="NA - Prenatal: CS other person(4)";
  attrib M2J_5    format=F00227_. label="NA - Prenatal: CS other person(5)";
  attrib M2J_6    format=F00227_. label="NA - Prenatal: CS other person(6)";
  attrib M2K_1    format=F00228_. label="Prenatal: other(1)";
  attrib M2K_2    format=F00228_. label="Prenatal: other(2)";
  attrib M2K_3    format=F00228_. label="Prenatal: other(3)";
  attrib M2K_4    format=F00228_. label="Prenatal: other(4)";
  attrib M2K_5    format=F00228_. label="Prenatal: other(5)";
  attrib M2K_6    format=F00228_. label="Prenatal: other(6)";
  attrib M2L_1    format=F00229_. label="NA - Prenatal: CS other(1)";
  attrib M2L_2    format=F00229_. label="NA - Prenatal: CS other(2)";
  attrib M2L_3    format=F00229_. label="NA - Prenatal: CS other(3)";
  attrib M2L_4    format=F00229_. label="NA - Prenatal: CS other(4)";
  attrib M2L_5    format=F00229_. label="NA - Prenatal: CS other(5)";
  attrib M2L_6    format=F00229_. label="NA - Prenatal: CS other(6)";
  attrib M2M_1    format=F00230_. label="NA - Prenatal: CS other(1)";
  attrib M2M_2    format=F00230_. label="NA - Prenatal: CS other(2)";
  attrib M2M_3    format=F00230_. label="NA - Prenatal: CS other(3)";
  attrib M2M_4    format=F00230_. label="NA - Prenatal: CS other(4)";
  attrib M2M_5    format=F00230_. label="NA - Prenatal: CS other(5)";
  attrib M2M_6    format=F00230_. label="NA - Prenatal: CS other(6)";
  attrib M2N_1    format=F00231_. label="Prenatal: no one(1)";
  attrib M2N_2    format=F00231_. label="Prenatal: no one(2)";
  attrib M2N_3    format=F00231_. label="Prenatal: no one(3)";
  attrib M2N_4    format=F00231_. label="Prenatal: no one(4)";
  attrib M2N_5    format=F00231_. label="Prenatal: no one(5)";
  attrib M2N_6    format=F00231_. label="Prenatal: no one(6)";
  attrib M3A_1    format=F00232_. label="Assistance: doctor(1)";
  attrib M3A_2    format=F00232_. label="Assistance: doctor(2)";
  attrib M3A_3    format=F00232_. label="Assistance: doctor(3)";
  attrib M3A_4    format=F00232_. label="Assistance: doctor(4)";
  attrib M3A_5    format=F00232_. label="Assistance: doctor(5)";
  attrib M3A_6    format=F00232_. label="Assistance: doctor(6)";
  attrib M3B_1    format=F00233_. label="Assistance: nurse(1)";
  attrib M3B_2    format=F00233_. label="Assistance: nurse(2)";
  attrib M3B_3    format=F00233_. label="Assistance: nurse(3)";
  attrib M3B_4    format=F00233_. label="Assistance: nurse(4)";
  attrib M3B_5    format=F00233_. label="Assistance: nurse(5)";
  attrib M3B_6    format=F00233_. label="Assistance: nurse(6)";
  attrib M3C_1    format=F00234_. label="Assistance: auxiliary nurse(1)";
  attrib M3C_2    format=F00234_. label="Assistance: auxiliary nurse(2)";
  attrib M3C_3    format=F00234_. label="Assistance: auxiliary nurse(3)";
  attrib M3C_4    format=F00234_. label="Assistance: auxiliary nurse(4)";
  attrib M3C_5    format=F00234_. label="Assistance: auxiliary nurse(5)";
  attrib M3C_6    format=F00234_. label="Assistance: auxiliary nurse(6)";
  attrib M3D_1    format=F00235_. label="NA - Assistance: CS health professional(1)";
  attrib M3D_2    format=F00235_. label="NA - Assistance: CS health professional(2)";
  attrib M3D_3    format=F00235_. label="NA - Assistance: CS health professional(3)";
  attrib M3D_4    format=F00235_. label="NA - Assistance: CS health professional(4)";
  attrib M3D_5    format=F00235_. label="NA - Assistance: CS health professional(5)";
  attrib M3D_6    format=F00235_. label="NA - Assistance: CS health professional(6)";
  attrib M3E_1    format=F00236_. label="NA - Assistance: CS health professional(1)";
  attrib M3E_2    format=F00236_. label="NA - Assistance: CS health professional(2)";
  attrib M3E_3    format=F00236_. label="NA - Assistance: CS health professional(3)";
  attrib M3E_4    format=F00236_. label="NA - Assistance: CS health professional(4)";
  attrib M3E_5    format=F00236_. label="NA - Assistance: CS health professional(5)";
  attrib M3E_6    format=F00236_. label="NA - Assistance: CS health professional(6)";
  attrib M3F_1    format=F00237_. label="NA - Assistance: CS health professional(1)";
  attrib M3F_2    format=F00237_. label="NA - Assistance: CS health professional(2)";
  attrib M3F_3    format=F00237_. label="NA - Assistance: CS health professional(3)";
  attrib M3F_4    format=F00237_. label="NA - Assistance: CS health professional(4)";
  attrib M3F_5    format=F00237_. label="NA - Assistance: CS health professional(5)";
  attrib M3F_6    format=F00237_. label="NA - Assistance: CS health professional(6)";
  attrib M3G_1    format=F00238_. label="Assistance: traditional birth attendant(1)";
  attrib M3G_2    format=F00238_. label="Assistance: traditional birth attendant(2)";
  attrib M3G_3    format=F00238_. label="Assistance: traditional birth attendant(3)";
  attrib M3G_4    format=F00238_. label="Assistance: traditional birth attendant(4)";
  attrib M3G_5    format=F00238_. label="Assistance: traditional birth attendant(5)";
  attrib M3G_6    format=F00238_. label="Assistance: traditional birth attendant(6)";
  attrib M3H_1    format=F00239_. label="Assistance: relative/friend(1)";
  attrib M3H_2    format=F00239_. label="Assistance: relative/friend(2)";
  attrib M3H_3    format=F00239_. label="Assistance: relative/friend(3)";
  attrib M3H_4    format=F00239_. label="Assistance: relative/friend(4)";
  attrib M3H_5    format=F00239_. label="Assistance: relative/friend(5)";
  attrib M3H_6    format=F00239_. label="Assistance: relative/friend(6)";
  attrib M3I_1    format=F00240_. label="NA - Assistance: CS other person(1)";
  attrib M3I_2    format=F00240_. label="NA - Assistance: CS other person(2)";
  attrib M3I_3    format=F00240_. label="NA - Assistance: CS other person(3)";
  attrib M3I_4    format=F00240_. label="NA - Assistance: CS other person(4)";
  attrib M3I_5    format=F00240_. label="NA - Assistance: CS other person(5)";
  attrib M3I_6    format=F00240_. label="NA - Assistance: CS other person(6)";
  attrib M3J_1    format=F00241_. label="NA - Assistance: CS other person(1)";
  attrib M3J_2    format=F00241_. label="NA - Assistance: CS other person(2)";
  attrib M3J_3    format=F00241_. label="NA - Assistance: CS other person(3)";
  attrib M3J_4    format=F00241_. label="NA - Assistance: CS other person(4)";
  attrib M3J_5    format=F00241_. label="NA - Assistance: CS other person(5)";
  attrib M3J_6    format=F00241_. label="NA - Assistance: CS other person(6)";
  attrib M3K_1    format=F00242_. label="Assistance: other(1)";
  attrib M3K_2    format=F00242_. label="Assistance: other(2)";
  attrib M3K_3    format=F00242_. label="Assistance: other(3)";
  attrib M3K_4    format=F00242_. label="Assistance: other(4)";
  attrib M3K_5    format=F00242_. label="Assistance: other(5)";
  attrib M3K_6    format=F00242_. label="Assistance: other(6)";
  attrib M3L_1    format=F00243_. label="NA - Assistance: CS other(1)";
  attrib M3L_2    format=F00243_. label="NA - Assistance: CS other(2)";
  attrib M3L_3    format=F00243_. label="NA - Assistance: CS other(3)";
  attrib M3L_4    format=F00243_. label="NA - Assistance: CS other(4)";
  attrib M3L_5    format=F00243_. label="NA - Assistance: CS other(5)";
  attrib M3L_6    format=F00243_. label="NA - Assistance: CS other(6)";
  attrib M3M_1    format=F00244_. label="NA - Assistance: CS other(1)";
  attrib M3M_2    format=F00244_. label="NA - Assistance: CS other(2)";
  attrib M3M_3    format=F00244_. label="NA - Assistance: CS other(3)";
  attrib M3M_4    format=F00244_. label="NA - Assistance: CS other(4)";
  attrib M3M_5    format=F00244_. label="NA - Assistance: CS other(5)";
  attrib M3M_6    format=F00244_. label="NA - Assistance: CS other(6)";
  attrib M3N_1    format=F00245_. label="Assistance: no one(1)";
  attrib M3N_2    format=F00245_. label="Assistance: no one(2)";
  attrib M3N_3    format=F00245_. label="Assistance: no one(3)";
  attrib M3N_4    format=F00245_. label="Assistance: no one(4)";
  attrib M3N_5    format=F00245_. label="Assistance: no one(5)";
  attrib M3N_6    format=F00245_. label="Assistance: no one(6)";
  attrib M4_1     format=F00246_. label="NA - Duration of breastfeeding(1)";
  attrib M4_2     format=F00246_. label="NA - Duration of breastfeeding(2)";
  attrib M4_3     format=F00246_. label="NA - Duration of breastfeeding(3)";
  attrib M4_4     format=F00246_. label="NA - Duration of breastfeeding(4)";
  attrib M4_5     format=F00246_. label="NA - Duration of breastfeeding(5)";
  attrib M4_6     format=F00246_. label="NA - Duration of breastfeeding(6)";
  attrib M5_1     format=F00247_. label="NA - Months of breastfeeding(1)";
  attrib M5_2     format=F00247_. label="NA - Months of breastfeeding(2)";
  attrib M5_3     format=F00247_. label="NA - Months of breastfeeding(3)";
  attrib M5_4     format=F00247_. label="NA - Months of breastfeeding(4)";
  attrib M5_5     format=F00247_. label="NA - Months of breastfeeding(5)";
  attrib M5_6     format=F00247_. label="NA - Months of breastfeeding(6)";
  attrib M6_1     format=F00248_. label="Duration of amenorrhea(1)";
  attrib M6_2     format=F00248_. label="Duration of amenorrhea(2)";
  attrib M6_3     format=F00248_. label="Duration of amenorrhea(3)";
  attrib M6_4     format=F00248_. label="Duration of amenorrhea(4)";
  attrib M6_5     format=F00248_. label="Duration of amenorrhea(5)";
  attrib M6_6     format=F00248_. label="Duration of amenorrhea(6)";
  attrib M7_1     format=F00249_. label="Months of amenorrhea(1)";
  attrib M7_2     format=F00249_. label="Months of amenorrhea(2)";
  attrib M7_3     format=F00249_. label="Months of amenorrhea(3)";
  attrib M7_4     format=F00249_. label="Months of amenorrhea(4)";
  attrib M7_5     format=F00249_. label="Months of amenorrhea(5)";
  attrib M7_6     format=F00249_. label="Months of amenorrhea(6)";
  attrib M8_1     format=F00250_. label="Duration of abstinence(1)";
  attrib M8_2     format=F00250_. label="Duration of abstinence(2)";
  attrib M8_3     format=F00250_. label="Duration of abstinence(3)";
  attrib M8_4     format=F00250_. label="Duration of abstinence(4)";
  attrib M8_5     format=F00250_. label="Duration of abstinence(5)";
  attrib M8_6     format=F00250_. label="Duration of abstinence(6)";
  attrib M9_1     format=F00251_. label="Months of abstinence(1)";
  attrib M9_2     format=F00251_. label="Months of abstinence(2)";
  attrib M9_3     format=F00251_. label="Months of abstinence(3)";
  attrib M9_4     format=F00251_. label="Months of abstinence(4)";
  attrib M9_5     format=F00251_. label="Months of abstinence(5)";
  attrib M9_6     format=F00251_. label="Months of abstinence(6)";
  attrib M10_1    format=F00252_. label="Wanted pregnancy when became pregnant(1)";
  attrib M10_2    format=F00252_. label="Wanted pregnancy when became pregnant(2)";
  attrib M10_3    format=F00252_. label="Wanted pregnancy when became pregnant(3)";
  attrib M10_4    format=F00252_. label="Wanted pregnancy when became pregnant(4)";
  attrib M10_5    format=F00252_. label="Wanted pregnancy when became pregnant(5)";
  attrib M10_6    format=F00252_. label="Wanted pregnancy when became pregnant(6)";
  attrib M11_1    format=F00253_. label="Desired time would have waited(1)";
  attrib M11_2    format=F00253_. label="Desired time would have waited(2)";
  attrib M11_3    format=F00253_. label="Desired time would have waited(3)";
  attrib M11_4    format=F00253_. label="Desired time would have waited(4)";
  attrib M11_5    format=F00253_. label="Desired time would have waited(5)";
  attrib M11_6    format=F00253_. label="Desired time would have waited(6)";
  attrib M13_1    format=F00254_. label="Timing of 1st antenatal check (months)(1)";
  attrib M13_2    format=F00254_. label="Timing of 1st antenatal check (months)(2)";
  attrib M13_3    format=F00254_. label="Timing of 1st antenatal check (months)(3)";
  attrib M13_4    format=F00254_. label="Timing of 1st antenatal check (months)(4)";
  attrib M13_5    format=F00254_. label="Timing of 1st antenatal check (months)(5)";
  attrib M13_6    format=F00254_. label="Timing of 1st antenatal check (months)(6)";
  attrib M14_1    format=F00255_. label="Number of antenatal visits during pregnancy(1)";
  attrib M14_2    format=F00255_. label="Number of antenatal visits during pregnancy(2)";
  attrib M14_3    format=F00255_. label="Number of antenatal visits during pregnancy(3)";
  attrib M14_4    format=F00255_. label="Number of antenatal visits during pregnancy(4)";
  attrib M14_5    format=F00255_. label="Number of antenatal visits during pregnancy(5)";
  attrib M14_6    format=F00255_. label="Number of antenatal visits during pregnancy(6)";
  attrib M15_1    format=F00256_. label="Place of delivery(1)";
  attrib M15_2    format=F00256_. label="Place of delivery(2)";
  attrib M15_3    format=F00256_. label="Place of delivery(3)";
  attrib M15_4    format=F00256_. label="Place of delivery(4)";
  attrib M15_5    format=F00256_. label="Place of delivery(5)";
  attrib M15_6    format=F00256_. label="Place of delivery(6)";
  attrib M17_1    format=F00257_. label="Delivery by caesarean section(1)";
  attrib M17_2    format=F00257_. label="Delivery by caesarean section(2)";
  attrib M17_3    format=F00257_. label="Delivery by caesarean section(3)";
  attrib M17_4    format=F00257_. label="Delivery by caesarean section(4)";
  attrib M17_5    format=F00257_. label="Delivery by caesarean section(5)";
  attrib M17_6    format=F00257_. label="Delivery by caesarean section(6)";
  attrib M17A_1   format=F00258_. label="Timing on decision for caesarean section(1)";
  attrib M17A_2   format=F00258_. label="Timing on decision for caesarean section(2)";
  attrib M17A_3   format=F00258_. label="Timing on decision for caesarean section(3)";
  attrib M17A_4   format=F00258_. label="Timing on decision for caesarean section(4)";
  attrib M17A_5   format=F00258_. label="Timing on decision for caesarean section(5)";
  attrib M17A_6   format=F00258_. label="Timing on decision for caesarean section(6)";
  attrib M18_1    format=F00259_. label="NA - Size of child at birth(1)";
  attrib M18_2    format=F00259_. label="NA - Size of child at birth(2)";
  attrib M18_3    format=F00259_. label="NA - Size of child at birth(3)";
  attrib M18_4    format=F00259_. label="NA - Size of child at birth(4)";
  attrib M18_5    format=F00259_. label="NA - Size of child at birth(5)";
  attrib M18_6    format=F00259_. label="NA - Size of child at birth(6)";
  attrib M19_1    format=F00260_. label="Birth weight in kilograms (3 decimals)(1)";
  attrib M19_2    format=F00260_. label="Birth weight in kilograms (3 decimals)(2)";
  attrib M19_3    format=F00260_. label="Birth weight in kilograms (3 decimals)(3)";
  attrib M19_4    format=F00260_. label="Birth weight in kilograms (3 decimals)(4)";
  attrib M19_5    format=F00260_. label="Birth weight in kilograms (3 decimals)(5)";
  attrib M19_6    format=F00260_. label="Birth weight in kilograms (3 decimals)(6)";
  attrib M19A_1   format=F00261_. label="Weight at birth/recall(1)";
  attrib M19A_2   format=F00261_. label="Weight at birth/recall(2)";
  attrib M19A_3   format=F00261_. label="Weight at birth/recall(3)";
  attrib M19A_4   format=F00261_. label="Weight at birth/recall(4)";
  attrib M19A_5   format=F00261_. label="Weight at birth/recall(5)";
  attrib M19A_6   format=F00261_. label="Weight at birth/recall(6)";
  attrib M27_1    format=F00262_. label="NA - Flag for breastfeeding(1)";
  attrib M27_2    format=F00262_. label="NA - Flag for breastfeeding(2)";
  attrib M27_3    format=F00262_. label="NA - Flag for breastfeeding(3)";
  attrib M27_4    format=F00262_. label="NA - Flag for breastfeeding(4)";
  attrib M27_5    format=F00262_. label="NA - Flag for breastfeeding(5)";
  attrib M27_6    format=F00262_. label="NA - Flag for breastfeeding(6)";
  attrib M28_1    format=F00263_. label="Flag for amenorrhea(1)";
  attrib M28_2    format=F00263_. label="Flag for amenorrhea(2)";
  attrib M28_3    format=F00263_. label="Flag for amenorrhea(3)";
  attrib M28_4    format=F00263_. label="Flag for amenorrhea(4)";
  attrib M28_5    format=F00263_. label="Flag for amenorrhea(5)";
  attrib M28_6    format=F00263_. label="Flag for amenorrhea(6)";
  attrib M29_1    format=F00264_. label="Flag for abstinence(1)";
  attrib M29_2    format=F00264_. label="Flag for abstinence(2)";
  attrib M29_3    format=F00264_. label="Flag for abstinence(3)";
  attrib M29_4    format=F00264_. label="Flag for abstinence(4)";
  attrib M29_5    format=F00264_. label="Flag for abstinence(5)";
  attrib M29_6    format=F00264_. label="Flag for abstinence(6)";
  attrib M34_1    format=F00265_. label="NA - When child put to breast(1)";
  attrib M34_2    format=F00265_. label="NA - When child put to breast(2)";
  attrib M34_3    format=F00265_. label="NA - When child put to breast(3)";
  attrib M34_4    format=F00265_. label="NA - When child put to breast(4)";
  attrib M34_5    format=F00265_. label="NA - When child put to breast(5)";
  attrib M34_6    format=F00265_. label="NA - When child put to breast(6)";
  attrib M35_1    format=F00266_. label="NA - Number of times breastfed during previous night(1)";
  attrib M35_2    format=F00266_. label="NA - Number of times breastfed during previous night(2)";
  attrib M35_3    format=F00266_. label="NA - Number of times breastfed during previous night(3)";
  attrib M35_4    format=F00266_. label="NA - Number of times breastfed during previous night(4)";
  attrib M35_5    format=F00266_. label="NA - Number of times breastfed during previous night(5)";
  attrib M35_6    format=F00266_. label="NA - Number of times breastfed during previous night(6)";
  attrib M36_1    format=F00267_. label="NA - Number of times breastfed during previous day(1)";
  attrib M36_2    format=F00267_. label="NA - Number of times breastfed during previous day(2)";
  attrib M36_3    format=F00267_. label="NA - Number of times breastfed during previous day(3)";
  attrib M36_4    format=F00267_. label="NA - Number of times breastfed during previous day(4)";
  attrib M36_5    format=F00267_. label="NA - Number of times breastfed during previous day(5)";
  attrib M36_6    format=F00267_. label="NA - Number of times breastfed during previous day(6)";
  attrib M38_1    format=F00268_. label="NA - Drank from bottle with nipple yesterday/last night(1)";
  attrib M38_2    format=F00268_. label="NA - Drank from bottle with nipple yesterday/last night(2)";
  attrib M38_3    format=F00268_. label="NA - Drank from bottle with nipple yesterday/last night(3)";
  attrib M38_4    format=F00268_. label="NA - Drank from bottle with nipple yesterday/last night(4)";
  attrib M38_5    format=F00268_. label="NA - Drank from bottle with nipple yesterday/last night(5)";
  attrib M38_6    format=F00268_. label="NA - Drank from bottle with nipple yesterday/last night(6)";
  attrib M39A_1   format=F00269_. label="NA - Did eat any solid, semi-solid or soft foods yesterday(1)";
  attrib M39A_2   format=F00269_. label="NA - Did eat any solid, semi-solid or soft foods yesterday(2)";
  attrib M39A_3   format=F00269_. label="NA - Did eat any solid, semi-solid or soft foods yesterday(3)";
  attrib M39A_4   format=F00269_. label="NA - Did eat any solid, semi-solid or soft foods yesterday(4)";
  attrib M39A_5   format=F00269_. label="NA - Did eat any solid, semi-solid or soft foods yesterday(5)";
  attrib M39A_6   format=F00269_. label="NA - Did eat any solid, semi-solid or soft foods yesterday(6)";
  attrib M39_1    format=F00270_. label="NA - Number of times ate solid, semi-solid or soft food yesterday(1)";
  attrib M39_2    format=F00270_. label="NA - Number of times ate solid, semi-solid or soft food yesterday(2)";
  attrib M39_3    format=F00270_. label="NA - Number of times ate solid, semi-solid or soft food yesterday(3)";
  attrib M39_4    format=F00270_. label="NA - Number of times ate solid, semi-solid or soft food yesterday(4)";
  attrib M39_5    format=F00270_. label="NA - Number of times ate solid, semi-solid or soft food yesterday(5)";
  attrib M39_6    format=F00270_. label="NA - Number of times ate solid, semi-solid or soft food yesterday(6)";
  attrib M42A_1   format=F00271_. label="During pregnancy: weighed(1)";
  attrib M42A_2   format=F00271_. label="During pregnancy: weighed(2)";
  attrib M42A_3   format=F00271_. label="During pregnancy: weighed(3)";
  attrib M42A_4   format=F00271_. label="During pregnancy: weighed(4)";
  attrib M42A_5   format=F00271_. label="During pregnancy: weighed(5)";
  attrib M42A_6   format=F00271_. label="During pregnancy: weighed(6)";
  attrib M42B_1   format=F00272_. label="NA - During pregnancy: height measured(1)";
  attrib M42B_2   format=F00272_. label="NA - During pregnancy: height measured(2)";
  attrib M42B_3   format=F00272_. label="NA - During pregnancy: height measured(3)";
  attrib M42B_4   format=F00272_. label="NA - During pregnancy: height measured(4)";
  attrib M42B_5   format=F00272_. label="NA - During pregnancy: height measured(5)";
  attrib M42B_6   format=F00272_. label="NA - During pregnancy: height measured(6)";
  attrib M42C_1   format=F00273_. label="During pregnancy: blood pressure taken(1)";
  attrib M42C_2   format=F00273_. label="During pregnancy: blood pressure taken(2)";
  attrib M42C_3   format=F00273_. label="During pregnancy: blood pressure taken(3)";
  attrib M42C_4   format=F00273_. label="During pregnancy: blood pressure taken(4)";
  attrib M42C_5   format=F00273_. label="During pregnancy: blood pressure taken(5)";
  attrib M42C_6   format=F00273_. label="During pregnancy: blood pressure taken(6)";
  attrib M42D_1   format=F00274_. label="During pregnancy: urine sample taken(1)";
  attrib M42D_2   format=F00274_. label="During pregnancy: urine sample taken(2)";
  attrib M42D_3   format=F00274_. label="During pregnancy: urine sample taken(3)";
  attrib M42D_4   format=F00274_. label="During pregnancy: urine sample taken(4)";
  attrib M42D_5   format=F00274_. label="During pregnancy: urine sample taken(5)";
  attrib M42D_6   format=F00274_. label="During pregnancy: urine sample taken(6)";
  attrib M42E_1   format=F00275_. label="During pregnancy: blood sample taken(1)";
  attrib M42E_2   format=F00275_. label="During pregnancy: blood sample taken(2)";
  attrib M42E_3   format=F00275_. label="During pregnancy: blood sample taken(3)";
  attrib M42E_4   format=F00275_. label="During pregnancy: blood sample taken(4)";
  attrib M42E_5   format=F00275_. label="During pregnancy: blood sample taken(5)";
  attrib M42E_6   format=F00275_. label="During pregnancy: blood sample taken(6)";
  attrib M43_1    format=F00276_. label="Told about pregnancy complications(1)";
  attrib M43_2    format=F00276_. label="Told about pregnancy complications(2)";
  attrib M43_3    format=F00276_. label="Told about pregnancy complications(3)";
  attrib M43_4    format=F00276_. label="Told about pregnancy complications(4)";
  attrib M43_5    format=F00276_. label="Told about pregnancy complications(5)";
  attrib M43_6    format=F00276_. label="Told about pregnancy complications(6)";
  attrib M44_1    format=F00277_. label="Told where to go for pregnancy complications(1)";
  attrib M44_2    format=F00277_. label="Told where to go for pregnancy complications(2)";
  attrib M44_3    format=F00277_. label="Told where to go for pregnancy complications(3)";
  attrib M44_4    format=F00277_. label="Told where to go for pregnancy complications(4)";
  attrib M44_5    format=F00277_. label="Told where to go for pregnancy complications(5)";
  attrib M44_6    format=F00277_. label="Told where to go for pregnancy complications(6)";
  attrib M45_1    format=F00278_. label="During pregnancy, given or bought iron tablets/syrup(1)";
  attrib M45_2    format=F00278_. label="During pregnancy, given or bought iron tablets/syrup(2)";
  attrib M45_3    format=F00278_. label="During pregnancy, given or bought iron tablets/syrup(3)";
  attrib M45_4    format=F00278_. label="During pregnancy, given or bought iron tablets/syrup(4)";
  attrib M45_5    format=F00278_. label="During pregnancy, given or bought iron tablets/syrup(5)";
  attrib M45_6    format=F00278_. label="During pregnancy, given or bought iron tablets/syrup(6)";
  attrib M46_1    format=F00279_. label="NA - Days tablets or syrup taken(1)";
  attrib M46_2    format=F00279_. label="NA - Days tablets or syrup taken(2)";
  attrib M46_3    format=F00279_. label="NA - Days tablets or syrup taken(3)";
  attrib M46_4    format=F00279_. label="NA - Days tablets or syrup taken(4)";
  attrib M46_5    format=F00279_. label="NA - Days tablets or syrup taken(5)";
  attrib M46_6    format=F00279_. label="NA - Days tablets or syrup taken(6)";
  attrib M47_1    format=F00280_. label="NA - During pregnancy, had difficulty with daylight vision(1)";
  attrib M47_2    format=F00280_. label="NA - During pregnancy, had difficulty with daylight vision(2)";
  attrib M47_3    format=F00280_. label="NA - During pregnancy, had difficulty with daylight vision(3)";
  attrib M47_4    format=F00280_. label="NA - During pregnancy, had difficulty with daylight vision(4)";
  attrib M47_5    format=F00280_. label="NA - During pregnancy, had difficulty with daylight vision(5)";
  attrib M47_6    format=F00280_. label="NA - During pregnancy, had difficulty with daylight vision(6)";
  attrib M48_1    format=F00281_. label="NA - During pregnancy, had difficulty with night blindness(1)";
  attrib M48_2    format=F00281_. label="NA - During pregnancy, had difficulty with night blindness(2)";
  attrib M48_3    format=F00281_. label="NA - During pregnancy, had difficulty with night blindness(3)";
  attrib M48_4    format=F00281_. label="NA - During pregnancy, had difficulty with night blindness(4)";
  attrib M48_5    format=F00281_. label="NA - During pregnancy, had difficulty with night blindness(5)";
  attrib M48_6    format=F00281_. label="NA - During pregnancy, had difficulty with night blindness(6)";
  attrib M49A_1   format=F00282_. label="During pregnancy took: SP/fansidar for malaria(1)";
  attrib M49A_2   format=F00282_. label="During pregnancy took: SP/fansidar for malaria(2)";
  attrib M49A_3   format=F00282_. label="During pregnancy took: SP/fansidar for malaria(3)";
  attrib M49A_4   format=F00282_. label="During pregnancy took: SP/fansidar for malaria(4)";
  attrib M49A_5   format=F00282_. label="During pregnancy took: SP/fansidar for malaria(5)";
  attrib M49A_6   format=F00282_. label="During pregnancy took: SP/fansidar for malaria(6)";
  attrib M49B_1   format=F00283_. label="During pregnancy took: chloroquine for malaria(1)";
  attrib M49B_2   format=F00283_. label="During pregnancy took: chloroquine for malaria(2)";
  attrib M49B_3   format=F00283_. label="During pregnancy took: chloroquine for malaria(3)";
  attrib M49B_4   format=F00283_. label="During pregnancy took: chloroquine for malaria(4)";
  attrib M49B_5   format=F00283_. label="During pregnancy took: chloroquine for malaria(5)";
  attrib M49B_6   format=F00283_. label="During pregnancy took: chloroquine for malaria(6)";
  attrib M49C_1   format=F00284_. label="During pregnancy took: amodiaquine(1)";
  attrib M49C_2   format=F00284_. label="During pregnancy took: amodiaquine(2)";
  attrib M49C_3   format=F00284_. label="During pregnancy took: amodiaquine(3)";
  attrib M49C_4   format=F00284_. label="During pregnancy took: amodiaquine(4)";
  attrib M49C_5   format=F00284_. label="During pregnancy took: amodiaquine(5)";
  attrib M49C_6   format=F00284_. label="During pregnancy took: amodiaquine(6)";
  attrib M49D_1   format=F00285_. label="During pregnancy took: primaquine (neoquipenil)(1)";
  attrib M49D_2   format=F00285_. label="During pregnancy took: primaquine (neoquipenil)(2)";
  attrib M49D_3   format=F00285_. label="During pregnancy took: primaquine (neoquipenil)(3)";
  attrib M49D_4   format=F00285_. label="During pregnancy took: primaquine (neoquipenil)(4)";
  attrib M49D_5   format=F00285_. label="During pregnancy took: primaquine (neoquipenil)(5)";
  attrib M49D_6   format=F00285_. label="During pregnancy took: primaquine (neoquipenil)(6)";
  attrib M49E_1   format=F00286_. label="NA - During pregnancy took: CS drug for malaria(1)";
  attrib M49E_2   format=F00286_. label="NA - During pregnancy took: CS drug for malaria(2)";
  attrib M49E_3   format=F00286_. label="NA - During pregnancy took: CS drug for malaria(3)";
  attrib M49E_4   format=F00286_. label="NA - During pregnancy took: CS drug for malaria(4)";
  attrib M49E_5   format=F00286_. label="NA - During pregnancy took: CS drug for malaria(5)";
  attrib M49E_6   format=F00286_. label="NA - During pregnancy took: CS drug for malaria(6)";
  attrib M49F_1   format=F00287_. label="NA - During pregnancy took: CS drug for malaria(1)";
  attrib M49F_2   format=F00287_. label="NA - During pregnancy took: CS drug for malaria(2)";
  attrib M49F_3   format=F00287_. label="NA - During pregnancy took: CS drug for malaria(3)";
  attrib M49F_4   format=F00287_. label="NA - During pregnancy took: CS drug for malaria(4)";
  attrib M49F_5   format=F00287_. label="NA - During pregnancy took: CS drug for malaria(5)";
  attrib M49F_6   format=F00287_. label="NA - During pregnancy took: CS drug for malaria(6)";
  attrib M49G_1   format=F00288_. label="NA - During pregnancy took: CS drug for malaria(1)";
  attrib M49G_2   format=F00288_. label="NA - During pregnancy took: CS drug for malaria(2)";
  attrib M49G_3   format=F00288_. label="NA - During pregnancy took: CS drug for malaria(3)";
  attrib M49G_4   format=F00288_. label="NA - During pregnancy took: CS drug for malaria(4)";
  attrib M49G_5   format=F00288_. label="NA - During pregnancy took: CS drug for malaria(5)";
  attrib M49G_6   format=F00288_. label="NA - During pregnancy took: CS drug for malaria(6)";
  attrib M49X_1   format=F00289_. label="During pregnancy took: other drug for malaria(1)";
  attrib M49X_2   format=F00289_. label="During pregnancy took: other drug for malaria(2)";
  attrib M49X_3   format=F00289_. label="During pregnancy took: other drug for malaria(3)";
  attrib M49X_4   format=F00289_. label="During pregnancy took: other drug for malaria(4)";
  attrib M49X_5   format=F00289_. label="During pregnancy took: other drug for malaria(5)";
  attrib M49X_6   format=F00289_. label="During pregnancy took: other drug for malaria(6)";
  attrib M49Z_1   format=F00290_. label="During pregnancy took: don't know(1)";
  attrib M49Z_2   format=F00290_. label="During pregnancy took: don't know(2)";
  attrib M49Z_3   format=F00290_. label="During pregnancy took: don't know(3)";
  attrib M49Z_4   format=F00290_. label="During pregnancy took: don't know(4)";
  attrib M49Z_5   format=F00290_. label="During pregnancy took: don't know(5)";
  attrib M49Z_6   format=F00290_. label="During pregnancy took: don't know(6)";
  attrib M49Y_1   format=F00291_. label="During pregnancy took: no drug for malaria(1)";
  attrib M49Y_2   format=F00291_. label="During pregnancy took: no drug for malaria(2)";
  attrib M49Y_3   format=F00291_. label="During pregnancy took: no drug for malaria(3)";
  attrib M49Y_4   format=F00291_. label="During pregnancy took: no drug for malaria(4)";
  attrib M49Y_5   format=F00291_. label="During pregnancy took: no drug for malaria(5)";
  attrib M49Y_6   format=F00291_. label="During pregnancy took: no drug for malaria(6)";
  attrib M54_1    format=F00292_. label="NA - Received Vitamin A dose in first 2 months after delivery(1)";
  attrib M54_2    format=F00292_. label="NA - Received Vitamin A dose in first 2 months after delivery(2)";
  attrib M54_3    format=F00292_. label="NA - Received Vitamin A dose in first 2 months after delivery(3)";
  attrib M54_4    format=F00292_. label="NA - Received Vitamin A dose in first 2 months after delivery(4)";
  attrib M54_5    format=F00292_. label="NA - Received Vitamin A dose in first 2 months after delivery(5)";
  attrib M54_6    format=F00292_. label="NA - Received Vitamin A dose in first 2 months after delivery(6)";
  attrib M55_1    format=F00293_. label="NA - Given child anything other than breast milk(1)";
  attrib M55_2    format=F00293_. label="NA - Given child anything other than breast milk(2)";
  attrib M55_3    format=F00293_. label="NA - Given child anything other than breast milk(3)";
  attrib M55_4    format=F00293_. label="NA - Given child anything other than breast milk(4)";
  attrib M55_5    format=F00293_. label="NA - Given child anything other than breast milk(5)";
  attrib M55_6    format=F00293_. label="NA - Given child anything other than breast milk(6)";
  attrib M55A_1   format=F00294_. label="NA - First 3 days, given milk (other than breast milk)(1)";
  attrib M55A_2   format=F00294_. label="NA - First 3 days, given milk (other than breast milk)(2)";
  attrib M55A_3   format=F00294_. label="NA - First 3 days, given milk (other than breast milk)(3)";
  attrib M55A_4   format=F00294_. label="NA - First 3 days, given milk (other than breast milk)(4)";
  attrib M55A_5   format=F00294_. label="NA - First 3 days, given milk (other than breast milk)(5)";
  attrib M55A_6   format=F00294_. label="NA - First 3 days, given milk (other than breast milk)(6)";
  attrib M55B_1   format=F00295_. label="NA - First 3 days, given plain water(1)";
  attrib M55B_2   format=F00295_. label="NA - First 3 days, given plain water(2)";
  attrib M55B_3   format=F00295_. label="NA - First 3 days, given plain water(3)";
  attrib M55B_4   format=F00295_. label="NA - First 3 days, given plain water(4)";
  attrib M55B_5   format=F00295_. label="NA - First 3 days, given plain water(5)";
  attrib M55B_6   format=F00295_. label="NA - First 3 days, given plain water(6)";
  attrib M55C_1   format=F00296_. label="NA - First 3 days, given sugar/glucose water(1)";
  attrib M55C_2   format=F00296_. label="NA - First 3 days, given sugar/glucose water(2)";
  attrib M55C_3   format=F00296_. label="NA - First 3 days, given sugar/glucose water(3)";
  attrib M55C_4   format=F00296_. label="NA - First 3 days, given sugar/glucose water(4)";
  attrib M55C_5   format=F00296_. label="NA - First 3 days, given sugar/glucose water(5)";
  attrib M55C_6   format=F00296_. label="NA - First 3 days, given sugar/glucose water(6)";
  attrib M55D_1   format=F00297_. label="NA - First 3 days, given gripe water(1)";
  attrib M55D_2   format=F00297_. label="NA - First 3 days, given gripe water(2)";
  attrib M55D_3   format=F00297_. label="NA - First 3 days, given gripe water(3)";
  attrib M55D_4   format=F00297_. label="NA - First 3 days, given gripe water(4)";
  attrib M55D_5   format=F00297_. label="NA - First 3 days, given gripe water(5)";
  attrib M55D_6   format=F00297_. label="NA - First 3 days, given gripe water(6)";
  attrib M55E_1   format=F00298_. label="NA - First 3 days, given sugar/salt solution(1)";
  attrib M55E_2   format=F00298_. label="NA - First 3 days, given sugar/salt solution(2)";
  attrib M55E_3   format=F00298_. label="NA - First 3 days, given sugar/salt solution(3)";
  attrib M55E_4   format=F00298_. label="NA - First 3 days, given sugar/salt solution(4)";
  attrib M55E_5   format=F00298_. label="NA - First 3 days, given sugar/salt solution(5)";
  attrib M55E_6   format=F00298_. label="NA - First 3 days, given sugar/salt solution(6)";
  attrib M55F_1   format=F00299_. label="NA - First 3 days, given fruit juice(1)";
  attrib M55F_2   format=F00299_. label="NA - First 3 days, given fruit juice(2)";
  attrib M55F_3   format=F00299_. label="NA - First 3 days, given fruit juice(3)";
  attrib M55F_4   format=F00299_. label="NA - First 3 days, given fruit juice(4)";
  attrib M55F_5   format=F00299_. label="NA - First 3 days, given fruit juice(5)";
  attrib M55F_6   format=F00299_. label="NA - First 3 days, given fruit juice(6)";
  attrib M55G_1   format=F00300_. label="NA - First 3 days, given infant formula(1)";
  attrib M55G_2   format=F00300_. label="NA - First 3 days, given infant formula(2)";
  attrib M55G_3   format=F00300_. label="NA - First 3 days, given infant formula(3)";
  attrib M55G_4   format=F00300_. label="NA - First 3 days, given infant formula(4)";
  attrib M55G_5   format=F00300_. label="NA - First 3 days, given infant formula(5)";
  attrib M55G_6   format=F00300_. label="NA - First 3 days, given infant formula(6)";
  attrib M55H_1   format=F00301_. label="NA - First 3 days, given tea/infusions(1)";
  attrib M55H_2   format=F00301_. label="NA - First 3 days, given tea/infusions(2)";
  attrib M55H_3   format=F00301_. label="NA - First 3 days, given tea/infusions(3)";
  attrib M55H_4   format=F00301_. label="NA - First 3 days, given tea/infusions(4)";
  attrib M55H_5   format=F00301_. label="NA - First 3 days, given tea/infusions(5)";
  attrib M55H_6   format=F00301_. label="NA - First 3 days, given tea/infusions(6)";
  attrib M55I_1   format=F00302_. label="NA - First 3 days, given honey(1)";
  attrib M55I_2   format=F00302_. label="NA - First 3 days, given honey(2)";
  attrib M55I_3   format=F00302_. label="NA - First 3 days, given honey(3)";
  attrib M55I_4   format=F00302_. label="NA - First 3 days, given honey(4)";
  attrib M55I_5   format=F00302_. label="NA - First 3 days, given honey(5)";
  attrib M55I_6   format=F00302_. label="NA - First 3 days, given honey(6)";
  attrib M55J_1   format=F00303_. label="NA - First 3 days, given coffee(1)";
  attrib M55J_2   format=F00303_. label="NA - First 3 days, given coffee(2)";
  attrib M55J_3   format=F00303_. label="NA - First 3 days, given coffee(3)";
  attrib M55J_4   format=F00303_. label="NA - First 3 days, given coffee(4)";
  attrib M55J_5   format=F00303_. label="NA - First 3 days, given coffee(5)";
  attrib M55J_6   format=F00303_. label="NA - First 3 days, given coffee(6)";
  attrib M55K_1   format=F00304_. label="NA - First 3 days, given country specific(1)";
  attrib M55K_2   format=F00304_. label="NA - First 3 days, given country specific(2)";
  attrib M55K_3   format=F00304_. label="NA - First 3 days, given country specific(3)";
  attrib M55K_4   format=F00304_. label="NA - First 3 days, given country specific(4)";
  attrib M55K_5   format=F00304_. label="NA - First 3 days, given country specific(5)";
  attrib M55K_6   format=F00304_. label="NA - First 3 days, given country specific(6)";
  attrib M55L_1   format=F00305_. label="NA - First 3 days, given country specific(1)";
  attrib M55L_2   format=F00305_. label="NA - First 3 days, given country specific(2)";
  attrib M55L_3   format=F00305_. label="NA - First 3 days, given country specific(3)";
  attrib M55L_4   format=F00305_. label="NA - First 3 days, given country specific(4)";
  attrib M55L_5   format=F00305_. label="NA - First 3 days, given country specific(5)";
  attrib M55L_6   format=F00305_. label="NA - First 3 days, given country specific(6)";
  attrib M55M_1   format=F00306_. label="NA - First 3 days, given country specific(1)";
  attrib M55M_2   format=F00306_. label="NA - First 3 days, given country specific(2)";
  attrib M55M_3   format=F00306_. label="NA - First 3 days, given country specific(3)";
  attrib M55M_4   format=F00306_. label="NA - First 3 days, given country specific(4)";
  attrib M55M_5   format=F00306_. label="NA - First 3 days, given country specific(5)";
  attrib M55M_6   format=F00306_. label="NA - First 3 days, given country specific(6)";
  attrib M55N_1   format=F00307_. label="NA - First 3 days, given country specific(1)";
  attrib M55N_2   format=F00307_. label="NA - First 3 days, given country specific(2)";
  attrib M55N_3   format=F00307_. label="NA - First 3 days, given country specific(3)";
  attrib M55N_4   format=F00307_. label="NA - First 3 days, given country specific(4)";
  attrib M55N_5   format=F00307_. label="NA - First 3 days, given country specific(5)";
  attrib M55N_6   format=F00307_. label="NA - First 3 days, given country specific(6)";
  attrib M55O_1   format=F00308_. label="NA - First 3 days, given country specific(1)";
  attrib M55O_2   format=F00308_. label="NA - First 3 days, given country specific(2)";
  attrib M55O_3   format=F00308_. label="NA - First 3 days, given country specific(3)";
  attrib M55O_4   format=F00308_. label="NA - First 3 days, given country specific(4)";
  attrib M55O_5   format=F00308_. label="NA - First 3 days, given country specific(5)";
  attrib M55O_6   format=F00308_. label="NA - First 3 days, given country specific(6)";
  attrib M55X_1   format=F00309_. label="NA - First 3 days, given other(1)";
  attrib M55X_2   format=F00309_. label="NA - First 3 days, given other(2)";
  attrib M55X_3   format=F00309_. label="NA - First 3 days, given other(3)";
  attrib M55X_4   format=F00309_. label="NA - First 3 days, given other(4)";
  attrib M55X_5   format=F00309_. label="NA - First 3 days, given other(5)";
  attrib M55X_6   format=F00309_. label="NA - First 3 days, given other(6)";
  attrib M55Z_1   format=F00310_. label="NA - First 3 days, given nothing(1)";
  attrib M55Z_2   format=F00310_. label="NA - First 3 days, given nothing(2)";
  attrib M55Z_3   format=F00310_. label="NA - First 3 days, given nothing(3)";
  attrib M55Z_4   format=F00310_. label="NA - First 3 days, given nothing(4)";
  attrib M55Z_5   format=F00310_. label="NA - First 3 days, given nothing(5)";
  attrib M55Z_6   format=F00310_. label="NA - First 3 days, given nothing(6)";
  attrib M57A_1   format=F00311_. label="NA - Antenatal care: respondent's home(1)";
  attrib M57A_2   format=F00311_. label="NA - Antenatal care: respondent's home(2)";
  attrib M57A_3   format=F00311_. label="NA - Antenatal care: respondent's home(3)";
  attrib M57A_4   format=F00311_. label="NA - Antenatal care: respondent's home(4)";
  attrib M57A_5   format=F00311_. label="NA - Antenatal care: respondent's home(5)";
  attrib M57A_6   format=F00311_. label="NA - Antenatal care: respondent's home(6)";
  attrib M57B_1   format=F00312_. label="NA - Antenatal care: other home(1)";
  attrib M57B_2   format=F00312_. label="NA - Antenatal care: other home(2)";
  attrib M57B_3   format=F00312_. label="NA - Antenatal care: other home(3)";
  attrib M57B_4   format=F00312_. label="NA - Antenatal care: other home(4)";
  attrib M57B_5   format=F00312_. label="NA - Antenatal care: other home(5)";
  attrib M57B_6   format=F00312_. label="NA - Antenatal care: other home(6)";
  attrib M57C_1   format=F00313_. label="NA - Antenatal care: CS home(1)";
  attrib M57C_2   format=F00313_. label="NA - Antenatal care: CS home(2)";
  attrib M57C_3   format=F00313_. label="NA - Antenatal care: CS home(3)";
  attrib M57C_4   format=F00313_. label="NA - Antenatal care: CS home(4)";
  attrib M57C_5   format=F00313_. label="NA - Antenatal care: CS home(5)";
  attrib M57C_6   format=F00313_. label="NA - Antenatal care: CS home(6)";
  attrib M57D_1   format=F00314_. label="NA - Antenatal care: CS home(1)";
  attrib M57D_2   format=F00314_. label="NA - Antenatal care: CS home(2)";
  attrib M57D_3   format=F00314_. label="NA - Antenatal care: CS home(3)";
  attrib M57D_4   format=F00314_. label="NA - Antenatal care: CS home(4)";
  attrib M57D_5   format=F00314_. label="NA - Antenatal care: CS home(5)";
  attrib M57D_6   format=F00314_. label="NA - Antenatal care: CS home(6)";
  attrib M57E_1   format=F00315_. label="Antenatal care: government hospital(1)";
  attrib M57E_2   format=F00315_. label="Antenatal care: government hospital(2)";
  attrib M57E_3   format=F00315_. label="Antenatal care: government hospital(3)";
  attrib M57E_4   format=F00315_. label="Antenatal care: government hospital(4)";
  attrib M57E_5   format=F00315_. label="Antenatal care: government hospital(5)";
  attrib M57E_6   format=F00315_. label="Antenatal care: government hospital(6)";
  attrib M57F_1   format=F00316_. label="NA - Antenatal care: CS public health(1)";
  attrib M57F_2   format=F00316_. label="NA - Antenatal care: CS public health(2)";
  attrib M57F_3   format=F00316_. label="NA - Antenatal care: CS public health(3)";
  attrib M57F_4   format=F00316_. label="NA - Antenatal care: CS public health(4)";
  attrib M57F_5   format=F00316_. label="NA - Antenatal care: CS public health(5)";
  attrib M57F_6   format=F00316_. label="NA - Antenatal care: CS public health(6)";
  attrib M57G_1   format=F00317_. label="NA - Antenatal care: CS public health(1)";
  attrib M57G_2   format=F00317_. label="NA - Antenatal care: CS public health(2)";
  attrib M57G_3   format=F00317_. label="NA - Antenatal care: CS public health(3)";
  attrib M57G_4   format=F00317_. label="NA - Antenatal care: CS public health(4)";
  attrib M57G_5   format=F00317_. label="NA - Antenatal care: CS public health(5)";
  attrib M57G_6   format=F00317_. label="NA - Antenatal care: CS public health(6)";
  attrib M57H_1   format=F00318_. label="NA - Antenatal care: CS public health(1)";
  attrib M57H_2   format=F00318_. label="NA - Antenatal care: CS public health(2)";
  attrib M57H_3   format=F00318_. label="NA - Antenatal care: CS public health(3)";
  attrib M57H_4   format=F00318_. label="NA - Antenatal care: CS public health(4)";
  attrib M57H_5   format=F00318_. label="NA - Antenatal care: CS public health(5)";
  attrib M57H_6   format=F00318_. label="NA - Antenatal care: CS public health(6)";
  attrib M57I_1   format=F00319_. label="NA - Antenatal care: CS public health(1)";
  attrib M57I_2   format=F00319_. label="NA - Antenatal care: CS public health(2)";
  attrib M57I_3   format=F00319_. label="NA - Antenatal care: CS public health(3)";
  attrib M57I_4   format=F00319_. label="NA - Antenatal care: CS public health(4)";
  attrib M57I_5   format=F00319_. label="NA - Antenatal care: CS public health(5)";
  attrib M57I_6   format=F00319_. label="NA - Antenatal care: CS public health(6)";
  attrib M57J_1   format=F00320_. label="NA - Antenatal care: CS public health(1)";
  attrib M57J_2   format=F00320_. label="NA - Antenatal care: CS public health(2)";
  attrib M57J_3   format=F00320_. label="NA - Antenatal care: CS public health(3)";
  attrib M57J_4   format=F00320_. label="NA - Antenatal care: CS public health(4)";
  attrib M57J_5   format=F00320_. label="NA - Antenatal care: CS public health(5)";
  attrib M57J_6   format=F00320_. label="NA - Antenatal care: CS public health(6)";
  attrib M57K_1   format=F00321_. label="NA - Antenatal care: CS public health(1)";
  attrib M57K_2   format=F00321_. label="NA - Antenatal care: CS public health(2)";
  attrib M57K_3   format=F00321_. label="NA - Antenatal care: CS public health(3)";
  attrib M57K_4   format=F00321_. label="NA - Antenatal care: CS public health(4)";
  attrib M57K_5   format=F00321_. label="NA - Antenatal care: CS public health(5)";
  attrib M57K_6   format=F00321_. label="NA - Antenatal care: CS public health(6)";
  attrib M57L_1   format=F00322_. label="NA - Antenatal care: CS public health(1)";
  attrib M57L_2   format=F00322_. label="NA - Antenatal care: CS public health(2)";
  attrib M57L_3   format=F00322_. label="NA - Antenatal care: CS public health(3)";
  attrib M57L_4   format=F00322_. label="NA - Antenatal care: CS public health(4)";
  attrib M57L_5   format=F00322_. label="NA - Antenatal care: CS public health(5)";
  attrib M57L_6   format=F00322_. label="NA - Antenatal care: CS public health(6)";
  attrib M57M_1   format=F00323_. label="Antenatal care: private hospital/clinic(1)";
  attrib M57M_2   format=F00323_. label="Antenatal care: private hospital/clinic(2)";
  attrib M57M_3   format=F00323_. label="Antenatal care: private hospital/clinic(3)";
  attrib M57M_4   format=F00323_. label="Antenatal care: private hospital/clinic(4)";
  attrib M57M_5   format=F00323_. label="Antenatal care: private hospital/clinic(5)";
  attrib M57M_6   format=F00323_. label="Antenatal care: private hospital/clinic(6)";
  attrib M57N_1   format=F00324_. label="Antenatal care: EPS health center(1)";
  attrib M57N_2   format=F00324_. label="Antenatal care: EPS health center(2)";
  attrib M57N_3   format=F00324_. label="Antenatal care: EPS health center(3)";
  attrib M57N_4   format=F00324_. label="Antenatal care: EPS health center(4)";
  attrib M57N_5   format=F00324_. label="Antenatal care: EPS health center(5)";
  attrib M57N_6   format=F00324_. label="Antenatal care: EPS health center(6)";
  attrib M57O_1   format=F00325_. label="Antenatal care: private doctor(1)";
  attrib M57O_2   format=F00325_. label="Antenatal care: private doctor(2)";
  attrib M57O_3   format=F00325_. label="Antenatal care: private doctor(3)";
  attrib M57O_4   format=F00325_. label="Antenatal care: private doctor(4)";
  attrib M57O_5   format=F00325_. label="Antenatal care: private doctor(5)";
  attrib M57O_6   format=F00325_. label="Antenatal care: private doctor(6)";
  attrib M57P_1   format=F00326_. label="Antenatal care: PROFAMILIA(1)";
  attrib M57P_2   format=F00326_. label="Antenatal care: PROFAMILIA(2)";
  attrib M57P_3   format=F00326_. label="Antenatal care: PROFAMILIA(3)";
  attrib M57P_4   format=F00326_. label="Antenatal care: PROFAMILIA(4)";
  attrib M57P_5   format=F00326_. label="Antenatal care: PROFAMILIA(5)";
  attrib M57P_6   format=F00326_. label="Antenatal care: PROFAMILIA(6)";
  attrib M57Q_1   format=F00327_. label="NA - Antenatal care: CS private medical(1)";
  attrib M57Q_2   format=F00327_. label="NA - Antenatal care: CS private medical(2)";
  attrib M57Q_3   format=F00327_. label="NA - Antenatal care: CS private medical(3)";
  attrib M57Q_4   format=F00327_. label="NA - Antenatal care: CS private medical(4)";
  attrib M57Q_5   format=F00327_. label="NA - Antenatal care: CS private medical(5)";
  attrib M57Q_6   format=F00327_. label="NA - Antenatal care: CS private medical(6)";
  attrib M57R_1   format=F00328_. label="NA - Antenatal care: CS private medical(1)";
  attrib M57R_2   format=F00328_. label="NA - Antenatal care: CS private medical(2)";
  attrib M57R_3   format=F00328_. label="NA - Antenatal care: CS private medical(3)";
  attrib M57R_4   format=F00328_. label="NA - Antenatal care: CS private medical(4)";
  attrib M57R_5   format=F00328_. label="NA - Antenatal care: CS private medical(5)";
  attrib M57R_6   format=F00328_. label="NA - Antenatal care: CS private medical(6)";
  attrib M57S_1   format=F00329_. label="NA - Antenatal care: CS other(1)";
  attrib M57S_2   format=F00329_. label="NA - Antenatal care: CS other(2)";
  attrib M57S_3   format=F00329_. label="NA - Antenatal care: CS other(3)";
  attrib M57S_4   format=F00329_. label="NA - Antenatal care: CS other(4)";
  attrib M57S_5   format=F00329_. label="NA - Antenatal care: CS other(5)";
  attrib M57S_6   format=F00329_. label="NA - Antenatal care: CS other(6)";
  attrib M57T_1   format=F00330_. label="NA - Antenatal care: CS other(1)";
  attrib M57T_2   format=F00330_. label="NA - Antenatal care: CS other(2)";
  attrib M57T_3   format=F00330_. label="NA - Antenatal care: CS other(3)";
  attrib M57T_4   format=F00330_. label="NA - Antenatal care: CS other(4)";
  attrib M57T_5   format=F00330_. label="NA - Antenatal care: CS other(5)";
  attrib M57T_6   format=F00330_. label="NA - Antenatal care: CS other(6)";
  attrib M57U_1   format=F00331_. label="NA - Antenatal care: CS other(1)";
  attrib M57U_2   format=F00331_. label="NA - Antenatal care: CS other(2)";
  attrib M57U_3   format=F00331_. label="NA - Antenatal care: CS other(3)";
  attrib M57U_4   format=F00331_. label="NA - Antenatal care: CS other(4)";
  attrib M57U_5   format=F00331_. label="NA - Antenatal care: CS other(5)";
  attrib M57U_6   format=F00331_. label="NA - Antenatal care: CS other(6)";
  attrib M57V_1   format=F00332_. label="NA - Antenatal care: CS other(1)";
  attrib M57V_2   format=F00332_. label="NA - Antenatal care: CS other(2)";
  attrib M57V_3   format=F00332_. label="NA - Antenatal care: CS other(3)";
  attrib M57V_4   format=F00332_. label="NA - Antenatal care: CS other(4)";
  attrib M57V_5   format=F00332_. label="NA - Antenatal care: CS other(5)";
  attrib M57V_6   format=F00332_. label="NA - Antenatal care: CS other(6)";
  attrib M57X_1   format=F00333_. label="Antenatal care: other(1)";
  attrib M57X_2   format=F00333_. label="Antenatal care: other(2)";
  attrib M57X_3   format=F00333_. label="Antenatal care: other(3)";
  attrib M57X_4   format=F00333_. label="Antenatal care: other(4)";
  attrib M57X_5   format=F00333_. label="Antenatal care: other(5)";
  attrib M57X_6   format=F00333_. label="Antenatal care: other(6)";
  attrib M60_1    format=F00334_. label="NA - Drugs for intestinal parasites during pregnancy(1)";
  attrib M60_2    format=F00334_. label="NA - Drugs for intestinal parasites during pregnancy(2)";
  attrib M60_3    format=F00334_. label="NA - Drugs for intestinal parasites during pregnancy(3)";
  attrib M60_4    format=F00334_. label="NA - Drugs for intestinal parasites during pregnancy(4)";
  attrib M60_5    format=F00334_. label="NA - Drugs for intestinal parasites during pregnancy(5)";
  attrib M60_6    format=F00334_. label="NA - Drugs for intestinal parasites during pregnancy(6)";
  attrib M61_1    format=F00335_. label="NA - Time spent at place of delivery(1)";
  attrib M61_2    format=F00335_. label="NA - Time spent at place of delivery(2)";
  attrib M61_3    format=F00335_. label="NA - Time spent at place of delivery(3)";
  attrib M61_4    format=F00335_. label="NA - Time spent at place of delivery(4)";
  attrib M61_5    format=F00335_. label="NA - Time spent at place of delivery(5)";
  attrib M61_6    format=F00335_. label="NA - Time spent at place of delivery(6)";
  attrib M62_1    format=F00336_. label="NA - Respondent's health checked before discharge(1)";
  attrib M62_2    format=F00336_. label="NA - Respondent's health checked before discharge(2)";
  attrib M62_3    format=F00336_. label="NA - Respondent's health checked before discharge(3)";
  attrib M62_4    format=F00336_. label="NA - Respondent's health checked before discharge(4)";
  attrib M62_5    format=F00336_. label="NA - Respondent's health checked before discharge(5)";
  attrib M62_6    format=F00336_. label="NA - Respondent's health checked before discharge(6)";
  attrib M63_1    format=F00337_. label="NA - How long before discharging respondent health check took place(1)";
  attrib M63_2    format=F00337_. label="NA - How long before discharging respondent health check took place(2)";
  attrib M63_3    format=F00337_. label="NA - How long before discharging respondent health check took place(3)";
  attrib M63_4    format=F00337_. label="NA - How long before discharging respondent health check took place(4)";
  attrib M63_5    format=F00337_. label="NA - How long before discharging respondent health check took place(5)";
  attrib M63_6    format=F00337_. label="NA - How long before discharging respondent health check took place(6)";
  attrib M64_1    format=F00338_. label="NA - Who checked respondent health before discharge(1)";
  attrib M64_2    format=F00338_. label="NA - Who checked respondent health before discharge(2)";
  attrib M64_3    format=F00338_. label="NA - Who checked respondent health before discharge(3)";
  attrib M64_4    format=F00338_. label="NA - Who checked respondent health before discharge(4)";
  attrib M64_5    format=F00338_. label="NA - Who checked respondent health before discharge(5)";
  attrib M64_6    format=F00338_. label="NA - Who checked respondent health before discharge(6)";
  attrib M65A_1   format=F00339_. label="Reason didn't deliver at health facility: economics(1)";
  attrib M65A_2   format=F00339_. label="Reason didn't deliver at health facility: economics(2)";
  attrib M65A_3   format=F00339_. label="Reason didn't deliver at health facility: economics(3)";
  attrib M65A_4   format=F00339_. label="Reason didn't deliver at health facility: economics(4)";
  attrib M65A_5   format=F00339_. label="Reason didn't deliver at health facility: economics(5)";
  attrib M65A_6   format=F00339_. label="Reason didn't deliver at health facility: economics(6)";
  attrib M65B_1   format=F00340_. label="NA - Reason didn't deliver at health facility: facility not open(1)";
  attrib M65B_2   format=F00340_. label="NA - Reason didn't deliver at health facility: facility not open(2)";
  attrib M65B_3   format=F00340_. label="NA - Reason didn't deliver at health facility: facility not open(3)";
  attrib M65B_4   format=F00340_. label="NA - Reason didn't deliver at health facility: facility not open(4)";
  attrib M65B_5   format=F00340_. label="NA - Reason didn't deliver at health facility: facility not open(5)";
  attrib M65B_6   format=F00340_. label="NA - Reason didn't deliver at health facility: facility not open(6)";
  attrib M65C_1   format=F00341_. label="Reason didn't deliver at health facility: health services(1)";
  attrib M65C_2   format=F00341_. label="Reason didn't deliver at health facility: health services(2)";
  attrib M65C_3   format=F00341_. label="Reason didn't deliver at health facility: health services(3)";
  attrib M65C_4   format=F00341_. label="Reason didn't deliver at health facility: health services(4)";
  attrib M65C_5   format=F00341_. label="Reason didn't deliver at health facility: health services(5)";
  attrib M65C_6   format=F00341_. label="Reason didn't deliver at health facility: health services(6)";
  attrib M65D_1   format=F00342_. label="Reason didn't deliver at health facility: personal(1)";
  attrib M65D_2   format=F00342_. label="Reason didn't deliver at health facility: personal(2)";
  attrib M65D_3   format=F00342_. label="Reason didn't deliver at health facility: personal(3)";
  attrib M65D_4   format=F00342_. label="Reason didn't deliver at health facility: personal(4)";
  attrib M65D_5   format=F00342_. label="Reason didn't deliver at health facility: personal(5)";
  attrib M65D_6   format=F00342_. label="Reason didn't deliver at health facility: personal(6)";
  attrib M65E_1   format=F00343_. label="NA - Reason didn't deliver at health facility: no female provider(1)";
  attrib M65E_2   format=F00343_. label="NA - Reason didn't deliver at health facility: no female provider(2)";
  attrib M65E_3   format=F00343_. label="NA - Reason didn't deliver at health facility: no female provider(3)";
  attrib M65E_4   format=F00343_. label="NA - Reason didn't deliver at health facility: no female provider(4)";
  attrib M65E_5   format=F00343_. label="NA - Reason didn't deliver at health facility: no female provider(5)";
  attrib M65E_6   format=F00343_. label="NA - Reason didn't deliver at health facility: no female provider(6)";
  attrib M65F_1   format=F00344_. label="Reason didn't deliver at health facility: Opposition(1)";
  attrib M65F_2   format=F00344_. label="Reason didn't deliver at health facility: Opposition(2)";
  attrib M65F_3   format=F00344_. label="Reason didn't deliver at health facility: Opposition(3)";
  attrib M65F_4   format=F00344_. label="Reason didn't deliver at health facility: Opposition(4)";
  attrib M65F_5   format=F00344_. label="Reason didn't deliver at health facility: Opposition(5)";
  attrib M65F_6   format=F00344_. label="Reason didn't deliver at health facility: Opposition(6)";
  attrib M65G_1   format=F00345_. label="Reason didn't deliver at health facility: lack of knowledge(1)";
  attrib M65G_2   format=F00345_. label="Reason didn't deliver at health facility: lack of knowledge(2)";
  attrib M65G_3   format=F00345_. label="Reason didn't deliver at health facility: lack of knowledge(3)";
  attrib M65G_4   format=F00345_. label="Reason didn't deliver at health facility: lack of knowledge(4)";
  attrib M65G_5   format=F00345_. label="Reason didn't deliver at health facility: lack of knowledge(5)";
  attrib M65G_6   format=F00345_. label="Reason didn't deliver at health facility: lack of knowledge(6)";
  attrib M65H_1   format=F00346_. label="NA - Reason didn't deliver at health facility: not customary(1)";
  attrib M65H_2   format=F00346_. label="NA - Reason didn't deliver at health facility: not customary(2)";
  attrib M65H_3   format=F00346_. label="NA - Reason didn't deliver at health facility: not customary(3)";
  attrib M65H_4   format=F00346_. label="NA - Reason didn't deliver at health facility: not customary(4)";
  attrib M65H_5   format=F00346_. label="NA - Reason didn't deliver at health facility: not customary(5)";
  attrib M65H_6   format=F00346_. label="NA - Reason didn't deliver at health facility: not customary(6)";
  attrib M65I_1   format=F00347_. label="Reason didn't deliver at health facility: too much paper work(1)";
  attrib M65I_2   format=F00347_. label="Reason didn't deliver at health facility: too much paper work(2)";
  attrib M65I_3   format=F00347_. label="Reason didn't deliver at health facility: too much paper work(3)";
  attrib M65I_4   format=F00347_. label="Reason didn't deliver at health facility: too much paper work(4)";
  attrib M65I_5   format=F00347_. label="Reason didn't deliver at health facility: too much paper work(5)";
  attrib M65I_6   format=F00347_. label="Reason didn't deliver at health facility: too much paper work(6)";
  attrib M65J_1   format=F00348_. label="Reason didn't deliver at health facility: wasn't affiliated to health insurance(1)";
  attrib M65J_2   format=F00348_. label="Reason didn't deliver at health facility: wasn't affiliated to health insurance(2)";
  attrib M65J_3   format=F00348_. label="Reason didn't deliver at health facility: wasn't affiliated to health insurance(3)";
  attrib M65J_4   format=F00348_. label="Reason didn't deliver at health facility: wasn't affiliated to health insurance(4)";
  attrib M65J_5   format=F00348_. label="Reason didn't deliver at health facility: wasn't affiliated to health insurance(5)";
  attrib M65J_6   format=F00348_. label="Reason didn't deliver at health facility: wasn't affiliated to health insurance(6)";
  attrib M65K_1   format=F00349_. label="Reason didn't deliver at health facility: didn't know that could use the service(1)";
  attrib M65K_2   format=F00349_. label="Reason didn't deliver at health facility: didn't know that could use the service(2)";
  attrib M65K_3   format=F00349_. label="Reason didn't deliver at health facility: didn't know that could use the service(3)";
  attrib M65K_4   format=F00349_. label="Reason didn't deliver at health facility: didn't know that could use the service(4)";
  attrib M65K_5   format=F00349_. label="Reason didn't deliver at health facility: didn't know that could use the service(5)";
  attrib M65K_6   format=F00349_. label="Reason didn't deliver at health facility: didn't know that could use the service(6)";
  attrib M65L_1   format=F00350_. label="NA - Reason didn't deliver at health facility: CS(1)";
  attrib M65L_2   format=F00350_. label="NA - Reason didn't deliver at health facility: CS(2)";
  attrib M65L_3   format=F00350_. label="NA - Reason didn't deliver at health facility: CS(3)";
  attrib M65L_4   format=F00350_. label="NA - Reason didn't deliver at health facility: CS(4)";
  attrib M65L_5   format=F00350_. label="NA - Reason didn't deliver at health facility: CS(5)";
  attrib M65L_6   format=F00350_. label="NA - Reason didn't deliver at health facility: CS(6)";
  attrib M65X_1   format=F00351_. label="Reason didn't deliver at health facility: other(1)";
  attrib M65X_2   format=F00351_. label="Reason didn't deliver at health facility: other(2)";
  attrib M65X_3   format=F00351_. label="Reason didn't deliver at health facility: other(3)";
  attrib M65X_4   format=F00351_. label="Reason didn't deliver at health facility: other(4)";
  attrib M65X_5   format=F00351_. label="Reason didn't deliver at health facility: other(5)";
  attrib M65X_6   format=F00351_. label="Reason didn't deliver at health facility: other(6)";
  attrib M66_1    format=F00352_. label="Respondent's health checked after discharge/delivery at home(1)";
  attrib M66_2    format=F00352_. label="Respondent's health checked after discharge/delivery at home(2)";
  attrib M66_3    format=F00352_. label="Respondent's health checked after discharge/delivery at home(3)";
  attrib M66_4    format=F00352_. label="Respondent's health checked after discharge/delivery at home(4)";
  attrib M66_5    format=F00352_. label="Respondent's health checked after discharge/delivery at home(5)";
  attrib M66_6    format=F00352_. label="Respondent's health checked after discharge/delivery at home(6)";
  attrib M67_1    format=F00353_. label="How long after discharge/delivery at home respondent health check took place(1)";
  attrib M67_2    format=F00353_. label="How long after discharge/delivery at home respondent health check took place(2)";
  attrib M67_3    format=F00353_. label="How long after discharge/delivery at home respondent health check took place(3)";
  attrib M67_4    format=F00353_. label="How long after discharge/delivery at home respondent health check took place(4)";
  attrib M67_5    format=F00353_. label="How long after discharge/delivery at home respondent health check took place(5)";
  attrib M67_6    format=F00353_. label="How long after discharge/delivery at home respondent health check took place(6)";
  attrib M68_1    format=F00354_. label="Who checked respondent health after discharge/delivery at home(1)";
  attrib M68_2    format=F00354_. label="Who checked respondent health after discharge/delivery at home(2)";
  attrib M68_3    format=F00354_. label="Who checked respondent health after discharge/delivery at home(3)";
  attrib M68_4    format=F00354_. label="Who checked respondent health after discharge/delivery at home(4)";
  attrib M68_5    format=F00354_. label="Who checked respondent health after discharge/delivery at home(5)";
  attrib M68_6    format=F00354_. label="Who checked respondent health after discharge/delivery at home(6)";
  attrib M69_1    format=F00355_. label="Where respondent check after discharge/delivery at home took place(1)";
  attrib M69_2    format=F00355_. label="Where respondent check after discharge/delivery at home took place(2)";
  attrib M69_3    format=F00355_. label="Where respondent check after discharge/delivery at home took place(3)";
  attrib M69_4    format=F00355_. label="Where respondent check after discharge/delivery at home took place(4)";
  attrib M69_5    format=F00355_. label="Where respondent check after discharge/delivery at home took place(5)";
  attrib M69_6    format=F00355_. label="Where respondent check after discharge/delivery at home took place(6)";
  attrib M70_1    format=F00356_. label="Baby postnatal check within 2 months(1)";
  attrib M70_2    format=F00356_. label="Baby postnatal check within 2 months(2)";
  attrib M70_3    format=F00356_. label="Baby postnatal check within 2 months(3)";
  attrib M70_4    format=F00356_. label="Baby postnatal check within 2 months(4)";
  attrib M70_5    format=F00356_. label="Baby postnatal check within 2 months(5)";
  attrib M70_6    format=F00356_. label="Baby postnatal check within 2 months(6)";
  attrib M71_1    format=F00357_. label="Time after delivery postnatal check took place(1)";
  attrib M71_2    format=F00357_. label="Time after delivery postnatal check took place(2)";
  attrib M71_3    format=F00357_. label="Time after delivery postnatal check took place(3)";
  attrib M71_4    format=F00357_. label="Time after delivery postnatal check took place(4)";
  attrib M71_5    format=F00357_. label="Time after delivery postnatal check took place(5)";
  attrib M71_6    format=F00357_. label="Time after delivery postnatal check took place(6)";
  attrib M72_1    format=F00358_. label="NA - Person who performed postnatal checkup(1)";
  attrib M72_2    format=F00358_. label="NA - Person who performed postnatal checkup(2)";
  attrib M72_3    format=F00358_. label="NA - Person who performed postnatal checkup(3)";
  attrib M72_4    format=F00358_. label="NA - Person who performed postnatal checkup(4)";
  attrib M72_5    format=F00358_. label="NA - Person who performed postnatal checkup(5)";
  attrib M72_6    format=F00358_. label="NA - Person who performed postnatal checkup(6)";
  attrib M73_1    format=F00359_. label="NA - Place baby was first checked(1)";
  attrib M73_2    format=F00359_. label="NA - Place baby was first checked(2)";
  attrib M73_3    format=F00359_. label="NA - Place baby was first checked(3)";
  attrib M73_4    format=F00359_. label="NA - Place baby was first checked(4)";
  attrib M73_5    format=F00359_. label="NA - Place baby was first checked(5)";
  attrib M73_6    format=F00359_. label="NA - Place baby was first checked(6)";
  attrib M74_1    format=F00360_. label="NA - Child's health checked before discharge(1)";
  attrib M74_2    format=F00360_. label="NA - Child's health checked before discharge(2)";
  attrib M74_3    format=F00360_. label="NA - Child's health checked before discharge(3)";
  attrib M74_4    format=F00360_. label="NA - Child's health checked before discharge(4)";
  attrib M74_5    format=F00360_. label="NA - Child's health checked before discharge(5)";
  attrib M74_6    format=F00360_. label="NA - Child's health checked before discharge(6)";
  attrib M75_1    format=F00361_. label="NA - How long before discharging child health check took place(1)";
  attrib M75_2    format=F00361_. label="NA - How long before discharging child health check took place(2)";
  attrib M75_3    format=F00361_. label="NA - How long before discharging child health check took place(3)";
  attrib M75_4    format=F00361_. label="NA - How long before discharging child health check took place(4)";
  attrib M75_5    format=F00361_. label="NA - How long before discharging child health check took place(5)";
  attrib M75_6    format=F00361_. label="NA - How long before discharging child health check took place(6)";
  attrib M76_1    format=F00362_. label="NA - Who checked child health before discharge(1)";
  attrib M76_2    format=F00362_. label="NA - Who checked child health before discharge(2)";
  attrib M76_3    format=F00362_. label="NA - Who checked child health before discharge(3)";
  attrib M76_4    format=F00362_. label="NA - Who checked child health before discharge(4)";
  attrib M76_5    format=F00362_. label="NA - Who checked child health before discharge(5)";
  attrib M76_6    format=F00362_. label="NA - Who checked child health before discharge(6)";
  attrib M77_1    format=F00363_. label="NA - Was child put on mother's bare skin after birth(1)";
  attrib M77_2    format=F00363_. label="NA - Was child put on mother's bare skin after birth(2)";
  attrib M77_3    format=F00363_. label="NA - Was child put on mother's bare skin after birth(3)";
  attrib M77_4    format=F00363_. label="NA - Was child put on mother's bare skin after birth(4)";
  attrib M77_5    format=F00363_. label="NA - Was child put on mother's bare skin after birth(5)";
  attrib M77_6    format=F00363_. label="NA - Was child put on mother's bare skin after birth(6)";
  attrib M78A_1   format=F00364_. label="NA - During first 2 days health provider: examine cord(1)";
  attrib M78A_2   format=F00364_. label="NA - During first 2 days health provider: examine cord(2)";
  attrib M78A_3   format=F00364_. label="NA - During first 2 days health provider: examine cord(3)";
  attrib M78A_4   format=F00364_. label="NA - During first 2 days health provider: examine cord(4)";
  attrib M78A_5   format=F00364_. label="NA - During first 2 days health provider: examine cord(5)";
  attrib M78A_6   format=F00364_. label="NA - During first 2 days health provider: examine cord(6)";
  attrib M78B_1   format=F00365_. label="NA - During first 2 days health provider: measure temperature(1)";
  attrib M78B_2   format=F00365_. label="NA - During first 2 days health provider: measure temperature(2)";
  attrib M78B_3   format=F00365_. label="NA - During first 2 days health provider: measure temperature(3)";
  attrib M78B_4   format=F00365_. label="NA - During first 2 days health provider: measure temperature(4)";
  attrib M78B_5   format=F00365_. label="NA - During first 2 days health provider: measure temperature(5)";
  attrib M78B_6   format=F00365_. label="NA - During first 2 days health provider: measure temperature(6)";
  attrib M78C_1   format=F00366_. label="NA - During first 2 days health provider: counsel on newborn dangers(1)";
  attrib M78C_2   format=F00366_. label="NA - During first 2 days health provider: counsel on newborn dangers(2)";
  attrib M78C_3   format=F00366_. label="NA - During first 2 days health provider: counsel on newborn dangers(3)";
  attrib M78C_4   format=F00366_. label="NA - During first 2 days health provider: counsel on newborn dangers(4)";
  attrib M78C_5   format=F00366_. label="NA - During first 2 days health provider: counsel on newborn dangers(5)";
  attrib M78C_6   format=F00366_. label="NA - During first 2 days health provider: counsel on newborn dangers(6)";
  attrib M78D_1   format=F00367_. label="NA - During first 2 days health provider: counsel on breastfeeding(1)";
  attrib M78D_2   format=F00367_. label="NA - During first 2 days health provider: counsel on breastfeeding(2)";
  attrib M78D_3   format=F00367_. label="NA - During first 2 days health provider: counsel on breastfeeding(3)";
  attrib M78D_4   format=F00367_. label="NA - During first 2 days health provider: counsel on breastfeeding(4)";
  attrib M78D_5   format=F00367_. label="NA - During first 2 days health provider: counsel on breastfeeding(5)";
  attrib M78D_6   format=F00367_. label="NA - During first 2 days health provider: counsel on breastfeeding(6)";
  attrib M78E_1   format=F00368_. label="NA - During first 2 days health provider: observe breastfeeding(1)";
  attrib M78E_2   format=F00368_. label="NA - During first 2 days health provider: observe breastfeeding(2)";
  attrib M78E_3   format=F00368_. label="NA - During first 2 days health provider: observe breastfeeding(3)";
  attrib M78E_4   format=F00368_. label="NA - During first 2 days health provider: observe breastfeeding(4)";
  attrib M78E_5   format=F00368_. label="NA - During first 2 days health provider: observe breastfeeding(5)";
  attrib M78E_6   format=F00368_. label="NA - During first 2 days health provider: observe breastfeeding(6)";
  attrib M78F_1   format=F00369_. label="NA - During first 2 days health provider: CS(1)";
  attrib M78F_2   format=F00369_. label="NA - During first 2 days health provider: CS(2)";
  attrib M78F_3   format=F00369_. label="NA - During first 2 days health provider: CS(3)";
  attrib M78F_4   format=F00369_. label="NA - During first 2 days health provider: CS(4)";
  attrib M78F_5   format=F00369_. label="NA - During first 2 days health provider: CS(5)";
  attrib M78F_6   format=F00369_. label="NA - During first 2 days health provider: CS(6)";
  attrib M78G_1   format=F00370_. label="NA - During first 2 days health provider: CS(1)";
  attrib M78G_2   format=F00370_. label="NA - During first 2 days health provider: CS(2)";
  attrib M78G_3   format=F00370_. label="NA - During first 2 days health provider: CS(3)";
  attrib M78G_4   format=F00370_. label="NA - During first 2 days health provider: CS(4)";
  attrib M78G_5   format=F00370_. label="NA - During first 2 days health provider: CS(5)";
  attrib M78G_6   format=F00370_. label="NA - During first 2 days health provider: CS(6)";
  attrib M78H_1   format=F00371_. label="NA - During first 2 days health provider: CS(1)";
  attrib M78H_2   format=F00371_. label="NA - During first 2 days health provider: CS(2)";
  attrib M78H_3   format=F00371_. label="NA - During first 2 days health provider: CS(3)";
  attrib M78H_4   format=F00371_. label="NA - During first 2 days health provider: CS(4)";
  attrib M78H_5   format=F00371_. label="NA - During first 2 days health provider: CS(5)";
  attrib M78H_6   format=F00371_. label="NA - During first 2 days health provider: CS(6)";
  attrib M78I_1   format=F00372_. label="NA - During first 2 days health provider: CS(1)";
  attrib M78I_2   format=F00372_. label="NA - During first 2 days health provider: CS(2)";
  attrib M78I_3   format=F00372_. label="NA - During first 2 days health provider: CS(3)";
  attrib M78I_4   format=F00372_. label="NA - During first 2 days health provider: CS(4)";
  attrib M78I_5   format=F00372_. label="NA - During first 2 days health provider: CS(5)";
  attrib M78I_6   format=F00372_. label="NA - During first 2 days health provider: CS(6)";
  attrib M78J_1   format=F00373_. label="NA - During first 2 days health provider: CS(1)";
  attrib M78J_2   format=F00373_. label="NA - During first 2 days health provider: CS(2)";
  attrib M78J_3   format=F00373_. label="NA - During first 2 days health provider: CS(3)";
  attrib M78J_4   format=F00373_. label="NA - During first 2 days health provider: CS(4)";
  attrib M78J_5   format=F00373_. label="NA - During first 2 days health provider: CS(5)";
  attrib M78J_6   format=F00373_. label="NA - During first 2 days health provider: CS(6)";
  attrib V401     format=F00374_. label="Last birth a caesarean section";
  attrib V404     format=F00375_. label="NA - Currently breastfeeding";
  attrib V405     format=F00376_. label="Currently amenorrheic";
  attrib V406     format=F00377_. label="Currently abstaining";
  attrib V407     format=F00378_. label="NA - Number of times breastfed during night";
  attrib V408     format=F00379_. label="NA - Number of times breastfed during day";
  attrib V409     format=F00380_. label="NA - Gave child plain water";
  attrib V409A    format=F00381_. label="NA - Gave child sugar water";
  attrib V410     format=F00382_. label="NA - Gave child juice";
  attrib V410A    format=F00383_. label="NA - Gave child tea or coffee";
  attrib V411     format=F00384_. label="NA - Gave child tinned, powdered or fresh milk";
  attrib V411A    format=F00385_. label="NA - Gave child baby formula";
  attrib V412     format=F00386_. label="NA - Gave child fresh milk";
  attrib V412A    format=F00387_. label="NA - Gave child fortified baby food (cerelac, etc)";
  attrib V412B    format=F00388_. label="NA - Gave child other porridge/gruel";
  attrib V412C    format=F00389_. label="NA - Gave child soup/clear broth";
  attrib V413     format=F00390_. label="NA - Gave child other liquid";
  attrib V413A    format=F00391_. label="NA - Gave child CS liquid";
  attrib V413B    format=F00392_. label="NA - Gave child CS liquid";
  attrib V413C    format=F00393_. label="NA - Gave child CS liquid";
  attrib V413D    format=F00394_. label="NA - Gave child CS liquid";
  attrib V414A    format=F00395_. label="NA - Gave child CS foods";
  attrib V414B    format=F00396_. label="NA - Gave child CS foods";
  attrib V414C    format=F00397_. label="NA - Gave child CS foods";
  attrib V414D    format=F00398_. label="NA - Gave child CS foods";
  attrib V414E    format=F00399_. label="NA - Gave child bread, noodles, other made from grains";
  attrib V414F    format=F00400_. label="NA - Gave child potatoes, cassava, or other tubers";
  attrib V414G    format=F00401_. label="NA - Gave child eggs";
  attrib V414H    format=F00402_. label="NA - Gave child meat (beef, pork, lamb, chicken, etc)";
  attrib V414I    format=F00403_. label="NA - Gave child pumpkin, carrots, squash (yellow or orange inside)";
  attrib V414J    format=F00404_. label="NA - Gave child any dark green leafy vegetables";
  attrib V414K    format=F00405_. label="NA - Gave child mangoes, papayas, other vitamin A fruits";
  attrib V414L    format=F00406_. label="NA - Gave child any other fruits";
  attrib V414M    format=F00407_. label="NA - Gave child liver, heart, other organs";
  attrib V414N    format=F00408_. label="NA - Gave child fish or shellfish";
  attrib V414O    format=F00409_. label="NA - Gave child food made from beans, peas, lentils, nuts";
  attrib V414P    format=F00410_. label="NA - Gave child cheese, yogurt, other milk products";
  attrib V414Q    format=F00411_. label="NA - Gave child oil, fats, butter, products made of them";
  attrib V414R    format=F00412_. label="NA - Gave child chocolates, sweets, candies, pastries, etc";
  attrib V414S    format=F00413_. label="NA - Gave child other solid-semisolid food";
  attrib V414T    format=F00414_. label="NA - Gave child CS foods";
  attrib V414U    format=F00415_. label="NA - Gave child CS foods";
  attrib V414V    format=F00416_. label="NA - Gave child yogurt";
  attrib V414W    format=F00417_. label="NA - Gave child CS foods";
  attrib V415     format=F00418_. label="NA - Drank from bottle with nipple yesterday/last night";
  attrib V416     format=F00419_. label="NA - Heard of oral rehydration";
  attrib V417     label="Entries in pregnancy and postnatal care roster";
  attrib V418     label="Entries in immunization roster";
  attrib V418A    label="Entries in child health roster";
  attrib V419     label="Entries in height/weight roster";
  attrib V420     label="NA - Measurer's code";
  attrib V421     label="NA - Assistant measurer's code";
  attrib V426     format=F00420_. label="NA - When child put to breast";
  attrib V437     format=F00421_. label="NA - Respondent's weight in kilograms (1 decimal)";
  attrib V438     format=F00422_. label="NA - Respondent's height in centimeters (1 decimal)";
  attrib V439     format=F00423_. label="NA - Height/Age percentile";
  attrib V440     format=F00424_. label="Height/Age standard deviation";
  attrib V441     format=F00425_. label="NA - Height/Age percent ref. median";
  attrib V442     format=F00426_. label="NA - Weight/Height percent ref. median (DHS)";
  attrib V443     format=F00427_. label="NA - Weight/Height percent ref. median (Fog)";
  attrib V444     format=F00428_. label="NA - Weight/Height percent ref. median (WHO)";
  attrib V444A    format=F00429_. label="Weight/Height standard deviation (DHS)";
  attrib V445     format=F00430_. label="NA - Body Mass Index";
  attrib V446     format=F00431_. label="NA - Rohrer's index";
  attrib V447     format=F00432_. label="NA - Result of measurement - height/weight";
  attrib V447A    label="NA - Women's age in years (from household questionnaire)";
  attrib V452A    format=F00433_. label="NA - Under age 18 (from household questionnaire)";
  attrib V452B    format=F00434_. label="NA - Line number of parent/caretaker";
  attrib V452C    format=F00435_. label="NA - Read consent statement - hemoglobin";
  attrib V453     label="NA - Hemoglobin level (g/dl - 1 decimal)";
  attrib V454     format=F00436_. label="NA - Currently pregnant (from household questionnaire)";
  attrib V455     format=F00437_. label="NA - Result of measurement - hemoglobin";
  attrib V456     label="NA - Hemoglobin level adjusted for altitude and smoking (g/dl - 1 decimal)";
  attrib V457     format=F00438_. label="NA - Anemia level";
  attrib V458     format=F00439_. label="NA - Agrees to referral - anemia";
  attrib V459     format=F00440_. label="NA - Have mosquito bed net for sleeping (from household questionnaire)";
  attrib V460     format=F00441_. label="NA - Children under 5 slept under mosquito bed net last night (household questionnaire)";
  attrib V461     format=F00442_. label="NA - Respondent slept under mosquito bed net";
  attrib V462     format=F00443_. label="NA - Washed hands before preparing last meal";
  attrib V463A    format=F00444_. label="NA - Smokes cigarettes";
  attrib V463B    format=F00445_. label="NA - Smokes pipe full of tobacco";
  attrib V463C    format=F00446_. label="NA - Chews tobacco";
  attrib V463D    format=F00447_. label="NA - Snuffs by nose";
  attrib V463E    format=F00448_. label="NA - Smokes kreteks";
  attrib V463F    format=F00449_. label="NA - Smokes cigars, cheroots or cigarillos";
  attrib V463G    format=F00450_. label="NA - Smokes water pipe";
  attrib V463H    format=F00451_. label="NA - Snuff by mouth";
  attrib V463I    format=F00452_. label="NA - Chews betel quid with tobacco";
  attrib V463J    format=F00453_. label="NA - Smokes country specific";
  attrib V463K    format=F00454_. label="NA - Smokes country specific";
  attrib V463L    format=F00455_. label="NA - Smokes country specific";
  attrib V463X    format=F00456_. label="NA - Smokes other";
  attrib V463Z    format=F00457_. label="NA - Does not use cigarettes and tobacco";
  attrib V463AA   format=F00458_. label="NA - Frequency smokes cigarettes";
  attrib V463AB   format=F00459_. label="NA - Frequency smokes or uses other type of tobacco";
  attrib V464     format=F00460_. label="NA - Number of cigarettes in last 24 hours";
  attrib V465     format=F00461_. label="NA - Disposal of youngest child's stools when not using toilet";
  attrib V466     format=F00462_. label="NA - When child is seriously ill, can decide whether medical treatment sought";
  attrib V467A    format=F00463_. label="NA - Getting medical help for self: know where to go";
  attrib V467B    format=F00464_. label="NA - Getting medical help for self: getting permission to go";
  attrib V467C    format=F00465_. label="NA - Getting medical help for self: getting money needed for treatment";
  attrib V467D    format=F00466_. label="NA - Getting medical help for self: distance to health facility";
  attrib V467E    format=F00467_. label="NA - Getting medical help for self: having to take transport";
  attrib V467F    format=F00468_. label="NA - Getting medical help for self: not wanting to go alone";
  attrib V467G    format=F00469_. label="NA - Getting medical help for self: concern no female health provider";
  attrib V467H    format=F00470_. label="NA - Getting medical help for self: concern no provider";
  attrib V467I    format=F00471_. label="NA - Getting medical help for self: concern no drugs available";
  attrib V467J    format=F00472_. label="NA - Getting medical help for self: CS";
  attrib V467K    format=F00473_. label="NA - Getting medical help for self: CS";
  attrib V467L    format=F00474_. label="NA - Getting medical help for self: CS";
  attrib V467M    format=F00475_. label="NA - Getting medical help for self: CS";
  attrib V468     format=F00476_. label="NA - Record for Last Birth";
  attrib V469E    format=F00477_. label="NA - Times gave child powdered/tinned/fresh milk";
  attrib V469F    format=F00478_. label="NA - Times gave child infant formula";
  attrib V469X    format=F00479_. label="NA - Times gave child yogurt";
  attrib V471A    format=F00480_. label="NA - Mother had tinned, powdered or fresh milk";
  attrib V471B    format=F00481_. label="NA - Mother had tea or coffee";
  attrib V471C    format=F00482_. label="NA - Mother had any other liquid";
  attrib V471D    format=F00483_. label="NA - Mother had CS liquid";
  attrib V471E    format=F00484_. label="NA - Mother had CS liquid";
  attrib V471F    format=F00485_. label="NA - Mother had CS liquid";
  attrib V471G    format=F00486_. label="NA - Mother had CS liquid";
  attrib V472A    format=F00487_. label="NA - Mother had CS foods";
  attrib V472B    format=F00488_. label="NA - Mother had CS foods";
  attrib V472C    format=F00489_. label="NA - Mother had CS foods";
  attrib V472D    format=F00490_. label="NA - Mother had CS foods";
  attrib V472E    format=F00491_. label="NA - Mother had bread, noodles, other made from grains";
  attrib V472F    format=F00492_. label="NA - Mother had potatoes, cassava, or other tubers";
  attrib V472G    format=F00493_. label="NA - Mother had eggs";
  attrib V472H    format=F00494_. label="NA - Mother had meat (beef, pork, lamb, chicken, etc)";
  attrib V472I    format=F00495_. label="NA - Mother had pumpkin, carrots, squash (yellow or orange inside)";
  attrib V472J    format=F00496_. label="NA - Mother had any dark green leafy vegetables";
  attrib V472K    format=F00497_. label="NA - Mother had mangoes, papayas, other vitamin A fruits";
  attrib V472L    format=F00498_. label="NA - Mother had any other fruits";
  attrib V472M    format=F00499_. label="NA - Mother had liver, heart, other organs";
  attrib V472N    format=F00500_. label="NA - Mother had fish or shellfish";
  attrib V472O    format=F00501_. label="NA - Mother had food made from beans, peas, lentils, nuts";
  attrib V472P    format=F00502_. label="NA - Mother had cheese, yogurt, other milk products";
  attrib V472Q    format=F00503_. label="NA - Mother had oil, fats, butter, products made of them";
  attrib V472R    format=F00504_. label="NA - Mother had chocolates, sweets, candies, pastries, etc";
  attrib V472S    format=F00505_. label="NA - Mother had other solid-semisolid food";
  attrib V472T    format=F00506_. label="NA - Mother had CS foods";
  attrib V472U    format=F00507_. label="NA - Mother had CS foods";
  attrib V473A    format=F00508_. label="NA - Read consent statement - HIV";
  attrib V473B    format=F00509_. label="NA - Result of measurement - HIV";
  attrib V474     format=F00510_. label="NA - Heard of tuberculosis or TB";
  attrib V474A    format=F00511_. label="NA - Tuberculosis spread by: air when coughing or sneezing";
  attrib V474B    format=F00512_. label="NA - Tuberculosis spread by: sharing utensils";
  attrib V474C    format=F00513_. label="NA - Tuberculosis spread by: touching a person with TB";
  attrib V474D    format=F00514_. label="NA - Tuberculosis spread by: food";
  attrib V474E    format=F00515_. label="NA - Tuberculosis spread by: sexual contact";
  attrib V474F    format=F00516_. label="NA - Tuberculosis spread by: mosquito bites";
  attrib V474G    format=F00517_. label="NA - Tuberculosis spread by: CS";
  attrib V474H    format=F00518_. label="NA - Tuberculosis spread by: CS";
  attrib V474I    format=F00519_. label="NA - Tuberculosis spread by: CS";
  attrib V474J    format=F00520_. label="NA - Tuberculosis spread by: CS";
  attrib V474X    format=F00521_. label="NA - Tuberculosis spread by: other";
  attrib V474Z    format=F00522_. label="NA - Tuberculosis spread by: don't know";
  attrib V475     format=F00523_. label="NA - Tuberculosis can be cured";
  attrib V476     format=F00524_. label="NA - Keep secret when family member gets TB";
  attrib V477     format=F00525_. label="NA - Number of injections in last 12 months";
  attrib V478     format=F00526_. label="NA - Number of injections administered by a health worker";
  attrib V479     format=F00527_. label="NA - Place of injection by health worker";
  attrib V480     format=F00528_. label="NA - Syringe and needle from new, unopened package";
  attrib V481     format=F00529_. label="NA - Covered by health insurance";
  attrib V481A    format=F00530_. label="NA - Health insurance type: mutual/community organization";
  attrib V481B    format=F00531_. label="NA - Health insurance type: provided by employer";
  attrib V481C    format=F00532_. label="NA - Health insurance type: social security";
  attrib V481D    format=F00533_. label="NA - Health insurance type: private/commercially purchased";
  attrib V481E    format=F00534_. label="NA - Health insurance type: CS";
  attrib V481F    format=F00535_. label="NA - Health insurance type: CS";
  attrib V481G    format=F00536_. label="NA - Health insurance type: CS";
  attrib V481H    format=F00537_. label="NA - Health insurance type: CS";
  attrib V481X    format=F00538_. label="NA - Health insurance type: other";
  attrib V482A    format=F00539_. label="NA - Arrange care of biological children under age 18";
  attrib V482B    format=F00540_. label="NA - Primary caregiver of children under age 18";
  attrib V482C    format=F00541_. label="NA - Arrange care of non-biological children < 18";
  attrib HIDX_1   label="NA - Index to birth history(1)";
  attrib HIDX_2   label="NA - Index to birth history(2)";
  attrib HIDX_3   label="NA - Index to birth history(3)";
  attrib HIDX_4   label="NA - Index to birth history(4)";
  attrib HIDX_5   label="NA - Index to birth history(5)";
  attrib HIDX_6   label="NA - Index to birth history(6)";
  attrib H1_1     format=F00542_. label="NA - Has health card(1)";
  attrib H1_2     format=F00542_. label="NA - Has health card(2)";
  attrib H1_3     format=F00542_. label="NA - Has health card(3)";
  attrib H1_4     format=F00542_. label="NA - Has health card(4)";
  attrib H1_5     format=F00542_. label="NA - Has health card(5)";
  attrib H1_6     format=F00542_. label="NA - Has health card(6)";
  attrib H1A_1    format=F00543_. label="NA - Has health card and or other vaccination document(1)";
  attrib H1A_2    format=F00543_. label="NA - Has health card and or other vaccination document(2)";
  attrib H1A_3    format=F00543_. label="NA - Has health card and or other vaccination document(3)";
  attrib H1A_4    format=F00543_. label="NA - Has health card and or other vaccination document(4)";
  attrib H1A_5    format=F00543_. label="NA - Has health card and or other vaccination document(5)";
  attrib H1A_6    format=F00543_. label="NA - Has health card and or other vaccination document(6)";
  attrib H2_1     format=F00544_. label="NA - Received BCG(1)";
  attrib H2_2     format=F00544_. label="NA - Received BCG(2)";
  attrib H2_3     format=F00544_. label="NA - Received BCG(3)";
  attrib H2_4     format=F00544_. label="NA - Received BCG(4)";
  attrib H2_5     format=F00544_. label="NA - Received BCG(5)";
  attrib H2_6     format=F00544_. label="NA - Received BCG(6)";
  attrib H2D_1    format=F00545_. label="NA - BCG day(1)";
  attrib H2D_2    format=F00545_. label="NA - BCG day(2)";
  attrib H2D_3    format=F00545_. label="NA - BCG day(3)";
  attrib H2D_4    format=F00545_. label="NA - BCG day(4)";
  attrib H2D_5    format=F00545_. label="NA - BCG day(5)";
  attrib H2D_6    format=F00545_. label="NA - BCG day(6)";
  attrib H2M_1    format=F00546_. label="NA - BCG month(1)";
  attrib H2M_2    format=F00546_. label="NA - BCG month(2)";
  attrib H2M_3    format=F00546_. label="NA - BCG month(3)";
  attrib H2M_4    format=F00546_. label="NA - BCG month(4)";
  attrib H2M_5    format=F00546_. label="NA - BCG month(5)";
  attrib H2M_6    format=F00546_. label="NA - BCG month(6)";
  attrib H2Y_1    format=F00547_. label="NA - BCG year(1)";
  attrib H2Y_2    format=F00547_. label="NA - BCG year(2)";
  attrib H2Y_3    format=F00547_. label="NA - BCG year(3)";
  attrib H2Y_4    format=F00547_. label="NA - BCG year(4)";
  attrib H2Y_5    format=F00547_. label="NA - BCG year(5)";
  attrib H2Y_6    format=F00547_. label="NA - BCG year(6)";
  attrib H3_1     format=F00548_. label="NA - Received DPT 1(1)";
  attrib H3_2     format=F00548_. label="NA - Received DPT 1(2)";
  attrib H3_3     format=F00548_. label="NA - Received DPT 1(3)";
  attrib H3_4     format=F00548_. label="NA - Received DPT 1(4)";
  attrib H3_5     format=F00548_. label="NA - Received DPT 1(5)";
  attrib H3_6     format=F00548_. label="NA - Received DPT 1(6)";
  attrib H3D_1    format=F00549_. label="NA - DPT 1 day(1)";
  attrib H3D_2    format=F00549_. label="NA - DPT 1 day(2)";
  attrib H3D_3    format=F00549_. label="NA - DPT 1 day(3)";
  attrib H3D_4    format=F00549_. label="NA - DPT 1 day(4)";
  attrib H3D_5    format=F00549_. label="NA - DPT 1 day(5)";
  attrib H3D_6    format=F00549_. label="NA - DPT 1 day(6)";
  attrib H3M_1    format=F00550_. label="NA - DPT 1 month(1)";
  attrib H3M_2    format=F00550_. label="NA - DPT 1 month(2)";
  attrib H3M_3    format=F00550_. label="NA - DPT 1 month(3)";
  attrib H3M_4    format=F00550_. label="NA - DPT 1 month(4)";
  attrib H3M_5    format=F00550_. label="NA - DPT 1 month(5)";
  attrib H3M_6    format=F00550_. label="NA - DPT 1 month(6)";
  attrib H3Y_1    format=F00551_. label="NA - DPT 1 year(1)";
  attrib H3Y_2    format=F00551_. label="NA - DPT 1 year(2)";
  attrib H3Y_3    format=F00551_. label="NA - DPT 1 year(3)";
  attrib H3Y_4    format=F00551_. label="NA - DPT 1 year(4)";
  attrib H3Y_5    format=F00551_. label="NA - DPT 1 year(5)";
  attrib H3Y_6    format=F00551_. label="NA - DPT 1 year(6)";
  attrib H4_1     format=F00552_. label="NA - Received POLIO 1(1)";
  attrib H4_2     format=F00552_. label="NA - Received POLIO 1(2)";
  attrib H4_3     format=F00552_. label="NA - Received POLIO 1(3)";
  attrib H4_4     format=F00552_. label="NA - Received POLIO 1(4)";
  attrib H4_5     format=F00552_. label="NA - Received POLIO 1(5)";
  attrib H4_6     format=F00552_. label="NA - Received POLIO 1(6)";
  attrib H4D_1    format=F00553_. label="NA - POLIO 1 day(1)";
  attrib H4D_2    format=F00553_. label="NA - POLIO 1 day(2)";
  attrib H4D_3    format=F00553_. label="NA - POLIO 1 day(3)";
  attrib H4D_4    format=F00553_. label="NA - POLIO 1 day(4)";
  attrib H4D_5    format=F00553_. label="NA - POLIO 1 day(5)";
  attrib H4D_6    format=F00553_. label="NA - POLIO 1 day(6)";
  attrib H4M_1    format=F00554_. label="NA - POLIO 1 month(1)";
  attrib H4M_2    format=F00554_. label="NA - POLIO 1 month(2)";
  attrib H4M_3    format=F00554_. label="NA - POLIO 1 month(3)";
  attrib H4M_4    format=F00554_. label="NA - POLIO 1 month(4)";
  attrib H4M_5    format=F00554_. label="NA - POLIO 1 month(5)";
  attrib H4M_6    format=F00554_. label="NA - POLIO 1 month(6)";
  attrib H4Y_1    format=F00555_. label="NA - POLIO 1 year(1)";
  attrib H4Y_2    format=F00555_. label="NA - POLIO 1 year(2)";
  attrib H4Y_3    format=F00555_. label="NA - POLIO 1 year(3)";
  attrib H4Y_4    format=F00555_. label="NA - POLIO 1 year(4)";
  attrib H4Y_5    format=F00555_. label="NA - POLIO 1 year(5)";
  attrib H4Y_6    format=F00555_. label="NA - POLIO 1 year(6)";
  attrib H5_1     format=F00556_. label="NA - Received DPT 2(1)";
  attrib H5_2     format=F00556_. label="NA - Received DPT 2(2)";
  attrib H5_3     format=F00556_. label="NA - Received DPT 2(3)";
  attrib H5_4     format=F00556_. label="NA - Received DPT 2(4)";
  attrib H5_5     format=F00556_. label="NA - Received DPT 2(5)";
  attrib H5_6     format=F00556_. label="NA - Received DPT 2(6)";
  attrib H5D_1    format=F00557_. label="NA - DPT 2 day(1)";
  attrib H5D_2    format=F00557_. label="NA - DPT 2 day(2)";
  attrib H5D_3    format=F00557_. label="NA - DPT 2 day(3)";
  attrib H5D_4    format=F00557_. label="NA - DPT 2 day(4)";
  attrib H5D_5    format=F00557_. label="NA - DPT 2 day(5)";
  attrib H5D_6    format=F00557_. label="NA - DPT 2 day(6)";
  attrib H5M_1    format=F00558_. label="NA - DPT 2 month(1)";
  attrib H5M_2    format=F00558_. label="NA - DPT 2 month(2)";
  attrib H5M_3    format=F00558_. label="NA - DPT 2 month(3)";
  attrib H5M_4    format=F00558_. label="NA - DPT 2 month(4)";
  attrib H5M_5    format=F00558_. label="NA - DPT 2 month(5)";
  attrib H5M_6    format=F00558_. label="NA - DPT 2 month(6)";
  attrib H5Y_1    format=F00559_. label="NA - DPT 2 year(1)";
  attrib H5Y_2    format=F00559_. label="NA - DPT 2 year(2)";
  attrib H5Y_3    format=F00559_. label="NA - DPT 2 year(3)";
  attrib H5Y_4    format=F00559_. label="NA - DPT 2 year(4)";
  attrib H5Y_5    format=F00559_. label="NA - DPT 2 year(5)";
  attrib H5Y_6    format=F00559_. label="NA - DPT 2 year(6)";
  attrib H6_1     format=F00560_. label="NA - Received POLIO 2(1)";
  attrib H6_2     format=F00560_. label="NA - Received POLIO 2(2)";
  attrib H6_3     format=F00560_. label="NA - Received POLIO 2(3)";
  attrib H6_4     format=F00560_. label="NA - Received POLIO 2(4)";
  attrib H6_5     format=F00560_. label="NA - Received POLIO 2(5)";
  attrib H6_6     format=F00560_. label="NA - Received POLIO 2(6)";
  attrib H6D_1    format=F00561_. label="NA - POLIO 2 day(1)";
  attrib H6D_2    format=F00561_. label="NA - POLIO 2 day(2)";
  attrib H6D_3    format=F00561_. label="NA - POLIO 2 day(3)";
  attrib H6D_4    format=F00561_. label="NA - POLIO 2 day(4)";
  attrib H6D_5    format=F00561_. label="NA - POLIO 2 day(5)";
  attrib H6D_6    format=F00561_. label="NA - POLIO 2 day(6)";
  attrib H6M_1    format=F00562_. label="NA - POLIO 2 month(1)";
  attrib H6M_2    format=F00562_. label="NA - POLIO 2 month(2)";
  attrib H6M_3    format=F00562_. label="NA - POLIO 2 month(3)";
  attrib H6M_4    format=F00562_. label="NA - POLIO 2 month(4)";
  attrib H6M_5    format=F00562_. label="NA - POLIO 2 month(5)";
  attrib H6M_6    format=F00562_. label="NA - POLIO 2 month(6)";
  attrib H6Y_1    format=F00563_. label="NA - POLIO 2 year(1)";
  attrib H6Y_2    format=F00563_. label="NA - POLIO 2 year(2)";
  attrib H6Y_3    format=F00563_. label="NA - POLIO 2 year(3)";
  attrib H6Y_4    format=F00563_. label="NA - POLIO 2 year(4)";
  attrib H6Y_5    format=F00563_. label="NA - POLIO 2 year(5)";
  attrib H6Y_6    format=F00563_. label="NA - POLIO 2 year(6)";
  attrib H7_1     format=F00564_. label="NA - Received DPT 3(1)";
  attrib H7_2     format=F00564_. label="NA - Received DPT 3(2)";
  attrib H7_3     format=F00564_. label="NA - Received DPT 3(3)";
  attrib H7_4     format=F00564_. label="NA - Received DPT 3(4)";
  attrib H7_5     format=F00564_. label="NA - Received DPT 3(5)";
  attrib H7_6     format=F00564_. label="NA - Received DPT 3(6)";
  attrib H7D_1    format=F00565_. label="NA - DPT 3 day(1)";
  attrib H7D_2    format=F00565_. label="NA - DPT 3 day(2)";
  attrib H7D_3    format=F00565_. label="NA - DPT 3 day(3)";
  attrib H7D_4    format=F00565_. label="NA - DPT 3 day(4)";
  attrib H7D_5    format=F00565_. label="NA - DPT 3 day(5)";
  attrib H7D_6    format=F00565_. label="NA - DPT 3 day(6)";
  attrib H7M_1    format=F00566_. label="NA - DPT 3 month(1)";
  attrib H7M_2    format=F00566_. label="NA - DPT 3 month(2)";
  attrib H7M_3    format=F00566_. label="NA - DPT 3 month(3)";
  attrib H7M_4    format=F00566_. label="NA - DPT 3 month(4)";
  attrib H7M_5    format=F00566_. label="NA - DPT 3 month(5)";
  attrib H7M_6    format=F00566_. label="NA - DPT 3 month(6)";
  attrib H7Y_1    format=F00567_. label="NA - DPT 3 year(1)";
  attrib H7Y_2    format=F00567_. label="NA - DPT 3 year(2)";
  attrib H7Y_3    format=F00567_. label="NA - DPT 3 year(3)";
  attrib H7Y_4    format=F00567_. label="NA - DPT 3 year(4)";
  attrib H7Y_5    format=F00567_. label="NA - DPT 3 year(5)";
  attrib H7Y_6    format=F00567_. label="NA - DPT 3 year(6)";
  attrib H8_1     format=F00568_. label="NA - Received POLIO 3(1)";
  attrib H8_2     format=F00568_. label="NA - Received POLIO 3(2)";
  attrib H8_3     format=F00568_. label="NA - Received POLIO 3(3)";
  attrib H8_4     format=F00568_. label="NA - Received POLIO 3(4)";
  attrib H8_5     format=F00568_. label="NA - Received POLIO 3(5)";
  attrib H8_6     format=F00568_. label="NA - Received POLIO 3(6)";
  attrib H8D_1    format=F00569_. label="NA - POLIO 3 day(1)";
  attrib H8D_2    format=F00569_. label="NA - POLIO 3 day(2)";
  attrib H8D_3    format=F00569_. label="NA - POLIO 3 day(3)";
  attrib H8D_4    format=F00569_. label="NA - POLIO 3 day(4)";
  attrib H8D_5    format=F00569_. label="NA - POLIO 3 day(5)";
  attrib H8D_6    format=F00569_. label="NA - POLIO 3 day(6)";
  attrib H8M_1    format=F00570_. label="NA - POLIO 3 month(1)";
  attrib H8M_2    format=F00570_. label="NA - POLIO 3 month(2)";
  attrib H8M_3    format=F00570_. label="NA - POLIO 3 month(3)";
  attrib H8M_4    format=F00570_. label="NA - POLIO 3 month(4)";
  attrib H8M_5    format=F00570_. label="NA - POLIO 3 month(5)";
  attrib H8M_6    format=F00570_. label="NA - POLIO 3 month(6)";
  attrib H8Y_1    format=F00571_. label="NA - POLIO 3 year(1)";
  attrib H8Y_2    format=F00571_. label="NA - POLIO 3 year(2)";
  attrib H8Y_3    format=F00571_. label="NA - POLIO 3 year(3)";
  attrib H8Y_4    format=F00571_. label="NA - POLIO 3 year(4)";
  attrib H8Y_5    format=F00571_. label="NA - POLIO 3 year(5)";
  attrib H8Y_6    format=F00571_. label="NA - POLIO 3 year(6)";
  attrib H9_1     format=F00572_. label="NA - Received MEASLES 1(1)";
  attrib H9_2     format=F00572_. label="NA - Received MEASLES 1(2)";
  attrib H9_3     format=F00572_. label="NA - Received MEASLES 1(3)";
  attrib H9_4     format=F00572_. label="NA - Received MEASLES 1(4)";
  attrib H9_5     format=F00572_. label="NA - Received MEASLES 1(5)";
  attrib H9_6     format=F00572_. label="NA - Received MEASLES 1(6)";
  attrib H9D_1    format=F00573_. label="NA - MEASLES 1 day(1)";
  attrib H9D_2    format=F00573_. label="NA - MEASLES 1 day(2)";
  attrib H9D_3    format=F00573_. label="NA - MEASLES 1 day(3)";
  attrib H9D_4    format=F00573_. label="NA - MEASLES 1 day(4)";
  attrib H9D_5    format=F00573_. label="NA - MEASLES 1 day(5)";
  attrib H9D_6    format=F00573_. label="NA - MEASLES 1 day(6)";
  attrib H9M_1    format=F00574_. label="NA - MEASLES 1 month(1)";
  attrib H9M_2    format=F00574_. label="NA - MEASLES 1 month(2)";
  attrib H9M_3    format=F00574_. label="NA - MEASLES 1 month(3)";
  attrib H9M_4    format=F00574_. label="NA - MEASLES 1 month(4)";
  attrib H9M_5    format=F00574_. label="NA - MEASLES 1 month(5)";
  attrib H9M_6    format=F00574_. label="NA - MEASLES 1 month(6)";
  attrib H9Y_1    format=F00575_. label="NA - MEASLES 1 year(1)";
  attrib H9Y_2    format=F00575_. label="NA - MEASLES 1 year(2)";
  attrib H9Y_3    format=F00575_. label="NA - MEASLES 1 year(3)";
  attrib H9Y_4    format=F00575_. label="NA - MEASLES 1 year(4)";
  attrib H9Y_5    format=F00575_. label="NA - MEASLES 1 year(5)";
  attrib H9Y_6    format=F00575_. label="NA - MEASLES 1 year(6)";
  attrib H9A_1    format=F00576_. label="NA - Received MEASLES 1(1)";
  attrib H9A_2    format=F00576_. label="NA - Received MEASLES 1(2)";
  attrib H9A_3    format=F00576_. label="NA - Received MEASLES 1(3)";
  attrib H9A_4    format=F00576_. label="NA - Received MEASLES 1(4)";
  attrib H9A_5    format=F00576_. label="NA - Received MEASLES 1(5)";
  attrib H9A_6    format=F00576_. label="NA - Received MEASLES 1(6)";
  attrib H9AD_1   format=F00577_. label="NA - MEASLES 1 day(1)";
  attrib H9AD_2   format=F00577_. label="NA - MEASLES 1 day(2)";
  attrib H9AD_3   format=F00577_. label="NA - MEASLES 1 day(3)";
  attrib H9AD_4   format=F00577_. label="NA - MEASLES 1 day(4)";
  attrib H9AD_5   format=F00577_. label="NA - MEASLES 1 day(5)";
  attrib H9AD_6   format=F00577_. label="NA - MEASLES 1 day(6)";
  attrib H9AM_1   format=F00578_. label="NA - MEASLES 1 month(1)";
  attrib H9AM_2   format=F00578_. label="NA - MEASLES 1 month(2)";
  attrib H9AM_3   format=F00578_. label="NA - MEASLES 1 month(3)";
  attrib H9AM_4   format=F00578_. label="NA - MEASLES 1 month(4)";
  attrib H9AM_5   format=F00578_. label="NA - MEASLES 1 month(5)";
  attrib H9AM_6   format=F00578_. label="NA - MEASLES 1 month(6)";
  attrib H9AY_1   format=F00579_. label="NA - MEASLES 1 year(1)";
  attrib H9AY_2   format=F00579_. label="NA - MEASLES 1 year(2)";
  attrib H9AY_3   format=F00579_. label="NA - MEASLES 1 year(3)";
  attrib H9AY_4   format=F00579_. label="NA - MEASLES 1 year(4)";
  attrib H9AY_5   format=F00579_. label="NA - MEASLES 1 year(5)";
  attrib H9AY_6   format=F00579_. label="NA - MEASLES 1 year(6)";
  attrib H0_1     format=F00580_. label="NA - Received POLIO 0(1)";
  attrib H0_2     format=F00580_. label="NA - Received POLIO 0(2)";
  attrib H0_3     format=F00580_. label="NA - Received POLIO 0(3)";
  attrib H0_4     format=F00580_. label="NA - Received POLIO 0(4)";
  attrib H0_5     format=F00580_. label="NA - Received POLIO 0(5)";
  attrib H0_6     format=F00580_. label="NA - Received POLIO 0(6)";
  attrib H0D_1    format=F00581_. label="NA - POLIO 0 day(1)";
  attrib H0D_2    format=F00581_. label="NA - POLIO 0 day(2)";
  attrib H0D_3    format=F00581_. label="NA - POLIO 0 day(3)";
  attrib H0D_4    format=F00581_. label="NA - POLIO 0 day(4)";
  attrib H0D_5    format=F00581_. label="NA - POLIO 0 day(5)";
  attrib H0D_6    format=F00581_. label="NA - POLIO 0 day(6)";
  attrib H0M_1    format=F00582_. label="NA - POLIO 0 month(1)";
  attrib H0M_2    format=F00582_. label="NA - POLIO 0 month(2)";
  attrib H0M_3    format=F00582_. label="NA - POLIO 0 month(3)";
  attrib H0M_4    format=F00582_. label="NA - POLIO 0 month(4)";
  attrib H0M_5    format=F00582_. label="NA - POLIO 0 month(5)";
  attrib H0M_6    format=F00582_. label="NA - POLIO 0 month(6)";
  attrib H0Y_1    format=F00583_. label="NA - POLIO 0 year(1)";
  attrib H0Y_2    format=F00583_. label="NA - POLIO 0 year(2)";
  attrib H0Y_3    format=F00583_. label="NA - POLIO 0 year(3)";
  attrib H0Y_4    format=F00583_. label="NA - POLIO 0 year(4)";
  attrib H0Y_5    format=F00583_. label="NA - POLIO 0 year(5)";
  attrib H0Y_6    format=F00583_. label="NA - POLIO 0 year(6)";
  attrib H10_1    format=F00584_. label="NA - Ever had vaccination(1)";
  attrib H10_2    format=F00584_. label="NA - Ever had vaccination(2)";
  attrib H10_3    format=F00584_. label="NA - Ever had vaccination(3)";
  attrib H10_4    format=F00584_. label="NA - Ever had vaccination(4)";
  attrib H10_5    format=F00584_. label="NA - Ever had vaccination(5)";
  attrib H10_6    format=F00584_. label="NA - Ever had vaccination(6)";
  attrib H33_1    format=F00585_. label="NA - Received Vitamin A1 (most recent)(1)";
  attrib H33_2    format=F00585_. label="NA - Received Vitamin A1 (most recent)(2)";
  attrib H33_3    format=F00585_. label="NA - Received Vitamin A1 (most recent)(3)";
  attrib H33_4    format=F00585_. label="NA - Received Vitamin A1 (most recent)(4)";
  attrib H33_5    format=F00585_. label="NA - Received Vitamin A1 (most recent)(5)";
  attrib H33_6    format=F00585_. label="NA - Received Vitamin A1 (most recent)(6)";
  attrib H33D_1   format=F00586_. label="NA - Vitamin A1 day(1)";
  attrib H33D_2   format=F00586_. label="NA - Vitamin A1 day(2)";
  attrib H33D_3   format=F00586_. label="NA - Vitamin A1 day(3)";
  attrib H33D_4   format=F00586_. label="NA - Vitamin A1 day(4)";
  attrib H33D_5   format=F00586_. label="NA - Vitamin A1 day(5)";
  attrib H33D_6   format=F00586_. label="NA - Vitamin A1 day(6)";
  attrib H33M_1   format=F00587_. label="NA - Vitamin A1 month(1)";
  attrib H33M_2   format=F00587_. label="NA - Vitamin A1 month(2)";
  attrib H33M_3   format=F00587_. label="NA - Vitamin A1 month(3)";
  attrib H33M_4   format=F00587_. label="NA - Vitamin A1 month(4)";
  attrib H33M_5   format=F00587_. label="NA - Vitamin A1 month(5)";
  attrib H33M_6   format=F00587_. label="NA - Vitamin A1 month(6)";
  attrib H33Y_1   format=F00588_. label="NA - Vitamin A1 year(1)";
  attrib H33Y_2   format=F00588_. label="NA - Vitamin A1 year(2)";
  attrib H33Y_3   format=F00588_. label="NA - Vitamin A1 year(3)";
  attrib H33Y_4   format=F00588_. label="NA - Vitamin A1 year(4)";
  attrib H33Y_5   format=F00588_. label="NA - Vitamin A1 year(5)";
  attrib H33Y_6   format=F00588_. label="NA - Vitamin A1 year(6)";
  attrib H35_1    format=F00589_. label="NA - Any vaccinations in last 2 years part of campaign(1)";
  attrib H35_2    format=F00589_. label="NA - Any vaccinations in last 2 years part of campaign(2)";
  attrib H35_3    format=F00589_. label="NA - Any vaccinations in last 2 years part of campaign(3)";
  attrib H35_4    format=F00589_. label="NA - Any vaccinations in last 2 years part of campaign(4)";
  attrib H35_5    format=F00589_. label="NA - Any vaccinations in last 2 years part of campaign(5)";
  attrib H35_6    format=F00589_. label="NA - Any vaccinations in last 2 years part of campaign(6)";
  attrib H36A_1   format=F00590_. label="NA - Vaccinated during Campaign A(1)";
  attrib H36A_2   format=F00590_. label="NA - Vaccinated during Campaign A(2)";
  attrib H36A_3   format=F00590_. label="NA - Vaccinated during Campaign A(3)";
  attrib H36A_4   format=F00590_. label="NA - Vaccinated during Campaign A(4)";
  attrib H36A_5   format=F00590_. label="NA - Vaccinated during Campaign A(5)";
  attrib H36A_6   format=F00590_. label="NA - Vaccinated during Campaign A(6)";
  attrib H36B_1   format=F00591_. label="NA - Vaccinated during Campaign B(1)";
  attrib H36B_2   format=F00591_. label="NA - Vaccinated during Campaign B(2)";
  attrib H36B_3   format=F00591_. label="NA - Vaccinated during Campaign B(3)";
  attrib H36B_4   format=F00591_. label="NA - Vaccinated during Campaign B(4)";
  attrib H36B_5   format=F00591_. label="NA - Vaccinated during Campaign B(5)";
  attrib H36B_6   format=F00591_. label="NA - Vaccinated during Campaign B(6)";
  attrib H36C_1   format=F00592_. label="NA - Vaccinated during Campaign C(1)";
  attrib H36C_2   format=F00592_. label="NA - Vaccinated during Campaign C(2)";
  attrib H36C_3   format=F00592_. label="NA - Vaccinated during Campaign C(3)";
  attrib H36C_4   format=F00592_. label="NA - Vaccinated during Campaign C(4)";
  attrib H36C_5   format=F00592_. label="NA - Vaccinated during Campaign C(5)";
  attrib H36C_6   format=F00592_. label="NA - Vaccinated during Campaign C(6)";
  attrib H36D_1   format=F00593_. label="NA - Vaccinated during Campaign D(1)";
  attrib H36D_2   format=F00593_. label="NA - Vaccinated during Campaign D(2)";
  attrib H36D_3   format=F00593_. label="NA - Vaccinated during Campaign D(3)";
  attrib H36D_4   format=F00593_. label="NA - Vaccinated during Campaign D(4)";
  attrib H36D_5   format=F00593_. label="NA - Vaccinated during Campaign D(5)";
  attrib H36D_6   format=F00593_. label="NA - Vaccinated during Campaign D(6)";
  attrib H36E_1   format=F00594_. label="NA - Vaccinated during Campaign E(1)";
  attrib H36E_2   format=F00594_. label="NA - Vaccinated during Campaign E(2)";
  attrib H36E_3   format=F00594_. label="NA - Vaccinated during Campaign E(3)";
  attrib H36E_4   format=F00594_. label="NA - Vaccinated during Campaign E(4)";
  attrib H36E_5   format=F00594_. label="NA - Vaccinated during Campaign E(5)";
  attrib H36E_6   format=F00594_. label="NA - Vaccinated during Campaign E(6)";
  attrib H36F_1   format=F00595_. label="NA - Vaccinated during Campaign F(1)";
  attrib H36F_2   format=F00595_. label="NA - Vaccinated during Campaign F(2)";
  attrib H36F_3   format=F00595_. label="NA - Vaccinated during Campaign F(3)";
  attrib H36F_4   format=F00595_. label="NA - Vaccinated during Campaign F(4)";
  attrib H36F_5   format=F00595_. label="NA - Vaccinated during Campaign F(5)";
  attrib H36F_6   format=F00595_. label="NA - Vaccinated during Campaign F(6)";
  attrib H40_1    format=F00596_. label="NA - Received Vitamin A2 (2nd most recent)(1)";
  attrib H40_2    format=F00596_. label="NA - Received Vitamin A2 (2nd most recent)(2)";
  attrib H40_3    format=F00596_. label="NA - Received Vitamin A2 (2nd most recent)(3)";
  attrib H40_4    format=F00596_. label="NA - Received Vitamin A2 (2nd most recent)(4)";
  attrib H40_5    format=F00596_. label="NA - Received Vitamin A2 (2nd most recent)(5)";
  attrib H40_6    format=F00596_. label="NA - Received Vitamin A2 (2nd most recent)(6)";
  attrib H40D_1   format=F00597_. label="NA - Vitamin A2 day(1)";
  attrib H40D_2   format=F00597_. label="NA - Vitamin A2 day(2)";
  attrib H40D_3   format=F00597_. label="NA - Vitamin A2 day(3)";
  attrib H40D_4   format=F00597_. label="NA - Vitamin A2 day(4)";
  attrib H40D_5   format=F00597_. label="NA - Vitamin A2 day(5)";
  attrib H40D_6   format=F00597_. label="NA - Vitamin A2 day(6)";
  attrib H40M_1   format=F00598_. label="NA - Vitamin A2 month(1)";
  attrib H40M_2   format=F00598_. label="NA - Vitamin A2 month(2)";
  attrib H40M_3   format=F00598_. label="NA - Vitamin A2 month(3)";
  attrib H40M_4   format=F00598_. label="NA - Vitamin A2 month(4)";
  attrib H40M_5   format=F00598_. label="NA - Vitamin A2 month(5)";
  attrib H40M_6   format=F00598_. label="NA - Vitamin A2 month(6)";
  attrib H40Y_1   format=F00599_. label="NA - Vitamin A2 year(1)";
  attrib H40Y_2   format=F00599_. label="NA - Vitamin A2 year(2)";
  attrib H40Y_3   format=F00599_. label="NA - Vitamin A2 year(3)";
  attrib H40Y_4   format=F00599_. label="NA - Vitamin A2 year(4)";
  attrib H40Y_5   format=F00599_. label="NA - Vitamin A2 year(5)";
  attrib H40Y_6   format=F00599_. label="NA - Vitamin A2 year(6)";
  attrib H41A_1   format=F00600_. label="NA - Received vitamin A after most recent in card(1)";
  attrib H41A_2   format=F00600_. label="NA - Received vitamin A after most recent in card(2)";
  attrib H41A_3   format=F00600_. label="NA - Received vitamin A after most recent in card(3)";
  attrib H41A_4   format=F00600_. label="NA - Received vitamin A after most recent in card(4)";
  attrib H41A_5   format=F00600_. label="NA - Received vitamin A after most recent in card(5)";
  attrib H41A_6   format=F00600_. label="NA - Received vitamin A after most recent in card(6)";
  attrib H41B_1   format=F00601_. label="NA - Ever received Vitamin A dose(1)";
  attrib H41B_2   format=F00601_. label="NA - Ever received Vitamin A dose(2)";
  attrib H41B_3   format=F00601_. label="NA - Ever received Vitamin A dose(3)";
  attrib H41B_4   format=F00601_. label="NA - Ever received Vitamin A dose(4)";
  attrib H41B_5   format=F00601_. label="NA - Ever received Vitamin A dose(5)";
  attrib H41B_6   format=F00601_. label="NA - Ever received Vitamin A dose(6)";
  attrib H50_1    format=F00602_. label="NA - Received Hepatitis B at birth(1)";
  attrib H50_2    format=F00602_. label="NA - Received Hepatitis B at birth(2)";
  attrib H50_3    format=F00602_. label="NA - Received Hepatitis B at birth(3)";
  attrib H50_4    format=F00602_. label="NA - Received Hepatitis B at birth(4)";
  attrib H50_5    format=F00602_. label="NA - Received Hepatitis B at birth(5)";
  attrib H50_6    format=F00602_. label="NA - Received Hepatitis B at birth(6)";
  attrib H50D_1   format=F00603_. label="NA - Hepatitis B at birth day(1)";
  attrib H50D_2   format=F00603_. label="NA - Hepatitis B at birth day(2)";
  attrib H50D_3   format=F00603_. label="NA - Hepatitis B at birth day(3)";
  attrib H50D_4   format=F00603_. label="NA - Hepatitis B at birth day(4)";
  attrib H50D_5   format=F00603_. label="NA - Hepatitis B at birth day(5)";
  attrib H50D_6   format=F00603_. label="NA - Hepatitis B at birth day(6)";
  attrib H50M_1   format=F00604_. label="NA - Hepatitis B at birth month(1)";
  attrib H50M_2   format=F00604_. label="NA - Hepatitis B at birth month(2)";
  attrib H50M_3   format=F00604_. label="NA - Hepatitis B at birth month(3)";
  attrib H50M_4   format=F00604_. label="NA - Hepatitis B at birth month(4)";
  attrib H50M_5   format=F00604_. label="NA - Hepatitis B at birth month(5)";
  attrib H50M_6   format=F00604_. label="NA - Hepatitis B at birth month(6)";
  attrib H50Y_1   format=F00605_. label="NA - Hepatitis B at birth year(1)";
  attrib H50Y_2   format=F00605_. label="NA - Hepatitis B at birth year(2)";
  attrib H50Y_3   format=F00605_. label="NA - Hepatitis B at birth year(3)";
  attrib H50Y_4   format=F00605_. label="NA - Hepatitis B at birth year(4)";
  attrib H50Y_5   format=F00605_. label="NA - Hepatitis B at birth year(5)";
  attrib H50Y_6   format=F00605_. label="NA - Hepatitis B at birth year(6)";
  attrib H51_1    format=F00606_. label="NA - Received Pentavalent 1(1)";
  attrib H51_2    format=F00606_. label="NA - Received Pentavalent 1(2)";
  attrib H51_3    format=F00606_. label="NA - Received Pentavalent 1(3)";
  attrib H51_4    format=F00606_. label="NA - Received Pentavalent 1(4)";
  attrib H51_5    format=F00606_. label="NA - Received Pentavalent 1(5)";
  attrib H51_6    format=F00606_. label="NA - Received Pentavalent 1(6)";
  attrib H51D_1   format=F00607_. label="NA - Pentavalent 1 day(1)";
  attrib H51D_2   format=F00607_. label="NA - Pentavalent 1 day(2)";
  attrib H51D_3   format=F00607_. label="NA - Pentavalent 1 day(3)";
  attrib H51D_4   format=F00607_. label="NA - Pentavalent 1 day(4)";
  attrib H51D_5   format=F00607_. label="NA - Pentavalent 1 day(5)";
  attrib H51D_6   format=F00607_. label="NA - Pentavalent 1 day(6)";
  attrib H51M_1   format=F00608_. label="NA - Pentavalent 1 month(1)";
  attrib H51M_2   format=F00608_. label="NA - Pentavalent 1 month(2)";
  attrib H51M_3   format=F00608_. label="NA - Pentavalent 1 month(3)";
  attrib H51M_4   format=F00608_. label="NA - Pentavalent 1 month(4)";
  attrib H51M_5   format=F00608_. label="NA - Pentavalent 1 month(5)";
  attrib H51M_6   format=F00608_. label="NA - Pentavalent 1 month(6)";
  attrib H51Y_1   format=F00609_. label="NA - Pentavalent 1 year(1)";
  attrib H51Y_2   format=F00609_. label="NA - Pentavalent 1 year(2)";
  attrib H51Y_3   format=F00609_. label="NA - Pentavalent 1 year(3)";
  attrib H51Y_4   format=F00609_. label="NA - Pentavalent 1 year(4)";
  attrib H51Y_5   format=F00609_. label="NA - Pentavalent 1 year(5)";
  attrib H51Y_6   format=F00609_. label="NA - Pentavalent 1 year(6)";
  attrib H52_1    format=F00610_. label="NA - Received Pentavalent 2(1)";
  attrib H52_2    format=F00610_. label="NA - Received Pentavalent 2(2)";
  attrib H52_3    format=F00610_. label="NA - Received Pentavalent 2(3)";
  attrib H52_4    format=F00610_. label="NA - Received Pentavalent 2(4)";
  attrib H52_5    format=F00610_. label="NA - Received Pentavalent 2(5)";
  attrib H52_6    format=F00610_. label="NA - Received Pentavalent 2(6)";
  attrib H52D_1   format=F00611_. label="NA - Pentavalent 2 day(1)";
  attrib H52D_2   format=F00611_. label="NA - Pentavalent 2 day(2)";
  attrib H52D_3   format=F00611_. label="NA - Pentavalent 2 day(3)";
  attrib H52D_4   format=F00611_. label="NA - Pentavalent 2 day(4)";
  attrib H52D_5   format=F00611_. label="NA - Pentavalent 2 day(5)";
  attrib H52D_6   format=F00611_. label="NA - Pentavalent 2 day(6)";
  attrib H52M_1   format=F00612_. label="NA - Pentavalent 2 month(1)";
  attrib H52M_2   format=F00612_. label="NA - Pentavalent 2 month(2)";
  attrib H52M_3   format=F00612_. label="NA - Pentavalent 2 month(3)";
  attrib H52M_4   format=F00612_. label="NA - Pentavalent 2 month(4)";
  attrib H52M_5   format=F00612_. label="NA - Pentavalent 2 month(5)";
  attrib H52M_6   format=F00612_. label="NA - Pentavalent 2 month(6)";
  attrib H52Y_1   format=F00613_. label="NA - Pentavalent 2 year(1)";
  attrib H52Y_2   format=F00613_. label="NA - Pentavalent 2 year(2)";
  attrib H52Y_3   format=F00613_. label="NA - Pentavalent 2 year(3)";
  attrib H52Y_4   format=F00613_. label="NA - Pentavalent 2 year(4)";
  attrib H52Y_5   format=F00613_. label="NA - Pentavalent 2 year(5)";
  attrib H52Y_6   format=F00613_. label="NA - Pentavalent 2 year(6)";
  attrib H53_1    format=F00614_. label="NA - Received Pentavalent 3(1)";
  attrib H53_2    format=F00614_. label="NA - Received Pentavalent 3(2)";
  attrib H53_3    format=F00614_. label="NA - Received Pentavalent 3(3)";
  attrib H53_4    format=F00614_. label="NA - Received Pentavalent 3(4)";
  attrib H53_5    format=F00614_. label="NA - Received Pentavalent 3(5)";
  attrib H53_6    format=F00614_. label="NA - Received Pentavalent 3(6)";
  attrib H53D_1   format=F00615_. label="NA - Pentavalent 3 day(1)";
  attrib H53D_2   format=F00615_. label="NA - Pentavalent 3 day(2)";
  attrib H53D_3   format=F00615_. label="NA - Pentavalent 3 day(3)";
  attrib H53D_4   format=F00615_. label="NA - Pentavalent 3 day(4)";
  attrib H53D_5   format=F00615_. label="NA - Pentavalent 3 day(5)";
  attrib H53D_6   format=F00615_. label="NA - Pentavalent 3 day(6)";
  attrib H53M_1   format=F00616_. label="NA - Pentavalent 3 month(1)";
  attrib H53M_2   format=F00616_. label="NA - Pentavalent 3 month(2)";
  attrib H53M_3   format=F00616_. label="NA - Pentavalent 3 month(3)";
  attrib H53M_4   format=F00616_. label="NA - Pentavalent 3 month(4)";
  attrib H53M_5   format=F00616_. label="NA - Pentavalent 3 month(5)";
  attrib H53M_6   format=F00616_. label="NA - Pentavalent 3 month(6)";
  attrib H53Y_1   format=F00617_. label="NA - Pentavalent 3 year(1)";
  attrib H53Y_2   format=F00617_. label="NA - Pentavalent 3 year(2)";
  attrib H53Y_3   format=F00617_. label="NA - Pentavalent 3 year(3)";
  attrib H53Y_4   format=F00617_. label="NA - Pentavalent 3 year(4)";
  attrib H53Y_5   format=F00617_. label="NA - Pentavalent 3 year(5)";
  attrib H53Y_6   format=F00617_. label="NA - Pentavalent 3 year(6)";
  attrib H54_1    format=F00618_. label="NA - Received Pneumococcal 1(1)";
  attrib H54_2    format=F00618_. label="NA - Received Pneumococcal 1(2)";
  attrib H54_3    format=F00618_. label="NA - Received Pneumococcal 1(3)";
  attrib H54_4    format=F00618_. label="NA - Received Pneumococcal 1(4)";
  attrib H54_5    format=F00618_. label="NA - Received Pneumococcal 1(5)";
  attrib H54_6    format=F00618_. label="NA - Received Pneumococcal 1(6)";
  attrib H54D_1   format=F00619_. label="NA - Pneumococcal 1 day(1)";
  attrib H54D_2   format=F00619_. label="NA - Pneumococcal 1 day(2)";
  attrib H54D_3   format=F00619_. label="NA - Pneumococcal 1 day(3)";
  attrib H54D_4   format=F00619_. label="NA - Pneumococcal 1 day(4)";
  attrib H54D_5   format=F00619_. label="NA - Pneumococcal 1 day(5)";
  attrib H54D_6   format=F00619_. label="NA - Pneumococcal 1 day(6)";
  attrib H54M_1   format=F00620_. label="NA - Pneumococcal 1 month(1)";
  attrib H54M_2   format=F00620_. label="NA - Pneumococcal 1 month(2)";
  attrib H54M_3   format=F00620_. label="NA - Pneumococcal 1 month(3)";
  attrib H54M_4   format=F00620_. label="NA - Pneumococcal 1 month(4)";
  attrib H54M_5   format=F00620_. label="NA - Pneumococcal 1 month(5)";
  attrib H54M_6   format=F00620_. label="NA - Pneumococcal 1 month(6)";
  attrib H54Y_1   format=F00621_. label="NA - Pneumococcal 1 year(1)";
  attrib H54Y_2   format=F00621_. label="NA - Pneumococcal 1 year(2)";
  attrib H54Y_3   format=F00621_. label="NA - Pneumococcal 1 year(3)";
  attrib H54Y_4   format=F00621_. label="NA - Pneumococcal 1 year(4)";
  attrib H54Y_5   format=F00621_. label="NA - Pneumococcal 1 year(5)";
  attrib H54Y_6   format=F00621_. label="NA - Pneumococcal 1 year(6)";
  attrib H55_1    format=F00622_. label="NA - Received Pneumococcal 2(1)";
  attrib H55_2    format=F00622_. label="NA - Received Pneumococcal 2(2)";
  attrib H55_3    format=F00622_. label="NA - Received Pneumococcal 2(3)";
  attrib H55_4    format=F00622_. label="NA - Received Pneumococcal 2(4)";
  attrib H55_5    format=F00622_. label="NA - Received Pneumococcal 2(5)";
  attrib H55_6    format=F00622_. label="NA - Received Pneumococcal 2(6)";
  attrib H55D_1   format=F00623_. label="NA - Pneumococcal 2 day(1)";
  attrib H55D_2   format=F00623_. label="NA - Pneumococcal 2 day(2)";
  attrib H55D_3   format=F00623_. label="NA - Pneumococcal 2 day(3)";
  attrib H55D_4   format=F00623_. label="NA - Pneumococcal 2 day(4)";
  attrib H55D_5   format=F00623_. label="NA - Pneumococcal 2 day(5)";
  attrib H55D_6   format=F00623_. label="NA - Pneumococcal 2 day(6)";
  attrib H55M_1   format=F00624_. label="NA - Pneumococcal 2 month(1)";
  attrib H55M_2   format=F00624_. label="NA - Pneumococcal 2 month(2)";
  attrib H55M_3   format=F00624_. label="NA - Pneumococcal 2 month(3)";
  attrib H55M_4   format=F00624_. label="NA - Pneumococcal 2 month(4)";
  attrib H55M_5   format=F00624_. label="NA - Pneumococcal 2 month(5)";
  attrib H55M_6   format=F00624_. label="NA - Pneumococcal 2 month(6)";
  attrib H55Y_1   format=F00625_. label="NA - Pneumococcal 2 year(1)";
  attrib H55Y_2   format=F00625_. label="NA - Pneumococcal 2 year(2)";
  attrib H55Y_3   format=F00625_. label="NA - Pneumococcal 2 year(3)";
  attrib H55Y_4   format=F00625_. label="NA - Pneumococcal 2 year(4)";
  attrib H55Y_5   format=F00625_. label="NA - Pneumococcal 2 year(5)";
  attrib H55Y_6   format=F00625_. label="NA - Pneumococcal 2 year(6)";
  attrib H56_1    format=F00626_. label="NA - Received Pneumococcal 3(1)";
  attrib H56_2    format=F00626_. label="NA - Received Pneumococcal 3(2)";
  attrib H56_3    format=F00626_. label="NA - Received Pneumococcal 3(3)";
  attrib H56_4    format=F00626_. label="NA - Received Pneumococcal 3(4)";
  attrib H56_5    format=F00626_. label="NA - Received Pneumococcal 3(5)";
  attrib H56_6    format=F00626_. label="NA - Received Pneumococcal 3(6)";
  attrib H56D_1   format=F00627_. label="NA - Pneumococcal 3 day(1)";
  attrib H56D_2   format=F00627_. label="NA - Pneumococcal 3 day(2)";
  attrib H56D_3   format=F00627_. label="NA - Pneumococcal 3 day(3)";
  attrib H56D_4   format=F00627_. label="NA - Pneumococcal 3 day(4)";
  attrib H56D_5   format=F00627_. label="NA - Pneumococcal 3 day(5)";
  attrib H56D_6   format=F00627_. label="NA - Pneumococcal 3 day(6)";
  attrib H56M_1   format=F00628_. label="NA - Pneumococcal 3 month(1)";
  attrib H56M_2   format=F00628_. label="NA - Pneumococcal 3 month(2)";
  attrib H56M_3   format=F00628_. label="NA - Pneumococcal 3 month(3)";
  attrib H56M_4   format=F00628_. label="NA - Pneumococcal 3 month(4)";
  attrib H56M_5   format=F00628_. label="NA - Pneumococcal 3 month(5)";
  attrib H56M_6   format=F00628_. label="NA - Pneumococcal 3 month(6)";
  attrib H56Y_1   format=F00629_. label="NA - Pneumococcal 3 year(1)";
  attrib H56Y_2   format=F00629_. label="NA - Pneumococcal 3 year(2)";
  attrib H56Y_3   format=F00629_. label="NA - Pneumococcal 3 year(3)";
  attrib H56Y_4   format=F00629_. label="NA - Pneumococcal 3 year(4)";
  attrib H56Y_5   format=F00629_. label="NA - Pneumococcal 3 year(5)";
  attrib H56Y_6   format=F00629_. label="NA - Pneumococcal 3 year(6)";
  attrib H57_1    format=F00630_. label="NA - Received Rotavirus 1(1)";
  attrib H57_2    format=F00630_. label="NA - Received Rotavirus 1(2)";
  attrib H57_3    format=F00630_. label="NA - Received Rotavirus 1(3)";
  attrib H57_4    format=F00630_. label="NA - Received Rotavirus 1(4)";
  attrib H57_5    format=F00630_. label="NA - Received Rotavirus 1(5)";
  attrib H57_6    format=F00630_. label="NA - Received Rotavirus 1(6)";
  attrib H57D_1   format=F00631_. label="NA - Rotavirus 1 day(1)";
  attrib H57D_2   format=F00631_. label="NA - Rotavirus 1 day(2)";
  attrib H57D_3   format=F00631_. label="NA - Rotavirus 1 day(3)";
  attrib H57D_4   format=F00631_. label="NA - Rotavirus 1 day(4)";
  attrib H57D_5   format=F00631_. label="NA - Rotavirus 1 day(5)";
  attrib H57D_6   format=F00631_. label="NA - Rotavirus 1 day(6)";
  attrib H57M_1   format=F00632_. label="NA - Rotavirus 1 month(1)";
  attrib H57M_2   format=F00632_. label="NA - Rotavirus 1 month(2)";
  attrib H57M_3   format=F00632_. label="NA - Rotavirus 1 month(3)";
  attrib H57M_4   format=F00632_. label="NA - Rotavirus 1 month(4)";
  attrib H57M_5   format=F00632_. label="NA - Rotavirus 1 month(5)";
  attrib H57M_6   format=F00632_. label="NA - Rotavirus 1 month(6)";
  attrib H57Y_1   format=F00633_. label="NA - Rotavirus 1 year(1)";
  attrib H57Y_2   format=F00633_. label="NA - Rotavirus 1 year(2)";
  attrib H57Y_3   format=F00633_. label="NA - Rotavirus 1 year(3)";
  attrib H57Y_4   format=F00633_. label="NA - Rotavirus 1 year(4)";
  attrib H57Y_5   format=F00633_. label="NA - Rotavirus 1 year(5)";
  attrib H57Y_6   format=F00633_. label="NA - Rotavirus 1 year(6)";
  attrib H58_1    format=F00634_. label="NA - Received Rotavirus 2(1)";
  attrib H58_2    format=F00634_. label="NA - Received Rotavirus 2(2)";
  attrib H58_3    format=F00634_. label="NA - Received Rotavirus 2(3)";
  attrib H58_4    format=F00634_. label="NA - Received Rotavirus 2(4)";
  attrib H58_5    format=F00634_. label="NA - Received Rotavirus 2(5)";
  attrib H58_6    format=F00634_. label="NA - Received Rotavirus 2(6)";
  attrib H58D_1   format=F00635_. label="NA - Rotavirus 2 day(1)";
  attrib H58D_2   format=F00635_. label="NA - Rotavirus 2 day(2)";
  attrib H58D_3   format=F00635_. label="NA - Rotavirus 2 day(3)";
  attrib H58D_4   format=F00635_. label="NA - Rotavirus 2 day(4)";
  attrib H58D_5   format=F00635_. label="NA - Rotavirus 2 day(5)";
  attrib H58D_6   format=F00635_. label="NA - Rotavirus 2 day(6)";
  attrib H58M_1   format=F00636_. label="NA - Rotavirus 2 month(1)";
  attrib H58M_2   format=F00636_. label="NA - Rotavirus 2 month(2)";
  attrib H58M_3   format=F00636_. label="NA - Rotavirus 2 month(3)";
  attrib H58M_4   format=F00636_. label="NA - Rotavirus 2 month(4)";
  attrib H58M_5   format=F00636_. label="NA - Rotavirus 2 month(5)";
  attrib H58M_6   format=F00636_. label="NA - Rotavirus 2 month(6)";
  attrib H58Y_1   format=F00637_. label="NA - Rotavirus 2 year(1)";
  attrib H58Y_2   format=F00637_. label="NA - Rotavirus 2 year(2)";
  attrib H58Y_3   format=F00637_. label="NA - Rotavirus 2 year(3)";
  attrib H58Y_4   format=F00637_. label="NA - Rotavirus 2 year(4)";
  attrib H58Y_5   format=F00637_. label="NA - Rotavirus 2 year(5)";
  attrib H58Y_6   format=F00637_. label="NA - Rotavirus 2 year(6)";
  attrib H59_1    format=F00638_. label="NA - Received Rotavirus 3(1)";
  attrib H59_2    format=F00638_. label="NA - Received Rotavirus 3(2)";
  attrib H59_3    format=F00638_. label="NA - Received Rotavirus 3(3)";
  attrib H59_4    format=F00638_. label="NA - Received Rotavirus 3(4)";
  attrib H59_5    format=F00638_. label="NA - Received Rotavirus 3(5)";
  attrib H59_6    format=F00638_. label="NA - Received Rotavirus 3(6)";
  attrib H59D_1   format=F00639_. label="NA - Rotavirus 3 day(1)";
  attrib H59D_2   format=F00639_. label="NA - Rotavirus 3 day(2)";
  attrib H59D_3   format=F00639_. label="NA - Rotavirus 3 day(3)";
  attrib H59D_4   format=F00639_. label="NA - Rotavirus 3 day(4)";
  attrib H59D_5   format=F00639_. label="NA - Rotavirus 3 day(5)";
  attrib H59D_6   format=F00639_. label="NA - Rotavirus 3 day(6)";
  attrib H59M_1   format=F00640_. label="NA - Rotavirus 3 month(1)";
  attrib H59M_2   format=F00640_. label="NA - Rotavirus 3 month(2)";
  attrib H59M_3   format=F00640_. label="NA - Rotavirus 3 month(3)";
  attrib H59M_4   format=F00640_. label="NA - Rotavirus 3 month(4)";
  attrib H59M_5   format=F00640_. label="NA - Rotavirus 3 month(5)";
  attrib H59M_6   format=F00640_. label="NA - Rotavirus 3 month(6)";
  attrib H59Y_1   format=F00641_. label="NA - Rotavirus 3 year(1)";
  attrib H59Y_2   format=F00641_. label="NA - Rotavirus 3 year(2)";
  attrib H59Y_3   format=F00641_. label="NA - Rotavirus 3 year(3)";
  attrib H59Y_4   format=F00641_. label="NA - Rotavirus 3 year(4)";
  attrib H59Y_5   format=F00641_. label="NA - Rotavirus 3 year(5)";
  attrib H59Y_6   format=F00641_. label="NA - Rotavirus 3 year(6)";
  attrib H60_1    format=F00642_. label="NA - Received Polio inactive(1)";
  attrib H60_2    format=F00642_. label="NA - Received Polio inactive(2)";
  attrib H60_3    format=F00642_. label="NA - Received Polio inactive(3)";
  attrib H60_4    format=F00642_. label="NA - Received Polio inactive(4)";
  attrib H60_5    format=F00642_. label="NA - Received Polio inactive(5)";
  attrib H60_6    format=F00642_. label="NA - Received Polio inactive(6)";
  attrib H60D_1   format=F00643_. label="NA - Polio inactive day(1)";
  attrib H60D_2   format=F00643_. label="NA - Polio inactive day(2)";
  attrib H60D_3   format=F00643_. label="NA - Polio inactive day(3)";
  attrib H60D_4   format=F00643_. label="NA - Polio inactive day(4)";
  attrib H60D_5   format=F00643_. label="NA - Polio inactive day(5)";
  attrib H60D_6   format=F00643_. label="NA - Polio inactive day(6)";
  attrib H60M_1   format=F00644_. label="NA - Polio inactive month(1)";
  attrib H60M_2   format=F00644_. label="NA - Polio inactive month(2)";
  attrib H60M_3   format=F00644_. label="NA - Polio inactive month(3)";
  attrib H60M_4   format=F00644_. label="NA - Polio inactive month(4)";
  attrib H60M_5   format=F00644_. label="NA - Polio inactive month(5)";
  attrib H60M_6   format=F00644_. label="NA - Polio inactive month(6)";
  attrib H60Y_1   format=F00645_. label="NA - Polio inactive year(1)";
  attrib H60Y_2   format=F00645_. label="NA - Polio inactive year(2)";
  attrib H60Y_3   format=F00645_. label="NA - Polio inactive year(3)";
  attrib H60Y_4   format=F00645_. label="NA - Polio inactive year(4)";
  attrib H60Y_5   format=F00645_. label="NA - Polio inactive year(5)";
  attrib H60Y_6   format=F00645_. label="NA - Polio inactive year(6)";
  attrib H61_1    format=F00646_. label="NA - Received Hepatitis B 1(1)";
  attrib H61_2    format=F00646_. label="NA - Received Hepatitis B 1(2)";
  attrib H61_3    format=F00646_. label="NA - Received Hepatitis B 1(3)";
  attrib H61_4    format=F00646_. label="NA - Received Hepatitis B 1(4)";
  attrib H61_5    format=F00646_. label="NA - Received Hepatitis B 1(5)";
  attrib H61_6    format=F00646_. label="NA - Received Hepatitis B 1(6)";
  attrib H61D_1   format=F00647_. label="NA - Hepatitis B 1 day(1)";
  attrib H61D_2   format=F00647_. label="NA - Hepatitis B 1 day(2)";
  attrib H61D_3   format=F00647_. label="NA - Hepatitis B 1 day(3)";
  attrib H61D_4   format=F00647_. label="NA - Hepatitis B 1 day(4)";
  attrib H61D_5   format=F00647_. label="NA - Hepatitis B 1 day(5)";
  attrib H61D_6   format=F00647_. label="NA - Hepatitis B 1 day(6)";
  attrib H61M_1   format=F00648_. label="NA - Hepatitis B 1 month(1)";
  attrib H61M_2   format=F00648_. label="NA - Hepatitis B 1 month(2)";
  attrib H61M_3   format=F00648_. label="NA - Hepatitis B 1 month(3)";
  attrib H61M_4   format=F00648_. label="NA - Hepatitis B 1 month(4)";
  attrib H61M_5   format=F00648_. label="NA - Hepatitis B 1 month(5)";
  attrib H61M_6   format=F00648_. label="NA - Hepatitis B 1 month(6)";
  attrib H61Y_1   format=F00649_. label="NA - Hepatitis B 1 year(1)";
  attrib H61Y_2   format=F00649_. label="NA - Hepatitis B 1 year(2)";
  attrib H61Y_3   format=F00649_. label="NA - Hepatitis B 1 year(3)";
  attrib H61Y_4   format=F00649_. label="NA - Hepatitis B 1 year(4)";
  attrib H61Y_5   format=F00649_. label="NA - Hepatitis B 1 year(5)";
  attrib H61Y_6   format=F00649_. label="NA - Hepatitis B 1 year(6)";
  attrib H62_1    format=F00650_. label="NA - Received Hepatitis B 2(1)";
  attrib H62_2    format=F00650_. label="NA - Received Hepatitis B 2(2)";
  attrib H62_3    format=F00650_. label="NA - Received Hepatitis B 2(3)";
  attrib H62_4    format=F00650_. label="NA - Received Hepatitis B 2(4)";
  attrib H62_5    format=F00650_. label="NA - Received Hepatitis B 2(5)";
  attrib H62_6    format=F00650_. label="NA - Received Hepatitis B 2(6)";
  attrib H62D_1   format=F00651_. label="NA - Hepatitis B 2 day(1)";
  attrib H62D_2   format=F00651_. label="NA - Hepatitis B 2 day(2)";
  attrib H62D_3   format=F00651_. label="NA - Hepatitis B 2 day(3)";
  attrib H62D_4   format=F00651_. label="NA - Hepatitis B 2 day(4)";
  attrib H62D_5   format=F00651_. label="NA - Hepatitis B 2 day(5)";
  attrib H62D_6   format=F00651_. label="NA - Hepatitis B 2 day(6)";
  attrib H62M_1   format=F00652_. label="NA - Hepatitis B 2 month(1)";
  attrib H62M_2   format=F00652_. label="NA - Hepatitis B 2 month(2)";
  attrib H62M_3   format=F00652_. label="NA - Hepatitis B 2 month(3)";
  attrib H62M_4   format=F00652_. label="NA - Hepatitis B 2 month(4)";
  attrib H62M_5   format=F00652_. label="NA - Hepatitis B 2 month(5)";
  attrib H62M_6   format=F00652_. label="NA - Hepatitis B 2 month(6)";
  attrib H62Y_1   format=F00653_. label="NA - Hepatitis B 2 year(1)";
  attrib H62Y_2   format=F00653_. label="NA - Hepatitis B 2 year(2)";
  attrib H62Y_3   format=F00653_. label="NA - Hepatitis B 2 year(3)";
  attrib H62Y_4   format=F00653_. label="NA - Hepatitis B 2 year(4)";
  attrib H62Y_5   format=F00653_. label="NA - Hepatitis B 2 year(5)";
  attrib H62Y_6   format=F00653_. label="NA - Hepatitis B 2 year(6)";
  attrib H63_1    format=F00654_. label="NA - Received Hepatitis B 3(1)";
  attrib H63_2    format=F00654_. label="NA - Received Hepatitis B 3(2)";
  attrib H63_3    format=F00654_. label="NA - Received Hepatitis B 3(3)";
  attrib H63_4    format=F00654_. label="NA - Received Hepatitis B 3(4)";
  attrib H63_5    format=F00654_. label="NA - Received Hepatitis B 3(5)";
  attrib H63_6    format=F00654_. label="NA - Received Hepatitis B 3(6)";
  attrib H63D_1   format=F00655_. label="NA - Hepatitis B 3 day(1)";
  attrib H63D_2   format=F00655_. label="NA - Hepatitis B 3 day(2)";
  attrib H63D_3   format=F00655_. label="NA - Hepatitis B 3 day(3)";
  attrib H63D_4   format=F00655_. label="NA - Hepatitis B 3 day(4)";
  attrib H63D_5   format=F00655_. label="NA - Hepatitis B 3 day(5)";
  attrib H63D_6   format=F00655_. label="NA - Hepatitis B 3 day(6)";
  attrib H63M_1   format=F00656_. label="NA - Hepatitis B 3 month(1)";
  attrib H63M_2   format=F00656_. label="NA - Hepatitis B 3 month(2)";
  attrib H63M_3   format=F00656_. label="NA - Hepatitis B 3 month(3)";
  attrib H63M_4   format=F00656_. label="NA - Hepatitis B 3 month(4)";
  attrib H63M_5   format=F00656_. label="NA - Hepatitis B 3 month(5)";
  attrib H63M_6   format=F00656_. label="NA - Hepatitis B 3 month(6)";
  attrib H63Y_1   format=F00657_. label="NA - Hepatitis B 3 year(1)";
  attrib H63Y_2   format=F00657_. label="NA - Hepatitis B 3 year(2)";
  attrib H63Y_3   format=F00657_. label="NA - Hepatitis B 3 year(3)";
  attrib H63Y_4   format=F00657_. label="NA - Hepatitis B 3 year(4)";
  attrib H63Y_5   format=F00657_. label="NA - Hepatitis B 3 year(5)";
  attrib H63Y_6   format=F00657_. label="NA - Hepatitis B 3 year(6)";
  attrib H64_1    format=F00658_. label="NA - Received HiB 1(1)";
  attrib H64_2    format=F00658_. label="NA - Received HiB 1(2)";
  attrib H64_3    format=F00658_. label="NA - Received HiB 1(3)";
  attrib H64_4    format=F00658_. label="NA - Received HiB 1(4)";
  attrib H64_5    format=F00658_. label="NA - Received HiB 1(5)";
  attrib H64_6    format=F00658_. label="NA - Received HiB 1(6)";
  attrib H64D_1   format=F00659_. label="NA - HiB 1 day(1)";
  attrib H64D_2   format=F00659_. label="NA - HiB 1 day(2)";
  attrib H64D_3   format=F00659_. label="NA - HiB 1 day(3)";
  attrib H64D_4   format=F00659_. label="NA - HiB 1 day(4)";
  attrib H64D_5   format=F00659_. label="NA - HiB 1 day(5)";
  attrib H64D_6   format=F00659_. label="NA - HiB 1 day(6)";
  attrib H64M_1   format=F00660_. label="NA - HiB 1 month(1)";
  attrib H64M_2   format=F00660_. label="NA - HiB 1 month(2)";
  attrib H64M_3   format=F00660_. label="NA - HiB 1 month(3)";
  attrib H64M_4   format=F00660_. label="NA - HiB 1 month(4)";
  attrib H64M_5   format=F00660_. label="NA - HiB 1 month(5)";
  attrib H64M_6   format=F00660_. label="NA - HiB 1 month(6)";
  attrib H64Y_1   format=F00661_. label="NA - HiB 1 year(1)";
  attrib H64Y_2   format=F00661_. label="NA - HiB 1 year(2)";
  attrib H64Y_3   format=F00661_. label="NA - HiB 1 year(3)";
  attrib H64Y_4   format=F00661_. label="NA - HiB 1 year(4)";
  attrib H64Y_5   format=F00661_. label="NA - HiB 1 year(5)";
  attrib H64Y_6   format=F00661_. label="NA - HiB 1 year(6)";
  attrib H65_1    format=F00662_. label="NA - Received HiB 2(1)";
  attrib H65_2    format=F00662_. label="NA - Received HiB 2(2)";
  attrib H65_3    format=F00662_. label="NA - Received HiB 2(3)";
  attrib H65_4    format=F00662_. label="NA - Received HiB 2(4)";
  attrib H65_5    format=F00662_. label="NA - Received HiB 2(5)";
  attrib H65_6    format=F00662_. label="NA - Received HiB 2(6)";
  attrib H65D_1   format=F00663_. label="NA - HiB 2 day(1)";
  attrib H65D_2   format=F00663_. label="NA - HiB 2 day(2)";
  attrib H65D_3   format=F00663_. label="NA - HiB 2 day(3)";
  attrib H65D_4   format=F00663_. label="NA - HiB 2 day(4)";
  attrib H65D_5   format=F00663_. label="NA - HiB 2 day(5)";
  attrib H65D_6   format=F00663_. label="NA - HiB 2 day(6)";
  attrib H65M_1   format=F00664_. label="NA - HiB 2 month(1)";
  attrib H65M_2   format=F00664_. label="NA - HiB 2 month(2)";
  attrib H65M_3   format=F00664_. label="NA - HiB 2 month(3)";
  attrib H65M_4   format=F00664_. label="NA - HiB 2 month(4)";
  attrib H65M_5   format=F00664_. label="NA - HiB 2 month(5)";
  attrib H65M_6   format=F00664_. label="NA - HiB 2 month(6)";
  attrib H65Y_1   format=F00665_. label="NA - HiB 2 year(1)";
  attrib H65Y_2   format=F00665_. label="NA - HiB 2 year(2)";
  attrib H65Y_3   format=F00665_. label="NA - HiB 2 year(3)";
  attrib H65Y_4   format=F00665_. label="NA - HiB 2 year(4)";
  attrib H65Y_5   format=F00665_. label="NA - HiB 2 year(5)";
  attrib H65Y_6   format=F00665_. label="NA - HiB 2 year(6)";
  attrib H66_1    format=F00666_. label="NA - Received HiB 3(1)";
  attrib H66_2    format=F00666_. label="NA - Received HiB 3(2)";
  attrib H66_3    format=F00666_. label="NA - Received HiB 3(3)";
  attrib H66_4    format=F00666_. label="NA - Received HiB 3(4)";
  attrib H66_5    format=F00666_. label="NA - Received HiB 3(5)";
  attrib H66_6    format=F00666_. label="NA - Received HiB 3(6)";
  attrib H66D_1   format=F00667_. label="NA - HiB 3 day(1)";
  attrib H66D_2   format=F00667_. label="NA - HiB 3 day(2)";
  attrib H66D_3   format=F00667_. label="NA - HiB 3 day(3)";
  attrib H66D_4   format=F00667_. label="NA - HiB 3 day(4)";
  attrib H66D_5   format=F00667_. label="NA - HiB 3 day(5)";
  attrib H66D_6   format=F00667_. label="NA - HiB 3 day(6)";
  attrib H66M_1   format=F00668_. label="NA - HiB 3 month(1)";
  attrib H66M_2   format=F00668_. label="NA - HiB 3 month(2)";
  attrib H66M_3   format=F00668_. label="NA - HiB 3 month(3)";
  attrib H66M_4   format=F00668_. label="NA - HiB 3 month(4)";
  attrib H66M_5   format=F00668_. label="NA - HiB 3 month(5)";
  attrib H66M_6   format=F00668_. label="NA - HiB 3 month(6)";
  attrib H66Y_1   format=F00669_. label="NA - HiB 3 year(1)";
  attrib H66Y_2   format=F00669_. label="NA - HiB 3 year(2)";
  attrib H66Y_3   format=F00669_. label="NA - HiB 3 year(3)";
  attrib H66Y_4   format=F00669_. label="NA - HiB 3 year(4)";
  attrib H66Y_5   format=F00669_. label="NA - HiB 3 year(5)";
  attrib H66Y_6   format=F00669_. label="NA - HiB 3 year(6)";
  attrib H80A_1   format=F00670_. label="NA - In the last 7 days given: LOCAL NAME for multiple micronutrient powder(1)";
  attrib H80A_2   format=F00670_. label="NA - In the last 7 days given: LOCAL NAME for multiple micronutrient powder(2)";
  attrib H80A_3   format=F00670_. label="NA - In the last 7 days given: LOCAL NAME for multiple micronutrient powder(3)";
  attrib H80A_4   format=F00670_. label="NA - In the last 7 days given: LOCAL NAME for multiple micronutrient powder(4)";
  attrib H80A_5   format=F00670_. label="NA - In the last 7 days given: LOCAL NAME for multiple micronutrient powder(5)";
  attrib H80A_6   format=F00670_. label="NA - In the last 7 days given: LOCAL NAME for multiple micronutrient powder(6)";
  attrib H80B_1   format=F00671_. label="NA - In the last 7 days given: LOCAL NAME ready to use therapeutic food(1)";
  attrib H80B_2   format=F00671_. label="NA - In the last 7 days given: LOCAL NAME ready to use therapeutic food(2)";
  attrib H80B_3   format=F00671_. label="NA - In the last 7 days given: LOCAL NAME ready to use therapeutic food(3)";
  attrib H80B_4   format=F00671_. label="NA - In the last 7 days given: LOCAL NAME ready to use therapeutic food(4)";
  attrib H80B_5   format=F00671_. label="NA - In the last 7 days given: LOCAL NAME ready to use therapeutic food(5)";
  attrib H80B_6   format=F00671_. label="NA - In the last 7 days given: LOCAL NAME ready to use therapeutic food(6)";
  attrib H80C_1   format=F00672_. label="NA - In the last 7 days given: LOCAL NAME for ready to use supplemental food(1)";
  attrib H80C_2   format=F00672_. label="NA - In the last 7 days given: LOCAL NAME for ready to use supplemental food(2)";
  attrib H80C_3   format=F00672_. label="NA - In the last 7 days given: LOCAL NAME for ready to use supplemental food(3)";
  attrib H80C_4   format=F00672_. label="NA - In the last 7 days given: LOCAL NAME for ready to use supplemental food(4)";
  attrib H80C_5   format=F00672_. label="NA - In the last 7 days given: LOCAL NAME for ready to use supplemental food(5)";
  attrib H80C_6   format=F00672_. label="NA - In the last 7 days given: LOCAL NAME for ready to use supplemental food(6)";
  attrib H80D_1   format=F00673_. label="NA - In the last 7 days given: CS(1)";
  attrib H80D_2   format=F00673_. label="NA - In the last 7 days given: CS(2)";
  attrib H80D_3   format=F00673_. label="NA - In the last 7 days given: CS(3)";
  attrib H80D_4   format=F00673_. label="NA - In the last 7 days given: CS(4)";
  attrib H80D_5   format=F00673_. label="NA - In the last 7 days given: CS(5)";
  attrib H80D_6   format=F00673_. label="NA - In the last 7 days given: CS(6)";
  attrib H80E_1   format=F00674_. label="NA - In the last 7 days given: CS(1)";
  attrib H80E_2   format=F00674_. label="NA - In the last 7 days given: CS(2)";
  attrib H80E_3   format=F00674_. label="NA - In the last 7 days given: CS(3)";
  attrib H80E_4   format=F00674_. label="NA - In the last 7 days given: CS(4)";
  attrib H80E_5   format=F00674_. label="NA - In the last 7 days given: CS(5)";
  attrib H80E_6   format=F00674_. label="NA - In the last 7 days given: CS(6)";
  attrib H80F_1   format=F00675_. label="NA - In the last 7 days given: CS(1)";
  attrib H80F_2   format=F00675_. label="NA - In the last 7 days given: CS(2)";
  attrib H80F_3   format=F00675_. label="NA - In the last 7 days given: CS(3)";
  attrib H80F_4   format=F00675_. label="NA - In the last 7 days given: CS(4)";
  attrib H80F_5   format=F00675_. label="NA - In the last 7 days given: CS(5)";
  attrib H80F_6   format=F00675_. label="NA - In the last 7 days given: CS(6)";
  attrib H80G_1   format=F00676_. label="NA - In the last 7 days given: CS(1)";
  attrib H80G_2   format=F00676_. label="NA - In the last 7 days given: CS(2)";
  attrib H80G_3   format=F00676_. label="NA - In the last 7 days given: CS(3)";
  attrib H80G_4   format=F00676_. label="NA - In the last 7 days given: CS(4)";
  attrib H80G_5   format=F00676_. label="NA - In the last 7 days given: CS(5)";
  attrib H80G_6   format=F00676_. label="NA - In the last 7 days given: CS(6)";
  attrib HIDXA_1  label="NA - Index to birth history(1)";
  attrib HIDXA_2  label="NA - Index to birth history(2)";
  attrib HIDXA_3  label="NA - Index to birth history(3)";
  attrib HIDXA_4  label="NA - Index to birth history(4)";
  attrib HIDXA_5  label="NA - Index to birth history(5)";
  attrib HIDXA_6  label="NA - Index to birth history(6)";
  attrib H11_1    format=F00677_. label="NA - Had diarrhea recently(1)";
  attrib H11_2    format=F00677_. label="NA - Had diarrhea recently(2)";
  attrib H11_3    format=F00677_. label="NA - Had diarrhea recently(3)";
  attrib H11_4    format=F00677_. label="NA - Had diarrhea recently(4)";
  attrib H11_5    format=F00677_. label="NA - Had diarrhea recently(5)";
  attrib H11_6    format=F00677_. label="NA - Had diarrhea recently(6)";
  attrib H11B_1   format=F00678_. label="NA - Blood in the stools(1)";
  attrib H11B_2   format=F00678_. label="NA - Blood in the stools(2)";
  attrib H11B_3   format=F00678_. label="NA - Blood in the stools(3)";
  attrib H11B_4   format=F00678_. label="NA - Blood in the stools(4)";
  attrib H11B_5   format=F00678_. label="NA - Blood in the stools(5)";
  attrib H11B_6   format=F00678_. label="NA - Blood in the stools(6)";
  attrib H12A_1   format=F00679_. label="NA - Diarrhea: government hospital(1)";
  attrib H12A_2   format=F00679_. label="NA - Diarrhea: government hospital(2)";
  attrib H12A_3   format=F00679_. label="NA - Diarrhea: government hospital(3)";
  attrib H12A_4   format=F00679_. label="NA - Diarrhea: government hospital(4)";
  attrib H12A_5   format=F00679_. label="NA - Diarrhea: government hospital(5)";
  attrib H12A_6   format=F00679_. label="NA - Diarrhea: government hospital(6)";
  attrib H12B_1   format=F00680_. label="NA - Diarrhea: CS public sector(1)";
  attrib H12B_2   format=F00680_. label="NA - Diarrhea: CS public sector(2)";
  attrib H12B_3   format=F00680_. label="NA - Diarrhea: CS public sector(3)";
  attrib H12B_4   format=F00680_. label="NA - Diarrhea: CS public sector(4)";
  attrib H12B_5   format=F00680_. label="NA - Diarrhea: CS public sector(5)";
  attrib H12B_6   format=F00680_. label="NA - Diarrhea: CS public sector(6)";
  attrib H12C_1   format=F00681_. label="NA - Diarrhea: CS public sector(1)";
  attrib H12C_2   format=F00681_. label="NA - Diarrhea: CS public sector(2)";
  attrib H12C_3   format=F00681_. label="NA - Diarrhea: CS public sector(3)";
  attrib H12C_4   format=F00681_. label="NA - Diarrhea: CS public sector(4)";
  attrib H12C_5   format=F00681_. label="NA - Diarrhea: CS public sector(5)";
  attrib H12C_6   format=F00681_. label="NA - Diarrhea: CS public sector(6)";
  attrib H12D_1   format=F00682_. label="NA - Diarrhea: CS public sector(1)";
  attrib H12D_2   format=F00682_. label="NA - Diarrhea: CS public sector(2)";
  attrib H12D_3   format=F00682_. label="NA - Diarrhea: CS public sector(3)";
  attrib H12D_4   format=F00682_. label="NA - Diarrhea: CS public sector(4)";
  attrib H12D_5   format=F00682_. label="NA - Diarrhea: CS public sector(5)";
  attrib H12D_6   format=F00682_. label="NA - Diarrhea: CS public sector(6)";
  attrib H12E_1   format=F00683_. label="NA - Diarrhea: CS public sector(1)";
  attrib H12E_2   format=F00683_. label="NA - Diarrhea: CS public sector(2)";
  attrib H12E_3   format=F00683_. label="NA - Diarrhea: CS public sector(3)";
  attrib H12E_4   format=F00683_. label="NA - Diarrhea: CS public sector(4)";
  attrib H12E_5   format=F00683_. label="NA - Diarrhea: CS public sector(5)";
  attrib H12E_6   format=F00683_. label="NA - Diarrhea: CS public sector(6)";
  attrib H12F_1   format=F00684_. label="NA - Diarrhea: CS public sector(1)";
  attrib H12F_2   format=F00684_. label="NA - Diarrhea: CS public sector(2)";
  attrib H12F_3   format=F00684_. label="NA - Diarrhea: CS public sector(3)";
  attrib H12F_4   format=F00684_. label="NA - Diarrhea: CS public sector(4)";
  attrib H12F_5   format=F00684_. label="NA - Diarrhea: CS public sector(5)";
  attrib H12F_6   format=F00684_. label="NA - Diarrhea: CS public sector(6)";
  attrib H12G_1   format=F00685_. label="NA - Diarrhea: CS public sector(1)";
  attrib H12G_2   format=F00685_. label="NA - Diarrhea: CS public sector(2)";
  attrib H12G_3   format=F00685_. label="NA - Diarrhea: CS public sector(3)";
  attrib H12G_4   format=F00685_. label="NA - Diarrhea: CS public sector(4)";
  attrib H12G_5   format=F00685_. label="NA - Diarrhea: CS public sector(5)";
  attrib H12G_6   format=F00685_. label="NA - Diarrhea: CS public sector(6)";
  attrib H12H_1   format=F00686_. label="NA - Diarrhea: CS public sector(1)";
  attrib H12H_2   format=F00686_. label="NA - Diarrhea: CS public sector(2)";
  attrib H12H_3   format=F00686_. label="NA - Diarrhea: CS public sector(3)";
  attrib H12H_4   format=F00686_. label="NA - Diarrhea: CS public sector(4)";
  attrib H12H_5   format=F00686_. label="NA - Diarrhea: CS public sector(5)";
  attrib H12H_6   format=F00686_. label="NA - Diarrhea: CS public sector(6)";
  attrib H12I_1   format=F00687_. label="NA - Diarrhea: CS public sector(1)";
  attrib H12I_2   format=F00687_. label="NA - Diarrhea: CS public sector(2)";
  attrib H12I_3   format=F00687_. label="NA - Diarrhea: CS public sector(3)";
  attrib H12I_4   format=F00687_. label="NA - Diarrhea: CS public sector(4)";
  attrib H12I_5   format=F00687_. label="NA - Diarrhea: CS public sector(5)";
  attrib H12I_6   format=F00687_. label="NA - Diarrhea: CS public sector(6)";
  attrib H12J_1   format=F00688_. label="NA - Diarrhea: private hospital/clinic(1)";
  attrib H12J_2   format=F00688_. label="NA - Diarrhea: private hospital/clinic(2)";
  attrib H12J_3   format=F00688_. label="NA - Diarrhea: private hospital/clinic(3)";
  attrib H12J_4   format=F00688_. label="NA - Diarrhea: private hospital/clinic(4)";
  attrib H12J_5   format=F00688_. label="NA - Diarrhea: private hospital/clinic(5)";
  attrib H12J_6   format=F00688_. label="NA - Diarrhea: private hospital/clinic(6)";
  attrib H12K_1   format=F00689_. label="NA - Diarrhea: private pharmacy(1)";
  attrib H12K_2   format=F00689_. label="NA - Diarrhea: private pharmacy(2)";
  attrib H12K_3   format=F00689_. label="NA - Diarrhea: private pharmacy(3)";
  attrib H12K_4   format=F00689_. label="NA - Diarrhea: private pharmacy(4)";
  attrib H12K_5   format=F00689_. label="NA - Diarrhea: private pharmacy(5)";
  attrib H12K_6   format=F00689_. label="NA - Diarrhea: private pharmacy(6)";
  attrib H12L_1   format=F00690_. label="NA - Diarrhea: private doctor(1)";
  attrib H12L_2   format=F00690_. label="NA - Diarrhea: private doctor(2)";
  attrib H12L_3   format=F00690_. label="NA - Diarrhea: private doctor(3)";
  attrib H12L_4   format=F00690_. label="NA - Diarrhea: private doctor(4)";
  attrib H12L_5   format=F00690_. label="NA - Diarrhea: private doctor(5)";
  attrib H12L_6   format=F00690_. label="NA - Diarrhea: private doctor(6)";
  attrib H12M_1   format=F00691_. label="NA - Diarrhea: CS private medical(1)";
  attrib H12M_2   format=F00691_. label="NA - Diarrhea: CS private medical(2)";
  attrib H12M_3   format=F00691_. label="NA - Diarrhea: CS private medical(3)";
  attrib H12M_4   format=F00691_. label="NA - Diarrhea: CS private medical(4)";
  attrib H12M_5   format=F00691_. label="NA - Diarrhea: CS private medical(5)";
  attrib H12M_6   format=F00691_. label="NA - Diarrhea: CS private medical(6)";
  attrib H12N_1   format=F00692_. label="NA - Diarrhea: CS private medical(1)";
  attrib H12N_2   format=F00692_. label="NA - Diarrhea: CS private medical(2)";
  attrib H12N_3   format=F00692_. label="NA - Diarrhea: CS private medical(3)";
  attrib H12N_4   format=F00692_. label="NA - Diarrhea: CS private medical(4)";
  attrib H12N_5   format=F00692_. label="NA - Diarrhea: CS private medical(5)";
  attrib H12N_6   format=F00692_. label="NA - Diarrhea: CS private medical(6)";
  attrib H12O_1   format=F00693_. label="NA - Diarrhea: CS private medical(1)";
  attrib H12O_2   format=F00693_. label="NA - Diarrhea: CS private medical(2)";
  attrib H12O_3   format=F00693_. label="NA - Diarrhea: CS private medical(3)";
  attrib H12O_4   format=F00693_. label="NA - Diarrhea: CS private medical(4)";
  attrib H12O_5   format=F00693_. label="NA - Diarrhea: CS private medical(5)";
  attrib H12O_6   format=F00693_. label="NA - Diarrhea: CS private medical(6)";
  attrib H12P_1   format=F00694_. label="NA - Diarrhea: CS private medical(1)";
  attrib H12P_2   format=F00694_. label="NA - Diarrhea: CS private medical(2)";
  attrib H12P_3   format=F00694_. label="NA - Diarrhea: CS private medical(3)";
  attrib H12P_4   format=F00694_. label="NA - Diarrhea: CS private medical(4)";
  attrib H12P_5   format=F00694_. label="NA - Diarrhea: CS private medical(5)";
  attrib H12P_6   format=F00694_. label="NA - Diarrhea: CS private medical(6)";
  attrib H12Q_1   format=F00695_. label="NA - Diarrhea: CS private medical(1)";
  attrib H12Q_2   format=F00695_. label="NA - Diarrhea: CS private medical(2)";
  attrib H12Q_3   format=F00695_. label="NA - Diarrhea: CS private medical(3)";
  attrib H12Q_4   format=F00695_. label="NA - Diarrhea: CS private medical(4)";
  attrib H12Q_5   format=F00695_. label="NA - Diarrhea: CS private medical(5)";
  attrib H12Q_6   format=F00695_. label="NA - Diarrhea: CS private medical(6)";
  attrib H12R_1   format=F00696_. label="NA - Diarrhea: CS private medical(1)";
  attrib H12R_2   format=F00696_. label="NA - Diarrhea: CS private medical(2)";
  attrib H12R_3   format=F00696_. label="NA - Diarrhea: CS private medical(3)";
  attrib H12R_4   format=F00696_. label="NA - Diarrhea: CS private medical(4)";
  attrib H12R_5   format=F00696_. label="NA - Diarrhea: CS private medical(5)";
  attrib H12R_6   format=F00696_. label="NA - Diarrhea: CS private medical(6)";
  attrib H12S_1   format=F00697_. label="NA - Diarrhea: CS other sector(1)";
  attrib H12S_2   format=F00697_. label="NA - Diarrhea: CS other sector(2)";
  attrib H12S_3   format=F00697_. label="NA - Diarrhea: CS other sector(3)";
  attrib H12S_4   format=F00697_. label="NA - Diarrhea: CS other sector(4)";
  attrib H12S_5   format=F00697_. label="NA - Diarrhea: CS other sector(5)";
  attrib H12S_6   format=F00697_. label="NA - Diarrhea: CS other sector(6)";
  attrib H12T_1   format=F00698_. label="NA - Diarrhea: CS other sector(1)";
  attrib H12T_2   format=F00698_. label="NA - Diarrhea: CS other sector(2)";
  attrib H12T_3   format=F00698_. label="NA - Diarrhea: CS other sector(3)";
  attrib H12T_4   format=F00698_. label="NA - Diarrhea: CS other sector(4)";
  attrib H12T_5   format=F00698_. label="NA - Diarrhea: CS other sector(5)";
  attrib H12T_6   format=F00698_. label="NA - Diarrhea: CS other sector(6)";
  attrib H12U_1   format=F00699_. label="NA - Diarrhea: CS other sector(1)";
  attrib H12U_2   format=F00699_. label="NA - Diarrhea: CS other sector(2)";
  attrib H12U_3   format=F00699_. label="NA - Diarrhea: CS other sector(3)";
  attrib H12U_4   format=F00699_. label="NA - Diarrhea: CS other sector(4)";
  attrib H12U_5   format=F00699_. label="NA - Diarrhea: CS other sector(5)";
  attrib H12U_6   format=F00699_. label="NA - Diarrhea: CS other sector(6)";
  attrib H12V_1   format=F00700_. label="NA - Diarrhea: CS other sector(1)";
  attrib H12V_2   format=F00700_. label="NA - Diarrhea: CS other sector(2)";
  attrib H12V_3   format=F00700_. label="NA - Diarrhea: CS other sector(3)";
  attrib H12V_4   format=F00700_. label="NA - Diarrhea: CS other sector(4)";
  attrib H12V_5   format=F00700_. label="NA - Diarrhea: CS other sector(5)";
  attrib H12V_6   format=F00700_. label="NA - Diarrhea: CS other sector(6)";
  attrib H12W_1   format=F00701_. label="NA - Diarrhea: CS other sector(1)";
  attrib H12W_2   format=F00701_. label="NA - Diarrhea: CS other sector(2)";
  attrib H12W_3   format=F00701_. label="NA - Diarrhea: CS other sector(3)";
  attrib H12W_4   format=F00701_. label="NA - Diarrhea: CS other sector(4)";
  attrib H12W_5   format=F00701_. label="NA - Diarrhea: CS other sector(5)";
  attrib H12W_6   format=F00701_. label="NA - Diarrhea: CS other sector(6)";
  attrib H12X_1   format=F00702_. label="NA - Diarrhea: Other(1)";
  attrib H12X_2   format=F00702_. label="NA - Diarrhea: Other(2)";
  attrib H12X_3   format=F00702_. label="NA - Diarrhea: Other(3)";
  attrib H12X_4   format=F00702_. label="NA - Diarrhea: Other(4)";
  attrib H12X_5   format=F00702_. label="NA - Diarrhea: Other(5)";
  attrib H12X_6   format=F00702_. label="NA - Diarrhea: Other(6)";
  attrib H12Y_1   format=F00703_. label="NA - Diarrhea: no treatment(1)";
  attrib H12Y_2   format=F00703_. label="NA - Diarrhea: no treatment(2)";
  attrib H12Y_3   format=F00703_. label="NA - Diarrhea: no treatment(3)";
  attrib H12Y_4   format=F00703_. label="NA - Diarrhea: no treatment(4)";
  attrib H12Y_5   format=F00703_. label="NA - Diarrhea: no treatment(5)";
  attrib H12Y_6   format=F00703_. label="NA - Diarrhea: no treatment(6)";
  attrib H12Z_1   format=F00704_. label="NA - Diarrhea: medical treatment(1)";
  attrib H12Z_2   format=F00704_. label="NA - Diarrhea: medical treatment(2)";
  attrib H12Z_3   format=F00704_. label="NA - Diarrhea: medical treatment(3)";
  attrib H12Z_4   format=F00704_. label="NA - Diarrhea: medical treatment(4)";
  attrib H12Z_5   format=F00704_. label="NA - Diarrhea: medical treatment(5)";
  attrib H12Z_6   format=F00704_. label="NA - Diarrhea: medical treatment(6)";
  attrib H13_1    format=F00705_. label="NA - Given oral rehydration(1)";
  attrib H13_2    format=F00705_. label="NA - Given oral rehydration(2)";
  attrib H13_3    format=F00705_. label="NA - Given oral rehydration(3)";
  attrib H13_4    format=F00705_. label="NA - Given oral rehydration(4)";
  attrib H13_5    format=F00705_. label="NA - Given oral rehydration(5)";
  attrib H13_6    format=F00705_. label="NA - Given oral rehydration(6)";
  attrib H13B_1   format=F00706_. label="NA - Given pre-packaged ORS liquid(1)";
  attrib H13B_2   format=F00706_. label="NA - Given pre-packaged ORS liquid(2)";
  attrib H13B_3   format=F00706_. label="NA - Given pre-packaged ORS liquid(3)";
  attrib H13B_4   format=F00706_. label="NA - Given pre-packaged ORS liquid(4)";
  attrib H13B_5   format=F00706_. label="NA - Given pre-packaged ORS liquid(5)";
  attrib H13B_6   format=F00706_. label="NA - Given pre-packaged ORS liquid(6)";
  attrib H14_1    format=F00707_. label="NA - Given recommended home solution(1)";
  attrib H14_2    format=F00707_. label="NA - Given recommended home solution(2)";
  attrib H14_3    format=F00707_. label="NA - Given recommended home solution(3)";
  attrib H14_4    format=F00707_. label="NA - Given recommended home solution(4)";
  attrib H14_5    format=F00707_. label="NA - Given recommended home solution(5)";
  attrib H14_6    format=F00707_. label="NA - Given recommended home solution(6)";
  attrib H15_1    format=F00708_. label="NA - Given antibiotic pills or syrups(1)";
  attrib H15_2    format=F00708_. label="NA - Given antibiotic pills or syrups(2)";
  attrib H15_3    format=F00708_. label="NA - Given antibiotic pills or syrups(3)";
  attrib H15_4    format=F00708_. label="NA - Given antibiotic pills or syrups(4)";
  attrib H15_5    format=F00708_. label="NA - Given antibiotic pills or syrups(5)";
  attrib H15_6    format=F00708_. label="NA - Given antibiotic pills or syrups(6)";
  attrib H15A_1   format=F00709_. label="NA - Given antimotility(1)";
  attrib H15A_2   format=F00709_. label="NA - Given antimotility(2)";
  attrib H15A_3   format=F00709_. label="NA - Given antimotility(3)";
  attrib H15A_4   format=F00709_. label="NA - Given antimotility(4)";
  attrib H15A_5   format=F00709_. label="NA - Given antimotility(5)";
  attrib H15A_6   format=F00709_. label="NA - Given antimotility(6)";
  attrib H15B_1   format=F00710_. label="NA - Given antibiotic injection(1)";
  attrib H15B_2   format=F00710_. label="NA - Given antibiotic injection(2)";
  attrib H15B_3   format=F00710_. label="NA - Given antibiotic injection(3)";
  attrib H15B_4   format=F00710_. label="NA - Given antibiotic injection(4)";
  attrib H15B_5   format=F00710_. label="NA - Given antibiotic injection(5)";
  attrib H15B_6   format=F00710_. label="NA - Given antibiotic injection(6)";
  attrib H15C_1   format=F00711_. label="NA - Given intravenous (IV)(1)";
  attrib H15C_2   format=F00711_. label="NA - Given intravenous (IV)(2)";
  attrib H15C_3   format=F00711_. label="NA - Given intravenous (IV)(3)";
  attrib H15C_4   format=F00711_. label="NA - Given intravenous (IV)(4)";
  attrib H15C_5   format=F00711_. label="NA - Given intravenous (IV)(5)";
  attrib H15C_6   format=F00711_. label="NA - Given intravenous (IV)(6)";
  attrib H15D_1   format=F00712_. label="NA - Given home remedy, herbal medicine(1)";
  attrib H15D_2   format=F00712_. label="NA - Given home remedy, herbal medicine(2)";
  attrib H15D_3   format=F00712_. label="NA - Given home remedy, herbal medicine(3)";
  attrib H15D_4   format=F00712_. label="NA - Given home remedy, herbal medicine(4)";
  attrib H15D_5   format=F00712_. label="NA - Given home remedy, herbal medicine(5)";
  attrib H15D_6   format=F00712_. label="NA - Given home remedy, herbal medicine(6)";
  attrib H15E_1   format=F00713_. label="NA - Given zinc(1)";
  attrib H15E_2   format=F00713_. label="NA - Given zinc(2)";
  attrib H15E_3   format=F00713_. label="NA - Given zinc(3)";
  attrib H15E_4   format=F00713_. label="NA - Given zinc(4)";
  attrib H15E_5   format=F00713_. label="NA - Given zinc(5)";
  attrib H15E_6   format=F00713_. label="NA - Given zinc(6)";
  attrib H15F_1   format=F00714_. label="NA - Given other (not antibiotic, antimotility, zinc)(1)";
  attrib H15F_2   format=F00714_. label="NA - Given other (not antibiotic, antimotility, zinc)(2)";
  attrib H15F_3   format=F00714_. label="NA - Given other (not antibiotic, antimotility, zinc)(3)";
  attrib H15F_4   format=F00714_. label="NA - Given other (not antibiotic, antimotility, zinc)(4)";
  attrib H15F_5   format=F00714_. label="NA - Given other (not antibiotic, antimotility, zinc)(5)";
  attrib H15F_6   format=F00714_. label="NA - Given other (not antibiotic, antimotility, zinc)(6)";
  attrib H15G_1   format=F00715_. label="NA - Given unknown pill or syrup(1)";
  attrib H15G_2   format=F00715_. label="NA - Given unknown pill or syrup(2)";
  attrib H15G_3   format=F00715_. label="NA - Given unknown pill or syrup(3)";
  attrib H15G_4   format=F00715_. label="NA - Given unknown pill or syrup(4)";
  attrib H15G_5   format=F00715_. label="NA - Given unknown pill or syrup(5)";
  attrib H15G_6   format=F00715_. label="NA - Given unknown pill or syrup(6)";
  attrib H15H_1   format=F00716_. label="NA - Given non-antibiotic injection(1)";
  attrib H15H_2   format=F00716_. label="NA - Given non-antibiotic injection(2)";
  attrib H15H_3   format=F00716_. label="NA - Given non-antibiotic injection(3)";
  attrib H15H_4   format=F00716_. label="NA - Given non-antibiotic injection(4)";
  attrib H15H_5   format=F00716_. label="NA - Given non-antibiotic injection(5)";
  attrib H15H_6   format=F00716_. label="NA - Given non-antibiotic injection(6)";
  attrib H15I_1   format=F00717_. label="NA - Given unknown injection(1)";
  attrib H15I_2   format=F00717_. label="NA - Given unknown injection(2)";
  attrib H15I_3   format=F00717_. label="NA - Given unknown injection(3)";
  attrib H15I_4   format=F00717_. label="NA - Given unknown injection(4)";
  attrib H15I_5   format=F00717_. label="NA - Given unknown injection(5)";
  attrib H15I_6   format=F00717_. label="NA - Given unknown injection(6)";
  attrib H15J_1   format=F00718_. label="NA - Given CS(1)";
  attrib H15J_2   format=F00718_. label="NA - Given CS(2)";
  attrib H15J_3   format=F00718_. label="NA - Given CS(3)";
  attrib H15J_4   format=F00718_. label="NA - Given CS(4)";
  attrib H15J_5   format=F00718_. label="NA - Given CS(5)";
  attrib H15J_6   format=F00718_. label="NA - Given CS(6)";
  attrib H15K_1   format=F00719_. label="NA - Given CS(1)";
  attrib H15K_2   format=F00719_. label="NA - Given CS(2)";
  attrib H15K_3   format=F00719_. label="NA - Given CS(3)";
  attrib H15K_4   format=F00719_. label="NA - Given CS(4)";
  attrib H15K_5   format=F00719_. label="NA - Given CS(5)";
  attrib H15K_6   format=F00719_. label="NA - Given CS(6)";
  attrib H15L_1   format=F00720_. label="NA - Given CS(1)";
  attrib H15L_2   format=F00720_. label="NA - Given CS(2)";
  attrib H15L_3   format=F00720_. label="NA - Given CS(3)";
  attrib H15L_4   format=F00720_. label="NA - Given CS(4)";
  attrib H15L_5   format=F00720_. label="NA - Given CS(5)";
  attrib H15L_6   format=F00720_. label="NA - Given CS(6)";
  attrib H15M_1   format=F00721_. label="NA - Given CS(1)";
  attrib H15M_2   format=F00721_. label="NA - Given CS(2)";
  attrib H15M_3   format=F00721_. label="NA - Given CS(3)";
  attrib H15M_4   format=F00721_. label="NA - Given CS(4)";
  attrib H15M_5   format=F00721_. label="NA - Given CS(5)";
  attrib H15M_6   format=F00721_. label="NA - Given CS(6)";
  attrib H20_1    format=F00722_. label="NA - Given other treatment(1)";
  attrib H20_2    format=F00722_. label="NA - Given other treatment(2)";
  attrib H20_3    format=F00722_. label="NA - Given other treatment(3)";
  attrib H20_4    format=F00722_. label="NA - Given other treatment(4)";
  attrib H20_5    format=F00722_. label="NA - Given other treatment(5)";
  attrib H20_6    format=F00722_. label="NA - Given other treatment(6)";
  attrib H21A_1   format=F00723_. label="NA - Given no treatment(1)";
  attrib H21A_2   format=F00723_. label="NA - Given no treatment(2)";
  attrib H21A_3   format=F00723_. label="NA - Given no treatment(3)";
  attrib H21A_4   format=F00723_. label="NA - Given no treatment(4)";
  attrib H21A_5   format=F00723_. label="NA - Given no treatment(5)";
  attrib H21A_6   format=F00723_. label="NA - Given no treatment(6)";
  attrib H21_1    format=F00724_. label="NA - Received any treatment(1)";
  attrib H21_2    format=F00724_. label="NA - Received any treatment(2)";
  attrib H21_3    format=F00724_. label="NA - Received any treatment(3)";
  attrib H21_4    format=F00724_. label="NA - Received any treatment(4)";
  attrib H21_5    format=F00724_. label="NA - Received any treatment(5)";
  attrib H21_6    format=F00724_. label="NA - Received any treatment(6)";
  attrib H22_1    format=F00725_. label="NA - Had fever in last two weeks(1)";
  attrib H22_2    format=F00725_. label="NA - Had fever in last two weeks(2)";
  attrib H22_3    format=F00725_. label="NA - Had fever in last two weeks(3)";
  attrib H22_4    format=F00725_. label="NA - Had fever in last two weeks(4)";
  attrib H22_5    format=F00725_. label="NA - Had fever in last two weeks(5)";
  attrib H22_6    format=F00725_. label="NA - Had fever in last two weeks(6)";
  attrib H31_1    format=F00726_. label="NA - Had cough in last two weeks(1)";
  attrib H31_2    format=F00726_. label="NA - Had cough in last two weeks(2)";
  attrib H31_3    format=F00726_. label="NA - Had cough in last two weeks(3)";
  attrib H31_4    format=F00726_. label="NA - Had cough in last two weeks(4)";
  attrib H31_5    format=F00726_. label="NA - Had cough in last two weeks(5)";
  attrib H31_6    format=F00726_. label="NA - Had cough in last two weeks(6)";
  attrib H31B_1   format=F00727_. label="NA - Short, rapid breaths(1)";
  attrib H31B_2   format=F00727_. label="NA - Short, rapid breaths(2)";
  attrib H31B_3   format=F00727_. label="NA - Short, rapid breaths(3)";
  attrib H31B_4   format=F00727_. label="NA - Short, rapid breaths(4)";
  attrib H31B_5   format=F00727_. label="NA - Short, rapid breaths(5)";
  attrib H31B_6   format=F00727_. label="NA - Short, rapid breaths(6)";
  attrib H31C_1   format=F00728_. label="NA - Problem in the chest or blocked or running nose(1)";
  attrib H31C_2   format=F00728_. label="NA - Problem in the chest or blocked or running nose(2)";
  attrib H31C_3   format=F00728_. label="NA - Problem in the chest or blocked or running nose(3)";
  attrib H31C_4   format=F00728_. label="NA - Problem in the chest or blocked or running nose(4)";
  attrib H31C_5   format=F00728_. label="NA - Problem in the chest or blocked or running nose(5)";
  attrib H31C_6   format=F00728_. label="NA - Problem in the chest or blocked or running nose(6)";
  attrib H31D_1   format=F00729_. label="NA - Had fever/cough in last 2 weeks: amount offered to drink(1)";
  attrib H31D_2   format=F00729_. label="NA - Had fever/cough in last 2 weeks: amount offered to drink(2)";
  attrib H31D_3   format=F00729_. label="NA - Had fever/cough in last 2 weeks: amount offered to drink(3)";
  attrib H31D_4   format=F00729_. label="NA - Had fever/cough in last 2 weeks: amount offered to drink(4)";
  attrib H31D_5   format=F00729_. label="NA - Had fever/cough in last 2 weeks: amount offered to drink(5)";
  attrib H31D_6   format=F00729_. label="NA - Had fever/cough in last 2 weeks: amount offered to drink(6)";
  attrib H31E_1   format=F00730_. label="NA - Had fever/cough in last 2 weeks: amount offered to eat(1)";
  attrib H31E_2   format=F00730_. label="NA - Had fever/cough in last 2 weeks: amount offered to eat(2)";
  attrib H31E_3   format=F00730_. label="NA - Had fever/cough in last 2 weeks: amount offered to eat(3)";
  attrib H31E_4   format=F00730_. label="NA - Had fever/cough in last 2 weeks: amount offered to eat(4)";
  attrib H31E_5   format=F00730_. label="NA - Had fever/cough in last 2 weeks: amount offered to eat(5)";
  attrib H31E_6   format=F00730_. label="NA - Had fever/cough in last 2 weeks: amount offered to eat(6)";
  attrib H32A_1   format=F00731_. label="NA - Fever/cough: government hospital(1)";
  attrib H32A_2   format=F00731_. label="NA - Fever/cough: government hospital(2)";
  attrib H32A_3   format=F00731_. label="NA - Fever/cough: government hospital(3)";
  attrib H32A_4   format=F00731_. label="NA - Fever/cough: government hospital(4)";
  attrib H32A_5   format=F00731_. label="NA - Fever/cough: government hospital(5)";
  attrib H32A_6   format=F00731_. label="NA - Fever/cough: government hospital(6)";
  attrib H32B_1   format=F00732_. label="NA - Fever/cough: CS public sector(1)";
  attrib H32B_2   format=F00732_. label="NA - Fever/cough: CS public sector(2)";
  attrib H32B_3   format=F00732_. label="NA - Fever/cough: CS public sector(3)";
  attrib H32B_4   format=F00732_. label="NA - Fever/cough: CS public sector(4)";
  attrib H32B_5   format=F00732_. label="NA - Fever/cough: CS public sector(5)";
  attrib H32B_6   format=F00732_. label="NA - Fever/cough: CS public sector(6)";
  attrib H32C_1   format=F00733_. label="NA - Fever/cough: CS public sector(1)";
  attrib H32C_2   format=F00733_. label="NA - Fever/cough: CS public sector(2)";
  attrib H32C_3   format=F00733_. label="NA - Fever/cough: CS public sector(3)";
  attrib H32C_4   format=F00733_. label="NA - Fever/cough: CS public sector(4)";
  attrib H32C_5   format=F00733_. label="NA - Fever/cough: CS public sector(5)";
  attrib H32C_6   format=F00733_. label="NA - Fever/cough: CS public sector(6)";
  attrib H32D_1   format=F00734_. label="NA - Fever/cough: CS public sector(1)";
  attrib H32D_2   format=F00734_. label="NA - Fever/cough: CS public sector(2)";
  attrib H32D_3   format=F00734_. label="NA - Fever/cough: CS public sector(3)";
  attrib H32D_4   format=F00734_. label="NA - Fever/cough: CS public sector(4)";
  attrib H32D_5   format=F00734_. label="NA - Fever/cough: CS public sector(5)";
  attrib H32D_6   format=F00734_. label="NA - Fever/cough: CS public sector(6)";
  attrib H32E_1   format=F00735_. label="NA - Fever/cough: CS public sector(1)";
  attrib H32E_2   format=F00735_. label="NA - Fever/cough: CS public sector(2)";
  attrib H32E_3   format=F00735_. label="NA - Fever/cough: CS public sector(3)";
  attrib H32E_4   format=F00735_. label="NA - Fever/cough: CS public sector(4)";
  attrib H32E_5   format=F00735_. label="NA - Fever/cough: CS public sector(5)";
  attrib H32E_6   format=F00735_. label="NA - Fever/cough: CS public sector(6)";
  attrib H32F_1   format=F00736_. label="NA - Fever/cough: CS public sector(1)";
  attrib H32F_2   format=F00736_. label="NA - Fever/cough: CS public sector(2)";
  attrib H32F_3   format=F00736_. label="NA - Fever/cough: CS public sector(3)";
  attrib H32F_4   format=F00736_. label="NA - Fever/cough: CS public sector(4)";
  attrib H32F_5   format=F00736_. label="NA - Fever/cough: CS public sector(5)";
  attrib H32F_6   format=F00736_. label="NA - Fever/cough: CS public sector(6)";
  attrib H32G_1   format=F00737_. label="NA - Fever/cough: CS public sector(1)";
  attrib H32G_2   format=F00737_. label="NA - Fever/cough: CS public sector(2)";
  attrib H32G_3   format=F00737_. label="NA - Fever/cough: CS public sector(3)";
  attrib H32G_4   format=F00737_. label="NA - Fever/cough: CS public sector(4)";
  attrib H32G_5   format=F00737_. label="NA - Fever/cough: CS public sector(5)";
  attrib H32G_6   format=F00737_. label="NA - Fever/cough: CS public sector(6)";
  attrib H32H_1   format=F00738_. label="NA - Fever/cough: CS public sector(1)";
  attrib H32H_2   format=F00738_. label="NA - Fever/cough: CS public sector(2)";
  attrib H32H_3   format=F00738_. label="NA - Fever/cough: CS public sector(3)";
  attrib H32H_4   format=F00738_. label="NA - Fever/cough: CS public sector(4)";
  attrib H32H_5   format=F00738_. label="NA - Fever/cough: CS public sector(5)";
  attrib H32H_6   format=F00738_. label="NA - Fever/cough: CS public sector(6)";
  attrib H32I_1   format=F00739_. label="NA - Fever/cough: CS public sector(1)";
  attrib H32I_2   format=F00739_. label="NA - Fever/cough: CS public sector(2)";
  attrib H32I_3   format=F00739_. label="NA - Fever/cough: CS public sector(3)";
  attrib H32I_4   format=F00739_. label="NA - Fever/cough: CS public sector(4)";
  attrib H32I_5   format=F00739_. label="NA - Fever/cough: CS public sector(5)";
  attrib H32I_6   format=F00739_. label="NA - Fever/cough: CS public sector(6)";
  attrib H32J_1   format=F00740_. label="NA - Fever/cough: private hospital/ clinic(1)";
  attrib H32J_2   format=F00740_. label="NA - Fever/cough: private hospital/ clinic(2)";
  attrib H32J_3   format=F00740_. label="NA - Fever/cough: private hospital/ clinic(3)";
  attrib H32J_4   format=F00740_. label="NA - Fever/cough: private hospital/ clinic(4)";
  attrib H32J_5   format=F00740_. label="NA - Fever/cough: private hospital/ clinic(5)";
  attrib H32J_6   format=F00740_. label="NA - Fever/cough: private hospital/ clinic(6)";
  attrib H32K_1   format=F00741_. label="NA - Fever/cough: private pharmacy(1)";
  attrib H32K_2   format=F00741_. label="NA - Fever/cough: private pharmacy(2)";
  attrib H32K_3   format=F00741_. label="NA - Fever/cough: private pharmacy(3)";
  attrib H32K_4   format=F00741_. label="NA - Fever/cough: private pharmacy(4)";
  attrib H32K_5   format=F00741_. label="NA - Fever/cough: private pharmacy(5)";
  attrib H32K_6   format=F00741_. label="NA - Fever/cough: private pharmacy(6)";
  attrib H32L_1   format=F00742_. label="NA - Fever/cough: private doctor(1)";
  attrib H32L_2   format=F00742_. label="NA - Fever/cough: private doctor(2)";
  attrib H32L_3   format=F00742_. label="NA - Fever/cough: private doctor(3)";
  attrib H32L_4   format=F00742_. label="NA - Fever/cough: private doctor(4)";
  attrib H32L_5   format=F00742_. label="NA - Fever/cough: private doctor(5)";
  attrib H32L_6   format=F00742_. label="NA - Fever/cough: private doctor(6)";
  attrib H32M_1   format=F00743_. label="NA - Fever/cough: CS private medical(1)";
  attrib H32M_2   format=F00743_. label="NA - Fever/cough: CS private medical(2)";
  attrib H32M_3   format=F00743_. label="NA - Fever/cough: CS private medical(3)";
  attrib H32M_4   format=F00743_. label="NA - Fever/cough: CS private medical(4)";
  attrib H32M_5   format=F00743_. label="NA - Fever/cough: CS private medical(5)";
  attrib H32M_6   format=F00743_. label="NA - Fever/cough: CS private medical(6)";
  attrib H32N_1   format=F00744_. label="NA - Fever/cough: CS private medical(1)";
  attrib H32N_2   format=F00744_. label="NA - Fever/cough: CS private medical(2)";
  attrib H32N_3   format=F00744_. label="NA - Fever/cough: CS private medical(3)";
  attrib H32N_4   format=F00744_. label="NA - Fever/cough: CS private medical(4)";
  attrib H32N_5   format=F00744_. label="NA - Fever/cough: CS private medical(5)";
  attrib H32N_6   format=F00744_. label="NA - Fever/cough: CS private medical(6)";
  attrib H32O_1   format=F00745_. label="NA - Fever/cough: CS private medical(1)";
  attrib H32O_2   format=F00745_. label="NA - Fever/cough: CS private medical(2)";
  attrib H32O_3   format=F00745_. label="NA - Fever/cough: CS private medical(3)";
  attrib H32O_4   format=F00745_. label="NA - Fever/cough: CS private medical(4)";
  attrib H32O_5   format=F00745_. label="NA - Fever/cough: CS private medical(5)";
  attrib H32O_6   format=F00745_. label="NA - Fever/cough: CS private medical(6)";
  attrib H32P_1   format=F00746_. label="NA - Fever/cough: CS private medical(1)";
  attrib H32P_2   format=F00746_. label="NA - Fever/cough: CS private medical(2)";
  attrib H32P_3   format=F00746_. label="NA - Fever/cough: CS private medical(3)";
  attrib H32P_4   format=F00746_. label="NA - Fever/cough: CS private medical(4)";
  attrib H32P_5   format=F00746_. label="NA - Fever/cough: CS private medical(5)";
  attrib H32P_6   format=F00746_. label="NA - Fever/cough: CS private medical(6)";
  attrib H32Q_1   format=F00747_. label="NA - Fever/cough: CS private medical(1)";
  attrib H32Q_2   format=F00747_. label="NA - Fever/cough: CS private medical(2)";
  attrib H32Q_3   format=F00747_. label="NA - Fever/cough: CS private medical(3)";
  attrib H32Q_4   format=F00747_. label="NA - Fever/cough: CS private medical(4)";
  attrib H32Q_5   format=F00747_. label="NA - Fever/cough: CS private medical(5)";
  attrib H32Q_6   format=F00747_. label="NA - Fever/cough: CS private medical(6)";
  attrib H32R_1   format=F00748_. label="NA - Fever/cough: CS private medical(1)";
  attrib H32R_2   format=F00748_. label="NA - Fever/cough: CS private medical(2)";
  attrib H32R_3   format=F00748_. label="NA - Fever/cough: CS private medical(3)";
  attrib H32R_4   format=F00748_. label="NA - Fever/cough: CS private medical(4)";
  attrib H32R_5   format=F00748_. label="NA - Fever/cough: CS private medical(5)";
  attrib H32R_6   format=F00748_. label="NA - Fever/cough: CS private medical(6)";
  attrib H32S_1   format=F00749_. label="NA - Fever/cough: CS other sector(1)";
  attrib H32S_2   format=F00749_. label="NA - Fever/cough: CS other sector(2)";
  attrib H32S_3   format=F00749_. label="NA - Fever/cough: CS other sector(3)";
  attrib H32S_4   format=F00749_. label="NA - Fever/cough: CS other sector(4)";
  attrib H32S_5   format=F00749_. label="NA - Fever/cough: CS other sector(5)";
  attrib H32S_6   format=F00749_. label="NA - Fever/cough: CS other sector(6)";
  attrib H32T_1   format=F00750_. label="NA - Fever/cough: CS other sector(1)";
  attrib H32T_2   format=F00750_. label="NA - Fever/cough: CS other sector(2)";
  attrib H32T_3   format=F00750_. label="NA - Fever/cough: CS other sector(3)";
  attrib H32T_4   format=F00750_. label="NA - Fever/cough: CS other sector(4)";
  attrib H32T_5   format=F00750_. label="NA - Fever/cough: CS other sector(5)";
  attrib H32T_6   format=F00750_. label="NA - Fever/cough: CS other sector(6)";
  attrib H32U_1   format=F00751_. label="NA - Fever/cough: CS other sector(1)";
  attrib H32U_2   format=F00751_. label="NA - Fever/cough: CS other sector(2)";
  attrib H32U_3   format=F00751_. label="NA - Fever/cough: CS other sector(3)";
  attrib H32U_4   format=F00751_. label="NA - Fever/cough: CS other sector(4)";
  attrib H32U_5   format=F00751_. label="NA - Fever/cough: CS other sector(5)";
  attrib H32U_6   format=F00751_. label="NA - Fever/cough: CS other sector(6)";
  attrib H32V_1   format=F00752_. label="NA - Fever/cough: CS other sector(1)";
  attrib H32V_2   format=F00752_. label="NA - Fever/cough: CS other sector(2)";
  attrib H32V_3   format=F00752_. label="NA - Fever/cough: CS other sector(3)";
  attrib H32V_4   format=F00752_. label="NA - Fever/cough: CS other sector(4)";
  attrib H32V_5   format=F00752_. label="NA - Fever/cough: CS other sector(5)";
  attrib H32V_6   format=F00752_. label="NA - Fever/cough: CS other sector(6)";
  attrib H32W_1   format=F00753_. label="NA - Fever/cough: CS other sector(1)";
  attrib H32W_2   format=F00753_. label="NA - Fever/cough: CS other sector(2)";
  attrib H32W_3   format=F00753_. label="NA - Fever/cough: CS other sector(3)";
  attrib H32W_4   format=F00753_. label="NA - Fever/cough: CS other sector(4)";
  attrib H32W_5   format=F00753_. label="NA - Fever/cough: CS other sector(5)";
  attrib H32W_6   format=F00753_. label="NA - Fever/cough: CS other sector(6)";
  attrib H32X_1   format=F00754_. label="NA - Fever/cough: other(1)";
  attrib H32X_2   format=F00754_. label="NA - Fever/cough: other(2)";
  attrib H32X_3   format=F00754_. label="NA - Fever/cough: other(3)";
  attrib H32X_4   format=F00754_. label="NA - Fever/cough: other(4)";
  attrib H32X_5   format=F00754_. label="NA - Fever/cough: other(5)";
  attrib H32X_6   format=F00754_. label="NA - Fever/cough: other(6)";
  attrib H32Y_1   format=F00755_. label="NA - Fever/cough: no treatment(1)";
  attrib H32Y_2   format=F00755_. label="NA - Fever/cough: no treatment(2)";
  attrib H32Y_3   format=F00755_. label="NA - Fever/cough: no treatment(3)";
  attrib H32Y_4   format=F00755_. label="NA - Fever/cough: no treatment(4)";
  attrib H32Y_5   format=F00755_. label="NA - Fever/cough: no treatment(5)";
  attrib H32Y_6   format=F00755_. label="NA - Fever/cough: no treatment(6)";
  attrib H32Z_1   format=F00756_. label="NA - Fever/cough: medical treatment(1)";
  attrib H32Z_2   format=F00756_. label="NA - Fever/cough: medical treatment(2)";
  attrib H32Z_3   format=F00756_. label="NA - Fever/cough: medical treatment(3)";
  attrib H32Z_4   format=F00756_. label="NA - Fever/cough: medical treatment(4)";
  attrib H32Z_5   format=F00756_. label="NA - Fever/cough: medical treatment(5)";
  attrib H32Z_6   format=F00756_. label="NA - Fever/cough: medical treatment(6)";
  attrib H34_1    format=F00757_. label="NA - Vitamin A in last 6 months(1)";
  attrib H34_2    format=F00757_. label="NA - Vitamin A in last 6 months(2)";
  attrib H34_3    format=F00757_. label="NA - Vitamin A in last 6 months(3)";
  attrib H34_4    format=F00757_. label="NA - Vitamin A in last 6 months(4)";
  attrib H34_5    format=F00757_. label="NA - Vitamin A in last 6 months(5)";
  attrib H34_6    format=F00757_. label="NA - Vitamin A in last 6 months(6)";
  attrib H37A_1   format=F00758_. label="NA - SP/Fansidar taken for fever(1)";
  attrib H37A_2   format=F00758_. label="NA - SP/Fansidar taken for fever(2)";
  attrib H37A_3   format=F00758_. label="NA - SP/Fansidar taken for fever(3)";
  attrib H37A_4   format=F00758_. label="NA - SP/Fansidar taken for fever(4)";
  attrib H37A_5   format=F00758_. label="NA - SP/Fansidar taken for fever(5)";
  attrib H37A_6   format=F00758_. label="NA - SP/Fansidar taken for fever(6)";
  attrib H37B_1   format=F00759_. label="NA - Chloroquine taken for fever(1)";
  attrib H37B_2   format=F00759_. label="NA - Chloroquine taken for fever(2)";
  attrib H37B_3   format=F00759_. label="NA - Chloroquine taken for fever(3)";
  attrib H37B_4   format=F00759_. label="NA - Chloroquine taken for fever(4)";
  attrib H37B_5   format=F00759_. label="NA - Chloroquine taken for fever(5)";
  attrib H37B_6   format=F00759_. label="NA - Chloroquine taken for fever(6)";
  attrib H37C_1   format=F00760_. label="NA - Amodiaquine taken for fever(1)";
  attrib H37C_2   format=F00760_. label="NA - Amodiaquine taken for fever(2)";
  attrib H37C_3   format=F00760_. label="NA - Amodiaquine taken for fever(3)";
  attrib H37C_4   format=F00760_. label="NA - Amodiaquine taken for fever(4)";
  attrib H37C_5   format=F00760_. label="NA - Amodiaquine taken for fever(5)";
  attrib H37C_6   format=F00760_. label="NA - Amodiaquine taken for fever(6)";
  attrib H37D_1   format=F00761_. label="NA - Quinine taken for fever(1)";
  attrib H37D_2   format=F00761_. label="NA - Quinine taken for fever(2)";
  attrib H37D_3   format=F00761_. label="NA - Quinine taken for fever(3)";
  attrib H37D_4   format=F00761_. label="NA - Quinine taken for fever(4)";
  attrib H37D_5   format=F00761_. label="NA - Quinine taken for fever(5)";
  attrib H37D_6   format=F00761_. label="NA - Quinine taken for fever(6)";
  attrib H37DA_1  format=F00762_. label="NA - Quinine injection/IV taken for fever(1)";
  attrib H37DA_2  format=F00762_. label="NA - Quinine injection/IV taken for fever(2)";
  attrib H37DA_3  format=F00762_. label="NA - Quinine injection/IV taken for fever(3)";
  attrib H37DA_4  format=F00762_. label="NA - Quinine injection/IV taken for fever(4)";
  attrib H37DA_5  format=F00762_. label="NA - Quinine injection/IV taken for fever(5)";
  attrib H37DA_6  format=F00762_. label="NA - Quinine injection/IV taken for fever(6)";
  attrib H37E_1   format=F00763_. label="NA - Combination with artemisinin taken for fever(1)";
  attrib H37E_2   format=F00763_. label="NA - Combination with artemisinin taken for fever(2)";
  attrib H37E_3   format=F00763_. label="NA - Combination with artemisinin taken for fever(3)";
  attrib H37E_4   format=F00763_. label="NA - Combination with artemisinin taken for fever(4)";
  attrib H37E_5   format=F00763_. label="NA - Combination with artemisinin taken for fever(5)";
  attrib H37E_6   format=F00763_. label="NA - Combination with artemisinin taken for fever(6)";
  attrib H37AA_1  format=F00764_. label="NA - Artesunate rectal taken for fever(1)";
  attrib H37AA_2  format=F00764_. label="NA - Artesunate rectal taken for fever(2)";
  attrib H37AA_3  format=F00764_. label="NA - Artesunate rectal taken for fever(3)";
  attrib H37AA_4  format=F00764_. label="NA - Artesunate rectal taken for fever(4)";
  attrib H37AA_5  format=F00764_. label="NA - Artesunate rectal taken for fever(5)";
  attrib H37AA_6  format=F00764_. label="NA - Artesunate rectal taken for fever(6)";
  attrib H37AB_1  format=F00765_. label="NA - Artesunate injection/IV taken for fever(1)";
  attrib H37AB_2  format=F00765_. label="NA - Artesunate injection/IV taken for fever(2)";
  attrib H37AB_3  format=F00765_. label="NA - Artesunate injection/IV taken for fever(3)";
  attrib H37AB_4  format=F00765_. label="NA - Artesunate injection/IV taken for fever(4)";
  attrib H37AB_5  format=F00765_. label="NA - Artesunate injection/IV taken for fever(5)";
  attrib H37AB_6  format=F00765_. label="NA - Artesunate injection/IV taken for fever(6)";
  attrib H37F_1   format=F00766_. label="NA - CS antimalarial taken for fever(1)";
  attrib H37F_2   format=F00766_. label="NA - CS antimalarial taken for fever(2)";
  attrib H37F_3   format=F00766_. label="NA - CS antimalarial taken for fever(3)";
  attrib H37F_4   format=F00766_. label="NA - CS antimalarial taken for fever(4)";
  attrib H37F_5   format=F00766_. label="NA - CS antimalarial taken for fever(5)";
  attrib H37F_6   format=F00766_. label="NA - CS antimalarial taken for fever(6)";
  attrib H37G_1   format=F00767_. label="NA - CS antimalarial taken for fever(1)";
  attrib H37G_2   format=F00767_. label="NA - CS antimalarial taken for fever(2)";
  attrib H37G_3   format=F00767_. label="NA - CS antimalarial taken for fever(3)";
  attrib H37G_4   format=F00767_. label="NA - CS antimalarial taken for fever(4)";
  attrib H37G_5   format=F00767_. label="NA - CS antimalarial taken for fever(5)";
  attrib H37G_6   format=F00767_. label="NA - CS antimalarial taken for fever(6)";
  attrib H37H_1   format=F00768_. label="NA - Other antimalarial taken for fever(1)";
  attrib H37H_2   format=F00768_. label="NA - Other antimalarial taken for fever(2)";
  attrib H37H_3   format=F00768_. label="NA - Other antimalarial taken for fever(3)";
  attrib H37H_4   format=F00768_. label="NA - Other antimalarial taken for fever(4)";
  attrib H37H_5   format=F00768_. label="NA - Other antimalarial taken for fever(5)";
  attrib H37H_6   format=F00768_. label="NA - Other antimalarial taken for fever(6)";
  attrib H37I_1   format=F00769_. label="NA - Antibiotic pill/syrup taken for fever(1)";
  attrib H37I_2   format=F00769_. label="NA - Antibiotic pill/syrup taken for fever(2)";
  attrib H37I_3   format=F00769_. label="NA - Antibiotic pill/syrup taken for fever(3)";
  attrib H37I_4   format=F00769_. label="NA - Antibiotic pill/syrup taken for fever(4)";
  attrib H37I_5   format=F00769_. label="NA - Antibiotic pill/syrup taken for fever(5)";
  attrib H37I_6   format=F00769_. label="NA - Antibiotic pill/syrup taken for fever(6)";
  attrib H37J_1   format=F00770_. label="NA - Antibiotic injection taken for fever(1)";
  attrib H37J_2   format=F00770_. label="NA - Antibiotic injection taken for fever(2)";
  attrib H37J_3   format=F00770_. label="NA - Antibiotic injection taken for fever(3)";
  attrib H37J_4   format=F00770_. label="NA - Antibiotic injection taken for fever(4)";
  attrib H37J_5   format=F00770_. label="NA - Antibiotic injection taken for fever(5)";
  attrib H37J_6   format=F00770_. label="NA - Antibiotic injection taken for fever(6)";
  attrib H37K_1   format=F00771_. label="NA - Aspirin taken for fever(1)";
  attrib H37K_2   format=F00771_. label="NA - Aspirin taken for fever(2)";
  attrib H37K_3   format=F00771_. label="NA - Aspirin taken for fever(3)";
  attrib H37K_4   format=F00771_. label="NA - Aspirin taken for fever(4)";
  attrib H37K_5   format=F00771_. label="NA - Aspirin taken for fever(5)";
  attrib H37K_6   format=F00771_. label="NA - Aspirin taken for fever(6)";
  attrib H37L_1   format=F00772_. label="NA - Acetaminophen/paracetamol/panadol taken for fever(1)";
  attrib H37L_2   format=F00772_. label="NA - Acetaminophen/paracetamol/panadol taken for fever(2)";
  attrib H37L_3   format=F00772_. label="NA - Acetaminophen/paracetamol/panadol taken for fever(3)";
  attrib H37L_4   format=F00772_. label="NA - Acetaminophen/paracetamol/panadol taken for fever(4)";
  attrib H37L_5   format=F00772_. label="NA - Acetaminophen/paracetamol/panadol taken for fever(5)";
  attrib H37L_6   format=F00772_. label="NA - Acetaminophen/paracetamol/panadol taken for fever(6)";
  attrib H37M_1   format=F00773_. label="NA - Ibuprofen taken for fever(1)";
  attrib H37M_2   format=F00773_. label="NA - Ibuprofen taken for fever(2)";
  attrib H37M_3   format=F00773_. label="NA - Ibuprofen taken for fever(3)";
  attrib H37M_4   format=F00773_. label="NA - Ibuprofen taken for fever(4)";
  attrib H37M_5   format=F00773_. label="NA - Ibuprofen taken for fever(5)";
  attrib H37M_6   format=F00773_. label="NA - Ibuprofen taken for fever(6)";
  attrib H37N_1   format=F00774_. label="NA - CS other taken for fever(1)";
  attrib H37N_2   format=F00774_. label="NA - CS other taken for fever(2)";
  attrib H37N_3   format=F00774_. label="NA - CS other taken for fever(3)";
  attrib H37N_4   format=F00774_. label="NA - CS other taken for fever(4)";
  attrib H37N_5   format=F00774_. label="NA - CS other taken for fever(5)";
  attrib H37N_6   format=F00774_. label="NA - CS other taken for fever(6)";
  attrib H37O_1   format=F00775_. label="NA - CS other for fever/cough(1)";
  attrib H37O_2   format=F00775_. label="NA - CS other for fever/cough(2)";
  attrib H37O_3   format=F00775_. label="NA - CS other for fever/cough(3)";
  attrib H37O_4   format=F00775_. label="NA - CS other for fever/cough(4)";
  attrib H37O_5   format=F00775_. label="NA - CS other for fever/cough(5)";
  attrib H37O_6   format=F00775_. label="NA - CS other for fever/cough(6)";
  attrib H37P_1   format=F00776_. label="NA - CS other for fever/cough(1)";
  attrib H37P_2   format=F00776_. label="NA - CS other for fever/cough(2)";
  attrib H37P_3   format=F00776_. label="NA - CS other for fever/cough(3)";
  attrib H37P_4   format=F00776_. label="NA - CS other for fever/cough(4)";
  attrib H37P_5   format=F00776_. label="NA - CS other for fever/cough(5)";
  attrib H37P_6   format=F00776_. label="NA - CS other for fever/cough(6)";
  attrib H37X_1   format=F00777_. label="NA - Other taken for fever(1)";
  attrib H37X_2   format=F00777_. label="NA - Other taken for fever(2)";
  attrib H37X_3   format=F00777_. label="NA - Other taken for fever(3)";
  attrib H37X_4   format=F00777_. label="NA - Other taken for fever(4)";
  attrib H37X_5   format=F00777_. label="NA - Other taken for fever(5)";
  attrib H37X_6   format=F00777_. label="NA - Other taken for fever(6)";
  attrib H37Y_1   format=F00778_. label="NA - Nothing taken and didn't seek treatment for fever/cough(1)";
  attrib H37Y_2   format=F00778_. label="NA - Nothing taken and didn't seek treatment for fever/cough(2)";
  attrib H37Y_3   format=F00778_. label="NA - Nothing taken and didn't seek treatment for fever/cough(3)";
  attrib H37Y_4   format=F00778_. label="NA - Nothing taken and didn't seek treatment for fever/cough(4)";
  attrib H37Y_5   format=F00778_. label="NA - Nothing taken and didn't seek treatment for fever/cough(5)";
  attrib H37Y_6   format=F00778_. label="NA - Nothing taken and didn't seek treatment for fever/cough(6)";
  attrib H37Z_1   format=F00779_. label="NA - Don't know if or what was taken for fever(1)";
  attrib H37Z_2   format=F00779_. label="NA - Don't know if or what was taken for fever(2)";
  attrib H37Z_3   format=F00779_. label="NA - Don't know if or what was taken for fever(3)";
  attrib H37Z_4   format=F00779_. label="NA - Don't know if or what was taken for fever(4)";
  attrib H37Z_5   format=F00779_. label="NA - Don't know if or what was taken for fever(5)";
  attrib H37Z_6   format=F00779_. label="NA - Don't know if or what was taken for fever(6)";
  attrib H38_1    format=F00780_. label="NA - Diarrhea in last 2 weeks: amount offered to drink(1)";
  attrib H38_2    format=F00780_. label="NA - Diarrhea in last 2 weeks: amount offered to drink(2)";
  attrib H38_3    format=F00780_. label="NA - Diarrhea in last 2 weeks: amount offered to drink(3)";
  attrib H38_4    format=F00780_. label="NA - Diarrhea in last 2 weeks: amount offered to drink(4)";
  attrib H38_5    format=F00780_. label="NA - Diarrhea in last 2 weeks: amount offered to drink(5)";
  attrib H38_6    format=F00780_. label="NA - Diarrhea in last 2 weeks: amount offered to drink(6)";
  attrib H39_1    format=F00781_. label="NA - Diarrhea in last 2 weeks: amount offered to eat(1)";
  attrib H39_2    format=F00781_. label="NA - Diarrhea in last 2 weeks: amount offered to eat(2)";
  attrib H39_3    format=F00781_. label="NA - Diarrhea in last 2 weeks: amount offered to eat(3)";
  attrib H39_4    format=F00781_. label="NA - Diarrhea in last 2 weeks: amount offered to eat(4)";
  attrib H39_5    format=F00781_. label="NA - Diarrhea in last 2 weeks: amount offered to eat(5)";
  attrib H39_6    format=F00781_. label="NA - Diarrhea in last 2 weeks: amount offered to eat(6)";
  attrib H42_1    format=F00782_. label="NA - Taking iron pills, sprinkles or syrup(1)";
  attrib H42_2    format=F00782_. label="NA - Taking iron pills, sprinkles or syrup(2)";
  attrib H42_3    format=F00782_. label="NA - Taking iron pills, sprinkles or syrup(3)";
  attrib H42_4    format=F00782_. label="NA - Taking iron pills, sprinkles or syrup(4)";
  attrib H42_5    format=F00782_. label="NA - Taking iron pills, sprinkles or syrup(5)";
  attrib H42_6    format=F00782_. label="NA - Taking iron pills, sprinkles or syrup(6)";
  attrib H43_1    format=F00783_. label="NA - Drugs for intestinal parasites in last 6 months(1)";
  attrib H43_2    format=F00783_. label="NA - Drugs for intestinal parasites in last 6 months(2)";
  attrib H43_3    format=F00783_. label="NA - Drugs for intestinal parasites in last 6 months(3)";
  attrib H43_4    format=F00783_. label="NA - Drugs for intestinal parasites in last 6 months(4)";
  attrib H43_5    format=F00783_. label="NA - Drugs for intestinal parasites in last 6 months(5)";
  attrib H43_6    format=F00783_. label="NA - Drugs for intestinal parasites in last 6 months(6)";
  attrib H44A_1   format=F00784_. label="NA - Place first sought treatment for diarrhea(1)";
  attrib H44A_2   format=F00784_. label="NA - Place first sought treatment for diarrhea(2)";
  attrib H44A_3   format=F00784_. label="NA - Place first sought treatment for diarrhea(3)";
  attrib H44A_4   format=F00784_. label="NA - Place first sought treatment for diarrhea(4)";
  attrib H44A_5   format=F00784_. label="NA - Place first sought treatment for diarrhea(5)";
  attrib H44A_6   format=F00784_. label="NA - Place first sought treatment for diarrhea(6)";
  attrib H44B_1   format=F00785_. label="NA - Days after diarrhea sought advice or treatment(1)";
  attrib H44B_2   format=F00785_. label="NA - Days after diarrhea sought advice or treatment(2)";
  attrib H44B_3   format=F00785_. label="NA - Days after diarrhea sought advice or treatment(3)";
  attrib H44B_4   format=F00785_. label="NA - Days after diarrhea sought advice or treatment(4)";
  attrib H44B_5   format=F00785_. label="NA - Days after diarrhea sought advice or treatment(5)";
  attrib H44B_6   format=F00785_. label="NA - Days after diarrhea sought advice or treatment(6)";
  attrib H44C_1   format=F00786_. label="NA - Still has diarrhea(1)";
  attrib H44C_2   format=F00786_. label="NA - Still has diarrhea(2)";
  attrib H44C_3   format=F00786_. label="NA - Still has diarrhea(3)";
  attrib H44C_4   format=F00786_. label="NA - Still has diarrhea(4)";
  attrib H44C_5   format=F00786_. label="NA - Still has diarrhea(5)";
  attrib H44C_6   format=F00786_. label="NA - Still has diarrhea(6)";
  attrib H45_1    format=F00787_. label="NA - Times zinc was given(1)";
  attrib H45_2    format=F00787_. label="NA - Times zinc was given(2)";
  attrib H45_3    format=F00787_. label="NA - Times zinc was given(3)";
  attrib H45_4    format=F00787_. label="NA - Times zinc was given(4)";
  attrib H45_5    format=F00787_. label="NA - Times zinc was given(5)";
  attrib H45_6    format=F00787_. label="NA - Times zinc was given(6)";
  attrib H46A_1   format=F00788_. label="NA - Place first sought treatment for fever(1)";
  attrib H46A_2   format=F00788_. label="NA - Place first sought treatment for fever(2)";
  attrib H46A_3   format=F00788_. label="NA - Place first sought treatment for fever(3)";
  attrib H46A_4   format=F00788_. label="NA - Place first sought treatment for fever(4)";
  attrib H46A_5   format=F00788_. label="NA - Place first sought treatment for fever(5)";
  attrib H46A_6   format=F00788_. label="NA - Place first sought treatment for fever(6)";
  attrib H46B_1   format=F00789_. label="NA - Number of days after fever began sought advice or treatment(1)";
  attrib H46B_2   format=F00789_. label="NA - Number of days after fever began sought advice or treatment(2)";
  attrib H46B_3   format=F00789_. label="NA - Number of days after fever began sought advice or treatment(3)";
  attrib H46B_4   format=F00789_. label="NA - Number of days after fever began sought advice or treatment(4)";
  attrib H46B_5   format=F00789_. label="NA - Number of days after fever began sought advice or treatment(5)";
  attrib H46B_6   format=F00789_. label="NA - Number of days after fever began sought advice or treatment(6)";
  attrib H47_1    format=F00790_. label="NA - Blood taken from child's finger/heel for testing(1)";
  attrib H47_2    format=F00790_. label="NA - Blood taken from child's finger/heel for testing(2)";
  attrib H47_3    format=F00790_. label="NA - Blood taken from child's finger/heel for testing(3)";
  attrib H47_4    format=F00790_. label="NA - Blood taken from child's finger/heel for testing(4)";
  attrib H47_5    format=F00790_. label="NA - Blood taken from child's finger/heel for testing(5)";
  attrib H47_6    format=F00790_. label="NA - Blood taken from child's finger/heel for testing(6)";
  attrib HWIDX_1  label="NA - Index to Birth History(1)";
  attrib HWIDX_2  label="NA - Index to Birth History(2)";
  attrib HWIDX_3  label="NA - Index to Birth History(3)";
  attrib HWIDX_4  label="NA - Index to Birth History(4)";
  attrib HWIDX_5  label="NA - Index to Birth History(5)";
  attrib HWIDX_6  label="NA - Index to Birth History(6)";
  attrib HW1_1    label="NA - Child's age in months(1)";
  attrib HW1_2    label="NA - Child's age in months(2)";
  attrib HW1_3    label="NA - Child's age in months(3)";
  attrib HW1_4    label="NA - Child's age in months(4)";
  attrib HW1_5    label="NA - Child's age in months(5)";
  attrib HW1_6    label="NA - Child's age in months(6)";
  attrib HW2_1    format=F00791_. label="NA - Child's weight in kilograms (1 decimal)(1)";
  attrib HW2_2    format=F00791_. label="NA - Child's weight in kilograms (1 decimal)(2)";
  attrib HW2_3    format=F00791_. label="NA - Child's weight in kilograms (1 decimal)(3)";
  attrib HW2_4    format=F00791_. label="NA - Child's weight in kilograms (1 decimal)(4)";
  attrib HW2_5    format=F00791_. label="NA - Child's weight in kilograms (1 decimal)(5)";
  attrib HW2_6    format=F00791_. label="NA - Child's weight in kilograms (1 decimal)(6)";
  attrib HW3_1    format=F00792_. label="NA - Child's height in centimeters (1 decimal)(1)";
  attrib HW3_2    format=F00792_. label="NA - Child's height in centimeters (1 decimal)(2)";
  attrib HW3_3    format=F00792_. label="NA - Child's height in centimeters (1 decimal)(3)";
  attrib HW3_4    format=F00792_. label="NA - Child's height in centimeters (1 decimal)(4)";
  attrib HW3_5    format=F00792_. label="NA - Child's height in centimeters (1 decimal)(5)";
  attrib HW3_6    format=F00792_. label="NA - Child's height in centimeters (1 decimal)(6)";
  attrib HW4_1    format=F00793_. label="NA - Height/Age percentile(1)";
  attrib HW4_2    format=F00793_. label="NA - Height/Age percentile(2)";
  attrib HW4_3    format=F00793_. label="NA - Height/Age percentile(3)";
  attrib HW4_4    format=F00793_. label="NA - Height/Age percentile(4)";
  attrib HW4_5    format=F00793_. label="NA - Height/Age percentile(5)";
  attrib HW4_6    format=F00793_. label="NA - Height/Age percentile(6)";
  attrib HW5_1    format=F00794_. label="NA - Height/Age standard deviation(1)";
  attrib HW5_2    format=F00794_. label="NA - Height/Age standard deviation(2)";
  attrib HW5_3    format=F00794_. label="NA - Height/Age standard deviation(3)";
  attrib HW5_4    format=F00794_. label="NA - Height/Age standard deviation(4)";
  attrib HW5_5    format=F00794_. label="NA - Height/Age standard deviation(5)";
  attrib HW5_6    format=F00794_. label="NA - Height/Age standard deviation(6)";
  attrib HW6_1    format=F00795_. label="NA - Height/Age percent of ref. median(1)";
  attrib HW6_2    format=F00795_. label="NA - Height/Age percent of ref. median(2)";
  attrib HW6_3    format=F00795_. label="NA - Height/Age percent of ref. median(3)";
  attrib HW6_4    format=F00795_. label="NA - Height/Age percent of ref. median(4)";
  attrib HW6_5    format=F00795_. label="NA - Height/Age percent of ref. median(5)";
  attrib HW6_6    format=F00795_. label="NA - Height/Age percent of ref. median(6)";
  attrib HW7_1    format=F00796_. label="NA - Weight/Age percentile(1)";
  attrib HW7_2    format=F00796_. label="NA - Weight/Age percentile(2)";
  attrib HW7_3    format=F00796_. label="NA - Weight/Age percentile(3)";
  attrib HW7_4    format=F00796_. label="NA - Weight/Age percentile(4)";
  attrib HW7_5    format=F00796_. label="NA - Weight/Age percentile(5)";
  attrib HW7_6    format=F00796_. label="NA - Weight/Age percentile(6)";
  attrib HW8_1    format=F00797_. label="NA - Weight/Age standard deviation(1)";
  attrib HW8_2    format=F00797_. label="NA - Weight/Age standard deviation(2)";
  attrib HW8_3    format=F00797_. label="NA - Weight/Age standard deviation(3)";
  attrib HW8_4    format=F00797_. label="NA - Weight/Age standard deviation(4)";
  attrib HW8_5    format=F00797_. label="NA - Weight/Age standard deviation(5)";
  attrib HW8_6    format=F00797_. label="NA - Weight/Age standard deviation(6)";
  attrib HW9_1    format=F00798_. label="NA - Weight/Age percent of ref. median(1)";
  attrib HW9_2    format=F00798_. label="NA - Weight/Age percent of ref. median(2)";
  attrib HW9_3    format=F00798_. label="NA - Weight/Age percent of ref. median(3)";
  attrib HW9_4    format=F00798_. label="NA - Weight/Age percent of ref. median(4)";
  attrib HW9_5    format=F00798_. label="NA - Weight/Age percent of ref. median(5)";
  attrib HW9_6    format=F00798_. label="NA - Weight/Age percent of ref. median(6)";
  attrib HW10_1   format=F00799_. label="NA - Weight/Height percentile(1)";
  attrib HW10_2   format=F00799_. label="NA - Weight/Height percentile(2)";
  attrib HW10_3   format=F00799_. label="NA - Weight/Height percentile(3)";
  attrib HW10_4   format=F00799_. label="NA - Weight/Height percentile(4)";
  attrib HW10_5   format=F00799_. label="NA - Weight/Height percentile(5)";
  attrib HW10_6   format=F00799_. label="NA - Weight/Height percentile(6)";
  attrib HW11_1   format=F00800_. label="NA - Weight/Height standard deviation(1)";
  attrib HW11_2   format=F00800_. label="NA - Weight/Height standard deviation(2)";
  attrib HW11_3   format=F00800_. label="NA - Weight/Height standard deviation(3)";
  attrib HW11_4   format=F00800_. label="NA - Weight/Height standard deviation(4)";
  attrib HW11_5   format=F00800_. label="NA - Weight/Height standard deviation(5)";
  attrib HW11_6   format=F00800_. label="NA - Weight/Height standard deviation(6)";
  attrib HW12_1   format=F00801_. label="NA - Weight/Height percent of ref. median(1)";
  attrib HW12_2   format=F00801_. label="NA - Weight/Height percent of ref. median(2)";
  attrib HW12_3   format=F00801_. label="NA - Weight/Height percent of ref. median(3)";
  attrib HW12_4   format=F00801_. label="NA - Weight/Height percent of ref. median(4)";
  attrib HW12_5   format=F00801_. label="NA - Weight/Height percent of ref. median(5)";
  attrib HW12_6   format=F00801_. label="NA - Weight/Height percent of ref. median(6)";
  attrib HW13_1   format=F00802_. label="NA - Result of measurement - height/weight(1)";
  attrib HW13_2   format=F00802_. label="NA - Result of measurement - height/weight(2)";
  attrib HW13_3   format=F00802_. label="NA - Result of measurement - height/weight(3)";
  attrib HW13_4   format=F00802_. label="NA - Result of measurement - height/weight(4)";
  attrib HW13_5   format=F00802_. label="NA - Result of measurement - height/weight(5)";
  attrib HW13_6   format=F00802_. label="NA - Result of measurement - height/weight(6)";
  attrib HW15_1   format=F00803_. label="NA - Height: lying or standing(1)";
  attrib HW15_2   format=F00803_. label="NA - Height: lying or standing(2)";
  attrib HW15_3   format=F00803_. label="NA - Height: lying or standing(3)";
  attrib HW15_4   format=F00803_. label="NA - Height: lying or standing(4)";
  attrib HW15_5   format=F00803_. label="NA - Height: lying or standing(5)";
  attrib HW15_6   format=F00803_. label="NA - Height: lying or standing(6)";
  attrib HW16_1   format=F00804_. label="NA - Day of birth of child(1)";
  attrib HW16_2   format=F00804_. label="NA - Day of birth of child(2)";
  attrib HW16_3   format=F00804_. label="NA - Day of birth of child(3)";
  attrib HW16_4   format=F00804_. label="NA - Day of birth of child(4)";
  attrib HW16_5   format=F00804_. label="NA - Day of birth of child(5)";
  attrib HW16_6   format=F00804_. label="NA - Day of birth of child(6)";
  attrib HW17_1   label="NA - Date measured (day)(1)";
  attrib HW17_2   label="NA - Date measured (day)(2)";
  attrib HW17_3   label="NA - Date measured (day)(3)";
  attrib HW17_4   label="NA - Date measured (day)(4)";
  attrib HW17_5   label="NA - Date measured (day)(5)";
  attrib HW17_6   label="NA - Date measured (day)(6)";
  attrib HW18_1   label="NA - Date measured (month)(1)";
  attrib HW18_2   label="NA - Date measured (month)(2)";
  attrib HW18_3   label="NA - Date measured (month)(3)";
  attrib HW18_4   label="NA - Date measured (month)(4)";
  attrib HW18_5   label="NA - Date measured (month)(5)";
  attrib HW18_6   label="NA - Date measured (month)(6)";
  attrib HW19_1   label="NA - Date measured (year)(1)";
  attrib HW19_2   label="NA - Date measured (year)(2)";
  attrib HW19_3   label="NA - Date measured (year)(3)";
  attrib HW19_4   label="NA - Date measured (year)(4)";
  attrib HW19_5   label="NA - Date measured (year)(5)";
  attrib HW19_6   label="NA - Date measured (year)(6)";
  attrib HW51_1   format=F00805_. label="NA - Line number of parent/caretaker(1)";
  attrib HW51_2   format=F00805_. label="NA - Line number of parent/caretaker(2)";
  attrib HW51_3   format=F00805_. label="NA - Line number of parent/caretaker(3)";
  attrib HW51_4   format=F00805_. label="NA - Line number of parent/caretaker(4)";
  attrib HW51_5   format=F00805_. label="NA - Line number of parent/caretaker(5)";
  attrib HW51_6   format=F00805_. label="NA - Line number of parent/caretaker(6)";
  attrib HW52_1   format=F00806_. label="NA - Read consent statement - hemoglobin(1)";
  attrib HW52_2   format=F00806_. label="NA - Read consent statement - hemoglobin(2)";
  attrib HW52_3   format=F00806_. label="NA - Read consent statement - hemoglobin(3)";
  attrib HW52_4   format=F00806_. label="NA - Read consent statement - hemoglobin(4)";
  attrib HW52_5   format=F00806_. label="NA - Read consent statement - hemoglobin(5)";
  attrib HW52_6   format=F00806_. label="NA - Read consent statement - hemoglobin(6)";
  attrib HW53_1   format=F00807_. label="NA - Hemoglobin level (g/dl - 1 decimal)(1)";
  attrib HW53_2   format=F00807_. label="NA - Hemoglobin level (g/dl - 1 decimal)(2)";
  attrib HW53_3   format=F00807_. label="NA - Hemoglobin level (g/dl - 1 decimal)(3)";
  attrib HW53_4   format=F00807_. label="NA - Hemoglobin level (g/dl - 1 decimal)(4)";
  attrib HW53_5   format=F00807_. label="NA - Hemoglobin level (g/dl - 1 decimal)(5)";
  attrib HW53_6   format=F00807_. label="NA - Hemoglobin level (g/dl - 1 decimal)(6)";
  attrib HW55_1   format=F00808_. label="NA - Result of measurement - hemoglobin(1)";
  attrib HW55_2   format=F00808_. label="NA - Result of measurement - hemoglobin(2)";
  attrib HW55_3   format=F00808_. label="NA - Result of measurement - hemoglobin(3)";
  attrib HW55_4   format=F00808_. label="NA - Result of measurement - hemoglobin(4)";
  attrib HW55_5   format=F00808_. label="NA - Result of measurement - hemoglobin(5)";
  attrib HW55_6   format=F00808_. label="NA - Result of measurement - hemoglobin(6)";
  attrib HW56_1   format=F00809_. label="NA - Hemoglobin level adjusted for altitude (g/dl - 1 decimal)(1)";
  attrib HW56_2   format=F00809_. label="NA - Hemoglobin level adjusted for altitude (g/dl - 1 decimal)(2)";
  attrib HW56_3   format=F00809_. label="NA - Hemoglobin level adjusted for altitude (g/dl - 1 decimal)(3)";
  attrib HW56_4   format=F00809_. label="NA - Hemoglobin level adjusted for altitude (g/dl - 1 decimal)(4)";
  attrib HW56_5   format=F00809_. label="NA - Hemoglobin level adjusted for altitude (g/dl - 1 decimal)(5)";
  attrib HW56_6   format=F00809_. label="NA - Hemoglobin level adjusted for altitude (g/dl - 1 decimal)(6)";
  attrib HW57_1   format=F00810_. label="NA - Anemia level(1)";
  attrib HW57_2   format=F00810_. label="NA - Anemia level(2)";
  attrib HW57_3   format=F00810_. label="NA - Anemia level(3)";
  attrib HW57_4   format=F00810_. label="NA - Anemia level(4)";
  attrib HW57_5   format=F00810_. label="NA - Anemia level(5)";
  attrib HW57_6   format=F00810_. label="NA - Anemia level(6)";
  attrib HW58_1   format=F00811_. label="NA - Agrees to referral - anemia(1)";
  attrib HW58_2   format=F00811_. label="NA - Agrees to referral - anemia(2)";
  attrib HW58_3   format=F00811_. label="NA - Agrees to referral - anemia(3)";
  attrib HW58_4   format=F00811_. label="NA - Agrees to referral - anemia(4)";
  attrib HW58_5   format=F00811_. label="NA - Agrees to referral - anemia(5)";
  attrib HW58_6   format=F00811_. label="NA - Agrees to referral - anemia(6)";
  attrib HW70_1   format=F00812_. label="NA - Height/Age standard deviation (new WHO)(1)";
  attrib HW70_2   format=F00812_. label="NA - Height/Age standard deviation (new WHO)(2)";
  attrib HW70_3   format=F00812_. label="NA - Height/Age standard deviation (new WHO)(3)";
  attrib HW70_4   format=F00812_. label="NA - Height/Age standard deviation (new WHO)(4)";
  attrib HW70_5   format=F00812_. label="NA - Height/Age standard deviation (new WHO)(5)";
  attrib HW70_6   format=F00812_. label="NA - Height/Age standard deviation (new WHO)(6)";
  attrib HW71_1   format=F00813_. label="NA - Weight/Age standard deviation (new WHO)(1)";
  attrib HW71_2   format=F00813_. label="NA - Weight/Age standard deviation (new WHO)(2)";
  attrib HW71_3   format=F00813_. label="NA - Weight/Age standard deviation (new WHO)(3)";
  attrib HW71_4   format=F00813_. label="NA - Weight/Age standard deviation (new WHO)(4)";
  attrib HW71_5   format=F00813_. label="NA - Weight/Age standard deviation (new WHO)(5)";
  attrib HW71_6   format=F00813_. label="NA - Weight/Age standard deviation (new WHO)(6)";
  attrib HW72_1   format=F00814_. label="NA - Weight/Height standard deviation (new WHO)(1)";
  attrib HW72_2   format=F00814_. label="NA - Weight/Height standard deviation (new WHO)(2)";
  attrib HW72_3   format=F00814_. label="NA - Weight/Height standard deviation (new WHO)(3)";
  attrib HW72_4   format=F00814_. label="NA - Weight/Height standard deviation (new WHO)(4)";
  attrib HW72_5   format=F00814_. label="NA - Weight/Height standard deviation (new WHO)(5)";
  attrib HW72_6   format=F00814_. label="NA - Weight/Height standard deviation (new WHO)(6)";
  attrib HW73_1   format=F00815_. label="NA - BMI standard deviation (new WHO)(1)";
  attrib HW73_2   format=F00815_. label="NA - BMI standard deviation (new WHO)(2)";
  attrib HW73_3   format=F00815_. label="NA - BMI standard deviation (new WHO)(3)";
  attrib HW73_4   format=F00815_. label="NA - BMI standard deviation (new WHO)(4)";
  attrib HW73_5   format=F00815_. label="NA - BMI standard deviation (new WHO)(5)";
  attrib HW73_6   format=F00815_. label="NA - BMI standard deviation (new WHO)(6)";
  attrib V501     format=F00816_. label="Current marital status";
  attrib V502     format=F00817_. label="Currently/formerly/never in union";
  attrib V503     format=F00818_. label="Number of unions";
  attrib V504     format=F00819_. label="Currently residing with husband/partner";
  attrib V505     format=F00820_. label="NA - Number of other wives";
  attrib V506     format=F00821_. label="NA - Respondent's rank among wives";
  attrib V507     label="Month of first cohabitation";
  attrib V508     label="Year of first cohabitation";
  attrib V509     label="Date of first cohabitation (CMC)";
  attrib V510     format=F00822_. label="Completeness of date information in V509";
  attrib V511     label="Age at first cohabitation";
  attrib V512     label="Years since first cohabitation";
  attrib V513     format=F00823_. label="Cohabitation duration (grouped)";
  attrib V525     format=F00824_. label="Age at first sex";
  attrib V527     format=F00825_. label="Time since last sex";
  attrib V528     format=F00826_. label="Time since last sex (in days)";
  attrib V529     format=F00827_. label="Time since last sex (in months)";
  attrib V530     format=F00828_. label="Flag for V529";
  attrib V531     format=F00829_. label="Age at first sex (imputed)";
  attrib V532     format=F00830_. label="Flag for V531";
  attrib V535     format=F00831_. label="Ever been married or in union";
  attrib V536     format=F00832_. label="Recent sexual activity";
  attrib V537     format=F00833_. label="Months of abstinence";
  attrib V538     format=F00834_. label="NA - How previous marriage or union ended";
  attrib V539     format=F00835_. label="NA - Person who received most of late husband's property";
  attrib V540     format=F00836_. label="NA - Respondent received any of late husband's assets or valuables";
  attrib V541     format=F00837_. label="Intends to wait until marriage to have sex";
  attrib V602     format=F00838_. label="Fertility preference";
  attrib V603     format=F00839_. label="Preferred waiting time for birth of a/another child";
  attrib V604     format=F00840_. label="Preferred waiting time for birth of a/another child (grouped)";
  attrib V605     format=F00841_. label="Desire for more children";
  attrib V613     format=F00842_. label="Ideal number of children";
  attrib V614     format=F00843_. label="Ideal number of children (grouped)";
  attrib V616     format=F00844_. label="Time for future birth";
  attrib V621     format=F00845_. label="Husband's desire for children";
  attrib V623     format=F00846_. label="Exposure";
  attrib V624     format=F00847_. label="Unmet need";
  attrib V625     format=F00848_. label="Exposure (definition 2)";
  attrib V626     format=F00849_. label="Unmet need (definition 2)";
  attrib V625A    format=F00850_. label="Exposure to need for contraception (definition 3)";
  attrib V626A    format=F00851_. label="Unmet need for contraception (definition 3)";
  attrib V627     format=F00852_. label="Ideal number of boys";
  attrib V628     format=F00853_. label="Ideal number of girls";
  attrib V629     format=F00854_. label="Ideal number of either sex";
  attrib V631     format=F00855_. label="Problem if became pregnant";
  attrib V632     format=F00856_. label="Decision maker for using contraception";
  attrib V632A    format=F00857_. label="NA - Decision maker for not using contraception";
  attrib V633A    format=F00858_. label="Reason for not having sex: husband has STI";
  attrib V633B    format=F00859_. label="Reason for not having sex: husband has other women";
  attrib V633C    format=F00860_. label="Reason for not having sex: recent birth";
  attrib V633D    format=F00861_. label="Reason for not having sex: tired, not in mood";
  attrib V633E    format=F00862_. label="NA - Reason for not having sex: CS";
  attrib V633F    format=F00863_. label="NA - Reason for not having sex: CS";
  attrib V633G    format=F00864_. label="NA - Reason for not having sex: CS";
  attrib V634     format=F00865_. label="Husband knows that respondent is using contraception";
  attrib V701     format=F00866_. label="NA - Husband/partner's education level";
  attrib V702     format=F00867_. label="NA - Husband/partner's highest year of education (at level in V701)";
  attrib V704     format=F00868_. label="NA - Husband/partner's occupation";
  attrib V704A    format=F00869_. label="NA - Husband/partner work in last 7 days/12 months";
  attrib V705     format=F00870_. label="NA - Husband/partner's occupation (grouped)";
  attrib V714     format=F00871_. label="Respondent currently working";
  attrib V714A    format=F00872_. label="NA - Respondent has a job, but currently absent";
  attrib V715     format=F00873_. label="NA - Husband/partner's total number of years of education";
  attrib V716     format=F00874_. label="Respondent's occupation";
  attrib V717     format=F00875_. label="Respondent's occupation (grouped)";
  attrib V719     format=F00876_. label="NA - Respondent works for family, others, self";
  attrib V721     format=F00877_. label="NA - Respondent works at home or away";
  attrib V729     format=F00878_. label="NA - Husband/partner's educational attainment";
  attrib V730     format=F00879_. label="NA - Husband/partner's age";
  attrib V731     format=F00880_. label="Respondent worked in last 12 months";
  attrib V732     format=F00881_. label="Respondent employed all year/seasonal";
  attrib V739     format=F00882_. label="Person who usually decides how to spend respondent's earnings";
  attrib V740     format=F00883_. label="NA - Type of land where respondent works";
  attrib V741     format=F00884_. label="Type of earnings from respondent's work";
  attrib V743A    format=F00885_. label="Person who usually decides on respondent's health care";
  attrib V743B    format=F00886_. label="Person who usually decides on large household purchases";
  attrib V743C    format=F00887_. label="Person who usually decides on household purchases for daily needs";
  attrib V743D    format=F00888_. label="Person who usually decides on visits to family or relatives";
  attrib V743E    format=F00889_. label="Person who usually decides on food to be cooked each day";
  attrib V743F    format=F00890_. label="Person who usually decides what to do with money husband earns";
  attrib V744A    format=F00891_. label="Beating justified if wife goes out without telling husband";
  attrib V744B    format=F00892_. label="Beating justified if wife neglects the children";
  attrib V744C    format=F00893_. label="Beating justified if wife argues with husband";
  attrib V744D    format=F00894_. label="Beating justified if wife refuses to have sex with husband";
  attrib V744E    format=F00895_. label="Beating justified if wife burns the food";
  attrib V745A    format=F00896_. label="Owns a house alone or jointly";
  attrib V745B    format=F00897_. label="Owns land alone or jointly";
  attrib V745C    format=F00898_. label="NA - Title deed on house own by respondent";
  attrib V745D    format=F00899_. label="NA - Title deed on land own by respondent";
  attrib V746     format=F00900_. label="Respondent earns more than husband/partner";
  attrib V750     format=F00901_. label="Ever heard of a Sexually Transmitted Infection (STI)";
  attrib V751     format=F00902_. label="Ever heard of AIDS";
  attrib V754BP   format=F00903_. label="Reduce risk of getting HIV: do not have sex at all";
  attrib V754CP   format=F00904_. label="Reduce risk of getting HIV: always use condoms during sex";
  attrib V754DP   format=F00905_. label="Reduce risk of getting HIV: have 1 sex partner only, who has no other partners";
  attrib V754JP   format=F00906_. label="Can get HIV from mosquito bites";
  attrib V754WP   format=F00907_. label="Can get HIV by sharing food with person who has AIDS";
  attrib V756     format=F00908_. label="A healthy looking person can have HIV";
  attrib V761     format=F00909_. label="Condom used during last sex with most recent partner";
  attrib V761B    format=F00910_. label="Condom used during last sex with 2nd to most recent partner";
  attrib V761C    format=F00911_. label="Condom used during last sex with 3rd to most recent partner";
  attrib V762     format=F00912_. label="NA - Source of condoms used for last sex";
  attrib V762A    format=F00913_. label="NA - Brand of condom used for last sex";
  attrib V762AA   format=F00914_. label="Source for condoms: government hospital";
  attrib V762AB   format=F00915_. label="NA - Source for condoms: CS public";
  attrib V762AC   format=F00916_. label="NA - Source for condoms: CS public";
  attrib V762AD   format=F00917_. label="NA - Source for condoms: CS public";
  attrib V762AE   format=F00918_. label="NA - Source for condoms: CS public";
  attrib V762AF   format=F00919_. label="NA - Source for condoms: CS public";
  attrib V762AG   format=F00920_. label="NA - Source for condoms: CS public";
  attrib V762AH   format=F00921_. label="NA - Source for condoms: CS public";
  attrib V762AI   format=F00922_. label="NA - Source for condoms: CS public";
  attrib V762AJ   format=F00923_. label="Source for condoms: private hospital/clinic";
  attrib V762AK   format=F00924_. label="Source for condoms: pharmacy";
  attrib V762AL   format=F00925_. label="Source for condoms: private doctor";
  attrib V762AM   format=F00926_. label="Source for condoms: EPS health center";
  attrib V762AN   format=F00927_. label="Source for condoms: PROFAMILIA";
  attrib V762AO   format=F00928_. label="NA - Source for condoms: CS private";
  attrib V762AP   format=F00929_. label="NA - Source for condoms: CS private";
  attrib V762AQ   format=F00930_. label="NA - Source for condoms: CS private";
  attrib V762AR   format=F00931_. label="NA - Source for condoms: CS private";
  attrib V762AS   format=F00932_. label="Source for condoms: shop/supermarket";
  attrib V762AT   format=F00933_. label="Source for condoms: public places/motels";
  attrib V762AU   format=F00934_. label="NA - Source for condoms: CS other";
  attrib V762AV   format=F00935_. label="NA - Source for condoms: CS other";
  attrib V762AW   format=F00936_. label="NA - Source for condoms: CS other";
  attrib V762AX   format=F00937_. label="Source for condoms: other";
  attrib V762AZ   format=F00938_. label="Don't know any source for condoms";
  attrib V762BA   format=F00939_. label="NA - Source for female condoms: government hospital";
  attrib V762BB   format=F00940_. label="NA - Source for female condoms: CS public";
  attrib V762BC   format=F00941_. label="NA - Source for female condoms: CS public";
  attrib V762BD   format=F00942_. label="NA - Source for female condoms: CS public";
  attrib V762BE   format=F00943_. label="NA - Source for female condoms: CS public";
  attrib V762BF   format=F00944_. label="NA - Source for female condoms: CS public";
  attrib V762BG   format=F00945_. label="NA - Source for female condoms: CS public";
  attrib V762BH   format=F00946_. label="NA - Source for female condoms: CS public";
  attrib V762BI   format=F00947_. label="NA - Source for female condoms: CS public";
  attrib V762BJ   format=F00948_. label="NA - Source for female condoms: private hospital/clinic";
  attrib V762BK   format=F00949_. label="NA - Source for female condoms: pharmacy";
  attrib V762BL   format=F00950_. label="NA - Source for female condoms: private doctor";
  attrib V762BM   format=F00951_. label="NA - Source for female condoms: CS private";
  attrib V762BN   format=F00952_. label="NA - Source for female condoms: CS private";
  attrib V762BO   format=F00953_. label="NA - Source for female condoms: CS private";
  attrib V762BP   format=F00954_. label="NA - Source for female condoms: CS private";
  attrib V762BQ   format=F00955_. label="NA - Source for female condoms: CS private";
  attrib V762BR   format=F00956_. label="NA - Source for female condoms: CS private";
  attrib V762BS   format=F00957_. label="NA - Source for female condoms: CS other";
  attrib V762BT   format=F00958_. label="NA - Source for female condoms: CS other";
  attrib V762BU   format=F00959_. label="NA - Source for female condoms: CS other";
  attrib V762BV   format=F00960_. label="NA - Source for female condoms: CS other";
  attrib V762BW   format=F00961_. label="NA - Source for female condoms: CS other";
  attrib V762BX   format=F00962_. label="NA - Source for female condoms: other";
  attrib V762BZ   format=F00963_. label="NA - Don't know any source for female condoms";
  attrib V763A    format=F00964_. label="Had any STI in last 12 months";
  attrib V763B    format=F00965_. label="Had genital sore/ulcer in last 12 months";
  attrib V763C    format=F00966_. label="Had genital discharge in last 12 months";
  attrib V763D    format=F00967_. label="NA - Had CS STI in last 12 months";
  attrib V763E    format=F00968_. label="NA - Had CS STI in last 12 months";
  attrib V763F    format=F00969_. label="NA - Had CS STI in last 12 months";
  attrib V763G    format=F00970_. label="NA - Had CS STI in last 12 months";
  attrib V766A    format=F00971_. label="Number of sex partners, excluding spouse, in last 12 months";
  attrib V766B    format=F00972_. label="Number of sex partners, including spouse, in last 12 months";
  attrib V767A    format=F00973_. label="Relationship with most recent sex partner";
  attrib V767B    format=F00974_. label="Relationship with 2nd to most recent sex partner";
  attrib V767C    format=F00975_. label="Relationship with 3rd to most recent sex partner";
  attrib V768A    format=F00976_. label="NA - Length of time knows last partner";
  attrib V768B    format=F00977_. label="NA - Length of time knows other partner (1)";
  attrib V768C    format=F00978_. label="NA - Length of time knows other partner (2)";
  attrib V769     format=F00979_. label="Can get a condom";
  attrib V769A    format=F00980_. label="NA - Can get a female condom";
  attrib V770     format=F00981_. label="Sought advice/treatment for last STI infection";
  attrib V770A    format=F00982_. label="Sought STI advice/treatment from: government hospital";
  attrib V770B    format=F00983_. label="NA - Sought STI advice/treatment from: CS public";
  attrib V770C    format=F00984_. label="NA - Sought STI advice/treatment from: CS public";
  attrib V770D    format=F00985_. label="NA - Sought STI advice/treatment from: CS public";
  attrib V770E    format=F00986_. label="NA - Sought STI advice/treatment from: CS public";
  attrib V770F    format=F00987_. label="NA - Sought STI advice/treatment from: CS public";
  attrib V770G    format=F00988_. label="NA - Sought STI advice/treatment from: CS public";
  attrib V770H    format=F00989_. label="NA - Sought STI advice/treatment from: CS public";
  attrib V770I    format=F00990_. label="NA - Sought STI advice/treatment from: CS public";
  attrib V770J    format=F00991_. label="NA - Sought STI advice/treatment from: CS public";
  attrib V770K    format=F00992_. label="Sought STI advice/treatment from: private hospital/clinic/doctor";
  attrib V770L    format=F00993_. label="Sought STI advice/treatment from: EPS health center";
  attrib V770M    format=F00994_. label="Sought STI advice/treatment from: private doctor";
  attrib V770N    format=F00995_. label="Sought STI advice/treatment from: PROFAMILIA";
  attrib V770O    format=F00996_. label="Sought STI advice/treatment from: drug store/pharmacy";
  attrib V770P    format=F00997_. label="NA - Sought STI advice/treatment from: CS private";
  attrib V770Q    format=F00998_. label="NA - Sought STI advice/treatment from: CS private";
  attrib V770R    format=F00999_. label="NA - Sought STI advice/treatment from: CS private";
  attrib V770S    format=F01000_. label="NA - Sought STI advice/treatment from: CS private";
  attrib V770T    format=F01001_. label="Sought STI advice/treatment from: traditional healer";
  attrib V770U    format=F01002_. label="NA - Sought STI advice/treatment from: CS other";
  attrib V770V    format=F01003_. label="NA - Sought STI advice/treatment from: CS other";
  attrib V770W    format=F01004_. label="NA - Sought STI advice/treatment from: CS other";
  attrib V770X    format=F01005_. label="Sought STI advice/treatment from: other";
  attrib V774A    format=F01006_. label="HIV transmitted during pregnancy";
  attrib V774B    format=F01007_. label="HIV transmitted during delivery";
  attrib V774C    format=F01008_. label="HIV transmitted by breastfeeding";
  attrib V775     format=F01009_. label="NA - Knows someone who has, or is suspected of having, HIV";
  attrib V777     format=F01010_. label="Would want HIV infection in family to remain secret";
  attrib V777A    format=F01011_. label="NA - Would be ashamed if someone in the family had HIV";
  attrib V778     format=F01012_. label="Willing to care for relative with AIDS";
  attrib V779     format=F01013_. label="A female teacher infected with HIV, but is not sick, should be allowed to continue teaching";
  attrib V780     format=F01014_. label="Children should be taught about condoms to avoid AIDS";
  attrib V781     format=F01015_. label="Ever been tested for HIV";
  attrib V783     format=F01016_. label="Know a place to get HIV test";
  attrib V784A    format=F01017_. label="Place for HIV test: government hospital";
  attrib V784B    format=F01018_. label="NA - Place for HIV test: CS public";
  attrib V784C    format=F01019_. label="NA - Place for HIV test: CS public";
  attrib V784D    format=F01020_. label="NA - Place for HIV test: CS public";
  attrib V784E    format=F01021_. label="NA - Place for HIV test: CS public";
  attrib V784F    format=F01022_. label="NA - Place for HIV test: CS public";
  attrib V784G    format=F01023_. label="NA - Place for HIV test: CS public";
  attrib V784H    format=F01024_. label="NA - Place for HIV test: CS public";
  attrib V784I    format=F01025_. label="NA - Place for HIV test: CS public";
  attrib V784J    format=F01026_. label="NA - Place for HIV test: CS public";
  attrib V784K    format=F01027_. label="Place for HIV test: private hospital/clinic/doctor";
  attrib V784L    format=F01028_. label="Place for HIV test: EPS health center";
  attrib V784M    format=F01029_. label="Place for HIV test: private doctor";
  attrib V784N    format=F01030_. label="Place for HIV test: PROFAMILIA";
  attrib V784O    format=F01031_. label="NA - Place for HIV test: CS private";
  attrib V784P    format=F01032_. label="NA - Place for HIV test: CS private";
  attrib V784Q    format=F01033_. label="NA - Place for HIV test: CS private";
  attrib V784R    format=F01034_. label="NA - Place for HIV test: CS private";
  attrib V784S    format=F01035_. label="NA - Place for HIV test: CS private";
  attrib V784T    format=F01036_. label="NA - Place for HIV test: CS other";
  attrib V784U    format=F01037_. label="NA - Place for HIV test: CS other";
  attrib V784V    format=F01038_. label="NA - Place for HIV test: CS other";
  attrib V784X    format=F01039_. label="Place for HIV test: other";
  attrib V785     format=F01040_. label="Heard about other STIs";
  attrib V791A    format=F01041_. label="NA - Last 12 months had sex in return for gifts, cash or other";
  attrib V820     format=F01042_. label="Condom used at first sex";
  attrib V821A    format=F01043_. label="Most recent sex partner younger, the same age or older";
  attrib V821B    format=F01044_. label="2nd to most recent sex partner younger, the same age or older";
  attrib V821C    format=F01045_. label="3rd to most recent sex partner younger, the same age or older";
  attrib V822     format=F01046_. label="Wife justified asking husband to use condom if he has STI";
  attrib V823     format=F01047_. label="NA - Can get HIV by witchcraft or supernatural means";
  attrib V824     format=F01048_. label="Drugs to avoid HIV transmission to baby during pregnancy";
  attrib V825     format=F01049_. label="Would buy vegetables from vendor with HIV";
  attrib V826     format=F01050_. label="Last time tested for HIV";
  attrib V826A    format=F01051_. label="NA - Months ago most recent HIV test";
  attrib V827     format=F01052_. label="NA - Last HIV test: on your own, offered or required";
  attrib V828     format=F01053_. label="Received result from last HIV test";
  attrib V829     format=F01054_. label="Place where last HIV test was taken";
  attrib V830     format=F01055_. label="Age of first sex partner";
  attrib V831     format=F01056_. label="First sex partner younger, same age or older";
  attrib V832B    format=F01057_. label="Time since last sex with 2nd to most recent partner";
  attrib V832C    format=F01058_. label="Time since last sex with 3rd to most recent partner";
  attrib V833A    format=F01059_. label="Used condom every time had sex with most recent partner in last 12 months";
  attrib V833B    format=F01060_. label="Used condom every time had sex with 2nd to most recent partner in last 12 months";
  attrib V833C    format=F01061_. label="Used condom every time had sex with 3rd to most recent partner in last 12 months";
  attrib V834A    format=F01062_. label="Age of most recent partner";
  attrib V834B    format=F01063_. label="Age of 2nd to most recent partner";
  attrib V834C    format=F01064_. label="Age of 3rd to most recent partner";
  attrib V835A    format=F01065_. label="NA - Alcohol consumption at last sex with most recent partner";
  attrib V835B    format=F01066_. label="NA - Alcohol consumption at last sex with 2nd to most recent partner";
  attrib V835C    format=F01067_. label="NA - Alcohol consumption at last sex with 3rd to most recent partner";
  attrib V836     format=F01068_. label="Total lifetime number of sex partners";
  attrib V837     format=F01069_. label="Heard of drugs to help HIV infected people live longer";
  attrib V838A    format=F01070_. label="During antenatal visit talked about: HIV transmitted mother to child";
  attrib V838B    format=F01071_. label="During antenatal visit talked about: things to do to prevent getting HIV";
  attrib V838C    format=F01072_. label="During antenatal visit talked about: getting tested for HIV";
  attrib V839     format=F01073_. label="Offered HIV test as part of antenatal visit";
  attrib V839A    format=F01074_. label="NA - Offered HIV test between the time went for delivery and before baby was born";
  attrib V840     format=F01075_. label="Tested for HIV as part of antenatal visit";
  attrib V840A    format=F01076_. label="NA - Tested for HIV between the time went for delivery and before baby was born";
  attrib V841     format=F01077_. label="Got results of HIV test as part of antenatal visit";
  attrib V841A    format=F01078_. label="NA - Got results of HIV test when tested before baby was born";
  attrib V842     format=F01079_. label="Place were HIV test was taken as part of antenatal visit";
  attrib V843     format=F01080_. label="Tested for HIV since antenatal visit test";
  attrib V844     format=F01081_. label="NA - Knows someone denied health services for having, or suspected of having, HIV; in last 12 months";
  attrib V845     format=F01082_. label="NA - Knows someone denied social events for having, or suspected of having, HIV; in last 12 months";
  attrib V846     format=F01083_. label="NA - Knows someone verbally abused for having, or suspected of having, HIV; in last 12 months";
  attrib V847     format=F01084_. label="NA - People with HIV should be ashamed of themselves";
  attrib V848     format=F01085_. label="NA - People with HIV should be blamed for bringing disease to community";
  attrib V849     format=F01086_. label="NA - Children 12-14 should be taught to wait for sex until marriage";
  attrib V850A    format=F01087_. label="NA - Respondent can refuse sex";
  attrib V850B    format=F01088_. label="NA - Respondent can ask partner to use a condom";
  attrib V851A    format=F01089_. label="NA - Young men should wait for sex until marriage";
  attrib V851B    format=F01090_. label="NA - Most young men wait for sex until marriage";
  attrib V851C    format=F01091_. label="NA - Unmarried sexually active men should only having sex with one partner";
  attrib V851D    format=F01092_. label="NA - Most unmarried sexually active men have only one partner";
  attrib V851E    format=F01093_. label="NA - Married men should only have sex with their wives";
  attrib V851F    format=F01094_. label="NA - Most married men only have sex with their wives";
  attrib V851G    format=F01095_. label="NA - Young women should wait for sex until marriage";
  attrib V851H    format=F01096_. label="NA - Most young women wait for sex until marriage";
  attrib V851I    format=F01097_. label="NA - Unmarried sexually active women should have only one partner";
  attrib V851J    format=F01098_. label="NA - Most unmarried sexually active women have only one partner";
  attrib V851K    format=F01099_. label="NA - Married women should only have sex with their husbands";
  attrib V851L    format=F01100_. label="NA - Most married women only have sex with their husbands";
  attrib V852A    format=F01101_. label="How long ago first had sex with most recent partner";
  attrib V852B    format=F01102_. label="How long ago first had sex with 2nd most recent partner";
  attrib V852C    format=F01103_. label="How long ago first had sex with 3rd most recent partner";
  attrib V853A    format=F01104_. label="Times in last 12 months had sex with most recent partner";
  attrib V853B    format=F01105_. label="Times in last 12 months had sex with 2nd most recent partner";
  attrib V853C    format=F01106_. label="Times in last 12 months had sex with 3rd most recent partner";
  attrib V854A    format=F01107_. label="Point concurrent sexual partners";
  attrib V854B    format=F01108_. label="Cumulative concurrent sexual partners";
  attrib V855     format=F01109_. label="NA - Received counseling after tested for AIDS during antenatal care";
  attrib V856     format=F01110_. label="NA - Knowledge and use of HIV test kits";
  attrib V857A    format=F01111_. label="NA - Children with HIV should be allowed to attend school with children without HIV";
  attrib V857B    format=F01112_. label="NA - People hesitate to take HIV test because reaction of other people if positive";
  attrib V857C    format=F01113_. label="NA - People talk badly about people with or believed to have HIV";
  attrib V857D    format=F01114_. label="NA - People with or believed to have HIV lose respect from other people";
  attrib V858     format=F01115_. label="NA - Would be afraid to get HIV from contact with saliva from infected person";
  attrib V801     label="Time interview started (hhmm - 24 hour clock)";
  attrib V802     label="Time interview ended (hhmm - 24 hour clock)";
  attrib V803     format=F01116_. label="Length of interview in minutes";
  attrib V804     label="Number of visits";
  attrib V805     label="Interviewer identification";
  attrib V806     label="NA - Keyer identification";
  attrib V811     format=F01117_. label="NA - Presence of children < 10 for 'Wife beating justified' questions";
  attrib V812     format=F01118_. label="NA - Presence of husband for 'Wife beating justified' questions";
  attrib V813     format=F01119_. label="NA - Presence of other males for 'Wife beating justified' questions";
  attrib V814     format=F01120_. label="NA - Presence of other females for 'Wife beating justified' questions";
  attrib V815A    format=F01121_. label="NA - Presence of children < 10 during sexual activity section";
  attrib V815B    format=F01122_. label="NA - Presence of male adults during sexual activity section";
  attrib V815C    format=F01123_. label="NA - Presence of female adults during sexual activity section";
  attrib VCOL_1   format=F01124_. label="Calendar column number(1)";
  attrib VCOL_2   format=F01124_. label="Calendar column number(2)";
  attrib VCOL_3   format=F01124_. label="Calendar column number(3)";
  attrib VCOL_4   format=F01124_. label="Calendar column number(4)";
  attrib VCOL_5   format=F01124_. label="Calendar column number(5)";
  attrib VCOL_6   format=F01124_. label="Calendar column number(6)";
  attrib VCOL_7   format=F01124_. label="Calendar column number(7)";
  attrib VCOL_8   format=F01124_. label="Calendar column number(8)";
  attrib VCOL_9   format=F01124_. label="Calendar column number(9)";
  attrib VCAL_1   length=$80 format=$F01125_. label="Calendar(1)";
  attrib VCAL_2   length=$80 format=$F01125_. label="Calendar(2)";
  attrib VCAL_3   length=$80 format=$F01125_. label="Calendar(3)";
  attrib VCAL_4   length=$80 format=$F01125_. label="Calendar(4)";
  attrib VCAL_5   length=$80 format=$F01125_. label="Calendar(5)";
  attrib VCAL_6   length=$80 format=$F01125_. label="Calendar(6)";
  attrib VCAL_7   length=$80 format=$F01125_. label="Calendar(7)";
  attrib VCAL_8   length=$80 format=$F01125_. label="Calendar(8)";
  attrib VCAL_9   length=$80 format=$F01125_. label="Calendar(9)";
  attrib MMIDX_01 label="Index to Maternal Mortality(1)";
  attrib MMIDX_02 label="Index to Maternal Mortality(2)";
  attrib MMIDX_03 label="Index to Maternal Mortality(3)";
  attrib MMIDX_04 label="Index to Maternal Mortality(4)";
  attrib MMIDX_05 label="Index to Maternal Mortality(5)";
  attrib MMIDX_06 label="Index to Maternal Mortality(6)";
  attrib MMIDX_07 label="Index to Maternal Mortality(7)";
  attrib MMIDX_08 label="Index to Maternal Mortality(8)";
  attrib MMIDX_09 label="Index to Maternal Mortality(9)";
  attrib MMIDX_10 label="Index to Maternal Mortality(10)";
  attrib MMIDX_11 label="Index to Maternal Mortality(11)";
  attrib MMIDX_12 label="Index to Maternal Mortality(12)";
  attrib MMIDX_13 label="Index to Maternal Mortality(13)";
  attrib MMIDX_14 label="Index to Maternal Mortality(14)";
  attrib MMIDX_15 label="Index to Maternal Mortality(15)";
  attrib MMIDX_16 label="Index to Maternal Mortality(16)";
  attrib MMIDX_17 label="Index to Maternal Mortality(17)";
  attrib MMIDX_18 label="Index to Maternal Mortality(18)";
  attrib MMIDX_19 label="Index to Maternal Mortality(19)";
  attrib MMIDX_20 label="Index to Maternal Mortality(20)";
  attrib MM1_01   format=F01126_. label="Sex of sibling(1)";
  attrib MM1_02   format=F01126_. label="Sex of sibling(2)";
  attrib MM1_03   format=F01126_. label="Sex of sibling(3)";
  attrib MM1_04   format=F01126_. label="Sex of sibling(4)";
  attrib MM1_05   format=F01126_. label="Sex of sibling(5)";
  attrib MM1_06   format=F01126_. label="Sex of sibling(6)";
  attrib MM1_07   format=F01126_. label="Sex of sibling(7)";
  attrib MM1_08   format=F01126_. label="Sex of sibling(8)";
  attrib MM1_09   format=F01126_. label="Sex of sibling(9)";
  attrib MM1_10   format=F01126_. label="Sex of sibling(10)";
  attrib MM1_11   format=F01126_. label="Sex of sibling(11)";
  attrib MM1_12   format=F01126_. label="Sex of sibling(12)";
  attrib MM1_13   format=F01126_. label="Sex of sibling(13)";
  attrib MM1_14   format=F01126_. label="Sex of sibling(14)";
  attrib MM1_15   format=F01126_. label="Sex of sibling(15)";
  attrib MM1_16   format=F01126_. label="Sex of sibling(16)";
  attrib MM1_17   format=F01126_. label="Sex of sibling(17)";
  attrib MM1_18   format=F01126_. label="Sex of sibling(18)";
  attrib MM1_19   format=F01126_. label="Sex of sibling(19)";
  attrib MM1_20   format=F01126_. label="Sex of sibling(20)";
  attrib MM2_01   format=F01127_. label="Survival status of sibling(1)";
  attrib MM2_02   format=F01127_. label="Survival status of sibling(2)";
  attrib MM2_03   format=F01127_. label="Survival status of sibling(3)";
  attrib MM2_04   format=F01127_. label="Survival status of sibling(4)";
  attrib MM2_05   format=F01127_. label="Survival status of sibling(5)";
  attrib MM2_06   format=F01127_. label="Survival status of sibling(6)";
  attrib MM2_07   format=F01127_. label="Survival status of sibling(7)";
  attrib MM2_08   format=F01127_. label="Survival status of sibling(8)";
  attrib MM2_09   format=F01127_. label="Survival status of sibling(9)";
  attrib MM2_10   format=F01127_. label="Survival status of sibling(10)";
  attrib MM2_11   format=F01127_. label="Survival status of sibling(11)";
  attrib MM2_12   format=F01127_. label="Survival status of sibling(12)";
  attrib MM2_13   format=F01127_. label="Survival status of sibling(13)";
  attrib MM2_14   format=F01127_. label="Survival status of sibling(14)";
  attrib MM2_15   format=F01127_. label="Survival status of sibling(15)";
  attrib MM2_16   format=F01127_. label="Survival status of sibling(16)";
  attrib MM2_17   format=F01127_. label="Survival status of sibling(17)";
  attrib MM2_18   format=F01127_. label="Survival status of sibling(18)";
  attrib MM2_19   format=F01127_. label="Survival status of sibling(19)";
  attrib MM2_20   format=F01127_. label="Survival status of sibling(20)";
  attrib MM3_01   format=F01128_. label="Sibling's current age(1)";
  attrib MM3_02   format=F01128_. label="Sibling's current age(2)";
  attrib MM3_03   format=F01128_. label="Sibling's current age(3)";
  attrib MM3_04   format=F01128_. label="Sibling's current age(4)";
  attrib MM3_05   format=F01128_. label="Sibling's current age(5)";
  attrib MM3_06   format=F01128_. label="Sibling's current age(6)";
  attrib MM3_07   format=F01128_. label="Sibling's current age(7)";
  attrib MM3_08   format=F01128_. label="Sibling's current age(8)";
  attrib MM3_09   format=F01128_. label="Sibling's current age(9)";
  attrib MM3_10   format=F01128_. label="Sibling's current age(10)";
  attrib MM3_11   format=F01128_. label="Sibling's current age(11)";
  attrib MM3_12   format=F01128_. label="Sibling's current age(12)";
  attrib MM3_13   format=F01128_. label="Sibling's current age(13)";
  attrib MM3_14   format=F01128_. label="Sibling's current age(14)";
  attrib MM3_15   format=F01128_. label="Sibling's current age(15)";
  attrib MM3_16   format=F01128_. label="Sibling's current age(16)";
  attrib MM3_17   format=F01128_. label="Sibling's current age(17)";
  attrib MM3_18   format=F01128_. label="Sibling's current age(18)";
  attrib MM3_19   format=F01128_. label="Sibling's current age(19)";
  attrib MM3_20   format=F01128_. label="Sibling's current age(20)";
  attrib MM4_01   label="Sibling's date of birth (CMC)(1)";
  attrib MM4_02   label="Sibling's date of birth (CMC)(2)";
  attrib MM4_03   label="Sibling's date of birth (CMC)(3)";
  attrib MM4_04   label="Sibling's date of birth (CMC)(4)";
  attrib MM4_05   label="Sibling's date of birth (CMC)(5)";
  attrib MM4_06   label="Sibling's date of birth (CMC)(6)";
  attrib MM4_07   label="Sibling's date of birth (CMC)(7)";
  attrib MM4_08   label="Sibling's date of birth (CMC)(8)";
  attrib MM4_09   label="Sibling's date of birth (CMC)(9)";
  attrib MM4_10   label="Sibling's date of birth (CMC)(10)";
  attrib MM4_11   label="Sibling's date of birth (CMC)(11)";
  attrib MM4_12   label="Sibling's date of birth (CMC)(12)";
  attrib MM4_13   label="Sibling's date of birth (CMC)(13)";
  attrib MM4_14   label="Sibling's date of birth (CMC)(14)";
  attrib MM4_15   label="Sibling's date of birth (CMC)(15)";
  attrib MM4_16   label="Sibling's date of birth (CMC)(16)";
  attrib MM4_17   label="Sibling's date of birth (CMC)(17)";
  attrib MM4_18   label="Sibling's date of birth (CMC)(18)";
  attrib MM4_19   label="Sibling's date of birth (CMC)(19)";
  attrib MM4_20   label="Sibling's date of birth (CMC)(20)";
  attrib MM5_01   format=F01129_. label="NA - Sibling's marital status(1)";
  attrib MM5_02   format=F01129_. label="NA - Sibling's marital status(2)";
  attrib MM5_03   format=F01129_. label="NA - Sibling's marital status(3)";
  attrib MM5_04   format=F01129_. label="NA - Sibling's marital status(4)";
  attrib MM5_05   format=F01129_. label="NA - Sibling's marital status(5)";
  attrib MM5_06   format=F01129_. label="NA - Sibling's marital status(6)";
  attrib MM5_07   format=F01129_. label="NA - Sibling's marital status(7)";
  attrib MM5_08   format=F01129_. label="NA - Sibling's marital status(8)";
  attrib MM5_09   format=F01129_. label="NA - Sibling's marital status(9)";
  attrib MM5_10   format=F01129_. label="NA - Sibling's marital status(10)";
  attrib MM5_11   format=F01129_. label="NA - Sibling's marital status(11)";
  attrib MM5_12   format=F01129_. label="NA - Sibling's marital status(12)";
  attrib MM5_13   format=F01129_. label="NA - Sibling's marital status(13)";
  attrib MM5_14   format=F01129_. label="NA - Sibling's marital status(14)";
  attrib MM5_15   format=F01129_. label="NA - Sibling's marital status(15)";
  attrib MM5_16   format=F01129_. label="NA - Sibling's marital status(16)";
  attrib MM5_17   format=F01129_. label="NA - Sibling's marital status(17)";
  attrib MM5_18   format=F01129_. label="NA - Sibling's marital status(18)";
  attrib MM5_19   format=F01129_. label="NA - Sibling's marital status(19)";
  attrib MM5_20   format=F01129_. label="NA - Sibling's marital status(20)";
  attrib MM6_01   format=F01130_. label="Years since sibling died(1)";
  attrib MM6_02   format=F01130_. label="Years since sibling died(2)";
  attrib MM6_03   format=F01130_. label="Years since sibling died(3)";
  attrib MM6_04   format=F01130_. label="Years since sibling died(4)";
  attrib MM6_05   format=F01130_. label="Years since sibling died(5)";
  attrib MM6_06   format=F01130_. label="Years since sibling died(6)";
  attrib MM6_07   format=F01130_. label="Years since sibling died(7)";
  attrib MM6_08   format=F01130_. label="Years since sibling died(8)";
  attrib MM6_09   format=F01130_. label="Years since sibling died(9)";
  attrib MM6_10   format=F01130_. label="Years since sibling died(10)";
  attrib MM6_11   format=F01130_. label="Years since sibling died(11)";
  attrib MM6_12   format=F01130_. label="Years since sibling died(12)";
  attrib MM6_13   format=F01130_. label="Years since sibling died(13)";
  attrib MM6_14   format=F01130_. label="Years since sibling died(14)";
  attrib MM6_15   format=F01130_. label="Years since sibling died(15)";
  attrib MM6_16   format=F01130_. label="Years since sibling died(16)";
  attrib MM6_17   format=F01130_. label="Years since sibling died(17)";
  attrib MM6_18   format=F01130_. label="Years since sibling died(18)";
  attrib MM6_19   format=F01130_. label="Years since sibling died(19)";
  attrib MM6_20   format=F01130_. label="Years since sibling died(20)";
  attrib MM7_01   format=F01131_. label="Sibling's age at death(1)";
  attrib MM7_02   format=F01131_. label="Sibling's age at death(2)";
  attrib MM7_03   format=F01131_. label="Sibling's age at death(3)";
  attrib MM7_04   format=F01131_. label="Sibling's age at death(4)";
  attrib MM7_05   format=F01131_. label="Sibling's age at death(5)";
  attrib MM7_06   format=F01131_. label="Sibling's age at death(6)";
  attrib MM7_07   format=F01131_. label="Sibling's age at death(7)";
  attrib MM7_08   format=F01131_. label="Sibling's age at death(8)";
  attrib MM7_09   format=F01131_. label="Sibling's age at death(9)";
  attrib MM7_10   format=F01131_. label="Sibling's age at death(10)";
  attrib MM7_11   format=F01131_. label="Sibling's age at death(11)";
  attrib MM7_12   format=F01131_. label="Sibling's age at death(12)";
  attrib MM7_13   format=F01131_. label="Sibling's age at death(13)";
  attrib MM7_14   format=F01131_. label="Sibling's age at death(14)";
  attrib MM7_15   format=F01131_. label="Sibling's age at death(15)";
  attrib MM7_16   format=F01131_. label="Sibling's age at death(16)";
  attrib MM7_17   format=F01131_. label="Sibling's age at death(17)";
  attrib MM7_18   format=F01131_. label="Sibling's age at death(18)";
  attrib MM7_19   format=F01131_. label="Sibling's age at death(19)";
  attrib MM7_20   format=F01131_. label="Sibling's age at death(20)";
  attrib MM8_01   label="Date of death of sibling (CMC)(1)";
  attrib MM8_02   label="Date of death of sibling (CMC)(2)";
  attrib MM8_03   label="Date of death of sibling (CMC)(3)";
  attrib MM8_04   label="Date of death of sibling (CMC)(4)";
  attrib MM8_05   label="Date of death of sibling (CMC)(5)";
  attrib MM8_06   label="Date of death of sibling (CMC)(6)";
  attrib MM8_07   label="Date of death of sibling (CMC)(7)";
  attrib MM8_08   label="Date of death of sibling (CMC)(8)";
  attrib MM8_09   label="Date of death of sibling (CMC)(9)";
  attrib MM8_10   label="Date of death of sibling (CMC)(10)";
  attrib MM8_11   label="Date of death of sibling (CMC)(11)";
  attrib MM8_12   label="Date of death of sibling (CMC)(12)";
  attrib MM8_13   label="Date of death of sibling (CMC)(13)";
  attrib MM8_14   label="Date of death of sibling (CMC)(14)";
  attrib MM8_15   label="Date of death of sibling (CMC)(15)";
  attrib MM8_16   label="Date of death of sibling (CMC)(16)";
  attrib MM8_17   label="Date of death of sibling (CMC)(17)";
  attrib MM8_18   label="Date of death of sibling (CMC)(18)";
  attrib MM8_19   label="Date of death of sibling (CMC)(19)";
  attrib MM8_20   label="Date of death of sibling (CMC)(20)";
  attrib MM9_01   format=F01132_. label="Sibling's death and pregnancy(1)";
  attrib MM9_02   format=F01132_. label="Sibling's death and pregnancy(2)";
  attrib MM9_03   format=F01132_. label="Sibling's death and pregnancy(3)";
  attrib MM9_04   format=F01132_. label="Sibling's death and pregnancy(4)";
  attrib MM9_05   format=F01132_. label="Sibling's death and pregnancy(5)";
  attrib MM9_06   format=F01132_. label="Sibling's death and pregnancy(6)";
  attrib MM9_07   format=F01132_. label="Sibling's death and pregnancy(7)";
  attrib MM9_08   format=F01132_. label="Sibling's death and pregnancy(8)";
  attrib MM9_09   format=F01132_. label="Sibling's death and pregnancy(9)";
  attrib MM9_10   format=F01132_. label="Sibling's death and pregnancy(10)";
  attrib MM9_11   format=F01132_. label="Sibling's death and pregnancy(11)";
  attrib MM9_12   format=F01132_. label="Sibling's death and pregnancy(12)";
  attrib MM9_13   format=F01132_. label="Sibling's death and pregnancy(13)";
  attrib MM9_14   format=F01132_. label="Sibling's death and pregnancy(14)";
  attrib MM9_15   format=F01132_. label="Sibling's death and pregnancy(15)";
  attrib MM9_16   format=F01132_. label="Sibling's death and pregnancy(16)";
  attrib MM9_17   format=F01132_. label="Sibling's death and pregnancy(17)";
  attrib MM9_18   format=F01132_. label="Sibling's death and pregnancy(18)";
  attrib MM9_19   format=F01132_. label="Sibling's death and pregnancy(19)";
  attrib MM9_20   format=F01132_. label="Sibling's death and pregnancy(20)";
  attrib MM10_01  format=F01133_. label="NA - Death and a pregnancy are related(1)";
  attrib MM10_02  format=F01133_. label="NA - Death and a pregnancy are related(2)";
  attrib MM10_03  format=F01133_. label="NA - Death and a pregnancy are related(3)";
  attrib MM10_04  format=F01133_. label="NA - Death and a pregnancy are related(4)";
  attrib MM10_05  format=F01133_. label="NA - Death and a pregnancy are related(5)";
  attrib MM10_06  format=F01133_. label="NA - Death and a pregnancy are related(6)";
  attrib MM10_07  format=F01133_. label="NA - Death and a pregnancy are related(7)";
  attrib MM10_08  format=F01133_. label="NA - Death and a pregnancy are related(8)";
  attrib MM10_09  format=F01133_. label="NA - Death and a pregnancy are related(9)";
  attrib MM10_10  format=F01133_. label="NA - Death and a pregnancy are related(10)";
  attrib MM10_11  format=F01133_. label="NA - Death and a pregnancy are related(11)";
  attrib MM10_12  format=F01133_. label="NA - Death and a pregnancy are related(12)";
  attrib MM10_13  format=F01133_. label="NA - Death and a pregnancy are related(13)";
  attrib MM10_14  format=F01133_. label="NA - Death and a pregnancy are related(14)";
  attrib MM10_15  format=F01133_. label="NA - Death and a pregnancy are related(15)";
  attrib MM10_16  format=F01133_. label="NA - Death and a pregnancy are related(16)";
  attrib MM10_17  format=F01133_. label="NA - Death and a pregnancy are related(17)";
  attrib MM10_18  format=F01133_. label="NA - Death and a pregnancy are related(18)";
  attrib MM10_19  format=F01133_. label="NA - Death and a pregnancy are related(19)";
  attrib MM10_20  format=F01133_. label="NA - Death and a pregnancy are related(20)";
  attrib MM11_01  format=F01134_. label="NA - Cause of death sibling(1)";
  attrib MM11_02  format=F01134_. label="NA - Cause of death sibling(2)";
  attrib MM11_03  format=F01134_. label="NA - Cause of death sibling(3)";
  attrib MM11_04  format=F01134_. label="NA - Cause of death sibling(4)";
  attrib MM11_05  format=F01134_. label="NA - Cause of death sibling(5)";
  attrib MM11_06  format=F01134_. label="NA - Cause of death sibling(6)";
  attrib MM11_07  format=F01134_. label="NA - Cause of death sibling(7)";
  attrib MM11_08  format=F01134_. label="NA - Cause of death sibling(8)";
  attrib MM11_09  format=F01134_. label="NA - Cause of death sibling(9)";
  attrib MM11_10  format=F01134_. label="NA - Cause of death sibling(10)";
  attrib MM11_11  format=F01134_. label="NA - Cause of death sibling(11)";
  attrib MM11_12  format=F01134_. label="NA - Cause of death sibling(12)";
  attrib MM11_13  format=F01134_. label="NA - Cause of death sibling(13)";
  attrib MM11_14  format=F01134_. label="NA - Cause of death sibling(14)";
  attrib MM11_15  format=F01134_. label="NA - Cause of death sibling(15)";
  attrib MM11_16  format=F01134_. label="NA - Cause of death sibling(16)";
  attrib MM11_17  format=F01134_. label="NA - Cause of death sibling(17)";
  attrib MM11_18  format=F01134_. label="NA - Cause of death sibling(18)";
  attrib MM11_19  format=F01134_. label="NA - Cause of death sibling(19)";
  attrib MM11_20  format=F01134_. label="NA - Cause of death sibling(20)";
  attrib MM12_01  format=F01135_. label="NA - Amount of time between sibling's delivery and death(1)";
  attrib MM12_02  format=F01135_. label="NA - Amount of time between sibling's delivery and death(2)";
  attrib MM12_03  format=F01135_. label="NA - Amount of time between sibling's delivery and death(3)";
  attrib MM12_04  format=F01135_. label="NA - Amount of time between sibling's delivery and death(4)";
  attrib MM12_05  format=F01135_. label="NA - Amount of time between sibling's delivery and death(5)";
  attrib MM12_06  format=F01135_. label="NA - Amount of time between sibling's delivery and death(6)";
  attrib MM12_07  format=F01135_. label="NA - Amount of time between sibling's delivery and death(7)";
  attrib MM12_08  format=F01135_. label="NA - Amount of time between sibling's delivery and death(8)";
  attrib MM12_09  format=F01135_. label="NA - Amount of time between sibling's delivery and death(9)";
  attrib MM12_10  format=F01135_. label="NA - Amount of time between sibling's delivery and death(10)";
  attrib MM12_11  format=F01135_. label="NA - Amount of time between sibling's delivery and death(11)";
  attrib MM12_12  format=F01135_. label="NA - Amount of time between sibling's delivery and death(12)";
  attrib MM12_13  format=F01135_. label="NA - Amount of time between sibling's delivery and death(13)";
  attrib MM12_14  format=F01135_. label="NA - Amount of time between sibling's delivery and death(14)";
  attrib MM12_15  format=F01135_. label="NA - Amount of time between sibling's delivery and death(15)";
  attrib MM12_16  format=F01135_. label="NA - Amount of time between sibling's delivery and death(16)";
  attrib MM12_17  format=F01135_. label="NA - Amount of time between sibling's delivery and death(17)";
  attrib MM12_18  format=F01135_. label="NA - Amount of time between sibling's delivery and death(18)";
  attrib MM12_19  format=F01135_. label="NA - Amount of time between sibling's delivery and death(19)";
  attrib MM12_20  format=F01135_. label="NA - Amount of time between sibling's delivery and death(20)";
  attrib MM13_01  format=F01136_. label="NA - Place of sibling's death(1)";
  attrib MM13_02  format=F01136_. label="NA - Place of sibling's death(2)";
  attrib MM13_03  format=F01136_. label="NA - Place of sibling's death(3)";
  attrib MM13_04  format=F01136_. label="NA - Place of sibling's death(4)";
  attrib MM13_05  format=F01136_. label="NA - Place of sibling's death(5)";
  attrib MM13_06  format=F01136_. label="NA - Place of sibling's death(6)";
  attrib MM13_07  format=F01136_. label="NA - Place of sibling's death(7)";
  attrib MM13_08  format=F01136_. label="NA - Place of sibling's death(8)";
  attrib MM13_09  format=F01136_. label="NA - Place of sibling's death(9)";
  attrib MM13_10  format=F01136_. label="NA - Place of sibling's death(10)";
  attrib MM13_11  format=F01136_. label="NA - Place of sibling's death(11)";
  attrib MM13_12  format=F01136_. label="NA - Place of sibling's death(12)";
  attrib MM13_13  format=F01136_. label="NA - Place of sibling's death(13)";
  attrib MM13_14  format=F01136_. label="NA - Place of sibling's death(14)";
  attrib MM13_15  format=F01136_. label="NA - Place of sibling's death(15)";
  attrib MM13_16  format=F01136_. label="NA - Place of sibling's death(16)";
  attrib MM13_17  format=F01136_. label="NA - Place of sibling's death(17)";
  attrib MM13_18  format=F01136_. label="NA - Place of sibling's death(18)";
  attrib MM13_19  format=F01136_. label="NA - Place of sibling's death(19)";
  attrib MM13_20  format=F01136_. label="NA - Place of sibling's death(20)";
  attrib MM14_01  label="Number of sibling's children(1)";
  attrib MM14_02  label="Number of sibling's children(2)";
  attrib MM14_03  label="Number of sibling's children(3)";
  attrib MM14_04  label="Number of sibling's children(4)";
  attrib MM14_05  label="Number of sibling's children(5)";
  attrib MM14_06  label="Number of sibling's children(6)";
  attrib MM14_07  label="Number of sibling's children(7)";
  attrib MM14_08  label="Number of sibling's children(8)";
  attrib MM14_09  label="Number of sibling's children(9)";
  attrib MM14_10  label="Number of sibling's children(10)";
  attrib MM14_11  label="Number of sibling's children(11)";
  attrib MM14_12  label="Number of sibling's children(12)";
  attrib MM14_13  label="Number of sibling's children(13)";
  attrib MM14_14  label="Number of sibling's children(14)";
  attrib MM14_15  label="Number of sibling's children(15)";
  attrib MM14_16  label="Number of sibling's children(16)";
  attrib MM14_17  label="Number of sibling's children(17)";
  attrib MM14_18  label="Number of sibling's children(18)";
  attrib MM14_19  label="Number of sibling's children(19)";
  attrib MM14_20  label="Number of sibling's children(20)";
  attrib MM15_01  format=F01137_. label="NA - Sibling's year of death(1)";
  attrib MM15_02  format=F01137_. label="NA - Sibling's year of death(2)";
  attrib MM15_03  format=F01137_. label="NA - Sibling's year of death(3)";
  attrib MM15_04  format=F01137_. label="NA - Sibling's year of death(4)";
  attrib MM15_05  format=F01137_. label="NA - Sibling's year of death(5)";
  attrib MM15_06  format=F01137_. label="NA - Sibling's year of death(6)";
  attrib MM15_07  format=F01137_. label="NA - Sibling's year of death(7)";
  attrib MM15_08  format=F01137_. label="NA - Sibling's year of death(8)";
  attrib MM15_09  format=F01137_. label="NA - Sibling's year of death(9)";
  attrib MM15_10  format=F01137_. label="NA - Sibling's year of death(10)";
  attrib MM15_11  format=F01137_. label="NA - Sibling's year of death(11)";
  attrib MM15_12  format=F01137_. label="NA - Sibling's year of death(12)";
  attrib MM15_13  format=F01137_. label="NA - Sibling's year of death(13)";
  attrib MM15_14  format=F01137_. label="NA - Sibling's year of death(14)";
  attrib MM15_15  format=F01137_. label="NA - Sibling's year of death(15)";
  attrib MM15_16  format=F01137_. label="NA - Sibling's year of death(16)";
  attrib MM15_17  format=F01137_. label="NA - Sibling's year of death(17)";
  attrib MM15_18  format=F01137_. label="NA - Sibling's year of death(18)";
  attrib MM15_19  format=F01137_. label="NA - Sibling's year of death(19)";
  attrib MM15_20  format=F01137_. label="NA - Sibling's year of death(20)";
  attrib MM16_01  format=F01138_. label="NA - Sibling's death due to violence or accident(1)";
  attrib MM16_02  format=F01138_. label="NA - Sibling's death due to violence or accident(2)";
  attrib MM16_03  format=F01138_. label="NA - Sibling's death due to violence or accident(3)";
  attrib MM16_04  format=F01138_. label="NA - Sibling's death due to violence or accident(4)";
  attrib MM16_05  format=F01138_. label="NA - Sibling's death due to violence or accident(5)";
  attrib MM16_06  format=F01138_. label="NA - Sibling's death due to violence or accident(6)";
  attrib MM16_07  format=F01138_. label="NA - Sibling's death due to violence or accident(7)";
  attrib MM16_08  format=F01138_. label="NA - Sibling's death due to violence or accident(8)";
  attrib MM16_09  format=F01138_. label="NA - Sibling's death due to violence or accident(9)";
  attrib MM16_10  format=F01138_. label="NA - Sibling's death due to violence or accident(10)";
  attrib MM16_11  format=F01138_. label="NA - Sibling's death due to violence or accident(11)";
  attrib MM16_12  format=F01138_. label="NA - Sibling's death due to violence or accident(12)";
  attrib MM16_13  format=F01138_. label="NA - Sibling's death due to violence or accident(13)";
  attrib MM16_14  format=F01138_. label="NA - Sibling's death due to violence or accident(14)";
  attrib MM16_15  format=F01138_. label="NA - Sibling's death due to violence or accident(15)";
  attrib MM16_16  format=F01138_. label="NA - Sibling's death due to violence or accident(16)";
  attrib MM16_17  format=F01138_. label="NA - Sibling's death due to violence or accident(17)";
  attrib MM16_18  format=F01138_. label="NA - Sibling's death due to violence or accident(18)";
  attrib MM16_19  format=F01138_. label="NA - Sibling's death due to violence or accident(19)";
  attrib MM16_20  format=F01138_. label="NA - Sibling's death due to violence or accident(20)";
  attrib MMC1     format=F01139_. label="Number of siblings of respondent";
  attrib MMC2     label="Number of siblings born before respondent's birth";
  attrib MMC3     format=F01140_. label="NA - Sister's reporting***";
  attrib MMC4_1   label="NA - Sister's line number from Household Schedule***(1)";
  attrib MMC4_2   label="NA - Sister's line number from Household Schedule***(2)";
  attrib MMC4_3   label="NA - Sister's line number from Household Schedule***(3)";
  attrib MMC4_4   label="NA - Sister's line number from Household Schedule***(4)";
  attrib MMC4_5   label="NA - Sister's line number from Household Schedule***(5)";
  attrib MMC4_6   label="NA - Sister's line number from Household Schedule***(6)";
  attrib MMC5     format=F01141_. label="Cut off age for Maternal Mortality collection (in questionnaire)";
  attrib D005     label="Weight for Domestic Violence (6 decimals)";
  attrib D101A    format=F01142_. label="Husband/partner jealous if respondent talks with other men";
  attrib D101B    format=F01143_. label="Husband/partner accuses respondent of unfaithfulness";
  attrib D101C    format=F01144_. label="Husband/partner does not permit respondent to meet female friends";
  attrib D101D    format=F01145_. label="Husband/partner tries to limit respondent's contact with family";
  attrib D101E    format=F01146_. label="Husband/partner insists on knowing where respondent is";
  attrib D101F    format=F01147_. label="Husband/partner doesn't trust respondent with money";
  attrib D101G    format=F01148_. label="Partner ignores/don't address her";
  attrib D101H    format=F01149_. label="Hasn't request opinion for family/social gatherings";
  attrib D101I    format=F01150_. label="Hasn't request opinion on important family matters";
  attrib D101J    format=F01151_. label="NA - CS control issue";
  attrib D102     label="Number of control issues answered 'Yes'  (D101x = 1)***";
  attrib D103A    format=F01152_. label="NA - Ever been humiliated by husband/partner";
  attrib D103B    format=F01153_. label="NA - Ever been threatened with harm by husband/partner";
  attrib D103C    format=F01154_. label="Ever been insulted or made to feel bad by husband/partner";
  attrib D103D    format=F01155_. label="Has threaten to leave her";
  attrib D103E    format=F01156_. label="Has threaten to take away children";
  attrib D103F    format=F01157_. label="NA - Ever CS emotional abuse by husband/partner";
  attrib D104     format=F01158_. label="NA - Experienced any emotional violence (D103x series)***";
  attrib D105A    format=F01159_. label="Ever been pushed, shook or had something thrown by husband/partner";
  attrib D105B    format=F01160_. label="Ever been slapped by husband/partner";
  attrib D105C    format=F01161_. label="Ever been punched with fist or hit by something harmful by husband/partner";
  attrib D105D    format=F01162_. label="Ever been kicked or dragged by husband/partner";
  attrib D105E    format=F01163_. label="Ever been strangled or burnt by husband/partner";
  attrib D105F    format=F01164_. label="Ever been threatened with knife/gun or other weapon by husband/partner";
  attrib D105G    format=F01165_. label="Ever been attacked with knife/gun or other weapon by husband/partner";
  attrib D105H    format=F01166_. label="Ever been physically forced into unwanted sex by husband/partner";
  attrib D105I    format=F01167_. label="NA - Ever been forced into other unwanted sexual acts by husband/partner";
  attrib D105J    format=F01168_. label="NA - Ever had arm twisted or hair pulled by husband/partner";
  attrib D105K    format=F01169_. label="NA - Ever been physically forced to perform sexual acts respondent didn't want to";
  attrib D105L    format=F01170_. label="NA - Ever CS physical violence by husband/partner";
  attrib D105M    format=F01171_. label="NA - Ever CS physical violence by husband/partner";
  attrib D105N    format=F01172_. label="NA - Ever CS physical violence by husband/partner";
  attrib D106     format=F01173_. label="Experienced any less severe violence (D105A-C,J) by husband/partner";
  attrib D107     format=F01174_. label="Experienced any severe violence (D105D-F) by husband/partner";
  attrib D108     format=F01175_. label="Experienced any sexual violence (D105H-I,K) by husband/partner";
  attrib D109     format=F01176_. label="Timing of first event (from D105 series) in years after marriage";
  attrib D110A    format=F01177_. label="Ever had bruises because of husband/partner's actions";
  attrib D110B    format=F01178_. label="NA - Ever had eye injuries, sprains, dislocations or burns because of husband/partner's actions";
  attrib D110C    format=F01179_. label="NA - Ever went to health facility because of husband/partner's actions";
  attrib D110D    format=F01180_. label="Ever had wounds, broken bones, broken teeth or other serious injury because of husband/partner's actions";
  attrib D110E    format=F01181_. label="Got sick";
  attrib D110F    format=F01182_. label="Got pregnant with a termination";
  attrib D110G    format=F01183_. label="Lost limbs or body organs that affected body functions";
  attrib D110H    format=F01184_. label="Got suicide desires";
  attrib D111     format=F01185_. label="Experienced any of listed husband/partner's actions (D110x series)***";
  attrib D112     format=F01186_. label="Respondent ever physically hurt husband/partner when he was not hurting her";
  attrib D112A    format=F01187_. label="Frequency of physically hurting husband/partner in last 12 months";
  attrib D113     format=F01188_. label="NA - Husband/partner drinks alcohol";
  attrib D114     format=F01189_. label="NA - Frequency of husband/partner being drunk";
  attrib D115B    format=F01190_. label="Mother: Person who has ever physically hurt respondent";
  attrib D115C    format=F01191_. label="NA - Father: Person who has ever physically hurt respondent";
  attrib D115D    format=F01192_. label="Daughter: Person who has ever physically hurt respondent";
  attrib D115E    format=F01193_. label="NA - Son: Person who has ever physically hurt respondent";
  attrib D115F    format=F01194_. label="NA - Sister/brother: Person who has ever physically hurt respondent";
  attrib D115G    format=F01195_. label="Other relative: Person who has ever physically hurt respondent";
  attrib D115H    format=F01196_. label="NA - Other female family: Person who has ever physically hurt respondent";
  attrib D115I    format=F01197_. label="NA - Other male family: Person who has ever physically hurt respondent";
  attrib D115J    format=F01198_. label="NA - Former partner: Person who has ever physically hurt respondent";
  attrib D115K    format=F01199_. label="Current boyfriend: Person who has ever physically hurt respondent";
  attrib D115L    format=F01200_. label="Former boyfriend: Person who has ever physically hurt respondent";
  attrib D115M    format=F01201_. label="Step-mother: Person who has ever physically hurt respondent";
  attrib D115N    format=F01202_. label="NA - Step-father: Person who has ever physically hurt respondent";
  attrib D115O    format=F01203_. label="NA - Mother-in-law: Person who has ever physically hurt respondent";
  attrib D115P    format=F01204_. label="NA - Father-in-law: Person who has ever physically hurt respondent";
  attrib D115Q    format=F01205_. label="Other in-law: Person who has ever physically hurt respondent";
  attrib D115R    format=F01206_. label="NA - Other male in-law: Person who has ever physically hurt respondent";
  attrib D115S    format=F01207_. label="NA - Female friend: Person who has ever physically hurt respondent";
  attrib D115T    format=F01208_. label="NA - Male friend: Person who has ever physically hurt respondent";
  attrib D115U    format=F01209_. label="NA - Neighbor: Person who has ever physically hurt respondent";
  attrib D115V    format=F01210_. label="NA - Teacher: Person who has ever physically hurt respondent";
  attrib D115W    format=F01211_. label="NA - Employer: Person who has ever physically hurt respondent";
  attrib D115X    format=F01212_. label="Other: Person who has ever physically hurt respondent";
  attrib D115Y    format=F01213_. label="No person other than husband/partner ever physically hurt respondent";
  attrib D115XA   format=F01214_. label="NA - Stranger: Person who has ever physically hurt respondent";
  attrib D115XB   format=F01215_. label="NA - Other female in-law/family: Person who has ever physically hurt respondent";
  attrib D115XC   format=F01216_. label="NA - Other male in-law/family: Person who has ever physically hurt respondent";
  attrib D115XD   format=F01217_. label="NA - Friend: Person who has ever physically hurt respondent";
  attrib D115XE   format=F01218_. label="NA - Police: Person who has ever physically hurt respondent";
  attrib D115XF   format=F01219_. label="NA - Religious leader: Person who has ever physically hurt respondent";
  attrib D115XG   format=F01220_. label="NA - Lawyer: Person who has ever physically hurt respondent";
  attrib D115XH   format=F01221_. label="NA - Doctor: Person who has ever physically hurt respondent";
  attrib D115XI   format=F01222_. label="Mates: Person who has ever physically hurt respondent";
  attrib D115XJ   format=F01223_. label="Unknown person: Person who has ever physically hurt respondent";
  attrib D115XK   format=F01224_. label="NA - CS: Person who has ever physically hurt respondent";
  attrib D116     format=F01225_. label="NA - Person who hurt respondent most often";
  attrib D117A    format=F01226_. label="NA - Frequency of being hit in last 12 months by other than husband/partner";
  attrib D118A    format=F01227_. label="Husband/partner: Person who hurt respondent during a pregnancy";
  attrib D118B    format=F01228_. label="Mother: Person who hurt respondent during a pregnancy";
  attrib D118C    format=F01229_. label="NA - Father/step-father: Person who hurt respondent during a pregnancy";
  attrib D118D    format=F01230_. label="Daughter/son: Person who hurt respondent during a pregnancy";
  attrib D118E    format=F01231_. label="NA - Son alone: Person who hurt respondent during a pregnancy";
  attrib D118F    format=F01232_. label="NA - Sister/brother: Person who hurt respondent during a pregnancy";
  attrib D118G    format=F01233_. label="Other relative: Person who hurt respondent during a pregnancy";
  attrib D118H    format=F01234_. label="NA - Other female family: Person who hurt respondent during a pregnancy";
  attrib D118I    format=F01235_. label="NA - Other male family: Person who hurt respondent during a pregnancy";
  attrib D118J    format=F01236_. label="Former husband/partner: Person who hurt respondent during a pregnancy";
  attrib D118K    format=F01237_. label="Current boyfriend: Person who hurt respondent during a pregnancy";
  attrib D118L    format=F01238_. label="Former boyfriend: Person who hurt respondent during a pregnancy";
  attrib D118M    format=F01239_. label="Step-mother: Person who hurt respondent during a pregnancy";
  attrib D118N    format=F01240_. label="NA - Step-father: Person who hurt respondent during a pregnancy";
  attrib D118O    format=F01241_. label="NA - Mother-in-law: Person who hurt respondent during a pregnancy";
  attrib D118P    format=F01242_. label="NA - Father-in-law: Person who hurt respondent during a pregnancy";
  attrib D118Q    format=F01243_. label="Other in-law: Person who hurt respondent during a pregnancy";
  attrib D118R    format=F01244_. label="NA - Other female/male in-law: Person who hurt respondent during a pregnancy";
  attrib D118S    format=F01245_. label="NA - Female friend: Person who hurt respondent during a pregnancy";
  attrib D118T    format=F01246_. label="NA - Male friend: Person who hurt respondent during a pregnancy";
  attrib D118U    format=F01247_. label="NA - Neighbor: Person who hurt respondent during a pregnancy";
  attrib D118V    format=F01248_. label="NA - Teacher: Person who hurt respondent during a pregnancy";
  attrib D118W    format=F01249_. label="NA - Employer: Person who hurt respondent during a pregnancy";
  attrib D118X    format=F01250_. label="Other: Person who hurt respondent during a pregnancy";
  attrib D118Y    format=F01251_. label="Respondent was not hurt by anyone during a pregnancy";
  attrib D118XA   format=F01252_. label="NA - Stranger: Person who hurt respondent during a pregnancy";
  attrib D118XB   format=F01253_. label="NA - Other female in-law/family: Person who hurt respondent during a pregnancy";
  attrib D118XC   format=F01254_. label="NA - Other male in-law/family: Person who hurt respondent during a pregnancy";
  attrib D118XD   format=F01255_. label="NA - Friend: Person who hurt respondent during a pregnancy";
  attrib D118XE   format=F01256_. label="NA - Police: Person who hurt respondent during a pregnancy";
  attrib D118XF   format=F01257_. label="NA - Religious leader: Person who hurt respondent during a pregnancy";
  attrib D118XG   format=F01258_. label="NA - Lawyer: Person who hurt respondent during a pregnancy";
  attrib D118XH   format=F01259_. label="NA - Doctor: Person who hurt respondent during a pregnancy";
  attrib D118XI   format=F01260_. label="NA - CS: Person who hurt respondent during a pregnancy";
  attrib D118XJ   format=F01261_. label="NA - CS: Person who hurt respondent during a pregnancy";
  attrib D118XK   format=F01262_. label="NA - CS: Person who hurt respondent during a pregnancy";
  attrib D119A    format=F01263_. label="Husband/partner: Person respondent went to seek help";
  attrib D119B    format=F01264_. label="Mother: Person respondent went to seek help";
  attrib D119C    format=F01265_. label="NA - Father: Person respondent went to seek help";
  attrib D119D    format=F01266_. label="NA - Daughter: Person respondent went to seek help";
  attrib D119E    format=F01267_. label="NA - Son: Person respondent went to seek help";
  attrib D119F    format=F01268_. label="NA - Sister: Person respondent went to seek help";
  attrib D119G    format=F01269_. label="NA - Brother: Person respondent went to seek help";
  attrib D119H    format=F01270_. label="Own family: Person respondent went to seek help";
  attrib D119I    format=F01271_. label="Husband/partner family: Person respondent went to seek help";
  attrib D119J    format=F01272_. label="Current/former husband/partner: Person respondent went to seek help";
  attrib D119K    format=F01273_. label="Current/former boyfriend: Person respondent went to seek help";
  attrib D119L    format=F01274_. label="Former boyfriend alone: Person respondent went to seek help";
  attrib D119M    format=F01275_. label="Step-mother: Person respondent went to seek help";
  attrib D119N    format=F01276_. label="NA - Step-father: Person respondent went to seek help";
  attrib D119O    format=F01277_. label="NA - Mother-in-law: Person respondent went to seek help";
  attrib D119P    format=F01278_. label="NA - Father-in-law: Person respondent went to seek help";
  attrib D119Q    format=F01279_. label="NA - Other female in-law: Person respondent went to seek help";
  attrib D119R    format=F01280_. label="NA - Other male in-law: Person respondent went to seek help";
  attrib D119S    format=F01281_. label="NA - Female friend: Person respondent went to seek help";
  attrib D119T    format=F01282_. label="Male friend: Person respondent went to seek help";
  attrib D119U    format=F01283_. label="NA - Neighbor: Person respondent went to seek help";
  attrib D119V    format=F01284_. label="Teacher: Person respondent went to seek help";
  attrib D119W    format=F01285_. label="NA - Employer: Person respondent went to seek help";
  attrib D119X    format=F01286_. label="Other: Person respondent went to seek help";
  attrib D119Y    format=F01287_. label="Respondent did not seek help from anyone";
  attrib D119XA   format=F01288_. label="NA - Stranger: Person respondent went to seek help";
  attrib D119XB   format=F01289_. label="NA - Social service organization: Person respondent went to seek help";
  attrib D119XC   format=F01290_. label="NA - CS: Person respondent went to seek help";
  attrib D119XD   format=F01291_. label="NA - Friend: Person respondent went to seek help";
  attrib D119XE   format=F01292_. label="NA - Police: Person respondent went to seek help";
  attrib D119XF   format=F01293_. label="NA - Religious leader: Person respondent went to seek help";
  attrib D119XG   format=F01294_. label="NA - Lawyer: Person respondent went to seek help";
  attrib D119XH   format=F01295_. label="NA - Doctor: Person respondent went to seek help";
  attrib D119XI   format=F01296_. label="NA - CS: Person respondent went to seek help";
  attrib D119XJ   format=F01297_. label="NA - CS: Person respondent went to seek help";
  attrib D119XK   format=F01298_. label="NA - CS: Person respondent went to seek help";
  attrib D120     format=F01299_. label="NA - Main reason never sought help";
  attrib D121     format=F01300_. label="Respondent's father ever beat her mother";
  attrib D122A    format=F01301_. label="NA - Interview interrupted: husband's presence";
  attrib D122B    format=F01302_. label="NA - Interview interrupted: other adult male's presence";
  attrib D122C    format=F01303_. label="NA - Interview interrupted: adult female's presence";
  attrib D123     format=F01304_. label="NA - First sex was wanted or forced";
  attrib D124     format=F01305_. label="Ever forced to have sex by anyone other than husband/partner in last 12 months";
  attrib D125     format=F01306_. label="NA - Ever forced to perform unwanted sexual acts";
  attrib D126     format=F01307_. label="Age at first forced sexual act";
  attrib D127     format=F01308_. label="NA - Person who forced respondent into first sexual act";
  attrib D128     format=F01309_. label="NA - Ever told anyone else about violence";
  attrib D129     format=F01310_. label="NA - Respondent afraid of husband/partner most of the time, sometimes or never";
  attrib D130A    format=F01311_. label="NA - Previous husband: ever hit, slap, kick or physically hurt respondent";
  attrib D130B    format=F01312_. label="NA - Previous husband: physically forced to have sex or to perform sexual acts";
  attrib D130C    format=F01313_. label="NA - Previous husband: humiliate, threaten to hurt, insult or make feel bad";
  attrib SDEPTO   format=F01314_. label="Department";
  attrib SMUNIP   label="Municipality";
  attrib SAREAM   label="Area ministry of health";
  attrib SAREA    format=F01315_. label="Area";
  attrib SSUBREG  format=F01316_. label="Subregions";
  attrib S101A    format=F01317_. label="Sex as declared by respondent";
  attrib S102     format=F01318_. label="Time living in current place of residence";
  attrib S106A    format=F01319_. label="Level of school completed";
  attrib S106B    format=F01320_. label="Grade of school completed";
  attrib S109     format=F01321_. label="Currently attending school";
  attrib S107     format=F01322_. label="Knows how to read and write";
  attrib S110     format=F01323_. label="Number of years stopped school";
  attrib S111     format=F01324_. label="Reason stop school";
  attrib S112     format=F01325_. label="Mother current marital status";
  attrib S113     format=F01326_. label="Father current marital status";
  attrib S115     format=F01327_. label="Mother marital status when respondent was 12-14 years old";
  attrib S116     format=F01328_. label="Father marital status when respondent was 12-14 years old";
  attrib S117     format=F01329_. label="Who did used to live with when between 12-14 years old";
  attrib S118     format=F01330_. label="Currently respondent lives with";
  attrib S119     format=F01331_. label="Respondent's mother children ever born";
  attrib S120     format=F01332_. label="All respondent's mother children from same man";
  attrib S121     format=F01333_. label="All respondent's father children from same woman";
  attrib S122     format=F01334_. label="Respondent's mother age at first child";
  attrib S123     format=F01335_. label="Respondent's father age at first child";
  attrib S124     format=F01336_. label="Any of respondent's brothers/sisters had children before they were 18";
  attrib S125A    format=F01337_. label="Frequency father, mother, caretaker: know where you are when leaving the house";
  attrib S125B    format=F01338_. label="Frequency father, mother, caretaker: know your friends";
  attrib S125C    format=F01339_. label="Frequency father, mother, caretaker: establish a time to go out and come back";
  attrib S125D    format=F01340_. label="Frequency father, mother, caretaker: know how you are doing at school";
  attrib S126A    format=F01341_. label="Among friends the same age: how many started sex live";
  attrib S126B    format=F01342_. label="Among friends the same age: how many got pregnant";
  attrib S126C    format=F01343_. label="Among friends the same age: how many use condom always";
  attrib S206A    format=F01344_. label="Daughters living in country";
  attrib S206B    format=F01345_. label="Sons living in country";
  attrib S206C    format=F01346_. label="Daughters living abroad";
  attrib S206D    format=F01347_. label="Sons living abroad";
  attrib S207A    label="Daughters living in Venezuela";
  attrib S207B    label="Sons living in Venezuela";
  attrib S207C    label="Daughters living in U.S.A";
  attrib S207D    label="Sons living in U.S.A.";
  attrib S207E    label="Daughters living in Spain";
  attrib S207F    label="Sons living in Spain";
  attrib S207G    label="Daughters living in Ecuador";
  attrib S207H    label="Sons living in Ecuador";
  attrib S207I    label="Daughters living in Panama";
  attrib S207J    label="Sons living in Panama";
  attrib S207M    label="Daughters living in other countries";
  attrib S207N    label="Sons living in other countries";
  attrib S213     format=F01348_. label="All respondent's children from the same father";
  attrib S225A    format=F01349_. label="Age at first child (provided by respondent)";
  attrib S225B    format=F01350_. label="Age biological father at first child";
  attrib S233     format=F01351_. label="Was afraid to tell partner about your pregnancy";
  attrib S234     format=F01352_. label="Current pregnancy was a couple's decision";
  attrib S235     format=F01353_. label="Place for prenatal check-up of current pregnancy";
  attrib S236     label="Months of pregnancy at first prenatal check-up";
  attrib S238A    format=F01354_. label="Reason no prenatal check-up: personal";
  attrib S238B    format=F01355_. label="Reason no prenatal check-up: health services related inconveniences";
  attrib S238C    format=F01356_. label="Reason no prenatal check-up: economic";
  attrib S238D    format=F01357_. label="Reason no prenatal check-up: too much paper work";
  attrib S238E    format=F01358_. label="Reason no prenatal check-up: lack of knowledge";
  attrib S238F    format=F01359_. label="Reason no prenatal check-up: others opposed";
  attrib S238G    format=F01360_. label="Reason no prenatal check-up: don't have health insurance";
  attrib S238H    format=F01361_. label="Reason no prenatal check-up: didn't know that had access to service";
  attrib S238X    format=F01362_. label="Reason no prenatal check-up: other";
  attrib S239     format=F01363_. label="Days respondent was hospitalized during current pregnancy";
  attrib S240A    format=F01364_. label="Reason for hospitalization: infection";
  attrib S240B    format=F01365_. label="Reason for hospitalization: bleeding";
  attrib S240C    format=F01366_. label="Reason for hospitalization: hypertension";
  attrib S240D    format=F01367_. label="Reason for hospitalization: convulsions";
  attrib S240E    format=F01368_. label="Reason for hospitalization: threat of abortion/miscarriage";
  attrib S240F    format=F01369_. label="Reason for hospitalization: membrane breakup";
  attrib S240G    format=F01370_. label="Reason for hospitalization: physical aggression";
  attrib S240X    format=F01371_. label="Reason for hospitalization: other";
  attrib S242     label="Total number of terminations";
  attrib S243_1   label="Termination line number(1)";
  attrib S243_2   label="Termination line number(2)";
  attrib S243_3   label="Termination line number(3)";
  attrib S243_4   label="Termination line number(4)";
  attrib S243_5   label="Termination line number(5)";
  attrib S243_6   label="Termination line number(6)";
  attrib S243_7   label="Termination line number(7)";
  attrib S243_8   label="Termination line number(8)";
  attrib S244M_1  format=F01372_. label="Termination month(1)";
  attrib S244M_2  format=F01372_. label="Termination month(2)";
  attrib S244M_3  format=F01372_. label="Termination month(3)";
  attrib S244M_4  format=F01372_. label="Termination month(4)";
  attrib S244M_5  format=F01372_. label="Termination month(5)";
  attrib S244M_6  format=F01372_. label="Termination month(6)";
  attrib S244M_7  format=F01372_. label="Termination month(7)";
  attrib S244M_8  format=F01372_. label="Termination month(8)";
  attrib S244Y_1  label="Termination year(1)";
  attrib S244Y_2  label="Termination year(2)";
  attrib S244Y_3  label="Termination year(3)";
  attrib S244Y_4  label="Termination year(4)";
  attrib S244Y_5  label="Termination year(5)";
  attrib S244Y_6  label="Termination year(6)";
  attrib S244Y_7  label="Termination year(7)";
  attrib S244Y_8  label="Termination year(8)";
  attrib S245_1   format=F01373_. label="Age at termination(1)";
  attrib S245_2   format=F01373_. label="Age at termination(2)";
  attrib S245_3   format=F01373_. label="Age at termination(3)";
  attrib S245_4   format=F01373_. label="Age at termination(4)";
  attrib S245_5   format=F01373_. label="Age at termination(5)";
  attrib S245_6   format=F01373_. label="Age at termination(6)";
  attrib S245_7   format=F01373_. label="Age at termination(7)";
  attrib S245_8   format=F01373_. label="Age at termination(8)";
  attrib S246_1   format=F01374_. label="Termination type(1)";
  attrib S246_2   format=F01374_. label="Termination type(2)";
  attrib S246_3   format=F01374_. label="Termination type(3)";
  attrib S246_4   format=F01374_. label="Termination type(4)";
  attrib S246_5   format=F01374_. label="Termination type(5)";
  attrib S246_6   format=F01374_. label="Termination type(6)";
  attrib S246_7   format=F01374_. label="Termination type(7)";
  attrib S246_8   format=F01374_. label="Termination type(8)";
  attrib S247_1   label="Termination months of pregnancy(1)";
  attrib S247_2   label="Termination months of pregnancy(2)";
  attrib S247_3   label="Termination months of pregnancy(3)";
  attrib S247_4   label="Termination months of pregnancy(4)";
  attrib S247_5   label="Termination months of pregnancy(5)";
  attrib S247_6   label="Termination months of pregnancy(6)";
  attrib S247_7   label="Termination months of pregnancy(7)";
  attrib S247_8   label="Termination months of pregnancy(8)";
  attrib S251     format=F01375_. label="Place where care was provided for last termination";
  attrib S252     format=F01376_. label="Age at first menstruation";
  attrib S302_01  format=F01377_. label="Source of information for method(1)";
  attrib S302_02  format=F01377_. label="Source of information for method(2)";
  attrib S302_03  format=F01377_. label="Source of information for method(3)";
  attrib S302_04  format=F01377_. label="Source of information for method(4)";
  attrib S302_05  format=F01377_. label="Source of information for method(5)";
  attrib S302_06  format=F01377_. label="Source of information for method(6)";
  attrib S302_07  format=F01377_. label="Source of information for method(7)";
  attrib S302_08  format=F01377_. label="Source of information for method(8)";
  attrib S302_09  format=F01377_. label="Source of information for method(9)";
  attrib S302_10  format=F01377_. label="Source of information for method(10)";
  attrib S302_11  format=F01377_. label="Source of information for method(11)";
  attrib S302_12  format=F01377_. label="Source of information for method(12)";
  attrib S302_13  format=F01377_. label="Source of information for method(13)";
  attrib S302_14  format=F01377_. label="Source of information for method(14)";
  attrib S302_15  format=F01377_. label="Source of information for method(15)";
  attrib S302_16  format=F01377_. label="Source of information for method(16)";
  attrib S302_17  format=F01377_. label="Source of information for method(17)";
  attrib S302_18  format=F01377_. label="Source of information for method(18)";
  attrib S302_19  format=F01377_. label="Source of information for method(19)";
  attrib S302_20  format=F01377_. label="Source of information for method(20)";
  attrib S313     format=F01378_. label="What days during menstrual cycle shouldn't have sex";
  attrib S316     format=F01379_. label="Regret sterilization";
  attrib S317     format=F01380_. label="Reason regret sterilization";
  attrib S332     format=F01381_. label="Ever been forced to use a contraceptive method";
  attrib S333A    format=F01382_. label="Forced to use contraception by: mother/father";
  attrib S333B    format=F01383_. label="Forced to use contraception by: step-mother/step-father";
  attrib S333C    format=F01384_. label="Forced to use contraception by: ex-husband/partner";
  attrib S333D    format=F01385_. label="Forced to use contraception by: boyfriend";
  attrib S333E    format=F01386_. label="Forced to use contraception by: ex-boyfriend";
  attrib S333F    format=F01387_. label="Forced to use contraception by: husband/partner";
  attrib S333X    format=F01388_. label="Forced to use contraception by: other";
  attrib S334     format=F01389_. label="Do you know that health insurance must provide method with no copay";
  attrib S600     format=F01390_. label="Respondent's sexual orientation";
  attrib S606     label="Number of unions";
  attrib S607N_1  label="Order number(1)";
  attrib S607N_2  label="Order number(2)";
  attrib S607N_3  label="Order number(3)";
  attrib S607N_4  label="Order number(4)";
  attrib S607N_5  label="Order number(5)";
  attrib S607M_1  format=F01391_. label="Month union started(1)";
  attrib S607M_2  format=F01391_. label="Month union started(2)";
  attrib S607M_3  format=F01391_. label="Month union started(3)";
  attrib S607M_4  format=F01391_. label="Month union started(4)";
  attrib S607M_5  format=F01391_. label="Month union started(5)";
  attrib S607Y_1  format=F01392_. label="Year union started(1)";
  attrib S607Y_2  format=F01392_. label="Year union started(2)";
  attrib S607Y_3  format=F01392_. label="Year union started(3)";
  attrib S607Y_4  format=F01392_. label="Year union started(4)";
  attrib S607Y_5  format=F01392_. label="Year union started(5)";
  attrib S607A_1  label="Age at union(1)";
  attrib S607A_2  label="Age at union(2)";
  attrib S607A_3  label="Age at union(3)";
  attrib S607A_4  label="Age at union(4)";
  attrib S607A_5  label="Age at union(5)";
  attrib S608_1   format=F01393_. label="Sex of partner(1)";
  attrib S608_2   format=F01393_. label="Sex of partner(2)";
  attrib S608_3   format=F01393_. label="Sex of partner(3)";
  attrib S608_4   format=F01393_. label="Sex of partner(4)";
  attrib S608_5   format=F01393_. label="Sex of partner(5)";
  attrib S609_1   format=F01394_. label="Partner's age(1)";
  attrib S609_2   format=F01394_. label="Partner's age(2)";
  attrib S609_3   format=F01394_. label="Partner's age(3)";
  attrib S609_4   format=F01394_. label="Partner's age(4)";
  attrib S609_5   format=F01394_. label="Partner's age(5)";
  attrib S610_1   format=F01395_. label="Union type(1)";
  attrib S610_2   format=F01395_. label="Union type(2)";
  attrib S610_3   format=F01395_. label="Union type(3)";
  attrib S610_4   format=F01395_. label="Union type(4)";
  attrib S610_5   format=F01395_. label="Union type(5)";
  attrib S611_1   format=F01396_. label="Got married later with partner(1)";
  attrib S611_2   format=F01396_. label="Got married later with partner(2)";
  attrib S611_3   format=F01396_. label="Got married later with partner(3)";
  attrib S611_4   format=F01396_. label="Got married later with partner(4)";
  attrib S611_5   format=F01396_. label="Got married later with partner(5)";
  attrib S612M_1  format=F01397_. label="Month of marriage(1)";
  attrib S612M_2  format=F01397_. label="Month of marriage(2)";
  attrib S612M_3  format=F01397_. label="Month of marriage(3)";
  attrib S612M_4  format=F01397_. label="Month of marriage(4)";
  attrib S612M_5  format=F01397_. label="Month of marriage(5)";
  attrib S612Y_1  format=F01398_. label="Year of marriage(1)";
  attrib S612Y_2  format=F01398_. label="Year of marriage(2)";
  attrib S612Y_3  format=F01398_. label="Year of marriage(3)";
  attrib S612Y_4  format=F01398_. label="Year of marriage(4)";
  attrib S612Y_5  format=F01398_. label="Year of marriage(5)";
  attrib S613_1   format=F01399_. label="Still lives with partner(1)";
  attrib S613_2   format=F01399_. label="Still lives with partner(2)";
  attrib S613_3   format=F01399_. label="Still lives with partner(3)";
  attrib S613_4   format=F01399_. label="Still lives with partner(4)";
  attrib S613_5   format=F01399_. label="Still lives with partner(5)";
  attrib S614M_1  format=F01400_. label="Month stopped living with partner(1)";
  attrib S614M_2  format=F01400_. label="Month stopped living with partner(2)";
  attrib S614M_3  format=F01400_. label="Month stopped living with partner(3)";
  attrib S614M_4  format=F01400_. label="Month stopped living with partner(4)";
  attrib S614M_5  format=F01400_. label="Month stopped living with partner(5)";
  attrib S614Y_1  format=F01401_. label="Year stopped living with partner(1)";
  attrib S614Y_2  format=F01401_. label="Year stopped living with partner(2)";
  attrib S614Y_3  format=F01401_. label="Year stopped living with partner(3)";
  attrib S614Y_4  format=F01401_. label="Year stopped living with partner(4)";
  attrib S614Y_5  format=F01401_. label="Year stopped living with partner(5)";
  attrib S615_1   format=F01402_. label="Reason stopped living with partner(1)";
  attrib S615_2   format=F01402_. label="Reason stopped living with partner(2)";
  attrib S615_3   format=F01402_. label="Reason stopped living with partner(3)";
  attrib S615_4   format=F01402_. label="Reason stopped living with partner(4)";
  attrib S615_5   format=F01402_. label="Reason stopped living with partner(5)";
  attrib S616A    format=F01403_. label="Has ever had sexual intercourse without condoms";
  attrib S617     format=F01404_. label="Has ever had anal sexual intercourse";
  attrib S617A    format=F01405_. label="Has ever had anal sexual intercourse without condoms";
  attrib S618     format=F01406_. label="Has ever had sexual approaches or intercourse with other women";
  attrib S622A    format=F01407_. label="Has ever felt pressure from friends to have sex (women 13-17 years old)";
  attrib S622B    format=F01408_. label="Had felt pressure from friends before  respondent was 18 years old to have sex (women 18-24 years old)";
  attrib S628     format=F01409_. label="Sex of first sexual partner";
  attrib S631     format=F01410_. label="First sex partner older, age difference";
  attrib S632     format=F01411_. label="First sex partner younger, age difference";
  attrib S638A    format=F01412_. label="Reason last sexual partner didn't use condom";
  attrib S638B    format=F01413_. label="Reason next to last sexual partner didn't use condom";
  attrib S638C    format=F01414_. label="Reason second to last sexual partner didn't use condom";
  attrib S652     format=F01415_. label="Circumstances under which abortion is legal in Colombia";
  attrib S653A    format=F01416_. label="Abortion is legal when: woman was raped";
  attrib S653B    format=F01417_. label="Abortion is legal when: fetus malformation";
  attrib S653C    format=F01418_. label="Abortion is legal when: pregnancy risks the woman's life";
  attrib S653D    format=F01419_. label="Abortion is legal when: pregnancy risks woman's physical health";
  attrib S653E    format=F01420_. label="Abortion is legal when: pregnancy risks woman's mental health";
  attrib S653F    format=F01421_. label="Abortion is legal when: woman got pregnant from close relative";
  attrib S653G    format=F01422_. label="Abortion is legal when: woman is less than 14 years old";
  attrib S654A    format=F01423_. label="Abortion should be permitted when: woman was raped";
  attrib S654B    format=F01424_. label="Abortion should be permitted when: fetus malformation";
  attrib S654C    format=F01425_. label="Abortion should be permitted when: have too many children";
  attrib S654D    format=F01426_. label="Abortion should be permitted when: save woman's life";
  attrib S654E    format=F01427_. label="Abortion should be permitted when: pregnancy risks woman's physical health";
  attrib S654F    format=F01428_. label="Abortion should be permitted when: pregnancy risks woman's mental health";
  attrib S654G    format=F01429_. label="Abortion should be permitted when: woman is single";
  attrib S654H    format=F01430_. label="Abortion should be permitted when: poor economic situation";
  attrib S654I    format=F01431_. label="Abortion should be permitted when: contraceptive failed";
  attrib S654J    format=F01432_. label="Abortion should be permitted when: woman got pregnant from close relative";
  attrib S654K    format=F01433_. label="Abortion should be permitted when: woman is less than 14 years old";
  attrib S654L    format=F01434_. label="Abortion should be permitted when: woman's decision";
  attrib S655     format=F01435_. label="Ever requested abortion since 2006";
  attrib S656     format=F01436_. label="Was the abortion performed";
  attrib S657     format=F01437_. label="Reason to request abortion";
  attrib S708     format=F01438_. label="Has ever wanted unsuccessfully a child";
  attrib S709     format=F01439_. label="How long have been waiting for a pregnancy";
  attrib S710     format=F01440_. label="Has ever sought fertility help to get pregnant";
  attrib S711     format=F01441_. label="Reason given for not getting pregnant";
  attrib S712     format=F01442_. label="Respondent/partner receiving fertility treatment";
  attrib S713     format=F01443_. label="Main reason not seeking fertility treatment";
  attrib S722     format=F01444_. label="Respondent approves couples using contraception";
  attrib S723A    format=F01445_. label="Heard FP at health insurance last 12 months";
  attrib S805     format=F01446_. label="Has respondent ever worked";
  attrib S813     format=F01447_. label="Respondent salary average used for household consumption";
  attrib S814F    format=F01448_. label="Final say on studying";
  attrib S814G    format=F01449_. label="Final say on having sex";
  attrib S815B    format=F01450_. label="House/apartment where respondent lives";
  attrib S815C    format=F01451_. label="Other house/apartment";
  attrib S815D    format=F01452_. label="Owns jewelry";
  attrib S815E    format=F01453_. label="Owns animals, cattle, chickens, pigs";
  attrib S1008    format=F01454_. label="Life expectancy for someone with AIDS";
  attrib S1010    format=F01455_. label="Person with highest vulnerability to get AIDS";
  attrib S1011A   format=F01456_. label="Perception on getting AIDS when: have sex without protection";
  attrib S1011B   format=F01457_. label="Perception on getting AIDS when: use alcohol or drugs before sex";
  attrib S1011C   format=F01458_. label="Perception on getting AIDS when: couples get tested and agree on mutual protection";
  attrib S1011D   format=F01459_. label="Perception on getting AIDS when: have other STDs";
  attrib S1021A   format=F01460_. label="Received counseling before HIV test as part of antenatal visit";
  attrib S1022A   format=F01461_. label="Received counseling after receiving test results as part of antenatal visit";
  attrib S1026A   format=F01462_. label="Received counseling before HIV test last time";
  attrib S1027A   format=F01463_. label="Reason last HIV test was done";
  attrib S1032    format=F01464_. label="Frequency of taking HIV test";
  attrib S1038    format=F01465_. label="Ever had sex with an infected individual";
  attrib S1040A   format=F01466_. label="Man's symptoms of STD: abdominal pain";
  attrib S1040B   format=F01467_. label="Man's symptoms of STD:  genital discharge/dripping";
  attrib S1040C   format=F01468_. label="Man's symptoms of STD: foul smelling discharge";
  attrib S1040D   format=F01469_. label="Man's symptoms of STD: burning pain during urination";
  attrib S1040E   format=F01470_. label="Man's symptoms of STD: redness/rash";
  attrib S1040F   format=F01471_. label="Man's symptoms of STD: swelling";
  attrib S1040G   format=F01472_. label="Man's symptoms of STD: sores/ulcers";
  attrib S1040H   format=F01473_. label="Man's symptoms of STD: warts";
  attrib S1040I   format=F01474_. label="Man's symptoms of STD: blood in urine";
  attrib S1040J   format=F01475_. label="Man's symptoms of STD: weight loss";
  attrib S1040K   format=F01476_. label="Man's symptoms of STD: impotence";
  attrib S1040L   format=F01477_. label="Man's symptoms of STD: no symptoms";
  attrib S1040X   format=F01478_. label="Man's symptoms of STD: other";
  attrib S1040Z   format=F01479_. label="Man's symptoms of STD: don't know";
  attrib S1041A   format=F01480_. label="Woman's symptoms of STD: abdominal pain";
  attrib S1041B   format=F01481_. label="Woman's symptoms of STD:  genital discharge/dripping";
  attrib S1041C   format=F01482_. label="Woman's symptoms of STD: foul smelling discharge";
  attrib S1041D   format=F01483_. label="Woman's symptoms of STD: burning pain during urination";
  attrib S1041E   format=F01484_. label="Woman's symptoms of STD: redness/rash";
  attrib S1041F   format=F01485_. label="Woman's symptoms of STD: swelling";
  attrib S1041G   format=F01486_. label="Woman's symptoms of STD: sores/ulcers";
  attrib S1041H   format=F01487_. label="Woman's symptoms of STD: warts";
  attrib S1041I   format=F01488_. label="Woman's symptoms of STD: blood in urine";
  attrib S1041J   format=F01489_. label="Woman's symptoms of STD: weight loss";
  attrib S1041K   format=F01490_. label="Woman's symptoms of STD: incapacity to deliver";
  attrib S1041L   format=F01491_. label="Woman's symptoms of STD: no symptoms";
  attrib S1041X   format=F01492_. label="Woman's symptoms of STD: other";
  attrib S1041Z   format=F01493_. label="Woman's symptoms of STD: don't know";
  attrib S1050    format=F01494_. label="Advise partner when had STD";
  attrib S1051    format=F01495_. label="When had STD did something to avoid infecting partner";
  attrib S1052A   format=F01496_. label="What was done: no sexual intercourse";
  attrib S1052B   format=F01497_. label="What was done: used condoms";
  attrib S1052C   format=F01498_. label="What was done: took medicines";
  attrib S1055    format=F01499_. label="During health consultations got advice to prevent STDs";
  attrib S1203CA  format=F01500_. label="Place where event occurred when: partner jealous";
  attrib S1203CB  format=F01501_. label="Place where event occurred when: accused not being faithful";
  attrib S1203CC  format=F01502_. label="Place where event occurred when: didn't allow to see friends";
  attrib S1203CD  format=F01503_. label="Place where event occurred when: limit family contact";
  attrib S1203CE  format=F01504_. label="Place where event occurred when: insist in knowing where you are";
  attrib S1203CF  format=F01505_. label="Place where event occurred when: ignored you";
  attrib S1203CI  format=F01506_. label="Place where event occurred when:  threaten with a weapon";
  attrib S1203CJ  format=F01507_. label="Place where event occurred when: refer to you in terms not appropriate";
  attrib S1203CK  format=F01508_. label="Place where event occurred when: threaten to leave you";
  attrib S1203CL  format=F01509_. label="Place where event occurred when: threaten to take away the children";
  attrib S1204CA  format=F01510_. label="Place where event occurred when: push, shake you";
  attrib S1204CB  format=F01511_. label="Place where event occurred when: slap you";
  attrib S1204CC  format=F01512_. label="Place where event occurred when: hit you with an object";
  attrib S1204CD  format=F01513_. label="Place where event occurred when: kick, drag you";
  attrib S1204CE  format=F01514_. label="Place where event occurred when: attack with a knife/gun";
  attrib S1204CF  format=F01515_. label="Place where event occurred when: tried to strangle, burn";
  attrib S1205B   format=F01516_. label="Husband/partner: threaten to withdraw economic support";
  attrib S1205C   format=F01517_. label="Husband/partner: didn't allow to study/work";
  attrib S1205D   format=F01518_. label="Husband/partner: spent the household money";
  attrib S1205E   format=F01519_. label="Husband/partner: take away money or real state";
  attrib S1205CA  format=F01520_. label="Place where event occurred when: controlling money";
  attrib S1205CB  format=F01521_. label="Place where event occurred when: threaten to withdraw economic support";
  attrib S1205CC  format=F01522_. label="Place where event occurred when: didn't allow to study/work";
  attrib S1206CA  format=F01523_. label="Place where event occurred when: forced to perform sexual acts";
  attrib S1207    format=F01524_. label="As a result of sexual violence: took emergency contraception";
  attrib S1208    format=F01525_. label="As a result of sexual violence: got pregnant";
  attrib S1209    format=F01526_. label="Did sexual violence occurred after May 2006";
  attrib S1210    format=F01527_. label="When sexual violence occurred was told that could request to stop of the pregnancy";
  attrib S1211    format=F01528_. label="Did request interruption of pregnancy";
  attrib S1212    format=F01529_. label="Was interruption of pregnancy done";
  attrib S1215G   format=F01530_. label="As a result of mistreatment respondent: self-esteem went down";
  attrib S1215H   format=F01531_. label="As a result of mistreatment respondent: affected relationship with children";
  attrib S1215I   format=F01532_. label="As a result of mistreatment respondent: stopped talking to people";
  attrib S1215J   format=F01533_. label="As a result of mistreatment respondent: diminish productivity";
  attrib S1215K   format=F01534_. label="As a result of mistreatment respondent: got sick (disoriented)";
  attrib S1215L   format=F01535_. label="As a result of mistreatment respondent: lost interest in sex";
  attrib S1215M   format=F01536_. label="As a result of mistreatment respondent: stopped working";
  attrib S1215N   format=F01537_. label="As a result of mistreatment respondent: stopped studying";
  attrib S1217    format=F01538_. label="Last 12 months considered separating from partner";
  attrib S1218    format=F01539_. label="Reasons for wanting to separate from partner";
  attrib S1220    format=F01540_. label="At the time of aggression did you attack your husband back";
  attrib S1221A   format=F01541_. label="At times when husband is not mistreating have you: hit him";
  attrib S1221B   format=F01542_. label="At times when husband is not mistreating have you: insulted him";
  attrib S1221C   format=F01543_. label="At times when husband is not mistreating have you: controlled him";
  attrib S1221D   format=F01544_. label="At times when husband is not mistreating have you: got jealous";
  attrib S1221X   format=F01545_. label="At times when husband is not mistreating have you: other";
  attrib S1221Z   format=F01546_. label="At times when husband is not mistreating have you: done nothing";
  attrib S1222    format=F01547_. label="Frequency of physically hurting husband/partner in last 12 months";
  attrib S1224A   format=F01548_. label="How did your parents punish you: spanking";
  attrib S1224B   format=F01549_. label="How did your parents punish you: pushing";
  attrib S1224C   format=F01550_. label="How did your parents punish you: by verbal reprimand";
  attrib S1224D   format=F01551_. label="How did your parents punish you: prohibiting something I liked";
  attrib S1224E   format=F01552_. label="How did your parents punish you: depriving from food";
  attrib S1224F   format=F01553_. label="How did your parents punish you: hitting with objects";
  attrib S1224G   format=F01554_. label="How did your parents punish you: keeping me locked up";
  attrib S1224H   format=F01555_. label="How did your parents punish you: ignoring";
  attrib S1224I   format=F01556_. label="How did your parents punish you: assigning non-appropriate work";
  attrib S1224J   format=F01557_. label="How did your parents punish you: leaving me out of the house";
  attrib S1224K   format=F01558_. label="How did your parents punish you: throwing water";
  attrib S1224L   format=F01559_. label="How did your parents punish you: hiding clothes";
  attrib S1224M   format=F01560_. label="How did your parents punish you: taking away belongings";
  attrib S1224N   format=F01561_. label="How did your parents punish you: withdrawing economic support";
  attrib S1224X   format=F01562_. label="How did your parents punish you: other";
  attrib S1224Z   format=F01563_. label="How did your parents punish you: didn't punish";
  attrib S1225    format=F01564_. label="Was partner mistreated by parents/step parents";
  attrib S1226    format=F01565_. label="Did partner's father mistreated your mother";
  attrib S1226A   format=F01566_. label="Do you have a step child living with you";
  attrib S1228A   format=F01567_. label="Who punish children/step children: father/step-father";
  attrib S1228B   format=F01568_. label="Who punish children/step children: respondent/step-mother";
  attrib S1228X   format=F01569_. label="Who punish children/step children: other";
  attrib S1228Y   format=F01570_. label="Who punish children/step children: nobody";
  attrib S1230AA  format=F01571_. label="How partner punish children: spanking them";
  attrib S1230AB  format=F01572_. label="How partner punish children: pushing them";
  attrib S1230AC  format=F01573_. label="How partner punish children: by verbal reprimand";
  attrib S1230AD  format=F01574_. label="How partner punish children: prohibiting something they like";
  attrib S1230AE  format=F01575_. label="How partner punish children: depriving from food";
  attrib S1230AF  format=F01576_. label="How partner punish children:  hitting with objects";
  attrib S1230AG  format=F01577_. label="How partner punish children: keeping them locked up";
  attrib S1230AH  format=F01578_. label="How partner punish children: ignoring them";
  attrib S1230AI  format=F01579_. label="How partner punish children: assigning non-appropriate work";
  attrib S1230AJ  format=F01580_. label="How partner punish children: leaving them out of the house";
  attrib S1230AK  format=F01581_. label="How partner punish children: throwing water at them";
  attrib S1230AL  format=F01582_. label="How partner punish children: hiding their clothes";
  attrib S1230AM  format=F01583_. label="How partner punish children: taking away belongings";
  attrib S1230AN  format=F01584_. label="How partner punish children: withdrawing economic support";
  attrib S1230AX  format=F01585_. label="How partner punish children: other";
  attrib S1230BA  format=F01586_. label="How respondent punish children: spanking them";
  attrib S1230BB  format=F01587_. label="How respondent punish children: pushing them";
  attrib S1230BC  format=F01588_. label="How respondent punish children: by verbal reprimand";
  attrib S1230BD  format=F01589_. label="How respondent punish children: prohibiting something they like";
  attrib S1230BE  format=F01590_. label="How respondent punish children: depriving from food";
  attrib S1230BF  format=F01591_. label="How respondent punish children:  hitting with objects";
  attrib S1230BG  format=F01592_. label="How respondent punish children: keeping them locked up";
  attrib S1230BH  format=F01593_. label="How respondent punish children: ignoring them";
  attrib S1230BI  format=F01594_. label="How respondent punish children: assigning non-appropriate work";
  attrib S1230BJ  format=F01595_. label="How respondent punish children: leaving them out of the house";
  attrib S1230BK  format=F01596_. label="How respondent punish children: throwing water at them";
  attrib S1230BL  format=F01597_. label="How respondent punish children: hiding their clothes";
  attrib S1230BM  format=F01598_. label="How respondent punish children: taking away belongings";
  attrib S1230BN  format=F01599_. label="How respondent punish children: withdrawing economic support";
  attrib S1230BX  format=F01600_. label="How respondent punish children: other";
  attrib S1230CA  format=F01601_. label="How other punish children: spanking them";
  attrib S1230CB  format=F01602_. label="How other punish children: pushing them";
  attrib S1230CC  format=F01603_. label="How other punish children: by verbal reprimand";
  attrib S1230CD  format=F01604_. label="How other punish children: prohibiting something they like";
  attrib S1230CE  format=F01605_. label="How other punish children: depriving from food";
  attrib S1230CF  format=F01606_. label="How other punish children:  hitting with objects";
  attrib S1230CG  format=F01607_. label="How other punish children: keeping them locked up";
  attrib S1230CH  format=F01608_. label="How other punish children: ignoring them";
  attrib S1230CI  format=F01609_. label="How other punish children: assigning non-appropriate work";
  attrib S1230CJ  format=F01610_. label="How other punish children: leaving them out of the house";
  attrib S1230CK  format=F01611_. label="How other punish children: throwing water at them";
  attrib S1230CL  format=F01612_. label="How other punish children: hiding their clothes";
  attrib S1230CM  format=F01613_. label="How other punish children: taking away belongings";
  attrib S1230CN  format=F01614_. label="How other punish children: withdrawing economic support";
  attrib S1230CX  format=F01615_. label="How other punish children: other";
  attrib S1234    format=F01616_. label="As a result of violence did lost a pregnancy";
  attrib S1235    format=F01617_. label="Someone/partner didn't allow you to see friends";
  attrib S1236A   format=F01618_. label="Person who didn't allow see friends: mother/father";
  attrib S1236B   format=F01619_. label="Person who didn't allow see friends: step mother/father";
  attrib S1236C   format=F01620_. label="Person who didn't allow see friends: son/daughter/step-child";
  attrib S1236D   format=F01621_. label="Person who didn't allow see friends: partner relative";
  attrib S1236E   format=F01622_. label="Person who didn't allow see friends: own relative";
  attrib S1236F   format=F01623_. label="Person who didn't allow see friends: boyfriend";
  attrib S1236G   format=F01624_. label="Person who didn't allow see friends: ex-boyfriend";
  attrib S1236X   format=F01625_. label="Person who didn't allow see friends: other";
  attrib S1237    format=F01626_. label="Someone/partner limit contact with family";
  attrib S1238A   format=F01627_. label="Person who limit contact with family: mother/father";
  attrib S1238B   format=F01628_. label="Person who limit contact with family: step mother/father";
  attrib S1238C   format=F01629_. label="Person who limit contact with family: son/daughter/step-child";
  attrib S1238D   format=F01630_. label="Person who limit contact with family: partner relative";
  attrib S1238E   format=F01631_. label="Person who limit contact with family: own relative";
  attrib S1238F   format=F01632_. label="Person who limit contact with family: boyfriend";
  attrib S1238G   format=F01633_. label="Person who limit contact with family: ex-boyfriend";
  attrib S1238X   format=F01634_. label="Person who limit contact with family: other";
  attrib S1239    format=F01635_. label="Someone/partner have addressed you in bad terms";
  attrib S1240A   format=F01636_. label="Person who addressed you in bad terms: mother/father";
  attrib S1240B   format=F01637_. label="Person who addressed you in bad terms: step mother/father";
  attrib S1240C   format=F01638_. label="Person who addressed you in bad terms: son/daughter/step-child";
  attrib S1240D   format=F01639_. label="Person who addressed you in bad terms: partner relative";
  attrib S1240E   format=F01640_. label="Person who addressed you in bad terms: own relative";
  attrib S1240F   format=F01641_. label="Person who addressed you in bad terms: boyfriend";
  attrib S1240G   format=F01642_. label="Person who addressed you in bad terms: ex-boyfriend";
  attrib S1240H   format=F01643_. label="Person who addressed you in bad terms: friends/mates";
  attrib S1240X   format=F01644_. label="Person who addressed you in bad terms: other";
  attrib S1243    format=F01645_. label="Forced by someone/partner to have sexual intercourse";
  attrib S1245    format=F01646_. label="Want to talk about the time when forced for sexual acts";
  attrib S1246A   format=F01647_. label="Who forced to perform sexual acts: father";
  attrib S1246B   format=F01648_. label="Who forced to perform sexual acts: step father";
  attrib S1246C   format=F01649_. label="Who forced to perform sexual acts: brother";
  attrib S1246D   format=F01650_. label="Who forced to perform sexual acts: ex-partner";
  attrib S1246E   format=F01651_. label="Who forced to perform sexual acts: boyfriend";
  attrib S1246F   format=F01652_. label="Who forced to perform sexual acts: ex-boyfriend";
  attrib S1246G   format=F01653_. label="Who forced to perform sexual acts: acquaintance";
  attrib S1246H   format=F01654_. label="Who forced to perform sexual acts: neighbor";
  attrib S1246I   format=F01655_. label="Who forced to perform sexual acts: other male relative of respondent";
  attrib S1246J   format=F01656_. label="Who forced to perform sexual acts: friend";
  attrib S1246K   format=F01657_. label="Who forced to perform sexual acts: teacher";
  attrib S1246L   format=F01658_. label="Who forced to perform sexual acts: boss";
  attrib S1246M   format=F01659_. label="Who forced to perform sexual acts: unknown person";
  attrib S1246X   format=F01660_. label="Who forced to perform sexual acts: other";
  attrib S1247    format=F01661_. label="When was raped took emergency contraception";
  attrib S1248    format=F01662_. label="As a result of the rape got pregnant";
  attrib S1249    format=F01663_. label="Did rape occurred after May 2006";
  attrib S1250    format=F01664_. label="Was told about abortion rights when raped";
  attrib S1251    format=F01665_. label="Requested termination of pregnancy";
  attrib S1252    format=F01666_. label="Was pregnancy termination performed";
  attrib S1254    format=F01667_. label="Has been forced to have sex to obtain money/benefits for others";
  attrib S1255A   format=F01668_. label="Who forced to have sex for money/benefits: mother/father";
  attrib S1255B   format=F01669_. label="Who forced to have sex for money/benefits: step parents";
  attrib S1255C   format=F01670_. label="Who forced to have sex for money/benefits: ex-partner";
  attrib S1255D   format=F01671_. label="Who forced to have sex for money/benefits: other relative";
  attrib S1255E   format=F01672_. label="Who forced to have sex for money/benefits: boyfriend";
  attrib S1255F   format=F01673_. label="Who forced to have sex for money/benefits: friend";
  attrib S1255G   format=F01674_. label="Who forced to have sex for money/benefits: teacher";
  attrib S1255H   format=F01675_. label="Who forced to have sex for money/benefits: boss";
  attrib S1255I   format=F01676_. label="Who forced to have sex for money/benefits: unknown person";
  attrib S1255X   format=F01677_. label="Who forced to have sex for money/benefits: other";
  attrib S1256    format=F01678_. label="Age when forced to have sex to get benefits to others";
  attrib S1257    format=F01679_. label="Has been sexually harassed";
  attrib S1258A   format=F01680_. label="Sexual harassment occurred in: public transportation";
  attrib S1258B   format=F01681_. label="Sexual harassment occurred in: street";
  attrib S1258C   format=F01682_. label="Sexual harassment occurred at: home";
  attrib S1258D   format=F01683_. label="Sexual harassment occurred at: school";
  attrib S1258E   format=F01684_. label="Sexual harassment occurred in: supermarket";
  attrib S1258F   format=F01685_. label="Sexual harassment occurred at: meeting";
  attrib S1258G   format=F01686_. label="Sexual harassment occurred at: bar/discotheque";
  attrib S1258H   format=F01687_. label="Sexual harassment occurred at: work";
  attrib S1258X   format=F01688_. label="Sexual harassment occurred at: other";
  attrib S1259    format=F01689_. label="Has felt uneasy or sexually harassed in a street";
  attrib S1260    format=F01690_. label="Have been requested pregnancy test when looking for a job";
  attrib S1261    format=F01691_. label="Have been requested sterilization prove when looking for a job";
  attrib S1262    format=F01692_. label="Have been requested HIV test when looking for a job";
  attrib S1263    format=F01693_. label="Have been fired from a job because of pregnancy";
  attrib S1267A   format=F01694_. label="Authority where help was sought: police inspection";
  attrib S1267B   format=F01695_. label="Authority where help was sought: family commissary";
  attrib S1267C   format=F01696_. label="Authority where help was sought: Colombia welfare institute (ICBF)";
  attrib S1267D   format=F01697_. label="Authority where help was sought: public attorney office (fiscalia)";
  attrib S1267E   format=F01698_. label="Authority where help was sought: court";
  attrib S1267F   format=F01699_. label="Authority where help was sought: health institution";
  attrib S1267G   format=F01700_. label="Authority where help was sought: PROFAMILIA";
  attrib S1267H   format=F01701_. label="Authority where help was sought: town official (personero)";
  attrib S1267I   format=F01702_. label="Authority where help was sought: house of justice";
  attrib S1267J   format=F01703_. label="Authority where help was sought: town office (defensoria)";
  attrib S1267X   format=F01704_. label="Authority where help was sought: other";
  attrib S1267Z   format=F01705_. label="Authority where help was sought: never denounced";
  attrib S1268    format=F01706_. label="Was violence denounced";
  attrib S1269A   format=F01707_. label="Authority where denouncement was placed: police inspection";
  attrib S1269B   format=F01708_. label="Authority where denouncement was placed: family commissary";
  attrib S1269C   format=F01709_. label="Authority where denouncement was placed: public attorney office (fiscalia)";
  attrib S1269D   format=F01710_. label="Authority where denouncement was placed: court";
  attrib S1269X   format=F01711_. label="Authority where denouncement was placed: other";
  attrib S1270A   format=F01712_. label="Result of denouncement was: sanction the aggressor";
  attrib S1270B   format=F01713_. label="Result of denouncement was: prohibited entering the house";
  attrib S1270C   format=F01714_. label="Result of denouncement was: prohibited from coming close to her";
  attrib S1270D   format=F01715_. label="Result of denouncement was: didn't receive any sanction";
  attrib S1270E   format=F01716_. label="Result of denouncement was: violence didn't stop";
  attrib S1270F   format=F01717_. label="Result of denouncement was: violence increased";
  attrib S1270G   format=F01718_. label="Result of denouncement was: were called for conciliation";
  attrib S1270X   format=F01719_. label="Result of denouncement was: other";
  attrib S1271A   format=F01720_. label="Reason didn't denounce person who mistreat: don't know where to go";
  attrib S1271B   format=F01721_. label="Reason didn't denounce person who mistreat: shame/humiliation";
  attrib S1271C   format=F01722_. label="Reason didn't denounce person who mistreat: it is part of life";
  attrib S1271D   format=F01723_. label="Reason didn't denounce person who mistreat: feel that deserves the abuse";
  attrib S1271E   format=F01724_. label="Reason didn't denounce person who mistreat: fear of divorce/separation";
  attrib S1271F   format=F01725_. label="Reason didn't denounce person who mistreat: fear of retaliation";
  attrib S1271G   format=F01726_. label="Reason didn't denounce person who mistreat: don't feel the damage was big";
  attrib S1271H   format=F01727_. label="Reason didn't denounce person who mistreat: don't want to cause harm to the aggressor";
  attrib S1271I   format=F01728_. label="Reason didn't denounce person who mistreat: think can resolve the problem herself";
  attrib S1271J   format=F01729_. label="Reason didn't denounce person who mistreat: don't believe in judicial system";
  attrib S1271K   format=F01730_. label="Reason didn't denounce person who mistreat: bad experience when denounced in previous occasions";
  attrib S1271L   format=F01731_. label="Reason didn't denounce person who mistreat: think it will not happen again";
  attrib S1271M   format=F01732_. label="Reason didn't denounce person who mistreat: still loves the aggressor";
  attrib S1271X   format=F01733_. label="Reason didn't denounce person who mistreat: other";
  attrib S1272    format=F01734_. label="Did go to see a doctor as a result of violence";
  attrib S1273    format=F01735_. label="Received information on how to denounce violence";
  attrib S1274A   format=F01736_. label="Does agree on statement: when men are mad it is better not to tempt them";
  attrib S1274B   format=F01737_. label="Does agree on statement: when women are mad it is better not to tempt them";
  attrib S1274C   format=F01738_. label="Does agree on statement: true men can control their partners";
  attrib S1274D   format=F01739_. label="Does agree on statement: sometimes it is good that men hit their partners";
  attrib S1274E   format=F01740_. label="Does agree on statement: it is OK to hit partner if she is unfaithful";
  attrib S1274F   format=F01741_. label="Does agree on statement: a good wife always obey her husband";
  attrib S1274G   format=F01742_. label="Does agree on statement: a women can choose her friends regardless of couple's wishes";
  attrib S1274H   format=F01743_. label="Does agree on statement: women that stay in a relation after being hit is because they like it";
  attrib S1274I   format=F01744_. label="Does agree on statement: it is normal that men don't allow their wives to go out alone";
  attrib S1276    format=F01745_. label="Has a friend who had verbally abused their wives";
  attrib S1277    format=F01746_. label="Would call the attention of a friend who verbally abuse a woman";
  attrib S1278A   format=F01747_. label="If a friend hit a woman: intervene at that moment";
  attrib S1278B   format=F01748_. label="If a friend hit a woman: talk to him later";
  attrib S1278C   format=F01749_. label="If a friend hit a woman: ignore it";
  attrib S1278D   format=F01750_. label="If a friend hit a woman: call the police";
  attrib S1278E   format=F01751_. label="If a friend hit a woman: talk to the neighbors to do something";
  attrib S1279A   format=F01752_. label="If an unknown person hit a woman: intervene at that moment";
  attrib S1279B   format=F01753_. label="If an unknown person hit a woman: talk to him later";
  attrib S1279C   format=F01754_. label="If an unknown person hit a woman: ignore it";
  attrib S1279D   format=F01755_. label="If an unknown person hit a woman: call the police";
  attrib S1279E   format=F01756_. label="If an unknown person hit a woman: talk to the neighbors to do something";
  attrib IDX94_1  label="Index to Birth History(1)";
  attrib IDX94_2  label="Index to Birth History(2)";
  attrib IDX94_3  label="Index to Birth History(3)";
  attrib IDX94_4  label="Index to Birth History(4)";
  attrib IDX94_5  label="Index to Birth History(5)";
  attrib IDX94_6  label="Index to Birth History(6)";
  attrib S412_1   format=F01757_. label="Months of pregnant at last control prenatal(1)";
  attrib S412_2   format=F01757_. label="Months of pregnant at last control prenatal(2)";
  attrib S412_3   format=F01757_. label="Months of pregnant at last control prenatal(3)";
  attrib S412_4   format=F01757_. label="Months of pregnant at last control prenatal(4)";
  attrib S412_5   format=F01757_. label="Months of pregnant at last control prenatal(5)";
  attrib S412_6   format=F01757_. label="Months of pregnant at last control prenatal(6)";
  attrib S413B_1  format=F01758_. label="Was uterine height measured(1)";
  attrib S413B_2  format=F01758_. label="Was uterine height measured(2)";
  attrib S413B_3  format=F01758_. label="Was uterine height measured(3)";
  attrib S413B_4  format=F01758_. label="Was uterine height measured(4)";
  attrib S413B_5  format=F01758_. label="Was uterine height measured(5)";
  attrib S413B_6  format=F01758_. label="Was uterine height measured(6)";
  attrib S413F_1  format=F01759_. label="Did hear the bay's heartbeat(1)";
  attrib S413F_2  format=F01759_. label="Did hear the bay's heartbeat(2)";
  attrib S413F_3  format=F01759_. label="Did hear the bay's heartbeat(3)";
  attrib S413F_4  format=F01759_. label="Did hear the bay's heartbeat(4)";
  attrib S413F_5  format=F01759_. label="Did hear the bay's heartbeat(5)";
  attrib S413F_6  format=F01759_. label="Did hear the bay's heartbeat(6)";
  attrib S414A_1  format=F01760_. label="Told about pregnancy physical complications(1)";
  attrib S414A_2  format=F01760_. label="Told about pregnancy physical complications(2)";
  attrib S414A_3  format=F01760_. label="Told about pregnancy physical complications(3)";
  attrib S414A_4  format=F01760_. label="Told about pregnancy physical complications(4)";
  attrib S414A_5  format=F01760_. label="Told about pregnancy physical complications(5)";
  attrib S414A_6  format=F01760_. label="Told about pregnancy physical complications(6)";
  attrib S414B_1  format=F01761_. label="Told about physical and emotional changes during pregnancy(1)";
  attrib S414B_2  format=F01761_. label="Told about physical and emotional changes during pregnancy(2)";
  attrib S414B_3  format=F01761_. label="Told about physical and emotional changes during pregnancy(3)";
  attrib S414B_4  format=F01761_. label="Told about physical and emotional changes during pregnancy(4)";
  attrib S414B_5  format=F01761_. label="Told about physical and emotional changes during pregnancy(5)";
  attrib S414B_6  format=F01761_. label="Told about physical and emotional changes during pregnancy(6)";
  attrib S416A_1  format=F01762_. label="Reason didn't have prenatal care: personal reasons(1)";
  attrib S416A_2  format=F01762_. label="Reason didn't have prenatal care: personal reasons(2)";
  attrib S416A_3  format=F01762_. label="Reason didn't have prenatal care: personal reasons(3)";
  attrib S416A_4  format=F01762_. label="Reason didn't have prenatal care: personal reasons(4)";
  attrib S416A_5  format=F01762_. label="Reason didn't have prenatal care: personal reasons(5)";
  attrib S416A_6  format=F01762_. label="Reason didn't have prenatal care: personal reasons(6)";
  attrib S416B_1  format=F01763_. label="Reason didn't have prenatal care: health services(1)";
  attrib S416B_2  format=F01763_. label="Reason didn't have prenatal care: health services(2)";
  attrib S416B_3  format=F01763_. label="Reason didn't have prenatal care: health services(3)";
  attrib S416B_4  format=F01763_. label="Reason didn't have prenatal care: health services(4)";
  attrib S416B_5  format=F01763_. label="Reason didn't have prenatal care: health services(5)";
  attrib S416B_6  format=F01763_. label="Reason didn't have prenatal care: health services(6)";
  attrib S416C_1  format=F01764_. label="Reason didn't have prenatal care: economic reasons(1)";
  attrib S416C_2  format=F01764_. label="Reason didn't have prenatal care: economic reasons(2)";
  attrib S416C_3  format=F01764_. label="Reason didn't have prenatal care: economic reasons(3)";
  attrib S416C_4  format=F01764_. label="Reason didn't have prenatal care: economic reasons(4)";
  attrib S416C_5  format=F01764_. label="Reason didn't have prenatal care: economic reasons(5)";
  attrib S416C_6  format=F01764_. label="Reason didn't have prenatal care: economic reasons(6)";
  attrib S416D_1  format=F01765_. label="Reason didn't have prenatal care: authorization problems(1)";
  attrib S416D_2  format=F01765_. label="Reason didn't have prenatal care: authorization problems(2)";
  attrib S416D_3  format=F01765_. label="Reason didn't have prenatal care: authorization problems(3)";
  attrib S416D_4  format=F01765_. label="Reason didn't have prenatal care: authorization problems(4)";
  attrib S416D_5  format=F01765_. label="Reason didn't have prenatal care: authorization problems(5)";
  attrib S416D_6  format=F01765_. label="Reason didn't have prenatal care: authorization problems(6)";
  attrib S416E_1  format=F01766_. label="Reason didn't have prenatal care: lack of knowledge(1)";
  attrib S416E_2  format=F01766_. label="Reason didn't have prenatal care: lack of knowledge(2)";
  attrib S416E_3  format=F01766_. label="Reason didn't have prenatal care: lack of knowledge(3)";
  attrib S416E_4  format=F01766_. label="Reason didn't have prenatal care: lack of knowledge(4)";
  attrib S416E_5  format=F01766_. label="Reason didn't have prenatal care: lack of knowledge(5)";
  attrib S416E_6  format=F01766_. label="Reason didn't have prenatal care: lack of knowledge(6)";
  attrib S416F_1  format=F01767_. label="Reason didn't have prenatal care: Opposition(1)";
  attrib S416F_2  format=F01767_. label="Reason didn't have prenatal care: Opposition(2)";
  attrib S416F_3  format=F01767_. label="Reason didn't have prenatal care: Opposition(3)";
  attrib S416F_4  format=F01767_. label="Reason didn't have prenatal care: Opposition(4)";
  attrib S416F_5  format=F01767_. label="Reason didn't have prenatal care: Opposition(5)";
  attrib S416F_6  format=F01767_. label="Reason didn't have prenatal care: Opposition(6)";
  attrib S416G_1  format=F01768_. label="Reason didn't have prenatal care: wasn't affiliated to health insurance(1)";
  attrib S416G_2  format=F01768_. label="Reason didn't have prenatal care: wasn't affiliated to health insurance(2)";
  attrib S416G_3  format=F01768_. label="Reason didn't have prenatal care: wasn't affiliated to health insurance(3)";
  attrib S416G_4  format=F01768_. label="Reason didn't have prenatal care: wasn't affiliated to health insurance(4)";
  attrib S416G_5  format=F01768_. label="Reason didn't have prenatal care: wasn't affiliated to health insurance(5)";
  attrib S416G_6  format=F01768_. label="Reason didn't have prenatal care: wasn't affiliated to health insurance(6)";
  attrib S416H_1  format=F01769_. label="Reason didn't have prenatal care: didn't know that could use the service(1)";
  attrib S416H_2  format=F01769_. label="Reason didn't have prenatal care: didn't know that could use the service(2)";
  attrib S416H_3  format=F01769_. label="Reason didn't have prenatal care: didn't know that could use the service(3)";
  attrib S416H_4  format=F01769_. label="Reason didn't have prenatal care: didn't know that could use the service(4)";
  attrib S416H_5  format=F01769_. label="Reason didn't have prenatal care: didn't know that could use the service(5)";
  attrib S416H_6  format=F01769_. label="Reason didn't have prenatal care: didn't know that could use the service(6)";
  attrib S416X_1  format=F01770_. label="Reason didn't have prenatal care: other(1)";
  attrib S416X_2  format=F01770_. label="Reason didn't have prenatal care: other(2)";
  attrib S416X_3  format=F01770_. label="Reason didn't have prenatal care: other(3)";
  attrib S416X_4  format=F01770_. label="Reason didn't have prenatal care: other(4)";
  attrib S416X_5  format=F01770_. label="Reason didn't have prenatal care: other(5)";
  attrib S416X_6  format=F01770_. label="Reason didn't have prenatal care: other(6)";
  attrib S420_1   format=F01771_. label="Health insurance provided iron(1)";
  attrib S420_2   format=F01771_. label="Health insurance provided iron(2)";
  attrib S420_3   format=F01771_. label="Health insurance provided iron(3)";
  attrib S420_4   format=F01771_. label="Health insurance provided iron(4)";
  attrib S420_5   format=F01771_. label="Health insurance provided iron(5)";
  attrib S420_6   format=F01771_. label="Health insurance provided iron(6)";
  attrib S421_1   format=F01772_. label="Months took iron supplement during pregnancy(1)";
  attrib S421_2   format=F01772_. label="Months took iron supplement during pregnancy(2)";
  attrib S421_3   format=F01772_. label="Months took iron supplement during pregnancy(3)";
  attrib S421_4   format=F01772_. label="Months took iron supplement during pregnancy(4)";
  attrib S421_5   format=F01772_. label="Months took iron supplement during pregnancy(5)";
  attrib S421_6   format=F01772_. label="Months took iron supplement during pregnancy(6)";
  attrib S422_1   format=F01773_. label="During pregnancy was prescribed calcium supplements(1)";
  attrib S422_2   format=F01773_. label="During pregnancy was prescribed calcium supplements(2)";
  attrib S422_3   format=F01773_. label="During pregnancy was prescribed calcium supplements(3)";
  attrib S422_4   format=F01773_. label="During pregnancy was prescribed calcium supplements(4)";
  attrib S422_5   format=F01773_. label="During pregnancy was prescribed calcium supplements(5)";
  attrib S422_6   format=F01773_. label="During pregnancy was prescribed calcium supplements(6)";
  attrib S423_1   format=F01774_. label="Health insurance provided calcium supplements(1)";
  attrib S423_2   format=F01774_. label="Health insurance provided calcium supplements(2)";
  attrib S423_3   format=F01774_. label="Health insurance provided calcium supplements(3)";
  attrib S423_4   format=F01774_. label="Health insurance provided calcium supplements(4)";
  attrib S423_5   format=F01774_. label="Health insurance provided calcium supplements(5)";
  attrib S423_6   format=F01774_. label="Health insurance provided calcium supplements(6)";
  attrib S424_1   format=F01775_. label="Months took calcium supplement(1)";
  attrib S424_2   format=F01775_. label="Months took calcium supplement(2)";
  attrib S424_3   format=F01775_. label="Months took calcium supplement(3)";
  attrib S424_4   format=F01775_. label="Months took calcium supplement(4)";
  attrib S424_5   format=F01775_. label="Months took calcium supplement(5)";
  attrib S424_6   format=F01775_. label="Months took calcium supplement(6)";
  attrib S425_1   format=F01776_. label="During pregnancy was prescribed folic acid(1)";
  attrib S425_2   format=F01776_. label="During pregnancy was prescribed folic acid(2)";
  attrib S425_3   format=F01776_. label="During pregnancy was prescribed folic acid(3)";
  attrib S425_4   format=F01776_. label="During pregnancy was prescribed folic acid(4)";
  attrib S425_5   format=F01776_. label="During pregnancy was prescribed folic acid(5)";
  attrib S425_6   format=F01776_. label="During pregnancy was prescribed folic acid(6)";
  attrib S426_1   format=F01777_. label="Health insurance provided folic acid(1)";
  attrib S426_2   format=F01777_. label="Health insurance provided folic acid(2)";
  attrib S426_3   format=F01777_. label="Health insurance provided folic acid(3)";
  attrib S426_4   format=F01777_. label="Health insurance provided folic acid(4)";
  attrib S426_5   format=F01777_. label="Health insurance provided folic acid(5)";
  attrib S426_6   format=F01777_. label="Health insurance provided folic acid(6)";
  attrib S427_1   format=F01778_. label="Months took folic acid(1)";
  attrib S427_2   format=F01778_. label="Months took folic acid(2)";
  attrib S427_3   format=F01778_. label="Months took folic acid(3)";
  attrib S427_4   format=F01778_. label="Months took folic acid(4)";
  attrib S427_5   format=F01778_. label="Months took folic acid(5)";
  attrib S427_6   format=F01778_. label="Months took folic acid(6)";
  attrib S428_1   format=F01779_. label="During pregnancy took alcoholic beverages(1)";
  attrib S428_2   format=F01779_. label="During pregnancy took alcoholic beverages(2)";
  attrib S428_3   format=F01779_. label="During pregnancy took alcoholic beverages(3)";
  attrib S428_4   format=F01779_. label="During pregnancy took alcoholic beverages(4)";
  attrib S428_5   format=F01779_. label="During pregnancy took alcoholic beverages(5)";
  attrib S428_6   format=F01779_. label="During pregnancy took alcoholic beverages(6)";
  attrib S429_1   format=F01780_. label="Number of drinks (glasses) took in a week(1)";
  attrib S429_2   format=F01780_. label="Number of drinks (glasses) took in a week(2)";
  attrib S429_3   format=F01780_. label="Number of drinks (glasses) took in a week(3)";
  attrib S429_4   format=F01780_. label="Number of drinks (glasses) took in a week(4)";
  attrib S429_5   format=F01780_. label="Number of drinks (glasses) took in a week(5)";
  attrib S429_6   format=F01780_. label="Number of drinks (glasses) took in a week(6)";
  attrib S430_1   format=F01781_. label="During pregnancy smoked cigarettes(1)";
  attrib S430_2   format=F01781_. label="During pregnancy smoked cigarettes(2)";
  attrib S430_3   format=F01781_. label="During pregnancy smoked cigarettes(3)";
  attrib S430_4   format=F01781_. label="During pregnancy smoked cigarettes(4)";
  attrib S430_5   format=F01781_. label="During pregnancy smoked cigarettes(5)";
  attrib S430_6   format=F01781_. label="During pregnancy smoked cigarettes(6)";
  attrib S431_1   format=F01782_. label="Number of cigarettes smoked daily(1)";
  attrib S431_2   format=F01782_. label="Number of cigarettes smoked daily(2)";
  attrib S431_3   format=F01782_. label="Number of cigarettes smoked daily(3)";
  attrib S431_4   format=F01782_. label="Number of cigarettes smoked daily(4)";
  attrib S431_5   format=F01782_. label="Number of cigarettes smoked daily(5)";
  attrib S431_6   format=F01782_. label="Number of cigarettes smoked daily(6)";
  attrib S433A_1  format=F01783_. label="During pregnancy smoked marijuana(1)";
  attrib S433A_2  format=F01783_. label="During pregnancy smoked marijuana(2)";
  attrib S433A_3  format=F01783_. label="During pregnancy smoked marijuana(3)";
  attrib S433A_4  format=F01783_. label="During pregnancy smoked marijuana(4)";
  attrib S433A_5  format=F01783_. label="During pregnancy smoked marijuana(5)";
  attrib S433A_6  format=F01783_. label="During pregnancy smoked marijuana(6)";
  attrib S433B_1  format=F01784_. label="During pregnancy smoked used crack(1)";
  attrib S433B_2  format=F01784_. label="During pregnancy smoked used crack(2)";
  attrib S433B_3  format=F01784_. label="During pregnancy smoked used crack(3)";
  attrib S433B_4  format=F01784_. label="During pregnancy smoked used crack(4)";
  attrib S433B_5  format=F01784_. label="During pregnancy smoked used crack(5)";
  attrib S433B_6  format=F01784_. label="During pregnancy smoked used crack(6)";
  attrib S433C_1  format=F01785_. label="During pregnancy used cocaine(1)";
  attrib S433C_2  format=F01785_. label="During pregnancy used cocaine(2)";
  attrib S433C_3  format=F01785_. label="During pregnancy used cocaine(3)";
  attrib S433C_4  format=F01785_. label="During pregnancy used cocaine(4)";
  attrib S433C_5  format=F01785_. label="During pregnancy used cocaine(5)";
  attrib S433C_6  format=F01785_. label="During pregnancy used cocaine(6)";
  attrib S433X_1  format=F01786_. label="During pregnancy used other(1)";
  attrib S433X_2  format=F01786_. label="During pregnancy used other(2)";
  attrib S433X_3  format=F01786_. label="During pregnancy used other(3)";
  attrib S433X_4  format=F01786_. label="During pregnancy used other(4)";
  attrib S433X_5  format=F01786_. label="During pregnancy used other(5)";
  attrib S433X_6  format=F01786_. label="During pregnancy used other(6)";
  attrib S434_1   format=F01787_. label="Frequency took the drug most commonly used(1)";
  attrib S434_2   format=F01787_. label="Frequency took the drug most commonly used(2)";
  attrib S434_3   format=F01787_. label="Frequency took the drug most commonly used(3)";
  attrib S434_4   format=F01787_. label="Frequency took the drug most commonly used(4)";
  attrib S434_5   format=F01787_. label="Frequency took the drug most commonly used(5)";
  attrib S434_6   format=F01787_. label="Frequency took the drug most commonly used(6)";
  attrib S437_1   format=F01788_. label="Hospitalized during pregnancy(1)";
  attrib S437_2   format=F01788_. label="Hospitalized during pregnancy(2)";
  attrib S437_3   format=F01788_. label="Hospitalized during pregnancy(3)";
  attrib S437_4   format=F01788_. label="Hospitalized during pregnancy(4)";
  attrib S437_5   format=F01788_. label="Hospitalized during pregnancy(5)";
  attrib S437_6   format=F01788_. label="Hospitalized during pregnancy(6)";
  attrib S438A_1  format=F01789_. label="Reason hospitalization: infection(1)";
  attrib S438A_2  format=F01789_. label="Reason hospitalization: infection(2)";
  attrib S438A_3  format=F01789_. label="Reason hospitalization: infection(3)";
  attrib S438A_4  format=F01789_. label="Reason hospitalization: infection(4)";
  attrib S438A_5  format=F01789_. label="Reason hospitalization: infection(5)";
  attrib S438A_6  format=F01789_. label="Reason hospitalization: infection(6)";
  attrib S438B_1  format=F01790_. label="Reason hospitalization: bleeding(1)";
  attrib S438B_2  format=F01790_. label="Reason hospitalization: bleeding(2)";
  attrib S438B_3  format=F01790_. label="Reason hospitalization: bleeding(3)";
  attrib S438B_4  format=F01790_. label="Reason hospitalization: bleeding(4)";
  attrib S438B_5  format=F01790_. label="Reason hospitalization: bleeding(5)";
  attrib S438B_6  format=F01790_. label="Reason hospitalization: bleeding(6)";
  attrib S438C_1  format=F01791_. label="Reason hospitalization: hypertension(1)";
  attrib S438C_2  format=F01791_. label="Reason hospitalization: hypertension(2)";
  attrib S438C_3  format=F01791_. label="Reason hospitalization: hypertension(3)";
  attrib S438C_4  format=F01791_. label="Reason hospitalization: hypertension(4)";
  attrib S438C_5  format=F01791_. label="Reason hospitalization: hypertension(5)";
  attrib S438C_6  format=F01791_. label="Reason hospitalization: hypertension(6)";
  attrib S438D_1  format=F01792_. label="Reason hospitalization: convulsions(1)";
  attrib S438D_2  format=F01792_. label="Reason hospitalization: convulsions(2)";
  attrib S438D_3  format=F01792_. label="Reason hospitalization: convulsions(3)";
  attrib S438D_4  format=F01792_. label="Reason hospitalization: convulsions(4)";
  attrib S438D_5  format=F01792_. label="Reason hospitalization: convulsions(5)";
  attrib S438D_6  format=F01792_. label="Reason hospitalization: convulsions(6)";
  attrib S438E_1  format=F01793_. label="Reason hospitalization: abortion threat(1)";
  attrib S438E_2  format=F01793_. label="Reason hospitalization: abortion threat(2)";
  attrib S438E_3  format=F01793_. label="Reason hospitalization: abortion threat(3)";
  attrib S438E_4  format=F01793_. label="Reason hospitalization: abortion threat(4)";
  attrib S438E_5  format=F01793_. label="Reason hospitalization: abortion threat(5)";
  attrib S438E_6  format=F01793_. label="Reason hospitalization: abortion threat(6)";
  attrib S438F_1  format=F01794_. label="Reason hospitalization: broken membranes(1)";
  attrib S438F_2  format=F01794_. label="Reason hospitalization: broken membranes(2)";
  attrib S438F_3  format=F01794_. label="Reason hospitalization: broken membranes(3)";
  attrib S438F_4  format=F01794_. label="Reason hospitalization: broken membranes(4)";
  attrib S438F_5  format=F01794_. label="Reason hospitalization: broken membranes(5)";
  attrib S438F_6  format=F01794_. label="Reason hospitalization: broken membranes(6)";
  attrib S438G_1  format=F01795_. label="Reason hospitalization: violence within family(1)";
  attrib S438G_2  format=F01795_. label="Reason hospitalization: violence within family(2)";
  attrib S438G_3  format=F01795_. label="Reason hospitalization: violence within family(3)";
  attrib S438G_4  format=F01795_. label="Reason hospitalization: violence within family(4)";
  attrib S438G_5  format=F01795_. label="Reason hospitalization: violence within family(5)";
  attrib S438G_6  format=F01795_. label="Reason hospitalization: violence within family(6)";
  attrib S438H_1  format=F01796_. label="Reason hospitalization: problems with the baby(1)";
  attrib S438H_2  format=F01796_. label="Reason hospitalization: problems with the baby(2)";
  attrib S438H_3  format=F01796_. label="Reason hospitalization: problems with the baby(3)";
  attrib S438H_4  format=F01796_. label="Reason hospitalization: problems with the baby(4)";
  attrib S438H_5  format=F01796_. label="Reason hospitalization: problems with the baby(5)";
  attrib S438H_6  format=F01796_. label="Reason hospitalization: problems with the baby(6)";
  attrib S438I_1  format=F01797_. label="Reason hospitalization: mother illness(1)";
  attrib S438I_2  format=F01797_. label="Reason hospitalization: mother illness(2)";
  attrib S438I_3  format=F01797_. label="Reason hospitalization: mother illness(3)";
  attrib S438I_4  format=F01797_. label="Reason hospitalization: mother illness(4)";
  attrib S438I_5  format=F01797_. label="Reason hospitalization: mother illness(5)";
  attrib S438I_6  format=F01797_. label="Reason hospitalization: mother illness(6)";
  attrib S438J_1  format=F01798_. label="Reason hospitalization: preeclampsia(1)";
  attrib S438J_2  format=F01798_. label="Reason hospitalization: preeclampsia(2)";
  attrib S438J_3  format=F01798_. label="Reason hospitalization: preeclampsia(3)";
  attrib S438J_4  format=F01798_. label="Reason hospitalization: preeclampsia(4)";
  attrib S438J_5  format=F01798_. label="Reason hospitalization: preeclampsia(5)";
  attrib S438J_6  format=F01798_. label="Reason hospitalization: preeclampsia(6)";
  attrib S438X_1  format=F01799_. label="Reason hospitalization: other(1)";
  attrib S438X_2  format=F01799_. label="Reason hospitalization: other(2)";
  attrib S438X_3  format=F01799_. label="Reason hospitalization: other(3)";
  attrib S438X_4  format=F01799_. label="Reason hospitalization: other(4)";
  attrib S438X_5  format=F01799_. label="Reason hospitalization: other(5)";
  attrib S438X_6  format=F01799_. label="Reason hospitalization: other(6)";
  attrib S441_1   format=F01800_. label="Caesarean was programmed or an urgency(1)";
  attrib S441_2   format=F01800_. label="Caesarean was programmed or an urgency(2)";
  attrib S441_3   format=F01800_. label="Caesarean was programmed or an urgency(3)";
  attrib S441_4   format=F01800_. label="Caesarean was programmed or an urgency(4)";
  attrib S441_5   format=F01800_. label="Caesarean was programmed or an urgency(5)";
  attrib S441_6   format=F01800_. label="Caesarean was programmed or an urgency(6)";
  attrib S444A_1  format=F01801_. label="Complications at delivery: prolonged labor(1)";
  attrib S444A_2  format=F01801_. label="Complications at delivery: prolonged labor(2)";
  attrib S444A_3  format=F01801_. label="Complications at delivery: prolonged labor(3)";
  attrib S444A_4  format=F01801_. label="Complications at delivery: prolonged labor(4)";
  attrib S444A_5  format=F01801_. label="Complications at delivery: prolonged labor(5)";
  attrib S444A_6  format=F01801_. label="Complications at delivery: prolonged labor(6)";
  attrib S444B_1  format=F01802_. label="Complications at delivery: excessive bleeding(1)";
  attrib S444B_2  format=F01802_. label="Complications at delivery: excessive bleeding(2)";
  attrib S444B_3  format=F01802_. label="Complications at delivery: excessive bleeding(3)";
  attrib S444B_4  format=F01802_. label="Complications at delivery: excessive bleeding(4)";
  attrib S444B_5  format=F01802_. label="Complications at delivery: excessive bleeding(5)";
  attrib S444B_6  format=F01802_. label="Complications at delivery: excessive bleeding(6)";
  attrib S444C_1  format=F01803_. label="Complications at delivery: fever with vaginal discharge(1)";
  attrib S444C_2  format=F01803_. label="Complications at delivery: fever with vaginal discharge(2)";
  attrib S444C_3  format=F01803_. label="Complications at delivery: fever with vaginal discharge(3)";
  attrib S444C_4  format=F01803_. label="Complications at delivery: fever with vaginal discharge(4)";
  attrib S444C_5  format=F01803_. label="Complications at delivery: fever with vaginal discharge(5)";
  attrib S444C_6  format=F01803_. label="Complications at delivery: fever with vaginal discharge(6)";
  attrib S444D_1  format=F01804_. label="Complications at delivery: convulsions(1)";
  attrib S444D_2  format=F01804_. label="Complications at delivery: convulsions(2)";
  attrib S444D_3  format=F01804_. label="Complications at delivery: convulsions(3)";
  attrib S444D_4  format=F01804_. label="Complications at delivery: convulsions(4)";
  attrib S444D_5  format=F01804_. label="Complications at delivery: convulsions(5)";
  attrib S444D_6  format=F01804_. label="Complications at delivery: convulsions(6)";
  attrib S444E_1  format=F01805_. label="Complications at delivery: other(1)";
  attrib S444E_2  format=F01805_. label="Complications at delivery: other(2)";
  attrib S444E_3  format=F01805_. label="Complications at delivery: other(3)";
  attrib S444E_4  format=F01805_. label="Complications at delivery: other(4)";
  attrib S444E_5  format=F01805_. label="Complications at delivery: other(5)";
  attrib S444E_6  format=F01805_. label="Complications at delivery: other(6)";
  attrib S445_1   format=F01806_. label="Was a premature child(1)";
  attrib S445_2   format=F01806_. label="Was a premature child(2)";
  attrib S445_3   format=F01806_. label="Was a premature child(3)";
  attrib S445_4   format=F01806_. label="Was a premature child(4)";
  attrib S445_5   format=F01806_. label="Was a premature child(5)";
  attrib S445_6   format=F01806_. label="Was a premature child(6)";
  attrib S446_1   label="Number of weeks pregnant at delivery(1)";
  attrib S446_2   label="Number of weeks pregnant at delivery(2)";
  attrib S446_3   label="Number of weeks pregnant at delivery(3)";
  attrib S446_4   label="Number of weeks pregnant at delivery(4)";
  attrib S446_5   label="Number of weeks pregnant at delivery(5)";
  attrib S446_6   label="Number of weeks pregnant at delivery(6)";
  attrib S449_1   format=F01807_. label="Was child measured at birth(1)";
  attrib S449_2   format=F01807_. label="Was child measured at birth(2)";
  attrib S449_3   format=F01807_. label="Was child measured at birth(3)";
  attrib S449_4   format=F01807_. label="Was child measured at birth(4)";
  attrib S449_5   format=F01807_. label="Was child measured at birth(5)";
  attrib S449_6   format=F01807_. label="Was child measured at birth(6)";
  attrib S450_1   format=F01808_. label="Height of child at birth (centimeters)(1)";
  attrib S450_2   format=F01808_. label="Height of child at birth (centimeters)(2)";
  attrib S450_3   format=F01808_. label="Height of child at birth (centimeters)(3)";
  attrib S450_4   format=F01808_. label="Height of child at birth (centimeters)(4)";
  attrib S450_5   format=F01808_. label="Height of child at birth (centimeters)(5)";
  attrib S450_6   format=F01808_. label="Height of child at birth (centimeters)(6)";
  attrib S452A_1  format=F01809_. label="Complications after delivery: excessive bleeding(1)";
  attrib S452A_2  format=F01809_. label="Complications after delivery: excessive bleeding(2)";
  attrib S452A_3  format=F01809_. label="Complications after delivery: excessive bleeding(3)";
  attrib S452A_4  format=F01809_. label="Complications after delivery: excessive bleeding(4)";
  attrib S452A_5  format=F01809_. label="Complications after delivery: excessive bleeding(5)";
  attrib S452A_6  format=F01809_. label="Complications after delivery: excessive bleeding(6)";
  attrib S452B_1  format=F01810_. label="Complications after delivery: loss of conscience(1)";
  attrib S452B_2  format=F01810_. label="Complications after delivery: loss of conscience(2)";
  attrib S452B_3  format=F01810_. label="Complications after delivery: loss of conscience(3)";
  attrib S452B_4  format=F01810_. label="Complications after delivery: loss of conscience(4)";
  attrib S452B_5  format=F01810_. label="Complications after delivery: loss of conscience(5)";
  attrib S452B_6  format=F01810_. label="Complications after delivery: loss of conscience(6)";
  attrib S452C_1  format=F01811_. label="Complications after delivery: fever, high temperature(1)";
  attrib S452C_2  format=F01811_. label="Complications after delivery: fever, high temperature(2)";
  attrib S452C_3  format=F01811_. label="Complications after delivery: fever, high temperature(3)";
  attrib S452C_4  format=F01811_. label="Complications after delivery: fever, high temperature(4)";
  attrib S452C_5  format=F01811_. label="Complications after delivery: fever, high temperature(5)";
  attrib S452C_6  format=F01811_. label="Complications after delivery: fever, high temperature(6)";
  attrib S452D_1  format=F01812_. label="Complications after delivery: breast infection(1)";
  attrib S452D_2  format=F01812_. label="Complications after delivery: breast infection(2)";
  attrib S452D_3  format=F01812_. label="Complications after delivery: breast infection(3)";
  attrib S452D_4  format=F01812_. label="Complications after delivery: breast infection(4)";
  attrib S452D_5  format=F01812_. label="Complications after delivery: breast infection(5)";
  attrib S452D_6  format=F01812_. label="Complications after delivery: breast infection(6)";
  attrib S452E_1  format=F01813_. label="Complications after delivery: pain when urinate(1)";
  attrib S452E_2  format=F01813_. label="Complications after delivery: pain when urinate(2)";
  attrib S452E_3  format=F01813_. label="Complications after delivery: pain when urinate(3)";
  attrib S452E_4  format=F01813_. label="Complications after delivery: pain when urinate(4)";
  attrib S452E_5  format=F01813_. label="Complications after delivery: pain when urinate(5)";
  attrib S452E_6  format=F01813_. label="Complications after delivery: pain when urinate(6)";
  attrib S452F_1  format=F01814_. label="Complications after delivery: vaginal discharges(1)";
  attrib S452F_2  format=F01814_. label="Complications after delivery: vaginal discharges(2)";
  attrib S452F_3  format=F01814_. label="Complications after delivery: vaginal discharges(3)";
  attrib S452F_4  format=F01814_. label="Complications after delivery: vaginal discharges(4)";
  attrib S452F_5  format=F01814_. label="Complications after delivery: vaginal discharges(5)";
  attrib S452F_6  format=F01814_. label="Complications after delivery: vaginal discharges(6)";
  attrib S452G_1  format=F01815_. label="Complications after delivery: involuntary loss of urine(1)";
  attrib S452G_2  format=F01815_. label="Complications after delivery: involuntary loss of urine(2)";
  attrib S452G_3  format=F01815_. label="Complications after delivery: involuntary loss of urine(3)";
  attrib S452G_4  format=F01815_. label="Complications after delivery: involuntary loss of urine(4)";
  attrib S452G_5  format=F01815_. label="Complications after delivery: involuntary loss of urine(5)";
  attrib S452G_6  format=F01815_. label="Complications after delivery: involuntary loss of urine(6)";
  attrib S452H_1  format=F01816_. label="Complications after delivery: postpartum depression(1)";
  attrib S452H_2  format=F01816_. label="Complications after delivery: postpartum depression(2)";
  attrib S452H_3  format=F01816_. label="Complications after delivery: postpartum depression(3)";
  attrib S452H_4  format=F01816_. label="Complications after delivery: postpartum depression(4)";
  attrib S452H_5  format=F01816_. label="Complications after delivery: postpartum depression(5)";
  attrib S452H_6  format=F01816_. label="Complications after delivery: postpartum depression(6)";
  attrib S454_1   format=F01817_. label="Received medical attention because of complications(1)";
  attrib S454_2   format=F01817_. label="Received medical attention because of complications(2)";
  attrib S454_3   format=F01817_. label="Received medical attention because of complications(3)";
  attrib S454_4   format=F01817_. label="Received medical attention because of complications(4)";
  attrib S454_5   format=F01817_. label="Received medical attention because of complications(5)";
  attrib S454_6   format=F01817_. label="Received medical attention because of complications(6)";
  attrib S457A_1  format=F01818_. label="Postpartum check-up by: doctor(1)";
  attrib S457A_2  format=F01818_. label="Postpartum check-up by: doctor(2)";
  attrib S457A_3  format=F01818_. label="Postpartum check-up by: doctor(3)";
  attrib S457A_4  format=F01818_. label="Postpartum check-up by: doctor(4)";
  attrib S457A_5  format=F01818_. label="Postpartum check-up by: doctor(5)";
  attrib S457A_6  format=F01818_. label="Postpartum check-up by: doctor(6)";
  attrib S457B_1  format=F01819_. label="Postpartum check-up by: nurse(1)";
  attrib S457B_2  format=F01819_. label="Postpartum check-up by: nurse(2)";
  attrib S457B_3  format=F01819_. label="Postpartum check-up by: nurse(3)";
  attrib S457B_4  format=F01819_. label="Postpartum check-up by: nurse(4)";
  attrib S457B_5  format=F01819_. label="Postpartum check-up by: nurse(5)";
  attrib S457B_6  format=F01819_. label="Postpartum check-up by: nurse(6)";
  attrib S457C_1  format=F01820_. label="Postpartum check-up by: auxiliary nurse(1)";
  attrib S457C_2  format=F01820_. label="Postpartum check-up by: auxiliary nurse(2)";
  attrib S457C_3  format=F01820_. label="Postpartum check-up by: auxiliary nurse(3)";
  attrib S457C_4  format=F01820_. label="Postpartum check-up by: auxiliary nurse(4)";
  attrib S457C_5  format=F01820_. label="Postpartum check-up by: auxiliary nurse(5)";
  attrib S457C_6  format=F01820_. label="Postpartum check-up by: auxiliary nurse(6)";
  attrib S457D_1  format=F01821_. label="Postpartum check-up by: traditional birth attendant(1)";
  attrib S457D_2  format=F01821_. label="Postpartum check-up by: traditional birth attendant(2)";
  attrib S457D_3  format=F01821_. label="Postpartum check-up by: traditional birth attendant(3)";
  attrib S457D_4  format=F01821_. label="Postpartum check-up by: traditional birth attendant(4)";
  attrib S457D_5  format=F01821_. label="Postpartum check-up by: traditional birth attendant(5)";
  attrib S457D_6  format=F01821_. label="Postpartum check-up by: traditional birth attendant(6)";
  attrib S457X_1  format=F01822_. label="Postpartum check-up by: other(1)";
  attrib S457X_2  format=F01822_. label="Postpartum check-up by: other(2)";
  attrib S457X_3  format=F01822_. label="Postpartum check-up by: other(3)";
  attrib S457X_4  format=F01822_. label="Postpartum check-up by: other(4)";
  attrib S457X_5  format=F01822_. label="Postpartum check-up by: other(5)";
  attrib S457X_6  format=F01822_. label="Postpartum check-up by: other(6)";
  attrib S458A_1  format=F01823_. label="Received post-partum contraception information(1)";
  attrib S458A_2  format=F01823_. label="Received post-partum contraception information(2)";
  attrib S458A_3  format=F01823_. label="Received post-partum contraception information(3)";
  attrib S458A_4  format=F01823_. label="Received post-partum contraception information(4)";
  attrib S458A_5  format=F01823_. label="Received post-partum contraception information(5)";
  attrib S458A_6  format=F01823_. label="Received post-partum contraception information(6)";
  attrib S458B_1  format=F01824_. label="When contraception post-partum information was received(1)";
  attrib S458B_2  format=F01824_. label="When contraception post-partum information was received(2)";
  attrib S458B_3  format=F01824_. label="When contraception post-partum information was received(3)";
  attrib S458B_4  format=F01824_. label="When contraception post-partum information was received(4)";
  attrib S458B_5  format=F01824_. label="When contraception post-partum information was received(5)";
  attrib S458B_6  format=F01824_. label="When contraception post-partum information was received(6)";
  attrib S458C_1  format=F01825_. label="Was post-partum contraception method provided immediately(1)";
  attrib S458C_2  format=F01825_. label="Was post-partum contraception method provided immediately(2)";
  attrib S458C_3  format=F01825_. label="Was post-partum contraception method provided immediately(3)";
  attrib S458C_4  format=F01825_. label="Was post-partum contraception method provided immediately(4)";
  attrib S458C_5  format=F01825_. label="Was post-partum contraception method provided immediately(5)";
  attrib S458C_6  format=F01825_. label="Was post-partum contraception method provided immediately(6)";
  attrib S459A_1  format=F01826_. label="Reason for no postpartum check-up: personal reasons(1)";
  attrib S459A_2  format=F01826_. label="Reason for no postpartum check-up: personal reasons(2)";
  attrib S459A_3  format=F01826_. label="Reason for no postpartum check-up: personal reasons(3)";
  attrib S459A_4  format=F01826_. label="Reason for no postpartum check-up: personal reasons(4)";
  attrib S459A_5  format=F01826_. label="Reason for no postpartum check-up: personal reasons(5)";
  attrib S459A_6  format=F01826_. label="Reason for no postpartum check-up: personal reasons(6)";
  attrib S459B_1  format=F01827_. label="Reason for no postpartum check-up: health services(1)";
  attrib S459B_2  format=F01827_. label="Reason for no postpartum check-up: health services(2)";
  attrib S459B_3  format=F01827_. label="Reason for no postpartum check-up: health services(3)";
  attrib S459B_4  format=F01827_. label="Reason for no postpartum check-up: health services(4)";
  attrib S459B_5  format=F01827_. label="Reason for no postpartum check-up: health services(5)";
  attrib S459B_6  format=F01827_. label="Reason for no postpartum check-up: health services(6)";
  attrib S459C_1  format=F01828_. label="Reason for no postpartum check-up: economic reasons(1)";
  attrib S459C_2  format=F01828_. label="Reason for no postpartum check-up: economic reasons(2)";
  attrib S459C_3  format=F01828_. label="Reason for no postpartum check-up: economic reasons(3)";
  attrib S459C_4  format=F01828_. label="Reason for no postpartum check-up: economic reasons(4)";
  attrib S459C_5  format=F01828_. label="Reason for no postpartum check-up: economic reasons(5)";
  attrib S459C_6  format=F01828_. label="Reason for no postpartum check-up: economic reasons(6)";
  attrib S459D_1  format=F01829_. label="Reason for no postpartum check-up: authorization related problems(1)";
  attrib S459D_2  format=F01829_. label="Reason for no postpartum check-up: authorization related problems(2)";
  attrib S459D_3  format=F01829_. label="Reason for no postpartum check-up: authorization related problems(3)";
  attrib S459D_4  format=F01829_. label="Reason for no postpartum check-up: authorization related problems(4)";
  attrib S459D_5  format=F01829_. label="Reason for no postpartum check-up: authorization related problems(5)";
  attrib S459D_6  format=F01829_. label="Reason for no postpartum check-up: authorization related problems(6)";
  attrib S459E_1  format=F01830_. label="Reason for no postpartum check-up: lack of knowledge(1)";
  attrib S459E_2  format=F01830_. label="Reason for no postpartum check-up: lack of knowledge(2)";
  attrib S459E_3  format=F01830_. label="Reason for no postpartum check-up: lack of knowledge(3)";
  attrib S459E_4  format=F01830_. label="Reason for no postpartum check-up: lack of knowledge(4)";
  attrib S459E_5  format=F01830_. label="Reason for no postpartum check-up: lack of knowledge(5)";
  attrib S459E_6  format=F01830_. label="Reason for no postpartum check-up: lack of knowledge(6)";
  attrib S459F_1  format=F01831_. label="Reason for no postpartum check-up: Opposition(1)";
  attrib S459F_2  format=F01831_. label="Reason for no postpartum check-up: Opposition(2)";
  attrib S459F_3  format=F01831_. label="Reason for no postpartum check-up: Opposition(3)";
  attrib S459F_4  format=F01831_. label="Reason for no postpartum check-up: Opposition(4)";
  attrib S459F_5  format=F01831_. label="Reason for no postpartum check-up: Opposition(5)";
  attrib S459F_6  format=F01831_. label="Reason for no postpartum check-up: Opposition(6)";
  attrib S459G_1  format=F01832_. label="Reason for no postpartum check-up: wasn't affiliated to health insurance(1)";
  attrib S459G_2  format=F01832_. label="Reason for no postpartum check-up: wasn't affiliated to health insurance(2)";
  attrib S459G_3  format=F01832_. label="Reason for no postpartum check-up: wasn't affiliated to health insurance(3)";
  attrib S459G_4  format=F01832_. label="Reason for no postpartum check-up: wasn't affiliated to health insurance(4)";
  attrib S459G_5  format=F01832_. label="Reason for no postpartum check-up: wasn't affiliated to health insurance(5)";
  attrib S459G_6  format=F01832_. label="Reason for no postpartum check-up: wasn't affiliated to health insurance(6)";
  attrib S459H_1  format=F01833_. label="Reason for no postpartum check-up: didn't know that service was provided(1)";
  attrib S459H_2  format=F01833_. label="Reason for no postpartum check-up: didn't know that service was provided(2)";
  attrib S459H_3  format=F01833_. label="Reason for no postpartum check-up: didn't know that service was provided(3)";
  attrib S459H_4  format=F01833_. label="Reason for no postpartum check-up: didn't know that service was provided(4)";
  attrib S459H_5  format=F01833_. label="Reason for no postpartum check-up: didn't know that service was provided(5)";
  attrib S459H_6  format=F01833_. label="Reason for no postpartum check-up: didn't know that service was provided(6)";
  attrib S459X_1  format=F01834_. label="Reason for no postpartum check-up: other(1)";
  attrib S459X_2  format=F01834_. label="Reason for no postpartum check-up: other(2)";
  attrib S459X_3  format=F01834_. label="Reason for no postpartum check-up: other(3)";
  attrib S459X_4  format=F01834_. label="Reason for no postpartum check-up: other(4)";
  attrib S459X_5  format=F01834_. label="Reason for no postpartum check-up: other(5)";
  attrib S459X_6  format=F01834_. label="Reason for no postpartum check-up: other(6)";
  attrib S465_1   format=F01835_. label="Duration of abstinence after birth(1)";
  attrib S465_2   format=F01835_. label="Duration of abstinence after birth(2)";
  attrib S465_3   format=F01835_. label="Duration of abstinence after birth(3)";
  attrib S465_4   format=F01835_. label="Duration of abstinence after birth(4)";
  attrib S465_5   format=F01835_. label="Duration of abstinence after birth(5)";
  attrib S465_6   format=F01835_. label="Duration of abstinence after birth(6)";
  attrib S501     format=F01836_. label="Interpretation of sexuality";
  attrib S502     format=F01837_. label="Have received information on sexual related topics";
  attrib S503N_01 format=F01838_. label="Specific topics(1)";
  attrib S503N_02 format=F01838_. label="Specific topics(2)";
  attrib S503N_03 format=F01838_. label="Specific topics(3)";
  attrib S503N_04 format=F01838_. label="Specific topics(4)";
  attrib S503N_05 format=F01838_. label="Specific topics(5)";
  attrib S503N_06 format=F01838_. label="Specific topics(6)";
  attrib S503N_07 format=F01838_. label="Specific topics(7)";
  attrib S503N_08 format=F01838_. label="Specific topics(8)";
  attrib S503N_09 format=F01838_. label="Specific topics(9)";
  attrib S503N_10 format=F01838_. label="Specific topics(10)";
  attrib S503N_11 format=F01838_. label="Specific topics(11)";
  attrib S503N_12 format=F01838_. label="Specific topics(12)";
  attrib S503N_13 format=F01838_. label="Specific topics(13)";
  attrib S503N_14 format=F01838_. label="Specific topics(14)";
  attrib S503N_15 format=F01838_. label="Specific topics(15)";
  attrib S503N_16 format=F01838_. label="Specific topics(16)";
  attrib S503N_17 format=F01838_. label="Specific topics(17)";
  attrib S503_01  format=F01839_. label="Have received information on topic(1)";
  attrib S503_02  format=F01839_. label="Have received information on topic(2)";
  attrib S503_03  format=F01839_. label="Have received information on topic(3)";
  attrib S503_04  format=F01839_. label="Have received information on topic(4)";
  attrib S503_05  format=F01839_. label="Have received information on topic(5)";
  attrib S503_06  format=F01839_. label="Have received information on topic(6)";
  attrib S503_07  format=F01839_. label="Have received information on topic(7)";
  attrib S503_08  format=F01839_. label="Have received information on topic(8)";
  attrib S503_09  format=F01839_. label="Have received information on topic(9)";
  attrib S503_10  format=F01839_. label="Have received information on topic(10)";
  attrib S503_11  format=F01839_. label="Have received information on topic(11)";
  attrib S503_12  format=F01839_. label="Have received information on topic(12)";
  attrib S503_13  format=F01839_. label="Have received information on topic(13)";
  attrib S503_14  format=F01839_. label="Have received information on topic(14)";
  attrib S503_15  format=F01839_. label="Have received information on topic(15)";
  attrib S503_16  format=F01839_. label="Have received information on topic(16)";
  attrib S503_17  format=F01839_. label="Have received information on topic(17)";
  attrib S504_01  format=F01840_. label="Age first received information on topic(1)";
  attrib S504_02  format=F01840_. label="Age first received information on topic(2)";
  attrib S504_03  format=F01840_. label="Age first received information on topic(3)";
  attrib S504_04  format=F01840_. label="Age first received information on topic(4)";
  attrib S504_05  format=F01840_. label="Age first received information on topic(5)";
  attrib S504_06  format=F01840_. label="Age first received information on topic(6)";
  attrib S504_07  format=F01840_. label="Age first received information on topic(7)";
  attrib S504_08  format=F01840_. label="Age first received information on topic(8)";
  attrib S504_09  format=F01840_. label="Age first received information on topic(9)";
  attrib S504_10  format=F01840_. label="Age first received information on topic(10)";
  attrib S504_11  format=F01840_. label="Age first received information on topic(11)";
  attrib S504_12  format=F01840_. label="Age first received information on topic(12)";
  attrib S504_13  format=F01840_. label="Age first received information on topic(13)";
  attrib S504_14  format=F01840_. label="Age first received information on topic(14)";
  attrib S504_15  format=F01840_. label="Age first received information on topic(15)";
  attrib S504_16  format=F01840_. label="Age first received information on topic(16)";
  attrib S504_17  format=F01840_. label="Age first received information on topic(17)";
  attrib S505_01  format=F01841_. label="Place where information on topic was received(1)";
  attrib S505_02  format=F01841_. label="Place where information on topic was received(2)";
  attrib S505_03  format=F01841_. label="Place where information on topic was received(3)";
  attrib S505_04  format=F01841_. label="Place where information on topic was received(4)";
  attrib S505_05  format=F01841_. label="Place where information on topic was received(5)";
  attrib S505_06  format=F01841_. label="Place where information on topic was received(6)";
  attrib S505_07  format=F01841_. label="Place where information on topic was received(7)";
  attrib S505_08  format=F01841_. label="Place where information on topic was received(8)";
  attrib S505_09  format=F01841_. label="Place where information on topic was received(9)";
  attrib S505_10  format=F01841_. label="Place where information on topic was received(10)";
  attrib S505_11  format=F01841_. label="Place where information on topic was received(11)";
  attrib S505_12  format=F01841_. label="Place where information on topic was received(12)";
  attrib S505_13  format=F01841_. label="Place where information on topic was received(13)";
  attrib S505_14  format=F01841_. label="Place where information on topic was received(14)";
  attrib S505_15  format=F01841_. label="Place where information on topic was received(15)";
  attrib S505_16  format=F01841_. label="Place where information on topic was received(16)";
  attrib S505_17  format=F01841_. label="Place where information on topic was received(17)";
  attrib S506_01  format=F01842_. label="Media through which information was received(1)";
  attrib S506_02  format=F01842_. label="Media through which information was received(2)";
  attrib S506_03  format=F01842_. label="Media through which information was received(3)";
  attrib S506_04  format=F01842_. label="Media through which information was received(4)";
  attrib S506_05  format=F01842_. label="Media through which information was received(5)";
  attrib S506_06  format=F01842_. label="Media through which information was received(6)";
  attrib S506_07  format=F01842_. label="Media through which information was received(7)";
  attrib S506_08  format=F01842_. label="Media through which information was received(8)";
  attrib S506_09  format=F01842_. label="Media through which information was received(9)";
  attrib S506_10  format=F01842_. label="Media through which information was received(10)";
  attrib S506_11  format=F01842_. label="Media through which information was received(11)";
  attrib S506_12  format=F01842_. label="Media through which information was received(12)";
  attrib S506_13  format=F01842_. label="Media through which information was received(13)";
  attrib S506_14  format=F01842_. label="Media through which information was received(14)";
  attrib S506_15  format=F01842_. label="Media through which information was received(15)";
  attrib S506_16  format=F01842_. label="Media through which information was received(16)";
  attrib S506_17  format=F01842_. label="Media through which information was received(17)";
  attrib S508     format=F01843_. label="Last 12 months, frequency participated on sexual education activities in education institutions";
  attrib S509     format=F01844_. label="Last 12 months, frequency participated on sexual education activities with community";
  attrib S510A    format=F01845_. label="Information received helped to: clarify doubts";
  attrib S510B    format=F01846_. label="Information received helped to: obtain knowledge";
  attrib S510C    format=F01847_. label="Information received helped to: clarify own values and beliefs";
  attrib S510D    format=F01848_. label="Information received helped to: develop skills to take decisions";
  attrib S510E    format=F01849_. label="Information received helped to: understand what others think and feel";
  attrib S510F    format=F01850_. label="Information received helped to: learn how to communicate opinions and expectations";
  attrib S510G    format=F01851_. label="Information received helped to: know who and where to request help or claim rights";
  attrib S511     format=F01852_. label="Assessment on much have missed due to lack of information on sexuality";
  attrib S512A    format=F01853_. label="Opinion on sexuality topics: family should be the only one providing sexual orientation to children";
  attrib S512B    format=F01854_. label="Opinion on sexuality topics: materials used in sexual education programs are vulgar";
  attrib S512C    format=F01855_. label="Opinion on sexuality topics: sexual education is responsible for pregnancy in adolescents";
  attrib S512D    format=F01856_. label="Opinion on sexuality topics: contraception should be taught when people start having sex";
  attrib S512E    format=F01857_. label="Opinion on sexuality topics: as part of sexual education condoms should be provided to adolescents";
  attrib S512F    format=F01858_. label="Opinion on sexuality topics: sexual education should be given after children understand those topics";
  attrib S512G    format=F01859_. label="Opinion on sexuality topics: to avoid STI sexual education should be given since preschool";
  attrib S512H    format=F01860_. label="Opinion on sexuality topics: sexual education promotes respect regardless of gender";
  attrib S512I    format=F01861_. label="Opinion on sexuality topics: sexual education increases curiosity on things they shouldn't know";
  attrib S512J    format=F01862_. label="Opinion on sexuality topics: children 12 to 14 should be taught how to use condoms to prevent AIDS";
  attrib S902     format=F01863_. label="Ever heard of cytology";
  attrib S903A    format=F01864_. label="How was aware about cytology: doctor/nurse";
  attrib S903B    format=F01865_. label="How was aware about cytology: friends/relative";
  attrib S903C    format=F01866_. label="How was aware about cytology: media";
  attrib S903D    format=F01867_. label="How was aware about cytology: health organization";
  attrib S903E    format=F01868_. label="How was aware about cytology: education institution";
  attrib S903X    format=F01869_. label="How was aware about cytology: other";
  attrib S903Z    format=F01870_. label="How was aware about cytology: don't remember";
  attrib S904     format=F01871_. label="Ever taken a cytology test";
  attrib S905M    format=F01872_. label="Month last cytology";
  attrib S905Y    format=F01873_. label="Year last cytology";
  attrib S906     format=F01874_. label="Was last cytology more than 3 years ago";
  attrib S907     format=F01875_. label="Frequency of cytology";
  attrib S908     format=F01876_. label="Did get the result of last cytology";
  attrib S909     format=F01877_. label="Result of last cytology";
  attrib S910     format=F01878_. label="What does it mean that a cytology is abnormal";
  attrib S911     format=F01879_. label="What a woman can do when the result is abnormal";
  attrib S913     format=F01880_. label="Reason didn't pick up result of cytology";
  attrib S914     format=F01881_. label="Reason has never taken a cytology test";
  attrib S915     format=F01882_. label="Ever had intention for a cytology test";
  attrib S916     format=F01883_. label="Reason they have never taken a cytology test";
  attrib S917     format=F01884_. label="Ever heard of the papiloma virus";
  attrib S918     format=F01885_. label="Ever heard of other tests to find cancer in uterus";
  attrib S920     format=F01886_. label="Knows about self breast exam";
  attrib S921     format=F01887_. label="Has made herself breast exam";
  attrib S922A    format=F01888_. label="Who taught to self test breast: doctor/nurse";
  attrib S922B    format=F01889_. label="Who taught to self test breast: friend/relative";
  attrib S922C    format=F01890_. label="Who taught to self test breast: media";
  attrib S922D    format=F01891_. label="Who taught to self test breast: health institute";
  attrib S922X    format=F01892_. label="Who taught to self test breast: other";
  attrib S922Z    format=F01893_. label="Who taught to self test breast: don't remember";
  attrib S923     format=F01894_. label="Frequency for self breast exam";
  attrib S924     format=F01895_. label="Timing between menstrual cycle for self breast exam";
  attrib S925     format=F01896_. label="During a doctor's visit have the breasts examined";
  attrib S926     format=F01897_. label="Time since last clinical breast exam";
  attrib S927     format=F01898_. label="Reason they did a clinic breast test";
  attrib S928     format=F01899_. label="Have been diagnosed with beast tumor";
  attrib S930     format=F01900_. label="Have ever heard of mammography";
  attrib S931A    format=F01901_. label="Find out about mammography by: doctor/nurse";
  attrib S931B    format=F01902_. label="Find out about mammography by: friends/relatives";
  attrib S931C    format=F01903_. label="Find out about mammography by: media";
  attrib S931D    format=F01904_. label="Find out about mammography by: health institute";
  attrib S931X    format=F01905_. label="Find out about mammography by: others";
  attrib S931Z    format=F01906_. label="Find out about mammography by: don't remember";
  attrib S932     format=F01907_. label="Have ever have taken a mammography";
  attrib S933     format=F01908_. label="Reason for mammography";
  attrib S934M    format=F01909_. label="Month last mammography";
  attrib S934Y    format=F01910_. label="Year last mammography";
  attrib S935     format=F01911_. label="Frequency for mammography";
  attrib S936     format=F01912_. label="Place for last mammography";
  attrib S937     format=F01913_. label="Did get the result of last mammography";
  attrib S938     format=F01914_. label="Result of last mammography";
  attrib S939     format=F01915_. label="Reason didn't pick up result of mammography";
  attrib S940     format=F01916_. label="As a result of mammography a biopsy was requested";
  attrib S941     format=F01917_. label="Reason they did made a biopsy as a result of mammography";
  attrib S942     format=F01918_. label="Did receive treatment as a result of mammography or biopsy";
  attrib S943     format=F01919_. label="Reason didn't receive treatment for mammography or biopsy";
  attrib S944     format=F01920_. label="Reason hasn't taken a mammography test";
  attrib S1301A   format=F01921_. label="Agrees on gender relations statement: most important role for women take care of house";
  attrib S1301B   format=F01922_. label="Agrees on gender relations statement: men need more sex than women";
  attrib S1301C   format=F01923_. label="Agrees on gender relations statement: men don't talk of sex they make it";
  attrib S1301D   format=F01924_. label="Agrees on gender relations statement: sometimes is right to hit women";
  attrib S1301E   format=F01925_. label="Agrees on gender relations statement: take care of children is women responsibility";
  attrib S1301F   format=F01926_. label="Agrees on gender relations statement: women should take precautions to avoid pregnancy";
  attrib S1301G   format=F01927_. label="Agrees on gender relations statement: men has the last word on household decisions";
  attrib S1301H   format=F01928_. label="Agrees on gender relations statement: men are always ready for sex";
  attrib S1301I   format=F01929_. label="Agrees on gender relations statement: women should endure violence to maintain family unity";
  attrib S1301J   format=F01930_. label="Agrees on gender relations statement: would be bold for women to request use of condom";
  attrib S1301K   format=F01931_. label="Agrees on gender relations statement: never would have a gay friend";
  attrib S1301L   format=F01932_. label="Agrees on gender relations statement: if someone insults me, I defend myself as needed";
  attrib S1302A   format=F01933_. label="Agrees on statement: men are head of households";
  attrib S1302B   format=F01934_. label="Agrees on statement: women need men to be happy";
  attrib S1302C   format=F01935_. label="Agrees on statement: men need a women at home";
  attrib S1302D   format=F01936_. label="Agrees on statement: families with a man have less problems";
  attrib S1302E   format=F01937_. label="Agrees on statement: it is more serious that women abandoned children than if a men do";
  attrib S1302F   format=F01938_. label="Agrees on statement: women should arrive virgin at marriage";
  attrib S1302G   format=F01939_. label="Agrees on statement: masturbation is for men";
  attrib S1302H   format=F01940_. label="Agrees on statement: women are free to decide if they want to work";
  attrib S1304A   format=F01941_. label="Sharing of household chores: clean the house";
  attrib S1304B   format=F01942_. label="Sharing of household chores: repair the house";
  attrib S1304C   format=F01943_. label="Sharing of household chores: prepare food";
  attrib S1304D   format=F01944_. label="Sharing of household chores: clean bathroom";
  attrib S1304E   format=F01945_. label="Sharing of household chores: wash clothes";
  attrib S1304F   format=F01946_. label="Sharing of household chores: buy food/go to the supermarket";
  attrib S1304G   format=F01947_. label="Sharing of household chores: pay bills";
  attrib S1304H   format=F01948_. label="Sharing of household chores: take care of sick/disable/old relatives";
  attrib S1308A   format=F01949_. label="Sharing children care: daily care of children";
  attrib S1308B   format=F01950_. label="Sharing children care: stay home when child is sick";
  attrib S1308C   format=F01951_. label="Sharing children care: pick them from school";
  attrib S1308D   format=F01952_. label="Sharing children care: take them to sportive or fun activities";
  attrib S1308E   format=F01953_. label="Sharing children care: help with homework";
  attrib S1309    format=F01954_. label="Who takes care of youngest child";
  attrib S1310A   format=F01955_. label="Witness attitude to gays: not considered for a job/fired";
  attrib S1310B   format=F01956_. label="Witness attitude to gays: didn't allow to enter fun places";
  attrib S1310C   format=F01957_. label="Witness attitude to gays: deny service at a health facility";
  attrib S1310D   format=F01958_. label="Witness attitude to gays: discriminated by teachers/classmates";
  attrib S1310E   format=F01959_. label="Witness attitude to gays: discriminated by friends";
  attrib S1310F   format=F01960_. label="Witness attitude to gays: discriminated by neighbors";
  attrib S1310G   format=F01961_. label="Witness attitude to gays: discriminated by boss/coworker";
  attrib S1310H   format=F01962_. label="Witness attitude to gays: excluded/marginalized from family";
  attrib S1310I   format=F01963_. label="Witness attitude to gays: excluded/marginalized from religious group";
  attrib S1310J   format=F01964_. label="Witness attitude to gays: mistreated by police";
  attrib S1310X   format=F01965_. label="Witness attitude to gays: other";
  attrib S1312    format=F01966_. label="What would do if an unknown person is mistreated for being homosexual";
  attrib S1313    format=F01967_. label="homosexuals have the same rights as heterosexuals";
  attrib S1314    format=F01968_. label="Approves to grant rights to couples of the same sex";
  attrib S1315    format=F01969_. label="Approves that gay couples adopt children";
  attrib S1316A   format=F01970_. label="Perception on gays: a homosexual can be a good parent";
  attrib S1316B   format=F01971_. label="Perception on gays: I like when I see a man with feminine manners";
  attrib S1316C   format=F01972_. label="Perception on gays:  I like when I see a woman with masculine manners";
  attrib S1316D   format=F01973_. label="Perception on gays: same sex couples should kiss on streets";
  attrib S1316E   format=F01974_. label="Perception on gays: would leave your child with a gay";
  attrib S1316F   format=F01975_. label="Perception on gays: would prefer not to have gay neighbors";
  attrib S1316G   format=F01976_. label="Perception on gays: being homosexual is a sin";
  attrib S1316H   format=F01977_. label="Perception on gays: I prefer a delinquent than a homosexual child";
  attrib S1321    format=F01978_. label="Does respondent has a homosexual child";
  attrib S1322    format=F01979_. label="What would do if knows that your child is gay";
  attrib S1323    format=F01980_. label="What did you do when knew that your child is homosexual";
infile 'C:\COUNTRIES\CO72\COIR72FL.DAT' LRECL=9678 MISSOVER ;
    input
    @1    CASEID   $15.
    @16   V000     $3.
    @19   V001     6.0
    @25   V002     6.0
    @31   V003     3.0
    @34   V004     4.0
    @38   V005     8.0
    @46   V006     2.0
    @48   V007     4.0
    @52   V008     4.0
    @56   V008A    5.0
    @61   V009     2.0
    @63   V010     4.0
    @67   V011     4.0
    @71   V012     2.0
    @73   V013     1.0
    @74   V014     1.0
    @75   V015     1.0
    @76   V016     2.0
    @78   V017     4.0
    @82   V018     2.0
    @84   V019     2.0
    @86   V019A    1.0
    @87   V020     1.0
    @88   V021     4.0
    @92   V022     4.0
    @96   V023     2.0
    @98   V024     2.0
    @100  V025     1.0
    @101  V026     1.0
    @102  V027     1.0
    @103  V028     5.0
    @108  V029     3.0
    @111  V030     5.0
    @116  V031     5.0
    @121  V032     3.0
    @124  V034     2.0
    @126  V040     4.0
    @130  V042     1.0
    @131  V044     1.0
    @132  V045A    3.0
    @135  V045B    3.0
    @138  V045C    3.0
    @141  V046     1.0
    @142  V101     2.0
    @144  V102     1.0
    @145  V103     1.0
    @146  V104     2.0
    @148  V105     1.0
    @149  V105A    2.0
    @151  V106     1.0
    @152  V107     2.0
    @154  V113     2.0
    @156  V115     3.0
    @159  V116     2.0
    @161  V119     1.0
    @162  V120     1.0
    @163  V121     1.0
    @164  V122     1.0
    @165  V123     1.0
    @166  V124     1.0
    @167  V125     1.0
    @168  V127     2.0
    @170  V128     2.0
    @172  V129     2.0
    @174  V130     2.0
    @176  V131     3.0
    @179  V133     2.0
    @181  V134     1.0
    @182  V135     1.0
    @183  V136     2.0
    @185  V137     2.0
    @187  V138     2.0
    @189  V139     2.0
    @191  V140     1.0
    @192  V141     1.0
    @193  V149     1.0
    @194  V150     2.0
    @196  V151     1.0
    @197  V152     2.0
    @199  V153     1.0
    @200  AWFACTT  5.0
    @205  AWFACTU  5.0
    @210  AWFACTR  5.0
    @215  AWFACTE  5.0
    @220  AWFACTW  5.0
    @225  V155     1.0
    @226  V156     1.0
    @227  V157     1.0
    @228  V158     1.0
    @229  V159     1.0
    @230  V160     1.0
    @231  V161     2.0
    @233  V166     3.0
    @236  V167     2.0
    @238  V168     1.0
    @239  V169A    1.0
    @240  V169B    1.0
    @241  V170     1.0
    @242  V171A    1.0
    @243  V171B    1.0
    @244  V190     1.0
    @245  V191     8.0
    @253  V190A    1.0
    @254  V191A    8.0
    @262  ML101    1.0
    @263  BIDX_01  2.0
    @265  BIDX_02  2.0
    @267  BIDX_03  2.0
    @269  BIDX_04  2.0
    @271  BIDX_05  2.0
    @273  BIDX_06  2.0
    @275  BIDX_07  2.0
    @277  BIDX_08  2.0
    @279  BIDX_09  2.0
    @281  BIDX_10  2.0
    @283  BIDX_11  2.0
    @285  BIDX_12  2.0
    @287  BIDX_13  2.0
    @289  BIDX_14  2.0
    @291  BIDX_15  2.0
    @293  BIDX_16  2.0
    @295  BIDX_17  2.0
    @297  BIDX_18  2.0
    @299  BIDX_19  2.0
    @301  BIDX_20  2.0
    @303  BORD_01  2.0
    @305  BORD_02  2.0
    @307  BORD_03  2.0
    @309  BORD_04  2.0
    @311  BORD_05  2.0
    @313  BORD_06  2.0
    @315  BORD_07  2.0
    @317  BORD_08  2.0
    @319  BORD_09  2.0
    @321  BORD_10  2.0
    @323  BORD_11  2.0
    @325  BORD_12  2.0
    @327  BORD_13  2.0
    @329  BORD_14  2.0
    @331  BORD_15  2.0
    @333  BORD_16  2.0
    @335  BORD_17  2.0
    @337  BORD_18  2.0
    @339  BORD_19  2.0
    @341  BORD_20  2.0
    @343  B0_01    1.0
    @344  B0_02    1.0
    @345  B0_03    1.0
    @346  B0_04    1.0
    @347  B0_05    1.0
    @348  B0_06    1.0
    @349  B0_07    1.0
    @350  B0_08    1.0
    @351  B0_09    1.0
    @352  B0_10    1.0
    @353  B0_11    1.0
    @354  B0_12    1.0
    @355  B0_13    1.0
    @356  B0_14    1.0
    @357  B0_15    1.0
    @358  B0_16    1.0
    @359  B0_17    1.0
    @360  B0_18    1.0
    @361  B0_19    1.0
    @362  B0_20    1.0
    @363  B1_01    2.0
    @365  B1_02    2.0
    @367  B1_03    2.0
    @369  B1_04    2.0
    @371  B1_05    2.0
    @373  B1_06    2.0
    @375  B1_07    2.0
    @377  B1_08    2.0
    @379  B1_09    2.0
    @381  B1_10    2.0
    @383  B1_11    2.0
    @385  B1_12    2.0
    @387  B1_13    2.0
    @389  B1_14    2.0
    @391  B1_15    2.0
    @393  B1_16    2.0
    @395  B1_17    2.0
    @397  B1_18    2.0
    @399  B1_19    2.0
    @401  B1_20    2.0
    @403  B2_01    4.0
    @407  B2_02    4.0
    @411  B2_03    4.0
    @415  B2_04    4.0
    @419  B2_05    4.0
    @423  B2_06    4.0
    @427  B2_07    4.0
    @431  B2_08    4.0
    @435  B2_09    4.0
    @439  B2_10    4.0
    @443  B2_11    4.0
    @447  B2_12    4.0
    @451  B2_13    4.0
    @455  B2_14    4.0
    @459  B2_15    4.0
    @463  B2_16    4.0
    @467  B2_17    4.0
    @471  B2_18    4.0
    @475  B2_19    4.0
    @479  B2_20    4.0
    @483  B3_01    4.0
    @487  B3_02    4.0
    @491  B3_03    4.0
    @495  B3_04    4.0
    @499  B3_05    4.0
    @503  B3_06    4.0
    @507  B3_07    4.0
    @511  B3_08    4.0
    @515  B3_09    4.0
    @519  B3_10    4.0
    @523  B3_11    4.0
    @527  B3_12    4.0
    @531  B3_13    4.0
    @535  B3_14    4.0
    @539  B3_15    4.0
    @543  B3_16    4.0
    @547  B3_17    4.0
    @551  B3_18    4.0
    @555  B3_19    4.0
    @559  B3_20    4.0
    @563  B4_01    1.0
    @564  B4_02    1.0
    @565  B4_03    1.0
    @566  B4_04    1.0
    @567  B4_05    1.0
    @568  B4_06    1.0
    @569  B4_07    1.0
    @570  B4_08    1.0
    @571  B4_09    1.0
    @572  B4_10    1.0
    @573  B4_11    1.0
    @574  B4_12    1.0
    @575  B4_13    1.0
    @576  B4_14    1.0
    @577  B4_15    1.0
    @578  B4_16    1.0
    @579  B4_17    1.0
    @580  B4_18    1.0
    @581  B4_19    1.0
    @582  B4_20    1.0
    @583  B5_01    1.0
    @584  B5_02    1.0
    @585  B5_03    1.0
    @586  B5_04    1.0
    @587  B5_05    1.0
    @588  B5_06    1.0
    @589  B5_07    1.0
    @590  B5_08    1.0
    @591  B5_09    1.0
    @592  B5_10    1.0
    @593  B5_11    1.0
    @594  B5_12    1.0
    @595  B5_13    1.0
    @596  B5_14    1.0
    @597  B5_15    1.0
    @598  B5_16    1.0
    @599  B5_17    1.0
    @600  B5_18    1.0
    @601  B5_19    1.0
    @602  B5_20    1.0
    @603  B6_01    3.0
    @606  B6_02    3.0
    @609  B6_03    3.0
    @612  B6_04    3.0
    @615  B6_05    3.0
    @618  B6_06    3.0
    @621  B6_07    3.0
    @624  B6_08    3.0
    @627  B6_09    3.0
    @630  B6_10    3.0
    @633  B6_11    3.0
    @636  B6_12    3.0
    @639  B6_13    3.0
    @642  B6_14    3.0
    @645  B6_15    3.0
    @648  B6_16    3.0
    @651  B6_17    3.0
    @654  B6_18    3.0
    @657  B6_19    3.0
    @660  B6_20    3.0
    @663  B7_01    3.0
    @666  B7_02    3.0
    @669  B7_03    3.0
    @672  B7_04    3.0
    @675  B7_05    3.0
    @678  B7_06    3.0
    @681  B7_07    3.0
    @684  B7_08    3.0
    @687  B7_09    3.0
    @690  B7_10    3.0
    @693  B7_11    3.0
    @696  B7_12    3.0
    @699  B7_13    3.0
    @702  B7_14    3.0
    @705  B7_15    3.0
    @708  B7_16    3.0
    @711  B7_17    3.0
    @714  B7_18    3.0
    @717  B7_19    3.0
    @720  B7_20    3.0
    @723  B8_01    2.0
    @725  B8_02    2.0
    @727  B8_03    2.0
    @729  B8_04    2.0
    @731  B8_05    2.0
    @733  B8_06    2.0
    @735  B8_07    2.0
    @737  B8_08    2.0
    @739  B8_09    2.0
    @741  B8_10    2.0
    @743  B8_11    2.0
    @745  B8_12    2.0
    @747  B8_13    2.0
    @749  B8_14    2.0
    @751  B8_15    2.0
    @753  B8_16    2.0
    @755  B8_17    2.0
    @757  B8_18    2.0
    @759  B8_19    2.0
    @761  B8_20    2.0
    @763  B9_01    1.0
    @764  B9_02    1.0
    @765  B9_03    1.0
    @766  B9_04    1.0
    @767  B9_05    1.0
    @768  B9_06    1.0
    @769  B9_07    1.0
    @770  B9_08    1.0
    @771  B9_09    1.0
    @772  B9_10    1.0
    @773  B9_11    1.0
    @774  B9_12    1.0
    @775  B9_13    1.0
    @776  B9_14    1.0
    @777  B9_15    1.0
    @778  B9_16    1.0
    @779  B9_17    1.0
    @780  B9_18    1.0
    @781  B9_19    1.0
    @782  B9_20    1.0
    @783  B10_01   1.0
    @784  B10_02   1.0
    @785  B10_03   1.0
    @786  B10_04   1.0
    @787  B10_05   1.0
    @788  B10_06   1.0
    @789  B10_07   1.0
    @790  B10_08   1.0
    @791  B10_09   1.0
    @792  B10_10   1.0
    @793  B10_11   1.0
    @794  B10_12   1.0
    @795  B10_13   1.0
    @796  B10_14   1.0
    @797  B10_15   1.0
    @798  B10_16   1.0
    @799  B10_17   1.0
    @800  B10_18   1.0
    @801  B10_19   1.0
    @802  B10_20   1.0
    @803  B11_01   3.0
    @806  B11_02   3.0
    @809  B11_03   3.0
    @812  B11_04   3.0
    @815  B11_05   3.0
    @818  B11_06   3.0
    @821  B11_07   3.0
    @824  B11_08   3.0
    @827  B11_09   3.0
    @830  B11_10   3.0
    @833  B11_11   3.0
    @836  B11_12   3.0
    @839  B11_13   3.0
    @842  B11_14   3.0
    @845  B11_15   3.0
    @848  B11_16   3.0
    @851  B11_17   3.0
    @854  B11_18   3.0
    @857  B11_19   3.0
    @860  B11_20   3.0
    @863  B12_01   3.0
    @866  B12_02   3.0
    @869  B12_03   3.0
    @872  B12_04   3.0
    @875  B12_05   3.0
    @878  B12_06   3.0
    @881  B12_07   3.0
    @884  B12_08   3.0
    @887  B12_09   3.0
    @890  B12_10   3.0
    @893  B12_11   3.0
    @896  B12_12   3.0
    @899  B12_13   3.0
    @902  B12_14   3.0
    @905  B12_15   3.0
    @908  B12_16   3.0
    @911  B12_17   3.0
    @914  B12_18   3.0
    @917  B12_19   3.0
    @920  B12_20   3.0
    @923  B13_01   1.0
    @924  B13_02   1.0
    @925  B13_03   1.0
    @926  B13_04   1.0
    @927  B13_05   1.0
    @928  B13_06   1.0
    @929  B13_07   1.0
    @930  B13_08   1.0
    @931  B13_09   1.0
    @932  B13_10   1.0
    @933  B13_11   1.0
    @934  B13_12   1.0
    @935  B13_13   1.0
    @936  B13_14   1.0
    @937  B13_15   1.0
    @938  B13_16   1.0
    @939  B13_17   1.0
    @940  B13_18   1.0
    @941  B13_19   1.0
    @942  B13_20   1.0
    @943  B15_01   1.0
    @944  B15_02   1.0
    @945  B15_03   1.0
    @946  B15_04   1.0
    @947  B15_05   1.0
    @948  B15_06   1.0
    @949  B15_07   1.0
    @950  B15_08   1.0
    @951  B15_09   1.0
    @952  B15_10   1.0
    @953  B15_11   1.0
    @954  B15_12   1.0
    @955  B15_13   1.0
    @956  B15_14   1.0
    @957  B15_15   1.0
    @958  B15_16   1.0
    @959  B15_17   1.0
    @960  B15_18   1.0
    @961  B15_19   1.0
    @962  B15_20   1.0
    @963  B16_01   2.0
    @965  B16_02   2.0
    @967  B16_03   2.0
    @969  B16_04   2.0
    @971  B16_05   2.0
    @973  B16_06   2.0
    @975  B16_07   2.0
    @977  B16_08   2.0
    @979  B16_09   2.0
    @981  B16_10   2.0
    @983  B16_11   2.0
    @985  B16_12   2.0
    @987  B16_13   2.0
    @989  B16_14   2.0
    @991  B16_15   2.0
    @993  B16_16   2.0
    @995  B16_17   2.0
    @997  B16_18   2.0
    @999  B16_19   2.0
    @1001 B16_20   2.0
    @1003 B17_01   2.0
    @1005 B17_02   2.0
    @1007 B17_03   2.0
    @1009 B17_04   2.0
    @1011 B17_05   2.0
    @1013 B17_06   2.0
    @1015 B17_07   2.0
    @1017 B17_08   2.0
    @1019 B17_09   2.0
    @1021 B17_10   2.0
    @1023 B17_11   2.0
    @1025 B17_12   2.0
    @1027 B17_13   2.0
    @1029 B17_14   2.0
    @1031 B17_15   2.0
    @1033 B17_16   2.0
    @1035 B17_17   2.0
    @1037 B17_18   2.0
    @1039 B17_19   2.0
    @1041 B17_20   2.0
    @1043 B18_01   5.0
    @1048 B18_02   5.0
    @1053 B18_03   5.0
    @1058 B18_04   5.0
    @1063 B18_05   5.0
    @1068 B18_06   5.0
    @1073 B18_07   5.0
    @1078 B18_08   5.0
    @1083 B18_09   5.0
    @1088 B18_10   5.0
    @1093 B18_11   5.0
    @1098 B18_12   5.0
    @1103 B18_13   5.0
    @1108 B18_14   5.0
    @1113 B18_15   5.0
    @1118 B18_16   5.0
    @1123 B18_17   5.0
    @1128 B18_18   5.0
    @1133 B18_19   5.0
    @1138 B18_20   5.0
    @1143 B19_01   3.0
    @1146 B19_02   3.0
    @1149 B19_03   3.0
    @1152 B19_04   3.0
    @1155 B19_05   3.0
    @1158 B19_06   3.0
    @1161 B19_07   3.0
    @1164 B19_08   3.0
    @1167 B19_09   3.0
    @1170 B19_10   3.0
    @1173 B19_11   3.0
    @1176 B19_12   3.0
    @1179 B19_13   3.0
    @1182 B19_14   3.0
    @1185 B19_15   3.0
    @1188 B19_16   3.0
    @1191 B19_17   3.0
    @1194 B19_18   3.0
    @1197 B19_19   3.0
    @1200 B19_20   3.0
    @1203 B20_01   2.0
    @1205 B20_02   2.0
    @1207 B20_03   2.0
    @1209 B20_04   2.0
    @1211 B20_05   2.0
    @1213 B20_06   2.0
    @1215 B20_07   2.0
    @1217 B20_08   2.0
    @1219 B20_09   2.0
    @1221 B20_10   2.0
    @1223 B20_11   2.0
    @1225 B20_12   2.0
    @1227 B20_13   2.0
    @1229 B20_14   2.0
    @1231 B20_15   2.0
    @1233 B20_16   2.0
    @1235 B20_17   2.0
    @1237 B20_18   2.0
    @1239 B20_19   2.0
    @1241 B20_20   2.0
    @1243 V201     2.0
    @1245 V202     2.0
    @1247 V203     2.0
    @1249 V204     2.0
    @1251 V205     2.0
    @1253 V206     2.0
    @1255 V207     2.0
    @1257 V208     1.0
    @1258 V209     1.0
    @1259 V210     1.0
    @1260 V211     4.0
    @1264 V212     2.0
    @1266 V213     1.0
    @1267 V214     2.0
    @1269 V215     3.0
    @1272 V216     1.0
    @1273 V217     1.0
    @1274 V218     2.0
    @1276 V219     2.0
    @1278 V220     1.0
    @1279 V221     3.0
    @1282 V222     3.0
    @1285 V223     1.0
    @1286 V224     2.0
    @1288 V225     1.0
    @1289 V226     3.0
    @1292 V227     1.0
    @1293 V228     1.0
    @1294 V229     2.0
    @1296 V230     4.0
    @1300 V231     4.0
    @1304 V232     1.0
    @1305 V233     2.0
    @1307 V234     1.0
    @1308 V235     2.0
    @1310 V237     1.0
    @1311 V238     1.0
    @1312 V239     1.0
    @1313 V240     2.0
    @1315 V241     4.0
    @1319 V242     4.0
    @1323 V243     1.0
    @1324 V244     1.0
    @1325 V301     1.0
    @1326 V302     1.0
    @1327 V302A    1.0
    @1328 V304A_01 1.0
    @1329 V304A_02 1.0
    @1330 V304A_03 1.0
    @1331 V304A_04 1.0
    @1332 V304A_05 1.0
    @1333 V304A_06 1.0
    @1334 V304A_07 1.0
    @1335 V304A_08 1.0
    @1336 V304A_09 1.0
    @1337 V304A_10 1.0
    @1338 V304A_11 1.0
    @1339 V304A_12 1.0
    @1340 V304A_13 1.0
    @1341 V304A_14 1.0
    @1342 V304A_15 1.0
    @1343 V304A_16 1.0
    @1344 V304A_17 1.0
    @1345 V304A_18 1.0
    @1346 V304A_19 1.0
    @1347 V304A_20 1.0
    @1348 V304_01  1.0
    @1349 V304_02  1.0
    @1350 V304_03  1.0
    @1351 V304_04  1.0
    @1352 V304_05  1.0
    @1353 V304_06  1.0
    @1354 V304_07  1.0
    @1355 V304_08  1.0
    @1356 V304_09  1.0
    @1357 V304_10  1.0
    @1358 V304_11  1.0
    @1359 V304_12  1.0
    @1360 V304_13  1.0
    @1361 V304_14  1.0
    @1362 V304_15  1.0
    @1363 V304_16  1.0
    @1364 V304_17  1.0
    @1365 V304_18  1.0
    @1366 V304_19  1.0
    @1367 V304_20  1.0
    @1368 V305_01  1.0
    @1369 V305_02  1.0
    @1370 V305_03  1.0
    @1371 V305_04  1.0
    @1372 V305_05  1.0
    @1373 V305_06  1.0
    @1374 V305_07  1.0
    @1375 V305_08  1.0
    @1376 V305_09  1.0
    @1377 V305_10  1.0
    @1378 V305_11  1.0
    @1379 V305_12  1.0
    @1380 V305_13  1.0
    @1381 V305_14  1.0
    @1382 V305_15  1.0
    @1383 V305_16  1.0
    @1384 V305_17  1.0
    @1385 V305_18  1.0
    @1386 V305_19  1.0
    @1387 V305_20  1.0
    @1388 V307_01  1.0
    @1389 V307_02  1.0
    @1390 V307_03  1.0
    @1391 V307_04  1.0
    @1392 V307_05  1.0
    @1393 V307_06  1.0
    @1394 V307_07  1.0
    @1395 V307_08  1.0
    @1396 V307_09  1.0
    @1397 V307_10  1.0
    @1398 V307_11  1.0
    @1399 V307_12  1.0
    @1400 V307_13  1.0
    @1401 V307_14  1.0
    @1402 V307_15  1.0
    @1403 V307_16  1.0
    @1404 V307_17  1.0
    @1405 V307_18  1.0
    @1406 V307_19  1.0
    @1407 V307_20  1.0
    @1408 V310     2.0
    @1410 V311     1.0
    @1411 V312     2.0
    @1413 V313     1.0
    @1414 V315     2.0
    @1416 V316     4.0
    @1420 V317     4.0
    @1424 V318     1.0
    @1425 V319     1.0
    @1426 V320     1.0
    @1427 V321     1.0
    @1428 V322     1.0
    @1429 V323     2.0
    @1431 V323A    2.0
    @1433 V325A    7.0
    @1440 V326     2.0
    @1442 V327     1.0
    @1443 V337     3.0
    @1446 V359     2.0
    @1448 V360     2.0
    @1450 V361     1.0
    @1451 V362     1.0
    @1452 V363     2.0
    @1454 V364     1.0
    @1455 V367     1.0
    @1456 V372     1.0
    @1457 V372A    1.0
    @1458 V375A    2.0
    @1460 V376     2.0
    @1462 V376A    1.0
    @1463 V379     2.0
    @1465 V380     1.0
    @1466 V384A    1.0
    @1467 V384B    1.0
    @1468 V384C    1.0
    @1469 V384D    1.0
    @1470 V393     1.0
    @1471 V393A    1.0
    @1472 V394     1.0
    @1473 V395     1.0
    @1474 V3A00A   1.0
    @1475 V3A00B   1.0
    @1476 V3A00C   1.0
    @1477 V3A00D   1.0
    @1478 V3A00E   1.0
    @1479 V3A00F   1.0
    @1480 V3A00G   1.0
    @1481 V3A00H   1.0
    @1482 V3A00I   1.0
    @1483 V3A00J   1.0
    @1484 V3A00K   1.0
    @1485 V3A00L   1.0
    @1486 V3A00M   1.0
    @1487 V3A00N   1.0
    @1488 V3A00O   1.0
    @1489 V3A00P   1.0
    @1490 V3A00Q   1.0
    @1491 V3A00R   1.0
    @1492 V3A00S   1.0
    @1493 V3A00T   1.0
    @1494 V3A00U   1.0
    @1495 V3A00V   1.0
    @1496 V3A00W   1.0
    @1497 V3A00X   1.0
    @1498 V3A00Y   1.0
    @1499 V3A00Z   1.0
    @1500 V3A01    1.0
    @1501 V3A02    1.0
    @1502 V3A03    1.0
    @1503 V3A04    1.0
    @1504 V3A05    1.0
    @1505 V3A06    1.0
    @1506 V3A07    2.0
    @1508 V3A08A   1.0
    @1509 V3A08B   1.0
    @1510 V3A08C   1.0
    @1511 V3A08D   1.0
    @1512 V3A08E   1.0
    @1513 V3A08F   1.0
    @1514 V3A08G   1.0
    @1515 V3A08H   1.0
    @1516 V3A08I   1.0
    @1517 V3A08J   1.0
    @1518 V3A08K   1.0
    @1519 V3A08L   1.0
    @1520 V3A08M   1.0
    @1521 V3A08N   1.0
    @1522 V3A08O   1.0
    @1523 V3A08P   1.0
    @1524 V3A08Q   1.0
    @1525 V3A08R   1.0
    @1526 V3A08S   1.0
    @1527 V3A08T   1.0
    @1528 V3A08U   1.0
    @1529 V3A08V   1.0
    @1530 V3A08W   1.0
    @1531 V3A08AA  1.0
    @1532 V3A08AB  1.0
    @1533 V3A08AC  1.0
    @1534 V3A08AD  1.0
    @1535 V3A08X   1.0
    @1536 V3A08Z   1.0
    @1537 V3A09A   3.0
    @1540 V3A09B   3.0
    @1543 MIDX_1   1.0
    @1544 MIDX_2   1.0
    @1545 MIDX_3   1.0
    @1546 MIDX_4   1.0
    @1547 MIDX_5   1.0
    @1548 MIDX_6   1.0
    @1549 M1_1     1.0
    @1550 M1_2     1.0
    @1551 M1_3     1.0
    @1552 M1_4     1.0
    @1553 M1_5     1.0
    @1554 M1_6     1.0
    @1555 M1A_1    1.0
    @1556 M1A_2    1.0
    @1557 M1A_3    1.0
    @1558 M1A_4    1.0
    @1559 M1A_5    1.0
    @1560 M1A_6    1.0
    @1561 M1B_1    2.0
    @1563 M1B_2    2.0
    @1565 M1B_3    2.0
    @1567 M1B_4    2.0
    @1569 M1B_5    2.0
    @1571 M1B_6    2.0
    @1573 M1C_1    4.0
    @1577 M1C_2    4.0
    @1581 M1C_3    4.0
    @1585 M1C_4    4.0
    @1589 M1C_5    4.0
    @1593 M1C_6    4.0
    @1597 M1D_1    2.0
    @1599 M1D_2    2.0
    @1601 M1D_3    2.0
    @1603 M1D_4    2.0
    @1605 M1D_5    2.0
    @1607 M1D_6    2.0
    @1609 M1E_1    4.0
    @1613 M1E_2    4.0
    @1617 M1E_3    4.0
    @1621 M1E_4    4.0
    @1625 M1E_5    4.0
    @1629 M1E_6    4.0
    @1633 M2A_1    1.0
    @1634 M2A_2    1.0
    @1635 M2A_3    1.0
    @1636 M2A_4    1.0
    @1637 M2A_5    1.0
    @1638 M2A_6    1.0
    @1639 M2B_1    1.0
    @1640 M2B_2    1.0
    @1641 M2B_3    1.0
    @1642 M2B_4    1.0
    @1643 M2B_5    1.0
    @1644 M2B_6    1.0
    @1645 M2C_1    1.0
    @1646 M2C_2    1.0
    @1647 M2C_3    1.0
    @1648 M2C_4    1.0
    @1649 M2C_5    1.0
    @1650 M2C_6    1.0
    @1651 M2D_1    1.0
    @1652 M2D_2    1.0
    @1653 M2D_3    1.0
    @1654 M2D_4    1.0
    @1655 M2D_5    1.0
    @1656 M2D_6    1.0
    @1657 M2E_1    1.0
    @1658 M2E_2    1.0
    @1659 M2E_3    1.0
    @1660 M2E_4    1.0
    @1661 M2E_5    1.0
    @1662 M2E_6    1.0
    @1663 M2F_1    1.0
    @1664 M2F_2    1.0
    @1665 M2F_3    1.0
    @1666 M2F_4    1.0
    @1667 M2F_5    1.0
    @1668 M2F_6    1.0
    @1669 M2G_1    1.0
    @1670 M2G_2    1.0
    @1671 M2G_3    1.0
    @1672 M2G_4    1.0
    @1673 M2G_5    1.0
    @1674 M2G_6    1.0
    @1675 M2H_1    1.0
    @1676 M2H_2    1.0
    @1677 M2H_3    1.0
    @1678 M2H_4    1.0
    @1679 M2H_5    1.0
    @1680 M2H_6    1.0
    @1681 M2I_1    1.0
    @1682 M2I_2    1.0
    @1683 M2I_3    1.0
    @1684 M2I_4    1.0
    @1685 M2I_5    1.0
    @1686 M2I_6    1.0
    @1687 M2J_1    1.0
    @1688 M2J_2    1.0
    @1689 M2J_3    1.0
    @1690 M2J_4    1.0
    @1691 M2J_5    1.0
    @1692 M2J_6    1.0
    @1693 M2K_1    1.0
    @1694 M2K_2    1.0
    @1695 M2K_3    1.0
    @1696 M2K_4    1.0
    @1697 M2K_5    1.0
    @1698 M2K_6    1.0
    @1699 M2L_1    1.0
    @1700 M2L_2    1.0
    @1701 M2L_3    1.0
    @1702 M2L_4    1.0
    @1703 M2L_5    1.0
    @1704 M2L_6    1.0
    @1705 M2M_1    1.0
    @1706 M2M_2    1.0
    @1707 M2M_3    1.0
    @1708 M2M_4    1.0
    @1709 M2M_5    1.0
    @1710 M2M_6    1.0
    @1711 M2N_1    1.0
    @1712 M2N_2    1.0
    @1713 M2N_3    1.0
    @1714 M2N_4    1.0
    @1715 M2N_5    1.0
    @1716 M2N_6    1.0
    @1717 M3A_1    1.0
    @1718 M3A_2    1.0
    @1719 M3A_3    1.0
    @1720 M3A_4    1.0
    @1721 M3A_5    1.0
    @1722 M3A_6    1.0
    @1723 M3B_1    1.0
    @1724 M3B_2    1.0
    @1725 M3B_3    1.0
    @1726 M3B_4    1.0
    @1727 M3B_5    1.0
    @1728 M3B_6    1.0
    @1729 M3C_1    1.0
    @1730 M3C_2    1.0
    @1731 M3C_3    1.0
    @1732 M3C_4    1.0
    @1733 M3C_5    1.0
    @1734 M3C_6    1.0
    @1735 M3D_1    1.0
    @1736 M3D_2    1.0
    @1737 M3D_3    1.0
    @1738 M3D_4    1.0
    @1739 M3D_5    1.0
    @1740 M3D_6    1.0
    @1741 M3E_1    1.0
    @1742 M3E_2    1.0
    @1743 M3E_3    1.0
    @1744 M3E_4    1.0
    @1745 M3E_5    1.0
    @1746 M3E_6    1.0
    @1747 M3F_1    1.0
    @1748 M3F_2    1.0
    @1749 M3F_3    1.0
    @1750 M3F_4    1.0
    @1751 M3F_5    1.0
    @1752 M3F_6    1.0
    @1753 M3G_1    1.0
    @1754 M3G_2    1.0
    @1755 M3G_3    1.0
    @1756 M3G_4    1.0
    @1757 M3G_5    1.0
    @1758 M3G_6    1.0
    @1759 M3H_1    1.0
    @1760 M3H_2    1.0
    @1761 M3H_3    1.0
    @1762 M3H_4    1.0
    @1763 M3H_5    1.0
    @1764 M3H_6    1.0
    @1765 M3I_1    1.0
    @1766 M3I_2    1.0
    @1767 M3I_3    1.0
    @1768 M3I_4    1.0
    @1769 M3I_5    1.0
    @1770 M3I_6    1.0
    @1771 M3J_1    1.0
    @1772 M3J_2    1.0
    @1773 M3J_3    1.0
    @1774 M3J_4    1.0
    @1775 M3J_5    1.0
    @1776 M3J_6    1.0
    @1777 M3K_1    1.0
    @1778 M3K_2    1.0
    @1779 M3K_3    1.0
    @1780 M3K_4    1.0
    @1781 M3K_5    1.0
    @1782 M3K_6    1.0
    @1783 M3L_1    1.0
    @1784 M3L_2    1.0
    @1785 M3L_3    1.0
    @1786 M3L_4    1.0
    @1787 M3L_5    1.0
    @1788 M3L_6    1.0
    @1789 M3M_1    1.0
    @1790 M3M_2    1.0
    @1791 M3M_3    1.0
    @1792 M3M_4    1.0
    @1793 M3M_5    1.0
    @1794 M3M_6    1.0
    @1795 M3N_1    1.0
    @1796 M3N_2    1.0
    @1797 M3N_3    1.0
    @1798 M3N_4    1.0
    @1799 M3N_5    1.0
    @1800 M3N_6    1.0
    @1801 M4_1     2.0
    @1803 M4_2     2.0
    @1805 M4_3     2.0
    @1807 M4_4     2.0
    @1809 M4_5     2.0
    @1811 M4_6     2.0
    @1813 M5_1     2.0
    @1815 M5_2     2.0
    @1817 M5_3     2.0
    @1819 M5_4     2.0
    @1821 M5_5     2.0
    @1823 M5_6     2.0
    @1825 M6_1     2.0
    @1827 M6_2     2.0
    @1829 M6_3     2.0
    @1831 M6_4     2.0
    @1833 M6_5     2.0
    @1835 M6_6     2.0
    @1837 M7_1     2.0
    @1839 M7_2     2.0
    @1841 M7_3     2.0
    @1843 M7_4     2.0
    @1845 M7_5     2.0
    @1847 M7_6     2.0
    @1849 M8_1     2.0
    @1851 M8_2     2.0
    @1853 M8_3     2.0
    @1855 M8_4     2.0
    @1857 M8_5     2.0
    @1859 M8_6     2.0
    @1861 M9_1     2.0
    @1863 M9_2     2.0
    @1865 M9_3     2.0
    @1867 M9_4     2.0
    @1869 M9_5     2.0
    @1871 M9_6     2.0
    @1873 M10_1    1.0
    @1874 M10_2    1.0
    @1875 M10_3    1.0
    @1876 M10_4    1.0
    @1877 M10_5    1.0
    @1878 M10_6    1.0
    @1879 M11_1    3.0
    @1882 M11_2    3.0
    @1885 M11_3    3.0
    @1888 M11_4    3.0
    @1891 M11_5    3.0
    @1894 M11_6    3.0
    @1897 M13_1    2.0
    @1899 M13_2    2.0
    @1901 M13_3    2.0
    @1903 M13_4    2.0
    @1905 M13_5    2.0
    @1907 M13_6    2.0
    @1909 M14_1    2.0
    @1911 M14_2    2.0
    @1913 M14_3    2.0
    @1915 M14_4    2.0
    @1917 M14_5    2.0
    @1919 M14_6    2.0
    @1921 M15_1    2.0
    @1923 M15_2    2.0
    @1925 M15_3    2.0
    @1927 M15_4    2.0
    @1929 M15_5    2.0
    @1931 M15_6    2.0
    @1933 M17_1    1.0
    @1934 M17_2    1.0
    @1935 M17_3    1.0
    @1936 M17_4    1.0
    @1937 M17_5    1.0
    @1938 M17_6    1.0
    @1939 M17A_1   1.0
    @1940 M17A_2   1.0
    @1941 M17A_3   1.0
    @1942 M17A_4   1.0
    @1943 M17A_5   1.0
    @1944 M17A_6   1.0
    @1945 M18_1    1.0
    @1946 M18_2    1.0
    @1947 M18_3    1.0
    @1948 M18_4    1.0
    @1949 M18_5    1.0
    @1950 M18_6    1.0
    @1951 M19_1    4.0
    @1955 M19_2    4.0
    @1959 M19_3    4.0
    @1963 M19_4    4.0
    @1967 M19_5    4.0
    @1971 M19_6    4.0
    @1975 M19A_1   1.0
    @1976 M19A_2   1.0
    @1977 M19A_3   1.0
    @1978 M19A_4   1.0
    @1979 M19A_5   1.0
    @1980 M19A_6   1.0
    @1981 M27_1    1.0
    @1982 M27_2    1.0
    @1983 M27_3    1.0
    @1984 M27_4    1.0
    @1985 M27_5    1.0
    @1986 M27_6    1.0
    @1987 M28_1    1.0
    @1988 M28_2    1.0
    @1989 M28_3    1.0
    @1990 M28_4    1.0
    @1991 M28_5    1.0
    @1992 M28_6    1.0
    @1993 M29_1    1.0
    @1994 M29_2    1.0
    @1995 M29_3    1.0
    @1996 M29_4    1.0
    @1997 M29_5    1.0
    @1998 M29_6    1.0
    @1999 M34_1    3.0
    @2002 M34_2    3.0
    @2005 M34_3    3.0
    @2008 M34_4    3.0
    @2011 M34_5    3.0
    @2014 M34_6    3.0
    @2017 M35_1    2.0
    @2019 M35_2    2.0
    @2021 M35_3    2.0
    @2023 M35_4    2.0
    @2025 M35_5    2.0
    @2027 M35_6    2.0
    @2029 M36_1    2.0
    @2031 M36_2    2.0
    @2033 M36_3    2.0
    @2035 M36_4    2.0
    @2037 M36_5    2.0
    @2039 M36_6    2.0
    @2041 M38_1    1.0
    @2042 M38_2    1.0
    @2043 M38_3    1.0
    @2044 M38_4    1.0
    @2045 M38_5    1.0
    @2046 M38_6    1.0
    @2047 M39A_1   1.0
    @2048 M39A_2   1.0
    @2049 M39A_3   1.0
    @2050 M39A_4   1.0
    @2051 M39A_5   1.0
    @2052 M39A_6   1.0
    @2053 M39_1    1.0
    @2054 M39_2    1.0
    @2055 M39_3    1.0
    @2056 M39_4    1.0
    @2057 M39_5    1.0
    @2058 M39_6    1.0
    @2059 M42A_1   1.0
    @2060 M42A_2   1.0
    @2061 M42A_3   1.0
    @2062 M42A_4   1.0
    @2063 M42A_5   1.0
    @2064 M42A_6   1.0
    @2065 M42B_1   1.0
    @2066 M42B_2   1.0
    @2067 M42B_3   1.0
    @2068 M42B_4   1.0
    @2069 M42B_5   1.0
    @2070 M42B_6   1.0
    @2071 M42C_1   1.0
    @2072 M42C_2   1.0
    @2073 M42C_3   1.0
    @2074 M42C_4   1.0
    @2075 M42C_5   1.0
    @2076 M42C_6   1.0
    @2077 M42D_1   1.0
    @2078 M42D_2   1.0
    @2079 M42D_3   1.0
    @2080 M42D_4   1.0
    @2081 M42D_5   1.0
    @2082 M42D_6   1.0
    @2083 M42E_1   1.0
    @2084 M42E_2   1.0
    @2085 M42E_3   1.0
    @2086 M42E_4   1.0
    @2087 M42E_5   1.0
    @2088 M42E_6   1.0
    @2089 M43_1    1.0
    @2090 M43_2    1.0
    @2091 M43_3    1.0
    @2092 M43_4    1.0
    @2093 M43_5    1.0
    @2094 M43_6    1.0
    @2095 M44_1    1.0
    @2096 M44_2    1.0
    @2097 M44_3    1.0
    @2098 M44_4    1.0
    @2099 M44_5    1.0
    @2100 M44_6    1.0
    @2101 M45_1    1.0
    @2102 M45_2    1.0
    @2103 M45_3    1.0
    @2104 M45_4    1.0
    @2105 M45_5    1.0
    @2106 M45_6    1.0
    @2107 M46_1    3.0
    @2110 M46_2    3.0
    @2113 M46_3    3.0
    @2116 M46_4    3.0
    @2119 M46_5    3.0
    @2122 M46_6    3.0
    @2125 M47_1    1.0
    @2126 M47_2    1.0
    @2127 M47_3    1.0
    @2128 M47_4    1.0
    @2129 M47_5    1.0
    @2130 M47_6    1.0
    @2131 M48_1    1.0
    @2132 M48_2    1.0
    @2133 M48_3    1.0
    @2134 M48_4    1.0
    @2135 M48_5    1.0
    @2136 M48_6    1.0
    @2137 M49A_1   1.0
    @2138 M49A_2   1.0
    @2139 M49A_3   1.0
    @2140 M49A_4   1.0
    @2141 M49A_5   1.0
    @2142 M49A_6   1.0
    @2143 M49B_1   1.0
    @2144 M49B_2   1.0
    @2145 M49B_3   1.0
    @2146 M49B_4   1.0
    @2147 M49B_5   1.0
    @2148 M49B_6   1.0
    @2149 M49C_1   1.0
    @2150 M49C_2   1.0
    @2151 M49C_3   1.0
    @2152 M49C_4   1.0
    @2153 M49C_5   1.0
    @2154 M49C_6   1.0
    @2155 M49D_1   1.0
    @2156 M49D_2   1.0
    @2157 M49D_3   1.0
    @2158 M49D_4   1.0
    @2159 M49D_5   1.0
    @2160 M49D_6   1.0
    @2161 M49E_1   1.0
    @2162 M49E_2   1.0
    @2163 M49E_3   1.0
    @2164 M49E_4   1.0
    @2165 M49E_5   1.0
    @2166 M49E_6   1.0
    @2167 M49F_1   1.0
    @2168 M49F_2   1.0
    @2169 M49F_3   1.0
    @2170 M49F_4   1.0
    @2171 M49F_5   1.0
    @2172 M49F_6   1.0
    @2173 M49G_1   1.0
    @2174 M49G_2   1.0
    @2175 M49G_3   1.0
    @2176 M49G_4   1.0
    @2177 M49G_5   1.0
    @2178 M49G_6   1.0
    @2179 M49X_1   1.0
    @2180 M49X_2   1.0
    @2181 M49X_3   1.0
    @2182 M49X_4   1.0
    @2183 M49X_5   1.0
    @2184 M49X_6   1.0
    @2185 M49Z_1   1.0
    @2186 M49Z_2   1.0
    @2187 M49Z_3   1.0
    @2188 M49Z_4   1.0
    @2189 M49Z_5   1.0
    @2190 M49Z_6   1.0
    @2191 M49Y_1   1.0
    @2192 M49Y_2   1.0
    @2193 M49Y_3   1.0
    @2194 M49Y_4   1.0
    @2195 M49Y_5   1.0
    @2196 M49Y_6   1.0
    @2197 M54_1    1.0
    @2198 M54_2    1.0
    @2199 M54_3    1.0
    @2200 M54_4    1.0
    @2201 M54_5    1.0
    @2202 M54_6    1.0
    @2203 M55_1    1.0
    @2204 M55_2    1.0
    @2205 M55_3    1.0
    @2206 M55_4    1.0
    @2207 M55_5    1.0
    @2208 M55_6    1.0
    @2209 M55A_1   1.0
    @2210 M55A_2   1.0
    @2211 M55A_3   1.0
    @2212 M55A_4   1.0
    @2213 M55A_5   1.0
    @2214 M55A_6   1.0
    @2215 M55B_1   1.0
    @2216 M55B_2   1.0
    @2217 M55B_3   1.0
    @2218 M55B_4   1.0
    @2219 M55B_5   1.0
    @2220 M55B_6   1.0
    @2221 M55C_1   1.0
    @2222 M55C_2   1.0
    @2223 M55C_3   1.0
    @2224 M55C_4   1.0
    @2225 M55C_5   1.0
    @2226 M55C_6   1.0
    @2227 M55D_1   1.0
    @2228 M55D_2   1.0
    @2229 M55D_3   1.0
    @2230 M55D_4   1.0
    @2231 M55D_5   1.0
    @2232 M55D_6   1.0
    @2233 M55E_1   1.0
    @2234 M55E_2   1.0
    @2235 M55E_3   1.0
    @2236 M55E_4   1.0
    @2237 M55E_5   1.0
    @2238 M55E_6   1.0
    @2239 M55F_1   1.0
    @2240 M55F_2   1.0
    @2241 M55F_3   1.0
    @2242 M55F_4   1.0
    @2243 M55F_5   1.0
    @2244 M55F_6   1.0
    @2245 M55G_1   1.0
    @2246 M55G_2   1.0
    @2247 M55G_3   1.0
    @2248 M55G_4   1.0
    @2249 M55G_5   1.0
    @2250 M55G_6   1.0
    @2251 M55H_1   1.0
    @2252 M55H_2   1.0
    @2253 M55H_3   1.0
    @2254 M55H_4   1.0
    @2255 M55H_5   1.0
    @2256 M55H_6   1.0
    @2257 M55I_1   1.0
    @2258 M55I_2   1.0
    @2259 M55I_3   1.0
    @2260 M55I_4   1.0
    @2261 M55I_5   1.0
    @2262 M55I_6   1.0
    @2263 M55J_1   1.0
    @2264 M55J_2   1.0
    @2265 M55J_3   1.0
    @2266 M55J_4   1.0
    @2267 M55J_5   1.0
    @2268 M55J_6   1.0
    @2269 M55K_1   1.0
    @2270 M55K_2   1.0
    @2271 M55K_3   1.0
    @2272 M55K_4   1.0
    @2273 M55K_5   1.0
    @2274 M55K_6   1.0
    @2275 M55L_1   1.0
    @2276 M55L_2   1.0
    @2277 M55L_3   1.0
    @2278 M55L_4   1.0
    @2279 M55L_5   1.0
    @2280 M55L_6   1.0
    @2281 M55M_1   1.0
    @2282 M55M_2   1.0
    @2283 M55M_3   1.0
    @2284 M55M_4   1.0
    @2285 M55M_5   1.0
    @2286 M55M_6   1.0
    @2287 M55N_1   1.0
    @2288 M55N_2   1.0
    @2289 M55N_3   1.0
    @2290 M55N_4   1.0
    @2291 M55N_5   1.0
    @2292 M55N_6   1.0
    @2293 M55O_1   1.0
    @2294 M55O_2   1.0
    @2295 M55O_3   1.0
    @2296 M55O_4   1.0
    @2297 M55O_5   1.0
    @2298 M55O_6   1.0
    @2299 M55X_1   1.0
    @2300 M55X_2   1.0
    @2301 M55X_3   1.0
    @2302 M55X_4   1.0
    @2303 M55X_5   1.0
    @2304 M55X_6   1.0
    @2305 M55Z_1   1.0
    @2306 M55Z_2   1.0
    @2307 M55Z_3   1.0
    @2308 M55Z_4   1.0
    @2309 M55Z_5   1.0
    @2310 M55Z_6   1.0
    @2311 M57A_1   1.0
    @2312 M57A_2   1.0
    @2313 M57A_3   1.0
    @2314 M57A_4   1.0
    @2315 M57A_5   1.0
    @2316 M57A_6   1.0
    @2317 M57B_1   1.0
    @2318 M57B_2   1.0
    @2319 M57B_3   1.0
    @2320 M57B_4   1.0
    @2321 M57B_5   1.0
    @2322 M57B_6   1.0
    @2323 M57C_1   1.0
    @2324 M57C_2   1.0
    @2325 M57C_3   1.0
    @2326 M57C_4   1.0
    @2327 M57C_5   1.0
    @2328 M57C_6   1.0
    @2329 M57D_1   1.0
    @2330 M57D_2   1.0
    @2331 M57D_3   1.0
    @2332 M57D_4   1.0
    @2333 M57D_5   1.0
    @2334 M57D_6   1.0
    @2335 M57E_1   1.0
    @2336 M57E_2   1.0
    @2337 M57E_3   1.0
    @2338 M57E_4   1.0
    @2339 M57E_5   1.0
    @2340 M57E_6   1.0
    @2341 M57F_1   1.0
    @2342 M57F_2   1.0
    @2343 M57F_3   1.0
    @2344 M57F_4   1.0
    @2345 M57F_5   1.0
    @2346 M57F_6   1.0
    @2347 M57G_1   1.0
    @2348 M57G_2   1.0
    @2349 M57G_3   1.0
    @2350 M57G_4   1.0
    @2351 M57G_5   1.0
    @2352 M57G_6   1.0
    @2353 M57H_1   1.0
    @2354 M57H_2   1.0
    @2355 M57H_3   1.0
    @2356 M57H_4   1.0
    @2357 M57H_5   1.0
    @2358 M57H_6   1.0
    @2359 M57I_1   1.0
    @2360 M57I_2   1.0
    @2361 M57I_3   1.0
    @2362 M57I_4   1.0
    @2363 M57I_5   1.0
    @2364 M57I_6   1.0
    @2365 M57J_1   1.0
    @2366 M57J_2   1.0
    @2367 M57J_3   1.0
    @2368 M57J_4   1.0
    @2369 M57J_5   1.0
    @2370 M57J_6   1.0
    @2371 M57K_1   1.0
    @2372 M57K_2   1.0
    @2373 M57K_3   1.0
    @2374 M57K_4   1.0
    @2375 M57K_5   1.0
    @2376 M57K_6   1.0
    @2377 M57L_1   1.0
    @2378 M57L_2   1.0
    @2379 M57L_3   1.0
    @2380 M57L_4   1.0
    @2381 M57L_5   1.0
    @2382 M57L_6   1.0
    @2383 M57M_1   1.0
    @2384 M57M_2   1.0
    @2385 M57M_3   1.0
    @2386 M57M_4   1.0
    @2387 M57M_5   1.0
    @2388 M57M_6   1.0
    @2389 M57N_1   1.0
    @2390 M57N_2   1.0
    @2391 M57N_3   1.0
    @2392 M57N_4   1.0
    @2393 M57N_5   1.0
    @2394 M57N_6   1.0
    @2395 M57O_1   1.0
    @2396 M57O_2   1.0
    @2397 M57O_3   1.0
    @2398 M57O_4   1.0
    @2399 M57O_5   1.0
    @2400 M57O_6   1.0
    @2401 M57P_1   1.0
    @2402 M57P_2   1.0
    @2403 M57P_3   1.0
    @2404 M57P_4   1.0
    @2405 M57P_5   1.0
    @2406 M57P_6   1.0
    @2407 M57Q_1   1.0
    @2408 M57Q_2   1.0
    @2409 M57Q_3   1.0
    @2410 M57Q_4   1.0
    @2411 M57Q_5   1.0
    @2412 M57Q_6   1.0
    @2413 M57R_1   1.0
    @2414 M57R_2   1.0
    @2415 M57R_3   1.0
    @2416 M57R_4   1.0
    @2417 M57R_5   1.0
    @2418 M57R_6   1.0
    @2419 M57S_1   1.0
    @2420 M57S_2   1.0
    @2421 M57S_3   1.0
    @2422 M57S_4   1.0
    @2423 M57S_5   1.0
    @2424 M57S_6   1.0
    @2425 M57T_1   1.0
    @2426 M57T_2   1.0
    @2427 M57T_3   1.0
    @2428 M57T_4   1.0
    @2429 M57T_5   1.0
    @2430 M57T_6   1.0
    @2431 M57U_1   1.0
    @2432 M57U_2   1.0
    @2433 M57U_3   1.0
    @2434 M57U_4   1.0
    @2435 M57U_5   1.0
    @2436 M57U_6   1.0
    @2437 M57V_1   1.0
    @2438 M57V_2   1.0
    @2439 M57V_3   1.0
    @2440 M57V_4   1.0
    @2441 M57V_5   1.0
    @2442 M57V_6   1.0
    @2443 M57X_1   1.0
    @2444 M57X_2   1.0
    @2445 M57X_3   1.0
    @2446 M57X_4   1.0
    @2447 M57X_5   1.0
    @2448 M57X_6   1.0
    @2449 M60_1    1.0
    @2450 M60_2    1.0
    @2451 M60_3    1.0
    @2452 M60_4    1.0
    @2453 M60_5    1.0
    @2454 M60_6    1.0
    @2455 M61_1    3.0
    @2458 M61_2    3.0
    @2461 M61_3    3.0
    @2464 M61_4    3.0
    @2467 M61_5    3.0
    @2470 M61_6    3.0
    @2473 M62_1    1.0
    @2474 M62_2    1.0
    @2475 M62_3    1.0
    @2476 M62_4    1.0
    @2477 M62_5    1.0
    @2478 M62_6    1.0
    @2479 M63_1    3.0
    @2482 M63_2    3.0
    @2485 M63_3    3.0
    @2488 M63_4    3.0
    @2491 M63_5    3.0
    @2494 M63_6    3.0
    @2497 M64_1    2.0
    @2499 M64_2    2.0
    @2501 M64_3    2.0
    @2503 M64_4    2.0
    @2505 M64_5    2.0
    @2507 M64_6    2.0
    @2509 M65A_1   1.0
    @2510 M65A_2   1.0
    @2511 M65A_3   1.0
    @2512 M65A_4   1.0
    @2513 M65A_5   1.0
    @2514 M65A_6   1.0
    @2515 M65B_1   1.0
    @2516 M65B_2   1.0
    @2517 M65B_3   1.0
    @2518 M65B_4   1.0
    @2519 M65B_5   1.0
    @2520 M65B_6   1.0
    @2521 M65C_1   1.0
    @2522 M65C_2   1.0
    @2523 M65C_3   1.0
    @2524 M65C_4   1.0
    @2525 M65C_5   1.0
    @2526 M65C_6   1.0
    @2527 M65D_1   1.0
    @2528 M65D_2   1.0
    @2529 M65D_3   1.0
    @2530 M65D_4   1.0
    @2531 M65D_5   1.0
    @2532 M65D_6   1.0
    @2533 M65E_1   1.0
    @2534 M65E_2   1.0
    @2535 M65E_3   1.0
    @2536 M65E_4   1.0
    @2537 M65E_5   1.0
    @2538 M65E_6   1.0
    @2539 M65F_1   1.0
    @2540 M65F_2   1.0
    @2541 M65F_3   1.0
    @2542 M65F_4   1.0
    @2543 M65F_5   1.0
    @2544 M65F_6   1.0
    @2545 M65G_1   1.0
    @2546 M65G_2   1.0
    @2547 M65G_3   1.0
    @2548 M65G_4   1.0
    @2549 M65G_5   1.0
    @2550 M65G_6   1.0
    @2551 M65H_1   1.0
    @2552 M65H_2   1.0
    @2553 M65H_3   1.0
    @2554 M65H_4   1.0
    @2555 M65H_5   1.0
    @2556 M65H_6   1.0
    @2557 M65I_1   1.0
    @2558 M65I_2   1.0
    @2559 M65I_3   1.0
    @2560 M65I_4   1.0
    @2561 M65I_5   1.0
    @2562 M65I_6   1.0
    @2563 M65J_1   1.0
    @2564 M65J_2   1.0
    @2565 M65J_3   1.0
    @2566 M65J_4   1.0
    @2567 M65J_5   1.0
    @2568 M65J_6   1.0
    @2569 M65K_1   1.0
    @2570 M65K_2   1.0
    @2571 M65K_3   1.0
    @2572 M65K_4   1.0
    @2573 M65K_5   1.0
    @2574 M65K_6   1.0
    @2575 M65L_1   1.0
    @2576 M65L_2   1.0
    @2577 M65L_3   1.0
    @2578 M65L_4   1.0
    @2579 M65L_5   1.0
    @2580 M65L_6   1.0
    @2581 M65X_1   1.0
    @2582 M65X_2   1.0
    @2583 M65X_3   1.0
    @2584 M65X_4   1.0
    @2585 M65X_5   1.0
    @2586 M65X_6   1.0
    @2587 M66_1    1.0
    @2588 M66_2    1.0
    @2589 M66_3    1.0
    @2590 M66_4    1.0
    @2591 M66_5    1.0
    @2592 M66_6    1.0
    @2593 M67_1    3.0
    @2596 M67_2    3.0
    @2599 M67_3    3.0
    @2602 M67_4    3.0
    @2605 M67_5    3.0
    @2608 M67_6    3.0
    @2611 M68_1    2.0
    @2613 M68_2    2.0
    @2615 M68_3    2.0
    @2617 M68_4    2.0
    @2619 M68_5    2.0
    @2621 M68_6    2.0
    @2623 M69_1    2.0
    @2625 M69_2    2.0
    @2627 M69_3    2.0
    @2629 M69_4    2.0
    @2631 M69_5    2.0
    @2633 M69_6    2.0
    @2635 M70_1    1.0
    @2636 M70_2    1.0
    @2637 M70_3    1.0
    @2638 M70_4    1.0
    @2639 M70_5    1.0
    @2640 M70_6    1.0
    @2641 M71_1    3.0
    @2644 M71_2    3.0
    @2647 M71_3    3.0
    @2650 M71_4    3.0
    @2653 M71_5    3.0
    @2656 M71_6    3.0
    @2659 M72_1    2.0
    @2661 M72_2    2.0
    @2663 M72_3    2.0
    @2665 M72_4    2.0
    @2667 M72_5    2.0
    @2669 M72_6    2.0
    @2671 M73_1    2.0
    @2673 M73_2    2.0
    @2675 M73_3    2.0
    @2677 M73_4    2.0
    @2679 M73_5    2.0
    @2681 M73_6    2.0
    @2683 M74_1    1.0
    @2684 M74_2    1.0
    @2685 M74_3    1.0
    @2686 M74_4    1.0
    @2687 M74_5    1.0
    @2688 M74_6    1.0
    @2689 M75_1    3.0
    @2692 M75_2    3.0
    @2695 M75_3    3.0
    @2698 M75_4    3.0
    @2701 M75_5    3.0
    @2704 M75_6    3.0
    @2707 M76_1    2.0
    @2709 M76_2    2.0
    @2711 M76_3    2.0
    @2713 M76_4    2.0
    @2715 M76_5    2.0
    @2717 M76_6    2.0
    @2719 M77_1    1.0
    @2720 M77_2    1.0
    @2721 M77_3    1.0
    @2722 M77_4    1.0
    @2723 M77_5    1.0
    @2724 M77_6    1.0
    @2725 M78A_1   1.0
    @2726 M78A_2   1.0
    @2727 M78A_3   1.0
    @2728 M78A_4   1.0
    @2729 M78A_5   1.0
    @2730 M78A_6   1.0
    @2731 M78B_1   1.0
    @2732 M78B_2   1.0
    @2733 M78B_3   1.0
    @2734 M78B_4   1.0
    @2735 M78B_5   1.0
    @2736 M78B_6   1.0
    @2737 M78C_1   1.0
    @2738 M78C_2   1.0
    @2739 M78C_3   1.0
    @2740 M78C_4   1.0
    @2741 M78C_5   1.0
    @2742 M78C_6   1.0
    @2743 M78D_1   1.0
    @2744 M78D_2   1.0
    @2745 M78D_3   1.0
    @2746 M78D_4   1.0
    @2747 M78D_5   1.0
    @2748 M78D_6   1.0
    @2749 M78E_1   1.0
    @2750 M78E_2   1.0
    @2751 M78E_3   1.0
    @2752 M78E_4   1.0
    @2753 M78E_5   1.0
    @2754 M78E_6   1.0
    @2755 M78F_1   1.0
    @2756 M78F_2   1.0
    @2757 M78F_3   1.0
    @2758 M78F_4   1.0
    @2759 M78F_5   1.0
    @2760 M78F_6   1.0
    @2761 M78G_1   1.0
    @2762 M78G_2   1.0
    @2763 M78G_3   1.0
    @2764 M78G_4   1.0
    @2765 M78G_5   1.0
    @2766 M78G_6   1.0
    @2767 M78H_1   1.0
    @2768 M78H_2   1.0
    @2769 M78H_3   1.0
    @2770 M78H_4   1.0
    @2771 M78H_5   1.0
    @2772 M78H_6   1.0
    @2773 M78I_1   1.0
    @2774 M78I_2   1.0
    @2775 M78I_3   1.0
    @2776 M78I_4   1.0
    @2777 M78I_5   1.0
    @2778 M78I_6   1.0
    @2779 M78J_1   1.0
    @2780 M78J_2   1.0
    @2781 M78J_3   1.0
    @2782 M78J_4   1.0
    @2783 M78J_5   1.0
    @2784 M78J_6   1.0
    @2785 V401     1.0
    @2786 V404     1.0
    @2787 V405     1.0
    @2788 V406     1.0
    @2789 V407     2.0
    @2791 V408     2.0
    @2793 V409     1.0
    @2794 V409A    1.0
    @2795 V410     1.0
    @2796 V410A    1.0
    @2797 V411     1.0
    @2798 V411A    1.0
    @2799 V412     1.0
    @2800 V412A    1.0
    @2801 V412B    1.0
    @2802 V412C    1.0
    @2803 V413     1.0
    @2804 V413A    1.0
    @2805 V413B    1.0
    @2806 V413C    1.0
    @2807 V413D    1.0
    @2808 V414A    1.0
    @2809 V414B    1.0
    @2810 V414C    1.0
    @2811 V414D    1.0
    @2812 V414E    1.0
    @2813 V414F    1.0
    @2814 V414G    1.0
    @2815 V414H    1.0
    @2816 V414I    1.0
    @2817 V414J    1.0
    @2818 V414K    1.0
    @2819 V414L    1.0
    @2820 V414M    1.0
    @2821 V414N    1.0
    @2822 V414O    1.0
    @2823 V414P    1.0
    @2824 V414Q    1.0
    @2825 V414R    1.0
    @2826 V414S    1.0
    @2827 V414T    1.0
    @2828 V414U    1.0
    @2829 V414V    1.0
    @2830 V414W    1.0
    @2831 V415     1.0
    @2832 V416     1.0
    @2833 V417     1.0
    @2834 V418     1.0
    @2835 V418A    1.0
    @2836 V419     1.0
    @2837 V420     5.0
    @2842 V421     5.0
    @2847 V426     3.0
    @2850 V437     4.0
    @2854 V438     4.0
    @2858 V439     4.0
    @2862 V440     4.0
    @2866 V441     5.0
    @2871 V442     5.0
    @2876 V443     5.0
    @2881 V444     5.0
    @2886 V444A    4.0
    @2890 V445     4.0
    @2894 V446     4.0
    @2898 V447     1.0
    @2899 V447A    2.0
    @2901 V452A    1.0
    @2902 V452B    2.0
    @2904 V452C    1.0
    @2905 V453     3.0
    @2908 V454     1.0
    @2909 V455     1.0
    @2910 V456     3.0
    @2913 V457     1.0
    @2914 V458     1.0
    @2915 V459     1.0
    @2916 V460     1.0
    @2917 V461     1.0
    @2918 V462     1.0
    @2919 V463A    1.0
    @2920 V463B    1.0
    @2921 V463C    1.0
    @2922 V463D    1.0
    @2923 V463E    1.0
    @2924 V463F    1.0
    @2925 V463G    1.0
    @2926 V463H    1.0
    @2927 V463I    1.0
    @2928 V463J    1.0
    @2929 V463K    1.0
    @2930 V463L    1.0
    @2931 V463X    1.0
    @2932 V463Z    1.0
    @2933 V463AA   1.0
    @2934 V463AB   1.0
    @2935 V464     2.0
    @2937 V465     2.0
    @2939 V466     1.0
    @2940 V467A    1.0
    @2941 V467B    1.0
    @2942 V467C    1.0
    @2943 V467D    1.0
    @2944 V467E    1.0
    @2945 V467F    1.0
    @2946 V467G    1.0
    @2947 V467H    1.0
    @2948 V467I    1.0
    @2949 V467J    1.0
    @2950 V467K    1.0
    @2951 V467L    1.0
    @2952 V467M    1.0
    @2953 V468     1.0
    @2954 V469E    1.0
    @2955 V469F    1.0
    @2956 V469X    1.0
    @2957 V471A    1.0
    @2958 V471B    1.0
    @2959 V471C    1.0
    @2960 V471D    1.0
    @2961 V471E    1.0
    @2962 V471F    1.0
    @2963 V471G    1.0
    @2964 V472A    1.0
    @2965 V472B    1.0
    @2966 V472C    1.0
    @2967 V472D    1.0
    @2968 V472E    1.0
    @2969 V472F    1.0
    @2970 V472G    1.0
    @2971 V472H    1.0
    @2972 V472I    1.0
    @2973 V472J    1.0
    @2974 V472K    1.0
    @2975 V472L    1.0
    @2976 V472M    1.0
    @2977 V472N    1.0
    @2978 V472O    1.0
    @2979 V472P    1.0
    @2980 V472Q    1.0
    @2981 V472R    1.0
    @2982 V472S    1.0
    @2983 V472T    1.0
    @2984 V472U    1.0
    @2985 V473A    1.0
    @2986 V473B    1.0
    @2987 V474     1.0
    @2988 V474A    1.0
    @2989 V474B    1.0
    @2990 V474C    1.0
    @2991 V474D    1.0
    @2992 V474E    1.0
    @2993 V474F    1.0
    @2994 V474G    1.0
    @2995 V474H    1.0
    @2996 V474I    1.0
    @2997 V474J    1.0
    @2998 V474X    1.0
    @2999 V474Z    1.0
    @3000 V475     1.0
    @3001 V476     1.0
    @3002 V477     2.0
    @3004 V478     2.0
    @3006 V479     2.0
    @3008 V480     1.0
    @3009 V481     1.0
    @3010 V481A    1.0
    @3011 V481B    1.0
    @3012 V481C    1.0
    @3013 V481D    1.0
    @3014 V481E    1.0
    @3015 V481F    1.0
    @3016 V481G    1.0
    @3017 V481H    1.0
    @3018 V481X    1.0
    @3019 V482A    1.0
    @3020 V482B    1.0
    @3021 V482C    1.0
    @3022 HIDX_1   1.0
    @3023 HIDX_2   1.0
    @3024 HIDX_3   1.0
    @3025 HIDX_4   1.0
    @3026 HIDX_5   1.0
    @3027 HIDX_6   1.0
    @3028 H1_1     1.0
    @3029 H1_2     1.0
    @3030 H1_3     1.0
    @3031 H1_4     1.0
    @3032 H1_5     1.0
    @3033 H1_6     1.0
    @3034 H1A_1    2.0
    @3036 H1A_2    2.0
    @3038 H1A_3    2.0
    @3040 H1A_4    2.0
    @3042 H1A_5    2.0
    @3044 H1A_6    2.0
    @3046 H2_1     1.0
    @3047 H2_2     1.0
    @3048 H2_3     1.0
    @3049 H2_4     1.0
    @3050 H2_5     1.0
    @3051 H2_6     1.0
    @3052 H2D_1    2.0
    @3054 H2D_2    2.0
    @3056 H2D_3    2.0
    @3058 H2D_4    2.0
    @3060 H2D_5    2.0
    @3062 H2D_6    2.0
    @3064 H2M_1    2.0
    @3066 H2M_2    2.0
    @3068 H2M_3    2.0
    @3070 H2M_4    2.0
    @3072 H2M_5    2.0
    @3074 H2M_6    2.0
    @3076 H2Y_1    4.0
    @3080 H2Y_2    4.0
    @3084 H2Y_3    4.0
    @3088 H2Y_4    4.0
    @3092 H2Y_5    4.0
    @3096 H2Y_6    4.0
    @3100 H3_1     1.0
    @3101 H3_2     1.0
    @3102 H3_3     1.0
    @3103 H3_4     1.0
    @3104 H3_5     1.0
    @3105 H3_6     1.0
    @3106 H3D_1    2.0
    @3108 H3D_2    2.0
    @3110 H3D_3    2.0
    @3112 H3D_4    2.0
    @3114 H3D_5    2.0
    @3116 H3D_6    2.0
    @3118 H3M_1    2.0
    @3120 H3M_2    2.0
    @3122 H3M_3    2.0
    @3124 H3M_4    2.0
    @3126 H3M_5    2.0
    @3128 H3M_6    2.0
    @3130 H3Y_1    4.0
    @3134 H3Y_2    4.0
    @3138 H3Y_3    4.0
    @3142 H3Y_4    4.0
    @3146 H3Y_5    4.0
    @3150 H3Y_6    4.0
    @3154 H4_1     1.0
    @3155 H4_2     1.0
    @3156 H4_3     1.0
    @3157 H4_4     1.0
    @3158 H4_5     1.0
    @3159 H4_6     1.0
    @3160 H4D_1    2.0
    @3162 H4D_2    2.0
    @3164 H4D_3    2.0
    @3166 H4D_4    2.0
    @3168 H4D_5    2.0
    @3170 H4D_6    2.0
    @3172 H4M_1    2.0
    @3174 H4M_2    2.0
    @3176 H4M_3    2.0
    @3178 H4M_4    2.0
    @3180 H4M_5    2.0
    @3182 H4M_6    2.0
    @3184 H4Y_1    4.0
    @3188 H4Y_2    4.0
    @3192 H4Y_3    4.0
    @3196 H4Y_4    4.0
    @3200 H4Y_5    4.0
    @3204 H4Y_6    4.0
    @3208 H5_1     1.0
    @3209 H5_2     1.0
    @3210 H5_3     1.0
    @3211 H5_4     1.0
    @3212 H5_5     1.0
    @3213 H5_6     1.0
    @3214 H5D_1    2.0
    @3216 H5D_2    2.0
    @3218 H5D_3    2.0
    @3220 H5D_4    2.0
    @3222 H5D_5    2.0
    @3224 H5D_6    2.0
    @3226 H5M_1    2.0
    @3228 H5M_2    2.0
    @3230 H5M_3    2.0
    @3232 H5M_4    2.0
    @3234 H5M_5    2.0
    @3236 H5M_6    2.0
    @3238 H5Y_1    4.0
    @3242 H5Y_2    4.0
    @3246 H5Y_3    4.0
    @3250 H5Y_4    4.0
    @3254 H5Y_5    4.0
    @3258 H5Y_6    4.0
    @3262 H6_1     1.0
    @3263 H6_2     1.0
    @3264 H6_3     1.0
    @3265 H6_4     1.0
    @3266 H6_5     1.0
    @3267 H6_6     1.0
    @3268 H6D_1    2.0
    @3270 H6D_2    2.0
    @3272 H6D_3    2.0
    @3274 H6D_4    2.0
    @3276 H6D_5    2.0
    @3278 H6D_6    2.0
    @3280 H6M_1    2.0
    @3282 H6M_2    2.0
    @3284 H6M_3    2.0
    @3286 H6M_4    2.0
    @3288 H6M_5    2.0
    @3290 H6M_6    2.0
    @3292 H6Y_1    4.0
    @3296 H6Y_2    4.0
    @3300 H6Y_3    4.0
    @3304 H6Y_4    4.0
    @3308 H6Y_5    4.0
    @3312 H6Y_6    4.0
    @3316 H7_1     1.0
    @3317 H7_2     1.0
    @3318 H7_3     1.0
    @3319 H7_4     1.0
    @3320 H7_5     1.0
    @3321 H7_6     1.0
    @3322 H7D_1    2.0
    @3324 H7D_2    2.0
    @3326 H7D_3    2.0
    @3328 H7D_4    2.0
    @3330 H7D_5    2.0
    @3332 H7D_6    2.0
    @3334 H7M_1    2.0
    @3336 H7M_2    2.0
    @3338 H7M_3    2.0
    @3340 H7M_4    2.0
    @3342 H7M_5    2.0
    @3344 H7M_6    2.0
    @3346 H7Y_1    4.0
    @3350 H7Y_2    4.0
    @3354 H7Y_3    4.0
    @3358 H7Y_4    4.0
    @3362 H7Y_5    4.0
    @3366 H7Y_6    4.0
    @3370 H8_1     1.0
    @3371 H8_2     1.0
    @3372 H8_3     1.0
    @3373 H8_4     1.0
    @3374 H8_5     1.0
    @3375 H8_6     1.0
    @3376 H8D_1    2.0
    @3378 H8D_2    2.0
    @3380 H8D_3    2.0
    @3382 H8D_4    2.0
    @3384 H8D_5    2.0
    @3386 H8D_6    2.0
    @3388 H8M_1    2.0
    @3390 H8M_2    2.0
    @3392 H8M_3    2.0
    @3394 H8M_4    2.0
    @3396 H8M_5    2.0
    @3398 H8M_6    2.0
    @3400 H8Y_1    4.0
    @3404 H8Y_2    4.0
    @3408 H8Y_3    4.0
    @3412 H8Y_4    4.0
    @3416 H8Y_5    4.0
    @3420 H8Y_6    4.0
    @3424 H9_1     1.0
    @3425 H9_2     1.0
    @3426 H9_3     1.0
    @3427 H9_4     1.0
    @3428 H9_5     1.0
    @3429 H9_6     1.0
    @3430 H9D_1    2.0
    @3432 H9D_2    2.0
    @3434 H9D_3    2.0
    @3436 H9D_4    2.0
    @3438 H9D_5    2.0
    @3440 H9D_6    2.0
    @3442 H9M_1    2.0
    @3444 H9M_2    2.0
    @3446 H9M_3    2.0
    @3448 H9M_4    2.0
    @3450 H9M_5    2.0
    @3452 H9M_6    2.0
    @3454 H9Y_1    4.0
    @3458 H9Y_2    4.0
    @3462 H9Y_3    4.0
    @3466 H9Y_4    4.0
    @3470 H9Y_5    4.0
    @3474 H9Y_6    4.0
    @3478 H9A_1    1.0
    @3479 H9A_2    1.0
    @3480 H9A_3    1.0
    @3481 H9A_4    1.0
    @3482 H9A_5    1.0
    @3483 H9A_6    1.0
    @3484 H9AD_1   2.0
    @3486 H9AD_2   2.0
    @3488 H9AD_3   2.0
    @3490 H9AD_4   2.0
    @3492 H9AD_5   2.0
    @3494 H9AD_6   2.0
    @3496 H9AM_1   2.0
    @3498 H9AM_2   2.0
    @3500 H9AM_3   2.0
    @3502 H9AM_4   2.0
    @3504 H9AM_5   2.0
    @3506 H9AM_6   2.0
    @3508 H9AY_1   4.0
    @3512 H9AY_2   4.0
    @3516 H9AY_3   4.0
    @3520 H9AY_4   4.0
    @3524 H9AY_5   4.0
    @3528 H9AY_6   4.0
    @3532 H0_1     1.0
    @3533 H0_2     1.0
    @3534 H0_3     1.0
    @3535 H0_4     1.0
    @3536 H0_5     1.0
    @3537 H0_6     1.0
    @3538 H0D_1    2.0
    @3540 H0D_2    2.0
    @3542 H0D_3    2.0
    @3544 H0D_4    2.0
    @3546 H0D_5    2.0
    @3548 H0D_6    2.0
    @3550 H0M_1    2.0
    @3552 H0M_2    2.0
    @3554 H0M_3    2.0
    @3556 H0M_4    2.0
    @3558 H0M_5    2.0
    @3560 H0M_6    2.0
    @3562 H0Y_1    4.0
    @3566 H0Y_2    4.0
    @3570 H0Y_3    4.0
    @3574 H0Y_4    4.0
    @3578 H0Y_5    4.0
    @3582 H0Y_6    4.0
    @3586 H10_1    1.0
    @3587 H10_2    1.0
    @3588 H10_3    1.0
    @3589 H10_4    1.0
    @3590 H10_5    1.0
    @3591 H10_6    1.0
    @3592 H33_1    1.0
    @3593 H33_2    1.0
    @3594 H33_3    1.0
    @3595 H33_4    1.0
    @3596 H33_5    1.0
    @3597 H33_6    1.0
    @3598 H33D_1   2.0
    @3600 H33D_2   2.0
    @3602 H33D_3   2.0
    @3604 H33D_4   2.0
    @3606 H33D_5   2.0
    @3608 H33D_6   2.0
    @3610 H33M_1   2.0
    @3612 H33M_2   2.0
    @3614 H33M_3   2.0
    @3616 H33M_4   2.0
    @3618 H33M_5   2.0
    @3620 H33M_6   2.0
    @3622 H33Y_1   4.0
    @3626 H33Y_2   4.0
    @3630 H33Y_3   4.0
    @3634 H33Y_4   4.0
    @3638 H33Y_5   4.0
    @3642 H33Y_6   4.0
    @3646 H35_1    1.0
    @3647 H35_2    1.0
    @3648 H35_3    1.0
    @3649 H35_4    1.0
    @3650 H35_5    1.0
    @3651 H35_6    1.0
    @3652 H36A_1   1.0
    @3653 H36A_2   1.0
    @3654 H36A_3   1.0
    @3655 H36A_4   1.0
    @3656 H36A_5   1.0
    @3657 H36A_6   1.0
    @3658 H36B_1   1.0
    @3659 H36B_2   1.0
    @3660 H36B_3   1.0
    @3661 H36B_4   1.0
    @3662 H36B_5   1.0
    @3663 H36B_6   1.0
    @3664 H36C_1   1.0
    @3665 H36C_2   1.0
    @3666 H36C_3   1.0
    @3667 H36C_4   1.0
    @3668 H36C_5   1.0
    @3669 H36C_6   1.0
    @3670 H36D_1   1.0
    @3671 H36D_2   1.0
    @3672 H36D_3   1.0
    @3673 H36D_4   1.0
    @3674 H36D_5   1.0
    @3675 H36D_6   1.0
    @3676 H36E_1   1.0
    @3677 H36E_2   1.0
    @3678 H36E_3   1.0
    @3679 H36E_4   1.0
    @3680 H36E_5   1.0
    @3681 H36E_6   1.0
    @3682 H36F_1   1.0
    @3683 H36F_2   1.0
    @3684 H36F_3   1.0
    @3685 H36F_4   1.0
    @3686 H36F_5   1.0
    @3687 H36F_6   1.0
    @3688 H40_1    1.0
    @3689 H40_2    1.0
    @3690 H40_3    1.0
    @3691 H40_4    1.0
    @3692 H40_5    1.0
    @3693 H40_6    1.0
    @3694 H40D_1   2.0
    @3696 H40D_2   2.0
    @3698 H40D_3   2.0
    @3700 H40D_4   2.0
    @3702 H40D_5   2.0
    @3704 H40D_6   2.0
    @3706 H40M_1   2.0
    @3708 H40M_2   2.0
    @3710 H40M_3   2.0
    @3712 H40M_4   2.0
    @3714 H40M_5   2.0
    @3716 H40M_6   2.0
    @3718 H40Y_1   4.0
    @3722 H40Y_2   4.0
    @3726 H40Y_3   4.0
    @3730 H40Y_4   4.0
    @3734 H40Y_5   4.0
    @3738 H40Y_6   4.0
    @3742 H41A_1   1.0
    @3743 H41A_2   1.0
    @3744 H41A_3   1.0
    @3745 H41A_4   1.0
    @3746 H41A_5   1.0
    @3747 H41A_6   1.0
    @3748 H41B_1   1.0
    @3749 H41B_2   1.0
    @3750 H41B_3   1.0
    @3751 H41B_4   1.0
    @3752 H41B_5   1.0
    @3753 H41B_6   1.0
    @3754 H50_1    1.0
    @3755 H50_2    1.0
    @3756 H50_3    1.0
    @3757 H50_4    1.0
    @3758 H50_5    1.0
    @3759 H50_6    1.0
    @3760 H50D_1   2.0
    @3762 H50D_2   2.0
    @3764 H50D_3   2.0
    @3766 H50D_4   2.0
    @3768 H50D_5   2.0
    @3770 H50D_6   2.0
    @3772 H50M_1   2.0
    @3774 H50M_2   2.0
    @3776 H50M_3   2.0
    @3778 H50M_4   2.0
    @3780 H50M_5   2.0
    @3782 H50M_6   2.0
    @3784 H50Y_1   4.0
    @3788 H50Y_2   4.0
    @3792 H50Y_3   4.0
    @3796 H50Y_4   4.0
    @3800 H50Y_5   4.0
    @3804 H50Y_6   4.0
    @3808 H51_1    1.0
    @3809 H51_2    1.0
    @3810 H51_3    1.0
    @3811 H51_4    1.0
    @3812 H51_5    1.0
    @3813 H51_6    1.0
    @3814 H51D_1   2.0
    @3816 H51D_2   2.0
    @3818 H51D_3   2.0
    @3820 H51D_4   2.0
    @3822 H51D_5   2.0
    @3824 H51D_6   2.0
    @3826 H51M_1   2.0
    @3828 H51M_2   2.0
    @3830 H51M_3   2.0
    @3832 H51M_4   2.0
    @3834 H51M_5   2.0
    @3836 H51M_6   2.0
    @3838 H51Y_1   4.0
    @3842 H51Y_2   4.0
    @3846 H51Y_3   4.0
    @3850 H51Y_4   4.0
    @3854 H51Y_5   4.0
    @3858 H51Y_6   4.0
    @3862 H52_1    1.0
    @3863 H52_2    1.0
    @3864 H52_3    1.0
    @3865 H52_4    1.0
    @3866 H52_5    1.0
    @3867 H52_6    1.0
    @3868 H52D_1   2.0
    @3870 H52D_2   2.0
    @3872 H52D_3   2.0
    @3874 H52D_4   2.0
    @3876 H52D_5   2.0
    @3878 H52D_6   2.0
    @3880 H52M_1   2.0
    @3882 H52M_2   2.0
    @3884 H52M_3   2.0
    @3886 H52M_4   2.0
    @3888 H52M_5   2.0
    @3890 H52M_6   2.0
    @3892 H52Y_1   4.0
    @3896 H52Y_2   4.0
    @3900 H52Y_3   4.0
    @3904 H52Y_4   4.0
    @3908 H52Y_5   4.0
    @3912 H52Y_6   4.0
    @3916 H53_1    1.0
    @3917 H53_2    1.0
    @3918 H53_3    1.0
    @3919 H53_4    1.0
    @3920 H53_5    1.0
    @3921 H53_6    1.0
    @3922 H53D_1   2.0
    @3924 H53D_2   2.0
    @3926 H53D_3   2.0
    @3928 H53D_4   2.0
    @3930 H53D_5   2.0
    @3932 H53D_6   2.0
    @3934 H53M_1   2.0
    @3936 H53M_2   2.0
    @3938 H53M_3   2.0
    @3940 H53M_4   2.0
    @3942 H53M_5   2.0
    @3944 H53M_6   2.0
    @3946 H53Y_1   4.0
    @3950 H53Y_2   4.0
    @3954 H53Y_3   4.0
    @3958 H53Y_4   4.0
    @3962 H53Y_5   4.0
    @3966 H53Y_6   4.0
    @3970 H54_1    1.0
    @3971 H54_2    1.0
    @3972 H54_3    1.0
    @3973 H54_4    1.0
    @3974 H54_5    1.0
    @3975 H54_6    1.0
    @3976 H54D_1   2.0
    @3978 H54D_2   2.0
    @3980 H54D_3   2.0
    @3982 H54D_4   2.0
    @3984 H54D_5   2.0
    @3986 H54D_6   2.0
    @3988 H54M_1   2.0
    @3990 H54M_2   2.0
    @3992 H54M_3   2.0
    @3994 H54M_4   2.0
    @3996 H54M_5   2.0
    @3998 H54M_6   2.0
    @4000 H54Y_1   4.0
    @4004 H54Y_2   4.0
    @4008 H54Y_3   4.0
    @4012 H54Y_4   4.0
    @4016 H54Y_5   4.0
    @4020 H54Y_6   4.0
    @4024 H55_1    1.0
    @4025 H55_2    1.0
    @4026 H55_3    1.0
    @4027 H55_4    1.0
    @4028 H55_5    1.0
    @4029 H55_6    1.0
    @4030 H55D_1   2.0
    @4032 H55D_2   2.0
    @4034 H55D_3   2.0
    @4036 H55D_4   2.0
    @4038 H55D_5   2.0
    @4040 H55D_6   2.0
    @4042 H55M_1   2.0
    @4044 H55M_2   2.0
    @4046 H55M_3   2.0
    @4048 H55M_4   2.0
    @4050 H55M_5   2.0
    @4052 H55M_6   2.0
    @4054 H55Y_1   4.0
    @4058 H55Y_2   4.0
    @4062 H55Y_3   4.0
    @4066 H55Y_4   4.0
    @4070 H55Y_5   4.0
    @4074 H55Y_6   4.0
    @4078 H56_1    1.0
    @4079 H56_2    1.0
    @4080 H56_3    1.0
    @4081 H56_4    1.0
    @4082 H56_5    1.0
    @4083 H56_6    1.0
    @4084 H56D_1   2.0
    @4086 H56D_2   2.0
    @4088 H56D_3   2.0
    @4090 H56D_4   2.0
    @4092 H56D_5   2.0
    @4094 H56D_6   2.0
    @4096 H56M_1   2.0
    @4098 H56M_2   2.0
    @4100 H56M_3   2.0
    @4102 H56M_4   2.0
    @4104 H56M_5   2.0
    @4106 H56M_6   2.0
    @4108 H56Y_1   4.0
    @4112 H56Y_2   4.0
    @4116 H56Y_3   4.0
    @4120 H56Y_4   4.0
    @4124 H56Y_5   4.0
    @4128 H56Y_6   4.0
    @4132 H57_1    1.0
    @4133 H57_2    1.0
    @4134 H57_3    1.0
    @4135 H57_4    1.0
    @4136 H57_5    1.0
    @4137 H57_6    1.0
    @4138 H57D_1   2.0
    @4140 H57D_2   2.0
    @4142 H57D_3   2.0
    @4144 H57D_4   2.0
    @4146 H57D_5   2.0
    @4148 H57D_6   2.0
    @4150 H57M_1   2.0
    @4152 H57M_2   2.0
    @4154 H57M_3   2.0
    @4156 H57M_4   2.0
    @4158 H57M_5   2.0
    @4160 H57M_6   2.0
    @4162 H57Y_1   4.0
    @4166 H57Y_2   4.0
    @4170 H57Y_3   4.0
    @4174 H57Y_4   4.0
    @4178 H57Y_5   4.0
    @4182 H57Y_6   4.0
    @4186 H58_1    1.0
    @4187 H58_2    1.0
    @4188 H58_3    1.0
    @4189 H58_4    1.0
    @4190 H58_5    1.0
    @4191 H58_6    1.0
    @4192 H58D_1   2.0
    @4194 H58D_2   2.0
    @4196 H58D_3   2.0
    @4198 H58D_4   2.0
    @4200 H58D_5   2.0
    @4202 H58D_6   2.0
    @4204 H58M_1   2.0
    @4206 H58M_2   2.0
    @4208 H58M_3   2.0
    @4210 H58M_4   2.0
    @4212 H58M_5   2.0
    @4214 H58M_6   2.0
    @4216 H58Y_1   4.0
    @4220 H58Y_2   4.0
    @4224 H58Y_3   4.0
    @4228 H58Y_4   4.0
    @4232 H58Y_5   4.0
    @4236 H58Y_6   4.0
    @4240 H59_1    1.0
    @4241 H59_2    1.0
    @4242 H59_3    1.0
    @4243 H59_4    1.0
    @4244 H59_5    1.0
    @4245 H59_6    1.0
    @4246 H59D_1   2.0
    @4248 H59D_2   2.0
    @4250 H59D_3   2.0
    @4252 H59D_4   2.0
    @4254 H59D_5   2.0
    @4256 H59D_6   2.0
    @4258 H59M_1   2.0
    @4260 H59M_2   2.0
    @4262 H59M_3   2.0
    @4264 H59M_4   2.0
    @4266 H59M_5   2.0
    @4268 H59M_6   2.0
    @4270 H59Y_1   4.0
    @4274 H59Y_2   4.0
    @4278 H59Y_3   4.0
    @4282 H59Y_4   4.0
    @4286 H59Y_5   4.0
    @4290 H59Y_6   4.0
    @4294 H60_1    1.0
    @4295 H60_2    1.0
    @4296 H60_3    1.0
    @4297 H60_4    1.0
    @4298 H60_5    1.0
    @4299 H60_6    1.0
    @4300 H60D_1   2.0
    @4302 H60D_2   2.0
    @4304 H60D_3   2.0
    @4306 H60D_4   2.0
    @4308 H60D_5   2.0
    @4310 H60D_6   2.0
    @4312 H60M_1   2.0
    @4314 H60M_2   2.0
    @4316 H60M_3   2.0
    @4318 H60M_4   2.0
    @4320 H60M_5   2.0
    @4322 H60M_6   2.0
    @4324 H60Y_1   4.0
    @4328 H60Y_2   4.0
    @4332 H60Y_3   4.0
    @4336 H60Y_4   4.0
    @4340 H60Y_5   4.0
    @4344 H60Y_6   4.0
    @4348 H61_1    1.0
    @4349 H61_2    1.0
    @4350 H61_3    1.0
    @4351 H61_4    1.0
    @4352 H61_5    1.0
    @4353 H61_6    1.0
    @4354 H61D_1   2.0
    @4356 H61D_2   2.0
    @4358 H61D_3   2.0
    @4360 H61D_4   2.0
    @4362 H61D_5   2.0
    @4364 H61D_6   2.0
    @4366 H61M_1   2.0
    @4368 H61M_2   2.0
    @4370 H61M_3   2.0
    @4372 H61M_4   2.0
    @4374 H61M_5   2.0
    @4376 H61M_6   2.0
    @4378 H61Y_1   4.0
    @4382 H61Y_2   4.0
    @4386 H61Y_3   4.0
    @4390 H61Y_4   4.0
    @4394 H61Y_5   4.0
    @4398 H61Y_6   4.0
    @4402 H62_1    1.0
    @4403 H62_2    1.0
    @4404 H62_3    1.0
    @4405 H62_4    1.0
    @4406 H62_5    1.0
    @4407 H62_6    1.0
    @4408 H62D_1   2.0
    @4410 H62D_2   2.0
    @4412 H62D_3   2.0
    @4414 H62D_4   2.0
    @4416 H62D_5   2.0
    @4418 H62D_6   2.0
    @4420 H62M_1   2.0
    @4422 H62M_2   2.0
    @4424 H62M_3   2.0
    @4426 H62M_4   2.0
    @4428 H62M_5   2.0
    @4430 H62M_6   2.0
    @4432 H62Y_1   4.0
    @4436 H62Y_2   4.0
    @4440 H62Y_3   4.0
    @4444 H62Y_4   4.0
    @4448 H62Y_5   4.0
    @4452 H62Y_6   4.0
    @4456 H63_1    1.0
    @4457 H63_2    1.0
    @4458 H63_3    1.0
    @4459 H63_4    1.0
    @4460 H63_5    1.0
    @4461 H63_6    1.0
    @4462 H63D_1   2.0
    @4464 H63D_2   2.0
    @4466 H63D_3   2.0
    @4468 H63D_4   2.0
    @4470 H63D_5   2.0
    @4472 H63D_6   2.0
    @4474 H63M_1   2.0
    @4476 H63M_2   2.0
    @4478 H63M_3   2.0
    @4480 H63M_4   2.0
    @4482 H63M_5   2.0
    @4484 H63M_6   2.0
    @4486 H63Y_1   4.0
    @4490 H63Y_2   4.0
    @4494 H63Y_3   4.0
    @4498 H63Y_4   4.0
    @4502 H63Y_5   4.0
    @4506 H63Y_6   4.0
    @4510 H64_1    1.0
    @4511 H64_2    1.0
    @4512 H64_3    1.0
    @4513 H64_4    1.0
    @4514 H64_5    1.0
    @4515 H64_6    1.0
    @4516 H64D_1   2.0
    @4518 H64D_2   2.0
    @4520 H64D_3   2.0
    @4522 H64D_4   2.0
    @4524 H64D_5   2.0
    @4526 H64D_6   2.0
    @4528 H64M_1   2.0
    @4530 H64M_2   2.0
    @4532 H64M_3   2.0
    @4534 H64M_4   2.0
    @4536 H64M_5   2.0
    @4538 H64M_6   2.0
    @4540 H64Y_1   4.0
    @4544 H64Y_2   4.0
    @4548 H64Y_3   4.0
    @4552 H64Y_4   4.0
    @4556 H64Y_5   4.0
    @4560 H64Y_6   4.0
    @4564 H65_1    1.0
    @4565 H65_2    1.0
    @4566 H65_3    1.0
    @4567 H65_4    1.0
    @4568 H65_5    1.0
    @4569 H65_6    1.0
    @4570 H65D_1   2.0
    @4572 H65D_2   2.0
    @4574 H65D_3   2.0
    @4576 H65D_4   2.0
    @4578 H65D_5   2.0
    @4580 H65D_6   2.0
    @4582 H65M_1   2.0
    @4584 H65M_2   2.0
    @4586 H65M_3   2.0
    @4588 H65M_4   2.0
    @4590 H65M_5   2.0
    @4592 H65M_6   2.0
    @4594 H65Y_1   4.0
    @4598 H65Y_2   4.0
    @4602 H65Y_3   4.0
    @4606 H65Y_4   4.0
    @4610 H65Y_5   4.0
    @4614 H65Y_6   4.0
    @4618 H66_1    1.0
    @4619 H66_2    1.0
    @4620 H66_3    1.0
    @4621 H66_4    1.0
    @4622 H66_5    1.0
    @4623 H66_6    1.0
    @4624 H66D_1   2.0
    @4626 H66D_2   2.0
    @4628 H66D_3   2.0
    @4630 H66D_4   2.0
    @4632 H66D_5   2.0
    @4634 H66D_6   2.0
    @4636 H66M_1   2.0
    @4638 H66M_2   2.0
    @4640 H66M_3   2.0
    @4642 H66M_4   2.0
    @4644 H66M_5   2.0
    @4646 H66M_6   2.0
    @4648 H66Y_1   4.0
    @4652 H66Y_2   4.0
    @4656 H66Y_3   4.0
    @4660 H66Y_4   4.0
    @4664 H66Y_5   4.0
    @4668 H66Y_6   4.0
    @4672 H80A_1   1.0
    @4673 H80A_2   1.0
    @4674 H80A_3   1.0
    @4675 H80A_4   1.0
    @4676 H80A_5   1.0
    @4677 H80A_6   1.0
    @4678 H80B_1   1.0
    @4679 H80B_2   1.0
    @4680 H80B_3   1.0
    @4681 H80B_4   1.0
    @4682 H80B_5   1.0
    @4683 H80B_6   1.0
    @4684 H80C_1   1.0
    @4685 H80C_2   1.0
    @4686 H80C_3   1.0
    @4687 H80C_4   1.0
    @4688 H80C_5   1.0
    @4689 H80C_6   1.0
    @4690 H80D_1   1.0
    @4691 H80D_2   1.0
    @4692 H80D_3   1.0
    @4693 H80D_4   1.0
    @4694 H80D_5   1.0
    @4695 H80D_6   1.0
    @4696 H80E_1   1.0
    @4697 H80E_2   1.0
    @4698 H80E_3   1.0
    @4699 H80E_4   1.0
    @4700 H80E_5   1.0
    @4701 H80E_6   1.0
    @4702 H80F_1   1.0
    @4703 H80F_2   1.0
    @4704 H80F_3   1.0
    @4705 H80F_4   1.0
    @4706 H80F_5   1.0
    @4707 H80F_6   1.0
    @4708 H80G_1   1.0
    @4709 H80G_2   1.0
    @4710 H80G_3   1.0
    @4711 H80G_4   1.0
    @4712 H80G_5   1.0
    @4713 H80G_6   1.0
    @4714 HIDXA_1  1.0
    @4715 HIDXA_2  1.0
    @4716 HIDXA_3  1.0
    @4717 HIDXA_4  1.0
    @4718 HIDXA_5  1.0
    @4719 HIDXA_6  1.0
    @4720 H11_1    1.0
    @4721 H11_2    1.0
    @4722 H11_3    1.0
    @4723 H11_4    1.0
    @4724 H11_5    1.0
    @4725 H11_6    1.0
    @4726 H11B_1   1.0
    @4727 H11B_2   1.0
    @4728 H11B_3   1.0
    @4729 H11B_4   1.0
    @4730 H11B_5   1.0
    @4731 H11B_6   1.0
    @4732 H12A_1   1.0
    @4733 H12A_2   1.0
    @4734 H12A_3   1.0
    @4735 H12A_4   1.0
    @4736 H12A_5   1.0
    @4737 H12A_6   1.0
    @4738 H12B_1   1.0
    @4739 H12B_2   1.0
    @4740 H12B_3   1.0
    @4741 H12B_4   1.0
    @4742 H12B_5   1.0
    @4743 H12B_6   1.0
    @4744 H12C_1   1.0
    @4745 H12C_2   1.0
    @4746 H12C_3   1.0
    @4747 H12C_4   1.0
    @4748 H12C_5   1.0
    @4749 H12C_6   1.0
    @4750 H12D_1   1.0
    @4751 H12D_2   1.0
    @4752 H12D_3   1.0
    @4753 H12D_4   1.0
    @4754 H12D_5   1.0
    @4755 H12D_6   1.0
    @4756 H12E_1   1.0
    @4757 H12E_2   1.0
    @4758 H12E_3   1.0
    @4759 H12E_4   1.0
    @4760 H12E_5   1.0
    @4761 H12E_6   1.0
    @4762 H12F_1   1.0
    @4763 H12F_2   1.0
    @4764 H12F_3   1.0
    @4765 H12F_4   1.0
    @4766 H12F_5   1.0
    @4767 H12F_6   1.0
    @4768 H12G_1   1.0
    @4769 H12G_2   1.0
    @4770 H12G_3   1.0
    @4771 H12G_4   1.0
    @4772 H12G_5   1.0
    @4773 H12G_6   1.0
    @4774 H12H_1   1.0
    @4775 H12H_2   1.0
    @4776 H12H_3   1.0
    @4777 H12H_4   1.0
    @4778 H12H_5   1.0
    @4779 H12H_6   1.0
    @4780 H12I_1   1.0
    @4781 H12I_2   1.0
    @4782 H12I_3   1.0
    @4783 H12I_4   1.0
    @4784 H12I_5   1.0
    @4785 H12I_6   1.0
    @4786 H12J_1   1.0
    @4787 H12J_2   1.0
    @4788 H12J_3   1.0
    @4789 H12J_4   1.0
    @4790 H12J_5   1.0
    @4791 H12J_6   1.0
    @4792 H12K_1   1.0
    @4793 H12K_2   1.0
    @4794 H12K_3   1.0
    @4795 H12K_4   1.0
    @4796 H12K_5   1.0
    @4797 H12K_6   1.0
    @4798 H12L_1   1.0
    @4799 H12L_2   1.0
    @4800 H12L_3   1.0
    @4801 H12L_4   1.0
    @4802 H12L_5   1.0
    @4803 H12L_6   1.0
    @4804 H12M_1   1.0
    @4805 H12M_2   1.0
    @4806 H12M_3   1.0
    @4807 H12M_4   1.0
    @4808 H12M_5   1.0
    @4809 H12M_6   1.0
    @4810 H12N_1   1.0
    @4811 H12N_2   1.0
    @4812 H12N_3   1.0
    @4813 H12N_4   1.0
    @4814 H12N_5   1.0
    @4815 H12N_6   1.0
    @4816 H12O_1   1.0
    @4817 H12O_2   1.0
    @4818 H12O_3   1.0
    @4819 H12O_4   1.0
    @4820 H12O_5   1.0
    @4821 H12O_6   1.0
    @4822 H12P_1   1.0
    @4823 H12P_2   1.0
    @4824 H12P_3   1.0
    @4825 H12P_4   1.0
    @4826 H12P_5   1.0
    @4827 H12P_6   1.0
    @4828 H12Q_1   1.0
    @4829 H12Q_2   1.0
    @4830 H12Q_3   1.0
    @4831 H12Q_4   1.0
    @4832 H12Q_5   1.0
    @4833 H12Q_6   1.0
    @4834 H12R_1   1.0
    @4835 H12R_2   1.0
    @4836 H12R_3   1.0
    @4837 H12R_4   1.0
    @4838 H12R_5   1.0
    @4839 H12R_6   1.0
    @4840 H12S_1   1.0
    @4841 H12S_2   1.0
    @4842 H12S_3   1.0
    @4843 H12S_4   1.0
    @4844 H12S_5   1.0
    @4845 H12S_6   1.0
    @4846 H12T_1   1.0
    @4847 H12T_2   1.0
    @4848 H12T_3   1.0
    @4849 H12T_4   1.0
    @4850 H12T_5   1.0
    @4851 H12T_6   1.0
    @4852 H12U_1   1.0
    @4853 H12U_2   1.0
    @4854 H12U_3   1.0
    @4855 H12U_4   1.0
    @4856 H12U_5   1.0
    @4857 H12U_6   1.0
    @4858 H12V_1   1.0
    @4859 H12V_2   1.0
    @4860 H12V_3   1.0
    @4861 H12V_4   1.0
    @4862 H12V_5   1.0
    @4863 H12V_6   1.0
    @4864 H12W_1   1.0
    @4865 H12W_2   1.0
    @4866 H12W_3   1.0
    @4867 H12W_4   1.0
    @4868 H12W_5   1.0
    @4869 H12W_6   1.0
    @4870 H12X_1   1.0
    @4871 H12X_2   1.0
    @4872 H12X_3   1.0
    @4873 H12X_4   1.0
    @4874 H12X_5   1.0
    @4875 H12X_6   1.0
    @4876 H12Y_1   1.0
    @4877 H12Y_2   1.0
    @4878 H12Y_3   1.0
    @4879 H12Y_4   1.0
    @4880 H12Y_5   1.0
    @4881 H12Y_6   1.0
    @4882 H12Z_1   1.0
    @4883 H12Z_2   1.0
    @4884 H12Z_3   1.0
    @4885 H12Z_4   1.0
    @4886 H12Z_5   1.0
    @4887 H12Z_6   1.0
    @4888 H13_1    1.0
    @4889 H13_2    1.0
    @4890 H13_3    1.0
    @4891 H13_4    1.0
    @4892 H13_5    1.0
    @4893 H13_6    1.0
    @4894 H13B_1   1.0
    @4895 H13B_2   1.0
    @4896 H13B_3   1.0
    @4897 H13B_4   1.0
    @4898 H13B_5   1.0
    @4899 H13B_6   1.0
    @4900 H14_1    1.0
    @4901 H14_2    1.0
    @4902 H14_3    1.0
    @4903 H14_4    1.0
    @4904 H14_5    1.0
    @4905 H14_6    1.0
    @4906 H15_1    1.0
    @4907 H15_2    1.0
    @4908 H15_3    1.0
    @4909 H15_4    1.0
    @4910 H15_5    1.0
    @4911 H15_6    1.0
    @4912 H15A_1   1.0
    @4913 H15A_2   1.0
    @4914 H15A_3   1.0
    @4915 H15A_4   1.0
    @4916 H15A_5   1.0
    @4917 H15A_6   1.0
    @4918 H15B_1   1.0
    @4919 H15B_2   1.0
    @4920 H15B_3   1.0
    @4921 H15B_4   1.0
    @4922 H15B_5   1.0
    @4923 H15B_6   1.0
    @4924 H15C_1   1.0
    @4925 H15C_2   1.0
    @4926 H15C_3   1.0
    @4927 H15C_4   1.0
    @4928 H15C_5   1.0
    @4929 H15C_6   1.0
    @4930 H15D_1   1.0
    @4931 H15D_2   1.0
    @4932 H15D_3   1.0
    @4933 H15D_4   1.0
    @4934 H15D_5   1.0
    @4935 H15D_6   1.0
    @4936 H15E_1   1.0
    @4937 H15E_2   1.0
    @4938 H15E_3   1.0
    @4939 H15E_4   1.0
    @4940 H15E_5   1.0
    @4941 H15E_6   1.0
    @4942 H15F_1   1.0
    @4943 H15F_2   1.0
    @4944 H15F_3   1.0
    @4945 H15F_4   1.0
    @4946 H15F_5   1.0
    @4947 H15F_6   1.0
    @4948 H15G_1   1.0
    @4949 H15G_2   1.0
    @4950 H15G_3   1.0
    @4951 H15G_4   1.0
    @4952 H15G_5   1.0
    @4953 H15G_6   1.0
    @4954 H15H_1   1.0
    @4955 H15H_2   1.0
    @4956 H15H_3   1.0
    @4957 H15H_4   1.0
    @4958 H15H_5   1.0
    @4959 H15H_6   1.0
    @4960 H15I_1   1.0
    @4961 H15I_2   1.0
    @4962 H15I_3   1.0
    @4963 H15I_4   1.0
    @4964 H15I_5   1.0
    @4965 H15I_6   1.0
    @4966 H15J_1   1.0
    @4967 H15J_2   1.0
    @4968 H15J_3   1.0
    @4969 H15J_4   1.0
    @4970 H15J_5   1.0
    @4971 H15J_6   1.0
    @4972 H15K_1   1.0
    @4973 H15K_2   1.0
    @4974 H15K_3   1.0
    @4975 H15K_4   1.0
    @4976 H15K_5   1.0
    @4977 H15K_6   1.0
    @4978 H15L_1   1.0
    @4979 H15L_2   1.0
    @4980 H15L_3   1.0
    @4981 H15L_4   1.0
    @4982 H15L_5   1.0
    @4983 H15L_6   1.0
    @4984 H15M_1   1.0
    @4985 H15M_2   1.0
    @4986 H15M_3   1.0
    @4987 H15M_4   1.0
    @4988 H15M_5   1.0
    @4989 H15M_6   1.0
    @4990 H20_1    1.0
    @4991 H20_2    1.0
    @4992 H20_3    1.0
    @4993 H20_4    1.0
    @4994 H20_5    1.0
    @4995 H20_6    1.0
    @4996 H21A_1   1.0
    @4997 H21A_2   1.0
    @4998 H21A_3   1.0
    @4999 H21A_4   1.0
    @5000 H21A_5   1.0
    @5001 H21A_6   1.0
    @5002 H21_1    1.0
    @5003 H21_2    1.0
    @5004 H21_3    1.0
    @5005 H21_4    1.0
    @5006 H21_5    1.0
    @5007 H21_6    1.0
    @5008 H22_1    1.0
    @5009 H22_2    1.0
    @5010 H22_3    1.0
    @5011 H22_4    1.0
    @5012 H22_5    1.0
    @5013 H22_6    1.0
    @5014 H31_1    1.0
    @5015 H31_2    1.0
    @5016 H31_3    1.0
    @5017 H31_4    1.0
    @5018 H31_5    1.0
    @5019 H31_6    1.0
    @5020 H31B_1   1.0
    @5021 H31B_2   1.0
    @5022 H31B_3   1.0
    @5023 H31B_4   1.0
    @5024 H31B_5   1.0
    @5025 H31B_6   1.0
    @5026 H31C_1   1.0
    @5027 H31C_2   1.0
    @5028 H31C_3   1.0
    @5029 H31C_4   1.0
    @5030 H31C_5   1.0
    @5031 H31C_6   1.0
    @5032 H31D_1   1.0
    @5033 H31D_2   1.0
    @5034 H31D_3   1.0
    @5035 H31D_4   1.0
    @5036 H31D_5   1.0
    @5037 H31D_6   1.0
    @5038 H31E_1   1.0
    @5039 H31E_2   1.0
    @5040 H31E_3   1.0
    @5041 H31E_4   1.0
    @5042 H31E_5   1.0
    @5043 H31E_6   1.0
    @5044 H32A_1   1.0
    @5045 H32A_2   1.0
    @5046 H32A_3   1.0
    @5047 H32A_4   1.0
    @5048 H32A_5   1.0
    @5049 H32A_6   1.0
    @5050 H32B_1   1.0
    @5051 H32B_2   1.0
    @5052 H32B_3   1.0
    @5053 H32B_4   1.0
    @5054 H32B_5   1.0
    @5055 H32B_6   1.0
    @5056 H32C_1   1.0
    @5057 H32C_2   1.0
    @5058 H32C_3   1.0
    @5059 H32C_4   1.0
    @5060 H32C_5   1.0
    @5061 H32C_6   1.0
    @5062 H32D_1   1.0
    @5063 H32D_2   1.0
    @5064 H32D_3   1.0
    @5065 H32D_4   1.0
    @5066 H32D_5   1.0
    @5067 H32D_6   1.0
    @5068 H32E_1   1.0
    @5069 H32E_2   1.0
    @5070 H32E_3   1.0
    @5071 H32E_4   1.0
    @5072 H32E_5   1.0
    @5073 H32E_6   1.0
    @5074 H32F_1   1.0
    @5075 H32F_2   1.0
    @5076 H32F_3   1.0
    @5077 H32F_4   1.0
    @5078 H32F_5   1.0
    @5079 H32F_6   1.0
    @5080 H32G_1   1.0
    @5081 H32G_2   1.0
    @5082 H32G_3   1.0
    @5083 H32G_4   1.0
    @5084 H32G_5   1.0
    @5085 H32G_6   1.0
    @5086 H32H_1   1.0
    @5087 H32H_2   1.0
    @5088 H32H_3   1.0
    @5089 H32H_4   1.0
    @5090 H32H_5   1.0
    @5091 H32H_6   1.0
    @5092 H32I_1   1.0
    @5093 H32I_2   1.0
    @5094 H32I_3   1.0
    @5095 H32I_4   1.0
    @5096 H32I_5   1.0
    @5097 H32I_6   1.0
    @5098 H32J_1   1.0
    @5099 H32J_2   1.0
    @5100 H32J_3   1.0
    @5101 H32J_4   1.0
    @5102 H32J_5   1.0
    @5103 H32J_6   1.0
    @5104 H32K_1   1.0
    @5105 H32K_2   1.0
    @5106 H32K_3   1.0
    @5107 H32K_4   1.0
    @5108 H32K_5   1.0
    @5109 H32K_6   1.0
    @5110 H32L_1   1.0
    @5111 H32L_2   1.0
    @5112 H32L_3   1.0
    @5113 H32L_4   1.0
    @5114 H32L_5   1.0
    @5115 H32L_6   1.0
    @5116 H32M_1   1.0
    @5117 H32M_2   1.0
    @5118 H32M_3   1.0
    @5119 H32M_4   1.0
    @5120 H32M_5   1.0
    @5121 H32M_6   1.0
    @5122 H32N_1   1.0
    @5123 H32N_2   1.0
    @5124 H32N_3   1.0
    @5125 H32N_4   1.0
    @5126 H32N_5   1.0
    @5127 H32N_6   1.0
    @5128 H32O_1   1.0
    @5129 H32O_2   1.0
    @5130 H32O_3   1.0
    @5131 H32O_4   1.0
    @5132 H32O_5   1.0
    @5133 H32O_6   1.0
    @5134 H32P_1   1.0
    @5135 H32P_2   1.0
    @5136 H32P_3   1.0
    @5137 H32P_4   1.0
    @5138 H32P_5   1.0
    @5139 H32P_6   1.0
    @5140 H32Q_1   1.0
    @5141 H32Q_2   1.0
    @5142 H32Q_3   1.0
    @5143 H32Q_4   1.0
    @5144 H32Q_5   1.0
    @5145 H32Q_6   1.0
    @5146 H32R_1   1.0
    @5147 H32R_2   1.0
    @5148 H32R_3   1.0
    @5149 H32R_4   1.0
    @5150 H32R_5   1.0
    @5151 H32R_6   1.0
    @5152 H32S_1   1.0
    @5153 H32S_2   1.0
    @5154 H32S_3   1.0
    @5155 H32S_4   1.0
    @5156 H32S_5   1.0
    @5157 H32S_6   1.0
    @5158 H32T_1   1.0
    @5159 H32T_2   1.0
    @5160 H32T_3   1.0
    @5161 H32T_4   1.0
    @5162 H32T_5   1.0
    @5163 H32T_6   1.0
    @5164 H32U_1   1.0
    @5165 H32U_2   1.0
    @5166 H32U_3   1.0
    @5167 H32U_4   1.0
    @5168 H32U_5   1.0
    @5169 H32U_6   1.0
    @5170 H32V_1   1.0
    @5171 H32V_2   1.0
    @5172 H32V_3   1.0
    @5173 H32V_4   1.0
    @5174 H32V_5   1.0
    @5175 H32V_6   1.0
    @5176 H32W_1   1.0
    @5177 H32W_2   1.0
    @5178 H32W_3   1.0
    @5179 H32W_4   1.0
    @5180 H32W_5   1.0
    @5181 H32W_6   1.0
    @5182 H32X_1   1.0
    @5183 H32X_2   1.0
    @5184 H32X_3   1.0
    @5185 H32X_4   1.0
    @5186 H32X_5   1.0
    @5187 H32X_6   1.0
    @5188 H32Y_1   1.0
    @5189 H32Y_2   1.0
    @5190 H32Y_3   1.0
    @5191 H32Y_4   1.0
    @5192 H32Y_5   1.0
    @5193 H32Y_6   1.0
    @5194 H32Z_1   1.0
    @5195 H32Z_2   1.0
    @5196 H32Z_3   1.0
    @5197 H32Z_4   1.0
    @5198 H32Z_5   1.0
    @5199 H32Z_6   1.0
    @5200 H34_1    1.0
    @5201 H34_2    1.0
    @5202 H34_3    1.0
    @5203 H34_4    1.0
    @5204 H34_5    1.0
    @5205 H34_6    1.0
    @5206 H37A_1   1.0
    @5207 H37A_2   1.0
    @5208 H37A_3   1.0
    @5209 H37A_4   1.0
    @5210 H37A_5   1.0
    @5211 H37A_6   1.0
    @5212 H37B_1   1.0
    @5213 H37B_2   1.0
    @5214 H37B_3   1.0
    @5215 H37B_4   1.0
    @5216 H37B_5   1.0
    @5217 H37B_6   1.0
    @5218 H37C_1   1.0
    @5219 H37C_2   1.0
    @5220 H37C_3   1.0
    @5221 H37C_4   1.0
    @5222 H37C_5   1.0
    @5223 H37C_6   1.0
    @5224 H37D_1   1.0
    @5225 H37D_2   1.0
    @5226 H37D_3   1.0
    @5227 H37D_4   1.0
    @5228 H37D_5   1.0
    @5229 H37D_6   1.0
    @5230 H37DA_1  1.0
    @5231 H37DA_2  1.0
    @5232 H37DA_3  1.0
    @5233 H37DA_4  1.0
    @5234 H37DA_5  1.0
    @5235 H37DA_6  1.0
    @5236 H37E_1   1.0
    @5237 H37E_2   1.0
    @5238 H37E_3   1.0
    @5239 H37E_4   1.0
    @5240 H37E_5   1.0
    @5241 H37E_6   1.0
    @5242 H37AA_1  1.0
    @5243 H37AA_2  1.0
    @5244 H37AA_3  1.0
    @5245 H37AA_4  1.0
    @5246 H37AA_5  1.0
    @5247 H37AA_6  1.0
    @5248 H37AB_1  1.0
    @5249 H37AB_2  1.0
    @5250 H37AB_3  1.0
    @5251 H37AB_4  1.0
    @5252 H37AB_5  1.0
    @5253 H37AB_6  1.0
    @5254 H37F_1   1.0
    @5255 H37F_2   1.0
    @5256 H37F_3   1.0
    @5257 H37F_4   1.0
    @5258 H37F_5   1.0
    @5259 H37F_6   1.0
    @5260 H37G_1   1.0
    @5261 H37G_2   1.0
    @5262 H37G_3   1.0
    @5263 H37G_4   1.0
    @5264 H37G_5   1.0
    @5265 H37G_6   1.0
    @5266 H37H_1   1.0
    @5267 H37H_2   1.0
    @5268 H37H_3   1.0
    @5269 H37H_4   1.0
    @5270 H37H_5   1.0
    @5271 H37H_6   1.0
    @5272 H37I_1   1.0
    @5273 H37I_2   1.0
    @5274 H37I_3   1.0
    @5275 H37I_4   1.0
    @5276 H37I_5   1.0
    @5277 H37I_6   1.0
    @5278 H37J_1   1.0
    @5279 H37J_2   1.0
    @5280 H37J_3   1.0
    @5281 H37J_4   1.0
    @5282 H37J_5   1.0
    @5283 H37J_6   1.0
    @5284 H37K_1   1.0
    @5285 H37K_2   1.0
    @5286 H37K_3   1.0
    @5287 H37K_4   1.0
    @5288 H37K_5   1.0
    @5289 H37K_6   1.0
    @5290 H37L_1   1.0
    @5291 H37L_2   1.0
    @5292 H37L_3   1.0
    @5293 H37L_4   1.0
    @5294 H37L_5   1.0
    @5295 H37L_6   1.0
    @5296 H37M_1   1.0
    @5297 H37M_2   1.0
    @5298 H37M_3   1.0
    @5299 H37M_4   1.0
    @5300 H37M_5   1.0
    @5301 H37M_6   1.0
    @5302 H37N_1   1.0
    @5303 H37N_2   1.0
    @5304 H37N_3   1.0
    @5305 H37N_4   1.0
    @5306 H37N_5   1.0
    @5307 H37N_6   1.0
    @5308 H37O_1   1.0
    @5309 H37O_2   1.0
    @5310 H37O_3   1.0
    @5311 H37O_4   1.0
    @5312 H37O_5   1.0
    @5313 H37O_6   1.0
    @5314 H37P_1   1.0
    @5315 H37P_2   1.0
    @5316 H37P_3   1.0
    @5317 H37P_4   1.0
    @5318 H37P_5   1.0
    @5319 H37P_6   1.0
    @5320 H37X_1   1.0
    @5321 H37X_2   1.0
    @5322 H37X_3   1.0
    @5323 H37X_4   1.0
    @5324 H37X_5   1.0
    @5325 H37X_6   1.0
    @5326 H37Y_1   1.0
    @5327 H37Y_2   1.0
    @5328 H37Y_3   1.0
    @5329 H37Y_4   1.0
    @5330 H37Y_5   1.0
    @5331 H37Y_6   1.0
    @5332 H37Z_1   1.0
    @5333 H37Z_2   1.0
    @5334 H37Z_3   1.0
    @5335 H37Z_4   1.0
    @5336 H37Z_5   1.0
    @5337 H37Z_6   1.0
    @5338 H38_1    1.0
    @5339 H38_2    1.0
    @5340 H38_3    1.0
    @5341 H38_4    1.0
    @5342 H38_5    1.0
    @5343 H38_6    1.0
    @5344 H39_1    1.0
    @5345 H39_2    1.0
    @5346 H39_3    1.0
    @5347 H39_4    1.0
    @5348 H39_5    1.0
    @5349 H39_6    1.0
    @5350 H42_1    1.0
    @5351 H42_2    1.0
    @5352 H42_3    1.0
    @5353 H42_4    1.0
    @5354 H42_5    1.0
    @5355 H42_6    1.0
    @5356 H43_1    1.0
    @5357 H43_2    1.0
    @5358 H43_3    1.0
    @5359 H43_4    1.0
    @5360 H43_5    1.0
    @5361 H43_6    1.0
    @5362 H44A_1   2.0
    @5364 H44A_2   2.0
    @5366 H44A_3   2.0
    @5368 H44A_4   2.0
    @5370 H44A_5   2.0
    @5372 H44A_6   2.0
    @5374 H44B_1   2.0
    @5376 H44B_2   2.0
    @5378 H44B_3   2.0
    @5380 H44B_4   2.0
    @5382 H44B_5   2.0
    @5384 H44B_6   2.0
    @5386 H44C_1   1.0
    @5387 H44C_2   1.0
    @5388 H44C_3   1.0
    @5389 H44C_4   1.0
    @5390 H44C_5   1.0
    @5391 H44C_6   1.0
    @5392 H45_1    2.0
    @5394 H45_2    2.0
    @5396 H45_3    2.0
    @5398 H45_4    2.0
    @5400 H45_5    2.0
    @5402 H45_6    2.0
    @5404 H46A_1   2.0
    @5406 H46A_2   2.0
    @5408 H46A_3   2.0
    @5410 H46A_4   2.0
    @5412 H46A_5   2.0
    @5414 H46A_6   2.0
    @5416 H46B_1   2.0
    @5418 H46B_2   2.0
    @5420 H46B_3   2.0
    @5422 H46B_4   2.0
    @5424 H46B_5   2.0
    @5426 H46B_6   2.0
    @5428 H47_1    1.0
    @5429 H47_2    1.0
    @5430 H47_3    1.0
    @5431 H47_4    1.0
    @5432 H47_5    1.0
    @5433 H47_6    1.0
    @5434 HWIDX_1  1.0
    @5435 HWIDX_2  1.0
    @5436 HWIDX_3  1.0
    @5437 HWIDX_4  1.0
    @5438 HWIDX_5  1.0
    @5439 HWIDX_6  1.0
    @5440 HW1_1    2.0
    @5442 HW1_2    2.0
    @5444 HW1_3    2.0
    @5446 HW1_4    2.0
    @5448 HW1_5    2.0
    @5450 HW1_6    2.0
    @5452 HW2_1    4.0
    @5456 HW2_2    4.0
    @5460 HW2_3    4.0
    @5464 HW2_4    4.0
    @5468 HW2_5    4.0
    @5472 HW2_6    4.0
    @5476 HW3_1    4.0
    @5480 HW3_2    4.0
    @5484 HW3_3    4.0
    @5488 HW3_4    4.0
    @5492 HW3_5    4.0
    @5496 HW3_6    4.0
    @5500 HW4_1    4.0
    @5504 HW4_2    4.0
    @5508 HW4_3    4.0
    @5512 HW4_4    4.0
    @5516 HW4_5    4.0
    @5520 HW4_6    4.0
    @5524 HW5_1    4.0
    @5528 HW5_2    4.0
    @5532 HW5_3    4.0
    @5536 HW5_4    4.0
    @5540 HW5_5    4.0
    @5544 HW5_6    4.0
    @5548 HW6_1    5.0
    @5553 HW6_2    5.0
    @5558 HW6_3    5.0
    @5563 HW6_4    5.0
    @5568 HW6_5    5.0
    @5573 HW6_6    5.0
    @5578 HW7_1    4.0
    @5582 HW7_2    4.0
    @5586 HW7_3    4.0
    @5590 HW7_4    4.0
    @5594 HW7_5    4.0
    @5598 HW7_6    4.0
    @5602 HW8_1    4.0
    @5606 HW8_2    4.0
    @5610 HW8_3    4.0
    @5614 HW8_4    4.0
    @5618 HW8_5    4.0
    @5622 HW8_6    4.0
    @5626 HW9_1    5.0
    @5631 HW9_2    5.0
    @5636 HW9_3    5.0
    @5641 HW9_4    5.0
    @5646 HW9_5    5.0
    @5651 HW9_6    5.0
    @5656 HW10_1   4.0
    @5660 HW10_2   4.0
    @5664 HW10_3   4.0
    @5668 HW10_4   4.0
    @5672 HW10_5   4.0
    @5676 HW10_6   4.0
    @5680 HW11_1   4.0
    @5684 HW11_2   4.0
    @5688 HW11_3   4.0
    @5692 HW11_4   4.0
    @5696 HW11_5   4.0
    @5700 HW11_6   4.0
    @5704 HW12_1   5.0
    @5709 HW12_2   5.0
    @5714 HW12_3   5.0
    @5719 HW12_4   5.0
    @5724 HW12_5   5.0
    @5729 HW12_6   5.0
    @5734 HW13_1   1.0
    @5735 HW13_2   1.0
    @5736 HW13_3   1.0
    @5737 HW13_4   1.0
    @5738 HW13_5   1.0
    @5739 HW13_6   1.0
    @5740 HW15_1   1.0
    @5741 HW15_2   1.0
    @5742 HW15_3   1.0
    @5743 HW15_4   1.0
    @5744 HW15_5   1.0
    @5745 HW15_6   1.0
    @5746 HW16_1   2.0
    @5748 HW16_2   2.0
    @5750 HW16_3   2.0
    @5752 HW16_4   2.0
    @5754 HW16_5   2.0
    @5756 HW16_6   2.0
    @5758 HW17_1   2.0
    @5760 HW17_2   2.0
    @5762 HW17_3   2.0
    @5764 HW17_4   2.0
    @5766 HW17_5   2.0
    @5768 HW17_6   2.0
    @5770 HW18_1   2.0
    @5772 HW18_2   2.0
    @5774 HW18_3   2.0
    @5776 HW18_4   2.0
    @5778 HW18_5   2.0
    @5780 HW18_6   2.0
    @5782 HW19_1   4.0
    @5786 HW19_2   4.0
    @5790 HW19_3   4.0
    @5794 HW19_4   4.0
    @5798 HW19_5   4.0
    @5802 HW19_6   4.0
    @5806 HW51_1   2.0
    @5808 HW51_2   2.0
    @5810 HW51_3   2.0
    @5812 HW51_4   2.0
    @5814 HW51_5   2.0
    @5816 HW51_6   2.0
    @5818 HW52_1   1.0
    @5819 HW52_2   1.0
    @5820 HW52_3   1.0
    @5821 HW52_4   1.0
    @5822 HW52_5   1.0
    @5823 HW52_6   1.0
    @5824 HW53_1   3.0
    @5827 HW53_2   3.0
    @5830 HW53_3   3.0
    @5833 HW53_4   3.0
    @5836 HW53_5   3.0
    @5839 HW53_6   3.0
    @5842 HW55_1   1.0
    @5843 HW55_2   1.0
    @5844 HW55_3   1.0
    @5845 HW55_4   1.0
    @5846 HW55_5   1.0
    @5847 HW55_6   1.0
    @5848 HW56_1   4.0
    @5852 HW56_2   4.0
    @5856 HW56_3   4.0
    @5860 HW56_4   4.0
    @5864 HW56_5   4.0
    @5868 HW56_6   4.0
    @5872 HW57_1   1.0
    @5873 HW57_2   1.0
    @5874 HW57_3   1.0
    @5875 HW57_4   1.0
    @5876 HW57_5   1.0
    @5877 HW57_6   1.0
    @5878 HW58_1   1.0
    @5879 HW58_2   1.0
    @5880 HW58_3   1.0
    @5881 HW58_4   1.0
    @5882 HW58_5   1.0
    @5883 HW58_6   1.0
    @5884 HW70_1   4.0
    @5888 HW70_2   4.0
    @5892 HW70_3   4.0
    @5896 HW70_4   4.0
    @5900 HW70_5   4.0
    @5904 HW70_6   4.0
    @5908 HW71_1   4.0
    @5912 HW71_2   4.0
    @5916 HW71_3   4.0
    @5920 HW71_4   4.0
    @5924 HW71_5   4.0
    @5928 HW71_6   4.0
    @5932 HW72_1   4.0
    @5936 HW72_2   4.0
    @5940 HW72_3   4.0
    @5944 HW72_4   4.0
    @5948 HW72_5   4.0
    @5952 HW72_6   4.0
    @5956 HW73_1   4.0
    @5960 HW73_2   4.0
    @5964 HW73_3   4.0
    @5968 HW73_4   4.0
    @5972 HW73_5   4.0
    @5976 HW73_6   4.0
    @5980 V501     1.0
    @5981 V502     1.0
    @5982 V503     1.0
    @5983 V504     1.0
    @5984 V505     2.0
    @5986 V506     2.0
    @5988 V507     2.0
    @5990 V508     4.0
    @5994 V509     4.0
    @5998 V510     1.0
    @5999 V511     2.0
    @6001 V512     2.0
    @6003 V513     1.0
    @6004 V525     2.0
    @6006 V527     3.0
    @6009 V528     2.0
    @6011 V529     3.0
    @6014 V530     1.0
    @6015 V531     2.0
    @6017 V532     1.0
    @6018 V535     1.0
    @6019 V536     1.0
    @6020 V537     2.0
    @6022 V538     1.0
    @6023 V539     1.0
    @6024 V540     1.0
    @6025 V541     1.0
    @6026 V602     1.0
    @6027 V603     3.0
    @6030 V604     1.0
    @6031 V605     1.0
    @6032 V613     2.0
    @6034 V614     1.0
    @6035 V616     3.0
    @6038 V621     1.0
    @6039 V623     1.0
    @6040 V624     2.0
    @6042 V625     1.0
    @6043 V626     2.0
    @6045 V625A    1.0
    @6046 V626A    2.0
    @6048 V627     2.0
    @6050 V628     2.0
    @6052 V629     2.0
    @6054 V631     1.0
    @6055 V632     1.0
    @6056 V632A    1.0
    @6057 V633A    1.0
    @6058 V633B    1.0
    @6059 V633C    1.0
    @6060 V633D    1.0
    @6061 V633E    1.0
    @6062 V633F    1.0
    @6063 V633G    1.0
    @6064 V634     1.0
    @6065 V701     1.0
    @6066 V702     2.0
    @6068 V704     5.0
    @6073 V704A    1.0
    @6074 V705     2.0
    @6076 V714     1.0
    @6077 V714A    1.0
    @6078 V715     2.0
    @6080 V716     5.0
    @6085 V717     2.0
    @6087 V719     1.0
    @6088 V721     1.0
    @6089 V729     1.0
    @6090 V730     2.0
    @6092 V731     1.0
    @6093 V732     1.0
    @6094 V739     1.0
    @6095 V740     1.0
    @6096 V741     1.0
    @6097 V743A    1.0
    @6098 V743B    1.0
    @6099 V743C    1.0
    @6100 V743D    1.0
    @6101 V743E    1.0
    @6102 V743F    1.0
    @6103 V744A    1.0
    @6104 V744B    1.0
    @6105 V744C    1.0
    @6106 V744D    1.0
    @6107 V744E    1.0
    @6108 V745A    1.0
    @6109 V745B    1.0
    @6110 V745C    1.0
    @6111 V745D    1.0
    @6112 V746     1.0
    @6113 V750     1.0
    @6114 V751     1.0
    @6115 V754BP   1.0
    @6116 V754CP   1.0
    @6117 V754DP   1.0
    @6118 V754JP   1.0
    @6119 V754WP   1.0
    @6120 V756     1.0
    @6121 V761     1.0
    @6122 V761B    1.0
    @6123 V761C    1.0
    @6124 V762     2.0
    @6126 V762A    2.0
    @6128 V762AA   1.0
    @6129 V762AB   1.0
    @6130 V762AC   1.0
    @6131 V762AD   1.0
    @6132 V762AE   1.0
    @6133 V762AF   1.0
    @6134 V762AG   1.0
    @6135 V762AH   1.0
    @6136 V762AI   1.0
    @6137 V762AJ   1.0
    @6138 V762AK   1.0
    @6139 V762AL   1.0
    @6140 V762AM   1.0
    @6141 V762AN   1.0
    @6142 V762AO   1.0
    @6143 V762AP   1.0
    @6144 V762AQ   1.0
    @6145 V762AR   1.0
    @6146 V762AS   1.0
    @6147 V762AT   1.0
    @6148 V762AU   1.0
    @6149 V762AV   1.0
    @6150 V762AW   1.0
    @6151 V762AX   1.0
    @6152 V762AZ   1.0
    @6153 V762BA   1.0
    @6154 V762BB   1.0
    @6155 V762BC   1.0
    @6156 V762BD   1.0
    @6157 V762BE   1.0
    @6158 V762BF   1.0
    @6159 V762BG   1.0
    @6160 V762BH   1.0
    @6161 V762BI   1.0
    @6162 V762BJ   1.0
    @6163 V762BK   1.0
    @6164 V762BL   1.0
    @6165 V762BM   1.0
    @6166 V762BN   1.0
    @6167 V762BO   1.0
    @6168 V762BP   1.0
    @6169 V762BQ   1.0
    @6170 V762BR   1.0
    @6171 V762BS   1.0
    @6172 V762BT   1.0
    @6173 V762BU   1.0
    @6174 V762BV   1.0
    @6175 V762BW   1.0
    @6176 V762BX   1.0
    @6177 V762BZ   1.0
    @6178 V763A    1.0
    @6179 V763B    1.0
    @6180 V763C    1.0
    @6181 V763D    1.0
    @6182 V763E    1.0
    @6183 V763F    1.0
    @6184 V763G    1.0
    @6185 V766A    2.0
    @6187 V766B    2.0
    @6189 V767A    2.0
    @6191 V767B    2.0
    @6193 V767C    2.0
    @6195 V768A    3.0
    @6198 V768B    3.0
    @6201 V768C    3.0
    @6204 V769     1.0
    @6205 V769A    1.0
    @6206 V770     1.0
    @6207 V770A    1.0
    @6208 V770B    1.0
    @6209 V770C    1.0
    @6210 V770D    1.0
    @6211 V770E    1.0
    @6212 V770F    1.0
    @6213 V770G    1.0
    @6214 V770H    1.0
    @6215 V770I    1.0
    @6216 V770J    1.0
    @6217 V770K    1.0
    @6218 V770L    1.0
    @6219 V770M    1.0
    @6220 V770N    1.0
    @6221 V770O    1.0
    @6222 V770P    1.0
    @6223 V770Q    1.0
    @6224 V770R    1.0
    @6225 V770S    1.0
    @6226 V770T    1.0
    @6227 V770U    1.0
    @6228 V770V    1.0
    @6229 V770W    1.0
    @6230 V770X    1.0
    @6231 V774A    1.0
    @6232 V774B    1.0
    @6233 V774C    1.0
    @6234 V775     1.0
    @6235 V777     1.0
    @6236 V777A    1.0
    @6237 V778     1.0
    @6238 V779     1.0
    @6239 V780     1.0
    @6240 V781     1.0
    @6241 V783     1.0
    @6242 V784A    1.0
    @6243 V784B    1.0
    @6244 V784C    1.0
    @6245 V784D    1.0
    @6246 V784E    1.0
    @6247 V784F    1.0
    @6248 V784G    1.0
    @6249 V784H    1.0
    @6250 V784I    1.0
    @6251 V784J    1.0
    @6252 V784K    1.0
    @6253 V784L    1.0
    @6254 V784M    1.0
    @6255 V784N    1.0
    @6256 V784O    1.0
    @6257 V784P    1.0
    @6258 V784Q    1.0
    @6259 V784R    1.0
    @6260 V784S    1.0
    @6261 V784T    1.0
    @6262 V784U    1.0
    @6263 V784V    1.0
    @6264 V784X    1.0
    @6265 V785     1.0
    @6266 V791A    1.0
    @6267 V820     1.0
    @6268 V821A    1.0
    @6269 V821B    1.0
    @6270 V821C    1.0
    @6271 V822     1.0
    @6272 V823     1.0
    @6273 V824     1.0
    @6274 V825     1.0
    @6275 V826     1.0
    @6276 V826A    2.0
    @6278 V827     1.0
    @6279 V828     1.0
    @6280 V829     2.0
    @6282 V830     2.0
    @6284 V831     1.0
    @6285 V832B    3.0
    @6288 V832C    3.0
    @6291 V833A    1.0
    @6292 V833B    1.0
    @6293 V833C    1.0
    @6294 V834A    2.0
    @6296 V834B    2.0
    @6298 V834C    2.0
    @6300 V835A    1.0
    @6301 V835B    1.0
    @6302 V835C    1.0
    @6303 V836     2.0
    @6305 V837     1.0
    @6306 V838A    1.0
    @6307 V838B    1.0
    @6308 V838C    1.0
    @6309 V839     1.0
    @6310 V839A    1.0
    @6311 V840     1.0
    @6312 V840A    1.0
    @6313 V841     1.0
    @6314 V841A    1.0
    @6315 V842     2.0
    @6317 V843     1.0
    @6318 V844     1.0
    @6319 V845     1.0
    @6320 V846     1.0
    @6321 V847     1.0
    @6322 V848     1.0
    @6323 V849     1.0
    @6324 V850A    1.0
    @6325 V850B    1.0
    @6326 V851A    1.0
    @6327 V851B    1.0
    @6328 V851C    1.0
    @6329 V851D    1.0
    @6330 V851E    1.0
    @6331 V851F    1.0
    @6332 V851G    1.0
    @6333 V851H    1.0
    @6334 V851I    1.0
    @6335 V851J    1.0
    @6336 V851K    1.0
    @6337 V851L    1.0
    @6338 V852A    3.0
    @6341 V852B    3.0
    @6344 V852C    3.0
    @6347 V853A    2.0
    @6349 V853B    2.0
    @6351 V853C    2.0
    @6353 V854A    1.0
    @6354 V854B    1.0
    @6355 V855     1.0
    @6356 V856     1.0
    @6357 V857A    1.0
    @6358 V857B    1.0
    @6359 V857C    1.0
    @6360 V857D    1.0
    @6361 V858     1.0
    @6362 V801     4.0
    @6366 V802     4.0
    @6370 V803     2.0
    @6372 V804     1.0
    @6373 V805     5.0
    @6378 V806     2.0
    @6380 V811     1.0
    @6381 V812     1.0
    @6382 V813     1.0
    @6383 V814     1.0
    @6384 V815A    1.0
    @6385 V815B    1.0
    @6386 V815C    1.0
    @6387 VCOL_1   1.0
    @6388 VCOL_2   1.0
    @6389 VCOL_3   1.0
    @6390 VCOL_4   1.0
    @6391 VCOL_5   1.0
    @6392 VCOL_6   1.0
    @6393 VCOL_7   1.0
    @6394 VCOL_8   1.0
    @6395 VCOL_9   1.0
    @6396 VCAL_1   $80.
    @6476 VCAL_2   $80.
    @6556 VCAL_3   $80.
    @6636 VCAL_4   $80.
    @6716 VCAL_5   $80.
    @6796 VCAL_6   $80.
    @6876 VCAL_7   $80.
    @6956 VCAL_8   $80.
    @7036 VCAL_9   $80.
    @7116 MMIDX_01 2.0
    @7118 MMIDX_02 2.0
    @7120 MMIDX_03 2.0
    @7122 MMIDX_04 2.0
    @7124 MMIDX_05 2.0
    @7126 MMIDX_06 2.0
    @7128 MMIDX_07 2.0
    @7130 MMIDX_08 2.0
    @7132 MMIDX_09 2.0
    @7134 MMIDX_10 2.0
    @7136 MMIDX_11 2.0
    @7138 MMIDX_12 2.0
    @7140 MMIDX_13 2.0
    @7142 MMIDX_14 2.0
    @7144 MMIDX_15 2.0
    @7146 MMIDX_16 2.0
    @7148 MMIDX_17 2.0
    @7150 MMIDX_18 2.0
    @7152 MMIDX_19 2.0
    @7154 MMIDX_20 2.0
    @7156 MM1_01   1.0
    @7157 MM1_02   1.0
    @7158 MM1_03   1.0
    @7159 MM1_04   1.0
    @7160 MM1_05   1.0
    @7161 MM1_06   1.0
    @7162 MM1_07   1.0
    @7163 MM1_08   1.0
    @7164 MM1_09   1.0
    @7165 MM1_10   1.0
    @7166 MM1_11   1.0
    @7167 MM1_12   1.0
    @7168 MM1_13   1.0
    @7169 MM1_14   1.0
    @7170 MM1_15   1.0
    @7171 MM1_16   1.0
    @7172 MM1_17   1.0
    @7173 MM1_18   1.0
    @7174 MM1_19   1.0
    @7175 MM1_20   1.0
    @7176 MM2_01   1.0
    @7177 MM2_02   1.0
    @7178 MM2_03   1.0
    @7179 MM2_04   1.0
    @7180 MM2_05   1.0
    @7181 MM2_06   1.0
    @7182 MM2_07   1.0
    @7183 MM2_08   1.0
    @7184 MM2_09   1.0
    @7185 MM2_10   1.0
    @7186 MM2_11   1.0
    @7187 MM2_12   1.0
    @7188 MM2_13   1.0
    @7189 MM2_14   1.0
    @7190 MM2_15   1.0
    @7191 MM2_16   1.0
    @7192 MM2_17   1.0
    @7193 MM2_18   1.0
    @7194 MM2_19   1.0
    @7195 MM2_20   1.0
    @7196 MM3_01   2.0
    @7198 MM3_02   2.0
    @7200 MM3_03   2.0
    @7202 MM3_04   2.0
    @7204 MM3_05   2.0
    @7206 MM3_06   2.0
    @7208 MM3_07   2.0
    @7210 MM3_08   2.0
    @7212 MM3_09   2.0
    @7214 MM3_10   2.0
    @7216 MM3_11   2.0
    @7218 MM3_12   2.0
    @7220 MM3_13   2.0
    @7222 MM3_14   2.0
    @7224 MM3_15   2.0
    @7226 MM3_16   2.0
    @7228 MM3_17   2.0
    @7230 MM3_18   2.0
    @7232 MM3_19   2.0
    @7234 MM3_20   2.0
    @7236 MM4_01   4.0
    @7240 MM4_02   4.0
    @7244 MM4_03   4.0
    @7248 MM4_04   4.0
    @7252 MM4_05   4.0
    @7256 MM4_06   4.0
    @7260 MM4_07   4.0
    @7264 MM4_08   4.0
    @7268 MM4_09   4.0
    @7272 MM4_10   4.0
    @7276 MM4_11   4.0
    @7280 MM4_12   4.0
    @7284 MM4_13   4.0
    @7288 MM4_14   4.0
    @7292 MM4_15   4.0
    @7296 MM4_16   4.0
    @7300 MM4_17   4.0
    @7304 MM4_18   4.0
    @7308 MM4_19   4.0
    @7312 MM4_20   4.0
    @7316 MM5_01   1.0
    @7317 MM5_02   1.0
    @7318 MM5_03   1.0
    @7319 MM5_04   1.0
    @7320 MM5_05   1.0
    @7321 MM5_06   1.0
    @7322 MM5_07   1.0
    @7323 MM5_08   1.0
    @7324 MM5_09   1.0
    @7325 MM5_10   1.0
    @7326 MM5_11   1.0
    @7327 MM5_12   1.0
    @7328 MM5_13   1.0
    @7329 MM5_14   1.0
    @7330 MM5_15   1.0
    @7331 MM5_16   1.0
    @7332 MM5_17   1.0
    @7333 MM5_18   1.0
    @7334 MM5_19   1.0
    @7335 MM5_20   1.0
    @7336 MM6_01   2.0
    @7338 MM6_02   2.0
    @7340 MM6_03   2.0
    @7342 MM6_04   2.0
    @7344 MM6_05   2.0
    @7346 MM6_06   2.0
    @7348 MM6_07   2.0
    @7350 MM6_08   2.0
    @7352 MM6_09   2.0
    @7354 MM6_10   2.0
    @7356 MM6_11   2.0
    @7358 MM6_12   2.0
    @7360 MM6_13   2.0
    @7362 MM6_14   2.0
    @7364 MM6_15   2.0
    @7366 MM6_16   2.0
    @7368 MM6_17   2.0
    @7370 MM6_18   2.0
    @7372 MM6_19   2.0
    @7374 MM6_20   2.0
    @7376 MM7_01   2.0
    @7378 MM7_02   2.0
    @7380 MM7_03   2.0
    @7382 MM7_04   2.0
    @7384 MM7_05   2.0
    @7386 MM7_06   2.0
    @7388 MM7_07   2.0
    @7390 MM7_08   2.0
    @7392 MM7_09   2.0
    @7394 MM7_10   2.0
    @7396 MM7_11   2.0
    @7398 MM7_12   2.0
    @7400 MM7_13   2.0
    @7402 MM7_14   2.0
    @7404 MM7_15   2.0
    @7406 MM7_16   2.0
    @7408 MM7_17   2.0
    @7410 MM7_18   2.0
    @7412 MM7_19   2.0
    @7414 MM7_20   2.0
    @7416 MM8_01   4.0
    @7420 MM8_02   4.0
    @7424 MM8_03   4.0
    @7428 MM8_04   4.0
    @7432 MM8_05   4.0
    @7436 MM8_06   4.0
    @7440 MM8_07   4.0
    @7444 MM8_08   4.0
    @7448 MM8_09   4.0
    @7452 MM8_10   4.0
    @7456 MM8_11   4.0
    @7460 MM8_12   4.0
    @7464 MM8_13   4.0
    @7468 MM8_14   4.0
    @7472 MM8_15   4.0
    @7476 MM8_16   4.0
    @7480 MM8_17   4.0
    @7484 MM8_18   4.0
    @7488 MM8_19   4.0
    @7492 MM8_20   4.0
    @7496 MM9_01   2.0
    @7498 MM9_02   2.0
    @7500 MM9_03   2.0
    @7502 MM9_04   2.0
    @7504 MM9_05   2.0
    @7506 MM9_06   2.0
    @7508 MM9_07   2.0
    @7510 MM9_08   2.0
    @7512 MM9_09   2.0
    @7514 MM9_10   2.0
    @7516 MM9_11   2.0
    @7518 MM9_12   2.0
    @7520 MM9_13   2.0
    @7522 MM9_14   2.0
    @7524 MM9_15   2.0
    @7526 MM9_16   2.0
    @7528 MM9_17   2.0
    @7530 MM9_18   2.0
    @7532 MM9_19   2.0
    @7534 MM9_20   2.0
    @7536 MM10_01  1.0
    @7537 MM10_02  1.0
    @7538 MM10_03  1.0
    @7539 MM10_04  1.0
    @7540 MM10_05  1.0
    @7541 MM10_06  1.0
    @7542 MM10_07  1.0
    @7543 MM10_08  1.0
    @7544 MM10_09  1.0
    @7545 MM10_10  1.0
    @7546 MM10_11  1.0
    @7547 MM10_12  1.0
    @7548 MM10_13  1.0
    @7549 MM10_14  1.0
    @7550 MM10_15  1.0
    @7551 MM10_16  1.0
    @7552 MM10_17  1.0
    @7553 MM10_18  1.0
    @7554 MM10_19  1.0
    @7555 MM10_20  1.0
    @7556 MM11_01  2.0
    @7558 MM11_02  2.0
    @7560 MM11_03  2.0
    @7562 MM11_04  2.0
    @7564 MM11_05  2.0
    @7566 MM11_06  2.0
    @7568 MM11_07  2.0
    @7570 MM11_08  2.0
    @7572 MM11_09  2.0
    @7574 MM11_10  2.0
    @7576 MM11_11  2.0
    @7578 MM11_12  2.0
    @7580 MM11_13  2.0
    @7582 MM11_14  2.0
    @7584 MM11_15  2.0
    @7586 MM11_16  2.0
    @7588 MM11_17  2.0
    @7590 MM11_18  2.0
    @7592 MM11_19  2.0
    @7594 MM11_20  2.0
    @7596 MM12_01  3.0
    @7599 MM12_02  3.0
    @7602 MM12_03  3.0
    @7605 MM12_04  3.0
    @7608 MM12_05  3.0
    @7611 MM12_06  3.0
    @7614 MM12_07  3.0
    @7617 MM12_08  3.0
    @7620 MM12_09  3.0
    @7623 MM12_10  3.0
    @7626 MM12_11  3.0
    @7629 MM12_12  3.0
    @7632 MM12_13  3.0
    @7635 MM12_14  3.0
    @7638 MM12_15  3.0
    @7641 MM12_16  3.0
    @7644 MM12_17  3.0
    @7647 MM12_18  3.0
    @7650 MM12_19  3.0
    @7653 MM12_20  3.0
    @7656 MM13_01  2.0
    @7658 MM13_02  2.0
    @7660 MM13_03  2.0
    @7662 MM13_04  2.0
    @7664 MM13_05  2.0
    @7666 MM13_06  2.0
    @7668 MM13_07  2.0
    @7670 MM13_08  2.0
    @7672 MM13_09  2.0
    @7674 MM13_10  2.0
    @7676 MM13_11  2.0
    @7678 MM13_12  2.0
    @7680 MM13_13  2.0
    @7682 MM13_14  2.0
    @7684 MM13_15  2.0
    @7686 MM13_16  2.0
    @7688 MM13_17  2.0
    @7690 MM13_18  2.0
    @7692 MM13_19  2.0
    @7694 MM13_20  2.0
    @7696 MM14_01  2.0
    @7698 MM14_02  2.0
    @7700 MM14_03  2.0
    @7702 MM14_04  2.0
    @7704 MM14_05  2.0
    @7706 MM14_06  2.0
    @7708 MM14_07  2.0
    @7710 MM14_08  2.0
    @7712 MM14_09  2.0
    @7714 MM14_10  2.0
    @7716 MM14_11  2.0
    @7718 MM14_12  2.0
    @7720 MM14_13  2.0
    @7722 MM14_14  2.0
    @7724 MM14_15  2.0
    @7726 MM14_16  2.0
    @7728 MM14_17  2.0
    @7730 MM14_18  2.0
    @7732 MM14_19  2.0
    @7734 MM14_20  2.0
    @7736 MM15_01  4.0
    @7740 MM15_02  4.0
    @7744 MM15_03  4.0
    @7748 MM15_04  4.0
    @7752 MM15_05  4.0
    @7756 MM15_06  4.0
    @7760 MM15_07  4.0
    @7764 MM15_08  4.0
    @7768 MM15_09  4.0
    @7772 MM15_10  4.0
    @7776 MM15_11  4.0
    @7780 MM15_12  4.0
    @7784 MM15_13  4.0
    @7788 MM15_14  4.0
    @7792 MM15_15  4.0
    @7796 MM15_16  4.0
    @7800 MM15_17  4.0
    @7804 MM15_18  4.0
    @7808 MM15_19  4.0
    @7812 MM15_20  4.0
    @7816 MM16_01  1.0
    @7817 MM16_02  1.0
    @7818 MM16_03  1.0
    @7819 MM16_04  1.0
    @7820 MM16_05  1.0
    @7821 MM16_06  1.0
    @7822 MM16_07  1.0
    @7823 MM16_08  1.0
    @7824 MM16_09  1.0
    @7825 MM16_10  1.0
    @7826 MM16_11  1.0
    @7827 MM16_12  1.0
    @7828 MM16_13  1.0
    @7829 MM16_14  1.0
    @7830 MM16_15  1.0
    @7831 MM16_16  1.0
    @7832 MM16_17  1.0
    @7833 MM16_18  1.0
    @7834 MM16_19  1.0
    @7835 MM16_20  1.0
    @7836 MMC1     2.0
    @7838 MMC2     2.0
    @7840 MMC3     2.0
    @7842 MMC4_1   2.0
    @7844 MMC4_2   2.0
    @7846 MMC4_3   2.0
    @7848 MMC4_4   2.0
    @7850 MMC4_5   2.0
    @7852 MMC4_6   2.0
    @7854 MMC5     2.0
    @7856 D005     8.0
    @7864 D101A    1.0
    @7865 D101B    1.0
    @7866 D101C    1.0
    @7867 D101D    1.0
    @7868 D101E    1.0
    @7869 D101F    1.0
    @7870 D101G    1.0
    @7871 D101H    1.0
    @7872 D101I    1.0
    @7873 D101J    1.0
    @7874 D102     2.0
    @7876 D103A    1.0
    @7877 D103B    1.0
    @7878 D103C    1.0
    @7879 D103D    1.0
    @7880 D103E    1.0
    @7881 D103F    1.0
    @7882 D104     1.0
    @7883 D105A    1.0
    @7884 D105B    1.0
    @7885 D105C    1.0
    @7886 D105D    1.0
    @7887 D105E    1.0
    @7888 D105F    1.0
    @7889 D105G    1.0
    @7890 D105H    1.0
    @7891 D105I    1.0
    @7892 D105J    1.0
    @7893 D105K    1.0
    @7894 D105L    1.0
    @7895 D105M    1.0
    @7896 D105N    1.0
    @7897 D106     1.0
    @7898 D107     1.0
    @7899 D108     1.0
    @7900 D109     2.0
    @7902 D110A    1.0
    @7903 D110B    1.0
    @7904 D110C    1.0
    @7905 D110D    1.0
    @7906 D110E    1.0
    @7907 D110F    1.0
    @7908 D110G    1.0
    @7909 D110H    1.0
    @7910 D111     1.0
    @7911 D112     1.0
    @7912 D112A    1.0
    @7913 D113     1.0
    @7914 D114     1.0
    @7915 D115B    1.0
    @7916 D115C    1.0
    @7917 D115D    1.0
    @7918 D115E    1.0
    @7919 D115F    1.0
    @7920 D115G    1.0
    @7921 D115H    1.0
    @7922 D115I    1.0
    @7923 D115J    1.0
    @7924 D115K    1.0
    @7925 D115L    1.0
    @7926 D115M    1.0
    @7927 D115N    1.0
    @7928 D115O    1.0
    @7929 D115P    1.0
    @7930 D115Q    1.0
    @7931 D115R    1.0
    @7932 D115S    1.0
    @7933 D115T    1.0
    @7934 D115U    1.0
    @7935 D115V    1.0
    @7936 D115W    1.0
    @7937 D115X    1.0
    @7938 D115Y    1.0
    @7939 D115XA   1.0
    @7940 D115XB   1.0
    @7941 D115XC   1.0
    @7942 D115XD   1.0
    @7943 D115XE   1.0
    @7944 D115XF   1.0
    @7945 D115XG   1.0
    @7946 D115XH   1.0
    @7947 D115XI   1.0
    @7948 D115XJ   1.0
    @7949 D115XK   1.0
    @7950 D116     2.0
    @7952 D117A    1.0
    @7953 D118A    1.0
    @7954 D118B    1.0
    @7955 D118C    1.0
    @7956 D118D    1.0
    @7957 D118E    1.0
    @7958 D118F    1.0
    @7959 D118G    1.0
    @7960 D118H    1.0
    @7961 D118I    1.0
    @7962 D118J    1.0
    @7963 D118K    1.0
    @7964 D118L    1.0
    @7965 D118M    1.0
    @7966 D118N    1.0
    @7967 D118O    1.0
    @7968 D118P    1.0
    @7969 D118Q    1.0
    @7970 D118R    1.0
    @7971 D118S    1.0
    @7972 D118T    1.0
    @7973 D118U    1.0
    @7974 D118V    1.0
    @7975 D118W    1.0
    @7976 D118X    1.0
    @7977 D118Y    1.0
    @7978 D118XA   1.0
    @7979 D118XB   1.0
    @7980 D118XC   1.0
    @7981 D118XD   1.0
    @7982 D118XE   1.0
    @7983 D118XF   1.0
    @7984 D118XG   1.0
    @7985 D118XH   1.0
    @7986 D118XI   1.0
    @7987 D118XJ   1.0
    @7988 D118XK   1.0
    @7989 D119A    1.0
    @7990 D119B    1.0
    @7991 D119C    1.0
    @7992 D119D    1.0
    @7993 D119E    1.0
    @7994 D119F    1.0
    @7995 D119G    1.0
    @7996 D119H    1.0
    @7997 D119I    1.0
    @7998 D119J    1.0
    @7999 D119K    1.0
    @8000 D119L    1.0
    @8001 D119M    1.0
    @8002 D119N    1.0
    @8003 D119O    1.0
    @8004 D119P    1.0
    @8005 D119Q    1.0
    @8006 D119R    1.0
    @8007 D119S    1.0
    @8008 D119T    1.0
    @8009 D119U    1.0
    @8010 D119V    1.0
    @8011 D119W    1.0
    @8012 D119X    1.0
    @8013 D119Y    1.0
    @8014 D119XA   1.0
    @8015 D119XB   1.0
    @8016 D119XC   1.0
    @8017 D119XD   1.0
    @8018 D119XE   1.0
    @8019 D119XF   1.0
    @8020 D119XG   1.0
    @8021 D119XH   1.0
    @8022 D119XI   1.0
    @8023 D119XJ   1.0
    @8024 D119XK   1.0
    @8025 D120     2.0
    @8027 D121     1.0
    @8028 D122A    1.0
    @8029 D122B    1.0
    @8030 D122C    1.0
    @8031 D123     1.0
    @8032 D124     1.0
    @8033 D125     1.0
    @8034 D126     2.0
    @8036 D127     2.0
    @8038 D128     1.0
    @8039 D129     1.0
    @8040 D130A    1.0
    @8041 D130B    1.0
    @8042 D130C    1.0
    @8043 SDEPTO   2.0
    @8045 SMUNIP   3.0
    @8048 SAREAM   4.0
    @8052 SAREA    1.0
    @8053 SSUBREG  2.0
    @8055 S101A    1.0
    @8056 S102     3.0
    @8059 S106A    2.0
    @8061 S106B    2.0
    @8063 S109     1.0
    @8064 S107     1.0
    @8065 S110     2.0
    @8067 S111     2.0
    @8069 S112     1.0
    @8070 S113     1.0
    @8071 S115     1.0
    @8072 S116     1.0
    @8073 S117     1.0
    @8074 S118     1.0
    @8075 S119     2.0
    @8077 S120     1.0
    @8078 S121     1.0
    @8079 S122     2.0
    @8081 S123     2.0
    @8083 S124     1.0
    @8084 S125A    1.0
    @8085 S125B    1.0
    @8086 S125C    1.0
    @8087 S125D    1.0
    @8088 S126A    1.0
    @8089 S126B    1.0
    @8090 S126C    1.0
    @8091 S206A    2.0
    @8093 S206B    2.0
    @8095 S206C    2.0
    @8097 S206D    2.0
    @8099 S207A    2.0
    @8101 S207B    2.0
    @8103 S207C    2.0
    @8105 S207D    2.0
    @8107 S207E    2.0
    @8109 S207F    2.0
    @8111 S207G    2.0
    @8113 S207H    2.0
    @8115 S207I    2.0
    @8117 S207J    2.0
    @8119 S207M    2.0
    @8121 S207N    2.0
    @8123 S213     1.0
    @8124 S225A    2.0
    @8126 S225B    2.0
    @8128 S233     1.0
    @8129 S234     1.0
    @8130 S235     2.0
    @8132 S236     2.0
    @8134 S238A    1.0
    @8135 S238B    1.0
    @8136 S238C    1.0
    @8137 S238D    1.0
    @8138 S238E    1.0
    @8139 S238F    1.0
    @8140 S238G    1.0
    @8141 S238H    1.0
    @8142 S238X    1.0
    @8143 S239     3.0
    @8146 S240A    1.0
    @8147 S240B    1.0
    @8148 S240C    1.0
    @8149 S240D    1.0
    @8150 S240E    1.0
    @8151 S240F    1.0
    @8152 S240G    1.0
    @8153 S240X    1.0
    @8154 S242     2.0
    @8156 S243_1   1.0
    @8157 S243_2   1.0
    @8158 S243_3   1.0
    @8159 S243_4   1.0
    @8160 S243_5   1.0
    @8161 S243_6   1.0
    @8162 S243_7   1.0
    @8163 S243_8   1.0
    @8164 S244M_1  2.0
    @8166 S244M_2  2.0
    @8168 S244M_3  2.0
    @8170 S244M_4  2.0
    @8172 S244M_5  2.0
    @8174 S244M_6  2.0
    @8176 S244M_7  2.0
    @8178 S244M_8  2.0
    @8180 S244Y_1  4.0
    @8184 S244Y_2  4.0
    @8188 S244Y_3  4.0
    @8192 S244Y_4  4.0
    @8196 S244Y_5  4.0
    @8200 S244Y_6  4.0
    @8204 S244Y_7  4.0
    @8208 S244Y_8  4.0
    @8212 S245_1   2.0
    @8214 S245_2   2.0
    @8216 S245_3   2.0
    @8218 S245_4   2.0
    @8220 S245_5   2.0
    @8222 S245_6   2.0
    @8224 S245_7   2.0
    @8226 S245_8   2.0
    @8228 S246_1   1.0
    @8229 S246_2   1.0
    @8230 S246_3   1.0
    @8231 S246_4   1.0
    @8232 S246_5   1.0
    @8233 S246_6   1.0
    @8234 S246_7   1.0
    @8235 S246_8   1.0
    @8236 S247_1   2.0
    @8238 S247_2   2.0
    @8240 S247_3   2.0
    @8242 S247_4   2.0
    @8244 S247_5   2.0
    @8246 S247_6   2.0
    @8248 S247_7   2.0
    @8250 S247_8   2.0
    @8252 S251     2.0
    @8254 S252     2.0
    @8256 S302_01  2.0
    @8258 S302_02  2.0
    @8260 S302_03  2.0
    @8262 S302_04  2.0
    @8264 S302_05  2.0
    @8266 S302_06  2.0
    @8268 S302_07  2.0
    @8270 S302_08  2.0
    @8272 S302_09  2.0
    @8274 S302_10  2.0
    @8276 S302_11  2.0
    @8278 S302_12  2.0
    @8280 S302_13  2.0
    @8282 S302_14  2.0
    @8284 S302_15  2.0
    @8286 S302_16  2.0
    @8288 S302_17  2.0
    @8290 S302_18  2.0
    @8292 S302_19  2.0
    @8294 S302_20  2.0
    @8296 S313     1.0
    @8297 S316     1.0
    @8298 S317     1.0
    @8299 S332     1.0
    @8300 S333A    1.0
    @8301 S333B    1.0
    @8302 S333C    1.0
    @8303 S333D    1.0
    @8304 S333E    1.0
    @8305 S333F    1.0
    @8306 S333X    1.0
    @8307 S334     1.0
    @8308 S600     1.0
    @8309 S606     2.0
    @8311 S607N_1  1.0
    @8312 S607N_2  1.0
    @8313 S607N_3  1.0
    @8314 S607N_4  1.0
    @8315 S607N_5  1.0
    @8316 S607M_1  2.0
    @8318 S607M_2  2.0
    @8320 S607M_3  2.0
    @8322 S607M_4  2.0
    @8324 S607M_5  2.0
    @8326 S607Y_1  4.0
    @8330 S607Y_2  4.0
    @8334 S607Y_3  4.0
    @8338 S607Y_4  4.0
    @8342 S607Y_5  4.0
    @8346 S607A_1  2.0
    @8348 S607A_2  2.0
    @8350 S607A_3  2.0
    @8352 S607A_4  2.0
    @8354 S607A_5  2.0
    @8356 S608_1   1.0
    @8357 S608_2   1.0
    @8358 S608_3   1.0
    @8359 S608_4   1.0
    @8360 S608_5   1.0
    @8361 S609_1   2.0
    @8363 S609_2   2.0
    @8365 S609_3   2.0
    @8367 S609_4   2.0
    @8369 S609_5   2.0
    @8371 S610_1   1.0
    @8372 S610_2   1.0
    @8373 S610_3   1.0
    @8374 S610_4   1.0
    @8375 S610_5   1.0
    @8376 S611_1   1.0
    @8377 S611_2   1.0
    @8378 S611_3   1.0
    @8379 S611_4   1.0
    @8380 S611_5   1.0
    @8381 S612M_1  2.0
    @8383 S612M_2  2.0
    @8385 S612M_3  2.0
    @8387 S612M_4  2.0
    @8389 S612M_5  2.0
    @8391 S612Y_1  4.0
    @8395 S612Y_2  4.0
    @8399 S612Y_3  4.0
    @8403 S612Y_4  4.0
    @8407 S612Y_5  4.0
    @8411 S613_1   1.0
    @8412 S613_2   1.0
    @8413 S613_3   1.0
    @8414 S613_4   1.0
    @8415 S613_5   1.0
    @8416 S614M_1  2.0
    @8418 S614M_2  2.0
    @8420 S614M_3  2.0
    @8422 S614M_4  2.0
    @8424 S614M_5  2.0
    @8426 S614Y_1  4.0
    @8430 S614Y_2  4.0
    @8434 S614Y_3  4.0
    @8438 S614Y_4  4.0
    @8442 S614Y_5  4.0
    @8446 S615_1   1.0
    @8447 S615_2   1.0
    @8448 S615_3   1.0
    @8449 S615_4   1.0
    @8450 S615_5   1.0
    @8451 S616A    1.0
    @8452 S617     1.0
    @8453 S617A    1.0
    @8454 S618     1.0
    @8455 S622A    1.0
    @8456 S622B    1.0
    @8457 S628     1.0
    @8458 S631     1.0
    @8459 S632     1.0
    @8460 S638A    2.0
    @8462 S638B    2.0
    @8464 S638C    2.0
    @8466 S652     1.0
    @8467 S653A    1.0
    @8468 S653B    1.0
    @8469 S653C    1.0
    @8470 S653D    1.0
    @8471 S653E    1.0
    @8472 S653F    1.0
    @8473 S653G    1.0
    @8474 S654A    1.0
    @8475 S654B    1.0
    @8476 S654C    1.0
    @8477 S654D    1.0
    @8478 S654E    1.0
    @8479 S654F    1.0
    @8480 S654G    1.0
    @8481 S654H    1.0
    @8482 S654I    1.0
    @8483 S654J    1.0
    @8484 S654K    1.0
    @8485 S654L    1.0
    @8486 S655     1.0
    @8487 S656     1.0
    @8488 S657     1.0
    @8489 S708     1.0
    @8490 S709     1.0
    @8491 S710     1.0
    @8492 S711     1.0
    @8493 S712     1.0
    @8494 S713     2.0
    @8496 S722     1.0
    @8497 S723A    1.0
    @8498 S805     1.0
    @8499 S813     1.0
    @8500 S814F    1.0
    @8501 S814G    1.0
    @8502 S815B    1.0
    @8503 S815C    1.0
    @8504 S815D    1.0
    @8505 S815E    1.0
    @8506 S1008    1.0
    @8507 S1010    1.0
    @8508 S1011A   1.0
    @8509 S1011B   1.0
    @8510 S1011C   1.0
    @8511 S1011D   1.0
    @8512 S1021A   1.0
    @8513 S1022A   1.0
    @8514 S1026A   1.0
    @8515 S1027A   1.0
    @8516 S1032    1.0
    @8517 S1038    1.0
    @8518 S1040A   1.0
    @8519 S1040B   1.0
    @8520 S1040C   1.0
    @8521 S1040D   1.0
    @8522 S1040E   1.0
    @8523 S1040F   1.0
    @8524 S1040G   1.0
    @8525 S1040H   1.0
    @8526 S1040I   1.0
    @8527 S1040J   1.0
    @8528 S1040K   1.0
    @8529 S1040L   1.0
    @8530 S1040X   1.0
    @8531 S1040Z   1.0
    @8532 S1041A   1.0
    @8533 S1041B   1.0
    @8534 S1041C   1.0
    @8535 S1041D   1.0
    @8536 S1041E   1.0
    @8537 S1041F   1.0
    @8538 S1041G   1.0
    @8539 S1041H   1.0
    @8540 S1041I   1.0
    @8541 S1041J   1.0
    @8542 S1041K   1.0
    @8543 S1041L   1.0
    @8544 S1041X   1.0
    @8545 S1041Z   1.0
    @8546 S1050    1.0
    @8547 S1051    1.0
    @8548 S1052A   1.0
    @8549 S1052B   1.0
    @8550 S1052C   1.0
    @8551 S1055    1.0
    @8552 S1203CA  1.0
    @8553 S1203CB  1.0
    @8554 S1203CC  1.0
    @8555 S1203CD  1.0
    @8556 S1203CE  1.0
    @8557 S1203CF  1.0
    @8558 S1203CI  1.0
    @8559 S1203CJ  1.0
    @8560 S1203CK  1.0
    @8561 S1203CL  1.0
    @8562 S1204CA  1.0
    @8563 S1204CB  1.0
    @8564 S1204CC  1.0
    @8565 S1204CD  1.0
    @8566 S1204CE  1.0
    @8567 S1204CF  1.0
    @8568 S1205B   1.0
    @8569 S1205C   1.0
    @8570 S1205D   1.0
    @8571 S1205E   1.0
    @8572 S1205CA  1.0
    @8573 S1205CB  1.0
    @8574 S1205CC  1.0
    @8575 S1206CA  1.0
    @8576 S1207    1.0
    @8577 S1208    1.0
    @8578 S1209    1.0
    @8579 S1210    1.0
    @8580 S1211    1.0
    @8581 S1212    1.0
    @8582 S1215G   1.0
    @8583 S1215H   1.0
    @8584 S1215I   1.0
    @8585 S1215J   1.0
    @8586 S1215K   1.0
    @8587 S1215L   1.0
    @8588 S1215M   1.0
    @8589 S1215N   1.0
    @8590 S1217    1.0
    @8591 S1218    2.0
    @8593 S1220    1.0
    @8594 S1221A   1.0
    @8595 S1221B   1.0
    @8596 S1221C   1.0
    @8597 S1221D   1.0
    @8598 S1221X   1.0
    @8599 S1221Z   1.0
    @8600 S1222    1.0
    @8601 S1224A   1.0
    @8602 S1224B   1.0
    @8603 S1224C   1.0
    @8604 S1224D   1.0
    @8605 S1224E   1.0
    @8606 S1224F   1.0
    @8607 S1224G   1.0
    @8608 S1224H   1.0
    @8609 S1224I   1.0
    @8610 S1224J   1.0
    @8611 S1224K   1.0
    @8612 S1224L   1.0
    @8613 S1224M   1.0
    @8614 S1224N   1.0
    @8615 S1224X   1.0
    @8616 S1224Z   1.0
    @8617 S1225    1.0
    @8618 S1226    1.0
    @8619 S1226A   1.0
    @8620 S1228A   1.0
    @8621 S1228B   1.0
    @8622 S1228X   1.0
    @8623 S1228Y   1.0
    @8624 S1230AA  1.0
    @8625 S1230AB  1.0
    @8626 S1230AC  1.0
    @8627 S1230AD  1.0
    @8628 S1230AE  1.0
    @8629 S1230AF  1.0
    @8630 S1230AG  1.0
    @8631 S1230AH  1.0
    @8632 S1230AI  1.0
    @8633 S1230AJ  1.0
    @8634 S1230AK  1.0
    @8635 S1230AL  1.0
    @8636 S1230AM  1.0
    @8637 S1230AN  1.0
    @8638 S1230AX  1.0
    @8639 S1230BA  1.0
    @8640 S1230BB  1.0
    @8641 S1230BC  1.0
    @8642 S1230BD  1.0
    @8643 S1230BE  1.0
    @8644 S1230BF  1.0
    @8645 S1230BG  1.0
    @8646 S1230BH  1.0
    @8647 S1230BI  1.0
    @8648 S1230BJ  1.0
    @8649 S1230BK  1.0
    @8650 S1230BL  1.0
    @8651 S1230BM  1.0
    @8652 S1230BN  1.0
    @8653 S1230BX  1.0
    @8654 S1230CA  1.0
    @8655 S1230CB  1.0
    @8656 S1230CC  1.0
    @8657 S1230CD  1.0
    @8658 S1230CE  1.0
    @8659 S1230CF  1.0
    @8660 S1230CG  1.0
    @8661 S1230CH  1.0
    @8662 S1230CI  1.0
    @8663 S1230CJ  1.0
    @8664 S1230CK  1.0
    @8665 S1230CL  1.0
    @8666 S1230CM  1.0
    @8667 S1230CN  1.0
    @8668 S1230CX  1.0
    @8669 S1234    1.0
    @8670 S1235    1.0
    @8671 S1236A   1.0
    @8672 S1236B   1.0
    @8673 S1236C   1.0
    @8674 S1236D   1.0
    @8675 S1236E   1.0
    @8676 S1236F   1.0
    @8677 S1236G   1.0
    @8678 S1236X   1.0
    @8679 S1237    1.0
    @8680 S1238A   1.0
    @8681 S1238B   1.0
    @8682 S1238C   1.0
    @8683 S1238D   1.0
    @8684 S1238E   1.0
    @8685 S1238F   1.0
    @8686 S1238G   1.0
    @8687 S1238X   1.0
    @8688 S1239    1.0
    @8689 S1240A   1.0
    @8690 S1240B   1.0
    @8691 S1240C   1.0
    @8692 S1240D   1.0
    @8693 S1240E   1.0
    @8694 S1240F   1.0
    @8695 S1240G   1.0
    @8696 S1240H   1.0
    @8697 S1240X   1.0
    @8698 S1243    1.0
    @8699 S1245    1.0
    @8700 S1246A   1.0
    @8701 S1246B   1.0
    @8702 S1246C   1.0
    @8703 S1246D   1.0
    @8704 S1246E   1.0
    @8705 S1246F   1.0
    @8706 S1246G   1.0
    @8707 S1246H   1.0
    @8708 S1246I   1.0
    @8709 S1246J   1.0
    @8710 S1246K   1.0
    @8711 S1246L   1.0
    @8712 S1246M   1.0
    @8713 S1246X   1.0
    @8714 S1247    1.0
    @8715 S1248    1.0
    @8716 S1249    1.0
    @8717 S1250    1.0
    @8718 S1251    1.0
    @8719 S1252    1.0
    @8720 S1254    1.0
    @8721 S1255A   1.0
    @8722 S1255B   1.0
    @8723 S1255C   1.0
    @8724 S1255D   1.0
    @8725 S1255E   1.0
    @8726 S1255F   1.0
    @8727 S1255G   1.0
    @8728 S1255H   1.0
    @8729 S1255I   1.0
    @8730 S1255X   1.0
    @8731 S1256    2.0
    @8733 S1257    1.0
    @8734 S1258A   1.0
    @8735 S1258B   1.0
    @8736 S1258C   1.0
    @8737 S1258D   1.0
    @8738 S1258E   1.0
    @8739 S1258F   1.0
    @8740 S1258G   1.0
    @8741 S1258H   1.0
    @8742 S1258X   1.0
    @8743 S1259    1.0
    @8744 S1260    1.0
    @8745 S1261    1.0
    @8746 S1262    1.0
    @8747 S1263    1.0
    @8748 S1267A   1.0
    @8749 S1267B   1.0
    @8750 S1267C   1.0
    @8751 S1267D   1.0
    @8752 S1267E   1.0
    @8753 S1267F   1.0
    @8754 S1267G   1.0
    @8755 S1267H   1.0
    @8756 S1267I   1.0
    @8757 S1267J   1.0
    @8758 S1267X   1.0
    @8759 S1267Z   1.0
    @8760 S1268    1.0
    @8761 S1269A   1.0
    @8762 S1269B   1.0
    @8763 S1269C   1.0
    @8764 S1269D   1.0
    @8765 S1269X   1.0
    @8766 S1270A   1.0
    @8767 S1270B   1.0
    @8768 S1270C   1.0
    @8769 S1270D   1.0
    @8770 S1270E   1.0
    @8771 S1270F   1.0
    @8772 S1270G   1.0
    @8773 S1270X   1.0
    @8774 S1271A   1.0
    @8775 S1271B   1.0
    @8776 S1271C   1.0
    @8777 S1271D   1.0
    @8778 S1271E   1.0
    @8779 S1271F   1.0
    @8780 S1271G   1.0
    @8781 S1271H   1.0
    @8782 S1271I   1.0
    @8783 S1271J   1.0
    @8784 S1271K   1.0
    @8785 S1271L   1.0
    @8786 S1271M   1.0
    @8787 S1271X   1.0
    @8788 S1272    1.0
    @8789 S1273    1.0
    @8790 S1274A   1.0
    @8791 S1274B   1.0
    @8792 S1274C   1.0
    @8793 S1274D   1.0
    @8794 S1274E   1.0
    @8795 S1274F   1.0
    @8796 S1274G   1.0
    @8797 S1274H   1.0
    @8798 S1274I   1.0
    @8799 S1276    1.0
    @8800 S1277    1.0
    @8801 S1278A   1.0
    @8802 S1278B   1.0
    @8803 S1278C   1.0
    @8804 S1278D   1.0
    @8805 S1278E   1.0
    @8806 S1279A   1.0
    @8807 S1279B   1.0
    @8808 S1279C   1.0
    @8809 S1279D   1.0
    @8810 S1279E   1.0
    @8811 IDX94_1  1.0
    @8812 IDX94_2  1.0
    @8813 IDX94_3  1.0
    @8814 IDX94_4  1.0
    @8815 IDX94_5  1.0
    @8816 IDX94_6  1.0
    @8817 S412_1   2.0
    @8819 S412_2   2.0
    @8821 S412_3   2.0
    @8823 S412_4   2.0
    @8825 S412_5   2.0
    @8827 S412_6   2.0
    @8829 S413B_1  1.0
    @8830 S413B_2  1.0
    @8831 S413B_3  1.0
    @8832 S413B_4  1.0
    @8833 S413B_5  1.0
    @8834 S413B_6  1.0
    @8835 S413F_1  1.0
    @8836 S413F_2  1.0
    @8837 S413F_3  1.0
    @8838 S413F_4  1.0
    @8839 S413F_5  1.0
    @8840 S413F_6  1.0
    @8841 S414A_1  1.0
    @8842 S414A_2  1.0
    @8843 S414A_3  1.0
    @8844 S414A_4  1.0
    @8845 S414A_5  1.0
    @8846 S414A_6  1.0
    @8847 S414B_1  1.0
    @8848 S414B_2  1.0
    @8849 S414B_3  1.0
    @8850 S414B_4  1.0
    @8851 S414B_5  1.0
    @8852 S414B_6  1.0
    @8853 S416A_1  1.0
    @8854 S416A_2  1.0
    @8855 S416A_3  1.0
    @8856 S416A_4  1.0
    @8857 S416A_5  1.0
    @8858 S416A_6  1.0
    @8859 S416B_1  1.0
    @8860 S416B_2  1.0
    @8861 S416B_3  1.0
    @8862 S416B_4  1.0
    @8863 S416B_5  1.0
    @8864 S416B_6  1.0
    @8865 S416C_1  1.0
    @8866 S416C_2  1.0
    @8867 S416C_3  1.0
    @8868 S416C_4  1.0
    @8869 S416C_5  1.0
    @8870 S416C_6  1.0
    @8871 S416D_1  1.0
    @8872 S416D_2  1.0
    @8873 S416D_3  1.0
    @8874 S416D_4  1.0
    @8875 S416D_5  1.0
    @8876 S416D_6  1.0
    @8877 S416E_1  1.0
    @8878 S416E_2  1.0
    @8879 S416E_3  1.0
    @8880 S416E_4  1.0
    @8881 S416E_5  1.0
    @8882 S416E_6  1.0
    @8883 S416F_1  1.0
    @8884 S416F_2  1.0
    @8885 S416F_3  1.0
    @8886 S416F_4  1.0
    @8887 S416F_5  1.0
    @8888 S416F_6  1.0
    @8889 S416G_1  1.0
    @8890 S416G_2  1.0
    @8891 S416G_3  1.0
    @8892 S416G_4  1.0
    @8893 S416G_5  1.0
    @8894 S416G_6  1.0
    @8895 S416H_1  1.0
    @8896 S416H_2  1.0
    @8897 S416H_3  1.0
    @8898 S416H_4  1.0
    @8899 S416H_5  1.0
    @8900 S416H_6  1.0
    @8901 S416X_1  1.0
    @8902 S416X_2  1.0
    @8903 S416X_3  1.0
    @8904 S416X_4  1.0
    @8905 S416X_5  1.0
    @8906 S416X_6  1.0
    @8907 S420_1   1.0
    @8908 S420_2   1.0
    @8909 S420_3   1.0
    @8910 S420_4   1.0
    @8911 S420_5   1.0
    @8912 S420_6   1.0
    @8913 S421_1   2.0
    @8915 S421_2   2.0
    @8917 S421_3   2.0
    @8919 S421_4   2.0
    @8921 S421_5   2.0
    @8923 S421_6   2.0
    @8925 S422_1   1.0
    @8926 S422_2   1.0
    @8927 S422_3   1.0
    @8928 S422_4   1.0
    @8929 S422_5   1.0
    @8930 S422_6   1.0
    @8931 S423_1   1.0
    @8932 S423_2   1.0
    @8933 S423_3   1.0
    @8934 S423_4   1.0
    @8935 S423_5   1.0
    @8936 S423_6   1.0
    @8937 S424_1   2.0
    @8939 S424_2   2.0
    @8941 S424_3   2.0
    @8943 S424_4   2.0
    @8945 S424_5   2.0
    @8947 S424_6   2.0
    @8949 S425_1   1.0
    @8950 S425_2   1.0
    @8951 S425_3   1.0
    @8952 S425_4   1.0
    @8953 S425_5   1.0
    @8954 S425_6   1.0
    @8955 S426_1   1.0
    @8956 S426_2   1.0
    @8957 S426_3   1.0
    @8958 S426_4   1.0
    @8959 S426_5   1.0
    @8960 S426_6   1.0
    @8961 S427_1   2.0
    @8963 S427_2   2.0
    @8965 S427_3   2.0
    @8967 S427_4   2.0
    @8969 S427_5   2.0
    @8971 S427_6   2.0
    @8973 S428_1   1.0
    @8974 S428_2   1.0
    @8975 S428_3   1.0
    @8976 S428_4   1.0
    @8977 S428_5   1.0
    @8978 S428_6   1.0
    @8979 S429_1   2.0
    @8981 S429_2   2.0
    @8983 S429_3   2.0
    @8985 S429_4   2.0
    @8987 S429_5   2.0
    @8989 S429_6   2.0
    @8991 S430_1   1.0
    @8992 S430_2   1.0
    @8993 S430_3   1.0
    @8994 S430_4   1.0
    @8995 S430_5   1.0
    @8996 S430_6   1.0
    @8997 S431_1   2.0
    @8999 S431_2   2.0
    @9001 S431_3   2.0
    @9003 S431_4   2.0
    @9005 S431_5   2.0
    @9007 S431_6   2.0
    @9009 S433A_1  1.0
    @9010 S433A_2  1.0
    @9011 S433A_3  1.0
    @9012 S433A_4  1.0
    @9013 S433A_5  1.0
    @9014 S433A_6  1.0
    @9015 S433B_1  1.0
    @9016 S433B_2  1.0
    @9017 S433B_3  1.0
    @9018 S433B_4  1.0
    @9019 S433B_5  1.0
    @9020 S433B_6  1.0
    @9021 S433C_1  1.0
    @9022 S433C_2  1.0
    @9023 S433C_3  1.0
    @9024 S433C_4  1.0
    @9025 S433C_5  1.0
    @9026 S433C_6  1.0
    @9027 S433X_1  1.0
    @9028 S433X_2  1.0
    @9029 S433X_3  1.0
    @9030 S433X_4  1.0
    @9031 S433X_5  1.0
    @9032 S433X_6  1.0
    @9033 S434_1   1.0
    @9034 S434_2   1.0
    @9035 S434_3   1.0
    @9036 S434_4   1.0
    @9037 S434_5   1.0
    @9038 S434_6   1.0
    @9039 S437_1   3.0
    @9042 S437_2   3.0
    @9045 S437_3   3.0
    @9048 S437_4   3.0
    @9051 S437_5   3.0
    @9054 S437_6   3.0
    @9057 S438A_1  1.0
    @9058 S438A_2  1.0
    @9059 S438A_3  1.0
    @9060 S438A_4  1.0
    @9061 S438A_5  1.0
    @9062 S438A_6  1.0
    @9063 S438B_1  1.0
    @9064 S438B_2  1.0
    @9065 S438B_3  1.0
    @9066 S438B_4  1.0
    @9067 S438B_5  1.0
    @9068 S438B_6  1.0
    @9069 S438C_1  1.0
    @9070 S438C_2  1.0
    @9071 S438C_3  1.0
    @9072 S438C_4  1.0
    @9073 S438C_5  1.0
    @9074 S438C_6  1.0
    @9075 S438D_1  1.0
    @9076 S438D_2  1.0
    @9077 S438D_3  1.0
    @9078 S438D_4  1.0
    @9079 S438D_5  1.0
    @9080 S438D_6  1.0
    @9081 S438E_1  1.0
    @9082 S438E_2  1.0
    @9083 S438E_3  1.0
    @9084 S438E_4  1.0
    @9085 S438E_5  1.0
    @9086 S438E_6  1.0
    @9087 S438F_1  1.0
    @9088 S438F_2  1.0
    @9089 S438F_3  1.0
    @9090 S438F_4  1.0
    @9091 S438F_5  1.0
    @9092 S438F_6  1.0
    @9093 S438G_1  1.0
    @9094 S438G_2  1.0
    @9095 S438G_3  1.0
    @9096 S438G_4  1.0
    @9097 S438G_5  1.0
    @9098 S438G_6  1.0
    @9099 S438H_1  1.0
    @9100 S438H_2  1.0
    @9101 S438H_3  1.0
    @9102 S438H_4  1.0
    @9103 S438H_5  1.0
    @9104 S438H_6  1.0
    @9105 S438I_1  1.0
    @9106 S438I_2  1.0
    @9107 S438I_3  1.0
    @9108 S438I_4  1.0
    @9109 S438I_5  1.0
    @9110 S438I_6  1.0
    @9111 S438J_1  1.0
    @9112 S438J_2  1.0
    @9113 S438J_3  1.0
    @9114 S438J_4  1.0
    @9115 S438J_5  1.0
    @9116 S438J_6  1.0
    @9117 S438X_1  1.0
    @9118 S438X_2  1.0
    @9119 S438X_3  1.0
    @9120 S438X_4  1.0
    @9121 S438X_5  1.0
    @9122 S438X_6  1.0
    @9123 S441_1   1.0
    @9124 S441_2   1.0
    @9125 S441_3   1.0
    @9126 S441_4   1.0
    @9127 S441_5   1.0
    @9128 S441_6   1.0
    @9129 S444A_1  1.0
    @9130 S444A_2  1.0
    @9131 S444A_3  1.0
    @9132 S444A_4  1.0
    @9133 S444A_5  1.0
    @9134 S444A_6  1.0
    @9135 S444B_1  1.0
    @9136 S444B_2  1.0
    @9137 S444B_3  1.0
    @9138 S444B_4  1.0
    @9139 S444B_5  1.0
    @9140 S444B_6  1.0
    @9141 S444C_1  1.0
    @9142 S444C_2  1.0
    @9143 S444C_3  1.0
    @9144 S444C_4  1.0
    @9145 S444C_5  1.0
    @9146 S444C_6  1.0
    @9147 S444D_1  1.0
    @9148 S444D_2  1.0
    @9149 S444D_3  1.0
    @9150 S444D_4  1.0
    @9151 S444D_5  1.0
    @9152 S444D_6  1.0
    @9153 S444E_1  1.0
    @9154 S444E_2  1.0
    @9155 S444E_3  1.0
    @9156 S444E_4  1.0
    @9157 S444E_5  1.0
    @9158 S444E_6  1.0
    @9159 S445_1   1.0
    @9160 S445_2   1.0
    @9161 S445_3   1.0
    @9162 S445_4   1.0
    @9163 S445_5   1.0
    @9164 S445_6   1.0
    @9165 S446_1   2.0
    @9167 S446_2   2.0
    @9169 S446_3   2.0
    @9171 S446_4   2.0
    @9173 S446_5   2.0
    @9175 S446_6   2.0
    @9177 S449_1   1.0
    @9178 S449_2   1.0
    @9179 S449_3   1.0
    @9180 S449_4   1.0
    @9181 S449_5   1.0
    @9182 S449_6   1.0
    @9183 S450_1   2.0
    @9185 S450_2   2.0
    @9187 S450_3   2.0
    @9189 S450_4   2.0
    @9191 S450_5   2.0
    @9193 S450_6   2.0
    @9195 S452A_1  1.0
    @9196 S452A_2  1.0
    @9197 S452A_3  1.0
    @9198 S452A_4  1.0
    @9199 S452A_5  1.0
    @9200 S452A_6  1.0
    @9201 S452B_1  1.0
    @9202 S452B_2  1.0
    @9203 S452B_3  1.0
    @9204 S452B_4  1.0
    @9205 S452B_5  1.0
    @9206 S452B_6  1.0
    @9207 S452C_1  1.0
    @9208 S452C_2  1.0
    @9209 S452C_3  1.0
    @9210 S452C_4  1.0
    @9211 S452C_5  1.0
    @9212 S452C_6  1.0
    @9213 S452D_1  1.0
    @9214 S452D_2  1.0
    @9215 S452D_3  1.0
    @9216 S452D_4  1.0
    @9217 S452D_5  1.0
    @9218 S452D_6  1.0
    @9219 S452E_1  1.0
    @9220 S452E_2  1.0
    @9221 S452E_3  1.0
    @9222 S452E_4  1.0
    @9223 S452E_5  1.0
    @9224 S452E_6  1.0
    @9225 S452F_1  1.0
    @9226 S452F_2  1.0
    @9227 S452F_3  1.0
    @9228 S452F_4  1.0
    @9229 S452F_5  1.0
    @9230 S452F_6  1.0
    @9231 S452G_1  1.0
    @9232 S452G_2  1.0
    @9233 S452G_3  1.0
    @9234 S452G_4  1.0
    @9235 S452G_5  1.0
    @9236 S452G_6  1.0
    @9237 S452H_1  1.0
    @9238 S452H_2  1.0
    @9239 S452H_3  1.0
    @9240 S452H_4  1.0
    @9241 S452H_5  1.0
    @9242 S452H_6  1.0
    @9243 S454_1   1.0
    @9244 S454_2   1.0
    @9245 S454_3   1.0
    @9246 S454_4   1.0
    @9247 S454_5   1.0
    @9248 S454_6   1.0
    @9249 S457A_1  1.0
    @9250 S457A_2  1.0
    @9251 S457A_3  1.0
    @9252 S457A_4  1.0
    @9253 S457A_5  1.0
    @9254 S457A_6  1.0
    @9255 S457B_1  1.0
    @9256 S457B_2  1.0
    @9257 S457B_3  1.0
    @9258 S457B_4  1.0
    @9259 S457B_5  1.0
    @9260 S457B_6  1.0
    @9261 S457C_1  1.0
    @9262 S457C_2  1.0
    @9263 S457C_3  1.0
    @9264 S457C_4  1.0
    @9265 S457C_5  1.0
    @9266 S457C_6  1.0
    @9267 S457D_1  1.0
    @9268 S457D_2  1.0
    @9269 S457D_3  1.0
    @9270 S457D_4  1.0
    @9271 S457D_5  1.0
    @9272 S457D_6  1.0
    @9273 S457X_1  1.0
    @9274 S457X_2  1.0
    @9275 S457X_3  1.0
    @9276 S457X_4  1.0
    @9277 S457X_5  1.0
    @9278 S457X_6  1.0
    @9279 S458A_1  1.0
    @9280 S458A_2  1.0
    @9281 S458A_3  1.0
    @9282 S458A_4  1.0
    @9283 S458A_5  1.0
    @9284 S458A_6  1.0
    @9285 S458B_1  1.0
    @9286 S458B_2  1.0
    @9287 S458B_3  1.0
    @9288 S458B_4  1.0
    @9289 S458B_5  1.0
    @9290 S458B_6  1.0
    @9291 S458C_1  1.0
    @9292 S458C_2  1.0
    @9293 S458C_3  1.0
    @9294 S458C_4  1.0
    @9295 S458C_5  1.0
    @9296 S458C_6  1.0
    @9297 S459A_1  1.0
    @9298 S459A_2  1.0
    @9299 S459A_3  1.0
    @9300 S459A_4  1.0
    @9301 S459A_5  1.0
    @9302 S459A_6  1.0
    @9303 S459B_1  1.0
    @9304 S459B_2  1.0
    @9305 S459B_3  1.0
    @9306 S459B_4  1.0
    @9307 S459B_5  1.0
    @9308 S459B_6  1.0
    @9309 S459C_1  1.0
    @9310 S459C_2  1.0
    @9311 S459C_3  1.0
    @9312 S459C_4  1.0
    @9313 S459C_5  1.0
    @9314 S459C_6  1.0
    @9315 S459D_1  1.0
    @9316 S459D_2  1.0
    @9317 S459D_3  1.0
    @9318 S459D_4  1.0
    @9319 S459D_5  1.0
    @9320 S459D_6  1.0
    @9321 S459E_1  1.0
    @9322 S459E_2  1.0
    @9323 S459E_3  1.0
    @9324 S459E_4  1.0
    @9325 S459E_5  1.0
    @9326 S459E_6  1.0
    @9327 S459F_1  1.0
    @9328 S459F_2  1.0
    @9329 S459F_3  1.0
    @9330 S459F_4  1.0
    @9331 S459F_5  1.0
    @9332 S459F_6  1.0
    @9333 S459G_1  1.0
    @9334 S459G_2  1.0
    @9335 S459G_3  1.0
    @9336 S459G_4  1.0
    @9337 S459G_5  1.0
    @9338 S459G_6  1.0
    @9339 S459H_1  1.0
    @9340 S459H_2  1.0
    @9341 S459H_3  1.0
    @9342 S459H_4  1.0
    @9343 S459H_5  1.0
    @9344 S459H_6  1.0
    @9345 S459X_1  1.0
    @9346 S459X_2  1.0
    @9347 S459X_3  1.0
    @9348 S459X_4  1.0
    @9349 S459X_5  1.0
    @9350 S459X_6  1.0
    @9351 S465_1   3.0
    @9354 S465_2   3.0
    @9357 S465_3   3.0
    @9360 S465_4   3.0
    @9363 S465_5   3.0
    @9366 S465_6   3.0
    @9369 S501     1.0
    @9370 S502     1.0
    @9371 S503N_01 2.0
    @9373 S503N_02 2.0
    @9375 S503N_03 2.0
    @9377 S503N_04 2.0
    @9379 S503N_05 2.0
    @9381 S503N_06 2.0
    @9383 S503N_07 2.0
    @9385 S503N_08 2.0
    @9387 S503N_09 2.0
    @9389 S503N_10 2.0
    @9391 S503N_11 2.0
    @9393 S503N_12 2.0
    @9395 S503N_13 2.0
    @9397 S503N_14 2.0
    @9399 S503N_15 2.0
    @9401 S503N_16 2.0
    @9403 S503N_17 2.0
    @9405 S503_01  1.0
    @9406 S503_02  1.0
    @9407 S503_03  1.0
    @9408 S503_04  1.0
    @9409 S503_05  1.0
    @9410 S503_06  1.0
    @9411 S503_07  1.0
    @9412 S503_08  1.0
    @9413 S503_09  1.0
    @9414 S503_10  1.0
    @9415 S503_11  1.0
    @9416 S503_12  1.0
    @9417 S503_13  1.0
    @9418 S503_14  1.0
    @9419 S503_15  1.0
    @9420 S503_16  1.0
    @9421 S503_17  1.0
    @9422 S504_01  2.0
    @9424 S504_02  2.0
    @9426 S504_03  2.0
    @9428 S504_04  2.0
    @9430 S504_05  2.0
    @9432 S504_06  2.0
    @9434 S504_07  2.0
    @9436 S504_08  2.0
    @9438 S504_09  2.0
    @9440 S504_10  2.0
    @9442 S504_11  2.0
    @9444 S504_12  2.0
    @9446 S504_13  2.0
    @9448 S504_14  2.0
    @9450 S504_15  2.0
    @9452 S504_16  2.0
    @9454 S504_17  2.0
    @9456 S505_01  2.0
    @9458 S505_02  2.0
    @9460 S505_03  2.0
    @9462 S505_04  2.0
    @9464 S505_05  2.0
    @9466 S505_06  2.0
    @9468 S505_07  2.0
    @9470 S505_08  2.0
    @9472 S505_09  2.0
    @9474 S505_10  2.0
    @9476 S505_11  2.0
    @9478 S505_12  2.0
    @9480 S505_13  2.0
    @9482 S505_14  2.0
    @9484 S505_15  2.0
    @9486 S505_16  2.0
    @9488 S505_17  2.0
    @9490 S506_01  2.0
    @9492 S506_02  2.0
    @9494 S506_03  2.0
    @9496 S506_04  2.0
    @9498 S506_05  2.0
    @9500 S506_06  2.0
    @9502 S506_07  2.0
    @9504 S506_08  2.0
    @9506 S506_09  2.0
    @9508 S506_10  2.0
    @9510 S506_11  2.0
    @9512 S506_12  2.0
    @9514 S506_13  2.0
    @9516 S506_14  2.0
    @9518 S506_15  2.0
    @9520 S506_16  2.0
    @9522 S506_17  2.0
    @9524 S508     1.0
    @9525 S509     1.0
    @9526 S510A    1.0
    @9527 S510B    1.0
    @9528 S510C    1.0
    @9529 S510D    1.0
    @9530 S510E    1.0
    @9531 S510F    1.0
    @9532 S510G    1.0
    @9533 S511     1.0
    @9534 S512A    1.0
    @9535 S512B    1.0
    @9536 S512C    1.0
    @9537 S512D    1.0
    @9538 S512E    1.0
    @9539 S512F    1.0
    @9540 S512G    1.0
    @9541 S512H    1.0
    @9542 S512I    1.0
    @9543 S512J    1.0
    @9544 S902     1.0
    @9545 S903A    1.0
    @9546 S903B    1.0
    @9547 S903C    1.0
    @9548 S903D    1.0
    @9549 S903E    1.0
    @9550 S903X    1.0
    @9551 S903Z    1.0
    @9552 S904     1.0
    @9553 S905M    2.0
    @9555 S905Y    4.0
    @9559 S906     1.0
    @9560 S907     1.0
    @9561 S908     1.0
    @9562 S909     1.0
    @9563 S910     1.0
    @9564 S911     1.0
    @9565 S913     1.0
    @9566 S914     2.0
    @9568 S915     1.0
    @9569 S916     2.0
    @9571 S917     1.0
    @9572 S918     1.0
    @9573 S920     1.0
    @9574 S921     1.0
    @9575 S922A    1.0
    @9576 S922B    1.0
    @9577 S922C    1.0
    @9578 S922D    1.0
    @9579 S922X    1.0
    @9580 S922Z    1.0
    @9581 S923     1.0
    @9582 S924     1.0
    @9583 S925     1.0
    @9584 S926     1.0
    @9585 S927     1.0
    @9586 S928     1.0
    @9587 S930     1.0
    @9588 S931A    1.0
    @9589 S931B    1.0
    @9590 S931C    1.0
    @9591 S931D    1.0
    @9592 S931X    1.0
    @9593 S931Z    1.0
    @9594 S932     1.0
    @9595 S933     1.0
    @9596 S934M    2.0
    @9598 S934Y    4.0
    @9602 S935     1.0
    @9603 S936     2.0
    @9605 S937     1.0
    @9606 S938     1.0
    @9607 S939     1.0
    @9608 S940     1.0
    @9609 S941     2.0
    @9611 S942     1.0
    @9612 S943     2.0
    @9614 S944     2.0
    @9616 S1301A   1.0
    @9617 S1301B   1.0
    @9618 S1301C   1.0
    @9619 S1301D   1.0
    @9620 S1301E   1.0
    @9621 S1301F   1.0
    @9622 S1301G   1.0
    @9623 S1301H   1.0
    @9624 S1301I   1.0
    @9625 S1301J   1.0
    @9626 S1301K   1.0
    @9627 S1301L   1.0
    @9628 S1302A   1.0
    @9629 S1302B   1.0
    @9630 S1302C   1.0
    @9631 S1302D   1.0
    @9632 S1302E   1.0
    @9633 S1302F   1.0
    @9634 S1302G   1.0
    @9635 S1302H   1.0
    @9636 S1304A   1.0
    @9637 S1304B   1.0
    @9638 S1304C   1.0
    @9639 S1304D   1.0
    @9640 S1304E   1.0
    @9641 S1304F   1.0
    @9642 S1304G   1.0
    @9643 S1304H   1.0
    @9644 S1308A   1.0
    @9645 S1308B   1.0
    @9646 S1308C   1.0
    @9647 S1308D   1.0
    @9648 S1308E   1.0
    @9649 S1309    2.0
    @9651 S1310A   1.0
    @9652 S1310B   1.0
    @9653 S1310C   1.0
    @9654 S1310D   1.0
    @9655 S1310E   1.0
    @9656 S1310F   1.0
    @9657 S1310G   1.0
    @9658 S1310H   1.0
    @9659 S1310I   1.0
    @9660 S1310J   1.0
    @9661 S1310X   1.0
    @9662 S1312    1.0
    @9663 S1313    1.0
    @9664 S1314    1.0
    @9665 S1315    1.0
    @9666 S1316A   1.0
    @9667 S1316B   1.0
    @9668 S1316C   1.0
    @9669 S1316D   1.0
    @9670 S1316E   1.0
    @9671 S1316F   1.0
    @9672 S1316G   1.0
    @9673 S1316H   1.0
    @9674 S1321    1.0
    @9675 S1322    2.0
    @9677 S1323    2.0
    ;
    if V026     = 9 then V026 = .;
    if V027     = 9 then V027 = .;
    if V028     = 99999 then V028 = .;
    if V030     = 99999 then V030 = .;
    if V034     = 99 then V034 = .;
    if V040     = 9999 then V040 = .;
    if V103     = 9 then V103 = .;
    if V104     = 99 then V104 = .;
    if V105     = 9 then V105 = .;
    if V105A    = 99 then V105A = .;
    if V106     = 9 then V106 = .;
    if V107     = 99 then V107 = .;
    if V113     = 99 then V113 = .;
    if V115     = 999 then V115 = .;
    if V116     = 99 then V116 = .;
    if V119     = 9 then V119 = .;
    if V120     = 9 then V120 = .;
    if V121     = 9 then V121 = .;
    if V122     = 9 then V122 = .;
    if V123     = 9 then V123 = .;
    if V124     = 9 then V124 = .;
    if V125     = 9 then V125 = .;
    if V127     = 99 then V127 = .;
    if V128     = 99 then V128 = .;
    if V129     = 99 then V129 = .;
    if V130     = 99 then V130 = .;
    if V131     = 999 then V131 = .;
    if V133     = 99 then V133 = .;
    if V134     = 9 then V134 = .;
    if V135     = 9 then V135 = .;
    if V139     = 99 then V139 = .;
    if V140     = 9 then V140 = .;
    if V141     = 9 then V141 = .;
    if V149     = 9 then V149 = .;
    if V150     = 99 then V150 = .;
    if V152     = 99 then V152 = .;
    if V153     = 9 then V153 = .;
    if V155     = 9 then V155 = .;
    if V156     = 9 then V156 = .;
    if V157     = 9 then V157 = .;
    if V158     = 9 then V158 = .;
    if V159     = 9 then V159 = .;
    if V160     = 9 then V160 = .;
    if V161     = 99 then V161 = .;
    if V166     = 999 then V166 = .;
    if V167     = 99 then V167 = .;
    if V168     = 9 then V168 = .;
    if V169A    = 9 then V169A = .;
    if V169B    = 9 then V169B = .;
    if V170     = 9 then V170 = .;
    if V171A    = 9 then V171A = .;
    if V171B    = 9 then V171B = .;
    if ML101    = 9 then ML101 = .;
    if B6_01    = 999 then B6_01 = .;
    if B6_02    = 999 then B6_02 = .;
    if B6_03    = 999 then B6_03 = .;
    if B6_04    = 999 then B6_04 = .;
    if B6_05    = 999 then B6_05 = .;
    if B6_06    = 999 then B6_06 = .;
    if B6_07    = 999 then B6_07 = .;
    if B6_08    = 999 then B6_08 = .;
    if B6_09    = 999 then B6_09 = .;
    if B6_10    = 999 then B6_10 = .;
    if B6_11    = 999 then B6_11 = .;
    if B6_12    = 999 then B6_12 = .;
    if B6_13    = 999 then B6_13 = .;
    if B6_14    = 999 then B6_14 = .;
    if B6_15    = 999 then B6_15 = .;
    if B6_16    = 999 then B6_16 = .;
    if B6_17    = 999 then B6_17 = .;
    if B6_18    = 999 then B6_18 = .;
    if B6_19    = 999 then B6_19 = .;
    if B6_20    = 999 then B6_20 = .;
    if B9_01    = 9 then B9_01 = .;
    if B9_02    = 9 then B9_02 = .;
    if B9_03    = 9 then B9_03 = .;
    if B9_04    = 9 then B9_04 = .;
    if B9_05    = 9 then B9_05 = .;
    if B9_06    = 9 then B9_06 = .;
    if B9_07    = 9 then B9_07 = .;
    if B9_08    = 9 then B9_08 = .;
    if B9_09    = 9 then B9_09 = .;
    if B9_10    = 9 then B9_10 = .;
    if B9_11    = 9 then B9_11 = .;
    if B9_12    = 9 then B9_12 = .;
    if B9_13    = 9 then B9_13 = .;
    if B9_14    = 9 then B9_14 = .;
    if B9_15    = 9 then B9_15 = .;
    if B9_16    = 9 then B9_16 = .;
    if B9_17    = 9 then B9_17 = .;
    if B9_18    = 9 then B9_18 = .;
    if B9_19    = 9 then B9_19 = .;
    if B9_20    = 9 then B9_20 = .;
    if B15_01   = 9 then B15_01 = .;
    if B15_02   = 9 then B15_02 = .;
    if B15_03   = 9 then B15_03 = .;
    if B15_04   = 9 then B15_04 = .;
    if B15_05   = 9 then B15_05 = .;
    if B15_06   = 9 then B15_06 = .;
    if B15_07   = 9 then B15_07 = .;
    if B15_08   = 9 then B15_08 = .;
    if B15_09   = 9 then B15_09 = .;
    if B15_10   = 9 then B15_10 = .;
    if B15_11   = 9 then B15_11 = .;
    if B15_12   = 9 then B15_12 = .;
    if B15_13   = 9 then B15_13 = .;
    if B15_14   = 9 then B15_14 = .;
    if B15_15   = 9 then B15_15 = .;
    if B15_16   = 9 then B15_16 = .;
    if B15_17   = 9 then B15_17 = .;
    if B15_18   = 9 then B15_18 = .;
    if B15_19   = 9 then B15_19 = .;
    if B15_20   = 9 then B15_20 = .;
    if B16_01   = 99 then B16_01 = .;
    if B16_02   = 99 then B16_02 = .;
    if B16_03   = 99 then B16_03 = .;
    if B16_04   = 99 then B16_04 = .;
    if B16_05   = 99 then B16_05 = .;
    if B16_06   = 99 then B16_06 = .;
    if B16_07   = 99 then B16_07 = .;
    if B16_08   = 99 then B16_08 = .;
    if B16_09   = 99 then B16_09 = .;
    if B16_10   = 99 then B16_10 = .;
    if B16_11   = 99 then B16_11 = .;
    if B16_12   = 99 then B16_12 = .;
    if B16_13   = 99 then B16_13 = .;
    if B16_14   = 99 then B16_14 = .;
    if B16_15   = 99 then B16_15 = .;
    if B16_16   = 99 then B16_16 = .;
    if B16_17   = 99 then B16_17 = .;
    if B16_18   = 99 then B16_18 = .;
    if B16_19   = 99 then B16_19 = .;
    if B16_20   = 99 then B16_20 = .;
    if V215     = 999 then V215 = .;
    if V217     = 9 then V217 = .;
    if V225     = 9 then V225 = .;
    if V226     = 999 then V226 = .;
    if V228     = 9 then V228 = .;
    if V229     = 99 then V229 = .;
    if V230     = 9999 then V230 = .;
    if V231     = 9999 then V231 = .;
    if V233     = 99 then V233 = .;
    if V234     = 9 then V234 = .;
    if V237     = 9 then V237 = .;
    if V239     = 9 then V239 = .;
    if V240     = 99 then V240 = .;
    if V241     = 9999 then V241 = .;
    if V242     = 9999 then V242 = .;
    if V244     = 9 then V244 = .;
    if V304_01  = 9 then V304_01 = .;
    if V304_02  = 9 then V304_02 = .;
    if V304_03  = 9 then V304_03 = .;
    if V304_04  = 9 then V304_04 = .;
    if V304_05  = 9 then V304_05 = .;
    if V304_06  = 9 then V304_06 = .;
    if V304_07  = 9 then V304_07 = .;
    if V304_08  = 9 then V304_08 = .;
    if V304_09  = 9 then V304_09 = .;
    if V304_10  = 9 then V304_10 = .;
    if V304_11  = 9 then V304_11 = .;
    if V304_12  = 9 then V304_12 = .;
    if V304_13  = 9 then V304_13 = .;
    if V304_14  = 9 then V304_14 = .;
    if V304_15  = 9 then V304_15 = .;
    if V304_16  = 9 then V304_16 = .;
    if V304_17  = 9 then V304_17 = .;
    if V304_18  = 9 then V304_18 = .;
    if V304_19  = 9 then V304_19 = .;
    if V304_20  = 9 then V304_20 = .;
    if V305_01  = 9 then V305_01 = .;
    if V305_02  = 9 then V305_02 = .;
    if V305_03  = 9 then V305_03 = .;
    if V305_04  = 9 then V305_04 = .;
    if V305_05  = 9 then V305_05 = .;
    if V305_06  = 9 then V305_06 = .;
    if V305_07  = 9 then V305_07 = .;
    if V305_08  = 9 then V305_08 = .;
    if V305_09  = 9 then V305_09 = .;
    if V305_10  = 9 then V305_10 = .;
    if V305_11  = 9 then V305_11 = .;
    if V305_12  = 9 then V305_12 = .;
    if V305_13  = 9 then V305_13 = .;
    if V305_14  = 9 then V305_14 = .;
    if V305_15  = 9 then V305_15 = .;
    if V305_16  = 9 then V305_16 = .;
    if V305_17  = 9 then V305_17 = .;
    if V305_18  = 9 then V305_18 = .;
    if V305_19  = 9 then V305_19 = .;
    if V305_20  = 9 then V305_20 = .;
    if V307_01  = 9 then V307_01 = .;
    if V307_02  = 9 then V307_02 = .;
    if V307_03  = 9 then V307_03 = .;
    if V307_04  = 9 then V307_04 = .;
    if V307_05  = 9 then V307_05 = .;
    if V307_06  = 9 then V307_06 = .;
    if V307_07  = 9 then V307_07 = .;
    if V307_08  = 9 then V307_08 = .;
    if V307_09  = 9 then V307_09 = .;
    if V307_10  = 9 then V307_10 = .;
    if V307_11  = 9 then V307_11 = .;
    if V307_12  = 9 then V307_12 = .;
    if V307_13  = 9 then V307_13 = .;
    if V307_14  = 9 then V307_14 = .;
    if V307_15  = 9 then V307_15 = .;
    if V307_16  = 9 then V307_16 = .;
    if V307_17  = 9 then V307_17 = .;
    if V307_18  = 9 then V307_18 = .;
    if V307_19  = 9 then V307_19 = .;
    if V307_20  = 9 then V307_20 = .;
    if V310     = 99 then V310 = .;
    if V311     = 9 then V311 = .;
    if V312     = 99 then V312 = .;
    if V313     = 9 then V313 = .;
    if V323     = 99 then V323 = .;
    if V323A    = 99 then V323A = .;
    if V325A    = 9999999 then V325A = .;
    if V326     = 99 then V326 = .;
    if V327     = 9 then V327 = .;
    if V337     = 999 then V337 = .;
    if V359     = 99 then V359 = .;
    if V360     = 99 then V360 = .;
    if V362     = 9 then V362 = .;
    if V363     = 99 then V363 = .;
    if V367     = 9 then V367 = .;
    if V372     = 9 then V372 = .;
    if V372A    = 9 then V372A = .;
    if V375A    = 99 then V375A = .;
    if V376     = 99 then V376 = .;
    if V376A    = 9 then V376A = .;
    if V379     = 99 then V379 = .;
    if V380     = 9 then V380 = .;
    if V384A    = 9 then V384A = .;
    if V384B    = 9 then V384B = .;
    if V384C    = 9 then V384C = .;
    if V384D    = 9 then V384D = .;
    if V393     = 9 then V393 = .;
    if V393A    = 9 then V393A = .;
    if V394     = 9 then V394 = .;
    if V395     = 9 then V395 = .;
    if V3A00A   = 9 then V3A00A = .;
    if V3A00B   = 9 then V3A00B = .;
    if V3A00C   = 9 then V3A00C = .;
    if V3A00D   = 9 then V3A00D = .;
    if V3A00E   = 9 then V3A00E = .;
    if V3A00F   = 9 then V3A00F = .;
    if V3A00G   = 9 then V3A00G = .;
    if V3A00H   = 9 then V3A00H = .;
    if V3A00I   = 9 then V3A00I = .;
    if V3A00J   = 9 then V3A00J = .;
    if V3A00K   = 9 then V3A00K = .;
    if V3A00L   = 9 then V3A00L = .;
    if V3A00M   = 9 then V3A00M = .;
    if V3A00N   = 9 then V3A00N = .;
    if V3A00O   = 9 then V3A00O = .;
    if V3A00P   = 9 then V3A00P = .;
    if V3A00Q   = 9 then V3A00Q = .;
    if V3A00R   = 9 then V3A00R = .;
    if V3A00S   = 9 then V3A00S = .;
    if V3A00T   = 9 then V3A00T = .;
    if V3A00U   = 9 then V3A00U = .;
    if V3A00V   = 9 then V3A00V = .;
    if V3A00W   = 9 then V3A00W = .;
    if V3A00X   = 9 then V3A00X = .;
    if V3A00Y   = 9 then V3A00Y = .;
    if V3A00Z   = 9 then V3A00Z = .;
    if V3A01    = 9 then V3A01 = .;
    if V3A02    = 9 then V3A02 = .;
    if V3A03    = 9 then V3A03 = .;
    if V3A04    = 9 then V3A04 = .;
    if V3A05    = 9 then V3A05 = .;
    if V3A06    = 9 then V3A06 = .;
    if V3A07    = 99 then V3A07 = .;
    if V3A08A   = 9 then V3A08A = .;
    if V3A08B   = 9 then V3A08B = .;
    if V3A08C   = 9 then V3A08C = .;
    if V3A08D   = 9 then V3A08D = .;
    if V3A08E   = 9 then V3A08E = .;
    if V3A08F   = 9 then V3A08F = .;
    if V3A08G   = 9 then V3A08G = .;
    if V3A08H   = 9 then V3A08H = .;
    if V3A08I   = 9 then V3A08I = .;
    if V3A08J   = 9 then V3A08J = .;
    if V3A08K   = 9 then V3A08K = .;
    if V3A08L   = 9 then V3A08L = .;
    if V3A08M   = 9 then V3A08M = .;
    if V3A08N   = 9 then V3A08N = .;
    if V3A08O   = 9 then V3A08O = .;
    if V3A08P   = 9 then V3A08P = .;
    if V3A08Q   = 9 then V3A08Q = .;
    if V3A08R   = 9 then V3A08R = .;
    if V3A08S   = 9 then V3A08S = .;
    if V3A08T   = 9 then V3A08T = .;
    if V3A08U   = 9 then V3A08U = .;
    if V3A08V   = 9 then V3A08V = .;
    if V3A08W   = 9 then V3A08W = .;
    if V3A08AA  = 9 then V3A08AA = .;
    if V3A08AB  = 9 then V3A08AB = .;
    if V3A08AC  = 9 then V3A08AC = .;
    if V3A08AD  = 9 then V3A08AD = .;
    if V3A08X   = 9 then V3A08X = .;
    if V3A08Z   = 9 then V3A08Z = .;
    if V3A09A   = 999 then V3A09A = .;
    if V3A09B   = 999 then V3A09B = .;
    if M1_1     = 9 then M1_1 = .;
    if M1_2     = 9 then M1_2 = .;
    if M1_3     = 9 then M1_3 = .;
    if M1_4     = 9 then M1_4 = .;
    if M1_5     = 9 then M1_5 = .;
    if M1_6     = 9 then M1_6 = .;
    if M1A_1    = 9 then M1A_1 = .;
    if M1A_2    = 9 then M1A_2 = .;
    if M1A_3    = 9 then M1A_3 = .;
    if M1A_4    = 9 then M1A_4 = .;
    if M1A_5    = 9 then M1A_5 = .;
    if M1A_6    = 9 then M1A_6 = .;
    if M1B_1    = 99 then M1B_1 = .;
    if M1B_2    = 99 then M1B_2 = .;
    if M1B_3    = 99 then M1B_3 = .;
    if M1B_4    = 99 then M1B_4 = .;
    if M1B_5    = 99 then M1B_5 = .;
    if M1B_6    = 99 then M1B_6 = .;
    if M1C_1    = 9999 then M1C_1 = .;
    if M1C_2    = 9999 then M1C_2 = .;
    if M1C_3    = 9999 then M1C_3 = .;
    if M1C_4    = 9999 then M1C_4 = .;
    if M1C_5    = 9999 then M1C_5 = .;
    if M1C_6    = 9999 then M1C_6 = .;
    if M1D_1    = 99 then M1D_1 = .;
    if M1D_2    = 99 then M1D_2 = .;
    if M1D_3    = 99 then M1D_3 = .;
    if M1D_4    = 99 then M1D_4 = .;
    if M1D_5    = 99 then M1D_5 = .;
    if M1D_6    = 99 then M1D_6 = .;
    if M2A_1    = 9 then M2A_1 = .;
    if M2A_2    = 9 then M2A_2 = .;
    if M2A_3    = 9 then M2A_3 = .;
    if M2A_4    = 9 then M2A_4 = .;
    if M2A_5    = 9 then M2A_5 = .;
    if M2A_6    = 9 then M2A_6 = .;
    if M2B_1    = 9 then M2B_1 = .;
    if M2B_2    = 9 then M2B_2 = .;
    if M2B_3    = 9 then M2B_3 = .;
    if M2B_4    = 9 then M2B_4 = .;
    if M2B_5    = 9 then M2B_5 = .;
    if M2B_6    = 9 then M2B_6 = .;
    if M2C_1    = 9 then M2C_1 = .;
    if M2C_2    = 9 then M2C_2 = .;
    if M2C_3    = 9 then M2C_3 = .;
    if M2C_4    = 9 then M2C_4 = .;
    if M2C_5    = 9 then M2C_5 = .;
    if M2C_6    = 9 then M2C_6 = .;
    if M2D_1    = 9 then M2D_1 = .;
    if M2D_2    = 9 then M2D_2 = .;
    if M2D_3    = 9 then M2D_3 = .;
    if M2D_4    = 9 then M2D_4 = .;
    if M2D_5    = 9 then M2D_5 = .;
    if M2D_6    = 9 then M2D_6 = .;
    if M2E_1    = 9 then M2E_1 = .;
    if M2E_2    = 9 then M2E_2 = .;
    if M2E_3    = 9 then M2E_3 = .;
    if M2E_4    = 9 then M2E_4 = .;
    if M2E_5    = 9 then M2E_5 = .;
    if M2E_6    = 9 then M2E_6 = .;
    if M2F_1    = 9 then M2F_1 = .;
    if M2F_2    = 9 then M2F_2 = .;
    if M2F_3    = 9 then M2F_3 = .;
    if M2F_4    = 9 then M2F_4 = .;
    if M2F_5    = 9 then M2F_5 = .;
    if M2F_6    = 9 then M2F_6 = .;
    if M2G_1    = 9 then M2G_1 = .;
    if M2G_2    = 9 then M2G_2 = .;
    if M2G_3    = 9 then M2G_3 = .;
    if M2G_4    = 9 then M2G_4 = .;
    if M2G_5    = 9 then M2G_5 = .;
    if M2G_6    = 9 then M2G_6 = .;
    if M2H_1    = 9 then M2H_1 = .;
    if M2H_2    = 9 then M2H_2 = .;
    if M2H_3    = 9 then M2H_3 = .;
    if M2H_4    = 9 then M2H_4 = .;
    if M2H_5    = 9 then M2H_5 = .;
    if M2H_6    = 9 then M2H_6 = .;
    if M2I_1    = 9 then M2I_1 = .;
    if M2I_2    = 9 then M2I_2 = .;
    if M2I_3    = 9 then M2I_3 = .;
    if M2I_4    = 9 then M2I_4 = .;
    if M2I_5    = 9 then M2I_5 = .;
    if M2I_6    = 9 then M2I_6 = .;
    if M2J_1    = 9 then M2J_1 = .;
    if M2J_2    = 9 then M2J_2 = .;
    if M2J_3    = 9 then M2J_3 = .;
    if M2J_4    = 9 then M2J_4 = .;
    if M2J_5    = 9 then M2J_5 = .;
    if M2J_6    = 9 then M2J_6 = .;
    if M2K_1    = 9 then M2K_1 = .;
    if M2K_2    = 9 then M2K_2 = .;
    if M2K_3    = 9 then M2K_3 = .;
    if M2K_4    = 9 then M2K_4 = .;
    if M2K_5    = 9 then M2K_5 = .;
    if M2K_6    = 9 then M2K_6 = .;
    if M2L_1    = 9 then M2L_1 = .;
    if M2L_2    = 9 then M2L_2 = .;
    if M2L_3    = 9 then M2L_3 = .;
    if M2L_4    = 9 then M2L_4 = .;
    if M2L_5    = 9 then M2L_5 = .;
    if M2L_6    = 9 then M2L_6 = .;
    if M2M_1    = 9 then M2M_1 = .;
    if M2M_2    = 9 then M2M_2 = .;
    if M2M_3    = 9 then M2M_3 = .;
    if M2M_4    = 9 then M2M_4 = .;
    if M2M_5    = 9 then M2M_5 = .;
    if M2M_6    = 9 then M2M_6 = .;
    if M2N_1    = 9 then M2N_1 = .;
    if M2N_2    = 9 then M2N_2 = .;
    if M2N_3    = 9 then M2N_3 = .;
    if M2N_4    = 9 then M2N_4 = .;
    if M2N_5    = 9 then M2N_5 = .;
    if M2N_6    = 9 then M2N_6 = .;
    if M3A_1    = 9 then M3A_1 = .;
    if M3A_2    = 9 then M3A_2 = .;
    if M3A_3    = 9 then M3A_3 = .;
    if M3A_4    = 9 then M3A_4 = .;
    if M3A_5    = 9 then M3A_5 = .;
    if M3A_6    = 9 then M3A_6 = .;
    if M3B_1    = 9 then M3B_1 = .;
    if M3B_2    = 9 then M3B_2 = .;
    if M3B_3    = 9 then M3B_3 = .;
    if M3B_4    = 9 then M3B_4 = .;
    if M3B_5    = 9 then M3B_5 = .;
    if M3B_6    = 9 then M3B_6 = .;
    if M3C_1    = 9 then M3C_1 = .;
    if M3C_2    = 9 then M3C_2 = .;
    if M3C_3    = 9 then M3C_3 = .;
    if M3C_4    = 9 then M3C_4 = .;
    if M3C_5    = 9 then M3C_5 = .;
    if M3C_6    = 9 then M3C_6 = .;
    if M3D_1    = 9 then M3D_1 = .;
    if M3D_2    = 9 then M3D_2 = .;
    if M3D_3    = 9 then M3D_3 = .;
    if M3D_4    = 9 then M3D_4 = .;
    if M3D_5    = 9 then M3D_5 = .;
    if M3D_6    = 9 then M3D_6 = .;
    if M3E_1    = 9 then M3E_1 = .;
    if M3E_2    = 9 then M3E_2 = .;
    if M3E_3    = 9 then M3E_3 = .;
    if M3E_4    = 9 then M3E_4 = .;
    if M3E_5    = 9 then M3E_5 = .;
    if M3E_6    = 9 then M3E_6 = .;
    if M3F_1    = 9 then M3F_1 = .;
    if M3F_2    = 9 then M3F_2 = .;
    if M3F_3    = 9 then M3F_3 = .;
    if M3F_4    = 9 then M3F_4 = .;
    if M3F_5    = 9 then M3F_5 = .;
    if M3F_6    = 9 then M3F_6 = .;
    if M3G_1    = 9 then M3G_1 = .;
    if M3G_2    = 9 then M3G_2 = .;
    if M3G_3    = 9 then M3G_3 = .;
    if M3G_4    = 9 then M3G_4 = .;
    if M3G_5    = 9 then M3G_5 = .;
    if M3G_6    = 9 then M3G_6 = .;
    if M3H_1    = 9 then M3H_1 = .;
    if M3H_2    = 9 then M3H_2 = .;
    if M3H_3    = 9 then M3H_3 = .;
    if M3H_4    = 9 then M3H_4 = .;
    if M3H_5    = 9 then M3H_5 = .;
    if M3H_6    = 9 then M3H_6 = .;
    if M3I_1    = 9 then M3I_1 = .;
    if M3I_2    = 9 then M3I_2 = .;
    if M3I_3    = 9 then M3I_3 = .;
    if M3I_4    = 9 then M3I_4 = .;
    if M3I_5    = 9 then M3I_5 = .;
    if M3I_6    = 9 then M3I_6 = .;
    if M3J_1    = 9 then M3J_1 = .;
    if M3J_2    = 9 then M3J_2 = .;
    if M3J_3    = 9 then M3J_3 = .;
    if M3J_4    = 9 then M3J_4 = .;
    if M3J_5    = 9 then M3J_5 = .;
    if M3J_6    = 9 then M3J_6 = .;
    if M3K_1    = 9 then M3K_1 = .;
    if M3K_2    = 9 then M3K_2 = .;
    if M3K_3    = 9 then M3K_3 = .;
    if M3K_4    = 9 then M3K_4 = .;
    if M3K_5    = 9 then M3K_5 = .;
    if M3K_6    = 9 then M3K_6 = .;
    if M3L_1    = 9 then M3L_1 = .;
    if M3L_2    = 9 then M3L_2 = .;
    if M3L_3    = 9 then M3L_3 = .;
    if M3L_4    = 9 then M3L_4 = .;
    if M3L_5    = 9 then M3L_5 = .;
    if M3L_6    = 9 then M3L_6 = .;
    if M3M_1    = 9 then M3M_1 = .;
    if M3M_2    = 9 then M3M_2 = .;
    if M3M_3    = 9 then M3M_3 = .;
    if M3M_4    = 9 then M3M_4 = .;
    if M3M_5    = 9 then M3M_5 = .;
    if M3M_6    = 9 then M3M_6 = .;
    if M3N_1    = 9 then M3N_1 = .;
    if M3N_2    = 9 then M3N_2 = .;
    if M3N_3    = 9 then M3N_3 = .;
    if M3N_4    = 9 then M3N_4 = .;
    if M3N_5    = 9 then M3N_5 = .;
    if M3N_6    = 9 then M3N_6 = .;
    if M4_1     = 99 then M4_1 = .;
    if M4_2     = 99 then M4_2 = .;
    if M4_3     = 99 then M4_3 = .;
    if M4_4     = 99 then M4_4 = .;
    if M4_5     = 99 then M4_5 = .;
    if M4_6     = 99 then M4_6 = .;
    if M5_1     = 99 then M5_1 = .;
    if M5_2     = 99 then M5_2 = .;
    if M5_3     = 99 then M5_3 = .;
    if M5_4     = 99 then M5_4 = .;
    if M5_5     = 99 then M5_5 = .;
    if M5_6     = 99 then M5_6 = .;
    if M6_1     = 99 then M6_1 = .;
    if M6_2     = 99 then M6_2 = .;
    if M6_3     = 99 then M6_3 = .;
    if M6_4     = 99 then M6_4 = .;
    if M6_5     = 99 then M6_5 = .;
    if M6_6     = 99 then M6_6 = .;
    if M7_1     = 99 then M7_1 = .;
    if M7_2     = 99 then M7_2 = .;
    if M7_3     = 99 then M7_3 = .;
    if M7_4     = 99 then M7_4 = .;
    if M7_5     = 99 then M7_5 = .;
    if M7_6     = 99 then M7_6 = .;
    if M8_1     = 99 then M8_1 = .;
    if M8_2     = 99 then M8_2 = .;
    if M8_3     = 99 then M8_3 = .;
    if M8_4     = 99 then M8_4 = .;
    if M8_5     = 99 then M8_5 = .;
    if M8_6     = 99 then M8_6 = .;
    if M9_1     = 99 then M9_1 = .;
    if M9_2     = 99 then M9_2 = .;
    if M9_3     = 99 then M9_3 = .;
    if M9_4     = 99 then M9_4 = .;
    if M9_5     = 99 then M9_5 = .;
    if M9_6     = 99 then M9_6 = .;
    if M10_1    = 9 then M10_1 = .;
    if M10_2    = 9 then M10_2 = .;
    if M10_3    = 9 then M10_3 = .;
    if M10_4    = 9 then M10_4 = .;
    if M10_5    = 9 then M10_5 = .;
    if M10_6    = 9 then M10_6 = .;
    if M11_1    = 999 then M11_1 = .;
    if M11_2    = 999 then M11_2 = .;
    if M11_3    = 999 then M11_3 = .;
    if M11_4    = 999 then M11_4 = .;
    if M11_5    = 999 then M11_5 = .;
    if M11_6    = 999 then M11_6 = .;
    if M13_1    = 99 then M13_1 = .;
    if M13_2    = 99 then M13_2 = .;
    if M13_3    = 99 then M13_3 = .;
    if M13_4    = 99 then M13_4 = .;
    if M13_5    = 99 then M13_5 = .;
    if M13_6    = 99 then M13_6 = .;
    if M14_1    = 99 then M14_1 = .;
    if M14_2    = 99 then M14_2 = .;
    if M14_3    = 99 then M14_3 = .;
    if M14_4    = 99 then M14_4 = .;
    if M14_5    = 99 then M14_5 = .;
    if M14_6    = 99 then M14_6 = .;
    if M15_1    = 99 then M15_1 = .;
    if M15_2    = 99 then M15_2 = .;
    if M15_3    = 99 then M15_3 = .;
    if M15_4    = 99 then M15_4 = .;
    if M15_5    = 99 then M15_5 = .;
    if M15_6    = 99 then M15_6 = .;
    if M17_1    = 9 then M17_1 = .;
    if M17_2    = 9 then M17_2 = .;
    if M17_3    = 9 then M17_3 = .;
    if M17_4    = 9 then M17_4 = .;
    if M17_5    = 9 then M17_5 = .;
    if M17_6    = 9 then M17_6 = .;
    if M17A_1   = 9 then M17A_1 = .;
    if M17A_2   = 9 then M17A_2 = .;
    if M17A_3   = 9 then M17A_3 = .;
    if M17A_4   = 9 then M17A_4 = .;
    if M17A_5   = 9 then M17A_5 = .;
    if M17A_6   = 9 then M17A_6 = .;
    if M18_1    = 9 then M18_1 = .;
    if M18_2    = 9 then M18_2 = .;
    if M18_3    = 9 then M18_3 = .;
    if M18_4    = 9 then M18_4 = .;
    if M18_5    = 9 then M18_5 = .;
    if M18_6    = 9 then M18_6 = .;
    if M19_1    = 9999 then M19_1 = .;
    if M19_2    = 9999 then M19_2 = .;
    if M19_3    = 9999 then M19_3 = .;
    if M19_4    = 9999 then M19_4 = .;
    if M19_5    = 9999 then M19_5 = .;
    if M19_6    = 9999 then M19_6 = .;
    if M34_1    = 999 then M34_1 = .;
    if M34_2    = 999 then M34_2 = .;
    if M34_3    = 999 then M34_3 = .;
    if M34_4    = 999 then M34_4 = .;
    if M34_5    = 999 then M34_5 = .;
    if M34_6    = 999 then M34_6 = .;
    if M35_1    = 99 then M35_1 = .;
    if M35_2    = 99 then M35_2 = .;
    if M35_3    = 99 then M35_3 = .;
    if M35_4    = 99 then M35_4 = .;
    if M35_5    = 99 then M35_5 = .;
    if M35_6    = 99 then M35_6 = .;
    if M36_1    = 99 then M36_1 = .;
    if M36_2    = 99 then M36_2 = .;
    if M36_3    = 99 then M36_3 = .;
    if M36_4    = 99 then M36_4 = .;
    if M36_5    = 99 then M36_5 = .;
    if M36_6    = 99 then M36_6 = .;
    if M38_1    = 9 then M38_1 = .;
    if M38_2    = 9 then M38_2 = .;
    if M38_3    = 9 then M38_3 = .;
    if M38_4    = 9 then M38_4 = .;
    if M38_5    = 9 then M38_5 = .;
    if M38_6    = 9 then M38_6 = .;
    if M39A_1   = 9 then M39A_1 = .;
    if M39A_2   = 9 then M39A_2 = .;
    if M39A_3   = 9 then M39A_3 = .;
    if M39A_4   = 9 then M39A_4 = .;
    if M39A_5   = 9 then M39A_5 = .;
    if M39A_6   = 9 then M39A_6 = .;
    if M39_1    = 9 then M39_1 = .;
    if M39_2    = 9 then M39_2 = .;
    if M39_3    = 9 then M39_3 = .;
    if M39_4    = 9 then M39_4 = .;
    if M39_5    = 9 then M39_5 = .;
    if M39_6    = 9 then M39_6 = .;
    if M42A_1   = 9 then M42A_1 = .;
    if M42A_2   = 9 then M42A_2 = .;
    if M42A_3   = 9 then M42A_3 = .;
    if M42A_4   = 9 then M42A_4 = .;
    if M42A_5   = 9 then M42A_5 = .;
    if M42A_6   = 9 then M42A_6 = .;
    if M42B_1   = 9 then M42B_1 = .;
    if M42B_2   = 9 then M42B_2 = .;
    if M42B_3   = 9 then M42B_3 = .;
    if M42B_4   = 9 then M42B_4 = .;
    if M42B_5   = 9 then M42B_5 = .;
    if M42B_6   = 9 then M42B_6 = .;
    if M42C_1   = 9 then M42C_1 = .;
    if M42C_2   = 9 then M42C_2 = .;
    if M42C_3   = 9 then M42C_3 = .;
    if M42C_4   = 9 then M42C_4 = .;
    if M42C_5   = 9 then M42C_5 = .;
    if M42C_6   = 9 then M42C_6 = .;
    if M42D_1   = 9 then M42D_1 = .;
    if M42D_2   = 9 then M42D_2 = .;
    if M42D_3   = 9 then M42D_3 = .;
    if M42D_4   = 9 then M42D_4 = .;
    if M42D_5   = 9 then M42D_5 = .;
    if M42D_6   = 9 then M42D_6 = .;
    if M42E_1   = 9 then M42E_1 = .;
    if M42E_2   = 9 then M42E_2 = .;
    if M42E_3   = 9 then M42E_3 = .;
    if M42E_4   = 9 then M42E_4 = .;
    if M42E_5   = 9 then M42E_5 = .;
    if M42E_6   = 9 then M42E_6 = .;
    if M43_1    = 9 then M43_1 = .;
    if M43_2    = 9 then M43_2 = .;
    if M43_3    = 9 then M43_3 = .;
    if M43_4    = 9 then M43_4 = .;
    if M43_5    = 9 then M43_5 = .;
    if M43_6    = 9 then M43_6 = .;
    if M44_1    = 9 then M44_1 = .;
    if M44_2    = 9 then M44_2 = .;
    if M44_3    = 9 then M44_3 = .;
    if M44_4    = 9 then M44_4 = .;
    if M44_5    = 9 then M44_5 = .;
    if M44_6    = 9 then M44_6 = .;
    if M45_1    = 9 then M45_1 = .;
    if M45_2    = 9 then M45_2 = .;
    if M45_3    = 9 then M45_3 = .;
    if M45_4    = 9 then M45_4 = .;
    if M45_5    = 9 then M45_5 = .;
    if M45_6    = 9 then M45_6 = .;
    if M46_1    = 999 then M46_1 = .;
    if M46_2    = 999 then M46_2 = .;
    if M46_3    = 999 then M46_3 = .;
    if M46_4    = 999 then M46_4 = .;
    if M46_5    = 999 then M46_5 = .;
    if M46_6    = 999 then M46_6 = .;
    if M47_1    = 9 then M47_1 = .;
    if M47_2    = 9 then M47_2 = .;
    if M47_3    = 9 then M47_3 = .;
    if M47_4    = 9 then M47_4 = .;
    if M47_5    = 9 then M47_5 = .;
    if M47_6    = 9 then M47_6 = .;
    if M48_1    = 9 then M48_1 = .;
    if M48_2    = 9 then M48_2 = .;
    if M48_3    = 9 then M48_3 = .;
    if M48_4    = 9 then M48_4 = .;
    if M48_5    = 9 then M48_5 = .;
    if M48_6    = 9 then M48_6 = .;
    if M49A_1   = 9 then M49A_1 = .;
    if M49A_2   = 9 then M49A_2 = .;
    if M49A_3   = 9 then M49A_3 = .;
    if M49A_4   = 9 then M49A_4 = .;
    if M49A_5   = 9 then M49A_5 = .;
    if M49A_6   = 9 then M49A_6 = .;
    if M49B_1   = 9 then M49B_1 = .;
    if M49B_2   = 9 then M49B_2 = .;
    if M49B_3   = 9 then M49B_3 = .;
    if M49B_4   = 9 then M49B_4 = .;
    if M49B_5   = 9 then M49B_5 = .;
    if M49B_6   = 9 then M49B_6 = .;
    if M49C_1   = 9 then M49C_1 = .;
    if M49C_2   = 9 then M49C_2 = .;
    if M49C_3   = 9 then M49C_3 = .;
    if M49C_4   = 9 then M49C_4 = .;
    if M49C_5   = 9 then M49C_5 = .;
    if M49C_6   = 9 then M49C_6 = .;
    if M49D_1   = 9 then M49D_1 = .;
    if M49D_2   = 9 then M49D_2 = .;
    if M49D_3   = 9 then M49D_3 = .;
    if M49D_4   = 9 then M49D_4 = .;
    if M49D_5   = 9 then M49D_5 = .;
    if M49D_6   = 9 then M49D_6 = .;
    if M49E_1   = 9 then M49E_1 = .;
    if M49E_2   = 9 then M49E_2 = .;
    if M49E_3   = 9 then M49E_3 = .;
    if M49E_4   = 9 then M49E_4 = .;
    if M49E_5   = 9 then M49E_5 = .;
    if M49E_6   = 9 then M49E_6 = .;
    if M49F_1   = 9 then M49F_1 = .;
    if M49F_2   = 9 then M49F_2 = .;
    if M49F_3   = 9 then M49F_3 = .;
    if M49F_4   = 9 then M49F_4 = .;
    if M49F_5   = 9 then M49F_5 = .;
    if M49F_6   = 9 then M49F_6 = .;
    if M49G_1   = 9 then M49G_1 = .;
    if M49G_2   = 9 then M49G_2 = .;
    if M49G_3   = 9 then M49G_3 = .;
    if M49G_4   = 9 then M49G_4 = .;
    if M49G_5   = 9 then M49G_5 = .;
    if M49G_6   = 9 then M49G_6 = .;
    if M49X_1   = 9 then M49X_1 = .;
    if M49X_2   = 9 then M49X_2 = .;
    if M49X_3   = 9 then M49X_3 = .;
    if M49X_4   = 9 then M49X_4 = .;
    if M49X_5   = 9 then M49X_5 = .;
    if M49X_6   = 9 then M49X_6 = .;
    if M49Z_1   = 9 then M49Z_1 = .;
    if M49Z_2   = 9 then M49Z_2 = .;
    if M49Z_3   = 9 then M49Z_3 = .;
    if M49Z_4   = 9 then M49Z_4 = .;
    if M49Z_5   = 9 then M49Z_5 = .;
    if M49Z_6   = 9 then M49Z_6 = .;
    if M49Y_1   = 9 then M49Y_1 = .;
    if M49Y_2   = 9 then M49Y_2 = .;
    if M49Y_3   = 9 then M49Y_3 = .;
    if M49Y_4   = 9 then M49Y_4 = .;
    if M49Y_5   = 9 then M49Y_5 = .;
    if M49Y_6   = 9 then M49Y_6 = .;
    if M54_1    = 9 then M54_1 = .;
    if M54_2    = 9 then M54_2 = .;
    if M54_3    = 9 then M54_3 = .;
    if M54_4    = 9 then M54_4 = .;
    if M54_5    = 9 then M54_5 = .;
    if M54_6    = 9 then M54_6 = .;
    if M55_1    = 9 then M55_1 = .;
    if M55_2    = 9 then M55_2 = .;
    if M55_3    = 9 then M55_3 = .;
    if M55_4    = 9 then M55_4 = .;
    if M55_5    = 9 then M55_5 = .;
    if M55_6    = 9 then M55_6 = .;
    if M55A_1   = 9 then M55A_1 = .;
    if M55A_2   = 9 then M55A_2 = .;
    if M55A_3   = 9 then M55A_3 = .;
    if M55A_4   = 9 then M55A_4 = .;
    if M55A_5   = 9 then M55A_5 = .;
    if M55A_6   = 9 then M55A_6 = .;
    if M55B_1   = 9 then M55B_1 = .;
    if M55B_2   = 9 then M55B_2 = .;
    if M55B_3   = 9 then M55B_3 = .;
    if M55B_4   = 9 then M55B_4 = .;
    if M55B_5   = 9 then M55B_5 = .;
    if M55B_6   = 9 then M55B_6 = .;
    if M55C_1   = 9 then M55C_1 = .;
    if M55C_2   = 9 then M55C_2 = .;
    if M55C_3   = 9 then M55C_3 = .;
    if M55C_4   = 9 then M55C_4 = .;
    if M55C_5   = 9 then M55C_5 = .;
    if M55C_6   = 9 then M55C_6 = .;
    if M55D_1   = 9 then M55D_1 = .;
    if M55D_2   = 9 then M55D_2 = .;
    if M55D_3   = 9 then M55D_3 = .;
    if M55D_4   = 9 then M55D_4 = .;
    if M55D_5   = 9 then M55D_5 = .;
    if M55D_6   = 9 then M55D_6 = .;
    if M55E_1   = 9 then M55E_1 = .;
    if M55E_2   = 9 then M55E_2 = .;
    if M55E_3   = 9 then M55E_3 = .;
    if M55E_4   = 9 then M55E_4 = .;
    if M55E_5   = 9 then M55E_5 = .;
    if M55E_6   = 9 then M55E_6 = .;
    if M55F_1   = 9 then M55F_1 = .;
    if M55F_2   = 9 then M55F_2 = .;
    if M55F_3   = 9 then M55F_3 = .;
    if M55F_4   = 9 then M55F_4 = .;
    if M55F_5   = 9 then M55F_5 = .;
    if M55F_6   = 9 then M55F_6 = .;
    if M55G_1   = 9 then M55G_1 = .;
    if M55G_2   = 9 then M55G_2 = .;
    if M55G_3   = 9 then M55G_3 = .;
    if M55G_4   = 9 then M55G_4 = .;
    if M55G_5   = 9 then M55G_5 = .;
    if M55G_6   = 9 then M55G_6 = .;
    if M55H_1   = 9 then M55H_1 = .;
    if M55H_2   = 9 then M55H_2 = .;
    if M55H_3   = 9 then M55H_3 = .;
    if M55H_4   = 9 then M55H_4 = .;
    if M55H_5   = 9 then M55H_5 = .;
    if M55H_6   = 9 then M55H_6 = .;
    if M55I_1   = 9 then M55I_1 = .;
    if M55I_2   = 9 then M55I_2 = .;
    if M55I_3   = 9 then M55I_3 = .;
    if M55I_4   = 9 then M55I_4 = .;
    if M55I_5   = 9 then M55I_5 = .;
    if M55I_6   = 9 then M55I_6 = .;
    if M55J_1   = 9 then M55J_1 = .;
    if M55J_2   = 9 then M55J_2 = .;
    if M55J_3   = 9 then M55J_3 = .;
    if M55J_4   = 9 then M55J_4 = .;
    if M55J_5   = 9 then M55J_5 = .;
    if M55J_6   = 9 then M55J_6 = .;
    if M55K_1   = 9 then M55K_1 = .;
    if M55K_2   = 9 then M55K_2 = .;
    if M55K_3   = 9 then M55K_3 = .;
    if M55K_4   = 9 then M55K_4 = .;
    if M55K_5   = 9 then M55K_5 = .;
    if M55K_6   = 9 then M55K_6 = .;
    if M55L_1   = 9 then M55L_1 = .;
    if M55L_2   = 9 then M55L_2 = .;
    if M55L_3   = 9 then M55L_3 = .;
    if M55L_4   = 9 then M55L_4 = .;
    if M55L_5   = 9 then M55L_5 = .;
    if M55L_6   = 9 then M55L_6 = .;
    if M55M_1   = 9 then M55M_1 = .;
    if M55M_2   = 9 then M55M_2 = .;
    if M55M_3   = 9 then M55M_3 = .;
    if M55M_4   = 9 then M55M_4 = .;
    if M55M_5   = 9 then M55M_5 = .;
    if M55M_6   = 9 then M55M_6 = .;
    if M55N_1   = 9 then M55N_1 = .;
    if M55N_2   = 9 then M55N_2 = .;
    if M55N_3   = 9 then M55N_3 = .;
    if M55N_4   = 9 then M55N_4 = .;
    if M55N_5   = 9 then M55N_5 = .;
    if M55N_6   = 9 then M55N_6 = .;
    if M55O_1   = 9 then M55O_1 = .;
    if M55O_2   = 9 then M55O_2 = .;
    if M55O_3   = 9 then M55O_3 = .;
    if M55O_4   = 9 then M55O_4 = .;
    if M55O_5   = 9 then M55O_5 = .;
    if M55O_6   = 9 then M55O_6 = .;
    if M55X_1   = 9 then M55X_1 = .;
    if M55X_2   = 9 then M55X_2 = .;
    if M55X_3   = 9 then M55X_3 = .;
    if M55X_4   = 9 then M55X_4 = .;
    if M55X_5   = 9 then M55X_5 = .;
    if M55X_6   = 9 then M55X_6 = .;
    if M55Z_1   = 9 then M55Z_1 = .;
    if M55Z_2   = 9 then M55Z_2 = .;
    if M55Z_3   = 9 then M55Z_3 = .;
    if M55Z_4   = 9 then M55Z_4 = .;
    if M55Z_5   = 9 then M55Z_5 = .;
    if M55Z_6   = 9 then M55Z_6 = .;
    if M57A_1   = 9 then M57A_1 = .;
    if M57A_2   = 9 then M57A_2 = .;
    if M57A_3   = 9 then M57A_3 = .;
    if M57A_4   = 9 then M57A_4 = .;
    if M57A_5   = 9 then M57A_5 = .;
    if M57A_6   = 9 then M57A_6 = .;
    if M57B_1   = 9 then M57B_1 = .;
    if M57B_2   = 9 then M57B_2 = .;
    if M57B_3   = 9 then M57B_3 = .;
    if M57B_4   = 9 then M57B_4 = .;
    if M57B_5   = 9 then M57B_5 = .;
    if M57B_6   = 9 then M57B_6 = .;
    if M57C_1   = 9 then M57C_1 = .;
    if M57C_2   = 9 then M57C_2 = .;
    if M57C_3   = 9 then M57C_3 = .;
    if M57C_4   = 9 then M57C_4 = .;
    if M57C_5   = 9 then M57C_5 = .;
    if M57C_6   = 9 then M57C_6 = .;
    if M57D_1   = 9 then M57D_1 = .;
    if M57D_2   = 9 then M57D_2 = .;
    if M57D_3   = 9 then M57D_3 = .;
    if M57D_4   = 9 then M57D_4 = .;
    if M57D_5   = 9 then M57D_5 = .;
    if M57D_6   = 9 then M57D_6 = .;
    if M57E_1   = 9 then M57E_1 = .;
    if M57E_2   = 9 then M57E_2 = .;
    if M57E_3   = 9 then M57E_3 = .;
    if M57E_4   = 9 then M57E_4 = .;
    if M57E_5   = 9 then M57E_5 = .;
    if M57E_6   = 9 then M57E_6 = .;
    if M57F_1   = 9 then M57F_1 = .;
    if M57F_2   = 9 then M57F_2 = .;
    if M57F_3   = 9 then M57F_3 = .;
    if M57F_4   = 9 then M57F_4 = .;
    if M57F_5   = 9 then M57F_5 = .;
    if M57F_6   = 9 then M57F_6 = .;
    if M57G_1   = 9 then M57G_1 = .;
    if M57G_2   = 9 then M57G_2 = .;
    if M57G_3   = 9 then M57G_3 = .;
    if M57G_4   = 9 then M57G_4 = .;
    if M57G_5   = 9 then M57G_5 = .;
    if M57G_6   = 9 then M57G_6 = .;
    if M57H_1   = 9 then M57H_1 = .;
    if M57H_2   = 9 then M57H_2 = .;
    if M57H_3   = 9 then M57H_3 = .;
    if M57H_4   = 9 then M57H_4 = .;
    if M57H_5   = 9 then M57H_5 = .;
    if M57H_6   = 9 then M57H_6 = .;
    if M57I_1   = 9 then M57I_1 = .;
    if M57I_2   = 9 then M57I_2 = .;
    if M57I_3   = 9 then M57I_3 = .;
    if M57I_4   = 9 then M57I_4 = .;
    if M57I_5   = 9 then M57I_5 = .;
    if M57I_6   = 9 then M57I_6 = .;
    if M57J_1   = 9 then M57J_1 = .;
    if M57J_2   = 9 then M57J_2 = .;
    if M57J_3   = 9 then M57J_3 = .;
    if M57J_4   = 9 then M57J_4 = .;
    if M57J_5   = 9 then M57J_5 = .;
    if M57J_6   = 9 then M57J_6 = .;
    if M57K_1   = 9 then M57K_1 = .;
    if M57K_2   = 9 then M57K_2 = .;
    if M57K_3   = 9 then M57K_3 = .;
    if M57K_4   = 9 then M57K_4 = .;
    if M57K_5   = 9 then M57K_5 = .;
    if M57K_6   = 9 then M57K_6 = .;
    if M57L_1   = 9 then M57L_1 = .;
    if M57L_2   = 9 then M57L_2 = .;
    if M57L_3   = 9 then M57L_3 = .;
    if M57L_4   = 9 then M57L_4 = .;
    if M57L_5   = 9 then M57L_5 = .;
    if M57L_6   = 9 then M57L_6 = .;
    if M57M_1   = 9 then M57M_1 = .;
    if M57M_2   = 9 then M57M_2 = .;
    if M57M_3   = 9 then M57M_3 = .;
    if M57M_4   = 9 then M57M_4 = .;
    if M57M_5   = 9 then M57M_5 = .;
    if M57M_6   = 9 then M57M_6 = .;
    if M57N_1   = 9 then M57N_1 = .;
    if M57N_2   = 9 then M57N_2 = .;
    if M57N_3   = 9 then M57N_3 = .;
    if M57N_4   = 9 then M57N_4 = .;
    if M57N_5   = 9 then M57N_5 = .;
    if M57N_6   = 9 then M57N_6 = .;
    if M57O_1   = 9 then M57O_1 = .;
    if M57O_2   = 9 then M57O_2 = .;
    if M57O_3   = 9 then M57O_3 = .;
    if M57O_4   = 9 then M57O_4 = .;
    if M57O_5   = 9 then M57O_5 = .;
    if M57O_6   = 9 then M57O_6 = .;
    if M57P_1   = 9 then M57P_1 = .;
    if M57P_2   = 9 then M57P_2 = .;
    if M57P_3   = 9 then M57P_3 = .;
    if M57P_4   = 9 then M57P_4 = .;
    if M57P_5   = 9 then M57P_5 = .;
    if M57P_6   = 9 then M57P_6 = .;
    if M57Q_1   = 9 then M57Q_1 = .;
    if M57Q_2   = 9 then M57Q_2 = .;
    if M57Q_3   = 9 then M57Q_3 = .;
    if M57Q_4   = 9 then M57Q_4 = .;
    if M57Q_5   = 9 then M57Q_5 = .;
    if M57Q_6   = 9 then M57Q_6 = .;
    if M57R_1   = 9 then M57R_1 = .;
    if M57R_2   = 9 then M57R_2 = .;
    if M57R_3   = 9 then M57R_3 = .;
    if M57R_4   = 9 then M57R_4 = .;
    if M57R_5   = 9 then M57R_5 = .;
    if M57R_6   = 9 then M57R_6 = .;
    if M57S_1   = 9 then M57S_1 = .;
    if M57S_2   = 9 then M57S_2 = .;
    if M57S_3   = 9 then M57S_3 = .;
    if M57S_4   = 9 then M57S_4 = .;
    if M57S_5   = 9 then M57S_5 = .;
    if M57S_6   = 9 then M57S_6 = .;
    if M57T_1   = 9 then M57T_1 = .;
    if M57T_2   = 9 then M57T_2 = .;
    if M57T_3   = 9 then M57T_3 = .;
    if M57T_4   = 9 then M57T_4 = .;
    if M57T_5   = 9 then M57T_5 = .;
    if M57T_6   = 9 then M57T_6 = .;
    if M57U_1   = 9 then M57U_1 = .;
    if M57U_2   = 9 then M57U_2 = .;
    if M57U_3   = 9 then M57U_3 = .;
    if M57U_4   = 9 then M57U_4 = .;
    if M57U_5   = 9 then M57U_5 = .;
    if M57U_6   = 9 then M57U_6 = .;
    if M57V_1   = 9 then M57V_1 = .;
    if M57V_2   = 9 then M57V_2 = .;
    if M57V_3   = 9 then M57V_3 = .;
    if M57V_4   = 9 then M57V_4 = .;
    if M57V_5   = 9 then M57V_5 = .;
    if M57V_6   = 9 then M57V_6 = .;
    if M57X_1   = 9 then M57X_1 = .;
    if M57X_2   = 9 then M57X_2 = .;
    if M57X_3   = 9 then M57X_3 = .;
    if M57X_4   = 9 then M57X_4 = .;
    if M57X_5   = 9 then M57X_5 = .;
    if M57X_6   = 9 then M57X_6 = .;
    if M60_1    = 9 then M60_1 = .;
    if M60_2    = 9 then M60_2 = .;
    if M60_3    = 9 then M60_3 = .;
    if M60_4    = 9 then M60_4 = .;
    if M60_5    = 9 then M60_5 = .;
    if M60_6    = 9 then M60_6 = .;
    if M61_1    = 999 then M61_1 = .;
    if M61_2    = 999 then M61_2 = .;
    if M61_3    = 999 then M61_3 = .;
    if M61_4    = 999 then M61_4 = .;
    if M61_5    = 999 then M61_5 = .;
    if M61_6    = 999 then M61_6 = .;
    if M62_1    = 9 then M62_1 = .;
    if M62_2    = 9 then M62_2 = .;
    if M62_3    = 9 then M62_3 = .;
    if M62_4    = 9 then M62_4 = .;
    if M62_5    = 9 then M62_5 = .;
    if M62_6    = 9 then M62_6 = .;
    if M63_1    = 999 then M63_1 = .;
    if M63_2    = 999 then M63_2 = .;
    if M63_3    = 999 then M63_3 = .;
    if M63_4    = 999 then M63_4 = .;
    if M63_5    = 999 then M63_5 = .;
    if M63_6    = 999 then M63_6 = .;
    if M64_1    = 99 then M64_1 = .;
    if M64_2    = 99 then M64_2 = .;
    if M64_3    = 99 then M64_3 = .;
    if M64_4    = 99 then M64_4 = .;
    if M64_5    = 99 then M64_5 = .;
    if M64_6    = 99 then M64_6 = .;
    if M65A_1   = 9 then M65A_1 = .;
    if M65A_2   = 9 then M65A_2 = .;
    if M65A_3   = 9 then M65A_3 = .;
    if M65A_4   = 9 then M65A_4 = .;
    if M65A_5   = 9 then M65A_5 = .;
    if M65A_6   = 9 then M65A_6 = .;
    if M65B_1   = 9 then M65B_1 = .;
    if M65B_2   = 9 then M65B_2 = .;
    if M65B_3   = 9 then M65B_3 = .;
    if M65B_4   = 9 then M65B_4 = .;
    if M65B_5   = 9 then M65B_5 = .;
    if M65B_6   = 9 then M65B_6 = .;
    if M65C_1   = 9 then M65C_1 = .;
    if M65C_2   = 9 then M65C_2 = .;
    if M65C_3   = 9 then M65C_3 = .;
    if M65C_4   = 9 then M65C_4 = .;
    if M65C_5   = 9 then M65C_5 = .;
    if M65C_6   = 9 then M65C_6 = .;
    if M65D_1   = 9 then M65D_1 = .;
    if M65D_2   = 9 then M65D_2 = .;
    if M65D_3   = 9 then M65D_3 = .;
    if M65D_4   = 9 then M65D_4 = .;
    if M65D_5   = 9 then M65D_5 = .;
    if M65D_6   = 9 then M65D_6 = .;
    if M65E_1   = 9 then M65E_1 = .;
    if M65E_2   = 9 then M65E_2 = .;
    if M65E_3   = 9 then M65E_3 = .;
    if M65E_4   = 9 then M65E_4 = .;
    if M65E_5   = 9 then M65E_5 = .;
    if M65E_6   = 9 then M65E_6 = .;
    if M65F_1   = 9 then M65F_1 = .;
    if M65F_2   = 9 then M65F_2 = .;
    if M65F_3   = 9 then M65F_3 = .;
    if M65F_4   = 9 then M65F_4 = .;
    if M65F_5   = 9 then M65F_5 = .;
    if M65F_6   = 9 then M65F_6 = .;
    if M65G_1   = 9 then M65G_1 = .;
    if M65G_2   = 9 then M65G_2 = .;
    if M65G_3   = 9 then M65G_3 = .;
    if M65G_4   = 9 then M65G_4 = .;
    if M65G_5   = 9 then M65G_5 = .;
    if M65G_6   = 9 then M65G_6 = .;
    if M65H_1   = 9 then M65H_1 = .;
    if M65H_2   = 9 then M65H_2 = .;
    if M65H_3   = 9 then M65H_3 = .;
    if M65H_4   = 9 then M65H_4 = .;
    if M65H_5   = 9 then M65H_5 = .;
    if M65H_6   = 9 then M65H_6 = .;
    if M65I_1   = 9 then M65I_1 = .;
    if M65I_2   = 9 then M65I_2 = .;
    if M65I_3   = 9 then M65I_3 = .;
    if M65I_4   = 9 then M65I_4 = .;
    if M65I_5   = 9 then M65I_5 = .;
    if M65I_6   = 9 then M65I_6 = .;
    if M65J_1   = 9 then M65J_1 = .;
    if M65J_2   = 9 then M65J_2 = .;
    if M65J_3   = 9 then M65J_3 = .;
    if M65J_4   = 9 then M65J_4 = .;
    if M65J_5   = 9 then M65J_5 = .;
    if M65J_6   = 9 then M65J_6 = .;
    if M65K_1   = 9 then M65K_1 = .;
    if M65K_2   = 9 then M65K_2 = .;
    if M65K_3   = 9 then M65K_3 = .;
    if M65K_4   = 9 then M65K_4 = .;
    if M65K_5   = 9 then M65K_5 = .;
    if M65K_6   = 9 then M65K_6 = .;
    if M65L_1   = 9 then M65L_1 = .;
    if M65L_2   = 9 then M65L_2 = .;
    if M65L_3   = 9 then M65L_3 = .;
    if M65L_4   = 9 then M65L_4 = .;
    if M65L_5   = 9 then M65L_5 = .;
    if M65L_6   = 9 then M65L_6 = .;
    if M65X_1   = 9 then M65X_1 = .;
    if M65X_2   = 9 then M65X_2 = .;
    if M65X_3   = 9 then M65X_3 = .;
    if M65X_4   = 9 then M65X_4 = .;
    if M65X_5   = 9 then M65X_5 = .;
    if M65X_6   = 9 then M65X_6 = .;
    if M66_1    = 9 then M66_1 = .;
    if M66_2    = 9 then M66_2 = .;
    if M66_3    = 9 then M66_3 = .;
    if M66_4    = 9 then M66_4 = .;
    if M66_5    = 9 then M66_5 = .;
    if M66_6    = 9 then M66_6 = .;
    if M67_1    = 999 then M67_1 = .;
    if M67_2    = 999 then M67_2 = .;
    if M67_3    = 999 then M67_3 = .;
    if M67_4    = 999 then M67_4 = .;
    if M67_5    = 999 then M67_5 = .;
    if M67_6    = 999 then M67_6 = .;
    if M68_1    = 99 then M68_1 = .;
    if M68_2    = 99 then M68_2 = .;
    if M68_3    = 99 then M68_3 = .;
    if M68_4    = 99 then M68_4 = .;
    if M68_5    = 99 then M68_5 = .;
    if M68_6    = 99 then M68_6 = .;
    if M69_1    = 99 then M69_1 = .;
    if M69_2    = 99 then M69_2 = .;
    if M69_3    = 99 then M69_3 = .;
    if M69_4    = 99 then M69_4 = .;
    if M69_5    = 99 then M69_5 = .;
    if M69_6    = 99 then M69_6 = .;
    if M70_1    = 9 then M70_1 = .;
    if M70_2    = 9 then M70_2 = .;
    if M70_3    = 9 then M70_3 = .;
    if M70_4    = 9 then M70_4 = .;
    if M70_5    = 9 then M70_5 = .;
    if M70_6    = 9 then M70_6 = .;
    if M71_1    = 999 then M71_1 = .;
    if M71_2    = 999 then M71_2 = .;
    if M71_3    = 999 then M71_3 = .;
    if M71_4    = 999 then M71_4 = .;
    if M71_5    = 999 then M71_5 = .;
    if M71_6    = 999 then M71_6 = .;
    if M72_1    = 99 then M72_1 = .;
    if M72_2    = 99 then M72_2 = .;
    if M72_3    = 99 then M72_3 = .;
    if M72_4    = 99 then M72_4 = .;
    if M72_5    = 99 then M72_5 = .;
    if M72_6    = 99 then M72_6 = .;
    if M73_1    = 99 then M73_1 = .;
    if M73_2    = 99 then M73_2 = .;
    if M73_3    = 99 then M73_3 = .;
    if M73_4    = 99 then M73_4 = .;
    if M73_5    = 99 then M73_5 = .;
    if M73_6    = 99 then M73_6 = .;
    if M74_1    = 9 then M74_1 = .;
    if M74_2    = 9 then M74_2 = .;
    if M74_3    = 9 then M74_3 = .;
    if M74_4    = 9 then M74_4 = .;
    if M74_5    = 9 then M74_5 = .;
    if M74_6    = 9 then M74_6 = .;
    if M75_1    = 999 then M75_1 = .;
    if M75_2    = 999 then M75_2 = .;
    if M75_3    = 999 then M75_3 = .;
    if M75_4    = 999 then M75_4 = .;
    if M75_5    = 999 then M75_5 = .;
    if M75_6    = 999 then M75_6 = .;
    if M76_1    = 99 then M76_1 = .;
    if M76_2    = 99 then M76_2 = .;
    if M76_3    = 99 then M76_3 = .;
    if M76_4    = 99 then M76_4 = .;
    if M76_5    = 99 then M76_5 = .;
    if M76_6    = 99 then M76_6 = .;
    if M77_1    = 9 then M77_1 = .;
    if M77_2    = 9 then M77_2 = .;
    if M77_3    = 9 then M77_3 = .;
    if M77_4    = 9 then M77_4 = .;
    if M77_5    = 9 then M77_5 = .;
    if M77_6    = 9 then M77_6 = .;
    if M78A_1   = 9 then M78A_1 = .;
    if M78A_2   = 9 then M78A_2 = .;
    if M78A_3   = 9 then M78A_3 = .;
    if M78A_4   = 9 then M78A_4 = .;
    if M78A_5   = 9 then M78A_5 = .;
    if M78A_6   = 9 then M78A_6 = .;
    if M78B_1   = 9 then M78B_1 = .;
    if M78B_2   = 9 then M78B_2 = .;
    if M78B_3   = 9 then M78B_3 = .;
    if M78B_4   = 9 then M78B_4 = .;
    if M78B_5   = 9 then M78B_5 = .;
    if M78B_6   = 9 then M78B_6 = .;
    if M78C_1   = 9 then M78C_1 = .;
    if M78C_2   = 9 then M78C_2 = .;
    if M78C_3   = 9 then M78C_3 = .;
    if M78C_4   = 9 then M78C_4 = .;
    if M78C_5   = 9 then M78C_5 = .;
    if M78C_6   = 9 then M78C_6 = .;
    if M78D_1   = 9 then M78D_1 = .;
    if M78D_2   = 9 then M78D_2 = .;
    if M78D_3   = 9 then M78D_3 = .;
    if M78D_4   = 9 then M78D_4 = .;
    if M78D_5   = 9 then M78D_5 = .;
    if M78D_6   = 9 then M78D_6 = .;
    if M78E_1   = 9 then M78E_1 = .;
    if M78E_2   = 9 then M78E_2 = .;
    if M78E_3   = 9 then M78E_3 = .;
    if M78E_4   = 9 then M78E_4 = .;
    if M78E_5   = 9 then M78E_5 = .;
    if M78E_6   = 9 then M78E_6 = .;
    if M78F_1   = 9 then M78F_1 = .;
    if M78F_2   = 9 then M78F_2 = .;
    if M78F_3   = 9 then M78F_3 = .;
    if M78F_4   = 9 then M78F_4 = .;
    if M78F_5   = 9 then M78F_5 = .;
    if M78F_6   = 9 then M78F_6 = .;
    if M78G_1   = 9 then M78G_1 = .;
    if M78G_2   = 9 then M78G_2 = .;
    if M78G_3   = 9 then M78G_3 = .;
    if M78G_4   = 9 then M78G_4 = .;
    if M78G_5   = 9 then M78G_5 = .;
    if M78G_6   = 9 then M78G_6 = .;
    if M78H_1   = 9 then M78H_1 = .;
    if M78H_2   = 9 then M78H_2 = .;
    if M78H_3   = 9 then M78H_3 = .;
    if M78H_4   = 9 then M78H_4 = .;
    if M78H_5   = 9 then M78H_5 = .;
    if M78H_6   = 9 then M78H_6 = .;
    if M78I_1   = 9 then M78I_1 = .;
    if M78I_2   = 9 then M78I_2 = .;
    if M78I_3   = 9 then M78I_3 = .;
    if M78I_4   = 9 then M78I_4 = .;
    if M78I_5   = 9 then M78I_5 = .;
    if M78I_6   = 9 then M78I_6 = .;
    if M78J_1   = 9 then M78J_1 = .;
    if M78J_2   = 9 then M78J_2 = .;
    if M78J_3   = 9 then M78J_3 = .;
    if M78J_4   = 9 then M78J_4 = .;
    if M78J_5   = 9 then M78J_5 = .;
    if M78J_6   = 9 then M78J_6 = .;
    if V401     = 9 then V401 = .;
    if V407     = 99 then V407 = .;
    if V408     = 99 then V408 = .;
    if V409     = 9 then V409 = .;
    if V409A    = 9 then V409A = .;
    if V410     = 9 then V410 = .;
    if V410A    = 9 then V410A = .;
    if V411     = 9 then V411 = .;
    if V411A    = 9 then V411A = .;
    if V412     = 9 then V412 = .;
    if V412A    = 9 then V412A = .;
    if V412B    = 9 then V412B = .;
    if V412C    = 9 then V412C = .;
    if V413     = 9 then V413 = .;
    if V413A    = 9 then V413A = .;
    if V413B    = 9 then V413B = .;
    if V413C    = 9 then V413C = .;
    if V413D    = 9 then V413D = .;
    if V414A    = 9 then V414A = .;
    if V414B    = 9 then V414B = .;
    if V414C    = 9 then V414C = .;
    if V414D    = 9 then V414D = .;
    if V414E    = 9 then V414E = .;
    if V414F    = 9 then V414F = .;
    if V414G    = 9 then V414G = .;
    if V414H    = 9 then V414H = .;
    if V414I    = 9 then V414I = .;
    if V414J    = 9 then V414J = .;
    if V414K    = 9 then V414K = .;
    if V414L    = 9 then V414L = .;
    if V414M    = 9 then V414M = .;
    if V414N    = 9 then V414N = .;
    if V414O    = 9 then V414O = .;
    if V414P    = 9 then V414P = .;
    if V414Q    = 9 then V414Q = .;
    if V414R    = 9 then V414R = .;
    if V414S    = 9 then V414S = .;
    if V414T    = 9 then V414T = .;
    if V414U    = 9 then V414U = .;
    if V414V    = 9 then V414V = .;
    if V414W    = 9 then V414W = .;
    if V415     = 9 then V415 = .;
    if V416     = 9 then V416 = .;
    if V426     = 999 then V426 = .;
    if V437     = 9999 then V437 = .;
    if V438     = 9999 then V438 = .;
    if V439     = 9999 then V439 = .;
    if V440     = 9999 then V440 = .;
    if V441     = 99999 then V441 = .;
    if V442     = 99999 then V442 = .;
    if V443     = 99999 then V443 = .;
    if V444     = 99999 then V444 = .;
    if V444A    = 9999 then V444A = .;
    if V445     = 9999 then V445 = .;
    if V446     = 9999 then V446 = .;
    if V447     = 9 then V447 = .;
    if V452A    = 9 then V452A = .;
    if V452B    = 99 then V452B = .;
    if V452C    = 9 then V452C = .;
    if V453     = 999 then V453 = .;
    if V454     = 9 then V454 = .;
    if V455     = 9 then V455 = .;
    if V456     = 999 then V456 = .;
    if V457     = 9 then V457 = .;
    if V458     = 9 then V458 = .;
    if V459     = 9 then V459 = .;
    if V460     = 9 then V460 = .;
    if V461     = 9 then V461 = .;
    if V462     = 9 then V462 = .;
    if V463A    = 9 then V463A = .;
    if V463B    = 9 then V463B = .;
    if V463C    = 9 then V463C = .;
    if V463D    = 9 then V463D = .;
    if V463E    = 9 then V463E = .;
    if V463F    = 9 then V463F = .;
    if V463G    = 9 then V463G = .;
    if V463H    = 9 then V463H = .;
    if V463I    = 9 then V463I = .;
    if V463J    = 9 then V463J = .;
    if V463K    = 9 then V463K = .;
    if V463L    = 9 then V463L = .;
    if V463X    = 9 then V463X = .;
    if V463Z    = 9 then V463Z = .;
    if V463AA   = 9 then V463AA = .;
    if V463AB   = 9 then V463AB = .;
    if V464     = 99 then V464 = .;
    if V465     = 99 then V465 = .;
    if V466     = 9 then V466 = .;
    if V467A    = 9 then V467A = .;
    if V467B    = 9 then V467B = .;
    if V467C    = 9 then V467C = .;
    if V467D    = 9 then V467D = .;
    if V467E    = 9 then V467E = .;
    if V467F    = 9 then V467F = .;
    if V467G    = 9 then V467G = .;
    if V467H    = 9 then V467H = .;
    if V467I    = 9 then V467I = .;
    if V467J    = 9 then V467J = .;
    if V467K    = 9 then V467K = .;
    if V467L    = 9 then V467L = .;
    if V467M    = 9 then V467M = .;
    if V469E    = 9 then V469E = .;
    if V469F    = 9 then V469F = .;
    if V469X    = 9 then V469X = .;
    if V471A    = 9 then V471A = .;
    if V471B    = 9 then V471B = .;
    if V471C    = 9 then V471C = .;
    if V471D    = 9 then V471D = .;
    if V471E    = 9 then V471E = .;
    if V471F    = 9 then V471F = .;
    if V471G    = 9 then V471G = .;
    if V472A    = 9 then V472A = .;
    if V472B    = 9 then V472B = .;
    if V472C    = 9 then V472C = .;
    if V472D    = 9 then V472D = .;
    if V472E    = 9 then V472E = .;
    if V472F    = 9 then V472F = .;
    if V472G    = 9 then V472G = .;
    if V472H    = 9 then V472H = .;
    if V472I    = 9 then V472I = .;
    if V472J    = 9 then V472J = .;
    if V472K    = 9 then V472K = .;
    if V472L    = 9 then V472L = .;
    if V472M    = 9 then V472M = .;
    if V472N    = 9 then V472N = .;
    if V472O    = 9 then V472O = .;
    if V472P    = 9 then V472P = .;
    if V472Q    = 9 then V472Q = .;
    if V472R    = 9 then V472R = .;
    if V472S    = 9 then V472S = .;
    if V472T    = 9 then V472T = .;
    if V472U    = 9 then V472U = .;
    if V473A    = 9 then V473A = .;
    if V473B    = 9 then V473B = .;
    if V474     = 9 then V474 = .;
    if V474A    = 9 then V474A = .;
    if V474B    = 9 then V474B = .;
    if V474C    = 9 then V474C = .;
    if V474D    = 9 then V474D = .;
    if V474E    = 9 then V474E = .;
    if V474F    = 9 then V474F = .;
    if V474G    = 9 then V474G = .;
    if V474H    = 9 then V474H = .;
    if V474I    = 9 then V474I = .;
    if V474J    = 9 then V474J = .;
    if V474X    = 9 then V474X = .;
    if V474Z    = 9 then V474Z = .;
    if V475     = 9 then V475 = .;
    if V476     = 9 then V476 = .;
    if V477     = 99 then V477 = .;
    if V478     = 99 then V478 = .;
    if V479     = 99 then V479 = .;
    if V480     = 9 then V480 = .;
    if V481     = 9 then V481 = .;
    if V481A    = 9 then V481A = .;
    if V481B    = 9 then V481B = .;
    if V481C    = 9 then V481C = .;
    if V481D    = 9 then V481D = .;
    if V481E    = 9 then V481E = .;
    if V481F    = 9 then V481F = .;
    if V481G    = 9 then V481G = .;
    if V481H    = 9 then V481H = .;
    if V481X    = 9 then V481X = .;
    if V482A    = 9 then V482A = .;
    if V482B    = 9 then V482B = .;
    if V482C    = 9 then V482C = .;
    if H1_1     = 9 then H1_1 = .;
    if H1_2     = 9 then H1_2 = .;
    if H1_3     = 9 then H1_3 = .;
    if H1_4     = 9 then H1_4 = .;
    if H1_5     = 9 then H1_5 = .;
    if H1_6     = 9 then H1_6 = .;
    if H1A_1    = 99 then H1A_1 = .;
    if H1A_2    = 99 then H1A_2 = .;
    if H1A_3    = 99 then H1A_3 = .;
    if H1A_4    = 99 then H1A_4 = .;
    if H1A_5    = 99 then H1A_5 = .;
    if H1A_6    = 99 then H1A_6 = .;
    if H2_1     = 9 then H2_1 = .;
    if H2_2     = 9 then H2_2 = .;
    if H2_3     = 9 then H2_3 = .;
    if H2_4     = 9 then H2_4 = .;
    if H2_5     = 9 then H2_5 = .;
    if H2_6     = 9 then H2_6 = .;
    if H2D_1    = 99 then H2D_1 = .;
    if H2D_2    = 99 then H2D_2 = .;
    if H2D_3    = 99 then H2D_3 = .;
    if H2D_4    = 99 then H2D_4 = .;
    if H2D_5    = 99 then H2D_5 = .;
    if H2D_6    = 99 then H2D_6 = .;
    if H2M_1    = 99 then H2M_1 = .;
    if H2M_2    = 99 then H2M_2 = .;
    if H2M_3    = 99 then H2M_3 = .;
    if H2M_4    = 99 then H2M_4 = .;
    if H2M_5    = 99 then H2M_5 = .;
    if H2M_6    = 99 then H2M_6 = .;
    if H2Y_1    = 9999 then H2Y_1 = .;
    if H2Y_2    = 9999 then H2Y_2 = .;
    if H2Y_3    = 9999 then H2Y_3 = .;
    if H2Y_4    = 9999 then H2Y_4 = .;
    if H2Y_5    = 9999 then H2Y_5 = .;
    if H2Y_6    = 9999 then H2Y_6 = .;
    if H3_1     = 9 then H3_1 = .;
    if H3_2     = 9 then H3_2 = .;
    if H3_3     = 9 then H3_3 = .;
    if H3_4     = 9 then H3_4 = .;
    if H3_5     = 9 then H3_5 = .;
    if H3_6     = 9 then H3_6 = .;
    if H3D_1    = 99 then H3D_1 = .;
    if H3D_2    = 99 then H3D_2 = .;
    if H3D_3    = 99 then H3D_3 = .;
    if H3D_4    = 99 then H3D_4 = .;
    if H3D_5    = 99 then H3D_5 = .;
    if H3D_6    = 99 then H3D_6 = .;
    if H3M_1    = 99 then H3M_1 = .;
    if H3M_2    = 99 then H3M_2 = .;
    if H3M_3    = 99 then H3M_3 = .;
    if H3M_4    = 99 then H3M_4 = .;
    if H3M_5    = 99 then H3M_5 = .;
    if H3M_6    = 99 then H3M_6 = .;
    if H3Y_1    = 9999 then H3Y_1 = .;
    if H3Y_2    = 9999 then H3Y_2 = .;
    if H3Y_3    = 9999 then H3Y_3 = .;
    if H3Y_4    = 9999 then H3Y_4 = .;
    if H3Y_5    = 9999 then H3Y_5 = .;
    if H3Y_6    = 9999 then H3Y_6 = .;
    if H4_1     = 9 then H4_1 = .;
    if H4_2     = 9 then H4_2 = .;
    if H4_3     = 9 then H4_3 = .;
    if H4_4     = 9 then H4_4 = .;
    if H4_5     = 9 then H4_5 = .;
    if H4_6     = 9 then H4_6 = .;
    if H4D_1    = 99 then H4D_1 = .;
    if H4D_2    = 99 then H4D_2 = .;
    if H4D_3    = 99 then H4D_3 = .;
    if H4D_4    = 99 then H4D_4 = .;
    if H4D_5    = 99 then H4D_5 = .;
    if H4D_6    = 99 then H4D_6 = .;
    if H4M_1    = 99 then H4M_1 = .;
    if H4M_2    = 99 then H4M_2 = .;
    if H4M_3    = 99 then H4M_3 = .;
    if H4M_4    = 99 then H4M_4 = .;
    if H4M_5    = 99 then H4M_5 = .;
    if H4M_6    = 99 then H4M_6 = .;
    if H4Y_1    = 9999 then H4Y_1 = .;
    if H4Y_2    = 9999 then H4Y_2 = .;
    if H4Y_3    = 9999 then H4Y_3 = .;
    if H4Y_4    = 9999 then H4Y_4 = .;
    if H4Y_5    = 9999 then H4Y_5 = .;
    if H4Y_6    = 9999 then H4Y_6 = .;
    if H5_1     = 9 then H5_1 = .;
    if H5_2     = 9 then H5_2 = .;
    if H5_3     = 9 then H5_3 = .;
    if H5_4     = 9 then H5_4 = .;
    if H5_5     = 9 then H5_5 = .;
    if H5_6     = 9 then H5_6 = .;
    if H5D_1    = 99 then H5D_1 = .;
    if H5D_2    = 99 then H5D_2 = .;
    if H5D_3    = 99 then H5D_3 = .;
    if H5D_4    = 99 then H5D_4 = .;
    if H5D_5    = 99 then H5D_5 = .;
    if H5D_6    = 99 then H5D_6 = .;
    if H5M_1    = 99 then H5M_1 = .;
    if H5M_2    = 99 then H5M_2 = .;
    if H5M_3    = 99 then H5M_3 = .;
    if H5M_4    = 99 then H5M_4 = .;
    if H5M_5    = 99 then H5M_5 = .;
    if H5M_6    = 99 then H5M_6 = .;
    if H5Y_1    = 9999 then H5Y_1 = .;
    if H5Y_2    = 9999 then H5Y_2 = .;
    if H5Y_3    = 9999 then H5Y_3 = .;
    if H5Y_4    = 9999 then H5Y_4 = .;
    if H5Y_5    = 9999 then H5Y_5 = .;
    if H5Y_6    = 9999 then H5Y_6 = .;
    if H6_1     = 9 then H6_1 = .;
    if H6_2     = 9 then H6_2 = .;
    if H6_3     = 9 then H6_3 = .;
    if H6_4     = 9 then H6_4 = .;
    if H6_5     = 9 then H6_5 = .;
    if H6_6     = 9 then H6_6 = .;
    if H6D_1    = 99 then H6D_1 = .;
    if H6D_2    = 99 then H6D_2 = .;
    if H6D_3    = 99 then H6D_3 = .;
    if H6D_4    = 99 then H6D_4 = .;
    if H6D_5    = 99 then H6D_5 = .;
    if H6D_6    = 99 then H6D_6 = .;
    if H6M_1    = 99 then H6M_1 = .;
    if H6M_2    = 99 then H6M_2 = .;
    if H6M_3    = 99 then H6M_3 = .;
    if H6M_4    = 99 then H6M_4 = .;
    if H6M_5    = 99 then H6M_5 = .;
    if H6M_6    = 99 then H6M_6 = .;
    if H6Y_1    = 9999 then H6Y_1 = .;
    if H6Y_2    = 9999 then H6Y_2 = .;
    if H6Y_3    = 9999 then H6Y_3 = .;
    if H6Y_4    = 9999 then H6Y_4 = .;
    if H6Y_5    = 9999 then H6Y_5 = .;
    if H6Y_6    = 9999 then H6Y_6 = .;
    if H7_1     = 9 then H7_1 = .;
    if H7_2     = 9 then H7_2 = .;
    if H7_3     = 9 then H7_3 = .;
    if H7_4     = 9 then H7_4 = .;
    if H7_5     = 9 then H7_5 = .;
    if H7_6     = 9 then H7_6 = .;
    if H7D_1    = 99 then H7D_1 = .;
    if H7D_2    = 99 then H7D_2 = .;
    if H7D_3    = 99 then H7D_3 = .;
    if H7D_4    = 99 then H7D_4 = .;
    if H7D_5    = 99 then H7D_5 = .;
    if H7D_6    = 99 then H7D_6 = .;
    if H7M_1    = 99 then H7M_1 = .;
    if H7M_2    = 99 then H7M_2 = .;
    if H7M_3    = 99 then H7M_3 = .;
    if H7M_4    = 99 then H7M_4 = .;
    if H7M_5    = 99 then H7M_5 = .;
    if H7M_6    = 99 then H7M_6 = .;
    if H7Y_1    = 9999 then H7Y_1 = .;
    if H7Y_2    = 9999 then H7Y_2 = .;
    if H7Y_3    = 9999 then H7Y_3 = .;
    if H7Y_4    = 9999 then H7Y_4 = .;
    if H7Y_5    = 9999 then H7Y_5 = .;
    if H7Y_6    = 9999 then H7Y_6 = .;
    if H8_1     = 9 then H8_1 = .;
    if H8_2     = 9 then H8_2 = .;
    if H8_3     = 9 then H8_3 = .;
    if H8_4     = 9 then H8_4 = .;
    if H8_5     = 9 then H8_5 = .;
    if H8_6     = 9 then H8_6 = .;
    if H8D_1    = 99 then H8D_1 = .;
    if H8D_2    = 99 then H8D_2 = .;
    if H8D_3    = 99 then H8D_3 = .;
    if H8D_4    = 99 then H8D_4 = .;
    if H8D_5    = 99 then H8D_5 = .;
    if H8D_6    = 99 then H8D_6 = .;
    if H8M_1    = 99 then H8M_1 = .;
    if H8M_2    = 99 then H8M_2 = .;
    if H8M_3    = 99 then H8M_3 = .;
    if H8M_4    = 99 then H8M_4 = .;
    if H8M_5    = 99 then H8M_5 = .;
    if H8M_6    = 99 then H8M_6 = .;
    if H8Y_1    = 9999 then H8Y_1 = .;
    if H8Y_2    = 9999 then H8Y_2 = .;
    if H8Y_3    = 9999 then H8Y_3 = .;
    if H8Y_4    = 9999 then H8Y_4 = .;
    if H8Y_5    = 9999 then H8Y_5 = .;
    if H8Y_6    = 9999 then H8Y_6 = .;
    if H9_1     = 9 then H9_1 = .;
    if H9_2     = 9 then H9_2 = .;
    if H9_3     = 9 then H9_3 = .;
    if H9_4     = 9 then H9_4 = .;
    if H9_5     = 9 then H9_5 = .;
    if H9_6     = 9 then H9_6 = .;
    if H9D_1    = 99 then H9D_1 = .;
    if H9D_2    = 99 then H9D_2 = .;
    if H9D_3    = 99 then H9D_3 = .;
    if H9D_4    = 99 then H9D_4 = .;
    if H9D_5    = 99 then H9D_5 = .;
    if H9D_6    = 99 then H9D_6 = .;
    if H9M_1    = 99 then H9M_1 = .;
    if H9M_2    = 99 then H9M_2 = .;
    if H9M_3    = 99 then H9M_3 = .;
    if H9M_4    = 99 then H9M_4 = .;
    if H9M_5    = 99 then H9M_5 = .;
    if H9M_6    = 99 then H9M_6 = .;
    if H9Y_1    = 9999 then H9Y_1 = .;
    if H9Y_2    = 9999 then H9Y_2 = .;
    if H9Y_3    = 9999 then H9Y_3 = .;
    if H9Y_4    = 9999 then H9Y_4 = .;
    if H9Y_5    = 9999 then H9Y_5 = .;
    if H9Y_6    = 9999 then H9Y_6 = .;
    if H9A_1    = 9 then H9A_1 = .;
    if H9A_2    = 9 then H9A_2 = .;
    if H9A_3    = 9 then H9A_3 = .;
    if H9A_4    = 9 then H9A_4 = .;
    if H9A_5    = 9 then H9A_5 = .;
    if H9A_6    = 9 then H9A_6 = .;
    if H9AD_1   = 99 then H9AD_1 = .;
    if H9AD_2   = 99 then H9AD_2 = .;
    if H9AD_3   = 99 then H9AD_3 = .;
    if H9AD_4   = 99 then H9AD_4 = .;
    if H9AD_5   = 99 then H9AD_5 = .;
    if H9AD_6   = 99 then H9AD_6 = .;
    if H9AM_1   = 99 then H9AM_1 = .;
    if H9AM_2   = 99 then H9AM_2 = .;
    if H9AM_3   = 99 then H9AM_3 = .;
    if H9AM_4   = 99 then H9AM_4 = .;
    if H9AM_5   = 99 then H9AM_5 = .;
    if H9AM_6   = 99 then H9AM_6 = .;
    if H9AY_1   = 9999 then H9AY_1 = .;
    if H9AY_2   = 9999 then H9AY_2 = .;
    if H9AY_3   = 9999 then H9AY_3 = .;
    if H9AY_4   = 9999 then H9AY_4 = .;
    if H9AY_5   = 9999 then H9AY_5 = .;
    if H9AY_6   = 9999 then H9AY_6 = .;
    if H0_1     = 9 then H0_1 = .;
    if H0_2     = 9 then H0_2 = .;
    if H0_3     = 9 then H0_3 = .;
    if H0_4     = 9 then H0_4 = .;
    if H0_5     = 9 then H0_5 = .;
    if H0_6     = 9 then H0_6 = .;
    if H0D_1    = 99 then H0D_1 = .;
    if H0D_2    = 99 then H0D_2 = .;
    if H0D_3    = 99 then H0D_3 = .;
    if H0D_4    = 99 then H0D_4 = .;
    if H0D_5    = 99 then H0D_5 = .;
    if H0D_6    = 99 then H0D_6 = .;
    if H0M_1    = 99 then H0M_1 = .;
    if H0M_2    = 99 then H0M_2 = .;
    if H0M_3    = 99 then H0M_3 = .;
    if H0M_4    = 99 then H0M_4 = .;
    if H0M_5    = 99 then H0M_5 = .;
    if H0M_6    = 99 then H0M_6 = .;
    if H0Y_1    = 9999 then H0Y_1 = .;
    if H0Y_2    = 9999 then H0Y_2 = .;
    if H0Y_3    = 9999 then H0Y_3 = .;
    if H0Y_4    = 9999 then H0Y_4 = .;
    if H0Y_5    = 9999 then H0Y_5 = .;
    if H0Y_6    = 9999 then H0Y_6 = .;
    if H10_1    = 9 then H10_1 = .;
    if H10_2    = 9 then H10_2 = .;
    if H10_3    = 9 then H10_3 = .;
    if H10_4    = 9 then H10_4 = .;
    if H10_5    = 9 then H10_5 = .;
    if H10_6    = 9 then H10_6 = .;
    if H33_1    = 9 then H33_1 = .;
    if H33_2    = 9 then H33_2 = .;
    if H33_3    = 9 then H33_3 = .;
    if H33_4    = 9 then H33_4 = .;
    if H33_5    = 9 then H33_5 = .;
    if H33_6    = 9 then H33_6 = .;
    if H33D_1   = 99 then H33D_1 = .;
    if H33D_2   = 99 then H33D_2 = .;
    if H33D_3   = 99 then H33D_3 = .;
    if H33D_4   = 99 then H33D_4 = .;
    if H33D_5   = 99 then H33D_5 = .;
    if H33D_6   = 99 then H33D_6 = .;
    if H33M_1   = 99 then H33M_1 = .;
    if H33M_2   = 99 then H33M_2 = .;
    if H33M_3   = 99 then H33M_3 = .;
    if H33M_4   = 99 then H33M_4 = .;
    if H33M_5   = 99 then H33M_5 = .;
    if H33M_6   = 99 then H33M_6 = .;
    if H33Y_1   = 9999 then H33Y_1 = .;
    if H33Y_2   = 9999 then H33Y_2 = .;
    if H33Y_3   = 9999 then H33Y_3 = .;
    if H33Y_4   = 9999 then H33Y_4 = .;
    if H33Y_5   = 9999 then H33Y_5 = .;
    if H33Y_6   = 9999 then H33Y_6 = .;
    if H35_1    = 9 then H35_1 = .;
    if H35_2    = 9 then H35_2 = .;
    if H35_3    = 9 then H35_3 = .;
    if H35_4    = 9 then H35_4 = .;
    if H35_5    = 9 then H35_5 = .;
    if H35_6    = 9 then H35_6 = .;
    if H36A_1   = 9 then H36A_1 = .;
    if H36A_2   = 9 then H36A_2 = .;
    if H36A_3   = 9 then H36A_3 = .;
    if H36A_4   = 9 then H36A_4 = .;
    if H36A_5   = 9 then H36A_5 = .;
    if H36A_6   = 9 then H36A_6 = .;
    if H36B_1   = 9 then H36B_1 = .;
    if H36B_2   = 9 then H36B_2 = .;
    if H36B_3   = 9 then H36B_3 = .;
    if H36B_4   = 9 then H36B_4 = .;
    if H36B_5   = 9 then H36B_5 = .;
    if H36B_6   = 9 then H36B_6 = .;
    if H36C_1   = 9 then H36C_1 = .;
    if H36C_2   = 9 then H36C_2 = .;
    if H36C_3   = 9 then H36C_3 = .;
    if H36C_4   = 9 then H36C_4 = .;
    if H36C_5   = 9 then H36C_5 = .;
    if H36C_6   = 9 then H36C_6 = .;
    if H36D_1   = 9 then H36D_1 = .;
    if H36D_2   = 9 then H36D_2 = .;
    if H36D_3   = 9 then H36D_3 = .;
    if H36D_4   = 9 then H36D_4 = .;
    if H36D_5   = 9 then H36D_5 = .;
    if H36D_6   = 9 then H36D_6 = .;
    if H36E_1   = 9 then H36E_1 = .;
    if H36E_2   = 9 then H36E_2 = .;
    if H36E_3   = 9 then H36E_3 = .;
    if H36E_4   = 9 then H36E_4 = .;
    if H36E_5   = 9 then H36E_5 = .;
    if H36E_6   = 9 then H36E_6 = .;
    if H36F_1   = 9 then H36F_1 = .;
    if H36F_2   = 9 then H36F_2 = .;
    if H36F_3   = 9 then H36F_3 = .;
    if H36F_4   = 9 then H36F_4 = .;
    if H36F_5   = 9 then H36F_5 = .;
    if H36F_6   = 9 then H36F_6 = .;
    if H40_1    = 9 then H40_1 = .;
    if H40_2    = 9 then H40_2 = .;
    if H40_3    = 9 then H40_3 = .;
    if H40_4    = 9 then H40_4 = .;
    if H40_5    = 9 then H40_5 = .;
    if H40_6    = 9 then H40_6 = .;
    if H40D_1   = 99 then H40D_1 = .;
    if H40D_2   = 99 then H40D_2 = .;
    if H40D_3   = 99 then H40D_3 = .;
    if H40D_4   = 99 then H40D_4 = .;
    if H40D_5   = 99 then H40D_5 = .;
    if H40D_6   = 99 then H40D_6 = .;
    if H40M_1   = 99 then H40M_1 = .;
    if H40M_2   = 99 then H40M_2 = .;
    if H40M_3   = 99 then H40M_3 = .;
    if H40M_4   = 99 then H40M_4 = .;
    if H40M_5   = 99 then H40M_5 = .;
    if H40M_6   = 99 then H40M_6 = .;
    if H40Y_1   = 9999 then H40Y_1 = .;
    if H40Y_2   = 9999 then H40Y_2 = .;
    if H40Y_3   = 9999 then H40Y_3 = .;
    if H40Y_4   = 9999 then H40Y_4 = .;
    if H40Y_5   = 9999 then H40Y_5 = .;
    if H40Y_6   = 9999 then H40Y_6 = .;
    if H41A_1   = 9 then H41A_1 = .;
    if H41A_2   = 9 then H41A_2 = .;
    if H41A_3   = 9 then H41A_3 = .;
    if H41A_4   = 9 then H41A_4 = .;
    if H41A_5   = 9 then H41A_5 = .;
    if H41A_6   = 9 then H41A_6 = .;
    if H41B_1   = 9 then H41B_1 = .;
    if H41B_2   = 9 then H41B_2 = .;
    if H41B_3   = 9 then H41B_3 = .;
    if H41B_4   = 9 then H41B_4 = .;
    if H41B_5   = 9 then H41B_5 = .;
    if H41B_6   = 9 then H41B_6 = .;
    if H50_1    = 9 then H50_1 = .;
    if H50_2    = 9 then H50_2 = .;
    if H50_3    = 9 then H50_3 = .;
    if H50_4    = 9 then H50_4 = .;
    if H50_5    = 9 then H50_5 = .;
    if H50_6    = 9 then H50_6 = .;
    if H50D_1   = 99 then H50D_1 = .;
    if H50D_2   = 99 then H50D_2 = .;
    if H50D_3   = 99 then H50D_3 = .;
    if H50D_4   = 99 then H50D_4 = .;
    if H50D_5   = 99 then H50D_5 = .;
    if H50D_6   = 99 then H50D_6 = .;
    if H50M_1   = 99 then H50M_1 = .;
    if H50M_2   = 99 then H50M_2 = .;
    if H50M_3   = 99 then H50M_3 = .;
    if H50M_4   = 99 then H50M_4 = .;
    if H50M_5   = 99 then H50M_5 = .;
    if H50M_6   = 99 then H50M_6 = .;
    if H50Y_1   = 9999 then H50Y_1 = .;
    if H50Y_2   = 9999 then H50Y_2 = .;
    if H50Y_3   = 9999 then H50Y_3 = .;
    if H50Y_4   = 9999 then H50Y_4 = .;
    if H50Y_5   = 9999 then H50Y_5 = .;
    if H50Y_6   = 9999 then H50Y_6 = .;
    if H51_1    = 9 then H51_1 = .;
    if H51_2    = 9 then H51_2 = .;
    if H51_3    = 9 then H51_3 = .;
    if H51_4    = 9 then H51_4 = .;
    if H51_5    = 9 then H51_5 = .;
    if H51_6    = 9 then H51_6 = .;
    if H51D_1   = 99 then H51D_1 = .;
    if H51D_2   = 99 then H51D_2 = .;
    if H51D_3   = 99 then H51D_3 = .;
    if H51D_4   = 99 then H51D_4 = .;
    if H51D_5   = 99 then H51D_5 = .;
    if H51D_6   = 99 then H51D_6 = .;
    if H51M_1   = 99 then H51M_1 = .;
    if H51M_2   = 99 then H51M_2 = .;
    if H51M_3   = 99 then H51M_3 = .;
    if H51M_4   = 99 then H51M_4 = .;
    if H51M_5   = 99 then H51M_5 = .;
    if H51M_6   = 99 then H51M_6 = .;
    if H51Y_1   = 9999 then H51Y_1 = .;
    if H51Y_2   = 9999 then H51Y_2 = .;
    if H51Y_3   = 9999 then H51Y_3 = .;
    if H51Y_4   = 9999 then H51Y_4 = .;
    if H51Y_5   = 9999 then H51Y_5 = .;
    if H51Y_6   = 9999 then H51Y_6 = .;
    if H52_1    = 9 then H52_1 = .;
    if H52_2    = 9 then H52_2 = .;
    if H52_3    = 9 then H52_3 = .;
    if H52_4    = 9 then H52_4 = .;
    if H52_5    = 9 then H52_5 = .;
    if H52_6    = 9 then H52_6 = .;
    if H52D_1   = 99 then H52D_1 = .;
    if H52D_2   = 99 then H52D_2 = .;
    if H52D_3   = 99 then H52D_3 = .;
    if H52D_4   = 99 then H52D_4 = .;
    if H52D_5   = 99 then H52D_5 = .;
    if H52D_6   = 99 then H52D_6 = .;
    if H52M_1   = 99 then H52M_1 = .;
    if H52M_2   = 99 then H52M_2 = .;
    if H52M_3   = 99 then H52M_3 = .;
    if H52M_4   = 99 then H52M_4 = .;
    if H52M_5   = 99 then H52M_5 = .;
    if H52M_6   = 99 then H52M_6 = .;
    if H52Y_1   = 9999 then H52Y_1 = .;
    if H52Y_2   = 9999 then H52Y_2 = .;
    if H52Y_3   = 9999 then H52Y_3 = .;
    if H52Y_4   = 9999 then H52Y_4 = .;
    if H52Y_5   = 9999 then H52Y_5 = .;
    if H52Y_6   = 9999 then H52Y_6 = .;
    if H53_1    = 9 then H53_1 = .;
    if H53_2    = 9 then H53_2 = .;
    if H53_3    = 9 then H53_3 = .;
    if H53_4    = 9 then H53_4 = .;
    if H53_5    = 9 then H53_5 = .;
    if H53_6    = 9 then H53_6 = .;
    if H53D_1   = 99 then H53D_1 = .;
    if H53D_2   = 99 then H53D_2 = .;
    if H53D_3   = 99 then H53D_3 = .;
    if H53D_4   = 99 then H53D_4 = .;
    if H53D_5   = 99 then H53D_5 = .;
    if H53D_6   = 99 then H53D_6 = .;
    if H53M_1   = 99 then H53M_1 = .;
    if H53M_2   = 99 then H53M_2 = .;
    if H53M_3   = 99 then H53M_3 = .;
    if H53M_4   = 99 then H53M_4 = .;
    if H53M_5   = 99 then H53M_5 = .;
    if H53M_6   = 99 then H53M_6 = .;
    if H53Y_1   = 9999 then H53Y_1 = .;
    if H53Y_2   = 9999 then H53Y_2 = .;
    if H53Y_3   = 9999 then H53Y_3 = .;
    if H53Y_4   = 9999 then H53Y_4 = .;
    if H53Y_5   = 9999 then H53Y_5 = .;
    if H53Y_6   = 9999 then H53Y_6 = .;
    if H54_1    = 9 then H54_1 = .;
    if H54_2    = 9 then H54_2 = .;
    if H54_3    = 9 then H54_3 = .;
    if H54_4    = 9 then H54_4 = .;
    if H54_5    = 9 then H54_5 = .;
    if H54_6    = 9 then H54_6 = .;
    if H54D_1   = 99 then H54D_1 = .;
    if H54D_2   = 99 then H54D_2 = .;
    if H54D_3   = 99 then H54D_3 = .;
    if H54D_4   = 99 then H54D_4 = .;
    if H54D_5   = 99 then H54D_5 = .;
    if H54D_6   = 99 then H54D_6 = .;
    if H54M_1   = 99 then H54M_1 = .;
    if H54M_2   = 99 then H54M_2 = .;
    if H54M_3   = 99 then H54M_3 = .;
    if H54M_4   = 99 then H54M_4 = .;
    if H54M_5   = 99 then H54M_5 = .;
    if H54M_6   = 99 then H54M_6 = .;
    if H54Y_1   = 9999 then H54Y_1 = .;
    if H54Y_2   = 9999 then H54Y_2 = .;
    if H54Y_3   = 9999 then H54Y_3 = .;
    if H54Y_4   = 9999 then H54Y_4 = .;
    if H54Y_5   = 9999 then H54Y_5 = .;
    if H54Y_6   = 9999 then H54Y_6 = .;
    if H55_1    = 9 then H55_1 = .;
    if H55_2    = 9 then H55_2 = .;
    if H55_3    = 9 then H55_3 = .;
    if H55_4    = 9 then H55_4 = .;
    if H55_5    = 9 then H55_5 = .;
    if H55_6    = 9 then H55_6 = .;
    if H55D_1   = 99 then H55D_1 = .;
    if H55D_2   = 99 then H55D_2 = .;
    if H55D_3   = 99 then H55D_3 = .;
    if H55D_4   = 99 then H55D_4 = .;
    if H55D_5   = 99 then H55D_5 = .;
    if H55D_6   = 99 then H55D_6 = .;
    if H55M_1   = 99 then H55M_1 = .;
    if H55M_2   = 99 then H55M_2 = .;
    if H55M_3   = 99 then H55M_3 = .;
    if H55M_4   = 99 then H55M_4 = .;
    if H55M_5   = 99 then H55M_5 = .;
    if H55M_6   = 99 then H55M_6 = .;
    if H55Y_1   = 9999 then H55Y_1 = .;
    if H55Y_2   = 9999 then H55Y_2 = .;
    if H55Y_3   = 9999 then H55Y_3 = .;
    if H55Y_4   = 9999 then H55Y_4 = .;
    if H55Y_5   = 9999 then H55Y_5 = .;
    if H55Y_6   = 9999 then H55Y_6 = .;
    if H56_1    = 9 then H56_1 = .;
    if H56_2    = 9 then H56_2 = .;
    if H56_3    = 9 then H56_3 = .;
    if H56_4    = 9 then H56_4 = .;
    if H56_5    = 9 then H56_5 = .;
    if H56_6    = 9 then H56_6 = .;
    if H56D_1   = 99 then H56D_1 = .;
    if H56D_2   = 99 then H56D_2 = .;
    if H56D_3   = 99 then H56D_3 = .;
    if H56D_4   = 99 then H56D_4 = .;
    if H56D_5   = 99 then H56D_5 = .;
    if H56D_6   = 99 then H56D_6 = .;
    if H56M_1   = 99 then H56M_1 = .;
    if H56M_2   = 99 then H56M_2 = .;
    if H56M_3   = 99 then H56M_3 = .;
    if H56M_4   = 99 then H56M_4 = .;
    if H56M_5   = 99 then H56M_5 = .;
    if H56M_6   = 99 then H56M_6 = .;
    if H56Y_1   = 9999 then H56Y_1 = .;
    if H56Y_2   = 9999 then H56Y_2 = .;
    if H56Y_3   = 9999 then H56Y_3 = .;
    if H56Y_4   = 9999 then H56Y_4 = .;
    if H56Y_5   = 9999 then H56Y_5 = .;
    if H56Y_6   = 9999 then H56Y_6 = .;
    if H57_1    = 9 then H57_1 = .;
    if H57_2    = 9 then H57_2 = .;
    if H57_3    = 9 then H57_3 = .;
    if H57_4    = 9 then H57_4 = .;
    if H57_5    = 9 then H57_5 = .;
    if H57_6    = 9 then H57_6 = .;
    if H57D_1   = 99 then H57D_1 = .;
    if H57D_2   = 99 then H57D_2 = .;
    if H57D_3   = 99 then H57D_3 = .;
    if H57D_4   = 99 then H57D_4 = .;
    if H57D_5   = 99 then H57D_5 = .;
    if H57D_6   = 99 then H57D_6 = .;
    if H57M_1   = 99 then H57M_1 = .;
    if H57M_2   = 99 then H57M_2 = .;
    if H57M_3   = 99 then H57M_3 = .;
    if H57M_4   = 99 then H57M_4 = .;
    if H57M_5   = 99 then H57M_5 = .;
    if H57M_6   = 99 then H57M_6 = .;
    if H57Y_1   = 9999 then H57Y_1 = .;
    if H57Y_2   = 9999 then H57Y_2 = .;
    if H57Y_3   = 9999 then H57Y_3 = .;
    if H57Y_4   = 9999 then H57Y_4 = .;
    if H57Y_5   = 9999 then H57Y_5 = .;
    if H57Y_6   = 9999 then H57Y_6 = .;
    if H58_1    = 9 then H58_1 = .;
    if H58_2    = 9 then H58_2 = .;
    if H58_3    = 9 then H58_3 = .;
    if H58_4    = 9 then H58_4 = .;
    if H58_5    = 9 then H58_5 = .;
    if H58_6    = 9 then H58_6 = .;
    if H58D_1   = 99 then H58D_1 = .;
    if H58D_2   = 99 then H58D_2 = .;
    if H58D_3   = 99 then H58D_3 = .;
    if H58D_4   = 99 then H58D_4 = .;
    if H58D_5   = 99 then H58D_5 = .;
    if H58D_6   = 99 then H58D_6 = .;
    if H58M_1   = 99 then H58M_1 = .;
    if H58M_2   = 99 then H58M_2 = .;
    if H58M_3   = 99 then H58M_3 = .;
    if H58M_4   = 99 then H58M_4 = .;
    if H58M_5   = 99 then H58M_5 = .;
    if H58M_6   = 99 then H58M_6 = .;
    if H58Y_1   = 9999 then H58Y_1 = .;
    if H58Y_2   = 9999 then H58Y_2 = .;
    if H58Y_3   = 9999 then H58Y_3 = .;
    if H58Y_4   = 9999 then H58Y_4 = .;
    if H58Y_5   = 9999 then H58Y_5 = .;
    if H58Y_6   = 9999 then H58Y_6 = .;
    if H59_1    = 9 then H59_1 = .;
    if H59_2    = 9 then H59_2 = .;
    if H59_3    = 9 then H59_3 = .;
    if H59_4    = 9 then H59_4 = .;
    if H59_5    = 9 then H59_5 = .;
    if H59_6    = 9 then H59_6 = .;
    if H59D_1   = 99 then H59D_1 = .;
    if H59D_2   = 99 then H59D_2 = .;
    if H59D_3   = 99 then H59D_3 = .;
    if H59D_4   = 99 then H59D_4 = .;
    if H59D_5   = 99 then H59D_5 = .;
    if H59D_6   = 99 then H59D_6 = .;
    if H59M_1   = 99 then H59M_1 = .;
    if H59M_2   = 99 then H59M_2 = .;
    if H59M_3   = 99 then H59M_3 = .;
    if H59M_4   = 99 then H59M_4 = .;
    if H59M_5   = 99 then H59M_5 = .;
    if H59M_6   = 99 then H59M_6 = .;
    if H59Y_1   = 9999 then H59Y_1 = .;
    if H59Y_2   = 9999 then H59Y_2 = .;
    if H59Y_3   = 9999 then H59Y_3 = .;
    if H59Y_4   = 9999 then H59Y_4 = .;
    if H59Y_5   = 9999 then H59Y_5 = .;
    if H59Y_6   = 9999 then H59Y_6 = .;
    if H60_1    = 9 then H60_1 = .;
    if H60_2    = 9 then H60_2 = .;
    if H60_3    = 9 then H60_3 = .;
    if H60_4    = 9 then H60_4 = .;
    if H60_5    = 9 then H60_5 = .;
    if H60_6    = 9 then H60_6 = .;
    if H60D_1   = 99 then H60D_1 = .;
    if H60D_2   = 99 then H60D_2 = .;
    if H60D_3   = 99 then H60D_3 = .;
    if H60D_4   = 99 then H60D_4 = .;
    if H60D_5   = 99 then H60D_5 = .;
    if H60D_6   = 99 then H60D_6 = .;
    if H60M_1   = 99 then H60M_1 = .;
    if H60M_2   = 99 then H60M_2 = .;
    if H60M_3   = 99 then H60M_3 = .;
    if H60M_4   = 99 then H60M_4 = .;
    if H60M_5   = 99 then H60M_5 = .;
    if H60M_6   = 99 then H60M_6 = .;
    if H60Y_1   = 9999 then H60Y_1 = .;
    if H60Y_2   = 9999 then H60Y_2 = .;
    if H60Y_3   = 9999 then H60Y_3 = .;
    if H60Y_4   = 9999 then H60Y_4 = .;
    if H60Y_5   = 9999 then H60Y_5 = .;
    if H60Y_6   = 9999 then H60Y_6 = .;
    if H61_1    = 9 then H61_1 = .;
    if H61_2    = 9 then H61_2 = .;
    if H61_3    = 9 then H61_3 = .;
    if H61_4    = 9 then H61_4 = .;
    if H61_5    = 9 then H61_5 = .;
    if H61_6    = 9 then H61_6 = .;
    if H61D_1   = 99 then H61D_1 = .;
    if H61D_2   = 99 then H61D_2 = .;
    if H61D_3   = 99 then H61D_3 = .;
    if H61D_4   = 99 then H61D_4 = .;
    if H61D_5   = 99 then H61D_5 = .;
    if H61D_6   = 99 then H61D_6 = .;
    if H61M_1   = 99 then H61M_1 = .;
    if H61M_2   = 99 then H61M_2 = .;
    if H61M_3   = 99 then H61M_3 = .;
    if H61M_4   = 99 then H61M_4 = .;
    if H61M_5   = 99 then H61M_5 = .;
    if H61M_6   = 99 then H61M_6 = .;
    if H61Y_1   = 9999 then H61Y_1 = .;
    if H61Y_2   = 9999 then H61Y_2 = .;
    if H61Y_3   = 9999 then H61Y_3 = .;
    if H61Y_4   = 9999 then H61Y_4 = .;
    if H61Y_5   = 9999 then H61Y_5 = .;
    if H61Y_6   = 9999 then H61Y_6 = .;
    if H62_1    = 9 then H62_1 = .;
    if H62_2    = 9 then H62_2 = .;
    if H62_3    = 9 then H62_3 = .;
    if H62_4    = 9 then H62_4 = .;
    if H62_5    = 9 then H62_5 = .;
    if H62_6    = 9 then H62_6 = .;
    if H62D_1   = 99 then H62D_1 = .;
    if H62D_2   = 99 then H62D_2 = .;
    if H62D_3   = 99 then H62D_3 = .;
    if H62D_4   = 99 then H62D_4 = .;
    if H62D_5   = 99 then H62D_5 = .;
    if H62D_6   = 99 then H62D_6 = .;
    if H62M_1   = 99 then H62M_1 = .;
    if H62M_2   = 99 then H62M_2 = .;
    if H62M_3   = 99 then H62M_3 = .;
    if H62M_4   = 99 then H62M_4 = .;
    if H62M_5   = 99 then H62M_5 = .;
    if H62M_6   = 99 then H62M_6 = .;
    if H62Y_1   = 9999 then H62Y_1 = .;
    if H62Y_2   = 9999 then H62Y_2 = .;
    if H62Y_3   = 9999 then H62Y_3 = .;
    if H62Y_4   = 9999 then H62Y_4 = .;
    if H62Y_5   = 9999 then H62Y_5 = .;
    if H62Y_6   = 9999 then H62Y_6 = .;
    if H63_1    = 9 then H63_1 = .;
    if H63_2    = 9 then H63_2 = .;
    if H63_3    = 9 then H63_3 = .;
    if H63_4    = 9 then H63_4 = .;
    if H63_5    = 9 then H63_5 = .;
    if H63_6    = 9 then H63_6 = .;
    if H63D_1   = 99 then H63D_1 = .;
    if H63D_2   = 99 then H63D_2 = .;
    if H63D_3   = 99 then H63D_3 = .;
    if H63D_4   = 99 then H63D_4 = .;
    if H63D_5   = 99 then H63D_5 = .;
    if H63D_6   = 99 then H63D_6 = .;
    if H63M_1   = 99 then H63M_1 = .;
    if H63M_2   = 99 then H63M_2 = .;
    if H63M_3   = 99 then H63M_3 = .;
    if H63M_4   = 99 then H63M_4 = .;
    if H63M_5   = 99 then H63M_5 = .;
    if H63M_6   = 99 then H63M_6 = .;
    if H63Y_1   = 9999 then H63Y_1 = .;
    if H63Y_2   = 9999 then H63Y_2 = .;
    if H63Y_3   = 9999 then H63Y_3 = .;
    if H63Y_4   = 9999 then H63Y_4 = .;
    if H63Y_5   = 9999 then H63Y_5 = .;
    if H63Y_6   = 9999 then H63Y_6 = .;
    if H64_1    = 9 then H64_1 = .;
    if H64_2    = 9 then H64_2 = .;
    if H64_3    = 9 then H64_3 = .;
    if H64_4    = 9 then H64_4 = .;
    if H64_5    = 9 then H64_5 = .;
    if H64_6    = 9 then H64_6 = .;
    if H64D_1   = 99 then H64D_1 = .;
    if H64D_2   = 99 then H64D_2 = .;
    if H64D_3   = 99 then H64D_3 = .;
    if H64D_4   = 99 then H64D_4 = .;
    if H64D_5   = 99 then H64D_5 = .;
    if H64D_6   = 99 then H64D_6 = .;
    if H64M_1   = 99 then H64M_1 = .;
    if H64M_2   = 99 then H64M_2 = .;
    if H64M_3   = 99 then H64M_3 = .;
    if H64M_4   = 99 then H64M_4 = .;
    if H64M_5   = 99 then H64M_5 = .;
    if H64M_6   = 99 then H64M_6 = .;
    if H64Y_1   = 9999 then H64Y_1 = .;
    if H64Y_2   = 9999 then H64Y_2 = .;
    if H64Y_3   = 9999 then H64Y_3 = .;
    if H64Y_4   = 9999 then H64Y_4 = .;
    if H64Y_5   = 9999 then H64Y_5 = .;
    if H64Y_6   = 9999 then H64Y_6 = .;
    if H65_1    = 9 then H65_1 = .;
    if H65_2    = 9 then H65_2 = .;
    if H65_3    = 9 then H65_3 = .;
    if H65_4    = 9 then H65_4 = .;
    if H65_5    = 9 then H65_5 = .;
    if H65_6    = 9 then H65_6 = .;
    if H65D_1   = 99 then H65D_1 = .;
    if H65D_2   = 99 then H65D_2 = .;
    if H65D_3   = 99 then H65D_3 = .;
    if H65D_4   = 99 then H65D_4 = .;
    if H65D_5   = 99 then H65D_5 = .;
    if H65D_6   = 99 then H65D_6 = .;
    if H65M_1   = 99 then H65M_1 = .;
    if H65M_2   = 99 then H65M_2 = .;
    if H65M_3   = 99 then H65M_3 = .;
    if H65M_4   = 99 then H65M_4 = .;
    if H65M_5   = 99 then H65M_5 = .;
    if H65M_6   = 99 then H65M_6 = .;
    if H65Y_1   = 9999 then H65Y_1 = .;
    if H65Y_2   = 9999 then H65Y_2 = .;
    if H65Y_3   = 9999 then H65Y_3 = .;
    if H65Y_4   = 9999 then H65Y_4 = .;
    if H65Y_5   = 9999 then H65Y_5 = .;
    if H65Y_6   = 9999 then H65Y_6 = .;
    if H66_1    = 9 then H66_1 = .;
    if H66_2    = 9 then H66_2 = .;
    if H66_3    = 9 then H66_3 = .;
    if H66_4    = 9 then H66_4 = .;
    if H66_5    = 9 then H66_5 = .;
    if H66_6    = 9 then H66_6 = .;
    if H66D_1   = 99 then H66D_1 = .;
    if H66D_2   = 99 then H66D_2 = .;
    if H66D_3   = 99 then H66D_3 = .;
    if H66D_4   = 99 then H66D_4 = .;
    if H66D_5   = 99 then H66D_5 = .;
    if H66D_6   = 99 then H66D_6 = .;
    if H66M_1   = 99 then H66M_1 = .;
    if H66M_2   = 99 then H66M_2 = .;
    if H66M_3   = 99 then H66M_3 = .;
    if H66M_4   = 99 then H66M_4 = .;
    if H66M_5   = 99 then H66M_5 = .;
    if H66M_6   = 99 then H66M_6 = .;
    if H66Y_1   = 9999 then H66Y_1 = .;
    if H66Y_2   = 9999 then H66Y_2 = .;
    if H66Y_3   = 9999 then H66Y_3 = .;
    if H66Y_4   = 9999 then H66Y_4 = .;
    if H66Y_5   = 9999 then H66Y_5 = .;
    if H66Y_6   = 9999 then H66Y_6 = .;
    if H80A_1   = 9 then H80A_1 = .;
    if H80A_2   = 9 then H80A_2 = .;
    if H80A_3   = 9 then H80A_3 = .;
    if H80A_4   = 9 then H80A_4 = .;
    if H80A_5   = 9 then H80A_5 = .;
    if H80A_6   = 9 then H80A_6 = .;
    if H80B_1   = 9 then H80B_1 = .;
    if H80B_2   = 9 then H80B_2 = .;
    if H80B_3   = 9 then H80B_3 = .;
    if H80B_4   = 9 then H80B_4 = .;
    if H80B_5   = 9 then H80B_5 = .;
    if H80B_6   = 9 then H80B_6 = .;
    if H80C_1   = 9 then H80C_1 = .;
    if H80C_2   = 9 then H80C_2 = .;
    if H80C_3   = 9 then H80C_3 = .;
    if H80C_4   = 9 then H80C_4 = .;
    if H80C_5   = 9 then H80C_5 = .;
    if H80C_6   = 9 then H80C_6 = .;
    if H80D_1   = 9 then H80D_1 = .;
    if H80D_2   = 9 then H80D_2 = .;
    if H80D_3   = 9 then H80D_3 = .;
    if H80D_4   = 9 then H80D_4 = .;
    if H80D_5   = 9 then H80D_5 = .;
    if H80D_6   = 9 then H80D_6 = .;
    if H80E_1   = 9 then H80E_1 = .;
    if H80E_2   = 9 then H80E_2 = .;
    if H80E_3   = 9 then H80E_3 = .;
    if H80E_4   = 9 then H80E_4 = .;
    if H80E_5   = 9 then H80E_5 = .;
    if H80E_6   = 9 then H80E_6 = .;
    if H80F_1   = 9 then H80F_1 = .;
    if H80F_2   = 9 then H80F_2 = .;
    if H80F_3   = 9 then H80F_3 = .;
    if H80F_4   = 9 then H80F_4 = .;
    if H80F_5   = 9 then H80F_5 = .;
    if H80F_6   = 9 then H80F_6 = .;
    if H80G_1   = 9 then H80G_1 = .;
    if H80G_2   = 9 then H80G_2 = .;
    if H80G_3   = 9 then H80G_3 = .;
    if H80G_4   = 9 then H80G_4 = .;
    if H80G_5   = 9 then H80G_5 = .;
    if H80G_6   = 9 then H80G_6 = .;
    if H11_1    = 9 then H11_1 = .;
    if H11_2    = 9 then H11_2 = .;
    if H11_3    = 9 then H11_3 = .;
    if H11_4    = 9 then H11_4 = .;
    if H11_5    = 9 then H11_5 = .;
    if H11_6    = 9 then H11_6 = .;
    if H11B_1   = 9 then H11B_1 = .;
    if H11B_2   = 9 then H11B_2 = .;
    if H11B_3   = 9 then H11B_3 = .;
    if H11B_4   = 9 then H11B_4 = .;
    if H11B_5   = 9 then H11B_5 = .;
    if H11B_6   = 9 then H11B_6 = .;
    if H12A_1   = 9 then H12A_1 = .;
    if H12A_2   = 9 then H12A_2 = .;
    if H12A_3   = 9 then H12A_3 = .;
    if H12A_4   = 9 then H12A_4 = .;
    if H12A_5   = 9 then H12A_5 = .;
    if H12A_6   = 9 then H12A_6 = .;
    if H12B_1   = 9 then H12B_1 = .;
    if H12B_2   = 9 then H12B_2 = .;
    if H12B_3   = 9 then H12B_3 = .;
    if H12B_4   = 9 then H12B_4 = .;
    if H12B_5   = 9 then H12B_5 = .;
    if H12B_6   = 9 then H12B_6 = .;
    if H12C_1   = 9 then H12C_1 = .;
    if H12C_2   = 9 then H12C_2 = .;
    if H12C_3   = 9 then H12C_3 = .;
    if H12C_4   = 9 then H12C_4 = .;
    if H12C_5   = 9 then H12C_5 = .;
    if H12C_6   = 9 then H12C_6 = .;
    if H12D_1   = 9 then H12D_1 = .;
    if H12D_2   = 9 then H12D_2 = .;
    if H12D_3   = 9 then H12D_3 = .;
    if H12D_4   = 9 then H12D_4 = .;
    if H12D_5   = 9 then H12D_5 = .;
    if H12D_6   = 9 then H12D_6 = .;
    if H12E_1   = 9 then H12E_1 = .;
    if H12E_2   = 9 then H12E_2 = .;
    if H12E_3   = 9 then H12E_3 = .;
    if H12E_4   = 9 then H12E_4 = .;
    if H12E_5   = 9 then H12E_5 = .;
    if H12E_6   = 9 then H12E_6 = .;
    if H12F_1   = 9 then H12F_1 = .;
    if H12F_2   = 9 then H12F_2 = .;
    if H12F_3   = 9 then H12F_3 = .;
    if H12F_4   = 9 then H12F_4 = .;
    if H12F_5   = 9 then H12F_5 = .;
    if H12F_6   = 9 then H12F_6 = .;
    if H12G_1   = 9 then H12G_1 = .;
    if H12G_2   = 9 then H12G_2 = .;
    if H12G_3   = 9 then H12G_3 = .;
    if H12G_4   = 9 then H12G_4 = .;
    if H12G_5   = 9 then H12G_5 = .;
    if H12G_6   = 9 then H12G_6 = .;
    if H12H_1   = 9 then H12H_1 = .;
    if H12H_2   = 9 then H12H_2 = .;
    if H12H_3   = 9 then H12H_3 = .;
    if H12H_4   = 9 then H12H_4 = .;
    if H12H_5   = 9 then H12H_5 = .;
    if H12H_6   = 9 then H12H_6 = .;
    if H12I_1   = 9 then H12I_1 = .;
    if H12I_2   = 9 then H12I_2 = .;
    if H12I_3   = 9 then H12I_3 = .;
    if H12I_4   = 9 then H12I_4 = .;
    if H12I_5   = 9 then H12I_5 = .;
    if H12I_6   = 9 then H12I_6 = .;
    if H12J_1   = 9 then H12J_1 = .;
    if H12J_2   = 9 then H12J_2 = .;
    if H12J_3   = 9 then H12J_3 = .;
    if H12J_4   = 9 then H12J_4 = .;
    if H12J_5   = 9 then H12J_5 = .;
    if H12J_6   = 9 then H12J_6 = .;
    if H12K_1   = 9 then H12K_1 = .;
    if H12K_2   = 9 then H12K_2 = .;
    if H12K_3   = 9 then H12K_3 = .;
    if H12K_4   = 9 then H12K_4 = .;
    if H12K_5   = 9 then H12K_5 = .;
    if H12K_6   = 9 then H12K_6 = .;
    if H12L_1   = 9 then H12L_1 = .;
    if H12L_2   = 9 then H12L_2 = .;
    if H12L_3   = 9 then H12L_3 = .;
    if H12L_4   = 9 then H12L_4 = .;
    if H12L_5   = 9 then H12L_5 = .;
    if H12L_6   = 9 then H12L_6 = .;
    if H12M_1   = 9 then H12M_1 = .;
    if H12M_2   = 9 then H12M_2 = .;
    if H12M_3   = 9 then H12M_3 = .;
    if H12M_4   = 9 then H12M_4 = .;
    if H12M_5   = 9 then H12M_5 = .;
    if H12M_6   = 9 then H12M_6 = .;
    if H12N_1   = 9 then H12N_1 = .;
    if H12N_2   = 9 then H12N_2 = .;
    if H12N_3   = 9 then H12N_3 = .;
    if H12N_4   = 9 then H12N_4 = .;
    if H12N_5   = 9 then H12N_5 = .;
    if H12N_6   = 9 then H12N_6 = .;
    if H12O_1   = 9 then H12O_1 = .;
    if H12O_2   = 9 then H12O_2 = .;
    if H12O_3   = 9 then H12O_3 = .;
    if H12O_4   = 9 then H12O_4 = .;
    if H12O_5   = 9 then H12O_5 = .;
    if H12O_6   = 9 then H12O_6 = .;
    if H12P_1   = 9 then H12P_1 = .;
    if H12P_2   = 9 then H12P_2 = .;
    if H12P_3   = 9 then H12P_3 = .;
    if H12P_4   = 9 then H12P_4 = .;
    if H12P_5   = 9 then H12P_5 = .;
    if H12P_6   = 9 then H12P_6 = .;
    if H12Q_1   = 9 then H12Q_1 = .;
    if H12Q_2   = 9 then H12Q_2 = .;
    if H12Q_3   = 9 then H12Q_3 = .;
    if H12Q_4   = 9 then H12Q_4 = .;
    if H12Q_5   = 9 then H12Q_5 = .;
    if H12Q_6   = 9 then H12Q_6 = .;
    if H12R_1   = 9 then H12R_1 = .;
    if H12R_2   = 9 then H12R_2 = .;
    if H12R_3   = 9 then H12R_3 = .;
    if H12R_4   = 9 then H12R_4 = .;
    if H12R_5   = 9 then H12R_5 = .;
    if H12R_6   = 9 then H12R_6 = .;
    if H12S_1   = 9 then H12S_1 = .;
    if H12S_2   = 9 then H12S_2 = .;
    if H12S_3   = 9 then H12S_3 = .;
    if H12S_4   = 9 then H12S_4 = .;
    if H12S_5   = 9 then H12S_5 = .;
    if H12S_6   = 9 then H12S_6 = .;
    if H12T_1   = 9 then H12T_1 = .;
    if H12T_2   = 9 then H12T_2 = .;
    if H12T_3   = 9 then H12T_3 = .;
    if H12T_4   = 9 then H12T_4 = .;
    if H12T_5   = 9 then H12T_5 = .;
    if H12T_6   = 9 then H12T_6 = .;
    if H12U_1   = 9 then H12U_1 = .;
    if H12U_2   = 9 then H12U_2 = .;
    if H12U_3   = 9 then H12U_3 = .;
    if H12U_4   = 9 then H12U_4 = .;
    if H12U_5   = 9 then H12U_5 = .;
    if H12U_6   = 9 then H12U_6 = .;
    if H12V_1   = 9 then H12V_1 = .;
    if H12V_2   = 9 then H12V_2 = .;
    if H12V_3   = 9 then H12V_3 = .;
    if H12V_4   = 9 then H12V_4 = .;
    if H12V_5   = 9 then H12V_5 = .;
    if H12V_6   = 9 then H12V_6 = .;
    if H12W_1   = 9 then H12W_1 = .;
    if H12W_2   = 9 then H12W_2 = .;
    if H12W_3   = 9 then H12W_3 = .;
    if H12W_4   = 9 then H12W_4 = .;
    if H12W_5   = 9 then H12W_5 = .;
    if H12W_6   = 9 then H12W_6 = .;
    if H12X_1   = 9 then H12X_1 = .;
    if H12X_2   = 9 then H12X_2 = .;
    if H12X_3   = 9 then H12X_3 = .;
    if H12X_4   = 9 then H12X_4 = .;
    if H12X_5   = 9 then H12X_5 = .;
    if H12X_6   = 9 then H12X_6 = .;
    if H12Y_1   = 9 then H12Y_1 = .;
    if H12Y_2   = 9 then H12Y_2 = .;
    if H12Y_3   = 9 then H12Y_3 = .;
    if H12Y_4   = 9 then H12Y_4 = .;
    if H12Y_5   = 9 then H12Y_5 = .;
    if H12Y_6   = 9 then H12Y_6 = .;
    if H12Z_1   = 9 then H12Z_1 = .;
    if H12Z_2   = 9 then H12Z_2 = .;
    if H12Z_3   = 9 then H12Z_3 = .;
    if H12Z_4   = 9 then H12Z_4 = .;
    if H12Z_5   = 9 then H12Z_5 = .;
    if H12Z_6   = 9 then H12Z_6 = .;
    if H13_1    = 9 then H13_1 = .;
    if H13_2    = 9 then H13_2 = .;
    if H13_3    = 9 then H13_3 = .;
    if H13_4    = 9 then H13_4 = .;
    if H13_5    = 9 then H13_5 = .;
    if H13_6    = 9 then H13_6 = .;
    if H13B_1   = 9 then H13B_1 = .;
    if H13B_2   = 9 then H13B_2 = .;
    if H13B_3   = 9 then H13B_3 = .;
    if H13B_4   = 9 then H13B_4 = .;
    if H13B_5   = 9 then H13B_5 = .;
    if H13B_6   = 9 then H13B_6 = .;
    if H14_1    = 9 then H14_1 = .;
    if H14_2    = 9 then H14_2 = .;
    if H14_3    = 9 then H14_3 = .;
    if H14_4    = 9 then H14_4 = .;
    if H14_5    = 9 then H14_5 = .;
    if H14_6    = 9 then H14_6 = .;
    if H15_1    = 9 then H15_1 = .;
    if H15_2    = 9 then H15_2 = .;
    if H15_3    = 9 then H15_3 = .;
    if H15_4    = 9 then H15_4 = .;
    if H15_5    = 9 then H15_5 = .;
    if H15_6    = 9 then H15_6 = .;
    if H15A_1   = 9 then H15A_1 = .;
    if H15A_2   = 9 then H15A_2 = .;
    if H15A_3   = 9 then H15A_3 = .;
    if H15A_4   = 9 then H15A_4 = .;
    if H15A_5   = 9 then H15A_5 = .;
    if H15A_6   = 9 then H15A_6 = .;
    if H15B_1   = 9 then H15B_1 = .;
    if H15B_2   = 9 then H15B_2 = .;
    if H15B_3   = 9 then H15B_3 = .;
    if H15B_4   = 9 then H15B_4 = .;
    if H15B_5   = 9 then H15B_5 = .;
    if H15B_6   = 9 then H15B_6 = .;
    if H15C_1   = 9 then H15C_1 = .;
    if H15C_2   = 9 then H15C_2 = .;
    if H15C_3   = 9 then H15C_3 = .;
    if H15C_4   = 9 then H15C_4 = .;
    if H15C_5   = 9 then H15C_5 = .;
    if H15C_6   = 9 then H15C_6 = .;
    if H15D_1   = 9 then H15D_1 = .;
    if H15D_2   = 9 then H15D_2 = .;
    if H15D_3   = 9 then H15D_3 = .;
    if H15D_4   = 9 then H15D_4 = .;
    if H15D_5   = 9 then H15D_5 = .;
    if H15D_6   = 9 then H15D_6 = .;
    if H15E_1   = 9 then H15E_1 = .;
    if H15E_2   = 9 then H15E_2 = .;
    if H15E_3   = 9 then H15E_3 = .;
    if H15E_4   = 9 then H15E_4 = .;
    if H15E_5   = 9 then H15E_5 = .;
    if H15E_6   = 9 then H15E_6 = .;
    if H15F_1   = 9 then H15F_1 = .;
    if H15F_2   = 9 then H15F_2 = .;
    if H15F_3   = 9 then H15F_3 = .;
    if H15F_4   = 9 then H15F_4 = .;
    if H15F_5   = 9 then H15F_5 = .;
    if H15F_6   = 9 then H15F_6 = .;
    if H15G_1   = 9 then H15G_1 = .;
    if H15G_2   = 9 then H15G_2 = .;
    if H15G_3   = 9 then H15G_3 = .;
    if H15G_4   = 9 then H15G_4 = .;
    if H15G_5   = 9 then H15G_5 = .;
    if H15G_6   = 9 then H15G_6 = .;
    if H15H_1   = 9 then H15H_1 = .;
    if H15H_2   = 9 then H15H_2 = .;
    if H15H_3   = 9 then H15H_3 = .;
    if H15H_4   = 9 then H15H_4 = .;
    if H15H_5   = 9 then H15H_5 = .;
    if H15H_6   = 9 then H15H_6 = .;
    if H15I_1   = 9 then H15I_1 = .;
    if H15I_2   = 9 then H15I_2 = .;
    if H15I_3   = 9 then H15I_3 = .;
    if H15I_4   = 9 then H15I_4 = .;
    if H15I_5   = 9 then H15I_5 = .;
    if H15I_6   = 9 then H15I_6 = .;
    if H15J_1   = 9 then H15J_1 = .;
    if H15J_2   = 9 then H15J_2 = .;
    if H15J_3   = 9 then H15J_3 = .;
    if H15J_4   = 9 then H15J_4 = .;
    if H15J_5   = 9 then H15J_5 = .;
    if H15J_6   = 9 then H15J_6 = .;
    if H15K_1   = 9 then H15K_1 = .;
    if H15K_2   = 9 then H15K_2 = .;
    if H15K_3   = 9 then H15K_3 = .;
    if H15K_4   = 9 then H15K_4 = .;
    if H15K_5   = 9 then H15K_5 = .;
    if H15K_6   = 9 then H15K_6 = .;
    if H15L_1   = 9 then H15L_1 = .;
    if H15L_2   = 9 then H15L_2 = .;
    if H15L_3   = 9 then H15L_3 = .;
    if H15L_4   = 9 then H15L_4 = .;
    if H15L_5   = 9 then H15L_5 = .;
    if H15L_6   = 9 then H15L_6 = .;
    if H15M_1   = 9 then H15M_1 = .;
    if H15M_2   = 9 then H15M_2 = .;
    if H15M_3   = 9 then H15M_3 = .;
    if H15M_4   = 9 then H15M_4 = .;
    if H15M_5   = 9 then H15M_5 = .;
    if H15M_6   = 9 then H15M_6 = .;
    if H20_1    = 9 then H20_1 = .;
    if H20_2    = 9 then H20_2 = .;
    if H20_3    = 9 then H20_3 = .;
    if H20_4    = 9 then H20_4 = .;
    if H20_5    = 9 then H20_5 = .;
    if H20_6    = 9 then H20_6 = .;
    if H21A_1   = 9 then H21A_1 = .;
    if H21A_2   = 9 then H21A_2 = .;
    if H21A_3   = 9 then H21A_3 = .;
    if H21A_4   = 9 then H21A_4 = .;
    if H21A_5   = 9 then H21A_5 = .;
    if H21A_6   = 9 then H21A_6 = .;
    if H21_1    = 9 then H21_1 = .;
    if H21_2    = 9 then H21_2 = .;
    if H21_3    = 9 then H21_3 = .;
    if H21_4    = 9 then H21_4 = .;
    if H21_5    = 9 then H21_5 = .;
    if H21_6    = 9 then H21_6 = .;
    if H22_1    = 9 then H22_1 = .;
    if H22_2    = 9 then H22_2 = .;
    if H22_3    = 9 then H22_3 = .;
    if H22_4    = 9 then H22_4 = .;
    if H22_5    = 9 then H22_5 = .;
    if H22_6    = 9 then H22_6 = .;
    if H31_1    = 9 then H31_1 = .;
    if H31_2    = 9 then H31_2 = .;
    if H31_3    = 9 then H31_3 = .;
    if H31_4    = 9 then H31_4 = .;
    if H31_5    = 9 then H31_5 = .;
    if H31_6    = 9 then H31_6 = .;
    if H31B_1   = 9 then H31B_1 = .;
    if H31B_2   = 9 then H31B_2 = .;
    if H31B_3   = 9 then H31B_3 = .;
    if H31B_4   = 9 then H31B_4 = .;
    if H31B_5   = 9 then H31B_5 = .;
    if H31B_6   = 9 then H31B_6 = .;
    if H31C_1   = 9 then H31C_1 = .;
    if H31C_2   = 9 then H31C_2 = .;
    if H31C_3   = 9 then H31C_3 = .;
    if H31C_4   = 9 then H31C_4 = .;
    if H31C_5   = 9 then H31C_5 = .;
    if H31C_6   = 9 then H31C_6 = .;
    if H31D_1   = 9 then H31D_1 = .;
    if H31D_2   = 9 then H31D_2 = .;
    if H31D_3   = 9 then H31D_3 = .;
    if H31D_4   = 9 then H31D_4 = .;
    if H31D_5   = 9 then H31D_5 = .;
    if H31D_6   = 9 then H31D_6 = .;
    if H31E_1   = 9 then H31E_1 = .;
    if H31E_2   = 9 then H31E_2 = .;
    if H31E_3   = 9 then H31E_3 = .;
    if H31E_4   = 9 then H31E_4 = .;
    if H31E_5   = 9 then H31E_5 = .;
    if H31E_6   = 9 then H31E_6 = .;
    if H32A_1   = 9 then H32A_1 = .;
    if H32A_2   = 9 then H32A_2 = .;
    if H32A_3   = 9 then H32A_3 = .;
    if H32A_4   = 9 then H32A_4 = .;
    if H32A_5   = 9 then H32A_5 = .;
    if H32A_6   = 9 then H32A_6 = .;
    if H32B_1   = 9 then H32B_1 = .;
    if H32B_2   = 9 then H32B_2 = .;
    if H32B_3   = 9 then H32B_3 = .;
    if H32B_4   = 9 then H32B_4 = .;
    if H32B_5   = 9 then H32B_5 = .;
    if H32B_6   = 9 then H32B_6 = .;
    if H32C_1   = 9 then H32C_1 = .;
    if H32C_2   = 9 then H32C_2 = .;
    if H32C_3   = 9 then H32C_3 = .;
    if H32C_4   = 9 then H32C_4 = .;
    if H32C_5   = 9 then H32C_5 = .;
    if H32C_6   = 9 then H32C_6 = .;
    if H32D_1   = 9 then H32D_1 = .;
    if H32D_2   = 9 then H32D_2 = .;
    if H32D_3   = 9 then H32D_3 = .;
    if H32D_4   = 9 then H32D_4 = .;
    if H32D_5   = 9 then H32D_5 = .;
    if H32D_6   = 9 then H32D_6 = .;
    if H32E_1   = 9 then H32E_1 = .;
    if H32E_2   = 9 then H32E_2 = .;
    if H32E_3   = 9 then H32E_3 = .;
    if H32E_4   = 9 then H32E_4 = .;
    if H32E_5   = 9 then H32E_5 = .;
    if H32E_6   = 9 then H32E_6 = .;
    if H32F_1   = 9 then H32F_1 = .;
    if H32F_2   = 9 then H32F_2 = .;
    if H32F_3   = 9 then H32F_3 = .;
    if H32F_4   = 9 then H32F_4 = .;
    if H32F_5   = 9 then H32F_5 = .;
    if H32F_6   = 9 then H32F_6 = .;
    if H32G_1   = 9 then H32G_1 = .;
    if H32G_2   = 9 then H32G_2 = .;
    if H32G_3   = 9 then H32G_3 = .;
    if H32G_4   = 9 then H32G_4 = .;
    if H32G_5   = 9 then H32G_5 = .;
    if H32G_6   = 9 then H32G_6 = .;
    if H32H_1   = 9 then H32H_1 = .;
    if H32H_2   = 9 then H32H_2 = .;
    if H32H_3   = 9 then H32H_3 = .;
    if H32H_4   = 9 then H32H_4 = .;
    if H32H_5   = 9 then H32H_5 = .;
    if H32H_6   = 9 then H32H_6 = .;
    if H32I_1   = 9 then H32I_1 = .;
    if H32I_2   = 9 then H32I_2 = .;
    if H32I_3   = 9 then H32I_3 = .;
    if H32I_4   = 9 then H32I_4 = .;
    if H32I_5   = 9 then H32I_5 = .;
    if H32I_6   = 9 then H32I_6 = .;
    if H32J_1   = 9 then H32J_1 = .;
    if H32J_2   = 9 then H32J_2 = .;
    if H32J_3   = 9 then H32J_3 = .;
    if H32J_4   = 9 then H32J_4 = .;
    if H32J_5   = 9 then H32J_5 = .;
    if H32J_6   = 9 then H32J_6 = .;
    if H32K_1   = 9 then H32K_1 = .;
    if H32K_2   = 9 then H32K_2 = .;
    if H32K_3   = 9 then H32K_3 = .;
    if H32K_4   = 9 then H32K_4 = .;
    if H32K_5   = 9 then H32K_5 = .;
    if H32K_6   = 9 then H32K_6 = .;
    if H32L_1   = 9 then H32L_1 = .;
    if H32L_2   = 9 then H32L_2 = .;
    if H32L_3   = 9 then H32L_3 = .;
    if H32L_4   = 9 then H32L_4 = .;
    if H32L_5   = 9 then H32L_5 = .;
    if H32L_6   = 9 then H32L_6 = .;
    if H32M_1   = 9 then H32M_1 = .;
    if H32M_2   = 9 then H32M_2 = .;
    if H32M_3   = 9 then H32M_3 = .;
    if H32M_4   = 9 then H32M_4 = .;
    if H32M_5   = 9 then H32M_5 = .;
    if H32M_6   = 9 then H32M_6 = .;
    if H32N_1   = 9 then H32N_1 = .;
    if H32N_2   = 9 then H32N_2 = .;
    if H32N_3   = 9 then H32N_3 = .;
    if H32N_4   = 9 then H32N_4 = .;
    if H32N_5   = 9 then H32N_5 = .;
    if H32N_6   = 9 then H32N_6 = .;
    if H32O_1   = 9 then H32O_1 = .;
    if H32O_2   = 9 then H32O_2 = .;
    if H32O_3   = 9 then H32O_3 = .;
    if H32O_4   = 9 then H32O_4 = .;
    if H32O_5   = 9 then H32O_5 = .;
    if H32O_6   = 9 then H32O_6 = .;
    if H32P_1   = 9 then H32P_1 = .;
    if H32P_2   = 9 then H32P_2 = .;
    if H32P_3   = 9 then H32P_3 = .;
    if H32P_4   = 9 then H32P_4 = .;
    if H32P_5   = 9 then H32P_5 = .;
    if H32P_6   = 9 then H32P_6 = .;
    if H32Q_1   = 9 then H32Q_1 = .;
    if H32Q_2   = 9 then H32Q_2 = .;
    if H32Q_3   = 9 then H32Q_3 = .;
    if H32Q_4   = 9 then H32Q_4 = .;
    if H32Q_5   = 9 then H32Q_5 = .;
    if H32Q_6   = 9 then H32Q_6 = .;
    if H32R_1   = 9 then H32R_1 = .;
    if H32R_2   = 9 then H32R_2 = .;
    if H32R_3   = 9 then H32R_3 = .;
    if H32R_4   = 9 then H32R_4 = .;
    if H32R_5   = 9 then H32R_5 = .;
    if H32R_6   = 9 then H32R_6 = .;
    if H32S_1   = 9 then H32S_1 = .;
    if H32S_2   = 9 then H32S_2 = .;
    if H32S_3   = 9 then H32S_3 = .;
    if H32S_4   = 9 then H32S_4 = .;
    if H32S_5   = 9 then H32S_5 = .;
    if H32S_6   = 9 then H32S_6 = .;
    if H32T_1   = 9 then H32T_1 = .;
    if H32T_2   = 9 then H32T_2 = .;
    if H32T_3   = 9 then H32T_3 = .;
    if H32T_4   = 9 then H32T_4 = .;
    if H32T_5   = 9 then H32T_5 = .;
    if H32T_6   = 9 then H32T_6 = .;
    if H32U_1   = 9 then H32U_1 = .;
    if H32U_2   = 9 then H32U_2 = .;
    if H32U_3   = 9 then H32U_3 = .;
    if H32U_4   = 9 then H32U_4 = .;
    if H32U_5   = 9 then H32U_5 = .;
    if H32U_6   = 9 then H32U_6 = .;
    if H32V_1   = 9 then H32V_1 = .;
    if H32V_2   = 9 then H32V_2 = .;
    if H32V_3   = 9 then H32V_3 = .;
    if H32V_4   = 9 then H32V_4 = .;
    if H32V_5   = 9 then H32V_5 = .;
    if H32V_6   = 9 then H32V_6 = .;
    if H32W_1   = 9 then H32W_1 = .;
    if H32W_2   = 9 then H32W_2 = .;
    if H32W_3   = 9 then H32W_3 = .;
    if H32W_4   = 9 then H32W_4 = .;
    if H32W_5   = 9 then H32W_5 = .;
    if H32W_6   = 9 then H32W_6 = .;
    if H32X_1   = 9 then H32X_1 = .;
    if H32X_2   = 9 then H32X_2 = .;
    if H32X_3   = 9 then H32X_3 = .;
    if H32X_4   = 9 then H32X_4 = .;
    if H32X_5   = 9 then H32X_5 = .;
    if H32X_6   = 9 then H32X_6 = .;
    if H32Y_1   = 9 then H32Y_1 = .;
    if H32Y_2   = 9 then H32Y_2 = .;
    if H32Y_3   = 9 then H32Y_3 = .;
    if H32Y_4   = 9 then H32Y_4 = .;
    if H32Y_5   = 9 then H32Y_5 = .;
    if H32Y_6   = 9 then H32Y_6 = .;
    if H32Z_1   = 9 then H32Z_1 = .;
    if H32Z_2   = 9 then H32Z_2 = .;
    if H32Z_3   = 9 then H32Z_3 = .;
    if H32Z_4   = 9 then H32Z_4 = .;
    if H32Z_5   = 9 then H32Z_5 = .;
    if H32Z_6   = 9 then H32Z_6 = .;
    if H34_1    = 9 then H34_1 = .;
    if H34_2    = 9 then H34_2 = .;
    if H34_3    = 9 then H34_3 = .;
    if H34_4    = 9 then H34_4 = .;
    if H34_5    = 9 then H34_5 = .;
    if H34_6    = 9 then H34_6 = .;
    if H37A_1   = 9 then H37A_1 = .;
    if H37A_2   = 9 then H37A_2 = .;
    if H37A_3   = 9 then H37A_3 = .;
    if H37A_4   = 9 then H37A_4 = .;
    if H37A_5   = 9 then H37A_5 = .;
    if H37A_6   = 9 then H37A_6 = .;
    if H37B_1   = 9 then H37B_1 = .;
    if H37B_2   = 9 then H37B_2 = .;
    if H37B_3   = 9 then H37B_3 = .;
    if H37B_4   = 9 then H37B_4 = .;
    if H37B_5   = 9 then H37B_5 = .;
    if H37B_6   = 9 then H37B_6 = .;
    if H37C_1   = 9 then H37C_1 = .;
    if H37C_2   = 9 then H37C_2 = .;
    if H37C_3   = 9 then H37C_3 = .;
    if H37C_4   = 9 then H37C_4 = .;
    if H37C_5   = 9 then H37C_5 = .;
    if H37C_6   = 9 then H37C_6 = .;
    if H37D_1   = 9 then H37D_1 = .;
    if H37D_2   = 9 then H37D_2 = .;
    if H37D_3   = 9 then H37D_3 = .;
    if H37D_4   = 9 then H37D_4 = .;
    if H37D_5   = 9 then H37D_5 = .;
    if H37D_6   = 9 then H37D_6 = .;
    if H37DA_1  = 9 then H37DA_1 = .;
    if H37DA_2  = 9 then H37DA_2 = .;
    if H37DA_3  = 9 then H37DA_3 = .;
    if H37DA_4  = 9 then H37DA_4 = .;
    if H37DA_5  = 9 then H37DA_5 = .;
    if H37DA_6  = 9 then H37DA_6 = .;
    if H37E_1   = 9 then H37E_1 = .;
    if H37E_2   = 9 then H37E_2 = .;
    if H37E_3   = 9 then H37E_3 = .;
    if H37E_4   = 9 then H37E_4 = .;
    if H37E_5   = 9 then H37E_5 = .;
    if H37E_6   = 9 then H37E_6 = .;
    if H37AA_1  = 9 then H37AA_1 = .;
    if H37AA_2  = 9 then H37AA_2 = .;
    if H37AA_3  = 9 then H37AA_3 = .;
    if H37AA_4  = 9 then H37AA_4 = .;
    if H37AA_5  = 9 then H37AA_5 = .;
    if H37AA_6  = 9 then H37AA_6 = .;
    if H37AB_1  = 9 then H37AB_1 = .;
    if H37AB_2  = 9 then H37AB_2 = .;
    if H37AB_3  = 9 then H37AB_3 = .;
    if H37AB_4  = 9 then H37AB_4 = .;
    if H37AB_5  = 9 then H37AB_5 = .;
    if H37AB_6  = 9 then H37AB_6 = .;
    if H37F_1   = 9 then H37F_1 = .;
    if H37F_2   = 9 then H37F_2 = .;
    if H37F_3   = 9 then H37F_3 = .;
    if H37F_4   = 9 then H37F_4 = .;
    if H37F_5   = 9 then H37F_5 = .;
    if H37F_6   = 9 then H37F_6 = .;
    if H37G_1   = 9 then H37G_1 = .;
    if H37G_2   = 9 then H37G_2 = .;
    if H37G_3   = 9 then H37G_3 = .;
    if H37G_4   = 9 then H37G_4 = .;
    if H37G_5   = 9 then H37G_5 = .;
    if H37G_6   = 9 then H37G_6 = .;
    if H37H_1   = 9 then H37H_1 = .;
    if H37H_2   = 9 then H37H_2 = .;
    if H37H_3   = 9 then H37H_3 = .;
    if H37H_4   = 9 then H37H_4 = .;
    if H37H_5   = 9 then H37H_5 = .;
    if H37H_6   = 9 then H37H_6 = .;
    if H37I_1   = 9 then H37I_1 = .;
    if H37I_2   = 9 then H37I_2 = .;
    if H37I_3   = 9 then H37I_3 = .;
    if H37I_4   = 9 then H37I_4 = .;
    if H37I_5   = 9 then H37I_5 = .;
    if H37I_6   = 9 then H37I_6 = .;
    if H37J_1   = 9 then H37J_1 = .;
    if H37J_2   = 9 then H37J_2 = .;
    if H37J_3   = 9 then H37J_3 = .;
    if H37J_4   = 9 then H37J_4 = .;
    if H37J_5   = 9 then H37J_5 = .;
    if H37J_6   = 9 then H37J_6 = .;
    if H37K_1   = 9 then H37K_1 = .;
    if H37K_2   = 9 then H37K_2 = .;
    if H37K_3   = 9 then H37K_3 = .;
    if H37K_4   = 9 then H37K_4 = .;
    if H37K_5   = 9 then H37K_5 = .;
    if H37K_6   = 9 then H37K_6 = .;
    if H37L_1   = 9 then H37L_1 = .;
    if H37L_2   = 9 then H37L_2 = .;
    if H37L_3   = 9 then H37L_3 = .;
    if H37L_4   = 9 then H37L_4 = .;
    if H37L_5   = 9 then H37L_5 = .;
    if H37L_6   = 9 then H37L_6 = .;
    if H37M_1   = 9 then H37M_1 = .;
    if H37M_2   = 9 then H37M_2 = .;
    if H37M_3   = 9 then H37M_3 = .;
    if H37M_4   = 9 then H37M_4 = .;
    if H37M_5   = 9 then H37M_5 = .;
    if H37M_6   = 9 then H37M_6 = .;
    if H37N_1   = 9 then H37N_1 = .;
    if H37N_2   = 9 then H37N_2 = .;
    if H37N_3   = 9 then H37N_3 = .;
    if H37N_4   = 9 then H37N_4 = .;
    if H37N_5   = 9 then H37N_5 = .;
    if H37N_6   = 9 then H37N_6 = .;
    if H37O_1   = 9 then H37O_1 = .;
    if H37O_2   = 9 then H37O_2 = .;
    if H37O_3   = 9 then H37O_3 = .;
    if H37O_4   = 9 then H37O_4 = .;
    if H37O_5   = 9 then H37O_5 = .;
    if H37O_6   = 9 then H37O_6 = .;
    if H37P_1   = 9 then H37P_1 = .;
    if H37P_2   = 9 then H37P_2 = .;
    if H37P_3   = 9 then H37P_3 = .;
    if H37P_4   = 9 then H37P_4 = .;
    if H37P_5   = 9 then H37P_5 = .;
    if H37P_6   = 9 then H37P_6 = .;
    if H37X_1   = 9 then H37X_1 = .;
    if H37X_2   = 9 then H37X_2 = .;
    if H37X_3   = 9 then H37X_3 = .;
    if H37X_4   = 9 then H37X_4 = .;
    if H37X_5   = 9 then H37X_5 = .;
    if H37X_6   = 9 then H37X_6 = .;
    if H37Y_1   = 9 then H37Y_1 = .;
    if H37Y_2   = 9 then H37Y_2 = .;
    if H37Y_3   = 9 then H37Y_3 = .;
    if H37Y_4   = 9 then H37Y_4 = .;
    if H37Y_5   = 9 then H37Y_5 = .;
    if H37Y_6   = 9 then H37Y_6 = .;
    if H37Z_1   = 9 then H37Z_1 = .;
    if H37Z_2   = 9 then H37Z_2 = .;
    if H37Z_3   = 9 then H37Z_3 = .;
    if H37Z_4   = 9 then H37Z_4 = .;
    if H37Z_5   = 9 then H37Z_5 = .;
    if H37Z_6   = 9 then H37Z_6 = .;
    if H38_1    = 9 then H38_1 = .;
    if H38_2    = 9 then H38_2 = .;
    if H38_3    = 9 then H38_3 = .;
    if H38_4    = 9 then H38_4 = .;
    if H38_5    = 9 then H38_5 = .;
    if H38_6    = 9 then H38_6 = .;
    if H39_1    = 9 then H39_1 = .;
    if H39_2    = 9 then H39_2 = .;
    if H39_3    = 9 then H39_3 = .;
    if H39_4    = 9 then H39_4 = .;
    if H39_5    = 9 then H39_5 = .;
    if H39_6    = 9 then H39_6 = .;
    if H42_1    = 9 then H42_1 = .;
    if H42_2    = 9 then H42_2 = .;
    if H42_3    = 9 then H42_3 = .;
    if H42_4    = 9 then H42_4 = .;
    if H42_5    = 9 then H42_5 = .;
    if H42_6    = 9 then H42_6 = .;
    if H43_1    = 9 then H43_1 = .;
    if H43_2    = 9 then H43_2 = .;
    if H43_3    = 9 then H43_3 = .;
    if H43_4    = 9 then H43_4 = .;
    if H43_5    = 9 then H43_5 = .;
    if H43_6    = 9 then H43_6 = .;
    if H44A_1   = 99 then H44A_1 = .;
    if H44A_2   = 99 then H44A_2 = .;
    if H44A_3   = 99 then H44A_3 = .;
    if H44A_4   = 99 then H44A_4 = .;
    if H44A_5   = 99 then H44A_5 = .;
    if H44A_6   = 99 then H44A_6 = .;
    if H44B_1   = 99 then H44B_1 = .;
    if H44B_2   = 99 then H44B_2 = .;
    if H44B_3   = 99 then H44B_3 = .;
    if H44B_4   = 99 then H44B_4 = .;
    if H44B_5   = 99 then H44B_5 = .;
    if H44B_6   = 99 then H44B_6 = .;
    if H44C_1   = 9 then H44C_1 = .;
    if H44C_2   = 9 then H44C_2 = .;
    if H44C_3   = 9 then H44C_3 = .;
    if H44C_4   = 9 then H44C_4 = .;
    if H44C_5   = 9 then H44C_5 = .;
    if H44C_6   = 9 then H44C_6 = .;
    if H45_1    = 99 then H45_1 = .;
    if H45_2    = 99 then H45_2 = .;
    if H45_3    = 99 then H45_3 = .;
    if H45_4    = 99 then H45_4 = .;
    if H45_5    = 99 then H45_5 = .;
    if H45_6    = 99 then H45_6 = .;
    if H46A_1   = 99 then H46A_1 = .;
    if H46A_2   = 99 then H46A_2 = .;
    if H46A_3   = 99 then H46A_3 = .;
    if H46A_4   = 99 then H46A_4 = .;
    if H46A_5   = 99 then H46A_5 = .;
    if H46A_6   = 99 then H46A_6 = .;
    if H46B_1   = 99 then H46B_1 = .;
    if H46B_2   = 99 then H46B_2 = .;
    if H46B_3   = 99 then H46B_3 = .;
    if H46B_4   = 99 then H46B_4 = .;
    if H46B_5   = 99 then H46B_5 = .;
    if H46B_6   = 99 then H46B_6 = .;
    if H47_1    = 9 then H47_1 = .;
    if H47_2    = 9 then H47_2 = .;
    if H47_3    = 9 then H47_3 = .;
    if H47_4    = 9 then H47_4 = .;
    if H47_5    = 9 then H47_5 = .;
    if H47_6    = 9 then H47_6 = .;
    if HW2_1    = 9999 then HW2_1 = .;
    if HW2_2    = 9999 then HW2_2 = .;
    if HW2_3    = 9999 then HW2_3 = .;
    if HW2_4    = 9999 then HW2_4 = .;
    if HW2_5    = 9999 then HW2_5 = .;
    if HW2_6    = 9999 then HW2_6 = .;
    if HW3_1    = 9999 then HW3_1 = .;
    if HW3_2    = 9999 then HW3_2 = .;
    if HW3_3    = 9999 then HW3_3 = .;
    if HW3_4    = 9999 then HW3_4 = .;
    if HW3_5    = 9999 then HW3_5 = .;
    if HW3_6    = 9999 then HW3_6 = .;
    if HW4_1    = 9999 then HW4_1 = .;
    if HW4_2    = 9999 then HW4_2 = .;
    if HW4_3    = 9999 then HW4_3 = .;
    if HW4_4    = 9999 then HW4_4 = .;
    if HW4_5    = 9999 then HW4_5 = .;
    if HW4_6    = 9999 then HW4_6 = .;
    if HW5_1    = 9999 then HW5_1 = .;
    if HW5_2    = 9999 then HW5_2 = .;
    if HW5_3    = 9999 then HW5_3 = .;
    if HW5_4    = 9999 then HW5_4 = .;
    if HW5_5    = 9999 then HW5_5 = .;
    if HW5_6    = 9999 then HW5_6 = .;
    if HW6_1    = 99999 then HW6_1 = .;
    if HW6_2    = 99999 then HW6_2 = .;
    if HW6_3    = 99999 then HW6_3 = .;
    if HW6_4    = 99999 then HW6_4 = .;
    if HW6_5    = 99999 then HW6_5 = .;
    if HW6_6    = 99999 then HW6_6 = .;
    if HW7_1    = 9999 then HW7_1 = .;
    if HW7_2    = 9999 then HW7_2 = .;
    if HW7_3    = 9999 then HW7_3 = .;
    if HW7_4    = 9999 then HW7_4 = .;
    if HW7_5    = 9999 then HW7_5 = .;
    if HW7_6    = 9999 then HW7_6 = .;
    if HW8_1    = 9999 then HW8_1 = .;
    if HW8_2    = 9999 then HW8_2 = .;
    if HW8_3    = 9999 then HW8_3 = .;
    if HW8_4    = 9999 then HW8_4 = .;
    if HW8_5    = 9999 then HW8_5 = .;
    if HW8_6    = 9999 then HW8_6 = .;
    if HW9_1    = 99999 then HW9_1 = .;
    if HW9_2    = 99999 then HW9_2 = .;
    if HW9_3    = 99999 then HW9_3 = .;
    if HW9_4    = 99999 then HW9_4 = .;
    if HW9_5    = 99999 then HW9_5 = .;
    if HW9_6    = 99999 then HW9_6 = .;
    if HW10_1   = 9999 then HW10_1 = .;
    if HW10_2   = 9999 then HW10_2 = .;
    if HW10_3   = 9999 then HW10_3 = .;
    if HW10_4   = 9999 then HW10_4 = .;
    if HW10_5   = 9999 then HW10_5 = .;
    if HW10_6   = 9999 then HW10_6 = .;
    if HW11_1   = 9999 then HW11_1 = .;
    if HW11_2   = 9999 then HW11_2 = .;
    if HW11_3   = 9999 then HW11_3 = .;
    if HW11_4   = 9999 then HW11_4 = .;
    if HW11_5   = 9999 then HW11_5 = .;
    if HW11_6   = 9999 then HW11_6 = .;
    if HW12_1   = 99999 then HW12_1 = .;
    if HW12_2   = 99999 then HW12_2 = .;
    if HW12_3   = 99999 then HW12_3 = .;
    if HW12_4   = 99999 then HW12_4 = .;
    if HW12_5   = 99999 then HW12_5 = .;
    if HW12_6   = 99999 then HW12_6 = .;
    if HW13_1   = 9 then HW13_1 = .;
    if HW13_2   = 9 then HW13_2 = .;
    if HW13_3   = 9 then HW13_3 = .;
    if HW13_4   = 9 then HW13_4 = .;
    if HW13_5   = 9 then HW13_5 = .;
    if HW13_6   = 9 then HW13_6 = .;
    if HW15_1   = 9 then HW15_1 = .;
    if HW15_2   = 9 then HW15_2 = .;
    if HW15_3   = 9 then HW15_3 = .;
    if HW15_4   = 9 then HW15_4 = .;
    if HW15_5   = 9 then HW15_5 = .;
    if HW15_6   = 9 then HW15_6 = .;
    if HW16_1   = 99 then HW16_1 = .;
    if HW16_2   = 99 then HW16_2 = .;
    if HW16_3   = 99 then HW16_3 = .;
    if HW16_4   = 99 then HW16_4 = .;
    if HW16_5   = 99 then HW16_5 = .;
    if HW16_6   = 99 then HW16_6 = .;
    if HW51_1   = 99 then HW51_1 = .;
    if HW51_2   = 99 then HW51_2 = .;
    if HW51_3   = 99 then HW51_3 = .;
    if HW51_4   = 99 then HW51_4 = .;
    if HW51_5   = 99 then HW51_5 = .;
    if HW51_6   = 99 then HW51_6 = .;
    if HW52_1   = 9 then HW52_1 = .;
    if HW52_2   = 9 then HW52_2 = .;
    if HW52_3   = 9 then HW52_3 = .;
    if HW52_4   = 9 then HW52_4 = .;
    if HW52_5   = 9 then HW52_5 = .;
    if HW52_6   = 9 then HW52_6 = .;
    if HW53_1   = 999 then HW53_1 = .;
    if HW53_2   = 999 then HW53_2 = .;
    if HW53_3   = 999 then HW53_3 = .;
    if HW53_4   = 999 then HW53_4 = .;
    if HW53_5   = 999 then HW53_5 = .;
    if HW53_6   = 999 then HW53_6 = .;
    if HW55_1   = 9 then HW55_1 = .;
    if HW55_2   = 9 then HW55_2 = .;
    if HW55_3   = 9 then HW55_3 = .;
    if HW55_4   = 9 then HW55_4 = .;
    if HW55_5   = 9 then HW55_5 = .;
    if HW55_6   = 9 then HW55_6 = .;
    if HW56_1   = 9999 then HW56_1 = .;
    if HW56_2   = 9999 then HW56_2 = .;
    if HW56_3   = 9999 then HW56_3 = .;
    if HW56_4   = 9999 then HW56_4 = .;
    if HW56_5   = 9999 then HW56_5 = .;
    if HW56_6   = 9999 then HW56_6 = .;
    if HW57_1   = 9 then HW57_1 = .;
    if HW57_2   = 9 then HW57_2 = .;
    if HW57_3   = 9 then HW57_3 = .;
    if HW57_4   = 9 then HW57_4 = .;
    if HW57_5   = 9 then HW57_5 = .;
    if HW57_6   = 9 then HW57_6 = .;
    if HW58_1   = 9 then HW58_1 = .;
    if HW58_2   = 9 then HW58_2 = .;
    if HW58_3   = 9 then HW58_3 = .;
    if HW58_4   = 9 then HW58_4 = .;
    if HW58_5   = 9 then HW58_5 = .;
    if HW58_6   = 9 then HW58_6 = .;
    if HW70_1   = 9999 then HW70_1 = .;
    if HW70_2   = 9999 then HW70_2 = .;
    if HW70_3   = 9999 then HW70_3 = .;
    if HW70_4   = 9999 then HW70_4 = .;
    if HW70_5   = 9999 then HW70_5 = .;
    if HW70_6   = 9999 then HW70_6 = .;
    if HW71_1   = 9999 then HW71_1 = .;
    if HW71_2   = 9999 then HW71_2 = .;
    if HW71_3   = 9999 then HW71_3 = .;
    if HW71_4   = 9999 then HW71_4 = .;
    if HW71_5   = 9999 then HW71_5 = .;
    if HW71_6   = 9999 then HW71_6 = .;
    if HW72_1   = 9999 then HW72_1 = .;
    if HW72_2   = 9999 then HW72_2 = .;
    if HW72_3   = 9999 then HW72_3 = .;
    if HW72_4   = 9999 then HW72_4 = .;
    if HW72_5   = 9999 then HW72_5 = .;
    if HW72_6   = 9999 then HW72_6 = .;
    if HW73_1   = 9999 then HW73_1 = .;
    if HW73_2   = 9999 then HW73_2 = .;
    if HW73_3   = 9999 then HW73_3 = .;
    if HW73_4   = 9999 then HW73_4 = .;
    if HW73_5   = 9999 then HW73_5 = .;
    if HW73_6   = 9999 then HW73_6 = .;
    if V501     = 9 then V501 = .;
    if V502     = 9 then V502 = .;
    if V503     = 9 then V503 = .;
    if V504     = 9 then V504 = .;
    if V505     = 99 then V505 = .;
    if V506     = 99 then V506 = .;
    if V525     = 99 then V525 = .;
    if V527     = 999 then V527 = .;
    if V528     = 99 then V528 = .;
    if V529     = 999 then V529 = .;
    if V531     = 99 then V531 = .;
    if V535     = 9 then V535 = .;
    if V536     = 9 then V536 = .;
    if V537     = 99 then V537 = .;
    if V538     = 9 then V538 = .;
    if V539     = 9 then V539 = .;
    if V540     = 9 then V540 = .;
    if V541     = 9 then V541 = .;
    if V602     = 9 then V602 = .;
    if V603     = 999 then V603 = .;
    if V604     = 9 then V604 = .;
    if V605     = 9 then V605 = .;
    if V613     = 99 then V613 = .;
    if V616     = 999 then V616 = .;
    if V621     = 9 then V621 = .;
    if V623     = 9 then V623 = .;
    if V624     = 99 then V624 = .;
    if V625     = 9 then V625 = .;
    if V626     = 99 then V626 = .;
    if V625A    = 9 then V625A = .;
    if V626A    = 99 then V626A = .;
    if V627     = 99 then V627 = .;
    if V628     = 99 then V628 = .;
    if V629     = 99 then V629 = .;
    if V631     = 9 then V631 = .;
    if V632     = 9 then V632 = .;
    if V632A    = 9 then V632A = .;
    if V633A    = 9 then V633A = .;
    if V633B    = 9 then V633B = .;
    if V633C    = 9 then V633C = .;
    if V633D    = 9 then V633D = .;
    if V633E    = 9 then V633E = .;
    if V633F    = 9 then V633F = .;
    if V633G    = 9 then V633G = .;
    if V634     = 9 then V634 = .;
    if V701     = 9 then V701 = .;
    if V702     = 99 then V702 = .;
    if V704     = 99999 then V704 = .;
    if V704A    = 9 then V704A = .;
    if V705     = 99 then V705 = .;
    if V714     = 9 then V714 = .;
    if V714A    = 9 then V714A = .;
    if V715     = 99 then V715 = .;
    if V716     = 99999 then V716 = .;
    if V717     = 99 then V717 = .;
    if V719     = 9 then V719 = .;
    if V721     = 9 then V721 = .;
    if V729     = 9 then V729 = .;
    if V730     = 99 then V730 = .;
    if V731     = 9 then V731 = .;
    if V732     = 9 then V732 = .;
    if V739     = 9 then V739 = .;
    if V740     = 9 then V740 = .;
    if V741     = 9 then V741 = .;
    if V743A    = 9 then V743A = .;
    if V743B    = 9 then V743B = .;
    if V743C    = 9 then V743C = .;
    if V743D    = 9 then V743D = .;
    if V743E    = 9 then V743E = .;
    if V743F    = 9 then V743F = .;
    if V744A    = 9 then V744A = .;
    if V744B    = 9 then V744B = .;
    if V744C    = 9 then V744C = .;
    if V744D    = 9 then V744D = .;
    if V744E    = 9 then V744E = .;
    if V745A    = 9 then V745A = .;
    if V745B    = 9 then V745B = .;
    if V745C    = 9 then V745C = .;
    if V745D    = 9 then V745D = .;
    if V746     = 9 then V746 = .;
    if V750     = 9 then V750 = .;
    if V751     = 9 then V751 = .;
    if V754BP   = 9 then V754BP = .;
    if V754CP   = 9 then V754CP = .;
    if V754DP   = 9 then V754DP = .;
    if V754JP   = 9 then V754JP = .;
    if V754WP   = 9 then V754WP = .;
    if V756     = 9 then V756 = .;
    if V761     = 9 then V761 = .;
    if V761B    = 9 then V761B = .;
    if V761C    = 9 then V761C = .;
    if V762     = 99 then V762 = .;
    if V762A    = 99 then V762A = .;
    if V762AA   = 9 then V762AA = .;
    if V762AB   = 9 then V762AB = .;
    if V762AC   = 9 then V762AC = .;
    if V762AD   = 9 then V762AD = .;
    if V762AE   = 9 then V762AE = .;
    if V762AF   = 9 then V762AF = .;
    if V762AG   = 9 then V762AG = .;
    if V762AH   = 9 then V762AH = .;
    if V762AI   = 9 then V762AI = .;
    if V762AJ   = 9 then V762AJ = .;
    if V762AK   = 9 then V762AK = .;
    if V762AL   = 9 then V762AL = .;
    if V762AM   = 9 then V762AM = .;
    if V762AN   = 9 then V762AN = .;
    if V762AO   = 9 then V762AO = .;
    if V762AP   = 9 then V762AP = .;
    if V762AQ   = 9 then V762AQ = .;
    if V762AR   = 9 then V762AR = .;
    if V762AS   = 9 then V762AS = .;
    if V762AT   = 9 then V762AT = .;
    if V762AU   = 9 then V762AU = .;
    if V762AV   = 9 then V762AV = .;
    if V762AW   = 9 then V762AW = .;
    if V762AX   = 9 then V762AX = .;
    if V762AZ   = 9 then V762AZ = .;
    if V762BA   = 9 then V762BA = .;
    if V762BB   = 9 then V762BB = .;
    if V762BC   = 9 then V762BC = .;
    if V762BD   = 9 then V762BD = .;
    if V762BE   = 9 then V762BE = .;
    if V762BF   = 9 then V762BF = .;
    if V762BG   = 9 then V762BG = .;
    if V762BH   = 9 then V762BH = .;
    if V762BI   = 9 then V762BI = .;
    if V762BJ   = 9 then V762BJ = .;
    if V762BK   = 9 then V762BK = .;
    if V762BL   = 9 then V762BL = .;
    if V762BM   = 9 then V762BM = .;
    if V762BN   = 9 then V762BN = .;
    if V762BO   = 9 then V762BO = .;
    if V762BP   = 9 then V762BP = .;
    if V762BQ   = 9 then V762BQ = .;
    if V762BR   = 9 then V762BR = .;
    if V762BS   = 9 then V762BS = .;
    if V762BT   = 9 then V762BT = .;
    if V762BU   = 9 then V762BU = .;
    if V762BV   = 9 then V762BV = .;
    if V762BW   = 9 then V762BW = .;
    if V762BX   = 9 then V762BX = .;
    if V762BZ   = 9 then V762BZ = .;
    if V763A    = 9 then V763A = .;
    if V763B    = 9 then V763B = .;
    if V763C    = 9 then V763C = .;
    if V763D    = 9 then V763D = .;
    if V763E    = 9 then V763E = .;
    if V763F    = 9 then V763F = .;
    if V763G    = 9 then V763G = .;
    if V766A    = 99 then V766A = .;
    if V766B    = 99 then V766B = .;
    if V767A    = 99 then V767A = .;
    if V767B    = 99 then V767B = .;
    if V767C    = 99 then V767C = .;
    if V768A    = 999 then V768A = .;
    if V768B    = 999 then V768B = .;
    if V768C    = 999 then V768C = .;
    if V769     = 9 then V769 = .;
    if V769A    = 9 then V769A = .;
    if V770     = 9 then V770 = .;
    if V770A    = 9 then V770A = .;
    if V770B    = 9 then V770B = .;
    if V770C    = 9 then V770C = .;
    if V770D    = 9 then V770D = .;
    if V770E    = 9 then V770E = .;
    if V770F    = 9 then V770F = .;
    if V770G    = 9 then V770G = .;
    if V770H    = 9 then V770H = .;
    if V770I    = 9 then V770I = .;
    if V770J    = 9 then V770J = .;
    if V770K    = 9 then V770K = .;
    if V770L    = 9 then V770L = .;
    if V770M    = 9 then V770M = .;
    if V770N    = 9 then V770N = .;
    if V770O    = 9 then V770O = .;
    if V770P    = 9 then V770P = .;
    if V770Q    = 9 then V770Q = .;
    if V770R    = 9 then V770R = .;
    if V770S    = 9 then V770S = .;
    if V770T    = 9 then V770T = .;
    if V770U    = 9 then V770U = .;
    if V770V    = 9 then V770V = .;
    if V770W    = 9 then V770W = .;
    if V770X    = 9 then V770X = .;
    if V774A    = 9 then V774A = .;
    if V774B    = 9 then V774B = .;
    if V774C    = 9 then V774C = .;
    if V775     = 9 then V775 = .;
    if V777     = 9 then V777 = .;
    if V777A    = 9 then V777A = .;
    if V778     = 9 then V778 = .;
    if V779     = 9 then V779 = .;
    if V780     = 9 then V780 = .;
    if V781     = 9 then V781 = .;
    if V783     = 9 then V783 = .;
    if V784A    = 9 then V784A = .;
    if V784B    = 9 then V784B = .;
    if V784C    = 9 then V784C = .;
    if V784D    = 9 then V784D = .;
    if V784E    = 9 then V784E = .;
    if V784F    = 9 then V784F = .;
    if V784G    = 9 then V784G = .;
    if V784H    = 9 then V784H = .;
    if V784I    = 9 then V784I = .;
    if V784J    = 9 then V784J = .;
    if V784K    = 9 then V784K = .;
    if V784L    = 9 then V784L = .;
    if V784M    = 9 then V784M = .;
    if V784N    = 9 then V784N = .;
    if V784O    = 9 then V784O = .;
    if V784P    = 9 then V784P = .;
    if V784Q    = 9 then V784Q = .;
    if V784R    = 9 then V784R = .;
    if V784S    = 9 then V784S = .;
    if V784T    = 9 then V784T = .;
    if V784U    = 9 then V784U = .;
    if V784V    = 9 then V784V = .;
    if V784X    = 9 then V784X = .;
    if V785     = 9 then V785 = .;
    if V791A    = 9 then V791A = .;
    if V820     = 9 then V820 = .;
    if V821A    = 9 then V821A = .;
    if V821B    = 9 then V821B = .;
    if V821C    = 9 then V821C = .;
    if V822     = 9 then V822 = .;
    if V823     = 9 then V823 = .;
    if V824     = 9 then V824 = .;
    if V825     = 9 then V825 = .;
    if V826     = 9 then V826 = .;
    if V826A    = 99 then V826A = .;
    if V827     = 9 then V827 = .;
    if V828     = 9 then V828 = .;
    if V829     = 99 then V829 = .;
    if V830     = 99 then V830 = .;
    if V831     = 9 then V831 = .;
    if V832B    = 999 then V832B = .;
    if V832C    = 999 then V832C = .;
    if V833A    = 9 then V833A = .;
    if V833B    = 9 then V833B = .;
    if V833C    = 9 then V833C = .;
    if V834A    = 99 then V834A = .;
    if V834B    = 99 then V834B = .;
    if V834C    = 99 then V834C = .;
    if V835A    = 9 then V835A = .;
    if V835B    = 9 then V835B = .;
    if V835C    = 9 then V835C = .;
    if V836     = 99 then V836 = .;
    if V837     = 9 then V837 = .;
    if V838A    = 9 then V838A = .;
    if V838B    = 9 then V838B = .;
    if V838C    = 9 then V838C = .;
    if V839     = 9 then V839 = .;
    if V839A    = 9 then V839A = .;
    if V840     = 9 then V840 = .;
    if V840A    = 9 then V840A = .;
    if V841     = 9 then V841 = .;
    if V841A    = 9 then V841A = .;
    if V842     = 99 then V842 = .;
    if V843     = 9 then V843 = .;
    if V844     = 9 then V844 = .;
    if V845     = 9 then V845 = .;
    if V846     = 9 then V846 = .;
    if V847     = 9 then V847 = .;
    if V848     = 9 then V848 = .;
    if V849     = 9 then V849 = .;
    if V850A    = 9 then V850A = .;
    if V850B    = 9 then V850B = .;
    if V851A    = 9 then V851A = .;
    if V851B    = 9 then V851B = .;
    if V851C    = 9 then V851C = .;
    if V851D    = 9 then V851D = .;
    if V851E    = 9 then V851E = .;
    if V851F    = 9 then V851F = .;
    if V851G    = 9 then V851G = .;
    if V851H    = 9 then V851H = .;
    if V851I    = 9 then V851I = .;
    if V851J    = 9 then V851J = .;
    if V851K    = 9 then V851K = .;
    if V851L    = 9 then V851L = .;
    if V852A    = 999 then V852A = .;
    if V852B    = 999 then V852B = .;
    if V852C    = 999 then V852C = .;
    if V853A    = 99 then V853A = .;
    if V853B    = 99 then V853B = .;
    if V853C    = 99 then V853C = .;
    if V854A    = 9 then V854A = .;
    if V854B    = 9 then V854B = .;
    if V855     = 9 then V855 = .;
    if V856     = 9 then V856 = .;
    if V857A    = 9 then V857A = .;
    if V857B    = 9 then V857B = .;
    if V857C    = 9 then V857C = .;
    if V857D    = 9 then V857D = .;
    if V858     = 9 then V858 = .;
    if V801     = 9999 then V801 = .;
    if V802     = 9999 then V802 = .;
    if V803     = 99 then V803 = .;
    if V804     = 9 then V804 = .;
    if V806     = 99 then V806 = .;
    if V811     = 9 then V811 = .;
    if V812     = 9 then V812 = .;
    if V813     = 9 then V813 = .;
    if V814     = 9 then V814 = .;
    if V815A    = 9 then V815A = .;
    if V815B    = 9 then V815B = .;
    if V815C    = 9 then V815C = .;
    if MM1_01   = 9 then MM1_01 = .;
    if MM1_02   = 9 then MM1_02 = .;
    if MM1_03   = 9 then MM1_03 = .;
    if MM1_04   = 9 then MM1_04 = .;
    if MM1_05   = 9 then MM1_05 = .;
    if MM1_06   = 9 then MM1_06 = .;
    if MM1_07   = 9 then MM1_07 = .;
    if MM1_08   = 9 then MM1_08 = .;
    if MM1_09   = 9 then MM1_09 = .;
    if MM1_10   = 9 then MM1_10 = .;
    if MM1_11   = 9 then MM1_11 = .;
    if MM1_12   = 9 then MM1_12 = .;
    if MM1_13   = 9 then MM1_13 = .;
    if MM1_14   = 9 then MM1_14 = .;
    if MM1_15   = 9 then MM1_15 = .;
    if MM1_16   = 9 then MM1_16 = .;
    if MM1_17   = 9 then MM1_17 = .;
    if MM1_18   = 9 then MM1_18 = .;
    if MM1_19   = 9 then MM1_19 = .;
    if MM1_20   = 9 then MM1_20 = .;
    if MM2_01   = 9 then MM2_01 = .;
    if MM2_02   = 9 then MM2_02 = .;
    if MM2_03   = 9 then MM2_03 = .;
    if MM2_04   = 9 then MM2_04 = .;
    if MM2_05   = 9 then MM2_05 = .;
    if MM2_06   = 9 then MM2_06 = .;
    if MM2_07   = 9 then MM2_07 = .;
    if MM2_08   = 9 then MM2_08 = .;
    if MM2_09   = 9 then MM2_09 = .;
    if MM2_10   = 9 then MM2_10 = .;
    if MM2_11   = 9 then MM2_11 = .;
    if MM2_12   = 9 then MM2_12 = .;
    if MM2_13   = 9 then MM2_13 = .;
    if MM2_14   = 9 then MM2_14 = .;
    if MM2_15   = 9 then MM2_15 = .;
    if MM2_16   = 9 then MM2_16 = .;
    if MM2_17   = 9 then MM2_17 = .;
    if MM2_18   = 9 then MM2_18 = .;
    if MM2_19   = 9 then MM2_19 = .;
    if MM2_20   = 9 then MM2_20 = .;
    if MM3_01   = 99 then MM3_01 = .;
    if MM3_02   = 99 then MM3_02 = .;
    if MM3_03   = 99 then MM3_03 = .;
    if MM3_04   = 99 then MM3_04 = .;
    if MM3_05   = 99 then MM3_05 = .;
    if MM3_06   = 99 then MM3_06 = .;
    if MM3_07   = 99 then MM3_07 = .;
    if MM3_08   = 99 then MM3_08 = .;
    if MM3_09   = 99 then MM3_09 = .;
    if MM3_10   = 99 then MM3_10 = .;
    if MM3_11   = 99 then MM3_11 = .;
    if MM3_12   = 99 then MM3_12 = .;
    if MM3_13   = 99 then MM3_13 = .;
    if MM3_14   = 99 then MM3_14 = .;
    if MM3_15   = 99 then MM3_15 = .;
    if MM3_16   = 99 then MM3_16 = .;
    if MM3_17   = 99 then MM3_17 = .;
    if MM3_18   = 99 then MM3_18 = .;
    if MM3_19   = 99 then MM3_19 = .;
    if MM3_20   = 99 then MM3_20 = .;
    if MM5_01   = 9 then MM5_01 = .;
    if MM5_02   = 9 then MM5_02 = .;
    if MM5_03   = 9 then MM5_03 = .;
    if MM5_04   = 9 then MM5_04 = .;
    if MM5_05   = 9 then MM5_05 = .;
    if MM5_06   = 9 then MM5_06 = .;
    if MM5_07   = 9 then MM5_07 = .;
    if MM5_08   = 9 then MM5_08 = .;
    if MM5_09   = 9 then MM5_09 = .;
    if MM5_10   = 9 then MM5_10 = .;
    if MM5_11   = 9 then MM5_11 = .;
    if MM5_12   = 9 then MM5_12 = .;
    if MM5_13   = 9 then MM5_13 = .;
    if MM5_14   = 9 then MM5_14 = .;
    if MM5_15   = 9 then MM5_15 = .;
    if MM5_16   = 9 then MM5_16 = .;
    if MM5_17   = 9 then MM5_17 = .;
    if MM5_18   = 9 then MM5_18 = .;
    if MM5_19   = 9 then MM5_19 = .;
    if MM5_20   = 9 then MM5_20 = .;
    if MM6_01   = 99 then MM6_01 = .;
    if MM6_02   = 99 then MM6_02 = .;
    if MM6_03   = 99 then MM6_03 = .;
    if MM6_04   = 99 then MM6_04 = .;
    if MM6_05   = 99 then MM6_05 = .;
    if MM6_06   = 99 then MM6_06 = .;
    if MM6_07   = 99 then MM6_07 = .;
    if MM6_08   = 99 then MM6_08 = .;
    if MM6_09   = 99 then MM6_09 = .;
    if MM6_10   = 99 then MM6_10 = .;
    if MM6_11   = 99 then MM6_11 = .;
    if MM6_12   = 99 then MM6_12 = .;
    if MM6_13   = 99 then MM6_13 = .;
    if MM6_14   = 99 then MM6_14 = .;
    if MM6_15   = 99 then MM6_15 = .;
    if MM6_16   = 99 then MM6_16 = .;
    if MM6_17   = 99 then MM6_17 = .;
    if MM6_18   = 99 then MM6_18 = .;
    if MM6_19   = 99 then MM6_19 = .;
    if MM6_20   = 99 then MM6_20 = .;
    if MM7_01   = 99 then MM7_01 = .;
    if MM7_02   = 99 then MM7_02 = .;
    if MM7_03   = 99 then MM7_03 = .;
    if MM7_04   = 99 then MM7_04 = .;
    if MM7_05   = 99 then MM7_05 = .;
    if MM7_06   = 99 then MM7_06 = .;
    if MM7_07   = 99 then MM7_07 = .;
    if MM7_08   = 99 then MM7_08 = .;
    if MM7_09   = 99 then MM7_09 = .;
    if MM7_10   = 99 then MM7_10 = .;
    if MM7_11   = 99 then MM7_11 = .;
    if MM7_12   = 99 then MM7_12 = .;
    if MM7_13   = 99 then MM7_13 = .;
    if MM7_14   = 99 then MM7_14 = .;
    if MM7_15   = 99 then MM7_15 = .;
    if MM7_16   = 99 then MM7_16 = .;
    if MM7_17   = 99 then MM7_17 = .;
    if MM7_18   = 99 then MM7_18 = .;
    if MM7_19   = 99 then MM7_19 = .;
    if MM7_20   = 99 then MM7_20 = .;
    if MM9_01   = 99 then MM9_01 = .;
    if MM9_02   = 99 then MM9_02 = .;
    if MM9_03   = 99 then MM9_03 = .;
    if MM9_04   = 99 then MM9_04 = .;
    if MM9_05   = 99 then MM9_05 = .;
    if MM9_06   = 99 then MM9_06 = .;
    if MM9_07   = 99 then MM9_07 = .;
    if MM9_08   = 99 then MM9_08 = .;
    if MM9_09   = 99 then MM9_09 = .;
    if MM9_10   = 99 then MM9_10 = .;
    if MM9_11   = 99 then MM9_11 = .;
    if MM9_12   = 99 then MM9_12 = .;
    if MM9_13   = 99 then MM9_13 = .;
    if MM9_14   = 99 then MM9_14 = .;
    if MM9_15   = 99 then MM9_15 = .;
    if MM9_16   = 99 then MM9_16 = .;
    if MM9_17   = 99 then MM9_17 = .;
    if MM9_18   = 99 then MM9_18 = .;
    if MM9_19   = 99 then MM9_19 = .;
    if MM9_20   = 99 then MM9_20 = .;
    if MM10_01  = 9 then MM10_01 = .;
    if MM10_02  = 9 then MM10_02 = .;
    if MM10_03  = 9 then MM10_03 = .;
    if MM10_04  = 9 then MM10_04 = .;
    if MM10_05  = 9 then MM10_05 = .;
    if MM10_06  = 9 then MM10_06 = .;
    if MM10_07  = 9 then MM10_07 = .;
    if MM10_08  = 9 then MM10_08 = .;
    if MM10_09  = 9 then MM10_09 = .;
    if MM10_10  = 9 then MM10_10 = .;
    if MM10_11  = 9 then MM10_11 = .;
    if MM10_12  = 9 then MM10_12 = .;
    if MM10_13  = 9 then MM10_13 = .;
    if MM10_14  = 9 then MM10_14 = .;
    if MM10_15  = 9 then MM10_15 = .;
    if MM10_16  = 9 then MM10_16 = .;
    if MM10_17  = 9 then MM10_17 = .;
    if MM10_18  = 9 then MM10_18 = .;
    if MM10_19  = 9 then MM10_19 = .;
    if MM10_20  = 9 then MM10_20 = .;
    if MM11_01  = 99 then MM11_01 = .;
    if MM11_02  = 99 then MM11_02 = .;
    if MM11_03  = 99 then MM11_03 = .;
    if MM11_04  = 99 then MM11_04 = .;
    if MM11_05  = 99 then MM11_05 = .;
    if MM11_06  = 99 then MM11_06 = .;
    if MM11_07  = 99 then MM11_07 = .;
    if MM11_08  = 99 then MM11_08 = .;
    if MM11_09  = 99 then MM11_09 = .;
    if MM11_10  = 99 then MM11_10 = .;
    if MM11_11  = 99 then MM11_11 = .;
    if MM11_12  = 99 then MM11_12 = .;
    if MM11_13  = 99 then MM11_13 = .;
    if MM11_14  = 99 then MM11_14 = .;
    if MM11_15  = 99 then MM11_15 = .;
    if MM11_16  = 99 then MM11_16 = .;
    if MM11_17  = 99 then MM11_17 = .;
    if MM11_18  = 99 then MM11_18 = .;
    if MM11_19  = 99 then MM11_19 = .;
    if MM11_20  = 99 then MM11_20 = .;
    if MM12_01  = 999 then MM12_01 = .;
    if MM12_02  = 999 then MM12_02 = .;
    if MM12_03  = 999 then MM12_03 = .;
    if MM12_04  = 999 then MM12_04 = .;
    if MM12_05  = 999 then MM12_05 = .;
    if MM12_06  = 999 then MM12_06 = .;
    if MM12_07  = 999 then MM12_07 = .;
    if MM12_08  = 999 then MM12_08 = .;
    if MM12_09  = 999 then MM12_09 = .;
    if MM12_10  = 999 then MM12_10 = .;
    if MM12_11  = 999 then MM12_11 = .;
    if MM12_12  = 999 then MM12_12 = .;
    if MM12_13  = 999 then MM12_13 = .;
    if MM12_14  = 999 then MM12_14 = .;
    if MM12_15  = 999 then MM12_15 = .;
    if MM12_16  = 999 then MM12_16 = .;
    if MM12_17  = 999 then MM12_17 = .;
    if MM12_18  = 999 then MM12_18 = .;
    if MM12_19  = 999 then MM12_19 = .;
    if MM12_20  = 999 then MM12_20 = .;
    if MM13_01  = 99 then MM13_01 = .;
    if MM13_02  = 99 then MM13_02 = .;
    if MM13_03  = 99 then MM13_03 = .;
    if MM13_04  = 99 then MM13_04 = .;
    if MM13_05  = 99 then MM13_05 = .;
    if MM13_06  = 99 then MM13_06 = .;
    if MM13_07  = 99 then MM13_07 = .;
    if MM13_08  = 99 then MM13_08 = .;
    if MM13_09  = 99 then MM13_09 = .;
    if MM13_10  = 99 then MM13_10 = .;
    if MM13_11  = 99 then MM13_11 = .;
    if MM13_12  = 99 then MM13_12 = .;
    if MM13_13  = 99 then MM13_13 = .;
    if MM13_14  = 99 then MM13_14 = .;
    if MM13_15  = 99 then MM13_15 = .;
    if MM13_16  = 99 then MM13_16 = .;
    if MM13_17  = 99 then MM13_17 = .;
    if MM13_18  = 99 then MM13_18 = .;
    if MM13_19  = 99 then MM13_19 = .;
    if MM13_20  = 99 then MM13_20 = .;
    if MM14_01  = 99 then MM14_01 = .;
    if MM14_02  = 99 then MM14_02 = .;
    if MM14_03  = 99 then MM14_03 = .;
    if MM14_04  = 99 then MM14_04 = .;
    if MM14_05  = 99 then MM14_05 = .;
    if MM14_06  = 99 then MM14_06 = .;
    if MM14_07  = 99 then MM14_07 = .;
    if MM14_08  = 99 then MM14_08 = .;
    if MM14_09  = 99 then MM14_09 = .;
    if MM14_10  = 99 then MM14_10 = .;
    if MM14_11  = 99 then MM14_11 = .;
    if MM14_12  = 99 then MM14_12 = .;
    if MM14_13  = 99 then MM14_13 = .;
    if MM14_14  = 99 then MM14_14 = .;
    if MM14_15  = 99 then MM14_15 = .;
    if MM14_16  = 99 then MM14_16 = .;
    if MM14_17  = 99 then MM14_17 = .;
    if MM14_18  = 99 then MM14_18 = .;
    if MM14_19  = 99 then MM14_19 = .;
    if MM14_20  = 99 then MM14_20 = .;
    if MM15_01  = 9999 then MM15_01 = .;
    if MM15_02  = 9999 then MM15_02 = .;
    if MM15_03  = 9999 then MM15_03 = .;
    if MM15_04  = 9999 then MM15_04 = .;
    if MM15_05  = 9999 then MM15_05 = .;
    if MM15_06  = 9999 then MM15_06 = .;
    if MM15_07  = 9999 then MM15_07 = .;
    if MM15_08  = 9999 then MM15_08 = .;
    if MM15_09  = 9999 then MM15_09 = .;
    if MM15_10  = 9999 then MM15_10 = .;
    if MM15_11  = 9999 then MM15_11 = .;
    if MM15_12  = 9999 then MM15_12 = .;
    if MM15_13  = 9999 then MM15_13 = .;
    if MM15_14  = 9999 then MM15_14 = .;
    if MM15_15  = 9999 then MM15_15 = .;
    if MM15_16  = 9999 then MM15_16 = .;
    if MM15_17  = 9999 then MM15_17 = .;
    if MM15_18  = 9999 then MM15_18 = .;
    if MM15_19  = 9999 then MM15_19 = .;
    if MM15_20  = 9999 then MM15_20 = .;
    if MM16_01  = 9 then MM16_01 = .;
    if MM16_02  = 9 then MM16_02 = .;
    if MM16_03  = 9 then MM16_03 = .;
    if MM16_04  = 9 then MM16_04 = .;
    if MM16_05  = 9 then MM16_05 = .;
    if MM16_06  = 9 then MM16_06 = .;
    if MM16_07  = 9 then MM16_07 = .;
    if MM16_08  = 9 then MM16_08 = .;
    if MM16_09  = 9 then MM16_09 = .;
    if MM16_10  = 9 then MM16_10 = .;
    if MM16_11  = 9 then MM16_11 = .;
    if MM16_12  = 9 then MM16_12 = .;
    if MM16_13  = 9 then MM16_13 = .;
    if MM16_14  = 9 then MM16_14 = .;
    if MM16_15  = 9 then MM16_15 = .;
    if MM16_16  = 9 then MM16_16 = .;
    if MM16_17  = 9 then MM16_17 = .;
    if MM16_18  = 9 then MM16_18 = .;
    if MM16_19  = 9 then MM16_19 = .;
    if MM16_20  = 9 then MM16_20 = .;
    if MMC1     = 99 then MMC1 = .;
    if MMC2     = 99 then MMC2 = .;
    if MMC3     = 99 then MMC3 = .;
    if MMC4_1   = 99 then MMC4_1 = .;
    if MMC4_2   = 99 then MMC4_2 = .;
    if MMC4_3   = 99 then MMC4_3 = .;
    if MMC4_4   = 99 then MMC4_4 = .;
    if MMC4_5   = 99 then MMC4_5 = .;
    if MMC4_6   = 99 then MMC4_6 = .;
    if D101A    = 9 then D101A = .;
    if D101B    = 9 then D101B = .;
    if D101C    = 9 then D101C = .;
    if D101D    = 9 then D101D = .;
    if D101E    = 9 then D101E = .;
    if D101F    = 9 then D101F = .;
    if D101G    = 9 then D101G = .;
    if D101H    = 9 then D101H = .;
    if D101I    = 9 then D101I = .;
    if D101J    = 9 then D101J = .;
    if D102     = 99 then D102 = .;
    if D103A    = 9 then D103A = .;
    if D103B    = 9 then D103B = .;
    if D103C    = 9 then D103C = .;
    if D103D    = 9 then D103D = .;
    if D103E    = 9 then D103E = .;
    if D103F    = 9 then D103F = .;
    if D104     = 9 then D104 = .;
    if D105A    = 9 then D105A = .;
    if D105B    = 9 then D105B = .;
    if D105C    = 9 then D105C = .;
    if D105D    = 9 then D105D = .;
    if D105E    = 9 then D105E = .;
    if D105F    = 9 then D105F = .;
    if D105G    = 9 then D105G = .;
    if D105H    = 9 then D105H = .;
    if D105I    = 9 then D105I = .;
    if D105J    = 9 then D105J = .;
    if D105K    = 9 then D105K = .;
    if D105L    = 9 then D105L = .;
    if D105M    = 9 then D105M = .;
    if D105N    = 9 then D105N = .;
    if D106     = 9 then D106 = .;
    if D107     = 9 then D107 = .;
    if D108     = 9 then D108 = .;
    if D109     = 99 then D109 = .;
    if D110A    = 9 then D110A = .;
    if D110B    = 9 then D110B = .;
    if D110C    = 9 then D110C = .;
    if D110D    = 9 then D110D = .;
    if D110E    = 9 then D110E = .;
    if D110F    = 9 then D110F = .;
    if D110G    = 9 then D110G = .;
    if D110H    = 9 then D110H = .;
    if D111     = 9 then D111 = .;
    if D112     = 9 then D112 = .;
    if D112A    = 9 then D112A = .;
    if D113     = 9 then D113 = .;
    if D114     = 9 then D114 = .;
    if D115B    = 9 then D115B = .;
    if D115C    = 9 then D115C = .;
    if D115D    = 9 then D115D = .;
    if D115E    = 9 then D115E = .;
    if D115F    = 9 then D115F = .;
    if D115G    = 9 then D115G = .;
    if D115H    = 9 then D115H = .;
    if D115I    = 9 then D115I = .;
    if D115J    = 9 then D115J = .;
    if D115K    = 9 then D115K = .;
    if D115L    = 9 then D115L = .;
    if D115M    = 9 then D115M = .;
    if D115N    = 9 then D115N = .;
    if D115O    = 9 then D115O = .;
    if D115P    = 9 then D115P = .;
    if D115Q    = 9 then D115Q = .;
    if D115R    = 9 then D115R = .;
    if D115S    = 9 then D115S = .;
    if D115T    = 9 then D115T = .;
    if D115U    = 9 then D115U = .;
    if D115V    = 9 then D115V = .;
    if D115W    = 9 then D115W = .;
    if D115X    = 9 then D115X = .;
    if D115Y    = 9 then D115Y = .;
    if D115XA   = 9 then D115XA = .;
    if D115XB   = 9 then D115XB = .;
    if D115XC   = 9 then D115XC = .;
    if D115XD   = 9 then D115XD = .;
    if D115XE   = 9 then D115XE = .;
    if D115XF   = 9 then D115XF = .;
    if D115XG   = 9 then D115XG = .;
    if D115XH   = 9 then D115XH = .;
    if D115XI   = 9 then D115XI = .;
    if D115XJ   = 9 then D115XJ = .;
    if D115XK   = 9 then D115XK = .;
    if D116     = 99 then D116 = .;
    if D117A    = 9 then D117A = .;
    if D118A    = 9 then D118A = .;
    if D118B    = 9 then D118B = .;
    if D118C    = 9 then D118C = .;
    if D118D    = 9 then D118D = .;
    if D118E    = 9 then D118E = .;
    if D118F    = 9 then D118F = .;
    if D118G    = 9 then D118G = .;
    if D118H    = 9 then D118H = .;
    if D118I    = 9 then D118I = .;
    if D118J    = 9 then D118J = .;
    if D118K    = 9 then D118K = .;
    if D118L    = 9 then D118L = .;
    if D118M    = 9 then D118M = .;
    if D118N    = 9 then D118N = .;
    if D118O    = 9 then D118O = .;
    if D118P    = 9 then D118P = .;
    if D118Q    = 9 then D118Q = .;
    if D118R    = 9 then D118R = .;
    if D118S    = 9 then D118S = .;
    if D118T    = 9 then D118T = .;
    if D118U    = 9 then D118U = .;
    if D118V    = 9 then D118V = .;
    if D118W    = 9 then D118W = .;
    if D118X    = 9 then D118X = .;
    if D118Y    = 9 then D118Y = .;
    if D118XA   = 9 then D118XA = .;
    if D118XB   = 9 then D118XB = .;
    if D118XC   = 9 then D118XC = .;
    if D118XD   = 9 then D118XD = .;
    if D118XE   = 9 then D118XE = .;
    if D118XF   = 9 then D118XF = .;
    if D118XG   = 9 then D118XG = .;
    if D118XH   = 9 then D118XH = .;
    if D118XI   = 9 then D118XI = .;
    if D118XJ   = 9 then D118XJ = .;
    if D118XK   = 9 then D118XK = .;
    if D119A    = 9 then D119A = .;
    if D119B    = 9 then D119B = .;
    if D119C    = 9 then D119C = .;
    if D119D    = 9 then D119D = .;
    if D119E    = 9 then D119E = .;
    if D119F    = 9 then D119F = .;
    if D119G    = 9 then D119G = .;
    if D119H    = 9 then D119H = .;
    if D119I    = 9 then D119I = .;
    if D119J    = 9 then D119J = .;
    if D119K    = 9 then D119K = .;
    if D119L    = 9 then D119L = .;
    if D119M    = 9 then D119M = .;
    if D119N    = 9 then D119N = .;
    if D119O    = 9 then D119O = .;
    if D119P    = 9 then D119P = .;
    if D119Q    = 9 then D119Q = .;
    if D119R    = 9 then D119R = .;
    if D119S    = 9 then D119S = .;
    if D119T    = 9 then D119T = .;
    if D119U    = 9 then D119U = .;
    if D119V    = 9 then D119V = .;
    if D119W    = 9 then D119W = .;
    if D119X    = 9 then D119X = .;
    if D119Y    = 9 then D119Y = .;
    if D119XA   = 9 then D119XA = .;
    if D119XB   = 9 then D119XB = .;
    if D119XC   = 9 then D119XC = .;
    if D119XD   = 9 then D119XD = .;
    if D119XE   = 9 then D119XE = .;
    if D119XF   = 9 then D119XF = .;
    if D119XG   = 9 then D119XG = .;
    if D119XH   = 9 then D119XH = .;
    if D119XI   = 9 then D119XI = .;
    if D119XJ   = 9 then D119XJ = .;
    if D119XK   = 9 then D119XK = .;
    if D120     = 99 then D120 = .;
    if D121     = 9 then D121 = .;
    if D122A    = 9 then D122A = .;
    if D122B    = 9 then D122B = .;
    if D122C    = 9 then D122C = .;
    if D123     = 9 then D123 = .;
    if D124     = 9 then D124 = .;
    if D125     = 9 then D125 = .;
    if D126     = 99 then D126 = .;
    if D127     = 99 then D127 = .;
    if D128     = 9 then D128 = .;
    if D129     = 9 then D129 = .;
    if D130A    = 9 then D130A = .;
    if D130B    = 9 then D130B = .;
    if D130C    = 9 then D130C = .;
    if S109     = 9 then S109 = .;
    if S107     = 9 then S107 = .;
    if S111     = 99 then S111 = .;
    if S112     = 9 then S112 = .;
    if S113     = 9 then S113 = .;
    if S115     = 9 then S115 = .;
    if S116     = 9 then S116 = .;
    if S120     = 9 then S120 = .;
    if S121     = 9 then S121 = .;
    if S124     = 9 then S124 = .;
    if S206A    = 99 then S206A = .;
    if S206B    = 99 then S206B = .;
    if S206C    = 99 then S206C = .;
    if S206D    = 99 then S206D = .;
    if S207A    = 99 then S207A = .;
    if S207B    = 99 then S207B = .;
    if S207C    = 99 then S207C = .;
    if S207D    = 99 then S207D = .;
    if S207E    = 99 then S207E = .;
    if S207F    = 99 then S207F = .;
    if S207G    = 99 then S207G = .;
    if S207H    = 99 then S207H = .;
    if S207I    = 99 then S207I = .;
    if S207J    = 99 then S207J = .;
    if S207M    = 99 then S207M = .;
    if S207N    = 99 then S207N = .;
    if S213     = 9 then S213 = .;
    if S233     = 9 then S233 = .;
    if S234     = 9 then S234 = .;
    if S235     = 99 then S235 = .;
    if S236     = 99 then S236 = .;
    if S238A    = 9 then S238A = .;
    if S238B    = 9 then S238B = .;
    if S238C    = 9 then S238C = .;
    if S238D    = 9 then S238D = .;
    if S238E    = 9 then S238E = .;
    if S238F    = 9 then S238F = .;
    if S238G    = 9 then S238G = .;
    if S238H    = 9 then S238H = .;
    if S238X    = 9 then S238X = .;
    if S240A    = 9 then S240A = .;
    if S240B    = 9 then S240B = .;
    if S240C    = 9 then S240C = .;
    if S240D    = 9 then S240D = .;
    if S240E    = 9 then S240E = .;
    if S240F    = 9 then S240F = .;
    if S240G    = 9 then S240G = .;
    if S240X    = 9 then S240X = .;
    if S251     = 99 then S251 = .;
    if S313     = 9 then S313 = .;
    if S316     = 9 then S316 = .;
    if S317     = 9 then S317 = .;
    if S332     = 9 then S332 = .;
    if S333A    = 9 then S333A = .;
    if S333B    = 9 then S333B = .;
    if S333C    = 9 then S333C = .;
    if S333D    = 9 then S333D = .;
    if S333E    = 9 then S333E = .;
    if S333F    = 9 then S333F = .;
    if S333X    = 9 then S333X = .;
    if S334     = 9 then S334 = .;
    if S616A    = 9 then S616A = .;
    if S617     = 9 then S617 = .;
    if S617A    = 9 then S617A = .;
    if S618     = 9 then S618 = .;
    if S622A    = 9 then S622A = .;
    if S622B    = 9 then S622B = .;
    if S631     = 9 then S631 = .;
    if S632     = 9 then S632 = .;
    if S638A    = 99 then S638A = .;
    if S638B    = 99 then S638B = .;
    if S638C    = 99 then S638C = .;
    if S653A    = 9 then S653A = .;
    if S653B    = 9 then S653B = .;
    if S653C    = 9 then S653C = .;
    if S653D    = 9 then S653D = .;
    if S653E    = 9 then S653E = .;
    if S653F    = 9 then S653F = .;
    if S653G    = 9 then S653G = .;
    if S654A    = 9 then S654A = .;
    if S654B    = 9 then S654B = .;
    if S654C    = 9 then S654C = .;
    if S654D    = 9 then S654D = .;
    if S654E    = 9 then S654E = .;
    if S654F    = 9 then S654F = .;
    if S654G    = 9 then S654G = .;
    if S654H    = 9 then S654H = .;
    if S654I    = 9 then S654I = .;
    if S654J    = 9 then S654J = .;
    if S654K    = 9 then S654K = .;
    if S654L    = 9 then S654L = .;
    if S655     = 9 then S655 = .;
    if S656     = 9 then S656 = .;
    if S708     = 9 then S708 = .;
    if S709     = 9 then S709 = .;
    if S710     = 9 then S710 = .;
    if S711     = 9 then S711 = .;
    if S712     = 9 then S712 = .;
    if S713     = 99 then S713 = .;
    if S722     = 9 then S722 = .;
    if S723A    = 9 then S723A = .;
    if S805     = 9 then S805 = .;
    if S814F    = 9 then S814F = .;
    if S814G    = 9 then S814G = .;
    if S815B    = 9 then S815B = .;
    if S815C    = 9 then S815C = .;
    if S815D    = 9 then S815D = .;
    if S815E    = 9 then S815E = .;
    if S1011A   = 9 then S1011A = .;
    if S1011B   = 9 then S1011B = .;
    if S1011C   = 9 then S1011C = .;
    if S1011D   = 9 then S1011D = .;
    if S1021A   = 9 then S1021A = .;
    if S1022A   = 9 then S1022A = .;
    if S1026A   = 9 then S1026A = .;
    if S1027A   = 9 then S1027A = .;
    if S1038    = 9 then S1038 = .;
    if S1040A   = 9 then S1040A = .;
    if S1040B   = 9 then S1040B = .;
    if S1040C   = 9 then S1040C = .;
    if S1040D   = 9 then S1040D = .;
    if S1040E   = 9 then S1040E = .;
    if S1040F   = 9 then S1040F = .;
    if S1040G   = 9 then S1040G = .;
    if S1040H   = 9 then S1040H = .;
    if S1040I   = 9 then S1040I = .;
    if S1040J   = 9 then S1040J = .;
    if S1040K   = 9 then S1040K = .;
    if S1040L   = 9 then S1040L = .;
    if S1040X   = 9 then S1040X = .;
    if S1040Z   = 9 then S1040Z = .;
    if S1041A   = 9 then S1041A = .;
    if S1041B   = 9 then S1041B = .;
    if S1041C   = 9 then S1041C = .;
    if S1041D   = 9 then S1041D = .;
    if S1041E   = 9 then S1041E = .;
    if S1041F   = 9 then S1041F = .;
    if S1041G   = 9 then S1041G = .;
    if S1041H   = 9 then S1041H = .;
    if S1041I   = 9 then S1041I = .;
    if S1041J   = 9 then S1041J = .;
    if S1041K   = 9 then S1041K = .;
    if S1041L   = 9 then S1041L = .;
    if S1041X   = 9 then S1041X = .;
    if S1041Z   = 9 then S1041Z = .;
    if S1050    = 9 then S1050 = .;
    if S1051    = 9 then S1051 = .;
    if S1052A   = 9 then S1052A = .;
    if S1052B   = 9 then S1052B = .;
    if S1052C   = 9 then S1052C = .;
    if S1055    = 9 then S1055 = .;
    if S1205B   = 9 then S1205B = .;
    if S1205C   = 9 then S1205C = .;
    if S1205D   = 9 then S1205D = .;
    if S1205E   = 9 then S1205E = .;
    if S1207    = 9 then S1207 = .;
    if S1208    = 9 then S1208 = .;
    if S1209    = 9 then S1209 = .;
    if S1210    = 9 then S1210 = .;
    if S1211    = 9 then S1211 = .;
    if S1212    = 9 then S1212 = .;
    if S1215G   = 9 then S1215G = .;
    if S1215H   = 9 then S1215H = .;
    if S1215I   = 9 then S1215I = .;
    if S1215J   = 9 then S1215J = .;
    if S1215K   = 9 then S1215K = .;
    if S1215L   = 9 then S1215L = .;
    if S1215M   = 9 then S1215M = .;
    if S1215N   = 9 then S1215N = .;
    if S1217    = 9 then S1217 = .;
    if S1218    = 99 then S1218 = .;
    if S1220    = 9 then S1220 = .;
    if S1221A   = 9 then S1221A = .;
    if S1221B   = 9 then S1221B = .;
    if S1221C   = 9 then S1221C = .;
    if S1221D   = 9 then S1221D = .;
    if S1221X   = 9 then S1221X = .;
    if S1221Z   = 9 then S1221Z = .;
    if S1222    = 9 then S1222 = .;
    if S1224A   = 9 then S1224A = .;
    if S1224B   = 9 then S1224B = .;
    if S1224C   = 9 then S1224C = .;
    if S1224D   = 9 then S1224D = .;
    if S1224E   = 9 then S1224E = .;
    if S1224F   = 9 then S1224F = .;
    if S1224G   = 9 then S1224G = .;
    if S1224H   = 9 then S1224H = .;
    if S1224I   = 9 then S1224I = .;
    if S1224J   = 9 then S1224J = .;
    if S1224K   = 9 then S1224K = .;
    if S1224L   = 9 then S1224L = .;
    if S1224M   = 9 then S1224M = .;
    if S1224N   = 9 then S1224N = .;
    if S1224X   = 9 then S1224X = .;
    if S1224Z   = 9 then S1224Z = .;
    if S1225    = 9 then S1225 = .;
    if S1226    = 9 then S1226 = .;
    if S1226A   = 9 then S1226A = .;
    if S1228A   = 9 then S1228A = .;
    if S1228B   = 9 then S1228B = .;
    if S1228X   = 9 then S1228X = .;
    if S1228Y   = 9 then S1228Y = .;
    if S1230AA  = 9 then S1230AA = .;
    if S1230AB  = 9 then S1230AB = .;
    if S1230AC  = 9 then S1230AC = .;
    if S1230AD  = 9 then S1230AD = .;
    if S1230AE  = 9 then S1230AE = .;
    if S1230AF  = 9 then S1230AF = .;
    if S1230AG  = 9 then S1230AG = .;
    if S1230AH  = 9 then S1230AH = .;
    if S1230AI  = 9 then S1230AI = .;
    if S1230AJ  = 9 then S1230AJ = .;
    if S1230AK  = 9 then S1230AK = .;
    if S1230AL  = 9 then S1230AL = .;
    if S1230AM  = 9 then S1230AM = .;
    if S1230AN  = 9 then S1230AN = .;
    if S1230AX  = 9 then S1230AX = .;
    if S1230BA  = 9 then S1230BA = .;
    if S1230BB  = 9 then S1230BB = .;
    if S1230BC  = 9 then S1230BC = .;
    if S1230BD  = 9 then S1230BD = .;
    if S1230BE  = 9 then S1230BE = .;
    if S1230BF  = 9 then S1230BF = .;
    if S1230BG  = 9 then S1230BG = .;
    if S1230BH  = 9 then S1230BH = .;
    if S1230BI  = 9 then S1230BI = .;
    if S1230BJ  = 9 then S1230BJ = .;
    if S1230BK  = 9 then S1230BK = .;
    if S1230BL  = 9 then S1230BL = .;
    if S1230BM  = 9 then S1230BM = .;
    if S1230BN  = 9 then S1230BN = .;
    if S1230BX  = 9 then S1230BX = .;
    if S1230CA  = 9 then S1230CA = .;
    if S1230CB  = 9 then S1230CB = .;
    if S1230CC  = 9 then S1230CC = .;
    if S1230CD  = 9 then S1230CD = .;
    if S1230CE  = 9 then S1230CE = .;
    if S1230CF  = 9 then S1230CF = .;
    if S1230CG  = 9 then S1230CG = .;
    if S1230CH  = 9 then S1230CH = .;
    if S1230CI  = 9 then S1230CI = .;
    if S1230CJ  = 9 then S1230CJ = .;
    if S1230CK  = 9 then S1230CK = .;
    if S1230CL  = 9 then S1230CL = .;
    if S1230CM  = 9 then S1230CM = .;
    if S1230CN  = 9 then S1230CN = .;
    if S1230CX  = 9 then S1230CX = .;
    if S1234    = 9 then S1234 = .;
    if S1235    = 9 then S1235 = .;
    if S1236A   = 9 then S1236A = .;
    if S1236B   = 9 then S1236B = .;
    if S1236C   = 9 then S1236C = .;
    if S1236D   = 9 then S1236D = .;
    if S1236E   = 9 then S1236E = .;
    if S1236F   = 9 then S1236F = .;
    if S1236G   = 9 then S1236G = .;
    if S1236X   = 9 then S1236X = .;
    if S1237    = 9 then S1237 = .;
    if S1238A   = 9 then S1238A = .;
    if S1238B   = 9 then S1238B = .;
    if S1238C   = 9 then S1238C = .;
    if S1238D   = 9 then S1238D = .;
    if S1238E   = 9 then S1238E = .;
    if S1238F   = 9 then S1238F = .;
    if S1238G   = 9 then S1238G = .;
    if S1238X   = 9 then S1238X = .;
    if S1239    = 9 then S1239 = .;
    if S1240A   = 9 then S1240A = .;
    if S1240B   = 9 then S1240B = .;
    if S1240C   = 9 then S1240C = .;
    if S1240D   = 9 then S1240D = .;
    if S1240E   = 9 then S1240E = .;
    if S1240F   = 9 then S1240F = .;
    if S1240G   = 9 then S1240G = .;
    if S1240H   = 9 then S1240H = .;
    if S1240X   = 9 then S1240X = .;
    if S1243    = 9 then S1243 = .;
    if S1245    = 9 then S1245 = .;
    if S1246A   = 9 then S1246A = .;
    if S1246B   = 9 then S1246B = .;
    if S1246C   = 9 then S1246C = .;
    if S1246D   = 9 then S1246D = .;
    if S1246E   = 9 then S1246E = .;
    if S1246F   = 9 then S1246F = .;
    if S1246G   = 9 then S1246G = .;
    if S1246H   = 9 then S1246H = .;
    if S1246I   = 9 then S1246I = .;
    if S1246J   = 9 then S1246J = .;
    if S1246K   = 9 then S1246K = .;
    if S1246L   = 9 then S1246L = .;
    if S1246M   = 9 then S1246M = .;
    if S1246X   = 9 then S1246X = .;
    if S1247    = 9 then S1247 = .;
    if S1248    = 9 then S1248 = .;
    if S1249    = 9 then S1249 = .;
    if S1250    = 9 then S1250 = .;
    if S1251    = 9 then S1251 = .;
    if S1252    = 9 then S1252 = .;
    if S1254    = 9 then S1254 = .;
    if S1255A   = 9 then S1255A = .;
    if S1255B   = 9 then S1255B = .;
    if S1255C   = 9 then S1255C = .;
    if S1255D   = 9 then S1255D = .;
    if S1255E   = 9 then S1255E = .;
    if S1255F   = 9 then S1255F = .;
    if S1255G   = 9 then S1255G = .;
    if S1255H   = 9 then S1255H = .;
    if S1255I   = 9 then S1255I = .;
    if S1255X   = 9 then S1255X = .;
    if S1257    = 9 then S1257 = .;
    if S1258A   = 9 then S1258A = .;
    if S1258B   = 9 then S1258B = .;
    if S1258C   = 9 then S1258C = .;
    if S1258D   = 9 then S1258D = .;
    if S1258E   = 9 then S1258E = .;
    if S1258F   = 9 then S1258F = .;
    if S1258G   = 9 then S1258G = .;
    if S1258H   = 9 then S1258H = .;
    if S1258X   = 9 then S1258X = .;
    if S1259    = 9 then S1259 = .;
    if S1260    = 9 then S1260 = .;
    if S1261    = 9 then S1261 = .;
    if S1262    = 9 then S1262 = .;
    if S1263    = 9 then S1263 = .;
    if S1267A   = 9 then S1267A = .;
    if S1267B   = 9 then S1267B = .;
    if S1267C   = 9 then S1267C = .;
    if S1267D   = 9 then S1267D = .;
    if S1267E   = 9 then S1267E = .;
    if S1267F   = 9 then S1267F = .;
    if S1267G   = 9 then S1267G = .;
    if S1267H   = 9 then S1267H = .;
    if S1267I   = 9 then S1267I = .;
    if S1267J   = 9 then S1267J = .;
    if S1267X   = 9 then S1267X = .;
    if S1267Z   = 9 then S1267Z = .;
    if S1268    = 9 then S1268 = .;
    if S1269A   = 9 then S1269A = .;
    if S1269B   = 9 then S1269B = .;
    if S1269C   = 9 then S1269C = .;
    if S1269D   = 9 then S1269D = .;
    if S1269X   = 9 then S1269X = .;
    if S1270A   = 9 then S1270A = .;
    if S1270B   = 9 then S1270B = .;
    if S1270C   = 9 then S1270C = .;
    if S1270D   = 9 then S1270D = .;
    if S1270E   = 9 then S1270E = .;
    if S1270F   = 9 then S1270F = .;
    if S1270G   = 9 then S1270G = .;
    if S1270X   = 9 then S1270X = .;
    if S1271A   = 9 then S1271A = .;
    if S1271B   = 9 then S1271B = .;
    if S1271C   = 9 then S1271C = .;
    if S1271D   = 9 then S1271D = .;
    if S1271E   = 9 then S1271E = .;
    if S1271F   = 9 then S1271F = .;
    if S1271G   = 9 then S1271G = .;
    if S1271H   = 9 then S1271H = .;
    if S1271I   = 9 then S1271I = .;
    if S1271J   = 9 then S1271J = .;
    if S1271K   = 9 then S1271K = .;
    if S1271L   = 9 then S1271L = .;
    if S1271M   = 9 then S1271M = .;
    if S1271X   = 9 then S1271X = .;
    if S1272    = 9 then S1272 = .;
    if S1273    = 9 then S1273 = .;
    if S1274A   = 9 then S1274A = .;
    if S1274B   = 9 then S1274B = .;
    if S1274C   = 9 then S1274C = .;
    if S1274D   = 9 then S1274D = .;
    if S1274E   = 9 then S1274E = .;
    if S1274F   = 9 then S1274F = .;
    if S1274G   = 9 then S1274G = .;
    if S1274H   = 9 then S1274H = .;
    if S1274I   = 9 then S1274I = .;
    if S1276    = 9 then S1276 = .;
    if S1278A   = 9 then S1278A = .;
    if S1278B   = 9 then S1278B = .;
    if S1278C   = 9 then S1278C = .;
    if S1278D   = 9 then S1278D = .;
    if S1278E   = 9 then S1278E = .;
    if S1279A   = 9 then S1279A = .;
    if S1279B   = 9 then S1279B = .;
    if S1279C   = 9 then S1279C = .;
    if S1279D   = 9 then S1279D = .;
    if S1279E   = 9 then S1279E = .;
    if S413B_1  = 9 then S413B_1 = .;
    if S413B_2  = 9 then S413B_2 = .;
    if S413B_3  = 9 then S413B_3 = .;
    if S413B_4  = 9 then S413B_4 = .;
    if S413B_5  = 9 then S413B_5 = .;
    if S413B_6  = 9 then S413B_6 = .;
    if S413F_1  = 9 then S413F_1 = .;
    if S413F_2  = 9 then S413F_2 = .;
    if S413F_3  = 9 then S413F_3 = .;
    if S413F_4  = 9 then S413F_4 = .;
    if S413F_5  = 9 then S413F_5 = .;
    if S413F_6  = 9 then S413F_6 = .;
    if S414A_1  = 9 then S414A_1 = .;
    if S414A_2  = 9 then S414A_2 = .;
    if S414A_3  = 9 then S414A_3 = .;
    if S414A_4  = 9 then S414A_4 = .;
    if S414A_5  = 9 then S414A_5 = .;
    if S414A_6  = 9 then S414A_6 = .;
    if S414B_1  = 9 then S414B_1 = .;
    if S414B_2  = 9 then S414B_2 = .;
    if S414B_3  = 9 then S414B_3 = .;
    if S414B_4  = 9 then S414B_4 = .;
    if S414B_5  = 9 then S414B_5 = .;
    if S414B_6  = 9 then S414B_6 = .;
    if S416A_1  = 9 then S416A_1 = .;
    if S416A_2  = 9 then S416A_2 = .;
    if S416A_3  = 9 then S416A_3 = .;
    if S416A_4  = 9 then S416A_4 = .;
    if S416A_5  = 9 then S416A_5 = .;
    if S416A_6  = 9 then S416A_6 = .;
    if S416B_1  = 9 then S416B_1 = .;
    if S416B_2  = 9 then S416B_2 = .;
    if S416B_3  = 9 then S416B_3 = .;
    if S416B_4  = 9 then S416B_4 = .;
    if S416B_5  = 9 then S416B_5 = .;
    if S416B_6  = 9 then S416B_6 = .;
    if S416C_1  = 9 then S416C_1 = .;
    if S416C_2  = 9 then S416C_2 = .;
    if S416C_3  = 9 then S416C_3 = .;
    if S416C_4  = 9 then S416C_4 = .;
    if S416C_5  = 9 then S416C_5 = .;
    if S416C_6  = 9 then S416C_6 = .;
    if S416D_1  = 9 then S416D_1 = .;
    if S416D_2  = 9 then S416D_2 = .;
    if S416D_3  = 9 then S416D_3 = .;
    if S416D_4  = 9 then S416D_4 = .;
    if S416D_5  = 9 then S416D_5 = .;
    if S416D_6  = 9 then S416D_6 = .;
    if S416E_1  = 9 then S416E_1 = .;
    if S416E_2  = 9 then S416E_2 = .;
    if S416E_3  = 9 then S416E_3 = .;
    if S416E_4  = 9 then S416E_4 = .;
    if S416E_5  = 9 then S416E_5 = .;
    if S416E_6  = 9 then S416E_6 = .;
    if S416F_1  = 9 then S416F_1 = .;
    if S416F_2  = 9 then S416F_2 = .;
    if S416F_3  = 9 then S416F_3 = .;
    if S416F_4  = 9 then S416F_4 = .;
    if S416F_5  = 9 then S416F_5 = .;
    if S416F_6  = 9 then S416F_6 = .;
    if S416G_1  = 9 then S416G_1 = .;
    if S416G_2  = 9 then S416G_2 = .;
    if S416G_3  = 9 then S416G_3 = .;
    if S416G_4  = 9 then S416G_4 = .;
    if S416G_5  = 9 then S416G_5 = .;
    if S416G_6  = 9 then S416G_6 = .;
    if S416H_1  = 9 then S416H_1 = .;
    if S416H_2  = 9 then S416H_2 = .;
    if S416H_3  = 9 then S416H_3 = .;
    if S416H_4  = 9 then S416H_4 = .;
    if S416H_5  = 9 then S416H_5 = .;
    if S416H_6  = 9 then S416H_6 = .;
    if S416X_1  = 9 then S416X_1 = .;
    if S416X_2  = 9 then S416X_2 = .;
    if S416X_3  = 9 then S416X_3 = .;
    if S416X_4  = 9 then S416X_4 = .;
    if S416X_5  = 9 then S416X_5 = .;
    if S416X_6  = 9 then S416X_6 = .;
    if S420_1   = 9 then S420_1 = .;
    if S420_2   = 9 then S420_2 = .;
    if S420_3   = 9 then S420_3 = .;
    if S420_4   = 9 then S420_4 = .;
    if S420_5   = 9 then S420_5 = .;
    if S420_6   = 9 then S420_6 = .;
    if S422_1   = 9 then S422_1 = .;
    if S422_2   = 9 then S422_2 = .;
    if S422_3   = 9 then S422_3 = .;
    if S422_4   = 9 then S422_4 = .;
    if S422_5   = 9 then S422_5 = .;
    if S422_6   = 9 then S422_6 = .;
    if S423_1   = 9 then S423_1 = .;
    if S423_2   = 9 then S423_2 = .;
    if S423_3   = 9 then S423_3 = .;
    if S423_4   = 9 then S423_4 = .;
    if S423_5   = 9 then S423_5 = .;
    if S423_6   = 9 then S423_6 = .;
    if S425_1   = 9 then S425_1 = .;
    if S425_2   = 9 then S425_2 = .;
    if S425_3   = 9 then S425_3 = .;
    if S425_4   = 9 then S425_4 = .;
    if S425_5   = 9 then S425_5 = .;
    if S425_6   = 9 then S425_6 = .;
    if S426_1   = 9 then S426_1 = .;
    if S426_2   = 9 then S426_2 = .;
    if S426_3   = 9 then S426_3 = .;
    if S426_4   = 9 then S426_4 = .;
    if S426_5   = 9 then S426_5 = .;
    if S426_6   = 9 then S426_6 = .;
    if S428_1   = 9 then S428_1 = .;
    if S428_2   = 9 then S428_2 = .;
    if S428_3   = 9 then S428_3 = .;
    if S428_4   = 9 then S428_4 = .;
    if S428_5   = 9 then S428_5 = .;
    if S428_6   = 9 then S428_6 = .;
    if S430_1   = 9 then S430_1 = .;
    if S430_2   = 9 then S430_2 = .;
    if S430_3   = 9 then S430_3 = .;
    if S430_4   = 9 then S430_4 = .;
    if S430_5   = 9 then S430_5 = .;
    if S430_6   = 9 then S430_6 = .;
    if S433A_1  = 9 then S433A_1 = .;
    if S433A_2  = 9 then S433A_2 = .;
    if S433A_3  = 9 then S433A_3 = .;
    if S433A_4  = 9 then S433A_4 = .;
    if S433A_5  = 9 then S433A_5 = .;
    if S433A_6  = 9 then S433A_6 = .;
    if S433B_1  = 9 then S433B_1 = .;
    if S433B_2  = 9 then S433B_2 = .;
    if S433B_3  = 9 then S433B_3 = .;
    if S433B_4  = 9 then S433B_4 = .;
    if S433B_5  = 9 then S433B_5 = .;
    if S433B_6  = 9 then S433B_6 = .;
    if S433C_1  = 9 then S433C_1 = .;
    if S433C_2  = 9 then S433C_2 = .;
    if S433C_3  = 9 then S433C_3 = .;
    if S433C_4  = 9 then S433C_4 = .;
    if S433C_5  = 9 then S433C_5 = .;
    if S433C_6  = 9 then S433C_6 = .;
    if S433X_1  = 9 then S433X_1 = .;
    if S433X_2  = 9 then S433X_2 = .;
    if S433X_3  = 9 then S433X_3 = .;
    if S433X_4  = 9 then S433X_4 = .;
    if S433X_5  = 9 then S433X_5 = .;
    if S433X_6  = 9 then S433X_6 = .;
    if S434_1   = 9 then S434_1 = .;
    if S434_2   = 9 then S434_2 = .;
    if S434_3   = 9 then S434_3 = .;
    if S434_4   = 9 then S434_4 = .;
    if S434_5   = 9 then S434_5 = .;
    if S434_6   = 9 then S434_6 = .;
    if S437_1   = 999 then S437_1 = .;
    if S437_2   = 999 then S437_2 = .;
    if S437_3   = 999 then S437_3 = .;
    if S437_4   = 999 then S437_4 = .;
    if S437_5   = 999 then S437_5 = .;
    if S437_6   = 999 then S437_6 = .;
    if S438A_1  = 9 then S438A_1 = .;
    if S438A_2  = 9 then S438A_2 = .;
    if S438A_3  = 9 then S438A_3 = .;
    if S438A_4  = 9 then S438A_4 = .;
    if S438A_5  = 9 then S438A_5 = .;
    if S438A_6  = 9 then S438A_6 = .;
    if S438B_1  = 9 then S438B_1 = .;
    if S438B_2  = 9 then S438B_2 = .;
    if S438B_3  = 9 then S438B_3 = .;
    if S438B_4  = 9 then S438B_4 = .;
    if S438B_5  = 9 then S438B_5 = .;
    if S438B_6  = 9 then S438B_6 = .;
    if S438C_1  = 9 then S438C_1 = .;
    if S438C_2  = 9 then S438C_2 = .;
    if S438C_3  = 9 then S438C_3 = .;
    if S438C_4  = 9 then S438C_4 = .;
    if S438C_5  = 9 then S438C_5 = .;
    if S438C_6  = 9 then S438C_6 = .;
    if S438D_1  = 9 then S438D_1 = .;
    if S438D_2  = 9 then S438D_2 = .;
    if S438D_3  = 9 then S438D_3 = .;
    if S438D_4  = 9 then S438D_4 = .;
    if S438D_5  = 9 then S438D_5 = .;
    if S438D_6  = 9 then S438D_6 = .;
    if S438E_1  = 9 then S438E_1 = .;
    if S438E_2  = 9 then S438E_2 = .;
    if S438E_3  = 9 then S438E_3 = .;
    if S438E_4  = 9 then S438E_4 = .;
    if S438E_5  = 9 then S438E_5 = .;
    if S438E_6  = 9 then S438E_6 = .;
    if S438F_1  = 9 then S438F_1 = .;
    if S438F_2  = 9 then S438F_2 = .;
    if S438F_3  = 9 then S438F_3 = .;
    if S438F_4  = 9 then S438F_4 = .;
    if S438F_5  = 9 then S438F_5 = .;
    if S438F_6  = 9 then S438F_6 = .;
    if S438G_1  = 9 then S438G_1 = .;
    if S438G_2  = 9 then S438G_2 = .;
    if S438G_3  = 9 then S438G_3 = .;
    if S438G_4  = 9 then S438G_4 = .;
    if S438G_5  = 9 then S438G_5 = .;
    if S438G_6  = 9 then S438G_6 = .;
    if S438H_1  = 9 then S438H_1 = .;
    if S438H_2  = 9 then S438H_2 = .;
    if S438H_3  = 9 then S438H_3 = .;
    if S438H_4  = 9 then S438H_4 = .;
    if S438H_5  = 9 then S438H_5 = .;
    if S438H_6  = 9 then S438H_6 = .;
    if S438I_1  = 9 then S438I_1 = .;
    if S438I_2  = 9 then S438I_2 = .;
    if S438I_3  = 9 then S438I_3 = .;
    if S438I_4  = 9 then S438I_4 = .;
    if S438I_5  = 9 then S438I_5 = .;
    if S438I_6  = 9 then S438I_6 = .;
    if S438J_1  = 9 then S438J_1 = .;
    if S438J_2  = 9 then S438J_2 = .;
    if S438J_3  = 9 then S438J_3 = .;
    if S438J_4  = 9 then S438J_4 = .;
    if S438J_5  = 9 then S438J_5 = .;
    if S438J_6  = 9 then S438J_6 = .;
    if S438X_1  = 9 then S438X_1 = .;
    if S438X_2  = 9 then S438X_2 = .;
    if S438X_3  = 9 then S438X_3 = .;
    if S438X_4  = 9 then S438X_4 = .;
    if S438X_5  = 9 then S438X_5 = .;
    if S438X_6  = 9 then S438X_6 = .;
    if S444A_1  = 9 then S444A_1 = .;
    if S444A_2  = 9 then S444A_2 = .;
    if S444A_3  = 9 then S444A_3 = .;
    if S444A_4  = 9 then S444A_4 = .;
    if S444A_5  = 9 then S444A_5 = .;
    if S444A_6  = 9 then S444A_6 = .;
    if S444B_1  = 9 then S444B_1 = .;
    if S444B_2  = 9 then S444B_2 = .;
    if S444B_3  = 9 then S444B_3 = .;
    if S444B_4  = 9 then S444B_4 = .;
    if S444B_5  = 9 then S444B_5 = .;
    if S444B_6  = 9 then S444B_6 = .;
    if S444C_1  = 9 then S444C_1 = .;
    if S444C_2  = 9 then S444C_2 = .;
    if S444C_3  = 9 then S444C_3 = .;
    if S444C_4  = 9 then S444C_4 = .;
    if S444C_5  = 9 then S444C_5 = .;
    if S444C_6  = 9 then S444C_6 = .;
    if S444D_1  = 9 then S444D_1 = .;
    if S444D_2  = 9 then S444D_2 = .;
    if S444D_3  = 9 then S444D_3 = .;
    if S444D_4  = 9 then S444D_4 = .;
    if S444D_5  = 9 then S444D_5 = .;
    if S444D_6  = 9 then S444D_6 = .;
    if S444E_1  = 9 then S444E_1 = .;
    if S444E_2  = 9 then S444E_2 = .;
    if S444E_3  = 9 then S444E_3 = .;
    if S444E_4  = 9 then S444E_4 = .;
    if S444E_5  = 9 then S444E_5 = .;
    if S444E_6  = 9 then S444E_6 = .;
    if S445_1   = 9 then S445_1 = .;
    if S445_2   = 9 then S445_2 = .;
    if S445_3   = 9 then S445_3 = .;
    if S445_4   = 9 then S445_4 = .;
    if S445_5   = 9 then S445_5 = .;
    if S445_6   = 9 then S445_6 = .;
    if S446_1   = 99 then S446_1 = .;
    if S446_2   = 99 then S446_2 = .;
    if S446_3   = 99 then S446_3 = .;
    if S446_4   = 99 then S446_4 = .;
    if S446_5   = 99 then S446_5 = .;
    if S446_6   = 99 then S446_6 = .;
    if S449_1   = 9 then S449_1 = .;
    if S449_2   = 9 then S449_2 = .;
    if S449_3   = 9 then S449_3 = .;
    if S449_4   = 9 then S449_4 = .;
    if S449_5   = 9 then S449_5 = .;
    if S449_6   = 9 then S449_6 = .;
    if S450_1   = 99 then S450_1 = .;
    if S450_2   = 99 then S450_2 = .;
    if S450_3   = 99 then S450_3 = .;
    if S450_4   = 99 then S450_4 = .;
    if S450_5   = 99 then S450_5 = .;
    if S450_6   = 99 then S450_6 = .;
    if S452A_1  = 9 then S452A_1 = .;
    if S452A_2  = 9 then S452A_2 = .;
    if S452A_3  = 9 then S452A_3 = .;
    if S452A_4  = 9 then S452A_4 = .;
    if S452A_5  = 9 then S452A_5 = .;
    if S452A_6  = 9 then S452A_6 = .;
    if S452B_1  = 9 then S452B_1 = .;
    if S452B_2  = 9 then S452B_2 = .;
    if S452B_3  = 9 then S452B_3 = .;
    if S452B_4  = 9 then S452B_4 = .;
    if S452B_5  = 9 then S452B_5 = .;
    if S452B_6  = 9 then S452B_6 = .;
    if S452C_1  = 9 then S452C_1 = .;
    if S452C_2  = 9 then S452C_2 = .;
    if S452C_3  = 9 then S452C_3 = .;
    if S452C_4  = 9 then S452C_4 = .;
    if S452C_5  = 9 then S452C_5 = .;
    if S452C_6  = 9 then S452C_6 = .;
    if S452D_1  = 9 then S452D_1 = .;
    if S452D_2  = 9 then S452D_2 = .;
    if S452D_3  = 9 then S452D_3 = .;
    if S452D_4  = 9 then S452D_4 = .;
    if S452D_5  = 9 then S452D_5 = .;
    if S452D_6  = 9 then S452D_6 = .;
    if S452E_1  = 9 then S452E_1 = .;
    if S452E_2  = 9 then S452E_2 = .;
    if S452E_3  = 9 then S452E_3 = .;
    if S452E_4  = 9 then S452E_4 = .;
    if S452E_5  = 9 then S452E_5 = .;
    if S452E_6  = 9 then S452E_6 = .;
    if S452F_1  = 9 then S452F_1 = .;
    if S452F_2  = 9 then S452F_2 = .;
    if S452F_3  = 9 then S452F_3 = .;
    if S452F_4  = 9 then S452F_4 = .;
    if S452F_5  = 9 then S452F_5 = .;
    if S452F_6  = 9 then S452F_6 = .;
    if S452G_1  = 9 then S452G_1 = .;
    if S452G_2  = 9 then S452G_2 = .;
    if S452G_3  = 9 then S452G_3 = .;
    if S452G_4  = 9 then S452G_4 = .;
    if S452G_5  = 9 then S452G_5 = .;
    if S452G_6  = 9 then S452G_6 = .;
    if S452H_1  = 9 then S452H_1 = .;
    if S452H_2  = 9 then S452H_2 = .;
    if S452H_3  = 9 then S452H_3 = .;
    if S452H_4  = 9 then S452H_4 = .;
    if S452H_5  = 9 then S452H_5 = .;
    if S452H_6  = 9 then S452H_6 = .;
    if S454_1   = 9 then S454_1 = .;
    if S454_2   = 9 then S454_2 = .;
    if S454_3   = 9 then S454_3 = .;
    if S454_4   = 9 then S454_4 = .;
    if S454_5   = 9 then S454_5 = .;
    if S454_6   = 9 then S454_6 = .;
    if S457A_1  = 9 then S457A_1 = .;
    if S457A_2  = 9 then S457A_2 = .;
    if S457A_3  = 9 then S457A_3 = .;
    if S457A_4  = 9 then S457A_4 = .;
    if S457A_5  = 9 then S457A_5 = .;
    if S457A_6  = 9 then S457A_6 = .;
    if S457B_1  = 9 then S457B_1 = .;
    if S457B_2  = 9 then S457B_2 = .;
    if S457B_3  = 9 then S457B_3 = .;
    if S457B_4  = 9 then S457B_4 = .;
    if S457B_5  = 9 then S457B_5 = .;
    if S457B_6  = 9 then S457B_6 = .;
    if S457C_1  = 9 then S457C_1 = .;
    if S457C_2  = 9 then S457C_2 = .;
    if S457C_3  = 9 then S457C_3 = .;
    if S457C_4  = 9 then S457C_4 = .;
    if S457C_5  = 9 then S457C_5 = .;
    if S457C_6  = 9 then S457C_6 = .;
    if S457D_1  = 9 then S457D_1 = .;
    if S457D_2  = 9 then S457D_2 = .;
    if S457D_3  = 9 then S457D_3 = .;
    if S457D_4  = 9 then S457D_4 = .;
    if S457D_5  = 9 then S457D_5 = .;
    if S457D_6  = 9 then S457D_6 = .;
    if S457X_1  = 9 then S457X_1 = .;
    if S457X_2  = 9 then S457X_2 = .;
    if S457X_3  = 9 then S457X_3 = .;
    if S457X_4  = 9 then S457X_4 = .;
    if S457X_5  = 9 then S457X_5 = .;
    if S457X_6  = 9 then S457X_6 = .;
    if S458A_1  = 9 then S458A_1 = .;
    if S458A_2  = 9 then S458A_2 = .;
    if S458A_3  = 9 then S458A_3 = .;
    if S458A_4  = 9 then S458A_4 = .;
    if S458A_5  = 9 then S458A_5 = .;
    if S458A_6  = 9 then S458A_6 = .;
    if S458B_1  = 9 then S458B_1 = .;
    if S458B_2  = 9 then S458B_2 = .;
    if S458B_3  = 9 then S458B_3 = .;
    if S458B_4  = 9 then S458B_4 = .;
    if S458B_5  = 9 then S458B_5 = .;
    if S458B_6  = 9 then S458B_6 = .;
    if S458C_1  = 9 then S458C_1 = .;
    if S458C_2  = 9 then S458C_2 = .;
    if S458C_3  = 9 then S458C_3 = .;
    if S458C_4  = 9 then S458C_4 = .;
    if S458C_5  = 9 then S458C_5 = .;
    if S458C_6  = 9 then S458C_6 = .;
    if S459A_1  = 9 then S459A_1 = .;
    if S459A_2  = 9 then S459A_2 = .;
    if S459A_3  = 9 then S459A_3 = .;
    if S459A_4  = 9 then S459A_4 = .;
    if S459A_5  = 9 then S459A_5 = .;
    if S459A_6  = 9 then S459A_6 = .;
    if S459B_1  = 9 then S459B_1 = .;
    if S459B_2  = 9 then S459B_2 = .;
    if S459B_3  = 9 then S459B_3 = .;
    if S459B_4  = 9 then S459B_4 = .;
    if S459B_5  = 9 then S459B_5 = .;
    if S459B_6  = 9 then S459B_6 = .;
    if S459C_1  = 9 then S459C_1 = .;
    if S459C_2  = 9 then S459C_2 = .;
    if S459C_3  = 9 then S459C_3 = .;
    if S459C_4  = 9 then S459C_4 = .;
    if S459C_5  = 9 then S459C_5 = .;
    if S459C_6  = 9 then S459C_6 = .;
    if S459D_1  = 9 then S459D_1 = .;
    if S459D_2  = 9 then S459D_2 = .;
    if S459D_3  = 9 then S459D_3 = .;
    if S459D_4  = 9 then S459D_4 = .;
    if S459D_5  = 9 then S459D_5 = .;
    if S459D_6  = 9 then S459D_6 = .;
    if S459E_1  = 9 then S459E_1 = .;
    if S459E_2  = 9 then S459E_2 = .;
    if S459E_3  = 9 then S459E_3 = .;
    if S459E_4  = 9 then S459E_4 = .;
    if S459E_5  = 9 then S459E_5 = .;
    if S459E_6  = 9 then S459E_6 = .;
    if S459F_1  = 9 then S459F_1 = .;
    if S459F_2  = 9 then S459F_2 = .;
    if S459F_3  = 9 then S459F_3 = .;
    if S459F_4  = 9 then S459F_4 = .;
    if S459F_5  = 9 then S459F_5 = .;
    if S459F_6  = 9 then S459F_6 = .;
    if S459G_1  = 9 then S459G_1 = .;
    if S459G_2  = 9 then S459G_2 = .;
    if S459G_3  = 9 then S459G_3 = .;
    if S459G_4  = 9 then S459G_4 = .;
    if S459G_5  = 9 then S459G_5 = .;
    if S459G_6  = 9 then S459G_6 = .;
    if S459H_1  = 9 then S459H_1 = .;
    if S459H_2  = 9 then S459H_2 = .;
    if S459H_3  = 9 then S459H_3 = .;
    if S459H_4  = 9 then S459H_4 = .;
    if S459H_5  = 9 then S459H_5 = .;
    if S459H_6  = 9 then S459H_6 = .;
    if S459X_1  = 9 then S459X_1 = .;
    if S459X_2  = 9 then S459X_2 = .;
    if S459X_3  = 9 then S459X_3 = .;
    if S459X_4  = 9 then S459X_4 = .;
    if S459X_5  = 9 then S459X_5 = .;
    if S459X_6  = 9 then S459X_6 = .;
    if S506_01  = 99 then S506_01 = .;
    if S506_02  = 99 then S506_02 = .;
    if S506_03  = 99 then S506_03 = .;
    if S506_04  = 99 then S506_04 = .;
    if S506_05  = 99 then S506_05 = .;
    if S506_06  = 99 then S506_06 = .;
    if S506_07  = 99 then S506_07 = .;
    if S506_08  = 99 then S506_08 = .;
    if S506_09  = 99 then S506_09 = .;
    if S506_10  = 99 then S506_10 = .;
    if S506_11  = 99 then S506_11 = .;
    if S506_12  = 99 then S506_12 = .;
    if S506_13  = 99 then S506_13 = .;
    if S506_14  = 99 then S506_14 = .;
    if S506_15  = 99 then S506_15 = .;
    if S506_16  = 99 then S506_16 = .;
    if S506_17  = 99 then S506_17 = .;
    if S902     = 9 then S902 = .;
    if S903A    = 9 then S903A = .;
    if S903B    = 9 then S903B = .;
    if S903C    = 9 then S903C = .;
    if S903D    = 9 then S903D = .;
    if S903E    = 9 then S903E = .;
    if S903X    = 9 then S903X = .;
    if S903Z    = 9 then S903Z = .;
    if S904     = 9 then S904 = .;
    if S906     = 9 then S906 = .;
    if S907     = 9 then S907 = .;
    if S908     = 9 then S908 = .;
    if S909     = 9 then S909 = .;
    if S910     = 9 then S910 = .;
    if S911     = 9 then S911 = .;
    if S913     = 9 then S913 = .;
    if S914     = 99 then S914 = .;
    if S915     = 9 then S915 = .;
    if S916     = 99 then S916 = .;
    if S917     = 9 then S917 = .;
    if S918     = 9 then S918 = .;
    if S920     = 9 then S920 = .;
    if S921     = 9 then S921 = .;
    if S922A    = 9 then S922A = .;
    if S922B    = 9 then S922B = .;
    if S922C    = 9 then S922C = .;
    if S922D    = 9 then S922D = .;
    if S922X    = 9 then S922X = .;
    if S922Z    = 9 then S922Z = .;
    if S924     = 9 then S924 = .;
    if S925     = 9 then S925 = .;
    if S926     = 9 then S926 = .;
    if S927     = 9 then S927 = .;
    if S928     = 9 then S928 = .;
    if S930     = 9 then S930 = .;
    if S931A    = 9 then S931A = .;
    if S931B    = 9 then S931B = .;
    if S931C    = 9 then S931C = .;
    if S931D    = 9 then S931D = .;
    if S931X    = 9 then S931X = .;
    if S931Z    = 9 then S931Z = .;
    if S932     = 9 then S932 = .;
    if S933     = 9 then S933 = .;
    if S934M    = 99 then S934M = .;
    if S934Y    = 9999 then S934Y = .;
    if S935     = 9 then S935 = .;
    if S936     = 99 then S936 = .;
    if S937     = 9 then S937 = .;
    if S938     = 9 then S938 = .;
    if S939     = 9 then S939 = .;
    if S940     = 9 then S940 = .;
    if S941     = 99 then S941 = .;
    if S942     = 9 then S942 = .;
    if S943     = 99 then S943 = .;
    if S944     = 99 then S944 = .;
    if S1301A   = 9 then S1301A = .;
    if S1301B   = 9 then S1301B = .;
    if S1301C   = 9 then S1301C = .;
    if S1301D   = 9 then S1301D = .;
    if S1301E   = 9 then S1301E = .;
    if S1301F   = 9 then S1301F = .;
    if S1301G   = 9 then S1301G = .;
    if S1301H   = 9 then S1301H = .;
    if S1301I   = 9 then S1301I = .;
    if S1301J   = 9 then S1301J = .;
    if S1301K   = 9 then S1301K = .;
    if S1301L   = 9 then S1301L = .;
    if S1302A   = 9 then S1302A = .;
    if S1302B   = 9 then S1302B = .;
    if S1302C   = 9 then S1302C = .;
    if S1302D   = 9 then S1302D = .;
    if S1302E   = 9 then S1302E = .;
    if S1302F   = 9 then S1302F = .;
    if S1302G   = 9 then S1302G = .;
    if S1302H   = 9 then S1302H = .;
    if S1310A   = 9 then S1310A = .;
    if S1310B   = 9 then S1310B = .;
    if S1310C   = 9 then S1310C = .;
    if S1310D   = 9 then S1310D = .;
    if S1310E   = 9 then S1310E = .;
    if S1310F   = 9 then S1310F = .;
    if S1310G   = 9 then S1310G = .;
    if S1310H   = 9 then S1310H = .;
    if S1310I   = 9 then S1310I = .;
    if S1310J   = 9 then S1310J = .;
    if S1310X   = 9 then S1310X = .;
    if S1313    = 9 then S1313 = .;
    if S1314    = 9 then S1314 = .;
    if S1315    = 9 then S1315 = .;
    if S1316A   = 9 then S1316A = .;
    if S1316B   = 9 then S1316B = .;
    if S1316C   = 9 then S1316C = .;
    if S1316D   = 9 then S1316D = .;
    if S1316E   = 9 then S1316E = .;
    if S1316F   = 9 then S1316F = .;
    if S1316G   = 9 then S1316G = .;
    if S1316H   = 9 then S1316H = .;
    if S1321    = 9 then S1321 = .;
Run;
