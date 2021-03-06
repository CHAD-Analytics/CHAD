### Included Projection Models

CHAD utilizes a mulititude of well known models from institutions and organizations in order to give a comprehensive picture of various forecasts. This section will list and keep track of all models being showcased in CHAD. 


### IHME

The IHME model was built by the Institute for Health Metrics and Evaluation (IHME) at the University of Washington. It assumes that three of the four mitigation measures of closing schools, closing non-essential services, shelter-in-place, and significant travel restricitons have been implemented. The IHME model is built for all U.S. states and certain countries. The models are adapted by CHAD at the county level by a simplistic scaling method using a ratio of the local population to the state population. 

**Data:**
http://www.healthdata.org/covid/data-downloads

**Live Model:**
https://covid19.healthdata.org/united-states-of-america

**Information:**
http://www.healthdata.org/covid/faqs


### Center for Army Analysis (CAA)

**Model Appropriateness**
A SEIR model is a compartmental population model with four bins – susceptible, exposed, infected, and removed – each with their own function that gives bin size with respect to time. Other compartmental population models similar to SEIR include SIR (susceptible, infectious, and removed), SIS (susceptible, infectious, susceptible), MSIR (maternally-derived immunity, susceptible, infectious, removed), and MSEIRS (maternally-derived immunity, susceptible, infectious, removed, and susceptible). Of these common models in infectious disease modeling, Center for Army Analysis (CAA) selected the SEIR construct because it accounts for important components of the COVID-19 disease process.

**Model Assumptions**
Key Modeling Assumptions:
• Total population remains constant (birth rate = mortality rate).
• Population suffering re-infection is statistically insignificant.
• Infection rates are constant across all demographics – all members of the population are equally
susceptible.
• Population is well-mixed amongst the bins.
• No intermixing between discrete population bins (an individual cannot be in the susceptible bin
and the removed bin at the same time).
• There is no difference in the behavior between asymptomatic and symptomatic infected people.
Data Assumptions:
• Hospitalization and ICU demand is ~2x as seasonal influenza.
• Hospital/ICU Bed availability assumed to be similar to that of the US Medical System.
Strict alignment between national populations and national hospital/ICU bed spaces (geographic
dispersion of hospitals vis-a-vis population is ignored).

**Methodology**
Each day, CAA uses all available historical data to estimate historical R0 values from a Weibull
distribution at the core based statistical area (CBSA) level.8 Using this Weibull function, CAA forecasts R0 values. The R0 values are then grouped into three categories: 1) CBSAs with few cases; 2) CBSAs with high population densities; and 3) CBSAs with low population densities. The R0 values for the high and low population densities are regressed against factors such as transformations on the population density, day of infection timeline, and date of stay at home order to create a parametric R0 function. The parametric R0 function then forecasts R0 values which informs β, a factor in the functions determining population bin size. Using the number of reported cases, calculated estimates for the exposed population, and the model functions, CAA forecasts the susceptible, exposed, infectious, and removed populations and compares them against general hospital bed and intensive care unit (ICU) bed capacities to produce COVID-19 response metrics at the county and core based statistical area level.


### Leavitt Partners Insight COVID-19 Burden Index (Torch Insight)

The Torch Insight COVID-19 Burden Index Projections analyzes hospital and ICU burden and
estimates the total hospital bed and ICU capacity in counties and hospitals given current and projected
COVID-19 case levels. We extend projections up to 180 days from the most recent COVID-19 reported
case numbers. The Burden Index Projections estimate if the hospital or county has exceeded its normal
capacity. To adjust for current conditions in specific regions, the Burden Projections:
• Factor in county-specific infection rates: Our projections are based on growth curves of what is
actually happening on the ground and update daily. Thus, when states implement social
distancing, shelter-in-place, or other measures, our projections take these into account by
updating accordingly as new case count data comes in. The models reflect what is observed to
be happening, not what is hoped or assumed will happen.
• Adjusts for Hospital Service Area: Not all counties have hospitals, and sometimes patients cross
county lines to go to different hospitals. We base our projections for counties and individual
hospitals on hospital service areas (HSAs), which are local healthcare markets for hospital care,
rather than simply on where the hospital is located. This means we have scores even for
counties that have no hospitals in their borders, and that we account for hospital patients from
outside the hospital's county.
• Adjusts for Hospital Baseline Capacity: Some hospitals were already near capacity before the
COVID crisis. We base our projections of capacity on hospital- and ICU-bed utilization rates
derived from the 2018 CMS Hospital Cost Reports.
• Eliminates Hospital Elective Procedures: Many hospitals can make additional beds available for
COVID-related illness by postponing elective admissions. Based on a sample of all-payer claims
and 100% of Medicare hospitalizations we estimate the percent of admissions in each hospital
that are elective, allowing us to account for increased capacity over the baseline utilization.

**Data:**
https://covid19.torchinsight.com/

**Methodology:**
https://covid19.torchinsight.com/method.html

**Non-Technical Summary:**
https://www.fiercehealthcare.com/hospitals-health-systems/industry-voices-new-covid-19-model-can-support-those-front-lines-aid 


### CHIME

The COVID-19 Hospital Impact Model for Epidemics (CHIME) app was created by the Predictive Healthcare team at Penn Medicine, University of Pennsylvania. CHIME provides estimates of daily and cumulative cases, hospitalizations, ICU admissions, and patients requiring ventilators. It uses an adapation of the Susceptible-Infected-Recovered (SIR) model to generate estimates based on user inputs. There are many inputs to CHIME relative to the location of interest. In CHAD, majority of these are pre-filled in and are specfic to the location chosen and COVID-19 case data. Furthermore, the model in CHAD is an adaptation of CHIME that includes Exposed and Asympotmatic in addition to Susceptible, Infected, and Asympotmatic. This adaption came from the Army’s ACME Tool, developed through collaborative efforts from Army Futures Command and the Army Public Health Center(APHC) COVID-19 Modeling Team.

**Live Model:**
https://penn-chime.phl.io/

**Information:**
https://code-for-philly.gitbook.io/chime/what-is-chime/sir-modeling

### Columbia University SEIR Model

CU uses a metapopulation SEIR model to project the spread of COVID-19 cases in the United States at county level. This model is updated weekly with projections of new COVID-19 cases, infections, and available critical care beds in states and counties across the United States. The model has multiple scenarios covering a six-week period, which are ran under a variety of social distancing and hospital responses.

**Data:**
https://github.com/shaman-lab/COVID-19Projection/tree/master/Projection_May14

**Information:**
http://www.columbia.edu/~sp3449/covid-19.html

### Los Alamos National Laboratory Model

The LANL model provides forecasts for infections and deaths short term and long term across all U.S. states. It consists of a statistical model that details how the number of COVID-19 infections changes over time. This is then mapped to the number of infections to the reported data. The model uses the assumption that interventions will be implemented and continue to be upheld in the future. The LANL is very detailed and it is advised that users review the links below for more information.

**Data:**
https://covid-19.bsvgateway.org/#link%20to%20forecasting%20site

**Information:**
https://covid-19.bsvgateway.org/#link%20to%20forecasting%20site

### University of Texas Model

The COVID-19 Modeling Consortium at UT Austin builds their model by using daily data on COVID-19 deaths, and mobile-phone data in order to characterize each state's social-distancing behavior. This allows them to form projections three weeks out for COVID-19 death rates in all US states and most major metropolitan areas. Their approach utilizes a Bayesian multilevel negative binomial regression model as opposed to a traditional SIR model, meaning it does not require estimates for different to calculate epidemiological parameters.

**Data:**
https://github.com/UT-Covid/USmortality

**Live Model:**
https://covid-19.tacc.utexas.edu/projections/

**Information:**
https://covid-19.tacc.utexas.edu/mortality-projection-faq/
https://covid-19.tacc.utexas.edu/media/filer_public/87/63/87635a46-b060-4b5b-a3a5-1b31ab8e0bc6/ut_covid-19_mortality_forecasting_model_latest.pdf

### Youyang Gu Model

This model is an independent venture developed by Youyang Gu and featured on the CDC website. It is a SEIS mechanistic model that utilizes machine learning to make infection and death projections for all 50 states and multiple countries. This model is one of the only ones accounting for state-by-state reopenings within its projections.

**Data:**
https://github.com/youyanggu/covid19_projections/tree/master/

**Live Model:**
https://covid19-projections.com/

**Information:**
https://covid19-projections.com/about/



