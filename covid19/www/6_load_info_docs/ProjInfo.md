### Included Projection Models

CHAD utilizes a mulititude of well known models from institutions and organizations in order to give a comprehensive picture of various forecasts. This section will list and keep track of all models being showcased in CHAD. 


### IHME

The IHME model was built by the Institute for Health Metrics and Evaluation (IHME) at the University of Washington. It assumes that three of the four mitigation measures of closing schools, closing non-essential services, shelter-in-place, and significant travel
restricitons have been implemented. The IHME model is built for all U.S. states and certain countries. The models are adapted by CHAD at the county level by a simplistic scaling method using a ratio of the local population to the state population. 

**Data:**
http://www.healthdata.org/covid/data-downloads

**Live Model:**
https://covid19.healthdata.org/united-states-of-america

**Information:**
http://www.healthdata.org/covid/faqs

### CHIME

The COVID-19 Hospital Impact Model for Epidemics (CHIME) app was created by the Predictive Healthcare team at Penn Medicine, University of Pennsylvania. CHIME provides estimates of daily and cumulative cases, hospitalizations, ICU admissions, and patients
requiring ventilators. It uses an adapation of the Susceptible-Infected-Recovered (SIR) model to generate estimates based on user inputs. There are many inputs to CHIME relative to the location of interest. In CHAD, majority of these are pre-filled in and are specfic to the location chosen and COVID-19 case data. Furthermore, the model in CHAD is an adaptation of CHIME that includes Exposed and Asympotmatic in addition to Susceptible, Infected, and Asympotmatic. This adaption came from the Army’s ACME Tool,
developed through collaborative efforts from Army Futures Command and the Army Public Health Center(APHC) COVID-19 Modeling Team.

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



