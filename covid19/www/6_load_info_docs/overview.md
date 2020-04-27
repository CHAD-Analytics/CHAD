### Effectively using the Air Force COVID-19 Health Assessment Dashboard (CHAD)

Website: https://armyanalytics.shinyapps.io/covid_analysis/

Use the drop box on the left tool bar to select which Air Force Installation to review.  interface between the CHAD application overview, model controls, and – once a model is run from the model control page – a facility impact assessment and a MTF reports page.

### APPLICATION OVERVIEW

This application pulls the most current modeling information from IHME, CHIME, etc.  The COVID-19 Hospital Impact Model for Epidemics (CHIME) is a civilian tool created by the University of Pennsylvania, and its capabilities have been expanded for the Air Force. The CHIME modifications were previously performed in the ACME model developed by LTC Isaac J. Faber, Chief Data Scientist at the AI Task Force with Army Futures Command, in consultation with the Army Public Health Center COVID-19 Modeling Team. 

ACME takes into consideration the dynamic nature of the present pandemic. Projections are generated using a mechanistic SIR or SEIAR model. Users have four sets of model controls:
* Meta controls: MTF selection, currently hospitalized COVID-19 patients, social distancing (percent reduction in social contact), and number of days to project the model to
* Proportion of impact: Hospitalization, ICU, and ventilated proportions of total infections
* Duration: Hospital, ICU, and ventilation length of stay or use
* Advanced controls (SIR): Doubling time before social distancing, days to recover once infected, and proportion of total infections identified
* Advanced controls (SEIAR):Doubling time before social distancing, days to recover once infected, latent period, and time to symptoms


Currently, two sets of scenarios are available for proportion of impact. The scenarios were estimated based on total infections. The first is a “most likely” scenario, which projects 0.60% for hospitalization, 16% of those are ICU, and 44% of those require ventilator support. The second is a “conservative” scenario, which projects 3.00% for hospitalizations, 16% of those are ICU, and 44% of those require ventilator support. 

| Variables                                          	| Most likely Scenario 	| Conservative Scenario 	|
|----------------------------------------------------	|----------------------	|-----------------------	|
| Currently hospitalized COVID-19 patients           	| 1                    	| 1                     	|
| Social distancing (% reduction in social contact)  	| 30%                  	| 30%                   	|
| Number of days to project                          	| 200                  	| 200                   	|
| Hospitalization proportion (% of total infections) 	| 0.6%                 	| 3.0%                  	|
| ICU proportion (% of total hospitalizations)        | 16%                 	| 16%                   	|
| Ventilated proportions (% of total ICU)     	      | 44%                 	| 44%                   	|
| Hospital length of stay                            	| 7                    	| 7                     	|
| ICU length of stay                                 	| 9                    	| 9                     	|
| Ventilation length of stay                         	| 10                   	| 10                    	|
| Doubling time before social distancing (days)      	| 6                    	| 6                     	|
| Days to recover once infected                      	| 14                   	| 14                    	|
| Proportion of infections identified                	| 50%                  	| 50%                   	|
