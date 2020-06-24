## Calculation Information

*These reference the essential calculations used to conduct analysis and
the assumptions made regarding the data. See Projections for information on projection models displayed in CHAD.*

### Distance Matrices

1.  **Local Counties**: Counties included in the analysis are determined
    using geospatial coordinates (Latitude, Longitude) and determining
    the mileage distance from the base to the center of a county.
    Counties are included if their central location (centroid) lies within the
    specified radius.

2.  **Hospitals**: Hospitals are included based on their straight line mileage distance to the
    installation of interest. Furthermore, hospitals are filtered to
    only include “General Acute Care” and “Critical Access” as these are the
    primary types of hospitals to supply beds and ventilators to
    COVID-19 patients.
    
### National/International Table

1.  **Average New Cases/Deaths Per Day**: This metric is calculated using a 7 day-average of daily cases/deaths.

2.  **Weekly Total Case Change**: This metric is calculated using the total cases in the last 7 days and dividing it by the overall total cases up to the last 7 days. It shows the weekly change in cases when compared to the total for that area. This can help put the case numbers in persepctive to identify if regions are experiencing an initial surge.

3.  **Weekly Case Change**: This metric is a ratio of cases in the past 7 days to cases in the week before that. This is helpful in identifying where outbreaks may be speeding up or slowing down, when used in conjunction with other metrics.

*Note: For all metrics in the top 5/bottom 5 tables, countries with a population less than 100,000 are excluded.*

### Local Health Statistics

1.  **Confirmed Cases**: Confirmed COVID-19 cases and deaths are
    reported daily and summed by state or local region, which is
    determined by the user.

2.  **Hospital Bed Utilzation**: Hospital bed utilization is calculated using estimated historic utilization numbers
    along with estimated hospitalized patients. Using an average hospital length stay of 7 days for COVID-19 patients, the total       confirmed cases in the last 7 days are used to estimate the number of patients currently in the hospital. Assuming every       patient uses a staffed bed, this number is divided by the total number of hosital beds in the user selected region. This percentage is added to the estimated historic utilization percentage to determine an overall utilization percentage of hospital beds in the region. For further information on this calculation please reference the document below:
    https://github.com/CHAD-Analytics/CHAD/raw/master/covid19/www/7_other_resources/Estimated%20Local%20Hospital%20Bed%20Utilization.docx

3.  **Case Doubling rate**: The case doubling rate is calculated for the current day. It involves counting the days since the confirmed case total was less than half of the current confirmed case total.

4. **Estimated Reproduction rate**: Calculating R_t can be quite difficult since some of the parameters aren't directly captured by data. Consultation with medical SMEs yielded a rough way to estimate R_t. This is done by dividing the 5 day case average by the 14 day case average. Values less than 1 indicate the possiblilty of slowing growth. 

5. **3-Day Moving Average**: This chart is created from plotting the 3-day moving average of cases as opposed to each day's individual case number. The moving average is taken from the current day's cases and two days prior. This is done to help provide a smoothing function to the chart, and correct for some of the irregularities in county case reporting procedures.

6. **Weekly Growth Chart**: This chart tracks the change in case growth per week. To do this, a rolling weekly case number can't be used since that would change the comparison every 6 days. Weekly total case counts are calculated from Monday-Sunday every week. The percent change from one week to the next is the weekly growth. To avoid instances of 4000% growth due to intial case increases, the threshold is set to total cases being greater than 30 before it starts counting weekly growth. This is why the dates on the chart may not start at the same week for each location. 

7. **Cases per 1,000/10,000/100,000**: This ratio is calculated the standard way. Total cases divided total population times your per captia number (1,000/10,000/100,000).

8. **Estimated Recovered Line**: This calculation appears as a line on the total cases chart. This estimation assumes a 14 day period from reported case to recovered. Though it is known that many people are being tested well into the incubation period, it is not known how far in. It is therefore more conservative to start the 14-period from the day the case was reported to reduce the chance of overestimating the amount of recovered people. An assumpation is also made that fatalities occur 7 days after test
