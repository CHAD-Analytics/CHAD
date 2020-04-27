### Calculation Information

*These reference the essential calculations used to conduct analysis and
the assumptions made regarding the data*

**Dynamic Elements**:

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

**Case Statistics**:

1.  **Confirmed Cases**: Confirmed COVID-19 cases and deaths are
    reported daily and summed by state and local region, which is
    determined by the user.

2.  **Hospital Bed Utilzation**: Hospital bed utilization is calculated using estimated historic utilization numbers
    along with estimated hospitalized patients. Using an average hospital length stay of 7 days for COVID-19 patients, the total       confirmed cases in the last 7 days are used to estimate the number of patients currently in the hospital. Assuming every       patient uses a staffed bed, this number is divided by the total number of hosital beds in the user selected region. This percentage is added to the estimated historic utilization percentage to determine an overall utilization percentage of hospital beds in the region. For further information on this calculation please reference the document below:
    https://github.com/treypujats/CHAD/blob/master/Estimated%20Local%20Hospital%20Bed%20Utilization.docx?raw=true

3.  **Case Doubling rate**: The case doubling rate is calculated for the current day. It involves counting the days since the confirmed case total was less than half of the current confirmed case total.

4. **Estimated Reproduction rate**: Calculating R_t can be quite difficult since some of the parameters aren't directly captured by data. Consultation with a medical SME yielded a rough way to estimate R_t. This is done by dividing the 5 day case average by the 14 day case average. Values less than 1 indicate the possiblilty of slowing growth.  

### Predictive Models:

**IHME**: Developed by the University of Washington’s Institute for
Health Metrics and Evaluation, this model uses data from state and
national governments, hospital groups, and the World Health Organization
to predict hospitalizations and ICU rates for planning purposes. Their model can be found at
<a href="https://covid19.healthdata.org/united-states-of-america" class="uri">https://covid19.healthdata.org/united-states-of-america

**SEIAR**: This predictive model was pulled from the Army’s ACME Tool
developed through collaborative efforts from Army Futures Command and
the Army Public Health Center(APHC) COVID-19 Modeling Team. This model
was inspired by Penn State University’s CHIME model. Detailed
information about the CHIME model can be found at
<a href="https://code-for-philly.gitbook.io/chime/" class="uri">https://code-for-philly.gitbook.io/chime/</a>.
