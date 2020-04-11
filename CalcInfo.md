### Calculation Information

*These reference the essential calculations used to conduct analysis and
the assumptions made regarding the data*

**Dynamic Elements**:

1.  **Local Counties**: Counties included in the analysis are determined
    using geospatial coordinates (Latitude, Longitude) and determining
    the mileage distance from the base to the center of a county.
    Counties are included if their central location lies within the
    specified radius.

2.  **Hospitals**: Hospitals are calculated using geospastial distance
    between the base and the hospital similar to local counties.
    Hospitals are included based on their mileage distance to the
    installation of interest. Furthermore, hospitals are filtered to
    only inlcude “General Acute Care” and “Critical Access” as these are the
    primary types of hospitals to supply beds and ventilators to
    Covid-19 patients.

**Case Statistics**:

1.  **Confirmed Cases**: Confirmed COVID-19 cases and deaths are
    reported daily and summed by state and local region, which is
    determined by the user.

2.  **Hospitalization Utilzation**: Hospitalization rates are a function
    of the total number of hospital beds in a county and the number of
    locally confirmed cases. The percentage of hospitalized patients
    totals the number of confirmed cases in the past 7 days (average
    length of hospital stay) and multiplied by the Estimated Values for hospitalization rates. This number
    is then added to the average historical hospital bed capacity for the region
    currently being examined.

3.  **Percentage Change in Hospitalization Utilization**: The change in
    hospitalization rates is calculated by the the difference in new
    cases and the confirmed cases 7 days prior. With that difference,
    the change is then divided by the total number of hospital beds in
    the local area to calculate a percentage change in the local area.

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
