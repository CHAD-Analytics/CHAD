### Source Information

*This section references the data collected, the sources of collection,
and the data manipulation.*

**Data Collection**:

1.  Confirmed Cases of COVID-19
2.  Confirmed deaths from COVID-19
3.  Department of Air Force installation name, MAJCOM, location, and state
4.  US county FIPS codes, location, state, population, land area
5.  Hospital FIPS codes, location, state, number of beds, type
6.  Distance matrices for counties and hospitals related to each Department of
    Air Force installation (preemptively created for this analysis)

**Data Sources**:

1. **John Hopkins University:** JHU provides daily updated information every morning for cumulative
    cases and deaths from the COVID-19 in every county around the
    U.S. 
    
    <a href="https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"             class="uri">https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv</a>
    
    <a href="https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv"             class="uri">https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv</a>


2.  **Open COVID-19 Github Repositrory:** This GitHub repository aggregates data from across multiple sources in order to create a database of time series COVID-19 cases around the world. Most European countries and large Asian countries have data broken out by region. Data is validated and pulled against country health departments when available. Updates daily.

    <a href="https://github.com/open-covid-19/data" class="uri">https://github.com/open-covid-19/data</a>

3.  **The American Hospital Directory:** The AHD contains stats for all
    non-federal, short term, acute care  and critical access hospitals in every state. Data
    includes number of staffed beds along with total discharges and
    total patient days. These metrics are used to estimate an average
    hospital utilization.
    
    <a href="https://www.ahd.com/state_statistics.html" class="uri">https://www.ahd.com/state_statistics.html</a>

4.  **U.S. Census Bureau:** The U.S. Census Bureau provides population data and age demographics for all U.S. counties.
    
    <a href="https://www.census.gov/data/tables/time-series/demo/popest/2010s-counties-detail.html" class="uri">https://www.census.gov/data/tables/time-series/demo/popest/2010s-counties-detail.html</a>

5.  **Institute for Health Metrics and Evaluation:** This website houses the data produced by the IHME model. Fatality and hospitalization projections are recorded for every state, and a large number of countries. Model updates occur typically 2-3 times a week.
    
    <a href="https://covid19.healthdata.org/projections" class="uri">https://covid19.healthdata.org/projections</a>

6.  **CHAD Github Repository:** Loaded on this repository is public static data regarding Department of Air Force installations, and distance matrices between hospitals and bases as well as counties and bases.
    
    <a href="https://github.com/CHAD-Analytics/CHAD" class="uri">https://github.com/CHAD-Analytics/CHAD</a>

**Data Manipulation**:

1.  **Distance matrices**: Distance matrices were created to limit the
    local area around an installation. Members of the AFIT team helped build
    these matrices to measure relative mileage distance between base and
    county as well as base and hospital. The distances are calculated
    using geospatial coordinates (latitude, longitude) to measure
    distance and store in a matrix.
