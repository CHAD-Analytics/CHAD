### Source Information

*This section references the data collected, the sources of collection,
and the data manipulation.*

**Data Collection**:

1.  Confirmed Cases of COVID-19
2.  Confirmed deaths from COVID-19
3.  Air Force base name, MAJCOM, location, and state
4.  US county FIPS codes, location, state, population, land area
5.  Hospital FIPS codes, location, state, number of beds, type
6.  Distance matrices for counties and hospitals related to each CONUS
    Air Force installation (preemptively created for this analysis)

**Data Sources**:

1.  **<a href="https://usafacts.org/visualizations/coronavirus-covid-19-spread-map/" class="uri">https://usafacts.org/visualizations/coronavirus-covid-19-spread-map/</a>**
    : This website provides daily updated information every morning of
    cases and deaths from the coronavirus in every county around the
    U.S. The data does not include secondary effects such as deaths from
    lack of treatment due to hospital capacity, rather it only included
    deaths of individuals with COVID-19.

2.  **<a href="https://covid19.healthdata.org/projections" class="uri">https://covid19.healthdata.org/projections</a>**
    : This website houses the data produced by the IHME model.
    Projections are recorded for every state, and updated daily.

3.  **<a href="https://www.ahd.com/state_statistics.html" class="uri">https://www.ahd.com/state_statistics.html</a>**
    : The American Hospital Directory (AHD) contains stats for all
    non-federal, short term, acute care hospitals in every state. Data
    includes number of staffed beds along with total discharges and
    total patient days. These metrics are used to estimate an average
    hospital utilization.

4.  **<a href="https://github.com/treypujats/CHAD" class="uri">https://github.com/treypujats/CHAD</a>**
    : Loaded on this repository is static data regarding Air Force
    installations, US county data, hospital data, and distance matrices
    between hospitals and bases as well as counties and bases.

**Data Manipulation**:

1.  **Distance matrices**: Distance matrices were created to limit the
    local area around an installation. Members of the AFIT team helped build
    these matrices to measure relative mileage distance between base and
    county as well as base and hospital. The distances are calculated
    using geospatial coordinates (latitude, longitude) to measure
    distance and store in a matrix.
