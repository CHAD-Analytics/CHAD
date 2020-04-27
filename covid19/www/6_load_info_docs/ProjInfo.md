### Summary

The two projections on the Local and National Health Projections tabs
are adapted from the IHME model and the CHIME model. Each model is
designed with specific assumptions and parameters. The table below
offers a quick comparison of the two models.

<table>
<thead>
<tr class="header">
<th style="text-align: center;">Model</th>
<th style="text-align: center;">IHME</th>
<th style="text-align: center;">CHIME</th>

<td class="divider"><hr /></td>
</tr>

</thead>
<tbody>
<tr class="odd">
<td style="text-align: center;">Institution</td>
<td style="text-align: center;">IHME of University of Washington</td>
<td style="text-align: center;">University of Pennsylvania</td>
<td class="divider"><hr /></td>

<tr class="even">
<td style="text-align: center;">Modeling Approach</td>
<td style="text-align: center;">Data-driven, Statistical</td>
<td style="text-align: center;">SIR-based, deterministic</td>
<td class="divider"><hr /></td>

<tr class="odd">
<td style="text-align: center;">Strengths</td>
<td style="text-align: center;">DoD Supported, ICU Capacity</td>
<td style="text-align: center;">DoD Suported, ICU Capactiy</td>
<td class="divider"><hr /></td>

<tr class="even">
<td style="text-align: center;">Limitations</td>
<td style="text-align: center;">Relies on initial data</td>
<td style="text-align: center;">Lacks seasonal factors, deterministic</td>
<td class="divider"><hr /></td>

<tr class="odd">
<td style="text-align: center;">Location Concentration</td>
<td style="text-align: center;">U.S. and individual states</td>
<td style="text-align: center;">Specific regions with states</td>
<td class="divider"><hr /></td>

<tr class="even">
<td style="text-align: center;">Inputs</td>
<td style="text-align: center;">Projection timeframe, current hospitalizations, mitigation</td>
<td style="text-align: center;">Total population, age distribution, epidemiology info, mitigation, severity</td>
<td class="divider"><hr /></td>

<tr class="odd">
<td style="text-align: center;">Outputs</td>
<td style="text-align: center;">Projected cases over time, ICU use</td>
<td style="text-align: center;">Projected cases over time, ICU use</td>
<td class="divider"><hr /></td>
</tr>
<tr class="even">
<td style="text-align: center;">Source</td>
<td style="text-align: center;"><a href="https://covid19.healthdata.org/projections/" class="uri">https://covid19.healthdata.org/projections/</a></td>
<td style="text-align: center;"><a href="https://penn-chime.phl.io/" class="uri">https://penn-chime.phl.io/</a></td>
</tr>
</tbody>
</table>

### IHME

The IHME model was built by the Institute for Health Metrics and
Evaluation (IHME) at the University of Washington. It assumes that three
of the four mitigation measures of closing schools, closing
non-essential services, shelter-in-place, and significant travel
restricitons have been implemented. The shaded region represents a
region of uncertainty on the projected expected values over time.

### CHIME

The COVID-19 Hospital Impact Model for Epidemics (CHIME) app was created
by the University of Pennsylvania. CHIME provides estimates of daily and
cumulative cases, hospitalizations, ICU admissions, and patients
requiring ventilators. It uses a Susceptible-Infected-Recovered (SIR)
model to generate estimates based on Estimated Values. There are many inputs to CHIME
relative to the location of interest. In CHAD, the user has the ability
to change the percent reduction of social contact. Furthermore, the
model in CHAD is an adaptation of CHIME that includes Exposed and
Asympotmatic in addition to Susceptible, Infected, and Asympotmatic.

The shaded region represents a region of uncertainty on the projected
expected values over time. The CHIME model itself does not provide a
region of uncertainty. However, uncertainty was calculated using
Estimated Value best and worst case scenario paramters to generate upper and
lower bound curve. The resulting region of uncertainty often shows a
*crossing* effect. This is because best case and worst case scenarios
move the peak of the curve. When the curve is flattened, the duration of
the pandemic is also lengthened, displaying a *crossing* effect between
the best and worst case projection curves.

