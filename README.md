## Introduction
This is my submission for the 2nd project in the August 2015 run of "Reproducible Research", part of the JHU DSS series on Coursera. Because of the submission requirements, this assignment is best by downloading the repository and viewing pa2.html. Viewing the .Rmd file only deprives one of the plots and sortable HTML table in the appendix -- a fate most wretched, indeed. :-)

## Postmortem
After project submission, the ever helpful CTA David Hood pointed out one particular note of interest which many of us (including me) missed: a `FLOOD` event was encoded with a severe typo in the Property Damage account. A more astute observer would have caught the gross error of $150bn for one event during data exploration! Notably, this was nearly 40% of the cumulative total in the 1993-2011 data.

Would have to refer to now-corrected NOAA data, but I believe the correct amount was around $1.5bn. The error resulted in a two orders of magnitude discrepancy for the event. A [separate NOAA report](http://www.nws.noaa.gov/om/hazstats/resources/weather_fatalities.pdf) shows a 2007 flood with a matching number of fatalities as costing ~$12.3bn in total damages, which indeed suggests property damage, in this case, of ~$1.5bn.
