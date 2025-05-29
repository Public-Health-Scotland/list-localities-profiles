# Custom Boundaries SOP

## Requirements

-  A set of boundaries that are coherent with Intermediate Zones - This means the custom boundaries must be at one Intermediate Zone in size and **must** be made up of complete Intermediate Zones.
-  Generally, we will only do one per partnership (i.e. no multiple custom boundaries for the same partnership!) per year (possibly per release).
-  The localities team has capacity
   - Team members' other work
   - We will usually only work on one custom profile at a time.
   - We will not work on a custom profile around the time we are producing a 'main' release.

## Lookup files

Request a lookup file from the team with the following columns:

- datazone2011
- hscp_locality
- hscp2019name
- hscp2019
- hb2019name
- hb2019
- intzone2011name
- intzone2011

(One idea would be to send an example of a lookup provided before in the correct format?) 

## Process

1.  Get the new lookups in order.
2.  Create a new data folder and update variables in a handful of places to reflect this.
3.  Run the MSG code in the Unscheduled Care folder to get MSG data at IZ-level-change folder path- change folder in `MSG 0`. Also note you must make sure the AE folder is in your new folder.
4.  Run the `Unscheduled Care 1 IZ` and `Unscheduled care 2` scripts to produce the data files needed- change the folder name.
5.  Run the `General Health 2` and `General Health 3` script to produce the data files needed- change folder and variable names.
6.  Run the full indicators as usual.


## Notes

- Some data has to be re-extracted from the underlying databases, in these cases the data will be more recent (and likely slightly different) to the data in the 'main' profiles for the same HSCP.
- Some data is not available at the Intermediate Zone level, therefore it is not possible for us to (reasonably) reproduce this for a custom boundary.
- Some ScotPho data is only released as the indicator at IZ-level, therefore it is not possible for us to properly calculate the indicator for a custom boundary. For these statistics, we will simply take the mean of the values at IZ-level and set the confidence interval to be the same as the value.

We can't produce these indicators as the data isn't readily available at Intermediate zone level:

 - Cancer Registrations/Incidence
 - Population with a prescription for anxiety, depression or psychosis
 - Alcohol Related Mortality
 - Drug Related Hospital Admissions
 - Psychiatric hospitalisations 
  
