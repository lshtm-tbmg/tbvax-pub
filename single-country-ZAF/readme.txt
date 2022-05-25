README 

April 20, 2022. Model version 3.2.7.4

1. Optimized conversion of vector to matrix (as deSolve requires passing a vector to the derivs() function) using dim() [as suggested by Chathika]

2. Documented derivs.deSolve() function ; see TB Vx documentation of principal R functions - draft v0.6 - model version 3.2.7.4.pdf

March 28, 2022. Model version 3.2.7.3

1. Added warning messages when resetting state variables to 0 [in the range between 0 and min.value.for.state.var]
   This only occurs after (not during) a simulation run and before producing output.

2. Support for flexible age ranges in target file e.g. 
   age_from = 15 and age_thru = 39 
   age_from = 15 and age_thru = 99 
   provided a correct setting of age.group.lower.limits="0,15,40" in <detailed.output> in the XML


March 7, 2022. Model version 3.2.7.2

1. Fixed a coding error in generating target hits: see TBVx-output-fncs-v18.R from line 87.

March 3, 2022. Model version 3.2.7.1

1. Fixed an issue that occurred with R version 4.0.3 [not with R 4.1.2]
   
   A call to the R 'any' function TBVx-output-fncs-v18.R to which a row of a data.frame is passed with logical values caused the error:

   Error in FUN(X[[i]], ...) : 
   only defined on a data frame with all numeric variables
   Called from: FUN(X[[i]], ...)
 
   Casting the data.frame row to logical values with as.logical() solved the issue.

   Apparently R version 4.1.2 is more forgiving....
   

Feb 24, 2022. Model version 3.2.7

1. Fixed an issue that was ultimately due to a bug in data.table
   Try this:
   library(data.table)
   df = data.frame(a=runif(20),b=runif(20))
   cbind(df[,1],x=21:40,df[,ncol(df)])
   setDT(df)
   cbind(df[,1],x=41:60,df[,ncol(df)])


   With this issue fixed everything runs smoothly (and _much_ faster than before:-)

Feb 23, 2022. Model version 3.2.6

1. Solved issues with autostepper and negative state variables

   Of course, the autostepper does not consider negative state variables as an error as it has no knowledge about the meaning of the state variables.
   The occurrence of negative state variables does mean that the time step is (slightly) too large.
   The following setting in the XML works quite well:
   <options>
      <numerical.integration method="ode23" atol="0" rtol="1e-3" hini="0.1" hmin="1e-4" hmax="0.5"/>
      <exceptional.conditions min.value.for.state.var="-0.2"/>
    </options>
   Whenever a state variable is less than min.value.for.state.var at the start of the year, an error message is written to the model.log
   After completion of a simulation run, negative state variables in the range min.value.for.state.var to 0 are reset to 0.
   This occurs before generating the stocks and flows output.
   Values more negative than min.value.for.state.var are not filtered out to be able to flag really weird issues.

2. Slow running of simulation run and frequent freeze-ups due to memory issues.
   This is mostly due to the volume of the flows output (over 20 years with 82 age groups).
   Profiling showed that the rbind() function which appends rows to a data frame caused much of the slowness.
   Replacing data frames with data tables improved this as rbind() is much faster with data tables.
   Refactoring of the current code (generate.flow.output) would still be a good idea.
   Memory issues were reduced with frequent calls to gc() [garbage collection]


Feb 14, 2022. Model version 3.2.5

1. Added a line of code to derivs.deSolve() function to reset negative values of state variables
   in the range [min.value.for.state.var, 0] (with e.g. min.value.for.state.var="-0.03") to 0.
   Values more negative than min.value.for.state.var are not reset and logged.
 

Jan  26, 2022. Model version 3.2.4

1. TBVx-optimization-fncs-v2.R has been reorganized to improve readability.

2. Added run-optimizer-ZAF.R as an example to run the optimizer (i.e. not from a set of emulator seeds).


Jan  20, 2022. Model version 3.2.4

1. Code has been reorganized to improve readability.

2. Default rtol and atol values in the XML Schema have been set to 1e-6

Jan  19, 2022. Possible issue found

1. A possible issue may have been introduced in v 3.2.1

   Running the model requires an XML file that defines the model structure and parameter values.
   In addition to that XML file a .csv with parameter values can be supplied (referred to as input.csv below).
   The parameter values in input.csv are meant to replace the XML parameters (in an in memory representation of the XML).
   To allow running the model with a set of sampled parameter values (as in emulation), in addition to input.csv a named vector of parameter values can be supplied (new.parameter.values).
   
   Up to model version 3.2.1:
   - the 'constant' values of parameters in input.csv were used to modify the XML (where 'constant' refers to parameters flagged with 'FALSE' in the choose column or flagged with 'const' in the distr column)
   - in addition any values from the vector of new.parameter.values were used to modify the XML (note that the names of these parameters are looked in the 'unique.name' column of input.csv)

   Starting with version 3.2.1:
   - values from new.parameter.values (if supplied) are used to replace the values in input.csv
   - all values of parameters in input.csv are used to modify the XML

   There will be no difference if all non constant values of input.csv are supplied in new.parameter.values.
   The simplest way to check any difference is to use the option to write the in memory XML representation to a file using the write.xml argument of the run() function.

Jan  12, 2022. Model version 3.2.3.a

1. Added files for generating parameter sets of full fits for ZAF
   
   The R script run-optimizer-area1-seeds.R is an example of creating parameter sets from seeds
   using parameters from ./countries-examples/ZAF/parameters/Area1
   and seeds from ./countries-examples/ZAF/parameters/Area1/ZAF_input_seeds_area1.csv

   The R script pairsplot-of-hits_area1_B.R creates a pairs plot using
   ./countries-examples/ZAF/logs-area1-seeds/ZAF_2021-12-20_15h45m57s_3_2_3_XMLinput_2910_m.txt [only used for the header i.e. parameter names]
   ./countries-examples/ZAF/logs-area1-seeds/all_36_36_hits.txt [this file was generated from many individual log files (?) by appending those after filtering lines with 36 of 36 hits]
   ./countries-examples/ZAF/parameters/Area1/input_0309.csv [used for prior ranges]

   A Unix command to check the number of hits in a set of .log files:

   grep -e '1[[:space:]]36[[:space:]]36' *.log | wc -l

   i.e. a regular expression to filter the lines the 36 hits out of 36 targets in all .log file (in the current folder) which is then sent to wc (word count) -l (count lines)

   Similar to generate a single output file from these lines:

   grep -e '1[[:space:]]36[[:space:]]36' *.log > all_36_36_hits.txt

Dec  21, 2021. Model version 3.2.3

1. Fixed a bug in demography in TBVx-data-reading-fncs-v13.R in function get.demography() [line 92]:
   the argument avg should be F in this function call:
     y = aggregate.by.age.groups(x,p$AGES,sumcols=F,avg=F)

   The bug caused the 80+ age groups to be misread from UNPOP data, and therefore caused the pop size
   to be adjusted to an erroneous target.

   After fixing this bug the fitted population size is less than 0.5% different from UNPOP data.

2. Modified the log format of the optimizer to enable reading the log files into data frames.

3. Improved verifying parameters in TBVx-parsing-xml-funcs-v13.R
   Adding further checks is work in progress.


Nov  26, 2021. Model version 3.2.2

1. Corrected output on [dPOPadj].txt to reflect the correct year (e.g. 2000 not 2000.5) 
   Note that the [POP].txt is from half way the year (e.g. 2000.5)

2. Added documentation on econ output to GitHub wiki

Nov  24, 2021. Model version 3.2.1

See ./run-RWA-neg.R and ./countries-examples/RWA/ for illustrative examples
Note that WHO deathrates were tweaked for testing purposes.

1. Improved error handling and more specific messages to model log

2. Fixed issue that caused negative health impact of Vx intervention for RWA

   This issue was due to a combination of causes:

   A. The comparison between intervention run and baseline run wasn't a fair comparison 
      This was fixed by moving the population adjustment (to UNPOP data) to an event that 
      takes place once a year. 
      The yearly event (at the start of the year) takes the following actions (in that order):
      - ages the population by 1 year
      - updates the population age composition to either
        = match UNPOP data (BASELINE)
        = reproduce changes which were recorded during the baseline (INTERVENTION)
      - handles incidence (HIV, TB, VXa) if incidence was defined to occur once a year
        = this means that vaccination can be defined to occur once per year and involves moving 
          (part of) specific age groups from one state (never) to another (vac)
      - rebalances the contacts matrix for the new population age composition

   B. When vaccination 'incidence' was defined as a rate for 0 year olds from e.g. 2028. it appeared that
      from 2028 (i.e. at 2028.5) vaccinated 1 year olds showed up in the output.
      - as the yearly event (e.g. at 2028.0) takes place _after_ the output (i.e. the values of 
        the state variables) is generated for 2028.0 the projected value of vaccinated 0 year olds 
        at 2028.0 was larger than 0 [even though the vaccination rate before 2028.0 had been 0, 
         numerical integration algorithms use the derivative i.e. rate at 2028.0 to estimate the 2028.0 value]
      - this was fixed by adding a small value (1e-4) to the start time of rate changes (e.g. vaccination starts at 2028.0 + 1e-4)

3. Additional output on vaccinations

   When vaccination is defined to occur once per year, the 'flows' output does not contain output on
   numbers of vaccinations as age groups are being moved over at a single instance in time.
   A new output type was coded (in the same format as the flows output) for intervention runs,
   both in the output data structure and as file: e.g. 
   output$vaccinated and *[dVx1pyr].txt
   The subject in this output starts with VXaXi_1 for the first VXa incidence file, VXaXi_2 for the 2nd etc
   
Nov  11, 2021. Model version 3.1.4

1. Improved error handling in run() function:
- when no path to input.csv is provided while a named vector of parameter values is supplied (new.parameter.values) the model will stop with an error message 
- modified the error message that is written to the model log due to negative state variables (parameter values will only be written to the WEIRD folder when an input.csv was provided)

Nov  8, 2021. Model version 3.1.3

1. Fixed error that prevented the model to run with model = new.env() instead of model = globalenv() 

Nov  5, 2021. Model version 3.1.2

1. Improved handling of exceptional conditions

When state variables become less negative then a configured value (e.g. -1e-2) the simulation will:
- write a message to the error log
- write a .csv with parameter values to output/weird/
- return a list from the run() function with elements flows=NULL, stocks=NULL 
  and a hits data frame with -1s in the model column (and Fs in hit, 1e10s in residuals and 1e10s in weighted.residuals)
- no files will be written

Oct 25, 2021. Model version 3.1.1

1. Updated incidence file format to support multiple 'from' and 'to' states:

[infant_scaleup_med_D_exp_A.txt in countries-examples/IND/data]

YEAR	country	dim	from			to			VXa	RISK	SES	HIV	TB	0	1	2
2032	IND	VXa	AnBn, AnBv, AnBu	AvBn, AvBv, AvBu	NA	NA	NA	NA	Un	0	0	0
2036	IND	VXa	AnBn, AnBv, AnBu	AvBn, AvBv, AvBu	NA	NA	NA	NA	Un	0.6	0	0

In the example above: 
- from the year 2036, 60% of those in TB state Un and VXa state AnBn (unvaccinated) are moved to AvBn (vaccinated with vaccine A), 
  and analogously, 60% of those in Un and AnBv are moved to AvBv, 60% of those in Un and AnBu are moved to AvBu

If the XML input file specifies:

    <VXa.incidence>
      <incidence.data file="data/infant_scaleup_med_D_exp_A.txt" times="2020,2040" values="1,1" proportions="false" denominator="susc" once.per.year="true"/>
      <incidence.data file="data/infant_scaleup_med_D_exp_B.txt" times="2020,2040" values="1,1" proportions="false" denominator="susc" once.per.year="true"/>
    </VXa.incidence>

The file [infant_scaleup_med_D_exp_B.txt in countries-examples/IND/data] would also be applied, with contents:

YEAR	country	dim	from	to	VXa	RISK	SES	HIV	TB	0	1	2
2032	IND	VXa	AnBn, AvBn, AuBn	AnBv, AvBv, AuBv	NA	NA	NA	NA	Un	0	0	0
2036	IND	VXa	AnBn, AvBn, AuBn	AnBv, AvBv, AuBv	NA	NA	NA	NA	Un	0.4	0	0

This incidence file specifies vaccination with vaccine B (i.e. moving from AnBn to AnBv, AvBn to AvBv and AuBn to AuBv).

NOTE: it is of vital importance that the fractions (0.6 and 0.4 in the above examples) should not exceed 1.0


Oct 18, 2021. Model version 3.1.0

0. NOTE: please update TB-Vx-schema-S.xsd in all applicable locations !!!

1. Modified the format of the incidence files to include dependencies of incidence in the 'dim' dimension to depend on the state in other dimensions.

For example (see tbvax/countries-examples/IND/data/infant_scaleup_med_C.txt ; age groups from 5 omitted):

YEAR	country	dim	from	to	VXa	RISK	SES	HIV	TB	0	1	2	3	4	
2032	IND	VXa	never	vac	NA	NA	high	NA	Un	0	0	0	0	0	
2033	IND	VXa	never	vac	NA	NA	high	NA	Un	0.545	0	0	0	0	
2034	IND	VXa	never	vac	NA	NA	high	NA	Un	0.755	0	0	0	0	
2035	IND	VXa	never	vac	NA	NA	high	NA	Un	0.994	0	0	0	0	
2036	IND	VXa	never	vac	NA	NA	high	NA	Un	1	0	0	0	0	

The modification is backwards compatible which means that the previous format (see tbvax/countries-examples/IND/data/infant_scaleup_med_B.txt) is also supported :

YEAR	country	dim	from	to	TB	0	1	2	3	4	
2032	IND	VXa	never	vac	Un	0	0	0	0	0	
2033	IND	VXa	never	vac	Un	0.545	0	0	0	0	
2034	IND	VXa	never	vac	Un	0.755	0	0	0	0	
2035	IND	VXa	never	vac	Un	0.994	0	0	0	0	
2036	IND	VXa	never	vac	Un	1	0	0	0	0	
2037	IND	VXa	never	vac	Un	1	0	0	0	0	
2038	IND	VXa	never	vac	Un	1	0	0	0	0	

NOTE: the values in each of the dim, from, to, VXa, RISK, SES, HIV, and TB columns should be the same (for now) and is enforced.

2. Added an option to move a fraction of an age group once a year.

The meaning of the numbers in the age columns of these files depend on the specification in the XML:

<incidence.data file="data/infant_scaleup_med_C.txt" times="2028,2029" values="1,1" proportions="false" denominator="susc" once.per.year="true"/>

If the new option 'once.per.year' equals 'true', the numbers in the age columns indicate the fraction of this age group to be moved to the 'to' state at the start of the year.
The maximum value is 1 (of course), which is enforced.

3. Handling of yearly events and output

At the start of each year an 'event' occurs (e.g. at 2000.0):
a. aging: i.e. the contents of 1-yr age groups are moved to the next higher age group (or for 5 year age groups 1/5 of the contents is aged)
b. adding of newborns to (the now empty if 1 yr age groups) age group 0
c. contact matrices are balanced (i.e. adjusted to the new population composition to ensure contacts between age group A and age group B are the same as vice versa)
d. contents of age groups are moved from one state to another according to the incidence files and only if the option 'once.per.year' equals 'true'

NOTE: unfortunately, output is generated just BEFORE the yearly event takes place....
This means that if output is generated e.g. for the year 2000.0 the ouput reported will contain the contents of age group 0 just before aging i.e. in fact age group 0 of 1999 after 1 year of aging.
Also, incidence reported for 2000.0 will be based on this incorrect number.      
Note that the implementation of the event is correct: age the 0 year olds, add newborns, and apply incidence (e.g. vaccinate the newborns).
As long as output is generated halfway the year, there is no problem.
The cause of generating output values of state variables first and then applying an event is due to the implemenetation of the rk numerical integration method in deSolve.
Imho it is a bug (and it can be illustrated using a very simple model ; I will contact the authors of the deSolve package about this).
A workaround is possible though not straightforward.



Aug ##, 2021. Model version 3.0.9

1. signature of run.optimization:
   
   run.optimization(model=NULL, lmcontrol, nruns=1, pwr=NULL)
   model:     a model with initialized parameters
   lmcontrol: a reference to a levmarq.csv file with columns parameter and value, e.g.:
                parameter	value
		ftol	1.00E-02
		diag	22
		epsfcn	2
		maxiter	100
   nruns:     number of optimization runs (not yet working correctly)         
   pwr:       NULL, 0, <0 or >0 
              if NULL: the fit.model function returns the weighted residuals i.e. (model value - target value ) / err
              if    0: the fit.model function returns the weighted residuals if abs((model value - target value ) / err) > 1 [i.e. the model value is outside the range] else 0
              if   <0: the fit.model function returns the weighted residuals plus a penalty of -pwr^2 (i.e. 49 if pwr=-7) for each model value that is out of range
              if   >0: the fit.model function returns sign(weighted residuals)*abs(weighted residuals)^pwr


July 27, 2021. Model version 3.0.8

0. NOTE: the XML Schema file has been updated (though not renamed): TB-Vx-schema-S.xsd
         Copy this file to your countries folder !!


1. Removed output files with ALL deaths as these were confusing.
   
   In both baseline and intervention, all deaths are simply equals to background deaths (dBGx) + HIVTBdeaths (dHIVTBdeaths) ; both in numbers/year.
   Note that for intervention scenarios with reduced deaths (e.g. TB Vx) the population size will be slightly larger, 
   and therefore BGx will be slightly larger, and the # lives saved smaller. 


2. Modified code to ensure that the output of an intervention run is identical to the baseline until the start of the intervention.

   The start moment of the intervention can be set in the XML of the intervention scenario, either by 
   a) referring to an incidence data file, in the example below the incidence data file for VXa 
      [NOTE: the first row of that data file should only contain 0's]:

  <TB.Vx.model.inputfile xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:noNamespaceSchemaLocation="../../TB-Vx-schema-S.xsd">
  <simulation from.year="1900" thru.year="2051">
    <options>
      <numerical.integration method="ode23"/>
      <exceptional.conditions min.value.for.state.var="-1e-2"/>
      <intervention.start from.incidence.data="VXa"/>
    </options>

    or by 
    b) setting the intervention start moment:

  <TB.Vx.model.inputfile xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:noNamespaceSchemaLocation="../../TB-Vx-schema-S.xsd">
  <simulation from.year="1900" thru.year="2051">
    <options>
      <numerical.integration method="ode23"/>
      <exceptional.conditions min.value.for.state.var="-1e-2"/>
      <intervention.start year="2033"/>
    </options>

   See ./run-baseline-vs-intervention-IND-example.R which uses the data in ./countries-examples/IND

3. Added the 'flatmid' option to the run.optimization function e.g.
   run.optimization(model,lmcontrol="./levmarq.csv",nruns=1,flatmid=0.6)
   to set the residuals to 0 with the middle region of a target.
   flatmid=0.6 denotes the mean value of the target +/- 30% of the error

June 28, 2021. Model version 3.0.7

1. Fixed a bug in parsing the targets file. Due to this bug, no more than two states in either of the columns 
   xVXa, xSES, xRISK, xHIV, xTB and yVXa, ySES, yRISK, yHIV, yTB were taken into account when querying the output files (stocks and flows).

2. Extended the option to define targets as a combination of other targets.
   In short:
   - rows with -1 in the err column are interpreted as intermediate targets (which will not show up in the [hits] output).
   - the intermediate targets are used as the source for compound targets
   - compound targets are identified by a value other than 'stock','inflow' or 'outflow' in either the xtype or ytype column
   - compound targets refer to intermediate target by the names in the xtype, xcause, ytype and ycause columns 
   - the values of the intermediate targets referenced in these columns are assigned to variables x, a, y and b (for the xtype, xcause, ytype and ycause columns respectively)
   - column fn defines a function applied to the variables x, a, y and b that is used to calculate the value in the model column
   - see run-India-with-retreat-target-example.R for an example (which uses files in countries-examples/IND)
   - NOTE: there is an option to exclude outputting the intermediate target values with is T by default ; to include those use 
           output = run(model,write.to.file = T, exclude.intermediates=F)


3. TO DO: add an option to write the queries that are run on the stocks and flows output to a file.

June 11, 2021. Model version 3.0.6

1. The function run (in TBVx-run-v1.R) now takes an additional argument output.format which should equal 'fst' , 'parquet' or 'txt' (the default).

June 7, 2021. Model version 3.0.5

1. Added an option to the target file format to add two targets (and automatically remove the target outcomes that are added).
   To be used to add HIV deaths to TB deaths in HIV+ and fit to that total.


June 3, 2021. Model version 3.0.4

1. Added runoptimization-from-RStudio-examples.R with optimization example.
   The example will work with the files in countries-temp/IND [the files required should be negated from .gitignore]
   To be documented on the Wiki.
   


May 25, 2021. Model version 3.0.3

1. Added an argument to the run function for setting the intervention run pop adjustment using an in memory output object rather than two files.

   The signature of the run function now is:
    
   run(mdl=NULL,new.parameter.values=NULL,write.to.file=F,write.xml=NULL, combine.stocks.and.flows=F, baseline=NULL)

   See runcountry-from-RStudio-new-examples.R for an example.


May 20, 2021. Model version 3.0.2

1. Tweaked the run() function to handle correctly a 1-row data.table as an argument for new.parameter.values

May 20, 2021. Model version 3.0.1

1. Re-enabled combined stocks and flows output

   The signature of the run function now is:
    
   run(mdl=NULL,new.parameter.values=NULL,write.to.file=F,write.xml=NULL, combine.stocks.and.flows=F)

   Note that the combined output [onerun] is written to file, though not in the output object returned from the run function.

May 20, 2021. Model version 3.0

1. Modified 'TBVx-run-v1.R' to not use %>% anymore as this may cause confusing error messages  
2. Add countries-temp/IND/* to have working examples
3. Modified 'runcountry-from-RStudio-new-examples.R'

May 20, 2021. Model version 2.9.9

1. Complete rewrite of simple functions to run the model:

The two main functions are: 
set.paths()
run()

See 'runcountry-from-RStudio-new-examples.R' for examples and 'TB Vx documentation of principal R functions v1.pdf' for documentation.

The functions in 'TBVx-hi-level-fncs-v1.R' will be documented shortly, and are meant to be used as an API to the model.
'TBVx-run-v1.R' contains the run() function.

2. An important difference compared to previous versions is that the model (both data and functions) is now in an R environment 
that is separate from the global R environment. Note that for debugging purposes it may be useful to set the model environment
temporarily to the global R environment.
In principle it would be possible to run multiple different models (each in its own environment) at the same time.




April 27, 2021. Model version 2.0.8

1. Added the VXa.incidence.data argument to the run.one() function:

  function(countries.dir=NULL, 
                   countrycode=NULL, 
                   write.to.file=T, 
                   baseline=NULL, 
                   xml="XMLinput.xml",
                   targets="target.csv",
                   parameters="input.csv",
                   update.constant.parameters=F,
                   new.parameter.values=NULL,
                   VXa.incidence.data=NA)

  Example use: 
  result = run.one(countries.dir, countrycode, write.to.file=T, xml="XMLinput.xml", VXa.incidence.data = c("data/VXA-incidence_LL_1.txt","data/VXA-incidence_LL_2.txt"))
 
  In other words: VXa.incidence.data is a vector of filenames (including the path from the country folder) each containing the VXa incidence data.
  The filenames specified will overwrite the filenames in:
    <VXa.incidence>
      <incidence.data file="data/VXA-incidence_LL_1_fix.txt" times="2030,2031" values="1,1" proportions="false" denominator="susc"/>
      <incidence.data file="data/VXA-incidence_LL_2_fix.txt" times="2030,2031" values="1,1" proportions="false" denominator="susc"/>
    </VXa.incidence>
  in the order as specified above.
  Other arguments will not be replaced so please use with care!


April 21, 2021. Model version 2.0.7

1. Fixed bug in run.one (reported by Andy on 14 Apr 2021)
2. Fixed bug in rescaling population (reported by Rebecca on 16? Apr 2021)
3. Added function to set up model log file.

April 13, 2021. Model version 2.0.6

1. Refactored a lot of code to produce one simple function to run the model. 
   See 'TB Vx documentation of run.one function.pdf'

2. Fixed bug introduced in version 2.0.0 which caused the contacts matrix not being updated for changing 
   population age composition

NOTES
   
* Not yet tested yet for interventions (i.e. a non NULL baseline option)
* Not optimized for performance (i.e. not rereading files when only some parameter values have changed;
  the performance enhancement will be minor, probably at most 0.10 to 0.15 secs)
* The code has undergone major changes - please use with care and report any issues asap
 


April 7, 2021. Model version 2.0.5

1. During intervention runs, the population was not scaled down in 1950 (the start of the UNPOP data on demographics).
   
   A new setting in the XML input file now sets the year for rescaling the alive population:

  <demography country.code="ignored" rescale.population="1950">

  The 'rescale.population' attribute is a required attribute and it is wise to set that to 1950.

  Two additional errors were fixed as well: 
  - params$intervention is now parsed and set correctly
  - the population adjustment during interventions runs is no longer corrected for HIV and TB deaths (as it should not)


March 31, 2021. Model version 2.0.4

1. Fixed missing [dALLx] (intervention runs).

March 30, 2021. Model version 2.0.3

1. A new format is supported for specifying an incidence multiplier.

   So far, both times and values had to be comma separated lists of value (optionally including a named HIV parameter), for instance:

   <incidence.data file="data/HIV-incidence.txt" times="1900,2051" values="lambdaH,lambdaH" proportions="false" denominator="susc"/>
 
   Since version 2.0.3 the following format is also supported, i.e. a range for times and an expression for values referring to times using x (x is the parsed value of times) 

  <incidence.data file="data/ART-incidence.txt" times="1999:2019" values="alphaH/(1+exp(-0.5*(x-2009.5)))" proportions="true" denominator="susc"/>

  Note that the new format is also supported for proportions = "false" which would prevent the issues now handled by the imposed limits on the multiplier (see below).


2. Imposed limits on the multiplier used with incidence files with type="proportional" and this could lead to attempted forced incidence in age groups with near 0 individuals. There is now a maximum of 50 for the age multiplier. Average run time with 5 year age groups is 4.5 secs.

3. The XML Schema was updated though the filename is the same (TB-Vx-schema-S.xsd)


March 29, 2021. Model version 2.0.2

1. Modified the calculation of the removal / addition rate from the demographics.csv data file for 5 year age groups: now taking into account the size of the individual 1-year age groups in the original data (i.e. use a weighted average for a 5 year age group).

2. NOTE: there is no change required in input.csv, target.csv or XMLinput.xml ( the example input file XMLinput_chris_updated_to_2_0_2.xml is 100% identical to XMLinput_chris_updated_to_2_0_1.xml ).		

March 22, 2021. Model version 2.0.1

1. Added the option to use the following age group definition:

  <ages lower.limits="c(0,1,5*(1:15))"/>

2. Support for including the 0 age group in outputs:

  <detailed.output years="c(1900:1925, 1980:2050)+0.5" age.group.lower.limits="0,1,5,15" econ.output="false" suppress.zeros.in.stocks="false" suppress.zeros.in.flows="false" combine.stocks.and.flows="false">

3. Support for concentrating HIV incidence in age group 0 (automatically enabled for HIV incidence), therefore 

   YEAR    country dim from    to  TB  0   5   10  15  20  25  30  35  40  45  50  55  60  65  70  75  80
   1900    ARM HIV HIV-    HIV1    NA  0.000197762 0   0   0.000273538 0.000603679 0.000590258 0.000566751 0.000416461 0.000259327 0.000142816 0.000117679 0.000132227 0.000144913 9.41E-05    0.000106209 5.75E-05    0
   2051    ARM HIV HIV-    HIV1    NA  0.000197762 0   0   0.000273538 0.000603679 0.000590258 0.000566751 0.000416461 0.000259327 0.000142816 0.000117679 0.000132227 0.000144913 9.41E-05    0.000106209 5.75E-05    0

   will lead to 0 HIV incidence in 1 thru 4 year olds and 5 x 0.00197762 = 0.000988811 in 0 year olds.

   There is no need to disable the option as the previous format will also work correctly:
   YEAR    country dim from    to  TB  0   1   5   10  15  20  25  30  35  40  45  50  55  60  65  70  75  80
   1900    ARM HIV HIV-    HIV1    NA  0.000988811 0   0   0   0.000273538 0.000603679 0.000590258 0.000566751 0.000416461 0.000259327 0.000142816 0.000117679 0.000132227 0.000144913 9.41488e-05 0.000106209 5.74815e-05 0
   2051    ARM HIV HIV-    HIV1    NA  0.000988811 0   0   0   0.000273538 0.000603679 0.000590258 0.000566751 0.000416461 0.000259327 0.000142816 0.000117679 0.000132227 0.000144913 9.41488e-05 0.000106209 5.74815e-05 0

March 15, 2021. Model version 2.0.0

0. See RB/countries/ZAF/XMLinput_chris_updated.xml for a working example input file

1. New output types (in addition to stocks and flows):

[POP].txt       : population size by age and year
[dALLx].txt     : deaths due to all causes by age and year (as number / year)
[dTBHIVx].txt   : deaths due to HIV and TB age and year (as number / year)
[dBGx].txt      : background deaths by age and year (as number / year)
[dfrBGx].txt    : the fraction of background deaths by age and year (as a fraction of a population age group / year)
[dfrPOPadj].txt : the fraction of a population age group used to fit to UNPOP data (as a fraction of a population age group / year)

The files above are generated for a baseline run.

2. New output options

a. Command line option 'baseline':

   either passed as argument to the paths function in runcountry.R:

    countrycode      = "ZAF"
    paths            = set.paths(paths, mydir="RB", 
                                 countries.dir = "countries",
                                 countrycode = countrycode, 
                                 targets="target.csv", 
                                 # xml = "XMLinput_chris_02032021_RB_edits.xml",
                                 baseline = "XMLinput_chris_02032021_RB_edits",
                                 xml = "XMLinput_chris_02032021_RB_edits_intv.xml", 
                                 parameters="input.csv")

   or as a command line argument using the baseline option with -b

   NOTE: baseline (e.g."XMLinput_chris_02032021_RB_edits") should refer to a unique (!) set of output files that contain the value of baseline in the file name.

   With the baseline option provided, the model will do an intervention run, i.e. 
   read the [dfrPOPadj].txt file and apply the saved fractional adjustments to adjust the simulated population over time,
   and read the [dfrBGx].txt file to calculate background deaths.

   The output of an intervention run will NOT contain [dfrBGx.txt] and [dfrPOPadj.txt] as these files were used as input.

   NOTE: background deaths are based on deaths all causes (UNPOP data in the data/[3 letter country code]_deathrates.csv) from UNPOP.

b. Options to restrict output:

   In the XML input file:

   <output>
    <detailed.output years="1970+10*(0:6)+0.5" age.group.lower.limits="0,30" econ.output="false" suppress.zeros.in.stocks="false" suppress.zeros.in.flows="false" combine.stocks.and.flows="false">
      <flows dim="TB"  incidence="true" transmission="true" progression="true" treatment="true"/>
      <flows dim="HIV" incidence="true" transmission="false" progression="true" treatment="true"/>
      <flows dim="VXa" incidence="true" transmission="false" progression="false" treatment="false"/>
    </detailed.output>
   </output>

   New attributes of <detailed.output>:
   econ.output              : to enable econ output (i.e. the file types mentioned under 1)
   suppress.zeros.in.stocks : does what the name suggests
   suppress.zeros.in.flows  : does what the name suggests
   combine.stocks.and.flows : option to generate previous output type (flows and stocks combined into [onerun]

   Specific outputs on flows by dim and type (see above, only generated if applicable)


3. Proportional mixing 

   Proportional mixing is enabled by omitting the reference to the contact matrix:
   Simple comment out this reference, for example:
   <!--<contact.matrix file="data/all_contacts_2020.txt"/>-->

4. Updated XML Schema 
   
   Please refer to XML Schema version 'S':

   xsi:noNamespaceSchemaLocation="../../TB-Vx-schema-S.xsd">

5. Reference to UNPOP mortality data

   Use

   <demography country.code="ignored"><!-- ignored really means from command line-->
      <from.data>
        <population file="data/demographics.csv"/>
        <birthrate from.population.data="true"/>
        <mortality file="data/deathrates.csv"/>
      </from.data>
   </demography>
 
   i.e. it is now obligatory to refer to a UNPOP mortality data file (with the ISO3 country code prepended e.g. ZAF_mortality.csv)
 
6. Always include the 'denominator' attribute (and preferably also the 'proportions' attribute which is "false" by default) when specifying incidence.data:
   
    <HIV.incidence>
      <incidence.data file="data/HIV-incidence.txt" times="1900,2051" values="lambdaH,lambdaH" proportions="false" denominator="susc"/>
      <incidence.data file="data/ART-incidence.txt" times="1900,2051" values="alphaH,alphaH" proportions="false" denominator="susc"/>
    </HIV.incidence>

    and be careful with the alternative denominator option ("all") as this may cause the autostepper to take very small time steps.   

7. Minor bug fixes in code which occurred with just one instead of two+ death stages.

8. UNPOP mortality data
   
   A zipfile with UNPOP mortality data can be found in the current branch TB/countries/LMIC_deathrates.zip


March 4, 2021

Model version 1.9.9

1. Added option to generate output either as separate files for stocks and flows or as one combined file.
   See runcountry.R which calls
     run.model(params, write.to.file=T, stocks.and.flows=!opts$output.combined,path=paths$country.output)
   with the new argument stocks.and.flows

NOTES:

A. The first line of the XMLinput.xml should be modified to:
   <TB.Vx.model.inputfile xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:noNamespaceSchemaLocation="../../TB-Vx-schema-Q.xsd"> 

B. Due to the new option for incidence files, the denominator should be specified, for example:
  
    <HIV.incidence>
      <incidence.data file="data/HIV-incidence.txt" times="1900,2051" values="lambdaH,lambdaH" denominator="all"/>
      <incidence.data file="data/ART-incidence.txt" times="1900,2051" values="alphaH,alphaH" denominator="susc"/>
    </HIV.incidence>



Model version 1.9.8

1. Fixed processing the path to the XML Schema ; now also works on OS X

Model version 1.9.7

See RB/countries/ZAF/parameters/XMLinput-RBedits23FEB21D.xml for a working example.


Mar 1, 2021

1. Bug fix 
   
   A bug was fixed that caused some deaths (e.g. due to treatment matrices) not to be used in adjusting the population to UNPOP demography


2. XML Schema
  
   The type of values in an element defining a time course such as the multiplier in the XML fragment below was reverted back from 'numberseq' (see XML Schema) to xs:string.
   Therefore, the fragment below now works correctly.
   <treatment.matrix name="init"> 
       <transition from="Dc" to="T" rate="eta*j4/(1-pE)" /> 
       <multiplier name="etamul" times="1950,2000,2019" values="0,etamul2000,1"/>
   </treatment.matrix>
 

3. Format of incidence files

   Incidence files may 
   - either contain incidence by year and age group:
    YEAR	country	dim	from	to	TB	0	1	5	10	15	20	25	30	35	40	45	50	55	60	65	70	75	80
    1900	ZAF	HIV	HIV-	HIV1	NA	0.000988811	0	0	0	0.000273538	0.000603679	0.000590258	0.000566751	0.000416461	0.000259327	0.000142816	0.000117679	0.000132227	0.000144913	9.41488e-05	0.000106209	5.74815e-05	0
    2051	ZAF	HIV	HIV-	HIV1	NA	0.000988811	0	0	0	0.000273538	0.000603679	0.000590258	0.000566751	0.000416461	0.000259327	0.000142816	0.000117679	0.000132227	0.000144913	9.41488e-05	0.000106209	5.74815e-05	0
    multiplied by a dimensionless multiplier:
    <HIV.incidence>
      <incidence.data file="data/HIV-incidence.txt" times="1900,2020" values="multH,multH" proportions="false" denominator="susc"/>
      <incidence.data file="data/ART-incidence.txt" times="1900,2020" values="multA,multA" proportions="false" denominator="susc"/>   
    </HIV.incidence>

   - or contain proportions by year and age group:
   YEAR	country	dim	from	to	TB	0	5	10	15	20	25	30	35	40	45	50	55	60	65	70	75	80
   1900	ZAF	HIV	HIV-	HIV1	NA	0.06923	0	0	0.08846	0.1859	0.1859	0.1705	0.11026	0.06538	0.0346	0.0282	0.02436	0.02179	0.01154	0.00388	0	0
   2051	ZAF	HIV	HIV-	HIV1	NA	0.06923	0	0	0.08846	0.1859	0.1859	0.1705	0.11026	0.06538	0.0346	0.0282	0.02436	0.02179	0.01154	0.00388	0	0
   multiplied by the incidence over all age groups (lambdaH or alphaH in the example below):
    <HIV.incidence>
      <incidence.data file="data/HIV-incidence-RB.txt" times="1900,2020" values="lambdaH,lambdaH" proportions="true" denominator="susc"/>
      <incidence.data file="data/ART-incidence-RB.txt" times="1900,2020" values="alphaH,alphaH" proportions="true" denominator="susc"/>   
    </HIV.incidence>

   In both cases, the denominator may either be "susc" or "all".
   Note that specifying "all" combined with a small fraction of susceptibles (e.g. 0.2) leads to a multiplication factor of e.g. 5 for the incidence rate which may increase instability of the simulation.
   The maximum multiplication factor equals 10.

   The susceptible state is derived from the "from" column of the incidence files.

   In addition to HIV.incidence it is possible to specify VXa.incidence using the same format.


4. Output

   Output is now split into [stocks].txt (i.e. prevalence) and [flows].txt (either 'in', 'out' or 'net' for demography adjustments)


5. Output

   Four new types of output have been added. 

   Files ending in:
   [population].txt contains population size over time (midyear)
   [HIVTB_X].txt    contains the sum of HIV and TB deaths (as a number)
   [frdTBHIV_X].txt contains fractions of HIV and TB deaths per year ; if the relevant numbers of this file are multiplied with those of [population].txt 
                    the numbers of [HIVTB_X].txt are approximated. Note that [HIVTB_X].txt contains accumulated deaths over the first half year, whereas 
                    [frdTBHIV_X].txt contains the derivative at midyear.
   [frdpopadj].txt  contains the fraction per year of each of the age groups that has been applied to fit the UNPOP population data ; 
                    it is the rate that is needed to compensate the difference between the actual simulated population size (after subtracting the rate 
                    due to TB and HIV deaths) and the UNPOP data.
  

6. Exiting a simulation run due to an exorbitant number of time steps between two output time points (usually 0.5 simulated year)

  The existing attribute maxsteps of the <numerical.integration> element can be used to prevent too many time steps betweem two output time points. 
  <simulation from.year="1900" thru.year="2051">
    <options>
      <numerical.integration method="ode23" rtol="1e-3" atol="1e-2" maxsteps="40"/>
      <exceptional.conditions min.value.for.state.var="-5e-2"/>
    </options>
  This will only work in combination with options(warn=2) in R (e.g. in the runcountry.R script)

7. Bug fix in referencing the XML Schema from XML input files

   The current XML Schema file is TB-Vx-schema-Q.xsd

   The schema file is now correctly referenced from the XML input file.
   For instance:
   <TB.Vx.model.inputfile xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:noNamespaceSchemaLocation="../../TB-Vx-schema-Q.xsd">

   If the XMLinput.xml is e.g. in countries/ZAF/parameters, the file TB-Vx-schema-Q.xsd needs to be in the countries folder.



Model version 1.9.5

Feb 3, 2021

1. Changed multiplier for seeded.infections to comply with processing parameters.csv and updated the XML Schema

See parameters.csv for an example to modify the seeded.infections multiplier

Model version 1.9.4

Feb 3, 2021

1. Removed XML run file and schema

The contents of the XML run file was moved to the regular XML input file

2. Updated the XML Schema (current version TB-Vx-schema-K.xsd)

The XML Schema was updated and therefore all XML input files need to be updated.
An example is in the current branch of tbvax in XMLinput.xml

The model code checks that the correct version of the XML Schema is referred to from the XML input file.

3. Updated runcountry.R

This example uses countries/ZAF/parameters/XMLinput.xml (which refers to TB-Vx-schema.xsd)
and 
countries/ZAF/HIV-incidence.txt
countries/ZAF/ART-incidence.txt
countries/ZAF/VXa-incidence_LL_1_fix.txt
countries/ZAF/VXa-incidence_LL_2_fix.txt
countries/ZAF/ZAF_all_contacts_2020.txt

The XMLinput.xml illustrates the new options detailed below.

4. Seeded TB infections

Definition of seeded TB infections is now in the XML input file.
The initial fraction of people in various TB stages is specified except for the first (Un) TB stage.
A multiplier is specified that multiplies these fractions (at the expense of the fraction in the Un stage).

5. Incidence

Incidence files are now specified in the new XML HIV.incidence, VXa.incidence, or TB.incidence elements, for example:

    <HIV.incidence>
      <incidence.data file = "data/HIV-incidence.txt" times="1900,2020" values="0.8*incimul,1.2*incimul" />
      <incidence.data file = "data/ART-incidence.txt" times="1900,2020" values="0.1,0.2" />
    </HIV.incidence>

The times and values attributes specify a multiplier that may include a parameter to be fitted and specified as a HIV.parameter under HIV.progression.

VXa and TB incidence work in a similar way.


Model version 1.9.3

Jan 27, 2021

1. Modified the XML Schema (TB-Vx-schema.xsd) to allow fractional numbers in a time series e.g.

      <treatment.matrix name="HIVinci">
        <transition from="HIV-" to="HIV1" rate="lambdaH"/>
        <multiplier name="mult" times="1984,2009.9999,2010,2010.9999,2011" values="0,0,1.0,1.0,0"/>
      </treatment.matrix>

2. Fixed a bug in postprocessing the output data frame which caused incorrect calculation of targets (only for type 'stock').
   The file TBVx-output-query-fncs-v5.R contains the code for calculating the targets (this is a new version ; previous version was TBVx-output-query-fncs-v4.R)

3. Added a script (convert_2020_contact_matrices.R) to convert the new 2020 synthetic contact matrices (draft paper by Keisha Prem https://www.medrxiv.org/content/10.1101/2020.07.22.20159772v2) to the usual format.
   
   To use these new contact matrices change (in the run.xml file):

   </demography>
     <contact.matrix file = "/data/all_contacts.txt"/>
   to:
   </demography>
     <contact.matrix file = "/data/all_contacts_2020.txt"/>

   Note that the iso3c code is added by the model code (e.g. ALB_all_contacts_2020.txt for ALB).

   The new contact matrices are in countries/contact_matrices/ and in countries/contact_matrices/all_contacts_2020.zip


Dec 15, 2020

Model version 1.9.2

1. Fixed bug in demography that was introduced in 1.9.1

Version 1.9.1 intended to rescale the population in 1950 back to original 1950 values _including the age distribution_
Unfortunately, a stupid and simple error was made in the code for rescaling which resulted in an age composition in 1950 that was not at all like the age distrubution in the demographics.csv data file.

This was corrected in version 1.9.2
In addition to properly rescaling the population, the demography adjustments that were made during the year (based on the demography data file) were implemented properly.
There are still differences between model and data which accumulate over the years to about 5% in 2050. However, the mean differences are small.

In a future version an even better match can be achieved by compensating differences that accumulate over time.

This model version was tested using data for ALB and LBN.
In the folder for these countries is a targetpop.csv that defines demography targets derived from the demography data file,
and adapted XML inputfile without treatment, increased transmission prob, and increased death rate (to make sure compensating TB deaths works).

Dec 9, 2020

Model version 1.9.1

1. Improved demographics

The only difference between 1.9.1 and 1.9 is in rescaling the population in 1950 (back to 1950 UNpop data values).
In 1.9 this was done by scaling the entire population back to the reported total for 1950 without taking into account age distribution (which then would have been the 2000 age distribution).
From version 1.9.1 the population is scaled to 1950 total and age distribution.
We expect demography to fit perfectly now.
Note that the assumption remains that any changes in demography due to migration assume that the distribution over TB (and SES etc) stages of immigrants and emigrants is not different from
the resident population.

Dec 8, 2020

Model version 1.9.0

1. Extensions for TB Vx: Vx incidence

Implementation of TB Vx is by means of incidence files.
These incidence files now contain an extra column TB to restrict application to specific TB stages.
The values in all rows for the columns 'country', 'from', 'to' and 'TB' should be identical (this is required for setting up transition matrices in a straightforward way).
To implement differential effects of administering a TB vaccine, multiple incidence files can be specified (point to these files in the run.xml file).

One Vx incidence file can be used to move people in the Un, Uc, Lf, Ls, and R TB states from 'never' to 'vac' whereas a second Vx incidence file can be used to 
move people in Ds from 'never' to 'prev'.

2. Extensions for TB Vx: TB stage dependent VXa transition rates

Parameters determining Vx progression rate can now depend on TB stage.
A TB transition to the Ds stage could trigger the transition from Vaccinated-Protected to Vaccinated-Unprotected, simply by making the transition rate VP -> VUP nonzero in the Ds stage.

NOTE: 
for those Vx progression parameters that are dependent on TB stage, parameter values for ALL TB stages should be defined (that may be a nuisance for now, but safer in the code, and designed to prevent accidents).
there is no clear error message (yet) for this condition

3. The XML Schema was updated to TB-Vx-schema-G.xsd

All XML input files should refer to the new schema (in one of the first lines)

An example of a working implementation can be found in countries/ALB




Dec 2, 2020

Model version 1.8.6

1. Demography

The cause of not fitting demography was that deathrates are derived from population.csv whereas population.csv includes migration.
The model restricted the result of applying deathrates to negative numbers (i.e. the population should decrease due to deaths).
This restriction was removed i.e. the changes applied to demography are the net changes derived from UNPOP data.


Dec 2, 2020

Model version 1.8.5

0. New version is in tbvax and in tbvax/R - ignore RB folder (which is a mess and contains old / intermediate / experimental code)

1. Paths

This version fixes the problem with paths.

runcountry.R

runcountry.R runs from the root tbvax folder and expects the following folder structure (from the root tbvax folder):
countries/[ISO3 country code]/data
countries/[ISO3 country code]/parameters
countries/[ISO3 country code]/logs
countries/[ISO3 country code]/output

Files expected:
countries/[ISO3 country code]/parameters/target.csv
countries/[ISO3 country code]/parameters/XMLinput.xml
countries/run.xml

These file names (though not the location) can be overridden with command line parameters, e.g.
-t myfavoritetargets.csv
-x theoneandonlyXML.xml
-r runsthemile.xml

It is possible to use the same folder structure in a different subfolder which can be specified with the -m command line argument.
For instance, to use the same folder structure in the subfolder RB pass -m RB as a command line argument.

For working examples, see:
run-countries-here.bat
run-countries-in-favorite-subfolder.bat

blast.R

In addition to runcountry.R there is also blast.R which runs many simulations by sampling from parameters using specified parameter values and ranges in a parameters.csv
The default name of this csv is input.csv and it is expected in the parameters subfolder.

modify-xml-with-parameters.R

This script does what the name suggests.
See run-modify.bat

Technical details

The problems with paths are solved by setting up a paths structure in the list paths, and by using the R here package.
The R source file include-v11.R contains details about the default folder structure.
All can be modified at your own risk.

2. Mysterious 'not yet implemented Matrix[<- method' bug

This is probably solved and was related to incompatibility of Matrices (with capital M from the Matrix package (not movie) which are sparse matrices) with subsetting using logical functions.
It is unclear why this did not always happen (suggesting R version or Matrix package version issue).



Nov 30, 2020

Model version 1.8.4

Fixed NaNs in derivs.deSolve.
Issue was due to 0 sized 90+ age group (like Fiji but in this case ALL 90+ states were 0) which caused 
NaNs in transmission and in the contacts matrix.

Nov 28, 2020

Model version 1.8.3

Fixed bug that caused the ODE solver to endlessly retry.
In the end the cause was Inf/-Inf/NaN values in the calculation of natural deaths from demographics data.
These Inf/-Inf/NaN values were due to 90+ population age groups with 0 individuals.
One of the example countries where this occurred was Fiji (FJI).

Nov 27, 2020

Model version 1.8.2

1. Assertions, error messages and logging

Many more checks on correctness including 
- dependencies between XML run file and XML HIV definitions
- correct specifications of years to generate output for vs simulation years
- targets file (no NAs in value, lo, hi and err columns ; lo >= value <= hi)
- demographics.csv checked for correct number of columns (i.e. at least 102, ignoring cols 103 and further)
- contacts matrix : correct header and correct dims (16x16)

Logging level set to DEBUG which will log successful completion of subsequent steps of a simulation run.


Nov 26, 2020

Model version 1.8.1

1. Demography

Fixed a bug in seeding the initial population with TB.
This would only be relevant for XML input files with > 1 HIV.stage defined.

2. Minor tweaks to the code

Removed rounding of numbers as this might cause inaccuracies for countries relatively small populations (as opposed to China or India).

3. Examples

See RB/countries/ALB for a working example of an HIV LMIC country

Note that LMIC countries should be split into two folders, one for LMICs with HIV and one for LMICs without HIV
The XML run file (shared across countries) is different for those two types of countries.

Nov 24, 2020

Model version 1.8

1. Demography

From version 1.8 demography will be initialised based on the 1950 values (including distributions over TB stages and SES stages).
At 1950 the population will be rescaled (i.e. including age distribution) to the 1950 values.

Changes to run.xml: 
  <seeded.infections>
     <TB fraction="0.482" age.from="0" age.thru="99" stage="Un"/>
     <TB fraction="0.246" age.from="0" age.thru="99" stage="Uc"/>
     <TB fraction="0.0188" age.from="0" age.thru="99" stage="Lf"/>
     <TB fraction="0.198" age.from="0" age.thru="99" stage="Ls"/>
     <TB fraction="0.00317" age.from="0" age.thru="99" stage="Ds"/>
     <TB fraction="0.00265" age.from="0" age.thru="99" stage="Dc"/>
     <TB fraction="0.0484" age.from="0" age.thru="99" stage="R"/>
  </seeded.infections>
</simulation>

I.e. seed infections should be specified as fractions rather than integers.

Change to run.xsd

The XML Schema will enforce that:

  <xs:element name="seeded.infections">
    <xs:complexType>
      <xs:sequence>
        <xs:element maxOccurs="unbounded" ref="TB"/>
      </xs:sequence>
    </xs:complexType>
  </xs:element>
  <xs:element name="TB">
    <xs:complexType>
      <xs:attribute name="age.from" use="required" type="xs:nonNegativeInteger"/>
      <xs:attribute name="age.thru" use="required" type="xs:nonNegativeInteger"/>
      <xs:attribute name="fraction" use="required" type="fraction"/>
      <xs:attribute name="stage" use="required" type="xs:string"/>
    </xs:complexType>
  </xs:element>

This means that previous versions of run.xml with seeded infections specified as integers WILL NO LONGER WORK (sorry).

2.
Naming of output files has been modified to include hash value of input XML and to include lots of square brackets to ease processing with regex's.

3.
The runcountry.R script accepts a single command line parameter either -c ALB or -c 2 
When an integer is passed, this integer is translated into a country code using ./RB/countries/LMIC_numbers.csv

4.
A couple of specific changes have been made by Rebecca for blast runs. These produce more output in subfolders of the output folder.


Nov 20, 2020

Model version 1.7.4

Folder structure and standard file names for countries:

from root: (with RB as an example for the initials of one of us; and ALB as an example ISO3 country code)

./R/[R source files]
./readme.txt [this file]
./RB/[*.bat, *.sh, *.R (scripts only which use ../R/*.R)]
./RB/countries/run.xml (i.e. one common XML run file, country code in this file is ignored - please set to ignore)
./RB/countries/ALB/parameters
./RB/countries/ALB/parameters/XMLinput.xml
./RB/countries/ALB/parameters/target.csv
./RB/countries/ALB/parameters/input.csv (not used by runcountry.R)
./RB/countries/ALB/data/demographics.csv (with the 1st column the ISO3 country code which will be checked against the -c command line argument)
./RB/countries/ALB/data/target.csv
./RB/countries/ALB/data/ALB_all_contacts.txt (i.e. ISO3 code preceding _all_contacts.txt)
./RB/countries/ALB/data/HIV-incidence.txt (with a column for ISO3 country code)

Please see
./RB/run-countries.bat
for an example

The second line of this batch file will fail as ZZZ is not an ISO3 country code found in the data files

 


Nov 17, 2020

Model version 1.7 (final - from now on just bug fixes and documentation)

1. Updated the format of the targets file

See ./RB/parameters-and-targets/RB_targets_1_7.csv for an example

In short, the header of this file is:
country	year	xVXa	xSES	xRISK	xHIV	xTB	xtype	xcause	yVXa	ySES	yRISK	yHIV	yTB	ytype	ycause	fn	age_from	age_thru	value	lo	hi	err	name	type

Don't use quotes.

The last two columns are not used in calculations.
xtype:  either 'stock', 'inflow' or 'outflow'
xcause: NA for 'stock', and either (or a combination of):
Tm:   for TB  transmission
TBp:  for TB  progression (i.e. derived from the transition matrix)
HIVp: for HIV progression (i.e. derived from the transition matrix)
TBtr_  followed by the name of a TB  treatment matrix: for treatment 
HIVtr_ followed by the name of a HIV treatment matrix: for treatment 
HIVXi: for HIV incidence

fn defines the function applied to the 
x data (i.e. data queried based on year + all columns preceded by x) and the 
y data (i.e. data queried based on year + all columns preceded by y)

2. Renamed output files.

The log file is named as follows: 
taskID_[taskID]_[blast|optim]_[model version]_[name of input file]_[datetime].log

Output files are now named as follows (where taskID is the value of an environemnt variable of the name is passed to an .R script with the -e option):

taskID_[taskID]_[runnr]_[datetime]_v[model version]_[name of input file]_[fraction targets hit * 100]_[runtype] followed by either 
.txt : overview of targets hit, SSwR etc
.xml : input file with parameter values that led to this score (i.e. the output can be reproduced by running this file with run-one.bat or runone.R
_full.txt: full output
_params.csv: parameter values that led to this score

Running 'modify-xml-with-parameters.R' or run-modify.bat with the *_params.csv produced and the original xml input file should produce an .xml file that is identical to the *xml file produced here.


Nov 10, 2020

Model version 1.6 

See the RB folder for:
bat files
R scripts
XML Schema files

See RB/parameters-and-targets 
for tested input files, parameters files, and targets files.

Currently the best fit fits 78 out of 78 targets with a sum of squared weighted residuals of 7.6 
i.e 0.1 per target or a difference of on average 0.3 times the error range (hi-lo)/2.



1. Added options for HIV / ART

A.

Added an option to define a (fittable) HIV incidence trend, base on file input for a single year (for various age groups) 
which is multiplied with an incidence multiplier to produce a time trend:

<HIV.incidence.trend name="HIVincitrend" times="1985,2000,2020" values="0.7*incimult,0.8*incimult,1.0"/>
 
B. 
Added options to define HIV 'treatment'

<treatment.matrix name="HIVdiag">
 <transition from="HIVu1" to="HIVd1" rate="psi"/>
 <transition from="HIVu2" to="HIVd2" rate="psi"/>
 <multiplier name="mult" times="2003,2010,2019" values="0,HIVdiagn1,HIVdiagn2"/>
</treatment.matrix>
<treatment.matrix name="ARTinit">
 <transition from="HIVd1" to="ARTn1" rate="alpha"/>
 <transition from="HIVd2" to="ARTn2" rate="alpha"/>
 <multiplier name="mult" times="2003,2010,2019" values="0,ARTtrendn1,ARTtrendn2"/>
</treatment.matrix>
<treatment.matrix name="ARTvsup">
 <transition from="ARTn1" to="ARTs1" rate="psi"/>
 <transition from="ARTn2" to="ARTs2" rate="psi"/>
 <multiplier name="mult" times="2003,2010,2019" values="0,virsupn1,virsupn2"/>
</treatment.matrix>

These matrices multiply transition rates with a time trend defined by the multiplier times and values which may contain sampled values.
Note that age dependency is supported automatically as the parameter psi (or alpha) above may be age dependent.

C.
See the following files for examples:
RB_086_hiv.xml
run_RB2019_hiv.xml
runone_hiv.R (when running from RStudio change if (F){ etc } to if (T){ etc })








Nov 3, 2020

Model version 1.5

1. Added wave.R and run-wave.bat
   
wave.R takes a parameters.csv extended with extra columns (on the right of the column with header 'choose') with selected parameter values 
(see parameter_values.csv for an example). wave.R runs simulations (one for each column of parameter values) based on all parameters supplied in those columns ; 
i.e. the 'choose' column or 'dist' is not used.

Summary output on model output vs targets is written to files, while a log file logs parameter values simulated.

Nov 3, 2020

Model version 1.4

1. Time dependent treatment initiation.
   
   Each treatment matrix now supports zero or more time dependent parameters 
   which may contain a fittable parameter in the defined values:
   
      <treatment.matrix name="init">
        <transition from="Dc" to="T" rate="(1-pE)*eta*j4"/>
        <multiplier name="etamul" times="1950,2000,2020" values="0,etamul2000,0.7"/>
      </treatment.matrix>

   Note that this parameter (etamul2000) should not be age group, HIV etc dependent.
   The entire treatment matrix is multiplied with the product of all multipliers at the moment in time the derivative is calculated.
    
2. Time dependent treatment success / failure

   To this end, two additional treatment matrices are defined.

      <treatment.matrix name="nofail">
        <transition from="T" to="Dc"     rate="(1-0.957)*sage/tau"/>
        <transition from="T" to="R"      rate=   "0.957 *sage/tau"/>
      </treatment.matrix>
      <treatment.matrix name="failed">
        <transition from="T" to="Dc"     rate="-(1-0.957)*sage/tau"/>
        <transition from="T" to="R"      rate=   "-0.957 *sage/tau"/>
        <transition from="T" to="TBdead" rate=           "sage/tau"/>
        <multiplier name="kappa" times="1950,2000,2020" values="0.4,0.3,0.2"/>
      </treatment.matrix>

   The first 'nofail' matrix defines the transitions (T->Dc and T->R) at 100% treatment success.
   The second 'failed' matrix defines treatment failure over time and is multiplied by the value of kappa at the time of calculating the derivative.
   The total effect of treatment success and treatment failure is simply the sum of the effect of the two matrices.
   Output will be generated separately for:
   Tr_init, Tr_nofail, and Tr_failed

3. Required changes in XML input file and in targets.csv

   XML input file : see above and RB_085_exp.xml
   targets.csv    : change subject 'Tr' to 'Tr_init' for notification targets

4. Additional options 
   
   With the definition of the transition matrix below

  <transition.matrix>
   <transition from="Lf" to="Ls" rate="omega"/>
   <transition from="Lf" to="Ds" rate="(1-pE)*theta*j1"/>
   <transition from="Ls" to="Ds" rate="(1-pE)*sigma*j2"/>
   <transition from="Lf" to="Uc" rate="phiF"/>
   <transition from="Ls" to="Uc" rate="phiS"/>
   <transition from="Ds" to="Dc" rate="zeta"/>
   <transition from="Ds" to="R" rate="chi"/>
   <transition from="Dc" to="R" rate="chi"/>
   <transition from="R" to="Ds" rate="(1-pE)*rho*j3"/>
   <transition from="Dc" to="TBdead" rate="muDc*sage"/>
   <transition from="R" to="Rdead" rate="muK*1.91"/>
  </transition.matrix>

  it is not straightforward to separate the flows Lf -> Ds vs Ls -> Ds although these could of course be calculated from the state variables and the parameter values.
  
  With the new extension it could be done simpler:

  <transition.matrix>
   <transition from="Lf" to="Ls" rate="omega"/>
   <transition from="Lf" to="Uc" rate="phiF"/>
   <transition from="Ls" to="Uc" rate="phiS"/>
   <transition from="Ds" to="Dc" rate="zeta"/>
   <transition from="Ds" to="R" rate="chi"/>
   <transition from="Dc" to="R" rate="chi"/>
   <transition from="R" to="Ds" rate="(1-pE)*rho*j3"/>
   <transition from="Dc" to="TBdead" rate="muDc*sage"/>
   <transition from="R" to="Rdead" rate="muK*1.91"/>
  </transition.matrix>
  <treatment.matrix name="LfLsDs">
   <transition from="Lf" to="Ds" rate="(1-pE)*theta*j1"/>
   <transition from="Ls" to="Ds" rate="(1-pE)*sigma*j2"/>
  </treatment.matrix>

  I.e. simply move Lf to Ds and Ls to Ds to a 'treatment' matrix (which is just a transition matrix with a different name) and
  inspect the output for flow 'out', subject Tr_LfLsDs to get the individual flows from Lf to Ds and from Ls to Ds.

  Note that this 'trick' can also be used to make flows time dependent .... (implementation in the XML is left as an exercise to the reader)

Nov 2, 2020

Name of log files is based on:
unlist(strsplit(opts$xmlinput,".xml"))[1],"-optim-",timestamp,Sys.getenv(opts$taskenvvar),".log")
i.e. a concatenation of the XML input filename, "-optim-" or "-blast", a timestamp in seconds since epoch (i.e. 1-1-1970), the environment variable passed with -e to the R script, ".log"

Fixed bug in calls to Sys.getenv() in which the argument should be opts$taskenvvar instead of 'taskid'


October 28, 2020

Model version 1.3

1. Fixed bug in code that modifies in memory XML object from parameters values in parameters.csv.
   The bug was in parameters that depend on e.g. age, HIV etc.

2. Fixed issue with UTF-8-BOM coded text / csv files (as the default may be when exporting files from MS Excel as csv).
   All files being read now have the option fileEncoding = "UTF-8-BOM" 
   Google 'R reading csv UTF-8-BOM' for more background; there also is info on 'UTF-8-BOM' at Wikipedia

3. There are now 4 different modes of running the model:
   a. blast: see run-blast.bat (and blast.R)
   b. optimize: see run-optimize.bat (and optimize.R)
   c. onerun: see run-one.bat (and runone.R)
   d. modify XML: see run-modify.bat (and modify-xml-with-parameters.R)
   Study the source files for command line options, and adapt the .bat files to your need (and copy to .sh ; don't forget to set the access right with chmod 755)
   
   Use option d to convert an existing XML file by modifying parameters in a parameters.csv file (i.e. both constant and fittable parameters); this is also meant as a check 
   for correctness of the code that modifies the in memory XML object based on parameters.csv files.

4. There is a new levmarq.csv with options for the optimizer. 
   Experience so far with optimization: the optimizer gets stuck in local minima now and then but is also capable of producing excellent fits within a couple of hours.


October 20, 2020

Model version 1.12

Modified the order of the columns of the blast log output:
taskID	nHITS	SSWR	SHA	TS	phiS	theta	sigma	rho	zeta	eta	muK	muDc	pT
The first 5 columns will always be in this order, remaining columns depend on parameters varied.

Added a simple shell script to process the log files with the following contents:
#!/bin/sh
tail -q -n +1 *.log | sort -nr -k 2 > sorted.log

This script should be run in the directory where the log files are.
It may be necessary to set the access rights of the log files (?):
chmod 755 *.log
and maybe of the script as well (?):
chmod 755 process-logs.sh

October 20, 2020

Model version 1.1

Separate R scripts:
blast.R
optimize.R
runone.R
with example .bat and .sh (not tested) files 

Included taskID in blast log file

October 15, 2020, 5.40 PM UK time

The main scripts, parameters, and data have moved to RAC's 'private' folders (AD, CM, RC).
Simply copy the contents of the RB folder to your own.

There is one script 'to rule them all' : run-batch-all.R 
And the equivalent for running from RStudio run-man-all.R

The following three .bat files illustrate how to call run-batch-all.R:
run-blast-example.bat, run-optim-example.bat and run-single-example.bat (and the equivalent .sh files for OSX / Linux / Unix)

The command-line parameters:

-e environment variable for taskID
-o the number of optimizations to perform (for each optimization a new random selection of nchoose parameters is selected from the parameters indicated TRUE in the choose column of the parameters file)
-a the number of runs with randomly sampled parameters from the parameters tagged 'uniform' in the dist column of the parameters file
-b subdirectory to find parameters, xml input file, xml run file, targets
-u subdirectory for output 
-t name of targets file
-p name of parameters file
-x name of xml input file
-r name of xml run file
-s seed (integer)
-d working directory (i.e. the current directory .)
-n number of iterations of optimization (1 optimization could call the simulation many times) [only for optimization]
-c the number of variable parameters to choose from the parameters tagged TRUE in the choose column [only for optimization] ; NOTE: these parameters should have min and max specified

Specify either the -o or the -a option (followed by an integer) to run optimization or blast.
Specify none of those for a single simulation run.

The .bat files start with setting an environment variable - on a cluster there should be an existing environment variable,
so leave out the first line and specify the name with -e [name of task ID].


October 15, 2020, 12.03 PM UK time

Fixed bug that was due to a combination of a coding error (i.e. calling as.numeric on a factor) and
the change in R 4.x that sets the default 'stringsAsFactors' argument of the data.frame() (and as.data.frame()) 
functions to F whereas it was T in R versions before 4.x.

The code runs fine now with both R 3.6 and R 4.0.x


October 12, 2020

from the cmd line:
start run.bat

[OS X .sh will follow later]


All xml, runxml, targets and parameter files are (for now) in: ./parameters-and-targets, i.e.
targets:    RC_IND_targets_RBv6.csv NOTE: TBdeaths 2000 and TB prev (2015) removed as these ruined the fits
parameters: RB_IND_parameters_v3.csv
The contents of these files is largely self-explanatory ; documentation will follow

input parameters: TBVxRC-3-RB-3.xml 
NOTE: this is a simplified version of Rebecca's TBVxRC-3_revisions-RB1.xml which was modified by Roel
without HIV and with 5 year age groups

run parameters: 
runRB-3.xml 
modified to ensure output for the proper years (compared to targets)

Dependencies for correct output processing

1. For calculating deaths: only 'HIVdead' and 'TBdead' are supported as names
   This will be improved in a later version (support for 'dead' as a substring of any state)

2. For calculating prr (prevalence rate ratio):
   SES stages should be named 'high' and 'low'

Bug fixes

1. contact matrix did not work correctly for 1 year age groups
2. generating incidence output expected non-null HIV parameter matrix
3. TO DO: HIV matrix with 0 values causes an error due to ddiMatrix 


September 16, 2020

1. Zombies
   
   Bug_1: HIV progression occurred in TBdead and TB progression in HIVdead.
   Bug_2: Both HIVdead and TBdead were aging.

   Bug_1 was fixed by setting the columns of 'dead' classes in all parameter matrices involved to 0.
   Bug_2 was fixed by not aging the dead. Both TBdead and HIVdead accumulate throughout the simulation run though.
   If needed, the dead could be removed yearly though that would mean that deaths reported at year ####.5 are deaths acculumated over the prior half year.
   
2. Output format
  
   The output format with the option report.inci=T [e.g. run.model(fparams,report.inci = T)] is now by default the long format.
   The previous output format is still supported though [use run.model(fparams,report.inci = T, long.format=F)]
   Example output:

    > out$dY
         YEAR   VXa  SES         RISK     HIV     TB age.from        n
    	    1: 2015.5 never poor non-diabetic    HIV-     Un        0 -1794.11
    	    2: 2015.5 never poor non-diabetic    HIV-     Uc        0     0.97
    	    3: 2015.5 never poor non-diabetic    HIV-     Lf        0   400.48
    	    4: 2015.5 never poor non-diabetic    HIV-     Ls        0   112.73
    	    5: 2015.5 never poor non-diabetic    HIV-     Ds        0    16.57
	---                                                                
	35420: 2018.5 never rich     diabetic HIVdead     Ds       90     0.00
	35421: 2018.5 never rich     diabetic HIVdead     Dc       90     0.00
	35422: 2018.5 never rich     diabetic HIVdead      T       90     0.00
	35423: 2018.5 never rich     diabetic HIVdead      R       90     0.00
	35424: 2018.5 never rich     diabetic HIVdead TBdead       90     0.00

3. Functions for processing output.

   TB.by.year.and.age.group(Z, years = NULL, TB.states = NA, lower.age.limits = NA, total=T)
   
   For example:  TB.by.year.and.age.group(out$dY, years = c(2015.5,2018.5), TB.states = c("Ds"), lower.age.limits = c(0,15,65), total=T)
 
   alive.pop.by.year.and.age.group(Z, years=NULL, lower.age.limits=NA, total=T)

   For example:  alive.pop.by.year.and.age.group(out$Y, years = c(2015.5,2018.5), lower.age.limits = c(0,15,65), total=T)
 
   See sensRange_simple-3.R for examples.

4. Removed imports of the reshape2 and dplyr R packages.



September 10, 2020

1. Improved performance of incidence output

2. Modified run.xsd

   The specification of output year for detailed output in e.g. runRC-2.xml now should be similar to:
   
    <output>
      <!--<final.population.as.fraction file="./data/IN-final-population-2050.txt"/>-->
      <detailed.output years="1970:2049+0.5"/> <!--age.group.lower.limits="0,15,50"/> -->
      <incidence.output years="2015+3*(0:1)+0.5">
   
   i.e. comparable to the specification of output years for incidence

3. Added an optional argument xattr to the function 
   
   modify.node.attr(xmldoc, xpath, name=NA, value=NULL, xattr=NULL) [in ./R/mod_xml.R]
   
   to allow selecting a parameter by name and by other attributes such as age.group, VXa etc.
   See sensRange_simple-3.R and sensRange-fncs-1.R for an example.
   

September 2, 2020

1. Extended output options to include output on incidence

There is 1 additional argument to the run.model function, which is 'report.inci' and which is F by default.
To generate incidence output, set 'report.inci' to T, e.g.

out=run.model(params,report.inci=T)

The data structure generated is a list of data frames:
out$prev is prevalance output, generated for the time points specified in the run file under 'detailed.output'

<detailed.output from.year="1970" thru.year="2050"/> <!--age.group.lower.limits="0,15,50"/> -->

i.e. from 1970 thru 2050 every 0.5 years (since dt=0.5 in <simulation from.year="1900" thru.year="2050" dt="0.5">),
for all age groups (as <!--age.group.lower.limits="0,15,50"/> --> is commented out).

New elements of the list out are:
out$Y       : prevalence output to be used for the denominator of incidence calculations
out$dY      : net flows i.e. the sum of dY_in and dY_out
out$dY.in   : inflow i.e. the number entering a box (state / compartment)
out$dY.out  : outflow i.e. the number leaving a box (state / compartment)

With the "write.to.file" option set to T, output is generated with filenames ending in:
_prev.txt
_Y.txt
_dY_net.txt
_dY_in.txt
dY_out.txt


The time points and contents of this output is determined by the incidence.output element of the run specification:

<incidence.output years="2000+5*(0:4)+0.5">
  <include natural.deaths="false">
    <output.dim  name="TB" transmission="true" progression="true" treatment="true"/>
    <output.dim  name="HIV" progression="true" incidence.from.data="true"/>
    <output.dim  name="VXa"  progression="true" incidence.from.data="true"/>
  </include>
</incidence.output>

The 'years' attribute determines the time points at which output is generated.
The result of the example above would be 2000.5, 2005.5, 2010.5, 2015.5 and 2020.5
The time points specified should be present in the prevalence output (as this is the basis of the incidence calculations).
It is best to use time points half way a year as aging and births occur once a year (at New Year) and cause a slight discontinuity.

There are various options to include or exclude the effect of various processes on the incidence output:
include.natural.deaths : either "true" or "false" 
for each of the dimensions:
transmission        : "true" or "false" ; only relevant for TB
treatment           : "true" or "false" ; only relevant for TB
progresssion        : "true" or "false" 
incidence.from.data : "true" or "false" 

With the use of these options it is probably possible to identify all individual in- and outflows of the model.
The age groupings used are the same as for prevalence output i.e. determined by the defaults age groups and in 
addition optionally by the age groupings specified in <detailed.output from.year="1970" thru.year="2050"/> <!--age.group.lower.limits="0,15,50"/> -->

The effect on performance of generating the incidence ouput is very small (depending on the time points for which this output is required).

August 20, 2020

1.
The default XML Schema file is now TB-Vx-schema-F.xsd and should be referenced as such in all input files.
The XML input files in this folder have been updated.

2.
A new element in the XML input file is 
<ages lower.limits="5*(0:16)"/>
which can be used to define any age grouping ; in the example above 0,5,...,80

The original age grouping was:
 <ages lower.limits="0:79,80,90"/>

3. 
A bug has been fixed that caused the model not to run when only one parameter was defined for VXa.

4.
A minor bug has been fixed in the yearly update of the contact matrix (yet to be proven mathematically that it is correct).
A note about contact matrices (and whether or not to use the transpose) is in preparation.

5. 
The simulation now uses deSolve and an automatic stepper.
This is the cause of the run time being increased to 8 seconds.

The arguments of the run.model function :
function(p=NULL, write.to.file=F, atol=1e-3, rtol=1e-3, method="ode23", print.t=F, print.aging=F)

with 
p             : parameters (result of calling params=init.parameters() with e.g. xmlfile="./TBVxB_noTBdeath.xml",xmlrunfile="./runB.xml" as arguments)
write.to.file : an option to write simulation results to a file ; otherwise a data.frame is returned
atol          : absolute tolerance
rtol          : relative tolerance
method        : numerical integration routine (see deSolve documentation)
print.t       : an option to print out the times when the derivatives function is called
print.aging   : an option to print out the times of aging the population by one year, adding newborns, and updating the contact matrix

6.
The run.model function returns a data frame and should be compatible with the FME package.
See the example code accompanying the FME package publication in JStatSoft 

7.
When data about demography is missing (e.g. year prior to 1950), the model uses the 1950 data.
The data file demographics_total_population.csv has been edited (data about years 1900 thru 1949 have been removed) and renamed to demographics_total_population_ok.csv





Aug 5, 2020

Added the edit_parameters.R script (and ./R/mod_xml.R).

Start edit_parameters.R from the command line as follows:

"C:\Program Files\R\R-4.0.2\bin\Rscript.exe" edit_parameters.R -p .\vary_params.txt -d .

The R script will read the tab delimited text file vary_params.txt and generate XML files using the information in that file:

source_file	xpath	attr_name	attr_value	target_file
TBVxA.xml	//TB/TB.progression/TB.parameter	omega	0.15	TBVxA_omega_0_15.xml
TBVxA.xml	//TB/TB.progression/TB.parameter	phiS	0.01	TBVxA_phiS_0_01.xml
TBVxA.xml	//TB/TB.progression/TB.parameter	omega	0.05	NA
NA	//TB/TB.progression/TB.parameter	phiS	0.02	TBVxA_omega_0_05_phiS_0_02.xml

The lines above are processed as follows:
line 1: TBVxA.xml is read and the parameter omega (located at the xpath specified) is edited to get the new value 0.15 ; the edits are written to the file in the column target_file
line 2: idem
line 3: TBVxA.xml is read, parameter omega is edited to 0.05 and as there is no file name in target_file the next line is read
line 4: in addition to the edited value of omega phiS is edited and the result is written to the file in column target file

In this way, multiple parameters can be changed and the cumulative changes will be written to a new file.

NOTE: this code has not been tested fully ....



July 30, 2020

See 'How to use the TB Vx model (version 30 July 2020).pdf'


July 24, 2020

Additions and modifications compared to July 14 version:

1. HIV incidence and implementation of ART
  
HIV incidence is read from a tab-delimited file (with incidence per 5 year age group per personyear (not per 1000 personyears)) with the following header:

YEAR country dim from to 0 5 10 15 20 25 30 35 40 45 50 55 60 65 70 75 80

with:
country: the two character ISO code of the country (e.g. IN for India, ZA for South Africa)
dim    : the dimension (for now always HIV)
from   : the HIV stage from which the transition takes place (the personyears apply to this group only)
to     : the target HIV stage 

i.e. for HIV incidence from = "HIV-" and to = "HIV1"

The format of the data file for ART implementation is identical, with e.g. from = "HIV1" and to = "ART1"
If required, it is possible to include in the same data file rows with from = "HIV2" and to = "ART2"
Personyears refer to either those in "HIV1" or "HIV2" (and are per age group of course).

2. Background deaths

Mortality rates (read from e.g. IN_mort_age.txt) are now corrected for HIV death and TB death.
'NATdead' no longer is a substage of TB.
There is no record of background deaths (they simply 'vaporize').

For now, both HIV deaths and TB deaths are being recorded (and not included in aging and in the denominator for calculations) as the number of deaths per year. It is therefore possible to keep track of HIV and TB deaths including VX, SES, RISK and TB (for HIV deaths) or HIV (for TB deaths) status. 



July 14, 2020

1. Information about the R version used and package versions (excerpt from info obtained with sessionInfo())

R version 4.0.2 (2020-06-22)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 10 x64 (build 18363)

Matrix products: default

Random number generation:
  RNG:     Mersenne-Twister 
Normal:  Inversion 
Sample:  Rounding 

attached base packages:
  [1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
  [1] microbenchmark_1.4-7 xml2_1.3.2           Matrix_1.2-18        stringi_1.4.6        assertthat_0.2.1    

loaded via a namespace (and not attached):
  [1] compiler_4.0.2  tools_4.0.2     rstudioapi_0.11 grid_4.0.2      lattice_0.20-41


2. TO DO

a. vaccine implementation
b. HIV incidence and implementation of ART
c. correct death rates for TB deaths and HIV deaths (and possibly correct HIV deaths for TB deaths)

Ad a.
It is possible to explore vaccination of newborns by setting the fraction.at.birth for a vaccinated stage to a value > 0

Ad b.
Similarly, the same can be done for HIV i.e. set fraction.at.birth to a value > 0

Ad c.
Currently, there are 3 TB stages for recording deaths.
This will probably be modified in the next version.

3. MAIN FUNCTIONS AND OPTIONS

init.parameters=function(xmlfile=NA, xmlschemafile=NA,
                         datapath=NA, countrycode=NA, 
                         initial.population.txt=NA, 
                         population.csv=NA,
                         deathrate.txt=NA,
                         contact.matrix.txt = NA,
                         birthrate = NA,
                         birthrate.as.number=F,
                         seed = NA)
  
Example use:
  params = init.parameters(xmlfile                = "./TB-Vx-TB-ALL-F.xml",
                           xmlschemafile          = "./TB-Vx-schema-E.xsd",
                           datapath               = "./data/", 
                           countrycode            = "IN", 
                           initial.population.txt = NA, #"IN_initial_population_as_fraction.txt", 
                           population.csv         = "demographics_total_population.csv",
                           deathrate.txt          = "IN_mort_age.txt",
                           contact.matrix.txt     = "IN_all_contacts.txt",
                           birthrate              = 21, # e.g. 21 , # NA if to be read from population data 
                           birthrate.as.number    = F, # F if birthrate as fraction (CBR per 1000 pop)
                           seed                   = list(TB.stages=c("Ds","Dc"),age.from.thru=c("A20","A24"),n=c(250,50)))

arguments:
  xmlfile:        the XML input file
  xmlschemafile : the XML schema file
  datapath:       the directory containing the data files (relative to current directory i.e. the directory with TB-Vx-main.R)
  countrycode:    2 character ISO countrycode ; currently only 'IN' (India) is supported
  initial.population.txt : optional ; tab-delimited file with the initial population (result from a previous run)
  population.csv: comma separated file with demographics (single year age groups) from 1900 thru 2100 (India and China only)
  deathrate.txt:  tab delimited file with deathrate 1971 thru 2050 for single year age groups 0 thru 99 
  contact.matrix.txt: tab delimited file with contact matrix (data from K. Prems paper 152 countries ...); data from Excel file ; header added
  birthrate:      either NA: birthrates derived from 0 year olds in demography data, or a fraction per 1000 (if birthrate.as.number is F) 
                  or an absolute number (if birthrate.as.number is T)
  seed:           to seed initial TB cases ; a list is expected as follows:                 
                  list(TB.stages=c("Ds","Dc"),age.from.thru=c("A20","A24"),n=c(250,50))
                  i.e. TB.stages, an age range and a number per TB stage


NOTES:
Demography data are available for the time period of 1900 - 2100 whereas death rates are from 1971 thru 2050.
Also, the initial population composition appears not to be in equilibrium with the birhts derived from the same data (huge and fast changes in population age groups). This may be due to death rates missing for the years 1900-1970?

run = function(derivs, 
               dt, 
               yini=fparams$y.ini, 
               years, 
               fparams, 
               births.and.aging=T, 
               write.final.alive.population.to = NA)

Example use:

yearz = 1900:2050
ycal=run(derivs, dt=1/2, years = yearz, fparams=params)
or 
ycal=run(derivs, dt=1/2, years = yearz, fparams=params, write.final.alive.population.to = "./data/IN_ALL_F_2050.txt") 


Arguments:

derivs:           the function that calculates the derivative (see TBVx-derivs-v6.R) 
dt:               the time step in years (try 1/2 then 1/3 if necessary, then 1/4 etc.)
yini:             initial population 
years:            the time period to run the simulation e.g. 1900:2050
fparams:          fixed parameters ; pass in the params obtained from init.parameters()
births.and.aging: defaults to T
write.final.alive.population.to: a file to write the population at the end of the simulation run to


NOTE:
The option to write the final population to a file may be useful to freeze a population and restart from that population.
It even is possible to write a population produced by a simulation run with VXa turned off (i.e. just the "never" VXa stage)
and use that as a starting point for a new simulation run with more than one VXa stage.
Of course, no conflicts in named stages should arise, but the frozen population may very well be a subset of the population simulated 
in a new run.







