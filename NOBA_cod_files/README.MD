NOBA Cod SAM2SS conversion

- State-space assessment model (SAM) 
	- [GitHub repository](https://github.com/fishfollower/SAM)
	- Aldrin et al., 2020: [The specification of the data model part in the SAM model matters](https://www.sciencedirect.com/science/article/pii/S0165783620301028#bib0020)
	- Nielsen and Berg, 2014: [Estimation of time-varying selectivity in stock assessments using state-space models](https://www.sciencedirect.com/science/article/abs/pii/S0165783614000228?via%3Dihub)
	- Berg and Nielsen, 2016: [Accounting for correlated observations in an age-based state-space stock assessment model](https://academic.oup.com/icesjms/article/73/7/1788/2458744)

- Northeast Arctic cod assessment
	- [ICES Report of the Arctic Fisheries Working Group (AFWG)](https://www.ices.dk/sites/pub/Publication%20Reports/Expert%20Group%20Report/acom/2018/AFWG/00-AFWG%202018%20Report.pdf)

- Data used in the assessment (page 124)
	- Catch-at-age
		- Biological sampling from parts of the fishery that may have become critically low
	- Weight-at-age (weighted average of the weight-at-age)
		- Because of very noisy values observed for older fish, weight-at-age 12–15 was set to constant values in this period
		- Weight-at-age 12 was equal to the mean for 1983–2015
		- The mean increment for ages 12–13, 13–14 and 14–15+ groups in the 1983–2015 period were used to calculate weight-at-age 13, 14, and 15
	- Natural mortality including cannibalism
		- M of 0.2 + cannibalism
		- Cannibalism is assumed to only affect natural mortality of ages 3-6
		- Calculate the consumption of cod by cod (Bogstadand Mehl, 1997)
		- Prey age groups 0—6 and predator age groups 1–11+
	- Maturity-at-age
		- The proportions of mature cod for age 13–15 was set to 1 for the period 1984–present
	- Surveys
		- 4 surveys (3 trawl surveys and 1 ecosystem survey)
		- Different areas, season, ages, and years
		- The surveys that are conducted during winter (FLT 15 and 16) are allocated to the time of the year when they are carried out
		- Survey indices for Fleet 15 have been multiplied by a factor 100, while survey indices for Fleets 007, 16 and 18 have been multiplied by a factor 10






