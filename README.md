### This repo will encompass the scripts analyzing performance efficiency, as well as forecasting performance, and target recommendations on City of New Orleans' Key Performance Indicators.


#### Component scripts
* Efficiency measurements and regression modeling (Efficiency.R)
* Performance Forecasting (forthcoming)
* Target Recommendations (forthcoming)

#### Current Methodologies - Efficency.R
* Efficiency measurement:
  * "Count" type KPIs
    * "Expected Efficiency" = Yearly Target/Yearly Budget
    * "Actual Efficiency" = Yearly Actual/ Yearly Budget
* Performance Regressions
  * Linear model of "Performance Status" ~ "Budget Growth"
    * "Performance Status" == Percent of target achieved on a KPIs
    * "Budget Growth" == Percent growth of budget from prior year
  * Linear Model of Net Performance ~ Budget Growth
    * Net Performance == Net change in Performance Status from prior year


#### To-Do's
* Finish Efficiency.R
  * Generally, provide more nuanced efficiency calculations (if needed)
    * Calculate percent change in performance
    * Efficiency calculations for different variable types?
    * Evaluate current regression models, and create new ones
  * Figure out how to automate efficiency and regression plots for all departments
    * Note: Efficiency plot is currently only for Public Works measures
  * Integrate more layers of resource data:
    * Full-Time Equivalents/employee head counts per department
    * Sub-departmental budget allocations
    * Actual Expenditures at departmental level
    * Actual Expenditures at sub-departmental level
* Begin PerForecast.R
* Begin Target_Recommender.R
* Markdowns
