---
title: "Covid Surveys: Data Display and Aggregation"
output:
  rmdformats::html_clean
---



# 1. Motivation

Lots of Covid-19 studies are going on. This tool can be used to present basic results in a simple manner in real time (or, as quickly as you can make your data available). 

We also have two ulterior motives:

* If teams use this it would be easy to use it to create a meta-dashboard across teams and countries making it easy to locate and view the good work being done
* If teams use this then it would not be hard to pool studies together to calculate cross study aggregates. In addition to the basic information, good aggregation woudl require sampling weights information.

* **Note on software**: the dashboard is produced using `Rmarkdown` but the inputs are just datafile. If you work in stata or anything else and can produce `.dta` files (best to save as version 13 or below), `.csv` or `xls` it should be easy to inputhose to this set up. 

* **Note on hosting**: If you can get the input files to us and can regularly upload your data to github we think we can produce and host the dashboard on this site and catalogue it with other ones. Contact us at:...

* **Note on open source material**: All material here is open source. Data posted here will be made public and should not contain any personally identifiable information.


# 2. Inputs

## `my_vars` dataframe

A spreadsheet that says what your variables are and to which families they belong. For example:


Table: sample `my_vars` dataframe

variable      family    short_label        description                 
------------  --------  -----------------  ----------------------------
market_open   markets   Is market open?    details on market open      
price_rice    markets   Price of a rice    Price of a cup of rice      
aware         actions   Aware of Covid19   Details on aware of Covid19 
water         actions   Access to water    Details on access to water  

## `my_data`  dataframe

Your data, cleaned and transformed to the point where it is ready for display:

<!--html_preserve--><div id="htmlwidget-cc9e60dcfc9b9ee2df1c" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-cc9e60dcfc9b9ee2df1c">{"x":{"filter":"none","caption":"<caption>sample `my_data` dataframe<\/caption>","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27"],["Bo","Bo","Bo","Bo","Bo","Bo","Bo","Bo","Bo","Bombali","Bombali","Bombali","Bombali","Bombali","Bombali","Bombali","Bombali","Bombali","Bonthe","Bonthe","Bonthe","Bonthe","Bonthe","Bonthe","Bonthe","Bonthe","Bonthe"],["2020-04-18","2020-04-18","2020-04-18","2020-04-19","2020-04-19","2020-04-19","2020-04-20","2020-04-20","2020-04-20","2020-04-18","2020-04-18","2020-04-18","2020-04-19","2020-04-19","2020-04-19","2020-04-20","2020-04-20","2020-04-20","2020-04-18","2020-04-18","2020-04-18","2020-04-19","2020-04-19","2020-04-19","2020-04-20","2020-04-20","2020-04-20"],[1,1,1,0,1,0,0,0,1,0,0,1,1,0,0,0,1,1,1,0,1,1,0,1,1,1,1],[0,1,1,0,1,1,0,1,0,0,0,1,1,0,1,0,0,1,1,0,1,1,0,0,1,0,1],[0,1,0,0,1,1,1,1,0,1,1,1,0,0,1,0,1,0,1,0,1,1,1,0,1,1,1],[1,1,1,1,1,1,1,1,0,1,0,0,1,0,0,0,0,0,1,1,0,0,0,1,1,0,0]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>id<\/th>\n      <th>date<\/th>\n      <th>market_open<\/th>\n      <th>price_rice<\/th>\n      <th>aware<\/th>\n      <th>water<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"columnDefs":[{"className":"dt-right","targets":[3,4,5,6]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

The key requirements are that the dataset contains:

* an `id` variable which gives the level at which you want the data to be aggregated for averages -- this variable should correspond to your id variable in your shape files   
* a `date` variable whch is a string variable of the form `YYYY-MM-DD`

## `my_text`  dataframe

Third, a spreadsheet with the text you want to appear. You can provide title and author information in here, or you can provide these directly to the dashboard function.


Table: sample `my_text` dataframe

intro_text   intro_note                                                        data_note                             
-----------  ----------------------------------------------------------------  --------------------------------------
Intro text   Lots of great people worked on this. More information here: ...   Study specific note about data source 

## `my_maps` shape files

Last if you want to display maps you should give a path to your shape files:


```r
 my_maps <- "c:/temp/shapefiles"
```


# 3 Produce Dashboard

The dashboard is then produced by `dashdash::dashdash` like this:





```r
remotes::install_github("wzb-ipi/dashdash")

dashdash::dashdash(
  output_file = "example.html",
  title = "A sample dashboard for my study",
  subtitle = "What's special about this study",
  author   = "D Ash",
  my_data = my_data,
  my_vars = my_vars,
  my_text = my_text,
  my_maps = my_maps)
```

You can then view it: [example.html](example.html)

# 4 Aggregating dashboards

Coming next
