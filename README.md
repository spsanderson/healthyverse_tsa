Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
28 March, 2025

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 135,205
    ## Columns: 11
    ## $ date      <date> 2020-11-23, 2020-11-23, 2020-11-23, 2020-11-23, 2020-11-23,…
    ## $ time      <Period> 15H 36M 55S, 11H 26M 39S, 23H 34M 44S, 18H 39M 32S, 9H 0M…
    ## $ date_time <dttm> 2020-11-23 15:36:55, 2020-11-23 11:26:39, 2020-11-23 23:34:…
    ## $ size      <int> 4858294, 4858294, 4858301, 4858295, 361, 4863722, 4864794, 4…
    ## $ r_version <chr> NA, "4.0.3", "3.5.3", "3.5.2", NA, NA, NA, NA, NA, NA, NA, N…
    ## $ r_arch    <chr> NA, "x86_64", "x86_64", "x86_64", NA, NA, NA, NA, NA, NA, NA…
    ## $ r_os      <chr> NA, "mingw32", "mingw32", "linux-gnu", NA, NA, NA, NA, NA, N…
    ## $ package   <chr> "healthyR.data", "healthyR.data", "healthyR.data", "healthyR…
    ## $ version   <chr> "1.0.0", "1.0.0", "1.0.0", "1.0.0", "1.0.0", "1.0.0", "1.0.0…
    ## $ country   <chr> "US", "US", "US", "GB", "US", "US", "DE", "HK", "JP", "US", …
    ## $ ip_id     <int> 2069, 2804, 78827, 27595, 90474, 90474, 42435, 74, 7655, 638…

The last day in the data set is 2025-03-26 23:46:42, the file was
birthed on: 2024-08-07 07:35:44, and at report knit time is -5556.18
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 135205        |
| Number of columns                                | 11            |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |               |
| Column type frequency:                           |               |
| character                                        | 6             |
| Date                                             | 1             |
| numeric                                          | 2             |
| POSIXct                                          | 1             |
| Timespan                                         | 1             |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |               |
| Group variables                                  | None          |

Data summary

**Variable type: character**

| skim_variable | n_missing | complete_rate | min | max | empty | n_unique | whitespace |
|:--------------|----------:|--------------:|----:|----:|------:|---------:|-----------:|
| r_version     |     97150 |          0.28 |   5 |   5 |     0 |       46 |          0 |
| r_arch        |     97150 |          0.28 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     97150 |          0.28 |   7 |  15 |     0 |       21 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       60 |          0 |
| country       |     11364 |          0.92 |   2 |   2 |     0 |      163 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2025-03-26 | 2023-05-24 | 1585 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1135388.77 | 1524588.92 | 355 | 14701 | 264474 | 2367773 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 10365.89 | 18358.85 | 1 | 305 | 3075 | 11765 | 209747 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2025-03-26 23:46:42 | 2023-05-24 06:38:54 | 82198 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |     median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-----------:|---------:|
| time          |         0 |             1 |   0 |  59 | 12H 8M 18S |       60 |

We can see that the following columns are missing a lot of data and for
us are most likely not useful anyways, so we will drop them
`c(r_version, r_arch, r_os)`

## Plots

Now lets take a look at a time-series plot of the total daily downloads
by package. We will use a log scale and place a vertical line at each
version release for each package.

![](man/figures/README-initial_ts_plot-1.png)<!-- -->![](man/figures/README-initial_ts_plot-2.png)<!-- -->

Now lets take a look at some time series decomposition graphs.

![](man/figures/README-ts_decomp_plt-1.png)<!-- -->![](man/figures/README-ts_decomp_plt-2.png)<!-- -->![](man/figures/README-ts_decomp_plt-3.png)<!-- -->![](man/figures/README-ts_decomp_plt-4.png)<!-- -->

## Feature Engineering

Now that we have our basic data and a shot of what it looks like, let’s
add some features to our data which can be very helpful in modeling.
Lets start by making a `tibble` that is aggregated by the day and
package, as we are going to be interested in forecasting the next 4
weeks or 28 days for each package. First lets get our base data.

    ## 
    ## Call:
    ## stats::lm(formula = .formula, data = df)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -151.16  -35.25  -10.18   26.92  811.26 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -186.41966   74.06765
    ## date                                                  0.01128    0.00393
    ## lag(value, 1)                                         0.11112    0.02491
    ## lag(value, 7)                                         0.09407    0.02577
    ## lag(value, 14)                                        0.09698    0.02580
    ## lag(value, 21)                                        0.06344    0.02588
    ## lag(value, 28)                                        0.06196    0.02575
    ## lag(value, 35)                                        0.07185    0.02595
    ## lag(value, 42)                                        0.04974    0.02601
    ## lag(value, 49)                                        0.08221    0.02593
    ## month(date, label = TRUE).L                         -10.68029    5.14576
    ## month(date, label = TRUE).Q                           2.56358    5.18984
    ## month(date, label = TRUE).C                         -12.07125    5.24042
    ## month(date, label = TRUE)^4                          -7.03564    5.19516
    ## month(date, label = TRUE)^5                         -12.40172    5.20150
    ## month(date, label = TRUE)^6                          -2.87808    5.28013
    ## month(date, label = TRUE)^7                          -6.44013    5.16127
    ## month(date, label = TRUE)^8                          -4.70902    5.19133
    ## month(date, label = TRUE)^9                           5.81768    5.25145
    ## month(date, label = TRUE)^10                          4.28924    5.29773
    ## month(date, label = TRUE)^11                         -5.88436    5.32627
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -11.76623    2.39119
    ## fourier_vec(date, type = "cos", K = 1, period = 7)    7.83880    2.52255
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.517 0.011942 *  
    ## date                                                 2.871 0.004152 ** 
    ## lag(value, 1)                                        4.461 8.74e-06 ***
    ## lag(value, 7)                                        3.650 0.000271 ***
    ## lag(value, 14)                                       3.759 0.000177 ***
    ## lag(value, 21)                                       2.451 0.014372 *  
    ## lag(value, 28)                                       2.406 0.016239 *  
    ## lag(value, 35)                                       2.769 0.005699 ** 
    ## lag(value, 42)                                       1.912 0.056052 .  
    ## lag(value, 49)                                       3.170 0.001554 ** 
    ## month(date, label = TRUE).L                         -2.076 0.038104 *  
    ## month(date, label = TRUE).Q                          0.494 0.621405    
    ## month(date, label = TRUE).C                         -2.303 0.021386 *  
    ## month(date, label = TRUE)^4                         -1.354 0.175853    
    ## month(date, label = TRUE)^5                         -2.384 0.017236 *  
    ## month(date, label = TRUE)^6                         -0.545 0.585780    
    ## month(date, label = TRUE)^7                         -1.248 0.212305    
    ## month(date, label = TRUE)^8                         -0.907 0.364502    
    ## month(date, label = TRUE)^9                          1.108 0.268115    
    ## month(date, label = TRUE)^10                         0.810 0.418277    
    ## month(date, label = TRUE)^11                        -1.105 0.269431    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -4.921 9.57e-07 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   3.107 0.001922 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 58.49 on 1513 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2544, Adjusted R-squared:  0.2436 
    ## F-statistic: 23.47 on 22 and 1513 DF,  p-value: < 2.2e-16

![](man/figures/README-base_data_frame-1.png)<!-- -->

## NNS Forecasting

This is something I have been wanting to try for a while. The `NNS`
package is a great package for forecasting time series data.

[NNS GitHub](https://github.com/OVVO-Financial/NNS)

``` r
library(NNS)

data_list <- base_data |>
    select(package, value) |>
    group_split(package)

data_list |>
    imap(
        \(x, idx) {
            obj <- x
            x <- obj |> pull(value) |> tail(7*52)
            train_set_size <- length(x) - 56
            pkg <- obj |> pluck(1) |> unique()
            sf <- NNS.seas(x, modulo = 7, plot = FALSE)$periods
            
            cat(paste0("Package: ", pkg, "\n"))
            NNS.ARMA.optim(
                variable = x,
                h = 28,
                training.set = train_set_size,
                #seasonal.factor = seq(12, 60, 7),
                seasonal.factor = sf,
                pred.int = 0.95,
                plot = TRUE
            )
            title(
                sub = paste0("\n",
                             "Package: ", pkg, " - NNS Optimization")
            )
        }
    )
```

    ## Package: healthyR
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 98 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.61597427637143"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 98, 77 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.52635969175873"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 98, 77 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.52635969175873"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 98, 77 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 1.89328015669716"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 98, 77 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 1.89328015669716"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 98, 77 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 1.63280288069986"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 98, 77 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 1.63280288069986"

![](man/figures/README-nns_forecasting-1.png)<!-- -->

    ## Package: healthyR.ai
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 77 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.53726670821228"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 77, 49 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.33608018041863"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 77, 49, 70 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.25738154388152"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 77, 49, 70, 98 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.23528865474958"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 77, 49, 70, 98 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.23528865474958"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 77, 49, 70, 98 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 1.90293007428554"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 77, 49, 70, 98 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 1.90293007428554"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 77, 49, 70, 98 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 1.4196668974297"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 77, 49, 70, 98 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 1.4196668974297"

![](man/figures/README-nns_forecasting-2.png)<!-- -->

    ## Package: healthyR.data
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 98 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.1940428434813"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 98, 77 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.16238535290181"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 98, 77 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.16238535290181"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 98, 77 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 1.70945748036512"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 98, 77 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 1.70945748036512"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 98, 77 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 1.36114000732557"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 98, 77 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 1.36114000732557"

![](man/figures/README-nns_forecasting-3.png)<!-- -->

    ## Package: healthyR.ts
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 98 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.27911204099034"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 98, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 0.990851554767481"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 98, 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 0.990851554767481"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 98, 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 1.58142424126755"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 98, 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 1.58142424126755"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 98, 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 1.17566863537857"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 98, 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 1.17566863537857"

![](man/figures/README-nns_forecasting-4.png)<!-- -->

    ## Package: healthyverse
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 49 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.89643308004905"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 49, 77 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.64981416377303"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 49, 77 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.64981416377303"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 49, 77 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 3.25960790996331"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 49, 77 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 3.25960790996331"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 49, 77 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 2.24339288190793"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 49, 77 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 2.24339288190793"

![](man/figures/README-nns_forecasting-5.png)<!-- -->

    ## Package: RandomWalker
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 35 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 4.83735614162943"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 35 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 4.83735614162943"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 35 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 9.59630946499393"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 35 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 9.59630946499393"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 35 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 7.24417218727197"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 35 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 7.24417218727197"

![](man/figures/README-nns_forecasting-6.png)<!-- -->

    ## Package: tidyAML
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 35 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 4.01107042694026"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 35 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 4.01107042694026"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 35 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 5.12435280320476"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 35 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 5.12435280320476"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 35 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 4.13697518026651"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 35 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 4.13697518026651"

![](man/figures/README-nns_forecasting-7.png)<!-- -->

    ## Package: TidyDensity
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 77 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.68109231985788"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 77, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.47058381779751"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 77, 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.47058381779751"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 77, 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 2.43715359502596"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 77, 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 2.43715359502596"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 77, 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 1.74347203412849"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 77, 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 1.74347203412849"

![](man/figures/README-nns_forecasting-8.png)<!-- -->

    ## [[1]]
    ## NULL
    ## 
    ## [[2]]
    ## NULL
    ## 
    ## [[3]]
    ## NULL
    ## 
    ## [[4]]
    ## NULL
    ## 
    ## [[5]]
    ## NULL
    ## 
    ## [[6]]
    ## NULL
    ## 
    ## [[7]]
    ## NULL
    ## 
    ## [[8]]
    ## NULL

## Pre-Processing

Now we are going to do some basic pre-processing.

``` r
data_padded_tbl <- base_data %>%
  pad_by_time(
    .date_var  = date,
    .pad_value = 0
  )

# Get log interval and standardization parameters
log_params  <- liv(data_padded_tbl$value, limit_lower = 0, offset = 1, silent = TRUE)
limit_lower <- log_params$limit_lower
limit_upper <- log_params$limit_upper
offset      <- log_params$offset

data_liv_tbl <- data_padded_tbl %>%
  # Get log interval transform
  mutate(value_trans = liv(value, limit_lower = 0, offset = 1, silent = TRUE)$log_scaled)

# Get Standardization Params
std_params <- standard_vec(data_liv_tbl$value_trans, silent = TRUE)
std_mean   <- std_params$mean
std_sd     <- std_params$sd

data_transformed_tbl <- data_liv_tbl %>%
  # get standardization
  mutate(value_trans = standard_vec(value_trans, silent = TRUE)$standard_scaled) %>%
  select(-value)
```

Since this is panel data we can follow one of two different modeling
strategies. We can search for a global model in the panel data or we can
use nested forecasting finding the best model for each of the time
series. Since we only have 5 panels, we will use nested forecasting.

To do this we will use the `nest_timeseries` and
`split_nested_timeseries` functions to create a nested `tibble`.

``` r
horizon <- 4*7

nested_data_tbl <- data_transformed_tbl %>%
    
    # 1. Extending: We'll predict n days into the future.
    extend_timeseries(
        .id_var        = package,
        .date_var      = date,
        .length_future = horizon
    ) %>%
    
    # 2. Nesting: We'll group by id, and create a future dataset
    #    that forecasts n days of extended data and
    #    an actual dataset that contains n*2 days
    nest_timeseries(
        .id_var        = package,
        .length_future = horizon
        #.length_actual = horizon*2
    ) %>%
    
   # 3. Splitting: We'll take the actual data and create splits
   #    for accuracy and confidence interval estimation of n das (test)
   #    and the rest is training data
    split_nested_timeseries(
        .length_test = horizon
    )

nested_data_tbl
```

    ## # A tibble: 8 × 4
    ##   package       .actual_data         .future_data      .splits          
    ##   <fct>         <list>               <list>            <list>           
    ## 1 healthyR.data <tibble [1,578 × 2]> <tibble [28 × 2]> <split [1550|28]>
    ## 2 healthyR      <tibble [1,571 × 2]> <tibble [28 × 2]> <split [1543|28]>
    ## 3 healthyR.ts   <tibble [1,515 × 2]> <tibble [28 × 2]> <split [1487|28]>
    ## 4 healthyverse  <tibble [1,486 × 2]> <tibble [28 × 2]> <split [1458|28]>
    ## 5 healthyR.ai   <tibble [1,310 × 2]> <tibble [28 × 2]> <split [1282|28]>
    ## 6 TidyDensity   <tibble [1,161 × 2]> <tibble [28 × 2]> <split [1133|28]>
    ## 7 tidyAML       <tibble [769 × 2]>   <tibble [28 × 2]> <split [741|28]> 
    ## 8 RandomWalker  <tibble [191 × 2]>   <tibble [28 × 2]> <split [163|28]>

Now it is time to make some recipes and models using the modeltime
workflow.

## Modeltime Workflow

### Recipe Object

``` r
recipe_base <- recipe(
  value_trans ~ date
  , data = extract_nested_test_split(nested_data_tbl)
  )

recipe_base

recipe_date <- recipe_base %>%
    step_mutate(date = as.numeric(date))
```

### Models

``` r
# Models ------------------------------------------------------------------

# Auto ARIMA --------------------------------------------------------------

model_spec_arima_no_boost <- arima_reg() %>%
  set_engine(engine = "auto_arima")

wflw_auto_arima <- workflow() %>%
  add_recipe(recipe = recipe_base) %>%
  add_model(model_spec_arima_no_boost)

# NNETAR ------------------------------------------------------------------

model_spec_nnetar <- nnetar_reg(
  mode              = "regression"
  , seasonal_period = "auto"
) %>%
  set_engine("nnetar")

wflw_nnetar <- workflow() %>%
  add_recipe(recipe = recipe_base) %>%
  add_model(model_spec_nnetar)

# TSLM --------------------------------------------------------------------

model_spec_lm <- linear_reg() %>%
  set_engine("lm")

wflw_lm <- workflow() %>%
  add_recipe(recipe = recipe_base) %>%
  add_model(model_spec_lm)

# MARS --------------------------------------------------------------------

model_spec_mars <- mars(mode = "regression") %>%
  set_engine("earth")

wflw_mars <- workflow() %>%
  add_recipe(recipe = recipe_base) %>%
  add_model(model_spec_mars)
```

### Nested Modeltime Tables

``` r
nested_modeltime_tbl <- modeltime_nested_fit(
  # Nested Data
  nested_data = nested_data_tbl,
   control = control_nested_fit(
     verbose = TRUE,
     allow_par = FALSE
   ),
  # Add workflows
  wflw_auto_arima,
  wflw_lm,
  wflw_mars,
  wflw_nnetar
)
```

``` r
nested_modeltime_tbl <- nested_modeltime_tbl[!is.na(nested_modeltime_tbl$package),]
```

### Model Accuracy

``` r
nested_modeltime_tbl %>%
  extract_nested_test_accuracy() %>%
  filter(!is.na(package)) %>%
  knitr::kable()
```

| package | .model_id | .model_desc | .type | mae | mape | mase | smape | rmse | rsq |
|:---|---:|:---|:---|---:|---:|---:|---:|---:|---:|
| healthyR.data | 1 | ARIMA | Test | 0.6747974 | 183.36248 | 0.7724265 | 165.7124 | 0.8470637 | 0.0016996 |
| healthyR.data | 2 | LM | Test | 0.6719436 | 211.50604 | 0.7691599 | 134.9085 | 0.7991761 | 0.0294560 |
| healthyR.data | 3 | EARTH | Test | 0.8385459 | 189.15554 | 0.9598660 | 149.5715 | 1.0599330 | 0.0294560 |
| healthyR.data | 4 | NNAR | Test | 0.6808234 | 98.86836 | 0.7793243 | 171.4010 | 0.8875712 | 0.0114197 |
| healthyR | 1 | ARIMA | Test | 0.7609381 | 156.87309 | 0.8576071 | 172.4757 | 0.8972127 | 0.0142150 |
| healthyR | 2 | LM | Test | 0.7018043 | 101.99653 | 0.7909609 | 186.0921 | 0.8662535 | 0.0762461 |
| healthyR | 3 | EARTH | Test | 0.7016307 | 99.41089 | 0.7907653 | 189.7185 | 0.8649072 | 0.0762461 |
| healthyR | 4 | NNAR | Test | 0.7030260 | 213.69462 | 0.7923379 | 155.7655 | 0.8386638 | 0.0659036 |
| healthyR.ts | 1 | ARIMA | Test | 0.9795933 | 361.92520 | 0.8342427 | 139.7795 | 1.1628869 | 0.0510867 |
| healthyR.ts | 2 | LM | Test | 0.9415225 | 315.26627 | 0.8018208 | 140.4872 | 1.1311742 | 0.0510867 |
| healthyR.ts | 3 | EARTH | Test | 0.9619552 | 341.96128 | 0.8192217 | 139.8728 | 1.1486982 | 0.0510867 |
| healthyR.ts | 4 | NNAR | Test | 0.8606074 | 105.72002 | 0.7329118 | 162.8306 | 1.0885740 | 0.0185864 |
| healthyverse | 1 | ARIMA | Test | 0.6319556 | 205.32500 | 0.9184594 | 106.9048 | 0.7694444 | 0.0083583 |
| healthyverse | 2 | LM | Test | 0.6470733 | 280.61593 | 0.9404308 | 101.3634 | 0.7645498 | 0.0118844 |
| healthyverse | 3 | EARTH | Test | 0.6180453 | 168.70176 | 0.8982427 | 110.9560 | 0.7798265 | 0.0118844 |
| healthyverse | 4 | NNAR | Test | 0.6243599 | 166.38465 | 0.9074201 | 114.6761 | 0.7820383 | 0.0065349 |
| healthyR.ai | 1 | ARIMA | Test | 0.7474409 | 121.52002 | 0.8674378 | 181.2248 | 0.8732701 | 0.0647014 |
| healthyR.ai | 2 | LM | Test | 0.6850809 | 124.10683 | 0.7950662 | 141.0032 | 0.8558869 | 0.0049784 |
| healthyR.ai | 3 | EARTH | Test | 0.8321566 | 197.44624 | 0.9657540 | 178.3094 | 0.9522077 | 0.0049784 |
| healthyR.ai | 4 | NNAR | Test | 0.6737286 | 144.41321 | 0.7818914 | 139.7919 | 0.8232442 | 0.0903327 |
| TidyDensity | 1 | ARIMA | Test | 0.5936316 | 231.29948 | 0.6868957 | 101.8460 | 0.7631129 | 0.0168284 |
| TidyDensity | 2 | LM | Test | 0.6467632 | 285.79010 | 0.7483747 | 105.3273 | 0.8139161 | 0.0184069 |
| TidyDensity | 3 | EARTH | Test | 0.5992760 | 219.41051 | 0.6934270 | 107.3936 | 0.7586662 | 0.0184069 |
| TidyDensity | 4 | NNAR | Test | 0.5562506 | 129.05210 | 0.6436419 | 122.8437 | 0.6999512 | 0.1248206 |
| tidyAML | 1 | ARIMA | Test | 0.6967179 | 293.68288 | 0.7742402 | 102.9931 | 0.8195339 | 0.0086360 |
| tidyAML | 2 | LM | Test | 0.6531693 | 272.72835 | 0.7258460 | 101.6823 | 0.7768798 | 0.0073287 |
| tidyAML | 3 | EARTH | Test | 0.6727138 | 307.98966 | 0.7475652 | 100.3714 | 0.7988334 | 0.0073287 |
| tidyAML | 4 | NNAR | Test | 0.6603226 | 247.89325 | 0.7337952 | 103.9271 | 0.7891303 | 0.0217861 |
| RandomWalker | 1 | ARIMA | Test | 1.1152397 | 117.41478 | 0.6961945 | 100.5058 | 1.5185781 | 0.0188790 |
| RandomWalker | 2 | LM | Test | 1.2670410 | 117.20828 | 0.7909573 | 190.7509 | 1.3762349 | 0.0010373 |
| RandomWalker | 3 | EARTH | Test | 1.1522939 | 91.22548 | 0.7193257 | 168.6647 | 1.3024047 | NA |
| RandomWalker | 4 | NNAR | Test | 1.1158132 | 94.04563 | 0.6965525 | 170.4659 | 1.2442706 | 0.1412565 |

### Plot Models

``` r
nested_modeltime_tbl %>%
  extract_nested_test_forecast() %>%
  group_by(package) %>%
  plot_modeltime_forecast(
    .interactive = FALSE,
    .conf_interval_show  = FALSE,
    .facet_scales = "free"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
```

![](man/figures/README-model_plot-1.png)<!-- -->

### Best Model

``` r
best_nested_modeltime_tbl <- nested_modeltime_tbl %>%
  modeltime_nested_select_best(
    metric = "rmse",
    minimize = TRUE,
    filter_test_forecasts = TRUE
  )

best_nested_modeltime_tbl %>%
  extract_nested_best_model_report()
```

    ## # Nested Modeltime Table
    ## 

    ## # A tibble: 8 × 10
    ##   package      .model_id .model_desc .type   mae  mape  mase smape  rmse     rsq
    ##   <fct>            <int> <chr>       <chr> <dbl> <dbl> <dbl> <dbl> <dbl>   <dbl>
    ## 1 healthyR.da…         2 LM          Test  0.672 212.  0.769  135. 0.799 0.0295 
    ## 2 healthyR             4 NNAR        Test  0.703 214.  0.792  156. 0.839 0.0659 
    ## 3 healthyR.ts          4 NNAR        Test  0.861 106.  0.733  163. 1.09  0.0186 
    ## 4 healthyverse         2 LM          Test  0.647 281.  0.940  101. 0.765 0.0119 
    ## 5 healthyR.ai          4 NNAR        Test  0.674 144.  0.782  140. 0.823 0.0903 
    ## 6 TidyDensity          4 NNAR        Test  0.556 129.  0.644  123. 0.700 0.125  
    ## 7 tidyAML              2 LM          Test  0.653 273.  0.726  102. 0.777 0.00733
    ## 8 RandomWalker         4 NNAR        Test  1.12   94.0 0.697  170. 1.24  0.141

``` r
best_nested_modeltime_tbl %>%
  extract_nested_test_forecast() %>%
  #filter(!is.na(.model_id)) %>%
  group_by(package) %>%
  plot_modeltime_forecast(
    .interactive = FALSE,
    .conf_interval_alpha = 0.2,
    .facet_scales = "free"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
```

![](man/figures/README-best_model-1.png)<!-- -->

## Refitting and Future Forecast

Now that we have the best models, we can make our future forecasts.

``` r
nested_modeltime_refit_tbl <- best_nested_modeltime_tbl %>%
    modeltime_nested_refit(
        control = control_nested_refit(verbose = TRUE)
    )
```

``` r
nested_modeltime_refit_tbl
```

    ## # Nested Modeltime Table
    ## 

    ## # A tibble: 8 × 5
    ##   package       .actual_data .future_data .splits           .modeltime_tables 
    ##   <fct>         <list>       <list>       <list>            <list>            
    ## 1 healthyR.data <tibble>     <tibble>     <split [1550|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1543|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1487|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1458|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1282|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1133|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [741|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [163|28]>  <mdl_tm_t [1 × 5]>

``` r
nested_modeltime_refit_tbl %>%
  extract_nested_future_forecast() %>%
  mutate(across(.value:.conf_hi, .fns = ~ standard_inv_vec(
    x    = .,
    mean = std_mean,
    sd   = std_sd
  )$standard_inverse_value)) %>%
  mutate(across(.value:.conf_hi, .fns = ~ liiv(
    x = .,
    limit_lower = limit_lower,
    limit_upper = limit_upper,
    offset      = offset
  )$rescaled_v)) %>%
  group_by(package) %>%
  plot_modeltime_forecast(
    .interactive = FALSE,
    .conf_interval_alpha = 0.2,
    .facet_scales = "free"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
```

![](man/figures/README-refit-1.png)<!-- -->
