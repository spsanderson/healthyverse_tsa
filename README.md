Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
23 April, 2025

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 137,825
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

The last day in the data set is 2025-04-21 23:43:01, the file was
birthed on: 2024-08-07 07:35:44, and at report knit time is -6180.12
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 137825        |
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
| r_version     |     99223 |          0.28 |   5 |   5 |     0 |       46 |          0 |
| r_arch        |     99223 |          0.28 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     99223 |          0.28 |   7 |  15 |     0 |       22 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       60 |          0 |
| country       |     11610 |          0.92 |   2 |   2 |     0 |      163 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2025-04-21 | 2023-06-08 | 1611 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1135127.52 | 1522003.28 | 355 | 14701 | 275341 | 2367771 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 10365.65 | 18401.19 | 1 | 299 | 3061 | 11655 | 209747 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2025-04-21 23:43:01 | 2023-06-08 02:12:04 | 83961 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |     median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-----------:|---------:|
| time          |         0 |             1 |   0 |  59 | 12H 7M 28S |       60 |

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
    ## -147.97  -35.50  -10.49   26.57  813.67 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -190.65428   72.19724
    ## date                                                  0.01156    0.00383
    ## lag(value, 1)                                         0.10527    0.02470
    ## lag(value, 7)                                         0.09680    0.02559
    ## lag(value, 14)                                        0.09609    0.02570
    ## lag(value, 21)                                        0.06882    0.02575
    ## lag(value, 28)                                        0.06462    0.02573
    ## lag(value, 35)                                        0.06693    0.02576
    ## lag(value, 42)                                        0.04907    0.02587
    ## lag(value, 49)                                        0.06880    0.02572
    ## month(date, label = TRUE).L                         -10.36727    5.13437
    ## month(date, label = TRUE).Q                           2.61725    5.19621
    ## month(date, label = TRUE).C                         -12.11711    5.20033
    ## month(date, label = TRUE)^4                          -6.62000    5.19723
    ## month(date, label = TRUE)^5                         -12.44504    5.20005
    ## month(date, label = TRUE)^6                          -2.82121    5.25359
    ## month(date, label = TRUE)^7                          -6.30914    5.16750
    ## month(date, label = TRUE)^8                          -4.61642    5.16966
    ## month(date, label = TRUE)^9                           5.67905    5.17491
    ## month(date, label = TRUE)^10                          4.27735    5.25563
    ## month(date, label = TRUE)^11                         -5.74688    5.33547
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -11.71454    2.38039
    ## fourier_vec(date, type = "cos", K = 1, period = 7)    8.13070    2.50683
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.641 0.008356 ** 
    ## date                                                 3.019 0.002578 ** 
    ## lag(value, 1)                                        4.261 2.16e-05 ***
    ## lag(value, 7)                                        3.783 0.000161 ***
    ## lag(value, 14)                                       3.739 0.000191 ***
    ## lag(value, 21)                                       2.673 0.007605 ** 
    ## lag(value, 28)                                       2.511 0.012124 *  
    ## lag(value, 35)                                       2.598 0.009477 ** 
    ## lag(value, 42)                                       1.897 0.058021 .  
    ## lag(value, 49)                                       2.675 0.007548 ** 
    ## month(date, label = TRUE).L                         -2.019 0.043641 *  
    ## month(date, label = TRUE).Q                          0.504 0.614556    
    ## month(date, label = TRUE).C                         -2.330 0.019931 *  
    ## month(date, label = TRUE)^4                         -1.274 0.202942    
    ## month(date, label = TRUE)^5                         -2.393 0.016819 *  
    ## month(date, label = TRUE)^6                         -0.537 0.591341    
    ## month(date, label = TRUE)^7                         -1.221 0.222301    
    ## month(date, label = TRUE)^8                         -0.893 0.372006    
    ## month(date, label = TRUE)^9                          1.097 0.272629    
    ## month(date, label = TRUE)^10                         0.814 0.415850    
    ## month(date, label = TRUE)^11                        -1.077 0.281600    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -4.921 9.52e-07 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   3.243 0.001206 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 58.77 on 1539 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2474, Adjusted R-squared:  0.2366 
    ## F-statistic: 22.99 on 22 and 1539 DF,  p-value: < 2.2e-16

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
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 70 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.665750411821"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 70, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.25544313352503"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 70, 63, 91 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.22888272867836"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 70, 63, 91 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 3.22888272867836"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 70, 63, 91 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 4.93600923403795"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 70, 63, 91 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 4.93600923403795"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 70, 63, 91 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.94633655605772"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 70, 63, 91 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.94633655605772"

![](man/figures/README-nns_forecasting-1.png)<!-- -->

    ## Package: healthyR.ai
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 28 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.12797772480728"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 28, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.8169839980933"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 28, 63, 70 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.77246141429321"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 28, 63, 70, 91 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.7164496330328"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 28, 63, 70, 91, 35 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.68545838071099"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 28, 63, 70, 91, 35, 84 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.66042687832714"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 28, 63, 70, 91, 35, 84 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.66042687832714"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 28, 63, 70, 91, 35, 84 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 3.78315844060351"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 28, 63, 70, 91, 35, 84 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 3.78315844060351"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 28, 63, 70, 91, 35, 84 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.1042176698188"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 28, 63, 70, 91, 35, 84 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.1042176698188"

![](man/figures/README-nns_forecasting-2.png)<!-- -->

    ## Package: healthyR.data
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 91 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.32569373569906"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 91, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.73840080227718"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 91, 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.73840080227718"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 91, 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 3.65514621752408"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 91, 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 3.65514621752408"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 91, 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.00458482877121"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 91, 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.00458482877121"

![](man/figures/README-nns_forecasting-3.png)<!-- -->

    ## Package: healthyR.ts
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.61788859276391"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 70 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.1078610580744"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63, 70 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 3.1078610580744"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63, 70 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 4.34052496092188"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63, 70 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 4.34052496092188"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63, 70 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.53410813857656"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63, 70 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.53410813857656"

![](man/figures/README-nns_forecasting-4.png)<!-- -->

    ## Package: healthyverse
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 77 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 5.44234866575951"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 77 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 5.44234866575951"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 77 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 6.34707621072757"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 77 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 6.34707621072757"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 77 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 5.78440922802498"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 77 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 5.78440922802498"

![](man/figures/README-nns_forecasting-5.png)<!-- -->

    ## Package: RandomWalker
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 42 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.65718948814561"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 42 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.65718948814561"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 42 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 3.37362817350544"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 42 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 3.37362817350544"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 42 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 2.75059568512984"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 42 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 2.75059568512984"

![](man/figures/README-nns_forecasting-6.png)<!-- -->

    ## Package: tidyAML
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 28 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.92800987766783"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 28 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 3.92800987766783"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 28 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 3.53329712682051"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 28 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 3.53329712682051"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 28 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.27519357160424"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 28 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.27519357160424"

![](man/figures/README-nns_forecasting-7.png)<!-- -->

    ## Package: TidyDensity
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.28478635106611"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 77 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.11229550190236"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63, 77 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 3.11229550190236"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63, 77 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 4.2879713162462"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63, 77 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 4.2879713162462"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63, 77 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.40994262438516"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63, 77 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.40994262438516"

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
    ## 1 healthyR.data <tibble [1,604 × 2]> <tibble [28 × 2]> <split [1576|28]>
    ## 2 healthyR      <tibble [1,597 × 2]> <tibble [28 × 2]> <split [1569|28]>
    ## 3 healthyR.ts   <tibble [1,541 × 2]> <tibble [28 × 2]> <split [1513|28]>
    ## 4 healthyverse  <tibble [1,512 × 2]> <tibble [28 × 2]> <split [1484|28]>
    ## 5 healthyR.ai   <tibble [1,336 × 2]> <tibble [28 × 2]> <split [1308|28]>
    ## 6 TidyDensity   <tibble [1,187 × 2]> <tibble [28 × 2]> <split [1159|28]>
    ## 7 tidyAML       <tibble [795 × 2]>   <tibble [28 × 2]> <split [767|28]> 
    ## 8 RandomWalker  <tibble [217 × 2]>   <tibble [28 × 2]> <split [189|28]>

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
| healthyR.data | 1 | ARIMA | Test | 0.8749805 | 105.38697 | 0.6116600 | 167.03456 | 1.0172655 | 0.1001000 |
| healthyR.data | 2 | LM | Test | 0.8500482 | 130.84838 | 0.5942310 | 139.65335 | 0.9760987 | 0.0570560 |
| healthyR.data | 3 | EARTH | Test | 0.8709442 | 143.24608 | 0.6088384 | 137.13730 | 0.9976900 | 0.0570560 |
| healthyR.data | 4 | NNAR | Test | 0.8678799 | 102.12109 | 0.6066963 | 178.92439 | 1.0388451 | 0.0355864 |
| healthyR | 1 | ARIMA | Test | 0.7748474 | 99.74889 | 0.7587960 | 179.01768 | 0.9596884 | 0.0177412 |
| healthyR | 2 | LM | Test | 0.7736899 | 98.21822 | 0.7576625 | 185.76259 | 0.9326958 | 0.0420290 |
| healthyR | 3 | EARTH | Test | 0.7714025 | 96.09463 | 0.7554225 | 172.03694 | 0.9281278 | 0.0420290 |
| healthyR | 4 | NNAR | Test | 0.7695991 | 94.96680 | 0.7536564 | 162.94557 | 0.9264762 | 0.2424144 |
| healthyR.ts | 1 | ARIMA | Test | 1.0064309 | 137.90768 | 0.7067599 | 147.44524 | 1.2258807 | 0.0284452 |
| healthyR.ts | 2 | LM | Test | 1.0260690 | 160.95287 | 0.7205507 | 135.82217 | 1.2833485 | 0.0284452 |
| healthyR.ts | 3 | EARTH | Test | 1.0252324 | 159.47089 | 0.7199632 | 136.48372 | 1.2797514 | NA |
| healthyR.ts | 4 | NNAR | Test | 0.9727721 | 96.28607 | 0.6831232 | 182.89049 | 1.1558075 | 0.0247227 |
| healthyverse | 1 | ARIMA | Test | 0.7773273 | 218.35244 | 0.7724732 | 119.67585 | 0.9378720 | 0.0033012 |
| healthyverse | 2 | LM | Test | 0.7317597 | 302.84371 | 0.7271901 | 102.42228 | 0.8770384 | 0.0402219 |
| healthyverse | 3 | EARTH | Test | 0.7603461 | 188.80817 | 0.7555980 | 119.17785 | 0.9463290 | 0.0402219 |
| healthyverse | 4 | NNAR | Test | 0.7707616 | 173.15959 | 0.7659485 | 122.97023 | 0.9743517 | 0.0521472 |
| healthyR.ai | 1 | ARIMA | Test | 0.7909087 | 114.02414 | 0.7078348 | 187.54118 | 0.9829612 | 0.0691509 |
| healthyR.ai | 2 | LM | Test | 0.7518898 | 134.04387 | 0.6729144 | 157.41701 | 0.9211785 | 0.0514326 |
| healthyR.ai | 3 | EARTH | Test | 0.7566326 | 125.48623 | 0.6771590 | 161.68808 | 0.9286777 | 0.0514326 |
| healthyR.ai | 4 | NNAR | Test | 0.7919207 | 170.50235 | 0.7087406 | 152.18872 | 0.9655787 | 0.0567771 |
| TidyDensity | 1 | ARIMA | Test | 0.6249633 | 213.72995 | 0.7350420 | 115.44392 | 0.7442529 | 0.0386207 |
| TidyDensity | 2 | LM | Test | 0.6380214 | 284.77744 | 0.7504001 | 103.31558 | 0.8003454 | 0.0063344 |
| TidyDensity | 3 | EARTH | Test | 0.6288339 | 207.01874 | 0.7395943 | 115.11653 | 0.7574044 | 0.0063344 |
| TidyDensity | 4 | NNAR | Test | 0.6428300 | 129.71854 | 0.7560557 | 142.15658 | 0.7773534 | 0.0483877 |
| tidyAML | 1 | ARIMA | Test | 0.5983858 | 242.51644 | 0.8410854 | 101.58593 | 0.7175750 | 0.1154988 |
| tidyAML | 2 | LM | Test | 0.6233129 | 277.34313 | 0.8761228 | 98.28852 | 0.7772732 | 0.0174091 |
| tidyAML | 3 | EARTH | Test | 0.5770097 | 185.36877 | 0.8110394 | 106.54211 | 0.7335999 | 0.0174091 |
| tidyAML | 4 | NNAR | Test | 0.6002536 | 218.74359 | 0.8437108 | 107.36514 | 0.7280595 | 0.0381219 |
| RandomWalker | 1 | ARIMA | Test | 1.3818797 | 139.28446 | 0.6465307 | 151.13454 | 1.6441726 | 0.0381863 |
| RandomWalker | 2 | LM | Test | 1.3183898 | 102.10895 | 0.6168261 | 193.72772 | 1.5190825 | 0.1062211 |
| RandomWalker | 3 | EARTH | Test | 1.3088147 | 112.79732 | 0.6123463 | 177.09506 | 1.5232013 | NA |
| RandomWalker | 4 | NNAR | Test | 1.3401347 | 225.39552 | 0.6269997 | 151.50869 | 1.5487856 | 0.0200514 |

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
    ##   package       .model_id .model_desc .type   mae  mape  mase smape  rmse    rsq
    ##   <fct>             <int> <chr>       <chr> <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl>
    ## 1 healthyR.data         2 LM          Test  0.850 131.  0.594  140. 0.976 0.0571
    ## 2 healthyR              4 NNAR        Test  0.770  95.0 0.754  163. 0.926 0.242 
    ## 3 healthyR.ts           4 NNAR        Test  0.973  96.3 0.683  183. 1.16  0.0247
    ## 4 healthyverse          2 LM          Test  0.732 303.  0.727  102. 0.877 0.0402
    ## 5 healthyR.ai           2 LM          Test  0.752 134.  0.673  157. 0.921 0.0514
    ## 6 TidyDensity           1 ARIMA       Test  0.625 214.  0.735  115. 0.744 0.0386
    ## 7 tidyAML               1 ARIMA       Test  0.598 243.  0.841  102. 0.718 0.115 
    ## 8 RandomWalker          2 LM          Test  1.32  102.  0.617  194. 1.52  0.106

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1576|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1569|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1513|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1484|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1308|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1159|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [767|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [189|28]>  <mdl_tm_t [1 × 5]>

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
