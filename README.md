Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
25 March, 2025

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 134,960
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

The last day in the data set is 2025-03-23 23:55:20, the file was
birthed on: 2024-08-07 07:35:44, and at report knit time is -5484.33
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 134960        |
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
| r_version     |     96960 |          0.28 |   5 |   5 |     0 |       46 |          0 |
| r_arch        |     96960 |          0.28 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     96960 |          0.28 |   7 |  15 |     0 |       21 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       60 |          0 |
| country       |     11359 |          0.92 |   2 |   2 |     0 |      163 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2025-03-23 | 2023-05-23 | 1582 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1135708.29 | 1525059.5 | 355 | 14701 | 261758 | 2367779 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 10365.15 | 18351.5 | 1 | 303 | 3077 | 11772 | 209747 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2025-03-23 23:55:20 | 2023-05-23 08:12:29 | 82006 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-------:|---------:|
| time          |         0 |             1 |   0 |  59 |   11.5 |       60 |

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
    ## -152.54  -35.18  -10.15   26.73  811.08 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -1.912e+02  7.422e+01
    ## date                                                1.152e-02  3.938e-03
    ## lag(value, 1)                                       1.106e-01  2.490e-02
    ## lag(value, 7)                                       9.278e-02  2.576e-02
    ## lag(value, 14)                                      9.664e-02  2.579e-02
    ## lag(value, 21)                                      6.504e-02  2.588e-02
    ## lag(value, 28)                                      6.100e-02  2.575e-02
    ## lag(value, 35)                                      6.965e-02  2.597e-02
    ## lag(value, 42)                                      5.249e-02  2.604e-02
    ## lag(value, 49)                                      8.483e-02  2.599e-02
    ## month(date, label = TRUE).L                        -1.100e+01  5.148e+00
    ## month(date, label = TRUE).Q                         2.541e+00  5.186e+00
    ## month(date, label = TRUE).C                        -1.189e+01  5.241e+00
    ## month(date, label = TRUE)^4                        -7.375e+00  5.199e+00
    ## month(date, label = TRUE)^5                        -1.228e+01  5.198e+00
    ## month(date, label = TRUE)^6                        -2.805e+00  5.277e+00
    ## month(date, label = TRUE)^7                        -6.737e+00  5.165e+00
    ## month(date, label = TRUE)^8                        -4.422e+00  5.195e+00
    ## month(date, label = TRUE)^9                         5.595e+00  5.253e+00
    ## month(date, label = TRUE)^10                        4.457e+00  5.295e+00
    ## month(date, label = TRUE)^11                       -5.974e+00  5.323e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.187e+01  2.392e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  7.944e+00  2.521e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.576 0.010098 *  
    ## date                                                 2.926 0.003484 ** 
    ## lag(value, 1)                                        4.444 9.49e-06 ***
    ## lag(value, 7)                                        3.601 0.000327 ***
    ## lag(value, 14)                                       3.747 0.000186 ***
    ## lag(value, 21)                                       2.513 0.012064 *  
    ## lag(value, 28)                                       2.369 0.017954 *  
    ## lag(value, 35)                                       2.682 0.007389 ** 
    ## lag(value, 42)                                       2.016 0.044000 *  
    ## lag(value, 49)                                       3.264 0.001122 ** 
    ## month(date, label = TRUE).L                         -2.136 0.032803 *  
    ## month(date, label = TRUE).Q                          0.490 0.624226    
    ## month(date, label = TRUE).C                         -2.268 0.023479 *  
    ## month(date, label = TRUE)^4                         -1.419 0.156178    
    ## month(date, label = TRUE)^5                         -2.363 0.018258 *  
    ## month(date, label = TRUE)^6                         -0.532 0.595064    
    ## month(date, label = TRUE)^7                         -1.305 0.192244    
    ## month(date, label = TRUE)^8                         -0.851 0.394817    
    ## month(date, label = TRUE)^9                          1.065 0.286967    
    ## month(date, label = TRUE)^10                         0.842 0.400126    
    ## month(date, label = TRUE)^11                        -1.122 0.261851    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -4.960 7.83e-07 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   3.151 0.001660 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 58.44 on 1510 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2565, Adjusted R-squared:  0.2457 
    ## F-statistic: 23.68 on 22 and 1510 DF,  p-value: < 2.2e-16

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
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.65203765955918"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 98 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.29555525141976"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 98, 77 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.27423532751044"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63, 98, 77 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.27423532751044"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63, 98, 77 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 2.02678856433709"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63, 98, 77 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 2.02678856433709"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63, 98, 77 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 1.56243726318324"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63, 98, 77 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 1.56243726318324"

![](man/figures/README-nns_forecasting-1.png)<!-- -->

    ## Package: healthyR.ai
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.29866494864091"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21, 49 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.1608234392989"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21, 49, 70 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.08962167261241"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21, 49, 70, 77 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.04678861598406"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21, 49, 70, 77, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.02149755567032"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21, 49, 70, 77, 63, 98 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.0141054393139"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 21, 49, 70, 77, 63, 98 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.0141054393139"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 21, 49, 70, 77, 63, 98 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 1.25195761718162"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 21, 49, 70, 77, 63, 98 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 1.25195761718162"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 21, 49, 70, 77, 63, 98 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 1.07017090346708"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 21, 49, 70, 77, 63, 98 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 1.07017090346708"

![](man/figures/README-nns_forecasting-2.png)<!-- -->

    ## Package: healthyR.data
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 98 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.23057380916866"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 98, 77 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.05721599027709"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 98, 77 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.05721599027709"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 98, 77 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 1.62465081028352"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 98, 77 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 1.62465081028352"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 98, 77 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 1.26674342430459"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 98, 77 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 1.26674342430459"

![](man/figures/README-nns_forecasting-3.png)<!-- -->

    ## Package: healthyR.ts
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.47385708813808"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 98 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 0.998258265285851"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63, 98 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 0.998258265285851"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63, 98 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 1.49268218677669"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63, 98 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 1.49268218677669"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63, 98 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 1.13831056904065"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63, 98 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 1.13831056904065"

![](man/figures/README-nns_forecasting-4.png)<!-- -->

    ## Package: healthyverse
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 49 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.57536143406016"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 49, 77 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.4540739361251"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 49, 77, 70 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.41222534120947"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 49, 77, 70 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.41222534120947"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 49, 77, 70 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 2.81447690910504"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 49, 77, 70 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 2.81447690910504"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 49, 77, 70 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 1.92441739854578"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 49, 77, 70 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 1.92441739854578"

![](man/figures/README-nns_forecasting-5.png)<!-- -->

    ## Package: RandomWalker
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 35 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 4.19950737006945"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 35 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 4.19950737006945"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 35 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 7.30272082036707"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 35 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 7.30272082036707"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 35 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 5.82576359750716"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 35 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 5.82576359750716"

![](man/figures/README-nns_forecasting-6.png)<!-- -->

    ## Package: tidyAML
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 56 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 4.11556373093916"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 56, 28 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.94994337451389"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 56, 28 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 3.94994337451389"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 56, 28 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 3.76998489079901"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 56, 28 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 3.76998489079901"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 56, 28 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.46447348194367"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 56, 28 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.46447348194367"

![](man/figures/README-nns_forecasting-7.png)<!-- -->

    ## Package: TidyDensity
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.66656162672833"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 77 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.38583709032771"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63, 77 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.38583709032771"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63, 77 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 2.29445031300988"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63, 77 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 2.29445031300988"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63, 77 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 1.6602138613908"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63, 77 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 1.6602138613908"

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
    ## 1 healthyR.data <tibble [1,575 × 2]> <tibble [28 × 2]> <split [1547|28]>
    ## 2 healthyR      <tibble [1,568 × 2]> <tibble [28 × 2]> <split [1540|28]>
    ## 3 healthyR.ts   <tibble [1,512 × 2]> <tibble [28 × 2]> <split [1484|28]>
    ## 4 healthyverse  <tibble [1,483 × 2]> <tibble [28 × 2]> <split [1455|28]>
    ## 5 healthyR.ai   <tibble [1,307 × 2]> <tibble [28 × 2]> <split [1279|28]>
    ## 6 TidyDensity   <tibble [1,158 × 2]> <tibble [28 × 2]> <split [1130|28]>
    ## 7 tidyAML       <tibble [766 × 2]>   <tibble [28 × 2]> <split [738|28]> 
    ## 8 RandomWalker  <tibble [188 × 2]>   <tibble [28 × 2]> <split [160|28]>

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
| healthyR.data | 1 | ARIMA | Test | 0.6565796 | 116.59599 | 0.7087049 | 163.14431 | 0.8560299 | 0.1235540 |
| healthyR.data | 2 | LM | Test | 0.6796253 | 205.84906 | 0.7335802 | 134.14130 | 0.8176252 | 0.0010151 |
| healthyR.data | 3 | EARTH | Test | 0.9702192 | 278.08628 | 1.0472442 | 149.35384 | 1.1956195 | 0.0010151 |
| healthyR.data | 4 | NNAR | Test | 0.6705705 | 98.63562 | 0.7238065 | 157.81912 | 0.8897434 | 0.0501536 |
| healthyR | 1 | ARIMA | Test | 0.6890054 | 174.24053 | 0.7935496 | 161.47950 | 0.8158807 | 0.0360881 |
| healthyR | 2 | LM | Test | 0.6272830 | 101.38875 | 0.7224619 | 183.75339 | 0.7852797 | 0.0132129 |
| healthyR | 3 | EARTH | Test | 1.3285323 | 498.96043 | 1.5301132 | 155.38119 | 1.5053280 | 0.0132129 |
| healthyR | 4 | NNAR | Test | 0.6283184 | 217.26022 | 0.7236545 | 158.48407 | 0.7577485 | 0.0717530 |
| healthyR.ts | 1 | ARIMA | Test | 0.9415448 | 390.63688 | 0.7667893 | 132.97745 | 1.1397569 | 0.0313140 |
| healthyR.ts | 2 | LM | Test | 0.8940678 | 321.84444 | 0.7281242 | 134.60365 | 1.0974871 | 0.0313140 |
| healthyR.ts | 3 | EARTH | Test | 0.9112286 | 349.82888 | 0.7421000 | 133.58187 | 1.1132884 | 0.0313140 |
| healthyR.ts | 4 | NNAR | Test | 0.8140338 | 91.85651 | 0.6629450 | 149.70367 | 1.0617763 | 0.0238798 |
| healthyverse | 1 | ARIMA | Test | 0.6299406 | 194.68351 | 0.9199621 | 110.03371 | 0.7596636 | 0.0109891 |
| healthyverse | 2 | LM | Test | 0.6282391 | 274.27691 | 0.9174773 | 96.70843 | 0.7362152 | 0.0079292 |
| healthyverse | 3 | EARTH | Test | 0.6245993 | 160.32605 | 0.9121616 | 108.86171 | 0.7813035 | 0.0079292 |
| healthyverse | 4 | NNAR | Test | 0.6156948 | 157.65515 | 0.8991576 | 108.86658 | 0.7749583 | 0.0623742 |
| healthyR.ai | 1 | ARIMA | Test | 0.7592044 | 132.58795 | 0.8424575 | 180.06270 | 0.8774807 | 0.0999979 |
| healthyR.ai | 2 | LM | Test | 0.6916956 | 104.82872 | 0.7675458 | 139.21493 | 0.8564570 | 0.0000071 |
| healthyR.ai | 3 | EARTH | Test | 1.8473839 | 843.51805 | 2.0499650 | 158.70978 | 2.0574101 | 0.0000071 |
| healthyR.ai | 4 | NNAR | Test | 0.6952361 | 125.29263 | 0.7714745 | 140.63123 | 0.8235728 | 0.1138667 |
| TidyDensity | 1 | ARIMA | Test | 0.6531213 | 247.52642 | 0.7867225 | 111.41476 | 0.8065647 | 0.0001124 |
| TidyDensity | 2 | LM | Test | 0.6816525 | 295.23654 | 0.8210899 | 110.77372 | 0.8369230 | 0.0330505 |
| TidyDensity | 3 | EARTH | Test | 0.6305063 | 222.25821 | 0.7594813 | 113.86294 | 0.7789545 | 0.0330505 |
| TidyDensity | 4 | NNAR | Test | 0.6132933 | 153.82800 | 0.7387473 | 132.50941 | 0.7760688 | 0.0349705 |
| tidyAML | 1 | ARIMA | Test | 0.6617345 | 270.46265 | 0.7658134 | 102.35723 | 0.7913574 | 0.0019970 |
| tidyAML | 2 | LM | Test | 0.6432591 | 269.94911 | 0.7444322 | 97.97285 | 0.7565889 | 0.0089875 |
| tidyAML | 3 | EARTH | Test | 0.6610224 | 115.71780 | 0.7649893 | 122.65866 | 0.8591560 | 0.0089875 |
| tidyAML | 4 | NNAR | Test | 0.6336836 | 228.64836 | 0.7333505 | 104.03267 | 0.7622930 | 0.0072458 |
| RandomWalker | 1 | ARIMA | Test | 0.9415180 | 111.69127 | 0.5786079 | 89.80395 | 1.3393827 | 0.0985831 |
| RandomWalker | 2 | LM | Test | 1.2138154 | 118.16185 | 0.7459477 | 190.67407 | 1.3190336 | 0.0037389 |
| RandomWalker | 3 | EARTH | Test | 1.0968298 | 90.67833 | 0.6740545 | 166.81853 | 1.2486564 | NA |
| RandomWalker | 4 | NNAR | Test | 1.6761239 | 291.91288 | 1.0300584 | 156.63613 | 2.0174300 | 0.0189678 |

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
    ##   package     .model_id .model_desc .type   mae  mape  mase smape  rmse      rsq
    ##   <fct>           <int> <chr>       <chr> <dbl> <dbl> <dbl> <dbl> <dbl>    <dbl>
    ## 1 healthyR.d…         2 LM          Test  0.680 206.  0.734 134.  0.818  0.00102
    ## 2 healthyR            4 NNAR        Test  0.628 217.  0.724 158.  0.758  0.0718 
    ## 3 healthyR.ts         4 NNAR        Test  0.814  91.9 0.663 150.  1.06   0.0239 
    ## 4 healthyver…         2 LM          Test  0.628 274.  0.917  96.7 0.736  0.00793
    ## 5 healthyR.ai         4 NNAR        Test  0.695 125.  0.771 141.  0.824  0.114  
    ## 6 TidyDensity         4 NNAR        Test  0.613 154.  0.739 133.  0.776  0.0350 
    ## 7 tidyAML             2 LM          Test  0.643 270.  0.744  98.0 0.757  0.00899
    ## 8 RandomWalk…         3 EARTH       Test  1.10   90.7 0.674 167.  1.25  NA

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1547|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1540|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1484|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1455|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1279|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1130|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [738|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [160|28]>  <mdl_tm_t [1 × 5]>

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
