Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
21 April, 2025

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 137,677
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

The last day in the data set is 2025-04-19 19:56:05, the file was
birthed on: 2024-08-07 07:35:44, and at report knit time is -6128.34
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 137677        |
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
| r_version     |     99109 |          0.28 |   5 |   5 |     0 |       46 |          0 |
| r_arch        |     99109 |          0.28 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     99109 |          0.28 |   7 |  15 |     0 |       22 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       60 |          0 |
| country       |     11597 |          0.92 |   2 |   2 |     0 |      163 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2025-04-19 | 2023-06-06 | 1609 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1135475.4 | 1522302.54 | 355 | 14701 | 275137 | 2367773 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 10368.2 | 18402.57 | 1 | 298 | 3064 | 11666 | 209747 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2025-04-19 19:56:05 | 2023-06-06 04:06:55 | 83876 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |     median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-----------:|---------:|
| time          |         0 |             1 |   0 |  59 | 12H 7M 25S |       60 |

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
    ## -147.93  -35.56  -10.52   26.60  813.77 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -1.912e+02  7.244e+01
    ## date                                                1.159e-02  3.843e-03
    ## lag(value, 1)                                       1.052e-01  2.473e-02
    ## lag(value, 7)                                       9.655e-02  2.560e-02
    ## lag(value, 14)                                      9.603e-02  2.571e-02
    ## lag(value, 21)                                      6.896e-02  2.577e-02
    ## lag(value, 28)                                      6.438e-02  2.575e-02
    ## lag(value, 35)                                      6.683e-02  2.579e-02
    ## lag(value, 42)                                      4.915e-02  2.589e-02
    ## lag(value, 49)                                      6.880e-02  2.573e-02
    ## month(date, label = TRUE).L                        -1.038e+01  5.139e+00
    ## month(date, label = TRUE).Q                         2.608e+00  5.200e+00
    ## month(date, label = TRUE).C                        -1.210e+01  5.209e+00
    ## month(date, label = TRUE)^4                        -6.629e+00  5.200e+00
    ## month(date, label = TRUE)^5                        -1.246e+01  5.205e+00
    ## month(date, label = TRUE)^6                        -2.811e+00  5.261e+00
    ## month(date, label = TRUE)^7                        -6.315e+00  5.171e+00
    ## month(date, label = TRUE)^8                        -4.632e+00  5.175e+00
    ## month(date, label = TRUE)^9                         5.697e+00  5.185e+00
    ## month(date, label = TRUE)^10                        4.264e+00  5.264e+00
    ## month(date, label = TRUE)^11                       -5.740e+00  5.340e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.169e+01  2.382e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  8.134e+00  2.508e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.640 0.008386 ** 
    ## date                                                 3.017 0.002598 ** 
    ## lag(value, 1)                                        4.255 2.21e-05 ***
    ## lag(value, 7)                                        3.771 0.000169 ***
    ## lag(value, 14)                                       3.735 0.000195 ***
    ## lag(value, 21)                                       2.676 0.007532 ** 
    ## lag(value, 28)                                       2.501 0.012505 *  
    ## lag(value, 35)                                       2.591 0.009647 ** 
    ## lag(value, 42)                                       1.898 0.057847 .  
    ## lag(value, 49)                                       2.674 0.007577 ** 
    ## month(date, label = TRUE).L                         -2.020 0.043600 *  
    ## month(date, label = TRUE).Q                          0.502 0.616079    
    ## month(date, label = TRUE).C                         -2.323 0.020324 *  
    ## month(date, label = TRUE)^4                         -1.275 0.202573    
    ## month(date, label = TRUE)^5                         -2.393 0.016809 *  
    ## month(date, label = TRUE)^6                         -0.534 0.593155    
    ## month(date, label = TRUE)^7                         -1.221 0.222173    
    ## month(date, label = TRUE)^8                         -0.895 0.370864    
    ## month(date, label = TRUE)^9                          1.099 0.271988    
    ## month(date, label = TRUE)^10                         0.810 0.418048    
    ## month(date, label = TRUE)^11                        -1.075 0.282576    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -4.909 1.01e-06 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   3.243 0.001209 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 58.8 on 1537 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2472, Adjusted R-squared:  0.2364 
    ## F-statistic: 22.94 on 22 and 1537 DF,  p-value: < 2.2e-16

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
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.62865238143103"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 70, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.22171055110123"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 70, 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 3.22171055110123"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 70, 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 4.66915802391366"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 70, 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 4.66915802391366"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 70, 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.756083126376"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 70, 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.756083126376"

![](man/figures/README-nns_forecasting-1.png)<!-- -->

    ## Package: healthyR.ai
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.18348811761582"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 98 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.65653126096287"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 98, 84 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.64023331742762"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63, 98, 84 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.64023331742762"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63, 98, 84 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 2.84492201266937"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63, 98, 84 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 2.84492201266937"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63, 98, 84 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 2.63281331301472"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63, 98, 84 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 2.63281331301472"

![](man/figures/README-nns_forecasting-2.png)<!-- -->

    ## Package: healthyR.data
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 98 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.42240190347293"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 98, 91 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.07507163233103"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 98, 91, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 1.95575674035937"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 98, 91, 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 1.95575674035937"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 98, 91, 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 1.99766395480792"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 98, 91, 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 1.99766395480792"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 98, 91, 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 1.77342311919695"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 98, 91, 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 1.77342311919695"

![](man/figures/README-nns_forecasting-3.png)<!-- -->

    ## Package: healthyR.ts
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.57118400064636"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 70 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.13306274117035"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63, 70 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 3.13306274117035"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63, 70 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 4.2297186638465"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63, 70 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 4.2297186638465"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63, 70 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.50605357024754"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63, 70 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.50605357024754"

![](man/figures/README-nns_forecasting-4.png)<!-- -->

    ## Package: healthyverse
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 77 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 5.74757795646205"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 77 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 5.74757795646205"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 77 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 6.37870716312976"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 77 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 6.37870716312976"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 77 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 5.95609613197795"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 77 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 5.95609613197795"

![](man/figures/README-nns_forecasting-5.png)<!-- -->

    ## Package: RandomWalker
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.33592707485223"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21, 42 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.3272813417874"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 21, 42 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.3272813417874"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 21, 42 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 2.502051250484"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 21, 42 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 2.502051250484"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 21, 42 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 2.13140064808338"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 21, 42 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 2.13140064808338"

![](man/figures/README-nns_forecasting-6.png)<!-- -->

    ## Package: tidyAML
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 28 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.82942831167978"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 28 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 3.82942831167978"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 28 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 3.27163998862849"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 28 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 3.27163998862849"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 28 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.07344489013037"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 28 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.07344489013037"

![](man/figures/README-nns_forecasting-7.png)<!-- -->

    ## Package: TidyDensity
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.43782403189103"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 77 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.1978988649978"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63, 77 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 3.1978988649978"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63, 77 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 4.22039449127691"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63, 77 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 4.22039449127691"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63, 77 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.45500897281852"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63, 77 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.45500897281852"

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
    ## 1 healthyR.data <tibble [1,602 × 2]> <tibble [28 × 2]> <split [1574|28]>
    ## 2 healthyR      <tibble [1,595 × 2]> <tibble [28 × 2]> <split [1567|28]>
    ## 3 healthyR.ts   <tibble [1,539 × 2]> <tibble [28 × 2]> <split [1511|28]>
    ## 4 healthyverse  <tibble [1,510 × 2]> <tibble [28 × 2]> <split [1482|28]>
    ## 5 healthyR.ai   <tibble [1,334 × 2]> <tibble [28 × 2]> <split [1306|28]>
    ## 6 TidyDensity   <tibble [1,185 × 2]> <tibble [28 × 2]> <split [1157|28]>
    ## 7 tidyAML       <tibble [793 × 2]>   <tibble [28 × 2]> <split [765|28]> 
    ## 8 RandomWalker  <tibble [215 × 2]>   <tibble [28 × 2]> <split [187|28]>

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
| healthyR.data | 1 | ARIMA | Test | 0.8282466 | 121.14875 | 0.5912356 | 163.2199 | 0.9637545 | 0.0897437 |
| healthyR.data | 2 | LM | Test | 0.8094273 | 138.92859 | 0.5778016 | 139.1003 | 0.9405749 | 0.0781077 |
| healthyR.data | 3 | EARTH | Test | 0.9236767 | 209.82525 | 0.6593575 | 124.7073 | 1.1199561 | 0.0781077 |
| healthyR.data | 4 | NNAR | Test | 0.8125324 | 106.91266 | 0.5800182 | 171.5214 | 0.9861226 | 0.0006235 |
| healthyR | 1 | ARIMA | Test | 0.7481140 | 98.11032 | 0.6935773 | 178.2173 | 0.9148885 | 0.0009995 |
| healthyR | 2 | LM | Test | 0.7504316 | 99.10256 | 0.6957259 | 187.2110 | 0.9029831 | 0.0553082 |
| healthyR | 3 | EARTH | Test | 0.7514641 | 98.84152 | 0.6966832 | 174.9204 | 0.9008701 | 0.0553082 |
| healthyR | 4 | NNAR | Test | 0.7549771 | 96.60229 | 0.6999401 | 162.9783 | 0.9172019 | 0.1188697 |
| healthyR.ts | 1 | ARIMA | Test | 0.9812930 | 170.19683 | 0.7065733 | 144.7231 | 1.2137738 | 0.0213130 |
| healthyR.ts | 2 | LM | Test | 0.9904957 | 205.85131 | 0.7131997 | 131.4162 | 1.2666496 | 0.0213130 |
| healthyR.ts | 3 | EARTH | Test | 0.9905136 | 204.19166 | 0.7132126 | 132.1209 | 1.2636686 | NA |
| healthyR.ts | 4 | NNAR | Test | 0.9641111 | 97.14004 | 0.6942016 | 183.8117 | 1.1517242 | 0.0668985 |
| healthyverse | 1 | ARIMA | Test | 0.7412762 | 264.74821 | 0.7087576 | 118.5738 | 0.8998102 | 0.0042706 |
| healthyverse | 2 | LM | Test | 0.7404838 | 339.21503 | 0.7080000 | 110.1571 | 0.8859395 | 0.0355258 |
| healthyverse | 3 | EARTH | Test | 0.7227399 | 211.62686 | 0.6910345 | 121.6355 | 0.9124378 | 0.0355258 |
| healthyverse | 4 | NNAR | Test | 0.7263858 | 201.37441 | 0.6945204 | 123.3879 | 0.9227882 | 0.0036992 |
| healthyR.ai | 1 | ARIMA | Test | 0.7415527 | 116.54615 | 0.6880686 | 176.6081 | 0.9415609 | 0.0171156 |
| healthyR.ai | 2 | LM | Test | 0.7132993 | 134.92540 | 0.6618528 | 154.6148 | 0.8902618 | 0.0776937 |
| healthyR.ai | 3 | EARTH | Test | 0.7179862 | 122.77729 | 0.6662017 | 159.2691 | 0.8993478 | 0.0776937 |
| healthyR.ai | 4 | NNAR | Test | 0.7584943 | 166.87475 | 0.7037882 | 158.2526 | 0.9385678 | 0.0466446 |
| TidyDensity | 1 | ARIMA | Test | 0.6329254 | 219.20467 | 0.7193480 | 114.9780 | 0.7554149 | 0.1015262 |
| TidyDensity | 2 | LM | Test | 0.6489713 | 287.43907 | 0.7375849 | 101.5739 | 0.8293817 | 0.0879550 |
| TidyDensity | 3 | EARTH | Test | 0.6379554 | 214.72334 | 0.7250648 | 111.7777 | 0.7855461 | 0.0879550 |
| TidyDensity | 4 | NNAR | Test | 0.6692618 | 128.26888 | 0.7606459 | 145.5179 | 0.8032779 | 0.0387137 |
| tidyAML | 1 | ARIMA | Test | 0.6179896 | 245.43458 | 0.8217908 | 101.6993 | 0.7522832 | 0.1280254 |
| tidyAML | 2 | LM | Test | 0.6601393 | 285.48275 | 0.8778408 | 101.7749 | 0.8051071 | 0.0730963 |
| tidyAML | 3 | EARTH | Test | 0.6102432 | 159.66311 | 0.8114899 | 117.7815 | 0.7818353 | 0.0730963 |
| tidyAML | 4 | NNAR | Test | 0.6404186 | 226.73639 | 0.8516165 | 108.7267 | 0.7778622 | 0.0142590 |
| RandomWalker | 1 | ARIMA | Test | 1.2586207 | 111.27033 | 0.5691371 | 157.5218 | 1.5076887 | 0.0012944 |
| RandomWalker | 2 | LM | Test | 1.2825178 | 106.72844 | 0.5799432 | 185.3625 | 1.4953482 | 0.0591196 |
| RandomWalker | 3 | EARTH | Test | 1.2760011 | 114.15840 | 0.5769964 | 174.4325 | 1.4992893 | NA |
| RandomWalker | 4 | NNAR | Test | 1.3556628 | 129.04166 | 0.6130187 | 155.5676 | 1.6024926 | 0.0003735 |

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
    ## 1 healthyR.data         2 LM          Test  0.809 139.  0.578  139. 0.941 0.0781
    ## 2 healthyR              3 EARTH       Test  0.751  98.8 0.697  175. 0.901 0.0553
    ## 3 healthyR.ts           4 NNAR        Test  0.964  97.1 0.694  184. 1.15  0.0669
    ## 4 healthyverse          2 LM          Test  0.740 339.  0.708  110. 0.886 0.0355
    ## 5 healthyR.ai           2 LM          Test  0.713 135.  0.662  155. 0.890 0.0777
    ## 6 TidyDensity           1 ARIMA       Test  0.633 219.  0.719  115. 0.755 0.102 
    ## 7 tidyAML               1 ARIMA       Test  0.618 245.  0.822  102. 0.752 0.128 
    ## 8 RandomWalker          2 LM          Test  1.28  107.  0.580  185. 1.50  0.0591

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1574|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1567|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1511|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1482|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1306|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1157|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [765|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [187|28]>  <mdl_tm_t [1 × 5]>

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
