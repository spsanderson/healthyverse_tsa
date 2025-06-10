Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
10 June, 2025

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 141,874
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

The last day in the data set is 2025-06-08 23:16:00, the file was
birthed on: 2024-08-07 07:35:44, and at report knit time is -7331.67
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 141874        |
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
| r_version     |    102331 |          0.28 |   5 |   5 |     0 |       47 |          0 |
| r_arch        |    102331 |          0.28 |   3 |   7 |     0 |        5 |          0 |
| r_os          |    102331 |          0.28 |   7 |  15 |     0 |       22 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       60 |          0 |
| country       |     12066 |          0.91 |   2 |   2 |     0 |      163 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2025-06-08 | 2023-07-01 | 1659 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1133618.08 | 1517147.84 | 355 | 14701.0 | 289703 | 2367735 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 10430.15 | 18577.34 | 1 | 282.5 | 3039 | 11666 | 209747 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2025-06-08 23:16:00 | 2023-07-01 07:24:30 | 86917 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-------:|---------:|
| time          |         0 |             1 |   0 |  59 |     41 |       60 |

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
    ## -146.68  -35.80  -10.98   26.82  814.90 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -1.534e+02  6.793e+01
    ## date                                                9.613e-03  3.598e-03
    ## lag(value, 1)                                       1.050e-01  2.432e-02
    ## lag(value, 7)                                       9.657e-02  2.520e-02
    ## lag(value, 14)                                      8.920e-02  2.519e-02
    ## lag(value, 21)                                      6.880e-02  2.526e-02
    ## lag(value, 28)                                      7.097e-02  2.515e-02
    ## lag(value, 35)                                      6.721e-02  2.525e-02
    ## lag(value, 42)                                      5.083e-02  2.536e-02
    ## lag(value, 49)                                      6.925e-02  2.524e-02
    ## month(date, label = TRUE).L                        -9.564e+00  5.104e+00
    ## month(date, label = TRUE).Q                         4.221e+00  5.110e+00
    ## month(date, label = TRUE).C                        -1.353e+01  5.127e+00
    ## month(date, label = TRUE)^4                        -7.471e+00  5.159e+00
    ## month(date, label = TRUE)^5                        -1.090e+01  5.111e+00
    ## month(date, label = TRUE)^6                        -3.146e+00  5.205e+00
    ## month(date, label = TRUE)^7                        -7.696e+00  5.087e+00
    ## month(date, label = TRUE)^8                        -3.654e+00  5.091e+00
    ## month(date, label = TRUE)^9                         6.170e+00  5.099e+00
    ## month(date, label = TRUE)^10                        3.141e+00  5.074e+00
    ## month(date, label = TRUE)^11                       -5.081e+00  5.181e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.148e+01  2.328e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  7.981e+00  2.447e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.258 0.024090 *  
    ## date                                                 2.672 0.007627 ** 
    ## lag(value, 1)                                        4.318 1.67e-05 ***
    ## lag(value, 7)                                        3.833 0.000132 ***
    ## lag(value, 14)                                       3.541 0.000409 ***
    ## lag(value, 21)                                       2.723 0.006534 ** 
    ## lag(value, 28)                                       2.822 0.004837 ** 
    ## lag(value, 35)                                       2.662 0.007844 ** 
    ## lag(value, 42)                                       2.004 0.045227 *  
    ## lag(value, 49)                                       2.743 0.006150 ** 
    ## month(date, label = TRUE).L                         -1.874 0.061141 .  
    ## month(date, label = TRUE).Q                          0.826 0.408934    
    ## month(date, label = TRUE).C                         -2.640 0.008374 ** 
    ## month(date, label = TRUE)^4                         -1.448 0.147786    
    ## month(date, label = TRUE)^5                         -2.133 0.033064 *  
    ## month(date, label = TRUE)^6                         -0.605 0.545596    
    ## month(date, label = TRUE)^7                         -1.513 0.130523    
    ## month(date, label = TRUE)^8                         -0.718 0.473000    
    ## month(date, label = TRUE)^9                          1.210 0.226464    
    ## month(date, label = TRUE)^10                         0.619 0.535989    
    ## month(date, label = TRUE)^11                        -0.981 0.326979    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -4.932 8.98e-07 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   3.261 0.001133 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 58.67 on 1587 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2395, Adjusted R-squared:  0.2289 
    ## F-statistic: 22.71 on 22 and 1587 DF,  p-value: < 2.2e-16

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
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.79140731500857"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.48243134553398"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21, 63, 91 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.4421847761676"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 21, 63, 91 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.4421847761676"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 21, 63, 91 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 4.68269722879261"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 21, 63, 91 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 4.68269722879261"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 21, 63, 91 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.15267446653991"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 21, 63, 91 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.15267446653991"

![](man/figures/README-nns_forecasting-1.png)<!-- -->

    ## Package: healthyR.ai
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.7142412680637"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 3.7142412680637"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 4.13038253015662"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 4.13038253015662"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.78864965124826"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.78864965124826"

![](man/figures/README-nns_forecasting-2.png)<!-- -->

    ## Package: healthyR.data
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.88117385802843"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 91 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.29486004826429"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 91, 84 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.07930491361838"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63, 91, 84 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.07930491361838"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63, 91, 84 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 2.38499090073459"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63, 91, 84 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 2.38499090073459"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63, 91, 84 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 2.08644080642745"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63, 91, 84 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 2.08644080642745"

![](man/figures/README-nns_forecasting-3.png)<!-- -->

    ## Package: healthyR.ts
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.19394558910844"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21, 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.83559077695956"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 21, 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.83559077695956"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 21, 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 4.76669755593727"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 21, 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 4.76669755593727"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 21, 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.14242709951645"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 21, 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.14242709951645"

![](man/figures/README-nns_forecasting-4.png)<!-- -->

    ## Package: healthyverse
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 56 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 4.76604153782689"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 56, 28 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 4.53547611678524"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 56, 28 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 4.53547611678524"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 56, 28 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 8.70828197374006"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 56, 28 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 8.70828197374006"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 56, 28 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 6.14573213403132"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 56, 28 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 6.14573213403132"

![](man/figures/README-nns_forecasting-5.png)<!-- -->

    ## Package: RandomWalker
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.28236391675071"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 3.28236391675071"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 3.24535712113871"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 3.24535712113871"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.08230291040381"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.08230291040381"

![](man/figures/README-nns_forecasting-6.png)<!-- -->

    ## Package: tidyAML
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 84 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 6.15134984591896"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 84, 91 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 6.11062192112897"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 84, 91 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 6.11062192112897"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 84, 91 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 4.09261708639387"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 84, 91 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 4.09261708639387"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 84, 91 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 4.79580286916397"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 84, 91 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 4.79580286916397"

![](man/figures/README-nns_forecasting-7.png)<!-- -->

    ## Package: TidyDensity
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 4.43591182045338"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 21, 56 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 4.03690366306092"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 21, 56 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 4.03690366306092"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 21, 56 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 6.85133611457817"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 21, 56 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 6.85133611457817"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 21, 56 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 4.95709663781472"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 21, 56 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 4.95709663781472"

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
    ## 1 healthyR.data <tibble [1,651 × 2]> <tibble [28 × 2]> <split [1623|28]>
    ## 2 healthyR      <tibble [1,645 × 2]> <tibble [28 × 2]> <split [1617|28]>
    ## 3 healthyR.ts   <tibble [1,589 × 2]> <tibble [28 × 2]> <split [1561|28]>
    ## 4 healthyverse  <tibble [1,559 × 2]> <tibble [28 × 2]> <split [1531|28]>
    ## 5 healthyR.ai   <tibble [1,384 × 2]> <tibble [28 × 2]> <split [1356|28]>
    ## 6 TidyDensity   <tibble [1,235 × 2]> <tibble [28 × 2]> <split [1207|28]>
    ## 7 tidyAML       <tibble [843 × 2]>   <tibble [28 × 2]> <split [815|28]> 
    ## 8 RandomWalker  <tibble [265 × 2]>   <tibble [28 × 2]> <split [237|28]>

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
| healthyR.data | 1 | ARIMA | Test | 0.5788670 | 102.41843 | 0.6606647 | 125.39623 | 0.7950100 | 0.1956641 |
| healthyR.data | 2 | LM | Test | 0.6294210 | 172.71570 | 0.7183624 | 118.43846 | 0.7912200 | 0.0000441 |
| healthyR.data | 3 | EARTH | Test | 0.6532281 | 120.25189 | 0.7455335 | 141.64828 | 0.8634093 | 0.0000441 |
| healthyR.data | 4 | NNAR | Test | 0.7287047 | 97.74071 | 0.8316755 | 166.14588 | 0.9803347 | 0.0219592 |
| healthyR | 1 | ARIMA | Test | 0.6538767 | 142.85458 | 0.7892525 | 160.23938 | 0.8116974 | 0.0424791 |
| healthyR | 2 | LM | Test | 0.6404637 | 98.37104 | 0.7730625 | 166.56356 | 0.8263484 | 0.0596509 |
| healthyR | 3 | EARTH | Test | 0.6081729 | 137.14926 | 0.7340863 | 135.14037 | 0.7522805 | 0.0596509 |
| healthyR | 4 | NNAR | Test | 0.6588434 | 151.37274 | 0.7952475 | 162.95236 | 0.8148584 | 0.0218853 |
| healthyR.ts | 1 | ARIMA | Test | 0.6031580 | 103.38064 | 0.7691389 | 140.88406 | 0.7518672 | 0.0606399 |
| healthyR.ts | 2 | LM | Test | 0.8865581 | 161.65144 | 1.1305268 | 154.63334 | 1.0991159 | 0.0606399 |
| healthyR.ts | 3 | EARTH | Test | 0.5328433 | 160.64207 | 0.6794746 | 90.15680 | 0.6903619 | 0.0606399 |
| healthyR.ts | 4 | NNAR | Test | 0.7342210 | 105.26485 | 0.9362687 | 177.26888 | 0.9338336 | 0.0000261 |
| healthyverse | 1 | ARIMA | Test | 0.5875948 | 157.86960 | 1.1339194 | 76.94388 | 0.7356160 | 0.0586233 |
| healthyverse | 2 | LM | Test | 0.5594740 | 148.02459 | 1.0796530 | 74.17554 | 0.6871867 | 0.0819579 |
| healthyverse | 3 | EARTH | Test | 0.5574495 | 144.65535 | 1.0757462 | 74.18406 | 0.6882926 | 0.0819579 |
| healthyverse | 4 | NNAR | Test | 0.6331090 | 109.12670 | 1.2217511 | 88.05994 | 0.8242053 | 0.0762671 |
| healthyR.ai | 1 | ARIMA | Test | 0.5817534 | 96.99079 | 0.8255798 | 146.90503 | 0.7635293 | 0.0110637 |
| healthyR.ai | 2 | LM | Test | 0.5508990 | 92.29046 | 0.7817936 | 124.92799 | 0.7155676 | 0.0255384 |
| healthyR.ai | 3 | EARTH | Test | 1.9039625 | 912.70056 | 2.7019576 | 127.23065 | 2.1695434 | 0.0255384 |
| healthyR.ai | 4 | NNAR | Test | 0.5643685 | 135.70314 | 0.8009084 | 124.32003 | 0.7357749 | 0.0945858 |
| TidyDensity | 1 | ARIMA | Test | 0.4738714 | 150.86766 | 1.0926990 | 111.98066 | 0.6285118 | 0.0007749 |
| TidyDensity | 2 | LM | Test | 0.6424508 | 263.25083 | 1.4814259 | 118.90324 | 0.8123362 | 0.0565334 |
| TidyDensity | 3 | EARTH | Test | 0.4763075 | 155.69279 | 1.0983165 | 110.79442 | 0.6269962 | 0.0565334 |
| TidyDensity | 4 | NNAR | Test | 0.4336321 | 118.39049 | 0.9999113 | 122.29942 | 0.5528484 | 0.0025604 |
| tidyAML | 1 | ARIMA | Test | 0.8410188 | 132.69279 | 0.9402095 | 104.08681 | 1.1349743 | 0.0190591 |
| tidyAML | 2 | LM | Test | 0.7983905 | 125.74316 | 0.8925535 | 100.93004 | 1.1026099 | 0.4689904 |
| tidyAML | 3 | EARTH | Test | 1.2195595 | 262.33670 | 1.3633957 | 118.09168 | 1.4536398 | 0.4689904 |
| tidyAML | 4 | NNAR | Test | 0.7887849 | 120.90446 | 0.8818150 | 103.39080 | 1.0873072 | 0.0030594 |
| RandomWalker | 1 | ARIMA | Test | 1.1393258 | 121.62700 | 0.6344827 | 127.77069 | 1.4272982 | 0.0109631 |
| RandomWalker | 2 | LM | Test | 1.2007578 | 113.10566 | 0.6686937 | 191.37927 | 1.3770711 | 0.0470215 |
| RandomWalker | 3 | EARTH | Test | 1.1311366 | 96.49641 | 0.6299222 | 169.66634 | 1.3635666 | NA |
| RandomWalker | 4 | NNAR | Test | 1.2623718 | 153.92813 | 0.7030061 | 167.68034 | 1.3734631 | 0.0539045 |

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
    ## 1 healthyR.d…         2 LM          Test  0.629 173.  0.718 118.  0.791  4.41e-5
    ## 2 healthyR            3 EARTH       Test  0.608 137.  0.734 135.  0.752  5.97e-2
    ## 3 healthyR.ts         3 EARTH       Test  0.533 161.  0.679  90.2 0.690  6.06e-2
    ## 4 healthyver…         2 LM          Test  0.559 148.  1.08   74.2 0.687  8.20e-2
    ## 5 healthyR.ai         2 LM          Test  0.551  92.3 0.782 125.  0.716  2.55e-2
    ## 6 TidyDensity         4 NNAR        Test  0.434 118.  1.00  122.  0.553  2.56e-3
    ## 7 tidyAML             4 NNAR        Test  0.789 121.  0.882 103.  1.09   3.06e-3
    ## 8 RandomWalk…         3 EARTH       Test  1.13   96.5 0.630 170.  1.36  NA

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1623|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1617|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1561|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1531|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1356|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1207|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [815|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [237|28]>  <mdl_tm_t [1 × 5]>

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
