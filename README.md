Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
22 May, 2025

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 140,563
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

The last day in the data set is 2025-05-20 19:53:14, the file was
birthed on: 2022-07-02 23:58:17.511888, and at report knit time is
-2.526392^{4} hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 140563        |
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
| r_version     |    101330 |          0.28 |   5 |   5 |     0 |       46 |          0 |
| r_arch        |    101330 |          0.28 |   3 |   7 |     0 |        5 |          0 |
| r_os          |    101330 |          0.28 |   7 |  15 |     0 |       22 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       60 |          0 |
| country       |     11980 |          0.91 |   2 |   2 |     0 |      163 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2025-05-20 | 2023-06-25 | 1640 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1133647.5 | 1518441.43 | 355 | 14701 | 289680 | 2367742 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 10428.1 | 18578.81 | 1 | 285 | 3045 | 11682 | 209747 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2025-05-20 19:53:14 | 2023-06-25 07:28:38 | 85952 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |     median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-----------:|---------:|
| time          |         0 |             1 |   0 |  59 | 12H 6M 55S |       60 |

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
    ## -147.41  -35.63  -11.19   26.65  814.79 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -180.37328   69.62512
    ## date                                                  0.01106    0.00369
    ## lag(value, 1)                                         0.10230    0.02450
    ## lag(value, 7)                                         0.09520    0.02534
    ## lag(value, 14)                                        0.08922    0.02538
    ## lag(value, 21)                                        0.06717    0.02547
    ## lag(value, 28)                                        0.07139    0.02539
    ## lag(value, 35)                                        0.06517    0.02547
    ## lag(value, 42)                                        0.04760    0.02565
    ## lag(value, 49)                                        0.06982    0.02549
    ## month(date, label = TRUE).L                          -9.95635    5.11928
    ## month(date, label = TRUE).Q                           3.19797    5.15554
    ## month(date, label = TRUE).C                         -12.76271    5.14970
    ## month(date, label = TRUE)^4                          -6.85947    5.18879
    ## month(date, label = TRUE)^5                         -11.87137    5.14553
    ## month(date, label = TRUE)^6                          -3.36188    5.23435
    ## month(date, label = TRUE)^7                          -6.72475    5.12387
    ## month(date, label = TRUE)^8                          -4.08476    5.12973
    ## month(date, label = TRUE)^9                           5.45110    5.13428
    ## month(date, label = TRUE)^10                          4.13155    5.13619
    ## month(date, label = TRUE)^11                         -5.51323    5.27200
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -11.78470    2.34878
    ## fourier_vec(date, type = "cos", K = 1, period = 7)    8.31722    2.47486
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.591 0.009669 ** 
    ## date                                                 2.997 0.002767 ** 
    ## lag(value, 1)                                        4.176 3.13e-05 ***
    ## lag(value, 7)                                        3.757 0.000178 ***
    ## lag(value, 14)                                       3.516 0.000450 ***
    ## lag(value, 21)                                       2.637 0.008456 ** 
    ## lag(value, 28)                                       2.812 0.004987 ** 
    ## lag(value, 35)                                       2.559 0.010593 *  
    ## lag(value, 42)                                       1.856 0.063688 .  
    ## lag(value, 49)                                       2.739 0.006227 ** 
    ## month(date, label = TRUE).L                         -1.945 0.051969 .  
    ## month(date, label = TRUE).Q                          0.620 0.535152    
    ## month(date, label = TRUE).C                         -2.478 0.013304 *  
    ## month(date, label = TRUE)^4                         -1.322 0.186368    
    ## month(date, label = TRUE)^5                         -2.307 0.021178 *  
    ## month(date, label = TRUE)^6                         -0.642 0.520791    
    ## month(date, label = TRUE)^7                         -1.312 0.189566    
    ## month(date, label = TRUE)^8                         -0.796 0.425983    
    ## month(date, label = TRUE)^9                          1.062 0.288532    
    ## month(date, label = TRUE)^10                         0.804 0.421288    
    ## month(date, label = TRUE)^11                        -1.046 0.295835    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -5.017 5.83e-07 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   3.361 0.000796 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 58.78 on 1568 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.242,  Adjusted R-squared:  0.2314 
    ## F-statistic: 22.75 on 22 and 1568 DF,  p-value: < 2.2e-16

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
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 4.04681362458384"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 28 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.41191103816392"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 28, 84 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.31459163114283"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63, 28, 84 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 3.31459163114283"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63, 28, 84 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 3.2807319502195"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63, 28, 84 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 3.2807319502195"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63, 28, 84 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.17059434408415"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63, 28, 84 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.17059434408415"

![](man/figures/README-nns_forecasting-1.png)<!-- -->

    ## Package: healthyR.ai
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 5.18326715522937"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 84 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 5.14722698811042"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63, 84 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 5.14722698811042"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63, 84 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 5.91798950852285"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63, 84 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 5.91798950852285"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63, 84 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 5.44049436027063"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63, 84 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 5.44049436027063"

![](man/figures/README-nns_forecasting-2.png)<!-- -->

    ## Package: healthyR.data
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.37374427487614"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 91 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.27471906525191"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63, 91 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.27471906525191"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63, 91 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 2.14681503145182"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63, 91 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 2.14681503145182"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63, 91 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 2.04903943848709"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63, 91 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 2.04903943848709"

![](man/figures/README-nns_forecasting-3.png)<!-- -->

    ## Package: healthyR.ts
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 5.37988540152151"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 56 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 4.93599155870229"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63, 56 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 4.93599155870229"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63, 56 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 6.323884778852"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63, 56 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 6.323884778852"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63, 56 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 5.3625414491335"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63, 56 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 5.3625414491335"

![](man/figures/README-nns_forecasting-4.png)<!-- -->

    ## Package: healthyverse
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 7.68647012209757"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63, 98 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 7.28476474575487"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63, 98 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 7.28476474575487"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63, 98 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 3.99073065269497"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63, 98 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 3.99073065269497"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63, 98 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 5.0178445400852"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63, 98 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 5.0178445400852"

![](man/figures/README-nns_forecasting-5.png)<!-- -->

    ## Package: RandomWalker
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 2.29258207338197"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 63 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 2.29258207338197"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 2.66024727283076"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 63 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 2.66024727283076"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 63 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 2.32944500483892"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 63 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 2.32944500483892"

![](man/figures/README-nns_forecasting-6.png)<!-- -->

    ## Package: tidyAML
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 84 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 5.60946703904922"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 84, 28 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 5.3614183713314"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 84, 28 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 5.3614183713314"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 84, 28 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 3.65501236082385"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 84, 28 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 3.65501236082385"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 84, 28 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 4.27783257051414"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 84, 28 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 4.27783257051414"

![](man/figures/README-nns_forecasting-7.png)<!-- -->

    ## Package: TidyDensity
    ## [1] "CURRNET METHOD: lin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 84 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.76170230344765"
    ## [1] "NNS.ARMA(... method =  'lin' , seasonal.factor =  c( 84, 28 ) ...)"
    ## [1] "CURRENT lin OBJECTIVE FUNCTION = 3.17994386635004"
    ## [1] "BEST method = 'lin', seasonal.factor = c( 84, 28 )"
    ## [1] "BEST lin OBJECTIVE FUNCTION = 3.17994386635004"
    ## [1] "CURRNET METHOD: nonlin"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'nonlin' , seasonal.factor =  c( 84, 28 ) ...)"
    ## [1] "CURRENT nonlin OBJECTIVE FUNCTION = 4.469131188015"
    ## [1] "BEST method = 'nonlin' PATH MEMBER = c( 84, 28 )"
    ## [1] "BEST nonlin OBJECTIVE FUNCTION = 4.469131188015"
    ## [1] "CURRNET METHOD: both"
    ## [1] "COPY LATEST PARAMETERS DIRECTLY FOR NNS.ARMA() IF ERROR:"
    ## [1] "NNS.ARMA(... method =  'both' , seasonal.factor =  c( 84, 28 ) ...)"
    ## [1] "CURRENT both OBJECTIVE FUNCTION = 3.46816900074565"
    ## [1] "BEST method = 'both' PATH MEMBER = c( 84, 28 )"
    ## [1] "BEST both OBJECTIVE FUNCTION = 3.46816900074565"

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
    ## 1 healthyR.data <tibble [1,633 × 2]> <tibble [28 × 2]> <split [1605|28]>
    ## 2 healthyR      <tibble [1,626 × 2]> <tibble [28 × 2]> <split [1598|28]>
    ## 3 healthyR.ts   <tibble [1,570 × 2]> <tibble [28 × 2]> <split [1542|28]>
    ## 4 healthyverse  <tibble [1,540 × 2]> <tibble [28 × 2]> <split [1512|28]>
    ## 5 healthyR.ai   <tibble [1,365 × 2]> <tibble [28 × 2]> <split [1337|28]>
    ## 6 TidyDensity   <tibble [1,216 × 2]> <tibble [28 × 2]> <split [1188|28]>
    ## 7 tidyAML       <tibble [824 × 2]>   <tibble [28 × 2]> <split [796|28]> 
    ## 8 RandomWalker  <tibble [246 × 2]>   <tibble [28 × 2]> <split [218|28]>

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
| healthyR.data | 1 | ARIMA | Test | 0.6024505 | 121.30809 | 0.6451794 | 124.29867 | 0.8139710 | 0.0238452 |
| healthyR.data | 2 | LM | Test | 0.6124481 | 186.30559 | 0.6558861 | 112.20170 | 0.8113033 | 0.0001151 |
| healthyR.data | 3 | EARTH | Test | 0.6107254 | 239.21882 | 0.6540413 | 100.46144 | 0.8401115 | 0.0001151 |
| healthyR.data | 4 | NNAR | Test | 0.7384285 | 131.06724 | 0.7908017 | 175.42980 | 0.9112582 | 0.0000150 |
| healthyR | 1 | ARIMA | Test | 0.6180443 | 102.85582 | 0.7004470 | 172.97619 | 0.7937511 | 0.0321888 |
| healthyR | 2 | LM | Test | 0.5983765 | 93.85549 | 0.6781569 | 162.63589 | 0.7663721 | 0.0199307 |
| healthyR | 3 | EARTH | Test | 0.5614027 | 104.07051 | 0.6362535 | 126.70136 | 0.7298067 | 0.0199307 |
| healthyR | 4 | NNAR | Test | 0.5818227 | 91.85703 | 0.6593960 | 137.00476 | 0.7556218 | 0.0396462 |
| healthyR.ts | 1 | ARIMA | Test | 0.6592480 | 98.00169 | 0.6862808 | 181.08091 | 0.8161877 | 0.0012602 |
| healthyR.ts | 2 | LM | Test | 0.8774649 | 247.93686 | 0.9134458 | 160.44838 | 1.0353776 | 0.0012602 |
| healthyR.ts | 3 | EARTH | Test | 0.5841894 | 217.69964 | 0.6081444 | 106.46609 | 0.7880542 | 0.0012602 |
| healthyR.ts | 4 | NNAR | Test | 0.6782554 | 116.19916 | 0.7060676 | 178.23915 | 0.8273768 | 0.0124290 |
| healthyverse | 1 | ARIMA | Test | 0.4868969 | 222.95674 | 0.7618956 | 75.53168 | 0.6466775 | 0.0007405 |
| healthyverse | 2 | LM | Test | 0.4962502 | 294.07235 | 0.7765316 | 75.51858 | 0.6181015 | 0.0151692 |
| healthyverse | 3 | EARTH | Test | 0.5242770 | 347.80295 | 0.8203880 | 75.84260 | 0.6270364 | 0.0151692 |
| healthyverse | 4 | NNAR | Test | 0.5117820 | 129.05174 | 0.8008359 | 89.17265 | 0.6884951 | 0.1635678 |
| healthyR.ai | 1 | ARIMA | Test | 0.6768257 | 113.09614 | 0.8857173 | 145.07026 | 1.0134024 | 0.1217525 |
| healthyR.ai | 2 | LM | Test | 0.6625740 | 124.03298 | 0.8670670 | 136.69623 | 0.9805577 | 0.3157332 |
| healthyR.ai | 3 | EARTH | Test | 0.6650362 | 156.92742 | 0.8702891 | 124.59006 | 1.0023498 | 0.3157332 |
| healthyR.ai | 4 | NNAR | Test | 0.6668461 | 159.50008 | 0.8726576 | 135.81448 | 0.9826882 | 0.0004123 |
| TidyDensity | 1 | ARIMA | Test | 0.3946882 | 278.65194 | 0.8012861 | 106.43998 | 0.5023423 | 0.0034602 |
| TidyDensity | 2 | LM | Test | 0.5339085 | 484.54942 | 1.0839278 | 112.96073 | 0.6563186 | 0.0065662 |
| TidyDensity | 3 | EARTH | Test | 0.4170037 | 311.01543 | 0.8465905 | 110.01464 | 0.5124040 | 0.0065662 |
| TidyDensity | 4 | NNAR | Test | 0.3501295 | 135.98560 | 0.7108242 | 117.14941 | 0.4416108 | 0.0693769 |
| tidyAML | 1 | ARIMA | Test | 0.8079619 | 176.20602 | 0.9929395 | 108.49521 | 1.0735639 | 0.0207307 |
| tidyAML | 2 | LM | Test | 0.8114346 | 193.87174 | 0.9972072 | 106.04485 | 1.0749523 | 0.1094946 |
| tidyAML | 3 | EARTH | Test | 0.7843035 | 127.04282 | 0.9638646 | 124.15947 | 1.0200088 | 0.1094946 |
| tidyAML | 4 | NNAR | Test | 0.8004854 | 170.25381 | 0.9837512 | 110.17859 | 1.0577341 | 0.0010807 |
| RandomWalker | 1 | ARIMA | Test | 1.2654683 | 127.79407 | 0.6426349 | 128.96365 | 1.6171319 | 0.0032691 |
| RandomWalker | 2 | LM | Test | 1.1970641 | 99.79538 | 0.6078976 | 198.11511 | 1.4115103 | 0.0243067 |
| RandomWalker | 3 | EARTH | Test | 1.1845679 | 94.42761 | 0.6015518 | 169.19614 | 1.4323749 | NA |
| RandomWalker | 4 | NNAR | Test | 1.2067769 | 127.80069 | 0.6128300 | 142.84383 | 1.3937849 | 0.0401196 |

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
    ## 1 healthyR.da…         2 LM          Test  0.612  186. 0.656 112.  0.811 1.15e-4
    ## 2 healthyR             3 EARTH       Test  0.561  104. 0.636 127.  0.730 1.99e-2
    ## 3 healthyR.ts          3 EARTH       Test  0.584  218. 0.608 106.  0.788 1.26e-3
    ## 4 healthyverse         2 LM          Test  0.496  294. 0.777  75.5 0.618 1.52e-2
    ## 5 healthyR.ai          2 LM          Test  0.663  124. 0.867 137.  0.981 3.16e-1
    ## 6 TidyDensity          4 NNAR        Test  0.350  136. 0.711 117.  0.442 6.94e-2
    ## 7 tidyAML              3 EARTH       Test  0.784  127. 0.964 124.  1.02  1.09e-1
    ## 8 RandomWalker         4 NNAR        Test  1.21   128. 0.613 143.  1.39  4.01e-2

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1605|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1598|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1542|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1512|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1337|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1188|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [796|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [218|28]>  <mdl_tm_t [1 × 5]>

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
