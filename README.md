Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
08 January, 2025

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 126,385
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

The last day in the data set is 2025-01-06 23:30:25, the file was
birthed on: 2024-08-07 07:35:44, and at report knit time is -3659.91
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 126385        |
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
| r_version     |     89851 |          0.29 |   5 |   5 |     0 |       45 |          0 |
| r_arch        |     89851 |          0.29 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     89851 |          0.29 |   7 |  15 |     0 |       21 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       60 |          0 |
| country       |     10768 |          0.91 |   2 |   2 |     0 |      160 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2025-01-06 | 2023-04-18 | 1506 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1145531.34 | 1533858.42 | 355 | 14701 | 260378 | 2367950 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 10355.21 | 18031.19 | 1 | 317 | 3098 | 11854 | 143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2025-01-06 23:30:25 | 2023-04-18 15:47:59 | 76557 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |     median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-----------:|---------:|
| time          |         0 |             1 |   0 |  59 | 12H 4M 48S |       60 |

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
    ## -154.92  -35.33   -9.92   27.06  803.97 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -1.958e+02  7.929e+01
    ## date                                                1.166e-02  4.201e-03
    ## lag(value, 1)                                       1.268e-01  2.543e-02
    ## lag(value, 7)                                       9.160e-02  2.646e-02
    ## lag(value, 14)                                      1.053e-01  2.647e-02
    ## lag(value, 21)                                      5.083e-02  2.660e-02
    ## lag(value, 28)                                      6.748e-02  2.643e-02
    ## lag(value, 35)                                      7.301e-02  2.654e-02
    ## lag(value, 42)                                      4.939e-02  2.661e-02
    ## lag(value, 49)                                      9.463e-02  2.646e-02
    ## month(date, label = TRUE).L                        -1.201e+01  5.430e+00
    ## month(date, label = TRUE).Q                         2.806e+00  5.266e+00
    ## month(date, label = TRUE).C                        -1.198e+01  5.333e+00
    ## month(date, label = TRUE)^4                        -7.053e+00  5.339e+00
    ## month(date, label = TRUE)^5                        -1.326e+01  5.314e+00
    ## month(date, label = TRUE)^6                        -6.833e-01  5.388e+00
    ## month(date, label = TRUE)^7                        -9.210e+00  5.284e+00
    ## month(date, label = TRUE)^8                        -2.129e+00  5.280e+00
    ## month(date, label = TRUE)^9                         4.134e+00  5.271e+00
    ## month(date, label = TRUE)^10                        5.181e+00  5.270e+00
    ## month(date, label = TRUE)^11                       -6.249e+00  5.284e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.154e+01  2.433e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  7.322e+00  2.547e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.469 0.013656 *  
    ## date                                                 2.775 0.005597 ** 
    ## lag(value, 1)                                        4.988 6.85e-07 ***
    ## lag(value, 7)                                        3.462 0.000552 ***
    ## lag(value, 14)                                       3.977 7.31e-05 ***
    ## lag(value, 21)                                       1.911 0.056243 .  
    ## lag(value, 28)                                       2.553 0.010772 *  
    ## lag(value, 35)                                       2.751 0.006014 ** 
    ## lag(value, 42)                                       1.856 0.063680 .  
    ## lag(value, 49)                                       3.577 0.000360 ***
    ## month(date, label = TRUE).L                         -2.212 0.027124 *  
    ## month(date, label = TRUE).Q                          0.533 0.594253    
    ## month(date, label = TRUE).C                         -2.246 0.024829 *  
    ## month(date, label = TRUE)^4                         -1.321 0.186728    
    ## month(date, label = TRUE)^5                         -2.495 0.012697 *  
    ## month(date, label = TRUE)^6                         -0.127 0.899107    
    ## month(date, label = TRUE)^7                         -1.743 0.081565 .  
    ## month(date, label = TRUE)^8                         -0.403 0.686851    
    ## month(date, label = TRUE)^9                          0.784 0.432969    
    ## month(date, label = TRUE)^10                         0.983 0.325732    
    ## month(date, label = TRUE)^11                        -1.183 0.237136    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -4.743 2.31e-06 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   2.875 0.004099 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 57.97 on 1434 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2591, Adjusted R-squared:  0.2477 
    ## F-statistic: 22.79 on 22 and 1434 DF,  p-value: < 2.2e-16

![](man/figures/README-base_data_frame-1.png)<!-- -->

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

    ## # A tibble: 9 × 4
    ##   package       .actual_data         .future_data      .splits          
    ##   <fct>         <list>               <list>            <list>           
    ## 1 healthyR.data <tibble [1,471 × 2]> <tibble [28 × 2]> <split [1443|28]>
    ## 2 healthyR      <tibble [1,464 × 2]> <tibble [28 × 2]> <split [1436|28]>
    ## 3 <NA>          <tibble [28 × 2]>    <tibble [28 × 2]> <split [0|28]>   
    ## 4 healthyR.ts   <tibble [1,410 × 2]> <tibble [28 × 2]> <split [1382|28]>
    ## 5 healthyverse  <tibble [1,381 × 2]> <tibble [28 × 2]> <split [1353|28]>
    ## 6 healthyR.ai   <tibble [1,207 × 2]> <tibble [28 × 2]> <split [1179|28]>
    ## 7 TidyDensity   <tibble [1,061 × 2]> <tibble [28 × 2]> <split [1033|28]>
    ## 8 tidyAML       <tibble [677 × 2]>   <tibble [28 × 2]> <split [649|28]> 
    ## 9 RandomWalker  <tibble [111 × 2]>   <tibble [28 × 2]> <split [83|28]>

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
| healthyR.data | 1 | ARIMA | Test | 0.8136919 | 198.32504 | 0.6713266 | 151.13233 | 0.9369198 | 0.0500168 |
| healthyR.data | 2 | LM | Test | 0.8326112 | 230.84511 | 0.6869357 | 143.58358 | 0.9636516 | 0.0753722 |
| healthyR.data | 3 | EARTH | Test | 0.9045265 | 317.94385 | 0.7462686 | 139.44540 | 1.0127526 | 0.0753722 |
| healthyR.data | 4 | NNAR | Test | 0.8296317 | 122.90192 | 0.6844776 | 168.05121 | 1.0507297 | 0.0127393 |
| healthyR | 1 | ARIMA | Test | 0.7164858 | 142.53315 | 0.6930381 | 174.30770 | 0.8342557 | 0.0377820 |
| healthyR | 2 | LM | Test | 0.6948851 | 99.94017 | 0.6721443 | 183.57520 | 0.8425205 | 0.0058752 |
| healthyR | 3 | EARTH | Test | 0.6925529 | 96.74296 | 0.6698884 | 176.67308 | 0.8451983 | 0.0058752 |
| healthyR | 4 | NNAR | Test | 0.6580695 | 98.37393 | 0.6365335 | 144.81465 | 0.8278597 | 0.0410641 |
| healthyR.ts | 1 | ARIMA | Test | 0.8523174 | 139.66645 | 0.6630008 | 124.64823 | 1.0570535 | 0.0007061 |
| healthyR.ts | 2 | LM | Test | 0.8620377 | 157.23692 | 0.6705621 | 121.27374 | 1.0608038 | 0.0002115 |
| healthyR.ts | 3 | EARTH | Test | 0.8639320 | 160.09220 | 0.6720356 | 120.82473 | 1.0619945 | 0.0002115 |
| healthyR.ts | 4 | NNAR | Test | 0.8606234 | 96.29895 | 0.6694619 | 181.28057 | 1.0695176 | 0.0746692 |
| healthyverse | 1 | ARIMA | Test | 0.5474721 | 284.12281 | 0.7755552 | 92.99008 | 0.6957154 | 0.1976074 |
| healthyverse | 2 | LM | Test | 0.6053536 | 303.97397 | 0.8575508 | 96.30367 | 0.7517585 | 0.0891281 |
| healthyverse | 3 | EARTH | Test | 0.6811244 | 377.13082 | 0.9648886 | 96.54686 | 0.8592751 | 0.0891281 |
| healthyverse | 4 | NNAR | Test | 0.5482359 | 161.69238 | 0.7766372 | 107.71981 | 0.6926316 | 0.0158897 |
| healthyR.ai | 1 | ARIMA | Test | 0.6623930 | 93.43718 | 0.7452468 | 165.17660 | 0.7789132 | 0.1058415 |
| healthyR.ai | 2 | LM | Test | 0.6757730 | 116.91273 | 0.7603003 | 152.79094 | 0.8074275 | 0.0157716 |
| healthyR.ai | 3 | EARTH | Test | 0.6677377 | 146.56241 | 0.7512600 | 128.99487 | 0.8399651 | 0.0157716 |
| healthyR.ai | 4 | NNAR | Test | 0.6509177 | 105.34179 | 0.7323361 | 152.62823 | 0.7870941 | 0.0421300 |
| TidyDensity | 1 | ARIMA | Test | 0.7721030 | 178.71921 | 0.7508674 | 126.43487 | 0.8943965 | 0.0441206 |
| TidyDensity | 2 | LM | Test | 0.8235818 | 237.74930 | 0.8009303 | 120.43327 | 0.9395086 | 0.0238664 |
| TidyDensity | 3 | EARTH | Test | 0.7860888 | 170.10686 | 0.7644685 | 129.81828 | 0.9139611 | 0.0238664 |
| TidyDensity | 4 | NNAR | Test | 0.7374964 | 107.80934 | 0.7172126 | 140.74850 | 0.9160685 | 0.0896967 |
| tidyAML | 1 | ARIMA | Test | 0.8280799 | 125.03188 | 0.7917877 | 106.89311 | 0.9787656 | 0.1503756 |
| tidyAML | 2 | LM | Test | 0.9207873 | 139.07235 | 0.8804320 | 113.72472 | 1.0633355 | 0.0100191 |
| tidyAML | 3 | EARTH | Test | 0.9023903 | 187.84610 | 0.8628413 | 104.01744 | 0.9836332 | 0.0100191 |
| tidyAML | 4 | NNAR | Test | 0.8751006 | 150.11924 | 0.8367476 | 105.97051 | 0.9965799 | 0.0591190 |
| RandomWalker | 1 | ARIMA | Test | 0.7126521 | 82.60993 | 0.3456491 | 75.71389 | 1.0201441 | 0.4502786 |
| RandomWalker | 2 | LM | Test | 1.2175631 | 93.81372 | 0.5905401 | 173.60658 | 1.3705159 | 0.0336662 |
| RandomWalker | 3 | EARTH | Test | 1.2125959 | 96.44861 | 0.5881309 | 163.52560 | 1.3724921 | NA |
| RandomWalker | 4 | NNAR | Test | 1.1122873 | 137.69078 | 0.5394794 | 139.11767 | 1.3353492 | 0.1199763 |

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
    ## 1 healthyR.da…         1 ARIMA       Test  0.814 198.  0.671 151.  0.937 5.00e-2
    ## 2 healthyR             4 NNAR        Test  0.658  98.4 0.637 145.  0.828 4.11e-2
    ## 3 healthyR.ts          1 ARIMA       Test  0.852 140.  0.663 125.  1.06  7.06e-4
    ## 4 healthyverse         4 NNAR        Test  0.548 162.  0.777 108.  0.693 1.59e-2
    ## 5 healthyR.ai          1 ARIMA       Test  0.662  93.4 0.745 165.  0.779 1.06e-1
    ## 6 TidyDensity          1 ARIMA       Test  0.772 179.  0.751 126.  0.894 4.41e-2
    ## 7 tidyAML              1 ARIMA       Test  0.828 125.  0.792 107.  0.979 1.50e-1
    ## 8 RandomWalker         1 ARIMA       Test  0.713  82.6 0.346  75.7 1.02  4.50e-1

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1443|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1436|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1382|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1353|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1179|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1033|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [649|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [83|28]>   <mdl_tm_t [1 × 5]>

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
