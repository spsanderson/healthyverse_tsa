Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
06 January, 2025

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 126,257
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

The last day in the data set is 2025-01-04 23:42:35, the file was
birthed on: 2024-08-07 07:35:44, and at report knit time is -3612.11
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 126257        |
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
| r_version     |     89753 |          0.29 |   5 |   5 |     0 |       45 |          0 |
| r_arch        |     89753 |          0.29 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     89753 |          0.29 |   7 |  15 |     0 |       21 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       60 |          0 |
| country       |     10764 |          0.91 |   2 |   2 |     0 |      160 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2025-01-04 | 2023-04-18 | 1504 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1145267.56 | 1534002.5 | 355 | 14701 | 260377 | 2367947 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 10327.71 | 17969.3 | 1 | 317 | 3098 | 11834 | 143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2025-01-04 23:42:35 | 2023-04-18 04:47:55 | 76442 |

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
    ## -155.00  -35.15   -9.98   27.40  802.94 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -1.992e+02  7.947e+01
    ## date                                                1.183e-02  4.211e-03
    ## lag(value, 1)                                       1.286e-01  2.547e-02
    ## lag(value, 7)                                       9.155e-02  2.647e-02
    ## lag(value, 14)                                      1.054e-01  2.648e-02
    ## lag(value, 21)                                      5.113e-02  2.661e-02
    ## lag(value, 28)                                      6.807e-02  2.644e-02
    ## lag(value, 35)                                      7.214e-02  2.656e-02
    ## lag(value, 42)                                      4.881e-02  2.663e-02
    ## lag(value, 49)                                      9.558e-02  2.647e-02
    ## month(date, label = TRUE).L                        -1.236e+01  5.445e+00
    ## month(date, label = TRUE).Q                         3.110e+00  5.277e+00
    ## month(date, label = TRUE).C                        -1.225e+01  5.341e+00
    ## month(date, label = TRUE)^4                        -6.806e+00  5.346e+00
    ## month(date, label = TRUE)^5                        -1.339e+01  5.318e+00
    ## month(date, label = TRUE)^6                        -5.699e-01  5.390e+00
    ## month(date, label = TRUE)^7                        -9.250e+00  5.285e+00
    ## month(date, label = TRUE)^8                        -2.101e+00  5.281e+00
    ## month(date, label = TRUE)^9                         4.128e+00  5.271e+00
    ## month(date, label = TRUE)^10                        5.167e+00  5.271e+00
    ## month(date, label = TRUE)^11                       -6.225e+00  5.284e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.149e+01  2.434e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  7.165e+00  2.550e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.506 0.012313 *  
    ## date                                                 2.808 0.005047 ** 
    ## lag(value, 1)                                        5.050 5.00e-07 ***
    ## lag(value, 7)                                        3.459 0.000559 ***
    ## lag(value, 14)                                       3.979 7.28e-05 ***
    ## lag(value, 21)                                       1.922 0.054822 .  
    ## lag(value, 28)                                       2.574 0.010142 *  
    ## lag(value, 35)                                       2.716 0.006685 ** 
    ## lag(value, 42)                                       1.833 0.067003 .  
    ## lag(value, 49)                                       3.610 0.000316 ***
    ## month(date, label = TRUE).L                         -2.270 0.023367 *  
    ## month(date, label = TRUE).Q                          0.589 0.555669    
    ## month(date, label = TRUE).C                         -2.294 0.021916 *  
    ## month(date, label = TRUE)^4                         -1.273 0.203188    
    ## month(date, label = TRUE)^5                         -2.518 0.011920 *  
    ## month(date, label = TRUE)^6                         -0.106 0.915806    
    ## month(date, label = TRUE)^7                         -1.750 0.080296 .  
    ## month(date, label = TRUE)^8                         -0.398 0.690838    
    ## month(date, label = TRUE)^9                          0.783 0.433682    
    ## month(date, label = TRUE)^10                         0.980 0.327096    
    ## month(date, label = TRUE)^11                        -1.178 0.238976    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -4.721 2.58e-06 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   2.810 0.005023 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 57.97 on 1432 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2596, Adjusted R-squared:  0.2483 
    ## F-statistic: 22.83 on 22 and 1432 DF,  p-value: < 2.2e-16

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
    ## 1 healthyR.data <tibble [1,469 × 2]> <tibble [28 × 2]> <split [1441|28]>
    ## 2 healthyR      <tibble [1,462 × 2]> <tibble [28 × 2]> <split [1434|28]>
    ## 3 <NA>          <tibble [28 × 2]>    <tibble [28 × 2]> <split [0|28]>   
    ## 4 healthyR.ts   <tibble [1,408 × 2]> <tibble [28 × 2]> <split [1380|28]>
    ## 5 healthyverse  <tibble [1,379 × 2]> <tibble [28 × 2]> <split [1351|28]>
    ## 6 healthyR.ai   <tibble [1,205 × 2]> <tibble [28 × 2]> <split [1177|28]>
    ## 7 TidyDensity   <tibble [1,059 × 2]> <tibble [28 × 2]> <split [1031|28]>
    ## 8 tidyAML       <tibble [675 × 2]>   <tibble [28 × 2]> <split [647|28]> 
    ## 9 RandomWalker  <tibble [109 × 2]>   <tibble [28 × 2]> <split [81|28]>

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
| healthyR.data | 1 | ARIMA | Test | 0.7968424 | 221.63352 | 0.6868998 | 150.66159 | 0.9460433 | 0.1612839 |
| healthyR.data | 2 | LM | Test | 0.8360111 | 280.92634 | 0.7206643 | 144.15176 | 0.9866348 | 0.1729891 |
| healthyR.data | 3 | EARTH | Test | 0.8779698 | 340.77683 | 0.7568338 | 141.74712 | 1.0131800 | 0.1729891 |
| healthyR.data | 4 | NNAR | Test | 0.8251817 | 151.33236 | 0.7113290 | 168.72735 | 1.0565725 | 0.0619773 |
| healthyR | 1 | ARIMA | Test | 0.7115511 | 153.96580 | 0.6744025 | 168.68172 | 0.8394866 | 0.0378959 |
| healthyR | 2 | LM | Test | 0.6892238 | 103.18243 | 0.6532408 | 185.51160 | 0.8495502 | 0.0719086 |
| healthyR | 3 | EARTH | Test | 0.7549823 | 204.76271 | 0.7155663 | 152.75030 | 0.8890555 | 0.0719086 |
| healthyR | 4 | NNAR | Test | 0.6585398 | 116.91007 | 0.6241588 | 147.43033 | 0.8357757 | 0.0391613 |
| healthyR.ts | 1 | ARIMA | Test | 0.8605068 | 154.00715 | 0.6959779 | 123.59286 | 1.0589463 | 0.0116198 |
| healthyR.ts | 2 | LM | Test | 0.8654068 | 162.29781 | 0.6999410 | 122.29167 | 1.0613995 | 0.0145750 |
| healthyR.ts | 3 | EARTH | Test | 0.8672701 | 165.30108 | 0.7014481 | 121.82476 | 1.0626560 | 0.0145750 |
| healthyR.ts | 4 | NNAR | Test | 0.8641451 | 94.68739 | 0.6989205 | 176.34537 | 1.0745446 | 0.0613878 |
| healthyverse | 1 | ARIMA | Test | 0.5575059 | 126.23484 | 0.8794353 | 96.60336 | 0.6873436 | 0.2718757 |
| healthyverse | 2 | LM | Test | 0.6384331 | 210.36558 | 1.0070935 | 93.68597 | 0.7857297 | 0.2414161 |
| healthyverse | 3 | EARTH | Test | 0.6801375 | 221.66786 | 1.0728801 | 94.85683 | 0.8369789 | 0.2414161 |
| healthyverse | 4 | NNAR | Test | 0.6242872 | 122.45048 | 0.9847793 | 109.59444 | 0.7653880 | 0.0879676 |
| healthyR.ai | 1 | ARIMA | Test | 0.6885494 | 95.91176 | 0.8429976 | 170.05693 | 0.7882956 | 0.2114148 |
| healthyR.ai | 2 | LM | Test | 0.7052822 | 96.53986 | 0.8634838 | 152.31395 | 0.8273610 | 0.0956215 |
| healthyR.ai | 3 | EARTH | Test | 0.7942397 | 120.33866 | 0.9723953 | 155.61987 | 0.9019229 | 0.0956215 |
| healthyR.ai | 4 | NNAR | Test | 0.6504507 | 86.04367 | 0.7963530 | 144.96123 | 0.7768832 | 0.1636183 |
| TidyDensity | 1 | ARIMA | Test | 0.6923828 | 171.64760 | 0.7887201 | 119.71265 | 0.8121941 | 0.0726103 |
| TidyDensity | 2 | LM | Test | 0.7779491 | 326.39050 | 0.8861921 | 121.11341 | 0.8617361 | 0.0002358 |
| TidyDensity | 3 | EARTH | Test | 0.7224248 | 223.15946 | 0.8229421 | 126.73030 | 0.8446799 | 0.0002358 |
| TidyDensity | 4 | NNAR | Test | 0.6742820 | 110.05172 | 0.7681008 | 138.38271 | 0.8605363 | 0.1021848 |
| tidyAML | 1 | ARIMA | Test | 0.8618492 | 111.54196 | 0.9169509 | 111.10827 | 1.0268926 | 0.1741050 |
| tidyAML | 2 | LM | Test | 0.9370835 | 136.76855 | 0.9969952 | 115.29710 | 1.0755954 | 0.0523499 |
| tidyAML | 3 | EARTH | Test | 0.9035221 | 178.19829 | 0.9612881 | 103.92891 | 0.9920868 | 0.0523499 |
| tidyAML | 4 | NNAR | Test | 0.8885748 | 149.36632 | 0.9453852 | 106.99012 | 1.0029865 | 0.0553303 |
| RandomWalker | 1 | ARIMA | Test | 0.7998777 | 75.20161 | 0.3656424 | 84.72897 | 0.9783424 | 0.6532004 |
| RandomWalker | 2 | LM | Test | 1.3301063 | 97.43031 | 0.6080220 | 185.22003 | 1.4605183 | 0.0526657 |
| RandomWalker | 3 | EARTH | Test | 1.3249335 | 98.13238 | 0.6056574 | 170.43060 | 1.4635220 | NA |
| RandomWalker | 4 | NNAR | Test | 3.6735980 | 310.94162 | 1.6792855 | 178.72907 | 6.3665297 | 0.0434913 |

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
    ## 1 healthyR.data         1 ARIMA       Test  0.797 222.  0.687 151.  0.946 0.161 
    ## 2 healthyR              4 NNAR        Test  0.659 117.  0.624 147.  0.836 0.0392
    ## 3 healthyR.ts           1 ARIMA       Test  0.861 154.  0.696 124.  1.06  0.0116
    ## 4 healthyverse          1 ARIMA       Test  0.558 126.  0.879  96.6 0.687 0.272 
    ## 5 healthyR.ai           4 NNAR        Test  0.650  86.0 0.796 145.  0.777 0.164 
    ## 6 TidyDensity           1 ARIMA       Test  0.692 172.  0.789 120.  0.812 0.0726
    ## 7 tidyAML               3 EARTH       Test  0.904 178.  0.961 104.  0.992 0.0523
    ## 8 RandomWalker          1 ARIMA       Test  0.800  75.2 0.366  84.7 0.978 0.653

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1441|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1434|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1380|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1351|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1177|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1031|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [647|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [81|28]>   <mdl_tm_t [1 × 5]>

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
