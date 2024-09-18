Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
18 September, 2024

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 114,308
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

The last day in the data set is 2024-09-16 20:50:10, the file was
birthed on: 2024-08-07 07:35:44, and at report knit time is -969.24
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 114308        |
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
| r_version     |     80446 |          0.30 |   5 |   5 |     0 |       43 |          0 |
| r_arch        |     80446 |          0.30 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     80446 |          0.30 |   7 |  15 |     0 |       19 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        7 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       59 |          0 |
| country       |      9735 |          0.91 |   2 |   2 |     0 |      157 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |         0 |             1 | 2020-11-23 | 2024-09-16 | 2023-02-10 |     1394 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |       mean |         sd |  p0 |   p25 |    p50 |     p75 |    p100 | hist  |
|:--------------|----------:|--------------:|-----------:|-----------:|----:|------:|-------:|--------:|--------:|:------|
| size          |         0 |             1 | 1174968.91 | 1556122.24 | 355 | 14701 | 271195 | 2373269 | 5677952 | ▇▁▂▁▁ |
| ip_id         |         0 |             1 |   10278.81 |   17979.46 |   1 |   317 |   3075 |   11430 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| date_time     |         0 |             1 | 2020-11-23 09:00:41 | 2024-09-16 20:50:10 | 2023-02-10 09:19:16 |    69228 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-------:|---------:|
| time          |         0 |             1 |   0 |  59 |   23.5 |       60 |

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
    ## -153.26  -34.21   -9.67   26.08  798.75 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -1.785e+02  8.684e+01
    ## date                                                1.067e-02  4.601e-03
    ## lag(value, 1)                                       1.496e-01  2.628e-02
    ## lag(value, 7)                                       1.057e-01  2.751e-02
    ## lag(value, 14)                                      1.145e-01  2.761e-02
    ## lag(value, 21)                                      2.466e-02  2.773e-02
    ## lag(value, 28)                                      8.019e-02  2.757e-02
    ## lag(value, 35)                                      6.843e-02  2.774e-02
    ## lag(value, 42)                                      3.773e-02  2.775e-02
    ## lag(value, 49)                                      9.955e-02  2.757e-02
    ## month(date, label = TRUE).L                        -1.009e+01  5.719e+00
    ## month(date, label = TRUE).Q                         2.583e+00  5.562e+00
    ## month(date, label = TRUE).C                        -1.150e+01  5.673e+00
    ## month(date, label = TRUE)^4                        -9.561e+00  5.682e+00
    ## month(date, label = TRUE)^5                        -1.590e+01  5.604e+00
    ## month(date, label = TRUE)^6                        -3.738e+00  5.687e+00
    ## month(date, label = TRUE)^7                        -9.758e+00  5.557e+00
    ## month(date, label = TRUE)^8                        -1.010e+00  5.541e+00
    ## month(date, label = TRUE)^9                         6.308e+00  5.462e+00
    ## month(date, label = TRUE)^10                        6.642e+00  5.356e+00
    ## month(date, label = TRUE)^11                       -5.051e+00  5.293e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.161e+01  2.517e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  6.280e+00  2.605e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.056 0.039981 *  
    ## date                                                 2.319 0.020544 *  
    ## lag(value, 1)                                        5.693 1.54e-08 ***
    ## lag(value, 7)                                        3.843 0.000127 ***
    ## lag(value, 14)                                       4.149 3.55e-05 ***
    ## lag(value, 21)                                       0.889 0.374064    
    ## lag(value, 28)                                       2.909 0.003692 ** 
    ## lag(value, 35)                                       2.467 0.013757 *  
    ## lag(value, 42)                                       1.360 0.174180    
    ## lag(value, 49)                                       3.610 0.000317 ***
    ## month(date, label = TRUE).L                         -1.764 0.077940 .  
    ## month(date, label = TRUE).Q                          0.464 0.642504    
    ## month(date, label = TRUE).C                         -2.027 0.042877 *  
    ## month(date, label = TRUE)^4                         -1.683 0.092703 .  
    ## month(date, label = TRUE)^5                         -2.838 0.004612 ** 
    ## month(date, label = TRUE)^6                         -0.657 0.511086    
    ## month(date, label = TRUE)^7                         -1.756 0.079308 .  
    ## month(date, label = TRUE)^8                         -0.182 0.855324    
    ## month(date, label = TRUE)^9                          1.155 0.248359    
    ## month(date, label = TRUE)^10                         1.240 0.215201    
    ## month(date, label = TRUE)^11                        -0.954 0.340197    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -4.613 4.36e-06 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   2.411 0.016050 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 57.86 on 1322 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2593, Adjusted R-squared:  0.2469 
    ## F-statistic: 21.03 on 22 and 1322 DF,  p-value: < 2.2e-16

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

    ## # A tibble: 8 × 4
    ##   package       .actual_data         .future_data      .splits          
    ##   <fct>         <list>               <list>            <list>           
    ## 1 healthyR.data <tibble [1,362 × 2]> <tibble [28 × 2]> <split [1334|28]>
    ## 2 healthyR      <tibble [1,354 × 2]> <tibble [28 × 2]> <split [1326|28]>
    ## 3 <NA>          <tibble [26 × 2]>    <tibble [28 × 2]> <split [0|26]>   
    ## 4 healthyR.ts   <tibble [1,300 × 2]> <tibble [28 × 2]> <split [1272|28]>
    ## 5 healthyverse  <tibble [1,271 × 2]> <tibble [28 × 2]> <split [1243|28]>
    ## 6 healthyR.ai   <tibble [1,097 × 2]> <tibble [28 × 2]> <split [1069|28]>
    ## 7 TidyDensity   <tibble [951 × 2]>   <tibble [28 × 2]> <split [923|28]> 
    ## 8 tidyAML       <tibble [567 × 2]>   <tibble [28 × 2]> <split [539|28]>

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
  knitr::kable()
```

| package       | .model_id | .model_desc | .type |       mae |      mape |      mase |     smape |      rmse |       rsq |
|:--------------|----------:|:------------|:------|----------:|----------:|----------:|----------:|----------:|----------:|
| healthyR.data |         1 | ARIMA       | Test  | 0.6762037 | 151.22331 | 0.6414321 | 145.91432 | 0.9754533 | 0.0000263 |
| healthyR.data |         2 | LM          | Test  | 0.8710625 | 478.71953 | 0.8262709 | 162.69652 | 1.0581345 | 0.0592768 |
| healthyR.data |         3 | EARTH       | Test  | 0.7025489 | 273.88106 | 0.6664225 | 138.72781 | 0.9942830 | 0.0592768 |
| healthyR.data |         4 | NNAR        | Test  | 0.7054533 | 202.17590 | 0.6691776 | 153.07118 | 0.9877622 | 0.0591224 |
| healthyR      |         1 | ARIMA       | Test  | 0.7072998 | 100.38139 | 0.6207526 | 107.36956 | 0.9840237 | 0.0010822 |
| healthyR      |         2 | LM          | Test  | 0.7967866 | 111.07892 | 0.6992896 | 187.30137 | 1.0197372 | 0.0011758 |
| healthyR      |         3 | EARTH       | Test  | 2.0583906 | 557.97095 | 1.8065202 | 139.84743 | 2.3501892 | 0.0011758 |
| healthyR      |         4 | NNAR        | Test  | 0.7999962 | 122.28801 | 0.7021064 | 168.57946 | 1.0122805 | 0.0106535 |
| NA            |         1 | NULL        | NA    |        NA |        NA |        NA |        NA |        NA |        NA |
| NA            |         2 | NULL        | NA    |        NA |        NA |        NA |        NA |        NA |        NA |
| NA            |         3 | NULL        | NA    |        NA |        NA |        NA |        NA |        NA |        NA |
| NA            |         4 | NULL        | NA    |        NA |        NA |        NA |        NA |        NA |        NA |
| healthyR.ts   |         1 | ARIMA       | Test  | 1.0829038 | 213.70352 | 0.9837537 | 114.80783 | 1.2571165 | 0.0101235 |
| healthyR.ts   |         2 | LM          | Test  | 0.7233385 | 100.41617 | 0.6571100 | 110.29576 | 0.9865017 | 0.0673338 |
| healthyR.ts   |         3 | EARTH       | Test  | 0.7225711 | 101.30386 | 0.6564129 | 109.39724 | 0.9853220 | 0.0673338 |
| healthyR.ts   |         4 | NNAR        | Test  | 0.7588663 |  92.49702 | 0.6893850 | 167.91691 | 1.0312516 | 0.0644331 |
| healthyverse  |         1 | ARIMA       | Test  | 0.7414820 | 365.79759 | 0.7371412 | 115.00606 | 0.9073591 | 0.0141471 |
| healthyverse  |         2 | LM          | Test  | 0.7877555 | 457.66572 | 0.7831438 | 111.67655 | 0.9603303 | 0.0027845 |
| healthyverse  |         3 | EARTH       | Test  | 0.7280050 | 327.13844 | 0.7237431 | 116.69811 | 0.9013073 | 0.0027845 |
| healthyverse  |         4 | NNAR        | Test  | 0.7166520 | 230.73863 | 0.7124565 | 130.33056 | 0.8897526 | 0.0641540 |
| healthyR.ai   |         1 | ARIMA       | Test  | 0.8474760 | 101.91909 | 0.6283023 | 180.25975 | 1.0973409 | 0.0591561 |
| healthyR.ai   |         2 | LM          | Test  | 0.9111549 | 160.90951 | 0.6755126 | 156.45251 | 1.1711308 | 0.0181387 |
| healthyR.ai   |         3 | EARTH       | Test  | 1.5267046 | 671.14881 | 1.1318692 | 135.04193 | 1.8198775 | 0.0181387 |
| healthyR.ai   |         4 | NNAR        | Test  | 0.8701219 | 128.08278 | 0.6450915 | 165.74935 | 1.1145358 | 0.0194892 |
| TidyDensity   |         1 | ARIMA       | Test  | 0.6274427 | 437.16101 | 0.9204381 | 102.13513 | 0.7828415 | 0.0165144 |
| TidyDensity   |         2 | LM          | Test  | 0.6461648 | 439.44438 | 0.9479029 | 104.17631 | 0.7909825 | 0.0215161 |
| TidyDensity   |         3 | EARTH       | Test  | 0.5996340 | 143.42681 | 0.8796436 | 165.67274 | 0.8519578 | 0.0215161 |
| TidyDensity   |         4 | NNAR        | Test  | 0.5100736 | 180.03177 | 0.7482615 | 112.15195 | 0.7794180 | 0.0287302 |
| tidyAML       |         1 | ARIMA       | Test  | 0.6659487 | 196.16908 | 0.9561683 | 119.57412 | 0.8171095 | 0.0003989 |
| tidyAML       |         2 | LM          | Test  | 0.6544245 | 282.32486 | 0.9396219 | 116.55793 | 0.7720271 | 0.0017040 |
| tidyAML       |         3 | EARTH       | Test  | 0.6204415 | 485.75980 | 0.8908293 |  92.52238 | 0.8037057 | 0.0017040 |
| tidyAML       |         4 | NNAR        | Test  | 0.5889561 | 353.56911 | 0.8456226 |  97.56351 | 0.7597220 | 0.0219131 |

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
    ##   # A tibble: 7 × 10
    ##   package      .model_id .model_desc .type   mae  mape  mase smape  rmse     rsq
    ##   <fct>            <int> <chr>       <chr> <dbl> <dbl> <dbl> <dbl> <dbl>   <dbl>
    ## 1 healthyR.da…         1 ARIMA       Test  0.676  151. 0.641 146.  0.975 2.63e-5
    ## 2 healthyR             1 ARIMA       Test  0.707  100. 0.621 107.  0.984 1.08e-3
    ## 3 healthyR.ts          3 EARTH       Test  0.723  101. 0.656 109.  0.985 6.73e-2
    ## 4 healthyverse         4 NNAR        Test  0.717  231. 0.712 130.  0.890 6.42e-2
    ## 5 healthyR.ai          1 ARIMA       Test  0.847  102. 0.628 180.  1.10  5.92e-2
    ## 6 TidyDensity          4 NNAR        Test  0.510  180. 0.748 112.  0.779 2.87e-2
    ## 7 tidyAML              4 NNAR        Test  0.589  354. 0.846  97.6 0.760 2.19e-2

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
    ##   # A tibble: 7 × 5
    ##   package       .actual_data .future_data .splits           .modeltime_tables 
    ##   <fct>         <list>       <list>       <list>            <list>            
    ## 1 healthyR.data <tibble>     <tibble>     <split [1334|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1326|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1272|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1243|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1069|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [923|28]>  <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [539|28]>  <mdl_tm_t [1 × 5]>

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
