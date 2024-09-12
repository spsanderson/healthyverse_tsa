Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
12 September, 2024

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 113,563
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

The last day in the data set is 2024-09-10 22:40:46, the file was
birthed on: 2024-08-07 07:35:44, and at report knit time is -827.08
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 113563        |
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
| r_version     |     79874 |          0.30 |   5 |   5 |     0 |       43 |          0 |
| r_arch        |     79874 |          0.30 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     79874 |          0.30 |   7 |  15 |     0 |       19 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        7 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       59 |          0 |
| country       |      9687 |          0.91 |   2 |   2 |     0 |      157 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |         0 |             1 | 2020-11-23 | 2024-09-10 | 2023-02-06 |     1388 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |       mean |         sd |  p0 |   p25 |    p50 |     p75 |    p100 | hist  |
|:--------------|----------:|--------------:|-----------:|-----------:|----:|------:|-------:|--------:|--------:|:------|
| size          |         0 |             1 | 1176466.83 | 1557313.23 | 355 | 14701 | 273954 | 2373433 | 5677952 | ▇▁▂▁▁ |
| ip_id         |         0 |             1 |   10252.53 |   17965.74 |   1 |   317 |   3064 |   11354 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| date_time     |         0 |             1 | 2020-11-23 09:00:41 | 2024-09-10 22:40:46 | 2023-02-06 16:20:53 |    68726 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |     median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-----------:|---------:|
| time          |         0 |             1 |   0 |  59 | 12H 5M 25S |       60 |

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
    ## -151.32  -34.33   -9.45   26.02  800.52 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -1.656e+02  8.664e+01
    ## date                                                9.971e-03  4.590e-03
    ## lag(value, 1)                                       1.579e-01  2.633e-02
    ## lag(value, 7)                                       9.954e-02  2.736e-02
    ## lag(value, 14)                                      1.093e-01  2.745e-02
    ## lag(value, 21)                                      2.919e-02  2.761e-02
    ## lag(value, 28)                                      8.641e-02  2.743e-02
    ## lag(value, 35)                                      6.926e-02  2.754e-02
    ## lag(value, 42)                                      4.282e-02  2.760e-02
    ## lag(value, 49)                                      9.068e-02  2.746e-02
    ## month(date, label = TRUE).L                        -1.035e+01  5.683e+00
    ## month(date, label = TRUE).Q                         2.738e+00  5.524e+00
    ## month(date, label = TRUE).C                        -1.103e+01  5.652e+00
    ## month(date, label = TRUE)^4                        -9.239e+00  5.645e+00
    ## month(date, label = TRUE)^5                        -1.628e+01  5.572e+00
    ## month(date, label = TRUE)^6                        -4.310e+00  5.670e+00
    ## month(date, label = TRUE)^7                        -1.005e+01  5.520e+00
    ## month(date, label = TRUE)^8                        -4.868e-01  5.510e+00
    ## month(date, label = TRUE)^9                         6.983e+00  5.462e+00
    ## month(date, label = TRUE)^10                        7.353e+00  5.348e+00
    ## month(date, label = TRUE)^11                       -4.793e+00  5.261e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.149e+01  2.501e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  5.971e+00  2.590e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -1.911 0.056218 .  
    ## date                                                 2.172 0.030023 *  
    ## lag(value, 1)                                        5.996 2.61e-09 ***
    ## lag(value, 7)                                        3.639 0.000285 ***
    ## lag(value, 14)                                       3.980 7.27e-05 ***
    ## lag(value, 21)                                       1.057 0.290581    
    ## lag(value, 28)                                       3.151 0.001666 ** 
    ## lag(value, 35)                                       2.514 0.012045 *  
    ## lag(value, 42)                                       1.551 0.121049    
    ## lag(value, 49)                                       3.303 0.000983 ***
    ## month(date, label = TRUE).L                         -1.822 0.068717 .  
    ## month(date, label = TRUE).Q                          0.496 0.620238    
    ## month(date, label = TRUE).C                         -1.952 0.051202 .  
    ## month(date, label = TRUE)^4                         -1.637 0.101957    
    ## month(date, label = TRUE)^5                         -2.921 0.003549 ** 
    ## month(date, label = TRUE)^6                         -0.760 0.447375    
    ## month(date, label = TRUE)^7                         -1.821 0.068792 .  
    ## month(date, label = TRUE)^8                         -0.088 0.929610    
    ## month(date, label = TRUE)^9                          1.279 0.201286    
    ## month(date, label = TRUE)^10                         1.375 0.169352    
    ## month(date, label = TRUE)^11                        -0.911 0.362489    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -4.593 4.78e-06 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   2.306 0.021287 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 57.44 on 1316 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2598, Adjusted R-squared:  0.2475 
    ## F-statistic:    21 on 22 and 1316 DF,  p-value: < 2.2e-16

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
    ## 1 healthyR.data <tibble [1,356 × 2]> <tibble [28 × 2]> <split [1328|28]>
    ## 2 healthyR      <tibble [1,348 × 2]> <tibble [28 × 2]> <split [1320|28]>
    ## 3 <NA>          <tibble [26 × 2]>    <tibble [28 × 2]> <split [0|26]>   
    ## 4 healthyR.ts   <tibble [1,294 × 2]> <tibble [28 × 2]> <split [1266|28]>
    ## 5 healthyverse  <tibble [1,265 × 2]> <tibble [28 × 2]> <split [1237|28]>
    ## 6 healthyR.ai   <tibble [1,091 × 2]> <tibble [28 × 2]> <split [1063|28]>
    ## 7 TidyDensity   <tibble [945 × 2]>   <tibble [28 × 2]> <split [917|28]> 
    ## 8 tidyAML       <tibble [561 × 2]>   <tibble [28 × 2]> <split [533|28]>

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

| package       | .model_id | .model_desc | .type |       mae |       mape |      mase |     smape |      rmse |       rsq |
|:--------------|----------:|:------------|:------|----------:|-----------:|----------:|----------:|----------:|----------:|
| healthyR.data |         1 | ARIMA       | Test  | 0.6039864 |  108.93633 | 0.6911130 | 167.33526 | 0.8256945 | 0.0008171 |
| healthyR.data |         2 | LM          | Test  | 0.8698701 |  444.33277 | 0.9953511 | 176.93906 | 0.9923399 | 0.0690383 |
| healthyR.data |         3 | EARTH       | Test  | 0.5556901 |  175.36655 | 0.6358497 | 129.34394 | 0.8068068 | 0.0690383 |
| healthyR.data |         4 | NNAR        | Test  | 0.5783697 |  165.55450 | 0.6618010 | 145.33376 | 0.8151950 | 0.0001557 |
| healthyR      |         1 | ARIMA       | Test  | 0.6584769 |   76.30877 | 0.6051777 | 113.66283 | 0.9108469 | 0.0846894 |
| healthyR      |         2 | LM          | Test  | 0.8290722 |  118.25998 | 0.7619644 | 192.09315 | 1.0317129 | 0.0391775 |
| healthyR      |         3 | EARTH       | Test  | 0.6362506 |   74.10577 | 0.5847505 |  91.02558 | 0.9291095 | 0.0391775 |
| healthyR      |         4 | NNAR        | Test  | 0.7857815 |  111.27311 | 0.7221779 | 170.34330 | 0.9684437 | 0.1483794 |
| NA            |         1 | NULL        | NA    |        NA |         NA |        NA |        NA |        NA |        NA |
| NA            |         2 | NULL        | NA    |        NA |         NA |        NA |        NA |        NA |        NA |
| NA            |         3 | NULL        | NA    |        NA |         NA |        NA |        NA |        NA |        NA |
| NA            |         4 | NULL        | NA    |        NA |         NA |        NA |        NA |        NA |        NA |
| healthyR.ts   |         1 | ARIMA       | Test  | 0.6406223 |   91.67915 | 0.6574389 |  86.75517 | 0.8711915 | 0.0170876 |
| healthyR.ts   |         2 | LM          | Test  | 0.6229453 |   78.99217 | 0.6392979 |  89.94043 | 0.8857965 | 0.0596791 |
| healthyR.ts   |         3 | EARTH       | Test  | 0.6181731 |   79.01568 | 0.6344004 |  88.50826 | 0.8824805 | 0.0596791 |
| healthyR.ts   |         4 | NNAR        | Test  | 0.7720060 |   98.35562 | 0.7922714 | 178.81379 | 0.9552645 | 0.2280478 |
| healthyverse  |         1 | ARIMA       | Test  | 0.6415121 |  417.76333 | 0.8650738 | 113.90414 | 0.7465279 | 0.0019258 |
| healthyverse  |         2 | LM          | Test  | 0.6605220 |  494.05644 | 0.8907084 | 109.46753 | 0.7860119 | 0.0062266 |
| healthyverse  |         3 | EARTH       | Test  | 0.5733356 |  348.09441 | 0.7731383 | 110.64358 | 0.7004278 | 0.0062266 |
| healthyverse  |         4 | NNAR        | Test  | 0.5558390 |  246.07717 | 0.7495442 | 122.40646 | 0.6862990 | 0.0162422 |
| healthyR.ai   |         1 | ARIMA       | Test  | 0.7104033 |  128.10386 | 0.5970831 | 157.50751 | 0.9312828 | 0.0699773 |
| healthyR.ai   |         2 | LM          | Test  | 0.7401486 |  193.62520 | 0.6220835 | 140.06507 | 0.9915618 | 0.0266695 |
| healthyR.ai   |         3 | EARTH       | Test  | 1.3662333 |  742.78363 | 1.1482982 | 146.92964 | 1.6996275 | 0.0266695 |
| healthyR.ai   |         4 | NNAR        | Test  | 0.7241674 |  139.12757 | 0.6086516 | 152.23767 | 0.9605208 | 0.0101477 |
| TidyDensity   |         1 | ARIMA       | Test  | 0.4951919 |  361.33585 | 0.8362879 |  89.43887 | 0.6003376 | 0.0118691 |
| TidyDensity   |         2 | LM          | Test  | 0.5163247 |  413.88344 | 0.8719774 |  87.53430 | 0.6180984 | 0.0450874 |
| TidyDensity   |         3 | EARTH       | Test  | 0.6784737 |  130.77814 | 1.1458172 | 164.94976 | 0.8685482 | 0.0450874 |
| TidyDensity   |         4 | NNAR        | Test  | 0.5062825 |  111.27144 | 0.8550179 | 116.81082 | 0.6836874 | 0.2299883 |
| tidyAML       |         1 | ARIMA       | Test  | 0.5732681 |  154.70167 | 0.9292552 | 132.52832 | 0.6532621 | 0.0849038 |
| tidyAML       |         2 | LM          | Test  | 0.5636008 |  223.58671 | 0.9135847 | 111.39894 | 0.6436224 | 0.0416626 |
| tidyAML       |         3 | EARTH       | Test  | 3.6621738 | 1966.04428 | 5.9363042 | 186.02796 | 4.0290726 | 0.0416626 |
| tidyAML       |         4 | NNAR        | Test  | 0.5479634 |  310.75898 | 0.8882367 |  98.47652 | 0.6711105 | 0.0018459 |

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
    ##   package       .model_id .model_desc .type   mae  mape  mase smape  rmse    rsq
    ##   <fct>             <int> <chr>       <chr> <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl>
    ## 1 healthyR.data         3 EARTH       Test  0.556 175.  0.636 129.  0.807 0.0690
    ## 2 healthyR              1 ARIMA       Test  0.658  76.3 0.605 114.  0.911 0.0847
    ## 3 healthyR.ts           1 ARIMA       Test  0.641  91.7 0.657  86.8 0.871 0.0171
    ## 4 healthyverse          4 NNAR        Test  0.556 246.  0.750 122.  0.686 0.0162
    ## 5 healthyR.ai           1 ARIMA       Test  0.710 128.  0.597 158.  0.931 0.0700
    ## 6 TidyDensity           1 ARIMA       Test  0.495 361.  0.836  89.4 0.600 0.0119
    ## 7 tidyAML               2 LM          Test  0.564 224.  0.914 111.  0.644 0.0417

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1328|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1320|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1266|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1237|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1063|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [917|28]>  <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [533|28]>  <mdl_tm_t [1 × 5]>

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
