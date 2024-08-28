Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
28 August, 2024

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 112,240
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

The last day in the data set is 2024-08-26 21:50:08, the file was
birthed on: 2024-08-07 07:35:44, and at report knit time is -466.24
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 112240        |
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
| r_version     |     78830 |          0.30 |   5 |   5 |     0 |       43 |          0 |
| r_arch        |     78830 |          0.30 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     78830 |          0.30 |   7 |  15 |     0 |       19 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        7 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       59 |          0 |
| country       |      9598 |          0.91 |   2 |   2 |     0 |      157 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |         0 |             1 | 2020-11-23 | 2024-08-26 | 2023-01-31 |     1373 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |       mean |         sd |  p0 |   p25 |      p50 |     p75 |    p100 | hist  |
|:--------------|----------:|--------------:|-----------:|-----------:|----:|------:|---------:|--------:|--------:|:------|
| size          |         0 |             1 | 1180136.19 | 1559856.35 | 355 | 14701 | 279942.5 | 2373526 | 5677952 | ▇▁▂▁▁ |
| ip_id         |         0 |             1 |   10285.42 |   17996.44 |   1 |   317 |   3075.0 |   11457 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| date_time     |         0 |             1 | 2020-11-23 09:00:41 | 2024-08-26 21:50:08 | 2023-01-31 21:26:40 |    67891 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-------:|---------:|
| time          |         0 |             1 |   0 |  59 |   39.5 |       60 |

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
    ## -150.75  -34.53   -9.56   25.83  799.04 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -170.03667   87.97296
    ## date                                                  0.01018    0.00466
    ## lag(value, 1)                                         0.16131    0.02646
    ## lag(value, 7)                                         0.10026    0.02752
    ## lag(value, 14)                                        0.11554    0.02763
    ## lag(value, 21)                                        0.03329    0.02774
    ## lag(value, 28)                                        0.08881    0.02754
    ## lag(value, 35)                                        0.06657    0.02777
    ## lag(value, 42)                                        0.03769    0.02791
    ## lag(value, 49)                                        0.08657    0.02769
    ## month(date, label = TRUE).L                         -10.18566    5.70228
    ## month(date, label = TRUE).Q                           2.49291    5.54405
    ## month(date, label = TRUE).C                         -11.28651    5.71040
    ## month(date, label = TRUE)^4                          -8.73480    5.66967
    ## month(date, label = TRUE)^5                         -15.82326    5.60894
    ## month(date, label = TRUE)^6                          -3.86178    5.72474
    ## month(date, label = TRUE)^7                         -10.42225    5.54725
    ## month(date, label = TRUE)^8                          -0.53774    5.55204
    ## month(date, label = TRUE)^9                           7.19928    5.54843
    ## month(date, label = TRUE)^10                          7.74835    5.43889
    ## month(date, label = TRUE)^11                         -4.10725    5.30228
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -11.38736    2.51676
    ## fourier_vec(date, type = "cos", K = 1, period = 7)    5.90050    2.60270
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -1.933 0.053474 .  
    ## date                                                 2.185 0.029045 *  
    ## lag(value, 1)                                        6.095 1.44e-09 ***
    ## lag(value, 7)                                        3.644 0.000279 ***
    ## lag(value, 14)                                       4.181 3.09e-05 ***
    ## lag(value, 21)                                       1.200 0.230274    
    ## lag(value, 28)                                       3.224 0.001296 ** 
    ## lag(value, 35)                                       2.397 0.016655 *  
    ## lag(value, 42)                                       1.351 0.177034    
    ## lag(value, 49)                                       3.126 0.001810 ** 
    ## month(date, label = TRUE).L                         -1.786 0.074292 .  
    ## month(date, label = TRUE).Q                          0.450 0.653034    
    ## month(date, label = TRUE).C                         -1.976 0.048311 *  
    ## month(date, label = TRUE)^4                         -1.541 0.123653    
    ## month(date, label = TRUE)^5                         -2.821 0.004859 ** 
    ## month(date, label = TRUE)^6                         -0.675 0.500064    
    ## month(date, label = TRUE)^7                         -1.879 0.060494 .  
    ## month(date, label = TRUE)^8                         -0.097 0.922857    
    ## month(date, label = TRUE)^9                          1.298 0.194677    
    ## month(date, label = TRUE)^10                         1.425 0.154507    
    ## month(date, label = TRUE)^11                        -0.775 0.438705    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -4.525 6.60e-06 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   2.267 0.023550 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 57.53 on 1301 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.261,  Adjusted R-squared:  0.2485 
    ## F-statistic: 20.88 on 22 and 1301 DF,  p-value: < 2.2e-16

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
    ## 1 healthyR.data <tibble [1,344 × 2]> <tibble [28 × 2]> <split [1316|28]>
    ## 2 healthyR      <tibble [1,336 × 2]> <tibble [28 × 2]> <split [1308|28]>
    ## 3 <NA>          <tibble [23 × 2]>    <tibble [28 × 2]> <split [0|23]>   
    ## 4 healthyR.ts   <tibble [1,282 × 2]> <tibble [28 × 2]> <split [1254|28]>
    ## 5 healthyverse  <tibble [1,253 × 2]> <tibble [28 × 2]> <split [1225|28]>
    ## 6 healthyR.ai   <tibble [1,079 × 2]> <tibble [28 × 2]> <split [1051|28]>
    ## 7 TidyDensity   <tibble [933 × 2]>   <tibble [28 × 2]> <split [905|28]> 
    ## 8 tidyAML       <tibble [549 × 2]>   <tibble [28 × 2]> <split [521|28]>

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
| healthyR.data |         1 | ARIMA       | Test  | 0.5885431 |  112.01418 | 0.6952363 | 150.95956 | 0.8120750 | 0.1534733 |
| healthyR.data |         2 | LM          | Test  | 0.8873944 |  312.92599 | 1.0482645 | 169.32470 | 1.0236302 | 0.0129208 |
| healthyR.data |         3 | EARTH       | Test  | 0.5733819 |  136.74035 | 0.6773266 | 112.94066 | 0.8400728 | 0.0129208 |
| healthyR.data |         4 | NNAR        | Test  | 0.5959022 |   92.42005 | 0.7039294 | 131.52862 | 0.8428758 | 0.0018647 |
| healthyR      |         1 | ARIMA       | Test  | 0.6933810 |   86.39105 | 0.8216503 | 134.87401 | 0.9207113 | 0.0090913 |
| healthyR      |         2 | LM          | Test  | 0.8095174 |  114.95517 | 0.9592709 | 181.92061 | 1.0126941 | 0.0059663 |
| healthyR      |         3 | EARTH       | Test  | 0.6687468 |   82.20689 | 0.7924590 | 115.37022 | 0.8969855 | 0.0059663 |
| healthyR      |         4 | NNAR        | Test  | 0.8089803 |  123.62488 | 0.9586344 | 169.15183 | 1.0120342 | 0.0000003 |
| NA            |         1 | NULL        | NA    |        NA |         NA |        NA |        NA |        NA |        NA |
| NA            |         2 | NULL        | NA    |        NA |         NA |        NA |        NA |        NA |        NA |
| NA            |         3 | NULL        | NA    |        NA |         NA |        NA |        NA |        NA |        NA |
| NA            |         4 | NULL        | NA    |        NA |         NA |        NA |        NA |        NA |        NA |
| healthyR.ts   |         1 | ARIMA       | Test  | 0.8780139 |  444.15790 | 0.8903308 | 109.10506 | 1.0620430 | 0.0001910 |
| healthyR.ts   |         2 | LM          | Test  | 0.7062410 |  227.40616 | 0.7161483 | 109.61660 | 0.9816798 | 0.0368462 |
| healthyR.ts   |         3 | EARTH       | Test  | 0.7044604 |  232.50534 | 0.7143427 | 108.60421 | 0.9800590 | 0.0368462 |
| healthyR.ts   |         4 | NNAR        | Test  | 0.8773162 |  166.97452 | 0.8896234 | 180.46746 | 1.1504468 | 0.0167461 |
| healthyverse  |         1 | ARIMA       | Test  | 0.6054897 |  572.50132 | 0.8762353 | 115.23639 | 0.7158103 | 0.1947734 |
| healthyverse  |         2 | LM          | Test  | 0.6878999 |  749.63110 | 0.9954952 | 113.73744 | 0.7954132 | 0.0004157 |
| healthyverse  |         3 | EARTH       | Test  | 0.6428328 |  529.50343 | 0.9302763 | 121.89046 | 0.7514679 | 0.0004157 |
| healthyverse  |         4 | NNAR        | Test  | 0.5889823 |  338.12574 | 0.8523466 | 129.37266 | 0.7283252 | 0.1788299 |
| healthyR.ai   |         1 | ARIMA       | Test  | 0.7228036 |  105.60662 | 0.7654723 | 164.03572 | 0.9307974 | 0.0489012 |
| healthyR.ai   |         2 | LM          | Test  | 0.7425481 |  148.35686 | 0.7863824 | 134.64707 | 0.9860269 | 0.0015583 |
| healthyR.ai   |         3 | EARTH       | Test  | 7.8766382 | 3498.52395 | 8.3416135 | 177.45039 | 8.7769199 | 0.0015583 |
| healthyR.ai   |         4 | NNAR        | Test  | 0.7596162 |  138.70091 | 0.8044580 | 150.40900 | 0.9514573 | 0.0162862 |
| TidyDensity   |         1 | ARIMA       | Test  | 0.4783397 |  325.56592 | 0.7838886 |  83.59672 | 0.5922948 | 0.1349899 |
| TidyDensity   |         2 | LM          | Test  | 0.5320880 |  365.95387 | 0.8719697 |  86.05596 | 0.6451402 | 0.0003113 |
| TidyDensity   |         3 | EARTH       | Test  | 0.8161544 |  233.63148 | 1.3374892 | 182.45638 | 0.9892894 | 0.0003113 |
| TidyDensity   |         4 | NNAR        | Test  | 0.5361618 |  103.75054 | 0.8786457 | 118.66544 | 0.7100542 | 0.0881290 |
| tidyAML       |         1 | ARIMA       | Test  | 0.5738802 |  186.91802 | 0.7774075 | 110.83801 | 0.6523719 | 0.2326697 |
| tidyAML       |         2 | LM          | Test  | 0.6157238 |  177.86990 | 0.8340911 | 112.60450 | 0.7391722 | 0.0011012 |
| tidyAML       |         3 | EARTH       | Test  | 5.0095923 | 2599.04598 | 6.7862508 | 157.16722 | 5.5526962 | 0.0011012 |
| tidyAML       |         4 | NNAR        | Test  | 0.5513873 |  234.47340 | 0.7469376 | 102.18402 | 0.6737708 | 0.1599169 |

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
    ## 1 healthyR.da…         1 ARIMA       Test  0.589 112.  0.695 151.  0.812 0.153  
    ## 2 healthyR             3 EARTH       Test  0.669  82.2 0.792 115.  0.897 0.00597
    ## 3 healthyR.ts          3 EARTH       Test  0.704 233.  0.714 109.  0.980 0.0368 
    ## 4 healthyverse         1 ARIMA       Test  0.605 573.  0.876 115.  0.716 0.195  
    ## 5 healthyR.ai          1 ARIMA       Test  0.723 106.  0.765 164.  0.931 0.0489 
    ## 6 TidyDensity          1 ARIMA       Test  0.478 326.  0.784  83.6 0.592 0.135  
    ## 7 tidyAML              1 ARIMA       Test  0.574 187.  0.777 111.  0.652 0.233

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1316|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1308|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1254|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1225|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1051|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [905|28]>  <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [521|28]>  <mdl_tm_t [1 × 5]>

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
