Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
05 September, 2024

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 112,843
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

The last day in the data set is 2024-09-03 20:55:32, the file was
birthed on: 2024-08-07 07:35:44, and at report knit time is -657.33
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 112843        |
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
| r_version     |     79334 |          0.30 |   5 |   5 |     0 |       43 |          0 |
| r_arch        |     79334 |          0.30 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     79334 |          0.30 |   7 |  15 |     0 |       19 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        7 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       59 |          0 |
| country       |      9656 |          0.91 |   2 |   2 |     0 |      157 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |         0 |             1 | 2020-11-23 | 2024-09-03 | 2023-02-02 |     1381 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |       mean |         sd |  p0 |   p25 |    p50 |     p75 |    p100 | hist  |
|:--------------|----------:|--------------:|-----------:|-----------:|----:|------:|-------:|--------:|--------:|:------|
| size          |         0 |             1 | 1178975.04 | 1558746.89 | 355 | 14701 | 274998 | 2373526 | 5677952 | ▇▁▂▁▁ |
| ip_id         |         0 |             1 |   10274.45 |   17968.34 |   1 |   319 |   3090 |   11417 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| date_time     |         0 |             1 | 2020-11-23 09:00:41 | 2024-09-03 20:55:32 | 2023-02-02 06:48:22 |    68236 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |     median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-----------:|---------:|
| time          |         0 |             1 |   0 |  59 | 12H 4M 59S |       60 |

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
    ## -150.62  -34.13   -9.29   25.77  800.39 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -1.608e+02  8.727e+01
    ## date                                                9.711e-03  4.623e-03
    ## lag(value, 1)                                       1.603e-01  2.640e-02
    ## lag(value, 7)                                       9.726e-02  2.745e-02
    ## lag(value, 14)                                      1.115e-01  2.755e-02
    ## lag(value, 21)                                      3.197e-02  2.772e-02
    ## lag(value, 28)                                      8.704e-02  2.746e-02
    ## lag(value, 35)                                      7.107e-02  2.763e-02
    ## lag(value, 42)                                      3.987e-02  2.772e-02
    ## lag(value, 49)                                      8.798e-02  2.765e-02
    ## month(date, label = TRUE).L                        -1.042e+01  5.695e+00
    ## month(date, label = TRUE).Q                         2.807e+00  5.533e+00
    ## month(date, label = TRUE).C                        -1.083e+01  5.686e+00
    ## month(date, label = TRUE)^4                        -9.005e+00  5.659e+00
    ## month(date, label = TRUE)^5                        -1.640e+01  5.589e+00
    ## month(date, label = TRUE)^6                        -4.379e+00  5.707e+00
    ## month(date, label = TRUE)^7                        -1.018e+01  5.531e+00
    ## month(date, label = TRUE)^8                        -2.037e-01  5.532e+00
    ## month(date, label = TRUE)^9                         7.240e+00  5.518e+00
    ## month(date, label = TRUE)^10                        7.530e+00  5.394e+00
    ## month(date, label = TRUE)^11                       -4.619e+00  5.277e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.150e+01  2.509e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  5.904e+00  2.596e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -1.842 0.065670 .  
    ## date                                                 2.101 0.035876 *  
    ## lag(value, 1)                                        6.070 1.67e-09 ***
    ## lag(value, 7)                                        3.544 0.000408 ***
    ## lag(value, 14)                                       4.048 5.48e-05 ***
    ## lag(value, 21)                                       1.154 0.248842    
    ## lag(value, 28)                                       3.170 0.001561 ** 
    ## lag(value, 35)                                       2.572 0.010220 *  
    ## lag(value, 42)                                       1.438 0.150560    
    ## lag(value, 49)                                       3.182 0.001497 ** 
    ## month(date, label = TRUE).L                         -1.830 0.067484 .  
    ## month(date, label = TRUE).Q                          0.507 0.612060    
    ## month(date, label = TRUE).C                         -1.905 0.056976 .  
    ## month(date, label = TRUE)^4                         -1.591 0.111794    
    ## month(date, label = TRUE)^5                         -2.935 0.003393 ** 
    ## month(date, label = TRUE)^6                         -0.767 0.443051    
    ## month(date, label = TRUE)^7                         -1.840 0.066001 .  
    ## month(date, label = TRUE)^8                         -0.037 0.970627    
    ## month(date, label = TRUE)^9                          1.312 0.189702    
    ## month(date, label = TRUE)^10                         1.396 0.162920    
    ## month(date, label = TRUE)^11                        -0.875 0.381511    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -4.584 4.99e-06 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   2.274 0.023141 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 57.5 on 1309 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2597, Adjusted R-squared:  0.2473 
    ## F-statistic: 20.88 on 22 and 1309 DF,  p-value: < 2.2e-16

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
    ## 1 healthyR.data <tibble [1,349 × 2]> <tibble [28 × 2]> <split [1321|28]>
    ## 2 healthyR      <tibble [1,341 × 2]> <tibble [28 × 2]> <split [1313|28]>
    ## 3 <NA>          <tibble [26 × 2]>    <tibble [28 × 2]> <split [0|26]>   
    ## 4 healthyR.ts   <tibble [1,287 × 2]> <tibble [28 × 2]> <split [1259|28]>
    ## 5 healthyverse  <tibble [1,258 × 2]> <tibble [28 × 2]> <split [1230|28]>
    ## 6 healthyR.ai   <tibble [1,084 × 2]> <tibble [28 × 2]> <split [1056|28]>
    ## 7 TidyDensity   <tibble [938 × 2]>   <tibble [28 × 2]> <split [910|28]> 
    ## 8 tidyAML       <tibble [554 × 2]>   <tibble [28 × 2]> <split [526|28]>

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
| healthyR.data |         1 | ARIMA       | Test  | 0.5966528 |  112.65902 | 0.6886037 | 147.45281 | 0.8280879 | 0.0380795 |
| healthyR.data |         2 | LM          | Test  | 0.8785417 |  307.07042 | 1.0139348 | 171.06409 | 1.0075188 | 0.0210243 |
| healthyR.data |         3 | EARTH       | Test  | 0.5731003 |  123.09181 | 0.6614214 | 122.75214 | 0.8303547 | 0.0210243 |
| healthyR.data |         4 | NNAR        | Test  | 0.6067385 |  140.07456 | 0.7002437 | 144.30809 | 0.8462776 | 0.0107582 |
| healthyR      |         1 | ARIMA       | Test  | 0.7289480 |   82.31060 | 0.7369979 | 137.30390 | 0.9495793 | 0.0875556 |
| healthyR      |         2 | LM          | Test  | 0.8643286 |  115.05577 | 0.8738734 | 187.12154 | 1.0557186 | 0.0440560 |
| healthyR      |         3 | EARTH       | Test  | 0.6872402 |   78.72752 | 0.6948294 | 103.37964 | 0.9452625 | 0.0440560 |
| healthyR      |         4 | NNAR        | Test  | 0.8382784 |  115.69638 | 0.8475356 | 178.90191 | 1.0191177 | 0.1038680 |
| NA            |         1 | NULL        | NA    |        NA |         NA |        NA |        NA |        NA |        NA |
| NA            |         2 | NULL        | NA    |        NA |         NA |        NA |        NA |        NA |        NA |
| NA            |         3 | NULL        | NA    |        NA |         NA |        NA |        NA |        NA |        NA |
| NA            |         4 | NULL        | NA    |        NA |         NA |        NA |        NA |        NA |        NA |
| healthyR.ts   |         1 | ARIMA       | Test  | 0.9097601 |  249.82807 | 0.9779225 | 168.01256 | 1.1317837 | 0.0437232 |
| healthyR.ts   |         2 | LM          | Test  | 0.6265488 |  188.57639 | 0.6734921 |  95.27345 | 0.8921821 | 0.0003509 |
| healthyR.ts   |         3 | EARTH       | Test  | 0.6232364 |  192.38687 | 0.6699315 |  94.11333 | 0.8894207 | 0.0003509 |
| healthyR.ts   |         4 | NNAR        | Test  | 0.7619371 |  204.82090 | 0.8190241 | 179.90600 | 0.9517171 | 0.1125801 |
| healthyverse  |         1 | ARIMA       | Test  | 0.6141483 |  456.29709 | 0.8073979 | 110.51720 | 0.7259537 | 0.1543726 |
| healthyverse  |         2 | LM          | Test  | 0.6803390 |  681.08392 | 0.8944163 | 108.35034 | 0.7853466 | 0.0031413 |
| healthyverse  |         3 | EARTH       | Test  | 0.6346930 |  475.35556 | 0.8344071 | 114.59696 | 0.7568987 | 0.0031413 |
| healthyverse  |         4 | NNAR        | Test  | 0.6043061 |  272.07968 | 0.7944586 | 124.85789 | 0.7498759 | 0.1650574 |
| healthyR.ai   |         1 | ARIMA       | Test  | 0.6410274 |  100.17225 | 0.6979386 | 142.87597 | 0.8660833 | 0.1001304 |
| healthyR.ai   |         2 | LM          | Test  | 0.6833397 |  140.20566 | 0.7440074 | 127.50848 | 0.9323507 | 0.0255854 |
| healthyR.ai   |         3 | EARTH       | Test  | 0.9549700 |  341.93290 | 1.0397534 | 123.06988 | 1.2394364 | 0.0255854 |
| healthyR.ai   |         4 | NNAR        | Test  | 0.6204572 |  109.89083 | 0.6755422 | 138.32258 | 0.8206320 | 0.1746560 |
| TidyDensity   |         1 | ARIMA       | Test  | 0.4990061 |  354.06745 | 0.8082712 |  90.59898 | 0.5987956 | 0.0923982 |
| TidyDensity   |         2 | LM          | Test  | 0.5379195 |  403.99739 | 0.8713016 |  92.00540 | 0.6437650 | 0.0108415 |
| TidyDensity   |         3 | EARTH       | Test  | 0.6211159 |  107.55373 | 1.0060599 | 150.22039 | 0.8159768 | 0.0108415 |
| TidyDensity   |         4 | NNAR        | Test  | 0.5597576 |  119.63903 | 0.9066742 | 137.85842 | 0.7210312 | 0.0785152 |
| tidyAML       |         1 | ARIMA       | Test  | 0.5536093 |  210.34670 | 0.8050046 | 112.93003 | 0.6157564 | 0.1309623 |
| tidyAML       |         2 | LM          | Test  | 0.5555669 |  165.80453 | 0.8078512 | 105.37993 | 0.6585507 | 0.0349009 |
| tidyAML       |         3 | EARTH       | Test  | 5.0692978 | 2107.04802 | 7.3712788 | 161.56609 | 5.5333513 | 0.0349009 |
| tidyAML       |         4 | NNAR        | Test  | 0.5416951 |  256.64332 | 0.7876802 |  91.45886 | 0.6560321 | 0.2060913 |

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
    ## 1 healthyR.da…         1 ARIMA       Test  0.597 113.  0.689 147.  0.828 3.81e-2
    ## 2 healthyR             3 EARTH       Test  0.687  78.7 0.695 103.  0.945 4.41e-2
    ## 3 healthyR.ts          3 EARTH       Test  0.623 192.  0.670  94.1 0.889 3.51e-4
    ## 4 healthyverse         1 ARIMA       Test  0.614 456.  0.807 111.  0.726 1.54e-1
    ## 5 healthyR.ai          4 NNAR        Test  0.620 110.  0.676 138.  0.821 1.75e-1
    ## 6 TidyDensity          1 ARIMA       Test  0.499 354.  0.808  90.6 0.599 9.24e-2
    ## 7 tidyAML              1 ARIMA       Test  0.554 210.  0.805 113.  0.616 1.31e-1

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1321|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1313|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1259|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1230|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1056|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [910|28]>  <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [526|28]>  <mdl_tm_t [1 × 5]>

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
