Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
04 September, 2024

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 112,766
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

The last day in the data set is 2024-09-02 18:25:13, the file was
birthed on: 2024-08-07 07:35:44, and at report knit time is -630.82
hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 112766        |
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
| r_version     |     79284 |          0.30 |   5 |   5 |     0 |       43 |          0 |
| r_arch        |     79284 |          0.30 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     79284 |          0.30 |   7 |  15 |     0 |       19 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        7 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       59 |          0 |
| country       |      9638 |          0.91 |   2 |   2 |     0 |      157 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |         0 |             1 | 2020-11-23 | 2024-09-02 | 2023-02-02 |     1380 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |       mean |         sd |  p0 |   p25 |    p50 |        p75 |    p100 | hist  |
|:--------------|----------:|--------------:|-----------:|-----------:|----:|------:|-------:|-----------:|--------:|:------|
| size          |         0 |             1 | 1178514.96 | 1558737.91 | 355 | 14701 | 274859 | 2373526.00 | 5677952 | ▇▁▂▁▁ |
| ip_id         |         0 |             1 |   10274.75 |   17968.46 |   1 |   319 |   3090 |   11422.25 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| date_time     |         0 |             1 | 2020-11-23 09:00:41 | 2024-09-02 18:25:13 | 2023-02-02 01:29:13 |    68176 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-------:|---------:|
| time          |         0 |             1 |   0 |  59 |   55.5 |       60 |

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
    ## -150.73  -34.19   -9.25   25.80  800.39 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -1.625e+02  8.739e+01
    ## date                                                9.802e-03  4.630e-03
    ## lag(value, 1)                                       1.602e-01  2.641e-02
    ## lag(value, 7)                                       9.708e-02  2.746e-02
    ## lag(value, 14)                                      1.114e-01  2.756e-02
    ## lag(value, 21)                                      3.199e-02  2.772e-02
    ## lag(value, 28)                                      8.698e-02  2.747e-02
    ## lag(value, 35)                                      7.079e-02  2.765e-02
    ## lag(value, 42)                                      4.030e-02  2.775e-02
    ## lag(value, 49)                                      8.802e-02  2.766e-02
    ## month(date, label = TRUE).L                        -1.038e+01  5.698e+00
    ## month(date, label = TRUE).Q                         2.775e+00  5.536e+00
    ## month(date, label = TRUE).C                        -1.092e+01  5.692e+00
    ## month(date, label = TRUE)^4                        -9.058e+00  5.662e+00
    ## month(date, label = TRUE)^5                        -1.635e+01  5.592e+00
    ## month(date, label = TRUE)^6                        -4.290e+00  5.713e+00
    ## month(date, label = TRUE)^7                        -1.014e+01  5.534e+00
    ## month(date, label = TRUE)^8                        -2.769e-01  5.536e+00
    ## month(date, label = TRUE)^9                         7.117e+00  5.527e+00
    ## month(date, label = TRUE)^10                        7.424e+00  5.402e+00
    ## month(date, label = TRUE)^11                       -4.677e+00  5.280e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.154e+01  2.511e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  5.898e+00  2.597e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -1.859 0.063212 .  
    ## date                                                 2.117 0.034436 *  
    ## lag(value, 1)                                        6.067 1.70e-09 ***
    ## lag(value, 7)                                        3.536 0.000421 ***
    ## lag(value, 14)                                       4.043 5.58e-05 ***
    ## lag(value, 21)                                       1.154 0.248764    
    ## lag(value, 28)                                       3.166 0.001579 ** 
    ## lag(value, 35)                                       2.561 0.010563 *  
    ## lag(value, 42)                                       1.452 0.146619    
    ## lag(value, 49)                                       3.182 0.001494 ** 
    ## month(date, label = TRUE).L                         -1.821 0.068771 .  
    ## month(date, label = TRUE).Q                          0.501 0.616292    
    ## month(date, label = TRUE).C                         -1.919 0.055192 .  
    ## month(date, label = TRUE)^4                         -1.600 0.109896    
    ## month(date, label = TRUE)^5                         -2.923 0.003528 ** 
    ## month(date, label = TRUE)^6                         -0.751 0.452855    
    ## month(date, label = TRUE)^7                         -1.832 0.067228 .  
    ## month(date, label = TRUE)^8                         -0.050 0.960121    
    ## month(date, label = TRUE)^9                          1.288 0.198104    
    ## month(date, label = TRUE)^10                         1.375 0.169522    
    ## month(date, label = TRUE)^11                        -0.886 0.375915    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -4.595 4.74e-06 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   2.271 0.023307 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 57.52 on 1308 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2598, Adjusted R-squared:  0.2474 
    ## F-statistic: 20.87 on 22 and 1308 DF,  p-value: < 2.2e-16

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
    ## 1 healthyR.data <tibble [1,348 × 2]> <tibble [28 × 2]> <split [1320|28]>
    ## 2 healthyR      <tibble [1,340 × 2]> <tibble [28 × 2]> <split [1312|28]>
    ## 3 <NA>          <tibble [26 × 2]>    <tibble [28 × 2]> <split [0|26]>   
    ## 4 healthyR.ts   <tibble [1,286 × 2]> <tibble [28 × 2]> <split [1258|28]>
    ## 5 healthyverse  <tibble [1,257 × 2]> <tibble [28 × 2]> <split [1229|28]>
    ## 6 healthyR.ai   <tibble [1,083 × 2]> <tibble [28 × 2]> <split [1055|28]>
    ## 7 TidyDensity   <tibble [937 × 2]>   <tibble [28 × 2]> <split [909|28]> 
    ## 8 tidyAML       <tibble [553 × 2]>   <tibble [28 × 2]> <split [525|28]>

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
| healthyR.data |         1 | ARIMA       | Test  | 0.6456952 |   98.38315 | 0.7167749 | 141.58564 | 0.9054171 | 0.0017993 |
| healthyR.data |         2 | LM          | Test  | 0.9171413 |  304.45793 | 1.0181023 | 173.31708 | 1.0373459 | 0.0004127 |
| healthyR.data |         3 | EARTH       | Test  | 0.6139223 |  137.23500 | 0.6815042 | 112.19378 | 0.9072037 | 0.0004127 |
| healthyR.data |         4 | NNAR        | Test  | 0.6137196 |  106.45509 | 0.6812792 | 123.54032 | 0.8866332 | 0.0461428 |
| healthyR      |         1 | ARIMA       | Test  | 0.7162755 |   79.47596 | 0.7277874 | 132.21438 | 0.9438938 | 0.1191850 |
| healthyR      |         2 | LM          | Test  | 0.8518462 |  113.68889 | 0.8655371 | 185.11620 | 1.0484087 | 0.0304556 |
| healthyR      |         3 | EARTH       | Test  | 0.7017227 |   83.10845 | 0.7130008 | 107.34186 | 0.9532296 | 0.0304556 |
| healthyR      |         4 | NNAR        | Test  | 0.8215862 |  112.27998 | 0.8347908 | 175.57455 | 1.0154665 | 0.0775592 |
| NA            |         1 | NULL        | NA    |        NA |         NA |        NA |        NA |        NA |        NA |
| NA            |         2 | NULL        | NA    |        NA |         NA |        NA |        NA |        NA |        NA |
| NA            |         3 | NULL        | NA    |        NA |         NA |        NA |        NA |        NA |        NA |
| NA            |         4 | NULL        | NA    |        NA |         NA |        NA |        NA |        NA |        NA |
| healthyR.ts   |         1 | ARIMA       | Test  | 0.6584743 |  111.84139 | 0.6672290 |  96.67537 | 0.9111014 | 0.1234167 |
| healthyR.ts   |         2 | LM          | Test  | 0.6986902 |  194.81646 | 0.7079796 | 100.99839 | 0.9801370 | 0.0180633 |
| healthyR.ts   |         3 | EARTH       | Test  | 0.6961174 |  198.87859 | 0.7053725 |  99.91535 | 0.9785851 | 0.0180633 |
| healthyR.ts   |         4 | NNAR        | Test  | 0.7975164 |  156.52222 | 0.8081197 | 177.51054 | 1.0069608 | 0.1004807 |
| healthyverse  |         1 | ARIMA       | Test  | 0.5744711 |  199.14540 | 0.7754759 | 112.57967 | 0.6964916 | 0.2119522 |
| healthyverse  |         2 | LM          | Test  | 0.6920244 |  679.87262 | 0.9341606 | 108.89576 | 0.7916859 | 0.0126665 |
| healthyverse  |         3 | EARTH       | Test  | 0.6650214 |  464.95499 | 0.8977093 | 118.61494 | 0.7762654 | 0.0126665 |
| healthyverse  |         4 | NNAR        | Test  | 0.6323810 |  272.64899 | 0.8536482 | 126.80022 | 0.7745172 | 0.1406151 |
| healthyR.ai   |         1 | ARIMA       | Test  | 0.6975563 |  102.71813 | 0.7397146 | 156.65657 | 0.8959669 | 0.1945596 |
| healthyR.ai   |         2 | LM          | Test  | 0.7314899 |  135.64197 | 0.7756990 | 130.18045 | 0.9739750 | 0.0007607 |
| healthyR.ai   |         3 | EARTH       | Test  | 1.7140965 |  606.63305 | 1.8176915 | 153.93702 | 2.0176493 | 0.0007607 |
| healthyR.ai   |         4 | NNAR        | Test  | 0.6687891 |  121.08136 | 0.7092088 | 138.12008 | 0.8545093 | 0.2433618 |
| TidyDensity   |         1 | ARIMA       | Test  | 0.5034203 |  346.74692 | 0.7957095 |  90.46885 | 0.6039463 | 0.1979996 |
| TidyDensity   |         2 | LM          | Test  | 0.5538287 |  383.73727 | 0.8753855 |  92.06680 | 0.6674356 | 0.0047463 |
| TidyDensity   |         3 | EARTH       | Test  | 0.6960865 |  142.93948 | 1.1002390 | 173.61273 | 0.8745016 | 0.0047463 |
| TidyDensity   |         4 | NNAR        | Test  | 0.5690611 |  105.87320 | 0.8994619 | 139.70099 | 0.7279183 | 0.1275560 |
| tidyAML       |         1 | ARIMA       | Test  | 0.5642979 |  199.97299 | 0.7770897 | 115.10209 | 0.6161223 | 0.2340202 |
| tidyAML       |         2 | LM          | Test  | 0.6016029 |  165.02665 | 0.8284622 | 109.78359 | 0.7099592 | 0.0040455 |
| tidyAML       |         3 | EARTH       | Test  | 3.0023674 | 1335.11065 | 4.1345339 | 142.59558 | 3.3040077 | 0.0040455 |
| tidyAML       |         4 | NNAR        | Test  | 0.5541939 |  255.08421 | 0.7631756 |  91.96768 | 0.6611461 | 0.3278044 |

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
    ## 1 healthyR.data         4 NNAR        Test  0.614 106.  0.681 124.  0.887 0.0461
    ## 2 healthyR              1 ARIMA       Test  0.716  79.5 0.728 132.  0.944 0.119 
    ## 3 healthyR.ts           1 ARIMA       Test  0.658 112.  0.667  96.7 0.911 0.123 
    ## 4 healthyverse          1 ARIMA       Test  0.574 199.  0.775 113.  0.696 0.212 
    ## 5 healthyR.ai           4 NNAR        Test  0.669 121.  0.709 138.  0.855 0.243 
    ## 6 TidyDensity           1 ARIMA       Test  0.503 347.  0.796  90.5 0.604 0.198 
    ## 7 tidyAML               1 ARIMA       Test  0.564 200.  0.777 115.  0.616 0.234

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1320|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1312|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1258|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1229|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1055|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [909|28]>  <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [525|28]>  <mdl_tm_t [1 × 5]>

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
