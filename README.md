Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
19 February, 2025

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 131,412
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

The last day in the data set is 2025-02-17 23:59:06, the file was
birthed on: 2022-07-02 23:58:17.511888, and at report knit time is
-2.306001^{4} hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 131412        |
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
| r_version     |     94081 |          0.28 |   5 |   5 |     0 |       45 |          0 |
| r_arch        |     94081 |          0.28 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     94081 |          0.28 |   7 |  15 |     0 |       21 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       60 |          0 |
| country       |     11104 |          0.92 |   2 |   2 |     0 |      160 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2025-02-17 | 2023-05-06 | 1548 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1139637.90 | 1529260.35 | 355 | 14701 | 260558 | 2367832 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 10414.05 | 18423.37 | 1 | 291 | 3086 | 11887 | 209747 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2025-02-17 23:59:06 | 2023-05-06 14:35:49 | 79645 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-------:|---------:|
| time          |         0 |             1 |   0 |  59 |    4.5 |       60 |

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
    ## -155.50  -34.93   -9.95   27.32  808.56 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -2.071e+02  7.654e+01
    ## date                                                1.227e-02  4.059e-03
    ## lag(value, 1)                                       1.145e-01  2.511e-02
    ## lag(value, 7)                                       8.925e-02  2.606e-02
    ## lag(value, 14)                                      1.047e-01  2.612e-02
    ## lag(value, 21)                                      5.411e-02  2.636e-02
    ## lag(value, 28)                                      6.461e-02  2.624e-02
    ## lag(value, 35)                                      7.944e-02  2.634e-02
    ## lag(value, 42)                                      5.736e-02  2.640e-02
    ## lag(value, 49)                                      8.781e-02  2.634e-02
    ## month(date, label = TRUE).L                        -1.244e+01  5.230e+00
    ## month(date, label = TRUE).Q                         2.416e+00  5.179e+00
    ## month(date, label = TRUE).C                        -1.109e+01  5.271e+00
    ## month(date, label = TRUE)^4                        -8.855e+00  5.264e+00
    ## month(date, label = TRUE)^5                        -1.166e+01  5.235e+00
    ## month(date, label = TRUE)^6                        -2.430e+00  5.318e+00
    ## month(date, label = TRUE)^7                        -8.126e+00  5.257e+00
    ## month(date, label = TRUE)^8                        -2.891e+00  5.284e+00
    ## month(date, label = TRUE)^9                         4.490e+00  5.293e+00
    ## month(date, label = TRUE)^10                        5.142e+00  5.298e+00
    ## month(date, label = TRUE)^11                       -6.426e+00  5.312e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.175e+01  2.412e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  7.388e+00  2.538e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.705 0.006903 ** 
    ## date                                                 3.024 0.002535 ** 
    ## lag(value, 1)                                        4.561 5.51e-06 ***
    ## lag(value, 7)                                        3.425 0.000631 ***
    ## lag(value, 14)                                       4.008 6.42e-05 ***
    ## lag(value, 21)                                       2.053 0.040264 *  
    ## lag(value, 28)                                       2.462 0.013915 *  
    ## lag(value, 35)                                       3.016 0.002601 ** 
    ## lag(value, 42)                                       2.173 0.029953 *  
    ## lag(value, 49)                                       3.334 0.000878 ***
    ## month(date, label = TRUE).L                         -2.379 0.017489 *  
    ## month(date, label = TRUE).Q                          0.466 0.640949    
    ## month(date, label = TRUE).C                         -2.105 0.035491 *  
    ## month(date, label = TRUE)^4                         -1.682 0.092734 .  
    ## month(date, label = TRUE)^5                         -2.228 0.026013 *  
    ## month(date, label = TRUE)^6                         -0.457 0.647784    
    ## month(date, label = TRUE)^7                         -1.546 0.122406    
    ## month(date, label = TRUE)^8                         -0.547 0.584388    
    ## month(date, label = TRUE)^9                          0.848 0.396468    
    ## month(date, label = TRUE)^10                         0.971 0.331938    
    ## month(date, label = TRUE)^11                        -1.210 0.226521    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -4.874 1.21e-06 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   2.911 0.003655 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 58.29 on 1476 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2612, Adjusted R-squared:  0.2502 
    ## F-statistic: 23.72 on 22 and 1476 DF,  p-value: < 2.2e-16

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
    ## 1 healthyR.data <tibble [1,513 × 2]> <tibble [28 × 2]> <split [1485|28]>
    ## 2 healthyR      <tibble [1,506 × 2]> <tibble [28 × 2]> <split [1478|28]>
    ## 3 <NA>          <tibble [28 × 2]>    <tibble [28 × 2]> <split [0|28]>   
    ## 4 healthyR.ts   <tibble [1,452 × 2]> <tibble [28 × 2]> <split [1424|28]>
    ## 5 healthyverse  <tibble [1,423 × 2]> <tibble [28 × 2]> <split [1395|28]>
    ## 6 healthyR.ai   <tibble [1,249 × 2]> <tibble [28 × 2]> <split [1221|28]>
    ## 7 TidyDensity   <tibble [1,103 × 2]> <tibble [28 × 2]> <split [1075|28]>
    ## 8 tidyAML       <tibble [718 × 2]>   <tibble [28 × 2]> <split [690|28]> 
    ## 9 RandomWalker  <tibble [152 × 2]>   <tibble [28 × 2]> <split [124|28]>

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
| healthyR.data | 1 | ARIMA | Test | 0.6938860 | 119.92983 | 0.6544509 | 158.21203 | 0.8192178 | 0.0014450 |
| healthyR.data | 2 | LM | Test | 0.7516811 | 124.53639 | 0.7089614 | 134.63493 | 0.8905992 | 0.0412348 |
| healthyR.data | 3 | EARTH | Test | 0.9136090 | 254.72215 | 0.8616865 | 127.68048 | 1.1223703 | 0.0412348 |
| healthyR.data | 4 | NNAR | Test | 0.7107749 | 118.19676 | 0.6703799 | 177.52082 | 0.8141182 | 0.0222088 |
| healthyR | 1 | ARIMA | Test | 0.9498607 | 102.15334 | 0.7038716 | 164.60017 | 1.1068751 | 0.0397115 |
| healthyR | 2 | LM | Test | 0.9669153 | 104.42765 | 0.7165095 | 193.24986 | 1.1265252 | 0.0038421 |
| healthyR | 3 | EARTH | Test | 0.9728274 | 113.00870 | 0.7208905 | 186.91762 | 1.1306085 | 0.0038421 |
| healthyR | 4 | NNAR | Test | 0.9601558 | 116.89682 | 0.7115006 | 169.96910 | 1.1071072 | 0.0394294 |
| healthyR.ts | 1 | ARIMA | Test | 0.9555539 | 124.32938 | 0.6153775 | 136.81603 | 1.1814976 | 0.0520531 |
| healthyR.ts | 2 | LM | Test | 0.9582234 | 143.45365 | 0.6170967 | 130.16795 | 1.1645637 | 0.0520531 |
| healthyR.ts | 3 | EARTH | Test | 0.9590184 | 145.50878 | 0.6176087 | 129.63683 | 1.1635232 | 0.0520531 |
| healthyR.ts | 4 | NNAR | Test | 0.9452564 | 110.69792 | 0.6087459 | 177.37375 | 1.1957640 | 0.1013214 |
| healthyverse | 1 | ARIMA | Test | 0.6412430 | 211.49780 | 0.7350295 | 117.45348 | 0.7593894 | 0.0008138 |
| healthyverse | 2 | LM | Test | 0.6631407 | 265.00996 | 0.7601299 | 104.76852 | 0.8333795 | 0.1065519 |
| healthyverse | 3 | EARTH | Test | 0.6556654 | 251.76573 | 0.7515613 | 105.74982 | 0.8201938 | 0.1065519 |
| healthyverse | 4 | NNAR | Test | 0.6054855 | 146.21948 | 0.6940422 | 120.68438 | 0.7220604 | 0.0830572 |
| healthyR.ai | 1 | ARIMA | Test | 0.8336475 | 98.30275 | 0.6886844 | 184.49261 | 0.9821271 | 0.0424199 |
| healthyR.ai | 2 | LM | Test | 0.8499566 | 102.22364 | 0.7021576 | 158.72591 | 1.0300391 | 0.0197256 |
| healthyR.ai | 3 | EARTH | Test | 0.8595650 | 106.72322 | 0.7100952 | 150.66552 | 1.0512101 | 0.0197256 |
| healthyR.ai | 4 | NNAR | Test | 0.8166436 | 97.91304 | 0.6746374 | 160.78820 | 0.9663247 | 0.0847165 |
| TidyDensity | 1 | ARIMA | Test | 0.6121270 | 178.42521 | 0.6534670 | 102.37809 | 0.7421730 | 0.1013478 |
| TidyDensity | 2 | LM | Test | 0.6637061 | 225.15562 | 0.7085295 | 102.01587 | 0.7890671 | 0.0273446 |
| TidyDensity | 3 | EARTH | Test | 0.6280578 | 154.78210 | 0.6704737 | 108.19726 | 0.7681366 | 0.0273446 |
| TidyDensity | 4 | NNAR | Test | 0.6302491 | 122.16208 | 0.6728130 | 129.41540 | 0.7774876 | 0.1177124 |
| tidyAML | 1 | ARIMA | Test | 0.7017533 | 152.57984 | 0.9052405 | 97.10777 | 0.8867796 | 0.0215421 |
| tidyAML | 2 | LM | Test | 0.6911859 | 148.24339 | 0.8916088 | 98.10684 | 0.8704498 | 0.0458501 |
| tidyAML | 3 | EARTH | Test | 0.7443252 | 184.68673 | 0.9601570 | 96.77386 | 0.9279765 | 0.0458501 |
| tidyAML | 4 | NNAR | Test | 0.7155320 | 154.09237 | 0.9230145 | 101.60166 | 0.9001243 | 0.0175820 |
| RandomWalker | 1 | ARIMA | Test | 1.1357472 | 81.13982 | 0.5289721 | 130.61456 | 1.3342698 | 0.2748522 |
| RandomWalker | 2 | LM | Test | 1.2884939 | 93.22659 | 0.6001136 | 174.17205 | 1.4899585 | 0.0059124 |
| RandomWalker | 3 | EARTH | Test | 1.2818076 | 88.11084 | 0.5969995 | 153.47320 | 1.5080969 | NA |
| RandomWalker | 4 | NNAR | Test | 1.4269758 | 162.88745 | 0.6646113 | 160.51620 | 1.6570686 | 0.0220975 |

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
    ## 1 healthyR.data         4 NNAR        Test  0.711 118.  0.670 178.  0.814 0.0222
    ## 2 healthyR              1 ARIMA       Test  0.950 102.  0.704 165.  1.11  0.0397
    ## 3 healthyR.ts           3 EARTH       Test  0.959 146.  0.618 130.  1.16  0.0521
    ## 4 healthyverse          4 NNAR        Test  0.605 146.  0.694 121.  0.722 0.0831
    ## 5 healthyR.ai           4 NNAR        Test  0.817  97.9 0.675 161.  0.966 0.0847
    ## 6 TidyDensity           1 ARIMA       Test  0.612 178.  0.653 102.  0.742 0.101 
    ## 7 tidyAML               2 LM          Test  0.691 148.  0.892  98.1 0.870 0.0459
    ## 8 RandomWalker          1 ARIMA       Test  1.14   81.1 0.529 131.  1.33  0.275

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1485|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1478|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1424|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1395|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1221|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1075|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [690|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [124|28]>  <mdl_tm_t [1 × 5]>

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
