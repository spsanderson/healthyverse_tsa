Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
17 September, 2024

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 114,272
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

The last day in the data set is 2024-09-15 23:53:33, the file was
birthed on: 2024-08-07 07:35:44, and at report knit time is -948.3 hours
old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 114272        |
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
| r_version     |     80428 |          0.30 |   5 |   5 |     0 |       43 |          0 |
| r_arch        |     80428 |          0.30 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     80428 |          0.30 |   7 |  15 |     0 |       19 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        7 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       59 |          0 |
| country       |      9729 |          0.91 |   2 |   2 |     0 |      157 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |         0 |             1 | 2020-11-23 | 2024-09-15 | 2023-02-09 |     1393 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |       mean |         sd |  p0 |   p25 |    p50 |     p75 |    p100 | hist  |
|:--------------|----------:|--------------:|-----------:|-----------:|----:|------:|-------:|--------:|--------:|:------|
| size          |         0 |             1 | 1174977.53 | 1556246.14 | 355 | 14701 | 271104 | 2373269 | 5677952 | ▇▁▂▁▁ |
| ip_id         |         0 |             1 |   10275.13 |   17973.49 |   1 |   317 |   3075 |   11424 |  143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min                 | max                 | median              | n_unique |
|:--------------|----------:|--------------:|:--------------------|:--------------------|:--------------------|---------:|
| date_time     |         0 |             1 | 2020-11-23 09:00:41 | 2024-09-15 23:53:33 | 2023-02-09 22:00:47 |    69196 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-------:|---------:|
| time          |         0 |             1 |   0 |  59 |     22 |       60 |

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
    ## -153.19  -34.12   -9.54   26.14  798.67 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -1.815e+02  8.694e+01
    ## date                                                1.083e-02  4.607e-03
    ## lag(value, 1)                                       1.496e-01  2.628e-02
    ## lag(value, 7)                                       1.057e-01  2.751e-02
    ## lag(value, 14)                                      1.145e-01  2.761e-02
    ## lag(value, 21)                                      2.426e-02  2.774e-02
    ## lag(value, 28)                                      8.029e-02  2.757e-02
    ## lag(value, 35)                                      6.856e-02  2.775e-02
    ## lag(value, 42)                                      3.717e-02  2.776e-02
    ## lag(value, 49)                                      9.975e-02  2.758e-02
    ## month(date, label = TRUE).L                        -1.001e+01  5.721e+00
    ## month(date, label = TRUE).Q                         2.531e+00  5.564e+00
    ## month(date, label = TRUE).C                        -1.162e+01  5.677e+00
    ## month(date, label = TRUE)^4                        -9.634e+00  5.684e+00
    ## month(date, label = TRUE)^5                        -1.581e+01  5.606e+00
    ## month(date, label = TRUE)^6                        -3.591e+00  5.691e+00
    ## month(date, label = TRUE)^7                        -9.702e+00  5.558e+00
    ## month(date, label = TRUE)^8                        -1.112e+00  5.544e+00
    ## month(date, label = TRUE)^9                         6.112e+00  5.469e+00
    ## month(date, label = TRUE)^10                        6.475e+00  5.362e+00
    ## month(date, label = TRUE)^11                       -5.131e+00  5.295e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.165e+01  2.518e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  6.235e+00  2.606e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.088 0.037031 *  
    ## date                                                 2.350 0.018896 *  
    ## lag(value, 1)                                        5.691 1.56e-08 ***
    ## lag(value, 7)                                        3.841 0.000129 ***
    ## lag(value, 14)                                       4.147 3.59e-05 ***
    ## lag(value, 21)                                       0.875 0.381917    
    ## lag(value, 28)                                       2.912 0.003654 ** 
    ## lag(value, 35)                                       2.471 0.013604 *  
    ## lag(value, 42)                                       1.339 0.180823    
    ## lag(value, 49)                                       3.617 0.000309 ***
    ## month(date, label = TRUE).L                         -1.750 0.080316 .  
    ## month(date, label = TRUE).Q                          0.455 0.649198    
    ## month(date, label = TRUE).C                         -2.047 0.040862 *  
    ## month(date, label = TRUE)^4                         -1.695 0.090330 .  
    ## month(date, label = TRUE)^5                         -2.821 0.004865 ** 
    ## month(date, label = TRUE)^6                         -0.631 0.528195    
    ## month(date, label = TRUE)^7                         -1.745 0.081144 .  
    ## month(date, label = TRUE)^8                         -0.201 0.841044    
    ## month(date, label = TRUE)^9                          1.118 0.263958    
    ## month(date, label = TRUE)^10                         1.208 0.227391    
    ## month(date, label = TRUE)^11                        -0.969 0.332788    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -4.628 4.05e-06 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   2.393 0.016870 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 57.87 on 1321 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2593, Adjusted R-squared:  0.2469 
    ## F-statistic: 21.02 on 22 and 1321 DF,  p-value: < 2.2e-16

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
    ## 1 healthyR.data <tibble [1,361 × 2]> <tibble [28 × 2]> <split [1333|28]>
    ## 2 healthyR      <tibble [1,353 × 2]> <tibble [28 × 2]> <split [1325|28]>
    ## 3 <NA>          <tibble [26 × 2]>    <tibble [28 × 2]> <split [0|26]>   
    ## 4 healthyR.ts   <tibble [1,299 × 2]> <tibble [28 × 2]> <split [1271|28]>
    ## 5 healthyverse  <tibble [1,270 × 2]> <tibble [28 × 2]> <split [1242|28]>
    ## 6 healthyR.ai   <tibble [1,096 × 2]> <tibble [28 × 2]> <split [1068|28]>
    ## 7 TidyDensity   <tibble [950 × 2]>   <tibble [28 × 2]> <split [922|28]> 
    ## 8 tidyAML       <tibble [566 × 2]>   <tibble [28 × 2]> <split [538|28]>

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
| healthyR.data |         1 | ARIMA       | Test  | 0.5987646 | 167.52436 | 0.6183345 | 146.58717 | 0.8583365 | 0.0035212 |
| healthyR.data |         2 | LM          | Test  | 0.8306506 | 503.68444 | 0.8577994 | 164.80594 | 1.0064287 | 0.0167390 |
| healthyR.data |         3 | EARTH       | Test  | 0.6127009 | 240.33684 | 0.6327263 | 138.50608 | 0.8645333 | 0.0167390 |
| healthyR.data |         4 | NNAR        | Test  | 0.6046798 | 216.41096 | 0.6244430 | 135.77284 | 0.8774896 | 0.0237262 |
| healthyR      |         1 | ARIMA       | Test  | 0.6623531 |  98.40969 | 0.6037826 | 101.66682 | 0.9470529 | 0.0000985 |
| healthyR      |         2 | LM          | Test  | 0.7740186 | 112.51953 | 0.7055738 | 187.90041 | 1.0037613 | 0.0021883 |
| healthyR      |         3 | EARTH       | Test  | 2.2873886 | 657.54257 | 2.0851195 | 142.53481 | 2.5643686 | 0.0021883 |
| healthyR      |         4 | NNAR        | Test  | 0.7804940 | 122.76040 | 0.7114766 | 159.81087 | 1.0070506 | 0.0024094 |
| NA            |         1 | NULL        | NA    |        NA |        NA |        NA |        NA |        NA |        NA |
| NA            |         2 | NULL        | NA    |        NA |        NA |        NA |        NA |        NA |        NA |
| NA            |         3 | NULL        | NA    |        NA |        NA |        NA |        NA |        NA |        NA |
| NA            |         4 | NULL        | NA    |        NA |        NA |        NA |        NA |        NA |        NA |
| healthyR.ts   |         1 | ARIMA       | Test  | 0.9907366 | 190.94846 | 0.8721776 | 107.20344 | 1.1822997 | 0.0101054 |
| healthyR.ts   |         2 | LM          | Test  | 0.7046521 |  95.66078 | 0.6203282 | 105.64146 | 0.9743950 | 0.0467203 |
| healthyR.ts   |         3 | EARTH       | Test  | 0.6973433 |  99.21203 | 0.6138939 | 101.15200 | 0.9674345 | 0.0467203 |
| healthyR.ts   |         4 | NNAR        | Test  | 0.7574177 |  90.68984 | 0.6667794 | 160.44741 | 1.0385779 | 0.0524700 |
| healthyverse  |         1 | ARIMA       | Test  | 0.7153887 | 334.25901 | 0.7869162 | 116.35751 | 0.8868326 | 0.0284615 |
| healthyverse  |         2 | LM          | Test  | 0.7739813 | 479.16891 | 0.8513671 | 112.90274 | 0.9496114 | 0.0005418 |
| healthyverse  |         3 | EARTH       | Test  | 0.6963444 | 341.01474 | 0.7659677 | 115.75191 | 0.8730453 | 0.0005418 |
| healthyverse  |         4 | NNAR        | Test  | 0.6799167 | 240.44252 | 0.7478975 | 128.29929 | 0.8488119 | 0.0358012 |
| healthyR.ai   |         1 | ARIMA       | Test  | 0.8506224 | 106.12259 | 0.6388238 | 179.56246 | 1.1001946 | 0.0660182 |
| healthyR.ai   |         2 | LM          | Test  | 0.9116880 | 163.74473 | 0.6846846 | 156.18183 | 1.1721516 | 0.0140599 |
| healthyR.ai   |         3 | EARTH       | Test  | 1.3649068 | 610.90320 | 1.0250554 | 129.99175 | 1.6711343 | 0.0140599 |
| healthyR.ai   |         4 | NNAR        | Test  | 0.8685354 | 128.27984 | 0.6522767 | 164.16763 | 1.1189706 | 0.0162001 |
| TidyDensity   |         1 | ARIMA       | Test  | 0.5973967 | 411.29868 | 0.8119988 |  98.70944 | 0.7498669 | 0.0005590 |
| TidyDensity   |         2 | LM          | Test  | 0.6198446 | 443.47898 | 0.8425106 |  99.45908 | 0.7556423 | 0.0174365 |
| TidyDensity   |         3 | EARTH       | Test  | 0.6514698 | 134.73675 | 0.8854965 | 164.36794 | 0.9085522 | 0.0174365 |
| TidyDensity   |         4 | NNAR        | Test  | 0.5164955 | 133.79081 | 0.7020356 | 109.84860 | 0.7823346 | 0.0705508 |
| tidyAML       |         1 | ARIMA       | Test  | 0.6135799 | 236.84693 | 0.8256882 | 115.41226 | 0.7626014 | 0.0001716 |
| tidyAML       |         2 | LM          | Test  | 0.6226376 | 306.34728 | 0.8378771 | 118.64084 | 0.7322058 | 0.0091899 |
| tidyAML       |         3 | EARTH       | Test  | 1.0544363 | 492.12189 | 1.4189441 | 164.35633 | 1.2675203 | 0.0091899 |
| tidyAML       |         4 | NNAR        | Test  | 0.5542042 | 367.17941 | 0.7457869 |  95.41449 | 0.7314289 | 0.0316335 |

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
    ## 1 healthyR.da…         1 ARIMA       Test  0.599 168.  0.618 147.  0.858 3.52e-3
    ## 2 healthyR             1 ARIMA       Test  0.662  98.4 0.604 102.  0.947 9.85e-5
    ## 3 healthyR.ts          3 EARTH       Test  0.697  99.2 0.614 101.  0.967 4.67e-2
    ## 4 healthyverse         4 NNAR        Test  0.680 240.  0.748 128.  0.849 3.58e-2
    ## 5 healthyR.ai          1 ARIMA       Test  0.851 106.  0.639 180.  1.10  6.60e-2
    ## 6 TidyDensity          1 ARIMA       Test  0.597 411.  0.812  98.7 0.750 5.59e-4
    ## 7 tidyAML              4 NNAR        Test  0.554 367.  0.746  95.4 0.731 3.16e-2

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1333|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1325|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1271|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1242|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1068|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [922|28]>  <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [538|28]>  <mdl_tm_t [1 × 5]>

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
