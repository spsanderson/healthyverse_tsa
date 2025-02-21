Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
21 February, 2025

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 131,629
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

The last day in the data set is 2025-02-19 23:35:27, the file was
birthed on: 2022-07-02 23:58:17.511888, and at report knit time is
-2.310762^{4} hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 131629        |
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
| r_version     |     94263 |          0.28 |   5 |   5 |     0 |       45 |          0 |
| r_arch        |     94263 |          0.28 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     94263 |          0.28 |   7 |  15 |     0 |       21 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       60 |          0 |
| country       |     11131 |          0.92 |   2 |   2 |     0 |      160 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2025-02-19 | 2023-05-08 | 1550 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1140090.93 | 1529175.17 | 355 | 14701 | 260611 | 2367833 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 10408.99 | 18422.19 | 1 | 281 | 3077 | 11877 | 209747 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2025-02-19 23:35:27 | 2023-05-08 02:13:01 | 79844 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max |     median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-----------:|---------:|
| time          |         0 |             1 |   0 |  59 | 12H 8M 31S |       60 |

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
    ## -154.75  -34.76   -9.86   27.35  809.39 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -2.036e+02  7.643e+01
    ## date                                                1.211e-02  4.053e-03
    ## lag(value, 1)                                       1.149e-01  2.509e-02
    ## lag(value, 7)                                       8.758e-02  2.602e-02
    ## lag(value, 14)                                      1.041e-01  2.608e-02
    ## lag(value, 21)                                      5.662e-02  2.628e-02
    ## lag(value, 28)                                      6.274e-02  2.620e-02
    ## lag(value, 35)                                      7.875e-02  2.633e-02
    ## lag(value, 42)                                      5.712e-02  2.640e-02
    ## lag(value, 49)                                      8.637e-02  2.628e-02
    ## month(date, label = TRUE).L                        -1.206e+01  5.221e+00
    ## month(date, label = TRUE).Q                         2.271e+00  5.178e+00
    ## month(date, label = TRUE).C                        -1.109e+01  5.270e+00
    ## month(date, label = TRUE)^4                        -8.583e+00  5.259e+00
    ## month(date, label = TRUE)^5                        -1.203e+01  5.226e+00
    ## month(date, label = TRUE)^6                        -2.053e+00  5.309e+00
    ## month(date, label = TRUE)^7                        -8.424e+00  5.251e+00
    ## month(date, label = TRUE)^8                        -2.693e+00  5.282e+00
    ## month(date, label = TRUE)^9                         4.364e+00  5.292e+00
    ## month(date, label = TRUE)^10                        5.173e+00  5.297e+00
    ## month(date, label = TRUE)^11                       -6.410e+00  5.311e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.173e+01  2.409e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  7.416e+00  2.538e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.664 0.007808 ** 
    ## date                                                 2.988 0.002859 ** 
    ## lag(value, 1)                                        4.580 5.05e-06 ***
    ## lag(value, 7)                                        3.365 0.000784 ***
    ## lag(value, 14)                                       3.992 6.86e-05 ***
    ## lag(value, 21)                                       2.154 0.031382 *  
    ## lag(value, 28)                                       2.395 0.016735 *  
    ## lag(value, 35)                                       2.991 0.002827 ** 
    ## lag(value, 42)                                       2.164 0.030616 *  
    ## lag(value, 49)                                       3.286 0.001041 ** 
    ## month(date, label = TRUE).L                         -2.310 0.021010 *  
    ## month(date, label = TRUE).Q                          0.439 0.661032    
    ## month(date, label = TRUE).C                         -2.105 0.035459 *  
    ## month(date, label = TRUE)^4                         -1.632 0.102890    
    ## month(date, label = TRUE)^5                         -2.302 0.021476 *  
    ## month(date, label = TRUE)^6                         -0.387 0.699007    
    ## month(date, label = TRUE)^7                         -1.604 0.108890    
    ## month(date, label = TRUE)^8                         -0.510 0.610230    
    ## month(date, label = TRUE)^9                          0.825 0.409669    
    ## month(date, label = TRUE)^10                         0.976 0.328990    
    ## month(date, label = TRUE)^11                        -1.207 0.227679    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -4.871 1.23e-06 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   2.922 0.003527 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 58.28 on 1478 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2605, Adjusted R-squared:  0.2495 
    ## F-statistic: 23.67 on 22 and 1478 DF,  p-value: < 2.2e-16

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
    ## 1 healthyR.data <tibble [1,515 × 2]> <tibble [28 × 2]> <split [1487|28]>
    ## 2 healthyR      <tibble [1,508 × 2]> <tibble [28 × 2]> <split [1480|28]>
    ## 3 <NA>          <tibble [28 × 2]>    <tibble [28 × 2]> <split [0|28]>   
    ## 4 healthyR.ts   <tibble [1,454 × 2]> <tibble [28 × 2]> <split [1426|28]>
    ## 5 healthyverse  <tibble [1,425 × 2]> <tibble [28 × 2]> <split [1397|28]>
    ## 6 healthyR.ai   <tibble [1,251 × 2]> <tibble [28 × 2]> <split [1223|28]>
    ## 7 TidyDensity   <tibble [1,105 × 2]> <tibble [28 × 2]> <split [1077|28]>
    ## 8 tidyAML       <tibble [720 × 2]>   <tibble [28 × 2]> <split [692|28]> 
    ## 9 RandomWalker  <tibble [154 × 2]>   <tibble [28 × 2]> <split [126|28]>

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
| healthyR.data | 1 | ARIMA | Test | 0.7302901 | 113.37599 | 0.6765001 | 177.84295 | 0.8633276 | 0.1592835 |
| healthyR.data | 2 | LM | Test | 0.7372113 | 123.95531 | 0.6829116 | 133.94148 | 0.8751827 | 0.0540001 |
| healthyR.data | 3 | EARTH | Test | 2.8800602 | 811.53119 | 2.6679275 | 162.03818 | 3.0858908 | 0.0540001 |
| healthyR.data | 4 | NNAR | Test | 0.6721281 | 105.92357 | 0.6226221 | 162.10975 | 0.8047144 | 0.0030113 |
| healthyR | 1 | ARIMA | Test | 0.9310757 | 111.86425 | 0.6945183 | 161.16747 | 1.0886999 | 0.0003317 |
| healthyR | 2 | LM | Test | 0.9125345 | 102.99189 | 0.6806878 | 195.11871 | 1.0585381 | 0.0517515 |
| healthyR | 3 | EARTH | Test | 3.8352716 | 1326.13346 | 2.8608480 | 166.94269 | 4.1491885 | 0.0517515 |
| healthyR | 4 | NNAR | Test | 0.9594251 | 127.58262 | 0.7156650 | 172.88366 | 1.1013981 | 0.0116668 |
| healthyR.ts | 1 | ARIMA | Test | 0.9355858 | 119.67868 | 0.6120620 | 131.80229 | 1.1253913 | 0.0939987 |
| healthyR.ts | 2 | LM | Test | 0.9387061 | 127.89985 | 0.6141033 | 128.70464 | 1.1212090 | 0.0939987 |
| healthyR.ts | 3 | EARTH | Test | 0.9395105 | 129.55880 | 0.6146296 | 128.14544 | 1.1208329 | 0.0939987 |
| healthyR.ts | 4 | NNAR | Test | 0.9828146 | 141.02899 | 0.6429592 | 184.54113 | 1.1789827 | 0.0244760 |
| healthyverse | 1 | ARIMA | Test | 0.5959333 | 169.41345 | 0.7379597 | 115.64398 | 0.7076185 | 0.0549150 |
| healthyverse | 2 | LM | Test | 0.6413721 | 268.84015 | 0.7942278 | 104.84167 | 0.7981236 | 0.2261727 |
| healthyverse | 3 | EARTH | Test | 0.5966690 | 131.58017 | 0.7388708 | 136.22145 | 0.7056151 | 0.2261727 |
| healthyverse | 4 | NNAR | Test | 0.5950443 | 117.40595 | 0.7368589 | 119.28404 | 0.7096856 | 0.0191859 |
| healthyR.ai | 1 | ARIMA | Test | 0.7967607 | 98.95059 | 0.6834275 | 171.84730 | 0.9273090 | 0.0034612 |
| healthyR.ai | 2 | LM | Test | 0.7975883 | 100.02609 | 0.6841374 | 157.51796 | 0.9452166 | 0.1421961 |
| healthyR.ai | 3 | EARTH | Test | 3.3824320 | 758.95104 | 2.9013065 | 165.26017 | 3.6060106 | 0.1421961 |
| healthyR.ai | 4 | NNAR | Test | 0.8101695 | 100.23643 | 0.6949290 | 162.35076 | 0.9603415 | 0.0063087 |
| TidyDensity | 1 | ARIMA | Test | 0.6011776 | 155.84382 | 0.6588063 | 106.07590 | 0.7072012 | 0.1678470 |
| TidyDensity | 2 | LM | Test | 0.6756282 | 239.83319 | 0.7403938 | 107.18496 | 0.7908273 | 0.0836639 |
| TidyDensity | 3 | EARTH | Test | 0.6354340 | 171.27919 | 0.6963466 | 113.12032 | 0.7549737 | 0.0836639 |
| TidyDensity | 4 | NNAR | Test | 0.6264240 | 133.35155 | 0.6864729 | 134.33421 | 0.7607771 | 0.0654111 |
| tidyAML | 1 | ARIMA | Test | 0.5958285 | 116.46145 | 0.7034609 | 94.46995 | 0.7318218 | 0.2134834 |
| tidyAML | 2 | LM | Test | 0.6529534 | 128.38021 | 0.7709049 | 94.23374 | 0.8199684 | 0.0000933 |
| tidyAML | 3 | EARTH | Test | 3.4511666 | 708.97120 | 4.0745963 | 186.08476 | 3.7613047 | 0.0000933 |
| tidyAML | 4 | NNAR | Test | 0.6481338 | 127.96325 | 0.7652147 | 105.48952 | 0.7809451 | 0.0877318 |
| RandomWalker | 1 | ARIMA | Test | 1.1041960 | 86.62239 | 0.5050346 | 124.47381 | 1.3337539 | 0.1339997 |
| RandomWalker | 2 | LM | Test | 1.2637600 | 111.11432 | 0.5780156 | 196.00669 | 1.4315064 | 0.0017665 |
| RandomWalker | 3 | EARTH | Test | 1.2155708 | 85.39873 | 0.5559749 | 152.18575 | 1.4441729 | NA |
| RandomWalker | 4 | NNAR | Test | 1.6091287 | 171.76099 | 0.7359796 | 177.92784 | 1.8045084 | 0.4117428 |

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
    ## 1 healthyR.da…         4 NNAR        Test  0.672 106.  0.623 162.  0.805 0.00301
    ## 2 healthyR             2 LM          Test  0.913 103.  0.681 195.  1.06  0.0518 
    ## 3 healthyR.ts          3 EARTH       Test  0.940 130.  0.615 128.  1.12  0.0940 
    ## 4 healthyverse         3 EARTH       Test  0.597 132.  0.739 136.  0.706 0.226  
    ## 5 healthyR.ai          1 ARIMA       Test  0.797  99.0 0.683 172.  0.927 0.00346
    ## 6 TidyDensity          1 ARIMA       Test  0.601 156.  0.659 106.  0.707 0.168  
    ## 7 tidyAML              1 ARIMA       Test  0.596 116.  0.703  94.5 0.732 0.213  
    ## 8 RandomWalker         1 ARIMA       Test  1.10   86.6 0.505 124.  1.33  0.134

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1487|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1480|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1426|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1397|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1223|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [1077|28]> <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [692|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [126|28]>  <mdl_tm_t [1 × 5]>

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
