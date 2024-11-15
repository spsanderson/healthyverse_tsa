Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
15 November, 2024

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 120,912
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

The last day in the data set is 2024-11-13 23:32:04, the file was
birthed on: 2022-07-02 23:58:17.511888, and at report knit time is
-2.075556^{4} hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 120912        |
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
| r_version     |     85430 |          0.29 |   5 |   5 |     0 |       44 |          0 |
| r_arch        |     85430 |          0.29 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     85430 |          0.29 |   7 |  15 |     0 |       20 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       60 |          0 |
| country       |     10381 |          0.91 |   2 |   2 |     0 |      160 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2024-11-13 | 2023-03-17 | 1452 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1160179.31 | 1544300.66 | 355 | 14701 | 260517.5 | 2369217 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 10334.21 | 18024.26 | 1 | 317 | 3098.0 | 11700 | 143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2024-11-13 23:32:04 | 2023-03-17 17:25:56 | 73380 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-------:|---------:|
| time          |         0 |             1 |   0 |  59 |     25 |       60 |

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
    ## -154.37  -34.26   -9.70   26.22  800.66 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -1.815e+02  8.317e+01
    ## date                                                1.087e-02  4.409e-03
    ## lag(value, 1)                                       1.401e-01  2.583e-02
    ## lag(value, 7)                                       9.930e-02  2.684e-02
    ## lag(value, 14)                                      1.113e-01  2.695e-02
    ## lag(value, 21)                                      2.671e-02  2.712e-02
    ## lag(value, 28)                                      8.028e-02  2.694e-02
    ## lag(value, 35)                                      7.047e-02  2.707e-02
    ## lag(value, 42)                                      3.746e-02  2.710e-02
    ## lag(value, 49)                                      1.038e-01  2.696e-02
    ## month(date, label = TRUE).L                        -1.077e+01  5.630e+00
    ## month(date, label = TRUE).Q                         1.660e+00  5.532e+00
    ## month(date, label = TRUE).C                        -1.199e+01  5.570e+00
    ## month(date, label = TRUE)^4                        -9.006e+00  5.515e+00
    ## month(date, label = TRUE)^5                        -1.423e+01  5.471e+00
    ## month(date, label = TRUE)^6                        -2.457e+00  5.508e+00
    ## month(date, label = TRUE)^7                        -1.003e+01  5.352e+00
    ## month(date, label = TRUE)^8                        -2.950e+00  5.304e+00
    ## month(date, label = TRUE)^9                         3.864e+00  5.273e+00
    ## month(date, label = TRUE)^10                        4.940e+00  5.266e+00
    ## month(date, label = TRUE)^11                       -6.119e+00  5.279e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.156e+01  2.468e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  6.447e+00  2.578e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.182 0.029303 *  
    ## date                                                 2.465 0.013831 *  
    ## lag(value, 1)                                        5.423 6.93e-08 ***
    ## lag(value, 7)                                        3.700 0.000224 ***
    ## lag(value, 14)                                       4.130 3.84e-05 ***
    ## lag(value, 21)                                       0.985 0.324882    
    ## lag(value, 28)                                       2.981 0.002928 ** 
    ## lag(value, 35)                                       2.603 0.009345 ** 
    ## lag(value, 42)                                       1.383 0.167022    
    ## lag(value, 49)                                       3.852 0.000123 ***
    ## month(date, label = TRUE).L                         -1.913 0.056000 .  
    ## month(date, label = TRUE).Q                          0.300 0.764238    
    ## month(date, label = TRUE).C                         -2.153 0.031526 *  
    ## month(date, label = TRUE)^4                         -1.633 0.102685    
    ## month(date, label = TRUE)^5                         -2.601 0.009389 ** 
    ## month(date, label = TRUE)^6                         -0.446 0.655577    
    ## month(date, label = TRUE)^7                         -1.874 0.061207 .  
    ## month(date, label = TRUE)^8                         -0.556 0.578131    
    ## month(date, label = TRUE)^9                          0.733 0.463829    
    ## month(date, label = TRUE)^10                         0.938 0.348343    
    ## month(date, label = TRUE)^11                        -1.159 0.246637    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -4.687 3.05e-06 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   2.501 0.012493 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 57.91 on 1380 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2605, Adjusted R-squared:  0.2487 
    ## F-statistic: 22.09 on 22 and 1380 DF,  p-value: < 2.2e-16

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
    ## 1 healthyR.data <tibble [1,418 × 2]> <tibble [28 × 2]> <split [1390|28]>
    ## 2 healthyR      <tibble [1,411 × 2]> <tibble [28 × 2]> <split [1383|28]>
    ## 3 <NA>          <tibble [27 × 2]>    <tibble [28 × 2]> <split [0|27]>   
    ## 4 healthyR.ts   <tibble [1,357 × 2]> <tibble [28 × 2]> <split [1329|28]>
    ## 5 healthyverse  <tibble [1,328 × 2]> <tibble [28 × 2]> <split [1300|28]>
    ## 6 healthyR.ai   <tibble [1,154 × 2]> <tibble [28 × 2]> <split [1126|28]>
    ## 7 TidyDensity   <tibble [1,008 × 2]> <tibble [28 × 2]> <split [980|28]> 
    ## 8 tidyAML       <tibble [624 × 2]>   <tibble [28 × 2]> <split [596|28]> 
    ## 9 RandomWalker  <tibble [58 × 2]>    <tibble [28 × 2]> <split [30|28]>

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

| package | .model_id | .model_desc | .type | mae | mape | mase | smape | rmse | rsq |
|:---|---:|:---|:---|---:|---:|---:|---:|---:|---:|
| healthyR.data | 1 | ARIMA | Test | 0.7112740 | 121.24351 | 0.6603024 | 159.46941 | 0.8358499 | 0.0000002 |
| healthyR.data | 2 | LM | Test | 0.7402847 | 131.72947 | 0.6872341 | 139.40994 | 0.8561331 | 0.0417413 |
| healthyR.data | 3 | EARTH | Test | 0.7645475 | 147.14697 | 0.7097582 | 134.12316 | 0.8903366 | 0.0417413 |
| healthyR.data | 4 | NNAR | Test | 0.7371814 | 114.01832 | 0.6843532 | 178.20105 | 0.8819363 | 0.0389419 |
| healthyR | 1 | ARIMA | Test | 0.6388741 | 114.16314 | 0.7485711 | 155.46597 | 0.7555960 | 0.0442078 |
| healthyR | 2 | LM | Test | 0.6694621 | 107.82943 | 0.7844112 | 180.21930 | 0.8120400 | 0.0014984 |
| healthyR | 3 | EARTH | Test | 0.6459769 | 164.01479 | 0.7568935 | 131.38474 | 0.7733612 | 0.0014984 |
| healthyR | 4 | NNAR | Test | 0.6657336 | 116.24062 | 0.7800425 | 159.58710 | 0.8200307 | 0.0457767 |
| NA | 1 | NULL | NA | NA | NA | NA | NA | NA | NA |
| NA | 2 | NULL | NA | NA | NA | NA | NA | NA | NA |
| NA | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| NA | 4 | NULL | NA | NA | NA | NA | NA | NA | NA |
| healthyR.ts | 1 | ARIMA | Test | 0.7680529 | 484.36903 | 0.8290711 | 114.92535 | 0.9334112 | 0.0399758 |
| healthyR.ts | 2 | LM | Test | 0.7642543 | 367.19950 | 0.8249707 | 122.18188 | 0.9668553 | 0.0398681 |
| healthyR.ts | 3 | EARTH | Test | 0.7623803 | 376.29220 | 0.8229478 | 121.10799 | 0.9631865 | 0.0398681 |
| healthyR.ts | 4 | NNAR | Test | 0.8960989 | 444.72242 | 0.9672898 | 169.27074 | 1.1520159 | 0.0024227 |
| healthyverse | 1 | ARIMA | Test | 0.5037706 | 184.49886 | 0.7484915 | 113.01214 | 0.6124219 | 0.1233180 |
| healthyverse | 2 | LM | Test | 0.5776788 | 309.49317 | 0.8583027 | 97.20465 | 0.7174099 | 0.0030752 |
| healthyverse | 3 | EARTH | Test | 0.8642920 | 397.27898 | 1.2841464 | 110.45464 | 1.0055715 | 0.0030752 |
| healthyverse | 4 | NNAR | Test | 0.5298610 | 158.94862 | 0.7872560 | 111.51730 | 0.6466785 | 0.0357914 |
| healthyR.ai | 1 | ARIMA | Test | 0.6567217 | 129.16508 | 0.7515666 | 165.53675 | 0.7830443 | 0.1294716 |
| healthyR.ai | 2 | LM | Test | 0.6560107 | 124.06909 | 0.7507529 | 138.29706 | 0.8536949 | 0.0014337 |
| healthyR.ai | 3 | EARTH | Test | 0.7150219 | 196.95708 | 0.8182867 | 153.72247 | 0.8245752 | 0.0014337 |
| healthyR.ai | 4 | NNAR | Test | 0.6354063 | 110.61468 | 0.7271727 | 146.89397 | 0.8120653 | 0.0310345 |
| TidyDensity | 1 | ARIMA | Test | 0.6287414 | 519.37933 | 0.6752342 | 114.02708 | 0.7221868 | 0.2148629 |
| TidyDensity | 2 | LM | Test | 0.7132972 | 709.67932 | 0.7660425 | 116.13001 | 0.8038073 | 0.0096144 |
| TidyDensity | 3 | EARTH | Test | 0.6328649 | 430.62614 | 0.6796626 | 118.86063 | 0.7691559 | 0.0096144 |
| TidyDensity | 4 | NNAR | Test | 0.5818050 | 132.90056 | 0.6248270 | 128.06453 | 0.7878456 | 0.1542608 |
| tidyAML | 1 | ARIMA | Test | 0.4564086 | 327.45364 | 0.7134467 | 94.09341 | 0.5964034 | 0.0118987 |
| tidyAML | 2 | LM | Test | 0.4745539 | 370.69320 | 0.7418109 | 93.17252 | 0.5965659 | 0.0153465 |
| tidyAML | 3 | EARTH | Test | 0.4671577 | 401.41643 | 0.7302494 | 89.73248 | 0.5923614 | 0.0153465 |
| tidyAML | 4 | NNAR | Test | 0.4803841 | 537.87820 | 0.7509246 | 82.83832 | 0.6508149 | 0.2702433 |
| RandomWalker | 1 | ARIMA | Test | 0.9405121 | 86.68743 | 0.6675107 | 121.83866 | 1.2031092 | 0.0047736 |
| RandomWalker | 2 | LM | Test | 1.7380561 | 245.95539 | 1.2335526 | 120.67219 | 2.1330700 | 0.0154858 |
| RandomWalker | 3 | EARTH | Test | 0.9782583 | 103.96312 | 0.6943004 | 99.62883 | 1.3538373 | NA |
| RandomWalker | 4 | NNAR | Test | 1.2544538 | 204.03383 | 0.8903250 | 122.51762 | 1.5716829 | 0.0763783 |

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
    ## 1 healthyR.da…         1 ARIMA       Test  0.711 121.  0.660 159.  0.836 2.35e-7
    ## 2 healthyR             1 ARIMA       Test  0.639 114.  0.749 155.  0.756 4.42e-2
    ## 3 healthyR.ts          1 ARIMA       Test  0.768 484.  0.829 115.  0.933 4.00e-2
    ## 4 healthyverse         1 ARIMA       Test  0.504 184.  0.748 113.  0.612 1.23e-1
    ## 5 healthyR.ai          1 ARIMA       Test  0.657 129.  0.752 166.  0.783 1.29e-1
    ## 6 TidyDensity          1 ARIMA       Test  0.629 519.  0.675 114.  0.722 2.15e-1
    ## 7 tidyAML              3 EARTH       Test  0.467 401.  0.730  89.7 0.592 1.53e-2
    ## 8 RandomWalker         1 ARIMA       Test  0.941  86.7 0.668 122.  1.20  4.77e-3

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1390|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1383|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1329|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1300|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1126|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [980|28]>  <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [596|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [30|28]>   <mdl_tm_t [1 × 5]>

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
