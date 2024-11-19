Time Series Analysis and Nested Modeling of the Healthyverse Packages
================
Steven P. Sanderson II, MPH - Date:
18 November, 2024

This analysis follows a *Nested Modeltime Workflow*.

## Get Data

``` r
glimpse(downloads_tbl)
```

    ## Rows: 121,242
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

The last day in the data set is 2024-11-16 19:39:15, the file was
birthed on: 2022-07-02 23:58:17.511888, and at report knit time is
-2.082368^{4} hours old. Happy analyzing!

Now that we have our data lets take a look at it using the `skimr`
package.

``` r
skim(downloads_tbl)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | downloads_tbl |
| Number of rows                                   | 121242        |
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
| r_version     |     85720 |          0.29 |   5 |   5 |     0 |       44 |          0 |
| r_arch        |     85720 |          0.29 |   3 |   7 |     0 |        5 |          0 |
| r_os          |     85720 |          0.29 |   7 |  15 |     0 |       20 |          0 |
| package       |         0 |          1.00 |   7 |  13 |     0 |        8 |          0 |
| version       |         0 |          1.00 |   5 |  17 |     0 |       60 |          0 |
| country       |     10412 |          0.91 |   2 |   2 |     0 |      160 |          0 |

**Variable type: Date**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date | 0 | 1 | 2020-11-23 | 2024-11-16 | 2023-03-18 | 1455 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean | sd | p0 | p25 | p50 | p75 | p100 | hist |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|:---|
| size | 0 | 1 | 1158401.0 | 1543461.96 | 355 | 14701 | 260378 | 2368362 | 5677952 | ▇▁▂▁▁ |
| ip_id | 0 | 1 | 10332.5 | 18007.86 | 1 | 317 | 3098 | 11769 | 143633 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:---|---:|---:|:---|:---|:---|---:|
| date_time | 0 | 1 | 2020-11-23 09:00:41 | 2024-11-16 19:39:15 | 2023-03-18 16:15:19 | 73510 |

**Variable type: Timespan**

| skim_variable | n_missing | complete_rate | min | max | median | n_unique |
|:--------------|----------:|--------------:|----:|----:|-------:|---------:|
| time          |         0 |             1 |   0 |  59 |     52 |       60 |

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
    ## -155.48  -34.18   -9.54   26.59  800.73 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -1.805e+02  8.310e+01
    ## date                                                1.082e-02  4.405e-03
    ## lag(value, 1)                                       1.370e-01  2.581e-02
    ## lag(value, 7)                                       9.859e-02  2.688e-02
    ## lag(value, 14)                                      1.086e-01  2.695e-02
    ## lag(value, 21)                                      2.988e-02  2.715e-02
    ## lag(value, 28)                                      8.032e-02  2.698e-02
    ## lag(value, 35)                                      6.959e-02  2.712e-02
    ## lag(value, 42)                                      3.714e-02  2.712e-02
    ## lag(value, 49)                                      1.075e-01  2.692e-02
    ## month(date, label = TRUE).L                        -1.089e+01  5.632e+00
    ## month(date, label = TRUE).Q                         1.620e+00  5.537e+00
    ## month(date, label = TRUE).C                        -1.206e+01  5.579e+00
    ## month(date, label = TRUE)^4                        -9.048e+00  5.516e+00
    ## month(date, label = TRUE)^5                        -1.420e+01  5.463e+00
    ## month(date, label = TRUE)^6                        -2.438e+00  5.497e+00
    ## month(date, label = TRUE)^7                        -9.987e+00  5.350e+00
    ## month(date, label = TRUE)^8                        -2.964e+00  5.307e+00
    ## month(date, label = TRUE)^9                         3.918e+00  5.281e+00
    ## month(date, label = TRUE)^10                        4.996e+00  5.275e+00
    ## month(date, label = TRUE)^11                       -6.174e+00  5.289e+00
    ## fourier_vec(date, type = "sin", K = 1, period = 7) -1.168e+01  2.470e+00
    ## fourier_vec(date, type = "cos", K = 1, period = 7)  6.616e+00  2.579e+00
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.172 0.030013 *  
    ## date                                                 2.456 0.014153 *  
    ## lag(value, 1)                                        5.308 1.29e-07 ***
    ## lag(value, 7)                                        3.668 0.000253 ***
    ## lag(value, 14)                                       4.030 5.87e-05 ***
    ## lag(value, 21)                                       1.101 0.271180    
    ## lag(value, 28)                                       2.977 0.002958 ** 
    ## lag(value, 35)                                       2.566 0.010384 *  
    ## lag(value, 42)                                       1.369 0.171097    
    ## lag(value, 49)                                       3.995 6.81e-05 ***
    ## month(date, label = TRUE).L                         -1.933 0.053443 .  
    ## month(date, label = TRUE).Q                          0.293 0.769901    
    ## month(date, label = TRUE).C                         -2.161 0.030841 *  
    ## month(date, label = TRUE)^4                         -1.640 0.101144    
    ## month(date, label = TRUE)^5                         -2.599 0.009454 ** 
    ## month(date, label = TRUE)^6                         -0.444 0.657472    
    ## month(date, label = TRUE)^7                         -1.867 0.062142 .  
    ## month(date, label = TRUE)^8                         -0.559 0.576593    
    ## month(date, label = TRUE)^9                          0.742 0.458312    
    ## month(date, label = TRUE)^10                         0.947 0.343764    
    ## month(date, label = TRUE)^11                        -1.167 0.243250    
    ## fourier_vec(date, type = "sin", K = 1, period = 7)  -4.729 2.48e-06 ***
    ## fourier_vec(date, type = "cos", K = 1, period = 7)   2.565 0.010426 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 58.01 on 1383 degrees of freedom
    ##   (49 observations deleted due to missingness)
    ## Multiple R-squared:  0.2605, Adjusted R-squared:  0.2487 
    ## F-statistic: 22.14 on 22 and 1383 DF,  p-value: < 2.2e-16

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
    ## 1 healthyR.data <tibble [1,421 × 2]> <tibble [28 × 2]> <split [1393|28]>
    ## 2 healthyR      <tibble [1,414 × 2]> <tibble [28 × 2]> <split [1386|28]>
    ## 3 <NA>          <tibble [27 × 2]>    <tibble [28 × 2]> <split [0|27]>   
    ## 4 healthyR.ts   <tibble [1,360 × 2]> <tibble [28 × 2]> <split [1332|28]>
    ## 5 healthyverse  <tibble [1,331 × 2]> <tibble [28 × 2]> <split [1303|28]>
    ## 6 healthyR.ai   <tibble [1,157 × 2]> <tibble [28 × 2]> <split [1129|28]>
    ## 7 TidyDensity   <tibble [1,011 × 2]> <tibble [28 × 2]> <split [983|28]> 
    ## 8 tidyAML       <tibble [627 × 2]>   <tibble [28 × 2]> <split [599|28]> 
    ## 9 RandomWalker  <tibble [61 × 2]>    <tibble [28 × 2]> <split [33|28]>

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
| healthyR.data | 1 | ARIMA | Test | 0.7810376 | 112.50472 | 0.7034284 | 149.74082 | 0.9292708 | 0.0017188 |
| healthyR.data | 2 | LM | Test | 0.7982267 | 124.46119 | 0.7189095 | 138.03599 | 0.9243863 | 0.0971981 |
| healthyR.data | 3 | EARTH | Test | 0.8027372 | 133.19776 | 0.7229718 | 132.32620 | 0.9290722 | 0.0971981 |
| healthyR.data | 4 | NNAR | Test | 0.8200925 | 115.30969 | 0.7386025 | 179.90550 | 0.9835572 | 0.0176729 |
| healthyR | 1 | ARIMA | Test | 0.7237616 | 114.76892 | 0.7620171 | 158.49599 | 0.8365450 | 0.0463836 |
| healthyR | 2 | LM | Test | 0.7358624 | 106.41873 | 0.7747575 | 183.13012 | 0.8730928 | 0.0047224 |
| healthyR | 3 | EARTH | Test | 0.7384760 | 163.25946 | 0.7775093 | 137.90268 | 0.8709153 | 0.0047224 |
| healthyR | 4 | NNAR | Test | 0.7040423 | 118.15219 | 0.7412555 | 158.52869 | 0.8374067 | 0.1289571 |
| NA | 1 | NULL | NA | NA | NA | NA | NA | NA | NA |
| NA | 2 | NULL | NA | NA | NA | NA | NA | NA | NA |
| NA | 3 | NULL | NA | NA | NA | NA | NA | NA | NA |
| NA | 4 | NULL | NA | NA | NA | NA | NA | NA | NA |
| healthyR.ts | 1 | ARIMA | Test | 0.8526869 | 509.67782 | 0.8039143 | 118.18199 | 1.0312388 | 0.0419216 |
| healthyR.ts | 2 | LM | Test | 0.8231722 | 357.16445 | 0.7760878 | 125.09264 | 1.0395543 | 0.0415370 |
| healthyR.ts | 3 | EARTH | Test | 2.0811405 | 1685.38327 | 1.9621021 | 128.93437 | 2.4177335 | 0.0415370 |
| healthyR.ts | 4 | NNAR | Test | 0.8950559 | 442.97305 | 0.8438599 | 154.29354 | 1.1051518 | 0.0291220 |
| healthyverse | 1 | ARIMA | Test | 0.5082383 | 93.72746 | 0.7379245 | 110.58125 | 0.6177964 | 0.1882470 |
| healthyverse | 2 | LM | Test | 0.5613716 | 165.57437 | 0.8150703 | 91.44232 | 0.7047465 | 0.0050818 |
| healthyverse | 3 | EARTH | Test | 0.7499580 | 116.12741 | 1.0888838 | 169.16034 | 0.8958506 | 0.0050818 |
| healthyverse | 4 | NNAR | Test | 0.5248916 | 104.29189 | 0.7621040 | 104.67467 | 0.6563780 | 0.0000550 |
| healthyR.ai | 1 | ARIMA | Test | 0.6971440 | 128.05394 | 0.7294960 | 169.90028 | 0.8318309 | 0.0179374 |
| healthyR.ai | 2 | LM | Test | 0.6829766 | 122.02458 | 0.7146712 | 140.34663 | 0.8803112 | 0.0031181 |
| healthyR.ai | 3 | EARTH | Test | 0.7439483 | 193.49509 | 0.7784723 | 153.60308 | 0.8626060 | 0.0031181 |
| healthyR.ai | 4 | NNAR | Test | 0.6623144 | 109.58744 | 0.6930501 | 141.90215 | 0.8496473 | 0.0185086 |
| TidyDensity | 1 | ARIMA | Test | 0.7147132 | 513.55135 | 0.7076641 | 123.68627 | 0.8185539 | 0.2235159 |
| TidyDensity | 2 | LM | Test | 0.7974121 | 685.04963 | 0.7895474 | 124.13760 | 0.9015823 | 0.0146151 |
| TidyDensity | 3 | EARTH | Test | 0.7249758 | 458.61440 | 0.7178254 | 128.70673 | 0.8487269 | 0.0146151 |
| TidyDensity | 4 | NNAR | Test | 0.6518085 | 132.19660 | 0.6453798 | 142.89746 | 0.8436090 | 0.1420901 |
| tidyAML | 1 | ARIMA | Test | 0.4152941 | 412.84769 | 0.7190458 | 88.42330 | 0.4968922 | 0.0479942 |
| tidyAML | 2 | LM | Test | 0.4339022 | 358.42789 | 0.7512641 | 90.16658 | 0.5093395 | 0.0083732 |
| tidyAML | 3 | EARTH | Test | 0.4258060 | 387.75671 | 0.7372462 | 86.65644 | 0.5074485 | 0.0083732 |
| tidyAML | 4 | NNAR | Test | 0.4586570 | 577.40139 | 0.7941249 | 84.52769 | 0.5671046 | 0.0226041 |
| RandomWalker | 1 | ARIMA | Test | 1.1063046 | 104.96045 | 0.6863441 | 115.24143 | 1.4312822 | 0.0201624 |
| RandomWalker | 2 | LM | Test | 1.7448823 | 230.52447 | 1.0825134 | 124.05367 | 2.1352879 | 0.0043354 |
| RandomWalker | 3 | EARTH | Test | 1.1060859 | 109.36517 | 0.6862083 | 107.97889 | 1.4641289 | NA |
| RandomWalker | 4 | NNAR | Test | 1.0766846 | 103.65364 | 0.6679680 | 115.28360 | 1.3964544 | 0.0372556 |

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
    ## 1 healthyR.data         2 LM          Test  0.798 124.  0.719 138.  0.924 0.0972
    ## 2 healthyR              1 ARIMA       Test  0.724 115.  0.762 158.  0.837 0.0464
    ## 3 healthyR.ts           1 ARIMA       Test  0.853 510.  0.804 118.  1.03  0.0419
    ## 4 healthyverse          1 ARIMA       Test  0.508  93.7 0.738 111.  0.618 0.188 
    ## 5 healthyR.ai           1 ARIMA       Test  0.697 128.  0.729 170.  0.832 0.0179
    ## 6 TidyDensity           1 ARIMA       Test  0.715 514.  0.708 124.  0.819 0.224 
    ## 7 tidyAML               1 ARIMA       Test  0.415 413.  0.719  88.4 0.497 0.0480
    ## 8 RandomWalker          4 NNAR        Test  1.08  104.  0.668 115.  1.40  0.0373

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
    ## 1 healthyR.data <tibble>     <tibble>     <split [1393|28]> <mdl_tm_t [1 × 5]>
    ## 2 healthyR      <tibble>     <tibble>     <split [1386|28]> <mdl_tm_t [1 × 5]>
    ## 3 healthyR.ts   <tibble>     <tibble>     <split [1332|28]> <mdl_tm_t [1 × 5]>
    ## 4 healthyverse  <tibble>     <tibble>     <split [1303|28]> <mdl_tm_t [1 × 5]>
    ## 5 healthyR.ai   <tibble>     <tibble>     <split [1129|28]> <mdl_tm_t [1 × 5]>
    ## 6 TidyDensity   <tibble>     <tibble>     <split [983|28]>  <mdl_tm_t [1 × 5]>
    ## 7 tidyAML       <tibble>     <tibble>     <split [599|28]>  <mdl_tm_t [1 × 5]>
    ## 8 RandomWalker  <tibble>     <tibble>     <split [33|28]>   <mdl_tm_t [1 × 5]>

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
