---
title: "Data Visualization for Exploratory Data Analysis"
output: 
  html_document:
    keep_md: true
    toc: true
    toc_float: true
---

# Data Visualization Project 03


In this exercise, you will explore methods to create different types of data visualizations (such as plotting text data, or exploring the distributions of continuous variables).


## PART 1: Density Plots

Using the dataset obtained from FSU's [Florida Climate Center](https://climatecenter.fsu.edu/climate-data-access-tools/downloadable-data), for a station at Tampa International Airport (TPA) for 2022, attempt to recreate the charts shown below which were generated using data from 2016. 


``` r
library(tidyverse)
weather_tpa <- read_csv("https://raw.githubusercontent.com/aalhamadani/datasets/master/tpa_weather_2022.csv")
# random sample 
sample_n(weather_tpa, 4)
```

```
## # A tibble: 4 × 7
##    year month   day precipitation max_temp min_temp ave_temp
##   <dbl> <dbl> <dbl>         <dbl>    <dbl>    <dbl>    <dbl>
## 1  2022    10    11       0.00001       89       74     81.5
## 2  2022    10    25       0             83       69     76  
## 3  2022     6    13       0             92       81     86.5
## 4  2022     4     7       1.34          81       71     76
```

See Slides from Week 4 of Visualizing Relationships and Models (slide 10) for a reminder on how to use this type of dataset with the `lubridate` package for dates and times (example included in the slides uses data from 2016).

Using the 2022 data: 

(a) Create a plot like the one below:

<img src="https://raw.githubusercontent.com/aalhamadani/dataviz_final_project/main/figures/tpa_max_temps_facet.png" width="80%" style="display: block; margin: auto;" />

``` r
library(lubridate) 
tpa <- weather_tpa %>% unite("date", year, month, day, sep = "-") %>% 
  mutate(date = ymd(date)) %>% mutate(month = month(date, label = TRUE, abbr = FALSE), 
                                      month = factor(month, levels = month.name)) 

cutie_colors <- c("#B6A6E3", "#DA9EC9", "#FF75A0", "#FBAED2", "#FDC5F5", "#FFC6A8", 
                  "#C5DCA0", "#B5EAD7", "#BFEFFF", "#87CEFA", "#A3C4F3", "#8AB6D6")

ggplot(tpa, aes(x = max_temp, fill = month)) + 
  geom_histogram(binwidth = 3, color = "white") +
  scale_fill_manual(values = cutie_colors) + facet_wrap(~ month, nrow = 3, ncol = 4) +
  labs(x = "Maximum Temperature", y = "Number of Days") + theme_minimal() + 
  theme(strip.background = element_rect(fill = "lightgray", color = "gray30", size = 2),
  strip.text = element_text(size = 13),  legend.position = "none", 
  panel.border = element_rect(color = "gray30", fill = NA, size = 2), panel.spacing = unit(0.5, "lines"),
  axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14), 
  axis.title.x = element_text(size = 18), axis.title.y = element_text(size = 18))
```

```
## Warning: The `size` argument of `element_rect()` is deprecated as of ggplot2 3.4.0.
## ℹ Please use the `linewidth` argument instead.
## This warning is displayed once every 8 hours.
## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
## generated.
```

![](bailey_project_03_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

(b) Create a plot like the one below:

<img src="https://raw.githubusercontent.com/aalhamadani/dataviz_final_project/main/figures/tpa_max_temps_density.png" width="80%" style="display: block; margin: auto;" />


``` r
ggplot(tpa, aes(x = max_temp)) + 
  geom_density(kernel = "epanechnikov", bw = 0.5, fill = "#FFB3C6", color = "#FF75A0", size = 1.1) +
  labs(x = "Maximum Temperature", y = "Density") + theme_minimal() +
  theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 18), axis.title.y = element_text(size = 18))
```

```
## Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
## ℹ Please use `linewidth` instead.
## This warning is displayed once every 8 hours.
## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
## generated.
```

![](bailey_project_03_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

(c) Create a plot like the one below:

<img src="https://raw.githubusercontent.com/aalhamadani/dataviz_final_project/main/figures/tpa_max_temps_density_facet.png" width="80%" style="display: block; margin: auto;" />


``` r
ggplot(tpa, aes(x = max_temp, fill = month)) + geom_density() +
  scale_fill_manual(values = cutie_colors) + facet_wrap(~ month, nrow = 3, ncol = 4) +
  labs(title = "Density plots for each month in 2022", x = "Maximum Temperatures", y = "") +
  theme_minimal() + theme(strip.background = element_rect(fill = "lightgray", color = "gray30", size = 2),
    strip.text = element_text(size = 13, face = "bold", hjust = 0.5), legend.position = "none",
    panel.border = element_rect(color = "gray30", fill = NA, size = 2), panel.spacing = unit(0.5, "lines"), 
    axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14), 
    plot.title = element_text(size = 20), axis.title.x = element_text(size = 18))
```

![](bailey_project_03_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

(d) Generate a plot like the chart below:

<img src="https://raw.githubusercontent.com/aalhamadani/dataviz_final_project/main/figures/tpa_max_temps_ridges_plasma.png" width="80%" style="display: block; margin: auto;" />


``` r
library(ggridges)
ggplot(tpa, aes(x = max_temp, y = month, fill = ..x..)) +
  geom_density_ridges_gradient(quantile_lines = TRUE, quantiles = 0.5,
    scale = 1.2, color = "black", size = 0.3) +
  scale_fill_gradientn(colors = c("#FFE0EF", "#FFB3D9", "#FF80BF", "#FF4DA6", "#D10072"), name = "") +
  labs(x = "Maximum Temperature", y = "") + theme_minimal(base_size = 14) +
  theme(axis.title = element_text(size = 16), axis.text = element_text(size = 14), legend.position = "right")
```

```
## Warning in geom_density_ridges_gradient(quantile_lines = TRUE, quantiles = 0.5,
## : Ignoring unknown parameters: `size`
```

```
## Warning: The dot-dot notation (`..x..`) was deprecated in ggplot2 3.4.0.
## ℹ Please use `after_stat(x)` instead.
## This warning is displayed once every 8 hours.
## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
## generated.
```

```
## Picking joint bandwidth of 1.93
```

![](bailey_project_03_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

(e) Create a plot of your choice that uses the attribute for precipitation _(values of -99.9 for temperature or -99.99 for precipitation represent missing data)_.


``` r
tpa <- tpa %>% filter(precipitation != -99.99) %>% mutate(day = day(date))
ggplot(tpa, aes(x = day, y = fct_rev(month), fill = precipitation)) +
  geom_tile(color = "white") + scale_fill_gradientn(colors = c("#FFB3D9", "#FF80BF", "#D10072")) +
  scale_x_continuous(breaks = seq(0, 32, 5), expand = c(0, 0)) +
  labs(title = "Daily Precipitation by Month", x = "Day of Month", y = "", fill = "Precip (in.)") +
  theme_minimal() + theme(panel.grid = element_blank(), legend.position = "bottom")
```

![](bailey_project_03_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

``` r
cutie_colors <- c("#9B5DE5", "#C9B9F8", "#F15BB5", "#FDB3E1", "#F9F871", "#FFC75F", 
                  "#FF6F3C", "#7FFF00", "darkgreen", "blue", "#4dd4ff", "#4D96FF")

ggplot(tpa, aes(x = ave_temp, y = precipitation)) +
  geom_point(aes(color = month), alpha = 0.7, size = 2) + geom_smooth(method = "lm", se = FALSE, color = "darkgrey") + 
  geom_smooth(data = subset(tpa, precipitation > 0), method = "lm", color = "darkgrey", linetype = "dashed", se = FALSE) +
  scale_color_manual(values = cutie_colors, name = "") +
  labs(title = "Precipitation vs Average Temperature", x = "Average Temperature", y = "Precipitation (in)") +
  theme_minimal(base_size = 14)
```

```
## `geom_smooth()` using formula = 'y ~ x'
## `geom_smooth()` using formula = 'y ~ x'
```

![](bailey_project_03_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

## PART 2 
### Data on Concrete Strength 

Concrete is the most important material in **civil engineering**. The concrete compressive strength is a highly nonlinear function of _age_ and _ingredients_. The dataset used here is from the [UCI Machine Learning Repository](https://archive.ics.uci.edu/ml/index.php), and it contains 1030 observations with 9 different attributes 9 (8 quantitative input variables, and 1 quantitative output variable). A data dictionary is included below: 


Variable                      |    Notes                
------------------------------|-------------------------------------------
Cement                        | kg in a $m^3$ mixture             
Blast Furnace Slag            | kg in a $m^3$ mixture  
Fly Ash                       | kg in a $m^3$ mixture             
Water                         | kg in a $m^3$ mixture              
Superplasticizer              | kg in a $m^3$ mixture
Coarse Aggregate              | kg in a $m^3$ mixture
Fine Aggregate                | kg in a $m^3$ mixture      
Age                           | in days                                             
Concrete compressive strength | MPa, megapascals


Below we read the `.csv` file using `readr::read_csv()` (the `readr` package is part of the `tidyverse`)


``` r
concrete <- read_csv("~/Downloads/concrete.csv")
```

```
## Rows: 1030 Columns: 9
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## dbl (9): Cement, Blast_Furnace_Slag, Fly_Ash, Water, Superplasticizer, Coars...
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```


Let us create a new attribute for visualization purposes, `strength_range`: 


``` r
new_concrete <- concrete %>%
  mutate(strength_range = cut(Concrete_compressive_strength, 
                              breaks = quantile(Concrete_compressive_strength, 
                                                probs = seq(0, 1, 0.2))))
new_concrete
```

```
## # A tibble: 1,030 × 10
##    Cement Blast_Furnace_Slag Fly_Ash Water Superplasticizer Coarse_Aggregate
##     <dbl>              <dbl>   <dbl> <dbl>            <dbl>            <dbl>
##  1   540                  0        0   162              2.5            1040 
##  2   540                  0        0   162              2.5            1055 
##  3   332.               142.       0   228              0               932 
##  4   332.               142.       0   228              0               932 
##  5   199.               132.       0   192              0               978.
##  6   266                114        0   228              0               932 
##  7   380                 95        0   228              0               932 
##  8   380                 95        0   228              0               932 
##  9   266                114        0   228              0               932 
## 10   475                  0        0   228              0               932 
## # ℹ 1,020 more rows
## # ℹ 4 more variables: Fine_Aggregate <dbl>, Age <dbl>,
## #   Concrete_compressive_strength <dbl>, strength_range <fct>
```



1. Explore the distribution of 2 of the continuous variables available in the dataset. Do ranges make sense? Comment on your findings.


``` r
ggplot(new_concrete) + 
  geom_histogram( aes(x = Age), binwidth = 20, fill = "hotpink", color = "white") +
  labs(title = "Age of Concrete", x = "Age in Days", y = "Count") +
  theme_minimal()
```

![](bailey_project_03_files/figure-html/unnamed-chunk-14-1.png)<!-- -->

``` r
ggplot(new_concrete) +
  geom_histogram(aes(x = Cement), binwidth = 25, fill = "hotpink", color = "white") +
  labs(title = "Cement in Concrete", x = "Cement (kg/m^3)", y = "Count") +
  theme_minimal()
```

![](bailey_project_03_files/figure-html/unnamed-chunk-14-2.png)<!-- -->

``` r
ggplot(new_concrete) +
  geom_histogram(aes(x = Water), binwidth = 10, fill = "hotpink", color = "white") +
  labs(title = "Water in Concrete", x = "Water (kg/m^3)", y = "Count") +
  theme_minimal() 
```

![](bailey_project_03_files/figure-html/unnamed-chunk-14-3.png)<!-- -->

2. Use a _temporal_ indicator such as the one available in the variable `Age` (measured in days). Generate a plot similar to the one shown below. Comment on your results.

<img src="https://raw.githubusercontent.com/aalhamadani/dataviz_final_project/main/figures/concrete_strength.png" width="80%" style="display: block; margin: auto;" />

``` r
cute_colors <- c("#B6A6E3", "#FF75A0", "#FFC6A8", "#B5EAD7", "#87CEFA")
ggplot(new_concrete) +
  geom_boxplot(aes(x = factor(Age), y = Concrete_compressive_strength, fill = strength_range)) +
  scale_fill_manual(values = cute_colors, name = "Strength Range", na.translate = FALSE) +
  labs(x = "Age (in days)", y = "Compressive Strength (in MPa)") + theme_minimal() 
```

![](bailey_project_03_files/figure-html/unnamed-chunk-16-1.png)<!-- -->


3. Create a scatterplot similar to the one shown below. Pay special attention to which variables are being mapped to specific aesthetics of the plot. Comment on your results. 

<img src="https://raw.githubusercontent.com/aalhamadani/dataviz_final_project/main/figures/cement_plot.png" width="80%" style="display: block; margin: auto;" />


``` r
ggplot(new_concrete) +
  geom_point(aes(x = Cement, y = Concrete_compressive_strength, size = Age, color = Water), alpha = 0.7) +
  scale_color_gradientn(colors = c("#FFE0EF", "#FFB3D9", "#FF80BF", "#FF4DA6", "#D10072"), name = "Water (kg/m^3)") + 
  labs(title = "Exploring Strength versus (Cement, Age, and Water)", x = "Cement (kg/m^3)", y = "Strength", 
       caption = "Age is measured in days") +
  theme_minimal() + guides(size = guide_legend(order = 1), color = guide_colorbar(order = 2))
```

![](bailey_project_03_files/figure-html/unnamed-chunk-18-1.png)<!-- -->




