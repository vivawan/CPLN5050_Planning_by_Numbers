---
title: "Assignment #2: Regression"
author: "Viva and Yaohan"
date: "2024-03-30"
output: 
  html_document:
    keep_md: yes
    toc: yes
    theme: flatly
    toc_float: yes
    code_folding: hide
    number_sections: no
    fontsize: 12pt
  pdf_document:
    toc: yes
---



# Task1

**Created Variables:**

Social Characteristics: 
1. Population Density (population/land_acre) 
2. Median Age (median_age & quartile) 
3. Education Level (edu_ba_above/pop_above_25 dummy) 
4. Non-White Population Share (1 - white/population)

Economic Characteristics: 
1. Job Density (tot_jobs/land_acre) 
2. Poverty Rate (below_pov/population) 
3. Median Household Income (income & quartile) 
4. No Vehicle Household Share (hh_no_veh/total_hh)

Spatial Characteristics: 
1. Land Use (com_share + indu_share) 
2. District (district dummy)

Multimodal Characteristics: 
1. Indego Station (indego_station dummy) 
2. Walk Score (walk_score, above/below mean, dummy) 
3. Transit Score (transit_score_d, above/below mean, dummy) 
4. Worker Drive Alone (drove_alone/total_worker)
5. Worker Take Transit (transit/total_worker) 
6. Average Pedestrian Trips per Person (ped_trips/population) 
7. Traffic Volume (total_aad quartile) 
8. Accessibility to Subway Station (station_buffer dummy)


```r
taxi_dat <- dat %>%
  filter(population > 0) %>% # Note reason for remove population = 0
  # create social variables
  mutate(pop_density = round(ifelse(land_acre > 0, population/land_acre, 0), digits = 2), # population/acre
         median_age_cat = cut(
           median_age, breaks = quantile(median_age, probs = c(0, 0.25, 0.5, 0.75, 1)),
           labels = c("age_1st", "age_2nd", "age_3rd", "age_4th"),
           include.lowest = TRUE), # median_age quartile 
         edu_ba_per = round(ifelse(pop_above_25> 0, edu_ba_above/pop_above_25, 0) * 100, digits = 2), # %
         edu_level_dum = ifelse(edu_ba_per >= mean(edu_ba_per), "above_ave", "below_ave"), # dummy
         non_white_per = round((1 - white/population) * 100, digits = 2)) %>% # %
  # create economic variables
  mutate(job_density = round(ifelse(land_acre > 0, tot_jobs/land_acre, 0), digits = 2), # employment/acre
         poverty_rate = round((below_pov/population) * 100, digits = 2), # %
         income_cat = cut(
           income, breaks = quantile(income, probs = c(0, 0.25, 0.5, 0.75, 1)),
           labels = c("income_1st", "income_2nd", "income_3rd", "income_4th"),
           include.lowest = TRUE), # median_hh_income quartile
         no_veh_hh_per = round(ifelse(total_hh > 0, hh_no_veh/total_hh, 0) * 100, digits = 2)) %>% # %
  # create spatial variables
  mutate(com_indu_per = round(com_share + indu_share, digits = 2), # %
         district_dum = ifelse(district == "Central", "central", "non_central")) %>% # dummy
  # create multimodal variables
  mutate(inde_station_dum = ifelse(indego_station > 0, "indego", "non_indego"),
         walk_score_dum = ifelse(walk_score >= mean(walk_score), "above_ave", "below_ave"), # dummy
         transit_score_dum = ifelse(transit_score_d >= mean(transit_score_d), "above_ave", "below_ave"), # dummy
         drive_per = round(ifelse(total_worker > 0, drove_alone/total_worker, 0) * 100, digits = 2), # %
         transit_per = round(ifelse(total_worker > 0, transit/total_worker, 0) * 100, digits = 2), # %
         ped_trips_pp = round(ifelse(population > 0, ped_trips/population, 0), digits = 2), # trips/person
         subway_dum = ifelse(station_buffer == 1, "subway", "non_subway")) %>% # dummy 
  select(GEOID, tract, taxi_trip_2015, taxi_trip_2017,
         pop_density, median_age, median_age_cat, edu_ba_per, edu_level_dum, non_white_per,
         job_density, poverty_rate, income, income_cat, no_veh_hh_per,
         com_indu_per, district_dum, inde_station_dum, walk_score, walk_score_dum,
         transit_score_d, transit_score_dum, drive_per, transit_per, ped_trips_pp, subway_dum)
```

# Task 2


```r
#create district boundary
union_district <- dat %>%
  group_by(district) %>% # Group by the district column
  summarise()%>%
  mutate(centroid = st_centroid(geometry)) %>% 
  mutate(lon = st_coordinates(centroid)[,1], lat = st_coordinates(centroid)[,2])


# The map of taxi trips by Census tract in Philadelphia in 2017
breaks_quantiles <- classIntervals(taxi_dat$taxi_trip_2017, n = 5, style = "quantile")
colors <- brewer.pal(n = 5, name = "Blues")
labels <- paste0(formatC(breaks_quantiles$brks[-length(breaks_quantiles$brks)], format = "f", digits = 0, big.mark = ","), 
                 " - ", 
                 formatC(breaks_quantiles$brks[-1], format = "f", digits = 0, big.mark = ","))

ggplot(taxi_dat) +
  geom_sf(aes(fill = cut(taxi_trip_2017, breaks = breaks_quantiles$brks, include.lowest = TRUE)), color = "transparent") +
  scale_fill_manual(values = colors,
                    labels = labels,
                    name = "Taxi Trips (Quantile)") +
  labs(title = "Taxi Trips by Census Tract in Philadelphia, 2017") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = c(1, 0.2))+
  geom_sf(data = union_district, fill = NA, color = "grey", inherit.aes = FALSE) +
  geom_text(data = union_district, aes(x = lon, y = lat, label = district), size = 1.75,
            color = "black",check_overlap = TRUE, inherit.aes = FALSE)
```

![](Assignment-2_Viva-Yaohan_files/figure-html/task2-1.png)<!-- -->

```r
# The map of changes in taxi trips between 2015 and 2017
taxi_change <- taxi_dat %>%
  mutate(change_trips = taxi_trip_2017 - taxi_trip_2015)

breaks_quantiles_change <- classIntervals(taxi_change$change_trips, n = 5, style = "quantile")
colors_heat <- rev(brewer.pal(n = 5, name = "YlOrRd"))
labels_heat <- paste0(formatC(breaks_quantiles_change$brks[-length(breaks_quantiles_change$brks)], format = "f", digits = 0, big.mark = ","), 
                 " - ", 
                 formatC(breaks_quantiles_change$brks[-1], format = "f", digits = 0, big.mark = ","))

ggplot(taxi_change) +
  geom_sf(aes(fill = cut(change_trips, breaks = breaks_quantiles_change$brks, include.lowest = TRUE)), color = "transparent") +
  scale_fill_manual(values = colors_heat,
                    labels = labels_heat,
                    name = "Change of Taxi Trips (Quantile)") +
  labs(title = "Change in Taxi Trips in Philadelphia, 2015 - 2017") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = c(1, 0.2))+
  geom_sf(data = union_district, fill = NA, color = "grey", inherit.aes = FALSE) +
  geom_text(data = union_district, aes(x = lon, y = lat, label = district), size = 1.75,
            color = "black",check_overlap = TRUE, inherit.aes = FALSE)
```

![](Assignment-2_Viva-Yaohan_files/figure-html/task2-2.png)<!-- -->

# Task 3


```r
# histogram of taxi trips in 2017
ggplot(taxi_dat) +
  geom_histogram(aes(x = taxi_trip_2017), bins = 15, fill = "white", color = "black") +
  labs(title = "Histogram of Philadelphia Taxi Trips in 2017",
       x = "Taxi Trips",
       y = "Frequency") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
```

![](Assignment-2_Viva-Yaohan_files/figure-html/task 3-1.png)<!-- -->

```r
# histogram of taxi trips (logged) in 2017
ggplot(taxi_dat) +
  geom_histogram(aes(x = log(taxi_trip_2017)), bins = 15, fill = "white", color = "black") +
  labs(title = "Histogram of Philadelphia Taxi Trips (logged) in 2017",
       x = "Taxi Trips (logged)",
       y = "Frequency") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
```

![](Assignment-2_Viva-Yaohan_files/figure-html/task 3-2.png)<!-- -->

# Task 4


```r
par(mfrow = c(2, 2))

## continuous independent variables
### pop_density
plot(taxi_dat$pop_density, taxi_dat$taxi_trip_2017)
plot(log(taxi_dat$pop_density + 1), taxi_dat$taxi_trip_2017)
plot(taxi_dat$pop_density, log(taxi_dat$taxi_trip_2017))
plot(log(taxi_dat$pop_density + 1), log(taxi_dat$taxi_trip_2017))
```

![](Assignment-2_Viva-Yaohan_files/figure-html/task4-1.png)<!-- -->

```r
### median age
plot(taxi_dat$median_age, taxi_dat$taxi_trip_2017)
plot(log(taxi_dat$median_age + 1), taxi_dat$taxi_trip_2017)
plot(taxi_dat$median_age, log(taxi_dat$taxi_trip_2017))
plot(log(taxi_dat$median_age + 1), log(taxi_dat$taxi_trip_2017))
```

![](Assignment-2_Viva-Yaohan_files/figure-html/task4-2.png)<!-- -->

```r
### edu_ba_per
plot(taxi_dat$edu_ba_per, taxi_dat$taxi_trip_2017)
plot(log(taxi_dat$edu_ba_per + 1), taxi_dat$taxi_trip_2017)
plot(taxi_dat$edu_ba_per, log(taxi_dat$taxi_trip_2017))
plot(log(taxi_dat$edu_ba_per + 1), log(taxi_dat$taxi_trip_2017))
```

![](Assignment-2_Viva-Yaohan_files/figure-html/task4-3.png)<!-- -->

```r
### non_white_per
plot(taxi_dat$non_white_per, taxi_dat$taxi_trip_2017)
plot(log(taxi_dat$non_white_per + 1), taxi_dat$taxi_trip_2017)
plot(taxi_dat$non_white_per, log(taxi_dat$taxi_trip_2017))
plot(log(taxi_dat$non_white_per + 1), log(taxi_dat$taxi_trip_2017))
```

![](Assignment-2_Viva-Yaohan_files/figure-html/task4-4.png)<!-- -->

```r
### job_density
plot(taxi_dat$job_density, taxi_dat$taxi_trip_2017)
plot(log(taxi_dat$job_density + 1), taxi_dat$taxi_trip_2017)
plot(taxi_dat$job_density, log(taxi_dat$taxi_trip_2017))
plot(log(taxi_dat$job_density + 1), log(taxi_dat$taxi_trip_2017))
```

![](Assignment-2_Viva-Yaohan_files/figure-html/task4-5.png)<!-- -->

```r
### poverty_rate
plot(taxi_dat$poverty_rate, taxi_dat$taxi_trip_2017)
plot(log(taxi_dat$poverty_rate + 1), taxi_dat$taxi_trip_2017)
plot(taxi_dat$poverty_rate, log(taxi_dat$taxi_trip_2017))
plot(log(taxi_dat$poverty_rate + 1), log(taxi_dat$taxi_trip_2017))
```

![](Assignment-2_Viva-Yaohan_files/figure-html/task4-6.png)<!-- -->

```r
### income
plot(taxi_dat$income, taxi_dat$taxi_trip_2017)
plot(log(taxi_dat$income + 1), taxi_dat$taxi_trip_2017)
plot(taxi_dat$income, log(taxi_dat$taxi_trip_2017))
plot(log(taxi_dat$income + 1), log(taxi_dat$taxi_trip_2017))
```

![](Assignment-2_Viva-Yaohan_files/figure-html/task4-7.png)<!-- -->

```r
### no_veh_hh_per
plot(taxi_dat$no_veh_hh_per, taxi_dat$taxi_trip_2017)
plot(log(taxi_dat$no_veh_hh_per + 1), taxi_dat$taxi_trip_2017)
plot(taxi_dat$no_veh_hh_per, log(taxi_dat$taxi_trip_2017))
plot(log(taxi_dat$no_veh_hh_per + 1), log(taxi_dat$taxi_trip_2017))
```

![](Assignment-2_Viva-Yaohan_files/figure-html/task4-8.png)<!-- -->

```r
### com_indu_per
plot(taxi_dat$com_indu_per, taxi_dat$taxi_trip_2017)
plot(log(taxi_dat$com_indu_per + 1), taxi_dat$taxi_trip_2017)
plot(taxi_dat$com_indu_per, log(taxi_dat$taxi_trip_2017))
plot(log(taxi_dat$com_indu_per + 1), log(taxi_dat$taxi_trip_2017))
```

![](Assignment-2_Viva-Yaohan_files/figure-html/task4-9.png)<!-- -->

```r
### walk_score
plot(taxi_dat$walk_score, taxi_dat$taxi_trip_2017)
plot(log(taxi_dat$walk_score + 1), taxi_dat$taxi_trip_2017)
plot(taxi_dat$walk_score, log(taxi_dat$taxi_trip_2017))
plot(log(taxi_dat$walk_score + 1), log(taxi_dat$taxi_trip_2017))
```

![](Assignment-2_Viva-Yaohan_files/figure-html/task4-10.png)<!-- -->

```r
### transit_score_d
plot(taxi_dat$transit_score_d, taxi_dat$taxi_trip_2017)
plot(log(taxi_dat$transit_score_d + 1), taxi_dat$taxi_trip_2017)
plot(taxi_dat$transit_score_d, log(taxi_dat$taxi_trip_2017))
plot(log(taxi_dat$transit_score_d + 1), log(taxi_dat$taxi_trip_2017))
```

![](Assignment-2_Viva-Yaohan_files/figure-html/task4-11.png)<!-- -->

```r
### drive_per
plot(taxi_dat$drive_per, taxi_dat$taxi_trip_2017)
plot(log(taxi_dat$drive_per + 1), taxi_dat$taxi_trip_2017)
plot(taxi_dat$drive_per, log(taxi_dat$taxi_trip_2017))
plot(log(taxi_dat$drive_per + 1), log(taxi_dat$taxi_trip_2017))
```

![](Assignment-2_Viva-Yaohan_files/figure-html/task4-12.png)<!-- -->

```r
### transit_per
plot(taxi_dat$transit_per, taxi_dat$taxi_trip_2017)
plot(log(taxi_dat$transit_per + 1), taxi_dat$taxi_trip_2017)
plot(taxi_dat$transit_per, log(taxi_dat$taxi_trip_2017))
plot(log(taxi_dat$transit_per + 1), log(taxi_dat$taxi_trip_2017))
```

![](Assignment-2_Viva-Yaohan_files/figure-html/task4-13.png)<!-- -->

```r
### ped_trips_pp
plot(taxi_dat$ped_trips_pp, taxi_dat$taxi_trip_2017)
plot(log(taxi_dat$ped_trips_pp + 1), taxi_dat$taxi_trip_2017)
plot(taxi_dat$ped_trips_pp, log(taxi_dat$taxi_trip_2017))
plot(log(taxi_dat$ped_trips_pp + 1), log(taxi_dat$taxi_trip_2017))
```

![](Assignment-2_Viva-Yaohan_files/figure-html/task4-14.png)<!-- -->


```r
## four chosen plots
par(mfrow = c(1, 1))

### job_density
ggplot(taxi_dat) +
  geom_point(aes(x = log(job_density + 1), y = log(taxi_trip_2017)), color = "black", pch = 16, size = 2) +
  labs(title = "2017 Taxi Trips vs. Job Density",
       x = "Employment per Acre (logged)",
       y = "Taxi Trips (logged)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
```

![](Assignment-2_Viva-Yaohan_files/figure-html/unnamed-chunk-1-1.png)<!-- -->

```r
### drive_per
ggplot(taxi_dat) +
  geom_point(aes(x = drive_per, y = log(taxi_trip_2017)), color = "black", pch = 16, size = 2) +
  labs(title = "2017 Taxi Trips vs. Drive Alone Workers",
       x = "Drive Alone Workers",
       y = "Taxi Trips (logged)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
```

![](Assignment-2_Viva-Yaohan_files/figure-html/unnamed-chunk-1-2.png)<!-- -->

```r
### transit_score_d
ggplot(taxi_dat) +
  geom_point(aes(x = transit_score_d, y = log(taxi_trip_2017)), color = "black", pch = 16, size = 2) +
  labs(title = "2017 Taxi Trips vs. Transit Score",
       x = "Transit Score",
       y = "Taxi Trips (logged)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
```

![](Assignment-2_Viva-Yaohan_files/figure-html/unnamed-chunk-1-3.png)<!-- -->

```r
### ped_trips_pp
ggplot(taxi_dat) +
  geom_point(aes(x = log(ped_trips_pp + 1), y = log(taxi_trip_2017)), color = "black", pch = 16, size = 2) +
  labs(title = "2017 Taxi Trips vs. Average Pedestrian Trips per Person",
       x = "Average Pedestrian Trips per Person (logged)",
       y = "Taxi Trips (logged)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
```

![](Assignment-2_Viva-Yaohan_files/figure-html/unnamed-chunk-1-4.png)<!-- -->

# Task 5


```r
## continuous variables
taxi_summary <- taxi_dat %>%
  st_drop_geometry() %>%
  summarise(across(c(taxi_trip_2015, taxi_trip_2017,
                     pop_density, median_age, edu_ba_per, non_white_per, 
                     job_density, poverty_rate, income,
                     no_veh_hh_per, com_indu_per, walk_score, transit_score_d,
                     drive_per, transit_per, ped_trips_pp),
                   list(maximum = ~ max(., na.rm = TRUE),
                        minimum = ~ min(., na.rm = TRUE),
                        mean = ~ mean(., na.rm = TRUE),
                        standard_deviation = ~ sd(., na.rm = TRUE)),
                   .names = "{.col}:{.fn}")) %>%
  pivot_longer(cols = everything(), names_to = c("variables", "statistic"),
               names_sep = ":", values_to = "value") %>%
  mutate(value = round(value, digits = 2)) %>%
  pivot_wider(names_from = statistic, values_from = value)

## categorical variables
### median_age_cat
median_age_cat_sum <- taxi_dat %>%
  st_drop_geometry() %>%
  group_by(median_age_cat) %>%
  summarise(count = n()) %>%
  mutate(percent = round(count/sum(count) * 100, digits = 2)) 
  
### edu_level_dum
edu_level_dum_sum <- taxi_dat %>%
  st_drop_geometry() %>%
  group_by(edu_level_dum) %>%
  summarise(count = n()) %>%
  mutate(percent = round(count/sum(count) * 100, digits = 2)) 

### income_cat
income_cat_sum <- taxi_dat %>%
  st_drop_geometry() %>%
  group_by(income_cat) %>%
  summarise(count = n()) %>%
  mutate(percent = round(count/sum(count) * 100, digits = 2)) 

### district_dum
district_dum_sum <- taxi_dat %>%
  st_drop_geometry() %>%
  group_by(district_dum) %>%
  summarise(count = n()) %>%
  mutate(percent = round(count/sum(count) * 100, digits = 2)) 

### inde_station_dum
inde_station_dum_sum <- taxi_dat %>%
  st_drop_geometry() %>%
  group_by(inde_station_dum) %>%
  summarise(count = n()) %>%
  mutate(percent = round(count/sum(count) * 100, digits = 2)) 

### walk_score_dum
walk_score_dum_sum <- taxi_dat %>%
  st_drop_geometry() %>%
  group_by(walk_score_dum) %>%
  summarise(count = n()) %>%
  mutate(percent = round(count/sum(count) * 100, digits = 2)) 

### transit_score_dum
transit_score_dum_sum <- taxi_dat %>%
  st_drop_geometry() %>%
  group_by(transit_score_dum) %>%
  summarise(count = n()) %>%
  mutate(percent = round(count/sum(count) * 100, digits = 2)) 

### subway_dum
subway_dum_sum <- taxi_dat %>%
  st_drop_geometry() %>%
  group_by(subway_dum) %>%
  summarise(count = n()) %>%
  mutate(percent = round(count/sum(count) * 100, digits = 2))
```

# Task 6


```r
## simple regression on continuous variables
con.var <- c("pop_density","median_age","edu_ba_per","non_white_per",
             "job_density","poverty_rate","income","no_veh_hh_per",
             "com_indu_per","walk_score","transit_score_d",
             "drive_per","transit_per","ped_trips_pp")

con_models <- list()

for (var in con.var) {
    formula1 <- as.formula(paste("taxi_trip_2017 ~", var))
    formula2 <- as.formula(paste("taxi_trip_2017 ~ log(", var, " + 1)"))
    formula3 <- as.formula(paste("log(taxi_trip_2017) ~", var))
    formula4 <- as.formula(paste("log(taxi_trip_2017) ~ log(", var, " + 1)"))
    
    model1 <- lm(formula1, data = taxi_dat)
    model2 <- lm(formula2, data = taxi_dat)
    model3 <- lm(formula3, data = taxi_dat)
    model4 <- lm(formula4, data = taxi_dat)
    
    # store the models in the list
    con_models[[paste(var, "1", sep = ".")]] <- model1
    con_models[[paste(var, "2", sep = ".")]] <- model2
    con_models[[paste(var, "3", sep = ".")]] <- model3
    con_models[[paste(var, "4", sep = ".")]] <- model4
    
    # use stargazer to display the model summaries side-by-side
    cat(paste("Model summaries for variable:", var, "\n"))
    stargazer(model1, model2, model3, model4, type = "text")
    cat("\n\n")  # add some space between different sets of models for clarity
}
```

```
## Model summaries for variable: pop_density 
## 
## ============================================================================
##                                             Dependent variable:             
##                                ---------------------------------------------
##                                     taxi_trip_2017      log(taxi_trip_2017) 
##                                    (1)         (2)         (3)        (4)   
## ----------------------------------------------------------------------------
## pop_density                    352.815***                0.060***           
##                                 (127.952)                (0.006)            
##                                                                             
## log(pop_density + 1)                        1,387.809              1.237*** 
##                                            (3,198.592)              (0.143) 
##                                                                             
## Constant                        1,364.045   7,631.291    5.146***  2.944*** 
##                                (4,527.408) (10,655.220)  (0.195)    (0.476) 
##                                                                             
## ----------------------------------------------------------------------------
## Observations                       377         377         377        377   
## R2                                0.020       0.001       0.237      0.167  
## Adjusted R2                       0.017       -0.002      0.235      0.164  
## Residual Std. Error (df = 375) 44,323.060   44,758.910    1.913      2.000  
## F Statistic (df = 1; 375)       7.603***      0.188     116.562*** 74.969***
## ============================================================================
## Note:                                            *p<0.1; **p<0.05; ***p<0.01
## 
## 
## Model summaries for variable: median_age 
## 
## ==============================================================================
##                                              Dependent variable:              
##                                -----------------------------------------------
##                                      taxi_trip_2017        log(taxi_trip_2017)
##                                     (1)           (2)         (3)       (4)   
## ------------------------------------------------------------------------------
## median_age                       -591.191*                 -0.106***          
##                                  (316.229)                  (0.015)           
##                                                                               
## log(median_age + 1)                          -17,301.490**           -2.460***
##                                               (8,694.013)             (0.408) 
##                                                                               
## Constant                       33,016.130*** 73,859.450**  10.720*** 15.743***
##                                (11,397.520)  (31,096.320)   (0.524)   (1.460) 
##                                                                               
## ------------------------------------------------------------------------------
## Observations                        377           377         377       377   
## R2                                 0.009         0.010       0.125     0.088  
## Adjusted R2                        0.007         0.008       0.122     0.086  
## Residual Std. Error (df = 375)  44,562.960    44,535.590     2.049     2.092  
## F Statistic (df = 1; 375)         3.495*        3.960**    53.418*** 36.302***
## ==============================================================================
## Note:                                              *p<0.1; **p<0.05; ***p<0.01
## 
## 
## Model summaries for variable: edu_ba_per 
## 
## =============================================================================
##                                             Dependent variable:              
##                                ----------------------------------------------
##                                      taxi_trip_2017        log(taxi_trip_2017)
##                                    (1)           (2)          (3)      (4)   
## -----------------------------------------------------------------------------
## edu_ba_per                      807.384***                 0.032***          
##                                 (114.056)                   (0.006)          
##                                                                              
## log(edu_ba_per + 1)                         16,036.660***            0.434***
##                                              (2,857.235)             (0.144) 
##                                                                              
## Constant                       -8,008.947** -35,665.800*** 6.178***  5.673***
##                                (3,577.107)   (8,801.561)    (0.179)  (0.443) 
##                                                                              
## -----------------------------------------------------------------------------
## Observations                       377           377          377      377   
## R2                                0.118         0.077        0.076    0.024  
## Adjusted R2                       0.116         0.075        0.073    0.021  
## Residual Std. Error (df = 375)  42,048.800    43,000.430     2.106    2.164  
## F Statistic (df = 1; 375)       50.110***     31.502***    30.686*** 9.124***
## =============================================================================
## Note:                                             *p<0.1; **p<0.05; ***p<0.01
## 
## 
## Model summaries for variable: non_white_per 
## 
## ============================================================================
##                                             Dependent variable:             
##                                ---------------------------------------------
##                                      taxi_trip_2017        log(taxi_trip_2017)
##                                     (1)           (2)        (3)      (4)   
## ----------------------------------------------------------------------------
## non_white_per                   -268.240***                 0.0005          
##                                  (71.466)                  (0.004)          
##                                                                             
## log(non_white_per + 1)                       -8,203.104***           0.042  
##                                               (2,867.022)           (0.142) 
##                                                                             
## Constant                       27,692.940*** 43,681.220*** 6.941*** 6.807***
##                                 (4,720.532)  (11,255.640)  (0.235)  (0.557) 
##                                                                             
## ----------------------------------------------------------------------------
## Observations                        377           377        377      377   
## R2                                 0.036         0.021     0.00005   0.0002 
## Adjusted R2                        0.034         0.019      -0.003   -0.002 
## Residual Std. Error (df = 375)  43,952.150    44,289.320    2.190    2.190  
## F Statistic (df = 1; 375)        14.088***     8.186***     0.017    0.087  
## ============================================================================
## Note:                                            *p<0.1; **p<0.05; ***p<0.01
## 
## 
## Model summaries for variable: job_density 
## 
## ==============================================================================
##                                              Dependent variable:              
##                                -----------------------------------------------
##                                      taxi_trip_2017       log(taxi_trip_2017) 
##                                    (1)          (2)          (3)       (4)    
## ------------------------------------------------------------------------------
## job_density                    554.984***                 0.015***            
##                                 (28.752)                   (0.002)            
##                                                                               
## log(job_density + 1)                       26,207.230***             1.321*** 
##                                             (1,873.239)              (0.090)  
##                                                                               
## Constant                       4,282.141** -32,741.490*** 6.751***   4.706*** 
##                                (1,683.110)  (3,713.093)    (0.107)   (0.179)  
##                                                                               
## ------------------------------------------------------------------------------
## Observations                       377          377          377       377    
## R2                                0.498        0.343        0.158     0.364   
## Adjusted R2                       0.497        0.341        0.156     0.362   
## Residual Std. Error (df = 375) 31,708.530    36,290.170     2.010     1.747   
## F Statistic (df = 1; 375)      372.577***    195.729***   70.351*** 214.478***
## ==============================================================================
## Note:                                              *p<0.1; **p<0.05; ***p<0.01
## 
## 
## Model summaries for variable: poverty_rate 
## 
## =============================================================================
##                                             Dependent variable:              
##                                ----------------------------------------------
##                                       taxi_trip_2017        log(taxi_trip_2017)
##                                     (1)           (2)         (3)      (4)   
## -----------------------------------------------------------------------------
## poverty_rate                    -558.150***                 0.019**          
##                                  (155.919)                  (0.008)          
##                                                                              
## log(poverty_rate + 1)                        -14,335.830***          0.379** 
##                                               (3,252.150)            (0.162) 
##                                                                              
## Constant                       26,059.350*** 55,980.410***  6.500*** 5.809***
##                                 (4,499.957)   (10,195.270)  (0.222)  (0.508) 
##                                                                              
## -----------------------------------------------------------------------------
## Observations                        377           377         377      377   
## R2                                 0.033         0.049       0.016    0.014  
## Adjusted R2                        0.030         0.047       0.013    0.012  
## Residual Std. Error (df = 375)  44,024.250     43,653.430    2.173    2.175  
## F Statistic (df = 1; 375)        12.815***     19.431***    5.958**  5.477** 
## =============================================================================
## Note:                                             *p<0.1; **p<0.05; ***p<0.01
## 
## 
## Model summaries for variable: income 
## 
## ==========================================================================
##                                            Dependent variable:            
##                                -------------------------------------------
##                                     taxi_trip_2017      log(taxi_trip_2017)
##                                    (1)         (2)         (3)      (4)   
## --------------------------------------------------------------------------
## income                          0.432***                -0.00000          
##                                  (0.100)                (0.00001)         
##                                                                           
## log(income + 1)                            5,290.555**             -0.103 
##                                            (2,503.090)            (0.123) 
##                                                                           
## Constant                       -6,445.988  -43,379.000  7.037***  8.048***
##                                (4,870.908) (26,369.510)  (0.244)  (1.297) 
##                                                                           
## --------------------------------------------------------------------------
## Observations                       377         377         377      377   
## R2                                0.047       0.012      0.0003    0.002  
## Adjusted R2                       0.045       0.009      -0.002    -0.001 
## Residual Std. Error (df = 375) 43,703.800   44,505.830    2.190    2.188  
## F Statistic (df = 1; 375)       18.523***    4.467**      0.103    0.699  
## ==========================================================================
## Note:                                          *p<0.1; **p<0.05; ***p<0.01
## 
## 
## Model summaries for variable: no_veh_hh_per 
## 
## =============================================================================
##                                             Dependent variable:              
##                                ----------------------------------------------
##                                     taxi_trip_2017       log(taxi_trip_2017) 
##                                    (1)         (2)         (3)        (4)    
## -----------------------------------------------------------------------------
## no_veh_hh_per                  426.605***                0.069***            
##                                 (130.288)                (0.005)             
##                                                                              
## log(no_veh_hh_per + 1)                      5,265.570               1.648*** 
##                                            (3,241.975)              (0.134)  
##                                                                              
## Constant                       -1,557.154   -5,265.951   4.743***   1.517*** 
##                                (4,762.400) (10,963.230)  (0.197)    (0.455)  
##                                                                              
## -----------------------------------------------------------------------------
## Observations                       377         377         377        377    
## R2                                0.028       0.007       0.306      0.286   
## Adjusted R2                       0.025       0.004       0.304      0.284   
## Residual Std. Error (df = 375) 44,143.560   44,613.500    1.824      1.851   
## F Statistic (df = 1; 375)       10.721***     2.638     165.570*** 150.217***
## =============================================================================
## Note:                                             *p<0.1; **p<0.05; ***p<0.01
## 
## 
## Model summaries for variable: com_indu_per 
## 
## ============================================================================
##                                             Dependent variable:             
##                                ---------------------------------------------
##                                     taxi_trip_2017       log(taxi_trip_2017)
##                                    (1)          (2)         (3)       (4)   
## ----------------------------------------------------------------------------
## com_indu_per                   1,057.506***              0.045***           
##                                 (156.777)                 (0.008)           
##                                                                             
## log(com_indu_per + 1)                       8,561.689***           0.650*** 
##                                             (2,432.544)             (0.116) 
##                                                                             
## Constant                        -3,662.198   -8,343.609  6.291***  5.412*** 
##                                (3,198.905)  (6,247.625)   (0.159)   (0.298) 
##                                                                             
## ----------------------------------------------------------------------------
## Observations                       377          377         377       377   
## R2                                0.108        0.032       0.083     0.077  
## Adjusted R2                       0.106        0.029       0.080     0.075  
## Residual Std. Error (df = 375)  42,278.700   44,048.490    2.098     2.104  
## F Statistic (df = 1; 375)       45.499***    12.388***   33.873*** 31.293***
## ============================================================================
## Note:                                            *p<0.1; **p<0.05; ***p<0.01
## 
## 
## Model summaries for variable: walk_score 
## 
## ============================================================================
##                                             Dependent variable:             
##                                ---------------------------------------------
##                                     taxi_trip_2017      log(taxi_trip_2017) 
##                                    (1)         (2)         (3)        (4)   
## ----------------------------------------------------------------------------
## walk_score                      246.920**                0.057***           
##                                 (106.947)                (0.004)            
##                                                                             
## log(walk_score + 1)                          947.795               1.832*** 
##                                            (4,831.271)              (0.217) 
##                                                                             
## Constant                       -6,728.402   8,093.287    2.583***   -0.863  
##                                (8,489.075) (20,780.930)  (0.346)    (0.932) 
##                                                                             
## ----------------------------------------------------------------------------
## Observations                       377         377         377        377   
## R2                                0.014       0.0001      0.316      0.160  
## Adjusted R2                       0.011       -0.003      0.314      0.158  
## Residual Std. Error (df = 375) 44,455.290   44,767.840    1.812      2.007  
## F Statistic (df = 1; 375)        5.331**      0.038     173.240*** 71.513***
## ============================================================================
## Note:                                            *p<0.1; **p<0.05; ***p<0.01
## 
## 
## Model summaries for variable: transit_score_d 
## 
## ==================================================================================
##                                                Dependent variable:                
##                                ---------------------------------------------------
##                                       taxi_trip_2017          log(taxi_trip_2017) 
##                                     (1)            (2)          (3)        (4)    
## ----------------------------------------------------------------------------------
## transit_score_d                 1,704.059***                  0.106***            
##                                  (160.629)                    (0.007)             
##                                                                                   
## log(transit_score_d + 1)                      13,661.290***              1.730*** 
##                                                (3,188.152)               (0.132)  
##                                                                                   
## Constant                       -16,927.970*** -24,413.570***  5.162***   2.338*** 
##                                 (3,405.820)    (8,823.736)    (0.151)    (0.367)  
##                                                                                   
## ----------------------------------------------------------------------------------
## Observations                        377            377          377        377    
## R2                                 0.231          0.047        0.372      0.313   
## Adjusted R2                        0.229          0.044        0.370      0.311   
## Residual Std. Error (df = 375)   39,264.260     43,712.760     1.736      1.816   
## F Statistic (df = 1; 375)        112.544***     18.361***    222.197*** 170.625***
## ==================================================================================
## Note:                                                  *p<0.1; **p<0.05; ***p<0.01
## 
## 
## Model summaries for variable: drive_per 
## 
## =================================================================================
##                                               Dependent variable:                
##                                --------------------------------------------------
##                                       taxi_trip_2017         log(taxi_trip_2017) 
##                                     (1)           (2)          (3)        (4)    
## ---------------------------------------------------------------------------------
## drive_per                       -927.933***                 -0.097***            
##                                  (128.672)                   (0.004)             
##                                                                                  
## log(drive_per + 1)                           -31,749.600***            -2.833*** 
##                                               (4,638.540)               (0.191)  
##                                                                                  
## Constant                       57,602.620*** 133,855.200*** 11.743***  17.828*** 
##                                 (6,663.460)   (17,913.990)   (0.230)    (0.738)  
##                                                                                  
## ---------------------------------------------------------------------------------
## Observations                        377           377          377        377    
## R2                                 0.122         0.111        0.561      0.369   
## Adjusted R2                        0.119         0.109        0.560      0.368   
## Residual Std. Error (df = 375)  41,955.240     42,210.920     1.451      1.739   
## F Statistic (df = 1; 375)        52.008***     46.850***    479.977*** 219.640***
## =================================================================================
## Note:                                                 *p<0.1; **p<0.05; ***p<0.01
## 
## 
## Model summaries for variable: transit_per 
## 
## =============================================================================
##                                             Dependent variable:              
##                                ----------------------------------------------
##                                      taxi_trip_2017       log(taxi_trip_2017)
##                                     (1)          (2)         (3)       (4)   
## -----------------------------------------------------------------------------
## transit_per                      -329.434*                0.055***           
##                                  (176.235)                 (0.008)           
##                                                                              
## log(transit_per + 1)                          -5,363.961            1.308*** 
##                                              (4,148.149)             (0.192) 
##                                                                              
## Constant                       21,091.860*** 29,362.330** 5.463***  2.770*** 
##                                 (5,308.108)  (13,512.170)  (0.246)   (0.625) 
##                                                                              
## -----------------------------------------------------------------------------
## Observations                        377          377         377       377   
## R2                                 0.009        0.004       0.109     0.110  
## Adjusted R2                        0.007        0.002       0.107     0.108  
## Residual Std. Error (df = 375)  44,563.000    44,670.660    2.067     2.066  
## F Statistic (df = 1; 375)         3.494*        1.672     45.949*** 46.468***
## =============================================================================
## Note:                                             *p<0.1; **p<0.05; ***p<0.01
## 
## 
## Model summaries for variable: ped_trips_pp 
## 
## ===============================================================================
##                                              Dependent variable:               
##                                ------------------------------------------------
##                                       taxi_trip_2017        log(taxi_trip_2017)
##                                     (1)           (2)         (3)       (4)    
## -------------------------------------------------------------------------------
## ped_trips_pp                      64.682*                   0.004**            
##                                  (38.831)                   (0.002)            
##                                                                                
## log(ped_trips_pp + 1)                        47,192.860***            2.558*** 
##                                               (3,056.988)             (0.138)  
##                                                                                
## Constant                       11,781.190*** -39,234.550*** 6.943***  4.183*** 
##                                 (2,307.649)   (3,785.168)   (0.113)   (0.171)  
##                                                                                
## -------------------------------------------------------------------------------
## Observations                        377           377         377       377    
## R2                                 0.007         0.389       0.014     0.477   
## Adjusted R2                        0.005         0.387       0.012     0.476   
## Residual Std. Error (df = 375)  44,605.420     35,007.380    2.175     1.584   
## F Statistic (df = 1; 375)         2.775*       238.323***   5.492**  342.003***
## ===============================================================================
## Note:                                               *p<0.1; **p<0.05; ***p<0.01
```

```r
## simple regression on categorical variables
cat.var <- c("median_age_cat","edu_level_dum",
             "income_cat","district_dum",
             "inde_station_dum","walk_score_dum","transit_score_dum",
             "subway_dum")

cat_models <- list()

for (var2 in cat.var) {
    formula5 <- as.formula(paste("taxi_trip_2017 ~", var2))
    formula6 <- as.formula(paste("log(taxi_trip_2017) ~", var2))
    
    model5 <- lm(formula5, data = taxi_dat)
    model6 <- lm(formula6, data = taxi_dat)
    
    # store the models in the list
    cat_models[[paste(var2, "5", sep = ".")]] <- model5
    cat_models[[paste(var2, "6", sep = ".")]] <- model6
    
    # use stargazer to display the model summaries side-by-side
    cat(paste("Model summaries for variable:", var2, "\n"))
    stargazer(model5, model6, type = "text")
    cat("\n\n")  # add some space between different sets of models for clarity
}
```

```
## Model summaries for variable: median_age_cat 
## 
## =================================================================
##                                       Dependent variable:        
##                                ----------------------------------
##                                taxi_trip_2017 log(taxi_trip_2017)
##                                     (1)               (2)        
## -----------------------------------------------------------------
## median_age_catage_2nd            5,907.364           0.457       
##                                 (6,442.850)         (0.291)      
##                                                                  
## median_age_catage_3rd            -9,677.701         -0.373       
##                                 (6,460.328)         (0.292)      
##                                                                  
## median_age_catage_4th            -9,369.966        -1.925***     
##                                 (6,442.850)         (0.291)      
##                                                                  
## Constant                       15,395.590***       7.426***      
##                                 (4,531.741)         (0.204)      
##                                                                  
## -----------------------------------------------------------------
## Observations                        377               377        
## R2                                 0.022             0.168       
## Adjusted R2                        0.014             0.161       
## Residual Std. Error (df = 373)   44,401.820          2.004       
## F Statistic (df = 3; 373)         2.749**          25.054***     
## =================================================================
## Note:                                 *p<0.1; **p<0.05; ***p<0.01
## 
## 
## Model summaries for variable: edu_level_dum 
## 
## =================================================================
##                                       Dependent variable:        
##                                ----------------------------------
##                                taxi_trip_2017 log(taxi_trip_2017)
##                                     (1)               (2)        
## -----------------------------------------------------------------
## edu_level_dumbelow_ave         -27,186.800***      -0.918***     
##                                 (4,599.854)         (0.230)      
##                                                                  
## Constant                       29,596.380***       7.557***      
##                                 (3,685.370)         (0.185)      
##                                                                  
## -----------------------------------------------------------------
## Observations                        377               377        
## R2                                 0.085             0.041       
## Adjusted R2                        0.083             0.038       
## Residual Std. Error (df = 375)   42,820.130          2.145       
## F Statistic (df = 1; 375)        34.932***         15.875***     
## =================================================================
## Note:                                 *p<0.1; **p<0.05; ***p<0.01
## 
## 
## Model summaries for variable: income_cat 
## 
## =================================================================
##                                       Dependent variable:        
##                                ----------------------------------
##                                taxi_trip_2017 log(taxi_trip_2017)
##                                     (1)               (2)        
## -----------------------------------------------------------------
## income_catincome_2nd              -337.424          -0.423       
##                                 (6,363.306)         (0.317)      
##                                                                  
## income_catincome_3rd            11,777.710*        -0.781**      
##                                 (6,363.306)         (0.317)      
##                                                                  
## income_catincome_4th           24,093.060***        -0.406       
##                                 (6,363.306)         (0.317)      
##                                                                  
## Constant                         3,285.126         7.369***      
##                                 (4,487.618)         (0.224)      
##                                                                  
## -----------------------------------------------------------------
## Observations                        377               377        
## R2                                 0.051             0.016       
## Adjusted R2                        0.043             0.008       
## Residual Std. Error (df = 373)   43,739.890          2.179       
## F Statistic (df = 3; 373)         6.625***           2.031       
## =================================================================
## Note:                                 *p<0.1; **p<0.05; ***p<0.01
## 
## 
## Model summaries for variable: district_dum 
## 
## =================================================================
##                                       Dependent variable:        
##                                ----------------------------------
##                                taxi_trip_2017 log(taxi_trip_2017)
##                                     (1)               (2)        
## -----------------------------------------------------------------
## district_dumnon_central        -69,504.330***      -3.827***     
##                                 (6,666.468)         (0.313)      
##                                                                  
## Constant                       74,459.130***       10.399***     
##                                 (6,312.239)         (0.297)      
##                                                                  
## -----------------------------------------------------------------
## Observations                        377               377        
## R2                                 0.225             0.285       
## Adjusted R2                        0.223             0.283       
## Residual Std. Error (df = 375)   39,419.920          1.853       
## F Statistic (df = 1; 375)        108.701***       149.228***     
## =================================================================
## Note:                                 *p<0.1; **p<0.05; ***p<0.01
## 
## 
## Model summaries for variable: inde_station_dum 
## 
## =================================================================
##                                       Dependent variable:        
##                                ----------------------------------
##                                taxi_trip_2017 log(taxi_trip_2017)
##                                     (1)               (2)        
## -----------------------------------------------------------------
## inde_station_dumnon_indego     -45,314.890***      -3.201***     
##                                 (5,379.058)         (0.235)      
##                                                                  
## Constant                       48,805.490***       9.557***      
##                                 (4,838.218)         (0.211)      
##                                                                  
## -----------------------------------------------------------------
## Observations                        377               377        
## R2                                 0.159             0.332       
## Adjusted R2                        0.157             0.330       
## Residual Std. Error (df = 375)   41,053.640          1.791       
## F Statistic (df = 1; 375)        70.969***        186.056***     
## =================================================================
## Note:                                 *p<0.1; **p<0.05; ***p<0.01
## 
## 
## Model summaries for variable: walk_score_dum 
## 
## =================================================================
##                                       Dependent variable:        
##                                ----------------------------------
##                                taxi_trip_2017 log(taxi_trip_2017)
##                                     (1)               (2)        
## -----------------------------------------------------------------
## walk_score_dumbelow_ave        -10,645.330**       -2.445***     
##                                 (4,977.682)         (0.210)      
##                                                                  
## Constant                       15,392.150***       7.714***      
##                                 (2,749.194)         (0.116)      
##                                                                  
## -----------------------------------------------------------------
## Observations                        377               377        
## R2                                 0.012             0.266       
## Adjusted R2                        0.009             0.264       
## Residual Std. Error (df = 375)   44,499.590          1.877       
## F Statistic (df = 1; 375)         4.574**         135.608***     
## =================================================================
## Note:                                 *p<0.1; **p<0.05; ***p<0.01
## 
## 
## Model summaries for variable: transit_score_dum 
## 
## =================================================================
##                                       Dependent variable:        
##                                ----------------------------------
##                                taxi_trip_2017 log(taxi_trip_2017)
##                                     (1)               (2)        
## -----------------------------------------------------------------
## transit_score_dumbelow_ave     -16,581.450***      -2.164***     
##                                 (4,589.905)         (0.199)      
##                                                                  
## Constant                       21,733.110***       8.219***      
##                                 (3,490.288)         (0.152)      
##                                                                  
## -----------------------------------------------------------------
## Observations                        377               377        
## R2                                 0.034             0.239       
## Adjusted R2                        0.031             0.237       
## Residual Std. Error (df = 375)   44,010.850          1.910       
## F Statistic (df = 1; 375)        13.051***        117.957***     
## =================================================================
## Note:                                 *p<0.1; **p<0.05; ***p<0.01
## 
## 
## Model summaries for variable: subway_dum 
## 
## =================================================================
##                                       Dependent variable:        
##                                ----------------------------------
##                                taxi_trip_2017 log(taxi_trip_2017)
##                                     (1)               (2)        
## -----------------------------------------------------------------
## subway_dumsubway               32,257.830***       2.542***      
##                                 (4,790.996)         (0.211)      
##                                                                  
## Constant                         2,732.801         6.226***      
##                                 (2,587.924)         (0.114)      
##                                                                  
## -----------------------------------------------------------------
## Observations                        377               377        
## R2                                 0.108             0.280       
## Adjusted R2                        0.105             0.278       
## Residual Std. Error (df = 375)   42,287.020          1.859       
## F Statistic (df = 1; 375)        45.333***        145.700***     
## =================================================================
## Note:                                 *p<0.1; **p<0.05; ***p<0.01
```


```r
## four chosen models
### pop_density
pop_density_1 <- con_models[["pop_density.1"]]
pop_density_2 <- con_models[["pop_density.2"]]
pop_density_3 <- con_models[["pop_density.3"]]
pop_density_4 <- con_models[["pop_density.4"]]
#### chose model
stargazer(pop_density_3, type = "text")
```

```
## 
## ===============================================
##                         Dependent variable:    
##                     ---------------------------
##                         log(taxi_trip_2017)    
## -----------------------------------------------
## pop_density                  0.060***          
##                               (0.006)          
##                                                
## Constant                     5.146***          
##                               (0.195)          
##                                                
## -----------------------------------------------
## Observations                    377            
## R2                             0.237           
## Adjusted R2                    0.235           
## Residual Std. Error      1.913 (df = 375)      
## F Statistic          116.562*** (df = 1; 375)  
## ===============================================
## Note:               *p<0.1; **p<0.05; ***p<0.01
```

```r
#### comparison models
stargazer(pop_density_1, pop_density_2, pop_density_3, pop_density_4, type = "text")
```

```
## 
## ============================================================================
##                                             Dependent variable:             
##                                ---------------------------------------------
##                                     taxi_trip_2017      log(taxi_trip_2017) 
##                                    (1)         (2)         (3)        (4)   
## ----------------------------------------------------------------------------
## pop_density                    352.815***                0.060***           
##                                 (127.952)                (0.006)            
##                                                                             
## log(pop_density + 1)                        1,387.809              1.237*** 
##                                            (3,198.592)              (0.143) 
##                                                                             
## Constant                        1,364.045   7,631.291    5.146***  2.944*** 
##                                (4,527.408) (10,655.220)  (0.195)    (0.476) 
##                                                                             
## ----------------------------------------------------------------------------
## Observations                       377         377         377        377   
## R2                                0.020       0.001       0.237      0.167  
## Adjusted R2                       0.017       -0.002      0.235      0.164  
## Residual Std. Error (df = 375) 44,323.060   44,758.910    1.913      2.000  
## F Statistic (df = 1; 375)       7.603***      0.188     116.562*** 74.969***
## ============================================================================
## Note:                                            *p<0.1; **p<0.05; ***p<0.01
```

```r
### drive_per
drive_per_1 <- con_models[["drive_per.1"]]
drive_per_2 <- con_models[["drive_per.2"]]
drive_per_3 <- con_models[["drive_per.3"]]
drive_per_4 <- con_models[["drive_per.4"]]
#### chose model
stargazer(drive_per_3, type = "text")
```

```
## 
## ===============================================
##                         Dependent variable:    
##                     ---------------------------
##                         log(taxi_trip_2017)    
## -----------------------------------------------
## drive_per                    -0.097***         
##                               (0.004)          
##                                                
## Constant                     11.743***         
##                               (0.230)          
##                                                
## -----------------------------------------------
## Observations                    377            
## R2                             0.561           
## Adjusted R2                    0.560           
## Residual Std. Error      1.451 (df = 375)      
## F Statistic          479.977*** (df = 1; 375)  
## ===============================================
## Note:               *p<0.1; **p<0.05; ***p<0.01
```

```r
#### comparison models
stargazer(drive_per_1, drive_per_2, drive_per_3, drive_per_4, type = "text")
```

```
## 
## =================================================================================
##                                               Dependent variable:                
##                                --------------------------------------------------
##                                       taxi_trip_2017         log(taxi_trip_2017) 
##                                     (1)           (2)          (3)        (4)    
## ---------------------------------------------------------------------------------
## drive_per                       -927.933***                 -0.097***            
##                                  (128.672)                   (0.004)             
##                                                                                  
## log(drive_per + 1)                           -31,749.600***            -2.833*** 
##                                               (4,638.540)               (0.191)  
##                                                                                  
## Constant                       57,602.620*** 133,855.200*** 11.743***  17.828*** 
##                                 (6,663.460)   (17,913.990)   (0.230)    (0.738)  
##                                                                                  
## ---------------------------------------------------------------------------------
## Observations                        377           377          377        377    
## R2                                 0.122         0.111        0.561      0.369   
## Adjusted R2                        0.119         0.109        0.560      0.368   
## Residual Std. Error (df = 375)  41,955.240     42,210.920     1.451      1.739   
## F Statistic (df = 1; 375)        52.008***     46.850***    479.977*** 219.640***
## =================================================================================
## Note:                                                 *p<0.1; **p<0.05; ***p<0.01
```

```r
### income_cat
income_cat_1 <- cat_models[["income_cat.5"]]
income_cat_2 <- cat_models[["income_cat.6"]]
#### chose model
stargazer(income_cat_1, type = "text")
```

```
## 
## ================================================
##                          Dependent variable:    
##                      ---------------------------
##                            taxi_trip_2017       
## ------------------------------------------------
## income_catincome_2nd          -337.424          
##                              (6,363.306)        
##                                                 
## income_catincome_3rd         11,777.710*        
##                              (6,363.306)        
##                                                 
## income_catincome_4th        24,093.060***       
##                              (6,363.306)        
##                                                 
## Constant                      3,285.126         
##                              (4,487.618)        
##                                                 
## ------------------------------------------------
## Observations                     377            
## R2                              0.051           
## Adjusted R2                     0.043           
## Residual Std. Error     43,739.890 (df = 373)   
## F Statistic            6.625*** (df = 3; 373)   
## ================================================
## Note:                *p<0.1; **p<0.05; ***p<0.01
```

```r
#### comparison models
stargazer(income_cat_1, income_cat_2, type = "text")
```

```
## 
## =================================================================
##                                       Dependent variable:        
##                                ----------------------------------
##                                taxi_trip_2017 log(taxi_trip_2017)
##                                     (1)               (2)        
## -----------------------------------------------------------------
## income_catincome_2nd              -337.424          -0.423       
##                                 (6,363.306)         (0.317)      
##                                                                  
## income_catincome_3rd            11,777.710*        -0.781**      
##                                 (6,363.306)         (0.317)      
##                                                                  
## income_catincome_4th           24,093.060***        -0.406       
##                                 (6,363.306)         (0.317)      
##                                                                  
## Constant                         3,285.126         7.369***      
##                                 (4,487.618)         (0.224)      
##                                                                  
## -----------------------------------------------------------------
## Observations                        377               377        
## R2                                 0.051             0.016       
## Adjusted R2                        0.043             0.008       
## Residual Std. Error (df = 373)   43,739.890          2.179       
## F Statistic (df = 3; 373)         6.625***           2.031       
## =================================================================
## Note:                                 *p<0.1; **p<0.05; ***p<0.01
```

```r
### inde_station_dum
inde_station_dum_1 <- cat_models[["inde_station_dum.5"]]
inde_station_dum_2 <- cat_models[["inde_station_dum.6"]]
#### chose model
stargazer(inde_station_dum_2, type = "text")
```

```
## 
## ======================================================
##                                Dependent variable:    
##                            ---------------------------
##                                log(taxi_trip_2017)    
## ------------------------------------------------------
## inde_station_dumnon_indego          -3.201***         
##                                      (0.235)          
##                                                       
## Constant                            9.557***          
##                                      (0.211)          
##                                                       
## ------------------------------------------------------
## Observations                           377            
## R2                                    0.332           
## Adjusted R2                           0.330           
## Residual Std. Error             1.791 (df = 375)      
## F Statistic                 186.056*** (df = 1; 375)  
## ======================================================
## Note:                      *p<0.1; **p<0.05; ***p<0.01
```

```r
#### comparison models
stargazer(inde_station_dum_1, inde_station_dum_2, type = "text")
```

```
## 
## =================================================================
##                                       Dependent variable:        
##                                ----------------------------------
##                                taxi_trip_2017 log(taxi_trip_2017)
##                                     (1)               (2)        
## -----------------------------------------------------------------
## inde_station_dumnon_indego     -45,314.890***      -3.201***     
##                                 (5,379.058)         (0.235)      
##                                                                  
## Constant                       48,805.490***       9.557***      
##                                 (4,838.218)         (0.211)      
##                                                                  
## -----------------------------------------------------------------
## Observations                        377               377        
## R2                                 0.159             0.332       
## Adjusted R2                        0.157             0.330       
## Residual Std. Error (df = 375)   41,053.640          1.791       
## F Statistic (df = 1; 375)        70.969***        186.056***     
## =================================================================
## Note:                                 *p<0.1; **p<0.05; ***p<0.01
```

# Task 7


```r
# multiple regression for 2017 taxi trips
## step one: simple regression and plot
lm0<- lm(taxi_trip_2017 ~ pop_density + edu_ba_per + non_white_per +job_density
         + log(poverty_rate+1) + income + no_veh_hh_per + com_indu_per + district_dum
         + inde_station_dum +walk_score+ transit_score_d + drive_per + log(ped_trips_pp+1)
         + subway_dum, data = taxi_dat)
summary(lm0)
```

```
## 
## Call:
## lm(formula = taxi_trip_2017 ~ pop_density + edu_ba_per + non_white_per + 
##     job_density + log(poverty_rate + 1) + income + no_veh_hh_per + 
##     com_indu_per + district_dum + inde_station_dum + walk_score + 
##     transit_score_d + drive_per + log(ped_trips_pp + 1) + subway_dum, 
##     data = taxi_dat)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -105673   -7597    -839    5526  330259 
## 
## Coefficients:
##                               Estimate  Std. Error t value    Pr(>|t|)    
## (Intercept)                 73484.5771  24404.1118   3.011    0.002786 ** 
## pop_density                 -1775.3714    446.1785  -3.979 0.000083640 ***
## edu_ba_per                   -251.7108    138.1340  -1.822    0.069249 .  
## non_white_per                 103.8758     71.9117   1.444    0.149468    
## job_density                    25.5898     87.3648   0.293    0.769762    
## log(poverty_rate + 1)       -7780.2694   3966.3124  -1.962    0.050579 .  
## income                          0.0124      0.1508   0.082    0.934507    
## no_veh_hh_per                -773.2905    198.6009  -3.894    0.000118 ***
## com_indu_per                  -35.4899    135.2965  -0.262    0.793231    
## district_dumnon_central      -480.0859   7669.8504  -0.063    0.950125    
## inde_station_dumnon_indego -16928.1446   4772.4857  -3.547    0.000441 ***
## walk_score                   -178.2323    109.3128  -1.630    0.103872    
## transit_score_d              3539.1734    792.2623   4.467 0.000010618 ***
## drive_per                    -382.7562    168.1365  -2.276    0.023403 *  
## log(ped_trips_pp + 1)       23052.0710   4425.2664   5.209 0.000000319 ***
## subway_dumsubway             8411.1287   3790.6821   2.219    0.027115 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 27200 on 361 degrees of freedom
## Multiple R-squared:  0.6448,	Adjusted R-squared:   0.63 
## F-statistic: 43.68 on 15 and 361 DF,  p-value: < 0.00000000000000022
```

```r
vif(lm0)
```

```
##           pop_density            edu_ba_per         non_white_per 
##             32.296965              3.506303              2.644502 
##           job_density log(poverty_rate + 1)                income 
##             12.550468              3.832211              5.828535 
##         no_veh_hh_per          com_indu_per          district_dum 
##              6.121676              1.799838              2.780953 
##      inde_station_dum            walk_score       transit_score_d 
##              1.793743              2.791464             50.706510 
##             drive_per log(ped_trips_pp + 1)            subway_dum 
##              4.063603              3.472087              1.513482
```

```r
## step two: avoid multicolinearity (vif: all below 4)
### identify correlated variables (continuous)
taxi_dat_mod.2017 <- taxi_dat%>%
  st_drop_geometry()%>%
  mutate(log_poverty_rate = log(poverty_rate + 1),
         log_ped_trips_pp = log(ped_trips_pp + 1))%>%
  select(pop_density, edu_ba_per, non_white_per, job_density, log_poverty_rate,
         income, no_veh_hh_per,com_indu_per, walk_score, transit_score_d, drive_per,
         log_ped_trips_pp)

cor(taxi_dat_mod.2017) ## -> highly correlated:pop_density&transit_score_d
```

```
##                  pop_density  edu_ba_per non_white_per job_density
## pop_density       1.00000000  0.09601210    0.08547198  0.15794666
## edu_ba_per        0.09601210  1.00000000   -0.56191512  0.29433743
## non_white_per     0.08547198 -0.56191512    1.00000000 -0.15690227
## job_density       0.15794666  0.29433743   -0.15690227  1.00000000
## log_poverty_rate  0.22654418 -0.54257108    0.64422204 -0.09743012
## income           -0.09964101  0.72736059   -0.66651585  0.15180944
## no_veh_hh_per     0.38208418 -0.23575106    0.49975596  0.19210556
## com_indu_per     -0.10045589  0.01286357   -0.13129730  0.39296744
## walk_score        0.64582168  0.02801943    0.26446579  0.19181951
## transit_score_d   0.85746879  0.22662818    0.01094100  0.61205633
## drive_per        -0.48672262 -0.14674073   -0.27765513 -0.25048650
## log_ped_trips_pp  0.20979638  0.28642049   -0.10216125  0.58298692
##                  log_poverty_rate       income no_veh_hh_per com_indu_per
## pop_density            0.22654418 -0.099641014     0.3820842  -0.10045589
## edu_ba_per            -0.54257108  0.727360592    -0.2357511   0.01286357
## non_white_per          0.64422204 -0.666515852     0.4997560  -0.13129730
## job_density           -0.09743012  0.151809436     0.1921056   0.39296744
## log_poverty_rate       1.00000000 -0.774065978     0.5976721   0.11000652
## income                -0.77406598  1.000000000    -0.6107819  -0.06673940
## no_veh_hh_per          0.59767210 -0.610781912     1.0000000   0.21076363
## com_indu_per           0.11000652 -0.066739405     0.2107636   1.00000000
## walk_score             0.45061395 -0.236510287     0.5160991   0.13305928
## transit_score_d        0.15118705 -0.018708840     0.4641454   0.14251103
## drive_per             -0.28329402  0.256117106    -0.7348815  -0.15401464
## log_ped_trips_pp      -0.04072327 -0.005602768     0.4960479   0.47088640
##                   walk_score transit_score_d  drive_per log_ped_trips_pp
## pop_density       0.64582168      0.85746879 -0.4867226      0.209796378
## edu_ba_per        0.02801943      0.22662818 -0.1467407      0.286420491
## non_white_per     0.26446579      0.01094100 -0.2776551     -0.102161248
## job_density       0.19181951      0.61205633 -0.2504865      0.582986918
## log_poverty_rate  0.45061395      0.15118705 -0.2832940     -0.040723273
## income           -0.23651029     -0.01870884  0.2561171     -0.005602768
## no_veh_hh_per     0.51609910      0.46414545 -0.7348815      0.496047937
## com_indu_per      0.13305928      0.14251103 -0.1540146      0.470886404
## walk_score        1.00000000      0.59480868 -0.5504719      0.315469978
## transit_score_d   0.59480868      1.00000000 -0.5469649      0.477034001
## drive_per        -0.55047191     -0.54696494  1.0000000     -0.606434751
## log_ped_trips_pp  0.31546998      0.47703400 -0.6064348      1.000000000
```

```r
### choose between pop_density&transit_score_d
lm0.1<- lm(taxi_trip_2017 ~ edu_ba_per + non_white_per +job_density + log(poverty_rate+1)
           + income + no_veh_hh_per + com_indu_per + district_dum + inde_station_dum
           + walk_score + transit_score_d + drive_per + log(ped_trips_pp+1) + subway_dum,
           data = taxi_dat)

lm0.2<- lm(taxi_trip_2017 ~ pop_density + edu_ba_per + non_white_per + job_density
           + log(poverty_rate+1) + income + no_veh_hh_per + com_indu_per + district_dum
           + inde_station_dum + walk_score+ drive_per + log(ped_trips_pp+1) + subway_dum,
           data = taxi_dat)

stargazer(lm0.1, lm0.2, type = "text") #-->delete pop_density
```

```
## 
## ============================================================
##                                     Dependent variable:     
##                                -----------------------------
##                                       taxi_trip_2017        
##                                     (1)            (2)      
## ------------------------------------------------------------
## pop_density                                      148.675    
##                                                 (119.484)   
##                                                             
## edu_ba_per                        -171.213       -172.908   
##                                  (139.416)      (140.544)   
##                                                             
## non_white_per                     128.621*       109.867    
##                                   (73.095)       (73.758)   
##                                                             
## job_density                      332.501***     387.645***  
##                                   (41.861)       (33.460)   
##                                                             
## log(poverty_rate + 1)           -7,613.753*    -7,606.985*  
##                                 (4,046.531)    (4,068.638)  
##                                                             
## income                             0.102          0.112     
##                                   (0.152)        (0.153)    
##                                                             
## no_veh_hh_per                    -418.652**     -346.334*   
##                                  (181.079)      (178.590)   
##                                                             
## com_indu_per                       89.894         62.877    
##                                  (134.244)      (136.943)   
##                                                             
## district_dumnon_central          -2,831.638     -4,612.809  
##                                 (7,802.144)    (7,810.653)  
##                                                             
## inde_station_dumnon_indego     -14,173.390*** -12,929.820***
##                                 (4,817.775)    (4,808.974)  
##                                                             
## walk_score                      -334.040***    -293.937***  
##                                  (104.129)      (108.945)   
##                                                             
## transit_score_d                  496.022**                  
##                                  (211.012)                  
##                                                             
## drive_per                        -329.485*      -369.963**  
##                                  (171.002)      (172.457)   
##                                                             
## log(ped_trips_pp + 1)          21,186.460***  20,159.630*** 
##                                 (4,489.606)    (4,490.793)  
##                                                             
## subway_dumsubway                8,655.819**    8,458.503**  
##                                 (3,867.055)    (3,888.648)  
##                                                             
## Constant                        56,886.760**   59,438.010** 
##                                 (24,532.650)   (24,826.220) 
##                                                             
## ------------------------------------------------------------
## Observations                        377            377      
## R2                                 0.629          0.625     
## Adjusted R2                        0.615          0.611     
## Residual Std. Error (df = 362)   27,747.900     27,899.280  
## F Statistic (df = 14; 362)       43.873***      43.118***   
## ============================================================
## Note:                            *p<0.1; **p<0.05; ***p<0.01
```

```r
### delete pop_density
lm1 <-  lm(taxi_trip_2017 ~ edu_ba_per + non_white_per +job_density + log(poverty_rate+1)
           + income + no_veh_hh_per + com_indu_per + district_dum + inde_station_dum
           + walk_score+ transit_score_d + drive_per + log(ped_trips_pp+1) + subway_dum,
           data = taxi_dat)
summary(lm1)
```

```
## 
## Call:
## lm(formula = taxi_trip_2017 ~ edu_ba_per + non_white_per + job_density + 
##     log(poverty_rate + 1) + income + no_veh_hh_per + com_indu_per + 
##     district_dum + inde_station_dum + walk_score + transit_score_d + 
##     drive_per + log(ped_trips_pp + 1) + subway_dum, data = taxi_dat)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -119124   -7362    -371    4898  341576 
## 
## Coefficients:
##                               Estimate  Std. Error t value           Pr(>|t|)
## (Intercept)                 56886.7611  24532.6540   2.319            0.02096
## edu_ba_per                   -171.2129    139.4159  -1.228            0.22022
## non_white_per                 128.6209     73.0954   1.760            0.07932
## job_density                   332.5011     41.8606   7.943 0.0000000000000252
## log(poverty_rate + 1)       -7613.7530   4046.5310  -1.882            0.06070
## income                          0.1016      0.1521   0.668            0.50452
## no_veh_hh_per                -418.6525    181.0788  -2.312            0.02134
## com_indu_per                   89.8943    134.2445   0.670            0.50352
## district_dumnon_central     -2831.6380   7802.1440  -0.363            0.71687
## inde_station_dumnon_indego -14173.3893   4817.7748  -2.942            0.00347
## walk_score                   -334.0402    104.1288  -3.208            0.00146
## transit_score_d               496.0216    211.0120   2.351            0.01928
## drive_per                    -329.4852    171.0020  -1.927            0.05479
## log(ped_trips_pp + 1)       21186.4620   4489.6058   4.719 0.0000033915889807
## subway_dumsubway             8655.8188   3867.0550   2.238            0.02581
##                               
## (Intercept)                *  
## edu_ba_per                    
## non_white_per              .  
## job_density                ***
## log(poverty_rate + 1)      .  
## income                        
## no_veh_hh_per              *  
## com_indu_per                  
## district_dumnon_central       
## inde_station_dumnon_indego ** 
## walk_score                 ** 
## transit_score_d            *  
## drive_per                  .  
## log(ped_trips_pp + 1)      ***
## subway_dumsubway           *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 27750 on 362 degrees of freedom
## Multiple R-squared:  0.6292,	Adjusted R-squared:  0.6148 
## F-statistic: 43.87 on 14 and 362 DF,  p-value: < 0.00000000000000022
```

```r
vif(lm1) # biggest: income = 5.7, cor(income & log_poverty_rate)
```

```
##            edu_ba_per         non_white_per           job_density 
##              3.431096              2.624725              2.767938 
## log(poverty_rate + 1)                income         no_veh_hh_per 
##              3.831784              5.699605              4.888805 
##          com_indu_per          district_dum      inde_station_dum 
##              1.702208              2.764442              1.755996 
##            walk_score       transit_score_d             drive_per 
##              2.433278              3.455413              4.037839 
## log(ped_trips_pp + 1)            subway_dum 
##              3.433111              1.513084
```

```r
### choose between income&log_poverty_rate
lm1.1 <- lm(taxi_trip_2017 ~ edu_ba_per + non_white_per +job_density + log(poverty_rate+1)
            + no_veh_hh_per + com_indu_per + district_dum + inde_station_dum + walk_score
            + transit_score_d + drive_per + log(ped_trips_pp+1) + subway_dum, data = taxi_dat)

lm1.2 <- lm(taxi_trip_2017 ~ edu_ba_per + non_white_per +job_density + income + no_veh_hh_per
            + com_indu_per + district_dum + inde_station_dum +walk_score+ transit_score_d
            + drive_per + log(ped_trips_pp+1) + subway_dum, data = taxi_dat)

stargazer(lm1.1, lm1.2, type = "text") #-->delete income
```

```
## 
## ============================================================
##                                     Dependent variable:     
##                                -----------------------------
##                                       taxi_trip_2017        
##                                     (1)            (2)      
## ------------------------------------------------------------
## edu_ba_per                        -131.662       -184.158   
##                                  (126.126)      (139.732)   
##                                                             
## non_white_per                     124.373*       102.163    
##                                   (72.763)       (71.981)   
##                                                             
## job_density                      332.880***     331.612***  
##                                   (41.825)       (42.004)   
##                                                             
## log(poverty_rate + 1)           -8,776.650**                
##                                 (3,650.215)                 
##                                                             
## income                                            0.225     
##                                                  (0.138)    
##                                                             
## no_veh_hh_per                   -453.414***    -491.913***  
##                                  (173.309)      (177.461)   
##                                                             
## com_indu_per                       88.612         38.705    
##                                  (134.128)      (131.918)   
##                                                             
## district_dumnon_central          -4,585.448     -2,258.173  
##                                 (7,341.624)    (7,823.420)  
##                                                             
## inde_station_dumnon_indego     -14,015.470*** -14,291.070***
##                                 (4,808.301)    (4,834.195)  
##                                                             
## walk_score                      -329.928***    -398.713***  
##                                  (103.867)       (98.636)   
##                                                             
## transit_score_d                  498.644**      496.638**   
##                                  (210.815)      (211.749)   
##                                                             
## drive_per                        -322.459*      -361.951**  
##                                  (170.548)      (170.723)   
##                                                             
## log(ped_trips_pp + 1)          20,813.480***  23,642.440*** 
##                                 (4,451.358)    (4,310.672)  
##                                                             
## subway_dumsubway                8,840.559**    8,294.951**  
##                                 (3,854.212)    (3,875.786)  
##                                                             
## Constant                       66,299.690***   36,829.440*  
##                                 (20,067.730)   (22,172.780) 
##                                                             
## ------------------------------------------------------------
## Observations                        377            377      
## R2                                 0.629          0.626     
## Adjusted R2                        0.615          0.612     
## Residual Std. Error (df = 363)   27,726.730     27,844.820  
## F Statistic (df = 13; 363)       47.286***      46.649***   
## ============================================================
## Note:                            *p<0.1; **p<0.05; ***p<0.01
```

```r
### delete income
lm2 <-  lm(taxi_trip_2017 ~ edu_ba_per + non_white_per +job_density + log(poverty_rate+1)
           + no_veh_hh_per + com_indu_per + district_dum + inde_station_dum + walk_score
           + transit_score_d + drive_per + log(ped_trips_pp+1) + subway_dum, data = taxi_dat)
summary(lm2)
```

```
## 
## Call:
## lm(formula = taxi_trip_2017 ~ edu_ba_per + non_white_per + job_density + 
##     log(poverty_rate + 1) + no_veh_hh_per + com_indu_per + district_dum + 
##     inde_station_dum + walk_score + transit_score_d + drive_per + 
##     log(ped_trips_pp + 1) + subway_dum, data = taxi_dat)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -120769   -7203    -394    4591  340556 
## 
## Coefficients:
##                             Estimate Std. Error t value           Pr(>|t|)    
## (Intercept)                 66299.69   20067.73   3.304            0.00105 ** 
## edu_ba_per                   -131.66     126.13  -1.044            0.29723    
## non_white_per                 124.37      72.76   1.709            0.08825 .  
## job_density                   332.88      41.82   7.959 0.0000000000000224 ***
## log(poverty_rate + 1)       -8776.65    3650.22  -2.404            0.01670 *  
## no_veh_hh_per                -453.41     173.31  -2.616            0.00926 ** 
## com_indu_per                   88.61     134.13   0.661            0.50925    
## district_dumnon_central     -4585.45    7341.62  -0.625            0.53264    
## inde_station_dumnon_indego -14015.47    4808.30  -2.915            0.00378 ** 
## walk_score                   -329.93     103.87  -3.176            0.00162 ** 
## transit_score_d               498.64     210.81   2.365            0.01854 *  
## drive_per                    -322.46     170.55  -1.891            0.05946 .  
## log(ped_trips_pp + 1)       20813.48    4451.36   4.676 0.0000041364231898 ***
## subway_dumsubway             8840.56    3854.21   2.294            0.02238 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 27730 on 363 degrees of freedom
## Multiple R-squared:  0.6287,	Adjusted R-squared:  0.6154 
## F-statistic: 47.29 on 13 and 363 DF,  p-value: < 0.00000000000000022
```

```r
vif(lm2) # biggest: no_veh_hh_per = 4.5, cor(no_veh_hh_per & drive_per)
```

```
##            edu_ba_per         non_white_per           job_density 
##              2.812399              2.604860              2.767428 
## log(poverty_rate + 1)         no_veh_hh_per          com_indu_per 
##              3.122735              4.485127              1.701860 
##          district_dum      inde_station_dum            walk_score 
##              2.451472              1.751769              2.424776 
##       transit_score_d             drive_per log(ped_trips_pp + 1) 
##              3.454217              4.022567              3.380021 
##            subway_dum 
##              1.505347
```

```r
### choose between no_veh_hh_per&drive_per
lm2.1 <- lm(taxi_trip_2017 ~ edu_ba_per + non_white_per + job_density + log(poverty_rate+1)
            + com_indu_per + district_dum + inde_station_dum + walk_score + transit_score_d
            + drive_per + log(ped_trips_pp+1) + subway_dum, data = taxi_dat)

lm2.2 <- lm(taxi_trip_2017 ~ edu_ba_per + non_white_per + job_density + log(poverty_rate+1)
            + no_veh_hh_per + com_indu_per + district_dum + inde_station_dum + walk_score
            + transit_score_d + log(ped_trips_pp+1) + subway_dum, data = taxi_dat)

stargazer(lm2.1, lm2.2, type = "text") #-->delete no_veh_hh_per
```

```
## 
## ============================================================
##                                     Dependent variable:     
##                                -----------------------------
##                                       taxi_trip_2017        
##                                     (1)            (2)      
## ------------------------------------------------------------
## edu_ba_per                        -65.972        -73.240    
##                                  (124.589)      (122.714)   
##                                                             
## non_white_per                      88.765       162.199**   
##                                   (72.050)       (70.205)   
##                                                             
## job_density                      346.725***     311.850***  
##                                   (41.820)       (40.461)   
##                                                             
## log(poverty_rate + 1)          -12,516.630***  -9,346.899** 
##                                 (3,385.500)    (3,650.577)  
##                                                             
## no_veh_hh_per                                   -325.312**  
##                                                 (160.080)   
##                                                             
## com_indu_per                       86.771         73.783    
##                                  (135.199)      (134.372)   
##                                                             
## district_dumnon_central          -4,709.279     -3,999.438  
##                                 (7,400.176)    (7,360.977)  
##                                                             
## inde_station_dumnon_indego     -12,732.970*** -15,829.920***
##                                 (4,821.495)    (4,728.197)  
##                                                             
## walk_score                      -298.397***    -317.854***  
##                                  (103.991)      (104.037)   
##                                                             
## transit_score_d                   383.125*      590.025***  
##                                  (207.787)      (205.924)   
##                                                             
## drive_per                         -148.026                  
##                                  (158.231)                  
##                                                             
## log(ped_trips_pp + 1)          16,742.780***  23,554.740*** 
##                                 (4,203.913)    (4,223.484)  
##                                                             
## subway_dumsubway                8,343.713**   10,404.390*** 
##                                 (3,880.313)    (3,777.710)  
##                                                             
## Constant                       58,093.980***  40,020.760*** 
##                                 (19,979.610)   (14,526.450) 
##                                                             
## ------------------------------------------------------------
## Observations                        377            377      
## R2                                 0.622          0.625     
## Adjusted R2                        0.609          0.613     
## Residual Std. Error (df = 364)   27,948.440     27,824.630  
## F Statistic (df = 12; 364)       49.855***      50.570***   
## ============================================================
## Note:                            *p<0.1; **p<0.05; ***p<0.01
```

```r
### delete drive_per
lm3 <-  lm(taxi_trip_2017 ~ edu_ba_per + non_white_per +job_density + log(poverty_rate+1)
           + no_veh_hh_per + com_indu_per + district_dum + inde_station_dum + walk_score
           + transit_score_d + log(ped_trips_pp+1) + subway_dum, data = taxi_dat)
summary(lm3) #r=0.6127
```

```
## 
## Call:
## lm(formula = taxi_trip_2017 ~ edu_ba_per + non_white_per + job_density + 
##     log(poverty_rate + 1) + no_veh_hh_per + com_indu_per + district_dum + 
##     inde_station_dum + walk_score + transit_score_d + log(ped_trips_pp + 
##     1) + subway_dum, data = taxi_dat)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -124488   -6954    -320    4538  347913 
## 
## Coefficients:
##                             Estimate Std. Error t value          Pr(>|t|)    
## (Intercept)                 40020.76   14526.45   2.755          0.006164 ** 
## edu_ba_per                    -73.24     122.71  -0.597          0.550990    
## non_white_per                 162.20      70.21   2.310          0.021427 *  
## job_density                   311.85      40.46   7.707 0.000000000000123 ***
## log(poverty_rate + 1)       -9346.90    3650.58  -2.560          0.010858 *  
## no_veh_hh_per                -325.31     160.08  -2.032          0.042861 *  
## com_indu_per                   73.78     134.37   0.549          0.583275    
## district_dumnon_central     -3999.44    7360.98  -0.543          0.587235    
## inde_station_dumnon_indego -15829.92    4728.20  -3.348          0.000899 ***
## walk_score                   -317.85     104.04  -3.055          0.002415 ** 
## transit_score_d               590.03     205.92   2.865          0.004409 ** 
## log(ped_trips_pp + 1)       23554.74    4223.48   5.577 0.000000047764432 ***
## subway_dumsubway            10404.39    3777.71   2.754          0.006180 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 27820 on 364 degrees of freedom
## Multiple R-squared:  0.6251,	Adjusted R-squared:  0.6127 
## F-statistic: 50.57 on 12 and 364 DF,  p-value: < 0.00000000000000022
```

```r
vif(lm3) # no variable over 4
```

```
##            edu_ba_per         non_white_per           job_density 
##              2.643598              2.407938              2.571709 
## log(poverty_rate + 1)         no_veh_hh_per          com_indu_per 
##              3.101416              3.799652              1.696041 
##          district_dum      inde_station_dum            walk_score 
##              2.447103              1.681990              2.415610 
##       transit_score_d log(ped_trips_pp + 1)            subway_dum 
##              3.272665              3.021446              1.436022
```

```r
## step three: keep significant variables only (all significant at p = 0.01)
### delete district_dum
lm4 <-  lm(taxi_trip_2017 ~ edu_ba_per + non_white_per +job_density + log(poverty_rate+1)
           + no_veh_hh_per + com_indu_per  + inde_station_dum +walk_score+ transit_score_d
           + log(ped_trips_pp+1) + subway_dum, data = taxi_dat)
summary(lm4) # adjusted r-squared=0.6135
```

```
## 
## Call:
## lm(formula = taxi_trip_2017 ~ edu_ba_per + non_white_per + job_density + 
##     log(poverty_rate + 1) + no_veh_hh_per + com_indu_per + inde_station_dum + 
##     walk_score + transit_score_d + log(ped_trips_pp + 1) + subway_dum, 
##     data = taxi_dat)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -125504   -7007    -480    4289  346852 
## 
## Coefficients:
##                             Estimate Std. Error t value          Pr(>|t|)    
## (Intercept)                 35659.56   12095.66   2.948          0.003403 ** 
## edu_ba_per                    -47.18     112.84  -0.418          0.676123    
## non_white_per                 164.38      70.02   2.348          0.019430 *  
## job_density                   312.11      40.42   7.722 0.000000000000111 ***
## log(poverty_rate + 1)       -9587.58    3620.10  -2.648          0.008438 ** 
## no_veh_hh_per                -327.48     159.88  -2.048          0.041240 *  
## com_indu_per                   87.83     131.73   0.667          0.505367    
## inde_station_dumnon_indego -16406.57    4603.09  -3.564          0.000413 ***
## walk_score                   -311.38     103.25  -3.016          0.002743 ** 
## transit_score_d               611.38     201.94   3.028          0.002641 ** 
## log(ped_trips_pp + 1)       23740.10    4205.62   5.645 0.000000033249266 ***
## subway_dumsubway            10389.63    3773.96   2.753          0.006201 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 27800 on 365 degrees of freedom
## Multiple R-squared:  0.6248,	Adjusted R-squared:  0.6135 
## F-statistic: 55.25 on 11 and 365 DF,  p-value: < 0.00000000000000022
```

```r
### delete edu_ba_per
lm5 <-  lm(taxi_trip_2017 ~ non_white_per +job_density + log(poverty_rate+1) +
             no_veh_hh_per + com_indu_per  + inde_station_dum + walk_score
           + transit_score_d + log(ped_trips_pp+1) + subway_dum, data = taxi_dat)
summary(lm5) # adjusted r-squared=0.6143
```

```
## 
## Call:
## lm(formula = taxi_trip_2017 ~ non_white_per + job_density + log(poverty_rate + 
##     1) + no_veh_hh_per + com_indu_per + inde_station_dum + walk_score + 
##     transit_score_d + log(ped_trips_pp + 1) + subway_dum, data = taxi_dat)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -122938   -7187    -487    4356  347987 
## 
## Coefficients:
##                             Estimate Std. Error t value          Pr(>|t|)    
## (Intercept)                 32747.83    9878.43   3.315          0.001008 ** 
## non_white_per                 171.96      67.56   2.545          0.011333 *  
## job_density                   311.55      40.35   7.721 0.000000000000111 ***
## log(poverty_rate + 1)       -9194.63    3492.03  -2.633          0.008821 ** 
## no_veh_hh_per                -318.36     158.20  -2.012          0.044912 *  
## com_indu_per                   95.48     130.31   0.733          0.464203    
## inde_station_dumnon_indego -15797.37    4361.47  -3.622          0.000334 ***
## walk_score                   -316.61     102.37  -3.093          0.002136 ** 
## transit_score_d               603.78     200.89   3.005          0.002835 ** 
## log(ped_trips_pp + 1)       23509.90    4164.72   5.645 0.000000033160447 ***
## subway_dumsubway            10327.17    3766.75   2.742          0.006413 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 27770 on 366 degrees of freedom
## Multiple R-squared:  0.6246,	Adjusted R-squared:  0.6143 
## F-statistic: 60.89 on 10 and 366 DF,  p-value: < 0.00000000000000022
```

```r
### delete com_indu_per
lm6 <-  lm(taxi_trip_2017 ~ non_white_per +job_density + log(poverty_rate+1) + no_veh_hh_per
           + inde_station_dum +walk_score+ transit_score_d + log(ped_trips_pp+1) + subway_dum,
           data = taxi_dat)
summary(lm6) # adjusted r-squared=0.6148
```

```
## 
## Call:
## lm(formula = taxi_trip_2017 ~ non_white_per + job_density + log(poverty_rate + 
##     1) + no_veh_hh_per + inde_station_dum + walk_score + transit_score_d + 
##     log(ped_trips_pp + 1) + subway_dum, data = taxi_dat)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -122737   -6849    -525    4521  346894 
## 
## Coefficients:
##                             Estimate Std. Error t value            Pr(>|t|)    
## (Intercept)                 31764.11    9780.60   3.248            0.001271 ** 
## non_white_per                 160.26      65.61   2.443            0.015050 *  
## job_density                   319.89      38.69   8.268 0.00000000000000252 ***
## log(poverty_rate + 1)       -8510.30    3362.69  -2.531            0.011797 *  
## no_veh_hh_per                -319.01     158.10  -2.018            0.044340 *  
## inde_station_dumnon_indego -15518.35    4342.07  -3.574            0.000399 ***
## walk_score                   -313.39     102.22  -3.066            0.002331 ** 
## transit_score_d               566.42     194.19   2.917            0.003754 ** 
## log(ped_trips_pp + 1)       24403.68    3979.56   6.132 0.00000000224105732 ***
## subway_dumsubway            10639.18    3740.24   2.845            0.004697 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 27750 on 367 degrees of freedom
## Multiple R-squared:  0.624,	Adjusted R-squared:  0.6148 
## F-statistic: 67.68 on 9 and 367 DF,  p-value: < 0.00000000000000022
```

```r
### delete no_veh_hh_per 
lm7 <-  lm(taxi_trip_2017 ~ non_white_per +job_density + log(poverty_rate+1)
           + inde_station_dum + walk_score+ transit_score_d + log(ped_trips_pp+1)
           + subway_dum, data = taxi_dat)
summary(lm7) # adjusted r-squared=0.6116 (-0.0032)
```

```
## 
## Call:
## lm(formula = taxi_trip_2017 ~ non_white_per + job_density + log(poverty_rate + 
##     1) + inde_station_dum + walk_score + transit_score_d + log(ped_trips_pp + 
##     1) + subway_dum, data = taxi_dat)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -130928   -6757      -7    4959  354244 
## 
## Coefficients:
##                             Estimate Std. Error t value             Pr(>|t|)
## (Intercept)                 38362.55    9256.10   4.145        0.00004228664
## non_white_per                 107.78      60.48   1.782             0.075579
## job_density                   341.77      37.29   9.164 < 0.0000000000000002
## log(poverty_rate + 1)      -11688.73    2983.30  -3.918             0.000106
## inde_station_dumnon_indego -14054.81    4298.89  -3.269             0.001179
## walk_score                   -288.09     101.87  -2.828             0.004938
## transit_score_d               433.21     183.39   2.362             0.018685
## log(ped_trips_pp + 1)       19884.64    3303.24   6.020        0.00000000423
## subway_dumsubway             9546.96    3716.28   2.569             0.010594
##                               
## (Intercept)                ***
## non_white_per              .  
## job_density                ***
## log(poverty_rate + 1)      ***
## inde_station_dumnon_indego ** 
## walk_score                 ** 
## transit_score_d            *  
## log(ped_trips_pp + 1)      ***
## subway_dumsubway           *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 27860 on 368 degrees of freedom
## Multiple R-squared:  0.6199,	Adjusted R-squared:  0.6116 
## F-statistic: 75.01 on 8 and 368 DF,  p-value: < 0.00000000000000022
```

```r
### delete non_white_per
lm8 <-  lm(taxi_trip_2017 ~ job_density + log(poverty_rate+1) + inde_station_dum
           + walk_score + transit_score_d + log(ped_trips_pp+1) + subway_dum, data = taxi_dat)
summary(lm8) # adjusted r-squared=0.6039(-0.0077)
```

```
## 
## Call:
## lm(formula = taxi_trip_2017 ~ job_density + log(poverty_rate + 
##     1) + inde_station_dum + walk_score + transit_score_d + log(ped_trips_pp + 
##     1) + subway_dum, data = taxi_dat)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -128079   -6292    -238    4799  361636 
## 
## Coefficients:
##                             Estimate Std. Error t value             Pr(>|t|)
## (Intercept)                 34050.30    8960.46   3.800             0.000169
## job_density                   339.49      37.38   9.082 < 0.0000000000000002
## log(poverty_rate + 1)       -8622.68    2444.31  -3.528             0.000472
## inde_station_dumnon_indego -13366.55    4294.11  -3.113             0.001998
## walk_score                   -274.78     101.89  -2.697             0.007322
## transit_score_d               417.07     183.70   2.270             0.023764
## log(ped_trips_pp + 1)       19992.29    3312.41   6.036        0.00000000386
## subway_dumsubway             8864.27    3707.36   2.391             0.017303
##                               
## (Intercept)                ***
## job_density                ***
## log(poverty_rate + 1)      ***
## inde_station_dumnon_indego ** 
## walk_score                 ** 
## transit_score_d            *  
## log(ped_trips_pp + 1)      ***
## subway_dumsubway           *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 27950 on 369 degrees of freedom
## Multiple R-squared:  0.6166,	Adjusted R-squared:  0.6093 
## F-statistic: 84.77 on 7 and 369 DF,  p-value: < 0.00000000000000022
```

```r
### delete transit_score_d
lm9 <-  lm(taxi_trip_2017 ~ job_density + log(poverty_rate+1) + inde_station_dum
           + walk_score+ log(ped_trips_pp+1) + subway_dum, data = taxi_dat)
summary(lm9) # adjusted r-squared=0.6049(-0010)
```

```
## 
## Call:
## lm(formula = taxi_trip_2017 ~ job_density + log(poverty_rate + 
##     1) + inde_station_dum + walk_score + log(ped_trips_pp + 1) + 
##     subway_dum, data = taxi_dat)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -127228   -6470    -782    5591  362823 
## 
## Coefficients:
##                             Estimate Std. Error t value             Pr(>|t|)
## (Intercept)                 31503.07    8939.71   3.524             0.000478
## job_density                   385.35      31.63  12.184 < 0.0000000000000002
## log(poverty_rate + 1)       -8824.89    2456.36  -3.593             0.000372
## inde_station_dumnon_indego -13331.18    4318.12  -3.087             0.002172
## walk_score                   -152.93      87.09  -1.756             0.079933
## log(ped_trips_pp + 1)       20190.15    3329.80   6.063        0.00000000329
## subway_dumsubway             9115.64    3726.45   2.446             0.014902
##                               
## (Intercept)                ***
## job_density                ***
## log(poverty_rate + 1)      ***
## inde_station_dumnon_indego ** 
## walk_score                 .  
## log(ped_trips_pp + 1)      ***
## subway_dumsubway           *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 28100 on 370 degrees of freedom
## Multiple R-squared:  0.6112,	Adjusted R-squared:  0.6049 
## F-statistic: 96.95 on 6 and 370 DF,  p-value: < 0.00000000000000022
```

```r
### delete walk_score
lm10 <-  lm(taxi_trip_2017 ~ job_density + log(poverty_rate+1) + inde_station_dum
            + log(ped_trips_pp+1) + subway_dum, data = taxi_dat)
summary(lm10) # adjusted r-squared=0.6027(-0.0022)
```

```
## 
## Call:
## lm(formula = taxi_trip_2017 ~ job_density + log(poverty_rate + 
##     1) + inde_station_dum + log(ped_trips_pp + 1) + subway_dum, 
##     data = taxi_dat)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -118266   -6507    -472    5275  371340 
## 
## Coefficients:
##                            Estimate Std. Error t value             Pr(>|t|)    
## (Intercept)                 26152.4     8427.9   3.103              0.00206 ** 
## job_density                   383.4       31.7  12.094 < 0.0000000000000002 ***
## log(poverty_rate + 1)      -10931.4     2149.5  -5.086         0.0000005831 ***
## inde_station_dumnon_indego -11589.1     4214.4  -2.750              0.00625 ** 
## log(ped_trips_pp + 1)       19387.9     3307.6   5.862         0.0000000101 ***
## subway_dumsubway             7727.6     3651.8   2.116              0.03500 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 28180 on 371 degrees of freedom
## Multiple R-squared:  0.608,	Adjusted R-squared:  0.6027 
## F-statistic: 115.1 on 5 and 371 DF,  p-value: < 0.00000000000000022
```

```r
### delete subway_dum
lm11 <-  lm(taxi_trip_2017 ~ job_density + log(poverty_rate+1) + inde_station_dum
            + log(ped_trips_pp+1) , data = taxi_dat)
summary(lm11) # adjusted r-squared=0.599(-0.0037)
```

```
## 
## Call:
## lm(formula = taxi_trip_2017 ~ job_density + log(poverty_rate + 
##     1) + inde_station_dum + log(ped_trips_pp + 1), data = taxi_dat)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -127775   -6923    -230    4693  375539 
## 
## Coefficients:
##                             Estimate Std. Error t value             Pr(>|t|)
## (Intercept)                 25506.60    8461.64   3.014              0.00275
## job_density                   386.41      31.81  12.147 < 0.0000000000000002
## log(poverty_rate + 1)      -10077.88    2121.17  -4.751       0.000002894515
## inde_station_dumnon_indego -13605.64    4124.37  -3.299              0.00106
## log(ped_trips_pp + 1)       21113.72    3220.38   6.556       0.000000000185
##                               
## (Intercept)                ** 
## job_density                ***
## log(poverty_rate + 1)      ***
## inde_station_dumnon_indego ** 
## log(ped_trips_pp + 1)      ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 28310 on 372 degrees of freedom
## Multiple R-squared:  0.6033,	Adjusted R-squared:  0.599 
## F-statistic: 141.4 on 4 and 372 DF,  p-value: < 0.00000000000000022
```

```r
## step four: add interaction variables
### possible variables:
#### log(poverty_rate+1)*inde_station_dum
lm12 <-  lm(taxi_trip_2017 ~ job_density + log(poverty_rate+1)*inde_station_dum
            + log(ped_trips_pp+1) , data = taxi_dat)
summary(lm12) # adjusted r-squared=0.645
```

```
## 
## Call:
## lm(formula = taxi_trip_2017 ~ job_density + log(poverty_rate + 
##     1) * inde_station_dum + log(ped_trips_pp + 1), data = taxi_dat)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -91140  -3879    137   3501 309920 
## 
## Coefficients:
##                                                    Estimate Std. Error t value
## (Intercept)                                       110594.61   14510.97   7.621
## job_density                                          377.15      29.96  12.588
## log(poverty_rate + 1)                             -37222.35    4354.55  -8.548
## inde_station_dumnon_indego                       -118778.66   15489.51  -7.668
## log(ped_trips_pp + 1)                              18769.15    3048.47   6.157
## log(poverty_rate + 1):inde_station_dumnon_indego   34425.93    4908.43   7.014
##                                                              Pr(>|t|)    
## (Intercept)                                      0.000000000000211761 ***
## job_density                                      < 0.0000000000000002 ***
## log(poverty_rate + 1)                            0.000000000000000331 ***
## inde_station_dumnon_indego                       0.000000000000154581 ***
## log(ped_trips_pp + 1)                            0.000000001927884499 ***
## log(poverty_rate + 1):inde_station_dumnon_indego 0.000000000011062055 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 26640 on 371 degrees of freedom
## Multiple R-squared:  0.6497,	Adjusted R-squared:  0.645 
## F-statistic: 137.6 on 5 and 371 DF,  p-value: < 0.00000000000000022
```

```r
#### log(poverty_rate+1)*log(ped_trips_pp+1)
lm13 <-  lm(taxi_trip_2017 ~ job_density + log(poverty_rate+1)* log(ped_trips_pp+1)
            + inde_station_dum , data = taxi_dat)
summary(lm13) # adjusted r-squared=0.6073
```

```
## 
## Call:
## lm(formula = taxi_trip_2017 ~ job_density + log(poverty_rate + 
##     1) * log(ped_trips_pp + 1) + inde_station_dum, data = taxi_dat)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -78917  -7594    162   6142 380168 
## 
## Coefficients:
##                                              Estimate Std. Error t value
## (Intercept)                                  38237.54    9404.46   4.066
## job_density                                    348.96      33.91  10.292
## log(poverty_rate + 1)                       -18965.91    3652.03  -5.193
## log(ped_trips_pp + 1)                        11794.20    4469.42   2.639
## inde_station_dumnon_indego                   -9408.31    4318.66  -2.179
## log(poverty_rate + 1):log(ped_trips_pp + 1)   6559.45    2205.51   2.974
##                                                         Pr(>|t|)    
## (Intercept)                                          0.000058455 ***
## job_density                                 < 0.0000000000000002 ***
## log(poverty_rate + 1)                                0.000000341 ***
## log(ped_trips_pp + 1)                                    0.00867 ** 
## inde_station_dumnon_indego                               0.03000 *  
## log(poverty_rate + 1):log(ped_trips_pp + 1)              0.00313 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 28020 on 371 degrees of freedom
## Multiple R-squared:  0.6125,	Adjusted R-squared:  0.6073 
## F-statistic: 117.3 on 5 and 371 DF,  p-value: < 0.00000000000000022
```

```r
#### log(ped_trips_pp+1)*inde_station_dum
lm14 <-  lm(taxi_trip_2017 ~ job_density + log(poverty_rate+1) + log(ped_trips_pp+1)*inde_station_dum,
            data = taxi_dat)
summary(lm14) # adjusted r-squared=0.6843
```

```
## 
## Call:
## lm(formula = taxi_trip_2017 ~ job_density + log(poverty_rate + 
##     1) + log(ped_trips_pp + 1) * inde_station_dum, data = taxi_dat)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -77047  -5432   -359   3738 333502 
## 
## Coefficients:
##                                                   Estimate Std. Error t value
## (Intercept)                                      -68547.51   11979.42  -5.722
## job_density                                         125.02      38.34   3.261
## log(poverty_rate + 1)                             -8190.54    1891.38  -4.330
## log(ped_trips_pp + 1)                             84096.15    6873.07  12.236
## inde_station_dumnon_indego                        84820.11   10431.63   8.131
## log(ped_trips_pp + 1):inde_station_dumnon_indego -71975.72    7143.53 -10.076
##                                                              Pr(>|t|)    
## (Intercept)                                       0.00000002170770018 ***
## job_density                                                   0.00121 ** 
## log(poverty_rate + 1)                             0.00001917180929183 ***
## log(ped_trips_pp + 1)                            < 0.0000000000000002 ***
## inde_station_dumnon_indego                        0.00000000000000645 ***
## log(ped_trips_pp + 1):inde_station_dumnon_indego < 0.0000000000000002 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 25120 on 371 degrees of freedom
## Multiple R-squared:  0.6885,	Adjusted R-squared:  0.6843 
## F-statistic:   164 on 5 and 371 DF,  p-value: < 0.00000000000000022
```

```r
### model comparison
anova(lm11,lm14)
```

```
## Analysis of Variance Table
## 
## Model 1: taxi_trip_2017 ~ job_density + log(poverty_rate + 1) + inde_station_dum + 
##     log(ped_trips_pp + 1)
## Model 2: taxi_trip_2017 ~ job_density + log(poverty_rate + 1) + log(ped_trips_pp + 
##     1) * inde_station_dum
##   Res.Df          RSS Df   Sum of Sq      F                Pr(>F)    
## 1    372 298207298212                                                
## 2    371 234138665357  1 64068632855 101.52 < 0.00000000000000022 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
## step five:assumption
vif(lm11)
```

```
##           job_density log(poverty_rate + 1)      inde_station_dum 
##              1.535285              1.011281              1.236036 
## log(ped_trips_pp + 1) 
##              1.696563
```

```r
plot(lm14)
```

![](Assignment-2_Viva-Yaohan_files/figure-html/task7-1-1.png)<!-- -->![](Assignment-2_Viva-Yaohan_files/figure-html/task7-1-2.png)<!-- -->![](Assignment-2_Viva-Yaohan_files/figure-html/task7-1-3.png)<!-- -->![](Assignment-2_Viva-Yaohan_files/figure-html/task7-1-4.png)<!-- -->

```r
#interpretation
-8190.54*log(1+0.01)
```

```
## [1] -81.49858
```

```r
84096*log(1+0.01)
```

```
## [1] 836.783
```

```r
-71976*log(1+0.01)
```

```
## [1] -716.185
```


```r
# multiple regression for logged 2017 taxi trips

## add dummy variable for median age
taxi_dat <- taxi_dat %>%
  mutate(median_age_dum = ifelse(median_age_cat == "age_4th","old","young"))

summary(lm(log(taxi_trip_2017) ~ median_age_dum, data = taxi_dat))
```

```
## 
## Call:
## lm(formula = log(taxi_trip_2017) ~ median_age_dum, data = taxi_dat)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -4.8902 -1.6410 -0.3136  1.2705  7.2368 
## 
## Coefficients:
##                     Estimate Std. Error t value             Pr(>|t|)    
## (Intercept)           5.5010     0.2083  26.406 < 0.0000000000000002 ***
## median_age_dumyoung   1.9541     0.2404   8.127  0.00000000000000645 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.02 on 375 degrees of freedom
## Multiple R-squared:  0.1498,	Adjusted R-squared:  0.1475 
## F-statistic: 66.05 on 1 and 375 DF,  p-value: 0.000000000000006452
```

```r
## step one: simple regression and plot
taxi_log_2017 <- taxi_dat %>%
  select(taxi_trip_2017, pop_density, job_density, poverty_rate, no_veh_hh_per, walk_score,
         edu_ba_per, transit_score_d, drive_per, transit_per, ped_trips_pp, median_age_dum,
         district_dum, inde_station_dum, subway_dum)

lma <- lm(log(taxi_trip_2017) ~ pop_density + log(job_density + 1) + poverty_rate
          + no_veh_hh_per + walk_score + edu_ba_per + transit_score_d + drive_per
          + log(transit_per + 1) + log(ped_trips_pp + 1) + median_age_dum
          + district_dum + inde_station_dum + subway_dum, data = taxi_log_2017)
summary(lma)
```

```
## 
## Call:
## lm(formula = log(taxi_trip_2017) ~ pop_density + log(job_density + 
##     1) + poverty_rate + no_veh_hh_per + walk_score + edu_ba_per + 
##     transit_score_d + drive_per + log(transit_per + 1) + log(ped_trips_pp + 
##     1) + median_age_dum + district_dum + inde_station_dum + subway_dum, 
##     data = taxi_log_2017)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -2.7974 -0.7129 -0.0372  0.7091  3.5668 
## 
## Coefficients:
##                              Estimate Std. Error t value       Pr(>|t|)    
## (Intercept)                 5.4176604  0.8466876   6.399 0.000000000485 ***
## pop_density                 0.0184828  0.0085653   2.158       0.031596 *  
## log(job_density + 1)        0.1859977  0.1044456   1.781       0.075782 .  
## poverty_rate               -0.0265534  0.0070228  -3.781       0.000183 ***
## no_veh_hh_per               0.0048590  0.0070293   0.691       0.489854    
## walk_score                 -0.0008272  0.0045215  -0.183       0.854942    
## edu_ba_per                 -0.0084775  0.0048125  -1.762       0.078985 .  
## transit_score_d            -0.0061022  0.0128827  -0.474       0.636019    
## drive_per                  -0.0277227  0.0066831  -4.148 0.000041798823 ***
## log(transit_per + 1)        0.8945707  0.1548187   5.778 0.000000016294 ***
## log(ped_trips_pp + 1)       1.0890910  0.1697105   6.417 0.000000000435 ***
## median_age_dumyoung         0.4446097  0.1601168   2.777       0.005776 ** 
## district_dumnon_central    -1.0375695  0.2844570  -3.648       0.000304 ***
## inde_station_dumnon_indego -0.8565534  0.1844300  -4.644 0.000004781638 ***
## subway_dumsubway            0.4872997  0.1494757   3.260       0.001220 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.07 on 362 degrees of freedom
## Multiple R-squared:  0.7696,	Adjusted R-squared:  0.7607 
## F-statistic: 86.38 on 14 and 362 DF,  p-value: < 0.00000000000000022
```

```r
vif(lma) #biggest: transit_score_d = 8.7
```

```
##           pop_density  log(job_density + 1)          poverty_rate 
##              7.688482              3.575681              3.433975 
##         no_veh_hh_per            walk_score            edu_ba_per 
##              4.953816              3.085069              2.749108 
##       transit_score_d             drive_per  log(transit_per + 1) 
##              8.660553              4.147126              2.427559 
## log(ped_trips_pp + 1)        median_age_dum          district_dum 
##              3.298651              1.579916              2.470929 
##      inde_station_dum            subway_dum 
##              1.730379              1.520165
```

```r
## step two: avoid multicolinearity (vif: all below 4)
### identify correlated variables (continuous)
cor(taxi_log_2017 %>%
      st_drop_geometry() %>%
      mutate(job_density = log(job_density + 1), transit_per = log(transit_per + 1),
             ped_trips_pp = log(ped_trips_pp + 1)) %>%
      select(pop_density, job_density, poverty_rate, no_veh_hh_per, walk_score,
             edu_ba_per, transit_score_d,drive_per, transit_per, ped_trips_pp
             )) ## -> highly correlated:pop_density&transit_score_d
```

```
##                 pop_density job_density poverty_rate no_veh_hh_per  walk_score
## pop_density       1.0000000  0.27818032   0.17211572     0.3820842  0.64582168
## job_density       0.2781803  1.00000000  -0.03458097     0.3001766  0.41238091
## poverty_rate      0.1721157 -0.03458097   1.00000000     0.6573707  0.36708705
## no_veh_hh_per     0.3820842  0.30017659   0.65737073     1.0000000  0.51609910
## walk_score        0.6458217  0.41238091   0.36708705     0.5160991  1.00000000
## edu_ba_per        0.0960121  0.43962696  -0.56982301    -0.2357511  0.02801943
## transit_score_d   0.8574688  0.56718667   0.11528040     0.4641454  0.59480868
## drive_per        -0.4867226 -0.44462520  -0.34319312    -0.7348815 -0.55047191
## transit_per       0.3048426 -0.03740426   0.58260714     0.5417197  0.53355192
## ped_trips_pp      0.2097964  0.68482552   0.02344180     0.4960479  0.31546998
##                  edu_ba_per transit_score_d  drive_per  transit_per
## pop_density      0.09601210       0.8574688 -0.4867226  0.304842606
## job_density      0.43962696       0.5671867 -0.4446252 -0.037404262
## poverty_rate    -0.56982301       0.1152804 -0.3431931  0.582607139
## no_veh_hh_per   -0.23575106       0.4641454 -0.7348815  0.541719677
## walk_score       0.02801943       0.5948087 -0.5504719  0.533551922
## edu_ba_per       1.00000000       0.2266282 -0.1467407 -0.278525436
## transit_score_d  0.22662818       1.0000000 -0.5469649  0.235304799
## drive_per       -0.14674073      -0.5469649  1.0000000 -0.454774796
## transit_per     -0.27852544       0.2353048 -0.4547748  1.000000000
## ped_trips_pp     0.28642049       0.4770340 -0.6064348  0.007944566
##                 ped_trips_pp
## pop_density      0.209796378
## job_density      0.684825518
## poverty_rate     0.023441799
## no_veh_hh_per    0.496047937
## walk_score       0.315469978
## edu_ba_per       0.286420491
## transit_score_d  0.477034001
## drive_per       -0.606434751
## transit_per      0.007944566
## ped_trips_pp     1.000000000
```

```r
### choose between pop_density&transit_score_d
lma.1 <- lm(log(taxi_trip_2017) ~ log(job_density + 1) + poverty_rate
          + no_veh_hh_per + walk_score + edu_ba_per + transit_score_d + drive_per
          + log(transit_per + 1) + log(ped_trips_pp + 1) + median_age_dum
          + district_dum + inde_station_dum + subway_dum, data = taxi_log_2017)
summary(lma.1)
```

```
## 
## Call:
## lm(formula = log(taxi_trip_2017) ~ log(job_density + 1) + poverty_rate + 
##     no_veh_hh_per + walk_score + edu_ba_per + transit_score_d + 
##     drive_per + log(transit_per + 1) + log(ped_trips_pp + 1) + 
##     median_age_dum + district_dum + inde_station_dum + subway_dum, 
##     data = taxi_log_2017)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -2.8451 -0.7283 -0.0655  0.7225  3.5454 
## 
## Coefficients:
##                             Estimate Std. Error t value         Pr(>|t|)    
## (Intercept)                 5.856733   0.826003   7.090 0.00000000000703 ***
## log(job_density + 1)        0.098323   0.096702   1.017         0.309943    
## poverty_rate               -0.026895   0.007056  -3.812         0.000162 ***
## no_veh_hh_per               0.003354   0.007030   0.477         0.633527    
## walk_score                  0.003166   0.004146   0.764         0.445595    
## edu_ba_per                 -0.008323   0.004836  -1.721         0.086092 .  
## transit_score_d             0.017827   0.006590   2.705         0.007147 ** 
## drive_per                  -0.031042   0.006536  -4.749 0.00000294710661 ***
## log(transit_per + 1)        0.829640   0.152629   5.436 0.00000010038541 ***
## log(ped_trips_pp + 1)       0.984340   0.163437   6.023 0.00000000420493 ***
## median_age_dumyoung         0.505986   0.158362   3.195         0.001520 ** 
## district_dumnon_central    -0.976570   0.284471  -3.433         0.000666 ***
## inde_station_dumnon_indego -0.858993   0.185353  -4.634 0.00000499981255 ***
## subway_dumsubway            0.492909   0.150204   3.282         0.001132 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.075 on 363 degrees of freedom
## Multiple R-squared:  0.7667,	Adjusted R-squared:  0.7583 
## F-statistic: 91.75 on 13 and 363 DF,  p-value: < 0.00000000000000022
```

```r
lma.2 <- lm(log(taxi_trip_2017) ~ pop_density + log(job_density + 1) + poverty_rate
          + no_veh_hh_per + walk_score + edu_ba_per + drive_per
          + log(transit_per + 1) + log(ped_trips_pp + 1) + median_age_dum
          + district_dum + inde_station_dum + subway_dum, data = taxi_log_2017)
summary(lma.2) #-->delete transit_score_d
```

```
## 
## Call:
## lm(formula = log(taxi_trip_2017) ~ pop_density + log(job_density + 
##     1) + poverty_rate + no_veh_hh_per + walk_score + edu_ba_per + 
##     drive_per + log(transit_per + 1) + log(ped_trips_pp + 1) + 
##     median_age_dum + district_dum + inde_station_dum + subway_dum, 
##     data = taxi_log_2017)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -2.7914 -0.7144 -0.0497  0.7141  3.5546 
## 
## Coefficients:
##                              Estimate Std. Error t value       Pr(>|t|)    
## (Intercept)                 5.4940960  0.8302795   6.617 0.000000000132 ***
## pop_density                 0.0149904  0.0043548   3.442       0.000644 ***
## log(job_density + 1)        0.1636288  0.0930601   1.758       0.079537 .  
## poverty_rate               -0.0263443  0.0070014  -3.763       0.000196 ***
## no_veh_hh_per               0.0042422  0.0069003   0.615       0.539082    
## walk_score                 -0.0003836  0.0044187  -0.087       0.930868    
## edu_ba_per                 -0.0084381  0.0048066  -1.756       0.080012 .  
## drive_per                  -0.0281843  0.0066046  -4.267 0.000025267750 ***
## log(transit_per + 1)        0.8824586  0.1525292   5.786 0.000000015626 ***
## log(ped_trips_pp + 1)       1.0719661  0.1656377   6.472 0.000000000314 ***
## median_age_dumyoung         0.4548225  0.1584889   2.870       0.004349 ** 
## district_dumnon_central    -1.0137766  0.2796876  -3.625       0.000331 ***
## inde_station_dumnon_indego -0.8633436  0.1836755  -4.700 0.000003693090 ***
## subway_dumsubway            0.4901301  0.1491965   3.285       0.001119 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.069 on 363 degrees of freedom
## Multiple R-squared:  0.7695,	Adjusted R-squared:  0.7612 
## F-statistic: 93.21 on 13 and 363 DF,  p-value: < 0.00000000000000022
```

```r
### delete transit_score_d
lmb <- lm(log(taxi_trip_2017) ~ pop_density + log(job_density + 1) + poverty_rate
          + no_veh_hh_per + walk_score + edu_ba_per + drive_per
          + log(transit_per + 1) + log(ped_trips_pp + 1) + median_age_dum
          + district_dum + inde_station_dum + subway_dum, data = taxi_log_2017)
vif(lmb) #biggest: no_veh_hh_per = 4.8
```

```
##           pop_density  log(job_density + 1)          poverty_rate 
##              1.991649              2.844688              3.420417 
##         no_veh_hh_per            walk_score            edu_ba_per 
##              4.783810              2.952721              2.748287 
##             drive_per  log(transit_per + 1) log(ped_trips_pp + 1) 
##              4.058931              2.361337              3.148953 
##        median_age_dum          district_dum      inde_station_dum 
##              1.551268              2.393880              1.719925 
##            subway_dum 
##              1.517736
```

```r
cor(taxi_log_2017 %>%
      st_drop_geometry() %>%
      mutate(job_density = log(job_density + 1), transit_per = log(transit_per + 1),
             ped_trips_pp = log(ped_trips_pp + 1)) %>%
      select(pop_density, job_density, poverty_rate, no_veh_hh_per, walk_score,
             edu_ba_per, drive_per, transit_per, ped_trips_pp))
```

```
##               pop_density job_density poverty_rate no_veh_hh_per  walk_score
## pop_density     1.0000000  0.27818032   0.17211572     0.3820842  0.64582168
## job_density     0.2781803  1.00000000  -0.03458097     0.3001766  0.41238091
## poverty_rate    0.1721157 -0.03458097   1.00000000     0.6573707  0.36708705
## no_veh_hh_per   0.3820842  0.30017659   0.65737073     1.0000000  0.51609910
## walk_score      0.6458217  0.41238091   0.36708705     0.5160991  1.00000000
## edu_ba_per      0.0960121  0.43962696  -0.56982301    -0.2357511  0.02801943
## drive_per      -0.4867226 -0.44462520  -0.34319312    -0.7348815 -0.55047191
## transit_per     0.3048426 -0.03740426   0.58260714     0.5417197  0.53355192
## ped_trips_pp    0.2097964  0.68482552   0.02344180     0.4960479  0.31546998
##                edu_ba_per  drive_per  transit_per ped_trips_pp
## pop_density    0.09601210 -0.4867226  0.304842606  0.209796378
## job_density    0.43962696 -0.4446252 -0.037404262  0.684825518
## poverty_rate  -0.56982301 -0.3431931  0.582607139  0.023441799
## no_veh_hh_per -0.23575106 -0.7348815  0.541719677  0.496047937
## walk_score     0.02801943 -0.5504719  0.533551922  0.315469978
## edu_ba_per     1.00000000 -0.1467407 -0.278525436  0.286420491
## drive_per     -0.14674073  1.0000000 -0.454774796 -0.606434751
## transit_per   -0.27852544 -0.4547748  1.000000000  0.007944566
## ped_trips_pp   0.28642049 -0.6064348  0.007944566  1.000000000
```

```r
### choose between drive_per&no_veh_hh_per
lmb.1 <- lm(log(taxi_trip_2017) ~ pop_density + log(job_density + 1) + poverty_rate
          + no_veh_hh_per + walk_score + edu_ba_per
          + log(transit_per + 1) + log(ped_trips_pp + 1) + median_age_dum
          + district_dum + inde_station_dum + subway_dum, data = taxi_log_2017)
summary(lmb.1)
```

```
## 
## Call:
## lm(formula = log(taxi_trip_2017) ~ pop_density + log(job_density + 
##     1) + poverty_rate + no_veh_hh_per + walk_score + edu_ba_per + 
##     log(transit_per + 1) + log(ped_trips_pp + 1) + median_age_dum + 
##     district_dum + inde_station_dum + subway_dum, data = taxi_log_2017)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -2.7946 -0.7305 -0.0287  0.7272  4.2492 
## 
## Coefficients:
##                             Estimate Std. Error t value          Pr(>|t|)    
## (Intercept)                 2.887264   0.575457   5.017 0.000000820909273 ***
## pop_density                 0.018851   0.004359   4.324 0.000019774503834 ***
## log(job_density + 1)        0.123316   0.094743   1.302          0.193881    
## poverty_rate               -0.027882   0.007156  -3.897          0.000116 ***
## no_veh_hh_per               0.017155   0.006346   2.703          0.007191 ** 
## walk_score                 -0.001716   0.004511  -0.380          0.703801    
## edu_ba_per                 -0.003600   0.004780  -0.753          0.451811    
## log(transit_per + 1)        1.050079   0.150829   6.962 0.000000000015724 ***
## log(ped_trips_pp + 1)       1.262128   0.163259   7.731 0.000000000000105 ***
## median_age_dumyoung         0.603888   0.158204   3.817          0.000159 ***
## district_dumnon_central    -1.020905   0.286218  -3.567          0.000410 ***
## inde_station_dumnon_indego -0.987742   0.185585  -5.322 0.000000179553686 ***
## subway_dumsubway            0.599723   0.150404   3.987 0.000080746426330 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.094 on 364 degrees of freedom
## Multiple R-squared:  0.7579,	Adjusted R-squared:  0.7499 
## F-statistic: 94.97 on 12 and 364 DF,  p-value: < 0.00000000000000022
```

```r
lmb.2 <- lm(log(taxi_trip_2017) ~ pop_density + log(job_density + 1) + poverty_rate
          + walk_score + edu_ba_per + drive_per
          + log(transit_per + 1) + log(ped_trips_pp + 1) + median_age_dum
          + district_dum + inde_station_dum + subway_dum, data = taxi_log_2017)
summary(lmb.2) #-->delete no_veh_hh_per
```

```
## 
## Call:
## lm(formula = log(taxi_trip_2017) ~ pop_density + log(job_density + 
##     1) + poverty_rate + walk_score + edu_ba_per + drive_per + 
##     log(transit_per + 1) + log(ped_trips_pp + 1) + median_age_dum + 
##     district_dum + inde_station_dum + subway_dum, data = taxi_log_2017)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -2.7702 -0.7125 -0.0530  0.7168  3.4180 
## 
## Coefficients:
##                              Estimate Std. Error t value         Pr(>|t|)    
## (Intercept)                 5.6283881  0.8003453   7.032 0.00000000001010 ***
## pop_density                 0.0153130  0.0043194   3.545         0.000443 ***
## log(job_density + 1)        0.1621346  0.0929488   1.744         0.081943 .  
## poverty_rate               -0.0242037  0.0060691  -3.988 0.00008054012807 ***
## walk_score                 -0.0002907  0.0044124  -0.066         0.947501    
## edu_ba_per                 -0.0090649  0.0046932  -1.931         0.054199 .  
## drive_per                  -0.0299649  0.0059306  -5.053 0.00000069117621 ***
## log(transit_per + 1)        0.8912960  0.1517205   5.875 0.00000000957102 ***
## log(ped_trips_pp + 1)       1.1082864  0.1546115   7.168 0.00000000000426 ***
## median_age_dumyoung         0.4293387  0.1528415   2.809         0.005237 ** 
## district_dumnon_central    -1.0171344  0.2793952  -3.640         0.000312 ***
## inde_station_dumnon_indego -0.8732709  0.1828079  -4.777 0.00000258455246 ***
## subway_dumsubway            0.4878457  0.1490228   3.274         0.001163 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.068 on 364 degrees of freedom
## Multiple R-squared:  0.7692,	Adjusted R-squared:  0.7616 
## F-statistic: 101.1 on 12 and 364 DF,  p-value: < 0.00000000000000022
```

```r
### delete no_veh_hh_per
lmc <- lm(log(taxi_trip_2017) ~ pop_density + log(job_density + 1) + poverty_rate
          + walk_score + edu_ba_per + drive_per
          + log(transit_per + 1) + log(ped_trips_pp + 1) + median_age_dum
          + district_dum + inde_station_dum + subway_dum, data = taxi_log_2017)
vif(lmc) # no variable over 4
```

```
##           pop_density  log(job_density + 1)          poverty_rate 
##              1.962728              2.842748              2.574483 
##            walk_score            edu_ba_per             drive_per 
##              2.949271              2.624652              3.278402 
##  log(transit_per + 1) log(ped_trips_pp + 1)        median_age_dum 
##              2.340364              2.748363              1.445154 
##          district_dum      inde_station_dum            subway_dum 
##              2.392967              1.706632              1.516794
```

```r
summary(lmc) # adjusted r-squared=0.7616
```

```
## 
## Call:
## lm(formula = log(taxi_trip_2017) ~ pop_density + log(job_density + 
##     1) + poverty_rate + walk_score + edu_ba_per + drive_per + 
##     log(transit_per + 1) + log(ped_trips_pp + 1) + median_age_dum + 
##     district_dum + inde_station_dum + subway_dum, data = taxi_log_2017)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -2.7702 -0.7125 -0.0530  0.7168  3.4180 
## 
## Coefficients:
##                              Estimate Std. Error t value         Pr(>|t|)    
## (Intercept)                 5.6283881  0.8003453   7.032 0.00000000001010 ***
## pop_density                 0.0153130  0.0043194   3.545         0.000443 ***
## log(job_density + 1)        0.1621346  0.0929488   1.744         0.081943 .  
## poverty_rate               -0.0242037  0.0060691  -3.988 0.00008054012807 ***
## walk_score                 -0.0002907  0.0044124  -0.066         0.947501    
## edu_ba_per                 -0.0090649  0.0046932  -1.931         0.054199 .  
## drive_per                  -0.0299649  0.0059306  -5.053 0.00000069117621 ***
## log(transit_per + 1)        0.8912960  0.1517205   5.875 0.00000000957102 ***
## log(ped_trips_pp + 1)       1.1082864  0.1546115   7.168 0.00000000000426 ***
## median_age_dumyoung         0.4293387  0.1528415   2.809         0.005237 ** 
## district_dumnon_central    -1.0171344  0.2793952  -3.640         0.000312 ***
## inde_station_dumnon_indego -0.8732709  0.1828079  -4.777 0.00000258455246 ***
## subway_dumsubway            0.4878457  0.1490228   3.274         0.001163 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.068 on 364 degrees of freedom
## Multiple R-squared:  0.7692,	Adjusted R-squared:  0.7616 
## F-statistic: 101.1 on 12 and 364 DF,  p-value: < 0.00000000000000022
```

```r
vif(lma)
```

```
##           pop_density  log(job_density + 1)          poverty_rate 
##              7.688482              3.575681              3.433975 
##         no_veh_hh_per            walk_score            edu_ba_per 
##              4.953816              3.085069              2.749108 
##       transit_score_d             drive_per  log(transit_per + 1) 
##              8.660553              4.147126              2.427559 
## log(ped_trips_pp + 1)        median_age_dum          district_dum 
##              3.298651              1.579916              2.470929 
##      inde_station_dum            subway_dum 
##              1.730379              1.520165
```

```r
vif(lmc)
```

```
##           pop_density  log(job_density + 1)          poverty_rate 
##              1.962728              2.842748              2.574483 
##            walk_score            edu_ba_per             drive_per 
##              2.949271              2.624652              3.278402 
##  log(transit_per + 1) log(ped_trips_pp + 1)        median_age_dum 
##              2.340364              2.748363              1.445154 
##          district_dum      inde_station_dum            subway_dum 
##              2.392967              1.706632              1.516794
```

```r
## step three: keep significant variables only (all significant at p = 0.001)
### delete walk_score
lmd <- lm(log(taxi_trip_2017) ~ pop_density + log(job_density + 1) + poverty_rate
          + edu_ba_per + drive_per + log(transit_per + 1) + log(ped_trips_pp + 1)
          + median_age_dum + district_dum + inde_station_dum + subway_dum,
          data = taxi_log_2017)
summary(lmd) # adjusted r-squared=0.7623
```

```
## 
## Call:
## lm(formula = log(taxi_trip_2017) ~ pop_density + log(job_density + 
##     1) + poverty_rate + edu_ba_per + drive_per + log(transit_per + 
##     1) + log(ped_trips_pp + 1) + median_age_dum + district_dum + 
##     inde_station_dum + subway_dum, data = taxi_log_2017)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -2.7702 -0.7097 -0.0532  0.7157  3.4314 
## 
## Coefficients:
##                             Estimate Std. Error t value         Pr(>|t|)    
## (Intercept)                 5.626162   0.798541   7.046 0.00000000000926 ***
## pop_density                 0.015179   0.003803   3.991 0.00007957252175 ***
## log(job_density + 1)        0.160699   0.090237   1.781         0.075768 .  
## poverty_rate               -0.024228   0.006050  -4.005 0.00007526234212 ***
## edu_ba_per                 -0.009047   0.004679  -1.934         0.053947 .  
## drive_per                  -0.029989   0.005911  -5.073 0.00000062355610 ***
## log(transit_per + 1)        0.887142   0.137811   6.437 0.00000000038287 ***
## log(ped_trips_pp + 1)       1.108420   0.154387   7.179 0.00000000000395 ***
## median_age_dumyoung         0.427262   0.149353   2.861         0.004469 ** 
## district_dumnon_central    -1.014669   0.276500  -3.670         0.000279 ***
## inde_station_dumnon_indego -0.872445   0.182129  -4.790 0.00000242642037 ***
## subway_dumsubway            0.487089   0.148377   3.283         0.001127 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.067 on 365 degrees of freedom
## Multiple R-squared:  0.7692,	Adjusted R-squared:  0.7623 
## F-statistic: 110.6 on 11 and 365 DF,  p-value: < 0.00000000000000022
```

```r
### delete log(job_density + 1)
lme <- lm(log(taxi_trip_2017) ~ pop_density + poverty_rate
          + edu_ba_per + drive_per + log(transit_per + 1) + log(ped_trips_pp + 1)
          + median_age_dum + district_dum + inde_station_dum + subway_dum,
          data = taxi_log_2017)
summary(lme) # adjusted r-squared=0.7609
```

```
## 
## Call:
## lm(formula = log(taxi_trip_2017) ~ pop_density + poverty_rate + 
##     edu_ba_per + drive_per + log(transit_per + 1) + log(ped_trips_pp + 
##     1) + median_age_dum + district_dum + inde_station_dum + subway_dum, 
##     data = taxi_log_2017)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -2.8039 -0.7262 -0.0467  0.7277  3.2049 
## 
## Coefficients:
##                             Estimate Std. Error t value             Pr(>|t|)
## (Intercept)                 5.717034   0.799269   7.153     0.00000000000466
## pop_density                 0.016031   0.003784   4.236     0.00002881672871
## poverty_rate               -0.022434   0.005983  -3.750             0.000206
## edu_ba_per                 -0.007282   0.004587  -1.588             0.113221
## drive_per                  -0.028467   0.005866  -4.853     0.00000180633789
## log(transit_per + 1)        0.870264   0.137892   6.311     0.00000000080082
## log(ped_trips_pp + 1)       1.240902   0.135682   9.146 < 0.0000000000000002
## median_age_dumyoung         0.429235   0.149791   2.866             0.004403
## district_dumnon_central    -1.119783   0.270926  -4.133     0.00004439319576
## inde_station_dumnon_indego -0.900683   0.181975  -4.949     0.00000113745065
## subway_dumsubway            0.557560   0.143426   3.887             0.000120
##                               
## (Intercept)                ***
## pop_density                ***
## poverty_rate               ***
## edu_ba_per                    
## drive_per                  ***
## log(transit_per + 1)       ***
## log(ped_trips_pp + 1)      ***
## median_age_dumyoung        ** 
## district_dumnon_central    ***
## inde_station_dumnon_indego ***
## subway_dumsubway           ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.07 on 366 degrees of freedom
## Multiple R-squared:  0.7672,	Adjusted R-squared:  0.7609 
## F-statistic: 120.6 on 10 and 366 DF,  p-value: < 0.00000000000000022
```

```r
### delete edu_ba_per
lmf <- lm(log(taxi_trip_2017) ~ pop_density + poverty_rate
          + drive_per + log(transit_per + 1) + log(ped_trips_pp + 1)
          + median_age_dum + district_dum + inde_station_dum + subway_dum,
          data = taxi_log_2017)
summary(lmf) # adjusted r-squared=0.7599
```

```
## 
## Call:
## lm(formula = log(taxi_trip_2017) ~ pop_density + poverty_rate + 
##     drive_per + log(transit_per + 1) + log(ped_trips_pp + 1) + 
##     median_age_dum + district_dum + inde_station_dum + subway_dum, 
##     data = taxi_log_2017)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -2.6870 -0.7124 -0.0279  0.7479  3.2775 
## 
## Coefficients:
##                             Estimate Std. Error t value             Pr(>|t|)
## (Intercept)                 5.128651   0.709628   7.227     0.00000000000288
## pop_density                 0.016240   0.003790   4.285     0.00002335365567
## poverty_rate               -0.017583   0.005155  -3.411             0.000719
## drive_per                  -0.027156   0.005820  -4.666     0.00000430937324
## log(transit_per + 1)        0.872180   0.138172   6.312     0.00000000079366
## log(ped_trips_pp + 1)       1.252411   0.135769   9.225 < 0.0000000000000002
## median_age_dumyoung         0.433986   0.150071   2.892             0.004058
## district_dumnon_central    -0.946605   0.248513  -3.809             0.000163
## inde_station_dumnon_indego -0.851092   0.179645  -4.738     0.00000309627488
## subway_dumsubway            0.548624   0.143612   3.820             0.000157
##                               
## (Intercept)                ***
## pop_density                ***
## poverty_rate               ***
## drive_per                  ***
## log(transit_per + 1)       ***
## log(ped_trips_pp + 1)      ***
## median_age_dumyoung        ** 
## district_dumnon_central    ***
## inde_station_dumnon_indego ***
## subway_dumsubway           ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.072 on 367 degrees of freedom
## Multiple R-squared:  0.7656,	Adjusted R-squared:  0.7599 
## F-statistic: 133.2 on 9 and 367 DF,  p-value: < 0.00000000000000022
```

```r
### delete median_age_dum
lmg <- lm(log(taxi_trip_2017) ~ pop_density + poverty_rate
          + drive_per + log(transit_per + 1) + log(ped_trips_pp + 1)
          + district_dum + inde_station_dum + subway_dum,
          data = taxi_log_2017)
summary(lmg) # adjusted r-squared=0.7551
```

```
## 
## Call:
## lm(formula = log(taxi_trip_2017) ~ pop_density + poverty_rate + 
##     drive_per + log(transit_per + 1) + log(ped_trips_pp + 1) + 
##     district_dum + inde_station_dum + subway_dum, data = taxi_log_2017)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -2.6206 -0.7788 -0.0014  0.8017  3.4375 
## 
## Coefficients:
##                             Estimate Std. Error t value             Pr(>|t|)
## (Intercept)                 5.434962   0.708663   7.669    0.000000000000156
## pop_density                 0.018720   0.003728   5.021    0.000000801925775
## poverty_rate               -0.013612   0.005018  -2.713             0.006986
## drive_per                  -0.029009   0.005842  -4.966    0.000001050499756
## log(transit_per + 1)        0.845417   0.139234   6.072    0.000000003150190
## log(ped_trips_pp + 1)       1.281239   0.136750   9.369 < 0.0000000000000002
## district_dumnon_central    -0.918892   0.250800  -3.664             0.000285
## inde_station_dumnon_indego -0.890637   0.180907  -4.923    0.000001287944801
## subway_dumsubway            0.538833   0.145001   3.716             0.000234
##                               
## (Intercept)                ***
## pop_density                ***
## poverty_rate               ** 
## drive_per                  ***
## log(transit_per + 1)       ***
## log(ped_trips_pp + 1)      ***
## district_dumnon_central    ***
## inde_station_dumnon_indego ***
## subway_dumsubway           ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.083 on 368 degrees of freedom
## Multiple R-squared:  0.7603,	Adjusted R-squared:  0.7551 
## F-statistic: 145.9 on 8 and 368 DF,  p-value: < 0.00000000000000022
```

```r
### delete poverty_rate
lmh <- lm(log(taxi_trip_2017) ~ pop_density + drive_per + log(transit_per + 1)
          + log(ped_trips_pp + 1) + district_dum + inde_station_dum + subway_dum,
          data = taxi_log_2017)
summary(lmh)  # adjusted r-squared=0.7509
```

```
## 
## Call:
## lm(formula = log(taxi_trip_2017) ~ pop_density + drive_per + 
##     log(transit_per + 1) + log(ped_trips_pp + 1) + district_dum + 
##     inde_station_dum + subway_dum, data = taxi_log_2017)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -2.7074 -0.7939 -0.0686  0.7682  3.7945 
## 
## Coefficients:
##                             Estimate Std. Error t value             Pr(>|t|)
## (Intercept)                 5.652972   0.710132   7.960   0.0000000000000214
## pop_density                 0.018705   0.003760   4.974   0.0000010055193335
## drive_per                  -0.026203   0.005799  -4.518   0.0000084010484808
## log(transit_per + 1)        0.692107   0.128341   5.393   0.0000001241839928
## log(ped_trips_pp + 1)       1.276492   0.137912   9.256 < 0.0000000000000002
## district_dumnon_central    -1.092942   0.244534  -4.469   0.0000104459706769
## inde_station_dumnon_indego -0.930874   0.181845  -5.119   0.0000004953266930
## subway_dumsubway            0.509711   0.145844   3.495             0.000532
##                               
## (Intercept)                ***
## pop_density                ***
## drive_per                  ***
## log(transit_per + 1)       ***
## log(ped_trips_pp + 1)      ***
## district_dumnon_central    ***
## inde_station_dumnon_indego ***
## subway_dumsubway           ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.092 on 369 degrees of freedom
## Multiple R-squared:  0.7555,	Adjusted R-squared:  0.7509 
## F-statistic: 162.9 on 7 and 369 DF,  p-value: < 0.00000000000000022
```

```r
## step four: add interaction variables
lmi <- lm(log(taxi_trip_2017) ~ pop_density + drive_per + log(transit_per + 1)
          + log(ped_trips_pp + 1) + district_dum + inde_station_dum + subway_dum
          + pop_density*subway_dum,
          data = taxi_log_2017)
summary(lmi)  # adjusted r-squared=0.7557
```

```
## 
## Call:
## lm(formula = log(taxi_trip_2017) ~ pop_density + drive_per + 
##     log(transit_per + 1) + log(ped_trips_pp + 1) + district_dum + 
##     inde_station_dum + subway_dum + pop_density * subway_dum, 
##     data = taxi_log_2017)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -2.7071 -0.7968 -0.0210  0.7628  3.2503 
## 
## Coefficients:
##                               Estimate Std. Error t value             Pr(>|t|)
## (Intercept)                   5.754290   0.704065   8.173   0.0000000000000049
## pop_density                   0.026268   0.004554   5.768   0.0000000169861143
## drive_per                    -0.026716   0.005745  -4.650   0.0000046326381281
## log(transit_per + 1)          0.600178   0.131020   4.581   0.0000063494429157
## log(ped_trips_pp + 1)         1.254912   0.136769   9.175 < 0.0000000000000002
## district_dumnon_central      -1.057646   0.242452  -4.362   0.0000167359957282
## inde_station_dumnon_indego   -0.939369   0.180091  -5.216   0.0000003057212523
## subway_dumsubway              1.241078   0.291759   4.254   0.0000266918036228
## pop_density:subway_dumsubway -0.020553   0.007124  -2.885              0.00414
##                                 
## (Intercept)                  ***
## pop_density                  ***
## drive_per                    ***
## log(transit_per + 1)         ***
## log(ped_trips_pp + 1)        ***
## district_dumnon_central      ***
## inde_station_dumnon_indego   ***
## subway_dumsubway             ***
## pop_density:subway_dumsubway ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.081 on 368 degrees of freedom
## Multiple R-squared:  0.7609,	Adjusted R-squared:  0.7557 
## F-statistic: 146.4 on 8 and 368 DF,  p-value: < 0.00000000000000022
```

```r
### model comparison
anova(lmh, lmi)
```

```
## Analysis of Variance Table
## 
## Model 1: log(taxi_trip_2017) ~ pop_density + drive_per + log(transit_per + 
##     1) + log(ped_trips_pp + 1) + district_dum + inde_station_dum + 
##     subway_dum
## Model 2: log(taxi_trip_2017) ~ pop_density + drive_per + log(transit_per + 
##     1) + log(ped_trips_pp + 1) + district_dum + inde_station_dum + 
##     subway_dum + pop_density * subway_dum
##   Res.Df    RSS Df Sum of Sq      F   Pr(>F)   
## 1    369 439.92                                
## 2    368 430.19  1    9.7296 8.3231 0.004145 **
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
stargazer(lmh, lmi, type = "text")
```

```
## 
## ==============================================================================
##                                             Dependent variable:               
##                              -------------------------------------------------
##                                             log(taxi_trip_2017)               
##                                        (1)                      (2)           
## ------------------------------------------------------------------------------
## pop_density                          0.019***                 0.026***        
##                                      (0.004)                  (0.005)         
##                                                                               
## drive_per                           -0.026***                -0.027***        
##                                      (0.006)                  (0.006)         
##                                                                               
## log(transit_per + 1)                 0.692***                 0.600***        
##                                      (0.128)                  (0.131)         
##                                                                               
## log(ped_trips_pp + 1)                1.276***                 1.255***        
##                                      (0.138)                  (0.137)         
##                                                                               
## district_dumnon_central             -1.093***                -1.058***        
##                                      (0.245)                  (0.242)         
##                                                                               
## inde_station_dumnon_indego          -0.931***                -0.939***        
##                                      (0.182)                  (0.180)         
##                                                                               
## subway_dumsubway                     0.510***                 1.241***        
##                                      (0.146)                  (0.292)         
##                                                                               
## pop_density:subway_dumsubway                                 -0.021***        
##                                                               (0.007)         
##                                                                               
## Constant                             5.653***                 5.754***        
##                                      (0.710)                  (0.704)         
##                                                                               
## ------------------------------------------------------------------------------
## Observations                           377                      377           
## R2                                    0.755                    0.761          
## Adjusted R2                           0.751                    0.756          
## Residual Std. Error              1.092 (df = 369)         1.081 (df = 368)    
## F Statistic                  162.886*** (df = 7; 369) 146.394*** (df = 8; 368)
## ==============================================================================
## Note:                                              *p<0.1; **p<0.05; ***p<0.01
```

```r
## step five:assumption
vif(lmi)
```

```
##            pop_density              drive_per   log(transit_per + 1) 
##               2.128701               3.001980               1.702944 
##  log(ped_trips_pp + 1)           district_dum       inde_station_dum 
##               2.098437               1.758260               1.616093 
##             subway_dum pop_density:subway_dum 
##               5.672844               6.448043
```

```r
plot(lmi)
```

![](Assignment-2_Viva-Yaohan_files/figure-html/task7-2-1.png)<!-- -->![](Assignment-2_Viva-Yaohan_files/figure-html/task7-2-2.png)<!-- -->![](Assignment-2_Viva-Yaohan_files/figure-html/task7-2-3.png)<!-- -->![](Assignment-2_Viva-Yaohan_files/figure-html/task7-2-4.png)<!-- -->

```r
## interpretation
(exp(0.026268)-1)*100
```

```
## [1] 2.661604
```

```r
(exp(-0.026716)-1)*100
```

```
## [1] -2.636228
```

```r
(exp(-1.057646)-1)*100
```

```
## [1] -65.27277
```

```r
(exp(-0.939369)-1)*100
```

```
## [1] -60.91256
```

```r
(exp(1.241078)-1)*100
```

```
## [1] 245.9341
```

```r
(exp(-0.020553)-1)*100
```

```
## [1] -2.034323
```

# Task 8


```r
lm_2017 <-  lm(taxi_trip_2017 ~ job_density + log(poverty_rate+1)
               + log(ped_trips_pp+1)*inde_station_dum, data = taxi_dat)
lm_2015 <-  lm(taxi_trip_2015 ~ job_density + log(poverty_rate+1)
               + log(ped_trips_pp+1)*inde_station_dum, data = taxi_dat)

stargazer(lm_2017, lm_2015, type = "text")
```

```
## 
## ===============================================================================
##                                                       Dependent variable:      
##                                                  ------------------------------
##                                                  taxi_trip_2017 taxi_trip_2015 
##                                                       (1)             (2)      
## -------------------------------------------------------------------------------
## job_density                                        125.020***     182.574***   
##                                                     (38.337)       (51.332)    
##                                                                                
## log(poverty_rate + 1)                            -8,190.545***  -12,454.140*** 
##                                                   (1,891.377)     (2,532.519)  
##                                                                                
## log(ped_trips_pp + 1)                            84,096.150***  129,930.200*** 
##                                                   (6,873.069)     (9,202.915)  
##                                                                                
## inde_station_dumnon_indego                       84,820.110***  124,058.000*** 
##                                                   (10,431.620)   (13,967.760)  
##                                                                                
## log(ped_trips_pp + 1):inde_station_dumnon_indego -71,975.720*** -110,493.400***
##                                                   (7,143.530)     (9,565.059)  
##                                                                                
## Constant                                         -68,547.510*** -100,176.900***
##                                                   (11,979.420)   (16,040.220)  
##                                                                                
## -------------------------------------------------------------------------------
## Observations                                          377             377      
## R2                                                   0.688           0.751     
## Adjusted R2                                          0.684           0.748     
## Residual Std. Error (df = 371)                     25,121.730     33,637.550   
## F Statistic (df = 5; 371)                          163.998***     224.001***   
## ===============================================================================
## Note:                                               *p<0.1; **p<0.05; ***p<0.01
```

```r
#interpretation
-8190*log(1+0.01)
```

```
## [1] -81.49321
```

```r
84096*log(1+0.01)
```

```
## [1] 836.783
```

```r
-71976*log(1+0.01)
```

```
## [1] -716.185
```

```r
-12454*log(1+0.01)
```

```
## [1] -123.9214
```

```r
129930*log(1+0.01)
```

```
## [1] 1292.846
```

```r
-110493*log(1+0.01)
```

```
## [1] -1099.442
```
