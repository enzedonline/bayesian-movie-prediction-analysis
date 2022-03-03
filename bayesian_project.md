---
title: "Bayesian Statistics: Movie Popularity Data Analysis"
output: 
  html_document: 
    fig_height: 4
    fig_width: 9
    highlight: pygments
    theme: yeti
    toc: yes
    keep_md: yes
---
## Setup

```r
# set defaults: cache chunks to speed compiling subsequent edits.
knitr::opts_chunk$set(cache=TRUE, echo = TRUE)
```

### Load packages


```r
library(ggplot2)
library(dplyr)
library(statsr)
library(BAS)
library(ggthemes)
library(plotly)
library(kableExtra)
```

### Load data



```r
load("movies.Rdata")
```



* * *

## Part 1: Data


* * *

## Part 2: Data manipulation


```r
movies2 <- movies %>%
    mutate(feature_film = factor(title_type == 'Feature Film', labels=c("No", "Yes"))) %>%
    mutate(drama  = factor(genre == 'Drama', labels=c("No", "Yes"))) %>%
    mutate(mpaa_rating_R = factor(mpaa_rating == 'R', labels=c("No", "Yes"))) %>%
    mutate(oscar_season = factor(thtr_rel_month %in% 10:12, labels=c("No", "Yes"))) %>%
    mutate(summer_season = factor(thtr_rel_month %in% 5:8, labels=c("No", "Yes"))) 
sample_n(movies2, 10) %>%
    select(title, feature_film:summer_season) %>%
    kbl() %>% 
    kable_styling(bootstrap_options = c("condensed"))
```

<table class="table table-condensed" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> title </th>
   <th style="text-align:left;"> feature_film </th>
   <th style="text-align:left;"> drama </th>
   <th style="text-align:left;"> mpaa_rating_R </th>
   <th style="text-align:left;"> oscar_season </th>
   <th style="text-align:left;"> summer_season </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Young Adult </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> No </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Thieves Like Us </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> No </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Beverly Hills Cop </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> No </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Conviction </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> No </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Alvin and the Chipmunks: Chipwrecked </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> No </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Heaven &amp; Earth </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> Yes </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Revenge of the Nerds II: Nerds in Paradise </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> Yes </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Jonestown: The Life and Death of Peoples Temple </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> No </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Date Night </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> No </td>
  </tr>
  <tr>
   <td style="text-align:left;"> A Boy and His Dog </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> No </td>
  </tr>
</tbody>
</table>


* * *

## Part 3: Exploratory data analysis



* * *

## Part 4: Modeling



* * *

## Part 5: Prediction



* * *

## Part 6: Conclusion

