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
editor_options: 
  markdown: 
    wrap: 72
---

# Setup


```r
# set defaults: cache chunks to speed compiling subsequent edits.
knitr::opts_chunk$set(cache=TRUE, echo = TRUE)
```

## Load packages


```r
library(ggplot2)
library(dplyr)
library(tidyr)
library(statsr)
library(BAS)
library(MASS)
library(ggthemes)
library(plotly)
library(kableExtra)
library(moments)
```

## Load data


```r
load("movies.Rdata")
```

------------------------------------------------------------------------

# Part 1: Data

## Type of Study

This is an observational study due to the nature of data collection. It only establishes associations and trends and cannot be used to infer causality.

## Generalizability & Potential Bias

No random assignment was used in the surveys and the sampling methodology is randomised. 

It can be considered to be generalisable to the entire population of movies that appear on Rotten Tomatoes and IMDB but with consideration that this does not represent the general population of movies.

Note also, that when considering audience scores, IMDB ratings and critics scores, these are representative of people that both use that site and actively contribute to ratings. It is not a representation of the population at large.


------------------------------------------------------------------------

# Part 2: Data manipulation


```r
# Add new variables
movies2 <- movies %>%
    mutate(feature_film = factor(title_type == 'Feature Film', labels=c("No", "Yes"))) %>%
    mutate(drama  = factor(genre == 'Drama', labels=c("No", "Yes"))) %>%
    mutate(mpaa_rating_R = factor(mpaa_rating == 'R', labels=c("No", "Yes"))) %>%
    mutate(oscar_season = factor(thtr_rel_month %in% 10:12, labels=c("No", "Yes"))) %>%
    mutate(summer_season = factor(thtr_rel_month %in% 5:8, labels=c("No", "Yes"))) 

# Print sample of rows with new variable
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
   <td style="text-align:left;"> Vampire Hunter D: Bloodlust </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> No </td>
  </tr>
  <tr>
   <td style="text-align:left;"> The Package </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> Yes </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Welcome to Mooseport </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> No </td>
  </tr>
  <tr>
   <td style="text-align:left;"> The Last Remake of Beau Geste </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> Yes </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Captain Phillips </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> No </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Django Unchained </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> No </td>
  </tr>
  <tr>
   <td style="text-align:left;"> The Sum of All Fears </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> Yes </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Blind Spot: Hitler's Secretary </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> No </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Bottle Rocket </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> No </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Love and Death </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:left;"> Yes </td>
  </tr>
</tbody>
</table>

```r
# Drop unused columns, drop rows with NA's
movies2 <- movies2 %>%
    select(
        audience_score, feature_film, drama, runtime, mpaa_rating_R, 
        thtr_rel_year, oscar_season, summer_season, imdb_rating, imdb_num_votes, 
        critics_score, best_pic_nom, best_pic_win, best_actor_win, best_actress_win,
        best_dir_win, top200_box) %>% 
    drop_na()
```

------------------------------------------------------------------------

# Part 3: Exploratory data analysis

Each of the new variables are boolean type, each to be examined against
Audience Score. Since analysis methodology is the same for each, we create
a function for summary stats and box plot.


```r
summ_var <- function(variable, description){
    summ_list = list(
        box = ggplot(data = movies2, aes(x=(!!as.name(variable)), y=audience_score)) + 
            geom_boxplot(fill="lightblue") + 
            theme_excel_new() +
            xlab(description) +
            ylab("Audience Score") +
            ggtitle(paste("Audience Score vs.", description)) +
            theme(
                plot.margin = margin(0, 0, 0.5, 0.5, "cm"),
                axis.title = element_text()
        ),
    
    tbl = movies2 %>% group_by((!!as.name(variable))) %>% 
        summarise(
            count = n(),
            min = min(audience_score), 
            q25 = quantile(audience_score, 0.25), 
            median = median(audience_score), 
            q75 = quantile(audience_score, 0.75), 
            max = max(audience_score),
            mean = round(mean(audience_score),2), 
            sd = round(sd(audience_score),2),
            skew = round(skewness(audience_score),2)
        ) %>%
        kbl() %>% 
        kable_styling(bootstrap_options = c("condensed"))
    )
}
```

------------------------------------------------------------------------

## Feature Film

------------------------------------------------------------------------


```r
feature_film <- summ_var("feature_film", "Feature Film")
```

::: columns
::: {.column width="38%"}

```r
feature_film$box
```

![](bayesian_project_files/figure-html/feature_filmbox-1.png)<!-- -->
:::

::: {.column width="2%"}
 
:::

::: {.column width="60%"}

```r
feature_film$tbl
```

<table class="table table-condensed" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> feature_film </th>
   <th style="text-align:right;"> count </th>
   <th style="text-align:right;"> min </th>
   <th style="text-align:right;"> q25 </th>
   <th style="text-align:right;"> median </th>
   <th style="text-align:right;"> q75 </th>
   <th style="text-align:right;"> max </th>
   <th style="text-align:right;"> mean </th>
   <th style="text-align:right;"> sd </th>
   <th style="text-align:right;"> skew </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> No </td>
   <td style="text-align:right;"> 59 </td>
   <td style="text-align:right;"> 19 </td>
   <td style="text-align:right;"> 77.0 </td>
   <td style="text-align:right;"> 86 </td>
   <td style="text-align:right;"> 89 </td>
   <td style="text-align:right;"> 96 </td>
   <td style="text-align:right;"> 81.20 </td>
   <td style="text-align:right;"> 13.64 </td>
   <td style="text-align:right;"> -3.07 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:right;"> 591 </td>
   <td style="text-align:right;"> 11 </td>
   <td style="text-align:right;"> 44.5 </td>
   <td style="text-align:right;"> 62 </td>
   <td style="text-align:right;"> 78 </td>
   <td style="text-align:right;"> 97 </td>
   <td style="text-align:right;"> 60.47 </td>
   <td style="text-align:right;"> 19.82 </td>
   <td style="text-align:right;"> -0.26 </td>
  </tr>
</tbody>
</table>

-   There is a much greater count of feature films reviewed
-   Both mean and median are much greater for non-feature films, the
    difference is greater than one feature-film SD
-   The range is similar for both
-   Variance is greater for feature films
-   Feature films are mostly normally distributed while non-feature
    films are heavily left-skewed.
:::
:::




```r
bi <- bayes_inference(y = audience_score, x = feature_film, data = movies2, 
            statistic = "mean", type = "ht", null=0, alternative = "twosided")
```

::: columns
::: {.column width="38%"}
![](bayesian_project_files/figure-html/feature_film_bi-1.png)<!-- -->
:::

::: {.column width="2%"}
 
:::

::: {.column width="60%"}
Hypotheses:<br> H1: mu_No = mu_Yes<br> H2: mu_No != mu_Yes<br>

Priors:<br> P(H1) = 0.5 P(H2) = 0.5 <br>

Results:<br> BF[H2:H1] = 322781721529
<br> P(H1\|data) = 0 <br>
P(H2\|data) = 1 <br>

Posterior summaries for under H2: <br> 95% Cred. Int.:
(15.1737, 25.7915)
:::
:::

The bayes inference output confirms that there is a significant
difference in audience score between feature film and non-feature film.

------------------------------------------------------------------------

## Drama

------------------------------------------------------------------------


```r
drama <- summ_var("drama", "Drama")
```

::: columns
::: {.column width="38%"}

```r
drama$box
```

![](bayesian_project_files/figure-html/dramabox-1.png)<!-- -->
:::

::: {.column width="2%"}
 
:::

::: {.column width="60%"}

```r
drama$tbl
```

<table class="table table-condensed" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> drama </th>
   <th style="text-align:right;"> count </th>
   <th style="text-align:right;"> min </th>
   <th style="text-align:right;"> q25 </th>
   <th style="text-align:right;"> median </th>
   <th style="text-align:right;"> q75 </th>
   <th style="text-align:right;"> max </th>
   <th style="text-align:right;"> mean </th>
   <th style="text-align:right;"> sd </th>
   <th style="text-align:right;"> skew </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> No </td>
   <td style="text-align:right;"> 345 </td>
   <td style="text-align:right;"> 11 </td>
   <td style="text-align:right;"> 41 </td>
   <td style="text-align:right;"> 61 </td>
   <td style="text-align:right;"> 79 </td>
   <td style="text-align:right;"> 97 </td>
   <td style="text-align:right;"> 59.70 </td>
   <td style="text-align:right;"> 21.30 </td>
   <td style="text-align:right;"> -0.11 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:right;"> 305 </td>
   <td style="text-align:right;"> 13 </td>
   <td style="text-align:right;"> 52 </td>
   <td style="text-align:right;"> 70 </td>
   <td style="text-align:right;"> 80 </td>
   <td style="text-align:right;"> 95 </td>
   <td style="text-align:right;"> 65.35 </td>
   <td style="text-align:right;"> 18.54 </td>
   <td style="text-align:right;"> -0.66 </td>
  </tr>
</tbody>
</table>

-   There is a more even number of drama vs non-drama, though non-drama
    is higher
-   Both mean and median are higher for drama films, the
    difference is approximately one half of the drama-film SD
-   The range is similar for both
-   Variance is slightly higher for non-drama films
-   Non-drama films are mostly normally distributed while drama films
    are mildly left-skewed.
:::
:::





```r
bi <- bayes_inference(y = audience_score, x = drama, data = movies2, 
            statistic = "mean", type = "ht", null=0, alternative = "twosided")
```

::: columns
::: {.column width="38%"}
![](bayesian_project_files/figure-html/drama_bi-1.png)<!-- -->
:::

::: {.column width="2%"}
 
:::

::: {.column width="60%"}
Hypotheses:<br> H1: mu_No = mu_Yes<br> H2: mu_No != mu_Yes<br>

Priors:<br> P(H1) = 0.5 P(H2) = 0.5 <br>

Results:<br> BF[H2:H1] = 34.0362
<br> P(H1\|data) = 0.0285 <br>
P(H2\|data) = 0.9715 <br>

Posterior summaries for under H2: <br> 95% Cred. Int.:
(-8.6372, -2.5226)
:::
:::


The bayes inference output confirms that there is a significant
difference in audience score between drama film and non-drama film with
no overlap of zero in the credible interval.

------------------------------------------------------------------------

## MPAA Rating R

------------------------------------------------------------------------


```r
mpaa_rating_R <- summ_var("mpaa_rating_R", "MPAA Rating R")
```

::: columns
::: {.column width="38%"}

```r
mpaa_rating_R$box
```

![](bayesian_project_files/figure-html/mpaa_rating_Rbox-1.png)<!-- -->
:::

::: {.column width="2%"}
 
:::

::: {.column width="60%"}

```r
mpaa_rating_R$tbl
```

<table class="table table-condensed" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> mpaa_rating_R </th>
   <th style="text-align:right;"> count </th>
   <th style="text-align:right;"> min </th>
   <th style="text-align:right;"> q25 </th>
   <th style="text-align:right;"> median </th>
   <th style="text-align:right;"> q75 </th>
   <th style="text-align:right;"> max </th>
   <th style="text-align:right;"> mean </th>
   <th style="text-align:right;"> sd </th>
   <th style="text-align:right;"> skew </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> No </td>
   <td style="text-align:right;"> 321 </td>
   <td style="text-align:right;"> 11 </td>
   <td style="text-align:right;"> 48 </td>
   <td style="text-align:right;"> 65 </td>
   <td style="text-align:right;"> 80 </td>
   <td style="text-align:right;"> 96 </td>
   <td style="text-align:right;"> 62.66 </td>
   <td style="text-align:right;"> 20.34 </td>
   <td style="text-align:right;"> -0.44 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:right;"> 329 </td>
   <td style="text-align:right;"> 14 </td>
   <td style="text-align:right;"> 44 </td>
   <td style="text-align:right;"> 64 </td>
   <td style="text-align:right;"> 79 </td>
   <td style="text-align:right;"> 97 </td>
   <td style="text-align:right;"> 62.04 </td>
   <td style="text-align:right;"> 20.16 </td>
   <td style="text-align:right;"> -0.29 </td>
  </tr>
</tbody>
</table>

-   There is an approximately even number of R vs non-R movies
-   Both mean and median are similar for both types of film
-   The range is similar for both
-   Variance is nearly identical
-   Both film types show a small left-skew.
:::
:::




```r
bi <- bayes_inference(y = audience_score, x = mpaa_rating_R, data = movies2, 
            statistic = "mean", type = "ht", null=0, alternative = "twosided")
```

::: columns
::: {.column width="38%"}
![](bayesian_project_files/figure-html/mpaa_rating_R_bi-1.png)<!-- -->
:::

::: {.column width="2%"}
 
:::

::: {.column width="60%"}
Hypotheses:<br> H1: mu_No = mu_Yes<br> H2: mu_No != mu_Yes<br>

Priors:<br> P(H1) = 0.5 P(H2) = 0.5 <br>

Results:<br> BF[H2:H1] = 14.9135
<br> P(H1\|data) = 0.9372 <br>
P(H2\|data) = 0.0628 <br>

Posterior summaries for under H2: <br> 95% Cred. Int.:
(-2.4516,  3.6660)
:::
:::

The bayes inference output confirms a strong overlap between the two
types of movies.

------------------------------------------------------------------------

## Oscar Season

------------------------------------------------------------------------


```r
oscar_season <- summ_var("oscar_season", "Oscar Season")
```

::: columns
::: {.column width="38%"}

```r
oscar_season$box
```

![](bayesian_project_files/figure-html/oscar_seasonbox-1.png)<!-- -->
:::

::: {.column width="2%"}
 
:::

::: {.column width="60%"}

```r
oscar_season$tbl
```

<table class="table table-condensed" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> oscar_season </th>
   <th style="text-align:right;"> count </th>
   <th style="text-align:right;"> min </th>
   <th style="text-align:right;"> q25 </th>
   <th style="text-align:right;"> median </th>
   <th style="text-align:right;"> q75 </th>
   <th style="text-align:right;"> max </th>
   <th style="text-align:right;"> mean </th>
   <th style="text-align:right;"> sd </th>
   <th style="text-align:right;"> skew </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> No </td>
   <td style="text-align:right;"> 460 </td>
   <td style="text-align:right;"> 11 </td>
   <td style="text-align:right;"> 46.00 </td>
   <td style="text-align:right;"> 64.0 </td>
   <td style="text-align:right;"> 79 </td>
   <td style="text-align:right;"> 96 </td>
   <td style="text-align:right;"> 61.81 </td>
   <td style="text-align:right;"> 20.12 </td>
   <td style="text-align:right;"> -0.34 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:right;"> 190 </td>
   <td style="text-align:right;"> 13 </td>
   <td style="text-align:right;"> 47.25 </td>
   <td style="text-align:right;"> 68.5 </td>
   <td style="text-align:right;"> 81 </td>
   <td style="text-align:right;"> 97 </td>
   <td style="text-align:right;"> 63.64 </td>
   <td style="text-align:right;"> 20.51 </td>
   <td style="text-align:right;"> -0.42 </td>
  </tr>
</tbody>
</table>

-   There is a much greater number of non-Oscar Season movies
-   Both mean and median are roughly similar for both types of film
-   The range is similar for both
-   Variance is nearly identical
-   Both film types show a small left-skew.
:::
:::




```r
bi <- bayes_inference(y = audience_score, x = oscar_season, data = movies2, 
            statistic = "mean", type = "ht", null=0, alternative = "twosided")
```

::: columns
::: {.column width="38%"}
![](bayesian_project_files/figure-html/oscar_season_bi-1.png)<!-- -->
:::

::: {.column width="2%"}
 
:::

::: {.column width="60%"}
Hypotheses:<br> H1: mu_No = mu_Yes<br> H2: mu_No != mu_Yes<br>

Priors:<br> P(H1) = 0.5 P(H2) = 0.5 <br>

Results:<br> BF[H2:H1] = 8.5148
<br> P(H1\|data) = 0.8949 <br>
P(H2\|data) = 0.1051 <br>

Posterior summaries for under H2: <br> 95% Cred. Int.:
(-5.2649,  1.5886)
:::
:::

The bayes inference output shows a higher rating for Oscar Season
movies, however there is also significant overlap between the two types.

------------------------------------------------------------------------

## Summer Season

------------------------------------------------------------------------


```r
summer_season <- summ_var("summer_season", "Summer Season")
```

::: columns
::: {.column width="38%"}

```r
summer_season$box
```

![](bayesian_project_files/figure-html/summer_seasonbox-1.png)<!-- -->
:::

::: {.column width="2%"}
 
:::

::: {.column width="60%"}

```r
summer_season$tbl
```

<table class="table table-condensed" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> summer_season </th>
   <th style="text-align:right;"> count </th>
   <th style="text-align:right;"> min </th>
   <th style="text-align:right;"> q25 </th>
   <th style="text-align:right;"> median </th>
   <th style="text-align:right;"> q75 </th>
   <th style="text-align:right;"> max </th>
   <th style="text-align:right;"> mean </th>
   <th style="text-align:right;"> sd </th>
   <th style="text-align:right;"> skew </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> No </td>
   <td style="text-align:right;"> 442 </td>
   <td style="text-align:right;"> 13 </td>
   <td style="text-align:right;"> 46.00 </td>
   <td style="text-align:right;"> 65.5 </td>
   <td style="text-align:right;"> 80 </td>
   <td style="text-align:right;"> 97 </td>
   <td style="text-align:right;"> 62.60 </td>
   <td style="text-align:right;"> 20.40 </td>
   <td style="text-align:right;"> -0.36 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:right;"> 208 </td>
   <td style="text-align:right;"> 11 </td>
   <td style="text-align:right;"> 44.75 </td>
   <td style="text-align:right;"> 65.0 </td>
   <td style="text-align:right;"> 78 </td>
   <td style="text-align:right;"> 94 </td>
   <td style="text-align:right;"> 61.81 </td>
   <td style="text-align:right;"> 19.91 </td>
   <td style="text-align:right;"> -0.38 </td>
  </tr>
</tbody>
</table>

-   There is a much greater number of non-Summer Season movies
    (approximately 2:1)
-   Both mean and median are roughly similar for both types of film
-   The range is similar for both
-   Variance is very similar
-   Both types of film have a small left-skew.
:::
:::




```r
bi <- bayes_inference(y = audience_score, x = summer_season, data = movies2, 
            statistic = "mean", type = "ht", null=0, alternative = "twosided")
```

::: columns
::: {.column width="38%"}
![](bayesian_project_files/figure-html/summer_season_bi-1.png)<!-- -->
:::

::: {.column width="2%"}
 
:::

::: {.column width="60%"}
Hypotheses:<br> H1: mu_No = mu_Yes<br> H2: mu_No != mu_Yes<br>

Priors:<br> P(H1) = 0.5 P(H2) = 0.5 <br>

Results:<br> BF[H2:H1] = 13.4802
<br> P(H1\|data) = 0.9309 <br>
P(H2\|data) = 0.0691 <br>

Posterior summaries for under H2: <br> 95% Cred. Int.:
(-2.5584,  4.1228)
:::
:::

The bayes inference output shows a slightly higher rating for Summer
Season movies, however there is also significant overlap between the two
types.

---

## Number of votes on IMDB

---

This variable has an extreme right skew distribution over a large range.

::: {.columns}
::: {.column width="50%"}


```r
hist(movies2$imdb_num_votes)
```

![](bayesian_project_files/figure-html/imdb_num_hist-1.png)<!-- -->


:::
::: {.column width="50%"}


```r
ggplot(data = movies2, aes(x=audience_score, y=imdb_num_votes)) + geom_point()
```

![](bayesian_project_files/figure-html/imdb_num_plot-1.png)<!-- -->

:::
:::

Taking the log of this variable normalises the distribution and the relationship with audience score.

::: {.columns}
::: {.column width="50%"}


```r
hist(log(movies2$imdb_num_votes))
```

![](bayesian_project_files/figure-html/imdb_num_log_hist-1.png)<!-- -->


:::
::: {.column width="50%"}


```r
ggplot(data = movies2, aes(x=audience_score, y=log(imdb_num_votes))) + geom_point()
```

![](bayesian_project_files/figure-html/imdb_num_log_plot-1.png)<!-- -->

:::
:::

For modelling, we will use the log of Number of votes on IMDB:


```r
movies2 <- movies2 %>%
    mutate(l_imdb_num_votes = log(imdb_num_votes)) %>%
    select(-imdb_num_votes)
```

------------------------------------------------------------------------

# Part 4: Modelling

## Base Model

We create an initial model using the `bas.lm` function from the `BAS` package. 

Here we have 16 potential predictors. The total number of models is $2^{16}=65536$. 

This can take a long time to process, so we can use the argument `method = MCMC` inside the `bas.lm` function to speed processing by only sampling most likely models. 

We also use the Zellner-Siow cauchy prior (`prior = "ZS-null"`) for the prior distributions of the coefficients in this regression.

We assume all variables have an equal likelihood of being included in the final model using the uniform distribution `modelprior = uniform()`.


```r
m_movies <- bas.lm(audience_score ~ ., 
                          data = movies2,
                          prior = "ZS-null", 
                          method = "MCMC",
                          modelprior = uniform()
                        )
m_movies
```

```
## 
## Call:
## bas.lm(formula = audience_score ~ ., data = movies2, prior = "ZS-null", 
##     modelprior = uniform(), method = "MCMC")
## 
## 
##  Marginal Posterior Inclusion Probabilities: 
##           Intercept      feature_filmYes             dramaYes  
##             1.00000              0.08251              0.04699  
##             runtime     mpaa_rating_RYes        thtr_rel_year  
##             0.47373              0.20470              0.10808  
##     oscar_seasonYes     summer_seasonYes          imdb_rating  
##             0.07846              0.08209              0.99999  
##       critics_score      best_pic_nomyes      best_pic_winyes  
##             0.88311              0.13804              0.04130  
##   best_actor_winyes  best_actress_winyes      best_dir_winyes  
##             0.14701              0.14342              0.06986  
##       top200_boxyes     l_imdb_num_votes  
##             0.05009              0.10413
```


## Model Diagnostics

### Posterior inclusion probabilities

We will run the convergence plots to double-check the base model first:

::: {.columns}
::: {.column width="50%"}


```r
diagnostics(m_movies, type="pip", col = "blue", pch = 16, cex = 1.5)
```

![](bayesian_project_files/figure-html/converge_pip-1.png)<!-- -->

:::
::: {.column width="50%"}


```r
diagnostics(m_movies, type="model", col = "blue", pch = 16, cex = 1.5)
```

![](bayesian_project_files/figure-html/converge_model-1.png)<!-- -->

:::
:::

The posterior inclusion & model probabilities converge well to the theoretical posterior inclusion probability.

### Residuals Versus Fitted Values. 

::: {.columns}
::: {.column width="50%"}


```r
plot(m_movies, which = 1, add.smooth = F, 
     ask = F, pch = 16, sub.caption="", caption="")
abline(a = 0, b = 0, col = "darkgrey", lwd = 2)
```

![](bayesian_project_files/figure-html/diag_residuals-1.png)<!-- -->

:::
::: {.column width="50%"}
\
\
\
\
\
\
\
Since the Best Predictive Model is the model whose predictions are closest to those given by BMA, we use BMA to check the residuals.

For predictions over 25, there is a fairly even distribution of residuals, however this becomes skewed to the positive below that. Potential outliers are cases 126, 216 & 251.

:::
:::

### Cumulative Sampled Probability

::: {.columns}
::: {.column width="50%"}


```r
plot(m_movies, which=2, add.smooth = F, sub.caption="", caption="")
```

![](bayesian_project_files/figure-html/diag_csp-1.png)<!-- -->

:::
::: {.column width="50%"}

\
\
\
\
\
\
We can see that after we have discovered about 7,000 unique models with MCMC sampling, the probability is starting to level off after less than 100, indicating that these additional models have very small probability and do not contribute substantially to the posterior distribution. 

These probabilities are proportional to the product of marginal likelihoods of models and priors, $p(data | M_m)p(M_m)$, rather than Monte Carlo frequencies.


:::
:::

### Model Complexity

::: {.columns}
::: {.column width="50%"}


```r
plot(m_movies, which=3, ask=F, caption="", sub.caption="")
```

![](bayesian_project_files/figure-html/diag_complexity-1.png)<!-- -->

:::
::: {.column width="50%"}

\
\
\
\
\
\
The models with 2 & 3 variables have both the lowest and highest BF, throwing the graph scale out. 

The model selection methodology will remove these low values.

:::
:::

### Marginal Inclusion Probability

::: {.columns}
::: {.column width="50%"}


```r
plot(m_movies, which = 4, ask = F, caption = "", sub.caption = "", 
     col.in = "blue", col.ex = "darkgrey", lwd = 3)
```

![](bayesian_project_files/figure-html/diag_mip-1.png)<!-- -->

:::
::: {.column width="50%"}

\
\
\
\
\
\
\
Only two variables have marginal inclusion probabilities greater than 0.5: `imdb_rating` and `critics_score`. 

Additionally, `runtime` has a value close to 0.5 and may be considered for the model.

It should be kept in mind that low scores may be a result of multicollinearity.

:::
:::


## Model Selection - Best Predictive Model

Since the purpose of the model is to predict future audience movie scores, the approach used will be Best Predictive Model (BPM).


```r
movies.BPM = predict(m_movies, estimator = "BPM")
movies.BPM$best.vars
```

```
## [1] "Intercept"     "runtime"       "imdb_rating"   "critics_score"
```

BPM estimation with the `predict` function tells us that only three significant variables exist for creating the best predictive model: `runtime`, `imdb_rating`, `critics_score`

Using the `se.fit = TRUE` option with `predict` we can calculate standard deviations for the predictions or for the mean. Then we can use this as input for the `confint` function for the prediction object. Here we only show the results of the first 10 data points.


```r
movies.BPM = predict(m_movies, estimator = "BPM", se.fit = TRUE)
movies.BPM.conf.fit = confint(movies.BPM, parm = "mean")
movies.BPM.conf.pred = confint(movies.BPM, parm = "pred")
head(cbind(movies.BPM$fit, movies.BPM.conf.fit, movies.BPM.conf.pred), 10)
```

```
##                    2.5%    97.5%     mean     2.5%     97.5%     pred
##  [1,] 48.00963 46.62389 49.39537 48.00963 28.24466  67.77460 48.00963
##  [2,] 77.39261 76.01322 78.77200 77.39261 57.62808  97.15713 77.39261
##  [3,] 82.43884 80.81128 84.06640 82.43884 62.65544 102.22223 82.43884
##  [4,] 72.73853 71.14737 74.32968 72.73853 52.95809  92.51896 72.73853
##  [5,] 40.64808 39.32988 41.96629 40.64808 20.88773  60.40843 40.64808
##  [6,] 85.75164 83.87994 87.62335 85.75164 65.94667 105.55662 85.75164
##  [7,] 70.96218 69.23199 72.69236 70.96218 51.17007  90.75428 70.96218
##  [8,] 45.34669 43.92780 46.76559 45.34669 25.57937  65.11402 45.34669
##  [9,] 80.65865 79.17317 82.14413 80.65865 60.88643 100.43086 80.65865
## [10,] 65.04514 63.66929 66.42099 65.04514 45.28086  84.80942 65.04514
```

The option `estimator = "BPM"` is not yet available in `coef()`, so we need to extract a vector of zeros and ones representing which variables are included in the BPM model.


```r
BPM = as.vector(which.matrix(m_movies$which[movies.BPM$best],
                             m_movies$n.vars))
BPM
```

```
##  [1] 1 0 0 1 0 0 0 0 1 1 0 0 0 0 0 0 0
```

Next, we will refit the model with `bas.lm` using the optional argument `bestmodel = BPM`. 

We will also specify that want to have 1 model by setting `n.models = 1`. In this way, `bas.lm` starts with the BPM and fits only that model.


```r
movies.BPM = bas.lm(audience_score ~ ., data = movies2,
                      prior = "ZS-null",
                      modelprior = uniform(),
                      bestmodel = BPM, n.models = 1)
```

Now, since we have only one model in our new object representing the BPM, we can use the `coef` function to obtain the summaries.


```r
movies.BPM.coef <- coef(movies.BPM)
movies.BPM.coef
```

```
## 
##  Marginal Posterior Summaries of Coefficients: 
## 
##  Using  BMA 
## 
##  Based on the top  1 models 
##                      post mean  post SD   post p(B != 0)
## Intercept            62.34769    0.39383   1.00000      
## feature_filmYes       0.00000    0.00000   0.00000      
## dramaYes              0.00000    0.00000   0.00000      
## runtime              -0.05353    0.02105   1.00000      
## mpaa_rating_RYes      0.00000    0.00000   0.00000      
## thtr_rel_year         0.00000    0.00000   0.00000      
## oscar_seasonYes       0.00000    0.00000   0.00000      
## summer_seasonYes      0.00000    0.00000   0.00000      
## imdb_rating          14.95802    0.57691   1.00000      
## critics_score         0.07025    0.02154   1.00000      
## best_pic_nomyes       0.00000    0.00000   0.00000      
## best_pic_winyes       0.00000    0.00000   0.00000      
## best_actor_winyes     0.00000    0.00000   0.00000      
## best_actress_winyes   0.00000    0.00000   0.00000      
## best_dir_winyes       0.00000    0.00000   0.00000      
## top200_boxyes         0.00000    0.00000   0.00000      
## l_imdb_num_votes      0.00000    0.00000   0.00000
```

```r
par(mfrow = c(1, 3))
plot(movies.BPM.coef, subset = c(4,10,9))
```

![](bayesian_project_files/figure-html/bpm_coef-1.png)<!-- -->

Here, we see the model predicts a small negative correlation with runtime and audience score, a small positive correlation with critics score, while the greatest factor in prediction is the IMDB rating.

From the above table, the approximate predictions at the 95% credible interval are:

- for each minute of movie length, we would expect a **decrease** in audience score of between 0.095 and 0.013
- for each point on critic's score, we would expect the audience score to increase by between 0.027 and 0.113
- for each point on IMDB rating, we would expect to see an audience score increase by between 13.83 and 16.09

------------------------------------------------------------------------

# Part 5: Prediction

For prediction, we will use the movie [Arrival](https://www.rottentomatoes.com/m/arrival_2016) with a Rotten Tomatoes audience score of 82%.

For this, we will need the critics score (94%), runtime (116 minutes) and IMDB rating (7.9).

We start by creating a dataframe with the necessary movie properties then set up a new model using the g-prior and our previously selected variables. 

Finally, this is passed to the `predict` function, the prediction is in the `fit` element of the returned list object while the standard error is found in `se.pred`.


```r
movie_props <- data.frame(
    imdb_rating = 7.9, 
    critics_score = 94, 
    runtime = 116
    )

movies.g_prior = bas.lm(audience_score ~ imdb_rating + critics_score + runtime, 
                       data = movies2,
                       alpha=3, 
                       prior="g-prior"
                       )

prediction <- predict(movies.g_prior, newdata=movie_props, estimator="BPM", se.fit=T)
as.numeric(prediction$fit)
```

```
## [1] 79.68117
```

```r
as.numeric(prediction$se.pred)
```

```
## [1] 13.35252
```

The model predicts an audience score of 80%. 

More specifically, at the 95% credible interval, the model predicts that a movie with these attributes will have an audience score of 80 $\pm$ 26.2%.

 

------------------------------------------------------------------------

# Part 6: Conclusion

We have created a Bayesian Multilinear Regression model using the Zellner-Siow prior with the Markov Chain Monte Carlo method and a uniform model prior.

The fitted model was selected using the Best Predictive approach and employed just 3 of the original 16 variables: runtime, IMDB rating, critic's score to predict a Rotten Tomatoes audience rating.

While the predicted value (80%) is very close to the actual value (82%), the large standard error makes this somewhat meaningless. To reduce the standard error, we would need a much larger sample size than 650 observations (since the precision g is proportional to the sample size).

On a final note, using IMDB rating and critic's score to predict an audience rating doesn't have much value as the movie needs to be already released and rated by critics and members of the public. A model as a predictive tool for unreleased movies would need to take these two variables out of consideration when constructing the model.
\
\
\
\

----

\
\
\
\


