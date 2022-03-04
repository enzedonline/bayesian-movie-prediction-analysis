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

# Drop unused columns, drop rows with NA's
movies2 <- movies2 %>%
    select(
        audience_score, feature_film, drama, runtime, mpaa_rating_R, 
        thtr_rel_year, oscar_season, summer_season, imdb_rating, imdb_num_votes, 
        critics_score, best_pic_nom, best_pic_win, best_actor_win, best_actress_win,
        best_dir_win, top200_box) %>% 
    drop_na()

movies2 %>% ggplot(aes(x=feature_film, y=audience_score)) + 
    geom_boxplot(fill="slateblue", alpha=0.2) + 
    xlab("Feature Film") +
    ylab("Audience Score")

ggplot(data = movies2, aes(x=feature_film, y=audience_score)) + 
    geom_boxplot(fill="lightblue") + 
    theme_excel_new() +
    xlab("Feature Film") +
    ylab("Audience Score") +
    ggtitle("Audience Score vs. Feature Film") +
    theme(
        plot.margin = margin(0, 0, 0.5, 0.5, "cm"),
        axis.title = element_text()
        ) 
    
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
            sd = round(sd(audience_score),2)
        ) %>%
        kbl() %>% 
        kable_styling(bootstrap_options = c("condensed"))
    )
}

bayes_inference(y = audience_score, x = feature_film, data = movies2, statistic = "mean", type = "ht",null=0, alternative = "twosided")

bi <- bayes_inference(y = audience_score, x = feature_film, data = movies2, 
            statistic = "mean", type = "ht", null=0, alternative = "twosided"
        )


plot(movies2$audience_score, movies2$imdb_num_votes)
crime.ZS =  bas.lm(y ~ ., data = UScrime,
                   prior = "ZS-null", modelprior = uniform())


m_movies <- bas.lm(audience_score ~ ., 
                          data = movies2,
                          prior = "ZS-null", 
                          method = "MCMC",
                          modelprior = uniform()
                        )
m_movies
diagnostics(m_movies, type="pip", col = "blue", pch = 16, cex = 1.5)
diagnostics(m_movies, type = "model", col = "blue", pch = 16, cex = 1.5)

image(m_movies, rotate=F)

#BICk <- log(nrow(movies2))
#modBIC <- MASS::stepAIC(m_movies, k = BICk)

movies.BPM = predict(m_movies, estimator = "BPM")
movies.BPM$best.vars

movies.BPM = predict(m_movies, estimator = "BPM", se.fit = TRUE)
movies.BPM.conf.fit = confint(movies.BPM, parm = "mean")
movies.BPM.conf.pred = confint(movies.BPM, parm = "pred")
head(cbind(movies.BPM$fit, movies.BPM.conf.fit, movies.BPM.conf.pred), 10)
# Extract a binary vector of zeros and ones for the variables included 
# in the BPM
BPM = as.vector(which.matrix(m_movies$which[movies.BPM$best],
                             m_movies$n.vars))
BPM
names(movies2)

movies.BPM = bas.lm(audience_score ~ ., data = movies2,
                      prior = "ZS-null",
                      modelprior = uniform(),
                      bestmodel = BPM, n.models = 1)
coef(movies.BPM)

movies.coef = coefficients(movies.BPM)

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
prediction$se.pred









