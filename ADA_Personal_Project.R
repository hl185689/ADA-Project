library(ggplot2)

uobs <- readr::read_tsv("/Users/hannahleonard/Documents/UM/2019_Fall/ADA/AA_Project/Data/landtrusts_userobj_regions.txt")
tweets <- readr::read_tsv("/Users/hannahleonard/Documents/UM/2019_Fall/ADA/AA_Project/Data/all_clean_tweets.txt")


# descriptives
twtorg <- data.frame(table(tweets$organization))
summary(twtorg$Freq) 
sd(twtorg$Freq)

hist(twtorg$Freq)

ggplot(data = twtorg, aes(x = Freq)) + geom_histogram(bins = 10, color = 'black', fill = 'lightblue')+
  labs(x = "Number of Tweets", y = "Organization Count", 
       title = "Historgram: Tweets by Organization")+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  theme_classic() + 
  theme(axis.text = element_text(color = "black"))

# creating data frame to count tweets by organization
df <- data.frame(table(tweets$organization))
head(df)


# renaming columns
names(df) <- c("screen_name", "count")
head(df)

# define regions
rdf <- data.frame(state = unique(uobs$state),
           region = c("east","east","east","west","east","west","west",
                      "east","east","west", "east", "east", "east", "east",
                      "east", "east", "east","east","west", "west", "east",
                      "west", "west", "east", "west", "east", "east","west",
                      "east", "east","east","east","east","west", "east",
                      "west", "east","west","west","west", "west",
                      "east","west","west","west"))

# merging data
step1 <- merge(df,uobs[,c("screen_name","state","importance")], by="screen_name")

final <- merge(step1,rdf, by="state")              
              
table(duplicated(final)) #no duplicates


# ANOVA 
m1 <- aov(count ~ region + importance, data=final)

summary(m1)
coefficients(m1)

# predictions
new.df <- expand.grid(region = unique(final$region),
                     importance = c(0,1))

predict(m1, newdata = new.df, se.fit = T)

pred.df <- cbind(new.df,
                 mean = predict(m1, newdata = new.df, se.fit = T)$fit,
                 se = predict(m1, newdata = new.df, se.fit = T)$se.fit)

pred.df$se.hi <- pred.df$mean + pred.df$se
pred.df$se.lo <- pred.df$mean - pred.df$se
pred.df$se.lo <- ifelse(pred.df$se.lo < 0, 0, pred.df$se.lo) # no negative tweet numbers
pred.df$region <- ifelse(pred.df$region == "east","East","West")

#quartz()

ggplot(data = pred.df, aes(x = region, y = mean)) +
  geom_errorbar(aes(ymin = se.lo, ymax = se.hi), lwd =0.3, width = 0.25)+
  geom_point(aes(fill = as.factor(importance)), size = 2, shape = 21) +
  labs(x = "Region", y = "Tweet Count", fill = "Importance")+
  scale_fill_manual(values = c("#619cff","coral"))+
  scale_y_continuous(limits =c(500,3500),breaks = seq(500,3500,by=500))+
  theme_classic() + 
  theme(axis.text = element_text(color = "black"),
        legend.position = "bottom")


