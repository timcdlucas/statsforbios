
library(dplyr)
library(magrittr)
library(ggplot2)
library(rpart)


# can download from http://esapubs.org/Archive/ecol/E090/184/PanTHERIA_1-0_WR05_Aug2008.txt
pantheria <- read.table(file = 'data/Chapter3/PanTHERIA_1-0_WR05_Aug2008.txt',
                        header = TRUE, sep = "\t", na.strings = c("-999", "-999.00"))


#####################################################
# Q: What causes Agre at first birth (exploratory)  #
#####################################################

# Use 90% of data for exploratory analysis
explore <- sample(1:nrow(pantheria), round(0.9 * nrow(pantheria)))
nrow(pantheria) - length(explore) # This leaves 500 data points for our hypothesis testing.

panExpl <- pantheria[explore, ]

ggplot(panExpl, aes(x = X3.1_AgeatFirstBirth_d)) + 
  geom_density() 

ggplot(panExpl, aes(x = X3.1_AgeatFirstBirth_d)) + 
  geom_density() +
  scale_x_log10()

# I don't know what causes age at first birth, so chuck a bunch in.
# Loads of missing data, so do a tree and pick greatest var importance for hypothesis testing.
Y <- log(panExpl$X3.1_AgeatFirstBirth_d)

# Remove Y, create a bunch of extra feature, add Y back in.
model <- panExpl %>%
          select(6:10, 13, 14, 18, 19, 21:33) %>%
          mutate_all(funs(sqrt, log)) %>% 
          mutate(X3.1_AgeatFirstBirth_d = Y) %>%
          rpart(X3.1_AgeatFirstBirth_d ~ ., data = ., control = list(minsplit = 2, cp = 0.001))

model$variable.importance # X23.1_SexualMaturityAge_d is most important... obviously...

#####################################################
# Q: Does Order cause littersize (test)             #
#####################################################

# Ignoring phylogeny... Old boss would not be happy. 

panHyp <- pantheria[-explore, ]

hypmodel <- lm(X3.1_AgeatFirstBirth_d ~ X23.1_SexualMaturityAge_d, panHyp)
anova(hypmodel) # Who woulda thought... it's significant.
                 


# Just as a comparison, what sort of power do we get if we don't do the exploratory bit first.
# There is in fact 0 rows with complete cases... 
# So we're going to have to reduce the variables we look at anyway... and still only get 26 data points.

model_poor <- pantheria %>%
                select(11, 28, 7, 31, 21, 32, 14, 9, 6) %>%
                #mutate_all(funs(sqrt, log)) %>% 
                #mutate(X3.1_AgeatFirstBirth_d = Y) %>%
                lm(X3.1_AgeatFirstBirth_d ~ ., data = .)
anova(model_poor)

# In this cases... we get exactly the same result.... oh well.

                 
                 
