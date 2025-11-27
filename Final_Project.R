# install packages
library(tidyverse)
library(mgcv)
library(readr)
library(dplyr)

shark <- read.csv("/Users/siyona/STAT 380/Shark Tank US dataset.csv")
names(shark)
## Research Question: How likely are the contestants to get a deal based on the original equity offered to the sharks?
## Required Variables: Got Deal(Binary outcome variable (Yes/No or 1/0).), Original Offered Equity ( main predictor — this is what GAM models as a smooth function.)
## Optional Control: Variable	                            Why Add It?
              #  Original Ask Amount	                    Asking too much money may reduce deal probability.
              #  Valuation Requested	                    Sharks react differently to valuation ranges.
              #  Industry	                              Some industries get deals more often.
              #  Pitchers State	                          Might capture geography-based bias.
              #  Pitchers Gender	                        Might capture demographic bias.
              #  Multiple Entrepreneurs	Teams vs solo     pitches may receive deals differently.

#subsetting needed variables
shark_sub <- shark %>%
  select(Got.Deal,
         Original.Offered.Equity,
         Original.Ask.Amount,
         Valuation.Requested,
         Pitchers.Gender,
         Industry,
         Multiple.Entrepreneurs)

# Cleaning and converting types
shark_gam <- shark_sub %>%
  drop_na(Got.Deal, 
          Original.Offered.Equity, 
          Original.Ask.Amount,
          Valuation.Requested,
          Pitchers.Gender,
          Industry,
          Multiple.Entrepreneurs) %>%
  mutate(
    # Got.Deal is 0/1 in the file – turn into "No"/"Yes"
    Got.Deal = factor(Got.Deal, levels = c(0, 1),
                      labels = c("No", "Yes")),
    Pitchers.Gender = factor(Pitchers.Gender),
    Industry = factor(Industry),
    Multiple.Entrepreneurs = factor(Multiple.Entrepreneurs)
  )

glimpse(shark_gam)
nrow(shark_gam)

## Fitting GAM  
gam_equity <- gam(
  Got.Deal ~ s(Original.Offered.Equity) +
    Original.Ask.Amount +
    Valuation.Requested +
    Pitchers.Gender +
    Industry +
    Multiple.Entrepreneurs,
  data   = shark_gam,
  family = binomial(link = "logit")
)

summary(gam_equity)

## Plotting GAM
plot(gam_equity,
     select = 1,            # first smooth term (equity)
     shade = TRUE,
     seWithMean = TRUE,
     xlab = "Original Equity Offered (%)",
     ylab = "Effect on log-odds of Getting a Deal",
     main = "Smooth Effect of Equity on Deal (GAM)")

## Plotting predicted probability vs equity 

# grid of equity values across observed range
newdat <- tibble(
  Original.Offered.Equity = seq(
    min(shark_gam$Original.Offered.Equity),
    max(shark_gam$Original.Offered.Equity),
    length.out = 200
  ),
  # setting other predictors to typical values 
  Original.Ask.Amount   = median(shark_gam$Original.Ask.Amount),
  Valuation.Requested   = median(shark_gam$Valuation.Requested),
  Pitchers.Gender       = names(sort(table(shark_gam$Pitchers.Gender), decreasing = TRUE))[1],
  Industry              = names(sort(table(shark_gam$Industry), decreasing = TRUE))[1],
  Multiple.Entrepreneurs= names(sort(table(shark_gam$Multiple.Entrepreneurs), decreasing = TRUE))[1]
)

# predicted log-odds and SE
pred <- predict(gam_equity, newdata = newdat, type = "link", se.fit = TRUE)

newdat <- newdat %>%
  mutate(
    fit_link = pred$fit,
    se_link  = pred$se.fit,
    prob     = plogis(fit_link),
    prob_low = plogis(fit_link - 1.96 * se_link),
    prob_high= plogis(fit_link + 1.96 * se_link)
  )

## How likely are they to get a deal as equity changes? 
# grid of equity values across observed range
newdat <- tibble(
  Original.Offered.Equity = seq(
    min(shark_gam$Original.Offered.Equity),
    max(shark_gam$Original.Offered.Equity),
    length.out = 200
  ),
  # set other predictors to typical values; here:
  Original.Ask.Amount   = median(shark_gam$Original.Ask.Amount),
  Valuation.Requested   = median(shark_gam$Valuation.Requested),
  Pitchers.Gender       = names(sort(table(shark_gam$Pitchers.Gender), decreasing = TRUE))[1],
  Industry              = names(sort(table(shark_gam$Industry), decreasing = TRUE))[1],
  Multiple.Entrepreneurs= names(sort(table(shark_gam$Multiple.Entrepreneurs), decreasing = TRUE))[1]
)

# predicted log-odds and SE
pred <- predict(gam_equity, newdata = newdat, type = "link", se.fit = TRUE)

newdat <- newdat %>%
  mutate(
    fit_link = pred$fit,
    se_link  = pred$se.fit,
    prob     = plogis(fit_link),
    prob_low = plogis(fit_link - 1.96 * se_link),
    prob_high= plogis(fit_link + 1.96 * se_link)
  )

ggplot(newdat, aes(x = Original.Offered.Equity, y = prob)) +
  geom_ribbon(aes(ymin = prob_low, ymax = prob_high), alpha = 0.2) +
  geom_line(size = 1) +
  labs(
    x = "Original Equity Offered (%)",
    y = "Predicted Probability of Getting a Deal",
    title = "Predicted Deal Probability vs Equity Offered\n(GAM with Smooth for Equity)"
  )

