library(dplyr)
library(magrittr)
library(ggplot2)
library(lubridate)
library(Hmisc)
library(MatchIt)
library(lfe)
library(stargazer)
library(MESS)
library(data.table)

# laptop
setwd('G:/My Drive/MSBA/Spring 2019/MSBA 6440/MSBA 6440 Data/')
file_dir <- ''

# home desktop
# setwd('C:/Users/Donny/Google Drive/MSBA/Spring 2019/MSBA 6440/Data/')
file_dir <- 'C:/Users/Donny/Google Drive/MSBA/Spring 2019/MSBA 6440/MSBA 6440 Data/'

#ks_df1 <- read.csv(paste(file_dir, 'ks-projects-201612.csv', sep = ''))
ks_df2 <- read.csv('ks-projects-201801.csv',header=TRUE)

#colnames(ks_df1)
cols_keep <- c('ID', 'name', 'category', 'main_category', 
               'currency', 'deadline', 'goal', 'launched', 
               'pledged', 'state', 'backers', 'country', 'usd.pledged')

#ks_df1 %<>% select(cols_keep)
ks_df2 %<>% select(cols_keep)

# difference data types in corresponding columns
#str(ks_df1)
str(ks_df2)

# ID variable
#ks_df1$ID <- as.factor(ks_df1$ID)
ks_df2$ID <- as.factor(ks_df2$ID)

# name variable
#ks_df1$name <- as.character(ks_df1$name)
ks_df2$name <- as.character(ks_df2$name)

# category variable
#ks_df1$category <- as.factor(ks_df1$category)
ks_df2$category <- as.factor(ks_df2$category)

# maincategory variable
#ks_df1$main_category <- as.factor(ks_df1$main_category)
ks_df2$main_category <- as.factor(ks_df2$main_category)

# currency variable
#ks_df1$currency <- as.factor(ks_df1$currency)
ks_df2$currency <- as.factor(ks_df2$currency)



###########
## DATES ##
# NEED TO FIGURE OUT HOW TO DEAL WITH NON DATE VALUES
# 624 in total
# also need to convert 'Date' to 'POSIXct', or vice versa

#---------------------------------------------------
# POSIXct to Date works

# deadline variable
#ks_df1$deadline <- as.Date(ymd_hms(ks_df1$deadline))
ks_df2$deadline <- ymd(ks_df2$deadline)

# launched variable
#ks_df1$launched <- as.Date(ymd_hms(ks_df1$launched))
ks_df2$launched <- as.Date(ymd_hms(ks_df2$launched))
#---------------------------------------------------

# # deadline variable
# ks_df1$deadline <- ymd_hms(ks_df1$deadline)
# ks_df2$deadline <- ymd(ks_df2$deadline)
# 
# # launched variable
# ks_df1$launched <- ymd_hms(ks_df1$launched)
# ks_df2$launched <- ymd_hms(ks_df2$launched)
###########



# goal variable
#ks_df1$goal <- as.numeric(ks_df1$goal)
ks_df2$goal <- as.numeric(ks_df2$goal)

# pledged variable
#ks_df1$pledged <- as.numeric(ks_df1$pledged)
ks_df2$pledged <- as.numeric(ks_df2$pledged)

# pledged variable
#ks_df1$state <- as.factor(ks_df1$state)
ks_df2$state <- as.factor(ks_df2$state)

# backers variable
#ks_df1$backers <- as.numeric(ks_df1$backers)
ks_df2$backers <- as.numeric(ks_df2$backers)

# country variable
#ks_df1$country <- as.factor(ks_df1$country)
ks_df2$country <- as.factor(ks_df2$country)

# usd pledged variable
#ks_df1$usd.pledged <- as.numeric(ks_df1$usd.pledged)
ks_df2$usd.pledged <- as.numeric(ks_df2$usd.pledged)

#########################
## COMBINE DATA FRAMES ##
# DOES NOT WORK!
# NEED TO FIGURE OUT DATE FORMATS

# ks_df <- bind_rows('2016' = ks_df1,
#                    '2018' = ks_df2, 
#                    .id = 'year.data')

# TEMPORARY ASSIGNMENT
ks_df <- ks_df2

# clear memory
#ks_df1 <- NULL
ks_df2 <- NULL

# Check final structure
str(ks_df)
#########################



#########################
## FEATURE ENGINEERING ##

# if POSIXct not Date:
ks_df %<>%
  mutate(duration.days = as.numeric(deadline - as.Date(launched)))
# else:
ks_df %<>%
  mutate(duration.days = as.numeric(deadline - launched))

ks_df %<>%
  mutate(pledged.goal.ratio = pledged/goal)

# Check for nulls/NAs
sapply(ks_df, function(x) sum(is.na(x)))
# only in usd.pledged



##################
## Inspect Data ##
length(unique(ks_df$ID))
# Do we only care about success vs failure? what about cancelations?
describe(ks_df$state)
# what does each level mean?
# check each level in terms of pledged/goal ratio

# lots of skew, extremely high values in very skinny right tail
ggplot(data = ks_df, aes(x=pledged.goal.ratio, fill=state)) +
  geom_histogram(bins = 50, position='dodge')

# look at data related to extremely high pledged to goal ratios:
ks_df %>%
  arrange(desc(pledged.goal.ratio)) %>%
  head(25)

# there are several kick starter campaigns that have goals of $1 or some extremely low number
# these campaigns are easily successful, although some get cancelled even after the goal was met
# this further implies that some campaigns get cancelled even after exceeding goal
# so cancelled <> failed

# check for campaigns not successful or cancelled
ks_df %>%
  filter(!(state %in% c('successful', 'canceled'))) %>%
  arrange(desc(pledged.goal.ratio)) %>%
  head(25)

# live and suspended both include positive ratio 
# refine the filter further to exclude live and suspended campaigns
# also, undefined includes campaigns with ratios above 1
ks_df %>%
  filter(!(state %in% c(
    'successful','canceled',
    'live', 'suspended',
    'undefined'
  ))) %>%
  arrange(desc(pledged.goal.ratio)) %>%
  head(25)

# there are 6 failed campaigns with ratios above 1. remove these?
# can we strictly include failed only?  Include cancelled?
# do we only include 'successful' campaigns as postive outcomes?
##################


###############
## Tidying Data
ks_df_temp <- ks_df
ks_df <- ks_df_temp 
# only success or failed
# treatment: duration in days less than or equal to 30 days
ks_df %<>%
  select(-usd.pledged) %>%
  filter(state %in% c('successful', 'failed')) %>%
  mutate(
    launch.year = as.factor(year(launched)),
    launch.month = as.factor(month(launched)),
    treat = as.factor(ifelse(duration.days <= 30, 1, 0)),
    success = ifelse(state == 'successful', 1, 0)
  )

ggplot(data = ks_df, aes(x=pledged.goal.ratio, fill=state)) +
  geom_histogram(bins = 50, position='dodge')



# check balance of propensity scores
ks_df$PS <- glm(treat ~ goal + country, data= ks_df, family = "binomial")$fitted.values
ggplot(ks_df, aes(x = PS)) + 
  geom_histogram(color = "white") + 
  facet_wrap(~success) + 
  xlab("Pr(treat)") + theme_bw() + coord_flip() 

table.pre.match <- CreateTableOne(
  vars = c("goal", "country"), 
  strata = "treat", 
  test = TRUE,
  data = ks_df
)

print(table.pre.match, smd=TRUE)



###########################
# MATCHING AWAY CONFOUNDERS
t <- now()
t
m.out = matchit(
  treat ~ goal + country, 
  data = ks_df, 
  method = 'nearest', 
  distance = "logit", 
  caliper = 0.001, 
  replace = FALSE
)
now()
now() - t

# Stricter matching
t <- now()
t
m.out = matchit(
  treat ~ goal + country, 
  data = ks_df.matched, 
  method = 'nearest', 
  distance = "logit", 
  caliper = 0.0001, 
  replace = FALSE
)
now()
now() - t


###########################


# prune table
matched.ids <- data.table(match.data(m.out))$ID

# prune the original data
ks_df.matched <- ks_df[ks_df$ID %in% matched.ids,]



# checkpoint
write.csv(ks_df.matched, paste(file_dir, 'KickStarterMatched.csv', sep=''))

#load checkpoint
ks_df.matched <- read.csv(paste(file_dir, 'KickStarterMatched.csv', sep = ''))
ks_df.matched <- ks_df[ks_df$ID %in% ks_df.matched$ID,]



# checking balance after matching
ks_df.matched %<>%
  mutate(
    launch.year = as.factor(launch.year),
    launch.month = as.factor(launch.month),
    treat = as.factor(treat),
    success = as.numeric(success)
  )

ggplot(ks_df.matched, aes(x = PS)) + 
  geom_histogram(color = "white") + 
  facet_wrap(~treat) + 
  xlab("Pr(Success)") + theme_bw() + coord_flip() 

table.matched <- CreateTableOne(
  vars = c("goal", "country"), 
  strata = "treat", 
  test = TRUE,
  data = ks_df.matched
)

print(table.matched, smd=TRUE)

################
# FE Regressions
# REWRITE TO SHOW MATCHIT DISTRIBUTION
ggplot(data = ks_df.matched, aes(x=log(pledged.goal.ratio + 1), fill=state)) +
  geom_histogram(bins = 50, position='dodge')

ks_df.matched %>%
  select(goal, pledged, pledged.goal.ratio) %>%
  filter(goal == 1) %>%
  arrange((pledged.goal.ratio))

# regressions into star gazer
FE_success_treat <- felm(
  data = ks_df.matched, 
  success ~ treat | (main_category + launch.year + launch.month)
)

FE_success_duration <- felm(
  data = ks_df.matched, 
  success ~ duration.days | (main_category + launch.year + launch.month)
)

FE_PtoG_success <- felm(
  data = ks_df.matched, 
  log(pledged.goal.ratio + 1) ~ treat | (main_category + launch.year + launch.month)
)

FE_PtoG_duration <- felm(
  data = ks_df.matched, 
  log(pledged.goal.ratio + 1) ~ duration.days | (main_category + launch.year + launch.month)
)

stargazer(
  FE_success_treat,
  FE_success_duration,
  FE_PtoG_success,
  FE_PtoG_duration,
  title="Duration Effect on KS Success",
  type="text",
  column.labels=c(
    "Success on Treatment", "Success on Duration",
    "Pledged/Goal on Treatment", "Pledged/Goal on Duration"
  )
)

summary(FE_success_treat)
