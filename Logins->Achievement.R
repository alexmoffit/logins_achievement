# Time zone correction
  full.logins.achieve$when <- ymd_hms(full.logins.achieve$when, tz = "America/Los_Angeles")
  full.goals.achieve$start <- ymd(full.goals.achieve$start, tz = "America/Los_Angeles")
  full.goals.achieve$end <- ymd(full.goals.achieve$end, tz = "America/Los_Angeles")

# selecting organizations that signed contracts prior to Q4
org.logins.achieve <- full.logins.achieve %>%
  filter(org_id == 2 | 
           org_id == 8 |
           org_id == 15 |
           org_id == 12 |
           org_id == 20 |
           org_id == 21 |
           org_id == 23 |
           org_id == 31 |
           org_id == 29 |
           org_id == 26 |
           org_id == 40 |
           org_id == 47 |
           org_id == 37 |
           org_id == 38 |
           org_id == 36 |
           org_id == 39 |
           org_id == 49 |
           org_id == 33 |
           org_id == 42 |
           org_id == 44 |
           org_id == 45 |
           org_id == 43 |
           org_id == 46
  )
org.logins.achieve <- org.logins.achieve %>%
  arrange(desc(when))

# Q4 logins
Q4.logins.achieve <- org.logins.achieve %>%
  filter(when < "2015-01-01 00:00:00") %>%
  filter(when > "2014-09-30 23:59:59")

Q4.no.z.logins <- Q4.logins.achieve %>%
  filter(org_id != 20)
  
# filtering for logins after Q4 2014
active.logins.achieve <- org.logins.achieve %>%
  filter(when > "2014-12-31 23:59:59") %>%
  group_by(user_id) %>%
  arrange(user_id)

# Q4 login data
Q4.logins.experiment <- org.logins.achieve %>%
  filter(when < "2015-01-01 00:00:00") %>%
  filter(when > "2014-09-30 23:59:59") %>%
  group_by(user_id) %>%
  arrange(user_id)

# Creating a vector of unique users who have logged in after Q4 2014
active.users.achieve <- unique(active.logins.achieve$user_id)
Q4.users.experiment <- unique(Q4.logins.experiment$user_id)

# Q4 login data for those users
login.subset.experiment <- subset(Q4.logins.achieve, user_id %in% active.users.achieve)
# login.subset.users <- unique(login.subset.experiment$user_id)
no.z.login.subset.experiment <- subset(Q4.no.z.logins, user_id %in% active.users.achieve)

# Number of users who have logged in in Q4 & Q1
login.subset.users <- login.subset.experiment %>%
  select(user_id, when) %>%
  group_by(user_id) %>%
  tally(sort=TRUE)

# Bucketing users into login buckets: less than monthly (n < 3), monthly (3=< n < 12), and weekly (12 < n)
less.than.monthly <- login.subset.users %>%
  filter(n < 3)
less.than.monthly.users <- less.than.monthly$user_id

monthly.login <- login.subset.users %>%
  filter(n >= 3) %>%
  filter(n < 12)
monthly.login.users <- monthly.login$user_id

weekly.login <- login.subset.users %>%
  filter(n >= 12)
weekly.login.users <- weekly.login$user_id

# Cleaning up the goal information
# Getting rid of deleted and "Getting Started" goals
no.delete.or.started.achieve <- full.goals.achieve %>%
  filter(deleted != "t") %>%
  filter(name != "Getting Started With BetterWorks")

# Getting Q4 goals only
Q4.goals.achieve <- no.delete.or.started.achieve %>%
  filter(end > "2014-09-30" & end < "2015-01-01")

# Getting Q4 goals from users who have logged in in Q1
subset.Q4.goals.achieve <- subset(Q4.goals.achieve, owner_id %in% active.users.achieve)
# Getting Q4 goals from users who have logged in in Q4 and Q1
subset2.Q4.goals.achieve <- subset(Q4.goals.achieve, owner_id %in% login.subset.users$user_id)

# subset.Q4.goals.achieve %>%
  # summarise(number.user = n_distinct(owner_id))
# subset2.Q4.goals.achieve %>%
  # summarise(number.user = n_distinct(owner_id))

# getting a Q4 average progress for each user
avg.goals.percent <- subset2.Q4.goals.achieve %>%
  select(owner_id, id, progress) %>%
  group_by(owner_id) %>%
  arrange(owner_id) %>%
  summarise(avg.progress = mean(progress))

# Calculating percent of goals that are completed
# Getting number of goals, per user, that are >= 100%
testing.completed <- subset2.Q4.goals.achieve %>%
  select(owner_id, id, progress) %>%
  group_by(owner_id) %>%
  arrange(owner_id) %>%
  tally(progress >= 1)

# Getting nuber of goals for each user
testing.completed2 <- subset2.Q4.goals.achieve %>%
  select(owner_id, id, progress) %>%
  group_by(owner_id) %>%
  arrange(owner_id) %>%
  tally()

# combining the two new values into one data frame
testing.completed[,"completed"] <- testing.completed$n
testing.completed[, "total"] <- testing.completed2$n

# Dividing number of completed goals by number of total goals, per user
testing.completed[, "complete.percent"]<-(testing.completed$completed/testing.completed$total)


# THE BIG KAHUNA
# I use the same three variables for both "avg progress" and "% of goals completed
# To change between the two, I change the first variable in subset()
# For avg progess --> subset(avg.goals.percent, )
# For % completed --> subset(testing.completed, )
less.monthly.completion <- subset(avg.goals.percent, owner_id %in% less.than.monthly.users)
monthly.completion <- subset(avg.goals.percent, owner_id %in% monthly.login.users)
weekly.completion <- subset(avg.goals.percent, owner_id %in% weekly.login.users)

summary(less.monthly.completion)
summary(monthly.completion)
summary(weekly.completion)