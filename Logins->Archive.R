# Starts after filtering out the deleted and "Getting Started" goals
# Getting archived only
  archived.achieve <- no.delete.or.started.achieve %>%
    filter(archived == "t")

# Getting Q4 goals only
  Q4.archive.achieve <- archived.achieve %>%
    filter(end > "2014-09-30" & end < "2015-01-01")
  
# Getting users who have logged in in Q4
  Q4.logins.count <- Q4.logins.achieve %>%
    select(user_id, when) %>%
    group_by(user_id) %>%
    tally(sort=TRUE)
  
# Bucketing users into login buckets: less than monthly (n < 3), monthly (3=< n < 12), and weekly (12 < n)
  less.than.monthly <- Q4.logins.count %>%
    filter(n < 3)
  less.than.monthly.users <- less.than.monthly$user_id
  
  monthly.login <- Q4.logins.count %>%
    filter(n >= 3) %>%
    filter(n < 12)
  monthly.login.users <- monthly.login$user_id
  
  weekly.login <- Q4.logins.count %>%
    filter(n >= 12)
  weekly.login.users <- weekly.login$user_id

# Getting Q4 goals from users who have logged in in Q1
  # subset.Q4.archive.achieve <- subset(Q4.archive.achieve, owner_id %in% active.users.achieve)
# Getting Q4 goals from users who have logged in in Q4 and Q1
  # subset2.Q4.archive.achieve <- subset(Q4.archive.achieve, owner_id %in% login.subset.users$user_id)

  # subset.Q4.archive.achieve %>%
  # summarise(number.user = n_distinct(owner_id))
  # subset2.Q4.archive.achieve %>%
  # summarise(number.user = n_distinct(owner_id))

# getting a Q4 average progress for each user
  avg.archive.percent <- Q4.archive.achieve %>%
    select(owner_id, id, progress) %>%
    group_by(owner_id) %>%
    arrange(owner_id) %>%
    summarise(avg.progress = mean(progress))


# THE BIG KAHUNA
# I use the same three variables for both "avg progress" and "% of goals completed
# To change between the two, I change the first variable in subset()
# For avg progess --> subset(avg.goals.percent, )
# For % completed --> subset(testing.completed, )
  less.monthly.completion <- subset(avg.archive.percent, owner_id %in% less.than.monthly.users)
  monthly.completion <- subset(avg.archive.percent, owner_id %in% monthly.login.users)
  weekly.completion <- subset(avg.archive.percent, owner_id %in% weekly.login.users)

  summary(less.monthly.completion)
  summary(monthly.completion)
  summary(weekly.completion)