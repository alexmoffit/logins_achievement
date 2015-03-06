# NO Z
# Starts after filtering out the deleted and "Getting Started" goals
# Getting archived only
no.z.archived.achieve <- no.delete.or.started.achieve %>%
  filter(archived == "t") %>%
  filter(org_id != "20")

# Getting Q4 goals only
no.z.Q4.archive.achieve <- no.z.archived.achieve %>%
  filter(end > "2014-09-30" & end < "2015-01-01")

# Getting users who have logged in in Q4
Q4.no.z.logins.count <- Q4.no.z.logins %>%
  select(user_id, when) %>%
  group_by(user_id) %>%
  tally(sort=TRUE)

z.less.than.monthly <- Q4.no.z.logins.count %>%
  filter(n < 3)
z.less.than.monthly.users <- z.less.than.monthly$user_id

z.monthly.login <- Q4.no.z.logins.count %>%
  filter(n >= 3) %>%
  filter(n < 12)
z.monthly.login.users <- z.monthly.login$user_id

z.weekly.login <- Q4.no.z.logins.count %>%
  filter(n >= 12)
z.weekly.login.users <- z.weekly.login$user_id

# Getting Q4 goals from users who have logged in in Q1
# subset.Q4.archive.achieve <- subset(Q4.archive.achieve, owner_id %in% active.users.achieve)
# Getting Q4 goals from users who have logged in in Q4 and Q1
# subset2.Q4.archive.achieve <- subset(Q4.archive.achieve, owner_id %in% login.subset.users$user_id)

# subset.Q4.archive.achieve %>%
# summarise(number.user = n_distinct(owner_id))
# subset2.Q4.archive.achieve %>%
# summarise(number.user = n_distinct(owner_id))

# getting a Q4 average progress for each user
no.z.avg.archive.percent <- no.z.Q4.archive.achieve %>%
  select(owner_id, id, progress) %>%
  group_by(owner_id) %>%
  arrange(owner_id) %>%
  summarise(avg.progress = mean(progress))


# THE BIG KAHUNA
# I use the same three variables for both "avg progress" and "% of goals completed
# To change between the two, I change the first variable in subset()
# For avg progess --> subset(avg.goals.percent, )
# For % completed --> subset(testing.completed, )
  less.monthly.completion <- subset(no.z.avg.archive.percent, owner_id %in% z.less.than.monthly.users)
  monthly.completion <- subset(no.z.avg.archive.percent, owner_id %in% z.monthly.login.users)
  weekly.completion <- subset(no.z.avg.archive.percent, owner_id %in% z.weekly.login.users)

  summary(less.monthly.completion)
  # str(less.monthly.completion)
  summary(monthly.completion)
  # str(monthly.completion)
  summary(weekly.completion)
  # str(weekly.completion)

write.csv(less.monthly.completion, file = "LessMonthlyCompletion.csv", na = "")
boxplot(less.monthly.completion$avg.progress, monthly.completion$avg.progress, weekly.completion$avg.progress)

# Dumbest stuff ever. I hate this wrangling. Getting login count and avg. progress into the same data frame
  dumb.variable <- subset(no.z.avg.archive.percent, owner_id %in% Q4.no.z.logins.count.2$owner_id)
  dumb.variable.2 <- subset(Q4.no.z.logins.count.2, owner_id %in% no.z.avg.archive.percent$owner_id)
  
  dumb.variable.2 <- dumb.variable.2 %>%
    arrange(owner_id)
  dumb.variable[, "logins"] <- dumb.variable.2$n
  dumbest.variable[, "owner_id"] <- NULL

# Clustering
goal.cluster <- kmeans(dumbest.variable, centers = 8)
clusplot(dumbest.variable, goal.cluster$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)
goal.cluster
