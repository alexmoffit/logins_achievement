# Takes the list of users who logged in in Q4 & Q1, and flters out z from that
no.z.login.subset.users <- no.z.login.subset.experiment %>%
  select(user_id, when) %>%
  group_by(user_id) %>%
  tally(sort=TRUE)

z.less.than.monthly <- no.z.login.subset.users %>%
  filter(n < 3)
z.less.than.monthly.users <- z.less.than.monthly$user_id

z.monthly.login <- no.z.login.subset.users %>%
  filter(n >= 3) %>%
  filter(n < 12)
z.monthly.login.users <- z.monthly.login$user_id

z.weekly.login <- no.z.login.subset.users %>%
  filter(n >= 12)
z.weekly.login.users <- z.weekly.login$user_id

z.subset.Q4.goals.achieve <- subset(Q4.goals.achieve, owner_id %in% no.z.login.subset.users$user_id)

z.subset.Q4.goals.achieve %>%
  summarise(number.user = n_distinct(owner_id))

z.avg.goals.percent <- z.subset.Q4.goals.achieve %>%
  select(owner_id, id, progress) %>%
  group_by(owner_id) %>%
  arrange(owner_id) %>%
  summarise(avg.progress = mean(progress))

# getting the percent of goals that are completed
z.testing.completed <- subset2.Q4.goals.achieve %>%
  select(owner_id, id, progress) %>%
  group_by(owner_id) %>%
  arrange(owner_id) %>%
  tally(progress >= 1)

# this one is only necessary for doing the % Completed measure
z.testing.completed2 <- subset2.Q4.goals.achieve %>%
  select(owner_id, id, progress) %>%
  group_by(owner_id) %>%
  arrange(owner_id) %>%
  tally()

# combining the two
z.testing.completed[,"completed"] <- z.testing.completed$n
z.testing.completed[, "total"] <- z.testing.completed2$n
# math
z.testing.completed[, "complete.percent"]<-(z.testing.completed$completed/z.testing.completed$total)

z.less.monthly.completion <- subset(avg.goals.percent, owner_id %in% z.less.than.monthly.users)
z.monthly.completion <- subset(avg.goals.percent, owner_id %in% z.monthly.login.users)
z.weekly.completion <- subset(avg.goals.percent, owner_id %in% z.weekly.login.users)


summary(z.less.monthly.completion)
summary(z.monthly.completion)
summary(z.weekly.completion)