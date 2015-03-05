# NO ZYNGA
# Starts after filtering out the deleted and "Getting Started" goals
# Getting archived only
no.z.archived.achieve <- no.delete.or.started.achieve %>%
  filter(archived == "t") %>%
  filter(org_id != "20")

# Getting Q4 goals only
no.z.Q4.archive.achieve <- no.z.archived.achieve %>%
  filter(end > "2014-09-30" & end < "2015-01-01")

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
less.monthly.completion <- subset(no.z.avg.archive.percent, owner_id %in% less.than.monthly.users)
monthly.completion <- subset(no.z.avg.archive.percent, owner_id %in% monthly.login.users)
weekly.completion <- subset(no.z.avg.archive.percent, owner_id %in% weekly.login.users)

summary(less.monthly.completion)
summary(monthly.completion)
summary(weekly.completion)