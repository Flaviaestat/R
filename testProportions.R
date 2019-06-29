D = data.frame(mood = c('happy', 'sad', 'meh'),
               counts = c(60, 90, 70))

# Dummy coding for the linear model
D$mood_happy = ifelse(D$mood == 'happy', 1, 0)
D$mood_sad = ifelse(D$mood == 'sad', 1, 0)

a = chisq.test(D$counts)

# As log-linear model, comparing to an intercept-only model
full = glm(counts ~ 1 + mood_happy + mood_sad, data = D, family = poisson())
null = glm(counts ~ 1, data = D, family = poisson())

b = anova(null, full, test = 'Rao')



summary(b)

summary(full)

# Note: glm can also do the dummy coding for you:
c = glm(counts ~ mood, data = D, family = poisson())