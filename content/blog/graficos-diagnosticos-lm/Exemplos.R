library(tidyverse)

set.seed(4321)
dados1 <- as.data.frame(list(x = rnorm(n = 80, mean = 4, sd = 4))) |> 
  mutate(y = 3*x + 6) |> 
  mutate(y = jitter(y, amount = 13))

plot(dados1$y ~ dados1$x)

mod1 <- lm(y ~ x, data = dados1)
plot(mod1, which = 1)
plot(mod1, which = 2)



set.seed(4321)
dados2 <- as.data.frame(list(x = rnorm(n = 80, mean = 4, sd = 4))) |> 
  mutate(y = -3*x^2) |> 
  mutate(y = jitter(y, amount = 150))

mod2 <- lm(y ~ x, data = dados2)
plot(mod2, which = 1)
plot(mod2, which = 2)

