set.seed(1976)

# Количество компаний (наблюдений)
n <- 100

# Генерация факторов
ESG     <- runif(n, 0, 1)              # индекс ESG/green branding [0;1]
Quota   <- runif(n, 0, 0.5)            # доля фондов с ESG-квотой
Size    <- rnorm(n, mean = 10, sd = 1) # лог(активов)
Leverage<- rnorm(n, mean = 0.5, sd = 0.1)

# Истинные параметры (для симуляции)
alpha   <- 0.0
b1      <- 0.5   # эффект ESG
b2      <- 0.8   # эффект квоты ESG-фондов
b3      <- 1.2   # усиление ESG при высокой квоте
b4      <- 0.1
b5      <- -0.2

# Ошибка
eps     <- rnorm(n, mean = 0, sd = 0.2)

# Изменение капитализации (или доходности) как функция факторов
CapChange <- alpha +
  b1 * ESG +
  b2 * Quota +
  b3 * ESG * Quota +
  b4 * Size +
  b5 * Leverage +
  eps


df <- data.frame(CapChange, ESG, Quota, Size, Leverage)

#модель
model <- lm(CapChange ~ ESG * Quota + Size + Leverage, data = df)
summary(model)

#график зависимости CapChange от ESG
# при низкой и высокой квоте ESG-инвесторов

#"сценарные" значения квоты
Quota_low  <- 0.05
Quota_high <- 0.4

ESG_grid <- seq(0, 1, length.out = 50)

# Предсказания при фиксированных Size и Leverage (например, средних)
Size_mean     <- mean(df$Size)
Leverage_mean <- mean(df$Leverage)

new_low <- data.frame(
  ESG      = ESG_grid,
  Quota    = Quota_low,
  Size     = Size_mean,
  Leverage = Leverage_mean
)

new_high <- data.frame(
  ESG      = ESG_grid,
  Quota    = Quota_high,
  Size     = Size_mean,
  Leverage = Leverage_mean
)

pred_low  <- predict(model, newdata = new_low)
pred_high <- predict(model, newdata = new_high)

#график
plot(ESG_grid, pred_low, type = "l", lwd = 3, col = "darkblue",
     xlab = "ESG(индекс зеленого бренда)",
     ylab = "Изменение капитализации ",
     main = "ESG-эффект при разной квоте фондов")
lines(ESG_grid, pred_high, lwd = 3, col ="darkgreen")
legend("topleft",
       legend = c("Низкая квота ESG-фондов", "Высокая квота ESG-фондов"),
       col    = c("darkblue", "darkgreen"),
       lwd    = 2, bty = "n")

