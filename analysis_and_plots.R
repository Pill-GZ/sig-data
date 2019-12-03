
data <- read.csv(file = "stockdata3.csv")

#### select stock ######################

stock_name <- 'd'
stock_prices <- data[,stock_name]

#### raw series ########################

setEPS()
postscript(paste0("preprocessing_", stock_name, ".eps"), width = 6, height = 8)

par(mfrow = c(4,1), mar = c(3,4,1,1), las = 1)
plot(stock_prices, type = 'l', ylab = "Price (raw)", xaxt = 'n')
days_tick_lab <- c(1, 60, 120, 180, 240, 299, 360)
axis(side = 1, labels = days_tick_lab,
     at = sapply(X = days_tick_lab, FUN = function(x){min(which(data$day == x))}))
mtext(text = 'day', side = 1, line = 1, at = 0, adj = 1.5, cex = 0.8)

#### correct series ########################

price_filled <- stock_prices
price_filled[price_filled == 0 | price_filled == 1] <- NA

# or more succintly
while (anyNA(price_filled)) {
  price_filled[is.na(price_filled)] <- price_filled[which(is.na(price_filled))-1]
}

plot(price_filled, type = 'l', ylab = "Price (gaps filled)", xaxt = 'n')
days_tick_lab <- c(1, 60, 120, 180, 240, 299, 360)
axis(side = 1, labels = days_tick_lab,
     at = sapply(X = days_tick_lab, FUN = function(x){min(which(data$day == x))}))
mtext(text = 'day', side = 1, line = 1, at = 0, adj = 1.5, cex = 0.8)

#### minute returns ########################

plot(diff(log(price_filled)), type = 'l', ylab = "Returns (by minute)", xaxt = 'n')
axis(side = 1, labels = days_tick_lab,
     at = sapply(X = days_tick_lab, FUN = function(x){min(which(data$day == x))}))
mtext(text = 'day', side = 1, line = 1, at = 0, adj = 1.5, cex = 0.8)

acf(diff(log(price_filled)), ylim = c(-1,1), lag.max = 50)
mtext(text = 'lag', side = 1, line = 1, at = 0, adj = 1.5, cex = 0.8)

#### 5-minute returns ########################

five_minutes <- levels(data$timestr)[seq(1, 391, 5)]
five_minute_price <- price_filled[data$timestr %in% five_minutes]

#### 10-minute returns ########################

ten_minutes <- levels(data$timestr)[seq(1, 391, 10)]
ten_minute_price <- price_filled[data$timestr %in% ten_minutes]

#### 20-minute returns ########################

twenty_minutes <- levels(data$timestr)[seq(1, 391, 20)]
twenty_minute_price <- price_filled[data$timestr %in% twenty_minutes]

#### 30-minute returns ########################

thirty_minutes <- levels(data$timestr)[seq(1, 391, 30)]
thirty_minute_price <- price_filled[data$timestr %in% thirty_minutes]

#### daily returns ########################

days <- unique(data$day)
daily_price <- price_filled[sapply(X = days, FUN = function(x){max(which(data$day == x))})]

# plot(diff(log(daily_price)), type = 'l', ylab = "Returns (daily)", xaxt = 'n')
# axis(side = 1, labels = days_tick_lab,
#      at = sapply(X = days_tick_lab, FUN = function(x){min(which(days == x))}))

# save image
dev.off()

#### monthly returns ########################

months <- days[round(seq(1, length(days), length.out = 13))]
monthly_price <- price_filled[sapply(X = months, FUN = function(x){max(which(data$day == x))})]

#### volatillity by sampling frequency ###################

nlevels(data$timestr)

# 1-, 5-, 10-, 20-, 30-, daily, and monthly intervals
time_freq <- c(1, 5, 10, 20, 30, nlevels(data$timestr), nlevels(data$timestr)*21)

vars_freq <- c(var(diff(log(price_filled))),
               var(diff(log(five_minute_price))),
               var(diff(log(ten_minute_price))),
               var(diff(log(twenty_minute_price))),
               var(diff(log(thirty_minute_price))),
               var(diff(log(daily_price))), 
               var(diff(log(monthly_price))))

n_samples_freq <- lengths(list(price_filled, five_minute_price, 
                               ten_minute_price, twenty_minute_price, 
                               thirty_minute_price, daily_price, monthly_price))

# calculate confidence limits of variance esteimates
alpha <- 0.05
vars_L_freq <- ((n_samples_freq-1)*vars_freq / qchisq(p = 1-alpha/2, df = n_samples_freq))
vars_U_freq <- ((n_samples_freq-1)*vars_freq / qchisq(p = alpha/2, df = n_samples_freq))

#### volatillity by sampling frequency plot ###################

setEPS()
postscript(paste0("vol-sampling-freq_", stock_name, ".eps"), width = 6, height = 6)

# plot variance estimates and error bars
par(mfrow = c(1,1), mar = c(3,4,1,1))
plot(x = time_freq, y = (vars_freq), las = 1, xaxt = 'n', yaxt = 'n', 
     ylab = '', xlab = '', log = 'xy', ylim = c(min(vars_freq)*0.9, max(vars_freq)*3))
# x-axis
axis(side = 1, at = time_freq, labels = rep("", 7))
time_freq_lab <- c(time_freq[1:5], 'min', '1 day', '1 month')
mtext(text = time_freq_lab, side = 1, at = c(time_freq[1:5], 50, time_freq[6:7]), line = 1)
mtext(side = 1, at = max(time_freq)*1.5, text = "Sampling interval (log scale)", line = 2, adj = 1)
# y-axis
var_range <- as.vector(outer(c(1,2,5), 10^(-7:-1)))
var_range_lab <- as.vector(outer(c(1,2,5), -7:-1, 
                                 FUN = function(a, e){parse(text=paste0(a,"%*%10^", e))}))
axis(side = 2, at = var_range, labels = var_range_lab, las = 1)
mtext(side = 3, at = 0.16, text = "Est. variance (log scale)", line = 0, adj = 0)
# label the plot with name of the stock
text(paste('Stock', stock_name), x = 1, y = max(vars_freq), pos = 4)

# error bars
arrows(x0 = time_freq, y0 = vars_L_freq, x1 = time_freq, y1 = vars_U_freq, 
       code = 3, angle = 90, length = 0.1, lty = 1)
# regression line
# abline(lm(vars_freq ~ -1 + time_freq, weights = (nrow(data)/time_freq)), lty = 2, untf=TRUE)
abline(lm(vars_freq ~ -1 + time_freq, weights = (nrow(data)/time_freq)^2), lty = 2, untf=TRUE, col = 2, lwd = 2)
# abline(a = 0, b = vars_freq[6] / time_freq[6], lty = 2, untf=TRUE)
# abline(a = 0, b = vars_freq[7] / time_freq[7], lty = 2, untf=TRUE)
# abline(lm(vars_freq ~ time_freq, weights = (nrow(data)/time_freq)), lty = 2, untf=TRUE)

# calculate confidence limits of pooled variance esteimates
alpha <- 0.05
m_freq <- time_freq / nlevels(data$timestr)
vars_L_freq_pooled <- (21*sum((n_samples_freq-1)*vars_freq/m_freq) / 
                         qchisq(p = 1-alpha/2, df = sum(n_samples_freq)))
vars_U_freq_pooled <- (21*sum((n_samples_freq-1)*vars_freq/m_freq) / 
                         qchisq(p = alpha/2, df = sum(n_samples_freq)))
s2_pooled <- 21*sum((n_samples_freq-1)*vars_freq/m_freq) / sum(n_samples_freq)
# error bars
arrows(x0 = time_freq[7], y0 = vars_L_freq_pooled, x1 = time_freq[7], y1 = vars_U_freq_pooled, 
       code = 3, angle = 90, length = 0.1, lty = 1, col = 2, lwd = 2)

if (stock_name %in% c('b', 'd')) {
  # CI of pooled variance esteimates using only daily + monthly
  vars_L_freq_pooled_daily_monthly <- (21*sum((n_samples_freq[6:7]-1)*vars_freq[6:7]/m_freq[6:7]) / 
                                         qchisq(p = 1-alpha/2, df = sum(n_samples_freq[6:7])))
  vars_U_freq_pooled_daily_monthly <- (21*sum((n_samples_freq[6:7]-1)*vars_freq[6:7]/m_freq[6:7]) / 
                                         qchisq(p = alpha/2, df = sum(n_samples_freq[6:7])))
  # error bars
  arrows(x0 = time_freq[7], y0 = vars_L_freq_pooled_daily_monthly, 
         x1 = time_freq[7], y1 = vars_U_freq_pooled_daily_monthly, 
         code = 3, angle = 90, length = 0.1, lty = 1, col = 'darkgreen', lwd = 2)
  s2_pooled_daily_monthly <- 21*sum((n_samples_freq[6:7]-1)*vars_freq[6:7]/m_freq[6:7]) / sum(n_samples_freq[6:7])
  points(x = 21 * 391, y = s2_pooled_daily_monthly, col = 'darkgreen', lwd = 2)
  abline(a = 0, b = s2_pooled_daily_monthly / (21 * 391), col = 'darkgreen', lty = 2, untf = TRUE, lwd = 2)
}

# save image
dev.off()


############# volatility estimated #######################################################

if (stock_name %in% c('b', 'd')) {
  print(sqrt(12*c(s2_pooled_daily_monthly, 
                  vars_L_freq_pooled_daily_monthly, 
                  vars_U_freq_pooled_daily_monthly)) * 100)
} else {
  print(sqrt(12*c(s2_pooled, 
                  vars_L_freq_pooled, 
                  vars_U_freq_pooled)) * 100) 
}

# plot(x = time_freq, y = vars_freq, ylab = 'var',
#      ylim = c(min(vars_freq)*0.9, max(vars_freq)*3))
# arrows(x0 = time_freq, y0 = vars_L_freq, x1 = time_freq, y1 = vars_U_freq,
#        code = 3, angle = 90, length = 0.5, lty = 2)
# abline(lm(vars_freq ~ -1 + time_freq, weights = nrow(data)/time_freq), lty = 2, untf=TRUE)

# plot(time_freq, vars_freq * n_samples_freq / 252, log = 'x',
#      ylim = range(vars_freq * n_samples_freq) / 252)

# lm_fit <- lm(vars_freq ~ -1 + time_freq, weights = nrow(data)/time_freq)
# summary(lm_fit)


#### EDA on Stock d ######################

n <- nrow(data)
if (stock_name == 'd') {
  
  setEPS()
  postscript(paste0("lagged_returns_", stock_name, ".eps"), width = 8, height = 5)
  s <- sample(n, n/10)
  # 1-min returns lag-1 scatter plot #
  par(mar = c(2,3,2,2))
  # plot(diff(log(price_filled[(n*9/10):n])), diff(log(price_filled[(n*9/10):n - 1])), 
  #      pch = 20, ylab = '', xlab = '', las = 1)
  plot(diff(log(price_filled[3:n - 1]))[s], diff(log(price_filled[3:n]))[s], 
       pch = 20, ylab = '', xlab = '', las = 1, cex = 1, 
       xlim = c(-0.15, 0.15), ylim = c(-0.15, 0.15),
       col = ifelse(diff(log(price_filled[3:n - 2]))[s] > 0.005, yes = 'darkgreen', 
                    no = ifelse(diff(log(price_filled[3:n - 2]))[s] < - 0.005, 
                                yes = 'red', no = 'black')))
  mtext(text = expression(r[t+1]), side = 3, line = 0.5, at = -0.18, cex = 1.2)
  mtext(text = expression(r[t]), side = 1, line = 1, at = 0.17, cex = 1.2)
  legend("topright", legend = c(expression(r[t-1] < -0.05),
                                expression(paste('|', r[t-1], '| <= ', 0.05)),
                                # expression(-0.05 <=  r[t-1] <= 0.05),
                                expression(r[t-1] > 0.05)),
         col = c('red', 'black', 'darkgreen'), pch = 20, bty = 'n')
  dev.off()
  
  # intraday vol structure
  setEPS()
  postscript(paste0("intraday_vol_", stock_name, ".eps"), width = 8, height = 5)
  par(mar = c(3,4,2,2))
  # plot(data$timestr[2:n], diff(log(price_filled)), pch = 20, cex = 0.5,
  #      xaxt = 'n', las = 1)
  plot(data$timestr[2:n], diff(log(price_filled)), pch = 20, cex = 0.5,
       ylim = c(-0.01, 0.01), xaxt = 'n', las = 1)
  axis(side = 1, at = seq(1, nlevels(data$timestr), 30), las = 1,
       labels = substr(levels(data$timestr)[seq(1, nlevels(data$timestr), 30)], start = 1, stop = 5))
  mtext(text = expression(r[t]), side = 3, line = 0.5, at = -40, cex = 1.2)
  mtext(text = 'time', side = 1, line = 1, at = 410, cex = 1.2)
  dev.off()
  
}


