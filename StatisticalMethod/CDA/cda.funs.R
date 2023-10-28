e_code <- function(rmdname, rname){
 knitr::purl(rmdname, output=rname, encoding='utf8')
}
na_count <- function(df){
 sapply(df, function(y) sum(length(which(is.na(y)))))
}

is_outlier <- function(x) {
 out <- (x < quantile(x, 0.25) - 1.5 * IQR(x) | 
          x > quantile(x, 0.75) + 1.5 * IQR(x)
 )
 return(out)
}
lv_outlier <- function(x, p = 0.0625) {
 out <- (x < quantile(x, p) | 
          x > quantile(x, 1-p)
 )
 return(out)
}


tsk <- function(x) {
 q <- quantile(x, probs = c(0.25, 0.75), na.rm = TRUE)
 e <- quantile(x, probs = c(0.125, 0.875), na.rm = TRUE)
 d <- quantile(x, probs = c(0.0625, 0.9375), na.rm = TRUE)
 m <- median(x) 
 tri <- 0.5*(m + (q[1] + q[2])/2)
 s <- ((q[2] - m) - (m - q[1]))/((q[2] - m) + (m - q[1]))
 ke <-(e[2] - e[1])/(q[2] - q [1]) - 1.740
 kd <-(d[2] - d[1])/(q[2] - q [1]) - 2.274
 out <- round(cbind(tri, s, ke, kd), 3)
 row.names(out) <- NULL
 out
}

fence <- function(x, k = 1.5) {
 quar <- quantile(x, probs = c(0.25, 0.75), na.rm = TRUE)
 iqr <- diff(quar)
 fence_l <- as.numeric(quar[1] - k * iqr)
 fence_u <- as.numeric(quar[2] + k * iqr)
 out_l <- x[(x <= fence_l)]
 out_u <- x[(x >= fence_u)]
 out <- cbind(out_l, out_u)
 out
}

hinkley <- function (d){
 md <- median(d)
 mn <- mean(d)
 iqr <- diff(quantile(d, probs = c(0.25, 0.75)))
 d <- (mn - md)/iqr
 names(d) <- "d"
 d
}

han <- function (data) {
 smooth <- stats::filter(data, 
                         c(1, 2, 1)/4)
 smooth[c(1, length(data))] <-
  data[c(1, length(data))]
 smooth
}

symmetry_plot <- function(d){
 n <- length(d)
 no <- floor((n + 1) / 2)
 sd <- sort(d)
 i <- 1 : no
 u <- sd[n + 1 - i] - median(d)
 v <- median(d) - sd[i]
 ggplot(data.frame(v, u),
        aes(v, u)) +
  geom_point(size = 0.7, color = "blue") +
  geom_abline(color="red") +
  ggtitle("symmetry plot")+
  theme_bw() +
  theme(
   plot.title = element_text(
    colour = "black",
    size = 10)
  )    
}

find_p <- function(data, p){
 d <- rep(0, length(p))
 i <- 0
 for( k in p) {
  i <- i+1
  t <- trans_p(data, k)
  h <- round(hinkley(t), 4)
  d[i] <- h
 }
 dout <- cbind(p,d)
 return(dout)
}

match_trans <- function(d, p){
 x0 <- median(d)
 if(p == 0){
  x0 + (log10(d) - log10(x0)) / (log10(exp(1)) / x0)
 }
 else {
  x0 + (d ^ p - x0 ^ p) / (p * x0 ^ (p - 1))
 }
}


trans_p <- function(x, p=0.0, Pp=FALSE){
 if(p == 0.0) {
  l <- ifelse(!is.na(x), log(x), NA)
 } else if( Pp == FALSE) {
  l <- ifelse(!is.na(x), (x^p - 1)/p , NA)
 } else {
  l <- ifelse(!is.na(x), x^p , NA)
 }
 return(l)
}


spread_level_values <- function(df, variate, group_var) {
 group_var <- enquo(group_var)
 variate <- enquo(variate)
 sl <- df %>%
  group_by(!!group_var) %>%
  dplyr::summarise(Q_LO = quantile(!!variate, 0.25),
                   Q_HI = quantile(!!variate, 0.75),
                   M = median(!!variate),
                   df = Q_HI - Q_LO,
                   log.M = log10(M),
                   log.df = log10(df)) %>% 
  dplyr::select(!!group_var, M, df, log.M, log.df) 
 sl
}
#

spread_level_plot <- function(df, variate, group_var) {
 group_var <- enquo(group_var)
 variate <- enquo(variate)
 sl <- df %>%
  group_by(!!group_var) %>%
  dplyr::summarise(Q_LO = quantile(!!variate, 0.25),
                   Q_HI = quantile(!!variate, 0.75),
                   M = median(!!variate),
                   df = Q_HI - Q_LO,
                   log.M = log10(M),
                   log.df = log10(df)
  ) 
 p <- ggplot(sl, aes(log.M, log.df, label = !!group_var)) +
  geom_point(size = 1) +
  geom_smooth(method = "lm", se = FALSE, 
              color = "blue") +
  xlab("Log(median)") + ylab("Log(d_F)") +
  geom_label(size = 3) +
  ggtitle(paste("spread vs level Plot: slope =", 
                round(lm(log.df ~ 0 + log.M, data = sl)$coef, 2))) +
  theme_bw() + 
  theme(
   plot.title = element_text(
    colour = "black",
    size = 10)
  )            
 print(p)
}

lval_plus <- function(df, variate, group_var) {
 if(nargs() == 2){
  variate <- enquo(variate)
  df %>%
   summarise(Q_LO = quantile(!! variate, 0.25),
             Q_HI = quantile(!! variate, 0.75),
             M = median(!! variate),
             df = Q_HI - Q_LO,
             Fence_HI = Q_HI + 1.5 * df,
             Fence_LO = Q_LO - 1.5 * df) -> S 
  mutate(df, 
         Fence_LO = S$Fence_LO,
         Fence_HI = S$Fence_HI,
         OUT = !! variate > S$Fence_HI | 
          !! variate < S$Fence_LO) 
 } else {
  group_var <- enquo(group_var)
  variate <- enquo(variate)
  df %>%
   group_by(!! group_var) %>%
   summarise(Q_LO = quantile(!! variate, 0.25),
             Q_HI = quantile(!! variate, 0.75),
             M = median(!! variate),
             df = Q_HI - Q_LO,
             Fence_HI = Q_HI + 1.5 * df,
             Fence_LO = Q_LO - 1.5 * df) %>% 
   select(!! group_var, Fence_LO, Fence_HI)  %>% 
   inner_join(df) %>% 
   mutate(OUT = !! variate > Fence_HI | !! variate < Fence_LO)}
}

straighten <- function(sp, px, py, Pp){
 sp$tx <- trans_p(sp[,1], px, Pp = Pp )
 sp$ty <- trans_p(sp[,2], py, Pp = Pp )
 sp$slope[1] <- with(sp, diff(ty[1:2]) / diff(tx[1:2]))
 sp$slope[2] <- with(sp, diff(ty[2:3]) / diff(tx[2:3]))
 bh <- with(sp, slope[2] / slope[1])
 bh
}

straighten_work <- function(sp, px =1, py = 1){
 sp$tx <- trans_p(sp[,1], px, Pp = FALSE )
 sp$ty <- trans_p(sp[,2], py, Pp = FALSE )
 sp$slope[1] <- with(sp, diff(ty[1:2]) / diff(tx[1:2]))
 sp$slope[2] <- with(sp, diff(ty[2:3]) / diff(tx[2:3]))
 sp$bh <- with(sp, slope[2] / slope[1])
 sp$slope[3] <- NA
 sp$bh[2:3] <- NA
 row.names(sp) <- c("Left", "Center", "Right")
 sp$bh[1]
}




# par.s estimation by group
#
pars_group <- function(df, var, group, dist){
 df %>%
  filter(!is.na({{var}})) %>%
  group_by({{group}}) %>%
  summarize(fit = list(fitdistr({{var}}, 
                                dist, 
                                na.action = na.omit)$estimate),
            .groups = "drop") %>%
  mutate({{group}} := {{group}}) %>%
  select({{group}}, fit)
}

# QQ plot for normal distribution
#
qq_normal <- function(data, var) {
 ggplot(data, aes(sample = {{var}})) +
  stat_qq(color = "blue") +
  stat_qq_line(color = "red") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 1, linetype = "dashed") +
  labs(title = "Probability(Q-Q) plot",
       x = "Theoretical quantiles (Standard Normal)",
       y = "Sample quantiles (Data)") +
  theme_bw()
}
# QQ plot by group for normal distribution
#
qq_group_normal <- function(data, var, group) {
 data %>% 
 ggplot(aes(sample = {{var}}, color = {{group}})) +
 geom_qq() +
 stat_qq_line(aes(color = {{group}})) +
 labs(title = "Q-Q plot by group",
      x = "Standard Normal", y = "Data" ) + 
 theme_bw()
}

# histogram with density 
hist_density <- function(df, var) {
 df %>% 
  ggplot(aes(x = {{var}}, y = ..density..)) + 
  geom_histogram(color = "white", fill = "blue") +
  geom_density(color = "red", adjust = 2) +
  geom_density(color = "green", adjust = 0.5) +
  theme_bw()
}

m_residuals <- function(df){
  style_m <- theme_bw() +
    theme(axis.text.x = element_text(size = 6),
          axis.title.x = element_text(size = 6),
          axis.title.y = element_text(size = 6))

  m1 <- df %>% 
    ggplot(aes(sample = rsd)) +
    stat_qq(color = sbl, size = 0.7) +
    stat_qq_line(color = lre) +
    labs(x = "표준화 잔차", y = "표준 정규 변수") +
    style_m

  m2 <- df %>% 
    ggplot(aes(x = fit, y = rsd)) +
    geom_point(color = sbl, size = 0.7) +
    geom_hline(yintercept = 0, color = lre) +
    geom_hline(yintercept = c(-2,2),
               color = sbl, linetype = "dashed") +
    labs(x = "적합치", y = "표준화 잔차") +
    style_m

  m3 <- df %>% 
    ggplot(aes(x = rsd)) +
    geom_histogram(fill = sbl, color = "white") +
    geom_vline(xintercept = 0, color = lre) +
    labs(x = "표본화 잔차", y = "빈도") +
    style_m

  m4 <- df %>% 
    ggplot(aes(x = as.numeric(row.names(df)), y = rsd)) +
    geom_line(color = "gray") +
    geom_point(color = sbl, size = 0.7) +
    geom_hline(yintercept = 0, color = lre) +
    labs(x = "순서", y = "표준화 잔차") +
    style_m

  gridExtra::grid.arrange(m1, m2, m3, m4, nrow = 2, ncol = 2)
}


data.pre <- function(data){
 data  %<>%                         # %<>% 정제후 동일 이름에 저장
  rename_all(tolower)              # .after = 또는 .before = 
 # Missing counts
 data
}
simple_reg <- function(data, x, y){
 model <- lm(formula = y~x, data = data)
 grid <- seq(min(x), max(x), by = 0.01)
 
 na <- sapply(data, function(y) sum(length(which(is.na(y))))) 
 output <- summary(model)
 print(na)
 print(output) 
 #
 c_band <- predict(model, 
                   newdata = data.frame(x = grid), 
                   interval = "confidence", level = 0.95
 )
 p_band <- predict(model, 
                   newdata = data.frame(x = grid), 
                   interval = "prediction", level = 0.95
 ) 
 data %>%
  ggplot(aes(x = x, y = y)) +
  geom_point(color = "blue")  +
  geom_smooth(method=lm, se=TRUE) +
  geom_point(aes(x = mean(x), y = mean(y)), 
             color = "yellow", 
             size = 3
  )  +
  geom_line(data = c_band, 
            aes(x =grid, y=lwr), 
            color = "red", linetype = "dashed"
  ) + 
  geom_line(data = c_band, 
            aes(x= grid, y=upr), 
            color = "red", 
            linetype = "dashed"
  ) +
  geom_line(data = p_band, 
            aes(x =grid, y=lwr),
            color = "darkgray", linetype = "dashed"
  ) + 
  geom_line(data = p_band, 
            aes(x= grid, y=upr), 
            color = "darkgray", 
            linetype = "dashed"
  ) +
  labs(title = "추정된 회귀선과 신뢰대/예측대", 
       subtitle = paste("y", round(model$coefficients[1], 2),
                        "+", round(model$coefficients[2], 2), "x", 
                        ", R^2 =", round(summary(model)$r.squared, 2))
  ) +
  theme_economist() + 
  scale_color_economist()
}