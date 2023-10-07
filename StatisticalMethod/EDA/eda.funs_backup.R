na_count <- function(df){
 sapply(df, function(y) sum(length(which(is.na(y)))))
}

is_outlier <- function(x) {
 out <- (x < quantile(x, 0.25) - 1.5 * IQR(x) | 
          x > quantile(x, 0.75) + 1.5 * IQR(x)
 )
 return(out)
}

e_code <- function(rmdname, rname){
          knitr::purl(rmdname, output=rname, encoding='utf8')
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

eda_3pt <- function(dat, x, y, x.lab = NULL, y.lab = NULL, adj = -.12, dir = TRUE,
                    pch=20,  col="grey40", ...){
 
 if(is.null(x.lab)){
  x.lab = as.character(substitute(x))
 }
 if(is.null(y.lab)){
  y.lab = as.character(substitute(y))
 }
 
 if(!missing(dat))
 {
  x <- eval(substitute(x), dat)
  y <- eval(substitute(y), dat)
 }
 
 # find unique values and add index to dataframe
 x.unique <- unique(x)
 dat2 <- data.frame(x=x, y =y, order = match(x, x.unique[order(x.unique)] ) )
 
 # We need at least three unique x values
 if (length(x.unique) < 3) stop("You need at least 3 unique x-values.")
 
 # Find the three thirds
 n <- length( unique(dat2$order) ) # Get the number of unique values
 span <- floor(n/3)
 r    <- n %% 3
 # Compute the max index of each third
 if( r == 0) d <- c(span, span *2, span *3)
 if( r == 1) d <- c(span, span *2 + 1, span *3 + 1)
 if( r == 2) d <- c(span + 1, span * 2 + 1, span *3 + 2)
 # Get X medians
 xmed <- vector(length = 3)
 xmed[1] <- median( dat2$x[dat2$order %in% (1 : d[1]) ] )
 xmed[2] <- median( dat2$x[dat2$order %in% ( (d[1] + 1) : d[2]) ])
 xmed[3] <- median( dat2$x[dat2$order %in% ( (d[2] + 1) : d[3]) ])
 # Get Y medians
 ymed <- vector(length = 3)
 ymed[1] <- median( dat2$y[dat2$order %in% (1 : d[1]) ] )
 ymed[2] <- median( dat2$y[dat2$order %in% ( (d[1] + 1) : d[2]) ])
 ymed[3] <- median( dat2$y[dat2$order %in% ( (d[2] + 1) : d[3]) ])
 
 # Compute the two slopes
 slope1 <- (ymed[2] - ymed[1]) / (xmed[2] - xmed[1])
 slope2 <- (ymed[3] - ymed[2]) / (xmed[3] - xmed[2])
 
 # Generate plot
 plot(x,y,las = 1, ylab=NA, xlab= x.lab,pch=20, col=col,
      col.lab="grey65", col.axis="grey65", fg="grey65", ...)
 #  box(col="grey80")
 #  axis(1,col="grey80", col.axis="grey80", labels=TRUE)
 #  axis(2,col="grey80", col.axis="grey80", labels=TRUE, las=1)
 mtext(y.lab, side=3, adj = adj, col="grey65")
 points(cbind(xmed,ymed), pch=16,col=rgb(1,0,0,0.5), cex=1.2)
 # Draw batch boundaries
 abline(v = dat2$x[ dat2$order== d[1] ], lty=3, col="grey")
 abline(v = dat2$x[ dat2$order== d[2] ], lty=3, col="grey")
 lines( cbind( c(min(x), xmed[2]),
               c( -( slope1*(xmed[1] - min(x)) - ymed[1] ), ymed[2])),
        col="red",lty=1)
 lines( cbind( c(xmed[2], max(x)),
               c(ymed[2], slope2*(max(x) - xmed[2]) + ymed[2]) ),
        col="red",lty=1 )
 lines(cbind(xmed[-2], ymed[-2]),lty=1,col="#444444")
 
 # Compute half-slope ratio
 hsrtio <- slope2/slope1
 
 # Determine the re-expression directions on the ladder of powers
 if (dir == TRUE) {
  if( sign(slope1) == sign(slope2) )
  {
   if( hsrtio > 1) { rexpx = "up"; rexpy = "down" }
   else if( 1 > hsrtio && hsrtio > 0 ) {rexpx = "down"; rexpy = "up"}
   else if( -1 < hsrtio && hsrtio < 0 ) {rexpx = "down"; rexpy = "down"}
   else if(  hsrtio < -1 ) {rexpx = "up"; rexpy = "up"}
   else if( hsrtio == 0) {rexpx = "N/A"; rexpy = "N/A"}
   else{ rexpx = "No change"; rexpy = "No change" }   # Case where ratio equals 1 or 0
   text.o <- sprintf("X = %s, Y = %s", rexpx, rexpy)
   mtext(text.o, side = 3, col="blue")
  }
  else{
   mtext("Slopes have different signs, no re-expression", side = 3, col="blue")
  }
 }
 out <- list(slope1, slope2, hsrtio, xmed, ymed)
 names(out) <- c("slope1", "slope2","hsrtio","xmed","ymed")
 return(out)
}

eda_lsum <- function (x, l = 5, all = TRUE)
{
 # Limit max letter summaries to 9
 if (l > 9) {
  print("Limit level summary to 9")
  return()
 }
 # letter summary labels
 let <- c("M", "H", "E", "D", "C", "B", "A", "Z", "Y", "X")
 # Remove missing values
 x <- na.omit(x)
 # Sort values
 x <- sort(x)
 # Find depths from each end
 n <- length(x)
 Lrnk <- vector()
 Mrnk <- vector()
 Rrnk <- vector()
 Lrnk[1] <- n
 Mrnk[1] <- n
 Rrnk[1] <- n
 i = 1
 while( (i <= l) & (Lrnk[i] > 1) ){
  i=i + 1
  Lrnk[i] <- floor(Lrnk[i-1] + 1 ) /2
  Mrnk[i] <- floor(Lrnk[i])
  Rrnk[i] <- floor(Lrnk[i] + 0.5)
 }
 # Get final set of letters
 val <- factor(let[1:length(Lrnk[-1])],levels=let[1:length(Lrnk[-1])])
 # Find the summary values
 LO <- (x[Mrnk[-1]] + x[Rrnk[-1]])  / 2
 HI <- (  x[n-Mrnk[-1] + 1] + x[n-Rrnk[-1]+1] ) / 2
 MD <- (LO + HI) / 2
 SP <- HI - LO
 # Generate output
 if(all == TRUE) {
  out <- data.frame(letter=val, depth=Lrnk[-1], lower=LO,
                    mid=MD, upper=HI, spread=SP)
 } else {
  out <- data.frame(letter=val, mid=MD)
 }
 return(out)
}

rline <- function (formula, df, iter = 1) {
 rline0 <- function(x, y) {
  n <- length(x)
  k <- n - floor(n/3) * 3
  if (k == 0) 
   n.gp <- c(n, n, n) / 3
  if (k == 1) {
   l <- (n - 1) / 3
   n.gp <- c(l, l + 1, l)
  }
  if (k == 2) {
   l <- (n - 2) / 3
   n.gp <- c(l + 1, l, l + 1)
  }
  left <- 1 : n.gp[1]
  center <- (n.gp[1] + 1) : (n.gp[1] + n.gp[2])
  right <- (n.gp[1] + n.gp[2] + 1) : n
  sort.x <- sort(x, index.return = TRUE)
  sort.y <- y[sort.x$ix]
  x.med <- c(median(sort.x$x[left]), 
             median(sort.x$x[center]), 
             median(sort.x$x[right]))
  y.med <- c(median(sort.y[left]), 
             median(sort.y[center]), 
             median(sort.y[right]))
  b <- (y.med[3] - y.med[1]) / 
   (x.med[3] - x.med[1])
  bR <- (y.med[3] - y.med[2]) / 
   (x.med[3] - x.med[2])
  bL <- (y.med[2] - y.med[1]) / 
   (x.med[2] - x.med[1])
  a <- mean(y.med - b * (x.med - x.med[2]))
  list(a = a, b = b, xC = x.med[2], 
       bL = bL, bR = bR,
       summary.points = cbind(x.med, y.med))
 }
 yx <- unlist(strsplit(gsub(" ", "", formula, 
                            fixed = FALSE), "~"))
 yx <- yx[nchar(yx) > 0]
 residual <- df[, yx[1]]
 x <- df[, yx[2]]
 y <- df[, yx[1]]
 sum.points <- rline0(x, y)$summary.points
 a <- 0
 b <- 0
 bL <- 0
 bR <- 0
 for (j in 1:iter) {
  fit <- rline0(x, residual)
  a <- a + fit$a
  b <- b + fit$b
  if (j == 1) {
   bR <- fit$bR
   bL <- fit$bL
  }
  residual <- y - (a + b * (x - fit$xC))
 }
 return(list(a = a, b = b, xC = fit$xC, 
             half.slope.ratio = bR / bL, 
             residual = residual, 
             spoints.x = sum.points[,1],
             spoints.y = sum.points[,2]))
}