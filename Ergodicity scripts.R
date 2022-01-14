library(ggplot2)

########### time-average trials

wealth <- 1000
mat <- matrix(nrow=1000, ncol=100)
wealthvec <- numeric(nrow(mat))

set.seed(1001)

# nrow=j is the number of samples. ncol=i is the number of flips per trial
for(j in 1:nrow(mat)){
  wealth <- 1000
for(i in 1:ncol(mat)){
  flip <- sample(0:1,
              size=1,
              prob=c(0.5,0.5),
              replace=T)
  if(flip==1){
    wealth <- wealth*1.2
    } else{
    wealth <- wealth*0.82
    }
  mat[j,i] <- jitter(wealth, amount = 1)
}
  wealthvec[j] <- wealth
}



wealth <- 1000
mean(wealthvec)
sd(wealthvec)



sample_ror <- (mean(wealthvec)/wealth)^(1/ncol(mat))-1
sample_ror

sum(wealthvec>1000)
max(wealthvec)

mattran <- t(mat)
mattran2 <- data.frame(apply(mattran, 2, function(x) as.numeric(as.character(x))))

numbervec <- seq(from = 1, to=(ncol(mat)), by = 1)

numberedmat <- cbind(numbervec, mattran2)

## data for doc

plot(x=c(0,numberedmat$numbervec), y=c(wealth,numberedmat$X1), type="l", xlim=c(0,100), xaxs="i", yaxs="i",log="y", ylim=c(10,1000000), main = 
       "Figure 7: Non-Ergodic Coinflip Simulation", ylab="Wealth (logarithm, in dollars)", xlab= "Flips", yaxs="i",
     lwd=0.01, col="black")
abline(h=c(1000), col = "red")
for(i in 3:ncol(numberedmat)){
lines(x= c(0,numberedmat$numbervec), y= c(wealth,numberedmat[,i]), type="l", lwd= 0.01, col="black")
}

quantiles <- quantile(wealthvec)
Q1 <- subset(wealthvec, wealthvec<quantiles[2])
Q2 <- subset(wealthvec, wealthvec>quantiles[2] & wealthvec<quantiles[3])
Q3 <- subset(wealthvec, wealthvec>quantiles[3] & wealthvec<quantiles[4])
Q4 <- subset(wealthvec, wealthvec>quantiles[4])

mq1 <- mean(Q1)
sdq1 <- sd(Q1)
mq2 <- mean(Q2)
sdq2 <- sd(Q2)
mq3 <- mean(Q3)
sdq3 <- sd(Q3)
mq4 <- mean(Q4)
sdq4 <- sd(Q4)

quartilemeans <- round(rbind (mq1, mq2, mq3, mq4), digits = 2)
quartilesds <- round(rbind(sdq1, sdq2, sdq3, sdq4), digits = 2)
quartilemins <- round(rbind(quantiles[1], quantiles[2], quantiles[3], quantiles[4]), digits = 2)
quartilemax <- round(rbind(quantiles[2], quantiles[3], quantiles[4], quantiles[5]), digits = 2)
col1 <- rbind("First Quartile","Second Quartile","Third Quartile","Fourth Quartile")

quartiles <- cbind(col1, quartilemins, quartilemax, quartilemeans, quartilesds)

quartiledf <- as.data.frame(quartiles)
colnames(quartiledf) <- c("Table X.1.", "Minimum","Maximum", "Mean", "Standard Deviation")

### mean-var calcs

mean_var_util <- function(n, m, g, l, wealth, gam){
  exp <- wealth*m*(p*g + (1-p)*l)^n
  var <- (1/m)*(wealth^2)*((p*g^2 + (1-p)*l^2)^n-(p*g + (1-p)*l)^n)
  return(var)
  utility <- exp - (gam/2)*var^2
  return(utility)
}

mean_var_util(1, 1, 1.5, 0.6, 1000, gam=1)

########## CapM Calcs

capm <- function(R_f) 

########## space average trials

wealth <- 1000

matsat <- matrix(nrow=1000000, ncol=1)
wealthvecsat <- nrow(matsat)

# nrow=j is the number of samples. ncol=i is the number of flips per trial. Note that we need an extra *10 trials to have the same number of flips
for(j in 1:nrow(matsat)){
  wealth <- 1000
    flip <- sample(0:1,
                   size=1,
                   prob=c(0.5,0.5),
                   replace=T)
    if(flip==1){
      wealth <- wealth*1.5} 
    else{
      wealth <- wealth*0.6
    }
    matsat[j,1] <- wealth
  wealthvecsat[j] <- wealth
}

wealth <- 1000
mean(wealthvecsat)
wealthvecsatround <- round(wealthvecsat, digits = 2)
wvrahsat <- subset(wealthvecsatround, wealthvecsatround>1000)

exp_ror <- (mean(wealthvec)/wealth)^(1/ncol(mat))-1

exp_ror
sd(wealthvecsat)
mean(wealthvecsat)
sum(wealthvecsatround>wealth)

round(sum(wealthvecsatround>wealth)/nrow(matsat), digits = 2)

hist(wealthvecsatround)


###

df <- data.frame(mat)

numb <- numeric(100000)
  for(j in 1:100000){
  numb[j] <- j}

mattran <- as.data.frame(t(mat))

dftran <- cbind(numb,mattran)

summary(df)
rownames(df)

## TEST for how to graph the results

tmat <- matrix(data = c(1,2,3,4,5,6), nrow=2, ncol=3)
tmattran <- t(tmat)

tvec <- c("vec1","vec2","vec2")
tvecno <- c(1,2,3)

tdat <- as.data.frame(cbind(tvec,tvecno,tmattran))

tdat2 <- data.frame(apply(tdat, 2, function(x) as.numeric(as.character(x))))


tdat$tvecno <- as.numeric((as.character(tdat$tvecno)))
tdat$V3 <- as.numeric(as.character((tdat$V3)))
tdat$V4 <- as.numeric(as.character(tdat$V4))


typeof(tdat$tvecno)
is.numeric(tdat$V4)


plot(x=c(0,tdat2$tvecno), y=c(0,tdat2$V3), type="o", xlim=c(0,3), ylim=c(0,6))
lines(x= c(0,tdat2$tvecno), y= c(0,tdat2$V4), type="o")

## 

exp <- numeric(1000000)

for(i in 1:1000000){
  out <- 1
  flip <- sample(0:1,
                 size=1,
                 prob=c(0.5,0.5),
                 replace=T)
  
  if(flip==1){
    out <- out*1.5} 
  else{
    out <- out*0.6
  }
  exp[i] <- out
}
prob <- mean(exp)
prob

round(prob, digits = 2)