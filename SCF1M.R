install.packages("plyr")
library(plyr)
library(dplyr)
## Import data 

temp = list.files(pattern="*.csv")
for (i in 1:length(temp)) assign(temp[i], read.csv(temp[i]))

l.df <- lapply(ls(pattern="SCFP+"), function(x) get(x))

l.df[[1]] <- aggregate(. ~ XX1, l.df[[1]], FUN=mean)

## extract useful data, rearrange

df1989 <- aggregate(SCFP1989.csv, by = list(SCFP1989.csv$XX1), FUN=mean, na.rm = TRUE)
df1992 <- aggregate(SCFP1992.csv, by = list(SCFP1992.csv$YY1), FUN=mean, na.rm = TRUE)
df1995 <- aggregate(SCFP1995.csv, by = list(SCFP1995.csv$YY1), FUN=mean, na.rm = TRUE)

df1998 <- aggregate(. ~ YY1, SCFP1998.csv, FUN=mean)    #4
df2001 <- aggregate(. ~ YY1, SCFP2001.csv, FUN=mean)
df2004 <- aggregate(. ~ YY1, SCFP2004.csv, FUN=mean)    #6
df2007 <- aggregate(. ~ YY1, SCFP2007.csv, FUN=mean)
df2010 <- aggregate(. ~ YY1, SCFP2010.csv, FUN=mean)
df2013 <- aggregate(. ~ YY1, SCFP2013.csv, FUN=mean)    #9
df2016 <- aggregate(. ~ YY1, SCFP2016.csv, FUN=mean)
df2019 <- aggregate(. ~ YY1, SCFP2019.csv, FUN=mean)    #11

################### above same as in baseline SCF.R script. 


df19891M <- subset(df1989, df1989$ASSET>1000000)
df19921M <- subset(df1992, df1992$ASSET>1000000)
df19951M <- subset(df1995, df1995$ASSET>1000000)
df19981M <- subset(df1998, df1998$ASSET>1000000)
df20011M <- subset(df2001, df2001$ASSET>1000000)
df20041M <- subset(df2004, df2004$ASSET>1000000)
df20071M <- subset(df2007, df2007$ASSET>1000000)
df20101M <- subset(df2010, df2010$ASSET>1000000)
df20131M <- subset(df2013, df2013$ASSET>1000000)
df20161M <- subset(df2016, df2016$ASSET>1000000)
df20191M <- subset(df2019, df2019$ASSET>1000000)

df19891M <- select(df19891M, all_of(vars))
df19921M <- select(df19921M, all_of(vars))
df19951M <- select(df19951M, all_of(vars))
df19981M <- select(df19981M, all_of(vars))
df20011M <- select(df20011M, all_of(vars))
df20041M <- select(df20041M, all_of(vars))
df20071M <- select(df20071M, all_of(vars))
df20101M <- select(df20101M, all_of(vars))
df20131M <- select(df20131M, all_of(vars))
df20161M <- select(df20161M, all_of(vars))
df20191M <- select(df20191M, all_of(vars))

tempdf19891M <- cbind(df19891M$ASSET, df19891M[,-1]/df19891M$ASSET)
tempdf19921M <- cbind(df19921M$ASSET, df19921M[,-1]/df19921M$ASSET)
tempdf19951M <- cbind(df19951M$ASSET, df19951M[,-1]/df19951M$ASSET)
tempdf19981M <- cbind(df19981M$ASSET, df19981M[,-1]/df19981M$ASSET)
tempdf20011M <- cbind(df20011M$ASSET, df20011M[,-1]/df20011M$ASSET)
tempdf20041M <- cbind(df20041M$ASSET, df20041M[,-1]/df20041M$ASSET)
tempdf20071M <- cbind(df20071M$ASSET, df20071M[,-1]/df20071M$ASSET)
tempdf20101M <- cbind(df20101M$ASSET, df20101M[,-1]/df20101M$ASSET)
tempdf20131M <- cbind(df20131M$ASSET, df20131M[,-1]/df20131M$ASSET)
tempdf20161M <- cbind(df20161M$ASSET, df20161M[,-1]/df20161M$ASSET)
tempdf20191M <- cbind(df20191M$ASSET, df20191M[,-1]/df20191M$ASSET)


ratmeans19891M <- colMeans(tempdf19891M[-(1:2)])
ratmeans19921M <- colMeans(tempdf19921M[-(1:2)])
ratmeans19951M <- colMeans(tempdf19951M[-(1:2)])
ratmeans19981M <- colMeans(tempdf19981M[-(1:2)])
ratmeans20011M <- colMeans(tempdf20011M[-(1:2)])
ratmeans20041M <- colMeans(tempdf20041M[-(1:2)])
ratmeans20071M <- colMeans(tempdf20071M[-(1:2)])
ratmeans20101M <- colMeans(tempdf20101M[-(1:2)])
ratmeans20131M <- colMeans(tempdf20131M[-(1:2)])
ratmeans20161M <- colMeans(tempdf20161M[-(1:2)])
ratmeans20191M <- colMeans(tempdf20191M[-(1:2)])

df_ratios1M <- as.data.frame(rbind(ratmeans19891M, ratmeans19921M, ratmeans19951M,
                   ratmeans19981M, ratmeans20011M, ratmeans20041M,
                   ratmeans20071M, ratmeans20101M, ratmeans20131M,
                   ratmeans20161M, ratmeans20191M))

years <- seq(from = 1989, to= 2019, by = 3)

N <- c(nrow(df19891M), nrow(df19921M), nrow(df19951M), nrow(df19981M), nrow(df20011M),
       nrow(df20041M), nrow(df20071M), nrow(df20101M), nrow(df20131M), nrow(df20161M),
       nrow(df20191M))

df_ratios1M <- as.data.frame(cbind(years, N, df_ratios1M))

vecselec <- c("years", "N", "FIN", "BOND", "STOCKS", "NMMF", "RETQLIQ","NFIN", "HOUSES", "ORESRE", "NNRESRE", "BUS")
df_rat_tab1M <- select(df_ratios1M, all_of(vecselec))
df_rat_tab1M[,-(1:2)] <- round(df_rat_tab1M[,-(1:2)], 4)

HValInf <- df_rat_tab1M$NFIN
HValSup <- df_rat_tab1M$NFIN + df_rat_tab1M$NMMF + df_rat_tab1M$RETQLIQ

df_rat_tab1M <- as.data.frame(cbind(df_rat_tab1M, HValInf, HValSup))

## graphs

plot(df_ratios$years, df_rat_tab1M$STOCKS, type="b", main="Figure 5: Portfolio Breakdown for 
     Taxpayers with Assets in Excess of $1 million (Fin. Assets to Total Assets)",
     xlab="Year", ylab="Fraction of Assets", pch=17, xaxt="n", ylim=c(0,0.2), yaxs="i")
lines(df_rat_tab1M$years, df_rat_tab1M$BOND, type="b", pch=15, col="blue")
lines(df_rat_tab1M$years, df_rat_tab1M$NMMF, type="b", pch=16, col="red")
lines(df_rat_tab1M$years, df_rat_tab1M$RETQLIQ, type = "b", pch=18, col = "purple")
axis(side = 1, at = df_rat_tab1M$years,labels = T)
abline(h=seq(0,0.25,0.05), col=alpha(rgb(0,0,0), 0.3))
legend("topleft",legend=c("Directly held equity","Directly held bonds", "Funds","Retirement Accounts Excl. D.B."),
       text.col=c("black","blue","red", "purple"),col=c("black","blue","red", "purple"), pch=c(17,15,16,18),
       lty=c(1,1))

plot(df_ratios$years, df_ratios$FIN, type="b", main="Figure 1: Portfolio Breakdown for
     Taxpayers with Assets in Excess of $1 million (Fin. Assets vs NFin. Assets)", ###### not used
     xlab="Year", ylab="Fraction of Assets", pch=17, xaxt="n", ylim=c(0,1))
lines(df_ratios$years, df_ratios$NFIN, type="b", pch=15, col="blue")
axis(side = 1, at = df_ratios$years,labels = T)
legend("topright",legend=c("Financial Assets","Non Financial Assets"),
       text.col=c("black","blue","red"),col=c("black","blue","red"), pch=c(17,15,16),
       lty=c(1,1))

plot(df_rat_tab1M$years, df_rat_tab1M$HOUSES, type="b", main="Portfolio Breakdown for 
     Taxpayers with Assets in Excess of $1 million (NFin. Assets to Total Assets)",
     xlab="Year", ylab="Fraction of Assets", pch=17, xaxt="n", ylim=c(0,0.5), yaxs="i")
lines(df_rat_tab1M$years, df_rat_tab1M$ORESRE, type="b", pch=15, col="blue")
lines(df_rat_tab1M$years, df_rat_tab1M$NNRESRE, type="b", pch=16, col="red")
lines(df_rat_tab1M$years, df_rat_tab1M$BUS, type="b", pch=18, col="purple")
axis(side = 1, at = df_rat_tab1M$years,labels = T)
abline(h=seq(0,0.5,0.1), col=alpha(rgb(0,0,0), 0.3))
legend("topleft",legend=c("Primary Residence","Secondary Residence(s)","Other Real Estate",
                        "Private Business"),
       text.col=c("black","blue","red","purple"),col=c("black","blue","red","purple"),
       pch=c(17,15,16,18),lty=c(1,1))

plot(df_rat_tab1M$years, df_rat_tab1M$FIN - df_rat_tab1M$NMMF - df_rat_tab1M$RETQLIQ,
type="b", main="Figure 6: Portfolio Breakdown for 
     Taxpayers with Assets in Excess of $1 million (E.Val vs H.Val Assets)", ##E. Val LBound
     xlab="Year", ylab="Fraction of Assets", pch=17, xaxt="n", ylim=c(0,0.9), )
lines(df_rat_tab1M$years, df_rat_tab1M$FIN, type="b", pch=12)
lines(df_rat_tab1M$years, df_rat_tab1M$NFIN+df_rat_tab1M$NMMF + df_rat_tab1M$RETQLIQ, type="b", pch=15, col="blue")
lines(df_rat_tab1M$years, df_rat_tab1M$NFIN, type="b", pch=13, col="blue")
axis(side = 1, at = df_rat_tab1M$years,labels = T)
abline(h=seq(0,0.8,0.2), col=alpha(rgb(0,0,0), 0.3))
legend("bottomleft",legend=c("Easily Valued Assets Inf. Lim.","Easily Valued Assets Sup. Lim.",
                              "Hard to Value Assets Sup. Lim.", "Hard to Value Assets Inf. Lim"),
       text.col=c("black","black","blue","blue"),col=c("black","black","blue","blue"), pch=c(17,12,15,13),
       lty=c(1,1,1,1))


#### pie chart for 1MT
pie_rat1m <- (df_ratios1M[8,c(4:13, 15:19)])
pie_rat1m$OTHER <- 1-sum(pie_rat1m[1,])
pie_rat1m_nam <- as.vector(colnames(pie_rat1m))
pie_rat1m <- t(pie_rat1m)
pie_rat1m <- as.vector(pie_rat1m)
pie_rat1m <- as.data.frame(cbind(as.factor(pie_rat1m_nam), as.numeric(pie_rat1m*100)))

nb.cols <- 16
mycolors <- colorRampPalette(brewer.pal(8, "Set3"))(nb.cols)

ggplot(data=pie_rat1m, aes(x="", y=V2, fill= (pie_rat1m_nam)))+
        geom_bar(width=1, color="black",stat="identity")+
        coord_polar(theta = "y", start = 0 ) +
        scale_fill_manual(values = mycolors) +
        guides(fill = guide_legend(title = "Asset Class")) +
        ggtitle("         Figure 4: Circle chart of 2019 Breakdown of Assets by Class (1MT)")+
        theme(plot.title = element_text(hjust = 0.5))+
        theme_void()

