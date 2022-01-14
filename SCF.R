getwd()
setwd("C:/Users/robin/Dropbox/Harvard SJD/Thesis/Wealth Taxes/SCF csvs")

install.packages("plyr")
install.packages("RColorBrewer")
library(plyr)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
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

df1989 <- subset(df1989, df1989$ASSET>50000000)
df1992 <- subset(df1992, df1992$ASSET>50000000)
df1995 <- subset(df1995, df1995$ASSET>50000000)
df1998 <- subset(df1998, df1998$ASSET>50000000)
df2001 <- subset(df2001, df2001$ASSET>50000000)
df2004 <- subset(df2004, df2004$ASSET>50000000)
df2007 <- subset(df2007, df2007$ASSET>50000000)
df2010 <- subset(df2010, df2010$ASSET>50000000)
df2013 <- subset(df2013, df2013$ASSET>50000000)
df2016 <- subset(df2016, df2016$ASSET>50000000)
df2019 <- subset(df2019, df2019$ASSET>50000000)


vars <- c("ASSET","NETWORTH","FIN","LIQ","CDS","SAVBND","BOND","STOCKS","NMMF","RETQLIQ","CASHLI",
          "OTHMA","OTHFIN","NFIN","VEHIC","HOUSES","ORESRE","NNRESRE","BUS",
          "OTHFIN")

df1989 <- select(df1989, all_of(vars))
df1992 <- select(df1992, all_of(vars))
df1995 <- select(df1995, all_of(vars))
df1998 <- select(df1998, all_of(vars))
df2001 <- select(df2001, all_of(vars))
df2004 <- select(df2004, all_of(vars))
df2007 <- select(df2007, all_of(vars))
df2010 <- select(df2010, all_of(vars))
df2013 <- select(df2013, all_of(vars))
df2016 <- select(df2016, all_of(vars))
df2019 <- select(df2019, all_of(vars))

tempdf1989 <- cbind(df1989$ASSET, df1989[,-1]/df1989$ASSET)
tempdf1992 <- cbind(df1992$ASSET, df1992[,-1]/df1992$ASSET)
tempdf1995 <- cbind(df1995$ASSET, df1995[,-1]/df1995$ASSET)
tempdf1998 <- cbind(df1998$ASSET, df1998[,-1]/df1998$ASSET)
tempdf2001 <- cbind(df2001$ASSET, df2001[,-1]/df2001$ASSET)
tempdf2004 <- cbind(df2004$ASSET, df2004[,-1]/df2004$ASSET)
tempdf2007 <- cbind(df2007$ASSET, df2007[,-1]/df2007$ASSET)
tempdf2010 <- cbind(df2010$ASSET, df2010[,-1]/df2010$ASSET)
tempdf2013 <- cbind(df2013$ASSET, df2013[,-1]/df2013$ASSET)
tempdf2016 <- cbind(df2016$ASSET, df2016[,-1]/df2016$ASSET)
tempdf2019 <- cbind(df2019$ASSET, df2019[,-1]/df2019$ASSET)


ratmeans1989 <- colMeans(tempdf1989[-(1:2)])
ratmeans1992 <- colMeans(tempdf1992[-(1:2)])
ratmeans1995 <- colMeans(tempdf1995[-(1:2)])
ratmeans1998 <- colMeans(tempdf1998[-(1:2)])
ratmeans2001 <- colMeans(tempdf2001[-(1:2)])
ratmeans2004 <- colMeans(tempdf2004[-(1:2)])
ratmeans2007 <- colMeans(tempdf2007[-(1:2)])
ratmeans2010 <- colMeans(tempdf2010[-(1:2)])
ratmeans2013 <- colMeans(tempdf2013[-(1:2)])
ratmeans2016 <- colMeans(tempdf2016[-(1:2)])
ratmeans2019 <- colMeans(tempdf2019[-(1:2)])

df_ratios <- rbind(ratmeans1989, ratmeans1992, ratmeans1995,
                   ratmeans1998, ratmeans2001, ratmeans2004,
                   ratmeans2007, ratmeans2010, ratmeans2013,
                   ratmeans2016, ratmeans2019)

years <- seq(from = 1989, to= 2019, by = 3)

N <- c(nrow(df1989), nrow(df1992), nrow(df1995), nrow(df1998), nrow(df2001),
       nrow(df2004), nrow(df2007), nrow(df2010), nrow(df2013), nrow(df2016),
       nrow(df2019))

df_ratios <- as.data.frame(cbind(years, N, df_ratios))

vecselec <- c("years", "N", "FIN", "BOND", "STOCKS", "NMMF", "RETQLIQ","NFIN", "HOUSES", "ORESRE", "NNRESRE", "BUS")
df_rat_tab <- select(df_ratios, all_of(vecselec))
df_rat_tab[,-(1:2)] <- round(df_rat_tab[,-(1:2)], 4)

HValInf <- df_rat_tab$NFIN
HValSup <- df_rat_tab$NFIN + df_rat_tab$NMMF + df_rat_tab$RETQLIQ

df_rat_tab <- as.data.frame(cbind(df_rat_tab, HValInf, HValSup))

## plots
### ratio: 775/500
 
par(mfrow=c(1,1))

plot(df_ratios$years, df_ratios$STOCKS, type="b", main="Figure 2: Portfolio Breakdown for 
     Taxpayers with Assets in Excess of $50 million (Fin. Assets to Total Assets)", yaxs="i",
     xlab="Year", ylab="Fraction of Assets", pch=17, xaxt="n", ylim=c(0,0.25))
lines(df_ratios$years, df_ratios$BOND, type="b", pch=15, col="blue")
lines(df_ratios$years, df_ratios$NMMF, type="b", pch=16, col="red")
axis(side = 1, at = df_ratios$years,labels = T)
abline(h=seq(0,0.25,0.05), col=alpha(rgb(0,0,0), 0.3))
legend("topright",legend=c("Directly held equity","Directly held bonds", "Funds"),
       text.col=c("black","blue","red"),col=c("black","blue","red"), pch=c(17,15,16),
       lty=c(1,1))

plot(df_ratios$years, df_ratios$FIN, type="b", main="Figure 1: Portfolio Breakdown for 
     Taxpayers with Assets in Excess of $50 million (Fin. Assets vs NFin. Assets)",
     xlab="Year", ylab="Fraction of Assets", pch=17, xaxt="n", ylim=c(0,1))
lines(df_ratios$years, df_ratios$NFIN, type="b", pch=15, col="blue")
axis(side = 1, at = df_ratios$years,labels = T)
legend("topright",legend=c("Financial Assets","Non Financial Assets"),
       text.col=c("black","blue","red"),col=c("black","blue","red"), pch=c(17,15,16),
       lty=c(1,1))

plot(df_ratios$years, df_ratios$HOUSES, type="b", main="Portfolio Breakdown for 
     Taxpayers with Assets in Excess of $50 million (NFin. Assets to Total Assets)",
     xlab="Year", ylab="Fraction of Assets", pch=17, xaxt="n", ylim=c(0,0.6), yaxs="i")
lines(df_ratios$years, df_ratios$ORESRE, type="b", pch=15, col="blue")
lines(df_ratios$years, df_ratios$NNRESRE, type="b", pch=16, col="red")
lines(df_ratios$years, df_ratios$BUS, type="b", pch=18, col="darkgreen")
axis(side = 1, at = df_ratios$years,labels = T)
abline(h=seq(0,0.6,0.1), col=alpha(rgb(0,0,0), 0.3))
legend("right",legend=c("Primary Residence","Secondary Residence(s)","Other Real Estate",
                           "Private Business"),
       text.col=c("black","blue","red","darkgreen"),col=c("black","blue","red","darkgreen"),
       pch=c(17,15,16,18),lty=c(1,1))

plot(df_ratios$years, df_ratios$FIN - df_ratios$NMMF, type="b", main="Figure 3: Portfolio Breakdown for 
     Taxpayers with Assets in Excess of $50 million (E.Val vs H.Val Assets)", yaxs="i",
     xlab="Year", ylab="Fraction of Assets", pch=17, xaxt="n", ylim=c(0,0.8))
abline(h=seq(0,0.8,0.2), col=alpha(rgb(0,0,0), 0.3))
lines(df_ratios$years, df_ratios$FIN, type="b", pch=12)
lines(df_ratios$years, df_ratios$NFIN+df_ratios$NMMF, type="b", pch=15, col="blue")
lines(df_ratios$years, df_ratios$NFIN, type="b", pch=13, col="blue")
axis(side = 1, at = df_ratios$years,labels = T)
legend("bottomright",legend=c("Easily Valued Assets Inf. Lim.","Easily Valued Assets Sup. Lim.",
"Hard to Value Assets Sup. Lim.", "Hard to Value Assets Inf. Lim"),
       text.col=c("black","black","blue","blue"),col=c("black","black","blue","blue"), pch=c(17,12,15,13),
       lty=c(1,1,1,1))

#width = 800

df_ratios$NFIN+df_ratios$NMMF
df_ratios$NFIN

mean(df2019$BUS*0.8/df2019$ASSET)

### tables

df_rat_tab <- select(df_ratios2, years, FIN, BOND, STOCKS, NMMF, NFIN, HOUSES, ORESRE, NNRESRE, BUS)

write.table(df_ratios2, file = "sumstats.txt", sep = ",", quote = FALSE, row.names = F)


hello <- write.table(df_rat_tab, sep= ",")

## average imputed values



##for one
df2019 <- aggregate(. ~ YY1, SCFP2019.csv, FUN=mean)

mean(df2_2019$NETWORTH)
quantile(df2_2019$NETWORTH, 0.99)

WTL_SCF_2019 <- subset(df2_2019, df2_2019$ASSET>50000000)

vars <- c("ASSET","NETWORTH","FIN","LIQ","CDS","SAVBND","BOND","STOCKS","NMMF","RETQLIQ","CASHLI",
          "OTHMA","OTHFIN","NFIN","VEHIC","HOUSES","ORESRE","NNRESRE","BUS",
          "OTHFIN")

WTL_SCF_2019 <- select(WTL_SCF_2019, all_of(vars))

WTL_SCF_RAT_2019 <- cbind(WTL_SCF_2019$ASSET, WTL_SCF_2019[,-1]/WTL_SCF_2019$ASSET)

mean_rat_2019 <- colMeans(WTL_SCF_RAT_2019[-1])

plot(mean_rat_2019)



### calc for reconciliation with Smith, Zidar, Zwick

df2016REC <- df2016

df2016REC$BUSSZZ <- (df2016$BUS+df2016$NNRESRE) * 1.0125 #multiplyer equals 0.9 (liq. disc) * SZZ Forbes 400 Bus/SCF Bus
df2016REC$DESZZ <- (df2016$FIN - df2016$RETQLIQ - df2016$OTHFIN - df2016$CASHLI)*1.285 #multiplier equals SZZ Forbes 400 DE/SCF DE
df2016REC$ASSET <- df2016$ASSET + (df2016REC$BUSSZZ-df2016$BUS-df2016$NNRESRE) +
        df2016REC$DESZZ - (df2016$FIN - df2016$RETQLIQ - df2016$OTHFIN - df2016$CASHLI)

df2016RECsums <- colSums(df2016REC)
df2016RECsums[-1]/df2016RECsums[1]

LDtempdf2016 <- cbind(df2016REC$ASSET, df2016REC[,-1]/df2016REC$ASSET)
LDratmeans2016 <- colMeans(LDtempdf2016[-(1:2)])
LDratmeans2016

LDratmeans2016[["FIN"]]



### pie chart for 2019
pie_rat <- (df_ratios[8,c(4:13, 15:19)])
pie_rat$OTHER <- 1-sum(pie_rat[1,])
pie_rat_nam <- as.vector(colnames(pie_rat))
pie_rat <- t(pie_rat)
pie_rat <- as.vector(pie_rat)
pie_rat <- as.data.frame(cbind(as.factor(pie_rat_nam), as.numeric(pie_rat*100)))

nb.cols <- 16
mycolors <- colorRampPalette(brewer.pal(8, "Set3"))(nb.cols)


ggplot(data=pie_rat, aes(x="", y=V2, fill= (pie_rat_nam)))+
        geom_bar(width=1, color="black",stat="identity")+
        coord_polar(theta = "y", start = 0 ) +
        scale_fill_manual(values = mycolors) +
        guides(fill = guide_legend(title = "Asset Class")) +
        ggtitle("         Figure 1: Circle chart of 2019 Breakdown of Assets by Class (50MT)")+
        theme(plot.title = element_text(hjust = 0.5))+
        theme_void()


## Export df for table in word
write.csv(df_rat_tab,".\\Table3.csv", row.names = FALSE)

write.csv(df_rat_tab1M,".\\Table4.csv", row.names = FALSE)
