require(metafor)
require(stringr)


pocket <- read.csv("pocketdataapr28.csv", stringsAsFactors = FALSE)
pocket <- pocket[pocket[,3]==1,]
sum(as.numeric(pocket[,11]))

table(str_trim(pocket[,10])) # weighting
table(str_trim(pocket[,9]) == "UKN") # Response rate
table(str_trim(pocket[,14]) == "UKN") # Response rate


yi <- as.numeric(pocket[,15]) / 100
vi <- (yi*(1-yi)) / (as.numeric(pocket[,11]) - 1)
forest(rma(yi=yi, vi=vi, slab=paste(str_trim(pocket[,8]), pocket[,5]), method="REML"))

metavi <- weights(rma(yi=yi, vi=vi, slab=paste(str_trim(pocket[,8]), pocket[,5]), method="REML"))

wi <- 1/sqrt(vi) #weighted by inversed se, only for plotting
size  <- 0.5 + 3.0 * (wi - min(wi))/(max(wi) - min(wi))
loessdat <- data.frame(y = yi, x = as.Date(pocket[,5], format="%m/%d/%Y"))
loessfit <- loess(y~as.numeric(x), weights = metavi, data=loessdat, span=0.75)

univ <- rep(1, 29)
univ[9:24] <- 19
univ[30] <- 19
univcol <- rep('black', 29)
univcol[9:24] <- 'blue'
univcol[30] <- 'blue'


png("timetrendc.png", width = 16, height = 10, units='in', res = 300)
plot(as.Date(pocket[,5], format="%m/%d/%Y"),(yi*100), cex = size, ylim=c(20,70), xlab = "月份", ylab = "支持「袋住先」, %", frame.plot = FALSE, xlim = c(min(as.Date(pocket[,5], format="%m/%d/%Y")), as.Date("2015-6-1")), cex.lab= 1.2, col = univcol, pch = univ)
text(as.Date(pocket[,5], format="%m/%d/%Y"),(yi*100), pocket[,8], cex = 1.1, pos = 4, col = 'blue')
plx <- predict(loessfit, newdata = as.numeric(seq(from = min(loessdat$x), to = max(loessdat$x), by = 1)), se = TRUE)
abline(h=c(50), col = 'grey', lty = 2)
lines(as.numeric(seq(from = min(loessdat$x), to = max(loessdat$x), by = 1)), (plx$fit*100), lwd = 2, col = 'blue')
xseq <- as.numeric(seq(from = min(loessdat$x), to = max(loessdat$x), by = 1))
highrange <- (plx$fit + qt(0.975, plx$df)*plx$se) * 100
lowrange <- (plx$fit - qt(0.975, plx$df)*plx$se) * 100
polygon(c(xseq, rev(xseq)), c(highrange, rev(lowrange)), border = NA, col = rgb(0,0,1,0.1))
dev.off()

smoothed <- data.frame(date=seq(from = min(loessdat$x), to = max(loessdat$x), by = 1), loessfit = plx$fit, loess.min = plx$fit + qt(0.975, plx$df)*plx$se, loess.max = plx$fit - qt(0.975, plx$df)*plx$se)

write.csv(smoothed, "smoothed.csv", row.names = FALSE)

#require(ggplot2)
#plotdata <- data.frame(yi=yi, vi=vi, size = size, date = as.Date(pocket[,6], format="%m/%d/%Y"), sponsor = as.factor(pocket[,8]))
#ggplot(plotdata, aes(x = date, y = yi)) + geom_point(aes(color = sponsor, size = size * 10)) + stat_smooth()

# univariate meta-regression

rma(yi=yi, vi=vi, slab=paste(str_trim(pocket[,8]), pocket[,5]), method="REML", mod = str_trim(pocket[,9]) != "UKN")
rma(yi=yi, vi=vi, slab=paste(str_trim(pocket[,8]), pocket[,5]), method="REML", subset = str_trim(pocket[,9]) != "UKN")
rma(yi=yi, vi=vi, slab=paste(str_trim(pocket[,8]), pocket[,5]), method="REML", subset = str_trim(pocket[,9]) == "UKN")

#forest(rma(yi=yi, vi=vi, slab=paste(str_trim(pocket[,8]), pocket[,5]), method="REML", subset = str_detect(gsub(" ", "", pocket[,14]), "原地踏步")))
#forest(rma(yi=yi, vi=vi, slab=paste(str_trim(pocket[,8]), pocket[,5]), method="REML", subset = !str_detect(gsub(" ", "", pocket[,14]), "原地踏步")))
rma(yi=yi, vi=vi, slab=paste(str_trim(pocket[,8]), pocket[,5]), method="REML", mod = str_detect(gsub(" ", "", pocket[,14]), "原地踏步"), subset = str_trim(pocket[,14]) != "UKN")

str_detect(gsub(" ", "", pocket[,14]), "立法會")
rma(yi=yi, vi=vi, slab=paste(str_trim(pocket[,8]), pocket[,5]), method="REML", mod = str_detect(gsub(" ", "", pocket[,14]), "立法會"))
