library(lubridate)
library(reshape2)
library(ggplot2)
library(gridExtra)
library(fda)
library(RCurl)
library(lattice)
load('C:\\Users\\sunli\\Documents\\Ozone\\FDA\\scripts\\OzoneDate.RData')

################################## Introduction ##################################################
growthdf <- as.data.frame(cbind(growth$age, growth$hgtm, growth$hgtf))
growthdflong <- melt(growthdf, id.var = 'V1')
names(growthdflong) <- c('Age', 'ID', 'Height')
growthdflong$sex <- gsub('[0-9]+','', growthdflong$ID)
introp1 <- ggplot(data=growthdflong, aes(x=Age, y=Height, group = ID, col=sex)) + geom_point() + geom_line() + theme_bw() + labs(title = 'Berkeley Growth Data') + theme(plot.title = element_text(hjust=0.5))

#####################################################################################################
# weekly ozone
#datadir <- 'C:\\Users\\sunli\\Documents\\Ozone\\All_year_data'
#years <- list.files(datadir)
#file <- 'C:\\Users\\sunli\\Documents\\Ozone\\All_year_data\\Y1978\\L3_ozone_n7t_19781104.txt'
#yeardir <- 'C:\\Users\\sunli\\Documents\\Ozone\\All_year_data\\Y1978'
readday <- function(file){
  con1 <- file(file)
  # Parse title
  ## date
  header <- readLines(con1, 3)
  nday <- gsub('^ Day: ([0-9]{1,3}) [A-Z].*', '\\1', header[1])
  day <- gsub('^ Day: ([0-9]{1,3}) ([a-zA-Z]{3}) ([ 0-9]{2}), ([0-9]{4}) .*', '\\3', header[1])
  mon <- gsub('^ Day: ([0-9]{1,3}) ([a-zA-Z]{3}) ([ 0-9]{2}), ([0-9]{4}) .*', '\\2', header[1])
  year <- gsub('^ Day: ([0-9]{1,3}) ([a-zA-Z]{3}) ([ 0-9]{2}), ([0-9]{4}) .*', '\\4', header[1])
  date <- as.Date(gsub(' ','0', paste(year, day, mon, sep='-')), '%Y-%d-%b')
  ## lon
  lonstart <- -as.numeric(gsub('^ Longitudes: +([0-9]{3}) +bins +centered +on +([0-9.]{5,7}) +W +to +([0-9.]{5,7}) +E +\\(([0-9.]{4}) +degree +steps\\).*', '\\2', header[2]))
  lonstop <- as.numeric(gsub('^ Longitudes: +([0-9]{3}) +bins +centered +on +([0-9.]{5,7}) +W +to +([0-9.]{5,7}) +E +\\(([0-9.]{4}) +degree +steps\\).*', '\\3', header[2]))
  nlon <- as.numeric(gsub('^ Longitudes: +([0-9]{3}) +bins +centered +on +([0-9.]{5,7}) +W +to +([0-9.]{5,7}) +E +\\(([0-9.]{4}) +degree +steps\\).*', '\\1', header[2]))
  lonstep <- as.numeric(gsub('^ Longitudes: +([0-9]{3}) +bins +centered +on +([0-9.]{5,7}) +W +to +([0-9.]{5,7}) +E +\\(([0-9.]{4}) +degree +steps\\).*', '\\4', header[2]))
  lons <- seq(lonstart, lonstop, lonstep)
  ## lat
  latstart <- -as.numeric(gsub('^ Latitudes : +([0-9]{3}) +bins +centered +on +([0-9.]{4}) +S +to +([0-9.]{4}) +N +\\(([0-9.]{4}) +degree +steps\\).*', '\\2', header[3]))
  latstop <- as.numeric(gsub('^ Latitudes : +([0-9]{3}) +bins +centered +on +([0-9.]{4}) +S +to +([0-9.]{4}) +N +\\(([0-9.]{4}) +degree +steps\\).*', '\\3', header[3]))
  nlat <- as.numeric(gsub('^ Latitudes : +([0-9]{3}) +bins +centered +on +([0-9.]{4}) +S +to +([0-9.]{4}) +N +\\(([0-9.]{4}) +degree +steps\\).*', '\\1', header[3]))
  latstep <- as.numeric(gsub('^ Latitudes : +([0-9]{3}) +bins +centered +on +([0-9.]{4}) +S +to +([0-9.]{4}) +N +\\(([0-9.]{4}) +degree +steps\\).*', '\\4', header[3]))
  lats <- seq(latstart, latstop, latstep)
  # Read data
  tempdf <- read.fwf(con1, width=append(rep(list(c(4,rep(3, 24))), nlon %/% 25), list(c(4, rep(3, ((nlon %% 25) -1))))), header = F, skip =3)
  closeAllConnections()
  
  tmpm <- as.matrix(tempdf)
  tmpm[tmpm==0] <- NA
  return(list(date=date, nday=nday, m=tmpm))
}

#area is a list of 2 matrices of 288 by 180 and 360 by 180 with T and F
readyear <- function(yeardir, area){
  files <- list.files(yeardir)
  day <- as.Date(gsub('.*_([0-9]{8}).txt$', '\\1', files), '%Y%m%d')
  nday <- yday(day)
  week <- nday %/% 7
  tmp <- data.frame(day, nday, week, ozone=NA)
  for(d in files){
    daypath <- paste(yeardir, '\\', d, sep='')
    tmpm <- readday(daypath)[['m']]
    if(ncol(tmpm) == 288){
      tmp$ozone[files==d] <- mean(tmpm[area[[1]]], na.rm=T)
    }
    else if(ncol(tmpm) == 360){
      tmp$ozone[files==d] <- mean(tmpm[area[[2]]], na.rm=T)
    }
  }
  wkly <- tapply(tmp$ozone, tmp$week, mean, na.rm=T)
  return(wkly)
}
 
# Generate area for world
m288 <- matrix(TRUE, 180, 280)
m360 <- matrix(TRUE, 180, 360)
area <- list(m288=m288, m360=m360)
mycol=colorRampPalette(c('green','black','red'))(36)

datadir <- 'C:\\Users\\sunli\\Documents\\Ozone\\All_year_data'
wly <- data.frame(week=seq(0, 52))

for(yeardir in list.dirs(datadir)[-1]){
  y <- substr(yeardir, nchar(yeardir)-4, nchar(yeardir))
  print(y)
  wly[y] <- NA
  tempwkly <- readyear(yeardir, area)
  for(i in 1:length(tempwkly)){
    wly[,y][wly$week==as.numeric(names(tempwkly)[i])] <- tempwkly[i]
  }
}

wlylong <- melt(wly, id.vars='week')
names(wlylong) <- c('week', 'year', 'ozone')
##############################################################################################

pworld <- ggplot(data=wlylong, aes(x=week, y=ozone, group=year, color=year)) + geom_path() + labs(title='World Ozone', x='Week', y='Global average ozone level') + theme_bw()+theme(plot.title = element_text(hjust=0.5))

png('Ozone/FDA/plots/p1raw.png', width = 8, height = 5, units = 'in', res = 300)
pworld
dev.off()

week <- 1:53

# Get rid of years with more than 20 NAs
w0 <- wly[,sapply(wly, function(x)(sum(is.na(x)))) < 20]
y0 <- w0[,-1]
# Impute missing by using mean number of all directions with
diffsum <- matrix(0, ncol=36, nrow=53)
for(i in 1:nrow(y0)){
  for(j in 1:ncol(y0)){
    neitemp <- rbind(c(i-1,j), c(i,j-1), c(i+1,j), c(i, j+1))
    neiind <- ((neitemp[,1] %% (nrow(y0)+1)) * (neitemp[,2] %% (ncol(y0)+1)))!=0
    nei=neitemp[neiind,]
    #print(nei)
    est <- mean(unlist(apply(nei, 1, function(x)y0[x[1],x[2]])), na.rm = T)
    if(is.na(y0[i,j])){
      y0[i,j] <- est
    }
    else {
      diffsum[i, j] <-  abs(est - y0[i,j])
    }
  }
}

levelplot(diffsum, xlab='Week', ylab='', main='Difference of 4 neighbor estimation',
          scales = list(x=list(at = c(1:53),
                               labels = rownames(w0),
                               rot=90,
                               tck=0),
                        y=list(at=c(1:36),
                               labels=colnames(w0)[-1],
                               rot=0,
                               tck=0)))

# all data are substracted by the first
baseozone <- y0[1,]
for(r in 1:nrow(y0)){
  y0[r,] <- y0[r,] - baseozone
}

# Smoothing
mycol1=colorRampPalette(c('green','black','red'))(8)
plot(x=1:53, y=y0[[1]], xlim = c(0,53), ylim=c(-5,40), pch=19, main='Different numgber of basis', ylab='Ozone Level', xlab='Week')
legend(x=47,y=40, legend=as.character(c(5,11,17,23,29,35,41,47)), col=mycol1, lty = 1, lwd=1, title='nbasis')
i=1
for(n in c(5,11,17,23,29,35,41,47)){
  tempbasis <- create.fourier.basis(c(1,53), n)
  tempdf <- smooth.basis(1:53, y0[[1]], tempbasis)
  plot(tempdf, col=mycol1[i], add=T)
  i = i+1
}

# Smooth by GCV
ozonebasis <- create.fourier.basis(c(1,53), 53)
loglam = seq(-6, 6, 0.25)
ygcv <- list()
for(j in 1:length(y0)){
  Gcvsave = rep(NA, length(loglam))
  names(Gcvsave) = loglam
  Dfsave = Gcvsave
  for(i in 1:length(loglam)){
    fdPari = fdPar(ozonebasis, Lfdobj=2, 10^loglam[i])
    Sm.i = smooth.basis(1:53, y0[[j]], fdPari)
    Gcvsave[i] = sum(Sm.i$gcv)
    Dfsave[i] = Sm.i$df
  }
  curlam <- as.numeric(names(Gcvsave)[Gcvsave==min(Gcvsave)])
  curfdpari <- fdPar(ozonebasis, Lfdobj = 2, 10^curlam)
  curfd <- smooth.basis(1:53, y0[[j]], curfdpari)
  ygcv[[j]] <- curfd
}
plot(loglam,Gcvsave, xlab='log(lambda)', ylab='GCV', pch=19)

mycol2=colorRampPalette(c('green','black','red'))(50)
plot(1:53, y0[[1]], pch=19, xlab='week', ylab='Ozone level', main='Choosing lambda')
legend(x=45,y=33, legend=as.character(loglam[c(1,10,20,30,40,49)]), col=mycol2[c(1,10,20,30,40,49)], lty = 1, lwd=3)
for(i in 1:length(loglam)){
  fdPari = fdPar(ozonebasis, Lfdobj=2, 10^loglam[i])
  Sm.i = smooth.basis(1:53, y0[[1]], fdPari)
  plot(Sm.i, col=mycol2[i], add=T)
}
mycol3=colorRampPalette(c('green','black','red'))(36)
plot(1, type='n', xlab='week', ylab='Ozone level', main='Smoothed functions', xlim = c(0,53), ylim=c(-25,45))
for(i in 1:length(ygcv)){
  plot(ygcv[[i]]$fd, add=T, col=mycol3[i])
}
mycol4=colorRampPalette(c('green','black','red'))(36)
plot(1, type='n', xlab='week', ylab='Ozone level', main='Unsmoothed functions', xlim = c(0,53), ylim=c(-25,45))
for(i in 1:ncol(y0)){
  lines(1:53, y0[,i], type='l', col=mycol4[i])
}

############################################################
# Smooth with 35 fourier basis functions
# fb <- create.fourier.basis(c(1,53), 35)
# y <- list()
# for(i in 1:ncol(y0)){
#   y[[i]] <- smooth.basis(1:53, y0[,i], fb)$fd
# }
# names(y0)
# wk <- seq(1,53, length.out=300)
# ydf <- data.frame(wk=wk)
# for(i in 1:36){
#   ytmp <- eval.fd(wk, y[[i]])
#   ydf <- cbind(ydf, ytmp)
# }
# names(ydf) <- c('wk', names(y0))
# ydflong <- melt(ydf, id.vars='wk')
# names(ydflong) <- c('wk', 'year', 'ozone')
# ggplot(data=ydflong, aes(x=wk, y=ozone, color=year)) + geom_path()
###################################################################

##########################################################################
####### Registering functions
#########################################################################
source('C:\\Users\\sunli\\Documents\\Ozone\\FDA\\scripts\\ozoneFDA_functions.R')
# Convert all functions to q
wk <- seq(1,53, length.out=300)
nf <- length(ygcv)
qlist=vector('list', nf)
for(i in 1:nf){
  qlist[[i]] = f2q(ygcv[[i]]$fd, wk)
}

# Plot q functions
plot(qlist[[1]], col=mycol4[1], ylab='', main='Q functions', xlab='Week', ylim=c(-5, 5))
for(i in 2:nf){
  plot(qlist[[i]], col=mycol4[i], add=T)
}

x=wk
q1v=eval.fd(x,qlist[[1]])
dp=0
da=0
da2=0
d1=ygcv[[1]]$fd
y1=eval.fd(wk, d1)
d135 = smooth.basis(wk, y1, create.fourier.basis(c(1,53), 35))$fd
newq=vector('list',nf)
newq[[1]] <- qlist[[1]]
newd=vector('list',nf)
newd[[1]]=d135
sumd=d135

plot(d1, col=mycol4[1], ylab='Ozone level', main='Registered curves to 1st observation', xlab='Week', ylim=c(-25, 50))
for(i in 2:36){
  q2v=eval.fd(x,qlist[[i]])
  # plot(x,q1v,'l')
  # lines(x,q2v,col='red')
  re=sldp2(q1v,q2v)
  r12=pth2r(re[[2]])
  d2=ygcv[[i]]$fd
  newq[[i]] <- warp(qlist[[i]], r12)$fd
  newd2=warp(d2,r12)$fd
  newd[[i]]=newd2
  sumd = sumd + newd2
  plot(newd2, col=mycol4[i], add=T)
  # plot(d2, col='blue', add=T)
  # legend(x=250, y=1.425, c('f1(t)', 'f2(r(t))', 'f2(t)'), col=c('black', 'red', 'blue'), lty=1)
  
  #Calculate phase difference dp
  dp[i] = acos(sum(sqrt(eval.fd(seq(0,1,len=100)[2:99],r12,1)))/98)
  #Calculate amplitude difference da
  da[i] = sqrt(sum((eval.fd(seq(1,53,len=1000)[2:999],d1-newd2))^2)/998)
  da2[i] = sqrt(sum((eval.fd(seq(1,53,len=1000)[2:999], qlist[[i]]) - eval.fd(seq(1,53,len=1000)[2:999], newq[[i]])*sqrt(eval.fd(seq(0,1,len=1000)[2:999],r12,1)))^2))
}

##plot registered qs
plot(newq[[1]], col=mycol4[1], ylab='', main='Registered Q functions', xlab='Week', ylim=c(-5, 5))
for(i in 2:nf){
  plot(newq[[i]], col=mycol4[i], add=T)
}

dfdpda <- data.frame(year=rep(names(y0), 2), diff=c(dp, da), cat=rep(c('Phase', 'Amplitude'), each=36))
diffplot <- ggplot(data=dfdpda, aes(x=year, y=diff, fill=cat)) + geom_bar(stat = 'identity') + theme_bw() + labs(y='Difference of Phase/Amplitude', x='Year', title='Difference between curves') + theme(axis.text.x=element_text(angle = 90), plot.title = element_text(hjust=0.5)) + guides(fill=guide_legend(title=""))
png('Ozone/FDA/plots/diff_p_a.png', width = 8, height = 5, units = 'in', res = 300)
diffplot
dev.off()

# Plot mean curve
sumf = newd[[1]]
for(i in 2:nf){
  sumf = sumf + newd[[i]]
}
meanf = sumf*(1/nf)
png('Ozone/FDA/plots/meanf.png', width = 8, height = 5, units = 'in', res = 300)
plot(meanf, main = 'Mean function of Weekly Ozone', ylab = 'Ozone level', xlab = 'Week')
dev.off()

# coefm <- matrix(NA, 35, 36)
# basis <- newd[[1]]$basis
# for(i in 1:length(newd)){
#   coefm[,i] <- newd[[i]]$coefs
# }
# colnames(coefm) <- colnames(y0)
# newy <- fd(coefm, basis)

newym <- matrix(NA, nrow=300, ncol=36)
for(i in 1:length(newd)){
  newym[,i] <- eval.fd(wk, newd[[i]])
}
ozonebasis2 <- create.fourier.basis(c(1,53), 65)
newyfull <- smooth.basis(wk, newym, ozonebasis2)
newy <- newyfull$fd

# Bivariate variance and covariance surface using mean curve
varcov <- var.fd(newy)
vcmat <- eval.bifd(wk, wk, varcov)
persp(wk, wk, vcmat, theta = -45, phi=25, r=3, expand = 0.5,
      ticktype = 'detailed', xlab='Week', ylab='Week', zlab = 'Variance')
persp(wk, wk, vcmat, theta = 0, phi=25, r=3, expand = 0.5,
      ticktype = 'detailed', xlab='Week', ylab='Week', zlab = 'Variance')
persp(wk, wk, vcmat, theta = 45, phi=25, r=3, expand = 0.5,
      ticktype = 'detailed', xlab='Week', ylab='Week', zlab = 'Variance')

# FPCA
nb=6
pcacol <- colorRampPalette(c('red','blue','orange', 'light green', 'purple', 'brown'))(nb)
pca = pca.fd(newy, nb)
# Rotate
pca2 <- varmx.pca.fd(pca)
# Plot basis functions
plot(pca2$harmonics, col=pcacol, lty=1, xlab='Week', ylab='', main=sprintf('First %d Eigen functions', nb))
legend(0, -0.05, pca2$harmonics$fdnames[[2]], lty=1, col=pcacol)
# Plot eigen values
barplot(pca2$values[1:nb], names.arg = pca2$harmonics$fdnames[[2]], ylab='eigen values', main=sprintf('First %d Eigen values', nb))

# How to approx. a function by FPCA
pcacol2 <- colorRampPalette(c('red','black','green'))(nb)
plot(newd[[1]], col='blue', lwd=3, ylim=c(-10, 40), main='Approx. Function by FPCA', ylab='Ozone Level', xlab='Week')
plot(meanf, col='light blue', add=T, lwd=3)
for(i in 1:nb){
  pcafi <- pca2$scores[1,i] * fd(pca2$harmonics$coefs[,i], pca2$harmonics$basis)
  if(i==1){
    pcaf <- pcafi
  }
  else{
    pcaf <- pcaf + pcafi
  }
  plot(pcaf+meanf, col=pcacol2[i], add=T)
}
legend(30, 40, c('function', 'mean', '1 pc', '2 pc', '3 pc', '4 pc', '5 pc', '6 pc'), col=c('blue', 'light blue', pcacol2), lty=1, lwd=c(3,3,1,1,1,1,1,1))
# Check any relationship between year and pc
scoredf <- as.data.frame(pca2$scores)
scoredf$year <- as.numeric(substr(names(y0), 2,5))
p12<-ggplot(data=scoredf, aes(V1, V2, col=year)) + geom_point() + geom_text(aes(label=names(y0)),nudge_y=2) + labs(x='PC scores 1', y='PC scores 2') + theme(legend.position="none")
p13<-ggplot(data=scoredf, aes(V1, V3, col=year)) + geom_point() + geom_text(aes(label=names(y0)),nudge_y=2) + labs(x='PC scores 1', y='PC scores 3') + theme(legend.position="none")
p23<-ggplot(data=scoredf, aes(V2, V3, col=year)) + geom_point() + geom_text(aes(label=names(y0)),nudge_y=2) + labs(x='PC scores 2', y='PC scores 3') + theme(legend.position="none")
p14<-ggplot(data=scoredf, aes(V1, V4, col=year)) + geom_point() + geom_text(aes(label=names(y0)),nudge_y=2) + labs(x='PC scores 1', y='PC scores 4') + theme(legend.position="none")
p24<-ggplot(data=scoredf, aes(V2, V4, col=year)) + geom_point() + geom_text(aes(label=names(y0)),nudge_y=2) + labs(x='PC scores 2', y='PC scores 4') + theme(legend.position="none")
p34<-ggplot(data=scoredf, aes(V3, V4, col=year)) + geom_point() + geom_text(aes(label=names(y0)),nudge_y=2) + labs(x='PC scores 3', y='PC scores 4') + theme(legend.position="none")
png('C:\\Users\\sunli\\Documents\\Ozone\\FDA\\plots\\expFPCA.png', width = 15, height=11, unit='in', res=300)
grid.arrange(p12, p13, p23, p14, p24, p34, ncol=3)
dev.off()

# Read in temperature data
tempfilepath <- 'C:\\Users\\sunli\\Documents\\Ozone\\temperature_data\\GLB.Ts+dSST.csv'
tempraw <- read.csv(tempfilepath, skip = 1, header=T, as.is=T)
tempraw$annual <- NA
for(i in 1:nrow(tempraw)-1){
  tempraw$annual[i] <- mean(as.numeric(tempraw[i,2:13]))
}
temperature <- tempraw[tempraw$Year >1978 & tempraw$Year < 2017, 'annual']
names(temperature) <- as.character(1979:2016)
length(temperature)
plot(tempraw$Year, tempraw$annual, type='l', col='red', lwd=2, main='Temperature', xlab='Year', ylab='Temperaturechange')


# Read in CO2 data
co2file1 <- 'C:\\Users\\sunli\\Documents\\Ozone\\CO2\\Global_emission_1591.csv'
co2file2 <- 'C:\\Users\\sunli\\Documents\\Ozone\\CO2\\CO2_1970-2015_dataset_of_CO2_report_2016.csv'
co2raw1 <- read.csv(co2file1, skip = 28, header=T, as.is=T)
co2raw2 <- read.csv(co2file2, skip = 11, header=T, as.is=T)
xx7915 <- as.numeric(co2raw2[co2raw2$Country=='World', 11:47])
xx7913 <- as.numeric(co2raw2[co2raw2$Country=='World', 11:45])
yy7913 <- as.numeric(co2raw1$Total)
ggplot(aes(x=xx7913, y=yy7913), data=data.frame(xx7913, yy7913)) + geom_point() + geom_smooth(method='lm') + theme_bw() + labs(y='CO2 emision from NASA', x='CO2 emission from EDGAR (mtons)', title='Predict 2014,2015 CO2 emission (ktons)') + guides(fill=guide_legend(title="")) + theme(plot.title = element_text(hjust=0.5))
co2 <- round(xx7915/1000000, 4) # in billion tons
names(co2) <- as.character(1979:2015)
# Forcast 2016 CO2 emission data
library(forecast)
co2ts <- ts(co2, 1979, 2015, 1)
fit <- ets(co2ts)
co2 <- c(co2, as.numeric(predict(fit, 1)[2]))
names(co2)[38] <- '2016'
dco2 <- data.frame(y = names(co2), co2=co2)
ggplot(dco2, aes(y, co2)) + geom_point() + geom_smooth()
plot(x=1979:2016, co2, type='l', main='Global CO2', xlab='years', ylab='Global CO2 in Billion tons')


# Read in precipitation data (.nc file)
library(ncdf4)
ncf <- 'C:\\Users\\sunli\\Documents\\Ozone\\Precip\\precip.mon.mean.nc'
nc <- nc_open(ncf)
prep <- ncvar_get(nc)
str(prep)
prepmon <- apply(prep, 3, median)[1:456]
prepyear <- tapply(prepmon, rep(1979:2016, 12), mean)
names(prepyear) <- 1979:2016
plot(1979:2016, prepyear, type='l', lwd=2, col='blue', xlab='Year', ylab='Global average precipitation')


# Construct Design Matrix
covar <- data.frame(year = 1979:2016, intercept=rep(1, 38), tem = temperature, co2=co2, prep = prepyear)
covar2 <- covar[!(covar$year %in% c(1995, 1996)),]
Z <- list()
Z[[1]] <- covar2$intercept
Z[[2]] <- covar2$tem
Z[[3]] <- covar2$co2
Z[[4]] <- covar2$prep
str(Z)

# We now create functional parameter objects for each of the coefficient functions, using 11 Fourier basis functions for each
bbasis = create.fourier.basis(c(1, 53), 5)
bfdPar = fdPar(bbasis)
blist = vector("list",4)
for (j in 1:4) blist[[j]] = bfdPar

# Function calculate residual (Integral of different fd)
fres <- function(fit){
  coef <- fit$yfdPar$fd$coefs
  bas <- fit$yfdPar$fd$basis
  coefe <- fit$yhatfdobj$fd$coefs
  base <- fit$yhatfdobj$fd$basis
  n <- ncol(coef)
  difffd <- list()
  res <- list()
  for(i in 1:n){
    difffd[[i]] <- fd(coef[,i], bas) - fd(coefe[,i], base)
    res[[i]] <- sum((eval.fd(wk, difffd[[i]])^2))/length(wk)
  }
  return(list(difffd, res))
}

# Function calculate leave one out cross validation
floo <- function(betapar){
  totalres <- 0
  for(i in 1:36){
    looy <- fd(newy$coefs[,-i], newy$basis)
    loox <- lapply(Z, function(x)x[-i])
    loofit <- fRegress(newy, Z, betapar)
    # Predict
    b <- loofit$betaestlist
    predy <- b[[1]]$fd + Z[[2]][i]*b[[2]]$fd + Z[[3]][i]*b[[3]]$fd + Z[[4]][i]*b[[4]]$fd
    diffy <- fd(newy$coefs[,i], newy$basis) - predy
    res <- sum((eval.fd(wk, diffy)^2))/length(wk)
    totalres <- totalres + res
  }
  return(totalres)
}
# CV to choose number of basis
loores <- list()
nbas <- seq(35, 70, 5)
for(i in 1:length(nbas)){
  bbasis = create.fourier.basis(c(1, 53), nbas[i])
  bfdPar = fdPar(bbasis)
  blist = vector("list",4)
  for (j in 1:4) blist[[j]] = bfdPar
  
  loores[i] <- floo(blist)
}

plot(nbas, unlist(loores), type='l', lwd=2, col='blue', ylab='LOOCV Residual', xlab='Number of basis for beta')
abline(v=nbas[unlist(loores)==min(unlist(loores))][1], col='red')
# Zoom in
plot(nbas, unlist(loores), type='l', lwd=2, col='blue', ylab='LOOCV Residual', xlab='Number of basis for beta', xlim=c(37,70), ylim=c(979.62465, 979.62475))
abline(v=nbas[unlist(loores)==min(unlist(loores))][1], col='red')

# CV to choose lambda
loores2 <- list()
lam <- 10^seq(-6, 6, 0.5)
for(i in 1:length(lam)){
  bbasis = create.fourier.basis(c(1, 53), 65)
  bfdPar = fdPar(bbasis, 0, lam[i])
  blist = vector("list",4)
  for (j in 1:4) blist[[j]] = bfdPar
  loores2[i] <- floo(blist)
}

plot(log10(lam), unlist(loores2), type='l', lwd=2, col='blue', ylab='LOOCV Residual', xlab='log10(Lambda)')
abline(v=log10(lam[unlist(loores2)==min(unlist(loores2))][1]), col='red')

# Final model
bbasis = create.fourier.basis(c(1, 53), 41)
bfdPar = fdPar(bbasis)
blist = vector("list",4)
for (j in 1:4) blist[[j]] = bfdPar

# Fit model
fit <- fRegress(newy, Z, blist)
par(mfrow=c(2,2))
plot(fit$betaestlist[[1]]$fd, main='intercept')
plot(fit$betaestlist[[2]]$fd, main='temperature')
plot(fit$betaestlist[[3]]$fd, main='CO2')
plot(fit$betaestlist[[4]]$fd, main='precipitation')

# CI
yhatfdobj2 <- fit$yhatfdobj$fd
yhatmat <- eval.fd(wk, yhatfdobj2)
ymat <- eval.fd(wk, newy)
rmatb <- ymat - yhatmat
SigmaEb <- var(t(rmatb))
# Plot error varcov
par(mfrow=c(1,1))
persp(wk, wk, SigmaEb, theta = -45, phi=25, r=3, expand = 0.5,
      ticktype = 'detailed', xlab='Week', ylab='Week', zlab = 'Variance', zlim=c(0,70))
#persp(wk, wk, vcmat, theta = -45, phi=25, r=3, expand = 0.5,
#     ticktype = 'detailed', xlab='Week', ylab='Week', zlab = 'Variance', zlim=c(0,70))
rsqr <- 1-sum(SigmaEb)/sum(vcmat)

# Construct Y2c map from smoothing y
y2cm2 <- matrix(0, nrow=65, ncol=300)
y2cm <- newyfull$y2cMap
stderrList <- fRegress.stderr(fit, y2cm, SigmaEb)
str(stderrList)
names(stderrList)
bstr <- stderrList$betastderrlist
length(bstr)
class(bstr[[1]])
par(mfrow=c(2,2))
plotbeta(fit$betaestlist,bstr)



