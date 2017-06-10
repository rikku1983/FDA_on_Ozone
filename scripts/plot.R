library(png)
library(animation)

fdir <- 'C:/Users/sunli/Documents/mytest/monthall'
flist <- list.files(fdir)
ys <- as.numeric(gsub('^Y([0-9]{4})-.*', '\\1', flist))
ms <- as.numeric(gsub('^Y[0-9]{4}-([0-9]{1,2})\\.png', '\\1', flist))
ny <- length(unique(ys))
img <- list()
nf = length(flist)
for(i in 1:nf){
  imagepath <- paste(fdir, '/', flist[i], sep='')
  tempimg <- readPNG(imagepath)
  img[[i]] <- tempimg
}
par(mfrow=c(1,1))
pdf('C:/Users/sunli/Documents/mytest/monthall/monthall.pdf', width=16, height=22)
plot(x=1, y=1, type='n',  axes=FALSE, xlab ='month', ylab='year', xlim = c(0,12), ylim = c(range(ys)[1]-1, range(ys)[2]))
axis(side=2, at=unique(ys), labels=as.character(unique(ys)), tck=0, las=2)
axis(side=1, at=unique(ms), labels=as.character(unique(ms)), tck=0, las=0)
for(i in 1:length(img)){
  y = ys[i]
  x = ms[i]
  rasterImage(img[[i]][1:480, 1:640, 1:4], x-1,y-1,x,y)
}
dev.off()

# Make gif for yearly data
yfdir <- 'C:\\Users\\sunli\\Documents\\Ozone\\FDA\\plots\\p1\\04-17'
yflist <- list.files(yfdir)
wks <- as.numeric(gsub('week([0-9]+).png', '\\1', yflist))
yflist <- yflist[order(wks)]
wks <- sort(wks)
yimg <- list()
for(i in 1:length(yflist)){
  imagepath <- paste(yfdir, '/', yflist[i], sep='')
  tempimg <- readPNG(imagepath)
  yimg[[i]] <- tempimg
}
aniplot=function(n){
  f <- yflist[n]
  plot(x=1, y=1, type='n',  axes=FALSE, xlab = '', ylab='', main='Year 2004~2009', xlim = c(0,1), ylim = c(0, 1.2))
  rasterImage(yimg[[n]][1:480, 1:640, 1:4], 0,0,1,1)
}
trace.animate <- function() {
  lapply(1:length(wks), aniplot)
}
filename  <- paste('C:\\Users\\sunli\\Documents\\Ozone\\FDA\\plots\\p1\\weekly_ozone2.gif', sep='')
#save all iterations into one GIF
saveGIF(trace.animate(), interval = 1, movie.name=filename)




#grid.raster(nimg)
