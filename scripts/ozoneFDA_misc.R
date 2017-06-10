# Generate area for US
m288 <- matrix(F, 180, 288)
m360 <- matrix(F, 180, 360)
colnames(m288) <- as.character(seq(-179.375, 179.375, 1.25))
rownames(m288) <- as.character(seq(-89.5, 89.5, 1))
colnames(m360) <- as.character(seq(-179.5, 179.5, 1))
rownames(m360) <- as.character(seq(-89.5, 89.5, 1))
# Set a square area from lat: 25~50, lon: -124~-67
m288[as.numeric(rownames(m288))>25 & as.numeric(rownames(m288)) < 50, as.numeric(colnames(m288)) >-124 & as.numeric(colnames(m288)) < -67] <- T
m360[as.numeric(rownames(m360))>25 & as.numeric(rownames(m360)) < 50, as.numeric(colnames(m360)) >-124 & as.numeric(colnames(m360)) < -67] <- T
areaus <- list(m288=m288, m360=m360)

datadir <- 'C:\\Users\\sunli\\Documents\\Ozone\\All_year_data'
wlyus <- data.frame(week=seq(0, 52))

for(yeardir in list.dirs(datadir)[-1]){
  y <- substr(yeardir, nchar(yeardir)-4, nchar(yeardir))
  print(y)
  wlyus[y] <- NA
  tempwkly <- readyear(yeardir, areaus)
  for(i in 1:length(tempwkly)){
    wlyus[,y][wlyus$week==as.numeric(names(tempwkly)[i])] <- tempwkly[i]
  }
}

wlyuslong <- melt(wlyus, id.vars='week')
names(wlyuslong) <- c('week', 'year', 'ozone')
pus <- ggplot(data=wlyuslong, aes(x=week, y=ozone, group=year, color=year)) + geom_path() + labs(title='US Ozone')

pdf('Ozone/FDA/weekly_ozone.pdf', width = 7, height=8)
grid.arrange(pworld, pus, ncol=1)
dev.off()

t80 <- wlylong[wlylong$year=='Y1980',]
ggplot(data=t80, aes(x=week, y=ozone, group=year, color=year)) + geom_point()+geom_smooth(se=F, span=0.2) +geom_path()

t80us <- wlyuslong[wlyuslong$year=='Y1980',]
ggplot(data=t80us, aes(x=week, y=ozone, group=year, color=year)) + geom_point()+geom_smooth(se=F, span=0.15) +geom_path()