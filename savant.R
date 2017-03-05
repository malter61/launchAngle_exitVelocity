library(baseballr)
library(ggplot2)
library(data.table)
d <- fg_bat_leaders(2016, 2016) ## just something I found to scrape fangraphs
                                ## from Jim Albert's blog

start <- Sys.time() ### Should take about 30 minutes to grab all records.

## This finds the number of records (plate appearances) for each batter in the
## atbat file, and keeps only the hitters with >= 200 pa's. I also keep 
## only games from the 2016 season.
ab <- atbat[,':='(n=.N),by=.(batter)][n>=500 & substr(gameday_link,5,8)=='2016']
batters <- sort(unique(ab$batter))
length(batters)

## This loop will create a list of data frames- one df for each unique batter.
## One record for every one of the batter's pa's.
s <- list()
i=0
for(i in 1:length(batters)){
  print(i) ## just so we know where we are in the loop
  if(i %in% c(336)){i <- i+1}
  s[[i]] <- scrape_statcast_savant_batter("2016-04-01",
                                     "2016-10-02",
                                     batterid = batters[i])
}

## Tis rbinds the list of df's into one giant df.
savant.2016 <- do.call('rbind',s)
savant.2016 <- data.table(savant.2016)

## Since I'm interested in hit_angle and hit_speed, I only want records when the
## ball was put in play. 
inplay.2016 <- savant.2016[grepl('In play',description)]

end <- Sys.time()
end-start
inplay.2016$velocity <- inplay.2016$effective_speed%/%5*5 ##Rounds velocities to nearest 5 mph.


mt <- inplay.2016[player_name=='Mike Trout']
mt$hd <- mt$hit_distance_sc%/%50*50
p <- ggplot(mt[events %in% c('Home Run','Flyout','Pop Out')],
            aes(x=hit_angle,y=hit_speed,col=factor(hd),shape=factor(events)))+
            geom_point() + xlab('launch angle') + ylab('exit velocity') +
            ggtitle('Mike Trout balls hit in Air') + theme(legend.title=element_blank())
p

q <- ggplot(mt,
            aes(x=px,y=pz,color=zone))
q <- q + scale_color_discrete(name='zone',
                breaks=c('1 ','2 ','3 ','4 ','5 ','6 ','7 ','8 ','9 ','11','12','13','14'),
                     labels=c('1','2','3','4','5','6','7','8','9','11','12','13','14'))+
  geom_point() + xlab('launch angle') + ylab('exit velocity') +
  ggtitle('Mike Trout balls hit in Air')
q


write.csv(inplay.2016,'C:/baseball/data sets/inplay.2016.csv',row.names=F)






s <- list()
i=0
for(i in 1:length(fielders)){
  print(i) ## just so we know where we are in the loop
  if(i %in% c(2,3)){i <- i+1}
  s[[i]] <- scrape_statcast_savant_fielders("2016-04-01",
                                          "2016-10-02",
                                          fielderid = fielders[i])
}
