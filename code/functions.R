f.xmap <- function(x){
	# function to create a base background map - called and named xmap in other functions
	ggplot()+
		geom_polygon(data=ak,aes(long,lat,group=group),fill=8,color='black') +
		theme(panel.background=element_rect(fill='white')) + 
		xlab(expression(paste(Longitude^o,~'W'))) +
		ylab(expression(paste(Latitude^o,~'W'))) + 
		coord_map(xlim=c(min(x$set_lon,na.rm=T)-.2,max(x$set_lon,na.rm=T)+.2),
					 ylim=c(min(x$set_lat,na.rm=T)-.2,max(x$set_lat,na.rm=T)+.2))
}

f.district_map <- function(x){
	# map of all fishing areas from 2009 - current assessment
	xmap <- f.xmap(x)
	
	if(!anyNA(unique(x$bed))){
		xmap + geom_point(data=x,aes(set_lon,set_lat,color=Bed),alpha=.2) +
			guides(colour = guide_legend(override.aes = list(alpha = 1))) +
			facet_wrap(~FY) + scale_x_continuous(breaks = -170:-130) +
			theme(legend.position = c(0.75, 0.10))
	} else{
		xmap + geom_point(data=x,aes(set_lon,set_lat),alpha=.2) +
			guides(colour = guide_legend(override.aes = list(alpha = 1))) +
			facet_wrap(~FY) + scale_x_continuous(breaks = -170:-130) +
			theme(legend.position = c(0.75, 0.10))
	}
}
