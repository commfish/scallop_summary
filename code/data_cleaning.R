# code to cleanup scallop observer data taken from the Kodiak Wiki
# Ben Williams ben.williams@alaska.gov
# last updated 2017-06-16


#load ----
library(tidyverse)
library(lubridate)
library(gtools)

# function to subset data by District
# Must use values : YAK, D16, EKI, WKI, KNE, KSH, KSW, UB (M), O, Q, C
scal <- function(data,district,...){
	require(dplyr)
	if(missing(district)){
		data
	} else {
		district = deparse(substitute(district))
		filter(data, District == district) 
	}
}


# data ----
#call in data by pasting the directory to the filenames then reading in all .csv and rbind them
#########################################################################################
scallops <- do.call(bind_rows,
						  lapply(gsub(" ", "",paste("./data/catch/", 
						  								  list.files("./data/catch/")), fixed=TRUE), read_csv))

names(scallops) <- c("Fishery", "District", "ADFG", "Trip_ID", "Haul_ID", 
							"haul", "gear_perf", "haul_sampled", "Set_date", "set_lat",
							"set_lon", "statarea", "depth", "dredge_count", 
							"dredge_width", "dredge_down", "dredge_up", "duration", 
							"dredge_hrs", "haul_speed", "distance_nm", "area_swept", 
							"rtnd_basket", "round_weight", "meat_weight", "est_yield",
							"tot_rtnd_basket", 'tot_day_meat_weight')

scallops %>%
	# adjust dates and set variables
	# remove data where dredge hours & area swept are zero
	# calculate round weight cpue by time and by area
	# calculate meat weight cpue by time and by area
	mutate(set_date = mdy(Set_date),
			 year = year(set_date),
			 date = yday(set_date),
			 year = ifelse(date<100, year-1, year),
			 Year = factor(year),
			 Vessel = factor(ADFG),
			 FY = ifelse(year==2009, '2009/10', 
			 				ifelse(year==2010, '2010/11',
			 						 ifelse(year==2011, '2011/12',
			 						 		 ifelse(year==2012, '2012/13',
			 						 		 		 ifelse(year==2013,'2013/14',
			 						 		 		 		 ifelse(year==2014, '2014/15',
			 						 		 		 		 		 ifelse(year==2015, '2015/16',
		 						 		 		 		 		 		 ifelse(year==2016, '2016/17', NA))))))))) %>%
	filter(area_swept>0) %>%
	mutate(rw.cpue.t = round_weight/dredge_hrs,
			 mw.cpue.t = meat_weight/dredge_hrs) -> scallops

#adjust area cpue for log transformed data (Cambell et al 1996; Cambell 2004)

rwt.adjust = mean(scallops$rw.cpue.t, na.rm=T)*.10
mwt.adjust = mean(scallops$mw.cpue.t, na.rm=T)*.10

adjust <- data.frame(rwt.adjust, mwt.adjust)

write_csv(adjust, "output/adjust.csv")

#log transform cpue data
scallops %>%
	mutate(rwt.cpue = log(rw.cpue.t+rwt.adjust),
			 mwt.cpue = log(mw.cpue.t+mwt.adjust),
			 bed=NA) %>% 
	filter(!is.na(rtnd_basket))-> scallops


write_csv(scallops, "output/scallops.csv")



# Commented out the earlier scallop data - there are some likley changes that need to be addressed


# early.scallops <- read_csv("data/old.catch/ScallopCatchByHaul_1996-2008.csv")
# names(early.scallops) <- c("Fishery", "District", "ADFG", "Trip_ID", "Haul_ID",
# 									"Haul", "Bed_Code", "Gear_Perf", "Haul_Sampled", 
# 									"Set_Date", "Set_Lat", "Set_Lon", "Statarea", "Depth",
# 									"Dredge_Count", "Dredge_Width", "Dredge_Down", 
# 									"Dredge_Up", "Duration", "Dredge_Hrs", "Haul_Speed",
# 									"Distance.nm.", "Area_Swept.nm2.", "Rtnd_Basket", 
# 									"Round_Weight", "Meat_Weight", "State_Waters")

# early.scallops %>% 
# 	mutate(set_date = mdy(Set_Date),
# 			 year = year(set_date),
# 			 date = yday(set_date),
# 			 year = ifelse(date<100, year-1, year),
# 			 Year = factor(year),
# 			 Vessel = factor(ADFG)) %>% glimpse()
# 	-> early_scallops
# 	
# write_csv(early.scallops, file="../2016_Observer_Summary/data/early.scallops.csv")

# scallops %>% 
# mutate(Set_Date = as.Date(Set_Date, "%m-%d-%Y")) -> scallops 


# tbl %>% 
# mutate(rwt.cpue = log(rw.cpue.t+rwt.adjust),
# rwa.cpue = log(rw.cpue.a+rwa.adjust)) -> tbl

# write_csv(tbl, file="output/tbl.csv")

# d16 ----
d16 <- scal(scallops, D16)
write_csv(d16, "output/d16.csv")

# yak ----
yak <- filter(scallops, District=='YAK'| District=='D16')

#split by bed
yak <- within(yak, bed[set_lon > -138.3]  <-6)#actually bed D16
yak <- within(yak, bed[set_lon < -138.301] <-5)
yak <- within(yak, bed[set_lon < -138.79] <-4)
yak <- within(yak, bed[set_lon < -140.1]  <-3)
yak <- within(yak, bed[set_lon < -142.15] <-2)
yak <- within(yak, bed[set_lon < -142.7]  <-1)
yak <- within(yak, bed[set_lon < -143.5]  <-0)#actually bed B

yak %>% 
	filter(complete.cases(.)) %>% 
	mutate(Bed=factor(bed)) -> yak

#change bed names
yak$Bed <- recode(yak$Bed, "0"="B", "6"='D16')
write_csv(yak, "output/yak.csv")

# ksh ----
ksh <- scal(scallops,KSH)

ksh <- within(ksh,bed[set_lat > 58.35] <-1)
ksh <- within(ksh,bed[set_lat < 58.35] <-'2-7')
ksh$Bed <- factor(ksh$bed)
write_csv(ksh, "output/ksh.csv")

# kne ----
kne <- scal(scallops,KNE)
kne <- within(kne,bed[set_lat > 57.8] <- 1)
kne <- within(kne,bed[set_lat < 57.8 & set_lat > 57.17 & set_lon > -151.9] <- 2 )
kne <- within(kne,bed[set_lon< -151.9] <- 3)
kne <- within(kne,bed[set_lat<57.03 & set_lon> -152.2] <- 4)
kne <- within(kne,bed[set_lat<56.9 & set_lon> -152.3] <- 5)
kne <- within(kne,bed[set_lat<56.95 & set_lon< -152.5] <- 6)
kne$Bed <- factor(kne$bed)

write_csv(kne, "output/kne.csv")

# ksw ----
ksw <- scal(scallops,KSW)
ksw <- within(ksw,bed[set_lat > 56.7] <-1)
ksw <- within(ksw,bed[set_lat < 56.7] <-2)
ksw$Bed <- factor(ksw$bed)
write_csv(ksw, "output/ksw.csv")

# m ----

m <- scal(scallops,UB)
write_csv(m, "output/m.csv")

# o ----
o <- scal(scallops,O)
o<-within(o,bed[set_lon > -168 & set_lat > 53.2]<-1)
o<-within(o,bed[set_lon < -168 & set_lat > 53.2]<-4)
o<-within(o,bed[set_lat < 53.2 ]<-2)
o$Bed <- factor(o$bed)

write_csv(o, "output/o.csv")

# q ----

q <- scal(scallops,Q)
#excluded samples outside of the main bed
q <- filter(q,set_lon>-166)
write_csv(q, "output/q.csv")


# ki ----
ki <- filter(scallops, District=='WKI'| District =='EKI')
#excluded samples outside of the main bed
ki <- within(ki, bed[District == 'WKI'] <-'wki')
ki <- within(ki, bed[District == 'EKI']<-'eki')
ki$Bed <- factor(ki$bed)
write_csv(ki, "output/ki.csv")

# ksem ----

# ksem <- scal(scallops,KSEM)
# #excluded samples outside of the main bed
# write_csv(q, "output/ksem.csv")

# shell height ----

sh <- do.call(bind_rows,
				  lapply(gsub(" ", "",paste("./data/shell_height/", 
				  								  list.files("./data/shell_height/")), fixed=TRUE), read_csv))

write_csv(sh, "output/sh.csv")

# log book ----
log <- do.call(bind_rows,
					lapply(gsub(" ", "",paste("./data/log/", 
													  list.files("./data/log/")), fixed=TRUE), read_csv))

write_csv(log, "output/log.csv")

# crab ----

crab <- do.call(bind_rows,
					 lapply(gsub(" ", "",paste("./data/crab_size/", 
					 								  list.files("./data/crab_size/")), fixed=TRUE), read_csv))

write_csv(crab, "output/crab.csv")

# bycatch ----

by <- do.call(bind_rows,
				  lapply(gsub(" ", "",paste("./data/bycatch/", 
				  								  list.files("./data/bycatch/")), fixed=TRUE), read_csv))

by %>% 
	mutate(set_date = as.Date(Set_Date, "%m-%d-%Y"),
			 year = as.numeric(format(set_date, "%Y")),
			 date = yday(set_date),
			 year = ifelse(date<100, year-1, year),
			 Year = factor(year),
			 Vessel = factor(ADFG)) -> by

write_csv(by, "output/by.csv")

# age ----

ages <- read.csv("./data/age/scallop_ages_bcw.csv")
ages <- ages[,2:20]

#add 2014 data
ages14 <- read.csv("./data/age/2014ScallopShellAgeData.csv")
ages14 <- ages14[,1:17]
#adjust names to match other files
names(ages14) <- c('id', 'trip_id', 'haul_id', 'aged_by','fishery',
						 'fishing_season','district','vessel','trip_or_packet','date',
						 'small_or_retained','haul','shell','annuli','letter_code',
						 'number_code','shell_height')
ages14 %>%
   mutate(shell = factor(shell)) -> ages14

ages<- smartbind(ages,ages14)

#add 2015 data
ages15 <- read.csv("./data/age/2015ShellAgeData_5.10.16.csv")
ages15 <- ages15[,1:16]

names(ages15) <- c( 'trip_id', 'haul_id', 'aged_by','fishery', 'fishing_season',
						  'district','vessel','trip_or_packet','date',
						  'small_or_retained','haul','shell','annuli','letter_code',
						  'number_code','shell_height')
ages15 %>%
	mutate(letter_code = factor(letter_code), shell = factor(shell)) -> ages15

ages <- smartbind(ages, ages15)

names(ages) <- c('id','Trip_ID', 'Haul_ID', 'aged_by','date_aged','Fishery',
					  'Season','District','ADFG','packetnum','stat_area','Set_Date',
					  'Rtnd_Disc','haul','shell_num','annuli','code_let','code_num',
					  'Shell_Height')

ages %>% mutate(set_date = as.Date(ages$Set_Date, "%m/%d/%Y"),
                year = as.numeric(format(set_date, "%Y")),
                date = yday(set_date),
                year = ifelse(date<100, year-1, year),
                Year = factor(year)) -> ages

ages %>%
   mutate(Set_Date = format(Set_Date, format = "%Y-%m-%d")) %>%
   select(-id, -aged_by, -date_aged, -stat_area, -code_let, -code_num, -shell_num)-> ages


write_csv(ages, "output/ages.csv")
