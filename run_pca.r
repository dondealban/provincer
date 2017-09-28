# POSSIBLE FEATURES
#+ add wos country species query (latin + common names), to show how much work is done on specific species



library("tidyverse")
library("sf")
library("ggfortify")
library("cluster")
library("rredlist")
library("countrycode")
library("vegan")
library("lme4")
library("lmerTest")
library("data.table")
library("outliers")
library("viridis")

token = "9bb4facb6d23f48efbf424bb05c0c1ef1cf6f468393bc745d42179ac4aca5fee"

outlierReplace <- function(dataframe, cols, rows, newValue = NA) {
    if (any(rows)) {
        set(dataframe, rows, cols, newValue)
    }
}
outliersper <- function(x){
  length(which(x >  mean(x) + 3 * sd(x) | x < mean(x) - 3 * sd(x))  ) / length(x)
}
pull <- function(x,y) {x[,if(is.name(substitute(y))) deparse(substitute(y)) else y, drop = FALSE][[1]]}
clean 																					<- function(filenames){
	for (filename in filenames) {
		if (file.exists(filename)) file.remove(filename)
	}
	return(NULL)
}
getallISOspecies																<- function(specieslist){
	spcountries 		<- list()
	for (i in 1:length(specieslist))
		{
			res <- rl_occ_country(name = specieslist[i],key=token)
			spcountries <- c(spcountries,res$result$code)
		}
	spcountries <- unique(spcountries)
	iso3spcountries <- list()	
	for (i in 1:length(spcountries)) iso3spcountries <- c(iso3spcountries,countrycode(spcountries[[i]], "iso2c","iso3c"))	
	return(iso3spcountries) 
}
getsprange																			<- function(iso3,lvl=0,province_file_id=NULL,buffer=NULL,legends,province_id=NULL){
	if (is.null(buffer) || buffer == 0) sprangefilename <- paste(sprangepath,iso3,lvl,"_",province_file_id,"_in",CACHE_FILE_TYPE,sep="")	
	else																sprangefilename	<- paste(sprangepath,iso3,lvl,"_",province_file_id,"_in","_buffer_deg_",buffer,CACHE_FILE_TYPE,sep="")	
	print(paste("reading ",sprangefilename))
	if (!file.exists(sprangefilename)) {
		sprange_within <- NULL
		print("No sp range file. returning NULL")
	}
	else {
		print("attempting read sp range")
		sprange_within	<- st_read(sprangefilename)
		print("successfully read sp range")
		
	}
	return(sprange_within)
}
getspsummary																		<- function(sp, species, iso3, province_file_id, buffer=NULL,legends,province_id=NULL){	
	print(paste0("doing species summary ",species))
	result = tryCatch({
		if (length(st_geometry(sp)) == 0) return(NULL)
			
		spofinterest <- sp %>% 
			filter(binomial %in% species) 
			
		if (length(st_geometry(spofinterest)) == 0) return(NULL)
			
		spofinterest <- spofinterest	%>% 
				filter(legend %in% legends) 
				
		if (length(st_geometry(spofinterest)) == 0) return(NULL)
		
		spofinterest <- spofinterest	%>% 
					mutate(size_m2 = st_area(st_geometry(spofinterest))) %>% 
						select(binomial,size_m2) %>% 
							mutate(binomial = make.names(paste(spec_col_identifier,binomial,"buffer",buffer,"deg",sep="_"))) %>%
								group_by(binomial) %>% 
									as_tibble()
		
		st_geometry(spofinterest) <- NULL
		
		spprovsummary <- spofinterest %>% 
			group_by(binomial) %>% 
				summarise(size_m2_total = sum(size_m2)) %>% 
					select(binomial,size_m2_total) %>% 
						replace_na(list(size_m2_total = 0)) %>% 
							spread(key=binomial,value=size_m2_total)
		
		spprovsummary$country_iso <- iso3
		spprovsummary$province_file_id <- province_file_id
		spprovsummary$province_id <- province_id
		
		
		print(spprovsummary)

	}, error = function(e) {
	 return(NULL)
	})
	
	return(spprovsummary)
}

sprangepath																			<- "data/geo/IUCNsprange/"
provinceshpfile																	<- "data/stats/clean/gadm28_adm1_multi.gpkg"
CACHE_FILE_TYPE																	<- ".gpkg"
provincrres																			<- "supplementary material/aggregated regional data - sep 2017.csv"
subsetfile																			<- "supplementary material/subset_supplementary.csv"
recipefile																			<- "results/recipes_pub.csv"
spec_col_identifier															<- "SPX"
term																						<- "supplement_redone_remoteness_with_port_stats_v2"
results																					<- "results/"
date																						<- format(Sys.time(), "%d-%b-%Y")
# folder																					<- make.names(paste(date,provincrres,subsetfile,recipefile))
folder																					<- make.names(paste(term,date))

dir.create(file.path(results, folder), showWarnings = FALSE)
mainpath																				<- paste0(results,folder)

rawmapbase																			<- paste0(mainpath,"/mapdata_raw_")
subsetcachebase																	<- paste0(mainpath,"/subset_ISOs_")
qualitiescachebase															<- paste0(mainpath,"/subset_spqualities_")
plotbase																				<- paste0(mainpath,"/rawdata_plot_")
inputbase																				<- paste0(mainpath,"/rawdata_input_")
clustbase																				<- paste0(mainpath,"/rawdata_clust_")

provincetmp																			<- read.csv(file = provincrres,na.strings=c(""," ","NA"))
provinces																				<- as_tibble(provincetmp)



# ID FIX IN CASE NEEDED
# provinces <- provinces %>% group_by(country_iso) %>% filter(province_id >0) %>% mutate(shape_unique_id_1 = 1:n()) %>% ungroup() 	%>% mutate(province_label = paste(province_name, province_id, shape_unique_id_1, sep = ' ')) %>% as_tibble()
# print(provinces$province_label)
# stop()

# FIXME UNBALANCED COLUMNS - JUST FOR LM SCALING!!!!!
# provinces$country_iso 													<- as.factor(provinces$country_iso)
# provinces$country_name_en 											<- as.factor(provinces$country_name_en)
# provinces$province_id 													<- as.factor(provinces$province_id)
# provinces$province_label 												<- as.factor(provinces$province_label)


provincesshp																		<- st_read(provinceshpfile)

str(provinces)



# FIX FOUR DUPLICATE ENTRIES IN GADM1. MORE HANDLING IN MERGE
tmp_shp 																				<- provincesshp
st_geometry(tmp_shp)														<- NULL
tmp_shp <- tmp_shp %>%
  group_by(ISO) %>%
  mutate(unique_id_1 = 1:n()) %>%
	ungroup %>% 
	as_tibble()	

tmp_iso_counts_gadm1 <- tmp_shp %>% 
	  group_by(ISO) %>% 
	  summarise(n = n()) %>%
		as_tibble()			
	
tmp																							<- tmp_shp	
st_geometry(tmp) 																<- st_geometry(provincesshp)
provincesshp																		<- tmp
print(provincesshp)



subset																					<- read.csv(file = subsetfile,na.strings=c(""," ","NA"))

qualities																				<- unique(subset$Quality)
recipes																					<- read.csv(file = recipefile,na.strings=c(""," ","NA"))
recipenames																			<- unique(recipes$Recipe)



# set_wd(".")

# STEP 1 - IF NEEDED, GET SPECIES TARGET COUNTRIES
for (j in 1:length(recipenames)){
	for (i in 1:length(qualities)) {		
		quality								<- qualities[i]
		recipe								<- recipenames[j]
		#params								<- recipes %>% filter(Include == T & Recipe == recipe) %>% select(Parameter)	%>% filter(!is.na(.))	%>% as.list()	
		
		partmp <- recipes[recipes$Recipe == recipe,]
		partmp <- partmp[partmp$Include,]
		params <- partmp[rowSums(is.na(partmp)) != ncol(partmp),]$Parameter			
		print(params)
		
		buffers								<- as.numeric(as.list(strsplit(as.character(subset[subset$Quality == quality,]$RangeModDegLatLon[1]), '|',fixed=TRUE)[[1]]))				
		print(buffers)
		legends								<- as.list(strsplit(as.character(subset[subset$Quality == quality,]$Legend[[1]]), '|',fixed=TRUE))[[1]]
		print(legends)
		
		qualityfilename				<- paste0(qualitiescachebase,quality,".csv")
		provincestmp					<- provinces
		normcolumns						<- ncol(as.data.frame(provincestmp))
		
		if (!file.exists(qualityfilename)){
			resfile 							<- paste0(subsetcachebase,quality,".csv")	
			helpertibble					<- tibble(country_iso = character(), province_id = integer())
			# get the isos of the countries that have the species of interest. 
			if (!(file.exists(resfile))){
				# FIXME: TEST THIS				
				print(paste("CREATING QUALITYFILE ",quality))
				if (as.character(subset[subset$Quality == quality,]$Species)=="ALL") countriesofinterest <- 
					# c(	"CHN")
					#
				c("ABW","AFG","AGO","ALB","AND","ARE","ARG","ARM","ASM","ATA","ATF","ATG","AUS","AUT","AZE","BDI","BEL",
				"BEN","BES","BFA","BGD","BGR","BHR","BHS","BIH","BLM","BLR","BLZ","BMU","BOL","BRA","BRB","BRN","BTN",
				"BVT","BWA","CAF","CAN","CHE","CHL","CHN","CIV","CMR","COD","COG","COK","COL","COM","CPV","CRI","CUB",
				"CUW","CXR","CYM","CYP","CZE","DEU","DJI","DMA","DNK","DOM","DZA","ECU","EGY","ERI","ESH","ESP","EST",
				"ETH","FIN","FJI","FLK","FRA","FRO","FSM","GAB","GBR","GEO","GGY","GHA","GIB","GIN","GLP","GMB","GNB",
				"GNQ","GRC","GRD","GRL","GTM","GUF","GUM","GUY","HKG","HMD","HND","HRV","HTI","HUN","IDN","IMN","IND",
				"IOT","IRL","IRN","IRQ","ISL","ISR","ITA","JAM","JEY","JOR","JPN","KAZ","KEN","KGZ","KHM","KIR","KNA",
				"KOR","KWT","LAO","LBN","LBR","LBY","LCA","LIE","LKA","LSO","LTU","LUX","LVA","MAC","MAF","MAR","MCO",
				"MDA","MDG","MDV","MEX","MHL","MKD","MLI","MLT","MMR","MNE","MNG","MNP","MOZ","MRT","MSR","MTQ","MUS",
				"MWI","MYS","MYT","NAM","NCL","NER","NFK","NGA","NIC","NIU","NLD","NOR","NPL","NRU","NZL","OMN","PAK",
				"PAN","PCN","PER","PHL","PLW","PNG","POL","PRI","PRK","PRT","PRY","PSE","PYF","QAT","REU","ROU","RUS",
				"RWA","SAU","SDN","SEN","SGP","SGS","SHN","SJM","SLB","SLE","SLV","SMR","SOM","SPM","SRB","SSD","STP",
				"SUR","SVK","SVN","SWE","SWZ","SXM","SYC","SYR","TCA","TCD","TGO","THA","TJK","TKL","TKM","TLS","TON",
				"TTO","TUN","TUR","TUV","TZA","UGA","UKR","UMI","URY","USA","UZB","VAT","VCT","VEN","VGB","VIR","VNM",
				"VUT","WLF","WSM","XKO","YEM","ZAF","ZMB","ZWE"	)
				else countriesofinterest <- getallISOspecies(as.character(subset[subset$Quality == quality,]$Species))
				print("there ")
				
				coi 								<- data.frame(matrix(unlist(countriesofinterest), nrow=length(countriesofinterest), byrow=T))
				colnames(coi) 			<- c("iso3")
				coi$quality					<- quality
				print("here again ")
				
	  		write.table(coi,resfile , sep = ",", col.names = colnames(coi),row.names=FALSE)
			}
			coi										<- read.csv(file = resfile, na.strings=c(""," ","NA"))
			
			# only the iso3s that are available in the resulset!
			coi										<- coi %>% filter(iso3 %in% provinces$country_iso)
			provincestmp					<- provincestmp %>% filter(country_iso %in% coi$iso3)
			
			x <-(subset[subset$Quality == quality,]$Species=="ALL")
			print(x)
			print(length(unique(subset[subset$Quality == quality,]$Species)))
			y <-  (length(unique(subset[subset$Quality == quality,]$Species)) == 1)
			print(y)
			z <- (!( x && y))
			print(z)
			r <- (spec_col_identifier %in% params)
			print(z)

			if (z && r) {		
				print("i")
				print(length(coi$iso3))
				print(coi$iso3)
				for (i in 1:length(coi$iso3)){
					iso3 								<- as.character(coi$iso3[i])
					print("k")
					print(length(provinces[provinces$country_iso == iso3,]$province_file_id))
					print(provinces[provinces$country_iso == iso3,]$province_file_id)
					for (k in 1:length(provinces[provinces$country_iso == iso3,]$province_file_id)){
						province_file_id				<- as.numeric(provinces[provinces$country_iso == iso3,]$province_file_id[k]) 
						print(province_file_id)
						province_id				<- as.numeric(provinces[provinces$country_iso == iso3 & provinces$province_file_id == province_file_id,]$province_id) 	
						print(province_id)
						
						provincepcadata					<- provinces %>% filter(country_iso == iso3) %>% filter(province_file_id ==province_file_id) %>% as_tibble()
						print(provincepcadata)
					
						# FIXME DO THIS WITH ALL BUFFER OPTIONS - AT THE MOMENT BUFFER OTIONS ARE DROPPED
						print(paste("Processing these Species buffers",str(buffers)))	
						
						print(length(buffers))
						for (l in (1:length(buffers))){			
									
							provincespecies		<- getsprange(iso3=iso3,lvl=1,province_file_id=province_file_id,buffer=buffers[l],legends=legends,province_id=province_id)				
      				print(paste("received species range for ",as.character(coi$iso3[i]),province_id,province_file_id))
							class(provincespecies)
							str(provincespecies)
							if (is.null(provincespecies 	) || (length(st_geometry(provincespecies)) == 0) )	res_spr											<- 0 else res_spr	<- Reduce("+",st_area(st_cast(provincespecies,"MULTIPOLYGON")))                                      
							if (is.null(res_spr						))	res_spr											<- 0
							if (res_spr>0) {
								spprovsummary <- NULL				
								if (!is.null(subset[subset$Quality == quality,]$Species)) spprovsummary <- getspsummary(sp = provincespecies, species = subset[subset$Quality == quality,]$Species, iso3 = iso3, province_file_id = province_file_id,buffer = buffers[l],legends=legends,province_id=province_id)
								
								print(spprovsummary)
								if(is.null(spprovsummary)) next
								else {
									helpertibble <<- merge(helpertibble,spprovsummary,all=T)
								}
							}
						}
					}
				}
			}
			print("attempting to merge species data into provinces")

			# add this info to the province dataset, instead of NAs just 0s

			# fix
			if (class(helpertibble) == "data.frame") helpertibble <- as.tibble(helpertibble)

			# fix for no species
			if (nrow(helpertibble[,1]) == 0) 	write_csv(provincestmp,qualityfilename)
			else {
				helpertibble  <- helpertibble %>% group_by(country_iso,province_id,province_file_id) %>% summarise_if(is.numeric, sum, na.rm = TRUE)
				print(helpertibble)
				print("helpertibble before map merge")
				merged 				<- left_join(unique(as.data.frame(provincestmp)), as.data.frame(helpertibble),by = c("country_iso" = "country_iso", "province_id" = "province_id","province_file_id" = "province_file_id"))
				index_new_col <- (normcolumns + 1):ncol(merged)
				merged[, index_new_col][is.na(merged[, index_new_col])] <- 0
				provincestmp 	<<- as.tibble(merged)
				print(provincestmp)
				write_csv(provincestmp,qualityfilename)
			}
		}
		
		# GET COMPLETE DATASET
		pca_with_sp						<- read_csv(qualityfilename)

		if (nrow(pca_with_sp[,1]) == 0) {
			warning("No ISO countries in range available. Check input dataset")
			next
		}

		# APPLY RECIPE FILTERS
		filters <- recipes %>% filter(!is.na(Filter) && Recipe == recipe) %>% as.data.frame()
		# filters <- as.data.frame(filters[filters$Recipe == recipe, ])
		print(filters)
		print(nrow(filters))
		pca_with_sp_filtered <- pca_with_sp
		
		print("rows in filter")
		print(nrow(filters)>0)
		if (nrow(filters)>0) {
			print("you should not be here")
			for (p in 1:nrow(filters)){
				which_column <- as.data.frame(filters)$Parameter[p]
				allowed_value <- as.list(strsplit(filters$Filter[p], '|',fixed=TRUE)[[1]])

			  filter_criteria <- lazyeval::interp(~x %in% y, .values = list(x = as.name(which_column), y = unlist(allowed_value)))

			  # filter_ instead of filter
			  pca_with_sp %>%
			    filter_(filter_criteria) -> pca_with_sp_filtered
				print(pca_with_sp_filtered)
			}
		}
		# pca_labels_col <- recipes %>% filter(Include == F && Recipe == recipe && Label == "text") %>% select(Parameter) %>% unique() %>% unlist()
		# pca_color_col <- recipes %>% filter(Include == F && Recipe == recipe && Label == "color") %>% select(Parameter) %>% unique() %>% unlist()
		pca_labels_col 	<- as.character(unique(sort(recipes[recipes$Recipe == recipe & recipes$Label == "text",]$Parameter,na.last = T))[1])
		pca_color_col 	<- as.character(unique(sort(recipes[recipes$Recipe == recipe & recipes$Label == "color",]$Parameter,na.last = T))[1])		
		
		
		print(recipes[recipes$Recipe == recipe & recipes$Label == "color",])
		print(recipes[recipes$Recipe == recipe,])
		print("Checking Configuration Columns")
		print("Label: ")
		print(pca_labels_col)
		print("Color: ")
		print(pca_color_col)
		print("Params: ")
		print(params)
		print("Available Columns: ")
		print(colnames(tmp))
		
		tmp <- pca_with_sp_filtered %>% as.data.frame()
		x <- tmp[,colnames(tmp) %in% params] 
		names.use 	<- colnames(x)
		print("Defined Columns: ")
		print(names.use)
		names.use <- unlist(names.use)
		print("Unlisted: ")
		print(names.use)
	  names.use <- c(names.use, pca_labels_col)
	  names.use <- c(names.use, pca_color_col)		
		names.use <- c(names.use,"province_id")
		names.use <- c(names.use,"province_file_id")
		names.use <- c(names.use,"province_label")
		names.use <- c(names.use,"country_iso")
		print("Unique: ")
		names.use <- unique(names.use)
		print(names.use)
		print(params)
		print(spec_col_identifier)
		print(spec_col_identifier %in% params)
		
		if (spec_col_identifier %in% params) {
			# add the calculated species columns
			print("SPX in list")
			calculated_species_names <- as.character(colnames(as.data.frame(pca_with_sp_filtered)))
			names.use <- c(names.use,calculated_species_names[grep(paste0("^",spec_col_identifier), calculated_species_names)])
			names.use[names.use != spec_col_identifier]
		}
		else warning(paste0("no species for PCA selected. Use ",spec_col_identifier," in the subset parameter configuration."))


		# filter down to the relevant columns
		print("before filter by column")
		print(paste("filtering for ",names.use))
		print(paste("available columns",str(pca_with_sp_filtered)))
		print(names.use)
		pca_with_sp_filtered <- pca_with_sp_filtered[, names.use]



		# create gpkg map for qgis
		if (!is.null(pca_with_sp_filtered) & (length(pca_with_sp_filtered)>0)){
			# WRITE RAW DATA TO GPKG FOR VISUALISATION IN QGIS
			rawmapfilename															<- paste0(rawmapbase,quality,"_",recipe,CACHE_FILE_TYPE)
			clean(rawmapfilename)


			# TEMPORARY FIX NOT NEEDED NOW AS OF WOS SCRIPT
			tmp_iso_counts_provincr<-pca_with_sp_filtered %>%
			  group_by(country_iso) %>%
			  summarise(n = n()) %>%
				as_tibble()

			print(tmp_iso_counts_provincr)

			tmp_iso_counts_gadm1
			print(tmp_iso_counts_gadm1)


			differing_ones <- anti_join(tmp_iso_counts_provincr, tmp_iso_counts_gadm1,by = c("country_iso" = "ISO", "n" = "n"))
			print(differing_ones)


			data_for_row_based_join <- pca_with_sp_filtered %>% filter(!country_iso %in% differing_ones$country_iso)
			data_for_id_based_join  <- pca_with_sp_filtered %>% filter(country_iso %in% differing_ones$country_iso)

			print(data_for_row_based_join)
			print(data_for_id_based_join)

			out1 		<- tmp_shp 			%>% inner_join(data_for_id_based_join, by = c("ID_1" = "province_id", "ISO" = "country_iso")) %>% as.data.frame()
			out2		<- tmp_shp			%>% inner_join(data_for_row_based_join, by = c("unique_id_1" = "province_file_id", "ISO" = "country_iso")) %>% as.data.frame()
			
			# fix to rename
			names(out2)[names(out2)=="province_id"] <- "province_file_id"

			print(str(out1))
			print(str(out2))
			
			comb		<- rbind(out1,out2)
			
			out 		<- as_tibble(comb)

			glimpse(out)
			out 		<- provincesshp %>%	left_join(out)


			# out 		<- provincesshp %>%	inner_join(pca_with_sp_filtered,by = c("ID_1" = "province_id", "ISO" = "country_iso"))
			out %>% st_write(rawmapfilename)


		}
		else print(paste("no data for ",rawmapfilename))

		# make sure values are unique
		pca_with_sp_filtered <- pca_with_sp_filtered %>% group_by(province_label) %>% filter(row_number() == 1)

		
		scaled_pca_with_sp_filtered_tmp <- pca_with_sp_filtered %>% as.data.frame(stringsAsFactors = T)
		scaled_pca_with_sp_filtered_tmp$country_iso <- as.factor(scaled_pca_with_sp_filtered_tmp$country_iso)
				
		scaled_pca_with_sp_filtered <- scaled_pca_with_sp_filtered_tmp %>%
																			mutate_if(is.numeric, funs(scale)) %>%
																			as.data.frame(stringsAsFactors = T)
		print(str(scaled_pca_with_sp_filtered))
		
		scaleinputfile <- paste0(inputbase,quality,"_",recipe,"_scaled.csv")
		if (!(file.exists(scaleinputfile))) write_csv(scaled_pca_with_sp_filtered,scaleinputfile)
  	

		attach(scaled_pca_with_sp_filtered)
		head(scaled_pca_with_sp_filtered)
		print("Doing linear model")


		# CONCLUSION SET 1: INFERENCE
		lmbase <- lm(Publications ~
				province_wos_cons
		)
		print(summary(lmbase))
		print(anova(lmbase))

		plot(province_wos_cons_inferred,province_wos_cons)
		abline(lmbase)

		# CONCLUSION SET II: PUBLICATIONS
		lmpub<- lmer(Publications ~
		+province_pa_area_count
		+province_area_species_count
		+province_pa_area_Iab_II_count
		+province_nightlight
		+country_SP.DYN.LE00.IN
		+log_province_road_area_2
		+(1|country_iso))
		print(summary(lmpub))
		print(anova(lmpub))
		plot(lmpub)

		# CONCLUSION SET 3: FIRECLUSTER SHOWS HUMAN PRESENCE DIFFERENT THAN GHP

		lmfirecluster <- lm(Fire.Clusters ~
				+log_province_road_area_2
				+province_pa_area
				+province_pa_area_Iab_II
		  	+province_nightlight

				## unmanaged PAs have more fire
				## knowledge artefacts
				# +province_num_villages
				# +province_waterbody_area
				# +province_area_species_count
				## nightlight artefact
				# +downscaled_UN.HDI
				## not significant:
				# +province_ghp

		# +(1|country_iso)
		#	+(1|country_continent)
		#	+(1|country_region)
		)
		print(summary(lmfirecluster))
		print(anova(lmfirecluster))
		plot(lmfirecluster)

		lmremoteness <- lm(Remoteness ~
				+log_province_area_m2
				+log_province_road_area_2
				+province_nightlight
				# could also use, but don't like composite variables
				# +province_num_villages
				# +province_ghp

		#	+(1|country_continent)
		#	+(1|country_region)

		)
		print(summary(lmremoteness))
		print(anova(lmremoteness))

		lmuppsala <- lm(province_num_conflict ~
				+country_SP.DYN.LE00.IN
		)
		print(summary(lmuppsala))
		print(anova(lmuppsala))

		detach(scaled_pca_with_sp_filtered)
		stop()
		
		# drop empty values
		print("dropping provinces")
		droppedfile <- paste0(inputbase,quality,"_",recipe,"_dropped_provinces",".csv")
		if (!(file.exists(droppedfile))) write_csv(pca_with_sp_filtered[!complete.cases(pca_with_sp_filtered), ],droppedfile)
		
		print(pca_with_sp_filtered[!complete.cases(pca_with_sp_filtered), ]$province_label)
		pca_with_sp_filtered <- pca_with_sp_filtered[complete.cases(pca_with_sp_filtered), ]
	
		inputfile <- paste0(inputbase,quality,"_",recipe,".csv")
		if (!(file.exists(inputfile))) write_csv(pca_with_sp_filtered,inputfile)

		if (nrow(pca_with_sp_filtered[,1])<2) warning(paste("Zero or one feature remaining. No PCA."))
		else {
			pca_with_sp_filtered_labels 								<- pca_with_sp_filtered[,pca_labels_col]

			pca_with_sp_filtered_labels$province_label  <- pca_with_sp_filtered_labels$province_label
			plotlabels <- lapply(pca_with_sp_filtered_labels$province_label, as.character)

			# set country names as df labels
		  pca_with_sp_filtered 												<- pca_with_sp_filtered %>% as.data.frame
			row.names(pca_with_sp_filtered) 						<- unlist(plotlabels)

			# remove non numeric columns
			nums 																				<- sapply(pca_with_sp_filtered, is.numeric)
			pca_with_sp_filtered 												<- pca_with_sp_filtered[ , nums]

			# remove constants
			pca_with_sp_filtered												<- pca_with_sp_filtered[ , apply(pca_with_sp_filtered, 2, var) != 0]

			# remove factors
			pca_with_sp_filtered												<- pca_with_sp_filtered %>% select(-province_file_id,-province_id)				
			
			# tryCatch({
				# RUN PCA (BASIC PRCOMP)
      	
				plotpcafilename															<- paste0(plotbase,quality,"_",recipe,"_pca.png")
				plotpcalabelfilename												<- paste0(plotbase,quality,"_",recipe,"_pca_label.png")
				
				if (!(file.exists(plotpcafilename))){
					x <- prcomp(pca_with_sp_filtered,scale=TRUE)
					str(x)
					pcafile <- paste0(plotbase,quality,"_",recipe,"_pca.txt")
					pcarotationssfile <- paste0(plotbase,quality,"_",recipe,"_pca_rotation.txt")
					
					pcarest <- as.data.frame(x$x)
					print(str(pcarest))
					
					pcafilebottomleft <- paste0(plotbase,quality,"_",recipe,"_pca_bottom_left.csv")
					pcabottomleft <- pcarest[(pcarest$PC1<0 & pcarest$PC2<0),]
					pcabottomleft$score <- abs(pcabottomleft$PC1)+abs(pcabottomleft$PC2)
					if (!(file.exists(pcafilebottomleft))) write.csv(pcabottomleft, file = pcafilebottomleft)

					pcafilebottomright <- paste0(plotbase,quality,"_",recipe,"_pca_bottom_right.csv")
					pcabottomright <- pcarest[(pcarest$PC1>0 & pcarest$PC2<0),]
					pcabottomright$score <- abs(pcabottomright$PC1)+abs(pcabottomright$PC2)
					if (!(file.exists(pcafilebottomright))) write.csv(pcabottomright, file = pcafilebottomright)

					pcafiletopright <- paste0(plotbase,quality,"_",recipe,"_pca_top_right.csv")
					pcatopright <- pcarest[(pcarest$PC1>0 & pcarest$PC2>0),]
					pcatopright$score <- abs(pcatopright$PC1)+abs(pcatopright$PC2)
					if (!(file.exists(pcafiletopright))) write.csv(pcatopright, file = pcafiletopright)
	
					pcafiletopleft <- paste0(plotbase,quality,"_",recipe,"_pca_top_left.csv")
					pcatopleft <- pcarest[(pcarest$PC1<0 & pcarest$PC2>0),]
					pcatopleft$score <- abs(pcatopleft$PC1)+abs(pcatopleft$PC2)
					if (!(file.exists(pcafiletopleft))) write.csv(pcatopleft, file = pcafiletopleft)		
					
					if (!(file.exists(pcafile))) capture.output(summary(x), file = pcafile)					
					if (!(file.exists(pcarotationssfile))) capture.output(x$rotation, file = pcarotationssfile)
					
					glimpse(pca_with_sp_filtered)	
					print(pca_color_col)
					
					ggsave(autoplot(x,data = pca_with_sp_filtered, colour = pca_color_col,label.size = .5,loadings=TRUE,loadings.colour = 'cornflowerblue',loadings.size = 2,loadings.label = T,loadings.label.size = 2.5,loadings.label.colour = 'cornflowerblue', shape = T,shape.size=.5) + scale_color_viridis(direction=-1),filename=plotpcafilename, dpi=600)
					ggsave(autoplot(x,data = pca_with_sp_filtered, colour = pca_color_col,label.size = .5,loadings=TRUE,loadings.colour = 'cornflowerblue',loadings.size = 2,loadings.label = T,loadings.label.size = 2.5,loadings.label.colour = 'cornflowerblue', shape = FALSE,shape.size=.5)+ scale_color_viridis(direction=-1),filename=plotpcalabelfilename, dpi=600)
				
					#compute standard deviation of each principal 
					std_dev <- x$sdev
					#compute variance
					pr_var <- std_dev^2
					prop_varex <- pr_var/sum(pr_var)
										
					pcarotationfile <- paste0(plotbase,quality,"_",recipe,"_pca_rotation.csv")
					if (!(file.exists(pcarotationfile))) write.csv(x$rotation,pcarotationfile,sep = ",",row.names=FALSE)
					
					pcarotationplot												 <- paste0(plotbase,quality,"_",recipe,"_pca_scree.png")
					#scree plot
					if (!(file.exists(pcarotationplot))) 
					ggsave(plot(prop_varex, xlab = "Principal Component",
					             ylab = "Proportion of Variance Explained",
					             type = "b"),filename=pcarotationplot)
				}
				tryCatch({
				
				# prepare set with no outliers
				print("removing outliers")
				pca_with_sp_filtered_no_outliers						<- pca_with_sp_filtered			
				print(nrow(pca_with_sp_filtered_no_outliers))			
				for (column in colnames(pca_with_sp_filtered_no_outliers)){
					outlierReplace(pca_with_sp_filtered_no_outliers,column, 
						which(pca_with_sp_filtered_no_outliers[,column] %in% c(outlier(pca_with_sp_filtered_no_outliers[,column])), 
						NA))
				}	
				pca_with_sp_filtered_no_outliers						<- pca_with_sp_filtered_no_outliers
				pca_with_sp_filtered_no_outliers 						<- pca_with_sp_filtered_no_outliers[complete.cases(pca_with_sp_filtered_no_outliers), ]			 
      	
				print(nrow(pca_with_sp_filtered_no_outliers))
				
				# RUN NO OUTLIER PCA
				if (nrow(pca_with_sp_filtered_no_outliers)<2) warning(paste("Zero or one feature remaining. No PCA."))
				else{
					pca_with_sp_filtered_no_outliers						<- pca_with_sp_filtered_no_outliers[ , apply(pca_with_sp_filtered_no_outliers, 2, var) != 0]		
					
					print("running outlier PCA")
					plotnooutpcafilename												 <- paste0(plotbase,quality,"_",recipe,"_pca_no_outlier.png")
					inputfilenooutlier <- paste0(inputbase,quality,"_",recipe,"_no_outlier.csv")
					if (!(file.exists(inputfilenooutlier))) write_csv(pca_with_sp_filtered_no_outliers,inputfilenooutlier)
					
					if (!(file.exists(plotnooutpcafilename))){
						x <- prcomp(pca_with_sp_filtered_no_outliers,scale=TRUE)
						str(x)
						pcanooutlierfile <- paste0(plotbase,quality,"_",recipe,"_pca_no_outlier.txt")
						if (!(file.exists(pcanooutlierfile))) capture.output(summary(x), file = pcanooutlierfile)
						ggsave(autoplot(x,data = pca_with_sp_filtered_no_outliers, colour = pca_color_col,label.size = 1.1,loadings=TRUE,loadings.colour = 'red',loadings.label = TRUE,loadings.label.size = 2, shape = FALSE),filename=plotnooutpcafilename)
					}								
				}			
				if (nrow(pca_with_sp_filtered)<10) warning(paste("Too few lines for RDA. Skipping."))
				else{
      		
					# RUN ORDI PCA WITH VEGAN
					plotrdafilename															<- paste0(plotbase,quality,"_",recipe,"_rda.png")
					plotccafilename															<- paste0(plotbase,quality,"_",recipe,"_cca.png")
					plotdcafilename															<- paste0(plotbase,quality,"_",recipe,"_dca.png")
					
								
					if (!(file.exists(plotrdafilename))){
						print("starting rda")
						my.rda <- vegan::rda(pca_with_sp_filtered[,-5],scale=TRUE)
						print(summary(my.rda))
						rdafile <- paste0(plotbase,quality,"_",recipe,"_rda.txt")
						if (!(file.exists(rdafile))) capture.output(summary(my.rda), file = rdafile)
						ggsave(biplot(my.rda),filename=plotrdafilename)
						print("done rda")
						
					}
      		
					if (!(file.exists(plotccafilename))){
						print("starting cca")
						my.cca <- vegan::cca(pca_with_sp_filtered[,-5])
						print(summary(my.cca))
						ccafile <- paste0(plotbase,quality,"_",recipe,"_cca.txt")
						if (!(file.exists(ccafile))) capture.output(summary(my.cca), file = ccafile)
						ggsave(plot(my.cca),filename=plotccafilename)
						print("done cca")

					}
      		
					if (!(file.exists(plotdcafilename))){
						print("starting dca")
						my.dca <- vegan::decorana(pca_with_sp_filtered[,-5])
						print(summary(my.dca))
      		
						dcafile <- paste0(plotbase,quality,"_",recipe,"_dca.txt")
						if (!(file.exists(dcafile))) capture.output(summary(my.dca), file = dcafile)
						ggsave(plot(my.dca),filename=plotdcafilename)
						print("dca")

					}
      		
					# CREATE CLUSTERS
      		
					plotclustfilename														<- paste0(plotbase,quality,"_",recipe,"_clust.png")
					# fulfill max cluster condition
					k <- (nrow(pca_with_sp_filtered) %/% 2) - 1
					if (k>5) k <- 5
					if (!(file.exists(plotclustfilename))){
						y <- fanny(pca_with_sp_filtered[-5], k)
						str(y)
						clustfile <- paste0(clustbase,quality,"_",recipe,".csv")
						if (!(file.exists(clustfile))) write.csv(y$membership,clustfile)
						ggsave(autoplot(y, frame = TRUE,label.size = 2, shape = FALSE),filename=plotclustfilename)
					}	
				}
			} , error = function(e) {
 			})
 		}
	}
}