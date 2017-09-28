# THIS SCRIPT PROCESSES AVAILABLE GLOBAL VECTOR AND RASTER DATA SETS AND CLIPS 
# THEM TO THE PROVINCE LEVEL. IT THUS REQUESTS

# POSSIBLE FEATURES
# + ADD HYDROPOWER DAM CONSTRUCTION
# + ADD DIRECT OSM DATASET SUPPORT

library("raster")
library("tidyverse")
library("sqldf")
library("rgdal")
# install.packages("weathermetrics")
library("weathermetrics")
# https://stackoverflow.com/questions/34333624/trouble-installing-rgdal
# library("devtools")
# install_github("r-spatial/sf")
library("sf")
library("spatialEco")
library("overpass")
library("rwos")
library("countrycode")
library("SearchTrees")
library("osrm")
library("fastcluster")
library("lubridate")
library("RCurl")
library("xml2")

# library("devtools")
# install_github("eblondel/cleangeo")
library("cleangeo")

# directly keep sf!!! skip sf func
sf_extSoftVersion()["lwgeom"]

setwd("/Users/raff/Desktop/project canids/")


#httr:::user_agent(paste("raffaelhickisch@gmail.com",httr:::default_ua()))

provincrresultsfile 		<- "results/provincr_final.csv"
logfile 								<- "results/provincr_final_corr.csv"
firesfile								<- "data/geo/fire_nrt_V1_14389.csv"
firespath								<- "data/geo/fires/"
MAXBUFFCLUST						<- 10000
MAXFIRETOCLUST					<- 300000				
MAXWEEKFIRESTOCLUST			<- 20000					
roadlessfile						<- "data/geo/Global_roadlessareas_valid.gpkg"

# RDLESS				<- st_read(roadlessfile)
# # RDLESS				<- st_make_valid(RDLESS)
# RDLESS				<- st_simplify(RDLESS, preserveTopology = FALSE, dTolerance = 0.0002)
# st_write(RDLESS, "data/geo/Global_roadlessareas_valid.gpkg")

roadlesspath						<- "data/geo/roadless/"
gadmpath								<- "data/geo/GADM/"
climpath								<- "data/geo/WORLDCLIM/"
nightlightsfile 				<- "data/geo/F182013.v4c_web.stable_lights.avg_vis.tif"
ghfpfile								<- "data/geo/GHP/w001001x.adf"	
gasflaresfile						<- "data/geo/gas_flares_2013_NAAC_valid.gpkg"
distancesfile						<- "data/geo/DISTEC2011/w001001x.adf"
gasflarefolder					<- "data/geo/GASFLARES/"
basefolder							<- "data/geo/"
pasource								<- "data/geo/WDPA_May2017-shapefile-polygons.shp"
pageodb									<- "data/geo/WDPA_July2017_Public.gdb"
pageodblayer						<- "WDPA_poly_July2017"
paspath									<- "data/geo/PA/"
villagessource					<- "data/geo/global_settlement_points_v1.01.gpkg"
villagespath						<- "data/geo/villages/"
sprangepath							<- "data/geo/IUCNsprange/"
sprangefile							<- "data/geo/TERR_MAMM_SIMPLE_2.gpkg"
canidrangepath					<- "data/geo/IUCNcanidrange/"
canidrangefile					<- "data/geo/TERR_CANIDS_SIMPLE_2.gpkg"
ged50path								<- "data/geo/ged50/"
ged50csvfile						<- "data/geo/ged171_mod.csv"
#WOS Specific
WOSSTART 								=  1966
WOSEND									=  2016
# NOTS 										<- 	read.csv(file = "data/stats/gathered/world_country_query_nots.csv", header = TRUE, stringsAsFactors = FALSE, encoding="UTF-8")
COUNTRY.LANG			 			<- c("de","ar","fr","en","es","ru","zh")
WOSPATH									<- "data/stats/raw/WOSCOUNTRYQUERIES/"
# OSM SPECIFIC      		
CAPITALCITIES						=  "data/geo/capital_cities_sf.gpkg"
AIRPORTS								=  "data/geo/international_airports_NE_10m_airports.gpkg"
AIRPORTS								=  "data/geo/open_airports_large_medium.gpkg"
SEAPORTS								=  "data/geo/WPI.gpkg"
OSMPATH									<- "data/geo/OSMROUTES/"
COUNTRY_NAME_WILDCARD		<- read.csv(file = "data/stats/clean/country_name_add_dict_wildcard.csv", header = TRUE, stringsAsFactors = FALSE)

# SOME STATIC READS TO START WITH
COUNTRIES 							<- read.csv(file = "data/stats/gathered/world_countries.csv", header = TRUE, stringsAsFactors = FALSE)
PROTECTEDAREAS					<- st_read(dsn=pageodb, layer = pageodblayer)

#WOSSID 									<- wos_authenticate()
#WOSSID
WOSSID									<- "W9Mah6SlImvzaFSnyGB"

# CACHE_FILE_TYPE					<- ".shp"
CACHE_FILE_TYPE					<- ".gpkg"

# GET WORLD CLIMATE DATA
# worldclim								<- raster::getData("worldclim",var="bio",res=2.5, path=climpath)
# worldclim 							<- worldclim[[c(1,12)]]
# names(worldclim) 				<- c("Temp","Prec")
# CLIMATE 								<- worldclim
SCRIPTSTARTTIME 				<- format(Sys.time(), "%a %b %d %X %Y")
TIMER										<- Sys.time()
                    		
waterbodiessqlite				<- "data/geo/worldwaterbodies_simple.sqlite"
waterbodiesgdb					<- "data/geo/worldwaterbodies.gdb"
waterbodiesgdblyr				<- "waterbodies"
waterbodiesshp					<- "data/geo/waterbodies_selected_simple.shp"
waterbodiesgpkg					<- "data/geo/waterbodies_selected_simple.gpkg"
                    		
waterbodiespath					<- "data/geo/WATER/"
WORLDWATERBODIES				<- st_read(waterbodiesgpkg)
# WORLDWATERBODIES				<- WORLDWATERBODIES %>%
# 								 								filter(TYPE %in% 	c("Inland perennial", "Inland intermittent"))
# WORLDWATERBODIES				<- st_make_valid(WORLDWATERBODIES)
# WORLDWATERBODIES				<- st_simplify(WORLDWATERBODIES, preserveTopology = FALSE, dTolerance = 0.01)
# st_write(WORLDWATERBODIES, "data/geo/worldwaterbodies_simple.shp")

COUNTRY_ATTRS1 					<- read.csv(file = "data/stats/clean/RCLS_other_indicators.csv", header = TRUE, stringsAsFactors = FALSE, na.strings=c(""," ","NA"))
COUNTRY_ATTRS2 					<- read.csv(file = "data/stats/clean/RCLS_WorldBank.csv", header = TRUE, stringsAsFactors = FALSE, na.strings=c(""," "))

GADM0SHP_FILE						<- "data/stats/clean/gadm28_adm0.gpkg"
GADM0SHP								<- st_read(GADM0SHP_FILE)
st_geometry(GADM0SHP) 	<- NULL

# LOAD THE RASTER DATASETS
CLIMATE									<- raster::getData("worldclim",var="bio",res=2.5, path=climpath)
TEMP 										<- CLIMATE[[c(1)]]
names(TEMP) 						<- c("TEMP")

CLIMATE									<- raster::getData("worldclim",var="bio",res=2.5, path=climpath)
PRECIP 									<- CLIMATE[[c(12)]]
names(PRECIP) 					<- c("PRECIP")

NIGHTLIGHT							<- raster::raster(nightlightsfile)
NIGHTLIGHT 							<- NIGHTLIGHT[[c(1)]]
names(NIGHTLIGHT) 			<- c("NIGHTLIGHT")

DIST50K									<- raster::raster(distancesfile)
DIST50K 								<- DIST50K[[c(1)]]
names(DIST50K) 					<- c("DIST50K")

GHP2006									<- raster::raster(ghfpfile)
GHP2006 								<- GHP2006[[c(1)]]
names(GHP2006) 					<- c("GHP2006")   


# iso3s <- unique(COUNTRIES$ISO.alpha3.code)
# A POSSIBLE FIX FOR STRANGE COUNTRIES
iso3s <- read.csv(file = "data/stats/clean/iso3s.csv", header = TRUE, stringsAsFactors = FALSE)
iso3s <- iso3s[iso3s$exclude == FALSE,]$iso3
iso3s

wos_search_sci_ssci <- function(sid, query = "",
                       api = "lite",
                       editions = if (api == "lite") c("SCI", "SSCI")) {

  if (api == "lite") {

    ## Editions tags
    editions_str <- paste0("<editions><collection>WOS</collection><edition>",
                           editions,
                           "</edition></editions>",
                           collapse = "\n")

    ## SOAP request
    body <- paste0('<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/"
  xmlns:woksearchlite="http://woksearchlite.v3.wokmws.thomsonreuters.com">
    <soapenv:Header/>
    <soapenv:Body>
    <woksearchlite:search>
    <queryParameters>
    <databaseId>WOS</databaseId>
    <userQuery>', query, '</userQuery>',
    editions_str,
    '<queryLanguage>en</queryLanguage>
    </queryParameters>
    <retrieveParameters>
    <firstRecord>1</firstRecord>
    <count>100</count>
    </retrieveParameters>
    </woksearchlite:search>
    </soapenv:Body>
    </soapenv:Envelope>')

    url <- "http://search.webofknowledge.com/esti/wokmws/ws/WokSearchLite"
  }

  if (api == "premium") {

    body <- paste0('<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/"
  xmlns:woksearchlite="http://woksearch.v3.wokmws.thomsonreuters.com">
    <soapenv:Header/>
    <soapenv:Body>
    <woksearch:search>
    <queryParameters>
    <databaseId>WOS</databaseId>
    <userQuery>', query, '</userQuery>
    <editions>
    <collection>WOS</collection>
    <edition>SCI</edition>
    </editions>
    <queryLanguage>en</queryLanguage>
    </queryParameters>
    <retrieveParameters>
    <firstRecord>1</firstRecord>
    <count>100</count>
    </retrieveParameters>
    </woksearch:search>
    </soapenv:Body>
    </soapenv:Envelope>')

    url <- "http://search.webofknowledge.com/esti/wokmws/ws/WokSearch"
  }


  headers <- c(
    Accept = "multipart/*",
    'Content-Type' = "text/xml; charset=utf-8",
    'Cookie' = paste0("SID=", sid),
    SOAPAction = ""
  )

  h <- RCurl::basicTextGatherer()
  RCurl::curlPerform(
    url = url,
    httpheader = headers,
    postfields = body,
    writefunction = h$update
  )

  resp <- xml2::read_xml(h$value())

  err <- xml2::xml_find_first(resp, xpath = ".//faultstring")
  if (length(err) > 0) {
    stop("Error : ", xml2::xml_text(err))
  }

  results <- as.numeric(xml_text(xml_find_first(resp, xpath = "//return/recordsFound")))
  query_id <- xml_text(xml_find_first(resp, xpath = "//return/queryId"))

  cat(paste0(results, " records found"))

  return(list(sid = sid, results = results, id = query_id))

}
docollectstring 		<- function(country=NULL,province=NULL){
	return(NULL)
}
clipgeocsv 					<- function(shape,geocsv){	
	bb <- bbox(shape)
	minlat <- bb[2,1]
	minlon <- bb[1,1]
	maxlon <- bb[1,2]
	maxlat <- bb[2,2]
	query <- paste("select * from file where latitude BETWEEN ",minlat," AND ",maxlat," and longitude BETWEEN ",minlon," and ",maxlon)
	data <- read.csv.sql(geocsv, 
	    sql = query, eol = "\n")
	data_sf = st_as_sf(data, coords = c("longitude", "latitude"), crs = 4326)
	return(data_sf)
}
# deprecated and only for logging. remove this function
getcountrynames 		<- function(iso){
	return(COUNTRIES[COUNTRIES$ISO.alpha3.code == iso,])
}
getgadmshapes				<- function(iso,lvl=0){
	country_names 										<- getcountrynames(iso3)	
	country = tryCatch({
	    getData("GADM", country = iso3, level = lvl, path=gadmpath)
	}, warning = function(w) {
		Sys.sleep(.1)
		getData("GADM", country = iso3, level = lvl, path=gadmpath)
		log(lineify(lineify(country_names[country_names$Language == "English",],"warning when getting GADM shape"),lvl),logfile)		
	}, error = function(e) {
		Sys.sleep(1)
		log(lineify(lineify(country_names[country_names$Language == "English",],"error when getting GADM shape"),lvl),logfile)		
		(NULL)		
	}, finally = {
	})
	if(!is.null(country)) {
		filename<-paste("data/geo/GADM/",paste(iso,lvl,CACHE_FILE_TYPE,sep=""),sep="")
		if (!file.exists(filename)) {		
			#writeOGR(country, gadmpath, paste(iso,lvl,sep=""), driver="ESRI Shapefile")	
			country <- st_cast(st_simplify(st_as_sf(country), preserveTopology = TRUE, dTolerance = 0.002),"MULTIPOLYGON")		
			st_write(country,filename)
			country <- clgeo_Clean(as(st_read(filename),"Spatial"))
		}	
	}
	return(country)
}
getsfgadmshapes			<- function(iso,lvl=0){
	filename<-paste("data/geo/GADM/",paste(iso,lvl,CACHE_FILE_TYPE,sep=""),sep="")
	s<-st_read(filename)
	# FIXES AN ERROR THAT CAME IN FOR BTN CENTROID WITHOUT PROJ4STR
	ll = "+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
	st_crs(s) = ll
	return(s)
}
lineify 						<- function(line,addon){
	return (paste(paste(line,collapse=";",sep=""),addon,sep=";")) 
}
log 								<- function(line,file){
	write.table(line,col.names=FALSE,row.names=FALSE,file=file,append = TRUE)
}
clean 							<- function(filenames){
	for (filename in filenames) {
		if (file.exists(filename)) file.remove(filename)
	}
	return(NULL)
}
getfiresbb					<- function(shp,iso3,lvl=0,provinceid=NULL){
	firefilename			<- paste(firespath,iso3,lvl,"_",provinceid,"_bb",CACHE_FILE_TYPE,sep="")		
	if (!file.exists(firefilename)) {
		country_fires		<- clipgeocsv(shp,firesfile)
		if (length(country_fires$geom) > 0) 
			st_write(country_fires, firefilename)
		else country_fires <- NULL
	}
	else {
		count <- st_layers(firefilename, options = character(0), do_count = FALSE)
		if (count$features>0) { country_fires 	<- st_read(firefilename) }
		else country_fires <- NULL
	}
	return(country_fires)
}			
getinfires					<- function(shp,iso3,lvl=0,provinceid=NULL){
	firefilename			<- paste(firespath,iso3,lvl,"_",provinceid,"_in",CACHE_FILE_TYPE,sep="")	
	if (!file.exists(firefilename)) {
		points					<- getfiresbb(shp,iso3,lvl,provinceid)
		print("Received Fire Buffer")
		if (!is.null(points)) {
			# pts.poly	<- st_intersection(points,st_as_sf(shp))
			# pts.poly			 	<- point.in.poly(as(points,"Spatial"),shp)
			pts.poly	<- points[!is.na(sp::over(as(points,"Spatial"),as(shp,"SpatialPolygons"))),]
			points_within 	<- NULL
			if (!is.null(pts.poly))
				if (length(pts.poly$geom)>0) {
					points_within 	<- pts.poly
					st_write(points_within, firefilename)
				}
		}
		else {
			points_within 	<- NULL
		}
	}
	else {
		points_within 	<- st_read(firefilename)
	}
	return(points_within)
}
# fix bounding box issue spanning across date islands for USA1_2
mclip 							<- function(shp, bb){
  if(class(bb) == "matrix") b_poly <- as(extent(as.vector(t(bb))), "SpatialPolygons")
		else										b_poly <- as(extent(bb), "SpatialPolygons")
	
	frame							<- st_as_sf(b_poly)
	st_crs(frame)			= 4326
  return(st_intersection(shp,frame))
	
}
readcreatebuff 			<- function(sourcefile,buffer = NULL){
	if(is.null(buffer) || buffer == 0) sourcefilename		<- sourcefile		
	else {															 
		sourcefilename		<- paste0(gsub(CACHE_FILE_TYPE,"",sourcefile,fixed = TRUE),"_buffer_deg_",buffer,CACHE_FILE_TYPE)	
		if (!(file.exists(sourcefilename))) {
			simplefile			<- paste0(gsub(CACHE_FILE_TYPE,"",sourcefile,fixed = TRUE),"_simple",CACHE_FILE_TYPE)	
			if (!(file.exists(simplefile))) {
				simple					<- st_simplify(st_read(sourcefile),preserveTopology = TRUE, dTolerance = 0.1)
				st_write(simple,simplefile)
			}
			bufferedshape		<- st_buffer(st_read(simplefile),buffer)
			st_write(bufferedshape,sourcefilename)
		}
	}
	srcshape 		   	<- st_read(sourcefilename)
}
getbb								<- function(shp,sourcefile,outputpath,iso3,lvl=0,provinceid=NULL,buffer=NULL){
	if(is.null(buffer) || buffer == 0) outputfilename		<- paste(outputpath,iso3,lvl,"_",provinceid,"_bb",CACHE_FILE_TYPE,sep="")		
	else															 outputfilename		<- paste(outputpath,iso3,lvl,"_",provinceid,"_bb","_buffer_deg_",buffer,CACHE_FILE_TYPE,sep="")
	if (!file.exists(outputfilename)) {
		srcshape 		   	<- readcreatebuff(sourcefile,buffer)
		clip						<- mclip(srcshape,bbox(shp))
		st_write(clip, outputfilename)
		clip 						<- st_read(outputfilename)
	}
	else {
		# clip 						<- st_make_valid(st_read(outputfilename))
		clip 						<- st_read(outputfilename)
	}
	# FIX for empty area
	count <- st_layers(outputfilename, options = character(0), do_count = FALSE)
	if (count$features<1) {
		print("here")
		if (length(clip$geom) == 0) {
			print("ret null")
			return(NULL)
		}
		return(NULL)
		
	}
	else return(clip)
}		
getroadless					<- function(shp,iso3,lvl=0,provinceid=NULL){
	# NICETOHAVE GET ROADS (needs to be buffered)
	# <!--
	# This has been generated by the overpass-turbo wizard.
	# The original search was:
	# “highway=* and highway!=footway and highway!=pedestrian and -highway!=path”
	# -->
	# <osm-script output="json" timeout="25">
	# <!-- gather results -->
	# 	<union>
	# 		<query type="way">
	# 		<has-kv k="highway"/>
	# 		<bbox-query {{bbox}}/>
	# 		</query>
	# 		<query type="way">
	# 		<has-kv k="motorway"/>
	# 		<bbox-query {{bbox}}/>
	# 		</query>
	# 		<query type="way">
	# 		<has-kv k="trunk"/>
	# 		<bbox-query {{bbox}}/>
	# 		</query>
	# 		<query type="way">
	# 		<has-kv k="secondary"/>
	# 		<bbox-query {{bbox}}/>
	# 		</query>
	# 		<query type="way">
	# 		<has-kv k="unclassified"/>
	# 		<bbox-query {{bbox}}/>
	# 		</query>
	# 		<query type="way">
	# 		<has-kv k="track"/>
	# 		<bbox-query {{bbox}}/>
	# 		</query>
	# 		<query type="way">
	# 		<has-kv k="service"/>
	# 		<bbox-query {{bbox}}/>
	# 		</query>
	# 		<query type="way">
	# 		<has-kv k="path"/>
	# 		<bbox-query {{bbox}}/>
	# 		</query>
	# 		<query type="way">
	# 		<has-kv k="primary"/>
	# 		<bbox-query {{bbox}}/>
	# 		</query>
	# 		<query type="way">
	# 		<has-kv k="tertiary"/>
	# 		<bbox-query {{bbox}}/>
	# 		</query>
	# 		<query type="way">
	# 		<has-kv k="residential"/>
	# 		<bbox-query {{bbox}}/>
	# 		</query>
	# 		<query type="way">
	# 		<has-kv k="living_street"/>
	# 		<bbox-query {{bbox}}/>
	# 		</query>
	# 	</union>
	# <!-- print results -->
	# <print mode="body"/>
	# <recurse type="down"/>
	# <print mode="skeleton" order="quadtile"/>
	# </osm-script>	
	
	
	roadlessfilename	<- paste(roadlesspath,iso3,lvl,"_",provinceid,"_in",CACHE_FILE_TYPE,sep="")	
	if (!file.exists(roadlessfilename)) {
		print(str(shp))
		roadless				<- getbb(shp,roadlessfile,roadlesspath,iso3,lvl,provinceid)
		if (!is.null(roadless)){
	  	roadless_within <- st_intersection(roadless,st_as_sf(shp))
			st_write(roadless_within, roadlessfilename)
			roadless_within <- st_read(roadlessfilename)				
		}
		else roadless_within <- NULL
	}
	else {
		roadless_within	<- st_read(roadlessfilename)
	}
	return(roadless_within)
}
getpabuffer					<- function(shp,iso3,lvl=0,provinceid=NULL){
	# categories			 	<- c("II","Ia","Ib")
	print("Attempting to read PA buffer")
	pasbuffer					<- paste(paspath,iso3,"0_","bb",CACHE_FILE_TYPE,sep="")
	if (!file.exists(pasbuffer)) {
		print("Creating PA buffer")
	  possibleError 	<- tryCatch(
			pa						<- PROTECTEDAREAS %>% 
											 							filter(ISO3 == iso3) %>% 
																		filter(MARINE == 0),  
																		# filter(IUCN_CAT %in% categories)
	      														error=function(e) e
	  )	
	  if (inherits(possibleError, "error")){
	  	return(pa <- NULL)
	  } 
		
		if (!is.na(sf_extSoftVersion()["lwgeom"])) {
			pa<-st_make_valid(pa)	
		}		

		pa<-st_simplify(pa, preserveTopology = TRUE, dTolerance = 0.002)
		st_write(pa, pasbuffer)
		pa<-st_read(pasbuffer)
		return(pa)
	}
	else return(st_read(pasbuffer))
}
getpas							<- function(shp,iso3,lvl=0,provinceid=NULL){
	pa_within					<- NULL
	pasfile						<- paste(paspath,iso3,lvl,"_",provinceid,"_in",CACHE_FILE_TYPE,sep="")	
	pasbuffer					<- paste(paspath,iso3,"0_","bb",CACHE_FILE_TYPE,sep="")
	if (file.exists(pasfile)) return(st_read(pasfile))
	print("Requesting PA buffer")
	buffer						<- getpabuffer(shp,iso3,lvl,provinceid)	
	if (!is.null(buffer)){
		print("Received PA buffer")
		if (lvl==0) {
			pa_within 			<- buffer	
		  possibleError 	<- tryCatch({
				print("Writing Level 0 PA in file")
				st_write(pa_within, pasfile)
				},
		    error=function(e) e
		  )	
		  if (inherits(possibleError, "error")){
				st_write(st_cast(pa_within,"MULTIPOLYGON"), pasfile)
				pa_within	<- st_read(pasfile)
				log(lineify(country_names[country_names$Language == "English",],"error when writing PA shape"),logfile)		
		  } 
		}			
		else {
			count <- st_layers(pasbuffer, options = character(0), do_count = FALSE)
			if (!is.null(count)){
				if (count$features<1) pa_within	<- NULL 
				else {
				  possibleError 	<- tryCatch({			
						print("Clipping Buffer for in file")	
						pa_within <- st_intersection(buffer,st_as_sf(shp))
						st_write(pa_within, pasfile)
						pa_within			<- st_read(pasfile)	
						},					
						error=function(e) e
				  )	
				  if (inherits(possibleError, "error")){
				  	pa_within <- NULL
				  } 
				}	
			}
			else pa_within <- NULL
		}
	}
	return(pa_within)
}
getvillages					<- function(shp,iso3,lvl=0,provinceid=NULL){
	
	villagesfile			<- paste(villagespath,iso3,lvl,"_",provinceid,"_in",CACHE_FILE_TYPE,sep="")	
	if (!file.exists(villagesfile)) {
		village					<- getbb(shp,villagessource,villagespath,iso3,lvl,provinceid)
		if (!is.null(village)){
	  	village_within	<- st_intersection(village,st_as_sf(shp))
			st_write(village_within, villagesfile)
		}
		else village_within <- NULL
	}
	else {
		village_within	<- st_read(villagesfile)
	}
	return(village_within)
}	
getsprange					<- function(shp,iso3,lvl=0,provinceid=NULL,buffer=NULL){
	if(is.null(buffer) || buffer == 0) sprangefilename	<- paste(sprangepath,iso3,lvl,"_",provinceid,"_in",CACHE_FILE_TYPE,sep="")	
	else 															 sprangefilename	<- paste(sprangepath,iso3,lvl,"_",provinceid,"_in","_buffer_deg_",buffer,CACHE_FILE_TYPE,sep="")
	if (!file.exists(sprangefilename)) {
		sprange				<- getbb(shp,sprangefile,sprangepath,iso3,lvl,provinceid,buffer)
		if (!is.null(sprange)) {	
			print(str(sprange))
	  	sprange_within <- st_intersection(sprange,st_as_sf(shp))
			st_write(sprange_within, sprangefilename)
			sprange_within <- st_read(sprangefilename)
		}
		else sprange_within <- NULL
	}
	else {
		sprange_within	<- st_read(sprangefilename)
	}
	return(sprange_within)
}
getcanidrange				<- function(shp,iso3,lvl=0,provinceid=NULL){
	
	canidrangefilename<- paste(canidrangepath,iso3,lvl,"_",provinceid,"_in",CACHE_FILE_TYPE,sep="")	
	if (!file.exists(canidrangefilename)) {
		canidrange				<- getbb(shp,canidrangefile,canidrangepath,iso3,lvl,provinceid)
		# fix for empty ones
		if (!is.null(canidrange)) {
		  canidrange_within <- st_intersection(canidrange,st_as_sf(shp))
			st_write(canidrange_within, canidrangefilename)
			canidrange_within <- st_read(canidrangefilename)
		}
		else canidrange_within	<- NULL
	}
	else {
		canidrange_within	<- st_read(canidrangefilename)
	}
	return(canidrange_within)
}
getrivers						<- function(shp,iso3,lvl=0,provinceid=NULL){
	
	riversfilename		<- paste(riverspath,iso3,lvl,"_",provinceid,"_in",CACHE_FILE_TYPE,sep="")	
	if (!file.exists(riversfilename)) {
		rivers					<- getbb(shp,riversfile,riverspath,iso3,lvl,provinceid)
	  rivers_within 	<- st_intersection(rivers,st_as_sf(shp))
		st_write(rivers_within, riversfilename)
	}
	else {
		rivers_within		<- st_read(riversfilename)
	}
	return(rivers_within)
}
getged50sbb					<- function(shp,iso3,lvl=0,provinceid=NULL){
	
	ged50filename			<- paste(ged50path,iso3,"0_","bb",CACHE_FILE_TYPE,sep="")		
	if (!file.exists(ged50filename)) {
		country_ged50s		<- clipgeocsv(shp,ged50csvfile)
		st_write(country_ged50s, ged50filename)
	}
	else {
		country_ged50s 	<- st_read(ged50filename)
	}
	# FIX for empty area
	count <- st_layers(ged50filename, options = character(0), do_count = FALSE)
	if (count$features<1) {
		return(NULL)}
	else return(country_ged50s)
}			
getged50						<- function(shp,iso3,lvl=0,provinceid=NULL){
	
	ged50filename	<- paste(ged50path,iso3,lvl,"_",provinceid,"_in",CACHE_FILE_TYPE,sep="")	
	if (!file.exists(ged50filename)) {
		ged50				<- getged50sbb(shp,iso3,lvl,provinceid)
		if (!is.null(ged50)) {
			ged50_within <- st_intersection(ged50,st_as_sf(shp))
			st_write(ged50_within, ged50filename)
		}
		else ged50_within	<- NULL
	}
	else {
		ged50_within	<- st_read(ged50filename)
	}
	return(ged50_within)
}
buildcountryquery 	<- function(iso3,province = NULL,cat=NULL) {	
	query <- ""

	print(paste("DOING COUNTRY",iso3,"PROVINCE",province,"CATEGORY",cat))
	
	country_names <- GADM0SHP %>% as.data.frame() %>% filter(ISO == iso3) %>% select(starts_with("NAME_")) %>% top_n(1) %>% unique() %>% unlist(., use.names=FALSE) %>% droplevels() %>% levels() 
	print(country_names)
	
	for (z in (1:length(country_names))){	
		if (!is.null(country_names[z]) & !is.na(country_names[z]) & tolower(country_names[z]) != "<null>" & !is.null(cleanstr(country_names[z]))) 
			if (query=="")
				query<-paste(query,'(TS = \'',cleanstr(country_names[z]),'\'',sep="")
			else
				query<-paste(query,' OR TS = \'',cleanstr(country_names[z]),'\'',sep="")	
	}
	
	for (lang in COUNTRY.LANG) {
		mtry <- try(countrycode(iso3, "iso3c", paste('country.name.',lang,sep="")))
		if (!inherits(mtry, "try-error")) {
			if (query=="") {
				# SEE: MIGHT NEED CLEANSTRING!				
				query<-paste('(')				
				mtry <- try(countrycode(iso3, "iso3c", "country.name.en"))
				if (!inherits(mtry, "try-error")) if (!(is.na((countrycode(iso3,"iso3c","country.name.en"))))) 	query<-paste(query,'TS = \'',cleanstr(countrycode(iso3,"iso3c","country.name.en")),'\'',sep="")
				
				mtry <- try(countrycode(iso3, "iso3c", "p4.name"))
				if (!inherits(mtry, "try-error")) if (!(is.na((countrycode(iso3,"iso3c","p4.name"))))) 				query<-paste(query,' OR TS = \'',cleanstr(countrycode(iso3, "iso3c", 'p4.name'		)),'\'',sep="")
					
				mtry <- try(countrycode(iso3, "iso3c", "undp.name"))
				if (!inherits(mtry, "try-error")) if (!(is.na((countrycode(iso3,"iso3c","undp.name"))))) 			query<-paste(query,' OR TS = \'',cleanstr(countrycode(iso3, "iso3c", 'undp.name'	)),'\'',sep="")

				mtry <- try(countrycode(iso3, "iso3c", "wb_api.name"))
				if (!inherits(mtry, "try-error")) if (!(is.na((countrycode(iso3,"iso3c","wb_api.name"))))) 		query<-paste(query,' OR TS = \'',cleanstr(countrycode(iso3, "iso3c", 'wb_api.name')),'\'',sep="")

				mtry <- try(countrycode(iso3, "iso3c", "un.name.en"))
				if (!inherits(mtry, "try-error")) if (!(is.na((countrycode(iso3,"iso3c","un.name.en"))))) 		query<-paste(query,' OR TS = \'',cleanstr(countrycode(iso3, "iso3c", 'un.name.en'	)),'\'',sep="")

				if (!(is.na((countrycode(iso3, "iso3c", paste('country.name.',lang,sep="")))))) query<-paste(query,' OR TS = \'',cleanstr(countrycode(iso3, "iso3c", paste('country.name.',lang,sep=""))),'\'',sep="")	
			}
			else 
				if (!(is.na((countrycode(iso3, "iso3c", paste('country.name.',lang,sep="")))))) query<-paste(query,' OR TS = \'',cleanstr(countrycode(iso3, "iso3c", paste('country.name.',lang,sep=""))),'\'',sep="")	
		}
	}
	

	
	query<-paste(query,' )',sep="")
	
	# part II (the ones to be excluded)
	# not_query <- NOTS[NOTS$iso3 == iso3,]
	# if (!is.null(not_query)) query<-paste(query,not_query,sep="")	

	# part III (the province)
	if (!is.null(province[1])){
		province_query <- ""
		for (name in province) {
			# FIXME add and test gsbub <null> below
			if (is.null(name) || is.na(name) || name=="<Null>" || name==" " || name=="" || is.null(name) || gsub("[[:punct:]]", "",name) =="" || gsub("[[:punct:]]", "",name) ==" " || gsub("[[:punct:]]", "",name) =="  " || gsub("[[:punct:]]", "",name) =="NA") next
			if (province_query=="") province_query<-paste(' AND (TS = \'',cleanstr(name),'\' ',sep="")
			else province_query<-paste(province_query,' OR TS = \'',cleanstr(name),'\' ',sep="")
		}
		province_query<-paste(province_query,')',sep="")
		query<-paste(query,province_query)
	}

	biology_query <-  ' AND WC=(\'*ecolog*\' OR \'*environment*\' OR \'*evolution*\' OR \'*biol*\' OR \'Geogra*\' OR \'Zool*\' OR \'Ornitho*\' OR \'Plant*\' OR \'Biodiversity Conservation\')'
	conservation_query <- ' AND WC=\'Biodiversity Conservation\''
	
	# Categories from https://images.webofknowledge.com/WOKRS511B4_1/help/WOS/hp_subject_category_terms_tasca.html

	if	(cat == "biol") query<-paste(query,biology_query,sep="")
	if	(cat == "cons") query<-paste(query,conservation_query,sep="")
	
	if (is.null(province[1])){
		# part IV (the year)
		year_query <- ""
		for (year in WOSCOUNTRYSTART:WOSCOUNTRYEND){
			if (year_query=="") {
				year_query<-paste(' AND (PY = ',year,sep="")
			}
			else 
				year_query<-paste(year_query,' OR PY = ',year,sep="")
		}
		year_query<-paste(year_query,' )',sep="")
		
		query<-paste(query,year_query,sep="")	
	}
	else {
		# part IV (the year)
		year_query <- ""
		for (year in WOSPROVINCESTART:WOSPROVINCEEND){
			if (year_query=="") {
				year_query<-paste(' AND (PY = ',year,sep="")
			}
			else 
				year_query<-paste(year_query,' OR PY = ',year,sep="")
		}
		year_query<-paste(year_query,' )',sep="")
		
		query<-paste(query,year_query,sep="")	
	}
	print(query)
	return(query)
}
runcountryquery 		<- function(query1) {
	res1 = tryCatch({
	    wos_search_sci_ssci(WOSSID,query1)
	}, warning = function(w) {
		Sys.sleep(.1)
		res1 <- try(wos_search_sci_ssci(WOSSID,query1))
		w
	}, error = function(e) {
		Sys.sleep(.5)
		res1 <- try(wos_search_sci_ssci(WOSSID,query1))
		e
	}, finally = {
		Sys.sleep(.5)
		res1 <- try(wos_search_sci_ssci(WOSSID,query1))
	})
}
runwos 							<- function(iso3,lvl=0,provinceID=NULL,provincename = NULL,cat="all"){
	if (as.numeric((Sys.time()-TIMER), units="hours")%/%2 > 0) {
		print("reset WOS ID due to time (2hrs)")
		print(as.numeric((Sys.time()-TIMER), units="hours")%/%2)
		TIMER 																	<<- Sys.time()
		WOSSID 																	<<- wos_authenticate()
		WOSSID
	}
	resfilename	<- 	paste(WOSPATH,iso3,lvl,"_",provinceID,"_wos_country_",cat,".csv",sep="")	
	if (!(file.exists(resfilename))) {
		# # ONLY IF DATA ALREADY AVAILABLE
		# return(0)
		
		query1																	<-	buildcountryquery(iso3,provincename,cat)
		res																			<-	runcountryquery(query1)
		type 																		<- 	"Freq_Occurrence"	            		
		if(is.null(provincename)) provincename 	<- ""		
		if(is.null(res$results)) res$results 		<- NA	
			
		else if (as.integer(res$id) > 2400) {
			print("reset WOS ID due to ID number (>2400)")
			TIMER 																<<- Sys.time()
			WOSSID 																<<- wos_authenticate()
		}
		result																	<- 	data.frame(iso3,provincename[1],type,cat,query1,WOSPROVINCESTART,WOSPROVINCEEND,as.integer(res$results))		
		names(result)														<-c("iso3","province","type","cat","query1","start","end","TOTAL")
		# ONLY WRITE THE NON 0 VALUES
		if(!is.na(res$results) & as.integer(res$results)>0) {
			clean(cbind(paste0(resfilename,"_NA.csv")))
			write.table(result, resfilename , sep = ",", col.names = colnames(result),row.names=FALSE)
		}
		else if(is.na(res$results) | as.integer(res$results)==0) {
			clean(cbind(paste0(resfilename,"_NA.csv")))
			write.table(result, paste0(resfilename,"_NA.csv") , sep = ",", col.names = colnames(result),row.names=FALSE)
		}
	}
	else
		result 																	<-	read.csv(file = resfilename, header = TRUE, stringsAsFactors = FALSE)	
	
	if (is.na(result$TOTAL)) result$TOTAL <- 0
	
	return(result$TOTAL)
}
cleanstr						<- function(str){	
	str<-tolower(str)
	# x<-gsub("XXXXXXX","-",gsub("**","*",gsub(" "," ",gsub("  "," ",gsub(" and "," * ",gsub(" or "," * ",gsub(" y "," * ",gsub("[[:punct:]]", "",gsub("k.","k ",gsub("u.s","us ",gsub("u.s.a","usa ",gsub("l\'","l ",gsub("d\'","d ",gsub("-","XXXXXXX",str,fixed = TRUE),fixed = TRUE),fixed = TRUE),fixed = TRUE),fixed = TRUE),fixed = TRUE)), fixed = TRUE), fixed = TRUE),fixed = TRUE), fixed = TRUE), fixed = TRUE), fixed = TRUE), fixed = TRUE)
	#
	# if (is.null(str) | is.na(str) | x == "" | x == " " | (str=="<NULL>")) return(NULL)
	
	if (grepl("-", str, fixed=TRUE)) 	return(paste0(
									gsub("XXXXXXX","-",gsub("**","*",gsub(" "," ",gsub("  "," ",gsub(" and "," * ",gsub(" or "," * ",gsub(" y "," * ",gsub("[[:punct:]]", "*",gsub("k.","k ",gsub("u.s","us ",gsub("u.s.a","usa ",gsub("l\'","l ",gsub("d\'","d ",gsub("-","XXXXXXX",str,fixed = TRUE),fixed = TRUE),fixed = TRUE),fixed = TRUE),fixed = TRUE),fixed = TRUE)), fixed = TRUE), fixed = TRUE),fixed = TRUE), fixed = TRUE), fixed = TRUE), fixed = TRUE), fixed = TRUE),
									'\' OR TS = \'',
									gsub("XXXXXXX","-",gsub("**","*",gsub(" "," ",gsub("  "," ",gsub(" and "," * ",gsub(" or "," * ",gsub(" y "," * ",gsub("[[:punct:]]", " ",gsub("k.","k ",gsub("u.s","us ",gsub("u.s.a","usa ",gsub("l\'","l ",gsub("d\'","d ",gsub("-","XXXXXXX",str,fixed = TRUE),fixed = TRUE),fixed = TRUE),fixed = TRUE),fixed = TRUE),fixed = TRUE)), fixed = TRUE), fixed = TRUE),fixed = TRUE), fixed = TRUE), fixed = TRUE), fixed = TRUE), fixed = TRUE),
									'\' OR TS = \'',
									gsub("XXXXXXX"," - ",gsub("**","*",gsub(" "," ",gsub("  "," ",gsub(" and "," * ",gsub(" or "," * ",gsub(" y "," * ",gsub("[[:punct:]]", "*",gsub("-","XXXXXXX",str,fixed = TRUE)), fixed = TRUE), fixed = TRUE),fixed = TRUE), fixed = TRUE), fixed = TRUE), fixed = TRUE), fixed = TRUE),
									'\' OR (TS = \'',
									as.list(strsplit(gsub("XXXXXXX","-",gsub("**","*",gsub(" "," ",gsub("  "," ",gsub(" and "," * ",gsub(" or "," * ",gsub(" y "," * ",gsub("[[:punct:]]", "*",gsub("-","XXXXXXX",str,fixed = TRUE)), fixed = TRUE), fixed = TRUE),fixed = TRUE), fixed = TRUE), fixed = TRUE), fixed = TRUE), fixed = TRUE), '-',fixed=TRUE)[[1]])[1],
									'\' AND TS = \'',
									as.list(strsplit(gsub("XXXXXXX","-",gsub("**","*",gsub(" "," ",gsub("  "," ",gsub(" and "," * ",gsub(" or "," * ",gsub(" y "," * ",gsub("[[:punct:]]", "*",gsub("-","XXXXXXX",str,fixed = TRUE)), fixed = TRUE), fixed = TRUE),fixed = TRUE), fixed = TRUE), fixed = TRUE), fixed = TRUE), fixed = TRUE), '-',fixed=TRUE)[[1]])[2],
									'\') OR TS = \'',
									gsub("XXXXXXX"," ",gsub("**","*",gsub(" "," ",gsub("  "," ",gsub(" and "," * ",gsub(" or "," * ",gsub(" y "," * ",gsub("[[:punct:]]", "*",gsub("-","XXXXXXX",str,fixed = TRUE)), fixed = TRUE), fixed = TRUE),fixed = TRUE), fixed = TRUE), fixed = TRUE), fixed = TRUE), fixed = TRUE)
									
									))
	if (grepl(" and ", str, fixed=TRUE)) 	return(paste0(
									gsub("XXXXXXX","-",gsub("**","*",gsub(" "," ",gsub("  "," ",gsub(" and "," * ",gsub(" or "," * ",gsub(" y "," * ",gsub("[[:punct:]]", "*",gsub("k.","k ",gsub("u.s","us ",gsub("u.s.a","usa ",gsub("l\'","l ",gsub("d\'","d ",gsub("-","XXXXXXX",str,fixed = TRUE),fixed = TRUE),fixed = TRUE),fixed = TRUE),fixed = TRUE),fixed = TRUE)), fixed = TRUE), fixed = TRUE),fixed = TRUE), fixed = TRUE), fixed = TRUE), fixed = TRUE), fixed = TRUE),
									'\' OR TS = \'',
									gsub("XXXXXXX","-",gsub("**","*",gsub(" "," ",gsub("  "," ",gsub(" and "," * ",gsub(" or "," * ",gsub(" y "," * ",gsub("[[:punct:]]", " ",gsub("k.","k ",gsub("u.s","us ",gsub("u.s.a","usa ",gsub("l\'","l ",gsub("d\'","d ",gsub("-","XXXXXXX",str,fixed = TRUE),fixed = TRUE),fixed = TRUE),fixed = TRUE),fixed = TRUE),fixed = TRUE)), fixed = TRUE), fixed = TRUE),fixed = TRUE), fixed = TRUE), fixed = TRUE), fixed = TRUE), fixed = TRUE),
									'\' OR TS = \'',
									gsub("XXXXXXX"," - ",gsub("**","*",gsub(" "," ",gsub("  "," ",gsub(" and "," * ",gsub(" or "," * ",gsub(" y "," * ",gsub("[[:punct:]]", "*",gsub("-","XXXXXXX",str,fixed = TRUE)), fixed = TRUE), fixed = TRUE),fixed = TRUE), fixed = TRUE), fixed = TRUE), fixed = TRUE), fixed = TRUE),
									'\' OR (TS = \'',
									as.list(strsplit(gsub("XXXXXXX"," and ",gsub("**","*",gsub(" "," ",gsub("  "," ",gsub(" and "," * ",gsub(" or "," * ",gsub(" y "," * ",gsub("[[:punct:]]", "*",gsub(" and ","XXXXXXX",str,fixed = TRUE)), fixed = TRUE), fixed = TRUE),fixed = TRUE), fixed = TRUE), fixed = TRUE), fixed = TRUE), fixed = TRUE), ' and ',fixed=TRUE)[[1]])[1],
									'\' AND TS = \'',
									as.list(strsplit(gsub("XXXXXXX"," and ",gsub("**","*",gsub(" "," ",gsub("  "," ",gsub(" and "," * ",gsub(" or "," * ",gsub(" y "," * ",gsub("[[:punct:]]", "*",gsub(" and ","XXXXXXX",str,fixed = TRUE)), fixed = TRUE), fixed = TRUE),fixed = TRUE), fixed = TRUE), fixed = TRUE), fixed = TRUE), fixed = TRUE), ' and ',fixed=TRUE)[[1]])[2],
									'\') OR TS = \'',
									gsub("XXXXXXX"," ",gsub("**","*",gsub(" "," ",gsub("  "," ",gsub(" and "," * ",gsub(" or "," * ",gsub(" y "," * ",gsub("[[:punct:]]", "*",gsub("-","XXXXXXX",str,fixed = TRUE)), fixed = TRUE), fixed = TRUE),fixed = TRUE), fixed = TRUE), fixed = TRUE), fixed = TRUE), fixed = TRUE)							
									))
	return(
		paste0(gsub("XXXXXXX","-",gsub("**","*",gsub(" "," ",gsub("  "," ",gsub(" and "," * ",gsub(" or "," * ",gsub(" y "," * ",gsub("[[:punct:]]", "*",gsub("k.","k ",gsub("u.s","us ",gsub("u.s.a","usa ",gsub("l\'","l ",gsub("d\'","d ",gsub("-","XXXXXXX",str,fixed = TRUE),fixed = TRUE),fixed = TRUE),fixed = TRUE),fixed = TRUE),fixed = TRUE)), fixed = TRUE), fixed = TRUE),fixed = TRUE), fixed = TRUE), fixed = TRUE), fixed = TRUE), fixed = TRUE),
		'\' OR TS = \'',
		gsub("XXXXXXX","-",gsub("**","*",gsub(" "," ",gsub("  "," ",gsub(" and "," * ",gsub(" or "," * ",gsub(" y "," * ",gsub("[[:punct:]]", " ",gsub("k.","k ",gsub("u.s","us ",gsub("u.s.a","usa ",gsub("l\'","l ",gsub("d\'","d ",gsub("-","XXXXXXX",str,fixed = TRUE),fixed = TRUE),fixed = TRUE),fixed = TRUE),fixed = TRUE),fixed = TRUE)), fixed = TRUE), fixed = TRUE),fixed = TRUE), fixed = TRUE), fixed = TRUE), fixed = TRUE), fixed = TRUE)
		))
}
getOSMclosest 			<- function(sfpoint,source,caption,iso3,klevel=3,lvl=0,provinceID=NULL){	
	resfilename	<- 	paste(OSMPATH,iso3,lvl,"_",provinceID,"_",caption,"_osmroute",CACHE_FILE_TYPE,sep="")	
	if (!file.exists(resfilename)) {
		target 							<- st_read(source)		
		A 									<- as(target,'Spatial')
		B 									<- as(sfpoint,'Spatial')
		tree 								<- createTree(coordinates(A))
		inds 								<- knnLookup(tree, newdat=coordinates(B), k=klevel)	
		res									<- A[inds[1,],]
		print("OSRM RESULT")
	  res$distmin				<- sapply(osrmTable(src = B, dst = res)$duration, mean)		
		final							<- st_as_sf(res)
		st_write(final,resfilename)
	}
	else {
		final<-st_read(resfilename)
	}
	return(final)
}
getwaterbodybb			<- function(shp,iso3,lvl=0,provinceid=NULL){

	waterbodiesfilename		<- paste(waterbodiespath,iso3,lvl,"_",provinceid,"_bb",CACHE_FILE_TYPE,sep="")		
	if (!file.exists(waterbodiesfilename)) {
		clip						<- mclip(WORLDWATERBODIES,bbox(shp))
		st_write(clip, waterbodiesfilename)
	}
	else {
		clip 						<- st_read(waterbodiesfilename)
	}
	# FIX for empty area
	count <- st_layers(waterbodiesfilename, options = character(0), do_count = FALSE)
	if (count$features<1) {
		return(NULL)}
	else return(clip)
}		
getwaterbodies			<- function(shp,iso3,lvl=0,provinceid=NULL){
	# NICETOHAVE
	#
	# GET WATERWAYS DIRECTLY FROM OSM
	# http://overpass-turbo.eu/
	#
	# USING OVERPASS library("overpass")
	# <!--
	# This has been generated by the overpass-turbo wizard.
	# The original search was:
	# “waterway=*”
	# -->
	# <osm-script output="json" timeout="25">
	#   <!-- gather results -->
	#   <union>
	#     <!-- query part for: “waterway=*” -->
	#     <query type="way">
	#       <has-kv k="waterway"/>
	#       <bbox-query {{bbox}}/>
	#     </query>
	#   </union>
	#   <!-- print results -->
	#   <print mode="body"/>
	#   <recurse type="down"/>
	#   <print mode="skeleton" order="quadtile"/>
	# </osm-script>
	#
		
	waterbodiesfilename	<- paste(waterbodiespath,iso3,lvl,"_",provinceid,"_in",CACHE_FILE_TYPE,sep="")	
	if (!file.exists(waterbodiesfilename)) {
		waterbodies				<- getwaterbodybb(shp,iso3,lvl,provinceid)
		if (!is.null(waterbodies)){
	  	waterbodies_within <- st_intersection(waterbodies,st_as_sf(shp))
			st_write(waterbodies_within, waterbodiesfilename)
			waterbodies_within <- st_read(waterbodiesfilename)		
		}
		else waterbodies_within <- NULL
	}
	else {
		waterbodies_within	<- st_read(waterbodiesfilename)
	}
	return(waterbodies_within)
}
filterattr1 				<- function(x,y,z){COUNTRY_ATTRS1[COUNTRY_ATTRS1$year >= x & COUNTRY_ATTRS1$year <= y & COUNTRY_ATTRS1$ISO2 == countrycode(z,"iso3c","iso2c"),]}
filterattr2 				<- function(x,y,z){COUNTRY_ATTRS2[COUNTRY_ATTRS2$year >= x & COUNTRY_ATTRS2$year <= y & COUNTRY_ATTRS2$iso2c == countrycode(z,"iso3c","iso2c"),]}
medianWithoutNA			<- function(x) {
   median(x[which(!is.na(x))])
}
meanWithoutNA				<- function(x) {
   mean(x[which(!is.na(x))])
}
getclustfire 				<- function(infire_shp,iso3,lvl=0,provinceid=NULL){
	fbufferfilenamecsv							<- paste(firespath,iso3,lvl,"_",provinceid,"_fireclusters.csv",sep="")
	fbufferfilename									<- paste(firespath,iso3,lvl,"_",provinceid,"_fireclusters",CACHE_FILE_TYPE,sep="")					
	if (is.null(infire_shp))				return(NULL)		
	# A FIX FOR VERY MANY FIRES, COD_6
	if (!file.exists(fbufferfilename)){
		if (length(infire_shp$geom)>MAXFIRETOCLUST) {
			print("TOO MANY FIRES TO CLUSTER. TRIMMING FIRES DOWN TO FIT")
			infire_shp			<- as(infire_shp,'Spatial')
			infire_shp			<- st_as_sf(infire_shp[sample(1:length(infire_shp),MAXFIRETOCLUST),])
			print(length(infire_shp$geom))
			print("SELECTED")
		}
	}
	infire_shp$weeknum 							<- as.factor(lubridate::isoweek(ymd(infire_shp$acq_date)))
	infire_shp$yearnum 							<- as.factor(lubridate::isoyear(ymd(infire_shp$acq_date)))
	infire_shp$yearweek							<- as.factor(paste(infire_shp$yearnum,infire_shp$weeknum,sep="-"))
	if (!file.exists(fbufferfilename)){
		if (file.exists(fbufferfilenamecsv)) clean(fbufferfilenamecsv)
		print("BUFFER IS NOT THERE")
		for (yw in unique(infire_shp$yearweek)){				
			week_infires 		<- infire_shp %>% filter(yearweek == yw)	
			print(paste("Processing fire clusters for week",yw))
			if (length(week_infires$geom)<2) {
				if (length(week_infires$geom)<1) NULL
				else {
					if (length(week_infires$geom)>MAXWEEKFIRESTOCLUST) {
						print("TOO MANY FIRES TO CLUSTER FOR THIS WEEK. TRIMMING FIRES DOWN TO FIT")
						week_infires			<- as(week_infires,'Spatial')
						week_infires				<- st_as_sf(week_infires[sample(1:length(week_infires),MAXWEEKFIRESTOCLUST),])
						print(length(week_infires$geom))
						print("SELECTED")
					}
					week_infires_xy 				<- do.call(rbind, unclass(st_geometry(week_infires)))
					firepoints							<-data.frame(week_infires_xy)
					names(firepoints) 			<- c("lon","lat")
					firepoints$group				<-	1
					weeksfires <- firepoints %>% group_by(group) %>% summarize(num_firepoints = n(), centroid_lat = mean(lat, na.rm = T), centroid_lon = mean(lon, na.rm = T),yearweek=yw)
					if (!file.exists(fbufferfilenamecsv)) {
		  		  write.table(weeksfires, fbufferfilenamecsv , sep = ",", col.names = T,row.names=FALSE,append=F)
					}
					else {
			  	  write.table(weeksfires, fbufferfilenamecsv , sep = ",", col.names = F,row.names=FALSE,append=T)
					}
				}
			}				
			else {
				if (length(week_infires$geom)>MAXWEEKFIRESTOCLUST) {
					print("TOO MANY FIRES TO CLUSTER FOR THIS WEEK. TRIMMING FIRES DOWN TO FIT")
					week_infires			<- as(week_infires,'Spatial')
					week_infires				<- st_as_sf(week_infires[sample(1:length(week_infires),MAXWEEKFIRESTOCLUST),])
					print(length(week_infires$geom))
					print("SELECTED")
				}
				week_infires_xy 				<- do.call(rbind, unclass(st_geometry(week_infires)))
				firepoints							<-data.frame(week_infires_xy)
				names(firepoints) 			<- c("lon","lat")
				clusters <- hclust(dist(firepoints[, 1:2]))		
				firepoints$group	 			<- cutree(clusters, h=2)
				firepoints$yearweek			<- yw
				weeksfires <- firepoints %>% group_by(group) %>% summarize(num_firepoints = n(), centroid_lat = mean(lat, na.rm = T), centroid_lon = mean(lon, na.rm = T),yearweek=yw)
				if (!file.exists(fbufferfilenamecsv)) {
			    write.table(weeksfires, fbufferfilenamecsv , sep = ",", col.names = T,row.names=FALSE,append=F)
				}
				else {
			    write.table(weeksfires, fbufferfilenamecsv , sep = ",", col.names = F,row.names=FALSE,append=T)
				}
			}
		}
	}
	if (file.exists(fbufferfilenamecsv)) {
		fcentroids 				<- read.csv(file = fbufferfilenamecsv, header = TRUE, stringsAsFactors = FALSE)
		fcentroids_sf	 		<- st_as_sf(fcentroids, coords = c("centroid_lon", "centroid_lat"), crs = 4326)
		fcentroids_sf$ids	<- as.factor(paste(fcentroids_sf$yearweek,"-",fcentroids_sf$group,sep=""))
		if (!file.exists(fbufferfilename)) st_write(fcentroids_sf,fbufferfilename)
		return(length(unique(fcentroids_sf$ids)))
	}
	else return(0)
}
getMBclosest				<- function(sfpoint,source,caption,iso3,klevel=3,lvl=0,provinceID=NULL){	
	print("GET OSRM CLOSEST")
	resfilename	<- 	paste(OSMPATH,iso3,lvl,"_",provinceID,"_",caption,"_mapbox_route",CACHE_FILE_TYPE,sep="")	
	if (!file.exists(resfilename)) {
		print("FILE NOT AVAILABLE; SO NOW DO ROUTING")
		target 							<- st_read(source)		
		A 									<- as(target,'Spatial')
		B 									<- as(sfpoint,'Spatial')
		tree 								<- createTree(coordinates(A))
		inds 								<- knnLookup(tree, newdat=coordinates(B), k=klevel)	
		res									<- A[inds[1,],]
	  res$distmin				<- sapply(mbTable(src = B, dst = res)$duration, mean)		
		final							<- st_as_sf(res)
		st_write(final,resfilename)
	}
	else {
		print("ALREADY ROUTED. JUST READING THE FILE.")
		final<-st_read(resfilename)
	}
	return(final)
}
mbtestSp 						<- function(x){
  if (class(x) %in% c("SpatialPolygonsDataFrame", "SpatialPointsDataFrame")){
    if (is.na(sp::proj4string(x))){
      stop(
        paste(
          "Your input (", quote(x),
          ") does not have a valid coordinate reference system.", sep=""),
        call. = F)
    }
    return(TRUE)
  }else{
    return(FALSE)
  }
}
mbspToDf 						<- function(x){
  # transform to WGS84
  x <- sp::spTransform(x = x, CRSobj = "+init=epsg:4326")
  # this function takes a SpatialDataFrame and transforms it into a dataframe
  x <- data.frame(id = row.names(x), 
                  lon = round(sp::coordinates(x)[,1],6), 
                  lat = round(sp::coordinates(x)[,2],6), 
                  stringsAsFactors = FALSE)
  return(x)
}
mbrasterToContourPoly <- function(r, nclass = 8, breaks = NULL, mask = NULL){

  rmin <- raster::cellStats(r, min, na.rm = TRUE)
  rmax <- raster::cellStats(r, max, na.rm = TRUE)
  
  # default breaks and nclass
  if(is.null(breaks)){
    breaks <- seq(from = rmin,
                  to = rmax,
                  length.out = (nclass+1))
  }else{
    breaks <- c(rmin, breaks[breaks > rmin & breaks < rmax], rmax)
    breaks <- unique(breaks)
    breaks <- sort(breaks)
    # nclass <- length(breaks)-1
  }
  
  myres <- raster::res(r)[1]
  myproj <- sp::CRS(sp::proj4string(r))
  
  if (is.null(mask)){
    mask <- masker(r)
    maskbuff <- rgeos::gBuffer(mask, byid = FALSE, width = 5 * myres )
    r <- raster::extend(r, maskbuff, value=-1)
  }else{
    mask <- rgeos::gUnaryUnion(mask)
    maskbuff <- rgeos::gBuffer(mask, byid = FALSE, width = 5 * myres )
    r <- raster::mask(r, maskbuff, updatevalue = -1)
    if(rgeos::gWithin(masker(r), mask)){stop("mask should be smaller than r",
                                             call. = FALSE)}
  }
  
  rmin <- min(r[r!=-1])
  rmax <- max(r[r!=-1])
  breaks <- c(rmin, breaks[breaks > rmin & breaks < rmax], rmax)
  breaks <- unique(breaks)
  breaks <- sort(breaks)
  finalBreaks <- breaks
  # zero level problem
  if(breaks[1] <= 0){
    zv <- TRUE
    breaks <- breaks + 1
    r <- r + 1
  }else{
    zv <- FALSE
  }
  
  nclass <- length(breaks)-1
  breaks <- breaks[-(nclass+1)]
  
  r[is.na(r)] <- 0
  
  # test breaks
  if(length(breaks)<2){stop("breaks values do not fit the raster values",
                            call. = FALSE)}
  # build the contour lines
  cl <- raster::rasterToContour(r, levels = breaks)
  cl$level <- as.numeric(as.character(cl$level))
  SPlist <- list()
  SPlevels <- character()
  for (i in cl$level){
    linex <- cl[cl@data$level == i,]
    linex <- linex@lines
    linex <- linex[[1]]
    linex <- linex@Lines
    Plist <- NULL
    Plist <- list()
    for (j in 1:length(linex)){
      x <- linex[[j]]@coords
      x <- sp::Polygon(coords =  x, hole = F)
      x <- sp::Polygons(srl = list(x), ID = j)
      Plist[[j]] <- x
    }
    x <- sp::SpatialPolygons(Srl = Plist)
    x <- rgeos::union(x = x)

    if (class(x) != "SpatialPolygonsDataFrame"){
      x <- sp::SpatialPolygonsDataFrame(Sr = x,
                                        data = data.frame(
                                          level = rep(i, length(x))))
    } else {
      x <- x[x@data$count < 2,]
      x@data <- data.frame(level = rep(i, dim(x)[1]))
    }
    SPlist <- c(SPlist , x@polygons  )
    SPlevels <- c(SPlevels,x@data$level)
  }
  for (i in 1:length(SPlist)){
    SPlist[[i]]@ID <- as.character(i)
  }
  x <- sp::SpatialPolygons(Srl = SPlist, proj4string = myproj)
  x <- sp::SpatialPolygonsDataFrame(Sr = x,
                                    data = data.frame(levels = SPlevels))
  
  bks <- data.frame(b =c(breaks, rmax), t = finalBreaks)
  
  # # manage attributes data of the contour spdf
  # breaks <- c(breaks, rmax)
  x@data <- data.frame(id = paste("id_",row.names(x),sep=""),
                       min = bks[match(x$levels, bks[,1]),2],
                       max = bks[match(x$levels, bks[,1])+1,2],
                       center = NA,
                       stringsAsFactors = FALSE)
  x$center <- (x$min+x$max) / 2
  row.names(x) <- x$id
  
  # clip the contour spdf with the mask
  final <- rgeos::gIntersection(spgeom1 = x, spgeom2 = mask, byid = TRUE,
                                id = row.names(x))
  
  df <- data.frame(id = sapply(methods::slot(final, "polygons"),
                               methods::slot, "ID"))
  row.names(df) <- df$id
  final <- sp::SpatialPolygonsDataFrame(Sr = final, data = df)
  final@data <- data.frame(id = final$id, x[match(final$id, x$id),2:4])
  final@plotOrder <- 1:nrow(final)
  
  # ring correction
  df <- unique(final@data[,2:4])
  df$id <- 1:nrow(df)
  df <- df[order(df$center, decreasing = T),]
  
  z <- rgeos::gIntersection(final[final$center==df[1,3],],
                            final[final$center==df[1,3],], byid = F,
                            id = as.character(df[1,4]))
  for(i in 2:nrow(df)){
    y <- rgeos::gDifference(final[final$center==df[i,3],],
                            final[final$center==df[i-1,3],], byid = F, 
                            id = as.character(df[i,4]))
    z <- rbind(z, y)
  }
  dfx <- data.frame(id = sapply(methods::slot(z, "polygons"), 
                                methods::slot, "ID"))
  row.names(dfx) <- dfx$id
  z <- sp::SpatialPolygonsDataFrame(z, dfx)
  z@data <- df[match(x=z@data$id, table = df$id),c(4,1:3)]
  return(z)
}
mbmasker 						<- function(r){
  xy <- sp::coordinates(r)[which(!is.na(raster::values(r))),]
  i <- grDevices::chull(xy)
  b <- xy[c(i,i[1]),]
  mask <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(b,
                                                                 hole = FALSE)),
                                                ID = "1")),
                              proj4string = sp::CRS(sp::proj4string(r)))
  return(mask)
}
mbrgrid 						<- function(loc, dmax, res){
  boxCoordX <- seq(from = loc[1] - dmax,
                   to = loc[1] + dmax,
                   length.out = res)
  boxCoordY <- seq(from = loc[2] - dmax,
                   to = loc[2] + dmax,
                   length.out = res)
  sgrid <- expand.grid(boxCoordX, boxCoordY)
  idSeq <- seq(1, nrow(sgrid), 1)
  sgrid <- data.frame(ID = idSeq,
                      COORDX = sgrid[, 1],
                      COORDY = sgrid[, 2])
  sgrid <- sp::SpatialPointsDataFrame(coords = sgrid[ , c(2, 3)],
                                      data = sgrid,
                                      proj4string = sp::CRS("+init=epsg:3857"))
  return(sgrid)
}
mbdistTableFormat 	<- function(res, src, dst){
  # extract distance table
  mat <- res$durations
  # From sec to minutes
  mat <- round(mat/(60), 1)
  # NA management
  mat[mat == 357913.94] <- NA
  # col and row names management
  dimnames(mat) <- list(src$id, dst$id)
  return(mat)
}  
mbcoordFormat 			<- function(res, src, dst){
  sources <- data.frame(matrix(unlist(res$sources$location, 
                                      use.names = T), 
                               ncol = 2, byrow = T, 
                               dimnames = list(src$id, c("lon", "lat"))))
  destinations <- data.frame(matrix(unlist(res$destinations$location, 
                                           use.names = T), 
                                    ncol = 2, byrow = T, 
                                    dimnames = list(dst$id, c("lon", "lat"))))
  return(list(sources = sources, destinations = destinations)
  )
}
mbtableLoc 					<- function(loc){
  # Query build
  # tab <- paste(getOption("osrm.server"), "table/v1/", getOption("osrm.profile"), "/polyline(", sep = "")
  # tab <- paste0(tab, gepaf::encodePolyline(loc[,c("lat","lon")]),")")

	
	lc 			<- loc[,c("lat","lon")]
	lc$qp 	<- paste(lc$lon,lc$lat,sep = ",")
	coords 	<- paste(lc$qp, collapse=";")
	
	tab <- paste0("https://api.mapbox.com/directions-matrix/v1/mapbox/driving/",coords)
	
	
  return(tab)
}
mbLimit 						<- function(nSrc, nDst){
  e <- simpleError("The public OSRM API does not allow results with 
  a number of durations higher than 10000")
  if(getOption("osrm.server") == "http://router.project-osrm.org/" & (nSrc*nDst) > 10000){
    stop(e)
  }
}
mbTable 						<- function(loc, src = NULL, dst = NULL){
  tryCatch({
    if (is.null(src)){
      # check if inpout is sp, transform and name columns
      if(mbtestSp(loc)){
        loc <- mbspToDf(x = loc)
      }else{
        names(loc) <- c("id", "lon", "lat")
      }
      
      # Check query size
      mbLimit(nSrc = nrow(loc), nDst = nrow(loc))
      
      # Format
      src <- loc
      dst <- loc

      # Build the query
      req <- mbtableLoc(loc = loc)
    }else{
      # check if inpout is sp, transform and name columns
      if(mbtestSp(src)){
        src <- mbspToDf(x = src)
      }else{
        names(src) <- c("id", "lon", "lat")
      }
      # check if inpout is sp, transform and name columns
      if(mbtestSp(dst)){
        dst <- mbspToDf(x = dst)
      }else{
        names(dst) <- c("id", "lon", "lat")
      }
      
      # Check query size
      mbLimit(nSrc = nrow(src), nDst = nrow(dst))
      
      # Build the query
      loc <- rbind(src, dst)
			
      req <- paste(mbtableLoc(loc = loc),
                   "?sources=", 
                   paste(0:(nrow(src)-1), collapse = ";"), 
                   "&destinations=", 
                   paste(nrow(src):(nrow(loc)-1), collapse = ";"), 
									 "&access_token=pk.eyJ1Ijoic2FuamF5YiIsImEiOiJjaWZzMjMyMGgxNnJrc3BrcjZhM2ZiZHR4In0.0pFYU5tHm1tuFYhTAva1SA",
                   sep="")
			print("current OSRM request")
			print(req)
    }

    
    # Get the result
    resRaw <- RCurl::getURL(utils::URLencode(req), 
                            useragent = "'osrm' R package")
    # Error if URL is too long
    e <- simpleError("Something went wrong. It could be that the URL sent to the OSRM public API is too long. 
         Try to enter less sources or destinations.")
    if(getOption("osrm.server") == "http://router.project-osrm.org/" & resRaw==""){
      stop(e)
    }
    # Parse the results
    res <- jsonlite::fromJSON(resRaw)
    
    # Check results
    e <- simpleError(res$message)
    if(res$code != "Ok"){stop(e)}
    
    # get the distance table
    durations <- mbdistTableFormat(res = res, src = src, dst = dst)
    
    # get the coordinates
    coords <- mbcoordFormat(res = res, src = src, dst = dst)
    
    return(list(durations = durations, 
                sources = coords$sources, 
                destinations = coords$destinations))
  }, error=function(e) {message("osrmTable function returns an error: \n", e)})
  return(NULL)
}
st_erase 						<- function(x, y) st_difference(x, st_union(st_combine(y)))					
getclippedrasterdata  <- function(poly,raster,iso3,lvl=0,provinceID=NULL){
	rescsvfilename				<- paste0(basefolder,names(raster),"/",iso3,lvl,"_",provinceID,"_avg.csv")	
	resrastfilename				<- paste0(basefolder,names(raster),"/",iso3,lvl,"_",provinceID,".grd")
	
	print(resrastfilename)
	
	if (!file.exists(rescsvfilename)) {
		if (!file.exists(resrastfilename)) {
			rast.sub <- crop(raster, extent(poly))
			writeRaster(rast.sub, filename=resrastfilename)
		}
		else {
			rast.sub <- raster(system.file(resrastfilename, package="raster"))
		}		
		extrast <- raster::extract(rast.sub, poly)	
		res <- lapply(extrast, FUN=meanWithoutNA)
    write.table(res, rescsvfilename , sep = ",", col.names = FALSE,row.names=FALSE)
	}	
	else {
		res <- read.csv(file = rescsvfilename, header = FALSE, stringsAsFactors = FALSE)
	}		
	return(res)	
	
}
getshapenoflares		<- function(shape,iso3,lvl=0,provinceID=NULL){
	resfilename						<- paste0(gasflarefolder,iso3,lvl,"_",provinceID,CACHE_FILE_TYPE)	
	print(resfilename)
	
	if (!file.exists(resfilename)) {
		flares							<- getbb(as(shape,"Spatial"),gasflaresfile,gasflarefolder,iso3,lvl,provinceID)
		if (is.null(flares))shape_no_flare <- shape
		else 								shape_no_flare	<- st_erase(shape,flares)
		shape_no_flare			<- st_cast(st_simplify(st_as_sf(shape_no_flare), preserveTopology = TRUE, dTolerance = 0.002),"MULTIPOLYGON")		
		st_write(shape_no_flare,resfilename)	
	}
	else {
		shape_no_flare			<- st_read(resfilename)
	}
	return(shape_no_flare)	
}

# clean up from last run
clean(cbind(logfile))

for (iso3 in iso3s){
	country_names 										<- getcountrynames(iso3)	
	country_dat 											<- getgadmshapes(iso3)	
	
	# if no GADM data skip for now
	if (is.null(country_dat)) {
		log(lineify(country_names[country_names$Language == "English",],"unknown shape"),logfile)		
		next
	}		
	
	# continue gathering relevant data
	country_shape											<- getsfgadmshapes(iso3)
	
	# SAVE TIME, DON'T DO ALL OF THAT
	# country_infires										<- getinfires(country_dat,iso3)
	# country_infire_clust							<- getclustfire(country_infires,iso3)
	# country_roadlss										<- getroadless(country_dat,iso3)
	# country_villgs										<- getvillages(country_dat,iso3)
	# country_waterbodies								<- getwaterbodies(country_dat,iso3)
	# country_cdrngs										<- getcanidrange(country_dat,iso3)
	# country_ged50											<- getged50(country_dat,iso3)			
	# country_sprngs										<- getsprange(country_dat,iso3)
	country_pas												<- getpas(country_dat,iso3)
	
	country_wos												<- runwos(iso3)
	country_wos_biol									<- runwos(iso3,,,,"biol")  
	country_wos_cons									<- runwos(iso3,,,,"cons")                        	
	country_cntrd 										<- st_centroid(country_shape)
	
	# FIXME: UGLY WORKAROUND, BUT NO OTHER SOLUTION. SEEMS TO BE AN LAPPLY ISSUE IN ERROR HANDLING
	
  tmp_cap	 													<- tryCatch(getMBclosest(country_cntrd,CAPITALCITIES,"capitalcities",iso3,3)$distmin,error=function(e) e)	
  if (inherits(tmp_cap, "error"))		country_dist_capital <- NA
	else country_dist_capital 				<- meanWithoutNA(tmp_cap)

	tmp_sp	 													<- tryCatch(getMBclosest(country_cntrd,SEAPORTS,"seaports",iso3,3)$distmin,error=function(e) e)	
	if (inherits(tmp_sp, "error"))		country_dist_seaport <- NA
	else country_dist_seaport 				<- meanWithoutNA(tmp_sp)

	tmp_ap	 													<- tryCatch(getMBclosest(country_cntrd,AIRPORTS,"airports",iso3,3)$distmin,error=function(e) e)	
	if (inherits(tmp_ap, "error"))		country_dist_airport <- NA
	else country_dist_airport 				<- meanWithoutNA(tmp_ap)
	
	# SET SOME NAs to 0 (oil and diamond production and military)
	
	tmp_diamond 											<- medianWithoutNA(filterattr1(2001,2016,iso3)$KIMB.ANN.PROD.VOL			)
	if (is.null(tmp_diamond) 	|| is.na(tmp_diamond)) 	tmp_diamond <- 0
	country_KIMB.ANN.PROD.VOL   			<- tmp_diamond

	tmp_milex													<- medianWithoutNA(filterattr1(1966,2016,iso3)$SIPRI.MILEX.USD.2016		)
	if (is.null(tmp_milex) 		|| is.na(tmp_milex)) 		tmp_milex <- 0
	country_SIPRI.MILEX.USD.2016			<- tmp_milex
	
	tmp_oilprod												<- medianWithoutNA(filterattr1(1966,2016,iso3)$BP.OIL.PROD.TBARR.DAY	)
	if (is.null(tmp_oilprod) 	|| is.na(tmp_oilprod)) 	tmp_oilprod <- 0
	country_BP.OIL.PROD.TBARR.DAY   	<- tmp_oilprod

	country_FH.CL											<- medianWithoutNA(filterattr1(2001,2016,iso3)$FH.CL									)
	country_FH.PR               			<- medianWithoutNA(filterattr1(2001,2016,iso3)$FH.PR               		)
	country_OECD.IEA.CO2.FUEL.MT			<- medianWithoutNA(filterattr1(2001,2016,iso3)$OECD.IEA.CO2.FUEL.MT		)
	country_UN.HDI              			<- medianWithoutNA(filterattr1(2001,2016,iso3)$UN.HDI              		)                                       	
	country_BP.OIL.PROV.RSRV.TMBARR		<- medianWithoutNA(filterattr1(1966,2016,iso3)$BP.OIL.PROV.RSRV.TMBARR)
	country_NY.GDP.MKTP.KD.ZG   			<- medianWithoutNA(filterattr2(2001,2016,iso3)$NY.GDP.MKTP.KD.ZG   		)
	country_NY.GDP.PCAP.PP.CD   			<- medianWithoutNA(filterattr2(2001,2016,iso3)$NY.GDP.PCAP.PP.CD   		)
	country_NY.GNP.PCAP.CD      			<- medianWithoutNA(filterattr2(2001,2016,iso3)$NY.GNP.PCAP.CD      		)
	country_EN.POP.DNST         			<- medianWithoutNA(filterattr2(2001,2016,iso3)$EN.POP.DNST         		)
	country_AG.LND.CREL.HA      			<- medianWithoutNA(filterattr2(2001,2016,iso3)$AG.LND.CREL.HA      		)
	country_IQ.CPA.TRAN.XQ      			<- medianWithoutNA(filterattr2(2001,2016,iso3)$IQ.CPA.TRAN.XQ      		)
	country_SP.URB.TOTL.IN.ZS   			<- medianWithoutNA(filterattr2(2001,2016,iso3)$SP.URB.TOTL.IN.ZS   		)
	country_IT.CEL.SETS.P2						<- medianWithoutNA(filterattr2(1966,2016,iso3)$IT.CEL.SETS.P2					)	
	country_IT.MLT.MAIN.P2						<- medianWithoutNA(filterattr2(1966,2016,iso3)$IT.MLT.MAIN.P2					)	
	
	# HANDLE WB NO DATA
  possibleError <- tryCatch(
    	if (country_IT.CEL.SETS.P2 == 0) 		 country_IT.CEL.SETS.P2  <- NA,
      error=function(e) e
  )
  if (inherits(possibleError, "error")){
		log(lineify(country_names[country_names$Language == "English",],"no WB data"),logfile)			
  	# next
  } 
	
	country_SP.DYN.LE00.IN         		<- medianWithoutNA(filterattr2(1966,2016,iso3)$SP.DYN.LE00.IN         )
	country_SP.POP.GROW            		<- medianWithoutNA(filterattr2(1966,2016,iso3)$SP.POP.GROW            )
	country_region 										<- countrycode(iso3,"iso3c","region")
	country_continent 								<- countrycode(iso3,"iso3c","continent")

	# FIXME DO WE WANT TO SAVE COUNTRY ATTRIBUTES?
	# now do the same thing by province
	province_dat 											<- getgadmshapes(iso3,1)
	
	if (is.null(province_dat)) {
		log(lineify(country_names[country_names$Language == "English",],"unknown province shape, using country level"),logfile)		
		province_dat 				<- country_dat
		province_dat$NAME_1 <- country_dat$NAME_ENGLISH 
		province_dat$ID_1  	<- 1
	}		
	
	for (i in 1:length(unique(province_dat$ID_1))){	
		# FIX FOR DOUBLE IDs		
		
		allprovs <- st_as_sf(province_dat)
			
		province_shapes 					<- allprovs[allprovs$ID_1 == i,]
				
		if (length(province_shapes)>1) {
			province_shape 					<- province_shapes[1,]
			province_shape$geometry	<- NULL
			province_shape$geometry	<- st_simplify(st_cast(st_union(province_shapes$geometry,dissolve=T),"MULTIPOLYGON"), preserveTopology = T, dTolerance = 0.0002)
			print(paste("Merged Province Shape for",province_shape$NAME_1,i))
		}
		else province_shape 			<- province_shapes

		province_name 						<- province_shape$NAME_1
		province_rgeo 						<- as(province_shape,"Spatial")
		
		# OLD WAY
		# province_name 						<- province_dat$NAME_1[i]
		# province_rgeo 						<- province_dat[i,1]
		# province_shape						<- st_as_sf(province_dat[i,1])
		
		print(paste0("Getting fires ",province_name))
		province_infires					<- getinfires(province_rgeo,iso3,1,i)
 		province_infires_clust		<- getclustfire(province_infires,iso3,1,i)
		province_roadlss					<- getroadless(province_rgeo,iso3,1,i)
		province_villgs						<- getvillages(province_rgeo,iso3,1,i)
		province_waterbodies			<- getwaterbodies(province_rgeo,iso3,1,i)
		province_ged50						<- getged50(province_rgeo,iso3,1,i)		
		province_sprngs						<- getsprange(province_rgeo,iso3,1,i)
		province_sprngs_plus1deg	<- getsprange(province_rgeo,iso3,lvl=1,provinceid=i,buffer=1)
		province_sprngs_plus.5deg	<- getsprange(province_rgeo,iso3,lvl=1,provinceid=i,buffer=.5)
		province_sprngs_minus1deg	<- getsprange(province_rgeo,iso3,lvl=1,provinceid=i,buffer=-1)
		province_sprngs_minus.5deg<- getsprange(province_rgeo,iso3,lvl=1,provinceid=i,buffer=-.5)
		province_cdrngs						<- getcanidrange(province_rgeo,iso3,1,i)
		province_pas							<- getpas(province_rgeo,iso3,1,i)	
				
		# CHECK IF THIS FIX ACTUALL WORKS
		province_names						<- as.list(strsplit(paste(paste(province_name,province_shape$VARNAME_1,sep="|"),province_shape$NL_NAME_1,sep="|"), '|',fixed=TRUE)[[1]])		
		province_wos							<- runwos(iso3,1,i,province_names)  
		province_wos_biol					<- runwos(iso3,1,i,province_names,"biol")  
		province_wos_cons					<- runwos(iso3,1,i,province_names,"cons")  
		province_cntrd 						<- st_centroid(province_shape)
				
		# FIXME: UGLY WORKAROUND, BUT NO OTHER SOLUTION. SEEMS TO BE AN LAPPLY ISSUE IN ERROR HANDLING
	  tmp_cap	 													<- tryCatch(getMBclosest(province_cntrd,CAPITALCITIES,"capitalcities",iso3,3,1,i)$distmin,error=function(e) e)	
	  if (inherits(tmp_cap, "error"))		province_dist_capital <- NA
		else province_dist_capital 				<- meanWithoutNA(tmp_cap)

		tmp_sp	 													<- tryCatch(getMBclosest(province_cntrd,SEAPORTS,"seaports",iso3,3,1,i)$distmin,error=function(e) e)	
		if (inherits(tmp_sp, "error"))		province_dist_seaport <- NA
		else province_dist_seaport 				<- meanWithoutNA(tmp_sp)

		tmp_ap	 													<- tryCatch(getMBclosest(province_cntrd,AIRPORTS,"airports",iso3,3,1,i)$distmin,error=function(e) e)	
		if (inherits(tmp_ap, "error"))		province_dist_airport <- NA
		else province_dist_airport 				<- meanWithoutNA(tmp_ap)
				
		# province_worldclim_temp						<- getclim(as(st_cast(province_shape,"MULTIPOLYGON"),'Spatial'),1,iso3,1,i)
		# province_worldclim_precip					<- getclim(as(st_cast(province_shape,"MULTIPOLYGON"),'Spatial'),2,iso3,1,i)
		
		province_temp											<- getclippedrasterdata(as(st_cast(province_shape,"MULTIPOLYGON"),'Spatial'),TEMP,iso3,1,i)
		print("done with temp")
		province_temp_kelvin							<- NA
		
		print(province_temp)
		if (!is.na(province_temp) && !is.null(province_temp))	province_temp_kelvin <- celsius.to.kelvin(as.numeric(province_temp)/10, round = 2)	
		print("done with temp to kelvin")
		print(province_temp_kelvin)
		
		province_precip										<- getclippedrasterdata(as(st_cast(province_shape,"MULTIPOLYGON"),'Spatial'),PRECIP,iso3,1,i)
		province_ghp											<- getclippedrasterdata(as(st_cast(province_shape,"MULTIPOLYGON"),'Spatial'),GHP2006,iso3,1,i)
		province_dist50k									<- getclippedrasterdata(as(st_cast(province_shape,"MULTIPOLYGON"),'Spatial'),DIST50K,iso3,1,i)
		province_shp_excl_gasflares		 		<- getshapenoflares(st_cast(province_shape,"MULTIPOLYGON"),iso3,1,i)
		
		tmp_nl		 												<- tryCatch(getclippedrasterdata(as(st_cast(province_shp_excl_gasflares,"MULTIPOLYGON"),'Spatial'),NIGHTLIGHT,iso3,1,i),error=function(e) e)	
		if (inherits(tmp_nl, "error"))		province_nightlight <- 0
		else province_nightlight 					<- tmp_nl
		
		
		# FIXME: THESE ARE UGLY WORKAROUNDS
		# BUG st_cast_sfc_as_default not available
		
		
		if (is.null(country_shape 		)) res_carea 										<- 0 else res_carea 								<- st_area(country_shape)		                                                 
		print("DONE WITH country_shape             				")	
		if (is.null(province_shape 		)) res_parea 										<- 0 else res_parea 								<- st_area(province_shape)			                                             
		print("DONE WITH province_shape             			")	
		if (is.null(province_roadlss 	)) res_roadless 								<- 0 else res_roadless 							<- Reduce("+",st_area(province_roadlss))                                     
		print("DONE WITH province_roadlss             		")	
		if (is.null(province_roadlss 	)) res_toptenroad 							<- 0 else res_toptenroad 						<- medianWithoutNA(sort(st_area(province_roadlss), decreasing = TRUE)[1:5])	 
		print("DONE WITH province_roadlss             		")	
		if (is.null(province_roadlss 	)) res_roadless_cnt 						<- 0 else res_roadless_cnt 					<- length(province_roadlss$geom)	                                   
		print("DONE WITH province_roadlss_cnt  						")
		if (is.null(province_villgs 	)) res_villages	 								<- 0 else res_villages	 						<- length(province_villgs$geom)		                                           
		print("DONE WITH province_villgs             			")	
		if (is.null(province_ged50 		)) res_ged50										<- 0 else res_ged50									<- length(province_ged50$geom)	                                             
		print("DONE WITH province_ged50             			")	
		if (is.null(province_ged50 		)) res_ged50vict 								<- 0 else res_ged50vict 						<- Reduce("+",province_ged50$best)                                           
		print("DONE WITH province_ged50             			")	
		if (is.null(province_infires 	)) res_fires 										<- 0 else res_fires 								<- length(province_infires$geom)                                             
		print("DONE WITH province_infires             		")	
		if (is.null(province_infires_clust 	)) res_infires_clust 			<- 0 else res_infires_clust 				<- province_infires_clust                                      
		print("DONE WITH province_infires_clust           ")	
		if (is.null(province_waterbodies)) res_wbarea 								<- 0 else res_wbarea 								<- Reduce("+",st_area(province_waterbodies))		                             
		print("DONE WITH province_waterbodies             ")	
		if (is.null(province_waterbodies)) res_wbarea_cnt 						<- 0 else res_wbarea_cnt 						<- length(province_waterbodies$geom)		                             
		print("DONE WITH province_waterbodies_cnt      		")	
		if (is.null(province_pas 			)) res_paarea 									<- 0 else res_paarea 								<- Reduce("+",st_area(st_cast(province_pas,"MULTIPOLYGON")))		                                     
		print("DONE WITH province_pas             				")	
		if (is.null(province_pas 			)) res_pacount 									<- 0 else res_pacount 							<- length(st_cast(province_pas,"MULTIPOLYGON")$geom)                                                 
		print("DONE WITH province_pas             				")	
		
		# JUST Iab and II PAs
		if (is.null(province_pas 		) || (length(province_pas$geom)<1)) {
			res_paarea_Iab_II 																					<- 0
			res_paarea_Iab_II_count 																		<- 0  
		}
		else {
			paarea_Iab_II 																							<- province_pas %>% filter(IUCN_CAT %in% c("II","Ia","Ib"))
			if (is.null(paarea_Iab_II) || (length(paarea_Iab_II$geom)<1)) {
				res_paarea_Iab_II 																				<- 0 
				res_paarea_Iab_II_count																		<- 0
			}	
			else {
				res_paarea_Iab_II 																				<- Reduce("+",st_area(st_cast(paarea_Iab_II,"MULTIPOLYGON")))
				res_paarea_Iab_II_count																		<- length(st_cast(paarea_Iab_II,"MULTIPOLYGON")$geom)  
			}
		}
		print("DONE WITH province_pas_Iab_II") 
			
		if (is.null(province_sprngs 	)) res_spr											<- 0 else res_spr											<- Reduce("+",st_area(st_cast(province_sprngs,"MULTIPOLYGON")))                                      
		print("DONE WITH province_sprngs             ")	
		
		if (is.null(province_sprngs_plus1deg 		)) res_spr_plus1deg		<- 0 else res_spr_plus1deg						<- Reduce("+",st_area(st_cast(province_sprngs_plus1deg,"MULTIPOLYGON")))
		print("DONE WITH province_sprngs_plus1deg             ")
		if (is.null(province_sprngs_plus.5deg 	)) res_spr_plus.5deg	<- 0 else res_spr_plus.5deg						<- Reduce("+",st_area(st_cast(province_sprngs_plus.5deg,"MULTIPOLYGON")))
		print("DONE WITH province_sprngs_plus.5deg             ")
		if (is.null(province_sprngs_minus1deg 	)) res_spr_minus1deg	<- 0 else res_spr_minus1deg						<- Reduce("+",st_area(st_cast(province_sprngs_minus1deg,"MULTIPOLYGON")))
		print("DONE WITH province_sprngs_minus1deg             ")
		if (is.null(province_sprngs_minus.5deg 	)) res_spr_minus.5deg <- 0 else res_spr_minus.5deg					<- Reduce("+",st_area(st_cast(province_sprngs_minus.5deg,"MULTIPOLYGON")))
		print("DONE WITH province_sprngs_minus.5deg             ")
		
		if (is.null(province_sprngs 	)) res_sprcount									<- 0 else res_sprcount								<- length(st_cast(province_sprngs,"MULTIPOLYGON")$geom)                                              
		print("DONE WITH province_sprngs             ")	
		if (is.null(province_cdrngs		)) res_canr 										<- 0 else res_canr 										<- Reduce("+",st_area(st_cast(province_cdrngs,"MULTIPOLYGON")))	                                     
		print("DONE WITH province_cdrngs             ")	
		if (is.null(province_cdrngs		)) res_canrcount 								<- 0 else res_canrcount								<- length(st_cast(province_cdrngs,"MULTIPOLYGON")$geom)                                              
		print("DONE WITH province_cdrngs            ")	
		if (((is.null(province_shp_excl_gasflares))  || length(province_shp_excl_gasflares$geom) == 0))			res_nogasflr	<- 0 else res_nogasflr 								<- Reduce("+",st_area(st_cast(province_shp_excl_gasflares,"MULTIPOLYGON")))	                                     
		print("DONE WITH no_gas_flares             ")	
				
		# FIXME: THESE ARE UGLY WORKAROUNDS
		if (is.null(res_carea 				)) res_carea 		  <- 0
		if (is.null(res_parea 				)) res_parea 		  <- 0
		if (is.null(res_roadless 			)) res_roadless 	<- 0
		if (is.null(res_toptenroad 		)) res_toptenroad <- 0
		if (is.null(res_roadless_cnt	)) res_roadless_cnt	<- 0
		
		if (is.null(res_villages	 		)) res_villages	  <- 0
		if (is.null(res_ged50					)) res_ged50			<- 0
		if (is.null(res_ged50vict 		)) res_ged50vict  <- 0
		if (is.null(res_fires 				)) res_fires 		  <- 0
		if (is.null(res_wbarea 				)) res_wbarea 		<- 0
		if (is.null(res_wbarea_cnt		)) res_wbarea_cnt	<- 0			
		if (is.null(res_paarea 				)) res_paarea 		<- 0
		if (is.null(res_pacount 			)) res_pacount 	  <- 0
		
		if (is.null(res_paarea_Iab_II 		 )) res_paarea_Iab_II 		  <- 0
		if (is.null(res_paarea_Iab_II_count)) res_paarea_Iab_II_count <- 0	
		if (is.null(res_spr_plus1deg	)) 			res_spr_plus1deg	  		<- 0
		if (is.null(res_spr_plus.5deg )) 			res_spr_plus.5deg  			<- 0
		if (is.null(res_spr_minus1deg	)) 			res_spr_minus1deg  			<- 0
		if (is.null(res_spr_minus.5deg)) 			res_spr_minus.5deg 			<- 0
			
		if (is.null(res_spr						)) res_spr				<- 0
		if (is.null(res_canr 					)) res_canr 			<- 0
		if (is.null(res_sprcount			)) res_sprcount		<- 0
		if (is.null(res_canrcount 		)) res_canrcount 	<- 0	
		if (is.null(res_nogasflr 			)) res_nogasflr 	<- 0

		resultdf<-data.frame(c(
			SCRIPTSTARTTIME														  
			,iso3                                       
			,countrycode(iso3, "iso3c", 'country.name') 
			,i                                          
			,province_name
			,paste(province_name,iso3,i)                              
			,res_parea                                  
			,res_roadless                             
			,res_toptenroad   
			,res_roadless_cnt                          
			,res_villages	 	                            
			,res_ged50			                            
			,res_ged50vict 	                            
			,res_fires
			,res_infires_clust
			,res_wbarea 	
			,res_wbarea_cnt		                            
			,res_paarea 		                            
			,res_pacount 	
			,res_paarea_Iab_II 		 
			,res_paarea_Iab_II_count
			,res_spr_plus1deg	
			,res_spr_plus.5deg 
			,res_spr_minus1deg	
			,res_spr_minus.5deg		                            
			,res_spr		
			,res_sprcount                         
			,res_canr 	
			,res_canrcount		                            
			,province_dist_capital                      
			,province_dist_seaport                      
			,province_dist_airport                      
			,province_temp   
			,province_temp_kelvin                 
			,province_precip 
			,province_ghp			
			,province_dist50k	
			,res_nogasflr
			,province_nightlight                
			,province_wos
			,province_wos_biol
			,province_wos_cons
			,country_FH.CL											
			,country_FH.PR               			
			,country_KIMB.ANN.PROD.VOL   				
			,country_UN.HDI              			
			,country_SIPRI.MILEX.USD.2016				
			,country_NY.GDP.MKTP.KD.ZG   			
			,country_NY.GDP.PCAP.PP.CD   			
			,country_NY.GNP.PCAP.CD      			
			,country_EN.POP.DNST         			
			,country_AG.LND.CREL.HA      			
			,country_IQ.CPA.TRAN.XQ      			
			,country_SP.URB.TOTL.IN.ZS   			
			,country_IT.CEL.SETS.P2						
			,country_SP.DYN.LE00.IN         		
			,country_SP.POP.GROW  
			,country_IT.MLT.MAIN.P2		
			,country_OECD.IEA.CO2.FUEL.MT
			,country_BP.OIL.PROD.TBARR.DAY  		
			,country_BP.OIL.PROV.RSRV.TMBARR
			,res_carea       
			,country_wos
			,country_wos_biol
			,country_wos_cons  	
			,country_region 				
			,country_continent 
			)                              
		)	
		names(resultdf) <- c(
			"SCRIPTSTARTTIME"																			
			,"country_iso"                                    
			,"country_name_en"                                
			,"province_id"                                    
			,"province_name"   
			,"province_label"                               
			,"province_area_m2"                                  
			,"province_roadless_area_2"                         
			,"province_5_largest_roadless_mean_m2"     
			,"province_roadless_count"         
			,"province_num_villages"                          
			,"province_num_conflict"                          
			,"province_num_conflict_victims"                  
			,"province_num_fire_points"
			,"province_num_weekly_fire_clusters"
			,"province_waterbody_area"  
			,"province_waterbody_area_count"                    
			,"province_pa_area"                               
			,"province_pa_area_count"  
			,"province_pa_area_Iab_II" 		 
			,"province_pa_area_Iab_II_count"
			,"province_area_speciesranges_m2_plus1deg"	
			,"province_area_speciesranges_m2_plus.5deg" 
			,"province_area_speciesranges_m2_minus1deg"	
			,"province_area_speciesranges_m2_minus.5deg"                       
			,"province_area_speciesranges_m2" 
			,"province_area_species_count" 
			,"province_area_canidranges_m2"    
			,"province_area_canid_count"                                        
			,"province_dist_capital_minutes"                          
			,"province_dist_seaport_minutes"                          
			,"province_dist_airport_minutes"                          
			,"province_temp"  
			,"province_temp_kelvin"                  
			,"province_precip" 
			,"province_ghp"			
			,"province_dist50k"	
			,"res_nogasflr"
			,"province_nightlight"	                     
			,"province_wos" 
			,"province_wos_biol"
			,"province_wos_cons"
			,"country_FH.CL"											
			,"country_FH.PR"              			
			,"country_KIMB.ANN.PROD.VOL"   					
			,"country_UN.HDI"             			
			,"country_SIPRI.MILEX.USD.2016"			
			,"country_NY.GDP.MKTP.KD.ZG"  			
			,"country_NY.GDP.PCAP.PP.CD"			
			,"country_NY.GNP.PCAP.CD"      			
			,"country_EN.POP.DNST"         			
			,"country_AG.LND.CREL.HA"      			
			,"country_IQ.CPA.TRAN.XQ"      			
			,"country_SP.URB.TOTL.IN.ZS"   			
			,"country_IT.CEL.SETS.P2"						
			,"country_SP.DYN.LE00.IN"         		
			,"country_SP.POP.GROW"  
			,"country_IT.MLT.MAIN.P2"
			,"country_OECD.IEA.CO2.FUEL.MT"	
			,"country_BP.OIL.PROD.TBARR.DAY" 		
			,"country_BP.OIL.PROV.RSRV.TMBARR"	
			,"country_area"
			,"country_wos" 
			,"country_wos_biol"
			,"country_wos_cons"
			,"country_region" 	
			,"country_continent"                                   
		)  
			
		if (!file.exists(provincrresultsfile)) write.table(resultdf, provincrresultsfile , sep = ",", col.names = colnames(resultdf),row.names=FALSE)
		else write.table(resultdf,col.names=FALSE,row.names=FALSE,file=provincrresultsfile,sep = "," , append = TRUE)
		
		# TODO calculate overall score
		# TODO attach to SF country shape -> easy plot
	}
	# FIXME by park & maybe also by roadless area
	# FIXME replace all of the province things with subcat pa1 and pr1
}