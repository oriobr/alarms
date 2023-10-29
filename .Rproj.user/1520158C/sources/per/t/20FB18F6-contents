




# Alarm zone sf data  ---------------------

geo_url <- "https://services-eu1.arcgis.com/Vh4Ni8nW38EHDz4d/ArcGIS/rest/services/03112021_Warning_Zones_Targil/FeatureServer/0"
df_geo <- arcpullr::get_spatial_layer(geo_url)
df_geo_sub <- gv(df_geo,c("area_NameLable","Uniq_ID"))


# sf data of cities for those not listed in the previous data ------------------------

Sys.sleep(10)

geo_url_2 <- "https://services-eu1.arcgis.com/Vh4Ni8nW38EHDz4d/ArcGIS/rest/services/ADM_Setl_Insturction_Policy_V/FeatureServer/0"
df_geo_2 <- arcpullr::get_spatial_layer(geo_url_2)
df_geo_2 <- funique(df_geo_2,cols = "SETL_CODE")
df_geo_2_sub <- gv(df_geo_2,c("SETL_NAME"))


# Home Front Command alarm areas ------------------------


cities_mix_url = "https://www.oref.org.il/Shared/Ajax/GetCitiesMix.aspx"

req <- request(cities_mix_url)
resp <- req_perform(req)

df_cities <- 
  resp %>% 
  resp_body_string() %>% 
  jsonlite::fromJSON() %>% 
  qDT()

# Select only those that appear in the first source -----------------------------------------------



str_name_oref <-  df_cities$label 
str_name_geo <- df_geo_sub$area_NameLable
str_match_jw <- stringdist::amatch(str_name_oref,str_name_geo,method = "jw")
str_match_qgram <- stringdist::amatch(str_name_oref,str_name_geo,method = "qgram")

str_match <- 
  fcoalesce(str_match_jw,str_match_qgram)



# Add matching ID
df_cities[,str_match:=str_match]
df_geo_sub %<>% ftransform(str_match = seq_row(df_geo))


# Split the data into those that appear and those that don't

city_name_oref <-  df_cities$str_match 
city_name_geo <- df_geo_sub$str_match


list_names <-  split(city_name_oref,(city_name_oref %in% city_name_geo))
list_df_oref <-  split(df_cities,(city_name_oref %in% city_name_geo))
list_df_geo <-  split(df_geo_sub,(  city_name_geo %in% city_name_oref))




names_in_notin <- function(x) {
  if (all(x %chin% c("FALSE" ,"TRUE"))) {
    fifelse(x =="FALSE","notin","in")
  } else {
    x
  }     
}


names(list_names) %<>% names_in_notin 
names(list_df_oref) %<>% names_in_notin
names(list_df_geo) %<>% names_in_notin

# Create a file of the data that appears in the first source ----------------------------

df_in <- 
  join(list_df_oref$`in`,list_df_geo$`in` ,on = c("str_match"))
df_in %<>% st_as_sf


# Check matches for those who didn't appear in the first source ---------------------------------

df_oref_notin <-  list_df_oref$notin
notin_label <- df_oref_notin$label
str_name_geo2 <- df_geo_2_sub$SETL_NAME
str_match_jw <- stringdist::amatch(notin_label,str_name_geo2,method = "jw")
str_match_qgram <- stringdist::amatch(notin_label,str_name_geo2,method = "qgram")

str_match2 <- 
  fcoalesce(str_match_jw,str_match_qgram)


df_oref_notin[,str_match:= str_match2]
df_geo_2_sub %<>% ftransform(str_match = seq_row(df_geo_2_sub))

#--------------------------

df_notin <- 
  join(df_oref_notin ,df_geo_2_sub ,on = c("str_match"),how = "i")
df_notin %<>% st_as_sf
rm(list = ls(pattern = "df_geo"))
gc()

# Build the full file --------------------------


data_names <- intersect(names(df_in),names(df_notin))
df_in %<>% gv(data_names)
df_notin %<>% gv(data_names)

df_alarms_geo <- rowbind(df_in,df_notin)


#-----------------


qsave(df_alarms_geo,"data/inter/Rdata/df_alarms_geo.qs")


rm(list = ls(pattern = "df|in|str_m"))
