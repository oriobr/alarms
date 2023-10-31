

# fun ----------------------


resp_more_then <- function(resp, min_length) {
  resp_content_length <- resp_header(resp, "Content-Length")  
  resp_content_length <- as.numeric(resp_content_length)
  return(resp_content_length>=min_length)
}


resp_json_as_DT <- function(resp) {
  resp  |> resp_body_string() |> fromJSON() |> qDT()
}



get_alarm_history <- function(days_range, language) {
  
  url_alarms_history <-
    "https://www.oref.org.il//Shared/Ajax/GetAlarmsHistory.aspx"
  
  req <- request(url_alarms_history)
  
  
  resp_list <- 
    foreach(day = days_range) %do%{
      print(day)
      
      query_alarms_history <- 
        list( fromDate = day-1,
              toDate = day,
              lang=language,
              mode=0)
      
      resp <- 
        req %>% 
        req_url_query(!!!query_alarms_history) %>% 
        req_perform()
      resp
    }
  
  more_then_30 <- resp_list %>% sapply(resp_more_then,min_length = 30)
  resp_list %<>% .[more_then_30]
  resp_list %<>%  lapply(resp_json_as_DT)
  
  df_return <-  rowbind(resp_list)
  
  return(df_return)
  
}


#----------------------------


# add Heebo fint ------------------

windowsFonts(Heebo = windowsFont("TT Heebo"))


# Map of the entire threatened area in Israel -----------
# That is, all of Israel except Gaza 

all_area_url <- 
  "https://services-eu1.arcgis.com/Vh4Ni8nW38EHDz4d/ArcGIS/rest/services/ADM_Insturction_Policy_V/FeatureServer/0"


df_all_area <- arcpullr::get_spatial_layer(all_area_url)

sf_use_s2(FALSE)
map_all_area <- 
  df_all_area %>%
  st_geometry() %>% 
  st_buffer(dist = 0.00005) %>%
  st_union()
sf_use_s2(TRUE) 

rm(df_all_area)


# Alarm history  ----------------

day_list <-  seq.Date(as.Date("2023-10-6"),Sys.Date(),by = 1)
df1 <-  get_alarm_history(day_list,language = "he")
df1 %<>% funique()
df_en <-  get_alarm_history(day_list,language = "en")
df_en %<>% .[,.(category_desc_en = category_desc,category) ]
df_en %<>% funique()
df1<- join(df1,df_en,on = "category")
rm(df_en)


# Adjust column formats ----------------------------

df1[, alertDate :=anytime(alertDate) ]
df1[,days_date := anydate(alertDate)  ]
setorder(df1,alertDate)
setnames(df1,"data","loc_name")




# Geographic data loading ------------------
df_alarms_geo <- 
  qread("data/inter/Rdata/df_alarms_geo.qs")


# Polygons to circles ---------------------
df_red_point <- df_alarms_geo 

df_red_point %<>% ftransform(area_size =  st_area(geoms))
df_red_point %<>% ftransform(r =  sqrt(area_size/pi))
st_geometry(df_red_point) %<>% st_centroid()



#-----------------------

df_plot <- 
  join(df1,df_red_point,on =c("loc_name"="label") )
df_plot %<>% st_as_sf(crs = st_crs(df_red_point))

# allow multiple alarms to be detected on the same day
set.seed(456)
df_plot <- st_jitter(df_plot)




# Create map ----------------------------

pal_info <-   wes_palette(name = "FantasticFox1")[c(1,2,4,5)]
pal_info %<>% rev()
pal_info[4] <- "#228B22"

#-------------------------

map_all_area <- st_transform(map_all_area,st_crs(df_red_point))

hamas_is_isis <- 
  "#<span style='color:#0c800f'>**Hamas**</span>is<span style='color:#FF0000'>**ISIS**</span>"


#-------------------------


vec_category_desc_en <-  df_plot$category_desc_en %>% funique()
info_guide <- guide_legend(title = NULL, label.theme = element_text(family = "Heebo", face = "bold", size = 5), keywidth = 0.4, keyheight = 0.4, override.aes = list(alpha = 1))

#-------------------------


p1 <-
  map_all_area %>% ggplot() +
  geom_sf(fill = "black") +
  geom_sf(
    data = df_plot,
    aes(
      size = as.numeric(r),
      color = category_desc_en,
      fill = category_desc_en,
      group = days_date
    ),
    shape = 21,
    alpha = 0.5,
    key_glyph = draw_key_polygon
  ) +
  scale_color_manual(values = (pal_info), aesthetics = c("fill"), breaks = vec_category_desc_en) +
  scale_color_manual(
    values = darken(pal_info, amount = 0.3),
    aesthetics = c("color"), breaks = vec_category_desc_en
  ) +
  labs(subtitle = "Israel under attack") +
  guides(colour = info_guide, fill = info_guide, size = "none") +
  theme_void(base_size = 18, base_family = "Heebo") +
  xlab(hamas_is_isis) +
  theme(
    plot.title = element_markdown(hjust = 0.5, face = "bold", family = "Heebo", margin = unit(c(0, 0, 0, 0), "lines"), padding = unit(c(0.25, 0, 0, 0), "lines")),
    plot.subtitle = element_markdown(hjust = 0.5, face = "bold", family = "Heebo", size = 10, margin = unit(c(0, 0, 0, 0), "lines"), padding = unit(c(0, 0, 0, 0), "lines")),
    axis.title.x.bottom = element_markdown(hjust = 0.5, face = "bold", family = "Heebo"),
    legend.position = c(.3, .035),
    legend.justification = c("right", "bottom")
  )


p1

# Create Animation ----------------------------------------

amin <- 
  p1+
  transition_time(days_date)+
  view_static()+
  labs(title = " <span style='color:#CD3700'>{mday(frame_time)}</span>-{month(frame_time)}-{year(frame_time)}")+
  enter_grow(size = 0.2)+
  exit_fade()




alarms_gif <-
  gganimate::animate(amin,device = 'ragg_png',renderer = gifski_renderer(),start_pause = 7,width = 500,height = 800,res = 200,detail = 3)

# save ----------------
gganimate::anim_save("output/figures/alarms_gif_map.gif",animation = alarms_gif )

alarms_gif 

rm(list = ls())
gc()

