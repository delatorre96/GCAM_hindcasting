library(rgcam)
library(dplyr)
library(ggplot2)
library(stringr)
library(patchwork)
library(tidyr)
library(rmap)
library(sf)

selected_regions <- c( 'USA',  'Brazil', #America
                       'India', 'China', 'Russia','EU-15',  #Eurasia
                       'Africa_Eastern', #Africa
                       'Indonesia', 'Australia_NZ') #Oceania
to_global <- function(df, func = sum) {
  df %>%
    group_by(scenario, year) %>%
    summarize(value = func(value, na.rm = TRUE), .groups = 'drop') %>%
    mutate(region = "global") %>%
    select(scenario, region, year, value)
}
to_global_custom <- function(df, group_cols = c("scenario", "year"), func = sum) {
  df %>%
    group_by(across(all_of(group_cols))) %>%
    summarize(value = func(value, na.rm = TRUE), .groups = 'drop') %>%
    select(any_of(group_cols), value)
}


pathToDbs <- "C:/GCAM_working_group/SUSMIP/gcam-cgs/output"
my_gcamdb_basexdb <- "database_basexdb_SUSMIP"

conn <- localDBConn(pathToDbs, my_gcamdb_basexdb)

myQueryfile  <- "C:/GCAM/Nacho/susmip/susmip_queries.xml"

scenariosAnalyze<-c("Baseline","SUSMIP_SUSTAINABLE")

prj1 <- addScenario(conn = conn, proj = paste0(getwd(),'/myProject.dat'), scenario  = scenariosAnalyze, queryFile = myQueryfile)
queries <- listQueries(prj1)
print(queries)

##############global mean temperature############## 
df_query <- getQuery(prj1, "global mean temperature" )

df_query$scenario <- gsub("SUSMIP_SUSTAINABLE", "Susmip Sustaintable", df_query$scenario)
global_temp <- ggplot(df_query, aes(x = year, y = value, color = scenario)) +
  geom_line(size = 1) +
  geom_point() +
  labs(x = "Year", y = "Degrees Celsius (%change)", color = "Scenario", 
       title = "Global Mean Temperature") +
  theme_minimal() +
  theme(legend.position = c(0.7, 0.2))


##############Electricity generation ##############

df_query <- getQuery(prj1, "elec gen by region (incl CHP)")


df_global <- to_global(df_query)
df_global$scenario <- gsub("SUSMIP_SUSTAINABLE", "Susmip Sustaintable", elegGenByRegion_word$scenario)

eleGeneration <- ggplot(df_global, aes(x = year, y = value, color = scenario)) +
  geom_line(size = 1) +
  geom_point() +
  labs(x = "Year", y = "Quantity (EJ)", color = "Scenario", 
       title = "Production") +
  theme_minimal() +
  theme(legend.position = c(0.7, 0.2))


##############elec prices by sector##############
df_query <- getQuery(prj1, "elec prices by sector" )

df_global <- to_global_custom(df_query, group_cols = c("scenario", "year", "fuel"), func = mean)

df_global$scenario <- gsub("SUSMIP_SUSTAINABLE", "Susmip Sustaintable", df_global$scenario)

elecPriceBySector <- ggplot(df_global, aes(x = year, y = value, fill = fuel)) +
  geom_area() +
  facet_wrap(~scenario, ncol = 1) +
  labs(x = "Year", y = "Electricity prices (1975$/GJ)", fill = "Fuel") +
  theme_minimal()


df_global <- to_global_custom(df_query, group_cols = c("scenario", "year"), func = mean)

df_global$scenario <- gsub("SUSMIP_SUSTAINABLE", "Susmip Sustaintable", df_global$scenario)

elecPrice <- ggplot(df_global, aes(x = year, y = value, color = scenario)) +
  geom_line(size = 1) +
  geom_point() +
  labs(x = "Year", y = "Prices (1975$/GJ)", color = "Scenario", 
       title = "Prices") +
  theme_minimal() +
  theme(legend.position = c(0.7, 0.2))


### Combination
electr_gen_prices <- eleGeneration / elecPrice + 
  plot_annotation(title = "Electricity Sector")
#print(electr_gen_prices)




##############total final energy by region##############
df_query <- getQuery(prj1, "total final energy by region" )
df_global <- to_global_custom(df_query, group_cols = c("scenario", "year"), func = sum)

df_global$scenario <- gsub("SUSMIP_SUSTAINABLE", "Susmip Sustaintable", df_global$scenario)
total_energy <- ggplot(df_global, aes(x = year, y = value, color = scenario)) +
  geom_line(size = 1) +
  geom_point() +
  labs(x = "Year", y = "Energy (EJ)", color = "Scenario", 
       title = "Total final energy") +
  theme_minimal() +
  theme(legend.position = c(0.7, 0.2))
#### By region
df_query <- getQuery(prj1, "total final energy by region" )
df_region <- df_query[df_query$region %in% selected_regions,]
df_region <- df_query %>%
  filter(region %in% selected_regions) %>%
  mutate(
    scenario = gsub("SUSMIP_SUSTAINABLE", 
                    "Susmip Sustaintable", 
                    scenario)
  )

totalenergy_byregion <- ggplot(
  df_region,
  aes(x = year, y = value, color = scenario)
) +
  geom_line(linewidth = 1) +
  geom_point() +
  facet_wrap(~ region, scales = "free_y") +
  labs(
    x = "Year", y = "Energy (EJ)", color = "Scenario", 
    title = "Total final energy by region"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold")
  )
##############final energy prices ##############
# df_query <- getQuery(prj1, "final energy prices" )
# 
# df_global <- to_global_custom(df_query, group_cols = c("scenario", "year", 'fuel'), func = mean)
# df_global$scenario <- gsub("SUSMIP_SUSTAINABLE", "Susmip Sustaintable", df_global$scenario)
# energyPriceBySector <- ggplot(df_global, aes(x = year, y = value, fill = fuel)) +
#   geom_area() +
#   facet_wrap(~scenario, ncol = 1) +
#   labs(x = "Year", y = "Electricity prices", fill = "Fuel") +
#   theme_minimal()


##############consumption by fuel##############
df_query <- getQuery(prj1, "final energy consumption by fuel" )

df_global <- to_global_custom(df_query, group_cols = c("scenario", "year", "input"), func = sum)

 df_global$scenario <- gsub("SUSMIP_SUSTAINABLE", "Susmip Sustaintable", df_global$scenario)
# ggplot(df_global, aes(x = year, y = value, fill = input)) +
#   geom_area() +
#   facet_wrap(~scenario, ncol = 1) +
#   labs(x = "Year", y = "Energy Consumption (EJ)", fill = "Fuel") +
#   theme_minimal()

p_fuel  <- ggplot(df_global, aes(x = factor(year), y = value, fill = input)) +
  geom_col() +
  facet_wrap(~scenario, ncol = 1) +  # Aquí indicamos 1 columna para apilar verticalmente
  labs(x = "Year", y = "Energy Consumption (EJ)", fill = "Fuel") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


##############consumption by sector##############
df_query <- getQuery(prj1, "final energy consumption by sector and fuel" )

df_global <- to_global_custom(df_query, group_cols = c("scenario", "year", "input", "sector"), func = sum)

df_global$scenario <- gsub("SUSMIP_SUSTAINABLE", "Susmip Sustaintable", df_global$scenario)

df_global2 <- df_global %>%
  mutate(
    sector_group = case_when(
      str_detect(sector, "^trn_") ~ "Transport",
      str_detect(sector, "^resid") ~ "Residential",
      str_detect(sector, "^comm") ~ "Commercial",
      str_detect(sector, "industrial|process|cement|steel|aluminum|ammonia|chemical|paper|food|mining|construction|alumina") ~ "Industry",
      str_detect(sector, "water|irrigation|wastewater") ~ "Water",
      str_detect(sector, "agricultural") ~ "Agriculture",
      str_detect(sector, "CO2 removal|dac") ~ "CDR",
      TRUE ~ "Other"
    )
  )
df_sector_summary <- df_global2 %>%
  group_by(scenario, year, sector_group) %>%
  summarize(value = sum(value, na.rm = TRUE), .groups = "drop")

p_sector<- ggplot(df_sector_summary,
       aes(x = factor(year), y = value, fill = sector_group)) +
  geom_col() +
  facet_wrap(~scenario, ncol = 1) +
  labs(
    x = "Year",
    y = "Final energy consumption (EJ)",
    fill = "Sector"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
#### poner consumption by fuel y by sector en misma pagina
p_fuelSector <- p_fuel * p_sector + 
  plot_annotation(title = "Energy consumption by sector and fuel")
#print (p_fuelSector)

############## "water consumption by state, sector, basin" ############## 
df_query <- getQuery(prj1, "water consumption by state, sector, basin"  )
df_global <- to_global_custom(df_query, group_cols = c("scenario", "year"), func = sum)

df_global$scenario <- gsub("SUSMIP_SUSTAINABLE", "Susmip Sustaintable", df_global$scenario)
total_waterconsumption <- ggplot(df_global, aes(x = year, y = value, color = scenario)) +
  geom_line(size = 1) +
  geom_point() +
  labs(x = "Year", y = "Water Consumption (km^3)", color = "Scenario", 
       title = "Total Water Consumption") +
  theme_minimal() +
  theme(legend.position = c(0.7, 0.2))

##### By region
df_query <- getQuery(prj1, "water consumption by state, sector, basin" )
df_region <- df_query[df_query$region %in% selected_regions,]
df_region <- df_query %>%
  filter(region %in% selected_regions) %>%
  mutate(
    scenario = gsub("SUSMIP_SUSTAINABLE", 
                    "Susmip Sustaintable", 
                    scenario)
  )
df_region <- df_region %>%
  group_by(scenario, region, year) %>%
  summarise(
    value = sum(value, na.rm = TRUE),
    .groups = "drop"
  ) 
total_waterConsumption_byregion <- ggplot(
  df_region,
  aes(x = year, y = value, color = scenario)
) +
  geom_line(linewidth = 1) +
  geom_point() +
  facet_wrap(~ region, scales = "free_y") +
  labs(
    x = "Year",
    y = "Water (km^3)",
    color = "Scenario",
    title = "Water Consumption by region"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold")
  )


############## "water withdrawals by state, sector, basin" ############## 

df_query <- getQuery(prj1, "water withdrawals by state, sector, basin"  )
df_global <- to_global_custom(df_query, group_cols = c("scenario", "year", "sector"), func = sum)
df_global <- to_global_custom(df_query, group_cols = c("scenario", "year"), func = sum)
df_global$scenario <- gsub("SUSMIP_SUSTAINABLE", "Susmip Sustaintable", df_global$scenario)
total_waterWithdrawal <- ggplot(df_global, aes(x = year, y = value, color = scenario)) +
  geom_line(size = 1) +
  geom_point() +
  labs(x = "Year", y = "Water Withdrawals (km^3)", color = "Scenario", 
       title = "Total Water Withdrawals") +
  theme_minimal() +
  theme(legend.position = c(0.7, 0.2))

###combined
water_sector <- total_waterconsumption/total_waterWithdrawal + 
  plot_annotation(title = "Water sector")

##### By region
# df_query <- getQuery(prj1,"water withdrawals by state, sector, basin" )
# df_region <- df_query[df_query$region %in% selected_regions,]
# df_region <- df_query %>%
#   filter(region %in% selected_regions) %>%
#   mutate(
#     scenario = gsub("SUSMIP_SUSTAINABLE", 
#                     "Susmip Sustaintable", 
#                     scenario)
#   )
# df_region <- df_region %>%
#   group_by(scenario, region, year) %>%
#   summarise(
#     value = sum(value, na.rm = TRUE),
#     .groups = "drop"
#   ) 
# total_waterWithdrawals_byregion <- ggplot(
#   df_region,
#   aes(x = year, y = value, color = scenario)
# ) +
#   geom_line(linewidth = 1) +
#   geom_point() +
#   facet_wrap(~ region, scales = "free_y") +
#   labs(
#     x = "Year",
#     y = "Water (km^3)",
#     color = "Scenario",
#     title = "Water withdrawals by region"
#   ) +
#   theme_minimal() +
#   theme(
#     legend.position = "bottom",
#     strip.text = element_text(face = "bold")
#   )


df_sum <- df_query %>%
  group_by(scenario, subsector, year) %>%
  summarize(value = sum(value, na.rm = TRUE), .groups = "drop")

# Convertimos el data frame a formato ancho para restar fácilmente
df_wide <- df_sum %>%
  pivot_wider(names_from = scenario, values_from = value)

# Asumiendo que los escenarios se llaman "Baseline" y "Susmip Sustaintable"
df_diff <- df_wide %>%
  mutate(diff = `SUSMIP_SUSTAINABLE` - Baseline) %>%
  select(subsector, year, diff)

# Relativa en porcentaje
df_diff <- df_wide %>%
  mutate(pct_diff = 100 * (`SUSMIP_SUSTAINABLE` - Baseline) / Baseline) %>%
  select(subsector, year, pct_diff)

df_summary <- df_diff %>%
  group_by(subsector) %>%
  summarize(
    avg_diff = mean(pct_diff, na.rm = TRUE),
    median_diff = median(pct_diff, na.rm = TRUE),
    diff_2100 = pct_diff[year == 2100]
  ) %>%
  ungroup()

name_map <- c(
  "AdrBlkSea"    = "Adriatic_Sea_Greece_Black_Sea_Coast",
  "AfrCstE"      = "Africa_East_Central_Coast",
  "AfrCstNE"     = "Africa_Red_Sea_Gulf_of_Aden_Coast",
  "AfrCstNW"     = "Africa_North_West_Coast",
  "AfrCstS"      = "Africa_South_Interior",
  "AfrCstSE"     = "Africa_Indian_Ocean_Coast",
  "AfrCstSSW"    = "South_Africa_South_Coast",  # aprox
  "AfrCstSW"     = "Africa_West_Coast",
  "AfrCstW"      = "Africa_West_Coast",
  "AfrIntN"      = "Africa_North_Interior",
  "AfrIntS"      = "Africa_South_Interior",
  "AmazonR"      = "Amazon",
  "AmuDaryaR"    = "Amu_Darya",
  "AmurR"        = "Amur",
  "AngolaCst"    = "Angola_Coast",
  "ArabianP"     = "Arabian_Peninsula",
  "ArabianSea"   = "Arabian_Sea_Coast",
  "ArcticIsl"    = "Arctic_Ocean_Islands",
  "ArgColoR"     = "South_America_Colorado",  
  "ArgCstN"      = "North_Argentina_South_Atlantic_Coast",    
  "ArgCstS"      = "South_Argentina_South_Atlantic_Coast",  
  "ArkWhtRedR"   = "Arkansas_White_Red",
  "AusCstE"      = "Australia_East_Coast",
  "AusCstN"      = "Australia_North_Coast",
  "AusCstS"      = "Australia_South_Coast",
  "AusCstW"      = "Australia_West_Coast",
  "AusInt"       = "Australia_Interior",
  "BalticSea"    = "Baltic_Sea_Coast",
  "BarentsSea"   = "Russia_Barents_Sea_Coast",
  "BengalBay"    = "Bay_of_Bengal_North_East_Coast",
  "BengalW"      = "Gulf_of_Thailand_Coast", # aproximado
  "BlackSeaN"    = "Black_Sea_North_Coast",
  "BlackSeaS"    = "Black_Sea_South_Coast",
  "BoHai"        = "Bo_Hai_Korean_Bay_North_Coast",
  "BorneoCstN"   = "North_Borneo_Coast",
  "BrahmaniR"    = "Brahamani",
  "BrzCstE"      = "East_Brazil_South_Atlantic_Coast",        
  "BrzCstN"      = "Uruguay_Brazil_South_Atlantic_Coast",     
  "BrzCstS"      = "South_Brazil_South_Atlantic_Coast",      
  "California"   = "California_River",
  "CanAtl"       = "Canada_Atlantic_Coast",     # Aproximación
  "Caribbean"    = "Caribbean",
  "CaspianE"     = "Caspian_Sea_East_Coast",
  "CaspianNE"    = "Caspian_Sea_Coast", 
  "CaspianSW"    = "Caspian_Sea_South_West_Coast",
  "CauveryR"     = "Cauvery",
  "ChaoPhrR"     = "Chao_Phraya",
  "ChileCstN"    = "North_Chile_Pacific_Coast",
  "ChileCstS"    = "South_Chile_Pacific_Coast",
  "ChinaCst"     = "China_Coast",
  "ChurchillR"   = "Churchill",
  "CntAmer"      = "Southern_Central_America",            # Aproximación
  "ColEcuaCst"   = "Colombia_Ecuador_Pacific_Coast",
  "CongoR"       = "Congo",
  "DanubeR"      = "Danube",
  "DaugavaR"     = "Daugava",
  "DeadSea"      = "Dead_Sea",
  "DnieperR"     = "Dnieper",
  "DniesterR"    = "Dniester",
  "DnkGrmCst"    = "Denmark_Germany_Coast",
  "DonR"         = "Don",
  "DouroR"       = "Douro",
  "DvinaRN"      = "Northern_Dvina",
  "EJrdnSyr"     = "Eastern_Jordan_Syria",
  "EbroR"        = "Ebro",
  "ElbeR"        = "Elbe",
  "EmsWeserR"    = "Ems_Weser",
  "EngWales"     = "England_and_Wales",
  "FarahrudR"    = "Farahrud",
  "Finland"      = "Finland",
  "FlyR"         = "Fly",
  "FranceCstS"   = "France_South_Coast",
  "FranceCstW"   = "France_West_Coast",
  "FraserR"      = "Fraser",
  "GangesR"      = "Ganges_Bramaputra",
  "Gironde"      = "Gironde",
  "Gobi"         = "Gobi_Interior",
  "GodavariR"    = "Godavari",
  "GreatBasin"   = "Great",                 
  "GreatLakes"   = "Great_Lakes",
  "GrijUsuR"     = "Grijalva_Usumacinta",
  "GuadalqR"     = "Guadalquivir",
  "GuadianaR"    = "Guadiana",
  "GuineaGulf"   = "Gulf_of_Guinea",
  "Hainan"       = "Hainan",
  "HamuMashR"    = "Hamun_i_Mashkel",
  "Hawaii"       = "Hawaii",
  "Helmand"      = "Helmand",
  "Hong"         = "Hong_Red_River",
  "HuangHeR"     = "Huang_He",
  "HudsonBay"    = "Hudson_Bay_Coast",
  "IberiaCst"    = "Spain_Portugal_Atlantic_Coast",              
  "Iceland"      = "Iceland",
  "IdnE"         = "Irian_Jaya_Coast",
  "IndCstE"      = "India_East_Coast",
  "IndCstNE"     = "India_North_East_Coast",
  "IndCstS"      = "India_South_Coast",
  "IndCstW"      = "India_West_Coast",
  "IndusR"       = "Indus",
  "Iran"         = "Central_Iran",
  "Ireland"      = "Ireland",
  "IrianJaya"    = "Irian_Jaya_Coast",
  "IrrawaddyR"   = "Irrawaddy",
  "ItalyCstE"    = "Italy_East_Coast",
  "ItalyCstW"    = "Italy_West_Coast",
  "Japan"        = "Japan",
  "JavaTimor"    = "Java_Timor",
  "Kalimantan"   = "Kalimantan",
  "KaraSea"      = "Kara_Sea_Coast",
  "Korea"        = "North_and_South_Korea",
  "KrishnaR"     = "Krishna",
  "LBalkash"     = "Lake_Balkash",
  "LChad"        = "Lake_Chad",
  "LaPuna"       = "La_Puna_Region",
  "LenaR"        = "Lena",
  "LimpopoR"     = "Limpopo",
  "LoireR"       = "Loire",
  "Mackenzie"    = "Mackenzie",
  "Madagascar"   = "Madagascar",
  "MagdalenaR"   = "Magdalena",
  "MahanadiR"    = "Mahanadi",
  "MahiR"        = "Mahi",
  "MalaysiaP"    = "Peninsula_Malaysia",
  "MarChiq"      = "Mar_Chiquita",
  "MeditE"       = "Mediterranean_Sea_East_Coast",
  "MeditIsl"     = "Mediterranean_Sea_Islands",
  "MeditS"       = "Mediterranean_South_Coast",
  "Mekong"       = "Mekong",
  "MexBaja"      = "Baja_California",
  "MexCstNW"     = "Mexico_Northwest_Coast",
  "MexCstW"      = "Mexico_West_Coast",
  "MexGulf"      = "Mexico_Gulf",               # Aproximación
  "MexInt"       = "Mexico_Interior",
  "MissouriR"    = "Missouri_River",
  "MissppRN"     = "Upper_Mississippi",
  "MissppRS"     = "Lower_Mississippi_River",
  "MurrayDrlg"   = "Murray_Darling",
  "NWTerr"       = "Northwest_Territories",
  "NarmadaR"     = "Narmada",
  "NarvaR"       = "Narva",
  "NegroR"       = "Negro",
  "NelsonR"      = "Saskatchewan_Nelson",
  "NemanR"       = "Neman",
  "NevaR"        = "Neva",
  "NewCaledn"    = "South_Pacific_Islands",              
  "NewZealand"   = "New_Zealand",
  "NigerR"       = "Niger",
  "NileR"        = "Nile",
  "ObR"         = "Ob",
  "OderR"       = "Oder",
  "OhioR"       = "Ohio_River",
  "OrangeR"     = "Orange",
  "OrinocoR"    = "Orinoco",
  "PacArctic"   = "Pacific_and_Arctic_Coast",
  "Pampas"      = "Pampas_Region",
  "Papaloapan"  = "Papaloapan",
  "PapuaCst"    = "Papua_New_Guinea_Coast",
  "ParnaibaR"   = "Parnaiba",
  "Patagonia"   = "Central_Patagonia_Highlands", 
  "PennarR"     = "Pennar",
  "PersianGulf" = "Persian_Gulf_Coast",
  "PeruCst"     = "Peru_Pacific_Coast",
  "Phlppns"    = "Philippines",
  "PoR"        = "Po",
  "PolandCst"  = "Poland_Coast",
  "RedSeaE"    = "Red_Sea_East_Coast",
  "RhineR"     = "Rhine",
  "RhoneR"     = "Rhone",
  "RiftValley" = "Rift_Valley",
  "RioBalsas"  = "Rio_Balsas",
  "RioGrande"  = "Rio_Grande_River",
  "RioLaPlata" = "La_Plata",
  "RioLerma"   = "Rio_Lerma",
  "RioVerde"   = "Rio_Verde",
  "RusCstSE"   = "Russia_South_East_Coast",
  "SAmerCstN"  = "Northeast_South_America_South_Atlantic_Coast",
  "SAmerCstNE" = "North_Brazil_South_Atlantic_Coast",
  "SChinaSea"  = "South_China_Sea_Coast",
  "SabarmatiR" = "Sabarmati",
  "Salinas"    = "Salinas_Grandes",
  "Salween"    = "Salween",
  "SaoFrancR"  = "Sao_Francisco",
  "ScheldtR"   = "Scheldt",
  "ScndnvN"    = "Scandinavia_North_Coast",
  "Scotland"   = "Scotland",
  "SeineR"     = "Seine",
  "SenegalR"   = "Senegal",
  "SepikR"     = "Sepik",
  "ShebJubR"   = "Shebelli_Juba",
  "SiberiaN"   = "Siberia_North_Coast",
  "SiberiaW"   = "Siberia_West_Coast",
  "SinaiP"     = "Sinai_Peninsula",
  "SittaungR"  = "Sittang",
  "SolomonIsl" = "Solomon_Islands",
  "SpainCstSE" = "Spain_South_and_East_Coast",
  "SriLanka"   = "Sri_Lanka",
  "StLwrncR"   = "St_Lawrence",
  "Sulawesi"   = "Sulawesi",
  "Sumatra"   = "Sumatra",
  "Sweden"    = "Sweden",
  "SyrDaryaR" = "Syr_Darya",
  "TagusR"    = "Tagus",
  "Taiwan"    = "Taiwan",
  "TaptiR"    = "Tapti",
  "Tarim"     = "Tarim_Interior",
  "Tasmania"  = "Tasmania",
  "Tehuantpc" = "Isthmus_of_Tehuantepec",
  "TennR"     = "Tennessee_River",
  "TexasCst"  = "Texas_Gulf_Coast",
  "ThaiGulf"  = "Gulf_of_Thailand_Coast",
  "TiberR"   = "Tiber",
  "Tibet"    = "Plateau_of_Tibet_Interior",
  "TigrEuphR"= "Tigris_Euphrates",
  "TocantinsR"= "Tocantins",
  "UralR"    = "Ural",
  "UsaColoRN"= "Upper_Colorado_River",
  "UsaColoRS"= "Lower_Colorado_River",
  "UsaCstE"  = "Atlantic_Ocean_Seaboard",      
  "UsaCstNE" = "Mid_Atlantic",
  "UsaCstSE" = "South_Atlantic_Gulf",
  "UsaPacNW" = "Pacific_Northwest",
  "VietnamCst"= "Viet_Nam_Coast",
  "VolgaR"    = "Volga",
  "VoltaR"    = "Volta",
  "WislaR"    = "Wisla",
  "XunJiang"  = "Xun_Jiang",
  "Yangtze"   = "Yangtze",
  "YeniseiR"  = "Yenisey",
  "YucatanP"  = "Yucatan_Peninsula",
  "ZambeziR"  = "Zambezi",
  "ZiyaHe"    = "Ziya_He_Interior"
)

df_summary$subRegion <- name_map[df_summary$subsector]

df_map <- dplyr::left_join(mapGCAMBasins, df_summary, by = "subRegion")

p1 <- ggplot(df_map) +
  geom_sf(aes(fill = diff_2100)) +
  scale_fill_viridis_c(option = "plasma", na.value = "grey90") +
  labs(title = "Difference in year 2100") +
  theme_minimal()

p2 <- ggplot(df_map) +
  geom_sf(aes(fill = avg_diff)) +
  scale_fill_viridis_c(option = "plasma", na.value = "grey90") +
  labs(title = "Average difference") +
  theme_minimal()

map_waterWithdrawals <- p1 / p2 + 
  plot_annotation(title = "Differences between scenarios in water withdrawals",
                  subtitle = "Withdrawals in Susmip - Withdrawals in Baseline")
#print(map_waterWithdrawals)



##############ag production by crop type##############
df_query <- getQuery(prj1, "ag production by crop type" )

df_global <- to_global_custom(df_query, group_cols = c("scenario", "year", "output"), func = sum)

df_global$scenario <- gsub("SUSMIP_SUSTAINABLE", "Susmip Sustaintable", df_global$scenario)

cropProduction <- ggplot(df_global, aes(x = factor(year), y = value, fill = output)) +
  geom_col() +
  facet_wrap(~scenario, ncol = 1) +  # Aquí indicamos 1 columna para apilar verticalmente
  labs(x = "Year", y = "Energy Consumption (EJ)", fill = "Crop", title = 'Production by crop') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

####Diferencias####
df_diff <- df_global %>%
  select(scenario, year, output, value) %>%
  pivot_wider(
    names_from = scenario,
    values_from = value
  ) %>%
  mutate(
    diff = `Susmip Sustaintable` - Baseline
  )
df_diff <- df_diff %>%
  mutate(
    pct_diff = 100 * diff / Baseline
  )

crop_diffs <- ggplot(df_diff, aes(x = factor(year), y = pct_diff)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_col(fill = "darkorange") +
  facet_wrap(~output, scales = "free_y") +
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 2)]) +
  labs(
    title = "Crop production differences Susmip Sustaintable - Baseline",
    x = "Year",
    y = "% change"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


##############ag production by subsector (land use region)############## 

df_query <- getQuery(prj1, "ag production by subsector (land use region)" )
df_global <- to_global_custom(df_query, group_cols = c("scenario", "year"), func = sum)
df_global$scenario <- gsub("SUSMIP_SUSTAINABLE", "Susmip Sustaintable", df_global$scenario)
ag_commodity_totalProd <- ggplot(df_global, aes(x = year, y = value, color = scenario)) +
  geom_line(size = 1) +
  geom_point() +
  labs(x = "Year", y = "Production (EJ)", color = "Scenario", 
       title = "Crop commodity total production") +
  theme_minimal() +
  theme(legend.position = c(0.7, 0.2))

#### By region
df_query <- getQuery(prj1, "ag production by subsector (land use region)" )
df_region <- df_query[df_query$region %in% selected_regions,]
df_region <- df_query %>%
  filter(region %in% selected_regions) %>%
  mutate(
    scenario = gsub("SUSMIP_SUSTAINABLE", 
                    "Susmip Sustaintable", 
                    scenario)
  )
df_region <- df_region %>%
  group_by(scenario, region, year) %>%
  summarise(
    value = sum(value, na.rm = TRUE),
    .groups = "drop"
  ) 
total_agProd_byregion <- ggplot(
  df_region,
  aes(x = year, y = value, color = scenario)
) +
  geom_line(linewidth = 1) +
  geom_point() +
  facet_wrap(~ region, scales = "free_y") +
  labs(
    x = "Year", y = "Production (EJ)", color = "Scenario", 
    title = "Crop commodity total production by region"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold")
  )



############## ag commodity prices############## 
df_query <- getQuery(prj1, "ag commodity prices" )
df_global <- to_global_custom(df_query, group_cols = c("scenario", "year"), func = mean)
df_global$scenario <- gsub("SUSMIP_SUSTAINABLE", "Susmip Sustaintable", df_global$scenario)

ag_commodity_prices <- ggplot(df_global, aes(x = year, y = value, color = scenario)) +
  geom_line(size = 1) +
  geom_point() +
  labs(x = "Year", y = "Prices (1975$/GJ)", color = "Scenario", 
       title = "Crop commodity mean prices") +
  theme_minimal() +
  theme(legend.position = c(0.7, 0.2))
agCommoditiesPriceQuant <- ag_commodity_totalProd / ag_commodity_prices + 
  plot_annotation(title = "Crop Markets")
#print(combined_plot)

############## CO2 emissions by region############## 

df_query <- getQuery(prj1, "CO2 emissions by region" )
df_global <- to_global_custom(df_query, group_cols = c("scenario", "year"), func = sum)

df_global$scenario <- gsub("SUSMIP_SUSTAINABLE", "Susmip Sustaintable", df_global$scenario)


total_co2Emissions <- ggplot(df_global, aes(x = year, y = value, color = scenario)) +
  geom_line(size = 1) +
  geom_point() +
  labs(x = "Year", y = "CO2 (MTC)", color = "Scenario", 
       title = "CO2 emissions)") +
  theme_minimal() +
  theme(legend.position = c(0.7, 0.2))

#### Mapa


df_sum <- df_query %>%
  group_by(scenario, year, region) %>%
  summarize(value = sum(value, na.rm = TRUE), .groups = "drop")

# Convertimos el data frame a formato ancho para restar fácilmente
df_wide <- df_sum %>%
  pivot_wider(names_from = scenario, values_from = value)

# Asumiendo que los escenarios se llaman "Baseline" y "Susmip Sustaintable"
df_diff <- df_wide %>%
  mutate(diff = `SUSMIP_SUSTAINABLE` - Baseline) %>%
  select(region, year, diff)

# Relativa en porcentaje
df_diff <- df_wide %>%
  mutate(pct_diff = 100 * (`SUSMIP_SUSTAINABLE` - Baseline) / Baseline) %>%
  select(region, year, pct_diff)

df_summary <- df_diff %>%
  group_by(region) %>%
  summarize(
    avg_diff = mean(pct_diff, na.rm = TRUE),
    median_diff = median(pct_diff, na.rm = TRUE),
    diff_2100 = pct_diff[year == 2100]
  ) %>%
  ungroup()

df_summary <- df_summary %>%
  mutate(
    region = case_when(
      region == "EU-12" ~ "EU_12",
      region == "EU-15" ~ "EU_15",
      TRUE ~ region
    )
  )

df_map <- dplyr::left_join(mapGCAMReg32, df_summary, by = "region")

p1 <- ggplot(df_map) +
  geom_sf(aes(fill = diff_2100)) +
  scale_fill_viridis_c(option = "plasma", na.value = "grey90") +
  labs(title = "Difference in year 2100") +
  theme_minimal()

p2 <- ggplot(df_map) +
  geom_sf(aes(fill = avg_diff)) +
  scale_fill_viridis_c(option = "plasma", na.value = "grey90") +
  labs(title = "Average difference") +
  theme_minimal()

map_emissions <- p1 / p2 + 
  plot_annotation(title = "Differences between scenarios in CO2 emissions",
                  subtitle = "Emissions in Susmip - Emissions in Baseline")
#print(map_waterWithdrawals)

#### By region
df_query <- getQuery(prj1, "CO2 emissions by region" )
df_region <- df_query[df_query$region %in% selected_regions,]
df_region <- df_query %>%
  filter(region %in% selected_regions) %>%
  mutate(
    scenario = gsub("SUSMIP_SUSTAINABLE", 
                    "Susmip Sustaintable", 
                    scenario)
  )

total_co2Emissions_byregion <- ggplot(
  df_region,
  aes(x = year, y = value, color = scenario)
) +
  geom_line(linewidth = 1) +
  geom_point() +
  facet_wrap(~ region, scales = "free_y") +
  labs(
    x = "Year",
    y = "CO2 (MtC)",
    color = "Scenario",
    title = "CO2 emissions by region"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold")
  )



############## "water consumption by region"############## 

df_query <- getQuery(prj1, "water consumption by region" )
df_global <- to_global_custom(df_query, group_cols = c("scenario", "year"), func = sum)

df_global$scenario <- gsub("SUSMIP_SUSTAINABLE", "Susmip Sustaintable", df_global$scenario)
total_waterConsumption <- ggplot(df_global, aes(x = year, y = value, color = scenario)) +
  geom_line(size = 1) +
  geom_point() +
  labs(x = "Year", y = "Value", color = "Scenario", 
       title = "Water consumption (km^3)") +
  theme_minimal() +
  theme(legend.position = c(0.7, 0.2))

##############energy-for-water TFE############## 

df_query <- getQuery(prj1, "energy-for-water TFE" )
df_global <- to_global_custom(df_query, group_cols = c("scenario", "year"), func = sum)

df_global$scenario <- gsub("SUSMIP_SUSTAINABLE", "Susmip Sustaintable", df_global$scenario)
energy_for_water <- ggplot(df_global, aes(x = year, y = value, color = scenario)) +
  geom_line(size = 1) +
  geom_point() +
  labs(x = "Year", y = "Energy (EJ)", color = "Scenario", 
       title = "Energy for Water") +
  theme_minimal() +
  theme(legend.position = c(0.7, 0.2))

df_global <- to_global_custom(df_query, group_cols = c("scenario", "year", "input"), func = sum)

df_global$scenario <- gsub("SUSMIP_SUSTAINABLE", "Susmip Sustaintable", df_global$scenario)

energyforWaterbyUse <- ggplot(df_global, aes(x = factor(year), y = value, fill = input)) +
  geom_col() +
  facet_wrap(~scenario, ncol = 1) +  # Aquí indicamos 1 columna para apilar verticalmente
  labs(x = "Year", y = "Energy Consumption (EJ)", fill = "Type of use") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##############food demand prices############## 



##############demand balances by meat and dairy commodity############## 
df_query <- getQuery(prj1, "demand balances by meat and dairy commodity" )
df_query <- df_query %>%
  filter(year >= 2000)
df_global <- to_global_custom(df_query, group_cols = c("scenario", "year"), func = sum)

df_global$scenario <- gsub("SUSMIP_SUSTAINABLE", "Susmip Sustaintable", df_global$scenario)

meat_prod <- ggplot(df_global, aes(x = year, y = value, color = scenario)) +
  geom_line(size = 1) +
  geom_point() +
  labs(x = "Year", y = "Meat (Mt)", color = "Scenario", 
       title = "Meat Consumption") +
  theme_minimal() +
  theme(legend.position = c(0.7, 0.7))

df_global <- to_global_custom(df_query, group_cols = c("scenario", "year", "input"), func = sum)
df_global$scenario <- gsub("SUSMIP_SUSTAINABLE", "Susmip Sustaintable", df_global$scenario)
meatProdByUse <- ggplot(df_global, aes(x = factor(year), y = value, fill = input)) +
  geom_col() +
  facet_wrap(~scenario, ncol = 1) +  # Aquí indicamos 1 columna para apilar verticalmente
  labs(x = "Year", y =  "Meat (Mt)",title = "Disaggregated meat Consumption", fill = "Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

meat_sector <- meat_prod/meatProdByUse + 
  plot_annotation(title = "Meat sector")

##### By region
df_query <- getQuery(prj1, "demand balances by meat and dairy commodity" )
df_query <- df_query %>%
  filter(year >= 2000)
df_region <- df_query[df_query$region %in% selected_regions,]
df_region <- df_query %>%
  filter(region %in% selected_regions) %>%
  mutate(
    scenario = gsub("SUSMIP_SUSTAINABLE",
                    "Susmip Sustaintable",
                    scenario)
  )
df_region <- df_region %>%
  group_by(scenario, region, year) %>%
  summarise(
    value = sum(value, na.rm = TRUE),
    .groups = "drop"
  )
total_meatConsumption_byregion <- ggplot(
  df_region,
  aes(x = year, y = value, color = scenario)
) +
  geom_line(linewidth = 1) +
  geom_point() +
  facet_wrap(~ region, scales = "free_y") +
  labs(
    x = "Year",
    y = "Meat (Mt)",
    color = "Scenario",
    title = "Meat consumption by region"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold")
  )

#### Mapa
df_query <- getQuery(prj1, "demand balances by meat and dairy commodity" )
df_query <- df_query %>%
  filter(year >= 2000)

df_sum <- df_query %>%
  group_by(scenario, year, region) %>%
  summarize(value = sum(value, na.rm = TRUE), .groups = "drop")

# Convertimos el data frame a formato ancho para restar fácilmente
df_wide <- df_sum %>%
  pivot_wider(names_from = scenario, values_from = value)

# Asumiendo que los escenarios se llaman "Baseline" y "Susmip Sustaintable"
df_diff <- df_wide %>%
  mutate(diff = `SUSMIP_SUSTAINABLE` - Baseline) %>%
  select(region, year, diff)

# Relativa en porcentaje
df_diff <- df_wide %>%
  mutate(pct_diff = 100 * (`SUSMIP_SUSTAINABLE` - Baseline) / Baseline) %>%
  select(region, year, pct_diff)

df_summary <- df_diff %>%
  group_by(region) %>%
  summarize(
    avg_diff = mean(pct_diff, na.rm = TRUE),
    median_diff = median(pct_diff, na.rm = TRUE),
    diff_2100 = pct_diff[year == 2100]
  ) %>%
  ungroup()

df_summary <- df_summary %>%
  mutate(
    region = case_when(
      region == "EU-12" ~ "EU_12",
      region == "EU-15" ~ "EU_15",
      TRUE ~ region
    )
  )

df_map <- dplyr::left_join(mapGCAMReg32, df_summary, by = "region")

p1 <- ggplot(df_map) +
  geom_sf(aes(fill = diff_2100)) +
  scale_fill_viridis_c(option = "plasma", na.value = "grey90") +
  labs(title = "Difference in year 2100") +
  theme_minimal()

p2 <- ggplot(df_map) +
  geom_sf(aes(fill = avg_diff)) +
  scale_fill_viridis_c(option = "plasma", na.value = "grey90") +
  labs(title = "Average difference") +
  theme_minimal()

map_meatCons<- p1 / p2 + 
  plot_annotation(title = "Differences between scenarios in meat consumption",
                  subtitle = "Meat consumption in Susmip - meat consumption in Baseline")



############## PDF globals ############## 
library(grid)

section_page <- function(title) {
  grid.newpage()
  grid.text(
    title,
    x = 0.5, y = 0.5,
    gp = gpar(fontsize = 28, fontface = "bold")
  )
}
pdf("susmip.pdf", width = 11, height = 8)

# Global Temperature
section_page("Global Temperature")
print(global_temp)

# Emissions
section_page("Emissions")
print(total_co2Emissions)
print(map_emissions)
print(total_co2Emissions_byregion)

# Electricity and Energy
section_page("Electricity and Energy")
print(electr_gen_prices)
print(total_energy)
print(totalenergy_byregion)
print(p_fuelSector)

# Water
section_page("Water")
print(water_sector)
print(energy_for_water)
print(total_waterConsumption_byregion)
print(map_waterWithdrawals)

# Crops and Land
section_page("Crops and Land")
print(agCommoditiesPriceQuant)
print(cropProduction)
print(crop_diffs)
print(total_agProd_byregion)

# Food
section_page("Food")
print(meat_sector)
print(total_meatConsumption_byregion)
print(map_meatCons)

dev.off()



















########## por que no coinciden años historicos: water withdrawals by state, sector, basin

##Sacamos query
df_query <- getQuery(prj1, "water withdrawals by state, sector, basin"  )

##Filtramos por años históricos:
df_hist <- df_query %>%
  filter(year < 2000)

##Creamos dos data frames uno con el Baseline y otro con susmip:
df_base <- df_hist %>%
  filter(scenario == "Baseline") %>%
  select(-scenario) %>%
  rename(value_baseline = value) #renombrar value para escenario

df_susmip <- df_hist %>%
  filter(scenario == "SUSMIP_SUSTAINABLE") %>%
  select(-scenario) %>%
  rename(value_susmip = value) #renombrar value para escenario

##Mergeamos ambos data frames en base al resto de columnas y así tener los valores de cada escenario alineados
df_compare <- df_base %>%
  full_join(
    df_susmip,
    by = c("Units", "region", "sector", "subsector", "year")
  )
### Hacemos diferencias 
df_compare <- df_compare %>%
  mutate(
    diff = value_susmip - value_baseline
  )
#filtramos diferencias para ver 
View(df_compare %>%
       filter(abs(diff) > 0.01))

########## por que no coinciden años historicos: meat and dairy production by type
df_query <- getQuery(prj1, "demand balances by meat and dairy commodity" )
df_hist <- df_query %>%
  filter(year <= 2000)
df_base <- df_hist %>%
  filter(scenario == "Baseline") %>%
  select(-scenario) %>%
  rename(value_baseline = value)

df_susmip <- df_hist %>%
  filter(scenario == "SUSMIP_SUSTAINABLE") %>%
  select(-scenario) %>%
  rename(value_susmip = value)

df_compare <- df_base %>%
  full_join(
    df_susmip,
    by = c("Units", "region", "sector", "input","year")
  )

df_compare <- df_compare %>%
  mutate(
    diff = value_susmip - value_baseline
  )
View(df_compare %>%
       filter(abs(diff) > 0.01))


susmip <- df_query[df_query['scenario'] == 'SUSMIP_SUSTAINABLE', ]
unique(susmip['sector'])



