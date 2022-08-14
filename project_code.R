library(tidyverse)
library(janitor)
library(lubridate)
library(highcharter)
russiaLossesEquipment <- read.csv('Desktop/russia_losses_equipment.csv')
russiaLossesPersonnel <- read.csv('Desktop/russia_losses_personnel.csv')
glimpse(russiaLossesEquipment)
summary(russiaLossesEquipment)
# Clean columns names
russiaLossesEquipment.clean <- russiaLossesEquipment %>% clean_names()
# Let's take a look at the clean names dataset
glimpse(russiaLossesEquipment.clean)
# Correct formatting type of date column
russiaLossesEquipment.clean <- russiaLossesEquipment.clean %>%
  mutate(date = ymd(date))

# Add a new column with the differences of the losses of the previous day with the current date.
russiaLossesEquipment.clean <- russiaLossesEquipment.clean %>%
  mutate(aircraft_difference = aircraft - lag(aircraft, 
                                              default = first(aircraft)),
         helicopter_difference = helicopter - lag(helicopter,
                                                  default = first(helicopter)),
         tank_difference = tank - lag(tank,
                                      default = first(tank)),
         apc_difference = apc - lag(apc,
                                    default = first(apc)),
         field_artillery_difference = field_artillery - lag(field_artillery,
                                                            default = first(field_artillery)),
         mrl_difference = mrl - lag(mrl,
                                    default = first(mrl)),
         military_auto_difference = military_auto - lag(military_auto,
                                                        default = first(military_auto)),
         fuel_tank_difference = fuel_tank - lag(fuel_tank,
                                                default = first(fuel_tank)),
         drone_difference = drone - lag(drone,
                                        default = first(drone)),
         naval_ship_difference = naval_ship - lag(naval_ship,
                                                  default = first(naval_ship)),
         anti_aircraft_warfare_difference = anti_aircraft_warfare - lag(anti_aircraft_warfare,
                                                                        default = first(anti_aircraft_warfare)),
         special_equipment_difference = special_equipment - lag(special_equipment,
                                                                default = first(special_equipment)),
         mobile_srbm_system_difference = mobile_srbm_system - lag(mobile_srbm_system,
                                                                  default = first(mobile_srbm_system)))

# Order columns
russiaLossesEquipment.clean <- russiaLossesEquipment.clean %>%
  select(date,
         day,
         aircraft,
         aircraft_difference,
         helicopter,
         helicopter_difference,
         tank,
         tank_difference,
         apc,
         apc_difference,
         field_artillery,
         field_artillery_difference,
         mrl,
         mrl_difference,
         military_auto,
         military_auto_difference,
         fuel_tank,
         fuel_tank_difference,
         drone,
         drone_difference,
         naval_ship,
         naval_ship_difference,
         anti_aircraft_warfare,
         anti_aircraft_warfare_difference,
         special_equipment,
         special_equipment_difference,
         mobile_srbm_system,
         mobile_srbm_system_difference)

# Plot
russiaLossesEquipment.clean %>%
  group_by(date) %>%
  summarise(value = sum(aircraft,
                        helicopter,
                        tank,
                        apc,
                        field_artillery,
                        mrl,
                        military_auto,
                        fuel_tank,
                        drone,
                        naval_ship,
                        anti_aircraft_warfare,
                        special_equipment,
                        mobile_srbm_system,
                        na.rm = TRUE)) %>%
  hchart('line',
         name = 'Losses',
         hcaes(x = date,
               y = value)) %>%
  hc_colors('#da291c') %>%
  hc_xAxis(title = list(text = 'Date')) %>%
  hc_yAxis(title = list(text = 'Total')) %>%
  hc_title(text = 'Russia losses equipment') %>%
  hc_subtitle(text = 'How many equipment losses has Russia recorded each day?') %>%
  hc_caption(text = '<b>Russia losses equipment:</b><br>
             <em>Aircraft, Helicopter, Tank, APC, Field Artillery, MRL<br>
             Military Auto, Drone, Naval Ship, Anti Aircraft Warfare, Special Equipment and Mobile SRBM System</em>') %>%
  hc_add_theme(hc_theme_elementary()) -> plot1
plot1

# Calculate the total losses by each day
totalRussianLostEquipmentDay <- russiaLossesEquipment.clean %>%
  group_by(date) %>%
  mutate(total = sum(aircraft,
                     helicopter,
                     tank,
                     apc,
                     field_artillery,
                     mrl,
                     military_auto,
                     fuel_tank,
                     drone,
                     naval_ship,
                     anti_aircraft_warfare,
                     special_equipment,
                     mobile_srbm_system,
                     na.rm = TRUE)) %>%
  ungroup()

# We create a new column with the difference values of the previous day
totalRussianLostEquipmentDay <- totalRussianLostEquipmentDay%>%
  mutate(difference = total - lag(total, default = first(total)))

# We assign default value for the first input value
totalRussianLostEquipmentDay <- totalRussianLostEquipmentDay %>%
  mutate(difference = case_when(row_number() == 1 ~ first(total),
                                TRUE ~ difference))

# Plot: Equipment Lost by day
totalRussianLostEquipmentDay %>%
  hchart('column',
         name = 'Losses',
         hcaes(x = date,
               y = difference)) %>%
  hc_colors('#da291c') %>%
  hc_xAxis(title = list(text = 'Date')) %>%
  hc_yAxis(title = list(text = 'Total')) %>%
  hc_title(text = 'Equipment lost') %>%
  hc_plotOptions(series = list(boderWidth = 2,
                               dataLabels = list(enabled = TRUE))) %>%
  hc_subtitle(text = 'How many equipment losses has Russia recorded each day?') %>%
  hc_caption(text = '<b>Equipment lost by Russia:</b><br>
             <em>Aircraft, Helicopter, Tank, APC, Field Artillery, Fuel Tank, MRL<br>
             Military Auto, Drone, Naval Ship, Anti Aircraft Warfare, Special Equipment and Mobile SRBM System</em>') %>%
  hc_add_theme(hc_theme_elementary()) -> plot2
plot2

# Total number of equipments lost by category
categoryRussianLostequipment <- russiaLossesEquipment %>%
  clean_names() %>%
  mutate(date = ymd(date))

# We convert it to a longer format
categoryRussianLostequipment <- categoryRussianLostequipment %>%
  pivot_longer(cols = aircraft:mobile_srbm_system, names_to = 'equipment' , values_to = 'losses')

# Clean the output
categoryRussianLostequipment <- categoryRussianLostequipment %>%
  mutate(equipment = gsub('_', ' ', equipment)) %>%
  group_by(equipment) %>%
  summarise(total = max(losses,
                        na.rm = TRUE))
# Colors
colors <- c('#0048ba',
            '#ffb745',
            '#483d8b',
            '#af002a',
            '#50ffd5',
            '#3b7a57',
            '#c485a8',
            '#03c03c',
            '#171412',
            '#4cb7a5',
            '#87413f',
            '#004225',
            '#5f1933')

# Plot
categoryRussianLostequipment %>%
  hchart('bar',
         hcaes(x = equipment,
               y = total,
               group = equipment)) %>%
  hc_plotOptions(series = list(boderWidth = 2,
                               dataLabels = list(enabled = TRUE))) %>%
  hc_colors(colors) %>%
  hc_title(text = 'Equipment lost') %>%
  hc_subtitle(text = 'Total Russian equipment lost by category') %>%
  hc_xAxis(title = list(text = 'Equipment')) %>%
  hc_yAxis(title = list(text = 'Total')) %>%
  hc_add_theme(hc_theme_elementary()) -> plot3
plot3

# We create a new data frame with only the amounts of equipment lost per day
amountRussianLostEquipmentDay <- totalRussianLostEquipmentDay %>%
  mutate(aircraft_difference = coalesce(aircraft_difference, aircraft),
         helicopter_difference = coalesce(helicopter_difference,
                                          helicopter),
         tank_difference = coalesce(tank_difference,
                                    tank),
         apc_difference = coalesce(apc_difference,
                                   apc),
         field_artillery_difference = coalesce(field_artillery_difference,
                                               field_artillery),
         mrl_difference = coalesce(mrl_difference,
                                   mrl),
         military_auto_difference = coalesce(military_auto_difference,
                                             military_auto),
         fuel_tank_difference = coalesce(fuel_tank_difference,
                                         fuel_tank),
         drone_difference = coalesce(drone_difference,
                                     drone),
         naval_ship_difference = coalesce(naval_ship_difference,
                                          naval_ship),
         anti_aircraft_warfare_difference = coalesce(anti_aircraft_warfare_difference,
                                                     anti_aircraft_warfare),
         special_equipment_difference = coalesce(special_equipment_difference,
                                                 special_equipment),
         mobile_srbm_system_difference = coalesce(mobile_srbm_system_difference,
                                                  mobile_srbm_system)) %>%
  mutate(aircraft_difference = case_when(row_number() == 1 ~ first(aircraft),
                                         TRUE ~ aircraft_difference),
         helicopter_difference = case_when(row_number() == 1 ~ first(helicopter),
                                           TRUE ~ helicopter_difference),
         tank_difference = case_when(row_number() == 1 ~ first(tank),
                                     TRUE ~ tank_difference),
         apc_difference = case_when(row_number() == 1 ~ first(apc),
                                    TRUE ~ apc_difference),
         field_artillery_difference = case_when(row_number() == 1 ~ first(field_artillery),
                                                TRUE ~ field_artillery_difference),
         mrl_difference = case_when(row_number() == 1 ~ first(mrl),
                                    TRUE ~ mrl_difference),
         military_auto_difference = case_when(row_number() == 1 ~ first(military_auto),
                                              TRUE ~ military_auto_difference),
         fuel_tank_difference = case_when(row_number() == 1 ~ first(fuel_tank),
                                          TRUE ~ fuel_tank_difference),
         drone_difference = case_when(row_number() == 1 ~ first(drone),
                                      TRUE ~ drone_difference),
         naval_ship_difference = case_when(row_number() == 1 ~ first(naval_ship),
                                           TRUE ~ naval_ship_difference),
         anti_aircraft_warfare_difference = case_when(row_number() == 1 ~ first(anti_aircraft_warfare),
                                                      TRUE ~ anti_aircraft_warfare_difference),
         special_equipment_difference = case_when(row_number() == 1 ~ first(special_equipment),
                                                  TRUE ~ special_equipment_difference),
         mobile_srbm_system_difference = case_when(row_number() == 1 ~ first(mobile_srbm_system),
                                                   TRUE ~ mobile_srbm_system_difference)) %>%
  select(date,
         aircraft_difference,
         helicopter_difference,
         tank_difference,
         apc_difference,
         field_artillery_difference,
         mrl_difference,
         military_auto_difference,
         fuel_tank_difference,
         drone_difference,
         naval_ship_difference,
         anti_aircraft_warfare_difference,
         special_equipment_difference,
         mobile_srbm_system_difference)

# Plot
highchart(type = 'stock') %>%
  hc_chart('line',
           name = 'base',
           hcaes(x = date)) %>%
  hc_title(text = 'Equipment Lost') %>%
  hc_subtitle(text = 'Amount of Russian equipment lost by categories per day') %>%
  hc_add_series(amountRussianLostEquipmentDay,
                name = 'Aircrafts',
                type = 'line',
                hcaes(x = date,
                      y = aircraft_difference)) %>%
  hc_add_series(amountRussianLostEquipmentDay,
                name = 'Helicopter',
                type = 'line',
                hcaes(x = date,
                      y = helicopter_difference)) %>%
  hc_add_series(amountRussianLostEquipmentDay,
                name = 'Tanks',
                type = 'line',
                hcaes(x = date,
                      y = tank_difference)) %>%
  hc_add_series(amountRussianLostEquipmentDay,
                name = 'APC',
                type = 'line',
                hcaes(x = date,
                      y = apc_difference)) %>%
  hc_add_series(amountRussianLostEquipmentDay,
                name = 'Field Artillery',
                type = 'line',
                hcaes(x = date,
                      y = field_artillery_difference)) %>%
  hc_add_series(amountRussianLostEquipmentDay,
                name = 'MRL',
                type = 'line',
                hcaes(x = date,
                      y = mrl_difference)) %>%
  hc_add_series(amountRussianLostEquipmentDay,
                name = 'Military Auto',
                type = 'line',
                hcaes(x = date,
                      y = military_auto_difference)) %>%
  hc_add_series(amountRussianLostEquipmentDay,
                name = 'Fuel Tank',
                type = 'line',
                hcaes(x = date,
                      y = fuel_tank_difference)) %>%
  hc_add_series(amountRussianLostEquipmentDay,
                name = 'Drone',
                type = 'line',
                hcaes(x = date,
                      y = drone_difference)) %>%
  hc_add_series(amountRussianLostEquipmentDay,
                name = 'Naval Ship',
                type = 'line',
                hcaes(x = date,
                      y = naval_ship_difference)) %>%
  hc_add_series(amountRussianLostEquipmentDay,
                name = 'Anti Aircraft Warfare',
                type = 'line',
                hcaes(x = date,
                      y = anti_aircraft_warfare_difference)) %>%
  hc_add_series(amountRussianLostEquipmentDay,
                name = 'Special Equipment',
                type = 'line',
                hcaes(x = date,
                      y = special_equipment_difference)) %>%
  hc_add_series(amountRussianLostEquipmentDay,
                name = 'Mobile SRBM System',
                type = 'line',
                hcaes(x = date,
                      y = mobile_srbm_system_difference)) %>%
  hc_add_theme(hc_theme_elementary()) -> plot4
plot4

# Table  
amountRussianLostEquipmentDay %>%
  group_by(date) %>%
  summarise(equipment_lost = sum(aircraft_difference,
                                 helicopter_difference,
                                 tank_difference,
                                 apc_difference,
                                 field_artillery_difference,
                                 mrl_difference,
                                 military_auto_difference,
                                 fuel_tank_difference,
                                 drone_difference,
                                 naval_ship_difference,
                                 anti_aircraft_warfare_difference,
                                 special_equipment_difference,
                                 mobile_srbm_system_difference,
                                 na.rm = TRUE)) %>%
  ungroup() %>%
  View()

# Create categories
categories <- amountRussianLostEquipmentDay %>%
  pivot_longer(cols = aircraft_difference:mobile_srbm_system_difference,
               names_to = 'equipment' ,
               values_to = 'losses') %>%
  mutate(equipment = gsub('_',
                          ' ',
                          equipment))
# Clean categories output  
categories <- categories %>%
  mutate(equipment = sub(' difference',
                         '',
                         equipment))
# Create data frame for drilldown
totalRussianLostEquipmentCategory <- tibble(name = categoryRussianLostequipment$equipment,
                                            y = categoryRussianLostequipment$total,
                                            drilldown = tolower(name))

# Filter by category
aircraft <- categories %>%
  filter(equipment %in% 'aircraft')

antiAircraftWarfare <- categories %>%
  filter(equipment %in% 'anti aircraft warfare')

apc <- categories %>%
  filter(equipment %in% 'apc')

drone <- categories %>%
  filter(equipment %in% 'drone')

fieldArtillery <- categories %>%
  filter(equipment %in% 'field artillery')

fuelTank <- categories %>%
  filter(equipment %in% 'fuel tank')

helicopter <- categories %>%
  filter(equipment %in% 'helicopter')

militaryAuto <- categories %>%
  filter(equipment %in% 'military auto')

mobileSrbmSystem <- categories %>%
  filter(equipment %in% 'mobile srbm system')

mrl <- categories %>%
  filter(equipment %in% 'mrl')

navalShip <- categories %>%
  filter(equipment %in% 'naval ship')

specialEquipment <- categories %>%
  filter(equipment %in% 'special equipment')

tank <- categories %>%
  filter(equipment %in% 'tank')

x <- c('Lost Equipment')
y <- c('{point.y}')
tt <- tooltip_table(x,
                    y)
# Create data frames by category for drilldown  
dAircarft = data.frame(name = aircraft$date,
                       y = aircraft$losses)

dAntiAircraftWarfare = data.frame(name = antiAircraftWarfare$date,
                                  y = antiAircraftWarfare$losses)

dApc = data.frame(name = apc$date,
                  y = apc$losses)

dDrone = data.frame(name = drone$date,
                    y = drone$losses)

dFieldArtillery = data.frame(name = fieldArtillery$date,
                             y = fieldArtillery$losses)

dFuelTank = data.frame(name = fuelTank$date,
                       y = fuelTank$losses)

dHelicopter = data.frame(name = helicopter$date,
                         y = helicopter$losses)

dMilitaryAuto = data.frame(name = militaryAuto$date,
                           y = militaryAuto$losses)

dMobileSrbmSystem = data.frame(name = mobileSrbmSystem$date,
                               y = mobileSrbmSystem$losses)

dMrl = data.frame(name = mrl$date,
                  y = mrl$losses)

dNavalShip = data.frame(name = navalShip$date,
                        y = navalShip$losses)

dSpecialEquipment = data.frame(name = specialEquipment$date,
                               y = specialEquipment$losses)

dTank = data.frame(name = tank$date,
                   y = tank$losses)
# Plot
highchart() %>%
  hc_chart(type = "column") %>%
  hc_title(text = "Russian lost equipment") %>%
  hc_subtitle(text = 'Drilldown: How many equipments have been lost per day in each category?') %>%
  hc_xAxis(type = "category") %>%
  hc_legend(enabled = FALSE) %>%
  hc_plotOptions(series = list(boderWidth = 2,
                               dataLabels = list(enabled = TRUE))) %>%
  hc_add_series(data = totalRussianLostEquipmentCategory,
                name = "Lost equipment",
                colorByPoint = TRUE) %>%
  hc_drilldown(allowPointDrilldown = TRUE,
               series = list(list(id = "aircraft",
                                  data = list_parse2(dAircarft)),
                             list(id = 'anti aircraft warfare',
                                  data = list_parse2(dAntiAircraftWarfare)),
                             list(id = 'apc',
                                  data = list_parse2(dApc)),
                             list(id = 'drone',
                                  data = list_parse2(dDrone)),
                             list(id = 'field artillery',
                                  data = list_parse2(dFieldArtillery)),
                             list(id = 'fuel tank',
                                  data = list_parse2(dFuelTank)),
                             list(id = 'helicopter',
                                  data = list_parse2(dHelicopter)),
                             list(id = 'military auto',
                                  data = list_parse2(dMilitaryAuto)),
                             list(id = 'mobile srbm system',
                                  data = list_parse2(dMobileSrbmSystem)),
                             list(id = 'mrl',
                                  data = list_parse2(dMrl)),
                             list(id = 'naval ship',
                                  data = list_parse2(dNavalShip)),
                             list(id = 'special equipment',
                                  data = list_parse2(dSpecialEquipment)),
                             list(id = 'tank',
                                  data = list_parse2(dTank)))) %>%
  hc_tooltip(pointFormat = tt,
             useHTML = TRUE) %>%
  hc_colors(colors) %>%
  hc_add_theme(hc_theme_elementary()) -> plot5
plot5
)

glimpse(russiaLossesPersonnel)
colnames(russiaLossesPersonnel)
summary(russiaLossesPersonnel)
russiaLossesPersonnel.clean <- russiaLossesPersonnel %>%
  clean_names()
russiaLossesPersonnel.clean %>%
  hchart('line',
         name = 'Losses',
         hcaes(x = date,
               y = personnel)) %>%
  hc_colors('#da291c') %>%
  hc_title(text = 'Dead russian soldiers') %>%
  hc_subtitle(text = 'How many dead soldiers has Russia registered each day?') %>%
  hc_xAxis(title = list(text = 'Date')) %>%
  hc_yAxis(title = list(text = 'Total')) %>%
  hc_add_theme(hc_theme_elementary()) -> plot7
plot7

# Lost personal by day
totalRussianLostPersonalDay <- russiaLossesPersonnel.clean %>%
  mutate(personnel_difference = personnel - lag(personnel,
                                                default = first(personnel)),
         pow_difference = pow - lag(pow,
                                    default = first(pow))) %>%
  mutate(personnel_difference = case_when(row_number() == 1 ~ first(personnel),
                                          TRUE ~ personnel_difference),
         pow_difference = case_when(row_number() == 1 ~ first(pow),
                                    TRUE ~ pow_difference)) %>%
  select(date,
         day,
         personnel,
         personnel_difference,
         everything())
# Plot
totalRussianLostPersonalDay %>%
  hchart('column',
         name = 'Losses',
         hcaes(x = date,
               y = personnel_difference)) %>%
  hc_colors('#da291c') %>%
  hc_title(text = 'Missing Russian personnel') %>%
  hc_subtitle(text = 'How many dead soldiers has Russia registered each day?') %>%
  hc_plotOptions(series = list(boderWidth = 2,
                               dataLabels = list(enabled = TRUE))) %>%
  hc_xAxis(title = list(text = 'Date')) %>%
  hc_yAxis(title = list(text = 'Total')) %>%
  hc_add_theme(hc_theme_elementary()) -> plot8
plot8
# POW Plot
russiaLossesPersonnel.clean %>%
  hchart('line',
         name = 'Losses',
         hcaes(x = date,
               y = pow)) %>%
  hc_colors('#da291c') %>%
  hc_title(text = 'Russian prisoners of war') %>%
  hc_subtitle(text = 'How many prisoners of war has Russia registered each day?') %>%
  hc_xAxis(title = list(text = 'Date')) %>%
  hc_yAxis(title = list(text = 'Total')) %>%
  hc_add_theme(hc_theme_elementary()) -> plot9
plot9

totalRussianLostPersonalDay %>%
  hchart('column',
         name = 'Losses',
         hcaes(x = date,
               y = pow_difference)) %>%
  hc_colors('#da291c') %>%
  hc_title(text = 'Russian prisoners of war') %>%
  hc_subtitle(text = 'How many prisoners of war has Russia registered each day?') %>%
  hc_plotOptions(series = list(boderWidth = 2,
                               dataLabels = list(enabled = TRUE))) %>%
  hc_xAxis(title = list(text = 'Date')) %>%
  hc_yAxis(title = list(text = 'Total')) %>%
  hc_add_theme(hc_theme_elementary()) -> plot10
plot10

# MAX russian soldier deaths registered
totalRussianLostPersonalDay %>%
  slice(which.max(personnel_difference)) %>%
  ungroup() %>%
  select(date,
         personnel = personnel_difference) %>%
  View()
# MIN russian soldier deaths registered 
totalRussianLostPersonalDay %>%
  slice(which.min(personnel_difference)) %>%
  ungroup() %>%
  select(date,
         personnel = personnel_difference) %>%
  View()