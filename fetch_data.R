
# this script reads the raw morbidity and mortality data from the EPIDEMIA
# database and saves it as a CSV file. 
# 
# this data can be used as is to develop the workshop documents, or used to
# create a pseudo-real dataset for eventual dissemination at the workshop


# load packages -----------------------------------------------------------

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(DBI, RMySQL, dbplyr, dplyr, ggplot2, readr)

if (!require("eastweb")) {
  devtools::install_git(
    url = "https://gsce-git.sdstate.edu/gitblit/r/epidemia/epical_package.git",
    branch = "develop",
    credentials = git2r::cred_user_pass(
      winDialogString("Gitblit Username", ""), 
      winDialogString("Gitblit Password", "")
    ),
    quiet = FALSE,
    dependencies = FALSE,
    upgrade_dependencies = FALSE
  )
}
library(eastweb)


# create data-raw directory -----------------------------------------------

if (!dir.exists("data-raw")) dir.create("data-raw")


# fetch the epidemiological data --------------------------------------------

cnf_file_path <- file.path(getwd(), "epidemia.cnf") # doesnt work with rel path
connection_group <- "epidemia_production"

db_con <- DBI::dbConnect(drv = RMySQL::MySQL(), 
                         default.file = cnf_file_path, 
                         group = connection_group)

morb_data <-
  tbl(db_con, "Weekly_Morbidity_Upload") %>%
  left_join(                                      # add woreda names
    tbl(db_con, "Woreda") %>%
      select(Woreda_ID, Woreda, Is_Pilot_Woreda),
    by = "Woreda_ID"
  ) %>% 
  filter(Is_Pilot_Woreda != 0) %>%                # exclude non-pilot woredas
  left_join(                                      # add zone names
    tbl(db_con, "Zone") %>% 
      select(Zone_ID, Zone),
    by = "Zone_ID"
  ) %>% 
  select(-Population, -Popul_Malar) %>%           # get from Popul. tbl instead
  left_join(                                      # add population data
    tbl(db_con, "Population") %>% 
      select(Woreda_ID, Year, Epiweek, 
             Population, Popul_Malar),
    by = c("Woreda_ID", "Year", "Epiweek")
  ) %>% 
  select(Upload_ID,                               # needed for de-duping later
         woreda = Woreda,
         zone = Zone,
         epi_year = Year,
         epi_week = Epiweek,
         popul_tot = Population,
         popul_malar = Popul_Malar,
         mal_inp,
         mal_outp,
         rdt_done,
         rdt_pf_tot,
         rdt_pv_only,
         bf_done,
         bf_pf_tot,
         bf_pv_only) %>% 
  arrange(woreda, epi_year, epi_week, 
          desc(Upload_ID)) %>% 
  collect() 

DBI::dbDisconnect(db_con)                         # always do this when done

# remove duplicate combinations of woreda & epi week caused by multiple uploads
morb_data <-
  morb_data %>% 
  group_by(woreda, epi_year, epi_week) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(-Upload_ID)


# save the morbidity data
readr::write_csv(morb_data, "data-raw/morb_data.csv")
message("Data saved to data-raw/morb_data.csv")

# there is no good function to write to an Excel file
# instead open the CSV and save it as an Excel file


# explore the epidemiological data ------------------------------------------

# there are 47 woredas, most with 258 weeks of data
count(morb_data, woreda)

# 4 woredas have only 257 weeks of data (i.e. 1 week is missing)
count(morb_data, woreda) %>%
  filter(n < 258)

# there are quite a few NAs for mal_outp, rdt_pf_tot, and bf_pf_tot
morb_data %>% 
  summary()

# for the original 30 pilot woredas, 1st year is missing for those 3 variables
morb_data %>% 
  group_by(woreda) %>% 
  mutate(week = row_number()) %>%
  ungroup() %>% 
  ggplot(aes(week, mal_outp)) +
  geom_line() +
  facet_wrap(~ woreda, scales = "free_y") +
  labs(title = "Malaria Outpatient Cases",
       subtitle = "For the original 30 pilot woredas, 1st year is missing")

# why is the 1st year of data missing?
# the first year had male + female cases, with total malaria = sum of those
# when 17 new woredas were added, old data was uploaded from 2013 w/ outp + inp

# suggestion: remove data from 2013-W28 through 2014-W27 to make a complete sets

# fetch epidemia weekly processed data --------------------------------------

cnf_file_path <- file.path(getwd(), "epidemia.cnf") # doesnt work with rel path
connection_group <- "epidemia_production"

db_con <- DBI::dbConnect(drv = RMySQL::MySQL(), 
                         default.file = cnf_file_path, 
                         group = connection_group)

epidemia_wkly_environ_data <-
  tbl(db_con, "Wkly_Data") %>%
  select(zone = Zone,
         woreda = Woreda,
         epi_year = Year,
         epi_week = Epiweek,
         popul_tot = Population,
         popul_malar = Popul_Malar,
         everything(),
         -Region, 
         -ends_with("_ID")) %>% 
  rename() %>% 
  arrange(woreda, epi_year, epi_week) %>% 
  collect()

DBI::dbDisconnect(db_con)                         # always do this when done

# save the environmental data
readr::write_csv(epidemia_wkly_environ_data, "data-raw/wkly_environ_data.csv")
message("Data saved to data-raw/wkly_environ_data.csv")

# fetch the eastweb data ---------------------------------------------------

config <- ew_read_config_xml(
  "R:/epidemia/eastweb_ops/eastweb_v2.2.3/config/config.xml", 
  host = "rosa.jacks.local", 
  mapped_drive = "R"
)
db_pool <- ew_db_pool(config)
ew_list_schemata(db_pool)

ew_read_zonal_stat_tbl(db_pool, schema_name = 
                         "modis_nbar_20170517_modisnbarv6") %>% 
  write_csv("data-raw/modis_nbar_20170517_modisnbarv6.csv")
ew_read_zonal_stat_tbl(db_pool, schema_name = 
                         "modis_nbar_20170518_modisnbarv6") %>% 
  write_csv("data-raw/modis_nbar_20170518_modisnbarv6.csv")
read_csv("data-raw/modis_nbar_20170518_modisnbarv6.csv") %>% 
  filter(Year >= 2014 & Year <= 2016, Index_Name == "ModisNBARV6NDWI6") %>% 
  transmute(woreda, year = Year, doy = DayOfYear, value = Mean) %>% 
  readr::write_csv("ndwi.csv")

ew_read_zonal_stat_tbl(db_pool, schema_name = 
                         "imerg_final_20170407_imerg") %>% 
  write_csv("data-raw/imerg_final_20170407_imerg.csv")
read_csv("data-raw/imerg_final_20170407_imerg.csv") %>% 
  filter(Year >= 2014 & Year <= 2016) %>% 
  transmute(woreda, year = Year, doy = DayOfYear, value = Mean) %>% 
  readr::write_csv("imerg.csv")


read_csv("ndwi.csv") %>% filter(woreda=="Mecha") %>% count(year)
read_csv("trmm.csv") %>%  count(year, doy)

ew_read_zonal_stat_tbl(db_pool, schema_name = 
                         "trmm_3b42_20170407_trmm3b42_new") %>% 
  write_csv("data-raw/trmm_3b42_20170407_trmm3b42_new.csv")

read_csv("data-raw/trmm_3b42_20170407_trmm3b42_new.csv") %>% 
  filter(Year >= 2014 & Year <= 2016) %>% 
  transmute(woreda, year = Year, doy = DayOfYear, value = Mean) %>% 
  readr::write_csv("trmm.csv")

