
# This script takes the raw data files and prepares subsets of data as example 
# datasets for use in the daily tutorials. Example datasets are saved as CSV or
# Excel files in the working directory.
#
# Here is the list of example datasets in the order they are used in the 
# tutorials:
#
#   abargelie.csv
#     A small epidemiological dataset that can be used to illustrate reading 
#     data into R and doing the basic things you do with data frames, e.g. 
#     print, str, summary, subsetting, and plotting. Can also be used to 
#     demonstrate calculation of indices. Used on days 1 and 2. Can be renamed
#     if we decide to use a different woreda.
#     ID cols: 
#       woreda, zone, popul_tot, popul_malar: Abargelie
#       epi_year: 2014--2016
#       epi_week: 1--53
#     Data cols: (this should be pared down to only what we need)
#       tot_case, mal_case, mal_death, mal_inp, mal_outp, rdt_done,
#       rdt_pf_tot, rdt_pv_only, bf_done, bf_pf_tot, bf_pv_only
#
#   mecha.csv
#     The same as abargelie.csv, just with data from Mecha instead. Can be used
#     for t tests on day 1 and to demonstrate row binding on day 2.
#
#   harmonized.xlsx
#     A larger dataset containing weekly epidemiological and environmental data
#     from four woredas over four years. No missing rows.
#     ID cols: 
#       woreda, zone, popul_tot, popul_malar: Fogera, Mecha, Metema, Jabi Tehnan
#       epi_year: 2013--2016
#       epi_week: 1--53
#     Data cols:
#       mal_case, rain, temp, ndvis
#   
#   rain.csv
#   ndvi.csv
#     Two long-term daily dataset of rainfall and NDVI. Can be used to
#     demonstrate calculation of climatologies and anomalies, summarizing by
#     week to join with epidemiological data.
#
#   epidemia.csv
#     The processed data exactly as exported from the EPIDEMIA system.
#
#
# run this first, if necessary
# source("fetch_data")

morb_data <-
  read_csv("data-raw/epidemia_raw_morbidity_data_2017_07_05.csv") %>% 
  select(1:5, 8, 7, 14, 13, 37:41, 42:46) %>% 
  `names<-`(c("zone", "woreda", "epi_year", "epi_week", "popul_tot", 
              "tot_inp", "tot_outp", "mal_inp", "mal_outp", 
              "rdt_done", "rdt_pf", "rdt_pv", "rdt_m", "rdt_pfm",
              "bf_done", "bf_pf", "bf_pv", "bf_m", "bf_pfm")) %>% 
  mutate(
    tot_case = tot_inp + tot_outp,
    mal_case = mal_inp + mal_outp,
    rdt_pfm = ifelse(!is.na(rdt_pfm), rdt_pfm, rdt_pf + rdt_m),
    bf_pfm = ifelse(!is.na(bf_pfm), bf_pfm, bf_pf + bf_m)
  ) %>% 
  select(zone:popul_tot, tot_case, mal_case, rdt_pfm, rdt_pv, bf_pfm, bf_pv)

# fogera.csv -----------------------------------------------------------

morb_data %>% 
  filter(woreda == "Fogera",
         epi_year == 2016) %>% 
write_excel_csv("fogera.csv", col_names = TRUE)

# select(epi_year, epi_week, tot_case:bf_pv) %>%
# tidyr::gather(key, value, tot_case:bf_pv) %>%
# ggplot(aes(epi_week, value)) +
# geom_line() +
# facet_grid(epi_year ~ key)
  
# mecha.csv ---------------------------------------------------------------

morb_data %>% 
  filter(woreda == "Mecha",
         epi_year %in% 2014:2016) %>% 
  write_excel_csv("mecha.csv", col_names = TRUE)

# harmonized.xlsx ----------------------------------------------------------

woreda_subset <- c("Fogera", "Mecha", "Metema", "Jabi Tehnan")

epidemia_proc_data <- 
  read_csv("data-raw/epidemia_processed_data_2017_07_05.csv", 
           col_names = c(
             "zone", "woreda", "epi_year", "epi_week", "popul_tot", 
             "tot_case", "mal_case", "mal_case_outp", "mal_case_inp", 
             "mal_death", "mal_0_5", "mal_5_14", "mal_15_up", 
             "rdt_done", "bf_done", "test_done", 
             "mal_inc", "conf_inc", "pfm_inc", "pv_inc", 
             "mal_prop", "conf_prop", "pfm_prop", "pv_prop",
             "pos_rate", "pfm_pos_rate", "pv_pos_rate",
             "compl_gov", "compl_ngov", "sr_phys", "sr_chem",
             "prev_part_rate", "educ_part_rate", "irs_cov_rate", 
             "llin_visit", "llin_using", 
             "tmpa_obs", "tmpa_anom", "gpm_obs", "gpm_anom", 
             "lstd_obs", "lstd_anom", "ndvi_obs", "ndvi_anom", 
             "evi_obs", "evi_anom", "savi_obs", "savi_anom",
             "ndwi5_obs", "ndwi5_anom", "ndwi6_obs", "ndwi6_anom"
           ),
           skip = 1
  ) %>% 
  filter(woreda %in% woreda_subset) %>% 
  select(zone, woreda, epi_year, epi_week, popul_tot, tot_case, mal_case,
         ends_with("_inc"), ends_with("_prop"), contains("pos_rate"),
         matches("_obs|_anom")) %>% 
  print()

write_excel_csv(epidemia_proc_data, "harmonized.csv",
                col_names = TRUE)

# rain.csv ----------------------------------------------------------------


# ndvi.csv ----------------------------------------------------------------


# epidemia.csv ------------------------------------------------------------

