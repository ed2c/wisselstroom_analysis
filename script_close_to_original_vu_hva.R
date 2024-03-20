################################################################################
###
### Based on Wisselstroom script VU --> HvA
### as read in:
### "Wisselstroom - Doorstroom VU naar HvA v1.3.pdf"
### by Bastiaan van der Wulp & Annemarie Engels
### 2024_01_22
###
### This script, close to intention above :
### Martine Jansen
### 2024_03_13
###
################################################################################

#### comments on diverting from original script --------------------------------

# Most of the time, original code is given in the comments


#### libraries -----------------------------------------------------------------

library(tidyverse)


#### location of funding files hisbek and vlpbek -------------------------------

# Original script not in project form,
# files seem exptected to be located in working directory
# I chose to let the user specify the location for data directory
# that includes not being dependent on the specific brin


# specify reading locations, opens a window

locationname_hisbek <- choose.files(caption = "Select HISBEK",
                                    multi = FALSE)
locationname_vlpbek <- choose.files(caption = "Select VLPBEK",
                                    multi = FALSE)

### Brin of bek

brin_vlpbek <- str_sub(locationname_vlpbek,
                       start = -8, 
                       end = -5)


### Brin of interest

# In de VU_HvA script, VU is the previous brin
# Here we can set our own brin_of_interest

brin_of_interest <- "21PG"

### read in files --------------------------------------------------------------

# since the hisbek and vlpbek are in a specific location, code is changed
# in contrast to original script, every mutation is done in R, so
# result is saved in df and not in csv or on ms sql
# original code lines are commented out
# in next iteration of this script i will turn it into a smaller set of functions



#read.hisvlp <- function( yyyy, yyyymmdd, saveto = 0 ) {
read.hisvlp <- function( dir_hisbek, dir_vlpbek ) {
  # Kolomnamen in de HRD-records
  fields = c( "Recordsoort",
              "Burgerservicenummer",
              "Onderwijsnummer",
              "Bekostigingsjaar",
              "Brin",
              "inschrijvingvolgnummer",
              "Bekostigingsindicatie",
              "CodeBekostigingstatus",
              "bekostigingsniveau",
              "opleidingscode",
              "opleidingsniveau",
              "opleidingsfase",
              "datumInschrijving",
              "datumUitschrijving",
              "eersteInschrijving",
              "inschrijvingsvorm",
              "onderwijsvorm",
              "datumEersteAanlevering",
              "bekostigingsduur",
              "ECTS",
              "ECTSBekostigd",
              "opleidingOnderdeel",
              "bekostigingscode",
              "indicatieSectorLG",
              "indicatieBaMa",
              "indicatieAcademischZiekenhuis",
              "duitseDeelstaat",
              "indicatieWoonplaatsVereiste",
              "indicatieNationaliteitsvoorwaardeSF",
              "indicatieGBARelatie" )
  
  # added line to find funding year (yyyy as argument in original code)
  
  funding_year <- funding_year_vlpbek <- str_sub(locationname_vlpbek,
                                                 start = -22,
                                                 end = -19) |> as.integer()
  
  # print line commented out
  # print( "Reading from files..." )
  # HISBEK bestand lezen, filteren op HRD-records
  # hisbek <- read.csv( paste0( "HISBEK_", yyyy - 1, "_", yyyymmdd, "_28DN.csv" ),
  #                     header = FALSE,
  #                     sep = "|",
  #                     col.names = fields ) %>%
  hisbek <- read.csv( dir_hisbek,
                      header = FALSE,
                      sep = "|",
                      col.names = fields ) %>%
    filter( Recordsoort == "HRD" )
  # VLPBEK bestand lezen, filteren op BRD-records, ontbrekende kolommen toevoegen
  # vlpbek <- read.csv( paste0( "VLPBEK_", yyyy, "_", yyyymmdd, "_28DN.csv" ),
  #                     header = FALSE,
  #                     sep = "|",col.names = fields[ !fields %in% c( "Bekostigingsjaar",
  #                                                                   "ECTS",
  #                                                                   "ECTSBekostigd",
  #                                                                   "duitseDeelstaat",
  #                                                                   "indicatieWoonplaatsVereiste" ) ]
  # ) %>%
    vlpbek <- read.csv( dir_vlpbek,
                        header = FALSE,
                        sep = "|",col.names = fields[ !fields %in% c( "Bekostigingsjaar",
                                                                      "ECTS",
                                                                      "ECTSBekostigd",
                                                                      "duitseDeelstaat",
                                                                      "indicatieWoonplaatsVereiste" ) ]
    ) %>%
    filter( Recordsoort == "BRD" ) %>%
   # add_column( Bekostigingsjaar = as.integer(yyyy), .after = "Onderwijsnummer" ) %>%
    add_column( Bekostigingsjaar = funding_year, .after = "Onderwijsnummer" ) %>%
    add_column( ECTS = NA, .after = "bekostigingsduur" ) %>%
    add_column( ECTSBekostigd = NA, .after = "ECTS" ) %>%
    add_column( duitseDeelstaat = NA, .after = "indicatieAcademischZiekenhuis" ) %>%
    add_column( indicatieWoonplaatsVereiste = NA, .after = "duitseDeelstaat" )
  # Gegevens HISBEK en VLPBEK samenvoegen
  data <- rbind( hisbek, vlpbek )
   # next lines commented out
  # Afhankelijk van parameter wel/niet opslaan in bestand (1) of database (2)
#   switch( saveto,
#           '1' = {
#             print( "Writing to file..." )
#             write.csv2( data, paste0( "TOTBEK_", yyyy, "_", yyyymmdd, "_28DN.csv" ),
#                         row.names = F,
#                         quote = FALSE,
#                         na = "" )
#           },
#         '2' = {
#             print( "Writing to database..." )
#             # Datatypes van kolommen bepalen: allemaal varchar, verschillende lengte
#             fieldtypes <- paste0( "varchar(",
#                                   c( 3, 9, 9, 4, 4, 20, 1, 50, 4, 5, 6, 1, 8, 8, 1,
#                                      1, 2, 8, 2, 4, 4, 32, 14, 1, 1, 1, 2, 1, 1, 1 ),
#                                   ")" )
#             names(fieldtypes) <- fields
#             # Verbinden met SQL-server database
#             con <- dbConnect( odbc(),
#                               .connection_string = "Driver={SQL Server};
# [Server=[nader in te vullen],[poortnummer];
# Database=[databasenaam];
# Trusted_Connection=Yes;" )
#             # Data overzetten naar SQL-server
#             dbWriteTable( con,
#                           DBI::SQL( paste0( "dbo.tmp_bronho_", yyyy ) ),
#                           data,
#                           field.types = as.list( fieldtypes ) )
#             # Verbinding met database sluiten
#             dbDisconnect(con)
#           }
#   )
  return(data)
}


# data2025 <- read.hisvlp( 2025, "20231016", 2 )
data2025 <- read.hisvlp(locationname_hisbek, locationname_vlpbek)




#### SQL mutations, but now done in R ------------------------------------------

vlphisbek_2025 <- data2025 %>%
 # -- Studiejaar afleiden uit datum inschrijving
 #ALTER TABLE vlphisbek_2025 ADD Studiejaar VARCHAR(4)
 #UPDATE vlphisbek_2025 SET Studiejaar = SUBSTRING(datumInschrijving,1,4)
 #UPDATE vlphisbek_2025 SET Studiejaar = Studiejaar - 1 WHERE SUBSTRING(datumInschrijving,1,4)*1 < 1994 AND SUBSTRING(datumInschrijving,5,2)*1 < 8
 #UPDATE vlphisbek_2025 SET Studiejaar = Studiejaar - 1 WHERE SUBSTRING(datumInschrijving,1,4)*1 >= 1994 AND SUBSTRING(datumInschrijving,5,2)*1 < 9
 # following the suggestion in vu-hva, that in 1994 there was a change in start academic year
  mutate(datumInschrijving = ymd(datumInschrijving)) %>%
  mutate(Studiejaar = case_when(year(datumInschrijving) < 1994 & month(datumInschrijving) < 8 ~ year(datumInschrijving) - 1,
                                   year(datumInschrijving) < 1994 & month(datumInschrijving) >= 8 ~ year(datumInschrijving),
                                   year(datumInschrijving) >= 1994 & month(datumInschrijving) < 9 ~ year(datumInschrijving) - 1,
                                   year(datumInschrijving) >= 1994 & month(datumInschrijving) >= 9 ~ year(datumInschrijving))) %>%
#  -- Actief in september/oktober
# ALTER TABLE vlphisbek_2025 ADD vroegestart VARCHAR(1)
# UPDATE vlphisbek_2025 SET vroegestart = 1 WHERE SUBSTRING(datumInschrijving,1,4) = Studiejaar AND SUBSTRING(datumInschrijving,5,2)*1 <= 10
# UPDATE vlphisbek_2025 SET vroegestart = 0 WHERE vroegestart IS NULL
  # started in september or october, FALSE = 0, TRUE = 1
  mutate(vroegestart = as.integer((Studiejaar == year(datumInschrijving)) & (month(datumInschrijving) <= 10))) %>%
#  -- Persoonsnummer obv twee andere nummers
# ALTER TABLE vlphisbek_2025 ADD Persoonsnummer VARCHAR(9)
# UPDATE vlphisbek_2025 SET Persoonsnummer = Burgerservicenummer
# UPDATE vlphisbek_2025 SET Persoonsnummer = Onderwijsnummer WHERE Persoonsnummer IS NULL
  mutate(Persoonsnummer = case_when(Burgerservicenummer == "" ~ as.character(Onderwijsnummer),
                                   TRUE ~ Burgerservicenummer))


#### More SQL ------------------------------------------------------------------

tmp_wisselstroom2025 <- vlphisbek_2025 %>%
  # -- Unieke records voor persoon per studiejaar-brin-opleidingscode-onderwijsvorm
  # SELECT DISTINCT Persoonsnummer, Studiejaar, Brin, opleidingscode, onderwijsvorm INTO
  # dbo.tmp_wisselstroom2025 FROM vlphisbek_2025 WHERE vroegestart = 1
  filter(vroegestart == 1) %>%
  distinct(Persoonsnummer, Studiejaar, Brin, opleidingscode, onderwijsvorm) %>%
  # -- Instroomjaar bij deze brin-opleidingscode-onderwijsvorm
  # ALTER TABLE tmp_wisselstroom2025 ADD Instroomjaar VARCHAR(4)
  # UPDATE tmp_wisselstroom2025 SET Instroomjaar = (
  #   SELECT MIN(Studiejaar) FROM tmp_wisselstroom2025 b
  #   WHERE tmp_wisselstroom2025.Persoonsnummer = b.Persoonsnummer
  #   AND tmp_wisselstroom2025.Brin = b.Brin
  #   AND tmp_wisselstroom2025.opleidingscode = b.opleidingscode
  #   AND tmp_wisselstroom2025.onderwijsvorm = b.onderwijsvorm )
  group_by(Persoonsnummer, Brin, opleidingscode, onderwijsvorm) %>%
  mutate(Instroomjaar = min(Studiejaar)) %>%
  ungroup() %>%
  #   -- Hoogste jaar bij de VU
  # ALTER TABLE tmp_wisselstroom2025 ADD LaatsteJaarVU VARCHAR(4)
  # UPDATE tmp_wisselstroom2025 SET LaatsteJaarVU = (
  #   SELECT MAX(Studiejaar) FROM tmp_wisselstroom2025 b
  #   WHERE tmp_wisselstroom2025.Persoonsnummer = b.Persoonsnummer
  #  AND b.Brin = '21PL'
  #  AND tmp_wisselstroom2025.Studiejaar >= b.Studiejaar )
  #  MMj: changed this to find designated brin_of_interest
  group_by(Persoonsnummer) %>%
  arrange(Persoonsnummer, Studiejaar, Brin) %>%
  mutate(Laatstejaar_brin_of_interest = case_when(Brin == brin_of_interest ~ Studiejaar,
                                                  TRUE ~ NA_integer_)) %>%
  fill(Laatstejaar_brin_of_interest) %>%
  ungroup() %>%
  #   -- Laatste opleiding bij de VU
  # ALTER TABLE tmp_wisselstroom2025 ADD LaatsteOpleidingVU VARCHAR(5)
  # UPDATE tmp_wisselstroom2025 SET LaatsteOpleidingVU = (
  #   SELECT MAX(opleidingscode) FROM tmp_wisselstroom2025 b
  #   WHERE tmp_wisselstroom2025.Persoonsnummer = b.Persoonsnummer
  #   AND b.Brin = '21PL'
  #   AND tmp_wisselstroom2025.LaatsteJaarVU = b.Studiejaar )
  # WHERE LaatsteJaarVU IS NOT NULL
  # MMJ: this saves the maximum code, so master trumps bachelor, within bachelor no real meaning
  # and of course ad starts with an 8 that trumps master(code starts with 4) as well.
  # and, since opleidingscode is read as integer, old opleidingsnummers starting with a zero lose the starting zero,
  # will be regarded higher. of course the zero numbers are masters, so equivalend with 4 now
  group_by(Persoonsnummer, Studiejaar, Brin) %>%
  arrange(Persoonsnummer, Studiejaar, Brin) %>%
  mutate(LaatsteOpleiding_brin_of_interest = case_when(Brin == brin_of_interest ~ max(opleidingscode),
                                                       TRUE ~ NA_integer_)) %>%
  ungroup() %>%
  group_by(Persoonsnummer, Laatstejaar_brin_of_interest) %>%
  fill(LaatsteOpleiding_brin_of_interest) %>%
  ungroup() %>%
  # -- Aantal jaar bij de VU (historie *voor* dit studiejaar)
  # ALTER TABLE tmp_wisselstroom2025 ADD StudiejarenVU VARCHAR(4)
  # UPDATE tmp_wisselstroom2025 SET StudiejarenVU = (
  # SELECT COUNT(DISTINCT(Studiejaar)) FROM tmp_wisselstroom2025 b
  # WHERE tmp_wisselstroom2025.Persoonsnummer = b.Persoonsnummer
  # AND b.Brin = '21PL'
  # AND tmp_wisselstroom2025.Studiejaar > b.Studiejaar )
  # WHERE LaatsteJaarVU IS NOT NULL
  group_by(Persoonsnummer, Brin, Studiejaar) %>%
  arrange(Persoonsnummer, Brin, Studiejaar) %>%
  mutate(Studiejaren_brin_of_interest = case_when(Brin == brin_of_interest ~ 1,
                                                      TRUE ~ 0)) %>%
  ungroup() %>%
  group_by(Persoonsnummer) %>%
  arrange(Studiejaar, Brin) %>%
  mutate(Studiejaren_brin_of_interest = cumsum(Studiejaren_brin_of_interest)) %>%
  ungroup()
  

### Some intermediate results --------------------------------------------------

# Following page 17/21 of the VU_HvA doc

# -- Instroom
# SELECT Studiejaar, COUNT(*) FROM tmp_wisselstroom2025 WHERE brin = '28DN' AND Studiejaar = Instroomjaar
# GROUP BY Studiejaar ORDER BY Studiejaar
# MMJ: select amount of rows per studiejaar for own brin
# MMJ: not really wise for older years, as this gives only historical data about students enrolled in current years

tmp_wisselstroom2025 %>%
  filter(Brin == brin_vlpbek,
         Studiejaar == Instroomjaar) %>%
  count(Studiejaar)

# -- Inschrijvingen
# SELECT Studiejaar, COUNT(*) FROM tmp_wisselstroom2025 WHERE brin = '28DN' 
# GROUP BY Studiejaar ORDER BY Studiejaar

tmp_wisselstroom2025 %>%
  filter(Brin == brin_vlpbek) %>%
  count(Studiejaar)


# -- Telling doorstroom
# SELECT studiejaar, COUNT(*) FROM tmp_wisselstroom2025 WHERE brin = '28DN' AND studiejaar >= 2021 
# AND instroomjaar = studiejaar AND LaatstejaarVU < studiejaar AND LaatstejaarVU >= studiejaar - 5 GROUP BY 
# studiejaar ORDER BY studiejaar
# MMJ: amount of instroom from brin of interest, with at most 5 years between brin of interest and own brin

tmp_wisselstroom2025 %>%
  filter(Brin == brin_vlpbek,
         Studiejaar > 2021,
         Instroomjaar == Studiejaar,
         Laatstejaar_brin_of_interest < Studiejaar,
         Laatstejaar_brin_of_interest >= Studiejaar - 5) %>%
  count(Studiejaar)


# todo (bottom page 16 of VU_HvA doc)

#### Collect all the tmp's in one file -----------------------------------------

# MMJ: cannot do that, since i only have one year of data
# so I only add one year

# DROP TABLE tmp_wisselstroom
# SELECT * INTO tmp_wisselstroom FROM tmp_wisselstroom2021 WHERE 
# brin = '28DN' AND studiejaar IN (2017,2018) AND instroomjaar = studiejaar AND LaatstejaarVU < studiejaar AND LaatstejaarVU >= studiejaar - 5
# INSERT INTO tmp_wisselstroom SELECT * FROM tmp_wisselstroom2022 WHERE
# brin = '28DN' AND studiejaar = 2019 AND instroomjaar = studiejaar AND LaatstejaarVU < studiejaar AND LaatstejaarVU >= studiejaar - 5
# INSERT INTO tmp_wisselstroom SELECT * FROM tmp_wisselstroom2023 WHERE
# brin = '28DN' AND studiejaar = 2020 AND instroomjaar = studiejaar AND LaatstejaarVU < studiejaar AND LaatstejaarVU >= studiejaar - 5
# INSERT INTO tmp_wisselstroom SELECT * FROM tmp_wisselstroom2024 WHERE
# brin = '28DN' AND studiejaar = 2021 AND instroomjaar = studiejaar AND LaatstejaarVU < studiejaar AND LaatstejaarVU >= studiejaar - 5
# INSERT INTO tmp_wisselstroom SELECT * FROM tmp_wisselstroom2025 WHERE
# brin = '28DN' AND studiejaar IN (2022,2023) AND instroomjaar = studiejaar AND LaatstejaarVU < studiejaar AND LaatstejaarVU >= studiejaar - 5
  
tmp_wisselstroom <- bind_rows(
  tmp_wisselstroom2025 %>% filter(Brin == brin_vlpbek,
                                  Instroomjaar == Studiejaar,
                                  Laatstejaar_brin_of_interest < Studiejaar,
                                  Laatstejaar_brin_of_interest >= Studiejaar - 5)
)  

#### Make selection for excel --------------------------------------------------

# -- Selectie voor draaitabel Excel
# SELECT opleidingscode,onderwijsvorm,Instroomjaar,LaatsteJaarVU,LaatsteOpleidingVU,StudiejarenVU,COUNT(*) AS Aantal
# FROM tmp_wisselstroom
# GROUP BY opleidingscode,onderwijsvorm,Instroomjaar,LaatsteJaarVU,LaatsteOpleidingVU,StudiejarenVU

data_for_excel <- tmp_wisselstroom %>%
  count(opleidingscode, onderwijsvorm, Instroomjaar, Laatstejaar_brin_of_interest,
        LaatsteOpleiding_brin_of_interest, Studiejaren_brin_of_interest)

####  Enrich SQL dataset in excel ----------------------------------------------

data_for_excel %>%
  # [Tussenjaren] = [Instroomjaar] -/- [LaatstejaarVU] -/- 1
  mutate(Tussenjaren= Instroomjaar - Laatstejaar_brin_of_interest - 1) %>%
  # [Directe Doorstroom] = IF [Tussenjaren] = 0 THEN ‘JA’ ELSE ‘NEE’
  mutate(`Directe Doorstroom` = ifelse(Tussenjaren == 0,
                                       "ja",
                                       "nee"))

# Voeg de omschrijvingen bijbehorend bij het croho/ isat code toe
# MMJ: this can be done via a left_join with a table containing the data
# i did not do that here, since no code for it in the VU_HvA doc