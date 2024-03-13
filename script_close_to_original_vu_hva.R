################################################################################
###
### Based on Wisselstroom script VU --> HvA
### as read in:
### "Wisselstroom - Doorstroom VU naar HvA v1.3.pdf"
### by Bastiaan van der Wulp & Annemarie Engels
### 2024_01_22
###
### This script, :
### Martine Jansen
### 2024_03_13
###
################################################################################

### comments on diverting from original script ---------------------------------

# location of funding files hisbek and vlpbek
# read in files




#### libraries -----------------------------------------------------------------

library(tidyverse)


### location of funding files hisbek and vlpbek --------------------------------

# Original script not in project form,
# files seem to be located in working directory
# I chose to let the user specify the location for working directory
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

# -- Unieke records voor persoon per studiejaar-brin-opleidingscode-onderwijsvorm
# SELECT DISTINCT Persoonsnummer, Studiejaar, Brin, opleidingscode, onderwijsvorm INTO
# dbo.tmp_wisselstroom2025 FROM vlphisbek_2025 WHERE vroegestart = 1

tmp_wisselstroom2025 <- vlphisbek_2025 %>%
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
  ungroup() 

# todo (bottom page 16 of VU_HvA doc)


  
  