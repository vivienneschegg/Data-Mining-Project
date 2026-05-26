# ==============================================================================
# PROJEKT: Masterseminararbeit - Institutioneller Isomorphismus
# SKRIPT: Vollständige Erstellung der erweiterten Firmen-Dataframes
# ==============================================================================

library(tidyverse)

# ------------------------------------------------------------------------------
# 1. Variante: Erster Ansatz mit spezifischen Unterseiten (firms_data)
# ------------------------------------------------------------------------------

firms_data <- data.frame(
  name = c(
    # Startups (30 Firmen)
    "DePoly", "Bloom Biorenewables", "Divea", "CellX", "Composite Recycling", 
    "Grensol", "Tethys Robotics", "Oxyle", "CompPair", "WattAnyWhere", 
    "Seprify", "Exnaton", "Enerdrape", "viboo", "Perovskia Solar", 
    "Planted Foods", "Corintis", "Voltiris", "MobyFly", "Climeworks", 
    "Verretex", "Neology", "HAYA Therapeutics", "Battrion", "Synhelion", 
    "ClearSpace", "Kyburz Switzerland", "Wingtra", "Yali Bio", "ANYbotics",
    # Incumbents (15 Firmen)
    "Alpiq", "Novartis", "Axpo", "Nestle", "BKW", 
    "Romande Energie", "Holcim", "SBB", "UBS", "ZKB", 
    "Migros", "Swisscom", "Swiss Prime Site", "Geberit", "ABB Schweiz"
  ),
  
  type = c(rep("Startup", 30), rep("Incumbent", 15)),
  
  url = c(
    # Startups - Spezifische Unterseiten 
    "https://www.depoly.co/about", "https://bloombiorenewables.com/company", 
    "https://www.divea.ch/", "https://www.cellx.ch/", "https://www.composite-recycling.ch/our-solution/", 
    "https://grensolgroup.com/", "https://www.tethys-robotics.ch/company/our-mission", 
    "https://oxyle.com/about", "https://www.comppair.ch/sustainability", "https://www.wattanywhere.com/about/", 
    "https://seprify.com/about", "https://www.exnaton.ai/en/about-us", "https://enerdrape.com/en/about-us/", 
    "https://viboo.io/en/uber-uns", "https://perovskia.solar/about-us/", 
    "https://www.planted.com/pages/sustainability", "https://www.corintis.com/", "https://www.voltiris.com/", "https://www.mobyfly.com/", "https://www.climeworks.com/", 
    "https://www.verretex.com/", "https://www.neology.ch/", "https://www.hayatx.com/", "https://www.battrion.com/", "https://synhelion.com/", 
    "https://clearspace.today/", "https://kyburz-switzerland.ch/", "https://wingtra.com/", "https://www.yali-bio.com/", "https://www.anybotics.com/",
    # Incumbents - Spezifische Unterseiten 
    "https://www.alpiq.com/sustainability", "https://www.novartis.com/esg/environmental-sustainability", 
    "https://www.axpo.com/group/en/about-us/sustainability.html", "https://www.nestle.com/sustainability", "https://www.bkw.ch/de/ueber-uns/nachhaltigkeit", 
    "https://www.romande-energie.ch/groupe/engagements", "https://www.holcim.ch/nachhaltigkeit", "https://company.sbb.ch/de/ueber-uns/verantwortung/nachhaltigkeit.html", "https://www.ubs.com/global/en/sustainability-reporting.html", "https://www.zkb.ch/de/unsere-bank/nachhaltigkeit.html", 
    "https://report.migros.ch/de/nachhaltigkeit/", "https://www.swisscom.ch/de/about/nachhaltigkeit.html", "https://www.sps.swiss/verantwortung", "https://www.geberit.com/unternehmen/nachhaltigkeit/", "https://new.abb.com/ch/ueber-uns/nachhaltigkeit"
  ),
  
  source_page = c(
    # Startups 
    "About us", "Company", "Mission", "Home", "Our solution", 
    "Our impact", "Our mission", "About", "Sustainability", "About Us", 
    "About", "About us", "About us", "About us", "About us", 
    "Sustainability", "Home", "Home", "Home", "Home", 
    "Home", "Home", "Home", "Home", "Home", 
    "Home", "Home", "Home", "Home", "Home",
    # Incumbents 
    "Sustainability", "Environmental Sustainability", "Sustainability", "Sustainability", "Sustainability",
    "Engagements", "Sustainability", "Sustainability", "Sustainability", "Sustainability",
    "Sustainability", "Sustainability", "Sustainability", "Sustainability", "Sustainability"
  ),
  
  stringsAsFactors = FALSE
)

print("--- ERSTER ANSATZ (MIT UNTERSEITEN) ---")
head(firms_data)


# ------------------------------------------------------------------------------
# 2. Variante: Bereinigter Ansatz ohne Unterseiten für den automatischen Crawler (firms2_data)
# ------------------------------------------------------------------------------
firms2_data <- data.frame(
  name = c(
    # Startups (30 Firmen)
    "DePoly", "Bloom Biorenewables", "Divea", "CellX", "Composite Recycling", 
    "Grensol", "Tethys Robotics", "Oxyle", "CompPair", "WattAnyWhere", 
    "Seprify", "Exnaton", "Enerdrape", "viboo", "Perovskia Solar", 
    "Planted Foods", "Corintis", "Voltiris", "MobyFly", "Climeworks", 
    "Verretex", "Neology", "HAYA Therapeutics", "Battrion", "Synhelion", 
    "ClearSpace", "Kyburz Switzerland", "Wingtra", "Yali Bio", "ANYbotics",
    # Incumbents (15 Firmen)
    "Alpiq", "Novartis", "Axpo", "Nestle", "BKW", 
    "Romande Energie", "Holcim", "SBB", "UBS", "ZKB", 
    "Migros", "Swisscom", "Swiss Prime Site", "Geberit", "ABB Schweiz"
  ),
  
  type = c(rep("Startup", 30), rep("Incumbent", 15)),
  
  url = c(
    # Startups - Nur Hauptdomains 
    "https://www.depoly.co/", "https://bloombiorenewables.com/", 
    "https://www.divea.ch/", "https://www.cellx.ch/", "https://www.composite-recycling.ch/", 
    "https://grensolgroup.com/", "https://www.tethys-robotics.ch/", 
    "https://oxyle.com/", "https://www.comppair.ch/", "https://www.wattanywhere.com/", 
    "https://seprify.com/", "https://www.exnaton.ai/", "https://enerdrape.com/", 
    "https://viboo.io/", "https://perovskia.solar/", 
    "https://www.planted.com/", "https://www.corintis.com/", "https://www.voltiris.com/", "https://www.mobyfly.com/", "https://www.climeworks.com/", 
    "https://www.verretex.com/", "https://www.neology.ch/", "https://www.hayatx.com/", "https://www.battrion.com/", "https://synhelion.com/", 
    "https://clearspace.today/", "https://kyburz-switzerland.ch/", "https://wingtra.com/", "https://www.yali-bio.com/", "https://www.anybotics.com/",
    # Incumbents - Nur Hauptdomains 
    "https://www.alpiq.com/", "https://www.novartis.com/", 
    "https://www.axpo.com/", "https://www.nestle.com/", "https://www.bkw.ch/", 
    "https://www.romande-energie.ch/", "https://www.holcim.ch/", "https://www.sbb.ch/", "https://www.ubs.com/", "https://www.zkb.ch/", 
    "https://www.migros.ch/", "https://www.swisscom.ch/", "https://www.sps.swiss/", "https://www.geberit.com/", "https://new.abb.com/ch"
  ),
  
  stringsAsFactors = FALSE
)

print("--- ZWEITER ANSATZ (NUR HAUPTDOMAINS) ---")
print(firms2_data)

# Verteilung überprüfen zur Kontrolle
print("Verteilung der Firmen-Typen:")
table(firms2_data$type)

# ------------------------------------------------------------------------------
# 3. Export in die CSV-Datei für die Pipeline
# ------------------------------------------------------------------------------
write_csv(firms2_data, "firms_data_starting.csv")
print("Datei 'firms_data_starting.csv' erfolgreich mit 45 Firmen exportiert!")
