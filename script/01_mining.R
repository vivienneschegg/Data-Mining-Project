#Creating a Dataframe for my firms (first idea )

firms_data <- data.frame(
  name = c("DePoly", "Bloom Biorenewables", "Divea", "CellX", "Composite Recycling", 
           "Grensol", "Tethys Robotics", "Oxyle", "CompPair", "WattAnyWhere", 
           "Seprify", "Exnaton", "Enerdrape", "viboo", "Perovskia Solar", 
           "Alpiq", "Novartis", "Axpo", "Planted", "Nestlé"),
  
  type = c(rep("Startup", 15), rep("Incumbent", 5)),
  
  url = c(
          "https://www.depoly.co/about", "https://www.bloombiorenewables.com/company", 
          "https://www.divea.ch/", "https://www.cellx.ch/", "https://composite-recycling.ch/our-solution/", 
          "https://grensolgroup.com/", "https://www.tethys-robotics.ch/company/our-mission", 
          "https://oxyle.com/about", "https://www.comppair.ch/sustainability", "https://www.wattanywhere.com/about/", 
          "https://seprify.com/about", "https://www.exnaton.ai/en/about-us", "https://enerdrape.com/en/about-us/", 
          "https://viboo.io/en/uber-uns", "https://perovskia.solar/about-us/", 
          "https://www.alpiq.com/sustainability", "https://www.novartis.com/esg/environmental-sustainability", 
          "https://www.axpo.com/group/en/about-us/sustainability.html", "https://eatplanted.com/pages/sustainability", 
          "https://www.nestle.com/sustainability"),
  source_page = c(
    "About us", "Company", "Mission", "Home", "Our solution", 
    "Our impact", "Our mission", "About", "Sustainability", "About Us", 
    "About", "About us", "About us", "About us", "About us", 
    "Sustainability", "Environmental Sustainability", "Sustainability", "Sustainability", "Sustainability"
  ),
  
  stringsAsFactors = FALSE
)

head(firms_data)

#Data.frame without the subpages (implementing the critics)
firms2_data <- data.frame(
  name = c("DePoly", "Bloom Biorenewables", "Divea", "CellX", "Composite Recycling", 
           "Grensol", "Tethys Robotics", "Oxyle", "CompPair", "WattAnyWhere", 
           "Seprify", "Exnaton", "Enerdrape", "viboo", "Perovskia Solar", 
           "Alpiq", "Novartis", "Axpo", "Planted", "Nestlé"),
  
  type = c(rep("Startup", 15), rep("Incumbent", 5)),
  
  url = c(
    "https://www.depoly.co/", "https://www.bloombiorenewables.com/", 
    "https://www.divea.ch/", "https://www.cellx.ch/", "https://composite-recycling.ch/", 
    "https://grensolgroup.com/", "https://www.tethys-robotics.ch/", 
    "https://oxyle.com/", "https://www.comppair.ch/", "https://www.wattanywhere.com/", 
    "https://seprify.com/", "https://www.exnaton.ai/", "https://enerdrape.com/", 
    "https://viboo.io/", "https://perovskia.solar/", 
    "https://www.alpiq.com/", "https://www.novartis.com/", 
    "https://www.axpo.com/", "https://eatplanted.com/", 
    "https://www.nestle.com/"),
  
  stringsAsFactors = FALSE
)

library(tidyverse)
write_csv(firms2_data, "firms_data_starting.csv")



