#Creating a Dataframe for my firms

firms_data <- data.frame(
  name = c("DePoly", "Bloom Biorenewables", "Divea", "CellX", "Composite Recycling", 
           "Grensol", "Tethys Robotics", "Oxyle", "CompPair", "WattAnyWhere", 
           "Seprify", "Exnaton", "Enerdrape", "viboo", "Perovskia Solar", 
           "Alpiq", "Novartis", "Axpo", "Planted", "Nestlé"),
  
  type = c(rep("Startup", 15), rep("Incumbent", 5)),
  
  url = c(
          "https://www.depoly.co/about", "https://www.bloombiorenewables.com/company", 
          "https://www.divea.ch/", "https://www.cellx.ch/", "https://composite-recycling.ch/our-solution/", 
          "https://grensolgroup.com/", "https://www.tethys-robotics.ch/de-ch/company/our-mission", 
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
  
  category = c("Recycling", "Biorenewables", "Water/Mission", "FoodTech", "Recycling", 
               "Impact/Energy", "Robotics", "Water Treatment", "Material Science", "Energy", 
               "Materials", "Energy/AI", "Energy", "Energy/Efficiency", "Solar Energy", 
               "Energy", "Pharma", "Energy", "Food", "Food/Consumer Goods"),
  
  stringsAsFactors = FALSE
)

head(firms_data)

