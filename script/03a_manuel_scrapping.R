# ==============================================================================
# PROJEKT: Masterseminararbeit - Institutioneller Isomorphismus
# SKRIPT 03b: Manuelle Ergänzung der 5 durch Bot-Detection blockierten
#             Incumbents (Nestle, Holcim, SBB, Swisscom, ABB Schweiz)
# Jede Firma ist EINE Zeile, subpage_url listet ALLE Quell-Unterseiten
# (Semikolon-getrennt), content fasst alle Unterseiten-Texte zusammen.
#
# Methode: Für jede Unterseite wurde über die Browser-Entwicklerkonsole
# derselbe DOM-Selektor wie im automatisierten Crawler angewendet:
#   Array.from(document.querySelectorAll('p, h1, h2, h3'))
#     .map(el => el.innerText.trim()).filter(t => t.length > 0).join(' ')
#
# HINWEIS: /about/management (Nestlé, Team-/Führungskräfte-Seite mit
# Namen und persönlichen Profilen) wurde bewusst NICHT erhoben, konsistent
# mit dem Ausschluss von Team-Seiten mit Personendaten im automatisierten
# Crawler (Skript 02, Entfernung des Keywords "team").
#
# Abruf-/Erhebungsdatum: [07.09.2026]
# ==============================================================================

library(dplyr)
library(jsonlite)
library(readr)

# ------------------------------------------------------------------------------
# --- Nestlé (17 Quellen) ---
# ------------------------------------------------------------------------------
nestle_pages <- data.frame(
  name = "Nestle",
  type = "Incumbent",
  subpage_url = "https://www.nestle.com/sustainability ; https://www.nestle.com/about/how-we-do-business/purpose-values ; https://www.nestle.com/sustainability/responsible-business/ethics ; https://www.nestle.com/about/overview ; https://www.nestle.com/about/strategy ; https://www.nestle.com/about/how-we-do-business ; https://www.nestle.com/about/research-development ; https://www.nestle.com/about/history ; https://www.nestle.com/about/quality-safety ; https://www.nestle.com/sustainability/climate-change ; https://www.nestle.com/sustainability/nature-environment ; https://www.nestle.com/sustainability/water ; https://www.nestle.com/sustainability/packaging-circularity ; https://www.nestle.com/sustainability/responsible-sourcing ; https://www.nestle.com/sustainability/human-rights ; https://www.nestle.com/sustainability/people-communities ; https://www.nestle.com/sustainability/performance-reporting",
  content = paste(
    "Sustainability at Nestle. Our purpose is to unlock the power of food and beverages to enhance quality of life for everyone, today and for generations to come. Creating Shared Value, our strong conviction that a company should create value both for its shareholders and society at large, is at the heart of this purpose.",
    "Our purpose and values. The Nestle purpose is to unlock the power of food to enhance quality of life for everyone, today and tomorrow. Our values are rooted in respect.",
    "Fostering a culture of business ethics. For Nestle to be successful over time, we must continue to earn and retain the trust of our employees, consumers, customers, suppliers, shareholders and wider society.",
    "At a glance. A world-leading brand portfolio: over 30 billionaire brands. A global presence: products sold in 185 countries, CHF 89.5 billion sales in 2025. A diverse modern workforce: around 271000 employees, 48.2% of middle and senior management are female. A pioneer of innovation: CHF 1.7 billion annual investment in R&D. A force for good: taking action for the climate to reach net zero by 2050.",
    "Our business strategy. Nestle is the Good food, Good life company. We aim to achieve sustainable, profitable growth, returning to organic growth of 4% plus, with an underlying trading operating profit margin of 17% plus. Strategic priorities: sharpening our portfolio around Coffee, Petcare, Nutrition and Food and Snacks, prioritizing RIG-led growth, accelerating our business transformation.",
    "How we do business. We live our purpose and values day in and day out. Creating Shared Value is integral to the way we do business, guided by our Corporate Business Principles. Speak Up is Nestle's independent non-compliance reporting system.",
    "Innovation, science and technology. We invest CHF 1.7 billion every year in R&D, with 4000 employees worldwide across 22 R&D locations. Our innovation capabilities span nutrition and taste, food safety and quality, sustainability.",
    "Our history. 160 years of expertise in nutrition, health and wellness, since Henri Nestle and the Page brothers laid the foundations of our company in 1866.",
    "Quality and safety. Quality and safety for our consumers is Nestle's top priority. Our Quality Management System starts on farms, we work with farmers to improve material quality and adopt sustainable practices. In 2023 our Quality Assurance Centers performed more than 4.1 million analytical tests.",
    "Climate change. We aim to reduce our GHG emissions by 50% by 2030 versus 2018 baseline, and reach net zero by 2050. 24.52% net reduction achieved in 2025. 96.70% of primary supply chains assessed as deforestation-free.",
    "Nature. The ingredients we use come from nature, we aim to source half of our key ingredients from farmers adopting regenerative agriculture practices by 2030. Currently 27.6% of volumes sourced this way.",
    "Water stewardship. Water is vital for our product manufacturing, factory operations and crop cultivation. 39 Nestle Waters sites are certified to the Alliance for Water Stewardship Standard.",
    "Packaging and circularity. Our vision is that none of our packaging ends up in landfills or as litter. Circularity is about designing out waste and pollution, keeping materials in use at their highest value.",
    "Responsible sourcing. We aim for 100% of key ingredients volumes to be responsibly sourced by 2030, representing 87% of total raw materials purchased volumes globally.",
    "Human rights. Respecting and promoting human rights is integral to our business strategy. 96.2% of cocoa volume is covered by due diligence systems for child labor risks.",
    "Caring about our people and communities. Our employees are at the heart of being a force for good. 48.2% of management positions are held by women.",
    "Performance and reporting. Transparent public reporting on our activities is embedded in how we do business. 2025 performance: 24.5% GHG reduction, 96.7% deforestation-free supply chains, 27.6% regenerative agriculture sourcing."
  ),
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------------------------
# --- Holcim (18 Quellen) ---
# ------------------------------------------------------------------------------
holcim_pages <- data.frame(
  name = "Holcim",
  type = "Incumbent",
  subpage_url = "https://www.holcim.ch/de/nachhaltigkeit ; https://www.holcim.ch/de/unsere-werte ; https://www.holcim.ch/de/wer-wir-sind ; https://www.holcim.ch/de/unsere-geschichte ; https://www.holcim.ch/de/was-uns-antreibt ; https://www.holcim.ch/de/nextgen-growth-action ; https://www.holcim.ch/de/corporate-governance ; https://www.holcim.ch/de/gesundheit-und-sicherheit ; https://www.holcim.ch/de/dekarbonisierung ; https://www.holcim.ch/de/ccus-und-rekarbonatisierung ; https://www.holcim.ch/de/erneuerbare-energie ; https://www.holcim.ch/de/nachhaltige-logistik ; https://www.holcim.ch/de/luftreinhaltung ; https://www.holcim.ch/de/kreislaufwirtschaft-1 ; https://www.holcim.ch/de/biodiversitaet ; https://www.holcim.ch/de/austausch-mit-gemeinden-und-bevoelkerung ; https://www.holcim.ch/de/innovation-und-zusammenarbeit-mit-hochschulen-und-start-ups ; https://www.holcim.ch/de/vielfalt-und-inklusion",
  content = paste(
    "Fuer eine nachhaltig gebaute Zukunft. Als fuehrende Baustoffherstellerin foerdern wir eine Zukunft des Bauens, die uns alle weiterbringt. Nachhaltigkeit als Kern unserer Strategie: Wir gestalten das Bauen neu, mit Loesungen, die zirkulaer, widerstandsfaehig, CO2-arm und energieeffizient sind. Unser Ziel: Netto-Null bis 2050 und vollstaendig rezyklierbare Baustoffe. Klimaneutralitaet: Wir haben seit 1990 fast 40 Prozent CO2-Netto-Emissionen pro Tonne Zement eingespart. Dekarbonisierung, CO2-Abscheidung und -Speicherung, Erneuerbare Energie, Nachhaltige Logistik, Luftreinhaltung, Kreislaufwirtschaft: In der Schweiz werden bereits heute 75 Prozent der Aushub- und Abbruchmaterialien wiederverwertet. Umwelt und Ressourcen, Gesellschaftliche Verantwortung, Innovation und Zusammenarbeit mit Hochschulen und Start-Ups, Vielfalt und Inklusion.",
    "Unsere Vision. Innovation und Nachhaltigkeit stehen im Zentrum unserer Geschaeftstaetigkeit. Bis 2050 produzieren wir klimaneutrale und vollstaendig rezyklierbare Baustoffe. Mit unserer Strategie NextGen Growth 2030 gestalten wir eine neue Generation des Wachstums. Unsere Werte: Holcim verpflichtet sich den Werten Purpose, People und Performance. Purpose: Nachhaltigkeit ist Teil unseres unternehmerischen Selbstverstaendnisses. People: Wir haben den Anspruch, den besten Arbeitsplatz fuer unsere Mitarbeitenden zu schaffen. Performance: Wir sind neugierig und offen und suchen stets nach neuen, zukunftsweisenden Technologien und Loesungen.",
    "Wer wir sind. Die Holcim Schweiz AG ist die fuehrende Partnerin fuer nachhaltiges Bauen in der Schweiz und gestaltet die gebaute Umwelt von der Infrastruktur ueber die Industrie bis hin zu Gebaeuden. Das Unternehmen produziert an fast 60 Standorten in der Schweiz Beton, Kies und Zement und recycelt Baumaterialien aus Rueckbauten. Holcim Schweiz beschaeftigt rund 1500 Mitarbeitende. Unser Geschaeft: Die Grundpfeiler des taeglichen Handelns bilden Innovation, Nachhaltigkeit und Partnerschaft. Unser Zement Susteno ist der erste ressourcenschonende Zement weltweit mit hochwertig aufbereitetem Mischgranulat aus rueckgebauten Gebaeuden. Globaler Konzern mit Schweizer Wurzeln: Holcim Schweiz ist eine Tochtergesellschaft von Holcim Ltd, vertreten in rund 45 Laendern mit 48300 Mitarbeitenden.",
    "Holcim Schweiz, eine Zeitreise durch mehr als ein Jahrhundert. 1912: Adolf Gygi gruendet die Aargauische Portlandcement-Fabrik in Holderbank. 1958: Boersengang der Holdinggesellschaft HOFI. 1978: Thomas Schmidheiny uebernimmt die operative Leitung. 2001: Namenswechsel von Holderbank zu Holcim. 2003: Gruendung der Holcim Foundation for Sustainable Construction. 2015: Fusion mit dem franzoesischen Unternehmen Lafarge zu LafargeHolcim, dem weltweit groessten Anbieter von Baustoffen. 2021: Rebranding zurueck zu Holcim, mit strategischer Neuausrichtung auf Kreislaufwirtschaft, Dekarbonisierung und nachhaltiges Bauen. 2024: Uebernahme der Cand-Landi SA. 2025: Umsetzung der Strategie NextGen Growth 2030.",
    "Unsere Unternehmenskultur. Unsere Mitarbeitenden sind unser hoechstes Gut, ihre Kreativitaet und Leidenschaft sind entscheidend fuer unseren Erfolg. Wir verfolgen einen ganzheitlichen Ansatz, bei dem die Gesundheit unserer Mitarbeitenden hoechste Prioritaet hat, mit dem Ziel Zero Harm to People. Gesundheit und Soziales, Work-Life-Balance, Entwicklung und Karriere. Unsere Werte und Prinzipien: Unsere Unternehmenskultur wird getragen durch unsere Strategie NextGen Growth 2030 und den Holcim Spirit, gepraegt von Purpose, People und Performance. Vielfalt und Inklusion: Unabhaengig von Geschlecht, Alter oder Nationalitaet bieten wir allen Mitarbeitenden die gleichen Chancen. Soziale Verantwortung: Gesellschaftliche Verantwortung bedeutet fuer uns, ein ethisches Geschaeftsverhalten festzulegen und ein fairer Arbeitgeber zu sein.",
    "Unsere Strategie NextGen Growth 2030. Holcim hat seine neue Strategie NextGen Growth 2030 angekuendigt, um der fuehrende Partner fuer nachhaltiges Bauen zu werden. Holcim wird seine Fuehrungsrolle im Bereich Nachhaltigkeit nutzen, um profitables Wachstum in Europa, Australien und Nordafrika zu erzielen. Nachhaltigkeit foerdert profitables Wachstum: Holcim erweitert sein nachhaltiges Angebot mit ECOPact und ECOPlanet, strebt fuer 2030 eine Menge von 20 Millionen Tonnen recycelten Bauabbruchmaterials an. Ausbau hochwertiger Bauloesungen. Leistungskultur und Wertschoepfung: Die Leistungskultur von Holcim beruht auf einem dezentralen, schlanken Geschaeftsmodell.",
    "Corporate Governance. Transparenz gegenueber unseren externen und internen Stakeholdern sowie die Einhaltung von gesetzlichen Vorgaben bestimmen unser Denken und Handeln. Unser Leitbild sind die Vorgaben des Swiss Code of Best Practice for Corporate Governance sowie die Corporate-Governance-Richtlinie der SIX Swiss Exchange. Unser Verhaltenskodex bildet die Grundlage fuer jegliche Geschaeftstaetigkeit und ist fuer all unsere Mitarbeitenden verbindlich. Verhaltenskodex fuer Lieferanten: Wir streben langfristige Lieferantenbeziehungen an, die sich zu einer nachhaltigen Entwicklung verpflichten. Integrity-Line: ein internes Beratungs- und Meldesystem fuer Fragen oder Bedenken in Bezug auf ethisch korrekte Geschaeftspraktiken.",
    "Gesundheit und Sicherheit. Unsere Ambition ist, nicht nur bei nachhaltigen und innovativen Bauloesungen fuehrend zu sein, sondern auch bezueglich Gesundheits- und Sicherheitsstandards. Unser vorrangiges Ziel ist es, unsere Geschaefte unfallfrei zu fuehren. Ambition Zero: null Schaden fuer Mensch und Umwelt. Unsere Strategie basiert auf fuenf Pfeilern: Fuehrung und Kompetenz, Risikomanagement fuer Schluesselprozesse, Gesundheit, Systeme und digitale Instrumente sowie Sicherheit im Strassenverkehr. Kontinuierliches Lernen: Wir haben eine Kultur der Lessons Learned geschaffen. Foerderung der Gesundheit unserer Belegschaft und Partner.",
    "Dekarbonisierung. Der gesellschaftliche Bedarf an Infrastruktur ist enorm, ein grosses Potenzial zur CO2-Reduktion sehen wir bei der Produktion von Zementen mit tieferem Klinkerfaktor. Alternative Brennstoffe: wir setzen auf alternative Brennstoffe anstelle fossiler Energien. Projekt Flame: Im Werk Eclepens kann der Anteil an alternativen Brennstoffen von 70 auf 95 Prozent gesteigert werden, dadurch spart Holcim jaehrlich rund 40000 Tonnen CO2. Holcim EcoPlanet: ein Zement, der den CO2-Ausstoss um bis zu 39 Prozent reduziert. Nachhaltige Transportloesungen: Fuer die zweite Gotthardroehre liefern wir 150000 Tonnen Zement ausschliesslich mit elektrischen Lkws.",
    "CO2-Abscheidung, -Nutzung und -Speicherung. Die Abscheidung, Nutzung und Speicherung von CO2 ist ein zentraler Baustein unserer Netto-Null-Strategie. Wir entwickeln rund zehn CCUS-Projekte, die Holcim Gruppe erforscht die Moeglichkeiten in mehr als 30 weltweiten Pilotprojekten. Holcim gehoert zum Konsortium CO2-Plume Geothermal, gefuehrt von der ETH Zuerich, mit dem Ziel, Erdwaerme unter Nutzung von CO2 zu gewinnen und das CO2 im Untergrund zu speichern. Strategische Partnerschaft mit neustark zur dauerhaften Speicherung von CO2 in rezyklierten mineralischen Abfaellen.",
    "Erneuerbare Energie. Bis 2030 wollen wir 10 Prozent unseres Stromverbrauchs aus eigener Produktion abdecken. Strom aus Waermerueckgewinnungsanlagen: Unsere Anlagen in Untervaz und Eclepens produzieren zusammen rund 11000 MWh pro Jahr Strom. Holcim und AEW betreiben eine der groessten Solaranlagen im Aargau mit 2,4 Megawatt-Peakleistung. Fernwaerme fuer Haushalte: die Industriepartnerschaft Cadcime speist Abwaerme in ein Fernwaermenetz ein, das etwa dem Bedarf von 2300 Haushalten entspricht.",
    "Nachhaltige Logistik. Grosses Potenzial, um unsere indirekten Emissionen zu senken, sehen wir in der Zement-, Kies- und Betonlogistik. Super-Charger-Ladestation in Siggenthal fuer elektrische Lkws. Verlagerung auf die Bahn: Wir setzen in der Schweiz etwa 700 Bahnwagen fuer den Gueterverkehr ein, damit sparen wir im Vergleich zum Strassentransport circa 98 Prozent CO2 ein.",
    "Luftreinhaltung. Im Zementofen bilden sich wegen der hohen Verbrennungstemperaturen Stickoxide, Schwefeldioxid und Staub. Wir verwenden neueste Technologien wie moderne Schlauchfilter. NOx-Emissionen unter den Grenzwerten: wir reduzieren die Stickoxidemissionen seit Jahren unter den gesetzlichen Grenzwert. Pilotprojekt Flue gas recirculation reduziert die Bildung von Stickoxiden. Siggenthal verfuegt ueber eine weltweit einmalige Absorptionsanlage zur Reduktion von Schwefeldioxid.",
    "Kreislaufwirtschaft. In der Schweiz werden bereits heute 75 Prozent der Aushub- und Abbruchmaterialien wiederverwertet und rund 85 Prozent des Betons rezykliert. Unter den Leitprinzipien Reduzieren, Wiederverwenden und Rezyklieren sucht Holcim zukunftsfaehige Bauloesungen. Susteno, der ressourcenschonende Zement, verursacht 10 Prozent weniger CO2-Emissionen. Geocycle sorgt fuer einen geschlossenen Materialkreislauf. Referenzprojekte: Pantanal-Voliere im Zoo Zuerich, Innovationslabor Grueze, Rippmann Floor System, Sanierung Arosertunnel.",
    "Biodiversitaet. Wir foerdern die lokale Biodiversitaet an unseren Abbaustandorten mit gezielten Massnahmen. Bisher haben wir ca. 400 Hektar Land rekultiviert und renaturiert. Unsere Kiesgruben und Steinbrueche bilden Pionierflaechen und Rueckzugsgebiete fuer seltene Tier- und Pflanzenarten. Konkrete Aktionsplaene mit Umweltbehoerden und Naturschutzorganisationen, zum Beispiel mit Birdlife Aargau kuenstliche Brutwaende fuer Uferschwalben. Kooperation mit der Stiftung Natur und Wirtschaft.",
    "Austausch mit Gemeinden und Bevoelkerung. Wir informieren unsere Nachbarinnen und Nachbarn regelmaessig ueber Aktivitaeten in und um unsere Werke. Tag der offenen Tuer: Wir oeffnen unsere Tueren und bieten Fuehrungen an. Regionalrat: regelmaessige Treffen mit Vertretern der Kantone, umliegenden Gemeinden und lokalen Naturschutzorganisationen.",
    "Innovation und Zusammenarbeit mit Hochschulen und Start-Ups. Dialog mit Stakeholdern durch die jaehrlich organisierte Betontagung und Wirtschaftswochen fuer Schuelerinnen und Schueler. Zusammenarbeit mit Hochschulen: das ultraleichte Deckensystem HiLo, entwickelt mit der ETH Block Research Group, benoetigt ueber 70 Prozent weniger Baustoffe. Zusammenarbeit mit Start-ups: Kooperation mit dem britischen Start-up HyBird fuer einen virtuellen 3D-Zwilling des Zementwerks Siggenthal.",
    "Vielfalt und Inklusion. Mit Mitarbeitenden aus fast 40 Laendern streben wir ein integratives Arbeitsumfeld an. Wir setzen uns fuer die Chancengleichheit aller Mitarbeitenden ein, unabhaengig von Geschlecht, Alter, Sprache, Herkunft, Kultur, Nationalitaet, Religion oder sexueller Orientierung. Partnerschaft mit Advance zur Erhoehung des Frauenanteils. Schluesselzahlen: 20,6 Prozent der Fuehrungsfunktionen mit Frauen besetzt, 37 Nationalitaeten vertreten. Programm GLOWH fuer Hochschulabsolventinnen und -absolventen."
  ),
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------------------------
# --- SBB (7 Quellen) ---
# ------------------------------------------------------------------------------
sbb_pages <- data.frame(
  name = "SBB",
  type = "Incumbent",
  subpage_url = "https://company.sbb.ch/de/unternehmen/verantwortung/nachhaltigkeit.html ; https://company.sbb.ch/de/ueber-die-sbb/verantwortung/nachhaltigkeit/nachhaltigkeitsengagement.html ; https://company.sbb.ch/de/unternehmen/ueber-uns.html ; https://company.sbb.ch/de/unternehmen/verantwortung/sicherheit.html ; https://company.sbb.ch/de/unternehmen/verantwortung/kunden.html ; https://company.sbb.ch/de/unternehmen/verantwortung/gesellschaft.html ; https://company.sbb.ch/de/unternehmen/verantwortung/partnerschaften.html",
  content = paste(
    "Nachhaltigkeit bei der SBB. Als SBB setzen wir uns mit ueber 200 Nachhaltigkeitsmassnahmen dafuer ein, dass Ihre Zugreise noch klimafreundlicher ist. Energieeffizient: Jedes Jahr sparen wir den jaehrlichen Stromverbrauch von 120000 Haushalten ein. Klimafreundlich: Zugfahren verursacht nur 0,3 Prozent des CO2-Ausstosses des gesamten Verkehrs der Schweiz. Biodiversitaet: Mit unseren Gruenflaechen entlang der Schienen leisten wir einen wichtigen Beitrag zur Biodiversitaet. Netto-Null bis 2040: Um die Pariser Klimaziele zu erreichen, reduzieren wir unsere CO2-Emissionen massiv. Swisstainable: Die SBB ist Teil des Nachhaltigkeits-Programms Swisstainable, hoechstes Level III. Menschenrechte, Anpassung an den Klimawandel, Nachhaltige Baustellen, Laermschutz, Abfallbewirtschaftung.",
    "Nachhaltigkeitsengagement. Die SBB uebernimmt als groesste Anbieterin von nachhaltiger Mobilitaet in der Schweiz Verantwortung. Die SBB verpflichtet sich zu einer umfassenden Nachhaltigkeit, wirtschaftlich, oekologisch und sozial. Die SBB beruecksichtigt Nachhaltigkeitskriterien konsequent entlang der ganzen Wertschoepfungskette. Wir foerdern nachhaltige, vernetzte Lebensraeume fuer Mensch und Natur. Wir bieten unseren Kundinnen und Kunden eine umweltfreundliche, barrierefreie Mobilitaet. Wir bieten sinnstiftende Arbeit fuer heutige und kuenftige Mitarbeitende. Als eine der groessten Arbeitgeberinnen der Schweiz ist die SBB fuer rund 35000 Mitarbeitende verantwortlich.",
    "Ueber uns. SBB, weil Verbindungen die Schweiz ausmachen. Strategie SBB 2030: Mobilitaet veraendert sich stark und wird volatiler, unsere Kund:innen erwarten mehr Flexibilitaet, der Effizienzdruck steigt. Leitbild: Raison d'etre und Werte bilden zusammen das Leitbild SBB, sie geben uns langfristig Sinn und Orientierung. Geschaeftsbericht: Dank erneutem Rekordstand an Reisenden schreibt die SBB im Geschaeftsjahr 2025 wieder schwarze Zahlen. Organisation: Der SBB Konzern ist aufgeteilt in vier Divisionen. Die Marke SBB: Mit einer starken Marke differenziert sich die SBB, schafft Werte und Vertrauen. Geschichte: Im Jahr 1902 begann die bewegte Geschichte der Schweizerischen Bundesbahnen SBB.",
    "Sicher unterwegs mit der SBB. Sicherheit hat bei der SBB hoechste Prioritaet, fuer ihre Kundinnen und Kunden wie auch ihre Mitarbeitenden. Grundregeln beim Reisen: Zum Fahrvergnuegen ohne unnoetige Risiken koennen auch Sie beitragen. Datenschutz: Die SBB haelt sich im Umgang mit Kundendaten an die geltenden Datenschutzbestimmungen. Informationssicherheit: Die SBB sieht die Informationssicherheit als einen geschaeftskritischen Faktor fuer die Bahnsicherheit. Sicherheitsorganisationen und Sicherheit im Bahnumfeld: Die SBB ergreift verschiedene Massnahmen fuer die Sicherheit von Kundinnen und Mitarbeitenden.",
    "Unsere Kunden sollen sich bei uns wohl fuehlen. Unsere Kundinnen und Kunden sollen sich bei der SBB wohlfuehlen, gut aufgehoben sein und gut ankommen. Um zu wissen, was sie bewegt, befragen wir jedes Jahr 20000 Reisende. Puenktlichkeit: Unsere Kundinnen und Kunden sicher und puenktlich ans Ziel zu bringen, ist unser oberstes Ziel. Kundenstimme: Werden Sie eine SBB Kundenstimme, Sie koennen bei der SBB mitreden und mitbestimmen. SBB Community: Frag unsere Kundinnen und Kunden, stelle Fragen, teile deine Erfahrungen.",
    "Politik und Gesellschaft. Als groesstes Transportunternehmen der Schweiz ist die SBB in eine Vielzahl von politischen Themen eingebunden. Nachhaltige Mobilitaet: Mobilitaet ist die Grundlage fuer eine gut funktionierende Gesellschaft und Wirtschaft. Stellungnahmen: Kaum ein anderes Unternehmen in der Schweiz ist so stark von politischen Entscheidungen abhaengig wie die SBB. Positionspapiere: Als bundesnahes Unternehmen ist die SBB eingebettet in die Schweizerische Gesellschaft und Politik.",
    "Als Partner bei uns einsteigen. Als Unternehmen im Besitz des Bundes betreibt die SBB grundsaetzlich kein Sponsoring. Ausnahme bilden Partnerschaften, welche in einem engen oder direkten Zusammenhang mit unserer Taetigkeit als Transportunternehmung des oeffentlichen Verkehrs stehen. Wichtige Kriterien: Wirtschaftlicher Nutzen, Bezug zu Produkten und Dienstleistungen, Unterstuetzung Marketingziele, keine Geldleistung, starke Rolle der SBB, markenstarke Partner."
  ),
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------------------------
# --- Swisscom (7 Quellen) ---
# ------------------------------------------------------------------------------
swisscom_pages <- data.frame(
  name = "Swisscom",
  type = "Incumbent",
  subpage_url = "https://www.swisscom.ch/de/about/nachhaltigkeit.html ; https://www.swisscom.ch/de/about/nachhaltigkeit/ethik-corporate-responsibility-governance.html ; https://www.swisscom.ch/de/about/nachhaltigkeit/umwelt.html ; https://www.swisscom.ch/de/about/nachhaltigkeit/gesellschaft.html ; https://www.swisscom.ch/de/about/nachhaltigkeit/handeln.html ; https://www.swisscom.ch/de/about/nachhaltigkeit/ziele.html ; https://www.swisscom.ch/de/about/nachhaltigkeit/partner.html",
  content = paste(
    "Nachhaltigkeit. Als nachhaltiges ICT-Unternehmen integrieren wir seit unserer Gruendung Nachhaltigkeit-Ueberlegungen zu Umwelteinfluessen, sozialen Aspekten und Governance in saemtliche Unternehmensentscheidungen. Gruppen-Nachhaltigkeitsstrategie 2030: Die Swisscom Gruppe uebernimmt Verantwortung fuer laenderspezifische Prioritaeten in der Schweiz und Italien. Unser Versprechen fuer die Umwelt: Bis 2035 wollen wir Netto-Null erreichen. Unser Versprechen fuer die Gesellschaft: Wir uebernehmen Verantwortung fuer die Gesellschaft. Unser Versprechen als verantwortungsbewusste Leaderin: Wir verfolgen hohe Standards in Governance, Sicherheit und Ethik.",
    "ESG Framework und Ethik. Swisscom setzt auf eine nachhaltige und ethisch verantwortungsvolle Unternehmensfuehrung. Environment: Oekologisches Framework, Umweltpolitik, Wasser, Kreislaufwirtschaft, Biodiversitaet. Social: Soziales Framework, Menschenrechte, Diversity Equity and Inclusion, Jugendmedienschutz, Datensicherheit, Artificial Intelligence und Datenethik. Governance: Fuehrungs-Framework, Verhaltenskodex, Anti-Korruption, Anti-Geldwaesche, Whistleblower, Risikomanagement, Compliance Management, Steuerpolitik.",
    "Unser Versprechen fuer die Umwelt. Bis 2035 wollen wir Netto-Null erreichen, im Einklang mit der Science Based Targets Initiative und dem 1,5 Grad-Ziel des Pariser Abkommens. Zusammensetzung unserer Emissionen: Scope 1 3.9 Prozent, Scope 2 0 Prozent, Scope 3 96.1 Prozent. Reduktion direkter Emissionen: Nachhaltige Mobilitaet, Erneuerbare Energien, Effiziente Infrastruktur. Biodiversitaet: Wir wollen Verantwortung fuer unseren Einfluss uebernehmen und uns aktiv fuer den Schutz lokaler Naturraeume einsetzen.",
    "Unser Versprechen fuer die Gesellschaft. Wir befaehigen die Menschen, sich in der digitalen Welt kompetent zu bewegen. Digitale Inklusion foerdern: Wir unterstuetzen rund 2 Millionen Menschen pro Jahr. Engagement fuer die Schweiz: Soziales Engagement ist ein zentraler Teil der Unternehmenskultur. Fuer Top Talente: 12935 Mitarbeitende aus 100 Nationen. Wohlbefinden: Wir staerken Gesundheit und Wohlbefinden unserer Mitarbeitenden.",
    "Unser Versprechen als verantwortungsbewusste Leaderin. Wir verstehen uns als Teil der Gesellschaft und verfolgen hohe Standards in Governance, Sicherheit und Ethik. Verantwortungsvolle KI: Swisscom bewertet KI-Systeme im Rahmen eines risikobasierten KI-Governance-Prozesses. Kontrollinstrumente: Corporate Responsibility Governance, Risikomanagement. Faire Lieferketten: Wir legen Wert auf eine faire Partnerschaft mit Lieferanten, die unsere Werte teilen.",
    "Nachhaltigkeitsziele. Als fuehrendes ICT-Unternehmen in der Schweiz und Italien will Swisscom die Chancen der digitalen Transformation nutzen, Risiken minimieren und die Zukunft mitgestalten. Unsere Nachhaltigkeitsgruppenstrategie orientiert sich an den 17 globalen Nachhaltigkeitszielen der UNO. Faire Lieferketten: Wir sind Mitglied der Joint Alliance for CSR.",
    "Partnerschaften fuer Nachhaltigkeit. Unser Engagement fuer eine nachhaltige Zukunft geht ueber die Swisscom hinaus, wir setzen auf langjaehrige Partnerschaften in den Bereichen Klimaschutz, faires Arbeiten, Digitalisierung sowie soziale Verantwortung. Umwelt: Energie-Agentur der Wirtschaft, European Telecommunications Network Operators Association, Science Based Targets initiative. myclimate und South Pole, Partner fuer den Bezug von CO2 Zertifikaten. swisscleantech, seit 2019 Mitglied des Verbands fuer eine klimataugliche Wirtschaft."
  ),
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------------------------
# --- ABB Schweiz (3 Quellen) ---
# ------------------------------------------------------------------------------
abb_pages <- data.frame(
  name = "ABB Schweiz",
  type = "Incumbent",
  subpage_url = "https://new.abb.com/ch/nachhaltigkeit-abb-schweiz ; https://new.abb.com/ch/Du-und-ABB ; https://new.abb.com/ch/ueber-uns",
  content = paste(
    "Nachhaltigkeit ABB Schweiz. ABB Schweiz treibt die Energiewende voran und foerdert den sozialen Fortschritt, sowohl im eigenen Betrieb als auch bei Kunden. Wir ermoeglichen eine kohlenstoffarme Gesellschaft. Im eigenen Betrieb: Wir setzen auf Energieeffizienz, 100 Prozent erneuerbare Energien und eine vollelektrische Fahrzeugflotte. Seit 2019 konnte ABB Schweiz ihren CO2-Ausstoss mehr als halbieren. Sozialer Fortschritt: Soziales Engagement ist tief verankert in der Unternehmenskultur von ABB Schweiz. Verpflichtungen: Auf dem Weg zu Netto-Null bis 2050 orientiert sich ABB am internationalen Standard der Science Based Targets Initiative.",
    "Du und ABB, das bedeutet Engagement fuer unsere Mitmenschen, fuer Vielfalt und Wohlbefinden von rund 4000 Mitarbeitenden bei ABB Schweiz. Dein Wohlbefinden: Wir schaffen die Rahmenbedingungen, du lebst Wellbeing im Alltag. Seit ueber 80 Jahren unterstuetzt die ABB Sozialberatung Mitarbeitende von ABB Schweiz und ihre Angehoerigen. Dein Potenzial: Erfolgsfaktor Vielfalt, ABB Schweiz foerdert gezielt das Interesse an Technik bei Maedchen und Frauen. Deine Community: Erfolgsfaktor Engagement, ABB Schweiz hat den Volunteer Day eingefuehrt.",
    "Ueber uns. ABB ist ein fuehrendes Technologieunternehmen in den Bereichen Elektrifizierung und Automation, das eine nachhaltigere und ressourceneffizientere Zukunft ermoeglicht. Das Unternehmen blickt auf eine ueber 140-jaehrige Geschichte zurueck und beschaeftigt mehr als 105000 Mitarbeitende weltweit."
  ),
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------------------------
# Alle Blöcke zusammenführen
# ------------------------------------------------------------------------------
manual_incumbents <- bind_rows(nestle_pages, holcim_pages, sbb_pages,
                               swisscom_pages, abb_pages)

print(manual_incumbents %>% select(name, subpage_url))

# ------------------------------------------------------------------------------
# An bestehende research_data anhängen
# ------------------------------------------------------------------------------
if (!exists("research_data")) {
  research_data <- fromJSON(readLines("full_research_data.json", warn = FALSE))
}

research_data <- bind_rows(research_data, manual_incumbents)

cat("Neue Gesamtzahl Firmen in research_data:",
    length(unique(research_data$name)), "\n")
cat("Davon Incumbents:",
    length(unique(research_data$name[research_data$type == "Incumbent"])), "\n")

# ------------------------------------------------------------------------------
# Aktualisierte Version speichern
# ------------------------------------------------------------------------------
write_json(research_data, "full_research_data.json", pretty = TRUE)
