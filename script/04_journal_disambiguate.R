# Disambigutate journal names
setwd("~/Box/citation_classifier/")
df <- fread("data/trial1_references.csv")
df$container <- base::trimws(df$container)

container_match_journal.o <- df$container %in% scimago.j$title
table(container_match_journal.o) # 7

## CLEAN JOURNAL ABBREVIATIONS ----
### From what I can tell, gsub is slower
df$journal.disam <- df$container
# Adv[.]? should be Advances -- see Adv for some inspiration
df$journal.disam <- str_replace(df$journal.disam, "Adv\\b|Advn\\b", "Advances in")
# Agric[.]? for Agricultur
df$journal.disam <- str_replace(df$journal.disam, "Agric\\b", "Agriculture")
# Anim. = Animal
df$journal.disam <- str_replace(df$journal.disam, "Anim\\b", "Animal")
# Am J = American journal of
df$journal.disam <- str_replace(df$journal.disam, "Am\\sJ\\b", "American Journal of")
# Am = America at end
df$journal.disam <- str_replace(df$journal.disam, "Am$|Amer$", "America")
# Am = American 
df$journal.disam <- str_replace(df$journal.disam, "Am\\b|Amer\\b", "American")
# Ann. is Annals of -- removing because impossible to discern between Annual, which can also be Ann.
#df$journal <- str_replace(df$journal, "Ann\\b", "Annals")
# Annu. is annual
df$journal.disam <- str_replace(df$journal.disam, "Annu\\b", "Annual")
# Atmos.is Atmospheric
df$journal.disam <- str_replace(df$journal.disam, "Atmos\\b", "Atmospheric")
# Assoc.= Association
df$journal.disam <- str_replace(df$journal.disam, "Assoc\\b", "Association")
# Appl. is Applied
df$journal.disam <- str_replace(df$journal.disam, "Appl\\b", "Applied")
# Biol. = Biology
df$journal.disam <- str_replace(df$journal.disam, "Biol\\b", "Biology")
#Behav = Behavior
df$journal.disam <- str_replace(df$journal.disam, "Behav\\b", "Behavior")
#Bull = Bulletin at end
df$journal.disam <- str_replace(df$journal.disam, "Bull$", "Bulletin")
#Bull = Bulletin
df$journal.disam <- str_replace(df$journal.disam, "Bull\\b", "Bulletin of")
# Cem Bas Mat
df$journal.disam <- str_replace(df$journal.disam, "Cem\\sBas\\sMat[a-z]?\\b", "Cement-Based Materials")
# Cem Bas Mat
df$journal.disam <- str_replace(df$journal.disam, "Cem\\-Bas\\sMat[a-z]?\\b", "Cement-Based Materials")
# Civ = Civil
df$journal.disam <- str_replace(df$journal.disam, "Civ\\b", "Civil")
# Climatol = Climatology
df$journal.disam <- str_replace(df$journal.disam, "Climatol\\b", "Climatology")
# Conf = Consference
df$journal.disam <- str_replace(df$journal.disam, "Conf\\b", "Conference")
# Conserv = Conservation
df$journal.disam <- str_replace(df$journal.disam, "Conserv\\b", "Conservation")
# Comput = Computing
df$journal.disam <- str_replace(df$journal.disam, "Comput\\b", "Computing")
# Constr = Constructions
df$journal.disam <- str_replace(df$journal.disam, "Constr\\b", "Construction")
# Corro = Corrosion
df$journal.disam <- str_replace(df$journal.disam, "Corros?\\b", "Corrosion")
# Croat == Croation
df$journal.disam <- str_replace(df$journal.disam, "Croat?\\b", "Croatian")
# Earthq.= Earthquake
df$journal.disam <- str_replace(df$journal.disam, "Earthq\\b", "Earthquake")
# Ecol[.]? should be Ecology
df$journal.disam <- str_replace(df$journal.disam, "Ecol\\b", "Ecology")
# Eng[.]? should be Engineering
df$journal.disam <- str_replace(df$journal.disam, "Eng\\b", "Engineering")
# Ent[.]? should be Entomology
df$journal.disam <- str_replace(df$journal.disam, "Ent\\b|Entomol\\b", "Entomology")
# Environ. = Environment at end
df$journal.disam <- str_replace(df$journal.disam, "Environ$|Envt$|Envir$", "Environment")
# Environ. = Environmtnal
df$journal.disam <- str_replace(df$journal.disam, "Environ\\b|Env\\b|Envir\\b", "Environmental")
# Ergon Ergonomics
df$journal.disam <- str_replace(df$journal.disam, "Ergon\\b", "Ergonomics")
# Epidemiol
df$journal.disam <- str_replace(df$journal.disam, "Epidemiol\\b", "Epidemiology")
# European Euro
df$journal.disam <- str_replace(df$journal.disam, "Euro?\\b", "European")
# Genet
df$journal.disam <- str_replace(df$journal.disam, "Genet\\b", "Genetics")
# Geophys
df$journal.disam <- str_replace(df$journal.disam, "Geophys\\b", "Geophysics")
# Geol. = Geology
df$journal.disam <- str_replace(df$journal.disam, "Geol\\b", "Geology")
# Geoenv Geoenvi.
df$journal.disam <- str_replace(df$journal.disam, "Geo[Ee]nvi?r?o?n?\\b", "Geoenvironmental")
# Geotech
df$journal.disam <- str_replace(df$journal.disam, "Geotech\\b", "Geotechnical")
# Hous
df$journal.disam <- str_replace(df$journal.disam, "Hous\\b", "Housing")
# Hydrogeol
df$journal.disam <- str_replace(df$journal.disam, "Hydrogeol\\b", "Hydrogeology")
# Hydrol
df$journal.disam <- str_replace(df$journal.disam, "Hydrol\\b", "Hydrology")
# Ieee
df$journal.disam <- str_replace(df$journal.disam, "^Ieee\\b", "IEEE")
# Int = International
df$journal.disam <- str_replace(df$journal.disam, "Int\\b", "International")
# J[.]? should be journal, if at end
df$journal.disam <- str_replace(df$journal.disam, "\\bJ$", "Journal")
# J[.]? should be journal of, if at start
df$journal.disam <- str_replace(df$journal.disam, "\\bJ\\,?\\b", "Journal of")
# Mater = Materials
df$journal.disam <- str_replace(df$journal.disam, "Mat\\b|Mater\\b", "Materials")
# Mech = Mechanical
df$journal.disam <- str_replace(df$journal.disam, "Mech\\b", "Mechanical")
# Ornith = Ornithology
df$journal.disam <- str_replace(df$journal.disam, "Ornith\\b", "Ornithology")
# Psychol = Psychology
df$journal.disam <- str_replace(df$journal.disam, "Psychol\\b", "Psychology")
# Sci = Science
df$journal.disam <- str_replace(df$journal.disam, "Sci\\b", "Science")
# Seis.= Siesmic
df$journal.disam <- str_replace(df$journal.disam, "Seism?\\b", "Seismological")
# Soc = Society
df$journal.disam <- str_replace(df$journal.disam, "Soc\\b", "Society")
# Sociol = Sociology
df$journal.disam <- str_replace(df$journal.disam, "Sociol\\b", "Sociology")
# Softw = Software
df$journal.disam <- str_replace(df$journal.disam, "Softw\\b", "Software")
# Stud
df$journal.disam <- str_replace(df$journal.disam, "Stud\\b", "Studies")
# Struct = Structural
df$journal.disam <- str_replace(df$journal.disam, "Struct\\b", "Structural")
# Resour. = Resources
df$journal.disam <- str_replace(df$journal.disam, "Resour\\b", "Resources")
# Res. = Research
df$journal.disam <- str_replace(df$journal.disam, "Res\\b", "Research")
# Rev. = Review at end
df$journal.disam <- str_replace(df$journal.disam, "Rev$", "Review")
# Rev. = Review of
df$journal.disam <- str_replace(df$journal.disam, "Rev\\b", "Review of")
# Zool = Zoology
df$journal.disam <- str_replace(df$journal.disam, "Zool\\b", "Zoology")

# Checking on match improvement
df$journal.disam <- trimws(df$journal.disam)
journal_match_journal.n <- df$journal.disam %in% scimago.j$title
table(journal_match_journal.n) # no improvement
look <- select(df, container, journal.disam)
fwrite(df, "data/gsp_references_clean.csv")

