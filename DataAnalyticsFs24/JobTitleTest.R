DataGlassdoor <- read.csv("/Users/joyackermann/Documents/Universität/Universität Basel (MSc)/Data Analytics/War for Talents/glassdoor_reviews.csv")
DatenGlassdoor <- read.csv("C:/Users/nussb/OneDrive - Universität Basel/FS24/data science/glassdoor_reviews.csv")


str(DatenGlassdoor)

# Zähle die Anzahl der Vorkommen jeder Jobbezeichnung, Location & Firma
job_counts <- table(DatenGlassdoor$job_title)
location_counts = table(DatenGlassdoor$location)
firm_counts=table(DatenGlassdoor$firm)

# Wandele die Ergebnisse in eine Datenrahmen um und sortiere sie absteigend nach der Häufigkeit
job_counts_df <- data.frame(Job_Title = names(job_counts), Count = as.numeric(job_counts))
job_counts_df <- job_counts_df[order(job_counts_df$Count, decreasing = TRUE),]
location_counts_df =data.frame(Location = names(location_counts), Count = as.numeric(location_counts))
location_counts_df =location_counts_df[order(location_counts_df$Count, decreasing = TRUE),]
firm_counts_df =data.frame(Firm = names(firm_counts), Count = as.numeric(firm_counts))
firm_counts_df =firm_counts_df[order(firm_counts_df$Count, decreasing = TRUE),]
  
# Ausgabe der ersten 30 Zeilen der Tabelle. Zahl kann angepasst werden
head(job_counts_df, 30)
head(location_counts_df, 30)
head(firm_counts_df, 30)

# Alle ausgeben lassen, jedoch kann R  nicht alle anzeigen
print(job_counts_df)
print(location_counts_df)
print(firm_counts_df)


