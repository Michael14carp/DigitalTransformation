DatenGlassdoor <- read.csv("/Users/joyackermann/Documents/Universität/Universität Basel (MSc)/Data Analytics/War for Talents/glassdoor_reviews.csv")
DatenGlassdoor <- read.csv("C:/Users/nussb/OneDrive - Universität Basel/FS24/data science/glassdoor_reviews.csv")


str(DatenGlassdoor)


# Zähle die Anzahl der Vorkommen jeder Jobbezeichnung, Location & Firma

job_counts <- table(DatenGlassdoor$job_title)
location_counts = table(DatenGlassdoor$location)
firm_counts = table(DatenGlassdoor$firm)


# Wandele die Ergebnisse in eine Datenrahmen um und sortiere sie absteigend nach der Häufigkeit
job_counts_df <- data.frame(Job_Title = names(job_counts), Count = as.numeric(job_counts))
job_counts_df <- job_counts_df[order(job_counts_df$Count, decreasing = TRUE),]
location_counts_df <- data.frame(Location = names(location_counts), Count = as.numeric(location_counts))
location_counts_df <- location_counts_df[order(location_counts_df$Count, decreasing = TRUE),]
firm_counts_df <- data.frame(Firm = names(firm_counts), Count = as.numeric(firm_counts))
firm_counts_df <- firm_counts_df[order(firm_counts_df$Count, decreasing = TRUE),]
  
# Ausgabe der ersten 30 Zeilen der Tabelle. Zahl kann angepasst werden
head(job_counts_df, 30)
head(location_counts_df, 30)
head(firm_counts_df, 30)

# Alle ausgeben lassen, jedoch kann R  nicht alle anzeigen
print(job_counts_df)
print(location_counts_df)
print(firm_counts_df)


# check how many NA's in whole dataset

sum(is.na(DatenGlassdoor))

# check how many NA's in each rating

sum(is.na(DatenGlassdoor$overall_rating))
sum(is.na(DatenGlassdoor$work_life_balance))
sum(is.na(DatenGlassdoor$culture_values))
sum(is.na(DatenGlassdoor$diversity_inclusion))
sum(is.na(DatenGlassdoor$career_opp))
sum(is.na(DatenGlassdoor$comp_benefits))
sum(is.na(DatenGlassdoor$senior_mgmt))
sum(is.na(DatenGlassdoor$recommend))
sum(is.na(DatenGlassdoor$ceo_approv))
sum(is.na(DatenGlassdoor$outlook))

# check how many empty cells in each column
# job title

empty_cells_job_title <- DatenGlassdoor$job_title == " "
sum_empty_cells_job_title <- sum(empty_cells_job_title)
print(sum_empty_cells_job_title)

# location
empty_cells_location <- DatenGlassdoor$location == ""
sum_empty_cells_location <- sum(empty_cells_location)
print(sum_empty_cells_location)

# check how many software engineers in dataset

software_engineer <- DatenGlassdoor$job_title == "Software Engineer"
as.factor(software_engineer)
sum_software_engineer <- sum(software_engineer)
print(sum_software_engineer)

# delete all rows with NA variables
DataGlassdoor <- na.omit(DatenGlassdoor)
print(na.omit(DatenGlassdoor))

# delete all rows with empty variables
CleanDataGlassdoor <- DataGlassdoor[rowSums(DataGlassdoor == "") == 0, ]
# nächster code funktioniert aus irgendeinem grund nicht 
DataGlassdoor_1 <- CleanDataGlassdoor[-c(585759, 707520, 767175),]

job_title_non_empty <- table(FullDataGlassdoor$job_title)
job_title_non_empty <- data.frame(Job_Title = names(job_title_non_empty), Count = as.numeric(job_title_non_empty))

# add country column


# Nur Personen, die Software Engineer im job_title haben
# Filtere den Datensatz nach Personen mit "Software Engineer" im Jobtitel
SoftwareEngineerData <- DatenGlassdoor[grep("Software Engineer", DatenGlassdoor$job_title), ]

# Überprüfe den neuen Datensatz

