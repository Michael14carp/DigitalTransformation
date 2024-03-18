DatenGlassdoor <- read.csv("/Users/joyackermann/Documents/Universität/Universität Basel (MSc)/Data Analytics/War for Talents/glassdoor_reviews.csv")
DatenGlassdoor <- read.csv("C:/Users/nussb/OneDrive - Universität Basel/FS24/data science/glassdoor_reviews.csv")

# Fragestellung 1: Vergleich SE und anderer Job
# Fragestellung 2: Vergleich Top 3 Branchen (mit oder ohne Anonymus?)
# Fragestellung 3: Nur SE
# Fragestellung 4: Vergleich SE zwischen 2 Branchen


# Filtere den Datensatz nach Personen mit "Software Engineer" im Jobtitel
SoftwareEngineerData <- DatenGlassdoor[grep("Software Engineer", DatenGlassdoor$job_title), ]
# Filtere den Datensatz nach Personen mit "Manager" im Jobtitel
ManagerData <- DatenGlassdoor[grep("Manager", DatenGlassdoor$job_title), ]
# Setze den ursprünglichen Datensatz ohne Software Engineers; Hier müssen noch anonymus und fehlende job title entfernt werden? 
RestlicheJobsData <- subset(DatenGlassdoor, !(job_title %in% SoftwareEngineerData$job_title))


#  Branchen
FinanceData=DatenGlassdoor[grep("J-P-Morgan|Citi|HSBC-Holdings|Barclays|Thomson-Reuters|American-Express|Morgan-Stanley|Goldman-Sachs|BNY-Mellon|Deutsche-Bank|", DatenGlassdoor$firm), ]
TechData=DatenGlassdoor[grep("IBM|Oracle|Microsoft|Apple|Google|SAP|Salesforce", DatenGlassdoor$firm), ]
ConsultingData=DatenGlassdoor[grep("Deloitte|EY|PwC|KPMG", DatenGlassdoor$firm), ]
FoodData=DatenGlassdoor[grep("McDonald-s|Pizza-Hut", DatenGlassdoor$firm), ]
# Subway, Burger Kind, Dominos

# Funktion zur Anzeige der Anzahl der Fälle in jedem Dataset
library(dplyr)
count_cases <- function(data, name) {
  num_cases <- nrow(data)
  dataset_info <- data.frame(Dataset = name, NumCases = num_cases)
  return(dataset_info)
}

# Anzahl der Fälle in jedem Dataset anzeigen und nach Größe ordnen
case_counts <- bind_rows(
  count_cases(TechData, "TechData"),
  count_cases(FoodData, "FoodData"),
  count_cases(ConsultingData, "ConsultingData"),
  count_cases(FinanceData, "FinanceData")
) %>%
  arrange(desc(NumCases))

# Ausgabe der Anzahl der Fälle in jedem Dataset, geordnet nach Größe
print(case_counts)

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

# Geocode locations and extract country information
register_google(key = "AIzaSyAU9mANCBK3lyFkyWmYoWA4Uaw5MhBJY9k")
CountriesDataGlassdoor <- CleanDataGlassdoor %>%
  mutate(geo_info = geocode(paste(location, sep = ", ")),
         country = sapply(geo_info, function(x) x$country))


