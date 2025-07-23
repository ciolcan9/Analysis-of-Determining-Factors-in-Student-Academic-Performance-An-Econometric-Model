library(tidyverse)
library(lmtest)
library(car)
library(tseries)


nume_fisier <- "Student_performance_data _.csv"
tryCatch({
  date_studenti <- read.csv(nume_fisier)
}, error = function(e) {
  stop(paste("EROARE: Fișierul", nume_fisier, "nu a fost găsit."))
})

date_studenti <- date_studenti %>%
  mutate(
    Gender = factor(Gender, levels = c(0, 1), labels = c("Masculin", "Feminin")),
    Ethnicity = factor(Ethnicity, levels = c(0, 1, 2, 3), labels = c("Caucasian", "African American", "Asian", "Other")),
    ParentalEducation = factor(ParentalEducation, levels = c(0, 1, 2, 3, 4), labels = c("None", "High School", "Some College", "Bachelor's", "Higher")),
    ParentalSupport = factor(ParentalSupport, levels = c(0, 1, 2, 3, 4), labels = c("None", "Low", "Moderate", "High", "Very High")),
    Tutoring = factor(Tutoring, levels = c(0, 1), labels = c("Nu", "Da")),
    Extracurricular = factor(Extracurricular, levels = c(0, 1), labels = c("Nu", "Da")),
    Sports = factor(Sports, levels = c(0, 1), labels = c("Nu", "Da")),
    Music = factor(Music, levels = c(0, 1), labels = c("Nu", "Da")),
    Volunteering = factor(Volunteering, levels = c(0, 1), labels = c("Nu", "Da"))
  )


model <- lm(GPA ~ Age + Gender + Ethnicity + ParentalEducation + StudyTimeWeekly +
              Absences + Tutoring + ParentalSupport + Extracurricular + Sports +
              Music + Volunteering,
            data = date_studenti)

summary_model <- summary(model)
print(summary_model)


cat(paste("\nR-squared:", round(summary_model$r.squared, 4)))
cat(paste("\nAdjusted R-squared:", round(summary_model$adj.r.squared, 4), "\n"))

print("--- VIF ---")
print(vif(model))

print("--- Breusch-Pagan Test ---")
bptest(model)

print("--- Jarque-Bera Test ---")
jarque.bera.test(residuals(model))
hist(residuals(model), main = "Histograma Reziduurilor", col = "lightblue")

print("--- Durbin-Watson Test ---")
dwtest(model)

student_nou <- data.frame(
  Age = 17,
  Gender = factor("Feminin", levels = c("Masculin", "Feminin")),
  Ethnicity = factor("Caucasian", levels = c("Caucasian", "African American", "Asian", "Other")),
  ParentalEducation = factor("Bachelor's", levels = c("None", "High School", "Some College", "Bachelor's", "Higher")),
  StudyTimeWeekly = 15.5,
  Absences = 3,
  Tutoring = factor("Da", levels = c("Nu", "Da")),
  ParentalSupport = factor("High", levels = c("None", "Low", "Moderate", "High", "Very High")),
  Extracurricular = factor("Da", levels = c("Nu", "Da")),
  Sports = factor("Nu", levels = c("Nu", "Da")),
  Music = factor("Da", levels = c("Nu", "Da")),
  Volunteering = factor("Nu", levels = c("Nu", "Da"))
)

gpa_prognozat <- predict(model, newdata = student_nou)
cat("\n--- PROGNOZĂ ---\n")
cat(paste("GPA prognozat:", round(gpa_prognozat, 2), "\n"))

