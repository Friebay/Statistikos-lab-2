# Renovuotų ir nerenovuotų butų kainų porinis t-testas

# Atliksime porinį t-testą naudodami jau paruoštus duomenis iš combined_data_clean
# Porinis t-testas tinka, nes turime poras: renovuotas butas ir nerenovuotas butas su tokiais pačiais statybos metais

# Tikriname ar duomenys tenkina normalumo prielaidą
par(mfrow=c(1,2))
hist(combined_data_clean$Kaina_Renovuoto, main="Renovuotų butų kainų pasiskirstymas", 
     xlab="Kaina (EUR)", col="lightblue")
hist(combined_data_clean$Kaina_Nerenovuoto, main="Nerenovuotų butų kainų pasiskirstymas", 
     xlab="Kaina (EUR)", col="lightgreen")

# QQ grafikai
par(mfrow=c(1,2))
qqnorm(combined_data_clean$Kaina_Renovuoto, main="Renovuotų butų QQ grafikas")
qqline(combined_data_clean$Kaina_Renovuoto)
qqnorm(combined_data_clean$Kaina_Nerenovuoto, main="Nerenovuotų butų QQ grafikas")
qqline(combined_data_clean$Kaina_Nerenovuoto)

# Patikrinkime ar reikia logaritminės transformacijos
# Jei duomenys stipriai nukrypsta nuo normalumo, galima taikyti logaritminę transformaciją
log_kaina_renovuoto <- log(combined_data_clean$Kaina_Renovuoto)
log_kaina_nerenovuoto <- log(combined_data_clean$Kaina_Nerenovuoto)

par(mfrow=c(1,2))
hist(log_kaina_renovuoto, main="Log-transformuotos renovuotų butų kainos", 
     xlab="ln(Kaina)", col="lightblue")
hist(log_kaina_nerenovuoto, main="Log-transformuotos nerenovuotų butų kainos", 
     xlab="ln(Kaina)", col="lightgreen")

par(mfrow=c(1,2))
qqnorm(log_kaina_renovuoto, main="Log-renovuotų butų QQ grafikas")
qqline(log_kaina_renovuoto)
qqnorm(log_kaina_nerenovuoto, main="Log-nerenovuotų butų QQ grafikas")
qqline(log_kaina_nerenovuoto)

# Apskaičiuojame statisines charakteristikas
cat("\nStatistinės charakteristikos (originalūs duomenys):\n")
cat("Renovuotų butų vidutinė kaina:", mean(combined_data_clean$Kaina_Renovuoto), "EUR\n")
cat("Nerenovuotų butų vidutinė kaina:", mean(combined_data_clean$Kaina_Nerenovuoto), "EUR\n")
cat("Vidutinis kainų skirtumas:", mean(combined_data_clean$Kaina_Renovuoto - combined_data_clean$Kaina_Nerenovuoto), "EUR\n")
cat("Renovuotų butų kainų standartinis nuokrypis:", sd(combined_data_clean$Kaina_Renovuoto), "EUR\n")
cat("Nerenovuotų butų kainų standartinis nuokrypis:", sd(combined_data_clean$Kaina_Nerenovuoto), "EUR\n\n")

# Atliekame porinį t-testą originaliems duomenims
t_test_result <- t.test(
  combined_data_clean$Kaina_Renovuoto, 
  combined_data_clean$Kaina_Nerenovuoto, 
  alternative = "greater",  # Tikriname ar renovuotų butų kainos didesnės
  paired = TRUE             # Porinis testas, nes lyginama tos pačios statybos metų butai
)

print(t_test_result)

# Nustatomos kritinės t-reikšmės ir išvados
alpha <- 0.05  # reikšmingumo lygis
df <- nrow(combined_data_clean) - 1  # laisvės laipsnių skaičius
critical_t <- qt(1 - alpha, df)

cat("\nKritinė t-reikšmė:", critical_t, "\n")
cat("Apskaičiuota t-reikšmė:", t_test_result$statistic, "\n")

if (t_test_result$p.value < alpha) {
  cat("\nIšvada: Atmetame nulinę hipotezę. Renovuotų butų nuomos kainos vidurkis yra statistiškai reikšmingai didesnis nei nerenovuotų butų nuomos kainos vidurkis (p-reikšmė =", t_test_result$p.value, ").\n")
} else {
  cat("\nIšvada: Neatmetame nulinės hipotezės. Negalime teigti, kad renovuotų butų nuomos kainos vidurkis yra statistiškai reikšmingai didesnis nei nerenovuotų butų nuomos kainos vidurkis (p-reikšmė =", t_test_result$p.value, ").\n")
}

# Efekto dydžio apskaičiavimas (Cohen's d)
mean_diff <- mean(combined_data_clean$Kaina_Renovuoto - combined_data_clean$Kaina_Nerenovuoto)
pooled_sd <- sd(combined_data_clean$Kaina_Renovuoto - combined_data_clean$Kaina_Nerenovuoto)
cohens_d <- mean_diff / pooled_sd

cat("\nEfekto dydis (Cohen's d):", cohens_d, "\n")
cat("Efekto interpretacija: ")
if (abs(cohens_d) < 0.2) {
  cat("mažas efektas\n")
} else if (abs(cohens_d) < 0.8) {
  cat("vidutinis efektas\n")
} else {
  cat("didelis efektas\n")
}

# Konfidencinis intervalas
conf_interval <- t_test_result$conf.int
cat("\n", 100*(1-alpha), "% pasikliovimo intervalas: [", conf_interval[1], ",", conf_interval[2], "]\n", sep="")
