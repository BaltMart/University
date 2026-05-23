install.packages("readxl")
library(readxl)
library(dplyr)

data_variable_names <- read_excel("C:/Users/martynas/Desktop/uni/Year 2/DATA/Task 2/millions.XLS", sheet = "Variable code")
data_sheet <- read_excel("C:/Users/martynas/Desktop/uni/Year 2/DATA/Task 2/millions.XLS", sheet = "BARROSHO", na = c("#N/A",".")) #To make it easier to deal with na values later on, I tell the function to replace #N/A with just na

head(data_sheet) #We take a look at the data set we read
head(data_variable_names) #Also we see how the variable names are constructed

#Since it is confusing to work with such column names as "X1" and so on, here I make sure the data set jis 
varNames <- data_variable_names[[2]]
colnames(data_sheet)[5:ncol(data_sheet)] <- varNames

#Defining variables of interest
varOfInt = c("gamma", "GDPSH60", "LIFEE060", "P60")

data_clean <- data_sheet %>%
  filter(!is.na(gamma), !is.na(GDPSH60), !is.na(LIFEE060), !is.na(P60))

#After cleaning the data, we can look at summary statistics for our variables of interest
sum_Var_int = summary(data_clean[,varOfInt, drop = FALSE])
print(sum_Var_int)

#Running the fixed regression
reg0 = lm(gamma ~ GDPSH60 + LIFEE060 + P60, data_clean)
summary(reg0)
plot(residuals(reg0))

#Regression 1: main variables + 
#Rule of Law (RULELAw), Equipment Investment (EQINV), Number of years Open Economy (YrsOpen), Fraction of confucius (CONFUC)
reg1 = lm(gamma ~ GDPSH60 + LIFEE060 + P60 + RULELAW + EQINV + YrsOpen + CONFUC, data_clean)
summary(reg1)
plot(residuals(reg1))

#Regression 2: main variables +
#Rule of Law (RULELAW), Number of years Open Economy (YrsOpen), Fraction of confucius (CONFUC), Fraction of muslim (MUSLIM)
reg2 = lm(gamma ~ GDPSH60 + LIFEE060 + P60 + RULELAW + YrsOpen + CONFUC + MUSLIM, data_clean)
summary(reg2)
plot(residuals(reg2))

#Regression 3: main variables +
#Rule of Law (RULELAW), Fraction of confucius (CONFUC), Fraction of muslim (MUSLIM), Political rights (prightsb)
reg3 = lm(gamma ~ GDPSH60 + LIFEE060 + P60 + RULELAW  + CONFUC + MUSLIM + prightsb, data_clean)
summary(reg3)
plot(residuals(reg3))

#Regression 4: main variables +
#Rule of Law (RULELAW), Fraction of muslim (MUSLIM), Political rights (prightsb), Latin America (laam)
reg4 = lm(gamma ~ GDPSH60 + LIFEE060 + P60 + RULELAW + MUSLIM + prightsb + laam, data_clean)
summary(reg4)
plot(residuals(reg4))

#Regression 5: main variables +
#Fraction of muslim (MUSLIM), Political rights (prightsb), Latin America (laam), Sub-Saharan Africa (safrica)
reg5 = lm(gamma ~ GDPSH60 + LIFEE060 + P60 + MUSLIM + prightsb + laam + safrica, data_clean)
summary(reg5)
plot(residuals(reg5))

#Regression 6: main variables +
#Political rights (prightsb), Latin America (laam), Sub-Saharan Africa (safrica), Civil liberties(civlibb)
reg6 = lm(gamma ~ GDPSH60 + LIFEE060 + P60 + prightsb + laam + safrica + civlibb, data_clean)
summary(reg6)
plot(residuals(reg6))

#Regression 7: main variables +
#Sub-Saharan Africa (safrica), Civil liberties(civlibb), Revolutions and coups (revcoup), Fraction of GDP in mining (Mining)
reg7 = lm(gamma ~ GDPSH60 + LIFEE060 + P60 + safrica + civlibb + revcoup + Mining, data_clean)
summary(reg7)
plot(residuals(reg7))

#Regression 8: main variables +
#Civil liberties(civlibb), Revolutions and coups (revcoup), Fraction of GDP in mining (Mining), SD black-market premium (BMS6087)
reg8 = lm(gamma ~ GDPSH60 + LIFEE060 + P60 + civlibb + revcoup + Mining + BMS6087, data_clean)
summary(reg8)
plot(residuals(reg8))

#Regression 9: main variables +
#Revolutions and coups (revcoup), Fraction of GDP in mining (Mining), SD black-market premium (BMS6087), Primary exports in 1970 (PRIEXP70)
reg9 = lm(gamma ~ GDPSH60 + LIFEE060 + P60 + revcoup + Mining + BMS6087 + PRIEXP70, data_clean)
summary(reg9)
plot(residuals(reg9))

#Regression 10: main variables +
#Fraction of GDP in mining (Mining), SD black-market premium (BMS6087), Primary exports in 1970 (PRIEXP70), degree of capitalism (EcOrg)
reg10 = lm(gamma ~ GDPSH60 + LIFEE060 + P60 + Mining + BMS6087 + PRIEXP70 + EcOrg, data_clean)
summary(reg10)
plot(residuals(reg10))

#Regression 11: main variables +
#SD black-market premium (BMS6087), Primary exports in 1970 (PRIEXP70), degree of capitalism (EcOrg), War dummy (wardum)
reg11 = lm(gamma ~ GDPSH60 + LIFEE060 + P60 + BMS6087 + PRIEXP70 + EcOrg + wardum, data_clean)
summary(reg11)
plot(residuals(reg11))

#Regression 12: main variables +
#Primary exports in 1970 (PRIEXP70), degree of capitalism (EcOrg), War dummy (wardum), Non-equipment investment (NONEQINV)
reg12 = lm(gamma ~ GDPSH60 + LIFEE060 + P60 + PRIEXP70 + EcOrg + wardum + NONEQINV, data_clean)
summary(reg12)
plot(residuals(reg12))

#Regression 13: main variables +
#degree of capitalism (EcOrg), War dummy (wardum), Non-equipment investment (NONEQINV), Absolute latitude (ABSLATIT)
reg13 = lm(gamma ~ GDPSH60 + LIFEE060 + P60 + EcOrg + wardum + NONEQINV + ABSLATIT, data_clean)
summary(reg13)
plot(residuals(reg13))

#Regression 14: main variables +
#War dummy (wardum), Non-equipment investment (NONEQINV), Absolute latitude (ABSLATIT), Exchange-rate distortions (RERD)
reg14 = lm(gamma ~ GDPSH60 + LIFEE060 + P60 + wardum + NONEQINV + ABSLATIT + RERD, data_clean)
summary(reg14)
plot(residuals(reg14))

#Regression 15: main variables +
#Non-equipment investment (NONEQINV), Absolute latitude (ABSLATIT), Exchange-rate distortions (RERD), Fraction protestant (PROT)
reg15 = lm(gamma ~ GDPSH60 + LIFEE060 + P60 + NONEQINV + ABSLATIT + RERD + PROT, data_clean)
summary(reg15)
plot(residuals(reg15))

#Regression 16: main variables +
#Absolute latitude (ABSLATIT), Exchange-rate distortions (RERD), Fraction protestant (PROT), Fraction Buddhist (BUDDHA)
reg16 = lm(gamma ~ GDPSH60 + LIFEE060 + P60 + ABSLATIT + RERD + PROT + BUDDHA, data_clean)
summary(reg16)
plot(residuals(reg16))

#Regression 17: main variables +
#Exchange-rate distortions (RERD), Fraction protestant (PROT), Fraction Buddhist (BUDDHA), Fraction Catholic (CATH) 
reg17 = lm(gamma ~ GDPSH60 + LIFEE060 + P60 + RERD + PROT + BUDDHA + CATH, data_clean)
summary(reg17)
plot(residuals(reg17))

#Regression 18: main variables +
#Fraction protestant (PROT), Fraction Buddhist (BUDDHA), Fraction Catholic (CATH), Spanish colony (SPAIN)
reg18 = lm(gamma ~ GDPSH60 + LIFEE060 + P60 + PROT + BUDDHA + CATH + SPAIN, data_clean)
summary(reg18)
plot(residuals(reg18))

#Regression 19: main variables +
#Fraction Buddhist (BUDDHA), Fraction Catholic (CATH), Spanish colony (SPAIN), Equipment Investment (EQINV)
reg19 = lm(gamma ~ GDPSH60 + LIFEE060 + P60 + BUDDHA + CATH + SPAIN + EQINV, data_clean)
summary(reg19)
plot(residuals(reg19))

#Regression 20: main variables +
#Fraction Catholic (CATH), Spanish colony (SPAIN), Equipment Investment (EQINV), Number of years of Open Economy (YrsOpen)
reg20 = lm(gamma ~ GDPSH60 + LIFEE060 + P60 + CATH + SPAIN + EQINV + YrsOpen, data_clean)
summary(reg20)
plot(residuals(reg20))

#Regression 21: main variables +
#Spanish colony (SPAIN), Equipment Investment (EQINV), Number of years of Open Economy (YrsOpen), Fraction Confucian (CONFUC)
reg21 = lm(gamma ~ GDPSH60 + LIFEE060 + P60 + SPAIN + EQINV + YrsOpen + CONFUC, data_clean)
summary(reg21)
plot(residuals(reg21))

#To collect data in a more manageble way for it to be interpreted I will use texreg package and will create multiple tables with a few regressions each
install.packages("texreg")
library(texreg)
screenreg(list(reg0, reg1, reg2, reg3, reg4),
          custom.model.names = c("Reg0", "Reg1", "Reg2", "Reg3", "Reg4"),
          digits = 4,
          single.row = TRUE, 
          table.layout ="a")

screenreg(list(reg5, reg6, reg7, reg8, reg9),
          custom.model.names = c("Reg5", "Reg6", "Reg7", "Reg8", "Reg9"),
          digits = 4,
          single.row = TRUE,
          table.layout = "a")

screenreg(list(reg10, reg11, reg12, reg13, reg14),
          custom.model.names = c("Reg10", "Reg11", "Reg12", "Reg13", "Reg14"),
          digits = 4,
          single.row = TRUE,
          table.layout = "a")

screenreg(list(reg15, reg16, reg17, reg18, reg19),
          custom.model.names = c("Reg15", "Reg16", "Reg17", "Reg18", "Reg19"),
          digits = 4,
          single.row = TRUE,
          table.layout = "a")

screenreg(list(reg20, reg21),
          custom.model.names = c("Reg20", "Reg21"),
          digits = 4,
          single.row = TRUE,
          table.layout = "a")

#Next, after displaying each results of the regressions, I need to produce the averages of each regressor to see whether my work replicates the paper
regression_models <- list(reg0, reg1, reg2, reg3, reg4, reg5, reg6, reg7, reg8, reg9, reg10, reg11, reg12, reg13,
                          reg14, reg15, reg16, reg17, reg18, reg19, reg20, reg21)

#Store regression summaries in a list
regression_summaries <- lapply(regression_models, summary)

#Extract coefficients and standard deviations from each regression summary
coefficients_list <- lapply(regression_summaries, function(summary_obj) {
  coef_table <- summary_obj$coefficients
  data.frame(
    Variable = rownames(coef_table),
    Beta = coef_table[, "Estimate"],
    SD = coef_table[, "Std. Error"]
  )
})

#Combine all coefficients into a single data frame
all_coefficients <- do.call(rbind, coefficients_list)

#Calculate the average of the coefficients for each variable
average_coefficients <- aggregate(Beta ~ Variable, data = all_coefficients, FUN = mean)
sd_coefficients <- aggregate(Beta ~ Variable, data = all_coefficients, FUN = sd)

#Rounding of the values
average_coefficients$Beta <- round(average_coefficients$Beta, 4)
sd_coefficients$Beta <- round(sd_coefficients$Beta, 4)
options(scipen = 999) #Used to ensure that number are not produced as scientific

#Here I will produce a final table (reproduction of original paper table 1)
column_names <- c("Variable")
results_df <- data.frame(matrix(ncol = length(column_names), nrow = 22))
colnames(results_df)[1:ncol(results_df)] <- column_names
varNamesRes <- c( "EQINV", "YrsOpen", "CONFUC","RULELAW", "MUSLIM",
                  "prightsb", "laam", "safrica", "civlibb", "revcoup", "Mining", "BMS6087",
                  "PRIEXP70", "EcOrg", "wardum", "NONEQINV", "ABSLATIT", "RERD", "PROT", "BUDDHA",
                  "CATH", "SPAIN") #Listed in the same order as it appears in the paper
results_df$Variable <- varNamesRes

results_df <- merge(results_df, average_coefficients, by = "Variable", sort = FALSE)
results_df <- merge(results_df, sd_coefficients, by = "Variable", sort = FALSE)
colnames(results_df)[3] <- "SD"
