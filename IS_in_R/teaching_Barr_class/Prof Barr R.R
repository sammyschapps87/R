
"packages"
install.packages("stringr")
install.packages("plyr")
install.packages("tidyverse")
install.packages("data.table")
install.packages("dummies")
install.packages("ggstatsplot")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("purrr")
install.packages("seasonal")
install.packages("fpp2")
install.packages("writexl")

library(purrr)
library(dplyr)
library(ggplot2)
library(ggstatsplot)
library(dummies)
library(data.table)
library(tidyverse)
library(plyr)
library(stringr)
library(seasonal)
library(fpp2)
library(writexl)
"----------------------------------------------------------------------------------------------------------------------------------------------"
"For BBL"
Working_NYC_data_set_2_10_21$Block <- sprintf("%05d", Working_NYC_data_set_2_10_21$Block)
Working_NYC_data_set_2_10_21$Lot <- sprintf("%04d", Working_NYC_data_set_2_10_21$Lot)
summary(Complete_merged_data$Land_sqft)
Working_NYC_data_set_2_10_21$BBL <- paste(Working_NYC_data_set_2_10_21$Borough, Working_NYC_data_set_2_10_21$Block, Working_NYC_data_set_2_10_21$Lot, sep = "")
"-------------------------------------------------------------------------------------------------------------------------------------------------"
"To merge data"
Merged_data <- rbind(Working_NYC_data_set_2_10_21, Sales_Data_Master_renamed_varibles)
Complete_merged_data <- merge(Pluto_data_file, Merged_data, by="BBL")
"-------------------------------------------------------------------------------------------------------------------------------------------------"
"remaning varibles after merge"
ln_sale_price <- Complete_merged_data$ln_sale_price
ln_gross_sqft <- ln_gross_sqft1$ln_gross_sqft
ln_land_sqft <- Complete_merged_data$ln_land_sqft
Year_built <- Complete_merged_data$yearbuilt
BBL <- Complete_merged_data$BBL
Building_class_category <- Complete_merged_data$Building_class_category 
Total_units <- Complete_merged_data$Total_units
Zip_code <- Complete_merged_data$zipcode
Sale_date <- Complete_merged_data$Sale_date
Community_District <- Complete_merged_data$cd
Complete_merged_data$ln_sale_price <- log(Complete_merged_data$Sale_price)
Complete_merged_data$ln_land_sqft <- log(Complete_merged_data$Land_sqft)
Complete_merged_data$ln_gross_sqft <- log(Complete_merged_data$Gross_sqft)
Complete_merged_data$ln_lot_area <- log(Complete_merged_data$lotarea)
"--------------------------------------------------------------------------------------------------------------------------------------------"
"EASY COMANDS FOR CHECKING VARIBLE VALUES"
summary()
edit()
any(is.na())
any(is.infinite())
any(is.nan())
any(is.numeric())
any(is.factor())
count()
any()
length()
levels()
max(Complete_merged_data$Sale_date)
"-------------------------------------------------------------------------------------------------------------------------------------------"
"Quick Plots"
plot() "scatter plot" 
boxplot() "box plot" 
barplot() "line plot"
pie() "pie chart"
hist() "histogram plot"
density() "density plot"
dotchart() "dot chart"

"HARDER PLOTS"

"line plot" 
plot(x, y, type = "1", lty = 1) 
lines(x, y, type = "1", lty = 1)
plot(Sale_Date_coef_no_outliers_exp_100$`Sale Date`, Sale_Date_coef_no_outliers_exp_100$`CD 101`, type = "1", lty = 1)
"quantile quantile plot"
qqnorm(varible, pch = 1, frame = FALSE)
qqline(Varible, col = "steelblue", lwd = 2)
"--------------------------------------------------------------------------------------------------------------------------------------------"
"Replacing Inf values"
  "For ln_gross_sqft"
is.na(Complete_merged_data$ln_gross_sqft) <- sapply(Complete_merged_data$ln_gross_sqft, is.infinite)
any(is.na(Complete_merged_data$ln_gross_sqft))

is.na(Complete_merged_data$Gross_sqft) <- sapply(Complete_merged_data$Gross_sqft, is.infinite)
  "For ln_lot_area"
  
is.na(Complete_merged_data$ln_lot_area) <- sapply(Complete_merged_data$ln_lot_area, is.infinite)

count(any(is.na(Complete_merged_data$ln_gross_sqft)))
any(is.infinite(Complete_merged_data$ln_gross_sqft))
any(is.infinite(Complete_merged_data$ln_lot_area))
"--------------------------------------------------------------------------------------------------------------------------------------------"
"Zip Code dummy"
Complete_merged_data$Zip_code_dummies <- dummy(Complete_merged_data$Zip_code, sep = ".")

levels(Complete_merged_data$Zip_code)
edit(Complete_merged_data$Zip_code_dummies)
Zipcode_matrix <-  data.frame(Complete_merged_data$Zip_code_dummies)

lm(Complete_merged_data$ln_sale_price ~ Complete_merged_data$Zip_code + Complete_merged_data$Sale_date + Complete_merged_data$Year_build)
"---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------"
"Sale Date dummy"
Complete_merged_data$Sale_date <- substr(Complete_merged_data$Sale_date, 0, 4)
Complete_merged_data$Sale_date_dummies <- dummy(Complete_merged_data$Sale_date, sep = ".")
Saledate_matrix <- data.frame(Complete_merged_data$Sale_date_dummies)
"---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------"
"Fixing 0 values in Year Build"
Complete_merged_data$Year_build[Complete_merged_data$Year_build == 0] <- NA
summary(Complete_merged_data$Year_build)
class(Complete_merged_data$Year_build)
"---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------"
"Year Build dummy"
Complete_merged_data$Year_build_dummies <- dummy(Complete_merged_data$Year_build, sep = ".")
Year_build_matrix <- data.frame(Complete_merged_data$Year_build_dummies)
"-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------"
"Simple and Main regressions"
simple_reg <- lm(Complete_merged_data$ln_sale_price ~ Complete_merged_data$Sale_date_dummies)
summary(simple_reg)

main_reg <- lm(Complete_merged_data$ln_sale_price ~ Complete_merged_data$ln_land_sqft + Complete_merged_data$ln_gross_sqft + Complete_merged_data$ln_lot_area + Complete_merged_data$Sale_date_dummies + Complete_merged_data$Zip_code_dummies + Complete_merged_data$Year_build_dummies)
options(max.print = 1000000)
options(scipen=999)
summary(main_reg)
"-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------"
"Creating regression loop for every indivuisal community district"
out=with(Complete_merged_data,
         by(Complete_merged_data, Complete_merged_data$cd, function(Complete_merged_data) lm(Complete_merged_data$ln_sale_price ~ Complete_merged_data$Sale_date_dummies + Complete_merged_data$ln_land_sqft + Complete_merged_data$ln_gross_sqft + Complete_merged_data$ln_lot_area + Complete_merged_data$Year_build_dummies + Complete_merged_data$Zip_code_dummies, data = Complete_merged_data)))
outsum <- lapply(out, summary)
coefmat <- sapply(outsum, coef)
out[[1]]
memory.limit(999999999)
outsum[[1]]
"---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------"
"Important once regression works"
Community_district_coef <- coefmat[seq(1, length(coefmat),4)]
Community_district_coef <- data.frame(out)
out <- out[-c(25, 26, 45, 60, 61, 65)]
rm(Community_district_coef)
Community_district_coef[[1]]
"-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------"
"Convert to CSV"
write.csv(Community_District_Coef_NYC,"C:\\Windows (C:)\\Users\\Samuel M Schappel\\OneDrive\\Documents\\Downloads\\Community_district_coef_NYC.csv", row.names = FALSE)
edit(Community.District.Coef.NYC)
rm(Community.District.Coef.NYC)
str(outsum[["595"]])
"------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------"
"Creating Price per sqft and ln_price_per_sqft"
Complete_merged_data$price_per_sqft <- (Complete_merged_data$Sale_price / Complete_merged_data$Gross_sqft) 
Complete_merged_data$ln_ppsf <- log(Complete_merged_data$price_per_sqft)
"-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------"
"NA to Inf for ln_pp"
is.na(Complete_merged_data$ln_ppsf) <- sapply(Complete_merged_data$ln_ppsf, is.infinite)
any(is.infinite(Complete_merged_data$ln_ppsf))
"---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------"
"getting rid of outliers"
no_outliers_complete_merged_data <- subset(Complete_merged_data, Complete_merged_data$ln_ppsf > 3 & Complete_merged_data$ln_ppsf < 7.5)
main_reg_no_outlier <- lm(no_outliers_complete_merged_data$ln_sale_price ~ no_outliers_complete_merged_data$Sale_date_dummies + no_outliers_complete_merged_data$ln_land_sqft + no_outliers_complete_merged_data$ln_gross_sqft + no_outliers_complete_merged_data$ln_lot_area + no_outliers_complete_merged_data$Zip_code_dummies + no_outliers_complete_merged_data$Year_build_dummies)
summary(main_reg_no_outlier)
"-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------"
"NEW CD regression loop without outliers"
out2=with(no_outliers_complete_merged_data,
         by(no_outliers_complete_merged_data, no_outliers_complete_merged_data$cd, function(no_outliers_complete_merged_data) lm(no_outliers_complete_merged_data$ln_sale_price ~ no_outliers_complete_merged_data$Sale_date_dummies + no_outliers_complete_merged_data$ln_land_sqft + no_outliers_complete_merged_data$ln_gross_sqft + no_outliers_complete_merged_data$ln_lot_area + no_outliers_complete_merged_data$Year_build_dummies + no_outliers_complete_merged_data$Zip_code_dummies, data = no_outliers_complete_merged_data)))
out2[[1]]
out2 <- out2[-c(25, 26, 45, 60, 61, 65)]
"-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------"
"take exponante and mutiply by 100 then / by base year 2003"
Sale_Date_coef_no_outliers_exp_100  <- Sale_Date_coef_no_outliers_exp*100
Sale_Date_coef_no_outliers_exp <- exp(Sale_Date_coef_no_outliers)
"Creating complete merged data sale date cofficient matrix"
Sale_date <- c(2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020)
Estimate <- c(-0.722362, -0.530726, -0.317237, -0.192584, -0.219587, -0.368410, -0.476928 , -0.477032, -0.445778, -0.412179, -0.365715, -0.268278, -0.141868, -0.099176, -0.003202, 0.081246, -0.078817, 0)
CMD_sale_date_vs_Estimate <- data.frame(Sale_date, Estimate)
CMD_sale_date_vs_Estimate$Estimate <- exp(CMD_sale_date_vs_Estimate$Estimate)
CMD_sale_date_vs_Estimate$Estimate <- 100*(CMD_sale_date_vs_Estimate$Estimate)
CMD_sale_date_vs_Estimate$Estimate <- CMD_sale_date_vs_Estimate$Estimate/48.56039
"-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------"
"No outlier sale date coef matrix"
Sale_date <- c(2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020)
Estimate_no_outlier <- c(-0.93159, -0.77434, -0.59598, -0.48177, -0.47419, -0.54853, -0.66635, -0.68036, -0.69627, -0.66702, -0.60512, -0.51939, -0.40785, -0.35967, -0.24868, -0.17472, 0.06964, 0)
CMD_no_outlier <- data.frame(Sale_date, Estimate_no_outlier)
CMD_no_outlier$Estimate_no_outlier <- exp(CMD_no_outlier$Estimate_no_outlier)
CMD_no_outlier$Estimate_no_outlier <- (100*CMD_no_outlier$Estimate_no_outlier)
CMD_no_outlier$Estimate_no_outlier <- CMD_no_outlier$Estimate_no_outlier/ 39.392669

ggplot(CMD_no_outlier, aes(x= CMD_no_outlier$Sale_date, y= CMD_no_outlier$Estimate_no_outlier)) + 
  geom_line()+
  geom_smooth()
  


hist(no_outliers_complete_merged_data$ln_sale_price)


summary(Sale)
outsum2 <- lapply(out2, summary)
outsum2[[1]]
coefmat2 <- sapply(outsum2, coef)
out2[[59]]
options(scipen=999)
format(out2, scientific = FALSE)
length(out2)
outsum2[[1]]
levels(length(out2$))
summary(no_outliers_complete_merged_data$ln_ppsf)
length(Complete_merged_data$ln_ppsf)
if (Complete_merged_data$ln_ppsf < 3){
  NA
} else {
  

rm(Sale_Date_coef_no_outliers_exp_100_base_year_2003)
summary(Complete_merged_data$ln_sale_price)
any(is.na(Complete_merged_data$ln_ppsf))
Sale_Date_coef_no_outliers_exp_100  <- Sale_Date_coef_no_outliers_exp*100
Sale_Date_coef_no_outliers_exp <- exp(Sale_Date_coef_no_outliers)
Sale_Date_coef_no_outliers$
#Log Transform
log_prc_pr_sqft <- log(price_per_sqft)
class(log_prc_pr_sqft)
log_prc_pr_sqft <- as.numeric(log_prc_pr_sqft)
log_prc_pr_sqft<- matrix(log_prc_pr_sqft, )
count(log_prc_pr_sqft)
summary(log_p)
#price_per_sqft <- list(price_per_sqft)
price_per_sqft<- c("price per sqft")
hist(log_prc_pr_sqft)

rm(log_prc_pr_sqft)
class(Complete_merged_data$Sale_price)
class(Complete_merged_data$Gross_sqft)

Complete_merged_data$ln_ppsf <- log(Complete_merged_data$price_per_sqft)
rm(ln_ppsf)
ifelse(ln_ppsf, lm(Complete_merged_data$ln_sale_price ~ Complete_merged_data$ln_land_sqft, ''))
summary(Complete_merged_data$ln_ppsf)
hist(Complete_merged_data$ln_sale_price)
hist(ln_ppsf)
outsum[[1]]
str(coefmat)
coefmat["101"]
coefmat["102"]






"----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------"
"to clean garbage"
gc()
"-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------"
"Important once regression works"
Community_district_coef <- coefmat[seq(1, length(coefmat),4)]
Community_district_coef <- data.frame(out)
out <- out[-c(25, 26, 45, 60, 61, 65)]
rm(Community_district_coef)
Community_district_coef[[1]]
"-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------"
"Convert to CSV"
write.csv(Community_District_Coef_NYC,"C:\\Windows (C:)\\Users\\Samuel M Schappel\\OneDrive\\Documents\\Downloads\\Community_district_coef_NYC.csv", row.names = FALSE)
edit(Community.District.Coef.NYC)
rm(Community.District.Coef.NYC)
str(outsum[["595"]])
"--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------"
"getting rid of outliers"
quantile((ln_ppsf), .999)

summary(ln_ppsf)
no_outliers_complete_merged_data <- subset(Complete_merged_data, ln_ppsf > quantile((ln_ppsf), .05) & ln_ppsf < quantile((ln_ppsf), .999))

length(no_outliers_complete_merged_data$

length(out)

rm(Community.District.Coef.NYC)

read.csv(Community.District.Coef.NYC,)
write.csv(Sale_Date_coef_no_outliers_exp_100, "Sale_Date_coef_no_outliers_exp_100.csv")
out[10]
write_xlsx(Sale_Date_coef_no_outliers_exp_100,"C:\\Users\\Sam\\Desktop\\Saledate.xlsx" )
Community.District.Coef.NYC$CD101 <- Community.District.Coef.NYC$V1
rm(Community.District.Coef.NYC$CD101)
edit(Community.District.Coef.NYC)
summary(str(coefmat))
"EXTRACT COEF"
Sale_date_dummies <- Complete_merged_data$Sale_date_dummies

Sale_Date_coef_no_outliers_exp_100$`CD 101` <- Sale_Date_coef_no_outliers_exp_100$`CD 101`/27.18486
Sale_Date_coef_no_outliers_exp_100$CD102 <- Sale_Date_coef_no_outliers_exp_100$CD102 / 32.57712
Sale_Date_coef_no_outliers_exp_100$CD103 <- Sale_Date_coef_no_outliers_exp_100$CD103 / 16.32044
Sale_Date_coef_no_outliers_exp_100$CD104 <- Sale_Date_coef_no_outliers_exp_100$CD104 / 41.64749
Sale_Date_coef_no_outliers_exp_100$CD105 <- Sale_Date_coef_no_outliers_exp_100$CD105 / 80.25541
Sale_Date_coef_no_outliers_exp_100$CD106 <- Sale_Date_coef_no_outliers_exp_100$CD106 / 63.90261
Sale_Date_coef_no_outliers_exp_100$CD107 <- Sale_Date_coef_no_outliers_exp_100$CD107 / 46.15569
Sale_Date_coef_no_outliers_exp_100$CD108 <- Sale_Date_coef_no_outliers_exp_100$CD108 / 76.92417
Sale_Date_coef_no_outliers_exp_100$CD109 <- Sale_Date_coef_no_outliers_exp_100$CD109 / 39.11751
Sale_Date_coef_no_outliers_exp_100$CD110 <- Sale_Date_coef_no_outliers_exp_100$CD110 / 34.83314
Sale_Date_coef_no_outliers_exp_100$CD111 <- Sale_Date_coef_no_outliers_exp_100$CD111 / 30.22282
Sale_Date_coef_no_outliers_exp_100$CD112 <- Sale_Date_coef_no_outliers_exp_100$CD112 / 23.46430
Sale_Date_coef_no_outliers_exp_100$CD201 <- Sale_Date_coef_no_outliers_exp_100$CD201 / 25.67455
Sale_Date_coef_no_outliers_exp_100$CD202 <- Sale_Date_coef_no_outliers_exp_100$CD202 / 23.21680

Sale_Date_coef_no_outliers_exp_100$CD203 <- Sale_Date_coef_no_outliers_exp_100$CD203 / 21.90980
Sale_Date_coef_no_outliers_exp_100$CD204 <- Sale_Date_coef_no_outliers_exp_100$CD204 / 28.67358
Sale_Date_coef_no_outliers_exp_100$CD205 <- Sale_Date_coef_no_outliers_exp_100$CD205 / 43.18703
Sale_Date_coef_no_outliers_exp_100$CD206 <- Sale_Date_coef_no_outliers_exp_100$CD206 / 29.29188
Sale_Date_coef_no_outliers_exp_100$CD207 <- Sale_Date_coef_no_outliers_exp_100$CD207 / 35.19188
Sale_Date_coef_no_outliers_exp_100$CD208 <- Sale_Date_coef_no_outliers_exp_100$CD208 / 38.86539
Sale_Date_coef_no_outliers_exp_100$CD209 <- Sale_Date_coef_no_outliers_exp_100$CD209 / 46.72083
Sale_Date_coef_no_outliers_exp_100$CD210 <- Sale_Date_coef_no_outliers_exp_100$CD210 / 44.80350
Sale_Date_coef_no_outliers_exp_100$CD211 <- Sale_Date_coef_no_outliers_exp_100$CD211 / 42.53936
Sale_Date_coef_no_outliers_exp_100$CD212 <- Sale_Date_coef_no_outliers_exp_100$CD212 / 41.71473
Sale_Date_coef_no_outliers_exp_100$CD301 <- Sale_Date_coef_no_outliers_exp_100$CD301 / 24.01862
Sale_Date_coef_no_outliers_exp_100$CD302 <- Sale_Date_coef_no_outliers_exp_100$CD302 / 30.06850
Sale_Date_coef_no_outliers_exp_100$CD303 <- Sale_Date_coef_no_outliers_exp_100$CD303 / 23.53923
Sale_Date_coef_no_outliers_exp_100$CD304 <- Sale_Date_coef_no_outliers_exp_100$CD304 / 19.72507
Sale_Date_coef_no_outliers_exp_100$CD305 <- Sale_Date_coef_no_outliers_exp_100$CD305 / 28.17155
Sale_Date_coef_no_outliers_exp_100$CD306 <- Sale_Date_coef_no_outliers_exp_100$CD306 / 25.72345
Sale_Date_coef_no_outliers_exp_100$CD307 <- Sale_Date_coef_no_outliers_exp_100$CD307 / 30.34844
Sale_Date_coef_no_outliers_exp_100$CD308 <- Sale_Date_coef_no_outliers_exp_100$CD308 / 25.81073
Sale_Date_coef_no_outliers_exp_100$CD309 <- Sale_Date_coef_no_outliers_exp_100$CD309 / 28.87826
Sale_Date_coef_no_outliers_exp_100$CD310 <- Sale_Date_coef_no_outliers_exp_100$CD310 / 35.17948
Sale_Date_coef_no_outliers_exp_100$CD311 <- Sale_Date_coef_no_outliers_exp_100$CD311 / 35.55860
Sale_Date_coef_no_outliers_exp_100$CD312 <- Sale_Date_coef_no_outliers_exp_100$CD312 / 29.80536
Sale_Date_coef_no_outliers_exp_100$CD313 <- Sale_Date_coef_no_outliers_exp_100$CD313 / 41.42648
Sale_Date_coef_no_outliers_exp_100$CD314 <- Sale_Date_coef_no_outliers_exp_100$CD314 / 32.48993
Sale_Date_coef_no_outliers_exp_100$CD315 <- Sale_Date_coef_no_outliers_exp_100$CD315 / 43.71320
Sale_Date_coef_no_outliers_exp_100$CD316 <- Sale_Date_coef_no_outliers_exp_100$CD316 / 51.06392
Sale_Date_coef_no_outliers_exp_100$CD317 <- Sale_Date_coef_no_outliers_exp_100$CD317 / 41.72207
Sale_Date_coef_no_outliers_exp_100$CD318 <- Sale_Date_coef_no_outliers_exp_100$CD318 / 42.57291
Sale_Date_coef_no_outliers_exp_100$CD401 <- Sale_Date_coef_no_outliers_exp_100$CD401 / 32.14922
Sale_Date_coef_no_outliers_exp_100$CD402 <- Sale_Date_coef_no_outliers_exp_100$CD402 / 30.84650
Sale_Date_coef_no_outliers_exp_100$CD403 <- Sale_Date_coef_no_outliers_exp_100$CD403 / 34.46245
Sale_Date_coef_no_outliers_exp_100$CD404 <- Sale_Date_coef_no_outliers_exp_100$CD404 / 36.39241
Sale_Date_coef_no_outliers_exp_100$CD405 <- Sale_Date_coef_no_outliers_exp_100$CD405 / 33.08798
Sale_Date_coef_no_outliers_exp_100$CD406 <- Sale_Date_coef_no_outliers_exp_100$CD406 / 36.96377
Sale_Date_coef_no_outliers_exp_100$CD407 <- Sale_Date_coef_no_outliers_exp_100$CD407 / 31.39615
Sale_Date_coef_no_outliers_exp_100$CD408 <- Sale_Date_coef_no_outliers_exp_100$CD408 / 46.79691
Sale_Date_coef_no_outliers_exp_100$CD409 <- Sale_Date_coef_no_outliers_exp_100$CD409 / 44.24588
Sale_Date_coef_no_outliers_exp_100$CD410 <- Sale_Date_coef_no_outliers_exp_100$CD410 / 33.78644
Sale_Date_coef_no_outliers_exp_100$CD411 <- Sale_Date_coef_no_outliers_exp_100$CD411 / 49.22534
Sale_Date_coef_no_outliers_exp_100$CD412 <- Sale_Date_coef_no_outliers_exp_100$CD412 / 49.13680
Sale_Date_coef_no_outliers_exp_100$CD413 <- Sale_Date_coef_no_outliers_exp_100$CD413 / 41.70413
Sale_Date_coef_no_outliers_exp_100$CD414 <- Sale_Date_coef_no_outliers_exp_100$CD414 / 35.07541
Sale_Date_coef_no_outliers_exp_100$CD501 <- Sale_Date_coef_no_outliers_exp_100$CD501 / 52.38016
Sale_Date_coef_no_outliers_exp_100$CD502 <- Sale_Date_coef_no_outliers_exp_100$CD502 / 39.93289
Sale_Date_coef_no_outliers_exp_100$CD503 <- Sale_Date_coef_no_outliers_exp_100$CD503 / 53.01963







"Creating A Loop For CD"
list.of.CD.data <- list(a=`101`, b=`102`)
for(i in 1:length(list.of.CD.data)){
my.data <- list.of.CD.data[[i]]
}

CD_data_list <- list(a=data.frame(`101`), b=data.frame(`102`)
                     )
Regression_CDs <- function(my.data)
  lm(ln_sale_price ~ ln_land_sqft +Total_units)

for(i in 1:length(CD_data_list)){
  Regression_CDs(my.data=list.of.CD.data[[i]])
}
  
results <- lapply(list.of.CD.data, FUN = Regression_CDs)


print(results)
(`101`)
if(Community_District == 101)
  print(lm(ln_sale_price ~ ln_land_sqft)) data = Complete_merged_data

Community_District_101 <- subset(Complete_merged_data, Community_District == 101)

lm(`101`$ln_sale_price ~ `101`$ln_land_sqft)

"Creating seprate data frames for Community District"
list_cd <- split(Complete_merged_data, Community_District) 
list2env(list_cd, envir = .GlobalEnv)

any(is.na(Community_District))
lm(`105$ln_sale_price` ~ `105$ln_land_sqft`)

ifelse(Community_District ==101, lm(ln_sale_price ~ Community_District),"" )
options(max.print = 1)

for(i in 2:ncol(Complete_merged_data)) {                
  
  predictors_i <- colnames(Complete_merged_data)[2:i]    

    mod_summaries <- summary(
    if(Community_District == 102)
      print(lm(ln_sale_price ~ Community_District)), Complete_merged_data[ , c("ln_sale_price", predictors_i)])
}

  mod_summaries
  
summary(Community_District_101$ln_sale_price)

subset(Community_District)

summary(`203`)
summary(`203`$BBL)

cd105_land_sqft <- `105$ln_sale_price`

ls(`105`)
ls(`105$ln_sale_price`)
ls(`105$ln_land_sqft`)
 
ls(`203`) 
ls(`203$[1])


length(Community_District)
count(Community_District)
group_by(Complete_merged_data$cd)

"Newest attempt at loop regression for CD"

lm(ln_sale_price ~ ln_land_sqft + Total_units + Sale_date)

summary(Community_District)

depVarlist <- setdiff(colnames(Complete_merged_data), c(ln_land_sqft, Total_units))

lapply(depVarlist, function(x){
  lm(ln_sale_price ~ ln_land_sqft + Total_units)
}

lm(ln_sale_price ~ ln_land_sqft + Total_units, data = `101`, na.action = "na.exclude")
   
levels(Community_District)
lapply(Complete_merged_data, levels)
is.factor(Community_District)
Community_District_factor <- as.factor(Community_District)
levels(Complete_merged_data$cd)
length(Complete_merged_data$cd)


Complete_merged_data$Sale_date
length(Zip_code_dummies)
length(Year_build)
length(Year_built)
length(ln_sale_price)
out[[1]]
rm(out)

outsum

levels(length(unique(Complete_merged_data$cd)))
length(Total_units)
length(Years_of_sale)
data.frame(outsum)

length(ln_land_sqft)
length(Total_units)

edit(Year_of_Sale)
edit(Sale_date)
Year_of_sale_2003 <- ifelse(Year_of_Sale == 2003, print('1'), print('0'))
length(Year_of_sale_2003)

out=with(Complete_merged_data,
         by(Complete_merged_data, Community_District, function(x) lm(ln_sale_price_integer ~ Sale_date_2004_integer, data = x)))

Community_District_interger <- as.integer(Community_District)
is.integer(Community_District_interger)
is.integer(Sale_date_2004_integer)
length(Sale_date_2003_interger)
length(ln_sale_price_integer)
length(Community_District)
any(is.infinite(Sale_date_2003))
class(Community_District)
class(ln_sale_price)
ln_sale_price_integer <- as.integer(ln_sale_price)

summary(Year_of_Sale)

lm(ln_sale_price)

scatter(ln_sale_price)
out=with(Complete_merged_data,
         by(Complete_merged_data, Community_District, function(x) lm(ln_sale_price ~ ln_land_sqft, data = x)))


class(Sale_date_2003_interger)
plot(, Sale_date)
Sale_date_2003 <- integer(Sale_date_2003)
Sale_date_2003_interger <- as.integer(Sale_date_2003)
class(Sale_date)
Complete_merged_data$Gross_sqft_noNA <-  
Complete_merged_data_NAomit$cd <- factor(Complete_merged_data_NAomit$cd)
any(is.na(Community_District))
sapply(Complete_merged_data, class)
is.integer(Sale_date_2003)
is.numeric(Sale_date_2003)
is.factor(Community_District)
is.factor(ln_sale_price)
is.factor(Year_of_Sale)
count(any(is.na(Community_District)))
length(Year_of_Sale)
Sale_date_2003 <- factor(Sale_date_2003)
"To MAKE NUMERIC INTO FACTOR"
Year_of_Sale <- factor(Year_of_Sale)
Community_District <- factor(Community_District)
ln_sale_price <- factor(ln_sale_price)
is.numeric(Complete_merged_data$Sale_price)
pa
par("mar")
par(mar=c(1,1,1,1))
which(is.na(Community_District))

levels(Complete_merged_data$cd)
lm(ln_sale_price ~ ln_land_sqft + Total_units + Complete_merged_data$Sale_date, data = `101`)
lm(ln)
length(Complete_merged_data$Sale_date_2003)
lm(ln_sale_price ~ ln_land_sqft)
lapply(Complete_merged_data, levels)
length(num_floor)
lm(ln_sale_price ~ ln_land_sqft + Total_units + Sale_date, data = `101`)

num_floor <- Complete_merged_data$numfloors
lm(ln_sale_price ~ ln_land_sqft + Total_units + Complete_merged_data$Sale_date_2003 + Complete_merged_data$Sale_date_2004 +
     Complete_merged_data$Sale_date_2005 + Complete_merged_data$Sale_date_2006 + Complete_merged_data$Sale_date_2007 + Complete_merged_data$Sale_date_2008 +
     Complete_merged_data$Sale_date_2009 + Complete_merged_data$Sale_date_2010 + Complete_merged_data$Sale_date_2011 + Complete_merged_data$Sale_date_2012 +
  Complete_merged_data$Sale_date_2013 + Complete_merged_data$Sale_date_2014 + Complete_merged_data$Sale_date_2015 + Complete_merged_data$Sale_date_2016 +
  Complete_merged_data$Sale_date_2017 + Complete_merged_data$Sale_date_2018 + Complete_merged_data$Sale_date_2019 + Complete_merged_data$Sale_date_2020)

Complete_merged_data$Sale_date_2003 <- ifelse(Sale_date == 2003, print("1"), print("0"))
Sale_date_2004 <- ifelse(Sale_date == 2004, print("1"), print("0"))
Sale_date_2005 <- ifelse(Sale_date == 2005, print("1"), print("0"))
Sale_date_2006 <- ifelse(Sale_date == 2006, print("1"), print("0"))
Sale_date_2007 <- ifelse(Sale_date == 2007, print("1"), print("0"))
Sale_date_2008 <- ifelse(Sale_date == 2008, print("1"), print("0"))
Sale_date_2009 <- ifelse(Sale_date == 2009, print("1"), print("0"))
Sale_date_2010 <- ifelse(Sale_date == 2010, print("1"), print("0"))
Sale_date_2011 <- ifelse(Sale_date == 2011, print("1"), print("0"))
Sale_date_2012 <- ifelse(Sale_date == 2012, print("1"), print("0"))
Sale_date_2013 <- ifelse(Sale_date == 2013, print("1"), print("0"))
Sale_date_2014 <- ifelse(Sale_date == 2014, print("1"), print("0"))
Sale_date_2015 <- ifelse(Sale_date == 2015, print("1"), print("0"))
Sale_date_2016 <- ifelse(Sale_date == 2016, print("1"), print("0"))
Sale_date_2017 <- ifelse(Sale_date == 2017, print("1"), print("0"))
Sale_date_2018 <- ifelse(Sale_date == 2018, print("1"), print("0"))
Sale_date_2019 <- ifelse(Sale_date == 2019, print("1"), print("0"))
Sale_date_2020 <- ifelse(Sale_date == 2020, print("1"), print("0"))


length(Year_of_sale_2003)
Summary(Year_of_Sale)

is.list(out)
length(Year_of_Sale_dummies)
sum(is.nan(Year_of_sale_2003))
summarise(Year_of_sale_2003)
lm(ln_sale_price ~ ln_land_sqft + Year_of_sale_2003)
is.infinite(Year_of_Sale)

summary(Complete_merged_data$numfloors)
summary(num_floors)
length(num_floors)
hist(ln_ppsf)
NA <- Complete_merged_data(num_floors == 0)

view(Complete_merged_data$Building_class_category)

summary(coefmat)

length(Year_2003)
 summary(num_floors)
   
outsum[[2]]    
outsum <- lapply(out, summary)
coefmat <- lapply(outsum, coefficients)
edit(coefmat)
coefmat[2]
coefmat <- data.frame(coefmat)
(coefmat)
summary(str(coefmat))
coefmat[[1]]
coefmat[[3]]
view(coefmat)
coefmat[[1]]
list(coefmat[[1]]:coefmat[[65]]
  separate()   )
summary(coefmat)
1:65

data.frame(out)
memory.limit(1000000)

summary(Complete_merged_data$lot)
str(coefmat)
outsum[[1]]
out[1]
out <- out[-c(25, 26, 45, 60, 61, 65)]
coefmat[[]]
edit(Community_district_coef)
view(coefmat)
outsum[1]
filter(coefmat, )
filter
Community_district_coef <- coefmat[seq(1, length(coefmat),4)]
Community_district_coef <- data.frame(out)
data.frame(outsum)
view(Community_district_coef)

martix
head(outsum)
coefmat[2,]

length(out)
lm(ln_sale_price ~ ln_land_sqft + ln_gro Total_units + Year_built)

ci <- matrix(nrow = 3, ncol=65)
ci[1]=coefmat[2,]
ci[2]=coefmat[3]
ci[3]=coefmat[4]

ts

head(ci)

length(out)
head(ci)
lm()
is.list(out)
summary(out[[1]])

length(Complete_merged_data$cd)
summary(Complete_merged_data$cd)
Community_District <- print(unique(Complete_merged_data$cd))

out[[1]]
length(out)
summary(out)
coefmat[1]

for (Community_District in 1:500) {
  if(Community_District == 101){
  print(lm(ln_sale_price ~ ln_land_sqft))
    }else{
    print("")
  }
}
  
edit(Community_District == 101)

nrow(filter(Community_District ==101))
length(Community_District ==101)
length(Community_District ==302)
length(Community_District)

   purrr::map2_df(unique(Community_District), function(modelCommunity_Districts)
  myModel = lm(ln_sale_price ~ ln_land_sqft + Total_units, data = Complete_merged_data %>% filter(Community_District == modelCommunity_Districts) %>% select(-Community_District))
  data.frame(Community_District = modelCommunity_Districts, Coefficients = summary(myModel))


"to delete INF values"
DT <- data.table(Complete_merged_data)
do.call(data.frame,lapply(Complete_merged_data, function(DTln_gross_sqft1) replace(DTln_gross_sqft1, is.infinite(DTln_gross_sqft1),NA)))

summary(ln_gross_sqft)

is.na()
lm(ln_sale_price ~ ln_gross_sqft)
lm(ln_sale_price ~ Community_District)
is.nan(ln_gross_sqft)
is.infinite(Complete_merged_data$ln_gross_sqft)
DTln_gross_sqft1 <- Complete_merged_data$ln_gross_sqft

CD_value <- unique(Complete_merged_data[c("cd")])
summary(CD_value)
print(CD_value)

is.infinite(DT$ln_gross_sqft)

ln_gross_sqft <- Complete_merged_data$ln_gross_sqft

rm(ln_gross_sqft)

ln_




martix_out_101 <- data.frame(out$`101`, row.names = NULL, check.rows = FALSE, check.names = TRUE, fix.empty.names = TRUE, stringsAsFactors = default.stringsAsFactors())
   
   
system.time(invisible(lapply(names(),function(DTln_gross_sqft) set(DT, which(is.infinite(DT[[DTln_gross_sqft]])), j = DTln_gross_sqft, value =NA))))

num_floors <- Complete_merged_data$numfloors

system.time(invisible(lapply(names(Complete_merged_data),function(ln_gross_sqft) set(Complete_merged_data, which(is.infinite(Complete_merged_data[[ln_gross_sqft]])), j = ln_gross_sqft, value =NA))))

system.time(invisible(lapply(names(Complete_merged_data),function(num_floors) set(Complete_merged_data, which(is.infinite(Complete_merged_data[[num_floors]])), j = num_floors, value =NA))))
count <- 0

rm(count)

for(val in Community_District){
  if(val ==  102) print(lm(ln_sale_price ~ ln_land_sqft))
}

lm(ln_sale_price ~ ln_land_sqft + Total_units, data = `101`, na.action = "na.exclude")

CD_regressions <- lapply(1:70, function(x) lm(ln_sale_price ~ Community_District))
(sapply(CD_regressions, coef))
summaries <- lapply(CD_regressions, summary)
lapply(summaries, function(x) x$coefficients[, c(1,70)])

summary(CD_regressions[[1]])
summary(CD_regressions[[2]])
edit(CD_regressions[[1]])



ln_gross_sqft
edit(Community_District)
"Smiple Regression"
simple_reg <- lm(ln_sale_price ~ ln_land_sqft)
summary(simple_reg)

lm(ln_sale_price ~ DT$ln_gross_sqft)

lm(ln_sale_price ~ Zip_code_Lower_Manhattan)

lm(ln_sale_price ~ ln_land_sqft + ln_gross_sqft + num_floors)
lm(no_outliers$ln_sale_price ~ no_outliers$ln_land_sqft)
length(Complete_merged_data$Sale_date_)
out[103]
levels(length(Complete_merged_data$cd))
outsum[9]
(Complete_merged_data$Sale_date)
"Main Regression"
main_reg <- lm(ln_sale_price ~ ln_land_sqft + Year_built + Total_units + Sale_date + Building_class_category)
length(Building_class_category)
Community_district_coef
count(Complete_merged_data$cd)
Year_2003
out2[2]
(coef(out))
out$102
options(max.print = 1000000)
options(scipen=999)
summary(main_reg)

main_reg <- lm(ln_sale_price ~ ln_land_sqft + Year_built + Total_units + Sale_date + Building_class_category)

lm(ln_sale_price ~ ln_land_sqft + Year_built + Total_units + Sale_date + Building_class_category + Year_of_Sale)
"GRAPHS"
Coefficient_yearofsale <- substr(Price_Index_data$Coefficent, 0, 8)

ggplot(Sale_Date_coef_no_outliers_exp_100_base_year_2003, aes(x= Sale_Date_coef_no_outliers_exp_100_base_year_2003$`Sale Date`, y= Sale_Date_coef_no_outliers_exp_100_base_year_2003$CD102)) + 
  geom_line() 
  

ggplot(Community.District.Coef.NYC, aes(x= Community.District.Coef.NYC$Year.of.Sale, y= Community.District.Coef.NYC$CD102)) + 
  geom_line()+
  geom_smooth()

install.packages("matplot")
library(matplot)

ggplot(Sale_Date_coef_no_outliers_exp_100_base_year_2003, aes(x=Sale_Date_coef_no_outliers_exp_100_base_year_2003$`Sale Date`)) + 
  geom_line(aes(y = Sale_Date_coef_no_outliers_exp_100_base_year_2003$CD101), color = "darkred") + 
  geom_line(aes(y = Sale_Date_coef_no_outliers_exp_100_base_year_2003$CD102), color="steelblue", linetype="twodash") + 
  geom_line(aes(y = Sale_Date_coef_no_outliers_exp_100_base_year_2003$CD103), color="yellow") +
  geom_line(aes(y = Sale_Date_coef_no_outliers_exp_100_base_year_2003$CD104), color="green") +
  geom_line(aes(y = Sale_Date_coef_no_outliers_exp_100_base_year_2003$CD105), color="purple") +
  geom_line(aes(y = Sale_Date_coef_no_outliers_exp_100_base_year_2003$CD106), color="black") +
  geom_line(aes(y = Sale_Date_coef_no_outliers_exp_100_base_year_2003$CD107), color="royalblue") +
  geom_line(aes(y = Sale_Date_coef_no_outliers_exp_100_base_year_2003$CD108), color="hotpink") +
  geom_line(aes(y = Sale_Date_coef_no_outliers_exp_100_base_year_2003$CD109), color="orange") +
  geom_line(aes(y = Sale_Date_coef_no_outliers_exp_100_base_year_2003$CD110), color="brown") +
  geom_line(aes(y = Sale_Date_coef_no_outliers_exp_100_base_year_2003$CD111), color="gold") +
  geom_line(aes(y = Sale_Date_coef_no_outliers_exp_100_base_year_2003$CD112), color="plum") 






matplot(dat, type = c("b"),pch=1,col = 1:4) #plot
legend("topleft", legend = 1:4, col=1:4, pch=1) # optional legend

coefmat[[1]]

ggplot(Community_district_coef, aes(x= Community_district_coef$X101.Estimate, y= Community_district_coef$X102.Estimate)) + 
  geom_point()+
  geom_smooth(method = "lm")

Complete_merged_data$Sale_date

hist(ln_sale_price)
options(scipen = 999)
if(Merged_data$Sale_price < 4111111767)
  
"excuding bottom and top 1%"

X <- Merged_data
outliers <- boxplot(Merged_data$Sale_price, plot=FALSE)$out
print(outliers)
XX <- X[-which(ln_sale_price %in% outliers),]

boxplot(XX$ln_sale_price)
scatter
Year_of_Sale
plot(ln_sale_price, Year_of_Sale)

Complete_merged_data$fi

quantile((ln_ppsf), .999)

summary(ln_ppsf)
no_outliers_complete_merged_data <- subset(Complete_merged_data, ln_ppsf > quantile((ln_ppsf), .05) & ln_ppsf < quantile((ln_ppsf), .999))
summary(no_outliers)
summary(Merged_data$Sale_price)
no_outliers <- subset(Merged_data, log(Merged_data$Sale_price)> (Q1 - 1.5*IQR) & log(Merged_data$Sale_price) < (Q3 + 1.5*IQR)
is.numeric(log(Merged_data$Sale_price))
hist(no_outliers$Sale_price)
boxplot(no_outliers$ln_sale_price)  
hist(no_outliers$ln_sale_price)
hist(ln_sale_price)
IQR <- IQR(log(Merged_data$Sale_price))
summary(ln_ppsf)
lm
(Complete_merged_data$ln ~ )

Q1 <- quantile(log(Merged_data$Sale_price), .001)
Q3 <- quantile(log(Merged_data$Sale_price), .99999)
  
rm(ln_sales_price_1_99)
boxplot(ln_sale_price)$out
boxplot(X)
"Creating New Data Set"

Years_of_sale <- c(2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020)
Cofficient <- c(-0.62736329, -0.43931778, -0.24036309, -0.12582101, -0.14842199, -0.30648531, -0.40979732, -0.40113394, -0.36852319, -0.33250287, 0.03870649, 0.14411667, 0.27344118, 0.31507149, 0.31703238, 0.38404839, -0.07591336, NA)

Sale_Year_vs_Cofficient <- data.frame(Years_of_sale, Cofficient)

'MESS AROUND AREA'

CD_factor <- Complete_merged_data %>% mutate(Complete_merged_data$cd == as.character(Complete_merged_data$cd))
(Community_District)
lm(ln_sale_price ~ ln_land_sqft + ln_gross_sqft + Year_of_Sale)
(CD_factor)
summary(Sale_date)
rm(ln_gross_sqft)
x_y <- data.frame(x, y)
x <- c(0, 2, 3, 4, 4, 6)
y <- c(38, 45, 50, 47, 63, 71)

summary(lm(x ~ y))
a_b <- data.frame(a,b)
lm(a~b)
lm(b~a)
a <- c(8, 10, 10.5, 11.5, 12, 13.5)
b <- c(66.5, 69.5, 72.5, 71.5, 73.5, 79.05)
ss <- lm(Complete_merged_data$ln_sale_price ~ Complete_merged_data$ln_gross_sqft + Complete_merged_data$ln_land_sqft + Complete_merged_data$ln_lot_area)
plot()
rm(Complete_merged_data_omitNA)
length(Sale_date_2004_integer)
Sale_date_2004_integer <- as.integer(Sale_date_2004)
Complete_merged_data <- Complete_merged_data[!is.na(Complete_merged_data$cd),]
any(is.na(Community_District))
Complete_merged_data
