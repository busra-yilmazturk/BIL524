# temel fonksiyonlar
dikdortgen_alan_hesapla <- function(a, b) {
  return(a*b)
}
print(dikdortgen_alan_hesapla(5, 4))

# veriyi kaynagindan indirdikten sonra kendi
# calisma alanimda /data isimli bir klasor acarak
# icine yukledim
veri = read.csv("data/Youth_Tobacco_Survey_YTS_Data.csv")
View(veri)

View(mtcars)

paste("Satır Sayısı: ", nrow(veri), sep=" ")
paste("Kolon Sayısı: ", ncol(veri), sep=" ")

install.packages("dplyr")
install.packages("readr")
install.packages("tidyverse")

library(dplyr)
veri_yeniden_ad = rename(veri, Year=YEAR)
View(veri_yeniden_ad)

library(readr)
write_csv(veri_yeniden_ad, file="data/tutun.csv")

# veriyi dataframe'e cevir
df_veri = as_tibble(veri)

# veriden kolon sec
select(df_veri, c(LocationAbbr, LocationDesc))

# veri filtreleme
filter(df_veri, LocationAbbr=="AZ" | LocationAbbr=="TN")

# veriye yeni kolon ekleme
df_veri_kolon = mutate(df_veri, Data_Value_X2 = Data_Value * 2)
View(df_veri_kolon)
