# temel fonksiyonlar

# veriyi kaynagindan indirdikten sonra kendi
# calisma alanimda /data isimli bir klasor acarak
# icine yukledim
veri = read.csv("data/Youth_Tobacco_Survey_YTS_Data.csv")
View(veri)

paste("Satır Sayısı: ", nrow(veri), sep=" ")
paste("Kolon Sayısı: ", ncol(veri), sep=" ")
