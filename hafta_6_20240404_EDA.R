# nancy howell tarafindan toplanmis populasyon veri seti
# boy, kilo, yas ve cinsiyet kolonlarini icerir, toplam 544 satir
# kaynak: https://tspace.library.utoronto.ca/handle/1807/17973

# veriyi oku
df = read.csv("data/Howell1.csv", sep=";")

# veriyi kontrol et
head(df)

# satir-kolon sayilarini kontrol et
paste("Satır Sayısı: ", nrow(df), sep=" ") # 544
paste("Kolon Sayısı: ", ncol(df), sep=" ") # 4

# veriyi ozetlestirerek kolon bazinda temel istatistiklere bak
summary(df)