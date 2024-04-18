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

# kolon veri tiplerine bak
str(df)

# kolon bazinda eksik deger var mi kontrol et
colSums(is.na(df))

# kolon bazinda eksik deger varsa yerini bul
which(is.na(df$height))

# coklamis satir var mi? (bu veride yok)
sum(duplicated(df))

# coklamis satir varsa hangileri?
duplicates <- df[duplicated(df), ]
duplicates

# veriyi ozetlestirerek kolon bazinda temel istatistiklere bak
summary(df)

# her bir kolon icin veri frekansina/dagilimlarina bak, histogram uretimi
library(ggplot2)
ggplot(df, aes(x=height)) + geom_histogram() + ggtitle("Height Verisi Dağılımı")
ggplot(df, aes(x=weight)) + geom_histogram() + ggtitle("Weight Verisi Dağılımı")
ggplot(df, aes(x=age)) + geom_histogram() + ggtitle("Age Verisi Dağılımı")
ggplot(df, aes(x=male)) + geom_histogram() + ggtitle("Male Verisi Dağılımı")

# outlier/aykırı deger tespiti
boxplot(df)

# height bazinda alt/lower ve ust/upper ceyreklikleri/quartile hesaplama
Q1 <- quantile(df$height, 0.25)
Q3 <- quantile(df$height, 0.75)
cat("height icin -> ", "alt ceyreklik: ", Q1, "ust ceyreklik: ", Q3)

# height bazinda alt ve ust sinir hesaplama
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
cat("height icin -> ", "alt sinir: ", lower_bound, "ust sinir: ", upper_bound)

# height bazinda aykiri deger iceren satirlari belirleme
outliers <- df$height < lower_bound | df$height > upper_bound
df$aykiri_deger_mi <- outliers
df_aykiri_degerler <- df[df$aykiri_deger_mi == TRUE, ]
df_aykiri_degerler

# korelasyon matrisi
library(corrplot)
corr_mat <- cor(df)
corrplot(corr_mat, method="number")

# scatterplot/sacilim grafigi olusturma (ikili ciftler halinde kontrol)
plot(df)
