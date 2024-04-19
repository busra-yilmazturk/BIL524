# Brett Lantz tarafindan yazilmis "Machine Learning with R" kitabindan alinmis
# saglik sigorta giderleri veri seti
# 
# yas (age), cinsiyet (sex), vucut kitle endeksi (bmi), cocuk sayisi (children),
# sigara icip icmedigi (smoker), yasadigi bolge (region) ve sigorta giderleri (charges)
# olmak uzere toplam 7 kolon ve 1338 satir icerir
# kaynak: https://github.com/stedy/Machine-Learning-with-R-datasets

# veriyi oku
df <- read.csv("https://raw.githubusercontent.com/stedy/Machine-Learning-with-R-datasets/master/insurance.csv")

# veriyi kontrol et
head(df)

# satir-kolon sayilarini kontrol et
paste("Satır Sayısı: ", nrow(df), sep=" ") # 1338
paste("Kolon Sayısı: ", ncol(df), sep=" ") # 7

# kolon veri tiplerine bak
str(df)

# tum kolonlarda eksik deger iceren hucre var mi kontrol et (bu veride yok)
colSums(is.na(df))

# tekil kolon bazinda bakacak olsaydik (age) eksik deger varsa yerini bul (bu veride yok)
which(is.na(df$age))

# coklamis satir var mi? (bu veride 1 tane var)
sum(duplicated(df))

# coklamis satir varsa hangileri? (bu veride yok)
duplicates <- df[duplicated(df), ]
duplicates

# veriyi ozetlestirerek kolon bazinda temel istatistiklere bak
summary(df)

# her bir kolon icin veri frekansina/dagilimlarina bak, histogram uretimi
library(ggplot2)
ggplot(df, aes(x=age)) + geom_histogram() + ggtitle("Yaş Verisi Dağılımı")
ggplot(df, aes(x=sex)) + geom_histogram() + ggtitle("Cinsiyet Verisi Dağılımı")
ggplot(df, aes(x=bmi)) + geom_histogram() + ggtitle("Vücut Kitle Endeksi Verisi Dağılımı")
ggplot(df, aes(x=children)) + geom_histogram() + ggtitle("Çocuk Sayısı Verisi Dağılımı")
ggplot(df, aes(x=smoker)) + geom_histogram(stat="count") + ggtitle("Sigara İçen ve İçmeyen Verisi Dağılımı")
ggplot(df, aes(x=region)) + geom_histogram(stat="count") + ggtitle("Yaşanılan Bölge Verisi Dağılımı")
ggplot(df, aes(x=charges)) + geom_histogram() + ggtitle("Sigorta Giderleri Verisi Dağılımı")

# outlier/aykiri deger tespiti
boxplot(df[c("age", "bmi", "children", "charges")]) # numerik degiskenler secildi

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
corr_mat <- cor(df[c("height", "weight", "age", "male")])
corrplot(corr_mat, method="number")

# scatterplot/sacilim grafigi olusturma (ikili ciftler halinde kontrol)
plot(df[c("height", "weight", "age", "male")])
