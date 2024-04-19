#
# HAZIR VERI SETINDEN KESIFSEL VERI ANALIZI
# 
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
paste("Satır Sayısı:", nrow(df), sep=" ") # 1338
paste("Kolon Sayısı:", ncol(df), sep=" ") # 7

# kolon veri tiplerine bak
str(df)

# tum kolonlarda eksik deger iceren hucre var mi kontrol et (bu veride yok)
colSums(is.na(df))

# tekil kolon bazinda bakacak olsaydik (age) eksik deger varsa yerini bul (bu veride yok)
which(is.na(df$age))

# coklamis satir var mi? (bu veride 1 tane var)
sum(duplicated(df))

# coklamis satir varsa hangileri?
duplicates <- df[duplicated(df), ]
duplicates

# coklamis satirlari veriden cikar
df_temiz <- df[!duplicated(df), ]
df_temiz

# veriyi ozetlestirerek kolon bazinda temel istatistiklere bak
summary(df_temiz)

# her bir kolon icin veri frekansina/dagilimlarina bak, histogram uretimi
library(ggplot2)
ggplot(df_temiz, aes(x=age)) + geom_histogram() + ggtitle("Yaş Verisi Dağılımı")
ggplot(df_temiz, aes(x=sex)) + geom_histogram() + ggtitle("Cinsiyet Verisi Dağılımı")
ggplot(df_temiz, aes(x=bmi)) + geom_histogram() + ggtitle("Vücut Kitle Endeksi Verisi Dağılımı")
ggplot(df_temiz, aes(x=children)) + geom_histogram() + ggtitle("Çocuk Sayısı Verisi Dağılımı")
ggplot(df_temiz, aes(x=smoker)) + geom_histogram(stat="count") + ggtitle("Sigara İçen ve İçmeyen Verisi Dağılımı")
ggplot(df_temiz, aes(x=region)) + geom_histogram(stat="count") + ggtitle("Yaşanılan Bölge Verisi Dağılımı")
ggplot(df_temiz, aes(x=charges)) + geom_histogram() + ggtitle("Sigorta Giderleri Verisi Dağılımı")

# outlier/aykiri deger tespiti
boxplot(df_temiz[c("age", "bmi", "children", "charges")]) # numerik degiskenler secildi

# "charges" bazinda alt/lower ve ust/upper ceyreklikleri/quartile hesaplama
Q1 <- quantile(df_temiz$charges, 0.25)
Q3 <- quantile(df_temiz$charges, 0.75)
cat("charges icin -> ", "alt ceyreklik: ", Q1, "ust ceyreklik: ", Q3)

# "charges" bazinda alt ve ust sinir hesaplama
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
cat("charges icin -> ", "alt sinir: ", lower_bound, "ust sinir: ", upper_bound)

# "charges" bazinda aykiri deger iceren satirlari belirleme
outliers <- df_temiz$charges < lower_bound | df_temiz$charges > upper_bound
df_temiz$aykiri_deger_mi <- outliers
df_aykiri_degerler <- df_temiz[df_temiz$aykiri_deger_mi == TRUE, ]
paste("Toplam Aykırı Değer İçeren Satır Sayısı:", nrow(df_aykiri_degerler), sep=" ")

# "charges" bazinda aykiri degerlerden arindirilmis satirlari getir
df_hazir <- df_temiz[df_temiz$aykiri_deger_mi == FALSE, ]
df_hazir

# kategorik degiskenler icin dummy encoding
library(caret)
dummy_sex <- dummyVars(~ sex, data=df_hazir, sep="_")
df_sex_combined <- data.frame(predict(dummy_sex, newdata=df_hazir))
df_all <- cbind(df_hazir, df_sex_combined)

dummy_smoker <- dummyVars(~ smoker, data=df_hazir, sep="_")
df_smoker_combined <- data.frame(predict(dummy_smoker, newdata=df_hazir))
df_all <- cbind(df_all, df_smoker_combined)

dummy_region <- dummyVars(~ region, data=df_hazir, sep="_")
df_region_combined <- data.frame(predict(dummy_region, newdata=df_hazir))
df_all <- cbind(df_all, df_region_combined)
df_all

# korelasyon matrisi
library(corrplot)
corr_mat <- cor(df_all[c("age", "bmi", "children", "charges")]) # numerik degiskenler secildi
corrplot(corr_mat, method="number")

# scatterplot/sacilim grafigi olusturma (ikili ciftler halinde kontrol)
plot(df_all[c("age", "bmi", "children", "charges")])

#
# VERI URETEREK KESIFSEL VERI ANALIZI
#

library(MASS)

# tekrarlanabilirlik için seed belirleme
set.seed(999) 

# sample sayisi
n <- 100 

# bagimsiz degiskenler
x1 <- rnorm(n, mean = 10, sd = 2)
x2 <- rnorm(n, mean = 5, sd = 1) 
x3 <- rnorm(n, mean = 3, sd = 0.5) 
x4 <- rnorm(n, mean = 7, sd = 1.5) 
x5 <- rnorm(n, mean = 15, sd = 3)

# bagimli degisken
y <- 3 + 2*x1 - 1.5*x2 + 0.5*x3 + 1.8*x4 - 1.2*x5 + rnorm(n, mean = 0, sd = 2)

# dataframe'e donustur
df_sentetik <- data.frame(x1, x2, x3, x4, x5, y)

# veriyi kontrol et
head(df_sentetik)

# satir-kolon sayilarini kontrol et
paste("Satır Sayısı:", nrow(df_sentetik), sep=" ") # 100
paste("Kolon Sayısı:", ncol(df_sentetik), sep=" ") # 6

# coklamis satirlari veriden cikar
df_sentetik_temiz <- df_sentetik[!duplicated(df_sentetik), ]
df_sentetik_temiz

# veriyi ozetlestirerek kolon bazinda temel istatistiklere bak
summary(df_sentetik_temiz)

# her bir kolon icin veri frekansina/dagilimlarina bak, histogram uretimi
library(ggplot2)
ggplot(df_sentetik_temiz, aes(x=x1)) + geom_histogram() + ggtitle("x1 Verisi Dağılımı")
ggplot(df_sentetik_temiz, aes(x=x2)) + geom_histogram() + ggtitle("x2 Verisi Dağılımı")
ggplot(df_sentetik_temiz, aes(x=x3)) + geom_histogram() + ggtitle("x3 Verisi Dağılımı")
ggplot(df_sentetik_temiz, aes(x=x4)) + geom_histogram() + ggtitle("x4 Verisi Dağılımı")
ggplot(df_sentetik_temiz, aes(x=x5)) + geom_histogram() + ggtitle("x5 Verisi Dağılımı")
ggplot(df_sentetik_temiz, aes(x=y)) + geom_histogram() + ggtitle("y Verisi Dağılımı")

# outlier/aykiri deger tespiti
boxplot(df_sentetik_temiz)

# "y" bazinda alt/lower ve ust/upper ceyreklikleri/quartile hesaplama
Q1 <- quantile(df_sentetik_temiz$y, 0.25)
Q3 <- quantile(df_sentetik_temiz$y, 0.75)
cat("y icin -> ", "alt ceyreklik: ", Q1, "ust ceyreklik: ", Q3)

# "y" bazinda alt ve ust sinir hesaplama
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
cat("y icin -> ", "alt sinir: ", lower_bound, "ust sinir: ", upper_bound)

# "y" bazinda aykiri deger iceren satirlari belirleme
outliers <- df_sentetik_temiz$y < lower_bound | df_sentetik_temiz$y > upper_bound
df_sentetik_temiz$aykiri_deger_mi <- outliers
df_sentetik_aykiri_degerler <- df_sentetik_temiz[df_sentetik_temiz$aykiri_deger_mi == TRUE, ]
paste("Toplam Aykırı Değer İçeren Satır Sayısı:", nrow(df_sentetik_aykiri_degerler), sep=" ")

# "y" bazinda aykiri degerlerden arindirilmis satirlari getir
df_sentetik_hazir <- df_sentetik_temiz[df_sentetik_temiz$aykiri_deger_mi == FALSE, ]
df_sentetik_hazir

# korelasyon matrisi
library(corrplot)
corr_mat <- cor(df_sentetik_hazir[c("x1", "x2", "x3", "x4", "x5", "y")])
corrplot(corr_mat, method="number")

# scatterplot/sacilim grafigi olusturma (ikili ciftler halinde kontrol)
plot(df_sentetik_hazir[c("x1", "x2", "x3", "x4", "x5", "y")])
