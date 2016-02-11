data <- read.csv("Data1-WaterTemp.csv") # import data ke dalam R
data # melihat isi data atau anda bisa membuka file dari jendela _Global Environment_.
str(data) # untuk melihat struktur data (terlihat sebagai `int` atau `integer`)
dim(data) # untuk melihat dimensi data (jumlah kolom x jumlah baris) ada 20 baris atau sampel dan 2 kolom. 
summary(data) # untuk melihat ringkasan data secara statistik

###
sort(data, decreasing=F) # muncul pesan kesalahan (_error message_). Mengapa?
sort(data$Temp, decreasing=F)
