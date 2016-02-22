# membuat data frame
datamurid <- data.frame(nama = c("Abi", "Aci", "Adi", "Afi", "Agi", "Ali"),
                        tahun.lahir = c(76, 78, 79, 80, 83, 87), 
                        usia = c(38, 36, 35, 34, 31, 27)
                        ) 

datamurid2 <- data.frame(nama = c("Ani", "Ami", "Aki"),
                         tahun.lahir = c(76, 78, 79), 
                         usia = c(38, 36, 35)
                         )   

# menggabungkan dua data frame `merge()` vs `rbind()` 
datamuridtotal <- merge(datamurid, datamurid2, by="nama") 
datamuridrevisi <- rbind(datamurid, datamurid2) 
datamuridtotal # merge gagal
datamuridrevisi # rbind berhasil

# operasi `merge()`
## membuat data frame baru
df1 <- data.frame(ref = c('Ref1', 'Ref2'), 
                label = c('Label01', 'Label02')
                )

df2 <- data.frame(id = c('A1', 'C2', 'B3', 'D4'), 
                  ref = c('Ref1', 'Ref2' , 'Ref3','Ref1'), 
                  val = c( 1.11, 2.22, 3.33, 4.44 )
                  )
df1
df2

## `merge()`
dftotal <- merge(df1, df2, by='ref', all.y = T, sort= T)
dftotal

# manipulasi data dengan `dplyr` package

install.packages("dplyr") # instalasi package (lewati bila telah dilakukan sebelumnya)
library(dplyr) # load packages
library(hflights) # load data set internal R

## explore data
data(hflights)
head(hflights)

## merapihkan tampilan layar 
flights <- tbl_df(hflights) # convert to local data frame
flights # memperlihatkan hanya 10 baris dan dengan jumlah kolom mengikuti lebar layar

print(flights, n=20) # mengubah jumlah baris yang akan ditampilkan
data.frame(head(flights)) # mengkonversi menjadi data frame normal untuk melihat seluruh kolom

## filter data
# melihat semua penerbangan di tanggal 1 Januari menggunakan fungsi dasar R
flights[flights$Month==1 & flights$DayofMonth==1, ]

# melihat semua penerbangan di tanggal 1 Januari menggunakan dplyr
# anda dapat menggunakan "," atau "&", hasilnya akan sama
filter(flights, Month==1, DayofMonth==1)
filter(flights, Month==1 & DayofMonth==1)

# gunakan perintah `piping` untuk mengatur kondisi fungsi 
filter(flights, UniqueCarrier=="AA" | UniqueCarrier=="UA")

#atau menggunakan operator `%in%` 
filter(flights, UniqueCarrier %in% c("AA", "UA"))

## select()
# memilih (select) kolom DepTime, ArrTime, and FlightNum columns dengan fungsi dasar R
flights[, c("DepTime", "ArrTime", "FlightNum")]

# bila menggunakan dplyr
select(flights, DepTime, ArrTime, FlightNum)

# gunakan simbol colon ":" untuk memilih beberapa kolom yang menerus (bersebelahan) dan gunakan `contains` untuk menyeleksi kolom dengan kriteria tertentu 
# anda dapat menggunakan `starts_with`, `ends_with`, dan `matches` untuk menyeleksi nama kolom
select(flights, Year:DayofMonth, contains("Taxi"), contains("Delay"))

## arrange()
# menggunakan fungsi dasar R untuk memilih (select) kolom "UniqueCarrier" dan "DepDelay" dan menyortirnya berdasarkan kolom "DepDelay""
flights[order(flights$DepDelay), c("UniqueCarrier", "DepDelay")]

# Dengan urutan yang sama kita pakai teknik dplyr
flights %>%
  select(UniqueCarrier, DepDelay) %>%
  arrange(DepDelay)

# Kalau anda ingin menyortir mengecil ke bawah (secara _descending_), gunakan operator “desc”.
flights %>%
  select(UniqueCarrier, DepDelay) %>%
  arrange(desc(DepDelay))

## mutate()
# Bila menggunakan fungsi dasar R, membuat variabel bar "Speed" (in mph)
flights$Speed <- flights$Distance / flights$AirTime*60
flights[, c("Distance", "AirTime", "Speed")]

# menggunakan dplyr 
flights %>%
  select(Distance, AirTime) %>%
  mutate(Speed = Distance/AirTime*60)

# atau anda bisa mempersingkatnya menjadi:
flights <- flights %>% mutate(Speed = Distance/AirTime*60)

## summarise()
# menggunakan fungsi dasar 
head(with(flights, tapply(ArrDelay, Dest, mean, na.rm=TRUE)))

# atau gunakan or you can use "aggregate"
head(aggregate(ArrDelay ~ Dest, flights, mean))
# menggunakan dplyr: buat table digrup berdasarkan "Dest" kemudian then `summarise` each group dengan menghitung rata-rata (mean) "ArrDelay"
flights %>%
  group_by(Dest) %>%
  summarise(avg_delay = mean(ArrDelay, na.rm=TRUE))

# untuk setiap "carrier", hitung persentase penerbangan yang dibatalkan (cancelled) atau diubah rutenya (diverted)
flights %>%
  group_by(UniqueCarrier) %>%
  summarise_each(funs(mean), Cancelled, Diverted)

# untuk tiap penerbangan, hitung minimum dan maksimum delay untuk kedatangan dan keberangkatan
flights %>%
  group_by(UniqueCarrier) %>% 
  summarise_each(funs(min(., na.rm=TRUE), max(., na.rm=TRUE)), matches("Delay"))

## Mengubah format data dengan package "reshape"
install.packages("reshape2")
library(reshape2)
names(airquality) <- tolower(names(airquality)) # membuat nama kolom menjadi huruf kecil (lower case)
head(airquality)

# menggunakan melt() dengan argumen standar
head(airquality)
aql <- melt(airquality) 
head(aql)

# menggunakan melt() dengan id_variable
head(airquality)
aql <- melt(airquality, id.vars = c("month", "day"))
head(aql)

# mengendalikan nama kolom dalam _long_ data format yang baru
aql <- melt(airquality, id.vars = c("month", "day"),
            variable.name = "variabel_iklim", 
            value.name = "nilai_iklim")
head(aql)

# menggunakan dcast()
head(airquality)
aql <- melt(airquality, id.vars = c("month", "day")) 
aqw <- dcast(aql, month + day ~ variable)
head(aqw)

# agregasi (aggregate)
dcast(aql, month ~ variable, fun.aggregate = mean, 
      na.rm = TRUE)

