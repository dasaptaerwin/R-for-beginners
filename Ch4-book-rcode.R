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
