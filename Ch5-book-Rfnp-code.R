# mtcars
attach(mtcars) # untuk memberitahu R bahwa kita menggunakan dataset `mtcars`
mtcars # melihat isi data 
str(mtcars) # melihat tipe data, seluruhnya numerik
dim(mtcars) # melihat dimensi data = 32 baris dan 11 kolom
plot(wt, mpg) # membuat scatterplot antara wt (weight) dan mpg (miles per galon)
abline(lm(mpg~wt)) # membuat garis regresi
title("Regresi antara berat mobil (wt)-konsumsi BBM (mpg)")

# plot pertamaku
x <- rnorm(100)
y <- rnorm(100)
plot(x, y, pch=21,
     mar=c(4,4,2,2),
     col='red',bg='black', 
     xlim=c(-3,3), 
     ylim=c(-3,3))
fit <- lm(y ~ x)
abline(fit, lwd = 3, col = "blue")
title('Plot pertamaku')
text(-2, -2, 'Label')
legend("topleft", 
       legend="Data", 
       pch=21,
       pt.bg='black', 
       col='red')

# Multiple plot dalam satu halaman
plot.new() # untuk membuat plot baru
par(mfrow= c(2,2)) # mengatur jumlah baris dan kolom
par(mar = c(3, 3, 2, 2)) # mengatur margin antar plot
plot(x, y, pch = 20, main="plot 1")
plot(x, z, pch = 19, main="plot 2")
plot(y, z, pch = 1, main="plot 3")
plot(y, z, pch = 5, main="plot 4")

# Plot dua grup data dalam satu grafik
plot.new()
x <- rnorm(100)
y <- x + rnorm(100)
g <- gl(2, 50, labels = c("Kelas A", "Kelas B"))
str(g)
plot(x,y, type='n') # Draws no points
points(x[g == "Kelas A"], y[g == "Kelas A"], 
       col="blue", 
       pch=1)
points(x[g == "Kelas B"], y[g == "Kelas B"], 
       col="red", 
       pch=19)
legend("topleft", c("Kelas A", "Kelas B"), 
       col=c("blue", "red"), 
       pch=c(1,19))

# membuat histogram

## histogram sederhana untuk variabel "mpg"
hist(mtcars$mpg) 

## histogram berwarna dengan pengaturan jumlah "Bins"
hist(mtcars$mpg, breaks=12, col="red") 

## Menambahkan kurva distribusi normal
x <- mtcars$mpg
h<-hist(x, 
        breaks=10, 
        col="red", 
        xlab="mil per galon",
        main="Histogram dengan garis kurva normal")
xfit <- seq(min(x),
            max(x),
            length=40)
yfit <- dnorm(xfit,
              mean=mean(x),
              sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)

## Kernel Density Plot
d <- density(mtcars$mpg) # returns the density data
plot(d) # plots the results

## Density Plot berwarna
d <- density(mtcars$mpg)
plot(d, 
     main="Kernel Density mil per galon")
polygon(d, 
        col="red", 
        border="blue") 

## dot plot sederhana
dotchart(mtcars$mpg,
         labels=row.names(mtcars),
         cex=.7,
         main="Konsumsi BBM berbagai merk mobil",          xlab="Miles Per Gallon")

mtcars
# Dotplot: Grouped Sorted and Colored
## Sort by mpg, group and color by cylinder
x <- mtcars[order(mtcars$mpg),] # sort by mpg
x$cyl <- factor(x$cyl) # it must be a factor
x$color[x$cyl==4] <- "red"
x$color[x$cyl==6] <- "blue"
x$color[x$cyl==8] <- "darkgreen"
dotchart(x$mpg,
         labels=row.names(x),
         cex=.7,groups= x$cyl,
         main="Konsumsi BBM berdasarkan merk mobil (berdasarkan jumlah silinder)",
         xlab="mil per galon", 
         gcolor="black", 
         color=x$color) 
