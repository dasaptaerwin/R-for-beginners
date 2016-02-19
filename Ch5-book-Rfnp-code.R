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

## Bar Plots
# Bar Plot vertikal (default) sederhana
counts <- table(mtcars$gear)
barplot(counts, main="Car Distribution",
        xlab="Number of Gears")

# Bar plot horisontal sederhana
counts <- table(mtcars$gear)
barplot(counts, main="Car Distribution", horiz=TRUE,
        names.arg=c("3 Gears", "4 Gears", "5 Gears"))

# Bar plot bersusun dengan warna dan legenda 
counts <- table(mtcars$vs, mtcars$gear)
barplot(counts, main="Car Distribution by Gears and VS",
        xlab="Number of Gears", col=c("darkblue","red"),
        legend = rownames(counts))

# Bar plot dengan pengelompokkan 
counts <- table(mtcars$vs, mtcars$gear)
barplot(counts, main="Car Distribution by Gears and VS",
        xlab="Number of Gears", col=c("darkblue","red"),
        legend = rownames(counts), beside=TRUE)


## Line Charts

x <- c(1:5); y <- x # create some data
par(pch=22, col="red") # plotting symbol and color
par(mfrow=c(2,4)) # all plots on one page
opts = c("p","l","o","b","c","s","S","h")
for(i in 1:length(opts)){
  heading = paste("type=",opts[i])
  plot(x, y, type="n", main=heading)
  lines(x, y, type=opts[i])
}
dev.off()

# Simple Pie Chart
slices <- c(10, 12,4, 16, 8)
lbls <- c("US", "UK", "Australia", "Germany", "France")
pie(slices, labels = lbls, main="Pie Chart of Countries")

# Pie Chart with Percentages
slices <- c(10, 12, 4, 16, 8)
lbls <- c("US", "UK", "Australia", "Germany", "France")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices,labels = lbls, col=rainbow(length(lbls)),
main="Pie Chart of Countries")





## Boxplots

# Boxplot of MPG by Car Cylinders
boxplot(mpg~cyl,data=mtcars, main="Car Milage Data",
xlab="Number of Cylinders", ylab="Miles Per Gallon")


# Notched Boxplot of Tooth Growth Against 2 Crossed Factors
# boxes colored for ease of interpretation
boxplot(len~supp*dose, data=ToothGrowth, notch=TRUE,
col=(c("gold","darkgreen")),
main="Tooth Growth", xlab="Suppliment and Dose")



# Simple Scatterplot
attach(mtcars)
plot(wt, mpg, main="Scatterplot Example",
     xlab="Car Weight ", ylab="Miles Per Gallon ", pch=19)

# Add fit lines
abline(lm(mpg~wt), col="red") # regression line (y~x)
lines(lowess(wt,mpg), col="blue") # lowess line (x,y)

# Scatterplot matrix sederhana
pairs(~mpg+disp+drat+wt,data=mtcars,
        main="Simple Scatterplot Matrix")


# Scatterplot Matrices from the lattice Package
library(lattice)
splom(mtcars[c(1,3,5,6)]) # scatterplot matrix using variable no 1, 3, 5, 6


# Scatterplot Matrices from the glus Package
install.packages("gclus")
library(gclus)
dta <- mtcars[c(1,3,5,6)] # get data
dta.r <- abs(cor(dta)) # get correlations
dta.col <- dmat.color(dta.r) # get colors

# reorder variables so those with highest correlation
# are closest to the diagonal
dta.o <- order.single(dta.r)
cpairs(dta, dta.o, panel.colors=dta.col, gap=.5,
       main="Variables Ordered and Colored by Correlation" )


# High Density Scatterplot with Binning
install.packages("hexbin")
library(hexbin)
x <- rnorm(1000) # membuat angka acak
y <- rnorm(1000)
bin <- hexbin(x, y, xbins=50)
plot(bin, main="Hexagonal Binning")

# High Density Scatterplot with Color Transparency
x <- rnorm(1000)
y <- rnorm(1000)
plot(x,y, main="PDF Scatterplot Example", col=rgb(0, 100, 0, 50, 
                                                  maxColorValue=255), pch=16)
dev.off() # menghapus plot dari layar

# 3D Scatterplot menggunakan package scatterplot3d
install.packages("scatterplot3d")
library(scatterplot3d)
attach(mtcars)
scatterplot3d(wt,disp,mpg, main="3D Scatterplot")


# 3D Scatterplot with Coloring and Vertical Drop Lines
library(scatterplot3d)
attach(mtcars)
scatterplot3d(wt,disp,mpg, pch=16, highlight.3d=TRUE,
              type="h", main="3D Scatterplot")

# 3D Scatterplot with Coloring and Vertical Lines
# and Regression Plane
dev.off()
library(scatterplot3d)
attach(mtcars)
s3d <-scatterplot3d(wt,disp,mpg, pch=16, highlight.3d=TRUE,
                    type="h", main="3D Scatterplot")
fit <- lm(mpg ~ wt+disp)
s3d$plane3d(fit)

# Spinning 3d Scatterplot
install.packages("rgl")
library(rgl)
plot3d(wt, disp, mpg, col="red", size=3)


## Multiple plot dalam satu halaman

plot.new() # untuk membuat plot baru
par(mfrow= c(2,2)) # mengatur jumlah baris dan kolom
par(mar = c(3, 3, 2, 2)) # mengatur margin antar plot
plot(x, y, pch = 20, main="plot 1")
plot(x, z, pch = 19, main="plot 2")
plot(y, z, pch = 1, main="plot 3")
plot(y, z, pch = 5, main="plot 4")


## Plot dua grup data dalam satu grafik


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


# Membuat grafik dengan package `ggplot2` 


# Membangun sebuah plot 


library(ggplot2)
p <- ggplot(mpg, aes(displ, hwy))
p

library(ggplot2)
p <- ggplot(mpg, aes(displ, hwy))
p + geom_point()

p + layer(
  mapping = NULL, 
  data = NULL,
  geom = "point", geom_params = list(),
  stat = "identity", stat_params = list(),
  position = "identity"
)


# Data

mod <- loess(hwy ~ displ, data = mpg)
grid <- data.frame(displ = seq(min(mpg$displ), max(mpg$displ), length = 50))
grid$hwy <- predict(mod, newdata = grid)
head(grid)

std_resid <- resid(mod) / mod$s
outlier <- subset(mpg, abs(std_resid) > 2)

ggplot(mpg, aes(displ, hwy)) + 
  geom_point() + 
  geom_line(data = grid, colour = "blue", size = 1.5) + 
  geom_text(data = outlier, aes(label = model))

ggplot(mapping = aes(displ, hwy)) + 
  geom_point(data = mpg) + 
  geom_line(data = grid) + 
  geom_text(data = outlier, aes(label = model))



## Membuat spesifikasi estetik pada plot vs pada layer


ggplot(mpg, aes(displ, hwy, colour = class)) + 
  geom_point()
ggplot(mpg, aes(displ, hwy)) + 
  geom_point(aes(colour = class))
ggplot(mpg, aes(displ)) + 
  geom_point(aes(y = hwy, colour = class))
ggplot(mpg) + 
  geom_point(aes(displ, hwy, colour = class))

ggplot(mpg, aes(displ, hwy, colour = class)) + 
  geom_point() + 
  geom_smooth(se = FALSE)

ggplot(mpg, aes(displ, hwy)) + 
  geom_point(aes(colour = class)) + 
  geom_smooth(se = FALSE)


# Setting vs mapping

ggplot(mpg, aes(cty, hwy)) + 
  geom_point(colour = "darkblue") 

ggplot(mpg, aes(cty, hwy)) + 
  geom_point(aes(colour = "darkblue"))

ggplot(mpg, aes(cty, hwy)) + 
  geom_point(aes(colour = "darkblue")) + 
  scale_colour_identity()

ggplot(mpg, aes(displ, hwy)) + 
  geom_point() +
  geom_smooth(aes(colour = "loess"), method = "loess", se = FALSE) + 
  geom_smooth(aes(colour = "lm"), method = "lm", se = FALSE)



# Fungsi statistik dalam `ggplot2` 

## fungsi `stat_summary()`

ggplot(mpg, aes(trans, cty)) + 
  geom_point() + 
  stat_summary(geom = "point", fun.y = "mean", colour = "red", size = 4)

ggplot(mpg, aes(trans, cty)) + 
  geom_point() + 
  geom_point(stat = "summary", fun.y = "mean", colour = "red", size = 4)

## Variabel yang dibuat oleh fungsi (_Generated variables_)

ggplot(diamonds, aes(price)) + 
  geom_histogram(aes(y = ..density..), binwidth = 500)

ggplot(diamonds, aes(price, colour = cut)) + 
  geom_freqpoly(binwidth = 500)
ggplot(diamonds, aes(price, colour = cut)) + 
  geom_freqpoly(aes(y = ..density..), binwidth = 500)



# Pengaturan posisi (_position adjustment_)

## opsi `fill`
dplot <- ggplot(diamonds, aes(clarity, fill = cut)) + 
  theme(legend.position = "none")
dplot + geom_bar()
dplot + geom_bar(position = "fill")
dplot + geom_bar(position = "dodge")

  
# opsi `position_identity`
dplot + geom_bar(position = "identity")
ggplot(diamonds, aes(clarity, colour = cut)) + 
  geom_freqpoly(aes(group = cut)) + 
  theme(legend.position = "none")


## Jittering
ggplot(mpg, aes(displ, hwy)) + 
  geom_point(position = "jitter")
ggplot(mpg, aes(displ, hwy)) + 
  geom_point(position = position_jitter(width = 0.02, height = 0.2))



