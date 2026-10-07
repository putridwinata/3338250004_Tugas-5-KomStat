# SOAL 1 (Rata-rata waktu tunggu mu = 5 menit. Berapa peluang P(X > 5)?)

# Sebaran Eksponensial
peluang <- pexp(5, rate = 1/5, lower.tail = FALSE)
cat("Peluang P(X > 5):", peluang, "\n")

# Grafik Eksponensial dengan lambda = 0.2
x_dexp <- seq(1, 30, by = 1)
y_dexp <- dexp(x_dexp, rate = 0.2)
plot(x_dexp, y_dexp,
     type = "l",
     col = "purple",       
     lwd = 3,
     main = "PDF Distribusi Eksponensial (lambda = 0.2)",
     xlab = "Waktu Tunggu (menit)",
     ylab = "f(x)")


# Soal 2 (Kereta komuter tiba di stasiun secara acak antara pukul 07.00 hingga 07.20 
#(interval 20 menit). Berapakah ragam (varians) waktu tunggu penumpang?)


# Uniform pada interval [0,20]
set.seed(2025)
n <- 1000
a <- 0
b <- 20

var_teoritis <- (b - a)^2 / 12
cat("Varians waktu tunggu:",
    var_teoritis,
    "menit^2\n")

# Generate sampel
x <- runif(n, min = a, max = b)

# Nilai Density, CDF, dan Quantile
d_values <- dunif(c(0, 10, 20),
                  min = a,
                  max = b)
p_values <- punif(c(0, 10, 20),
                  min = a,
                  max = b)
q_values <- qunif(c(0.25, 0.5, 0.75),
                  min = a,
                  max = b)


# Histogram Sampel
hist(x,
     breaks = 30,
     probability = TRUE,
     main = "Histogram Sampel U(0,20) dengan PDF Teoritis",
     xlab = "Waktu Tunggu (menit)",
     col = "deeppink",        
     border = "white")

# PDF teoritis
curve(dunif(x, min = a, max = b),
      from = a,
      to = b,
      add = TRUE,
      col = "pink",       
      lwd = 3)


# SOAL 3 (Masa pakai sensor suhu memiliki rata-rata mu = 10 tahun. 
#Berapa peluang sensor tersebut rusak sebelum mencapai usia 5 tahun?)

# Sebaran Eksponensial
peluang <- pexp(5, rate = 1/10)
cat("Peluang P(X < 5):", peluang, "\n")


# Grafik Eksponensial dengan lambda = 0.1
x_dexp <- seq(1, 30, by = 1)
y_dexp <- dexp(x_dexp, rate = 0.1)

plot(x_dexp,
     y_dexp,
     type = "l",
     col = "blue",       
     lwd = 3,
     main = "PDF Distribusi Eksponensial (lambda = 0.1)",
     xlab = "Masa Pakai (tahun)",
     ylab = "f(x)")


# SOAL 4 (4. Berat bersih kemasan kopi menyebar normal dengan mu = 250 gram dan sigma = 5 gram. 
#Kemasan dianggap underweight jika beratnya kurang dari 240 gram. 
#Berapa proporsi produk yang tergolong underweight?)

# Distribusi Normal (Gaussian)
set.seed(2025)
n <- 100
mu <- 250
sigma <- 5


# Proporsi produk underweight
p4 <- pnorm(240,
            mean = mu,
            sd = sigma)
cat("Proporsi underweight P(X < 240):",
    p4,
    "\n")

# Generate sampel
x <- rnorm(n,
           mean = mu,
           sd = sigma)


# Statistik sampel
(x_bar <- mean(x))

(mle_sigma2 <- mean((x - x_bar)^2))

(sd_sample <- sd(x))


# Histogram + PDF teoritis
hist(x,
     breaks = 30,
     probability = TRUE,
     main = "Histogram Sampel N(250, 5^2) dengan PDF Teoritis",
     xlab = "Berat Bersih Kopi (gram)",
     col = "pink",        
     border = "white")

# PDF teoritis
curve(dnorm(x,
            mean = mu,
            sd = sigma),
      from = mu - 4*sigma,
      to = mu + 4*sigma,
      add = TRUE,
      col = "purple",
      lwd = 3)

# Mean sampel
abline(v = x_bar,
       col = "deeppink",
       lwd = 2)

# Mean sebenarnya
abline(v = mu,
       col = "blue",
       lwd = 2,
       lty = 2)

legend("topright",
       legend = c("PDF teoritis",
                  "mean sampel",
                  "mean true"),
       lty = c(1, 1, 2),
       col = c("purple",
               "pink",
               "blue"),
       bty = "n")