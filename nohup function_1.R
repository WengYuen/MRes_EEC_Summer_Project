#1. Working directory
setwd("C:/Users/User/Desktop/Winter Project/R/111_05-04_2/6_112")

#2. Packages
library(tuneR)
library(seewave)
library(soundecology)

#3. Calculate acoustic indices
files <- dir(pattern = "WAV$")
indices <- function(x){
  x <- readWave(x)
  return(c(H(x),
           acoustic_evenness(x)$aei_left,
           acoustic_diversity(x)$adi_left,
           ACI(x),
           NDSI(soundscapespec(x, plot=FALSE))
  )
  )
}
n <- length(files)
num <- rep(NA, n)
res <- data.frame(H=num, AEI=num, ADI=num, ACI=num, NDSI=num,
                  row.names=files)
for(i in 1:n) res[i,] <- indices(files[i])

#4. Write the output as a csv file
write.csv(res,"Out_6112.csv")
