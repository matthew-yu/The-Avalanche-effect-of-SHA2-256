library(openssl)
library(gtools)
library(BMS)
library(bitops)
#library(e1071)
library(ggplot2)

#Convert integer to binary
toBin <- function(x){
  bits=floor(log(x)/log(2))
  powers = 2^(bits:0)
  return(bitwAnd(x,powers)/powers)
}

#Set input text to binary for tweaking
# flipbit <- function(bin_in){
#   if(bin_in==1){
#     bin_out <- 0
#   } else {bin_out=1}
#   return(bin_out)
# }

#Extract last n characters from string
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

#Operate flipping one bit of arbitrary input at a time
#Displays the first nres results
Avalanche_demo <- function(input, nres=6){
  #input_bin <- as.numeric(unlist(as.binary(asc(input))))
  input_bin <- unlist(lapply(asc(input), toBin))
  N <- length(input_bin)
  flipped_data <- c()
  for(i in 1:N){
    flip_vec <- input_bin
    #flip_vec[N-i+1] <- flipbit(input_bin[N-i+1])
    flip_vec[N-i+1] <- bitwXor(input_bin[N-i+1],1)
    flipped_data[i] <- paste(flip_vec, collapse = "")
  }
  #calculate SHA-2 digest of flipped bit
  flipped_sha2 <- c()
  for(i in 1:N){
    flipped_sha2[i] <- sha256(flipped_data[i])
  }
  #Hex to bin for HD calculation
  HD <- c()
  original_sha2 <- sha256(paste(input_bin, collapse = ""))
  for(i in 1:N){
    HD[i] <- sum(hex2bin(original_sha2) != hex2bin(flipped_sha2[i]))/256
  }
  #Print results
  org_bin <- substrRight(paste(input_bin, collapse = ""),12)
  exhibit <- c(head(substrRight(flipped_data[1:nres],12), nres))
  cat("\n","original data = \n", input, "\n")
  cat("\n","original data in binary = \n ...", org_bin, "\n")
  cat("\n","flipped data in binary = \n")
  for(i in 1:nres){
    cat(" ...", exhibit[i],"\n")
  }
  cat("\n","original sha2 = \n",sha256(paste(input_bin, collapse = "")),"\n")
  cat("\n","flipped sha2 = \n")
  for(i in 1:nres){
    cat("",flipped_sha2[i],"\n")
  }
  cat("\n","mean of Hamming Distance = ",mean(HD),"\n")
  res_plot <- ggplot(as.data.frame(HD), aes(HD))+geom_bar(fill="steelblue")+scale_x_binned()
  res_plot <- res_plot+labs(title="Hamming Distances with message tweaked")
  res_plot <- res_plot+theme(plot.background=element_rect(colour="black", fill=NA, size=1))
  res_plot
}

Avalanche_demo("February 7th, 2022 ?V PUFsecurity, a subsidiary of eMemory, 
               and the leading provider of Physical Unclonable Function (PUF)-based security
               solutions, launched a new generation of its flagship product, PUFrt",12)

Avalanche_demo("(December 14th, 2023) PUFsecurity, a subsidiary of eMemory dedicated to
               innovating PUF-based security solutions, and Himax Technologies, Inc. 
               (Nasdaq: HIMX) (“Himax”), a leading supplier and fabless manufacturer 
               of display drivers and other semiconductor products, today announced 
               the successful deployment of PUFsecurity’s PUF-based Root of Trust IP, PUFrt,
               into Himax’s WiseEye2 AI processor (WE2). WE2 is an ultra-low-power AI chip 
               revolutionizing Endpoint AI application performance while prioritizing user 
               security and data protection.")
