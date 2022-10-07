##Group member:
##1. Jiayi Tu(s2306508)
##2. Xiaoyu Zhu(s2296761)
##3. Jingyun Shan(s1943324)

##All steps were discussed among the group before writing the code. 
##Xiaoyu Zhu wrote the code for steps 4 and 5 and added comments. 
##Jingyun Shan wrote the code for steps 6 and 7 and added comments. 
##Jiayi Tu wrote the code for steps 8 and 9 and added comments. 
##At each step, two people other than the person responsible for 
##writing the code will test and check and suggest changes.

##github repo https://github.com/TJY51129/proj1.r.git

sessionInfo()
Sys.getlocale()
#Sys.setlocale("LC_ALL", "English")

##Input the bible the text data into R Studio.
setwd("C:/Users/Phyllis Shan/SP-proj1")
a <- scan("10-0.txt",what="character",skip=104,encoding="UTF-8") ## Read text data into a vector from the '10-0.txt' file £¬Ignore the first 104 lines content.
n <- length(a)
a <- a[-((n-2886):n)]                                             ## strip license
a <- a[-grep("[0123456789]:[0123456789]",a)]                      ## strip out verse numbers

#4
##Define the function to split the punctuation 
##make p represent the punctuation, t represent the text
split_punct <- function(t,punc){
  ip <- grep(punc,t,fixed = TRUE)        ## indices of punctuation punc in text t
  rp <-gsub(punc,"",t,fixed = TRUE)      ## remove the punctuation punc
  nv <- rep("",length(rp)+length(ip))    ## Build a vector nv to store text and punctuation   
  ipnv <- ip+1:length(ip)                ## indices of  punctuations in nv
  nv[ipnv] <- punc                       ## insert the punctuation punc
  nv[-ipnv] <- rp                        ## insert rp in unoccupied place nv
  t <-nv                                 ## update text t
  return(t)
}

#5
##Split all punctuation(',','.',';','!','?',':') from vector a.
p <- c(',','.',';','!',':','?')      ## Assign  punctuation vector
for (punc in p) a <- split_punct(a,punc)   ## Split all punctuation 


#6
##generate word vector of the most common word
lower_a<-tolower(a)                       ##replaced the capital letters in words with lower case letters
b<-unique(lower_a)                        ##vector containing every unique words in vector a. 
freq<-tabulate(match(lower_a,b))          ##count up how many time each unique word occurs in vector a.
freq_sort <- sort(freq,decreasing=TRUE)   ##Sort unique word frequency from the unique words.
m<-500                                    ## set of m = 500 most common words
threshold <- freq_sort[m]                 ##Set the threshold number of word occurs in vector a.
b<-b[which(freq>=threshold)]              ##Create vector of the most common word.

#7 
##create a three column matrix 
d <- match(lower_a,b)   ##generate word vector  index of the most common words in vector a.
matrixT <- cbind(d[1:(length(d)-2)],d[2:(length(d)-1)],d[3:length(d)])  ##first column is the index of common words, and the next two columns is the index for the following word.
matrix <-  subset(matrixT , is.na(rowSums(matrixT)) == 0)  ##drop the other word triplets (those that contain an NA).
##Initialize matrix T.
T <- array(0,c(m,m,m))      ##construct an m*m*m array T and assign the initial value as 0.
for (y in 1:nrow(matrix)){  ##array of frequencies
  i <- matrix[y,1]          ##loop through the common word triplets ,look i,k,j
  k <- matrix[y,2]
  j <- matrix[y,3]          ##according the conditional probability
  T[i,k,j] <- T[i,k,j]+1    ##adding a 1 to T[i,k,j] every time the jth common word follows the pair i,k
}


##compute Tikj  probability

for (i in 1:m) {          ##i loop       
  for (k in 1:m) {        ##k loop
    sumik <- sum(T[i,k,])     ##limit i,k, control variable, sum the first and second columns
    T[i,k,] <- T[i,k,]/sumik ##find j probability that the last column matches  i,k
  }
}

#create a two column matrix A
matrixA <- cbind(d[1:(length(d)-1)],d[2:(length(d))])      ##first column is the index of common words, and the next two columns is the index for the following word
matrix <-  subset(matrixA , is.na(rowSums(matrixA)) == 0)  ##drop the other word pairs (those that contain an NA)

##Initialize matrix A.
A <- array(0,c(m,m))            ####construct an m*m matrix A and assign the initial value as 0.
for (y in 1:nrow(matrix)){  ##array of frequencies 
  i <- matrix[y,1]          ##loop through the common word pairs ,look i,j
  j <- matrix[y,2]
  A[i,j] <- A[i,j]+1        ##adding a 1 to A[i,j] every time the jth common word follows the word i
}


##compute Aij  probability
for (i in 1:m) {        ##array of probability
    aa <- sum(A[i,])    ##sum the first column
    A[i,] <- A[i,]/aa   ##find the probability that the second column matches the first column
}

#vector S
vectorS <- d  ##construct an m vector S and assign the initial value as d.
vectorS <-  subset(vectorS , is.na(vectorS) == 0)  ##drop the other word triplets (those that contain an NA).

##Initialize vector S.
S <- rep(0,m)      ##construct an m vector S and assign the initial value as 0.
for (y in 1:length(vectorS)){  ##vector of frequencies, loop through the common word, look i
  i <- matrix[y]
  S[i] <- S[i]+1    ##adding a 1 to S[i] every time
}

##compute Si  probability
S <- S/sum(S)


#8 
num <- 50          ##set sample text number
index<-rep(0,num)  ##assign the 50 words as 0
index[1] <- sample(1:m, size = 1, prob = S)  ##sample the first word from 500 common words, according to the probability of SA
index[2] <- sample(1:m, size = 1, prob = A[index[1],]) ##sample the second word from 500 common words, according to the probability of A
for (i in 3:num) {  ##sample the third to the 50th words from 500 common words
  if(sum(T[index[i-2], index[i-1], ]) != 0){  ##When we confirm the first and the second dimension, we can have a look at the third one.
    index[i] <- sample(1:m,1, prob = T[index[i-2], index[i-1], ])    ##when the probability of the sum of T is zero, have a look at its probability
  } else{
    index[i] <- sample(1:m, 1, prob = A[index[i-1],])    ##when the first condition do not meet, then use the second one
  }                                                                   ##since all probabilities in A are not equal to 0, 
}                                                                     ##this means that we do not need to consider the case of S
cat(b[index])  ##print out the corresponding text with cat 

#9
##simply simulate 50
simply50 <- b[sample(1:m, 50, replace = TRUE, prob = S)] ## simulate 50 word sections of text where the word probabilities are simply taken from S
cat(simply50)   ##print simply simulate 50

#10
##start with a capital letter in my simulation.
capitalb <- b[index]  ##assign simulation 50 words to capitalb  vector.
amb=match(lower_a,b)  ##assign lower_a match b result to  amb  vector.
mostcapitalp <- 0.2   ##assign Common Proportional Values to.
for (i in 1:50){      ##output 50 words loop.
  FirstLettercapitalb <- paste0(toupper(substr(capitalb[i],1,1)),substr(capitalb[i],2,nchar(capitalb[i])))  ##First Letter to upper.
  somewordset <- a[which(amb == index[i])]    ## Assign the set containing the initial case of a word in vector a to the vector somewordset.
  
  if (length(grep(FirstLettercapitalb,somewordset))/length(somewordset) > mostcapitalp) { capitalb[i] <- FirstLettercapitalb ##if first Letter  capital proportion then if first Letter  capital
  }
}
cat(capitalb) ##print result
