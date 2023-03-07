# question 3.1

sixdie <- function(n){
  
  roll_current <- sample(1:6,6, replace = TRUE)
  diff <- max(roll_current)-min(roll_current)
  
  if(diff < 3){
    pay = pay + 1
  }
  else{
    pay = pay + 0
  }
  return(pay)

}

run_exp <- sapply(1:10^5, sixdie)
mean(run_exp)

# expected pay: 0.058 = 6 cents

n <- 100
dollar <- c()
N <- c()
while (n < 10^5) {
  
  run_exp <- sapply(1:n, sixdie)
  p <- mean(run_exp)
  dollar <- append(dollar,p)
  N <- append(N,n)
  n <- n+100
}

plot(N,dollar, type = 'l')



# question 3.2
toss_seq <- c()
double_heads <- function(n) {
  toss_current <- sample(c("H","T"),1)
  toss_seq <- c(toss_seq,toss_current)
  condition <- FALSE
  prev <- toss_current
  while (!condition) {
    toss_current <- sample(c("H","T"),1)
    condition <- (toss_current == prev) & (toss_current == "H")
    toss_seq <- c(toss_seq,toss_current)
    prev <- toss_current
  }
  
  return(toss_seq)
}

lapply(1, double_heads)
toss_exp <- lapply(1:10^4, double_heads)
m <- mean(sapply(toss_exp,length))

k = 1/m
k

N <- 1:99
dollark <- c()

for (i in 2:100) {
  
  toss_exp = lapply(1:i,double_heads)
  m = mean(sapply(toss_exp,length))
  k = 1/m
  dollark = append(dollark,k)
  
   
}

plot(N,dollark, type = 'l')


# Question 3.4

x <- runif(100,0,1)






























