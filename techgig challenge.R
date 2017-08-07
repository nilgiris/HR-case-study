# Enter your code here.
# Read input from STDIN. Write your output to STDOUT. 


#a <- c(4,2,3,4,6,10) #number of pipes
input <- c(4,9,8,9,4,6,2,3)
a1<- c(4,2,3,6)  #from original construction
a2 <- input[-which(a1 %in% input)]  #input from the new construction
#a <- c(4,2,3,6) 
b1 <- sort(c(a1,a2))
b2 <- sort(a1)

pipe_problem <- function(z){
  x <- 0
  c <- c()
  
  for (i in 1:length(z)){
    
    ifelse(i>1, x<-x+z[i], x<-z[i])
    c <- append(c,x)
    
  }
  return(c[-1])
}

 ifelse(length(a2) >1,catch1 <- pipe_problem(b1),catch1 <- pipe_problem(b2))
#catch1 <- catch1[-1]

write.table(catch1, sep = "", append=T, row.names = F, col.names = F)
total_sum <- sum(catch1)
total_sum
