`kfcv` <-
function(k,N){
if(k>N){
  warning("'k' is larger than 'N'!")
  return(rep(1,N)) 
} else {
if(N%%k==0){
  rep(N%/%k,k) 
} else {
  c(N%%k+N%/%k,rep(N%/%k,k-1)) 
} 

} 
}

