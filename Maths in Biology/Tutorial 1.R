#3
r <- function(a,N,K){
  r <- (a * N) / (K + N)
  return(r)
}

N = seq(1,100)

first_scene <- r(5,N,1)
second_scene <- r(6,N,1)
third_scene <- r(8,N,1)

plot(first_scene~N)
lines(second_scene~N)
lines(third_scene~N, col="red")

#4
f <- function(n,b,x){
  f <- (x ^ n) / ((b ^ n) + (x ^ n))
  return(f)
}

x = seq(1:15)

first <- f(1,2,x)
second <- f(2,2,x)
third <- f(3,2,x)

plot(first~x, type='l', ylim=c(0,1))
lines(second~x, col="blue")
lines(third~x, col = "red")

#5
leaf_area <- function(stem_diameter){
  la <- sd ^ 1.84
  return(la)
}

sd <-seq(1:50)
result <- leaf_area(sd)

plot(result~sd, type="l")

volume_fraction <- function(leaf_thickness){
  vf <- lt ^ -0.49
  return(vf)
}

lt <- seq(0:100)

spongy_mesophyll <- volume_fraction(lt)

plot(spongy_mesophyll~lt, type="l")

#7
intrinsic <- function(N0,r,t){
  N_t <- N0 * exp(r * t)
  return(N_t)
}

t <- seq(0:100)

first <- intrinsic(100,2,t)
second <- intrinsic(100,3,t)

plot(first~t,type='l')
lines(second~t,col="blue")

#8
v_b <- function(x,k,L_inf){
  L <- L_inf * (1 - exp(-k * x))
  return(L)
}

x <- seq(1:50)

first <- v_b(x,1,20)
second <- v_b(x,0.1,20)

plot(first ~ x, type='l')
lines(second ~ x, col='red')








