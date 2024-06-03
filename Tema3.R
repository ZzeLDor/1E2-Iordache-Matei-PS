C1a = function(n)
{
  val = runif(n)
  sval = order(val)
  return (sval)
}



C1b = function(a, b)
{
  len = min(length(a), length(b))
  
  for(i in 1:len)
  {
    if(a[i] < b[i])
      return (TRUE)
    else if(a[i] > b[i])
      return (FALSE)
  }
  
  while(length(a) < length(b))
  {
    a = c(a, sample(0:1, 1))
    if(a[length(a)] < b[length(a)])
      return (TRUE)
    else if(a[length(a)] > b[length(a)])
      return (FALSE)
  }
  while(length(a) > length(b))
  {
    b = c(b, sample(0:1, 1))
    if(a[length(b)] < b[length(b)])
      return (TRUE)
    else if(a[length(b)] > b[length(b)])
      return (FALSE)
  }
  while(1)
  {
    a = c(a, sample(0:1, 1))
    b = c(b, sample(0:1, 1))
    if(a[length(a)] < b[length(a)])
      return (TRUE)
    else if(a[length(a)] > b[length(a)])
      return (FALSE)
  }
}

C1c = function(v)
{
    if(length(v)< 2)
      return (v)
    
    lt = list()
    mt = list()
    x = sample(1:length(v), 1)
    pivx = v[[x]]
    v = v[-x]
    
    for(i in 1:length(v))
    {
      if(C1b(v[[i]], pivx))
        lt = c(lt, v[i])
      else
        mt = c(mt, v[i])
    }
    return (c(C1c(lt),list(pivx),C1c(mt)))
}

C1d = function(n, k)
{
  v = lapply(1:n, function(x) rbinom(k, 1, 0.5))
  vv = C1c(v)
  p = vector("list", n)
  for(i in 1:n)
    for(j in 1:n)
      if(identical(vv[[i]], v[[j]]) && !(j %in% p))
        p[i] = j;
  return (p)
}

C2 = function(n,m)
{
  v = 1:(2*n + 1)
  comb = combn(v,2)
  muchii = sample(ncol(comb), m)
  a = sample(v, n)
  b = setdiff(v, a)
  s = sum(comb[1,muchii] %in% a & comb[2,muchii] %in% b)
  
  return (s)
  
}

cat("\n\n ########## \n\n\n")
C1a(10)
cat("\n\n ########## \n\n\n")
C1b(c(1,0,0,1,0,1), c(1,0,1,0,0,1))
cat("\n\n ########## \n\n\n")
C1c(list(c(0,1,1,0,0,1),c(0),c(0,0,1,0,1),c(1,0,1),c(1)))
cat("\n\n ########## \n\n\n")
C1d(7, 5)
cat("\n\n ########## \n\n\n")
C2(3,8)
cat("\n\n ########## \n\n\n")
