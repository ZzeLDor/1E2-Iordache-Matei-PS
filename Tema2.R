B1 = function(v)  
{
  torvol = function(N) 
  {
    N_C = 0
    for(i in 1:N)
    {
      x1 = runif(1, - 3 - 10, 3 + 10)
      x2 = runif(1, - 3 - 10, 3 + 10)
      x3 = runif(1, -3, 3)
      if(x3 ^ 2 + (sqrt(x1 ^ 2 + x2 ^ 2) - 10) ^ 2 < 3 ^ 2)
        N_C = N_C + 1
    }
    return(N_C * ((2*(3 + 10)) ^ 2 * (2 * 3)) / N)
  }
  
  isample = function(v)
  {
    vex = 2 * pi ^ 2 * 10 * 3 ^ 2
    cat("Volum exact:", vex, "\n\n")
    for(i in v)
    {
      vest = torvol(i)
      cat("Volum estimat (",i, "rulaje):", vest, "\n")
      cat("Eroare relativa (",i, "rulaje):", abs((vest - vex) / vex), "\n\n")
    }
  }
  isample(v)
}

B2 = function(v)
{
  triaria = function(N) 
  {
    a = 0
    b = 2
    c = 0
    d = 2.4
    
    N_C = 0
    for(i in 1:N)
    {
      x = runif(1, 0, 2)
      y = runif(1, 0, 2.4)
      if(y >= 0 && y <= 2 * x && y <= 6 - 3 * x)
        N_C = N_C + 1
    }
    
    return(N_C * 2 * 2.4 / N)
  }
  isample = function(v)
  {
    aex = 2.4
    cat("Aria exacta:", aex, "\n")
    for(i in v)
    {
      aest = triaria(N)
      cat("Aria estimata (cu", N, "puncte):", aest, "\n")
      cat("Eroare relativă:", abs((aest - aex) / aex), "\n\n")
    }
  }
  isample(v)
}

B3a = function(N)
{
  inta = function(N) {
    sum = 0
    for (i in 1:N) {
      x = runif(1, -1, 1)
      sum = sum + (2 * x - 1) / (x^2 - x - 6)
    }
    return(sum * (2 / N))
  }
  
  isample = function(N)
  {
    iex = log(3) - log(2)
    cat("Valoarea exacta pentru subpunctul a:", iex, "\n")
    iest = inta(N)
    cat("Valoarea estimata pentru subpunctul a:", iest, "\n")
    cat("Eroare relativa pentru subpunctul a:", abs((iest-iex)/iex))
  }
  isample(N)
}

B3b = function(N)
{
  intb = function(N) {
    sum = 0
    for (i in 1:N) {
      x = runif(1, 3, 11)
      sum = sum + (x + 4) / exp(1/3 * log(x - 3))
    }
    return(sum * (8 / N))
  }
  
  isample = function(N)
  {
    iex = 61.2
    cat("Valoarea exacta pentru subpunctul b:", iex, "\n")
    iest = intb(N)
    cat("Valoarea estimata pentru subpunctul b:", iest, "\n")
    cat("Eroare relativa pentru subpunctul b:", abs((iest-iex)/iex))
  }
  isample(N)
}

B3c = function(N)
{
  intc = function(N) {
    sum = 0
    for (i in 1:N) {
      x = rexp(1,1)
      sum = sum + x * exp(-x^2) / exp(-x)
    }
    return(sum / N)
  }
  
  isample = function(N)
  {
    iex = 1/2
    cat("Valoarea exacta pentru subpunctul c:", iex, "\n")
    iest = intc(N)
    cat("Valoarea estimata pentru subpunctul c:", iest, "\n")
    cat("Eroare relativa pentru subpunctul c:", abs((iest-iex)/iex))
  }
  isample(N)
}

B4a = function(n, p, q)
{
  estyears = function(n, p, q) 
  {
    nru = 10000
    nrfin = 15000
    years = 0
    while (nru < nrfin) 
    {
      newnru = rbinom(1, n, p)
      elimnru = rbinom(1, nru, q)
      nru = nru + newnru - elimnru
      years = years + 1
    }
    return(years)
  }
  
  MC_estyears = function(n, p, q)
  {
    s = vector()
    for(i in 1:1000)
      s[i] = estyears(n, p, q)
    cat("Nr de ani estimati:",mean(s),"\n")
  }
  MC_estyears(n, p, q)
}

B4b = function(n, p, q)
{
  estchance = function(n, p, q)
  {
    lunitot = 40 * 12 + 10
    nru = 10000
    nrfin = 15000
    for(i in 1:lunitot)
    {
      newnru = rbinom(1, n, p/12)
      elimnru = rbinom(1, nru, q/12)
      nru = nru + newnru - elimnru
    }
    if(nru >= nrfin)
      return (1)
    return (0)
  }
  
  MC_estchance = function(n, p, q)
  {
    s = vector()
    for(i in 1:1000)
      s[i] = estchance(n, p, q)
    cat("Probabilitate:",mean(s), "\n")
  }
  MC_estchance(n, p, q)
}

B4c = function(n, p, q)
{
  estchance = function(n, p, q)
  {
    lunitot = 100 * 12 + 10
    nru = 10000
    nrfin = 15000
    for(i in 1:lunitot)
    {
      newnru = rbinom(1, n, p/12)
      elimnru = rbinom(1, nru, q/12)
      nru = nru + newnru - elimnru
    }
    if(nru >= nrfin)
      return (1)
    return (0)
  }
  
  MC_estchance = function(n, p, q)
  {
    s = vector()
    for(i in 1:1000)
      s[i] = estchance(n, p, q)
    cat("Probabilitate:",mean(s), "\n")
  }
  MC_estchance(n, p, q)
}
cat("\n\n ########## \n\n\n")
B1(c(10000, 20000, 50000))
cat("\n\n ########## \n\n\n")
B2(c(20000, 50000))
cat("\n\n ########## \n\n\n")
B3a(10000)
cat("\n\n ########## \n\n\n")
B3b(10000)
cat("\n\n ########## \n\n\n")
B3c(10000)
cat("\n\n ########## \n\n\n")
B4a(1000, 0.25, 0.01)
cat("\n\n ########## \n\n\n")
B4b(1000, 0.25, 0.01)
cat("\n\n ########## \n\n\n")
B4c(1000, 0.25, 0.01)
cat("\n\n ########## \n\n\n")
