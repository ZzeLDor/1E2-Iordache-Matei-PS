D1 = function(sigma)
{
  incr95 = function(sigma, m, len)
  {
    alpha = 0.05
    critical_z = qnorm(1 - alpha / 2, 0,1)
    a = m - critical_z * sqrt(sigma / len)
    b = m + critical_z * sqrt(sigma / len)
    return (c(a,b))
  }
  
  incr99 = function(sigma, m, len)
  {
    alpha = 0.01
    critical_z = qnorm(1 - alpha / 2, 0,1)
    a = m - critical_z * sqrt(sigma / len)
    b = m + critical_z * sqrt(sigma / len)
    return (c(a,b))
  }
  
  csv = read.csv("probabilitati.csv")
  m = mean(csv$probabilitati)
  len = length(csv$probabilitati)
  
  print(incr95(sigma, m, len))
  print(incr99(sigma, m, len))
}

D2 = function()
{
  incr95 = function(sigma, m, len)
  {
    alpha = 0.05
    critical_z = qnorm(1 - alpha / 2, 0,1)
    a = m - critical_z * sqrt(sigma / len)
    b = m + critical_z * sqrt(sigma / len)
    return (c(a,b))
  }
  
  incr99 = function(sigma, m, len)
  {
    alpha = 0.01
    critical_z = qnorm(1 - alpha / 2, 0,1)
    a = m - critical_z * sigma / sqrt(len)
    b = m + critical_z * sigma / sqrt(len)
    return (c(a,b))
  }
  
  csv = read.csv("statistica.csv")
  m = mean(csv$statistica)
  len = length(csv$statistica)
  sigma = sd(csv$statistica)
  
  print(incr95(sigma, m, len))
  print(incr99(sigma, m, len))
}

D3 = function()
{
  semnif1 = function()
  {
    alpha = 0.01
    critical_z = qnorm(1 - alpha, 0,1)
    return (critical_z)
  }
  
  semnif5 = function()
  {
    alpha = 0.05
    critical_z = qnorm(1 - alpha, 0,1)
    return(critical_z)
  }
  
  p0 = 0.15
  n = 100
  succese = 14
  p_prim = succese/n
  z_score = (p_prim - p0) / sqrt(p0 * (1 - p0) / n)
  
  cz1 = semnif1()
  cz5 = semnif5()
  
  if(cz1 < z_score)
    print("Ipoteza nula este respinsa la nivel de semnificatie de 1%")
  else
    print("Ipoteza nula nu poate fi respinsa la nivel de semnificatie de 1%")
  
  if(cz5 < z_score)
    print("Ipoteza nula este respinsa la nivel de semnificatie de 5%")
  else
    print("Ipoteza nula nu poate fi respinsa la nivel de semnificatie de 5%")
}
cat("\n\n ########## \n\n\n")
D1(92.16)
cat("\n\n ########## \n\n\n")
D2()
cat("\n\n ########## \n\n\n")
D3()
cat("\n\n ########## \n\n\n")
