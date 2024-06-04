A1a = function(l, p, n, m, k)
{
  p_pois = dpois(k:m, l)
  p_geom = dgeom(k:m, p)
  p_binom = dbinom(k:m, n, p)
  list(poisson = p_pois, geometric = p_geom, binomial = p_binom)
}

A1b = function(l, p, n, m, k)
{
  p = A1a(l, p, n, m, k)
  x = k:m
  
  par(mfcol = c(1,1))
  
  plot(x, p$binomial, type = 'b', ylim = range(p$binomial, p$poisson, p$geometric), ylab = "Probabilitate", main = "Poisson = Verde / Geometric = Albastru / Binomial = Rosu", col = "red")
  lines(x, p$poisson, type = 'b', col = "green")
  lines(x, p$geometric, type = 'b', col = "blue")
}

A1c = function(l)
{
  k0 = 0
  pc_pois = ppois(k0, l)
  
  while(pc_pois < 1 - 10^(-6))
  {
    k0 = k0 + 1
    pc_pois = ppois(k0, l)
  }
  
  return (k0)
}

A2a = function(path)
{
  csv = read.csv(path, header = TRUE)
  colP = csv$P
  colS = csv$S
  faP = table(colP)
  faS = table(colS)
  frP = as.vector(faP) / length(colP)
  frS = as.vector(faS) / length(colS)
  mP = mean(colP)
  mS = mean(colS)
  list(frecventaAbsolutaP = faP, frecventaRelativaP = frP, medieP = mP, frecventaAbsolutaS = faS, frecventaRelativaS = frS, medieS = mS)
}

A2b = function(path, e)
{
  csv = read.csv(path, header = TRUE)
  if(e == "P")
    col = csv$P
  else
    col = csv$S
  
  Q1 = quantile(col, 0.25)
  Q3 = quantile(col, 0.75)
  IQR = Q3 - Q1
  
  col = col[col >= Q1 - 1.5 * IQR & col <= Q3 +1.5 * IQR]
  
  hist(col, labels = TRUE,breaks = seq(1, 10), xlim = c(min(col)-1,max(col)), xlab = "Nota")
  return (col)
}
cat("\n\n ########## \n\n\n")
A1a(2, 0.5, 10, 5, 2)
cat("\n\n ########## \n\n\n")
A1b(2, 0.5, 10, 5, 2)
cat("\n\n ########## \n\n\n")
A1c(2)
cat("\n\n ########## \n\n\n")
A2a("note_PS.csv")
cat("\n\n ########## \n\n\n")
A2b("note_PS.csv", "P")
cat("\n\n ########## \n\n\n")
