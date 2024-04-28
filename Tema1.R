outliers_mean = function(x)
{
  m = mean(x)
  s = sd(x)
  outliers = vector()
  j = 0
  for(i in 1:length(x))
    if(x[i] < m - 2*s | x[i] > m + 2*s) 
      {
      j = j + 1
      outliers[j] = x[i]
      }
  outliers
}

outliers_iqr = function(x)
{
  q = quantile(x, probs = c(0.25, 0.75))
  q1 = q[1]
  q3 = q[2]
  iqr = q3 - q1
  lo = q1 - 1.5 * iqr
  up = q3 + 1.5 * iqr
  
  outliers = vector()
  j = 0
  for(i in 1:length(x))
    if(x[i] < lo | x[i] > up)
    {
      j = j + 1
      outliers[j] = x[i]
    }
  outliers
}