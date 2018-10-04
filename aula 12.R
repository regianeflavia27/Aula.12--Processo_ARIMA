> library("urca")
> library(readxl)
> library(pwt8)
> data("pwt8.0")
> View(pwt8.0)
> br <- subset(pwt8.0, country=="Brazil", 
               +              select = c("rgdpna","emp","xr"))
> colnames(br) <-  c("PIB","Emprego","Câmbio")
> PIB <- br$PIB[45:62]
> EMPREGO <- br$Emprego[45:62]
> CAMBIO <- br$Câmbio[45:62]
> Anos <- seq(from=1994, to=2011, by=1)
> plot(EMPREGO, type = "l")
> emprego <- ts(EMPREGO, start = 1994, frequency = 1)
> plot(emprego, main="Pessoa Empregadas no Brasil",
       + ylab="Qte de Pessoas Empregadas-milhões",
       + xlab="Ano")
> acf(emprego)
> pacf(emprego)
> reglinEMP <- lm(EMPREGO ~ Anos)
> plot(EMPREGO)
> plot(CAMBIO)
> plot(Anos)
> plot(PIB,type="l",col="blue")
> plot(CAMBIO,type="l",col="blue")
> plot(Anos,type="l",col="blue")
> plot(EMPREGO, type = "l")
> emprego <- ts(EMPREGO, start = 1994, frequency = 1)
> plot(emprego, main="Pessoa Empregadas no Brasil",
       + xlab="Ano")
> acf(emprego)
> pacf(emprego)
> reglinEMP

Call:
  lm(formula = EMPREGO ~ Anos)

Coefficients:
  (Intercept)         Anos  
-3736.592        1.908  

> summary(reglinEMP)

Call:
  lm(formula = EMPREGO ~ Anos)

Residuals:
  Min      1Q  Median      3Q     Max 
-3.3808 -1.3820  0.0899  1.0826  4.6436 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept) -3.737e+03  1.958e+02  -19.08 1.97e-12 ***
  Anos         1.908e+00  9.778e-02   19.51 1.40e-12 ***
  ---
  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 2.152 on 16 degrees of freedom
Multiple R-squared:  0.9597,	Adjusted R-squared:  0.9572 
F-statistic: 380.7 on 1 and 16 DF,  p-value: 1.399e-12

> plot(emprego)
> abline(reglinEMP, col="Blue")
> residuosEMP <- reglinEMP$residuals
> reglinEMPres <- lm(residuosEMP ~ Anos)
> plot(residuosEMP,type="l")
> abline(reglinEMPres, col="Blue")
> pdemprego <- diff(EMPREGO)
> diferenca1 <- (data.frame(EMPREGO[2:18],pdemprego))
> DIFERENCA <- ts(diferenca1, start = 1994, frequency = 1)
> plot(DIFERENCA, plot.type="single", col=c("Black","Green"))
> plot(pdemprego, type="l")
> pdemprego1 <- diff(emprego)
> TesteDF_Emprego1_trend <- ur.df(pdemprego1, "trend", lags = 1)
> summary(TesteDF_Emprego1_trend)

############################################### 
# Augmented Dickey-Fuller Test Unit Root Test # 
############################################### 

Test regression trend 


Call:
  lm(formula = z.diff ~ z.lag.1 + 1 + tt + z.diff.lag)

Residuals:
  Min      1Q  Median      3Q     Max 
-1.5029 -0.8663 -0.0643  0.4765  2.5857 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)  
(Intercept)  1.32512    0.79632   1.664   0.1243  
z.lag.1     -0.82873    0.38961  -2.127   0.0569 .
tt           0.04494    0.09784   0.459   0.6549  
z.diff.lag  -0.30943    0.24691  -1.253   0.2361  
---
  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 1.318 on 11 degrees of freedom
Multiple R-squared:  0.7223,	Adjusted R-squared:  0.6466 
F-statistic: 9.538 on 3 and 11 DF,  p-value: 0.002147


Value of test-statistic is: -2.1271 2.1918 2.7568 

Critical values for test statistics: 
  1pct  5pct 10pct
tau3 -4.38 -3.60 -3.24
phi2  8.21  5.68  4.67
phi3 10.61  7.24  5.91

> pdemprego2 <- diff(diff(emprego))
> TesteDF_Emprego2_trend <- ur.df(pdemprego2, "trend", lags = 1)
> summary(TesteDF_Emprego2_trend)

############################################### 
# Augmented Dickey-Fuller Test Unit Root Test # 
############################################### 

Test regression trend 


Call:
  lm(formula = z.diff ~ z.lag.1 + 1 + tt + z.diff.lag)

Residuals:
  Min     1Q Median     3Q    Max 
-1.963 -1.103  0.172  1.003  2.498 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)   
(Intercept)   1.5612     1.0479   1.490  0.16711   
z.lag.1      -2.2447     0.5709  -3.932  0.00281 **
  tt           -0.1320     0.1090  -1.211  0.25391   
z.diff.lag    0.2699     0.2993   0.902  0.38837   
---
  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 1.559 on 10 degrees of freedom
Multiple R-squared:  0.8941,	Adjusted R-squared:  0.8624 
F-statistic: 28.15 on 3 and 10 DF,  p-value: 3.438e-05


Value of test-statistic is: -3.9318 5.1638 7.7303 

Critical values for test statistics: 
  1pct  5pct 10pct
tau3 -4.38 -3.60 -3.24
phi2  8.21  5.68  4.67
phi3 10.61  7.24  5.91

> arima123 <- arima(emprego, c(1,2,3))
> arima120 <- arima(emprego, c(1,2,0))
> arima121 <- arima(emprego, c(1,2,1))
> arima122 <- arima(emprego, c(1,2,2))
> arima120 <- arima(emprego, c(1,2,0))
> arima121 <- arima(emprego, c(1,2,1))
> arima122 <- arima(emprego, c(1,2,2))
> arima123 <- arima(emprego, c(1,2,3))
> arima120 <- arima(emprego, c(1,2,0))
> arima121 <- arima(emprego, c(1,2,1))
> arima122 <- arima(emprego, c(1,2,2))
> 
  > arima220 <- arima(emprego, c(2,2,0)
                      + arima221 <- arima(emprego, c(2,2,1)
                                          Error: unexpected symbol in:
                                            "arima220 <- arima(emprego, c(2,2,0)
                                          arima221"
                                          > arima222 <- arima(emprego, c(2,2,2)
                                                              + arima223 <- arima(emprego, c(2,2,3)
                                                                                  Error: unexpected symbol in:
                                                                                    "arima222 <- arima(emprego, c(2,2,2)
                                                                                  arima223"
                                                                                  > arima221 <- arima(emprego, c(2,2,1)
                                                                                                      + arima222 <- arima(emprego, c(2,2,2)
                                                                                                                          Error: unexpected symbol in:
                                                                                                                            "arima221 <- arima(emprego, c(2,2,1)
                                                                                                                          arima222"
                                                                                                                          > arima222 <- arima(emprego, c(2,2,2))
                                                                                                                          > arima223 <- arima(emprego, c(2,2,3))
                                                                                                                          > arima021 <- arima(emprego, c(0,2,1))
                                                                                                                          > arima022 <- arima(emprego, c(0,2,2))
                                                                                                                          > arima023 <- arima(emprego, c(0,2,3))
                                                                                                                          > arima0120 <- arima(emprego, c(1,2,0))
                                                                                                                          > estimacoes <- list(arima123,arima120,arima121,
                                                                                                                                               + arima122,arima220,rima221,
                                                                                                                                               + arima222,arima223,arima021,arima021, arima022,
                                                                                                                                               + arima023,arima0120)
                                                                                                                          Error: object 'arima220' not found
                                                                                                                          > arima120 <- arima(emprego, c(1,2,0))
                                                                                                                          > arima121 <- arima(emprego, c(1,2,1))
                                                                                                                          > arima122 <- arima(emprego, c(1,2,2))
                                                                                                                          > 
                                                                                                                            > arima220 <- arima(emprego, c(2,2,0))
                                                                                                                          > arima221 <- arima(emprego, c(2,2,1))
                                                                                                                          > arima222 <- arima(emprego, c(2,2,2))
                                                                                                                          > arima223 <- arima(emprego, c(2,2,3))
                                                                                                                          > #MA
                                                                                                                            > arima021 <- arima(emprego, c(0,2,1))
                                                                                                                          > arima022 <- arima(emprego, c(0,2,2))
                                                                                                                          > arima023 <- arima(emprego, c(0,2,3))
                                                                                                                          > arima220 <- arima(emprego, c(2,2,0))
                                                                                                                          > arima0120 <- arima(emprego, c(1,2,0))
                                                                                                                          > estimacoes <- list(arima123,arima120,arima121,
                                                                                                                                               + arima122,arima220,rima221,
                                                                                                                                               + arima222,arima223,arima021,arima021, arima022,
                                                                                                                                               + arima023,arima0120)
                                                                                                                          Error: object 'rima221' not found
                                                                                                                          > estimacoes <- list(arima123,arima120,arima121,
                                                                                                                                               +                    arima122,arima220,arima221,
                                                                                                                                               +                    arima222,arima223,arima021,arima021, arima022,
                                                                                                                                               +                    arima023,arima0120)
                                                                                                                          > AIC <- sapply(estimacoes, AIC)
                                                                                                                          > BIC <- sapply(estimacoes, BIC)
                                                                                                                          > Modelo <-c("arima123","arima120","arima121",
                                                                                                                                       +                 "arima122","arima220","arima221",
                                                                                                                                       +                 "arima222","arima223","arima021","arima021", "arima022",
                                                                                                                                       +                 "arima023","arima0120")) 
                                                                                                      Error: unexpected ')' in:
                                                                                                        "                "arima222","arima223","arima021","arima021", "arima022",
                                                                                                      "arima023","arima0120"))"
                                                                                                      > Resultados <- data.frame(Modelo,AIC,BIC)
                                                                                                      Error in data.frame(Modelo, AIC, BIC) : 
                                                                                                        arguments imply differing number of rows: 3, 13
                                                                                                      > AIC <- sapply(estimacoes, AIC)
                                                                                                      > BIC <- sapply(estimacoes, BIC)
                                                                                                      > Modelo <-c("arima123","arima120","arima121",
                                                                                                                   + "arima122","arima220","arima221",
                                                                                                                   + Modelo <-c("arima123","arima120","arima121",
                                                                                                                                +                 "arima122","arima220","arima221",
                                                                                                                                +                 "arima222","arima223","arima021","arima021", "arima022",
                                                                                                                                +                 "arima023","arima0120")
                                                                                                                   + Resultados <- data.frame(Modelo,AIC,BIC)
                                                                                                                   Error: unexpected symbol in:
                                                                                                                     "                "arima023","arima0120")
                                                                                                                   Resultados"
                                                                                                                   > Modelo <-c("arima123","arima120","arima121",
                                                                                                                                +                 "arima122","arima220","arima221",
                                                                                                                                +                 "arima222","arima223","arima021","arima021", "arima022",
                                                                                                                                +                 "arima023","arima0120")
                                                                                                                   > Resultados <- data.frame(Modelo,AIC,BIC)