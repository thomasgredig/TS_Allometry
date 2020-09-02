# TS_Allometry
 allometry data from Ted Stankowich

## Double Linear fit

```r
nls(data=d,
    Y ~ (N*X+B)*((Sign(X,a=k)+1)/2) +
      (M*X+k*(N-M)+B)*(1-Sign(X,a=k))/2,
    start = list(k=4.6, M=5.2,N=0.22, B=3.8)) -> fit
```

!(fitted data)[allometry-fit.png]
