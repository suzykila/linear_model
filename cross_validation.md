cross_validation
================
ww2745
2024-11-07

look at LIDAR data

``` r
data("lidar")

lidar_df=
  lidar |> 
  as_tibble() |> 
  mutate(id=row_number())
```

``` r
lidar_df |> 
  ggplot(aes(x=range, y=logratio))+
  geom_point()
```

![](cross_validation_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

## Try to do CV

We’ll compre 3 models–one linear, one smooth, one wiggly.

Construct training and testing df

``` r
train_df = sample_frac(lidar_df, size=.8)
test_df=anti_join(lidar_df, train_df, by="id")
```

Look at these

``` r
ggplot(train_df, aes(x=range, y=logratio))+
  geom_point()+
  geom_point(data=test_df, color="red")
```

![](cross_validation_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

Fit three model

``` r
linear_mod=lm(logratio~range, data=train_df)
smooth_mod=gam(logratio~s(range),data=train_df)
wiggly_mod=gam(logratio~s(range,k=30),sp=10e-6,data=train_df)
```

Look at fits

``` r
train_df |> 
  add_predictions(smooth_mod) |> 
  ggplot(aes(x=range,y=logratio)) +
  geom_point()+
  #geom_point(data=test_df, color="red")+
  geom_line(aes(y=pred),color="red")
```

![](cross_validation_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->
wiggly–too complex, alittle bit too much flexibility linear:linear may
not be able to cath smooth:just right

Compare these numerically using RMSE.

``` r
rmse(linear_mod,test_df)
```

    ## [1] 0.127317

``` r
rmse(smooth_mod,test_df)
```

    ## [1] 0.08302008

``` r
rmse(wiggly_mod,test_df)
```

    ## [1] 0.08848557

## Repeat the train? test split
