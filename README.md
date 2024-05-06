# TwoStage_IMPVLGrangerTest:

This is a new method by improving the original  `VL_Granger Test` to compare two time series what the relationships they are and combining a new idea said `Separated Method` .

`VL_Granger Test` is proposed by [HERE](https://github.com/DarkEyes/VLTimeSeriesCausality "link")

This algorithm is constucted by six functions.

##  Whole Procedure 

![Procedure](https://i.meee.com.tw/6IjdzuE.png "proc")

You can see that the `Improved VL-Granger Test` and `Separated Method` are the classification of the way to get the result : **Variable Lag Relationship**、**Fixed Lag Relationship**、**No Lag Relationship** .

## Installation (R language)

You can use the funtion by the folder **R_type** , load the below six functions .
*  `VLgranger_Func.R`
*  `compute_P_Func.R`
*  `Simulate_Bound_Part1.R`
*  `Simulate_Bound_Part2.R`
*  `Separated_Method.R`
*  `TwoStage_ImpVLGrangerTest.R`

By using `TwoStage_ImpVLGrangerTest.R` , you can get the *Bound* and *lag Relations* .

## Example

We use the variable lag simulation to generate the time series **Xt** & **Yt** , where

**Xt** is a AR(1) model with parameter 0.3 by generating from `Variable_GENERATOR.R`  .

**Yt** is a variable lag of mixture of lag 1 and lag 2 delay to Xt by generating from `Variable_GENERATOR.R` .

![XYex](https://i.meee.com.tw/Pkra6Id.png "tsSIM")

Then we can use the function `SeparateIMPVLGranger` in `TwoStage_ImpVLGrangerTest.R`

```
RtOutcome <- SeparateImpVLGranger(Yt=Yt,Xt=Xt,maxLag = 10,alpha = 0.05,N_firstDO = 10)
```

And the reslut is

```
"所給定之 Xt 對於 Yt 具有 variable lag 之關係 ."
> RtOutcome$en
[1] 2
> RtOutcome$Bound
[1] 0.5066862
```

The **en** value represents that
* **en = 0** : no lag relationship
* **en = 1** : fixed lag relationship
* **en = 2** : variable lag relationship
