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
*  `Simmulate_Bound_Part1.R`
*  `Simmulate_Bound_Part2.R`
*  `Separated_Method.R`
*  `TwoStage_ImpVLGrangerTest.R`

By using `TwoStage_ImpVLGrangerTest.R` , you can get the *Bound* and *lag Relations* .
