# MCDA (multicriteria decision analysis) validation using serological (serosurveillance data)

Case study: PPR (peste-des-petits ruminants) in three states Bauchi, Kano and Plateau in Nigeria

What is the predictive value of the MCDA index for infection risk with PPR? Can high deciles (above the median) of the MCDA index be used to focus PPR surveillance on these regions?

Step 1: Serological data

```         
Seroprevalence by local government, LGAs (in the form m positive animals among n sampled). 
```

Step 2: Getting the quantiles and relative risk of the estimates

Calculation of relative risks RRiRR_iRRi associated with the intervals between deciles of this MCDA index located above the median. Seroprevalence in the interval between the minimum and the median of MCDA is taken as the common denominator of RRiRR_iRRi.

```         
Calculation of quantiles 0% (min), 50% (median), 60%, 70%,..., 100% (max) = limits of the MCDA index classes. For each class iii (50-60%,..., 90-100%), calculate the average seroprevalence PiP_iPi of municipalities in this class Calculate RRi=Pi/P0−50%RR_i = P_i / P_{0-50\%}RRi=Pi/P0−50%
```

Step 3: Bootstrap for confidence intervals

```         
Draw 1000 times, with replacement, a pseudo-sample of municipalities, of the same size as the initial sample.
Re-calculate the RRiRR_iRRi for each pseudo-sample.
CI bounds: 25% and 75% quantiles of each series of 1000
RRiRR_iRRi values.
```

```         
```

![Relative risk per quantile based on the estimated data and the pseudo-sampled dataset in the 3 Nigerian states A. Bauchi, B. Kano and C. Plateau ](output/figure_3_states.png)
