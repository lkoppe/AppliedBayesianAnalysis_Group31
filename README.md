# Applied Bayesian Data Analysis

## Dataset

[Communities and Crime dataset](https://archive.ics.uci.edu/dataset/183/communities+and+crime)

### Key Features:

-   **Size**: 1994 observations with 122 attributes.
-   **Target Variable**: `ViolentCrimesPerPop` (Per capita violent crime rate, normalized between 0 and 1).
-   **Attributes**: Socio-economic indicators, demographic proportions, law enforcement statistics, and more.
-   **Missing Values**: Exist

------------------------------------------------------------------------

## Model

**Bayesian Beta Regression** model for the response variable (`ViolentCrimesPerPop`) constrained to [0, 1].

### Model Assumptions

1.  The **response variable** (`y`) follows a **Beta distribution** with shape parameters `a` and `b`.
2.  Predictors are linearly related to the logit-transformed mean of the response variable.
3.  Observations are independent.

### Model Specification

#### Mean of the Beta Distribution

The mean (`mu`) is modeled as:

```         
mu = logit^-1(alpha + beta1 * x1 + beta2 * x2 + beta3 * x3 + beta4 * x4 + beta5 * x5)
```

Where: - `alpha`: Intercept - `beta1`, `beta2`, ..., `beta5`: Coefficients for predictors (`x1`, `x2`, ..., `x5`).

#### Shape Parameters

The shape parameters of the Beta distribution are:

```         
a = mu * phi
b = (1 - mu) * phi
```

Where: - `phi`: Precision parameter, representing the concentration of the Beta distribution.

#### Likelihood

The likelihood of the observed data:

```         
y ~ Beta(a, b)
```

------------------------------------------------------------------------

## Variables

### Response Variable

-   **`ViolentCrimesPerPop`**: Normalized violent crime rate, calculated as the ratio of violent crimes (murder, rape, robbery, and assault) to the population.

### Predictors

Selected predictors include: - **`PctKids2Par`**: Percentage of children in two-parent households. - **`PctImmigRec10`**: Percentage of recent immigrants (last 10 years). - **`PctPopUnderPov`**: Percentage of the population under poverty. - **`medFamInc`**: Median family income. - **`racePctWhite`**: Percentage of white population.

------------------------------------------------------------------------

## Limitations

1.  **Linear Assumptions**:

    -   The model assumes a linear relationship between predictors and the logit-transformed response.
    -   Nonlinear effects or interactions are not captured.

2.  **Feature Selection**:

    -   Only a subset of predictors is used.

3.  **Missing Communities**

4.  **Recorded crimes**

    -   Not all types of crime are recorded in dataset
