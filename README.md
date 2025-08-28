# Socio-Spatial Patterns of Suicide Mortality and Social Network Exposure (2010–2022)

## 1. Data Description
This repository integrates multiple county-level datasets:

- **Mortality Data:**  
  National Vital Statistics System (NVSS) Multiple Cause of Death files (2010–2022), restricted to suicide deaths identified using ICD-10 codes: **X60–X84, Y87.0**. Deaths were aggregated annually and standardized per 100,000 population using Census denominators.

- **Social Connectedness:**  
  Meta’s *Social Connectedness Index (SCI)* (2020), quantifying relative probabilities of Facebook friendship ties between counties.

- **Sociodemographic and Economic Covariates:**  
  - Agency for Healthcare Research and Quality (AHRQ) community-level indicators (2010–2020).  
  - American Community Survey (ACS) 5-year estimates for years not covered by AHRQ.  
  - Variables include: population density, age distribution, racial/ethnic composition, median household income, unemployment rate, educational attainment, and limited English proficiency.

---

## 2. Hypotheses
- **H1:** A one–standard deviation (1-SD) increase in the SCI-weighted average suicide mortality rate in socially connected counties is positively associated with suicide mortality in the focal county, controlling for spatial exposure.  
- **H2:** A one–standard deviation (1-SD) increase in *ERPO (Extreme Risk Protection Order)* social exposure is negatively associated with suicide mortality in the focal county.

---

## 3. Metric Construction

### Social Proximity to Suicide Deaths
The SCI-weighted average suicide mortality rate in counties socially connected to county *i* at time *t* is:

```
s_{-it} = Σ_{j ≠ i} w_{ij} y_{jt}
```
where the weights are:
```
w_{ij} = [n_j × SCI_{ij}] / Σ_{k ≠ i} [n_k × SCI_{ik}]
```

- `y_{jt}`: suicide mortality rate in county *j* at time *t*
- `n_j`: population of county *j*
- `SCI_{ij}`: Social Connectedness Index between counties *i* and *j*

---
  

### Spatial Proximity to Suicide Deaths
The spatially weighted average suicide mortality rate in counties geographically close to county *i* at time *t* is:

```
d_{-it} = Σ_{j ≠ i} a_{ij} y_{jt}
```
where the weights are:
```
a_{ij} = [1/d_{ij}] / Σ_{k ≠ i} [1/d_{ik}]
```

- `y_{jt}`: suicide mortality rate in county *j* at time *t*
- `d_{ij}`: great-circle distance between centroids of counties *i* and *j*

### ERPO Social Exposure

The ERPO Social Exposure for county *i* at time *t* is:

```
ERPO_Social_Exposure_{it} = Σ_{s(i) ≠ s(j)} 1[ERPO in state s(j)]_t × [SCI_{ij} / Σ_h SCI_{ih}]
```

where:
- `1[ERPO in state s(j)]_t`: indicator if state *j* has an ERPO law at time *t*
- `SCI_{ij}`: Social Connectedness Index between counties *i* and *j*
- The sum is over all counties *j* in a different state than *i*

---

### ERPO Spatial Exposure

The ERPO Spatial Exposure for county *i* at time *t* is:

```
ERPO_Spatial_Exposure_{it} = Σ_{s(i) ≠ s(j)} 1[ERPO in state s(j)]_t × [1/d_{ij} / Σ_{k ≠ i} (1/d_{ik})]
```

where:
- `d_{ij}`: great-circle distance between centroids of counties *i* and *j*
- The sum is over all counties *j* in a different state than *i*

---

## 4. Regression Specifications

### Equation (3): Social and Spatial Influence

The regression model for social and spatial influence is:

```
y_{it} = ζ₁ s_{-it} + ζ₂ d_{-it} + ζ₃ᵗ X_{it} + μ_i + φ_t + ε_{it}
```

where:
- `y_{it}`: suicide mortality in county *i*, year *t*
- `s_{-it}`: SCI-weighted average suicide mortality rate in socially connected counties
- `d_{-it}`: spatially weighted average suicide mortality rate in geographically close counties
- `X_{it}`: time-varying covariates
- `μ_i`: county fixed effects
- `φ_t`: year fixed effects
- `ε_{it}`: error term

---

### Equation (8): ERPO Social and Spatial Exposure

The regression model for ERPO social and spatial exposure is:

```
y_{it} = θ₁ ERPO_Social_Exposure_{it} + θ₂ ERPO_Spatial_Exposure_{it} + θ₃ᵗ X_{it} + φ_i + γ_{st} + ε_{it}
```

where:
- `ERPO_Social_Exposure_{it}`: ERPO social exposure for county *i* at time *t*
- `ERPO_Spatial_Exposure_{it}`: ERPO spatial exposure for county *i* at time *t*
- `X_{it}`: time-varying covariates
- `φ_i`: county fixed effects
- `γ_{st}`: state-by-year fixed effects
- `ε_{it}`: error term

---

## 5. Decision Criteria
- **Statistical significance threshold:** \(p < 0.05\).  
- Results are reported with cluster-robust standard errors at the state level.  

---

## 6. Repository Structure
- **`nvss_indirect_exposure_2010_2022.R`**  
  Contains implementation of the regression models testing main hypotheses (equation 3, equation 8).  

- **`sensitivity_analysis_with_age_adjusted_suicide_.R`**  
  Implements robustness checks using **age-adjusted suicide mortality** as the dependent variable (Supplementary Section).  

- **`population_estimate_by_age_group_2010_2022.R`**  
  Data pipeline for generating age-group-specific population estimates for denominator alignment.  


