# Replication Package: Import Input Dependence and Financial Performance

**Author:** Sravani Pothana Boina  
**Study Target:** Listed Indian Manufacturing Companies (NSE)  
**Sample Details:** Balanced panel ($N=31, T=8, N \times T = 248$), covering FY 2016–17 to FY 2024–25 (excluding FY 2020–21).

---

### Repository Structure
* `importdata.xlsx`: Primary dataset containing 8-year financial statement disclosures across 12 NSE manufacturing sectors.
* `PothanaBoina_Replication_Script.R`: Complete R script to reproduce descriptive statistics, Spearman correlations, VIF diagnostics, Hausman tests, and baseline/interaction panel regression models with firm-clustered HC1 robust standard errors.

### Execution Instructions
1. Open `PothanaBoina_Replication_Script.R` in R or RStudio (requires R $\ge$ 4.0).
2. Execute the script. A file selection dialog will pop up automatically.
3. Select `importdata.xlsx` when prompted.
4. Regression outputs and diagnostics will print directly to the console.
