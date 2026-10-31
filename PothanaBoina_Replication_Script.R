Author: Sravani Pothana Boina0. Load required packagesrequired <- c("readxl", "plm", "lmtest", "sandwich", "car", "dplyr", "Hmisc", "stargazer")
for (p in required) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p, dependencies = TRUE)
  library(p, character.only = TRUE)
}1. Load data via file selection dialogcat("\nSelect dataset file (importdata.xlsx)\n")
data_path <- file.choose()
stopifnot(file.exists(data_path))raw <- read_excel(data_path, sheet = "DATA")2. Data cleaning and ratio verificationnum_cols <- c("operating_revenue", "cost_of_raw_material_consumed",
                                                                                                                                  "operating_profit", "EBITDA", "PAT", "Total_Assets",
                                                                                                                                  "Current_Assets", "Current_Liabilities", "Total_Debt",
                                                                                                                                  "Forex_Earnings", "Forex_Outgo", "forex_dependency_ratio",
                                                                                                                                  "profit_margin", "current_ratio", "raw_material_cost_ratio")for (v in num_cols) {
                                                                                                                                    raw[[v]] <- suppressWarnings(as.numeric(raw[[v]]))
                                                                                                                                  }raw <- raw %>%
  mutate(
    Policy = factor(Policy_Period, levels = c("Pre", "Post")),
    post = ifelse(Policy == "Post", 1, 0),
    rmcr_chk = cost_of_raw_material_consumed / operating_revenue,
    fdr_chk = Forex_Outgo / operating_revenue,
    cr_chk = Current_Assets / Current_Liabilities,
    pm_chk = PAT / operating_revenue
  )3. Sample construction and panel setupvars_model <- c("EBITDA", "profit_margin", "raw_material_cost_ratio",
                                                         "forex_dependency_ratio", "current_ratio", "Total_Assets", "Total_Debt")panel_all  <- raw %>% filter(complete.cases(.[, vars_model]))
panel_pre  <- panel_all %>% filter(Policy == "Pre")
panel_post <- panel_all %>% filter(Policy == "Post")pdf_all  <- pdata.frame(panel_all, index = c("ticker", "year"))
pdf_pre  <- pdata.frame(panel_pre, index = c("ticker", "year"))
pdf_post <- pdata.frame(panel_post, index = c("ticker", "year"))f_fm <- EBITDA ~ raw_material_cost_ratio + forex_dependency_ratio + current_ratio + Total_Assets + Total_Debt
f_pm <- profit_margin ~ raw_material_cost_ratio + forex_dependency_ratio + current_ratio + Total_Assets + Total_Debt4. Model estimationols_ebitda  <- plm(f_fm, data = pdf_all, model = "pooling")
re_ebitda   <- plm(f_fm, data = pdf_all, model = "random")
fe_ebitda   <- plm(f_fm, data = pdf_all, model = "within")
twfe_ebitda <- plm(f_fm, data = pdf_all, model = "within", effect = "twoways")ols_pm  <- plm(f_pm, data = pdf_all, model = "pooling")
re_pm   <- plm(f_pm, data = pdf_all, model = "random")
fe_pm   <- plm(f_pm, data = pdf_all, model = "within")
twfe_pm <- plm(f_pm, data = pdf_all, model = "within", effect = "twoways")5. Diagnosticsht_ebitda <- phtest(fe_ebitda, re_ebitda)
ht_pm     <- phtest(fe_pm, re_pm)vif_ebitda <- car::vif(ols_ebitda)
vif_pm     <- car::vif(ols_pm)6. Pre/Post Sub-samples and Interaction Modelsfe_pre_ebitda  <- plm(f_fm, data = pdf_pre, model = "within")
fe_post_ebitda <- plm(f_fm, data = pdf_post, model = "within")
fe_pre_pm      <- plm(f_pm, data = pdf_pre, model = "within")
fe_post_pm     <- plm(f_pm, data = pdf_post, model = "within")f_fm_int <- EBITDA ~ raw_material_cost_ratio * post + forex_dependency_ratio * post + current_ratio + Total_Assets + Total_Debt
f_pm_int <- profit_margin ~ raw_material_cost_ratio * post + forex_dependency_ratio * post + current_ratio + Total_Assets + Total_Debtfe_int_ebitda <- plm(f_fm_int, data = pdf_all, model = "within", effect = "twoways")
re_int_pm     <- plm(f_pm_int, data = pdf_all, model = "random")7. Print Output Tablescat("\n--- HAUSMAN SPECIFICATION TESTS ---\n")
cat("EBITDA Model (FE vs RE): p-value =", ht_ebitda$p.value, "\n")
cat("Profit Margin Model (FE vs RE): p-value =", ht_pm$p.value, "\n")cat("\n--- DIAGNOSTIC VIFs ---\n")
print(round(vif_ebitda, 2))cat("\n--- TABLE 6: EBITDA PANEL MODELS ---\n")
stargazer(ols_ebitda, re_ebitda, fe_ebitda, twfe_ebitda,
          type = "text",
          se = list(sqrt(diag(vcovHC(ols_ebitda, type="HC1", cluster="group"))),
                    sqrt(diag(vcovHC(re_ebitda, type="HC1", cluster="group"))),
                    sqrt(diag(vcovHC(fe_ebitda, type="HC1", cluster="group"))),
                    sqrt(diag(vcovHC(twfe_ebitda, type="HC1", cluster="group")))),
          column.labels = c("Pooled OLS", "RE", "FE", "TWFE"),
          dep.var.labels = "EBITDA", digits = 3)cat("\n--- TABLE 6: PROFIT MARGIN PANEL MODELS ---\n")
stargazer(ols_pm, re_pm, fe_pm, twfe_pm,
          type = "text",
          se = list(sqrt(diag(vcovHC(ols_pm, type="HC1", cluster="group"))),
                    sqrt(diag(vcovHC(re_pm, type="HC1", cluster="group"))),
                    sqrt(diag(vcovHC(fe_pm, type="HC1", cluster="group"))),
                    sqrt(diag(vcovHC(twfe_pm, type="HC1", cluster="group")))),
          column.labels = c("Pooled OLS", "RE", "FE", "TWFE"),
          dep.var.labels = "Profit Margin", digits = 3)cat("\n--- TABLE 7: INTERACTION MODELS ---\n")
cat("EBITDA Interaction Model (TWFE):\n")
print(coeftest(fe_int_ebitda, vcov = vcovHC(fe_int_ebitda, type="HC1", cluster="group")))cat("\nProfit Margin Interaction Model (RE):\n")
print(coeftest(re_int_pm, vcov = vcovHC(re_int_pm, type="HC1", cluster="group")))