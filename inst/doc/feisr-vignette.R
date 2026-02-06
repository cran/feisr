## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(feisr)
data("mwp", package = "feisr")
head(mwp)

## ----feis0, width = 70--------------------------------------------------------
wages.feis0 <- feis(lnw ~ marry | year, data = mwp, id = "id")
summary(wages.feis0)

## ----feis, width = 70---------------------------------------------------------
wages.feis <- feis(lnw ~ marry + enrol + yeduc + as.factor(yeargr)
                   | exp + I(exp^2), data = mwp, id = "id")
summary(wages.feis)

## ----coef, echo = FALSE , results = "hide"------------------------------------
coef1 <- unname(round(wages.feis$coefficients[1], 3))

## ----feis2, width = 70--------------------------------------------------------
wages.feis.rob <- feis(lnw ~ marry + enrol + yeduc + as.factor(yeargr)
                       | exp + I(exp^2), data = mwp, id = "id",
                       robust = TRUE)

## ----fe, width = 70-----------------------------------------------------------
library(plm)
wages.fe <- plm(lnw ~ marry + enrol + yeduc + as.factor(yeargr)
                + exp + I(exp^2), data = mwp, index = c("id", "year"),
                model = "within", effect = "individual")
wages.re <- plm(lnw ~ marry + enrol + yeduc + as.factor(yeargr)
                + exp + I(exp^2), data = mwp, index = c("id", "year"),
                model = "random", effect = "individual")

## ----extract.feis, echo = FALSE , results = "hide", warning = FALSE, message = FALSE----
# Fallback for production of vignettes if texreg < 1.37.1 (internal extract.feis)
if(utils::packageVersion("texreg") < "1.37.1"){
  library(texreg)
  setMethod("extract", signature = className("feis", "feisr"),
        definition = feisr:::extract.feis)
}

## ----compare, width = 70------------------------------------------------------
library(texreg)
screenreg(list(wages.feis, wages.feis.rob, wages.fe, wages.re), digits = 3,
          custom.model.names = c("FEIS", "FEIS robust", "FE", "RE"))

## ----coef2, echo = FALSE , results = "hide"-----------------------------------
coef2 <- unname(round(wages.fe$coefficients[1], 3))
coef3 <- unname(round(wages.re$coefficients[2], 3))
diff <- coef2 - coef1
diff2 <- coef3 - coef1
rel <- round(coef3/coef1, 1)
r2.fe <- unname(round(summary(wages.fe)$r.squared[1], 3))
r2.feis <- unname(round(wages.feis$r2, 3))

## ----feistes, width = 70------------------------------------------------------
ht <- feistest(wages.feis, robust = TRUE, type = "all")
summary(ht)

## ----coef3, echo = FALSE , results = "hide"-----------------------------------
chi2_feis <- round(unname(ht$wald_feis$result$chi2[1]), 3)
chi2_fe <- round(unname(ht$wald_fe$result$chi2[1]), 3)
p_fe <- round(unname(ht$wald_fe$result$chi2[3]), 3)
chi2_re <- round(unname(ht$wald_re$result$chi2[1]), 3)
p_re <- round(unname(ht$wald_re$result$chi2[3]), 3)

## ----bsfeistes, width = 70----------------------------------------------------
bsht <- bsfeistest(wages.feis, type = "all", rep = 100, seed = 91020104)
summary(bsht)

## ----coef4, echo = FALSE , results = "hide"-----------------------------------
bschi2_feis <- round(unname(bsht$wald_feis$result$chi2[1]), 3)
bschi2_fe <- round(unname(bsht$wald_fe$result$chi2[1]), 3)
bsp_fe <- round(unname(bsht$wald_fe$result$chi2[3]), 3)
bschi2_re <- round(unname(bsht$wald_re$result$chi2[1]), 3)
bsp_re <- round(unname(bsht$wald_re$result$chi2[3]), 3)

## ----slope, width = 70--------------------------------------------------------
alpha <- slopes(wages.feis)
head(alpha)

## ----merge, width = 70, fig.width = 7, fig.height = 5-------------------------
colnames(alpha) <- paste0("slp_", colnames(alpha))
alpha.df <- data.frame(alpha, id = rownames(alpha))
mwp <- merge(mwp, alpha.df, by = "id")

## ----predicted, width = 70, fig.width = 7, fig.height = 5---------------------
### Individual predicted trend of lnw (based on slopes)
mwp$pred <- mwp$slp_.Intercept. + mwp$exp * mwp$slp_exp + mwp$exp^2 * mwp$slp_I.exp.2.

### Average value by evermarry and exp bins 
mwp$exp_gr <- round(mwp$exp, 0)
aggr <- aggregate(mwp$pred, by = list(exp = mwp$exp_gr, evermarry = mwp$evermarry), mean)

### Plot
library(ggplot2)
zp1  <- ggplot(data = mwp, aes(x = exp, y = pred)) +
  geom_line(aes(group = id, col = as.factor(marry)),
            linetype = "solid", lwd = 0.5, alpha = 0.4) + 
  geom_line(data = aggr, aes(y = x, linetype = as.factor(evermarry)),
            alpha = 1, size = 1.2) +
  labs(color = "Married", linetype = "Ever-married") + 
  guides(colour = guide_legend(override.aes = list(alpha = 1))) + 
  labs(y = "Predicted log wage growth", x = "Work experience") +
  theme_bw()
zp1

