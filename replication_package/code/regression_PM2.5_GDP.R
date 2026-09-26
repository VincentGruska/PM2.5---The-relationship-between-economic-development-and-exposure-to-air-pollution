###############################################################################
# PIP V — Track B (Structural development)
# Inverted-U (Environmental Kuznets Curve) relationship between economic
# development and population exposure to fine particulate matter (PM2.5).
#
#   Dependent variable : ln(PM2.5)        mean annual population-weighted
#                                         exposure, micrograms / m3 (World Bank)
#   Independent variable: ln(GDP/capita)  development proxy
#   Hypothesis          : inverted U  (b1 > 0, b2 < 0, turning point IN-sample)
#
# VERSION: population density excluded as control variable in model 6.
# Estimation sample kept identical (3,185 obs) for direct comparability.
###############################################################################

## 0) PACKAGES -----------------------------------------------------------------

pkgs <- c("fixest", "modelsummary", "car", "flextable", "officer")
for (p in pkgs) if (!requireNamespace(p, quietly = TRUE))
  install.packages(p, repos = "https://cloud.r-project.org")

library(fixest)
library(modelsummary)
library(car)
library(flextable)
library(officer)

setFixest_etable(digits = 3, digits.stats = 3)

# Output directory
OUT <- "../output"
dir.create(OUT, showWarnings = FALSE)

## 1) DATA & TRANSFORMATIONS ---------------------------------------------------

d    <- read.csv("../data/panel_final.csv", stringsAsFactors = FALSE)
d$cc <- factor(d$country_code)

xbar  <- mean(d$ln_gdp_pc, na.rm = TRUE)
d$x   <- d$ln_gdp_pc - xbar
d$x2  <- d$x^2
d$x3  <- d$x^3

cat(sprintf("Mean ln(GDP/cap) used for centring: %.4f  (GDP ~ $%.0f)\n",
            xbar, exp(xbar)))

# Keep same estimation sample as main regression for direct comparability
controls <- c("ln_pop_density","urban_pct","industry_gdp_pct",
              "trade_gdp_pct","renewables_pct")
keep  <- complete.cases(d[, c("ln_pm25","x","x2", controls)])
d5    <- d[keep, ]
cat(sprintf("Estimation sample: N = %d, countries = %d\n",
            nrow(d5), nlevels(droplevels(d5$cc))))

## 2) DESCRIPTIVE STATISTICS (§III) -------------------------------------------

descr_vars <- c("pm25","gdp_pc","urban_pct",
                "industry_gdp_pct","trade_gdp_pct","renewables_pct","co2_pc")
datasummary(
  All(d5[, descr_vars]) ~ N + Mean + SD + Min + Median + Max,
  data   = d5[, descr_vars],
  output = file.path(OUT, "descriptives.txt")
)
cat(sprintf("Descriptive statistics saved to %s/descriptives.txt\n", OUT))
print(summary(d5[, descr_vars]))

# Descriptive statistics as Word document (booktabs style, Times New Roman)
descr_vars_tbl <- c("pm25","gdp_pc","urban_pct",
                    "industry_gdp_pct","trade_gdp_pct","renewables_pct")

descr_labels <- c(
  pm25             = "PM2.5 exposure",
  gdp_pc           = "GDP per capita",
  urban_pct        = "Urban population share",
  industry_gdp_pct = "Industrial production share",
  trade_gdp_pct    = "Trade openness",
  renewables_pct   = "Renewable energy"
)
descr_units <- c(
  pm25             = "µg/m³",
  gdp_pc           = "current USD",
  urban_pct        = "%",
  industry_gdp_pct = "% of GDP",
  trade_gdp_pct    = "% of GDP",
  renewables_pct   = "% of energy"
)

descr_tbl <- do.call(rbind, lapply(descr_vars_tbl, function(v) {
  x <- d5[[v]]
  data.frame(
    Variable = descr_labels[v],
    Unit     = descr_units[v],
    N        = sum(!is.na(x)),
    Mean     = round(mean(x,   na.rm = TRUE), 2),
    SD       = round(sd(x,     na.rm = TRUE), 2),
    Min      = round(min(x,    na.rm = TRUE), 2),
    Median   = round(median(x, na.rm = TRUE), 2),
    Max      = round(max(x,    na.rm = TRUE), 2),
    stringsAsFactors = FALSE
  )
}))

thick <- fp_border(width = 1.5)
thin  <- fp_border(width = 0.75)
notes <- paste(
  "Notes: Estimation sample: 3,185 country-year observations, 174 countries, 2000–2019.",
  "All statistics computed on the estimation sample (complete cases for the fullest specification).",
  "GDP per capita and PM2.5 are entered in log form in the regression.",
  "Population density and CO₂ per capita are excluded from this table;",
  "CO₂ is included in the dataset for reference only and is not used as a control variable."
)

ft_descr <- flextable(descr_tbl) |>
  font(fontname = "Times New Roman", part = "all") |>
  fontsize(size = 11, part = "all") |>
  bold(part = "header") |>
  align(j = 1:2, align = "left",  part = "all") |>
  align(j = 3:8, align = "right", part = "all") |>
  border_remove() |>
  hline_top(part = "header", border = thick) |>
  hline(part = "header",     border = thin)  |>
  hline_bottom(part = "body", border = thick) |>
  add_footer_lines(notes) |>
  font(fontname = "Times New Roman", part = "footer") |>
  fontsize(size = 9, part = "footer") |>
  autofit()

doc_descr <- read_docx() |>
  body_add_flextable(ft_descr)
print(doc_descr, target = file.path(OUT, "descriptives.docx"))
cat(sprintf("Word descriptives table saved to %s/descriptives.docx\n", OUT))

## 3) REGRESSION LADDER (§IV) --------------------------------------------------

m1  <- feols(ln_pm25 ~ x + x2,             data = d5, vcov = ~cc)  # I   pooled OLS
m2  <- feols(ln_pm25 ~ x + x2 | cc,        data = d5, vcov = ~cc)  # II  country FE
m3  <- feols(ln_pm25 ~ x + x2 | year,      data = d5, vcov = ~cc)  # III year FE
m3b <- feols(ln_pm25 ~ x + x2 + year,      data = d5, vcov = ~cc)  # III continuous trend
m4  <- feols(ln_pm25 ~ x + x2 | cc + year, data = d5, vcov = ~cc)  # IV  two-way FE
m5  <- feols(ln_pm25 ~ x + x2 +                                     # V   + controls (no pop. density)
               urban_pct +
               industry_gdp_pct + trade_gdp_pct + renewables_pct |
               cc + year,                    data = d5, vcov = ~cc)

DICT <- c(x                = "ln GDP/cap (centred)",
          x2               = "(ln GDP/cap)²",
          urban_pct        = "Urban %",
          industry_gdp_pct = "Industry %GDP",
          trade_gdp_pct    = "Trade %GDP",
          renewables_pct   = "Renewables %")

# Print to console
cat("\n── Regression Table ────────────────────────────────────────────────────\n")
etable(m1, m2, m3, m3b, m4, m5, se.below = TRUE,
       fitstat = ~ n + r2 + wr2, dict = DICT)

# Save as LaTeX and plain text
etable(m1, m2, m3, m3b, m4, m5, se.below = TRUE,
       fitstat = ~ n + r2 + wr2, dict = DICT,
       title   = "PM2.5 exposure and economic development (no population density control)",
       file    = file.path(OUT, "regression_table.tex"), replace = TRUE)

etable(m1, m2, m3, m3b, m4, m5, se.below = TRUE,
       fitstat = ~ n + r2 + wr2, dict = DICT,
       title   = "PM2.5 exposure and economic development (no population density control)",
       file    = file.path(OUT, "regression_table.txt"), replace = TRUE)

cat(sprintf("Regression table saved to %s/\n", OUT))

# Save as Word document
ft <- modelsummary(
  list("Pooled OLS" = m1, "Country FE" = m2, "Year FE" = m3,
       "Year trend" = m3b, "Two-way FE" = m4, "+ Controls" = m5),
  coef_rename  = DICT,
  stars        = c("*" = .1, "**" = .05, "***" = .01),
  gof_map      = c("nobs", "r.squared", "within.r.squared"),
  title        = "PM2.5 exposure and economic development (no population density control)",
  output       = "flextable"
) |> autofit()

doc <- read_docx() |>
  body_add_par("PM2.5 exposure and economic development (no population density control)",
               style = "heading 1") |>
  body_add_flextable(ft)
print(doc, target = file.path(OUT, "regression_table.docx"))
cat(sprintf("Word table saved to %s/regression_table.docx\n", OUT))

## 4) FUNCTIONAL-FORM TESTS (§IV) ----------------------------------------------

m4c <- feols(ln_pm25 ~ x + x2 + x3 | cc + year, data = d5, vcov = ~cc)
cat("\n── Functional-form test: cubic term x^3 ────────────────────────────────\n")
print(summary(m4c)$coeftable["x3", , drop = FALSE])

d5$yhat2 <- predict(m4)^2
m4r <- feols(ln_pm25 ~ x + x2 + yhat2 | cc + year, data = d5, vcov = ~cc)
cat("\n── RESET test: fitted^2 coefficient (p-value) ──────────────────────────\n")
print(summary(m4r)$coeftable["yhat2", , drop = FALSE])

## 5) INVERTED-U: TURNING POINT, CI, AND LIND & MEHLUM U-TEST ----------------

uturn <- function(m, xbar, xrange) {
  b  <- coef(m);  V  <- vcov(m)
  b1 <- b["x"];   b2 <- b["x2"]
  xs <- as.numeric(-b1 / (2 * b2))
  g  <- c(-1 / (2 * b2), b1 / (2 * b2^2))
  Vb <- V[c("x","x2"), c("x","x2")]
  se <- sqrt(as.numeric(t(g) %*% Vb %*% g))
  ln_lo <- xs + xbar - 1.96 * se
  ln_hi <- xs + xbar + 1.96 * se
  s_lo  <- b1 + 2 * b2 * xrange[1]
  s_hi  <- b1 + 2 * b2 * xrange[2]
  cat(sprintf("Turning point: ln GDP = %.3f  ->  GDP/cap = $%.0f\n",
              xs + xbar, exp(xs + xbar)))
  cat(sprintf("  95%% CI:        GDP/cap  [$%.0f ,  $%.0f]\n",
              exp(ln_lo), exp(ln_hi)))
  cat(sprintf("  Lind-Mehlum:  slope@min = %+.3f (want > 0);  slope@max = %+.3f (want < 0)\n",
              s_lo, s_hi))
  cat(sprintf("  Interior turning point: %s\n",
              ifelse(xrange[1] < xs & xs < xrange[2], "YES — EKC confirmed", "NO")))
}

cat("\n── Two-way FE (m4): inverted-U test ────────────────────────────────────\n")
uturn(m4, xbar, range(d5$x))

## 6) SENSITIVITY ANALYSIS (§VI) -----------------------------------------------

b    <- coef(m4); V <- vcov(m4)
xs   <- seq(min(d5$x), max(d5$x), length.out = 200)
ME   <- b["x"] + 2 * b["x2"] * xs
seME <- sqrt(V["x","x"] + 4*xs^2*V["x2","x2"] + 4*xs*V["x","x2"])
gdp  <- exp(xs + xbar)
xstar <- exp(-b["x"] / (2 * b["x2"]) + xbar)

png(file.path(OUT, "sensitivity_marginal_effect.png"),
    width = 1050, height = 640, res = 150)
plot(gdp, ME, type = "l", lwd = 2, log = "x", col = "#234d7a",
     xlab = "GDP per capita (US$, log scale)",
     ylab = "d ln(PM2.5) / d ln(GDP)",
     main = "Sensitivity: income marginal effect on PM2.5 (two-way FE)")
polygon(c(gdp, rev(gdp)),
        c(ME - 1.96 * seME, rev(ME + 1.96 * seME)),
        col = adjustcolor("#234d7a", 0.18), border = NA)
abline(h = 0, col = "grey50")
abline(v = xstar, col = "#b5343f", lty = 2, lwd = 1.3)
legend("topright", bty = "n", lwd = c(2, 6, 1.3),
       col = c("#234d7a", adjustcolor("#234d7a", 0.18), "#b5343f"),
       legend = c("Marginal effect", "95% CI",
                  sprintf("Turning point ~ $%.0f", xstar)))
dev.off()
cat(sprintf("Marginal effect plot saved to %s/sensitivity_marginal_effect.png\n", OUT))

se_b1 <- sqrt(V["x","x"]);  se_b2 <- sqrt(V["x2","x2"])
tp    <- function(b1, b2) exp(-b1 / (2 * b2) + xbar)

cat("\n── Sensitivity: turning point under ±1 SD parameter perturbation ───────\n")
cat(sprintf("  b1 ± 1 SD:  [$%.0f ,  $%.0f]\n",
            tp(b["x"] - se_b1, b["x2"]),
            tp(b["x"] + se_b1, b["x2"])))
cat(sprintf("  b2 ± 1 SD:  [$%.0f ,  $%.0f]\n",
            tp(b["x"], b["x2"] - se_b2),
            tp(b["x"], b["x2"] + se_b2)))

cat(sprintf("\nDone. All outputs saved to %s/\n", OUT))

## 7) EKC VISUALISATION --------------------------------------------------------

x_at  <- c(100, 1000, 10000, 100000)
x_lab <- expression(10^2, 10^3, 10^4, 10^5)

png(file.path(OUT, "ekc_scatter.png"), width = 2000, height = 850, res = 160)
par(mfrow = c(1, 2), mar = c(4.2, 4.5, 3, 1), bg = "white")

# --- Panel A: RAW pooled cross-section ----------------------------------------
plot(d5$gdp_pc, d5$ln_pm25, log = "x", pch = 16, cex = .3,
     col = adjustcolor("#6b7b8c", .18),
     xlab = "GDP per capita (US$, log scale)", ylab = "ln(PM2.5 exposure)",
     main = "A. Pooled cross-section: monotone decline",
     axes = FALSE)
box(); axis(1, at = x_at, labels = x_lab); axis(2)

mp <- coef(m1)
xx <- seq(min(d5$x), max(d5$x), length.out = 200)
gg <- exp(xx + xbar)
lines(gg, mp[1] + mp["x"]*xx + mp["x2"]*xx^2, col = "#234d7a", lwd = 2.3)
lw <- lowess(d5$ln_gdp_pc, d5$ln_pm25, f = .5)
lines(exp(lw$x), lw$y, col = "#b5343f", lwd = 1.8, lty = 2)
legend("bottomleft", bty = "n", lwd = c(2.3, 1.8), lty = c(1, 2),
       col = c("#234d7a", "#b5343f"), cex = 0.75,
       legend = c("Pooled quadratic fit", "LOWESS"))

# --- Panel B: within-country partial-regression plot --------------------------
b    <- coef(m4)
cr   <- b["x"]*d5$x + b["x2"]*d5$x2 + resid(m4)
plot(d5$gdp_pc, cr, log = "x", pch = 16, cex = .3,
     col = adjustcolor("#6b7b8c", .10),
     xlab = "GDP per capita (US$, log scale)",
     ylab = "PM2.5 | country & year FE (component+residual)",
     main = "B. Within countries: inverted U",
     axes = FALSE)
box(); axis(1, at = x_at, labels = x_lab); axis(2)

br   <- cut(d5$ln_gdp_pc,
            quantile(d5$ln_gdp_pc, seq(0, 1, length.out = 41)),
            include.lowest = TRUE)
bm   <- tapply(cr, br, mean)
bg_g <- exp(tapply(d5$ln_gdp_pc, br, mean))
points(bg_g, bm, pch = 16, cex = .9, col = "grey20")
lines(gg, b["x"]*xx + b["x2"]*xx^2, col = "#234d7a", lwd = 2.5)
abline(v = xstar, col = "#b5343f", lty = 2, lwd = 1.4)
legend("topright", bty = "n", pch = c(16, NA, NA), lwd = c(NA, 2.5, 1.4),
       lty = c(NA, 1, 2), col = c("grey20", "#234d7a", "#b5343f"), cex = 0.75,
       legend = c("Binned means (40 bins)", "Two-way FE fitted curve",
                  sprintf("Turning point ≈ $%.0f", xstar)))
dev.off()
cat(sprintf("EKC scatter plot saved to %s/ekc_scatter.png\n", OUT))
