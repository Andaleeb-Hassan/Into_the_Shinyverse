# Chi-square test of independence, written in order.
#
# Open this file in RStudio and click Source, or from this folder run:
#   Rscript lesson.R
#
# Read the console from top to bottom. app.R is the same arithmetic
# with menus, after you have seen the steps once.

say <- function(...) cat(paste0(...), "\n", sep = "")

# Where is this file? Works for Rscript and for sourcing from this folder.
args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
root <- if (length(file_arg)) dirname(sub("^--file=", "", file_arg[[1]])) else "."

# ---------------------------------------------------------------------------
# The question
# ---------------------------------------------------------------------------
# We have two groupings, for example gender and smoking status.
# H0 (null):        they are independent. Knowing the row does not
#                   change the mix of the column.
# Ha (alternative): they are not independent.
#
# "Not independent" is not the same sentence as "one causes the other."

# ---------------------------------------------------------------------------
# Example 1. A table small enough to check by hand
# ---------------------------------------------------------------------------
#                 Current   Former
# Female             30       20
# Male               20       30

say("")
say("Example 1. A 2 by 2 table you can recompute on paper")
say("-----------------------------------------------------")

observed <- matrix(
  c(30, 20,
    20, 30),
  nrow = 2,
  byrow = TRUE,
  dimnames = list(
    Gender = c("Female", "Male"),
    Smoking = c("Current", "Former")
  )
)
say("Observed counts, O:")
print(observed)

# Expected count if H0 is true and the totals stay fixed:
#   E = (row total * column total) / grand total
n <- sum(observed)
expected <- outer(rowSums(observed), colSums(observed)) / n
say("Expected counts, E = (row total * column total) / ", n, ":")
print(expected)

# One contribution per cell, then add them.
#   chi-square = sum of (O - E)^2 / E
pieces <- (observed - expected)^2 / expected
say("Each cell contributes (O - E)^2 / E:")
print(pieces)
chi_sq <- sum(pieces)
say("Chi-square statistic = ", chi_sq)

# An r by c table has df = (r - 1) * (c - 1).
# Here that is 1 * 1 = 1. The curve has one shape for each df.
df <- (nrow(observed) - 1) * (ncol(observed) - 1)
say("Degrees of freedom = ", df)

# Alpha is the significance level. The critical value leaves
# probability alpha in the right tail of the chi-square curve.
# Reject H0 when the statistic is larger than that critical value.
# A true H0 is then rejected about alpha of the time (a Type I error),
# when the chi-square approximation is reasonable.
alpha <- 0.05
critical_05 <- qchisq(alpha, df, lower.tail = FALSE)
critical_01 <- qchisq(0.01, df, lower.tail = FALSE)
p_value <- pchisq(chi_sq, df, lower.tail = FALSE)

say("Critical value at alpha 0.05 = ", round(critical_05, 3))
say("Critical value at alpha 0.01 = ", round(critical_01, 3))
say("A smaller alpha moves the critical value to the right.")
say("p-value = ", round(p_value, 4))
say("The p-value is the probability of a statistic at least this large if H0 is true.")
say("It is not the probability that H0 is true.")

if (chi_sq > critical_05) {
  say("At alpha 0.05, ", round(chi_sq, 3), " > ", round(critical_05, 3), ", so reject H0.")
} else {
  say("At alpha 0.05, do not reject H0.")
}
if (chi_sq > critical_01) {
  say("At alpha 0.01, ", round(chi_sq, 3), " > ", round(critical_01, 3), ", so reject H0.")
} else {
  say("At alpha 0.01, ", round(chi_sq, 3), " is not larger than ", round(critical_01, 3), ", so do not reject H0.")
}
say("Same table, two decisions. Alpha is a choice, not a property of the data.")

# Odds ratio, only because the table is 2 by 2.
# Odds of Current rather than Former in a row = first column / second column.
# Odds ratio = Female odds / Male odds = (n11 * n22) / (n12 * n21).
n11 <- observed[1, 1]
n12 <- observed[1, 2]
n21 <- observed[2, 1]
n22 <- observed[2, 2]
odds_ratio <- (n11 * n22) / (n12 * n21)
say(
  "Odds ratio = (", n11, " * ", n22, ") / (", n12, " * ", n21, ") = ",
  round(odds_ratio, 2)
)
say("Female odds of Current rather than Former are ", round(odds_ratio, 2), " times the Male odds.")
say("1 would mean the two rows have the same odds.")

# Wald interval on the log odds ratio.
# se = sqrt(1/n11 + 1/n12 + 1/n21 + 1/n22)
se <- sqrt(1 / n11 + 1 / n12 + 1 / n21 + 1 / n22)
z <- qnorm(0.975) # 95% interval, because alpha was 0.05 and 1 - 0.05 = 0.95
ci <- exp(log(odds_ratio) + c(-1, 1) * z * se)
say(
  "Wald 95% interval: ", round(ci[1], 2), " to ", round(ci[2], 2), ".",
  " Across repeated samples, 95% of such intervals contain the population odds ratio."
)
say("This interval either contains it or it does not. The 95% is not a probability for this one interval.")

# ---------------------------------------------------------------------------
# Example 2. Independence on the nose
# ---------------------------------------------------------------------------
#                 Current   Former
# Female             10       20
# Male               30       60
# Every column is three times the Female row. Observed equals expected.

say("")
say("Example 2. A table that matches independence exactly")
say("----------------------------------------------------")

quiet <- matrix(
  c(10, 20,
    30, 60),
  nrow = 2,
  byrow = TRUE,
  dimnames = list(
    Gender = c("Female", "Male"),
    Smoking = c("Current", "Former")
  )
)
quiet_expected <- outer(rowSums(quiet), colSums(quiet)) / sum(quiet)
quiet_chi <- sum((quiet - quiet_expected)^2 / quiet_expected)
say("Chi-square = ", round(quiet_chi, 4), ". Do not reject H0.")
say("Not rejecting H0 does not prove the variables are independent.")
say("Failing to notice a real association is a Type II error. This lesson does not calculate that risk.")

# ---------------------------------------------------------------------------
# Example 3. The same steps on the practice file
# ---------------------------------------------------------------------------
say("")
say("Example 3. The same steps on data/lung_cancer.csv")
say("--------------------------------------------------")
say("This file is simulated practice data, not a health study.")
say("Smoking status was generated to depend on gender and on race.")
say("Gender and race were generated independently.")

path <- file.path(root, "data", "lung_cancer.csv")
if (!file.exists(path)) {
  stop("Could not find ", path, call. = FALSE)
}
raw <- read.csv(path, stringsAsFactors = FALSE)

# Short labels, and drop rows that are not a single race group.
long_names <- c(
  "Black or African-American" = "Black",
  "Native Hawaiian or Other Pacific Islander" = "Hawaiian",
  "American Indian or Alaskan Native" = "Native",
  "Participant refused to answer" = "Other"
)
known <- !is.na(raw$race) & raw$race %in% names(long_names)
raw$race[known] <- unname(long_names[raw$race[known]])

ok <- !is.na(raw$gender) & nzchar(raw$gender) &
  !is.na(raw$race) & nzchar(raw$race) &
  !is.na(raw$smoker) & nzchar(raw$smoker) &
  !(raw$race %in% c("N/A", "More than one race"))
cohort <- raw[ok, c("gender", "race", "smoker")]
say("Rows used: ", nrow(cohort), ". Rows set aside: ", nrow(raw) - nrow(cohort), ".")

gender_smoke <- table(cohort$gender, cohort$smoker)
say("Observed counts, gender by smoking status:")
print(gender_smoke)
say("Percent in each smoking status, within gender:")
print(round(100 * prop.table(gender_smoke, margin = 1), 1))

gs_expected <- outer(rowSums(gender_smoke), colSums(gender_smoke)) / sum(gender_smoke)
gs_chi <- sum((gender_smoke - gs_expected)^2 / gs_expected)
gs_df <- (nrow(gender_smoke) - 1) * (ncol(gender_smoke) - 1)
gs_p <- pchisq(gs_chi, gs_df, lower.tail = FALSE)
gs_p_text <- if (gs_p < 0.001) "p < 0.001" else paste0("p = ", sprintf("%.3f", gs_p))
say(
  "Chi-square = ", round(gs_chi, 2),
  ", df = ", gs_df,
  ", ", gs_p_text
)
say("With thousands of rows, a visible gap in the percentages is enough to reject H0.")
say("Read the percentages for the size of the gap. The p-value will not tell you the size.")

gender_race <- table(cohort$gender, cohort$race)
gr_expected <- outer(rowSums(gender_race), colSums(gender_race)) / sum(gender_race)
gr_chi <- sum((gender_race - gr_expected)^2 / gr_expected)
gr_df <- (nrow(gender_race) - 1) * (ncol(gender_race) - 1)
gr_p <- pchisq(gr_chi, gr_df, lower.tail = FALSE)
say(
  "Gender by race: chi-square = ", round(gr_chi, 2),
  ", df = ", gr_df,
  ", p = ", round(gr_p, 3),
  ". This one should not reject at alpha 0.05, because those two columns were generated independently."
)

# ---------------------------------------------------------------------------
# The curve for Example 1
# ---------------------------------------------------------------------------
# Slate line: critical value at alpha 0.05.
# Rose line:  the statistic from Example 1.
# The p-value is the area under the curve to the right of the rose line.

draw_curve <- function() {
  curve(
    dchisq(x, df = df),
    from = 0,
    to = max(12, chi_sq * 1.15),
    ylim = c(0, dchisq(critical_05, df) * 6),
    xlab = "Chi-square value",
    ylab = "Density",
    main = "Example 1 if H0 is true (df = 1)",
    col = "#2c3338",
    lwd = 2,
    bty = "l"
  )
  abline(v = critical_05, col = "#3d5366", lwd = 2)
  abline(v = chi_sq, col = "#CC6677", lwd = 2, lty = 2)
  legend(
    "topright",
    legend = c("Critical value", "Statistic from the table"),
    col = c("#3d5366", "#CC6677"),
    lty = c(1, 2),
    lwd = 2,
    bty = "n"
  )
}

say("")
say("Slate line = critical value at alpha 0.05. Rose line = the statistic.")
if (interactive()) {
  say("The plot window shows the chi-square curve for Example 1.")
  draw_curve()
} else {
  curve_path <- file.path(root, "example-1-curve.png")
  png(curve_path, width = 800, height = 520, res = 110)
  draw_curve()
  dev.off()
  say("The curve for Example 1 was saved as ", curve_path)
}
say("Next, run the app and change the variables yourself:  shiny::runApp()")

