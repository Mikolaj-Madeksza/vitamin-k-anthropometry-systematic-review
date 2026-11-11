# Prepare selected study effects; no meta-analysis is fitted.
# Run: Rscript analysis/prepare_data.R
suppressPackageStartupMessages(library(readxl))
suppressPackageStartupMessages(library(metafor))
script <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1])
analysis_dir <- dirname(normalizePath(script))
root <- dirname(analysis_dir)
out <- file.path(root, "data")
input <- file.path(root, "data", "extraction.xlsx")
robfile <- file.path(root, "data", "risk_of_bias.xlsx")
before <- tools::md5sum(c(input, robfile))
read_sheet <- function(file, sheet) as.data.frame(read_excel(file, sheet, col_types = "text"), stringsAsFactors = FALSE)
d <- read_sheet(input, "Outcome data")
e <- read_sheet(input, "Reported effects")
studies <- read_sheet(input, "Studies")
arms <- read_sheet(input, "Arms")
rob <- read_sheet(robfile, "Judgements")
names(d) <- c("study","cohort","outcome","arm","time","type","n","mean","sd","se","lo","hi","median","q1","q3","unit","origin","source","notes","measurement","subgroup")
names(e) <- c("study","cohort","outcome","k","control","time","unit","nk","nc","original","lo_original","hi_original","se","direction","sign","effect","lo","hi","analysis","source","notes")
for (v in c("n","mean","sd","se","lo","hi","median","q1","q3")) d[[v]] <- as.numeric(d[[v]])
for (v in c("nk","nc","original","lo_original","hi_original","se","sign","effect","lo","hi")) e[[v]] <- as.numeric(e[[v]])
d$row <- seq_len(nrow(d)) + 1L
e$row <- seq_len(nrow(e)) + 1L
stopifnot(nrow(studies) == 26L, nrow(d) == 290L, nrow(e) == 13L)
join <- function(x) paste(unique(x[!is.na(x) & nzchar(x)]), collapse = "; ")
slug <- function(x) {
  x <- gsub("²", "2", gsub("/", "_per_", x, fixed=TRUE), fixed=TRUE)
  gsub("(^_|_$)", "", gsub("[^a-z0-9]+", "_", tolower(x)))
}
normal <- qnorm(.975)
results <- list()

# Combine independent intervention arms before calculating the comparison.
combine_arms <- function(x) {
  stopifnot(all(is.finite(x$n)), all(x$n > 1), all(is.finite(x$mean)), all(is.finite(x$sd)), all(x$sd >= 0))
  if (nrow(x) == 1) return(c(n=x$n,mean=x$mean,sd=x$sd))
  n <- sum(x$n)
  m <- sum(x$n * x$mean) / n
  s <- sqrt(sum((x$n - 1) * x$sd^2 + x$n * (x$mean - m)^2) / (n - 1))
  c(n = n, mean = m, sd = s)
}

make_result <- function(x, effect, se, status = "Primary", decision = "", synthesis = NULL,
                        values = rep(NA_real_, 6), comparison = NULL, type = NULL,
                        scenario = "", source_rows = NULL) {
  stopifnot(length(unique(x$study)) == 1, length(unique(x$outcome)) == 1,
            length(unique(x$time)) == 1, length(unique(x$unit)) == 1)
  unit <- x$unit[1]
  if (unit == "%") unit <- "percentage points"
  if (unit == "% change from baseline") unit <- "difference in relative percent change"
  if (is.null(synthesis)) synthesis <- paste(slug(x$outcome[1]), slug(unit), sep = "__")
  r <- rob[rob$Cohort == x$cohort[1], , drop = FALSE]
  stopifnot(nrow(r) == 1)
  id <- paste(x$study[1], synthesis, sep = "__")
  if (nzchar(scenario)) id <- paste(id, scenario, sep = "__")
  data.frame(effect_id = id, study_id = x$study[1], cohort = x$cohort[1], outcome = x$outcome[1],
    synthesis_id = synthesis, time = x$time[1],
    comparison = if (is.null(comparison)) paste(join(x$arm[seq_len(nrow(x)-1)]), "versus", tail(x$arm, 1)) else comparison,
    estimate_type = if (is.null(type)) x$type[1] else type, unit = unit,
    n_k = values[1], n_control = values[4], mean_k = values[2], sd_k = values[3], mean_control = values[5], sd_control = values[6],
    effect = effect, se = se, variance = se^2,
    ci_low = effect - normal * se, ci_high = effect + normal * se,
    rob = r$Overall, measurement = join(x$measurement), status = status, scenario = scenario,
    source = join(x$source), source_rows = if (is.null(source_rows)) paste("Outcome data", paste(x$row, collapse = ",")) else source_rows,
    decision = decision, stringsAsFactors = FALSE)
}

add_pair <- function(study, outcome, krows, crows, status = "Primary", decision = "",
                     synthesis = NULL, scenario = "") {
  x <- d[match(c(krows, crows), d$row), , drop = FALSE]
  stopifnot(all(x$study == study), all(x$outcome == outcome), all(x$type != "Baseline"),
            length(unique(x$type)) == 1, length(unique(x$cohort)) == 1,
            !anyDuplicated(x$row))
  k <- combine_arms(x[seq_along(krows), , drop = FALSE])
  c <- combine_arms(x[length(krows) + seq_along(crows), , drop = FALSE])
  z <- escalc(measure = "MD", m1i = k["mean"], sd1i = k["sd"], n1i = k["n"],
              m2i = c["mean"], sd2i = c["sd"], n2i = c["n"])
  stopifnot(isTRUE(all.equal(as.numeric(z$vi), unname(k["sd"]^2/k["n"] + c["sd"]^2/c["n"]))))
  comp <- paste(join(x$arm[seq_along(krows)]), "versus", join(x$arm[length(krows) + seq_along(crows)]))
  res <- make_result(x, as.numeric(z$yi), sqrt(z$vi), status, decision, synthesis,
                     unname(c(k,c)), comp, scenario = scenario)
  results[[length(results)+1L]] <<- res
}

# Table 1 supplies arm CIs as well as nominal N=121/122. Its CIs imply fewer
# observations than the nominal N. Use the published uncertainty directly.
# Columns: K lower/upper, placebo lower/upper; outcome order follows rows 18:29.
bittner_ci <- rbind(c(-1.08,.16,-1.04,.53),c(-1.29,-.03,-1.25,.38),
  c(-1.25,1.79,-1.02,2.62),c(-.75,.12,-1.31,.76),
  c(-.09,6.46,-5.21,2.06),c(-1.15,.58,-1.73,.21))
for (i in 1:6) {
  rr <- 18+2*(i-1); x <- d[match(c(rr,rr+1),d$row), ]
  stopifnot(all(x$study=="Bittner_2026"),all(x$time=="12 months"))
  ci <- bittner_ci[i, ]
  stopifnot(abs(mean(ci[1:2])-x$mean[1])<=.011,abs(mean(ci[3:4])-x$mean[2])<=.011)
  se <- sqrt(((ci[2]-ci[1])/(2*normal))^2+((ci[4]-ci[3])/(2*normal))^2)
  results[[length(results)+1L]] <- make_result(x,x$mean[1]-x$mean[2],se,
    values=c(NA,x$mean[1],NA,NA,x$mean[2],NA),
    decision="SE derived directly from published arm 95% CIs using a normal approximation. Table labels N=121/122, but CI widths imply smaller effective Ns; analysis Ns left blank. Relative changes only, not absolute units. Omit in a missing-data-uncertainty sensitivity.",
    source_rows=paste("Outcome data",paste(x$row,collapse=","),"and Bittner 2026 Table 1 arm 95% CIs"))
}

# Independent sex strata are combined within this trial, not entered as two trials.
for (spec in list(list("Appendicular lean mass", c(31,34,37,40)), list("Fat mass", c(43,46,49,52)))) {
  x <- d[match(spec[[2]], d$row), ]
  stopifnot(all(x$study == "Booth_2008"), all(x$outcome == spec[[1]]), all(x$time == "36 months"))
  ses <- (x$hi - x$lo) / (2 * normal)
  y <- c(x$mean[1]-x$mean[2], x$mean[3]-x$mean[4])
  v <- c(sum(ses[1:2]^2), sum(ses[3:4]^2))
  effect <- sum(y/v) / sum(1/v)
  results[[length(results)+1L]] <- make_result(x, effect, sqrt(1/sum(1/v)), status = "Sensitivity", scenario = "unadjusted_booth_endpoints",
    decision = "Sensitivity only: unadjusted digitized endpoints largely retain baseline body-composition imbalance. Independent women/men contrasts combined within trial; SE from graph CIs. Prefer adjusted longitudinal effects, but no coefficients or contrast SE/CI recovered. Interaction P values are not SEs for 36-month change contrasts. Sex-specific endpoint Ns unavailable.",
    comparison = "K1 versus matched multivitamin/Ca/D control; women and men combined", type = "Endpoint; sex-stratified mean difference")
}

add_pair("Graff_2016", "Body fat", 65, 67,
  synthesis = "body_fat_soft_tissue__percentage_points",
  decision = "Matched salmon arms; high D in both (114 versus 105 micrograms/week), common calcium. Body fat is percent soft tissue, kept separate from percent total body mass. Omit in strict cointervention sensitivity.")
add_pair("Graff_2016", "Fat mass", 69, 71,
  decision = "Matched high-D salmon arms and common calcium; D is approximately, not exactly, balanced (114 versus 105 micrograms/week). Omit in strict cointervention sensitivity.")
add_pair("Graff_2016", "Lean mass", 73, 75,
  decision = "Matched high-D salmon arms and common calcium; D is approximately, not exactly, balanced (114 versus 105 micrograms/week). Omit in strict cointervention sensitivity.")

for (rr in seq(77,109,4)) add_pair("Karamzad_2020", d$outcome[d$row == rr], rr, rr+2,
  synthesis = if (rr == 109) "muscle_mass_device_defined__kg" else NULL,
  decision = "Unadjusted endpoint in 23/22 completers. Published adjusted models include post-randomization dietary vitamin K change, so they are sensitivity estimates. Dedicated composition report preferred over conflicting companion. RoB reviewed for these endpoint comparisons: High because missing outcome data remain unresolved.")

add_pair("Knapen_2007", "Weight", 112, 113, decision = "Full 133/124 completer cohort, rather than overlapping 89/75 biomarker subset.")
add_pair("Knapen_2007", "BMI", 114, 115, decision = "Full 133/124 completer cohort, rather than overlapping 89/75 biomarker subset.")
add_pair("Knapen_2013", "Android-to-gynoid fat ratio", 138,139,
  decision = "Published full-arm numerical result preferred over discordant recombined graph; SD=reported SE 0.6 times sqrt(107). Relative percent change, not raw ratio.")
for (rr in c(116,seq(120,136,2))) add_pair("Knapen_2013", d$outcome[d$row == rr], rr,rr+1,
  status = "Sensitivity", scenario = "graph_recombined", decision = "Approximate graph values; two exhaustive, mutually exclusive MK-7 responder strata recombined (54+53). Only full 107 versus 107 comparison; no responder-only effect. Disagreement of graph with text for A/G ratio warrants sensitivity-only use of other graph outcomes.")

for (sp in list(c("Koitaya_2009",141,143), c("Koitaya_2009",145,147),
                c("Koitaya_2014",152,153), c("Koitaya_2014",158,159), c("Kurnatowska_2015",161,163),
                c("Rahimi_Sakak_2021",187,189), c("Rahimi_Sakak_2021",191,193),
                c("Rasekhi_2015",195,197), c("Rasekhi_2015",199,201), c("Rasekhi_2015",203,205),
                c("Ronn_2016",207,209), c("Ronn_2016",211,213),
                c("Volpe_2008",241,243), c("Volpe_2008",245,247))) {
  rr <- as.integer(sp[2:3]); note <- "Longest available on-treatment result; outcome-specific analysis N."
  if (sp[1] == "Rahimi_Sakak_2021") note <- "Author email verified 4 December 2025; 32/31 completers. Supplied +/- interpreted as SD, consistent with reported between-arm P values and article convention, but not explicitly labelled. Omit in dispersion-uncertainty sensitivity."
  if (sp[1] == "Ronn_2016") note <- "12-month per-protocol result (>85% adherence); trial extended to 36 months but no eligible numerical 36-month result available. Earlier on-treatment time retained, not relabelled end of 36-month trial."
  if (sp[1] == "Koitaya_2009") note <- "Baseline imbalance: K minus placebo -4.0 kg for weight and -1.6 kg/m2 for BMI; endpoint -3.7 kg and -1.5 kg/m2, versus rounded differences in changes +0.3 kg and +0.1 kg/m2. No adjusted effect or change SD recovered from Table 1/Methods. Retain endpoints provisionally; omit this trial separately and jointly with Kurnatowska in baseline-imbalance sensitivities. Do not interpret endpoint differences as weight loss caused by supplementation."
  if (sp[1] == "Kurnatowska_2015") note <- "Baseline BMI difference K minus control +1.6 kg/m2; endpoint +1.3 kg/m2; rounded difference in changes -0.3 kg/m2. Table 1 supplies no adjusted BMI effect or change SD; rounded within-arm P values and ambiguous test choice do not permit reliable recovery. Retain endpoint; omit separately and jointly with Koitaya 2009 in baseline-imbalance sensitivities."
  add_pair(sp[1], d$outcome[d$row == rr[1]], rr[1],rr[2], decision = note)
}
add_pair("Moschonis_2011", "BMI", c(165,169),167,
  decision = "K1 and MK-7 arms combined (26+24) against one matched Ca/D arm (26); pooled SD includes within- and between-arm variability. Usual-diet control omitted because cointerventions differ.")
add_pair("Moschonis_2011", "Body fat", c(171,175),173,
  decision = "K1 and MK-7 arms combined (26+24) against one matched Ca/D arm (26); pooled SD includes within- and between-arm variability. Usual-diet control omitted because cointerventions differ.")

# A reported ITT table denominator is not a verified observed endpoint N.
for (rr in seq(221,233,4)) add_pair("Tarkesh_2020", d$outcome[d$row == rr], rr,rr+2,
  status = "Sensitivity", scenario = "reported_itt_n",
  decision = "Approximation using reported ITT table N=42/42. Only 40/39 completed; effective N and missing-value handling unknown. Sensitivity only, not a verified observed-data SE; do not substitute completer Ns into these means/SDs.")

for (pair in list(c(281,288), c(282,289), c(283,290), c(284,291))) add_pair("Zhang_2023", d$outcome[d$row == pair[1]], pair[1],pair[2],
  decision = "Paired 6-month changes from public participant data, available cases; no correlation or missing-value imputation. One study ID shared with 2025 report; do not sum cohorts or treat reports as independent.")
add_pair("Zhang_2023", "Hip circumference", 270,271,
  decision = "Published 6-month endpoint mean/SE; SD=SE times sqrt(nominal N=30). Variance equals SE_K squared plus SE_control squared, independent of assumed N. Public dataset does not supply hip data. One linked study ID for 2023/2025.")
for (rr in c(274,276)) add_pair("Zhang_2023", d$outcome[d$row == rr], rr,rr+1,
  status = "Definition unresolved", synthesis = paste0("unresolved_", slug(d$outcome[d$row == rr]), "__", if (rr==274) "kg" else "kg_per_m2"),
  decision = "2025 Table S3 change mean/SD, 50/43. Effect calculable, but SM/ASM and SMI/ASMI labels conflict; no total- or appendicular-muscle pooling until clarified. Mass and index are separate outcomes, not independent estimates of one outcome. Shares study ID with 2023; exact cohort overlap unknown.")

# Replacement analyses for post-randomization-adjusted Karamzad contrasts.
# A normal CI approximation is explicit; ANCOVA residual degrees of freedom are not reported.
for (rr in setdiff(4:12,8)) {
  z <- e[e$row == rr, ]
  x <- d[d$study == z$study & d$outcome == z$outcome & d$type == "Endpoint", ][1:2, ]
  stopifnot(z$study == "Karamzad_2020", abs((z$lo+z$hi)/2-z$effect) <= .011)
  syn <- if (z$outcome == "Muscle mass") "muscle_mass_device_defined__kg" else NULL
  x$source <- z$source
  results[[length(results)+1L]] <- make_result(x,z$effect,(z$hi-z$lo)/(2*normal),"Sensitivity",
    "Replace, never add to, the unadjusted endpoint. Published model adjusts baseline outcome and post-randomization dietary vitamin K change; may alter the assignment estimand. SE is a normal approximation from rounded 95% CI; residual df unavailable. WHR contrast omitted because CI is materially asymmetric.",
    synthesis = syn, values = c(z$nk,NA,NA,z$nc,NA,NA), comparison = "MK-7 versus placebo",
    type = "Adjusted mean difference", scenario = "postrandomization_adjusted",
    source_rows = paste("Reported effects",rr))
}

all_results <- do.call(rbind, results)
all_results$baseline_imbalance <- all_results$study_id %in% c("Booth_2008","Koitaya_2009","Kurnatowska_2015")
all_results <- all_results[order(all_results$study_id, all_results$outcome, all_results$status), ]
primary <- all_results[all_results$status == "Primary", ]
sensitivity <- all_results[all_results$status == "Sensitivity", ]
sensitivity$replaces_effect_id <- primary$effect_id[match(paste(sensitivity$study_id,sensitivity$synthesis_id),paste(primary$study_id,primary$synthesis_id))]
sensitivity$use <- ifelse(is.na(sensitivity$replaces_effect_id), "Add only in named sensitivity scenario", "Replace matching primary estimate")

# One decision for every extracted outcome and every outcome covered by RoB.
ledger <- unique(d[,c("study","outcome")])
for (i in seq_len(nrow(rob))) for (oc in strsplit(rob$Outcomes[i], "; ", fixed = TRUE)[[1]])
  ledger <- rbind(ledger,data.frame(study=rob$`Study ID`[i],outcome=oc))
ledger <- unique(rbind(ledger, e[,c("study","outcome")],
  data.frame(study=c(rep("Booth_2008",4),"Dalmeijer_2012",rep("Moschonis_2011",2),"Pena-Vega_2026"),
    outcome=c("Weight","BMI","Body fat","Lean mass","Weight","Weight","Lean mass","Weight"))))
ledger <- ledger[order(ledger$study,ledger$outcome), ]
names(ledger)[1] <- "study_id"
ledger$time <- studies$Duration[match(ledger$study_id, studies$`Study ID`)]
ledger$comparison <- ledger$status <- ledger$unit <- ledger$decision <- ledger$source <- ledger$effect_id <- ""
ledger$cohort <- ledger$study_id
ledger$source_rows <- ""
ledger$effect <- ledger$se <- NA_real_
for (i in seq_len(nrow(ledger))) {
  st <- ledger$study_id[i]; oc <- ledger$outcome[i]
  dd <- d[d$study==st & d$outcome==oc, ]; aa <- arms[arms$`Study ID`==st, ]
  ee <- e[e$study==st & e$outcome==oc, ]
  ledger$source[i] <- join(c(dd$source,ee$source,studies$Source[studies$`Study ID`==st]))
  ledger$unit[i] <- if(nrow(dd)) dd$unit[1] else if(nrow(ee)) ee$unit[1] else ""
  ledger$comparison[i] <- paste(aa$Arm[1], "versus", aa$Arm[2])
  rr <- all_results[all_results$study_id==st & all_results$outcome==oc, ]
  if(nrow(rr)) {
    rr <- rr[order(match(rr$status,c("Primary","Definition unresolved","Sensitivity"))), ][1, ]
    for (v in c("time","comparison","status","unit","decision","source","effect_id","effect","se","cohort","source_rows")) ledger[[v]][i] <- rr[[v]]
  } else {
    ledger$status[i] <- "Results unavailable"
    ledger$decision[i] <- "No usable numerical follow-up result; baseline measurements are not intervention effects. Remains an included study."
    ledger$source_rows[i] <- join(c(if(nrow(dd)) paste("Outcome data",paste(dd$row,collapse=",")),if(nrow(ee)) paste("Reported effects",paste(ee$row,collapse=","))))
  }
}
set_decision <- function(st, oc = NULL, status, decision, comparison = NULL, time = NULL) {
  j <- ledger$study_id==st & (is.null(oc) | ledger$outcome %in% oc)
  ledger$status[j] <<- status; ledger$decision[j] <<- decision
  if(!is.null(comparison)) ledger$comparison[j] <<- comparison
  if(!is.null(time)) ledger$time[j] <<- time
}
set_decision("Booth_2008",c("Appendicular lean mass","Fat mass"),status="Change variance unavailable",
  decision="Reported 36-month changes retained descriptively; adjusted longitudinal treatment-by-time tests give no recoverable coefficient/SE/CI for a 36-month effect. Unadjusted endpoints are sensitivity-only because they retain baseline imbalance.")
for (spec in list(list("Appendicular lean mass",c(32,35,38,41)),list("Fat mass",c(44,47,50,53)))) {
  x <- d[match(spec[[2]],d$row), ]; j <- ledger$study_id=="Booth_2008" & ledger$outcome==spec[[1]]
  stopifnot(all(x$study=="Booth_2008"),all(x$type=="Change"))
  ledger$effect[j] <- ledger$se[j] <- NA_real_
  ledger$effect_id[j] <- ""
  ledger$comparison[j] <- "K1 versus matched control; women and men described separately"
  ledger$source_rows[j] <- paste("Outcome data",paste(x$row,collapse=","))
  ledger$decision[j] <- paste0(ledger$decision[j]," Approximate change differences (K minus control): women ",
    format(round(x$mean[1]-x$mean[2],2),nsmall=2)," kg; men ",format(round(x$mean[3]-x$mean[4],2),nsmall=2)," kg. No overall change estimate or zero effect imputed.")
}
set_decision("Ali_2023", status="Variance unavailable",
  decision="24-week endpoint means/SDs average five multiple-imputation analyses. Rubin-pooled variance unavailable; SD/sqrt(45) would omit between-imputation uncertainty. No manufactured SE or substitution of observed N=33/35 into imputed summaries.")
for (rr in seq(3,15,4)) {
  x <- d[d$row %in% c(rr,rr+2), ]
  ledger$effect[ledger$study_id=="Ali_2023" & ledger$outcome==x$outcome[1]] <- diff(rev(x$mean))
}
set_decision("Binkley_2009", status="Different estimand",
  decision="Published weight effects average 6/12-month follow-up-to-baseline geometric-mean ratios: K1 -0.87% (95% CI -1.88,0.15); MK-4 -0.70% (-1.70,0.31). Neither is a 12-month kg or arithmetic-change effect. Shared placebo; cannot combine adjusted contrasts without covariance. Retain descriptively, not as two independent trials.",
  comparison="K1 and MK-4 versus one shared placebo; common Ca/D",time="Average of 6 and 12 months")
set_decision("Dalmeijer_2012","BMI",status="Variance unavailable",
  decision="Both MK-7 doses selected together: N=22+18, weighted mean BMI change 0.158 versus -0.060 in 20 controls. Change SD/contrast covariance unavailable; do not reconstruct from separate mixed-model P values.",comparison="MK-7 180 and 360 micrograms/day combined versus placebo")
dk <- d[d$row %in% c(59,63), ]; dc <- d[d$row==61, ]
stopifnot(all(dk$study=="Dalmeijer_2012"),dc$study=="Dalmeijer_2012",all(dk$outcome=="BMI"),dc$outcome=="BMI")
ledger$effect[ledger$study_id=="Dalmeijer_2012" & ledger$outcome=="BMI"] <- weighted.mean(dk$mean,dk$n) - dc$mean
set_decision("Dalmeijer_2012","Weight",status="Results unavailable",decision="Weight measured; no numerical follow-up weight result identified.",comparison="MK-7 180 and 360 micrograms/day combined versus placebo")
set_decision("Pena-Vega_2026",c("BMI","Body fat","Fat-free mass"),status="Variance unavailable",
  decision="Difference of adjusted arm changes retained as a point estimate only. Covariance of adjusted arm means and valid between-group SE unavailable; do not add marginal variances or infer SE from the reported Bonferroni-adjusted P values.")
for (rr in c(178,182,184)) {
  x <- d[d$row %in% c(rr,rr+1), ]; j <- ledger$study_id=="Pena-Vega_2026" & ledger$outcome==x$outcome[1]
  ledger$effect[j] <- x$mean[1]-x$mean[2]
  ledger$unit[j] <- if(x$unit[1]=="%") "percentage points" else x$unit[1]
}
set_decision("Pena-Vega_2026","Waist circumference",status="Scale and variance unresolved",
  decision="Adjusted arm changes are described as log-transformed/back-transformed, yet negative values are reported. Between-group scale and covariance unclear; no pooled effect derived.")
set_decision("OConnor_2014",status="Qualitative result only",
  decision="No numerical between-group weight/BMI estimate. Within-arm nonsignificance does not imply zero intervention effect.")
set_decision("Tarkesh_2020","Waist circumference",status="Median only",
  decision="Endpoint medians (IQR): MK-7 86.25 (82.27-98.80) versus placebo 90.00 (84.20-98.80) cm. Not converted to means/SDs; missing-value handling also unclear.")
ledger$comparison[ledger$study_id=="Bolton-Smith_2007"] <- "K1 versus placebo and K1+Ca/D versus Ca/D; independent factorial strata"
ledger$comparison[ledger$study_id=="Moschonis_2011"] <- "Ca/D/K1 and Ca/D/MK-7 dairy combined versus Ca/D dairy"
ledger$comparison[ledger$study_id=="Braam_2003"] <- "K1+minerals/D versus minerals/D; unmatched placebo omitted"

# Independently reproduce the selected paired changes from the retained public data.
raw <- as.data.frame(read_excel(file.path(root,"data/sources/Zhang_2023_RCT_data.xlsx"),
  sheet="Clinical information",col_types="text"))
for (spec in list(c(6,8,281,288),c(9,11,282,289),c(24,26,283,290),c(27,29,284,291))) {
  for (g in 1:2) {
    group <- c("VK","NC")[g]
    ix <- which(raw[[1]] == group)
    change <- as.numeric(raw[ix,spec[2]]) - as.numeric(raw[ix,spec[1]])
    change <- change[is.finite(change)]
    ref <- d[d$row==spec[g+2], ]
    stopifnot(length(change)==ref$n, abs(mean(change)-ref$mean)<1e-10,
              abs(sd(change)-ref$sd)<1e-10)
  }
}

# Validate source coverage, independent comparisons, units and calculable precision.
stopifnot(setequal(unique(ledger$study_id),studies$`Study ID`), !anyDuplicated(ledger[c("study_id","outcome")]),
  !anyDuplicated(primary[c("study_id","synthesis_id")]), !anyDuplicated(all_results$effect_id),
  all(is.finite(all_results$effect)), all(is.finite(all_results$se)), all(all_results$se>0),
  all(abs(all_results$variance-all_results$se^2)<1e-12),
  all(primary$rob %in% c("Low","Some concerns","High")),
  !any(primary$cohort=="Zhang_2025_cohort"),
  !any(primary$study_id=="Booth_2008"),
  sum(sensitivity$scenario=="unadjusted_booth_endpoints")==2L,
  sum(primary$baseline_imbalance)==3L,
  all(primary$n_k[primary$study_id=="Moschonis_2011"]==50),
  all(primary$n_control[primary$study_id=="Moschonis_2011"]==26),
  all(primary$time[primary$study_id=="Koitaya_2014"]=="12 months"),
  all(primary$n_k[primary$study_id=="Knapen_2007"]==133),
  identical(before,tools::md5sum(c(input,robfile))))
for (key in unique(primary$synthesis_id)) stopifnot(length(unique(primary$unit[primary$synthesis_id==key]))==1)
for (scenario in unique(sensitivity$scenario)) {
  s <- sensitivity[sensitivity$scenario==scenario, ]
  expanded <- rbind(primary[!primary$effect_id %in% s$replaces_effect_id, ],s[,names(primary)])
  stopifnot(!anyDuplicated(expanded[c("study_id","synthesis_id")]))
}
write.csv(primary, file.path(out,"analysis_data.csv"), row.names=FALSE, na="", fileEncoding="UTF-8")
write.csv(sensitivity, file.path(out,"sensitivity_data.csv"), row.names=FALSE, na="", fileEncoding="UTF-8")
write.csv(ledger, file.path(out,"study_outcomes.csv"), row.names=FALSE, na="", fileEncoding="UTF-8")
cat(nrow(primary),"primary effects from",length(unique(primary$study_id)),"studies;",nrow(sensitivity),"sensitivity effects;",nrow(ledger),"study-outcome decisions across",length(unique(ledger$study_id)),"studies.\n")
print(table(ledger$status))
print(table(primary$synthesis_id))
