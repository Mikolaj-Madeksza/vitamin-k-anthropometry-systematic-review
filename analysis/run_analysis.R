suppressPackageStartupMessages(library(metafor))
suppressPackageStartupMessages(library(readxl))
stopifnot(requireNamespace("ragg", quietly = TRUE))
script <- sub("^--file=", "", grep("^--file=", commandArgs(), value=TRUE)[1])
analysis_dir <- dirname(normalizePath(script))
root <- dirname(analysis_dir)
invisible(capture.output(sys.source(file.path(analysis_dir,"prepare_data.R"),new.env())))
output_dir <- file.path(root,"results")
figure_dir <- file.path(root,"figures")
dir.create(output_dir,showWarnings=FALSE)
dir.create(figure_dir,showWarnings=FALSE)
read_data <- function(name) read.csv(file.path(root,"data",name),stringsAsFactors=FALSE,na.strings="")
d <- read_data("analysis_data.csv")
s <- read_data("sensitivity_data.csv")
decisions <- read_data("study_outcomes.csv")
studies <- as.data.frame(read_excel(file.path(root,"data/extraction.xlsx"),sheet="Studies"))
outcomes <- c(weight__kg="Body weight",bmi__kg_per_m2="BMI",body_fat__percentage_points="Total body fat")
units <- c(weight__kg="kg",bmi__kg_per_m2="kg/m²",body_fat__percentage_points="percentage points")
foods <- c("Moschonis_2011","Zhang_2023","Graff_2016")
label <- function(x) gsub("_"," ",sub("Ronn_","Rønn_",x))
fmt <- function(x,digits=2) formatC(x,format="f",digits=digits)
ci_text <- function(y,lo,hi) paste0(fmt(y)," [",fmt(lo),", ",fmt(hi),"]")
write_data <- function(x,name) write.csv(x,file.path(output_dir,name),row.names=FALSE,na="",fileEncoding="UTF-8")
fit_model <- function(x,test="adhoc",method="REML") {
  stopifnot(!anyDuplicated(x$study_id),all(is.finite(x$variance)),all(x$variance>0))
  rma(yi=effect,vi=variance,data=x,method=method,test=test,slab=x$study_id)
}
summarize_model <- function(x,scenario,test="adhoc",method="REML") {
  key <- unique(x$synthesis_id); stopifnot(length(key)==1)
  if(nrow(x)<2) return(data.frame(outcome=outcomes[key],scenario=scenario,k=nrow(x),n=sum(x$n_k+x$n_control),
    effect=x$effect,se=x$se,ci_low=x$ci_low,ci_high=x$ci_high,p=NA,I2=NA,tau2=NA,Q=NA,Q_p=NA,
    pi_low=NA,pi_high=NA,high_rob=nrow(x[x$rob=="High",]),high_rob_weight=NA,method="Single study; not pooled"))
  m <- fit_model(x,test,method)
  pred <- if(method=="REML" && nrow(x)>=5) predict(m) else NULL
  if(test=="adhoc") {
    w <- 1/(x$variance+m$tau2)
    q <- max(1,sum(w*(x$effect-as.numeric(m$b))^2)/(nrow(x)-1))
    stopifnot(abs(m$se-sqrt(q/sum(w)))<1e-9,
      abs(m$ci.lb-(as.numeric(m$b)-qt(.975,nrow(x)-1)*sqrt(q/sum(w))))<1e-9)
  }
  data.frame(outcome=outcomes[key],scenario=scenario,k=m$k,n=sum(x$n_k+x$n_control),effect=as.numeric(m$b),
    se=m$se,ci_low=m$ci.lb,ci_high=m$ci.ub,p=m$pval,I2=m$I2,tau2=m$tau2,Q=m$QE,Q_p=m$QEp,
    pi_low=if(is.null(pred))NA else pred$pi.lb,pi_high=if(is.null(pred))NA else pred$pi.ub,
    high_rob=sum(x$rob=="High"),high_rob_weight=sum(weights(m)[x$rob=="High"]),
    method=paste(method,test,sep="; "))
}
primary <- sensitivity <- influence_rows <- weights_rows <- list()
models <- list()
for(key in names(outcomes)) {
  x <- d[d$synthesis_id==key, ]
  stopifnot(nrow(x)==c(9,10,5)[match(key,names(outcomes))],sum(x$n_k+x$n_control)==c(749,851,277)[match(key,names(outcomes))])
  models[[key]] <- fit_model(x)
  primary[[key]] <- summarize_model(x,if(key=="body_fat__percentage_points")"Exploratory" else "Primary")
  weight <- weights(models[[key]])
  stopifnot(abs(sum(weight)-100)<1e-8)
  weights_rows[[key]] <- data.frame(outcome=unname(outcomes[key]),study_id=x$study_id,weight=as.numeric(weight),rob=x$rob)
  scenarios <- list("Conventional supplements"=x[!x$study_id %in% foods, ],"Exclude high RoB"=x[x$rob!="High", ])
  if(key %in% c("weight__kg","bmi__kg_per_m2")) {
    scenarios[["Exclude Koitaya 2009"]] <- x[x$study_id!="Koitaya_2009", ]
    scenarios[["Exclude Rahimi Sakak"]] <- x[x$study_id!="Rahimi_Sakak_2021", ]
  }
  if(key=="bmi__kg_per_m2") {
    scenarios[["Exclude Kurnatowska"]] <- x[x$study_id!="Kurnatowska_2015", ]
    scenarios[["Exclude both baseline-imbalanced trials"]] <- x[!x$study_id %in% c("Koitaya_2009","Kurnatowska_2015"), ]
  }
  replacement <- s[s$synthesis_id==key & s$scenario=="postrandomization_adjusted",names(d)]
  stopifnot(nrow(replacement)==1,replacement$study_id=="Karamzad_2020")
  scenarios[["Karamzad adjusted replacement"]] <- rbind(x[x$study_id!="Karamzad_2020", ],replacement)
  for(nm in names(scenarios)) sensitivity[[paste(key,nm)]] <- summarize_model(scenarios[[nm]],nm)
  sensitivity[[paste(key,"common")]] <- summarize_model(x,"Common-effect model","z","FE")
  sensitivity[[paste(key,"hk")]] <- summarize_model(x,"Unmodified Hartung–Knapp","knha")
  inf <- influence(models[[key]])
  for(i in seq_len(nrow(x))) {
    z <- summarize_model(x[-i, ],"Leave one out")
    influence_rows[[paste(key,i)]] <- data.frame(outcome=outcomes[key],omitted_study=x$study_id[i],
      effect=z$effect,ci_low=z$ci_low,ci_high=z$ci_high,I2=z$I2,tau2=z$tau2,
      studentized_residual=inf$inf$rstudent[i],cooks_distance=inf$inf$cook.d[i],
      leverage=inf$inf$hat[i],dfbetas=inf$dfbs$intrcpt[i],influential=inf$is.infl[i])
  }
}
primary <- do.call(rbind,primary)
sensitivity <- do.call(rbind,sensitivity)
influence_rows <- do.call(rbind,influence_rows)
weights_rows <- do.call(rbind,weights_rows)
heterogeneity_ci <- lapply(models,function(m)confint(m)$random)
primary$tau2_low <- vapply(heterogeneity_ci,function(x)x["tau^2","ci.lb"],numeric(1))
primary$tau2_high <- vapply(heterogeneity_ci,function(x)x["tau^2","ci.ub"],numeric(1))
primary$I2_low <- vapply(heterogeneity_ci,function(x)x["I^2(%)","ci.lb"],numeric(1))
primary$I2_high <- vapply(heterogeneity_ci,function(x)x["I^2(%)","ci.ub"],numeric(1))
write_data(primary,"pooled_results.csv")
write_data(sensitivity,"sensitivity_results.csv")
write_data(influence_rows,"influence.csv")
write_data(weights_rows,"study_weights.csv")
bmi_egger <- regtest(models[["bmi__kg_per_m2"]],model="lm",predictor="sei")
write_data(data.frame(outcome="BMI",k=10,test="Egger weighted regression; standard error predictor",
  statistic=bmi_egger$zval,df=bmi_egger$ddf,p=bmi_egger$pval),"small_study_effects.csv")

nonpooled <- decisions[!decisions$effect_id %in% d$effect_id[d$synthesis_id %in% names(outcomes)], ]
nonpooled$ci_low <- nonpooled$effect-qnorm(.975)*nonpooled$se
nonpooled$ci_high <- nonpooled$effect+qnorm(.975)*nonpooled$se
nonpooled$population <- studies$Population[match(nonpooled$study_id,studies$`Study ID`)]
rob <- as.data.frame(read_excel(file.path(root,"data/risk_of_bias.xlsx"),sheet="Judgements"))
nonpooled$rob <- rob$Overall[match(nonpooled$cohort,rob$Cohort)]
nonpooled$measurement <- d$measurement[match(nonpooled$effect_id,d$effect_id)]
nonpooled$sensitivity_only <- nonpooled$status=="Sensitivity"
write_data(nonpooled,"nonpooled_results.csv")

individual <- d[!d$synthesis_id %in% names(outcomes),
 c("study_id","outcome","time","comparison","unit","n_k","n_control","effect","ci_low","ci_high","estimate_type","source")]
individual$outcome[individual$study_id=="Graff_2016" & individual$outcome=="Body fat"] <- "Soft-tissue fat"
individual$outcome[individual$outcome=="Muscle mass"] <- "Muscle mass, device-defined"
individual$unit[individual$unit=="difference in relative percent change"] <- "difference in relative change, percentage points"
individual$estimate_type <- paste("Calculated",tolower(individual$estimate_type),"difference")
reported <- as.data.frame(read_excel(file.path(root,"data/extraction.xlsx"),sheet="Reported effects"))
for(i in which(reported$`Study ID`=="Binkley_2009")) {
  individual <- rbind(individual,data.frame(study_id=reported$`Study ID`[i],outcome="Weight",
    time=reported$Time[i],comparison=paste(reported$`K arm`[i],"versus placebo"),
    unit="relative effect, %",n_k=reported$`K N`[i],n_control=reported$`Control N`[i],
    effect=reported$`K minus control`[i],ci_low=reported$`CI low harmonized`[i],
    ci_high=reported$`CI high harmonized`[i],estimate_type="Reported geometric relative effect",source=reported$Source[i]))
}
individual <- individual[order(individual$unit=="difference in relative change, percentage points" |
  individual$unit=="relative effect, %",individual$outcome,individual$study_id,individual$comparison), ]
stopifnot(nrow(individual)==20L)
write_data(individual,"individual_effects.csv")

draw_forest <- function(x,unit,model) {
  k <- nrow(x)
  lim <- range(c(x$ci_low,x$ci_high,0))
  padding <- .15*diff(lim)
  ticks <- pretty(range(c(lim,-padding,padding)),n=4)
  bmi <- unique(x$synthesis_id)=="bmi__kg_per_m2"
  if(bmi) ticks <- seq(-4,4,2)
  limits <- range(ticks)
  plot_left <- .31
  plot_right <- if(bmi).55 else .61
  weight_x <- if(bmi).61 else .67
  estimate_left <- if(bmi).68 else .74
  xpos <- function(v) plot_left+(v-limits[1])/diff(limits)*(plot_right-plot_left)
  par(mai=c(.38,.09,.05,.09),family="Arial",cex=.85)
  plot.new()
  plot.window(c(0,par("pin")[1]/6.02),c(-.65,k+1.3),xaxs="i",yaxs="i")
  rows <- k:1
  segments(xpos(0),-.2,xpos(0),k+.4,col="grey65",lty=2)
  segments(xpos(x$ci_low),rows,xpos(x$ci_high),rows,lwd=1)
  weight <- weights(model)
  points(xpos(x$effect),rows,pch=15,cex=.45+.95*sqrt(weight/max(weight)))
  text(0,rows,label(x$study_id),adj=0)
  text(.28,rows,paste(x$n_k,x$n_control,sep="/"),adj=1)
  text(estimate_left,rows,ci_text(x$effect,x$ci_low,x$ci_high),adj=0,xpd=TRUE)
  text(weight_x,rows,paste0(fmt(weight,1),"%"))
  text(0,k+1,"Study",adj=0,font=2)
  text(.28,k+1,"n (K/control)",adj=1,font=2)
  text(weight_x,k+1,"Weight",font=2)
  text(estimate_left,k+1,"MD [95% CI]",adj=0,font=2)
  mu <- as.numeric(model$b)
  polygon(xpos(c(model$ci.lb,mu,model$ci.ub,mu)),c(0,.22,0,-.22),col="grey35",border="black")
  text(0,0,"Random effects",adj=0,font=2)
  text(.28,0,paste(sum(x$n_k),sum(x$n_control),sep="/"),adj=1,font=2)
  text(estimate_left,0,ci_text(mu,model$ci.lb,model$ci.ub),adj=0,font=2)
  axis(1,at=xpos(ticks),labels=ticks,pos=-.55,cex.axis=.88,tcl=-.2,mgp=c(3,.45,0))
  mtext(paste0("Mean difference (",unit,"); vitamin K − control"),side=1,line=1.4,
        at=(plot_left+plot_right)/2,cex=.8)
}
save_figure <- function(name,width,height,draw) {
  pdf_file <- file.path(figure_dir,paste0(name,".pdf"))
  if(capabilities("aqua")) quartz(type="pdf",file=pdf_file,width=width,height=height,family="Arial",pointsize=10)
  else cairo_pdf(pdf_file,width=width,height=height,family="Arial",pointsize=10)
  draw(); dev.off()
  ragg::agg_png(file.path(figure_dir,paste0(name,".png")),width=width,height=height,units="in",res=300,pointsize=10)
  draw(); dev.off()
}
for(key in names(outcomes)) {
  x <- d[d$synthesis_id==key, ]
  save_figure(paste0("forest_",c("weight","bmi","body_fat")[match(key,names(outcomes))]),if(key=="bmi__kg_per_m2")5.25 else 5.7,.85+.25*nrow(x),
    function()draw_forest(x,units[key],models[[key]]))
}
np <- d[!d$synthesis_id %in% names(outcomes) & !grepl("relative",d$unit), ]
np$title <- np$outcome
np$title[np$synthesis_id=="body_fat_soft_tissue__percentage_points"] <- "Body fat, soft-tissue percentage"
np$title[np$synthesis_id=="muscle_mass_device_defined__kg"] <- "Muscle mass, device-defined"
np <- np[order(match(np$title,c("Fat mass","Lean mass","Fat-free mass","Muscle mass, device-defined",
                              "Body fat, soft-tissue percentage","Waist circumference","Hip circumference","Waist-to-hip ratio")),np$study_id), ]
rob_domains <- c("Randomization","Deviations from intended intervention","Missing outcome data",
                 "Outcome measurement","Selection of reported result","Overall")
rob_levels <- c("Low","Some concerns","High")
assessed <- rob[rob$Overall %in% rob_levels, ]
trial_ids <- unique(assessed$`Study ID`)
rob_matrix <- t(vapply(trial_ids,function(id) vapply(rob_domains,function(domain)
  max(match(assessed[assessed$`Study ID`==id,domain],rob_levels)),numeric(1)),numeric(6)))
rownames(rob_matrix) <- trial_ids
colnames(rob_matrix) <- rob_domains
write_data(data.frame(Study=label(trial_ids),apply(rob_matrix,2,function(v)rob_levels[v]),check.names=FALSE),"risk_of_bias_summary.csv")
rob_colours <- c("#6BAD7A","#E5C65A","#D96A67")
save_figure("risk_of_bias_trials",5.1,5.25,function(){
  par(mai=c(.04,.04,.06,.04),family="Arial",cex=.88)
  plot.new();plot.window(c(0,6),c(-.2,length(trial_ids)+1.5),xaxs="i",yaxs="i")
  x <- seq(1.55,5.68,length.out=6); y <- rev(seq_along(trial_ids))
  text(0,y,label(trial_ids),adj=0)
  text(x,length(trial_ids)+1,c("D1","D2","D3","D4","D5","Overall"),font=2)
  for(j in seq_len(6)) {
    points(rep(x[j],length(y)),y,pch=21,bg=rob_colours[rob_matrix[,j]],col="grey35",cex=2.65)
    text(rep(x[j],length(y)),y,c("+","!","−")[rob_matrix[,j]],cex=1.05)
  }
  widths <- strwidth(rob_levels,cex=.85)+.28
  gap <- (diff(range(x))+.2-sum(widths))/2
  starts <- x[1]-.1+c(0,cumsum(widths+gap)[1:2])
  points(starts,rep(.05,3),pch=21,bg=rob_colours,col="grey35",cex=1.65)
  text(starts+.17,.05,rob_levels,adj=0,cex=.85)
})
session <- sessionInfo()
session$BLAS <- basename(session$BLAS)
session$LAPACK <- basename(session$LAPACK)
capture.output(print(session),file=file.path(output_dir,"session_info.txt"))

grade <- data.frame(
  outcome=c("Body weight","BMI","Total body fat","Fat mass","Waist circumference","Hip circumference"),
  k=c(9,10,5,2,2,2),n=c(749,851,277,107,105,105),
  risk_of_bias=c(1,1,2,1,2,2),inconsistency=0,indirectness=0,
  imprecision=c(0,0,1,1,1,1),publication_bias=c(1,1,1,0,0,0),
  certainty=c("Low","Low","Very low","Low","Very low","Very low"),
  rob_reason=c(
    sprintf("Four of nine trials are High RoB and contribute %.1f%% of primary weight; missing outcomes affect influential trials.",primary$high_rob_weight[1]),
    sprintf("Four of ten trials are High RoB and contribute %.1f%% of primary weight; complete-case and incompletely reported analyses remain concerns.",primary$high_rob_weight[2]),
    sprintf("Four of five trials are High RoB and contribute %.1f%% of primary weight. The only remaining trial has Some concerns; missing outcomes dominate the evidence.",primary$high_rob_weight[3]),
    "One trial has High RoB from missing outcomes; the other has Some concerns about reporting and an approximately balanced vitamin D cointervention.",
    "Both contributing trials have High RoB; missing outcomes and deviations from intended intervention undermine the available contrasts.",
    "Both contributing trials have High RoB; missing outcomes and deviations from intended intervention undermine the available contrasts."),
  inconsistency_reason=c(
    "No downgrade: modest estimated heterogeneity and overlapping CIs; leave-one-out results do not materially change interpretation. Heterogeneity remains uncertain.",
    "No downgrade: modest estimated heterogeneity and overlapping CIs; baseline-imbalance and leave-one-out checks do not materially change interpretation.",
    "No downgrade: point estimates are close and CIs overlap. An estimated tau-squared of zero does not establish homogeneous effects.",
    "No downgrade: both point estimates indicate lower fat mass and CIs overlap; clinical differences preclude a useful pooled summary.",
    "No downgrade: point estimates differ in direction but have broad overlapping CIs; uncertainty is addressed under imprecision.",
    "No downgrade: both point estimates indicate lower hip circumference and CIs overlap."),
  indirectness_reason="No downgrade for the broad adult review question. Populations, formulations and duration vary; results should not be generalized to every subgroup or to patient-important health benefits.",
  precision_reason=c(
    "No downgrade for the average measured effect: 749 participants and a CI of -0.99 to +0.21 kg constrain its absolute magnitude. Small effects remain possible; this is not an equivalence claim.",
    "No downgrade for the average measured effect: 851 participants and a CI of -0.40 to +0.16 kg/m2 constrain its absolute magnitude. Small effects remain possible; this is not an equivalence claim.",
    "Serious: 277 participants; the CI permits about one percentage point lower or higher body fat and widens substantially when foods or individual studies are omitted. No cross-population MID is established.",
    "Serious: only 107 participants; individual CIs include no difference, lower fat mass, and in the smaller trial increases exceeding 2 kg. No common pooled effect is estimated.",
    "Serious: only 105 participants; individual CIs permit several centimetres of reduction or increase. No common pooled effect is estimated.",
    "Serious: only 105 participants; individual CIs permit several centimetres of reduction and little difference or increases. No common pooled effect is estimated."),
  publication_bias_reason=c(
    "Serious concern about selective nonreporting: confirmed measured weight outcomes are unavailable from Bolton-Smith, Braam, Shishavan and Zwakenberg; other trials lack usable numerical effects. This concerns absent trial results, separate from bias within contributing results.",
    "Serious concern about selective nonreporting: measured BMI outcomes are unavailable from Braam, Shishavan and Zwakenberg; OConnor provides only qualitative findings. Egger testing has limited power and cannot resolve this missing evidence.",
    "Serious concern about selective nonreporting: TAKEOVER measured body fat without available results; Booth reports only a qualitative percentage-fat comparison, and Pena-Vega lacks usable contrast variance. Direction of missing effects is unknown.",
    "No additional downgrade: Booth cannot supply the preferred longitudinal contrast variance; the available information does not establish direction-dependent suppression. Missing evidence cannot be excluded.",
    "No additional downgrade: other available waist results have known variance/scale/denominator limitations. These limitations alone do not establish direction-dependent suppression.",
    "No additional downgrade: Zwakenberg lacks numerical follow-up hip results, but direction-dependent suppression is not established."),stringsAsFactors=FALSE)
grade$effect <- c(primary$effect,rep(NA,3))
grade$ci_low <- c(primary$ci_low,rep(NA,3));grade$ci_high <- c(primary$ci_high,rep(NA,3))
single_outcomes <- c("Lean mass","Fat-free mass","Muscle mass, device-defined",
                     "Body fat, soft-tissue percentage","Waist-to-hip ratio")
for (outcome in single_outcomes) {
  x <- np[np$title == outcome, ]
  stopifnot(nrow(x) == 1)
  food_trial <- x$study_id == "Graff_2016"
  row <- grade[4, ]
  row$outcome <- outcome
  row$k <- 1; row$n <- x$n_k + x$n_control
  row$risk_of_bias <- if(food_trial) 1 else 2
  row$imprecision <- 1; row$publication_bias <- 0
  row$certainty <- if(food_trial) "Low" else "Very low"
  row$rob_reason <- if(food_trial)
    "Serious: the only trial has concerns about selection of the reported result, without a prespecified analysis plan; vitamin D was approximately, rather than exactly, balanced between the salmon groups." else
    "Very serious: all evidence comes from a high-risk, complete-case result in Karamzad 2020; substantial missing outcomes could bias the contrast."
  row$inconsistency_reason <- "Not assessable with one trial; no downgrade solely for the absence of replication."
  row$precision_reason <- sprintf(
    "Serious: only %d participants. The CI of %.2f to %.2f %s leaves uncertainty about the magnitude and clinical importance of the effect; no established clinically important threshold was available.",
    row$n,x$ci_low,x$ci_high,if(x$unit == "ratio") "ratio units" else x$unit)
  row$publication_bias_reason <- "No additional downgrade: selective absence of comparable results was not established. One trial cannot support an asymmetry test, and missing evidence cannot be excluded."
  row$effect <- x$effect;row$ci_low <- x$ci_low;row$ci_high <- x$ci_high
  grade <- rbind(grade,row)
}
levels <- c("Very low","Low","Moderate","High")
stopifnot(identical(grade$certainty,levels[pmax(1,4-rowSums(grade[,c("risk_of_bias","inconsistency","indirectness","imprecision","publication_bias")]))]))
write_data(grade,"grade.csv")

print(primary, row.names=FALSE)
