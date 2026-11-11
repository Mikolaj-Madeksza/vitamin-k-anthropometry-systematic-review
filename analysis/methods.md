# Analysis methods

Statistical methods and effect-selection rules.

## Analyses

- Primary: body weight (kg) and BMI (kg/m²).
- Exploratory: total body-fat percentage, expressed as a percentage-point difference.
- Remaining native-unit outcomes: individual estimates and structured narrative synthesis, without pooled summaries. Soft-tissue fat percentage, lean mass, fat-free mass and differently defined muscle measures remain separate. Interpretable relative-change effects are reported separately from native-unit effects.

Use the selected end-of-treatment or latest available on-treatment estimate, one comparison per trial and outcome. `data/study_outcomes.csv` records selection decisions for overlapping reports, combined intervention arms, missing results and sensitivity-only estimates. Outcome-specific trial and participant counts are in `results/pooled_results.csv`; participant counts must not be summed across outcomes.

## Statistical methods

Use inverse-variance random-effects mean differences, REML heterogeneity estimation, and modified Hartung–Knapp inference. In `metafor`, `test="adhoc"` prevents the Hartung–Knapp adjustment from reducing the conventional standard error; confidence intervals use k−1 degrees of freedom. Report 95% CIs, I², τ² and Q, alongside the clinical differences between studies. Report model-based prediction intervals for the three analyses, with explicit caution about few studies and uncertain heterogeneity.

Check leave-one-out estimates, studentized residuals, Cook's distances, leverage and DFBETAS. Investigate influence without automatically excluding trials. Compare with conventional common-effect estimates and unmodified Hartung–Knapp intervals. No formal subgroup analyses or meta-regression: the small, uneven groups do not support reliable effect-modification inference.

## Sensitivity analyses

- Exclude Koitaya 2009 from weight and BMI; exclude Kurnatowska from BMI; exclude both from BMI.
- Restrict to conventional supplements, omitting fortified-food interventions.
- Exclude trials at high risk of bias; where only one remains, report that study without pooling.
- Omit Rahimi Sakak from weight and BMI because its supplied dispersion label is uncertain.
- Replace Karamzad's primary effects with its adjusted effects, never add both.
- Keep Tarkesh's unverified-denominator estimates and the other reconstructed sensitivity-only results out of these pooled models. Booth's endpoint estimates remain nonpooled sensitivity evidence.

Use exploratory Egger regression for BMI only (10 trials). Do not interpret a nonsignificant test as absence of missing-evidence bias. For weight and body fat, discuss missing evidence without an asymmetry test.

## Nonpooled evidence and certainty

Follow SWiM reporting principles: group by outcome definition and unit; show individual effects and CIs where available; compare populations, interventions, measurements and RoB; retain unavailable results explicitly. Do not vote-count statistical significance or infer zero effects from nonsignificant statements.

Assess GRADE for all 11 reported native-unit outcomes, starting at High for randomized evidence. Consider RoB contribution, inconsistency, applicability, precision and missing evidence separately, with reasons for each downgrade. No validated cross-population minimally important difference is specified; interpret continuous effects and uncertainty without inventing an equivalence margin. Consider information size and clinically relevant uncertainty rather than crossing zero alone. Do not grade statistical significance as certainty.

The R script generates pooled forest plots and a trial-level risk-of-bias figure. Nonpooled effects are supplied as tables in `results/`.

Sources: [metafor model documentation](https://wviechtb.github.io/metafor/reference/rma.uni.html); [Cochrane GRADE guidance](https://www.cochrane.org/authors/handbooks-and-manuals/handbook/current/chapter-14); [SWiM](https://doi.org/10.1136/bmj.l6890).
