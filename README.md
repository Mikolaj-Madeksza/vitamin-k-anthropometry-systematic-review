# Vitamin K supplementation and anthropometric outcomes

Data and code for a systematic review of 26 randomized trials (PROSPERO CRD420261278785).

## Run the analysis

Install R and the required packages:

```r
install.packages(c("readxl", "metafor", "ragg"))
```

From this folder, run:

```sh
Rscript --vanilla analysis/run_analysis.R
```

This rebuilds the analysis datasets, results, and PDF/PNG figures from the supplied workbooks. Existing generated outputs are overwritten; input workbooks are unchanged. Software versions are recorded in `results/session_info.txt`. Figures request Arial; appearance may vary with available fonts.

## Contents

- `data/`: extraction and risk-of-bias workbooks, the public Zhang source dataset, outcome priorities, and generated analysis datasets.
- `analysis/`: effect derivation, statistical analysis, and methods.
- `results/`: numerical results, sensitivity analyses, diagnostics, and GRADE assessments.
- `search/` and `screening/`: search strategies, record provenance, decisions, and PRISMA counts.
- `revision_protocol.md`: the dated revision protocol; executed methods are described in `search/search_methods.md` and `analysis/methods.md`.

`record_id` links screening records to source occurrences; `study_id` groups reports from the same trial. Full-text records have `stage_1 = Included`; retracted records have `stage_1 = Not screened: retracted`. Duplicate copies equal source occurrences per record minus one.

All numerical inputs are included. Raw search exports and private author correspondence are not distributed; source references and scientific caveats are retained in the workbooks.

## License

Code and original review materials: MIT. Third-party material retains its original terms. The Zhang dataset is from [Figshare](https://doi.org/10.6084/m9.figshare.22353985.v1).
