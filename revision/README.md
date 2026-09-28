# Journal revision workflow

This directory contains the reproducible analysis and document-processing code used for the journal revision. It belongs on the `journal_revision` branch because it is specific to the review process rather than the general analysis package.

Source documents, copied data, generated outputs, working notes, scratch files and the resubmission package are kept locally and backed up separately. They are intentionally ignored by Git.

## Local artefacts

The current clean and tracked manuscripts, supplements, response letter and editor-only title page are in the ignored `resubmission/` directory. Intermediate analysis results remain in the ignored `outputs/` directory. Supplementary sensitivity analyses are presented as Text S1 and Tables S3–S9.

## Organisation

- `docs/`: supplied originals, read-only throughout this work; ignored by Git.
- `data/`: 46 copied input CSVs plus derived CSVs; ignored by Git.
- `scripts/`: numbered workflow below; all writes remain within `revision/`.
- `src/docx_revision.py`: narrowly scoped Word XML editing that retains reference fields, equations, tables and original media.
- `outputs/`: generated documents, original-style figures, line mappings, analysis tables and verification records; ignored by Git.
- `scratch/`: temporary extraction/rendering, model objects and live logs; ignored by Git.
- `notes/`: local scientific decisions and troubleshooting; ignored by Git.

Python follows Google-style conventions where practical, with explicit inputs, short functions and no new dependencies in the original project environment. R uses the original lme4 workflow. The Python dependency lock is `uv.lock`; exact R versions are recorded in `outputs/analysis/R_session.txt` (R 4.5.1; lme4 2.0.6 in this run).

## Reproduce from the repository root

Set `<DATA_DIRECTORY>` to the original data directory locally. Do not publish private paths.

```bash
uv sync --project revision --frozen
Rscript revision/scripts/setup_r.R > revision/scratch/r_install.log 2>&1
revision/.venv/bin/python revision/scripts/prepare_data.py --source <DATA_DIRECTORY>
revision/.venv/bin/python revision/scripts/richness_sensitivity.py
revision/.venv/bin/python revision/scripts/additional_checks.py
timeout 1200 Rscript revision/scripts/fit_sensitivities.R > revision/scratch/model_progress.log 2>&1
timeout 600 Rscript revision/scripts/fit_corrections.R > revision/scratch/corrections_progress.log 2>&1
Rscript revision/scripts/audit_final_fits.R
revision/.venv/bin/python revision/scripts/summarise_analysis.py
revision/.venv/bin/python revision/scripts/register_comments.py
revision/.venv/bin/python revision/scripts/plot_revisions.py
revision/.venv/bin/python revision/scripts/plan_edits.py
revision/.venv/bin/python revision/scripts/build_manuscript.py
revision/.venv/bin/python revision/scripts/build_supplement.py
```

Progress can be monitored with `tail -f revision/scratch/model_progress.log` in a separate terminal. Do not execute the original analysis scripts: several contain hard-coded output locations.

Render a **copy** of the clean manuscript with LibreOffice to `scratch/render/`; copy its PDF to `outputs/Manuscript_line_reference.pdf`, then run:

```bash
revision/.venv/bin/python revision/scripts/map_lines.py
revision/.venv/bin/python revision/scripts/build_responses.py
revision/.venv/bin/python revision/scripts/build_summary.py
revision/.venv/bin/python revision/scripts/verify_outputs.py
```

The model-to-manuscript prose replacements are intentionally explicit in `plan_edits.py` so scientific text is reviewable. The submitted documents were subsequently refined manually, so the current files in `resubmission/` are authoritative. Reconcile those edits before rebuilding documents from these scripts.

## Evidence and limits

- `outputs/input_manifest.json`: relative filenames, hashes and sizes for copied CSVs.
- `outputs/all_model_coefficients.csv`: estimates, intervals, p-values and scenario labels.
- `outputs/final_fit_diagnostics.csv`: **final** optimiser status, distinct from intermediate warnings retained in per-model logs. Finalised model diagnostics are recorded for review; several random-effect variances are on the boundary.
- `outputs/shrimp_inclusion_audit.csv`: exact reconciliation of all original richness values.
- `outputs/indonesia_age_site_permutation.csv`: six independent Indonesian site means, all 720 permutations.
- `outputs/covariate_design_audit.json`: rank deficiency of country plus star count on the matched restored subset.
- `outputs/verification.json`: actual integrity and document checks.

Most scientific changes were completed from CSVs only. Identifying the sources of Mexican Scrape false positives would require annotation notes or read-only inspection of the relevant audio; no source audio was changed or reprocessed. Thesis factual differences were not automatically imported. Reviewer wording is preserved except PDF whitespace and omission of contact/signature/navigation material; administrative instructions are consolidated. Lettered parts inherit their parent comment number.
