# ODAP-2026 TODO

Scope source: **"TOR ODAP 2026 Consultant 14.09.2026.pdf"** (attached to the UN offer email of
29 Sep 2026), plus the action items from the Jorge / Patrick / Ivan call on 5 Oct 2026.
Only work named in the ToR is listed here. The old backlog in `TODO.md` is out of scope.

| Deliverable | Due | Shown by |
|---|---|---|
| **Task 1**: indirect mortality modules | **16 Dec 2026** | Running the three modules on published census and survey tables for a recent country, including in bulk at subnational level |
| **Task 2**: model life table modules + release | **9 Mar 2027** | Cloud and Docker deployment, outputs checked against Mortpak and the Division's R code, brief user docs |

Backend source for the new methods: **`IvanWilli/MortIndEst`** (cloned at `~/repositories/MortIndEst`).
Legacy model life tables: `IvanWilli/MortpakAbacusLegacyMLT`. Reference tables: `DemoToolsData::modelLTx1`.

---

## Definition of done (applies to every module task)

The ToR feature table (rows 1–6, 8, 11, 12, 14), mapped to this repo's module pattern (`module_plan.md`):

1. **Template + sample**: a worksheet-style CSV/Excel template and a sample file in `inst/extdata/`.
   Upload and download both work.
2. **Validation**: a module-specific validator returning the app's standard checks table
   (`check`, `message`, `pass` = "Pass"/"Fail"), one row per check per `.id`.
3. **Visualisation**: at least one plotly chart of the estimates, updating per group.
4. **Diagnostic feedback**: a plain-language summary of the result plus warnings
   (for example, an estimate falling outside the model life table range).
5. **Bulk**: grouped `.id` input runs per group, keeps `.id`, offers single-group vs all-groups
   download (`module_plan.md` Phase 3).
6. **Customisation**: the method parameters are exposed as controls, with sensible defaults.
7. **i18n**: every user-facing string goes through `i18n$t()` and has EN/FR/ES entries in `inst/extdata/translation.json`.
8. **Tests**: unit tests in `tests/testthat/` pass. The Chrome end-to-end run covers
   `module_plan.md` Testing Checklist Phases 1–5 (sample, upload, grouped download, language, navigation).

---

## Task list

| ID | Task | Depends on | ToR | Effort |
|---|---|---|---|---|
| T0 | Dev and test environment (R in Docker, testthat scaffold, Chrome e2e procedure) | none | (enabler) | S |
| T1 | Add MortIndEst + shared backend wrapper and error contract | T0 | Task 1 | S |
| **T2** | **Child mortality from children ever born / dead (Brass, `childmort_brass`)**: **FIRST** | T0, T1 | Task 1 | **M** |
| T3 | Adult mortality from orphanhood (`orphanhood_one`, `orphanhood_two`, HIV variants) | T1, T2 | Task 1 | L |
| T4 | Intercensal survival (`intercensal_survival`, methods + HIV/ART) | T1, T2 | Task 1 | L |
| T5 | FR/ES translations for T2–T4 | T2–T4 | row 14 | S |
| T6 | Task 1 acceptance demo: recent country, subnational bulk | T2–T5, data from Patrick | Task 1 output | M |
| T7 | Model life tables: empirical relational approach (Brass logit) | T0, T1 | Task 2 | M |
| T8 | Model life tables: classic systems (Coale-Demeny, UN) | T0; Sarah's code (Patrick) or `MortpakAbacusLegacyMLT` | Task 2 | M |
| T9 | Model life tables: HIV- and treatment-adjusted variants | T8 | Task 2 | M |
| T10 | Check outputs against Mortpak and Division R references | T2–T4, T7–T9 | Task 2 | M |
| T11 | Performance tuning + user testing with national counterparts | T6, T10 | Task 2 | M |
| T12 | Deployment: cloud + local Docker image with new modules | T2–T9 | row 13 | S |
| T13 | Brief user documentation for new modules | T2–T9 | row 9 | S |

Critical path to 16 Dec: T0 → T1 → T2 → (T3 ∥ T4) → T5 → T6.

---

## T0: Dev and test environment

**Status: local R environment DONE (4 Nov 2026).** Installed natively instead of Docker:
- `rig` 0.11.1 → R 4.6.2 (`/opt/R/4.6.2`, repos = P3M binaries for Ubuntu 26.04 "resolute").
- System libraries via apt: the list printed by `pak::pkg_sysreqs()` (`libfreetype6-dev` is `libfreetype-dev` on 26.04).
- Packages: `PKG_SYSREQS=false Rscript -e 'pak::pak(c("deps::.", "IvanWilli/MortIndEst", "timriffe/DemoToolsData", "devtools", "testthat"))'`
  (253 packages, about 1.5 min). All 28 Imports + MortIndEst/DemoToolsData load; MortIndEst Panama example matches Manual X.
- Run from local source: `Rscript -e 'pkgload::load_all("."); shiny::runApp(run_app(), port = 8180, launch.browser = FALSE)'`.
  The landing page renders all 5 module cards in Chrome. A Shiny console error ("Error in inputBinding.receiveMessage()")
  was already showing on the landing page before any of this work.

Remaining for T0:
- ~~testthat scaffold~~: done with T2 (`test-childmort.R`, plus a fixture under `tests/testthat/fixtures/`); `devtools::test()` runs locally.
- Chrome e2e procedure: run the app on `http://localhost:8180`, follow `module_plan.md` Phases 1–5, and check
  download ZIPs by fetching the `session/<token>/download/<id>` link with curl rather than saving through the browser.
- Optional later: Docker dev image for CI or for others (the release `Dockerfile` installs from GitHub, not local source).

## T1: MortIndEst dependency + shared backend layer

**Status: DONE (4 Nov 2026)** on `dev/childmort-module`. `DESCRIPTION` changes landed with T2 (`89395b9`);
`run_by_group(data, fun, ..., check = NULL)` in `R/backend_indirect.R` (`582539a`) is now used by
`run_childmort()`. It returns one bound table per table `fun` returns, with `.id` first and `.id_label` attached,
plus `group_errors` (`.id`, `message`) for failed groups. Tests: `tests/testthat/test-backend_indirect.R`.
The error contract is unchanged until Ivan and Roy decide.

- `DESCRIPTION`: add `MortIndEst` (Imports) and `DemoToolsData` (Imports; `childmort_brass` needs it
  at runtime, although MortIndEst only lists it under Suggests). Add the `Remotes` entries for both.
- New `R/backend_indirect.R` with a generic `run_by_group(data, fun, ...)` that splits on `.id`,
  calls the MortIndEst function, binds the results back together with `.id`, and catches errors
  per group so one bad group doesn't kill the batch.
- Error contract: a per-group `status` / `message`. Align with whatever Ivan and Roy decide
  (Oct 5 action item: how validation and computation errors are returned to the app).
- Check: the backend wrappers stay in this app for now. Move them to ODAPbackend later if Tim wants them there.

## T2: Child mortality from children ever born / children dead (FIRST TASK)

**Status: DONE (4 Nov 2026) on branch `dev/childmort-module`** (commits `89395b9`, `307d394`, `90627f0`).
- Backend `R/backend_childmort.R`, module `R/mod_childmort.R`, landing card, navigation, 44 EN/FR/ES strings.
- Unit tests: `tests/testthat/test-childmort.R`. The full suite has 28 tests and 79 expectations, 0 failures, plus the 1 golem test that was already skipped.
  The Panama sample reproduces Manual X Table 53 to 4 decimals.
- Chrome e2e passed `module_plan.md` Phases 1–5: sample run (Trussell and Palloni-Heligman), 2-region CS upload with
  Region grouping, group switch, single and all-groups ZIP downloads (contents checked), FR/ES switch, back and
  re-entry navigation, invalid-file validation.
- Problems found in shared code during T2, **all fixed on 4 Nov 2026** (unit tests plus a Chrome check each):
  - Tab labels "Upload Instructions" / "Method Documentation" stayed in English after a language switch.
    Fixed for all five upload modules (`6dbdb23`, `tests/testthat/test-info-tab-labels.R`).
  - Clicking Confirm in the grouping modal without picking columns or ticking "No grouping" made
    `ODAPbackend:::create_groupid()` put each row in its own group. The modal now asks for a column or the tick;
    this also covers the life table page (`2be5bd9`, `tests/testthat/test-grouping-modal.R`).
  - Re-uploading the same file after Cancel didn't reopen the grouping modal. Fixed for the five upload
    modules (`df34c0d`) and the life table page (next commit).
- Found in the full Chrome re-test (4 Nov 2026) and fixed:
  - The grouping modal opened the column menu over the "No grouping" checkbox (Semantic UI autofocus), so a click
    on the checkbox picked a column (`c92b5de`).
  - "Browse...", "no file selected" and "Upload complete" stayed in English in every upload widget (`342e811`).
- Still open, not caused by this branch: the browser console logs `[shiny] Error in inputBinding.receiveMessage()`
  once per language switch (same on the code before these fixes).
- Follow-ups (4 Nov 2026): a "Results for: …" line above the chart, a warning when the controls no longer match the
  shown results, and a hint that the model life table sex only changes the implied e0 (`73be953`).

**Why first:** smallest backend (148 lines, one census, no HIV or two-census logic), and it has a
built-in check: the roxygen example must reproduce **UN Manual X Table 53 (Panama 1976)** to 4 decimal places.
The Oct 5 action items also assigned it to Jorge (child mortality module architecture, then an early result to Yvonne).

**Backend facts (`MortIndEst::childmort_brass`, read in full):**
- Args: `CEB, CD, W, age = seq(15,45,5), variant = "trussell" | "palloni_helligman",
  mlt_model, Sex = "male" | "female", date_obs, mam` (mean age at maternity, used by Palloni-Helligman only; defaults to 27).
- Returns a list:
  - `qx_estimate`: `age, lx, qx, x, t, date_t` (x = 1,2,3,5,10,15,20 hard-coded).
  - `mlts_interp`: `age, sex, x, lx, date_t, age_mlt, mx_mlt, qx_mlt, lx_mlt, ex_mlt`.
- **Requires exactly 7 rows, ages 15–45 by 5.** Anything else silently misaligns `x`.
- No input checks: CD > CEB, W = 0, or NA give silent garbage or Inf.
- Model families: Trussell uses `North/South/East/West`; Palloni-Helligman uses
  `Latin/Chilean/South_Asian/Far_East_Asian/General`. Mismatches silently fall back to West or General.
  Verified: these names match `DemoToolsData::modelLTx1$family`, whose `sex` values are `male` and `female`.
- Defaults are announced with `message()`. The wrapper always passes explicit values.

**Template / sample**
- Columns: `Age` (15,20,…,45), `Women`, `CEB`, `CD` (or `CS` = children surviving; the wrapper derives
  `CD = CEB − CS`), optional `Date` (per-group date of observation, overriding the UI value), plus any grouping columns.
- Sample: Panama (Manual X Table 49) saved as `inst/extdata/childmort_sample.csv`. Also a 2-group sample (Panama ×2 regions) for bulk.

**Controls:** variant (radio); model family (choices change with the variant); MLT sex
(male/female); date of observation (numeric, decimal year); mean age at maternity (shown only for Palloni-Helligman).

**Outputs**
- Table: age group of women, `x`, `qx`, `t`, `date_t`, implied e0 level.
- Chart: q(x) estimates vs reference date (`date_t`), one series per group, x-labels `q(1)…q(20)`.
- Diagnostics: flag age groups 15–19 and 45–49 as less reliable (Manual X guidance), and flag any `lx` outside the MLT range.
- Download: CSV of both data frames + PNG of the chart. All-groups ZIP for bulk.

**Files** (per `module_plan.md`)
- New: `R/mod_childmort.R` (built on `mod_simple_module_server`; `mod_odap.R` is the nearest analogue,
  with its own sample loader and validator), `R/backend_childmort.R` (validator + wrapper),
  `inst/extdata/childmort_sample.csv`, `tests/testthat/test-childmort.R`.
- Edit: `R/app_modules.R` (registry entry), `R/app_handles.R` (navigation), `R/app_ui.R` + `R/app_server.R`
  (landing card), `inst/extdata/translation.json`, `DESCRIPTION`.

**Unit tests (`test-childmort.R`)**
1. Panama sample → wrapper reproduces Manual X Table 53 `qx` to 4 dp:
   `0.0952, 0.0580, 0.0707, 0.0757, 0.1021, 0.1201, 0.1308`.
2. `CS` input gives the same result as the equivalent `CD` input.
3. Validator fails on: missing column; ages ≠ 15..45 by 5 or ≠ 7 rows; `CD > CEB`; `Women <= 0`; `CEB < 0`; NA values.
   Each failure returns a readable `message`.
4. Variant/family mismatch is rejected (no silent fallback).
5. Palloni-Helligman runs with `mam` and returns 7 finite `qx`.
6. Grouped input (2 `.id`s) returns 7 rows per group with `.id` kept. One broken group returns an
   error status for that group only; the other group still computes.
7. The per-group `Date` column overrides the UI date in `date_t`.

**Chrome e2e** (`module_plan.md` Phases 1–5): landing card → module → sample → validation passes →
run → table + chart render → change variant/family → re-run → upload the 2-group CSV → switch groups →
download current group and all groups → switch to FR and ES → go back and re-enter (state resets).

**Done when:** all unit tests pass in the T0 image, the Chrome run passes all five phases, and the
Panama numbers in the UI match Manual X.

## T3: Adult mortality from orphanhood

- `orphanhood_one` (single census or survey: proportion with mother/father alive by respondent age,
  mean age at childbearing, maternal/paternal) and `orphanhood_two` (two observations; variants
  `timaeus` (default), `preston_chen`, `masquellier_timaeus`).
- Brass logit smoothing (`brass_logit`, `brass_logit_e0`, `brass_logit_5q0`); model family or user standard `lx`.
- HIV sub-item: antenatal prevalence, vertical transmission, fertility ratio, male prevalence and partner inputs, as an
  "advanced" panel.
- Built-in checks: roxygen examples for Iraq 1997 (published 45q15) and Kenya 1999 AIDS-adjusted.
- Output: `adult_mort_index` (q15_45 by respondent age with date). Chart: 45q15 vs reference date.

**Status: DONE (9 Nov 2026)** on branch `dev/orphanhood-module`, which starts from `dev/childmort-module` because it
needs the T1 layer. Commits: `d95005c` (backend), `806214c` (module), `4a9f7fb` (fixes from the Chrome run),
`fb9321b`, `78ee128`, `24d0652` (Masquelier-Timaeus), `35578f6` (Spectrum). Not pushed.

- Backend `R/backend_orphanhood.R`, module `R/mod_orphanhood.R`, landing card, navigation, 112 EN/FR/ES strings.
- **Template** (one row per five-year age group of respondents):
  - One census or survey: `Age`, `NotOrphaned` (proportion with the parent alive).
  - Two censuses or surveys: `NotOrphaned1` and `NotOrphaned2` instead. The module detects this from `NotOrphaned2`.
  - Optional columns:
    - `Date`, or `Date1` and `Date2`: decimal years.
    - `MAC`, or `MAC1` and `MAC2`: mean age at childbearing, one value or one per age group.
    - `HIVPrev`: one census only.
  - Each optional column overrides its control, per group.
  - It's a wide format because the grouping check (`ODAPbackend:::check_groupid`) needs `Age` to be unique within
    each group, so a long format with two rows per age would fail grouped uploads.
- **Methods:**
  - One census: `orphanhood_one`.
  - Two censuses: `orphanhood_two` with all three variants, Timaeus (default), Preston-Chen and Masquelier-Timaeus.
- **Brass logit standard:**
  - A model life table: 9 families, level by e0 or by 5q0 (the level with the closest 5q0), female tables for mothers
    and male tables for fathers.
  - Or a custom `lx` for ages 15–85, typed or pasted into a table.
  - Or the Spectrum HIV model life tables: HIV prevalence, ART coverage and 5q0; this covers `brass_logit_5q0`.
  - The wrapper builds the model standard and passes it as `mlt_data_input` with `brass_logit = TRUE`. This works
    around MortIndEst bug 1 below.
- **HIV, both MortIndEst options:**
  - Timaeus and Nunn: one census, `HIVPrev` column (antenatal prevalence for mothers, male prevalence for fathers),
    vertical transmission (default 1/3), fertility ratio, and the proportion of men with an infected partner.
  - Spectrum life tables: one census, or two censuses with Timaeus or Preston-Chen.
  - Masquelier-Timaeus: HIV prevalence, ART and PMTCT series by calendar year, typed or pasted into a table (blank
    cells allowed), plus the period-covariates option (midpoint or average).
    - The same series is used for every group; per-group series would need another input.
- **Samples** (picked next to the sample button):
  - Iraq 1997 (IUSSP).
  - Panama 1977–1980 (Preston-Chen example).
  - Kenya 1999 with `HIVPrev` (IUSSP AIDS).
  - South Africa 2007–2016, whose HIV series fills the table.
- **Not exposed:** interpolation between model life table levels (`brass_logit = FALSE`). For mothers it would use the
  male tables (bug 1 below), and there is no workaround from outside MortIndEst.
- **MortIndEst issues to report to Ivan:**
  1. `orphanhood_one()` and `orphanhood_two_timaeus()` filter `MortCast::MLTlookup` with
     `sex == ifelse(sex == "f", 2, 1)`. Inside `filter()`, `sex` is the column, so this always selects the **male**
     tables, also for maternal orphanhood. Preston-Chen goes through `orphanhood_one()`, so it is affected too;
     Masquelier-Timaeus hard-codes `sex == 2` and is fine.
     - Size: up to 0.019 in 45q15 (Iraq, CD West, e0 60).
     - Fix: compute the sex code before the pipe and use `sex == !!code`.
     - `test-orphanhood.R` pins the current upstream behaviour, so that test fails once this is fixed.
  2. The orphanhood code calls dplyr, purrr and tidyr functions without importing them; NAMESPACE imports only `%>%`.
     Without dplyr attached, `lag()` silently becomes `stats::lag()` and Preston-Chen gives wrong proportions.
     - The app wraps each call in `with_mortindest_deps()`, which attaches those packages for the duration of the
       call.
     - Checked from an installed build in a clean R session.
     - `childmort_brass()` is not affected.
  3. `orphanhood_one(mac = NULL)` fails with "arguments imply differing number of rows". The app requires a MAC.
  4. The Spectrum path calls `rms::rcs()` and DemoTools functions without importing them, and reads `mods.r` (lazy
     package data) without `MortIndEst::`, so it only works with rms, DemoTools and MortIndEst attached.
     - `with_mortindest_deps(extra = ORPHANHOOD_SPECTRUM_PACKAGES)` attaches them for the call and detaches everything
       it added.
- **Unit tests:** `tests/testthat/test-orphanhood.R`, 42 tests and 302 expectations.
  - Iraq 1997 reproduces the published IUSSP 45q15 within 0.0007.
  - Kenya 1999 with HIV is within 0.3%.
  - South Africa Masquelier-Timaeus p25 matches the workbook within 0.001.
  - The Spectrum results equal the direct MortIndEst call.
  - The Salomons paternal two-census example is kept as a snapshot.
  - Also covered: validation (including proportions that rise with age, i.e. the proportion orphaned entered by
    mistake), HIV series checks, grouped runs with a failing group, column overrides, settings and diagnostics, and
    translation coverage.
  - Full suite: 90 tests, 460 expectations, 0 failures, 1 golem skip.
- **Chrome e2e (9 Nov 2026):**
  - All four samples.
  - Typed into the custom table, the IUSSP standards reproduce the published values in the UI: Iraq exactly, and
    Kenya with HIV within 0.3%.
  - Timaeus and Preston-Chen; paternal with HIV.
  - Masquelier-Timaeus:
    - South Africa with the pre-filled series matches the backend.
    - Editing the series or the period option shows the out-of-date warning.
    - Mother-only rule; empty series; HIV files in the ZIPs.
    - Two provinces with the series typed in, plus an all-groups ZIP.
  - 5q0 level.
  - Spectrum for one census (Kenya) and two censuses (Timaeus, Preston-Chen), with the Masquelier-Timaeus combination
    refused.
  - FR and ES for the new controls.
  - Two-region one-census upload grouped by Region: group switch, plus single and all-groups ZIPs with contents
    checked.
  - Two-province two-census upload with dates and MAC from the controls, including a group that fails Timaeus: the
    error is shown in the chart and `orphanhood_errors.csv` is in the ZIP.
  - FR and ES across both steps and the docs; back and re-entry navigation.
  - Invalid files: percentages, proportion orphaned, missing column.
  - No new console errors; only the known `inputBinding.receiveMessage()` error on a language switch.
- **Found in shared code, not caused by T3, left as is:**
  - After a language switch, the group selector goes back to the first group (confirmed in child mortality too).
  - In all six upload modules, `shinyjs::show/hide(id = ns(...))` inside module observers namespaces the id twice,
    so those calls do nothing.
    - Users don't hit it today: the Previous button hides the results with jQuery, and uploads happen on the upload
      step.
    - Orphanhood now hides the download button itself when a run fails.

## T4: Intercensal survival

- Two censuses (c1, c2, exact dates, 5-year ages with open group), sex, family, `method`
  (match / fproj / logit / feeney …), `ages_fit`, `e0_accept`, optional `q01_q05`, pre-smoothing span,
  HIV prevalence / ART.
- Built-in checks: roxygen examples for Panama 1960–70 (match), India 1980–90 (fproj), Japan (feeney).
- Open: Ivan is comparing Sarah's interpolation against the current implementation (Oct 5 action item).
  Pin the MortIndEst commit once he confirms.

## T5: Translations
FR/ES for every string added in T2–T4, reviewed in the UI in both languages.
T2 and T3 are done (T3: 112 strings, checked in the UI in FR and ES on 9 Nov 2026); T4 is pending.

## T6: Task 1 acceptance demo
Ask Patrick for the target country and its published census and survey tables. Build a subnational bulk dataset for each of
T2–T4, run it in the deployed app, and record a GIF and a short write-up. This is the 16 Dec deliverable.

## T7–T9: Model life tables (Task 2)
- T7: empirical relational (Brass logit with a user-supplied standard), fitted to user data. Outputs a full life table.
- T8: classic systems: Coale-Demeny (N/S/E/W) and UN (Latin, Chilean, South Asian, Far East Asian, General)
  by e0 or a child/adult mortality index. **Blocked on Patrick** getting Sarah's model life table code released
  (Oct 5 action item). Fallback: `IvanWilli/MortpakAbacusLegacyMLT`.
- T9: HIV- and treatment-adjusted variants (inputs: prevalence, ART coverage). Depends on T8.

## T10: Checks against reference implementations
For each module, a test fixture comparing outputs against Mortpak and the Division's R results, with
documented tolerances. Ivan noted that outputs differ across implementations (OrphaCook I vs II,
intercensal survival). Record which one is the reference. Ivan will supply cases where the methods break,
for stress tests (Oct 5 action item).

**Child mortality status (4 Nov 2026):** the only reference so far is UN Manual X Table 53 (Panama 1976,
Trussell / West), reproduced to 4 decimals in `test-childmort.R`. That is not a Mortpak comparison:
we have no Mortpak (CEBCD) or Division R outputs yet, and no Palloni-Heligman reference values.
Needed from Ivan / Patrick: Mortpak outputs for a few CEB/CD datasets, including at least one Palloni-Heligman case.

**Orphanhood status (9 Nov 2026):** the references so far are the IUSSP spreadsheet values for Iraq 1997 (one census)
and Kenya 1999 (HIV), both reproduced in `test-orphanhood.R`.
- Masquelier-Timaeus: the South Africa p25 values match the workbook figures quoted in MortIndEst.
- There are no published values for the Timaeus and Preston-Chen two-census variants, or for the Spectrum life tables;
  the Salomons and Panama examples are snapshots only.
- Mortpak outputs are still needed for one and two censuses (maternal and paternal), plus Preston-Chen and Timaeus
  two-census reference values.
- MortIndEst's built-in family lookup uses male tables for mothers (see T3), so any comparison must use the app's
  wrapper or pass the standard explicitly.

## T11: Performance + user testing
Profile bulk runs with many groups (parallel downloads already exist). Run a session with selected national counterparts
and fix what they find.

## T12: Deployment
Update the release `Dockerfile` (it installs from GitHub, so MortIndEst and DemoToolsData must resolve) and
the cloud deploy. Smoke-test every new module on both.

**Status (4 Nov 2026):** not started. The Docker image installs from GitHub master, so it needs
`dev/childmort-module` pushed and merged first. Done meanwhile: `R CMD build` + `R CMD INSTALL` into a
temporary library works; the installed package loads, reproduces Manual X and builds `run_app()`.

## T13: User documentation
A short page per module: inputs, template, parameters, how to read the outputs, references (Manual X etc.),
in the app's existing docs/info-button pattern.

**Child mortality: DONE (4 Nov 2026)** (`5728ad9`). The Method Documentation tab covers the method, both variants,
how to read the chart and the implied e0, the less reliable age groups, and references (Manual X, Brass 1968,
Trussell 1975, Palloni-Heligman 1985). Inputs and the template are in the Upload Instructions tab. EN/FR/ES.

**Orphanhood: DONE (9 Nov 2026)** (`806214c`). The Method Documentation tab covers:
- the method;
- one and two censuses;
- the standard life table;
- the HIV adjustment;
- how to read the chart, the adoption effect, and paternal reliability;
- references: IUSSP Tools 2013, Timaeus 1991 and 1992, Preston-Chen 1984, Timaeus-Nunn 1997.

The Upload Instructions tab lists both templates and the optional columns. EN/FR/ES.

---

## Open questions (resolve before or while doing T2–T4)
- Should the child mortality template take CD, CS, or both? (Planned: both, with CS converted.)
- Which country and data for the Task 1 demo (T6)?
- Error-return contract (Ivan ↔ Roy) for T1.
- Which reference should the checks use when implementations disagree (T10)?
