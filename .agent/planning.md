# Planning - Lifetableprojection Module Improvements

**Note:** No tests need to be written for these tasks.

---

- [x] Task 1: Make Data Upload Generic for Any Column (Not Just Deaths/Exposures)

**Goal:** Update the variable selection in heaping, smoothing, and graduation modules to allow any column (not just hardcoded Deaths/Exposures) to be selected as the variable to evaluate.

**Current State:**
- In `mod_heaping.R:394-407`: Variable selector hardcodes `choices = c("Deaths", "Exposures")`
- In `mod_smoothing.R:346-349`: Variable selector hardcodes `choices = c("Deaths", "Exposures")`
- In `mod_graduation.R`: Similar hardcoded pattern
- The Age column is always required, but the analysis column should be any numeric column that isn't Age

**Required Changes:**

1. **Modify variable selector UI** in each module to dynamically populate choices from uploaded data:
   - Get column names from `shared$data()`
   - Filter out `Age`, `.id`, `.id_label`, and any non-numeric columns
   - Show remaining columns as choices for "Variable to evaluate"

2. **Files to modify:**
   - `/home/jorge/repositories/un_apps/lifetableprojection/R/mod_heaping.R` (lines ~394-407)
   - `/home/jorge/repositories/un_apps/lifetableprojection/R/mod_smoothing.R` (lines ~345-350)
   - `/home/jorge/repositories/un_apps/lifetableprojection/R/mod_graduation.R` (similar location)

3. **Implementation pattern:**
```r
output$[module]_variable_selector <- shiny::renderUI({
  df <- shared$data()
  req(df)
  # Get numeric columns that aren't Age or grouping columns
  numeric_cols <- names(df)[sapply(df, is.numeric)]
  exclude_cols <- c("Age", ".id", ".id_label")
  choices <- setdiff(numeric_cols, exclude_cols)

  shiny.semantic::selectInput(
    ns("[module]_variable"),
    i18n$t("Variable to evaluate"),
    choices = choices,
    selected = if (!is.null(input$[module]_variable) && input$[module]_variable %in% choices)
      input$[module]_variable else choices[1]
  )
})
```

4. **Update validation** in `app_data_validation.R` to only require Age column, making other columns optional

---

- [x] Task 2: Fix Graduation Scale Mismatch (Single Ages to 5-Year Age Groups)
**Note:** No tests need to be written for these tasks.

**Goal:** When graduating from single ages to 5-year age groups, divide the values appropriately so they're on a comparable scale in the plot. Also ensure downloaded data has matching totals between original and graduated columns.

**Current State:**
- In `ODAPbackend/R/graduation.R`: The `graduate_auto()` function transforms data but doesn't normalize for scale comparison
- When converting single ages (e.g., age 0,1,2,3,4) to 5-year groups (0-4), the 5-year value is the sum of 5 single-age values
- This makes the plot comparison misleading since scales differ

**Root Cause:**
- `groupAges()` from DemoTools sums values when creating age groups
- The plot shows both on same Y-axis without scale adjustment
- Downloaded data has different totals between original and graduated columns

**Required Changes:** (Normalization applies to BOTH plot AND downloaded data)

1. **In plotting section** (`ODAPbackend/R/graduation.R` ~lines 516-578):
   - When input is single ages and output is 5-year or abridged, divide graduated values by the age interval width (typically 5) for plot display
   - This makes the visual comparison meaningful on the same scale

2. **In download/output section:**
   - Normalize the graduated data so that `sum(original)` equals `sum(graduated)` in the exported CSV
   - Add a scaling factor: `graduated_normalized = graduated * (sum(original) / sum(graduated))`
   - The downloaded data MUST have matching totals between original and graduated columns

3. **Files to modify:**
   - `/home/jorge/repositories/un_apps/ODAPbackend/R/graduation.R` (plotting section ~lines 516-578, data output ~lines 548-600)
   - `/home/jorge/repositories/un_apps/lifetableprojection/R/mod_graduation.R` (download handler ~lines 791-827)

4. **Implementation approach:**
```r
# For plotting - divide by interval width for visual comparison
age_in_type <- if (is_single(Age)) "single" else "grouped"
age_out_type <- age_out  # "5-year", "abridged", or "single"

if (age_in_type == "single" && age_out_type %in% c("5-year", "abridged")) {
  plot_graduated <- graduated_df
  plot_graduated$Value <- plot_graduated$Value / 5  # Average per single age
}

# For download - normalize totals
total_original <- sum(original_data[[variable]], na.rm = TRUE)
total_graduated <- sum(graduated_data[[variable]], na.rm = TRUE)
scaling_factor <- total_original / total_graduated
graduated_data[[paste0(variable, "_graduated")]] <-
  graduated_data[[variable]] * scaling_factor
```

---

- [x] Task 3: ODAP Module - Allow Sex/Year Columns in Upload Data

**Goal:** If user provides `sex` and/or `year` columns in their upload data, use those values for WPP data retrieval instead of showing dropdown inputs. Each group should use its corresponding sex/year values.

**Current State:**
- In `mod_odap.R:379-388`: Code checks for `has_nlx` and `has_grouping` but only hides WPP inputs if data has nLx OR all of (name, sex, year)
- WPP dropdowns (country, sex, year) apply globally to ALL groups
- In parallel execution (`mod_odap.R:688-851`): sex/year parameters are not passed per-group

**Required Changes:** (Only sex + year columns supported, country always from dropdown)

1. **Update upload documentation** to clarify:
   - Required columns: `Age`, `pop`
   - Optional columns: `nLx` (if provided, no WPP fetch needed), `sex`, `year`
   - If `sex` column provided: hide sex dropdown in analysis page
   - If `year` column provided: hide year dropdown in analysis page
   - If both provided: only show country dropdown (country is ALWAYS from dropdown, never from data)

2. **Modify controls UI** (`mod_odap.R:374-466`):
```r
# Check which columns are provided
col_names_lower <- tolower(names(df))
has_sex_col <- "sex" %in% col_names_lower
has_year_col <- "year" %in% col_names_lower
has_nlx <- "nlx" %in% col_names_lower

# Conditionally show WPP inputs
needs_country <- !has_nlx
needs_sex <- !has_nlx && !has_sex_col
needs_year <- !has_nlx && !has_year_col
```

3. **Modify parallel execution** (`mod_odap.R:708-727`) to pass per-group sex/year:
```r
results <- future.apply::future_lapply(all_ids, function(gid) {
  data_subset <- df[df$.id == gid, , drop = FALSE]

  # Get sex/year from data if available, otherwise use UI inputs
  row_sex <- if ("sex" %in% tolower(names(data_subset)))
    data_subset$sex[1] else input$wpp_sex
  row_year <- if ("year" %in% tolower(names(data_subset)))
    data_subset$year[1] else input$wpp_year

  ODAPbackend::odap_opag(
    data_in = data_subset,
    sex = row_sex,
    year = row_year,
    name = wpp_name,
    ...
  )
}, future.seed = TRUE)
```

But also modify any call to odap_opag (sequentially if called) such that if groups are changed interactively in the frontend, it takes care of passing the appropriate year/sex if as columns in the odap_opag of that group.

4. **Files to modify:**
   - `/home/jorge/repositories/un_apps/lifetableprojection/R/mod_odap.R` (lines 374-466, 688-851)
   - Upload info box content (wherever `odap_info_box_content` is rendered)

---

- [x] Task 4: Add Two-Tab System to Upload Documentation Pages

**Note:** No tests need to be written for these tasks.

**Goal:** Add a tabbed interface to each module's upload/info-box section with two tabs: "Upload Instructions" (current content) and "Method Documentation" (detailed method descriptions).

**Current State:**
- Each module has an `info-box` div containing upload instructions
- Example: `mod_heaping.R:197-200`, `mod_smoothing.R:124-129`
- No tabs currently - just a single container with instructions

**Required Changes:**

1. **Create tab UI structure** using `shiny.semantic::tabset`:
```r
shiny::div(
  class = "info-box",
  shiny.semantic::tabset(
    tabs = list(
      list(
        menu = i18n$t("Upload Instructions"),
        content = shiny::div(
          # Existing upload instructions content
          shiny::uiOutput(ns("[module]_info_box_content")),
          rhandsontable::rHandsontableOutput(ns("[module]_sample_table")),
          shiny::uiOutput(ns("[module]_info_box_instructions"))
        )
      ),
      list(
        menu = i18n$t("Method Documentation"),
        content = shiny::div(
          # New method documentation content
          shiny::uiOutput(ns("[module]_method_docs"))
        )
      )
    )
  )
)
```

2. **Create method documentation content** for each module:

   **Heaping Module (`mod_heaping.R`):**
   - Bachi Index: Measures digit preference by comparing observed vs expected frequencies
   - Myers' Blended Index: Detects preference for terminal digits 0-9
   - Whipple Index: Measures attraction to ages ending in 0 or 5
   - Noumbissi Index: Digit-specific heaping measure
   - Sawtooth Pattern: Detects alternating high-low pattern in adjacent ages

   **Smoothing Module (`mod_smoothing.R`):**
   - Fine Methods (splitting to single ages):
     - `sprague`: Sprague 4th difference formula - standard demographic method
     - `beers(ord)`: Beers ordinary interpolation - less constrained
     - `beers(mod)`: Beers modified interpolation - additional smoothing constraints
     - `grabill`: Grabill method - designed for population data
     - `pclm`: Penalized Composite Link Model - modern spline-based approach
     - `mono`: Monotonic graduation - prevents oscillations/negative values
     - `uniform`: Uniform distribution within age groups - simplest method
   - Rough Methods (smoothing 5-year groups):
     - `Carrier-Farrag`: Ratio-based smoothing method
     - `KKN`: Karup-King-Newton method
     - `Arriaga`: Arriaga redistribution method
     - `United Nations`: UN standard smoothing
     - `Strong`: Aggressive smoothing method
     - `Zigzag`: Corrects oscillation patterns

   **ODAP Module (`mod_odap.R`):**
   - `mono`: Monotonic redistribution - preserves mortality monotonicity
   - `pclm`: PCLM-based redistribution - spline smoothing approach
   - `uniform`: Uniform redistribution - simple proportional method
   - Parameters: Age_fit, AgeInt_fit, Redistribute_from, OAnew explained

   **Graduation Module (`mod_graduation.R`):**
   - Same methods as smoothing (uses `graduate_auto()` from ODAPbackend)

   **Lifetable Module**

3. **Files to modify:**
   - `/home/jorge/repositories/un_apps/lifetableprojection/R/mod_heaping.R` (info-box section)
   - `/home/jorge/repositories/un_apps/lifetableprojection/R/mod_smoothing.R` (info-box section)
   - `/home/jorge/repositories/un_apps/lifetableprojection/R/mod_graduation.R` (info-box section)
   - `/home/jorge/repositories/un_apps/lifetableprojection/R/mod_odap.R` (info-box section)
   - `lifetable module`

---

- [x] Task 5: Add Info Buttons for Method Parameters

**Note:** No tests need to be written for these tasks.

**Goal:** For each method selector dropdown in the analysis pages, add an info/help icon button that shows a **tooltip on hover** explaining what each method does. (Use Semantic UI tooltip with hover interaction)

**Current State:**
- Method selectors in `mod_smoothing.R:351-361` show arcane names like "beers(ord)", "KKN", etc.
- No explanation of what each method means
- Users unfamiliar with demographic methods cannot make informed choices

**Required Changes:**

1. **Create helper function** for info button:
```r
create_info_button <- function(ns, id, tooltip_content, i18n) {
  shiny::div(
    style = "display: inline-flex; align-items: center; gap: 8px;",
    shiny::tags$label(`for` = ns(id), i18n$t(label_text)),
    shiny::tags$span(
      class = "ui circular label",
      style = "cursor: pointer; font-size: 0.8em;",
      `data-tooltip` = tooltip_content,
      `data-position` = "right center",
      "?"
    )
  )
}
```

2. **Add to each method selector** (only where methods exist):

   **Smoothing Module (`mod_smoothing.R:351-361`):**
   ```r
   # Fine Method with info button
   shiny::div(
     create_method_label_with_info(
       ns, "fine_method", i18n$t("Fine Method"),
       i18n$t("Method for splitting grouped ages to single ages.
       'sprague' - standard formula, 'beers(ord)' - ordinary Beers,
       'beers(mod)' - modified Beers, 'pclm' - modern spline method,
       'mono' - monotonic (no negatives), 'uniform' - equal distribution.")
     ),
     shiny.semantic::selectInput(...)
   )

   # Rough Method with info button
   shiny::div(
     create_method_label_with_info(
       ns, "rough_method", i18n$t("Rough Method"),
       i18n$t("Method for smoothing 5-year age groups.
       'Carrier-Farrag' - ratio method, 'KKN' - Karup-King-Newton,
       'Arriaga' - redistribution, 'United Nations' - UN method,
       'Strong' - aggressive smoothing, 'Zigzag' - oscillation correction.")
     ),
     shiny.semantic::selectInput(...)
   )
   ```

   **ODAP Module (`mod_odap.R:391-396`):**
   ```r
   # Redistribution method with info button
   shiny::div(
     create_method_label_with_info(
       ns, "method", i18n$t("Redistribution method"),
       i18n$t("Method for redistributing old-age population.
       'mono' - monotonic (preserves mortality pattern),
       'pclm' - spline-based smoothing,
       'uniform' - simple proportional distribution.")
     ),
     shiny.semantic::selectInput(...)
   )
   ```

3. **Use Semantic UI tooltip** by adding the required JS initialization:
```r
shiny::tags$script(shiny::HTML("
  $(document).ready(function() {
    $('[data-tooltip]').popup();
  });
"))
```

4. **Files to modify:**
   - `/home/jorge/repositories/un_apps/lifetableprojection/R/mod_smoothing.R` (controls section ~340-390)
   - `/home/jorge/repositories/un_apps/lifetableprojection/R/mod_odap.R` (controls section ~390-400)
   - `/home/jorge/repositories/un_apps/lifetableprojection/R/mod_graduation.R` (if it has method selectors)
   - Heaping module doesn't have method selection (only diagnostics output)

**Method Documentation Reference (from DemoTools/ODAPbackend):**

| Method | Type | Description |
|--------|------|-------------|
| sprague | Fine | Sprague 4th difference formula for age graduation |
| beers(ord) | Fine | Beers ordinary interpolation |
| beers(mod) | Fine | Beers modified with extra smoothing |
| grabill | Fine | Grabill interpolation for population |
| pclm | Fine | Penalized Composite Link Model (splines) |
| mono | Fine/ODAP | Monotonic - prevents negative values |
| uniform | Fine/ODAP | Uniform distribution within intervals |
| Carrier-Farrag | Rough | Ratio-based 5-year smoothing |
| KKN | Rough | Karup-King-Newton adjustment |
| Arriaga | Rough | Arriaga age redistribution |
| United Nations | Rough | UN standard smoothing |
| Strong | Rough | Aggressive smoothing |
| Zigzag | Rough | Oscillation pattern correction |

---

- [x] Task 6: Update Upload Documentation - Column Requirements and Data Format
**Note:** No tests need to be written for these tasks.
**Goal:** Clarify in upload documentation that users can provide one column or both, and that proportions (not just raw numbers) are acceptable. Also specify that data should be in single ages (not abridged or 5-year groups).

**Changes needed:**
- Update info-box content in heaping, smoothing, and graduation modules
- Clarify: "Upload data with Age column and at least one numeric column (Deaths, Exposures, or any other metric)"
- Specify: "Data can be counts or proportions"
- Emphasize: "Data should be in single-year ages (0, 1, 2, 3, ...), not 5-year or abridged format"

**Files to modify:**
- `/home/jorge/repositories/un_apps/lifetableprojection/R/mod_heaping.R` (info box content)
- `/home/jorge/repositories/un_apps/lifetableprojection/R/mod_smoothing.R` (info box content)
- `/home/jorge/repositories/un_apps/lifetableprojection/R/mod_graduation.R` (info box content)

---

- [ ] Task 7: Add TXT Explanation File to Module Downloads
**Note:** No tests need to be written for these tasks.

**Goal:** When users download results from heaping, smoothing, ODAP and graduation modules, include a TXT file explaining what the method does and the date/time of the analysis run.

**Changes needed:**
- Modify download handlers to create a zip containing:
  1. The CSV data file (existing)
  2. A new `analysis_info.txt` file with:
     - Module name and brief explanation of what it does
     - Date and time of analysis run
     - Parameters used (if applicable)
- Apply to: heaping, smoothing, graduation modules (NOT lifetable)

**Files to modify:**
- `/home/jorge/repositories/un_apps/lifetableprojection/R/mod_heaping.R` (download handler)
- `/home/jorge/repositories/un_apps/lifetableprojection/R/mod_smoothing.R` (download handler)
- `/home/jorge/repositories/un_apps/lifetableprojection/R/mod_graduation.R` (download handler)
- `also odap`

---

- [ ] Task 8: French Translation - Fix Untranslated 'data' Word
**Note:** No tests need to be written for these tasks.

**Goal:** The word "data" appears untranslated in the French UI in several places, including complete sentences. Find and add proper French translations.

**Changes needed:**
- Search for instances of "data" in UI text that aren't being translated
- Add missing translation keys to the i18n translation file
- French translation for "data" is typically "données"
- Check complete sentences containing "data" that may need full translation

**Files to check:**
- `/home/jorge/repositories/un_apps/lifetableprojection/inst/app/www/translations/` (translation CSV files)
- All module files for hardcoded "data" strings not wrapped in `i18n$t()`

---

- [ ] Task 9: Sort Age Columns Numerically in All Downloads
**Note:** No tests need to be written for these tasks.

**Goal:** Age columns in downloaded CSV files are currently saved as strings and appear unsorted (e.g., "1", "10", "11", ... "2", "20"). Ensure Age is treated as numeric and rows are sorted by Age in all download modules.

**Changes needed:**
- Before writing CSV in download handlers, ensure:
  1. Age column is numeric: `df$Age <- as.numeric(df$Age)`
  2. Data is sorted by Age: `df <- df[order(df$Age), ]`
- Apply to all modules with download functionality

**Files to modify:**
- `/home/jorge/repositories/un_apps/lifetableprojection/R/mod_heaping.R` (download handler)
- `/home/jorge/repositories/un_apps/lifetableprojection/R/mod_smoothing.R` (download handler)
- `/home/jorge/repositories/un_apps/lifetableprojection/R/mod_graduation.R` (download handler)
- `/home/jorge/repositories/un_apps/lifetableprojection/R/mod_odap.R` (download handler)

---

- [ ] Task 10: Graduation Download - Include Plot in Download
**Note:** No tests need to be written for these tasks.

**Goal:** The graduation module download currently only exports the data CSV. Add the plot (as PNG or PDF) to the download.

**Changes needed:**
- Modify graduation download handler to create a zip containing:
  1. The CSV data file (existing)
  2. The graduation plot as PNG (e.g., `graduation_plot.png`)
- Use `ggplot2::ggsave()` to save the plot to a temp file before adding to zip

**Files to modify:**
- `/home/jorge/repositories/un_apps/lifetableprojection/R/mod_graduation.R` (download handler ~lines 791-827)

---

## Notes

- Each task is independent and can be implemented separately
- Tasks 4 and 5 are related but distinct (tabs vs info buttons)
- All changes should use `shiny.i18n` for user-facing text
- Follow existing code patterns in the modules for consistency
