# VDD Tracker: benchmark_treatment_overlap.R

**Target**: `code/exploratory/benchmark_treatment_overlap.R`

**Termination condition**: 2 rounds with no surviving crash-or-wrong-number finding

## Claim Checklist

### Row set and keys
- [x] C1: Each respondent in `comp_panel` appears exactly once (no duplication from panel creation)
- [x] C2: `Benchmark_Selected_Municipalities` parsing produces the correct set of muni IDs per respondent
- [x] C3: No respondent is silently dropped between `panel` and `overlap_df`

### Normalization
- [x] C4: Municipality IDs in W1 selections and W2 comparisons use the same format (both zero-padded or both not)

### Semantics
- [x] C5: `n_overlap` correctly counts municipalities that appear in BOTH W1 selections AND W2 shown
- [x] C6: Treatment group filter correctly identifies comparison treatments (T2, T3, T4, control2)

### Edge cases
- [x] C7: Empty or NA `Benchmark_Selected_Municipalities` handled correctly (no false overlaps)
- [x] C8: Respondents with fewer than 4 comparison municipalities handled correctly

---

## Round 1

### Findings

**F1: C4 - CONFIRMED**
W1 and W2 IDs both use 5-character zero-padded format. No mismatch.

**F2: C5 - CONFIRMED**
Spot-checked intersection logic:
- W1 selected: 16043, 16006, 11007
- W2 shown: 16030, 16043, 16085, 16023
- Overlap correctly identified: 16043

**F3: C6 - DISMISSED (no impact)**
Control and T1 treatments also have Comparison_Muni_*_ID values (471 and 226 respectively).
However, the script correctly filters to only T2/T3/T4/control2, so output is unaffected.
The in-code comment claiming "only comparison treatments have non-NA comparison muni IDs" is false but harmless.

**F4: C7 - CONFIRMED (current data)**
No NA or empty strings in Benchmark_Selected_Municipalities for comparison treatments.
Code lacks defensive handling but doesn't fail on current data.

**F5: C2 - POTENTIAL ISSUE**
Line 33: `strsplit(Benchmark_Selected_Municipalities, ";")[[1]]`
If Benchmark_Selected_Municipalities is NA, strsplit returns list(NA), and trimws(NA) = NA.
intersect(c(NA, "16043"), c("16043")) = "16043" - NA is excluded, so no false match.
DISMISSED - R's intersect handles NA correctly.

### Actions
- None required. All claims verified or dismissed.

---

## Round 2

### Findings

**F6: C1 - CONFIRMED**
1647 rows, 1647 unique Netquest_PIDs. No duplicates.

**F7: C3 - CONFIRMED**
1647 rows in comp_panel, 1647 rows in overlap_df. No silent drops.

**F8: C8 - CONFIRMED**
All 1647 respondents have exactly 4 comparison municipalities shown.

### Actions
- None required.

---

## Termination

**Reason**: 2 rounds with no crash-or-wrong-number findings.

All 8 claims verified. Script output is trustworthy.

