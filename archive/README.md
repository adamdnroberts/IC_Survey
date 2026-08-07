# archive/

Scripts that are out of the analysis pipeline. Nothing here is run by
`code/master_script.R`, and nothing in `code/` references any of it. Kept
because it documents how the sample was actually fielded, not because it
still runs — most of it reads files that no longer exist.

- **`wave1_batches/`** — one script per wave 1 fielding batch, named by send
  date (`batch_5_18_wave1.R` … `batch_7_8_wave1.R`, plus `all_pids.R`,
  `first200_wave1.R`, `final_batch_5_29_wave1.R`). Each selected the PIDs for
  one send and wrote a list into what is now `data/temp/`. Genuinely
  operational: the date stamps are the point.

- **`laer_replication_exploratory/`** — `explore_data.R` through
  `explore_data11.R`, an incremental exploration of the LAER replication data.
  The working replication scripts stayed in `code/laer_replication/`.

Deleted rather than archived: `code/power_analysis/power_analysis_simulation.R`,
superseded by `power_analysis_simulation_v2.R`, which is what produces the two
power figures in the appendix. Its only output, `power_graph.pdf`, is
referenced by neither `main.tex` nor `appendix.tex`. Recoverable from git
history.
