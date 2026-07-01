library(paws.storage)
library(dplyr)

readRenviron("C:/Users/adamd/Documents/IC_Survey/.Renviron")

s3_client <- paws.storage::s3()

all_objects <- list()
continuation_token <- NULL
repeat {
  args <- list(Bucket = Sys.getenv("S3_BUCKET"), Prefix = "wave1/")
  if (!is.null(continuation_token)) args$ContinuationToken <- continuation_token
  page <- do.call(s3_client$list_objects_v2, args)
  all_objects <- c(all_objects, page$Contents)
  if (!isTRUE(page$IsTruncated)) break
  continuation_token <- page$NextContinuationToken
}

if (length(all_objects) == 0) {
  stop("No responses found in S3 bucket.")
}

all_responses <- dplyr::bind_rows(lapply(all_objects, function(obj) {
  raw <- s3_client$get_object(Bucket = Sys.getenv("S3_BUCKET"), Key = obj$Key)
  read.csv(
    text = rawToChar(raw$Body),
    stringsAsFactors = FALSE,
    colClasses = "character"
  )
}))

cat(nrow(all_responses), "total\n")
cat(sum(is.na(all_responses$Netquest_PID)), "missing Netquest_PID\n")
all_responses <- filter(all_responses, !is.na(Netquest_PID))

# Save the full, unfiltered set (still contains the bad/duplicate cross-wave
# matches that the filter below removes). This is the filter's input.
saveRDS(
  all_responses,
  "C:/Users/adamd/Documents/IC_Survey/data/wave1_responses_w_duplicates.rds"
)

# Remove implausible-timing and duplicate cross-wave matches. Reads the full set
# above plus data/wave2_responses.rds and the match files; creates
# `wave1_filtered`.
source("code/filter_wave1_bad_links.R")

# wave1_responses.rds is the canonical (filtered) wave-1 dataset.
saveRDS(
  wave1_filtered,
  "C:/Users/adamd/Documents/IC_Survey/data/wave1_responses.rds"
)
