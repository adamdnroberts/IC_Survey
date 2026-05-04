library(paws.storage)
library(dplyr)

readRenviron("C:/Users/adamd/Documents/IC_Survey/.Renviron")

s3_client <- paws.storage::s3()

cutoff <- as.POSIXct("2026-04-21 15:48:26", tz = "UTC")

all_objects <- list()
continuation_token <- NULL
repeat {
  args <- list(Bucket = Sys.getenv("S3_BUCKET"), Prefix = "wave2/")
  if (!is.null(continuation_token)) {
    args$ContinuationToken <- continuation_token
  }
  page <- do.call(s3_client$list_objects_v2, args)
  all_objects <- c(all_objects, page$Contents)
  if (!isTRUE(page$IsTruncated)) {
    break
  }
  continuation_token <- page$NextContinuationToken
}

if (length(all_objects) == 0) {
  stop("No responses found in S3 bucket.")
}

new_objects <- Filter(
  function(obj) {
    as.POSIXct(obj$LastModified, tz = "UTC") > cutoff
  },
  all_objects
)

cat(length(new_objects), "responses\n")

if (length(new_objects) == 0) {
  stop("No responses found after cutoff date.")
}

all_responses2 <- dplyr::bind_rows(lapply(new_objects, function(obj) {
  raw <- s3_client$get_object(Bucket = Sys.getenv("S3_BUCKET"), Key = obj$Key)
  read.csv(
    text = rawToChar(raw$Body),
    stringsAsFactors = FALSE,
    colClasses = "character"
  )
}))

cat(nrow(all_responses2), "total\n")
cat(sum(is.na(all_responses2$Netquest_PID)), "missing Netquest_PID\n")
all_responses2 <- filter(all_responses2, !is.na(Netquest_PID))

print(
  paste0(
    "pct failing attention check: ",
    round(
      sum(all_responses2$Attention_Check != "somewhat_agree") /
        nrow(all_responses2),
      3
    ) *
      100,
    "%"
  )
)

print(
  paste0(
    "# failing attention check: ",
    sum(all_responses2$Attention_Check != "somewhat_agree")
  )
)

saveRDS(
  all_responses2,
  "C:/Users/adamd/Documents/IC_Survey/data/wave2_responses.rds"
)
