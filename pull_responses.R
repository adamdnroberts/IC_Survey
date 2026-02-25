library(paws.storage)

readRenviron("C:/Users/adamd/Documents/IC_Survey/.Renviron")

s3_client <- paws.storage::s3()
objects <- s3_client$list_objects_v2(
  Bucket = Sys.getenv("S3_BUCKET"),
  Prefix = "responses/"
)

if (length(objects$Contents) == 0) {
  stop("No responses found in S3 bucket.")
}

all_responses <- dplyr::bind_rows(lapply(objects$Contents, function(obj) {
  raw <- s3_client$get_object(Bucket = Sys.getenv("S3_BUCKET"), Key = obj$Key)
  read.csv(text = rawToChar(raw$Body), stringsAsFactors = FALSE)
}))

saveRDS(all_responses, "data/responses.rds")
cat("Saved", nrow(all_responses), "responses to data/responses.rds\n")
