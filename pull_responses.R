library(paws.storage)

readRenviron("C:/Users/adamd/Documents/IC_Survey/.Renviron")

s3_client <- paws.storage::s3()
objects <- s3_client$list_objects_v2(
  Bucket = Sys.getenv("S3_BUCKET"),
  Prefix = "responses/"
)

responses <- lapply(objects$Contents, function(obj) {
  raw <- s3_client$get_object(Bucket = Sys.getenv("S3_BUCKET"), Key = obj$Key)
  read.csv(text = rawToChar(raw$Body))
})

all_responses <- do.call(rbind, responses)
