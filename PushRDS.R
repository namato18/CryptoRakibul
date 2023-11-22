library(aws.s3)

Sys.setenv(
  "AWS_ACCESS_KEY_ID" = "AKIAZI3NHYNJ2L5YMIHV",
  "AWS_SECRET_ACCESS_KEY" = "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",
  "AWS_DEFAULT_REGION" = "us-east-1"
)

list_files = list.files('../bsts_T-11-17-2023', full.names = TRUE)
file_names = list.files('../bsts_T-11-17-2023')

for(i in 1:length(list_files)){
  
  temp.loc = tempdir()
  
  x = readRDS(list_files[i])
  
  saveRDS(x, paste0(temp.loc,"/",file_names[i]))
  
  put_object(
    file = paste0(temp.loc,"/",file_names[i]), 
    object = file_names[i], 
    bucket = paste0("cryptomlbucket/bsts_T/bsts_T")
  )
  
  print(paste0(i, " out of: ", length(list_files)))
}
