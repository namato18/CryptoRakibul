library(aws.s3)

Sys.setenv(
  "AWS_ACCESS_KEY_ID" = "AKIAZI3NHYNJ2L5YMIHV",
  "AWS_SECRET_ACCESS_KEY" = "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",
  "AWS_DEFAULT_REGION" = "us-east-1"
)

list_files = list.files('E:/bsts_T-11-26-2023', full.names = TRUE)
file_names = list.files('E:/bsts_T-11-26-2023')

# ind = grep(pattern = "BreakL", x = file_names)
# 
# list_files = list_files[ind]
# file_names = file_names[ind]

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
