library(aws.s3)

Sys.setenv(
  "AWS_ACCESS_KEY_ID" = "AKIAZI3NHYNJ2L5YMIHV",
  "AWS_SECRET_ACCESS_KEY" = "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",
  "AWS_DEFAULT_REGION" = "us-east-1"
)

credentials1 = s3read_using(FUN = readRDS, bucket = paste0("cryptomlbucket/APIKeys"), object = "credentials.rds")

df = data.frame(User = "eb1tdaddy",
                Password = "MRen*1177$",
                APIKey = "CSd5FtnzmasGAdqD4hXthrekaMtprukLMh8ZhtLLZhQuTSEn82jGH7mtWGZoAhQg",
                APISecret = "BIAwNDCD8wE0vbTsOohewRj4pKepyBTuqSYVjwdU5CLSRRQEQAB2AEmPmQqFTQ71")

credentials1 = rbind(credentials1, df)
# list_files = list.files('E:/bsts_T-11-26-2023', full.names = TRUE)
# file_names = list.files('E:/bsts_T-11-26-2023')

# ind = grep(pattern = "BreakL", x = file_names)
# 
# list_files = list_files[ind]
# file_names = file_names[ind]

for(i in 1:length(list_files)){
  
  temp.loc = tempdir()
  
  x = readRDS(list_files[i])
  
  saveRDS(credentials1, paste0(temp.loc,"/x.rds"))
  
  put_object(
    file = paste0(temp.loc,"/x.rds"), 
    object = "credentials.rds", 
    bucket = paste0("cryptomlbucket/APIKeys")
  )
  
  print(paste0(i, " out of: ", length(list_files)))
}
