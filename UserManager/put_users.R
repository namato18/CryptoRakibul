library(aws.s3)

Sys.setenv(
  "AWS_ACCESS_KEY_ID" = "AKIAZI3NHYNJ2L5YMIHV",
  "AWS_SECRET_ACCESS_KEY" = "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",
  "AWS_DEFAULT_REGION" = "us-east-1"
)

user = c('nick')
password = c("123")
secret = "rEg9vqo61kMpB7up3kbp2Huy1mMyYQFpAdyc3OBO32dwE8m32eHcr3185aEa2d7k"
api_key = "UWG67pA2SI65uA3ZzqEzSQZbU9poUYHtOiZ5YAdV3lJXhi6dUSeanbxLlcTFrN3w"

userpass.df = data.frame(user = user,
                         password = password,
                         secret = secret,
                         api_key = api_key)

saveRDS(userpass.df, "UserManager/userpass.df.rds")


put_object(
  file = file.path("UserManager", "userpass.df.rds"), 
  object = "userpass.df.rds", 
  bucket = "cryptomlbucket/rakibul_users"
)
