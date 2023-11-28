library(aws.s3)

posts = data.frame(Timestamp = character(),
                   User = character(),
                   Post.Title = character(),
                   Post.Text = character(),
                   Post.Image = character())


Sys.setenv(
  "AWS_ACCESS_KEY_ID" = "AKIAZI3NHYNJ2L5YMIHV",
  "AWS_SECRET_ACCESS_KEY" = "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",
  "AWS_DEFAULT_REGION" = "us-east-1"
)

tmp.dir = tempdir()

saveRDS(posts, paste0(tmp.dir, "/posts.RDS"))

put_object(
  file = paste0(tmp.dir,"/posts.RDS"), 
  object = "posts.RDS", 
  bucket = paste0("cryptomlbucket/rakibul_posts")
)
