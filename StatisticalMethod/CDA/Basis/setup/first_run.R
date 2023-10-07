
# 압축을 푼 드라이브를 지정하세요.

mydir <- "C:"   #  또는 "D:"

# 1:00 방향의 "->Source"를 클릭하세요. 

#-------------------------------------------------------------------------------
A_first_run <- function(mydir) {
 if (!dir.exists(file.path(".", "data"))) {
  dir.create(file.path(".", "data"))
 } else {
  message("Folder already exists")
 }
 
 file.copy(
  file.path(mydir, "Basis", "data"),
  file.path("."),
  recursive = TRUE
  )
 
files <- append(
 list.files(file.path(mydir, "statM"), 
            pattern = ".R+"
            ), 
 ".Rprofile"
 )

 file.copy(
  file.path(mydir, "Basis", files),
  file.path("."),
  overwrite = TRUE
  )
}
#-------------------------------------------------------------------------------
A_first_run(mydir)
