## Select files with regex
## --------------------------------------
## grep.file - Get downloaded CSV file from SSB. Use regular expression to get all relevant files,
## eg. "jan2019" will grep all file with "jan2019" for both new and change
## grep.change - Get the changes file in xlsx copy/paste from SSB that is related to regexp in
## 'grep.file' and file eg. "change". Regexp will find word "change" for all "jan2019"

select_ssb <- function(grep.file, grep.change, fpath){
  files <- fs::dir_ls(fpath)
  filInd <- grep(grep.file, files, ignore.case = TRUE)
  chgInd <- grep(grep.change, files[filInd])
  chgFil <- files[filInd][chgInd]
  codeList <- files[filInd][-chgInd]
  list(chgfile = chgFil, allfile = codeList)
}
