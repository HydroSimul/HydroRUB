#' Data im- & export in UNF file
#' @name unf
#' @description
#' # UNF Format
#' - UNF0 / UNF8: **numeric** with 4 / 8 Bytes,
#' - UNF1, UNF2 / UNF4: **integer** with 1, 2, / 4 Bytes
#' # Read & Write
#' - Read data from UNF file
#' - Write data to UNF file
#' @param fn_UNF string, name of the UNF file
#' @importFrom stringr str_sub
#' @return vector of numric or integer by read, UNF file by write
#' @export
read_UNF <- function(fn_UNF) {
  mark_UNF <- str_sub(fn_UNF, -1) |> as.integer()
  type_UNF <- c("numeric", "int", "int", "", "integer", "", "", "", "", "double")[mark_UNF + 1]
  n_Byte <- ifelse(mark_UNF, mark_UNF, 4L)
  n_FileSize <- file.size(fn_UNF)
  n_DataSize <- n_FileSize / n_Byte
  readBin(fn_UNF, type_UNF, n_DataSize, n_Byte, endian = "big")
}

#' @rdname unf
#' @importFrom RCurl scp
#' @param ip_Host string, the name of the remote host or its IP address
#' @param str_Username string, the name of the user on the remote machine
#' @param str_Password string, a password for accessing the local SSH key
#' @export
read_UNF_scp <- function(fn_UNF, ip_Host, str_Password, str_Username) {
  mark_UNF <- str_sub(fn_UNF, -1) |> as.integer()
  type_UNF <- c("numeric", "int", "int", "", "integer", "", "", "", "", "double")[mark_UNF + 1]
  n_Byte <- ifelse(mark_UNF, mark_UNF, 4L)
  raw_SCP <- scp(ip_Host, fn_UNF, password = str_Password, user = str_Username)
  n_FileSize <- length(raw_SCP)
  n_DataSize <- n_FileSize / n_Byte
  readBin(raw_SCP, type_UNF, n_DataSize, n_Byte, endian = "big")
}

#' @rdname unf
#' @param data_Export vector, data, that would exported
#' @export
write_UNF <- function(data_Export, fn_UNF) {
  mark_UNF <- str_sub(fn_UNF, -1) |> as.integer()
  fct_AsType <- c(as.numeric, as.integer, as.integer, "", as.integer, "", "", "", "", as.numeric)[[mark_UNF + 1]]
  n_Byte <- ifelse(mark_UNF, mark_UNF, 4L)
  writeBin(fct_AsType(data_Export), fn_UNF, n_Byte, endian = "big")
}
