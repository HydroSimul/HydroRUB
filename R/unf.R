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
#' @param data_Export vector, data, that would exported
#' @importFrom stringr str_sub
#' @return vector of numric or integer by read, UNF file by write
#' @export
read_UNF <- function(fn_UNF) {
  mark_UNF <- str_sub(fn_UNF, -1) |> as.integer()
  type_UNF <- c("numeric", "int", "int", "", "integer", "", "", "", "", "double")[mark_UNF + 1]
  n_Byte <- ifelse(mark_UNF, mark_UNF, 4L)
  n_FileSize <- file.info(fn_UNF)["size"] |> as.integer()
  n_DataSize <- n_FileSize / n_Byte
  readBin(fn_UNF, type_UNF, n_DataSize, n_Byte, endian = "big")
}


#' @rdname unf
#' @export
write_UNF <- function(data_Export, fn_UNF) {
  mark_UNF <- str_sub(fn_UNF, -1) |> as.integer()
  fct_AsType <- c(as.numeric, as.integer, as.integer, "", as.integer, "", "", "", "", as.numeric)[[mark_UNF + 1]]
  n_Byte <- ifelse(mark_UNF, mark_UNF, 4L)
  writeBin(fct_AsType(data_Export), fn_UNF, n_Byte, endian = "big")
}
