# update save file
resave <- function(..., list = character(), file) {
  previous  <- load(file)
  var.names <- c(list, as.character(substitute(list(...)))[-1L])
  for (var in var.names) assign(var, get(var, envir = parent.frame()))
  save(list = unique(c(previous, var.names)), file = file)
}
# slice text in n bins
slice_text <- function(text,bin){
  sliced.text.l <- split(text, cut(1:length(text),bin))
}
