#' Gera um sumário (TOC) a partir dos títulos "#" de um .Rmd ou .md
#' Os ids seguem a regra do Pandoc, que é a que o site usa:
#'  minúsculas; só letras, números, "_", "-" e "."; espaço vira "-";
#'  tira o que vier antes da primeira letra; repetidos ganham -1, -2...
#'
#' Uso (em um chunk com echo=FALSE):
#'   render_toc("/caminho/do/arquivo.Rmd")

slug_pandoc <- function(texto) {
  s <- gsub("\\[([^]]*)\\]\\([^)]*\\)", "\\1", texto)     # [texto](link) -> texto
  s <- gsub("[*`]", "", s)                                # negrito e código
  # _itálico_ (mas não o "_" no meio de uma palavra, como escore_z)
  s <- gsub("(^|[^[:alnum:]])_([^_]+)_($|[^[:alnum:]])", "\\1\\2\\3", s)
  s <- tolower(s)
  s <- gsub("[^[:alnum:]_. -]", "", s)                    # mantém _ - . e espaço
  s <- gsub(" ", "-", s)                                  # cada espaço vira hífen
  s <- sub("^[^[:alpha:]]+", "", s)                       # tira até a primeira letra
  if (s == "") s <- "section"
  s
}

render_toc <- function(filename,
                       toc_header_name = "Table of Contents",
                       base_level = NULL,
                       toc_depth = 3) {
  x <- readLines(filename, warn = FALSE, encoding = "UTF-8")
  x <- paste0("\n", paste(x, collapse = "\n"), "\n")
  x <- sub("(?s)^\\s*---\n.*?\n---\n", "\n", x, perl = TRUE)   # tira o front matter
  for (i in 5:3) {                                            # tira os blocos de código
    regex_code_fence <- paste0("\n[`]{", i, "}.+?[`]{", i, "}\n")
    x <- gsub(regex_code_fence, "\n", x)
  }
  linhas <- strsplit(x, "\n")[[1]]
  linhas <- linhas[grepl("^#+\\s", linhas)]
  if (length(linhas) == 0) return(knitr::asis_output(""))
  
  nivel <- nchar(sub("^(#+).*", "\\1", linhas))
  texto <- trimws(sub("^#+\\s+", "", linhas))
  
  # id escrito à mão: ## Título {#meu-id}
  tem_id <- grepl("\\{[^}]*#[^ }]+[^}]*\\}\\s*$", texto)
  id_manual <- ifelse(tem_id, sub(".*\\{[^}]*#([^ }]+)[^}]*\\}\\s*$", "\\1", texto), NA)
  texto <- trimws(sub("\\s*\\{[^}]*\\}\\s*$", "", texto))
  
  # ids de TODOS os títulos, na ordem do documento (os repetidos ganham -1, -2...)
  ids <- character(length(texto)); vistos <- character()
  for (k in seq_along(texto)) {
    id <- if (!is.na(id_manual[k])) id_manual[k] else slug_pandoc(texto[k])
    n <- sum(vistos == id); vistos <- c(vistos, id)
    ids[k] <- if (n > 0) paste0(id, "-", n) else id
  }
  
  manter <- rep(TRUE, length(texto))
  if (!is.null(toc_header_name))
    manter <- manter & !grepl(paste0("^", toc_header_name), texto)
  if (!any(manter)) return(knitr::asis_output(""))
  if (is.null(base_level)) base_level <- min(nivel[manter])
  rel <- nivel - base_level
  if (any(rel[manter] < 0))
    stop("Há títulos com nível menor que o base_level. Ajuste `base_level`.")
  manter <- manter & rel <= toc_depth - 1
  
  # ignora o que vem antes do primeiro título do nível base
  primeiro <- which(manter & rel == 0)[1]
  if (is.na(primeiro)) return(knitr::asis_output(""))
  manter <- manter & seq_along(texto) >= primeiro
  
  itens <- paste0(strrep(" ", rel[manter] * 4), "- [", texto[manter], "](#", ids[manter], ")")
  knitr::asis_output(paste(itens, collapse = "\\\n"))
}