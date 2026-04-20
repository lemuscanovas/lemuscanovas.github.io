library(htmltools)
library(stringr)
library(dplyr)
library(readr)
library(glue)

knitr::opts_chunk$set(
  collapse = TRUE,
  warning = FALSE,
  message = FALSE,
  fig.retina = 3,
  comment = "#>"
)

# ---- Publication helpers ----

get_pubs <- function() {
  pubs <- read_csv(here::here("content", "pubs.csv"), show_col_types = FALSE)
  pubs <- make_citations(pubs)
  pubs$summary <- ifelse(is.na(pubs$summary), FALSE, pubs$summary)
  return(pubs)
}

make_citations <- function(pubs) {
  pubs$citation <- unlist(lapply(split(pubs, seq_len(nrow(pubs))), make_citation))
  return(pubs)
}

make_citation <- function(pub) {
  if (!is.na(pub$journal) && pub$journal != "") {
    pub$journal <- glue("_{pub$journal}_.")
  }
  if (!is.na(pub$number) && pub$number != "") {
    pub$number <- glue("{pub$number}.")
  }
  if (!is.na(pub$doi) && pub$doi != "") {
    pub$doi <- make_doi(pub$doi)
  }
  pub$year <- glue("({pub$year})")
  pub$title <- glue('"{pub$title}"')
  pub[, which(is.na(pub))] <- ""
  return(paste(pub$author, pub$year, pub$title, pub$journal,
               pub$number, pub$doi))
}

make_doi <- function(doi) {
  return(glue("DOI: [{doi}](https://doi.org/{doi})"))
}

make_pub_list <- function(pubs, category) {
  x <- pubs[which(pubs$category == category), ]
  if (nrow(x) == 0) return(HTML(""))
  pub_list <- list()
  for (i in seq_len(nrow(x))) {
    pub_list[[i]] <- make_pub(x[i, ], index = i)
  }
  return(HTML(paste(unlist(pub_list), collapse = "")))
}

make_pub <- function(pub, index = NULL) {
  if (is.null(index)) {
    cite <- pub$citation
    icons <- make_icons(pub)
  } else {
    cite <- glue("{index}) {pub$citation}")
    icons <- glue('<ul style="list-style: none;"><li>{make_icons(pub)}</li></ul>')
  }
  return(HTML(glue(
    '<div class="pub">
      <div class="g-col-11">{markdown_to_html(cite)}</div>
      {icons}
    </div>'
  )))
}

make_icons <- function(pub) {
  html <- c()
  if (!is.na(pub$url_pub) && pub$url_pub != "") {
    html <- c(html, as.character(icon_link(
      icon = "fas fa-external-link-alt", text = "View", url = pub$url_pub
    )))
  }
  if (!is.na(pub$url_pdf) && pub$url_pdf != "") {
    html <- c(html, as.character(icon_link(
      icon = "fa fa-file-pdf", text = "PDF", url = pub$url_pdf
    )))
  }
  if (!is.na(pub$url_repo) && pub$url_repo != "") {
    html <- c(html, as.character(icon_link(
      icon = "fab fa-github", text = "Code", url = pub$url_repo
    )))
  }
  if (!is.na(pub$url_rg) && pub$url_rg != "") {
    html <- c(html, as.character(icon_link(
      icon = "ai ai-researchgate", text = "RG", url = pub$url_rg
    )))
  }
  return(paste(html, collapse = ""))
}

markdown_to_html <- function(text) {
  if (is.null(text) || is.na(text)) return(text)
  return(HTML(markdown::renderMarkdown(text = text)))
}

icon_link <- function(icon = NULL, text = NULL, url = NULL,
                      class = "icon-link", target = "_blank") {
  if (!is.null(icon)) {
    text <- HTML(paste0('<i class="', icon, '"></i> ', text))
  }
  return(a(href = url, text, class = class, target = target, rel = "noopener"))
}

make_icon <- function(icon) {
  return(tag("i", list(class = icon)))
}

last_updated <- function() {
  return(span(
    paste0("Last updated on ", format(Sys.Date(), format = "%B %d, %Y")),
    style = "font-size:0.8rem;"
  ))
}
