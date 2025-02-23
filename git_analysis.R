#!/usr/bin/env Rscript
# /* Copyright (C) 2025 Athanasios Natsis <natsisphysicist@gmail.com> */
#' ---
#' title:         "Plot git stats"
#' author:        "Natsis Athanasios"
#' institute:     "AUTH"
#' affiliation:   "Laboratory of Atmospheric Physics"
#' documentclass: article
#' classoption:   a4paper,oneside
#' fontsize:      10pt
#' geometry:      "left=0.5in,right=0.5in,top=0.5in,bottom=0.5in"
#'
#' link-citations:  yes
#' colorlinks:      yes
#'
#' header-includes:
#' - \usepackage{caption}
#' - \usepackage{placeins}
#' - \captionsetup{font=small}
#'
#' output:
#'   html_document:
#'     toc:        true
#'     fig_width:  7.5
#'     fig_height: 5
#'
#'   bookdown::pdf_document2:
#'     number_sections:  no
#'     fig_caption:      no
#'     keep_tex:         no
#'     latex_engine:     xelatex
#'     toc:              yes
#'     toc_depth:        4
#'     fig_width:        8
#'     fig_height:       5
#'
#' date: "`r format(Sys.time(), '%F')`"
#'
#' ---

#+ include=F
#'
#' https://www.r-bloggers.com/2018/03/guide-to-tidy-git-analysis/
#'
#'

#+ include=F
## __ Document options  --------------------------------------------------------
knitr::opts_chunk$set(comment   = ""      )
knitr::opts_chunk$set(dev       = "png"   )
knitr::opts_chunk$set(out.width = "100%"  )
knitr::opts_chunk$set(fig.align = "center")
knitr::opts_chunk$set(warning   = FALSE   )
knitr::opts_chunk$set(fig.cap   = " empty caption ")
knitr::opts_chunk$set(fig.pos   = '!h'    )
knitr::opts_chunk$set(tidy = TRUE,
                      tidy.opts = list(
                        indent       = 4,
                        blank        = FALSE,
                        comment      = FALSE,
                        args.newline = TRUE,
                        arrow        = TRUE)
)

## __ Set environment  ---------------------------------------------------------
closeAllConnections()
Sys.setenv(TZ = "UTC")
tic <- Sys.time()
Script.Name  <- "~/CODE/git_analysis/git_analysys.R"

if (!interactive()) {
  pdf(file = paste0("~/CODE/git_analysis/RUNTIME/", basename(sub("\\.R$", ".pdf", Script.Name))))
}


## __ Load libraries  ----------------------------------------------------------
library(data.table, warn.conflicts = FALSE, quietly = TRUE)
library(glue,       warn.conflicts = FALSE, quietly = TRUE)
library(stringr,    warn.conflicts = FALSE, quietly = TRUE)
library(forcats,    warn.conflicts = FALSE, quietly = TRUE)
library(tidyverse,  warn.conflicts = FALSE, quietly = TRUE)
library(tidygraph,  warn.conflicts = FALSE, quietly = TRUE)
library(ggraph,     warn.conflicts = FALSE, quietly = TRUE)
library(lubridate,  warn.conflicts = FALSE, quietly = TRUE)
library(tidytext,   warn.conflicts = FALSE, quietly = TRUE)
library(ggplot2,    warn.conflicts = FALSE, quietly = TRUE)
library(pander,     warn.conflicts = FALSE, quietly = TRUE)

panderOptions('table.alignment.default', 'left')

#+ include=T, echo=F, results="asis", warnings=F
##  Variables  -----------------------------------------------------------------

folders <- unique(sort(c(
  "~/.dot_files/",
  "~/.dotfiles/",
  "~/BASH/",
  "~/Aerosols/",
  "~/BBand_LAP/",
  "~/CODE/",
  "~/CODE/Clothes_drying/",
  "~/CODE/deploy/",
  "~/CODE/fi_analysis/",
  "~/CODE/git_analysis/",
  "~/CODE/nixos/",
  "~/MANUSCRIPTS/01_2022_sdr_trends/",
  "~/MANUSCRIPTS/02_2024_enhancement/",
  "~/MANUSCRIPTS/03_thesis/",
  "~/MANUSCRIPTS/presentations/",
  "~/NOTES/",
  "~/PANDOC/CHP1_measurements_guide/",
  "~/PANDOC/Libradtran_guide/",
  "~/PANDOC/thanasisnsite/",
  NULL
)))

allgit   <- data.table()
commitsl <- data.table()

for (repodir in folders) {

  if (!dir.exists(repodir)) next()

  cat(paste("## ", repodir, "\n\n"))

  ## get data from git log
  log_format_options <- c(datetime = "cd", commit = "h", parents = "p", author = "an", subject = "s")
  option_delim <- "\t"
  log_format   <- glue("%{log_format_options}") %>% glue_collapse(option_delim)
  log_options  <- glue('--pretty=format:"{log_format}" --date=format:"%Y-%m-%d %H:%M:%S" --name-status')
  log_cmd      <- glue('git -C {repodir} log {log_options}')
  lines        <- system(log_cmd, intern = TRUE)

  ## separate commits from file lists
  breaks <- which(grepl("^[[:space:]]*$", lines))
  start  <- c(1, breaks + 1)
  end    <- c(breaks, length(lines))
  comits <- lines[start]

  tt     <- data.frame(start = start, end = end)
  tt$ddd <- apply(tt, 1, function(x) (lines[c((x[1]+1):(x[2]-1))]), simplify = T  )

  history_logs <- comits %>%
    str_split_fixed(option_delim, length(log_format_options)) %>%
    as_tibble() %>%
    setNames(names(log_format_options))

  history_logs$repo <- basename(repodir)
  commitsl          <- rbind(commitsl, history_logs)


  ## align all files with commits
  history_logs$files <- tt$ddd
  history_logs       <- unnest(history_logs, files)

  ## split file and status
  history_logs <- history_logs |>
    separate(col = files, into = c("file_status", "file"), sep = "\t") |>
    data.table()
  history_logs$file_exists <- file.exists(paste0(repodir, "/", history_logs$file))

  history_logs$repo <- basename(repodir)
  allgit            <- rbind(allgit, history_logs)


  c <- history_logs[, .(N = .N) , by = .(date = as.Date(datetime))] |>
    ggplot() +
    geom_col(aes(x = date, y = N)) +
    labs(title = paste(basename(repodir), "commits by day"))
  show(c)


  w <- history_logs[, .(N = .N) , by = .(date = as.Date(as.numeric(as.Date(datetime))%/%7 * 7, origin = origin))] |>
    ggplot() +
    geom_col(aes(x = date, y = N)) +
    labs(title = paste(basename(repodir), "commits by week"))
  show(w)

  cat(pander(
    history_logs[, .(N      = .N,
                     exists = unique(file_exists)), by = .(file)] |>
      arrange(N) |> tail(n = 30),
    caption = "Most commited files"
  ))

  cat(pander(
    history_logs[, .N, by = repo] |> arrange(N),
    caption = "Number of commits"
  ))

}


# cat(paste("## All repos\n\n"))


c <- allgit[, .(N = .N) , by = .(date = as.Date(datetime), repo = repo)] |>
  ggplot() +
  geom_col(aes(x = date, y = N, colour = repo)) +
  labs(title = "Commits by day")
show(c)

# w <- allgit[, .(N = .N) , by = .(date = as.Date(as.numeric(as.Date(allgit$datetime))%/%7 * 7, origin = origin), repo = repo)] |>
#   ggplot() +
#   geom_point(aes(x = date, y = N, colour = repo)) +
#   labs(title = "Commits by week")
# show(w)

w <- allgit[, .(N = .N) , by = .(date = as.Date(as.numeric(as.Date(allgit$datetime))%/%7 * 7, origin = origin), repo = repo)] |>
  ggplot() +
  geom_col(aes(x = date, y = N, colour = repo)) +
  labs(title = "Commits by week")
show(w)


pander(
  allgit[, .(N = .N) , by = .(file, repo = repo)] |> arrange(N) |> tail(n = 40),
  caption = "Most commited files"
)

pander(
  allgit[, .(N = .N) , by = .(repo = repo)]
)

pander(
  commitsl[, .N, by = repo] |> arrange(N)
)










#
#
# history_logs <- history_logs %>%
#   mutate(parents = str_split(parents, " "))
#
# # Start with NA
# history_logs <- history_logs %>% mutate(branch = NA_integer_)
#
# # Create a boolean vector to represent free columns (1000 should be plenty!)
# free_col <- rep(TRUE, 1000)
#
# for (i in seq_len(nrow(history_logs) - 1)) { # - 1 to ignore root
#   # Check current branch col and assign open col if NA
#   branch <- history_logs$branch[i]
#
#   if (is.na(branch)) {
#     branch <- which.max(free_col)
#     free_col[branch] <- FALSE
#     history_logs$branch[i] <- branch
#   }
#
#   # Go through parents
#   parents <- history_logs$parents[[i]]
#
#   for (p in parents) {
#     parent_col <- history_logs$branch[history_logs$commit == p]
#
#     # If col is missing, assign it to same branch (if first parent) or new
#     # branch (if other)
#     if (is.na(parent_col)) {
#       parent_col <- if_else(p == parents[1], branch, which.max(free_col))
#
#       # If NOT missing this means a split has occurred. Assign parent the lowest
#       # and re-open both cols (parent closed at the end)
#     } else {
#       free_col[c(branch, parent_col)] <- TRUE
#       parent_col <- min(branch, parent_col)
#
#     }
#
#     # Close parent col and assign
#     free_col[parent_col] <- FALSE
#     history_logs$branch[history_logs$commit == p] <- parent_col
#   }
# }
#
# history_logs %>%
#   count(author, sort = TRUE)
#
#
# # istory_logs <- history_logs %>%
# #   mutate(author = case_when(
# #     str_detect(tolower(author), "hadley") ~ "Hadley Wickham",
# #     str_detect(tolower(author), "kohske takahashi") ~ "Kohske Takahashi",
# #     TRUE ~ str_to_title(author)
# #   ))
#
# history_logs %>%
#   count(author) %>%
#   arrange(desc(n))
#
# history_logs %>%
#   count(author) %>%
#   top_n(10, n) %>%
#   mutate(author = fct_reorder(author, n)) %>%
#   ggplot(aes(author, n)) +
#   geom_col(aes(fill = n), show.legend = FALSE) +
#   coord_flip() +
#   theme_minimal() +
#   ggtitle("ggplot2 authors with most commits") +
#   labs(x = NULL, y = "Number of commits", caption = "Post by @drsimonj")
#
#
# # Convert commit to a factor (for ordering nodes)
# history_logs <- history_logs %>%
#   mutate(commit = factor(commit))
#
# # Nodes are the commits (keeping relevant info)
# nodes <- history_logs %>%
#   select(-parents) %>%
#   arrange(commit)
#
# # Edges are connections between commits and their parents
# edges <- history_logs %>%
#   select(commit, parents) %>%
#   unnest(parents) %>%
#   mutate(parents = factor(parents, levels = levels(commit))) %>%
#   transmute(from = as.integer(parents), to = as.integer(commit)) %>%
#   drop_na()
#
# # Create tidy directed graph object
# git_graph <- tbl_graph(nodes = nodes, edges = edges, directed = TRUE)
#
# git_graph %>%
#   ggraph() +
#   geom_edge_link(alpha = .1) +
#   geom_node_point(aes(color = factor(branch)), alpha = .3) +
#   theme_graph() +
#   theme(legend.position = "none")
#
# ggraph_git <- . %>%
#   # Set node x,y coordinates
#   activate(nodes) %>%
#   mutate(x = datetime, y = branch) %>%
#   # Plot with correct layout
#   create_layout(layout = "manual", node.positions = as_tibble(activate(., nodes))) %>%
#   {ggraph(., layout = "manual") + theme_graph() + labs(caption = "Post by @drsimonj")}
#
#
# git_graph %>%
#   ggraph_git() +
#   geom_edge_link(alpha = .1) +
#   geom_node_point(aes(color = factor(branch)), alpha = .3) +
#   theme(legend.position = "none") +
#   ggtitle("Commit history of ggplot2")
#
#
# git_graph %>%
#   activate(nodes) %>%
#   filter(datetime > "2015-11-01", datetime < "2016-08-01") %>%
#   ggraph_git() +
#   geom_edge_link(alpha = .1) +
#   geom_node_point(aes(color = factor(branch)), alpha = .3) +
#   theme(legend.position = "none") +
#   ggtitle("Git history of ggplot2",
#           subtitle = "2015-11 to 2016-08")
#
#
# # 10 most-common authors
# top_authors <- git_graph %>%
#   activate(nodes) %>%
#   as_tibble() %>%
#   count(author, sort = TRUE) %>%
#   top_n(10, n) %>%
#   pull(author)
#
# # Plot
# git_graph %>%
#   activate(nodes) %>%
#   filter(datetime > "2015-11-01", datetime < "2016-08-01") %>%
#   mutate(author = factor(author, levels = top_authors),
#          author = fct_explicit_na(author, na_level = "Other")) %>%
#   ggraph_git() +
#   geom_edge_link(alpha = .1) +
#   geom_node_point(aes(color = author), alpha = .3) +
#   theme(legend.position = "bottom") +
#   ggtitle("ggplot2 commits by author",
#           subtitle = "2015-11 to 2016-08")
#
#
# data(stop_words)
#
# tidy_subjects <- history_logs %>%
#   unnest_tokens(word, subject) %>%
#   anti_join(stop_words)
# #> Joining, by = "word"
#
# tidy_subjects
# #> # A tibble: 16,477 x 6
# #>    datetime            commit   parents   author       branch word
# #>    <chr>               <fct>    <list>    <chr>         <int> <chr>
# #>  1 2018-03-22 18:42:25 3c9c504f <chr [2]> Lionel Henry      1 merge
# #>  2 2018-03-22 18:42:25 3c9c504f <chr [2]> Lionel Henry      1 pull
# #>  3 2018-03-22 18:42:25 3c9c504f <chr [2]> Lionel Henry      1 request
# #>  4 2018-03-22 18:42:25 3c9c504f <chr [2]> Lionel Henry      1 2491
# #>  5 2018-03-22 18:42:25 3c9c504f <chr [2]> Lionel Henry      1 tidyverse
# #>  6 2018-03-22 18:42:25 3c9c504f <chr [2]> Lionel Henry      1 tidyeval
# #>  7 2018-03-22 18:42:25 3c9c504f <chr [2]> Lionel Henry      1 facets
# #>  8 2018-03-22 17:55:23 449bc039 <chr [1]> Lionel Henry      2 remove
# #>  9 2018-03-22 17:55:23 449bc039 <chr [1]> Lionel Henry      2 dependency
# #> 10 2018-03-22 17:55:23 449bc039 <chr [1]> Lionel Henry      2 plyr
# #> # ... with 16,467 more rows
# #>
# #>
# #>
# #>
#
# tidy_subjects %>%
#   count(word) %>%
#   top_n(10, n) %>%
#   mutate(word = fct_reorder(word, n)) %>%
#   ggplot(aes(word, n)) +
#   geom_col(aes(fill = n), show.legend = FALSE) +
#   coord_flip() +
#   theme_minimal() +
#   ggtitle("Most-used words in ggplot2 commit subjects") +
#   labs(x = NULL, y = "Word frequency", caption = "Post by @drsimonj")
#
#
# history_logs %>%
#   select(commit, author, subject) %>%
#   unnest_tokens(bigram, subject, token = "ngrams", n = 2) %>%
#   separate(bigram, c("word1", "word2"), sep = " ") %>%
#   filter(word1 == "fix") %>%
#   anti_join(stop_words, by = c("word2" = "word")) %>%
#   count(word2, sort = TRUE)


#+ include=T, echo=F, results="asis"
tac <- Sys.time()
cat(sprintf("**END** %s %s@%s %s %f mins\n\n", Sys.time(), Sys.info()["login"],
            Sys.info()["nodename"], basename(Script.Name), difftime(tac,tic,units = "mins")))
