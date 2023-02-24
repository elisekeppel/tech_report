remotes::install_github("pbs-assess/csasdown")
setwd("C:/Users/KeppelE/Desktop")
csasdown::draft("techreport")
bookdown::render_book("index.Rmd")
# go check _book/techreport.pdf


```{r include=FALSE, message = FALSE, warning=FALSE, cache = TRUE}

```