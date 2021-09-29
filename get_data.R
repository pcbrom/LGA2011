library(tidyverse)
library(rvest)


# LGA 2011 V2 CPU List --------------------------------------------------------

# reference url
URL1 <- 'https://www.cpu-list.com/lga2011-cpu-list/'

# take table
tmp <- URL1 |> 
  read_html() |> 
  html_elements(xpath = '//*[@id="tablepress-10"]') |> 
  html_table() |> 
  {\(x) x[[1]]}()

# get only the processor names
tmp <- tmp |>
  select(Name) |> 
  mutate(
    Name = Name |> 
      str_remove_all('.*.Xeon ') |> 
      str_remove_all('\n') |> 
      str_remove('E5-') |> 
      str_trim()
  ) |> 
  filter(
    str_detect(string = Name, pattern = '^X')
  ) |> 
  unlist() |> 
  unique()



# intel website to collect spec sheets -------------------

# download:
# https://ark.intel.com/content/www/us/en/ark.html#@PanelLabel595

# import spreadsheets

# csv files
fl <- list.files(pattern = 'csv', full.names = T)

# function to import data
get_plan <- function(fl) {
  pl <- read_csv2(file = fl, skip = 2)
  names(pl)[1] <- 'variaveis'
  pl <- pl |> 
    pivot_longer(
      cols = -variaveis,
      names_to = 'processador',
      values_to = 'dados'
    ) |> 
    na.omit() |> 
    pivot_wider(
      names_from = variaveis,
      values_from = dados
    )
  pl <- pl |> 
    filter(`Soquetes suportados` == 'FCLGA2011') |> 
    select(
      `Número do processador`, 
      lithograph, 
      `Número de núcleos`:Cache,
      TDP
    )
  names(pl) <- c(
    'model', 'lithograph', 'cores', 
    'threads', 'freq', 'freq_max',
    'cache', 'power'
  )
  pl <- pl |> 
    mutate(
      lithograph = lithograph |>
        str_extract(pattern = '\\d+'),
      freq = freq |>
        str_extract(pattern = '[0-9]{1}\\.[0-9]{2}') |> 
        as.numeric(),
      freq_max = freq_max |>
        str_extract(pattern = '[0-9]{1}\\.[0-9]{2}') |> 
        as.numeric(),
      cache = cache |> 
        str_extract(pattern = '\\d+'),
      power = power |> 
        str_extract(pattern = '\\d+') |> 
        as.numeric()
    )
  return(pl)
}

get_plan(fl[[2]])
db <- lapply(X = fl, FUN = get_plan)
db <- db |> 
  bind_rows()
saveRDS(object = db, file = 'proc_manufacture.RDS')



# performance information --------------------------------------------- --

# import data from processors
db <- readRDS(file = 'proc_manufacture.RDS')
processador <- db$model
URLent <- paste0('https://browser.geekbench.com/v5/cpu/search?page=1&q=', processador)
length(URLent)
head(URLent)

# creating function to collect data
get_data <- function(URL) {
  base <- read_html(URL)
  all_data <- NULL
  while(T) {
    model <- base |> 
      html_elements(xpath = '//span[@class="list-col-model"]') |> 
      html_text2()
    cores <- model |> 
      str_extract('\\([0-9]{1,2}') |> 
      str_remove('\\(') |> 
      as.numeric()
    score <- base |> 
      html_elements(xpath = '//span[@class="list-col-text-score"]') |> 
      html_text2() |> 
      as.numeric() |> 
      matrix(ncol = 2, byrow = T) |> 
      data.frame() |> 
      rename(single = X1, multi = X2)
    all_data <- rbind.data.frame(
      all_data,
      cbind.data.frame(model, cores, score)
    )
    tmp <- base |> 
      html_elements(xpath = '//*[@rel="next"]') |> 
      html_attr('href') |> 
      unique() |> 
      {\(x) paste0('https://browser.geekbench.com', x)}()
    if (nchar(tmp) > 30) {
      base <- read_html(tmp)
    } else {
      break
    }
  }
  return(all_data)
}

db <- NULL; i = 1
for (urli in URLent) {
  db <- rbind.data.frame(db, get_data(URL = urli))
  cat(i/length(URLent), '\n')
  i = i + 1
  if (i %% 10 == 0) {
    cat('waiting 30s\n')
    Sys.sleep(30)
  }
}
saveRDS(object = db, file = 'bench.RDS')  


# filter the simple and v2 ------------------------------------------- ----------

tmp1 <- readRDS(file = 'proc_manufacture.RDS')
tmp2 <- readRDS(file = 'bench.RDS') |> as_tibble()

head(tmp1)
head(tmp2)

tmp2 <- tmp2 |> 
  filter(str_detect(string = model, pattern = 'v3|v4', negate = T)) |> 
  rename(cores = cores) |> 
  mutate(
    model = model |> 
      str_remove_all('.*.Xeon ') |> 
      str_remove_all('[0-9]{4} M.*') |> 
      toupper() |> 
      str_replace_all('\\s+', '') |> 
      str_trim(),
    cores = cores |> 
      as.character()
  )

db <- inner_join(tmp1, tmp2)
saveRDS(object = db, file = 'consolidated.RDS')
