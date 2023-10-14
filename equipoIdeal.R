
# 📚 Librerias--------------------------------------------------------------------------

library(tidyverse)
library(rvest)
library(janitor)
library(glue)


# 💾 Scrap, load  & clean data ---------------------------------------------------------------


caras <- read.csv("https://raw.githubusercontent.com/IvoVillanueva/acb/main/nombres_fotos_2024_1.csv") |>
  select(jug = nombre, lic) %>%
  mutate(lic = case_when(
    str_detect(lic, "JFL") ~ "JFL",
    str_detect(lic, "COT") ~ "COT",
    str_detect(lic, "EXT") ~ "EXT",
    str_detect(lic, "EUR") ~ "EUR",
    TRUE ~ lic
  ))

#Scrap

pos <- c("Bases", "Aleros", "Pívots")

bt <- function(pos) {
  b <- "https://www.rincondelmanager.com/smgr/broker.php?pos=B" |>
    read_html()
  df <- b |>
    html_element(glue::glue("div#{pos}")) |>
    html_table() |>
    janitor::row_to_names(row_number = 1) |>
    janitor::clean_names() |>
    mutate(
      eq = case_when(
        eq == "CBG" ~ "COV",
        eq == "OBR" ~ "MOB",
        eq == "RMA" ~ "RMB",
        eq == "BAS" ~ "BKN",
        eq == "FCB" ~ "BAR",
        eq == "CAN" ~ "LNT",
        eq == "ZAR" ~ "CAZ",
        eq == "BLB" ~ "SBB",
        eq == "MUR" ~ "UCM",
        eq == "MAN" ~ "BAX",
        eq == "PAL" ~ "ZPA",
        eq == "GCA" ~ "DGC",
        eq == "AND" ~ "MBA",
        TRUE ~ eq
      ),
      broker = as.character(broker),
      val_10 = as.numeric(str_replace(val_10, ",", ".")),
      ult3 = as.numeric(str_replace(ult3, ",", "."))
    ) |>
    mutate(across(c("broker", "sub_7"), ~ str_remove_all(., "\\.")),
      pos = glue::glue("{pos}")
    )
  return(df)
}


bt_df <- map_df(pos, bt) %>%
  left_join(caras)


# 🪅 functions ------------------------------------------------------------

# 2 PLAYERS MAX LIC EXT, AND MIN 4 PLAYERS OR MORE PLAYERS JFL
# 2 Bases 4 Aleros 4 Pívots

equipo_ideal <- function(data) {
  ext <- data %>%
    filter(lic == "EXT") %>%
    arrange(desc(ult3)) %>%
    head(2) %>%#data with only the best two EXT
    bind_rows(data %>%
      filter(lic != "EXT")) #Joint with the rest of lic

  ideal <- ext %>%
    filter(pos == "Bases") %>%
    arrange(desc(ult3)) %>%
    head(2) %>%
    bind_rows(
      data %>%
        filter(pos == "Aleros") %>%
        arrange(desc(ult3)) %>%
        head(4)
    ) %>%
    bind_rows(
      data %>%
        filter(pos == "Pívots") %>%
        arrange(desc(ult3)) %>%
        head(4)
    ) %>%
    mutate(broker = parse_number(broker)) %>%
    select(jug, eq, broker, lic, pos, ult3)

#count number of JFL and substrac for 4

  jfl <- count(ideal, lic) %>%
    ungroup() %>%
    filter(lic == "JFL") %>%
    select(n) %>%
    mutate(n = 4 - n)

  if (jfl$n <= 3) {

# 4 Best players JFL

    ideal1 <- ext %>%
      filter(lic == "JFL") %>%
      arrange(desc(ult3)) %>%
      head(4)

#extract his positions

    bases <- ideal1 %>%
      filter(pos == "Bases") %>%
      count(pos) %>%
      pull()
    aleros <- ideal1 %>%
      filter(pos == "Aleros") %>%
      count(pos)%>%
      pull()
    pivots <- ideal1 %>%
      filter(pos == "Pívots") %>%
      count(pos)%>%
      pull()

#data without JFL players for don`t duplicate

    ideal2 <- ext %>%
      filter(lic != "JFL")

#data without Jfl 4 players positions

    ideal3 <- ideal2 %>%
      filter(pos == "Bases") %>%
      arrange(desc(ult3)) %>%
      head(2-bases) %>%
      bind_rows(
        ideal2 %>%
          filter(pos == "Aleros") %>%
          arrange(desc(ult3)) %>%
          head(4-aleros)
      ) %>%
      bind_rows(
        ideal2 %>%
          filter(pos == "Pívots") %>%
          arrange(desc(ult3)) %>%
          head(4-pivots)
      ) %>%
      bind_rows(ideal1) %>% #join with the 4 best JFL players
      select(jug, eq, broker, lic, pos, ult3) %>%
      mutate(broker = parse_number(broker)) %>%
      arrange(match(pos, c("Bases", "Aleros", "Pívots")))

        return(ideal3)

  } else {

    return(ideal)
  }
}

#ET VOILÀ !
ideal_1 <- equipo_ideal(bt_df)


#write_csv(ideal_1, "ideal_1.csv")

sum(ideal_1$broker)


