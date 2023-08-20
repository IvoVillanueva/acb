# get_nfl_teams <- function() {
#   message("Getting NFL teams!")
#
#   team_url <- "https://site.api.espn.com/apis/site/v2/sports/football/nfl/teams"
#
#   raw_teams <- httr::GET(team_url, query = list(limit = "50")) %>%
#     httr::content()
#
#   purrr::pluck(raw_teams, "sports", 1, "leagues", 1, "teams") %>%
#     dplyr::tibble(value = .) %>%
#     tidyr::unnest_wider(value) %>%
#     tidyr::unnest_wider(team) %>%
#     tidyr::hoist(
#       logos,
#       logo = list(1, "href")
#     ) %>%
#     dplyr::select(id, name, nickname, abbreviation, displayName, color, alternateColor, logo) %>%
#     purrr::set_names(
#       nm = c(
#         "team_id", "team_name", "team_nickname", "team_abb", "team_full_name", "team_color",
#         "team_alt_color", "logo"
#       )
#     ) %>%
#     dplyr::mutate(
#       team_color = paste0("#", team_color),
#       team_alt_color = paste0("#", team_alt_color)
#     )
# }
# Librerias --------------------------------------------------------------


library(tidyverse) # la madre del corder0
library(rvest) # para extraer datos de la web
library(janitor) # limpiar encabezados
library(cropcircles) # fotos en circulo
library(gt) # tabla
library(gtExtras)



# Enlaces -----------------------------------------------------------------


cabecera <- "https://raw.githubusercontent.com/IvoVillanueva/arbitros/main/Fondo%20de%20%E2%80%9C9983566e-c80e-445f-81b7-c5c52f5ecf5b_alta-libre-aspect-ratio_default_0%E2%80%9D%20eliminado.png"
nba1 <- "https://www.basketball-reference.com/players/p/parkeja01.html#per_game"
nba2 <- "https://www.basketball-reference.com/players/p/parkeja01.html#per_poss"
nba3 <- "https://www.basketball-reference.com/players/p/parkeja01.html#advanced"
acc1 <- "https://www.sports-reference.com/cbb/players/jabari-parker-1.html#players_per_game"
acc2 <- "https://www.sports-reference.com/cbb/players/jabari-parker-1.html#players_per_poss"
acc3 <- "https://www.sports-reference.com/cbb/players/jabari-parker-1.html#players_advanced"


# Tabla para fotos, colores y logos de fondo -------------------------------------

# como las tablas gt no admiten poner color en el fondo, las he convertido con la librerias cropcircles
# y luego desde el disco duro subirlas a gitub porque gt solo lee imagenes con enlace web
# he aquí un ejemplo de la foto circular de washinton:
# circle_crop(
#   "https://cdn.statmuse.com/img/nba/players/washington-wizards-jabari-parker-min--0vxs4xoe.png",
#   to  = "was-colour-bg.png",
#   border_size = 4,
#   border_colour = "#002b5c"
# )

fotis <- tribble(
  ~tm, ~headshots, ~logo, ~color,
  "Duke", "https://raw.githubusercontent.com/IvoVillanueva/nbapng/main/duke-colour-bg.png", "https://a.espncdn.com/combiner/i?img=/i/teamlogos/ncaa/500/150.png&h=200&w=200", "#012169",
  "SAC", "https://raw.githubusercontent.com/IvoVillanueva/nbapng/main/sac-colour-bg.png", "https://cdn.nba.com/teams/uploads/sites/1610612758/2022/04/Kings-Primary.svg", "#5a2d81",
  "ATL", "https://raw.githubusercontent.com/IvoVillanueva/nbapng/main/atl-colour-bg.png", "https://cdn.nba.com/teams/uploads/sites/1610612737/2023/01/atl_hawks_primary_icon.svg", "#c8102e",
  "WAS", "https://raw.githubusercontent.com/IvoVillanueva/nbapng/main/was-colour-bg.png", "https://cdn.nba.com/teams/uploads/sites/1610612764/2022/06/wiz-primary.svg", "#002b5c",
  "MIL", "https://raw.githubusercontent.com/IvoVillanueva/nbapng/main/mil-colour-bg.png", "https://cdn.nba.com/teams/uploads/sites/1610612749/2021/09/logo.svg", "#00471c",
  "CHI", "https://raw.githubusercontent.com/IvoVillanueva/nbapng/main/chi-colour-bg.png", "https://cdn.nba.com/teams/uploads/sites/1610612741/2021/10/bulls-svg.svg", "#c8102e",
  "BOS", "https://raw.githubusercontent.com/IvoVillanueva/nbapng/main/bos1-colour-bg.png", "https://cdn.nba.com/teams/uploads/sites/1610612738/2022/05/celtics_secondary.svg", "#008348"
)

# Tabla liga logos --------------------------------------------------------

tm_table <- tribble(
  ~lg, ~log,
  "NBA", "https://upload.wikimedia.org/wikipedia/fr/thumb/8/87/NBA_Logo.svg/langfr-270px-NBA_Logo.svg.png",
  "ACC", "https://www.ncaa.com/sites/default/files/images/logos/conferences/acc.40.png"
)


# Data scrap y Data Wrangling--------------------------------------------------------------

# saco los datos de 6 tablas diferentes y selecciono las columnas que me interesan de cada una
# para no complicar el left join (union de ellas) hago que sean iguales en el select
# uso html node o element según me dá es igual

# Por partido NBA
jbporgame <- nba1 %>%
  read_html() %>%
  html_element("#per_game") %>%
  html_table() %>%
  clean_names() %>%
  slice(1:14) %>%
  select(season, age, tm, lg, g, mp, pts, trb, ast)

# Por 100 posesiones NBA (algunas tablas en esta página estan subidas como comentarios, por eso la forma es diferente)
jbporposs <- nba2 %>%
  read_html() %>%
  html_nodes(xpath = "//comment()") %>%
  html_text() %>%
  paste(collapse = "") %>%
  read_html() %>%
  html_node("#per_poss") %>%
  html_table() %>%
  clean_names() %>%
  slice(1:14) %>%
  select(season, age, tm, lg, g, mp1 = mp, pts1 = pts, trb1 = trb, ast1 = ast, o_rtg, d_rtg)

# Advanced NBA
jbporadvanced <- nba3 %>%
  read_html() %>%
  html_element("#advanced") %>%
  html_table() %>%
  clean_names() %>%
  slice(1:14) %>%
  select(season, age, tm, lg, g, mp1 = mp, per)

# Unión tablas NBA

jbnba <- jbporgame %>%
  left_join(jbporposs) %>%
  left_join(jbporadvanced)


# Por partido NCAA
jbporgame_acc <- acc1 %>%
  read_html() %>%
  html_element("#players_per_game") %>%
  html_table() %>%
  clean_names() %>%
  slice(1) %>%
  select(season, age = class, tm = school, lg = conf, g, mp, pts, trb, ast)

# Por 100 posesiones NCAA
jbporposs_acc <- acc2 %>%
  read_html() %>%
  html_node("#players_per_poss") %>%
  html_table() %>%
  clean_names() %>%
  slice(1) %>%
  select(season, age = class, tm = school, lg = conf, g, mp1 = mp, pts1 = pts, trb1 = trb, ast1 = ast, o_rtg, d_rtg)

# Advanced NCAA
jbporadvanced_acc <- acc3 %>%
  read_html() %>%
  html_element("#players_advanced") %>%
  html_table() %>%
  clean_names() %>%
  slice(1) %>%
  select(season, age = class, tm = school, lg = conf, g, mp1 = mp, per)

# Unión tablas ncaa

jbacc <- jbporgame_acc %>%
  left_join(jbporposs_acc) %>%
  left_join(jbporadvanced_acc)

# Unión tablas NBA y NCAA

jbtotal <- jbacc %>%
  rbind(jbnba) %>%
  filter(tm != "TOT") %>%
  left_join(fotis) %>%
  left_join(tm_table)


# GT table ----------------------------------------------------------------


jbtotal %>%
  select(img = headshots, logo, log, season, g:per) %>%
  gt() %>%
  gt_img_rows(img) %>%
  gt_img_rows(logo) %>%
  gt_img_rows(log) |>
  cols_label(
    img = "",
    logo = "",
    log = "",
    season = gt::html("<span style='font-weight:bold;font-dystopian:small-caps;font-size:16px'>año</span>"),
    g = gt::html("<span style='font-weight:bold;font-dystopian:small-caps;font-size:16px'>Gms</span>"),
    mp = gt::html("<span style='font-weight:bold;font-dystopian:small-caps;font-size:16px'>mp</span>"),
    pts = gt::html("<span style='font-weight:bold;font-dystopian:small-caps;font-size:16px'>pts</span>"),
    trb = gt::html("<span style='font-weight:bold;font-dystopian:small-caps;font-size:16px'>trb</span>"),
    ast = gt::html("<span style='font-weight:bold;font-dystopian:small-caps;font-size:16px'>ast</span>"),
    mp1 = gt::html("<span style='font-weight:bold;font-dystopian:small-caps;font-size:16px'>mp</span>"),
    pts1 = gt::html("<span style='font-weight:bold;font-dystopian:small-caps;font-size:16px'>pts</span>"),
    trb1 = gt::html("<span style='font-weight:bold;font-dystopian:small-caps;font-size:16px'>trb</span>"),
    ast1 = gt::html("<span style='font-weight:bold;font-dystopian:small-caps;font-size:16px'>ast</span>"),
    o_rtg = gt::html("<span style='font-weight:bold;font-dystopian:small-caps;font-size:16px'>ortg</span>"),
    d_rtg = gt::html("<span style='font-weight:bold;font-dystopian:small-caps;font-size:16px'>drtg</span>"),
    per = gt::html("<span style='font-weight:bold;font-dystopian:small-caps;font-size:16px'>per</span>")
  ) %>%
  tab_spanner(gt::html("<span style='font-weight:bold;font-dystopian:small-caps;font-size:16px'>por 100 posesiones</span>"),
    columns = c(mp1:d_rtg)
  ) %>%
  cols_align(
    align = "center",
    columns = c(everything())
  ) |>
  data_color(
    columns = c(per), #summary(jbtotal$per) para saber el mínimo y el máximo
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "nbapalettes::celtics",
        direction = -1
      ) %>% as.character(),
      domain = c(5, 29),
      na.color = "#005C55FF"
    )
  ) |>
  espnscrapeR::gt_theme_espn() |> #Me encanta gt por como te deja meter un montón de html y desde aquí hay un montón
  opt_row_striping(row_striping = TRUE) %>%
  tab_options(
    heading.align = "center",
    table.font.names = "Roboto Condensed",
    table.background.color = "white",
    table.font.size = 14,
    data_row.padding = px(1),
    source_notes.font.size = 10,
    table.additional_css = ".gt_table {
                margin-bottom: 40px;
              }"
  ) |>
  tab_header(
    title = md("<img src='https://raw.githubusercontent.com/IvoVillanueva/arbitros/main/Fondo%20de%20%E2%80%9C9983566e-c80e-445f-81b7-c5c52f5ecf5b_alta-libre-aspect-ratio_default_0%E2%80%9D%20eliminado.png'
               style='height:120px;'><br><span style='font-weight:bold;font-dystopian:small-caps;font-size:40px'>JABARI PARKER</span>"),
    subtitle = md("<span style='font-weight:400;color:#8C8C8C;font-size:20px'>Sus numeros desde el 2013 al 2022<br><br></span>")
  ) %>%
  tab_source_note(
    source_note = md(" <span style='font-weight:bold;font-dystopian:small-caps;font-size:14px'>**Datos**: </span> <span style='font-dystopian:small-caps;font-size:14px'>*@collegebb_ref*</span>
    &nbsp;&nbsp; <img src='https://cdn.ssref.net/req/202307191/logos/cbb-logo.svg'
                     style='height:12px;'> & <span style='font-dystopian:small-caps;font-size:14px'>*@bball_ref*</span>&nbsp;&nbsp;<img src='https://cdn.ssref.net/req/202308191/logos/bbr-logo.svg'
                     style='height:12px;'><br><span style='font-weight:bold;font-dystopian:small-caps;font-size:14px'>**Gráfico**: </span>
                      <span style='font-dystopian:small-caps;font-size:14px'>*Ivo Villanueva*</span> &bull;
                     <span style='color:#000000;font-family: \"Font Awesome 6 Brands\"'>&#xE61B;</span> <span style='font-weight:bold;font-dystopian:small-caps;font-size:14px'>*@elcheff*</span>")
  ) %>%
  gtsave("jbParker.png", expand = 50)
