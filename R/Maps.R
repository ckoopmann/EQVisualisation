#' Earthquake Map
#'
#' Create interactive html-Earthquake map based on leaflet package
#'
#' @param df Raw data frame with columns LONGITUDE, LATITUDE, EQ_PRIMARY
#' @param annot_col Column name to use as text for annotations
#' 
#' @details  This function creas an interactive html map with Earthquake Locations marked with Circles whose
#' radius is proportional to the Earthquake magnitude (EQ_PRIMARY). The text supplied in the annotation column will be displayed upon clicking
#' on a specific Earthquake marker
#'
#' @return Interactive map object as defined by the leaflet package
#' 
#' @examples 
#' readr::read_delim("results.tsv", delim = "\t") %>% eq_clean_data() %>% 
#' dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>% eq_map(annot_col = "DATE")
#' 
#' @importFrom leaflet leaflet
#' @importFrom leaflet addTiles
#' @importFrom leaflet addCircleMarkers
#' 
#' @export
eq_map <- function(df,annot_col = "DATE"){
      map = leaflet::leaflet() %>% leaflet::addTiles() %>% leaflet::addCircleMarkers(data = df, lng = ~ LONGITUDE,
                                                          lat = ~ LATITUDE,
                                                          radius = ~ EQ_PRIMARY,
                                                          popup = ~ eval(parse(text = annot_col)))
      return(map)
}
#' Map Annotations
#'
#' Create Annotation Column for use in eq_map
#'
#' @param data dataframe containing the columns LOCATION_NAME, EQ_PRIMARY and DEATHS
#' 
#' @details  This function creates a text annotation column in html 
#' with the format VariableName: Value for Location, Magnitude and Total Deaths of each earthquake in seperate rows.
#' Where the data is missing the corresponding row will be omitted. Can be used for example with the dplyr::mutate function (see examples).
#'
#' @return Label Character vector in html format
#' 
#' @examples 
#' readr::read_delim("results.tsv", delim = "\t") %>% eq_clean_data() %>% dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>% 
#' dplyr::mutate(popup_text = eq_create_label(.)) %>% eq_map(annot_col = "popup_text")
#' 
#' @export
eq_create_label <- function(data){
      data = eq_location_clean(data)
      label = rep("", times = nrow(data))
      label[!is.na(data$LOCATION_NAME)] = paste0(label[!is.na(data$LOCATION_NAME)], "<b>Location:</b>",data$LOCATION_NAME[!is.na(data$LOCATION_NAME)], "<br>")
      label[!is.na(data$EQ_PRIMARY)] = paste0(label[!is.na(data$EQ_PRIMARY)], "<b>Magnitude:</b>",data$EQ_PRIMARY[!is.na(data$EQ_PRIMARY)], "<br>")
      label[!is.na(data$DEATHS)] = paste0(label[!is.na(data$DEATHS)], "<b>Total Deaths:</b>",data$DEATHS[!is.na(data$DEATHS)], "<br>")
      return(label)
}
