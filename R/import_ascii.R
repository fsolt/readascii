#' Import ASCII datasets using SPSS syntax files
#'
#' \code{import_ascii} imports ASCII data files using their associated SPSS setup files.
#'
#' @param data_file A path to an ASCII data file.
#' @param setup_file A path to the corresponding SPSS setup file.
#' @param total_cards For multicard files, the number of cards in the file.
#' @param var_names A string vector of variable names.
#' @param var_cards For multicard files, a numeric vector of the cards on which \code{var_names} are recorded.
#' @param var_positions A numeric vector of the column positions in which \code{var_names} are recorded.
#' @param var_widths A numeric vector of the widths used to record \code{var_names}.
#' @param card_pattern For use when the file does not contain a line for every card for every respondent (or contains extra lines that correspond to no respondent), a regular expression that matches the file's card identifier; e.g., if the card number is stored in the last digit of each line, "\\d$".
#' @param respondent_pattern For use when the file does not contain a line for every card for every respondent (or contains extra lines that correspond to no respondent), a regular expression that matches the file's respondent identifier; e.g., if the respondent number is stored in the first four digits of each line, preceded by a space, "(?<=^\\s)\\d{4}".
#'
#' @details Many older survey datasets are available only in ASCII format, which is notoriously difficult to work with.  The `import_ascii` function imports uses the SPSS setup file to import data stored in this archaic format.
#'
#' See \code{\link[readroper]{read_rpr}} and \code{\link[ropercenter]{read_roper}} for implementations that read single, specified variables.
#'
#' @return A data frame
#'
#' @examples
#'
#' @importFrom readr read_lines parse_guess
#' @importFrom tibble as_tibble
#' @importFrom purrr as_vector
#' @importFrom dplyr '%>%' mutate filter select group_by summarise if_else row_number first
#' @importFrom tidyr pivot_longer spread fill separate nest
#' @importFrom stringr str_extract str_replace str_replace_all str_detect str_trim str_c
#' @importFrom labelled val_labels set_variable_labels
#' @importFrom rlang is_empty
#'
#' @export
import_ascii <- function(data_file,
                         setup_file,
                         card_pattern,
                         respondent_pattern) {

  setup <- read_lines(setup_file) %>%
    as_tibble() %>%
    mutate(section = str_extract(value, "^\\s*(?<!\\/\\*)[^\\*]*((DATA LIST)|(VARIABLE LABELS)|(VALUE LABELS))")) %>%
    fill(section)

  videntical <- Vectorize(identical)

  data_list <- setup %>%
    filter(section == "DATA LIST") %>%
    mutate(value = value %>%
             str_trim() %>%
             str_replace_all("-\\s+", "-") %>%
             str_replace_all("(?<![\\w/-])(\\d+)(?![-\\d])", "\\1-\\1")) %>%
    separate(value,
             sep = "(?<=/\\d)\\s+",
             into = c("card", "value"),
             fill = "right") %>%
    mutate(value = if_else(is.na(value) & !str_detect(card, "/\\d+\\s*$"), str_replace(card, "/\\d+\\s+", ""), value),
           card = if_else(videntical(card, value), NA_character_, card) %>%
             str_replace(".*(/\\d+).*", "\\1")) %>%
    separate(value,
             sep = "(?<=/\\d\\d)\\s+",
             into = c("card1", "value"),
             fill = "right") %>%
    mutate(value = if_else(is.na(value), str_replace(card1, "/\\d+\\s+", ""), value),
           card1 = if_else(videntical(card1, value), NA_character_, card1),
           card = if_else(is.na(card), card1, card)) %>%
    fill(card) %>%
    separate(value,
             sep = "(?=<\\w+\\s+\\d+-\\d+)\\s+(?=\\w+\\s+\\d+-\\d+)",
             into = LETTERS[1:20],
             fill = "right") %>%
    mutate(A = if_else(!str_detect(A, "\\d+-\\d+") &
                         (str_detect(B, "\\d+-\\d+") & !is.na(B)), paste(A, B), A),
           B = if_else(str_detect(A, B), NA_character_, B),
           A = if_else(!str_detect(A, "\\d+-\\d+") &
                         is.na(B) &
                         str_detect(lead(A), "\\d+-\\d+"), paste(A, lead(A)), A),
           A = if_else(str_detect(lag(A), A), NA_character_, A)) %>%
    pivot_longer(LETTERS[1:20]) %>%
    filter(!is.na(value)) %>%
    mutate(card = if_else(rep(!all(is.na(card)), nrow(.)),
                          str_replace(card, "/", ""),
                          "1")) %>%
    fill(card) %>%
    select(-card1, -name)

  data_list1 <- data_list %>%
    filter(str_detect(value, "^(\\w+\\s+\\d+-\\d+)+$")) %>%
    separate(value,
             sep = "[\\s-]+",
             into = c("variable", "first_col", "last_col", "notes"),
             fill = "right") %>%
    mutate(first_col = as.numeric(first_col),
           last_col = as.numeric(last_col),
           width = last_col - first_col + 1,
           card = as.numeric(card)) %>%
    select(variable, first_col, last_col, width, card)

  variable_labels <- setup %>%
    filter(str_detect(section, "LABELS")) %>%
    mutate(section = str_extract(value, ".*LABELS.*")) %>%
    fill(section) %>%
    filter(str_detect(section, "VARIABLE")) %>%
    mutate(var = str_extract(value, "^[^'\"]*") %>%
             str_trim(),
           label = str_extract(value, "\".*\""),
           label2 = if_else(is.na(label), str_extract(value, "'.*'"), label) %>%
             str_replace_all("^[\"']|[\"']$", "") %>%
             str_trim(),
           id = if_else(var != "", row_number(), NA_integer_)) %>%
    fill(id) %>%
    group_by(id) %>%
    summarise(variable = first(var),
              label = str_c(label2, collapse = " ")) %>%
    filter(!is.na(label)) %>%
    select(-id) %>%
    filter(variable %in% data_list1$variable)

  var_labels <- split(variable_labels$label, variable_labels$variable)

  value_labels <- setup %>%
    filter(str_detect(section, "LABELS")) %>%
    mutate(section = str_extract(value, ".*LABELS.*")) %>%
    fill(section) %>%
    filter(str_detect(section, "VALUE")) %>%
    mutate(variable = if_else(str_detect(value, "^\\s*[\\d\"\\']"), NA_character_, str_trim(str_extract(value, "(^\\s*[\\d\"\\'].*\\s+)\\w+"))),
           leftover = if_else(str_detect(value, "^\\s*[\\d\"\\']"), str_trim(str_replace(value, "\\w+(.*)", "\\1")), NA_character_),
           value2 = if_else(is.na(variable),
                            str_replace(value, "((\\d+)|(\".\")|(\\'.\\'))\\s.*", "\\1") %>%
                              str_trim() %>%
                              str_replace("/", ""),
                            str_replace(leftover, "((\\d+)|(\".\")|(\\'.\\'))\\s.*", "\\1") %>%
                              str_trim() %>%
                              str_replace("/", "")),
           label2 = if_else(is.na(variable),
                            str_replace(value, paste0("\\s*", value2, "\\s.*(.*)"), "\\1") %>%
                              str_replace("/", "") %>%
                              str_trim(),
                            NA_character_)) %>%
    fill(variable) %>%
    filter(!is.na(value2)) %>%
    select(variable, value2, label2) %>%
    nest(labels = c(value2, label2))

  total_cards0 <- setup %>%
    pull(value) %>%
    str_subset("RECORDS=\\d+") %>%
    str_replace(".*\\bRECORDS=(\\d+)\\b.*", "\\1")

  total_cards <- ifelse(rlang::is_empty(total_cards0),
            1,
            as.numeric(total_cards0))

  ascii_dataset <- read_ascii(data_file,
                              var_names = data_list1$variable,
                              var_positions = data_list1$first_col,
                              var_widths = data_list1$width,
                              total_cards = total_cards,
                              var_cards = data_list1$card)

  ascii_dataset2 <- ascii_dataset %>%
    labelled::set_variable_labels(.labels = var_labels) %>%
    select(-respondent, -matches("card\\d+"))

  try(
    for(i in 1:nrow(value_labels)) {
    x_labs <- value_labels[i, "labels"][[1]][[1]]
    labelled::val_labels(ascii_dataset2[, value_labels$variable[i]]) <- split(as.numeric(x_labs$value2), x_labs$label2) %>%
      as_vector()
    },
  silent = TRUE)

  return(ascii_dataset2)
}


read_ascii <- function(file,
                       total_cards = 1,
                       var_names,
                       var_cards = 1,
                       var_positions,
                       var_widths) {

  . <- value <- NULL   # satisfy R CMD check

  if ((length(read_lines(file)) %% total_cards) != 0 & (missing(card_pattern) | missing(respondent_pattern))) {
    stop("The number of lines in the file is not a multiple of the number of cards in the file.  Please specify card_pattern and respondent_pattern", call. = FALSE)
  }

  if (length(var_cards) == 1 & !missing(var_names)) {
    var_cards = rep(var_cards, length(var_names))
  }

  if (missing(card_pattern)) {
    df <- read_lines(file) %>%
      as_tibble() %>%
      mutate(card = paste0("card", rep_len(seq_len(total_cards), nrow(.))),
             respondent = rep(seq(to = nrow(.)/total_cards), each = total_cards)) %>%
      spread(key = "card", value = "value")
  } else {
    df <- read_lines(file) %>%
      as_tibble() %>%
      mutate(card = paste0("card", str_extract(value, card_pattern))) %>%
      filter(!card == "cardNA") %>%
      mutate(respondent = str_extract(value, respondent_pattern)) %>%
      spread(key = "card", value = "value")
  }

  if (!missing(var_names)) {
    if (missing(var_positions) | missing(var_widths)) {
      stop("Variable positions and widths should also be given when variable names are specified", call. = FALSE)
    } else if (length(unique(sapply(list(var_names, var_positions, var_widths), length))) > 1) {
      stop("The lengths of the vectors of variable names, positions, and widths must be the same", call. = FALSE)
    } else if ((max(var_cards) > max(total_cards))) {
      stop("When reading a multi-card dataset, the numbers of the cards with variables to be read must not be greater than the total number of cards",
           call. = FALSE)
    }

    for (i in seq_along(var_names)) {
      card <- paste0("card", var_cards[i])
      df[[var_names[i]]] <- parse_guess(str_replace(df[[card]],
                                                    paste0("^.{", var_positions[i] - 1, "}(.{", var_widths[i], "}).*"),
                                                    "\\1") %>%
                                          str_replace("^\\s+$", ""))
      if (var_widths[i] < 4) {
        is.na(df[[var_names[i]]]) <- which(!nchar(df[[var_names[i]]]) == var_widths[i])
      }
    }
  }

  return(df)
}

