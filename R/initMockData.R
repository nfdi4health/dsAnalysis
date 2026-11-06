#'
#' @title Function to get mock data from connected data sources
#' @description The function will create a new DataSHIELD analysis project environment
#' @details This function adaptssome things.
#' @return mock data in the utils/mock_data folder
#' @author Florian Schwarz for the German Institute of Human Nutrition
#' @param folder_name folder where the mock data shall be stored
#' @param df data.frame object on the server-side to produce mock data from
#' @param datasources a list of \code{\link[DSI]{DSConnection-class}} objects obtained after login.
#' If the \code{datasources} argument is not specified the default set of connections will be
#' used: see \code{\link[DSI]{datashield.connections_default}}.
#' @import dsSupportClient
#' @import dsBaseClient
#' @import dplyr
#' @import tidyr
#' @import tibble
#' @import here
#' @import DSOpal
#' @importFrom purrr map map2
#' @importFrom methods is
#' @importFrom DSI datashield.connections_find
#' @export
#'

initMockData <- function(folder_name = NULL, df = "D", datasources = NULL){

  if(is.null(folder_name)){
    folder_name <- "MockData_New"
  }

  new_mockdata_path <- here::here("utils/mock_data", folder_name)

  if (fs::dir_exists(new_mockdata_path)) {
    stop(paste0("The folder name you have provided would overwrite an existing
                directory (", new_mockdata_path, "). Setup aborted."),
         call. = FALSE)
  }

  # look for DS connections
  if(is.null(datasources)){
    datasources <- DSI::datashield.connections_find()
  }

  # ensure datasources is a list of DSConnection-class
  if(!(is.list(datasources) && all(unlist(lapply(datasources, function(d) {methods::is(d,"DSConnection")}))))){
    stop("The 'datasources' were expected to be a list of DSConnection-class objects", call.=FALSE)
  }

  ds_servers <- names(datasources)

  vars_missing <- dsSupportClient::ds.wrapper(df, ds_function = ds.numNA, datasources = datasources)
  vars_class <- dsSupportClient::ds.wrapper(df, ds_function = ds.class, datasources = datasources)

  vars_columns <- vars_class |>
    tibble::rownames_to_column() |>
    select(1) |>
    pull()

  vars_cont <- vars_class |>
    dplyr::filter(if_all(everything(), ~ .x == "numeric")) |>
    tibble::rownames_to_column() |>
    select(1) |>
    pull()

  vars_cat <- vars_class |>
    dplyr::filter(if_all(everything(), ~ .x == "factor")) |>
    tibble::rownames_to_column() |>
    select(1) |>
    pull()

  #### cont vars

  vars_cont_stats <- dsSupportClient::ds.summaryVars(df, datasources = datasources)

  all_datasources_length <- c()
  mock_data_cont <- list()

  for (i in seq_along(vars_cont_stats)){

    datasource_length <- vars_cont_stats[[i]][2] |>
      pull()
    datasource_length <- as.numeric(datasource_length)[1]

    all_datasources_length[i] <- datasource_length

    vars_cont_negative <- vars_cont_stats[[i]] |>
      select(3)|>
      filter(if_any(1, ~ .x < 0)) |>
      tibble::rownames_to_column() |>
      select(1) |>
      pull()

    datasource_cont_stats <- vars_cont_stats[[i]] |>
      select(c(6,11)) |>
      mutate(across(everything(), ~ as.numeric(.))) |>
      tibble::rownames_to_column() |>
      rename(Variable = 1,
             Mean = 2,
             Std = 3) |>
      mutate(Entries = purrr::map2(Mean, Std, ~ rnorm(n = datasource_length,
                                                      mean = .x,
                                                      sd = .y))) |>
      select(Variable, Entries) |>
      pivot_wider(names_from = "Variable",
                  values_from = "Entries") |>
      unnest(cols = all_of(vars_cont))|>
      mutate(across(!all_of(vars_cont_negative), ~ ifelse(.x < 0, 0, .x)))

    mock_data_cont[[i]] <- datasource_cont_stats
    assign(x = ds_servers[i],
           value = datasource_cont_stats)

  }


  count <- 0L

  #### "D" hardcoded replaced
  for (k in seq_along(vars_cat)){

    var_cat_level <- ds.levels(paste0(df, "$",vars_cat[k]), datasources = datasources)
    names_list <- names(var_cat_level)
    list2 <- cbind(var_cat_level, names_list)

    for (j in 1:length(var_cat_level)){

      number_categories <- length(var_cat_level[[j]]$Levels)

      list3 <- as_tibble(list2) |>
        unnest_wider(col = 1) |>
        unnest_longer(col = 1) |>
        #### enough entries for the largest server; each server's are cut to its length below
        mutate(Entries = purrr::map(.x = Levels, .f = ~ rep(x = .x,
                                                            times = ceiling(max(all_datasources_length)/number_categories)))) |>
        mutate(Variable = vars_cat[k])

    }

    if(count == 0){

      var_cat_compressed <- list3

    } else {

      var_cat_compressed <- rbind(var_cat_compressed,
                                  list3)

    }

    count <- count + 1L


  }


  if(length(vars_cat) > 0){
    var_cat_long <- var_cat_compressed |>
      select(-c(1,2)) |>
      unnest_longer(col = Entries)
  }


  for (p in seq_along(ds_servers)){
    for (q in seq_along(vars_cat)){


      new_vector <- var_cat_long |>
        filter(names_list == ds_servers[p]) |>
        filter(Variable == vars_cat[q]) |>
        select(Entries) |>
        pull()

      new_vector_adjusted <- new_vector[1:all_datasources_length[p]]
      new_vector_sampled <- sample(new_vector_adjusted)


      mock_data_cont[[p]][vars_cat[q]] <- new_vector_sampled


    }

  }


  #### the folder is only created once the servers have answered
  dir.create(new_mockdata_path, recursive = TRUE)

  for (w in seq_along(mock_data_cont)){

    vars_missing_study <- vars_missing |>
      tibble::rownames_to_column() |>
      select(c(1,1+w)) |>
      filter(if_all(2, ~ . != 0)) |>
      rename(Variable = 1,
             Value = 2)

    df1 <- mock_data_cont[[w]] |>
      mutate(across(all_of(vars_cat), ~ as.factor(.)))

    if(!(dim(vars_missing_study)[1] == 0)){

      for (k in 1:dim(vars_missing_study)[1]){
        x <- df1[[vars_missing_study$Variable[k]]]
        df1[[vars_missing_study$Variable[k]]] <- replace(x, sample(length(x), vars_missing_study$Value[k]), NA)
      }

    }

    df1 <- df1 |>
      mutate(ID = row_number()) |>
      select(all_of(vars_columns)) |>
      as.data.frame()


    assign(x = ds_servers[w],
           value = df1)

    save(list = ds_servers[w],
         file = file.path(new_mockdata_path, paste0(ds_servers[w], ".rda")))

  }

  invisible(new_mockdata_path)

}





