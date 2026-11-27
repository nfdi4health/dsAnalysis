#'
#' @title Function to update the 01_DSLite_Setup.R file
#' @description XXX
#' @details XXX.
#' @return adjusted R Script
#' @author Florian Schwarz for the German Institute of Human Nutrition
#' @param folder_name the folder where the mock data is stored
#' @param table_names optional - given new table names
#' @import dplyr
#' @import tidyr
#' @import tibble
#' @import here
#' @import stringr
#' @export
#'

update_MockData <- function(folder_name = NULL, table_names = NULL){

  #### column names used by dplyr/tidyr below (no global variables)
  Code <- Code_1 <- Code_5 <- server <- NULL



  if(is.null(folder_name)){
    stop("You need to name a folder that contains the mock data.", call.=FALSE)
  }


  mockData_files <- list.files(path = here::here(paste0("utils/mock_data/", folder_name)))
  mockData_files_name <- stringr::str_remove(string = mockData_files,
                                             pattern = ".rda")

  if(!is.null(table_names)){
    if(!(length(table_names) == length(mockData_files))){

      stop("The length of the provided table names and mock data files are not the same.", call.=FALSE)

    } else {

      information <- data.frame(server = mockData_files_name,
                                table = table_names)

    }
  }

  if(is.null(table_names)){

    message("No table names were given. Hence, they will be retrieved from the 01_DS_Login.R file!")


    ds_login_codelines <- readLines(con = here::here("R", "01_DS_Login.R"))
    ds_login_codelines <- stringr::str_replace_all(string = ds_login_codelines, pattern = " ", replacement = "")

    builderAppend_line <- which(stringr::str_detect(ds_login_codelines, "builder[$]append"))
    logindata_line <- which(stringr::str_detect(ds_login_codelines, "logindata<-"))

    builder_df <- as.data.frame(matrix(nrow = length(builderAppend_line),ncol = 2))
    colnames(builder_df) <- c("Codeline_Start",
                              "Code")

    builder_df$Codeline_Start <- builderAppend_line

    for (p in 1:length(builderAppend_line)){

      if(p < length(builderAppend_line)){

        builder_df$Code[p] <-  tibble(ds_login_codelines[builderAppend_line[p]:(builderAppend_line[1+p]-1)])

      } else if (p == length(builderAppend_line)){

        builder_df$Code[p] <-  tibble(ds_login_codelines[builderAppend_line[p]:logindata_line])

      }

    }

    information <- builder_df |>
      unnest_wider(col = Code, names_sep = "_") |>
      select(c(Code_1, Code_5)) |>
      mutate(server = stringr::str_split_i(string = Code_1,
                                           pattern = "=",
                                           i = 2)) |>
      mutate(server = stringr::str_replace_all(string = server,
                                               pattern = "\\p{quotation mark}",
                                               replacement = "")) |>
      mutate(server = stringr::str_replace_all(string = server,
                                               pattern = ",",
                                               replacement = "")) |>
      mutate(table = stringr::str_split_i(string = Code_5,
                                          pattern = "\\.",
                                          i = 2)) |>
      mutate(table = stringr::str_replace_all(string = table,
                                              pattern = "\\p{quotation mark}",
                                              replacement = "")) |>
      mutate(table = stringr::str_replace_all(string = table,
                                              pattern = ",",
                                              replacement = "")) |>
      select(c(server, table))


  }



  dslite_setup_codelines <- readLines(con = here::here("utils/setup", "01_DSLite_Setup.R"))

  step1_text <- "#### Step 1: Loading necessary libraries"
  step2_text <- "#### Step 2: Import of mock data files"
  step3_text <- "#### Step 3: Defining the server-side data in a new dslite server"
  step4_text <- "#### Step 4: Defining the server-side settings"
  step5_text <- "#### Step 5: Building the logindata object"
  step6_text <- "#### Step 6: Login to the different DSLite Servers"
  step7_text <- "#### Step 7: Cleaning the environment"

  step1_line <- which(dslite_setup_codelines == step1_text)
  step2_line <- which(dslite_setup_codelines == step2_text)
  step3_line <- which(dslite_setup_codelines == step3_text)
  step4_line <- which(dslite_setup_codelines == step4_text)
  step5_line <- which(dslite_setup_codelines == step5_text)
  step6_line <- which(dslite_setup_codelines == step6_text)
  step7_line <- which(dslite_setup_codelines == step7_text)

  block0 <- dslite_setup_codelines[1:(step1_line-1)]
  block1 <- dslite_setup_codelines[step1_line:(step2_line-1)]
  block2 <- dslite_setup_codelines[step2_line:(step3_line-1)]
  block3 <- dslite_setup_codelines[step3_line:(step4_line-1)]
  block4 <- dslite_setup_codelines[step4_line:(step5_line-1)]
  block5 <- dslite_setup_codelines[step5_line:(step6_line-1)]
  block6 <- dslite_setup_codelines[step6_line:(step7_line-1)]
  block7 <- dslite_setup_codelines[step7_line:length(dslite_setup_codelines)]


  #### block2

  #### number of elements is 1+1+number of studies
  number_elements <- 2L + length(mockData_files)
  block2_new <- c()
  block2_new[1] <- step2_text
  block2_new[number_elements] <- ""

  for (k in 1:length(mockData_files)){

    block2_new[k+1] <- paste0("load(file = here::here(\"utils/mock_data/", folder_name ,"\", \"", mockData_files[k], "\"))")

  }

  #### block3
  block3_new <- c()
  block3_new[1] <- step3_text
  block3_new[number_elements] <- ""

  for (k in 1:length(mockData_files)){

    if(k == 1){

      block3_new[k+1] <- paste0("dslite.server <<- DSLite::newDSLiteServer(tables=list(",information$table[k], " = ", information$server[k], ",")

    } else if(k < length(mockData_files)){

      block3_new[k+1] <- paste0("                                                      ", information$table[k]," = ", information$server[k],",")

    } else if(k == length(mockData_files)){

      block3_new[k+1] <- paste0("                                                      ", information$table[k]," = ", information$server[k],"))")


    }

  }


  #### block5
  block5_new <- c()
  block5_new[1] <- step5_text

  for (k in 1:length(mockData_files)){

    if(k == 1){

      block5_new[k+1] <- paste0("logindata.dslite.data <- data.frame(server = c(\"",information$server[k],"\",")

    } else if(k < length(mockData_files)){

      block5_new[k+1] <- paste0("                                               \"",information$server[k],"\",")

    } else if(k == length(mockData_files)){

      block5_new[k+1] <- paste0("                                               \"",information$server[k],"\"),")


    }

  }

  block5_new[2+length(mockData_files)] <- paste0("                                    url = \"dslite.server\",")
  block5_new[3+length(mockData_files)] <- paste0("                                    user = \"\",")
  block5_new[4+length(mockData_files)] <- paste0("                                    password = \"\",")

  for (k in 1:length(mockData_files)){

    if(k == 1){

      block5_new[k+4+length(mockData_files)] <- paste0("                                    table = c(\"",information$table[k],"\",")

    } else if(k < length(mockData_files)){

      block5_new[k+4+length(mockData_files)] <- paste0("                                              \"",information$table[k],"\",")

    } else if(k == length(mockData_files)){

      block5_new[k+4+length(mockData_files)] <- paste0("                                              \"",information$table[k],"\"),")


    }

  }

  block5_new[5+2*length(mockData_files)] <- paste0("                                    options = list(datashield.privacyControlLevel = \"permissive\"),")
  block5_new[6+2*length(mockData_files)] <- paste0("                                    driver = \"DSLiteDriver\")")
  block5_new[7+2*length(mockData_files)] <- paste0("")


  dslite_setup_codelines_new <- c(block0,
                                  block1,
                                  block2_new,
                                  block3_new,
                                  block4,
                                  block5_new,
                                  block6,
                                  block7)


  writeLines(text = dslite_setup_codelines_new,con = here::here("utils/setup", "01_DSLite_Setup.R"))


}

