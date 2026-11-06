#### this file contains the functions to connect to Opal Servers of the participating studies

library(DSI)
library(DSOpal)
library(dsBaseClient)

#### credentials come from .Renviron: never write them into this file (it is committed)
#### to log in with a DataSHIELD profile other than the server's default (it decides which
#### server packages are available), add profile = "<name>" to each server's append() call below

builder <- DSI::newDSLoginBuilder(.silent = FALSE)
builder$append(server = "DEMO_OBIBA_1",
               url = Sys.getenv("OBIBA1_URL"),
               user = Sys.getenv("OBIBA1_USER"),
               password = Sys.getenv("OBIBA1_PWD"),
               table = "CNSIM.CNSIM1",
               driver = "OpalDriver")
builder$append(server = "DEMO_OBIBA_2",
               url = Sys.getenv("OBIBA2_URL"),
               user = Sys.getenv("OBIBA2_USER"),
               password = Sys.getenv("OBIBA2_PWD"),
               table = "CNSIM.CNSIM2",
               driver = "OpalDriver")
builder$append(server = "DEMO_OBIBA_3",
               url = Sys.getenv("OBIBA3_URL"),
               user = Sys.getenv("OBIBA3_USER"),
               password = Sys.getenv("OBIBA3_PWD"),
               table = "CNSIM.CNSIM3",
               driver = "OpalDriver")


logindata <- builder$build()
conns <- DSI::datashield.login(logins = logindata,
                               assign = TRUE,
                               symbol = "D")
