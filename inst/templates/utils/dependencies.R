#### added this file for simple library call to dsBase so that the package will be tracked in renv
#### dsBase is not necessary for the realworld setting but for the testing setup using DSLite
#### however, we do not want to necessarily load the package then
#### keeping this library call here makes sure that all packages for both setups are properly installed

library(dsAnalysis)
library(dsBase)

#### DataSHIELD packages added with install_dsPackage() or add_dsPackage(): server and client per line
#### DataSHIELD packages (managed by add_dsPackage and remove_dsPackage)
#### DataSHIELD packages end

#### packages needed by the scripts of the datashield-analysis-suggest workflow
#### bot-suggest: packages (updated by datashield-analysis-suggest)
#### bot-suggest: packages end
