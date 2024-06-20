library(dataMaid)
library(DataExplorer)
library(SmartEDA)

#dataMaid
makeDataReport(all_data, output="html", replace=T)

#DataExplorer-- didn't work
create_report(all_data)

#smartEDA
ExpReport(all_data,op_file='smartEDA_all_data.html')