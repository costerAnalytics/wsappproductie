library(readxl)

data <- read_xlsx("productiedata.xlsx") ## misschien heir nog even helemaal zeker zijn dat het klopt met datum
write.table(data,'productiedata.dat',sep = ",")

