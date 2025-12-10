library(lubridate)

temp_file <- tempfile()
temp_dir <- tempdir()
download.file("https://www.data.boem.gov/Other/Files/IncInvRawData.zip", temp_file, mode = "wb")
unzip(temp_file, exdir = temp_dir)

extracted_files <- list.files(temp_dir, full.names = TRUE)
invest <- read.csv(file.path(temp_dir,'IncInvRawData', "mv_acc_investigations.txt"))
invest$date <- mdy(invest$DATE_OCCURRED)

pollution <- invest[grep('pollution',invest$ACCIDENT_TYPE, ignore.case = T),]

barplot(table(year(pollution$date)))




temp_file <- tempfile()
temp_dir <- tempdir()
download.file("https://www.data.boem.gov/Pipeline/Files/PipeLocRawData.zip", temp_file, mode = "wb")
unzip(temp_file, exdir = temp_dir)

extracted_files <- list.files(temp_dir, full.names = TRUE)
list.files(file.path(temp_dir,'PipeLocRawData'))
pipelines <- read.csv(file.path(temp_dir,'PipeLocRawData', "mv_pipelinelocation.txt"))


temp_file <- tempfile()
temp_dir <- tempdir()
download.file("https://www.data.boem.gov/Pipeline/Files/pplmastdelimit.zip", temp_file, mode = "wb")
unzip(temp_file, exdir = temp_dir)

extracted_files <- list.files(temp_dir, full.names = TRUE)
pipelines <- read.csv(file.path(temp_dir, "pplmastdelimit.txt"),header = F)
name <- c('Segment Num',
          'Seg Length',
          'Orig Id Name',
          'Orig Ar Code',
          'Orig Blk Num',
          'Orig Lse Num',
          'Dest Id Name',
          'Dest Ar Code',
          'Dest Blk Num',
          'Dest Lse Num',
          'Aban Aprv Dt',
          'Aban Date',
          'Approved Date',
          'Auth Code',
          'Bd Ppl Sdv Fl',
          'Bur Dsgn Fl',
          'Cat Life Tm',
          'Dep Flag',
          'Ppl Const Date',
          'Lk Detec Fl',
          'Last Rev Date',
          'Init Hs Dt',
          'Fed St Lgth',
          'Status Code',
          'Ppl Size Code',
          'Row Number',
          'Recv Maop Prss',
          'Recv Seg Num',
          'Prpsl Con Dt',
          'Prod Code',
          'Sys Desig Code',
          'ROW Permittee Code',
          'Facil Operator',
          'Min Wtr Dpth',
          'Max Wtr Dpth',
          'Prot Number',
          'Maop Prss',
          'Cathodic Code',
          'Bidir Flag',
          'Bd Ppl Fsv Fl',
          'Aprv Code',
          'Aban Type'
)
pipe_name <- gsub(' ','_',name)
names(pipelines) <- pipe_name


pipelines$construction_date <- ymd(pipelines$Ppl_Const_Date)
table(year(pipelines$construction_date))

pp_lth <- aggregate(Seg_Length ~ year(construction_date), data = pipelines, sum, na.rm = T)
pp_lth$lth_mi <- pp_lth$Seg_Length/5280
cumsum(pp_lth$lth_mi) # this does not match the published numbers
# https://doi.org/10.1080/17445302.2018.1472517