
library(lubridate)
library(purrr)

temp_file <- tempfile()
temp_dir <- tempdir()
download.file("https://www.data.boem.gov/Other/Files/IncInvRawData.zip", temp_file, mode = "wb")
unzip(temp_file, exdir = temp_dir)

extracted_files <- list.files(temp_dir, full.names = TRUE)
invest <- read.csv(file.path(temp_dir,'IncInvRawData', "mv_acc_investigations.txt"))
invest$date <- mdy(invest$DATE_OCCURRED)

pollution <- invest[grep('pollution',invest$ACCIDENT_TYPE, ignore.case = T),]

barplot(table(year(pollution$date)))




# temp_file <- tempfile()
# temp_dir <- tempdir()
# download.file("https://www.data.boem.gov/Pipeline/Files/PipeLocRawData.zip", temp_file, mode = "wb")
# unzip(temp_file, exdir = temp_dir)
# 
# extracted_files <- list.files(temp_dir, full.names = TRUE)
# list.files(file.path(temp_dir,'PipeLocRawData'))
# pipelines <- read.csv(file.path(temp_dir,'PipeLocRawData', "mv_pipelinelocation.txt"))


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
pipelines$abandon_date <- ymd(pipelines$Aban_Date)
table(year(pipelines$abandon_date))

pipelines$abandon_date_aprv <- ymd(pipelines$Aban_Aprv_Dt)
aban_i <- which(is.na(pipelines$abandon_date) &
                   !is.na(pipelines$abandon_date_aprv))
pipelines$abandon_date[aban_i] <- pipelines$abandon_date_aprv[aban_i]

pipeli <- subset(pipelines, Aban_Type!='REMOVED')
pp_lth <- aggregate(Seg_Length ~ year(construction_date), data = pipeli, sum, na.rm = T)
pp_lth$lth_mi <- pp_lth$Seg_Length/5280
cumsum(pp_lth$lth_mi) # this does not match the published numbers
# https://doi.org/10.1080/17445302.2018.1472517
piperm <- subset(pipelines, Aban_Type=='REMOVED')
pprm_lth <- aggregate(Seg_Length ~ year(construction_date), data = piperm, sum, na.rm = T)
pprm_lth$lth_mi <- pprm_lth$Seg_Length/5280
cumsum(pprm_lth$lth_mi) # this does not match the published numbers
# https://doi.org/10.1080/17445302.2018.1472517

unique(year(pipelines$construction_date)) |> sort()
yrs <- seq(1958 ,2025)

pipeline_yr <- list()
n <- 1
for(i in yrs){
  pipe_i <- subset(pipelines, year(construction_date)==i | 
                      year(construction_date)<i) #|>
    # subset(year(abandon_date)>i |
             # is.na(abandon_date)) |>
    # subset(Aban_Type!='REMOVED')
  length(which(pipe_i$Aban_Type=='INPLACE'))/nrow(pipe_i)
  
  rm1 <- which(pipe_i$Aban_Type=='REMOVED' & year(pipe_i$abandon_date)<=i)
  rm2 <- which(pipe_i$Aban_Type=='INPLC/REMV' & year(pipe_i$abandon_date)<=i)
  rm <- c(rm1, rm2)
  if(length(rm)>0){
    pipe_i <- pipe_i[-rm,]
  }

  # pipe_lth <- sum(pipe_i$Seg_Length, na.rm = T)/5280
  pipe_lth <- sum(pipe_i$Fed_St_Lgth, na.rm = T)/5280
  
  apip_lth <- subset(pipe_i, Aban_Type=='INPLACE', select = 'Fed_St_Lgth') |> 
    sum(na.rm = T)/5280
  
  pipeline_yr[[n]] <- data.frame(year = i, 
                              pipe_lth = pipe_lth,
                              aban_lth = apip_lth)
  n <- n + 1
}
pipeline_yr <- list_rbind(pipeline_yr)
# leases_year <- merge(full_yrs, lease_yr, by = 'year', all = T)

plot(pipeline_yr$year, pipeline_yr$pipe_lth, typ = 'l')
plot(pipeline_yr$year, pipeline_yr$aban_lth, typ = 'l')
