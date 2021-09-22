# path to the files
pathbegin <- "fsmtesting/gpu-fsm-validation-artifacts/data/"
pathend <- "input-execution"
path <- paste(pathbegin, pathend, collapse="", sep="")
pattern="-[0-9]+-gpu"
plotfolder <- "fsmtesting/gpu-fsm-validation-artifacts/data/generated-plots/"

library(ggplot2)
library(scales)
library(RColorBrewer)
library(reshape2)
library(dplyr)
library(tidyr)
library(tidyverse)
library(zoo)
library(viridis)

theme_set(theme_minimal(base_size=22))
cbPalette <- c("#004949", "#ff6db6", "#009292", "#ffb677")
gray1Palette <- gray.colors(1, 0.4, 0.4)
gray2Palette <- gray.colors(2, 0.3, 0.7)
gray3Palette <- gray.colors(3, 0.3, 0.8)

# mappings for better readability
config_m <- c()
config_m["p"] <- "padded"
config_m["pt"] <- "padded-transposed"
config_m["wo"] <- "with-offsets"

su_m <- c()
su_m["s"] <- "sorted"
su_m["us"] <- "unsorted"

# read file line by line to determine how many lines to skip
how_many_to_skip <- function(filename, isgpu) {
  con <- file(filename, "r")
  linestoskip <- 0
  #print(filename)
  while (TRUE) {
    line <- readLines(con, n=1)
    line
    if ((isgpu & startsWith(line, "trans-inputs")) |
        (!isgpu & startsWith(line, "Time"))) {
      break;
    }
    
    linestoskip <- linestoskip+1;
  }

  close(con)
  return(linestoskip)
}

get_is_gpu <- function(filename) {
  res <- basename(filename)
  res <- strsplit(res, "\\.")[[1]][1]
  res <- strsplit(res, "-")[[1]][3]
  return(res == "gpu")
}

# function to extract number of tests from filename
get_number_of_tests <- function(filename) {
  res <- basename(filename)
  res <- strsplit(res, "\\.")[[1]][1]
  res <- strsplit(filename, "-")[[1]][2]
  return(res)
}

# function to extract configuration
get_config <- function(filename) {
  res <- basename(filename)
  res <- strsplit(res, "\\.")[[1]][1]
  res <- strsplit(res, "-")[[1]][4]
  return(res)
}

# function to extract the fsm name
get_fsm <- function(filename) {
  res <- basename(filename)
  res <- strsplit(res, "\\.")[[1]][1]
  res <- strsplit(res, "-")[[1]][1]
  return(res)
}

# function to extract sorted/unsorted
get_sored_unsorted <- function(filename) {
  res <- basename(filename)
  res <- strsplit(res, "\\.")[[1]][1]
  res <- strsplit(res, "-")[[1]][5]
  return(res)
}

# function to extract the size of the chunk
get_chunksize<- function(filename) {
  res <- basename(filename)
  res <- strsplit(res, "\\.")[[1]][1]
  res <- strsplit(res, "-")[[1]][6]
  return(res)
}

# function to extract the size of the test suite in bytes from inside the file
get_testsuitesize <- function(filename) {
  con <- file(filename, "r")
  testsuitesize <- 0
  while (TRUE) {
    line <- readLines(con, n=1)
    line
    if( length(line) == 0) { #end of file
      break;
    }
    
    if (startsWith(line, "chunk:")) {
      testsuitesize <- line  
      testsuitesize <- strsplit(testsuitesize, " ")[[1]][7]
      break;
    }
  }

  close(con)
  return(testsuitesize)
}

############################################################
# Organise data
############################################################

# loop over the gpu files to extract the data
gpufiles <- list.files(path=path, pattern=pattern, recursive=TRUE)
allfiles <- list.files(path=path, recursive=TRUE)

fsm_gpu <- factor()
fsm_cpu <- factor()
num_tests_gpu <- integer()
num_tests_cpu <- integer()
config_gpu <- factor()
config_cpu <- factor()
sorted_unsorted_gpu <- factor()
sorted_unsorted_cpu <- factor()

cpu_time_median <- double()
cpu_time_sd <- double()
cpu_tests_per_second <- double()

chunksize_KB <- integer()
testsuitesize_B <- integer()
gpu_time_kernel_median <- double()
gpu_time_kernel_sd <- double()
gpu_time_total_median <- double()
gpu_time_total_sd <- double()
gpu_time_inputs_median <- double()
gpu_time_results_median <- double()
gpu_tests_per_second_total <- double()
transfer_rate_GB_s <- double()

for (f in allfiles) {
  
  #print(f)
  
  cfsm <- get_fsm(f)
  numtests <- as.numeric(get_number_of_tests(f))
  cconfig <- get_config(f)
  cconfig <- config_m[cconfig]
  csorted_unsorted <- get_sored_unsorted(f)
  csorted_unsorted <- su_m[csorted_unsorted]
  
  is_gpu <- get_is_gpu(f)
  f <- file.path(path, f) # append the full path
  
  if(is_gpu) { # gpu file
    
    fsm_gpu <- append(fsm_gpu, cfsm)
    num_tests_gpu <- append(num_tests_gpu, numtests)
    config_gpu <- append(config_gpu, cconfig)
    sorted_unsorted_gpu <- append(sorted_unsorted_gpu, csorted_unsorted)
    
    cchunksize <- get_chunksize(f)
    cchunksize <- as.numeric(cchunksize)
    chunksize_KB <- append(chunksize_KB, cchunksize)
    
    ctestsuitesize <- as.numeric("NA")
    if(is.na(cchunksize)) {
      ctestsuitesize <- get_testsuitesize(f)
    }
    ctestsuitesize <- as.numeric(ctestsuitesize)
    testsuitesize_B <- append(testsuitesize_B, ctestsuitesize)
   
    # read gpu file
    linestoskip = how_many_to_skip(f, TRUE)
    cgpudata <- read.delim(f, skip=linestoskip)
    # remove lines in results that start with "totals: "... (i.e. are not numeric)
    cgpudata <- cgpudata[!is.na(as.numeric(as.character(cgpudata$trans.inputs))),]
    cgpudata$trans.inputs <- as.numeric(as.vector(cgpudata$trans.inputs))
    
    cgpu_time_kernel_median <- median(cgpudata$`exec.kernel`, na.rm=TRUE)
    gpu_time_kernel_median <- append(gpu_time_kernel_median, cgpu_time_kernel_median)
  
    cgpu_time_kernel_sd <- sd(cgpudata$`exec.kernel`, na.rm=TRUE)
    gpu_time_kernel_sd <- append(gpu_time_kernel_sd, cgpu_time_kernel_sd)
  
    cgpu_time_total_median <- median(cgpudata$`time.total`, na.rm=TRUE)
    gpu_time_total_median <- append(gpu_time_total_median, cgpu_time_total_median)
  
    cgpu_time_total_sd <- sd(cgpudata$`time.total`, na.rm=TRUE)
    gpu_time_total_sd <- append(gpu_time_total_sd, cgpu_time_total_sd)
  
    cgpu_time_inputs_median <- median(cgpudata$`trans.inputs`, na.rm=TRUE)
    gpu_time_inputs_median <- append(gpu_time_inputs_median, cgpu_time_inputs_median)
  
    cgpu_time_results_median<- median(cgpudata$`trans.results`, na.rm=TRUE)
    gpu_time_results_median <- append(gpu_time_results_median, cgpu_time_results_median)
    
    cgpu_tests_per_second_total <- numtests / cgpu_time_total_median
    gpu_tests_per_second_total <- append(gpu_tests_per_second_total, cgpu_tests_per_second_total)
  
    #ctransfer_rate <- (cchunksize * 0.001) / cgpu_time_inputs_median;
    ctransfer_rate <- median(cgpudata$`trans.rate.in`, na.rm=TRUE)
    if(is.null(ctransfer_rate)) {
      ctransfer_rate=as.numeric("NA")
    }
    transfer_rate_GB_s <- append(transfer_rate_GB_s, ctransfer_rate)
    
  } else { # cpu file
    
    fsm_cpu <- append(fsm_cpu, cfsm)
    num_tests_cpu <- append(num_tests_cpu, numtests)
    config_cpu <- append(config_cpu, cconfig)
    sorted_unsorted_cpu <- append(sorted_unsorted_cpu, csorted_unsorted)
    
    linestoskip = how_many_to_skip(f, FALSE)
    ccpudata <- read.delim(f, skip=linestoskip)
    
    ccpu_time_median <- median(ccpudata$`Time.in.ms`, na.rm=TRUE)
    ccpu_time_sd <- sd(ccpudata$`Time.in.ms`, na.rm=TRUE)
    ccpu_tests_per_second <- numtests / ccpu_time_median
    
    cpu_time_median <- append(cpu_time_median, ccpu_time_median)
    cpu_time_sd <- append(cpu_time_sd, ccpu_time_sd)
    cpu_tests_per_second <- append(cpu_tests_per_second, ccpu_tests_per_second)
  }
}

gpudata <- data.frame(fsm=fsm_gpu,
                      num_tests=num_tests_gpu,
                      config=config_gpu,
                      sorted_unsorted=sorted_unsorted_gpu,
                      chunksize_KB,
                      testsuitesize_B,
                      gpu_time_inputs_median,
                      gpu_time_results_median,
                      gpu_time_kernel_median, 
                      #gpu_time_kernel_sd, 
                      gpu_time_total_median,
                      gpu_tests_per_second_total,
                      transfer_rate_GB_s
                      #gpu_time_total_sd                     
                      )

cpudata <- data.frame(fsm=fsm_cpu, 
                      num_tests=num_tests_cpu, 
                      config=config_cpu, 
                      sorted_unsorted=sorted_unsorted_cpu, 
                      cpu_time_median, 
                      cpu_tests_per_second
                      #cpu_time_sd, 
                   )

data <- full_join(cpudata, gpudata, by=c("fsm", "num_tests", "config", "sorted_unsorted"))

# Extrapolate missing CPU & GPU times
fsmlevels <- c("fsm37","fsm770","fsm213","fsm382","fsm268")
extrapdata <- data.frame(fsm=factor(levels=fsmlevels),
                         num_tests=integer(),
                         cpu_time_median=double(),
                         cpu_tests_per_second=double(),
                         gpu_time_total_median=double(),
                         gpu_tests_per_second=double(),
                         chunksize_KB=integer())

get_start_time <- function(data, fsm_req, num_tests_req, col_name) {
  starttime <- data[data$fsm==fsm_req & data$num_tests==num_tests_req,] %>%
      select(col_name) %>% unique() 
  print(starttime)
  return(min(starttime))
}

add_fsm_rows <- function(
  extrapdata, fsm_req, starting_power, num_extra_points)
{
  cpustarttime <- get_start_time(data, fsm_req, 2^(starting_power), "cpu_time_median")
  gpustarttime <- get_start_time(data, fsm_req, 2^(starting_power), "gpu_time_total_median")
  print(gpustarttime)
  for(i in 1:num_extra_points) 
  {
    num_tests<-2^(starting_power+i)
    cpu_time_median<-cpustarttime*2^(i)
    gpu_time_total_median<-gpustarttime*2^(i)
    chunksize_KB=32768
    cpu_tests_per_second<-num_tests/cpu_time_median
    gpu_tests_per_second<-num_tests/gpu_time_total_median
    extrapdata[nrow(extrapdata)+1,]=
      list(fsm_req,num_tests,cpu_time_median,cpu_tests_per_second,gpu_time_total_median,gpu_tests_per_second,chunksize_KB)
  }
  return(extrapdata)
}

#fsm37
extrapdata<-add_fsm_rows(extrapdata,"fsm37", 23, 1)
#fsm770
extrapdata<-add_fsm_rows(extrapdata,"fsm770", 23, 3)
#fsm213
extrapdata<-add_fsm_rows(extrapdata,"fsm213", 22, 4)
#fsm382
extrapdata<-add_fsm_rows(extrapdata,"fsm382", 22, 5)
#fsm268
extrapdata<-add_fsm_rows(extrapdata,"fsm268", 22, 5)

data1<-merge(data,extrapdata,all=TRUE)
data1["config"][is.na(data1["config"])]<-"padded-transposed"
data1["sorted_unsorted"][is.na(data1["sorted_unsorted"])]<-"sorted"
# fill NA cpu times
data <- data1%>%
  group_by(fsm, num_tests) %>%
  fill(cpu_time_median, cpu_tests_per_second, .direction="down")

############################################################
# Organise datasets
############################################################
chunkdata <- data[
                  !is.na(data$chunksize_KB) &
                  data$config == "padded-transposed" &
                  data$sorted_unsorted == "sorted"
                  ,]
chunkdata$chunksize_KB <- as.factor(chunkdata$chunksize_KB)

nochunkdata <- data[
                    is.na(data$chunksize_KB) & 
                    data$config=="padded-transposed" &
                    data$sorted_unsorted=="sorted",]

# reorder new labels to arrange them based on the y values in the plot
newlevels = unique(
  nochunkdata[
    order(-nochunkdata$cpu_time_median/nochunkdata$gpu_time_total_median), # the `-` order in descending order
    "fsm"
  ]
)$fsm
nochunkdata$fsm <- factor(nochunkdata$fsm, levels=newlevels)

# find the chunksizes with minimum gpu time per fsm per test suite size
gpuchunkdata <- chunkdata[!is.na(chunkdata$gpu_time_total_median),]
chunksizes <- gpuchunkdata %>%
    group_by(fsm, num_tests) %>%                         # first group_by fsm and number of tests
    filter(gpu_time_total_median == min(gpu_time_total_median)) %>% # second filter by the minimum gpu time
    select(fsm, num_tests, chunksize_KB)      # finally select only the relevant columns
write.csv(chunksizes, "~/fsmtesting/fsmdata-2019journal/data/plots/test_group_sizes.csv", row.names=TRUE)

# filter only the relevant chunksizes
chunkdata_max <- right_join(chunkdata, chunksizes, by=c("fsm", "num_tests", "chunksize_KB"))

# count the number of chunks
numchunksdata <- data[!is.na(data$testsuitesize_B),]
numchunksdata <- numchunksdata[c("fsm", "num_tests", "testsuitesize_B")]
numchunksdata <- right_join(numchunksdata, chunksizes, by=c("fsm", "num_tests"))
numchunksdata$fsm <- factor(numchunksdata$fsm, levels=newlevels)

# plot the chunksizes
ggplot(
  aes(x=num_tests, y=chunksize_KB, color=fsm), data=numchunksdata) +
  #geom_line() +
  geom_point(size=4) +
  theme(legend.position = "none") +
  facet_wrap(~fsm, ncol=3) +
  # scales the x axis based on log2 and turns the labels in to powers of 2
  scale_x_continuous(trans=log2_trans(),
                     breaks=trans_breaks("log2", function(x) 2^x),
                     labels=trans_format("log2", math_format(2^.x))) +
  # better labels
  xlab("Number of tests") +
  ylab("Size of test groups [KB]")

cplotfile <- c(plotfolder, pathend, "-size-of-groups.png")
plotfile <- paste(cplotfile, collapse="")
ggsave(plotfile, width=13, height=12)

# plot the number of chunks 
ggplot(
  aes(x=num_tests, y=testsuitesize_B/(as.numeric(chunksize_KB)*1000), color=fsm), data=numchunksdata) +
  #geom_line() +
  geom_point(size=4) +
  theme(legend.position = "none") +
  facet_wrap(~fsm, ncol=3) +
  # scales the x axis based on log2 and turns the labels in to powers of 2
  scale_x_continuous(trans=log2_trans(),
                     breaks=trans_breaks("log2", function(x) 2^x),
                     labels=trans_format("log2", math_format(2^.x))) +
  scale_y_continuous(trans=log2_trans()) +
  # better labels
  xlab("Number of tests") +
  ylab("Number of groups")

cplotfile <- c(plotfolder, pathend, "-number-of-groups.png")
plotfile <- paste(cplotfile, collapse="")
ggsave(plotfile, width=13, height=12)

############################################################
# Plot
############################################################

speedup_m <- c()
speedup_m["speedup_exec"] <- "Speedup (kernel execution only)"
speedup_m["speedup_total"] <- "Speedup (kernel execution + data transfer)"

# Sanity checks: CPU Tests per second
cpuonlydata = data[
                  data$config=="padded-transposed" &
                  data$sorted_unsorted=="sorted" & 
                  !is.na(data$cpu_time_median), 
]
cpuonlydata$fsm <- factor(cpuonlydata$fsm, levels=newlevels)
ggplot(
  aes(x=num_tests, y=cpu_tests_per_second), data=cpuonlydata) +
  geom_line() +
  geom_point() +
  facet_wrap(~fsm, ncol=3) +
  # scales the x axis based on log2 and turns the labels in to powers of 2
  scale_x_continuous(trans=log2_trans(),
                     breaks=trans_breaks("log2", function(x) 2^x),
                     labels=trans_format("log2", math_format(2^.x))) +
  # better labels
  xlab("Number of tests") +
  ylab("Tests per milisecond")

#CPU time doubles?
# functions to generate perfect doubling line
gen_perfectline <- function(x, y, num_points) {
  val_x = c()
  val_y = c()
  for(i in 0:num_points-1) {
    val_x[i+1] = x*(2^i)
    val_y[i+1] = y*(2^i)
  }
  
  df=data.frame(num_tests=(val_x[1:num_points]), time=(val_y[1:num_points]))
  return(df)
}

gen_perfectline_per_fsm <- function(data, start_x, start_y_name, num_points) {
  df <- data.frame()
  data$fsm <- as.factor(data$fsm)
  for(i in levels(data$fsm)) {
    #print(i)
    y <- data[
              data$fsm==i & 
              data$num_tests == start_x
              ,][[start_y_name]]
    if(length(y) != 0) {
      line <- gen_perfectline(start_x, y, num_points)
      line$fsm = i
      df <- rbind(df, line)
    }
  }
  return(df)
}

perfectline_per_fsm <- gen_perfectline_per_fsm(cpuonlydata, 2^11, "cpu_time_median", 13)
ggplot(
  aes(x=num_tests, y=cpu_time_median), data=cpuonlydata) +
  geom_line() +
  geom_point() +
  geom_line(aes(x=num_tests, y=time), data=perfectline_per_fsm, color="red") +
  facet_wrap(~fsm, ncol=3) +
  # scales the x axis based on log2 and turns the labels in to powers of 2
  scale_x_continuous(trans=log2_trans(),
                     breaks=trans_breaks("log2", function(x) 2^x),
                     labels=trans_format("log2", math_format(2^.x))) +
  scale_y_continuous(trans=log2_trans()) +
  # better labels
  xlab("Number of tests") +
  ylab("Time in ms")

# Sanity checks: GPU no chunk transfer rate 
ggplot(
  aes(x=num_tests, y=(testsuitesize_B*0.001*0.001*0.001)/(gpu_time_results_median*0.001), color=fsm), data=nochunkdata) +
  geom_line() +
  geom_point() +
  facet_wrap(~fsm, ncol=3) +
  # scales the x axis based on log2 and turns the labels in to powers of 2
  scale_x_continuous(trans=log2_trans(),
                     breaks=trans_breaks("log2", function(x) 2^x),
                     labels=trans_format("log2", math_format(2^.x))) +
  #scale_y_continuous(trans=log2_trans()) +
  theme(legend.position = "none") +
  # better labels
  xlab("Number of tests") +
  ylab("Data transfer rate GB/s")

# Sanity checks: GPU chunk transfer rate 
ggplot(aes(x=num_tests, y=transfer_rate_GB_s, color=chunksize_KB), data=chunkdata) +
  #geom_line() +
  geom_point() +
  facet_wrap(~fsm, ncol=3) +
  # scales the x axis based on log2 and turns the labels in to powers of 2
  scale_x_continuous(trans=log2_trans(),
                     breaks=trans_breaks("log2", function(x) 2^x),
                     labels=trans_format("log2", math_format(2^.x))) +
  scale_y_continuous(trans=log2_trans()) +
  # better labels
  xlab("Number of tests") +
  ylab("Data transfer rate GB/s")

# Sanity checks: GPU total time doubles NO chunks?
gpuonlydata = data[
                  is.na(data$chunksize_KB) &
                  data$config=="padded-transposed" &
                  data$sorted_unsorted=="sorted", 
  ]

perfectline_per_fsm = gen_perfectline_per_fsm(gpuonlydata, 2^14, "gpu_time_total_median", 9)
ggplot(
  aes(x=num_tests, y=gpu_tests_per_second_total), data=gpuonlydata) +
  #aes(x=num_tests, y=gpu_time_total_median), data=gpuonlydata) +
  geom_point() +
  #geom_line(aes(x=num_tests, y=time), data=perfectline_per_fsm, color="red") +
  facet_wrap(~fsm, ncol=3) +
  # scales the x axis based on log2 and turns the labels in to powers of 2
  scale_x_continuous(trans=log2_trans(),
                     breaks=trans_breaks("log2", function(x) 2^x),
                     labels=trans_format("log2", math_format(2^.x))) +
  scale_y_continuous(trans=log2_trans()) +
  # better labels
  xlab("Number of tests") +
  ylab("Tests per second")

# Sanity checks: GPU kernel time doubles WITH chunks?
gpuonlydata = data[
                  !is.na(data$chunksize_KB) &
                  data$config=="padded-transposed" &
                  data$sorted_unsorted=="sorted", 
  ]
gpuonlydata$chunksize_KB = as.factor(gpuonlydata$chunksize_KB)
ggplot(aes(x=num_tests, y=gpu_time_kernel_median, color=chunksize_KB), data=gpuonlydata) +
  #geom_line() +
  geom_point() +
  facet_wrap(~fsm, ncol=3) +
  # scales the x axis based on log2 and turns the labels in to powers of 2
  scale_x_continuous(trans=log2_trans(),
                     breaks=trans_breaks("log2", function(x) 2^x),
                     labels=trans_format("log2", math_format(2^.x))) +
  scale_y_continuous(trans=log2_trans()) +
  # better labels
  xlab("Number of tests") +
  ylab("Median GPU time kernel (ms)")

# Performance: GPU total vs CPU NO chunks : Q1
#Q1: Speedup
maxspeedup <- max(nochunkdata$cpu_time_median/nochunkdata$gpu_time_total_median, na.rm = TRUE)

#plot
perfectline_cpu <- gen_perfectline_per_fsm(cpuonlydata, 2^11, "cpu_time_median", 13)
perfectlinedata <- full_join(perfectline_cpu, nochunkdata, by=c("fsm", "num_tests"))
ggplot(
       aes(x=num_tests, y=cpu_time_median/gpu_time_total_median, color=fsm), 
       data=nochunkdata
       ) +
  geom_line() +
  geom_point(size=4) +
  geom_hline(yintercept=1, linetype="dashed") +
  scale_colour_manual(values=gray.colors(15,0.4,0.4))+
  theme(legend.position = "none") +
  facet_wrap(~fsm, ncol=3) +
  # scales the x axis based on log2 and turns the labels in to powers of 2
  scale_x_continuous(trans=log2_trans(),
                     breaks=trans_breaks("log2", function(x) 2^x),
                     labels=trans_format("log2", math_format(2^.x))) +
  scale_y_continuous(trans=log2_trans()) +
  #scale_y_continuous(breaks=seq(0, maxspeedup, by=1)) +
  theme(axis.text.x=element_text(color="black")) +
  theme(axis.text.y=element_text(color="black")) +
  # better labels
  xlab("Number of inputs") +
  ylab("GPU speedup compared to a 16-core CPU")

cplotfile <- c(plotfolder, pathend, "-q1.pdf")
plotfile <- paste(cplotfile, collapse="")
ggsave(plotfile, width=13, height=12)

# Q1 analysis
max(nochunkdata$cpu_time_median/nochunkdata$gpu_time_total_median)
mean(nochunkdata$cpu_time_median/nochunkdata$gpu_time_total_median)

# cpu vs gpu - tests per ms
cpudataall <- data[
                  data$config=="padded-transposed" &
                  data$sorted_unsorted=="sorted"
              ,]
cpudataall$fsm <- factor(cpudataall$fsm, levels=newlevels)
gpudatanochunk <- data[
                      is.na(data$chunksize_KB) &
                      data$config=="padded-transposed" &
                      data$sorted_unsorted=="sorted"
                      ,]
gpudatanochunk$fsm <- factor(gpudatanochunk$fsm, levels=newlevels)
mdrj<-full_join(cpudataall[c("fsm","num_tests","cpu_tests_per_second")],
                     gpudatanochunk[c("fsm","num_tests","gpu_tests_per_second_total")],
                     by=c("fsm","num_tests"))
mdrj<-melt(mdrj,c("fsm","num_tests"))
mdrj$variable <- as.character(mdrj$variable)
mdrj$variable[mdrj$variable == "cpu_tests_per_second"] <- "CPU"
mdrj$variable[mdrj$variable == "gpu_tests_per_second_total"] <- "GPU total"
mdrj$variable <- factor(mdrj$variable, levels=c("GPU total", "CPU"))

ggplot(data=mdrj, aes(x=num_tests, y=value, color=variable)) +
  geom_line() +
  geom_point(size=4, aes(shape=variable)) +
  facet_wrap(~fsm, ncol=3) +
  #scale_colour_manual(values=cbPalette)+
  scale_colour_manual(values=gray2Palette)+
  # scales the x axis based on log2 and turns the labels in to powers of 2
  scale_x_continuous(trans=log2_trans(),
                     breaks=trans_breaks("log2", function(x) 2^x),
                     labels=trans_format("log2", math_format(2^.x))) +
  scale_y_continuous(trans=log2_trans()) +
  theme(legend.title=element_blank()) +
  theme(legend.position="bottom") +
  theme(legend.text=element_text(size=26)) +
  theme(legend.key.width=unit(3,"cm")) +
  theme(legend.spacing.x=unit(0.5,"cm")) +
  theme(axis.text.x=element_text(colour="black"))+
  theme(axis.text.y=element_text(colour="black"))+
  # better labels
  xlab("Number of inputs") +
  ylab("Inputs per ms")

cplotfile <- c(plotfolder, pathend, "-q1-cpuvsgpu.pdf")
plotfile <- paste(cplotfile, collapse="")
ggsave(plotfile, width=14, height=12)

# Speedup: Kernel only vs CPU
# reorder new labels to arrange them based on the y values in the plot
newlevels1 = unique(
  nochunkdata[
    order(-nochunkdata$cpu_time_median/nochunkdata$gpu_time_total_median), # the `-` order in descending order
    "fsm"
  ]
)$fsm
nochunkdata$fsm <- factor(nochunkdata$fsm, levels=newlevels1)
maxspeedup <- max(nochunkdata$cpu_time_median/nochunkdata$gpu_time_kernel_median, na.rm = TRUE)

speedups <- nochunkdata %>%
  group_by(fsm) %>%
  summarise(speedup=max(cpu_time_median/gpu_time_total_median, na.rm=TRUE))

#plot
ggplot(
       aes(x=num_tests, y=cpu_time_median/gpu_time_kernel_median, color=fsm), 
       data=nochunkdata
       ) +
  geom_line() +
  geom_point(size=4) +
  geom_hline(yintercept=1, linetype="dashed") +
  scale_colour_manual(values=gray.colors(15, 0.4, 0.4))+
  theme(legend.position ="none") +
  theme(axis.text.x=element_text(colour="black"))+
  theme(axis.text.y=element_text(colour="black"))+
  facet_wrap(~fsm, ncol=3)+
  # scales the x axis based on log2 and turns the labels in to powers of 2
  scale_x_continuous(trans=log2_trans(),
                     breaks=trans_breaks("log2", function(x) 2^x),
                     labels=trans_format("log2", math_format(2^.x))) +
  scale_y_continuous(trans=log2_trans(), 
                     breaks=trans_breaks("log2", function(x) 2^x)) +
  #scale_y_continuous(breaks=seq(0, maxspeedup, by=20)) +
  # better labels
  xlab("Number of inputs") +
  ylab("GPU kernel only speedup compared to a 16-core CPU")

cplotfile <- c(plotfolder, pathend, "-q1-kernel-only.pdf")
plotfile <- paste(cplotfile, collapse="")
ggsave(plotfile, width=13, height=12)

# Q1 analysis - kernel only
min(nochunkdata$cpu_time_median/nochunkdata$gpu_time_kernel_median)
max(nochunkdata$cpu_time_median/nochunkdata$gpu_time_kernel_median)
mean(nochunkdata$cpu_time_median/nochunkdata$gpu_time_kernel_median)

#Q1: Time breakdown
mdr <- melt(nochunkdata[,c('num_tests','fsm', 'config', 'chunksize_KB', 'gpu_time_total_median',
                         'gpu_time_inputs_median', 
                         'gpu_time_kernel_median',
                         'gpu_time_results_median'
                         )], id=c('num_tests','fsm', 'config', 'chunksize_KB', 'gpu_time_total_median'))
# rename legend labels
mdr$variable <- as.character(mdr$variable)
mdr$variable[mdr$variable == "gpu_time_inputs_median"] <- "Data transfer: inputs"
mdr$variable[mdr$variable == "gpu_time_kernel_median"] <- "Kernel execution"
mdr$variable[mdr$variable == "gpu_time_results_median"] <- "Data transfer: outputs"
mdr$variable <- factor(mdr$variable, levels=c("Data transfer: inputs", "Kernel execution", "Data transfer: outputs"))

# calculate the total time
mdrsums <- mdr %>%
  group_by(fsm, num_tests) %>%
  summarise(sum = sum(value))

mdr <- inner_join(mdr, mdrsums, by=c("fsm", "num_tests"))

ggplot(data=mdr, aes(x=num_tests, y=value/sum)) +
  geom_bar(aes(fill=variable), stat="identity") +
  #scale_fill_manual(values=cbPalette)+
  scale_fill_manual(values=gray3Palette)+
  theme(legend.title=element_blank()) +
  theme(legend.text=element_text(size=22)) +
  theme(legend.key.width=unit(1,"cm")) +
  theme(legend.spacing.x=unit(0.5,"cm")) +
  theme(axis.text.x=element_text(colour="black"))+
  theme(axis.text.y=element_text(colour="black"))+
  facet_wrap(~fsm, ncol=3) +
  theme(legend.position = "bottom") +
  # scales the x axis based on log2 and turns the labels in to powers of 2
  scale_x_continuous(trans=log2_trans(),
                     breaks=trans_breaks("log2", function(x) 2^x),
                     labels=trans_format("log2", math_format(2^.x))) +
  #scale_y_continuous(trans=log2_trans()) +
  scale_y_continuous(labels=scales::percent) +
  # better labels
  xlab("Number of inputs") +
  ylab("Percentage of total GPU time")

cplotfile <- c(plotfolder, pathend, "-nochunk-breakdown.pdf")
plotfile <- paste(cplotfile, collapse="")
ggsave(plotfile, width=13, height=13)

# Q3: Time without chunks, shows only CPU for large FSMs
# Bar plot, showing memory space required
testsuitesizefilename="fsmtesting/fsmdata-2019journal/data/analysis-fsm/test-suite-size.analysis"
testanalysisdata=read.csv(testsuitesizefilename)
newlevels2 = unique(
  testanalysisdata[
    order(testanalysisdata$size),
    "fsm"
  ]
)
testanalysisdata$fsm <- factor(testanalysisdata$fsm, levels=newlevels2)

ggplot(testanalysisdata, aes(fill=category, y=size, x=fsm)) +
  geom_bar(position="stack", stat="identity") +
  geom_hline(yintercept=12, linetype="dashed") +
  annotate("text", x=3, y=15, label="GPU Memory (12GB)", size=8) +
  #scale_fill_manual(values=cbPalette, labels=c("inputs", "outputs"))+
  scale_fill_manual(values=gray2Palette, labels=c("inputs", "outputs"))+
  scale_y_continuous(trans="log2")+
  theme(legend.title=element_blank()) +
  theme(legend.position = "bottom") +
  theme(legend.text=element_text(size=22)) +
  theme(legend.key.width=unit(1,"cm")) +
  theme(legend.spacing.x=unit(0.5,"cm")) +
  theme(axis.text.x=element_text(colour="black", angle=90)) +
  theme(axis.text.y=element_text(colour="black")) +
  xlab("") +
  ylab("Size in GBytes (log)")

cplotfile <- c(plotfolder, "test-suite-size.pdf")
plotfile <- paste(cplotfile, collapse="")
ggsave(plotfile, width=13, height=9)

# Q3: time WITH chunks vs cpu - shows all test sizes
# finds the chunksize wich achieves the minimum GPU time for the largest test suite per GPU
chunkdata_max$fsm <- factor(chunkdata_max$fsm, levels=newlevels)
ggplot(data=chunkdata_max, aes(x=num_tests, y=gpu_time_total_median, color="GPU total")) +
  geom_bar(stat="identity") +
  geom_line(data=chunkdata_max, aes(x=num_tests, y=cpu_time_median, color="CPU"))+
  geom_point(data=chunkdata_max, aes(x=num_tests, y=cpu_time_median, color="CPU"))+
  scale_colour_manual("",
                     breaks=c("CPU", "GPU total"),
                     values=c("CPU"=2, "GPU total"=3)) +
  facet_wrap(~fsm, ncol=3) +
  # scales the x axis based on log2 and turns the labels in to powers of 2
  scale_x_continuous(trans=log2_trans(),
                     breaks=trans_breaks("log2", function(x) 2^x),
                     labels=trans_format("log2", math_format(2^.x))) +
  scale_y_continuous(trans=log2_trans()) +
  # better labels
  xlab("Number of tests") +
  ylab("Execution time in ms")

cplotfile <- c(plotfolder, pathend, "-q3-chunk.pdf")
plotfile <- paste(cplotfile, collapse="")
ggsave(plotfile, width=13, height=10)

#Q2: speedup WITH chunks
chunkdata_max$fsm <- factor(chunkdata_max$fsm, levels=newlevels)
chunkdata_max<-transform(chunkdata_max,speedup_total=cpu_time_median/gpu_time_total_median)
gpudatanochunk<-transform(gpudatanochunk,speedup_total=cpu_time_median/gpu_time_total_median)
maxspeedup <- max(chunkdata_max$cpu_time_median/chunkdata_max$gpu_time_total_median, na.rm = TRUE)
mdri<-full_join(gpudatanochunk[c("fsm","num_tests","speedup_total")],
                chunkdata_max[c("fsm","num_tests","speedup_total")],
                by=c("fsm","num_tests"))
mdri<-melt(mdri,c("fsm","num_tests"))
mdri$variable <- as.character(mdri$variable)
mdri$variable[mdri$variable == "speedup_total.x"] <- "without overlap"
mdri$variable[mdri$variable == "speedup_total.y"] <- "with overlap"
mdri$variable <- factor(mdri$variable, levels=c("with overlap", "without overlap"))

ggplot(aes(x=num_tests, y=value, color=variable), data=mdri) +
  geom_line() +
  geom_point(size=4, aes(shape=variable)) +
  geom_hline(yintercept=1, linetype="dashed") +

  #scale_colour_manual(values=cbPalette)+
  scale_colour_manual(values=gray2Palette)+
  facet_wrap(~fsm, ncol=3) +
  # scales the x axis based on log2 and turns the labels in to powers of 2
  scale_x_continuous(trans=log2_trans(),
                     breaks=trans_breaks("log2", function(x) 2^x),
                     labels=trans_format("log2", math_format(2^.x))) +
  scale_y_continuous(trans=log2_trans(),
                     breaks=trans_breaks("log2", function(x) 2^x)) +
  #scale_y_continuous(breaks=seq(1, maxspeedup, by=3)) +
  theme(legend.title=element_blank()) +
  theme(legend.position="bottom") +
  theme(legend.text=element_text(size=26)) +
  theme(legend.key.width=unit(3,"cm")) +
  theme(legend.spacing.x=unit(0.5,"cm")) +
  theme(axis.text.x=element_text(colour="black"))+
  theme(axis.text.y=element_text(colour="black"))+
  theme(axis.title.x=element_text(colour="black"))+
  theme(axis.title.y=element_text(colour="black"))+
  # better labels
  xlab("Number of inputs") +
  ylab("GPU speedup compared to a 16-core CPU")

cplotfile <- c(plotfolder, pathend, "-q2-speedup.pdf")
plotfile <- paste(cplotfile, collapse="")
ggsave(plotfile, width=13, height=12)

# Q2 analysis
max(chunkdata_max$cpu_time_median/chunkdata_max$gpu_time_total_median, na.rm=TRUE)
mean(chunkdata_max$cpu_time_median/chunkdata_max$gpu_time_total_median, na.rm=TRUE)

# cpu vs gpu - tests per ms
cpudataall <- data[
                  data$config=="padded-transposed" &
                  data$sorted_unsorted=="sorted"
              ,]
cpudataall$fsm <- factor(cpudataall$fsm, levels=newlevels)

ggplot(data=chunkdata_max, aes(x=num_tests, y=gpu_tests_per_second_total, color="GPU total")) +
  #geom_bar(stat="identity") +
  geom_line() +
  geom_point() +
  geom_line(data=cpudataall, aes(x=num_tests, y=cpu_tests_per_second, color="CPU"))+
  geom_point(data=cpudataall, aes(x=num_tests, y=cpu_tests_per_second, color="CPU"))+
  scale_colour_manual("",
                     breaks=c("CPU", "GPU total"),
                     values=c("CPU"="firebrick", "GPU total"="forestgreen")) +
  theme(legend.title=element_blank()) +
  theme(legend.position="bottom") +
  theme(legend.text = element_text(size=26)) +
  facet_wrap(~fsm, ncol=3) +
  #scale_colour_manual("",
  #                   breaks=c("CPU", "GPU total"),
  #                   values=c("CPU"=2, "GPU total"=3)) +
  # scales the x axis based on log2 and turns the labels in to powers of 2
  scale_x_continuous(trans=log2_trans(),
                     breaks=trans_breaks("log2", function(x) 2^x),
                     labels=trans_format("log2", math_format(2^.x))) +
  scale_y_continuous(trans=log2_trans()) +
  # better labels
  xlab("Number of tests") +
  ylab("Tests per ms")

#Q2 analysis
min(chunkdata_max$cpu_time_median/chunkdata_max$gpu_time_total_median, na.rm=TRUE)
max(chunkdata_max$cpu_time_median/chunkdata_max$gpu_time_total_median, na.rm=TRUE)
mean(chunkdata_max$cpu_time_median/chunkdata_max$gpu_time_total_median, na.rm=TRUE)
max_su_per_fsm = chunkdata_max%>%
  group_by(fsm) %>%
  summarize(max_su_per_fsm=max(cpu_time_median/gpu_time_total_median, na.rm=TRUE))
min(max_su_per_fsm$max_su_per_fsm)
mean(max_su_per_fsm$max_su_per_fsm)

#Q2 statistical analysis
t.test(chunkdata_max$cpu_time_median/chunkdata_max$gpu_time_total_median, mu = 0, alternative = "greater")
sd(chunkdata_max$cpu_time_median/chunkdata_max$gpu_time_total_median, na.rm=TRUE)
t.test(chunkdata_max$cpu_time_median,chunkdata_max$gpu_time_total_median, paired = TRUE)

# GPU total speedup WITH chunks vs NO chunks
joineddata <- full_join(chunkdata_max, nochunkdata, by=c("fsm", "num_tests"))
ggplot(
       aes(x=num_tests, y=gpu_time_total_median.y/gpu_time_total_median.x, color=fsm), 
       data=joineddata
       ) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept=1, linetype="dashed") +
  facet_wrap(~fsm, ncol=3) +
  theme(legend.position = "none") +
  # scales the x axis based on log2 and turns the labels in to powers of 2
  scale_x_continuous(trans=log2_trans(),
                     breaks=trans_breaks("log2", function(x) 2^x),
                     labels=trans_format("log2", math_format(2^.x))) +
  #scale_y_continuous(trans=log2_trans()) +
  scale_y_continuous(breaks=seq(0, 100, by=1)) +
  # better labels
  xlab("Number of tests") +
  ylab("GPU speedup chunking vs no chunking")

# Q4 - percentage change in test suite size
library(scales)
reductionfilename="fsmtesting/fsmdata-2019journal/data/test-reduction/results.reduce"
reductiondata=read.csv(reductionfilename)
newlevels = unique(reductiondata[order(reductiondata$total.tests),"fsm"])
reductiondata$fsm <- factor(reductiondata$fsm, levels=newlevels)

reductiondata=filter(reductiondata, category=="large")
reductiondata$total.tests = as.numeric(as.character(reductiondata$total.tests))
reductiondata$removed.tests = as.numeric(as.character(reductiondata$removed.tests))

reductiondata = reductiondata %>%
  mutate(perc.change = (removed.tests)/total.tests*100)

ggplot(reductiondata, aes(y=perc.change, x=fsm)) +
  geom_bar(stat="identity", fill=gray1Palette, width=0.4) +
  #geom_bar(stat="identity", fill="#009292", width=0.4) +
  theme(legend.title=element_blank()) +
  theme(legend.position = "none") +
  theme(axis.text.x=element_text(colour="black", angle=90)) +
  theme(axis.text.y=element_text(colour="black")) +
  ylim(0, 0.5) +
  xlab("") +
  ylab("Number of inputs - % reduction")

cplotfile <- c(plotfolder, "test-suite-perc-change.pdf")
plotfile <- paste(cplotfile, collapse="")
ggsave(plotfile, width=13, height=9)

