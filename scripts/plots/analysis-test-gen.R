library(dplyr)
library(ggplot2)

densityfilename <- "fsmtesting/gpu-fsm-validation-artifacts/data/fsm-density/all-fsm.density"
datadensity <- read.csv(densityfilename)
reductiondata <- read.csv("fsmtesting/gpu-fsm-validation-artifacts/data/input-reduction/results.reduce")
reductiondata <- reductiondata[reductiondata$fsm != "fsm12",]

joineddata <- inner_join(datadensity,reductiondata,by="fsm")

# test reduction
p1 <- ggplot(aes(x=density..., y=removed.perc), data=joineddata) +
  geom_point() +
  geom_text(aes(label=fsm), hjust=-0.1, vjust=-0.1) +
  xlab("Density(%)") +
  ylab("Test suite reduction(%)")
p1
