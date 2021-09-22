library(dplyr)
library(ggplot2)
library(scales)

theme_set(theme_minimal(base_size=22))

# test sutie size
testsizefile="fsmtesting/gpu-fsm-validation-artifacts/data/input-analysis/test-suite-size.analysis"
testsuitedata=read.csv(testsizefile)

# average test lenth per fsm per test suite size
newlevels = unique(
  testsuitedata[order(-testsuitedata$test.length.avg),
          "fsm"]
)

testsuitedata$fsm <- factor(testsuitedata$fsm, levels=newlevels)
p <- ggplot(aes(x=num.tests, y=test.length.avg, color=fsm), data=testsuitedata) +
  geom_point() +
  geom_line() +
  #geom_errorbar(aes(ymin=test.length.avg-test.length.sd, ymax=test.length.avg+test.length.sd), width=.2, position=position_dodge(.9)) +
  scale_x_continuous(trans=log2_trans(),
                     breaks=trans_breaks("log2", function(x) 2^x),
                     labels=trans_format("log2", math_format(2^.x))) +
  xlab("Size of the test suite") +
  ylab("Average test lenght")
p
