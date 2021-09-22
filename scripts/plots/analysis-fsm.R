# analysis-fsm
library(ggplot2)
library(grid)
library(gridExtra)
library(scales)
library(RColorBrewer)
library(reshape2)
library(dplyr)

theme_set(theme_minimal(base_size=22))
cbPalette <- c("#004949", "#ff6db6", "#009292", "#ffb677")
gray1Palette <- gray.colors(1, 0.4, 0.4)
gray2Palette <- gray.colors(2, 0.3, 0.7)

analysisfilename="fsmtesting/gpu-fsm-validation-artifacts/data/analysis-fsm/all-fsm.analysis"
densityfilename="fsmtesting/gpu-fsm-validation-artifacts/data/fsm-density/all-fsm.density"
plotfolder <- "fsmtesting/gpu-fsm-validation-artifacts/data/generated-plots/"

fsmanalysisdata=read.csv(analysisfilename)
fsmdensitydata=read.csv(densityfilename)
fsmdata=inner_join(fsmanalysisdata, fsmdensitydata, by="fsm")
fsmdata=fsmdata[fsmdata$category=="large",]

# average test length per fsm
newlevels = unique(
  fsmdata[order(fsmdata$test.length.avg),
          "fsm"]
)
fsmdata$fsm <- factor(fsmdata$fsm, levels=newlevels)

corr_eqn <- function(x,y,digits=2) {
  corr_coef <- round(cor(x,y),digits=digits)
  paste("italic(r) == ", corr_coef)
}

#p <- ggplot(aes(x=fsm, y=test.length.avg, color=category), data=fsmdata) +
p <- ggplot(aes(x=fsm, y=test.length.avg), data=fsmdata) +
  #geom_point(color="#009292", size=4) +
  geom_point(color=gray1Palette, size=6) +
  geom_errorbar(aes(ymin=test.length.avg-test.length.sd, ymax=test.length.avg+test.length.sd), width=.2, position=position_dodge(.9),
  #              color="#009292") +
                color=gray1Palette) +
  theme(axis.text.x=element_text(angle=90, color="black")) +
  theme(axis.text.y=element_text(color="black")) +
  xlab("") +
  ylab("Average input length")
p

cplotfile <- c(plotfolder, "fsm-test-lenghts.pdf")
plotfile <- paste(cplotfile, collapse="")
ggsave(plotfile, width=7, height=7)


# num tests vs num states
p1 <- ggplot(aes(x=num.states.x, y=num.tests), data=fsmdata) +
  geom_point() +
  geom_text(aes(label=fsm), vjust=-0.1, size=6, angle=40) +
  xlab("Number of states") +
  ylab("Number of tests")

# num tests vs num transitions
p2 <- ggplot(aes(x=num.transitions.x, y=num.tests), data=fsmdata) +
  geom_point() +
  geom_text(aes(label=fsm), vjust=-0.1, size=6, angle=40) +
  xlab("Number of transitions") +
  ylab("Number of tests")

# num tests vs fsm density
p3 <- ggplot(aes(x=density..., y=num.tests), data=fsmdata) +
  geom_point() +
  geom_text(aes(label=fsm), vjust=-0.1, size=6, angle=40) +
  xlab("FSM density (%)") +
  ylab("Number of tests")

grid.arrange(p1, p2, p3, ncol=3)

# avg test length vs num states
p11 <- ggplot(aes(x=num.states.x, y=test.length.avg), data=fsmdata) +
  geom_point() +
  geom_text(aes(label=fsm), vjust=-0.1) +
  xlab("Number of states") +
  ylab("Avg test length")

# avg test lenth vs num transitions
p21 <- ggplot(aes(x=num.transitions.x, y=test.length.avg), data=fsmdata) +
  geom_point() +
  geom_text(aes(label=fsm), vjust=-0.1) +
  xlab("Number of transitions") +
  ylab("Avg test length")

# avg test length vs fsm density
p31 <- ggplot(aes(x=density..., y=test.length.avg), data=fsmdata) +
  geom_point() +
  geom_text(aes(label=fsm), vjust=-0.1) +
  xlab("FSM density (%)") +
  ylab("Avg test length")

grid.arrange(p11, p21, p31, ncol=3)

# avg test length vs number of tests
p4 <- ggplot(aes(x=num.tests, y=test.length.avg), data=fsmdata) +
  geom_point() +
  geom_text(aes(label=fsm), vjust=-0.1) +
  xlab("Number of tests") +
  ylab("Avg test length")
p4

# this part uses "chunkdata" from the "speedup.R" script
library(dplyr)
maxsu <- chunkdata %>% 
  group_by(fsm) %>% 
  summarise(max.speedup=max(cpu_time_median/gpu_time_total_median, na.rm=TRUE))

combdata <- left_join(maxsu, fsmdata, by="fsm")
combdata

# max speedup vs avg. test length
p3 <- ggplot(aes(x=test.length.avg, y=max.speedup), data=combdata) +
  geom_point() +
  geom_text(aes(label=fsm), size=7, vjust=-0.1) +
  geom_label(x=400, y=7.5, size=8, color=gray1Palette,
  #geom_label(x=400, y=7.5, size=8, color="#007500",
             label=corr_eqn(combdata$test.length.avg, combdata$max.speedup),
             parse=TRUE) +
  geom_smooth(method="lm", size=0.5, color=gray1Palette, se=FALSE, linetype="dashed") +
  #geom_smooth(method="lm", size=0.5, color="#007500", se=FALSE, linetype="dashed") +
  theme(axis.text.x=element_text(colour="black"))+
  theme(axis.text.y=element_text(colour="black"))+
  xlab("Average input length") +
  ylab("Maximum speedup on the GPU")
p3
corr_eqn(combdata$test.length.avg, combdata$max.speedup)

cplotfile <- c(plotfolder, pathend, "-su-vs-tl.pdf")
plotfile <- paste(cplotfile, collapse="")
ggsave(plotfile, width=13, height=8)

# max speedup vs max. test length
p <- ggplot(aes(x=test.length.max, y=max.speedup), data=combdata) +
  geom_point() +
  geom_text(aes(label=fsm), size=7, vjust=-0.1) +
  geom_label(x=900, y=6, size=8, color="#007500",
            label=corr_eqn(combdata$test.length.max, combdata$max.speedup), parse=TRUE) +
  geom_smooth(method="lm", size=0.5, color="#007500", se=FALSE, linetype="dashed") +
  xlab("Maximum test length") +
  ylab("Maximum speedup")
p
corr_eqn(combdata$test.length.max, combdata$max.speedup);

# max speedup vs num tests
p4 <- ggplot(aes(x=num.tests, y=max.speedup), data=combdata) +
  geom_point() +
  geom_text(aes(label=fsm), size=7, vjust=-0.1) +
  geom_label(x=100000000, y=5, size=8, color="#007500",
             label=corr_eqn(combdata$num.tests, combdata$max.speedup), parse=TRUE) +
  geom_smooth(method="auto", size=0.5, color="#007500", se=FALSE, linetype="dashed") +
  xlab("Number of tests") +
  ylab("Maximum speedup")
p4
corr_eqn(combdata$num.tests, combdata$max.speedup)

cplotfile <- c(plotfolder, pathend, "-su-vs-tl.pdf")
plotfile <- paste(cplotfile, collapse="")
ggsave(plotfile, width=13, height=7)

# max test length vs test suite size
combdata4 <- left_join(chunkdata, fsmdata, by="fsm")
p <- ggplot(aes(x=test.length.avg, y=testsuitesize_B, color=num_tests), data=combdata4) +
  geom_point() +
  geom_text(aes(label=fsm), vjust=-0.1) +
  facet_wrap(~num_tests, ncol=3) +
  #scale_y_continuous(trans=log2_trans()) +
  xlab("Maximum test length") +
  ylab("Test suite size in bytes")
p

# max speedup vs percentage of execution time
percentdata <- nochunkdata %>% 
  group_by(fsm) %>% 
  summarise(kernel.perc=min(gpu_time_kernel_median/gpu_time_total_median))

combdata2 <- left_join(maxsu, percentdata, by="fsm")

p5 <- ggplot(aes(x=kernel.perc, y=max.speedup), data=combdata2) +
  geom_point() +
  geom_text(aes(label=fsm), vjust=-0.1, size=6, angle=40) +
  geom_label(x=8, y=6, size=8, color="#007500",
             label=corr_eqn(combdata2$kernel.perc, combdata2$max.speedup), parse=TRUE) +
  geom_smooth(method="auto", size=0.5, color="#007500", se=FALSE, linetype="dashed") +
  xlab("Number of tests") +
  scale_x_continuous(labels=scales::percent) +
  xlab("Kernel execution time as percentage of total GPU time") +
  ylab("Maximum speedup")
p5
corr_eqn(combdata2$kernel.perc, combdata2$max.speedup)

cplotfile <- c(plotfolder, pathend, "-su-vs-percent.pdf")
plotfile <- paste(cplotfile, collapse="")
ggsave(plotfile, width=13, height=8)

# max input transfer rate vs avg. test length
transferdata <- nochunkdata %>%
  group_by(fsm) %>%
  summarise(max.input.transfer.rate = max(num_tests/gpu_time_inputs_median))

combdata3 <- left_join(transferdata, fsmdata, by="fsm")
combdata3

p6 <- ggplot(aes(x=test.length.avg, y=max.input.transfer.rate), data=combdata3) +
  geom_point() +
  geom_text(aes(label=fsm), vjust=-0.1, size=6, angle=40) +
  geom_label(x=400, y=40000, size=8, color="#007500",
             label=corr_eqn(combdata3$test.length.avg, combdata3$max.input.transfer.rate), parse=TRUE) +
  geom_smooth(method="auto", size=0.5, color="#007500", se=FALSE, linetype="dashed") +
  xlab("Average test length") +
  ylab("Maximum input transfer rate (test per ms)")
p6
corr_eqn(combdata3$test.length.avg, combdata3$max.input.transfer.rate)

# density vs perc reduced
library(scales)
reductionfilename="fsmtesting/fsmdata-2019journal/data/test-reduction/results.reduce"
reductiondata=read.csv(reductionfilename)
reductiondata$total.tests = as.numeric(as.character(reductiondata$total.tests))
reductiondata$removed.tests = as.numeric(as.character(reductiondata$removed.tests))
reductiondata$covered.pairs.perc = as.numeric(as.character(reductiondata$covered.pairs.perc))
reductiondata=reductiondata[reductiondata$covered.pairs.perc > 98,]

reductiondata = reductiondata %>%
  mutate(perc.change = (removed.tests)/total.tests*100)
fsmreddata = inner_join(reductiondata, fsmdensitydata, by="fsm")
fsmreddata$category<-as.character(fsmreddata$category)
fsmreddata$category[fsmreddata$category=="small"]<-"additional FSMs"
fsmreddata$category[fsmreddata$category=="large"]<-"subject FSMs"
fsmreddata$category<-factor(fsmreddata$category,levels=c("subject FSMs", "additional FSMs"))

ggplot(aes(x=density..., y=perc.change, color=category), data=fsmreddata, color=category) +
  geom_point(size=6, aes(shape=category)) +
  #scale_colour_manual(values=cbPalette)+
  scale_colour_manual(values=gray2Palette)+
  geom_label(x=27, y=6, size=8, color=gray1Palette,
  #geom_label(x=27, y=6, size=8, color="#007500",
             label=corr_eqn(fsmreddata$density..., fsmreddata$perc.change),
             parse=TRUE) +
  geom_smooth(method="lm", size=0.5, color=gray1Palette, se=FALSE, linetype="dashed") +
  #geom_smooth(method="lm", size=0.5, color="#007500", se=FALSE, linetype="dashed") +
  theme(legend.position = "bottom") +
  theme(legend.title=element_blank())+
  theme(legend.text=element_text(size=22)) +
  theme(legend.key.width=unit(1,"cm")) +
  theme(legend.spacing.x=unit(0.5,"cm")) +
  theme(axis.text.x=element_text(colour="black"))+
  theme(axis.text.y=element_text(colour="black"))+
  xlab("FSM density (%)") +
  ylab("Number of inputs - % reduction")
corr_eqn(fsmreddata$density..., fsmreddata$perc.change)

cplotfile <- c(plotfolder, "density-vs-perc-change.pdf")
plotfile <- paste(cplotfile, collapse="")
ggsave(plotfile, width=13, height=9)
