# We look at how the error in HyperLogLog cardinality estimation
# depends on the number of bits (or, the number of buckets) used.
#
# https://github.com/echen/streaming-simulations/wiki/How-the-accuracy-of-HyperLogLog-for-cardinality-estimation-depends-on-its-size

library(ggplot2)
library(scales)

###########################
# Data from a single run. #
###########################

d = read.csv("data/hll_cardinality_estimation_vs_space.tsv",
  sep = "\t",
  header = F,
  col.names = c("size", "estimate", "Bits"))
d$Bits = factor(d$Bits)

qplot(size, (estimate - size) / size, data = d,
  xlab = "True Cardinality", ylab = "% Error in Cardinality Estimation",
  main = "HyperLogLog for Cardinality Estimation",
  geom = "line", colour = Bits) +
  scale_colour_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3")) +
  scale_y_continuous(labels = percent)

######################################
# Aggregate data from multiple runs. #
######################################

d = read.csv("data/hll_cardinality_estimation_vs_size_100_runs.tsv",
  sep = "\t",
  header = F,
  col.names = c("size", "estimate", "Bits", "index"))
d$Bits = factor(d$Bits)
x = ddply(d, .(size, Bits), summarise, estimate = median(estimate), error = median(abs(estimate - size) / size))

qplot(size, (estimate - size) / size, data = x,
  xlab = "True Cardinality", ylab = "% Error in Cardinality Estimation",
  main = "HyperLogLog for Cardinality Estimation, Median Estimate over 100 runs",
  geom = "line", colour = Bits) +
  scale_colour_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A")) +
  scale_y_continuous(labels = percent)

qplot(size, error, data = x,
  xlab = "True Cardinality", ylab = "% Error in Cardinality Estimation",
  main = "HyperLogLog for Cardinality Estimation, Median Absolute Error over 100 runs",
  geom = "line", colour = Bits) +
  scale_colour_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A")) +
  scale_y_continuous(labels = percent)