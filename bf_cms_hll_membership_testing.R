# We compare the false positive rates of
# - Bloom filters
# - Count-Min sketches
# - HyperLogLogs
# for membership testing.
#
# https://github.com/echen/streaming-simulations/wiki/Membership-Testing%3A-Bloom-Filter-vs.-Count-Min-Sketch-vs.-HyperLogLog

library(ggplot2)
library(scales)

#######################################
# STEP 1. Read in the simulated data. #
#######################################

# Each Bloom filter is of size 4096 bits, and uses 200 trials to estimate the
# FP rate.
d = read.csv("data/bf_cms_hll_membership.tsv",
  sep = "\t",
  header = F,
  col.names = c("cardinality", "fp_rate", "Structure"))

############################
# STEP 2. Plot everything. #
############################

base =
  qplot(cardinality, fp_rate, data = d, geom = "line",
    colour = Structure,
    xlab = "Cardinality", ylab = "False Positive Rate",
    main = "Membership Testing under Three Streaming Algorithms")
colors = scale_colour_manual(values = c("#377EB8", "#E41A1C", "#4DAF4A"))

base + colors + scale_y_continuous(labels = percent)
