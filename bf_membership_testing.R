# We look at the false positive rate of Bloom filters with
# varying numbers of hashes.
#
# https://github.com/echen/streaming-simulations/wiki/Membership-Testing-False-Positive-Rate:-Bloom-Filter

library(ggplot2)
library(scales)

#######################################
# STEP 1. Read in the simulated data. #
#######################################

# Each Bloom filter is of width 4096, and uses 200 trials to estimate the
# FP rate.
BF_WIDTH = 4096
d = read.csv("data/bf_membership.tsv",
  sep = "\t",
  header = F,
  col.names = c("cardinality", "fp_rate", "num_hashes"))
d = subset(d, num_hashes <= 4)

######################################################
# STEP 2. Get the true expected false positive rate. #
######################################################

expected_fp_rate = function(num_hashes, width, cardinality) {
  (1 - (1 - 1 / width)^(num_hashes * cardinality))^num_hashes
  # (1 - exp(-num_hashes * cardinality / width))^num_hashes
}

expected_d = data.frame(
  num_hashes = d$num_hashes,
  cardinality = d$cardinality,
  fp_rate = expected_fp_rate(d$num_hashes, BF_WIDTH, d$cardinality)
)

############################
# STEP 3. Plot everything. #
############################

simulated =
  geom_line(data = d,
    aes(x = cardinality, y = fp_rate, colour = factor(num_hashes)),
    alpha = 1
  )
true =
  geom_line(data = expected_d,
    aes(x = cardinality, y = fp_rate, group = factor(num_hashes)),
    alpha = 0
  )
colors = scale_colour_manual(name = "# Hash Functions", values = c("#377EB8", "#E41A1C", "#4DAF4A"))

ggplot() + simulated + colors + true +
  xlab("Cardinality") +
  ylab("False Positive Rate") +
  scale_y_continuous(labels = percent) +
  opts(title = "Bloom Filter False Positive Rate")
