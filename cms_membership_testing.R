# We look at the false positive rate of Count-Min sketches with
# varying depths and widths.
#
# https://github.com/echen/streaming-simulations/wiki/Membership-Testing-False-Positive-Rate:-Count-Min-Sketch

library(ggplot2)
library(scales)

#######################################
# STEP 1. Read in the simulated data. #
#######################################

# Each Count-Min sketch is of size 4096 bits, and uses 200 trials
# to estimate the FP rate.
d = read.csv("data/cms_membership.tsv",
  sep = "\t",
  header = F,
  col.names = c("cardinality", "fp_rate", "num_hashes"))
d$width = 4096 / (32 * d$num_hashes)
d = subset(d, cardinality < 500)

######################################################
# STEP 2. Get the true expected false positive rate. #
######################################################

expected_fp_rate = function(num_hashes, width, cardinality) {
  (1 - (1 - 1 / width)^cardinality)^num_hashes
}

expected_d = data.frame(
  num_hashes = d$num_hashes,
  cardinality = d$cardinality,
  fp_rate = expected_fp_rate(d$num_hashes, d$width, d$cardinality)
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
  opts(title = "Count-Min Sketch False Positive Rate")
