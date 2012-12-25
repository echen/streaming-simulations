# We look at the false positive rate of HyperLogLog for membership testing.
#
# https://github.com/echen/streaming-simulations/wiki/Membership-Testing-False-Positive-Rate:-HyperLogLog

library(ggplot2)
library(scales)

#######################################
# STEP 1. Read in the simulated data. #
#######################################

# The HyperLogLog is of size 4096 bits, so it uses 9 bucketing bits
# and has 2^9 = 512 buckets.
NUM_BUCKETS = 512
d = read.csv("hll_membership.tsv",
  sep = "\t",
  header = F,
  col.names = c("cardinality", "fp_rate", "Trial"))

######################################################
# STEP 2. Get the true expected false positive rate. #
######################################################

expected_fp_rate = function(cardinality) {
  k = c(1:100)

  sum = 0
  f = function(k) {
    # 1 / (2^k) * (1 - (1 - 1 / (NUM_BUCKETS * 2^(k-1)))^cardinality)
    1 / (2^k) * (1 - exp(-cardinality / (NUM_BUCKETS * 2^(k-1))))
  }
  for (k in 1:100) {
    sum = sum + f(k)
  }

  sum
}

expected_d = data.frame(
  cardinality = d$cardinality,
  fp_rate = expected_fp_rate(d$cardinality)
)

############################
# STEP 3. Plot everything. #
############################

d = subset(d, Trial == "Trial 1")
simulated =
  geom_line(data = d,
    aes(x = cardinality, y = fp_rate), alpha = 0.5, colour = "#034E7B"
  )
true =
  geom_line(data = expected_d,
    aes(x = cardinality, y = fp_rate),
  )

ggplot() + simulated + true +
  xlab("Cardinality") +
  ylab("False Positive Rate") +
  scale_y_continuous(labels = percent) +
  opts(title = "HyperLogLog for Membership Testing")
