# A note about code lists

The medcode and medcodeid fields must be kept as character vectors so that they do not get truncated. Stata, R and Excel all truncate these values if stored as numeric.
Some of these code lists, for example Ethnicity, undergo additional post-processing to select the variables of interest.
