# Column names referenced via non-standard evaluation inside dplyr/tidyr verbs.
# Declaring them here avoids spurious "no visible binding for global variable"
# NOTEs from R CMD check.
utils::globalVariables(c("age", "condition", "gender", "id", "score"))
