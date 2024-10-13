# Function to add a pill
add_adjustment_pill <- function(adjustment_name, executed_adjustments) {
  current_adjustments <- executed_adjustments()
  if (!(adjustment_name %in% names(current_adjustments))) {
    current_adjustments[[adjustment_name]] <- TRUE
    executed_adjustments(current_adjustments)
    print(paste("Added pill:", adjustment_name))
    print("Current adjustments:")
    print(executed_adjustments())
  }
}

# Function to remove a pill
remove_adjustment_pill <- function(adjustment_name, executed_adjustments) {
  current_adjustments <- executed_adjustments()
  current_adjustments[[adjustment_name]] <- NULL
  executed_adjustments(current_adjustments)
}
