calcThermo <- function(sim_vals, temp, vol, accum_pt){
  # Finds changes in concentration between timepoints and normalizes (essentially a crude derivative)
  added_prod <-
    transmute(sim_vals,
              minutes = minutes)
  #Need to multiply by something to 
  accumprod <- filter(sim_vals, minutes >= accum_pt)
  # Change in temp per vol (mL)
  temp_rate <- accumprod/vol
  
  return (temp_rate)
}