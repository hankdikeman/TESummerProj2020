calcThermo <- function(sim_vals, temp, vol, accum_pt){
  added_prod <-
    transmute(sim_vals,
              minutes = minutes)
  #Need to multiply by something to 
  accumprod <- filter(sim_vals, minutes >= accum_pt)
  temp_rate <- accumprod/vol
  
  return (temp_rate)
}