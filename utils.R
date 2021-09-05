hit_chance <- function(atk, AC)
{
  min_roll_tohit <- AC - atk
  tohit <- max(min((21 - min_roll_tohit) / 20, 0.95), 0.05)
  return(tohit)
}

hit_chance_adv <- function(atk, AC)
{
  base_tohit <- hit_chance(atk,AC)
  tohit_adv <- 1 - (1 - base_tohit)^2
  return(tohit_adv)
}
