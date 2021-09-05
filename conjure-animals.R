library(tidyverse)

source("utils.R")

pack_tactics_damage <- function(AC, atk, dpth, n_animals, n_pack_tactics)
{
  ## AC:   enemy AC
  ## atk:  animal attack bonus
  ## dpth: expected damage per turn if all attacks hit
  ## n_animals: number of animals
  ## n_pack_tactics: number assumed to get pack tactics (should be <= n_animals, obviously)
  hit_prob     = hit_chance(atk, AC)
  hit_prob_adv = hit_chance_adv(atk, AC)
  dpr          = n_pack_tactics * hit_prob_adv * dpth + 
                  (n_animals - n_pack_tactics) * hit_prob * dpth
  return(dpr)
}

elk_damage <- function(AC, STRsave, n_elks, n_charges)
{
  atk = 5
  DC  = 13
  charge_damage     = 3 * 3.5 + 3
  non_charge_damage = 2 * 2.5 + 3
  hit_prob     = hit_chance(atk, AC)
  hit_prob_adv = hit_chance_adv(atk, AC)
  prone_chance = 1 - hit_chance(STRsave, DC)
  hit_and_prone = hit_prob * prone_chance
  cumulative_charge_attempts = c(0:min(n_charges, n_elks - 1))
  if(n_charges < n_elks - 1) {
    cumulative_charge_attempts = c(cumulative_charge_attempts, rep(n_charges, n_elks-1-n_charges))
  }
  still_standing  = (1 - hit_and_prone)^cumulative_charge_attempts
  to_hit_sequence = (1 - still_standing) * hit_prob_adv + still_standing * hit_prob
  damage_sequence = rep(c(charge_damage, non_charge_damage), times = c(n_charges, n_elks - n_charges))
  total_damage = sum(to_hit_sequence * damage_sequence)
  return(total_damage)
}