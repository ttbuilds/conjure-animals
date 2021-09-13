library(tidyverse)
source("utils.R")

pack_tactics_damage <- function(AC, atk, dpth, dptc, n_animals, n_pack_tactics)
{
  ## AC:   enemy AC
  ## atk:  animal attack bonus
  ## dpth: expected damage per turn if all attacks hit
  ## dptc: expected damage per turn if all attacks crit  
  ## n_animals: number of animals
  ## n_pack_tactics: number assumed to get pack tactics (should be <= n_animals, obviously)
  hit_prob      = hit_chance(atk, AC)
  hit_prob_adv  = hit_chance_adv(atk, AC)
  crit_prob     = 0.05
  crit_prob_adv = 1 - (1 - 0.05)^2
  dpr           = n_pack_tactics * (hit_prob_adv * dpth + crit_prob_adv * dptc) +
                  (n_animals - n_pack_tactics) * (hit_prob * dpth + crit_prob * dptc)
  return(dpr)
}

charge_prone_damage <- function(
  AC, 
  STRsave, 
  n_animals, 
  n_charges, 
  prone_DC, 
  atk_mod,
  charge_dmg_dice,
  non_charge_dmg_dice,
  dmg_mod)
{
  atk = atk_mod
  DC  = prone_DC
  charge_damage          = charge_dmg_dice + dmg_mod
  charge_crit_damage     = charge_dmg_dice
  non_charge_damage      = non_charge_dmg_dice + dmg_mod
  non_charge_crit_damage = non_charge_dmg_dice
  hit_prob     = hit_chance(atk, AC)
  hit_prob_adv = hit_chance_adv(atk, AC)
  crit_prob     = 0.05
  crit_prob_adv = 1 - (1 - 0.05)^2
  prone_chance = 1 - hit_chance(STRsave, DC)
  hit_and_prone = hit_prob * prone_chance
  cumulative_charge_attempts = c(0:min(n_charges, n_animals - 1))
  if(n_charges < n_animals - 1) {
    cumulative_charge_attempts = c(cumulative_charge_attempts, rep(n_charges, n_animals-1-n_charges))
  }
  still_standing   = (1 - hit_and_prone)^cumulative_charge_attempts
  to_hit_sequence  = (1 - still_standing) * hit_prob_adv + still_standing * hit_prob
  to_crit_sequence = (1 - still_standing) * crit_prob_adv + still_standing * crit_prob  
  damage_sequence = rep(c(charge_damage, non_charge_damage), times = c(n_charges, n_animals - n_charges))
  crit_damage_sequence = rep(
    c(charge_crit_damage, non_charge_crit_damage), times = c(n_charges, n_animals - n_charges))
  hit_damage = sum(to_hit_sequence * damage_sequence)
  crit_damage = sum(to_crit_sequence * crit_damage_sequence)
  total_damage = hit_damage + crit_damage 
  return(total_damage)
}

