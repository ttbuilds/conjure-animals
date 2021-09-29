library(tidyverse)
source("../utils/utils.R")

pack_tactics_damage <- function(AC, atk, dpth, dptc, n_animals, n_pack_tactics)
{
  ## AC:   enemy AC
  ## atk:  animal attack bonus
  ## dpth: expected damage per turn if all attacks hit
  ## dptc: expected damage per turn if all attacks crit  
  ## n_animals: number of animals
  ## n_pack_tactics: number assumed to get pack tactics (should be <= n_animals, obviously)
  hit_prob      = hit_chance(atk, AC)
  hit_prob_adv  = hit_chance(atk, AC, adv = 1)
  crit_prob     = hit_chance(0, 20)
  crit_prob_adv = hit_chance(0,20, adv = 1)
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
  hit_prob_adv = hit_chance(atk, AC, adv = 1)
  crit_prob    = hit_chance(0, 20) 
  crit_prob_adv = hit_chance(0, 20, adv = 1)
  prone_chance = 1 - check_save_chance(STRsave, DC)
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
  final_prone = (1 - still_standing[n_charges]) + still_standing[n_charges] * hit_and_prone
  # print(final_prone)
  # print(1 - still_standing)
  # print(sum(to_hit_sequence))
  return(total_damage)
}

pounce_damage <- function(
  AC, 
  STRsave, 
  n_animals, 
  n_pounces, 
  prone_DC, 
  atk_mod,
  pounce_dmg_dice,
  non_pounce_dmg_dice,
  dmg_mod)
{
  atk = atk_mod
  DC  = prone_DC
  pounce_damage          = pounce_dmg_dice + dmg_mod
  pounce_crit_damage     = pounce_dmg_dice
  non_pounce_damage      = non_pounce_dmg_dice + dmg_mod
  non_pounce_crit_damage = non_pounce_dmg_dice
  hit_prob               = hit_chance(atk, AC)
  hit_prob_adv           = hit_chance(atk, AC, adv = 1)
  crit_prob              = hit_chance(0, 20) 
  crit_prob_adv          = hit_chance(0, 20, adv = 1)
  prone_chance           = 1 - check_save_chance(STRsave, DC)
  hit_and_prone          = hit_prob * prone_chance
  cumulative_pounce_attempts = c(0:min(n_pounces, n_animals - 1))
  if(n_pounces < n_animals - 1) {
    cumulative_pounce_attempts = c(cumulative_pounce_attempts, rep(n_pounces, n_animals-1-n_pounces))
  }
  still_standing            = (1 - hit_and_prone)^cumulative_pounce_attempts
  main_to_hit_sequence      = (1 - still_standing) * hit_prob_adv  + still_standing * hit_prob
  main_to_crit_sequence     = (1 - still_standing) * crit_prob_adv + still_standing * crit_prob  
  main_damage_sequence      = rep(c(non_pounce_damage, pounce_damage), times = c(n_pounces, n_animals - n_pounces))
  main_crit_damage_sequence = rep(c(non_pounce_crit_damage, pounce_crit_damage), c(n_pounces, n_animals - n_pounces))
  pounce_to_hit_sequence    = ((1 - still_standing) + still_standing * hit_and_prone) * hit_prob_adv
  pounce_to_crit_sequence   = ((1 - still_standing) + still_standing * hit_and_prone) * crit_prob_adv
  pounce_damage_sequence    =   rep(c(pounce_damage,      0), times = c(n_pounces, n_animals - n_pounces))
  pounce_crit_damage_sequence = rep(c(pounce_crit_damage, 0), times = c(n_pounces, n_animals - n_pounces))    
  main_hit_damage           = sum(main_to_hit_sequence  * main_damage_sequence)
  main_crit_damage          = sum(main_to_crit_sequence * main_crit_damage_sequence)
  main_total_damage         = main_hit_damage + main_crit_damage 
  pounce_hit_damage         = sum(pounce_to_hit_sequence * pounce_damage_sequence)
  pounce_crit_damage        = sum(pounce_to_crit_sequence * pounce_crit_damage_sequence)  
  pounce_total_damage       = pounce_hit_damage + pounce_crit_damage
  total_damage              = main_total_damage + pounce_total_damage
  final_prone               = (1 - still_standing[n_pounces]) + still_standing[n_pounces] * hit_and_prone
  # print(final_prone)
  # print(1 - still_standing)
  # print(sum(to_hit_sequence))
  return(total_damage)
}

