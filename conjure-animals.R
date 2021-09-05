library(tidyverse)

hit_chance <- function(atk, AC)
{
  min_to_hit = AC - atk
  chance = max(min((21 - min_to_hit) / 20, 0.95), 0.05)
  return(chance)
}

pack_tactics_damage <- function(AC, atk, dpth, n_animals, n_pack_tactics)
{
  hit_prob = hit_chance(atk, AC)
  hit_prob_adv = 1 - (1 - hit_prob)^2
  dpr = n_pack_tactics * hit_prob_adv * dpth + 
    (n_animals - n_pack_tactics) * hit_prob * dpth
  return(dpr)
}

elk_damage <- function(AC, STRsave, n_elks, n_charges)
{
  atk = 5
  DC = 13
  charge_damage = 3 * 3.5 + 3
  non_charge_damage = 2 * 2.5 + 3
  hit_prob = hit_chance(atk, AC)
  hit_prob_adv = 1 - (1 - hit_prob)^2
  prone_chance = 1 - hit_chance(STRsave, DC)
  hit_and_prone = hit_prob * prone_chance
  cumulative_charge_attempts = c(0:min(n_charges, n_elks - 1))
  if(n_charges < n_elks - 1) {
    cumulative_charge_attempts = c(cumulative_charge_attempts, rep(n_charges, n_elks-1-n_charges))
  }
  still_standing = (1 - hit_and_prone)^cumulative_charge_attempts
  to_hit_sequence = (1 - still_standing) * hit_prob_adv + still_standing * hit_prob
  damage_sequence = rep(c(charge_damage, non_charge_damage), times = c(n_charges, n_elks - n_charges))
  total_damage = sum(to_hit_sequence * damage_sequence)
  return(total_damage)
}

damage_by_AC <- tibble(
  AC      = 6:25,
  Raptors = sapply(6:25, pack_tactics_damage, atk = 4, dpth = 10, n_animals = 7, n_pack_tactics = 6),
  Wolves  = sapply(6:25, pack_tactics_damage, atk = 4, dpth = 7, n_animals = 7, n_pack_tactics = 6),  
  Elks8   = sapply(6:25, elk_damage, STRsave = 4, n_elks = 6, n_charges = 5),
  Elks6   = sapply(6:25, elk_damage, STRsave = 4, n_elks = 6, n_charges = 3),  
  Elks4   = sapply(6:25, elk_damage, STRsave = 4, n_elks = 6, n_charges = 2),
  Elks2   = sapply(6:25, elk_damage, STRsave = 4, n_elks = 6, n_charges = 1))

damage_by_AC %>%
  pivot_longer(
    cols = -AC,
    names_to = "Summon",
    values_to = "DPR") %>%
ggplot(aes(x = AC, y = DPR, color = Summon)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(
    name = "Enemy AC",
    breaks = 6:25) +
  scale_y_continuous(
    name   = "Damage Per Round (Assuming 8 attacks)",
    breaks = seq(0, 120, by = 5))
