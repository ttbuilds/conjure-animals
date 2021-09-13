source("conjure-animals.R")

damage_by_AC <- tibble(
  AC      = 6:25,
  Raptors = sapply(6:25, pack_tactics_damage, atk = 4, dpth = 10, dptc = 6, n_animals = 8, n_pack_tactics = 8),
  Wolves  = sapply(6:25, pack_tactics_damage, atk = 4, dpth = 7, dptc = 5, n_animals = 8, n_pack_tactics = 8),  
  Elks8   = sapply(
    6:25, charge_prone_damage, 
    STRsave   = 4, n_animals = 8, 
    n_charges = 8, prone_DC  = 13, 
    atk_mod   = 5, dmg_mod   = 3,
    charge_dmg_dice = 10.5, 
    non_charge_dmg_dice = 5),
  Elks6   = sapply(
    6:25, charge_prone_damage, 
    STRsave = 4, n_animals = 8, 
    n_charges = 6, prone_DC = 13, 
    atk_mod = 5, dmg_mod   = 3,
    charge_dmg_dice = 10.5, 
    non_charge_dmg_dice = 5),  
  Elks4   = sapply(
    6:25, charge_prone_damage, 
    STRsave = 4, n_animals = 8, 
    n_charges = 4, prone_DC = 13, 
    atk_mod = 5, dmg_mod   = 3,
    charge_dmg_dice = 10.5, 
    non_charge_dmg_dice = 5),  
  Elks2   = sapply(
    6:25, charge_prone_damage, 
    STRsave = 4, n_animals = 8, 
    n_charges = 2, prone_DC = 13, 
    atk_mod = 5, dmg_mod   = 3,
    charge_dmg_dice = 10.5, 
    non_charge_dmg_dice = 5))

plot_data <- damage_by_AC %>%
  pivot_longer(
    cols = -AC,
    names_to = "Summon",
    values_to = "DPR")

damage_comparison_plot <- plot_data %>%
  ggplot(aes(x = AC, y = DPR, color = Summon)) +
    geom_point() +
    geom_line() +
    scale_x_continuous(
      name = "Enemy AC",
      breaks = 6:25) +
    scale_y_continuous(
      name   = "Damage Per Round (Assuming 8 attacks)",
      breaks = seq(0, 120, by = 5))