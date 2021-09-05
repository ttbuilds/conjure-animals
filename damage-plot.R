source("conjure-animals.R")

damage_by_AC <- tibble(
  AC      = 6:25,
  Raptors = sapply(6:25, pack_tactics_damage, atk = 4, dpth = 10, n_animals = 7, n_pack_tactics = 6),
  Wolves  = sapply(6:25, pack_tactics_damage, atk = 4, dpth = 7, n_animals = 7, n_pack_tactics = 6),  
  Elks8   = sapply(6:25, elk_damage, STRsave = 4, n_elks = 6, n_charges = 5),
  Elks6   = sapply(6:25, elk_damage, STRsave = 4, n_elks = 6, n_charges = 3),  
  Elks4   = sapply(6:25, elk_damage, STRsave = 4, n_elks = 6, n_charges = 2),
  Elks2   = sapply(6:25, elk_damage, STRsave = 4, n_elks = 6, n_charges = 1))

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