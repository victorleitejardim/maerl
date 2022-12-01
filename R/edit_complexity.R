edit_complexity <- function(table){
  table <- table %>% 
    mutate(Mini = paste(complexity$Code, complexity$Point, sep = "_")) %>% 
    mutate(Site = recode(Code,"NBK" = "Keraliou", "MBE" = "Belle-ile", "MCM" = "Camaret", "MGL" = "Glenan","MME"= "Meaban","MMO" = "Molene", "MMX" = "Morlaix", "MRZ" = "Rozegat", "MPP" = "Saint-Brieuc", "MTR" = "Trevignon")) %>%  
    mutate(Site = factor(Site, levels = c("Belle-ile", "Meaban", "Glenan", "Trevignon", "Camaret", "Keraliou", "Rozegat", "Molene", "Morlaix", "Saint-Brieuc"))) %>% 
    mutate(Old_density = recode(Site, "Keraliou" = 	2288, "Belle-ile" = 1311, "Camaret" = 1322, "Glenan" = 2155, "Meaban" = 2222, "Molene" = 977, "Morlaix" = 3000, "Saint-Brieuc" = 277, "Rozegat" = 3377, "Trevignon" = 988)) %>%  ## add maerl density data at site level
    mutate(Sphericity = ((S^2)/(L*I))^(1/3)) %>% 
    mutate(DR1 = S/L) %>% 
    mutate(DR2 = ((L-I)/(L-S))) %>% 
    mutate(DR3 = I/L) %>% 
    mutate(Point = paste(complexity$Site, complexity$Point, sep = " ")) %>% 
    mutate(Point = factor(Point, levels = c("Belle-ile 1","Belle-ile 2", "Belle-ile 3", "Meaban 1", "Meaban 2", "Meaban 3", "Glenan 1", "Glenan 2", "Glenan 3", "Trevignon 1", "Trevignon 2", "Trevignon 3", "Camaret 1", "Camaret 2", "Camaret 3", "Keraliou 1", "Keraliou 2", "Keraliou 3", "Rozegat 1", "Rozegat 2", "Rozegat 3", "Molene 1", "Molene 2", "Molene 3", "Morlaix 1", "Morlaix 2", "Morlaix 3", "Saint-Brieuc 1", "Saint-Brieuc 2", "Saint-Brieuc 3"))) %>% 
    select(Mini, Site, Point, everything(), -Code) %>%
    as.data.frame() %>%
    mutate_if(is.character, as.factor)
}
