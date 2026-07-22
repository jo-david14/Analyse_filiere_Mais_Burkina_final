#=======================================================================
#          MODULE 1 : CHOIX ET JUSTIFICATION DU PRODUIT (MAÏS)
#=======================================================================
# Pré-requis : préambule.R (consommation, superficies, ventes, FAOSTAT)
#              déjà sourcé depuis main.R.

# --- Conversion en kg des quantités consommées (S07B) ---------------------
# Table NSU, spécifique Burkina Faso, même référentiel produit que S07B
nsu <- read_dta("donnee/base_burkina/ehcvm_nsu_bfa2021.dta")

table_conversion_nsu <- nsu %>%
  filter(!is.na(poids_moyen)) %>%
  group_by(codpr, uniteID, tailleID) %>%
  summarise(poids = mean(poids_moyen, na.rm = TRUE), .groups = "drop")

s07b_kg <- s07b %>%
  left_join(table_conversion_nsu,
            by = c("s07bq01" = "codpr",
                   "s07bq03b" = "uniteID",
                   "s07bq03c" = "tailleID")) %>%
  mutate(q_cons_kg = s07bq03a * poids / 1000)

# Nombre de lignes sans correspondance
sum(is.na(s07b_kg$q_cons_kg) & !is.na(s07b_kg$s07bq03a))


# --- Fréquence de consommation du maïs ------------------------------------
total_menages_s07b <- s07b %>% distinct(hhid, hhweight) %>%
  summarise(total = sum(hhweight, na.rm = TRUE)) %>% pull(total)

mais_freq_cons <- s07b_kg %>%
  mutate(label_produits = as_factor(s07bq01)) %>%
  filter(s07bq02 == 1) %>%
  group_by(label_produits) %>%
  summarise(n_pondere = sum(hhweight, na.rm = TRUE),
            qte_moyenne_kg = mean(q_cons_kg, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(pourcentage = round(100 * n_pondere / total_menages_s07b, 2)) %>%
  filter(str_detect(label_produits, regex("maïs", ignore_case = TRUE))) %>%
  select(-n_pondere)

View(mais_freq_cons)


# --- Quantité moyenne consommée, maïs, pondérée ---------------------------
codes_mais <- c(5, 6, 12, 13)  # Maïs en épi, Maïs en grain, Farine de maïs, Semoule de maïs

mais_qte_kg <- s07b_kg %>%
  filter(s07bq01 %in% codes_mais, s07bq02 == 1) %>%
  group_by(hhid, hhweight) %>%
  summarise(q_cons_kg_hh = sum(q_cons_kg, na.rm = TRUE), .groups = "drop") %>%
  summarise(
    qte_moy_kg = round(sum(q_cons_kg_hh * hhweight, na.rm = TRUE) /
                         sum(hhweight, na.rm = TRUE), 3)
  )
cat("Quantite moyenne de maïs consommee:", mais_qte_kg$qte_moy_kg, "kg/menage/7 jours\n")


# --- Sources d'approvisionnement du maïs ----------------------------------
# Autoconsommation, achat, cadeau, en kg
s07b_src <- s07b %>%
  left_join(table_conversion_nsu %>% select(codpr, uniteID, tailleID, poids),
            by = c("s07bq01" = "codpr", "s07bq03b" = "uniteID", "s07bq03c" = "tailleID")) %>%
  rename(poids_cons = poids) %>%
  left_join(table_conversion_nsu %>% select(codpr, uniteID, tailleID, poids),
            by = c("s07bq01" = "codpr", "s07bq07b" = "uniteID", "s07bq07c" = "tailleID")) %>%
  rename(poids_achat = poids) %>%
  mutate(
    autocons_kg = s07bq04 * poids_cons / 1000,
    achat_kg    = s07bq07a * poids_achat / 1000,
    cadeau_kg   = s07bq05 * poids_cons / 1000
  )

source_mais <- s07b_src %>%
  filter(s07bq01 %in% codes_mais, s07bq02 == 1) %>%
  summarise(
    autocons = sum(autocons_kg * hhweight, na.rm = TRUE),
    achat    = sum(achat_kg * hhweight, na.rm = TRUE),
    cadeau   = sum(cadeau_kg * hhweight, na.rm = TRUE)
  ) %>%
  mutate(total = autocons + achat + cadeau) %>%
  pivot_longer(cols = c(autocons, achat, cadeau),
               names_to = "source", values_to = "kg_pondere") %>%
  mutate(part = round(100 * kg_pondere / total, 2),
         source = factor(source, levels = c("autocons", "achat", "cadeau"),
                         labels = c("Autoconsommation", "Achat", "Cadeau")))

View(source_mais)

source_mais_graph <- ggplot(source_mais, aes(x = source, y = part, fill = source)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = paste0(part, "%")), vjust = -0.5) +
  scale_fill_manual(values = c("Autoconsommation" = "forestgreen",
                               "Achat" = "steelblue",
                               "Cadeau" = "darkorange")) +
  theme_minimal() +
  labs(title = "Sources d'approvisionnement du maïs (%)",
       x = "", y = "Part (%)") +
  theme(legend.position = "none")
ggsave("sorties/Sorties_module_1/sources_mais.png", plot = source_mais_graph, width = 8, height = 6, dpi = 300)


# --- Part du maïs dans la superficie agricole ------------------------------
part_sup_mais <- s16_a_c %>%
  filter(str_detect(produit, regex("maïs", ignore_case = TRUE)))
cat("Le maïs occupe", part_sup_mais$poids, "% de la superificie agricole totale du pays.")


# --- Part du maïs dans la valeur totale des ventes agricoles --------------
part_mais_vendu <- ventes %>%
  filter(str_detect(produit, regex("maïs", ignore_case = TRUE)))
cat("La part de la vente de maïs dans la valeur totale des ventes agricoles est de :",
    round(100 * part_mais_vendu$part_vente, 2), "%.")


# --- Part des producteurs de maïs parmi tous les producteurs --------------
total_producteurs_pondere <- producteurs %>%
  distinct(hhid, hhweight) %>%
  summarise(total = sum(hhweight, na.rm = TRUE)) %>%
  pull(total)

producteurs_mais_pondere <- producteurs %>%
  filter(str_detect(produit, regex("maïs", ignore_case = TRUE))) %>%
  distinct(hhid, hhweight) %>%
  summarise(total = sum(hhweight, na.rm = TRUE)) %>%
  pull(total)

part_producteurs_mais <- round(100 * producteurs_mais_pondere / total_producteurs_pondere, 2)

cat("Les producteurs de maïs représentent", part_producteurs_mais,
    "% de l'ensemble des ménages producteurs agricoles du pays.\n")

# --- Part des producteurs par produit, parmi les ménages agricoles --------
part_producteurs_produit <- producteurs %>%
  filter(!is.na(produit)) %>%
  group_by(produit) %>%
  summarise(
    part_menages = round(
      100 * sum(hhweight, na.rm = TRUE) / total_producteurs_pondere, 2)
  ) %>%
  arrange(desc(part_menages))

top10_part_producteurs <- part_producteurs_produit %>%
  slice_head(n = 10)

top10_part_producteurs_graph <- ggplot(top10_part_producteurs,
                                       aes(x = reorder(produit, -part_menages), y = part_menages)) +
  geom_bar(stat = "identity", fill = "chocolate4") +
  geom_text(aes(label = paste0(part_menages, "%")), vjust = -0.5) +
  theme_minimal() +
  labs(title = "Top 10 des produits par part des ménages producteurs (%)",
       x = "Produit", y = "Part des ménages producteurs (%)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("sorties/Sorties_module_1/top10_part_producteurs.png", plot = top10_part_producteurs_graph,
       width = 10, height = 6, dpi = 300)


# --- Part du maïs dans les calories disponibles ----------------------------
calories_mais <- calories %>%
  filter(codpr %in% codes_mais) %>%
  mutate(label = prodlab)

# Calories apportées par chaque type de maïs
cal_mais <- s07b_kg %>%
  filter(s07bq01 %in% codes_mais, s07bq02 == 1, !is.na(q_cons_kg)) %>%
  left_join(calories_mais %>% select(codpr, cal, refuse),
            by = c("s07bq01" = "codpr")) %>%
  mutate(
    kg_comestible = q_cons_kg * (1 - refuse / 100),
    kcal = kg_comestible * 10 * cal
  ) %>%
  group_by(s07bq01) %>%
  summarise(
    kcal_total = sum(kcal * hhweight, na.rm = TRUE),
    kg_total   = sum(q_cons_kg * hhweight, na.rm = TRUE)
  )

# Calories totales, tous produits confondus
total_kcal_pays <- s07b_kg %>%
  filter(s07bq02 == 1, !is.na(q_cons_kg)) %>%
  left_join(calories %>% select(codpr, cal, refuse),
            by = c("s07bq01" = "codpr")) %>%
  mutate(
    kg_comestible = q_cons_kg * (1 - coalesce(refuse, 0) / 100),
    kcal = kg_comestible * 10 * coalesce(cal, 0)
  ) %>%
  summarise(tot = sum(kcal * hhweight, na.rm = TRUE))

# Part du maïs (tous types confondus) dans le total national
part_mais_calories_nationales <- cal_mais %>%
  summarise(kcal_mais_total = sum(kcal_total, na.rm = TRUE)) %>%
  mutate(part_cal = round(100 * kcal_mais_total / total_kcal_pays$tot, 2))

print(part_mais_calories_nationales)
cat("Le maïs représente", part_mais_calories_nationales$part_cal,
    "% des calories disponibles (proxy national, données EHCVM).\n")


# --- Balance commerciale du maïs -------------------------------------------
balance_table <- export_val %>%
  full_join(import_val, by = "Item", suffix = c("_export", "_import")) %>%
  filter(str_detect(Item, regex("maize", ignore_case = TRUE))) %>%
  summarize(
    val_export_mais = sum(val_export, na.rm = TRUE),
    val_import_mais = sum(val_import, na.rm = TRUE),
    balance = val_export_mais - val_import_mais
  )
print(balance_table)
cat("La balance commerciale du maïs et de ses produits dérivés est globalement déficitaire.")

balance_table_2 <- export_val %>%
  full_join(import_val, by = "Item", suffix = c("_export", "_import")) %>%
  filter(str_detect(Item, regex("maize", ignore_case = TRUE))) %>%
  mutate(
    val_export = coalesce(val_export, 0),
    val_import = coalesce(val_import, 0),
    balance = val_export - val_import
  )
cat("La balance commerciale du maïs pur est excédentaire tandis que celle de ses produits dérivés est déficitaire.")
