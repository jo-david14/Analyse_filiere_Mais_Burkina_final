#=======================================================================
#                            PREAMBULE
#=======================================================================

# --- Fréquence de consommation, tous produits ----------------------------
cons <- s07b %>%
  mutate(label_produits = as_factor(s07bq01)) %>%
  filter(s07bq02 == 1) %>%
  group_by(label_produits) %>%
  summarise(n_pondere = sum(hhweight, na.rm = TRUE)) %>%
  mutate(pourcentage = round(100 * n_pondere / sum(n_pondere), 2)) %>%
  arrange(desc(n_pondere))

top10_cons <- cons %>%
  slice_head(n = 10)

top10_cons_graph <- ggplot(top10_cons, aes(x = reorder(factor(label_produits), -n_pondere), y = pourcentage)) +
  geom_bar(stat = "identity", fill = "darkorange") +
  geom_text(aes(label = paste0(pourcentage, "%")), vjust = -0.5) +
  theme_minimal() +
  labs(title = "Top 10 des produits les plus consommés (%)",
       x = "Code produit", y = "Pourcentage (%)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("sorties/Sorties_preambule/top10_produits_cons.png", plot = top10_cons_graph, width = 10, height = 6, dpi = 300)


# --- Superficie agricole, tous produits -----------------------------------
# Surface cultivée par ménage, en hectares
s16a_sup <- s16a %>%
  mutate(sup_hectare = ifelse(s16aq09b == 2,
                              s16aq09a / 10000,
                              s16aq09a)) %>%
  group_by(hhid) %>%
  summarise(champ = sum(sup_hectare, na.rm = TRUE))

# Poids de sondage, un par ménage
poids_uniques <- s16c %>%
  distinct(hhid, hhweight)

s16a_sup_pondere <- s16a_sup %>%
  left_join(poids_uniques, by = "hhid")

total_pondere <- sum(s16a_sup_pondere$champ * s16a_sup_pondere$hhweight, na.rm = TRUE)

# Part de chaque produit dans la superficie agricole totale
s16_a_c <- s16c %>%
  left_join(s16a_sup, by = "hhid") %>%
  mutate(produit = as_factor(s16cq04)) %>%
  group_by(produit) %>%
  summarise(
    poids = round(sum(champ * s16cq08 * hhweight, na.rm = TRUE) / total_pondere, 2)
  ) %>%
  arrange(desc(poids))

top10_cul <- s16_a_c %>%
  slice_head(n = 10)

top10_cul_graph <- ggplot(top10_cul, aes(x = reorder(produit, -poids), y = poids)) +
  geom_bar(stat = "identity", fill = "forestgreen") +
  geom_text(aes(label = paste0(poids, "%")), vjust = -0.5) +
  theme_minimal() +
  labs(title = "Part de chaque produit dans la superficie totale récoltée (pondérée)",
       x = "Produit", y = "Part (%)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("sorties/Sorties_preambule/top10_produits_cultives.png", plot = top10_cul_graph, width = 10, height = 6, dpi = 300)


# --- Ventes agricoles, tous produits --------------------------------------
total_vente <- sum(s16d$s16dq06 * s16d$hhweight, na.rm = TRUE)

ventes <- s16d %>%
  mutate(produit = as_factor(s16dq01)) %>%
  group_by(produit) %>%
  summarise(
    part_vente = sum(s16dq06 * hhweight, na.rm = TRUE) / total_vente
  )

top10_ventes <- ventes %>%
  slice_head(n = 10)

top10_ventes_graph <- ggplot(top10_ventes, aes(x = reorder(produit, -part_vente), y = part_vente * 100)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = paste0(round(part_vente * 100, 1), "%")), vjust = -0.5) +
  theme_minimal() +
  labs(title = "Top 10 des produits par part dans les ventes",
       x = "Produit", y = "Part des ventes (%)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("sorties/Sorties_preambule/top10_produits_vendus.png", plot = top10_ventes_graph, width = 10, height = 6, dpi = 300)


# --- Taux de commercialisation, tous produits -----------------------------
producteurs <- s16c %>%
  mutate(produit = as_factor(s16cq04)) %>%
  distinct(hhid, produit, hhweight)

vendeurs <- s16d %>%
  mutate(produit = as_factor(s16dq01)) %>%
  filter(s16dq06 > 0) %>%
  distinct(hhid, produit)

taux_commercialisation <- producteurs %>%
  left_join(vendeurs %>% mutate(a_vendu = TRUE), by = c("hhid", "produit")) %>%
  mutate(a_vendu = ifelse(is.na(a_vendu), FALSE, TRUE)) %>%
  group_by(produit) %>%
  summarise(
    taux_commercialisation = round(
      100 * sum(a_vendu * hhweight, na.rm = TRUE) / sum(hhweight, na.rm = TRUE), 1)
  ) %>%
  arrange(desc(taux_commercialisation))

top10_comm <- taux_commercialisation %>%
  slice_head(n = 10)

top10_comm_graph <- ggplot(top10_comm, aes(x = reorder(produit, -taux_commercialisation), y = taux_commercialisation)) +
  geom_bar(stat = "identity", fill = "purple") +
  geom_text(aes(label = paste0(taux_commercialisation, "%")), vjust = -0.5) +
  theme_minimal() +
  labs(title = "Taux de commercialisation par produit",
       x = "Produit", y = "% de producteurs qui vendent") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("sorties/Sorties_preambule/taux_commercialisation.png", plot = top10_comm_graph, width = 10, height = 6, dpi = 300)


# --- Conversion en kg des quantités consommées (S16D) ---------------------
# Table de conversion phase 2, feuille nationale
table_conversion_16d <- read_excel("donnee/Table de conversion phase 2.xlsx", sheet = "nationale") %>%
  filter(!is.na(poids)) %>%
  mutate(poids = as.numeric(gsub(",", ".", gsub(";", ".", gsub(" ", "", poids))))) %>%
  filter(!is.na(poids))

# Labels des codes 1 à 7 de s16dq02b
labels_unite_16d <- s16d %>%
  mutate(label_unite = as_factor(s16dq02b)) %>%
  distinct(s16dq02b, label_unite) %>%
  arrange(s16dq02b) %>%
  rename(code_unite = s16dq02b)

print(labels_unite_16d)
# -> 1 Kilogramme | 2 Unité | 3 Yorouba | 4 Tine | 5 Sac moyen | 6 Sac gros | 7 Autres

# Correspondance label -> uniteID (+ tailleNom pour le Sac)
# Yorouba, Tine, Autres absents de la table -> poids = NA
correspondance_unite <- tibble(
  label_unite    = c("Kilogramme", "Unité", "Yorouba", "Tine", "Sac moyen", "Sac gros", "Autres"),
  uniteID_conv   = c(100,           147,     NA,        NA,     133,         133,        NA),
  tailleNom_conv = c(NA,            NA,      NA,        NA,     "Moyen",     "Grand",    NA)
)

# Poids moyen par unité/taille (approximation, poids variable selon le produit)
poids_par_unite <- table_conversion_16d %>%
  mutate(tailleNom_std = str_to_title(trimws(tailleNom))) %>%
  filter(uniteID %in% na.omit(correspondance_unite$uniteID_conv)) %>%
  filter(uniteID != 133 | tailleNom_std %in% c("Moyen", "Grand")) %>%
  group_by(uniteID, tailleNom_std) %>%
  summarise(poids_g = mean(poids, na.rm = TRUE), .groups = "drop")

# Table finale label -> poids en g/kg
correspondance_unite_poids <- correspondance_unite %>%
  left_join(poids_par_unite,
            by = c("uniteID_conv" = "uniteID", "tailleNom_conv" = "tailleNom_std")) %>%
  mutate(poids_g = ifelse(label_unite == "Kilogramme", 1000, poids_g),
         poids_kg = poids_g / 1000)

print(correspondance_unite_poids)

# Application à S16D
s16d_kg <- s16d %>%
  mutate(produit = as_factor(s16dq01),
         label_unite = as_factor(s16dq02b)) %>%
  left_join(correspondance_unite_poids %>% select(label_unite, poids_g, poids_kg),
            by = "label_unite") %>%
  mutate(q_cons_kg = s16dq02a * poids_kg)

glimpse(s16d_kg)

# Part de chaque produit dans la consommation totale, en kg
total_cons_kg <- sum(s16d_kg$q_cons_kg * s16d_kg$hhweight, na.rm = TRUE)

cons_cul_kg <- s16d_kg %>%
  group_by(produit) %>%
  summarise(
    part_consommation_kg = round(100 * sum(q_cons_kg * hhweight, na.rm = TRUE) / total_cons_kg, 2)
  ) %>%
  arrange(desc(part_consommation_kg))

top10_cons_cul_kg <- cons_cul_kg %>%
  slice_head(n = 10)

top10_cons_cul_kg_graph <- ggplot(top10_cons_cul_kg, aes(x = reorder(produit, -part_consommation_kg), y = part_consommation_kg)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  geom_text(aes(label = paste0(part_consommation_kg, "%")), vjust = -0.5) +
  theme_minimal() +
  labs(title = "Top 10 des produits par part dans la consommation (en kg)",
       x = "Produit", y = "Part de la consommation (%)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("sorties/Sorties_preambule/top10_produits_consommes_kg.png", plot = top10_cons_cul_kg_graph, width = 10, height = 6, dpi = 300)


# --- Importations / exportations, tous produits (FAOSTAT 2021) -----------
faostat <- read_csv("donnee/FAOSTAT_data_import_export_crops.csv", show_col_types = FALSE)

# Ne garder que les chiffres officiels
faostat <- faostat %>% filter(Flag == "A")

# Part en quantité (tonnes), têtes d'animaux vivants exclues
import_qty <- faostat %>%
  filter(Element == "Import quantity", Unit == "t") %>%
  group_by(Item) %>%
  summarise(qte = sum(Value, na.rm = TRUE)) %>%
  mutate(part_import_qty = round(100 * qte / sum(qte, na.rm = TRUE), 2)) %>%
  arrange(desc(part_import_qty))

export_qty <- faostat %>%
  filter(Element == "Export quantity", Unit == "t") %>%
  group_by(Item) %>%
  summarise(qte = sum(Value, na.rm = TRUE)) %>%
  mutate(part_export_qty = round(100 * qte / sum(qte, na.rm = TRUE), 2)) %>%
  arrange(desc(part_export_qty))

# Part en valeur (1000 USD)
import_val <- faostat %>%
  filter(Element == "Import value", Unit == '1000 USD') %>%
  group_by(Item) %>%
  summarise(val = sum(Value, na.rm = TRUE)) %>%
  mutate(part_import_val = round(100 * val / sum(val, na.rm = TRUE), 2)) %>%
  arrange(desc(part_import_val))

export_val <- faostat %>%
  filter(Element == "Export value") %>%
  group_by(Item) %>%
  summarise(val = sum(Value, na.rm = TRUE)) %>%
  mutate(part_export_val = round(100 * val / sum(val, na.rm = TRUE), 2)) %>%
  arrange(desc(part_export_val))

# Top 10 + graphiques
top10_import_qty <- import_qty %>% slice_head(n = 10)
top10_export_qty <- export_qty %>% slice_head(n = 10)
top10_import_val <- import_val %>%
  slice_head(n = 10) %>%
  mutate(Item_short = str_trunc(Item, 25))
top10_export_val <- export_val %>% slice_head(n = 10)

top10_import_qty_graph <- ggplot(top10_import_qty,
                                 aes(x = reorder(Item, -part_import_qty), y = part_import_qty)) +
  geom_bar(stat = "identity", fill = "darkorange") +
  geom_text(aes(label = paste0(part_import_qty, "%")), vjust = -0.5) +
  theme_minimal() +
  labs(title = "Top 10 des produits par part dans les importations (quantité, tonnes)",
       x = "Produit", y = "Part des importations (%)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("sorties/Sorties_preambule/top10_importations_quantite.png", plot = top10_import_qty_graph,
       width = 10, height = 6, dpi = 300)

top10_export_qty_graph <- ggplot(top10_export_qty,
                                 aes(x = reorder(Item, -part_export_qty), y = part_export_qty)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = paste0(part_export_qty, "%")), vjust = -0.5) +
  theme_minimal() +
  labs(title = "Top 10 des produits par part dans les exportations (quantité, tonnes)",
       x = "Produit", y = "Part des exportations (%)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("sorties/Sorties_preambule/top10_exportations_quantite.png", plot = top10_export_qty_graph,
       width = 10, height = 6, dpi = 300)

top10_import_val_graph <- ggplot(top10_import_val,
                                 aes(x = reorder(Item_short, -part_import_val), y = part_import_val)) +
  geom_bar(stat = "identity", fill = "darkred") +
  geom_text(aes(label = paste0(part_import_val, "%")), vjust = -0.5) +
  theme_minimal() +
  labs(title = "Top 10 des produits par part dans les importations (valeur, 1000 USD)",
       x = "Produit", y = "Part des importations (%)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("sorties/Sorties_preambule/top10_importations_valeur.png", plot = top10_import_val_graph,
       width = 12, height = 8, dpi = 300)

top10_export_val_graph <- ggplot(top10_export_val,
                                 aes(x = reorder(Item, -part_export_val), y = part_export_val)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  geom_text(aes(label = paste0(part_export_val, "%")), vjust = -0.5) +
  theme_minimal() +
  labs(title = "Top 10 des produits par part dans les exportations (valeur, 1000 USD)",
       x = "Produit", y = "Part des exportations (%)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("sorties/Sorties_preambule/top10_exportations_valeur.png", plot = top10_export_val_graph,
       width = 10, height = 6, dpi = 300)
