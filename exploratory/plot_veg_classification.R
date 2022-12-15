library(tidyterra)

frank_rgb_cropped_150 <- rast(here("data/processed/cropped_raster_150m_rad.tif"))

lc_class_manual_raster <- rast(here("data/processed/lc_class_manual_raster.tif"))

NAflag(lc_class_manual_raster) <- 4
levels(lc_class_manual_raster) <- c("barren", "bulrush", "hordeum", "island")

plot(lc_class_manual_raster)

ml_class_island <- rast(here("data/processed/ml_class_island.tif")) %>%
  mask(ft_150) %>%
  crop(ft_150)

plot(ml_class_island)

ml_class_no_island <- rast(here("data/processed/ml_class_no_island.tif")) %>%
  mask(ft_150) %>%
  crop(ft_150)

plot(ml_class_no_island)

RColorBrewer::brewer.pal(4, "Set1")

p0 <- ggplot() +
  geom_spatraster_rgb(data = frank_rgb_cropped_150) +
  geom_spatvector(data = wedge_sections, fill = NA) +
  ggtitle("RGB") +
  theme_classic() +
  theme(legend.position = "none")

p1 <- ggplot() +
  geom_spatraster(data = lc_class_manual_raster) +
  geom_spatvector(data = wedge_sections, fill = NA) +
  scale_fill_manual(labels = c("Barren", "Bulrush", "Hordeum", "Island"),
                    values = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3"),
                    na.value = NA) +
  ggtitle("Manual delineation") +
  theme_classic() +
  theme(legend.position = "none")

p2 <- ggplot() +
  geom_spatraster(data = ml_class_island) +
  geom_spatvector(data = wedge_sections, fill = NA) +
  scale_fill_manual(labels = c("Barren", "Bulrush", "Hordeum", "Island"),
                    values = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3"),
                    na.value = NA) +
  ggtitle("CART - island included") +
  theme_classic() +
  theme(legend.position = "none")

p3 <- ggplot() +
  geom_spatraster(data = ml_class_no_island) +
  geom_spatvector(data = wedge_sections, fill = NA) +
  scale_fill_manual(labels = c("Barren", "Bulrush", "Hordeum"),
                    values = c("#E41A1C", "#377EB8", "#4DAF4A"),
                    na.value = NA) +
  ggtitle("CART - no island") +
  theme_classic() +
  theme(legend.position = "none")

library(patchwork)

(p0 |p1) / (p2 |p3)
