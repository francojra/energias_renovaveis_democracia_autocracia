
# Energias renováveis em países democratas e autocratas ------------------------------------------------------------------------------------
# Autoria do script: Jeanne Franco ---------------------------------------------------------------------------------------------------------
# Data: 08/11/22 ---------------------------------------------------------------------------------------------------------------------------
# Referência: https://ourworldindata.org/renewable-energy ----------------------------------------------------------------------------------

# Sobre os dados ---------------------------------------------------------------------------------------------------------------------------

### Desde a Revolução Industrial, a combinação de energia da maioria dos países em volta
### do mundo tem se tornado dominada por combustíveis fósseis. Isso tem grandes implicações 
### para mudanças climáticas, como bem para a saúde humana. Três-quartos das emissões de gases
### de efeito estufa resulta da queima de combustíveis fósseis para energia. Combustíveis 
### fósseis são responsáveis por grande quantidade de poluição do ar local - um problema de
### saúde que leva a no mínimo 5 milhões de mortes prematuras a cada ano.

### Para reduzir as emissões de CO2 e poluição do ar local, o mundo necessita rapidamente mudar
### fontes energéticas de baixo-carbono - como tecnologias renováveis e nuclear.

### Energias renováveis tem um papel chave na descarbonização do nosso sistema de energia nas
### próximas décadas. Mas como rapidamente nossa produção de energias renováveis está mudando?
### Quais tecnologias parecem mais promissoras em transformar nossa energia?

### Nesse artigo nós observamos os dados sobre tecnologias renováveis no mundo. Qual porcentagem
### de energia que eles contam hoje e como isso está mudando.

# Carregar pacotes -------------------------------------------------------------------------------------------------------------------------

library(tidyverse)
library(cols4all)
library(hrbrthemes)
library(ggthemes)

# Carregar dados ---------------------------------------------------------------------------------------------------------------------------

energ <- read.csv("renewable-share-energy.csv")
view(energ)
names(energ)

# Manipular dados --------------------------------------------------------------------------------------------------------------------------

energ <- energ %>%
  select(-Code) %>%
  rename(por_energ = Renewables....equivalent.primary.energy.) %>%
  view()

energ1 <- energ %>%
  filter(Entity %in% c("United States", "Japan", "Germany",
                       "Cuba", "China", "North Korea")) %>%
  group_by(Entity) %>%
  summarise(media = mean(por_energ),
            sd = sd(por_energ), n = n(),
            se = sd/sqrt(n)) %>%
  view()

energ2 <- energ %>%
  filter(Entity %in% c("United States", "Japan", "Germany",
                       "Cuba", "China", "North Korea")) %>%
  view()

energ3 <- energ %>%
  filter(Entity %in% c("United States", "China", "Brazil")) %>%
  view()  

# Gráficos ---------------------------------------------------------------------------------------------------------------------------------

ggplot(energ1, aes(x = fct_reorder(Entity, media), 
                   y = media, fill = Entity)) +
  geom_col(width = 0.9) +
  geom_errorbar(aes(ymin = media - se, ymax = media + se),
                width = 0.2, size = 0.8) +
  scale_fill_manual(values = c("#88CCEE", "#CC6677",
                               "#DDCC77", "#117733",
                               "#332288", "#AA4499")) +
  scale_y_continuous(expand = expansion(mult = c(0,0))) +
  scale_x_discrete(labels = c("Alemanha", "Estados Unidos",
                              "China", "Japão")) +
  labs(x = "Países", y = "Energias renováveis (%)") +
  theme_ipsum(axis_text_size = 14, axis_title_size = 16) +
  theme(legend.position = "none",
        axis.text = element_text(color = "black"))

ggplot(energ2, aes(x = Year, y = por_energ,
                   group = Entity, color = Entity)) +
  geom_point(shape = 15, size = 2.5) +
  geom_line(size = 1.2) +
  scale_color_manual(values = c("#88CCEE", "#CC6677",
                               "#DDCC77", "#117733",
                               "#332288", "#AA4499"),
                     labels = c("China", "Alemanha",
                                "Japão", "Estados Unidos")) +
  labs(x = "Tempo (anos)", y = "Energias renováveis (%)",
       color = "Países") +
  theme_ipsum(axis_text_size = 14, axis_title_size = 16) +
  theme(axis.text = element_text(color = "black"))

ggplot(energ3, aes(x = Year, y = por_energ,
                   group = Entity, color = Entity)) +
  geom_line(size = 2) +
  scale_color_manual(values = c('#1B9E77', '#999999','#E69F00'),
                     labels = c("Brasil", "China", "Estados Unidos")) +
  labs(x = "Tempo (anos)", y = "Energias renováveis (%)",
       color = "Países") +
  theme_light() +
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(color = "black", size = 15),
        legend.text = element_text(size = 12))




