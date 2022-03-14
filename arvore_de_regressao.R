# Árvore com resposta contínua


# Gerando os dados --------------------------------------------------------
x <- seq(0, 1, length.out = 1000)

# Criando uma função 'Y' que possui relação quadrática
a <- 0
b <- 10
c <- -10

y <- a + b*x + c*x**2 + rnorm(length(x), mean = 0, sd = .1)

df <- data.frame(x, y)
plot_1 <- df %>% 
  ggplot(aes(x, y)) +
  geom_point(aes(colour = 'Observado')) +
  scale_colour_viridis_d(begin = 0, end = .85, name = 'Valor') +
  theme(legend.position = 'bottom', 
        legend.spacing.x = unit(0, 'cm'))+
  theme_bw()

plot_1


# Construindo a arvore ----------------------------------------------------
tree <- rpart(y~x, data = df, control = rpart.control(maxdepth = 3,
                                                      cp = 0))
class(tree)

# Plotando a arvore
paleta <- viridis_pal(begin = .75, end = 1)(20)
rpart.plot(tree, box.palette = paleta)

# Valores preditos
df['p'] <- predict(tree, df)
df['r'] <- df$y - df$p

# Plotando os valores esperados e observados
boost0_o_vs_E <- df %>% 
  ggplot(aes(x,y)) +
  geom_point(aes(colour = 'Observado'), alpha = .7, size = 1.5) +
  geom_path(aes(x, p, colour = 'Esperado')) +
  scale_color_viridis_d(begin = 0, end = .8, name = 'Dado:') +
  theme_bw() +
  theme(legend.position = 'bottom') +
  labs(title = 'Valores observados vs esperados') +
  scale_y_continuous(name = 'y') +
  scale_x_continuous(name = 'x')

boost0_o_vs_E

# Gráfico de resíduos

boost0_res <- df %>% 
  ggplot(aes(x,r)) +
  geom_point(aes(colour = 'Resíduo'), alpha = .7, size = 1.5) +
  scale_color_viridis_d(begin = 0, end = .8, name = 'Dado:') +
  theme_bw() +
  theme(legend.position = 'bottom') +
  labs(title = 'Resíduo') +
  scale_y_continuous(name = 'r') +
  scale_x_continuous(name = 'x')

boost0_res

ggarrange(boost0_o_vs_E, boost0_res, ncol = 2, nrow = 1)
