library(ggplot2)

peso = c(45,50,60,55,58,56,48,53)
altura = c(1.54, 1.56, 1.65, 1.60, 1.65, 1.63, 1.58, 1.59)

#grafico
plot (peso, altura)

#A função lm fornece o ajuste de um modelo de regressão linear de primeira ordem
lm (altura ~ peso)

# colocando a linha no gráfico
abline(lm(altura ~ peso))

# Melhorando o gráfico
ggplot(mapping = aes(peso, altura)) +  geom_point() + geom_smooth(method = "lm")

#tirar o delimitador cinza e colocar a linha da média
retas <- ggplot(mapping = aes(peso, altura)) +  geom_point() + geom_smooth(se = FALSE, method = "lm") +  geom_hline(yintercept = mean(altura))
retas

#medir distancias dos pontos para a média
retas + geom_segment(aes(x=peso, y=altura, xend = peso, yend = mean(altura)), color = "red")

#Calculando a distância entre Regressão e o valor real
retas + geom_segment(aes(x=peso, y=altura,  xend = peso, yend = predict (lm(altura~peso))),  color = "red")

#calculo do r2
sqt = sum( (mean(altura)-altura)**2)
sqres = sum((predict(lm(altura~peso))-altura)**2)
r2 = (sqt-sqres)/sqt
r2 #0.9009349

#criação de uma variavel modelo para armazenar a regressão
modelo <- lm(altura~peso)
predict(modelo)

altura

#testar o modelo com outros pesos
pesos = data.frame(peso = c(48, 52, 62))
pesos

predict(modelo, pesos)



