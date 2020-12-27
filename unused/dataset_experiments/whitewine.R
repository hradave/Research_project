whitewine = read.csv2('data\\winequality-white.csv')

plot(whitewine$alcohol, whitewine$quality, pch = 20)
