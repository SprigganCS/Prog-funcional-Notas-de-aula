--fatorial
fat :: Int->Int
fat 0 = 1
fat x = x * fat(x-1)

-- define o período de recursão
periodo::Int
periodo = 7

--tabela de vendas
vendas ::Int->Int
vendas 0=0
vendas 1=41
vendas 2=72
vendas 3=48
vendas 4=0
vendas 5=91
vendas 6=55
vendas 7=30

diaVenda::Int->Int->Int
--diaVenda 0 _ = 0
diaVenda d v
 |vendas d == v = d
 |otherwise = diaVenda (d-1) v

diaVendaS::Int->Int
diaVendaS v = diaVenda periodo v

--funcões que retornam total de vendas

totalVenda::Int->Int->Int
totalVenda t 0 = t
totalVenda t d = totalVenda(t + vendas d) (d-1)

totalVendas::Int->Int
totalVendas 0 = vendas 0
totalVendas d = vendas d + totalVendas(d-1)

--função que retorno quantas vendas superam um valor
superar :: Int -> Int -> Int -> Int

superar 0 _ v = v

superar d f v
 | vendas d > f = superar (d-1) f (v+1)
 | vendas d <= f = superar (d-1) f v


superarInterface :: Int -> Int
superarInterface f = superar 7 f 0


func1 :: Double -> Double --1a da lista
func1 a
  |a>=0 = (a+4)/(a+2)
  |a<0 = (2/a)
