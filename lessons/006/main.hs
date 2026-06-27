import Distribution.Simple.Utils (xargs)
{--
O type apenas cria um alias para o tipo, entao isso nao garante que um outro type qualquer que possui a mesma estrutura seja substituido
Ou seja, ou type, embora o nome possa enganar, nao eh um tipo novo, eh apenas um apelido
--}

type Produto = (Int, String, Double)

type Cliente = (Int, String, Double)

preco :: (a, b, c) -> c
preco (_, _, p) = p

pago = preco

troco :: Produto -> Cliente -> Double
troco produto cliente = pago cliente - preco produto

{--
Tipos parametricos
Sao tipos que aceitam tipos como parametro
--}
type Assoc k v = [(k, v)]

find' :: (Eq k) => k -> Assoc k v -> v
find' k assoc = head [v | (k', v) <- assoc, k' == k]

{-- 
Agora vamos falar de tipos pra valer
ou seja, sao realmente tipos novos e que o compilador vai tratar como um novo tipo, e nao apenas como um apelido

O primeiro eh o tipo SOma
ele tem esse nome pose eh a soma dos dois dominios
--}
data Dir = Norte | Sul | Leste | Oeste
type Coordenada = (Int, Int)
type Passo = Coordenada -> Coordenada
type Caminho = [Dir]

para :: Dir -> Passo
para Norte (x, y) = (x, y + 1)
para Sul (x, y) = (x, y - 1) 
para Leste (x, y) = (x + 1, y) 
para Oeste (x, y) = (x - 1, y) 

caminhar:: Caminho -> Passo
caminhar [] x = x
caminhar (d:ds)  coord = caminhar ds $ para d coord

{--
Outro tipo 'e o Produto AxB
--}
data Ponto = MkPonto Double Double

dist :: Ponto -> Ponto -> Double
dist (MkPonto x1 y1) (MkPonto x2 y2) = sqrt $ (x2 - x1) ^ 2 + (y2 - y1) ^ 2
