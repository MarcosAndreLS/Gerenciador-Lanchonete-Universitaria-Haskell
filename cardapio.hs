-- Arquivo Cardapio.hs
module Cardapio where

-- Função para exibir o cardápio principal, que contém todas as categorias de itens disponíveis
verCardapio :: IO ()
verCardapio = do
    putStrLn "\n|=-=-=-=-=-=-=-=-=-=-=-=-=-=-=CARDÁPIO=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=|"
    putStrLn "|-----------COMIDAS----------------|-----------BEBIDAS---------------------|"
    putStrLn "|1-Hambúrguer ........12,00 a 15,00| 7-Refrigerante ........8,00           |"
    putStrLn "|2-Pizza .............40,00 a 60,00| 8-Água mineral ........2,00           |"
    putStrLn "|3-Coxinha ...........3,00 a 4,00  | 9-Água de coco ........4,00           |"
    putStrLn "|4-Bomba .............3,00         | 10-Cerveja ............4,00 a 16,00   |"
    putStrLn "|5-Pastel ............3,00 a 4,00  |                                       |"
    putStrLn "|6-Salsichão .........3,00         |                                       |"
    putStrLn "|=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=|"

    return ()

-- Função para exibir os tipos de hambúrgueres disponíveis no cardápio
verHamburgueres :: IO ()
verHamburgueres = do
    putStrLn "\n1-X_Bacon.........12,00"
    putStrLn "2-X_Salada........13,00"
    putStrLn "3-X_Delicia.......14,00"
    putStrLn "4-X_Calabresa.....15,00"

-- Função para exibir os tipos de pizzas disponíveis no cardápio
verPizzas :: IO ()
verPizzas = do
    putStrLn "\n1-Mexicana.........50,00"
    putStrLn "2-Frango...........40,00"
    putStrLn "3-Calabresa........45,00"
    putStrLn "4-Nordestina.......60,00"

-- Função para exibir os tipos de coxinhas disponíveis no cardápio
verCoxinhas :: IO ()
verCoxinhas = do
    putStrLn "\n1-Frango....................3,00"
    putStrLn "2-Frango com Cheddar........4,00"
    putStrLn "3-Frango com catupiry.......4,00"
    putStrLn "4-Carne.....................3,00"

-- Função para exibir os tipos de pastéis disponíveis no cardápio
verPasteis :: IO ()
verPasteis = do
    putStrLn "\n1-Frango....................3,00"
    putStrLn "2-Frango de queijo..........5,00"
    putStrLn "3-Frango com catupiry.......4,00"
    putStrLn "4-Carne.....................3,00"

-- Função para exibir as cervejas disponíveis no cardápio, incluindo preços e tamanhos
verCervejas :: IO ()
verCervejas = do
    putStrLn "\n1-Heineken long neck...........................6,00"
    putStrLn "2-Heineken 600ml..............................16,00"
    putStrLn "3-Skol lonk neck...............................4,00"
    putStrLn "4-Skol 600ml...................................8,00"
    putStrLn "5-Budweiser long neck..........................5,00"
    putStrLn "6-Crystal 269ml................................2,10"
    putStrLn "7-Itaipava 1L..................................9,00"
    putStrLn "8-Berrió 350ml.................................3,00"

-- Função para exibir os refrigerantes disponíveis no cardápio
verRefrigerantes :: IO ()
verRefrigerantes = do
    putStrLn "\n1-Coca cola.................8,00"
    putStrLn "2-Guaraná...................8,00"
    putStrLn "3-Fanta laranja.............8,00"
    putStrLn "4-Fanta uva.................8,00"
