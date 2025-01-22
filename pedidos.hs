-- Arquivo pedidos.hs
module Pedidos where

import Cardapio
import System.IO
import Data.List (intercalate)
import Data.IORef
import System.IO.Unsafe (unsafePerformIO) -- Para inicializar a variável global 

-- Tipo para representar um pedido
data Pedido = Pedido {
    pedidoId :: Int,               -- ID único do pedido
    comidas :: [(String, Double)], -- Lista de itens do pedido (nome e preço)
    status :: Int,                 -- Status do pedido (1 = pendente, 0 = entregue)
    valorTotal :: Double           -- Valor total do pedido
} deriving (Show)

-- Lista global para armazenar os pedidos
pedidos :: IORef [Pedido]
pedidos = unsafePerformIO $ newIORef []

-- Gera um novo ID para o pedido com base na quantidade de pedidos existentes
-- currying
gerarIdPedido :: IO Int
gerarIdPedido = do
    lista <- readIORef pedidos
    return $ length lista + 1

-- Adiciona um novo pedido à lista global de pedidos
-- currying
adicionarPedido :: Pedido -> IO ()
adicionarPedido novoPedido = do
    lista <- readIORef pedidos
    writeIORef pedidos (lista ++ [novoPedido])

-- Gera um resumo textual dos itens de um pedido e seu valor total
-- list comprehensions
resumoPedido :: [(String, Double)] -> String
resumoPedido pedidos = 
    unlines [nome ++ " - R$ " ++ show preco | (nome, preco) <- pedidos] ++
    "\nTotal: R$ " ++ show (sum [preco | (_, preco) <- pedidos])


-- Placeholder para funcionalidades específicas do cliente

-- Inicia o processo de fazer um pedido, coletando itens do cardápio
fazerPedido :: IO ()
fazerPedido = do
    putStrLn "\nMuito bem, vamos fazer o seu pedido!"
    putStrLn "\nDigite o número dos itens que deseja pedir, ou 0 para finalizar:"
    coletarItens [] 0

-- Coleta itens escolhidos pelo cliente e atualiza a lista e o total acumulado
coletarItens :: [(String, Double)] -> Double -> IO ()
coletarItens itens total = do
    verCardapio
    putStrLn "11 - finalizar"
    putStrLn "12 - ver itens que já foram adicionados e preço"
    putStrLn "\nDigite o número do item:"
    opcao <- getLine
    case opcao of
        "1" -> do
            verHamburgueres
            putStrLn "Escolha o sabor do Hambúrguer:"
            sabor <- getLine
            case sabor of
                "1" -> registrarItem ("X-Bacon", 12.0)
                "2" -> registrarItem ("X-Salada", 13.0)
                "3" -> registrarItem ("X-Delícia", 14.0)
                "4" -> registrarItem ("X-Calabresa", 15.0)
                _ -> do
                    putStrLn "Opção inválida. Por favor, tente novamente."
                    coletarItens itens total
        "2" -> do
            putStrLn "Escolha o sabor da Pizza:"
            verPizzas
            sabor <- getLine
            case sabor of
                "1" -> registrarItem ("Pizza Mexicana", 50.0)
                "2" -> registrarItem ("Pizza Frango", 40.0)
                "3" -> registrarItem ("Pizza Calabresa", 45.0)
                "4" -> registrarItem ("Pizza Nordestina", 60.0)
                _ -> do
                    putStrLn "Opção inválida. Por favor, tente novamente."
                    coletarItens itens total
        "3" -> do
            putStrLn "Escolha o sabor da Coxinha:"
            verCoxinhas
            sabor <- getLine
            case sabor of
                "1" -> registrarItem ("Coxinha Frango", 3.0)
                "2" -> registrarItem ("Coxinha Frango com Cheddar", 4.0)
                "3" -> registrarItem ("Coxinha Frango com Catupiry", 4.0)
                "4" -> registrarItem ("Coxinha Carne", 3.0)
                _ -> do
                    putStrLn "Opção inválida. Por favor, tente novamente."
                    coletarItens itens total
        "4" -> registrarItem ("Bomba", 3.0)
        "5" -> do
            putStrLn "Escolha o sabor do Pastel:"
            verPasteis
            sabor <- getLine
            case sabor of
                "1" -> registrarItem ("Pastel Frango", 3.0)
                "2" -> registrarItem ("Pastel Frango de Queijo", 5.0)
                "3" -> registrarItem ("Pastel Frango com Catupiry", 4.0)
                "4" -> registrarItem ("Pastel Carne", 3.0)
                _ -> do
                    putStrLn "Opção inválida. Por favor, tente novamente."
                    coletarItens itens total
        "6" -> registrarItem ("Salsichão", 3.0)
        "7" -> registrarItem ("Refrigerante", 8.0)
        "8" -> registrarItem ("Água Mineral", 2.0)
        "9" -> registrarItem ("Água de Coco", 4.0)
        "10" -> do
            putStrLn "Escolha o tipo de Cerveja:"
            verCervejas
            sabor <- getLine
            case sabor of
                "1" -> registrarItem ("Heineken Long Neck", 6.0)
                "2" -> registrarItem ("Heineken 600ml", 16.0)
                "3" -> registrarItem ("Skol Long Neck", 4.0)
                "4" -> registrarItem ("Skol 600ml", 8.0)
                "5" -> registrarItem ("Budweiser Long Neck", 5.0)
                "6" -> registrarItem ("Crystal 269ml", 2.1)
                "7" -> registrarItem ("Itaipava 1L", 9.0)
                "8" -> registrarItem ("Berrió 350ml", 3.0)
                _ -> do
                    putStrLn "Opção inválida. Por favor, tente novamente."
                    coletarItens itens total
        "11" -> finalizarPedido itens
        "12" -> do
            putStrLn "\nItens adicionados até agora:"
            -- Exibir cada item com seu preço
            mapM_ (\(nome, preco) -> putStrLn $ nome ++ " - R$ " ++ show preco) itens
            -- Exibir o total acumulado
            putStrLn $ "\nTotal até agora: R$ " ++ show total
            coletarItens itens total
        _ -> do
            putStrLn "Opção inválida. Por favor, tente novamente."
            coletarItens itens total
  where
    registrarItem (nome, preco) = do
        let novosItens = itens ++ [(nome, preco)]
        let novoTotal = total + preco
        coletarItens novosItens novoTotal

-- Finaliza o pedido, gerando um ID e adicionando-o à lista de pedidos
finalizarPedido :: [(String, Double)] -> IO ()
finalizarPedido itens = do
    if null itens
        then putStrLn "\nVocê não selecionou nenhum item. Pedido não realizado."
        else do
            idPedido <- gerarIdPedido
            let novoPedido = Pedido idPedido itens 1 (sum (map snd itens))  -- Passa o total calculado
            adicionarPedido novoPedido
            putStrLn "\nPedido realizado com sucesso!"
            putStrLn $ "Resumo do pedido:\n" ++ resumoPedido itens

-- Visualiza detalhes de um pedido específico pelo ID
verPedidoID :: IO ()
verPedidoID = do
    putStrLn "\nDigite o ID do seu pedido para visualizar:"
    pedidoIdInput <- getLine
    let pedidoIdBusca = read pedidoIdInput :: Int  -- Converte o input para um inteiro
    lista <- readIORef pedidos
    let pedidoEncontrado = filter (\p -> pedidoId p == pedidoIdBusca) lista
    if null pedidoEncontrado
        then putStrLn "\nPedido não encontrado."
        else do
            putStrLn "\nPedido Encontrado:"
            mapM_ (\(Pedido pid itens status total) -> do
                putStrLn $ "Pedido ID: " ++ show pid
                putStrLn $ resumoPedido itens
                putStrLn $ "Status: " ++ if status == 1 
                    then "Pedido pendente." 
                    else "Pedido está sendo entregue."
                putStrLn "---------------------------") pedidoEncontrado

-- Exclui um pedido com base no ID, se ele ainda estiver pendente
excluirPedido :: IO ()
excluirPedido = do
    putStrLn "\nDigite o ID do pedido que deseja excluir:"
    inputId <- getLine
    let idProcurado = read inputId :: Int
    lista <- readIORef pedidos
    let pedidoEncontrado = filter (\p -> pedidoId p == idProcurado) lista
    if null pedidoEncontrado
        then putStrLn "\nPedido não encontrado."
        else do
            let pedido = head pedidoEncontrado
            if status pedido /= 1
                then putStrLn "\nApenas pedidos com status '1' (pendentes) podem ser excluídos."
                else do
                    let resto = filter (\p -> pedidoId p /= idProcurado) lista
                    writeIORef pedidos resto
                    putStrLn "\nPedido excluído com sucesso!"
                    putStrLn $ "Resumo do pedido excluído:\n" ++ resumoPedido (comidas pedido)

-- Placeholder para funcionalidades específicas do administrador

-- Lista todos os pedidos filtrados por status (pendente ou entregue)
listarPedidosPorStatus :: Int -> IO ()
listarPedidosPorStatus statusProcurado = do
    lista <- readIORef pedidos
    let filtrados = filter (\p -> status p == statusProcurado) lista
    if null filtrados
        then putStrLn "\nNão há pedidos no momento."
        else do
            let nomeStatus = if statusProcurado == 1 then "Pendentes" else "Entregues"
            putStrLn $ "\nPedidos " ++ nomeStatus
            mapM_ (\pedido -> do
                putStrLn $ "Pedido ID: " ++ show (pedidoId pedido)
                putStrLn $ resumoPedido (comidas pedido)
                putStrLn "---------------------------") filtrados

-- Atualiza o status de um pedido para "entregue" (status = 0)
mudarStatusPedido :: IO ()
mudarStatusPedido = do
    putStrLn "\nDigite o ID do pedido que deseja marcar como entregue:"
    inputId <- getLine
    let pedidoIdEscolhido = read inputId :: Int
    lista <- readIORef pedidos
    let pedidoAtualizado = map (atualizarStatus pedidoIdEscolhido) lista
    if any (\p -> pedidoId p == pedidoIdEscolhido && status p == 1) lista
        then do
            writeIORef pedidos pedidoAtualizado
            putStrLn "\nStatus do pedido atualizado para entregue com sucesso!"
        else putStrLn "\nPedido não encontrado ou já está entregue."
  where
    -- Função auxiliar para atualizar o status de um pedido específico
    atualizarStatus :: Int -> Pedido -> Pedido
    atualizarStatus pid pedido
        | pedidoId pedido == pid = pedido { status = 0 }
        | otherwise = pedido

-- Lista todos os pedidos cadastrados
listarTodosPedidos :: IO ()
listarTodosPedidos = do
    -- Lê a lista de pedidos do IORef
    lista <- readIORef pedidos
    -- Verifica se a lista está vazia
    if null lista
        then putStrLn "\nNão há pedidos cadastrados no momento."
        else do
            putStrLn "\nListando todos os pedidos:"
            mapM_ (\pedido -> do
                putStrLn $ "Pedido ID: " ++ show (pedidoId pedido)
                putStrLn $ resumoPedido (comidas pedido)
                putStrLn $ "Status: " ++ case status pedido of
                    1 -> "Pedido está sendo entregue."
                    0 -> "Entregue"
                    _ -> "Desconhecido"
                putStrLn "---------------------------") lista

-- Função que calcula e exibe o faturamento total dos pedidos entregues.
verFaturamento :: IO ()
verFaturamento = do
    lista <- readIORef pedidos
    -- Filtra os pedidos que estão com status igual a 0 (entregues).
    let pedidosEntregues = filter (\p -> status p == 0) lista
    -- Verifica se a lista de pedidos entregues está vazia.
    if null pedidosEntregues
        then putStrLn "\nNão há pedidos entregues cadastrados. Faturamento: R$ 0,00."
        else do
            -- Calcula o faturamento total sumando o valor total de cada pedido entregue.
            let faturamentoTotal = sum (map valorTotal pedidosEntregues)
            -- Exibe o faturamento total dos pedidos entregues.
            putStrLn $ "\nFaturamento Total dos pedidos entregues: R$ " ++ show faturamentoTotal

-- Função que grava os pedidos entregues em um arquivo.
gravarPedidosEntregues :: IO ()
gravarPedidosEntregues = do
    -- Lê a lista de pedidos armazenada em uma referência IORef.
    lista <- readIORef pedidos
    -- Filtra apenas os pedidos que estão entregues (com status igual a 0).
    let entregues = filter (\p -> status p == 0) lista
    -- Verifica se a lista de entregas está vazia
    if null entregues
        then putStrLn "\nNão há pedidos entregues no momento."
        else do
            -- Solicita ao usuário o nome do arquivo onde os pedidos serão salvos.
            putStrLn "Informe o nome do arquivo onde deseja salvar os pedidos entregues:"
            arquivo <- getLine
            -- Garante que o nome do arquivo tenha a extensão `.txt`, caso o usuário não forneça.
            let nomeArquivo = if ".txt" `elem` words arquivo
                              then arquivo
                              else arquivo ++ ".txt"
            -- Cria o conteúdo a ser salvo, concatenando informações de cada pedido.
            let content = unlines $ map (\pedido -> 
                    "Pedido ID: " ++ show (pedidoId pedido) ++ "\n" ++ resumoPedido (comidas pedido) ++ "\n" ++ replicate 20 '-' ++ "\n") entregues
            -- Escreve o conteúdo no arquivo especificado.
            writeFile nomeArquivo content
            putStrLn $ "Pedidos entregues salvos em '" ++ nomeArquivo ++ "'."