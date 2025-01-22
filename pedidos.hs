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
    -- Exibe o cardápio de opções ao usuário.
    verCardapio
    -- Opções do menu para o usuário escolher
    putStrLn "11 - finalizar"
    putStrLn "12 - ver itens que já foram adicionados e preço"
    putStrLn "\nDigite o número do item:"
    opcao <- getLine
    case opcao of
        -- Opção para adicionar hambúrgueres.
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
        -- Opção para adicionar pizzas.
        "2" -> do
            verPizzas
            putStrLn "Escolha o sabor da Pizza:"
            sabor <- getLine
            case sabor of
                "1" -> registrarItem ("Pizza Mexicana", 50.0)
                "2" -> registrarItem ("Pizza Frango", 40.0)
                "3" -> registrarItem ("Pizza Calabresa", 45.0)
                "4" -> registrarItem ("Pizza Nordestina", 60.0)
                _ -> do
                    putStrLn "Opção inválida. Por favor, tente novamente."
                    coletarItens itens total
        -- Opção para adicionar coxinhas.
        "3" -> do
            verCoxinhas
            putStrLn "Escolha o sabor da Coxinha:"
            sabor <- getLine
            case sabor of
                "1" -> registrarItem ("Coxinha Frango", 3.0)
                "2" -> registrarItem ("Coxinha Frango com Cheddar", 4.0)
                "3" -> registrarItem ("Coxinha Frango com Catupiry", 4.0)
                "4" -> registrarItem ("Coxinha Carne", 3.0)
                _ -> do
                    putStrLn "Opção inválida. Por favor, tente novamente."
                    coletarItens itens total
        -- Opção para adicionar bombas.
        "4" -> registrarItem ("Bomba", 3.0)
        -- Opção para adicionar pastéis.
        "5" -> do
            verPasteis
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
        -- Opção para adicionar salsichão.
        "6" -> registrarItem ("Salsichão", 3.0)
        -- Opção para adicionar refrigerantes.
        "7" -> registrarItem ("Refrigerante", 8.0)
        -- Opção para adicionar água mineral.
        "8" -> registrarItem ("Água Mineral", 2.0)
        -- Opção para adicionar água de coco.
        "9" -> registrarItem ("Água de Coco", 4.0)
        -- Opção para adicionar cervejas.
        "10" -> do
            verCervejas
            putStrLn "Escolha o tipo de Cerveja:"
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
        -- Opção para finalizar o pedido.
        "11" -> finalizarPedido itens
        -- Opção para ver os itens adicionados e o total acumulado.
        "12" -> do
            putStrLn "\nItens adicionados até agora:"
            putStrLn $ resumoPedido itens  -- Reaproveitar a função `resumoPedido`
            coletarItens itens total
        -- Caso o usuário insira uma opção inválida.
        _ -> do
            putStrLn "Opção inválida. Por favor, tente novamente."
            coletarItens itens total
  where
    -- Função que registra o item selecionado no pedido e atualiza o total.
    registrarItem (nome, preco) = do
        let novosItens = itens ++ [(nome, preco)] -- Adiciona o novo item à lista de itens
        let novoTotal = total + preco -- Atualiza o total com o preço do novo item
        coletarItens novosItens novoTotal  -- Chama a função novamente para continuar coletando itens

-- Função que finaliza o pedido, gerando um ID único e adicionando o pedido à lista de pedidos.
-- Parâmetros:
--   - itens: Lista de tuplas representando os itens do pedido. Cada tupla contém o nome do item (String) e seu preço (Double).
finalizarPedido :: [(String, Double)] -> IO ()
finalizarPedido itens = do
    -- Verifica se a lista de itens está vazia.
    if null itens
        -- Caso a lista esteja vazia, exibe uma mensagem informando que o pedido não foi realizado.
        then putStrLn "\nVocê não selecionou nenhum item. Pedido não realizado."
        -- Caso contrário, prossegue com a finalização do pedido.
        else do
            -- Gera um ID único para o pedido.
            idPedido <- gerarIdPedido
            -- Cria um novo pedido utilizando os itens fornecidos. O total é calculado somando os preços dos itens.
            let novoPedido = Pedido idPedido itens 1 (sum (map snd itens))
            -- Adiciona o novo pedido à lista de pedidos.
            adicionarPedido novoPedido
            -- Exibe uma mensagem de sucesso para o usuário.
            putStrLn "\nPedido realizado com sucesso!"
            -- Exibe um resumo do pedido para o usuário, incluindo os itens selecionados.
            putStrLn $ "Resumo do pedido:\n" ++ resumoPedido itens

-- Visualiza detalhes de um pedido específico pelo ID
verPedidoID :: IO ()
verPedidoID = do
    -- Solicita ao usuário o ID do pedido que deseja visualizar.
    putStrLn "\nDigite o ID do seu pedido para visualizar:"
    pedidoIdInput <- getLine
    -- Converte a entrada do usuário (String) para um número inteiro (Int).
    let pedidoIdBusca = read pedidoIdInput :: Int
    -- Lê a lista de pedidos armazenada na referência de IO (pedidos).
    lista <- readIORef pedidos
    -- Filtra os pedidos na lista para encontrar aquele com o ID especificado.
    let pedidoEncontrado = filter (\p -> pedidoId p == pedidoIdBusca) lista
    -- Verifica se o pedido foi encontrado.
    if null pedidoEncontrado
        -- Caso o pedido não seja encontrado, exibe uma mensagem informando o usuário.
        then putStrLn "\nPedido não encontrado."
        -- Caso o pedido seja encontrado, exibe seus detalhes.
        else do
            putStrLn "\nPedido Encontrado:"
            -- Itera sobre os pedidos encontrados (em teoria, apenas um) e exibe suas informações.
            mapM_ (\(Pedido pid itens status total) -> do
                -- Exibe o ID do pedido.
                putStrLn $ "Pedido ID: " ++ show pid
                -- Exibe um resumo dos itens no pedido.
                putStrLn $ resumoPedido itens
                -- Exibe o status do pedido, indicando se está pendente ou sendo entregue.
                putStrLn $ "Status: " ++ if status == 1 
                    then "Pedido pendente." 
                    else "Pedido está sendo entregue."
                putStrLn "---------------------------") pedidoEncontrado

-- Exclui um pedido com base no ID, se ele ainda estiver pendente
excluirPedido :: IO ()
excluirPedido = do
    -- Solicita ao usuário o ID do pedido que deseja excluir.
    putStrLn "\nDigite o ID do pedido que deseja excluir:"
    inputId <- getLine
    -- Converte a entrada do usuário (String) para um número inteiro (Int).
    let idProcurado = read inputId :: Int
    -- Lê a lista atual de pedidos da referência de IO (pedidos).
    lista <- readIORef pedidos
    -- Filtra a lista de pedidos para encontrar o pedido correspondente ao ID fornecido
    let pedidoEncontrado = filter (\p -> pedidoId p == idProcurado) lista
    -- Verifica se o pedido foi encontrado.
    if null pedidoEncontrado
        -- Caso o pedido não seja encontrado, exibe uma mensagem informando o usuário.
        then putStrLn "\nPedido não encontrado."
        -- Caso o pedido seja encontrado, realiza verificações adicionais.
        else do
            -- Obtém o pedido encontrado (assume que há apenas um pedido com o ID fornecido).
            let pedido = head pedidoEncontrado
            -- Verifica o status do pedido. Apenas pedidos pendentes (status == 1) podem ser excluídos.
            if status pedido /= 1
                -- Exibe uma mensagem informando que apenas pedidos pendentes podem ser excluídos.
                then putStrLn "\nApenas pedidos com status '1' (pendentes) podem ser excluídos."
                -- Exclui o pedido caso ele seja pendente.
                else do
                    -- Cria uma nova lista de pedidos, excluindo o pedido correspondente ao ID.
                    let resto = filter (\p -> pedidoId p /= idProcurado) lista
                    -- Atualiza a referência de IO (pedidos) com a lista filtrada.
                    writeIORef pedidos resto
                    -- Exibe uma mensagem de sucesso para o usuário.
                    putStrLn "\nPedido excluído com sucesso!"
                    -- Exibe um resumo do pedido excluído.
                    putStrLn $ "Resumo do pedido excluído:\n" ++ resumoPedido (comidas pedido)

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

-- Placeholder para funcionalidades específicas do administrador

-- Lista todos os pedidos filtrados por status (pendente ou entregue)
listarPedidosPorStatus :: Int -> IO ()
listarPedidosPorStatus statusProcurado = do
    -- Lê a lista de pedidos armazenada em 'pedidos'.
    lista <- readIORef pedidos
    -- Filtra os pedidos cujo status corresponde ao 'statusProcurado'.
    let filtrados = filter (\p -> status p == statusProcurado) lista
    -- Verifica se há pedidos correspondentes.
    if null filtrados
        then -- Se não houver pedidos, exibe uma mensagem informativa.
            putStrLn "\nNão há pedidos no momento."
        else do
            -- Define o nome do status para exibição (Pendentes ou Entregues).
            let nomeStatus = if statusProcurado == 1 then "Pendentes" else "Entregues"
            -- Exibe o cabeçalho indicando o status dos pedidos listados.
            putStrLn $ "\nPedidos " ++ nomeStatus
            -- Para cada pedido filtrado, exibe os detalhes relevantes.
            mapM_ (\pedido -> do
                putStrLn $ "Pedido ID: " ++ show (pedidoId pedido) -- Exibe o ID do pedido.
                putStrLn $ resumoPedido (comidas pedido)          -- Exibe o resumo das comidas.
                putStrLn "---------------------------"            -- Adiciona uma separação visual.
                ) filtrados

-- Atualiza o status de um pedido para "entregue" (status = 0)
mudarStatusPedido :: IO ()
mudarStatusPedido = do
    -- Solicita ao usuário o ID do pedido que deseja marcar como entregue.
    putStrLn "\nDigite o ID do pedido que deseja marcar como entregue:"
    inputId <- getLine
    -- Converte a entrada do usuário (String) para um número inteiro (Int).
    let pedidoIdEscolhido = read inputId :: Int
    -- Lê a lista de pedidos da referência de IO (pedidos).
    lista <- readIORef pedidos
    -- Cria uma nova lista de pedidos com o status atualizado para o ID especificado.
    let pedidoAtualizado = map (atualizarStatus pedidoIdEscolhido) lista
    -- Verifica se existe um pedido com o ID fornecido e se ele está com o status pendente.
    if any (\p -> pedidoId p == pedidoIdEscolhido && status p == 1) lista
        -- Se o pedido for encontrado e estiver pendente, atualiza o status na referência de IO.
        then do
            writeIORef pedidos pedidoAtualizado
            -- Exibe uma mensagem de sucesso para o usuário.
            putStrLn "\nStatus do pedido atualizado para entregue com sucesso!"
        -- Caso contrário, exibe uma mensagem informando que o pedido não foi encontrado ou já está entregue.
        else putStrLn "\nPedido não encontrado ou já está entregue."
  where
    -- Função auxiliar para atualizar o status de um pedido específico.
    -- Parâmetros:
    --   - pid: ID do pedido que será atualizado.
    --   - pedido: Pedido atual a ser verificado e possivelmente modificado.
    -- Retorna:
    --   - Um pedido com o status alterado para 0 (entregue), caso o ID corresponda.
    --   - O pedido original caso o ID não corresponda.
    atualizarStatus :: Int -> Pedido -> Pedido
    atualizarStatus pid pedido
        -- Atualiza o status se o ID do pedido corresponder ao ID fornecido.
        | pedidoId pedido == pid = pedido { status = 0 }
        -- Mantém o pedido inalterado caso o ID não corresponda.
        | otherwise = pedido

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