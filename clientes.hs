-- Arquivo clientes.hs
module Clientes where
    
import Cardapio
import Pedidos

-- Exibe o menu de opções disponíveis para o cliente
menuCliente :: IO ()
menuCliente = do
    putStrLn "\nMenu do Cliente:"
    putStrLn "1 - Ver Cardápio"
    putStrLn "2 - Fazer Pedido"
    putStrLn "3 - Ver Pedido pelo ID"
    putStrLn "4 - Excluir Pedido"
    putStrLn "5 - Listar todos Pedidos"
    putStrLn "7 - Voltar"
    opcao <- getLine
    processarOpcaoCliente opcao

-- Processa a escolha feita pelo cliente no menu
processarOpcaoCliente :: String -> IO ()
processarOpcaoCliente "1" = verCardapio >> menuCliente -- Chama a função para exibir o cardápio
processarOpcaoCliente "2" = fazerPedido >> menuCliente -- Chama a função para fazer um pedido
processarOpcaoCliente "3" = verPedidoID >> menuCliente -- Chama a função para visualizar um pedido pelo ID
processarOpcaoCliente "4" = excluirPedido >> menuCliente -- Chama a função para excluir um pedido pelo ID
processarOpcaoCliente "5" = listarTodosPedidos >> menuCliente -- Lista todos os pedidos feitos pelo cliente
processarOpcaoCliente "7" = return() -- Retorna ao menu principal
processarOpcaoCliente _   = do
    putStrLn "\nOpção inválida. Por favor, tente novamente."
    menuCliente