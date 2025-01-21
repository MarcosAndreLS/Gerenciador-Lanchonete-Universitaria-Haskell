-- Arquivo Administradores.hs
module Administradores where
import Pedidos

-- Realiza a autenticação do administrador verificando credenciais pré-definidas
autenticarAdministrador :: IO ()
autenticarAdministrador = do
    let administradores = [("Ismenia", "senha1"), ("Gabriel", "senha2")]
    putStrLn "\nLogin do Administrador"
    putStrLn "Digite o nome de usuário:"
    usuario <- getLine
    putStrLn "Digite a senha:"
    senha <- getLine
    if (usuario, senha) `elem` administradores
        then menuAdministrador -- Acessa o menu do administrador se as credenciais forem válidas
        else do
            putStrLn "\nCredenciais inválidas. Tente novamente."
            return ()

-- Exibe o menu de opções disponíveis para o administrador
menuAdministrador :: IO ()
menuAdministrador = do
    putStrLn "\nMenu do Administrador:"
    putStrLn "1 - Listar Pedidos Pendentes"
    putStrLn "2 - Mudar Status do Pedido para Entregue"
    putStrLn "3 - Ver Pedidos Entregues"
    putStrLn "4 - Ver Faturamento"
    putStrLn "7 - Voltar"
    opcao <- getLine
    processarOpcaoAdministrador opcao

-- Processa a escolha feita pelo administrador no menu
processarOpcaoAdministrador :: String -> IO ()
processarOpcaoAdministrador "1" = listarPedidosPorStatus 1 >> menuAdministrador -- Lista pedidos pendentes
processarOpcaoAdministrador "2" = mudarStatusPedido >> menuAdministrador  -- Altera o status de um pedido para "entregue"
processarOpcaoAdministrador "3" = listarPedidosPorStatus 0 >> menuAdministrador -- Lista pedidos entregues
processarOpcaoAdministrador "4" = verFaturamento >> menuAdministrador -- Mostra o faturamento total
processarOpcaoAdministrador "7" = return () -- Retorna ao menu principal
processarOpcaoAdministrador _   = do
    putStrLn "\nOpção inválida. Por favor, tente novamente."
    menuAdministrador