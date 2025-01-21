-- Arquivo Main.hs
module Main where

import System.IO
import Data.List
import Clientes
import Administradores
-- Função principal que exibe o menu inicial e processa a escolha do usuário
main :: IO ()
main = do
    putStrLn "\nBem-vindo à Lanchonete Universitária da Ismênia!"
    putStrLn "Por favor, escolha uma opção:"
    putStrLn "1 - Cliente"
    putStrLn "2 - Administrador"
    putStrLn "3 - Sair"
    opcao <- getLine
    processarOpcao opcao

-- Processa a escolha do menu principal\processarOpcao :: String -> IO ()
processarOpcao :: String -> IO ()
processarOpcao "1" = menuCliente >> main
processarOpcao "2" = autenticarAdministrador >> main
processarOpcao "3" = putStrLn "\nObrigado por visitar a Universitária da Ismênia!. Até a próxima!"
processarOpcao _   = do
    putStrLn "\nOpção inválida. Por favor, tente novamente."
    main

