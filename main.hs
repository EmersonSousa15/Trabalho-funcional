import Data.Char (isDigit, isUpper)

main :: IO ()
main = do
  putStrLn "===== Menu ====="
  putStrLn "1. Gerar cartão de crédito (12 dígitos)"
  putStrLn "2. Validar cartão de crédito (16 dígitos)"
  putStrLn "3. Validar senha"
  putStrLn "4. Filtrar lista de senhas"
  putStrLn "5. Sair"
  putStrLn "Escolha uma opção:"
  opcao <- getLine
  case opcao of
    "1" -> geraCartao >> main
    "2" -> validaCartaoIO >> main
    "3" -> validaSenhaIO >> main
    "4" -> filtraSenhasIO >> main
    "5" -> putStrLn "Encerrando..."
    _   -> putStrLn "Erro: Opção inválida." >> main

-- Opção 1: Gera o cartão com 12 dígitos
geraCartao :: IO ()
geraCartao = do
  putStrLn "Digite o código significativo (12 dígitos):"
  s <- getLine
  if length s /= 12 then
    putStrLn "Erro: O código deve conter exatamente 12 dígitos."
  else if not (all isDigit s) then
    putStrLn "Erro: O código deve conter apenas números."
  else do
    let sCompleto = adicionaVerificador s
    putStrLn $ "Cartão de crédito gerado: " ++ sCompleto

-- Opção 2: Valida um cartão de crédito com 16 dígitos
validaCartaoIO :: IO ()
validaCartaoIO = do
  putStrLn "Digite o número do cartão de crédito (16 dígitos):"
  s <- getLine
  if length s /= 16 then
    putStrLn "Erro: O cartão deve ter exatamente 16 dígitos."
  else if not (all isDigit s) then
    putStrLn "Erro: O cartão deve conter apenas números."
  else if validaCartao s then
    putStrLn "Cartão válido."
  else
    putStrLn "Erro: Cartão inválido."

-- Opção 3: Valida uma senha
validaSenhaIO :: IO ()
validaSenhaIO = do
  putStrLn "Digite a senha:"
  s <- getLine
  if not (lengthValid s) then
    putStrLn "Erro: A senha deve ter 6 a 10 caracteres."
  else if not (hasUpper s) then
    putStrLn "Erro: A senha deve conter pelo menos uma letra maiúscula."
  else if not (hasDigit s) then
    putStrLn "Erro: A senha deve conter pelo menos um número."
  else
    putStrLn "Senha válida."

-- Opção 4: Filtra uma lista de senhas (digite as senhas separadas por espaço)
filtraSenhasIO :: IO ()
filtraSenhasIO = do
  putStrLn "Digite as senhas, separadas por espaço:"
  s <- getLine
  let senhas   = words s
      validas  = filtraSenhas senhas
  if null validas then
    putStrLn "Nenhuma senha válida encontrada."
  else do
    putStrLn "Senhas válidas:"
    mapM_ putStrLn validas

--------------------------------------------------------------------------------
-- Funções de geração e validação do cartão de crédito

adicionaVerificador :: String -> String
adicionaVerificador s = s ++ verificador
  where 
    verificador = formatVerificationCode (verificationCode (stringToInt s))

formatVerificationCode :: Int -> String
formatVerificationCode v =
    let s = show v
    in if length s < 4 
          then s ++ replicate (4 - length s) '0'
          else take 4 s

verificationCode :: [Int] -> Int
verificationCode digits
    | length digits /= 12 = error "Erro interno: O código deve ter 12 dígitos!"
    | otherwise = abs (prodFirst - prodSecond)
  where
    (firstHalf, secondHalf) = splitAt 6 digits
    prodFirst  = product (filter (/= 0) firstHalf)
    prodSecond = product (filter (/= 0) secondHalf)

stringToInt :: String -> [Int]
stringToInt = map getDigito

getDigito :: Char -> Int
getDigito c
    | isDigit c = read [c]
    | otherwise = error "Erro interno: Dígito inválido encontrado!"

validaCartao :: String -> Bool
validaCartao s
    | length s /= 16 = False
    | otherwise =
        let (codigo, verificador) = splitAt 12 s
            esperado = formatVerificationCode (verificationCode (stringToInt codigo))
        in verificador == esperado

--------------------------------------------------------------------------------
-- Funções de validação de senha

validaSenha :: String -> Bool
validaSenha senha = lengthValid senha && hasUpper senha && hasDigit senha

lengthValid :: String -> Bool
lengthValid s = let l = length s in l >= 6 && l <= 10

hasUpper :: String -> Bool
hasUpper = any isUpper

hasDigit :: String -> Bool
hasDigit = any isDigit

filtraSenhas :: [String] -> [String]
filtraSenhas = filter validaSenha
