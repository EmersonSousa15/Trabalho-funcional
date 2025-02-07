import Data.Char (isDigit, isUpper)

main :: IO ()
main = do
  putStrLn "===== Menu ====="
  putStrLn "1. Gerar cartão de crédito (a partir de 12 dígitos)"
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
    _   -> putStrLn "Opção inválida." >> main

-- Opção 1: Gera o cartão a partir de 12 dígitos
geraCartao :: IO ()
geraCartao = do
  putStrLn "Digite o código significativo (12 dígitos):"
  s <- getLine
  if length s /= 12 || not (all isDigit s)
    then putStrLn "Erro: o código deve conter exatamente 12 dígitos numéricos."
    else do
      let sCompleto = adicionaVerificador s
      putStrLn $ "Cartão de crédito gerado: " ++ sCompleto

-- Opção 2: Valida um cartão de crédito com 16 dígitos
validaCartaoIO :: IO ()
validaCartaoIO = do
  putStrLn "Digite o número do cartão de crédito (16 dígitos):"
  s <- getLine
  if validaCartao s
    then putStrLn "Cartão válido."
    else putStrLn "Cartão inválido."

-- Opção 3: Valida uma senha
validaSenhaIO :: IO ()
validaSenhaIO = do
  putStrLn "Digite a senha:"
  s <- getLine
  if validaSenha s
    then putStrLn "Senha válida."
    else putStrLn "Senha inválida."

-- Opção 4: Filtra uma lista de senhas (digite as senhas separadas por espaço)
filtraSenhasIO :: IO ()
filtraSenhasIO = do
  putStrLn "Digite as senhas, separadas por espaço:"
  s <- getLine
  let senhas   = words s
      validas  = filtraSenhas senhas
  putStrLn "Senhas válidas:"
  mapM_ putStrLn validas

--------------------------------------------------------------------------------
-- Funções de geração e validação do cartão de crédito

-- Recebe uma string de 12 dígitos e adiciona ao final os 4 dígitos verificadores.
adicionaVerificador :: String -> String
adicionaVerificador s = s ++ verificador
  where 
    verificador = formatVerificationCode (verificationCode (stringToInt s))

-- Converte o resultado (Int) em uma string de 4 dígitos.
-- Se tiver menos de 4 dígitos, acrescenta zeros à direita; se tiver mais, pega apenas os 4 primeiros.
formatVerificationCode :: Int -> String
formatVerificationCode v =
    let s = show v
    in if length s < 4 
          then s ++ replicate (4 - length s) '0'
          else take 4 s

-- Calcula o código verificador a partir de uma lista de 12 dígitos inteiros.
-- Divide a lista em duas partes de 6 dígitos e subtrai o produto (dos dígitos não nulos) do primeiro grupo pelo do segundo.
-- Utiliza abs para evitar resultado negativo.
verificationCode :: [Int] -> Int
verificationCode digits
    | length digits /= 12 = error "O código deve ter 12 dígitos!"
    | otherwise = abs (prodFirst - prodSecond)
  where
    (firstHalf, secondHalf) = splitAt 6 digits
    prodFirst  = product (filter (/= 0) firstHalf)
    prodSecond = product (filter (/= 0) secondHalf)

-- Converte uma string em uma lista de inteiros, usando getDigito para cada caractere.
stringToInt :: String -> [Int]
stringToInt = map getDigito

-- Converte um caractere em dígito (se for numérico); caso contrário, gera erro.
getDigito :: Char -> Int
getDigito c
    | isDigit c = read [c]
    | otherwise = error "Digite apenas números!"

-- Valida um cartão de crédito de 16 dígitos:
-- Os 12 primeiros dígitos são usados para calcular os 4 dígitos verificadores e comparar com os dígitos informados.
validaCartao :: String -> Bool
validaCartao s
    | length s /= 16 || not (all isDigit s) = False
    | otherwise =
        let (codigo, verificador) = splitAt 12 s
            esperado = formatVerificationCode (verificationCode (stringToInt codigo))
        in verificador == esperado

--------------------------------------------------------------------------------
-- Funções de validação de senha

-- Uma senha é considerada válida se:
-- • Possuir entre 6 e 10 caracteres,
-- • Contiver pelo menos um caractere maiúsculo,
-- • Contiver pelo menos um dígito.
validaSenha :: String -> Bool
validaSenha senha = lengthValid senha && hasUpper senha && hasDigit senha
  where
    lengthValid s = let l = length s in l >= 6 && l <= 10
    hasUpper s    = any isUpper s
    hasDigit s    = any isDigit s

-- Filtra uma lista de senhas, retornando apenas as que são válidas.
filtraSenhas :: [String] -> [String]
filtraSenhas = filter validaSenha
