import Data.Char (isDigit, isUpper)

main :: IO ()
main = do
    putStrLn "Digite o código significativo (12 dígitos):"
    s <- getLine
    if length s /= 12 || not (all isDigit s)
      then putStrLn "Erro: o código deve conter exatamente 12 dígitos numéricos."
      else do
          let s' = adicionaVerificador s
          putStrLn $ "Cartão de crédito: " ++ s'

-- Adiciona os 4 dígitos verificadores ao final do código
adicionaVerificador :: String -> String
adicionaVerificador s = s ++ verificador
  where 
    verificador = formatVerificationCode (verificationCode (stringToInt s))

-- Formata o código verificador conforme os critérios:
-- se tiver menos de 4 dígitos, acrescenta zeros à direita; se tiver mais, pega os 4 primeiros.
formatVerificationCode :: Int -> String
formatVerificationCode v =
    let s = show v
    in if length s < 4 
          then s ++ replicate (4 - length s) '0'
          else take 4 s

-- Calcula o código verificador a partir de uma lista de 12 dígitos inteiros.
-- Divide a lista em duas partes de 6 dígitos e calcula a diferença absoluta entre os produtos (desconsiderando zeros).
verificationCode :: [Int] -> Int
verificationCode digits
    | length digits /= 12 = error "O código deve ter 12 dígitos!"
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
    | otherwise = error "Digite apenas números!"
    

validaCartao :: String -> Bool
validaCartao s
    | length s /= 16 || not (all isDigit s) = False
    | otherwise =
        let (codigo, verificador) = splitAt 12 s
            esperado = formatVerificationCode (verificationCode (stringToInt codigo))
        in verificador == esperado


validaSenha :: String -> Bool
validaSenha senha = lengthValid senha && hasUpper senha && hasDigit senha
  where
    lengthValid s = length s >= 6 && length s <= 10
    hasUpper s    = any isUpper s
    hasDigit s    = any isDigit s

FiltraSenhas :: [String] -> [String]
FiltraSenhas lista = filter validaSenha
