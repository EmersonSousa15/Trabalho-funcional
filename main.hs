import Data.Char (isDigit, isUpper)


main :: IO ()
main = do
    putStrLn("Digite o codigo significativo:")
    s <- getLine
    let s' = adicionaVerificador s
    putStrLn $ "Cartao de credito: " ++ s'


adicionaVerificador :: String -> String
adicionaVerificador s = s ++ verificador
    where 
        verificador = formatVerificationCode ( verificationCode (stringToInt s) )


formatVerificationCode :: Int -> String
formatVerificationCode v =
    let vChar = intToString v
    in vChar ++ replicate (4 - length vChar) '0'
{-
formatVerificationCode :: Int -> String
formatVerificationCode v
    | length vChar < 4 = replicate (4 - length vChar) '0' ++ vChar
    | length vChar > 4 = take 4 vChar
    | otherwise = vChar
    where
        vChar = intToString v
-}



verificationCode :: [Int] -> Int
verificationCode v = 
    abs (product (filter (/=0) (fst tuplaIdendtificador)) - product (filter (/=0) (snd tuplaIdendtificador)))
    where
        tuplaIdendtificador = splitAt 6 (if length v == 12 then v else error "O codigo deve ter 12 digitos!") 



stringToInt :: String -> [Int]
--stringToInt = map getDigito pode ficar assim
stringToInt i = map getDigito i

getDigito :: Char -> Int
getDigito c
    | isDigit c = read [c] 
    | otherwise = error "Digite apenas numeros!"

intToString :: Int -> [Char]
--intToString = show pode ficar assim
intToString n = show n
    

validaCartao :: [Char] -> Bool
validaCartao s
    | length s /= 16 = False  -- Deve ter exatamente 16 dígitos
    | otherwise = let (codigo, verificador) = splitAt 12 s
                      esperado = formatVerificationCode . verificationCode $ map (\c -> read [c]) codigo
                  in verificador == esperado


validaSenha :: String -> Bool
validaSenha senha =
    let tamanhoValido = (6 <= length senha) && (length senha <= 10)
        temMaiuscula = not . null $ [c | c <- senha, isUpper c]
        temDigito = not . null $ [c | c <- senha, isDigit c]
    in tamanhoValido && temMaiuscula && temDigito
