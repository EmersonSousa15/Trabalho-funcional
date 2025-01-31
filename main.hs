import Data.Char (isDigit)

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
formatVerificationCode v
    | length vChar < 4 = replicate (4 - length vChar) '0' ++ vChar  
    | length vChar > 4 = take 4 vChar
    | otherwise = vChar
    where 
        vChar = intToString v



verificationCode :: [Int] -> Int
verificationCode v = 
    abs (product (filter (/=0) (fst tuplaIdendtificador)) - product (filter (/=0) (snd tuplaIdendtificador)))
    where
        tuplaIdendtificador = splitAt 6 (if length v == 12 then v else error "O codigo deve ter 12 digitos!") 



stringToInt :: String -> [Int]
stringToInt s = map getDigito s

getDigito :: Char -> Int
getDigito c
    | isDigit c = read [c] 
    | otherwise = error "Digite apenas numeros!"

intToString :: Int -> [Char]
intToString n = show n
    