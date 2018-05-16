import Data.List
import Data.Char
main::IO()
main=do


 print (whisper "BAnaNA")
 print (removeSpaces "Na dedo ti duduka")
 print (shout "Na DeDo Drugiq duDuK")
 print (capitalise "Na dedu KAVALA")
 print (switchCaps "Na dedu gaDULKata")
 print (encode ['A'..'Z'] 'A' 1)
 print (encrypt ['A'..'Z'] (-2) "AXYZ")
 print (decrypt ['A'..'Z'] 1 "CDE")
 print (joinWords ',' ["The", "Sound", "of", "Silence"])
 
{-
Пример 1. Да се дефинира фунцкцията whisper str, която обръща в малки букви 
всички символи на низа str.
-}
whisper::String->String
whisper "" = ""
whisper (x:xs) = toLower x : whisper xs


{-
Пример 2. Да се дефинира функцията removeSpaces str, която премахва всички интервали
от символния низ str.
-}
removeSpaces :: String->String
removeSpaces "" = ""
removeSpaces (x:xs) = 
    if isSpace x then removeSpaces xs else x: removeSpaces xs
    
{-
Задача 1. Дефинирайте функцията shout str, която обръща в главни всички букви 
на символния низ str
-}
shout :: String -> String
shout "" = ""
shout (x:xs) = toUpper x : shout xs

{-
Задача 2. Напишете функцията capitalise str, която обръща само първата буква в 
главна, а останалите в малки.
-}
capitalise :: String -> String
capitalise "" = ""
capitalise (x:xs) = toUpper x : whisper xs

{-
Задача 3. Напишете функцията switchCaps str, която обръща малките букви в големи,
а големите в малки.
-}
switchCaps :: String->String
switchCaps "" = ""
switchCaps (x:xs) = 
    if isUpper x
    then toLower x : switchCaps xs
    else toUpper x : switchCaps xs

{-
Задача 4. Дефинирайте функциите encrypt str n и decrypt str n, които имплементират един от най-простите
начини за кодиране на информация - Ceasar Cipher. Той работи като измества всяка буква 
от азбуката с буквата която е n позиции след нея. Ако някоя от буквите "превърти", т.е. 
задмине последната буква от азбуката, то тогава продължаваме с буквите в началото на азбуката.
Тая педерастия тука е домашното 2 задача
-}
encode :: [Char] -> Char -> Int -> Char
encode alphabet x n = chr $ (ord x - ord 'A' + n) `mod` 26 + ord 'A'


encrypt :: [Char] -> Int -> String -> String
encrypt alphabet n str = 
   if null str
   then ""
   else shifted : encrypt alphabet n xs
   where 
       shifted = chr $ (ord x - ord 'A' + n) `mod` 26 + ord 'A'
       x  = head str
       xs = tail str
       
decrypt :: [Char] -> Int -> String -> String
decrypt alphabet n str = encrypt alphabet (-n) str

{-
Задача 5. Напишете функцията joinWords c strs, която слива няколко думи в една,
използвайки за разделител символ c.
-}
joinWords :: Char -> [String] -> String
joinWords c strs
     | null strs        = ""
     | length strs == 1 = head strs
     | otherwise        = head strs ++ [c] ++ joinWords c (tail strs)

