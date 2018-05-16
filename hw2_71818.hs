import Data.Char
import Data.List
import Data.Typeable
import Data.Maybe


--Задача 1. Нормализация на входните данни

{-Енигма, както повечето криптиращи машини от това време, е разполагала с
клавиатура със само 26-те главни букви от латинската азбука. Затова, преди да бъдат
криптирани, всички съобщения трябвало да бъдат приведени в т. нар. нормален вид:
всички числени стойности бивали изписвани словом, всички малки букви ставали
главни, а интервалите и пунктуационните знакове били премахвани или заменяни с
кодови комбинации от главни букви (напр. интервалът бил заменян с X и т. н.)
Напишете функция normalize message​, която нормализира входното
съобщение. Правилата за нормализация са следните:
- Всички малки букви стават главни.
- Ако съобщението съдържа цифри, функцията връща грешка.
- Всички останали знакове се игнорират.-}

normalize :: String -> String
normalize [] = []
normalize (x:xs) = 
               if isDigit x 
               then error "digits not allowed" 
               else if isLetter x 
               then toUpper x : normalize xs 
               else normalize xs
               
--Задача 2. Цезаров шифър

{-
а). Напишете функция encode alphabet ch offset​, която приема списък от знакове
alphabet, знак ch и отместване offset и връща знака от alphabet, отместен на offset от ch
(по модул дължината на списъкa). Функцията encode трябва да работи както с
положително, така и с отрицателно отместване и да връща грешка, ако ch не е
елемент на alphabet.
N.B. ​Не е задължително буквите в alphabet да са подредени от ‘A’ до ‘Z’, т.е. НЕ ​може
да разчитате на функциите ord ​и chr!
-}         

{-б). Напишете функция encrypt alphabet offset normalized​, която приема азбука
alphabet, отместване offset и съобщение в нормализиран вид и връща съобщението,
криптирано със съответното отместване.-}

{-в). Напишете функция decrypt alphabet offset ecrypted​, която приема отместване
offset и съобщение, криптирано с това отместване, и връща оригиналното съобщение
в нормализиран вид. Можете да използвате факта, че декриптирането на Цезаров
шифър с отместване offset е еквивалентно на криптиране с отместване -offset.-}

encode :: String -> Char -> Int -> Char
encode xs ch ofs 
    | (elemIndex ch xs) == Nothing = error ("unsupported symbol")
    | (ofs + fromJust (elemIndex ch xs))<0 = xs!!(length xs +ofs + fromJust (elemIndex ch xs)) 
    | (ofs + fromJust (elemIndex ch xs))>length xs = xs!!(ofs + fromJust (elemIndex ch xs) - length xs) 
    | otherwise = xs!!(ofs + fromJust (elemIndex ch xs))
    
encrypt:: String -> String -> Int -> String
encrypt [] [] _ =[]
encrypt [] _ _ =[]
encrypt _ [] _ =[]
encrypt xs (c:str) ofs = (encode xs c ofs) : encrypt xs str ofs

decrypt::String ->String -> Int->String
decrypt [] [] _ =[]
decrypt [] _ _ =[]
decrypt _ [] _ =[]
decrypt xs (c:str) ofs = (decode xs c ofs):decrypt xs str ofs

--Задача 3. Атака на Цезаровия шифър

{-Една от основните слабости на Цезаровия шифър се състои в това, че броят на
възможните шифри е ограничен до броя на ротациите на буквите в азбуката минус
едно. Това прави Цезаровия шифър податлив на т. нар. brute force атака, т. е. атака,
която генерира всички възможни дешифровки на кодираното съобщение.

а). Напишете функцията crackall alphabet encrypted​, която връща списък от
всички възможни дешифровки на кодираното съобщение enccrypted-}

crackall::String->String->[String]
crackall [] _ =[]
crackall xs str = helper xs str 0 (length xs)
    where helper xs str ofs end = if ofs==end then [] 
      else (decrypt xs str (ofs+1)):helper xs str (ofs+1) end
      
{-б). След като сме генерирали всички възможни дешифровки, бихме могли лесно да
намерим най-вероятните от тях, използвайки факта, че някои кратки думи, напр. the,
at, on​, се срещат много често в английския език.
За тази цел най-напред напишете функция substring sub str​, която
проверява дали поднизът sub се среща в низа str.-}

substring :: String -> String -> Bool
substring (x:xs) [] = False
substring xs ys
    | prefix xs ys = True
    | substring xs (tail ys) = True
    | otherwise = False

prefix :: String -> String -> Bool
prefix [] ys = True
prefix (x:xs) [] = False
prefix (x:xs) (y:ys) = (x == y) && prefix xs ys

{-в). Използвайте функциите от предишните две подточки, за да напишете функцията
crackcandidates alphabet commonwords encrypted​, която приема списък с често
срещани думи и криптирано съобщение и връща списък с потенициални вероятни
разшифровки.
Една разшифровка се смята за вероятна, ако съдържа поне една от думите от
списъка с често срещани думи.-}

crackcandidates :: [Char]->[[Char]]-> [Char]->[[Char]]
crackcandidates alphabet [[]] []=[[]]
crackcandidates alphabet commonwords encrypted =
                nub  [x|(i,x)<-enumerate (crackall alphabet encrypted),(i,y)<-enumerate commonwords,substring y x]

enumerate :: Integral b => [a] -> [(b, a)]
enumerate xs = zip [0..] xs 

--Задача 4: Polysubstitution cypher (шифър с множествено заместване)

{-Един от простите начини да се справим със слабостта на Цезаровия шифър е
да разбием съобщението на блокове от по няколко знака и да криптираме всеки от тях
с различен Цезаров шифър, отместен с определена стъпка спрямо предишния.


а). Напишете функция polyencrypt alphabet offset step blockSize normalized​,
която приема азбука alphabet, първоначално отместване offset, стъпка step и размер
на блока blockSize, както и съобщение в нормализиран вид, и връща криптирано
съобщение, първите blockSize знака на което са криптират с отместване offset,
следващите blockSize знака - с отместване offset + step, и т. н.-}

polyencrypt :: String -> Int -> Int -> Int -> String -> String
polyencrypt alphabet offset step blockSize [] = []
polyencrypt alphabet offset step blockSize normalized = helper alphabet offset step blockSize normalized 0          

helper :: String -> Int -> Int -> Int -> String -> Int -> String
helper alphabet offset step blockSize [] _ = []
helper alphabet offset step blockSize normalized index = 
      encrypt alphabet (take blockSize normalized) (offset + step * index)++ 
         helper alphabet offset step blockSize (drop blockSize normalized) (index + 1)
         
{-б). Напишете функция polydecrypt alphabet offset step blockSize encrypted​,
която декриптира съобщението от предишната подточка.-}


polydecrypt :: String -> Int -> Int -> Int -> String -> String
polydecrypt alphabet offset step blockSize [] = []
polydecrypt alphabet offset step blockSize encrypted = helper1 alphabet offset step blockSize encrypted 0

helper1 :: String -> Int -> Int -> Int -> String -> Int -> String
helper1 alphabet offgset step blockSize [] _ = []
helper1 alphabet offset step blockSize encrypted index =
     decrypt alphabet (take blockSize encrypted) (offset + step * index) ++
           helper1 alphabet offset step blockSize (drop blockSize encrypted )(index+1)         

      