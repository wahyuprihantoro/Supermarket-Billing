-- Wahyu Prihantoro
-- 1406579100
-- PemDek B

--ni : NumItem
--bc : BarCode
--n : Name

-- import untuk handle duplicate function
import Prelude hiding (lookup)

-- define the type
type Name = String
type Price = Int
type NumItem = Int
type BarCode = Int
type Database = [(BarCode, Name, Price)]
type TillType = [BarCode]
-- pair of bar-code and the number of items
type CodeNumItem = [(BarCode, NumItem)]
-- (name of item, the number of items, the total price)
type BillType = [(Name, NumItem, Price)]

-- define codeIndex
codeIndex :: Database
codeIndex = [(4719, "Fish Fingers", 121),
	(5643, "Nappies", 1010),
	(3814, "Orange Jelly", 56),
	(1111, "Hula Hoops", 21),
	(1112, "Hula Hoops (Giant)", 133),
	(1234, "Dry Sherry, 1lt", 540)]

--defint line length	
lineLength :: Int
lineLength = 30
	
-- function #1
-- dollars : mengembalikan string nilai dollars setelah p dibagi 10
-- cents : mengembalikan 2 digit string nilai cents dari p yaitu dengan menggunakan fungsi mod
formatPence :: Price -> String
formatPence p = dollars ++ ('.' : cents)
	where
		dollars = show (p `div` 100)
		cents = centsFixed (show (p `mod` 100))

-- fungsi centsFixed untuk mengatur format 2 digit cents
centsFixed :: String -> String
centsFixed xs
    | length xs <2 = '0':xs
    | otherwise    = xs
		
--function #2
--n : name
--nippair : tuple antara NumItem dan Price, (NumItem,Price)
--dots : titik - titik yang panjangnya dihitung dengan mengurangi lineLength dengan panjang n dan panajan nippair
formatLine :: (Name, NumItem, Price) -> String
formatLine (n, ni, p) = n ++ dots ++ nippair ++ "\n"
	where
		fixedPrice = formatPence p
		nameLength = length n
		nippair = "(" ++ (show ni) ++ ", " ++ fixedPrice ++ ")"
		nippairLength = length nippair
		dotsLength = lineLength - nameLength - nippairLength
		dots = replicate dotsLength '.'

--function #3
--mencetak detail setiap barang yang dibeli setiap barisnya menggunakan formatLine
formatLines :: [(Name, NumItem, Price)] -> String
formatLines [] = []
formatLines (x:xs) = formatLine x ++ formatLines xs

--function #4
--jumlahkan semua nilai p (price) pada masukan bertipe BillType
makeTotal :: BillType -> Price
makeTotal [] = 0
makeTotal ((n,ni,p):xs) = p + makeTotal xs

--function #5
--mengatur output baris
--dots : titik - titik yang panjangnya dihitung dengan mengurangi lineLength dengan panjang string "Total" dan panjang fixedPrice
--fixedPrice : harga total yang sudah diatur formatnya menggunakan formatPence
formatTotal :: Price -> String
formatTotal p = "Total" ++ dots ++ fixedPrice
	where
		fixedPrice = formatPence p
		priceLength = length fixedPrice
		fixedLength = lineLength - (length "Total")
		dotsLength = fixedLength - priceLength
		dots = replicate dotsLength '.'

--function #6
--mencetak informasi setiap baris supermarket billing
formatBill :: BillType -> String
formatBill x = "Haskell Store\n\n" ++ formatLines x ++ "\n" ++ formatTotal (makeTotal x) ++ "\n"

--function #7
--mencari nama dan price sesuai barcode, diimplementasikan menggunakan recursive
--jika sudah ditemukan di awal, langsung return (Name, Price)
--Jika tidak ditemukan return ("Unknown Item", 0)
look :: Database -> BarCode -> (Name, Price)
look ((b,n,p):xs) bc
	| b==bc = (n,p)
	| not (null xs) = look xs bc
	| otherwise = ("Unknown Item", 0)

--function #8
--mencari nama dan price dari codeIndex
lookup :: BarCode -> (Name, Price)
lookup bc = look codeIndex bc;

--function #9
--mengurutkan masukan dengan quicksort
--setiap elemen jadikan tuple dengan inisiasi nilai NumItem = 1
--lakukan penggabungan elemen jika ada yang sama dengan menggunakan fungsi makeCodeNumItemHelper
makeCodeNumItem :: TillType -> CodeNumItem
makeCodeNumItem tt = makeCodeNumItemHelper [(a,b)|(a,b)<-[(x,1)|x<-quicksort tt]]

-- auxiliary function for function #9	
-- jika ada 2 elemen pertama yang barcode nya sama, gabungkan 2 elemen tersebut menjadi satu dengan numItem nya dijumlahkan
-- lalu lakukan makeCodeNumItemHelper lagi pada list yang ada
-- jika tidak lakukan makeCodeNumItemHelper dari elemen kedua dengan elemen pertama ditambahakan sebelumnya secara rekursif
makeCodeNumItemHelper :: CodeNumItem -> CodeNumItem 
makeCodeNumItemHelper [] = []
makeCodeNumItemHelper [a] = [a]
makeCodeNumItemHelper ((a,b):(c,d):xs) = 
	if a==c then makeCodeNumItemHelper ((a,(b+d)):xs) 
	else (a,b) : makeCodeNumItemHelper ((c,d):xs) 

-- auxiliary function for function #9
-- quicksort, ref : http://learnyouahaskell.com/recursion
quicksort :: (Ord a) => [a] -> [a]  
quicksort [] = []  
quicksort (x:xs) =   
    let smallerSorted = quicksort [a | a <- xs, a <= x]  
        biggerSorted = quicksort [a | a <- xs, a > x]  
    in  smallerSorted ++ [x] ++ biggerSorted  
	
-- function #10
-- cari info nama dan harga barang sesuai barcode dengan menggunakan lookup
-- untuk menghitung harga total setiap barang : numItem * harga baragn yang dicari (snd np)
-- np : nameprice
makeBill :: CodeNumItem -> BillType
makeBill [] = []
makeBill ((bc,ni):xs) = (fst np, ni, ni*(snd np)) : makeBill xs 
	where
		np = lookup bc

--function #11
--hitung diskon dengan cara menbagi 2 banyak botol yg dibeli kemudian kali 100
--mencari banyak botol dengan menggunakan fungsi findBottle
makeDiscount :: BillType -> Int
makeDiscount xs = (ni `div` 2) * 100
	where
		ni = findBottle xs
		
--mencari barang "Dry Sherry, 1lt" dengan menggunakan fungsi findBottleHelper
findBottle :: BillType -> Int
findBottle xs = ni
	where
		[(n,ni,p)] = findBottleHelper xs
		
findBottleHelper :: BillType -> BillType
findBottleHelper x = [(n,ni,p)|(n,ni,p)<-x, n == "Dry Sherry, 1lt"]

--function #12
--mengatur output baris
--dots : titik - titik yang panjangnya dihitung dengan mengurangi lineLength dengan panjang string "Discount" dan panjang fixedDiscount
--fixedDiscount : total diskon yang sudah diatur formatnya menggunakan formatPence
formatDiscount :: Int -> String
formatDiscount d = "Discount" ++ dots ++ fixedDiscount
	where
		fixedDiscount = formatPence d
		priceLength = length fixedDiscount
		fixedLength = lineLength - (length "Discount")
		dotsLength = fixedLength - priceLength
		dots = replicate dotsLength '.'
		
--function #13
--mencetak informasi setiap baris supermarket billing dengan adanya discount
formatBill' :: BillType -> String
formatBill' x = "Haskell Store\n\n" ++ formatLines x ++ "\n" ++formatDiscount (makeDiscount x) ++ "\n\n" ++ formatTotal (makeTotal x - makeDiscount x) ++ "\n"
