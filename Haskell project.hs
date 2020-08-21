import System.Random
import System.IO.Unsafe
users = ["user1", "user2", "user3", "user4"]
items = ["item1", "item2", "item3", "item4", "item5", "item6"]
purchasesHistory =  [
                        ("user1", [["item1", "item2", "item3"], ["item1", "item2", "item4"]]),
                        ("user2", [["item2", "item5"], ["item4", "item5"]]),
                        ("user3", [["item3", "item2"]]),
                        ("user4", [])
                    ]
--Empty freq list

type Pair =([Char],[Int])
createEmptyFreqList :: [[Char]]->[Pair]

createEmptyFreqList []=[]
createEmptyFreqList (x:xs) = [(x,[])]++createEmptyFreqList xs

-- ***********************


--freq list user

freqListItems:: String -> [(String, Int)]

freqListItems user =freqListItemsHelper user purchasesHistory

freqListItemsHelper user [] = error "this user doen't exist"
freqListItemsHelper user ((x,a):xs) |x==user = freqListItemsHelper22 a       --gets the list of the required user
                                    |otherwise =freqListItemsHelper user xs



creatZeroFreqList [] l=l
creatZeroFreqList (x:xs) l =creatZeroFreqList xs (creatZeroFreqListH x l)

creatZeroFreqListH [] l=l
creatZeroFreqListH (y:ys) l= if elemBta3tna y l then creatZeroFreqListH ys l
                             else creatZeroFreqListH ys (l++[(y,0)] )

elemBta3tna a [] =False
elemBta3tna a ((s,n):xs) =if a == s then True
                          else elemBta3tna a xs





freqListItemsHelper22 v  = addelnumbers (creatZeroFreqList v []) v

addelnumbers [] l =[]
addelnumbers (x:xs) l = [(addelnumbersH x l)]  ++ addelnumbers xs l

addelnumbersH (a,b) [] = (a,b)
addelnumbersH  (s,n) (y:ys)  |elem s y =  addelnumbersH ((s),(n+length y-1)) ys
                             |otherwise= addelnumbersH (s,n) ys

---------------------------------------------------------
freqListCart:: String ->[String] -> [(String, Int)]

freqListCart user cart = freqlistcarthelperd(getsTheListIwant user purchasesHistory cart)
freqlistcarthelperd []=[]
freqlistcarthelperd  ((i,n):xs) = if n == 0 then freqlistcarthelperd xs
                                            else (i,n):freqlistcarthelperd xs


getsTheListIwant user ((x,a):xs) cart
                                      |x==user = freqListCarthelper a cart (creatZeroFreqList a [])      --gets the list of the required user
                                      |otherwise =getsTheListIwant user xs cart
freqListCarthelper a b []=[]
freqListCarthelper a cart ((i,n):xs) = [(freqListCarthelperhelper a cart (i,n))]++freqListCarthelper a cart  xs



freqListCarthelperhelper y [] (a,b)= (a,b)
freqListCarthelperhelper (y:ys) (x:xs) (i,n)= freqListCarthelperhelper (y:ys) xs (freqListCarthelperhelper3 (y:ys) x (i,n)) --checks for elemnt 1 and 2 in purchase history


freqListCarthelperhelper3 [] x (a,b) =(a,b)
freqListCarthelperhelper3 (y:ys) x (i,n)=if (elem i y) && (elem x y) && x/=i then freqListCarthelperhelper3 ys x (i,n+1)
                                          else freqListCarthelperhelper3 ys x (i,n)



-- --------------------------------------------------------

freqListCartAndItems:: String -> [String] -> [(String, Int)]
freqListCartAndItems user cart = helperA (freqListCart user cart) (freqListItems user)
helperA [] y=[]
helperA (x:xs) y = [helper x y]++helperA xs y
helper (a,b)[]=(a,b)
helper (l1,n1)((l2,n2):ys)=  if l1==l2 then (l1,(n1+n2))
                                    else helper (l1,n1) ys





-----------------------------------------------------------------------
--all user state
getAllUsersStats :: [(String, [[String]])] -> [(String, [(String, [(String, Int)])])]

getAllUsersStats []=[]
getAllUsersStats ((user,list):ys)= (user,getlistofitems list items): getAllUsersStats ys

getlistofitems l []=[]
getlistofitems listofItemsuser (y:ys)= [(y,(get_list_of_thingsHelper  y listofItemsuser))]++ (getlistofitems listofItemsuser ys)




--this gets the correct list
get_list_of_thingsHelper item l = getWithoutDup (get_list_of_things item l) []


get_list_of_things item []=[]
get_list_of_things  item (y:ys) |(check_presence item y) = (get_list_of_thingsh item (get_list_without_item item y))++(get_list_of_things item ys)
                                |otherwise =(get_list_of_things  item ys)
-- l is the correct out put , (y:ys) is the input




getWithoutDup [] l=l
getWithoutDup ((i,z):ys) l = if (elemBta3tna i l) then getWithoutDup ys (update (i,z) l)
                         else getWithoutDup ys (l++[(i,z)])



update (i,z) ((x,n):xs)	= if(i==x) then ((x,n+z):xs)
                          else (x,n):(update(i,z) xs)


get_list_of_user user = get_list_of_userh user purchasesHistory
get_list_of_userh user ((x,n):xs)=if user ==x then n
                                               else get_list_of_userh user xs

get_list_of_thingsh item []=[]
get_list_of_thingsh item ((i,n):xs) = [(i,n+1)]++get_list_of_thingsh item xs



get_list_without_item item []=[]
get_list_without_item item (x:xs) = if item /=x then [(x,0)]++get_list_without_item item xs
                                               else get_list_without_item item xs


--loop [] a=[]
--loop (x:xs) a	=(get_list_of_things x a)++ (loop xs a)

check_presence item []=False
check_presence item (x:xs)= if x==item then True
                                       else check_presence item xs

--------------------------------------------------------------------------
--get the user 
--the userinfo is the input of the method I'm implementing 
--outPut=getAllUsersStats purchasesHistory


--user = purchasesIntersection2Helper userInfo outPut

get  = getAllUsersStats purchasesHistory

purchasesIntersection (userInfo) list = purchasesIntersection2Finale ((purchasesIntersection2Helper userInfo get),userInfo) list 


purchasesIntersection2Helper userInfo [] =error "user not found"

purchasesIntersection2Helper userInfo ((user,l):xs) = if (userInfo == l) then user
                                                    else purchasesIntersection2Helper userInfo xs




purchasesIntersection2Finale x [] =[]									
purchasesIntersection2Finale (user,info)	((userx,infox):xs)	=	(getUnionOfItems (user,info) (userx,infox) (purchasesIntersection2 (user,(getHistory user purchasesHistory)) (userx,(getHistory userx purchasesHistory)))):	purchasesIntersection2Finale (user,info) xs 
													
													
getHistory user [] =[]													
getHistory user ((userx,items):xs) =if (user==userx) then items 
                                    else getHistory user xs  													
													
													
--interscetion of items  	"I get the infor from purchasesHistory "	
--how to get the user?	
--userinfor is the first of the input pair
--purchasesIntersection userInfo allUsersList = purchasesIntersection2 ((purchasesIntersection2Helper userInfo outPut),getHistory (purchasesIntersection2Helper userInfo outPut) purchasesHistory )) purchasesHistory 
		
	
purchasesIntersection2 (user,history) (userx,list) = getIntersection (withoutDup2(foldr (++) [] history) []) (withoutDup2(foldr (++) [] list) []) 
                                                         


														 

														 
 --takses the user list and the other user's history and gets the interscetion of items from purchasesHistory of the other user  
 --"item4","iem2"
getIntersection  [] x=[]
getIntersection (x:xs) otherUserHistory = if (elem x otherUserHistory) then x:getIntersection xs otherUserHistory
                                           else getIntersection xs otherUserHistory


getAllintersectionuser item []=[]
getAllintersectionuser item ((item1,item1List):xs) =  if(item==item1) then item1List 
                              else getAllintersectionuser item xs			
							  

 
										   
--assuming I have user1 user2 intersection
--getUnionOfItems x[] l=l
--getUnionOfItems [] x =[]


getUnionOfItems x y [] =[]
getUnionOfItems (user1,user1Info) (user2,user2Info) (x:xs )= (x,getWithoutDup((getAllintersectionuser x user1Info)++(getAllintersectionuser x user2Info)) []): getUnionOfItems (user1,user1Info) (user2,user2Info) xs	


withoutDup2 [] l=l
withoutDup2 (x:xs) l= if (elem x l) then withoutDup2 xs l
                     else withoutDup2 xs (l++[x])
---------------------------------------------------------------------------------------------					 


randomZeroToX :: Int -> Int
randomZeroToX x= unsafePerformIO (getStdRandom (randomR (0, x)))									


------------------------------------------------------------


recommendBasedOnItemsInCart :: String -> [String] -> String

recommendBasedOnItemsInCart user cart = akhtar1 user cart  !! (randomZeroToX (length(akhtar1 user cart)-1))

akhtar1 user cart =akhtarhelper (freqListCartAndItems user cart)



-----------------------------------------------------------------------------------
recommendEmptyCart :: String -> String
recommendEmptyCart user= if length(akhtar user)==0 then ""  else  akhtar user !! (randomZeroToX (length(akhtar user)-1))

akhtar user = akhtarhelper (freqListItems user)

akhtarhelper []=[]
akhtarhelper ((item,n):xs)= (akhtarhelperhelper item n 0 )++akhtarhelper xs



akhtarhelperhelper item n z = if (n==z) then [] else [item]++ (akhtarhelperhelper item n (z+1))



-----------------------------------------------------------------------------------
recommendBasedOnUsers :: String -> String
recommendBasedOnUsers user =akhtar2 user !! (randomZeroToX (length(akhtar2 user)-1))

akhtar2 user = akhtarhelper(freqListUsers user)

-----------------------------------------------------------------------
recommend user []=[recommendEmptyCart user,recommendBasedOnUsers user] !! (randomZeroToX 1)

recommend user cart=[recommendBasedOnItemsInCart user cart,recommendBasedOnUsers user] !! (randomZeroToX 1)



-----------------------------------------------------------------------
freqListUsers user = getWithoutDup (append (purchasesIntersection (getthestateoftheuser user (getAllUsersStats purchasesHistory)) (removetheuser user (getAllUsersStats purchasesHistory)))) []

append []=[]
append (x:xs)= append1 x ++ append xs



append1 []=[]
append1 ((name,n):xs)= n++append1 xs

getthestateoftheuser user [] = error "the user is not there"
getthestateoftheuser user ((name,list):xs)= if name==user then list else getthestateoftheuser user xs

 
removetheuser user []=[]
removetheuser user ((name,list):xs)= if name==user then xs else [(name,list)] ++ removetheuser user xs 
















