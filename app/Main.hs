module Main where


import Control.Concurrent
import Control.Monad (when)
import Control.Monad.Trans.Maybe
import Data.Char (generalCategory, isAlpha, isAlphaNum, isPunctuation, isSymbol,isUpper, isLower)
import Data.Data (typeOf)
import Data.Functor.Identity (Identity (runIdentity))
import Data.List (elemIndex, find, intersperse, isInfixOf)
import Data.List.Split
import GHC.IO.Handle
import System.Directory
import System.IO
import Text.Read (lift, readMaybe)
import GHC.Base (undefined)
import Data.Char (isUpper, isLower)


data ProjAdmin = ProjAdmin{ projname :: String,
                 animator :: String, projstatus :: String} deriving (Show, Eq, Read)

dbsource :: String
dbsource = "dbanimproj.txt"

getField :: Monad m => [String] -> m ProjAdmin
getField [n, a, s] = return ProjAdmin {projname = n, animator = a, projstatus = s}

getProjname :: ProjAdmin -> String
getProjname ProjAdmin {projname = x} = x

getAnim :: ProjAdmin -> String
getAnim ProjAdmin {animator = x} = x

getStatus :: ProjAdmin -> String
getStatus ProjAdmin {projstatus = x} = x

 ----- Login module
data LoginData = LoginData { username :: String, password :: String } deriving (Show, Read)

parse :: String -> [LoginData]
parse txt = let rows = lines txt
            in map read rows

filterProj :: String->String->[LoginData]->[LoginData]
filterProj usr pwd = filter(\p -> username p == usr && password p== pwd )

readUserName :: MaybeT IO String
readUserName = MaybeT $ do
    putStrLn "Enter Username: "
    str <- getLine
    if length str > 5
        then return $ Just str
        else return Nothing


readPassword :: MaybeT IO String
readPassword = MaybeT $ do
    putStrLn "Enter Password"
    str <- getLine
    if length str < 8 || null(filter isUpper str) || null(filter isLower str)
        then return Nothing
        else return $ Just str

changeToString :: LoginData -> String
changeToString (LoginData username password) = unwords [username, password]

loginToMain :: String->String->IO()
loginToMain u p = do

                    let adminDB = "dataadmin.txt"
                    contents <- readFile adminDB
                    let listedData = parse contents
                    let output = map changeToString(filterProj u p listedData)
                    if null output then do
                                         putStrLn "Your data not exist!!"
                                         dataValidate
                      else listOfMenu

dataValidate :: IO()
dataValidate = do
                putStrLn "==========================="
                putStrLn "=========  LOGIN  ========="
                putStrLn "==========================="
                maybeLogValid <- runMaybeT $ do
                  usrnm <- readUserName
                  psspwd <- readPassword
                  return (usrnm, psspwd)
                case maybeLogValid of
                  Nothing -> do
                              putStrLn "Your input was wrong!"
                              main
                  Just (u,p) -> loginToMain u p

----------------------------------------------------------------------------------
----------------------------------------------------------------------------------
listOfMenu :: IO()
listOfMenu  = do
              putStrLn "==========================================="
              putStrLn "               Welcome To                  "
              putStrLn "      Animator Project Administration      "
              putStrLn "==========================================="
              hSetBuffering stdout NoBuffering
              putStrLn "Select Menu:"
              putStrLn "==========================================="
              putStrLn "[c] Add New Project"
              putStrLn "[r] View All Data"
              putStrLn "[u] Edit Project Data"
              putStrLn "[d] Delete Project Data"
              putStrLn "[x] Exit"
              putStrLn "========================="
              putStr "Input Your Menu Selection: "
              choicemenu <- getLine
              putStrLn "========================="
              case choicemenu of
                "c" -> do
                        putStrLn ""
                        addData
                        listOfMenu
                "r" -> do
                       putStrLn ""
                       mainPrint
                       listOfMenu
                "u" -> do
                       putStrLn ""
                       editData
                       listOfMenu
                "d" -> do
                       putStrLn ""
                       deleteData
                "x" -> putStrLn "Thank you!!"
                _ ->   listOfMenu


main :: IO()
main = do
        putStrLn "=========================================="
        putStrLn "Choose [L] for Login or [X] Exit:"
        putStrLn "=========================================="
        chooseLogin <-getLine
        case chooseLogin of
          "L"-> dataValidate
          "X"-> putStrLn "See you again !"


--- Add New Project Function

addData :: IO()
addData = do
              putStrLn "=========================================="
              putStrLn "=========  ADD NEW DATA PROJECT  ========="
              putStrLn "=========================================="
              putStr "Project Name: "
              prjname <- getLine
              putStr "Animator Name:  "
              animname <- getLine
              putStr "Project Status: "
              projstat <- getLine
              if null prjname || null animname || null projstat
                  then do
                      putStrLn "Please Input All Data!"
                      addData
              else if
                  projstat `notElem` ["done","ongoing"] then do
                      putStrLn "Input Project Status only done or ongoing!"
                      addData
                  else do

                    fileAppend <- openFile dbsource AppendMode
                    let newinput = ProjAdmin {projname = prjname,
                                            animator = animname, projstatus = projstat}
                    let newinputdata = prjname ++ "," ++ animname ++ "," ++ projstat
                    hPutStrLn fileAppend newinputdata
                    putStrLn "==================================================================="
                    putStrLn "==================================================================="
                    print newinput
                    putStrLn "==================================================================="
                    putStrLn "Data Saved!"
                    putStrLn "==================================================================="
                    hClose fileAppend

--- editData Function

editData :: IO ()
editData = do
  mainPrint
  putStrLn "========================================="
  putStrLn "========  UPDATE PROJECT STATUS  ========"
  putStrLn "========================================="
  putStrLn "==================================================================="
  putStr   "Input Project Name to update: "
  prjnm <- getLine
  putStrLn "==================================================================="
  fileOpen <- openFile dbsource ReadMode
  text <- hGetContents fileOpen
  let list = lines text
  let filtered = find (isInfixOf (prjnm ++ ",")) list
  case filtered of
    Just value -> do
      hClose fileOpen
      editProcess value prjnm
    Nothing -> do
      putStrLn "There's no data, check again\n"
      hClose fileOpen
      listOfMenu

editProcess :: String -> String -> IO ()
editProcess value targetName = do
  let datafound = splitOn "," value  --value disini adalah satu record data berdasarkan nama yang di cari
  getData <- getField datafound
  let prjs = "done"
  putStrLn "Processing Data\n"
  let newData = getData{projstatus = prjs}
  renewData newData targetName

renewData :: ProjAdmin -> String -> IO ()
renewData newData targetName = do
  fileOpen <- openFile dbsource ReadMode
  text <- hGetContents fileOpen
  let theHead = head (lines text) ++ "\n"
  let list = tail $ lines text
  let filtered = filter (\x -> not (isInfixOf (targetName ++ ",") x)) list
  let newLine = (getProjname  newData) ++ "," ++ (getAnim newData) ++ "," ++ (getStatus newData)
  let newList = filtered ++ [newLine]
  let newInput = theHead ++ concat (intersperse "\n" newList)
  putStrLn "======================="
  print newLine
  putStrLn "======================="
  when (length newInput > 0) $
    dataUpdated newInput newData

dataUpdated :: Show a => String -> a -> IO ()
dataUpdated newInput newData = do
  fileOpen <- openFile dbsource WriteMode
  hPutStrLn fileOpen newInput
  putStrLn "======================="
  putStrLn "    Data Updated!!     "
  putStrLn "======================="
  hClose fileOpen

-- Function View Data

viewData :: IO()
viewData = do
            view <- readFile "dbanimproj.txt"
            putStrLn view

deleteData :: IO()
deleteData = do
              viewData
              putStrLn "========================================="
              putStrLn "========  DELETE A PROJECT DATA  ========"
              putStrLn "========================================="
              putStr "Input Project Name to delete: "
              prjnm <- getLine
              putStrLn "========================================="
              fileOpen <- openFile dbsource ReadMode
              text <- hGetContents fileOpen
              let theHead = (head $ lines text) ++ "\n"
              let line = tail $ lines text
              let filtered = filter (\x -> (isInfixOf (prjnm ++ ",") x) == False) line
              let deleted = find (\x -> isInfixOf (prjnm ++ ",") x) line
              let newData = theHead ++ concat (intersperse "\n" filtered)
              if (length filtered == length line)
                then do
                  putStrLn "No data changed\n"
                  listOfMenu
                  hClose fileOpen
                else do
                  hClose fileOpen
                  when (length newData > 0) $
                    dataDeleted newData deleted

dataDeleted :: String -> Maybe [Char] -> IO ()
dataDeleted newData (Just deleted) = do
  fileOpen <- openFile dbsource WriteMode
  let datafound = splitOn "," deleted
  deletedData <- getField datafound
  hPutStrLn fileOpen newData
  putStrLn "Success Delete Data\n"
  hClose fileOpe
  listOfMenu


-- PRINT/READ DATA FUNCTIONS
mainPrint ::IO()
mainPrint = do
            printHeader
            printField

printHeader :: IO ()
printHeader = do
  putStrLn "========================================================================"
  putStrLn "||      Project Name       ||   Animator Name   ||       Status       ||"
  putStrLn "========================================================================"

printField :: IO ()
printField = do
  fileOpen <- openFile dbsource ReadMode
  text <- hGetContents fileOpen
  let line = tail $ lines text
  mapM_ (getTheField) line
  hClose fileOpen
  bellowBorder

getTheField :: [Char] -> IO ()
getTheField x = do
  let part = splitOn "," x
  printFieldRecord part

dataRecord :: Monad m => Int -> [Char] -> m [Char]
dataRecord num value = return $ " " ++ value ++ concat (replicate (num - length value) " ") ++ "||"

printFieldRecord :: [[Char]] -> IO ()
printFieldRecord [n, a, s] = do
  projname <- dataRecord 24 n
  animator <- dataRecord 18 a
  projstatus <- dataRecord 19 s
  putStrLn $ "||" ++ projname ++ animator ++ projstatus

bellowBorder :: IO ()
bellowBorder = do
  putStrLn "--------------------------------------------------------------------------------------------------------------------------------------------------------\n"
