{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Data.Maybe
import Text.Read (readMaybe)
import Data.List
import Data.Ord

import Network.CGI
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow()
import Text.XHtml

data Oseba = Oseba
  { id :: Int
  , ime :: String
  , priimek :: String
  , telefon :: String
  } deriving (Show, Eq)

instance FromRow Oseba where
  fromRow = Oseba <$> field <*> field <*> field <*> field

exe_path = "/cgi-bin/imenik"
db_path = "imenik.db"

imenik :: CGI CGIResult
imenik = do
  conn <- liftIO $ open db_path

  param_zahteva <- fromMaybe "" <$> getInput "zahteva"

  case param_zahteva of

    "spremeni_form" -> do
      param_id <- fromMaybe "" <$> getInput "id"
      case (readMaybe param_id) :: Maybe Int of
        Nothing -> fail "neuporaben id"
        Just n  -> do
          os <- liftIO $ query conn "select * from imenik where id = ?" (Only (n :: Int)) :: CGI [Oseba]
          case length os of
            1 -> output $ showHtml $ spremeni_form (head os)
            _ -> fail "neuporaben id"
    
    --------------
    -- spremeni --
    --------------
    "spremeni" -> do
      param_id <- fromMaybe "" <$> getInput "id"
      case (readMaybe param_id) :: Maybe Int of
        Nothing -> fail "neuporaben id"
        Just n  -> do
          os <- liftIO $ query conn "select * from imenik where id = ?" (Only (n :: Int)) :: CGI [Oseba]
          case length os of
            1 -> do
              param_ime <- fromMaybe "" <$> getInput "ime"
              param_priimek <- fromMaybe "" <$> getInput "priimek"
              param_telefon <- fromMaybe "" <$> getInput "telefon"
              liftIO $ execute conn "update imenik set ime = ?, priimek = ?, telefon = ? where id = ?" (
                                                                      if null param_ime then "(prazno)"
                                                                                        else param_ime,
                                                                      if null param_priimek then "(prazno)"
                                                                                            else param_priimek,
                                                                      if null param_telefon then "(prazno)"
                                                                                            else param_telefon,
                                                                      n)
              output $ showHtml $ p (toHtml ("spremenjeno!" :: String))
            _ -> fail "neuporaben id"

    -----------
    -- dodaj --
    -----------
    "dodaj" -> do
      os <- liftIO $ query_ conn "SELECT * from imenik" :: CGI [Oseba]
      let naslednji_id = if null os then 1 else (1 + Main.id (last (sortOsebe os)))
      param_ime <- fromMaybe "" <$> getInput "ime"
      param_priimek <- fromMaybe "" <$> getInput "priimek"
      param_telefon <- fromMaybe "" <$> getInput "telefon"
      liftIO $ execute conn "insert into imenik values (?, ?, ?, ?)" (naslednji_id,
                                                                      if null param_ime then "(prazno)"
                                                                                        else param_ime,
                                                                      if null param_priimek then "(prazno)"
                                                                                            else param_priimek,
                                                                      if null param_telefon then "(prazno)"
                                                                                            else param_telefon)
      output $ showHtml $ p (toHtml ("dodano!" :: String))

    -----------
    -- brisi --
    -----------
    "brisi" -> do
      param_id <- fromMaybe "" <$> getInput "id"
      case readMaybe param_id of
        Nothing -> fail "neuporaben id"
        Just n  -> do
          liftIO $ execute conn "delete from imenik where id = ?" (Only (n :: Int))
          output $ showHtml $ p (toHtml ("izbrisano!" :: String))

    ----------
    -- isci --
    ----------
    "isci" -> do
      param_q <- getInput "q"
      case param_q of
        Nothing -> fail "prazna iskalna beseda"
        Just s -> do
          os <- liftIO $ query conn "SELECT * from imenik where ime = ? or priimek = ? or telefon = ?" (s, s, s) :: CGI [Oseba]
          case length os of
            0 -> output $ showHtml $ p (toHtml ("prazen rezultat" :: String))
            _ -> output $ showHtml $ p (toHtml (prikazOseb os))

    -------------
    -- prikazi --
    -------------
    _ -> do
        os <- liftIO $ query_ conn "SELECT * from imenik" :: CGI [Oseba]
        output $ showHtml $ h1 (toHtml ("Interni imenik" :: String))
                            +++
                            hr
                            +++
                            (if null os then (toHtml ("prazen imenik" :: String)) else (prikazOseb os))
                            +++
                            hr
                            +++
                            isci_form
                            +++
                            dodaj_form

isci_form :: Html
isci_form = form (
            p (toHtml ("isci" :: String))
            +++
            hidden "zahteva" "isci"
            +++
            textfield "q"
            +++
            submit "send" "submit") ! [ method "GET"
                                      , enctype "multipart/form-data"]

dodaj_form :: Html
dodaj_form = form (
             p (toHtml ("Dodaj novo osebo" :: String))
             +++
             hidden "zahteva" "dodaj"
             +++
             textfield "ime" ! [value "ime"]
             +++
             textfield "priimek" ! [value "priimek"]
             +++
             textfield "telefon" ! [value "telefon"]
             +++
             submit "send" "submit") ! [ method "GET"
                                       , enctype "multipart/form-data"]

spremeni_form :: Oseba -> Html
spremeni_form (Oseba id ime priimek telefon) = form (
             p (toHtml ("Spremeni podatke" ::String))
             +++
             hidden "zahteva" "spremeni"
             +++
             hidden "id" (show id)
             +++
             textfield "ime" ! [value ime]
             +++
             textfield "priimek" ! [value priimek]
             +++
             textfield "telefon" ! [value telefon]
             +++
             submit "send" "submit") ! [ method "GET"
                                       , enctype "multipart/form-data"]

prikazOsebe :: Oseba -> Html
prikazOsebe (Oseba id ime priimek telefon) =
  p (toHtml ("Ime: " ++ ime ++ " Priimek: " ++ priimek ++ " Telefon: " ++ telefon)
  +++
  toHtml (anchor (toHtml (" < izbrisi" :: String)) ! [href (exe_path ++ "?zahteva=brisi&id=" ++ show id)])
  +++
  toHtml (anchor (toHtml (" < spremeni" :: String)) ! [href (exe_path ++ "?zahteva=spremeni_form&id=" ++ show id)]))

prikazOseb :: [Oseba] -> Html
prikazOseb = foldr1 (+++) . map prikazOsebe

sortOsebe :: [Oseba] -> [Oseba]
sortOsebe = sortBy (comparing Main.id)


main :: IO ()
main = runCGI (handleErrors imenik)
