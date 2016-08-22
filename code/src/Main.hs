-- Code obtained from http://www.yesodweb.com/book/persistent

{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
import           Control.Monad.IO.Class  (liftIO)
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Artist
    Id sql=ArtistId
    name String
    deriving Show
Album
    Id sql=AlbumId
    title String
    artistId ArtistId sql=ArtistId
    deriving Show
|]


printAlbum artist = do
  albums <- selectList [AlbumArtistId ==. artist] [LimitTo 10]
  liftIO $ print albums

main :: IO ()
main = runSqlite "Chinook_Sqlite.sqlite" $ do
  artists <- selectList ([] :: [Filter Artist]) [LimitTo 10]
  liftIO $ print artists
  mapM_ (printAlbum . entityKey) $ artists
