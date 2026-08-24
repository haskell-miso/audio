-------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
-------------------------------------------------------------------------------
module Model where
-------------------------------------------------------------------------------
import Data.Map as Map (fromList, Map)
-------------------------------------------------------------------------------
import Miso.Lens.TH (makeLenses)
import Miso.String (MisoString, ms)
-------------------------------------------------------------------------------
-- SongId
-------------------------------------------------------------------------------
-- | Position of a song in the playlist (1-based)
newtype SongId = SongId { _songIx :: Int }
  deriving (Eq, Ord)
-------------------------------------------------------------------------------
makeLenses ''SongId
-------------------------------------------------------------------------------
-- | DOM id of the song's @\<audio\>@ element
songDomId :: SongId -> MisoString
songDomId (SongId ix) = "audio-" <> ms ix
-------------------------------------------------------------------------------
-- Song
-------------------------------------------------------------------------------
data Song = Song
  { _songArtist   :: MisoString
  , _songTitle    :: MisoString
  , _songUrl      :: MisoString
  , _songDuration :: Maybe Double -- ^ in seconds, read from the media element
  }
  deriving (Eq)
-------------------------------------------------------------------------------
makeLenses ''Song
-------------------------------------------------------------------------------
-- Status
-------------------------------------------------------------------------------
data Status = Stopped | Paused | Playing
  deriving (Eq)
-------------------------------------------------------------------------------
-- Skin
-------------------------------------------------------------------------------
data Skin = Classic | Obsidian | Gold | Ice
  deriving (Eq, Ord, Enum, Bounded)
-------------------------------------------------------------------------------
skinName :: Skin -> MisoString
skinName Classic  = "classic"
skinName Obsidian = "obsidian"
skinName Gold     = "gold"
skinName Ice      = "ice"
-------------------------------------------------------------------------------
-- Model
-------------------------------------------------------------------------------
data Model = Model
  { _modelCurrent :: Maybe SongId -- ^ song loaded in the deck, if any
  , _modelStatus  :: Status
  , _modelSkin    :: Skin
  , _modelVolume  :: Double       -- ^ 0.0 to 1.0
  , _modelTime    :: Double       -- ^ playback position in seconds
  , _modelSongs   :: Map SongId Song
  } deriving (Eq)
-------------------------------------------------------------------------------
makeLenses ''Model
-------------------------------------------------------------------------------
mkModel :: [(MisoString, MisoString, MisoString)] -> Model
mkModel tracks = Model Nothing Stopped Classic 0.8 0 songs
  where
    songs = Map.fromList
      [ (SongId ix, Song artist title url Nothing)
      | (ix, (artist, title, url)) <- zip [1..] tracks
      ]
-------------------------------------------------------------------------------
