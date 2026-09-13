----------------------------------------------------------------------
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
----------------------------------------------------------------------
module Main where
----------------------------------------------------------------------
import Control.Applicative ((<|>))
import Control.Monad (forM_, when)
import Data.Map as Map (adjust, lookupGT, lookupLT, lookupMax, lookupMin, toList, (!?))
import Data.Maybe (fromMaybe)
import Data.Time.Clock (DiffTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
----------------------------------------------------------------------
import Miso
import qualified Miso.CSS as CSS
import Miso.Html
import Miso.Html.Property
import Miso.Lens
import Miso.Media (Media(..), currentTime, duration, load, pause, play)
import Miso.String (fromMisoStringEither)
----------------------------------------------------------------------
import Model
----------------------------------------------------------------------
-- | Parameters
thePlaylist :: [(MisoString, MisoString, MisoString)]
thePlaylist =
  [ ("T. Schürger", "SoundHelix Song " <> ms n, soundHelix n)
  | n <- [1 .. 9 :: Int]
  ]
  where
    soundHelix n =
      "https://www.soundhelix.com/examples/mp3/SoundHelix-Song-" <> ms n <> ".mp3"
----------------------------------------------------------------------
-- | Action
data Action
  = ActionPlay SongId
  | ActionResume
  | ActionPause
  | ActionStop
  | ActionPrev
  | ActionNext
  | ActionEnded
  | ActionSeek MisoString
  | ActionVolume MisoString
  | ActionSkin Skin
  | ActionAskTime SongId Media
  | ActionSetTime SongId Double
  | ActionAskDuration SongId Media
  | ActionSetDuration SongId Double
----------------------------------------------------------------------
-- | View
handleView :: Model -> View context props Model Action
handleView model = div_ [ class_ rackClass ]
  [ viewMain model
  , viewSkins model
  , viewPlaylist model
  , footer_ [ class_ "credit" ]
      [ a_ [ href_ "https://github.com/haskell-miso/miso-audio" ] [ "🍜 miso-audio" ] ]
  , vfrag
      [ viewAudio (model ^. modelVolume) sId song
      | (sId, song) <- Map.toList (model ^. modelSongs)
      ]
  ]
  where
    rackClass = "rack skin-" <> skinName (model ^. modelSkin)
      <> (if model ^. modelStatus == Paused then " is-paused" else "")
----------------------------------------------------------------------
-- | One hidden <audio> element per song, driven by the update function
viewAudio :: Double -> SongId -> Song -> View context props Model Action
viewAudio vol sId song = audio_
  [ id_ (songDomId sId)
  , src_ (song ^. songUrl)
  , preload_ "metadata"
  , volume_ vol
  , onEnded ActionEnded
  , onLoadedMetadataWith (ActionAskDuration sId)
  , onTimeUpdateWith (ActionAskTime sId)
  ]
  []
----------------------------------------------------------------------
-- | Beveled window title bar with the classic grooved stripes
viewTitleBar :: Bool -> MisoString -> View context props Model Action
viewTitleBar withButtons caption = header_ [ class_ "titlebar" ] $
  [ span_ [ class_ "stripes" ] []
  , span_ [ class_ "caption" ] [ text caption ]
  , span_ [ class_ "stripes" ] []
  ] <>
  [ span_ [ class_ "winbtns" ] (replicate 3 (span_ [] [])) | withButtons ]
----------------------------------------------------------------------
-- | Main deck: LCD display, seek bar, volume, transport buttons
viewMain :: Model -> View context props Model Action
viewMain model = section_ [ class_ "window" ]
  [ viewTitleBar True "miso amp"
  , viewDisplay model
  , viewSeek model
  , div_ [ class_ "controls" ]
      [ viewTransport model
      , viewVolume model
      ]
  ]
----------------------------------------------------------------------
-- | The black LCD: spectrum bars, big time readout, scrolling track title
viewDisplay :: Model -> View context props Model Action
viewDisplay model = div_ [ class_ "display" ]
  [ div_ [ class_ "lcd-row" ]
      [ div_ [ class_ (if isPlaying then "viz playing" else "viz") ]
          (replicate 14 (span_ [] []))
      , span_ [ class_ "lcd-status" ] [ text statusGlyph ]
      , span_ [ class_ "lcd-time" ] [ text (fmtTime (model ^. modelTime)) ]
      ]
  , div_ [ class_ "marquee" ] [ span_ [] [ text (segment <> segment) ] ]
  , div_ [ class_ "lcd-info" ]
      [ span_ [ class_ "cell" ] [ text trackNo ]
      , span_ [ class_ "cell" ] [ text ("vol " <> fmtPct (model ^. modelVolume)) ]
      , span_ []
          [ span_ [ class_ "dim" ] [ "mono " ]
          , span_ [ class_ (if isPlaying then "lit" else "dim") ] [ "stereo" ]
          ]
      ]
  ]
  where
    isPlaying = model ^. modelStatus == Playing

    statusGlyph = case model ^. modelStatus of
      Playing -> "►"
      Paused  -> "❚❚"
      Stopped -> "■"

    segment = marqueeText <> "  ***  "

    marqueeText = case currentSong model of
      Just (sId, song) -> fmtSong sId song
        <> maybe "" (\d -> " (" <> fmtTime d <> ")") (song ^. songDuration)
      Nothing -> "miso amp - it really whips the llama's ass"

    trackNo = case model ^. modelCurrent of
      Just sId -> "track " <> ms (sId ^. songIx)
        <> "/" <> ms (length (model ^. modelSongs))
      Nothing -> "no track"
----------------------------------------------------------------------
-- | Seek bar over the current song
viewSeek :: Model -> View context props Model Action
viewSeek model = div_ [ class_ "seek" ]
  [ input_
      [ type_ "range"
      , min_ "0"
      , max_ (ms totalSecs)
      , step_ "0.1"
      , value_ (ms (model ^. modelTime))
      , onInput ActionSeek
      ]
  ]
  where
    totalSecs = fromMaybe 0 (currentSong model >>= (^. songDuration) . snd)
----------------------------------------------------------------------
viewTransport :: Model -> View context props Model Action
viewTransport _ = div_ [ class_ "transport" ]
  [ btn "Previous" "◄◄" ActionPrev
  , btn "Play" "►" ActionResume
  , btn "Pause" "❚❚" ActionPause
  , btn "Stop" "■" ActionStop
  , btn "Next" "►►" ActionNext
  ]
  where
    btn name glyph action =
      button_ [ class_ "btn", textProp "title" name, onClick action ] [ text glyph ]
----------------------------------------------------------------------
viewVolume :: Model -> View context props Model Action
viewVolume model = div_ [ class_ "volume" ]
  [ input_
      [ type_ "range"
      , min_ "0"
      , max_ "1"
      , step_ "0.02"
      , value_ (ms (model ^. modelVolume))
      , onInput ActionVolume
      ]
  ]
----------------------------------------------------------------------
-- | Skin selector, where the equalizer would be
viewSkins :: Model -> View context props Model Action
viewSkins model = section_ [ class_ "window" ]
  [ viewTitleBar False "miso skins"
  , div_ [ class_ "skins" ]
      [ chip skin | skin <- [minBound .. maxBound] ]
  ]
  where
    chip skin = button_
      [ class_ (if model ^. modelSkin == skin then "chip active" else "chip")
      , onClick (ActionSkin skin)
      ]
      [ span_ [ class_ "led" ] []
      , text (skinName skin)
      ]
----------------------------------------------------------------------
viewPlaylist :: Model -> View context props Model Action
viewPlaylist model = section_ [ class_ "window" ]
  [ viewTitleBar False "miso amp playlist"
  , ul_ [ class_ "tracks" ]
      [ viewTrack sId song | (sId, song) <- Map.toList (model ^. modelSongs) ]
  , div_ [ class_ "plist-bar" ]
      [ span_ [] [ text (ms (length (model ^. modelSongs)) <> " tracks") ]
      , span_ [] [ text position ]
      ]
  ]
  where
    position = fmtTime (model ^. modelTime) <> " / " <> total

    total = fromMaybe "--:--" $ do
      (_, song) <- currentSong model
      fmtTime <$> song ^. songDuration

    viewTrack sId song = li_
      [ class_ (if model ^. modelCurrent == Just sId then "track current" else "track")
      , onClick (ActionPlay sId)
      ]
      [ span_ [ class_ "track-name" ] [ text (fmtSong sId song) ]
      , span_ [ class_ "track-time" ]
          [ text (maybe "--:--" fmtTime (song ^. songDuration)) ]
      ]
----------------------------------------------------------------------
-- | Formatters
fmtSong :: SongId -> Song -> MisoString
fmtSong sId song = mconcat
  [ ms (sId ^. songIx), ". ", song ^. songArtist, " - ", song ^. songTitle ]

fmtTime :: Double -> MisoString
fmtTime secs =
  ms (formatTime defaultTimeLocale "%02M:%02S" (realToFrac secs :: DiffTime))

fmtPct :: Double -> MisoString
fmtPct x = ms (round (100 * x) :: Int) <> "%"

currentSong :: Model -> Maybe (SongId, Song)
currentSong model = do
  sId <- model ^. modelCurrent
  song <- (model ^. modelSongs) !? sId
  pure (sId, song)
----------------------------------------------------------------------
-- | Update
handleUpdate :: Action -> Effect context props Model Action
handleUpdate = \case
  ActionPlay sId -> do
    -- when switching tracks, rewind the previous song
    mCurrent <- use modelCurrent
    when (mCurrent /= Just sId) $ do
      forM_ mCurrent (io_ . withMedia load)
      modelTime .= 0
    modelCurrent .= Just sId
    modelStatus .= Playing
    io_ (withMedia play sId)
  ActionResume -> use modelStatus >>= \case
    Playing -> pure ()
    _ -> use modelCurrent >>= \case
      -- nothing loaded yet, start at the top of the playlist
      Nothing -> do
        songs <- use modelSongs
        forM_ (Map.lookupMin songs) (issue . ActionPlay . fst)
      Just sId -> do
        modelStatus .= Playing
        io_ (withMedia play sId)
  ActionPause -> use modelStatus >>= \case
    Playing -> do
      mCurrent <- use modelCurrent
      forM_ mCurrent $ \sId -> do
        modelStatus .= Paused
        io_ (withMedia pause sId)
    _ -> pure ()
  ActionStop -> do
    -- reloading rewinds the song back to the beginning
    mCurrent <- use modelCurrent
    forM_ mCurrent (io_ . withMedia load)
    modelStatus .= Stopped
    modelTime .= 0
  ActionPrev -> skipTo Map.lookupLT Map.lookupMax
  ActionNext -> skipTo Map.lookupGT Map.lookupMin
  ActionEnded -> do
    -- advance to the next song, stop at the end of the playlist
    mCurrent <- use modelCurrent
    songs <- use modelSongs
    case flip Map.lookupGT songs =<< mCurrent of
      Just (sId, _) -> issue (ActionPlay sId)
      Nothing -> issue ActionStop
  ActionSeek str ->
    forM_ (fromMisoStringEither str) $ \secs -> do
      mCurrent <- use modelCurrent
      forM_ mCurrent $ \sId -> do
        modelTime .= secs
        io_ $ do
          el <- getElementById (songDomId sId)
          Miso.set "currentTime" (secs :: Double) (Object el)
  ActionVolume str ->
    -- the new volume is applied declaratively via 'volume_'
    forM_ (fromMisoStringEither str) (modelVolume .=)
  ActionSkin skin ->
    modelSkin .= skin
  ActionAskTime sId media -> do
    mCurrent <- use modelCurrent
    when (mCurrent == Just sId) $
      io (ActionSetTime sId <$> currentTime media)
  ActionSetTime sId secs -> do
    mCurrent <- use modelCurrent
    when (mCurrent == Just sId) $
      modelTime .= secs
  ActionAskDuration sId media ->
    io (ActionSetDuration sId <$> duration media)
  ActionSetDuration sId secs ->
    modelSongs %= adjust (songDuration ?~ secs) sId
  where
    withMedia f sId = f . Media =<< getElementById (songDomId sId)

    -- move to a neighboring song, wrapping around the playlist
    skipTo neighbor wrap = do
      songs <- use modelSongs
      mCurrent <- use modelCurrent
      let mNext = (fst <$> (flip neighbor songs =<< mCurrent))
              <|> (fst <$> wrap songs)
      forM_ mNext (issue . ActionPlay)
----------------------------------------------------------------------
-- | Style, a classic Winamp-inspired rack of windows.
--
-- Each skin only redefines the custom properties on @.rack@; every
-- rule below draws from those variables.
theSkin :: CSS.StyleSheet
theSkin = CSS.sheet_
  [ CSS.selector_ "body"
      [ CSS.margin "0"
      , CSS.minHeight "100vh"
      , CSS.display "flex"
      , CSS.flexDirection "column"
      , CSS.alignItems "center"
      , CSS.justifyContent "center"
      , CSS.background "radial-gradient(circle at 50% 25%, #2a2a44 0%, #101018 70%)"
      , CSS.fontFamily "Tahoma, Verdana, 'DejaVu Sans', sans-serif"
      ]
  -- skins
  , CSS.selector_ ".rack" (rackLayout <> classicSkin)
  , CSS.selector_ ".rack.skin-obsidian" obsidianSkin
  , CSS.selector_ ".rack.skin-gold" goldSkin
  , CSS.selector_ ".rack.skin-ice" iceSkin
  -- windows
  , CSS.selector_ ".window"
      [ CSS.backgroundColor (CSS.var "metal")
      , CSS.border "2px outset var(--bevel-hi)"
      , CSS.boxShadow "0 12px 32px rgba(0,0,0,0.55)"
      ]
  -- title bars
  , CSS.selector_ ".titlebar"
      [ CSS.display "flex"
      , CSS.alignItems "center"
      , CSS.gap (CSS.px 6)
      , CSS.padding "2px 6px"
      , CSS.background "var(--chrome)"
      , CSS.color (CSS.var "text")
      , CSS.fontSize (CSS.px 9)
      , CSS.fontWeight "bold"
      , CSS.letterSpacing (CSS.px 3)
      , CSS.textTransform "uppercase"
      , CSS.textShadow "1px 1px 0 rgba(0,0,0,0.35)"
      ]
  , CSS.selector_ ".titlebar .stripes"
      [ CSS.flex "1"
      , CSS.height (CSS.px 8)
      , CSS.background
          "repeating-linear-gradient(to bottom, var(--stripe) 0 1px, transparent 1px 3px)"
      ]
  , CSS.selector_ ".titlebar .winbtns"
      [ CSS.display "flex"
      , CSS.gap (CSS.px 2)
      ]
  , CSS.selector_ ".titlebar .winbtns span"
      [ CSS.width (CSS.px 9)
      , CSS.height (CSS.px 9)
      , CSS.background "var(--face)"
      , CSS.border "1px outset var(--bevel-hi)"
      ]
  -- LCD display
  , CSS.selector_ ".display"
      [ CSS.position "relative"
      , CSS.margin (CSS.px 8)
      , CSS.marginBottom (CSS.px 4)
      , CSS.padding "6px 8px"
      , CSS.backgroundColor (CSS.hex "000000")
      , CSS.border "2px inset var(--bevel-hi)"
      , CSS.color (CSS.var "lcd")
      ]
  , CSS.selector_ ".display::after"
      [ "content" =: "''"
      , CSS.position "absolute"
      , "inset" =: "0"
      , CSS.background
          "repeating-linear-gradient(to bottom, transparent 0 2px, rgba(0,0,0,0.22) 2px 3px)"
      , CSS.pointerEvents "none"
      ]
  , CSS.selector_ ".lcd-row"
      [ CSS.display "flex"
      , CSS.alignItems "center"
      , CSS.gap (CSS.px 10)
      ]
  , CSS.selector_ ".lcd-status"
      [ CSS.fontSize (CSS.px 12)
      , CSS.marginLeft "auto"
      ]
  , CSS.selector_ ".lcd-time"
      [ CSS.fontFamily "'Courier New', 'DejaVu Sans Mono', monospace"
      , CSS.fontSize (CSS.px 30)
      , CSS.fontWeight "bold"
      , CSS.letterSpacing (CSS.px 2)
      , CSS.textShadow "0 0 8px var(--glow)"
      ]
  , CSS.selector_ ".rack.is-paused .lcd-time"
      [ CSS.animation "blink 1.2s linear infinite" ]
  , CSS.keyframes_ "blink"
      [ CSS.at "0%, 49%" [ CSS.opacity 1 ]
      , CSS.at "50%, 100%" [ CSS.opacity 0 ]
      ]
  , CSS.selector_ ".viz"
      [ CSS.display "flex"
      , CSS.alignItems "flex-end"
      , CSS.gap (CSS.px 2)
      , CSS.height (CSS.px 28)
      , CSS.width (CSS.px 110)
      ]
  , CSS.selector_ ".viz span"
      [ CSS.flex "1"
      , CSS.height (CSS.pct 12)
      , CSS.background "var(--viz)"
      ]
  , CSS.selector_ ".viz.playing span"
      [ CSS.animation "bounce 460ms ease-in-out infinite alternate" ]
  , CSS.selector_ ".viz.playing span:nth-child(2n)"
      [ CSS.animationDuration "340ms", CSS.animationDelay "80ms" ]
  , CSS.selector_ ".viz.playing span:nth-child(3n)"
      [ CSS.animationDuration "520ms", CSS.animationDelay "140ms" ]
  , CSS.selector_ ".viz.playing span:nth-child(5n)"
      [ CSS.animationDuration "300ms", CSS.animationDelay "40ms" ]
  , CSS.keyframes_ "bounce"
      [ CSS.from_ [ CSS.height (CSS.pct 12) ]
      , CSS.to_ [ CSS.height (CSS.pct 100) ]
      ]
  , CSS.selector_ ".marquee"
      [ CSS.overflow "hidden"
      , CSS.whiteSpace "nowrap"
      , CSS.marginTop (CSS.px 6)
      , CSS.fontSize (CSS.px 10)
      , CSS.letterSpacing (CSS.px 1)
      , CSS.textTransform "uppercase"
      ]
  , CSS.selector_ ".marquee span"
      [ CSS.display "inline-block"
      , CSS.animation "marquee 16s linear infinite"
      ]
  , CSS.keyframes_ "marquee"
      [ CSS.from_ [ "transform" =: "translateX(0)" ]
      , CSS.to_ [ "transform" =: "translateX(-50%)" ]
      ]
  , CSS.selector_ ".lcd-info"
      [ CSS.display "flex"
      , CSS.alignItems "center"
      , CSS.justifyContent "space-between"
      , CSS.marginTop (CSS.px 6)
      , CSS.fontSize (CSS.px 9)
      , CSS.letterSpacing (CSS.px 1)
      , CSS.textTransform "uppercase"
      ]
  , CSS.selector_ ".lcd-info .cell"
      [ CSS.padding "1px 5px"
      , CSS.border "1px inset var(--bevel-hi)"
      ]
  , CSS.selector_ ".lcd-info .dim"
      [ CSS.color (CSS.var "lcd-dim")
      , CSS.opacity 0.6
      ]
  , CSS.selector_ ".lcd-info .lit"
      [ CSS.textShadow "0 0 6px var(--glow)" ]
  -- sliders
  , CSS.selector_ ".seek"
      [ CSS.margin "0 8px" ]
  , CSS.selector_ ".seek input, .volume input"
      [ "-webkit-appearance" =: "none"
      , CSS.appearance "none"
      , CSS.display "block"
      , CSS.width (CSS.pct 100)
      , CSS.height (CSS.px 10)
      , CSS.background "linear-gradient(rgba(0,0,0,0.65), rgba(0,0,0,0.35))"
      , CSS.border "1px inset var(--bevel-hi)"
      , CSS.borderRadius (CSS.px 2)
      ]
  , CSS.selector_ ".volume input"
      [ CSS.background
          "linear-gradient(to right, #00C800, #E8E800 55%, #E80000)"
      ]
  , CSS.selector_ ".seek input::-webkit-slider-thumb, .volume input::-webkit-slider-thumb"
      [ "-webkit-appearance" =: "none"
      , CSS.width (CSS.px 24)
      , CSS.height (CSS.px 8)
      , CSS.background "var(--thumb)"
      , CSS.border "1px outset var(--thumb-edge)"
      , CSS.borderRadius (CSS.px 1)
      , CSS.cursor "pointer"
      ]
  , CSS.selector_ ".seek input::-moz-range-thumb, .volume input::-moz-range-thumb"
      [ CSS.width (CSS.px 24)
      , CSS.height (CSS.px 8)
      , CSS.background "var(--thumb)"
      , CSS.border "1px outset var(--thumb-edge)"
      , CSS.borderRadius (CSS.px 1)
      , CSS.cursor "pointer"
      ]
  -- transport
  , CSS.selector_ ".controls"
      [ CSS.display "flex"
      , CSS.alignItems "center"
      , CSS.gap (CSS.px 8)
      , CSS.margin (CSS.px 8)
      ]
  , CSS.selector_ ".transport"
      [ CSS.display "flex"
      , CSS.gap (CSS.px 2)
      ]
  , CSS.selector_ ".btn"
      [ CSS.width (CSS.px 30)
      , CSS.height (CSS.px 20)
      , CSS.padding "0"
      , CSS.background "var(--face)"
      , CSS.border "2px outset var(--bevel-hi)"
      , CSS.color (CSS.var "text")
      , CSS.fontSize (CSS.px 9)
      , CSS.cursor "pointer"
      ]
  , CSS.selector_ ".btn:active"
      [ CSS.border "2px inset var(--bevel-hi)"
      , CSS.background "var(--face-down)"
      ]
  , CSS.selector_ ".volume"
      [ CSS.flex "1" ]
  -- skin selector
  , CSS.selector_ ".skins"
      [ CSS.display "flex"
      , CSS.gap (CSS.px 4)
      , CSS.padding (CSS.px 8)
      ]
  , CSS.selector_ ".chip"
      [ CSS.flex "1"
      , CSS.display "flex"
      , CSS.alignItems "center"
      , CSS.justifyContent "center"
      , CSS.gap (CSS.px 5)
      , CSS.height (CSS.px 22)
      , CSS.background "var(--face)"
      , CSS.border "2px outset var(--bevel-hi)"
      , CSS.color (CSS.var "text")
      , CSS.fontSize (CSS.px 9)
      , CSS.textTransform "uppercase"
      , CSS.letterSpacing (CSS.px 1)
      , CSS.cursor "pointer"
      ]
  , CSS.selector_ ".chip.active"
      [ CSS.border "2px inset var(--bevel-hi)"
      , CSS.background "var(--face-down)"
      ]
  , CSS.selector_ ".chip .led"
      [ CSS.width (CSS.px 6)
      , CSS.height (CSS.px 6)
      , CSS.borderRadius (CSS.pct 50)
      , CSS.background "rgba(0,0,0,0.55)"
      ]
  , CSS.selector_ ".chip.active .led"
      [ CSS.background "var(--lcd)"
      , CSS.boxShadow "0 0 5px var(--glow)"
      ]
  -- playlist
  , CSS.selector_ ".tracks"
      [ "list-style" =: "none"
      , CSS.margin (CSS.px 8)
      , CSS.marginBottom (CSS.px 4)
      , CSS.padding (CSS.px 4)
      , CSS.backgroundColor (CSS.hex "000000")
      , CSS.border "2px inset var(--bevel-hi)"
      , CSS.height (CSS.px 148)
      , CSS.overflow "hidden auto"
      , CSS.fontSize (CSS.px 11)
      , CSS.color (CSS.var "list")
      ]
  , CSS.selector_ ".tracks::-webkit-scrollbar"
      [ CSS.width (CSS.px 10) ]
  , CSS.selector_ ".tracks::-webkit-scrollbar-track"
      [ CSS.background "rgba(0,0,0,0.9)" ]
  , CSS.selector_ ".tracks::-webkit-scrollbar-thumb"
      [ CSS.background "var(--metal)"
      , CSS.border "1px outset var(--bevel-hi)"
      ]
  , CSS.selector_ ".track"
      [ CSS.display "flex"
      , CSS.justifyContent "space-between"
      , CSS.gap (CSS.px 8)
      , CSS.padding "1px 4px"
      , CSS.cursor "pointer"
      , CSS.whiteSpace "nowrap"
      ]
  , CSS.selector_ ".track-name"
      [ CSS.overflow "hidden"
      , CSS.textOverflow "ellipsis"
      ]
  , CSS.selector_ ".track-time"
      [ "font-variant-numeric" =: "tabular-nums" ]
  , CSS.selector_ ".track:hover"
      [ CSS.background "rgba(255,255,255,0.12)" ]
  , CSS.selector_ ".track.current"
      [ CSS.background "var(--sel)"
      , CSS.color (CSS.hex "FFFFFF")
      ]
  , CSS.selector_ ".plist-bar"
      [ CSS.display "flex"
      , CSS.justifyContent "space-between"
      , CSS.margin "0 8px 8px"
      , CSS.padding "2px 6px"
      , CSS.backgroundColor (CSS.hex "000000")
      , CSS.border "2px inset var(--bevel-hi)"
      , CSS.color (CSS.var "lcd")
      , CSS.fontSize (CSS.px 9)
      , CSS.letterSpacing (CSS.px 1)
      , CSS.textTransform "uppercase"
      , "font-variant-numeric" =: "tabular-nums"
      ]
  -- credits
  , CSS.selector_ ".credit"
      [ CSS.textAlign "center"
      , CSS.padding (CSS.px 6)
      , CSS.fontSize (CSS.px 9)
      ]
  , CSS.selector_ ".credit a"
      [ CSS.color (CSS.hex "8A8AA8")
      , CSS.textDecoration "none"
      , CSS.letterSpacing (CSS.px 1)
      ]
  , CSS.selector_ ".credit a:hover"
      [ CSS.color (CSS.hex "E8E8F0") ]
  ]
  where
    rackLayout =
      [ CSS.width (CSS.px 350)
      , CSS.margin "16px 0"
      , CSS.display "flex"
      , CSS.flexDirection "column"
      , CSS.userSelect "none"
      ]
    classicSkin =
      [ "--metal"     =: "#35354A"
      , "--bevel-hi"  =: "#6A6A85"
      , "--stripe"    =: "#9C9CBA"
      , "--chrome"    =: "linear-gradient(#8A8AA8, #55556E 55%, #3A3A4E)"
      , "--face"      =: "linear-gradient(#5C5C72, #3A3A4A)"
      , "--face-down" =: "linear-gradient(#32323F, #4A4A5C)"
      , "--text"      =: "#E0E0EC"
      , "--lcd"       =: "#00E800"
      , "--lcd-dim"   =: "#00A800"
      , "--glow"      =: "rgba(0,232,0,0.7)"
      , "--list"      =: "#00C800"
      , "--sel"       =: "#0000C6"
      , "--thumb"     =: "linear-gradient(#F0E0A0, #B09040)"
      , "--thumb-edge" =: "#D8C070"
      , "--viz"       =: "linear-gradient(to top, #00E800 55%, #E8E800 75%, #E80000)"
      ]
    obsidianSkin =
      [ "--metal"     =: "#1E1E24"
      , "--bevel-hi"  =: "#4A4A55"
      , "--stripe"    =: "#54545F"
      , "--chrome"    =: "linear-gradient(#3A3A44, #202028 55%, #131318)"
      , "--face"      =: "linear-gradient(#33333C, #1B1B22)"
      , "--face-down" =: "linear-gradient(#101014, #26262E)"
      , "--text"      =: "#C8C8D2"
      , "--lcd"       =: "#FF4136"
      , "--lcd-dim"   =: "#B02A22"
      , "--glow"      =: "rgba(255,65,54,0.7)"
      , "--list"      =: "#F03A30"
      , "--sel"       =: "#6E1410"
      , "--thumb"     =: "linear-gradient(#E0E0E8, #70707E)"
      , "--thumb-edge" =: "#B8B8C4"
      , "--viz"       =: "linear-gradient(to top, #FF4136 55%, #FFB000 80%, #FFFFFF)"
      ]
    goldSkin =
      [ "--metal"     =: "#4A3B26"
      , "--bevel-hi"  =: "#8A7248"
      , "--stripe"    =: "#B49A62"
      , "--chrome"    =: "linear-gradient(#A8894E, #5E4A28 55%, #43351C)"
      , "--face"      =: "linear-gradient(#6E5834, #46371F)"
      , "--face-down" =: "linear-gradient(#33270F, #57431F)"
      , "--text"      =: "#F0E2C0"
      , "--lcd"       =: "#FFB000"
      , "--lcd-dim"   =: "#B87800"
      , "--glow"      =: "rgba(255,176,0,0.7)"
      , "--list"      =: "#E8A000"
      , "--sel"       =: "#7A4A00"
      , "--thumb"     =: "linear-gradient(#FFF8D8, #C8A050)"
      , "--thumb-edge" =: "#E8D0A0"
      , "--viz"       =: "linear-gradient(to top, #FFB000 55%, #FF6A00 80%, #FF2000)"
      ]
    iceSkin =
      [ "--metal"     =: "#B8C2D4"
      , "--bevel-hi"  =: "#F0F4FA"
      , "--stripe"    =: "#7E92AE"
      , "--chrome"    =: "linear-gradient(#E8F0FA, #9FB0C8 55%, #7E92AE)"
      , "--face"      =: "linear-gradient(#D8E2F0, #A0B0C6)"
      , "--face-down" =: "linear-gradient(#8C9CB4, #C0CEE0)"
      , "--text"      =: "#1E2A3C"
      , "--lcd"       =: "#00CFFF"
      , "--lcd-dim"   =: "#0080A8"
      , "--glow"      =: "rgba(0,207,255,0.7)"
      , "--list"      =: "#00B8E8"
      , "--sel"       =: "#2050A0"
      , "--thumb"     =: "linear-gradient(#FFFFFF, #8C9CB4)"
      , "--thumb-edge" =: "#D8E2F0"
      , "--viz"       =: "linear-gradient(to top, #00CFFF 55%, #00FFC8 80%, #FFFFFF)"
      ]
----------------------------------------------------------------------
-- | Main
main :: IO ()
main = startApp (defaultEvents <> mediaEvents) app

app :: App Model Action
app = (component (mkModel thePlaylist) handleUpdate handleView)
  { styles = [ Sheet theSkin ]
  , logLevel = DebugAll
  }
----------------------------------------------------------------------
#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif
----------------------------------------------------------------------
