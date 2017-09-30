module Components.PagePackages where

import Debug.Trace
import Control.Monad.Eff.Ref as Ref
import Data.Array as Arr
import Data.Char as Char
import Data.Foldable as F
import Data.Set as Set
import Data.StrMap as SM
import Data.String as Str
import Data.Tuple as Tuple
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Lib.MatrixApi as Api
import Lib.MatrixApi2 as Api2
import Lib.MiscFFI as MiscFFI
import Lib.Types as T
import Network.RemoteData as RD
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Reader.Class (asks)
import Data.Maybe (Maybe(..), isNothing)
import Prelude (type (~>), Unit, Void, class Ord, append, bind, const, discard, not, otherwise, pure, ($), (<$>), (<<<), (<>))

type State =
 {
   packages :: Array T.PackageName
 , tags :: Array T.TagName
 , tagsPkg :: T.TagsWithPackages
 , tagsMap :: SM.StrMap (Array T.TagName)
 , clicked :: Boolean
 , selectedTag :: Set.Set T.TagName
 , selectedPrefix :: Set.Set T.Prefixs
 }

data Query a
  = Initialize a
  | SelectedTag T.TagName a
  | SelectedPrefix T.Prefixs a
  | HandleCheckBox State Boolean a
  | Finalize a

component :: forall e. H.Component HH.HTML Query Unit Void (Api.Matrix e)
component = H.lifecycleComponent
  { initialState: const initialState
  , render
  , eval
  , initializer: Just (H.action Initialize)
  , finalizer: Just (H.action Finalize)
  , receiver: const Nothing
  }
  where

  initialState :: State
  initialState =
   {
     packages: []
   , tags: []
   , tagsPkg: SM.empty
   , tagsMap: SM.empty
   , clicked: false
   , selectedTag: Set.empty
   , selectedPrefix: Set.empty
   }

  render :: State -> H.ComponentHTML Query
  render state =
    HH.div
      [ HP.id_ "page-packages"
      , HP.class_ (H.ClassName "page")
      ]
      [ HH.div
          [ HP.class_ (H.ClassName "rightcol") ]
          [ HH.div
              [ HP.class_ (H.ClassName "sub") ]
              [ HH.text "Times are shown in your timezone" ]
          ]
      , HH.div
          [ HP.class_ (H.ClassName "leftcol") ]
          [ HH.h2
              [ HP.class_ (H.ClassName "main-header") ]
              [ HH.text "Packages" ]
          , HH.div
              [ HP.class_ (H.ClassName "main-header-subtext") ]
              []
          , HH.label_
              [ HH.input
                  [ HP.class_ (H.ClassName "packages-only-reports")
                  , HP.type_ HP.InputCheckbox
                  , HE.onChecked $ HE.input (HandleCheckBox state)
                  ]
              , HH.text " Only show packages with reports"
              ]
          , HH.ol
              [ HP.classes (H.ClassName <$> ["tag-filter","clearfix"])
              ] $ ( buildTags' state) <$> state.tags
          , HH.ol
              [ HP.classes (H.ClassName <$> ["headers","clearfix"]) ] $ buildPrefixs <$> prefixs
          , HH.ol
              [ HP.class_ (H.ClassName "packages") ] $
                Arr.take 650 $ buildPackages state <$> (packages' state)
          ]
      ]
    where
      packages' st = ((tagFilter st) <<< (prefixFilter st)) state.packages
      tagFilter {tagsMap, selectedTag, tagsPkg} = Arr.filter (tagContained selectedTag tagsMap)
      prefixFilter {selectedPrefix, packages} = Arr.filter (prefixContained selectedPrefix)

  eval :: Query ~> H.ComponentDSL State Query Void (Api.Matrix e)
  eval (Initialize next) = do
    st <- H.get
    pkgRef <- asks _.packages
    listPkg <- liftEff $ Ref.readRef pkgRef
    tagList <- H.lift Api2.getTagsWithoutPackage
    tagPkgList <- H.lift Api2.getTagsWithPackages
    let
      pkgTag = case tagPkgList of
        RD.Success a -> a
        _ -> SM.empty
      tagPkgs = pkgTagList pkgTag
      pkgArr =
        case listPkg of
          RD.Success arr -> arr
          _ -> []
    initState <- H.put $ st { packages =
                                case listPkg of
                                  RD.Success arr -> arr
                                  _              -> []
                            , tags =
                                case tagList of
                                  RD.Success arr -> arr
                                  _              -> []
                            , tagsPkg =
                                case tagPkgList of
                                  RD.Success map -> map
                                  _              -> SM.empty
                            , tagsMap = tagPkgs
                            , clicked = false
                            }
    traceAnyA tagPkgs
    traceAnyA tagPkgList
    pure next

  eval (SelectedTag tag next) = do
    H.modify \st -> st { selectedTag = if (Set.member tag st.selectedTag)
                                          then Set.delete tag st.selectedTag
                                          else Set.insert tag st.selectedTag }
    pure next

  eval (SelectedPrefix prefix next) = do
    H.modify \st -> st { selectedPrefix = Set.singleton prefix }
    pure next

  eval (HandleCheckBox st isCheck next)
    | isCheck = do
        H.modify _ { packages = []} -- TODO: get report for Arr.filter indexStateContained
        pure next
    | otherwise = eval (Initialize next)

  eval (Finalize next) = do
    pure next

prefixs :: Array T.Prefixs
prefixs = Str.singleton <$> Char.fromCharCode <$> (Arr.(..) 65 90)

buildPrefixs :: forall p. String -> HH.HTML p (Query Unit)
buildPrefixs prefix =
  HH.li_
    [ HH.a
        [ HP.class_ (H.ClassName "header")
        , HP.attr (H.AttrName "data-prefix") prefix
        , HE.onClick $ HE.input_ (SelectedPrefix prefix)
        ]
        [ HH.text $ prefix ]
    ]

buildTags :: forall p i. T.TagName -> HH.HTML p i
buildTags tag =
  HH.a
    [ HP.class_ (H.ClassName "tag-item")
    , HP.attr (H.AttrName "data-tag-name") tag
    ]
    [ HH.text $ tag ]

buildTags' :: forall p. State -> T.TagName -> HH.HTML p (Query Unit)
buildTags' st tag =
  HH.a
    [ HP.classes (H.ClassName <$> ["tag-item", clickStatus])
    , HP.attr (H.AttrName "data-tag-name") tag
    , HE.onClick $ HE.input_ (SelectedTag tag)
    ]
    [ HH.text $ tag ]
  where
    clickStatus = if (Set.member tag st.selectedTag)  then "active" else " "

buildPackages :: forall p. State -> T.PackageName -> HH.HTML p (Query Unit)
buildPackages state pkgName =
  HH.li_ $
    [ HH.a
        [ HP.href $ "#/package/" <> pkgName]
        [ HH.text pkgName ]
    ] <> (buildTags <$> (getTheTags state pkgName)) <> [ HH.small_
                                                           [ HH.text $ " - index-state: " -- <> (MiscFFI.formatDate packageMeta.report)
                                                           ]
                                                       ]

tagContained :: Set.Set T.TagName -> SM.StrMap (Array T.TagName) -> T.PackageName -> Boolean
tagContained selectedTags tagsMap pkgName
    | Set.isEmpty selectedTags = true
    | otherwise            =
      let
        tags =
          case SM.lookup pkgName tagsMap of
            Just a  -> a
            Nothing -> []
      in not Set.isEmpty (Set.fromFoldable tags `Set.intersection` selectedTags)

prefixContained :: Set.Set T.Prefixs -> T.PackageName -> Boolean
prefixContained selectedPrefix pkgName
    | Set.isEmpty selectedPrefix = true
    | otherwise              = Set.member (Str.toUpper $ Str.take 1 pkgName) selectedPrefix

indexStateContained :: T.PackageMeta -> Boolean
indexStateContained pkgMeta
    | isNothing pkgMeta.report = false
    | otherwise = true

concatTags :: T.TagsWithPackages -> T.TagName -> Array T.PackageName
concatTags smpkg tag =
  case SM.lookup tag smpkg of
    Just a -> a
    Nothing -> []

getTheTags :: State -> T.PackageName -> Array T.TagName
getTheTags { tagsMap } pkg =
  case SM.lookup pkg tagsMap of
    Just a -> a
    Nothing -> []

pkgTagList :: SM.StrMap (Array T.PackageName)
           -> SM.StrMap (Array T.TagName)
pkgTagList m =
        SM.fromFoldableWith append $ do
          Tuple.Tuple k vs <- SM.toUnfoldable m
          v <- vs
          pure (Tuple.Tuple v [k])

