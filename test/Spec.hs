{-# LANGUAGE RecordWildCards #-}

import Data.Foldable (find)
import Data.Map qualified as Map
import Data.Maybe (isJust)
import Data.Text.Lazy (isInfixOf, unpack)
import Data.Text.Lazy qualified as T
import Data.Time (fromGregorian)
import Data.UUID qualified as UUID
import Effectful (runPureEff)
import Handlers (getArchive)
import Handlers.Model (
  Request (Request, requestCookies, requestHeaders, requestParams),
  Response (Redirect, SamePage),
  SamePageResponse (
    SamePageResponse,
    samePageResponseContent,
    samePageResponseCookies,
    samePageResponseHeaders
  ),
 )
import Interpreters.ArchiveStore (runArchiveStorePure)
import Lucid (renderText)
import Model.Archive (ArchiveAction (Paid, Skipped), ArchivedItem (..))
import Model.Email (Email (..))
import Model.Id qualified as Id
import Model.Password (Password (..))
import Model.User (User (..))
import Test.Hspec (describe, hspec, it, shouldBe, shouldContain, shouldSatisfy)
import Text.HTML.TagSoup (Tag (..), parseTags)
import Text.HTML.TagSoup.Tree (TagTree (..), parseTree)
import Text.StringLike (StringLike)

main :: IO ()
main = hspec do
  describe "Archive handler tests" do
    it "should output a blank page (with loading) for a first page load" do
      let response =
            runPureEff $
              runArchiveStorePure Map.empty $
                getArchive
                  Request
                    { requestCookies = []
                    , requestHeaders = []
                    , requestParams = []
                    }
                  User
                    { email = Email ""
                    , name = ""
                    , passwordHash = Password ""
                    }
      case response of
        SamePage (SamePageResponse{..}) -> do
          let parsed = parseTree $ renderText samePageResponseContent
          let matches = findMatchingTags "main" parsed []
          case matches of
            [TagBranch "main" _ [TagLeaf (TagText str)]] ->
              unpack str `shouldContain` "Loading"
            _ -> fail "Patterns didn't match expected output"
        Redirect _ -> fail "Expecting a Same Page, got a Redirect"
    it "should display a friendly message if there are no items" do
      let response =
            runPureEff $
              runArchiveStorePure Map.empty $
                getArchive
                  Request
                    { requestCookies = []
                    , requestHeaders = [("HX-Request", "true")]
                    , requestParams = []
                    }
                  User
                    { email = Email "noitems@email.com"
                    , name = ""
                    , passwordHash = Password ""
                    }
      case response of
        SamePage (SamePageResponse{..}) -> do
          let parsed = parseTags $ renderText samePageResponseContent
          let found =
                find
                  ( \case
                      TagText str -> "No items" `isInfixOf` str
                      _ -> False
                  )
                  parsed
          found `shouldSatisfy` isJust
        _ -> fail "Expecting a Same Page, got a Redirect"
    it "should render the items if there are any" do
      let mNewId1 = Id.fromText (UUID.toText UUID.nil)
          mNewId2 = Id.fromText (UUID.toText UUID.nil)
      case (mNewId1, mNewId2) of
        (Just newId1, Just newId2) -> do
          let response =
                runPureEff
                  $ runArchiveStorePure
                    ( Map.fromList
                        [
                          ( Email "items@email.com"
                          ,
                            [ ArchivedItem
                                { archivedItemId = newId1
                                , archivedItemItemDefinitionId = newId2
                                , archivedItemDescription = "Test"
                                , archivedItemAmount = 123
                                , archivedItemDate = fromGregorian 2024 1 1
                                , archivedItemActionDate = fromGregorian 2024 1 1
                                , archivedItemAction = Paid
                                }
                            , ArchivedItem
                                { archivedItemId = newId1
                                , archivedItemItemDefinitionId = newId2
                                , archivedItemDescription = "Test 1"
                                , archivedItemAmount = 1234
                                , archivedItemDate = fromGregorian 2024 2 1
                                , archivedItemActionDate = fromGregorian 2024 2 1
                                , archivedItemAction = Skipped
                                }
                            ]
                          )
                        ]
                    )
                  $ getArchive
                    Request
                      { requestCookies = []
                      , requestHeaders = [("HX-Request", "true")]
                      , requestParams = []
                      }
                    User
                      { email = Email "items@email.com"
                      , name = ""
                      , passwordHash = Password ""
                      }
          case response of
            SamePage (SamePageResponse{..}) -> do
              let parsed = parseTags $ renderText samePageResponseContent
              let allCards =
                    filter
                      ( \case
                          TagOpen "div" attrs -> any (\(k, v) -> k == "class" && elem "card" (T.words v)) attrs
                          _ -> False
                      )
                      parsed
              length allCards `shouldBe` 2
            _ -> fail "Expecting a Same Page, got a Redirect"
        _ -> fail "This shouldn't have happened"

findMatchingTags :: (StringLike str) => str -> [TagTree str] -> [TagTree str] -> [TagTree str]
findMatchingTags tagName trees accum = do
  tree <- trees
  findTags tagName tree accum

findTags :: (StringLike str) => str -> TagTree str -> [TagTree str] -> [TagTree str]
findTags _ (TagLeaf _) accum = accum
findTags tagName t@(TagBranch tag _ children) accum
  | tagName == tag = findMatchingTags tagName children (t : accum)
  | otherwise = findMatchingTags tagName children accum
