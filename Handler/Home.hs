module Handler.Home where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
                              withSmallInput)
import Network.Wai (remoteHost)
import Data.Text (splitOn)
import Data.List as L (head)

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler TypedContent
getHomeR = selectRep $ do
    provideRep $ do
        (formWidget, formEnctype) <- generateFormPost sampleForm
        let submission = Nothing :: Maybe (Url)
            handlerName = "getHomeR" :: Text
        defaultLayout $ do
            aDomId <- newIdent
            setTitle "Urlist!"
            $(widgetFile "homepage")
    provideRep $ runDB $ do
        urls <- selectList [] [Desc UrlVotetotal]
	ip <- fmap (show . remoteHost . reqWaiRequest) getRequest
        let ipp = removeIp ip
        votes <- selectList [VoteIp ==. ipp] [Asc VoteUrlId]
        return $ object [ "urls" .= jsonUrls urls (map entityVal votes) ]

jsonUrls :: [Entity Url] -> [Vote] -> [Value]
jsonUrls [] _ = []
jsonUrls (url:urls) votes = jsonUrl url votes : jsonUrls urls votes

jsonUrl :: Entity Url -> [Vote] -> Value
jsonUrl url votes = object
    [ "id"           .= entityKey url
    , "href"         .= urlHref (entityVal url)
    , "votetotal"    .= urlVotetotal (entityVal url)
    , "alreadyVoted" .= case find (\v -> (voteUrlId v) == (entityKey url)) votes of
                        Just _  -> True
                        Nothing -> False
    ]

postHomeR :: Handler Html
postHomeR = do
    ((result, formWidget), formEnctype) <- runFormPost sampleForm
    case result of
        FormSuccess url -> do
            ip <- fmap (show . remoteHost . reqWaiRequest) getRequest
            let ipp = removeIp ip
            ($) runDB $ do
                urlId <- insert url
                _ <- insert $ Vote urlId ipp
                return ()
            redirect HomeR
        _               -> return ()
    redirect HomeR

postVoteUpR :: UrlId -> Handler Value
postVoteUpR urlId = runDB $ do
    ip <- fmap (show . remoteHost . reqWaiRequest) getRequest
    let ipp = removeIp ip
    update urlId [UrlVotetotal +=. 1]
    _ <- insert $ Vote urlId ipp
    urls <- selectList [] [Desc UrlVotetotal]
    votes <- selectList [VoteIp ==. ipp] [Asc VoteUrlId]
    return $ object [ "urls" .= jsonUrls urls (map entityVal votes) ]

postVoteDownR :: UrlId -> Handler Value
postVoteDownR urlId = runDB $ do
    ip <- fmap (show . remoteHost . reqWaiRequest) getRequest
    let ipp = removeIp ip
    update urlId [UrlVotetotal -=. 1]
    _ <- insert $ Vote urlId ipp
    urls <- selectList [] [Desc UrlVotetotal]
    votes <- selectList [VoteIp ==. ipp] [Asc VoteUrlId]
    return $ object [ "urls" .= jsonUrls urls (map entityVal votes) ]

removeIp :: String -> Text
removeIp ipp = L.head $ splitOn ":" $ pack ipp

sampleForm :: Form Url
sampleForm = renderBootstrap3 BootstrapBasicForm $ Url
    <$> areq textField "URL" Nothing
    <*> pure 1
