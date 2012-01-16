{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Char8 as B
import Data.Maybe
import Data.IORef
import qualified Data.Text as T

import Snap
import Snap.Snaplet.Heist
import Snap.Snaplet.Auth
import Snap.Snaplet.Auth.Backends.JsonFile
import Snap.Snaplet.Session
import Snap.Snaplet.Session.Backends.CookieSession

import Snap.Blaze (blaze)

import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

data App = App
  { _heist :: Snaplet (Heist App)
  , _auth :: Snaplet (AuthManager App)
  , _sess :: Snaplet SessionManager
  , _companyName :: IORef B.ByteString
  }

makeLenses [''App]

instance HasHeist App where heistLens = subSnaplet heist

appInit :: SnapletInit App App
appInit = makeSnaplet "snaplet-auth-example" "Snaplet Auth Example" Nothing $ do
  hs <- nestSnaplet "heist" heist $ heistInit "templates"
  ss <- nestSnaplet "session" sess $
    initCookieSessionManager "session_key.txt" "COOKIE" Nothing
  as <- nestSnaplet "auth" auth $
    initJsonFileAuthManager defAuthSettings sess "users.json"
  addRoutes [ ("/a/login", with auth $ loginHandler)
            , ("/a/register", with auth $ registerHandler)
            , ("/hello", namePage)
            , ("/company", companyHandler)
            ]
  wrapHandlers (<|> heistServe)
  ref <- liftIO $ newIORef "fooCorp"
  return $ App hs as ss ref

namePage :: Handler App App ()
namePage = do
  mu <- with auth currentUser
  blaze $ do
    H.h1 "Snaplet Auth Example"
    H.div $ H.toHtml $ fromMaybe "You're not logged in" $ fmap (T.append "Welcome " . userLogin) mu
    H.h2 "Login"
    H.form ! A.method "POST"
           ! A.action "/a/login" $ do
      H.label ! A.for "login" $ "Username: "
      H.input ! A.type_ "text"
              ! A.name "login"
              ! A.id "login"
              ! A.value ""
      H.label ! A.for "login" $ "Password: "
      H.input ! A.type_ "password"
              ! A.name "password"
              ! A.id "password"
              ! A.value ""
      H.label ! A.for "login" $ "Remember me: "
      H.input ! A.type_ "checkbox"
              ! A.name "remember"
              ! A.id "remember"
              ! A.value ""
      H.input ! A.type_ "submit"
              ! A.value "Login"
    H.h2 "Register"
    H.form ! A.method "POST"
           ! A.action "/a/register" $ do
      H.label ! A.for "login" $ "Username: "
      H.input ! A.type_ "text"
              ! A.name "login"
              ! A.id "login"
              ! A.value ""
      H.label ! A.for "login" $ "Password: "
      H.input ! A.type_ "password"
              ! A.name "password"
              ! A.id "password"
              ! A.value ""
      H.input ! A.type_ "submit"
              ! A.value "Register"

loginHandler :: Handler App (AuthManager App) ()
loginHandler = do
  loginUser "login" "password" (Just "remember") onFailure onSuccess
  where
  onFailure _ = blaze "Login and password don't match."
  onSuccess = do
    mu <- currentUser
    case mu of
       Just user -> blaze $ do
         "Logged in. Welcome"
         H.toHtml $ userLogin user
       Nothing -> blaze "Can't happen"

registerHandler :: Handler App (AuthManager App) ()
registerHandler = do
  authUser <- registerUser "login" "password"
  blaze $ H.toHtml $ show authUser

companyHandler :: Handler App App ()
companyHandler = method GET getter <|> method POST setter
  where
  getter = do
    nameRef <- gets _companyName
    name <- liftIO $ readIORef nameRef
    writeBS name
  setter = do
    mname <- getParam "name"
    nameRef <- gets _companyName
    liftIO $ maybe (return ()) (writeIORef nameRef) mname
    getter

main :: IO ()
main = serveSnaplet defaultConfig appInit
