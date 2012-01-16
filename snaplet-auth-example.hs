{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

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
            , ("/a/logout", with auth $ logoutHandler)
            , ("/a/register", with auth $ registerHandler)
            , ("/", namePage)
            ]
  wrapHandlers (<|> heistServe)
  return $ App hs as ss

namePage :: Handler App App ()
namePage = do
  mu <- with auth currentUser
  blaze $ do
    H.h1 "Snaplet Auth Example"
    case mu of
      Just u -> H.div $ do
        H.toHtml $ "Welcome " `T.append` userLogin u `T.append` ". "
        H.a ! A.href "/a/logout" $ "Logout"
      Nothing -> do
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
      Just _ -> redirect' "/" 303
      Nothing -> blaze "Can't happen"

logoutHandler :: Handler App (AuthManager App) ()
logoutHandler = do
  logout
  redirect' "/" 303

registerHandler :: Handler App (AuthManager App) ()
registerHandler = do
  authUser <- registerUser "login" "password"
  blaze $ H.toHtml $ show authUser

main :: IO ()
main = serveSnaplet defaultConfig appInit
