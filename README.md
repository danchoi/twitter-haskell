# twitter oauth using haskell

Experimenting with Haskell and Twitter OAuth/API


twitter.cfg is the necessary config file with creds:

      [app]
      key = yourkey
      secret =  yoursecret
      callback = http://localhost:3000/sign-in-with-twitter

Documentation is not complete


### TODO

Wire up Snap app with this function 


```
-- | Injects the oauth_verifier into the token. Usually this means the user has 
-- authorized the app to access his data.

injectOAuthVerifier :: String -> Token -> Token
injectOAuthVerifier value (ReqToken app params) = ReqToken app (replace ("oauth_verifier",value) params)
injectOAuthVerifier _ token                     = token



Twitter verifier looks like this

GET /local-endpoint-sign-in-with-twitter/?
        oauth_token=NPcudxy0yU5T3tBzho7iCotZ3cnetKwcTIRlX0iwRl0&
        oauth_verifier=uw7NjWHT6OJ1MpJOXsHfNxoAhPKpgI8BlYDhxEjIBY HTTP/1.1

https://dev.twitter.com/docs/auth/implementing-sign-twitter


```




I started with this gist from mlikka && updated and modified it

https://gist.github.com/548120
