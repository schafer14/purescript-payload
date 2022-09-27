-- | Helpers for dealing with request headers
module Payload.Headers
  ( Headers
  , empty
  , fromFoldable
  , lookup
  , set
  , setIfNotDefined
  , lookupCookie
  , setCookie
  , setCookieIfNotDefined
  , toUnfoldable
  , getCookies
  ) where

import Prelude

import Data.Foldable (class Foldable, foldl)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (class Unfoldable)
import Payload.Internal.Utils as Utils

--- | HTTP allows using repeated header lines.  All other
--- | cases can be collapsed to single header lines except
--- | for "Set-Cookie".  The second Map is for them.
newtype Headers = Headers {headers :: Map String String, cookies :: Map String String}

instance showHeaders :: Show Headers where
  show (Headers {headers, cookies}) = show headers <> " Set-Cookie: " <> show cookies

instance eqHeaders :: Eq Headers where
  eq (Headers {headers: a , cookies: c1}) (Headers {headers: b, cookies: c2}) = eq a b && eq c1 c2

--- | This prefers values on the right in the case
--- | of duplicate keys, i.e. old values are overwritten.
instance semigroupHeaders :: Semigroup Headers where
  append (Headers {headers: a, cookies: c1}) (Headers {headers: b, cookies: c2}) =
    Headers {headers: Map.union b a, cookies: Map.union c2 c1}

empty :: Headers
empty = Headers {headers: Map.empty, cookies: Map.empty}

set :: String -> String -> Headers -> Headers
set name value (Headers {headers, cookies}) =
  Headers {headers: Map.insert (Utils.toLowerCase name) value headers, cookies}

setCookie :: String -> String -> Headers -> Headers
setCookie name value (Headers {headers, cookies}) =
  Headers {headers, cookies: Map.insert name value cookies}

setIfNotDefined :: String -> String -> Headers -> Headers
setIfNotDefined name _ headers | member name headers = headers
setIfNotDefined name value headers = set name value headers

setCookieIfNotDefined :: String -> String -> Headers -> Headers
setCookieIfNotDefined name value (Headers {headers, cookies}) =
  Headers {headers, cookies: Map.insertWith const name value cookies}

fromFoldable :: forall f. Foldable f => f (Tuple String String) -> Headers
fromFoldable f = foldl setFromTuple empty f
  where
    setFromTuple m (Tuple k v) | Utils.toLowerCase k == "set-cookie" = setCookie k v m
    setFromTuple m (Tuple k v) = set k v m

--- | Returns non-repeatable fields (everything but Set-Cookie).
toUnfoldable :: forall f. Unfoldable f => Headers -> f (Tuple String String)
toUnfoldable (Headers {headers}) = Map.toUnfoldable headers

getCookies :: Headers -> Array String
getCookies (Headers {cookies}) =
  map (\(Tuple key value) -> key <> "=" <> value) $ Map.toUnfoldable cookies

lookup :: String -> Headers -> Maybe String
lookup key (Headers {headers}) = Map.lookup (Utils.toLowerCase key) headers

lookupCookie :: String -> Headers -> Maybe String
lookupCookie key (Headers {cookies}) = Map.lookup key cookies

member :: String -> Headers -> Boolean
member key (Headers {headers}) = Map.member (Utils.toLowerCase key) headers
