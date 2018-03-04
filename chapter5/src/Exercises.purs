module Exercises where
  
import Prelude

type Address = { street :: String, city :: String }
type Person = { name :: String, address :: Address }

sameCity :: Person -> Person -> Boolean
sameCity { address : {city : c1 }} { address : {city : c2 }} = c1 == c2

fromSingleton :: forall a. a -> Array a -> a
fromSingleton _ [x] = x
fromSingleton a _ = a