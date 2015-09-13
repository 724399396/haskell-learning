--file : ch06/BrokenClass.hs
import SimpleJSON
instance (JSON a) => JSON [a] where
  toJValue = undefined
  fromJValue = undefined

instance (JSON a) => JSON [(String,a)] where
  toJValue = undefined
  fromJValue = undefined
