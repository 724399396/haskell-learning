-- file: ch06/NewtypeDiff.hs
-- ok: any number of fields and contructors
data TwoFields = TwoFields Int Int

-- ok: exactly one field
newtype OKay = ExxactlyOne Int

-- ok: type parmeters are no problem
newtype Param a b = Param (Either a b)

-- ok: record syntax is fine
newtype Record = Record {
  getInt :: Int
}

-- bad: no fields
newtype TooFew = TooFew

-- bad: more than one field
newtype TooManyFields = Fields Int Int

-- bad: more than one constuctor
newtype TooManyCtors = Bad Int
                     | Worse Int
