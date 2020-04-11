module Chapter2.DataTypes where

data Client = GovOrg String
            | Company    String Integer String String
            | Individual Person Gender
            deriving Show

data Person = Person String String
            deriving Show

data Gender = Male | Female | Unknown
            deriving Show
