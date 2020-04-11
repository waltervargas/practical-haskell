module Chapter2.DataTypes where

data Client = GovOrg String
            | Company    String Integer String String
            | Individual Person
            deriving Show

data Person = Person String String Gender
            deriving Show

data Gender = Male | Female | Unknown
            deriving Show

data TimeMachine = TimeMachine String Integer String Time Float
                 deriving Show

data Time = Past | Future
          deriving Show


companyName :: Client -> Maybe String
companyName client = case client of
                       Company name _ _ _ -> Just name
                       _                  -> Nothing

clientName :: Client -> String
clientName (GovOrg name)                     = name
clientName (Company name _ _ _)              = name
clientName (Individual (Person fNm lNm _)) = fNm ++ " " ++ lNm

fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)
