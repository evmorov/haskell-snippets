data Patient = Patient { firstName
                       , lastName
                       , email        :: String
                       , age
                       , diseaseId    :: Int
                       , isIndoor
                       , hasInsurance :: Bool
                       }

main :: IO ()
main = print $ ("Email: " ++ email patientWithChangedEmail ++ ", insurance: " ++ show insurance)
  where
    patientWithChangedEmail = patient {
      email = "j.d@gmail.com"
    }

    patient = Patient {
        firstName    = "John"
      , lastName     = "Doe"
      , email        = "john.doe@gmail.com"
      , age          = 24
      , diseaseId    = 431
      , isIndoor     = True
      , hasInsurance = True
    }

    Patient _ _ _ _ _ _ insurance = patient
