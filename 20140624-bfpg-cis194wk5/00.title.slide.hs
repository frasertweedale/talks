thisLecture :: Lecture
thisLecture = Lecture
  { course = CIS194
  , author = Person "Brent Yorgey"
  , presenter = Person "Fraser Tweedale"
  , week = 5
  , title = "Parametric Polymorphism and Type Classes"
  }

data Course = CIS194 | BosCourse
newtype Person = Person { getName :: String }
data Lecture = Lecture
  { course :: Course
  , author :: Person
  , presenter :: Person
  , week :: Int
  , title :: String
  }
