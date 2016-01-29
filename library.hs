-- library program that you can run in ghci and update databases etc

type Person = String
type Book = String
type Database = [(Person, Book)]

exampleBase :: Database
exampleBase = [("Alice", "Tintin"), ("Rory", "Tintin"), ("Alice", "Little Women"), ("Alice", "Asterix")]

-- lookup functions --

-- returns list of books the person is borrowing
books :: Database -> Person -> [Book]
books dBase findPerson = [book | (person, book) <- dBase, person == findPerson]

-- returns list of people who are borrowing a certain book
borrowers :: Database -> Book -> [Person]
borrowers dBase findBook = [person | (person, book) <- dBase, book == findBook]

-- returns true or false whether book is being borrowed
borrowed :: Database -> Book -> Bool
borrowed dBase findBook = borrowers dBase findBook /= []

-- returns number of books a person is borrowing
numBorrowed :: Database -> Person -> Int
numBorrowed dBase findPerson = length $ books dBase findPerson

-- update functions --

-- add a person and book to the database
makeLoan :: Database -> Person -> Book -> Database
makeLoan dBase person book = [ (person, book) ] ++ dBase

-- removes a person and book from the database
returnLoan :: Database -> Person -> Book -> Database
returnLoan dBase person book = [pair | pair <- dBase, pair /= (person, book)]
