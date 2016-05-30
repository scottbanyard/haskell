# Haskell Programs

I have created a few Haskell programs so that I could learn some more features and functions.

---

### statistics.hs

To recap my statistics knowledge I decided to use Haskell to create functions that work out the:

`- mean`

`- variance`

`- standard deviation`

`- median`

For each of these you pass in a *list [a]* and it will return a value *a*.

I have used the Maybe type on median - if an empty list is entered then the median is *Nothing*, else it is *Just value*.

---

### efficientFibs.hs

This is just a small program that I analysed in my tutorial group. It shows the efficient and unefficient uses of Fibonacci through two different methods.

`The first method is recursive and has O(n^2) complexity

`The second method just accesses a list where the list contains the Fibonacci numbers, and so has O(n) complexity

---

### library.hs

This program uses list comprehensions to return answers.

You set up a database [(Person, Book)] for e.g.

```haskell
exampleBase :: Database
exampleBase = [("Alice", "Tintin"), ("Rory", "Tintin"), ("Alice", "Little Women"), ("Alice", "Asterix")]
```

and use this database to:
    
  `- return list of books the person is borrowing`
  
  `- returns list of people who are borrowing a certain book`
  
  `- returns true or false whether book is being borrowed`
  
  `- returns number of books a person is borrowing`
  
You can also update the database by:

  `- adding a person and book to the database`
  
  `- removing a person and book from the database`
