# Building Projects

## Modules

* Haskell programs are organised into modules
* Equivalent to namespaces in C#

## Making packages with Stack

* Haskell Cabal is the package manager being used
* Ensures dependencies are present that other packages depend on
* Stack is built on top of Cabal, helps manage projects
* Allows you to only have to build large libraries once
* Uses Long Term Service snapshot of packages, to guarantee compatability

## Working with a basic project

* see subfolder hello
* `stack build` to build project
* `stack setup` to set up GHC if necessary

### Loading and running code from REPL

* `stack ghci` to open REPL
* `:l Main` to compile Main 
* `main` to run main

### stack exec

* `Linking .stack-work/dist/{...noise...}/hello`
* noise is stack compiling an executable binary and linkint to it
* can run from command line using `stack exec -- hello`

### Executable stanzas in Cabal files

```cabal
executable hello
  hs-source-dirs:      src
  main-is:             Main.hs
  default-language:    Haskell2010
  build-depends:       base >= 4.7 && < 5
```
* hello is name of executable
* src is where code is located
* Main.hs is where the main function is located
* Haskell2010 is the standard of Haskell to expect
* base is the library it depends on

## Making our project a library

* see changes to hello.cabal, Hello.cs and Main.cs
* if you add code like Hello.cs to a different folder then it's best to import it as a library using build-depends
* `build-depends: base >= 4.7 && < 5, hello`

## Module exports

* If you don't specify any exports in a module, every top-level binding is expoted and can be imported
* can be explicit about what is to be exported using an export list
* `module Hello (sayHello) where`

### Exposing modules

* see file DogsRule.hs
* need to expose the DogsRule module to be able to use in Main.hs

## More on importing modules

* imported modules have scope throughout the module
* ordering of import declarations is ireelevant
* `:browse` lets you see what functions are included in a named module
* can turn off Prelude by using `stack ghci --ghci-options -XNoImplicitPrelude`
* can then import only bool using `import Data.Bool (bool)`
* now only bool is in scope

### Qualified imports

* using qualified keyword means that we always have to specify where something came from
* useful when two things with the same name are imported from different modules
* `import qualified Data.Bool`
* `:t bool` returns an error but `:t Data.Bool.bool` works
* can also use aliases to avoid typing out the entire namespace
* `import qualified Data.Bool as B`
* `:t B.bool`

#### Setting the Prelude prompt

* loading modules this way puts them in our prompt
* quickly becomes messy when many modules are loaded
* can use `:set prompt` to change it to whatever

## Intermission

1. forever, when
2. Data.Bits, Database.Blacktip.Types
3. the models representing the tables in the database
4. Control.Concurrent.MVar, Filesystem.Path.CurrentOS,, Control.Concurrent
5. Filesystem
6. Control.Monad

## Making our program interactive

* see src folder for file changes
* uses do syntax inside in order to sequence side effects
* uses `<-` called a bind, will be explained in later chapter
* binds the output of IO String to a String type variable
* sayHello takes the output of the bind and prints it to the screen
* have to change main function to add prompt for user to add name using `  hSetBuffering stdout NoBuffering`

## do syntax and IO

* do blocks are syntactic sugar for sequencing, are useful but not strictly necessary
* do syntax allows to sequence monadic actions
* Monads are covered later
* do blocks make more readable syntax and hides underlying nesting
* allows naming of values returned by monadic IO actions to use them as input to other actions in program

```haskell
main = do
  x1 <- getLine
  x2 <- getLine
  return (x1 ++ x2)
```

* takes two inputs and then concatenates them and returned the result

### return

* basically just returns a value
* value is returned inside a monadic structure
* main always has type IO () so output must match that type
* some do blocks just `return ()` which returns an empty tuple because haskell functions must return something

```haskell
twoo :: IO Bool
twoo = do c <- getChar
  c' <- getChar
  c == c'
```

* this doesn't work because `==` returns a value of type Bool while the function specified type IO Bool
* actual last line should be `return (c == c')`

```haskell
main :: IO ()
main = do c <- getChar
  c' <- getChar
  if c == c'
    then putStrLn "True"
    else return ()
```

* now simply outputs type IO () but only prints to the screen if the characters are equal
* do syntax is effectively imperative syntax but in haskell
* but cannot have side effects without using IO in the result type

#### Do notation considered harmful

* do blocks can be overused by beginners
* not necessary in single-line expressions, but that will be covered later
* also unnecessary to use do with putStrLn and print because they already have the effects
* do still works the same in these cases, but it's not necessary

## Hangman game

* `stack new hangman simple` to create project
* see cabal file for changes, adding random and split libraries

## Step One: Importing Modules

* See Main.hs for import list, not strictly necessary since they come with base
* forever allows infinite loop
* isJust used to tell if every character in puzzle has been discovered or not
* all allows to check if something returns True for all members of a list
* intersperse used to put spaces between characters guess by player
* exitSuccess allows to exit with no errors when done
* randomRIO selects a word from dictionary at random

## Step Two: Generating a world list

* see comments in Main.hs for explanation of code

## Step Three: Making a puzzle

* Needs to hide the word from the player
* means of asking for letter guesses
* seeing if letter is in a word
* etc
* see Main.hs for explanations

## Adding a newtype

* could add more clarity by using newtype
* `newType WordList = WordList [String] deriving (Eq, Show)`
* then changing functions to use this type

## Chapter Exercises

### Hangman game logic

1. see change to gameOver in hangman

### Modifying code

2. see file


