--
-- MATHFUN
-- Haskell coursework
-- UP773030
--
--
import Data.List
import Data.Char (isLetter, isDigit)
-- Types
type Title = String
type Director = String
type Year = Int
type Fans = [String]

type Database = [Film]

-- Define Film type here
data Film = Film Title Director Year Fans
            deriving (Show, Eq, Read)

db :: [Film]
db = [Film "Blade Runner" "Ridley Scott" 1982 ["Zoe", "Heidi", "Jo", "Kate", "Emma", "Liz", "Sam", "Olga", "Tim"],
      Film "The Fly" "David Cronenberg" 1986 ["Garry", "Dave", "Zoe", "Kevin", "Emma"],
      Film "Body Of Lies" "Ridley Scott" 2008 ["Bill", "Olga", "Tim", "Zoe", "Paula"],
      Film "Avatar" "James Cameron" 2009 ["Dave", "Amy", "Liz"],
      Film "Titanic" "James Cameron" 1997 ["Zoe", "Emma", "Paula", "Liz", "Olga", "Dave"],
      Film "The Departed" "Martin Scorsese" 2006 ["Wally", "Liz", "Kevin", "Tim", "Emma"],
      Film "Aliens" "Ridley Scott" 1986 ["Dave", "Garry", "Liz", "Sam", "Wally", "Kate", "Zoe"],
      Film "Kingdom Of Heaven" "Ridley Scott" 2005 ["Jo", "Wally", "Emma"],
      Film "Prometheus" "Ridley Scott" 2012 ["Kevin", "Tim", "Emma", "Jo", "Liz"],
      Film "E.T. The Extra-Terrestrial" "Steven Spielberg" 1982 ["Dave", "Amy", "Garry", "Ian", "Neal"],
      Film "Bridge of Spies" "Steven Spielberg" 2015 ["Wally", "Sam", "Dave", "Neal"],
      Film "Jaws" "Steven Spielberg" 1975 ["Dave", "Jo", "Zoe", "Wally", "Emma", "Kate"],
      Film "The Martian" "Ridley Scott" 2015 ["Wally", "Sam", "Dave", "Jo", "Jenny", "Kate", "Emma", "Olga"],
      Film "The BFG" "Steven Spielberg" 2016 ["Sam", "Wally", "Dave", "Jo", "Kate"],
      Film "The Shawshank Redemption" "Frank Darabont" 1994 ["Dave", "Amy", "Bill", "Garry", "Ian", "Neal", "Kate", "Jenny", "Zoe"],
      Film "Gladiator" "Ridley Scott" 2000 [],
      Film "The Green Mile" "Frank Darabont" 1999 ["Kevin", "Tim", "Emma", "Heidi"],
      Film "True Lies" "James Cameron" 1994 ["Sam", "Dave"],
      Film "Super 8" "J J Abrams" 2011 ["Kevin", "Tim", "Emma", "Olga", "Heidi"],
      Film "Minority Report" "Steven Spielberg" 2002 ["Kevin", "Kate", "Tim", "Emma", "Olga", "Jenny", "Zoe"],
      Film "War Horse" "Steven Spielberg" 2011 ["Garry", "Bill", "Olga", "Jo", "Wally", "Emma", "Tim", "Kate", "Zoe"],
      Film "Silence" "Martin Scorsese" 2016 ["Wally", "Emma", "Tim", "Heidi", "Bill", "Olga", "Jo"],
      Film "The Terminal" "Steven Spielberg" 2004 ["Kate", "Dave", "Jo", "Wally", "Emma"],
      Film "Star Wars: The Force Awakens" "J J Abrams" 2015 ["Emma", "Wally", "Zoe", "Kate", "Bill", "Dave", "Liz", "Jo"],
      Film "Hugo" "Martin Scorsese" 2011 ["Wally", "Sam"]]



--adds a (Film title director year fans)  to the database
addFilm :: Film -> Database -> Database
addFilm newFilm database = newFilm : database

--checks if the specified film was released after specified year
-- if specified year is greater then release year of the film true is returned else false
filmAfterYear :: Int -> Film -> Bool
filmAfterYear year (Film t d r f)
              | r > year = True
              | otherwise = False

--displays all films released after specified film, returns them as well formated string
releasedAfter :: Year -> Database -> String
releasedAfter year database = displayAllFilms (filter (filmAfterYear year) database)

--checks if the specified name is an element of the fans list in the film
isFanOfFilm :: String -> Film -> Bool
isFanOfFilm name (Film t d r f)
            | name `elem` f = True
            | otherwise = False

--returns all films which particular user is fan of, uses isFanOfFilm to filter the database
--returns the values as well formatted string
allFilmsParticularUser :: String -> Database -> String
allFilmsParticularUser name database = displayAllFilms (filter (isFanOfFilm name) database)

--returns all fans of specified film by its title, uses concat and to join the results
allFansParticularFilm :: Title -> Database -> String
allFansParticularFilm title database = "These are the fans of " ++ title ++ " : " ++ "\n" ++
                                        concat (intersperse "\n" (getFans (head (filter (filmsByTitle title) database))))

--returns the fans of the film without concat, just the list of fans
allFansParticularFilmList :: Title -> Database -> Fans
allFansParticularFilmList title database = getFans (head (filter (filmsByTitle title) database))

--returns true if the given title is equal to given Film's title
filmsByTitle :: Title -> Film -> Bool
filmsByTitle title (Film t d r f)
              | title == t      = True
              | otherwise       = False

--uses a displayOneFilm function and join the outputs together
displayAllFilms :: Database -> String
displayAllFilms database = concat(map displayOneFilm database)

--function to add a fan to a film by checking if the specified
--title matches any of the film's title in the database
-- if the user is not a fan of the chosen film it gets added else nothing happens
addFan :: String -> String -> [Film] -> [Film]
addFan _ _ [] = []
addFan fanName title ((Film t d r f):xs)
      | title == t && not (isFanOfFilm fanName (Film t d r f)) = ((Film t d r (fanName :f)):xs)
      | otherwise = (Film t d r f) : addFan fanName title xs

--if the specified director equals films director then true else false
filmsByDirector :: Director -> Film -> Bool
filmsByDirector director (Film t d r f)
                | director == d     = True
                | otherwise         = False

--function to return all fans of the specified director as a well formatted string
allFansOfDirector :: Director -> Database -> String
allFansOfDirector director database = "These are the fans of " ++ director ++ " : " ++ "\n" ++
                                      concat (intersperse "\n" (nub (showFansOfFilm (filter (filmsByDirector director) database ))))

--function to give list of all fans together after specifying a list of films
showFansOfFilm :: Database -> Fans
showFansOfFilm [] = []
showFansOfFilm (x:xs) = (getFans x ++ (showFansOfFilm xs))

--function to return the fans from the film
getFans :: Film -> Fans
getFans (Film t d r f) = f

--function to return title from the film
getTitle :: Film -> Title
getTitle (Film t d r f) = t

--function to return director from the film
getDirector :: Film -> Director
getDirector (Film t d r f) = d

--function to return the number of fans after giving a list of fans
getNumberOfFans :: Fans -> Int
getNumberOfFans fans = length fans

--function to display one film as a well formatted string
displayOneFilm :: Film -> String
displayOneFilm (Film title director date fans) = "Title : " ++ title ++ " ,Directed by : " ++ director
                                                ++ "\nReleased in : " ++ show date ++ "\nNumber of fans : "
                                                ++ show (length fans) ++ "\n" ++ "\n"


-- Demo function to test basic functionality (without persistence - i.e.
-- testDatabase doesn't change and nothing is saved/loaded to/from file).


--demos for the coursework
demo :: Int -> IO ()
demo 1  = putStrLn (displayAllFilms (addFilm (Film "Alien: Covenant" "Ridley Scott" 2017 []) db))
demo 2  = putStrLn (displayAllFilms db)
demo 3  = putStrLn (releasedAfter 2008 db)
demo 4  = putStrLn (allFilmsParticularUser "Liz" db)
demo 5  = putStrLn (allFansParticularFilm "Jaws" db)
demo 6 = putStrLn (displayAllFilms (addFan "Liz" "The Fly" db))
demo 66 = putStrLn (displayAllFilms (addFan "Liz" "Avatar" db))
demo 7 = putStrLn (allFansOfDirector "James Cameron" db)
--demo 8  = putStrLn all directors & no. of their films that "Liz" is a fan of

--
--
-- USER INTERFACE CODE
--
--

--function to check if user wants to exit, if yes returns true else false
checkIfExit :: String -> Bool
checkIfExit exit
      | exit == "Y" = True
      | exit == "y" = True
      | otherwise      = False

--function to check if the inputed string does not contain digits
validString :: String -> Bool
validString = all isLetter

--function to check if the inputed string contains only digits
validNumber :: String -> Bool
validNumber = all isDigit

--function to check if the specified year is before the current year
validYear :: Int -> Bool
validYear year
      | year <= 2017            = True
      | otherwise               = False

-- function used to get the name from the user
getName :: Database -> IO ()
getName films = do
        putStr ("Please enter your name : ")
        name <- getLine
        if name == "" || (validString name == False) then do
          putStrLn "#######################################################################"
          putStrLn "Please enter a valid name (no numbers and cant be empty)"
          putStrLn "#######################################################################"
          getName films
          else do
            displayMenu films name

--function to check if the specified director exists in the database
directorExist :: String -> Database -> Bool
directorExist director [] = False
directorExist director (x:xs)
        | director == getDirector x = True
directorExist director (x:xs) = directorExist director xs

--function to check if the specified title of the film exists in the database
titleExist :: String -> Database -> Bool
titleExist title [] = False
titleExist title (x:xs)
        | title == getTitle x = True
titleExist title (x:xs) = titleExist title xs

--function to convert the contents of films.txt into a Database type format
convertToFilm :: IO Database
convertToFilm = do
    films <- readFile "films.txt"
    return (read films :: Database)

--function to add a film to the database after asking questions
addFilmIO :: Database -> String -> IO ()
addFilmIO films name = do
              putStrLn "Please enter film's title"
              title <- getLine
              let exists = titleExist title films
              if exists then do
                putStrLn "#######################################################################"
                putStrLn "Sorry, film already exists"
                putStrLn "#######################################################################"
                addFilmIO films name
                else do
                  putStrLn "Please enter film's director"
                  director <- getLine
                  let valid = validNumber director
                  if valid then do
                    putStrLn "#######################################################################"
                    putStrLn "names cannot have numbers!"
                    putStrLn "#######################################################################"
                    addFilmIO films name
                    else do
                      putStrLn "Please enter release year"
                      year <- getLine
                      let validY = validNumber year
                      if not validY then do
                        putStrLn "#######################################################################"
                        putStrLn "Please enter a year with 4 digits"
                        putStrLn "#######################################################################"
                        addFilmIO films name
                        else do
                          let validY = validYear (read year :: Int)
                          if not validY then do
                            putStrLn "#######################################################################"
                            putStrLn "Cant set release date in future"
                            putStrLn "#######################################################################"
                            addFilmIO films name
                            else do
                              putStrLn "####################-----------FILM ADDED!-----------##################"
                              let newDatabase = addFilm (Film title director (read year :: Int) []) films
                              displayMenu newDatabase name
                              putStrLn "#######################################################################"


--function to display All of the films from the datbase
displayAllFilmsIO :: Database -> String -> IO ()
displayAllFilmsIO films name = do
              putStrLn "#######################################################################"
              putStrLn (displayAllFilms films)
              displayMenu films name
              putStrLn "#######################################################################"

--function to return the films released after specified year by the user
releasedAfterIO :: Database -> String -> IO ()
releasedAfterIO films name = do
              putStrLn "Please enter a year :"
              year <- getLine
              let valid = validNumber year
              if not valid then do
                putStrLn "Please enter a valid year (4 digits not empty, no letters)"
                releasedAfterIO films name
                else do
                  let valid = validYear (read year :: Int)
                  if not valid then do
                      putStrLn "#######################################################################"
                      putStrLn "Cannot check for years in future!"
                      putStrLn "#######################################################################"
                      else do
                        let query = (releasedAfter (read year :: Int) films)
                        if null query then do
                        putStrLn "#######################################################################"
                        putStrLn "There are no films released after specifed year"
                        putStrLn "#######################################################################"
                        else do
                          putStrLn (query)
              displayMenu films name

--function to return all films that the current user is a fan of
allFilmsParticularUserIO :: Database -> String -> IO ()
allFilmsParticularUserIO films name = do
              putStrLn (allFilmsParticularUser name films)
              displayMenu films name


--function to return all of the fans after specifiying title of the film
allFansParticularFilmIO :: Database -> String -> IO ()
allFansParticularFilmIO films name = do
              putStrLn "Please enter the title of the film"
              title <- getLine
              let exists = titleExist title films
              if (not exists) || (title == "") then do
                putStrLn "#######################################################################"
                putStrLn "Title not found, try again"
                putStrLn "#######################################################################"
                allFansParticularFilmIO films name
                else do
                  putStrLn "#######################################################################"
                  putStrLn (allFansParticularFilm title films)
                  putStrLn "#######################################################################"
              displayMenu films name

--function to return all fans of the director by asking directors name
allFansOfDirectorIO :: Database -> String -> IO ()
allFansOfDirectorIO films name = do
                putStrLn "Please enter a name of the director: "
                director <- getLine
                let exists = directorExist director films
                if (not exists) || (director == "") then do
                  putStrLn "director not found, try again"
                  allFansOfDirectorIO films name
                  else do
                    putStrLn "#######################################################################"
                    putStrLn(allFansOfDirector director films)
                    putStrLn "#######################################################################"
                displayMenu films name


--function to add a fan to the film, if the user is a fan already then show msg
--else adds the user as a fan
addFanToFilmIO :: Database -> String -> IO ()
addFanToFilmIO films name = do
                  putStrLn "Please enter title of the film"
                  title <- getLine
                  let fanAlready = name `elem` (allFansParticularFilmList title films)
                  let exists = titleExist title films
                  if exists then do
                    if fanAlready then do
                      putStrLn "#######################################################################"
                      putStrLn "Sorry you are already a fan of this movie"
                      putStrLn "#######################################################################"
                      displayMenu films name
                      else do
                        let newDatabase = addFan name title films
                        putStrLn "#######################################################################"
                        putStrLn "You have been added as a fan of this movie"
                        putStrLn "#######################################################################"
                        displayMenu newDatabase name
                  else do
                    putStrLn "#######################################################################"
                    putStrLn "Sorry, title doesnt exist"
                    putStrLn "Would you like to go to main menu? (Y or N)"
                    putStrLn "#######################################################################"
                    exit <- getLine
                    let doExit = checkIfExit exit
                    if doExit then do
                      displayMenu films name
                    else do
                      addFanToFilmIO films name

--function to exit and save the database into the txt file
exitSaveIO :: Database -> String -> IO ()
exitSaveIO films name = do
  writeFile "films.txt" (show films)
  putStrLn "#######################################################################"
  putStrLn "All changes saved, Bye"
  putStrLn "#######################################################################"

--function to check if the user has inputted an incorrect values when in the main menu of the program
errorIO :: Database -> String -> IO ()
errorIO films name = do
  putStrLn "#######################################################################"
  putStrLn "Invalid input, please enter a number from 0 to 7"
  putStrLn "#######################################################################"
  displayMenu films name

--main function of the program, converts the films.txt file
--into Database and calls getName function to ask the user for their name
main :: IO ()
main = do
        films <- convertToFilm
        putStrLn (displayAllFilms films)
        putStrLn ("************************* All films successfully loaded! *****************************")
        getName films

--function to call the correct function after specified input from the user
option :: String -> Database -> String -> IO ()
option "1" films name = addFilmIO films name
option "2" films name = displayAllFilmsIO films name
option "3" films name = releasedAfterIO films name
option "4" films name = allFilmsParticularUserIO films name
option "5" films name = allFansParticularFilmIO films name
option "6" films name = addFanToFilmIO films name
option "7" films name = allFansOfDirectorIO films name
option "0" films name = exitSaveIO films name
option _ films name = errorIO films name


--main menu for the application, this is displayed as soon as the user enter their name
displayMenu :: Database -> String -> IO ()
displayMenu films name = do
          putStrLn ""
          putStrLn "----------------------------------MAIN MENU-------------------------------------"
          putStrLn "Please choose one of the option below, by entering a corresponding number"
          putStrLn "1. Add a new film to the database"
          putStrLn "2. Display all films from the database"
          putStrLn "3. Display all films released after particular year"
          putStrLn "4. Display all films that particular user is fan of"
          putStrLn "5. Display all fans of particular film"
          putStrLn "6. Become a fan of particular film"
          putStrLn "7. Display all fans of particular director"
          putStrLn "0. Exit and save the database"
          putStrLn "Please enter your option : "
          input <- getLine
          option input films name
