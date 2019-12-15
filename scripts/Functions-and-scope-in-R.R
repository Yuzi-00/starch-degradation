
## Hi Yuzi,
##
## Please run this code in a completely clean R session!
##
## Note - I often use a suffix= '()' when referring to a function, and
## a suffix '=' when referring to a function argument.  e.g. 'nls()',
## and 'data='.  I think this makes things more readible...
##
## Note - some of the code below may produce errors - this may be
## deliberate!  I will point out in my comments when a code should
## have produced an error...
##
## OK.  I mentioned that I would write you some examples to help
## clarify R's rules about 'scope'.
##
## As I mentioned before, large programs (e.g. like the application
## Microsoft Word) are constructed from many lines of code (hundreds
## of thousands lines?  Millions of lines?) of code, written by many
## (hundreds?) of people.  In any such program, there will be many
## hundreds, or maby thousands of 'names' that are referred to - names
## of data structures, or names of functions.  And names will OFTEN be
## re-used to for different things.  Names like 'i' and 'j' are used a
## LOT as in programs as loop counters, e.g.: `for (i in 1:10)...`.
##
## So, it is absolutely vital for a programming language to have a way
## to prevent any confusion between different objects that have the
## same name in a program.  It is just not possible to expect all of
## the programmers that are working on a program to remember to use
## different names for every single data structure or function.
##
## So, programming languages include 'scoping rules' - the idea is
## that program code can be organised into different kinds of
## 'containers', with rules about what names can be seen inside, and
## outside, the different containers. A named object therefore has an
## associated 'scope', and only exists within its scope.  Outside that
## scope, the object cannot be seen, and another object can have the
## same name, and there will not be any confusion between the two
## objects.
##
## In R, there are two major scoping 'containers' - one is called an
## 'environment', and other is called a 'namespace'.  Namespaces are
## associated with R packages - I will talk about them later.
## Environments are associated with R functions, and the R 'global
## environment'.
##
## When you start R, and type commands at the command line (or when
## you send commands from your R script to the R session), you are
## working in the 'global environment'.  The names of any data objects
## or functions that you create using '=' or '<-' are stored in this
## global environment.  The ls() function shows you what names are
## defined in the global environment (and the RStudio 'Environment'
## tab also shows you what objects exist in the global environment).

ls()
## If your session is completely clear, you should see 'character(0)',
## meaning no object names were found.

x <- 1
y <- 2

ls()

## The global environment is called ".GlobalEnv", and we can specify
## the environment explicitly in ls()

ls(name=".GlobalEnv")

## ls() reports the names of objects in the current environment by
## default - when you are working at the command line, the current
## environment is the global environment.

## Now, lets define a simple function:
myfunc <- function()
{
  a <- 10
  b <- 20
  print(ls())
  a
}
## Running the above block of code does not _run_ the function, it
## only defines it.  If we check the global environment:

ls()
## ...we see that 'myfunc' now exists....

class(myfunc)
##..and it is a function.

## From the definition, myfunc() does not accept any arguments
myfunc("blah blah blah")
## The above line should generate an 'unused argument' error.  This
## means that we cannot pass any data to myfunc() (at least, not
## directly).

## Inside myfunc(), we see that two new variables are created, 'a' and
## 'b'.  Then the function will print out the result of running ls()
## inside the function, and finally, the function will return the
## value of a.  Lets run the function properly now:

myfunc()  ## no arguments!
## You should see:
##
## [1] "a" "b"
## [1] 10
##
## The first line above is the result of running ls() inside the
## function.  Notice that only "a" and "b" appear, not "x" or "y" (or
## "myfunc"!).  This is because when myfunc() is running, the names of
## the variables that are created within myfunc() are not stored in
## the global environment.  Instead, they are stored inside a 'local'
## environment, that is created for myfunc() when it is called.  When
## the function is finished running, that local environment
## disappears, (and so does 'a' and 'b').

## The second result above is just the value of 'a' that was passed
## back by the last line of the function.  Te default behaviour for a
## function is to return the last result that was created before the
## function ends.  We can also define myfunc() like this:
myfunc <- function()
{
  a <- 10
  b <- 20
  print(ls())
  return(a)
}
## ...and it will produce the same result.  I prefer using the
## 'return()' function in a function definition - I think it makes it
## clearer, what the function will return as a result.

## What objects exist now in the global environment?
ls()

## Note that "a" and "b" are not there - they only existed briefly
## while the function was running, and they never existed in the
## global environment - they only existed in the local environment
## that was temporarily created while myfunc() was running.

## Now lets do this:
a <- myfunc()
## We still see the result of printing the ls() call inside myfunc(),
## but now the result (10) that is returned from myfunc is being
## stored, under the name "a"

ls()
## OK, so "a" now exists in the global environment, but this is NOT
## the same "a" that was defined within myfunc()!  This is a different
## data object, which just happens to have the same name as "a" in
## myfunc(), and also happens to contain the contents of "a" in
## myfunc() that existed briefly, while myfunc() was running.

## OK, lets clean up a little:
rm(a,myfunc)  ## Remove a couple of things we don't want any more.

ls()
## Back to just x and y.

## Lets define a new function.

myfunc2 <- function(a)
{
  cat("\nValue of 'a' BEFORE it is changed",a,"\n")
  a <- a + 1
  cat("\nValue of 'a' AFTER it is changed",a,"\n\n")
  b <- 20
  return(a + b)
}
## I have added a couple of lines to print out the content of 'a'
## before, and after it is changed.  the '\n' symbol prints a 'new
## line', so that the output is nicely formatted...

## Here, a has been _passed_ to myfunc2().  myfunc2 will add 1 to a,
## then it will define b = 20, and then myfunc2 will return the value
## of a + b.

myfunc2()
## This should produce an error - myfunc2 expects to be passed exactly
## one argument, and I didn't pass anything.  Lets try again:

myfunc2(1)
## The result that is returned is 22 = 1+1+20.

ls()
## Again, a and b only existed while myfunc2 was running.

## Now, lets try this:
myfunc2(y)

## What exactly is happening here?  y was defined to be 2 above.  When
## we call myfunc2(), and pass y to myfunc2(), R makes a _copy_ of the
## content of y, and calls this copy 'a', and this is stored in the
## _local_ environment of the function.  While myfunc2() is running, a
## gets redefined to 'a + 1' (so myfunc2() changes the content of
## 'a'), but 'a' is just a copy of y, so y remains unchanged:

y
## Still 2, not 3!

## Now, lets do this...

a <- 50  ## Remember - NOT the same 'a' as in myfunc2()!

myfunc2(a)
## The result is 50 + 1 + 20.  We also see that inside myfunc2, 'a'
## was changed from 50 to 51.  However, 'a' inside myfunc2 was only a
## _copy_ of the 'a' that was passed through from the global
## environment.  'a' in the global environment has not been changed:

a
## Still 50.

## So, variables that are _created_ inside a function (like 'b' in
## myfunc2() ) are _local_ to that function - they only exist until
## the function is finished running.  Variables that are _passed_ to a
## function (like 'a' in myfunc2() ) are also _local_ to that
## function, and they are a _copy_ of the object that was passed to
## them.

## Variables defined inside, and outside a function, can have the same
## name, but they are are not the same variable!  Even if you pass a
## variable to a function via a function argument (like 'a' in
## myfunc2() ), the version of the variable inside the function is a
## copy of the variable that was passed:
myfunc2(y)
## 'a' contains a _copy_ of the content of y, but is not the same
## data object as y.

## This is all because functions in R each have their own local
## environment when they are run.  We can also say that each function
## has its own local 'scope'.

## Lets clean up again:
rm(a,myfunc2)
ls()

## Unfortunately, there is one more feature of R's scoping rules that
## does make things a little more complicated.  That feature is that
## functions CAN see OUTSIDE of their local scope!  If you refer to
## the name of an object inside a function that was not CREATED inside
## that function, and was not PASSED to that function, then the
## function will look OUTSIDE its local scope (into the global scope)
## to see if it can find an object with that name.

myfunc3 <- function()
{
  return(x + 1)
}
## This function accepts no arguments, and it does not DEFINE x inside
## the function, it just refers to x.  In other programming languages,
## this might represent a programming error, but in R, the function
## will run just fine:

myfunc3()
## ...and it returns 2 = 1 + 1.  x in the global scope is 1:

x
## When myfunc3 runs, R sees that it refers to a variable 'x'.  But
## 'x' has NOT been passed to the function.  Also, x has NOT been
## created inside the function using '<-' or '='.  So R will look
## outside the function's local scope, in the global scope, to see if
## it can find 'x'.  And it does find x!  And so it prints the result
## of adding 1 to x.

## Clean up.
rm(myfunc3)


## What happens if a function tries to _change_ a variable that is not
## in the local scope of the function?  Here's a modified version of
## myfunc3():
myfunc4 <- function()
{
  x <- x + 1
  return(x)
}

## Remember, x exists in the global environment, and it equals 1
x

## Now, what happens when we run myfunc4()?
myfunc4()
## OK, so myfunc4() returns x + 1 = 1 + 1 = 2.  But it also stored the
## result of 'x + 1' as 'x' inside the function.  Has this changed x
## in the global scope?

x
## No!  x in the global scope stays the same.  So what happened?  When
## myfunc4() ran, R saw the expression 'x <- x + 1' In order to
## executre this line of code, R needed to evaluate the right-hand
## side of the expression, 'x + 1'.  It looks for 'x' in the local
## scope of the function, but 'x' does not exist in the local scope.
## So R looked OUTSIDE the local scope, in the GLOBAL scope, and found
## 'x'.  R then calculated 'x + 1' and got 2.  It then needed to store
## this result, under the name 'x' (from the 'x <-' part of the
## expression).  When a variable is created inside a function using
## '<-' or '=', it is created as a LOCAL variable (i.e, it is created
## as a variable in the local environment of the function).  So,
## rather than changing the value of 'x' in the GLOBAL environment, R
## creates a new variable called 'x' in the LOCAL environment of the
## function.  And stores '2' in it.  This local version of x
## disappears when the function finishes, leaving only the global
## version of x, which was USED by myfunc4(), but was not CHANGED by
## myfunc4().

## So, functions can look outside their local scope when they find a
## variable name they don't recognise.  This can sometimes be a useful
## feature in R, but it can also be confusing (and can cause serious
## progamming errors!) if you are not careful.  It is generally a good
## idea to avoid referring to a variable in this way.  Ideally, if you
## write a function that needs to use an object that you have created
## in the global space, you should pass that object through using a
## function argument, rather than referring to the object directly.
##
## So, when you write a function, think about all of the data oject
## names that you refer to in the function.  Are they created inside
## the function? (This is good.) Are they passed to the function as an
## argument? (This is also good.).  Or are you referring to an object
## that actually exists outside the local scope of the function,
## without passing the object through as an argument. (This is not so
## good, and I suggest that you change it, to avoid confusion and
## errors in your code!)

## Remember that above, I pointed out that when I used '<-' to define
## 'x' inside myfunc4() (ie.e x <- x + 1'), this did NOT change x in
## the global environment.  Instead, a new local version of 'x' was
## created.  This is a VERY GOOD THING!  R code could get VERY
## confusing, and could result in lots of errors that are very hard to
## find, if functions were able to directly change (or create)
## variables that lie outside their local scope!
##
## (Actually, R is very flexible, and there IS a way to allow a
## function to directly change a variable outside of its local scope.
## But this is a very bad idea, and you won't ever need to do this.
## And so I am not going to tell you how to do this :-) )

rm(myfunc4)

######################################################################
######################################################################


## Remember that every argument to a function has a name (which you
## can see from the argument definition, or from the documentation for
## the function.

myfunc5 <- function(x)
{
  x <- x + 1
  return(x)
}
## There is one argument to myfunc5, and it is called 'x'.  We can
## pass lots of different data objects to myfunc5() - we just need to
## make sure that it is sensible to add 1 to the object.

myfunc5(3)

myfunc5(1:5)

mm <- matrix(1:9,ncol=3)
mm  ## a matrix object

myfunc5(mm)  ## Add 1 to each element of the matrix

myfunc5("blah blah blah")
## Error message - you can't add 1 to a character string!

## We sometimes refer to 'x' as a 'dummy variable' - it only exists
## while the function is running, and its purpose is to store a copy
## of whatever was passed to the function.  When we call the function,
## we can refer to x explicitly in the call:

myfunc5(x = 3)

myfunc5(x = 1:5)

myfunc5(x = mm)

## Rememver that we also have 'x' defined in the global environment:

x

## So we could call myfunc5 like this:

myfunc5(x)

## ...or like this
myfunc5(x = x)
## In this call, we need to remember that the 'x' on the RIGHT-hand
## side of the '=' is the name of the object we are PASSING to the
## function, from the global scope.  And, the 'x' on the LEFT-hand
## side of the '=' is the name in the local scope of the function that
## will store a copy of the object being passed to the function.  They
## look the same, but they are actually two different variables,
## stored in two different environments.


## When you write a function, it is a good idea to remember this
## difference between the name of an argument (representing the name
## of a variable stored in the local scope of the function), and the
## names of the objects that you might PASS to the function.  It is
## often a good idea to use a more 'general' name for the function
## argument, to make sure you don't get confused about the names.
##
## So, if I have a data frame called 'mydata_01_A', and I want to
## write a function to do something to this data frame, I will pass
## the data frame to the function, and I will use a name for the local
## variable that is 'general', like 'd', or 'dd', or 'dfr'' or 'data':

manipulate_dataframe <- function(dfr)
{
  ## do some stuff to the data frame, and return the result!
  ## .
  ## .
  ## .
  ##
  return(dfr)
}
## Note - the above function is just an example, that doesn't actually
## do anything except return the data you passed to it!

## Then, I can call the function on my data frame using
##
##  manipulate_dataframe(mydata_01_A)
##
## ..or
##
## manipulate_dataframe(dfr = mydata_01_A)
##

## It is probably not a good idea to define my function like this:
manipulate_dataframe <- function(mydata_01_A)
{
  ## do some stuff to the data frame, and return the result!
  ## .
  ## .
  ## .
  ##
  return(mydata_01_A)
}
## ...because this may could confusion for the person reading the code
## (which might be me, or someone else!), between the object name in
## the global scope, and the object name in the local scope of the
## function...

## I will quite often define a function with an argument name like 'x'
## or 'y' or 'dfr' (I use 'dfr' a lot when naming a data frame), and I
## may have objects in the global scope that have the same name.  But
## I am familiar with the R scoping rules now, so I don;t get confused
## by this.  Until you are comfortable with all of the ideas above, I
## recommend you take care in how you name function arguments, to
## avoid getting confused between argument names (object names defined
## in the local scope of a function) and the objects passed to the
## arguments (objects names defined in the global scope).

## Final thing: The function environment() returns the environment you
## are currently in.
environment()
## This does not look exactly like '.GlobalEnv' as I mentioned before,
## it IS referring to the same thing!

## I can define a function that similarly prints the local environment
## that is created when the function is run:
myfunc6 <- function()
{
  print(environment())
}

myfunc6()

myfunc6()

myfunc6()

myfunc6()
## The local environment of a function doesn't have a nice name like
## '.GlobalEnv' or 'R_GlobalEnv', it has a code.  This is because the
## computer has to create a new local environment, every time a
## function is called.
##
## You can see above that a new environment is created every time
## myfunc6() is called.  This environment exists briefly while the
## function is running, and then it disappears when the function
## finishes.  R 'cleans up' when the function is finished.

## And that is enough for now about environments and scope.  If you
## read this stuff, please do let me know if anything I have written
## above is unclear!  I may modify this script (to remove your name
## :-) ), to use as a general tutorial for others.


######################################################################
######################################################################

## If you want, you could stop reading this now :-) I've given you a
## lot to think about above, and the stuff above about global and
## local scope, and function arguments is the most important stuff to
## know, to avoid making errors in your programming.  But I also
## mentioned that I would talk about packages and namespaces, so I
## will do that now!  If you are interested, read on.

## Above, I have mostly talked about the names of data structures -
## vectors, data frames, lists, matrices.  But R also has functions
## (of course!), and functions have names as well.  When you refer to
## the name of a function (for example, when you call a function like
## nls() to fit a model), R has to find the name of that function so
## that it can read the code for the function, and then execute that
## code.  There are a LOT of functions available in R - just look at
## the help documentation!

ls()  ## This is itself, a function!

## But you will notice that when we list the objects in the global
## environment, we only see the objects that I have created above (and
## didn't remove).  Three of these are functions - myfunc5(),
## myfunc6(), and manipulate_dataframe().  But there are many, many
## more functions that we can call if we want to (including ls()!),
## and we don't see those in the global environment.  So where are
## they?

## Let's introduce the search path:
search()
## This is a set of locations that R will search through, every time
## you refer to the name of a data object, or a function.  You can see
## that the global environment is the first entry in that list.  So,
## if you refer to the name of an object or function, R will search
## for that name in the global environment first.  Then, it will
## search the next location in the list, and then the next location,
## and so-on until it has searched all of the locations named in the
## list.  When your code refers to a name from inside a function, R
## will search in that function's local scope first, THEN in the
## global scope, and then will start working through the other
## locations in the search path.

## Many of these search path locations are packages. A package is
## usually just a collection of functions and sometimes datasets.
## Some packages are loaded automatically when R starts a new session,
## these are packages 'graphics', 'grDevices', 'stats', 'utils'
## 'datasets', 'methods', 'base'.

head(ls(name = "package:stats"),n=10)
## Names in "package:stats".  There are A LOT of names in
## package:stats, so I have used head() to just print the first 10...

## When we use library() or require() to load a new package, these
## will be added to the search path:
require(tidyverse)
## 'tidyverse' actually loads several other packages - dplyr, ggplot2,
## readr, tidyr, tibble, stringr, forcats.

search()
## And there they are, in the search path.

## Packages have their own 'environments' for storing the names of the
## functions and data structures they contain, but they work a little
## differently to standard R environments, and they are called
## 'namespaces' instead of 'environments'.  The main thing you really
## need to know about package namespaces is that, for new packages,
## you need to use library() or require() in a session to 'load' a
## package (as with the tidyverse example above), before R will have
## access to the package namespace.  R does not know about the
## contents of a package until its namespace has been added to the
## search path in a session.  RStudio may provide a convenient way to
## add useful packages to the search path automatically at the start
## of a session.

## There is one other thing that you might find helpful to know about
## namespaces: You can refer to the name of a package function without
## using library() or require() first, by using the '::' operator,
## like this:

dplyr::filter(iris,Species == "setosa")
## ('iris' is a famous dataset that is available in R).  Just type
## 'iris' if you want to look at it, and there will be help for this
## dataset in the RStudio R help system...

## The above call would work even if I had not used require(tidyverse)
## to load the 'dplyr' package.  You might sometimes see me use '::'
## in code that I send to you.  This is usually just because I needed
## to use just one function from a package, and decided not to bother
## loading the entire package...

## The function filter() is actually interesting, because there is
## another function called filter() that is part of the stats package
## in R.  So there is dplyr::filter() (which we use to subset a data
## frame), and there is stats::filter() which does something
## completely different, and has very different arguments to
## dplyr::filter() !
##
## Previously I have loaded the tidyverse packages including dplyr.
## If I run the code

filter(iris,Species == "setosa")

## ...R will run the dplyr version of filter(), to subset the dataset.
## If R had run the stats version of filter, there would have been an
## error message:

stats::filter(iris,Species == "setosa")

## This is because the arguments I am specifying do not make sense to
## stats::filter().

## So, when I run
filter(iris,Species == "setosa")
## ...to subset the data frame, how does R know to use
## dplyr::filter(), and not stats::filter()?  Check the search path:

search()

## Notice that "package:dplyr" appears BEFORE "package:stats" in the
## search path.  When R sees the name 'filter' (without any ::
## prefix), it searches the locations in the search path, starting at
## the beginning of the search path (".GlobalEnv") and working towards
## the end of the path "package:base".  As soon as R finds the name
## 'filter', it will assume this is the right function, and will use
## it.  And the first version of 'filter' it finds is in
## "package:dplyr".
##
## When I loaded the tidyverse packages with require(tidyverse) above,
## there were some messages printed.  Those messages included the
## lines:
##
## -- Conflicts ---------------------------- tidyverse_conflicts() --
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag( )
##
## This was warning you that two functions in the dplyr package would
## 'mask' (or 'hide') functions having the same name in the stats
## package.

## This sort of thing doesn't happen very often.  It is usually a good
## idea to try to avoid defining new functions that have the same name
## as other, standard functions in R.  When the author of dplyr,
## Hadley Wickham, was deciding what to name the dplyr::filter()
## function, he would have realised that a filter() function already
## existed in the stats package.  And I am sure that Hadley would have
## asked himself whether it was a good idea to re-use the name for a
## different function!  But he obviously wanted to use 'filter' for
## the name of his dplyr function, and he probably decided that there
## wouldn't be too much confusion if he used the same name.  And the
## same for dplyr:lag().



