# Embedded MUPL
A simple interpreted and compiled DSL implementing closures and basic arithmetic based on the Made-Up-Programming-Language from the UW/Course programming languages course: https://courses.cs.washington.edu/courses/cse341/18au/hw5.pdf. The embedded DSL has the option of being either interpreted or compiled to Python.

## Getting started:

This codebase requires having installations of [Scala](https://www.scala-lang.org/) and [sbt](https://www.scala-sbt.org/).

Once `Scala` and `sbt` are installed, compile the codebase via
```
sbt compile
```

To run the unit tests, run
```
sbt test
```

## Writing embedded MUPL
[App.scala](src/main/scala/App.scala), provides multiple examples of embedded MUPL, including example arithmetic operations, let expressions, and function calls.

Running the command
```
sbt run
```
will run the main application, which will interpret a subset of the embedded MUPL and print the result. A version compiled to python will be produced as well, creating a file called `mupl.py` in the working directory. Adding `print("res = {}".format(res))` at the end of the resulting `mupl.py` will show that the result of running the compiled code is equivalent to the interpreted version.
