https://craftinginterpreters.com/functions.html#function-objects

Implementing custom functions.

i.e
```lox
fun sayHi(first, last) {
 print "Hi, " + first + " " + last + "!";
}
sayHi("Dear", "Reader");
```

The parsing for the func definition and the func call are implemented.

Work to do:
- Store the `IDENTIFIER:sayHi -> code to run` in interpreter
- Lookup this code
- Run this code
