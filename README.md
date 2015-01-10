# s4_intro
worked examples from Genolini's A (Not So) Short Introduction to S4 (2008)

http://christophe.genolini.free.fr/webTutorial/notSoShort.php

# Why Objects?
 - minimize execution errors
 - minimize type errors
 - execute validity checking
 - support inheritance
 - encapsulate the object and its tools

# set Class Syntax
setClass(Class, representation, prototype, contains=character(),
         validity, access, where, version, sealed, package,
         S3methods = FALSE, slots)

# setMethod Syntax
setMethod(f, signature=character(), definition,
          where = topenv(parent.frame()),
          valueClass = NULL, sealed = FALSE)

# setGenric Syntax
setGeneric(name, def= , group=list(), valueClass=character(),
           where= , package= , signature= , useAsDefault= ,
           genericFunction= , simpleInheritanceOnly = )
