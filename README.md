# cl-configurator
library to quickly load up a config file which is an sexp file
it doesn't do any checking if there is naughty stuff as it just uses
read to load up the sexp...

planning on adding some checks to make sure it doesn't add anything naughty
 
 
Currently if you modify the value of a symbol in the hash-table it makes no 
changes to the object tree and no changes to the configuration file itself
I am currently working on fixing this. The functionality to modify the tree
exists in the code I just have to jam it all together when I have some time.

See Todo for a list of things I am working on currently.
 
 
It's pretty simple to use just run the following:
```
(import-configuration (path &key (from-start *default-from-start*)
				    (depth-of-combination *default-depth*)
				    (ignore-positions *default-ignore*)
				    (all-parents *default-all-parents*)))
```
This function is does the following:
```

  "Imports the configuration, converts it to objects and then puts the leaves into a hashtable,
generating the keyword accessors based primarily on the value of depth-of-combination and 
ignore-positions which is a list, but if you change all-parents to t it'll add all parents
except those listed in ignore-positions"

```

If the configuration file looks like below:

```
;;;;this is the basic configuration file
(:configuration
 (:directory
  (:root "/home/bob/documents/website/bob/"))
 (:misc
  (:username-length 4)
  (:try-again-time 3))
 (:database
  (:name "bobs site")
  (:user "bob")
  (:address "localhost")
  (:password ""))
 (:html
  (:title "im a title")
  (:standard-image "/images/bobbing.gif")
  (:special
    (:numbers
     (:nine 9)
     (:eight 8))))
 (:data
  (:filename "data"))
 (:passwords
  (:login "bigyeetus3")
  (:totals "yeetusmcbeetus2")))
```
Then the output from evaluating (import-configuration ..) with that filename and the
default optional arguments will be as so:

```
|cl-con|> (import-configuration "../test/configuration.sexp")
Name: :PASSWORDS-TOTALS
Value: "yeetusmcbeetus2"
Name: :PASSWORDS-LOGIN
Value: "bigyeetus3"
Name: :DATA-FILENAME
Value: "data"
Name: :NUMBERS-EIGHT
Value: 8
Name: :NUMBERS-NINE
Value: 9
Name: :HTML-STANDARD-IMAGE
Value: "/images/bobbing.gif"
Name: :HTML-TITLE
Value: "im a title"
Name: :DATABASE-PASSWORD
Value: ""
Name: :DATABASE-ADDRESS
Value: "localhost"
Name: :DATABASE-USER
Value: "bob"
Name: :DATABASE-NAME
Value: "bobs site"
Name: :MISC-TRY-AGAIN-TIME
Value: 3
Name: :MISC-USERNAME-LENGTH
Value: 4
Name: :DIRECTORY-ROOT
Value: "/home/bob/documents/website/bob/"
#<ROOT Name: :CONFIGURATION Children: 6 {100354FC23}>
```
You have the symbols used to access the variables in their hash-table and a tree
which represents the configuration file as objects
```
|cl-con|> (access :passwords-totals)
"yeetusmcbeetus2"
T
|cl-con|> 
```
and you can modify that value with:
```
|cl-con|> (set-access :passwords-totals "abcd")
"abcd"
|cl-con|> (access :passwords-totals)
"abcd"
T
|cl-con|> 
```

Another couple examples of importing the configuration are:
```
|cl-con|> (import-configuration "../test/configuration.sexp" :all-parents t)
Name: :PASSWORDS-TOTALS
Value: "yeetusmcbeetus2"
Name: :PASSWORDS-LOGIN
Value: "bigyeetus3"
Name: :DATA-FILENAME
Value: "data"
Name: :HTML-SPECIAL-NUMBERS-EIGHT
Value: 8
Name: :HTML-SPECIAL-NUMBERS-NINE
Value: 9
Name: :HTML-STANDARD-IMAGE
Value: "/images/bobbing.gif"
Name: :HTML-TITLE
Value: "im a title"
Name: :DATABASE-PASSWORD
Value: ""
Name: :DATABASE-ADDRESS
Value: "localhost"
Name: :DATABASE-USER
Value: "bob"
Name: :DATABASE-NAME
Value: "bobs site"
Name: :MISC-TRY-AGAIN-TIME
Value: 3
Name: :MISC-USERNAME-LENGTH
Value: 4
Name: :DIRECTORY-ROOT
Value: "/home/bob/documents/website/bob/"
#<ROOT Name: :CONFIGURATION Children: 6 {1003C35113}>
```
In the example above :all-parents is set to t, so all the parents except those listed in 
:ignore-positions are used to generate the symbol names, :ignore-positions being '(0) which is the root branch of the sexp

the next example is:
```
|cl-con|> (import-configuration "../test/configuration.sexp" :all-parents t :ignore-positions nil)
Name: :CONFIGURATION-PASSWORDS-TOTALS
Value: "yeetusmcbeetus2"
Name: :CONFIGURATION-PASSWORDS-LOGIN
Value: "bigyeetus3"
Name: :CONFIGURATION-DATA-FILENAME
Value: "data"
Name: :CONFIGURATION-HTML-SPECIAL-NUMBERS-EIGHT
Value: 8
Name: :CONFIGURATION-HTML-SPECIAL-NUMBERS-NINE
Value: 9
Name: :CONFIGURATION-HTML-STANDARD-IMAGE
Value: "/images/bobbing.gif"
Name: :CONFIGURATION-HTML-TITLE
Value: "im a title"
Name: :CONFIGURATION-DATABASE-PASSWORD
Value: ""
Name: :CONFIGURATION-DATABASE-ADDRESS
Value: "localhost"
Name: :CONFIGURATION-DATABASE-USER
Value: "bob"
Name: :CONFIGURATION-DATABASE-NAME
Value: "bobs site"
Name: :CONFIGURATION-MISC-TRY-AGAIN-TIME
Value: 3
Name: :CONFIGURATION-MISC-USERNAME-LENGTH
Value: 4
Name: :CONFIGURATION-DIRECTORY-ROOT
Value: "/home/bob/documents/website/bob/"
#<ROOT Name: :CONFIGURATION Children: 6 {1003EADB93}>
```
In this example all parents are used to create the symbols and ignore-positions is set to nil.
By default the root node is always ignored only 1 parent and the entry-name itself is used to generate the symbol.

