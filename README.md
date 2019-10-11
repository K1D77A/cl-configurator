# cl-configurator
library to quickly load up a config file which is an sexp file
it doesn't do any checking if there is naughty stuff as it just uses
read to load up the sexp...

planning on adding some checks to make sure it doesn't add anything naughty
 
It's pretty simple to use just run the following:
```
(import-configuration <path-to-config>)
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
Then the output from evaluating (import-configuration ..) with that filename will be as so:

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

Currently if you modify the value of a symbol in the hash-table it makes no 
changes to the object tree and no changes to the configuration file itself
I am currently working on fixing this. The functionality to modify the tree
exists in the code I just have to jam it all together when I have some time.

See Todo for a list of things I am working on currently.

