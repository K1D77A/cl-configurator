# cl-configurator
library to quickly load up a config file which is an sexp file
it doesn't do any checking if there is naughty stuff as it just uses
read to load up the sexp...

planning on adding some checks to make sure it doesn't add anything naughty
 
It's pretty simple to use just run the following:
```
(config-to-hash (read-configuration-file <config file location>))
```

This will print a list of names and values, you can then use (access name) to retrieve the value
and (set-access name value) to edit the value.
However changing the value does not permanently change it, so the config file is not rewritten.


Currently only supports floats strings and integers as valid types unless `*allow-all*` is set to true
