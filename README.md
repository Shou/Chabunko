Chabunko
========

A simple websocket based web chat with a planned IRC interface.

`Chasocketo.hs` is the websocket server.  
`base.html` combined with `Chabunko.hs` is the interface.

# Protocol

The websocket server itself can receive eight different types of commands.

## Protocol input

### Msg
"msg <message>" - "msg" followed by a space and then the contents of the
message itself.

This sends a message to the server which then relays it to everyone except the
sender.

This can return a warning if the user is flooding/spamming.

### Set
"set <key> <data>" - "set" followed by a space, then the key which cannot
contain any spaces, another space after that, and finally the data you want to
associate with the key.

This is a general way to store settings, however as an example, you can use it
to store users' avatar URLs.

### Opt
"opt <key>" - "opt" followed by a space and then they key which cannot contain
any spaces.

Returns what has been set by every user for the specified key.

### Opts
"opts"

Returns the keys of the stores options.

### Req

### List

### Ban

### End

## Protocol output

