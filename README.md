Chabunko
========

A simple websocket based web chat with a planned IRC interface.

`Chasocketo.hs` is the websocket server.  
`base.html` is the interface.  
`Chabunko.hs` is the back-end spitting out the interface.

# Protocol

The websocket server itself can receive eight different types of commands.

## Protocol input

### Msg
"msg \<channel\> \<message\>" - "msg" followed by a space, a channel (no spaces),
another space, and then finally the contents of the message itself.

This sends a message to the server which then relays it to everyone except the
sender.

This can return a warning if the user is flooding/spamming.

### Set
"set \<key\> \<data\>" - "set" followed by a space, then the key which cannot
contain any spaces, another space after that, and finally the data you want to
associate with the key.

This is a general way to store settings, however as an example, you can use it
to store users' avatar URLs.

### Join

### Part

### Opt
"opt \<channel\> \<key\>" - "opt" followed by a space, a channel (no spaces),
another space, and then they key which cannot contain any spaces.

Returns what has been set by every user for the specified key.

### Opts
"opts \<channel\>"

Returns the keys of the stores options.

### Req

### List
"list \<channel\>"

Returns the userlist for a channel.

### Ban

### End

## Protocol output

