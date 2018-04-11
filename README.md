# dnd-chat

This is a basic D&D chat client/server application. The DM runs the server, the
clients connect, and then communicate through the terminal. 

":c" on the server will show current connections.

":q" on the server or client will close the application

## Installation

To install, you must have haskell's "stack" installed.

To get the code, clone this repo (or download it) and then run "stack build"

"stack exec s" will run the server, "stack exec c" will run the client, or you
can find the executables s and c inside the dist folder.

(For linux and mac, these executables are already compiled in the "executables"
folder. For windows, this program doesn't run quite as well anyhow)
