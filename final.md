% Networking Final Project: D&D Chat
% Logan Woodbury

# What did you learn doing your final project?

I learned that even simple ideas can often seem complicated to handle on the
code level, especially when it comes to threading. Networking requires more
threading than you would think. For instance, on a server you have to have many
threads in order to handle accepting connections, different inputs from
different connections, inputs from the user themselves. Having to create
threads with appropriate lifespans using haskell's version of "fork" was a bit
difficult.

I learned that weird things can happen over the internet that won't happen just
running the server and client on the local machine, so you can't always trust
the behavior of the local server to match that of a network server.

Also, the main reason I did this project was learning how to do networking in
Haskell, so I got a little bit of experience doing it using the "network"
package. It is quite similar to C++, though a little bit of the complicated
bits can be avoided.

Not related to networking, I also learned a bit about ANSI in terminals and
whatnot.

# Time spent

I estimate I spent about 11 hours on the project. About 5 of those were on the
UI though, and then 6 on getting the separate client and server clients to
behave correctly.

# Project README

The README.md file has instructions for installing and running. However, to let
you know, I have already compiled executables in the submitted folder (does
not seem to work quite as well on windows command prompt or powershell).

# Advice for future generations

I won't be around, but ask your friends if they've taken it before, and ask
them if they rememeber any web technologies that people seemed to like in
particular. If you plan on doing networking stuff a lot, then you can check an
in-depth and deep library, but for this lab you might want to try something a
bit easier.

# Grade earned

I believe that I earned a 100%, as far as I can tell. There isn't too much of a
rubric for this assignment, but I learned something through this project and
put in the required work.
