# Distributed-Lab2

Gary Gunn
ID: 12306321
Email: ggunn@tcd.ie

Setup Instructions:

- git clone https://github.com/Garygunn94/Distributed-Lab2.git
- sudo apt-get update
- sudo apt-get install haskell-platform (This should install everything needed)
- cd Distributed-Lab2
- chmod +x compile.sh (may not be needed, but I was required to do it)
- ./compile.sh
- chmod +x start.sh (again may not be needed)
- ./start.sh [portnum]
- Clients can connect via 'telnet [host-ip-address] [portnum]'

Edit: In start.sh there is a bash function and a commented out line that allows for the ip address of a linux machine to be returned and inserted as a command line argument. This has been disabled and a default value of 'localhost' is being used in order to ensure the code will work properly, however this should be swicthed if the user is using linux.
