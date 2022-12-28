# Twitter Clone

Developed as part of coursework for COP5615 - Distributed Operating System Principles  
  
**Programming Language:** Erlang

## Description

This project involves the development of a Twitter clone with support for various functionalities and a client tester/simulator. The Twitter engine developed in this first part of the project will be paired up with WebSockets in the second part of the project in order to provide a complete Twitter-like functionality.

## Implementation Details

The implementation has been divided into two separate processes: the client part and the server part. We have a server that distributes the tweets, and multiple independent clients that send and receive tweets working in a single server, multi-client environment. The server and clients communicate over a TCP connection. The clients have information regarding the IP address and the port number that are needed to establish a connection with the server. The server receives and establishes connections with the clients and stores all the information, such as usernames, passwords, tweets, and followers list in ETS (Erlang Term Storage) tables. ETS tables have been used since they are easy to implement, efficient, and fast. ETS table can be easily replaced with a database for future implementations. The implementation supports various functionalities like user registration, tweets, retweets, subscribe, queries, etc, and each functionality has been modeled as a separate service.

## Execution

### Program

<ins>**Server:**<ins>

**Compile:** ```c(server).```   
**Execute:** ```server:main().```   

<ins>**Client:**<ins>

**Compile:** ```c(client).```   
**Execute:** 
```client:main(IP Address, "Register").```   
```client:main(IP Address, "Login").```   

### Simulation

<ins>**Server:**<ins>

**Compile:** ```c(server_simulation).```   
**Execute:** ```server_simulation:main(N).```   

<ins>**Client:**<ins>

**Compile:** ```c(client_simulation).```   
**Execute:** ```client_simulation:main(IP Address, N, T).```   
* *N - Number of users*  
* *T - Threshold*  
* *IP Address - The IP address required to connect to the server*  

## Report 
  
[Report]()  
