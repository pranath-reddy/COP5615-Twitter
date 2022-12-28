%% Author: Pranath Reddy Kumbam
%% UFID: 8512-0977
%% DOSP Project-4 Part-1
%% Mock Twitter Client for Simulation

%% Usage:
%% c(server_simulation).
%% c(client_simulation).
%% server_simulation:main(200).

%% In a different shell:
%% client_simulation:main("10.20.0.124", 200, 0.02).

-module(client_simulation).
-export([main/3]).
-export([client/5]).
-define(PORT, 0476).
-export([counter/1,increment/1,value/1]).
-import(string, [substr/3]).
-import(base64, [encode_to_string/1]).

% Counter for tracking
increment(Counter) ->
  Counter ! increment.
value(Counter) ->
  Counter ! {self(),value},
  receive
    {Counter,Value} ->
      Value
  end.
counter(Val) ->
  receive
    increment ->
      counter(Val + 1);
    {From,value} ->
      From ! {self(),Val},
      counter(Val)
  end.

% Spawn users
spawner(0, _NSub, _Thresh, _L, _Socket) -> io:fwrite("Spawned All Users\nStarting Simulation\n");
spawner(N, NSub, Thresh, L, Socket) ->
  % Args:
  % N - No of users
  % NSub - Distribution of Subscribers
  % Thresh - Threshold for Subs to classify users
  % L - list of IDs
  % Socket for communication

  PID = spawn(client_simulation, client, [N, NSub, Thresh, L, Socket]),
  register(list_to_atom("user" ++ integer_to_list(N)), PID),
  timer:sleep(100),
  spawner(N-1, NSub, Thresh, L, Socket).

% The main client function
main(IP_address, N, T) ->
  % Args:
  % IP_address - Address of the Server
  % N - Number of Users
  % T - Threshold to classify users

  io:fwrite("Started Simulation Client\n"),
  Start_time = erlang:system_time(millisecond),

  Counter = spawn(client_simulation, counter, [0]),

  % Connect to server
  {_, Address} = inet:parse_address(IP_address),
  {_, Socket} = gen_tcp:connect(Address, ?PORT, [binary, {active, true}, {packet, 0}]),

  % Zipf distribution to assign number of followers
  Numbers = [ 1/Number || Number <- lists:seq(1, N)],
  Const = lists:sum(Numbers),
  Constant = math:pow(Const, -1),

  % Follows Zipf distribution and ensures every user has atleast one subscriber
  NSub = [ round((Constant/Number)*N)+1 || Number <- lists:seq(1, N)],
  Thresh = round(T*N),

  % Uncomment for Information
  %% io:fwrite("Distribution of Subscribers: ~p\n", [NSub]),
  %% io:fwrite("Subscribers Threshold: ~p\n", [Thresh]),

  L = lists:seq(1,N),
  spawner(N, NSub, Thresh, L, Socket),
  do_recv(Socket, Start_time, N, Counter).

tweet(0, _Socket, _Username, _NID) -> none;
tweet(N_T, Socket, Username, NID) ->
  % Args:
  % N_T - no of tweets
  % Socket for communication
  % Username - username of user
  % NID - process ID of the user

  Tweet = substr(encode_to_string(crypto:strong_rand_bytes(32)), 1, 32),
  Rem = round(N_T) rem 2,
  RemC = round(N_T) rem 5,

  % Tweet and retweet alternatively
  if
    Rem == 1 ->
      gen_tcp:send(Socket, term_to_binary({"Retweet", Username, NID}));
    % Simulate live connections and disconnections
    RemC == 0 ->
      gen_tcp:send(Socket, term_to_binary({"Connection", Username, NID}));
    true ->
      gen_tcp:send(Socket, term_to_binary({"Tweet", Username, Tweet, NID}))
  end,
  tweet(N_T-1, Socket, Username, NID).

client(N, NSub, Thresh, L, Socket) ->
  % Args:
  % N - no of users
  % NSub - Distribution of Subscribers
  % Thresh - Threshold for Subs to classify users
  % L - list of IDs
  % Socket for communication

  Username = "user" ++ integer_to_list(N),
  Mysub = lists:nth(N, NSub),
  NID = whereis(list_to_atom("user" ++ integer_to_list(N))),
  List = [X||{_,X} <- lists:sort([ {rand:uniform(), N_} || N_ <- L])],
  if
    Mysub > Thresh ->
      Mytweets = 28; % Popular
    true ->
      Mytweets = 14 % Regular
  end,
  MySubs = lists:sublist(List, 1, Mysub),
  MySubNames = [ "user" ++ integer_to_list(Num) || Num <- MySubs],
  TUsers = length(L),
  gen_tcp:send(Socket, term_to_binary({"Register", Username, NID, MySubNames, TUsers})),
  timer:sleep(100*TUsers),
  tweet(Mytweets, Socket, Username, NID),
  do_recv_client(Socket).

% Recieves messages for client
do_recv_client(Socket) ->
  % Args:
  % Socket for communication

  receive
      {ping, Binary} ->
        {_Request, _Data} = binary_to_term(Binary),
        do_recv_client(Socket)
  end.

% Recieves messages from Server
do_recv(Socket, Start_time, N, Counter) ->
    % Args:
    % Socket for communication
    % Start_time - Simulation start time
    % N - no of users
    % Counter - To record the number of tweets

    receive
        {tcp, Socket, Binary} ->
          {Data, NID} = try binary_to_term(Binary) catch _:_ -> {"Invalid Message", none} end,

          % Uncomment for Information
          %% io:fwrite("Notification: ~p\n", [Data]),

          if
            Data == "SimComplete" ->
              io:fwrite("Simulation Complete!\n"),
              SimTime = (erlang:system_time(millisecond) - ( Start_time + (100*N) + 2000 ))/1000,
              Count = value(Counter),
              io:fwrite("Tweets Convergence Time: ~p Seconds\n", [SimTime]),
              io:fwrite("Total Tweets Delivered: ~p\n", [Count]);
            true ->
              if
                NID /= none ->
                  increment(Counter),
                  NID ! {ping, term_to_binary({"Ping", Data})};
                true ->
                  none
              end,
              do_recv(Socket, Start_time, N, Counter)
          end
    end.
