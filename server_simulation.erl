%% Author: Pranath Reddy Kumbam
%% UFID: 8512-0977
%% DOSP Project-4 Part-1
%% Mock Twitter-Engine for Simulation

%% Usage:
%% c(server_simulation).
%% c(client_simulation).
%% server_simulation:main(200).

%% In a different shell:
%% client_simulation:main("10.20.0.124", 200, 0.02).

-module(server_simulation).
-export([main/1]).
-export([counter/1,increment/1,value/1]).
-define(PORT, 0476).

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

% Server function accepts connections using gen tcp and handles requests
main(N) ->
  % Args:
  % None

  io:fwrite("Twitter Engine Started!\n"),
  initiate_tables(),

  % Get the IP Address of the server
  {ok, L} = inet:getif(),
  S_IP = element(1, hd(L)),
  io:fwrite("Server IP: ~p\n", [S_IP]),
  Counter = spawn(server_simulation, counter, [0]),

  % Receive connection from Client
  {_, LSocket} = gen_tcp:listen(?PORT, [binary, {keepalive, true}, {reuseaddr, true}, {active, true}, {packet, 0}]),
  accept_connections(LSocket, Counter, N).

% Accepts connections and incoming requests
accept_connections(LSocket, Counter, N) ->
    % Args:
    % LSocket: Socket for communication

    {_, Socket} = gen_tcp:accept(LSocket),
    io:fwrite("Client Connected\n"),
    do_recv(Socket, Counter, N).

% Recieves messages from Client and handles requests
do_recv(Socket, Counter, N) ->
    % Args:
    % Socket for communication
    % Counter - Counter for tracking users
    % N - Number of users

    receive
        {tcp, Socket, Binary} ->
          Tuple = try binary_to_term(Binary) catch _:_ -> {"Tweet", "user1", "Invalid Message", none} end,
          Request = element(1, Tuple),
          if
            (Request =:= "Register") ->
              increment(Counter),
              {_Request, Username, NID, MySubNames, T} = Tuple,
              ets:insert(subscribers, {Username, MySubNames, NID}),
              _ = registration_service(Username, NID),
              Count = value(Counter),
              if
                Count == T ->
                  io:fwrite("All users succesfully registered\nPerforming Simulation Requests\n");
                true ->
                  none
              end;

            (Request =:= "Tweet") ->
              {_Request, Username, Tweet, NID} = Tuple,
              % Uncomment for Information
              %% io:fwrite("\n*********************************\n"),
              %% io:format("The user ~p tweets: \n", [Username]),
              %% io:format("~p\n", [Tweet]),
              %% io:fwrite("*********************************\n\n"),
              Out = ets:lookup(accounts, Username),
              {_, _, CStatus} = lists:nth(1, Out),
              if
                CStatus == "online" ->
                  tweet_service(Username, Tweet, Socket, NID, "Tweet");
                true ->
                  none
              end;

            (Request =:= "Retweet") ->
              {_Request, Username, NID} = Tuple,
              retweet_service(Username, NID);

            % Switch connection status
            (Request =:= "Connection") ->
              {_Request, Username, NID} = Tuple,
              Out = ets:lookup(accounts, Username),
              {_, _, CStatus} = lists:nth(1, Out),
              if
                CStatus == "online" ->
                  ets:insert(accounts, {Username, NID, "offline"});
                true ->
                  ets:insert(accounts, {Username, NID, "online"})
              end;

            (Request =:= "Invalid Request") ->
              io:fwrite("Warning: Recieved Invalid Request \n Please Restart Server\n"),
              exit(self(),kill);
            true ->
              gen_tcp:send(Socket, term_to_binary({"Invalid Request", "None"}))
            end,
            do_recv(Socket, Counter, N)

    after 2000 ->
      Count = value(Counter),
      if
        Count == N ->
          io:fwrite("Simulation Complete!\n"),
          io:fwrite("Sending Signal!\n"),
          gen_tcp:send(Socket, term_to_binary({"SimComplete", "None"}));
        true ->
          do_recv(Socket, Counter, N)
      end
    end.

% initiate ets tables
initiate_tables() ->
  % Args:
  % None

  ets:new(accounts, [set, named_table, public]),
  ets:new(tweets, [bag, named_table, public]),
  ets:new(subscribers, [bag, named_table, public]).

% Registers user by adding to ets table
registration_service(Username, NID)->
  % Args:
  % Username: Username (ID)
  % Password: Password (PWD)

  Status_0 = "Fail: User ID Already Exists",
  Status_1 = "Success: User Registered Successfully",
  Out = ets:lookup(accounts, Username),
  if
    length(Out) > 0 ->
        Status_0;
    true ->
        ets:insert(accounts, {Username, NID, "online"}),
        Status_1
  end.

% Distribute tweet to subsribers who are online
distribute(Socket, Message, Username) ->
  % Args:
  % Socket for communication
  % Message to be passed
  % Username: Username (ID)

  Out = ets:lookup(accounts, Username),
  {_, NID, CStatus} = lists:nth(1, Out),
  ets:insert(tweets, {Username, Message, Socket}),
  if
     CStatus == "online" ->
       gen_tcp:send(Socket, term_to_binary({Message, NID}));
     true ->
       none
  end.

% Store and distribute tweets
tweet_service(Username, Tweet, Socket, NID, Mode) ->
  % Args:
  % Username: Username (ID)
  % Tweet: User's tweet
  % Socket for communication
  % NID - User process ID
  % Mode: tweet or retweet

  ets:insert(tweets, {Username, Tweet, Socket}),
  if
    Mode == "Tweet" ->
      Message = Username ++ " tweeted: " ++ Tweet;
    true ->
      Message = Username ++ " retweeted: " ++ Tweet
  end,
  gen_tcp:send(Socket, term_to_binary({Message, NID})),
  Out = ets:lookup(subscribers, Username),
  {_, Sub_list, _} = lists:nth(1, Out),
  if
    length(Sub_list) > 0 ->
      _ = [distribute(Socket, Message, U) || U <- Sub_list];
    true ->
      none
  end.

% retweet latest tweet in the feed of the user
retweet_service(Username, NID) ->
  % Args:
  % Username: Username (ID)
  % NID - User process ID

  Tweet_list = ets:lookup(tweets, Username),
  Latest_Tweet = lists:last(Tweet_list),
  {ID, Twt, Sock} = Latest_Tweet,
  tweet_service(ID, Twt, Sock, NID, "Retweet").
