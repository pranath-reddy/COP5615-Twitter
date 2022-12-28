%% Author: Pranath Reddy Kumbam
%% UFID: 8512-0977
%% DOSP Project-4 Part-1
%% Mock Twitter Client

%% Usage:
%% c(server).
%% c(client).
%% server:main().
%% client:main("10.20.0.124", "Register").
%% client:main("10.20.0.124", "Login").

-module(client).
-export([main/2]).
-define(PORT, 0476).

% Get user username
get_username() ->
  % Args:
  % None

  {ok, Username} = io:read("Enter Username: "),
  io:format("The username you entered is: ~p\n", [Username]),
  Username.

% Get user password
get_password() ->
  % Args:
  % None

  {ok, Password} = io:read("Enter Password: "),
  io:format("The password you entered is: ~p\n", [Password]),
  Password.

% The main client function
main(IP_address, Req) ->
  % Args:
  % IP_address - Address of the Server
  % Req - Request ("Register" or "Login")

  % Connect to server
  {_, Address} = inet:parse_address(IP_address),
  {_, Socket} = gen_tcp:connect(Address, ?PORT, [binary, {active, true}, {packet, 0}]),
  %% io:fwrite("Socket: ~p\n", [Socket]),
  % Performs primary requests
  
  if
    Req == "Register" ->
      io:fwrite("Choose a unique username: \n"),
      Username = get_username(),
      io:fwrite("Thank you! Now create your password: \n"),
      Password = get_password(),
      gen_tcp:send(Socket, term_to_binary({Req, {Username, Password}})),
      do_recv(Socket, Username, Password);
    Req == "Login" ->
      Username = get_username(),
      Password = get_password(),
      gen_tcp:send(Socket, term_to_binary({Req, {Username, Password}})),
      do_recv(Socket, Username, Password);
    true ->
      gen_tcp:send(Socket, term_to_binary({"Invalid Request", "None"}))
  end.

% perform secondary requests
user_services(Socket, Username, Password) ->
  % Args:
  % Socket for communication
  % Username: Username (ID)
  % Password: Password (PWD)

  io:fwrite("\nPersistent Services: \n"),

  {ok, Action} = io:read("What do you want to do? : \n Tweet (1) \n Subscribe (2) \n Refresh Feed (3)\n Retweet (4)\n Query (5)\n Logoff (6)\n\n Choice: "),
  io:fwrite("Selected Action: ~p\n", [Action]),
  if
    Action == 1 ->
      io:fwrite("Enter Tweet!\n"),
      {ok, Tweet} = io:read("Tweet: "),
      gen_tcp:send(Socket, term_to_binary({"Tweet", {Username, Tweet}})),
      do_recv(Socket, Username, Password),
      timer:sleep(500),
      user_services(Socket, Username, Password);

    Action == 2 ->
      io:fwrite("To Whom Do You Want To Subscribe?\n"),
      {ok, SUsername} = io:read("Enter Username for Subscription: "),
      gen_tcp:send(Socket, term_to_binary({"Subscribe", {Username, SUsername}})),
      do_recv(Socket, Username, Password),
      timer:sleep(500),
      user_services(Socket, Username, Password);

    Action == 3 ->
      io:fwrite("Refreshing \n"),
      do_recv(Socket, Username, Password);

    Action == 4 ->
      io:fwrite("Retweeting!\n"),
      gen_tcp:send(Socket, term_to_binary({"Retweet", {Username, "None"}})),
      do_recv(Socket, Username, Password),
      timer:sleep(500),
      user_services(Socket, Username, Password);

    Action == 5 ->
      io:fwrite("Query Service\n"),
      io:fwrite("Select your query: \n Hashtag (1) \n My mentions (2) \n Tweets subscribed to (3)\n\n"),
      {ok, Choice} = io:read("Selection: "),
      if
        Choice == 1 ->
          io:fwrite("You chose the hashtag query :)\n"),
          {ok, Query} = io:read("Enter Query (#Hashtag): "),
          gen_tcp:send(Socket, term_to_binary({"HashQuery", {Username, Query}})),
          do_recv(Socket, Username, Password),
          timer:sleep(500),
          user_services(Socket, Username, Password);
        Choice == 2 ->
          io:fwrite("You chose the mentions query :)\n"),
          gen_tcp:send(Socket, term_to_binary({"MentionQuery", {Username, "Mentions"}})),
          do_recv(Socket, Username, Password),
          timer:sleep(500),
          user_services(Socket, Username, Password);
        Choice == 3 ->
          io:fwrite("Searching your subscribed tweets :)\n"),
          {ok, Key} = io:read("Enter Query (Keyword): "),
          gen_tcp:send(Socket, term_to_binary({"SubQuery", {Username, "SubTweets", Key}})),
          do_recv(Socket, Username, Password),
          timer:sleep(500),
          user_services(Socket, Username, Password);
        true ->
          io:fwrite("Invalid Command, Try again\n"),
          user_services(Socket, Username, Password)
      end;

    Action == 6 ->
      io:fwrite("Logging Off!\n"),
      io:fwrite("Bye :)\n"),
      gen_tcp:send(Socket, term_to_binary({"Logoff", {Username, Password}})),
      exit(self(),kill);

    true ->
      io:fwrite("Invalid Command, Please choose a valid option\n"),
      user_services(Socket, Username, Password)
  end.

% Recieves messages from Server
do_recv(Socket, Username, Password) ->
    % Args:
    % Socket for communication
    % Username: Username (ID)
    % Password: Password (PWD)

    %% io:fwrite("Started Message Listener\n"),

    receive
        {tcp, Socket, Binary} ->
          %% io:fwrite("User Received a Message\n"),
          {Request, Data} = binary_to_term(Binary),
          if
            (Request =:= "Ping") ->
              %% io:fwrite("Received Ping from Server\n"),
              io:fwrite("Notification: ~p\n", [Data]);

            (Request =:= "Tweet") ->
              io:fwrite("\nTwitter Feed Update: \n"),
              io:fwrite("*********************************\n"),
              io:format("~p\n", [Data]),
              io:fwrite("*********************************\n");

            (Request =:= "QueryResult") ->
              io:fwrite("\nQuery Result: \n"),
              io:fwrite("*********************************\n"),
              io:fwrite(Data),
              io:fwrite("\n*********************************\n");

            (Request =:= "DistributedTweet") ->
              io:fwrite("\nTwitter Feed Update: \n"),
              io:fwrite("*********************************\n"),
              io:format("~p\n", [Data]),
              io:fwrite("*********************************\n"),
              user_services(Socket, Username, Password);

            (Request =:= "Welcome") ->
              io:fwrite("Logged In!\n"),
              io:fwrite("Welcome to Twitter!\n"),
              user_services(Socket, Username, Password);

            (Request =:= "Logged") ->
              user_services(Socket, Username, Password);

            (Request =:= "Done") ->
              io:fwrite("Done!\n"),
              exit(self(),kill);

            (Request =:= "SubFail") ->
              io:fwrite("Subscription failed! User does not exist \nPlease try again\n"),
              user_services(Socket, Username, Password);

            (Request =:= "Invalid Request") ->
              io:fwrite("Warning: Recieved Invalid Request \n Please Restart Client\n"),
              exit(self(),kill);

            true ->
              gen_tcp:send(Socket, term_to_binary({"Invalid Request", "None"}))
          end,
          do_recv(Socket, Username, Password)
    after 2000 ->
      io:fwrite("\n*********************************\n"),
      io:fwrite("You're All Caught Up!\n"),
      io:fwrite("*********************************\n"),
      user_services(Socket, Username, Password)
    end.
