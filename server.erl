%% Author: Pranath Reddy Kumbam
%% UFID: 8512-0977
%% DOSP Project-4 Part-1
%% Mock Twitter-Engine

%% Usage:
%% c(server).
%% c(client).
%% server:main().
%% client:main("10.20.0.124", "Register").
%% client:main("10.20.0.124", "Login").

-module(server).
-export([main/0]).
-define(PORT, 0476).

% Server function accepts connections using gen tcp and handles requests
main() ->
  % Args:
  % None

  io:fwrite("Twitter Engine Started!\n"),
  initiate_tables(),

  % Get the IP Address of the server
  {ok, L} = inet:getif(),
  S_IP = element(1, hd(L)),
  io:fwrite("Server IP: ~p\n", [S_IP]),

  % Receive connection from Client
  {_, LSocket} = gen_tcp:listen(?PORT, [binary, {keepalive, true}, {reuseaddr, true}, {active, true}, {packet, 0}]),
  accept_connections(LSocket).

% Accepts connections and incoming requests
accept_connections(LSocket) ->
    % Args:
    % LSocket: Socket for communication

    {_, Socket} = gen_tcp:accept(LSocket),
    io:fwrite("Connected a User\n"),
    gen_tcp:send(Socket, term_to_binary({"Ping", "Connection Accepted"})),
    spawn(fun() -> accept_connections(LSocket) end),
    do_recv(Socket).

% Recieves messages from Client and handles requests
do_recv(Socket) ->
    % Args:
    % Socket for communication

    receive
        {tcp, Socket, Binary} ->
          {Request, Data} = binary_to_term(Binary),
          if
            (Request =:= "Register") ->
              io:fwrite("Recieved Registration Request from User\n"),
              io:fwrite("Server Received Data: ~p\n", [Data]),
              {Username, Password} = Data,
              io:format("The username received is: ~p\n", [Username]),
              gen_tcp:send(Socket, term_to_binary({"Ping", "Registration Request Accepted"})),
              Reg_Result = registration_service(Username, Password, Socket),
              io:format("Registration Result: ~p\n", [Reg_Result]),
              gen_tcp:send(Socket, term_to_binary({"Ping", Reg_Result})),
              io:format("Sending Complete Signal\n"),
              timer:sleep(500),
              gen_tcp:send(Socket, term_to_binary({"Done", "None"})),
              io:fwrite("Done\n");

            (Request =:= "Login") ->
              io:fwrite("Recieved Login Request from User\n"),
              io:fwrite("Server Received Data: ~p\n", [Data]),
              {Username, Password} = Data,
              io:format("The username received is: ~p\n", [Username]),
              gen_tcp:send(Socket, term_to_binary({"Ping", "Login Request Accepted"})),
              Reg_Result = login_service(Username, Password, Socket),
              io:format("Login Result: ~p\n", [Reg_Result]),
              gen_tcp:send(Socket, term_to_binary({"Ping", Reg_Result})),
              io:format("Sending Complete Signal\n"),
              timer:sleep(500),
              if
                Reg_Result == "Success: User Logged In Successfully" ->
                  gen_tcp:send(Socket, term_to_binary({"Welcome", "None"}));
                true ->
                  gen_tcp:send(Socket, term_to_binary({"Done", "None"}))
              end,
              io:fwrite("Done\n");

            (Request =:= "Tweet") ->
              io:fwrite("Recieved Tweet from User\n"),
              io:fwrite("Server Received Data: ~p\n", [Data]),
              {Username, Tweet} = Data,
              io:fwrite("*********************************\n"),
              io:format("The user ~p tweets: \n", [Username]),
              io:format("~p\n", [Tweet]),
              io:fwrite("*********************************\n"),
              tweet_service(Username, Tweet, Socket, "Tweet"),
              io:format("Sending Complete Signal\n"),
              timer:sleep(500),
              gen_tcp:send(Socket, term_to_binary({"Logged", "None"})),
              io:fwrite("Done\n");

            (Request =:= "Retweet") ->
              io:fwrite("Recieved Retweet Request from User\n"),
              io:fwrite("Server Received Data: ~p\n", [Data]),
              {Username, _} = Data,
              retweet_service(Username),
              io:format("Sending Complete Signal\n"),
              timer:sleep(500),
              gen_tcp:send(Socket, term_to_binary({"Logged", "None"})),
              io:fwrite("Done\n");

            (Request =:= "HashQuery") ->
              io:fwrite("Recieved Query for hashtags from User\n"),
              io:fwrite("Server Received Data: ~p\n", [Data]),
              {Username, Query} = Data,
              query_service(Username, Socket, Query, "None"),
              io:format("Sending Complete Signal\n"),
              timer:sleep(500),
              gen_tcp:send(Socket, term_to_binary({"Logged", "None"})),
              io:fwrite("Done\n");

            (Request =:= "MentionQuery") ->
              io:fwrite("Recieved Query for mentions from User\n"),
              io:fwrite("Server Received Data: ~p\n", [Data]),
              {Username, Query} = Data,
              query_service(Username, Socket, Query, "None"),
              io:format("Sending Complete Signal\n"),
              timer:sleep(500),
              gen_tcp:send(Socket, term_to_binary({"Logged", "None"})),
              io:fwrite("Done\n");

            (Request =:= "SubQuery") ->
              io:fwrite("Recieved Query for Tweets Subscribed to\n"),
              io:fwrite("Server Received Data: ~p\n", [Data]),
              {Username, Query, Key} = Data,
              query_service(Username, Socket, Query, Key),
              io:format("Sending Complete Signal\n"),
              timer:sleep(500),
              gen_tcp:send(Socket, term_to_binary({"Logged", "None"})),
              io:fwrite("Done\n");

            (Request =:= "Subscribe") ->
              io:fwrite("Subscription Request from User\n"),
              io:fwrite("Server Received Data: ~p\n", [Data]),
              {Username, SUsername} = Data,
              io:format("The user ~p wants to subscribe to: \n", [Username]),
              io:format("~p\n", [SUsername]),
              Sub_Result = subscription_service(Username, SUsername, Socket),
              io:format("Sending Complete Signal\n"),
              timer:sleep(500),
              if
                Sub_Result == "Success: User Subscribed" ->
                  gen_tcp:send(Socket, term_to_binary({"Logged", "None"}));
                true ->
                  io:format("Subscription Failed\n"),
                  gen_tcp:send(Socket, term_to_binary({"SubFail", "None"}))
              end,
              io:fwrite("Done\n");

            (Request =:= "Logoff") ->
              io:fwrite("Recieved Logoff Request from User\n"),
              io:fwrite("Server Received Data: ~p\n", [Data]),
              {Username, Password} = Data,
              io:format("The username received is: ~p\n", [Username]),
              gen_tcp:send(Socket, term_to_binary({"Ping", "Logoff Request Accepted"})),
              Reg_Result = logoff_service(Username, Password, Socket),
              io:format("Logoff Result: ~p\n", [Reg_Result]),
              gen_tcp:send(Socket, term_to_binary({"Ping", Reg_Result})),
              io:format("Sending Complete Signal\n"),
              timer:sleep(500),
              gen_tcp:send(Socket, term_to_binary({"Done", "None"})),
              io:fwrite("Done\n");

            (Request =:= "PingMe") ->
              io:fwrite("Recieved Ping Request from User\n"),
              gen_tcp:send(Socket, term_to_binary({"Logged", "None"}));

            (Request =:= "Invalid Request") ->
              io:fwrite("Warning: Recieved Invalid Request \n Please Restart Server\n"),
              exit(self(),kill);
            true ->
              gen_tcp:send(Socket, term_to_binary({"Invalid Request", "None"}))
            end,
            do_recv(Socket)
    end.

% initiate ets tables
initiate_tables() ->
  % Args:
  % None

  ets:new(accounts, [set, named_table, public]),
  ets:new(tweets, [bag, named_table, public]),
  ets:new(subscribers, [bag, named_table, public]),
  ets:new(hashtags, [bag, named_table, public]),
  ets:new(mymentions, [bag, named_table, public]).

% Registers user by adding to ets table
registration_service(Username, Password, Socket)->
  % Args:
  % Username: Username (ID)
  % Password: Password (PWD)
  % Socket for communication

  io:fwrite("Registering User\n"),
  Status_0 = "Fail: User ID Already Exists",
  Status_1 = "Success: User Registered Successfully. Please Login to use Twitter!",
  Out = ets:lookup(accounts, Username),
  if
    length(Out) > 0 ->
        Status_0;
    true ->
        ets:insert(accounts, {Username, Password, Socket, "offline"}),
        Status_1
  end.

% Logs in user by changing status in ets table
login_service(Username, Password, Socket) ->
  % Args:
  % Username: Username (ID)
  % Password: Password (PWD)
  % Socket for communication

  io:fwrite("Logging in User\n"),
  Status_0_1 = "Fail: Invalid Credentials, Check Password",
  Status_0_2 = "Fail: User Account Does Not Exist",
  Status_1 = "Success: User Logged In Successfully",
  Out = ets:lookup(accounts, Username),
  if
    length(Out) > 0 ->
      {_, PWD, _, _} = lists:nth(1, Out),
      if
        PWD == Password ->
         ets:insert(accounts, {Username, Password, Socket, "online"}),
         Status_1;
       true ->
         Status_0_1
      end;
    true ->
      Status_0_2
  end.

% Distribute tweet to subsribers
distribute(Socket, Message, Username) ->
  % Args:
  % Socket for communication
  % Message to be passed
  % Username: Username (ID)

  io:fwrite("Distribute Called\n"),
  ets:insert(tweets, {Username, Message, Socket}),
  hashtag_service(Username, Message),
  gen_tcp:send(Socket, term_to_binary({"DistributedTweet", Message})).

% Store and distribute tweets
tweet_service(Username, Tweet, Socket, Mode) ->
  % Args:
  % Username: Username (ID)
  % Tweet: User's tweet
  % Socket for communication
  % Mode: tweet or retweet

  ets:insert(tweets, {Username, Tweet, Socket}),
  hashtag_service(Username, Tweet),
  mention_service(Username, Tweet),
  io:fwrite("Distributing Tweet\n"),
  if
    Mode == "Tweet" ->
      Message = Username ++ " tweeted: " ++ Tweet;
    true ->
      Message = Username ++ " retweeted: " ++ Tweet
  end,
  io:fwrite("Message to be distributed ~p\n", [Message]),
  %% io:fwrite("Over Socket ~p\n", [Socket]),
  gen_tcp:send(Socket, term_to_binary({"Tweet", Message})),
  Sub_list = ets:lookup(subscribers, Username),
  io:fwrite("Subscribers Info: ~p\n", [Sub_list]),
  if
    length(Sub_list) > 0 ->
      _ = [distribute(Sock, Message, U) || {SU, U, Sock} <- Sub_list, (erlang:port_info(Sock) /= undefined) and (SU == Username)],
      io:fwrite("Tweet Distributed to Subscribers\n");
    true ->
      io:fwrite("No Subscribers Found\n")
  end.

% Process and check tweets for hashtags and store in table
hashtag_service(Username, Tweet) ->
  % Args:
  % Username: Username (ID)
  % Tweet: User's tweet

  H = string:find(Tweet, "#"),
  if
    H == nomatch ->
      io:fwrite("Tweet has no hashtag\n");
    true ->
      Hashtag = hd(string:split(H, " ")),
      ets:insert(hashtags, {Hashtag, Username, Tweet}),
      io:fwrite("Tweet has hashtag: ~p\n", [Hashtag])
  end.

% Process user queries
query_service(Username, QSocket, Query, Key) ->
  % Args:
  % Username: Username (ID)
  % QSocket: Socket for communication
  % Query: Mention or Hashtag

  if
    Query == "Mentions" ->
      Mention = "@" ++ Username,
      Tweet_list = ets:lookup(mymentions, Mention),
      Twt_list = [Message || {_, _, Message} <- Tweet_list],
      Result = string:join(Twt_list, " \n"),
      gen_tcp:send(QSocket, term_to_binary({"QueryResult", Result})),
      io:fwrite("Mentioned tweets: ~p\n", [Result]);
    Query == "SubTweets" ->
      Tweet_list = ets:lookup(tweets, Username),
      Twt_list = [Message || {_, Message, _} <- Tweet_list, string:find(Message, Key) /= nomatch],
      Result = string:join(Twt_list, " \n"),
      gen_tcp:send(QSocket, term_to_binary({"QueryResult", Result})),
      io:fwrite("Subscribed tweets with keyword: ~p\n", [Result]);
    true ->
      Hashtag = hd(string:split(string:find(Query, "#"), " ")),
      io:fwrite("Recieved hashtag query: ~p\n", [Hashtag]),
      Hash_list = ets:lookup(hashtags, Hashtag),
      Hs_list = [Message || {_, _, Message} <- Hash_list],
      Result = string:join(Hs_list, " \n"),
      gen_tcp:send(QSocket, term_to_binary({"QueryResult", Result})),
      io:fwrite("Hashtag tweets: ~p\n", [Result])
  end.

% Process and check tweets for mentions and store in table
% Deliver tweets live to online mentioned users
mention_service(Username, Tweet) ->
  % Args:
  % Username: Username (ID)
  % Tweet: User's tweet

  M = string:find(Tweet, "@"),
  if
    M == nomatch ->
      io:fwrite("Tweet has no mentions\n");
    true ->
      Mention = hd(string:split(M, " ")),
      io:fwrite("Tweet has mention: ~p\n", [Mention]),

      Message = Username ++ " mentioned you in a tweet: " ++ Tweet,
      MUsername = string:slice(Mention, 1),
      Out = ets:lookup(accounts, MUsername),
      ets:insert(mymentions, {Mention, MUsername, Message}),
      io:fwrite("Mentioned user info in table: ~p\n", [Out]),
      if
        length(Out) > 0 ->
            {_, _, MSock, _} = lists:nth(1, Out),
            Status = erlang:port_info(MSock),
            if
              Status /= undefined ->
                gen_tcp:send(MSock, term_to_binary({"DistributedTweet", Message}));
              true ->
                io:fwrite("Mentioned User is Offline\n")
            end;
        true ->
            io:fwrite("User not found\n")
      end
  end.

% retweet latest tweet in the feed of the user
retweet_service(Username) ->
  % Args:
  % Username: Username (ID)

  io:fwrite("Retweet!\n"),
  Tweet_list = ets:lookup(tweets, Username),
  %% io:fwrite("Tweets Info: ~p\n", [Tweet_list]),
  Latest_Tweet = lists:last(Tweet_list),
  io:fwrite("Latest Tweet: ~p\n", [Latest_Tweet]),
  {ID, Twt, Sock} = Latest_Tweet,
  tweet_service(ID, Twt, Sock, "Retweet").

% Update subscribers table
subscription_service(Username, SUsername, Socket) ->
  % Args:
  % Username: Subscriber Username
  % SUsername: Main Username (To Sub to)
  % Socket for communication

  Status_0 = "Fail: Account Does Not Exist",
  Status_1 = "Success: User Subscribed",
  Out = ets:lookup(accounts, SUsername),
  if
    length(Out) > 0 ->
      io:fwrite("Adding Subscriber\n"),
      ets:insert(subscribers, {SUsername, Username, Socket}),
      io:fwrite("Subscriber Added\n"),
      Message = Username ++ " now follows " ++ SUsername,
      gen_tcp:send(Socket, term_to_binary({"Tweet", Message})),
      Status_1;
    true ->
      Status_0
  end.

% Logs off user by changing status in ets table
logoff_service(Username, Password, Socket) ->
  % Args:
  % Username: Username (ID)
  % Password: Password (PWD)
  % Socket for communication

  io:fwrite("Logging off User\n"),
  Status_0 = "Fail: User Account Does Not Exist",
  Status_1 = "Success: User Logged Off Successfully",
  Out = ets:lookup(accounts, Username),
  if
    length(Out) > 0 ->
      ets:insert(accounts, {Username, Password, Socket, "offline"}),
      Status_1;
    true ->
      Status_0
  end.
