-module(client).
-export([handle/2, initial_state/3]).

% This record defines the structure of the state of a client.
% Add whatever other fields you need.
-record(client_st, {
    gui, % atom of the GUI process
    nick, % nick/username of the client
    server % atom of the chat server
}).

% Return an initial state record. This is called from GUI.
% Do not change the signature of this function.
initial_state(Nick, GUIAtom, ServerAtom) ->
    #client_st{
        gui = GUIAtom,
        nick = Nick,
        server = ServerAtom
    }.

% handle/2 handles each kind of request from GUI
% Parameters:
%   - the current state of the client (St)
%   - request data from GUI
% Must return a tuple {reply, Data, NewState}, where:
%   - Data is what is sent to GUI, either the atom `ok` or a tuple {error, Atom, "Error message"}
%   - NewState is the updated state of the client

% Join channel
handle(St, {join, Channel}) ->
    case (catch genserver:request(St#client_st.server, {join, self(), St#client_st.nick, Channel})) of
		% User joined the server
		join  -> {reply, ok, St};
		% Non fatal error user_already_joined
		error -> {reply, {error, user_already_joined, "You already joined this channel."}, St};
		% Fatal error server_not_reached
		{'EXIT', Reason} -> {reply, {error, server_not_reached, Reason}, St}
	end;

% Leave channel
handle(St, {leave, Channel}) ->
    case (catch genserver:request(St#client_st.server, {leave, self(), Channel})) of
		% User leaved the server
		leave -> {reply, ok, St};
		% Non fatal error user_not_joined
		error -> {reply, {error, user_not_joined, "You are not in this channel."}, St};
		% Fatal error server_not_reached
		{'EXIT', Reason} -> {reply, {error, server_not_reached, Reason}, St}
	end;

% Sending message (from GUI, to channel)
handle(St, {message_send, Channel, Msg}) ->
    case (catch genserver:request(list_to_atom(Channel), {message_send, self(), St#client_st.nick, Msg})) of
		% Message have been sended
		message_send -> {reply, ok, St};
		% Non fatal error user_not_joined
		error -> {reply, {error, user_not_joined, "You are not in this channel."}, St};
		% Fatal error server_not_reached
		{'EXIT', Reason} -> {reply, {error, server_not_reached, Reason}, St}
	end;

% This case is only relevant for the distinction assignment!
% Change nick with already taken check
handle(St, {nick, NewNick}) ->
    case (catch genserver:request(St#client_st.server, {nick, St#client_st.nick, NewNick})) of
		% User joined the server
		nick  -> {reply, ok, St#client_st{nick = NewNick}};
		% Non fatal error user_already_joined
		error -> {reply, {error, nick_taken, "Nickname already taken."}, St};
		% Fatal error server_not_reached
		{'EXIT', Reason} -> {reply, {error, server_not_reached, Reason}, St}
	end;

% ---------------------------------------------------------------------------
% The cases below do not need to be changed...
% But you should understand how they work!

% Get current nick
handle(St, whoami) ->
    {reply, St#client_st.nick, St} ;

% Incoming message (from channel, to GUI)
handle(St = #client_st{gui = GUI}, {message_receive, Channel, Nick, Msg}) ->
    gen_server:call(GUI, {message_receive, Channel, Nick++"> "++Msg}),
    {reply, ok, St} ;

% Quit client via GUI
handle(St, quit) ->
    % Any cleanup should happen here, but this is optional
    {reply, ok, St} ;

% Catch-all for any unhandled requests
handle(St, Data) ->
    {reply, {error, not_implemented, "Client does not handle this command"}, St} .
