-module(server).
-export([start/1,stop/1]).

% ********************************* %
%				States				%
% ********************************* %

% This record defines the structure of the state of the server.
-record(server_st, {
    nicks, % client nickname list connected to the server
	channels % used channel list
}).

% Return an initial state record. This is called at start.
initial_state() ->
    #server_st{
        nicks = [],
		channels = []
    }.

% ********************************* %
%				Start/Stop			%
% ********************************* %

% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
    % - Spawn a new process which waits for a message, handles it, then loops infinitely
    % - Register this process to ServerAtom
    % - Return the process ID
    genserver:start(ServerAtom, initial_state(), fun server_handler/2).

% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    % Return ok
    genserver:stop(ServerAtom).

% ********************************* %
%				Handlers			%
% ********************************* %

% Handle a join message
server_handler(State, {join, Pid, Nick, Channel}) ->
	% Call the function which will update the server state and check if the client can join
	join(State, Pid, Nick, Channel);

% Handle a leave message
server_handler(State, {leave, Pid, Channel}) ->
	% Call the function which will update the server state and check if the client can leave
	leave(State, Pid, Channel);

% Handle a nickname change message
server_handler(State, {nick, Nick, NewNick}) ->
	% Call the function which will check if the nickname is already taken or not
	nick(State, Nick, NewNick).
	
	
% ********************************* %
%				Functions			%
% ********************************* %


join(State, Pid, Nick, Channel) ->
	% Check if the channel is currently used
	case lists:member(Channel, State#server_st.channels) of
		false ->
			% If not we create a process for this new channel
			channel:start(list_to_atom(Channel), Pid),
			NewChannelList = [Channel|State#server_st.channels],
			NewNicksList = get_new_nick_list(Nick, State#server_st.nicks),
			% Then we update our state with the potentially new client nick and the updated channel list
			{reply, join, State#server_st{nicks = NewNicksList, channels = NewChannelList}};
		_ 	  -> 
			% If the channel exist we try to add the new client
			Answer = (catch genserver:request(list_to_atom(Channel), {join, Pid})),
			case Answer of
				join  -> 
					% If he can join we update with the potentially new client nickname
					NewNicksList = get_new_nick_list(State#server_st.nicks, Nick),
					{reply, join, State#server_st{nicks = NewNicksList}};
				error -> 
					% If the user has already joined
					{reply, error, State};
				_	  -> Answer
			end				
	end.

% Update the nickname list (add the nickname if not already inside)
get_new_nick_list(Nick, CurrentList) ->
	case lists:member(Nick, CurrentList) of
		true -> CurrentList;
		false -> [Nick, CurrentList]
	end.

% Leave a requested channel
leave(State, Pid, Channel) ->
	% Check if the channel exist at this time
	case lists:member(Channel, State#server_st.channels) of
		false -> 
			% Error if the channel not exist
			{reply, error, State};
		_ 	  -> 
			% Ask the channel to remove the client
			Answer = (catch genserver:request(list_to_atom(Channel), {leave, Pid})),
			case Answer of
				empty  -> 
					% If the channel is empty we stop its process
					genserver:stop(list_to_atom(Channel)),
					% We then remove the channel from the state of the server
					NewList = lists:delete(Channel, State#server_st.channels),
					{reply, leave, State#server_st{channels = NewList}};
				leave  -> 
					% If the client as been removed from the channel process state
					{reply, leave, State};
				error  -> 
					% If the client is not in the channel
					{reply, error, State};
				_	   -> Answer
			end			
	end.

% Change the nickname if possible and update the nickname list
nick(State, Nick, NewNick) ->
	case (Nick =/= NewNick) and (lists:member(NewNick, State#server_st.nicks)) of
		false -> {reply, nick, State#server_st{nicks = [NewNick|lists:delete(Nick, State#server_st.nicks)]}};
		_	  -> {reply, error, State}
	end.


	

	
			

