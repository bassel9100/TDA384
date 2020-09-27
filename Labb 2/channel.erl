-module(channel).
-export([start/2,stop/1]).

% ********************************* %
%				States				%
% ********************************* %

% This record defines the structure of the state of the channel.
-record(channel_st, {
	name, % name of the channel				 
    clients % list of clients PID connected to the channel
}).

% Return an initial state record. This is called at start.
initial_state(ChannelName, Pid) ->
    #channel_st{
		name = ChannelName,
        clients = [Pid] % client list with the first client
    }.

% ********************************* %
%				Start/Stop			%
% ********************************* %

% Start a new channel process with the given name
start(ChannelAtom, Pid) ->
    % - Spawn a new process which waits for a message, handles it, then loops infinitely
    % - Register this process to ChannelAtom
    % - Return the process ID
    genserver:start(ChannelAtom, initial_state(atom_to_list(ChannelAtom), Pid), fun channel_handler/2).

% Stop the channel process registered to the given name,
% together with any other associated processes
stop(ChannelAtom) ->
    % Return ok
    genserver:stop(ChannelAtom).

% ********************************* %
%				Handlers			%
% ********************************* %

% Handle a join message
channel_handler(State, {join, Pid}) ->
	case is_in_channel(State, Pid) of
		false -> {reply, join, State#channel_st{clients = [Pid|State#channel_st.clients]}};
		% The client is already in the channel
		_	  -> {reply, error, State}
	end;

% Handle a leave message
channel_handler(State, {leave, Pid}) ->
	case is_in_channel(State, Pid) of
		% The client is not in the channel
		false -> {reply, error, State};
		_	  ->
			% Get the new state without the client
			NewState = State#channel_st{clients = lists:delete(Pid, State#channel_st.clients)},
			case length(State#channel_st.clients) of
				% If the client list is empty say to the server that the channel is no more used
				0 -> {reply, empty, NewState};
				% The user just leave
				_ -> {reply, leave, NewState}
			end
	end;

% Handle a sending message requast
channel_handler(State, {message_send, Pid, Nick, Msg}) ->
	case is_in_channel(State, Pid) of
		% The user is not in the channel
		false -> {reply, error, State};
		_	  -> 
			% Send the message to all the other users (list delete is used to remove the sender)
			send_to_all(State#channel_st.name, Nick, Msg, lists:delete(Pid, State#channel_st.clients)),
			{reply, message_send, State}
	end.
			

% ********************************* %
%				Functions			%
% ********************************* %

% Check if the user is in the channel
is_in_channel(State, Pid) -> lists:member(Pid, State#channel_st.clients).

% Spawn a process which send a message to the head of the client list
% so if a client is not reachable we are not stuck for the others
send_to_all(ChannelName, Nick, Msg, [H|T]) ->
	spawn(fun() -> genserver:request(H, {message_receive, ChannelName, Nick, Msg}) end),
	% Recursive call
	send_to_all(ChannelName, Nick, Msg, T);

% Handle the case where the list of client is empty
send_to_all(ChannelName, Nick, Msg, []) -> ok.