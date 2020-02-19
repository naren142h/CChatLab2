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
        server = ServerAtom,
        croom = []
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
    % TODO: Implement this function
    % {reply, ok, St} ;
    InsideChannel = lists:member(Channel, St#client_st.croom),
    if
        St#client_st.server == "" ->
            {reply, {error, user_is_not_connected, "user is not connected to the server"}, St};
        InsideChannel ->
            {reply, {error, user_has__already_joined_channel, "user is lready in the channel."}, St};
        true ->
             ServerAtom = list_to_atom(St#client_st.server),
             Pid = self(),
             try genserver:request(ServerAtom, {join, Pid, Channel}) of
                _ ->
                    NewState = St#client_st {croom = [Channel|St#client_st.croom]},
                    {reply, ok, NewState}
            catch 
                _ -> {reply, {error, server_unreachable, "Server unreachable to be joined"}, St}
            end
    end;
    % {reply, {error, not_implemented, "join not implemented"}, St} ;

% Leave channel
handle(St, {leave, Channel}) ->
    % TODO: Implement this function
    % {reply, ok, St} ;
    InsideChannel = lists:member(Channel, St#client_st.croom),
    if 
        not InsideChannel ->
            {reply, {error, user_has_not_joined, "Cannot leave channel you're not part of"}, St};
        true -> 
            Atom = list_to_atom(Channel),
            Pid = self(),
            try genserver:request(Atom, {remove, Pid}) of
                _ ->
                    NewRooms = lists:delete(Channel, St#client_st.croom),
                    NewState = St#client_st {croom = NewRooms},
                    {reply, ok, NewState}
            catch
                _ -> {reply, {error, could_not_reach_server, "Server could not be reached."}, St}
            end
    end;
    % {reply, {error, not_implemented, "leave not implemented"}, St} ;

% Sending message (from GUI, to channel)
handle(St, {message_send, Channel, Msg}) ->
    % TODO: Implement this function
    % {reply, ok, St} ;
    InsideChannel = lists:member(Channel, St#client_st.croom),
    if 
        not InsideChannel ->
            {reply, {error, user_has_not_joined, "Message cannot be sent to the channel you're not part of"}, St};
        true -> 
            Atom = list_to_atom(Channel),
            Pid = self(),
            try spawn(fun() -> genserver:request(Atom, {message, St#client_st.nick, Msg, Pid}) end) of
                _ ->
                    {reply, ok, St}
            catch
                _ -> {reply, {error, could_not_reach_server, "Server could not be reached."}, St}
            end
    end;
    % {reply, {error, not_implemented, "message sending not implemented"}, St} ;

% This case is only relevant for the distinction assignment!
% Change nick (no check, local only)
handle(St, {nick, NewNick}) ->
    if
        St#client_st.server == "" ->
            {reply, ok, St#client_st{nick = NewNick}} ;
        true -> 
                {reply, {error, user_already_connected, "Can't change nick while connected."}, St}
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
