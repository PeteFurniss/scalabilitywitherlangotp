-module(phone).
-behaviour(gen_server).

%%%-----------------------------------------------------------------------------
%%% Description module phone
%%%-----------------------------------------------------------------------------
%%% Models the behaviour of a simple phone handset using gen_server.
%%% Implements the API for phone.erl defined in chapter 6 of Designing for
%%% Scalability with Erlang/OTP by Francesco Cesarini & Steve Vinoski (O'Reilly)
%%% ISBN 978-1-449-32073-7.
%%%-----------------------------------------------------------------------------

-export([start_link/1]).
-export([action/2, reply/2, stop/1, status/1]).
-export([init/1, handle_call/3, handle_cast/2]).
-export([handle_info/2, terminate/2, code_change/3]).


%%------------------------------------------------------------------------------
%% Client Functions
%%------------------------------------------------------------------------------


%%------------------------------------------------------------------------------
%% Function: start_link/1
%% Purpose: Starts a gen_server process for a phone for the given phone number.
%%          If a process exists already for that phone number, it is stopped
%%          before the new process is started.
%% Args:    The phone number to be associate with the phone process being
%%          started
%%------------------------------------------------------------------------------

start_link(PhoneNumber) ->
    PhonePid = whereis(ms2phone(PhoneNumber)),
    if
        is_pid(PhonePid) -> stop(PhonePid);
        true -> ok
    end,
    gen_server:start_link({local, ms2phone(PhoneNumber)}, ?MODULE,
                          [PhoneNumber], []).

%%------------------------------------------------------------------------------
%% Function: stop/1
%% Purpose: Stops the gen_server phone process with the given pid.
%% Args:    The pid of the phone process to be stopped.
%%------------------------------------------------------------------------------
stop(PhonePid) ->
    gen_server:call(PhonePid, stop).

%%------------------------------------------------------------------------------
%% Function: action/2
%% Purpose: Instructs the phone process to carry out the given action.
%% Args:    The pid of the phone process.
%%          The action to be performed.
%%------------------------------------------------------------------------------

%% Action: Call the given phone number
action(PhonePid, {call, PhoneNumber}) ->
    gen_server:cast(PhonePid, {action, {call, PhoneNumber}}),
    ok;

%% Action: Accept the incoming call
action(PhonePid, accept) ->
    gen_server:cast(PhonePid, {action, accept}),
    ok;

%% Action: Reject the incoming call
action(PhonePid, reject) ->
    gen_server:cast(PhonePid, {action, reject}),
    ok;

%% Action: Hangup on the current call
action(PhonePid, hangup) ->
    gen_server:cast(PhonePid, {action, hangup}),
    ok.

%%------------------------------------------------------------------------------
%% Function: reply/2
%% Purpose: Used by the FSM process associated with this phone to pass the phone
%%          a message received by the phone network.
%% Args:    The pid of the phone process.
%%          The message being forwarded to the phone.
%%------------------------------------------------------------------------------

%% Reply: Notify the phone of an incoming call from the given phone number
reply(PhonePid, {inbound, PhoneNumber}) ->
    gen_server:cast(PhonePid, {reply, {inbound, PhoneNumber}}),
    ok;

%% Reply: Notify the phone that the outbound call has been accepted
reply(PhonePid, accept) ->
    gen_server:cast(PhonePid, {reply, accept}),
    ok;

%% Reply: Notify the phone that the phone number used in the outbound call 
%%        was invalid
reply(PhonePid, invalid) ->
    gen_server:cast(PhonePid, {reply, invalid}),
    ok;

%% Reply: Notify the phone that the outbound call has been rejected
reply(PhonePid, reject) ->
    gen_server:cast(PhonePid, {reply, reject}),
    ok;

%% Reply: Notify the phone that the phone called in an outbound call is busy
reply(PhonePid, busy) ->
    gen_server:cast(PhonePid, {reply, busy}),
    ok;

%% Reply: Notify the phone that the phone called in an outbound call has hungup
reply(PhonePid, hangup) ->
    gen_server:cast(PhonePid, {reply, hangup}),
    ok.

%%------------------------------------------------------------------------------
%% Function: status/1
%% Purpose: Used to return the status of the phone process
%% Args:    The pid of the phone process.
%%------------------------------------------------------------------------------
status(PhonePid) ->
    gen_server:call(PhonePid, status).



%%------------------------------------------------------------------------------
%% Callback Functions for gen_server
%%------------------------------------------------------------------------------


%%------------------------------------------------------------------------------
%% Function: init/1
%% Purpose: Called by gen_server when the phone process is started
%%          Starts a phone FSM process linked-to this phone process.
%%          Connects to the phone FSM process.
%% Args:    The number of the phone being started.
%%------------------------------------------------------------------------------
init([PhoneNumber]) ->
    log_message(self(), PhoneNumber, "starting phone FSM"),
    {ok, PhoneFSMPid} = phone_fsm:start_link(PhoneNumber),
    log_message(self(), PhoneNumber, "phone FSM started"),
    ok = phone_fsm:connect(PhoneFSMPid),
    log_message(self(), PhoneNumber, "phone connected to phone FSM"),
    {ok, {PhoneNumber, PhoneFSMPid, init}}.

%%------------------------------------------------------------------------------
%% Function: handle_call/3
%% Purpose: Handles synchronous events sent to the phone process.
%% Args:    The synchronous event to be handled.
%%          The identity of the calling process
%%          The current state of the phone process
%%------------------------------------------------------------------------------

%% Stop: Instructs gen_server to stop the phone process.
handle_call(stop, _From, {MyPhoneNumber, FSMPid, _LastCall}) ->
    log_message(self(), MyPhoneNumber, "stop"),
    {stop, normal, ok, {MyPhoneNumber, FSMPid, {stop}}};

%% Status: Instructs gen_server to send the current status back to the caller.
handle_call(status, _From, State) ->
    {reply, {{loop_data, State}}, State}.

%%------------------------------------------------------------------------------
%% Function: handle_cast/2
%% Purpose: Handles asynchronous events sent to the phone process.
%% Args:    The asynchronous event to be handled.
%%          The current state of the phone process
%%------------------------------------------------------------------------------

%% Action: Outbound call
%%         Forwards the call to the linked FSM
handle_cast({action, {call, ToPhoneNumber}},
            {MyPhoneNumber, FSMPid, _LastCall}) ->
    phone_fsm:action(FSMPid, {outbound, ToPhoneNumber}),
    log_message(self(), MyPhoneNumber,
                "action: outbound call to " ++ ToPhoneNumber),
    {noreply, {MyPhoneNumber, FSMPid, {action, {call, ToPhoneNumber}}}};

%% Action: Accept
%%         Instructs the linked FSM that the incoming call has been accepted
handle_cast({action, accept}, {MyPhoneNumber, FSMPid, _LastCall}) ->
    phone_fsm:action(FSMPid, accept),
    log_message(self(), MyPhoneNumber, "action: accept"),
    {noreply, {MyPhoneNumber, FSMPid, {action, accept}}};

%% Action: Reject
%%         Instructs the linked FSM that the incoming call has been rejected
handle_cast({action, reject}, {MyPhoneNumber, FSMPid, _LastCall}) ->
    phone_fsm:action(FSMPid, reject),
    log_message(self(), MyPhoneNumber, "action: reject"),
    {noreply, {MyPhoneNumber, FSMPid, {action, reject}}};

%% Action: Hangup
%%         Instructs the linked FSM that the local phone has hung-up
handle_cast({action, hangup}, {MyPhoneNumber, FSMPid, _LastCall}) ->
    phone_fsm:action(FSMPid, hangup),
    log_message(self(), MyPhoneNumber, "action: hangup"),
    {noreply, {MyPhoneNumber, FSMPid, {action, hangup}}};

%% Reply: Inbound
%%        Logs a message that an inbound call has been received
handle_cast({reply, {inbound, PhoneNumber}},
            {MyPhoneNumber, FSMPid, _LastCall}) ->
    log_message(self(), MyPhoneNumber,
                "received inbound call from: " ++ PhoneNumber),
    {noreply, {MyPhoneNumber, FSMPid, {reply, {inbound, PhoneNumber}}}};

%% Reply: Logs the event received
handle_cast({reply, Reply}, {MyPhoneNumber, FSMPid, _LastCall}) ->
    log_message(self(), MyPhoneNumber,
                "received reply: " ++ atom_to_list(Reply)),
    {noreply, {MyPhoneNumber, FSMPid, {reply, Reply}}}.

%%------------------------------------------------------------------------------
%% Default implementations
%%------------------------------------------------------------------------------

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%------------------------------------------------------------------------------
%% Utility Functions
%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% Function: ms2phone/1
%% Purpose: Converts a phone number to the name of the phone process.
%% Args:    The phone number being converted
%%------------------------------------------------------------------------------

ms2phone(PhoneNumber) ->
    list_to_atom("phone"++PhoneNumber).

%%------------------------------------------------------------------------------
%% Function: log_message/3
%% Purpose: Logs a message in the form "<pid>: <phone number>: <event>"
%% Args:    The phone process pid
%%          The phone number
%%          The event being logged
%%------------------------------------------------------------------------------

log_message(PhonePid, PhoneNumber, Event) ->
    io:format("~p: ~p: ~s~n", [PhonePid, PhoneNumber, Event]).
