-module(phone_fsm).
-behaviour(gen_fsm).

%%%-----------------------------------------------------------------------------
%%% Description module phone_fsm
%%%-----------------------------------------------------------------------------
%%% Implements a phone controller as an FSM that holds the state of a single
%%% phone as per the description of phone_fsm.erl in chapter 6 of Designing for
%%% Scalability with Erlang/OTP by Francesco Cesarini & Steve Vinoski (O'Reilly)
%%% ISBN 978-1-449-32073-7.

%%%-----------------------------------------------------------------------------
%%% States Implemented
%%%
%%% idle       - the local phone is not in use. It may be connected to the
%%%              controller or disconnected.
%%% calling    - the local phone has initiated a call and is awaiting a
%%%              response.
%%% connecting - another party has initiated a call to the local phone and is
%%%              awaiting a response.
%%% receiving  - a call between the local phone and another party is in
%%%              progress.

%%%-----------------------------------------------------------------------------
%%% State Data
%%%
%%% {LocalMs, LocalPhonePid, RemoteMs, RemoteFSMPid, LastCall, Freq}

%%%-----------------------------------------------------------------------------
%%% Events
%%%
%%% Events from the phone process associated with the local controller
%%% connect    - attempt to connect to the controller - synchronous
%%% disconnect - attempt to disconnect from the controller - synchronous
%%% outbound   - initiate a to call another party - asynchronous
%%% accept     - accept an incoming call from another party - asynchronous
%%% reject     - reject a call from another party - asynchronous
%%% hangup     - terminate a call - asynchronous
%%%
%%% Events from the remote controller associate with another party's phone
%%% (all asynchronous)
%%% inbound    - another party is attempting to call the local phone
%%% accept     - the other party accepts the call initiated by the local phone
%%% reject     - the other party rejects the call initiated by the local phone
%%% hangup     - the other party terminates a call
%%% busy       - the other party is cannot accept the call initiated by the local phone
%%% 
%%%-----------------------------------------------------------------------------

-export([start_link/1, stop/1]).
-export([init/1, terminate/3, handle_sync_event/4, handle_event/3, handle_info/3, code_change/4]).
-export([connect/1, disconnect/1, action/2]).
-export([inbound/1, busy/1, reject/1, accept/1, hangup/1]).
-export([status/1]).
-export([idle/2, calling/2, connecting/2, receiving/2]).
-export([get_state_data_values/2]).
-export([test_state_data/0]).

%%------------------------------------------------------------------------------
%% Client Functions
%%------------------------------------------------------------------------------


%%------------------------------------------------------------------------------
% start_link
%%------------------------------------------------------------------------------
%% Function: start_link/1
%% Purpose:  Starts a gen_fsm process for a phone controller for the given phone
%%           number.
%%           Registers the process with the name "phone_fsm<Phone Number>".
%% Args:     The phone number to be associate with the phone_fsm process being
%%           started
%% Returns:  {ok, <Pid of the phone_fsm process>}
%%------------------------------------------------------------------------------

start_link(LocalMs) ->
    {ok, LocalFSMPid} = gen_fsm:start_link(?MODULE, LocalMs, []),
    register(list_to_atom("phone_fsm"++LocalMs), LocalFSMPid),
    {ok, LocalFSMPid}.

%%------------------------------------------------------------------------------
%% Function: stop/1
%% Purpose:  Stops the gen_fsm phone controller process with the given pid.
%% Args:     The pid of the phone_fsm process to be stopped.
%%------------------------------------------------------------------------------


stop(LocalFsmPid) ->
    gen_fsm:sync_send_all_state_event(LocalFsmPid, stop).

%%------------------------------------------------------------------------------
%% Function: status/1
%% Purpose:  Used to return the status of the phone controller fsm process
%% Args:     The pid of the phone_fsm process.
%%------------------------------------------------------------------------------

status(LocalFSMPid) ->
    gen_fsm:sync_send_all_state_event(LocalFSMPid, status).

%%------------------------------------------------------------------------------
%% Function: connect/1
%% Purpose:  Used by the phone process to connect to the phone controller.
%% Args:     The pid of the phone_fsm process the phone needs to connect to.
%%------------------------------------------------------------------------------

connect(LocalFsmPid) ->
    gen_fsm:sync_send_all_state_event(LocalFsmPid, connect).

%%------------------------------------------------------------------------------
%% Function: disconnect/1
%% Purpose:  Used by the phone process to disconnect from the phone controller.
%% Args:     The pid of the phone_fsm process the phone needs to disconnect from.
%%------------------------------------------------------------------------------

disconnect(LocalFsmPid) ->
    gen_fsm:sync_send_all_state_event(LocalFsmPid, disconnect).

%%------------------------------------------------------------------------------
%% Function: action/2
%% Purpose:  Handles an action from the connected phone process.
%% Args:     The pid of the phone_fsm process the phone is connected to.
%%           The action to be performed. Valid values are:
%%               {outbound, <The phone number to be called>}
%%               accept
%%               reject
%%               hangup
%%------------------------------------------------------------------------------

action(LocalFSMPid, {outbound, RemoteMs}) ->
    gen_fsm:send_event(LocalFSMPid, {action, {{outbound, RemoteMs}, self()}});
action(LocalFSMPid, Action) ->  %Action == hangup, reject, accept
    gen_fsm:send_event(LocalFSMPid, {action, {Action, self()}}).

%%------------------------------------------------------------------------------
%% Function: inbound/1
%% Purpose:  Handles an inbound call from another phone controller process.
%% Args:     The pid of the phone_fsm process from which the inbound call
%%           originated.
%%------------------------------------------------------------------------------
inbound(RemoteFSMPid) ->
    gen_fsm:send_event(RemoteFSMPid, {inbound, self()}).

%%------------------------------------------------------------------------------
%% Function: busy/1
%% Purpose:  Handles a busy response from another phone controller process.
%% Args:     The pid of the phone_fsm process sending the busy response.
%%------------------------------------------------------------------------------

busy(RemoteFSMPid) ->
    gen_fsm:send_event(RemoteFSMPid, {busy, self()}).

%%------------------------------------------------------------------------------
%% Function: reject/1
%% Purpose:  Handles a reject call response from another phone controller
%%           process.
%% Args:     The pid of the phone_fsm process sending the reject response.
%%------------------------------------------------------------------------------

reject(RemoteFSMPid) ->
    gen_fsm:send_event(RemoteFSMPid, {reject, self()}).

%%------------------------------------------------------------------------------
%% Function: accept/1
%% Purpose:  Handles an accept call response from another phone controller
%%           process.
%% Args:     The pid of the phone_fsm process sending the accept response.
%%------------------------------------------------------------------------------

accept(RemoteFSMPid) ->
    gen_fsm:send_event(RemoteFSMPid, {accept, self()}).

%%------------------------------------------------------------------------------
%% Function: hangup/1
%% Purpose:  Handles an hangup phone response from another phone controller
%%           process.
%% Args:     The pid of the phone_fsm process sending the hangup response.
%%------------------------------------------------------------------------------

hangup(RemoteFSMPid) ->
    gen_fsm:send_event(RemoteFSMPid, {hangup, self()}).


%%------------------------------------------------------------------------------
%% Callback Functions for gen_fsm
%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% Function: init/1
%% Purpose:  Called by gen_fsm when the phone_fsm process is started
%%           Records the fsm process id and the associated phone number in the
%%           HLR.
%% Args:     The number of the phone being started.
%% Returns:  Tuple informing the fsm to enter the idle state with state data
%%           updated with the phone number of the connected phone.
%%------------------------------------------------------------------------------

init(LocalMs) ->
    process_flag(trap_exit, true),
    hlr:attach(LocalMs),
    StateData = new_state_data(),
    NewStateData = set_state_data_values(StateData, [{localms,LocalMs},{lastcall,{init,{LocalMs}}}]),
    {ok, idle, NewStateData}.

%%------------------------------------------------------------------------------
%% Function: terminate/3
%% Purpose:  Called by gen_fsm when the phone_fsm process is terminated
%%           The actions taken depend on the state the fsm is in at the time:
%%           In all states, the fsm pid/phone number are removed from the HLR.
%%           In calling, a hangup is sent to the remote phone controller.
%%           In connecting, hangup and reject are sent to the remote phone
%%           controller.
%%           In receiving, a hangup is sent to the remote phone controller.
%% Args:     The reason for the termination.
%%           The current state.
%%           The current state data.
%%------------------------------------------------------------------------------

terminate(_Reason, idle, _StateData) ->
    hlr:detach();
terminate(_Reason, calling, StateData) ->
    [RemoteFSMPid] = get_state_data_values(StateData, [remotefsmpid]),
    phone_fsm:hangup(RemoteFSMPid),
    hlr:detach();
terminate(_Reason, connecting, StateData) ->
    [RemoteFSMPid] = get_state_data_values(StateData, [remotefsmpid]),
    phone_fsm:hangup(RemoteFSMPid),
    phone_fsm:reject(RemoteFSMPid),
    hlr:detach();
terminate(_Reason, receiving, StateData) ->
    [RemoteFSMPid] = get_state_data_values(StateData, [remotefsmpid]),
    phone_fsm:hangup(RemoteFSMPid),
    hlr:detach().

%%------------------------------------------------------------------------------
%% Function: handle_sync_event/4
%% Purpose:  Called by gen_fsm to handle synchronous events.
%% Args:     The synchronous event to be handled.
%%           Information about the originator of the event, including its pid.
%%           The current state.
%%           The current state data.
%%------------------------------------------------------------------------------

%% Event: status request
%%        Replies with the current state name and state data.
handle_sync_event(status, _From, StateName, StateData) ->
    {reply, {{state, StateName}, {loop_data, StateData}}, StateName, StateData};

%% Event: connect request
%%        Records the pid of the connecting phone in the state data
%%        Replies with ok
handle_sync_event(connect, {LocalPhonePid, _Tag}, State, StateData) ->
    NewStateData = set_state_data_values(StateData, [{localphonepid,LocalPhonePid},
                                                     {lastcall,{connect,{LocalPhonePid}}}]),
    {reply, ok, State, NewStateData};

%% Event: disconnect request
%%        Checks whether the request has come from the connected phone pid or
%%        not.
%%        If from the connected phone, records the phone pid in the state
%%        data and replies ok.
%%        If from another phone, logs an error and replies {error, wrongpid}.
handle_sync_event(disconnect, {LocalPhonePid, _Tag}, State, StateData) ->
    case get_state_data_values(StateData, [localphonepid]) of
        [LocalPhonePid] ->
            NewStateData = set_state_data_values(StateData, [{localphonepid,LocalPhonePid},
                                                             {lastcall,{disconnect,{LocalPhonePid}}}]),
            {reply, ok, State, NewStateData};
        [OtherPhonePid] ->
            [LocalPhonePid, LocalMs] = get_state_data_values(StateData, [localphonepid, localms]),
            log_message("ERROR", "Attempt to disconnect wrong phone pid [~p] from phone_fsm for number [~p], pid [~p]",
                [OtherPhonePid, LocalMs, LocalPhonePid]),
            {reply, {error, wrongpid}, State, StateData}
    end.
    

%%------------------------------------------------------------------------------
%% Function: handle_event/3
%% Purpose:  Default implementation
%% Args:     The event to be handled.
%%           The current state.
%%           The current state data.
%%------------------------------------------------------------------------------

handle_event(_Event, StateName, StateData) ->
    {nextstate, StateName, StateData}.

%%------------------------------------------------------------------------------
%% Function: handle_info/3
%% Purpose:  Default implementation
%% Args:     The info event to be handled.
%%           The current state.
%%           The current state data.
%%------------------------------------------------------------------------------

handle_info(_Info, StateName, StateData) ->
    {noreply, StateName, StateData}.

%%------------------------------------------------------------------------------
%% Function: code_change/4
%% Purpose:  Default implementation
%% Args:     The version of the old module being upgraded.
%%           The current state.
%%           The current state data.
%%           Extra arguments.
%%------------------------------------------------------------------------------

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.


%%------------------------------------------------------------------------------
%% State-specific events handling callback functions
%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% Function: idle/2
%% Purpose:  Idle state handler
%% Args:     The event to be handled
%%           The current state data.
%%
%% Actions taken for each event:
%%
%% Event              Actions                                           New State Data          Next State
%% ------------------ ------------------------------------              ---------------------   ----------- 
%% outbound (local)   Local phone Pid does not match state data
%%                    log error                                         No change               idle
%%                    Local phone Pid matches state data, continue
%%                    Lookup called phone number in HLR                   
%%                    If not found: log error                           No change               idle
%%                    If found: call phonefsm:inbound for RemoteFSMPid  Add RemoteMs, RemotePid calling
%% inbound (remote)   Lookup RemoteFSM Pid in HLR                   
%%                    Pid not found: log error                          No change               idle
%%                    Pid found: send inbound, RemoteMs to Phone        Add RemoteMs, RemotePid connecting
%% other              log error                                         No change               idle
%%------------------------------------------------------------------------------

idle({action, {{outbound, RemoteMs}, LocalPhonePid}}, StateData) ->
    case get_state_data_values(StateData, [localphonepid]) of
        [LocalPhonePid] ->
            case hlr:lookup_id(RemoteMs) of
                {error, invalid} ->
                    [LocalPhonePid] = get_state_data_values(StateData, [localphonepid]),
                    log_message("ERROR", "Attempt to connect to an unknown phone number [~p]~n", [RemoteMs]),
                    phone:reply(LocalPhonePid, invalid),
                    NewStateData = set_state_data_values(StateData, [{lastcall, {idle, {outbound, RemoteMs}}}]),
                    {next_state, idle, NewStateData};
                {ok, RemoteFSMPid} when is_pid(RemoteFSMPid) ->
                    phone_fsm:inbound(RemoteFSMPid),
                    NewStateData = set_state_data_values(StateData, [{lastcall,{idle,{outbound,RemoteMs}}},
                                                                     {remotems,RemoteMs},
                                                                     {remotefsmpid,RemoteFSMPid}]),
                    {next_state, calling, NewStateData}
            end;
        _ ->
            log_message("ERROR", "Outbound call attempt made by unknown phone pid [~p]~n", [LocalPhonePid]),
            NewStateData = set_state_data_values(StateData,
                                                 [{lastcall, {idle, {action, {{outbound, RemoteMs}, LocalPhonePid}}}}]),
            {next_state, idle, NewStateData}
    end;

idle({inbound, RemoteFSMPid}, StateData) ->
    case hlr:lookup_ms(RemoteFSMPid) of
        {error, invalid} ->
            log_message("ERROR", "Incoming call attempt from unknown FSM Pid [~p]~n", [RemoteFSMPid]),
            NewStateData = set_state_data_values(StateData, [{lastcall, {idle, {inbound, RemoteFSMPid}}}]),
            {next_state, idle, NewStateData};
        {ok, RemoteMs} ->
            log_message("INFO", "Incoming call attempt from FSM Pid [~p], Phone Number [~p]~n", [RemoteFSMPid, RemoteMs]),
            [LocalPhonePid] = get_state_data_values(StateData, [localphonepid]),
            phone:reply(LocalPhonePid, {inbound, RemoteMs}),
            NewStateData = set_state_data_values(StateData, [{lastcall,{idle,{inbound,RemoteFSMPid}}},
                                                             {remotems,RemoteMs},
                                                             {remotefsmpid,RemoteFSMPid}]),
            {next_state, connecting, NewStateData}
    end;

idle(Ignore, StateData) ->
    log_message("ERROR", "FSM Pid [~p] received invalid event [~p] in idle. Ignored. State data [~p]~n",
              [self(), Ignore, StateData]),
    NewStateData = set_state_data_values(StateData, [{lastcall,{idle,Ignore}}]),
    {next_state, idle, NewStateData}.


%%------------------------------------------------------------------------------
%% Function: calling/2
%% Purpose:  Calling state handler - the local phone is trying to make a call
%% Args:     The event to be handled
%%           The current state data.
%%
%% Actions taken for each event:
%%
%% Event              Actions                                    New State Data                Next State
%% ------------------ ------------------------------------       ---------------------         ----------- 
%% accept (remote)    Remote FSM Pid matches state data:         No change                     receiving
%%                    Send accept to Phone
%%                    Remote FSM Pid does not match state data:  No change                     calling
%%                    log error
%% reject (remote)    Remote FSM Pid matches state data:         Remove RemoteMs,RemoteFSMPid  idle
%%                    Send reject to Phone
%%                    Remote FSM Pid does not match state data:  No change                     calling
%%                    log error
%% busy (remote)      Remote FSM Pid matches state data:         Remove RemoteMs,RemoteFSMPid  idle
%%                    Send busy to Phone
%%                    Remote FSM Pid does not match state data:  No change                     calling
%%                    log error
%% hangup (local)     Phone Pid matches state data:              Remove RemoteMs,RemoteFSMPid  idle
%%                    Send hangup to RemoteFSM
%%                    Phone Pid does not match state data:       No change                     calling
%%                    log error
%% inbound (remote)   Send busy to Calling FSM                   No change                     calling
%% other              log error                                  No change                     calling
%%------------------------------------------------------------------------------

calling({accept, RemoteFSMPid}, StateData) ->
    case get_state_data_values(StateData, [remotefsmpid]) of
        [RemoteFSMPid] ->
            [LocalPhonePid] = get_state_data_values(StateData, [localphonepid]),
            log_message("INFO", "Sending accept to local phone Pid [~p]~n", [LocalPhonePid]),
            phone:reply(LocalPhonePid, accept),
            NewStateData = set_state_data_values(StateData, [{lastcall, {calling, {accept, RemoteFSMPid}}}]),
            {next_state, receiving, NewStateData};
        _ ->
            log_message("ERROR", "Attempt to accept received from invalid phone FSM Pid [~p]~n", [RemoteFSMPid]),
            NewStateData = set_state_data_values(StateData, [{lastcall, {calling, {accept, RemoteFSMPid}}}]),
            {next_state, calling, NewStateData}
    end;

calling({inbound, RemoteFSMPid}, StateData) ->
    phone_fsm:busy(RemoteFSMPid),
    NewStateData = set_state_data_values(StateData, [{lastcall, {calling, {inbound, RemoteFSMPid}}}]),
    {next_state, calling, NewStateData};

calling({busy, RemoteFSMPid}, StateData) ->
    case get_state_data_values(StateData, [remotefsmpid]) of
        [RemoteFSMPid] ->
            [LocalPhonePid] = get_state_data_values(StateData, [localphonepid]),
            log_message("INFO", "Sending busy to local phone Pid [~p]~n", [LocalPhonePid]),
            phone:reply(LocalPhonePid, busy),
            NewStateData = set_state_data_values(StateData, [{lastcall, {calling, {busy, RemoteFSMPid}}},
                                                             {remotefsmpid, void},
                                                             {remotems, void}]),
            {next_state, idle, NewStateData};
        _ ->
            log_message("ERROR", "Busy received from invalid phone FSM Pid [~p]~n", [RemoteFSMPid]),
            NewStateData = set_state_data_values(StateData, [{lastcall, {calling, {busy, RemoteFSMPid}}}]),
            {next_state, calling, NewStateData}
    end;

calling({reject, RemoteFSMPid}, StateData) ->
    case get_state_data_values(StateData, [remotefsmpid]) of
        [RemoteFSMPid] ->
            [LocalPhonePid] = get_state_data_values(StateData, [localphonepid]),
            log_message("INFO", "Sending reject to local phone Pid [~p]~n", [LocalPhonePid]),
            phone:reply(LocalPhonePid, reject),
            NewStateData = set_state_data_values(StateData, [{lastcall, {calling, {reject, RemoteFSMPid}}},
                                                             {remotefsmpid, void},
                                                             {remotems, void}]),
            {next_state, idle, NewStateData};
        _ ->
            log_message("ERROR", "Reject received from invalid phone FSM Pid [~p]~n", [RemoteFSMPid]),
            NewStateData = set_state_data_values(StateData, [{lastcall, {calling, {reject, RemoteFSMPid}}}]),
            {next_state, calling, NewStateData}
    end;

calling({action, {hangup, LocalPhonePid}}, StateData) ->
    case get_state_data_values(StateData, [localphonepid]) of
        [LocalPhonePid] ->
            [RemoteFSMPid] = get_state_data_values(StateData, [remotefsmpid]),
            log_message("INFO", "Sending hangup to remote FSM Pid [~p]~n", [RemoteFSMPid]),
            phone_fsm:hangup(RemoteFSMPid),
            NewStateData = set_state_data_values(StateData, [{lastcall, {calling, {hangup, LocalPhonePid}}},
                                                             {remotefsmpid, void},
                                                             {remotems, void}]),
            {next_state, idle, NewStateData};
        _ ->
            log_message("ERROR", "Hangup received from invalid phone Pid [~p]~n", [LocalPhonePid]),
            NewStateData = set_state_data_values(StateData, [{lastcall, {calling, {hangup, LocalPhonePid}}}]),
            {next_state, calling, NewStateData}
    end;

calling(Ignore, StateData) ->
    log_message("ERROR", "FSM Pid [~p] received invalid event [~p] in calling. Ignored. State data [~p]~n",
              [self(), Ignore, StateData]),
    NewStateData = set_state_data_values(StateData, [{lastcall,{calling,Ignore}}]),
    log_message("DEBUG", "NewStateData [~p]~n", [NewStateData]),
    {next_state, calling, NewStateData}.


%%------------------------------------------------------------------------------
%% Function: connecting/2
%% Purpose:  Connecting state handler - someone else is trying to call the local phone
%% Args:     The event to be handled
%%           The current state data.
%%
%% Actions taken for each event:
%%
%% Event              Actions                                    New State Data                Next State
%% ------------------ ------------------------------------       ---------------------         ----------- 
%% accept (local)     Local phone Pid matches state data:        No change                     receiving
%%                    Send accept to Remote FSM
%%                    Local phone Pid does not match state data: No change                     connecting
%%                    log error
%% reject (local)     Local phone Pid matches state data:        Remove RemoteMs,RemoteFSMPid  idle
%%                    Send reject to Remote FSM
%%                    Local phone Pid does not match state data: No change                     connecting
%%                    log error
%% hangup (remote)    Remote FSM Pid matches state data:         Remove RemoteMs,RemoteFSMPid  idle
%%                    Send hangup to Local Phone
%%                    Remote FSM Pid does not match state data:  No change                     connecting
%%                    log error
%% inbound (remote)   Send busy to Calling FSM                   No change                     connecting
%% other              log error                                  No change                     connecting
%%------------------------------------------------------------------------------

connecting({action, {accept, LocalPhonePid}}, StateData) ->
    case get_state_data_values(StateData, [localphonepid]) of
        [LocalPhonePid] ->
            [RemoteFSMPid] = get_state_data_values(StateData, [remotefsmpid]),
            log_message("INFO", "Sending accept to FSM Pid [~p]~n", [RemoteFSMPid]),
            phone_fsm:accept(RemoteFSMPid),
            NewStateData = set_state_data_values(StateData, [{lastcall, {connecting, {accept, LocalPhonePid}}}]),
            {next_state, receiving, NewStateData};
        _ ->
            log_message("ERROR", "Attempt to accept received from invalid phone Pid [~p]~n", [LocalPhonePid]),
            NewStateData = set_state_data_values(StateData, [{lastcall, {connecting, {accept, LocalPhonePid}}}]),
            {next_state, connecting, NewStateData}
    end;

connecting({action, {reject, LocalPhonePid}}, StateData) ->
    case get_state_data_values(StateData, [localphonepid]) of
        [LocalPhonePid] ->
            [RemoteFSMPid] = get_state_data_values(StateData, [remotefsmpid]),
            log_message("INFO", "Sending reject to FSM Pid [~p]~n", [RemoteFSMPid]),
            phone_fsm:reject(RemoteFSMPid),
            NewStateData = set_state_data_values(StateData, [{lastcall, {connecting, {reject, LocalPhonePid}}},
                                                             {remotefsmpid, void},
                                                             {remotems, void}]),
            {next_state, idle, NewStateData};
        _ ->
            log_message("ERROR", "Attempt to reject received from invalid phone Pid [~p]~n", [LocalPhonePid]),
            NewStateData = set_state_data_values(StateData, [{lastcall, {connecting, {reject, LocalPhonePid}}}]),
            {next_state, connecting, NewStateData}
    end;

connecting({inbound, RemoteFSMPid}, StateData) ->
    phone_fsm:busy(RemoteFSMPid),
    NewStateData = set_state_data_values(StateData, [{lastcall, {connecting, {inbound, RemoteFSMPid}}}]),
    {next_state, connecting, NewStateData};

connecting({hangup, RemoteFSMPid}, StateData) ->
    case get_state_data_values(StateData, [remotefsmpid]) of
        [RemoteFSMPid] ->
            [LocalPhonePid] = get_state_data_values(StateData, [localphonepid]),
            log_message("INFO", "Sending hangup to Phone Pid [~p]~n", [LocalPhonePid]),
            phone:reply(LocalPhonePid, hangup),
            NewStateData = set_state_data_values(StateData, [{lastcall, {connecting, {hangup, RemoteFSMPid}}},
                                                             {remotefsmpid, void},
                                                             {remotems, void}]),
            {next_state, idle, NewStateData};
        _ ->
            log_message("ERROR", "Attempt to hangup received from invalid FSM Pid [~p]~n", [RemoteFSMPid]),
            NewStateData = set_state_data_values(StateData, [{lastcall, {connecting, {hangup, RemoteFSMPid}}}]),
            {next_state, connecting, NewStateData}
    end;
    
connecting(Ignore, StateData) ->
    log_message("ERROR", "FSM Pid [~p] received invalid event [~p] in connecting. Ignored. State data [~p]~n",
              [self(), Ignore, StateData]),
    NewStateData = set_state_data_values(StateData, [{lastcall,{connecting,Ignore}}]),
    log_message("DEBUG", "NewStateData [~p]~n", [NewStateData]),
    {next_state, connecting, NewStateData}.


%%------------------------------------------------------------------------------
%% Function: receiving/2
%% Purpose:  Receiving state handler - we are connected to another phone
%% Args:     The event to be handled
%%           The current state data.
%%
%% Actions taken for each event:
%%
%% Event              Actions                                    New State Data                Next State
%% ------------------ ------------------------------------       ---------------------         ----------- 
%% hangup (local)     Local phone Pid matches state data:        Remove RemoteMs,RemoteFSMPid  idle
%%                    Send hangup to Remote FSM
%%                    Local phone Pid does not match state data: No change                     receiving
%%                    log error
%% hangup (remote)    Remote FSM Pid matches state data:         Remove RemoteMs,RemoteFSMPid  idle
%%                    Send hangup to Local phone
%%                    Remote FSM Pid does not match state data:  No change                     receiving
%%                    log error
%% inbound (remote)   Send busy to Calling FSM                   No change                     receiving
%% other              log error                                  No change                     receiving
%%------------------------------------------------------------------------------

receiving({action, {hangup, LocalPhonePid}}, StateData) ->
    case get_state_data_values(StateData, [localphonepid]) of
        [LocalPhonePid] ->
            [RemoteFSMPid] = get_state_data_values(StateData, [remotefsmpid]),
            log_message("INFO", "Received hangup from phone Pid [~p]. Terminating call~n", [LocalPhonePid]),
            phone_fsm:hangup(RemoteFSMPid),
            NewStateData = set_state_data_values(StateData, [{lastcall, {receiving, {hangup, LocalPhonePid}}},
                                                             {remotems, void},
                                                             {remotefsmpid, void}]),
            {next_state, idle, NewStateData};
        _ ->
            log_message("ERROR", "Attempt to hangup received from invalid phone Pid [~p]~n", [LocalPhonePid]),
            NewStateData = set_state_data_values(StateData, [{lastcall, {receiving, {hangup, LocalPhonePid}}}]),
            {next_state, receiving, NewStateData}
    end;

receiving({hangup, RemoteFSMPid}, StateData) ->
    case get_state_data_values(StateData, [remotefsmpid]) of
        [RemoteFSMPid] ->
            [LocalPhonePid] = get_state_data_values(StateData, [localphonepid]),
            log_message("INFO", "Received hangup from FSM Pid [~p]. Terminating call~n", [RemoteFSMPid]),
            phone:reply(LocalPhonePid,hangup),
            NewStateData = set_state_data_values(StateData, [{lastcall, {receiving, {hangup, RemoteFSMPid}}},
                                                             {remotems, void},
                                                             {remotefsmpid, void}]),
            {next_state, idle, NewStateData};
        _ ->
            log_message("ERROR", "Attempt to hangup received from invalid FSM Pid [~p]~n", [RemoteFSMPid]),
            NewStateData = set_state_data_values(StateData, [{lastcall, {receiving, {hangup, RemoteFSMPid}}}]),
            {next_state, receiving, NewStateData}
    end;

receiving({inbound, RemoteFSMPid}, StateData) ->
    phone_fsm:busy(RemoteFSMPid),
    NewStateData = set_state_data_values(StateData, [{lastcall, {receiving, {inbound, RemoteFSMPid}}}]),
    {next_state, receiving, NewStateData};
    
receiving(Ignore, StateData) ->
    log_message("ERROR", "FSM Pid [~p] received invalid event [~p] in receiving. Ignored. State data [~p]~n",
              [self(), Ignore, StateData]),
    NewStateData = set_state_data_values(StateData, [{lastcall,{receiving,Ignore}}]),
    log_message("DEBUG", "NewStateData [~p]~n", [NewStateData]),
    {next_state, receiving, NewStateData}.


%%------------------------------------------------------------------------------
%% Utility Functions
%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% Function: new_state_data/0
%% Purpose:  Initialises and returns state data
%%------------------------------------------------------------------------------

new_state_data() ->
    #{localms => void,
      localphonepid => void,
      remotems => void,
      remotefsmpid => void,
      lastcall => void,
      freq => void}.

%%------------------------------------------------------------------------------
%% Function: set_state_data_values/2
%% Purpose:  Updates state data with the values in the given list
%% Args:     The state data to be updated
%%           The list of values from which the state data is updated, each
%%           represented as a key/value pair.
%%------------------------------------------------------------------------------

set_state_data_values(StateData, []) -> StateData;
set_state_data_values(StateData, [{Key,Value}|KeyValues]) ->
    NewStateData = maps:update(Key, Value, StateData),
    set_state_data_values(NewStateData, KeyValues).

%%------------------------------------------------------------------------------
%% Function: get_state_data_values/2
%% Purpose:  Returns a list of state data values for each key requested.
%% Args:     The state data from which the values are to be extracted.
%%           The list of keys to extract from the state data.
%%------------------------------------------------------------------------------

get_state_data_values(StateData, Keys) ->
    [maps:get(Key, StateData) || Key <- Keys].
   

%%------------------------------------------------------------------------------
%% Test Functions
%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% Function: test_state_data/0
%% Purpose:  Simple unit test for state data handing functions.
%%------------------------------------------------------------------------------

test_state_data() ->
    StateData = new_state_data(),
    [void] = get_state_data_values(StateData, [localms]),
    NewStateData = set_state_data_values(StateData, [{remotems, "123"},{localms, "456"}]),
    ["456","123"] = get_state_data_values(NewStateData, [localms,remotems]),
    ok.

%%------------------------------------------------------------------------------
%% Function: log_message/3
%% Purpose:  Message logger for debugging purposes
%%           Set Debug to true to enable debug messages
%%------------------------------------------------------------------------------
log_message(Level, Message, Parameters) ->
    Debug = false,
    case Debug of
        true -> io:format(Level ++ ": " ++ Message ++ "~n", Parameters);
        _ -> ok

    end.
