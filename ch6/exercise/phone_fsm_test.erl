-module(phone_fsm_test).

%%%-------------------------------------------------------------------------------------------------
%%% Description module phone_fsm_test
%%%-------------------------------------------------------------------------------------------------
%%% Defines a set of eunit tests for the phone and phone_fsm modules
%%%-------------------------------------------------------------------------------------------------

-include_lib("eunit/include/eunit.hrl").

%%-------------------------------------------------------------------------------------------------
%% Test Functions
%%-------------------------------------------------------------------------------------------------

%%-------------------------------------------------------------------------------------------------
%% The tests focus on checking that a phone FSM process behaves as expected for possible event
%% in each of the possible states. An FSM has to be able to handle events from its associated
%% phone process, another FSM as well as from 'rogue' processes e.g. a phone which isn't its
%% associated phone.
%%
%% The following table shows whether an event is valid or not in a given state.
%%
%% Key to State values:
%%     Id: Idle
%%     Ca: Calling
%%     Co: Connecting
%%     Re: Receiving
%%
%% Key to table entries:
%%     V: Valid event for this state.
%%     X: Invalid event for this state.
%%
%% Event     From   State
%% -----     ----   -----
%%                  Id   Ca   Co   Re
%%                  --   --   --   --
%% Outbound  Phone  V    X    X    X
%% Accept    Phone  X    X    V    X
%% Reject    Phone  X    X    V    X
%% Hangup    Phone  X    V    X    V
%%
%% Busy      FSM    X    V    X    X
%% Reject    FSM    X    V    X    X
%% Accept    FSM    X    V    X    X
%% Hangup    FSM    X    X    V    V
%% Inbound   FSM    V    V    V    V
%%-------------------------------------------------------------------------------------------------

%%-------------------------------------------------------------------------------------------------
%% Tests for valid actions
%%-------------------------------------------------------------------------------------------------

%%-------------------------------------------------------------------------------------------------
%% Function: phone_init_test/0
%% Purpose:  Checks that the phone and its associated FSM are idle when the phone has been started
%%-------------------------------------------------------------------------------------------------
phone_init_test() ->

    [{P123,PFSM123}] = setup_phones(["123"]),
    check_status("123", P123, init, PFSM123, {connect, {P123}}, idle, void, void),
    teardown_phones([P123]).

%%-------------------------------------------------------------------------------------------------
%% Function: call_initiated_test/0
%% Purpose:  Checks the state of the phones/FSMs when one phone calls another
%%           Expect the calling phone to be "calling" and the called phone to be "connecting"
%%-------------------------------------------------------------------------------------------------
call_initiated_test() ->

    [{P123,PFSM123}, {P124,PFSM124}] = setup_phones(["123", "124"]),
    make_call(P123, "124"),
    check_status("123", P123, {action, {call, "124"}}, PFSM123, {idle, {outbound, "124"}}, calling, "124", PFSM124),
    check_status("124", P124, {reply, {inbound, "123"}}, PFSM124, {idle, {inbound, PFSM123}}, connecting, "123", PFSM123),
    teardown_phones([P123,P124]).

%%-------------------------------------------------------------------------------------------------
%% Function: call_accepted_test/0
%% Purpose:  Checks the state of the phones/FSMs when a phone accepts an incoming call
%%           Expect both phones to be "receiving"
%%-------------------------------------------------------------------------------------------------
call_accepted_test() ->

    [{P123,PFSM123}, {P124,PFSM124}] = setup_phones(["123", "124"]),
    make_and_accept_call(P123, P124, "124"),
    check_status("123", P123, {reply, accept}, PFSM123, {calling, {accept, PFSM124}}, receiving, "124", PFSM124),
    check_status("124", P124, {action, accept}, PFSM124, {connecting, {accept, P124}}, receiving, "123", PFSM123),
    teardown_phones([P123,P124]).

%%-------------------------------------------------------------------------------------------------
%% Function: call_terminated_test/0
%% Purpose:  Checks the state of the phones/FSMs when a phone terminates a call
%%           Expect both phones to be "idle"
%%-------------------------------------------------------------------------------------------------
call_terminated_test() ->

    [{P123,PFSM123}, {P124,PFSM124}] = setup_phones(["123", "124"]),
    make_and_accept_call(P123, P124, "124"),
    hangup_phone(P123),
    check_status("123", P123, {action, hangup}, PFSM123, {receiving, {hangup, P123}}, idle, void, void),
    check_status("124", P124, {reply, hangup}, PFSM124, {receiving, {hangup, PFSM123}}, idle, void, void),
    teardown_phones([P123,P124]).

%%-------------------------------------------------------------------------------------------------
%% Function: call_self_test/0
%% Purpose:  Checks the state of a phone/FSM when a phone calls itself
%%           Expect the phone to be "idle" having received a busy reply
%%-------------------------------------------------------------------------------------------------
call_self_test() ->

    [{P123,PFSM123}] = setup_phones(["123"]),
    make_call(P123, "123"),
    check_status("123", P123, {reply, busy}, PFSM123, {calling, {busy, PFSM123}}, idle, void, void),
    teardown_phones([P123]).

%%-------------------------------------------------------------------------------------------------
%% Function: phone_busy_test/0
%% Purpose:  Checks that a busy response is received when one phone calls another which is already
%%           participating in a call.
%%           Expect the phone that receives the busy response to be "idle" after receiving the busy reply
%%-------------------------------------------------------------------------------------------------
phone_busy_test() ->

    [{P123,PFSM123}, {P124,PFSM124}, {P125,PFSM125}] = setup_phones(["123", "124", "125"]),

    % 123 calls 124
    make_call(P123, "124"),

    % 125 calls 123
    make_call(P125, "123"),

    % Check that 125 returns to idle after getting a busy response from 123
    check_status("123", P123, {action, {call, "124"}}, PFSM123, {calling, {inbound, PFSM125}}, calling, "124", PFSM124),
    check_status("124", P124, {reply, {inbound, "123"}}, PFSM124, {idle, {inbound, PFSM123}}, connecting, "123", PFSM123),
    check_status("125", P125, {reply, busy}, PFSM125, {calling, {busy, PFSM123}}, idle, void, void),

    % 125 calls 124
    make_call(P125, "124"),

    % Check that 125 returns to idle after getting a busy response from 124
    check_status("123", P123, {action, {call, "124"}}, PFSM123, {calling, {inbound, PFSM125}}, calling, "124", PFSM124),
    check_status("124", P124, {reply, {inbound, "123"}}, PFSM124, {connecting, {inbound, PFSM125}}, connecting, "123", PFSM123),
    check_status("125", P125, {reply, busy}, PFSM125, {calling, {busy, PFSM124}}, idle, void, void),

    % 124 accepts 123's call
    accept_call(P124),

    % 125 calls 123
    make_call(P125, "123"),

    % Check that 125 returns to idle after getting a busy response from 123
    check_status("123", P123, {reply, accept}, PFSM123, {receiving, {inbound, PFSM125}}, receiving, "124", PFSM124),
    check_status("124", P124, {action, accept}, PFSM124, {connecting, {accept, P124}}, receiving, "123", PFSM123),
    check_status("125", P125, {reply, busy}, PFSM125, {calling, {busy, PFSM123}}, idle, void, void),

    % 125 calls 124
    make_call(P125, "124"),

    % Check that 125 returns to idle after getting a busy response from 124
    check_status("123", P123, {reply, accept}, PFSM123, {receiving, {inbound, PFSM125}}, receiving, "124", PFSM124),
    check_status("124", P124, {action, accept}, PFSM124, {receiving, {inbound, PFSM125}}, receiving, "123", PFSM123),
    check_status("125", P125, {reply, busy}, PFSM125, {calling, {busy, PFSM124}}, idle, void, void),

    teardown_phones([P123,P124,P125]).

%%-------------------------------------------------------------------------------------------------
%% Function: call_rejected_test/0
%% Purpose:  Checks the state of a phone/FSM whose call has been rejected
%%           Expect the phone to be "idle" having received a rejected reply
%%-------------------------------------------------------------------------------------------------
call_rejected_test() ->

    [{P123,PFSM123}, {P124,PFSM124}] = setup_phones(["123", "124"]),
    make_call(P123, "124"),
    reject_call(P124),
    check_status("123", P123, {reply, reject}, PFSM123, {calling, {reject, PFSM124}}, idle, void, void),
    check_status("124", P124, {action, reject}, PFSM124, {connecting, {reject, P124}}, idle, void, void),
    teardown_phones([P123,P124]).

%%-------------------------------------------------------------------------------------------------
%% Function: call_unanswered_hangup_test/0
%% Purpose:  Checks the state of a phone/FSM when the subscriber has hung-up after their call was unanswered
%%           Expect the phone to be "idle" after hanging-up.
%%-------------------------------------------------------------------------------------------------
call_unanswered_hangup_test() ->

    [{P123,PFSM123}, {P124,PFSM124}] = setup_phones(["123", "124"]),
    make_call(P123, "124"),
    hangup_phone(P123),
    check_status("123", P123, {action, hangup}, PFSM123, {calling, {hangup, P123}}, idle, void, void),
    check_status("124", P124, {reply, hangup}, PFSM124, {connecting, {hangup, PFSM123}}, idle, void, void),
    teardown_phones([P123,P124]).

%%-------------------------------------------------------------------------------------------------
%% Tests for invalid actions and valid actions from the 'wrong' process
%%-------------------------------------------------------------------------------------------------

%%-------------------------------------------------------------------------------------------------
%% Function: invalid_accept_from_phone_in_idle_test/0
%% Purpose:  Checks the state of a phone/FSM when the phone receives an unexpected "accept"
%%           when idle.
%%-------------------------------------------------------------------------------------------------
invalid_accept_from_phone_in_idle_test() -> check_invalid_idle_from_phone(accept).

%%-------------------------------------------------------------------------------------------------
%% Function: invalid_reject_from_phone_in_idle_test/0
%% Purpose:  Checks the state of a phone/FSM when the phone receives an unexpected "reject"
%%           when idle.
%%-------------------------------------------------------------------------------------------------
invalid_reject_from_phone_in_idle_test() -> check_invalid_idle_from_phone(reject).

%%-------------------------------------------------------------------------------------------------
%% Function: invalid_hangup_from_phone_in_idle_test/0
%% Purpose:  Checks the state of a phone/FSM when the phone receives an unexpected "hangup"
%%           when idle.
%%-------------------------------------------------------------------------------------------------
invalid_hangup_from_phone_in_idle_test() -> check_invalid_idle_from_phone(hangup).

%%-------------------------------------------------------------------------------------------------
%% Function: check_invalid_idle_from_phone/0
%% Purpose:  Helper function for unexpected messages during idle.
%%-------------------------------------------------------------------------------------------------
check_invalid_idle_from_phone(Action) ->

    [{P123,PFSM123}] = setup_phones(["123"]),
    ok = phone:action(P123, Action),
    check_status("123", P123, {action, Action}, PFSM123, {idle, {action, {Action, P123}}}, idle, void, void),
    teardown_phones([P123]).

%%-------------------------------------------------------------------------------------------------
%% Function: valid_action_from_wrong_phone_in_idle_test/0
%% Purpose:  Checks the state of a phone/FSM when the FSM is sent an "outbound" request from a
%%           process other than its associated phone.
%%           Expect the FSM to ignore the request and remain in idle.
%%-------------------------------------------------------------------------------------------------
valid_action_from_wrong_phone_in_idle_test() ->

    [{P123,PFSM123}] = setup_phones(["123"]),
    phone_fsm:action(PFSM123, {outbound,"124"}),
    check_status("123", P123, init, PFSM123, {idle, {action, {{outbound, "124"}, self()}}}, idle, void, void),
    teardown_phones([P123]).

%%-------------------------------------------------------------------------------------------------
%% Function: invalid_event_from_fsm_in_idle_test/0
%% Purpose:  Checks the state of a phone/FSM when the FSM is sent events it does not expect from
%%           another FSM when in idle.
%%           Expect the FSM to ignore the request and remain in idle.
%%-------------------------------------------------------------------------------------------------
invalid_event_from_fsm_in_idle_test() ->

    [{P123,PFSM123}] = setup_phones(["123"]),

    % Send busy to idle FSM
    phone_fsm:busy(PFSM123),
    check_status("123", P123, init, PFSM123, {idle, {busy, self()}}, idle, void, void),

    % Send reject to idle FSM
    phone_fsm:reject(PFSM123),
    check_status("123", P123, init, PFSM123, {idle, {reject, self()}}, idle, void, void),

    % Send accept to idle FSM
    phone_fsm:accept(PFSM123),
    check_status("123", P123, init, PFSM123, {idle, {accept, self()}}, idle, void, void),

    % Send hangup to idle FSM
    phone_fsm:hangup(PFSM123),
    check_status("123", P123, init, PFSM123, {idle, {hangup, self()}}, idle, void, void),

    teardown_phones([P123]).

%%-------------------------------------------------------------------------------------------------
%% Function: valid_event_from_wrong_fsm_in_idle_test/0
%% Purpose:  Checks the state of a phone/FSM when the FSM is sent an "inbound" request from a
%%           process that is not registered in the HLR.
%%           Expect the FSM to ignore the request and remain in idle.
%%-------------------------------------------------------------------------------------------------
valid_event_from_wrong_fsm_in_idle_test() ->

    [{P123,PFSM123}] = setup_phones(["123"]),
    phone_fsm:inbound(PFSM123),
    pause(),
    check_status("123", P123, init, PFSM123, {idle, {inbound, self()}}, idle, void, void),
    teardown_phones([P123]).

%%-------------------------------------------------------------------------------------------------
%% Function: invalid_action_from_phone_in_calling_test/0
%% Purpose:  Checks the state of a phone/FSM when the phone has initiated a call and then
%%           sends an invalid request to its associated FSM.
%%           Expect the FSM to ignore the request and remain in the calling state.
%%-------------------------------------------------------------------------------------------------
invalid_action_from_phone_in_calling_test() ->

    [{P123,PFSM123}, {P124,PFSM124}] = setup_phones(["123", "124"]),
    make_call(P123, "124"),

    % test invalid outbound
    ok = phone:action(P123, {call,"124"}),
    pause(),
    check_status("123", P123, {action, {call,"124"}}, PFSM123, {calling, {action, {{outbound,"124"}, P123}}}, calling, "124", PFSM124),

    % test invalid accept
    accept_call(P123),
    check_status("123", P123, {action, accept}, PFSM123, {calling, {action, {accept,P123}}}, calling, "124", PFSM124),

    % test invalid reject
    reject_call(P123),
    check_status("123", P123, {action, reject}, PFSM123, {calling, {action, {reject,P123}}}, calling, "124", PFSM124),

    teardown_phones([P123,P124]).

%%-------------------------------------------------------------------------------------------------
%% Function: valid_action_from_wrong_phone_in_calling_test/0
%% Purpose:  Checks the state of a phone/FSM when the FSM is sent an "hangup" request from a
%%           process other than its associated phone.
%%           Expect the local FSM to ignore the request and remain in calling and the remote FSM
%%           to remain in connecting.
%%-------------------------------------------------------------------------------------------------
valid_action_from_wrong_phone_in_calling_test() ->

    [{P123,PFSM123}, {P124,PFSM124}] = setup_phones(["123", "124"]),
    make_call(P123, "124"),
    phone_fsm:action(PFSM123, hangup),
    check_status("123", P123, {action, {call,"124"}}, PFSM123, {calling, {hangup,self()}}, calling, "124", PFSM124),
    check_status("124", P124, {reply, {inbound,"123"}}, PFSM124, {idle, {inbound, PFSM123}}, connecting, "123", PFSM123),
    teardown_phones([P123,P124]).

%%-------------------------------------------------------------------------------------------------
%% Function: invalid_event_from_fsm_in_calling_test/0
%% Purpose:  Checks the state of a phone/FSM when the FSM is sent a hangup from another FSM
%%           when in calling.
%%           Expect the FSM to ignore the request and remain in calling.
%%-------------------------------------------------------------------------------------------------
invalid_event_from_fsm_in_calling_test() ->

    [{P123,PFSM123}, {P124,PFSM124}] = setup_phones(["123", "124"]),
    make_call(P123, "124"),

    % test invalid hangup
    phone_fsm:hangup(PFSM123),
    check_status("123", P123, {action, {call,"124"}}, PFSM123, {calling, {hangup, self()}}, calling, "124", PFSM124),

    teardown_phones([P123,P124]).

%%-------------------------------------------------------------------------------------------------
%% Function: valid_event_from_wrong_fsm_in_calling_test/0
%% Purpose:  Checks the state of a phone/FSM when the FSM is sent valid events for the calling
%%           state but from a process that is not registered in the HLR.
%%           Expect the FSM to ignore the request and remain in calling.
%%-------------------------------------------------------------------------------------------------
valid_event_from_wrong_fsm_in_calling_test() ->

    [{P123,PFSM123}, {P124,PFSM124}] = setup_phones(["123", "124"]),
    make_call(P123, "124"),

    % Busy from invalid FSM
    phone_fsm:busy(PFSM123),
    check_status("123", P123, {action, {call, "124"}}, PFSM123, {calling, {busy, self()}}, calling, "124", PFSM124),
    check_status("124", P124, {reply, {inbound, "123"}}, PFSM124, {idle, {inbound, PFSM123}}, connecting, "123", PFSM123),

    % Reject from invalid FSM
    phone_fsm:reject(PFSM123),
    check_status("123", P123, {action, {call, "124"}}, PFSM123, {calling, {reject, self()}}, calling, "124", PFSM124),
    check_status("124", P124, {reply, {inbound, "123"}}, PFSM124, {idle, {inbound, PFSM123}}, connecting, "123", PFSM123),

    % Accept from invalid FSM
    phone_fsm:accept(PFSM123),
    check_status("123", P123, {action, {call, "124"}}, PFSM123, {calling, {accept, self()}}, calling, "124", PFSM124),
    check_status("124", P124, {reply, {inbound, "123"}}, PFSM124, {idle, {inbound, PFSM123}}, connecting, "123", PFSM123),

    teardown_phones([P123,P124]).

%%-------------------------------------------------------------------------------------------------
%% Function: invalid_action_from_phone_in_connecting_test/0
%% Purpose:  Checks the state of a phone/FSM when the phone has been called and then
%%           sends an invalid request to its associated FSM.
%%           Expect the FSM to ignore the request and remain in the connecting state.
%%-------------------------------------------------------------------------------------------------
invalid_action_from_phone_in_connecting_test() ->

    [{P123,PFSM123}, {P124,PFSM124}] = setup_phones(["123", "124"]),
    make_call(P123, "124"),

    % test invalid outbound
    make_call(P124, "123"),
    check_status("124", P124, {action, {call,"123"}}, PFSM124, {connecting, {action, {{outbound,"123"}, P124}}}, connecting, "123", PFSM123),

    % test invalid hangup
    hangup_phone(P124),
    check_status("124", P124, {action, hangup}, PFSM124, {connecting, {action, {hangup,P124}}}, connecting, "123", PFSM123),

    teardown_phones([P123,P124]).

%%-------------------------------------------------------------------------------------------------
%% Function: valid_action_from_wrong_phone_in_connecting_test/0
%% Purpose:  Checks the state of a phone/FSM when the FSM is sent a valid request from a
%%           process other than its associated phone.
%%           Expect the local FSM to ignore the request and remain in connecting and the remote FSM
%%           to remain in calling.
%%-------------------------------------------------------------------------------------------------
valid_action_from_wrong_phone_in_connecting_test() ->

    [{P123,PFSM123}, {P124,PFSM124}] = setup_phones(["123", "124"]),
    make_call(P123, "124"),

    % Accept from wrong phone
    phone_fsm:action(PFSM124, accept),
    pause(),
    check_status("123", P123, {action, {call,"124"}}, PFSM123, {idle, {outbound, "124"}}, calling, "124", PFSM124),
    check_status("124", P124, {reply, {inbound,"123"}}, PFSM124, {connecting, {accept, self()}}, connecting, "123", PFSM123),

    % Reject from wrong phone
    phone_fsm:action(PFSM124, reject),
    pause(),
    check_status("123", P123, {action, {call,"124"}}, PFSM123, {idle, {outbound, "124"}}, calling, "124", PFSM124),
    check_status("124", P124, {reply, {inbound,"123"}}, PFSM124, {connecting, {reject, self()}}, connecting, "123", PFSM123),

    teardown_phones([P123,P124]).

%%-------------------------------------------------------------------------------------------------
%% Function: invalid_event_from_fsm_in_connecting_test/0
%% Purpose:  Checks the state of a phone/FSM when the FSM is sent an invalid event from another FSM
%%           when in connecting.
%%           Expect the FSM to ignore the request and remain in connecting.
%%-------------------------------------------------------------------------------------------------
invalid_event_from_fsm_in_connecting_test() ->

    [{P123,PFSM123}, {P124,PFSM124}] = setup_phones(["123", "124"]),
    make_call(P123, "124"),

    % test invalid busy
    phone_fsm:busy(PFSM124),
    check_status("124", P124, {reply, {inbound,"123"}}, PFSM124, {connecting, {busy, self()}}, connecting, "123", PFSM123),

    % test invalid reject
    phone_fsm:reject(PFSM124),
    check_status("124", P124, {reply, {inbound,"123"}}, PFSM124, {connecting, {reject, self()}}, connecting, "123", PFSM123),

    % test invalid accept
    phone_fsm:accept(PFSM124),
    check_status("124", P124, {reply, {inbound,"123"}}, PFSM124, {connecting, {accept, self()}}, connecting, "123", PFSM123),

    teardown_phones([P123,P124]).

%%-------------------------------------------------------------------------------------------------
%% Function: valid_event_from_wrong_fsm_in_connecting_test/0
%% Purpose:  Checks the state of a phone/FSM when the FSM is sent valid events for the connecting
%%           state but from a process that is not registered in the HLR.
%%           Expect the FSM to ignore the request and remain in connecting.
%%-------------------------------------------------------------------------------------------------
valid_event_from_wrong_fsm_in_connecting_test() ->

    [{P123,PFSM123}, {P124,PFSM124}] = setup_phones(["123", "124"]),
    make_call(P123, "124"),

    % Hangup from invalid FSM
    phone_fsm:hangup(PFSM124),
    check_status("123", P123, {action, {call, "124"}}, PFSM123, {idle, {outbound, "124"}}, calling, "124", PFSM124),
    check_status("124", P124, {reply, {inbound, "123"}}, PFSM124, {connecting, {hangup, self()}}, connecting, "123", PFSM123),

    teardown_phones([P123,P124]).

%%-------------------------------------------------------------------------------------------------
%% Function: invalid_action_from_phone_in_receiving_test/0
%% Purpose:  Checks the state of a phone/FSM when the phone has accepted a call and then
%%           sends an invalid request to its associated FSM.
%%           Expect the FSM to ignore the request and remain in the receiving state.
%%-------------------------------------------------------------------------------------------------
invalid_action_from_phone_in_receiving_test() ->

    [{P123,PFSM123}, {P124,PFSM124}] = setup_phones(["123", "124"]),
    make_and_accept_call(P123, P124, "124"),

    % test invalid outbound
    make_call(P123, "124"),
    check_status("123", P123, {action, {call,"124"}}, PFSM123, {receiving, {action, {{outbound,"124"}, P123}}}, receiving, "124", PFSM124),

    % test invalid accept
    accept_call(P123),
    check_status("123", P123, {action, accept}, PFSM123, {receiving, {action, {accept,P123}}}, receiving, "124", PFSM124),

    % test invalid reject
    reject_call(P123),
    check_status("123", P123, {action, reject}, PFSM123, {receiving, {action, {reject,P123}}}, receiving, "124", PFSM124),

    teardown_phones([P123,P124]).

%%-------------------------------------------------------------------------------------------------
%% Function: valid_action_from_wrong_phone_in_receiving_test/0
%% Purpose:  Checks the state of a phone/FSM when the FSM is sent a valid request from a
%%           process other than its associated phone.
%%           Expect local FSM to ignore the request and both FSMs to remaining in receiving.
%%-------------------------------------------------------------------------------------------------
valid_action_from_wrong_phone_in_receiving_test() ->

    [{P123,PFSM123}, {P124,PFSM124}] = setup_phones(["123", "124"]),
    make_and_accept_call(P123, P124, "124"),
    phone_fsm:action(PFSM124, hangup),
    pause(),
    check_status("123", P123, {reply, accept}, PFSM123, {calling, {accept, PFSM124}}, receiving, "124", PFSM124),
    check_status("124", P124, {action, accept}, PFSM124, {receiving, {hangup, self()}}, receiving, "123", PFSM123),
    teardown_phones([P123,P124]).

%%-------------------------------------------------------------------------------------------------
%% Function: invalid_event_from_fsm_in_receiving_test/0
%% Purpose:  Checks the state of a phone/FSM when the FSM is sent an invalid event from another FSM
%%           when in receiving.
%%           Expect the FSM to ignore the request and remain in receiving.
%%-------------------------------------------------------------------------------------------------
invalid_event_from_fsm_in_receiving_test() ->

    [{P123,PFSM123}, {P124,PFSM124}] = setup_phones(["123", "124"]),
    make_and_accept_call(P123, P124, "124"),

    % test invalid busy
    phone_fsm:busy(PFSM124),
    check_status("124", P124, {action, accept}, PFSM124, {receiving, {busy, self()}}, receiving, "123", PFSM123),

    % test invalid reject
    phone_fsm:reject(PFSM124),
    check_status("124", P124, {action, accept}, PFSM124, {receiving, {reject, self()}}, receiving, "123", PFSM123),

    % test invalid accept
    phone_fsm:accept(PFSM124),
    check_status("124", P124, {action, accept}, PFSM124, {receiving, {accept, self()}}, receiving, "123", PFSM123),

    teardown_phones([P123,P124]).

%%-------------------------------------------------------------------------------------------------
%% Function: valid_event_from_wrong_fsm_in_receiving_test/0
%% Purpose:  Checks the state of a pair of phones/FSMs when one FSM is sent valid events for the
%%           receiving state but from a process that is not registered in the HLR.
%%           Expect the FSM to ignore the request and both FSMs to remain in receiving.
%%-------------------------------------------------------------------------------------------------
valid_event_from_wrong_fsm_in_receiving_test() ->

    [{P123,PFSM123}, {P124,PFSM124}] = setup_phones(["123", "124"]),
    make_and_accept_call(P123, P124, "124"),

    % Hangup from invalid FSM
    phone_fsm:hangup(PFSM124),
    check_status("123", P123, {reply, accept}, PFSM123, {calling, {accept, PFSM124}}, receiving, "124", PFSM124),
    check_status("124", P124, {action, accept}, PFSM124, {receiving, {hangup, self()}}, receiving, "123", PFSM123),

    teardown_phones([P123,P124]).


%%-------------------------------------------------------------------------------------------------
%% Utility Functions
%%-------------------------------------------------------------------------------------------------

%%-------------------------------------------------------------------------------------------------
%% Function: setup_phones/1
%% Purpose:  Initialises a test by creating a new HLR and starting a new phone/FSM pair for each
%%           of the phone numbers passed into the function as a list.
%% Args:     List of the phone numbers for which phone/FSM processes are to be created.
%%-------------------------------------------------------------------------------------------------
setup_phones(MsList) -> hlr:new(), setup_phones(MsList, []).
setup_phones([], PidList) -> lists:reverse(PidList);
setup_phones([Ms|MsTail], PidList) -> setup_phones(MsTail, [start_phone(Ms)|PidList]).

%%-------------------------------------------------------------------------------------------------
%% Function: teardown_phones/1
%% Purpose:  Tidies-up at the end of a test by creating stopping each of the phone processes (and
%%           associated FSM processes) passed into the function as a list.
%%           Finally, deletes the HLR.
%% Args:     List of the pids for the phone processes to be stopped.
%%-------------------------------------------------------------------------------------------------
teardown_phones(PhonePidList) -> teardown_phone_list(PhonePidList), hlr:delete().
teardown_phone_list([]) -> ok;
teardown_phone_list([PhonePid|PhonePidTail]) -> phone:stop(PhonePid), teardown_phone_list(PhonePidTail).

%%-------------------------------------------------------------------------------------------------
%% Function: pause/0
%% Purpose:  Sleeps for a short period of time to allow messages to propogate round the system.
%%           This helps avoid race conditions while running the tests.`
%%-------------------------------------------------------------------------------------------------
pause() -> timer:sleep(50).

%%-------------------------------------------------------------------------------------------------
%% Function: make_call/2
%% Purpose:  Helper function that allows one phone to call another.
%% Args:     1. The pid of the calling phone process.
%%           2. The phone number of the called phone.
%%-------------------------------------------------------------------------------------------------
make_call(CallerPhonePid, CalledMs) ->
    ok = phone:action(CallerPhonePid, {call, CalledMs}), pause().

%%-------------------------------------------------------------------------------------------------
%% Function: accept_call/1
%% Purpose:  Helper function that allows a phone to accept a call.
%% Args:     1. The pid of the called phone process.
%%-------------------------------------------------------------------------------------------------
accept_call(CalledPhonePid) ->
    ok = phone:action(CalledPhonePid, accept), pause().

%%-------------------------------------------------------------------------------------------------
%% Function: hangup_call/1
%% Purpose:  Helper function that allows a phone to hangup a call.
%% Args:     1. The pid of the phone process that hangs-up.
%%-------------------------------------------------------------------------------------------------
hangup_phone(PhonePid) ->
    ok = phone:action(PhonePid, hangup), pause().

%%-------------------------------------------------------------------------------------------------
%% Function: reject_call/1
%% Purpose:  Helper function that allows a phone to reject a call.
%% Args:     1. The pid of the phone process that rejects the call.
%%-------------------------------------------------------------------------------------------------
reject_call(PhonePid) ->
    ok = phone:action(PhonePid, reject), pause().

%%-------------------------------------------------------------------------------------------------
%% Function: make_and_accept_call/3
%% Purpose:  Helper function that allows one phone to make a call and the called phone to accept.
%% Args:     1. The pid of the calling phone process.
%%           2. The pid of the called phone process.
%%           3. The phone number of the called phone.
%%-------------------------------------------------------------------------------------------------
make_and_accept_call(CallerPhonePid, CalledPhonePid, CalledMs) ->
    make_call(CallerPhonePid, CalledMs),
    accept_call(CalledPhonePid).

%%-------------------------------------------------------------------------------------------------
%% Function: start_phone/1
%% Purpose:  Helper function that starts a phone process (which in turn starts the associated FSM
%%           process).
%%           Checks that both processes have been started with the correct process names.
%% Args:     1. The phone number of the phone to be started.
%% Returns:  1. The pid of the phone process.
%%           2. The pid of the FSM process.
%%-------------------------------------------------------------------------------------------------
start_phone(LocalMs) ->
    {ok,PhonePid}=phone:start_link(LocalMs),
    ?assert(is_pid(whereis(list_to_atom("phone"++LocalMs)))),

    PhoneFSMPid=whereis(list_to_atom("phone_fsm"++LocalMs)),
    ?assert(is_pid(whereis(list_to_atom("phone_fsm"++LocalMs)))),

    {PhonePid,PhoneFSMPid}.

%%-------------------------------------------------------------------------------------------------
%% Function: check_status/8
%% Purpose:  Helper function that checks that the status of a phone/FSM process pair match the
%%           expected values.
%%           The asserts raise an exception if any actual value does not match the expected value.
%% Args:     1. The expected phone number of the local phone.
%%           2. The expected pid of the local phone process.
%%           3. The expected value of the last function call to the local phone process.
%%           4. The expected pid of the FSM process associated with the local phone.
%%           5. The expected value of the last function call to the local FSM process.
%%           6. The expected state of the local FSM process.
%%           7. The expected phone number of the remote phone.
%%           8. The expected pid of the remote FSM process.
%%-------------------------------------------------------------------------------------------------
check_status(ExpectedLocalMs, ExpectedLocalPhonePid, ExpectedLocalPhoneLastCall,
             ExpectedLocalFSMPid, ExpectedLocalFSMLastCall, ExpectedLocalFSMState,
             ExpectedRemoteMs, ExpectedRemoteFSMPid) ->

    % Check phone status
    ActualLocalPhoneStatus = phone:status(ExpectedLocalPhonePid),
    ?assert({{loop_data, {ExpectedLocalMs, ExpectedLocalFSMPid, ExpectedLocalPhoneLastCall}}} =:= ActualLocalPhoneStatus),

    % Check phone FSM status
    {{state, ActualLocalFSMStateName}, {loop_data, ActualLocalFSMStateData}} = phone_fsm:status(ExpectedLocalFSMPid),

    % Check FSM state name
    ?assert(ActualLocalFSMStateName =:= ExpectedLocalFSMState),

    % Check FSM state data
    [ActualLocalMs,ActualLocalPhonePid,ActualLocalFSMLastCall,ActualRemoteMs,ActualRemoteFSMPid] =
        phone_fsm:get_state_data_values(ActualLocalFSMStateData, [localms,localphonepid,lastcall,remotems,remotefsmpid]),
    ?assert(ActualLocalMs =:= ExpectedLocalMs),
    ?assert(ActualLocalPhonePid =:= ExpectedLocalPhonePid),
    ?assert(ActualLocalFSMLastCall =:= ExpectedLocalFSMLastCall),
    ?assert(ActualRemoteMs =:= ExpectedRemoteMs),
    ?assert(ActualRemoteFSMPid =:= ExpectedRemoteFSMPid).
