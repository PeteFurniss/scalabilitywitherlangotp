# Example Code for Designing for Scalability with Erlang/OTP
Exercises and examples for Designing for Scalability with Erlang OTP by Francesco Cesarini &amp; Steve Vinoski (O'Reilly)
ISBN: 978-1-449-32073-7

== Chapter 6 Finite State Machines ==

The code in ch6/exercise implements a phone controller FSM and mobile phone as per the APIs defined in chapter 6 of Designing for Scalability with Erlang/OTP.

A suite of eunit tests are also provided.

Example usage:

 <nowiki>
Eshell V8.3  (abort with ^G)

1> c(hlr).  
{ok,hlr}

2> c(phone).
{ok,phone}

3> c(phone_fsm).
{ok,phone_fsm}

4> c(phone_fsm_test).
{ok,phone_fsm_test}

5> phone_fsm_test:test().
  All 26 tests passed.
ok

6> hlr:new().
ok

7> {ok,P123}=phone:start_link("123").
<0.210.0>: "123": starting phone FSM
<0.210.0>: "123": phone FSM started
<0.210.0>: "123": phone connected to phone FSM
{ok,<0.210.0>}

8> {ok,P124}=phone:start_link("124").
<0.213.0>: "124": starting phone FSM
<0.213.0>: "124": phone FSM started
<0.213.0>: "124": phone connected to phone FSM
{ok,<0.213.0>}

9> {ok,P125}=phone:start_link("125").
<0.216.0>: "125": starting phone FSM
<0.216.0>: "125": phone FSM started
<0.216.0>: "125": phone connected to phone FSM
{ok,<0.216.0>}

10> phone:action(P123, {call, "124"}).
<0.210.0>: "123": action: outbound call to 124
<0.213.0>: "124": received inbound call from: 123
ok

11> phone:action(P124, accept).       
<0.213.0>: "124": action: accept
<0.210.0>: "123": received reply: accept
ok

12> phone:action(P125, {call, "123"}).
<0.216.0>: "125": action: outbound call to 123
ok
<0.216.0>: "125": received reply: busy

13> phone:action(P125, {call, "124"}).
<0.216.0>: "125": action: outbound call to 124
ok
<0.216.0>: "125": received reply: busy

14> phone:action(P125, {call, "125"}).
<0.216.0>: "125": action: outbound call to 125
ok
<0.216.0>: "125": received reply: busy

15> phone:action(P124, hangup).
<0.213.0>: "124": action: hangup
<0.210.0>: "123": received reply: hangup
ok
</nowiki>


This implementation differs slightly from that described in the book:
1. The FSM processes do not need to be started explicitly; they are started by the phone processes when they are started.
2. The console messages differ slightly as both actions and replies are printed.
3. Additional functions have been added to the API for phone and phone_fsm to facilitate the eunit tests.
