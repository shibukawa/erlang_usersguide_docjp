.. 3 Gen_Fsm Behaviour

.. _gen_fsm:

=================
gen_fsmビヘイビア
=================

.. This chapter should be read in conjunction with gen_fsm(3), where all 
   interface functions and callback functions are described in detail.

この章は、すべてのインタフェース関数、コールバック関数が詳細に説明されている、gen_fsm(3)と合わせて読んでください。

.. 3.1 Finite State Machines

有限ステートマシン
==================

.. A finite state machine, FSM, can be described as a set of relations of the form:

有限ステートマシン(FSM: Finite State Machine, 有限状態機械)は次の形式で示される関係の集合として表すことができます。

.. State(S) x Event(E) -> Actions(A), State(S')

.. code-block:: none

   ステート(S) x イベント(E) -> アクション(A), 状態(S')

.. These relations are interpreted as meaning:

これらの関係は次のように解釈することができます

.. If we are in state S and the event E occurs, we should perform the 
   actions A and make a transition to the state S'.

.. code-block:: none

   もしも、ステートSの時に、イベントEが起きると、アクションAが実行され、
   次のステートS'に移行する。

.. For an FSM implemented using the gen_fsm behaviour, the state transition 
   rules are written as a number of Erlang functions which conform to 
   the following convention:

FSMの実装にはgen_fsmビヘイビアを使い、状態のトランジションのルールは次のような形式のErlangの関数として実装します。

.. StateName(Event, StateData) ->
       .. code for actions here ...
       {next_state, StateName', StateData'}

.. code-block:: erlang

   ステート名(イベント, 状態データ) ->
      .. アクションのコードをここに書く ...
      {次のステート, ステート名', ステートデータ'}

.. 3.2 Example

サンプル
========

.. A door with a code lock could be viewed as an FSM. Initially, the door 
   is locked. Anytime someone presses a button, this generates an event. 
   Depending on what buttons have been pressed before, the sequence so far 
   may be correct, incomplete or wrong.

FSMのサンプルとして、コードによってロックがかけられるドアについて見ていきます。初期状態では、ドアはロックされています。誰かがボタンを押すと、イベントが発生します。どのボタンが以前に押されたのかによって、操作シーケンスが、正しい、まだ完了していない、間違い、というどれかの状態になります。

.. If it is correct, the door is unlocked for 30 seconds (30000 ms). If it 
   is incomplete, we wait for another button to be pressed. If it is is wrong, 
   we start all over, waiting for a new button sequence.

正しい操作が行われたら、ドアは30秒間(30,000ミリ秒)だけ解錠されます。もし、まだ完了していない場合には、次のボタン操作を待ちます。もし間違ってしまうと、スタートに戻り、新しいボタンの操作のシーケンスを待ちます。

.. Implementing the code lock FSM using gen_fsm results in this callback module:

コードロックFSMは、gen_fsmを用いてコールバックモジュールとして実装します。

.. code-block:: erlang

   -module(code_lock).
   -behaviour(gen_fsm).

   -export([start_link/1]).
   -export([button/1]).
   -export([init/1, locked/2, open/2]).

   start_link(Code) ->
       gen_fsm:start_link({local, code_lock}, code_lock, Code, []).

   button(Digit) ->
       gen_fsm:send_event(code_lock, {button, Digit}).

   init(Code) ->
       {ok, locked, {[], Code}}.

   locked({button, Digit}, {SoFar, Code}) ->
       case [Digit|SoFar] of
           Code ->
               do_unlock(),
               {next_state, open, {[], Code}, 3000};
           Incomplete when length(Incomplete)<length(Code) ->
               {next_state, locked, {Incomplete, Code}};
           _Wrong ->
               {next_state, locked, {[], Code}}
       end.

   open(timeout, State) ->
       do_lock(),
       {next_state, locked, State}.

.. The code is explained in the next sections.

コードの解説は次のセクションで行います。

.. 3.3 Starting a Gen_Fsm

Gen_Fsmをはじめよう
===================

.. In the example in the previous section, the gen_fsm is started 
   by calling code_lock:start_link(Code):

前のセクションのサンプルでは、まず、 ``code_lock:start_link(Code)`` が呼ばれるところから、 ``gen_fsm`` の動作がスタートします。

.. code-block:: erlang

   start_link(Code) ->
       gen_fsm:start_link({local, code_lock}, code_lock, Code, []).

.. start_link calls the function gen_fsm:start_link/4. This function spawns 
   and links to a new process, a gen_fsm.

``start_link`` は ``gen_fsm:start_link/4`` を呼び出します。この関数はプロセスをspawnして、 ``gen_fsm`` と新しいプロセスを結びつけます。

.. * The first argument {local, code_lock} specifies the name. In this case, 
     the gen_fsm will be locally registered as code_lock.

     If the name is omitted, the gen_fsm is not registered. Instead its pid must 
     be used. The name could also be given as {global, Name}, in which case the 
     gen_fsm is registered using global:register_name/2.

* 最初の引数の ``{local, code_lock}`` は名前を設定します。この場合、ローカルで ``code_lock`` という名前で ``gen_fsm`` が登録されます。

  名前が省略されると、この ``gen_fsm`` は登録されません。代わりに、pidが使用されます。名前としては、 ``{global, 名前}`` という指定もできます。この場合、 ``gen_fsm`` は ``global:register_name/2`` を使って登録されます。

.. * The second argument, code_lock, is the name of the callback module, that is 
     the module where the callback functions are located.

     In this case, the interface functions (start_link and button) are located 
     in the same module as the callback functions (init, locked and open). This 
     is normally good programming practice, to have the code corresponding to one 
     process contained in one module.

* 2つ目の引数の ``code_lock`` は、コールバックモジュールの名前を表します。このモジュールは、コールバック関数が置かれているモジュールになります。

  この場合、インタフェース関数(``start_link`` と ``button``)は、コールバック関数(``init``, ``locked``, ``open``)同じモジュール内に置かれています。これは通常は良いプログラミングのプラクティスです。一つのプレセスに関連するコードは一つのモジュールにすべきです。

.. * The third argument, Code, is a term which is passed as-is to the callback 
     function init. Here, init gets the correct code for the lock as indata.

* 三番目の引数の ``Code`` はコールバック関数の ``init`` にそのまま渡されます。このサンプルの場合には、 ``init`` はロックのための暗証番号を受け取ります。

.. * The fourth argument, [], is a list of options. See gen_fsm(3) for available options.

* 4番目の引数の ``[]`` は、オプションのリストです。利用できるオプションは ``gen_fsm(3)`` を参照してください。

.. If name registration succeeds, the new gen_fsm process calls the callback function
   code_lock:init(Code). This function is expected to return {ok, StateName, StateData}, 
   where StateName is the name of the initial state of the gen_fsm. In this case locked,
   assuming the door is locked to begin with. StateData is the internal state of the 
   gen_fsm. (For gen_fsms, the internal state is often referred to 'state data' to 
   distinguish it from the state as in states of a state machine.) In this case, the 
   state data is the button sequence so far (empty to begin with) and the correct 
   code of the lock.

もし名前の登録が成功すると、新しいgen_fsmプロセスは、コールバック関数として、 ``code_lock:init(Code)`` という呼び出しを行います。この関数は ``{ok, ステート名, ステートデータ}`` という値を返すことが期待されています。 ``ステート名`` はgen_fsmの初期のステートの名前です。この場合は、ドアは最初は鍵がかかっているという想定で、 ``locked`` になっています。 ``ステートデータ`` は、gen_fsmの内部のステートです。gen_fsmの場合、ステートマシンのステートと区別するために、内部のステートは「ステートデータ」と呼びます。この場合、ステートデータには、今まで押されたボタンの順序(最初は空)と、現在の暗証番号を含んでいます。

.. code-block:: erlang

   init(Code) ->
       {ok, locked, {[], Code}}.

.. Note that gen_fsm:start_link is synchronous. It does not return until the gen_fsm 
   has been initialized and is ready to receive notifications.

``gen_fsm:start_link`` は同期実行されます。この関数はgen_fsmの初期化が完了し、通知を受け取る準備ができるまでは返りません。

.. gen_fsm:start_link must be used if the gen_fsm is part of a supervision tree, i.e. 
   is started by a supervisor. There is another function gen_fsm:start to start a 
   stand-alone gen_fsm, i.e. a gen_fsm which is not part of a supervision tree.

``gen_fsm:start_link`` は、スーパーバイザによって起動され、gen_fsmをスーパービジョンツリーの一部として使う場合にのみ使用してください。スーパービジョンツリーとは独立し、スタンドアローンのgen_fsmとして使う場合には、 ``gen_fsm:start`` という別の関数があります。

3.4 Notifying About Events

イベントの通知
==============

.. The function notifying the code lock about a button event is implemented using
   gen_fsm:send_event/2:

   関数が暗証番号をボタンイベントとして通知するというコードは、 ``gen_fsm:send_event/2`` を使って実装することができます。

.. code-block:: erlang
  
   button(Digit) ->
       gen_fsm:send_event(code_lock, {button, Digit}).

.. code_lock is the name of the gen_fsm and must agree with the name used to start it. 
   {button, Digit} is the actual event.

``code_lock`` はgen_fmsの名前で、その名前を使って開始されるということに同意する必要があります。

.. The event is made into a message and sent to the gen_fsm. When the event is 
   received, the gen_fsm calls StateName(Event, StateData) which is expected to 
   return a tuple {next_state, StateName1, StateData1}. StateName is the name of 
   the current state and StateName1 is the name of the next state to go to. 
   StateData1 is a new value for the state data of the gen_fsm.

イベントは、メッセージとして処理されて、gen_fsmに送られます。イベントを受信すると、gen_fsmは ``ステート名(イベント, ステートデータ)`` という名前で関数を呼び出します。この関数は、 ``{next_state, ステート名1, ステートデータ1}`` というタプルを返さなければなりません。この説明例の中の「ステート名」は現在のステート、「ステート名1」は次に進むステートの名前です。「ステートデータ1」はgen_fsmが持つ、新しいステートデータです。

.. code-block:: erlang

   locked({button, Digit}, {SoFar, Code}) ->
       case [Digit|SoFar] of
           Code ->
               do_unlock(),
               {next_state, open, {[], Code}, 30000};
           Incomplete when length(Incomplete)<length(Code) ->
               {next_state, locked, {Incomplete, Code}};
           _Wrong ->
               {next_state, locked, {[], Code}};
       end.

   open(timeout, State) ->
       do_lock(),
       {next_state, locked, State}.

.. If the door is locked and a button is pressed, the complete button 
   sequence so far is compared with the correct code for the lock and, 
   depending on the result, the door is either unlocked and the gen_fsm 
   goes to state open, or the door remains in state locked.

ドアがロックされていて、ボタンが押されると、今までの押されたボタンのシーケンスと、正しい解除コードを比較します。結果次第で、鍵を解除してgen_fsmが ``open`` というステートに移動したり、 ``locked`` のままのステートに居続けます。

.. 3.5 Timeouts

タイムアウト
============

.. When a correct code has been givened, the door is unlocked and the 
   following tuple is returned from locked/2:

もし、ただしいコードが与えられている時に、鍵が解除されたときに、 ``locked/2`` は次のようなタプルを返しています。

.. code-block:: erlang

   {next_state, open, {[], Code}, 30000};

.. 30000 is a timeout value in milliseconds. After 30000 ms, i.e. 30 seconds,
   a timeout occurs. Then StateName(timeout, StateData) is called. In this case,
   the timeout occurs when the door has been in state open for 30 seconds. After
   that the door is locked again:

30000はミリ秒単位の、タイムアウト時間を表しています。30000ミリ秒、つまり30秒経つと、タイムアウトが発生し、 ``ステート名(timeout, ステートデータ)`` が呼ばれます。この場合、ドアの状態は30秒間だけ ``open`` になり、その後タイムアウトが発生します。その後ドアは再び施錠されます。

.. code-block:: erlang

   open(timeout, State) ->
       do_lock(),
       {next_state, locked, State}.

.. 3.6 All State Events

すべてのステートのイベント
==========================

.. Sometimes an event can arrive at any state of the gen_fsm. Instead of sending the 
   message with gen_fsm:send_event/2 and writing one clause handling the event for 
   each state function, the message can be sent with gen_fsm:send_all_state_event/2 
   and handled with Module:handle_event/3:

イベントは、gen_fsmのあらゆるステート時に送ることができます。 ``gen_fsm:send_event/2`` を使い、ステート関数をそれぞれ作ってイベントを取り扱う方法もありますし、 ``gen_fsm:send_all_state_envet/2`` を使って送信し、 ``モジュール:handle_event/3`` を作ってイベントを取り扱うこともできます。

.. code-block:: erlang

   -module(code_lock).
   ...
   -export([stop/0]).
   ...

   stop() ->
       gen_fsm:send_all_state_event(code_lock, stop).

   ...

   handle_event(stop, _StateName, StateData) ->
       {stop, normal, StateData}.

.. 3.7 Stopping

停止
====

.. 3.7.1 In a Supervision Tree

監視ツリー内
------------

.. If the gen_fsm is part of a supervision tree, no stop function is needed. 
   The gen_fsm will automatically be terminated by its supervisor. Exactly 
   how this is done is defined by a shutdown strategy set in the supervisor.

もし、gen_fsmを監視ツリーの中で動かすのであれば、終了関数は不要です。監視ツリーが自動的にgen_fsmを終了させます。正確には、スーパバイザの :ref:`shutdown_strategy <シャットダウン戦略集>` を定義することで作業が完了します。

.. If it is necessary to clean up before termination, the shutdown strategy must 
   be a timeout value and the gen_fsm must be set to trap exit signals in the 
   init function. When ordered to shutdown, the gen_fsm will then call the 
   callback function terminate(shutdown, StateName, StateData):

終了前に片付けが必要であれば、シャットダウン戦略にタイムアウト値を設定し、 ``init`` 関数の中で、gen_fsmの終了シグナルを捕まえる設定をしなければなりません。シャットダウンの指令が来ると、 ``gen_ｆsm`` は ``terminal(shutdown, ステート名、ステートデータ)`` という形式で、コールバック関数を呼び出します。

.. code-block:: erlang

   init(Args) ->
       ...,
       process_flag(trap_exit, true),
       ...,
       {ok, StateName, StateData}.

   ...

   terminate(shutdown, StateName, StateData) ->
       ..片付けコードをここに書く..
       ok.

.. 3.7.2 Stand-Alone Gen_Fsms

スタンドアローンのgen_fsm
-------------------------

.. If the gen_fsm is not part of a supervision tree, a stop function may be useful, for example:

gen_fsmが監視ツリーの一部でない場合には、 ``stop`` 関数が便利です。次のコードがサンプルになります。

.. code-block:: erlang

   ...
   -export([stop/0]).
   ...

   stop() ->
       gen_fsm:send_all_state_event(code_lock, stop).
   ...

   handle_event(stop, _StateName, StateData) ->
       {stop, normal, StateData}.

   ...

   terminate(normal, _StateName, _StateData) ->
       ok.

.. The callback function handling the stop event returns a tuple 
   {stop, normal, StateData1}, where normal specifies that it is 
   a normal termination and StateData1 is a new value for the state 
   data of the gen_fsm. This will cause the gen_fsm to call 
   terminate(normal,StateName,StateData1) and then terminate gracefully:

このコールバック関数は終了イベント時に呼ばれ、 ``{stop, normal, ステートデータ}`` というタプルを返します。 ``normal`` は正常終了を表し、  ``ステートデータ`` はgen_fsmのステートデータです。これにより、gen_fsmは ``terminate(normal,StateName,StateData1)`` 優雅に終了します。

.. 3.8 Handling Other Messages

他のメッセージのハンドリング
============================

.. If the gen_fsm should be able to receive other messages than events, 
   the callback function handle_info(Info, StateName, StateData) must 
   be implemented to handle them. Examples of other messages are exit 
   messages, if the gen_fsm is linked to other processes (than the supervisor) 
   and trapping exit signals.

もしgen_fsmが、イベント以外の他のメッセージも受け取れるようにしたいと考えているのであれば、これを取り扱うために ``handle_info(情報、ステート名、ステートデータ)`` というコールバックファンクションを実装します。他のメッセージが「終了」で、gen_fsmがスーパバイザではなく、他のプロセスとリンクしている場合、終了イベントをトラップできます。

.. code-block:: erlang

   handle_info({'EXIT', Pid, Reason}, StateName, StateData) ->
       ..終了時のコード..
       {next_state, StateName1, StateData1}.

Copyright (c) 1991-2009 Ericsson AB
