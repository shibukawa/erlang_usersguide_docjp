.. 6 Sys and Proc_Lib

.. _sys_and_proc_lib:

=============
sysとproc_lib
=============

.. The module sys contains functions for simple debugging of processes 
   implemented using behaviours.

sysモジュールには、ビヘイビアを使って実装された、シンプルなデバッグ用プロセスのための関数が定義されています。

.. There are also functions that, together with functions in the module proc_lib, 
   can be used to implement a special process, a process which comply to the 
   OTP design principles without making use of a standard behaviour. They can also 
   be used to implement user defined (non-standard) behaviours.

これ以外にも、一緒に使用できる関数がproc_libモジュールに定義されており、これを使って **特別なプロセス** や、標準のビヘイビアを利用しないがOTPの設計原則に即したプロセスなどを実装することができます。これらを使って、非標準の、ユーザ定義のビヘイビアを実装することもできます。

.. Both sys and proc_lib belong to the STDLIB application.

sysとproc_libの両方共、標準ライブラリのapplicationに属しています。

.. 6.1 Simple Debugging

シンプルなデバッグ
==================

.. The module sys contains some functions for simple debugging of processes 
   implemented using behaviours. We use the code_lock example from the 
   gen_event chapter to illustrate this:

sysモジュールには、ビヘイビアを利用して実装したプロセスのデバッグを簡単に行うための関数がいくつか定義されています。 :ref:`get_event` の章で説明したcode_lockの例を使って紹介します。

.. code-block:: erlang

   % erl
   Erlang (BEAM) emulator version 5.2.3.6 [hipe] [threads:0]

   Eshell V5.2.3.6  (abort with ^G)
   1> code_lock:start_link([1,2,3,4]).
   {ok,<0.32.0>}
   2> sys:statistics(code_lock, true).
   ok
   3> sys:trace(code_lock, true).
   ok
   4> code_lock:button(4).
   *DBG* code_lock got event {button,4} in state closed
   ok
   *DBG* code_lock switched to state closed
   5> code_lock:button(3).
   *DBG* code_lock got event {button,3} in state closed 
   ok
   *DBG* code_lock switched to state closed
   6> code_lock:button(2).
   *DBG* code_lock got event {button,2} in state closed
   ok
   *DBG* code_lock switched to state closed
   7> code_lock:button(1).
   *DBG* code_lock got event {button,1} in state closed
   ok
   OPEN DOOR
   *DBG* code_lock switched to state open
   *DBG* code_lock got event timeout in state open
   CLOSE DOOR
   *DBG* code_lock switched to state closed
   8> sys:statistics(code_lock, get).
   {ok,[{start_time,{{2003,6,12},{14,11,40}}},
        {current_time,{{2003,6,12},{14,12,14}}},
        {reductions,333},
        {messages_in,5},
        {messages_out,0}]}
   9> sys:statistics(code_lock, false).
   ok
   10> sys:trace(code_lock, false).
   ok
   11> sys:get_status(code_lock).
   {status,<0.32.0>,
           {module,gen_fsm},
           [[{'$ancestors',[<0.30.0>]},
             {'$initial_call',{gen,init_it,
                                   [gen_fsm,<0.30.0>,<0.30.0>,
                                    {local,code_lock},
                                    code_lock,
                                    [1,2,3,4],
                                    []]}}],
            running,<0.30.0>,[],
            [code_lock,closed,{[],[1,2,3,4]},code_lock,infinity]]}

6.2 Special Processes

特別なプロセス
==============

.. This section describes how to write a process which comply to the 
   OTP design principles, without making use of a standard behaviour. 
   Such a process should:

このセクションでは、OTP設計原則に即したプロセスの実装方法を紹介します。

.. * be started in a way that makes the process fit into a supervision tree,

* このようなプロセスは、監視ツリー内で利用できるような方法でプロセスが起動できます。

.. * support the sysdebug facilities, and

* sys は :ref:`debug_facility` をサポートしています。

.. * take care of system messages.

* :ref:`system_message` に注意を払います。

.. System messages are messages with special meaning, used in the supervision 
   tree. Typical system messages are requests for trace output, and requests 
   to suspend or resume process execution (used during release handling). 
   Processes implemented using standard behaviours automatically understand 
   these messages.

システムメッセージは特別な意味を持つメッセージで、監視ツリーの内部で使用されます。システムメッセージの例としては、トレース情報の出力のリクエストや、プロセス実行の停止や再開などがあります。標準のビヘイビアを利用して実装されたプロセスは、自動でこれらのメッセージを解釈します。

.. 6.2.1 Example

サンプル
--------

.. The simple server from the Overview chapter, implemented using sys and 
   proc_lib so it fits into a supervision tree:

:ref:`overview` の章で、シンプルなサーバの実装例を紹介しましたが、ここではsysとproc_libを用いて、監視ツリーで使用できるように実装していきます。

.. code-block:: erlang

   -module(ch4).
   -export([start_link/0]).
   -export([alloc/0, free/1]).
   -export([init/1]).
   -export([system_continue/3, system_terminate/4,
            write_debug/3]).

   start_link() ->
       proc_lib:start_link(ch4, init, [self()]).

   alloc() ->
       ch4 ! {self(), alloc},
       receive
           {ch4, Res} ->
               Res
       end.

   free(Ch) ->
       ch4 ! {free, Ch},
       ok.

   init(Parent) ->
       register(ch4, self()),
       Chs = channels(),
       Deb = sys:debug_options([]),
       proc_lib:init_ack(Parent, {ok, self()}),
       loop(Chs, Parent, Deb).

   loop(Chs, Parent, Deb) ->
       receive
           {From, alloc} ->
               Deb2 = sys:handle_debug(Deb, {ch4, write_debug},
                                       ch4, {in, alloc, From}),
               {Ch, Chs2} = alloc(Chs),
               From ! {ch4, Ch},
               Deb3 = sys:handle_debug(Deb2, {ch4, write_debug},
                                       ch4, {out, {ch4, Ch}, From}),
               loop(Chs2, Parent, Deb3);
           {free, Ch} ->
               Deb2 = sys:handle_debug(Deb, {ch4, write_debug},
                                       ch4, {in, {free, Ch}}),
               Chs2 = free(Ch, Chs),
               loop(Chs2, Parent, Deb2);

           {system, From, Request} ->
               sys:handle_system_msg(Request, From, Parent,
                                     ch4, Deb, Chs)
       end.

   system_continue(Parent, Deb, Chs) ->
       loop(Chs, Parent, Deb).

   system_terminate(Reason, Parent, Deb, Chs) ->
       exit(Reason).

   write_debug(Dev, Event, Name) ->
       io:format(Dev, "~p event = ~p~n", [Name, Event]).

.. Example on how the simple debugging functions in sys can be used for ch4 as well:

ch4内で使用されている、sysのシンプルなデバッグ関数は次のように使用します。

.. code-block:: erlang

   % erl
   Erlang (BEAM) emulator version 5.2.3.6 [hipe] [threads:0]

   Eshell V5.2.3.6  (abort with ^G)
   1> ch4:start_link().
   {ok,<0.30.0>}
   2> sys:statistics(ch4, true).  
   ok
   3> sys:trace(ch4, true).
   ok
   4> ch4:alloc().
   ch4 event = {in,alloc,<0.25.0>}
   ch4 event = {out,{ch4,ch1},<0.25.0>}
   ch1
   5> ch4:free(ch1).
   ch4 event = {in,{free,ch1}}
   ok
   6> sys:statistics(ch4, get).
   {ok,[{start_time,{{2003,6,13},{9,47,5}}},
        {current_time,{{2003,6,13},{9,47,56}}},
        {reductions,109},
        {messages_in,2},
        {messages_out,1}]}
   7> sys:statistics(ch4, false).
   ok
   8> sys:trace(ch4, false).
   ok
   9> sys:get_status(ch4).
   {status,<0.30.0>,
           {module,ch4},
           [[{'$ancestors',[<0.25.0>]},{'$initial_call',{ch4,init,[<0.25.0>]}}],
            running,<0.25.0>,[],
            [ch1,ch2,ch3]]}

.. 6.2.2 Starting the Process

プロセスのスタート
------------------

.. A function in the proc_lib module should be used to start the process. There are 
   several possible functions, for example spawn_link/3,4 for asynchronous start and 
   start_link/3,4,5 for synchronous start.

proc_libモジュール内の関数は、プロセスを起動されるのに使用するべきです。利用可能な関数がいｋつかあります。例えば、spawn_link/3,4は非同期の起動に、start_link/3,4,5は同期起動に使うことができます。

.. A process started using one of these functions will store information that is 
   needed for a process in a supervision tree, for example about the ancestors 
   and initial call.

上記の関数を使って起動したプロセスは、ancestorや、initial callなどの監視ツリー内のプロセスが必要とする情報を格納しています。

.. Also, if the process terminates with another reason than normal or shutdown, 
   a crash report (see SASL User's Guide) is generated.

また、プロセスが通常の理由以外で終了したり、シャットダウンした場合には、クラッシュレポート(SASLユーザガイド参照)が生成されます。

.. In the example, synchronous start is used. The process is started by calling ch4:start_link():

例えば、同期起動が使用されたとします。 ``ch4:start_link()`` を呼んでプロセスをスタートさせます。

.. code-block:: erlang

   start_link() ->
       proc_lib:start_link(ch4, init, [self()]).

.. ch4:start_link calls the function proc_lib:start_link. This function takes 
   a module name, a function name and an argument list as arguments and spawns 
   and links to a new process. The new process starts by executing the given 
   function, in this case ch4:init(Pid), where Pid is the pid (self()) of the 
   first process, that is the parent process.

``ch4:start_link`` は ``proc_lib:start_link`` 関数を呼び出します。この関数はモジュール名、関数名、引き数のリストをパラメータに取り、新しいプロセスを生成してリンクします。

.. In init, all initialization including name registration is done. 
   The new process must also acknowledge that it has been started to the parent:

``init`` の中では、名前の登録を含む、すべての初期化を完了させます。新しいプロセスは、親のプロセスに対して、起動したことを知らせなければなりません。

.. code-block:: erlang

   init(Parent) ->
       ...
       proc_lib:init_ack(Parent, {ok, self()}),
       loop(...).

.. proc_lib:start_link is synchronous and does not return until proc_lib:init_ack 
   has been called.

``proc_lib:start_link`` は同期実行されるため、 ``proc_lib:init_ack`` を呼び出すまではリターンしません。

.. 6.2.3 Debugging

.. _debug_facility:

デバッグ機能
------------

.. To support the debug facilites in sys, we need a debug structure, a term 
   Deb which is initialized using sys:debug_options/1:

sysモジュールデバッグ環境をサポートさせるには、 ``sys:debug_options/1`` を使用して、 ``Deb`` という項を初期化する必要があります。

.. code-block:: erlang

   init(Parent) ->
       ...
       Deb = sys:debug_options([]),
       ...
       loop(Chs, Parent, Deb).

.. sys:debug_options/1 takes a list of options as argument. Here the list is empty, 
   which means no debugging is enabled initially. See sys(3) for information about 
   possible options.

``sys:debug_options/1`` はリスト型のオプションを引数に取ります。ここでは空のリストを渡していますが、これは初期化の際には、デバッグ機能は利用しない、という意味です。使用できるオプションについては、sys(3)を参照してください。

.. Then for each system event that we want to be logged or traced, the following
   function should be called.

ログを取ったり、トレースしたいシステムイベントごとに、次の関数を呼び出す必要があります。

.. code-block:: erlang

   sys:handle_debug(Deb, Func, Info, Event) => Deb1

.. * Deb is the debug structure.

* Debはデバッグ構造体です。

.. * Func is a tuple {Module, Name} (or a fun) and should specify a (user defined) 
     function used to format trace output. For each system event, the format function 
     is called as Module:Name(Dev, Event, Info), where:

* ``Func`` は ``{Module, Name}`` (もしくはfun)のタプルで、手レース出力のフォーマットに使用される、ユーザ定義関数を指定します。システムイベントごとに、 ``Module:Name(Dev, Event, Info)`` という形式でフォーマット関数が呼ばれます。

   .. o Dev is the IO device to which the output should be printed. See io(3).

   * Devは出力が書き出されるIOデバイスです。詳しくはio(3)を参照してください。

   .. o Event and Info are passed as-is from handle_debug.

   * ``Event`` 、 ``Info`` はそのまま ``handle_debug`` に渡されます。

.. * Info is used to pass additional information to Func, it can be any term and is 
     passed as-is.

* ``Info`` は ``Func`` に追加の情報を渡すのに使用されます。これにはあらゆる項を設定することができ、そのまま渡されます。

.. * Event is the system event. It is up to the user to define what a system event 
     is and how it should be represented, but typically at least incoming and 
     outgoing messages are considered system events and represented by the tuples 
     {in,Msg[,From]} and {out,Msg,To}, respectively.

* ``Event`` はシステムイベントです。どんなシステムイベントで、どのように表現すべきかはユーザしだいですが、良く使用されるのは、最低限、メッセージ入力と出力はシステムイベントとして考えられいて、それぞれ、 ``{in,Msg[,From]}`` 、もしくは ``{out,Msg,To}`` という形式で表現されます。

.. handle_debug returns an updated debug structure Deb1.

``handle_debug`` は、更新されたデバッグ構造体の ``Deb1`` を返します。

.. In the example, handle_debug is called for each incoming and outgoing message. 
   The format function Func is the function ch4:write_debug/3 which prints the
   message using io:format/3.

次のサンプルでは、メッセージの入力と、出力のそれぞれに対して、 ``handle_debug`` を呼び出しています。フォーマット関数の ``Func`` としては、 ``io:format/3`` を利用して情報をプリントする ``ch4:write_debug/3`` が渡されています。

.. code-block:: erlang

   loop(Chs, Parent, Deb) ->
       receive
           {From, alloc} ->
               Deb2 = sys:handle_debug(Deb, {ch4, write_debug},
                                       ch4, {in, alloc, From}),
               {Ch, Chs2} = alloc(Chs),
               From ! {ch4, Ch},
               Deb3 = sys:handle_debug(Deb2, {ch4, write_debug},
                                       ch4, {out, {ch4, Ch}, From}),
               loop(Chs2, Parent, Deb3);
           {free, Ch} ->
               Deb2 = sys:handle_debug(Deb, {ch4, write_debug},
                                       ch4, {in, {free, Ch}}),
               Chs2 = free(Ch, Chs),
               loop(Chs2, Parent, Deb2);
           ...
       end.

   write_debug(Dev, Event, Name) ->
       io:format(Dev, "~p event = ~p~n", [Name, Event]).

.. 6.2.4 Handling System Messages

.. _system_message:

システムメッセージの操作
------------------------

.. System messages are received as:

システムメッセージは次のような形式で受信されます。

.. code-block:: erlang

   {system, From, Request}

.. The content and meaning of these messages do not need to be interpreted by 
   the process. Instead the following function should be called:

これらのメッセージの中身と意味はプロセスが解釈する必要はありません。その代わりに次の関数を呼び出します。

.. code-block:: erlang

   sys:handle_system_msg(Request, From, Parent, Module, Deb, State)

.. This function does not return. It will handle the system message and then call:

この関数はリターンしません。この関数はシステムメッセージを捕まえて、もしプロセスの実行を継続すべき場合には、次のように呼び出します。

.. code-block:: erlang

   Module:system_continue(Parent, Deb, State)

.. if process execution should continue, or:

また、もしプロセスを停止させるべき場合は次の関数を呼び出します。

.. code-block:: erlang

   Module:system_terminate(Reason, Parent, Deb, State)

.. if the process should terminate. Note that a process in a supervision tree is 
   expected to terminate with the same reason as its parent.

監視ツリー上のプロセスは、その親のプロセスと同じ理由で終了されることが期待されています。

.. * Request and From should be passed as-is from the system message to the 
     call to handle_system_msg.

* システムメッセージから送られてきた ``Request`` と ``From`` は、そのまま ``handle_system_msg`` の呼び出し時に渡さなければなりません。

.. * Parent is the pid of the parent.

* ``Parent`` は親のプロセスidです。

.. * Module is the name of the module.

* ``Module`` はモジュール名です。

.. * Deb is the debug structure.

* ``Deb`` はデバッグ構造体です。

.. * State is a term describing the internal state and is passed to 
     system_continue/system_terminate.

* ``State`` は内部ステートを表す項で、 ``system_continue`` / ``system_terminate`` に渡されます。

.. In the example

例:

.. code-block:: erlang

   loop(Chs, Parent, Deb) ->
       receive
           ...

           {system, From, Request} ->
               sys:handle_system_msg(Request, From, Parent,
                                     ch4, Deb, Chs)
       end.

   system_continue(Parent, Deb, Chs) ->
       loop(Chs, Parent, Deb).

   system_terminate(Reason, Parent, Deb, Chs) ->
       exit(Reason).

.. If the special process is set to trap exits, note that if the 
   parent process terminates, the expected behavior is to terminate 
   with the same reason:

もし、終了をトラップする特別なプロセスが設定されていて、親プロセスが終了すると、同じ理由で終了するのが期待される動作です。

.. code-block:: erlang

   init(...) ->
       ...,
       process_flag(trap_exit, true),
       ...,
       loop(...).

   loop(...) ->
       receive
           ...

           {'EXIT', Parent, Reason} ->
               ..maybe some cleaning up here..
               exit(Reason);
           ...
       end.

.. 6.3 User-Defined Behaviours

ユーザ定義のビヘイビア
======================

.. To implement a user-defined behaviour, write code similar to code 
   for a special process but calling functions in a callback module 
   for handling specific tasks.

ユーザ定義のビヘイビアを実装する場合は、特別なプロセスと同じようなコードを書いて、特別なタスクを処理するために、コールバックモジュール内の関数を呼ぶようにすればできます。

.. If it is desired that the compiler should warn for missing callback functions, 
   as it does for the OTP behaviours, implement and export the function:

もし、OTPのビヘイビアと同じように、コールバック関数の定義がされていないという警告を出したいのであれば、次の関数を定義して、エクスポートします。

.. code-block:: erlang

   behaviour_info(callbacks) ->
       [{Name1,Arity1},...,{NameN,ArityN}].

.. where each {Name,Arity} specifies the name and arity of a callback function.

``{Name,Arity}`` というタプルによって、コールバック関数の名前とアリティを定義します。

.. When the compiler encounters the module attribute -behaviour(Behaviour). in a module Mod, 
   it will call Behaviour:behaviour_info(callbacks) and compare the result with the set of 
   functions actually exported from Mod, and issue a warning if any callback function 
   is missing.

コンパイラが ``Mod`` モジュールの中で ``-behaviour(Behaviour).`` というモジュール属性を検知すると、 ``Behaviour:behaviour_info(callbacks)`` を呼び出し、その結果と ``Mod`` モジュールが実際にエクスポートしている関数を比較します。もし、見つからないコールバック関数があれば、警告を発します。

.. Example:

サンプル:

.. 
   %% User-defined behaviour module

.. code-block:: erlang

   %% ユーザ定義ビヘイビアモジュール
   -module(simple_server).
   -export([start_link/2,...]).
   -export([behaviour_info/1]).

   behaviour_info(callbacks) ->
       [{init,1},
        {handle_req,1},
        {terminate,0}].

   start_link(Name, Module) ->
       proc_lib:start_link(?MODULE, init, [self(), Name, Module]).

   init(Parent, Name, Module) ->
       register(Name, self()),
       ...,
       Dbg = sys:debug_options([]),
       proc_lib:init_ack(Parent, {ok, self()}),
       loop(Parent, Module, Deb, ...).

   ...

.. In a callback module:

コールバックモジュール:

.. code-block:: erlang

   -module(db).
   -behaviour(simple_server).

   -export([init/0, handle_req/1, terminate/0]).

   ...

Copyright (c) 1991-2009 Ericsson AB
