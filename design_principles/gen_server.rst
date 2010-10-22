.. 2 Gen_Server Behaviour

====================
gen_serverビヘイビア
====================

.. This chapter should be read in conjunction with gen_server(3), where all 
   interface functions and callback functions are described in detail.

この章は、gen_server(3)をプログラムに組み込む場合は読むべきです。本章ではすべてのインタフェース関数、およびコールバック関数の詳細について説明しています。

.. 2.1 Client-Server Principles

クライアント・サーバの原則
==========================

.. The client-server model is characterized by a central server and an 
   arbitrary number of clients. The client-server model is generally 
   used for resource management operations, where several different 
   clients want to share a common resource. The server is responsible 
   for managing this resource.

クライアント・サーバモデルの特徴は、中央のサーバと、任意の数のクライアントで構成される、という点です。クライアント・サーバモデルが良く使われるのは、複数の異なるクライアントで共通のリソースを共有したい場面で、そのリソースの管理を行うという場面です。この場合、サーバはリソースの管理の責任を負います。

.. Client-Server Model

クライアントサーバモデル

.. 2.2 Example

サンプル
========

.. An example of a simple server written in plain Erlang was given in Overview. 
   The server can be re-implemented using gen_server, resulting in this callback module:

概要のページでは、純粋なErlangのみで実装したシンプルなサーバのサンプルを出しましたが、ここではそれをgen_serverを利用して書き換えたものを提示します。以下のコードがコールバックモジュールになります:

.. code-block:: erlang

  -module(ch3).
  -behaviour(gen_server).

  -export([start_link/0]).
  -export([alloc/0, free/1]).
  -export([init/1, handle_call/3, handle_cast/2]).

  start_link() ->
      gen_server:start_link({local, ch3}, ch3, [], []).

  alloc() ->
      gen_server:call(ch3, alloc).

  free(Ch) ->
      gen_server:cast(ch3, {free, Ch}).

  init(_Args) ->
      {ok, channels()}.

  handle_call(alloc, _From, Chs) ->
      {Ch, Chs2} = alloc(Chs),
      {reply, Ch, Chs2}.

  handle_cast({free, Ch}, Chs) ->
      Chs2 = free(Ch, Chs),
      {noreply, Chs2}.

.. The code is explained in the next sections.

このコードについては、これから先のセクションで説明していきます。


.. 2.3 Starting a Gen_Server

Gen_Serverの起動
================

.. In the example in the previous section, the gen_server is started by calling 
   ch3:start_link():

上で挙げたサンプルでは、ch3:start_link()を呼ぶことによってgen_serverを起動しています:

.. code-block:: erlang

  start_link() ->
      gen_server:start_link({local, ch3}, ch3, [], []) => {ok, Pid}

.. start_link calls the function gen_server:start_link/4. This function spawns and 
   links to a new process, a gen_server.

``start_link`` 関数は、中で :func:`gen_server:start_link/4` を呼んでいます。この関数は、新しいプロセスを作成して、gen_serverに結び付けます。

.. * The first argument {local, ch3} specifies the name. In this case, the gen_server 
     will be locally registered as ch3.

     If the name is omitted, the gen_server is not registered. Instead its pid must 
     be used. The name could also be given as {global, Name}, in which case the 
     gen_server is registered using global:register_name/2.

* 最初の引数の ``{local, ch3}`` では名前を設定しています。この場合は、 ``ch3`` という名前で、ローカルなgen_serverが登録されます。

  もしも名前が省略された場合には、gen_serverは登録されません。代わりにpidが使用されなければなりません。 ``{global, 名前}`` という形式でも名前を設定できます。この場合は ``register_name/2`` を使用してgen_serverが登録されます。

.. * The second argument, ch3, is the name of the callback module, that is the 
     module where the callback functions are located.

     In this case, the interface functions (start_link, alloc and free) are 
     located in the same module as the callback functions (init, handle_call 
     and handle_cast). This is normally good programming practice, to have 
     the code corresponding to one process contained in one module.

* ２つ目の引数の ``ch3`` はコールバックモジュールの名前です。コールバックモジュールには、コールバック関数を定義します。

  この場合は、インタフェース関数(start_link, alloc, free)はコールバック関数(init, handle_call, handle_cast)と同じモジュールに置かれています。この方法は良く使用されます。一つのプロセスに関連するコードを一つのモジュールにまとめる、良いプラクティスです。

.. * The third argument, [], is a term which is passed as-is to the callback 
     function init. Here, init does not need any in data and ignores the argument.

* ３つ目の引数の ``[]`` は、コールバック関数の ``init`` にそのまま渡される項です。ここでは ``init`` は引数を必要としておらず、無視されます。

.. * The fourth argument, [], is a list of options. See gen_server(3) for 
     available options.

* 4つ目の引数の ``[]`` はオプションのリストです。使用できる関数に関しては ``gen_server(3)`` を参照してください。

.. If name registration succeeds, the new gen_server process calls the 
   callback function ch3:init([]). init is expected to return {ok, State}, 
   where State is the internal state of the gen_server. In this case, the 
   state is the available channels.

名前の登録が成功した場合には、新しいgen_serverプロセスは、コールバック関数の ``ch3:init([])`` を呼びます。initは ``{ok, State}`` を返すことを期待されています。 ``State`` はgen_serverの内部状態を表します。この場合は、``State`` は利用可能なチャンネルになります。

.. code-block:: erlang

  init(_Args) ->
      {ok, channels()}.

.. Note that gen_server:start_link is synchronous. It does not return 
   until the gen_server has been initialized and is ready to receive requests.

gen_server:start_linkは同期実行されます。gen_serverが初期化されて、リクエストを受け取る用意ができるまでは関数から戻ることはありません。

.. gen_server:start_link must be used if the gen_server is part of a 
   supervision tree, i.e. is started by a supervisor. There is another 
   function gen_server:start to start a stand-alone gen_server, i.e. a 
   gen_server which is not part of a supervision tree.

もしもgen_serverが、スーパバイザとして実行されるなど、管理ツリーの一部として使用される場合には、 :func:`gen_server:start_link` を使用すべきです。管理ツリーの一部としては実行されないで、スタンドアローンのgen_serverとして実行される場合には、もう一つの関数の :func:`gen_server:start` を使用してください。

.. 2.4 Synchronous Requests - Call

同期リクエスト - Call
=====================

.. The synchronous request alloc() is implemented using gen_server:call/2:

同期リクエストの alloc() は、 :func:`gen_server:call/2` を使用して実装されています。

alloc() ->
    gen_server:call(ch3, alloc).

.. ch3 is the name of the gen_server and must agree with the name used to start 
   it. alloc is the actual request.

``ch3``\ はgen_serverの名前で、スタート時に使用した名前と一致する必要があります。 ``alloc`` が実際のリクエストになります。

.. The request is made into a message and sent to the gen_server. When the 
   request is received, the gen_server calls handle_call(Request, From, State) 
   which is expected to return a tuple {reply, Reply, State1}. Reply is the 
   reply which should be sent back to the client, and State1 is a new value 
   for the state of the gen_server.

このリクエストからメッセージが作成されて、gen_serverに送信されます。リクエストを受信すると、gen_serverは :func:`handle_call(Request, From, State)` を呼び出します。この関数は ``{reply, Reply, State1}`` というタプルを返すことが期待されています。 ``Reply`` はクライアントに送信し返す返事を表します。 ``State1`` はget_serverの状態を表す新しい値になります。

.. code-block:: erlang

  handle_call(alloc, _From, Chs) ->
      {Ch, Chs2} = alloc(Chs),
      {reply, Ch, Chs2}.

.. In this case, the reply is the allocated channel Ch and the new state is 
   the set of remaining available channels Chs2.

このコードの場合は、 ``Ch`` という、割り当てられたチャンネルを返し、取得可能なチャンネルの残りを現す ``Chs2`` の集合が新しい状態になります。

.. Thus, the call ch3:alloc() returns the allocated channel Ch and the 
   gen_server then waits for new requests, now with an updated list of available channels.

これにより、上記の場合、 ``ch3:alloc()`` は割り当て済みのチャンネル ``Ch`` を返し、gen_serverは新しいリクエストを待ちます。また、取得可能なチャンネルのリストが更新されます。

.. 2.5 Asynchronous Requests - Cast

非同期リクエスト - Cast
=======================

.. The asynchronous request free(Ch) is implemented using gen_server:cast/2:

非同期のリクエストである ``free(Ch)`` は、 :func:`gen_server:cast/2` を利用して実装されています:

.. code-block:: erlang

  free(Ch) ->
      gen_server:cast(ch3, {free, Ch}).

.. ch3 is the name of the gen_server. {free, Ch} is the actual request.

``ch3`` はgen_serverの名前になります。 ``{free, Ch}`` というのが実際のリクエストになります。

.. The request is made into a message and sent to the gen_server. cast, and thus free, then returns ok.

このリクエストからメッセージが作成されて、gen_serverに送信されます。 ``cast`` と、当然のことながら ``free`` の両方の関数は ``ok`` を返します。

.. When the request is received, the gen_server calls handle_cast(Request, 
   State) which is expected to return a tuple {noreply, State1}. State1 is 
   a new value for the state of the gen_server.

リクエストを受信すると、gen_serverは :func:`handle_cast(Request, State)` を呼び出します。この関数は ``{noreply, State1}`` というタプルを返すことを期待されています。 ``State1`` はgen_serverの新しい状態値になります。

.. code-block:: erlang

  handle_cast({free, Ch}, Chs) ->
      Chs2 = free(Ch, Chs),
      {noreply, Chs2}.

.. In this case, the new state is the updated list of available channels Chs2. 
   The gen_server is now ready for new requests.

この場合、取得可能な更新されたチャンネルのリスト ``Chs2`` が新しい状態となります。今、gen_serverは新しいリクエストを受け取る準備が整いました。

.. 2.6 Stopping

停止
====

.. 2.6.1 In a Supervision Tree

管理ツリー内での停止
--------------------

.. If the gen_server is part of a supervision tree, no stop function is needed. 
   The gen_server will automatically be terminated by its supervisor. Exactly 
   how this is done is defined by a shutdown strategy set in the supervisor.

もしもgen_serverが監視ツリーの一部となっている場合には、stop関数を作る必要はありません。gen_serverは監視ツリーによって、自動的に停止させられます。正確には、スーパバイザの中に、シャットダウン戦略集を定義する必要があります。

.. If it is necessary to clean up before termination, the shutdown strategy 
   must be a timeout value and the gen_server must be set to trap exit 
   signals in the init function. When ordered to shutdown, the gen_server 
   will then call the callback function terminate(shutdown, State):

もしも終了の前に色々片づけを行う必要がある場合には、シャットダウン戦略にタイムアウト値を設定し、gen_serverがinit()関数の中で終了シグナルを捕まえるようにしなければなりません。終了の命令があったときに、gen_serverは ``terminal(shutdown, State)`` という終了関数を呼び出します。

.. code-block:: erlang

  init(Args) ->
      ...,
      process_flag(trap_exit, true),
      ...,
      {ok, State}.

  ...

  terminate(shutdown, State) ->
      ..code for cleaning up here..
      ok.

.. 2.6.2 Stand-Alone Gen_Servers

スタンドアローンのgen_serverの場合
----------------------------------

.. If the gen_server is not part of a supervision tree, a stop function may 
   be useful, for example:

もしもgen_serverが監視ツリーの一部でなかった場合には、 ``stop`` 関数が便利でしょう。

.. code-block:: erlang

  ...
  export([stop/0]).
  ...

  stop() ->
      gen_server:cast(ch3, stop).
  ...

  handle_cast(stop, State) ->
      {stop, normal, State};
  handle_cast({free, Ch}, State) ->
      ....

  ...

  terminate(normal, State) ->
      ok.

.. The callback function handling the stop request returns a tuple {stop, 
   normal, State1}, where normal specifies that it is a normal termination 
   and State1 is a new value for the state of the gen_server. This will 
   cause the gen_server to call terminate(normal,State1) and then terminate gracefully.

stopリクエストを扱うコールバック関数は、 ``{stop, normal, State1}`` というタプルを返します。 ``normal`` は通常終了、 ``State1`` はgen_serverの状態を表す新しい値です。これにより、 gen_serverは ``terminate(normal, State1)`` を呼び出し、奥ゆかしく終了させます。

.. 2.7 Handling Other Messages

他のメッセージのハンドリング
============================

.. If the gen_server should be able to receive other messages than 
   requests, the callback function handle_info(Info, State) must be 
   implemented to handle them. Examples of other messages are exit 
   messages, if the gen_server is linked to other processes (than the 
   supervisor) and trapping exit signals.

もしもgen_serverが、リクエストではなくて他のメッセージを受信できるようにする場合には、 :func:`handle_info(Info, State)` というコールバック関数を実装する必要があります。他のメッセージの例としては、終了などがありますが、gen_serverが他のプロセス(スーパバイザではなく)とリンクしていて、そこから終了のシグナルを受け取る、という場面が想定されます:

.. code-block:: erlang

  handle_info({'EXIT', Pid, Reason}, State) ->
      .. 終了フラグを取り扱うコードをここに書きます ..
      {noreply, State1}.

.. 
   ..code to handle exits here..

Copyright (c) 1991-2009 Ericsson AB
