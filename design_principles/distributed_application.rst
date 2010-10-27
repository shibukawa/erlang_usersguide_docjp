[Ericsson AB]

.. 9 Distributed Applications

====================
分散アプリケーション
====================

.. 9.1 Definition

定義
====

.. In a distributed system with several Erlang nodes, there may be a need to 
   control applications in a distributed manner. If the node, where a certain 
   application is running, goes down, the application should be restarted at 
   another node.

複数のErlangのノードに分散したシステムにおいても、分散して動作しているアプリケーションを制御のできなければなりません。もし特定のアプリケーションがダウンした場合には、他のノードから再起動させてもらう必要があります。

.. Such an application is called a distributed application. Note that it is the 
   control of the application which is distributed, all applications can of 
   course be distributed in the sense that they, for example, use services on other nodes.

このようなアプリケーションは、 **分散アプリケーション** と呼ばれます。ここでは、分散したアプリケーションの制御について述べています。例えば、他のノードのサービスを利用するという意味では、すべてのアプリケーションが分散アプリケーションです。

.. Because a distributed application may move between nodes, some addressing mechanism 
   is required to ensure that it can be addressed by other applications, regardless 
   on which node it currently executes. This issue is not addressed here, but the
   Kernel module global or STDLIB module pg can be used for this purpose.

分散されたアプリケーションは、ノード間で移動する可能性があるため、現在実行されているノードがどれであろうと、他のアプリケーションでアドレッシングできるような仕組みが必要です。この問題はここでは触れませんが、この目的で、カーネルモジュールの ``global`` と、STDLIBモジュールの ``pg`` が使用できます。

.. 9.2 Specifying Distributed Applications

分散アプリケーションの記述
==========================

.. Distributed applications are controlled by both the application controller 
   and a distributed application controller process, dist_ac. Both these 
   processes are part of the kernel application. Therefore, distributed 
   applications are specified by configuring the kernel application, using 
   the following configuration parameter (see also kernel(6)):

分散アプリケーションは、アプリケーションコントローラと、分散アプリケーションコントローラの ``dist_ac`` の両方から制御されます。どちらのプロセスも、 ``kernel`` アプリケーションの一部です。そのため、分散アプリケーションの記述は、次の設定パラメータを使用して、 ``kerneall`` アプリケーションの設定を通じて行います。(kernel(6)参照)

.. code-block:: erlang

   distributed = [{Application, [Timeout,] NodeDesc}]

   .. Specifies where the application Application = atom() may execute. 
      NodeDesc = [Node |    {Node,...,Node}] is a list of node names in priority 
      order. The order between nodes in a tuple is undefined.

   ``Application = atom()`` には、実行されるアプリケーションを設定します。 ``NodeDesc = [Node | {Node,...,Node}]`` は、優先順位順のノード名のリストです。タプル内のノードの順序は未定義です。

   .. Timeout = integer() specifies how many milliseconds to wait before 
      restarting the application at another node. Defaults to 0.

   ``Timeout = integer()`` には、他のノードからアプリケーションを再起動させるまでに、何ミリ秒待つか、を設定します。デフォルトは0です。

.. For distribution of application control to work properly, the nodes where a 
   distributed application may run must contact each other and negotiate where
   to start the application. This is done using the following kernel configuration 
   parameters:

アプリケーションの分散が適切に動作しているのであれば、分散アプリケーションが実行されているノードはお互いに連絡をとりあって、どこでアプリケーションを実行するかのネゴシエーションを行います。次のカーネル設定パラメータを使用して、これの設定を行います。

``sync_nodes_mandatory = [Node]``

   .. Specifies which other nodes must be started (within the timeout 
      specified by sync_nodes_timeout. 

   立ち上がる必要があるノードはどれか(``sync_nodes_timeout`` で指定されたタイムアウト時間内に)を設定します。

``sync_nodes_optional = [Node]``

   .. Specifies which other nodes can be started (within the timeout 
      specified by sync_nodes_timeout. 

   立ち上げられてもよいノードはどれか(``sync_nodes_timeout`` で指定されたタイムアウト時間内に)を設定します。

``sync_nodes_timeout = integer() | infinity``

   .. Specifies how many milliseconds to wait for the other nodes to start. 

   他のノードが立ち上がるときにどのぐらい待つかの時間(ミリ秒)を設定します。

.. When started, the node will wait for all nodes specified by sync_nodes_mandatory 
   and sync_nodes_optional to come up. When all nodes have come up, or when all 
   mandatory nodes have come up and the time specified by sync_nodes_timeout has 
   elapsed, all applications will be started. If not all mandatory nodes have come up, 
   the node will terminate.

起動すると、ノードは ``sync_nodes_mandatory`` と ``sync_nodes_optional`` で指定されたすべてのノードが立ち上がってくるのを待ちます。すべてのノードが立ち上がり、 ``sync_nodes_timeout`` で指定された時間が経過すると、すべてのアプリケーションが起動します。もし、すべての義務的(mandatory)なノードが立ち上がっていない場合には、ノードは終了します。

.. Example: An application myapp should run at the node cp1@cave. If this node goes 
   down, myapp should be restarted at cp2@cave or cp3@cave. A system configuration 
   file cp1.config for cp1@cave could look like:

例えば、 ``cp1@cave`` というノードで ``myapp`` アプリケーションを起動しなければならないとシます。もしこのノードがダウンした場合には、 ``myapp`` は ``cp2@cave`` か、 ``cp3@cave`` で再起動しなければなりません。 ``cp1@cave`` のための設定ファイル ``cp1.config` は次のようになります。

.. code-block:: erlang

   [{kernel,
     [{distributed, [{myapp, 5000, [cp1@cave, {cp2@cave, cp3@cave}]}]},
      {sync_nodes_mandatory, [cp2@cave, cp3@cave]},
      {sync_nodes_timeout, 5000}
     ]
    }
   ].

.. The system configuration files for cp2@cave and cp3@cave are identical, except 
   for the list of mandatory nodes which should be [cp1@cave, cp3@cave] for 
   cp2@cave and [cp1@cave, cp2@cave] for cp3@cave.

``cp2@cave`` ノードと、 ``cp3@cave`` ノードの設定ファイルもほぼ同じですが、義務的な(mandatory)ノードがそれぞれ、 ``cp2@cave`` 向けが ``[cp1@cave, cp3@cave]`` 、 ``cp3@cave`` 向けが ``[cp1@cave, cp2@cave]`` になります。

.. note::

   .. All involved nodes must have the same value for distributed and sync_nodes_timeout, 
      or the behaviour of the system is undefined.

   すべての関連ノードの ``distributed`` と ``sync_nodes_timeout`` の値は同じにしてください。異なる値が設定されている場合の動作は未定義です。

.. 9.3 Starting and Stopping Distributed Applications

分散アプリケーションの起動と停止
================================

.. When all involved (mandatory) nodes have been started, the distributed 
   application can be started by calling application:start(Application) at 
   all of these nodes.

すべての(義務的な)ノードが起動したら、これらのすべてのノードで ``application:start(Application)`` を呼び出すと、分散アプリケーションを起動させることができます。

.. It is of course also possible to use a boot script (see Releases) which automatically 
   starts the application.

もちろん、ブートスクリプト(:ref:`releases` 参照)を使うと、自動でアプリケーションを起動させることができます。

.. The application will be started at the first node, specified by the distributed 
   configuration parameter, which is up and running. The application is started as 
   usual. That is, an application master is created and calls the application 
   callback function:

``distributed`` 設定パラメータので定義された最初のノードでアプリケーションが起動すると、実行が始まります。アプリケーションは通常と同じようにスタートします。アプリケーションマスターが作成され、アプリケーションのコールバック関数が呼ばれます。

.. code-block:: erlang

   Module:start(normal, StartArgs)

.. Example: Continuing the example from the previous section, the three nodes 
   are started, specifying the system configuration file:

例: 前のセクションの例の続きを説明します。システム設定ファイルで指定された3つのノードを起動します。

.. code-block:: bash

   > erl -sname cp1 -config cp1
   > erl -sname cp2 -config cp2
   > erl -sname cp3 -config cp3

.. When all nodes are up and running, myapp can be started. This is 
   achieved by calling application:start(myapp) at all three nodes. 
   It is then started at cp1, as shown in the figure below.

すべてのノードが起動されると、 ``myapp`` がスタートします。すべての3つのノードで ``application:start(myapp)`` を呼ばれます。 ``cp1`` で起動した状態が、次の図の通りです。

Application myapp - Situation 1

.. Similarly, the application must be stopped by calling application:stop(Application) 
   at all involved nodes.

.. 同様に、アプリケーションの動作を止める場合には、すべての関連するノードで ``application:stop(Application)`` を実行しなければなりません。

.. 9.4 Failover

フェイルオーバー
================

.. If the node where the application is running goes down, the application is 
   restarted (after the specified timeout) at the first node, specified by the 
   distributed configuration parameter, which is up and running. This is called 
   a failover.

アプリケーションを実行しているノードがダウンした場合には、指定された時間のタイムアウト後に、 ``distributed`` 設定パラメータで指定されたように、最初のノード上でアプリケーションが再起動します。これをフェイルオーバーと呼びます。

.. The application is started the normal way at the new node, that is, 
   by the application master calling:

アプリケーションが新しいノード上で正常に起動する場合には、アプリケーションマスターが次のように呼びます。

.. code-block:: erlang

   Module:start(normal, StartArgs)

.. Exception: If the application has the start_phases key defined (see Included 
   Applications), then the application is instead started by calling:

例外: もし、 ``start_phases`` キーが定義されたアプリケーション(:ref:`included_applications` 参照)の場合には、次のように呼ばれます。

.. code-block:: erlang

   Module:start({failover, Node}, StartArgs)

.. where Node is the terminated node.

この ``Node`` は終了したノードです。

.. Example: If cp1 goes down, the system checks which one of the other nodes, 
   cp2 or cp3, has the least number of running applications, but waits for 5 
   seconds for cp1 to restart. If cp1 does not restart and cp2 runs fewer 
   applications than cp3, then myapp is restarted on cp2.

例: もし ``cp1`` がダウンすると、システムは他のノード(``cp2`` 、 ``cp3``)のうち、起動しているアプリケーション数がもっとも少ないノードを調べます。ただし、 ``cp1`` が再起動するまで5秒間待ちます。もし ``cp1`` が再起動せず、 ``cp2`` で起動しているアプリケーションが ``cp3`` よりも少ない場合、 ``myapp`` は ``cp2`` 上で起動します。

dist2

Application myapp - Situation 2

.. Suppose now that cp2 goes down as well and does not restart within 5 seconds. 
   myapp is now restarted on cp3.

同じようにして ``cp2`` がダウンし、5秒間待っても再起動しなければ、 ``myapp`` は、 ``cp3`` 上で再起動します。

dist3

Application myapp - Situation 3

.. 9.5 Takeover

テイクオーバー
==============

.. If a node is started, which has higher priority according to distributed, 
   than the node where a distributed application is currently running, the 
   application will be restarted at the new node and stopped at the old node. 
   This is called a takeover.

もしノードが起動したときに、そのノードが、現在分散アプリケーションを実行しているノードよりも ``distributed`` 設定上で高い優先順位を持っていた場合に、新しいノードでアプリケーションを起動しなおして、古いノードのアプリケーションを止めることがあります。これをテイクオーバーと呼びます。

.. The application is started by the application master calling:

アプリケーションは次のように、アプリケーションマスターから呼ばれます。

.. code-block:: erlang

   Module:start({takeover, Node}, StartArgs)

.. where Node is the old node.

``Node`` は古いノードです。

.. Example: If myapp is running at cp3, and if cp2 now restarts, it will not 
   restart myapp, because the order between nodes cp2 and cp3 is undefined.

例: もし ``myapp`` が ``cp3`` で起動している時に、 ``cp2`` が再起動したとすると、 ``myapp`` の再起動は行われません。 ``cp2`` と、 ``cp3`` の間の優先順位が未定義だからです。

dist4
Application myapp - Situation 4

.. However, if cp1 restarts as well, the function application:takeover/2 moves 
   myapp to cp1, because cp1 has a higher priority than cp3 for this application. 
   In this case, Module:start({takeover, cp3@cave}, StartArgs) is executed at cp1 
   to start the application.

しかし、 ``cp1`` が再起動したとすると、このアプリケーションに関しては ``cp1`` の方が ``cp3`` よりも優先順位が高いため、 ``application:takeover/2`` 関数が ``myapp`` を ``cp1`` に移動します。この場合、アプリケーションを ``cp1`` で起動するために、 ``Module:start({takeover, cp3@cave}, StartArgs)`` が実行されます。

dist5
Application myapp - Situation 5
Copyright (c) 1991-2009 Ericsson AB
