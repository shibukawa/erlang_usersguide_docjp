.. 12 Appup Cookbook

.. _appup_cookbook:

=================
appupクックブック
=================

.. This chapter contains examples of .appup files for typical cases of 
   upgrades/downgrades done in run-time.

この章は、実行時のアップグレード、ダウングレードを行う、よくある様々なケースでの ``.appup`` ファイルのサンプルを紹介します。

.. 12.1 Changing a Functional Module

機能性モジュールの変更
======================

.. When a change has been made to a functional module, for example 
   if a new function has been added or a bug has been corrected, 
   simple code replacement is sufficient.

新しい関数を追加した、バグの修正をしたなど、機能性モジュールの変更を行った場合は、シンプルコード交換で十分です。

.. Example:

サンプル:

.. code-block:: erlang

   {"2",
    [{"1", [{load_module, m}]}],
    [{"1", [{load_module, m}]}]
   }.

.. 12.2 Changing a Residence Module

レジデンスモジュールの変更
==========================

.. In a system implemented according to the OTP Design Principles, 
   all processes, except system processes and special processes, 
   reside in one of the behaviours supervisor, gen_server, gen_fsm 
   or gen_event. These belong to the STDLIB application and 
   upgrading/downgrading normally requires an emulator restart.

OTP設計原則に従ったシステム実装ををすると、システムプロセスと特殊なプロセスを除く、全てのプロセスは ``supervisor`` 、 ``gen_server`` 、 ``gen_fsm`` 、 ``gen_event`` などのビヘイビア側に置かれることになります。これらは ``STDLIB`` アプリケーションに属し、通常の場合、アップグレードやダウングレードはエミュレータの再起動が必要となります。

.. OTP thus provides no support for changing residence modules except 
   in the case of special processes.

OTPは、特殊なプロセスを除いて、レジデンスモジュールの変更をサポートしています。

.. 12.3 Changing a Callback Module

コールバックモジュールの変更
============================

.. A callback module is a functional module, and for code extensions 
   simple code replacement is sufficient.

コールバックモジュールは機能性モジュールであるため、コードの拡張にあたっては、シンプルコード交換で十分です。

.. Example: When adding a function to ch3 as described in the example 
   in Release Handling, ch_app.appup looks as follows:

サンプル: :ref:`release_handling` の例で説明した ``ch3`` モジュールに関数を一つ追加した場合、 ``ch_app.appup`` は次のようになります。

.. code-block:: erlang

   {"2",
    [{"1", [{load_module, ch3}]}],
    [{"1", [{load_module, ch3}]}]
   }.

.. OTP also supports changing the internal state of behaviour 
   processes, see Changing Internal State below.

OTPはビヘイビアプロセスの内部ステートの変更についてもサポートしています。詳しくは :ref:`changing_internal_state` を参照してください。

.. 12.4 Changing Internal State

.. _changing_internal_state:

内部ステートの変更
==================

.. In this case, simple code replacement is not sufficient. The process 
   must explicitly transform its state using the callback function 
   code_change before switching to the new version of the callback 
   module. Thus synchronized code replacement is used.

このケースでは、シンプルコード交換では十分ではありません。新しいバージョンのコールバックモジュールに切り替える前に、プロセスは明示的にコールバック関数の ``code_change`` を使用して、内部ステートを変更する必要があります。同期コード交換が利用されます。

.. Example: Consider the gen_server ch3 from the chapter about the 
   gen_server behaviour. The internal state is a term Chs representing 
   the available channels. Assume we want add a counter N which keeps 
   track of the number of alloc requests so far. This means we need to 
   change the format to {Chs,N}.

サンプル: :ref:`gen_server` の章の ``ch3`` gen_serverについて考えていきます。内部ステートの ``Chs`` という項は、利用可能なチャンネルを表しています。これに、今まで割り当ててきた数をトラッキングし続ける、カウンター ``N`` を追加しようとしたとします。これはつまり、内部ステートのフォーマットを ``{Chs,N}`` にしなければならない、ということを意味しています。

.. The .appup file could look as follows:

``.appup`` ファイルは次のようになります。

.. code-block:: erlang

   {"2",
    [{"1", [{update, ch3, {advanced, []}}]}],
    [{"1", [{update, ch3, {advanced, []}}]}]
   }.

.. The third element of the update instruction is a tuple {advanced,Extra} 
   which says that the affected processes should do a state transformation 
   before loading the new version of the module. This is done by the 
   processes calling the callback function code_change (see gen_server(3)). 
   The term Extra, in this case [], is passed as-is to the function:

``update`` 命令の三番目の項目は、 ``{advanced, Extra}`` というタプルになっています。これは、適応されるプロセスは、新しいバージョンのモジュールをロードする前に内部ステートを変更しなければならない、ということを表しています。この変更作業は、プロセスが ``code_change`` コールバック関数( ``gen_server(3)`` 参照)を呼び出すことで行われます。このサンプルのケースでは、 ``Extra`` の項は ``[]`` となっています。この項は、そのままの形で関数に渡されます。


.. code-block:: erlang

   -module(ch3).
   ...
   -export([code_change/3]).
   ...
   code_change({down, _Vsn}, {Chs, N}, _Extra) ->
       {ok, Chs};
   code_change(_Vsn, Chs, _Extra) ->
       {ok, {Chs, 0}}.

.. The first argument is {down,Vsn} in case of a downgrade, or Vsn 
   in case of an upgrade. The term Vsn is fetched from the 'original' 
   version of the module, i.e. the version we are upgrading from, or 
   downgrading to.

最初の引数の ``{down,Vsn}`` はダウングレード時の、 ``Vsn`` はアップグレード時、という意味になります。 ``Vsn`` という項はモジュールの元のバージョンから取得されます。これはアップグレード元、あるいはダウングレード先のバージョンになります。

.. The version is defined by the module attribute vsn, if any. There is 
   no such attribute in ch3, so in this case the version is the checksum 
   (a huge integer) of the BEAM file, an uninteresting value which is ignored.

バージョンは、もしあればモジュール属性の ``vsn`` で定義されます。この ``ch3`` の場合には見あたらないため、とても巨大な数値ですが、BEAMファイルのチェックサムとなります。このプログラムでは特に必要ではないため無視しています。

.. (The other callback functions of ch3 need to be modified as well and 
   perhaps a new interface function added, this is not shown here).

(ここでは表示されていませんが、 ``ch3`` の他のコールバック変数も同じように変更されていたり、新しいインタフェース関数が追加されている必要があります。)

.. 12.5 Module Dependencies

モジュールの依存
================

.. Assume we extend a module by adding a new interface function, 
   as in the example in Release Handling, where a function available/0 
   is added to ch3.

:ref:`release_handling` のサンプルで示したように、新しいインタフェース関数を追加してモジュールを拡張したとします。ここでは、 ``available/0`` という関数を ``ch3`` モジュールに追加したものとして説明を行います。

.. If we also add a call to this function, say in the module m1, 
   a run-time error could occur during release upgrade if the new 
   version of m1 is loaded first and calls ch3:available/0 before 
   the new version of ch3 is loaded.

もし、 ``m1`` と呼ばれるモジュールからこの関数への呼び出しを追加した場合、新しいバージョンの ``ch3`` がロードされる前に、新しいバージョンの ``m1`` がロードされて、 ``ch3:available/0`` を呼び出したとすると、リリースのアップグレード時にランタイムエラーが発生してしまいます。

.. Thus, ch3 must be loaded before m1 is, in the upgrade case, and 
   vice versa in the downgrade case. We say that m1 is dependent on ch3. 
   In a release handling instruction, this is expressed by the element 
   DepMods:

そのため、このようなアップグレード場合や、逆にダウングレードを行う場合は、 ``ch3`` は ``m1`` よりも先にロードされなければなりません。私たちはこのようなケースを「 ``m1`` は ``ch3`` に **依存している** 」 と呼んでいます。リリースハンドリング命令の中では、 ``DepMods`` という要素を使ってこれを表現します。

.. code-block:: erlang

   {load_module, Module, DepMods}
   {update, Module, {advanced, Extra}, DepMods}

.. DepMods is a list of modules, on which Module is dependent.

``DepMods`` は、対象のモジュールが依存しているモジュール群のリストになります。

.. Example: The module m1 in the application myapp is dependent on 
   ch3 when upgrading from "1" to "2", or downgrading from "2" to "1":

サンプル: ``myapp`` アプリケーション内の ``m1`` モジュールは1から2へのアップグレード、および、2から1へのダウングレード時には、 ``ch3`` に依存します。

myapp.appup:

.. code-block:: erlang

   {"2",
    [{"1", [{load_module, m1, [ch3]}]}],
    [{"1", [{load_module, m1, [ch3]}]}]
   }.

ch_app.appup:

.. code-block:: erlang

   {"2",
    [{"1", [{load_module, ch3}]}],
    [{"1", [{load_module, ch3}]}]
   }.

.. If m1 and ch3 had belonged to the same application, the .appup 
   file could have looked like this:

``m1`` と ``ch3`` が同じアプリケーションに属しているのであれば、 ``.appup`` ファイルは次のような見た目になります。

.. code-block:: erlang

   {"2",
    [{"1",
      [{load_module, ch3},
       {load_module, m1, [ch3]}]}],
    [{"1",
      [{load_module, ch3},
       {load_module, m1, [ch3]}]}]
   }.

.. Note that it is m1 that is dependent on ch3 also when downgrading. 
   systools knows the difference between up- and downgrading and will 
   generate a correct relup, where ch3 is loaded before m1 when 
   upgrading but m1 is loaded before ch3 when downgrading.

ダウングレードの際にも、 ``ch3`` は ``m1`` に依存していることに注意してください。 ``systools`` はアップグレードとダウングレードの違いについても知っていて、アップグレード時には ``m1`` よりも先に ``ch3`` を読み込み、ダウングレード時には ``ch3`` よりも先に ``m1`` を読み込むという、正しい ``relup`` を生成します。

.. 12.6 Changing Code For a Special Process

特別なプロセスのコード交換
==========================

.. In this case, simple code replacement is not sufficient. When a new 
   version of a residence module for a special process is loaded, 
   the process must make a fully qualified call to its loop function 
   to switch to the new code. Thus synchronized code replacement must be used. 

この場合、シンプルコード交換では不十分です。特別なプロセスのための、新しいバージョンのレジデンスモジュールがロードされると、プロセスのループ関数呼び出しは、新しいコード上のループ関数呼び出しに切り替える必要があります。これには、同期コード交換が必要となります。

.. note::

   .. The name(s) of the user-defined residence module(s) must be listed 
      in the Modules part of the child specification for the special 
      process, in order for the release handler to find the process.

   リリースハンドラがプロセスを見つけるためには、ユーザ定義のレジデンスモジュールの名前が、特別なプロセスの :ref:`child_spec` の中の ``Modules`` リストに設定されている必要があります。

.. Example. Consider the example ch4 from the chapter about sys and proc_lib. When started by a supervisor, the child specification could look like this:

サンプル: :ref:`sys_and_proc_lib` の章で説明した ``ch4`` モジュールについて考えていきます。これはスーパバイザから起動され、　:ref:`child_spec` は次のようになっています。 

.. code-block:: erlang

   {ch4, {ch4, start_link, []},
    permanent, brutal_kill, worker, [ch4]}

.. If ch4 is part of the application sp_app and a new version of the 
   module should be loaded when upgrading from version "1" to "2" of 
   this application, sp_app.appup could look like this:

もし ``ch4`` が ``sp_app`` アプリケーションの一部であり、このアプリケーションを1から2にアップグレードする際に、新しいバージョンのモジュールをロードする必要があるとします。この :file:`sa_app.appup` は次のようになります。


.. code-block:: erlang

   {"2",
    [{"1", [{update, ch4, {advanced, []}}]}],
    [{"1", [{update, ch4, {advanced, []}}]}]
   }.

.. The update instruction must contain the tuple {advanced,Extra}. 
   The instruction will make the special process call the callback 
   function system_code_change/4, a function the user must implement. 
   The term Extra, in this case [], is passed as-is to system_code_change/4:

この ``update`` 命令には ``{advanced, Extra}`` タプルを含めなければなりません。この命令は特別なプロセスに対して、 ``system_code_change/4`` というコールバック関数を呼ぶようにさせます。ユーザはこの関数を実装しなければなりません。このサンプルの場合は、 ``Extra`` の項は ``[]`` ですが、これは ``system_code_change/4`` にそのまま渡されます。

.. code-block:: erlang

   -module(ch4).
   ...
   -export([system_code_change/4]).
   ...

   system_code_change(Chs, _Module, _OldVsn, _Extra) ->
       {ok, Chs}.

.. The first argument is the internal state State passed from the 
   function sys:handle_system_msg(Request, From, Parent, Module, Deb, State), 
   called by the special process when a system message is received. In ch4, 
   the internal state is the set of available channels Chs.

最初の引数は、システムメッセージを受信したときに、特別なプロセスから呼ばれた、 ``sys:handle_system_msg(Request, From, Parent, Module, Deb, State)`` 関数に渡された、内部ステートの ``State`` です。 ``ch4`` の中では、利用可能なチャンネルのリストの ``Chs`` が内部ステートとして設定されます。

.. The second argument is the name of the module (ch4).

2つ目の引数はモジュールの名前(``ch4``)になります。

.. The third argument is Vsn or {down,Vsn} as described for 
   gen_server:code_change/3.

3つ目の引数は ``gen_server:code_change/3`` の所で説明した通り、 ``Vsn`` か ``{down,Vsn}`` となります。

.. In this case, all arguments but the first are ignored and the function 
   simply returns the internal state again. This is enough if the code 
   only has been extended. If we had wanted to change the internal 
   state (similar to the example in Changing Internal State), it would 
   have been done in this function and {ok,Chs2} returned.

この場合、最初の引数以外のすべての引数を無視して、内部ステートをそのまま返しています。単なるコード拡張であれば、これで十分です。内部ステートを変更したい場合には(:ref:`changing_internal_state` のサンプルと同じように)この関数の中で行って、 ``{ok,Chs2}`` を返せば行えます。

.. 12.7 Changing a Supervisor

スーパバイザの変更
==================

.. The supervisor behaviour supports changing the internal state, 
   i.e. changing restart strategy and maximum restart frequency 
   properties, as well as changing existing child specifications.

:ref:`supervisor` は、既存の :ref:`child_spec` の変更と同じように、 :ref:`restart` や :ref:`restart_requency` などの内部ステートの変更をサポートしています。

.. Adding and deleting child processes are also possible, but not 
   handled automatically. Instructions must be given by in the .appup file.

子プロセスの追加と削除も行えますが、これは自動では行えません。 ``.appup`` ファイルに命令を追加する必要があります。

.. 12.7.1 Changing Properties


プロパティの変更
----------------

.. Since the supervisor should change its internal state, synchronized 
   code replacement is required. However, a special update instruction 
   must be used.

スーパバイザの内部ステートを変更しなければならない時は、同期コード交換が必要となります。しかし、特別な ``update`` 命令を使用する必要があります。

.. The new version of the callback module must be loaded first both 
   in the case of upgrade and downgrade. Then the new return value 
   of init/1 can be checked and the internal state be changed accordingly.

この場合は、アップグレード、ダウングレードの両方の場合で、コールバックモジュールの新しいバージョンが最初にロードされる必要があります。 ``init/1`` の新しい返り値がチェックでき、それにしたがって内部ステートが変更されます。

.. The following upgrade instruction is used for supervisors:

スーパバイザに対しては、次のような ``update`` 命令が使用されます。

.. code-block:: erlang

   {update, Module, supervisor}

.. Example: Assume we want to change the restart strategy of ch_sup 
   from the Supervisor Behaviour chapter from one_for_one to one_for_all. 
   We change the callback function init/1 in ch_sup.erl:

サンプル: :ref:`supervisor` の章の ``ch_sup`` の再起動戦略を ``one_for_one`` から ``one_for_all`` に変更したいとします。 ``ch_sup.erl`` の ``init/1`` のコールバック関数を次のように変更します。

.. code-block:: erlang

   -module(ch_sup).
   ...

   init(_Args) ->
       {ok, {{one_for_all, 1, 60}, ...}}.
   
.. The file ch_app.appup:

``ch_app.appup`` は次のようになります。

.. code-block:: erlang

   {"2",
    [{"1", [{update, ch_sup, supervisor}]}],
    [{"1", [{update, ch_sup, supervisor}]}]
   }.

.. 12.7.2 Changing Child Specifications

子プロセスの仕様の変更
----------------------

.. The instruction, and thus the .appup file, when changing an 
   existing child specification, is the same as when changing 
   properties as described above:

子プロセスの仕様の変更を行うのも、 ``.appup`` ファイルに対して、上記で説明したプロパティの変更と同じように変更することで行うことができます。

.. code-block:: erlang

   {"2",
    [{"1", [{update, ch_sup, supervisor}]}],
    [{"1", [{update, ch_sup, supervisor}]}]
   }.

.. The changes do not affect existing child processes. For example, 
   changing the start function only specifies how the child process 
   should be restarted, if needed later on.

この変更は既存の子プロセスには影響を与えません。例えば、起動関数の変更は、子プロセスの再起動時にのみ適用されます。

.. Note that the id of the child specification cannot be changed.

子プロセスのidは変更できないことに注意してください。

.. Note also that changing the Modules field of the child specification 
   may affect the release handling process itself, as this field is used 
   to identify which processes are affected when doing a synchronized code 
   replacement.

子プロセスの仕様の ``Modules`` フィールドの変更は、リリースハンドリングプロセスそのものに影響を与えます。このフィールドは、同期コード交換を行うときに、どのプロセスが影響を受けるかを特定するのに使用されます。

.. 12.7.3 Adding And Deleting Child Processes

.. _adding_and_deleting_child_processes:

子プロセスの追加と削除
----------------------

.. As stated above, changing child specifications does not affect 
   existing child processes. New child specifications are automatically 
   added, but not deleted. Also, child processes are not automatically 
   started or terminated. Instead, this must be done explicitly using 
   apply instructions.

上記で説明した通り、子プロセスの仕様の変更は、既存の子プロセスには影響を与えません。新しい子プロセスの仕様は自動的に追加されますが、削除はされません。そのため、子プロセスは自動的には起動したり、停止したりすることはありません。これを行うには、明示的に命令を使用しなければなりません。

.. Example: Assume we want to add a new child process m1 to ch_sup 
   when upgrading ch_app from "1" to "2". This means m1 should be 
   deleted when downgrading from "2" to "1":

サンプル: ``ch_sup`` を1から2にアップグレードするときに、新しい子プロセス ``m1`` を ``ch_sup`` に追加したいと想定して話を進めます。これは、2から1にダウングレードするときには、この ``m1`` を削除しなければならない、ということを意味します。

.. code-block:: erlang

   {"2",
    [{"1",
      [{update, ch_sup, supervisor},
       {apply, {supervisor, restart_child, [ch_sup, m1]}}
      ]}],
    [{"1",
      [{apply, {supervisor, terminate_child, [ch_sup, m1]}},
       {apply, {supervisor, delete_child, [ch_sup, m1]}},
       {update, ch_sup, supervisor}
      ]}]
   }.

.. Note that the order of the instructions is important.

この命令の順番が大切です。

.. Note also that the supervisor must be registered as ch_sup for the 
   script to work. If the supervisor is not registered, it cannot be 
   accessed directly from the script. Instead a help function that finds 
   the pid of the supervisor and calls supervisor:restart_child etc. must 
   be written, and it is this function that should be called from the 
   script using the apply instruction.

また、スクリプトを動作させるためには、スーパバイザを ``ch_sup`` として登録しなければなりません。もしスーパバイザが登録されていないと、スクリプトからは直接アクセスすることができません。スーパバイザのpidを見つけて、 ``supervisor:restart_child`` などを呼び出すような補助関数を書く代わりに、 ``apply`` 命令を使って、スクリプトからこの関数を呼び出すようにします。

.. If the module m1 is introduced in version "2" of ch_app, it must also 
   be loaded when upgrading and deleted when downgrading:

モジュール ``m1`` が ``ch_app`` のバージョン2で導入されるのであれば、アップグレード時に追加されたり、ダウングレード時に削除されるようにしなければなりません。

.. code-block:: erlang

   {"2",
    [{"1",
      [{add_module, m1},
       {update, ch_sup, supervisor},
       {apply, {supervisor, restart_child, [ch_sup, m1]}}
      ]}],
    [{"1",
      [{apply, {supervisor, terminate_child, [ch_sup, m1]}},
       {apply, {supervisor, delete_child, [ch_sup, m1]}},
       {update, ch_sup, supervisor},
       {delete_module, m1}
      ]}]
   }.

.. Note again that the order of the instructions is important. When 
   upgrading, m1 must be loaded and the supervisor's child specification 
   changed, before the new child process can be started. When 
   downgrading, the child process must be terminated before child 
   specification is changed and the module is deleted.

ここでも命令の順番が台説です。アップグレード時には ``m1`` がロードされて、新しい子プロセスが起動される前に、スーパバイザの子プロセスの仕様が変更されます。ダウングレード時には子プロセスの仕様が変更される前に子プロセスが停止され、モジュールが削除されなければなりません。

.. 12.8 Adding or Deleting a Module

モジュールの追加と削除
======================

.. Example: A new functional module m is added to ch_app:

サンプル: 新しい機能性モジュール ``m`` が ``ch_app`` に追加されました。

.. code-block:: erlang

   {"2",
    [{"1", [{add_module, m}]}],
    [{"1", [{delete_module, m}]}]

.. 12.9 Starting or Terminating a Process

プロセスの起動と停止
====================

.. In a system structured according to the OTP design principles, any 
   process would be a child process belonging to a supervisor, see 
   Adding and Deleting Child Processes above.

OTP設計原則に従ったシステム構成の中では、あらゆるプロセスが、スーパバイザに所属する子プロセスになります。 :ref:`adding_and_deleting_child_processes` を参照してください。

.. 12.10 Adding or Removing an Application

アプリケーションの追加と削除
=============================

.. When adding or removing an application, no .appup file is needed. 
   When generating relup, the .rel files are compared and add_application 
   and remove_application instructions are added automatically.

アプリケーションの追加と削除時は ``.appup`` ファイルは不要です。 ``relup`` が生成されるときに、 ``.rel`` ファイルの比較が行われ、 ``add_application`` と ``remove_application`` 命令が自動的に追加されます。

.. 12.11 Restarting an Application

アプリケーションの再起動
========================

.. Restarting an application is useful when a change is too complicated 
   to be made without restarting the processes, for example if the 
   supervisor hierarchy has been restructured.

アプリケーションの再起動は、スーパバイザの階層構造の構成変更など、変更が複雑すぎて、プロセスの再起動なしに更新ができない場合に有用です。

.. Example: When adding a new child m1 to ch_sup, as in the example above, 
   an alternative to updating the supervisor is to restart the entire 
   application:

サンプル: 新しい子プロセスの ``m1`` を ``ch_sup`` に追加するときは、スーパバイザのアップデートの代わりに、アプリケーション全体を再起動することもできます。

.. code-block:: erlang

   {"2",
    [{"1", [{restart_application, ch_app}]}],
    [{"1", [{restart_application, ch_app}]}]
   }.

.. 12.12 Changing an Application Specification

.. _changing_an_application_specification:

アプリケーション仕様の変更
===========================

.. When installing a release, the application specifications are 
   automatically updated before evaluating the relup script. Hence, 
   no instructions are needed in the .appup file:

リリースをインストールするときは、アプリケーションの仕様は ``relup`` スクリプトの評価前に自動的に更新されます。そのため、 ``.appup`` ファイルの中には何も命令を含める必要はありません。

.. code-block:: erlang

   {"2",
    [{"1", []}],
    [{"1", []}]
   }.

.. 12.13 Changing Application Configuration

アプリケーション構成の変更
==========================

.. Changing an application configuration by updating the env key in 
   the .app file is an instance of changing an application specification, 
   see above.

``.app`` ファイルの ``env`` キーを更新してアプリケーションの構成を変更する場合には、上記の :ref:`changing_an_application_specification` を参照してください。

.. Alternatively, application configuration parameters can be added or 
   updated in sys.config.

これ以外には、アプリケーション構成パラメータを ``sys.config`` の中で追加したり更新することもできます。

.. 12.14 Changing Included Applications

インクルードされたアプリケーションの変更
========================================

.. The release handling instructions for adding, removing and restarting 
   applications apply to primary applications only. There are no 
   corresponding instructions for included applications. However, since 
   an included application is really a supervision tree with a topmost 
   supervisor, started as a child process to a supervisor in the 
   including application, a relup file can be manually created.

アプリケーションの追加、削除、再起動を行うリリース・ハンドリング命令はプライマリ・アプリケーションにだけに適用できます。インクルードされたアプリケーションに対応した命令はありません。しかし、インクルードされたアプリケーションは最上位のスーパバイザを伴う、監視ツリーであるのでり、インクルードされたアプリケーションが、スーパバイザの子プロセスとして起動されているのであれば、 ``relup`` ファイルを手動で作成することができます。

.. Example: Assume we have a release containing an application prim_app 
   which have a supervisor prim_sup in its supervision tree.

サンプル: 監視ツリー内に、 ``prim_sup`` というスーパバイザを持つ、 ``prim_app`` というアプリケーションを含むリリースを行おうとしていたとします。

.. In a new version of the release, our example application ch_app 
   should be included in prim_app. That is, its topmost supervisor 
   ch_sup should be started as a child process to prim_sup.

新しいバージョンのリリースには、 ``prim_app`` のの中に、 ``ch_app`` というサンプルのアプリケーションをふくめないといけません。この場合、 ``ch_sup`` の最上位のスーパバイザは、 ``prim_sup`` の子プロセスとして起動されなければなりません。

.. 1) Edit the code for prim_sup:

1. ``prim_sup`` のコードの編集

   .. code-block:: erlang

      init(...) ->
          {ok, {...supervisor flags...,
                [...,
                 {ch_sup, {ch_sup,start_link,[]},
                  permanent,infinity,supervisor,[ch_sup]},
                 ...]}}.

.. 2) Edit the .app file for prim_app:

2. ``prim_app`` の ``.app`` ファイルの編集

   .. code-block:: erlang

      {application, prim_app,
       [...,
        {vsn, "2"},
        ...,
        {included_applications, [ch_app]},
        ...
       ]}.

.. 3) Create a new .rel file, including ch_app:

3. ``ch_app`` を含む、新しい ``.rel`` ファイルの作成

   .. code-block:: erlang

      {release,
       ...,
       [...,
        {prim_app, "2"},
        {ch_app, "1"}]}.

.. 12.14.1 Application Restart

アプリケーションの再起動
------------------------

.. 4a) One way to start the included application is to restart the entire 
   prim_app application. Normally, we would then use the restart_application 
   instruction in the .appup file for prim_app.

4. (a)インクルードされたアプリケーションの起動する方法の一つは、 ``prim_app`` アプリケーション全体を再起動する方法です。通常、 ``restart_application`` 命令を ``prim_app`` の ``.appup`` ファイルに記述して使います。

.. However, if we did this and then generated a relup file, not only 
   would it contain instructions for restarting (i.e. removing and adding) 
   prim_app, it would also contain instructions for starting ch_app 
   (and stopping it, in the case of downgrade). This is due to the 
   fact that ch_app is included in the new .rel file, but not in the 
   old one.

しかし、これを行って ``relup`` ファイルを生成するには、削除や追加などの ``prim_app`` の再起動のための命令を含めるだけではなく、 ``ch_app`` の起動(ダウングレード時は停止も)の命令も含めなければなりません。これは、 ``ch_app`` が新しい ``.rel`` ファイルに含まれているからではなく、古いファイルに含まれていないためです。

.. Instead, a correct relup file can be created manually, either from 
   scratch or by editing the generated version. The instructions for 
   starting/stopping ch_app are replaced by instructions for 
   loading/unloading the application:

その代わりに、正しい ``relup`` ファイルを手動で作成することができます。方法としては、スクラッチから作成する方法と、生成されたものを編集する方法があります。 ``ch_app`` の起動/停止の命令は、アプリケーションのロード/アンロードの命令と置き換えます。

.. code-block:: erlang

   {"B",
    [{"A",
      [],
      [{load_object_code,{ch_app,"1",[ch_sup,ch3]}},
       {load_object_code,{prim_app,"2",[prim_app,prim_sup]}},
       point_of_no_return,
       {apply,{application,stop,[prim_app]}},
       {remove,{prim_app,brutal_purge,brutal_purge}},
       {remove,{prim_sup,brutal_purge,brutal_purge}},
       {purge,[prim_app,prim_sup]},
       {load,{prim_app,brutal_purge,brutal_purge}},
       {load,{prim_sup,brutal_purge,brutal_purge}},
       {load,{ch_sup,brutal_purge,brutal_purge}},
       {load,{ch3,brutal_purge,brutal_purge}},
       {apply,{application,load,[ch_app]}},
       {apply,{application,start,[prim_app,permanent]}}]}],
    [{"A",
      [],
      [{load_object_code,{prim_app,"1",[prim_app,prim_sup]}},
       point_of_no_return,
       {apply,{application,stop,[prim_app]}},
       {apply,{application,unload,[ch_app]}},
       {remove,{ch_sup,brutal_purge,brutal_purge}},
       {remove,{ch3,brutal_purge,brutal_purge}},
       {purge,[ch_sup,ch3]},
       {remove,{prim_app,brutal_purge,brutal_purge}},
       {remove,{prim_sup,brutal_purge,brutal_purge}},
       {purge,[prim_app,prim_sup]},
       {load,{prim_app,brutal_purge,brutal_purge}},
       {load,{prim_sup,brutal_purge,brutal_purge}},
       {apply,{application,start,[prim_app,permanent]}}]}]
   }.

.. 12.14.2 Supervisor Change

スーパバイザの変更
------------------

.. 4b) Another way to start the included application (or stop it in the 
   case of downgrade) is by combining instructions for adding and removing 
   child processes to/from prim_sup with instructions for loading/unloading 
   all ch_app code and its application specification.

4. (b)インクルードされたアプリケーションの起動(ダウングレード時は停止)を行うもう一つの方法は、 ``prim_sup`` に対する子プロセスの追加と削除の命令と、全ての ``ch_app`` のコードと、アプリケーション仕様のロード/アンロードの命令を組み合わせるという方法があります。

.. Again, the relup file is created manually. Either from scratch or by 
   editing a generated version. Load all code for ch_app first, and also 
   load the application specification, before prim_sup is updated. When 
   downgrading, prim_sup should be updated first, before the code for 
   ch_app and its application specification are unloaded.

繰り返しになりますが、 ``relup`` ファイルは手動で作成する必要があります。スクラッチで書く方法と、生成されたファイルを編集する方法があります。 ``prim_sup`` が更新され、そのアプリケーション仕様がアンロードされる前に、 ``ch_app`` に関するすべてのコードを最初にロードし、アプリケーション仕様もロードします。

.. code-block:: erlang

   {"B",
    [{"A",
      [],
      [{load_object_code,{ch_app,"1",[ch_sup,ch3]}},
       {load_object_code,{prim_app,"2",[prim_sup]}},
       point_of_no_return,
       {load,{ch_sup,brutal_purge,brutal_purge}},
       {load,{ch3,brutal_purge,brutal_purge}},
       {apply,{application,load,[ch_app]}},
       {suspend,[prim_sup]},
       {load,{prim_sup,brutal_purge,brutal_purge}},
       {code_change,up,[{prim_sup,[]}]},
       {resume,[prim_sup]},
       {apply,{supervisor,restart_child,[prim_sup,ch_sup]}}]}],
    [{"A",
      [],
      [{load_object_code,{prim_app,"1",[prim_sup]}},
       point_of_no_return,
       {apply,{supervisor,terminate_child,[prim_sup,ch_sup]}},
       {apply,{supervisor,delete_child,[prim_sup,ch_sup]}},
       {suspend,[prim_sup]},
       {load,{prim_sup,brutal_purge,brutal_purge}},
       {code_change,down,[{prim_sup,[]}]},
       {resume,[prim_sup]},
       {remove,{ch_sup,brutal_purge,brutal_purge}},
       {remove,{ch3,brutal_purge,brutal_purge}},
       {purge,[ch_sup,ch3]},
       {apply,{application,unload,[ch_app]}}]}]
   }.

.. 12.15 Changing Non-Erlang Code

Erlang以外のコードの変更
========================

.. Changing code for a program written in another programming language 
   than Erlang, for example a port program, is very application dependent 
   and OTP provides no special support for it.

ポートプログラムなどErlang以外のプログラミング言語で書かれたプログラムの変更は、アプリケーションに非常に依存します。OTPではこれに対する特別なサポートを提供していません。

.. Example, changing code for a port program: Assume that the Erlang 
   process controlling the port is a gen_server portc and that the 
   port is opened in the callback function init/1:

ポートプログラムへの変更の例: Erlangプロセスが、 gen_serverの ``portc`` というポートの制御をしているという想定の下に話を進めます。このポートはコールバック関数の ``init/1`` の中でオープンされています。

.. code-block:: erlang

   init(...) ->
       ...,
       PortPrg = filename:join(code:priv_dir(App), "portc"),
       Port = open_port({spawn,PortPrg}, [...]),
       ...,
       {ok, #state{port=Port, ...}}.

.. If the port program should be updated, we can extend the code for 
   the gen_server with a code_change function which closes the old 
   port and opens a new port. (If necessary, the gen_server may first 
   request data that needs to be saved from the port program and pass 
   this data to the new port):

もしポートプログラムの更新が必要な場合、gen_serverの ``code_change`` 関数を拡張し、古いポートを閉じて、新しいポートを開くことができます。必要であれば、gen_server古いポートプログラムに保存すべきデータを最初に要求し、新しいポートにこのデータを渡すこともできます。

.. code-block:: erlang

   code_change(_OldVsn, State, port) ->
       State#state.port ! close,
       receive
           {Port,close} ->
               true
       end,
       PortPrg = filename:join(code:priv_dir(App), "portc"),
       Port = open_port({spawn,PortPrg}, [...]),
       {ok, #state{port=Port, ...}}.

.. Update the application version number in the .app file and write 
   an .appup file:

``.app`` ファイル内のアプリケーションのバージョン番号更新を行い、 ``.appup`` ファイルを書きます。

.. code-block:: erlang

   ["2",
    [{"1", [{update, portc, {advanced,port}}]}],
    [{"1", [{update, portc, {advanced,port}}]}]
   ].

.. Make sure the priv directory where the C program is located is 
   included in the new release package:

Ｃのプログラムが置かれている ``priv`` ディレクトリの情報を、新しいリリースパッケージに含めるようにします。

.. code-block:: erlang

   1> systools:make_tar("my_release", [{dirs,[priv]}]).
   ...

.. 12.16 Emulator Restart

エミュレータの再起動
====================

.. If the emulator can or should be restarted, the very simple .relup 
   file can be created manually:

もしエミュレータの再起動が行える、もしくは行わなければならない場合には、次のような ``.relup`` ファイルを作るだけで簡単に行えます。

.. code-block:: erlang

   {"B",
    [{"A",
      [],
      [restart_new_emulator]}],
    [{"A",
      [],
      [restart_new_emulator]}]
   }.

.. This way, the release handler framework with automatic packing and 
   unpacking of release packages, automatic path updates etc. can be 
   used without having to specify .appup files.

この方法を使うと、特別な ``.appup`` ファイルを使わないでも、リリースハンドラフレームワークは自動的にリリースパッケージをパックしたり、展開したり、パスを自動的に更新したりできます。

.. If some transformation of persistent data, for example database 
   contents, needs to be done before installing the new release version, 
   instructions for this can be added to the .relup file as well.

もし、データベースの内容の変更など、永続されたデータの変更が必要な場合は、新しいリリースバージョンをインストールする前に、同じようにしてこのための命令を ``.relup`` ファイルに追加することができます。

Copyright (c) 1991-2009 Ericsson AB
