.. 7 Applications

================
アプリケーション
================

.. This chapter should be read in conjunction with app(4) and application(3).

この章は、app(4)とapplication(3)と合わせて読んでください。

.. 7.1 Application Concept

アプリケーションの概念
======================

.. When we have written code implementing some specific functionality, we might 
   want to make the code into an application, that is a component that can be 
   started and stopped as a unit, and which can be re-used in other systems as well.

いくつかの機能を実装するコードを書いたら、それを単位ごとに起動停止ができ、他のシステムでもうまく再利用できるように、 **アプリケーション** としてにまとめたいと思うでしょう。

.. To do this, we create an application callback module, where we describe 
   how the application should be started and stopped.

これを行うためには、アプリケーションの起動と停止に関するコードを記述した、 :ref:`application_callback_module` を作成します。

.. Then, an application specification is needed, which is put in an application 
   resource file. Among other things, we specify which modules the application 
   consists of and the name of the callback module.

また、 :ref:`application_resource_file` に書かれた、 **アプリケーションの仕様** が必要となります。これ以外には、アプリケーションがどのモジュールから構成されるのか、コールバックのモジュール名を調べます。

.. If we use systools, the Erlang/OTP tools for packaging code (see Releases), the 
   code for each application is placed in a separate directory following a pre-defined 
   directory structure.

もしsystools、Erlang/OTPのコードのパッケージツールを使用するのであれば、コードは、これから説明する事前に定義された :ref:`directory_structure` に、アプリケーションごとにコードを配置します。

.. 7.2 Application Callback Module

.. _application_callback_module:

アプリケーション・コールバックモジュール
========================================

.. How to start and stop the code for the application, i.e. 
   the supervision tree, is described by two callback functions:

監視ツリーなど、アプリケーションの開始と終了でどのようなことをするかは、次の2つのコールバック関数で定義します。

.. code-block:: erlang

   start(StartType, StartArgs) -> {ok, Pid} | {ok, Pid, State}
   stop(State)

.. start is called when starting the application and should create the supervision 
   tree by starting the top supervisor. It is expected to return the pid of the 
   top supervisor and an optional term State, which defaults to []. This term 
   is passed as-is to stop.

``start`` 関数はアプリケーションの起動時に呼び出されます。この関数内では、トップレベルのスーパバイザを起動し、監視ツリーを作る必要があります。この関数はトップのスーパバイザと、追加の項の ``State`` を返すことが期待されます。デフォルトの ``State`` は ``[]`` です。この項はそのまま ``stop`` 関数に渡されます。

.. StartType is usually the atom normal. It has other values only in the case of a 
   takeover or failover, see Distributed Applications. StartArgs is defined by the 
   key mod in the application resource file file.

``StartType`` は通常アトムが使用されます。もし、テイクオーバー、あるいはフェイルオーバーの時は別の値を受け取ります。これらに関しては分散アプリケーションの説明を参照してください。 ``StartArgs`` は :ref:`application_resource_file` の中の、 ``mod`` キーで定義されています。

.. stop/1 is called after the application has been stopped and should do any 
   necessary cleaning up. Note that the actual stopping of the application, that 
   is the shutdown of the supervision tree, is handled automatically as described 
   in Starting and Stopping Applications.

``stop/1`` はアプリケーションの停止後に呼ばれます。この中には、必要な終了処理を実装しなければなりません。実際のアプリケーションの終了では、監視ツリーのシャットダウンは :ref:`starting_and_stopping_application` の中で説明します。

.. Example of an application callback module for packaging the supervision 
   tree from the Supervisor chapter:

サンプルとして、 :ref:`supervisor` の章の監視ツリーをパッケージ化するアプリケーションコールバックモジュールのサンプルを紹介します。

.. code-block:: erlang

   -module(ch_app).
   -behaviour(application).

   -export([start/2, stop/1]).

   start(_Type, _Args) ->
       ch_sup:start_link().

   stop(_State) ->
       ok.

.. A library application, which can not be started or stopped, does not need any 
   application callback module.

ライブラリのアプリケーションは、アプリケーションコールバックモジュールを必要とせず、起動も終了もできません。

.. 7.3 Application Resource File

.. _application_resource_file:

アプリケーション・リソースファイル
==================================

.. To define an application, we create an application specification which is put 
   in an application resource file, or in short .app file:

アプリケーションを定義するには、 **アプリケーション仕様** を記述した **アプリケーション・リソースファイル** 、あるいは短く ``.app`` ファイルと呼ばれるファイルを作成します。

.. code-block:: erlang

   {application, Application, [Opt1,...,OptN]}.

.. Application, an atom, is the name of the application. The file must be named 
   Application.app.

``Application`` の項は、アプリケーションの名前をアトムで指定します。このファイルは ``Application.app`` という名前にしなければなりません。

.. Each Opt is a tuple {Key, Value} which define a certain property of the 
   application. All keys are optional. Default values are used for any omitted keys.

それぞれの ``Opt`` は、 ``{Key, Value}`` というタプルで、アプリケーションのプロパティを定義します。すべてのキーはおプヨンです。省略したキーにタイしては、デフォルトの値が使用されます。

.. The contents of a minimal .app file for a library application libapp looks like this:

最小の、ライブラリアプリケーション向けの ``.app`` ファイルのサンプルは次の通りです。

.. code-block:: erlang

   {application, libapp, []}.

監視ツリーのための最小の ``.app`` ファイルのサンプルの ``ch_app.app`` は、次のようになります。

.. The contents of a minimal .app file ch_app.app for a supervision tree application 
   like ch_app looks like this:

.. code-block:: erlang

   {application, ch_app,
    [{mod, {ch_app,[]}}]}.

.. The key mod defines the callback module and start argument of the application, 
   in this case ch_app and [], respectively. This means that

``mod`` キーは、コールバックモジュールと、アプリケーションのスタート時の引数を定義します。この場合は、 ``ch_app`` モジュールに引数 ``[]`` を渡すという指定になっています。これは、

.. code-block:: erlang

   ch_app:start(normal, [])

という呼び出しがアプリケーションの起動時に行われ、

will be called when the application should be started and

.. code-block:: erlang

   ch_app:stop([])

.. will be called when the application has been stopped.

という処理がアプリケーションの終了時に呼ばれます。

.. When using systools, the Erlang/OTP tools for packaging code (see Releases), 
   the keys description, vsn, modules, registered and applications should also 
   be specified:

``systools`` を使用している場合、コードをパッケージ化するErlang/OTPのツール( :ref:`releases` 参照)を使用する場合は、 ``description``, ``vsn``, ``modules``, ``registered``, ``aapplication`` というキーを定義する必要があります。

.. code-block:: erlang

   {application, ch_app,
    [{description, "Channel allocator"},
     {vsn, "1"},
     {modules, [ch_app, ch_sup, ch3]},
     {registered, [ch3]},
     {applications, [kernel, stdlib, sasl]},
     {mod, {ch_app,[]}}
    ]}.

description
    .. A short description, a string. Defaults to "". 

    文字書い説明の文字列。デフォルトは""。

vsn
    .. Version number, a string. Defaults to "". 

    バージョン番号の文字列。デフォルトは""。

modules

    .. All modules introduced by this application. systools uses this list when 
       generating boot scripts and tar files. A module must be defined in one 
       and only one application. Defaults to []. 

    このアプリケーションに含まれる全モジュールです。 ``systools`` は、ブートスクリプトの生成や、tarファイルの作成にこのリストを使用します。モジュールは1つのアプリケーションにつき、1つ定義されなければなりません。デフォルトは ``[]`` です。

registered
    
    .. All names of registered processes in the application. systools uses this list 
       to detect name clashes between applications. Defaults to []. 

    プリケーション内で、登録されたプロセスのすべての名前を指定します。 ``systools`` はこのリストを用いて、アプリケーション間の名前の衝突を検知します。デフォルトは ``[]`` です。

applications

    .. All applications which must be started before this application is started. 
       systools uses this list to generate correct boot scripts. Defaults to [], 
       but note that all applications have dependencies to at least kernel and stdlib. 

    このアプリケーションの起動前に起動しておかないといけない全アプリケーションを指定します。 ``systools`` は、正しいブートスクリプトを起動するのに、このリストを利用します。デフォルトは ``[]`` ですが、すべてのアプリケーションは最低でも ``kernel `` と ``stdlib`` に依存しています。

.. The syntax and contents of of the application resource file are described in detail 
   in app(4).

アプリケーションリソースファイルの文法と内容については、app(4)の中で詳細に説明します。

.. 7.4 Directory Structure

.. _directory_structure:

ディレクトリ構造
================

.. When packaging code using systools, the code for each application is placed in a 
   separate directory lib/Application-Vsn, where Vsn is the version number.

``systools`` を用いてコードをパッケージングするときは、それぞれのアプリケーションのコードは ``lib/Application-Vsn`` という個別のディレクトリに分けて配置して置きます。 ``Vsn`` はバージョン番号です。

.. This may be useful to know, even if systools is not used, since Erlang/OTP itself 
   is packaged according to the OTP principles and thus comes with this directory 
   structure. The code server (see code(3)) will automatically use code from the 
   directory with the highest version number, if there are more than one version 
   of an application present.

このやり方は、例え ``systools`` を使っていなかったとしても、Erlang/OTP自身がこの原則に従って、このディレクトリ構造でパッケージングされているため、知っておくと便利です。もし複数バージョンのアプリケーションがある場合、コードサーバ(code(3)参照)は自動的に、バージョン番号の高いものを選択します。

.. The application directory structure can of course be used in the development 
   environment as well. The version number may then be omitted from the name.

もちろん、アプリケーションのディレクトリ構造は開発環境の中で使用することもできます。また、バージョン番号は名前から取り除かれる場合があります。

.. The application directory have the following sub-directories:

アプリケーションディレクトリには次のようなサブディレクトリが含まれます。

* ``src``
* ``ebin``
* ``priv``
* ``include``

``src``

    .. Contains the Erlang source code. 

    Erlangのソースコードが格納されます。

``ebin``

    .. Contains the Erlang object code, the beam files. The .app file is also placed here. 

    Erlangのオブジェクトコードである、 ``beam`` ファイルが格納されます。また、 ``.app`` ファイルもここに置かれます。

``priv``

    .. Used for application specific files. For example, C executables are placed here. 
       The function code:priv_dir/1 should be used to access this directory. 

    アプリケーション固有のファイルの格納に使用されます。例えば、Cの実行ファイルがここに置かれます。 ``code:priv_dir/1`` 関数を使用すると、このディレクトリにアクセスすることができます。


``include``

    .. Used for include files. 

    インクルードファイルが格納されます。

.. 7.5 Application Controller

アプリケーション・コントローラ
==============================

.. When an Erlang runtime system is started, a number of processes are 
   started as part of the Kernel application. One of these processes 
   is the application controller process, registered as application_controller.

Erlangランタイムシステムが起動されると、Kernelアプリケーションの一部として、いくつかのプロセスが起動されます。これらのプロセスの一つが、 **アプリケーション・コントローラ** プロセスです。これは、 ``application_controller`` という名前で登録されます。

.. All operations on applications are coordinated by the application controller. 
   It is interfaced through the functions in the module application, see 
   application(3). In particular, applications can be loaded, unloaded, 
   started and stopped.

アプリケーション上のすべての操作は、アプリケーション・コントロ－ラが調整を行います。 ``application`` モジュール内の関数を通じて操作を行うことができます。これについては、 application(3)を参照してください。具体的には、アプリケーションのロードやアンロード、起動と停止が行えます。

.. 7.6 Loading and Unloading Applications

アプリケーションのロード、アンロード
====================================

.. Before an application can be started, it must be loaded. The application 
   controller reads and stores the information from the .app file.

アプリケーションを実行するには、前もってロードしておく必要があります。アプリケーションコントローラは、 ``.app`` ファイルの情報を読み込んで、保存します。

.. code-block:: erlang

   1> application:load(ch_app).
   ok
   2> application:loaded_applications().
   [{kernel,"ERTS  CXC 138 10","2.8.1.3"},
    {stdlib,"ERTS  CXC 138 10","1.11.4.3"},
    {ch_app,"Channel allocator","1"}]

.. An application that has been stopped, or has never been started, can be 
   unloaded. The information about the application is erased from the internal 
   database of the application controller.

停止されていて、起動されていないアプリケーションは、アンロードすることができます。アプリケーションコントローラ内の内部データベースから、アプリケーション情報が消去されます。

.. code-block:: erlang

   3> application:unload(ch_app).
   ok
   4> application:loaded_applications().
   [{kernel,"ERTS  CXC 138 10","2.8.1.3"},
    {stdlib,"ERTS  CXC 138 10","1.11.4.3"}]

.. note::

   .. Loading/unloading an application does not load/unload the code used by the 
      application. Code loading is done the usual way.

   アプリケーションのロードとアンロードは、アプリケーションで使用されているコードのロードとアンロードはしません。コードのロードは通常の方法で行われます。

.. 7.7 Starting and Stopping Applications

.. starting_and_stopping_application:

アプリケーションの起動と停止
============================

.. An application is started by calling:

次のように呼び出すと、アプリケーションを起動できます。

.. code-block:: erlang

   5> application:start(ch_app).
   ok
   6> application:which_applications().
   [{kernel,"ERTS  CXC 138 10","2.8.1.3"},
    {stdlib,"ERTS  CXC 138 10","1.11.4.3"},
    {ch_app,"Channel allocator","1"}]

.. If the application is not already loaded, the application controller will first 
   load it using application:load/1. It will check the value of the applications 
   key, to ensure that all applications that should be started before this 
   application are running.

もしアプリケーションがまだロードされていなければ、アプリケーション・コントローラはまず最初に、 ``application:load/1`` を利用してロードします。この関数はアプリケーションキーの値をチェックし、そのアプリケーションの起動前に実行されているべき、すべてのアプリケーションが稼働されていることを確認します。

.. The application controller then creates an application master for the application. 
   The application master is the group leader of all the processes in the application. 
   The application master starts the application by calling the application callback 
   function start/2 in the module, and with the start argument, defined by the mod 
   key in the .app file.

アプリケーション・コントローラは、そのアプリケーションのための **アプリケーション・マスター** を作成します。アプリケーションマスターは、そのアプリケーションのすべてのプロセスのグループのリーダーです。アプリケーションマスターは、アプリケーションのモジュールに定義されたコールバック関数の ``start/2`` に、 ``.app`` ファイルの ``mod`` キーで定義された起動時の引数を付けて呼び出し、アプリケーションの実行を開始します。

.. An application is stopped, but not unloaded, by calling:

アプリケーションをロードしたままの状態で停止させるには、次のように呼び出します。

.. code-block:: erlang

   7> application:stop(ch_app).
   ok

The application master stops the application by telling the top supervisor to shutdown. The top supervisor tells all its child processes to shutdown etc. and the entire tree is terminated in reversed start order. The application master then calls the application callback function stop/1 in the module defined by the mod key.

アプリケーションマスターは、トップのスーパバイザに停止するように伝えて、アプリケーションを停止させます。トップのスーパバイザは、すべての子プロセスに停止するように伝え、ツリー全体は、起動した時とは逆の順序で終了します。全部の子プロセスが停止したら、アプリケーションマスターは、 ``mod`` キーで定義されたファイルのコールバック関数の ``stop/1`` を呼び出します。


.. 7.8 Configuring an Application

アプリケーションの設定
======================

.. An application can be configured using configuration parameters. 
   These are a list of {Par, Val} tuples specified by a key env in the .app file.

アプリケーションは設定パラメータを使用して、設定を行うことができます。これらは、 ``.app`` ファイル内の、 ``{Par, Val}`` というタプルで設定します。

.. code-block:: erlang

   {application, ch_app,
    [{description, "Channel allocator"},
     {vsn, "1"},
     {modules, [ch_app, ch_sup, ch3]},
     {registered, [ch3]},
     {applications, [kernel, stdlib, sasl]},
     {mod, {ch_app,[]}},
     {env, [{file, "/usr/local/log"}]}
    ]}.

.. Par should be an atom, Val is any term. The application can retrieve the 
   value of a configuration parameter by calling application:get_env(App, Par) 
   or a number of similar functions, see application(3).

``Par`` はアトムでなければなりません。 ``Val`` には任意の項を入れることができます。それぞれのアプリケーションは、 ``application:get_env(App, Par)`` 関数や、いくつかの類似の関数を呼ぶことで、設定パラメータの値を取得してくることができます。詳しくはapplication(3)を参照してください。

.. Example:

例:

.. code-block:: erlang

   % erl
   Erlang (BEAM) emulator version 5.2.3.6 [hipe] [threads:0]

   Eshell V5.2.3.6  (abort with ^G)
   1> application:start(ch_app).
   ok
   2> application:get_env(ch_app, file).
   {ok,"/usr/local/log"}

.. The values in the .app file can be overridden by values in a system configuration 
   file. This is a file which contains configuration parameters for relevant applications:

``.app`` ファイル内の値は、システム構成ファイルの値を使ってオーバーライドすることができます。このファイルは、関連のアプリケーション向けの設定パラメータを含んでいます。

.. code-block:: erlang

   [{Application1, [{Par11,Val11},...]},
     ...,
    {ApplicationN, [{ParN1,ValN1},...]}].

.. The system configuration should be called Name.config and Erlang should be 
   started with the command line argument -config Name. See config(4) for more 
   information.

システム設定は ``Name.config`` で呼び出されるか、Erlangの起動引数として、コマンドライン引数の ``-config Name`` を使用して読み込ませます。詳しくはconfig(4)を参照してください。

.. Example: A file test.config is created with the following contents:

例: ``test.config`` というファイルの中身が次のように書かれていたとします。

.. code-block:: erlang

   [{ch_app, [{file, "testlog"}]}].

.. The value of file will override the value of file as defined in the .app file:

``file`` というキーの値が、まるで ``.app`` ファイルの中で元々そのように定義されてあったように、このファイルの値でオーバーライドされます。

.. code-block:: erlang

   % erl -config test
   Erlang (BEAM) emulator version 5.2.3.6 [hipe] [threads:0]

   Eshell V5.2.3.6  (abort with ^G)
   1> application:start(ch_app).
   ok
   2> application:get_env(ch_app, file).
   {ok,"testlog"}

.. If release handling is used, exactly one system configuration file should be 
   used and that file should be called sys.config

もしも、 :ref:`updating_application_specifications` が使用されていて、1つのシステム構成だけが使用されるのであれば、そのファイルは ``sys.config`` という名前にすべきです。

The values in the .app file, as well as the values in a system configuration file, can be overridden directly from the command line:

``.app`` ファイルの値、および、システム構成ファイルの値は、次のようなコマンドライン引数を直接渡すと、オーバーライドすることができます。

.. code-block:: bash

   % erl -ApplName Par1 Val1 ... ParN ValN

.. Example:

例:

.. code-block:: erlang

   % erl -ch_app file '"testlog"'
   Erlang (BEAM) emulator version 5.2.3.6 [hipe] [threads:0]

   Eshell V5.2.3.6  (abort with ^G)
   1> application:start(ch_app).
   ok
   2> application:get_env(ch_app, file).
   {ok,"testlog"}

.. 7.9 Application Start Types

アプリケーションの起動タイプ
============================

.. A start type is defined when starting the application:

アプリケーションの起動時に、起動タイプが定義されます。

.. code-block:: erlang

   application:start(Application, Type)

.. application:start(Application) is the same as calling 
   application:start(Application, temporary). The type can also be
   permanent or transient:

``application:start(Application)`` という呼び出しは、 ``application:start(Application, temporary)`` と呼び出すのと同じです。タイプとしては、 ``permanent`` か ``transient`` が設定できます。

.. * If a permanent application terminates, all other applications and the 
     runtime system are also terminated.

* もしも ``permanent`` が設定されたアプリケーションが終了すると、他のすべてのアプリケーションとランタイムシステムも終了させられます。

.. * If a transient application terminates with reason normal, this is 
     reported but no other applications are terminated. If a transient 
     application terminates abnormally, that is with any other reason 
     than normal, all other applications and the runtime system are also terminated.

* もし ``transient`` が設定されたアプリケーションが、 ``normal`` という理由で終了した場合には、終了したことは報告されますが、他のアプリケーションが終了させられることはありません。もし、 ``transient`` が設定されたアプリケーションが ``normal`` 以外の理由で異常終了した場合には、他のすべてのアプリケーションとランタイムシステムも終了させられます。

.. * If a temporary application terminates, this is reported but no 
     other applications are terminated.

* もし一時的なアプリケーションが終了した場合には、終了したことは報告されますが、他のアプリケーションが終了させられることはありません。

.. It is always possible to stop an application explicitly by calling 
   application:stop/1. Regardless of the mode, no other applications will be affected.

``application:stop/1`` を呼び出して、アプリケーションを明示的に終了させることはいつでもできます。この場合は、モードに関わらず、他のアプリケーションは影響を受けません。

.. Note that transient mode is of little practical use, since when a supervision 
   tree terminates, the reason is set to shutdown, not normal.

``transient`` モードは、 ``normal`` 以外の理由を付けて監視ツリーを終了するような場合にはあまり使えない、ということに注意してください。

Copyright (c) 1991-2009 Ericsson AB
