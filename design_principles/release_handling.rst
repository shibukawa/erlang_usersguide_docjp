11 Release Handling

.. _release_handling:

======================
リリース・ハンドリング
======================

.. 11.1 Release Handling Principles

リリース・ハンドリングの原則
============================

.. An important feature of the Erlang programming language is the ability to 
   change module code in run-time, code replacement, as described in Erlang 
   Reference Manual.

**Erlang リファレンスマニュアル** で説明されている通り、実行時にモジュールコードを変更したり、コードを置き換えられるというのが、Erlangというプログラミング言語の重要な機能です。

.. Based on this feature, the OTP application SASL provides a framework for 
   upgrading and downgrading between different versions of an entire release
   in run-time. This is what we call release handling.

この機能に基づいて、OTPアプリケーションSASL(System Application Support Libraries)は、実行時に、異なったバージョン間でアップグレードしたり、ダウングレードしたりできるフレームワークを提供しています。これが **リリース・ハンドリング** です。

.. The framework consists of off-line support (systools) for generating 
   scripts and building release packages, and on-line support (release_handler) 
   for unpacking and installing release packages.

このフレームワークは、スクリプトの生成やリリースパッケージといったオフラインのサポート(``systools``)と、リリースパッケージの展開とインストールといったオンラインのサポート(``release_handler``)の2つの部分から構成されています。

.. Note that the minimal system based on Erlang/OTP, enabling release handling, 
   thus consists of Kernel, STDLIB and SASL.

リリース・ハンドリングを有効にするためのErlang/OTPの最小システム構成は、Kernel、STDLIB、SASLの3つになります。

.. * A release is created as described in the previous chapter Releases. The 
     release is transferred to and installed at target environment. Refer to 
     System Principles for information of how to install the first target system.

* リリースは前の :ref:`release` の章で説明したように作成します。リリースは、対象となる環境に送信、インストールされます。最初の対象システムへのインストール方法については、 :ref:`system_principles` を参照してください。

.. * Modifications, for example error corrections, are made to the code in the 
     development environment.

* エラーの修正などのコードへの修正は、開発環境の中で行われます。

.. * At some point it is time to make a new version of release. The relevant 
     .app files are updated and a new .rel file is written.

* 何らかのタイミングで新しいバージョンのリリースを作成します。関連する ``.app`` ファイルを更新し、新しい ``.rel`` ファイルが作成されます。

.. * For each modified application, an application upgrade file, .appup, 
     is created. In this file, it is described how to upgrade and/or 
     downgrade between the old and new version of the application.

* 変更されたアプリケーションごとに、 :ref:`application_upgrade_file` ``.appup`` を作成します。このファイルには、新・旧バージョンのアプリケーション間でのアップグレード、もしくはダウングレード、もしくはその両方の方法が記述されています。

.. * Based on the .appup files, a release upgrade file called relup, is created. 
     This file describes how to upgrade and/or downgrade between the old and new 
     version of the entire release.

* ``.appup`` ファイルにしたがって、 ``relup`` と呼ばれる、 :ref:`release_upgrade_file` が作成されます。このファイルには、新・旧のリリース間で、どのようにアップグレード、もしくはダウングレード、もしくはその両方の方法が記述されています。

.. * A new release package is made and transferred to the target system.

* 新しいリリースパッケージが作成され、対象システムに転送されます。

.. * The new release package is unpacked using the release handler.

* リリース・ハンドラを利用して、新しいリリースパッケージの展開を行います。

.. * The new version of the release is installed, also using the release handler. 
     This is done by evaluating the instructions in relup. Modules may be added, 
     deleted or re-loaded, applications may be started, stopped or re-started etc. 
     In some cases, it is even necessary to restart the entire emulator.

     If the installation fails, the system may be rebooted. The old release version 
     is then automatically used.

* 新しいバージョンのリリースのインストールにも、リリース・ハンドラが利用されます。これは、 ``relup`` 内の命令を評価するによって行われます。モジュールが追加されたり、削除されたり、リロードされたり、アプリケーションが起動されたり、停止されたり、再起動されたりします。場合によっては、エミュレータ全体の再起動が必要となります。

  もしインストールが失敗すると、システムが再起動し、古いリリースバージョンが自動的に使用されます。

.. * If the installation succeeds, the new version is made the default version, 
     which should now be used in case of a system reboot.

* もしインストールが成功すると、新しいバージョンがデフォルトのバージョンとなり、システムが再起動されたときに使用されるべきバージョンとなります。

.. The next chapter, Appup Cookbook, contains examples of .appup files for typical 
   cases of upgrades/downgrades that are normally easy to handle in run-time. However, 
   there are a many aspects that can make release handling complicated. To name a few examples:

次の :ref:`appup_cookbook` の章では、実行時に簡単にアップグレード、ダウングレードを行うための、典型的な ``.appup`` ファイルのサンプルが書かれています。しかし、リリース・ハンドリングは複雑で、さまざまな側面があります。

.. * Complicated or circular dependencies can make it difficult or even impossible to decide in which order things must be done without risking run-time errors during an upgrade or downgrade. Dependencies may be:

* 依存関係が複雑であったり、循環している場合には、どの順番で行えば、実行時のエラーのリスクなく、アップグレードやダウングレードができるかというのを決定することがとても難しいか、場合によっては不可能になります。この依存関係というのは次の間に発生するものです:

  .. o between nodes,
     o between processes, and
     o between modules.

  * ノード間
  * プロセス間
  * モジュール間

.. * During release handling, non-affected processes continue normal execution. 
     This may lead to timeouts or other problems. For example, new processes 
     created in the time window between suspending processes using a certain 
     module and loading a new version of this module, may execute old code.

* リリース・ハンドリングのあいだも、影響を受けないプロセスは通常通り実行し続けます。この時、タイムアウトや、その他の問題が発生することがあります。例えば、新しいバージョンのモジュールがロードされているが、古いモジュールを使用したプロセスが現在停止中というタイミングで新しいプロセスが作られたとすると、古いコードが実行され続ける可能性があります。

.. It is therefore recommended that code is changed in as small steps as possible, 
   and always kept backwards compatible.

そのため、なるべく小さなステップで、後方互換性を維持しながらコードを変更していくことが推奨されます。

.. 11.2 Requirements

必要なもの
==========

.. For release handling to work properly, the runtime system needs to have knowledge 
   about which release it is currently running. It must also be able to change 
   (in run-time) which boot script and system configuration file should be used 
   if the system is rebooted, for example by heart after a failure. Therefore, 
   Erlang must be started as an embedded system, see Embedded System for information 
   on how to do this.

リリース・ハンドリングをうまく動かすためには、現在はどのリリースを実行しているかを、ランタイムシステムが把握している必要があります。これは、失敗時などのシステムの再起動時に、どのブートスクリプトや、システム設定ファイルを読み込むべきか、などです。これにより、実行時も含めて変更が行えるようになります。ErlangがErlangを組み込みのシステムとして起動しなければならない場合は、これをどのように行えばよいのかは、 :ref:`embedded_system` を参照してください。

.. For system reboots to work properly, it is also required that the system is 
   started with heart beat monitoring, see erl(1) and heart(3).

適切に動作させるためにシステムの再起動をする場合、ハートビートモニター付きでシステムを起動する必要があることもあります。これについては、 ``erl(1)`` と ``heart(3)`` を参照してください。

.. Other requirements:

他の要件には次のようなものがあります。

.. * The boot script included in a release package must be generated from the 
     same .rel file as the release package itself.
      
     Information about applications are fetched from the script when an upgrade 
     or downgrade is performed.

* リリースパッケージ内に含めるブートスクリプトは、リリースパッケージを作成したのと同じ ``.rel`` ファイルから作成されなければなりません。

  アップグレードやダウングレードが行われる時に、このスクリプトからアプリケーションに関する情報が収集されます。

.. * The system must be configured using one and only one system configuration 
     file, called sys.config.

     If found, this file is automatically included when a release package is created.

* システムは、 ``sys.config`` と呼ばれるただ一つのシステム設定ファイルを使って行われる必要があります。

  リリースパッケージの作成時にこのファイルが見つかると、自動的に取り込まれます。

.. * All versions of a release, except the first one, must contain a relup file.
     If found, this file is automatically included when a release package is created.

* 最初のバージョンを除くすべてのリリースバージョンは、 ``relup`` ファイルを含まなければなりません。もしリリースパッケージの作成時にこれが見つかると、自動的に取り込まれます。

.. 11.3 Distributed Systems

分散システム
============

.. If the system consists of several Erlang nodes, each node may use its own 
   version of the release. The release handler is a locally registered process 
   and must be called at each node where an upgrade or downgrade is required. 
   There is a release handling instruction that can be used to synchronize the 
   release handler processes at a number of nodes: sync_nodes. See appup(4).

システムがいくつかのErlangノードから構成されている場合、それぞれのノードは、それぞれごとに違うバージョンのリリースを使用しているかもしれません。リリース・ハンドラはローカルに登録されたプロセスで、アップグレードやダウングレードが必要なそれぞれのノード上で呼び出されなければなりません。複数のノードのリリースハンドラプロセスを同期させることが可能な命令があります。それが ``sync_nodes`` です。詳しくは ``appup(4)`` を参照してください。

.. 11.4 Release Handling Instructions

リリース・ハンドリングの説明
============================

.. OTP supports a set of release handling instructions that is used when creating 
   .appup files. The release handler understands a subset of these, the low-level 
   instructions. To make it easier for the user, there are also a number of 
   high-level instructions, which are translated to low-level instructions by 
   systools:make_relup.

OTPは ``.appup`` ファイルを作成する時に使用できる、 **リリース・ハンドリング命令** を提供しています。リリースハンドラは、それらの命令のサブセットの、 **低レベルな命令** を理解することができます。ユーザから簡単に使用できる方法としては、 **高レベルな命令** もあります。これらの命令は、 ``systools:make_relup`` によって低レベルな命令に変換されます。

.. Here, some of the most frequently used instructions are described. The complete
   list of instructions is found in appup(4).

ここでは、もっとも良く利用される命令について説明します。利用可能なすべての命令については、 ``appup(4)`` を参照してください。

.. First, some definitions:

最初にいくつかの定義を示します。

.. Residence module

レジデンスモジュール

   .. The module where a process has its tail-recursive loop function(s). If the 
      tail-recursive loop functions are implemented in several modules, all those 
      modules are residence modules for the process.

   このモジュールは、プロセスが末尾再帰ループをする関数を持ちます。もし、末尾再帰ループ関数が複数のモジュールで実装されているのであれば、プロセスから見ると、これらのモジュールはすべてレジデンスモジュールです。

.. Functional module

機能性モジュール

    .. A module which is not a residence module for any process.

    レジデンスモジュールではない、モジュールです。

.. Note that for a process implemented using an OTP behaviour, the behaviour module is 
   the residence module for that process. The callback module is a functional module.

OTPビヘイビアを利用してプロセスを実装すると、ビヘイビアモジュールがレジデンスモジュールとなります。コールバックモジュールは機能性モジュールです。

.. 11.4.1 load_module

load_module
-----------

.. If a simple extension has been made to a functional module, it is sufficient to 
   simply load the new version of the module into the system, and remove the old 
   version. This is called simple code replacement and for this the following 
   instruction is used:

もし、機能性モジュールに関する単純な拡張を行ったのであれば、システムに新しいバージョンのモジュールをロードして、古いバージョンを削除するだけで十分です。これは **シンプルコード交換** と呼ばれていて、次の命令を使用して行います。

.. code-block:: erlang

   {load_module, Module}

.. 11.4.2 update

update
------

.. If a more complex change has been made, for example a change to the 
   format of the internal state of a gen_server, simple code replacement 
   is not sufficient. Instead it is necessary to suspend the processes 
   using the module (to avoid that they try to handle any requests before 
   the code replacement is completed), ask them to transform the internal 
   state format and switch to the new version of the module, remove the old 
   version and last, resume the processes. This is called synchronized code 
   replacement and for this the following instructions are used:

gen_serverの内部ステートの形式が変更されたなど、より複雑な変更を加えた場合には、単純なコード交換では不十分です。代わりに、モジュールを使用しているプロセスを中断させ(コード交換が完了する前にリクエストを受けるのを避けるため)、内部ステートの形式を変換して、新しいバージョンのモジュールに切り替えて、最後に古いバージョンを削除し、プロセスを再開する必要があります。これは、 **同期コード交換** と呼ばれ、次の命令によって行います。

.. code-block:: erlang

   {update, Module, {advanced, Extra}}
   {update, Module, supervisor}

.. update with argument {advanced,Extra} is used when changing the internal state 
   of a behaviour as described above. It will cause behaviour processes to call 
   the callback function code_change, passing the term Extra and some other 
   information as arguments. See the man pages for the respective behaviours 
   and Appup Cookbook.

``{advanced,Extra}`` という引数を渡して ``update`` というのは、上の説明でしたように、内部ステートを変更するときに使用します。これは、ビヘイビアプロセスに対して、 ``Extra`` と、その他のいくつかの情報を引数の渡して、コールバック関数の ``code_change`` を呼び出します。詳細については、それぞれのビヘイビアのmanページと、 :ref:`appup_cookbook` を参照してください。

.. update with argument supervisor is used when changing the start specification 
   of a supervisor. See Appup Cookbook.

スーパバイザの起動の仕様が変わる場合には、引数付きで、スーパバイザに対して ``update`` が使用されます。 :ref:`appup_cookbook` を参照してください。

.. The release handler finds the processes using a module to update by traversing 
   the supervision tree of each running application and checking all the child 
   specifications:

リリースハンドラは、現在実行中のアプリケーションの監視ツリーを探索し、次に挙げる形式の、すべての子アプリケーションの仕様を調べて、アップデートするモジュールを使用しているプロセスを探します。

.. code-block:: erlang

   {Id, StartFunc, Restart, Shutdown, Type, Modules}

.. A process is using a module if the name is listed in Modules in the child 
   specification for the process.

もし、子プロセスの仕様の中の ``Modules`` に、リストアップされている中にモジュールの名前があれば、そのプロセスは更新予定のモジュールを仕様しているということになります。

.. If Modules=dynamic, which is the case for event managers, the event manager 
   process informs the release handler about the list of currently installed 
   event handlers (gen_fsm) and it is checked if the module name is in this 
   list instead.

イベントマネージャの場合には、 ``Modules=dynamic`` となります。この場合は、イベントマネージャプロセスは、現在インストールされているイベントハンドラ(gen_fsm)のリストをリリースハンドラに知らせ、更新予定のモジュール名がそのリストに含まれているかどうかチェックします。

.. The release handler suspends, asks for code change, and resumes processes 
   by calling the functions sys:suspend/1,2, sys:change_code/4,5 and 
   sys:resume/1,2 respectively.

リリースハンドラは ``sys:suspend/1,2`` 、 ``sys:change_code/4,5`` 、 ``sys:resume/1,2`` を呼び出すことで、プロセスを停止させ、コードの変更の問い合わせを行い、レジュームを行います。

.. 11.4.3 add_module and delete_module

add_moduleとdelete_module
-------------------------

.. If a new module is introduced, the following instruction is used:

新しいモジュールが導入される場合には、次の命令が使用されます。

.. code-block:: erlang

   {add_module, Module}

.. The instruction loads the module and is absolutely necessary when running Erlang 
   in embedded mode. It is not strictly required when running Erlang in interactive 
   (default) mode, since the code server automatically searches for and loads 
   unloaded modules.

この命令はモジュールをロードします。この命令は、Erlangが組み込みモードでで実行しているときは絶対に必要となります。デフォルトの対話モードでErlangを実行しているときは、コードサーバが自動的にロードされていないモジュールをロードしにいくため、厳密には不要です。

.. The opposite of add_module is delete_module which unloads a module:

``add_module`` の反対が ``delete_module`` です。これはモジュールをアンロードします。

.. code-block:: erlang

   {delete_module, Module}

.. Note that any process, in any application, with Module as residence module, 
   is killed when the instruction is evaluated. The user should therefore ensure 
   that all such processes are terminated before deleting the module, to avoid a 
   possible situation with failing supervisor restarts.

この命令が実行される時は、モジュールがレジデンス・モードの場合、どんなアプリケーション内であっても、プロセスがキルされることに注意してください。ユーザは、モジュールの削除前にそのようなプロセスをすべて終了させて、スーパバイザの再起動時に失敗するという状況をなるべく避けるようにすべきです。

.. 11.4.4 Application Instructions

アプリケーション命令
--------------------

.. Instruction for adding an application:

アプリケーションを追加する際に使用する命令は次の通りです。

.. code-block:: erlang

   {add_application, Application}

.. Adding an application means that the modules defined by the modules key in the
   .app file are loaded using a number of add_module instructions, then the 
   application is started.

アプリケーションの追加が行われると、 ``.app`` ファイルの ``modules`` キーで定義されたモジュールに対して、 ``add_module`` 命令を使ってロードし、その後、アプリケーションが起動します。

.. Instruction for removing an application:

アプリケーションの削除には次の命令を使います。

.. code-block:: erlang

   {remove_application, Application}

.. Removing an application means that the application is stopped, the modules are 
   unloaded using a number of delete_module instructions and then the application 
   specification is unloaded from the application controller.

アプリケーションの削除が行われると、アプリケーションが停止され、 ``delete_module`` 命令を使用してモジュールをアンロードし、最後にアプリケーション仕様をアプリケーションコントローラから削除します。

.. Instruction for removing an application:

アプリケーションの再起動には次の命令を使います。

.. code-block:: erlang

   {restart_application, Application}

.. Restarting an application means that the application is stopped and then 
   started again similar to using the instructions remove_application and 
   add_application in sequence.

アプリケーションの再起動を行うと、ちょうど ``remove_application`` と ``add_application`` を連続して使用したように、アプリケーションが停止され、その後再起動します。

.. 11.4.5 apply (low-level)

apply(低レベル)
---------------

.. To call an arbitrary function from the release handler, the following instruction is used:

リリースハンドラから何らかの関数を呼ぶには、次の命令が使用されます。

.. code-block:: erlang

   {apply, {M, F, A}}

.. The release handler will evalute apply(M, F, A).

リリースハンドラは ``apply(M, F, A)`` という式を評価します。

.. 11.4.6 restart_new_emulator (low-level)

restart_new_emulator(低レベル)
------------------------------

.. This instruction is used when changing to a new emulator version, or if a system 
   reboot is needed for some other reason. Requires that the system is started with 
   heart beat monitoring, see erl(1) and heart(3).

この命令はエミュレータのバージョンを更新したり、なんらかの理由でシステムの再起動が必要になった時に使用されます。このためには、ハートビートモニターを使って再起動を行う必要があります。詳しくは ``erl(1)`` と ``heart(3)`` を参照してください。

.. When the release handler encounters the instruction, it shuts down the current 
   emulator by calling init:reboot(), see init(3). All processes are terminated 
   gracefully and the system can then be rebooted by the heart program, using the 
   new release version. This new version must still be made permanent when the new 
   emulator is up and running. Otherwise, the old version is used in case of a new 
   system reboot.

リリースハンドラがこの命令に遭遇すると、 ``init:reboot()`` を呼び出して、現在のエミュレータを停止します。詳しくは ``init(3)`` を参照してください。すべてのプロセスが正常通り終了したら、 :program:`heart` プログラムによってシステムが再起動され、新しいリリースバージョンが使用されます。新しいエミュレータが起動して実行されている場合には、新バージョンを永続化しなければなりません。そうでないと、新システムのリブートに古いバージョンが使用されてしまいます。

.. On UNIX, the release handler tells the heart program which command to use to 
   reboot the system. Note that the environment variable HEART_COMMAND, normally 
   used by the heart program, in this case is ignored. The command instead defaults 
   to $ROOT/bin/start. Another command can be set by using the SASL configuration 
   parameter start_prg, see sasl(6).

UNIX上では、リリースハンドラは、 :program:`heart` プログラムに対して、システムのリブートに使うコマンドを伝えます。通常は ``HEART_COMMAND`` 環境変数が使用されますが、この場合は無視されます。このコマンドのデフォルトは ``$ROOT/bin/start`` です。SASLの設定パラメータの ``start_prg`` を使用すると、他のコマンドを使用することができます。詳しくは、 ``sasl(6)`` を参照してください。

.. 11.5 Application Upgrade File

アプリケーション・アップグレードファイル
========================================

.. To define how to upgrade/downgrade between the current version and previous 
   versions of an application, we create an application upgrade file, or in 
   short .appup file. The file should be called Application.appup, where 
   Application is the name of the application:

現在のバージョンと、以前のバージョン間でアップグレード、ダウングレードを行う場合には、アプリケーション・アップグレードファイルを作成する必要があります。これは短く、 ``.appup`` ファイルとも呼ばれます。ファイル名は ``Application.appup`` という名前になります。この ``Application`` の部分はアプリケーション名になります。

.. code-block:: erlang

   {Vsn,
    [{UpFromVsn1, InstructionsU1},
     ...,
     {UpFromVsnK, InstructionsUK}],
    [{DownToVsn1, InstructionsD1},
     ...,
     {DownToVsnK, InstructionsDK}]}.

.. Vsn, a string, is the current version of the application, as defined in the 
   .app file. Each UpFromVsn is a previous version of the application to upgrade 
   from, and each DownToVsn is a previous version of the application to downgrade 
   to. Each Instructions is a list of release handling instructions.

``Vsn`` は文字列型で、 ``.app`` ファイルに記述された、現在のバージョンです。それぞれの ``UpFromVsn`` はアップグレード元のアプリケーションのバージョンです。また、 ``DownToVsn`` は、ダウングレード先のバージョンです。それぞれの命令は、リリースハンドリングの命令のリストになります。

.. The syntax and contents of the appup file are described in detail in appup(4).

``appup`` ファイルの文法や内容の詳細は ``appup(4)`` で説明されている。

.. In the chapter Appup Cookbook, examples of .appup files for typical 
   upgrade/downgrade cases are given.

:ref:`appup_cookbook` の章では、様々なアップグレード/ダウングレードの状況別の、 ``.appup`` ファイルのサンプルが紹介されています。

.. Example: Consider the release ch_rel-1 from the Releases chapter. Assume we want 
   to add a function available/0 to the server ch3 which returns the number of 
   available channels:

サンプル: :ref:`release` の章で紹介した ``ch_rel-1`` を使って紹介します。

.. (Hint: When trying out the example, make the changes in a copy of the original 
   directory, so that the first versions are still available.)

.. note::

   このサンプルを実行する場合には、オリジナルのディレクトリのコピーを行って、その中で変更を加えるようにしてください。そうすれば、元のバージョンを残すことができます。

.. code-block:: erlang

   -module(ch3).
   -behaviour(gen_server).

   -export([start_link/0]).
   -export([alloc/0, free/1]).
   -export([available/0]).
   -export([init/1, handle_call/3, handle_cast/2]).

   start_link() ->
       gen_server:start_link({local, ch3}, ch3, [], []).

   alloc() ->
       gen_server:call(ch3, alloc).

   free(Ch) ->
       gen_server:cast(ch3, {free, Ch}).

   available() ->
       gen_server:call(ch3, available).

   init(_Args) ->
       {ok, channels()}.

   handle_call(alloc, _From, Chs) ->
       {Ch, Chs2} = alloc(Chs),
       {reply, Ch, Chs2};
   handle_call(available, _From, Chs) ->
       N = available(Chs),
       {reply, N, Chs}.

   handle_cast({free, Ch}, Chs) ->
       Chs2 = free(Ch, Chs),
       {noreply, Chs2}.

.. A new version of the ch_app.app file must now be created, where the version is updated:

次に新しいバージョンの ``ch_app.app`` ファイルを作る必要があります。バージョン番号が更新されています。

.. code-block:: erlang

   {application, ch_app,
    [{description, "Channel allocator"},
     {vsn, "2"},
     {modules, [ch_app, ch_sup, ch3]},
     {registered, [ch3]},
     {applications, [kernel, stdlib, sasl]},
     {mod, {ch_app,[]}}
    ]}.

.. To upgrade ch_app from "1" to "2" (and to downgrade from "2" to "1"), we simply need 
   to load the new (old) version of the ch3 callback module. We create the application 
   upgrade file ch_app.appup in the ebin directory:

``ch_app`` を1から2にアップグレード(もしくは2から1にダウングレード)する場合は、単に新(もしくは旧)の ``ch3`` コールバックモジュールをロードするだけです。 :file:`ebin` ディレクトリの中に、 :file:`ch_app.appup` アップグレードファイルを作ります。

.. code-block:: erlang

   {"2",
    [{"1", [{load_module, ch3}]}],
    [{"1", [{load_module, ch3}]}]
   }.

.. 11.6 Release Upgrade File

.. _release_upgrade_file:

リリース・アップグレードファイル
================================

.. To define how to upgrade/downgrade between the new version and previous 
   versions of a release, we create a release upgrade file, or in short relup file.

リリースの現在のバージョンと前のバージョン間で、アップグレード/ダウングレードする方法を定義するには、リリース・アップグレードファイルを作成します。これは短縮して、 ``relup`` ファイルと呼ばれます。

.. This file does not need to be created manually, it can be generated by 
   systools:make_relup/3,4. The relevant versions of the .rel file, .app files 
   and .appup files are used as input. It is deducted which applications should 
   be added and deleted, and which applications that need to be upgraded and/or 
   downgraded. The instructions for this is fetched from the .appup files and
   transformed into a single list of low-level instructions in the right order.

このファイルは、 ``systools:make_relup/3,4`` が作成するため、手動で作る必要はありません。それぞれのバージョンの ``.rel`` ファイル、 ``.app`` ファイル、 ``.appup`` ファイルが入力として使用されます。どのアプリケーションを追加/削除すべきか、どのアプリケーションのアップグレード/ダウングレードが必要なのか、という情報を抽出する必要があります。これに対する命令が ``.appup`` から取得され、正しく並んでいる単一の低レベルの命令のリストに変換されます。

.. If the relup file is relatively simple, it can be created manually. Remember 
   that it should only contain low-level instructions.

もし ``relup`` ファイルが比較的シンプルであれば、手動で作ることもできます。ただし、それには低レベルの命令だけしか含められないことに注意してください。

.. The syntax and contents of the release upgrade file are described in detail in relup(4).

リリースアップグレードファイルの構文と内容については、 ``relup(4)`` で詳細に説明しています。

.. Example, continued from the previous section. We have a new version "2" of ch_app 
   and an .appup file. We also need a new version of the .rel file. This time the 
   file is called ch_rel-2.rel and the release version string is changed changed 
   from "A" to "B":

例えば、前のセクションのサンプルについて、 ``ch_app`` の新バージョンの2と、 ``.appup`` ファイルを持っていたとします。新しいバージョンの ``.rel`` ファイルが必要になります。このファイルを ``ch_rel-2.rel`` という名前にしたとして、リリースバージョン文字列を ``A`` から ``B`` に変えます。

.. code-block:: erlang

   {release,
    {"ch_rel", "B"},
    {erts, "5.3"},
    [{kernel, "2.9"},
     {stdlib, "1.12"},
     {sasl, "1.10"},
     {ch_app, "2"}]
   }.

.. Now the relup file can be generated:

``relup`` ファイルが生成できるようになりました。

.. code-block:: erlang

   1> systools:make_relup("ch_rel-2", ["ch_rel-1"], ["ch_rel-1"]).
   ok

.. This will generate a relup file with instructions for how to upgrade from version "A" 
   ("ch_rel-1") to version "B" ("ch_rel-2") and how to downgrade from version "B" to 
   version "A".

この関数を起動することで、 バージョンA(``ch_rel-1``)からバージョンB(``ch_rel-2``)にアップグレードする命令と、バージョンBからバージョンAにダウングレードする命令を含む ``relup`` ファイルが生成されます。

.. Note that both the old and new versions of the .app and .rel files must be in the code 
   path, as well as the .appup and (new) .beam files. It is possible to extend the code 
   path by using the option path:

``.app`` と ``.rel`` の新旧の両バージョンとも、 ``.appup`` や新しい ``.beam`` ファイルと一緒に、コードのパスのなかに置かなければなりません。オプションのパスを設定することで、コードのパスを増やすことができます。

.. code-block:: erlang

   1> systools:make_relup("ch_rel-2", ["ch_rel-1"], ["ch_rel-1"],
   [{path,["../ch_rel-1",
   "../ch_rel-1/lib/ch_app-1/ebin"]}]).
   ok

.. 11.7 Installing a Release

リリースのインストール
======================

.. When we have made a new version of a release, a release package can be created 
   with this new version and transferred to the target environment.

新しいバージョンのリリースを作ると、リリースパッケージを作って、ターゲット環境に送信することができます。

.. To install the new version of the release in run-time, the release handler 
   is used. This is a process belonging to the SASL application, that handles 
   unpacking, installation, and removal of release packages. It is interfaced 
   through the module release_handler, which is described in detail in 
   release_handler(3).

新しいバージョンのリリースを実行時にインストールするには、リリースハンドラを仕様します。これは、SASLアプリケーションに属したプロセスで、パッケージの展開、インストール、リリースパッケージの削除などを行います。 ``release_handler`` モジュールがこの機能のインタフェースになっています。これについては、 ``release_handler(3)`` を参照してください。

.. Assuming there is a target system up and running with installation root directory 
   $ROOT, the release package with the new version of the release should be copied 
   to $ROOT/releases.

ターゲットシステムのインストールのルートディレクトリは :file:`$ROOT` であると想定して処理が行われます。新しいバージョンのリリースを含めたリリースパッケージは :file:`$ROOT` のコピーされなければなりません。

.. The first action is to unpack the release package, the files are then extracted 
   from the package:

最初のアクションは、リリースパッケージの展開です。まずはパッケージからファイルを抽出します。

.. code-block:: erlang

   release_handler:unpack_release(ReleaseName) => {ok, Vsn}

.. ReleaseName is the name of the release package except the .tar.gz extension. 
   Vsn is the version of the unpacked release, as defined in its .rel file.

``ReleaseName`` は、リリースパッケージの名前から :file:`.tar.gz` という拡張子外した名前です。 ``Vsn`` は、展開したリリースの ``.rel`` ファイルのバージョンです。

.. A directory $ROOT/lib/releases/Vsn will be created, where the .rel file, the 
   boot script start.boot, the system configuration file sys.config and relup are 
   placed. For applications with new version numbers, the application directories 
   will be placed under $ROOT/lib. Unchanged applications are not affected.

:file:`$ROOT/lib/releases/Vsn` というディレクトリが作られ、 ``.rel`` ファイルやブートスクリプトの ``start.boot`` 、システム構成ファイルの :file:`sys.config` 、`file:`relup`` が置かれます。バージョン番号ごとのアプリケーションのために、 :file:`$ROOT/lib` 以下にアプリケーションディレクトリが置かれます。変更されていないアプリケーションには適用されません。

.. An unpacked release can be installed. The release handler then evaluates the 
   instructions in relup, step by step:

展開されたリリースはインストールすることができます。リリースハンドラは、 ``relup`` 中に書かれた命令を評価して行きます。

.. code-block:: erlang

   release_handler:install_release(Vsn) => {ok, FromVsn, []}

.. If an error occurs during the installation, the system is rebooted using the 
   old version of the release. If installation succeeds, the system is afterwards 
   using the new version of the release, but should anything happen and the 
   system is rebooted, it would start using the previous version again. To be 
   made the default version, the newly installed release must be made permanent, 
   which means the previous version becomes old:

もしインストール中にエラーが発生すると、古いバージョンのリリースを使って再起動を行います。もしインストールが成功すると、新しいバージョンのリリースを使うようになりますが、何かが発生してシステムを再起動すると、以前のバージョンを使って起動してしまいます。この新しいバージョンをデフォルトのバージョンに設定するために、新しいバージョンを永続化しなければなりません。こうすると、以前のバージョンは古いバージョンとなります。

.. code-block:: erlang

   release_handler:make_permanent(Vsn) => ok

.. The system keeps information about which versions are old and permanent in the 
   files $ROOT/releases/RELEASES and $ROOT/releases/start_erl.data.

システムは、 :file:`$ROOT/releases/RELEASES` と :file:`$ROOT/releases/start_erl.data` の中に、どのバージョンが古く、どのバージョンが永続化されているのか、という情報を保持しています。

.. To downgrade from Vsn to FromVsn, install_release must be called again:

``Vsn`` から ``FromVsn`` にダウングレードする場合は、 ``install_release`` を再び呼び出します。

.. code-block:: erlang

   release_handler:install_release(FromVsn) => {ok, Vsn, []}

.. An installed, but not permanent, release can be removed. Information about the 
   release is then deleted from $ROOT/releases/RELEASES and the release specific 
   code, that is the new application directories and the $ROOT/releases/Vsn 
   directory, are removed.

インストールされているが、永続化されていないリリースは削除することができます。リリースに対する情報は :file:`ROOT/releases/RELEASES` から削除され、新しいアプリケーションのディレクトリと、 :file:`$ROOT/releases/Vsn` に格納されている、リリースに関連するコードも削除されます。

.. code-block:: erlang

   release_handler:remove_release(Vsn) => ok

.. Example, continued from the previous sections:

サンプル: 前のセクションからの続きのサンプルで説明をします。

.. 1) Create a target system as described in System Principles of the 
   first version "A" of ch_rel from the Releases chapter. This time 
   sys.config must be included in the release package. If no configuration 
   is needed, the file should contain the empty list:

1. **システム原則** で説明した通りに、:ref:`release` の章で説明した ``ch_rel`` の最初のバージョン"A"を作成します。今回は、 :file:`sys.config` がリリースパッケージに含まれていなければなりません。もし設定するものがなければ、ファイルには空のリストを書きます。

   .. code-block:: erlang

      [].

.. 2) Start the system as a simple target system. Note that in reality, it should 
   be started as an embedded system. However, using erl with the correct boot 
   script and .config file is enough for illustration purposes:

2. シンプルなターゲットのシステムから、システムをスタートさせます。実際には、組み込みのシステムから開始します。ですが、説明するという目的でいえば、適切なブートスクリプトと、 :file:`.config` ファイルがあれば十分です。

   .. code-block:: bash

      % cd $ROOT
      % bin/erl -boot $ROOT/releases/A/start -config $ROOT/releases/A/sys
      ...

   .. $ROOT is the installation directory of the target system.

   :file:`$ROOT` はターゲットシステムのインストールディレクトリです。

.. 3) In another Erlang shell, generate start scripts and create a release 
   package for the new version "B". Remember to include (a possible updated) 
   sys.config and the relup file, see Release Upgrade File above.

3. 他のErlangシェルを起動して、スタートスクリプトを作成し、新しいバージョン"B"のリリースパッケージを作成します。これには、 :file:`sys.config` と、 :file:`relup` ファイル(できれば更新して)を含めるようにしてください。詳しくは :ref:`release_upgrade_file` を参照してください。

   .. code-block:: erlang

      1> systools:make_script("ch_rel-2").
      ok
      2> systools:make_tar("ch_rel-2").
      ok

   .. The new release package now contains version "2" of ch_app and the relup file as well:

   新しいリリースパッケージに、 ``ch_app`` のバージョン"2"と、 :file:`relup` ファイルが含まれました。

   .. code-block:: bash

      % tar tf ch_rel-2.tar 
      lib/kernel-2.9/ebin/kernel.app
      lib/kernel-2.9/ebin/application.beam
      ...
      lib/stdlib-1.12/ebin/stdlib.app
      lib/stdlib-1.12/ebin/beam_lib.beam
      ...      
      lib/sasl-1.10/ebin/sasl.app
      lib/sasl-1.10/ebin/sasl.beam
      ...
      lib/ch_app-2/ebin/ch_app.app
      lib/ch_app-2/ebin/ch_app.beam
      lib/ch_app-2/ebin/ch_sup.beam
      lib/ch_app-2/ebin/ch3.beam
      releases/B/start.boot
      releases/B/relup
      releases/B/sys.config
      releases/ch_rel-2.rel

.. 4) Copy the release package ch_rel-2.tar.gz to the $ROOT/releases directory.

4. リリースパッケージ :file:`ch_rel-2.tar.gz` を :file:`$ROOT/releases` ディレクトリにコピーします。

.. 5) In the running target system, unpack the release package:

5. 起動中のターゲットシステムの中で、リリースパッケージを展開します。

   .. code-block:: erlang

      1> release_handler:unpack_release("ch_rel-2").
      {ok,"B"}

   .. The new application version ch_app-2 is installed under $ROOT/lib next to ch_app-1. 
      The kernel, stdlib and sasl directories are not affected, as they have not changed.

   新しいアプリケーションバージョン ``ch_app-2`` が :file:`$ROOT/lib` の ``ch_app-1`` の隣にインストールされます。 ``kernel`` 、 ``stdlib`` 、 ``sasl`` などは変更されていないため、このディレクトリは影響を受けません。

   .. Under $ROOT/releases, a new directory B is created, containing ch_rel-2.rel, 
      start.boot, sys.config and relup.

   :file:`$ROOT/releases` 以下には、新しい :file:`B` というディレクトリが作られ、その中に :file:`ch_rel-2.rel` 、 :file:`start.boot` 、 :file:`sys.config` 、 :file:`relup` が格納されます。

.. 6) Check if the function ch3:available/0 is available:

6. ``ch3:available/0`` 関数が呼び出せるか確認をします。

   .. code-block:: erlang

      2> ch3:available().
      ** exception error: undefined function ch3:available/0

.. 7) Install the new release. The instructions in $ROOT/releases/B/relup are 
      executed one by one, resulting in the new version of ch3 being loaded. 
      The function ch3:available/0 is now available:

7. 新しいリリースをインストールします。 :file:`$ROOT/releases/B/relup` に含まれている命令が1つずつ実行され、 ``ch3`` の新バージョンがロードされて、 ``ch3:available/0`` 関数が利用可能になります。

   .. code-block:: erlang

      3> release_handler:install_release("B").
      {ok,"A",[]}
      4> ch3:available().
      3
      5> code:which(ch3).
      ".../lib/ch_app-2/ebin/ch3.beam"
      6> code:which(ch_sup).
      ".../lib/ch_app-1/ebin/ch_sup.beam"

   .. Note that processes in ch_app for which code have not been updated, for 
      example the supervisor, are still evaluating code from ch_app-1.

   この時点では、 ``ch_app`` 内のプロセスはまだアップデートされません。スーパバイザのサンプルと同じく、この時点では、まだ ``ch_app-1`` のコードを評価しています。

.. 8) If the target system is now rebooted, it will use version "A" again. The "B" version 
   must be made permanent, in order to be used when the system is rebooted.

8. このときにターゲットのシステムがリブートされても、まだバージョンAが使用されます。システムの再起動時にバージョンBが使われるようにするには、永続化しなければなりません。

   .. code-block:: erlang

      7> release_handler:make_permanent("B").
      ok

.. 11.8 Updating Application Specifications

.. _updating_application_specifications:

アプリケーション仕様のアップデート
==================================

.. When a new version of a release is installed, the application specifications 
   are automatically updated for all loaded applications.

新しいバージョンのリリースがインストールされる時は、ロードされているすべてのアプリケーションのアプリケーション仕様が自動的に更新されます。

.. note::

   .. The information about the new application specifications are fetched from the boot script
      included in the release package. It is therefore important that the boot script is
      generated from the same .rel file as is used to build the release package itself.

   新しいアプリケーション仕様の情報はリリースパッケージに含まれる、ブートスクリプトが読み込みに行きます。そのため、ブートスクリプトはリリースパッケージを作ったときに使用されたのと同じ ``.rel`` ファイルが使用されます。

.. Specifically, the application configuration parameters are automatically updated 
   according to (in increasing priority order):

アプリケーション設定パラメーターは、次のような優先度で自動的に更新されます。

.. 1. The data in the boot script, fetched from the new application resource file App.app
   2. The new sys.config
   3. Command line arguments -App Par Val

1. 新しいアプリケーションのリソースファイル(:file:`App.app`)から取得された、ブートスクリプト内のデータ
2. 新しい :file:`sys.config`
3. コマンドライン引数の ``-App Par Val``

.. This means that parameter values set in the other system configuration files, 
   as well as values set using application:set_env/3, are disregarded.

パラメーター値は、他のシステム設定ファイルや、 ``application:set_env/3`` を使用して設定された値は無視されます。

.. When an installed release is made permanent, the system process init is set 
   to point out the new sys.config.

インストールされたリリースが永続化される時に、システムプロセスの ``init`` は、新しい :file:`sys.config` を見に行くように設定されます。

.. After the installation, the application controller will compare the old and 
   new configuration parameters for all running applications and call the 
   callback function:

インストールされると、アプリケーションコントローラーは、実行中のすべてのアプリケーションの新旧の設定パラメーターを比較して、コールバック関数を呼び出します。

.. code-block:: erlang

   Module:config_change(Changed, New, Removed)

.. Module is the application callback module as defined by the mod key in the 
   .app file. Changed and New are lists of {Par,Val} for all changed and added 
   configuration parameters, respectively. Removed is a list of all parameters 
   Par that have been removed.

``Module`` は、 ``.app`` ファイルの ``mod`` キーで定義された、アプリケーションのコールバックモジュールです。 ``Changed`` と ``New`` には、 ``{Par, Val}`` という形式のリストで、すべての設定値の変更や追加を個別に書かれます。 ``Removed`` には、すべての削除される ``Par`` のリストです。

.. The function is optional and may be omitted when implementing an application callback module.

この関数はオプションなので、アプリケーションのコールバックモジュールに実装するかどうかは任意で、省略することも可能です。

Copyright (c) 1991-2009 Ericsson AB
