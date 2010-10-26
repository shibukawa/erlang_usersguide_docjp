.. 10 Releases

.. _releases:

========
リリース
========

.. This chapter should be read in conjuction with rel(4), systools(3) and script(4).

この章を読む場合には、rel(4)、systools(3)、script(4)も合わせてお読みください。

.. 10.1 Release Concept

リリースの概念
==============

.. When we have written one or more applications, we might want to create a 
   complete system consisting of these applications and a subset of the 
   Erlang/OTP applications. This is called a release.

もしアプリケーションをいくつか作成したのであれば、これらのアプリケーションや、Erlang/OTPアプリケーションのサブセットから構成される、完全なシステムを作りたいと思うでしょう。これは **リリース** と呼ばれます。

.. To do this, we create a release resource file which defines which applications are included in the release.

これを行うためには、どのアプリケーションをリリースに含めるかを定義した、 :ref:`release_resource_file` を作成します。

.. The release resource file is used to generate boot scripts and release packages.
   A system which is transfered to and installed at another site is called a target
   system. How to use a release package to create a target system is described in
   System Principles.

:ref:`release_resource_file` は :ref:`boot_script` と :ref:`release_package` を作るのに使用されます。

.. 10.2 Release Resource File

.. _release_resource_file:

リリース・リソースファイル
==========================

.. To define a release, we create a release resource file, or in short .rel file, 
   where we specify the name and version of the release, which ERTS version it is 
   based on, and which applications it consists of:

リリースを定義するには、リリース・リソースファイル(.relファイルとも呼ばれる)を作成します。このファイルにはリリースの名前、バージョン、どのERTSバージョンを元にしているか、どのアプリケーションから構成されるのか、を定義していきます。

.. code-block:: erlang

   {release, {Name,Vsn}, {erts, EVsn},
    [{Application1, AppVsn1},
      ...
     {ApplicationN, AppVsnN}]}.

.. The file must be named Rel.rel, where Rel is a unique name.

ファイル名は ``Rel.rel`` という名前にします。 ``Rel`` はユニークな名前にします。

.. Name, Vsn and Evsn are strings.

``Name``, ``Vsn``, ``Evsn`` には文字列を設定します。

.. Each Application (atom) and AppVsn (string) is the name and version of an 
   application included in the release. Note the the minimal release based on 
   Erlang/OTP consists of the kernel and stdlib applications, so these 
   applications must be included in the list.

それぞれの ``Application`` (アトム)と ``AppVsn`` (文字列)は、リリースに含まれるアプリケーションの名前とバージョンをsて呈します。Erlang/OTPのリリースを構成する最小のものは、 ``kernel`` と ``stdlib`` アプリケーションであるため、これらのアプリケーションは必ずリストに含めなければなりません。

.. Example: We want to make a release of ch_app from the Applications chapter. 
   It has the following .app file:

それでは、 :ref:`applications: の章で紹介した ``ch_app`` のリリースをしたいと思ったとします。この場合、次のような ``.app`` ファイルになります。

.. code-block:: erlang

   {application, ch_app,
    [{description, "Channel allocator"},
     {vsn, "1"},
     {modules, [ch_app, ch_sup, ch3]},
     {registered, [ch3]},
     {applications, [kernel, stdlib, sasl]},
     {mod, {ch_app,[]}}
    ]}.

.. The .rel file must also contain kernel, stdlib and sasl, since these 
   applications are required by ch_app. We call the file ch_rel-1.rel:

``ch_app`` の実行に必要であるため、 ``.rel`` ファイルには ``kernel`` と ``stdlib`` 、 ``sasl`` を含めなければなりません。次のような ``ch_rel-1.rel`` ファイルを作成することになります。

.. code-block:: erlang

   {release,
    {"ch_rel", "A"},
    {erts, "5.3"},
    [{kernel, "2.9"},
     {stdlib, "1.12"},
     {sasl, "1.10"},
     {ch_app, "1"}]
   }.

.. 10.3 Generating Boot Scripts

.. _boot_script:

ブートスクリプトの作成
======================

.. There are tools in the SASL module systools available to build and check releases. 
   The functions read the .rel and .app files and performs syntax and dependency checks. 
   The function systools:make_script/1,2 is used to generate a boot script 
   (see System Principles).

リリースのビルドとチェックに使えるツールが、SASLモジュール systoolsに含まれます。これらの関数は、 ``.rel`` 、 ``.app`` ファイルを読み込み、文法の評価と依存性チェックをします。 ``systools:make_script/1,2`` という関数が、ブートスクリプトの生成に利用されます。 (:ref:`system_principles` 参照)

.. code-block:: erlang

   1> systools:make_script("ch_rel-1", [local]).
   ok

.. This creates a boot script, both the readable version ch_rel-1.script and the binary 
   version used by the runtime system, ch_rel-1.boot. "ch_rel-1" is the name of the .rel 
   file, minus the extension. local is an option that means that the directories where 
   the applications are found are used in the boot script, instead of $ROOT/lib. ($ROOT
   is the root directory of the installed release.) This is a useful way to test a 
   generated boot script locally.

この関数は、人が読むことができるバージョンの ``ch_rel-1.script``というファイルと、ランタイムシステムから使用される、バイナリバージョンの ``ch_rel-1.boot`` の、両方の形式のファイルが生成されます。 ``"ch_rel-1"`` という名前は、 ``.rel`` ファイルから、拡張子を取った名前です。オプションの ``local`` を指定すると、 ``$ROOT/lib`` によらず、ブートスクリプトのあるディレクトリから、アプリケーションが探索されます。(``$ROOT`` リリースがインストールされたディレクトリのルートです。これは、生成されたブートスクリプトをローカルでテストするには便利な方法です。

.. When starting Erlang/OTP using the boot script, all applications from the .rel file 
   are automatically loaded and started:
   
ブートスクリプトを使用してErlang/OTPを起動する場合、 ``.rel`` ファイルに含まれるすべてのアプリケションを自動的にロードして、スタートします。

.. code-block:: erlang

   % erl -boot ch_rel-1
   Erlang (BEAM) emulator version 5.3

   Eshell V5.3  (abort with ^G)
   1> 
   =PROGRESS REPORT==== 13-Jun-2003::12:01:15 ===
             supervisor: {local,sasl_safe_sup}
                started: [{pid,<0.33.0>},
                          {name,alarm_handler},
                          {mfa,{alarm_handler,start_link,[]}},
                          {restart_type,permanent},
                          {shutdown,2000},
                          {child_type,worker}]

   ...

   =PROGRESS REPORT==== 13-Jun-2003::12:01:15 ===
            application: sasl
             started_at: nonode@nohost

   ...
   =PROGRESS REPORT==== 13-Jun-2003::12:01:15 ===
            application: ch_app
             started_at: nonode@nohost

.. 10.4 Creating a Release Package

.. _release_package:

リリースパッケージの作成
========================

.. There is a function systools:make_tar/1,2 which takes a .rel file as input and 
   creates a zipped tar-file with the code for the specified applications, 
   a release package.

``.rel`` ファイルを入力に取り、特定のアプリケーションのコードをtar.gzにまとめて、リリースパッケージを作成する、 ``systools:make_tar/1,2`` 関数があります。

.. code-block:: erlang

   1> systools:make_script("ch_rel-1").
   ok
   2> systools:make_tar("ch_rel-1").
   ok

.. The release package by default contains the .app files and object code for 
   all applications, structured according to the application directory structure, 
   the binary boot script renamed to start.boot, and the .rel file.

リリースパッケージには、デフォルトで ``.app`` ファイルと、すべてのアプリケーションに関する、すべてのオブジェクトコードが含まれ、アプリケーションのディレクトリ構造に従って格納されます。バイナリのブートスクリプトは ``start.boo`` にリネームされて含まれます。また、 ``.rel`` ファイルも格納されます。

.. code-block:: bash

   % tar tf ch_rel-1.tar
   lib/kernel-2.9/ebin/kernel.app
   lib/kernel-2.9/ebin/application.beam
   ...
   lib/stdlib-1.12/ebin/stdlib.app
   lib/stdlib-1.12/ebin/beam_lib.beam
   ...
   lib/sasl-1.10/ebin/sasl.app
   lib/sasl-1.10/ebin/sasl.beam
   ...
   lib/ch_app-1/ebin/ch_app.app
   lib/ch_app-1/ebin/ch_app.beam
   lib/ch_app-1/ebin/ch_sup.beam
   lib/ch_app-1/ebin/ch3.beam
   releases/A/start.boot
   releases/ch_rel-1.rel

.. Note that a new boot script was generated, without the local option set, 
   before the release package was made. In the release package, all application 
   directories are placed under lib. Also, we do not know where the release 
   package will be installed, so we do not want any hardcoded absolute paths 
   in the boot script here.

これを実行すると、リリースパッケージの作成前に ``local`` オプションなしで新しいブートスクリプトが生成されます。リリースパッケージ内では、すべてのアプリケーションディレクトリは ``lib`` ディレクトリの中に配置されます。リリースパッケージがどのディレクトリにインストールされるかは知りませんが、このブートスクリプトには絶対パスをハードコードする必要はありません。

.. If a relup file and/or a system configuration file called sys.config is found, 
   these files are included in the release package as well. See Release Handling.

もし、 ``relup`` ファイルと、 ``sys.config`` という名前のシステム設定ファイルの両方、もしくはどちらかが見つかった場合には、これらのファイルも同じようにリリースパッケージに含まれます。これについては :ref:`release_handling` を参照してください。

.. Options can be set to make the release package include source code and the 
   ERTS binary as well.

リリースパッケージに、ソースコードやERTSバイナリも同じように含めるようなオプションもあります。

.. Refer to System Principles for how to install the first target system, 
   using a release package, and to Release Handling for how to install 
   a new release package in an existing system.

リリースパッケージを使用して、最初のターゲットシステムのインストールを行う方法については、 :ref:`system_principles` を参照してください。また、既存のシステムに新しいリリースパッケージをインストールする方法については、 :ref:`release_handling` を参照してください。

.. 10.5 Directory Structure

ディレクトリ構造
================

.. Directory structure for the code installed by the release handler 
   from a release package:

リリースハンドラによってインストールされるディレクトリの構造については :ref:`release package` と同様です。

.. code-block:: none

   $ROOT/lib/App1-AVsn1/ebin
                       /priv
            /App2-AVsn2/ebin
                       /priv
            ...
            /AppN-AVsnN/ebin
                       /priv
        /erts-EVsn/bin
        /releases/Vsn
        /bin

``lib``
    .. Application directories. 
    
    アプリケーションディレクトリ。

``erts-EVsn/bin``
    .. Erlang runtime system executables. 
    
    Erlang ランタイムシステムの実行ファイル。

releases/Vsn
    .. .rel file and boot script start.boot.
       If present in the release package,
       relup and/or sys.config. 
    
    ``.rel`` ファイルと、ブートスクリプトの ``start.boot`` です。もしリリースパッケージ内に ``relup`` や ``sys.config`` があれば、それも格納されます。

bin
    .. Top level Erlang runtime system executables. 
    
    トップレベルのErlangランタイムシステムの実行形式です。

.. Applications are not required to be located under the $ROOT/lib directory. 
   Accordingly, several installation directories may exist which contain 
   different parts of a system. For example, the previous example could be 
   extended as follows:

アプリケーションは、 ``$ROOT/lib`` ディレクトリの下に置く必要はありません。そのため、システムをいくつかの部品に分けて、複数のディレクトリにインストールすることもできます。例えば、前のサンプルは次のような配置に拡張することもできます。

.. code-block:: none

   $SECOND_ROOT/.../SApp1-SAVsn1/ebin
                                /priv
                   /SApp2-SAVsn2/ebin
                                /priv
                   ...
                   /SAppN-SAVsnN/ebin
                                /priv

   $THIRD_ROOT/TApp1-TAVsn1/ebin
                           /priv
              /TApp2-TAVsn2/ebin
                           /priv
              ...
              /TAppN-TAVsnN/ebin
                           /priv

.. The $SECOND_ROOT and $THIRD_ROOT are introduced as variables in the call to 
   the systools:make_script/2 function.

この ``$SECOND_ROOT`` と ``$THIRD_ROOT`` は ``systools:make_script/2`` 関数呼び出しの中で変数として導入されたものです。

.. 10.5.1 Disk-Less and/or Read-Only Clients

ディスクレス and/or 読み込み専用クライアント
--------------------------------------------

.. If a complete system consists of some disk-less and/or read-only client nodes, 
   a clients directory should be added to the $ROOT directory. By a read-only 
   node we mean a node with a read-only file system.
   
もし、全体のシステムの中に、ディスクレスのノードや、読み込み専用のノードが含まれる場合は、 ``clients`` ディレクトリを ``$ROOT`` ディレクトリに追加すべきでしょう。ここで言う読み込み専用のノードというのは、読み込み専用のファイルシステムを持つノードという意味です。

.. The clients directory should have one sub-directory per supported client node. 
   The name of each client directory should be the name of the corresponding client 
   node. As a minimum, each client directory should contain the bin and releases 
   sub-directories. These directories are used to store information about installed 
   releases and to appoint the current release to the client. Accordingly, the $ROOT 
   directory contains the following:

``clients`` ディレクトリは、サポートするクライアントのノード1つごとに1つのサブディレクトリを持ちます。各サブディレクトリの名前は、関連するクライアントノードの名前を付けます。少なくとも、それぞれのクライアントのディレクトリには、 ``bin`` と ``releases`` というサブディレクトリが含まれます。これらのディレクトリは、インストールされているリリースの情報を格納したり、現在のリリースを指定するのに使用します。 ``$ROOT`` は次のような構成になります。

.. code-block:: none

   $ROOT/...
       /clients/ClientName1/bin
                           /releases/Vsn
               /ClientName2/bin
                           /releases/Vsn
               ...
               /ClientNameN/bin
                           /releases/Vsn

.. This structure should be used if all clients are running the same type of Erlang 
   machine. If there are clients running different types of Erlang machines, or on 
   different operating systems, the clients directory could be divided into one 
   sub-directory per type of Erlang machine. Alternatively, you can set up one $ROOT 
   per type of machine. For each type, some of the directories specified for the $ROOT 
   directory should be included:

すべてのクライアントで同じ種類のErlangマシンを使用しているのであれば、このディレクトリ構造を使用すべきです。もし、一部のノードのOSや、Erlangマシンの種類が異なるという場合には、Erlangマシンの種類ごとにサブディレクトリに分割すべきです。これ以外の方法としては、マシンの種類ごとに ``$ROOT`` を設定することもできます。 ``$ROOT`` には、それぞれの種類のディレクトリを含むべきです。

.. code-block:: none

   $ROOT/...
       /clients/Type1/lib
                     /erts-EVsn
                     /bin
                     /ClientName1/bin
                                 /releases/Vsn
                     /ClientName2/bin
                                 /releases/Vsn
                     ...
                     /ClientNameN/bin
                                 /releases/Vsn
               ...
               /TypeN/lib
                     /erts-EVsn
                     /bin
                     ...
  
.. With this structure, the root directory for clients of Type1 is $ROOT/clients/Type1.

この構造の場合、 ``Type1`` のクライアントのルートディレクトリは ``$ROOT/clients/Type1`` となります。

Copyright (c) 1991-2009 Ericsson AB
