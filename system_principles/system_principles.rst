.. 1 System Principles

============
システム原則
============

.. 1.1  Starting the System

システムの起動
==============

.. An Erlang runtime system is started with the command erl:

Erlangのランタイムシステムは、 :program:`erl` コマンドにより起動できます。

.. code-block:: bash

   % erl
   Erlang (BEAM) emulator version 5.2.3.5 [hipe] [threads:0]
   Eshell V5.2.3.5  (abort with ^G)
   1> 

.. erl understands a number of command line arguments, see erl(1). A number of 
   them are also described in this chapter.

:program:`erl` が理解可能なコマンドライン引数については、 ``erl(1)`` を参照してください。この章では、そのうちのいくつかを紹介していきます。

.. Application programs can access the values of the command line arguments 
   by calling one of the functions init:get_argument(Key), or init:get_arguments(). 
   See init(3).

アプリケーションプログラムは、 ``init:get_argument(Key)`` や、 ``init:get_arguments()`` といった関数を使って、コマンドライン引き数の値にアクセスすることができます。詳しくは ``init(3)`` を参照してください。

.. 1.2  Restarting and Stopping the System

システムの再起動と停止
======================

.. The runtime system can be halted by calling halt/0,1. See erlang(3).

ランタイムシステムは ``halt/0,1`` を使って停止させることができます。 ``elrang(3)`` を参照してください。

.. The module init contains function for restarting, rebooting and stopping the 
   runtime system. See init(3).

``init`` モジュールには、ランタイムシステムを再起動、リブート、停止させるための関数がふくまれます。 ``init(3)`` を参照してください。

.. code-block:: erlang

   init:restart()
   init:reboot()
   init:stop()

.. Also, the runtime system will terminate if the Erlang shell is terminated.

もしErlangシェルが停止すると、ランタイムシステムも終了します。

.. 1.3  Boot Scripts

.. _boot_script:

ブートスクリプト
================

.. The runtime system is started using a boot script. The boot script contains 
   instructions on which code to load and which processes and applications to start.

ランタイムシステムは、ブートスクリプトを使って起動します。ブートスクリプトは起動時にどのコードをロードして、どのプロセスとアプリケーションを起動するのかといった命令を含んでいます。

.. A boot script file has the extension .script. The runtime system uses a binary 
   version of the script. This binary boot script file has the extension .boot.

ブートスクリプトファイルの拡張子は :file:`.script` です。ランタイムシステムはこれのバイナリバージョンのスクリプトを使っています。バイナリ版のブートスクリプトのファイルの拡張子は :file:`.boot` です。

.. Which boot script to use is specified by the command line flag -boot. The 
   extension .boot should be omitted. Example, using the boot script start_all.boot:

ブートスクリプトを指定するには、コマンドラインオプションの ``-boot`` フラグを使用してください。拡張子の :file:`.boot` は省略します。例えば、 :file:`start_all.boot` というファイルを使用する場合には、次のようにコマンドを起動します。

.. code-block:: bash

   % erl -boot start_all

.. If no boot script is specified, it defaults to ROOT/bin/start, see Default Boot 
   Scripts below.

ブートスクリプトを設定しなかった場合には、 :file:`$ROOT/bin/start` がデフォルトで設定されます。詳しくは :ref:`default_boot_scripts` を参照してください。

.. The command line flag -init_debug makes the init process write some debug 
   information while interpreting the boot script:

コマンドラインフラグの ``-init_debug`` を使うと、 ``init`` プロセスがブートスクリプトの実行中に、デバッグ情報を出力するようになります。

.. code-block:: erlang

   % erl -init_debug
   {progress,preloaded}
   {progress,kernel_load_completed}
   {progress,modules_loaded}
   {start,heart}
   {start,error_logger}
   ...

.. See script(4) for a detailed description of the syntax and contents of the boot script.

ブートスクリプトの文法や、含める内容についての詳細な説明は、 :manpage:`script(4)` を参照してください。

.. Default Boot Scripts

.. _default_boot_scripts:

デフォルト・ブートスクリプト
----------------------------

.. Erlang/OTP comes with two boot scripts:

Erlang/OTPは２つのブートスクリプトがあります。

:file:`start_clean.boot`

   .. Loads the code for and starts the applications Kernel and STDLIB.

   ``kernel`` と ``STDLIB`` アプリケーションを起動するためのコードをロードします。

:file:`start_sasl.boot`

   .. Loads the code for and starts the applications Kernel, STDLIB and SASL.

   ``kernel`` と ``STDLIB`` 、 ``SASL`` アプリケーションを起動するためのコードをロードします。


.. Which of start_clean and start_sasl to use as default is decided by the user 
   when installing Erlang/OTP using Install. The user is asked "Do you want to 
   use a minimal system startup instead of the SASL startup". If the answer is 
   yes, then start_clean is used, otherwise start_sasl is used. A copy of the 
   selected boot script is made, named start.boot and placed in the ROOT/bin directory.

:file:`start_clean.boot` と :file:`start_sasl.boot` のどちらがデフォルトで使用されるかは、 ``Install`` を使用してErlang/OTPをインストールする際にユーザが決定します。「Do you want to use a minimal system startup instead of the SASL startup(SASLを使わない最小のシステムのスタートアップを使用しますか？)」という質問がユーザに対して行われます。もしYesと答えたら :file:`start_clean.boot` が、 そうでなければ、 :file:`start_sasl.boot` が使用されます。選択されたブートスクリプトのコピーが作られ、 :file:`start.boot` という名前に設定されて、 :file:`$ROOT/bin` ディレクトリに置かれます。

.. User-Defined Boot Scripts

ユーザ定義・ブートスクリプト
----------------------------

.. It is sometimes useful or necessary to create a user-defined boot script. 
   This is true especially when running Erlang in embedded mode, see Code Loading Strategy.

ユーザ定義・ブートスクリプトを作った方が便利だったり、作らざるを得ないという場面がたまにあります。特に :ref:`code_loading_strategy` で説明するように、組み込みモードでErlangを実行する場合には作ることが多いでしょう。

.. It is possible to write a boot script manually. The recommended way to create a 
   boot script, however, is to generate the boot script from a release resource file 
   Name.rel, using the function systools:make_script/1,2. This requires that the source 
   code is structured as applications according to the OTP design principles. 
   (The program does not have to be started in terms of OTP applications but can 
   be plain Erlang).

ブートスクリプトを手で作成することもできますが、 :file:`Name.rel` という名前のリリースリソースファイルを作り、 ``systools:make_script/1,2`` を使用して生成するやり方が推奨されます。このためには、ソースコードを、 :ref:`otp_design_principles` に従って配置する必要があります。この場合、プログラムがOTPアプリケーションとして実行される必要はなく、純粋なErlangであっても問題ありません。

.. Read more about .rel files in OTP Design Principles and rel(4).

詳細については、OTPデザイン原則の :ref:`release_resource_file` の説明と、 :manpage:`rel(4)` を参照してください。

.. The binary boot script file Name.boot is generated from the boot script file 
   Name.script using the function systools:script2boot(File).

バイナリ形式のブートスクリプトファイルの :file:`Name.boot` は、 ``systools:script2boot(File)`` を使用して、 :file:`Name.script` というブートスクリプトファイルから作られます。

.. 1.4  Code Loading Strategy

.. _code_loading_strategy:

コードのロード戦略
==================

.. The runtime system can be started in either embedded or interactive mode. 
   Which one is decided by the command line flag -mode.

ランタイムシステムは組み込みモードか、対話モードのどちらかで起動することができます。どちらで起動するかは、コマンドラインの ``-mode`` フラグを使って決定します。

.. code-block:: bash

   % erl -mode embedded

.. Default mode is interactive.

デフォルトのモードは ``interactive`` です。

.. In embedded mode, all code is loaded during system start-up according to 
   the boot script. (Code can also be loaded later by explicitly ordering the 
   code server to do so).

組み込みモードでは、システムの起動時に、ブートスクリプトに従ってすべてのコードが読み込まれます。また、コードサーバに明示的に指示することで、後からロードすることもできます。

.. In interactive mode, code is dynamically loaded when first referenced. When a 
   call to a function in a module is made, and the module is not loaded, the code 
   server searches the code path and loads the module into the system.

対話モードでは、最初に参照されたタイミングで動的にロードされます。モジュール内の関数が呼ばれると、コードサーバはコードパスを探索し、モジュールをシステムにロードします。

.. Initially, the code path consists of the current working directory and all 
   object code directories under ROOT/lib, where ROOT is the installation 
   directory of Erlang/OTP. Directories can be named Name[-Vsn] and the code 
   server, by default, chooses the directory with the highest version number 
   among those which have the same Name. The -Vsn suffix is optional. If an 
   ebin directory exists under the Name[-Vsn] directory, it is this directory 
   which is added to the code path.

初めは、コードパスには、現在のワークディレクトリと、 :file:`ROOT/lib` 以下のすべてのオブジェクトコードのディレクトリが含まれます。この :file:`ROOT` はErlang/OTPがインストールされたディレクトリです。ディレクトリの名前は、　``Name[-Vsn]`` という形式で設定され、デフォルトでは、同じ名前の、バージョン番号がより高いディレクトリが選ばれます。 ``-Vsn`` というサフィックスはオプションです。もし、 　``Name[-Vsn]`` という名前のディレクトリの中に、 :file:`ebin` という名前のディレクトリがあったとすると、このディレクトリもコードパスに追加されます。

.. The code path can be extended by using the command line flags -pa Directories 
   and -pz Directories. These will add Directories to the head or end of the 
   code path, respectively. Example

:samp:`-pa {ディレクトリ}` や :samp:`-pz {ディレクトリ}` といったコマンドラインフラグを使用することで、コードパスを拡張することができます。これらのフラグはそれぞれ、コードパスの先頭、もしくは末尾に指定されたディレクトリを追加します。

.. code-block:: bash

   % erl -pa /home/arne/mycode

.. The code server module code contains a number of functions for modifying and 
   checking the search path, see code(3).

コードサーバモジュールにはサーチパスを変更したりチェックしたりするための関数が多く含まれています。 :manpage:`code(3)` を参照してください。

.. 1.5  File Types

ファイルタイプ
==============

.. The following file types are defined in Erlang/OTP:

次のような種類のファイルがErlang/OTPでは定義されています。

.. File Type	File Name/Extension	Documented in
   module	.erl	Erlang Reference Manual
   include file	.hrl	Erlang Reference Manual
   release resource file	.rel	rel(4)
   application resource file	.app	app(4)
   boot script	.script	script(4)
   binary boot script	.boot	-
   configuration file	.config	config(4)
   application upgrade file	.appup	appup(4)
   release upgrade file	relup	relup(4)

   Table 1.1:   File Types

.. list-table:: 
   :header-rows: 1
   
   - * ファイルタイプ
     * ファイル名/拡張子
     * ドキュメント
   - * モジュール
     * :file:`.erl`
     * Erlangリファレンスマニュアル
   - * インクルードファイル
     * :file:`.hrl`
     * Erlangリファレンスマニュアル
   - * リリースリソースファイル
     * :file:`.rel`
     * rel(4)
   - * アプリケーションリソースファイル
     * :file:`.app`
     * app(4)
   - * ブートスクリプト
     * :file:`.script`
     * script(4)
   - * バイナリ・ブートスクリプト
     * :file:`.boot`
     * -
   - * 設定ファイル
     * :file:`.config`
     * config(4)
   - * アプリケーションアップグレードファイル
     * :file:`.appup`
     * appup(4)
   - * リリースアップグレードファイル
     * :file:`relup`
     * relup(4)

Copyright c 1996-2010 Ericsson AB. All Rights Reserved.
