.. 3 Creating a First Target System

==============================
最初のターゲットシステムの作成
==============================

.. 3.1  Introduction

イントロダクション
==================

.. When creating a system using Erlang/OTP, the most simple way is to install 
   Erlang/OTP somewhere, install the application specific code somewhere else, 
   and then start the Erlang runtime system, making sure the code path includes 
   the application specific code.

Erlang/OTPを使ってシステムを作成する時に、一番簡単な方法はErlang/OTPをどこかにインストールすることです。次に、アプリケーション固有のコードもインストールし、アプリケーション固有のコードをコードパスに追加してErlangランタイムシステムを起動します。

.. Often it is not desirable to use an Erlang/OTP system as is. A developer may
   create new Erlang/OTP compliant applications for a particular purpose, and 
   several original Erlang/OTP applications may be irrelevant for the purpose 
   in question. Thus, there is a need to be able to create a new system based 
   on a given Erlang/OTP system, where dispensable applications are removed, 
   and a set of new applications that are included in the new system. 
   Documentation and source code is irrelevant and is therefore not included 
   in the new system.

Erlang/OTPシステムをそのまま使うのが、いつも最適とは限りません。開発者は、特定の目的のために新しくErlang/OTP対応のアプリケーションを作成するかもしれませんし、元々のアプリケーションのいくつかが目的と合わない、ということもあるでしょう。そのため、既存のErlang/OTPシステムを元にして、不要なアプリケーションを削除し、新しいアプリケーションを含めて、新しいシステムを作れる必要があります。また、ドキュメントとソースコードはこの配布用のシステムからは除外されます。

.. This chapter is about creating such a system, which we call a target system.

この章では、「ターゲットシステム」と呼ばれる、このようなシステムの構築方法について説明していきます。

.. In the following sections we consider creating target systems with different 
   requirements of functionality:

このセクションでは、次のような様々な異なった要件で目標のシステムを作成していきます。

.. * a basic target system that can be started by calling the ordinary erl script,

* 通常のerlスクリプトを呼んで起動する、基本的なターゲットシステム

.. * a simple target system where also code replacement in run-time can be performed, and

* 実行時にコードの交換が実行できる、シンプルなターゲットシステム

.. * an embedded target system where there is also support for logging output from the 
     system to file for later inspection, and where the system can be started 
     automatically at boot time.

後から確認できるようにシステムからファイルにログ出力するのをサポートし、起動時に自動的にスタートする、組み込みのターゲットシステム

.. We only consider the case when Erlang/OTP is running on a UNIX system.

ここからの説明では、UNIX系のシステムでErlang/OTPを実行するものとして説明を進めて行きます。

.. There is an example Erlang module target_system.erl that contains functions for 
   creating and installing a target system. That module is used in the examples 
   below. The source code of the module is listed at the end of this chapter.

ターゲットシステムを構成する関数を含む、 :file:`target_system.erl` というErlangモジュールがあったとします。このモジュールはこれからの説明で使用します。このソースコードはこの章の最後にリストされています。

.. 3.2  Creating a Target System

ターゲットシステムの作成
========================

.. It is assumed that you have a working Erlang/OTP system structured according 
   to the OTP Design Principles.

ここでは、 :ref:`otp_design_principles` で説明したErlang/OTPのシステム構成に合わせて、作業を進めているものと想定します。

.. Step 1. First create a .rel file (see rel(4)) that specifies the erts version 
   and lists all applications that should be included in the new basic target system. 
   An example is the following mysystem.rel file:

ステップ1
---------

最初に ``erts`` のバージョンと、新しいベースとなるターゲットシステムに含まれるすべてのアプリケーションの情報をリストアップして指定した、 :file:`.ref` ファイル(rel(4)参照)を最初に作ります。

.. code-block:: erlang

   %% mysystem.rel
   {release,
    {"MYSYSTEM", "FIRST"},
    {erts, "5.1"},
    [{kernel, "2.7"},
     {stdlib, "1.10"},
     {sasl, "1.9.3"},
     {pea, "1.0"}]}.    

.. The listed applications are not only original Erlang/OTP applications but possibly 
   also new applications that you have written yourself (here examplified by the 
   application pea).

リストに含められたアプリケーションは、元々あるErlang/OTPアプリケーションだけではなく、自分で作成した新しいアプリケーションも含めます。この例では、 ``pea`` というアプリケーションが自作のアプリケーションです。

.. Step 2. From the directory where the mysystem.rel file reside, start the Erlang/OTP system:

ステップ2
---------

:file:`mysystem.rel` ファイルのあるディレクトリから、Erlang/OTPのシステムを起動します。

.. code-block:: bash

   os> erl -pa /home/user/target_system/myapps/pea-1.0/ebin

.. where also the path to the pea-1.0 ebin directory is provided.

ここでは、 ``pea-1.0`` の :file:`ebin` ディレクトリのパスをオプションとして渡します。

.. Step 3. Now create the target system:

ステップ3
---------

ターゲットシステムの作成をします。

.. code-block:: erlang

   1> target_system:create("mysystem").

.. The target_system:create/1 function does the following:

:file:`target_system:create/1` 関数は次のようなことを行います。

.. * Reads the mysystem.rel file, and creates a new file plain.rel which is 
     identical to former, except that it only lists the kernel and stdlib 
     applications.

* :file:`mysystem.rel` ファイルを読み込み、新しい :file:`plain.rel` これは、 ``kernel`` と ``stdlib`` アプリケーションだけがリストに入っているというのを除いて、前者と同じです。

.. * From the mysystem.rel and plain.rel files creates the files mysystem.script, 
     mysystem.boot, plain.script, and plain.boot through a call to systools:make_script/2.

* ``systools:make_script/2`` を呼び出し、 :file:`mysystem.rel` と :file:`plain.rel` ファイルから、 :file:`mysystem.script` 、 :file:`mysystem.boot` 、 :file:`plain.script` 、 :file:`plain.boot` を作成します。

.. * Creates the file mysystem.tar.gz by a call to systools:make_tar/2. That file 
     has the following contents:

* :file:`systools:make_tar/2` を呼び出すことで、次のファイルを含む :file:`mysystem.tar.gz` を生成します。

  .. code-block:: none

     erts-5.1/bin/
     releases/FIRST/start.boot
     releases/mysystem.rel
     lib/kernel-2.7/
     lib/stdlib-1.10/
     lib/sasl-1.9.3/
     lib/pea-1.0/  
      
  .. The file releases/FIRST/start.boot is a copy of our mysystem.boot, and a 
     copy of the original mysystem.rel has been put in the releases directory.

  :file:`releases/FIRST/start.boot` は :file:`mysystem.boot` のコピーです。また、オリジナルの :file:`mysystem.rel` は、 :file:`releases` ディレクトリに置かれます。

.. * Creates the temporary directory tmp and extracts the tar file mysystem.tar.gz 
     into that directory.

* 一時ディレクトリの :file:`tmp` を作成し、 :file:`mysystem.tar.gz` ファイルを展開します。

.. * Deletes the erl and start files from tmp/erts-5.1/bin. XXX Why.

* :file:`tmp/erts-5.1/bin` から :file:`erl` と :file:`start` ファイルを削除します。

.. * Creates the directory tmp/bin.

* :file:`tmp/bin` ディレクトリを作成します。

.. * Copies the previously creates file plain.boot to tmp/bin/start.boot.

* 生成されたファイルの :file:`plain.boot` を、 :file:`tmp/bin/start.boot` としてコピーします。

.. * Copies the files epmd, run_erl, and to_erl from the directory tmp/erts-5.1/bin to 
     the directory tmp/bin.

* :file:`epmd` 、 :file:`run_erl` 、 :file:`and to_erl` を、 :file:`tmp/erts-5.1/bin` から :file:`tmp/bin` にコピーします。

.. * Creates the file tmp/releases/start_erl.data with the contents "5.1 FIRST".

* ``5.1 FIRST`` というテキストを含む、 :file:`tmp/releases/start_erl.data` を作成します。

.. * Recreates the file mysystem.tar.gz from the directories in the directory tmp, 
     and removes tmp.

* :file:`tmp` ディレクトリから、再び :file:`mysystem.tar.gz` を作成し、 :file:`tmp` ディレクトリを削除します。

.. 3.3  Installing a Target System

ターゲットシステムのインストール
================================

.. Step 4. Install the created target system in a suitable directory.

ステップ4
---------

作成されたターゲットシステムを、適切なディレクトリにインストールします。

.. code-block:: erlang

   2> target_system:install("mysystem", "/usr/local/erl-target").

.. The function target_system:install/2 does the following:

この :file:`target_system:install/2` 関数は次のことを行います。

.. * Extracts the tar file mysystem.tar.gz into the target directory /usr/local/erl-target.

* :file:`mysystem.tar.gz` を、ターゲットディレクトリの :file:`/usr/local/erl-target` に展開します。

.. * In the target directory reads the file releases/start_erl.data in order to find the 
     Erlang runtime system version ("5.1").

* Erlangのランタイムシステムのバージョン ``5.1`` を探すために、ターゲットディレクトリ内で :file:`releases/start_erl.data` を読みます。

.. * Substitutes %FINAL_ROOTDIR% and %EMU% for /usr/local/erl-target and beam, 
     respectively, in the files erl.src, start.src, and start_erl.src of the target 
     erts-5.1/bin directory, and puts the resulting files erl, start, and run_erl in the
     target bin directory.

* ``%FINAL_ROOTDIR%`` と ``%EMU%`` を、それぞれ、 :file:`/usr/local/erl-targe` と :file:`beam` に読み替えて、ターゲットの :file:`erts-5.1/bin` ディレクトリの中の、 :file:`erl.src` 、 :file:`start.src` 、 :file:`start_erl.src` 内で、結果のファイルの :file:`erl` 、 :file:`start` 、 :file:`run_erl` をターゲットの :file:`bin` ディレクトリの中に置きます。

.. * Finally the target releases/RELEASES file is created from data in the 
     releases/mysystem.rel file.

最後に、 :file:`releases/mysystem.rel` ファイルから、ターゲットの :file:`releases/RELEASES` ファイルが作られます。

.. 3.4  Starting a Target System

ターゲットシステムの起動
========================

.. Now we have a target system that can be started in various ways.

ターゲットシステムの起動方法も、様々あります。

.. We start it as a basic target system by invoking

次のようにして、基本的なターゲットのシステムを起動します。

.. code-block:: bash

   os> /usr/local/erl-target/bin/erl

.. where only the kernel and stdlib applications are started, i.e. the system 
   is started as an ordinary development system. There are only two files 
   needed for all this to work: bin/erl file (obtained from erts-5.1/bin/erl.src) 
   and the bin/start.boot file (a copy of plain.boot).

このように起動すると、 :file:`kernel` 、 :file:`stdlib` アプリケーションだけが起動し、システムは通常の開発環境と同じようにスタートします。このように実行されるには、2つのファイルだけが必要です。 :file:`erts-5.1/bin/erl.src` から取得された、 :file:`bin/erl` ファイルと、 :file:`plain.boot` のコピーの :file:`bin/start.boot` ファイルです。

.. We can also start a distributed system (requires bin/epmd).

それでは、分散のシステムをスタートさせましょう。

.. To start all applications specified in the original mysystem.rel file, 
   use the -boot flag as follows:

オリジナルの :file:`mysystem.rel` ファイルで指定されたすべてのアプリケーションをスタートするには、 ``-boot`` フラグを使用します。

.. code-block:: bash

   os> /usr/local/erl-target/bin/erl -boot /usr/local/erl-target/releases/FIRST/start

.. We start a simple target system as above. The only difference is that 
   also the file releases/RELEASES is present for code replacement in 
   run-time to work.

上記のようにして、簡単にターゲットのシステムをスタートします。唯一の違いは、実行時のコード交換が行えるように、 :file:`releases/RELEASES` というファイルがあるという点のみです。

To start an embedded target system the shell script bin/start is used. That shell script calls bin/run_erl, which in turn calls bin/start_erl (roughly, start_erl is an embedded variant of erl).

組み込みターゲットシステムを起動するには、 :file:`bin/start` というシェルスクリプトを使用します。このシェルスクリプトは :file:`bin/run_erl` を呼び出し、さらに :file:`bin/start_erl` を呼び出します。おおざっぱにまとめると、 :file:`start_erl` は埋め込み版の :file:`erl` を呼び出します。

.. The shell script start is only an example. You should edit it to 
   suite your needs. Typically it is executed when the UNIX system boots.

シェルスクリプトはここで紹介したようなことしかしないため、環境に合わせて編集すべきです。良くある使われ方としては、UNIXのシステムの起動時に一緒に起動するようにする、などです。

.. run_erl is a wrapper that provides logging of output from the 
   run-time system to file. It also provides a simple mechanism for 
   attaching to the Erlang shell (to_erl).

:file:`run_erl` はランタイムシステムからのログ出力をファイルに出力するラッパーです。これはErlangシェルにアタッチする簡単なメカニズムも提供しています(to_erl)。

start_erl requires the root directory ("/usr/local/erl-target"), the 
releases directory ("/usr/local/erl-target/releases"), and the location 
of the start_erl.data file. It reads the run-time system version ("5.1") 
and release version ("FIRST") from the start_erl.data file, starts the 
run-time system of the version found, and provides -boot flag specifying 
the boot file of the release version found ("releases/FIRST/start.boot").

start_erl also assumes that there is sys.config in release version directory ("releases/FIRST/sys.config). That is the topic of the next section (see below).

The start_erl shell script should normally not be altered by the user.

.. 3.5  System Configuration Parameters

システム設定パラメータ
======================

As was pointed out above start_erl requires a sys.config in the release version directory ("releases/FIRST/sys.config"). If there is no such a file, the system start will fail. Hence such a file has to added as well.

If you have system configuration data that are neither file location dependent nor site dependent, it may be convenient to create the sys.config early, so that it becomes a part of the target system tar file created by target_system:create/1. In fact, if you create, in the current directory, not only the mysystem.rel file, but also a sys.config file, that latter file will be tacitly put in the apropriate directory.

.. 3.6  Differences from the Install Script
インストールスクリプトとの違い
==============================

The above install/2 procedure differs somewhat from that of the ordinary Install shell script. In fact, create/1 makes the release package as complete as possible, and leave to the install/2 procedure to finish by only considering location dependent files.

.. 3.7  Listing of target_system.erl

target_system.erlのソースコード
===============================

.. code-block:: erlang

   -module(target_system).
   -include_lib("kernel/include/file.hrl").
   -export([create/1, install/2]).
   -define(BUFSIZE, 8192).
   %% Note: RelFileName below is the *stem* without trailing .rel,
   %% .script etc.
   %%
   %% create(RelFileName)
   %%
   create(RelFileName) ->
       RelFile = RelFileName ++ ".rel", 
       io:fwrite("Reading file: \"~s\" ...~n", [RelFile]),
       {ok, [RelSpec]} = file:consult(RelFile),
       io:fwrite("Creating file: \"~s\" from \"~s\" ...~n", 
                 ["plain.rel", RelFile]),
       {release,
        {RelName, RelVsn},
        {erts, ErtsVsn},
        AppVsns} = RelSpec,
       PlainRelSpec = {release, 
                       {RelName, RelVsn},
                       {erts, ErtsVsn},
                       lists:filter(fun({kernel, _}) -> 
                                            true;
                                       ({stdlib, _}) ->
                                            true;
                                       (_) ->
                                            false
                                    end, AppVsns)
                      },
       {ok, Fd} = file:open("plain.rel", [write]),
       io:fwrite(Fd, "~p.~n", [PlainRelSpec]),
       file:close(Fd),
       io:fwrite("Making \"plain.script\" and \"plain.boot\" files ...~n"),
       make_script("plain"),
       io:fwrite("Making \"~s.script\" and \"~s.boot\" files ...~n", 
                 [RelFileName, RelFileName]),
       make_script(RelFileName),
       TarFileName = io_lib:fwrite("~s.tar.gz", [RelFileName]),
       io:fwrite("Creating tar file \"~s\" ...~n", [TarFileName]),
       make_tar(RelFileName),
       io:fwrite("Creating directory \"tmp\" ...~n"),
       file:make_dir("tmp"), 
       io:fwrite("Extracting \"~s\" into directory \"tmp\" ...~n", [TarFileName]),
       extract_tar(TarFileName, "tmp"),
       TmpBinDir = filename:join(["tmp", "bin"]),
       ErtsBinDir = filename:join(["tmp", "erts-" ++ ErtsVsn, "bin"]),
       io:fwrite("Deleting \"erl\" and \"start\" in directory \"~s\" ...~n", 
                 [ErtsBinDir]),
       file:delete(filename:join([ErtsBinDir, "erl"])),
       file:delete(filename:join([ErtsBinDir, "start"])),
       io:fwrite("Creating temporary directory \"~s\" ...~n", [TmpBinDir]),
       file:make_dir(TmpBinDir),
       io:fwrite("Copying file \"plain.boot\" to \"~s\" ...~n", 
                 [filename:join([TmpBinDir, "start.boot"])]),
       copy_file("plain.boot", filename:join([TmpBinDir, "start.boot"])),
       io:fwrite("Copying files \"epmd\", \"run_erl\" and \"to_erl\" from \n"
                 "\"~s\" to \"~s\" ...~n", 
                 [ErtsBinDir, TmpBinDir]),
       copy_file(filename:join([ErtsBinDir, "epmd"]), 
                 filename:join([TmpBinDir, "epmd"]), [preserve]),
       copy_file(filename:join([ErtsBinDir, "run_erl"]), 
                 filename:join([TmpBinDir, "run_erl"]), [preserve]),
       copy_file(filename:join([ErtsBinDir, "to_erl"]), 
                 filename:join([TmpBinDir, "to_erl"]), [preserve]),
       StartErlDataFile = filename:join(["tmp", "releases", "start_erl.data"]),
       io:fwrite("Creating \"~s\" ...~n", [StartErlDataFile]),
       StartErlData = io_lib:fwrite("~s ~s~n", [ErtsVsn, RelVsn]),
       write_file(StartErlDataFile, StartErlData),
    
       io:fwrite("Recreating tar file \"~s\" from contents in directory "
                 "\"tmp\" ...~n", [TarFileName]),
       {ok, Tar} = erl_tar:open(TarFileName, [write, compressed]),
       {ok, Cwd} = file:get_cwd(),
       file:set_cwd("tmp"),
       erl_tar:add(Tar, "bin", []),
       erl_tar:add(Tar, "erts-" ++ ErtsVsn, []),
       erl_tar:add(Tar, "releases", []),
       erl_tar:add(Tar, "lib", []),
       erl_tar:close(Tar),
       file:set_cwd(Cwd),
       io:fwrite("Removing directory \"tmp\" ...~n"),
       remove_dir_tree("tmp"),
       ok.
   install(RelFileName, RootDir) ->
       TarFile = RelFileName ++ ".tar.gz", 
       io:fwrite("Extracting ~s ...~n", [TarFile]),
       extract_tar(TarFile, RootDir),
       StartErlDataFile = filename:join([RootDir, "releases", "start_erl.data"]),
       {ok, StartErlData} = read_txt_file(StartErlDataFile),
       [ErlVsn, RelVsn| _] = string:tokens(StartErlData, " \n"),
       ErtsBinDir = filename:join([RootDir, "erts-" ++ ErlVsn, "bin"]),
       BinDir = filename:join([RootDir, "bin"]),
       io:fwrite("Substituting in erl.src, start.src and start_erl.src to\n"
                 "form erl, start and start_erl ...\n"),
       subst_src_scripts(["erl", "start", "start_erl"], ErtsBinDir, BinDir, 
                         [{"FINAL_ROOTDIR", RootDir}, {"EMU", "beam"}],
                         [preserve]),
       io:fwrite("Creating the RELEASES file ...\n"),
       create_RELEASES(RootDir, 
                       filename:join([RootDir, "releases", RelFileName])).
   %% LOCALS 
   %% make_script(RelFileName)
   %%
   make_script(RelFileName) ->
       Opts = [no_module_tests],
       systools:make_script(RelFileName, Opts).
   %% make_tar(RelFileName)
   %%
   make_tar(RelFileName) ->
       RootDir = code:root_dir(),
       systools:make_tar(RelFileName, [{erts, RootDir}]).
   %% extract_tar(TarFile, DestDir)
   %%
   extract_tar(TarFile, DestDir) ->
       erl_tar:extract(TarFile, [{cwd, DestDir}, compressed]).
   create_RELEASES(DestDir, RelFileName) ->
       release_handler:create_RELEASES(DestDir, RelFileName ++ ".rel").
   subst_src_scripts(Scripts, SrcDir, DestDir, Vars, Opts) -> 
       lists:foreach(fun(Script) ->
                             subst_src_script(Script, SrcDir, DestDir, 
                                              Vars, Opts)
                     end, Scripts).
   subst_src_script(Script, SrcDir, DestDir, Vars, Opts) -> 
       subst_file(filename:join([SrcDir, Script ++ ".src"]),
                  filename:join([DestDir, Script]),
                  Vars, Opts).
   subst_file(Src, Dest, Vars, Opts) ->
       {ok, Conts} = read_txt_file(Src),
       NConts = subst(Conts, Vars),
       write_file(Dest, NConts),
       case lists:member(preserve, Opts) of
           true ->
               {ok, FileInfo} = file:read_file_info(Src),
               file:write_file_info(Dest, FileInfo);
           false ->
               ok
       end.
   %% subst(Str, Vars)
   %% Vars = [{Var, Val}]
   %% Var = Val = string()
   %% Substitute all occurrences of %Var% for Val in Str, using the list
   %% of variables in Vars.
   %%
   subst(Str, Vars) ->
       subst(Str, Vars, []).
   subst([$%, C| Rest], Vars, Result) when $A =< C, C =< $Z ->
       subst_var([C| Rest], Vars, Result, []);
   subst([$%, C| Rest], Vars, Result) when $a =< C, C =< $z ->
       subst_var([C| Rest], Vars, Result, []);
   subst([$%, C| Rest], Vars, Result) when  C == $_ ->
       subst_var([C| Rest], Vars, Result, []);
   subst([C| Rest], Vars, Result) ->
       subst(Rest, Vars, [C| Result]);
   subst([], _Vars, Result) ->
       lists:reverse(Result).
   subst_var([$%| Rest], Vars, Result, VarAcc) ->
       Key = lists:reverse(VarAcc),
       case lists:keysearch(Key, 1, Vars) of
           {value, {Key, Value}} ->
               subst(Rest, Vars, lists:reverse(Value, Result));
           false ->
               subst(Rest, Vars, [$%| VarAcc ++ [$%| Result]])
       end;
   subst_var([C| Rest], Vars, Result, VarAcc) ->
       subst_var(Rest, Vars, Result, [C| VarAcc]);
   subst_var([], Vars, Result, VarAcc) ->
       subst([], Vars, [VarAcc ++ [$%| Result]]).
   copy_file(Src, Dest) ->
       copy_file(Src, Dest, []).
   copy_file(Src, Dest, Opts) ->
       {ok, InFd} = file:open(Src, [raw, binary, read]),
       {ok, OutFd} = file:open(Dest, [raw, binary, write]),
       do_copy_file(InFd, OutFd),
       file:close(InFd),
       file:close(OutFd),
       case lists:member(preserve, Opts) of
           true ->
               {ok, FileInfo} = file:read_file_info(Src),
               file:write_file_info(Dest, FileInfo);
           false ->
               ok
       end.
   do_copy_file(InFd, OutFd) ->
       case file:read(InFd, ?BUFSIZE) of
           {ok, Bin} ->
               file:write(OutFd, Bin),
               do_copy_file(InFd, OutFd);
           eof  ->
               ok
       end.
       
   write_file(FName, Conts) ->
       {ok, Fd} = file:open(FName, [write]),
       file:write(Fd, Conts),
       file:close(Fd).
   read_txt_file(File) ->
       {ok, Bin} = file:read_file(File),
       {ok, binary_to_list(Bin)}.
   remove_dir_tree(Dir) ->
       remove_all_files(".", [Dir]).
   remove_all_files(Dir, Files) ->
       lists:foreach(fun(File) ->
                             FilePath = filename:join([Dir, File]),
                             {ok, FileInfo} = file:read_file_info(FilePath),
                             case FileInfo#file_info.type of
                                 directory ->
                                     {ok, DirFiles} = file:list_dir(FilePath), 
                                     remove_all_files(FilePath, DirFiles),
                                     file:del_dir(FilePath);
                                 _ ->
                                     file:delete(FilePath)
                             end
                     end, Files).
    
