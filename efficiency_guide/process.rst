.. highlight:: erlang

.. 8 Processes

========
プロセス
========

.. 8.1 Creation of an Erlang process

.. index::
  pair: 作成; プロセス
  pair: プロセス; 末尾再帰
  builtin: process_info
  builtin: spawn
  builtin: loop/0
  single: erlangモジュール; system_info/1

Erlangプロセスの作成
====================

.. An Erlang process is lightweight compared to operating systems threads and processes.

Erlangのプロセスは、オペレーティングシステムのスレッドやプロセスと比べて軽量です。

.. A newly spawned Erlang process uses 309 words of memory in the non-SMP emulator without HiPE support. (SMP support and HiPE support will both add to this size.) The size can be found out like this

Erlangのプロセスを新規の生成する場合には、HiPEサポートなしで、SMPエミュレータがない環境では、309ワード(32ビット環境では1ワード4バイト)のメモリを消費します。SMPサポートおよび、HiPEサポートを使用すると、それぞれサイズが大きくなります)。このサイズは以下のようにして核にすることができます::

  Erlang (BEAM) emulator version 5.6 [async-threads:0] [kernel-poll:false]

  Eshell V5.6  (abort with ^G)
  1> Fun = fun() -> receive after infinity -> ok end end.
  #Fun<...>
  2> {_,Bytes} = process_info(spawn(Fun), memory).
  {memory,1232}
  3> Bytes div erlang:system_info(wordsize).
  309

.. The size includes 233 words for the heap area (which includes the stack). The garbage collector will increase the heap as needed.

このプロセスのサイズには233ワードのヒープ領域(スタックも含みます)が含まれます。ガーベジコレクタは必要に応じてヒープの領域を増加させます。

.. The main (outer) loop for a process must be tail-recursive. If not, the stack will grow until the process terminates.

プロセスのメイン(一番外側)ループは **末尾再帰にしなければなりません** 。もし末尾再帰にしなかった場合には、ループごとにスタックを消費して、プロセスが異常終了することになります。

.. DO NOT

非推奨::

  loop() -> 
    receive
       {sys, Msg} ->
           handle_sys_msg(Msg),
           loop();
       {From, Msg} ->
            Reply = handle_msg(Msg),
            From ! Reply,
            loop()
    end,
    io:format("Message is processed~n", []).

.. The call to io:format/2 will never be executed, but a return address will still be pushed to the stack each time loop/0 is called recursively. The correct tail-recursive version of the function looks like this:

io:format/2の呼び出しは行われません。代わりにリターンアドレスがloop/0の一回の呼び出しごとにスタックに積まれていきます。正しい末尾再帰バージョンは以下のようになります::

.. DO

推奨::

   loop() -> 
      receive
         {sys, Msg} ->
            handle_sys_msg(Msg),
            loop();
         {From, Msg} ->
            Reply = handle_msg(Msg),
            From ! Reply,
            loop()
    end.

.. 8.1.1 Initial heap size

.. index::
  pair: プロセス; ヒープサイズ
  builtin: spawn_opt/4

初期ヒープサイズ
----------------

.. The default initial heap size of 233 words is quite conservative in order to support Erlang systems with hundreds of thousands or even millions of processes. The garbage collector will grow and shrink the heap as needed.

デフォルトの初期ヒープサイズが233ワードしかない理由としては、Erlangのシステムが何十万、何百万というプロセス数をサポートをするために、極めて保守的に設定されているからです。ガーベジコレクタは必要に応じて、ヒープのサイズを拡大したり、縮小したりします。

.. In a system that use comparatively few processes, performance might be improved by increasing the minimum heap size using either the +h option for erl or on a process-per-process basis using the min_heap_size option for spawn_opt/4.

比較的プロセス数が少ないシステムでは、elrの+hオプションを使用するか、CPU1プロセスに対して、プロセス数が1のシステムではspawn_opt/4のmin_heap_sizeオプションを使用して最低ヒープサイズを増やすことで、パフォーマンスが上がる可能性があります。

.. The gain is twofold: Firstly, although the garbage collector will grow the heap, it will it grow it step by step, which will be more costly than directly establishing a larger heap when the process is spawned. Secondly, the garbage collector may also shrink the heap if it is much larger than the amount of data stored on it; setting the minimum heap size will prevent that.

メリットとしては2要素あります。まず最初に、ガーベジコレクタはヒープサイズを拡大しますが、少しずつ増加させていきます。この場合、少しずつメモリを確保するよりも、大きなサイズのヒープをプロセス生成時に直接確保した方が効率的である、という点です。2番目は、ガーベジコレクタがヒープサイズを縮小する場合になります。これはヒープが必要量よりもはるかに多い量確保されている場合に縮小が行われますが、最小のヒープサイズを設定することで、これを防ぐことができます。

.. Warning

.. warning::

  エミュレータはおそらくより多くのメモリを消費します。というのは、ガーベジコレクタが実行される回数が少なく、より大きなバイナリデータが長期間保持されることになるからです。

..   The emulator will probably use more memory, and because garbage collections occur less frequently, huge binaries could be kept much longer.

.. In systems with many processes, computation tasks that run for a short time could be spawned off into a new process with a higher minimum heap size. When the process is done, it will send the result of the computation to another process and terminate. If the minimum heap size is calculated properly, the process may not have to do any garbage collections at all. This optimization should not be attempted without proper measurements.  

プロセス数が多いシステムの場合には、新しいプロセスのヒープサイズが小さいほど、処理するタスクの生成にかかる時間が少なくなります。もしプロセスが完了した場合には、計算結果は他のプロセスに送られ、終了します。計算を行うのに必要最低限のプロセスサイズが設定されている場合にはガーベジコレクションはまったく実行されない可能性があります。 **最適化を行う場合には適切に測定せずに行おうとしてはいけません。**

.. 8.2 Process messages

.. index::
  pair: プロセス; メッセージ

プロセスメッセージ
==================

.. All data in messages between Erlang processes is copied, with the exception of refc binaries on the same Erlang node.

Erlangプロセス間のメッセージに含まれる全てのデータは、同じErlangノード上のrefcバイナリを覗いて、コピーされます。

.. When a message is sent to a process on another Erlang node, it will first be encoded to the Erlang External Format before being sent via an TCP/IP socket. The receiving Erlang node decodes the message and distributes it to the right process.

メッセージが他のErlangノードに送信される場合には、まず最初に、Erlang外部フォーマットと呼ばれるものにエンコードされて、TCP/IPソケットを通じて送信されます。受信側のErlangノードは、まずはメッセージをデコードし、正しいプロセスに分配します。

.. 8.2.1 The constant pool

.. index::
  pair: プロセス; 定数
  pair: プロセス; 定数プール

定数プール
----------

.. Constant Erlang terms (also called literals) are now kept in constant pools; each loaded module has its own pool. The following function

定数Erlang項(リテラルとも呼ばれる)は定数プールというところに保存されます。ロードされたモジュールごとに、それぞれプールが存在します。以下のような関数があったとします。

.. DO (in R12B and later)

推奨(R12B以降)::

  days_in_month(M) ->
      element(M, {31,28,31,30,31,30,31,31,30,31,30,31}).

.. will no longer build the tuple every time it is called (only to have it discarded the next time the garbage collector was run), but the tuple will be located in the module's constant pool.

この関数を実行しても、ガーベジコレクタが実行された次の回に実行された時を除き、毎回タプルが生成されることはありません。このタプルはモジュールの定数プール内に配置されます。

.. But if a constant is sent to another process (or stored in an ETS table), it will be copied. The reason is that the run-time system must be able to keep track of all references to constants in order to properly unload code containing constants. (When the code is unloaded, the constants will be copied to the heap of the processes that refer to them.) The copying of constants might be eliminated in a future release.

しかし、定数が他のプロセスに送信されたり、ETSテーブルに保存される場合にはコピーされることになります。この理由というのは、ランタイムシステムは定数を含むコードを、適切なタイミングでアンロードできるように、すべての定数の参照を追跡できるようになっていなければならないのですが、他のプロセスなどに行ってしまうと、追跡が難しいため、コピーされます。コードがアンロードされると、その定数はプロセスのヒープにコピーされます。定数のコピーは、将来のリリースで削除される可能性があります。

.. 8.2.2 Loss of sharing

.. index::
  pair: プロセス; 共有
  single: erts_debugモジュール; size/1
  single: erts_debugモジュール; flat_size/1

共有することによる損失
----------------------

.. Shared sub-terms are not preserved when a term is sent to another process, passed as the initial process arguments in the spawn call, or stored in an ETS table. That is an optimization. Most applications do not send message with shared sub-terms.

sub-termの共有は、termが他のプロセスに送信するときにも保護されません。初期のプロセスの引数として生成の呼び出し時に渡されるか、ETSテーブルの中に格納されます。これは最適化です。ほとんどのアプリケーションでは、メッセージの送信時にはsub-termの共有は行いません。

.. Here is an example of how a shared sub-term can be created:

共有sub-termはどのようにしたら作成されるのか、というサンプルを以下に示します::

  kilo_byte() ->
      kilo_byte(10, [42]).

  kilo_byte(0, Acc) ->
      Acc;
  kilo_byte(N, Acc) ->
      kilo_byte(N-1, [Acc|Acc]).

.. kilo_byte/1 creates a deep list. If we call list_to_binary/1, we can convert the deep list to a binary of 1024 bytes:

``kilo_byte/1`` は深いリストを作成します。もし ``list_to_binary/1`` を呼び出すと、このディープリストは1024バイトのバイナリに変換されます::

  1> byte_size(list_to_binary(efficiency_guide:kilo_byte())).
  1024

.. Using the erts_debug:size/1 BIF we can see that the deep list only requires 22 words of heap space:

``erts_debug:size/1`` という組み込み関数を使用すると、この深いリストが22ワードのヒープ領域しか使用していないことを確認することができます::

  2> erts_debug:size(efficiency_guide:kilo_byte()).
  22

.. Using the erts_debug:flat_size/1 BIF, we can calculate the size of the deep list if sharing is ignored. It will be the size of the list when it has been sent to another process or stored in an ETS table:

``erts_debug:flat_size/1`` 組み込み関数を使用すると、共有が無視されていれば、深いリストのサイズを計算することができます。このサイズは、他のプロセスに送信されたり、ETSテーブルに格納されたりする場合のサイズになります::

  3> erts_debug:flat_size(efficiency_guide:kilo_byte()).
  4094

.. We can verify that sharing will be lost if we insert the data into an ETS table:

もしデータをETSテーブルに格納すると、共有が失われることを確認できます::

  4> T = ets:new(tab, []).
  17
  5> ets:insert(T, {key,efficiency_guide:kilo_byte()}).
  true
  6> erts_debug:size(element(2, hd(ets:lookup(T, key)))).
  4094
  7> erts_debug:flat_size(element(2, hd(ets:lookup(T, key)))).
  4094

.. When the data has passed through an ETS table, erts_debug:size/1 and erts_debug:flat_size/1 return the same value. Sharing has been lost.

データがETSテーブルに渡されると、 ``erts_debug:size/1`` と ``erts_debug:flat_size/1`` は同じ数値を返すようになります。共有はここで失われたと言うことが分かります。

.. In a future release of Erlang/OTP, we might implement a way to (optionally) preserve sharing. We have no plans to make preserving of sharing the default behaviour, since that would penalize the vast majority of Erlang applications.

Erlang/OTPの将来のリリースでは、オプションで、共有を保存する機能を実装しようと考えています。共有の保存に関して、デフォルトの振る舞いをどのようにするかはまだ計画がありませんが、これが導入されると、多くのErlangアプリケーションにとっては、ペナルティがあるでしょう。

.. 8.3 The SMP emulator

.. index::
  pair: SMPエミュレータ; プロセス

SMPエミュレータ
===============

.. The SMP emulator (introduced in R11B) will take advantage of multi-core or multi-CPU computer by running several Erlang schedulers threads (typically, the same as the number of cores). Each scheduler thread schedules Erlang processes in the same way as the Erlang scheduler in the non-SMP emulator.

R11Bから導入されたSMPエミュレータにより、マルチコアやマルチCPUのコンピュータ上でのErlangがスケジューリングしているスレッドの実行が改善されるでしょう。一般的にはコア数と同数のスレッドでもっとも効果を発揮するでしょう。それぞれのスケジューラスレッドはSMPエミュレータがないErlangのスケジューラと同じようにErlangプロセスをスケジューリングします。

.. To gain performance by using the SMP emulator, your application must have more than one runnable Erlang process most of the time. Otherwise, the Erlang emulator can still only run one Erlang process at the time, but you must still pay the overhead for locking. Although we try to reduce the locking overhead as much as possible, it will never become exactly zero.

SMPエミュレータを使用してパフォーマンスを古城させるには、ほとんどの箇所において、一つ以上のErlangプロセスが走るようなアプリケーション構造にする必要があります。そうでなければ、Erlangエミュレータは同時に一つのErlangプロセスしか実行することができません。それだけではなく、マルチプロセス用のロックのオーバーヘッドのコストも支払う必要があります。ロックのオーバーヘッドをできるだけ減らそうとしても、完全にゼロにはなりません。

.. Benchmarks that may seem to be concurrent are often sequential. The estone benchmark, for instance, is entirely sequential. So is also the most common implementation of the "ring benchmark"; usually one process is active, while the others wait in a receive statement.

ベンチマークは並列であっても、シーケンシャルであるかのように見えることがあります。この[estone?]ベンチマークは実際には、完全に直列実行になっています。そのため、リングベンチマークの一般的な実装では、一つのプロセスがアクティブである場合には他のプロセスは文を受け取るまでは待っていることになります。

.. The percept application can be used to profile your application to see how much potential (or lack thereof) it has for concurrency.

並列性に関する潜在能力がどの程度あるかや、スケールしないボトルネックがどれだけあるかについては、プロファイルを使用することで、そのアプリケーションの感覚を得ることができます。

Copyright c 1991-2009 Ericsson AB
