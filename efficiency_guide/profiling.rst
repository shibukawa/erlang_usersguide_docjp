.. 10 Profiling

================
プロファイリング
================

.. 10.1 Do not guess about performance - profile

パフォーマンスについて推測で語るべからず - プロファイル
=======================================================

.. Even experienced software developers often guess wrong about where the performance bottlenecks are in their programs.

経験のあるソフトウェア開発者であっても、自分の作っているソフトウェアのパフォーマンスのボトルネックについては、間違った推測をしてしまうことがよくあります。

.. Therefore, profile your program to see where the performance bottlenecks are and concentrate on optimizing them.

その時はパフォーマンス上のボトルネックがどこにあり、どこを集中的に最適化しなければならないのかを調べるためにプロファイルをすべきです。

.. Erlang/OTP contains several tools to help finding bottlenecks.

Erlang/OTPにはボトルネックを発見するためのツールがいくつか含まれています。

.. fprof and eprof provide the most detailed information about where the time is spent, but they significantly slow downs the programs they profile.

fprofとeprofはどこで時間が消費されているのか、という情報を、一番詳しく教えてくれるツールです。しかし、プロファイルを計測中はプログラムの実行速度は大幅に落ちます。

.. If the program is too big to be profiled by fprof or eprof, cover and cprof could be used to locate parts of the code that should be more thoroughly profiled using fprof or eprof.

もし、あなたが測定しようとしているプログラムが大きすぎて、fprofやeprofで測定できない場合には、まず全体をfporf, eprofで測定してあたりをつけてから、cover, cporfを使用して、徹底的にプロファイルすべき箇所をテストすることができるようになります。

.. cover provides execution counts per line per process, with less overhead than fprof/eprof. Execution counts can with some caution be used to locate potential performance bottlenecks. The most lightweight tool is cprof, but it only provides execution counts on a function basis (or all processes, not per process).

coverは、プロセスごとに、どの行を何回実行したかという情報を出力します。coverはfprof/eprofよりも、パフォーマンス上の劣化は少ないです。実行数というのは潜在的なパフォーマンスのボトルネックがどこにあるのかに注意するうえで使用することができます。もっとも軽いツールはcporfです。しかしこれはプロセス全体の関数の実行回数を知らせる機能しかありません。プロセスごとに集計する機能もありません。

.. 10.2 Big systems

巨大なシステム
==============

.. If you have a big system it might be interesting to run profiling on a simulated and limited scenario to start with. But bottlenecks have a tendency to only appear or cause problems when there are many things going on at the same time, and when there are many nodes involved. Therefore it is desirable to also run profiling in a system test plant on a real target system.

もし巨大なシステムを開発しているのであれば、まずは限定されたシナリオからテストを開始してプロファイラを実行してみるとよいでしょう。しかし、多くの事が同時進行で動いている時に限ってボトルネックが発生したり、問題が起きたりする傾向があります。そのため、理想的には、実際にターゲットになるシステム上にシステムテスト環境を構築してプロファイルを実行するのが理想です。

.. When your system is big you do not want to run the profiling tools on the whole system. You want to concentrate on processes and modules that you know are central and stand for a big part of the execution.

もしシステムが大きすぎる場合には、システム全体に対してプロファイルを実行したくないでしょう。その場合には、実行時に大部分を占めていたり、中心であると分かっているプロセスやモジュールに集中したいと思うでしょう。

.. 10.3 What to look for

何を探すのか？
==============

.. When analyzing the result file from the profiling activity you should look for functions that are called many times and have a long "own" execution time (time excluded calls to other functions). Functions that just are called very many times can also be interesting, as even small things can add up to quite a bit if they are repeated often. Then you need to ask yourself what can I do to reduce this time. Appropriate types of questions to ask yourself are:

プロファイラが出力した結果ファイルを分析する際には、呼ばれている回数が多い関数と、それ自身の実行時間(他の関数を呼び出している時間は除く)が長い関数を見つけましょう。非常に数多く呼び出されている関数というのも、注目すべき対象です。小さい時間も、頻繁に繰り返されて積もり積もれば、非常に大きな時間になる可能性があるからです。まずは、自分に対して、この時間を減らすには何ができるか？と自分に問いかけてみましょう。

  * その関数の呼び出し回数を減らすことはできますか？
  * テストの順番を変えるとそのテストの実行回数は少なくなりますか？(訳注：テスト環境依存なのか？ということを意味していると思われる)
  * 取り除けるような、余計なテストはありますか？
  * 毎回同じ結果を返すような計算式はありますか？
  * 同じ処理をより効率的に行う方法はありますか？
  * 処理を効率的に行えるような別の内部データ表現を考えることはできないか？

.. Can I reduce the number of times the function is called?
.. Are there tests that can be run less often if I change the order of tests?
.. Are there redundant tests that can be removed?
.. Is there some expression calculated giving the same result each time?
.. Is there other ways of doing this that are equivalent and more efficient?
.. Can I use another internal data representation to make things more efficient?

.. These questions are not always trivial to answer. You might need to do some benchmarks to back up your theory, to avoid making things slower if your theory is wrong. See benchmarking.

これらの質問は簡単に答えられないものも中にはあるでしょう。あなたの考えを補助するために、追加のベンチマークが必要になることもあるでしょう。もしあなたの考えが間違っていれば、プログラムの速度の低下を招くこともあります。まずはベンチマークをしてみましょう。

.. 10.4 Tools

ツール
======

.. 10.4.1 fprof

.. index::
  pair: fprof; プロファイリング

fprof
-----

.. fprof measures the execution time for each function, both own time i.e how much time a function has used for its own execution, and accumulated time i.e. including called functions. The values are displayed per process. You also get to know how many times each function has been called. fprof is based on trace to file in order to minimize runtime performance impact. Using fprof is just a matter of calling a few library functions, see fprof manual page under the application tools.

fprofは関数ごとの実行時間を測定します。実行時間は、その関数自身が実行するのに使用した時間と、そこから呼び出された関数の時間も積算した時間の両方が測定されます。測定値はプロセスごとに表示されます。また、それぞれの関数ごとに、何回呼び出されたのか、という情報を得ることもできます。fporfは実行時のパフォーマンスへの影響を最小にするために、トレースを元にして実装されています。fprofを使用すると、いくつかライブラリの関数を呼ぶことになるという制約があります。詳しくはapplication toolsの中にあるfprofのマニュアルを参照してください。してください。

.. fprof was introduced in version R8 of Erlang/OTP. Its predecessor eprof that is based on the Erlang trace BIFs, is still available, see eprof manual page under the application tools. Eprof shows how much time has been used by each process, and in which function calls this time has been spent. Time is shown as percentage of total time, not as absolute time.

fporfはErlang/OTPのR8というバージョンから導入されました。先輩であるeprofはErlangの ``trace`` 組み込み関数を使って実装されていました。eporfは現在でも利用可能です。詳しくはapplication tools以下にあるeporfのマニュアルを参照してください。eporfはプロセスごとにどれだけの時間を使用したか、ということを表示してくれました。どのこの時間はどの関数呼び出しで時間を過ごしたかということも分かります。しかし、この時間はトータル時間に占めるパーセンテージで表示されていて、絶対的な時間は分かりませんでした。

.. index::
  pair: cover; プロファイリング

cover
-----

.. cover's primary use is coverage analysis to verify test cases, making sure all relevant code is covered. cover counts how many times each executable line of code is executed when a program is run. This is done on a per module basis. Of course this information can be used to determine what code is run very frequently and could therefore be subject for optimization. Using cover is just a matter of calling a few library functions, see cover manual page under the application tools.

coverは主にテストケースが、関連コードをすべてカバーしているかを検証するためのカバレッジ分析に使用されます。coverは、プログラムの実行時にそれぞれの行が何回実行されたかというのをカウントします。これは基本的にモジュールごとに行われます。もちろん、この情報はどのコードが頻繁に実行されているかを明らかにしたり、どの箇所を最適化すればよいのか決めるのに使用することができます。coverを使用すると、いくつかの依存するライブラリ関数への呼び出しが発生するという制約があります。詳しくはapplication toolsのcoverのマニュアルをご覧ください。

.. index::
  pair: cprof; プロファイリング


cprof
-----

.. cprof is something in between fprof and cover regarding features. It counts how many times each function is called when the program is run, on a per module basis. cprof has a low performance degradation (versus fprof and eprof) and does not need to recompile any modules to profile (versus cover).

cporfはfporfとcoverの中間の機能を備えたものです。cporfは関数が一回のプログラムの実行中に、それぞれの関数が何回呼ばれたのかというのを、モジュールごとにカウントします。cporfはパフォーマンスへの影響が小さい(fporfやeporfと比べて)です。そしてcoverとは異なり、プロファイルを取りたいモジュールに対する再コンパイルも必要ありません。

.. 10.4.4 Tool summarization

.. index::
  single: プロファイリング; ツールまとめ
  single: fprof; 比較
  single: eprof; 比較
  single: covor; 比較
  single: cprof; 比較

ツールまとめ
------------

.. Tool	 Results	 Size of result	 Effects on program execution time	 Records number of calls	 Records Execution time	 Records called by	 Records garbage collection
.. fprof	 per process to screen/file	 large	 significant slowdown	 yes	 total and own	 yes	 yes
.. eprof	 per process/function to screen/file	 medium	 significant slowdown	 yes	 only total	 no	 no
.. cover	 per module to screen/file	 small	 moderate slowdown	 yes, per line	 no	 no	 no
.. cprof	 per module to caller	 small	 small slowdown	 yes	 no	 no	 no


.. list-table::
   :header-rows: 1
   :widths: 3 10 4 4 4 4 4 4
   
   - * ツール
     * 結果
     * 結果サイズ
     * プログラム実行時間への影響
     * コール回数の記録
     * 実行時間の記録
     * 呼び出し元の記録
     * ガベージコレクションの記録
   - * fprof
     * プロセスごとに画面/ファイルに出力
     * 大きい
     * 極めて大きい
     * O
     * - トータル
       - 個別
     * O
     * O
   - * eprof
     * プロセス/関数ごとに画面/ファイルに出力
     * 中間
     * 極めて大きい
     * O
     * トータル
     * X
     * X
   - * cover
     * モジュールごとに画面/ファイルに出力
     * 小さい
     * 中間の低下
     * O/行ごと
     * X
     * X
     * X
   - * eprof
     * モジュールごとに呼び出し元に結果を通知
     * 小さい
     * 極めて小さい
     * O
     * X
     * X
     * X

.. 10.5 Benchmarking

.. index::
  single: ベンチマーク; 壁掛け時計の時間
  single: ベンチマーク; CPU時間
  single: timerモジュール; tc/3
  builtin: statistics/1


ベンチマーク
============

.. The main purpose of benchmarking is to find out which implementation of a given algorithm or function is the fastest. Benchmarking is far from an exact science. Today's operating systems generally run background tasks that are difficult to turn off. Caches and multiple CPU cores doesn't make it any easier. It would be best to run Unix-computers in single-user mode when benchmarking, but that is inconvenient to say the least for casual testing.

ベンチマークの主な目的としては与えられたアルゴリズムの実装や関数のうち、どれが最速化を決めるというものです。ベンチマークは正確なコンピュータ科学とはほど遠いものです。今日のオペレーティングシステムでは、一般的には終了するのが難しいタスクはバックグラウンドで実行されます。キャッシュや複数コアを持つCPUは事態を複雑にします。ベンチマークを取るのは、シングルユーザモードで実行しているUNIXのコンピュータが最適だと思われますが、少なくとも、気軽にテストを行うには不便であると言えます。

.. Benchmarks can measure wall-clock time or CPU time.

ベンチマークは壁掛け時計の時間(訳注：実際にプログラムの処理が終わるまでに経過した時間)でも、CPU時間でも測定できます。

.. timer:tc/3 measures wall-clock time. The advantage with wall-clock time is that I/O, swapping, and other activities in the operating-system kernel are included in the measurements. The disadvantage is that the the measurements will vary wildly. Usually it is best to run the benchmark several times and note the shortest time - that time should be the minimum time that is possible to achieve under the best of circumstances.

`timer:tc/3`_ は壁掛け時計の時間で測定することができます。この時間を使うメリットは、I/O, ディスクのスワップ、その他のオペレーティングシステムのカーネルの行っている活動も含んだ時間が測定できるというものです。デメリットとしてはこの測定結果が幅広く変化する可能性がある、というものです。通常、このベンチマークを使用する場合には何度か実行して、もっとも時間の短いものを記録するのがベストです。テスト環境の中で最良の環境下で達成可能な結果が最小の時間と言えます。

_`timer:tc/3`: http://erlang.org/doc/man/timer.html#tc-3

引数付きで `statistics/1`_ を実行すると、Erlang仮想マシンで使用されたCPU時間を測定することができます。この時間を計測するメリットは、何度実行しても結果が安定しているということにあります。欠点としてはこの時間にはオペレーティングシステムのカーネル内で消費された時間が含まれないため、ディスクのスワップやI/Oの時間が記録されないことです。そのため、CPU時間を測定すると、I/O(ファイルやソケット)が含まれていると誤解を招くことがあります。

.. with the argument runtime measures CPU time spent in the Erlang virtual machine. The advantage is that the results are more consistent from run to run. The disadvantage is that the time spent in the operating system kernel (such as swapping and I/O) are not included. Therefore, measuring CPU time is misleading if any I/O (file or sockets) are involved.

_`statistics/1`: http://erlang.org/doc/man/erlang.html#statistics-1

.. It is probably a good idea to do both wall-clock measurements and CPU time measurements.

壁掛け時計の時間とCPU時間と両方を測定するのが良い考えだと思われます。

.. Some additional advice:

いくつかの追加のアドバイスがあります。

.. The granularity of both types measurement could be quite high so you should make sure that each individual measurement lasts for at least several seconds.  To make the test fair, each new test run should run in its own, newly created Erlang process. Otherwise, if all tests runs in the same process, the later tests would start out with larger heap sizes and therefore probably does less garbage collections. You could also consider restarting the Erlang emulator between each test.
.. Do not assume that the fastest implementation of a given algorithm on computer architecture X also is the fast on computer architecture Y.

* どちらのタイプの測定を行うにしても、システムの粒度は、極めて重要です。少なくとも数秒間続くようなものをそれぞれ計測すべきです。テストを平等なものにするためには、それぞれのテストを実行する時には、新しく作成したErlangのプロセスで行うべきでしょう。そうしないで、もし同じプロセスを使ってすべてのテストを実行してしまうと、後半のテストはヒープサイズが大きな状態で起動され、ガーベジコレクションの回数も小さくなってしまうことも考えられるからです。それぞれのテストごとにErlangエミュレータを再起動しましょう。
* Xというコンピュータのアーキテクチャで最速だったアルゴリズムの実装が、Yというコンピュータのアーキテクチャでも最速であるということはありません。
   
Copyright c 1991-2009 Ericsson AB
