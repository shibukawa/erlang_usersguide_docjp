.. 9 Advanced

9 マニアックな話題
==================

.. 9.1 Memory

9.1 メモリ
----------

.. A good start when programming efficiently is to have knowledge about how much memory different data types and operations require. It is implementation-dependent how much memory the Erlang data types and other items consume, but here are some figures for erts-5.2 system (OTP release R9B). (There have been no significant changes in R13.)

効率的なプログラミングを始めるスタートとして、データ型ごと、あるいは操作によって、どのぐらいのメモリを消費するのか、という知識を身につけるのは、悪くない選択と言えるでしょう。Erlangのデータ型や、その他の要素がどのぐらいメモリを消費するのかというのは実装依存になります。今回はerts-5.2システム(OTPリリースR9B)におけるメモリ消費量の数値をお見せしますが、R13においても、それほど大きな違いはありません。

.. The unit of measurement is memory words. There exists both a 32-bit and a 64-bit implementation, and a word is therefore, 4 bytes or 8 bytes, respectively.

メモリ消費量の単位はメモリワードというものになります。32ビット実装と64ビット実装の両方がありますが、メモリワードはそれに依存し、それぞれ4バイト、もしくは8バイトになります。

.. Memory size of different data types Data type   Memory size
.. Integer (-16#7FFFFFF < i <16#7FFFFFF)  1 word
.. Integer (big numbers)   3..N words
.. Atom  1 word. Note: an atom refers into an atom table which also consumes memory. The atom
.. text is stored once for each unique atom in this table. The atom table is not garbage-collected.
.. Float    On 32-bit architectures: 4 words
.. On 64-bit architectures: 3 words
.. Binary   3..6 + data (can be shared)
.. List  1 word per element + the size of each element
.. String (is the same as a list of integers)   2 words per character
.. Tuple    2 words + the size of each element
.. Pid   1 word for a process identifier from the current local node, and 5 words for a process
.. identifier from another node. Note: a process identifier refers into a process table and a
.. node table which also consumes memory.
.. Port  1 word for a port identifier from the current local node, and 5 words for a port
.. identifier from another node. Note: a port identifier refers into aport table and a node
.. table which also consumes memory.
.. Reference   On 32-bit architectures: 5 words for a reference from the current local node, and
.. 7 words for a reference from another node.
.. On 64-bit architectures: 4 words for a reference from the current local node, and 6 words for
.. a reference from another node. Note: a reference refers into a node table which also
.. consumes memory.
.. Fun   9..13 words + size of environment. Note: a fun refers into a fun table which also
.. consumes memory.
.. Ets table   Initially 768 words + the size of each element (6 words + size of Erlang data).
.. The table will grow when necessary.
.. Erlang process    327 words when spawned including a heap of 233 words.
.. Memory size of different data types

**データ型ごとのメモリサイズ**

+------------------------------------+-------------------------------------------------------------+
| データ型                           | メモリサイズ                                                |
+====================================+=============================================================+
| 数値(-16#7FFFFFF < i <16#7FFFFFF)  | 1ワード                                                     |
+------------------------------------+-------------------------------------------------------------+
| 数値(上記の範囲を超える大きな数値) | 3..Nワード                                                  |
+------------------------------------+-------------------------------------------------------------+
| アトム                             | 1ワード。ただし、アトムはアトムテーブルを参照するが、そちら |
|                                    | でもメモリは消費する。アトムのテキストは初めて出てきた      |
|                                    | ユニークなものだけがこのテーブルに格納されていく(同じアトム |
|                                    | は２回以上保存されることはない)。このアトムテーブルは       |
|                                    | ガベージコレクタでは掃除されることはない。                  |
+------------------------------------+-------------------------------------------------------------+
| 浮動小数点数                       | 32ビットアーキテクチャ: 4ワード                             |
|                                    | 64ビットアーキテクチャ: 3ワード                             |
+------------------------------------+-------------------------------------------------------------+
| バイナリ                           | 3..6 + データ(共有可能)                                     |
+------------------------------------+-------------------------------------------------------------+
| リスト                             | 要素ごとに1ワード + それぞれの要素サイズ                    |
+------------------------------------+-------------------------------------------------------------+
| 文字列(数値の入ったリストと同等)   | 1文字ごとに2ワード                                          |
+------------------------------------+-------------------------------------------------------------+
| タプル                             | 2ワード + それぞれの要素サイズ                              |
+------------------------------------+-------------------------------------------------------------+
| プロセスID                         | 現在のローカルノード上のプロセスの識別子: 1ワード。         |
|                                    | 他のノード上のプロセスの識別子: 5ワード                     |
|                                    | ただし、プロセス識別子はプロセステーブルを参照します。      |
|                                    | プロセステーブルは上記とは別にメモリを消費します。          |
+------------------------------------+-------------------------------------------------------------+
| ポート                             | 現在のローカルノード上のポートの識別子: 1ワード。           |
|                                    | 他のノード上のポートの識別子: 5ワード                       |
|                                    | ただし、ポート識別子はポートテーブルを参照します。          |
|                                    | ポートテーブルは上記とは別にメモリを消費します。            |
+------------------------------------+-------------------------------------------------------------+
| 参照                               | 32ビットアーキテクチャ:                                     |
|                                    |   現在のローカルノードからの参照: 5ワード。                 |
|                                    |   他のノードからの参照: 7ワード                             |
|                                    | 64ビットアーキテクチャ:                                     |
|                                    |   現在のローカルノードからの参照: 4ワード。                 |
|                                    |   他のノードからの参照: 6ワード                             |
|                                    |                                                             |
|                                    | ただし、参照はノードテーブルを参照し、                      |
|                                    | これもメモリを消費します。                                  |
+------------------------------------+-------------------------------------------------------------+
| fun                                | 9～13ワード + 環境のサイズ                                  |
|                                    | ただし、funはfunテーブルを参照し、これもメモリを消費します。|
+------------------------------------+-------------------------------------------------------------+
| ETSテーブル                        | 768ワード + それぞれの要素で消費される量                    |
|                                    | (6ワード+要素内のデータサイズ)。テーブルは必要に応じて拡張  |
|                                    | される。                                                    |
+------------------------------------+-------------------------------------------------------------+
| Erlangプロセス                     | 327ワード(そのうちヒープは233ワード                         |
+------------------------------------+-------------------------------------------------------------+

.. 9.2 System limits

9.2 システムの限界
------------------

.. The Erlang language specification puts no limits on number of processes, length of atoms etc., but for performance and memory saving reasons, there will always be limits in a practical implementation of the Erlang language and execution environment.

Erlangは言語仕様では、プロセス数やアトムの長さなどに関して、限界を設けていません。しかし、パフォーマンスや、メモリ消費の節約のために、実際のErlangの実装や、実行環境に関しては、制約が設けられています。

.. Processes
..     The maximum number of simultaneously alive Erlang processes is by default 32768. This limit can be
.. raised up to at most 268435456 processes at startup (see documentation of the system flag +P in the erl(1)
..  documentation). The maximum limit of 268435456 processes will at least on a 32-bit architecture be
.. impossible to reach due to memory shortage.
.. Distributed nodes
..     Known nodes
..         A remote node Y has to be known to node X if there exist any pids, ports, references, or funs
.. (Erlang data types) from Y on X, or if X and Y are connected. The maximum number of remote nodes
.. simultaneously/ever known to a node is limited by the maximum number of atoms available for node
.. names. All data concerning remote nodes, except for the node name atom, are garbage-collected.
..     Connected nodes
..         The maximum number of simultaneously connected nodes is limited by either the maximum
.. number of simultaneously known remote nodes, the maximum number of (Erlang) ports available, or
.. the maximum number of sockets available.
.. Characters in an atom
..     255
.. Atoms
..     The maximum number of atoms is 1048576.
.. Ets-tables
..     The default is 1400, can be changed with the environment variable ERL_MAX_ETS_TABLES.
.. Elements in a tuple
..     The maximum number of elements in a tuple is 67108863 (26 bit unsigned integer). Other factors such as the
..     available memory can of course make it hard to create a tuple of that size.
.. Size of binary
..     In the 32-bit implementation of Erlang, 536870911 bytes is the largest binary that can be constructed or
.. matched using the bit syntax. (In the 64-bit implementation, the maximum size is 2305843009213693951 bytes.)
.. If the limit is exceeded, bit syntax construction will fail with a system_limit exception, while any attempt
.. to match a binary that is too large will fail. This limit is enforced starting with the R11B-4 release;
.. in earlier releases, operations on too large binaries would in general either fail or give incorrect results.
.. In future releases of Erlang/OTP, other operations that create binaries (such as list_to_binary/1) will
.. probably also enforce the same limit.
.. Total amount of data allocated by an Erlang node
..     The Erlang runtime system can use the complete 32 (or 64) bit address space, but the operating system
.. often limits a single process to use less than that.
.. length of a node name
..     An Erlang node name has the form host@shortname or host@longname.  The node name is used as an
.. atom within the system so the maximum size of 255 holds for the node name too.
.. Open ports
..     The maximum number of simultaneously open Erlang ports is by default 1024. This limit can be raised
.. up to at most 268435456 at startup (see environment variable ERL_MAX_PORTS in erlang(3)) The maximum
.. limit of 268435456 open ports will at least on a 32-bit architecture be impossible to reach due to memory
.. shortage.
.. Open files, and sockets
..     The maximum number of simultaneously open files and sockets depend on the maximum number of Erlang
.. ports available, and operating system specific settings and limits.
.. Number of arguments to a function or fun
..     256

プロセス
   同時に起動できるErlangのプロセス数は32768です。この制限は起動時に設定することで最大268435456プロセスまで増やすことができます。erl(1)のドキュメント内の+Pシステムフラグの説明を参照してください。ただし、32ビット環境ではその最大のプロセス数に達する前に、メモリが足りなくなって起動できなくなります。

分散ノード数
   知っているノード数
       リモートノードYは他のノードXのpid, ポート、参照、funなどを通じて、YからXを「知っている」という状態を作ることができます。あるいは、XとYがつながっている場合もそうです。同時に知ることができるノード数の最大値は、ノード名として使用できるアトムの最大数により制限されます。リモートノードに関するすべてのデータは、ノード名のアトムを除いてガベージコレクタで回収されます。

   接続するノード数
       同時に接続できるノード数というのは、同時に知ることができるリモートノード数と、Erlangのポートの利用可能な最大数、もしくは利用可能な最大のソケット数により制限されます。

アトムの文字数
   255

アトムの数
   持てるアトムの限界数は1048576です。

Etsテーブル数
   デフォルトでは1400ですが、 ``ERL_MAX_ETS_TABLES`` という環境変数を設定することで変更することができます。

タプルの要素数
   タプルの要素数の最大値は67108863(26ビットの整数で表される範囲)です。これとは別の要因になりますが、使用できるメモリの限界によって、そのサイズのタプルを実際に作るのは難しいでしょう。

バイナリのサイズ
   Erlangの32ビット実装では、ビット文法を使って作成したり、一致しているかどうか見てみるような処理を行える限界は536870911バイトというサイズのデータが最大のサイズになります。また、参考までに64ビット環境であれば2305843009213693951バイトになります。もし限界値を超えてしまった場合には、ビット文法を使ったバイナリの作成は ``system_limit`` 例外を出して失敗します。この制限はR11B-4のリリースから適用されることになりましたが、それ以前のリリースでは大きすぎるバイナリに対する操作を行うと、計算に失敗したり、間違った結果を返したりしていました。Erlang/OTPの将来のリリースでは ``list_to_binary/1`` などの、他のバイナリ作成方法についても、同様の制限が加えられる予定です。

1つのErlangのノードが割り当てることができるデータの総量
   Erlangランタイムは32(もしくは64)ビットのアドレス空間をフルに使用することができますが、通常の場合、オペレーティングシステムの制限で、ひとつのプロセスが使用できる量はそれよりも少なくなります。

ノード名の長さ
   Erlangのノード名は、host@shortname, もしくは、host@longnameという形式を持ちます。ノード名はシステム内ではアトムとして扱われるため、ノード名に関してもアトム同様に、最大255文字という制限が課せられます。

オープンするポート数
   同時にオープンできるErlangのポート数は、デフォルトでは1024になっています。この制限は起動時に設定することで268435456まで増やすことができます。erlang(3)の ``ERL_MAX_PORTS`` の説明を見てください。ただし、32ビット環境ではその最大のポート数に達する前に、メモリが足りなくなってオープンできなくなります。

オープンするファイル数およびソケット数
   同時にオープンできるファイルとソケットの最大数は、利用可能なErlangのポート数と、オペレーティングシステムで設定されている限界数に依存します。

関数およびfunの引数の数
   256


Copyright (c) 1991-2009 Ericsson AB
