.. 1 Introduction

イントロダクション
==================

.. 1.1 Purpose

.. index:: 目的, 効率的なコード

目的
----

..  Premature optimization is the root of all evil. -- D.E. Knuth

  早まった最適化は諸悪の根源である -- D.E. Knuth

.. Efficient code can be well-structured and clean code, based on on a sound
.. overall architecture and sound algorithms. Efficient code can be highly
.. implementation-code that bypasses documented interfaces and takes advantage
.. of obscure quirks in the current implementation.

効率的なコード(efficient code)というものは、しっかりと構造が作られ、無駄のないコードです。無駄のないコードというのは、上から下までしっかりしたアーキテクチャと、きちんとしたアルゴリズムに基づいて開発されているものを意味します。効率的なコードというのは高度に実装されたコードであり、ドキュメントが必要なインタフェースや、現在の実装の分かりにくい部分をうまくやりくりしたりする必要のないものを意味しています。

.. Ideally, your code should only contain the first kind of efficient code. If
.. that turns out to be too slow, you should profile the application to find
.. out where the performance bottlenecks are and optimize only the
.. bottlenecks. Other code should stay as clean as possible.

理想的には、コードを書き始めるときには、最初は作成する効率的なコードだけを含むプログラムを作成すべきです。作ってみた結果、もしプログラムがとても遅いということが分かったら、まずはアプリケーションのプロファイルを取り、パフォーマンスのボトルネックがどこにあるのかを見つけます。そして、計測して見つけたボトルネックだけを最適化します。それ以外のコードはできる限りクリーンに保ち、無駄を排除すべきです。

.. Fortunately, compiler and run-time optimizations introduced in R12B makes
   it easier to write code that is both clean and efficient. For instance, the
   ugly workarounds needed in R11B and earlier releases to get the most speed
   out of binary pattern matching are no longer necessary. In fact, the ugly
   code is slower than the clean code (because the clean code has become
   faster, not because the uglier code has become slower).

幸いなことに、R12Bから導入されたコンパイラと、実行時の最適化のおかげで、無駄がなく、効率的なコードを作成するのがずっと簡単になりました。例えば、R11B以前で必要だった、高速化のためにバイナリパターンマッチをするという、決してスマートとはいえないような回避策を取る必要はもはやありません。実際、スマートではないコードというものは、無駄のないコードに比べると遅いのです。これは、スマートではないコードが遅くなるというのではなく、無駄のないコードがより高速になるというのが理由です。

.. This Efficiency Guide cannot really learn you how to write efficient code.
   It can give you a few pointers about what to avoid and what to use, and
   some understanding of how certain language features are implemented. We
   have generally not included general tips about optimization that will work
   in any language, such as moving common calculations out of loops.

このEfficiency Guideを読んだだけでは、実際に効率的なコードがどんどんと書くことができるようになる、ということはないでしょう。このガイドが目指しているものは、避けるべきテクニックと、使うべきテクニックに関していくつかの指針を与えることと、いくつかの言語の機能がどのように実装されているのかを理解してもらう、というものです。

.. 1.2 Prerequisites

条件
----

.. It is assumed that the reader is familiar with the Erlang programming
   language and concepts of OTP.

このガイドでは、読者がErlangのプログラミング言語と、OTPのコンセプトに精通していることを想定しています。

  Copyright (c) 1991-2009 Ericsson AB
