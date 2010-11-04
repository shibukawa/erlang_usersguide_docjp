.. 2 Error Logging

================
エラーのロギング
================

.. 2.1  Error Information From the Runtime System

ランタイムシステムから送られてくるエラー情報
============================================

.. Error information from the runtime system, that is, information about a 
   process terminating due to an uncaught error exception, is by default 
   written to terminal (tty):

ランタイムシステムから送られてくるエラ情報のうち、キャッチされない例外によってプロセスが終了したという情報は、デフォルトではターミナル(tty)に出力されます。

.. code-block:: none

   =ERROR REPORT==== 9-Dec-2003::13:25:02 ===
   Error in process <0.27.0> with exit value: {{badmatch,[1,2,3]},[{m,f,1},{shell,eval_loop,2}]}

.. The error information is handled by the error logger, a system process 
   registered as error_logger. This process receives all error messages from 
   the Erlang runtime system and also from the standard behaviours and different 
   Erlang/OTP applications.

エラー情報は、 ``error_logger`` というシステムプロセスのエラーロガーにより取り扱われます。このプロセスは、Erlangのランタイムシステムや、標準的なビヘイビア、他のErlang/OTPアプリケーションなどから送られてくるすべてのエラーメッセージを受信します。

.. The exit reasons (such as badarg above) used by the runtime system are described 
   in Errors and Error Handling in the Erlang Reference Manual.

ランタイムシステムが使用する、上記のサンプルの ``badarg`` のような終了理由については、Erlangリファレンスマニュアルの、 ``Errors and Error Handling`` で説明されています。

.. The process error_logger and its user interface (with the same name) are 
   described in error_logger(3). It is possible to configure the system so 
   that error information is written to file instead/as well as tty. Also,
   it is possible for user defined applications to send and format error
   information using error_logger.

``error_logger`` プロセスと、そのインタフェース(プロセス名と同じ名前)は、 :manpage:`error_logger(3)` で説明されています。また、設定によって、エラー情報をコンソールに出さないでファイルに出したり、両方に同時に出すことも可能です。また、ユーザ定義アプリケーションも、 ``error_logger`` を使ってエラー情報を送信したり、フォーマットすることも可能です。

.. 2.2  SASL Error Logging

SASLエラーロギング
===================

.. The standard behaviors (supervisor, gen_server, etc.) sends progress and error 
   information to error_logger. If the SASL application is started, this information 
   is written to tty as well. See SASL Error Logging in the SASL User's Guide for 
   further information.

標準のビヘイビア(:ref:`supervisor` 、 :ref:`gen_server` など)は進捗情報やエラー情報を ``error_logger`` に送付します。もし ``SASL`` アプリケーションが起動していると、この情報は同じようにttyに書き出されます。詳しくはSASLユーザガイドのSASL Error Loggingを参照してください。

.. code-block:: bash

   % erl -boot start_sasl
   Erlang (BEAM) emulator version 5.4.13 [hipe] [threads:0] [kernel-poll]
   =PROGRESS REPORT==== 31-Mar-2006::12:45:58 ===
             supervisor: {local,sasl_safe_sup}
                started: [{pid,<0.33.0>},
                          {name,alarm_handler},
                          {mfa,{alarm_handler,start_link,[]}},
                          {restart_type,permanent},
                          {shutdown,2000},
                          {child_type,worker}]
   =PROGRESS REPORT==== 31-Mar-2006::12:45:58 ===
             supervisor: {local,sasl_safe_sup}
                started: [{pid,<0.34.0>},
                          {name,overload},
                          {mfa,{overload,start_link,[]}},
                          {restart_type,permanent},
                          {shutdown,2000},
                          {child_type,worker}]
   =PROGRESS REPORT==== 31-Mar-2006::12:45:58 ===
             supervisor: {local,sasl_sup}
                started: [{pid,<0.32.0>},
                          {name,sasl_safe_sup},
                          {mfa,{supervisor,
                                   start_link,
                                   [{local,sasl_safe_sup},sasl,safe]}},
                          {restart_type,permanent},
                          {shutdown,infinity},
                          {child_type,supervisor}]
   =PROGRESS REPORT==== 31-Mar-2006::12:45:58 ===
             supervisor: {local,sasl_sup}
                started: [{pid,<0.35.0>},
                          {name,release_handler},
                          {mfa,{release_handler,start_link,[]}},
                          {restart_type,permanent},
                          {shutdown,2000},
                          {child_type,worker}]
   =PROGRESS REPORT==== 31-Mar-2006::12:45:58 ===
            application: sasl
             started_at: nonode@nohost
   Eshell V5.4.13  (abort with ^G)
   1> 

Copyright c 1996-2010 Ericsson AB. All Rights Reserved.
