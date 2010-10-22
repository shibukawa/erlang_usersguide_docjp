.. 9 Drivers

============
(ドライバー)
============

This chapter provides a (very) brief overview on how to write efficient drivers. It is assumed that you already have a good understanding of drivers.

.. 9.1  Drivers and concurrency

ドライバーと並行性
==================

The run-time system will always take a lock before running any code in a driver.

By default, that lock will be at the driver level, meaning that if several ports has been opened to the same driver, only code for one port at the same time can be running.

A driver can be configured to instead have one lock for each port.

If a driver is used in a functional way (i.e. it holds no state, but only does some heavy calculation and returns a result), several ports with registered names can be opened beforehand and the port to be used can be chosen based on the scheduler ID like this:

.. code-block:: erlang

   -define(PORT_NAMES(),
       {some_driver_01, some_driver_02, some_driver_03, some_driver_04,
        some_driver_05, some_driver_06, some_driver_07, some_driver_08,
        some_driver_09, some_driver_10, some_driver_11, some_driver_12,
        some_driver_13, some_driver_14, some_driver_15, some_driver_16}).

   client_port() ->
    element(erlang:system_info(scheduler_id) rem tuple_size(?PORT_NAMES()) + 1,
       ?PORT_NAMES()).

As long as there are no more than 16 schedulers, there will never be any lock contention on the port lock for the driver.

.. 9.2  Avoiding copying of binaries when calling a driver

(ドライバー呼び出し時のバイナリコピーを避ける)
==============================================

There are basically two ways to avoid copying a binary that is sent to a driver.

If the Data argument for port_control/3 is a binary, the driver will be passed a pointer to the contents of the binary and the binary will not be copied. If the Data argument is an iolist (list of binaries and lists), all binaries in the iolist will be copied.

Therefore, if you want to send both a pre-existing binary and some additional data to a driver without copying the binary, you must call port_control/3 twice; once with the binary and once with the additional data. However, that will only work if there is only one process communicating with the port (because otherwise another process could call the driver in-between the calls).

Another way to avoid copying binaries is to implement an outputv callback (instead of an output callback) in the driver. If a driver has an outputv callback, refc binaries passed in an iolist in the Data argument for port_command/2 will be passed as references to the driver.

.. 9.3 Returning small binaries from a driver

(ドライバーから小さなバイナリを返す)
====================================

The run-time system can represent binaries up to 64 bytes as heap binaries. They will always be copied when sent in a messages, but they will require less memory if they are not sent to another process and garbage collection is cheaper.

If you know that the binaries you return are always small, you should use driver API calls that do not require a pre-allocated binary, for instance driver_output() or driver_output_term() using the ERL_DRV_BUF2BINARY format, to allow the run-time to construct a heap binary.

.. 9.4 Returning big binaries without copying from a driver

(ドライバーからコピーしないで大きなバイナリを返す)
==================================================

To avoid copying data when a big binary is sent or returned from the driver to an Erlang process, the driver must first allocate the binary and then send it to an Erlang process in some way.

Use driver_alloc_binary() to allocate a binary.

There are several ways to send a binary created with driver_alloc_binary().

From the control callback, a binary can be returned provided that set_port_control() has been called with the flag value PORT_CONTROL_FLAG_BINARY.

A single binary can be sent with driver_output_binary().

Using driver_output_term() or driver_send_term(), a binary can be included in an Erlang term.

Copyright © 2001-2010 Ericsson AB. All Rights Reserved.
