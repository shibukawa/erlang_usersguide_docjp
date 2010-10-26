[Ericsson AB]
11 Release Handling
11.1 Release Handling Principles

An important feature of the Erlang programming language is the ability to change module code in run-time, code replacement, as described in Erlang Reference Manual.

Based on this feature, the OTP application SASL provides a framework for upgrading and downgrading between different versions of an entire release in run-time. This is what we call release handling.

The framework consists of off-line support (systools) for generating scripts and building release packages, and on-line support (release_handler) for unpacking and installing release packages.

Note that the minimal system based on Erlang/OTP, enabling release handling, thus consists of Kernel, STDLIB and SASL.

   1. A release is created as described in the previous chapter Releases. The release is transferred to and installed at target environment. Refer to System Principles for information of how to install the first target system.
   2. Modifications, for example error corrections, are made to the code in the development environment.
   3. At some point it is time to make a new version of release. The relevant .app files are updated and a new .rel file is written.
   4. For each modified application, an application upgrade file, .appup, is created. In this file, it is described how to upgrade and/or downgrade between the old and new version of the application.
   5. Based on the .appup files, a release upgrade file called relup, is created. This file describes how to upgrade and/or downgrade between the old and new version of the entire release.
   6. A new release package is made and transferred to the target system.
   7. The new release package is unpacked using the release handler.
   8. The new version of the release is installed, also using the release handler. This is done by evaluating the instructions in relup. Modules may be added, deleted or re-loaded, applications may be started, stopped or re-started etc. In some cases, it is even necessary to restart the entire emulator.
      If the installation fails, the system may be rebooted. The old release version is then automatically used.
   9. If the installation succeeds, the new version is made the default version, which should now be used in case of a system reboot.

The next chapter, Appup Cookbook, contains examples of .appup files for typical cases of upgrades/downgrades that are normally easy to handle in run-time. However, there are a many aspects that can make release handling complicated. To name a few examples:

    * Complicated or circular dependencies can make it difficult or even impossible to decide in which order things must be done without risking run-time errors during an upgrade or downgrade. Dependencies may be:
          o between nodes,
          o between processes, and
          o between modules.
    * During release handling, non-affected processes continue normal execution. This may lead to timeouts or other problems. For example, new processes created in the time window between suspending processes using a certain module and loading a new version of this module, may execute old code.

It is therefore recommended that code is changed in as small steps as possible, and always kept backwards compatible.
11.2 Requirements

For release handling to work properly, the runtime system needs to have knowledge about which release it is currently running. It must also be able to change (in run-time) which boot script and system configuration file should be used if the system is rebooted, for example by heart after a failure. Therefore, Erlang must be started as an embedded system, see Embedded System for information on how to do this.

For system reboots to work properly, it is also required that the system is started with heart beat monitoring, see erl(1) and heart(3).

Other requirements:

    * The boot script included in a release package must be generated from the same .rel file as the release package itself.
      Information about applications are fetched from the script when an upgrade or downgrade is performed.
    * The system must be configured using one and only one system configuration file, called sys.config.
      If found, this file is automatically included when a release package is created.
    * All versions of a release, except the first one, must contain a relup file.
      If found, this file is automatically included when a release package is created.

11.3 Distributed Systems

If the system consists of several Erlang nodes, each node may use its own version of the release. The release handler is a locally registered process and must be called at each node where an upgrade or downgrade is required. There is a release handling instruction that can be used to synchronize the release handler processes at a number of nodes: sync_nodes. See appup(4).
11.4 Release Handling Instructions

OTP supports a set of release handling instructions that is used when creating .appup files. The release handler understands a subset of these, the low-level instructions. To make it easier for the user, there are also a number of high-level instructions, which are translated to low-level instructions by systools:make_relup.

Here, some of the most frequently used instructions are described. The complete list of instructions is found in appup(4).

First, some definitions:

Residence module
    The module where a process has its tail-recursive loop function(s). If the tail-recursive loop functions are implemented in several modules, all those modules are residence modules for the process.
Functional module
    A module which is not a residence module for any process.

Note that for a process implemented using an OTP behaviour, the behaviour module is the residence module for that process. The callback module is a functional module.
11.4.1 load_module

If a simple extension has been made to a functional module, it is sufficient to simply load the new version of the module into the system, and remove the old version. This is called simple code replacement and for this the following instruction is used:

.. code-block:: erlang

   {load_module, Module}

11.4.2 update

If a more complex change has been made, for example a change to the format of the internal state of a gen_server, simple code replacement is not sufficient. Instead it is necessary to suspend the processes using the module (to avoid that they try to handle any requests before the code replacement is completed), ask them to transform the internal state format and switch to the new version of the module, remove the old version and last, resume the processes. This is called synchronized code replacement and for this the following instructions are used:

.. code-block:: erlang

   {update, Module, {advanced, Extra}}
   {update, Module, supervisor}

update with argument {advanced,Extra} is used when changing the internal state of a behaviour as described above. It will cause behaviour processes to call the callback function code_change, passing the term Extra and some other information as arguments. See the man pages for the respective behaviours and Appup Cookbook.

update with argument supervisor is used when changing the start specification of a supervisor. See Appup Cookbook.

The release handler finds the processes using a module to update by traversing the supervision tree of each running application and checking all the child specifications:

.. code-block:: erlang

   {Id, StartFunc, Restart, Shutdown, Type, Modules}

A process is using a module if the name is listed in Modules in the child specification for the process.

If Modules=dynamic, which is the case for event managers, the event manager process informs the release handler about the list of currently installed event handlers (gen_fsm) and it is checked if the module name is in this list instead.

The release handler suspends, asks for code change, and resumes processes by calling the functions sys:suspend/1,2, sys:change_code/4,5 and sys:resume/1,2 respectively.
11.4.3 add_module and delete_module

If a new module is introduced, the following instruction is used:

.. code-block:: erlang

   {add_module, Module}

The instruction loads the module and is absolutely necessary when running Erlang in embedded mode. It is not strictly required when running Erlang in interactive (default) mode, since the code server automatically searches for and loads unloaded modules.

The opposite of add_module is delete_module which unloads a module:

.. code-block:: erlang

   {delete_module, Module}

Note that any process, in any application, with Module as residence module, is killed when the instruction is evaluated. The user should therefore ensure that all such processes are terminated before deleting the module, to avoid a possible situation with failing supervisor restarts.
11.4.4 Application Instructions

Instruction for adding an application:

.. code-block:: erlang

   {add_application, Application}

Adding an application means that the modules defined by the modules key in the .app file are loaded using a number of add_module instructions, then the application is started.

Instruction for removing an application:

.. code-block:: erlang

   {remove_application, Application}

Removing an application means that the application is stopped, the modules are unloaded using a number of delete_module instructions and then the application specification is unloaded from the application controller.

Instruction for removing an application:

.. code-block:: erlang

   {restart_application, Application}

Restarting an application means that the application is stopped and then started again similar to using the instructions remove_application and add_application in sequence.
11.4.5 apply (low-level)

To call an arbitrary function from the release handler, the following instruction is used:

.. code-block:: erlang

   {apply, {M, F, A}}

The release handler will evalute apply(M, F, A).
11.4.6 restart_new_emulator (low-level)

This instruction is used when changing to a new emulator version, or if a system reboot is needed for some other reason. Requires that the system is started with heart beat monitoring, see erl(1) and heart(3).

When the release handler encounters the instruction, it shuts down the current emulator by calling init:reboot(), see init(3). All processes are terminated gracefully and the system can then be rebooted by the heart program, using the new release version. This new version must still be made permanent when the new emulator is up and running. Otherwise, the old version is used in case of a new system reboot.

On UNIX, the release handler tells the heart program which command to use to reboot the system. Note that the environment variable HEART_COMMAND, normally used by the heart program, in this case is ignored. The command instead defaults to $ROOT/bin/start. Another command can be set by using the SASL configuration parameter start_prg, see sasl(6).
11.5 Application Upgrade File

To define how to upgrade/downgrade between the current version and previous versions of an application, we create an application upgrade file, or in short .appup file. The file should be called Application.appup, where Application is the name of the application:

.. code-block:: erlang

   {Vsn,
    [{UpFromVsn1, InstructionsU1},
     ...,
     {UpFromVsnK, InstructionsUK}],
    [{DownToVsn1, InstructionsD1},
     ...,
     {DownToVsnK, InstructionsDK}]}.

Vsn, a string, is the current version of the application, as defined in the .app file. Each UpFromVsn is a previous version of the application to upgrade from, and each DownToVsn is a previous version of the application to downgrade to. Each Instructions is a list of release handling instructions.

The syntax and contents of the appup file are described in detail in appup(4).

In the chapter Appup Cookbook, examples of .appup files for typical upgrade/downgrade cases are given.

Example: Consider the release ch_rel-1 from the Releases chapter. Assume we want to add a function available/0 to the server ch3 which returns the number of available channels:

(Hint: When trying out the example, make the changes in a copy of the original directory, so that the first versions are still available.)

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

A new version of the ch_app.app file must now be created, where the version is updated:

.. code-block:: erlang

   {application, ch_app,
    [{description, "Channel allocator"},
     {vsn, "2"},
     {modules, [ch_app, ch_sup, ch3]},
     {registered, [ch3]},
     {applications, [kernel, stdlib, sasl]},
     {mod, {ch_app,[]}}
    ]}.

To upgrade ch_app from "1" to "2" (and to downgrade from "2" to "1"), we simply need to load the new (old) version of the ch3 callback module. We create the application upgrade file ch_app.appup in the ebin directory:

.. code-block:: erlang

   {"2",
    [{"1", [{load_module, ch3}]}],
    [{"1", [{load_module, ch3}]}]
   }.

11.6 Release Upgrade File

To define how to upgrade/downgrade between the new version and previous versions of a release, we create a release upgrade file, or in short relup file.

This file does not need to be created manually, it can be generated by systools:make_relup/3,4. The relevant versions of the .rel file, .app files and .appup files are used as input. It is deducted which applications should be added and deleted, and which applications that need to be upgraded and/or downgraded. The instructions for this is fetched from the .appup files and transformed into a single list of low-level instructions in the right order.

If the relup file is relatively simple, it can be created manually. Remember that it should only contain low-level instructions.

The syntax and contents of the release upgrade file are described in detail in relup(4).

Example, continued from the previous section. We have a new version "2" of ch_app and an .appup file. We also need a new version of the .rel file. This time the file is called ch_rel-2.rel and the release version string is changed changed from "A" to "B":

.. code-block:: erlang

   {release,
    {"ch_rel", "B"},
    {erts, "5.3"},
    [{kernel, "2.9"},
     {stdlib, "1.12"},
     {sasl, "1.10"},
     {ch_app, "2"}]
   }.

Now the relup file can be generated:

.. code-block:: erlang

   1> systools:make_relup("ch_rel-2", ["ch_rel-1"], ["ch_rel-1"]).
   ok

This will generate a relup file with instructions for how to upgrade from version "A" ("ch_rel-1") to version "B" ("ch_rel-2") and how to downgrade from version "B" to version "A".

Note that both the old and new versions of the .app and .rel files must be in the code path, as well as the .appup and (new) .beam files. It is possible to extend the code path by using the option path:

.. code-block:: erlang

   1> systools:make_relup("ch_rel-2", ["ch_rel-1"], ["ch_rel-1"],
   [{path,["../ch_rel-1",
   "../ch_rel-1/lib/ch_app-1/ebin"]}]).
   ok

11.7 Installing a Release

When we have made a new version of a release, a release package can be created with this new version and transferred to the target environment.

To install the new version of the release in run-time, the release handler is used. This is a process belonging to the SASL application, that handles unpacking, installation, and removal of release packages. It is interfaced through the module release_handler, which is described in detail in release_handler(3).

Assuming there is a target system up and running with installation root directory $ROOT, the release package with the new version of the release should be copied to $ROOT/releases.

The first action is to unpack the release package, the files are then extracted from the package:

.. code-block:: erlang

   release_handler:unpack_release(ReleaseName) => {ok, Vsn}

ReleaseName is the name of the release package except the .tar.gz extension. Vsn is the version of the unpacked release, as defined in its .rel file.

A directory $ROOT/lib/releases/Vsn will be created, where the .rel file, the boot script start.boot, the system configuration file sys.config and relup are placed. For applications with new version numbers, the application directories will be placed under $ROOT/lib. Unchanged applications are not affected.

An unpacked release can be installed. The release handler then evaluates the instructions in relup, step by step:

.. code-block:: erlang

   release_handler:install_release(Vsn) => {ok, FromVsn, []}

If an error occurs during the installation, the system is rebooted using the old version of the release. If installation succeeds, the system is afterwards using the new version of the release, but should anything happen and the system is rebooted, it would start using the previous version again. To be made the default version, the newly installed release must be made permanent, which means the previous version becomes old:

.. code-block:: erlang

   release_handler:make_permanent(Vsn) => ok

The system keeps information about which versions are old and permanent in the files $ROOT/releases/RELEASES and $ROOT/releases/start_erl.data.

To downgrade from Vsn to FromVsn, install_release must be called again:

.. code-block:: erlang

   release_handler:install_release(FromVsn) => {ok, Vsn, []}

An installed, but not permanent, release can be removed. Information about the release is then deleted from $ROOT/releases/RELEASES and the release specific code, that is the new application directories and the $ROOT/releases/Vsn directory, are removed.

.. code-block:: erlang

   release_handler:remove_release(Vsn) => ok

Example, continued from the previous sections:

1) Create a target system as described in System Principles of the first version "A" of ch_rel from the Releases chapter. This time sys.config must be included in the release package. If no configuration is needed, the file should contain the empty list:

.. code-block:: erlang

   [].

2) Start the system as a simple target system. Note that in reality, it should be started as an embedded system. However, using erl with the correct boot script and .config file is enough for illustration purposes:

.. code-block:: bash

   % cd $ROOT
   % bin/erl -boot $ROOT/releases/A/start -config $ROOT/releases/A/sys
   ...

$ROOT is the installation directory of the target system.

3) In another Erlang shell, generate start scripts and create a release package for the new version "B". Remember to include (a possible updated) sys.config and the relup file, see Release Upgrade File above.

1> systools:make_script("ch_rel-2").
ok
2> systools:make_tar("ch_rel-2").
ok

The new release package now contains version "2" of ch_app and the relup file as well:

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

4) Copy the release package ch_rel-2.tar.gz to the $ROOT/releases directory.

5) In the running target system, unpack the release package:

.. code-block:: erlang

   1> release_handler:unpack_release("ch_rel-2").
   {ok,"B"}

The new application version ch_app-2 is installed under $ROOT/lib next to ch_app-1. The kernel, stdlib and sasl directories are not affected, as they have not changed.

Under $ROOT/releases, a new directory B is created, containing ch_rel-2.rel, start.boot, sys.config and relup.

6) Check if the function ch3:available/0 is available:

.. code-block:: erlang

   2> ch3:available().
   ** exception error: undefined function ch3:available/0

7) Install the new release. The instructions in $ROOT/releases/B/relup are executed one by one, resulting in the new version of ch3 being loaded. The function ch3:available/0 is now available:

.. code-block:: erlang

   3> release_handler:install_release("B").
   {ok,"A",[]}
   4> ch3:available().
   3
   5> code:which(ch3).
   ".../lib/ch_app-2/ebin/ch3.beam"
   6> code:which(ch_sup).
   ".../lib/ch_app-1/ebin/ch_sup.beam"

Note that processes in ch_app for which code have not been updated, for example the supervisor, are still evaluating code from ch_app-1.

8) If the target system is now rebooted, it will use version "A" again. The "B" version must be made permanent, in order to be used when the system is rebooted.

.. code-block:: erlang

   7> release_handler:make_permanent("B").
   ok

.. _updating_application_specifications:

11.8 Updating Application Specifications

When a new version of a release is installed, the application specifications are automatically updated for all loaded applications.
Note

The information about the new application specifications are fetched from the boot script included in the release package. It is therefore important that the boot script is generated from the same .rel file as is used to build the release package itself.

Specifically, the application configuration parameters are automatically updated according to (in increasing priority order):

   1. The data in the boot script, fetched from the new application resource file App.app
   2. The new sys.config
   3. Command line arguments -App Par Val

This means that parameter values set in the other system configuration files, as well as values set using application:set_env/3, are disregarded.

When an installed release is made permanent, the system process init is set to point out the new sys.config.

After the installation, the application controller will compare the old and new configuration parameters for all running applications and call the callback function:

Module:config_change(Changed, New, Removed)

Module is the application callback module as defined by the mod key in the .app file. Changed and New are lists of {Par,Val} for all changed and added configuration parameters, respectively. Removed is a list of all parameters Par that have been removed.

The function is optional and may be omitted when implementing an application callback module.
Copyright (c) 1991-2009 Ericsson AB
