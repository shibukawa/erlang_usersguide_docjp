[Ericsson AB]
12 Appup Cookbook

This chapter contains examples of .appup files for typical cases of upgrades/downgrades done in run-time.
12.1 Changing a Functional Module

When a change has been made to a functional module, for example if a new function has been added or a bug has been corrected, simple code replacement is sufficient.

Example:

.. code-block:: erlang

   {"2",
    [{"1", [{load_module, m}]}],
    [{"1", [{load_module, m}]}]
   }.

12.2 Changing a Residence Module

In a system implemented according to the OTP Design Principles, all processes, except system processes and special processes, reside in one of the behaviours supervisor, gen_server, gen_fsm or gen_event. These belong to the STDLIB application and upgrading/downgrading normally requires an emulator restart.

OTP thus provides no support for changing residence modules except in the case of special processes.
12.3 Changing a Callback Module

A callback module is a functional module, and for code extensions simple code replacement is sufficient.

Example: When adding a function to ch3 as described in the example in Release Handling, ch_app.appup looks as follows:

{"2",
 [{"1", [{load_module, ch3}]}],
 [{"1", [{load_module, ch3}]}]
}.

OTP also supports changing the internal state of behaviour processes, see Changing Internal State below.
12.4 Changing Internal State

In this case, simple code replacement is not sufficient. The process must explicitly transform its state using the callback function code_change before switching to the new version of the callback module. Thus synchronized code replacement is used.

Example: Consider the gen_server ch3 from the chapter about the gen_server behaviour. The internal state is a term Chs representing the available channels. Assume we want add a counter N which keeps track of the number of alloc requests so far. This means we need to change the format to {Chs,N}.

The .appup file could look as follows:

{"2",
 [{"1", [{update, ch3, {advanced, []}}]}],
 [{"1", [{update, ch3, {advanced, []}}]}]
}.

The third element of the update instruction is a tuple {advanced,Extra} which says that the affected processes should do a state transformation before loading the new version of the module. This is done by the processes calling the callback function code_change (see gen_server(3)). The term Extra, in this case [], is passed as-is to the function:

-module(ch3).
...
-export([code_change/3]).
...
code_change({down, _Vsn}, {Chs, N}, _Extra) ->
    {ok, Chs};
code_change(_Vsn, Chs, _Extra) ->
    {ok, {Chs, 0}}.

The first argument is {down,Vsn} in case of a downgrade, or Vsn in case of an upgrade. The term Vsn is fetched from the 'original' version of the module, i.e. the version we are upgrading from, or downgrading to.

The version is defined by the module attribute vsn, if any. There is no such attribute in ch3, so in this case the version is the checksum (a huge integer) of the BEAM file, an uninteresting value which is ignored.

(The other callback functions of ch3 need to be modified as well and perhaps a new interface function added, this is not shown here).
12.5 Module Dependencies

Assume we extend a module by adding a new interface function, as in the example in Release Handling, where a function available/0 is added to ch3.

If we also add a call to this function, say in the module m1, a run-time error could occur during release upgrade if the new version of m1 is loaded first and calls ch3:available/0 before the new version of ch3 is loaded.

Thus, ch3 must be loaded before m1 is, in the upgrade case, and vice versa in the downgrade case. We say that m1is dependent onch3. In a release handling instruction, this is expressed by the element DepMods:

{load_module, Module, DepMods}
{update, Module, {advanced, Extra}, DepMods}

DepMods is a list of modules, on which Module is dependent.

Example: The module m1 in the application myapp is dependent on ch3 when upgrading from "1" to "2", or downgrading from "2" to "1":

myapp.appup:

{"2",
 [{"1", [{load_module, m1, [ch3]}]}],
 [{"1", [{load_module, m1, [ch3]}]}]
}.

ch_app.appup:

{"2",
 [{"1", [{load_module, ch3}]}],
 [{"1", [{load_module, ch3}]}]
}.

If m1 and ch3 had belonged to the same application, the .appup file could have looked like this:

{"2",
 [{"1",
   [{load_module, ch3},
    {load_module, m1, [ch3]}]}],
 [{"1",
   [{load_module, ch3},
    {load_module, m1, [ch3]}]}]
}.

Note that it is m1 that is dependent on ch3 also when downgrading. systools knows the difference between up- and downgrading and will generate a correct relup, where ch3 is loaded before m1 when upgrading but m1 is loaded before ch3 when downgrading.
12.6 Changing Code For a Special Process

In this case, simple code replacement is not sufficient. When a new version of a residence module for a special process is loaded, the process must make a fully qualified call to its loop function to switch to the new code. Thus synchronized code replacement must be used.
Note

The name(s) of the user-defined residence module(s) must be listed in the Modules part of the child specification for the special process, in order for the release handler to find the process.

Example. Consider the example ch4 from the chapter about sys and proc_lib. When started by a supervisor, the child specification could look like this:

{ch4, {ch4, start_link, []},
 permanent, brutal_kill, worker, [ch4]}

If ch4 is part of the application sp_app and a new version of the module should be loaded when upgrading from version "1" to "2" of this application, sp_app.appup could look like this:

{"2",
 [{"1", [{update, ch4, {advanced, []}}]}],
 [{"1", [{update, ch4, {advanced, []}}]}]
}.

The update instruction must contain the tuple {advanced,Extra}. The instruction will make the special process call the callback function system_code_change/4, a function the user must implement. The term Extra, in this case [], is passed as-is to system_code_change/4:

-module(ch4).
...
-export([system_code_change/4]).
...

system_code_change(Chs, _Module, _OldVsn, _Extra) ->
    {ok, Chs}.

The first argument is the internal state State passed from the function sys:handle_system_msg(Request, From, Parent, Module, Deb, State), called by the special process when a system message is received. In ch4, the internal state is the set of available channels Chs.

The second argument is the name of the module (ch4).

The third argument is Vsn or {down,Vsn} as described for gen_server:code_change/3.

In this case, all arguments but the first are ignored and the function simply returns the internal state again. This is enough if the code only has been extended. If we had wanted to change the internal state (similar to the example in Changing Internal State), it would have been done in this function and {ok,Chs2} returned.
12.7 Changing a Supervisor

The supervisor behaviour supports changing the internal state, i.e. changing restart strategy and maximum restart frequency properties, as well as changing existing child specifications.

Adding and deleting child processes are also possible, but not handled automatically. Instructions must be given by in the .appup file.
12.7.1 Changing Properties

Since the supervisor should change its internal state, synchronized code replacement is required. However, a special update instruction must be used.

The new version of the callback module must be loaded first both in the case of upgrade and downgrade. Then the new return value of init/1 can be checked and the internal state be changed accordingly.

The following upgrade instruction is used for supervisors:

{update, Module, supervisor}

Example: Assume we want to change the restart strategy of ch_sup from the Supervisor Behaviour chapter from one_for_one to one_for_all. We change the callback function init/1 in ch_sup.erl:

-module(ch_sup).
...

init(_Args) ->
    {ok, {{one_for_all, 1, 60}, ...}}.

The file ch_app.appup:

{"2",
 [{"1", [{update, ch_sup, supervisor}]}],
 [{"1", [{update, ch_sup, supervisor}]}]
}.

12.7.2 Changing Child Specifications

The instruction, and thus the .appup file, when changing an existing child specification, is the same as when changing properties as described above:

{"2",
 [{"1", [{update, ch_sup, supervisor}]}],
 [{"1", [{update, ch_sup, supervisor}]}]
}.

The changes do not affect existing child processes. For example, changing the start function only specifies how the child process should be restarted, if needed later on.

Note that the id of the child specification cannot be changed.

Note also that changing the Modules field of the child specification may affect the release handling process itself, as this field is used to identify which processes are affected when doing a synchronized code replacement.
12.7.3 Adding And Deleting Child Processes

As stated above, changing child specifications does not affect existing child processes. New child specifications are automatically added, but not deleted. Also, child processes are not automatically started or terminated. Instead, this must be done explicitly using apply instructions.

Example: Assume we want to add a new child process m1 to ch_sup when upgrading ch_app from "1" to "2". This means m1 should be deleted when downgrading from "2" to "1":

{"2",
 [{"1",
   [{update, ch_sup, supervisor},
    {apply, {supervisor, restart_child, [ch_sup, m1]}}
   ]}],
 [{"1",
   [{apply, {supervisor, terminate_child, [ch_sup, m1]}},
    {apply, {supervisor, delete_child, [ch_sup, m1]}},
    {update, ch_sup, supervisor}
   ]}]
}.

Note that the order of the instructions is important.

Note also that the supervisor must be registered as ch_sup for the script to work. If the supervisor is not registered, it cannot be accessed directly from the script. Instead a help function that finds the pid of the supervisor and calls supervisor:restart_child etc. must be written, and it is this function that should be called from the script using the apply instruction.

If the module m1 is introduced in version "2" of ch_app, it must also be loaded when upgrading and deleted when downgrading:

{"2",
 [{"1",
   [{add_module, m1},
    {update, ch_sup, supervisor},
    {apply, {supervisor, restart_child, [ch_sup, m1]}}
   ]}],
 [{"1",
   [{apply, {supervisor, terminate_child, [ch_sup, m1]}},
    {apply, {supervisor, delete_child, [ch_sup, m1]}},
    {update, ch_sup, supervisor},
    {delete_module, m1}
   ]}]
}.

Note again that the order of the instructions is important. When upgrading, m1 must be loaded and the supervisor's child specification changed, before the new child process can be started. When downgrading, the child process must be terminated before child specification is changed and the module is deleted.
12.8 Adding or Deleting a Module

Example: A new functional module m is added to ch_app:

{"2",
 [{"1", [{add_module, m}]}],
 [{"1", [{delete_module, m}]}]

12.9 Starting or Terminating a Process

In a system structured according to the OTP design principles, any process would be a child process belonging to a supervisor, see Adding and Deleting Child Processes above.
12.10 Adding or Removing an Application

When adding or removing an application, no .appup file is needed. When generating relup, the .rel files are compared and add_application and remove_application instructions are added automatically.
12.11 Restarting an Application

Restarting an application is useful when a change is too complicated to be made without restarting the processes, for example if the supervisor hierarchy has been restructured.

Example: When adding a new child m1 to ch_sup, as in the example above, an alternative to updating the supervisor is to restart the entire application:

{"2",
 [{"1", [{restart_application, ch_app}]}],
 [{"1", [{restart_application, ch_app}]}]
}.

12.12 Changing an Application Specification

When installing a release, the application specifications are automatically updated before evaluating the relup script. Hence, no instructions are needed in the .appup file:

{"2",
 [{"1", []}],
 [{"1", []}]
}.

12.13 Changing Application Configuration

Changing an application configuration by updating the env key in the .app file is an instance of changing an application specification, see above.

Alternatively, application configuration parameters can be added or updated in sys.config.
12.14 Changing Included Applications

The release handling instructions for adding, removing and restarting applications apply to primary applications only. There are no corresponding instructions for included applications. However, since an included application is really a supervision tree with a topmost supervisor, started as a child process to a supervisor in the including application, a relup file can be manually created.

Example: Assume we have a release containing an application prim_app which have a supervisor prim_sup in its supervision tree.

In a new version of the release, our example application ch_app should be included in prim_app. That is, its topmost supervisor ch_sup should be started as a child process to prim_sup.

1) Edit the code for prim_sup:

init(...) ->
    {ok, {...supervisor flags...,
          [...,
           {ch_sup, {ch_sup,start_link,[]},
            permanent,infinity,supervisor,[ch_sup]},
           ...]}}.

2) Edit the .app file for prim_app:

{application, prim_app,
 [...,
  {vsn, "2"},
  ...,
  {included_applications, [ch_app]},
  ...
 ]}.

3) Create a new .rel file, including ch_app:

{release,
 ...,
 [...,
  {prim_app, "2"},
  {ch_app, "1"}]}.

12.14.1 Application Restart

4a) One way to start the included application is to restart the entire prim_app application. Normally, we would then use the restart_application instruction in the .appup file for prim_app.

However, if we did this and then generated a relup file, not only would it contain instructions for restarting (i.e. removing and adding) prim_app, it would also contain instructions for starting ch_app (and stopping it, in the case of downgrade). This is due to the fact that ch_app is included in the new .rel file, but not in the old one.

Instead, a correct relup file can be created manually, either from scratch or by editing the generated version. The instructions for starting/stopping ch_app are replaced by instructions for loading/unloading the application:

{"B",
 [{"A",
   [],
   [{load_object_code,{ch_app,"1",[ch_sup,ch3]}},
    {load_object_code,{prim_app,"2",[prim_app,prim_sup]}},
    point_of_no_return,
    {apply,{application,stop,[prim_app]}},
    {remove,{prim_app,brutal_purge,brutal_purge}},
    {remove,{prim_sup,brutal_purge,brutal_purge}},
    {purge,[prim_app,prim_sup]},
    {load,{prim_app,brutal_purge,brutal_purge}},
    {load,{prim_sup,brutal_purge,brutal_purge}},
    {load,{ch_sup,brutal_purge,brutal_purge}},
    {load,{ch3,brutal_purge,brutal_purge}},
    {apply,{application,load,[ch_app]}},
    {apply,{application,start,[prim_app,permanent]}}]}],
 [{"A",
   [],
   [{load_object_code,{prim_app,"1",[prim_app,prim_sup]}},
    point_of_no_return,
    {apply,{application,stop,[prim_app]}},
    {apply,{application,unload,[ch_app]}},
    {remove,{ch_sup,brutal_purge,brutal_purge}},
    {remove,{ch3,brutal_purge,brutal_purge}},
    {purge,[ch_sup,ch3]},
    {remove,{prim_app,brutal_purge,brutal_purge}},
    {remove,{prim_sup,brutal_purge,brutal_purge}},
    {purge,[prim_app,prim_sup]},
    {load,{prim_app,brutal_purge,brutal_purge}},
    {load,{prim_sup,brutal_purge,brutal_purge}},
    {apply,{application,start,[prim_app,permanent]}}]}]
}.

12.14.2 Supervisor Change

4b) Another way to start the included application (or stop it in the case of downgrade) is by combining instructions for adding and removing child processes to/from prim_sup with instructions for loading/unloading all ch_app code and its application specification.

Again, the relup file is created manually. Either from scratch or by editing a generated version. Load all code for ch_app first, and also load the application specification, before prim_sup is updated. When downgrading, prim_sup should be updated first, before the code for ch_app and its application specification are unloaded.

{"B",
 [{"A",
   [],
   [{load_object_code,{ch_app,"1",[ch_sup,ch3]}},
    {load_object_code,{prim_app,"2",[prim_sup]}},
    point_of_no_return,
    {load,{ch_sup,brutal_purge,brutal_purge}},
    {load,{ch3,brutal_purge,brutal_purge}},
    {apply,{application,load,[ch_app]}},
    {suspend,[prim_sup]},
    {load,{prim_sup,brutal_purge,brutal_purge}},
    {code_change,up,[{prim_sup,[]}]},
    {resume,[prim_sup]},
    {apply,{supervisor,restart_child,[prim_sup,ch_sup]}}]}],
 [{"A",
   [],
   [{load_object_code,{prim_app,"1",[prim_sup]}},
    point_of_no_return,
    {apply,{supervisor,terminate_child,[prim_sup,ch_sup]}},
    {apply,{supervisor,delete_child,[prim_sup,ch_sup]}},
    {suspend,[prim_sup]},
    {load,{prim_sup,brutal_purge,brutal_purge}},
    {code_change,down,[{prim_sup,[]}]},
    {resume,[prim_sup]},
    {remove,{ch_sup,brutal_purge,brutal_purge}},
    {remove,{ch3,brutal_purge,brutal_purge}},
    {purge,[ch_sup,ch3]},
    {apply,{application,unload,[ch_app]}}]}]
}.

12.15 Changing Non-Erlang Code

Changing code for a program written in another programming language than Erlang, for example a port program, is very application dependent and OTP provides no special support for it.

Example, changing code for a port program: Assume that the Erlang process controlling the port is a gen_server portc and that the port is opened in the callback function init/1:

init(...) ->
    ...,
    PortPrg = filename:join(code:priv_dir(App), "portc"),
    Port = open_port({spawn,PortPrg}, [...]),
    ...,
    {ok, #state{port=Port, ...}}.

If the port program should be updated, we can extend the code for the gen_server with a code_change function which closes the old port and opens a new port. (If necessary, the gen_server may first request data that needs to be saved from the port program and pass this data to the new port):

code_change(_OldVsn, State, port) ->
    State#state.port ! close,
    receive
        {Port,close} ->
            true
    end,
    PortPrg = filename:join(code:priv_dir(App), "portc"),
    Port = open_port({spawn,PortPrg}, [...]),
    {ok, #state{port=Port, ...}}.

Update the application version number in the .app file and write an .appup file:

["2",
 [{"1", [{update, portc, {advanced,port}}]}],
 [{"1", [{update, portc, {advanced,port}}]}]
].

Make sure the priv directory where the C program is located is included in the new release package:

1> systools:make_tar("my_release", [{dirs,[priv]}]).
...

12.16 Emulator Restart

If the emulator can or should be restarted, the very simple .relup file can be created manually:

{"B",
 [{"A",
   [],
   [restart_new_emulator]}],
 [{"A",
   [],
   [restart_new_emulator]}]
}.

This way, the release handler framework with automatic packing and unpacking of release packages, automatic path updates etc. can be used without having to specify .appup files.

If some transformation of persistent data, for example database contents, needs to be done before installing the new release version, instructions for this can be added to the .relup file as well.
Copyright (c) 1991-2009 Ericsson AB
