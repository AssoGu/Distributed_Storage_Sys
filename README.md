
tested Erlang/OTP 23

Installation
-----
Download repository:

    $ git clone https://github.com/AssoGu/Distributed_Storage_Sys.git

Build:
    
    $ rebar3 compile
    
Start application
-----
###Proxy node:
Start by launching the proxy node: 

Start by running new erl shell with the following args:
     
     $ erl -name proxy@<node-ip> -mnesia dir "<projectpath>/Distributed_Storage_Sys/mnesiaDB" -setcookie mycookie  -pa /<project path>/Distributed_Storage_Sys/_build/default/lib/dss/ebin

execute:

    $ dss_app:start().
    
lastly, choose proxy button in the "operation mdoe" window.

###Storage node

Start new erl shell with the following args:
     
     $ erl -name StorageName@<node-ip> -mnesia dir "<projectpath>/Distributed_Storage_Sys/mnesiaDB" -setcookie mycookie  -pa /<project path>/Distributed_Storage_Sys/_build/default/lib/dss/ebin

execute:

    $ dss_app:start().
    
In the operation mode window click on "storage" button and enter proxy node address and storage node capacity.
For example:

    $ proxy@127.0.0.1
    $ 50 
    

At this point the main gui should be launch and automatically connect to the cluster.

Operation
-----
In the upper left corner the menu can be found.

Notes
-----
* Always start proxy node before storage node
* Close Application only by opening the menu and clicking on "Quit" option.
* Make sure mnesiaDB folder is empty before starting the application incase the application crushed.


Bugs
----
* wxWidget causes segmentation fault on termination
