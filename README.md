# spawn
a fcgi app spawner for easily handling your web applications.

## installation
just add the spawn executable to your path or move it to your bin directory.

## how to use
go to your web application directory and enter the command to initialize spawn for your application:

    spawn init -f <filepath> -p <port> 

this will create a config file named `.spawn` at current directory. now, to start your application just type `spawn start` on this directory, or use `-d <path>` option. here is the list of all the commands with complete options:

* `init -f <filepath> -p <port> [--onstart <path>] [--onstop <path>]`: creates spawn configuration for the process.
* `start [-d <abspath>]`: starts the process.
* `stop`: stops the process
* `status`: shows the spawn configuration for the process
* `clean`: removes spawn config
