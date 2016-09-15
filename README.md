# spawn
A fcgi process spawner for easily handling your web applications.

## installation
Just add the spawn executable to your path or move it to your bin directory.

## how to use
Go to your web application directory and enter the command to initialize spawn for your application:
=======
a spawn-fcgi wrapper that lets you smoothly handle your python-wsgi applications.

## installation
install spawn-fcgi package first. then add the file named spawn to your path or make a symbolic link to your bin directory.

## how to use
go to directory of your wsgi application and run the following command to initialize spawn:

    spawn init -f <filepath> -p <port> 

This will create a config file named `.spawn` at current directory. Now, start your application by typing `spawn start` on this directory, or use `-d <path>` option. 

Here is the list of all the commands with full options:

* `init -f <filepath> -p <port> [--onstart <path>] [--onstop <path>]`: Create spawn configuration for the process.
* `start [-d <abspath>]`: Start the process.
* `stop`: Stop the process.
* `reload`: Restart the process.
* `status`: Show spawn config.
* `clean`: Remove spawn config.

###creating spawnable processes

In order for your app to be run by spawn command, it must expect port number as the first (and only) argument. So, a regular way to start your server may be something like `./myapp 9218`. You can achieve such structure by either refactoring your code, or wrapping main executable with a shell script such as:
	
	#!/bin/sh
	./myapp -f "some custom option" --port $1 --arg "another one"
	
Which can be executable in desired form, therefore is suitable for spawning.
