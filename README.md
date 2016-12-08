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

This will create a config file named `.spawn` at current directory. Then, start your application by typing `spawn start` on this directory, or use `-d <path>` option. Your app must be an executable in order to be spawned.

Here is the list of all the commands with full options:

* `init -f <filepath> -p <port> [--onstart <path>] [--onstop <path>]`: Create spawn configuration for the process.
* `start [-d <path>]`: Start the process.
* `stop [-d <path>]`: Stop the process.
* `reload [-d <path>]`: Restart the process.
* `status [-d <path>]`: Show spawn config.
* `clean`: Remove spawn config.
