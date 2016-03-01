# spawn
a spawn-fcgi wrapper that lets you smoothly handle your python-wsgi applications.

## installation
install spawn-fcgi package first. then add the file named spawn to your path or make a symbolic link to your bin directory.

## how to use
go to directory of your wsgi application and run the following command to initialize spawn:

    spawn init -f <filename.py> -p <port> 

this will create a config file titled ```.spawn``` in the working directory. You can also use experimental ```--onstart <command>``` or ```--onstop <command>``` flags during initialization if you want to bind custom commands to start/stop events.

## commands

```start```: runs the application.

```stop```: kills the running process.

```restart```: just a shortcut for "stop, wait for a bit then start again".

```status```: prints the contents of config file in a human readable format.

```purge```: removes the config file.
