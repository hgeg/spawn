# spawn
a spawn-fcgi wrapper to easily handle web applications

## installation
install spawn-fcgi package first. then add the file named spawn to your path or move it to your bin directory.

## how to use
go to your web application directory enter the command to initialize spawn for your application:

    spawn init -f <filename.py> -p <port> 

if you want to run a custom command each time you start your project also add ```-c "command"``` option. After the initialization phase, start/stop your app using ```spawn start/stop``` or simply restart by using ```spawn restart```. To remove spawn configuration use ```purge``` command.
