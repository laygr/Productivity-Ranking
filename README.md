The goal of this project was to execute an R script but instead of reading the input data from a .csv, read it from a database and then, instead of outputting the result on a .csv, send the output as a .csv or a .json via a webservice.

An F# program was built for the project. Its input is:

* server_ip: the address of the server where this app will be running
* server_port: the port that the app will be listening to
* db_host: the address of the Postgres server
* db_port: the port of that the Postgres server uses
* database: the name of the Postgres database
* username: the name of the username to connect to the database
* a space for the database password (no password required…)

In order to interoperate with R, the [BlueMontain Capital’s R Provider](http://bluemountaincapital.github.io/FSharpRProvider/) was used.
Npgsql was used for connecting to the Database.
Suave was used as the web server.
Paket and Fake was used for building the F# project.
