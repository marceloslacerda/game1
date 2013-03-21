Copyright 2013 by Marcelo de Sena Lacerda
See the end of the file for license conditions.

This directory holds Botequim's Game 1. It is the open source scala
implementation of Taw.

COMPILATION
-----------
To compile and run, you will need a java sdk version 6:

* If you are using linux you can look up the internet for specific
instructions about your distribution. Openjdk is recommended but
Oracle JDK should also work.

If you are using Windows it is recommended to use the Oracle JDK.

sbt version 0.12.x is also necessary and its available at
http://www.scala-sbt.org/.

Once all the necessary tools are installed run from a terminal in this
directory

$ sbt compile

TESTING
-------
Assuming you got this directory from the main git repository,
https://github.com/marceloslacerda/witchcraft. You
should always test the code prior running. To do so execute

$ sbt test

RUNNING
-------
As of now there is no official binary distribution of Game 1 so,
before you run it, you should compile it. To run the game use

$ sbt run-main org.botequim.game1.swing.Main



This file is part of Botequim's Game 1.

Game 1 is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Game 1 is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Game 1.  If not, see <http://www.gnu.org/licenses/>.