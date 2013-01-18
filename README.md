Copyright 2013 by Marcelo de Sena Lacerda
See the end of the file for license conditions.

This directory holds Witchcraft. It is the open source scala
implementation of the game TAW.

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
As of now there is no official binary distribution of witchcraft so,
before you run it, you should compile it. To run witchcraft use

$ sbt run-main com.botequim.witchcraft.swing.WitchcraftApp



This file is part of Witchcraft.

Witchcraft is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Witchcraft is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Witchcraft.  If not, see <http://www.gnu.org/licenses/>.