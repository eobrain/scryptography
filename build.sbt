/////////////////////////////////////////////////////////////////////////////
// This file is part of the scryptography
//
// Copyright (c) Eamonn O'Brien-Strain All rights
// reserved. This program and the accompanying materials are made
// available under the terms of the Eclipse Public License v1.0 which
// accompanies this distribution, and is available at
// http://www.eclipse.org/legal/epl-v10.html
//
// Contributors:
//    Eamonn O'Brien-Strain  e@obrain.com - initial author
/////////////////////////////////////////////////////////////////////////////


name := "scryptography"

version := "0.1"

scalaVersion := "2.9.1"

libraryDependencies ++= Seq(
  "org.specs2"    %% "specs2"    % "1.8.2"  % "test",
  "com.github.scala-incubator.io" %% "scala-io-core" % "0.2.0",
  "com.github.scala-incubator.io" %% "scala-io-file" % "0.2.0"
)

	 resolvers ++= Seq("snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
                        "releases"  at "http://oss.sonatype.org/content/repositories/releases")

fork in test := true

javaOptions in test += "-Xmx4G"