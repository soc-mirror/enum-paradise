# Enum Paradise [<img src="https://api.travis-ci.org/soc/enum-paradise.png"/>](https://travis-ci.org/soc/enum-paradise)

A Scala enum implementation using type annotations provided by [Macro Paradise](http://docs.scala-lang.org/overviews/macros/paradise.html), intended to be interoperable and compatible with Java enums.

## Usage

    @Enum
    class Days {
      Monday
      Tuesday
      Wednesday
      Thursday
      Friday
      Saturday
      Sunday
    }
    Days.Monday
    Days.values // Array(Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday): Array[Days]
    Days.valueOf("Sunday") // Sunday: Days

## Features

Support for

1. User-defined constructors (implemented)
2. User-defined methods (implemented)
3. User-defined overridden methods in enum items (todo)
4. Adding/implementing traits (implemented)

<!-- -->

    @Enum
    class Days(val inGerman: String) /* 1. */ extends HasName /* 4. */ {
      Monday("Montag")
      Tuesday("Dienstag")
      Wednesday("Mittwoch")
      Thursday("Donnerstag")
      Friday("Freitag")
      Saturday("Samstag") { override def workingDay: Boolean = false }, // 3.
      Sunday("Sonntag") { override def workingDay: Boolean = false }

      def abbreviation = name take 3 // 2.
      def workingDay: Boolean = true // 3.
    }

    trait HasName { def name: String }

## Specification

The SIP for Enums is work in progress and available [here](https://docs.google.com/document/d/1mIKml4sJzezL_-iDJMmcAKmavHb9axjYJos_7UMlWJ8).
Please leave your comments!

## License

    This software is licensed under the Apache 2 license, quoted below.

    Copyright 2009-2012 Alois Cochard 

    Licensed under the Apache License, Version 2.0 (the "License"); you may not
    use this file except in compliance with the License. You may obtain a copy of
    the License at http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
    WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
    License for the specific language governing permissions and limitations under
    the License.
