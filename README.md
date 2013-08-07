# Enum Paradise (WORK IN PROGRESS)

Scala enumeration implementation using type macros provided by [Macro Paradise](http://docs.scala-lang.org/overviews/macros/paradise.html) intended to be a compatible with Java enums.

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

## Plans

Support for

1. User-defined constructors
2. User-defined methods
3. User-defined overridden methods in enum items
4. Adding/implementing traits

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

## Current Issues

- How to add the ENUM flag to the enum class (which is required by the JVM)?

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
