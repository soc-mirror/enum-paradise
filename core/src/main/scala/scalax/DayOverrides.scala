package scalax

@enum
class DayOverridesConcrete {
  Monday
  Tuesday
  Wednesday
  Thursday
  Friday
  Saturday {def isWeekend = true}
  Sunday {def isWeekend = true}

  def isWeekend: Boolean = false
}

//@enum
//class DayOverridesAbstract {
//  Monday {def isWeekend = false}
//  Tuesday {def isWeekend = false}
//  Wednesday {def isWeekend = false}
//  Thursday {def isWeekend = false}
//  Friday {def isWeekend = false}
//  Saturday {def isWeekend = true}
//  Sunday {def isWeekend = true}
//
//  def isWeekend: Boolean
//}