package tasks.adts

import org.junit.*
import org.junit.Assert.*
import u03.extensionmethods.Sequences.Sequence
import u03.extensionmethods.Sequences.Sequence.*

class SchoolModuleTest:
  import SchoolModel.BasicSchoolModule.*

  @Test def testTeacherName() =
    assertEquals("Mario", teacher("Mario"))

  @Test def testCourseName() =
    assertEquals("Math", course("Math"))

  @Test def testEmptySchool() =
    assertEquals(Nil(), emptySchool.teachers())
    assertEquals(Nil(), emptySchool.courses())

  @Test def testSetTeacherToCourse() = {
    val newSchool = emptySchool.setTeacherToCourse(teacher("Mario"), course("Math"))
    assertEquals(Cons("Mario", Nil()), newSchool.teachers())
    assertEquals(Cons("Math", Nil()), newSchool.courses())
    assertTrue(newSchool.hasTeacher("Mario"))
    assertTrue(newSchool.hasCourse("Math"))
    assertEquals(Cons("Math", Nil()), newSchool.coursesOfATeacher(teacher("Mario")))
  }
