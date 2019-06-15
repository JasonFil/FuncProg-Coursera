case class FullName(firstName: String, middleName:String, lastName:String)

case class Student(uID:String, name:FullName)

val s1 = Student("190-341-4599", FullName("Jorge", "Villa", "Lobos"))
val s2 = s1.copy()
val s3 = s1.copy(name=s1.name.copy(firstName = "Carla"))
s1.name
s2.name
s3.name