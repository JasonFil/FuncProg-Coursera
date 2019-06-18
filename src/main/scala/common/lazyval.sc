lazy val e = {
  println("I will throw now!")
  throw new RuntimeException("Threw!")
}

try {
  e
} catch()