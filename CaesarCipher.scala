object CaesarCipher extends App {
  val encryptedFileName = "encrypt.txt"
  val decryptedFileName = "decrypted.txt"
  val originalFile = "originalText.txt"
  val pattern = "[^A-Za-z]"
  val popularChar = 'e'
  val charCount = 26
  
//  Start
//  writeToFile(encryptedFileName, crypt(readFile(originalFile), 2))
  val str = readFile(encryptedFileName)
  val list = count(str)
  val key = findKey(list.head._1)
  println("Key = " + key)
  makeStatistick(list)
  writeToFile(decryptedFileName, decrypt(str, key))
  println("Finish")

  def readFile(fileName: String): String = Source.fromFile(fileName).getLines.reduceLeft(_ + "\n" + _)

  def count(letters: String): ListMap[Char, Int] = ListMap(letters.replaceAll(pattern, "").groupBy(_.toChar.toLower)
    .map { p => (p._1, p._2.length) }.toSeq.sortWith{(a, b) => if(a._2 != b._2) a._2 > b._2 else a._1 < b._1}: _*)

  def findKey(c: Char): Int = (c.toLower - popularChar) % charCount

  def writeToFile(fileName: String, text: String) {
    val out = new PrintWriter(fileName); out.println(text); out.close()
  }

  def makeStatistick(list: ListMap[Char, Int]) {
    println("Statistic info")
    val size = (for (x <- list) yield x._2).sum
    list.foreach { x => println("Частота появи " + x._1 + " = %2.2f".format((x._2.toFloat / size) * 100) + "%") }
  }

  def crypt(message: String, key: Int): String = {
    for (c <- message) yield if (c <= 90 && c >= 65) {
      if ((c + key) > 90) (((c + key) % 90) + 65).toChar else ((c + key) % 91).toChar
    } else if (c <= 122 && c >= 97) {
      if ((c + key > 122)) (((c + key) % 122) + 97).toChar else ((c + key) % 123).toChar
    } else c
  }

  def decrypt(message: String, key: Int): String = {
    for (c <- message) yield if (c <= 90 && c >= 65) {
      if ((c - key) < 65) (((c - key + 90) % 65) + 65).toChar else ((c - key) % 90).toChar
    } else if (c <= 122 && c >= 97) {
      if ((c - key < 97)) (((c - key + 122) % 97) + 97).toChar else ((c - key) % 122).toChar
    } else c
  }
}
