package util

/**
 * Temp variables
 * @revision Sri Raghavan (srikrish@andrew.cmu.edu)
 */
object TempFactory {
  var count = 0

  class Temp(i : Int) {
    private val _id = i

    override def toString = "%t" + _id
    def id = _id

    def equals(otherTemp : Temp) = _id == otherTemp.id
  }

  def newTemp = {
    val newTemp = new Temp(count)
    count +=1
    newTemp
  }
}
