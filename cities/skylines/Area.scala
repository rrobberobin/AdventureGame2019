package cities.skylines

import scala.collection.mutable.Map

import scala.util.Random


// The singleton object's purpose is to give other classes access to a method and a variable 
object Area {
  
  //Has a map of all the created areas. The keys in the map are the areas aka. the countries. 
  //The values in the map are the names of the capitals
  var countriesAndCities : Map[Area,String] = Map()

  //Selects a random area from the countriesAndCities map
  def initalRandomCountry: Area = countriesAndCities.keys.toVector(Random.nextInt(countriesAndCities.size))
  
  //Selects a random area from the countriesAndCities map, excluding the current location
  def pickRandomCountry: Area = (countriesAndCities.keys.toBuffer-(Adventure.player.location)).apply(Random.nextInt(countriesAndCities.size-1))
}


/** The class `Area` represents locations in a text adventure game world. A game world
  * consists of areas. In general, an "area" can be pretty much anything: a room, a building,
  * an acre of forest, or something completely different. What different areas have in
  * common is that players can be located in them and that they can have exits leading to
  * other, neighboring areas. An area also has a name and a description.
  * @param name         the name of the area
  * @param description  a basic description of the area (typically not including information about items) */
class Area(var country: String, var capital: String) {
  
  //adds the area to the countriesAndCities map
  //always when an area is created the area is added to the map
  Area.countriesAndCities += this -> capital
  
  private val neighbors = Map[String, Area]()

  private var itemsInThisArea : Map[String, Item] = Map()
  
  def addItem(item: Item): Unit = {
    itemsInThisArea += item.name.toLowerCase -> item
  }
  
  def contains(itemName: String): Boolean = {
    itemsInThisArea.contains(itemName)
  }
  
  def removeItem(itemName: String): Option[Item] = {
    itemsInThisArea.remove(itemName)
  }
  
  
  /** Returns the area that can be reached from this area by moving in the given direction. The result
    * is returned in an `Option`; `None` is returned if there is no exit in the given direction. */
  def neighbor(direction: String) = this.neighbors.get(direction)


  /** Adds an exit from this area to the given area. The neighboring area is reached by moving in
    * the specified direction from this area. */
  def setNeighbor(direction: String, neighbor: Area) = {
    this.neighbors += direction -> neighbor
  }


  /** Adds exits from this area to the given areas. Calling this method is equivalent to calling
    * the `setNeighbor` method on each of the given direction--area pairs.
    * @param exits  contains pairs consisting of a direction and the neighboring area in that direction
    * @see [[setNeighbor]] */
  def setNeighbors(exits: Vector[(String, Area)]) = {
    this.neighbors ++= exits
  }


  /** Returns a multi-line description of the area as a player sees it. This includes a basic
    * description of the area as well as information about exits and items. The return
    * value has the form "DESCRIPTION\n\nExits available: DIRECTIONS SEPARATED BY SPACES".
    * The directions are listed in an arbitrary order. */
  def fullDescription = {
        
    val standard = "Welcome to "+ this.country + "\nNumber of right answers: " + Adventure.numberOfRightAnswers + " Number of smoked cigars: " + Adventure.smokeCounter
    
    if(itemsInThisArea!=Map()) standard + "\nYou see here: " + this.itemsInThisArea.keys.mkString(", ") 
    else standard
    
  }


  /** Returns a single-line description of the area for debugging purposes. */
  override def toString = this.country



}
