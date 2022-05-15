package cities.skylines

import scala.collection.mutable.Map

import scala.util.Random

/** A `Player` object represents a player character controlled by the real-life user of the program.
  *
  * A player object's state is mutable: the player's location and possessions can change, for instance.
  *
  * @param startingArea  the initial location of the player */
class Player(startingArea: Area) {

  private var currentLocation = startingArea        // gatherer: changes in relation to the previous location
  private var quitCommandGiven = false              // one-way flag
  
  
  
  /** Determines if the player has indicated a desire to quit the game. */
  def hasQuit = this.quitCommandGiven


  /** Returns the current location of the player. */
  def location = this.currentLocation


  /** Attempts to move the player to the given country. This is successful if there is a location with the given name.
    *  Returns a description of the result: "You travelled to 'country'." or "You can't go to 'country'." */
  def go (country: String) = {
    val destination : Option[Area] = Area.countriesAndCities.keys.zip(Area.countriesAndCities.keys).map(n=> n._1.country-> n._2).toMap.get(country)
    this.currentLocation = destination.getOrElse(this.currentLocation)
    if (destination.isDefined) "You travelled to " + country + "." else "Choose a place that exists in the game. Eg. 'go china'"
  }
  
  //Moves the player to another random country. 
  //Returns "You travelled to 'destination' ."
  def goToRandom = {
    val destination = Area.pickRandomCountry
    currentLocation = destination
    "You travelled to " + destination + "."
  }

  /** Causes the player to smoke a cigar if the player is carrying one. (This has no substantial effect in game terms).
    * Returns a description of what happened. */
  def smoke() = {
    
    if(has("cigar")){ 
      Adventure.smokeCounter += 1
      "Feels mighty fine.......I was gonna get up and find the broom, but then I felt fine..."
    }
    
    else {
      "You got to find some quality dope"
    }
    
  }


  /** Signals that the player wants to quit the game. Returns a description of what happened within
    * the game as a result (which is the empty string, in this case). */
  def quit() = {
    this.quitCommandGiven = true
    ""
  }

  
  //A map of all the carried items
  private var itemsCarried : Map[String, Item] = Map()
  
  //Picks up the given item if possible
  def get(itemName: String): String = {
    var yesOrNo = "There is no " + itemName + " here to pick up."
    val newItem = location.removeItem(itemName)
    
    if(newItem!=None) {
      itemsCarried += itemName -> newItem.get
      yesOrNo = "You pick up the " + itemName + "."
      }
    yesOrNo
  }
  
  //Returns all the items in the inventory
  def inventory: String = {
    if(itemsCarried!=Map()){
    "You are carrying\n" + itemsCarried.values.mkString("\n")
    }
    else {"You are empty-handed."}
  }
  
  //Returns true if the player is carrying the given item. Otherwise returns false.
  def has(itemName: String): Boolean = {
    itemsCarried.contains(itemName)
  }
  
  //examines the given item if you are carrying it
  def examine(itemName: String): String = {
    itemsCarried.get(itemName).map("You look closely at the "+ itemName +".\n" + _.description)
    .getOrElse("If you want to examine something, you need to pick it up first.")
  }
  
  //Drops the given item. The item is now found in the country you dropped it in.
  def drop(itemName: String): String = {
    var yesOrNo = "You don't have that!"
    val oldItem = itemsCarried.remove(itemName)
    
    if(oldItem!=None) {
      location.addItem(oldItem.get)
      yesOrNo = "You drop the "+itemName+"."
      }
    yesOrNo
    
  }
  
  //Uses the item some way if possible.
  def use (itemName:String) :String = {
    if(itemName=="cigar")smoke()
    else if(itemName=="pepsi" && has("pepsi")) "Pepsi? I hate Pepsi."
    else "What is " + itemName + "?"
  }
  
  //Retruns all string of all the instructions and commands
  def help = """Here are all the available commands, their explanations and examples:
command:------------------------explanation:------------------------------------------------------example:
go-----------------takes-you-to-the-country-you-want-to-go-to-------------------------------------go sudan
quit----------------------------quits-the-game----------------------------------------------------quit
inventory---------tells-you-what-you-have-in-your-inventory---------------------------------------inventory
get------------picks-up-the-item,-if-there-is-an-item-in-your-current-location--------------------get pepsi
drop--------------drops-the-item,-if-you-have-got-it-in-your-inventory----------------------------drop Cigar
use---------------------uses-the-item-in-some-way,-if-possible------------------------------------use pepsi
smoke---------------you-smoke-the-cigar-if-it-is-in-your-inventory--------------------------------smoke
next---------------------takes-you-to-another-random-country--------------------------------------next
skip---------------------takes-you-to-another-random-country--------------------------------------skip
help------prints-out-the-instructions-for-playing-i.e.-the-output-you're-currently-looking-at-----help

Each turn you are located in a specific country. 
Every country has a capital and your task is to guess what the name of the capital is.
You need to guess at least 5 times correctly.
You also need to find the cigar and smoke it once. If you smoke it more than once you lose
If you don't know the answers, you can look them up in the walkthrough, google them or move to another country with the commands skip/next/go.
That's all you need to know. Use the command help to anytime bring up these instructions with all the commands included."""
      
      
      
  /** Returns a brief description of the player's state, for debugging purposes. */
  override def toString = "Now at: " + this.location.country


}


