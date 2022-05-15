package cities.skylines


/** The class `Action` represents actions that a player may take in a text adventure game.
  * `Action` objects are constructed on the basis of textual commands and are, in effect,
  * parsers for such commands. An action object is immutable after creation.
  * @param input  a textual in-game command such as "go east" or "rest" */
class Action(input: String) {

  private val commandText = input.trim.toLowerCase
  private val verb        = commandText.takeWhile( _ != ' ' )
  private val modifiers   = commandText.drop(verb.length).trim


  /** Causes the given player to take the action represented by this object, assuming
    * that the command was understood. Returns a description of what happened as a result
    * of the action (such as "You go west."). The description is returned in an `Option`
    * wrapper; if the command was not recognized, `None` is returned. */
  
  
  //evalutes the string written in the console by the player
  def execute(actor: Player) = {
    
    
    val answer : String = Area.countriesAndCities(actor.location).trim.toLowerCase.takeWhile( _ != ' ' )
    
    //all the commands
    var returnValue = this.verb match {
      case "go"         => Some(actor.go(this.modifiers))
      case "quit"       => Some(actor.quit())
      case "inventory"  => Some(actor.inventory)
      case "get"        => Some(actor.get(this.modifiers))
      case "drop"       => Some(actor.drop(this.modifiers))
      case "examine"    => Some(actor.examine(this.modifiers))
      case "use"        => Some(actor.use(this.modifiers))
      case "smoke"      => Some(actor.smoke())
      case "next"       => Some(actor.goToRandom)
      case "skip"       => Some(actor.goToRandom)
      case "help"       => Some(actor.help)
      
      case other        => None
  }
    
    // If the string is the right answer to the question the numberOfRightAnswers is incremented by 1.
    if(verb==answer){Adventure.numberOfRightAnswers = Adventure.numberOfRightAnswers + 1
      returnValue = Some(actor.goToRandom)
      
    }
     returnValue
  }
  
  
  
  /** Returns a textual description of the action object, for debugging purposes. */
  override def toString = this.verb + " (modifiers: " + this.modifiers + ")"


}

