package cities.skylines

import scala.util.Random

//This singleton object is created to give other classes access to the 3 variables
object Adventure {
  
  var numberOfRightAnswers = 0
  
  val player = new Player(Area.initalRandomCountry)
  
  var smokeCounter = 0

}

/** The class `Adventure` represents text adventure games. An adventure consists of a player and
  * a number of areas that make up the game world. It provides methods for playing the game one
  * turn at a time and for checking the state of the game.
  *
  * N.B. This version of the class has a lot of "hard-coded" information which pertain to a very
  * specific adventure game that involves a small trip through a twisted forest. All newly created
  * instances of class `Adventure` are identical to each other. To create other kinds of adventure
  * games, you will need to modify or replace the source code of this class. */
class Adventure {

  
  /** The title of the adventure game. */
  val title = "Cities: HighTides"

  
  private val Finland     = new Area("finland", "helsinki")
  private val Syria       = new Area("syria", "damascus")
  private val Sudan       = new Area("sudan", "khartoum")
  private val Russia      = new Area("russia", "moscow")
  private val China       = new Area("china", "beijing")
  private val Nigeria     = new Area("nigeria", "aduja")
  private val Cuba        = new Area ("cuba", "havana")
  
  
  Area.pickRandomCountry.addItem(new Item("Pepsi", "The label says: 'Magic Juice'."))
  Area.pickRandomCountry.addItem(new Item("Cigar", "It's brown in color."))

  /** The character that the player controls in the game. */
  val player = Adventure.player
  
  /** Determines if the adventure is complete, that is, if the player has won. */
  def isComplete = (Adventure.numberOfRightAnswers >= 5 && Adventure.smokeCounter==1)

  /** Determines whether the player has won, lost, or quit, thereby ending the game. */
  def isOver = this.isComplete || player.hasQuit || Adventure.smokeCounter>1

  /** Returns a message that is to be displayed to the player at the beginning of the game. */
  def welcomeMessage = 
"""Welcome to Cities: Skylines
You would think this game has something to do with the original game that you've maybe heard of.
Maybe this game has some nice graphics and natural disasters. No. 
Maybe it still has some epic traffic simulation and realistic game progression.
The answer is no. The answer is probably no to all your questions. 
The sole purpose of the game title is to gather revenue. 
We just copied the name for marketing reasons. 
Hopefully your expectations for the game are as low as they get, 
because otherwise they will get even lower when the game starts.

How to play the game:
This game is going to test your geography knowledge.
To get an answer right, you need to write the name of the capital of the country you are located in. 
You can find items along your journey. The game will tell you if there is an item in your current location.
To win the game you need to complete 2 tasks: 
You need to answer correctly 5 times(write the capital of the country you are in)
Find the cigar and smoke it. Don't smoke too much!
You can always say next or skip if you want to move to another country.
Write "help" in the console to get access to all the commands and for additional instructions"""


  /** Returns a message that is to be displayed to the player at the end of the game. The message
    * will be different depending on whether or not the player has completed their quest. */
  def goodbyeMessage = {
    if (this.isComplete)
      "Well done! You're an absolute expert. Game completed."
    else  // game over due to player quitting
      "Not cool. You need to step up your game. Game lost"
  }


  /** Plays a turn by executing the given in-game command, such as "go west". Returns a textual
    * report of what happened, or an error message if the command was unknown. In the latter
    * case, no turns elapse. */
  def playTurn(command: String) = {
    val action = new Action(command)
    val outcomeReport = action.execute(player)
        
    outcomeReport.getOrElse("Wrong answer or unknown command: \"" + command + "\".")
  }


}

