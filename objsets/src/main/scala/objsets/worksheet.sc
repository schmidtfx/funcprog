package objsets

object worksheet {
  def toTweetSet(l: List[Tweet]): TweetSet = {
    l.foldLeft(new Empty: TweetSet)((acc, x) => acc.incl(x))
  }                                               //> toTweetSet: (l: List[objsets.Tweet])objsets.TweetSet
  val tw1 = TweetReader.ParseTweets.getTweetData("gizmodo", TweetData.gizmodo)
                                                  //> tw1  : List[objsets.Tweet] = List(User: gizmodo
                                                  //| Text: Kindle Paperwhite Review: Forget Everything Else, This Is the E-Reader
                                                  //|  You Want http://t.co/737W6aNC [51], User: gizmodo
                                                  //| Text: These new Apple patents give a sneak peek at what future iPhone camera
                                                  //| s might have in store. http://t.co/0YT9rjxp [49], User: gizmodo
                                                  //| Text: Ever wonder why the sky is dark at night? Here's your answer. http://t
                                                  //| .co/eTKxkcaE [86], User: gizmodo
                                                  //| Text: The head of Homeland Security stays secure by just not using email, at
                                                  //|  all. http://t.co/W6KAFEUu [37], User: gizmodo
                                                  //| Text: This is how graphene will grow the flexible semiconductors of the futu
                                                  //| re: http://t.co/IoEvuxp4 [43], User: gizmodo
                                                  //| Text: It's the tech-based reality TV show you never knew you didn't want: ht
                                                  //| tp://t.co/j9J8gAo8 [19], User: gizmodo
                                                  //| Text: How do you make your Steve Jobs sculpture stand out? Easy, mix in some
                                                  //|  trash you stole from him. http://t.co/mvHBj3CH [15], User: gizmodo
                                                  //| Te
                                                  //| Output exceeds cutoff limit.
  val tw2 = tw1.slice(0, 10)                      //> tw2  : List[objsets.Tweet] = List(User: gizmodo
                                                  //| Text: Kindle Paperwhite Review: Forget Everything Else, This Is the E-Reader
                                                  //|  You Want http://t.co/737W6aNC [51], User: gizmodo
                                                  //| Text: These new Apple patents give a sneak peek at what future iPhone camera
                                                  //| s might have in store. http://t.co/0YT9rjxp [49], User: gizmodo
                                                  //| Text: Ever wonder why the sky is dark at night? Here's your answer. http://t
                                                  //| .co/eTKxkcaE [86], User: gizmodo
                                                  //| Text: The head of Homeland Security stays secure by just not using email, at
                                                  //|  all. http://t.co/W6KAFEUu [37], User: gizmodo
                                                  //| Text: This is how graphene will grow the flexible semiconductors of the futu
                                                  //| re: http://t.co/IoEvuxp4 [43], User: gizmodo
                                                  //| Text: It's the tech-based reality TV show you never knew you didn't want: ht
                                                  //| tp://t.co/j9J8gAo8 [19], User: gizmodo
                                                  //| Text: How do you make your Steve Jobs sculpture stand out? Easy, mix in some
                                                  //|  trash you stole from him. http://t.co/mvHBj3CH [15], User: gizmodo
                                                  //| Te
                                                  //| Output exceeds cutoff limit.
  val tw3 = TweetReader.ParseTweets.getTweetData("TechCrunch", TweetData.TechCrunch)
                                                  //> tw3  : List[objsets.Tweet] = List(User: TechCrunch
                                                  //| Text: Resignation Media Hires CEO John Ellis To Run Tapiture, Its Fast-Growi
                                                  //| ng Pinterest For Men  http://t.co/ctn7oWJc by @anthonyha [18], User: TechCru
                                                  //| nch
                                                  //| Text: FreedomPop Opens Its Freemium Internet Service To The Masses With New 
                                                  //| Public Beta http://t.co/35mA9Adp by @chrisvelazco [27], User: TechCrunch
                                                  //| Text: Dish And The Dream Of Internet TV http://t.co/y8KcSl8G by @ryanlawler 
                                                  //| [25], User: TechCrunch
                                                  //| Text: Adobe's Acrobat XI Boasts New PDF Editor And Touch-Friendly Interface 
                                                  //| ? Upgrades Start At $139 http://t.co/1YDWvlVI by @anthonyha [26], User: Tech
                                                  //| Crunch
                                                  //| Text: Testing Out Bodymetrics, The Startup That Wants To Be A Denim Shopper'
                                                  //| s Best Friend [TCTV] http://t.co/sPe6wA02 by @loyalelectron [22], User: Tech
                                                  //| Crunch
                                                  //| Text: Up Close With The Next Big Home Commodity: LED Lighting http://t.co/nG
                                                  //| PSMnMH [17], User: TechCrunch
                                                  //| Text: Cloning Instagram For Video Will Not R
                                                  //| Output exceeds cutoff limit.
  val tw4 = tw3.slice(0, 10)                      //> tw4  : List[objsets.Tweet] = List(User: TechCrunch
                                                  //| Text: Resignation Media Hires CEO John Ellis To Run Tapiture, Its Fast-Growi
                                                  //| ng Pinterest For Men  http://t.co/ctn7oWJc by @anthonyha [18], User: TechCru
                                                  //| nch
                                                  //| Text: FreedomPop Opens Its Freemium Internet Service To The Masses With New 
                                                  //| Public Beta http://t.co/35mA9Adp by @chrisvelazco [27], User: TechCrunch
                                                  //| Text: Dish And The Dream Of Internet TV http://t.co/y8KcSl8G by @ryanlawler 
                                                  //| [25], User: TechCrunch
                                                  //| Text: Adobe's Acrobat XI Boasts New PDF Editor And Touch-Friendly Interface 
                                                  //| ? Upgrades Start At $139 http://t.co/1YDWvlVI by @anthonyha [26], User: Tech
                                                  //| Crunch
                                                  //| Text: Testing Out Bodymetrics, The Startup That Wants To Be A Denim Shopper'
                                                  //| s Best Friend [TCTV] http://t.co/sPe6wA02 by @loyalelectron [22], User: Tech
                                                  //| Crunch
                                                  //| Text: Up Close With The Next Big Home Commodity: LED Lighting http://t.co/nG
                                                  //| PSMnMH [17], User: TechCrunch
                                                  //| Text: Cloning Instagram For Video Will Not R
                                                  //| Output exceeds cutoff limit.

  val tw5 = toTweetSet(tw1) union toTweetSet(tw4) //> tw5  : objsets.TweetSet = {{{{{{.[User: gizmodo
                                                  //| Text: (In case you're wondering who the awesome speller is, that's @brentros
                                                  //| e. He has an MFA.) [2]].}[User: TechCrunch
                                                  //| Text: (R)evolution http://t.co/dzQFqjBh by @sarahintampa [20]]{{{{{.[User: g
                                                  //| izmodo
                                                  //| Text: 10 stupid, crazy, wonderful gadget fails http://t.co/p3Al28G5 [32]]{.[
                                                  //| User: gizmodo
                                                  //| Text: 12 deadly inventions that killed their creators (not for the faint of 
                                                  //| heart) http://t.co/BCwRAzhe [82]]{.[User: gizmodo
                                                  //| Text: 14 people who should be ashamed of their @foursquare mayorship http://
                                                  //| t.co/7AEHQJLT [50]].}}}[User: gizmodo
                                                  //| Text: 18 places carbon fiber just doesn't belong: http://t.co/vJo1Yhlj [21]]
                                                  //| {.[User: gizmodo
                                                  //| Text: 18 unlucky people who already broke the iPhone 5: http://t.co/9RpvX4te
                                                  //|  [79]].}}[User: gizmodo
                                                  //| Text: A week with the iPhone 5: http://t.co/ReuK1aJs [111]].}[User: gizmodo
                                                  //| Text: A woman had a new ear grown on her arm and attached to her head and it
                                                  //| Output exceeds cutoff limit.
}