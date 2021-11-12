# UWR_games_engine
Prolog engine for control of Underwaterrugby seasonal leagues and tournament games.
draft version.
More to come next.
Text below is the header of the prolog file UWR.pl
   status:
   all functionality of underwaterrugby VDST rules status 2018 implemented
   engine (dynamic database) is used as
   - league structure definition
   - league and game tournament control and documentation
   - player license storage
   - timer control
   - engine should be updated (player, teams..) and can be used for now in command line modus
   - .. should be implemented on a server with web interfacing for a wider use
   - .. perhaps more to come on github the next months and years
   
   vision (via a webapplication):
   - showing game organisation is possible by retransription of rules
      into an intelligent system reflecting that rules (not only printing rules on pdf
      lie at the time of the first bible printing). Referee coordination in a match is
      a stressfull busy situation and a reasoning (KI?) system can overtake a lot of
      tasks (check of structure, player allowed to play, cap number, ejection..)
   - local key persons having access define the league structure in the year
   - local key persons having access introduce the player name 
      and references when requirements for playing exists
   - local key persons get the right the access the engine 
      and can start leagues or tournaments
   - administrator review the database at regular interval 
      and delete what is old (players without license since 2 years, old games..)
   - administrator control the access rights of persons
   - administrator control the private data law conformity
   - local league or tournament referee start/stop the games 
      and control all game happenings via a scoreboard communicating with this database
   
   due to limited programming skills, more to be done by others help:
   - put the engine on a server
   - develop a web interface (league / game setup and Game Scoreboard)
   
   comments:
   - the engine contain at the bottom an automatic self test sequence
   - contact me if any interest of use
   - can be used in any national leagues with a bit of adaptation
   - commercial use forbidden
   
   use:
   install swi-prolog on your pc or server
   in a terminal give "swipl" then press on the return key for starting the prolog main engine
   in the swipl prompt in the terminal put "consult(uwr)." and press the return key
   .. start a demo game with "testing." and return
   
   under CC BY SA creative commons 4.0 pascaldagornet at yahoo dot de
   
