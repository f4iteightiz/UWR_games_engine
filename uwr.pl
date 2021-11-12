/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   prolog engine for underwaterrugby league and tournament control
   
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
   
   change log:
   2019 03 05, internal freeze
   2021 11 12, rework of header prior upload into Github the next days
   
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   PROLOG libraries
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
:- use_module(library(clpfd)).
:- use_module(library(main)).
:- use_module(library(socket)).
:- set_prolog_flag(verbose,silent).
:- initialization (welcome).
%
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   PROLOG declarations: dynamic updates only for game happenings 
   after the team lists are defined (not for league setup etc.)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
:- dynamic
	team/2,                   % team(TeamName,TeamID)
	team_list/3,              % team_list((TeamName,TeamID,PlayerList)
	player/4,                 % player(PlayerName,Age,InscriptionTeamName,InscriptionTeamID)
	player_defined_status/3,  % player_defined_Status(PlayerName, Player_Cap_Number, Status) Status = captain / teamleader / ejected_start / ejected_match / banned / standard
	timepenalty_status/6,     % timepenalty_status(PlayerName, TimePenalty_Type, Overall_Expected_Length, Time_In_Timepenalty_Till_Now, LastStartTime, LastStopTime)
	warn_status/2,            % warn_status(PlayerName,Nb_Warnings)
	game/5,                   % game(GameID,TeamName1,TeamID1,TeamName2,TeamID2)
	phase_game/2,             % phase_game(GameID,PhaseG) PhaseG = periodgame1 / periodgame2 / breaktime1 / breaktime2
	starttime_game/2,         % starttime_gae(GameID,StartTime)
	last_starttime_game/2,    %
	stoptime_game/2,          % at breaktime2 start
	last_stoptime_game/2,     %
	restarttime_game/2,       % at gameperiod2
	starttime_break/2,        %
	starttime_timeout/2,      %
	starttime_penalty/2,      %
	stoptime_penalty/2,       %
	timeout_taken/2,          % timeout_taken(TeamColor,TimeoutNb)
	timesequence/2,           % timesequence(GameID,Tseq) Tseq = gametime / penalty / timeout
	team_blue_starts/2,       % team_blue_starts(GameID,Side)  Side = left / right
	time_in_gameperiod/2,     %
	points/3,                 % points(GameID,TeamColor,Points)
	period_number/1.          % period_number(2) if standard VDST
%
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   initialization
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
welcome :-
    format('-------------------------------------------~n'),
    format('  prolog UnderWaterRugby scoreboard engine ~n'),
	format('  May 3rd 2019                             ~n'),
	format('  pascaldagornet(at)yahoo.de               ~n'),
	format('  based on VDST rules for league           ~n'),
	format('-------------------------------------------~n'),
	format('recommendations for use interactivly:      ~n'),
	format('  in a terminal give "swipl"               ~n'),
	format('  in swipl -> "consult(uwr)."              ~n'),	
	format('           -> "activate.." and all other   ~n'),	
	format('              commands                     ~n'),	
	format('           -> regularly "submit_.."        ~n'),	
	format('testing:                                   ~n'),	
	format('  in a terminal give "swipl"               ~n'),
	format('  in swipl -> "consult(uwr)."              ~n'),	
	format('           -> "testing."                   ~n'),		
	format('end with "leave." and press return key     ~n'),
	format('  in testphase. Not for official league use~n'),
	format('-------------------------------------------~n').
	%create_server(54321). % take out for debugging 
%
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   leaving command
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
leave :- 
    halt(0).     
%
%
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   process setups for interfacing with server / Webapp. NOT completed.
   DONT WORK FOR NOW
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
%
create_server(Port) :-
        tcp_socket(Socket),
		%gethostname(Host),   HOST:PORT
		tcp_bind(Socket, Port),
        tcp_listen(Socket, 5),
        tcp_open_socket(Socket, AcceptFd, _),
        dispatch(AcceptFd).
%
dispatch(AcceptFd) :-
        tcp_accept(AcceptFd, Socket, Peer),
        thread_create(process_client(Socket, Peer), _,[detached(true)]),
        dispatch(AcceptFd).
%
process_client(Socket, _Peer) :-
        setup_call_cleanup(tcp_open_socket(Socket, In, Out),handle_service(In, Out),close_connection(In, Out)).
%
close_connection(In, Out) :-
        close(In, [force(true)]),
        close(Out, [force(true)]).
%
handle_service(In, Out) :-
    read(In, CommandFromClient),
    writeln(CommandFromClient),
    (   CommandFromClient == end_of_file
    ->  true
    ;   
        format(Out, 'seen(~q)~n', [CommandFromClient]),
        flush_output(Out),
        handle_service(In, Out)
    ).
%
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   diverse underwaterrugby rules facts
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
gamesequence(breaktime1).  
gamesequence(breaktime2). 
gamesequence(periodgame1).
gamesequence(periodgame2).
gamesequence(timeout).
gamesequence(penalty).
%
timeeffective_gamesequence(breaktime1).
timeeffective_gamesequence(breaktime2).
timeeffective_gamesequence(periodgame1).
timeeffective_gamesequence(periodgame2).
%
break_gamesequence(breaktime1).  
break_gamesequence(breaktime2).
period_gamesequence(periodgame1).
period_gamesequence(periodgame2).
extra_gamesequence(timeout).
extra_gamesequence(penalty).
%
status_player(standard).
status_player(captain).
status_player(teamleader).
status_player(ejected_start).
status_player(ejected_match).
status_player(banned).
status_player(reserve).
%
status_player_timepenalty(timepenalty_standard).
status_player_timepenalty(timepenalty_double).
status_player_timepenalty(timepenalty_ejection).
status_player_timepenalty(timepenalty_ban).
%
status_player_timepenalty_for_out(timepenalty_ejection).
status_player_timepenalty_for_out(timepenalty_ban).
%
status_in_game_player(standard).
status_in_game_player(captain).
status_in_game_player(teamleader).
status_in_game_player(reserve).
%
status_out_player(ejected_start).
status_out_player(ejected_match).
status_out_player(banned).
%
status_player_ejected(ejected_start).
status_player_ejected(ejected_match).
%
next_gamesequence(periodgame1, breaktime1, 2).
next_gamesequence(breaktime1, periodgame2, 2). 
next_gamesequence(periodgame2, breaktime2, 2).
next_gamesequence(breaktime2, periodgame1, 2).
%next_gamesequence(breaktime1,periodgame1,1).    
% if only 1 period in a friendly tournament, it start again 
%
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   diverse VDST rules
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
color(blue).
color(white).
%
number_max_player_per_team(15).
timeout_per_team_per_period(1).
period_number(2).
% standard in league playing mode, could be 1 in friendly tournament
%
% VDST times definition in seconds
%
/*
duration(periodgame1, 900).
duration(periodgame2, 900).
duration(breaktime1, 300).
duration(breaktime2, 300).
duration(timeout, 60).
duration(penalty, 45).
duration(timepenalty_standard, 120).
duration(timepenalty_double, 240).
duration(timepenalty_ejection, 300).
duration(timepenalty_ban, 300).
*/
%
% for testing only
duration(periodgame1, 60).
duration(periodgame2, 60).
duration(breaktime1, 20).
duration(breaktime2, 20).
duration(timeout, 30).
duration(penalty, 15).
duration(timepenalty_standard, 10).
duration(timepenalty_double, 20).
duration(timepenalty_ejection, 30).
duration(timepenalty_ban, 30).
%
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    according VDST first sorted team is blue, the other is white
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */	
%
team_color(Game_Nr, Team_Name, Team_Nb, Team_Color) :-  
    game(Game_Nr, Team_Name, Team_Nb, _, _), Team_Color = blue,!.
team_color(Game_Nr, Team_Name, Team_Nb, Team_Color) :-
    game(Game_Nr, _, _, Team_Name, Team_Nb), Team_Color = white.
%
team_of_other_color(Game_Nr, Team_Name, Team_Nb, Team_Color) :- 
    game(Game_Nr, Team_Name, Team_Nb, _, _), Team_Color = white,!.
team_of_other_color(Game_Nr, Team_Name, Team_Nb, Team_Color) :-
    game(Game_Nr, _, _, Team_Name, Team_Nb), Team_Color = blue.
%
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   a rule which
       give some information if player smaller than 19years
       fail if capnumber not correct
       fail is status has a wrong definition
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
listplayers_parental_agreement_and_capnumber_and(_,[]).
listplayers_parental_agreement_and_capnumber_and(Team_Name,[H|T]) :- 
    player(H,Age,_,_),
	player_defined_status(H,Cap_Number,Player_Status),
	(   Age < 19 
	    ->
	    write('info: team '),
	    write(Team_Name),
	    write(' player '),
	    write(H),
	    write(' <19 years, verify parent agreement exists'),
	    nl
	    ;
	    true
    ),
	(
	    Cap_Number in 1..99 
	    ->                          
	    true
	    ;
	    write('error: playing player '),
	    write(H),
	    write(' with number differ 1..99 not allowed in team '),
	    write(Team_Name),nl,fail
    ),
    (	
	    status_player(Player_Status)
		->
		true
		;
		write('error: player status of '),
		write(H),
		write(' not allowed in team '),
		write(Team_Name),nl,fail
	), 
	listplayers_parental_agreement_and_capnumber_and(Team_Name,T).
%
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   rule which check if a player is in a team
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
player_in_team(Player_Name,Team_Name,Team_Nb) :-
    team_list(Team_Name,Team_Nb,Player_List),
	member(Player_Name,Player_List).
% 
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   rule which check if a player is correctly inscripted
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
player_inscription_in_one_team(Player_Name) :-  % verify player inscript 
    bagof(Team_Name,player(Player_Name,Age,Team_Name,_),Teams_List), 
	length(Teams_List,Number_Teams), 
	(   Number_Teams > 2 
	    ->	
	    write('error: player '),
		write(Player_Name),
		write(' inscription was wrongly done in more than 2 teams'),nl,
		fail  
	;
		Number_Teams > 1, Age > 21 ->		
	    write('error: player '),
		write(Player_Name),
		write(' inscription was wrongly done 2 times -> take him out of the player list'),nl,
		fail  
	;
	    Number_Teams > 1, Age < 22 ->		
	    write('info: young player identified in several teams -> verify its start ability (main inscription team is lower league) '),
	    nl,
		true  
	;
	true
	).
%
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   rule which check if a player is ok for playing in 2 teams
   
   age > 21
   player of a lower league can play 3 games in higher league, 
       -> not implemented here; 
          could define a counter (no 100% functionality) 
          or better, put the info on his pass, 
       or create a database having all players pass at nationwide level
          
   age < 21
  a young player can play in 2 different teams; when the level is higher
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
listplayers_allowed_play_in_two_team(_, _, []).                               
listplayers_allowed_play_in_two_team(Play_Team_Name, Play_Team_Nb, [H|T]) :-
    player(H, Age, Inscription_Team_Name, Inscription_Team_Nb),                                         % the team where the player was originally declared
    team_league_inscription(Inscription_Team_Name, Inscription_Team_Nb, League_Level_Inscription_Team, _),            % backtrack the league levels
    team_league_inscription(Play_Team_Name, Play_Team_Nb, Play_Team_League_Level, _),
    (
        Play_Team_Name = Inscription_Team_Name, Play_Team_Nb = Inscription_Team_Nb  ->             % all fine the player play in the team he was declared
        true
        ;                                               
        Play_Team_Name = Inscription_Team_Name,                                                   
        dif(Play_Team_Nb,Inscription_Team_Nb),
        lower_league(League_Level_Inscription_Team,Play_Team_League_Level)
        ->
        true
        ;
        Age > 21, dif(Play_Team_Name,Inscription_Team_Name) 
        ->                 
        write('error: player '),
        write(H),
        write(' inscription was wrongly done in 2 teams -> take him out of the player list'),nl,
        fail
        ;
        Age < 22,
        dif(Play_Team_Name,Inscription_Team_Name),
        lower_league(League_Level_Inscription_Team,Play_Team_League_Level)  ->             
        write('player <22 years '),
        write(H),
        write(' in 2 teams'),nl,                                                         
        true      
        ;
        write('error: player '),write(H),
        write(', team '),write(Play_Team_Name),write(', inscription was wrongly done -> take him out of the player list'),nl,
        fail       
        ), 
        listplayers_allowed_play_in_two_team(Play_Team_Name, Play_Team_Nb,  T).
%
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   check if a player number is existing in the team  % a player can play in 2 different teams; 
   inscription team is at the higher league level
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
playernb_in_game(Game_Nr,Player_Nb,Team_Color) :-
	team_color(Game_Nr,Team_Name,Team_Nb,Team_Color),                         % backtrack team name
	team_list(Team_Name,Team_Nb,Player_List),                                 % find player list 
    playernb_in_playerlist(Player_Nb,Player_List),!.                          % identify player name
%playernb_in_game( _,Player_Nb, Team_Color) :- write('error: player '),write(Player_Nb),write(' '), write(Team_Color),write(' dont exist'),nl.
%
playernb_in_playerlist(Player_Nb,[H|_]) :- player_defined_status(H,Player_Nb,_).
playernb_in_playerlist(Player_Nb,[_ |T]) :- playernb_in_playerlist(Player_Nb,T).   %   ???????????   vgl player_number_in_playerlist
%
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   rule for team validity
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
team_validity(Team_Name, Team_Nb) :-
    team_list(Team_Name, Team_Nb, List_Players), 
    length(List_Players, Number_Players), 
	count_player_in_water_from_list(List_Players, Nb_in_Water),
	count_player_in_game_from_list(List_Players, Nb_in_Game),
	number_max_player_per_team(Nbmax),
	(
	    Number_Players > Nbmax ->
		write('error: team '),
		write(Team_Name),
		write(' has too much players -> take few out of the player list'),nl,
		fail
	;
	    Number_Players < 6 ->
		write('error: team '),
		write(Team_Name),
		write(' has not enough players to start '),nl,
		fail
	;
		Nb_in_Game < 6
		->
		write('error: team '),
		write(Team_Name),
		write(' has not enough players to start -> loose 20:0 '),nl,
		fail
	;
	    Nb_in_Water < 6
		->
		write('info: team '),
		write(Team_Name),
		write(' has not enough players in water to start'),nl,
		fail
	;
	    not(unique_capnumber(List_Players)) 
	    ->
	    write('multiple cap number'), nl, 
		fail
	;
	    count_status_in_playerlist(captain,List_Players,N1), N1>1 
		->
		write(' more than 2 captains '), nl, 
		fail
	;
	    count_status_in_playerlist(teamleader,List_Players,N1), N1>1 
		->
		write(' more than 2 teamleaders '),nl,
		fail
	;
		true
	), 
	listplayers_parental_agreement_and_capnumber_and(Team_Name,List_Players),
	%listplayers_allowed_play_in_two_team(Team_Name,Team_Nb,List_Players),
	set_captain_of_team_automatic(Team_Name, Team_Nb).
%
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   diverse rules defining the players type
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
%
player_can_play_in_game(Player):-  
    player_defined_status(Player,_,Status), 
    status_in_game_player(Status).
%
%
player_can_play_in_next_game(Player):-
    \+ player_defined_status(Player, _, banned);
    \+ player_defined_status(Player, _, ejected_start).
%
%
player_activ_in_game(Player) :- 
    player_can_play_in_game(Player),
    \+ player_defined_status(Player, _, reserve).
%
%
player_in_water(Player) :- 
    player_activ_in_game(Player),
    \+timepenalty_status(Player, _, _, _, _, _).
%
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   LEAGUE DECLARATION
   see existing definition in uwr1.de
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
league_level(beziksliga).
league_level(landesliga).
league_level(first_league).
league_level(second_league).
%
league_below(bezirksliga, landesliga).
league_below(landesliga, second_league).
league_below(second_league, first_league).
%
lower_league(Low, High) :- league_level(Low),league_level(High),league_below(Low, High).
lower_league(Low, High) :- league_level(Low),league_level(High),league_below(X, High),lower_league(Low, X).
%
league_region(badenwuerttemberg).
league_region(bayern).
league_region(hessen).
league_region(nrw).
%
league_place(south).
league_place(north).
league_place(west).
league_place(ladies).
%
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    TEAMS DECLARATION FEW DAYS PRIOR LEAGUE STARTS
    THE TEAMS MUST HAVE BEEN ALLOWED PREVIOUSLY FOR PLAYING
    BY PAYING PARTICIPATION FEES OR BEEN RELEASED FOR THE LEAGUE
    2 CLUB TEAMS CANNOT PLAY AT THE SAME LEAGUE LEVEL
    team(name/club,team mb of the name/club)
    update necessary each season
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
team(cologne, 1).
team(stuttgart, 1).
team(stuttgart, 2).
team(stuttgart, 3).
team(dortmund, 1).
team(berlin, 2).
team_league_inscription(cologne, 1, landesliga, badenwuerttemberg).            
team_league_inscription(stuttgart, 3, landesliga, badenwuerttemberg).
team_league_inscription(dortmund, 1, landesliga, west).
team_league_inscription(berlin, 2, landesliga, north).
%
play_series(badenwuerttemberg, landesliga, [day_one,day_two,day_three]).
play_day(day_one).
%			
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    BASIC RUGBY PLAYER DECLARATION BASED ON RUGBY PASS	   
    -> DOCTOR STAMP IS IN IT
    -> A PLAYER IS ANNOUNCED TO BE PART OF A TEAM
    -> STAMP WHICH ALLOW THE PLAYER TO PLAY IN THE LEAGUE IS IN IT
    -> PLAYER OF AGE <22 CAN PLAY IN A SECOND TEAM WHATEVER CLUB (NOT THE SAME LEAGUE)
       THE START FOR THE SECOND TEAM MUST BE ANNOUNCED ON THE RUGBY PASS
    -> PLAYER OF A CLUB TEAM CAN PLAY 3 GAMES IN THE HIGHER LEAGUE LEVEL OF THE CLUB TEAM
    THE LIST MUST BE DEFINED BEFORE THE LEAGUE STARTS
    
    player(passnumber,age,team,teamindex).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */		
player(rugby_passnumber_1, 17, cologne, 1).
player(rugby_passnumber_2, 19, cologne, 1).
player(rugby_passnumber_3, 25, cologne, 1).
player(rugby_passnumber_4, 25, cologne, 1).
player(rugby_passnumber_5, 25, cologne, 1).
player(rugby_passnumber_6, 25, cologne, 1).
player(rugby_passnumber_7, 25, cologne, 1).
player(rugby_passnumber_8, 25, cologne, 1).
player(rugby_passnumber_9, 25, cologne, 1).					
player(rugby_passnumber_10, 25, cologne, 1).
player(rugby_passnumber_11, 25, cologne, 1).
player(rugby_passnumber_12, 25, cologne, 1).
player(rugby_passnumber_13, 25, cologne, 1).
player(rugby_passnumber_14, 25, cologne, 1).
player(rugby_passnumber_15, 25, cologne, 1).
player(rugby_passnumber_16, 25, cologne, 1).
player(rugby_passnumber_17, 25, cologne, 1).
%
player(rugby_passnumber_18, 16, stuttgart, 3).
player(rugby_passnumber_19, 19, stuttgart, 3).					
player(rugby_passnumber_20, 22, stuttgart, 3).
player(rugby_passnumber_21, 33, stuttgart, 3).
player(rugby_passnumber_22, 33, stuttgart, 3).
player(rugby_passnumber_23, 33, stuttgart, 3).
player(rugby_passnumber_24, 33, stuttgart, 3).
player(rugby_passnumber_25, 33, stuttgart, 3).
player(rugby_passnumber_26, 33, stuttgart, 3).
player(rugby_passnumber_27, 33, stuttgart, 3).
player(rugby_passnumber_28, 33, stuttgart, 3).
player(rugby_passnumber_29, 33, stuttgart, 2).	
%				
player(rugby_passnumber_30, 16, berlin, 2).
player(rugby_passnumber_31, 16, berlin, 2).
player(rugby_passnumber_32, 19, berlin, 2).					
player(rugby_passnumber_33, 22, berlin, 2).
player(rugby_passnumber_34, 33, berlin, 2).
player(rugby_passnumber_35, 33, berlin, 2).
player(rugby_passnumber_36, 33, berlin, 2).
player(rugby_passnumber_37, 33, berlin, 2).
player(rugby_passnumber_38, 33, berlin, 2).
player(rugby_passnumber_39, 33, berlin, 2).
player(rugby_passnumber_40, 33, berlin, 2).
player(rugby_passnumber_41, 33, berlin, 2).
player(rugby_passnumber_42, 33, berlin, 2).					
player(rugby_passnumber_43, 33, berlin, 2).
%
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    declaration of teams for the league day
    
    team_list(team_name, team index, [list of 15 players]) 
    it could be uploaded via another prolog file and could be dynamic for inluding a last minute coming player
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */	
%
team_list(cologne, 1,
[
rugby_passnumber_1,
rugby_passnumber_2,
rugby_passnumber_3,
rugby_passnumber_4,
rugby_passnumber_5,
rugby_passnumber_6,
rugby_passnumber_7,
rugby_passnumber_8,
rugby_passnumber_9,
rugby_passnumber_10,
rugby_passnumber_11,
rugby_passnumber_12,
rugby_passnumber_13,
rugby_passnumber_14,
rugby_passnumber_15
]).  
%
team_list(stuttgart, 3,
[
rugby_passnumber_18,
rugby_passnumber_19,
rugby_passnumber_20,
rugby_passnumber_21,
rugby_passnumber_22,
rugby_passnumber_23,
rugby_passnumber_24,
rugby_passnumber_25,
rugby_passnumber_26,
rugby_passnumber_27,
rugby_passnumber_28
]).
%
team_list(berlin, 2,
[
rugby_passnumber_30,
rugby_passnumber_31,
rugby_passnumber_32,
rugby_passnumber_33,
rugby_passnumber_34,
rugby_passnumber_35,
rugby_passnumber_36,
rugby_passnumber_37,
rugby_passnumber_38,
rugby_passnumber_39,
rugby_passnumber_40,
rugby_passnumber_41,
rugby_passnumber_42
]).
%
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    player dynamic setup
    player_defined_status(rugby_passnumber_xx,cap_number,captain or player or teamleader).
    status can be standard captain teamleader ejected_start ejected_match banned
    from here, all facts can change (timing, status player, timepenalty etc.)
    0 should be given because tolerated the player had no capnumber 
    when the list was given to the tournament organization
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */	
%
player_defined_status(rugby_passnumber_1,1,captain).
player_defined_status(rugby_passnumber_2,3,teamleader).
player_defined_status(rugby_passnumber_3,5,standard).
player_defined_status(rugby_passnumber_4,7,standard).
player_defined_status(rugby_passnumber_5,4,standard).
player_defined_status(rugby_passnumber_6,9,standard).
player_defined_status(rugby_passnumber_7,10,standard).
player_defined_status(rugby_passnumber_8,22,standard).
player_defined_status(rugby_passnumber_9,33,standard).
player_defined_status(rugby_passnumber_10,15,standard).
player_defined_status(rugby_passnumber_11,19,standard).
player_defined_status(rugby_passnumber_12,13,standard).
player_defined_status(rugby_passnumber_13,44,reserve).
player_defined_status(rugby_passnumber_14,11,reserve).
player_defined_status(rugby_passnumber_15,12,reserve).
%
player_defined_status(rugby_passnumber_18,1,captain).
player_defined_status(rugby_passnumber_19,3,standard).
player_defined_status(rugby_passnumber_20,5,standard).
player_defined_status(rugby_passnumber_21,7,standard).
player_defined_status(rugby_passnumber_22,4,standard).
player_defined_status(rugby_passnumber_23,9,standard).
player_defined_status(rugby_passnumber_24,10,standard).
player_defined_status(rugby_passnumber_25,22,standard).
player_defined_status(rugby_passnumber_26,33,standard).
player_defined_status(rugby_passnumber_27,12,standard).
player_defined_status(rugby_passnumber_28,19,standard).
%
player_defined_status(rugby_passnumber_30,1,captain).
player_defined_status(rugby_passnumber_31,3,teamleader).
player_defined_status(rugby_passnumber_32,5,standard).
player_defined_status(rugby_passnumber_33,7,standard).
player_defined_status(rugby_passnumber_34,4,standard).
player_defined_status(rugby_passnumber_35,9,standard).
player_defined_status(rugby_passnumber_36,10,standard).
player_defined_status(rugby_passnumber_37,22,standard).
player_defined_status(rugby_passnumber_38,33,standard).
player_defined_status(rugby_passnumber_39,12,standard).
player_defined_status(rugby_passnumber_40,19,standard).
player_defined_status(rugby_passnumber_41,20,standard).
player_defined_status(rugby_passnumber_42,21,reserve).

%
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   rule for updating all timers
   put all timers outputs into screen and/or socket for use in another programm/GUI
   every 0,2s send the command "update_emit_timers(Game_Nr)."
   sequential activation of the commands via socket (no parallel) with waiting
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
%
update_emit_timers(Game_Nr) :-  
	show_resttime_main_countdown(Game_Nr),
	show_resttime_timepenalty(Game_Nr),
	show_resttime_break(Game_Nr),
	show_resttime_timeout(Game_Nr),
	show_resttime_penalty(Game_Nr),!.
update_emit_timers(_).                                                         % for avoiding error in case game was not activated
%
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   activate the game (if valid)
   rule if game can start later
   a) if both teams are in the same league (which team number is not verified)
   b) if both teams have max 3 reserve
   c) if both teams have max 12 players of type captain teamleader standard 
   d) the captain is defined; if not, the smallest capnumber will be defined 
   e) maximum 1 teamleader
   f) all player are allowed to play in the league of the game
   g) maximum 1 captain
   h) players age <22 can play in 2 teams of different level (team inscription is done at the lower level)
   i) players age >21 can play 3 games in the same team name at the higher level (team inscription is done at the lower level)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
%
activate_game(Game_Nr, Team_Blue, Team_NbB, Team_White, Team_NbW) :-          % activate game should be done shortly before the game starts
    team_validity(Team_Blue, Team_NbB),                                       % verify several team data
    team_validity(Team_White, Team_NbW),
    dif(Team_Blue,Team_White),                                                 % teams should be from different club
    team_league_inscription(Team_Blue, _, X, _),                               % get the inscription of the team
    team_league_inscription(Team_White, _, Y, _), 
    X=Y,                                                                      % only teams of same league level
    assertz(game(Game_Nr, Team_Blue, Team_NbB, Team_White, Team_NbW)),       % include the game setup in the database
	assertz(time_in_gameperiod(Game_Nr, 0.0)),
    assertz(timeout_taken(Game_Nr, blue, 0)),
    assertz(timeout_taken(Game_Nr, white, 0)),
    assertz(team_blue_starts(Game_Nr, left)),
    assertz(points(Game_Nr, blue, 0)),
    assertz(points(Game_Nr, white, 0)),
    assertz(phase_game(Game_Nr, periodgame1)),
    assertz(timesequence(Game_Nr, gametime)),                                  % other are timesequence(GameNr,penalty)  timesequence(GameNr,timeout)
    upgrade_ejection_status_for_new_game(Team_White, Team_NbW),              
    upgrade_ejection_status_for_new_game(Team_Blue, Team_NbB),
    write('info: activating game'),nl,!.
        
activate_game(_, _, _, _, _) :- write('error: no activating game'),nl,fail.
%
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   all coutdowns sent to Out
   s.xxx as internal calculation
   mn:ss as output format
   output the main countdown into a socket "M: mn:ss"
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
%
show_resttime_main_countdown(Game_Nr) :- 
	phase_game(Game_Nr, periodgame1),                                          % game was initialized
	timesequence(Game_Nr,gametime),                                            % no penalty no timeout
	time_in_gameperiod(Game_Nr, TimeInPeriod),
	TimeInPeriod > 0.0,                                                        % game was started
	duration(periodgame1, Gametime),
	Gametime2 is float(Gametime),
%	TimeInPeriod =< Gametime2,                                                 % game not at end
	last_stoptime_game(Game_Nr, LastStopTime),
	last_starttime_game(Game_Nr, LastStartTime),
	float(LastStopTime),
	float(LastStartTime),
	LastStartTime > LastStopTime,                                              % game still running
	get_time(Time_Stamp),
	DeltaTime is Time_Stamp - LastStartTime + TimeInPeriod,                    % evaluate the new TimeInPeriod
	stamp_date_time(DeltaTime, DateTime, local),
	date_time_value(minute,DateTime, Min),
	date_time_value(second,DateTime, Sec_float),
	Sec_Int is floor(Sec_float),
	DurationS is Min*60 + Sec_Int,
	CountDown is Gametime - DurationS,                                         % evaluate rest countdown
    (   DeltaTime =< Gametime2 ->
        CountDownMM is div(CountDown,60),
	    CountDownSS is mod(CountDown,60),
	    format('M: ~d:~d~n',[CountDownMM, CountDownSS]),
		write('info: game running'),nl
		;
		retractall(last_stoptime_game(Game_Nr, _)),
		retractall(time_in_gameperiod(Game_Nr, _)),
		assertz(last_stoptime_game(Game_Nr, Time_Stamp)),
		upgrade_state(Game_Nr, Time_Stamp),
		assertz(time_in_gameperiod(Game_Nr, 0.0)),
		write('info: break ready to start'),nl
	),!.
%
show_resttime_main_countdown(Game_Nr) :- 
	phase_game(Game_Nr, periodgame1),                                          
	timesequence(Game_Nr,gametime),                                            % no penalty no timeout
	time_in_gameperiod(Game_Nr, TimeInPeriod),
	TimeInPeriod > 0.0,                                                        % game was started
	duration(periodgame1,Gametime),
	%Gametime2 is float(Gametime),
	%TimeInPeriod < Gametime2,                                                 % game not at end
	%starttime_game(Game_Nr, StartTime),
	last_stoptime_game(Game_Nr, LastStopTime),
	%restarttime_game(Game_Nr, RestartTime),
	last_starttime_game(Game_Nr,LastStartTime),
	LastStartTime < LastStopTime,                                              % game stopped                               
	DeltaTime is TimeInPeriod,
	stamp_date_time(DeltaTime,DateTime,local),
	date_time_value(minute,DateTime,Min),
	date_time_value(second,DateTime,Sec_float),
	Sec_Int is floor(Sec_float),
	DurationS is Min*60 + Sec_Int,
	CountDown is Gametime - DurationS,
    %CountDown >= 0,
    CountDownMM is div(CountDown,60),
	CountDownSS is mod(CountDown,60),
	format('M: ~d:~d~n',[CountDownMM,CountDownSS]),
	write('info: game stopped'),nl,!.
%
%
show_resttime_main_countdown(Game_Nr) :- 
	phase_game(Game_Nr, periodgame1),                                          % game was initialized                                        
	timesequence(Game_Nr, gametime),
	time_in_gameperiod(Game_Nr, TimeInPeriod),
	TimeInPeriod = 0.0,                                                        % game never stopped
	get_time(Time_Stamp),
	duration(periodgame1,Gametime),
	Gametime2 is float(Gametime),
	last_starttime_game(Game_Nr, LastStartTime),                               % game was started
	DeltaTime is Time_Stamp - LastStartTime + TimeInPeriod,                    % evaluate the new TimeInPeriod
	stamp_date_time(DeltaTime, DateTime, local),
	date_time_value(minute,DateTime, Min),
	date_time_value(second,DateTime, Sec_float),
	Sec_Int is floor(Sec_float),
	DurationS is Min*60 + Sec_Int,
	CountDown is Gametime - DurationS,                                         % evaluate rest countdown
    (   DeltaTime =< Gametime2 ->
        CountDownMM is div(CountDown,60),
	    CountDownSS is mod(CountDown,60),
	    format('M: ~d:~d~n',[CountDownMM, CountDownSS]),
		write('info: game running'),nl
		;
		retractall(last_stoptime_game(Game_Nr, _)),
		retractall(time_in_gameperiod(Game_Nr, _)),
		assertz(last_stoptime_game(Game_Nr, Time_Stamp)),
		upgrade_state(Game_Nr,Time_Stamp),
		assertz(time_in_gameperiod(Game_Nr, 0.0)),
		write('info: break ready to start'),nl
	),!.
%
show_resttime_main_countdown(Game_Nr) :- 
	phase_game(Game_Nr, periodgame1),                                          % game was initialized                                        
	timesequence(Game_Nr, gametime),
	%starttime_game(Game_Nr, StartTime),
	%last_stoptime_game(Game_Nr, LastStopTime),
	time_in_gameperiod(Game_Nr, TimeInPeriod),
	%LastStopTime = StartTime,
	TimeInPeriod = 0.0,                                                        % game never started
	duration(periodgame1,Gametime),
	CountDownMM is div(Gametime,60),
	CountDownSS is mod(Gametime,60),
	format('M: ~d:~d~n',[CountDownMM,CountDownSS]),
	write('info: game ready to start'),nl,!.
%
%
show_resttime_main_countdown(Game_Nr) :- 
	phase_game(Game_Nr, periodgame2),                                    
	timesequence(Game_Nr,gametime),                                            % no penalty no timeout
	time_in_gameperiod(Game_Nr, TimeInPeriod),
	TimeInPeriod > 0.0,                                                        % game was started
	duration(periodgame2,Gametime),
	Gametime2 is float(Gametime),
	TimeInPeriod < Gametime2,                                                  % game not at end
	last_stoptime_game(Game_Nr, LastStopTime),
	last_starttime_game(Game_Nr,LastStartTime),
	LastStartTime > LastStopTime,                                              % game still running   
	get_time(Time_Stamp),                              
	DeltaTime is Time_Stamp - LastStartTime + TimeInPeriod,                    % evaluate the new TimeInPeriod
	stamp_date_time(DeltaTime,DateTime,local),
	date_time_value(minute,DateTime,Min),
	date_time_value(second,DateTime,Sec_float),
	Sec_Int is floor(Sec_float),
	DurationS is Min*60 + Sec_Int,
	CountDown is Gametime - DurationS,                                         % evaluate rest countdown
    (   DeltaTime =< Gametime2 ->
        CountDownMM is div(CountDown,60),
	    CountDownSS is mod(CountDown,60),
	    format('M: ~d:~d~n',[CountDownMM,CountDownSS]),
		write('info: game running'),nl
		;
		retractall(last_stoptime_game(Game_Nr, _)),
		retractall(last_starttime_game(Game_Nr, _)),
		retractall(restarttime_game(Game_Nr, _)),
		retractall(time_in_gameperiod(Game_Nr,_)),
		
		assertz(last_stoptime_game(Game_Nr, Time_Stamp)),
		assertz(last_starttime_game(Game_Nr, Time_Stamp)),
		assertz(stoptime_game(Game_Nr,Time_Stamp)),
		assertz(time_in_gameperiod(Game_Nr,0.0)),
		upgrade_state(Game_Nr,Time_Stamp),
		write('info: break ready to start'),nl
	),!.
%
show_resttime_main_countdown(Game_Nr) :- 
	phase_game(Game_Nr, periodgame2),                                          
	timesequence(Game_Nr, gametime),                                           % no penalty no timeout
	time_in_gameperiod(Game_Nr, TimeInPeriod),
	TimeInPeriod > 0.0,                                                        % game was started
	duration(periodgame2,Gametime),
	Gametime2 is float(Gametime),
	TimeInPeriod < Gametime2,                                                  % game not at end
	%starttime_game(Game_Nr, StartTime),
	last_stoptime_game(Game_Nr, LastStopTime),
	%restarttime_game(Game_Nr, RestartTime),
	last_starttime_game(Game_Nr, LastStartTime),
	LastStartTime < LastStopTime,                                              % game stopped                               
	DeltaTime is TimeInPeriod,
	stamp_date_time(DeltaTime, DateTime,local),
	date_time_value(minute,DateTime, Min),
	date_time_value(second,DateTime, Sec_float),
	Sec_Int is floor(Sec_float),
	DurationS is Min*60 + Sec_Int,
	CountDown is Gametime - DurationS,
    CountDown >= 0,
    CountDownMM is div(CountDown, 60),
	CountDownSS is mod(CountDown, 60),
	format('M: ~d:~d~n',[CountDownMM, CountDownSS]),
	write('info: game stopped'),nl,!.
%
show_resttime_main_countdown(Game_Nr) :- 
	phase_game(Game_Nr, periodgame2),                                          % game was initialized                                        
	timesequence(Game_Nr, gametime),
	time_in_gameperiod(Game_Nr, TimeInPeriod),
	TimeInPeriod = 0.0,                                                        % game never stopped
	get_time(Time_Stamp),
	duration(periodgame2,Gametime),
	Gametime2 is float(Gametime),
	last_stoptime_game(Game_Nr, LastStopTime),
	last_starttime_game(Game_Nr, LastStartTime), 
	LastStartTime > LastStopTime,                                              % game was started
	DeltaTime is Time_Stamp - LastStartTime + TimeInPeriod,                    % evaluate the new TimeInPeriod
	stamp_date_time(DeltaTime, DateTime, local),
	date_time_value(minute,DateTime, Min),
	date_time_value(second,DateTime, Sec_float),
	Sec_Int is floor(Sec_float),
	DurationS is Min*60 + Sec_Int,
	CountDown is Gametime - DurationS,                                         % evaluate rest countdown
    (   DeltaTime =< Gametime2 ->
        CountDownMM is div(CountDown,60),
	    CountDownSS is mod(CountDown,60),
	    format('M: ~d:~d~n',[CountDownMM, CountDownSS]),
		write('info: game running'),nl
		;
		retractall(last_stoptime_game(Game_Nr, _)),
		retractall(time_in_gameperiod(Game_Nr, _)),
		assertz(last_stoptime_game(Game_Nr, Time_Stamp)),
		upgrade_state(Game_Nr,Time_Stamp),
		assertz(time_in_gameperiod(Game_Nr, 0.0)),
		write('info: break ready to start'),nl
	),!.
%
show_resttime_main_countdown(Game_Nr) :- 
	phase_game(Game_Nr, periodgame2),                                          
	timesequence(Game_Nr, gametime),
	\+restarttime_game(Game_Nr, _),
	time_in_gameperiod(Game_Nr, TimeInPeriod),
	TimeInPeriod = 0.0,                                                         % game 2nd period never started
	duration(periodgame2,Gametime),
	CountDownMM is div(Gametime,60),
	CountDownSS is mod(Gametime,60),
	format('M: ~d:~d~n',[CountDownMM,CountDownSS]),
	write('info: game ready to start'),nl,!.
%
show_resttime_main_countdown(Game_Nr) :- 
	phase_game(Game_Nr, PhaseG),
	timesequence(Game_Nr, gametime), 
	break_gamesequence(PhaseG).                                                % no main countdown output at break time
%                                 
show_resttime_main_countdown(Game_Nr) :- 
	phase_game(Game_Nr, PhaseG),
	period_gamesequence(PhaseG),
    timesequence(Game_Nr, TimeSeq),  
    TimeSeq = timeout,                                                          % main countdown during timeout
	time_in_gameperiod(Game_Nr, TimeInPeriod),
	duration(PhaseG,Gametime),                           
	DeltaTime is TimeInPeriod,
	stamp_date_time(DeltaTime, DateTime,local),
	date_time_value(minute,DateTime, Min),
	date_time_value(second,DateTime, Sec_float),
	Sec_Int is floor(Sec_float),
	DurationS is Min*60 + Sec_Int,
	CountDown is Gametime - DurationS,
    CountDown >= 0,
    CountDownMM is div(CountDown, 60),
	CountDownSS is mod(CountDown, 60),
	format('M: ~d:~d~n',[CountDownMM, CountDownSS]),
	write('info: game stopped'),nl,!.          
%
show_resttime_main_countdown(Game_Nr) :- 
	phase_game(Game_Nr, PhaseG),
	period_gamesequence(PhaseG),
    timesequence(Game_Nr, penalty),                                              % main countdown during penalty
	time_in_gameperiod(Game_Nr, TimeInPeriod),
	duration(PhaseG,Gametime),                          
	DeltaTime is TimeInPeriod,
	stamp_date_time(DeltaTime, DateTime,local),
	date_time_value(minute,DateTime, Min),
	date_time_value(second,DateTime, Sec_float),
	Sec_Int is floor(Sec_float),
	DurationS is Min*60 + Sec_Int,
	CountDown is Gametime - DurationS,
    CountDown >= 0,
    CountDownMM is div(CountDown, 60),
	CountDownSS is mod(CountDown, 60),
	format('M: ~d:~d~n',[CountDownMM, CountDownSS]),
	write('info: game stopped'),nl,!.          
%
show_resttime_main_countdown(_) :- write('info: main countdown fail'),!.          
%
show_resttime_break(Game_Nr) :-
    phase_game(Game_Nr,breaktime1),                                            % breaktime activated
	starttime_break(Game_Nr,StartTime),                                        % breaktime was started
	duration(breaktime1,BreakTimeSS),                                           % read standard time
    get_time(Time_Stamp),                                                      % current time
    DeltaTime is Time_Stamp - StartTime,                                       % time since start
    RestTime is float(BreakTimeSS) - DeltaTime,                                 % rest time to zero
    RestTime >= 0.0,                                                           % rest time not down to zero
	DurationSS is floor(DeltaTime),
	CountDown is BreakTimeSS - DurationSS,
	%CountDown @>= 0 ->
	CountDownMM is div(CountDown, 60),
	CountDownSS is mod(CountDown, 60),
	format('M: ~d:~d~n',[CountDownMM, CountDownSS]),
	write('info: breaktime running'),nl,!.
%
show_resttime_break(Game_Nr) :-
    phase_game(Game_Nr,breaktime1),                                            % breaktime activated
	starttime_break(Game_Nr,StartTime),                                        % breaktime was started
	duration(breaktime1,BreakTimeSS),                                           % read standard time
    get_time(Time_Stamp),                                                      % current time
    DeltaTime is Time_Stamp - StartTime,                                       % time since start
    RestTime is float(BreakTimeSS) - DeltaTime,                                 % rest time to zero
    RestTime =< 0.0,                                                           % break over
	format('M: ~d:~d~n',[0,0]),
    upgrade_state(Game_Nr,Time_Stamp),
    write('info: game can start'),nl,
	retractall(starttime_break(Game_Nr,_)),!.
%
show_resttime_break(Game_Nr) :- 
	phase_game(Game_Nr, breaktime1),                                          
	timesequence(Game_Nr, gametime),
	\+starttime_break(Game_Nr, _),                                             % break was not started
	duration(breaktime1, Breaktime),
	CountDownMM is div(Breaktime, 60),
	CountDownSS is mod(Breaktime, 60),
	format('M: ~d:~d~n',[CountDownMM, CountDownSS]),
	write('info: break ready to start'),nl,!.
%
show_resttime_break(Game_Nr) :-
    phase_game(Game_Nr,breaktime2),                                            % breaktime activated
	starttime_break(Game_Nr,StartTime),                                        % breaktime was started
	duration(breaktime2,BreakTime2),                                           % read standard time
    get_time(Time_Stamp),                                                      % current time
    DeltaTime is Time_Stamp - StartTime,                                       % time since start
    RestTime is float(BreakTime2) - DeltaTime,                                 % rest time to zero
    RestTime >= 0.0,                                                           % rest time not down to zero
	DurationS is floor(DeltaTime),
	CountDown is BreakTime2 - DurationS,
	%CountDown @>= 0 ->
	CountDownMM is div(CountDown, 60),
	CountDownSS is mod(CountDown, 60),
	format('M: ~d:~d~n',[CountDownMM, CountDownSS]),
	write('info: breaktime running'),nl,!.

show_resttime_break(Game_Nr) :-
    phase_game(Game_Nr,breaktime2),                                            % breaktime activated
	starttime_break(Game_Nr,StartTime),                                        % breaktime was started
	float(StartTime),
	duration(breaktime2,BreakTime2),                                           % read standard time
    get_time(Time_Stamp),                                                      % current time
    DeltaTime is Time_Stamp - StartTime,                                       % time since start
    RestTime is float(BreakTime2) - DeltaTime,                                 % rest time to zero
    RestTime =< 0.0,                                                           % break over
	format('M: ~d:~d~n',[0,0]),
	write('info: break stopped'),nl,
    upgrade_state(Game_Nr,Time_Stamp),
    write('info: new game can start'),nl,
	retractall(starttime_break(Game_Nr,_)),!.
%
show_resttime_break(Game_Nr) :- 
	phase_game(Game_Nr, breaktime2),                                          
	timesequence(Game_Nr, gametime),
	duration(breaktime1, Breaktime),
	CountDownMM is div(Breaktime, 60),
	CountDownSS is mod(Breaktime, 60),
	format('M: ~d:~d~n',[CountDownMM, CountDownSS]),
	write('info: time between two matches ready to start'),nl,!.
%
show_resttime_break(_).
%
% output the countdown into a socket	Seq=gametime
%	
show_resttime_timeout(Game_Nr) :-
    phase_game(Game_Nr,PhaseG),
	period_gamesequence(PhaseG),
    timesequence(Game_Nr,timeout),
	get_time(TimeStamp), 
	starttime_timeout(Game_Nr,Starttime),                                      % timeout was started
	DeltaTime is TimeStamp - Starttime,                                        % time in timeout
	stamp_date_time(DeltaTime,DateTime,local),
	date_time_value(minute,DateTime,Min),
	date_time_value(second,DateTime,Sec_float),
	Sec_Int is floor(Sec_float),
	duration(timeout,TimeoutSS),
	TimeoutFF is float(TimeoutSS),
	RestTime is TimeoutFF - DeltaTime,
    DurationS is Min*60 + Sec_Int,
	CountDown is TimeoutSS - DurationS,
	(
	    RestTime >= 0.0 
	    ->
	    CountDownMM is div(CountDown, 60),
	    CountDownSS is mod(CountDown, 60),
	    format('T: ~d:~d~n',[CountDownMM,CountDownSS]),
	    write('info: timeout running'),nl
	    ;
		retractall(starttime_timeout(_, _)),
		retractall(timesequence(Game_Nr,timeout)),
		assertz(timesequence(Game_Nr,gametime)),
		write('info: timeout stopped'),nl
    ),!.
%
%show_resttime_timeout(Game_Nr) :-
%    phase_game(Game_Nr,PhaseG),
%	break_gamesequence(PhaseG),!.                                   % no timeout during breaktime
	
%show_resttime_timeout(Game_Nr) :-
%    phase_game(Game_Nr,PhaseG),
%	period_gamesequence(PhaseG),
%    \+timesequence(Game_Nr,timeout),!.                                         % no timeout present
    
%show_resttime_timeout(Game_Nr) :-
%    phase_game(Game_Nr, PhaseG),
%	member(PhaseG,[periodgame1, periodgame2]),
%    timesequence(Game_Nr, timeout),
%	\+starttime_timeout(Game_Nr, _),!.                                         % timeout was not started
%
show_resttime_timeout(_).    % when no game exists
%
% output the countdown into a socket "P: mn:ss"
show_resttime_penalty(Game_Nr):-   
    timesequence(Game_Nr,penalty),
	starttime_penalty(Game_Nr, X),
	stoptime_penalty(Game_Nr, Y),                                            % penalty stopped   
	duration(penalty,PenaltyDurationSS),
	DeltaTime is Y - X,
	stamp_date_time(DeltaTime,DateTime, local),
	date_time_value(second, DateTime, Sec_float),
	RestTime is float(PenaltyDurationSS) - DeltaTime,
	Sec_Int is floor(Sec_float),
	DurationS is Sec_Int,
	CountDown is PenaltyDurationSS - DurationS,
	CountDownSS is mod(CountDown,60),
	(
	RestTime >= 0.0 ->
	format('P: ~d~n',[CountDownSS]),
	write('info: penalty stopped'),nl
	;
	write('info: weird penalty situation'),nl,true
	),!.
%    
show_resttime_penalty(Game_Nr):-   
    timesequence(Game_Nr,penalty),
	starttime_penalty(Game_Nr, X),
	get_time(TimeStamp), 
	duration(penalty,PenaltyDurationSS),
	DeltaTime is TimeStamp - X,
	stamp_date_time(DeltaTime,DateTime, local),
	date_time_value(second, DateTime, Sec_float),
	RestTime is float(PenaltyDurationSS) - DeltaTime,
	Sec_Int is floor(Sec_float),
	DurationS is Sec_Int,
	CountDownSS is PenaltyDurationSS - DurationS,
	(
	RestTime >= 0.0 ->
	format('P: ~d~n',[CountDownSS]),
	write('info: penalty running'),nl
	;
	write('info: penalty ended'),nl,
	sleep(2),
	retractall(starttime_penalty( _, _)),
	retractall(timesequence(Game_Nr, penalty)),
	assertz(timesequence(Game_Nr, gametime))
	),!.

show_resttime_penalty(Game_Nr):-   
    timesequence(Game_Nr,penalty),
	duration(penalty,PenaltySS),
	format('P: ~d~n',[PenaltySS]),
	write('info: penalty ready to start'),nl,!.
%
show_resttime_penalty(_).
%
% output the countdown into a socket "TPL1..3: mn:ss" timepenalty left 1st counter
% output the countdown into a socket "TPR1..3: mn:ss"
%
show_resttime_timepenalty(Nr_Game) :-
	get_time(Time_Stamp),
    team_color(Nr_Game,Team_Name_B,Team_Nb_B,blue),  
	team_color(Nr_Game,Team_Name_W,Team_Nb_W,white),  
	team_list(Team_Name_B,Team_Nb_B,Player_List_B),
	team_list(Team_Name_W,Team_Nb_W,Player_List_W),
    show_resttime_timepenalty_team_list(Time_Stamp,blue,Player_List_B),
	show_resttime_timepenalty_team_list(Time_Stamp,white,Player_List_W),!.
%
show_resttime_timepenalty(_).
%
show_resttime_timepenalty_team_list(_, _, []).    
show_resttime_timepenalty_team_list(Time_Stamp, Team_Color, [H|T]) :- 
	(   timepenalty_status(H,Timepenalty_Status,Overall_Resttime_TimepenaltySS,Time_In_Timepenalty,Lasttime_Stop,Lasttime_Start),
	    status_player_timepenalty(Timepenalty_Status) 
	    ->
		%retract(timepenalty_status(H,Timepenalty_Status,Current_Resttime_Timepenalty,Time_In_Timepenalty,Lasttime_Stop,Lasttime_Start)),
		player_defined_status(H,Nb,_),
		(   Time_In_Timepenalty =0.0, Lasttime_Start > Lasttime_Stop 
		    ->                                                               % timepenalty not stopped
		    Current_Time_In_Timepenalty is (Time_Stamp - Lasttime_Start),
		    (
		        Current_Time_In_Timepenalty > float(Overall_Resttime_TimepenaltySS) 
		        ->
		        format('TP: Nr ~d  ~s  ~d:~d~n',[Nb, Team_Color, 0, 0])
		        ;
		        Countdown_TimepenaltySS is Overall_Resttime_TimepenaltySS - floor(Current_Time_In_Timepenalty),
		        CountDownMM is div(Countdown_TimepenaltySS, 60),
	            CountDownSS is mod(Countdown_TimepenaltySS, 60),
		        format('TP: Nr ~d  ~s  ~d:~d~n',[Nb, Team_Color, CountDownMM, CountDownSS])
		    )
            ;
		    Time_In_Timepenalty =0.0, Lasttime_Start = Lasttime_Stop 
		    ->                                                                % timepenalty not started
	        CountDownMM is div(Overall_Resttime_TimepenaltySS, 60),
	        CountDownSS is mod(Overall_Resttime_TimepenaltySS, 60),
		    format('TP: Nr ~d  ~s  ~d:~d~n',[Nb, Team_Color, CountDownMM, CountDownSS])
		    ;
		    Lasttime_Stop > Lasttime_Start 
		    ->                                                                % timepenalty stopped
		    Countdown_TimepenaltySS is Overall_Resttime_TimepenaltySS - floor(Time_In_Timepenalty),
            CountDownMM is div(Countdown_TimepenaltySS, 60),
	        CountDownSS is mod(Countdown_TimepenaltySS, 60),
		    format('TP: Nr ~d  ~s  ~d:~d~n',[Nb, Team_Color, CountDownMM, CountDownSS])
		    ;
		    Lasttime_Stop < Lasttime_Start 
		    ->                                                                 % timepenalty running
		    Current_Time_In_Timepenalty is (Time_Stamp - Lasttime_Start + Time_In_Timepenalty),
		    (
		        Current_Time_In_Timepenalty > float(Overall_Resttime_TimepenaltySS) 
		        ->
		        format('TP: Nr ~d  ~s  ~d:~d~n',[Nb, Team_Color, 0, 0])
		        ;
		        Time_In_TimepenaltySS is Overall_Resttime_TimepenaltySS - floor(Current_Time_In_Timepenalty),
		        CountDownMM is div(Time_In_TimepenaltySS, 60),
	            CountDownSS is mod(Time_In_TimepenaltySS, 60),
		        format('TP: Nr ~d  ~s  ~d:~d~n',[Nb, Team_Color, CountDownMM, CountDownSS])
		    )
		    ;
		    Time_In_Timepenalty > float(Overall_Resttime_TimepenaltySS) 
		    ->                                                                 % time in timepenalty done
		    format('TP: Nr ~d  ~s  ~d:~d~n',[Nb, Team_Color, 0, 0])
		    ;
		    true
        )
	    ;	
		true
	), 
	show_resttime_timepenalty_team_list(Time_Stamp,Team_Color, T).
%
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   starting the game
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
%
start_game(Game_Nr) :-         
	restarttime_game(Game_Nr, _),
	last_starttime_game(Game_Nr, Y),
    last_stoptime_game(Game_Nr, Z),       
	timesequence(Game_Nr, gametime),
	phase_game(Game_Nr, periodgame2),
	get_time(Time_Stamp), 
    Z > Y,                                                            % the game will start again only if it was stopped
    retractall(last_starttime_game(Game_Nr, _)),
    assertz(last_starttime_game(Game_Nr, Time_Stamp)),        
	start_timer_timepenalty(Game_Nr,Time_Stamp),
	write('info: start game'),nl,
	!.   
%
start_game(Game_Nr) :-         
	time_in_gameperiod(Game_Nr, DeltaTime1),
	last_starttime_game(Game_Nr, Y),
    last_stoptime_game(Game_Nr, Z),       
	timesequence(Game_Nr, gametime),
	phase_game(Game_Nr, periodgame2),
	get_time(Time_Stamp), 
    DeltaTime1 = 0.0,                                                     % start game the first time
    Z > Y,
    assertz(restarttime_game(Game_Nr, Time_Stamp)),
    retractall(last_starttime_game(Game_Nr, _)),
    assertz(last_starttime_game(Game_Nr, Time_Stamp)),
	start_timer_timepenalty(Game_Nr, Time_Stamp),
	write('info: start game'),nl,
	!.   
%
start_game(Game_Nr) :- 
	last_starttime_game(Game_Nr, Y),
    last_stoptime_game(Game_Nr, Z),       
	timesequence(Game_Nr, gametime),
	phase_game(Game_Nr, periodgame1),
	get_time(Time_Stamp), 
    Z > Y,                                                       % the game will start again only if it was stopped                                                             
    retractall(last_starttime_game(Game_Nr, _)),
    assertz(last_starttime_game(Game_Nr, Time_Stamp)),        
	start_timer_timepenalty(Game_Nr, Time_Stamp),
	write('info: start game'),nl,
	!.   
%
start_game(Game_Nr) :- 
	timesequence(Game_Nr, gametime),
	phase_game(Game_Nr, periodgame1),
	time_in_gameperiod(Game_Nr, DeltaTime1),
	DeltaTime1 = 0.0,                                                          % if first start at all    
	%starttime_game(Game_Nr, _),                                                % was started
	get_time(Time_Stamp),                                                      
    assertz(starttime_game(Game_Nr, Time_Stamp)),
    assertz(last_starttime_game(Game_Nr, Time_Stamp)),
    %assertz(last_stoptime_game(Game_Nr, Time_Stamp)),
	start_timer_timepenalty(Game_Nr, Time_Stamp),
	write('info: start game'),nl,
	!.   
%
start_game(Game_Nr) :- 
	%last_starttime_game(Game_Nr, Y),                                           % necessary, if penalty.. it was stopped
    %last_stoptime_game(Game_Nr, Z),       
	timesequence(Game_Nr, penalty),
	stoptime_penalty(Game_Nr,_),    % the game can start at a stopped penalty in a gamesequence
	phase_game(Game_Nr, PhaseG),
	period_gamesequence(PhaseG),
	get_time(Time_Stamp), 
    %Z > Y,                                                                     
    retractall(last_starttime_game(Game_Nr, _)),
    assertz(last_starttime_game(Game_Nr, Time_Stamp)),  
    retractall(starttime_penalty(Game_Nr, _)),    
    retractall(stoptime_penalty(Game_Nr, _)),
    retractall(timesequence(Game_Nr, penalty)),
    assertz(timesequence(Game_Nr, gametime)), 
	start_timer_timepenalty(Game_Nr, Time_Stamp),
	write('info: start game'),nl,
	!. 
%
start_game(_) :- write('error: no game start'),nl.
%
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   subrule of starting the game
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
%
start_timer_timepenalty(Nr_Game, Start_Time) :-
    (team_color(Nr_Game,Team_Name_B,Team_Nb_B,blue),  
	team_color(Nr_Game,Team_Name_W,Team_Nb_W,white),  
	team_list(Team_Name_B,Team_Nb_B,Player_List_B),
	team_list(Team_Name_W,Team_Nb_W,Player_List_W)
	->
    start_timer_timepenalty_team_list(Player_List_B, Start_Time),
	start_timer_timepenalty_team_list(Player_List_W, Start_Time)
	).
%
start_timer_timepenalty_team_list([],_).   
start_timer_timepenalty_team_list([H|T],Time_Stamp) :- 
	( 
	    timepenalty_status(H, Timepenalty_Status, Overall_Resttime_Timepenalty, Time_In_Timepenalty, _, _),
	    player_defined_status(H,PlayerNb, _),
	    Timepenalty_Status = timepenalty_ejection,
	    Time_In_Timepenalty > float(Overall_Resttime_Timepenalty)
	    ->
		retractall(timepenalty_status(H, _, _, _, _, _)),
		retractall(player_defined_status(H, PlayerNb, _)),   
		assertz(player_defined_status(H,PlayerNb, ejected_start))
		%assertz(timepenalty_status(H,Timepenalty_Status,Overall_Resttime_Timepenalty,Time_In_Timepenalty,Lasttime_Stop,Time_Stamp))
		;
		timepenalty_status(H, Timepenalty_Status, Overall_Resttime_Timepenalty, Time_In_Timepenalty, _, _),
		player_defined_status(H,PlayerNb, _),
	    Timepenalty_Status = timepenalty_ban,
	    Time_In_Timepenalty > float(Overall_Resttime_Timepenalty)
	    ->
		retractall(timepenalty_status(H, _, _, _, _, _)),
		retractall(player_defined_status(H, PlayerNb, _)),   
		assertz(player_defined_status(H,PlayerNb, banned))
		;
		timepenalty_status(H, Timepenalty_Status, Overall_Resttime_Timepenalty, Time_In_Timepenalty, Lasttime_Stop, _),
	    status_player_timepenalty(Timepenalty_Status),
	    Time_In_Timepenalty =< float(Overall_Resttime_Timepenalty)
	    ->
		retractall(timepenalty_status(H, _, _, _, _, _)),
		assertz(timepenalty_status(H, Timepenalty_Status, Overall_Resttime_Timepenalty, Time_In_Timepenalty, Lasttime_Stop, Time_Stamp))
	    ;	
	    timepenalty_status(H, Timepenalty_Status, Overall_Resttime_Timepenalty, Time_In_Timepenalty, _, _),
	    status_player_timepenalty(Timepenalty_Status),
	    Time_In_Timepenalty > float(Overall_Resttime_Timepenalty)
	    ->
		retractall(timepenalty_status(H, _, _, _, _, _))
	    ;
		true     % scanning all players searching for timepenalties
	), 
	start_timer_timepenalty_team_list(T,Time_Stamp).
start_timer_timepenalty_team_list([_|T],Time_Stamp) :- start_timer_timepenalty_team_list(T,Time_Stamp).
%
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   stopping the game
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
%
stop_game(Game_Nr) :-
    phase_game(Game_Nr,PhaseG),
    member(PhaseG,[periodgame1,periodgame2]),                                  % stop only during game time
	timesequence(Game_Nr,gametime),                                            % no timeout no penalty
	time_in_gameperiod(Game_Nr,DeltaTime),
	DeltaTime > 0.0,                                                           % the game will stop only if it had a first start
	last_starttime_game(Game_Nr,Y),
    last_stoptime_game(Game_Nr,Z),	
	get_time(Time_Stamp),                         
    Z<Y,                                                                       % the game will stop only if it is running
    retract(last_stoptime_game(Game_Nr,_)),
	retract(time_in_gameperiod(Game_Nr,_)),
    assertz(last_stoptime_game(Game_Nr,Time_Stamp)),
	DeltaTime2 is Time_Stamp - Y + DeltaTime,
	assertz(time_in_gameperiod(Game_Nr,DeltaTime2)),
	stop_timer_timepenalty(Game_Nr, Time_Stamp),
	write('info: stop game'),nl,
	!.   
%
stop_game(Game_Nr) :-                                                          % first stop at all
    phase_game(Game_Nr,PhaseG),
    member(PhaseG,[periodgame1,periodgame2]),                                  % stop only during game time
	timesequence(Game_Nr,gametime),                                            % no timeout no penalty
	time_in_gameperiod(Game_Nr,DeltaTime),
	DeltaTime = 0.0,                                                           % the game will stop only if it had a first start
	last_starttime_game(Game_Nr,Y),                                            % the game was never stopped, look when it was started
	get_time(Time_Stamp)
    ->
	stop_timer_timepenalty(Game_Nr, Time_Stamp),
	retractall(time_in_gameperiod(Game_Nr,_)),
    assertz(last_stoptime_game(Game_Nr,Time_Stamp)),                           % definition of the first stop
	DeltaTime2 is Time_Stamp - Y + DeltaTime,
	assertz(time_in_gameperiod(Game_Nr,DeltaTime2)),                         % definition of the time which was running
	write('info: stop game'),nl,
	!.   
%
stop_game(_) :- write(' wont stop game '),nl.
%	
%special_stop_break_timeout(Nr_Game) :-   % nice to have ???????????????????????
%
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   subrule of stopping the game
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
%
stop_timer_timepenalty(Nr_Game, Stop_Time) :-
	(
    team_color(Nr_Game,Team_Name_B,Team_Nb_B,blue),  
	team_color(Nr_Game,Team_Name_W,Team_Nb_W,white),  
	team_list(Team_Name_B,Team_Nb_B,Player_List_B),
	team_list(Team_Name_W,Team_Nb_W,Player_List_W)
	->
    stop_timer_timepenalty_team_list(Player_List_B,Stop_Time),
	stop_timer_timepenalty_team_list(Player_List_W,Stop_Time)
	).
%
stop_timer_timepenalty_team_list([],_).   
stop_timer_timepenalty_team_list([H|T], Time_Stamp) :- 
	( 
	    timepenalty_status(H,Timepenalty_Status,Overall_TimepenaltySS,Time_In_Timepenalty, _,Lasttime_Start),
	    player_defined_status(H,PlayerNb, _),
	    status_player_timepenalty(Timepenalty_Status),
	    Time_In_Timepenalty_New is Time_In_Timepenalty + Time_Stamp - Lasttime_Start,
	    Time_In_Timepenalty_New >= float(Overall_TimepenaltySS),
	    Timepenalty_Status = timepenalty_ejection
	    ->
		retractall(timepenalty_status(H, _, _, _, _, _)),
		retractall(player_defined_status(H, _, _)),   
		assertz(player_defined_status(H,PlayerNb, ejected_start))
		;
		timepenalty_status(H,Timepenalty_Status,Overall_TimepenaltySS,Time_In_Timepenalty, _,Lasttime_Start),
		player_defined_status(H,PlayerNb, _),
	    status_player_timepenalty(Timepenalty_Status),
	    Time_In_Timepenalty_New is Time_In_Timepenalty + Time_Stamp - Lasttime_Start,
	    Time_In_Timepenalty_New >= float(Overall_TimepenaltySS),
		Timepenalty_Status = timepenalty_ban 
		-> 
		retractall(timepenalty_status(H, _, _, _, _, _)),
		retractall(player_defined_status(H, _, _)),
		assertz(player_defined_status(H,PlayerNb, banned))
		;
		timepenalty_status(H,Timepenalty_Status,Overall_TimepenaltySS,Time_In_Timepenalty, _,Lasttime_Start),
	    status_player_timepenalty(Timepenalty_Status),
	    Time_In_Timepenalty_New is Time_In_Timepenalty + Time_Stamp - Lasttime_Start,
	    Time_In_Timepenalty_New >= float(Overall_TimepenaltySS)
	    ->
	    retractall(timepenalty_status(H, _, _, _, _, _))
	    ;
		timepenalty_status(H,Timepenalty_Status,Overall_TimepenaltySS,Time_In_Timepenalty, _,Lasttime_Start),
	    status_player_timepenalty(Timepenalty_Status),
	    Time_In_Timepenalty_New is Time_In_Timepenalty + Time_Stamp - Lasttime_Start,
	    Time_In_Timepenalty_New < float(Overall_TimepenaltySS)
	    ->
	    retractall(timepenalty_status(H, _, _, _, _, _)),
        assertz(timepenalty_status(H,Timepenalty_Status,Overall_TimepenaltySS,Time_In_Timepenalty_New,Time_Stamp,Lasttime_Start))
	    ;	
		true
	), 
	stop_timer_timepenalty_team_list(T, Time_Stamp).
stop_timer_timepenalty_team_list([_|T], Time_Stamp) :- stop_timer_timepenalty_team_list(T, Time_Stamp).
%
%
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   start timeout 
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
%
start_timeout(Game_Nr,Team_Color) :-                                           % no stop planned so far
    timesequence(Game_Nr,gametime),                                            % timeout starts only if in game (no timeout, no penalty)
    phase_game(Game_Nr,PhaseG),                                                % no timeout during breaktime
    timeout_taken(Game_Nr,Team_Color,Nb),
	last_starttime_game(Game_Nr,Y),
    last_stoptime_game(Game_Nr,Z),
	get_time(TimeStamp), 
    Nb=0,                                                                      % timeout only if not already taken
	Z > Y,                                                                     % the timeout will start again only if game was stopped
    member(PhaseG,[periodgame1,periodgame2]),                                  % timeout not during breaktime                            
    write('info: start timeout team '),write(Team_Color),nl
    ->
    retractall(timesequence(Game_Nr,gametime)),
    retract(timeout_taken(Game_Nr, Team_Color, _)),
    assertz(starttime_timeout(Game_Nr,TimeStamp)),
    assertz(timesequence(Game_Nr,timeout)),
    assertz(timeout_taken(Game_Nr,Team_Color,1)),
	!.  
%
start_timeout(Game_Nr,Team_Color) :-                                           % no stop planned so far
    timesequence(Game_Nr,gametime),                                            % timeout starts only if in game (no timeout, no penalty)
    phase_game(Game_Nr,PhaseG),                                                % no timeout during breaktime
    timeout_taken(Game_Nr,Team_Color,Nb),
	last_starttime_game(Game_Nr,Y),
    last_stoptime_game(Game_Nr,Z),
    Nb=1,                                                                      % timeout already taken
	Z > Y,                                                                     % the timeout will start again only if game was stopped
    member(PhaseG,[periodgame1,periodgame2]),                                  % timeout not during breaktime                            
    write('info: timeout for team '),write(Team_Color),write(' already given in that timeperiod'),nl,
	!.   
%	
start_timeout(_,_) :- write('info: timeout wont start'),nl.
%
%
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   penalty activation (teams have time to get ready before it starts)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
%
activate_penalty(Game_Nr) :-
	timesequence(Game_Nr, penalty),                                         % penalty following a penalty
	stoptime_penalty(Game_Nr, _),                                               % if the penalty is stopped
    write('info: re-activate penalty'),nl
    ->
    retract(starttime_penalty(Game_Nr, _)),
    retract(stoptime_penalty(Game_Nr, _)),!.                                          
%
activate_penalty(Game_Nr) :-
	timesequence(Game_Nr,gametime),                                            % penalty during game time  
	last_starttime_game(Game_Nr,Y),
    last_stoptime_game(Game_Nr,Z), 
    Z>Y,                                                                       % the penalty will start only if game was stopped      
    starttime_break(Game_Nr,_),                                             % penalty can start only if breaktime not started
	write('info: no penalty possible for now'),!.
%
activate_penalty(Game_Nr) :-
	timesequence(Game_Nr,gametime),                                            % penalty during game time  
	last_starttime_game(Game_Nr,Y),
    last_stoptime_game(Game_Nr,Z), 
    Z>Y                                                                       % the penalty will start only if game was stopped      
    ->
    retractall(timesequence(Game_Nr,_)),
    assertz(timesequence(Game_Nr,penalty)),
	write('info: activate penalty'),nl,
	!.   
%
activate_penalty(_) :- write('info: no penalty activation'),nl,!.
%
%
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   start penalty after the teams are ready
   penalty can happens during a gameperiod and
   at the beginning of a break 
   (before it will be made started by the referee)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
%
start_penalty(Game_Nr) :-
	timesequence(Game_Nr,penalty),                                         % penalty possible only when activated
	starttime_penalty(Game_Nr,_),                                           % penaltry cannot restart
    stoptime_penalty(Game_Nr,_),                                            % penalty must be activated and not only stopped to start
    write('info: activate penalty first'),nl,!.
%
start_penalty(Game_Nr) :-
	timesequence(Game_Nr,penalty),                                         % penalty possible only when activated
	starttime_penalty(Game_Nr,_),                                           % penaltry cannot restart
    write('info: stop penalty first'),nl,!.
%
start_penalty(Game_Nr) :-
	timesequence(Game_Nr,penalty),                                         % penalty possible only when activated
	get_time(Time_Stamp), 
    assertz(starttime_penalty(Game_Nr,Time_Stamp)),
	write('info: start penalty'),nl,
	!.   
%
start_penalty(_) :-
	write('info: penalty wont start'),nl,!.
%
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   stop penalty
   a) if goal scored
   b) if interruption 
      -> no score 
      -> for repetition
      -> for abort
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
%
stop_penalty(Game_Nr):-                                                       
	timesequence(Game_Nr,penalty),                                          % if penalty activated
    (   starttime_penalty(Game_Nr, _),                                          % stop penalty if started
		stoptime_penalty(Game_Nr, _)                                            % stop penalty if not already stopped
        ->   
        write('info: penalty stopped'),nl
        ;
        starttime_penalty(Game_Nr, _)
        ->                                             
        get_time(Time_Stamp), 
        assertz(stoptime_penalty(Game_Nr,Time_Stamp)),                          % the secondaer countdown will stop
	    write('info: stop penalty'),nl
        ;
	    write('info: start penalty first'),nl
	),!.
%  
stop_penalty(_):- write('info: no penalty stop'),nl,!.
% 
%
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   switch game phase
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
%
upgrade_state(Game_Nr,Time_Stamp) :-
	phase_game(Game_Nr,PhaseG),
    period_number(X),
    period_gamesequence(PhaseG)
    ->
    stop_timer_timepenalty(Game_Nr, Time_Stamp),                                     % for avoiding further countdown/reset of timepenalty timers during break
    next_gamesequence(PhaseG,B,X),
    retract(phase_game(Game_Nr,_)),
    assertz(phase_game(Game_Nr,B)),
	write('info: move from '),write(PhaseG),write(' to '),write(B),nl,
	!.   
%
upgrade_state(Game_Nr,_) :-
	phase_game(Game_Nr,PhaseG),
    period_number(X),
    %break_gamesequence(PhaseG)
    (   PhaseG = breaktime1
        ->
        next_gamesequence(PhaseG,B,X),
        retract(phase_game(Game_Nr,_)),
        assertz(phase_game(Game_Nr,B)),
	    write('info: move from '),write(PhaseG),write(' to '),write(B),nl
	    ;
	    write('info: no restart of game after '),write(PhaseG),nl
	),
	!.   
%
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   start break (after a penalty should have been given)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
%
start_break(Game_Nr) :- 
    (phase_game(Game_Nr,breaktime1),
    timesequence(Game_Nr,penalty),                                         % break can only be started when penalty not running
	stoptime_penalty(Game_Nr,_)
    ->
    get_time(Time_Stamp),                                                      % put the timeout counter to zero when the break starts
    retractall(timeout_taken(_,_)),  
    retractall(timesequence(Game_Nr,penalty)),
    assertz(timesequence(Game_Nr,gametime)),
    assertz(starttime_break(Game_Nr,Time_Stamp)),
    assertz(timeout_taken(Game_Nr,blue,0)),
    assertz(timeout_taken(Game_Nr,white,0)),
	write('info: start break'),nl,!
	).   
%
start_break(Game_Nr) :- 
    (
    phase_game(Game_Nr,breaktime1),
    timesequence(Game_Nr,gametime),                                            % break can only be started when timeout or penalty not running
	\+starttime_break(Game_Nr,_)
    ->
    get_time(Time_Stamp),                                                      % put the timeout counter to zero when the break starts
    retractall(timeout_taken(_,_)),  
    assertz(starttime_break(Game_Nr,Time_Stamp)),
    assertz(timeout_taken(Game_Nr,blue,0)),
    assertz(timeout_taken(Game_Nr,white,0)),
	write('info: start break'),nl,!
	).   
%
start_break(Game_Nr) :- 
    (
    phase_game(Game_Nr,breaktime2),
    timesequence(Game_Nr,penalty),                                         % break can only be started when penalty not running
	stoptime_penalty(Game_Nr,_),                                               % penalty was stopped
    get_time(Time_Stamp)                                                      % put the timeout counter to zero when the break starts
    ->
    retractall(timeout_taken(_,_)),  
    retractall(timesequence(Game_Nr,penalty)),
    assertz(timesequence(Game_Nr,gametime)),
    assertz(starttime_break(Game_Nr,Time_Stamp)),
    clean_warnings_ejections_at_game_end(Game_Nr),
	write('info: start break'),nl,!
	).   
%
start_break(Game_Nr) :- 
    (
    phase_game(Game_Nr,breaktime2),
    timesequence(Game_Nr,gametime),
    \+starttime_break(Game_Nr,_),
    get_time(Time_Stamp)
    -> 
    retractall(timeout_taken(_,_)),                                            % put the timeout counter to zero when the break starts
    assertz(starttime_break(Game_Nr,Time_Stamp)), 
	clean_warnings_ejections_at_game_end(Game_Nr),
	write('info: start break'),nl,!
	).   
%
start_break(_) :- write('info: break wont start'),nl.
%
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   points control
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
%
increase_points(Nr_Game,Team_Color) :-	
    points(Nr_Game,Team_Color,Points_Status),
	Points_Status_New is Points_Status +1
	->
	retractall(points(Nr_Game,Team_Color, _)),
	assertz(points(Nr_Game,Team_Color,Points_Status_New)),
	write('info: increase points from team '),write(Team_Color),write(' to '),write(Points_Status_New),nl,
	!.   
%
increase_points(_,_) :-
	write('error: no point increase'),nl.
%
%
decrease_points(Nr_Game,Team_Color) :-
    points(Nr_Game,Team_Color,Points_Status),Points_Status > 0 
    ->
	    Points_Status_New is Points_Status -1,
	    retractall(points(Nr_Game,Team_Color,Points_Status)),
	    assertz(points(Nr_Game,Team_Color,Points_Status_New)),
	    write('info: decrease points from team '),write(Team_Color),write(' to '),write(Points_Status_New),nl
	;
	    write('info: cannot reduce points'),nl,!.
%
decrease_points(_,_):-
	write('error: no point decrease'),nl.
%
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   warning of players in game or the whole team
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
%
warn_player_in_game(Game_Nr,Player_Nb,Team_Color):-  
    \+playernb_in_game(Game_Nr,Player_Nb,Team_Color),  
    write('error: player '),write(Player_Nb),write(' '), write(Team_Color),write(' dont exist'),nl,!.
%
warn_player_in_game(Game_Nr, Player_Nb, Team_Color):-
    team_color(Game_Nr, Team_Name, Team_Nb, Team_Color),
	phase_game(Game_Nr, PhaseG),
	member(PhaseG,[periodgame1, breaktime1, periodgame2]),
	team_list(Team_Name,Team_Nb, Player_List),   
	name_of_playernb_from_playerlist(Player_Nb, Player_List, Player_Name),
	(
	    warn_status(Player_Name, Nb_Warning), Nb_Warning > 1 
	    ->
	    write('error: not planned so far for Nb_Warning > 1; eject? ban?'),nl,
		fail
	;
	    warn_status(Player_Name,Nb_Warning), Nb_Warning = 1 
	    ->
	    retractall(warn_status(Player_Name, _)),
	    activate_timepenalty(Game_Nr, Player_Nb, Team_Color),
	    write('info: timepenalty player '),write(Player_Nb),write(' '), write(Team_Color),write(' activated after warning'),nl
	;
	    assertz(warn_status(Player_Name, 1)),
	    write('info: player player '),write(Player_Nb),write(' '), write(Team_Color),write(' warned'),nl
	),!.
warn_player_in_game(_, _, _):- write('error: player couldnt be warned'),nl.
%
%
warn_player_in_game_from_playerlist(Game_Nr,Player_Name):-
	(
	    warn_status(Player_Name, Nb_Warning), Nb_Warning > 1 
	    ->
	    write('error: not planned so far for Nb_Warning > 1; eject? ban?'),nl,
		fail
	;
	    warn_status(Player_Name,Nb_Warning), Nb_Warning = 1 
	    ->
	    retractall(warn_status(Player_Name, _)),
	    player_defined_status(Player_Name,Player_Nb,_),
	    color_of_playername_from_game(Player_Name,Game_Nr,Team_Color),
	    activate_timepenalty(Game_Nr, Player_Nb, Team_Color)
	;
	    assertz(warn_status(Player_Name, 1)),
	    write('info: player warned'),nl
	),!.
%	
warn_player_in_game_from_playerlist(_, _, _):- write('error: player couldnt be warned'),nl.
%
warn_team_in_game(Game_Nr,Team_Color) :-
    team_color(Game_Nr, Team_Name, Team_Nb, Team_Color),
    team_list(Team_Name, Team_Nb, Player_List),          
	warn_team_list_in_game(Game_Nr,Player_List).
%
warn_team_list_in_game(_,[]).
warn_team_list_in_game(Game_Nr,[H|T]) :- 
    warn_player_in_game_from_playerlist(Game_Nr,H), 
	warn_team_list_in_game(Game_Nr,T).	
%
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    warn team or player outside a game during a tournament
   
    warnings are just counted higher
    it means at game start, a warned player with >1, should go into timepenalty
    more than 2 is so far not defined in the VDST rules
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
%
warn_player(Player_Name):- 
	(
	    warn_status(Player_Name,Nr_Warning),
        integer(Nr_Warning)	
	    ->
	    Nr_Warning_New is Nr_Warning + 1
	    ->
	    retractall(warn_status(Player_Name, _)),
	    assertz(warn_status(Player_Name,Nr_Warning_New)),
	    write('info: player '),write(Player_Name),write(' warned'),nl
	; 
	    assertz(warn_status(Player_Name,1)),
	    write('info: player warned'),nl
	).
%
warn_team(Team_Name,Team_Nb) :-
    team_list(Team_Name, Team_Nb, Player_List),          
	warn_team_list(Player_List).
%
warn_team_list([]).
warn_team_list([H|T]) :- 
    warn_player(H), 
	warn_team_list(T).	
%
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   ejections
   -> player sent for 5 min (VDST regular) in timepenalty
      no player enter for this time
   -> new player can be entered after this time
   -> captain or teamleader become standard; new captain will be defined automatically
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
%	
eject_player_in_game(Game_Nr, Player_Nb, Team_Color):-  
    \+playernb_in_game(Game_Nr, Player_Nb, Team_Color),  
    write('error: no eject, player '),write(Player_Nb),write(' '), write(Team_Color),write(' dont exist'),nl,!.
%
eject_player_in_game(Game_Nr, Player_Nb, Team_Color):- 
	write('info: request to eject player '),write(Player_Nb),write(' '), write(Team_Color),nl,
    team_color(Game_Nr, Team_Name, Team_Nb, Team_Color),                         % backtrack team name
	team_list(Team_Name, Team_Nb, Player_List),                                 % find player list 
    name_of_playernb_from_playerlist(Player_Nb, Player_List, Player_Name),       % identify player name
	eject_player(Player_Name),
	set_captain_of_team_automatic(Team_Name, Team_Nb).
%
eject_player_in_game(_,_,_).             % fail will not have an issue
%
eject_player(Player_Name):- 
	player(Player_Name,_,_,_),
	player_defined_status(Player_Name, Nb, Status),
	\+status_out_player(Status),
	get_time(Time_Stamp), 
	duration(timepenalty_ejection,Duration),
	!,
	(
		timepenalty_status(Player_Name, Timepenalty_Status, _, _, _, _), 
		member(Timepenalty_Status,[timepenalty_ejection,timepenalty_ban])
		-> 
		write('error: no eject, player '),write(Player_Name),write(' already banned/ejected'),nl,
		fail
	;
	    timepenalty_status(Player_Name, timepenalty_standard, Current_Resttime_Timepenalty, Time_In_Timepenalty, Lasttime_Stop, Lasttime_start)
	    -> 
		retractall(timepenalty_status(Player_Name, _, _, _, _, _)),
	    New_Resttime_Timepenalty is Current_Resttime_Timepenalty + Duration,
		assertz(timepenalty_status(Player_Name, timepenalty_ejection, New_Resttime_Timepenalty, Time_In_Timepenalty, Lasttime_Stop, Lasttime_start)),
		retractall(player_defined_status(Player_Name, _, _)),
		assertz(player_defined_status(Player_Name, Nb, standard)),
	    write('info: player '),write(Player_Name),write(' ejected'),nl
	;
	    assertz(timepenalty_status(Player_Name, timepenalty_ejection, Duration, 0.0, Time_Stamp, Time_Stamp)),
	    retractall(player_defined_status(Player_Name, _, _)),
		assertz(player_defined_status(Player_Name, Nb, standard)),
	    write('info: player '),write(Player_Name),write(' ejected'),nl
	).
%
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   ban 
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
%
ban_player_in_game(Game_Nr,Player_Nb,Team_Color):-  
    \+playernb_in_game(Game_Nr,Player_Nb,Team_Color),  
    write('error: no ban, player '),write(Player_Nb),write(' '), write(Team_Color),write(' dont exist'),nl,!.
%
ban_player_in_game(Game_Nr,Player_Nb,Team_Color) :- 
    team_color(Game_Nr,Team_Name,Team_Nb,Team_Color),
	team_list(Team_Name,Team_Nb,PlayerList),                                   
    name_of_playernb_from_playerlist(Player_Nb,PlayerList,Player_Name), 
	ban_player(Player_Name),
	set_captain_of_team_automatic(Team_Name, Team_Nb).                         % in case the captain was banned -> define a new one
%
ban_player(Player_Name) :- 
	player(Player_Name, _, _, _),
	player_defined_status(Player_Name, _, banned),
	write('error: player '),write(Player_Name),write(' already banned'),nl,!.
	
ban_player(Player_Name) :- 
	player(Player_Name,_,_,_),
	player_defined_status(Player_Name,Player_Nb,Status),
	status_player_ejected(Status)
	->
	retractall(player_defined_status(Player_Name, _, _)),
    assertz(player_defined_status(Player_Name,Player_Nb,banned)),
    write('info: player '),write(Player_Name),write(' banned'),nl,!.

ban_player(Player_Name) :- 
	player(Player_Name,_,_,_),
    timepenalty_status(Player_Name,timepenalty_ban, _, _, _, _),
	write('info: already in timepenalty ban cannot be banned again, player '),write(Player_Name),write(' banned'),nl,!.

ban_player(Player_Name) :- 
	player(Player_Name,_,_,_),
	player_defined_status(Player_Name, Nb, Status),
    timepenalty_status(Player_Name, Timepenalty_Status,Current_Resttime_Timepenalty,Time_In_Timepenalty,Lasttime_Stop,Lasttime_Start),
    member(Timepenalty_Status,[timepenalty_standard,timepenalty_double,timepenalty_ejection])
    ->	
	retractall(timepenalty_status(Player_Name,_,_,_,_,_)),
	duration(timepenalty_ban,Duration),
	New_Resttime_Timepenalty is Current_Resttime_Timepenalty + Duration,
	assertz(timepenalty_status(Player_Name,timepenalty_ban,New_Resttime_Timepenalty,Time_In_Timepenalty,Lasttime_Stop,Lasttime_Start)),
	retractall(player_defined_status(Player_Name, Nb, Status)),
	assertz(player_defined_status(Player_Name,Nb, standard)),            % if captain or teamleader, they become standard status
	write('info: player '),write(Player_Name),write(' banned'),nl,!.

ban_player(Player_Name) :- 
	player(Player_Name,_,_,_),
	player_defined_status(Player_Name, Nb, Status),
	duration(timepenalty_ban,Duration),
	get_time(Time_Stamp)
	-> 
    assertz(timepenalty_status(Player_Name,timepenalty_ban,Duration,0.0,Time_Stamp,Time_Stamp)),
    retractall(player_defined_status(Player_Name, Nb, Status)),
	assertz(player_defined_status(Player_Name,Nb, standard)),           % if captain or teamleader, they become standard status
    write('info: player '),write(Player_Name),write(' banned'),nl.
%
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   timepenalty activation
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
%
activate_timepenalty(Game_Nr,Player_Nb,Team_Color):-  
    \+playernb_in_game(Game_Nr,Player_Nb,Team_Color),  
    write('error: player '),write(Player_Nb),write(' '), write(Team_Color),write(' dont exist'),nl,!.
%
activate_timepenalty(Game_Nr, Player_Nb, Team_Color) :- 
    team_color(Game_Nr, Team_Name, Team_Nb, Team_Color),
    player(Player_Name, _, Team_Name, Team_Nb),
	player_defined_status(Player_Name,Player_Nb,Status), 
	status_out_player(Status),
	write('error: already ejected/banned (no timepenalty) player '),write(Player_Nb),write(' '),write(Team_Color),nl,!.
%
activate_timepenalty(Game_Nr, Player_Nb, Team_Color) :- 
    team_color(Game_Nr, Team_Name, Team_Nb, Team_Color),
    player(Player_Name, _, Team_Name, Team_Nb),
	player_defined_status(Player_Name,Player_Nb,_),
	timepenalty_status(Player_Name,timepenalty_standard,_,Time_In_Timepenalty,Lasttime_Stop,Lasttime_start)
	->
	retractall(timepenalty_status(Player_Name,_,_,_,_,_)),
	duration(timepenalty_double,Duration),
	assertz(timepenalty_status(Player_Name,timepenalty_double,Duration,Time_In_Timepenalty,Lasttime_Stop,Lasttime_start)),
	write('info: sent for double timepenalty player '),write(Player_Nb),write(' '),write(Team_Color),nl,!.
%
activate_timepenalty(Game_Nr, Player_Nb, Team_Color) :- 
    team_color(Game_Nr, Team_Name, Team_Nb, Team_Color),
    player(Player_Name, _, Team_Name, Team_Nb),
	player_defined_status(Player_Name,Player_Nb,_),
	timepenalty_status(Player_Name,timepenalty_double,Current_Resttime_Timepenalty,Time_In_Timepenalty,Lasttime_Stop,Lasttime_start) 
	->
	retractall(timepenalty_status(Player_Name,_,_,_,_,_)),
	duration(timepenalty_ejection,Duration),
	New_Resttime_Timepenalty is Current_Resttime_Timepenalty + Duration,
	assertz(timepenalty_status(Player_Name,timepenalty_ejection,New_Resttime_Timepenalty,Time_In_Timepenalty,Lasttime_Stop,Lasttime_start)),
	write('info: sent for ejection player '),write(Player_Nb),write(' '),write(Team_Color),nl,!.
%
activate_timepenalty(Game_Nr, Player_Nb, Team_Color) :- 
    team_color(Game_Nr, Team_Name, Team_Nb, Team_Color),
    player(Player_Name, _, Team_Name, Team_Nb),
	player_defined_status(Player_Name,Player_Nb,_),
	timepenalty_status(Player_Name,timepenalty_ejection,Current_Resttime_Timepenalty,Time_In_Timepenalty,Lasttime_Stop,Lasttime_start)
	->
	retractall(timepenalty_status(Player_Name,_,_,_,_,_)),
    duration(timepenalty_standard,Duration),
	New_Resttime_Timepenalty is Current_Resttime_Timepenalty + Duration,
	assertz(timepenalty_status(Player_Name,timepenalty_ban,New_Resttime_Timepenalty,Time_In_Timepenalty,Lasttime_Stop,Lasttime_start)),
	write('info: sent longer for ban player '),write(Player_Nb),write(' '),write(Team_Color),nl,!.
%
activate_timepenalty(Game_Nr, Player_Nb, Team_Color) :- 
    team_color(Game_Nr, Team_Name, Team_Nb, Team_Color),
    player(Player_Name, _, Team_Name, Team_Nb),
	player_defined_status(Player_Name,Player_Nb,_),
	timepenalty_status(Player_Name,timepenalty_ban,Current_Resttime_Timepenalty,Time_In_Timepenalty,Lasttime_Stop,Lasttime_start)
	->
	retractall(timepenalty_status(Player_Name,_,_,_,_,_)),
    duration(timepenalty_standard,Duration),
	New_Resttime_Timepenalty is Current_Resttime_Timepenalty + Duration,
	assertz(timepenalty_status(Player_Name,timepenalty_ban,New_Resttime_Timepenalty,Time_In_Timepenalty,Lasttime_Stop,Lasttime_start)),
	write('info: sent longer for ban player '),write(Player_Nb),write(' '),write(Team_Color),nl,!.
%
activate_timepenalty(Game_Nr, Player_Nb, Team_Color) :- 
    team_color(Game_Nr, Team_Name, Team_Nb, Team_Color),
    player(Player_Name, _, Team_Name, Team_Nb),
	player_defined_status(Player_Name,Player_Nb,_),
	duration(timepenalty_standard,Duration),
	get_time(Time_Stamp), 
	assertz(timepenalty_status(Player_Name,timepenalty_standard,Duration,0.0,Time_Stamp,Time_Stamp)),
	write('info: player '),write(Team_Color),write(' number '), write(Player_Nb),write(' sent to timepenalty'),nl,!.
%
%
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   clearing actively timepenalty
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
%
clear_timepenalty(Game_Nr,Player_Nb,Team_Color):-  
    \+playernb_in_game(Game_Nr,Player_Nb,Team_Color),  
    write('error: player '),write(Player_Nb),write(' '), write(Team_Color),write(' dont exist'),nl,!.
%
clear_timepenalty(Game_Nr,Player_Nb,Team_Color):-                              % during a running game, if a goal was scored and less players in the team than the other
    team_color(Game_Nr,Team_Name,Team_Nb,Team_Color),
	player(Player_Name,_, Team_Name,Team_Nb),
	player_defined_status(Player_Name,Player_Nb,Status),
	status_out_player(Status),
	write('error: already ejected/banned, player '),write(Player_Nb),write(' '),write(Team_Color),nl,!.
%
clear_timepenalty(Game_Nr,Player_Nb,Team_Color):-                              % during a running game, if a goal was scored and less players in the team than the other
    team_color(Game_Nr,Team_Name,Team_Nb,Team_Color),
	player(Player_Name,_, Team_Name,Team_Nb),
	player_defined_status(Player_Name,Player_Nb,_),
	timepenalty_status(Player_Name,timepenalty_standard,_,Time_In_Timepenalty,_,_), 
	Time_In_Timepenalty > 0.0                                                 % only if already started !
	->
	retract(timepenalty_status(Player_Name,_,_,_,_,_)),
	write('info: timepenalty cleared for player '),write(Player_Nb),write(' '),write(Team_Color),nl,!.

clear_timepenalty(Game_Nr,Player_Nb,Team_Color):-                              % during a running game, if a goal was scored and less players in the team than the other
    team_color(Game_Nr,Team_Name,Team_Nb,Team_Color),                       
	player(Player_Name,_, Team_Name,Team_Nb),
	player_defined_status(Player_Name,Player_Nb,_),
    timepenalty_status(Player_Name,Timepenalty_Status,_,_,_,_), 
    status_player_timepenalty_for_out(Timepenalty_Status),
	write('error: no timepenalty clearance, in expulsion timepenalty, player '),write(Player_Nb),write(' '),write(Team_Color),nl,!.
%
clear_timepenalty(Game_Nr,Player_Nb,Team_Color):-                              % during a running game, if a goal was scored and less players in the team than the other
    team_color(Game_Nr,Team_Name,Team_Nb,Team_Color),
	player(Player_Name,_, Team_Name,Team_Nb),
	player_defined_status(Player_Name,Player_Nb,_),
	timepenalty_status(Player_Name,timepenalty_double,DurationTP,Time_In_Timepenalty,Lasttime_Stop,Lasttime_start), 
    get_time(Time_Stamp),
    (   Lasttime_start > Lasttime_Stop 
		->
        DeltaTime is Time_Stamp - Lasttime_start + Time_In_Timepenalty         % timepenalty running
		;
		DeltaTime is Time_In_Timepenalty                                       % timepenalty stopped
    ),
	stamp_date_time(DeltaTime,DateTime,_),
	date_time_value(minute,DateTime,Min),
	date_time_value(second,DateTime,Sec_float),
	Sec_Int is floor(Sec_float),
    duration(timepenalty_standard,Duration),
    DurationS is Min*60 + Sec_Int,
    (   DurationS > DurationTP                                                 % player has already completed its full timepenalty time
		->
		retractall(timepenalty_status(Player_Name,_,_,_,_,_)),
	    write('info: timepenalty cleared, player '),write(Player_Nb),write(' '),write(Team_Color),nl
	;
		DurationS >= Duration                                                  % if player in timepenalty > standard, he stays
	    ->
		retractall(timepenalty_status(Player_Name,_,_,_,_,_)),
		assertz(timepenalty_status(Player_Name,timepenalty_standard,Duration,Time_In_Timepenalty,Lasttime_Stop,Lasttime_start)),
	    write('info: timepenalty reduced, player '),write(Player_Nb),write(' '),write(Team_Color),nl
	;
		DurationS = DurationTP                                               % if player timepenalty was not started, no reason to clear it
    ;
		DurationS < Duration                                                   % if player had less than 1 timepenalty standard to do, it can be cleared
		->
		retractall(timepenalty_status(Player_Name,_,_,_,_,_)),
	    write('info: timepenalty cleared, player '),write(Player_Nb),write(' '),write(Team_Color),nl
	),
	!.
%
clear_timepenalty(Game_Nr, Player_Nb, Team_Color):-                              
    team_color(Game_Nr, Team_Name, Team_Nb, Team_Color),                       
	player(Player_Name,_, Team_Name, Team_Nb),
	player_defined_status(Player_Name, Player_Nb, _),
    \+timepenalty_status(Player_Name, _, _, _, _, _), 
	write('error: no timepenalty clearance, player '),write(Player_Nb),write(' '),write(Team_Color),write(' not in timepenalty'),nl,!.
%
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   specific timepenalty clearing after a goal
   if the given team has more players in timepenalty
   than the team of other color
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
%
clear_timepenalty_after_goal(Game_Nr,Team_Color):- 
    last_stoptime_game(Game_Nr, LastStopTime),
    last_starttime_game(Game_Nr, LastStartTime),
    LastStartTime < LastStopTime,                                              % only when game is stopped
    team_color(Game_Nr, Team_Name, Team_Nb, Team_Color),                       % identify the team name
    team_list(Team_Name, Team_Nb, Player_List),                                % identify its player list
    team_of_other_color(Game_Nr, Team_Name2, Team_Nb2, Team_Color),
    team_list(Team_Name2, Team_Nb2, Player_List2),                                                
    team_lists_with_more_timepenalty(Player_List,Player_List2),
    player_of_smallest_timepenalty_from_playerlist(Player_List,Player_Name),
    player_defined_status(Player_Name,Player_Nb,_),
    clear_timepenalty(Game_Nr,Player_Nb,Team_Color),!.
clear_timepenalty_after_goal(_,_):-  write('info: no timepenalty clearance after goal'),nl.
%    
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   confirming if the first team has more player in timepenalty >0s
   than the second
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
team_lists_with_more_timepenalty(Player_List,Player_List2) :-
    number_player_timepenalty_started_in_playerlist(Player_List,Nb_TP),
    number_player_timepenalty_started_in_playerlist(Player_List2,Nb_TP2),
    Nb_TP > Nb_TP2.
%
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   returning the number of players which are already in timepenalty since > 0s
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
number_player_timepenalty_started_in_playerlist([], Result) :-  Result is 0.
number_player_timepenalty_started_in_playerlist([H|T], Result) :- 	
    timepenalty_status(H,Timepenalty_Status, Duration_Max, Time_In_Timepenalty, _, _),
    \+status_player_timepenalty_for_out(Timepenalty_Status),
    Time_In_Timepenalty > 0.0,
    Time_In_Timepenalty =< float(Duration_Max),
    !,
    number_player_timepenalty_started_in_playerlist(T, Result1),
    Result is Result1 + 1.
number_player_timepenalty_started_in_playerlist([_|T], Result) :-
    number_player_timepenalty_started_in_playerlist(T, Result).
%
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   finding the smallest timepenalty of a team
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
player_of_smallest_timepenalty_from_playerlist([X,Y|Rest],Player_Name) :- 
    player_of_smallest_timepenalty_from_playerlist(X, [Y|Rest], Player_Name).
player_of_smallest_timepenalty_from_playerlist(Player_Name, [], Player_Name) :- !.
player_of_smallest_timepenalty_from_playerlist(Player_Name_Min, [X|Rest], Player_Name_Final_Min) :-
    (   timepenalty_status(X,Timepenalty_Status, _, Time_In_Timepenalty, _, _),
        \+status_player_timepenalty_for_out(Timepenalty_Status),
        Time_In_Timepenalty > 0.0
        ->
        (   min_resttime_timepenalty_of_two_players(Player_Name_Min,X,New_Player_Name_Min),
            player_of_smallest_timepenalty_from_playerlist(New_Player_Name_Min, Rest, Player_Name_Final_Min)
        )
        ;
           true
    ). 
%
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   comparator of timepenalty of 2 players
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
%http://mycurlycode.blogspot.com/2016/10/write-prolog-program-to-find-maximum-of.html
min_resttime_timepenalty_of_two_players(Player_X,Player_Y,Min_TP_Player):-
    timepenalty_status(Player_X, _, X_Duration_Max, X_Time_In_Timepenalty, _, _),
    timepenalty_status(Player_Y, _, Y_Duration_Max, Y_Time_In_Timepenalty, _, _),
    X_Resttime_Timepenalty is float(X_Duration_Max) - X_Time_In_Timepenalty,
    Y_Resttime_Timepenalty is float(Y_Duration_Max) - Y_Time_In_Timepenalty,
    (   X_Resttime_Timepenalty >  Y_Resttime_Timepenalty
        ->
        Min_TP_Player  is Player_Y
        ;
        Min_TP_Player  is Player_X
    ).
%
%
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   clear at leaving the game just before the breaktime start
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
%
clean_warnings_ejections_at_game_end(Game_Nr) :-
% at ending of a gameperiod2 only
    (
    team_color(Game_Nr, Team_NameB, Team_NbB, blue),  
	team_color(Game_Nr, Team_NameW, Team_NbW, white),  
	team_list(Team_NameB, Team_NbB, Player_ListB),
	team_list(Team_NameW, Team_NbW, Player_ListW)
	->                              
	freeze_ejection_status_for_next_game_from_playerlist(Player_ListB),
	freeze_ejection_status_for_next_game_from_playerlist(Player_ListW),
	clear_warnings_from_playerlist(Player_ListB),
	clear_warnings_from_playerlist(Player_ListW),
	clear_timepenalty_from_playerlist(Player_ListB),
	clear_timepenalty_from_playerlist(Player_ListW)
	).
%
%
clear_warnings_from_playerlist([]).
clear_warnings_from_playerlist([H|T]) :- 
    (   (warn_status(H,X),integer(X)) ->
		retractall(warn_status(H,_))
	;	
		true
	), 
	clear_warnings_from_playerlist(T).
%
%
clear_timepenalty_from_playerlist([]).
clear_timepenalty_from_playerlist([H|T]) :- 
    (   timepenalty_status(H, _, _, _, _, _) 
        ->
		retractall(timepenalty_status(H, _, _, _, _, _))
	;	
		true
	), 
	clear_timepenalty_from_playerlist(T).
%
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   entering a reserve player
   if ejection or ban over 5min
   if not enough player and player is allowed to comre
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
%
enter_player_in_game(Game_Nr,Player_Nb,Team_Color):-  
    \+playernb_in_game(Game_Nr,Player_Nb,Team_Color),  
    write('error: player '),write(Player_Nb),write(' '), write(Team_Color),write(' dont exist'),nl,!.
%
enter_player_in_game(Game_Nr, Player_Nb, Team_Color):-                                   % enter a player by Nb from a team color in a game
        team_color(Game_Nr, Team_Name, Team_Nb, Team_Color),                      % identify the team name
        team_list(Team_Name, Team_Nb, Player_List),                              % identify its player list
		name_of_playernb_from_playerlist(Player_Nb, Player_List, Player_Name),   % identify the player name via its number in the list
	    player_defined_status(Player_Name, _, Status),                         % get the status of the player
	    number_player_in_water_from_list(Player_List, Number),                  % return the number of players in water = activ in game
	    ( Number<12, Status = reserve                                                            % only if the team has less than 12 players, it can enter a reserve player
		->
	    retractall(player_defined_status(Player_Name, _, _)),
	    assertz(player_defined_status(Player_Name, Player_Nb, standard)),
	    write('info: player entered'),nl
		; 
		write('error: cannot enter player due to nb player / status '),nl
		).
enter_player_in_game(_, _, _):-  write('error: cannot enter player'),nl.
%
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   send player to reserve (mostly before entering another)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
change_status_to_reserve_player_in_game(Game_Nr,Player_Nb,Team_Color):-  
    \+playernb_in_game(Game_Nr,Player_Nb,Team_Color),  
    write('error: player '),write(Player_Nb),write(' '), write(Team_Color),write(' dont exist'),nl,!.
%
change_status_to_reserve_player_in_game(Game_Nr,Player_Nb,Team_Color):-                % = send out before another is entered
        (team_color(Game_Nr,Team_Name,Team_Nb,Team_Color),
        team_list(Team_Name,Team_Nb,Player_List),
        name_of_playernb_from_playerlist(Player_Nb,Player_List,Player_Name),   % identify the player name via its number in the list
	    player_defined_status(Player_Name,Player_Nb,Status),
	    player_in_water(Player_Name)                                            
		->
	    retractall(player_defined_status(Player_Name,Player_Nb,Status)),
	    assertz(player_defined_status(Player_Name,Player_Nb,reserve)),
	    write('info: player sent to reserve'),nl
		; 
		write('cannot send player to reserve'),nl
		).
%
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   count players of same cap number in a team
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
player_number_in_playerlist(Player_Nb,[H|_],0) :-
    player_defined_status(H,Player_Nb,_),!.
player_number_in_playerlist(Player_Nb,[_|T],Index) :- 
    player_number_in_playerlist(Player_Nb,T,Index1),!, 
	Index is Index1+1.
%
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   another counting
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
number_player_in_water_from_list([H|_],0) :-
    player_in_water(H),!.
number_player_in_water_from_list([_|T],Index) :- 
    number_player_in_water_from_list(T,Index1),!, 
	Index is Index1+1.
%
% https://stackoverflow.com/questions/33988462/prolog-minimum-value-in-a-list
smallest_capnumber_for_captain_from_playerlist([X,Y|Rest],Min) :- 
	player_defined_status(X,CapX,_),  
	(player_in_water(X) 
	->  
	smallest_capnumber_for_captain_from_playerlist(CapX,[Y|Rest],Min)
	;
	smallest_capnumber_for_captain_from_playerlist([Y|Rest],Min)
	).
%
smallest_capnumber_for_captain_from_playerlist(Min,[],Min) :- !. 
% 
smallest_capnumber_for_captain_from_playerlist(Min,[X|Rest],FinalMin) :- 
	player_defined_status(X,CapX,_),
    (player_in_water(X) 
    ->
    NewMin is min(Min, CapX),
    smallest_capnumber_for_captain_from_playerlist(NewMin, Rest, FinalMin)
    ;
    smallest_capnumber_for_captain_from_playerlist(Min, Rest, FinalMin)).
%
captain_in_list([H|_]) :- player_defined_status(H,_,captain).
captain_in_list([_ |T]) :- captain_in_list(T).
%
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   return name of a player number given
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
name_of_playernb_from_playerlist(_,[],_).
name_of_playernb_from_playerlist(Player_Nb,[H|_],Player_Name) :-
    player_defined_status(H,Nb,_),
	Nb=Player_Nb,
	Player_Name=H,
	!.
name_of_playernb_from_playerlist(PlayerNb,[_|T],PlayerName) :-
    name_of_playernb_from_playerlist(PlayerNb,T,PlayerName).	
% 
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   return the color of a player by name during a game
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
color_of_playername_from_game(Player_Name,Game_Nr,Team_Color) :-
	team_color(Game_Nr, Team_Name, Team_Nb, Team_Color),
	team_list(Team_Name,Team_Nb,Player_List),
	member(Player_Name,Player_List).
%
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   in case a defect cap is exchanged with another number
   exchange of caps between players should not been allowed for 
   avoiding complication
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
change_player_number_in_game(Game_Nr,Player_Nb_Out,Player_Nb_In,Team_Color):-          % if number dont already exists
    team_color(Game_Nr,Team_Name,Team_Nb,Team_Color),                          % identify team name based on color in game
	team_list(Team_Name,Team_Nb,Player_List),                                  % identify the team list
    (   player_number_in_playerlist(Player_Nb_In,Player_List,_) 
        ->
        write('cannot change nb '),write(Player_Nb_In),write(' already in team '),write(Team_Color),nl
        ;
        name_of_playernb_from_playerlist(Player_Nb_Out,Player_List,Player_Name)
		->
	    (player_defined_status(Player_Name,_,Status)
	        ->
	        retractall(player_defined_status(Player_Name,_,_)),
	        assertz(player_defined_status(Player_Name,Player_Nb_In,Status)),
	        write('info: change player number'),nl
	    )
		; 
		write('cannot change nb'),nl
	).
%
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   rule if captain not named at team start or
   when captain sent for ejection or ban
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
set_captain_automatic_in_game(Game_Nr,Team_Color) :-                                   % set the lowest cap number as a captain
    team_color(Game_Nr,Team_Name,Team_Nb,Team_Color),                          % identify team name based on color in game
    set_captain_of_team_automatic(Team_Name,Team_Nb).
%
set_captain_of_team_automatic(Team_Name, Team_Nb) :-                                   % set the lowest cap number as a captain
    team_list(Team_Name, Team_Nb, Player_List),                                  % extract the list of players
    count_status_in_playerlist(captain, Player_List, Counter),
    ( 
        Counter = 1
        ->
        write('captain of team '),write(Team_Name),write(' '),write(Team_Nb),write(' already defined'),nl,true
        ;                                                                      % if captain dont already exists
        Counter = 0
        ->
        (   smallest_capnumber_for_captain_from_playerlist(Player_List, Cap_Nb),        % smallest capnumber identified
            name_of_playernb_from_playerlist(Cap_Nb, Player_List, Player_Name)
            ->
            retractall(player_defined_status(Player_Name, _, _)),
            assertz(player_defined_status(Player_Name, Cap_Nb, captain)),
            write('captain of team '),write(Team_Name),write(' '),write(Team_Nb),write(' new: cap number '),write(Cap_Nb),nl,true
        )
        ;
        Counter > 1
        ->
        write('captain of team '),write(Team_Name),write(' '),write(Team_Nb),write(' defined multiple'),nl,fail
    ).
%         
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   function for controlling the ejection valid for 1 match more
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
freeze_ejection_status_for_next_game(Team_Name,Team_Nb):-                       % use at game-end ONLY  timepenalty ejection change to ejection start, ejection match change to  etc.
    team_list(Team_Name,Team_Nb,Player_List),
	freeze_ejection_status_for_next_game_from_playerlist(Player_List).
%
freeze_ejection_status_for_next_game_from_playerlist([]).
freeze_ejection_status_for_next_game_from_playerlist([H|T]):- 
    (   player_defined_status(H, Cap_Nb, Status)
        -> 
        (   Status = ejected_start 
            ->
            write('player ejected in match identified'),nl
	        ;
            Status = ejected_match 
            ->
            write('end of an ejection of a player'),nl,
            retractall(player_defined_status(H, _, _)),
            assertz(player_defined_status(H, Cap_Nb, standard))
            ;
            timepenalty_status(H, timepenalty_ejection, _, _, _, _)
            ->
	        retractall(player_defined_status(H, _, _)),
	        assertz(player_defined_status(H, Cap_Nb, ejected_start))
	        ;
	        timepenalty_status(H,timepenalty_ban, _, _, _, _)
	        ->
	        retractall(player_defined_status(H, _, _)),
	        assertz(player_defined_status(H, Cap_Nb, banned))
	        ;
	        true
        )
    ),
    freeze_ejection_status_for_next_game_from_playerlist(T).
freeze_ejection_status_for_next_game_from_playerlist([_|T]):- freeze_ejection_status_for_next_game_from_playerlist(T).
%	
upgrade_ejection_status_for_new_game(Team_Name,Team_Nb):-                       % use at game-end ONLY  timepenalty ejection change to ejection start, ejection match change to  etc.
    team_list(Team_Name,Team_Nb,Player_List),
	upgrade_ejection_status_for_new_game_playerlist(Player_List).
%
upgrade_ejection_status_for_new_game_playerlist([]).
upgrade_ejection_status_for_new_game_playerlist([H|T]):- 
    player_defined_status(H,Cap_Nb,Status), 
    (
    Status = ejected_start 
    ->
    retractall(player_defined_status(H, _ , _)),
    assertz(player_defined_status(H, Cap_Nb, ejected_match))
	;
    true
    ),upgrade_ejection_status_for_new_game_playerlist(T).
upgrade_ejection_status_for_new_game_playerlist([_|T]):- upgrade_ejection_status_for_new_game_playerlist(T).
%
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   another counting
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
count_player_in_water_from_list([], Result):-  Result is 0.                    % return number of all players active in game (in timepenalty or not; not reserve
count_player_in_water_from_list([H|T], Result) :-  
    player_activ_in_game(H),
    !, 
	count_player_in_water_from_list(T, Result1), Result is Result1+1.
count_player_in_water_from_list([_|T], Result):-  count_player_in_water_from_list(T, Result).
%
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   another counting
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
count_player_in_game_from_list([], Result):-  Result is 0.              % return number of all players who can play in a game (no ejection, no ban, reserve etc. ok 
count_player_in_game_from_list([H|T], Result) :-  
    player_can_play_in_game(H),
    !, 
	count_player_in_game_from_list(T, Result1),Result is Result1+1.
count_player_in_game_from_list([_|T], Result):-  count_player_in_game_from_list(T, Result).
%
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   another counting
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
count_status_in_playerlist(_,[],0).
count_status_in_playerlist(Status,[H|L],Counter):- player_defined_status(H, _, Status),!,count_status_in_playerlist(Status,L,Counter1),Counter is Counter1+1.
count_status_in_playerlist(Status,[_|L],Counter):- count_status_in_playerlist(Status,L,Counter). 
%
%
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   scanning rule if all capnumbers are unique
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
unique_capnumber([]). 
unique_capnumber([_,[]]). 
unique_capnumber([H|T]):-not(capnumber_member(H,T)),unique_capnumber(T).
%
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   scanning if the capnumber of a player exists in a list of players
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
capnumber_member(Player_X,[Player_Y| _]) :- 
    player_defined_status(Player_X, X_Nb, _), 
    player_defined_status(Player_Y, Y_Nb, _), 
    X_Nb = Y_Nb.
capnumber_member(Player_X,[_ |T]) :- capnumber_member(Player_X,T).
%
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   logging
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
%
log_game(Game_Nr) :-
    game(Game_Nr,Team_Blue,Team_Nb_B,Team_White,Team_Nb_W),  
	write('game number: '),write(Game_Nr),nl,
	team_blue_starts(Game_Nr,BluesStarts),atomic(BluesStarts),
	write(' team blue: '), write(Team_Blue), write(' '),write(Team_Nb_B), 
	write(' team white: '), write(Team_White), write(' '),write(Team_Nb_W),nl,
	write(' team blue starts: '),write(BluesStarts),nl,
    (   starttime_game(Game_Nr,StartTime), float(StartTime)
	    ->
	    write(' date: '), get_date_time_value(date, Date, StartTime), write(Date),nl,
		write(' start time 1st period: '),get_date_time_value(time, Time, StartTime), write(Time),nl
		;
		write(' 1st period not started '),nl
	),
	(
	    restarttime_game(Game_Nr,ReStartTime),float(ReStartTime)
		-> 
		write(' start time 2nd period: '),get_date_time_value(time,Time,ReStartTime),write(Time),nl
		;
		write(' 2nd period not started '),nl
	),
	(
	    stoptime_game(Game_Nr,StopTime),float(StopTime)
		->
		write(' stop time game: '),get_date_time_value(time,Time,StopTime),write(Time),nl
		;
		write(' game not ended '),nl
	),
    log_team_in_game(Game_Nr, blue),
	log_team_in_game(Game_Nr, white).
%
log_team_in_game(Game_Nr, Team_Color):-         
    team_color(Game_Nr, Team_Name, Team_Nb, Team_Color),
    write('team status: '), write(Team_Name), write('  '), write(Team_Nb), nl,
    points(Game_Nr,Team_Color,Points),
	write('points team '),write(Team_Color),write(' is '),write(Points),nl,
    log_team_players(Team_Name, Team_Nb).
%
log_team_players(Team_Name,Team_Nb):-
    team_list(Team_Name,Team_Nb,Player_List),
    log_status_timepenalty_warnings_playerslist(Player_List).
%
log_status_timepenalty_warnings_playerslist([]).                     
log_status_timepenalty_warnings_playerslist([H|T]):- 
    player_defined_status(H, Nb, Status),
    !,                                                               % avoid backtracking
	write('  player: '), write(H), write('  has status '), write(Status), write(' (number: '), write(Nb), write(')'),nl,
	(   warn_status(H,Nb_Warning) ->
	    write('  -> Warning: '), write(Nb_Warning),nl
	; 
	    true
	),
	(
	    timepenalty_status(H,Timepenalty_Status,Total_Timepenalty,Time_In_Timepenalty,_,_),
        status_player_timepenalty(Timepenalty_Status)		
        ->
		write('  -> Timepenalty '), write(Timepenalty_Status),
		write(' rest in seconds '), RestTime is Total_Timepenalty - Time_In_Timepenalty, RestTime >0.0, write(RestTime),nl
		;
		true
	),
	log_status_timepenalty_warnings_playerslist(T).	
log_status_timepenalty_warnings_playerslist([_|T]):- log_status_timepenalty_warnings_playerslist(T).
%
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   diverse functions
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
%
member(X,[X| _]).
member(X,[_ |T]) :- member(X,T).
%
smallest([A],A) :- !.
smallest([A|B],N) :- smallest(B,N), N<A, !.
smallest([A| _ ], A).
%
% occurence in a list
% ?- count(a,[a,b,c,[a,c],a],R).
% R = 2
count(_,[],0).
count(A,[A|L],N):- !,count(A,L,N1),N is N1+1.
count(A,[_|L],N):- count(A,L,N). 
%
% returning index/List of a value in a list (-1 if not present)
indexof(Index, Item, List):-
  nth1(Index, List, Item).
indexof(-1, _, _).
%
%
get_date_time_value(Key, Value, TimeStamp) :-
    stamp_date_time(TimeStamp, DateTime, local),
    date_time_value(Key, DateTime, Value).
%
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   testing area; start with "testing." in a terminal 
   this show the commands which will be used for a game control
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
%
testing :-
    activate_game(1,stuttgart,3,cologne,1),
    update_emit_timers(1),
    start_game(1),
    sleep(1),
    update_emit_timers(1),
    sleep(1),
    update_emit_timers(1),
    activate_timepenalty(1,1,white),
    stop_game(1),
    update_emit_timers(1),
    sleep(1),
    update_emit_timers(1),
    start_timeout(1,blue),   %60s
    update_emit_timers(1),
    sleep(1),
    update_emit_timers(1),
    sleep(1),
    update_emit_timers(1),
    sleep(1),
    update_emit_timers(1),
    sleep(30),
    update_emit_timers(1),
    start_game(1),
    sleep(1),
    update_emit_timers(1),
    stop_game(1),
    update_emit_timers(1),
    sleep(1),
    update_emit_timers(1),
    activate_timepenalty(1,5,blue),
    activate_timepenalty(1,7,blue),
    update_emit_timers(1),
    sleep(1),
    update_emit_timers(1),
    start_game(1),
    update_emit_timers(1),
    sleep(1),
    update_emit_timers(1),
    sleep(1),
    update_emit_timers(1),
    stop_game(1),
    update_emit_timers(1),
    sleep(1),
    update_emit_timers(1),
    activate_penalty(1),
    sleep(1),
    update_emit_timers(1),
    start_penalty(1),
    update_emit_timers(1),
    sleep(1),
    update_emit_timers(1),
    stop_penalty(1),
    update_emit_timers(1),
    increase_points(1,white),
    %log_team_game(1,blue),  % works
    update_emit_timers(1),
    sleep(1),
    update_emit_timers(1),
    log_game(1),
    update_emit_timers(1),
    sleep(1),
    update_emit_timers(1),
    clear_timepenalty(1,5,blue),   % make it automatic after a goal ????
    update_emit_timers(1),
    %
    start_game(1),
    update_emit_timers(1),
    sleep(1),
    update_emit_timers(1),
    sleep(1),
    update_emit_timers(1),
    stop_game(1),
    update_emit_timers(1),
    sleep(1),
    update_emit_timers(1),
    activate_penalty(1),
    sleep(1),
    update_emit_timers(1),
    start_penalty(1),
    update_emit_timers(1),
    sleep(1),
    update_emit_timers(1),
    stop_penalty(1),
    update_emit_timers(1),
    increase_points(1,blue),
    update_emit_timers(1),
    sleep(1),
    update_emit_timers(1),
    log_game(1),
    update_emit_timers(1),
    sleep(1),
    update_emit_timers(1),
    clear_timepenalty_after_goal(1,white),   % make it automatic after a goal ?
    update_emit_timers(1),
    %
    start_game(1),
    update_emit_timers(1),
    sleep(1),
    update_emit_timers(1),
    %log_game(1),               % works
    update_emit_timers(1),
    stop_game(1),
    update_emit_timers(1),
    sleep(1),
    start_timeout(1,blue),       % should not start
    update_emit_timers(1),
    sleep(1),
    update_emit_timers(1),
    start_timeout(1,white),      % 60s
    update_emit_timers(1),
    sleep(29),
    update_emit_timers(1),
    sleep(1),
    update_emit_timers(1),
    sleep(1),
    update_emit_timers(1),
    start_game(1),
    update_emit_timers(1),
    sleep(1),
    update_emit_timers(1),
    stop_game(1),
    update_emit_timers(1),
    activate_timepenalty(1,5,white),
    update_emit_timers(1),
    warn_player_in_game(1,12,blue),
    update_emit_timers(1),
    activate_timepenalty(1,5,white),
    update_emit_timers(1),
    start_game(1),
    update_emit_timers(1),
    sleep(1),
    update_emit_timers(1),
    sleep(1),
    update_emit_timers(1),
    stop_game(1),
    update_emit_timers(1),
    warn_player_in_game(1,12,blue),
    update_emit_timers(1),
    start_game(1),
    update_emit_timers(1),
    sleep(1),
    update_emit_timers(1),
    sleep(1),
    update_emit_timers(1),
    stop_game(1),
    update_emit_timers(1),
    sleep(1),    
    warn_team_in_game(1,blue),    % works
    sleep(1),
    update_emit_timers(1),
    sleep(1),
    increase_points(1,white),
    sleep(1),    
    update_emit_timers(1),
    sleep(1),    
    log_team_in_game(1,blue),       % works
    update_emit_timers(1),
    sleep(1), 
    log_team_in_game(1,white),   
    update_emit_timers(1),   
    sleep(1),
    clear_timepenalty(1,12,white),
    sleep(1),
    update_emit_timers(1),
    start_game(1),
    update_emit_timers(1),
    sleep(1),
    update_emit_timers(1),
    sleep(1),
    update_emit_timers(1),
    stop_game(1),
    sleep(1),     
    update_emit_timers(1),
    ban_player_in_game(1,22,blue),
    update_emit_timers(1),
    change_status_to_reserve_player_in_game(1,5,blue),
    enter_player_in_game(1,7,blue),
    update_emit_timers(1),
    start_game(1),
    update_emit_timers(1),
    sleep(1),
    update_emit_timers(1),
    sleep(1),
    update_emit_timers(1),
    stop_game(1),
    update_emit_timers(1),
    eject_player_in_game(1,1,white),
    activate_penalty(1),
    update_emit_timers(1),
    start_penalty(1),
    sleep(1),
    update_emit_timers(1),
    sleep(15),
    update_emit_timers(1),
    sleep(1),
    update_emit_timers(1),
    sleep(1),
    update_emit_timers(1),
    start_game(1),
    update_emit_timers(1),
    sleep(1),
    update_emit_timers(1),
    sleep(10),
    update_emit_timers(1),
    stop_game(1),
    change_player_number_in_game(1,9,88,blue),       
    update_emit_timers(1),
    sleep(1),
    enter_player_in_game(1,0,white),
    start_game(1),
    change_player_number_in_game(1,0,11,white), % 11 should not work because exists, 44 yes
    update_emit_timers(1),
    sleep(10),
    update_emit_timers(1),
    sleep(10),
    update_emit_timers(1),
    sleep(10),
    stop_game(1),
    eject_player_in_game(1,33,blue),
    update_emit_timers(1),
    start_game(1),
    sleep(10),
    update_emit_timers(1),
    sleep(1),
    update_emit_timers(1),
    sleep(1),
    update_emit_timers(1),
    %
    %
    start_break(1),                     % break between 2 periods
    update_emit_timers(1),
    sleep(15),
    update_emit_timers(1),
    sleep(15),
    update_emit_timers(1),
    %
    update_emit_timers(1),
    start_game(1),
    update_emit_timers(1),
    sleep(5),
    update_emit_timers(1),
    sleep(5),
    update_emit_timers(1),
    stop_game(1),
    update_emit_timers(1),
    sleep(5),
    update_emit_timers(1),
    enter_player_in_game(1,0,white),
    change_player_number_in_game(1,0,11,white), % 11 should not work because exists, 44 yes
    update_emit_timers(1),
    start_game(1),
    update_emit_timers(1),
    sleep(5),
    update_emit_timers(1),
    sleep(5),
    update_emit_timers(1),
    sleep(5),
    update_emit_timers(1),
    sleep(5),
    update_emit_timers(1),
    sleep(5),
    update_emit_timers(1),
    sleep(5),
    update_emit_timers(1),
    sleep(5),
    enter_player_in_game(1,0,white),
    update_emit_timers(1),
    change_player_number_in_game(1,0,11,white), % 11 should not work because exists, 44 yes
    sleep(5),
    update_emit_timers(1),
    sleep(5),
    warn_player_in_game(1,9,blue),
    update_emit_timers(1),
    sleep(5),
    update_emit_timers(1),
    sleep(5),
    update_emit_timers(1),
    %
    %
    start_break(1),                    % pause between 2 matches
    update_emit_timers(1),
    sleep(15),
    update_emit_timers(1),
    sleep(15),
    update_emit_timers(1),
    %
    activate_game(2,stuttgart,3,berlin,2),
    update_emit_timers(2),
    sleep(15),
    start_game(2),
    update_emit_timers(2).
    % eject captain 
    %leave.
