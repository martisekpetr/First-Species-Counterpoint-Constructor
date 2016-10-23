/* -*- Mode:Prolog; coding:iso-8859-1; -*- */
:-use_module(library(lists)).
:-use_module(library(clpfd)).


counterpoint(Melody, Counterpoint) :-
        % init second voice of the same length as the first 
        length(Melody, N),
        length(Counterpoint, N),
        % determine key of the song from the first voice
        get_key(Melody,Key),
        % restrict domain to valid MIDI notes (A0 - C8)
        domain(Counterpoint, 21, 108),
        % set domains for notes in second voice with respect to the key and first voice
        harmonic_domain(Melody, Counterpoint, Key),
        
        
        % constraints
        singable_range(Counterpoint),
        no_big_leap(Counterpoint),
        no_chromatic_move(Counterpoint),
        no_repeated_notes(Counterpoint),
        compensate_leap(Counterpoint),
        compensate_octave_leap(Counterpoint),
        arpeggiating_triads(Counterpoint),
        cadence(Counterpoint),
        no_crossing(Melody, Counterpoint),
        no_cross_tritone(Melody,Counterpoint),
        not_too_many_thirds(Melody, Counterpoint),
        not_too_many_sixths(Melody, Counterpoint),
        not_too_many_tenths(Melody, Counterpoint),
        enter_octave(Melody, Counterpoint),
        enter_fifth(Melody, Counterpoint),
        
        % optimizations
        count_imperfect(Melody,Counterpoint, Imperfects),
        evaluate_motions(Melody, Counterpoint, Motions),
        steps_or_leaps(Counterpoint, Steps),
        Objective #= Imperfects + Motions + Steps,
        !,
        
        labeling([maximize(Objective)], Counterpoint),
        
        % write result to file
        open('out.txt',write, Stream),
        write(Stream, Melody),
        nl(Stream),
        write(Stream, Counterpoint),
        close(Stream).
  

%%% DETERMINE THE KEY OF THE MELODY

% determine the Key of the song based on maximal number of matches   
get_key([], _).
get_key(Melody, Key):-
  findall(pair(K,N), try_key(Melody, K, N), List),
  best_match(List, pair(Key,_)).

% assign valid Key and compute number of matches
try_key([], _, 0).
try_key(Melody, Key, N):-
        is_key(Key),
        count_matches(Melody, Key, N).

% all 12 possible keys (A,A#,B,C,C#,D,D#,E,F,F#,G,G#)  
is_key(Key):-
        member(Key, [21,22,23,24,25,26,27,28,29,30,31,32]).

% count how many notes from given list match the major scale derived from Key
count_matches([], _, 0).
count_matches([X|Xs], Key, N):-
  R is mod(X-Key, 12), % convert note to relative interval above the Key
  Scale = [0,2,4,5,7,9,11], % major scale
  (member(R, Scale) ->
     count_matches(Xs, Key, N1),
     N is N1+1
     ;
     count_matches(Xs, Key, N)
   ).

% return key with biggest number of matches
best_match([],_).
best_match([pair(K,N)|Xs], BestPair):-
        best_match(Xs, pair(K, N), BestPair). % use first pair as initial max value

best_match([], Pair, Pair).
best_match([pair(K,N)|Xs], pair(K0, N0), BestPair):-
        (N > N0 ->
         best_match(Xs, pair(K, N), BestPair)
        ;
         best_match(Xs, pair(K0, N0), BestPair)
        ).

    

%%%  RESTRICT DOMAIN OF SECOND VOICE TO ALLOWED INTERVALS
% set domain of each note of the second voice with respect to first voice and the Key of the piece
harmonic_domain([],[],_).
harmonic_domain([X|Xs],[Y|Ys], Key):- 
        % PERFECT BEGIN: first note must be either octave below, octave above or fifth above the first voice
        addIntervals(X,[-12,0,7,12], L, Key),   
        list_to_fdset(L, Set),
        Y in_set Set,
        
        allowed_intervals(Xs,Ys,Key). % set domains for the rest of the notes

% convert given intervals (relative to first voice) to specific notes and if they are in key, add them to the list
addIntervals(_, [], [], _).
addIntervals(X, [I|Is], [Y|Ys], Key):-
        Y is X + I,
        in_key(Y, Key),!,
        addIntervals(X, Is, Ys, Key).
addIntervals(X, [_|Is], Ys, Key):-
        addIntervals(X, Is, Ys, Key).

% check if X is in major scale derived from Key
in_key(X, Key):-
        count_matches([X], Key, 1).

% define domains for the notes in the constructed voice with respect to first voice (only several allowed intervals) and according to Key 
allowed_intervals([], [],_).
% PERFECT ENDING: last note must be an octave or unison
allowed_intervals([X], [Y], Key):-
        addIntervals(X,[-12,0,12],L,Key),
        list_to_fdset(L, Set),
        Y in_set Set.
allowed_intervals([X|Xs], [Y|Ys], Key):-
        % allowed intervals are: minor third(3), major third(4), perfect fifth(7), minor sixth(8), major sixth(9), octave(12), minor tenth(=octave + minor third)(15), major tenth(octave + major third)(16)
        Intervals = [-16,-15,-12,-9,-8,-7,-4,-3,3,4,7,8,9,12,15,16],
        addIntervals(X, Intervals, L, Key),!,
        list_to_fdset(L, Set),
        Y in_set Set,
        allowed_intervals(Xs, Ys, Key).

  



%%% CONSTRAINTS (HARMONIC RULES)

%% HORIZONTAL RULES (independent on first voice)

% HORIZONTAL RULE: Range of the voice should not exceed a major tenth (= 16 semitones) from its highest to its lowest pitch. 
singable_range([]).
singable_range(Voice):-
        maximum(Max, Voice),
        minimum(Min, Voice),
        Max - Min #=< 16.

% HORIZONTAL RULE: Interval between adjacent notes should not exceed a fifth, except for the octave and the ascending minor sixth. 
no_big_leap([]).
no_big_leap([_]).
no_big_leap([X,Y|Xs]):-
        % atmost octave
        abs(X-Y) #=< 12, 
        
        X-Y #\= 8,      
        abs(X-Y) #\= 9, 
        abs(X-Y) #\= 10,
        abs(X-Y) #\= 11,
        
        no_big_leap([Y|Xs]).

% HORIZONTAL RULE: No voice should move by a chromatic interval (any augmented or diminished interval).
no_chromatic_move([]).
no_chromatic_move([_]).
no_chromatic_move([X,Y|Xs]):-
        abs(X-Y) #\= 6, % tritone (diminished fifth)
        
        no_chromatic_move([Y|Xs]).
        
% HORIZONTAL RULE: Do not repeat the same note more than once. 
no_repeated_notes([]).
no_repeated_notes([_]).
no_repeated_notes([_,_]).
no_repeated_notes([X,Y,Z|Xs]):-
  (X #\= Y #\/ X #\= Z),
  no_repeated_notes([Y,Z|Xs]).

% HORIZONTAL RULE: Leaps greater than a fifth should be compensated by movement in the opposite direction. 
% If the leap is ascending make sure the compensation is stepwise.
compensate_leap([]).
compensate_leap([_]).
compensate_leap([_,_]).
compensate_leap([X,Y,Z|Xs]):-
        (abs(X-Y) #< 7  % no big leap 
        #\/ 
        (Y-X #>= 7 #/\ Z #< Y #/\ abs(Y-Z) #=< 2) % ascending leap (compensate by step down)
        #\/
        (X-Y #>= 7 #/\ Z #> Y) % descending leap (compensate by step/leap up)
        ), 
        compensate_leap([Y,Z|Xs]).

%HORIZONTAL RULE: A leap of an octave should be balanced: preceded and followed by notes within the octave.
compensate_octave_leap([]).
compensate_octave_leap([_]).
compensate_octave_leap([_,_]).
compensate_octave_leap([_,_,_]).
compensate_octave_leap([X,Y,Z,W|Xs]):-
        (abs(Y-Z) #\= 12        % no octave leap 
        #\/
           (
           abs(Y-Z) #= 12 #/\   % octave leap
           abs(X-Y) #< 12 #/\   % preceding note within octave
           abs(X-Z) #< 12 #/\
           abs(W-Y) #< 12 #/\   % following note within octave
           abs(W-Z) #< 12
           )
        ),
         
        compensate_octave_leap([Y,Z,W|Xs]).
        
% HORIZONTAL RULE: Two leaps (intervals larger than 2 semitones) in the same direction are allowed 
% only if they spell out a triad shape (i.e. both leaps are thirds (= 3 or 4 semitones)). 
arpeggiating_triads([]).
arpeggiating_triads([_]).
arpeggiating_triads([_,_]).
arpeggiating_triads([X,Y,Z|Xs]):-
        (
           (Y-X)*(Z-Y) #=< 0     % different direction
        #\/
           abs(X-Y) #=< 2        % no leap
        #\/
           abs(Y-Z) #=< 2        % no leap
        #\/
           abs(X-Y) #=< 4 #/\    % two leaps in same direction -> must be triad
           abs(Y-Z) #=< 4
        ),
        
        arpeggiating_triads([Y,Z|Xs]).

% HORIZONTAL RULE: Note leading to the last note must be one step (=< 2) or fifth(7) away.    
cadence([]).
cadence([_]).
cadence([X,Y]):-
        (abs(X-Y) #= 1) #\/ (abs(X-Y) #= 2) #\/ (abs(X-Y) #= 7).       
cadence([_,Y,Z|Xs]):-
        cadence([Y,Z|Xs]).       



% VERTICAL RULES

% VERTICAL RULE: Voices should not cross (if the second voice is below the first one, it must not move above and vice versa).
no_crossing([],[]).
no_crossing([_],[_]).  
no_crossing([X1,X2|Xs],[Y1,Y2|Ys]):-
        X1 #< Y1 #=> X2 #=< Y2,
        X1 #> Y1 #=> X2 #>= Y2,  
        no_crossing([X2|Xs],[Y2|Ys]).

% VERTICAL RULE: Avoid adjacent use in different voices of two pitches that form the tritone (diminished fifth).  
no_cross_tritone([],[]).
no_cross_tritone([_],[_]).  
no_cross_tritone([X1,X2|Xs],[Y1,Y2|Ys]):-  
  abs(X1 - Y2) #\= 6,
  abs(Y1 - X2) #\= 6,
  no_cross_tritone([X2|Xs],[Y2|Ys]).

% VERTICAL RULE: Avoid writing more than three consecutive thirds in a row. 
not_too_many_thirds([], []).
not_too_many_thirds([_], [_]).
not_too_many_thirds([_,_], [_,_]).
not_too_many_thirds([_,_,_], [_,_,_]).
not_too_many_thirds([X1,X2,X3,X4|Xs], [Y1,Y2,Y3,Y4|Ys]):-
        (
           abs(X1-Y1) #\= 3 #/\ abs(X1-Y1) #\= 4
        #\/
           abs(X2-Y2) #\= 3 #/\ abs(X2-Y2) #\= 4
        #\/
           abs(X3-Y3) #\= 3 #/\ abs(X3-Y3) #\= 4
        #\/
           abs(X4-Y4) #\= 3 #/\ abs(X4-Y4) #\= 4
        ),
        
        not_too_many_thirds([X2,X3|Xs], [Y2,Y3|Ys]).

% VERTICAL RULE: Avoid writing more than three consecutive sixths in a row.                               
not_too_many_sixths([], []).
not_too_many_sixths([_], [_]).
not_too_many_sixths([_,_], [_,_]).
not_too_many_sixths([_,_,_], [_,_,_]).
not_too_many_sixths([X1,X2,X3,X4|Xs], [Y1,Y2,Y3,Y4|Ys]):-
        (
           abs(X1-Y1) #\= 8 #/\ abs(X1-Y1) #\= 9
        #\/
           abs(X2-Y2) #\= 8 #/\ abs(X2-Y2) #\= 9
        #\/
           abs(X3-Y3) #\= 8 #/\ abs(X3-Y3) #\= 9
        #\/
           abs(X4-Y4) #\= 8 #/\ abs(X4-Y4) #\= 9
        ),
        
        not_too_many_sixths([X2,X3|Xs], [Y2,Y3|Ys]).

% VERTICAL RULE: Avoid writing more than three consecutive tenths in a row.  
not_too_many_tenths([], []).
not_too_many_tenths([_], [_]).
not_too_many_tenths([_,_], [_,_]).
not_too_many_tenths([_,_,_], [_,_,_]).
not_too_many_tenths([X1,X2,X3,X4|Xs], [Y1,Y2,Y3,Y4|Ys]):-
        (
           abs(X1-Y1) #\= 15 #/\ abs(X1-Y1) #\= 16
        #\/
           abs(X2-Y2) #\= 15 #/\ abs(X2-Y2) #\= 16
        #\/
           abs(X3-Y3) #\= 15 #/\ abs(X3-Y3) #\= 16
        #\/
           abs(X4-Y4) #\= 15 #/\ abs(X4-Y4) #\= 16
        ),
        
        not_too_many_tenths([X2,X3|Xs], [Y2,Y3|Ys]).

% VERTICAL RULE: Only contrary or oblique motion are acceptable ways to enter into a fifth or octave. 
% Only exception is when landing on a final note. Do not move to a fifth or octave with leaping 
% motion (interval greater than major second) in either voice unless the movement is oblique. 
enter_octave([],[]).
enter_octave([_],[_]).
enter_octave([_,_],[_,_]). % suspend the rule for the last two notes (cadence)
enter_octave([X1,X2|Xs],[Y1,Y2|Ys]):-
        (
           abs(X2-Y2) #\= 12            % not entering octave
        #\/
           (X1-X2)*(Y1-Y2) #< 0 #/\     % contrary motion
           abs(X1-X2) #=< 2 #/\         % stepwise motion
           abs(Y1-Y2) #=< 2
        #\/
           (X1-X2)*(Y1-Y2) #= 0 #/\     % oblique (one voice stays on the same note) motion
           abs(X1-Y1) #\= 12            % no parallel octaves
        ),
        enter_octave([X2|Xs], [Y2|Ys]).

enter_fifth([],[]).
enter_fifth([_],[_]).
enter_fifth([_,_],[_,_]).
enter_fifth([X1,X2|Xs],[Y1,Y2|Ys]):-
        (
           abs(X2-Y2) #\= 7            % not entering fifth
       #\/
           (X1-X2)*(Y1-Y2) #< 0 #/\     % contrary motion
           abs(X1-X2) #=< 2 #/\         % stepwise motion
           abs(Y1-Y2) #=< 2
        #\/
           (X1-X2)*(Y1-Y2) #= 0 #/\     % oblique (one voice stays on the same note) motion
           abs(X1-Y1) #\= 7            % no parallel octaves
        ),
        enter_fifth([X2|Xs], [Y2|Ys]).



%%% OPTIMIZATIONS
% Count how many imperfect intervals (thirds, sixths, tenths) are between melody and counterpoint.
count_imperfect([],[],0).
count_imperfect([X|Xs],[Y|Ys],N):-
        % its easier to single out the perfect intervals
        (abs(X-Y) #= 0
           #\/
         abs(X-Y) #= 7
           #\/
         abs(X-Y) #= 12
        ) #<=> B,
        N #= N1 + 1 - B, % add negation of B
        count_imperfect(Xs,Ys, N1).

% Evaluate used motions (contrary > similar > oblique > parallel).
evaluate_motions([],[],0).
evaluate_motions([_],[_],0).
evaluate_motions([X1,X2|Xs],[Y1,Y2|Ys],N):-
        (X1-X2)*(Y1-Y2) #< 0 #<=> Contrary,
        (X1-X2)*(Y1-Y2) #= 0 #/\ abs(X1-Y1) #\= abs(X2-Y2) #<=> Oblique,
        (X1-X2)*(Y1-Y2) #>= 0 #/\ abs(X1-Y1) #= abs(X2-Y2) #<=> Parallel,
        (X1-X2)*(Y1-Y2) #> 0 #/\ abs(X1-Y1) #\= abs(X2-Y2) #<=> Similar,
        N #= N1 + 3*Contrary + Similar - Oblique - 2*Parallel,
        evaluate_motions([X2|Xs], [Y2|Ys], N1).

% Count how many times the counterpoint moves stepwise.        
steps_or_leaps([],0).
steps_or_leaps([_],0).
steps_or_leaps([X1,X2|Xs],N):-
        abs(X1-X2) #=< 2 #<=> Step,
        N #= N1 + Step,
        steps_or_leaps([X2|Xs], N1).
         