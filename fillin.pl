% Declarative Programming, Semester 2, 2016, Project2
% Author Name : Min Gao
% Student ID : 773090
% UserName : ming1
% Purpose : solve any fillin crossword puzzles
% Version : 4
% Date : 11, Oct, 2016

%____________________________main idea______________________________%
% Main idea : There are two method using in this project.
% method 1 is to quickly solve the simple puzzle without
% finding the slot with the fewest matching words each time,
% need to process the wordlist at first. (only fast for simple puzzle)
% method 2 is to quickly solve the complicated puzzle
% using finding the slot with the fewest matching words each time.
% (fast for all puzzles)

%___________________________library load____________________________%
% To use the correct transpose/2 predicates for this projet.
% deﬁnes the predicate transpose(Matrix0, Matrix)
% that holds when Matrix0 and Matrix are lists of lists.
:- ensure_loaded(library(clpfd)).

%___________________________main function___________________________%
% using to solve a valid puzzle and print the solution.
% PuzzleFile, WordlistFile and SolutionFile all are file names.
main(PuzzleFile, WordlistFile, SolutionFile) :-
	read_file(PuzzleFile, Puzzle),
	read_file(WordlistFile, Wordlist),
	valid_puzzle(Puzzle),
	solve_puzzle(Puzzle, Wordlist, Solved),
	print_puzzle(SolutionFile, Solved).

%____________________________file in/out____________________________%
% read a file and save each line into a list.
% Content is a list of lists of characters.
read_file(Filename, Content) :-
	open(Filename, read, Stream),
	read_lines(Stream, Content),
	close(Stream).

% read all lines from a stream and save them into a list.
read_lines(Stream, Content) :-
	read_line(Stream, Line, Last),
	(   Last = true
	->  (   Line = []
	    ->  Content = []
	    ;   Content = [Line]
	    )
	;  Content = [Line|Content1],
	    read_lines(Stream, Content1)
	).

% read a line from a stream and save each character into a list.
% Line is a list of characters.
% Last is boolean that whether the stream is end.
read_line(Stream, Line, Last) :-
	get_char(Stream, Char),
	(   Char = end_of_file
	->  Line = [],
	    Last = true
	; Char = '\n'
	->  Line = [],
	    Last = false
	;   Line = [Char|Line1],
	    read_line(Stream, Line1, Last)
	).

% print the solved puzzle to the solution file.
print_puzzle(SolutionFile, Puzzle) :-
	open(SolutionFile, write, Stream),
	maplist(print_row(Stream), Puzzle),
	close(Stream).

% put one row of puzzle to a stream.
print_row(Stream, Row) :-
	maplist(put_puzzle_char(Stream), Row),
	nl(Stream).

% put a character into a stream.
put_puzzle_char(Stream, Char) :-
	(   var(Char)
	->  put_char(Stream, '_')
	;   put_char(Stream, Char)
	).

%__________________________puzzle solver___________________________%
% solve any valid puzzle with its word list and generate a solved puzzle.
% Puzzle and SolvedPuzzle should be list of lists of characters
% (single-character atoms), one list per puzzle row.
% WordList is a list of lists of characters, one list per word.
% MinNum is the minimum of the same length words in the wordList.
% MaxmNum is the maximum of the same length words in the wordlist.
% MinNum and MaxNum are used to measure the difficulty of the puzzle.
solve_puzzle(Puzzle, WordList, SolvedPuzzle) :-
	process_wordlist(WordList, MinNum, MaxNum, OptimizedWordList),
	fill_variable_puzzle(Puzzle, PuzzleWithVariable),
	find_slots_all_h_v(PuzzleWithVariable, AllVHSlots),
	(		(	MaxNum < 16,
				MinNum < 5
			)
	->	unify_all_words(OptimizedWordList, AllVHSlots)
	;		unify_all_slots(AllVHSlots, OptimizedWordList)
	),
	SolvedPuzzle = PuzzleWithVariable.

%________________________wordlist processor_________________________%
% process the wordlist to get optimized wordlist.
% first sort the words by word size from large to small.
% then sort the words by the number of same length from small to large.
process_wordlist(WordList, MinNum, MaxNum, OptimizedWordList) :-
	list_with_key_length(WordList, WordListWithKeyLength),
	sort(1, @>=, WordListWithKeyLength, SortedWordListWithLength),
	wordlists_split_bylength(SortedWordListWithLength, SplitWordLists),
	list_with_key_length(SplitWordLists, SplitWordListsWithKey),
	sort(1, @=<, SplitWordListsWithKey, SortedSpiltWordListsWithKey),
	num_of_same_len_words(SortedSpiltWordListsWithKey, MinNum, MaxNum),
	wordlist_without_length(SortedSpiltWordListsWithKey, SortedSpiltWordLists),
	strip_nest(SortedSpiltWordLists, OptimizedWordList).

% count the length of every list in the lists.
% put the length as key with the list together, e.g. [3,['c','a','t']].
list_with_key_length([],[]).
list_with_key_length([List|Lists], [ListWithLen|ListsWithLen]) :-
	length(List, ListLen),
	ListWithLen = [ListLen|[List]],
	list_with_key_length(Lists, ListsWithLen).

% get the wordlist without the lenght of each word,
wordlist_without_length([], []).
wordlist_without_length([WordWithLength|WordsWithLength], [Word|Words]) :-
	WordWithLength = [_,Word],
	wordlist_without_length(WordsWithLength, Words).

% back the word to the outer list.
% generate a list of lists(words).
strip_nest(List, NewList) :-
	strip(List, [], NewList).
strip([], Pre, Pre).
strip([L|Ls], Pre, NewList) :-
	append(Pre, L, TempPre),
	strip(Ls, TempPre, NewList).

% put the same length words into one list without the length of each word.
wordlists_split_bylength(SortedWordListWithLen, SplitWordLists) :-
	SortedWordListWithLen = [WordWithLen|_],
	WordWithLen = [WordLen|_],
	wordlists_split_byl(SortedWordListWithLen, WordLen, [], [], SplitWordLists).
wordlists_split_byl([], _, [], Pre ,Pre).
wordlists_split_byl([], _, SameLenList, Pre, SplitWordLists) :-
	not(length(SameLenList,0)),
	append(Pre, [SameLenList], TempSplitWordLists),
	wordlists_split_byl([], _, [], TempSplitWordLists, SplitWordLists).
wordlists_split_byl([WL|WLs], PreWLen, SameLenList, Pre, SplitWordLists) :-
	WL = [WordLen, Word],
	(		WordLen =:= PreWLen
	->	append(SameLenList, [Word], TempSameLenList),
			wordlists_split_byl(WLs, WordLen, TempSameLenList, Pre, SplitWordLists)
	;		append(Pre, [SameLenList], TempPre),
			wordlists_split_byl(WLs, WordLen, [Word], TempPre, SplitWordLists)
	).

%_______________________words/slots unifying________________________%
% method 1 (to quickly solve simple puzzles without finding
% the slot with the fewest matching words each time, need to
% process the wordlist at first).
% unifying all words to all slots.
unify_all_words([],_).
unify_all_words([Word|Words], AllVHSlots) :-
	unify_word(Word, AllVHSlots, NewAllVHSlots),
	unify_all_words(Words, NewAllVHSlots).

% unifying a word to a slot.
% also can use to unifying a slot to a word.
unify_word(Word, [Slot|Slots], NewSlots) :-
	unify_w(Word, [Slot|Slots], [], NewSlots).
unify_w(Word, [Slot|Slots], TriedSlots, NewSlots) :-
	(		Slot = Word,
			append(Slots, TriedSlots, NewSlots)
	;		append(TriedSlots, [Slot], TempNewSlots),
			unify_w(Word, Slots, TempNewSlots, NewSlots)
	).

% method 2 (to quickly solve complicated puzzles)
% unifying all slots to all words
unify_all_slots([], _).
unify_all_slots([Slot|Slots], WordList) :-
	count_num_fit_slot(Slot, WordList, 0, MatchNum),
	find_best_slot(Slots, WordList, MatchNum, Slot, BestSlot, [], NewSlots),
	unify_word(BestSlot, WordList, NewWordList),
	unify_all_slots(NewSlots, NewWordList).

% find the slot with the fewest matching words.
find_best_slot([], _, FewestMatch, Slot, Slot, PreSlots, PreSlots) :-
	FewestMatch > 0.
find_best_slot([Slot|Slots], WordList, FewestMatch, NewSlot,
	BestSlot, PreSlots, NewSlots) :-
	count_num_fit_slot(Slot, WordList, 0, MatchNum),
	(		MatchNum < FewestMatch
	->	append(PreSlots, [NewSlot], TempNewSlots),
			find_best_slot(Slots, WordList, MatchNum, Slot, BestSlot,
				TempNewSlots, NewSlots)
	;		append(PreSlots, [Slot], TempNewSlots),
			find_best_slot(Slots, WordList, FewestMatch, NewSlot,
				BestSlot, TempNewSlots, NewSlots)
	).

% count the number of words that can match the slot
count_num_fit_slot(_, [], Pre, Pre).
count_num_fit_slot(Slot, [Word|Words], PreMatch, MatchNum) :-
	(		can_unified(Slot, Word)
	->	count_num_fit_slot(Slot, Words, PreMatch + 1, MatchNum)
	;		count_num_fit_slot(Slot, Words, PreMatch, MatchNum)
	).

%_________________________puzzle processor__________________________%
% fill the '_' with the logical variable for whole puzzle.
fill_variable_puzzle(Puzzle, PuzzleWithVariable) :-
	fill_variable_p(Puzzle, [], PuzzleWithVariable).
fill_variable_p([], Pre, Pre).
fill_variable_p([Row|Rows], Pre, NewPuzzle) :-
	fill_variable_row(Row,RWithV),
	append(Pre, [RWithV], Next),
	fill_variable_p(Rows, Next, NewPuzzle).

% fill the '_' with the logical variable for a row.
fill_variable_row(Row,RowWithVariable) :-
	fill_variable_r(Row, [], RowWithVariable).
fill_variable_r([], PreRow, PreRow).
fill_variable_r([C|Cs], PreRow, NewRow) :-
	(		C == '_'
	->	length(V, 1),
			append(PreRow, V, Next),
			fill_variable_r(Cs, Next, NewRow)
	;		append(PreRow, [C], Next),
			fill_variable_r(Cs, Next, NewRow)
	).

% construct all horizontal and vertical slots for a puzzle.
find_slots_all_h_v(PuzzleWithVariable, AllVHSlots) :-
	find_slots_puzzle(PuzzleWithVariable, AllSlots_h),
	transpose(PuzzleWithVariable,PuzzleWithVariable_v),
	find_slots_puzzle(PuzzleWithVariable_v, AllSlots_v),
	append(AllSlots_h, AllSlots_v, AllVHSlots).

% construct a list of slots for a puzzle.
find_slots_puzzle(PuzzleWithVariable, AllSlots) :-
	find_slots_p(PuzzleWithVariable, [], AllSlots).
find_slots_p([], Pre, Pre).
find_slots_p([Row|Rows], Pre, Slots) :-
	find_slots_row(Row, Slot),
	append(Pre, Slot, TempSlots),
	find_slots_p(Rows, TempSlots, Slots).

% construct a list of slots for a row.
find_slots_row(Row, Slots) :-
	find_slots_r(Row, [], [], Slots).
find_slots_r([], Slot, Pre, Slots) :-
	length(Slot,Len),
	(		Len < 2
	->	Slots = Pre
	;		append(Pre, [Slot], Slots)
	).
find_slots_r([C|Cs], Slot, Pre, Slots) :-
	(		C == '#'
	->	length(Slot, Len),
			(		Len < 2
			->	find_slots_r(Cs, [], Pre, Slots)
			;		append(Pre, [Slot], TempSlots),
					find_slots_r(Cs, [], TempSlots, Slots)
			)
	;		append(Slot, [C], ContinueSlot),
			find_slots_r(Cs, ContinueSlot, Pre, Slots)
	).

%___________________________help function___________________________%
% is true when each row has the same length.
% check a puzzle using valid_puzzle(Puzzle).
valid_puzzle([]).
valid_puzzle([Row|Rows]) :-
	maplist(samelength(Row), Rows).

% get the size(width and height) of puzzle.
get_puzzle_size(Puzzle, Width, Height) :-
	Puzzle = [Row|_],
	length(Row, Width),
	length(Puzzle, Height).

% check whether there are a lot of same length words in a puzzle.
% should input a sorted split wordlist with key of length.
% MinNum is the number of fewest same length words.
% MaxNum is the number of most same length words.
num_of_same_len_words(SortedSpiltWordListsWithKey, MinNum, MaxNum) :-
	SortedSpiltWordListsWithKey = [FewestSSameLenWordList|_],
	FewestSSameLenWordList = [MinNum|_],
	last(SortedSpiltWordListsWithKey, MostSameLenWordList),
	MostSameLenWordList = [MaxNum|_].

% whether two lists L1 and L2 have the same length.
samelength([], []).
samelength([_|L1], [_|L2]) :-
	same_length(L1, L2).

% check whether two lists can be unifyied.
can_unified([], []).
can_unified([C1|C1s], [C2|C2s]) :-
	(		(		C1 == C2
			;		var(C1)
			;		var(C2)
			)
	->	can_unified(C1s, C2s)
	).

%________________________________end________________________________%
