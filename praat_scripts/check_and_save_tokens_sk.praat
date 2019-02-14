# Check Token Script
#
# This script goes through all the files in the specified foler one by one
# and let the user modify the textgrid and decide whether the token is usable or not.
# If usable, the wav file and the modified textgrid file are saved to another specified folder. 
# It also tells you how many tokens you have saved and it also keeps track which word has been saved.
#
# Room for improvement:
# 	Window display (so that windows wouldn't overlap).
# 	Minimum vowel duration
#
# By SK KIM 2012/08

form Specify directories to work with
   comment Specify the folder of the files:
   text folder /home/cj/Desktop/Linguistics/QP2/potential_tokens/
   comment Specify the folder where usable tokens to be saved:
   text saving_to /home/cj/Desktop/Linguistics/QP2/usable_tokens/
   comment Specify the word tier number:
   integer word_tier 2
endform

sound_file_extension$ = ".wav"
textGrid_file_extension$ = ".TextGrid"

Create Strings as file list... wavlist 'folder$'*'sound_file_extension$'
Create Strings as file list... gridlist 'folder$'*'textGrid_file_extension$'
numberOfFiles = Get number of strings

pause There are 'numberOfFiles' potential tokens in the folder. 
echo Words you have accepted:

counter = 0
for ifile to numberOfFiles

	# reading in a pair of wav file and textgrid file
	
	select Strings wavlist
	wavname$ = Get string... ifile
	Read from file... 'folder$''wavname$'
	soundname$ = selected$("Sound", 1)

	select Strings gridlist
	gridname$ = Get string... ifile
	Read from file... 'folder$''gridname$'
	textgridname$ = selected$("TextGrid", 1)
	
	# need to check whether soundname and textgridname match (add an if statement)
	
	select Sound 'soundname$'
	plus TextGrid 'textgridname$'
	
	View & Edit
	
	beginPause ("Inpect the token and adjust the boundaries as necessary")
		comment ("Usable token?")
		choice ("usable", 2)
			option("Yes")
			option("No")
	#clicked = endPause ("Stop", "Continue", 2)
	endPause("Continue", 1)
	
	
	if 'usable' == 1
		new_textgrid_filename$ = "'saving_to$'" + "'textgridname$'" + ".TextGrid"
		sound_filename$ = "'saving_to$'" + "'soundname$'" + ".wav"
		
		select Sound 'soundname$'
		Write to WAV file... 'sound_filename$'
		
		select TextGrid 'textgridname$'
		word$ = Get label of interval... word_tier 2
		Write to text file... 'new_textgrid_filename$'
		
		counter = counter + 1
		pause You have accepted 'counter' tokens so far.
		printline 'word$'
		

	endif
	
	select all
	minus Strings wavlist
	minus Strings gridlist
	Remove
	select Strings wavlist
	plus Strings gridlist
endfor
select all
Remove
