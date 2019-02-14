#####
# This script loops thorugh all intervals of sound files in a folder, 
#     finds vowels of specified quality for each word and measures
#     the middle 50 points of the vowel (roughly every 2 percent from 
#     the 2nd to 98th percentile of the vowel).
# 
# FILE EXTENSIONS should be .wav and .TextGrid 
# EACH .wav and .TextGrid should contain one lemma, output from
#     extract_tokens_sk.praat will work, see that script for examples.
#
# 
# By Seung Kyung KIM 2012/12
# Revision by Christian BRICKHOUSE 2019/01


form Measure F1-F5 of stressed vowels

	comment Directory of sound files
	text sound_directory /home/cj/Desktop/Linguistics/QP2/usable_tokens/bot/
	comment Directory of TextGrid files
	text textGrid_directory /home/cj/Desktop/Linguistics/QP2/usable_tokens/bot/
   
	comment Analyze which tier in the TextGrid:
	integer sound_tier 1
	integer word_tier 2
   
	comment Which segment?
	sentence segment AA1

	comment Which vowel class?
	sentence vowel_class BOT

	comment Select sex of speaker:
	choice sex 1
		button male
		button female
	comment Maximum number of formants to be tracked:	
	integer nFormants 5
	comment Specify output file name: 
	text output_file /home/cj/Desktop/Linguistics/QP2/data/data_table.csv
	#text norm_input_file /home/cj/Desktop/sociophonetics/data_table.txt

endform

sound_file_extension$ = ".wav"
textGrid_file_extension$ = ".TextGrid"

Create Strings as file list... wavlist 'sound_directory$'*'sound_file_extension$'
Create Strings as file list... gridlist 'textGrid_directory$'*'textGrid_file_extension$'
numberOfFiles = Get number of strings


outfilename$ = "'output_file$'"
filedelete 'outfilename$'

header_row$ = "site,speaker,orientation,sex,age,education,interviewer,segment,vowel_class,token,duration,word,preceding,following,index,f1_hz,f2_hz,f3_hz,f1_bark,f2_bark,f3_bark,f1_normed,f2_normed" + newline$
header_row$ > 'outfilename$'

#norminputfile$ = "'norm_input_file$'"
#filedelete 'norminputfile$'
#header_norm$ = "speaker"+tab$+"vowel"+tab$+"context"+tab$+"f1"+tab$+"f2"+tab$+"f3"+tab$+"f1off"+tab$+"f2off"+tab$+"f3off" + newline$
#header_norm$ > 'norminputfile$'



# all the stressed vowel sounds 
	#vowel$[1] = "AA1"
	#vowel$[2] = "AE1"
	#vowel$[3] = "AH1"
	#vowel$[4] = "AO1"
	#vowel$[5] = "AW1"
	#vowel$[6] = "AY1"
	#vowel$[7] = "EH1"
	#vowel$[8] = "ER1"
	#vowel$[9] = "EY1"
	#vowel$[10] = "IH1"
	#vowel$[11] = "IY1"
	#vowel$[12] = "OW1"
	#vowel$[13] = "OY1"
	#vowel$[14] = "UH1"
	#vowel$[15] = "UW1"
	

# set maximum frequency of Formant calculation algorithm on basis of sex
# sex is 1 for male; sex is 2 for female

if 'sex' = 1
	maxf = 5000
	f1ref = 500
	f2ref = 1485
	f3ref = 2475
	f4ref = 3465
	f5ref = 4455
	freqcost = 1
	bwcost = 1
	transcost = 1
endif

if 'sex' = 2
	maxf = 5500
	f1ref = 550
	f2ref = 1650
	f3ref = 2750
	f4ref = 3850
	f5ref = 4950
	freqcost = 1
	bwcost = 1
	transcost = 1
endif



# string split function from http://www.ucl.ac.uk/~ucjt465/scripts/praat/split.proc.praat
procedure split .sep$ .str$
	.length = 0
	repeat
		.strlen = length(.str$)
		.sep = index(.str$, .sep$)
		if .sep > 0
			.part$ = left$(.str$, .sep-1)
			.str$ = mid$(.str$, .sep+1, .strlen)
		else
			.part$ = .str$
		endif
		.length = .length+1
		.array$[.length] = .part$
	until .sep = 0
endproc


#######################

for ifile to numberOfFiles

	# reading in a pair of wav file and textgrid file
	
	select Strings wavlist
	wavname$ = Get string... ifile
	Read from file... 'sound_directory$''wavname$'
	soundname$ = selected$("Sound", 1)
	
	echo 'soundname$'

	select Strings gridlist
	gridname$ = Get string... ifile
	Read from file... 'textGrid_directory$''gridname$'
	textgridname$ = selected$("TextGrid", 1)

	
	call split "_" 'soundname$'
    for i to split.length
        str$ [i] = split.array$ [i]
    endfor
	
	site$ = split.array$[1]
	speaker$ = split.array$[3]+"_"+split.array$[2]
	#before$ = split.array$[8]
	#after$ = split.array$[9]

	
	# creating Formant object and track formants	

		#select 'sound'
		select Sound 'soundname$'
		total_dur = Get total duration

		To Formant (burg)... 0.00 nFormants 'maxf' 0.025 50
		Rename... 'soundname$'_beforetracking
		formant_beforetracking = selected("Formant")

		# if you try to track more than m formants, you'd run into an error
		m = Get minimum number of formants
		Track... m 'f1ref' 'f2ref' 'f3ref' 'f4ref' 'f5ref' 'freqcost' 'bwcost' 'transcost'

		Rename... 'name$'_aftertracking
		formant_aftertracking = selected("Formant")

		
	# looping through all intervals

	#select 'grid'
	select TextGrid 'textgridname$'
	nIntervals = Get number of intervals... sound_tier

	for i from 1 to nIntervals

		phone$ = Get label of interval... sound_tier i
		
		# finding a stressed vowel
		
		stressVowel = 0
		#for j from 1 to 15
			#temp$ = vowel$[j]
			if "'segment$'" = "'phone$'" 
				stressVowel = stressVowel + 1
			endif
		#endfor
		
		start = Get starting point... sound_tier i
		end = Get end point... sound_tier i
			
		if stressVowel > 0 and i - 1 > 0 and total_dur > (end + 0.001)
			
			echo 'phone$'
			printline 'm'				
			
			duration = end - start
			duration_ms = duration * 1000
			
			printline 'duration_ms'

			before$ = Get label of interval... sound_tier i - 1

			printline 'before'

			after$ = Get label of interval... sound_tier i + 1
			
			printline 'after'

			w$ = Get label of interval... word_tier 2

			
			printline 'w$'
			
			printline 'foobar'

					
			
			### Get measurements at 25% 50% 75% 
			
			for k from 1 to 50
				
				t[k] = start + k*(duration/51)
				
				
				### Get the f1-f5 measurements 
				
				select 'formant_aftertracking'
				f1[k] = Get value at time... 1 t[k] Hertz Linear
				f2[k] = Get value at time... 2 t[k] Hertz Linear
				f3[k] = Get value at time... 3 t[k] Hertz Linear
				f4[k] = Get value at time... 4 t[k] Hertz Linear
				f5[k] = Get value at time... 5 t[k] Hertz Linear
				
				f1_n = f1[k]
				f2_n = f2[k]
				f3_n = f3[k]
				# Traunmuller (1990) Bark conversion equation; nice because it doesn't require arctan
				f1_bark = (26.81/(1+(1960/f1_n)))-0.53
				f2_bark = (26.81/(1+(1960/f2_n)))-0.53
				f3_bark = (26.81/(1+(1960/f3_n)))-0.53
				
				measures$ = "'site$','speaker$',,'sex',,,,'phone$','vowel_class$','soundname$','duration_ms','w$','before$','after$','k','f1_n','f2_n','f3_n','f1_bark','f2_bark','f3_bark',," + newline$
				fileappend 'outfilename$' 'measures$'
				
				
		
			endfor
			
						
			# Get the word label
			select TextGrid 'textgridname$'
			word_interval_number = Get interval at time... word_tier start + 0.001 
			word$ = Get label of interval... word_tier word_interval_number
			
			printline 'word$'
						
			#measures$ = "'soundname$','speaker$','word$','phone$','duration_ms','before$','after$','f1_25','f2_25','f3_25','f4_25','f5_25','f1_50','f2_50','f3_50','f4_50','f5_50','f1_75','f2_75','f3_75','f4_75','f5_50','newline$'"
			measures$ = "'site$','speaker$',,,,,,'phone$','vowel_class$','soundname$','duration_ms','word$','before$','after$','f1_50','f2_50','f3_50',,,,," + newline$
			header_row$ = "site,speaker,orientation,sex,age,education,interviewer,vowel,vowel_class,token,duration,word,preceding,following,f1_hz,f2_hz,f3_hz,f1_bark,f2_bark,f3_bark,f1_normed,f2_normed" + newline$
			#fileappend 'outfilename$' 'measures$'
			
			#norm_measures$ = "'speaker$''tab$''vowel_class$''tab$''word$''tab$''f1_50''tab$''f2_50''tab$''f3_50''newline$'"
			#fileappend 'norminputfile$' 'norm_measures$'

		endif
		endif
		
		
	endfor

	select all
	minus Strings wavlist
	minus Strings gridlist
	Remove
	select Strings wavlist
	plus Strings gridlist
	
endfor

printline 'DONE'

select all
Remove