# Extracting and Saving Token 
#
# This script extracts words that contains a specfied segment. 
# IMPORTANT: The long sound file (.wav) and the textgrids (.TextGrid) should have the SAME NAME.
#
# Words that are defined as function words are excluded. 
# Check the list of function words below in the script. 
# You can add (within the limit of total 80 function words) your own function words to the list.
#
# By SK KIM 2012/08
# Modified Christian Brickhouse 2019/02

sound_file_extension$ = ".wav"
textGrid_file_extension$ = ".TextGrid"


# function words
	function$[1] = "TO"
	function$[2] = "DO"
	function$[3] = "DOING"
	function$[4] = "OKAY"
	function$[5] = "GOING"
	function$[6] = "DON'T"
	function$[7] = "NO"
	function$[8] = "OVER"
	function$[9] = "AT"
	function$[10] = "THAT"
	function$[11] = "THAT'S"
	function$[12] = "YEAH"
	function$[13] = "HAVE"
	function$[14] = "HAS"
	function$[15] = "HAD"
	function$[16] = "HAVEN'T"
	function$[17] = "HASN'T"
	function$[18] = "HADN'T"
	function$[19] = "AND"
	function$[20] = "CAN"
	function$[21] = "CAN'T"
	function$[22] = "THEM"
	function$[23] = "THEN"
	function$[24] = "ANY"
	function$[25] = "ANYTHING"
	function$[26] = "IT"
	function$[27] = "IT'S"
	function$[28] = "DID"
	function$[29] = "DIDN'T"
	function$[30] = "IS"
	function$[31] = "ISN'T"
	function$[32] = "HIS"
	function$[33] = "THIS"
	function$[34] = "IF"
	function$[35] = "HIM"
	function$[36] = "IN"
	function$[37] = "BEEN"
	function$[38] = "THING"
	function$[39] = "THINGS"
	function$[40] = "WE"
	function$[41] = "ME"
	function$[42] = "HE"
	function$[43] = "HE'S"
	function$[44] = "SHE"
	function$[45] = "SHE'S"
	function$[46] = "THESE"
	function$[47] = "THEY"
	function$[48] = "LOT"
	function$[49] = "NOT"
	function$[50] = "GOT"
	function$[51] = "GOTTEN"
	function$[52] = "GONNA"
	function$[53] = "ON"
	function$[54] = "OR"
	function$[55] = "OFF"
	function$[56] = "BUT"
	function$[57] = "UM"
	function$[58] = "UH"
	function$[59] = "UP"
	function$[60] = "US"
	function$[61] = "OF"
	function$[62] = "THE"
	function$[63] = "BECAUSE"
	function$[64] = "DOES"
	function$[65] = "DOESN'T"
	function$[66] = "COULD"
	function$[67] = "SHOULD"
	function$[68] = "WOULD"
	function$[69] = ""
	function$[70] = ""
	function$[71] = ""
	function$[72] = ""
	function$[73] = ""
	function$[74] = ""
	function$[75] = ""
	function$[76] = ""
	function$[77] = ""
	function$[78] = ""
	function$[79] = ""
	function$[80] = ""
	

# all the sounds 
	all$[1] = "AA0"
	all$[2] = "AA1"
	all$[3] = "AA2"
	all$[4] = "AE0"
	all$[5] = "AE1"
	all$[6] = "AE2"
	all$[7] = "AH0"
	all$[8] = "AH1"
	all$[9] = "AH2"
	all$[10] = "AO0"
	all$[11] = "AO1"
	all$[12] = "AO2"
	all$[13] = "AW0"
	all$[14] = "AW1"
	all$[15] = "AW2"
	all$[16] = "AY0"
	all$[17] = "AY1"
	all$[18] = "AY2"
	all$[19] = "B"
	all$[20] = "CH"
	all$[21] = "D"
	all$[22] = "DH"
	all$[23] = "EH0"
	all$[24] = "EH1"
	all$[25] = "EH2"
	all$[26] = "ER0"
	all$[27] = "ER1"
	all$[28] = "ER2"
	all$[29] = "EY0"
	all$[30] = "EY1"
	all$[31] = "EY2"
	all$[32] = "F"
	all$[33] = "G"
	all$[34] = "HH"
	all$[35] = "IH0"
	all$[36] = "IH1"
	all$[37] = "IH2"
	all$[38] = "IY0"
	all$[39] = "IY1"
	all$[40] = "IY2"
	all$[41] = "JH"
	all$[42] = "K"
	all$[43] = "L"
	all$[44] = "M"
	all$[45] = "N"
	all$[46] = "NG"
	all$[47] = "OW0"
	all$[48] = "OW1"
	all$[49] = "OW2"
	all$[50] = "OY0"
	all$[51] = "OY1"
	all$[52] = "OY2"
	all$[53] = "P"
	all$[54] = "R"
	all$[55] = "S"
	all$[56] = "SH"
	all$[57] = "T"
	all$[58] = "TH"
	all$[59] = "UH0"
	all$[60] = "UH1"
	all$[61] = "UH2"
	all$[62] = "UW0"
	all$[63] = "UW1"
	all$[64] = "UW2"
	all$[65] = "V"
	all$[66] = "W"
	all$[67] = "Y"
	all$[68] = "Z"
	all$[69] = "ZH"
	all$[70] = "SP"

# coronals
	coronal$[1] = "T"
	coronal$[2] = "D"
	coronal$[3] = "TH"
	coronal$[4] = "DH"
	coronal$[5] = "S"
	coronal$[6] = "Z"
	coronal$[7] = "SH"
	coronal$[8] = "ZH"
	coronal$[9] = "CH"
	coronal$[10] = "JH"
	coronal$[11] = "N"
	coronal$[12] = "L"
	
# nasals
	nasal$[1] = "N"
	nasal$[2] = "M"
	nasal$[3] = "NG"
	
# L
	lateral$[1] = "L"
	

	# initializing arrays
	for i from 1 to 70
		previous_in$[i] = ""
		previous_out$[i] = ""
		following_in$[i] = ""
		following_out$[i] = ""
	endfor
	
	#echo 'all$[1]'
	
	
	### previous context conditions
	# previous: anything
	if 'previous' = 1
		for i from 1 to 70
			previous_in$[i] = all$[i]
		endfor
		
	# previous: coronals 
	elsif 'previous' = 2
		for i from 1 to 12
			previous_in$[i] = coronal$[i]
		endfor
		
	# previous: non-coronals -- when "out" array is not going to be empty, "in" array has to be filled in as well. 
	elsif 'previous' = 3
		for i from 1 to 70
			previous_in$[i] = all$[i]
		endfor
		for i from 1 to 12
			previous_out$[i] = coronal$[i]
		endfor
	endif

	
	### following context
	# following: anything
	if 'following' = 1
		for i from 1 to 70
			following_in$[i] = all$[i]
		endfor
	
	# following: nasals
	elsif 'following' = 2
		for i from 1 to 3
			following_in$[i] = nasal$[i]
		endfor
	
	# following: non-nasals -- when "out" array is not going to be empty, "in" array has to be filled in as well. 
	elsif 'following' = 3
		for i from 1 to 70
			following_in$[i] = all$[i]
		endfor
		for i from 1 to 3
			following_out$[i] = nasal$[i]
		endfor
	
	# following: L
	elsif 'following' = 4
		for i from 1 to 1
			following_in$[i] = lateral$[i]
		endfor
	
	# following: non-L -- when "out" array is not going to be empty, "in" array has to be filled in as well. 
	elsif 'following' = 5
		for i from 1 to 70
			following_in$[i] = all$[i]
		endfor
		for i from 1 to 1
			following_out$[i] = lateral$[i]
		endfor
	endif

	

	#########################################################################################
	
	
Create Strings as file list... list 'sound_directory$'*'sound_file_extension$'
numberOfFiles = Get number of strings
	
	
for ifile to numberOfFiles
	name$ = Get string... ifile
	
	# A sound file is opened from the listing:
	Open long sound file... 'sound_directory$''name$'
		
	soundname$ = selected$("LongSound", 1)
	
	word_tier = tier_number + 1
	
	
	# Open a TextGrid by the same name:
	gridfile$ = "'textGrid_directory$''soundname$''textGrid_file_extension$'"
	Read from file... 'gridfile$'
			
	textgrid = selected("TextGrid")
	select 'textgrid'
	nlabels = Get number of intervals... tier_number
	
	
	
	# looping through all intervals in the sound tier (except the very first and the very end of it)
	for label from 2 to nlabels - 1
		select 'textgrid'
		label_name$ = Get label of interval... tier_number label
		previous_segment$ = Get label of interval... tier_number label - 1
		following_segment$ = Get label of interval... tier_number label + 1
		
		# check conditions 
		if label_name$ = "'segment$'"
			
			previous_in_match = 0
			previous_out_match = 0
			for i from 1 to 70
				temp$ = previous_in$[i]
				if temp$ = "'previous_segment$'"
					previous_in_match = previous_in_match + 1
				endif
			endfor
			for i from 1 to 70
				temp$ = previous_out$[i]
				if temp$ = "'previous_segment$'"
					previous_out_match = previous_out_match + 1
				endif
			endfor
						
			following_in_match = 0
			following_out_match = 0
			for i from 1 to 70
				temp$ = following_in$[i]
				if temp$ = "'following_segment$'"
					following_in_match = following_in_match + 1
				endif
			endfor
			for i from 1 to 70
				temp$ = following_out$[i]
				if temp$ = "'following_segment$'"
					following_out_match = following_out_match + 1
				endif
			endfor
			
			
			if previous_in_match = 1 and previous_out_match = 0
			if following_in_match = 1 and following_out_match = 0
					
			segment_start = Get starting point... tier_number label
			segment_end = Get end point... tier_number label
			
			# Get textgrids for words
			word_tier_number = Get interval at time... word_tier segment_start + 0.01 
			word$ = Get label of interval... word_tier word_tier_number
			
			
			# check whether the word is a function word or not
			function_match = 0
			for i from 1 to 80
				temp$ = function$[i]
				if temp$ = "'word$'"
					function_match = function_match + 1
				endif
			endfor
			
			if function_match = 0
				word_start = Get starting point... word_tier word_tier_number
				word_end = Get end point... word_tier word_tier_number	
				word_start = word_start - 0.01
				word_start = 'word_start:3'
				word_end = word_end + 0.01
				Extract part... word_start word_end no 
		
		
			filename$ = "'folder$'" + "'soundname$'" + "_" + "'segment$'" + "_" + "'word$'"
			filename_textgrid$ = "'filename$'" + ".TextGrid"
			Write to text file... 'filename_textgrid$'
			
	
		# Extracting wave files for words
		select LongSound 'soundname$'
		Extract part... word_start word_end no
		filename_wav$ = "'filename$'" + ".wav"
		Write to WAV file... 'filename_wav$'
				
			endif
			endif
			endif
		endif
	endfor

	select all
	minus Strings list
	Remove
	select Strings list
endfor

select all
Remove
	
