form Find tokens and save them
   comment See header of script for details. 

   comment Directory of sound files
   text sound_directory /home/cj/ling/QP2/audio/wordlist_data/
   comment Directory of TextGrid files
   text textGrid_directory /home/cj/ling/QP2/audio/wordlist_data/
   comment Specify the folder of the resulting files:
   text folder /home/cj/ling/QP2/audio/for_sauce_all/
 endform

tier_number = 1

# BEET
segment$ = "IY1"
previous = 1 
# 1 = anything; 2=coronals; 3=non-coronals
following = 5 
# 1 = anything; 2 = nasals; 3 = non-nasals; 4 = L; 5 = non-L
include /home/cj/ling/QP2/praat_scripts/extract_tokens_sk_callable.praat

# BIT
segment$ = "IH1"
previous = 1
# 1 = anything; 2=coronals; 3=non-coronals
following = 1
# 1 = anything; 2 = nasals; 3 = non-nasals; 4 = L; 5 = non-L
include /home/cj/ling/QP2/praat_scripts/extract_tokens_sk_callable.praat

# BAIT
segment$ = "EY1"
previous = 1
# 1 = anything; 2=coronals; 3=non-coronals
following = 5
# 1 = anything; 2 = nasals; 3 = non-nasals; 4 = L; 5 = non-L
include /home/cj/ling/QP2/praat_scripts/extract_tokens_sk_callable.praat

# BET
segment$ = "EH1"
previous = 1
# 1 = anything; 2=coronals; 3=non-coronals
following = 5
# 1 = anything; 2 = nasals; 3 = non-nasals; 4 = L; 5 = non-L
include /home/cj/ling/QP2/praat_scripts/extract_tokens_sk_callable.praat

# BAT/BAN/PAL
segment$ = "AE1"
previous = 1
# 1 = anything; 2=coronals; 3=non-coronals
following = 1
# 1 = anything; 2 = nasals; 3 = non-nasals; 4 = L; 5 = non-L
include /home/cj/ling/QP2/praat_scripts/extract_tokens_sk_callable.praat

# BOWL/TOE
segment$ = "OW1"
previous = 1
# 1 = anything; 2=coronals; 3=non-coronals
following = 5
# 1 = anything; 2 = nasals; 3 = non-nasals; 4 = L; 5 = non-L
include /home/cj/ling/QP2/praat_scripts/extract_tokens_sk_callable.praat

# BOOT/POOL
segment$ = "UW1"
previous = 1
# 1 = anything; 2=coronals; 3=non-coronals
following = 3
# 1 = anything; 2 = nasals; 3 = non-nasals; 4 = L; 5 = non-L
include /home/cj/ling/QP2/praat_scripts/extract_tokens_sk_callable.praat

# BUT
segment$ = "AH1"
previous = 1
# 1 = anything; 2=coronals; 3=non-coronals
following = 5
# 1 = anything; 2 = nasals; 3 = non-nasals; 4 = L; 5 = non-L
include /home/cj/ling/QP2/praat_scripts/extract_tokens_sk_callable.praat

# BOT
segment$ = "AA1"
previous = 1
# 1 = anything; 2=coronals; 3=non-coronals
following = 5
# 1 = anything; 2 = nasals; 3 = non-nasals; 4 = L; 5 = non-L
include /home/cj/ling/QP2/praat_scripts/extract_tokens_sk_callable.praat

# BOUGHT
segment$ = "AO1"
previous = 1
# 1 = anything; 2=coronals; 3=non-coronals
following = 5
# 1 = anything; 2 = nasals; 3 = non-nasals; 4 = L; 5 = non-L
include /home/cj/ling/QP2/praat_scripts/extract_tokens_sk_callable.praat