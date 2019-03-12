#!/usr/bin/python

import csv

path = '/home/cj/ling/QP2/'

demographics = {}

with open(path+'data/Magnum_opus.csv','r') as mo:
    mo_lines = mo.readlines()
    for line in mo_lines:
        code = line.split(',')[0].strip('"').strip()
        demographics[code] = line
        
print(list(demographics.keys()))
        
with open(path+'data/all/full_data.csv','r') as sauce:
    csv_reader = csv.reader(sauce, delimiter=',')
    first = True
    total = 0
    e_count=0
    for line in csv_reader:
        total+=1
        if first != True:
            code='_'.join(line[1:4])
            if code not in demographics:
                print(code)
                e_count+=1
                continue
            else:
                newline = ','.join(line)+','+demographics[code]
        else:
            newline = ','.join(line)+','+demographics['Speaker_code']
            first=False
        with open(path+'data/full_data_with_demo.csv','a') as o:
            o.write(newline)
            
print(e_count)
print(total)
print(e_count/float(total))
