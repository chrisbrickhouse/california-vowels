#!/usr/bin/python
import os

path = '/home/cj/ling/QP2/data/all/'
fnames = os.listdir(path)
fnames = [x for x in fnames if len(x.split('_')) < 4 ]

first = True
for fname in fnames:
    with open(path+fname, 'r') as f:
        text = f.readlines()
    if first != True:
        text = text[1:]
    with open(path+'all_data.txt', 'a') as g:
        g.writelines(text)
    first = False
