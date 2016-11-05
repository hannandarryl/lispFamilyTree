# Short python program to convert old style of Input files To the New Correct format
# Note: Will overwrite the file so make sure copy is saved
# example: python oldToNewConvert.py testfile.txt

import sys

print('Note: Will overwrite the file')

inputName = sys.argv[1]

i = open('./' + inputName, 'r')

newStrings = []

for line in i:
	if line.endswith('\n'):
		line = line[:-1]
	newStrings.append('(' + line + ')\n')
i.close()

o = open('./' + inputName, 'w')
for newline in newStrings:
	o.write(newline)
o.close()

