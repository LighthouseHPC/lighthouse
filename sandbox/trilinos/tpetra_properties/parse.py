import re
import pprint

f = open('parsed.txt', 'w')

with open('output.txt') as fp:
	for line in fp:
		string = re.findall(r"[+-]? *(?:\d+(?:\.\d*)?|\.\d+)(?:[eE][+-]?\d+)?", line)
		for item in string:
			f.write("%s," % item)
		if string:
			f.write("\n")
	f.close()
