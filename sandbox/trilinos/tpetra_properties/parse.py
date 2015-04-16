import re,sys

input_file = sys.argv[1]
output_file = input_file + '.parsed'
writing = open(output_file, 'w')
reading = open(input_file, 'r')

for i, line in enumerate(reading):
	if i == 0 or i == 1: #matrix/procs
		writing.write( line.split(": ")[1] )
	if i == 2: #results
		writing.write(line)
	if i == 10: #timings
		string = re.findall(r"[+-]? *(?:\d+(?:\.\d*)?|\.\d+)(?:[eE][+-]?\d+)?", line)
		for item in string:
			writing.write("%s," % item)
writing.close()
reading.close()
