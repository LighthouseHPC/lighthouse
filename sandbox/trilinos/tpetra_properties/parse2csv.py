import re,sys

input_file = sys.argv[1]
timings_file = open(sys.argv[2] + '_timing.csv', 'a')
results_file = open(sys.argv[2] + '_results.csv', 'a')
reading = open(input_file, 'r')

results_line = ""
timings_line = ""

for i, line in enumerate(reading):
	if i == 0: #matrix
		results_line += (line.split(": ")[1]).rstrip() + ", "
		timings_line += (line.split(": ")[1]).rstrip() + ", "
	if i == 1: #procs
		timings_line += (line.split(": ")[1]).rstrip() + ", "
	if i == 2: #results
		results_line = results_line + line	
	if i == 10: #timings
		string = re.findall(r"[+-]? *(?:\d+(?:\.\d*)?|\.\d+)(?:[eE][+-]?\d+)?", line)
		for item in string:
			timings_line += item + ", "
results_file.write(results_line)
timings_file.write(timings_line)

reading.close()
results_file.close()
timings_file.close()
