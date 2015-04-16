import re,sys

input_file = sys.argv[1]
timings_file = open("timings.csv", 'a')
results_file = open("results.csv", 'a')
reading = open(input_file, 'r')

results_line = ""
timings_line = ""

for i, line in enumerate(reading):
	if i == 0:
		results_line = line.strip() + ", "
		timings_line = line.strip() + ", "
	if i == 1:
		timings_line += line.strip() + ", "
	if i == 2:
		results_line += line.strip()
	if i == 3:
		timings_line += line.strip()

timings_file.write(timings_line + "\n",)
results_file.write(results_line + "\n",)
