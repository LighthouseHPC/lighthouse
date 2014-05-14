import csv


###----- Created le_computational_all.csv based on all computational le files. -----###

f_le_factor = csv.reader(open('le_factor.csv', 'rb'), delimiter=',')
f_le_solve = csv.reader(open('le_solve.csv', 'rb'), delimiter=',')
f_le_condition_number = csv.reader(open('le_condition_number.csv', 'rb'), delimiter=',')
f_le_error_bounds = csv.reader(open('le_error_bounds.csv', 'rb'), delimiter=',')
f_le_invert = csv.reader(open('le_invert.csv', 'rb'), delimiter=',')
f_le_equilibrate = csv.reader(open('le_equilibrate.csv', 'rb'), delimiter=',')

f_le_computational_all = csv.writer(open('le_computational_all.csv', 'wb'), delimiter=',')


def writeRows(fileName, lineNumber):
    for row in fileName:
        row[0] = int(row[0]) + lineNumber
        f_le_computational_all.writerow(row)



writeRows(f_le_factor, 0)

writeRows(f_le_solve, f_le_factor.line_num)

writeRows(f_le_condition_number, f_le_factor.line_num+f_le_solve.line_num)

writeRows(f_le_error_bounds, f_le_factor.line_num+f_le_solve.line_num+f_le_condition_number.line_num)

writeRows(f_le_invert, f_le_factor.line_num+f_le_solve.line_num+f_le_condition_number.line_num+f_le_error_bounds.line_num)

writeRows(f_le_equilibrate, f_le_factor.line_num+f_le_solve.line_num+f_le_condition_number.line_num+f_le_error_bounds.line_num+f_le_invert.line_num)