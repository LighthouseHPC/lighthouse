import csv


###----- Created le_driver_all.csv based on le_simple.csv and le_expert.csv -----###

f_le_simple = csv.reader(open('le_simple.csv', 'rb'), delimiter=',')
f_le_expert = csv.reader(open('le_expert.csv', 'rb'), delimiter=',')
f_le_driver_all = csv.writer(open('le_driver_all.csv', 'wb'), delimiter=',')


def writeRows(fileName, lineNumber):
    for row in fileName:
        row[0] = int(row[0]) + lineNumber
        f_le_driver_all.writerow(row)


writeRows(f_le_simple, 0)

writeRows(f_le_expert, f_le_simple.line_num)