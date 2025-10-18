import csv

def genData(name, sz, first, func):
    with open(name, 'w', newline="") as csvfile:
        writer = csv.writer(csvfile, delimiter=',')
        writer.writerow(first)
        for i in range(0, sz):
            writer.writerow(func());

