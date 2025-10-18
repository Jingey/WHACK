import csv

def genData(name, sz, func):
    with open(name, 'w', newline="") as csvfile:
        writer = csv.writer(csvfile, delimiter=',')
        for i in range(0, sz):
            writer.writerow(func());

