import csv

uniqueId = {}



def generateSomeVal(ids):
    return hash(str(ids)) & ((1<<9)-1)

# with open('output.csv', 'w') as csvoutput:
#     writer = csv.writer(csvoutput)
#     with open('purchase.csv') as csv_file:
#         csv_reader = csv.reader(csv_file, delimiter=',',)
#         line_count = 0
#         for row in csv_reader:
#             ids = row[0]
#             if line_count == 0:
#                 print(",".join(row))
#                 # print('Column names are {", ".join(row)}')
#                 line_count += 1
#             else:

#                 if not ids in uniqueId:
#                     uniqueId[ids] = generateSomeVal(ids)
#                 print("for {} : {} ".format(ids,uniqueId[ids]))            
#                 # print('\t{row[0]} works in the {row[1]} department, and was born in {row[2]}.')
#                 line_count += 1


#         for row in csv.reader(csvinput):
#             if row[0] == "Name":
#                 writer.writerow(row+["Berry"])
#             else:
#                 writer.writerow(row+[row[0]])
#         print("Finished")

#         print(len(uniqueId))


def getModified(ids):
    if not ids in uniqueId:
        uniqueId[ids] = generateSomeVal(ids)
    return uniqueId[ids]

filename='purchase.csv'
filenameOutput ='purchase_modified.csv'
with open(filename,'r') as csvinput:
    with open(filenameOutput, 'w') as csvoutput:
        writer = csv.writer(csvoutput)

        for row in csv.reader(csvinput):
            if row[0] == "id":
                writer.writerow(row+["tourid"])
            else:
                addedRowItem = getModified(row[0])
                writer.writerow(row+[addedRowItem])
print(uniqueId)
print(len(uniqueId))