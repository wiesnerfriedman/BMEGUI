x=[-74.043700000000001, -74.043700000000001, -74.043700000000001]

y =['asdsadasds.txt',10,20];

myfile = open("testDelete.txt", "wb")
for item in y:
    myfile.write(str(item)+',')
    print item
myfile.close()
