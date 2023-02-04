import sys


def modifyConfig(filename, option1, option1value, option2, option2value,
                 option3, option3value, option4, option4value):
    data = ""
    with open(filename, 'r') as f:
        for line in f.readlines():
            if line.find(option1) == 0 or line.find(option1) == 1:  # maybe option is commented
                line = '%s: %s\n' % (option1, option1value)
            if line.find(option2) == 0 or line.find(option2) == 1:
                line = '%s: %s\n' % (option2, option2value)
            if line.find(option3) == 0 or line.find(option3) == 1:
                line = '%s: %s\n' % (option3, option3value)
            if line.find(option4) == 0 or line.find(option4) == 1:
                line = '%s: %s\n' % (option4, option4value)
            data += line
    with open(filename, 'w') as f:
        f.writelines(data)


filename = sys.argv[1]
o1 = sys.argv[2]
o1value = sys.argv[3]
o2 = sys.argv[4]
o2value = sys.argv[5]
o3 = sys.argv[6]
o3value = sys.argv[7]
o4 = sys.argv[8]
o4value = sys.argv[9]

modifyConfig(filename, o1, o1value, o2, o2value, o3, o3value, o4, o4value)
