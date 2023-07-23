with open('bad_utf8.txt', 'wb') as fh:
    fh.write("10\n20\n\xc3\x28\n40")
