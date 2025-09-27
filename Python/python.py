filename = 'pi_digits.txt'

with open(filename) as file_object:
    for line in file_object:
        print(line)

# try:
#     with open(filename) as file_object:
#         contents = file_object.read()
# except FileNotFoundError:
#     msg = "Sorry, the file " + filename + " does not exist."
#     print(msg)
# else:
#     #counting number of words in file
#     words = contents.split()
#     num_words = len(words)
#     print("This file " + filename + " has about " + str(num_words) + " words.")
 
