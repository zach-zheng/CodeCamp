# import os
# os.chdir(os.path.dirname(os.path.abspath(__file__)))


def count_words(filename):
    """Counting a file which the numbers of words"""
    try:
        with open(filename) as f_object:
            contents = f_object.read()
    except FileNotFoundError:
        # msg = "Sorry, the file " + filename + " don't exist."
        # print(msg)
        pass
    
    else:
        words_list = contents.split()
        num_words = len(words_list)
        print("The file " + filename + " has about " + str(num_words) + " words.")
    
filenames = ['Alice.txt', 'Monster Master', 'Wonderland.txt']
for filename in filenames:
    count_words(filename)


 
