from random import randint

class Die():
    """a class of a dice"""
    def __init__(self, num_sides=6):
        #6 sides of a dice
        self.num_sides = num_sides

    def roll(self):
        """return a number of 1 - 6 """
        return randint(1, self.num_sides)